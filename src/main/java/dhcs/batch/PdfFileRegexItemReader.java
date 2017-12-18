/*
 * The MIT License
 *
 * Copyright 2017 Clifford Errickson.
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
 * THE SOFTWARE.
 */
package dhcs.batch;

import java.util.regex.Matcher;
import java.util.regex.Pattern;
import org.apache.pdfbox.io.MemoryUsageSetting;
import org.apache.pdfbox.pdmodel.PDDocument;
import org.apache.pdfbox.text.PDFTextStripper;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.batch.item.ItemReader;
import org.springframework.batch.item.file.ResourceAwareItemReaderItemStream;
import org.springframework.batch.item.file.mapping.BeanWrapperFieldSetMapper;
import org.springframework.batch.item.file.transform.DefaultFieldSet;
import org.springframework.batch.item.file.transform.FieldSet;
import org.springframework.batch.item.support.AbstractItemCountingItemStreamItemReader;
import org.springframework.beans.factory.InitializingBean;
import org.springframework.core.io.Resource;
import org.springframework.util.Assert;
import org.springframework.util.ClassUtils;

/**
 * An {@link ItemReader} for reading items from a PDF file using a regular
 * expression to match records.
 *
 * @author Clifford Errickson
 * @since 1.0
 * @param <T> the type of object to read.
 */
public class PdfFileRegexItemReader<T> extends AbstractItemCountingItemStreamItemReader<T> implements
        ResourceAwareItemReaderItemStream<T>, InitializingBean {

  private static final Logger LOG = LoggerFactory.getLogger(PdfFileRegexItemReader.class);

  private Resource resource;

  private MemoryUsageSetting memoryUsageSetting = MemoryUsageSetting.setupMainMemoryOnly();

  private Pattern pattern;

  private String[] propertyNames;

  private BeanWrapperFieldSetMapper<T> fieldSetMapper;

  private String extractedText;

  private int currentIndex = -1;

  public PdfFileRegexItemReader() {
    setName(ClassUtils.getShortName(PdfFileRegexItemReader.class));
  }

  @Override
  public void afterPropertiesSet() throws Exception {
    Assert.notNull(resource, "resource must be set");
    Assert.notNull(pattern, "pattern must be set");
    Assert.notNull(propertyNames, "propertyNames must be set");
    Assert.notNull(fieldSetMapper, "fieldSetMapper must be set");
  }

  /**
   * Set the resource file to load data from.
   *
   * @param resource the resource file to load data from.
   */
  @Override
  public void setResource(final Resource resource) {
    this.resource = resource;
  }

  /**
   * Settings used when processing PDF file.
   *
   * @param memoryUsageSetting settings used when processing PDF file.
   */
  public void setMemoryUsageSetting(final MemoryUsageSetting memoryUsageSetting) {
    this.memoryUsageSetting = memoryUsageSetting;
  }

  /**
   * The regular expression to be used to match items into groups. Groups must
   * match the provided {@code propertyNames}.
   *
   * <p>
   * An alternative to {@link #setPattern(Pattern)}.
   *
   * @param regex a regular expression with group names.
   */
  public void setRegex(final String regex) {
    this.pattern = Pattern.compile(regex);
  }

  /**
   * The pattern used to match items into groups. Groups must match the provided
   * {@code propertyNames}.
   *
   * <p>
   * An alternative to {@link #setRegex(String)}.
   *
   * @param pattern a pattern.
   */
  public void setPattern(final Pattern pattern) {
    this.pattern = pattern;
  }

  /**
   * The names of properties matching group names in the underlying
   * regex/pattern.
   *
   * @param propertyNames names of properties matching group names in the
   * underlying regex/pattern.
   */
  public void setPropertyNames(final String[] propertyNames) {
    this.propertyNames = propertyNames;
  }

  /**
   * For mapping matched groups to bean properties.
   *
   * @param fieldSetMapper maps matched groups to bean properties.
   */
  public void setFieldSetMapper(final BeanWrapperFieldSetMapper<T> fieldSetMapper) {
    this.fieldSetMapper = fieldSetMapper;
  }

  /**
   * Using the provided regex pattern, match the next item from the underlying
   * source data, convert it into an object, and return it.
   *
   * @return the next item from the underlying source data.
   * @throws Exception on error
   */
  @Override
  protected T doRead() throws Exception {
    if (currentIndex < 0 || currentIndex >= extractedText.length() - 1) {
      return null;
    }

    Matcher matcher = pattern.matcher(extractedText);

    if (!matcher.find(currentIndex)) {
      if (LOG.isDebugEnabled()) {
        LOG.debug("No match for input [" + extractedText + "] and pattern [" + pattern.pattern() + "] starting from index [" + currentIndex + "]");
      }

      return null;
    }

    String[] values = new String[propertyNames.length];

    for (int i = 0; i < propertyNames.length; i++) {
      values[i] = matcher.group(propertyNames[i]);
    }

    FieldSet fieldSet = new DefaultFieldSet(values, propertyNames);

    if (LOG.isDebugEnabled()) {
      LOG.debug("Extracted values: " + fieldSet);
    }

    currentIndex = matcher.end();

    return fieldSetMapper.mapFieldSet(fieldSet);
  }

  /**
   * Loads text from the provided PDF file into memory and then closes the file.
   *
   * @throws Exception on error
   */
  @Override
  protected void doOpen() throws Exception {
    Assert.notNull(resource, "Input resource must be set");

    if (LOG.isDebugEnabled()) {
      LOG.debug("Extracting text from file: " + resource.getFilename());
    }

    try (PDDocument document = PDDocument.load(resource.getFile(), memoryUsageSetting)) {
      PDFTextStripper textStripper = new PDFTextStripper();

      extractedText = textStripper.getText(document);
    }

    if (LOG.isTraceEnabled()) {
      LOG.trace("Extracted text [" + extractedText + "]");
    }

    currentIndex = 0;
  }

  /**
   * Marks this reader as closed.
   *
   * <p>
   * Note: This method does not actually close the underlying PDF file. That is
   * done at the time the file is read into memory.
   *
   * @throws Exception on error.
   */
  @Override
  protected void doClose() throws Exception {
    extractedText = null;
    currentIndex = -1;
  }

}
