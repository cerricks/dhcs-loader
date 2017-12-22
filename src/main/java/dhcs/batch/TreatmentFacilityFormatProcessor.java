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

import org.apache.commons.lang.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.batch.item.ItemProcessor;
import org.springframework.beans.factory.InitializingBean;

/**
 * An implementation of {@link ItemProcessor} for formatting values in a
 * {@link TreatmentFacility} item.
 *
 * @author Clifford Errickson
 * @since 1.2
 */
public class TreatmentFacilityFormatProcessor implements ItemProcessor<TreatmentFacility, TreatmentFacility>, InitializingBean {

  private static final Logger LOG = LoggerFactory.getLogger(TreatmentFacilityFormatProcessor.class);

  private String[] addressLowerCaseExceptions;
  private String[] addressUpperCaseExceptions;
  private String[] nameLowerCaseExceptions;
  private String[] nameUpperCaseExceptions;

  public TreatmentFacilityFormatProcessor() {
  }

  @Override
  public void afterPropertiesSet() throws Exception {
    if (addressLowerCaseExceptions == null) {
      addressLowerCaseExceptions = new String[]{};
    }

    if (addressUpperCaseExceptions == null) {
      addressUpperCaseExceptions = new String[]{};
    }

    if (nameLowerCaseExceptions == null) {
      nameLowerCaseExceptions = new String[]{};
    }

    if (nameUpperCaseExceptions == null) {
      nameUpperCaseExceptions = new String[]{};
    }
  }

  /**
   * Configure the list of words in an address to be set to lowercase.
   *
   * @param addressLowerCaseExceptions words to be set to lowercase.
   */
  public void setAddressLowerCaseExceptions(final String[] addressLowerCaseExceptions) {
    this.addressLowerCaseExceptions = addressLowerCaseExceptions;
  }

  /**
   * Configure the list of words in an address to be set to upper case.
   *
   * @param addressUpperCaseExceptions words to be set to upper case.
   */
  public void setAddressUpperCaseExceptions(final String[] addressUpperCaseExceptions) {
    this.addressUpperCaseExceptions = addressUpperCaseExceptions;
  }

  /**
   * Configure the list of words in a name to be set to lower case.
   *
   * @param nameLowerCaseExceptions words to be set to lower case.
   */
  public void setNameLowerCaseExceptions(final String[] nameLowerCaseExceptions) {
    this.nameLowerCaseExceptions = nameLowerCaseExceptions;
  }

  /**
   * Configure the list of words in a name to be set to upper case.
   *
   * @param nameUpperCaseExceptions words to be set to upper case.
   */
  public void setNameUpperCaseExceptions(final String[] nameUpperCaseExceptions) {
    this.nameUpperCaseExceptions = nameUpperCaseExceptions;
  }

  /**
   * Formats attributes of {@link TreatmentFacility}.
   *
   * @param item the item to format.
   * @return the properly formatted item.
   * @throws Exception on error.
   */
  @Override
  public TreatmentFacility process(final TreatmentFacility item) throws Exception {
    item.setProgramName(TitleCaseUtil.toTitleCase(item.getProgramName(), nameLowerCaseExceptions, nameUpperCaseExceptions));
    item.setLegalName(TitleCaseUtil.toTitleCase(item.getLegalName(), nameLowerCaseExceptions, nameUpperCaseExceptions));
    item.setStreet1(TitleCaseUtil.toTitleCase(item.getStreet1(), addressLowerCaseExceptions, addressUpperCaseExceptions));
    item.setStreet2(TitleCaseUtil.toTitleCase(item.getStreet2(), addressLowerCaseExceptions, addressUpperCaseExceptions));
    item.setCity(TitleCaseUtil.toTitleCase(item.getCity(), addressLowerCaseExceptions, addressUpperCaseExceptions));
    item.setState(StringUtils.normalizeSpace(item.getState()));
    item.setPhone(StringUtils.normalizeSpace(item.getPhone()));
    item.setFax(StringUtils.normalizeSpace(item.getFax()));
    item.setRecordId(StringUtils.normalizeSpace(item.getRecordId()));
    item.setServiceType(StringUtils.normalizeSpace(item.getServiceType()));
    item.setTargetPopulation(StringUtils.normalizeSpace(item.getTargetPopulation()));
    item.setExpirationDate(StringUtils.normalizeSpace(item.getExpirationDate()));

    return item;
  }

}
