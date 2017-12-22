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
import org.apache.commons.lang.WordUtils;

/**
 * A utility for converting a string to a proper title.
 *
 * @author Clifford Errickson
 * @since 1.2
 */
public class TitleCaseUtil {

  private static final String[] EMPTY_EXCLUDE = new String[]{};

  /**
   * Converts the given string to the proper title case.
   *
   * @param str the string to convert.
   * @return a string in proper title case.
   */
  public static String toTitleCase(final String str) {
    return toTitleCase(str, EMPTY_EXCLUDE, EMPTY_EXCLUDE);
  }

  /**
   * Converts the given string to the proper title case.
   *
   * @param str the string to convert.
   * @param lowerCaseExceptions words to convert to all lower case.
   * @param upperCaseExceptions words to convert to all upper case.
   * @return a string in proper title case.
   */
  public static String toTitleCase(final String str, final String[] lowerCaseExceptions, final String[] upperCaseExceptions) {
    if (str == null) {
      return null;
    }

    String tempStr = str;

    tempStr = StringUtils.normalizeSpace(tempStr);

    tempStr = WordUtils.capitalizeFully(tempStr, new char[]{' ', '-', '.', '(', '/', ',', '"'});

    for (String s : lowerCaseExceptions) {
      tempStr = tempStr.replaceAll("(?<!(^|-[ ]?))(?i:\\b(?<!\\.)" + s + "\\b)", s.toLowerCase()); // matches string within a word boundary not at beginning
    }

    for (String s : upperCaseExceptions) {
      tempStr = tempStr.replaceAll("(?<!(^|-[ ]?))(?i:\\b" + s + "\\b)", s.toUpperCase()); // matches string within a word boundary not at beginning
    }

    return tempStr;
  }

}
