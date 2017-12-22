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

import static org.assertj.core.api.Assertions.assertThat;
import org.junit.Test;

/**
 * Unit test for {@link TitleCaseUtil}.
 *
 * @author Clifford Errickson
 * @since 1.2
 */
public class TitleCaseUtilTest {

  private final String[] addressLowerCaseExceptions = new String[]{"and", "as", "at", "by", "en", "in", "of", "on", "or", "to"};
  private final String[] addressUpperCaseExceptions = new String[]{"II", "III", "IV", "VI", "VII", "VIII"};
  private final String[] nameLowerCaseExceptions = new String[]{"a", "an", "and", "as", "at", "but", "by", "en", "for", "if", "in", "nor", "of", "on", "or", "per", "the", "to", "vs", "via"};
  private final String[] nameUpperCaseExceptions = new String[]{"II", "III", "IV", "VI", "VII", "VIII", "CA", "IOP", "LA", "LLC"};

  /**
   * Test of toTitleCase method, of class TitleCaseUtil.
   */
  @Test
  public void testToTitleCase() {
    assertThat(TitleCaseUtil.toTitleCase("CHANNEL VIEW HOUSE", nameLowerCaseExceptions, nameUpperCaseExceptions)).isEqualTo("Channel View House");
    assertThat(TitleCaseUtil.toTitleCase("COMFORT RECOVERY IOP LLC", nameLowerCaseExceptions, nameUpperCaseExceptions)).isEqualTo("Comfort Recovery IOP LLC");
    assertThat(TitleCaseUtil.toTitleCase("ALCOHOL AND DRUG EDUCATION AND COUNSELING CENTER", nameLowerCaseExceptions, nameUpperCaseExceptions)).isEqualTo("Alcohol and Drug Education and Counseling Center");
    assertThat(TitleCaseUtil.toTitleCase("LAKE COUNTY DIVISION OF ALCOHOL AND OTHER DRUG SERVICES", nameLowerCaseExceptions, nameUpperCaseExceptions)).isEqualTo("Lake County Division of Alcohol and Other Drug Services");
    assertThat(TitleCaseUtil.toTitleCase("THE NAPD GENESIS PROGRAM (NEW ADVANCES FOR PEOPLE WITH DISABILITIES)", nameLowerCaseExceptions, nameUpperCaseExceptions)).isEqualTo("The Napd Genesis Program (New Advances for People With Disabilities)");
    assertThat(TitleCaseUtil.toTitleCase("NEW DIRECTIONS FOR YOUNG ADULTS CA, INC.", nameLowerCaseExceptions, nameUpperCaseExceptions)).isEqualTo("New Directions for Young Adults CA, Inc.");
    assertThat(TitleCaseUtil.toTitleCase("C.U.R.A., INC.", nameLowerCaseExceptions, nameUpperCaseExceptions)).isEqualTo("C.U.R.A., Inc.");
    assertThat(TitleCaseUtil.toTitleCase("SECOND CHANCE (TRI-CITIES), INC.", nameLowerCaseExceptions, nameUpperCaseExceptions)).isEqualTo("Second Chance (Tri-Cities), Inc.");
    assertThat(TitleCaseUtil.toTitleCase("THE CAMP RECOVERY CENTER-SECTION II", nameLowerCaseExceptions, nameUpperCaseExceptions)).isEqualTo("The Camp Recovery Center-Section II");
    assertThat(TitleCaseUtil.toTitleCase("\"SET FREE\" DRUG & ALCOHOL TREATMENT CENTERS OF AMERICA", nameLowerCaseExceptions, nameUpperCaseExceptions)).isEqualTo("\"Set Free\" Drug & Alcohol Treatment Centers of America");
    assertThat(TitleCaseUtil.toTitleCase("A HEALING PLACE - THE ESTATES", nameLowerCaseExceptions, nameUpperCaseExceptions)).isEqualTo("A Healing Place - The Estates");

    assertThat(TitleCaseUtil.toTitleCase("15450 COUNTY ROAD 99, BUILDING A AND 15430 COUNTY ROAD 99, BUILDING B", addressLowerCaseExceptions, addressUpperCaseExceptions)).isEqualTo("15450 County Road 99, Building A and 15430 County Road 99, Building B");
    assertThat(TitleCaseUtil.toTitleCase("125-A, 125-B, 125-C & 125-D WEST HARRISON STREET", addressLowerCaseExceptions, addressUpperCaseExceptions)).isEqualTo("125-A, 125-B, 125-C & 125-D West Harrison Street");
    assertThat(TitleCaseUtil.toTitleCase("2885 CHURN CREEK ROAD, SUITE A", addressLowerCaseExceptions, addressUpperCaseExceptions)).isEqualTo("2885 Churn Creek Road, Suite A");
  }

}
