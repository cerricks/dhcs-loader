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

import org.springframework.classify.Classifier;

/**
 * An implementation of {@link Classifier} used to classify
 * {@link TreatmentFacility} items exist or not.
 *
 * @author Clifford Errickson
 * @since 1.0
 */
public class TreatmentFacilityExistsClassifier implements Classifier<TreatmentFacility, String> {

  /**
   * Classifies a {@link TreatmentFacility} as exists ("TRUE") or not exists
   * ("FALSE") based on whether it has an ID or not.
   *
   * @param treatmentFacility the item to classify.
   * @return "TRUE" if exists, "FALSE" otherwise.
   */
  @Override
  public String classify(final TreatmentFacility treatmentFacility) {
    return treatmentFacility != null
            && treatmentFacility.getId() != null
            ? Boolean.TRUE.toString()
            : Boolean.FALSE.toString();
  }

}
