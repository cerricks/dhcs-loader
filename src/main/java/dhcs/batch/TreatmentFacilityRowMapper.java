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

import java.sql.ResultSet;
import java.sql.SQLException;
import org.springframework.jdbc.core.RowMapper;

/**
 * An implementation of {@link RowMapper} for converting records in a
 * {@link ResultSet} to a {@link TreatmentFacility}.
 *
 * @author Clifford Errickson
 * @since 1.0
 */
public class TreatmentFacilityRowMapper implements RowMapper<TreatmentFacility> {

  @Override
  public TreatmentFacility mapRow(final ResultSet rs, final int rowNum) throws SQLException {
    TreatmentFacility treatmentFacility = new TreatmentFacility();
    treatmentFacility.setRecordId(rs.getString("RECORD_ID"));
    treatmentFacility.setProgramName(rs.getString("PROGRAM_NAME"));
    treatmentFacility.setLegalName(rs.getString("LEGAL_NAME"));
    treatmentFacility.setStreet1(rs.getString("ADDRESS_STREET1"));
    treatmentFacility.setStreet2(rs.getString("ADDRESS_STREET2"));
    treatmentFacility.setCity(rs.getString("ADDRESS_CITY"));
    treatmentFacility.setState(rs.getString("ADDRESS_STATE"));
    treatmentFacility.setZip(rs.getString("ADDRESS_ZIP"));
    treatmentFacility.setPhone(rs.getString("PHONE"));
    treatmentFacility.setFax(rs.getString("FAX"));
    treatmentFacility.setServiceType(rs.getString("SERVICE_TYPE"));
    treatmentFacility.setTargetPopulation(rs.getString("TARGET_POPULATION"));
    treatmentFacility.setResidentCapacity(rs.getInt("RESIDENT_CAPACITY"));
    treatmentFacility.setTotalOccupancy(rs.getInt("TOTAL_OCCUPANCY"));
    treatmentFacility.setExpirationDate(rs.getString("EXPIRATION_DATE"));
    treatmentFacility.setIms(rs.getString("IMS"));

    return treatmentFacility;
  }

}
