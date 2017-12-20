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
    treatmentFacility.setRecordId(rs.getString("record_id"));
    treatmentFacility.setProgramName(rs.getString("program_name"));
    treatmentFacility.setLegalName(rs.getString("legal_name"));
    treatmentFacility.setStreet1(rs.getString("address_street1"));
    treatmentFacility.setStreet2(rs.getString("address_street2"));
    treatmentFacility.setCity(rs.getString("address_city"));
    treatmentFacility.setState(rs.getString("address_state"));
    treatmentFacility.setZip(rs.getString("address_zip"));
    treatmentFacility.setPhone(rs.getString("phone"));
    treatmentFacility.setFax(rs.getString("fax"));
    treatmentFacility.setServiceType(rs.getString("service_type"));
    treatmentFacility.setTargetPopulation(rs.getString("target_population"));
    treatmentFacility.setResidentCapacity(rs.getInt("resident_capacity"));
    treatmentFacility.setTotalOccupancy(rs.getInt("total_occupancy"));
    treatmentFacility.setExpirationDate(rs.getString("expiration_date"));
    treatmentFacility.setIms(rs.getString("ims"));

    return treatmentFacility;
  }

}
