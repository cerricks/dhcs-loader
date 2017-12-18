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

import java.io.Serializable;
import java.util.Objects;

/**
 * Represents a non-medical alcoholism and drug abuse recovery or treatment
 * facilities licensed and/or certified by the Department of Health Care
 * Services.
 *
 * @author Clifford Errickson
 * @since 1.0
 */
public class TreatmentFacility implements Serializable {

  private Long id;
  private String recordId;
  private String programName;
  private String legalName;
  private String street1;
  private String street2;
  private String city;
  private String state;
  private String zip;
  private String phone;
  private String fax;
  private String serviceType;
  private Integer residentCapacity;
  private Integer totalOccupancy;
  private String targetPopulation;
  private String expirationDate;
  private String ims;

  public TreatmentFacility() {
  }

  @Override
  public String toString() {
    return "TreatmentFacility{" + "id=" + id + ", programName=" + programName + ", legalName=" + legalName + ", addressLine1=" + street1 + ", addressLine2=" + street2 + ", city=" + city + ", state=" + state + ", zip=" + zip + ", phone=" + phone + ", fax=" + fax + ", recordId=" + recordId + ", serviceType=" + serviceType + ", residentCapacity=" + residentCapacity + ", totalOccupancy=" + totalOccupancy + ", targetPopulation=" + targetPopulation + ", expirationDate=" + expirationDate + ", ims=" + ims + '}';
  }

  @Override
  public int hashCode() {
    int hash = 7;
    hash = 29 * hash + Objects.hashCode(this.programName);
    hash = 29 * hash + Objects.hashCode(this.legalName);
    hash = 29 * hash + Objects.hashCode(this.street1);
    hash = 29 * hash + Objects.hashCode(this.street2);
    hash = 29 * hash + Objects.hashCode(this.city);
    hash = 29 * hash + Objects.hashCode(this.state);
    hash = 29 * hash + Objects.hashCode(this.zip);
    hash = 29 * hash + Objects.hashCode(this.phone);
    hash = 29 * hash + Objects.hashCode(this.fax);
    hash = 29 * hash + Objects.hashCode(this.recordId);
    hash = 29 * hash + Objects.hashCode(this.serviceType);
    hash = 29 * hash + Objects.hashCode(this.residentCapacity);
    hash = 29 * hash + Objects.hashCode(this.totalOccupancy);
    hash = 29 * hash + Objects.hashCode(this.targetPopulation);
    hash = 29 * hash + Objects.hashCode(this.expirationDate);
    hash = 29 * hash + Objects.hashCode(this.ims);
    return hash;
  }

  @Override
  public boolean equals(final Object obj) {
    if (this == obj) {
      return true;
    }
    if (obj == null) {
      return false;
    }
    if (getClass() != obj.getClass()) {
      return false;
    }
    final TreatmentFacility other = (TreatmentFacility) obj;
    if (!Objects.equals(this.recordId, other.recordId)) {
      return false;
    }
    if (!Objects.equals(this.ims, other.ims)) {
      return false;
    }
    if (!Objects.equals(this.programName, other.programName)) {
      return false;
    }
    if (!Objects.equals(this.legalName, other.legalName)) {
      return false;
    }
    if (!Objects.equals(this.street1, other.street1)) {
      return false;
    }
    if (!Objects.equals(this.street2, other.street2)) {
      return false;
    }
    if (!Objects.equals(this.city, other.city)) {
      return false;
    }
    if (!Objects.equals(this.state, other.state)) {
      return false;
    }
    if (!Objects.equals(this.zip, other.zip)) {
      return false;
    }
    if (!Objects.equals(this.phone, other.phone)) {
      return false;
    }
    if (!Objects.equals(this.fax, other.fax)) {
      return false;
    }
    if (!Objects.equals(this.serviceType, other.serviceType)) {
      return false;
    }
    if (!Objects.equals(this.targetPopulation, other.targetPopulation)) {
      return false;
    }
    if (!Objects.equals(this.residentCapacity, other.residentCapacity)) {
      return false;
    }
    if (!Objects.equals(this.totalOccupancy, other.totalOccupancy)) {
      return false;
    }
    if (!Objects.equals(this.expirationDate, other.expirationDate)) {
      return false;
    }
    return true;
  }

  public Long getId() {
    return id;
  }

  public void setId(final Long id) {
    this.id = id;
  }

  public String getRecordId() {
    return recordId;
  }

  public void setRecordId(final String recordId) {
    this.recordId = recordId;
  }

  public String getProgramName() {
    return programName;
  }

  public void setProgramName(final String programName) {
    this.programName = programName != null
            ? programName.replaceAll(System.getProperty("line.separator"), " ")
            : programName;
  }

  public String getLegalName() {
    return legalName;
  }

  public void setLegalName(final String legalName) {
    this.legalName = legalName != null
            ? legalName.replaceAll(System.getProperty("line.separator"), " ")
            : legalName;
  }

  public String getStreet1() {
    return street1;
  }

  public void setStreet1(final String street1) {
    this.street1 = street1;
  }

  public String getStreet2() {
    return street2;
  }

  public void setStreet2(final String street2) {
    this.street2 = street2;
  }

  public String getCity() {
    return city;
  }

  public void setCity(final String city) {
    this.city = city;
  }

  public String getState() {
    return state;
  }

  public void setState(final String state) {
    this.state = state;
  }

  public String getZip() {
    return zip;
  }

  public void setZip(final String zip) {
    this.zip = zip;
  }

  public String getPhone() {
    return phone;
  }

  public void setPhone(final String phone) {
    this.phone = phone;
  }

  public String getFax() {
    return fax;
  }

  public void setFax(final String fax) {
    this.fax = fax;
  }

  public String getServiceType() {
    return serviceType;
  }

  public void setServiceType(final String serviceType) {
    this.serviceType = serviceType;
  }

  public Integer getResidentCapacity() {
    return residentCapacity;
  }

  public void setResidentCapacity(final Integer residentCapacity) {
    this.residentCapacity = residentCapacity;
  }

  public Integer getTotalOccupancy() {
    return totalOccupancy;
  }

  public void setTotalOccupancy(final Integer totalOccupancy) {
    this.totalOccupancy = totalOccupancy;
  }

  public String getTargetPopulation() {
    return targetPopulation;
  }

  public void setTargetPopulation(final String targetPopulation) {
    this.targetPopulation = targetPopulation;
  }

  public String getExpirationDate() {
    return expirationDate;
  }

  public void setExpirationDate(final String expirationDate) {
    this.expirationDate = expirationDate;
  }

  public String getIms() {
    return ims;
  }

  public void setIms(final String ims) {
    this.ims = ims;
  }

}
