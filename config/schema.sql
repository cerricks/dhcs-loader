drop table if exists treatment_facilities;
drop table if exists treatment_facilities_temp;
drop table if exists treatment_facilities_log;

CREATE TABLE treatment_facilities (
  id int NOT NULL AUTO_INCREMENT,
  record_id varchar(255) NOT NULL,
  program_name varchar(255) NOT NULL,
  legal_name varchar(255) DEFAULT NULL,
  address_street1 varchar(255) NOT NULL,
  address_street2 varchar(255) DEFAULT NULL,
  address_city varchar(255) DEFAULT NULL,
  address_state varchar(255) DEFAULT NULL,
  address_zip varchar(255) DEFAULT NULL,
  phone varchar(255) DEFAULT NULL,
  fax varchar(255) DEFAULT NULL,
  service_type varchar(255) DEFAULT NULL,
  target_population varchar(255) DEFAULT NULL,
  resident_capacity int(11) DEFAULT NULL,
  total_occupancy int(11) DEFAULT NULL,
  expiration_date varchar(255),
  ims varchar(255) DEFAULT NULL,
  PRIMARY KEY (id),
  CONSTRAINT uc_record_id UNIQUE (record_id,program_name,address_street1)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;

CREATE TABLE treatment_facilities_temp (
  record_id varchar(255) DEFAULT NULL,
  program_name varchar(255) DEFAULT NULL,
  legal_name varchar(255) DEFAULT NULL,
  address_street1 varchar(255) DEFAULT NULL,
  address_street2 varchar(255) DEFAULT NULL,
  address_city varchar(255) DEFAULT NULL,
  address_state varchar(255) DEFAULT NULL,
  address_zip varchar(255) DEFAULT NULL,
  phone varchar(255) DEFAULT NULL,
  fax varchar(255) DEFAULT NULL,
  service_type varchar(255) DEFAULT NULL,
  target_population varchar(255) DEFAULT NULL,
  resident_capacity int(11) DEFAULT NULL,
  total_occupancy int(11) DEFAULT NULL,
  expiration_date varchar(255),
  ims varchar(255) DEFAULT NULL
) ENGINE=InnoDB DEFAULT CHARSET=utf8;

CREATE TABLE treatment_facilities_log (
  trans_type varchar(255) DEFAULT NULL,
  record_id varchar(255) DEFAULT NULL,
  program_name varchar(255) DEFAULT NULL,
  legal_name varchar(255) DEFAULT NULL,
  address_street1 varchar(255) DEFAULT NULL,
  address_street2 varchar(255) DEFAULT NULL,
  address_city varchar(255) DEFAULT NULL,
  address_state varchar(255) DEFAULT NULL,
  address_zip varchar(255) DEFAULT NULL,
  phone varchar(255) DEFAULT NULL,
  fax varchar(255) DEFAULT NULL,
  service_type varchar(255) DEFAULT NULL,
  target_population varchar(255) DEFAULT NULL,
  resident_capacity int(11) DEFAULT NULL,
  total_occupancy int(11) DEFAULT NULL,
  expiration_date varchar(255),
  ims varchar(255) DEFAULT NULL
) ENGINE=InnoDB DEFAULT CHARSET=utf8;

