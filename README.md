# dhcs-loader
A spring-batch utility for loading treatment facility data extracted from a report.

## Directory Layout

The project is structured as follows:

| Path | Description |
| ---- | ----------- |
| config | Application configuration files |
| logs | Application output files (generated on run) |
| src/main | Application sources |
| src/main/resources | Application resources |
| target | Contains compiled source (generated on build) |

## Configuration

Various settings may be configured externally by modifying parameters in **config/application.properties**.

| Parameter Name | Description |
| -------------- | ----------- |
| spring.datasource.url | The URL of the database |
| spring.datasource.username | The ID of the database user |
| spring.datasource.password | The password of the database user |
| logging.config | The name/path to the logback configuration file |
| logging.file | The name/path of the external log file |
| treatmentFacility.item.regex | A regular expression pattern used to match a single record |
| treatmentFacility.item.propertyNames | A list of group names matched in **treatmentFacility.item.regex** |
| titleCase.name.lowerCaseExceptions | A comma separated list of words in a name to convert to lower case |
| titleCase.name.upperCaseExceptions | A comma separated list of words in a name to convert to upper case |
| titleCase.address.lowerCaseExceptions | A comma separated list of words in an address to convert to lower case |
| titleCase.address.upperCaseExceptions | A comma separated list of words in an address to convert to upper case |

## Usage
```
java -jar dhcs-loader-1.1.jar file=<path-to-input-file>
```

## Database

### Schema

See: **config/schema.sql**

| Table Name | Description |
| ---------- | ----------- |
| treatment_facilities | The current/up-to-date details. |
| treatment_facilities_temp | Temporary storage for processing. Cleared before every run. |
| treatment_facilities_log | Log for records that are added and removed from **treatment_facilities**. Cleared before every run. |

### Spring Batch Metadata Tables

See: https://docs.spring.io/spring-batch/trunk/reference/html/metaDataSchema.html

## Logging

During execution, log statements will be written to file logs/dhcs-loader.log. The default level for logged statements is **WARN**. Modify **config/logback.xml** for greater control over logging.
