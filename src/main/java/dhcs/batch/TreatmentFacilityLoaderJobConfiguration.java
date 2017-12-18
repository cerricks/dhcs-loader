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

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import javax.sql.DataSource;
import org.springframework.batch.core.Job;
import org.springframework.batch.core.Step;
import org.springframework.batch.core.configuration.annotation.EnableBatchProcessing;
import org.springframework.batch.core.configuration.annotation.JobBuilderFactory;
import org.springframework.batch.core.configuration.annotation.StepBuilderFactory;
import org.springframework.batch.core.configuration.annotation.StepScope;
import org.springframework.batch.core.launch.support.RunIdIncrementer;
import org.springframework.batch.core.step.tasklet.Tasklet;
import org.springframework.batch.item.ItemProcessor;
import org.springframework.batch.item.ItemReader;
import org.springframework.batch.item.ItemWriter;
import org.springframework.batch.item.database.BeanPropertyItemSqlParameterSourceProvider;
import org.springframework.batch.item.database.JdbcBatchItemWriter;
import org.springframework.batch.item.database.JdbcCursorItemReader;
import org.springframework.batch.item.file.mapping.BeanWrapperFieldSetMapper;
import org.springframework.batch.item.support.ClassifierCompositeItemWriter;
import org.springframework.batch.item.support.CompositeItemWriter;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.classify.BackToBackPatternClassifier;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.core.io.Resource;
import org.springframework.jdbc.core.namedparam.NamedParameterJdbcTemplate;

/**
 * The configuration for the TreatmentFacility loader batch job.
 *
 * @author Clifford Errickson
 * @since 1.0
 */
@Configuration
@EnableBatchProcessing
public class TreatmentFacilityLoaderJobConfiguration {

  @Value(value = "${treatmentFacility.item.regex}")
  private String regex;

  @Value(value = "${treatmentFacility.item.propertyNames}")
  private String[] propertyNames;

  @Autowired
  private JobBuilderFactory jobBuilderFactory;

  @Autowired
  private StepBuilderFactory stepBuilderFactory;

  @Autowired
  public DataSource dataSource;

  @Autowired
  private NamedParameterJdbcTemplate jdbcTemplate;

  /**
   * Listens for job completion.
   *
   * @return listens for job completion.
   */
  @Bean
  public JobStatusNotificationListener jobStatusCompletionListener() {
    return new JobStatusNotificationListener();
  }

  /**
   * Configures the {@link PdfFileRegexItemReader} used to parse the PDF report.
   *
   * @param resource the path to the PDF report (to be provided at runtime).
   * @return the configured {@link PdfFileRegexItemReader}.
   */
  @Bean
  @StepScope
  public PdfFileRegexItemReader<TreatmentFacility> treatmentFacilityPdfReportReader(@Value("file:#{jobParameters['file']}") Resource resource) {
    PdfFileRegexItemReader<TreatmentFacility> reader = new PdfFileRegexItemReader<>();
    reader.setResource(resource);
    reader.setRegex(regex);
    reader.setPropertyNames(propertyNames);

    BeanWrapperFieldSetMapper<TreatmentFacility> fieldSetMapper = new BeanWrapperFieldSetMapper<>();
    fieldSetMapper.setTargetType(TreatmentFacility.class);

    reader.setFieldSetMapper(fieldSetMapper);

    return reader;
  }

  /**
   * Configures the {@link TreatmentFacilityIdProcessor} to be used to process
   * {@link TreatmentFacility} items.
   *
   * @return the configured {@link ItemProcessor}.
   */
  @Bean
  public ItemProcessor<TreatmentFacility, TreatmentFacility> treatmentFacilityIdProcessor() {
    TreatmentFacilityIdProcessor processor = new TreatmentFacilityIdProcessor();
    processor.setJdbcTemplate(jdbcTemplate);
    processor.setSelectIdSql("select id from treatment_facilities where record_id = :recordId and program_name = :programName and address_street1 = :street1");

    return processor;
  }

  /**
   * Configure the {@link ItemReader} used to read {@link TreatmentFacility}
   * items in main storage that are NOT in temporary storage (i.e. they are to
   * be removed from main storage).
   *
   * @return the configured {@link ItemReader}.
   */
  @Bean
  public ItemReader<TreatmentFacility> removedTreatmentFacilityItemReader() {
    JdbcCursorItemReader reader = new JdbcCursorItemReader<>();
    reader.setSql("SELECT tf.* FROM TREATMENT_FACILITIES tf LEFT JOIN TREATMENT_FACILITIES_TEMP tft ON tft.RECORD_ID = tf.RECORD_ID and tft.PROGRAM_NAME = tf.PROGRAM_NAME and tft.ADDRESS_STREET1 = tf.ADDRESS_STREET1 WHERE tft.RECORD_ID IS NULL and tft.PROGRAM_NAME is NULL and tft.ADDRESS_STREET1 is NULL");
    reader.setRowMapper(new TreatmentFacilityRowMapper());
    reader.setDataSource(dataSource);
    return reader;
  }

  /**
   * Reads {@link TreatmentFacility} items from temporary storage.
   *
   * @return the {@link ItemWriter}.
   */
  @Bean
  public ItemReader<TreatmentFacility> tempTreatmentFacilityReader() {
    JdbcCursorItemReader reader = new JdbcCursorItemReader<>();
    reader.setSql("SELECT * FROM TREATMENT_FACILITIES_TEMP ORDER BY RECORD_ID, PROGRAM_NAME, ADDRESS_STREET1");
    reader.setRowMapper(new TreatmentFacilityRowMapper());
    reader.setDataSource(dataSource);
    return reader;
  }

  /**
   * Writes {@link TreatmentFacility} items to temporary storage.
   *
   * @return the {@link ItemWriter}.
   */
  @Bean
  public ItemWriter<TreatmentFacility> tempTreatmentFacilityWriter() {
    JdbcBatchItemWriter<TreatmentFacility> writer = new JdbcBatchItemWriter<>();
    writer.setItemSqlParameterSourceProvider(new BeanPropertyItemSqlParameterSourceProvider<>());
    writer.setSql("INSERT INTO TREATMENT_FACILITIES_TEMP (RECORD_ID, PROGRAM_NAME, LEGAL_NAME, ADDRESS_STREET1, ADDRESS_STREET2, ADDRESS_CITY, ADDRESS_STATE, ADDRESS_ZIP, PHONE, FAX, SERVICE_TYPE, TARGET_POPULATION, RESIDENT_CAPACITY, TOTAL_OCCUPANCY, IMS, EXPIRATION_DATE) "
            + "VALUES (:recordId, :programName, :legalName, :street1, :street2, :city, :state, :zip, :phone, :fax, :serviceType, :targetPopulation, :residentCapacity, :totalOccupancy, :ims, :expirationDate)");
    writer.setDataSource(dataSource);
    return writer;
  }

  /**
   * Writes {@link TreatmentFacility} items to main storage.
   *
   * @return the {@link ItemWriter}.
   */
  @Bean
  public ItemWriter<TreatmentFacility> treatmentFacilityItemWriter() {
    BackToBackPatternClassifier classifier = new BackToBackPatternClassifier();
    classifier.setRouterDelegate(new TreatmentFacilityExistsClassifier());

    JdbcBatchItemWriter<TreatmentFacility> addedTreatmentFacilityItemWriter = new JdbcBatchItemWriter<>();
    addedTreatmentFacilityItemWriter.setItemSqlParameterSourceProvider(new BeanPropertyItemSqlParameterSourceProvider<>());
    addedTreatmentFacilityItemWriter.setSql("INSERT INTO TREATMENT_FACILITIES (RECORD_ID, PROGRAM_NAME, LEGAL_NAME, ADDRESS_STREET1, ADDRESS_STREET2, ADDRESS_CITY, ADDRESS_STATE, ADDRESS_ZIP, PHONE, FAX, SERVICE_TYPE, TARGET_POPULATION, RESIDENT_CAPACITY, TOTAL_OCCUPANCY, IMS, EXPIRATION_DATE) "
            + "VALUES (:recordId, :programName, :legalName, :street1, :street2, :city, :state, :zip, :phone, :fax, :serviceType, :targetPopulation, :residentCapacity, :totalOccupancy, :ims, :expirationDate)");
    addedTreatmentFacilityItemWriter.setDataSource(dataSource);
    addedTreatmentFacilityItemWriter.afterPropertiesSet();

    JdbcBatchItemWriter<TreatmentFacility> addedTreatmentFacilityItemLogger = new JdbcBatchItemWriter<>();
    addedTreatmentFacilityItemLogger.setItemSqlParameterSourceProvider(new BeanPropertyItemSqlParameterSourceProvider<>());
    addedTreatmentFacilityItemLogger.setSql("INSERT INTO TREATMENT_FACILITIES_LOG (TRANS_TYPE, RECORD_ID, PROGRAM_NAME, LEGAL_NAME, ADDRESS_STREET1, ADDRESS_STREET2, ADDRESS_CITY, ADDRESS_STATE, ADDRESS_ZIP, PHONE, FAX, SERVICE_TYPE, TARGET_POPULATION, RESIDENT_CAPACITY, TOTAL_OCCUPANCY, IMS, EXPIRATION_DATE) "
            + "VALUES ('ADDED', :recordId, :programName, :legalName, :street1, :street2, :city, :state, :zip, :phone, :fax, :serviceType, :targetPopulation, :residentCapacity, :totalOccupancy, :ims, :expirationDate)");
    addedTreatmentFacilityItemLogger.setDataSource(dataSource);
    addedTreatmentFacilityItemLogger.afterPropertiesSet();

    CompositeItemWriter<TreatmentFacility> addedItemWriter = new CompositeItemWriter<>();
    addedItemWriter.setDelegates(Arrays.asList(addedTreatmentFacilityItemWriter, addedTreatmentFacilityItemLogger));

    JdbcBatchItemWriter<TreatmentFacility> updatedTreatmentFacilityItemWriter = new JdbcBatchItemWriter<>();
    updatedTreatmentFacilityItemWriter.setItemSqlParameterSourceProvider(new BeanPropertyItemSqlParameterSourceProvider<>());
    updatedTreatmentFacilityItemWriter.setSql("UPDATE TREATMENT_FACILITIES SET RECORD_ID=:recordId, PROGRAM_NAME=:programName, LEGAL_NAME=:legalName, ADDRESS_STREET1=:street1, ADDRESS_STREET2=:street2, ADDRESS_CITY=:city, ADDRESS_STATE=:state, ADDRESS_ZIP=:zip, PHONE=:phone, FAX=:fax, SERVICE_TYPE=:serviceType, TARGET_POPULATION=:targetPopulation, RESIDENT_CAPACITY=:residentCapacity, TOTAL_OCCUPANCY=:totalOccupancy, IMS=:ims, EXPIRATION_DATE=:expirationDate WHERE RECORD_ID=:recordId and PROGRAM_NAME=:programName and ADDRESS_STREET1=:street1");
    updatedTreatmentFacilityItemWriter.setDataSource(dataSource);
    updatedTreatmentFacilityItemWriter.afterPropertiesSet();

    Map<String, ItemWriter<TreatmentFacility>> matcherMap = new HashMap<>();
    matcherMap.put(Boolean.TRUE.toString(), updatedTreatmentFacilityItemWriter);
    matcherMap.put(Boolean.FALSE.toString(), addedItemWriter);

    classifier.setMatcherMap(matcherMap);

    ClassifierCompositeItemWriter<TreatmentFacility> compositeItemWriter = new ClassifierCompositeItemWriter();
    compositeItemWriter.setClassifier(classifier);

    return compositeItemWriter;
  }

  /**
   * Configure the {@link ItemWriter} to remove {@link TreatmentFacility} items
   * from main storage and log activity.
   *
   * @return the configured {@link ItemWriter}.
   */
  @Bean
  public ItemWriter<TreatmentFacility> removedTreatmentFacilityItemWriter() {
    JdbcBatchItemWriter<TreatmentFacility> deleteItemWriter = new JdbcBatchItemWriter<>();
    deleteItemWriter.setItemSqlParameterSourceProvider(new BeanPropertyItemSqlParameterSourceProvider<>());
    deleteItemWriter.setSql("DELETE FROM TREATMENT_FACILITIES WHERE RECORD_ID = :recordId and PROGRAM_NAME = :programName and ADDRESS_STREET1 = :street1");
    deleteItemWriter.setDataSource(dataSource);
    deleteItemWriter.afterPropertiesSet();

    JdbcBatchItemWriter<TreatmentFacility> deleteItemLogger = new JdbcBatchItemWriter<>();
    deleteItemLogger.setItemSqlParameterSourceProvider(new BeanPropertyItemSqlParameterSourceProvider<>());
    deleteItemLogger.setSql("INSERT INTO TREATMENT_FACILITIES_LOG (TRANS_TYPE, RECORD_ID, PROGRAM_NAME, LEGAL_NAME, ADDRESS_STREET1, ADDRESS_STREET2, ADDRESS_CITY, ADDRESS_STATE, ADDRESS_ZIP, PHONE, FAX, SERVICE_TYPE, TARGET_POPULATION, RESIDENT_CAPACITY, TOTAL_OCCUPANCY, IMS, EXPIRATION_DATE) "
            + "VALUES ('REMOVED', :recordId, :programName, :legalName, :street1, :street2, :city, :state, :zip, :phone, :fax, :serviceType, :targetPopulation, :residentCapacity, :totalOccupancy, :ims, :expirationDate)");
    deleteItemLogger.setDataSource(dataSource);
    deleteItemLogger.afterPropertiesSet();

    CompositeItemWriter<TreatmentFacility> compositeItemWriter = new CompositeItemWriter<>();
    compositeItemWriter.setDelegates(Arrays.asList(deleteItemWriter, deleteItemLogger));

    return compositeItemWriter;
  }

  @Bean
  public Tasklet jobInitializationTasklet() {
    JobInitializationTasklet tasklet = new JobInitializationTasklet();

    tasklet.setJdbcTemplate(jdbcTemplate);

    List<String> deleteSql = new ArrayList();
    deleteSql.add("delete from treatment_facilities_temp");
    deleteSql.add("delete from treatment_facilities_log");

    tasklet.setDeleteSql(deleteSql);

    return tasklet;
  }

  /**
   * Configures the flow of this job.
   *
   * @return job configuration.
   */
  @Bean
  public Job importTreatmentFacilityJob() {
    return jobBuilderFactory.get("importTreatmentFacilityJob")
            .incrementer(new RunIdIncrementer())
            .listener(jobStatusCompletionListener())
            .flow(preRunCleanup())
            .next(loadToTempStorage())
            .next(loadToMainStorage())
            .next(processRemoved())
            .end()
            .build();
  }

  /**
   * Clean up database prior to run.
   *
   * @return step configuration.
   */
  @Bean
  public Step preRunCleanup() {
    return stepBuilderFactory.get("preRunCleanup")
            .tasklet(jobInitializationTasklet())
            .build();
  }

  /**
   * Reads items from PDF and writes them to temporary storage.
   *
   * @return step configuration.
   */
  @Bean
  public Step loadToTempStorage() {
    return stepBuilderFactory.get("loadToTemp")
            .<TreatmentFacility, TreatmentFacility>chunk(100)
            .reader(treatmentFacilityPdfReportReader(null)) // actual value will be injected (not null)
            .writer(tempTreatmentFacilityWriter())
            .build();
  }

  /**
   * Reads items from temporary storage, determines if they exist in the main
   * storage, adds/updates item to main storage and log (if necessary).
   *
   * @return step configuration.
   */
  @Bean
  public Step loadToMainStorage() {
    return stepBuilderFactory.get("loadToMainStorage")
            .<TreatmentFacility, TreatmentFacility>chunk(100)
            .reader(tempTreatmentFacilityReader())
            .processor(treatmentFacilityIdProcessor())
            .writer(treatmentFacilityItemWriter())
            .build();
  }

  /**
   * Reads items to be removed from main storage, removes them and then adds
   * record to log.
   *
   * @return step configuration.
   */
  @Bean
  public Step processRemoved() {
    return stepBuilderFactory.get("processRemoved")
            .<TreatmentFacility, TreatmentFacility>chunk(100)
            .reader(removedTreatmentFacilityItemReader())
            .writer(removedTreatmentFacilityItemWriter())
            .build();
  }

}
