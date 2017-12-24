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

import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.batch.core.JobExecution;
import org.springframework.batch.core.listener.JobExecutionListenerSupport;
import org.springframework.beans.factory.InitializingBean;
import org.springframework.jdbc.core.namedparam.NamedParameterJdbcTemplate;
import org.springframework.util.Assert;

/**
 * Listens for job start and end and logs notification messages.
 *
 * @author Clifford Errickson
 * @since 1.0
 */
public class JobStatusNotificationListener extends JobExecutionListenerSupport implements InitializingBean {

  private static final Logger LOG = LoggerFactory.getLogger(JobStatusNotificationListener.class);

  private NamedParameterJdbcTemplate jdbcTemplate;

  public JobStatusNotificationListener() {
  }

  @Override
  public void afterPropertiesSet() throws Exception {
    Assert.notNull(jdbcTemplate, "jdbcTemplate must be set");
  }

  public void setJdbcTemplate(final NamedParameterJdbcTemplate jdbcTemplate) {
    this.jdbcTemplate = jdbcTemplate;
  }

  @Override
  public void beforeJob(final JobExecution jobExecution) {
    if (LOG.isInfoEnabled()) {
      LOG.info("Job [" + jobExecution.getJobInstance().getJobName() + "] started with ID [" + jobExecution.getId() + "]");
    }
  }

  @Override
  public void afterJob(final JobExecution jobExecution) {
    if (LOG.isInfoEnabled()) {
      LOG.info("Job [" + jobExecution.getJobInstance().getJobName() + "] finished with status [" + jobExecution.getStatus() + "]");

      switch (jobExecution.getStatus()) {
        case COMPLETED:
          Map<String, Object> params = new HashMap<>();
          params.put("jobExecutionId", jobExecution.getId());

          int totalCount = jdbcTemplate.queryForObject("select count(*) from treatment_facilities", Collections.emptyMap(), Integer.class);
          int addCount = jdbcTemplate.queryForObject("select count(*) from treatment_facilities_log where trans_type='ADDED' and job_execution_id=:jobExecutionId", params, Integer.class);
          int removedCount = jdbcTemplate.queryForObject("select count(*) from treatment_facilities_log where trans_type='REMOVED' and job_execution_id=:jobExecutionId", params, Integer.class);

          LOG.info("Results: total records processed = " + totalCount + ", new records = " + addCount + ", removed records = " + removedCount);

          break;

        default:
          LOG.info("Check logs for additional information");
      }
    }
  }

}
