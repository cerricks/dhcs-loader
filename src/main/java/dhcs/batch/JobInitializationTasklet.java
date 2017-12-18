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
import java.util.List;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.batch.core.StepContribution;
import org.springframework.batch.core.scope.context.ChunkContext;
import org.springframework.batch.core.step.tasklet.Tasklet;
import org.springframework.batch.repeat.RepeatStatus;
import org.springframework.beans.factory.InitializingBean;
import org.springframework.jdbc.core.namedparam.NamedParameterJdbcTemplate;
import org.springframework.util.Assert;

/**
 * Cleans up database from previous run before job starts.
 *
 * @author Clifford Errickson
 * @since 1.0
 */
public class JobInitializationTasklet implements Tasklet, InitializingBean {

  private static final Logger LOG = LoggerFactory.getLogger(JobInitializationTasklet.class);

  private NamedParameterJdbcTemplate jdbcTemplate;

  private List<String> deleteSql;

  public JobInitializationTasklet() {
  }

  public void setJdbcTemplate(final NamedParameterJdbcTemplate jdbcTemplate) {
    this.jdbcTemplate = jdbcTemplate;
  }

  public void setDeleteSql(final List<String> deleteSql) {
    this.deleteSql = deleteSql;
  }

  @Override
  public void afterPropertiesSet() throws Exception {
    Assert.notNull(jdbcTemplate, "jdbcTemplate must be set");
    Assert.notNull(deleteSql, "deleteSql must be set");
    Assert.notEmpty(deleteSql, "deleteSql cannot be empty");
  }

  @Override
  public RepeatStatus execute(final StepContribution contribution, final ChunkContext chunkContext) throws Exception {
    if (LOG.isDebugEnabled()) {
      LOG.debug("Cleaning up database from any prior runs");
    }

    deleteSql.forEach((sql) -> {
      jdbcTemplate.update(sql, Collections.emptyMap());
    });

    return RepeatStatus.FINISHED;
  }

}
