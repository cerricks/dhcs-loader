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

import java.util.HashMap;
import java.util.Map;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.batch.item.ItemProcessor;
import org.springframework.beans.factory.InitializingBean;
import org.springframework.dao.EmptyResultDataAccessException;
import org.springframework.jdbc.core.namedparam.NamedParameterJdbcTemplate;
import org.springframework.util.Assert;

/**
 * An implementation of {@link ItemProcessor} that sets a
 * {@link TreatmentFacility} item's {@code id} value if a record exists.
 *
 * @author Clifford Errickson
 * @since 1.0
 */
public class TreatmentFacilityIdProcessor implements ItemProcessor<TreatmentFacility, TreatmentFacility>, InitializingBean {

  private static final Logger LOG = LoggerFactory.getLogger(TreatmentFacilityIdProcessor.class);

  private NamedParameterJdbcTemplate jdbcTemplate;

  private String selectIdSql;

  public TreatmentFacilityIdProcessor() {
  }

  @Override
  public void afterPropertiesSet() throws Exception {
    Assert.notNull(jdbcTemplate, "jdbcTemplate must be set");
    Assert.notNull(selectIdSql, "selectIdSql must be set");
  }

  /**
   * Set the {@link NamedParameterJdbcTemplate} used by this processor.
   *
   * @param jdbcTemplate the {@link NamedParameterJdbcTemplate} used by this
   * processor.
   */
  public void setJdbcTemplate(final NamedParameterJdbcTemplate jdbcTemplate) {
    this.jdbcTemplate = jdbcTemplate;
  }

  /**
   * Set the SQL statement used to select a {@link TreatmentFacility} id.
   *
   * @param selectIdSql the SQL statement used to select a
   * {@link TreatmentFacility} id.
   */
  public void setSelectIdSql(final String selectIdSql) {
    this.selectIdSql = selectIdSql;
  }

  /**
   * Executes the {@code selectIdSql} using the {@code jdbcTemplate}. If a value
   * is found, the ID of the given {@link TreatmentFacility} item will be set to
   * that value.
   *
   * @param item the item being processed.
   * @return the processed item.
   * @throws Exception on error
   */
  @Override
  public TreatmentFacility process(final TreatmentFacility item) throws Exception {
    Map<String, Object> params = new HashMap<>();
    params.put("recordId", item.getRecordId());
    params.put("programName", item.getProgramName());
    params.put("street1", item.getStreet1());

    Long id;

    try {
      id = jdbcTemplate.queryForObject(selectIdSql, params, Long.class);
    } catch (EmptyResultDataAccessException ex) {
      if (LOG.isTraceEnabled()) {
        LOG.trace(ex.getMessage());
      }

      id = null;
    }

    if (id != null) {
      item.setId(id);
    }

    return item;
  }

}
