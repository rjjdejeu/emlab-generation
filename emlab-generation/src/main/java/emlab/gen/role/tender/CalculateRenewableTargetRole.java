/*******************************************************************************
 * Copyright 2013 the original author or authors.
 * 
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 * 
 *   http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 ******************************************************************************/
package emlab.gen.role.tender;

import java.util.HashMap;
import java.util.Map;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.transaction.annotation.Transactional;

import agentspring.role.AbstractRole;
import agentspring.role.Role;
import agentspring.role.RoleComponent;
import emlab.gen.domain.agent.Regulator;
import emlab.gen.domain.gis.Zone;
import emlab.gen.domain.market.electricity.ElectricitySpotMarket;
import emlab.gen.repository.Reps;
import emlab.gen.util.GeometricTrendRegression;

/**
 * @author rjjdejeu
 *
 */

@RoleComponent
public class CalculateRenewableTargetRole extends AbstractRole<Regulator> implements Role<Regulator> {

    @Autowired
    Reps reps;

    @Override
    @Transactional
    public void act(Regulator regulator) {

        // Zone is the country
        Zone zone = regulator.getZone();
        ElectricitySpotMarket market = reps.marketRepository.findElectricitySpotMarketForZone(zone);

        /*
         * Gets the demand for a 5 years ahead (or whatever the maximum total
         * permit + lead time is) E.g. it is now 2015, the regulator want the
         * tender start clearing and thus paying from 2020 onwards.
         */

        Map<ElectricitySpotMarket, Double> expectedDemand = new HashMap<ElectricitySpotMarket, Double>();
        for (ElectricitySpotMarket elm : reps.template.findAll(ElectricitySpotMarket.class)) {
            GeometricTrendRegression gtr = new GeometricTrendRegression();
            for (long time = getCurrentTick(); time > getCurrentTick() - 5 // regulator.getNumberOfYearsBacklookingForForecasting()
                    && time >= 0; time = time - 1) {
                gtr.addData(time, elm.getDemandGrowthTrend().getValue(time));
            }
            // 5 years ahead due to lead times + permits times or less or more??
            // I changed regulator.getInvestmentFutureTimeHorizon() into 5 years
            long futureTimePoint = getCurrentTick() + 5;

            /*
             * OR I can maybe use 
             * long futureTimePoint = getCurrentTick()+pgt.getExpectedLeadtime() + pgt.getExpectedPermittime();
             * (found in TargetInvestmentRole.java line 52)
             */

            expectedDemand.put(elm, gtr.predict(futureTimePoint));
        }

        /*
         * Gets the established renewable energy targets for NL and DE from a
         * stored file in data/policyNREAP_DE_2050.csv and
         * data/policyNREAP_NL_2050.csv which are stored as a scenario in the
         * file Tender.xml
         * 
         * I based this the following on, PowerGeneratingTechnologyTarget.java line 43
         */

        @RelatedTo(type = "TARGET_TREND", elementClass = TimeSeriesImpl.class, direction = Direction.OUTGOING)
        TimeSeriesImpl trend;
        public TimeSeriesImpl getTrend() {
        return trend;
        }
        
        
         double targetFactor = //getTender.xml;

        // Calculates the target for the current tick.
        double renewableTarget = expectedDemand.getCurrentTick() * targetFactor.getCurrentTick();

    }
}