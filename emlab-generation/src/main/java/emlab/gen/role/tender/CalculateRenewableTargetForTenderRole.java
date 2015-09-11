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

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.transaction.annotation.Transactional;

import agentspring.role.AbstractRole;
import agentspring.role.Role;
import agentspring.role.RoleComponent;
import emlab.gen.domain.gis.Zone;
import emlab.gen.domain.market.electricity.ElectricitySpotMarket;
import emlab.gen.domain.market.electricity.Segment;
import emlab.gen.domain.market.electricity.SegmentLoad;
import emlab.gen.domain.policy.renewablesupport.RenewableSupportSchemeTender;
import emlab.gen.domain.policy.renewablesupport.RenewableTargetForTender;
import emlab.gen.domain.technology.PowerGeneratingTechnology;
import emlab.gen.domain.technology.PowerPlant;
import emlab.gen.repository.Reps;
import emlab.gen.util.GeometricTrendRegression;

/**
 * @author Kaveri, rjjdejeu
 *
 */

@RoleComponent
public class CalculateRenewableTargetForTenderRole extends AbstractRole<RenewableSupportSchemeTender> implements
        Role<RenewableSupportSchemeTender> {

    @Autowired
    Reps reps;

    @Override
    @Transactional
    public void act(RenewableSupportSchemeTender scheme) {

        logger.warn("Calculate Renewable Target Role started");

        double demandFactor;
        double targetFactor;
        Zone zone = scheme.getRegulator().getZone();

        logger.warn("Calculate Renewable Target Role started of zone: " + zone);

        ElectricitySpotMarket market = reps.marketRepository.findElectricitySpotMarketForZone(zone);

        logger.warn("electricity spot market is: " + market);

        // get demand factor
        demandFactor = predictDemandForElectricitySpotMarket(market, scheme.getRegulator()
                .getNumberOfYearsLookingBackToForecastDemand(), scheme.getFutureTenderOperationStartTime());

        logger.warn("demandFactor for this tick: " + demandFactor);

        /*
         * it aggregates segments from both countries, so the boolean should
         * actually be true here and the code adjusted to FALSE case. Or a query
         * should be adjusted what probably will take less time.
         */

        

            // get renewable energy target in factor (percent)
            RenewableTargetForTender target = reps.renewableTargetForTenderRepository
                    .findRenewableTargetForTenderByRegulator(scheme.getRegulator());

            targetFactor = target.getYearlyRenewableTargetTimeSeries().getValue(
                    getCurrentTick() + scheme.getFutureTenderOperationStartTime());

            // logger.warn("future tender operations start time is: "
            // + (getCurrentTick() +
            // scheme.getFutureTenderOperationStartTime()));

            logger.warn("targetFactor for this tick: " + targetFactor);

            // get totalLoad in MWh
            double totalConsumption = 0;
            for (SegmentLoad segmentLoad : reps.segmentLoadRepository.findAll()) {

                totalConsumption += segmentLoad.getBaseLoad() * demandFactor
                        * segmentLoad.getSegment().getLengthInHours();

                logger.warn("segment hours are :" + segmentLoad.getSegment().getLengthInHours());

                logger.warn("segment load is: " + segmentLoad.getBaseLoad());

            }

            logger.warn("totalConsumption for this tick: " + totalConsumption);

            // renewable target for tender operation start year in MWh is
            double renewableTargetInMwh = targetFactor * totalConsumption;

            // logger.warn("renewableTargetInMwh for this tick: " +
            // renewableTargetInMwh);

            // calculate expected generation, and subtract that from annual
            // target.
            // will be ActualTarget
            double expectedGenerationPerTechnology = 0d;
            double totalExpectedGeneration = 0d;
            long numberOfSegments = reps.segmentRepository.count();
            double factor = 0d;
            double fullLoadHours = 0d;

            for (PowerGeneratingTechnology technology : scheme.getPowerGeneratingTechnologiesEligible()) {

                logger.warn("eligble techs are: " + scheme.getPowerGeneratingTechnologiesEligible());

                double expectedTechnologyCapacity = reps.powerPlantRepository
                        .calculateCapacityOfOperationalPowerPlantsByTechnology(technology,
                                scheme.getFutureTenderOperationStartTime());

                logger.warn("expectedTechnologyCapacity is: " + expectedTechnologyCapacity);

                for (PowerPlant plant : reps.powerPlantRepository.findOperationalPowerPlantsByTechnology(technology,
                        scheme.getFutureTenderOperationStartTime())) {
                    for (Segment segment : reps.segmentRepository.findAll()) {

                        if (technology.isIntermittent()) {
                            factor = plant.getIntermittentTechnologyNodeLoadFactor().getLoadFactorForSegment(segment);
                            logger.warn("technology.isIntermittent? (this logger should not happen): "
                                    + technology.isIntermittent());

                        } else {
                            double segmentID = segment.getSegmentID();
                            double min = technology.getPeakSegmentDependentAvailability();
                            double max = technology.getBaseSegmentDependentAvailability();
                            double segmentPortion = (numberOfSegments - segmentID) / (numberOfSegments - 1); // start
                            // counting
                            // at
                            // 1.

                            double range = max - min;
                            factor = max - segmentPortion * range;

                            // logger.warn("factor is: " + factor);

                        }

                        fullLoadHours += factor * segment.getLengthInHours();

                        // logger.warn("segment.getLengthInHours is: " +
                        // segment.getLengthInHours());

                        // logger.warn("fullLoadHours is: " + fullLoadHours);
                    }
                }
                expectedGenerationPerTechnology = fullLoadHours * expectedTechnologyCapacity;

                totalExpectedGeneration += expectedGenerationPerTechnology;
            }

            logger.warn("renwabletargetInMwh is: " + renewableTargetInMwh);
            logger.warn("totalExpectedGeneration is: " + totalExpectedGeneration);

            /*
             * To compare
             * 
             * Electricity consumption in 2011 was for 1) NL: 1.17E+08 MWh 2)
             * DE: 5.79E+08 MWh 3) source
             * http://www.indexmundi.com/facts/indicators
             * /EG.USE.ELEC.KH/rankings
             * 
             * Total electricity production in 2011 for 1) NL: 1.09E+08 MWh 2)
             * DE: 5.77+08 MWh 3) source
             * http://ec.europa.eu/eurostat/statistics-explained/images
             * /d/d9/Net_electricity_generation
             * %2C_1990%E2%80%932013_%28thousand_GWh%29_YB15.png
             * 
             * Total RENEWABLE electricity production in 2010 for 1) NL:
             * 1.04E+07 MWh DE: 1.05E+08 MWh 2) sources
             * http://www.cbs.nl/NR/rdonlyres
             * /BED23760-23C0-47D0-8A2A-224402F055F 3/0/2012c90pub.pdf
             * https://en.wikipedia.org/wiki/Renewable_energy_in_Germany#Sources
             * 
             * totalExpectedGeneration EMLab RES-E for 2020 1) NL: 2.51E+10 MWh
             * 2) DE: 2.51E+10 MWh
             * 
             * Conclusions: 0) Although I compare 2010 with 2020, the numbers
             * should be more or less in the same ball park, and they are not.
             * 1) There is a factor 1000 too much in EMLab most likely,
             * originating from totalExpectingGeneration 2) Also, NL and DE
             * start with the same totalExpectedGeneration, which could not be
             * right. Probably due to the initial portfolios.
             */

            renewableTargetInMwh = renewableTargetInMwh - totalExpectedGeneration;

            if (renewableTargetInMwh < 0) {
                renewableTargetInMwh = 0;
            }
            scheme.getRegulator().setAnnualRenewableTargetInMwh(renewableTargetInMwh);

            logger.warn("actual renewableTargetInMwh for this tick: " + renewableTargetInMwh);
        }
    

    public double predictDemandForElectricitySpotMarket(ElectricitySpotMarket market,
            long numberOfYearsBacklookingForForecasting, long futureTimePoint) {
        GeometricTrendRegression gtr = new GeometricTrendRegression();
        for (long time = getCurrentTick(); time > getCurrentTick() - numberOfYearsBacklookingForForecasting
                && time >= 0; time = time - 1) {
            gtr.addData(time, market.getDemandGrowthTrend().getValue(time));
        }
        double forecast = gtr.predict(futureTimePoint);
        if (Double.isNaN(forecast))
            forecast = market.getDemandGrowthTrend().getValue(getCurrentTick());
        return forecast;
    }
}
