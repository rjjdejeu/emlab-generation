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

        logger.warn("scheme is " + scheme);

        long futureStartingTenderTimePoint = getCurrentTick() + scheme.getFutureTenderOperationStartTime();
        double demandFactor;
        double targetFactor;
        Zone zone = scheme.getRegulator().getZone();

        logger.warn("Calculate Renewable Target Role started of zone: " + zone);

        ElectricitySpotMarket market = reps.marketRepository.findElectricitySpotMarketForZone(zone);

        // get demand factor
        demandFactor = predictDemandForElectricitySpotMarket(market, scheme.getRegulator()
                .getNumberOfYearsLookingBackToForecastDemand(), futureStartingTenderTimePoint);

        // get renewable energy target in factor (percent)
        RenewableTargetForTender target = reps.renewableTargetForTenderRepository
                .findRenewableTargetForTenderByRegulator(scheme.getRegulator());

        targetFactor = target.getYearlyRenewableTargetTimeSeries().getValue(futureStartingTenderTimePoint);
        logger.warn("targetFactor; " + targetFactor);

        // get totalLoad in MWh
        double totalExpectedConsumption = 0d;

        for (SegmentLoad segmentLoad : market.getLoadDurationCurve()) {
            totalExpectedConsumption += segmentLoad.getBaseLoad() * demandFactor
                    * segmentLoad.getSegment().getLengthInHours();

        }
        logger.warn("totalExpectedConsumption; " + totalExpectedConsumption);
        // renewable target for tender operation start year in MWh is

        double renewableTargetInMwh = targetFactor * totalExpectedConsumption;
        // logger.warn("renewableTargetInMwh; " + renewableTargetInMwh);

        // calculate expected generation, and subtract that from annual
        // target.
        // will be ActualTarget
        double totalExpectedGeneration = 0d;
        double totalExpectedGenerationAvailable = 0d;
        double expectedGenerationPerTechnologyAvailable = 0d;
        double expectedGenerationPerPlantAvailable = 0d;
        double totalExpectedGenerationPipeline = 0d;
        double expectedGenerationPerTechnologyPipeline = 0d;
        double expectedGenerationPerPlantPipeline = 0d;
        long numberOfSegments = reps.segmentRepository.count();
        // logger.warn("number of segments; " + numberOfSegments);

        int noOfPlants = 0;

        for (PowerGeneratingTechnology technology : scheme.getPowerGeneratingTechnologiesEligible()) {
            expectedGenerationPerTechnologyAvailable = 0d;

            logger.warn("For PGT - technology; " + technology);
            scheme.setCurrentTechnologyUnderConsideration(technology);

            for (PowerPlant plant : reps.powerPlantRepository.findExpectedOperationalPowerPlantsInMarketByTechnology(
                    market, technology, futureStartingTenderTimePoint)) {

                // logger.warn("For PP - plant; " + plant);

                expectedGenerationPerPlantAvailable = 0d;
                noOfPlants++;

                // logger.warn("FOR pp - no of plants; " + noOfPlants);

                for (Segment segment : reps.segmentRepository.findAll()) {
                    // logger.warn("For S - segment; " + segment);

                    double availablePlantCapacity = plant.getExpectedAvailableCapacity(futureStartingTenderTimePoint,
                            segment, numberOfSegments);
                    double lengthOfSegmentInHours = segment.getLengthInHours();
                    expectedGenerationPerPlantAvailable += availablePlantCapacity * lengthOfSegmentInHours;
                }
                expectedGenerationPerTechnologyAvailable += expectedGenerationPerPlantAvailable;

            }
            totalExpectedGenerationAvailable += expectedGenerationPerTechnologyAvailable;

            // logger.warn("For PGT pipeline - technology; " + technology);

            for (PowerPlant plant : reps.powerPlantRepository
                    .findExpectedOperationalPowerPlantsInMarketByTechnologyInPipeline(market, technology,
                            futureStartingTenderTimePoint)) {

                // logger.warn("For PP pipeline - plant; " + plant);

                expectedGenerationPerPlantPipeline = 0d;
                noOfPlants++;

                // logger.warn("FOR pp pipeline - no of plants; " + noOfPlants);

                for (Segment segment : reps.segmentRepository.findAll()) {
                    // logger.warn("For S - segment; " + segment);

                    double availablePlantCapacity = plant.getExpectedAvailableCapacity(futureStartingTenderTimePoint,
                            segment, numberOfSegments);
                    double lengthOfSegmentInHours = segment.getLengthInHours();
                    expectedGenerationPerPlantPipeline += availablePlantCapacity * lengthOfSegmentInHours;
                }
                expectedGenerationPerTechnologyPipeline += expectedGenerationPerPlantPipeline;

            }
            totalExpectedGenerationPipeline += expectedGenerationPerTechnologyPipeline;

        }

        totalExpectedGeneration = totalExpectedGenerationAvailable + totalExpectedGenerationPipeline;

        logger.warn("totalExpectedGenerationAvailable; " + totalExpectedGenerationAvailable);
        logger.warn("totalExpectedGenerationPipeline; " + totalExpectedGenerationPipeline);
        logger.warn("totalExpectedGeneration; " + totalExpectedGeneration);

        scheme.setExpectedRenewableGeneration(totalExpectedGeneration);

        renewableTargetInMwh = renewableTargetInMwh - totalExpectedGeneration;

        if (renewableTargetInMwh < 0) {
            renewableTargetInMwh = 0;
        }
        // scheme.getRegulator().setAnnualRenewableTargetInMwh(renewableTargetInMwh);

        scheme.setAnnualRenewableTargetInMwh(renewableTargetInMwh);

        logger.warn("actualRenewableTargetInMwh; " + renewableTargetInMwh);
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