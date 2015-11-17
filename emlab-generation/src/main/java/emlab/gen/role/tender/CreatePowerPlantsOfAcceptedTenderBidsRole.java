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
import org.springframework.data.annotation.Transient;
import org.springframework.data.neo4j.support.Neo4jTemplate;

import agentspring.role.AbstractRole;
import agentspring.role.Role;
import agentspring.role.RoleComponent;
import emlab.gen.domain.agent.BigBank;
import emlab.gen.domain.agent.EnergyProducer;
import emlab.gen.domain.agent.PowerPlantManufacturer;
import emlab.gen.domain.agent.Regulator;
import emlab.gen.domain.contract.CashFlow;
import emlab.gen.domain.contract.Loan;
import emlab.gen.domain.gis.Zone;
import emlab.gen.domain.market.Bid;
import emlab.gen.domain.policy.renewablesupport.RenewableSupportSchemeTender;
import emlab.gen.domain.policy.renewablesupport.TenderBid;
import emlab.gen.domain.technology.PowerPlant;
import emlab.gen.repository.Reps;

/**
 * @author rjjdejeu
 *
 */
@RoleComponent
public class CreatePowerPlantsOfAcceptedTenderBidsRole extends AbstractRole<Regulator> implements Role<Regulator> {

    @Transient
    @Autowired
    Reps reps;

    @Transient
    @Autowired
    Neo4jTemplate template;

    @Override
    public void act(Regulator regulator) {

        logger.warn("Create Power Plants Of Accepted Tender Bids Role started for: " + regulator);

        Zone zone = regulator.getZone();
        RenewableSupportSchemeTender scheme = reps.renewableSupportSchemeTenderRepository
                .determineSupportSchemeForZone(zone);

        // Initialize the accepted bids
        Iterable<TenderBid> acceptedTenderBidsByTime = null;
        acceptedTenderBidsByTime = reps.tenderBidRepository.findAllAcceptedTenderBidsByTime(scheme, getCurrentTick());

        for (TenderBid currentTenderBid : acceptedTenderBidsByTime) {

            logger.warn("current accepted bid: " + currentTenderBid);

            PowerPlant plant = currentTenderBid.getPowerPlant();
            // PowerPlant plant = new PowerPlant();
            // plant.setRenewableTenderDummyPowerPlant(false);

            EnergyProducer bidder = (EnergyProducer) currentTenderBid.getBidder();
            // check if all information exists
            plant.specifyAndPersist(currentTenderBid.getStart(), bidder, currentTenderBid.getPowerGridNode(),
                    currentTenderBid.getTechnology());
            PowerPlantManufacturer manufacturer = reps.genericRepository.findFirst(PowerPlantManufacturer.class);
            BigBank bigbank = reps.genericRepository.findFirst(BigBank.class);

            double investmentCostPayedByEquity = plant.getActualInvestedCapital()
                    * (1 - bidder.getDebtRatioOfInvestments());
            double investmentCostPayedByDebt = plant.getActualInvestedCapital() * bidder.getDebtRatioOfInvestments();
            double downPayment = investmentCostPayedByEquity;
            createSpreadOutDownPayments(bidder, manufacturer, downPayment, plant);

            double amount = determineLoanAnnuities(investmentCostPayedByDebt, plant.getTechnology()
                    .getDepreciationTime(), bidder.getLoanInterestRate());
            // logger.warn("Loan amount is: " + amount);
            Loan loan = reps.loanRepository.createLoan(currentTenderBid.getBidder(), bigbank, amount, plant
                    .getTechnology().getDepreciationTime(), getCurrentTick(), plant);
            // Create the loan
            plant.createOrUpdateLoan(loan);

            logger.warn("CreatingPowerPlant 69 - Agent " + bidder + " at tick " + getCurrentTick() + " in tech "
                    + currentTenderBid.getTechnology() + " with plant name " + plant.getName() + " in zone "
                    + currentTenderBid.getZone());
        }
        // Remove the non accepted bid power plants
        Iterable<TenderBid> sortedTenderBidsbyPriceAndZone = null;
        sortedTenderBidsbyPriceAndZone = reps.tenderBidRepository.findAllSortedTenderBidsbyTime(getCurrentTick(), zone);
        for (TenderBid currentBid : sortedTenderBidsbyPriceAndZone) {
            if (currentBid.getStatus() == Bid.FAILED) {
                currentBid.getPowerPlant().dismantlePowerPlant(getCurrentTick());

            }

        }

    }

    private void createSpreadOutDownPayments(EnergyProducer agent, PowerPlantManufacturer manufacturer,
            double totalDownPayment, PowerPlant plant) {
        int buildingTime = (int) plant.getActualLeadTime();
        reps.nonTransactionalCreateRepository.createCashFlow(agent, manufacturer, totalDownPayment / buildingTime,
                CashFlow.DOWNPAYMENT, getCurrentTick(), plant);
        Loan downpayment = reps.loanRepository.createLoan(agent, manufacturer, totalDownPayment / buildingTime,
                buildingTime - 1, getCurrentTick(), plant);
        plant.createOrUpdateDownPayment(downpayment);
    }

    public double determineLoanAnnuities(double totalLoan, double payBackTime, double interestRate) {

        double q = 1 + interestRate;
        double annuity = totalLoan * (Math.pow(q, payBackTime) * (q - 1)) / (Math.pow(q, payBackTime) - 1);

        return annuity;
    }

}
