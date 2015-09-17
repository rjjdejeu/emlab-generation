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
import org.springframework.data.neo4j.support.Neo4jTemplate;
import org.springframework.transaction.annotation.Transactional;

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
import emlab.gen.domain.policy.renewablesupport.TenderBid;
import emlab.gen.domain.policy.renewablesupport.TenderClearingPoint;
import emlab.gen.domain.technology.PowerPlant;
import emlab.gen.repository.Reps;

/**
 * @author rjjdejeu
 */
@RoleComponent
public class ClearRenewableTenderRole extends AbstractRole<Regulator> implements Role<Regulator> {

    @Autowired
    Reps reps;

    @Autowired
    Neo4jTemplate template;

    @Override
    @Transactional
    public void act(Regulator regulator) {

        logger.warn("Clear Renewable Tender Role started for regulator: " + regulator);

        Zone zone = regulator.getZone();

        // Initialize a sorted list for tender bids
        Iterable<TenderBid> sortedTenderBidsbyPriceAndZone = null;
        sortedTenderBidsbyPriceAndZone = reps.tenderBidRepository.findAllSortedTenderBidsbyTime(getCurrentTick(), zone);

        double tenderQuota = regulator.getAnnualRenewableTargetInMwh();
        double sumOfTenderBidQuantityAccepted = 0d;
        double acceptedSubsidyPrice = 0d;
        boolean isTheTenderCleared = false;

        if (tenderQuota == 0) {
            isTheTenderCleared = true;
            acceptedSubsidyPrice = 0;
        }

        logger.warn("tenderQuota is " + tenderQuota);
        // logger.warn("the tender is cleared " + isTheTenderCleared);

        // This epsilon is to account for rounding errors for java (only
        // relevant for exact clearing)
        double clearingEpsilon = 0.0001d;

        // Goes through the list of the bids that are sorted on ascending order
        // by price
        for (TenderBid currentTenderBid : sortedTenderBidsbyPriceAndZone) {

            // if the tender is not cleared yet, it collects complete bids
            logger.warn("isTendercleared? " + isTheTenderCleared);

            if (isTheTenderCleared == false) {
                if (tenderQuota - (sumOfTenderBidQuantityAccepted + currentTenderBid.getAmount()) >= -clearingEpsilon) {

                    logger.warn("tenderQuota is " + tenderQuota);
                    logger.warn("Bid status should be fully accepted");
                    logger.warn("current sum Of Tender Bid Quantiy Accepted is: " + sumOfTenderBidQuantityAccepted);
                    logger.warn("Tender Quota >= 0 ? "
                            + (tenderQuota - (sumOfTenderBidQuantityAccepted + currentTenderBid.getAmount())));
                    logger.warn("amount of currentTenderBid is: " + currentTenderBid.getAmount());

                    acceptedSubsidyPrice = currentTenderBid.getPrice();
                    currentTenderBid.setStatus(Bid.ACCEPTED);
                    currentTenderBid.setAcceptedAmount(currentTenderBid.getAmount());
                    sumOfTenderBidQuantityAccepted = sumOfTenderBidQuantityAccepted + currentTenderBid.getAmount();

                    logger.warn("new sum of Tender Bid Quantity accepted it" + sumOfTenderBidQuantityAccepted);
                    logger.warn("bid status is: " + currentTenderBid.getStatus());

                }

                // it collects a bid partially if that bid fulfills the quota
                // partially
                else if (tenderQuota - (sumOfTenderBidQuantityAccepted + currentTenderBid.getAmount()) < clearingEpsilon) {

                    logger.warn("tenderQuota is: " + tenderQuota);
                    logger.warn("Bid status should be partly accepted");
                    logger.warn("current sum Of Tender Bid Quantiy Accepted is: " + sumOfTenderBidQuantityAccepted);
                    logger.warn("amount of currentTenderBid is: " + currentTenderBid.getAmount());
                    logger.warn("Tender Quota < 0 ? "
                            + (tenderQuota - (sumOfTenderBidQuantityAccepted + currentTenderBid.getAmount())));

                    acceptedSubsidyPrice = currentTenderBid.getPrice();
                    currentTenderBid.setStatus(Bid.PARTLY_ACCEPTED);
                    currentTenderBid.setAcceptedAmount((tenderQuota - sumOfTenderBidQuantityAccepted));
                    sumOfTenderBidQuantityAccepted = sumOfTenderBidQuantityAccepted
                            + currentTenderBid.getAcceptedAmount();
                    isTheTenderCleared = true;

                    logger.warn("new sum of Tender Bid Quantity accepted it: " + sumOfTenderBidQuantityAccepted);
                    logger.warn("and should be equal to tender quota: " + tenderQuota);
                    logger.warn("bid status is: " + currentTenderBid.getStatus());

                }
                // the tenderQuota is reached and the bids after that are not
                // accepted
            } else {

                logger.warn("Bid status should be failed");

                currentTenderBid.setStatus(Bid.FAILED);
                currentTenderBid.setAcceptedAmount(0);

                logger.warn("bid status is: " + currentTenderBid.getStatus());
            }

            currentTenderBid.persist();

            // A power plant can be created if the bid is (partly) accepted
            if (currentTenderBid.getStatus() == Bid.ACCEPTED || currentTenderBid.getStatus() == Bid.PARTLY_ACCEPTED) {

                PowerPlant plant = new PowerPlant();
                EnergyProducer bidder = (EnergyProducer) currentTenderBid.getBidder();

                plant.specifyAndPersist(currentTenderBid.getStart(), bidder, currentTenderBid.getPowerGridNode(),
                        currentTenderBid.getTechnology());

                logger.warn("Start is: " + currentTenderBid.getStart() + " with bidder " + bidder
                        + " in power gride node " + currentTenderBid.getPowerGridNode() + " for technology "
                        + currentTenderBid.getTechnology());

                logger.warn("Plant is: " + plant + " (with bidder) " + bidder);

                PowerPlantManufacturer manufacturer = reps.genericRepository.findFirst(PowerPlantManufacturer.class);
                BigBank bigbank = reps.genericRepository.findFirst(BigBank.class);

                double investmentCostPayedByEquity = plant.getActualInvestedCapital()
                        * (1 - bidder.getDebtRatioOfInvestments());
                double investmentCostPayedByDebt = plant.getActualInvestedCapital()
                        * bidder.getDebtRatioOfInvestments();
                double downPayment = investmentCostPayedByEquity;
                createSpreadOutDownPayments(bidder, manufacturer, downPayment, plant);

                double amount = determineLoanAnnuities(investmentCostPayedByDebt, plant.getTechnology()
                        .getDepreciationTime(), bidder.getLoanInterestRate());
                logger.warn("Loan amount is: " + amount);
                Loan loan = reps.loanRepository.createLoan(currentTenderBid.getBidder(), bigbank, amount, plant
                        .getTechnology().getDepreciationTime(), getCurrentTick(), plant);
                // Create the loan
                plant.createOrUpdateLoan(loan);

            }
        } // FOR Loop ends here

        // This creates a clearing point that contains general information about
        // the cleared tender
        // volume, subsidy price, current tick, and stores it in the graph
        // database

        if (isTheTenderCleared == true) {
            TenderClearingPoint tenderClearingPoint = new TenderClearingPoint();
            logger.warn("Tender CLEARED at price: " + acceptedSubsidyPrice);
            tenderClearingPoint.setPrice(acceptedSubsidyPrice);
            tenderClearingPoint.setVolume(sumOfTenderBidQuantityAccepted);
            tenderClearingPoint.setTime(getCurrentTick());
            tenderClearingPoint.persist();
            logger.warn("Clearing point Price is {} and volume is " + tenderClearingPoint.getVolume(),
                    tenderClearingPoint.getPrice());

        } else {
            TenderClearingPoint tenderClearingPoint = new TenderClearingPoint();
            logger.warn("MARKET UNCLEARED at price: " + tenderClearingPoint.getPrice());
            tenderClearingPoint.setPrice(acceptedSubsidyPrice);
            tenderClearingPoint.setVolume(sumOfTenderBidQuantityAccepted);
            tenderClearingPoint.setTime(getCurrentTick());
            tenderClearingPoint.persist();
            logger.warn("Clearing point Price is {} and volume is " + tenderClearingPoint.getVolume(),
                    tenderClearingPoint.getPrice());

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
