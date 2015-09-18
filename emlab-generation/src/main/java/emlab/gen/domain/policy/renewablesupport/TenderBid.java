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
package emlab.gen.domain.policy.renewablesupport;

import org.neo4j.graphdb.Direction;
import org.springframework.data.neo4j.annotation.NodeEntity;
import org.springframework.data.neo4j.annotation.RelatedTo;
import org.springframework.transaction.annotation.Transactional;

import emlab.gen.domain.agent.EnergyProducer;
import emlab.gen.domain.gis.Zone;
import emlab.gen.domain.market.Bid;
import emlab.gen.domain.technology.PowerGeneratingTechnology;
import emlab.gen.domain.technology.PowerGridNode;
import emlab.gen.domain.technology.PowerPlant;

/**
 * @author Kaveri for tender
 *
 */
@NodeEntity
public class TenderBid extends Bid {

    @RelatedTo(type = "FOR_NODE", elementClass = PowerGridNode.class, direction = Direction.OUTGOING)
    private PowerGridNode powerGridNode;

    @RelatedTo(type = "FOR_TECHNOLOGY", elementClass = PowerGeneratingTechnology.class, direction = Direction.OUTGOING)
    private PowerGeneratingTechnology technology;

    // why connected to powerplant dispatchplan?
    @RelatedTo(type = "POWERPLANT_DISPATCHPLAN", elementClass = PowerPlant.class, direction = Direction.OUTGOING)
    private PowerPlant powerPlant;

    @RelatedTo(type = "TENDERBID_SUPPORTSCHEME", elementClass = RenewableSupportSchemeTender.class, direction = Direction.OUTGOING)
    private RenewableSupportSchemeTender renewableSupportSchemeTender;

    @RelatedTo(type = "ZONE", elementClass = Zone.class, direction = Direction.INCOMING)
    private Zone zone;

    private long start;

    private long finish;

    public long getStart() {
        return start;
    }

    public void setStart(long start) {
        this.start = start;
    }

    public RenewableSupportSchemeTender getRenewableSupportSchemeTender() {
        return renewableSupportSchemeTender;
    }

    public void setRenewableSupportSchemeTender(RenewableSupportSchemeTender renewableSupportSchemeTender) {
        this.renewableSupportSchemeTender = renewableSupportSchemeTender;
    }

    public long getFinish() {
        return finish;
    }

    public void setFinish(long finish) {
        this.finish = finish;
    }

    public Zone getZone() {
        return zone;
    }

    public void setZone(Zone zone) {
        this.zone = zone;
    }

    public PowerGridNode getPowerGridNode() {
        return powerGridNode;
    }

    public void setPowerGridNode(PowerGridNode powerGridNode) {
        this.powerGridNode = powerGridNode;
    }

    public PowerPlant getPowerPlant() {
        return powerPlant;
    }

    public void setPowerPlant(PowerPlant powerPlant) {
        this.powerPlant = powerPlant;
    }

    public PowerGeneratingTechnology getTechnology() {
        return technology;
    }

    public void setTechnology(PowerGeneratingTechnology technology) {
        this.technology = technology;
    }

    public void specifyNotPersist(double amount, PowerPlant plant, EnergyProducer agent, Zone zone, PowerGridNode node,
            long startTime, long finishTime, double bidPricePerMWh, PowerGeneratingTechnology technology,
            long currentTime, int status, RenewableSupportSchemeTender scheme) {
        this.setAmount(amount);
        this.setBidder(agent);
        this.setPrice(bidPricePerMWh);
        this.setZone(zone);
        this.setPowerGridNode(node);
        this.setTechnology(technology);
        this.setStart(startTime);
        this.setFinish(finishTime);
        this.setTime(currentTime);
        this.setRenewableSupportSchemeTender(scheme);
        this.setPowerPlant(plant);

    }

    /**
     * @param plant
     */

    // All transactional methods below are signified by starting with update
    @Transactional
    public void specifyAndPersist(double amount, PowerPlant plant, EnergyProducer agent, Zone zone, PowerGridNode node,
            long startTime, long finishTime, double bidPricePerMWh, PowerGeneratingTechnology technology,
            long currentTime, int status, RenewableSupportSchemeTender scheme) {
        this.persist();
        this.specifyNotPersist(amount, plant, agent, zone, node, startTime, finishTime, bidPricePerMWh, technology,
                currentTime, status, scheme);

    }

}
