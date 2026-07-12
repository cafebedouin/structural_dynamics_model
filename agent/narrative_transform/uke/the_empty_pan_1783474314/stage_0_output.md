```xml
<constraint_manifest>
  <selected>
    <constraint id="C1" name="Observer-Dependent Truth" generation_order="1">
      <base_properties>
        <epsilon>0.10</epsilon>
        <suppression>0.00</suppression>
        <coordination>false</coordination>
        <asymmetric>false</asymmetric>
      </base_properties>
      <graph>
        <downstream_of>none</downstream_of>
        <feeds_into>none</feeds_into>
      </graph>
      <character_classifications>
        <character name="narrator_apprentice">
          <index>
            <power>powerless</power>
            <time>biographical</time>
            <exit>identity_locked</exit>
            <scope>regional</scope>
          </index>
          <chi>0.14</chi>
          <type>Mountain</type>
        </character>
        <character name="verrel">
          <index>
            <power>analytical</power>
            <time>biographical</time>
            <exit>identity_locked</exit>
            <scope>regional</scope>
          </index>
          <chi>0.10</chi>
          <type>Mountain</type>
        </character>
      </character_classifications>
      <indexical_variance>None. As a law of nature within the narrative, this constraint is immutable from all tested character indices and correctly classifies as a Mountain for all agents.</indexical_variance>
      <selection_reason>Structurally distinct (Mountain) and philosophically central. It establishes the impossibility of the 'pure' alternative the protagonist seeks, forcing a confrontation with the compromised nature of all constructed systems.</selection_reason>
    </constraint>
    <constraint id="C2" name="Institutionalized Falsehood" generation_order="2">
      <base_properties>
        <epsilon>0.80</epsilon>
        <suppression>0.90</suppression>
        <coordination>true</coordination>
        <asymmetric>true</asymmetric>
      </base_properties>
      <graph>
        <downstream_of>C4 (deferred)</downstream_of>
        <feeds_into>C3</feeds_into>
      </graph>
      <character_classifications>
        <character name="narrator_apprentice">
          <index>
            <power>powerless</power>
            <time>biographical</time>
            <exit>identity_locked</exit>
            <scope>regional</scope>
          </index>
          <chi>1.08</chi>
          <type>Snare</type>
        </character>
        <character name="merchants">
          <index>
            <power>powerless</power>
            <time>biographical</time>
            <exit>trapped</exit>
            <scope>regional</scope>
          </index>
          <chi>1.08</chi>
          <type>Snare</type>
        </character>
        <character name="crown">
          <index>
            <power>institutional</power>
            <time>generational</time>
            <exit>arbitrage</exit>
            <scope>regional</scope>
          </index>
          <chi>-0.14</chi>
          <type>Rope</type>
        </character>
      </character_classifications>
      <indexical_variance>High. The same system is experienced as an extractive Snare by those subject to it (merchants, apprentices) and as a beneficial coordination Rope by the institutional power that profits from it (crown).</indexical_variance>
      <selection_reason>Highest centrality score (tied). This is the primary engine of conflict in the narrative, creating the asymmetric extraction that characters must navigate. Its high indexical variance is key to the story's structure.</selection_reason>
    </constraint>
    <constraint id="C3" name="Pragmatic Complicity" generation_order="3">
      <base_properties>
        <epsilon>0.52</epsilon>
        <suppression>0.60</suppression>
        <coordination>true</coordination>
        <asymmetric>true</asymmetric>
      </base_properties>
      <graph>
        <downstream_of>C2</downstream_of>
        <feeds_into>none</feeds_into>
      </graph>
      <character_classifications>
        <character name="narrator_master">
          <index>
            <power>moderate</power>
            <time>biographical</time>
            <exit>identity_locked</exit>
            <scope>regional</scope>
          </index>
          <chi>0.47</chi>
          <type>Tangled Rope</type>
        </character>
        <character name="verrel">
          <index>
            <power>moderate</power>
            <time>biographical</time>
            <exit>identity_locked</exit>
            <scope>regional</scope>
          </index>
          <chi>0.47</chi>
          <type>Tangled Rope</type>
        </character>
        <character name="crown">
          <index>
            <power>institutional</power>
            <time>generational</time>
            <exit>arbitrage</exit>
            <scope>regional</scope>
          </index>
          <chi>-0.09</chi>
          <type>Rope</type>
        </character>
      </character_classifications>
      <indexical_variance>Present. The pressure to uphold a flawed system for the sake of social function is a Tangled Rope for the agents who must administer it, mixing real coordination with moral cost. For the beneficiary, it is a simple Rope, a tool for ensuring stability and compliance.</indexical_variance>
      <selection_reason>Highest centrality score (tied) and the most downstream constraint. It represents the final, stable, compromised state of the protagonist, making it the terminal expression of the system's logic.</selection_reason>
    </constraint>
  </selected>

  <deferred>
    <constraint id="C4" name="The Ungrounded Choice">
      <hypothesis>Rope</hypothesis>
      <offstage_function>This constraint establishes that all systems of measure are founded on an initial, hidden choice. It works from offstage to universalize the story's central problem, preventing the institutional falsehood from seeming like a uniquely evil or correctable local error.</offstage_function>
    </constraint>
    <constraint id="C5" name="Atomized Resistance">
      <hypothesis>Tangled Rope</hypothesis>
      <offstage_function>This constraint forecloses the possibility of collective action to reform the system. It works from offstage to explain why the only available responses are individual and symbolic (e.g., writing in margins), forcing the characters into a path of personal integrity rather than political change.</offstage_function>
    </constraint>
  </deferred>

  <generation_sequence>C1 → C2 → C3</generation_sequence>

  <omegas>
    <omega id="origin_of_corruption">The analysis cannot resolve whether the institutional falsehood (C2) was designed with extractive intent from its inception or if it is the result of purity drift over time (e.g., a once-honest standard that was later corrupted).</omega>
  </omegas>
</constraint_manifest>
```