```xml
<constraint_manifest>
  <selected>
    <constraint id="C1" name="environmental_hazard" generation_order="1">
      <base_properties>
        <epsilon>0.05</epsilon>
        <suppression>0.0</suppression>
        <coordination>false</coordination>
        <asymmetric>false</asymmetric>
      </base_properties>
      <graph>
        <downstream_of>none</downstream_of>
        <feeds_into>C2</feeds_into>
      </graph>
      <character_classifications>
        <character name="sailors">
          <index>
            <power>powerless</power>
            <time>immediate</time>
            <exit>trapped</exit>
            <scope>universal</scope>
          </index>
          <chi>0.075</chi>
          <type>Mountain</type>
        </character>
        <character name="narrator">
          <index>
            <power>analytical</power>
            <time>biographical</time>
            <exit>analytical</exit>
            <scope>universal</scope>
          </index>
          <chi>0.0575</chi>
          <type>Mountain</type>
        </character>
        <character name="office_financiers">
          <index>
            <power>institutional</power>
            <time>biographical</time>
            <exit>arbitrage</exit>
            <scope>universal</scope>
          </index>
          <chi>-0.01</chi>
          <type>Mountain</type>
        </character>
      </character_classifications>
      <indexical_variance>None. As a natural law, this constraint is classified as a Mountain from all perspectives, demonstrating its failure of the Boltzmann test is not due to power-scaling but its fundamental nature.</indexical_variance>
      <selection_reason>This constraint is the foundational, unchangeable reality upon which all constructed systems in the narrative are built. It provides the most distinct structural contrast to the artificial constraints.</selection_reason>
    </constraint>
    <constraint id="C2" name="actuarial_pricing" generation_order="2">
      <base_properties>
        <epsilon>0.8</epsilon>
        <suppression>0.2</suppression>
        <coordination>true</coordination>
        <asymmetric>true</asymmetric>
      </base_properties>
      <graph>
        <downstream_of>C1</downstream_of>
        <feeds_into>C3</feeds_into>
      </graph>
      <character_classifications>
        <character name="sailors">
          <index>
            <power>powerless</power>
            <time>biographical</time>
            <exit>trapped</exit>
            <scope>regional</scope>
          </index>
          <chi>1.08</chi>
          <type>Snare</type>
        </character>
        <character name="narrator">
          <index>
            <power>analytical</power>
            <time>biographical</time>
            <exit>analytical</exit>
            <scope>regional</scope>
          </index>
          <chi>0.828</chi>
          <type>Snare</type>
        </character>
        <character name="office_financiers">
          <index>
            <power>institutional</power>
            <time>biographical</time>
            <exit>arbitrage</exit>
            <scope>regional</scope>
          </index>
          <chi>-0.144</chi>
          <type>Rope</type>
        </character>
      </character_classifications>
      <indexical_variance>High. The same system is a Snare for those who must pay into it and a Rope for those who benefit from it, demonstrating extreme power-scaling and index-sensitivity.</indexical_variance>
      <selection_reason>Highest centrality score (7). This is the core mechanism of the story, mediating between the natural world and the social world, and its properties change dramatically depending on the observer's position.</selection_reason>
    </constraint>
    <constraint id="C3" name="erasure_of_skill" generation_order="3">
      <base_properties>
        <epsilon>0.6</epsilon>
        <suppression>0.0</suppression>
        <coordination>false</coordination>
        <asymmetric>true</asymmetric>
      </base_properties>
      <graph>
        <downstream_of>C2</downstream_of>
        <feeds_into>none</feeds_into>
      </graph>
      <character_classifications>
        <character name="sailors">
          <index>
            <power>powerless</power>
            <time>biographical</time>
            <exit>identity_locked</exit>
            <scope>regional</scope>
          </index>
          <chi>0.81</chi>
          <type>Snare</type>
        </character>
        <character name="narrator">
          <index>
            <power>analytical</power>
            <time>biographical</time>
            <exit>analytical</exit>
            <scope>regional</scope>
          </index>
          <chi>0.621</chi>
          <type>Tangled Rope</type>
        </character>
        <character name="office_financiers">
          <index>
            <power>institutional</power>
            <time>biographical</time>
            <exit>arbitrage</exit>
            <scope>regional</scope>
          </index>
          <chi>-0.108</chi>
          <type>Rope</type>
        </character>
      </character_classifications>
      <indexical_variance>High. The system's blindness to a certain kind of knowledge is a destructive Snare for those who possess it, a functional Rope for those who don't need it, and a complex Tangled Rope for the analyst who sees both its function and its cost.</indexical_variance>
      <selection_reason>Selected as the primary downstream consequence of C2. It represents the epistemological violence of the system, providing a different axis of conflict (knowledge vs. data) from the economic and natural axes of C1 and C2.</selection_reason>
    </constraint>
  </selected>

  <deferred>
    <constraint id="C4" name="market_lockout">
      <hypothesis>Snare</hypothesis>
      <offstage_function>Provides the economic coercion that makes the central pricing system (C2) non-optional, ensuring universal participation and eliminating alternatives.</offstage_function>
    </constraint>
    <constraint id="C5" name="behavioral_substitution">
      <hypothesis>Snare</hypothesis>
      <offstage_function>Acts as the direct, observable consequence of C3, shaping character actions and demonstrating the lethal cost of substituting formal rules for embodied skill.</offstage_function>
    </constraint>
    <constraint id="C6" name="community_commodification">
      <hypothesis>Tangled Rope</hypothesis>
      <offstage_function>Provides world texture by showing how the logic of the central system (C2) degrades adjacent social structures, turning mutual aid into a transactional service.</offstage_function>
    </constraint>
  </deferred>

  <generation_sequence>C1 → C2 → C3</generation_sequence>

  <invariant_contract>
    <untranslatable_real present="yes">The embodied, non-transferable knowledge a person develops through long practice with a specific physical system. It cannot be written down or measured without ceasing to be the thing that it is, because its nature is performative and relational, not informational.</untranslatable_real>
    <missing_floor present="yes">The decision to value only statistically aggregated, written-down information as the sole basis for assessing risk. This choice creates the entire system by establishing a zero-point where any other form of knowledge is valued at nothing.</missing_floor>
    <inherent_instrument value="yes">The constraint is entirely mediated by a formal, written instrument of calculation. Without the instrument, the specific form of extraction and behavioral modification does not exist.</inherent_instrument>
  </invariant_contract>

  <break_contract>
    <original_break>A story about insurance and statistics will be dry and impersonal.</original_break>
    <prior_status>DEAD</prior_status>
    <target_prior>The expectation that a system based on accurate, objective data is therefore a just or beneficial system.</target_prior>
  </break_contract>

  <omegas>
    <omega id="motive">The analysis assumes the fish-buyers' price fixing is for risk management as stated; it could also be opportunistic exploitation, which would increase the epsilon of the deferred market_lockout constraint.</omega>
  </omegas>
</constraint_manifest>
```