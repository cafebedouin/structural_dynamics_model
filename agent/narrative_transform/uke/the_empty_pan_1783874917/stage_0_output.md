```xml
<constraint_manifest>
  <selected>
    <constraint id="C1" name="True_Measure_Is_Unpossessable" generation_order="1">
      <base_properties>
        <epsilon>0.05</epsilon>
        <suppression>0.0</suppression>
        <coordination>false</coordination>
        <asymmetric>false</asymmetric>
      </base_properties>
      <graph>
        <downstream_of>none</downstream_of>
        <feeds_into>C2, C3</feeds_into>
      </graph>
      <character_classifications>
        <character name="Narrator (apprentice)">
          <index>
            <power>powerless</power>
            <time>biographical</time>
            <exit>trapped</exit>
            <scope>local</scope>
          </index>
          <chi>N/A</chi>
          <type>Mountain</type>
        </character>
        <character name="Verrel">
          <index>
            <power>analytical</power>
            <time>biographical</time>
            <exit>identity_locked</exit>
            <scope>local</scope>
          </index>
          <chi>N/A</chi>
          <type>Mountain</type>
        </character>
      </character_classifications>
      <indexical_variance>None. As a natural law, this constraint is a Mountain from all character indices.</indexical_variance>
      <selection_reason>Structurally upstream of all other constraints (Centrality=4). It establishes the fundamental physical/epistemological reality that necessitates the creation of constructed systems of measurement.</selection_reason>
    </constraint>
    <constraint id="C2" name="The_Arbitrary_Baseline" generation_order="2">
      <base_properties>
        <epsilon>0.5</epsilon>
        <suppression>0.5</suppression>
        <coordination>true</coordination>
        <asymmetric>true</asymmetric>
      </base_properties>
      <graph>
        <downstream_of>C1</downstream_of>
        <feeds_into>C3</feeds_into>
      </graph>
      <character_classifications>
        <character name="Narrator (apprentice)">
          <index>
            <power>powerless</power>
            <time>biographical</time>
            <exit>trapped</exit>
            <scope>regional</scope>
          </index>
          <chi>0.675</chi>
          <type>Tangled Rope</type>
        </character>
        <character name="Verrel">
          <index>
            <power>analytical</power>
            <time>biographical</time>
            <exit>identity_locked</exit>
            <scope>regional</scope>
          </index>
          <chi>0.518</chi>
          <type>Tangled Rope</type>
        </character>
      </character_classifications>
      <indexical_variance>None. Both characters who perceive this constraint classify it as a Tangled Rope, as their power positions are insufficient to escape its moderate, power-scaled extraction.</indexical_variance>
      <selection_reason>Highest centrality score (5), tied with C3. Selected as the crucial bridge between the natural law (C1) and the specific social implementation (C3), representing the necessary but power-laden act of creating a system.</selection_reason>
    </constraint>
    <constraint id="C3" name="The_Sanctioned_Inaccuracy" generation_order="3">
      <base_properties>
        <epsilon>0.8</epsilon>
        <suppression>0.9</suppression>
        <coordination>true</coordination>
        <asymmetric>true</asymmetric>
      </base_properties>
      <graph>
        <downstream_of>C1, C2</downstream_of>
        <feeds_into>none</feeds_into>
      </graph>
      <character_classifications>
        <character name="Narrator (apprentice)">
          <index>
            <power>powerless</power>
            <time>biographical</time>
            <exit>trapped</exit>
            <scope>regional</scope>
          </index>
          <chi>1.08</chi>
          <type>Snare</type>
        </character>
        <character name="Verrel">
          <index>
            <power>analytical</power>
            <time>biographical</time>
            <exit>identity_locked</exit>
            <scope>regional</scope>
          </index>
          <chi>0.828</chi>
          <type>Snare</type>
        </character>
        <character name="The King">
          <index>
            <power>institutional</power>
            <time>generational</time>
            <exit>arbitrage</exit>
            <scope>regional</scope>
          </index>
          <chi>-0.144</chi>
          <type>Rope</type>
        </character>
      </character_classifications>
      <indexical_variance>High. The same constraint is a high-extraction Snare for those subject to it, but a beneficial coordination Rope for the institutional power that extracts value from it.</indexical_variance>
      <selection_reason>Highest centrality score (5), tied with C2. It is the most visible and socially active constraint in the narrative, representing the downstream consequence of the more foundational C1 and C2.</selection_reason>
    </constraint>
  </selected>
  <deferred>
    <constraint id="C4" name="The_Declared_Hand">
      <hypothesis>Rope</hypothesis>
      <offstage_function>Serves as the ethical resolution and primary character action in response to the selected constraints, demonstrating a strategy for maintaining integrity within an unchangeable, corrupt system.</offstage_function>
    </constraint>
  </deferred>
  <generation_sequence>C1 → C2 → C3</generation_sequence>
  <invariant_contract>
    <untranslatable_real present="yes">An absolute, objective state exists, but it is rendered inaccessible by the very act of observation or possession intended to capture it.</untranslatable_real>
    <missing_floor present="yes">All systems of value are built upon a foundational, arbitrary declaration of a zero-point, a choice that is then hidden to present the system as naturally grounded.</missing_floor>
    <inherent_instrument value="yes">The extraction is actualized through a certified process of measurement, where an official reading from an instrument is what gives the asymmetry force.</inherent_instrument>
  </invariant_contract>
  <omegas>
    <omega id="analytical_powerlessness">The χ formula reduces experienced extraction for an analytical position (π=1.15) compared to a powerless one (π=1.5). However, the source narrative suggests that for Verrel, analytical insight provides only clarity on the trap, not a reduction in its effect. This raises an unresolved question about whether analysis without agency truly mitigates experienced extraction.</omega>
  </omegas>
</constraint_manifest>
```