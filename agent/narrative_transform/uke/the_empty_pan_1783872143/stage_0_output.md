```xml
<constraint_manifest>
  <selected>
    <constraint id="C1" name="unownable_truth" generation_order="1">
      <base_properties>
        <epsilon>0.05</epsilon>
        <suppression>0.0</suppression>
        <coordination>false</coordination>
        <asymmetric>false</asymmetric>
      </base_properties>
      <graph>
        <downstream_of>none</downstream_of>
        <feeds_into>none</feeds_into>
      </graph>
      <character_classifications>
        <character name="Narrator (apprentice)">
          <index>
            <power>powerless</power>
            <time>biographical</time>
            <exit>identity_locked</exit>
            <scope>local</scope>
          </index>
          <chi>N/A</chi>
          <type>Mountain</type>
        </character>
        <character name="Verrel">
          <index>
            <power>analytical</power>
            <time>biographical</time>
            <exit>constrained</exit>
            <scope>local</scope>
          </index>
          <chi>N/A</chi>
          <type>Mountain</type>
        </character>
      </character_classifications>
      <indexical_variance>None. As a natural law, this constraint is a Mountain for all observers regardless of index.</indexical_variance>
      <selection_reason>This constraint is the most structurally distinct (Mountain) and provides the metaphysical backdrop for the entire system's failures, contrasting the ideal of truth with the reality of measurement.</selection_reason>
    </constraint>
    <constraint id="C2" name="extractive_standard" generation_order="2">
      <base_properties>
        <epsilon>0.8</epsilon>
        <suppression>0.8</suppression>
        <coordination>true</coordination>
        <asymmetric>true</asymmetric>
      </base_properties>
      <graph>
        <downstream_of>C4, C5 (deferred)</downstream_of>
        <feeds_into>C3</feeds_into>
      </graph>
      <character_classifications>
        <character name="Merchant">
          <index>
            <power>powerless</power>
            <time>immediate</time>
            <exit>trapped</exit>
            <scope>local</scope>
          </index>
          <chi>0.96</chi>
          <type>Snare</type>
        </character>
        <character name="Narrator (master)">
          <index>
            <power>moderate</power>
            <time>biographical</time>
            <exit>constrained</exit>
            <scope>local</scope>
          </index>
          <chi>0.64</chi>
          <type>Tangled Rope</type>
        </character>
        <character name="Crown">
          <index>
            <power>institutional</power>
            <time>generational</time>
            <exit>arbitrage</exit>
            <scope>national</scope>
          </index>
          <chi>-0.16</chi>
          <type>Rope</type>
        </character>
      </character_classifications>
      <indexical_variance>High. The same constraint is a Snare for its victims, a Tangled Rope for its conflicted operators, and a beneficial Rope for its institutional owner.</indexical_variance>
      <selection_reason>Highest centrality score (6). This is the primary engine of conflict and injustice in the narrative, demonstrating extreme indexical variance.</selection_reason>
    </constraint>
    <constraint id="C3" name="justified_compliance" generation_order="3">
      <base_properties>
        <epsilon>0.6</epsilon>
        <suppression>0.6</suppression>
        <coordination>true</coordination>
        <asymmetric>true</asymmetric>
      </base_properties>
      <graph>
        <downstream_of>C2</downstream_of>
        <feeds_into>none</feeds_into>
      </graph>
      <character_classifications>
        <character name="Narrator (master)">
          <index>
            <power>moderate</power>
            <time>biographical</time>
            <exit>constrained</exit>
            <scope>local</scope>
          </index>
          <chi>0.48</chi>
          <type>Tangled Rope</type>
        </character>
        <character name="Merchant">
          <index>
            <power>powerless</power>
            <time>immediate</time>
            <exit>trapped</exit>
            <scope>local</scope>
          </index>
          <chi>0.72</chi>
          <type>Snare</type>
        </character>
      </character_classifications>
      <indexical_variance>Significant. The narrative of civic duty ("keeping the port fed") is a Tangled Rope for the narrator who must balance the good it does with the harm it enables, but it functions as a Snare to the merchant for whom the justification masks pure extraction.</indexical_variance>
      <selection_reason>Second-highest centrality score (5). It provides the crucial psychological and social mechanism that explains why knowledgeable agents perpetuate a system they know is corrupt, making it a distinct and necessary axis of the story's logic.</selection_reason>
    </constraint>
  </selected>

  <deferred>
    <constraint id="C4" name="hidden_hand">
      <hypothesis>Rope</hypothesis>
      <offstage_function>Provides the philosophical foundation for the entire system, establishing that any standard is necessarily a human choice, which creates the vulnerability that the extractive standard (C2) exploits.</offstage_function>
    </constraint>
    <constraint id="C5" name="unquestionable_standard">
      <hypothesis>Rope</hypothesis>
      <offstage_function>Acts as the institutional enforcement mechanism, a social rule that grants the physical standard its authority and suppresses inquiry, thereby protecting the extractive standard (C2) from discovery.</offstage_function>
    </constraint>
    <constraint id="C6" name="declared_bias">
      <hypothesis>Rope</hypothesis>
      <offstage_function>Represents the protagonist's personal, ongoing ethical response to the corrupt system. It shapes her character and provides a thematic conclusion without being an active, generative constraint in the main conflict.</offstage_function>
    </constraint>
  </deferred>

  <generation_sequence>C1 → C2 → C3</generation_sequence>

  <invariant_contract>
    <untranslatable_real present="yes">There exists a state of objective balance that is invalidated by the desire of any observer to possess or instrumentalize it.</untranslatable_real>
    <missing_floor present="yes">Every system of measure is built upon a foundational act of setting a baseline, a choice that is then concealed to present the system as objective.</missing_floor>
    <inherent_instrument value="yes">The constraint is mediated by a certified instrument, and the harm it causes is a direct result of the reading that instrument produces.</inherent_instrument>
  </invariant_contract>

  <omegas>
    <omega id="origin_of_corruption">The analysis cannot resolve whether the extractive standard was corrupt from its inception 40 years ago or degraded over time through purity drift.</omega>
  </omegas>
</constraint_manifest>
```