```xml
<constraint_manifest>
  <selected>
    <constraint id="C1" name="Labor_Extraction" generation_order="1">
      <base_properties>
        <epsilon>0.90</epsilon>
        <suppression>0.80</suppression>
        <coordination>true</coordination>
        <asymmetric>true</asymmetric>
      </base_properties>
      <graph>
        <downstream_of>none</downstream_of>
        <feeds_into>C2, C3</feeds_into>
      </graph>
      <character_classifications>
        <character name="Ventkeeper">
          <index>
            <power>analytical</power>
            <time>biographical</time>
            <exit>identity_locked</exit>
            <scope>local</scope>
          </index>
          <chi>0.828</chi>
          <type>Snare</type>
        </character>
        <character name="Cores">
          <index>
            <power>powerless</power>
            <time>immediate</time>
            <exit>trapped</exit>
            <scope>local</scope>
          </index>
          <chi>1.08</chi>
          <type>Snare</type>
        </character>
      </character_classifications>
      <indexical_variance>false</indexical_variance>
      <selection_reason>Centrality score 4. This is the foundational extractive process that the entire system is built to serve and measure, making it the primary upstream driver of all other constraints.</selection_reason>
    </constraint>
    <constraint id="C2" name="Functional_Blinders" generation_order="2">
      <base_properties>
        <epsilon>0.50</epsilon>
        <suppression>0.10</suppression>
        <coordination>true</coordination>
        <asymmetric>true</asymmetric>
      </base_properties>
      <graph>
        <downstream_of>C1</downstream_of>
        <feeds_into>none</feeds_into>
      </graph>
      <character_classifications>
        <character name="Ventkeeper">
          <index>
            <power>analytical</power>
            <time>biographical</time>
            <exit>identity_locked</exit>
            <scope>local</scope>
          </index>
          <chi>0.46</chi>
          <type>Tangled Rope</type>
        </character>
      </character_classifications>
      <indexical_variance>false</indexical_variance>
      <selection_reason>Centrality score 4. This constraint reveals the system's method of maintaining focus by strictly defining relevance, creating structural ignorance as a feature, not a bug.</selection_reason>
    </constraint>
    <constraint id="C3" name="Coherence_Mandate" generation_order="3">
      <base_properties>
        <epsilon>0.70</epsilon>
        <suppression>0.20</suppression>
        <coordination>true</coordination>
        <asymmetric>true</asymmetric>
      </base_properties>
      <graph>
        <downstream_of>C1</downstream_of>
        <feeds_into>none</feeds_into>
      </graph>
      <character_classifications>
        <character name="Ventkeeper">
          <index>
            <power>analytical</power>
            <time>biographical</time>
            <exit>identity_locked</exit>
            <scope>local</scope>
          </index>
          <chi>0.644</chi>
          <type>Tangled Rope</type>
        </character>
      </character_classifications>
      <indexical_variance>false</indexical_variance>
      <selection_reason>Centrality score 4. This is the central epistemological conflict of the narrative, forcing the protagonist to corrupt data integrity to maintain systemic coherence.</selection_reason>
    </constraint>
  </selected>

  <deferred>
    <constraint id="C4" name="Atmospheric_Pressure">
      <hypothesis>Tangled Rope</hypothesis>
      <offstage_function>Provides the constant, non-negotiable background urgency (the quota pulse) that makes the system's demands feel like natural law rather than constructed choices.</offstage_function>
    </constraint>
    <constraint id="C5" name="Measurement_Protocol">
      <hypothesis>Rope</hypothesis>
      <offstage_function>Acts as the instrument of the Coherence Mandate, lending the process of narrative invention and data corruption the authority and objectivity of a technical procedure.</offstage_function>
    </constraint>
  </deferred>

  <generation_sequence>C1 → C2 → C3</generation_sequence>

  <invariant_contract>
    <untranslatable_real present="yes">A physical process generates a signal that, while measurable by the system's instruments, is irreducible to the system's explanatory categories.</untranslatable_real>
    <missing_floor present="yes">The obligation to produce is treated as a natural state of being, obscuring the founding choice that partitioned a class of people as fuel for a system.</missing_floor>
    <inherent_instrument value="yes">The constraint is an obligation to reconcile a certified measurement with an authorized model of the world, where the measurement itself creates the crisis.</inherent_instrument>
  </invariant_contract>

  <break_contract>
    <original_break>The expectation that an anomaly is a puzzle to be solved is violated when the protagonist's function is not to solve it, but to make it administratively disappear.</original_break>
    <prior_status>LIVE</prior_status>
    <target_prior>The expectation that a mysterious signal is a call to adventure is violated when the signal's true role is to test the integrity of a bureaucratic system, which fails.</target_prior>
  </break_contract>

  <omegas>
    <omega id="anomaly_source">The physical source of the anomalous readings at Vent Fourteen is deliberately left unresolved by the source text and is therefore outside the scope of this analysis.</omega>
  </omegas>
</constraint_manifest>
```