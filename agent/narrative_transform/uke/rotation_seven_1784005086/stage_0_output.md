```xml
<constraint_manifest>
  <selected>
    <constraint id="C1" name="Biological_Imperative" generation_order="1">
      <base_properties>
        <epsilon>0.10</epsilon>
        <suppression>0.00</suppression>
        <coordination>false</coordination>
        <asymmetric>false</asymmetric>
      </base_properties>
      <graph>
        <downstream_of>none</downstream_of>
        <feeds_into>C3</feeds_into>
      </graph>
      <character_classifications>
        <character name="Anna">
          <index>
            <power>powerless</power>
            <time>biographical</time>
            <exit>trapped</exit>
            <scope>local</scope>
          </index>
          <chi>N/A</chi>
          <type>Mountain</type>
        </character>
        <character name="Rina">
          <index>
            <power>powerless</power>
            <time>biographical</time>
            <exit>trapped</exit>
            <scope>local</scope>
          </index>
          <chi>N/A</chi>
          <type>Mountain</type>
        </character>
        <character name="Kwan">
          <index>
            <power>moderate</power>
            <time>biographical</time>
            <exit>constrained</exit>
            <scope>local</scope>
          </index>
          <chi>N/A</chi>
          <type>Mountain</type>
        </character>
      </character_classifications>
      <indexical_variance>None. As a natural law within the setting, its classification is stable across all indices.</indexical_variance>
      <selection_reason>This is the foundational, unchangeable reality that precipitates the story's central conflict. It is the upstream driver for C3.</selection_reason>
    </constraint>
    <constraint id="C2" name="Dyadic_Coordination" generation_order="2">
      <base_properties>
        <epsilon>0.05</epsilon>
        <suppression>0.00</suppression>
        <coordination>true</coordination>
        <asymmetric>false</asymmetric>
      </base_properties>
      <graph>
        <downstream_of>none</downstream_of>
        <feeds_into>none</feeds_into>
      </graph>
      <character_classifications>
        <character name="Rina">
          <index>
            <power>organized</power>
            <time>biographical</time>
            <exit>identity_locked</exit>
            <scope>local</scope>
          </index>
          <chi>0.02</chi>
          <type>Rope</type>
        </character>
        <character name="Anna">
          <index>
            <power>organized</power>
            <time>biographical</time>
            <exit>identity_locked</exit>
            <scope>local</scope>
          </index>
          <chi>0.02</chi>
          <type>Rope</type>
        </character>
      </character_classifications>
      <indexical_variance>None between its two participants. It is inaccessible to outsiders.</indexical_variance>
      <selection_reason>This constraint represents the story's central counter-logic to the dominant system, forming the primary axis of thematic tension.</selection_reason>
    </constraint>
    <constraint id="C3" name="Gamified_Compliance" generation_order="3">
      <base_properties>
        <epsilon>0.80</epsilon>
        <suppression>0.80</suppression>
        <coordination>true</coordination>
        <asymmetric>true</asymmetric>
      </base_properties>
      <graph>
        <downstream_of>C1</downstream_of>
        <feeds_into>none</feeds_into>
      </graph>
      <character_classifications>
        <character name="Anna">
          <index>
            <power>powerless</power>
            <time>biographical</time>
            <exit>trapped</exit>
            <scope>local</scope>
          </index>
          <chi>0.96</chi>
          <type>Snare</type>
        </character>
        <character name="Rina">
          <index>
            <power>powerless</power>
            <time>biographical</time>
            <exit>trapped</exit>
            <scope>local</scope>
          </index>
          <chi>0.96</chi>
          <type>Snare</type>
        </character>
        <character name="Rina (post-rejection)">
          <index>
            <power>analytical</power>
            <time>biographical</time>
            <exit>identity_locked</exit>
            <scope>local</scope>
          </index>
          <chi>0.74</chi>
          <type>Snare</type>
        </character>
        <character name="Kwan">
          <index>
            <power>moderate</power>
            <time>biographical</time>
            <exit>constrained</exit>
            <scope>local</scope>
          </index>
          <chi>0.64</chi>
          <type>Tangled Rope</type>
        </character>
      </character_classifications>
      <indexical_variance>Yes. The same system is experienced as a Snare by the powerless children but as a functional (if problematic) Tangled Rope by its mid-level administrator.</indexical_variance>
      <selection_reason>This is the highest-centrality constraint, representing the totalizing institutional logic that the characters must navigate. It is the primary antagonist force.</selection_reason>
    </constraint>
  </selected>

  <deferred>
    <constraint id="C4" name="Constant_Monitoring">
      <hypothesis>Tangled Rope</hypothesis>
      <offstage_function>Acts as the enabling infrastructure for C3, making compliance non-negotiable and dissent immediately visible through automated tracking.</offstage_function>
    </constraint>
    <constraint id="C5" name="Physical_Confinement">
      <hypothesis>Mountain</hypothesis>
      <offstage_function>Serves as the absolute background pressure that renders exit impossible, thereby locking all characters into the 'trapped' or 'constrained' categories and raising the stakes of all other constraints.</offstage_function>
    </constraint>
  </deferred>

  <generation_sequence>C1 → C2 → C3</generation_sequence>

  <invariant_contract>
    <untranslatable_real present="yes" primary="yes">The unconditional love between two people creates a reality of value that the system's metrics of performance and compliance cannot measure or contain.</untranslatable_real>
    <missing_floor present="yes" primary="no">The system's foundational choice is that individual lives are subordinate to institutional function and that persons are resources to be managed for productivity.</missing_floor>
    <inherent_instrument value="yes">The performance-ranking element of the system is entirely mediated by certified scores and public displays; removing the instruments of measurement and ranking would remove that aspect of the constraint.</inherent_instrument>
  </invariant_contract>

  <break_contract>
    <original_break>A story about individuals trapped in a harsh institution is expected to be about escape or overt rebellion; this story violates that by centering on an internal rebellion against the system's values.</original_break>
    <prior_status>LIVE</prior_status>
    <target_prior>The expectation that meaningful victory requires overthrowing the external power structure is violated by a story where victory is defined as preserving one's humanity, even while accepting total systemic defeat.</target_prior>
  </break_contract>

  <omegas>
    <omega id="system_intent">The ultimate purpose of the institution and the labor it extracts is unknown, making it impossible to determine if its harshness is born of malice, necessity, or indifference.</omega>
  </omegas>
</constraint_manifest>
```