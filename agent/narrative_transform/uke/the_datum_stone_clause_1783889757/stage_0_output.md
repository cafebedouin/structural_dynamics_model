```xml
<constraint_manifest>
  <selected>
    <constraint id="C1" name="The Naturalized Datum" generation_order="1">
      <base_properties>
        <epsilon>0.75</epsilon>
        <suppression>0.20</suppression>
        <coordination>false</coordination>
        <asymmetric>true</asymmetric>
      </base_properties>
      <graph>
        <downstream_of>none</downstream_of>
        <feeds_into>C2</feeds_into>
      </graph>
      <character_classifications>
        <character name="valley_dwellers">
          <index>
            <power>moderate</power>
            <time>generational</time>
            <exit>identity_locked</exit>
            <scope>local</scope>
          </index>
          <chi>0.60</chi>
          <type>Tangled Rope</type>
        </character>
        <character name="narrator (post-realization)">
          <index>
            <power>analytical</power>
            <time>biographical</time>
            <exit>analytical</exit>
            <scope>local</scope>
          </index>
          <chi>0.69</chi>
          <type>Tangled Rope</type>
        </character>
      </character_classifications>
      <indexical_variance>false</indexical_variance>
      <selection_reason>This is the foundational cognitive constraint that makes the primary extractive system possible. Its high centrality (5) comes from enabling the entire downstream structure of grievance and failed reform by framing a constructed power dynamic as a natural fact.</selection_reason>
    </constraint>
    <constraint id="C2" name="Height-Based Rights" generation_order="2">
      <base_properties>
        <epsilon>0.80</epsilon>
        <suppression>0.70</suppression>
        <coordination>true</coordination>
        <asymmetric>true</asymmetric>
      </base_properties>
      <graph>
        <downstream_of>C1</downstream_of>
        <feeds_into>C3</feeds_into>
      </graph>
      <character_classifications>
        <character name="high_heights">
          <index>
            <power>powerless</power>
            <time>biographical</time>
            <exit>trapped</exit>
            <scope>local</scope>
          </index>
          <chi>0.96</chi>
          <type>Snare</type>
        </character>
        <character name="low_heights">
          <index>
            <power>institutional</power>
            <time>biographical</time>
            <exit>identity_locked</exit>
            <scope>local</scope>
          </index>
          <chi>-0.128</chi>
          <type>Rope</type>
        </character>
        <character name="narrator (post-realization)">
          <index>
            <power>analytical</power>
            <time>biographical</time>
            <exit>analytical</exit>
            <scope>local</scope>
          </index>
          <chi>0.736</chi>
          <type>Snare</type>
        </character>
      </character_classifications>
      <indexical_variance>true</indexical_variance>
      <selection_reason>This is the primary, visible system of extraction. Its selection is critical because its indexical variance (a Snare for the powerless, a Rope for the powerful) is the engine of the story's central conflict and demonstrates the core logic of indexed relativity.</selection_reason>
    </constraint>
    <constraint id="C3" name="Destructive Codification" generation_order="3">
      <base_properties>
        <epsilon>0.90</epsilon>
        <suppression>0.90</suppression>
        <coordination>true</coordination>
        <asymmetric>true</asymmetric>
      </base_properties>
      <graph>
        <downstream_of>C2</downstream_of>
        <feeds_into>none</feeds_into>
      </graph>
      <character_classifications>
        <character name="water_walkers">
          <index>
            <power>powerless</power>
            <time>biographical</time>
            <exit>trapped</exit>
            <scope>local</scope>
          </index>
          <chi>1.08</chi>
          <type>Snare</type>
        </character>
        <character name="commissioners">
          <index>
            <power>institutional</power>
            <time>immediate</time>
            <exit>analytical</exit>
            <scope>local</scope>
          </index>
          <chi>-0.144</chi>
          <type>Rope</type>
        </character>
      </character_classifications>
      <indexical_variance>true</indexical_variance>
      <selection_reason>This constraint represents the story's tragic conclusion: the well-intentioned destruction of a functional, informal system by formalizing it. It demonstrates a key failure mode of reform and highlights a Snare-as-Rope error, making it structurally essential.</selection_reason>
    </constraint>
  </selected>

  <deferred>
    <constraint id="C4" name="The Unwritten Turns">
      <hypothesis>Rope</hypothesis>
      <offstage_function>Serves as the normative baseline and contrast case, representing the non-extractive, cooperative alternative that was lost, thereby giving the story its tragic weight.</offstage_function>
    </constraint>
    <constraint id="C5" name="Petitioning the Crown">
      <hypothesis>Tangled Rope</hypothesis>
      <offstage_function>Acts as the narrative driver, channeling all dissent into a single, state-sanctioned process that ultimately reinforces the system's underlying logic rather than challenging it.</offstage_function>
    </constraint>
    <constraint id="C6" name="Voice Requires Rights">
      <hypothesis>Snare</hypothesis>
      <offstage_function>Provides the specific procedural mechanism for the tragedy, explaining why the informal system was invisible and voiceless during the reform, ensuring its destruction.</offstage_function>
    </constraint>
  </deferred>

  <generation_sequence>C1 → C2 → C3</generation_sequence>

  <invariant_contract>
    <untranslatable_real present="yes" primary="no">The shared, unwritten knowledge of turn-taking that binds everyone equally because it belongs to no one. Any attempt to measure, write down, or enforce it as a right destroys its nature, turning a mutual obligation into a divisible property.</untranslatable_real>
    <missing_floor present="yes" primary="yes">The central injustice presupposes an arbitrary line as a neutral zero-point. The system treats this founding choice as a given, with no neutral ground beneath it, obscuring the fact that the "bottom" was a choice made for someone's convenience.</missing_floor>
    <inherent_instrument value="yes">The extraction runs through a certified measurement against an arbitrary standard; removing the standard and its instrument removes the constraint.</inherent_instrument>
  </invariant_contract>

  <break_contract>
    <original_break>The expectation that a story about correcting an injustice will end with the injustice being corrected.</original_break>
    <prior_status>LIVE</prior_status>
    <target_prior>The belief that a system's fairness can be fixed by adjusting its parameters, without questioning the act of measurement or the origin of the zero-point itself.</target_prior>
  </break_contract>

  <omegas>
    <omega id="motivation_of_surveyor">The source does not resolve whether the original surveyor's choice of datum was malicious collusion with the upper mill or simply a matter of convenience, leaving the system's origin as either corrupt or merely arbitrary.</omega>
  </omegas>
</constraint_manifest>
```