```xml
<constraint_manifest>
  <selected>
    <constraint id="C1" name="Temporal Standard Decay" generation_order="1">
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
        <character name="Warden">
          <index>
            <power>moderate</power>
            <time>biographical</time>
            <exit>constrained</exit>
            <scope>regional</scope>
          </index>
          <chi>0.09</chi>
          <type>Mountain</type>
        </character>
        <character name="Core">
          <index>
            <power>powerless</power>
            <time>immediate</time>
            <exit>trapped</exit>
            <scope>local</scope>
          </index>
          <chi>0.12</chi>
          <type>Mountain</type>
        </character>
      </character_classifications>
      <indexical_variance>none</indexical_variance>
      <selection_reason>This upstream constraint establishes the story's core dynamic of systemic drift. As a Mountain, it provides an unchangeable background pressure that causes downstream protocols to fail, revealing their brittleness.</selection_reason>
    </constraint>
    <constraint id="C2" name="Binary Purity Threshold" generation_order="2">
      <base_properties>
        <epsilon>1.00</epsilon>
        <suppression>0.80</suppression>
        <coordination>false</coordination>
        <asymmetric>true</asymmetric>
      </base_properties>
      <graph>
        <downstream_of>none</downstream_of>
        <feeds_into>C3</feeds_into>
      </graph>
      <character_classifications>
        <character name="Warden">
          <index>
            <power>moderate</power>
            <time>biographical</time>
            <exit>constrained</exit>
            <scope>regional</scope>
          </index>
          <chi>0.90</chi>
          <type>Snare</type>
        </character>
        <character name="Core">
          <index>
            <power>powerless</power>
            <time>immediate</time>
            <exit>trapped</exit>
            <scope>local</scope>
          </index>
          <chi>1.20</chi>
          <type>Snare</type>
        </character>
      </character_classifications>
      <indexical_variance>none</indexical_variance>
      <selection_reason>This constraint provides the central, violent conflict. As a pure Snare with maximum extractiveness, it demonstrates the system's brutal logic and creates the high-stakes decision point that tests all other components.</selection_reason>
    </constraint>
    <constraint id="C3" name="Simplified Record-Keeping" generation_order="3">
      <base_properties>
        <epsilon>0.60</epsilon>
        <suppression>0.50</suppression>
        <coordination>true</coordination>
        <asymmetric>true</asymmetric>
      </base_properties>
      <graph>
        <downstream_of>C1, C2</downstream_of>
        <feeds_into>none</feeds_into>
      </graph>
      <character_classifications>
        <character name="Warden">
          <index>
            <power>moderate</power>
            <time>biographical</time>
            <exit>constrained</exit>
            <scope>regional</scope>
          </index>
          <chi>0.54</chi>
          <type>Tangled Rope</type>
        </character>
        <character name="Core">
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
      <indexical_variance>true</indexical_variance>
      <selection_reason>Selected as the highest-centrality constraint. It is the downstream point where all systemic failures are laundered into an official, "correct" record. Its indexical variance (Tangled Rope for the enforcer, Snare for the victim) captures the core moral and structural conflict.</selection_reason>
    </constraint>
  </selected>
  <deferred>
    <constraint id="C4" name="Scripted Response Protocol">
      <hypothesis>Tangled Rope</hypothesis>
      <offstage_function>Demonstrates the system's inability to handle novelty or context, making its solutions absurd and highlighting the gap between protocol and reality.</offstage_function>
    </constraint>
    <constraint id="C5" name="Circular Escalation Path">
      <hypothesis>Tangled Rope</hypothesis>
      <offstage_function>Creates a sense of bureaucratic futility, isolating the protagonist and proving that the system's self-correction mechanisms are an illusion.</offstage_function>
    </constraint>
    <constraint id="C6" name="Protocol Immutability">
      <hypothesis>Tangled Rope</hypothesis>
      <offstage_function>Acts as a meta-constraint, providing the ideological background that prevents the protagonist from questioning the rules themselves, forcing him to operate within a flawed framework.</offstage_function>
    </constraint>
  </deferred>
  <generation_sequence>C1 → C2 → C3</generation_sequence>
  <invariant_contract>
    <untranslatable_real present="yes">An organism's emergent, embodied adaptation to a chronic stressor is a form of knowledge that cannot be captured by a system designed only to measure deviation from a static ideal.</untranslatable_real>
    <missing_floor present="yes">A system of control founded on a sharp, binary partition of a continuous reality will inevitably collapse into injustice when faced with phenomena that exist at the boundary.</missing_floor>
    <inherent_instrument value="yes">The constraint is an action triggered by a numerical reading from a device; without the device and its reading, the specific obligation to act does not exist.</inherent_instrument>
  </invariant_contract>
  <break_contract>
    <original_break>A story about a diligent functionary in a complex system is expected to be about that functionary discovering and correcting a flaw; this story is about the functionary discovering a flaw and being compelled by the system's logic to become an agent of its injustice.</original_break>
    <prior_status>LIVE</prior_status>
    <target_prior>The belief that a person of integrity operating within a system can correct its failings through diligence and adherence to its own stated rules.</target_prior>
  </break_contract>
  <omegas>
    <omega id="beneficiary">The identity and intent of the "upper administration" are unknown; it is unclear if the system's flaws are a result of neglect, deliberate design, or emergent dysfunction.</omega>
  </omegas>
</constraint_manifest>
```