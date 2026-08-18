```xml
<constraint_manifest>
  <selected>
    <constraint id="C1" name="Baseline_Physical_Form" generation_order="1">
      <base_properties>
        <epsilon>0.10</epsilon>
        <suppression>0.00</suppression>
        <coordination>true</coordination>
        <asymmetric>false</asymmetric>
      </base_properties>
      <graph>
        <downstream_of>none</downstream_of>
        <feeds_into>C3</feeds_into>
      </graph>
      <character_classifications>
        <character name="Kovaloff">
          <index>
            <power>moderate</power>
            <time>immediate</time>
            <exit>trapped</exit>
            <scope>regional</scope>
          </index>
          <chi>0.09</chi>
          <type>Mountain</type>
        </character>
      </character_classifications>
      <indexical_variance>None. This is a biological norm that functions as a background fact for all characters.</indexical_variance>
      <selection_reason>This is the upstream, inciting constraint. The violation of this seemingly unchangeable fact of reality triggers the entire narrative and exposes the logic of the other, socially constructed constraints.</selection_reason>
    </constraint>
    <constraint id="C2" name="Rank-as-Identity" generation_order="2">
      <base_properties>
        <epsilon>0.80</epsilon>
        <suppression>0.70</suppression>
        <coordination>true</coordination>
        <asymmetric>true</asymmetric>
      </base_properties>
      <graph>
        <downstream_of>none</downstream_of>
        <feeds_into>C3</feeds_into>
      </graph>
      <character_classifications>
        <character name="Kovaloff">
          <index>
            <power>moderate</power>
            <time>immediate</time>
            <exit>identity_locked</exit>
            <scope>regional</scope>
          </index>
          <chi>0.72</chi>
          <type>Tangled Rope</type>
        </character>
        <character name="The Nose">
          <index>
            <power>powerful</power>
            <time>immediate</time>
            <exit>mobile</exit>
            <scope>regional</scope>
          </index>
          <chi>0.43</chi>
          <type>Tangled Rope</type>
        </character>
        <character name="Ivan Jakovlevitch">
          <index>
            <power>powerless</power>
            <time>immediate</time>
            <exit>trapped</exit>
            <scope>local</scope>
          </index>
          <chi>0.96</chi>
          <type>Snare</type>
        </character>
      </character_classifications>
      <indexical_variance>High. The same system of social hierarchy is a Snare for the powerless, a Tangled Rope for those in the middle (both benefiting and trapped by it), and a different flavor of Tangled Rope for those who successfully climb it.</indexical_variance>
      <selection_reason>This is the highest-centrality constraint. It is the core logic of the story's world, determining the value, agency, and even the reality of the characters. Its absurd application to a body part drives the central conflict.</selection_reason>
    </constraint>
    <constraint id="C3" name="Inadmissible_Phenomena" generation_order="3">
      <base_properties>
        <epsilon>0.60</epsilon>
        <suppression>0.60</suppression>
        <coordination>true</coordination>
        <asymmetric>true</asymmetric>
      </base_properties>
      <graph>
        <downstream_of>C1, C2</downstream_of>
        <feeds_into>none</feeds_into>
      </graph>
      <character_classifications>
        <character name="Kovaloff">
          <index>
            <power>powerless</power>
            <time>immediate</time>
            <exit>trapped</exit>
            <scope>regional</scope>
          </index>
          <chi>0.81</chi>
          <type>Snare</type>
        </character>
        <character name="Ad Clerk">
          <index>
            <power>institutional</power>
            <time>biographical</time>
            <exit>mobile</exit>
            <scope>regional</scope>
          </index>
          <chi>-0.11</chi>
          <type>Rope</type>
        </character>
      </character_classifications>
      <indexical_variance>High. The rule that only plausible events can be processed is a Snare for the person experiencing an impossible reality, but it is a functional Rope for the bureaucrat whose job it protects from chaos.</indexical_variance>
      <selection_reason>This constraint is the primary downstream consequence and the engine of the plot's second act. It shows how the system (C2) defends itself against events (violations of C1) that challenge its logic, making it a crucial part of the structural narrative.</selection_reason>
    </constraint>
  </selected>
  <deferred>
    <constraint id="C4" name="Low-Status_Criminality_Default">
      <hypothesis>Snare</hypothesis>
      <offstage_function>This provides the motivation for the barber's terror in the opening scenes, establishing the high stakes of any deviation from the norm for those at the bottom of the hierarchy defined by C2.</offstage_function>
    </constraint>
  </deferred>
  <generation_sequence>C1 → C2 → C3</generation_sequence>
  <invariant_contract>
    <untranslatable_real present="no" primary="no">absent</untranslatable_real>
    <missing_floor present="yes" primary="yes">A person's value and reality are defined entirely by their position within a formal hierarchy, leaving no ground for identity outside of that system.</missing_floor>
    <inherent_instrument value="yes">The system of deference is entirely mediated by certified markers of status; without the visible instrument, the hierarchy it represents ceases to function.</inherent_instrument>
  </invariant_contract>
  <break_contract>
    <original_break>A story about a man's misadventures will follow the logic of a stable, shared reality, even if it is satirical.</original_break>
    <prior_status>DEAD</prior_status>
    <target_prior>A story that breaks the rules of reality will do so to make a metaphorical or allegorical point about a recognizable human world.</target_prior>
  </break_contract>
  <omegas>
    <omega id="causality_gap">The analysis cannot resolve the physical mechanism by which the central event occurs or resolves, as the source deliberately omits it to focus on the social system's reaction.</omega>
  </omegas>
</constraint_manifest>
```