```xml
<constraint_manifest>
  <selected>
    <constraint id="C1" name="Recognition_as_Threat" generation_order="1">
      <base_properties>
        <epsilon>0.45</epsilon>
        <suppression>0.40</suppression>
        <coordination>true</coordination>
        <asymmetric>true</asymmetric>
      </base_properties>
      <graph>
        <downstream_of>none</downstream_of>
        <feeds_into>C3</feeds_into>
      </graph>
      <character_classifications>
        <character name="little people">
          <index>
            <power>powerless</power>
            <time>biographical</time>
            <exit>trapped</exit>
            <scope>national</scope>
          </index>
          <chi>0.675</chi>
          <type>Tangled Rope</type>
        </character>
        <character name="players">
          <index>
            <power>institutional</power>
            <time>biographical</time>
            <exit>arbitrage</exit>
            <scope>national</scope>
          </index>
          <chi>-0.09</chi>
          <type>Rope</type>
        </character>
      </character_classifications>
      <indexical_variance>High. The rule of engagement that determines survival is a coordinating Rope for the powerful, but a hybrid coordination-extraction Tangled Rope for the powerless.</indexical_variance>
      <selection_reason>This Tangled Rope is the foundational rule of the system, defining the two classes of people and the only path for transitioning between them. It is the most structurally distinct upstream constraint.</selection_reason>
    </constraint>
    <constraint id="C2" name="Impersonal_Harm" generation_order="2">
      <base_properties>
        <epsilon>0.70</epsilon>
        <suppression>0.60</suppression>
        <coordination>true</coordination>
        <asymmetric>true</asymmetric>
      </base_properties>
      <graph>
        <downstream_of>none</downstream_of>
        <feeds_into>C3</feeds_into>
      </graph>
      <character_classifications>
        <character name="little people">
          <index>
            <power>powerless</power>
            <time>biographical</time>
            <exit>trapped</exit>
            <scope>national</scope>
          </index>
          <chi>1.05</chi>
          <type>Snare</type>
        </character>
        <character name="players">
          <index>
            <power>institutional</power>
            <time>biographical</time>
            <exit>arbitrage</exit>
            <scope>national</scope>
          </index>
          <chi>-0.14</chi>
          <type>Rope</type>
        </character>
      </character_classifications>
      <indexical_variance>High. The narrative frame that justifies violence is a coordinating principle (Rope) for its authors, but a pure extraction trap (Snare) for its victims.</indexical_variance>
      <selection_reason>This narrative constraint is a high-centrality, upstream Snare that provides the ideological justification for the system's violence, making it structurally essential.</selection_reason>
    </constraint>
    <constraint id="C3" name="Systemic_Liquidation" generation_order="3">
      <base_properties>
        <epsilon>1.00</epsilon>
        <suppression>0.90</suppression>
        <coordination>true</coordination>
        <asymmetric>true</asymmetric>
      </base_properties>
      <graph>
        <downstream_of>C1, C2</downstream_of>
        <feeds_into>none</feeds_into>
      </graph>
      <character_classifications>
        <character name="little people">
          <index>
            <power>powerless</power>
            <time>biographical</time>
            <exit>trapped</exit>
            <scope>national</scope>
          </index>
          <chi>1.50</chi>
          <type>Snare</type>
        </character>
        <character name="players">
          <index>
            <power>institutional</power>
            <time>biographical</time>
            <exit>arbitrage</exit>
            <scope>national</scope>
          </index>
          <chi>-0.20</chi>
          <type>Rope</type>
        </character>
      </character_classifications>
      <indexical_variance>High. The act of eliminating the powerless is a coordinating tool of statecraft (Rope) for the powerful, but the ultimate extractive Snare for the powerless.</indexical_variance>
      <selection_reason>This is the highest-centrality constraint, representing the ultimate downstream consequence of the entire system. It is the primary form of extraction the other constraints exist to enable.</selection_reason>
    </constraint>
  </selected>

  <deferred>
    <constraint id="C4" name="Instrumental_Justice">
      <hypothesis>Snare</hypothesis>
      <offstage_function>Provides the institutional mechanism and legitimizing theater for Systemic_Liquidation, making the violence feel official and inevitable.</offstage_function>
    </constraint>
    <constraint id="C5" name="Atomization">
      <hypothesis>Snare</hypothesis>
      <offstage_function>Explains the lack of effective collective opposition, forcing resistance to be individual, violent, and personal, thereby reinforcing the logic of the Recognition_as_Threat constraint.</offstage_function>
    </constraint>
    <constraint id="C6" name="Personalized_Resistance">
      <hypothesis>Rope</hypothesis>
      <offstage_function>Acts as the guiding philosophy for characters resisting the system, shaping their behavior toward direct, personal conflict rather than systemic reform.</offstage_function>
    </constraint>
  </deferred>

  <generation_sequence>C1 → C2 → C3</generation_sequence>

  <invariant_contract>
    <untranslatable_real present="no" primary="no">absent</untranslatable_real>
    <missing_floor present="yes" primary="yes">A system's foundational act is to partition people into two kinds: those who are ends in themselves and those who are means, subject to disposal.</missing_floor>
    <inherent_instrument value="yes">The constraint's power derives from its claim to be the sole legitimate arbiter of outcomes, a claim certified by a formal apparatus.</inherent_instrument>
  </invariant_contract>

  <break_contract>
    <original_break>The expectation that a system's designated channels for grievance are the legitimate and only arena for conflict is violated.</original_break>
    <prior_status>LIVE</prior_status>
    <target_prior>The belief that established institutions are the proper and exclusive venue for redressing harms caused by those same institutions must be violated.</target_prior>
  </break_contract>

  <omegas>
    <omega id="response_effectiveness">The analysis cannot resolve whether the proposed strategy of personalized violence is an effective path to being seen as a "player" or simply a trap that accelerates liquidation.</omega>
  </omegas>
</constraint_manifest>
```