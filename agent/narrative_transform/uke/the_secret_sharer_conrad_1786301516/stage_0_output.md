```xml
<constraint_manifest>
  <selected>
    <constraint id="C1" name="Formal_Responsibility" generation_order="1">
      <base_properties>
        <epsilon>0.20</epsilon>
        <suppression>0.10</suppression>
        <coordination>true</coordination>
        <asymmetric>true</asymmetric>
      </base_properties>
      <graph>
        <downstream_of>none</downstream_of>
        <feeds_into>C2, C3</feeds_into>
      </graph>
      <character_classifications>
        <character name="The Captain">
          <index>
            <power>powerful</power>
            <time>biographical</time>
            <exit>identity_locked</exit>
            <scope>local</scope>
          </index>
          <chi>0.096</chi>
          <type>Rope</type>
        </character>
        <character name="Captain Archbold">
          <index>
            <power>powerful</power>
            <time>biographical</time>
            <exit>identity_locked</exit>
            <scope>local</scope>
          </index>
          <chi>0.096</chi>
          <type>Rope</type>
        </character>
      </character_classifications>
      <indexical_variance>false</indexical_variance>
      <selection_reason>This foundational Rope establishes the system of authority and the high stakes of command, providing the essential structure that the other two constraints will test and subvert.</selection_reason>
    </constraint>
    <constraint id="C2" name="Outsider_Legitimacy_Test" generation_order="2">
      <base_properties>
        <epsilon>0.40</epsilon>
        <suppression>0.30</suppression>
        <coordination>true</coordination>
        <asymmetric>true</asymmetric>
      </base_properties>
      <graph>
        <downstream_of>C1</downstream_of>
        <feeds_into>C3</feeds_into>
      </graph>
      <character_classifications>
        <character name="The Captain">
          <index>
            <power>moderate</power>
            <time>biographical</time>
            <exit>trapped</exit>
            <scope>local</scope>
          </index>
          <chi>0.32</chi>
          <type>Rope</type>
        </character>
        <character name="The Crew">
          <index>
            <power>organized</power>
            <time>biographical</time>
            <exit>constrained</exit>
            <scope>local</scope>
          </index>
          <chi>0.128</chi>
          <type>Rope</type>
        </character>
      </character_classifications>
      <indexical_variance>false</indexical_variance>
      <selection_reason>This constraint introduces the social conflict, showing the gap between formal authority (C1) and earned trust. It creates the psychological isolation that makes the Captain receptive to the central Tangled Rope.</selection_reason>
    </constraint>
    <constraint id="C3" name="Subversive_Solidarity" generation_order="3">
      <base_properties>
        <epsilon>0.75</epsilon>
        <suppression>0.90</suppression>
        <coordination>true</coordination>
        <asymmetric>true</asymmetric>
      </base_properties>
      <graph>
        <downstream_of>C1, C2</downstream_of>
        <feeds_into>none</feeds_into>
      </graph>
      <character_classifications>
        <character name="The Captain">
          <index>
            <power>powerless</power>
            <time>immediate</time>
            <exit>trapped</exit>
            <scope>local</scope>
          </index>
          <chi>0.90</chi>
          <type>Tangled Rope</type>
        </character>
        <character name="Leggatt">
          <index>
            <power>institutional</power>
            <time>immediate</time>
            <exit>constrained</exit>
            <scope>local</scope>
          </index>
          <chi>-0.12</chi>
          <type>Rope</type>
        </character>
        <character name="The Crew">
          <index>
            <power>moderate</power>
            <time>immediate</time>
            <exit>constrained</exit>
            <scope>local</scope>
          </index>
          <chi>0.60</chi>
          <type>Tangled Rope</type>
        </character>
      </character_classifications>
      <indexical_variance>true</indexical_variance>
      <selection_reason>This is the highest-centrality constraint, a Tangled Rope that captures the core drama. It provides coordination for the two allies while extracting from the formal system, and its indexical variance (Rope for the beneficiary, Tangled Rope for others) drives the narrative tension.</selection_reason>
    </constraint>
  </selected>
  <deferred>
    <constraint id="C4" name="Fugitive_Status">
      <hypothesis>Snare</hypothesis>
      <offstage_function>Provides the inciting incident and the external threat (in the form of Captain Archbold's search) that tests the primary secret alliance.</offstage_function>
    </constraint>
    <constraint id="C5" name="Institutional_Confinement">
      <hypothesis>Mountain</hypothesis>
      <offstage_function>Acts as a universal amplifier, making all social tensions more acute and secrecy more difficult by trapping all agents in a closed, observable system.</offstage_function>
    </constraint>
    <constraint id="C6" name="Professional_Identity_Code">
      <hypothesis>Rope</hypothesis>
      <offstage_function>Serves as the ideological background, defining the shared values ("Conway boy") that enable the subversive solidarity between the Captain and Leggatt in the first place.</offstage_function>
    </constraint>
    <constraint id="C7" name="Pre-judged_Morality">
      <hypothesis>Piton</hypothesis>
      <offstage_function>Structurally removes moral ambiguity about the fugitive's crime, allowing the central conflict to focus purely on loyalty versus law, rather than justice versus law.</offstage_function>
    </constraint>
  </deferred>
  <generation_sequence>C1 → C2 → C3</generation_sequence>
  <invariant_contract>
    <untranslatable_real present="yes" primary="yes">The intuitive, unspoken recognition between two people who share a fundamental nature or outlook, which precedes and overrides formal codes or laws.</untranslatable_real>
    <missing_floor present="yes" primary="no">A system of formal judgment presupposes its own competence to assess actions taken under extreme, contextual duress, a founding choice that leaves no neutral ground for acts legible only to those who were there.</missing_floor>
    <inherent_instrument value="no">The constraints are mediated by social perception and legal codes, not by a specific, removable measurement instrument.</inherent_instrument>
  </invariant_contract>
  <break_contract>
    <original_break>A leader's primary loyalty is to their institution and its rules, not to an outlaw who embodies a shared personal code.</original_break>
    <prior_status>DEAD</prior_status>
    <target_prior>A protagonist's internal, psychological victory is more important than the messy, ambiguous external consequences of their actions.</target_prior>
  </break_contract>
  <omegas>
    <omega id="crew_awareness">The analysis cannot resolve the crew's actual state of knowledge versus the captain's paranoid projections of their suspicion.</omega>
  </omegas>
</constraint_manifest>
```