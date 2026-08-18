```xml
<constraint_manifest>
  <selected>
    <constraint id="C1" name="The Immutable Partition" generation_order="1">
      <base_properties>
        <epsilon>0.10</epsilon>
        <suppression>0.05</suppression>
        <coordination>true</coordination>
        <asymmetric>false</asymmetric>
      </base_properties>
      <graph>
        <downstream_of>none</downstream_of>
        <feeds_into>C2, C3</feeds_into>
      </graph>
      <character_classifications>
        <character name="Orpheus">
          <index>
            <power>powerless</power>
            <time>biographical</time>
            <exit>trapped</exit>
            <scope>universal</scope>
          </index>
          <chi>0.15</chi>
          <type>Mountain</type>
        </character>
        <character name="Pluto">
          <index>
            <power>institutional</power>
            <time>civilizational</time>
            <exit>arbitrage</exit>
            <scope>universal</scope>
          </index>
          <chi>-0.02</chi>
          <type>Rope</type>
        </character>
      </character_classifications>
      <indexical_variance>Yes. The fundamental separation of life and death is unchangeable terrain (Mountain) for a mortal subject, but a managed system of coordination (Rope) for its institutional administrator.</indexical_variance>
      <selection_reason>This is the foundational constraint of the setting, the upstream source of all other conflicts. Its apparent naturalness is what the protagonist's actions challenge.</selection_reason>
    </constraint>
    <constraint id="C2" name="The Power of Affect" generation_order="2">
      <base_properties>
        <epsilon>0.30</epsilon>
        <suppression>0.40</suppression>
        <coordination>true</coordination>
        <asymmetric>true</asymmetric>
      </base_properties>
      <graph>
        <downstream_of>C1</downstream_of>
        <feeds_into>C3</feeds_into>
      </graph>
      <character_classifications>
        <character name="Orpheus">
          <index>
            <power>moderate</power>
            <time>biographical</time>
            <exit>constrained</exit>
            <scope>universal</scope>
          </index>
          <chi>0.30</chi>
          <type>Rope</type>
        </character>
        <character name="Shades">
          <index>
            <power>powerless</power>
            <time>civilizational</time>
            <exit>trapped</exit>
            <scope>regional</scope>
          </index>
          <chi>0.41</chi>
          <type>Tangled Rope</type>
        </character>
      </character_classifications>
      <indexical_variance>Yes. For the master artist, art is a tool of agency (Rope). For the audience experiencing it, it is a hybrid that provides the coordinating value of shared meaning while also extracting painful, forgotten emotions (Tangled Rope).</indexical_variance>
      <selection_reason>Highest centrality score (6). This constraint represents the story's central mechanic—the force that allows the foundational constraint (C1) to be challenged, and which necessitates the creation of the terminal constraint (C3).</selection_reason>
    </constraint>
    <constraint id="C3" name="The Conditional Reprieve" generation_order="3">
      <base_properties>
        <epsilon>0.80</epsilon>
        <suppression>0.90</suppression>
        <coordination>false</coordination>
        <asymmetric>true</asymmetric>
      </base_properties>
      <graph>
        <downstream_of>C1, C2</downstream_of>
        <feeds_into>none</feeds_into>
      </graph>
      <character_classifications>
        <character name="Orpheus">
          <index>
            <power>powerless</power>
            <time>immediate</time>
            <exit>trapped</exit>
            <scope>local</scope>
          </index>
          <chi>0.96</chi>
          <type>Snare</type>
        </character>
        <character name="Pluto">
          <index>
            <power>institutional</power>
            <time>immediate</time>
            <exit>arbitrage</exit>
            <scope>local</scope>
          </index>
          <chi>-0.13</chi>
          <type>Rope</type>
        </character>
      </character_classifications>
      <indexical_variance>Yes. The bargain is a trap designed to fail by exploiting human weakness (Snare) for the person subjected to it, while for its author it is a low-cost administrative tool (Rope) to manage an exception and restore the status quo.</indexical_variance>
      <selection_reason>This is the story's dramatic climax, a downstream consequence of the other two constraints. It is a pure Snare for the protagonist and crystallizes the tragic logic of the system.</selection_reason>
    </constraint>
  </selected>

  <deferred>
    <constraint id="C4" name="Systemic Oblivion">
      <hypothesis>Mountain</hypothesis>
      <offstage_function>This constraint establishes the baseline state of the afterlife, making the protagonist's ability to reverse it a powerful demonstration of his unique capabilities and raising the stakes of his quest.</offstage_function>
    </constraint>
    <constraint id="C5" name="Arbitrary Misfortune">
      <hypothesis>Mountain</hypothesis>
      <offstage_function>This serves as the inciting incident, establishing a non-moralistic universe where tragedy is not a punishment but a random event, focusing the story on the response to loss rather than its cause.</offstage_function>
    </constraint>
  </deferred>

  <generation_sequence>C1 → C2 → C3</generation_sequence>

  <invariant_contract>
    <untranslatable_real present="yes" primary="yes">The force of personal devotion is irreducible and cannot be accounted for or extinguished by systems designed to process souls in aggregate.</untranslatable_real>
    <missing_floor present="yes" primary="no">A jurisdictional boundary between states of being is presented as a natural law, obscuring the fact that it is an enforced political settlement.</missing_floor>
    <inherent_instrument value="no">The primary constraints are enforced through direct sovereign power and natural consequence, not through a system of symbolic measurement.</inherent_instrument>
  </invariant_contract>

  <break_contract>
    <original_break>A mortal can successfully petition the sovereign of the dead and win back a lost soul.</original_break>
    <prior_status>DEAD</prior_status>
    <target_prior>A demonstration of perfect, world-altering love is not sufficient to guarantee a happy ending; human fallibility can still snatch defeat from the jaws of victory.</target_prior>
  </break_contract>

  <omegas>
    <omega id="sovereign_motive">The analysis cannot resolve whether the sovereign who offers the conditional reprieve does so out of genuine pity (believing the subject can succeed) or cynical realpolitik (knowing the condition is designed to fail).</omega>
  </omegas>
</constraint_manifest>
```