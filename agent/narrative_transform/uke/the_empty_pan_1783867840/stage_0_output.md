```xml
<constraint_manifest>
  <selected>
    <constraint id="C1" name="Impossibility_of_the_True_Measure" generation_order="1">
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
            <power>analytical</power>
            <time>biographical</time>
            <exit>analytical</exit>
            <scope>universal</scope>
          </index>
          <chi>0.0575</chi>
          <type>Mountain</type>
        </character>
        <character name="Verrel">
          <index>
            <power>analytical</power>
            <time>biographical</time>
            <exit>analytical</exit>
            <scope>universal</scope>
          </index>
          <chi>0.0575</chi>
          <type>Mountain</type>
        </character>
      </character_classifications>
      <indexical_variance>absent</indexical_variance>
      <selection_reason>This constraint is a foundational, unchangeable law of the story's physics that establishes the tragic impossibility of the protagonist's initial goal. It is one of the two upstream Mountains that create the conditions for the central conflict.</selection_reason>
    </constraint>
    <constraint id="C2" name="The_Arbitrary_Foundation" generation_order="2">
      <base_properties>
        <epsilon>0.1</epsilon>
        <suppression>0.1</suppression>
        <coordination>true</coordination>
        <asymmetric>false</asymmetric>
      </base_properties>
      <graph>
        <downstream_of>none</downstream_of>
        <feeds_into>C3</feeds_into>
      </graph>
      <character_classifications>
        <character name="Narrator (master)">
          <index>
            <power>analytical</power>
            <time>biographical</time>
            <exit>analytical</exit>
            <scope>universal</scope>
          </index>
          <chi>0.115</chi>
          <type>Mountain</type>
        </character>
        <character name="Verrel">
          <index>
            <power>analytical</power>
            <time>biographical</time>
            <exit>analytical</exit>
            <scope>universal</scope>
          </index>
          <chi>0.115</chi>
          <type>Mountain</type>
        </character>
      </character_classifications>
      <indexical_variance>absent</indexical_variance>
      <selection_reason>This constraint is the second foundational Mountain, revealing that all systems of measure are built on a choice, not a discovery. It provides the mechanism that the central extractive constraint (C3) exploits.</selection_reason>
    </constraint>
    <constraint id="C3" name="The_Enforced_Crooked_Standard" generation_order="3">
      <base_properties>
        <epsilon>0.8</epsilon>
        <suppression>0.7</suppression>
        <coordination>true</coordination>
        <asymmetric>true</asymmetric>
      </base_properties>
      <graph>
        <downstream_of>C2</downstream_of>
        <feeds_into>none</feeds_into>
      </graph>
      <character_classifications>
        <character name="Narrator (apprentice)">
          <index>
            <power>powerless</power>
            <time>biographical</time>
            <exit>constrained</exit>
            <scope>local</scope>
          </index>
          <chi>0.96</chi>
          <type>Snare</type>
        </character>
        <character name="Narrator (master)">
          <index>
            <power>moderate</power>
            <time>biographical</time>
            <exit>identity_locked</exit>
            <scope>local</scope>
          </index>
          <chi>0.64</chi>
          <type>Tangled Rope</type>
        </character>
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
        <character name="Crown">
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
      <indexical_variance>Present. The same constraint is experienced as a Snare by the powerless, a Tangled Rope by the complicit operator, and a Rope by the institutional beneficiary.</indexical_variance>
      <selection_reason>This is the highest-centrality constraint, representing the story's core socio-political conflict. Its high indexical variance drives the narrative and demonstrates the power-scaling nature of the system's injustice.</selection_reason>
    </constraint>
  </selected>

  <deferred>
    <constraint id="C4" name="Declared_Honesty">
      <hypothesis>Rope</hypothesis>
      <offstage_function>It provides the ethical framework for the protagonist's long-term survival within the corrupt system, shaping her actions as background pressure rather than an active conflict.</offstage_function>
    </constraint>
    <constraint id="C5" name="Cognitive_Closure">
      <hypothesis>Tangled Rope</hypothesis>
      <offstage_function>It explains why the system is so stable and why no widespread rebellion occurs, creating a background of social inertia against which the personal story unfolds.</offstage_function>
    </constraint>
    <constraint id="C6" name="Environmental_Attrition">
      <hypothesis>Rope</hypothesis>
      <offstage_function>It provides a constant, low-level physical pressure that underscores the difficulty of the moral and intellectual challenges the characters face.</offstage_function>
    </constraint>
  </deferred>

  <generation_sequence>C1 → C2 → C3</generation_sequence>

  <invariant_contract>
    <untranslatable_real present="yes">An objective state of affairs exists, but the act of measuring it for a purpose, including for oneself, introduces a bias that corrupts the measurement.</untranslatable_real>
    <missing_floor present="yes">Any system of comparison is built upon a foundational reference point that was established by a choice, not discovered as a fact, and this choice is then hidden to grant the system an appearance of objective authority.</missing_floor>
    <inherent_instrument value="yes">Extraction is executed through a certified reading from a calibrated device, where the device's calibration is the mechanism of the extraction itself.</inherent_instrument>
  </invariant_contract>

  <omegas>
    <omega id="origin_of_corruption">The analysis cannot resolve whether the enforced crookedness of the standard was a deliberate, one-time act of corruption or the inevitable result of any standard being put into long-term use.</omega>
  </omegas>
</constraint_manifest>
```