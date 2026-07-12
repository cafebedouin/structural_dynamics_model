```xml
<constraint_manifest>
  <selected>
    <constraint id="C1" name="Unownable_Truth" generation_order="1">
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
          <chi>0.06</chi>
          <type>Mountain</type>
        </character>
        <character name="Verrel">
          <index>
            <power>analytical</power>
            <time>biographical</time>
            <exit>identity_locked</exit>
            <scope>local</scope>
          </index>
          <chi>0.046</chi>
          <type>Mountain</type>
        </character>
      </character_classifications>
      <indexical_variance>None. This constraint is a natural law that functions as a Mountain from all observed indices.</indexical_variance>
      <selection_reason>This constraint represents the unobtainable ideal against which all other constraints are measured, providing the philosophical foundation for the central conflict.</selection_reason>
    </constraint>
    <constraint id="C2" name="Foundational_Choice" generation_order="2">
      <base_properties>
        <epsilon>0.1</epsilon>
        <suppression>0.8</suppression>
        <coordination>true</coordination>
        <asymmetric>true</asymmetric>
      </base_properties>
      <graph>
        <downstream_of>none</downstream_of>
        <feeds_into>C3</feeds_into>
      </graph>
      <character_classifications>
        <character name="Narrator (apprentice)">
          <index>
            <power>powerless</power>
            <time>biographical</time>
            <exit>identity_locked</exit>
            <scope>local</scope>
          </index>
          <chi>0.12</chi>
          <type>Mountain</type>
        </character>
        <character name="Verrel">
          <index>
            <power>analytical</power>
            <time>biographical</time>
            <exit>identity_locked</exit>
            <scope>local</scope>
          </index>
          <chi>0.092</chi>
          <type>Rope</type>
        </character>
      </character_classifications>
      <indexical_variance>Yes. From a powerless index, the constraint is immutable and appears as a Mountain (a Type I Error: False Mountain). From an analytical index, its constructed nature is visible, and it classifies as a Rope.</indexical_variance>
      <selection_reason>This constraint is the direct upstream enabler of the primary conflict (C3) and demonstrates indexical variance, revealing how a constructed choice can be naturalized into an apparent law.</selection_reason>
    </constraint>
    <constraint id="C3" name="Instrumentalized_Standard" generation_order="3">
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
            <exit>identity_locked</exit>
            <scope>local</scope>
          </index>
          <chi>0.96</chi>
          <type>Snare</type>
        </character>
        <character name="Narrator (adult)">
          <index>
            <power>moderate</power>
            <time>biographical</time>
            <exit>identity_locked</exit>
            <scope>local</scope>
          </index>
          <chi>0.64</chi>
          <type>Tangled Rope</type>
        </character>
        <character name="Verrel">
          <index>
            <power>analytical</power>
            <time>biographical</time>
            <exit>identity_locked</exit>
            <scope>local</scope>
          </index>
          <chi>0.736</chi>
          <type>Snare</type>
        </character>
        <character name="Merchants">
          <index>
            <power>powerless</power>
            <time>immediate</time>
            <exit>constrained</exit>
            <scope>local</scope>
          </index>
          <chi>0.96</chi>
          <type>Snare</type>
        </character>
      </character_classifications>
      <indexical_variance>Yes. The constraint is a Snare for the powerless and the analytical, who feel its extraction sharply. For the experienced insider (adult narrator), it has been partially normalized into a Tangled Rope, a corrupt but functional part of the system.</indexical_variance>
      <selection_reason>This is the highest-centrality constraint, representing the story's core mechanism of asymmetric extraction and coordination. Its indexical variance is key to the narrator's character development.</selection_reason>
    </constraint>
  </selected>

  <deferred>
    <constraint id="C4" name="Assayers_Duty">
      <hypothesis>Tangled Rope</hypothesis>
      <offstage_function>This constraint explains the lack of overt rebellion by binding the knowledgeable agents (assayers) to the corrupt system through professional identity, forcing their dissent to be personal and covert.</offstage_function>
    </constraint>
    <constraint id="C5" name="Physical_Cold">
      <hypothesis>Mountain</hypothesis>
      <offstage_function>This constraint provides a constant, physical analogue to the abstract, ethical pain of the other constraints, grounding the philosophical crisis in bodily experience.</offstage_function>
    </constraint>
  </deferred>

  <generation_sequence>C1 → C2 → C3</generation_sequence>

  <invariant_contract>
    <untranslatable_real present="yes">An absolute, underlying state of correctness exists, but it is corrupted by the very act of observation or possession intended to capture it for a specific purpose or person.</untranslatable_real>
    <missing_floor present="yes">The foundational baseline from which all judgments of deviation are made is itself an ungrounded, arbitrary choice presented as an objective fact.</missing_floor>
    <inherent_instrument value="yes">The extraction is actualized through a certified measurement against a physical standard; without the act of measurement, the extracted value cannot be defined.</inherent_instrument>
  </invariant_contract>

  <omegas>
    <omega id="long_term_stability">The stability of the system is assumed, but the long-term consequence of the narrator's hidden dissidence (writing in the margins) is unresolved and its potential to cause change is unknown.</omega>
  </omegas>
</constraint_manifest>
```