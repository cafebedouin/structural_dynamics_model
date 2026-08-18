```xml
<constraint_manifest>
  <selected>
    <constraint id="C1" name="foundational_precarity" generation_order="1">
      <base_properties>
        <epsilon>0.90</epsilon>
        <suppression>0.00</suppression>
        <coordination>false</coordination>
        <asymmetric>false</asymmetric>
      </base_properties>
      <graph>
        <downstream_of>none</downstream_of>
        <feeds_into>C2</feeds_into>
      </graph>
      <character_classifications>
        <character name="Inhabitant">
          <index>
            <power>moderate</power>
            <time>biographical</time>
            <exit>identity_locked</exit>
            <scope>local</scope>
          </index>
          <chi>0.72</chi>
          <type>Snare</type>
        </character>
        <character name="Intellectual">
          <index>
            <power>analytical</power>
            <time>biographical</time>
            <exit>constrained</exit>
            <scope>local</scope>
          </index>
          <chi>0.83</chi>
          <type>Snare</type>
        </character>
        <character name="Child Prodigy">
          <index>
            <power>moderate</power>
            <time>biographical</time>
            <exit>mobile</exit>
            <scope>local</scope>
          </index>
          <chi>0.72</chi>
          <type>Snare</type>
        </character>
      </character_classifications>
      <indexical_variance>None. The foundational condition of the environment is so overwhelming that it functions as a Snare for all inhabitants, regardless of their analytical capacity or future potential.</indexical_variance>
      <selection_reason>Highest-centrality upstream constraint. It is the source of all other major social dynamics and psychological states in the system.</selection_reason>
    </constraint>
    <constraint id="C2" name="compensatory_production" generation_order="2">
      <base_properties>
        <epsilon>0.60</epsilon>
        <suppression>0.70</suppression>
        <coordination>true</coordination>
        <asymmetric>true</asymmetric>
      </base_properties>
      <graph>
        <downstream_of>C1</downstream_of>
        <feeds_into>C3</feeds_into>
      </graph>
      <character_classifications>
        <character name="Inhabitant">
          <index>
            <power>moderate</power>
            <time>biographical</time>
            <exit>identity_locked</exit>
            <scope>local</scope>
          </index>
          <chi>0.48</chi>
          <type>Tangled Rope</type>
        </character>
        <character name="Intellectual">
          <index>
            <power>analytical</power>
            <time>biographical</time>
            <exit>constrained</exit>
            <scope>local</scope>
          </index>
          <chi>0.55</chi>
          <type>Tangled Rope</type>
        </character>
        <character name="Child Prodigy">
          <index>
            <power>powerful</power>
            <time>biographical</time>
            <exit>mobile</exit>
            <scope>local</scope>
          </index>
          <chi>0.29</chi>
          <type>Rope</type>
        </character>
      </character_classifications>
      <indexical_variance>High. The same system of social production is experienced as a hybrid coordination-extraction mechanism by the general populace, but as a purely functional coordination system by the emerging powerful generation.</indexical_variance>
      <selection_reason>Highest-centrality constraint overall. It is the primary social adaptation to C1 and the direct cause of C3, making it the core of the narrative's social logic.</selection_reason>
    </constraint>
    <constraint id="C3" name="authority_inversion" generation_order="3">
      <base_properties>
        <epsilon>0.70</epsilon>
        <suppression>0.20</suppression>
        <coordination>false</coordination>
        <asymmetric>true</asymmetric>
      </base_properties>
      <graph>
        <downstream_of>C2</downstream_of>
        <feeds_into>none</feeds_into>
      </graph>
      <character_classifications>
        <character name="Inhabitant">
          <index>
            <power>powerless</power>
            <time>biographical</time>
            <exit>trapped</exit>
            <scope>local</scope>
          </index>
          <chi>0.84</chi>
          <type>Snare</type>
        </character>
        <character name="Child Prodigy">
          <index>
            <power>institutional</power>
            <time>biographical</time>
            <exit>arbitrage</exit>
            <scope>local</scope>
          </index>
          <chi>-0.11</chi>
          <type>Rope</type>
        </character>
      </character_classifications>
      <indexical_variance>Extreme. The generational transfer of knowledge and authority is a trap for the parent generation (who become powerless) and a pure benefit for the child generation (who become the new institution).</indexical_variance>
      <selection_reason>Selected for its structural distinctness and high indexical variance. It represents the ultimate consequence of the system defined by C1 and C2, showing how the society's adaptations are producing its own replacement.</selection_reason>
    </constraint>
  </selected>
  <deferred>
    <constraint id="C4" name="the_magritte_problem">
      <hypothesis>Tangled Rope</hypothesis>
      <offstage_function>Provides the philosophical and aesthetic language for the inhabitants' anxiety, preventing them from fully normalizing their precarious reality.</offstage_function>
    </constraint>
    <constraint id="C5" name="the_globe_problem">
      <hypothesis>Snare</hypothesis>
      <offstage_function>Acts as a background pressure that locks inhabitants into the system by making exit seem more dangerous than staying, reinforcing their commitment to C2.</offstage_function>
    </constraint>
    <constraint id="C6" name="mirrors_of_the_past">
      <hypothesis>Rope</hypothesis>
      <offstage_function>Subtly reinforces the society's inability to envision a future, making them dependent on history and external services for self-perception.</offstage_function>
    </constraint>
    <constraint id="C7" name="collective_fatalism">
      <hypothesis>Tangled Rope</hypothesis>
      <offstage_function>Explains the absence of political or collective action to solve C1, channeling all energy into the individualist solution of C2.</offstage_function>
    </constraint>
  </deferred>
  <generation_sequence>C1 → C2 → C3</generation_sequence>
  <invariant_contract>
    <untranslatable_real present="yes" primary="no">A shared, internal experience of groundlessness binds a populace, but any attempt to measure or name it from the outside reduces it to a clinical symptom, missing its unifying truth.</untranslatable_real>
    <missing_floor present="yes" primary="yes">A society is founded on a violent and arbitrary suspension of natural law, a choice that is then normalized as a permanent, given condition.</missing_floor>
    <inherent_instrument value="no">The constraints are embedded in the physical and social environment directly, not mediated through a system of certified measurement.</inherent_instrument>
  </invariant_contract>
  <break_contract>
    <original_break>A story that appears to be a political allegory or a fantasy epic resolves into a philosophical meditation on uncertainty, refusing to provide the expected narrative clarity or resolution.</original_break>
    <prior_status>LIVE</prior_status>
    <target_prior>A story that presents a central, solvable-seeming problem will not solve it, but will instead explore the psychological state of living with the problem.</target_prior>
  </break_contract>
  <omegas>
    <omega id="future_state">The analysis cannot resolve whether the new generation will solve the foundational precarity or simply manage it more effectively, becoming new stewards of the same essential trap.</omega>
  </omegas>
</constraint_manifest>
```