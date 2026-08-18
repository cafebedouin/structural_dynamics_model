```xml
<constraint_manifest>
  <selected>
    <constraint id="C1" name="Hierarchical cosmic order" generation_order="1">
      <base_properties>
        <epsilon>0.80</epsilon>
        <suppression>0.90</suppression>
        <coordination>true</coordination>
        <asymmetric>true</asymmetric>
      </base_properties>
      <graph>
        <downstream_of>none</downstream_of>
        <feeds_into>C2, C3</feeds_into>
      </graph>
      <character_classifications>
        <character name="Zeus">
          <index>
            <power>institutional</power>
            <time>civilizational</time>
            <exit>arbitrage</exit>
            <scope>universal</scope>
          </index>
          <chi>-0.16</chi>
          <type>Rope</type>
        </character>
        <character name="Prometheus">
          <index>
            <power>powerful</power>
            <time>civilizational</time>
            <exit>constrained</exit>
            <scope>universal</scope>
          </index>
          <chi>0.48</chi>
          <type>Tangled Rope</type>
        </character>
        <character name="Mankind (pre-fire)">
          <index>
            <power>powerless</power>
            <time>generational</time>
            <exit>trapped</exit>
            <scope>universal</scope>
          </index>
          <chi>1.20</chi>
          <type>Snare</type>
        </character>
        <character name="Titans">
          <index>
            <power>powerless</power>
            <time>civilizational</time>
            <exit>trapped</exit>
            <scope>universal</scope>
          </index>
          <chi>1.20</chi>
          <type>Snare</type>
        </character>
      </character_classifications>
      <indexical_variance>High: The same system of cosmic rule is experienced as a beneficial coordination tool (Rope) by its enforcer, a hybrid of value and extraction (Tangled Rope) by a powerful insider, and a pure extraction trap (Snare) by its powerless subjects.</indexical_variance>
      <selection_reason>Highest centrality (6); it is the foundational constraint from which all others in the narrative derive their logic and enforcement.</selection_reason>
    </constraint>
    <constraint id="C2" name="Mandated mortal ignorance" generation_order="2">
      <base_properties>
        <epsilon>0.30</epsilon>
        <suppression>0.20</suppression>
        <coordination>true</coordination>
        <asymmetric>true</asymmetric>
      </base_properties>
      <graph>
        <downstream_of>C1</downstream_of>
        <feeds_into>none</feeds_into>
      </graph>
      <character_classifications>
        <character name="Mankind (pre-fire)">
          <index>
            <power>powerless</power>
            <time>generational</time>
            <exit>identity_locked</exit>
            <scope>global</scope>
          </index>
          <chi>0.54</chi>
          <type>Tangled Rope</type>
        </character>
        <character name="Zeus">
          <index>
            <power>institutional</power>
            <time>civilizational</time>
            <exit>analytical</exit>
            <scope>global</scope>
          </index>
          <chi>-0.07</chi>
          <type>Rope</type>
        </character>
        <character name="Prometheus">
          <index>
            <power>analytical</power>
            <time>civilizational</time>
            <exit>analytical</exit>
            <scope>global</scope>
          </index>
          <chi>0.41</chi>
          <type>Tangled Rope</type>
        </character>
      </character_classifications>
      <indexical_variance>Moderate: The state of blissful ignorance is a beneficial organizing principle (Rope) for the ruler, but a system of limited potential with real costs (Tangled Rope) for both its subjects and the analyst who observes their condition.</indexical_variance>
      <selection_reason>Second highest centrality (5) and represents the "soft power" aspect of C1, providing a structurally distinct mechanism of control (passivity) compared to C3 (violence).</selection_reason>
    </constraint>
    <constraint id="C3" name="Punishment for divine transgression" generation_order="3">
      <base_properties>
        <epsilon>0.95</epsilon>
        <suppression>1.00</suppression>
        <coordination>false</coordination>
        <asymmetric>true</asymmetric>
      </base_properties>
      <graph>
        <downstream_of>C1</downstream_of>
        <feeds_into>none</feeds_into>
      </graph>
      <character_classifications>
        <character name="Prometheus">
          <index>
            <power>powerless</power>
            <time>biographical</time>
            <exit>trapped</exit>
            <scope>universal</scope>
          </index>
          <chi>1.43</chi>
          <type>Snare</type>
        </character>
        <character name="Zeus">
          <index>
            <power>institutional</power>
            <time>civilizational</time>
            <exit>arbitrage</exit>
            <scope>universal</scope>
          </index>
          <chi>-0.19</chi>
          <type>Naturalized</type>
        </character>
      </character_classifications>
      <indexical_variance>High: The act of punishment is a pure extraction trap (Snare) for the one being punished, while for the one wielding power, the extreme base extraction is so completely negated that it becomes a normalized tool of governance (Naturalized).</indexical_variance>
      <selection_reason>High centrality (4) and represents the "hard power" enforcement arm of C1, providing a necessary contrast to the mechanism of C2 and targeting a different class of agent (rebels vs. subjects).</selection_reason>
    </constraint>
  </selected>
  <deferred>
    <constraint id="C4" name="The necessity of civilized toil">
      <hypothesis>Tangled Rope</hypothesis>
      <offstage_function>Establishes the permanent, new status quo for humanity after the central conflict is resolved, acting as background pressure shaping their future development.</offstage_function>
    </constraint>
    <constraint id="C5" name="The cosmic debt of atonement">
      <hypothesis>Tangled Rope</hypothesis>
      <offstage_function>Serves as a narrative device for the story's resolution, a final moral accounting that is transactional rather than a persistent, systemic constraint.</offstage_function>
    </constraint>
    <constraint id="C6" name="Leverage from a secret prophecy">
      <hypothesis>Rope</hypothesis>
      <offstage_function>Functions as a key to unlock a plot stalemate, representing temporary informational power rather than a durable structural feature of the world.</offstage_function>
    </constraint>
  </deferred>
  <generation_sequence>C1 → C2 → C3</generation_sequence>
  <invariant_contract>
    <untranslatable_real present="yes" primary="no">A capacity for self-directed growth, once granted, cannot be revoked or controlled by the grantor, as its nature is to generate futures the grantor cannot predict.</untranslatable_real>
    <missing_floor present="yes" primary="yes">The right to rule is assumed to belong to the current holder of power, obscuring that this position was seized by force and not derived from a shared principle.</missing_floor>
    <inherent_instrument value="no">The primary extractions are direct applications of power or consequences of a changed state, not mediated by a formal measurement system.</inherent_instrument>
  </invariant_contract>
  <break_contract>
    <original_break>The audience expects to sympathize with the established authority punishing a transgressor, but instead finds themselves aligned with the transgressor against a tyrannical authority.</original_break>
    <prior_status>DEAD</prior_status>
    <target_prior>A rebellion against an unjust system is expected to culminate in the system's destruction; instead, the rebellion is assimilated, forcing a compromise that ultimately preserves the system.</target_prior>
  </break_contract>
  <omegas>
    <omega id="O1">The analysis assumes the final reconciliation is stable. It is unresolved whether the compromise fundamentally alters the nature of C1 or merely forestalls the next conflict.</omega>
  </omegas>
</constraint_manifest>
```