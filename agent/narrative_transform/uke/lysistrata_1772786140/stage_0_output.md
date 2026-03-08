```xml
<constraint_manifest>
  <selected>
    <constraint id="C1" name="biological_imperative" generation_order="1">
      <base_properties>
        <epsilon>0.0</epsilon>
        <suppression>0.0</suppression>
        <coordination>false</coordination>
        <asymmetric>false</asymmetric>
      </base_properties>
      <graph>
        <downstream_of>none</downstream_of>
        <feeds_into>C3</feeds_into>
      </graph>
      <character_classifications>
        <character name="Myrrhine">
          <index>
            <power>powerless</power>
            <time>immediate</time>
            <exit>trapped</exit>
            <scope>national</scope>
          </index>
          <chi>0.0</chi>
          <type>Mountain</type>
        </character>
        <character name="Cinesias">
          <index>
            <power>powerless</power>
            <time>immediate</time>
            <exit>trapped</exit>
            <scope>national</scope>
          </index>
          <chi>0.0</chi>
          <type>Mountain</type>
        </character>
        <character name="Lysistrata">
          <index>
            <power>analytical</power>
            <time>biographical</time>
            <exit>analytical</exit>
            <scope>national</scope>
          </index>
          <chi>0.0</chi>
          <type>Mountain</type>
        </character>
      </character_classifications>
      <indexical_variance>None. As a natural law with zero base extractiveness, its classification is stable across all indices.</indexical_variance>
      <selection_reason>It is the unchangeable natural law that provides the leverage for the primary strategic action, acting as the terrain for the conflict.</selection_reason>
    </constraint>
    <constraint id="C2" name="gendered_spheres" generation_order="2">
      <base_properties>
        <epsilon>0.8</epsilon>
        <suppression>0.7</suppression>
        <coordination>true</coordination>
        <asymmetric>true</asymmetric>
      </base_properties>
      <graph>
        <downstream_of>none</downstream_of>
        <feeds_into>C3</feeds_into>
      </graph>
      <character_classifications>
        <character name="Myrrhine">
          <index>
            <power>powerless</power>
            <time>biographical</time>
            <exit>identity_locked</exit>
            <scope>national</scope>
          </index>
          <chi>1.20</chi>
          <type>Snare</type>
        </character>
        <character name="Magistrate">
          <index>
            <power>institutional</power>
            <time>biographical</time>
            <exit>arbitrage</exit>
            <scope>national</scope>
          </index>
          <chi>-0.16</chi>
          <type>Rope</type>
        </character>
      </character_classifications>
      <indexical_variance>Yes. The constraint is experienced as an oppressive trap by those in the domestic sphere (women) and as a beneficial organizing principle by those in the public sphere (institutional men).</indexical_variance>
      <selection_reason>It is the foundational social constraint that establishes the conflict's actors and defines their available tools, making the central strategic action both possible and necessary.</selection_reason>
    </constraint>
    <constraint id="C3" name="intimacy_embargo" generation_order="3">
      <base_properties>
        <epsilon>0.75</epsilon>
        <suppression>0.9</suppression>
        <coordination>true</coordination>
        <asymmetric>true</asymmetric>
      </base_properties>
      <graph>
        <downstream_of>C1, C2</downstream_of>
        <feeds_into>none</feeds_into>
      </graph>
      <character_classifications>
        <character name="Cinesias">
          <index>
            <power>powerless</power>
            <time>immediate</time>
            <exit>trapped</exit>
            <scope>national</scope>
          </index>
          <chi>1.125</chi>
          <type>Snare</type>
        </character>
        <character name="Myrrhine">
          <index>
            <power>organized</power>
            <time>immediate</time>
            <exit>constrained</exit>
            <scope>national</scope>
          </index>
          <chi>0.30</chi>
          <type>Rope</type>
        </character>
      </character_classifications>
      <indexical_variance>Yes. The constraint is a tool of coordination for its enactors but an inescapable, high-extraction trap for its targets.</indexical_variance>
      <selection_reason>It is the central, active plot mechanism—a constructed constraint designed to dismantle another—and it demonstrates extreme indexical variance.</selection_reason>
    </constraint>
  </selected>
  <deferred>
    <constraint id="C4" name="state_mobilization_duty">
      <hypothesis>Tangled Rope</hypothesis>
      <offstage_function>Provides the primary motivation for the protagonists' actions and justifies the antagonists' resistance, creating the core conflict.</offstage_function>
    </constraint>
    <constraint id="C5" name="sacred_pact">
      <hypothesis>Rope</hypothesis>
      <offstage_function>Enforces internal cohesion among the protagonists, preventing the collapse of their collective action from internal pressures and temptations.</offstage_function>
    </constraint>
    <constraint id="C6" name="treasury_control">
      <hypothesis>Snare</hypothesis>
      <offstage_function>Acts as a parallel economic attack vector, ensuring the antagonists cannot win a war of attrition against the primary social strategy.</offstage_function>
    </constraint>
  </deferred>
  <generation_sequence>C1 → C2 → C3</generation_sequence>
  <omegas>
    <omega id="resolution_stability">The long-term stability of the negotiated peace is unresolved. It is unclear if the events have permanently altered the foundational social constraints or merely produced a temporary truce, after which the prior order will reassert itself.</omega>
  </omegas>
</constraint_manifest>
```