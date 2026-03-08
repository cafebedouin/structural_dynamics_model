```xml
<constraint_manifest>
  <selected>
    <constraint id="C1" name="Fluid Reality" generation_order="1">
      <base_properties>
        <epsilon>0.20</epsilon>
        <suppression>0.00</suppression>
        <coordination>false</coordination>
        <asymmetric>true</asymmetric>
      </base_properties>
      <graph>
        <downstream_of>none</downstream_of>
        <feeds_into>C2, C3</feeds_into>
      </graph>
      <character_classifications>
        <character name="Alice">
          <index>
            <power>powerless</power>
            <time>immediate</time>
            <exit>trapped</exit>
            <scope>local</scope>
          </index>
          <chi>0.24</chi>
          <type>Mountain</type>
        </character>
        <character name="Cheshire Cat">
          <index>
            <power>analytical</power>
            <time>biographical</time>
            <exit>arbitrage</exit>
            <scope>local</scope>
          </index>
          <chi>0.184</chi>
          <type>Mountain</type>
        </character>
      </character_classifications>
      <indexical_variance>None. As a fundamental property of the environment, this constraint presents as unchangeable terrain to all characters within it, regardless of their power or perspective.</indexical_variance>
      <selection_reason>This is the foundational, upstream constraint that enables all others. The breakdown of consistent physics and causality is the terrain upon which nonsensical social rules and arbitrary power can be built.</selection_reason>
    </constraint>
    <constraint id="C2" name="Insular Nonsense" generation_order="2">
      <base_properties>
        <epsilon>0.50</epsilon>
        <suppression>0.40</suppression>
        <coordination>true</coordination>
        <asymmetric>true</asymmetric>
      </base_properties>
      <graph>
        <downstream_of>C1</downstream_of>
        <feeds_into>none</feeds_into>
      </graph>
      <character_classifications>
        <character name="Alice">
          <index>
            <power>powerless</power>
            <time>immediate</time>
            <exit>constrained</exit>
            <scope>local</scope>
          </index>
          <chi>0.60</chi>
          <type>Tangled Rope</type>
        </character>
        <character name="The Dodo">
          <index>
            <power>powerful</power>
            <time>immediate</time>
            <exit>mobile</exit>
            <scope>local</scope>
          </index>
          <chi>0.24</chi>
          <type>Rope</type>
        </character>
        <character name="The Hatter">
          <index>
            <power>moderate</power>
            <time>biographical</time>
            <exit>identity_locked</exit>
            <scope>local</scope>
          </index>
          <chi>0.40</chi>
          <type>Rope</type>
        </character>
      </character_classifications>
      <indexical_variance>Significant. For insiders like the Dodo or Hatter, it is a functional (if strange) coordination mechanism (Rope). For an outsider like Alice, it is a hybrid system that coordinates insiders while extracting her ability to participate (Tangled Rope).</indexical_variance>
      <selection_reason>Selected as the highest-centrality constraint. It represents the primary social logic of Wonderland, where shared understanding is replaced by performative, exclusionary rituals that are impossible for outsiders to navigate.</selection_reason>
    </constraint>
    <constraint id="C3" name="Capricious Condemnation" generation_order="3">
      <base_properties>
        <epsilon>0.90</epsilon>
        <suppression>0.90</suppression>
        <coordination>true</coordination>
        <asymmetric>true</asymmetric>
      </base_properties>
      <graph>
        <downstream_of>C1</downstream_of>
        <feeds_into>none</feeds_into>
      </graph>
      <character_classifications>
        <character name="Alice">
          <index>
            <power>powerless</power>
            <time>immediate</time>
            <exit>trapped</exit>
            <scope>local</scope>
          </index>
          <chi>1.08</chi>
          <type>Snare</type>
        </character>
        <character name="The Queen of Hearts">
          <index>
            <power>institutional</power>
            <time>immediate</time>
            <exit>arbitrage</exit>
            <scope>local</scope>
          </index>
          <chi>-0.144</chi>
          <type>Rope</type>
        </character>
        <character name="The Gardeners">
          <index>
            <power>powerless</power>
            <time>immediate</time>
            <exit>trapped</exit>
            <scope>local</scope>
          </index>
          <chi>1.08</chi>
          <type>Snare</type>
        </character>
      </character_classifications>
      <indexical_variance>Extreme. For the Queen, this is a pure instrument of control and coordination (Rope). For her subjects, it is a lethal extraction trap where any action can lead to a death sentence (Snare).</indexical_variance>
      <selection_reason>Selected as the next-highest centrality constraint with a different observable (direct physical threat vs. social exclusion) and a clear beneficiary/victim dynamic distinct from C2.</selection_reason>
    </constraint>
  </selected>

  <deferred>
    <constraint id="C4" name="Unstable Physicality">
      <hypothesis>Snare</hypothesis>
      <offstage_function>Provides the constant, visceral feeling of disorientation and powerlessness that makes the protagonist vulnerable to the selected social and political constraints.</offstage_function>
    </constraint>
    <constraint id="C5" name="Eroded Identity">
      <hypothesis>Snare</hypothesis>
      <offstage_function>Represents the internal, psychological stakes of navigating the selected constraints; failure to adapt leads to a loss of self.</offstage_function>
    </constraint>
  </deferred>

  <generation_sequence>C1 → C2 → C3</generation_sequence>

  <omegas>
    <omega id="consequence_ambiguity">The analysis assumes the Queen's threats are performative but still extract fear. If the executions were real, the ε of C3 would be 1.0 and the system would be a pure, unsustainable tyranny rather than a bizarrely stable one.</omega>
    <omega id="dream_boundary">The analysis classifies C1 as a Mountain from within the dream's frame. The nature of the boundary between the dream and reality, and whether it can be crossed by choice, remains an unresolved structural question.</omega>
  </omegas>
</constraint_manifest>
```