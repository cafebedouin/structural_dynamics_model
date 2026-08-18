```xml
<constraint_manifest>
  <selected>
    <constraint id="C1" name="Medical Ignorance" generation_order="1">
      <base_properties>
        <epsilon>0.10</epsilon>
        <suppression>0.00</suppression>
        <coordination>false</coordination>
        <asymmetric>false</asymmetric>
      </base_properties>
      <graph>
        <downstream_of>none</downstream_of>
        <feeds_into>C3</feeds_into>
      </graph>
      <character_classifications>
        <character name="Alicia">
          <index>
            <power>powerless</power>
            <time>immediate</time>
            <exit>trapped</exit>
            <scope>local</scope>
          </index>
          <chi>0.12</chi>
          <type>Mountain</type>
        </character>
        <character name="Jordán">
          <index>
            <power>moderate</power>
            <time>immediate</time>
            <exit>constrained</exit>
            <scope>local</scope>
          </index>
          <chi>0.08</chi>
          <type>Mountain</type>
        </character>
        <character name="Doctors">
          <index>
            <power>analytical</power>
            <time>biographical</time>
            <exit>analytical</exit>
            <scope>regional</scope>
          </index>
          <chi>0.10</chi>
          <type>Mountain</type>
        </character>
      </character_classifications>
      <indexical_variance>false</indexical_variance>
      <selection_reason>Structurally distinct upstream dependency (Mountain, low ε) that enables the primary threat by preventing rational intervention.</selection_reason>
    </constraint>
    <constraint id="C2" name="Gendered Passivity" generation_order="2">
      <base_properties>
        <epsilon>0.60</epsilon>
        <suppression>0.60</suppression>
        <coordination>true</coordination>
        <asymmetric>true</asymmetric>
      </base_properties>
      <graph>
        <downstream_of>none</downstream_of>
        <feeds_into>C3</feeds_into>
      </graph>
      <character_classifications>
        <character name="Alicia">
          <index>
            <power>powerless</power>
            <time>biographical</time>
            <exit>identity_locked</exit>
            <scope>local</scope>
          </index>
          <chi>0.72</chi>
          <type>Snare</type>
        </character>
        <character name="Jordán">
          <index>
            <power>institutional</power>
            <time>biographical</time>
            <exit>arbitrage</exit>
            <scope>local</scope>
          </index>
          <chi>-0.10</chi>
          <type>Rope</type>
        </character>
      </character_classifications>
      <indexical_variance>true</indexical_variance>
      <selection_reason>High-centrality upstream constraint that creates the conditions of isolation and helplessness necessary for the terminal constraint to function.</selection_reason>
    </constraint>
    <constraint id="C3" name="Hidden Predator" generation_order="3">
      <base_properties>
        <epsilon>1.00</epsilon>
        <suppression>0.90</suppression>
        <coordination>false</coordination>
        <asymmetric>true</asymmetric>
      </base_properties>
      <graph>
        <downstream_of>C1, C2</downstream_of>
        <feeds_into>none</feeds_into>
      </graph>
      <character_classifications>
        <character name="Alicia">
          <index>
            <power>powerless</power>
            <time>immediate</time>
            <exit>trapped</exit>
            <scope>local</scope>
          </index>
          <chi>1.20</chi>
          <type>Snare</type>
        </character>
        <character name="Jordán">
          <index>
            <power>powerless</power>
            <time>immediate</time>
            <exit>trapped</exit>
            <scope>local</scope>
          </index>
          <chi>1.20</chi>
          <type>Snare</type>
        </character>
        <character name="Servant">
          <index>
            <power>powerless</power>
            <time>immediate</time>
            <exit>trapped</exit>
            <scope>local</scope>
          </index>
          <chi>1.20</chi>
          <type>Snare</type>
        </character>
      </character_classifications>
      <indexical_variance>false</indexical_variance>
      <selection_reason>Highest-centrality constraint; it is the terminal, downstream effect of all other constraints and the story's central horror.</selection_reason>
    </constraint>
  </selected>
  <deferred>
    <constraint id="C4" name="Emotional Repression">
      <hypothesis>Tangled Rope</hypothesis>
      <offstage_function>This provides the mechanism for communication failure, preventing the victim from articulating her fear and the caregiver from offering comfort, thus deepening the isolation.</offstage_function>
    </constraint>
    <constraint id="C5" name="Environmental Oppression">
      <hypothesis>Mountain</hypothesis>
      <offstage_function>This sets the atmospheric tone of the story, creating a physical environment that mirrors the emotional coldness and is a plausible habitat for a hidden monster.</offstage_function>
    </constraint>
    <constraint id="C6" name="Social Isolation">
      <hypothesis>Tangled Rope</hypothesis>
      <offstage_function>This explains the absence of outside help (family, friends), ensuring the central conflict remains contained within the isolated domestic unit until it is too late.</offstage_function>
    </constraint>
  </deferred>
  <generation_sequence>C1 → C2 → C3</generation_sequence>
  <invariant_contract>
    <untranslatable_real present="no" primary="no">absent</untranslatable_real>
    <missing_floor present="yes" primary="yes">The structure of intimate partnership presupposes that the shared domestic space is a sanctuary, a choice that erases the possibility that the space itself, or what it contains, is the primary threat.</missing_floor>
    <inherent_instrument value="no">The extraction is direct physical predation; while an object is used as a lair, the constraint is the predator, not a system of measurement.</inherent_instrument>
  </invariant_contract>
  <break_contract>
    <original_break>A story about a woman's mysterious illness will be explained by psychological or mundane physical causes.</original_break>
    <prior_status>DEAD</prior_status>
    <target_prior>A story about an oppressive intimate relationship is a metaphor for symbolic or emotional violence.</target_prior>
  </break_contract>
  <omegas>
    <omega id="power_scaling">The 'Hidden Predator' constraint renders all characters equally powerless through ignorance, collapsing the social power distinctions relevant to other constraints.</omega>
  </omegas>
</constraint_manifest>
```