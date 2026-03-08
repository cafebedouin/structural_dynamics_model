```xml
<constraint_manifest>
  <selected>
    <constraint id="C1" name="Code of Social Honor" generation_order="1">
      <base_properties>
        <epsilon>0.70</epsilon>
        <suppression>0.40</suppression>
        <coordination>true</coordination>
        <asymmetric>true</asymmetric>
      </base_properties>
      <graph>
        <downstream_of>none</downstream_of>
        <feeds_into>C2</feeds_into>
      </graph>
      <character_classifications>
        <character name="Peyton Farquhar">
          <index>
            <power>moderate</power>
            <time>biographical</time>
            <exit>identity_locked</exit>
            <scope>regional</scope>
          </index>
          <chi>0.63</chi>
          <type>Tangled Rope</type>
        </character>
      </character_classifications>
      <indexical_variance>Low. This constraint primarily affects individuals who have internalized a specific social code, leading to similar classifications among them.</indexical_variance>
      <selection_reason>Highest centrality score (4). It is the primary upstream driver of the central character's actions, making his behavior intelligible and his vulnerability exploitable.</selection_reason>
    </constraint>
    <constraint id="C2" name="Deceptive Military Entrapment" generation_order="2">
      <base_properties>
        <epsilon>0.90</epsilon>
        <suppression>0.70</suppression>
        <coordination>true</coordination>
        <asymmetric>true</asymmetric>
      </base_properties>
      <graph>
        <downstream_of>C1</downstream_of>
        <feeds_into>C3</feeds_into>
      </graph>
      <character_classifications>
        <character name="Peyton Farquhar">
          <index>
            <power>powerless</power>
            <time>immediate</time>
            <exit>trapped</exit>
            <scope>local</scope>
          </index>
          <chi>1.08</chi>
          <type>Snare</type>
        </character>
        <character name="Federal Scout">
          <index>
            <power>powerful</power>
            <time>biographical</time>
            <exit>arbitrage</exit>
            <scope>regional</scope>
          </index>
          <chi>0.486</chi>
          <type>Tangled Rope</type>
        </character>
      </character_classifications>
      <indexical_variance>High. The same system of deception is experienced as a fatal trap by its target (Snare) but as a functional, if extractive, tool by its operator (Tangled Rope).</indexical_variance>
      <selection_reason>High centrality score (4). It serves as the causal link between the character's internal motivation (C1) and his external fate (C3), demonstrating significant indexical variance.</selection_reason>
    </constraint>
    <constraint id="C3" name="Summary Military Justice" generation_order="3">
      <base_properties>
        <epsilon>1.00</epsilon>
        <suppression>1.00</suppression>
        <coordination>true</coordination>
        <asymmetric>true</asymmetric>
      </base_properties>
      <graph>
        <downstream_of>C2</downstream_of>
        <feeds_into>none</feeds_into>
      </graph>
      <character_classifications>
        <character name="Peyton Farquhar">
          <index>
            <power>powerless</power>
            <time>immediate</time>
            <exit>trapped</exit>
            <scope>local</scope>
          </index>
          <chi>1.20</chi>
          <type>Snare</type>
        </character>
        <character name="Captain">
          <index>
            <power>institutional</power>
            <time>biographical</time>
            <exit>mobile</exit>
            <scope>national</scope>
          </index>
          <chi>-0.20</chi>
          <type>Rope</type>
        </character>
      </character_classifications>
      <indexical_variance>High. The system that executes one man (Snare) is experienced as a routine, coordinating procedure by the institutional agent administering it (Rope).</indexical_variance>
      <selection_reason>High centrality score (3). It is the terminal, downstream constraint that resolves the narrative's central tension, and it exhibits extreme indexical variance between its target and its agents.</selection_reason>
    </constraint>
  </selected>
  <deferred>
    <constraint id="C4" name="Ideological Imperative">
      <hypothesis>Snare</hypothesis>
      <offstage_function>Provides the political justification for the Code of Social Honor (C1), making the character's desire for distinction feel like a noble and necessary cause.</offstage_function>
    </constraint>
    <constraint id="C5" name="Physics of Hanging">
      <hypothesis>Mountain</hypothesis>
      <offstage_function>Acts as the immutable physical reality that the military justice system (C3) leverages and which ultimately overrides the character's subjective experience of escape.</offstage_function>
    </constraint>
    <constraint id="C6" name="State of War">
      <hypothesis>Tangled Rope</hypothesis>
      <offstage_function>Serves as the master context that authorizes all other constraints, suspending civilian norms and making both deceptive entrapment (C2) and summary justice (C3) permissible.</offstage_function>
    </constraint>
  </deferred>
  <generation_sequence>C1 → C2 → C3</generation_sequence>
  <omegas>
    <omega id="imperious_circumstances">The source mentions "circumstances of an imperious nature" prevented the character from enlisting. The specific nature of this prior constraint is unknown but shapes his motivation for C1.</omega>
    <omega id="scout_authorization">The degree to which the Federal Scout's entrapment (C2) was a sanctioned, systematic tactic versus an opportunistic field decision is unresolved.</omega>
  </omegas>
</constraint_manifest>
```