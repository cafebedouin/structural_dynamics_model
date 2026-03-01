```xml
<constraint_manifest>
  <selected>
    <constraint id="C1" name="Market_Imperative" generation_order="1">
      <base_properties>
        <epsilon>0.45</epsilon>
        <suppression>0.10</suppression>
        <coordination>true</coordination>
        <asymmetric>true</asymmetric>
      </base_properties>
      <graph>
        <downstream_of>none</downstream_of>
        <feeds_into>none</feeds_into>
      </graph>
      <character_experiences>
        <character name="Miller">
          <index>
            <power>moderate</power>
            <time>biographical</time>
            <exit>constrained</exit>
            <scope>local</scope>
          </index>
          <chi>0.36</chi>
          <type>Tangled Rope</type>
          <experience>To get a good price, I have to put in the work and bring my property to town in good condition, but the rules of the market feel like they work more for the buyers than for me.</experience>
        </character>
      </character_experiences>
      <indexical_variance>This constraint is the baseline goal. For the Miller, it's a standard mix of opportunity and effort. Other characters (like potential buyers, not present in the story) would experience it differently, but for the protagonist, it's the initial, understandable challenge.</indexical_variance>
      <selection_reason>This constraint establishes the protagonist's initial goal and the logical framework for his actions. It is the system that the central conflict (C2) actively undermines, creating the core tension of the narrative.</selection_reason>
    </constraint>
    <constraint id="C2" name="Public_Performance_Demand" generation_order="2">
      <base_properties>
        <epsilon>0.80</epsilon>
        <suppression>0.20</suppression>
        <coordination>false</coordination>
        <asymmetric>true</asymmetric>
      </base_properties>
      <graph>
        <downstream_of>none</downstream_of>
        <feeds_into>C3</feeds_into>
      </graph>
      <character_experiences>
        <character name="Miller">
          <index>
            <power>powerless</power>
            <time>immediate</time>
            <exit>trapped</exit>
            <scope>local</scope>
          </index>
          <chi>0.96</chi>
          <type>Snare</type>
          <experience>Every person I meet has a strong opinion on how I should be doing things, and their judgments are so sharp I feel I have no choice but to obey, even when they contradict each other. I am trapped by their eyes.</experience>
        </character>
        <character name="Observer">
          <index>
            <power>powerful</power>
            <time>immediate</time>
            <exit>arbitrage</exit>
            <scope>local</scope>
          </index>
          <chi>0.38</chi>
          <type>Rope</type>
          <experience>It's our duty to speak up when we see something that isn't right. A quick word helps keep the world in order and costs nothing.</experience>
        </character>
      </character_experiences>
      <indexical_variance>Extreme variance drives the narrative. The Miller experiences a destructive, inescapable trap (Snare) where he loses his agency and property. The observers who impose the constraint experience it as a zero-cost, functional way to enforce social norms (Rope). This gap is the engine of the tragedy.</indexical_variance>
      <selection_reason>This is the highest-centrality constraint. It is the primary antagonist force, a system-level Tangled Rope that generates the story's conflict by appearing as a Snare to its victim and a Rope to its enforcers. It directly causes the failure of C1's goal.</selection_reason>
    </constraint>
    <constraint id="C3" name="Material_Limits" generation_order="3">
      <base_properties>
        <epsilon>0.05</epsilon>
        <suppression>0.00</suppression>
        <coordination>false</coordination>
        <asymmetric>false</asymmetric>
      </base_properties>
      <graph>
        <downstream_of>C2</downstream_of>
        <feeds_into>none</feeds_into>
      </graph>
      <character_experiences>
        <character name="Ass">
          <index>
            <power>powerless</power>
            <time>immediate</time>
            <exit>trapped</exit>
            <scope>local</scope>
          </index>
          <chi>0.06</chi>
          <type>Mountain</type>
          <experience>Being bound, carried upside down over a noisy bridge is terrifying and unnatural; I must struggle to escape.</experience>
        </character>
        <character name="Miller">
          <index>
            <power>powerless</power>
            <time>immediate</time>
            <exit>trapped</exit>
            <scope>local</scope>
          </index>
          <chi>0.06</chi>
          <type>Mountain</type>
          <experience>There are some things you just can't do. An animal will only take so much before its nature takes over, and you can't argue with a river.</experience>
        </character>
      </character_experiences>
      <indexical_variance>This constraint is experienced as an unchangeable reality by all who encounter it. Unlike the social constraints, its effects are not dependent on power or perspective. It is the final, objective arbiter that cannot be pleased, ignored, or appeased.</indexical_variance>
      <selection_reason>This constraint serves as the story's terminal attractor. It is the unyielding physical reality that ultimately resolves the absurd social tensions created by C2. Its selection provides a non-negotiable endpoint to the Miller's attempts to navigate an impossible social landscape.</selection_reason>
    </constraint>
  </selected>
  <deferred>
    <constraint id="C4" name="Paternal_Duty_Norms">
      <hypothesis>Tangled Rope</hypothesis>
      <offstage_function>This provides a specific, emotionally resonant lever for C2 to operate. It makes the abstract pressure of public opinion concrete by framing it as a question of the Miller's competence as a father.</offstage_function>
    </constraint>
    <constraint id="C5" name="Public_Stage">
      <hypothesis>Mountain</hypothesis>
      <offstage_function>This constraint acts as the enabling medium for the entire conflict. The fact that the journey must happen on a public road ensures there is no escape from observation, making the Miller vulnerable to C2 at all times.</offstage_function>
    </constraint>
  </deferred>
  <generation_sequence>C1 → C2 → C3</generation_sequence>
  <omegas>
    <omega id="internal_compulsion">Is the Miller's failure caused entirely by external pressure, or does he have an internal psychological need for validation that makes him uniquely vulnerable to it? The text suggests the latter, but the degree is unresolvable.</omega>
  </omegas>
</constraint_manifest>
```