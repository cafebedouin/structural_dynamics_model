```xml
<constraint_manifest>
  <selected>
    <constraint id="C1" name="Toys as Disposable Property" generation_order="1">
      <base_properties>
        <epsilon>0.95</epsilon>
        <suppression>0.10</suppression>
        <coordination>false</coordination>
        <asymmetric>true</asymmetric>
      </base_properties>
      <graph>
        <downstream_of>none</downstream_of>
        <feeds_into>C2, C3</feeds_into>
      </graph>
      <character_classifications>
        <character name="Velveteen Rabbit">
          <index>
            <power>powerless</power>
            <time>biographical</time>
            <exit>trapped</exit>
            <scope>local</scope>
          </index>
          <chi>1.14</chi>
          <type>Snare</type>
        </character>
        <character name="Boy">
          <index>
            <power>powerful</power>
            <time>biographical</time>
            <exit>mobile</exit>
            <scope>local</scope>
          </index>
          <chi>0.456</chi>
          <type>Tangled Rope</type>
        </character>
      </character_classifications>
      <indexical_variance>The fundamental status of being property is a Snare for the toy (total contingent existence) but a Tangled Rope for the owner (provides comfort and utility at the cost of the object's autonomy).</indexical_variance>
      <selection_reason>This is the foundational, upstream constraint that enables all subsequent dynamics. The Rabbit's journey is only possible because he is an object owned by another.</selection_reason>
    </constraint>
    <constraint id="C2" name="Becoming Real Through Being Worn Out" generation_order="2">
      <base_properties>
        <epsilon>0.80</epsilon>
        <suppression>0.00</suppression>
        <coordination>true</coordination>
        <asymmetric>true</asymmetric>
      </base_properties>
      <graph>
        <downstream_of>C1</downstream_of>
        <feeds_into>C3</feeds_into>
      </graph>
      <character_classifications>
        <character name="Velveteen Rabbit">
          <index>
            <power>powerless</power>
            <time>biographical</time>
            <exit>identity_locked</exit>
            <scope>local</scope>
          </index>
          <chi>0.96</chi>
          <type>Snare</type>
        </character>
        <character name="Skin Horse">
          <index>
            <power>analytical</power>
            <time>generational</time>
            <exit>identity_locked</exit>
            <scope>local</scope>
          </index>
          <chi>0.736</chi>
          <type>Snare</type>
        </character>
      </character_classifications>
      <indexical_variance>This process is a Snare for those subject to it, regardless of their understanding. Both the naive Rabbit and the wise Skin Horse recognize its high extractive cost (physical destruction) in exchange for ontological status.</indexical_variance>
      <selection_reason>This is the central engine of the narrative's tragic beauty, defining the costly transformation the protagonist seeks. It directly links the story's concept of love to physical decay.</selection_reason>
    </constraint>
    <constraint id="C3" name="Hygiene Rules Mandate Destruction of Contaminated Items" generation_order="3">
      <base_properties>
        <epsilon>0.90</epsilon>
        <suppression>0.80</suppression>
        <coordination>false</coordination>
        <asymmetric>true</asymmetric>
      </base_properties>
      <graph>
        <downstream_of>C1, C2</downstream_of>
        <feeds_into>none</feeds_into>
      </graph>
      <character_classifications>
        <character name="Velveteen Rabbit">
          <index>
            <power>powerless</power>
            <time>immediate</time>
            <exit>trapped</exit>
            <scope>local</scope>
          </index>
          <chi>1.08</chi>
          <type>Snare</type>
        </character>
        <character name="Doctor">
          <index>
            <power>institutional</power>
            <time>biographical</time>
            <exit>mobile</exit>
            <scope>national</scope>
          </index>
          <chi>-0.18</chi>
          <type>Rope</type>
        </character>
      </character_classifications>
      <indexical_variance>From the perspective of the powerless toy, the doctor's order is an absolute Snare leading to annihilation. From the doctor's institutional perspective, it is a standard, beneficial Rope for coordinating public health and ensuring a child's safety.</indexical_variance>
      <selection_reason>This constraint represents the collision of the nursery's magical logic with the outside world's scientific logic, serving as the story's climactic threat and demonstrating the ultimate powerlessness of toys against institutional authority.</selection_reason>
    </constraint>
  </selected>
  <deferred>
    <constraint id="C4" name="Nursery Social Status Hierarchy">
      <hypothesis>Tangled Rope/Snare</hypothesis>
      <offstage_function>This provides the initial motivation for the protagonist's desire to become "Real," framing it as an escape from low social status.</offstage_function>
    </constraint>
    <constraint id="C5" name="Biological Standard of Authenticity">
      <hypothesis>Tangled Rope</hypothesis>
      <offstage_function>This constraint challenges the protagonist's achieved status, demonstrating its locality and fragility, which in turn necessitates the final supernatural transformation.</offstage_function>
    </constraint>
    <constraint id="C6" name="A Toy's Physical Construction Defines Its Limits">
      <hypothesis>Mountain</hypothesis>
      <offstage_function>This acts as a background physical law, explaining the protagonist's limitations and underpinning the social judgments made against him.</offstage_function>
    </constraint>
  </deferred>
  <generation_sequence>C1 → C2 → C3</generation_sequence>
  <omegas>
    <omega id="fairy_logic">The intervention by the Nursery Magic Fairy is a deus ex machina that breaks the logic of the established constraints. Is she an agent of a higher-level constraint (e.g., "True Realness transcends love and decay") or a rupture of the system entirely?</omega>
    <omega id="boy_awareness">The narrative is silent on the Boy's long-term awareness or memory of his original toy. The system's success in replacing the "contaminated" object relies on his apparent indifference, but the depth of this indifference is an unresolved variable.</omega>
  </omegas>
</constraint_manifest>
```