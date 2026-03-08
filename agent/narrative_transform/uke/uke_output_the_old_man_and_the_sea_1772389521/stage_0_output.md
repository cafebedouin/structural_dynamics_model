```xml
<constraint_manifest>
  <selected>
    <constraint id="C1" name="The Physical Contest" generation_order="1">
      <base_properties>
        <epsilon>0.0</epsilon>
        <suppression>0.0</suppression>
        <coordination>false</coordination>
        <asymmetric>true</asymmetric>
      </base_properties>
      <graph>
        <downstream_of>none</downstream_of>
        <feeds_into>C2, C3</feeds_into>
      </graph>
      <character_experiences>
        <character name="Santiago">
          <index>
            <power>moderate</power>
            <time>immediate</time>
            <exit>trapped</exit>
            <scope>universal</scope>
          </index>
          <chi>0.0</chi>
          <type>Mountain</type>
          <experience>The strength of the fish, the pain in his hands, the limits of his own endurance, and the nature of the sea are the unchangeable rules of the world he operates in.</experience>
        </character>
        <character name="The Marlin">
          <index>
            <power>powerful</power>
            <time>immediate</time>
            <exit>trapped</exit>
            <scope>universal</scope>
          </index>
          <chi>0.0</chi>
          <type>Mountain</type>
          <experience>Its own immense power, the hook in its mouth, and the unyielding pull of the line are the physical realities that define its struggle for survival.</experience>
        </character>
      </character_experiences>
      <indexical_variance>None. As a representation of natural law, this constraint functions as a Mountain for all biological entities involved, establishing the fundamental, unchangeable terrain of the conflict.</indexical_variance>
      <selection_reason>This constraint is the foundational, physical reality upon which all the social and personal dramas are built. It provides the non-negotiable backdrop for the entire narrative.</selection_reason>
    </constraint>
    <constraint id="C2" name="The Label of Unluck" generation_order="2">
      <base_properties>
        <epsilon>0.70</epsilon>
        <suppression>0.50</suppression>
        <coordination>true</coordination>
        <asymmetric>true</asymmetric>
      </base_properties>
      <graph>
        <downstream_of>none</downstream_of>
        <feeds_into>C3</feeds_into>
      </graph>
      <character_experiences>
        <character name="Santiago">
          <index>
            <power>powerless</power>
            <time>immediate</time>
            <exit>trapped</exit>
            <scope>local</scope>
          </index>
          <chi>0.84</chi>
          <type>Snare</type>
          <experience>He is socially and economically isolated by a communal judgment that he cannot disprove without a catch, costing him his apprentice and his standing.</experience>
        </character>
        <character name="Manolin's Parents">
          <index>
            <power>powerful</power>
            <time>biographical</time>
            <exit>mobile</exit>
            <scope>local</scope>
          </index>
          <chi>0.34</chi>
          <type>Rope</type>
          <experience>This is a sensible community norm that allows them to direct their son's labor toward a more productive boat, ensuring his future and the family's income.</experience>
        </character>
      </character_experiences>
      <indexical_variance>High. The same social belief system is an oppressive trap for the man it targets (Santiago) but a rational tool for managing labor and risk for those who enforce it (the parents).</indexical_variance>
      <selection_reason>This constraint provides the primary social pressure and inciting incident for the story, demonstrating sharp indexical variance between those subject to the label and those who use it.</selection_reason>
    </constraint>
    <constraint id="C3" name="A Boy's Duty" generation_order="3">
      <base_properties>
        <epsilon>0.60</epsilon>
        <suppression>0.40</suppression>
        <coordination>true</coordination>
        <asymmetric>true</asymmetric>
      </base_properties>
      <graph>
        <downstream_of>C2</downstream_of>
        <feeds_into>none</feeds_into>
      </graph>
      <character_experiences>
        <character name="Manolin">
          <index>
            <power>powerless</power>
            <time>biographical</time>
            <exit>constrained</exit>
            <scope>local</scope>
          </index>
          <chi>0.72</chi>
          <type>Tangled Rope</type>
          <experience>He is caught between his love and loyalty to the old man and the non-negotiable requirement to obey his father, a conflict which causes him sadness but which he accepts as normal.</experience>
        </character>
        <character name="Santiago">
          <index>
            <power>powerless</power>
            <time>biographical</time>
            <exit>trapped</exit>
            <scope>local</scope>
          </index>
          <chi>0.72</chi>
          <type>Snare</type>
          <experience>The boy's obligation to his father is a social rule that he cannot contest, which directly results in the loss of his helper and companion at sea.</experience>
        </character>
      </character_experiences>
      <indexical_variance>High. The boy experiences the constraint as a painful but legitimate conflict of duties (Tangled Rope). For the old man, who only feels the negative consequences, it is a pure trap that enforces his isolation (Snare).</indexical_variance>
      <selection_reason>Selected for its high centrality and its rich dramatic function as a Tangled Rope, creating an irresolvable tension between love and social structure that defines the central human relationship.</selection_reason>
    </constraint>
  </selected>
  <deferred>
    <constraint id="C4"