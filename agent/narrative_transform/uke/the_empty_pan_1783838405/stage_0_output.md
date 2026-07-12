```xml
<constraint_manifest>
  <selected>
    <constraint id="C1" name="Truth Corrupted by Possession" generation_order="1">
      <base_properties>
        <epsilon>0.0</epsilon>
        <suppression>0.0</suppression>
        <coordination>false</coordination>
        <asymmetric>false</asymmetric>
      </base_properties>
      <graph>
        <downstream_of>none</downstream_of>
        <feeds_into>none</feeds_into>
      </graph>
      <character_classifications>
        <character name="Narrator">
          <index>
            <power>powerless</power>
            <time>biographical</time>
            <exit>identity_locked</exit>
            <scope>universal</scope>
          </index>
          <chi>0.0</chi>
          <type>Mountain</type>
        </character>
        <character name="Verrel">
          <index>
            <power>analytical</power>
            <time>biographical</time>
            <exit>identity_locked</exit>
            <scope>universal</scope>
          </index>
          <chi>0.0</chi>
          <type>Mountain</type>
        </character>
      </character_classifications>
      <indexical_variance>false</indexical_variance>
      <selection_reason>This constraint represents a fundamental law of the story's world, acting as the immutable background condition that makes the other, constructed constraints necessary. It is the most upstream constraint.</selection_reason>
    </constraint>
    <constraint id="C2" name="Obscured Foundational Choice" generation_order="2">
      <base_properties>
        <epsilon>0.5</epsilon>
        <suppression>0.7</suppression>
        <coordination>true</coordination>
        <asymmetric>true</asymmetric>
      </base_properties>
      <graph>
        <downstream_of>none</downstream_of>
        <feeds_into>C3</feeds_into>
      </graph>
      <character_classifications>
        <character name="Narrator">
          <index>
            <power>powerless</power>
            <time>biographical</time>
            <exit>trapped</exit>
            <scope>local</scope>
          </index>
          <chi>0.60</chi>
          <type>Tangled Rope</type>
        </character>
        <character name="Verrel">
          <index>
            <power>analytical</power>
            <time>biographical</time>
            <exit>identity_locked</exit>
            <scope>local</scope>
          </index>
          <chi>0.46</chi>
          <type>Tangled Rope</type>
        </character>
      </character_classifications>
      <indexical_variance>false</indexical_variance>
      <selection_reason>Structurally upstream of the primary conflict, this constraint reveals the hidden choice that enables the system's injustice. Its Tangled Rope classification captures the necessary function (a zero point is needed) and the inherent corruption (the choice is hidden).</selection_reason>
    </constraint>
    <constraint id="C3" name="The Sanctioned Standard as Extractive Instrument" generation_order="3">
      <base_properties>
        <epsilon>0.8</epsilon>
        <suppression>0.9</suppression>
        <coordination>true</coordination>
        <asymmetric>true</asymmetric>
      </base_properties>
      <graph>
        <downstream_of>C2</downstream_of>
        <feeds_into>none</feeds_into>
      </graph>
      <character_classifications>
        <character name="Narrator">
          <index>
            <power>powerless</power>
            <time>biographical</time>
            <exit>trapped</exit>
            <scope>local</scope>
          </index>
          <chi>0.96</chi>
          <type>Snare</type>
        </character>
        <character name="Verrel">
          <index>
            <power>analytical</power>
            <time>biographical</time>
            <exit>identity_locked</exit>
            <scope>local</scope>
          </index>
          <chi>0.74</chi>
          <type>Snare</type>
        </character>
        <character name="King">
          <index>
            <power>institutional</power>
            <time>generational</time>
            <exit>arbitrage</exit>
            <scope>national</scope>
          </index>
          <chi>-0.16</chi>
          <type>Rope</type>
        </character>
      </character_classifications>
      <indexical_variance>true</indexical_variance>
      <selection_reason>This is the central, most visible injustice in the narrative, with the highest centrality score. Its indexical variance (Snare for the powerless, Rope for the institutional beneficiary) is critical to the story's structure.</selection_reason