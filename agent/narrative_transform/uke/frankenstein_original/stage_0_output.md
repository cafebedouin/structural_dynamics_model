```xml
<constraint_manifest>
  <selected>
    <constraint id="C1" name="Obsessive Pursuit of Glory" generation_order="1">
      <base_properties>
        <epsilon>0.70</epsilon>
        <suppression>0.20</suppression>
        <coordination>false</coordination>
        <asymmetric>true</asymmetric>
      </base_properties>
      <graph>
        <downstream_of>none</downstream_of>
        <feeds_into>C2</feeds_into>
      </graph>
      <character_classifications>
        <character name="Victor Frankenstein">
          <index>
            <power>moderate</power>
            <time>biographical</time>
            <exit>identity_locked</exit>
            <scope>local</scope>
          </index>
          <chi>0.56</chi>
          <type>Tangled Rope</type>
        </character>
        <character name="Robert Walton">
          <index>
            <power>moderate</power>
            <time>biographical</time>
            <exit>constrained</exit>
            <scope>regional</scope>
          </index>
          <chi>0.63</chi>
          <type>Tangled Rope</type>
        </character>
      </character_classifications>
      <indexical_variance>Minimal. For both primary characters driven by it, the constraint is a Tangled Rope, providing the abstract 'good' of knowledge or glory while extracting health, safety, and social connection. The higher chi for Walton reflects the greater scope and risk of his expedition.</indexical_variance>
      <selection_reason>Upstream cause of the central conflict. It is the personal drive that initiates the creation event and isolates the protagonist from the moderating influence of his community.</selection_reason>
    </constraint>
    <constraint id="C2" name="The Creator's Duty" generation_order="2">
      <base_properties>
        <epsilon>0.80</epsilon>
        <suppression>0.90</suppression>
        <coordination>true</coordination>
        <asymmetric>true</asymmetric>
      </base_properties>
      <