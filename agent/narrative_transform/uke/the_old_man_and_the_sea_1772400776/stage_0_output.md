```xml
<constraint_manifest>
  <selected>
    <constraint id="C1" name="The Body's Limits" generation_order="1">
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
      <character_experiences>
        <character name="Santiago">
          <index>
            <power>powerless</power>
            <time>biographical</time>
            <exit>trapped</exit>
            <scope>local</scope>
          </index>
          <chi>0.0</chi>
          <type>Mountain</type>
          <experience>His body is old; his hands cramp, his strength is finite, and his back hurts. This is an unchangeable fact of his existence that he must endure and work around with tricks and resolution.</experience>
        </character>
      </character_experiences>
      <indexical_variance>This constraint is a true Mountain; it does not scale with power and affects all characters who experience aging in the same fundamental way. The dramatic variance comes from how other constraints force a direct confrontation with it.</indexical_variance>
      <selection_reason>This is the foundational, unchangeable physical reality against which the social and economic dramas play out. It is the most upstream constraint.</selection_reason>
    </constraint>
    <constraint id="C2" name="The Tally of Luck" generation_order="2">
      <base_properties>
        <epsilon>0.70</epsilon>
        <suppression>0.40</suppression>
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
            <time>biographical</time>
            <exit>trapped</exit>
            <scope>local</scope>
          </index>
          <chi>0.84</chi>
          <type>Snare</type>
          <experience>He is considered 'salao,' the worst form of unlucky, which strips him of his apprentice and his standing in the community, forcing him into isolation.</experience>
        </character>
        <character name="Manolin">
          <index>
            <power>powerless</power>
            <time>biographical</time>
            <exit>constrained</exit>
            <scope>local</scope>
          </index>
          <chi>0.84</chi>
          <type>Snare</type>
          <experience>This belief system forces him to abandon the old man he loves for a 'lucky' boat, causing him sadness and guilt.</experience>
        </character>
        <character name="Boy's Parents">
          <index>
            <power>moderate</power>
            <time>generational</time>
            <exit>mobile</exit>
            <scope>local</scope>
          </index>
          <chi>0.56</chi>
          <type>Tangled Rope</type>
          <experience>It is a practical community rule for ensuring their son is on a boat that earns money, even though it goes against the boy's wishes.</experience>
        </character>
      </character_experiences>
      <indexical_variance>The same social belief is experienced as an oppressive trap (Snare) by the old man and the boy, but as a functional, if imperfect, tool for managing resources (Tangled Rope) by the parents who enforce it.</indexical_variance>
      <selection_reason>Selected as the highest-centrality constraint. It is the primary social engine of the narrative, creating the initial problem (the old man's isolation) and the motivation for his quest.</selection_reason>
    </constraint>
    <constraint id="C3" name="The Price of the Catch" generation_order="3">
      <base_properties>
        <epsilon>0.80</epsilon>
        <suppression>0.20</suppression>
        <coordination>true</coordination>
        <asymmetric>true</asymmetric>
      </base_properties>
      <graph>
        <downstream_of>C1, C2</downstream_of>
        <feeds_into>none</feeds_into>
      </graph>
      <character_experiences>
        <character name="Santiago">
          <index>
            <power>powerless</power>
            <time>biographical</time>
            <exit>trapped</exit>
            <scope>local</scope>
          </index>
          <chi>0.96</chi>
          <type>Snare</type>
          <experience>The immense value of his heroic labor and suffering is contingent on bringing the fish to market intact; if it is destroyed at sea, his effort is worth nothing and he is left only with the carcass.</experience>
        </character>
      </character_experiences>
      <indexical_variance>From the fisherman's perspective, the market is a brutal trap where all risk is borne by the producer. From the perspective of the market in Havana (an unseen institutional power), this is simply a Rope—a mechanism for pricing and distributing goods, where spoiled products have no value.</indexical_variance>
      <selection_reason>This constraint represents the final, systemic arbiter of the old man's struggle. It connects his physical effort (C1) and social motivation (C2) to an impersonal economic reality, creating the story's tragic conclusion.</selection_reason>
    </constraint>
  </selected>
  <deferred>
    <constraint id="C4" name="Kinship Obligation">
      <hypothesis>Tangled Rope</hypothesis>
      <offstage_function>This explains the boy's physical absence, enforcing the old man's solitude and making the central struggle an individual one.</offstage_function>
    </constraint>
    <constraint id="C5" name="Professional Pride">
      <hypothesis>Snare</hypothesis>
      <offstage_function>This acts as the character's internal motivation, his response to the other constraints, driving him to go "too far out" and take on a challenge that pushes him past his physical limits.</offstage_function>
    </constraint>
    <constraint id="C6" name="Ecological Indifference">
      <hypothesis>Mountain</hypothesis>
      <offstage_function>Provides the arena of conflict and the agents (marlin, sharks, weather) that enact the physical struggle and the economic loss, serving as the unfeeling backdrop for the human drama.</offstage_function>
    </constraint>
  </deferred>
  <generation_sequence>C1 → C2 → C3</generation_sequence>
  <omegas>
    <omega id="omega_1">The analysis treats the community's belief in 'luck' as a social construct. It cannot resolve whether, within the story's world, luck is a real, metaphysical force or purely a post-hoc rationalization for random success and failure.</omega>
  </omegas>
</constraint_manifest>
```