```xml
<constraint_manifest>
  <selected>
    <constraint id="C1" name="The Fisherman's Code" generation_order="1">
      <base_properties>
        <epsilon>0.30</epsilon>
        <suppression>0.80</suppression>
        <coordination>true</coordination>
        <asymmetric>false</asymmetric>
      </base_properties>
      <graph>
        <downstream_of>none</downstream_of>
        <feeds_into>none</feeds_into>
      </graph>
      <character_experiences>
        <character name="Santiago">
          <index>
            <power>moderate</power>
            <time>biographical</time>
            <exit>trapped</exit>
            <scope>local</scope>
          </index>
          <chi>0.24</chi>
          <type>Rope</type>
          <experience>He must be exact in his work, even if he is not lucky, because this precision is the source of his identity and pride. He endures immense suffering not just to survive, but to live up to his own standard of what a man should be.</experience>
        </character>
        <character name="Younger Fishermen">
          <index>
            <power>powerful</power>
            <time>biographical</time>
            <exit>mobile</exit>
            <scope>local</scope>
          </index>
          <chi>0.14</chi>
          <type>Rope</type>
          <experience>They see the sea as a place of business, an opponent to be conquered with technology, not a partner in a code of mutual respect. Their professionalism is about economic results, not existential endurance.</experience>
        </character>
      </character_experiences>
      <indexical_variance>For Santiago, the code is a deeply personal, almost spiritual discipline that gives meaning to his suffering. For the younger generation, it is a set of professional best practices mediated by technology, lacking the same existential weight.</indexical_variance>
      <selection_reason>Provides the internal, self-imposed constraint that drives the protagonist's actions, contrasting with the external social and familial pressures. It represents his primary source of agency and dignity.</selection_reason>
    </constraint>
    <constraint id="C2" name="The Law of Luck" generation_order="2">
      <base_properties>
        <epsilon>0.60</epsilon>
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
            <time>biographical</time>
            <exit>trapped</exit>
            <scope>local</scope>
          </index>
          <chi>0.72</chi>
          <type>Snare</type>
          <experience>He is marked by the community as definitively unlucky after a long dry spell, a label that isolates him socially and costs him his apprentice.</experience>
        </character>
        <character name="Manolin's Parents">
          <index>
            <power>moderate</power>
            <time>biographical</time>
            <exit>mobile</exit>
            <scope>local</scope>
          </index>
          <chi>0.48</chi>
          <type>Tangled Rope</type>
          <experience>They use the old man's unluckiness as a practical reason to move their son to a boat that brings in money, balancing their son's economic future against his emotional ties.</experience>
        </character>
      </character_experiences>
      <indexical_variance>What is an inescapable trap of reputation for Santiago is a pragmatic management tool for the parents. The same social belief system functions as oppression for one and a coordination mechanism for another.</indexical_variance>
      <selection_reason>This is the primary social constraint driving the plot, creating the initial separation between the man and the boy and establishing the stakes for the fishing trip. It is a False Mountain, appearing natural but being socially constructed.</selection_reason>
    </constraint>
    <constraint id="C3" name="A Son's Duty" generation_order="3">
      <base_properties>
        <epsilon>0.50</epsilon>
        <suppression>0.70</suppression>
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
            <exit>trapped</exit>
            <scope>local</scope>
          </index>
          <chi>0.60</chi>
          <type>Tangled Rope</type>
          <experience>He loves the old man and wants to fish with him, but he must obey his father's orders. This duty, which he does not question, causes him sadness but is also part of the family structure that supports him.</experience>
        </character>
        <character name="Santiago">
          <index>
            <power>moderate</power>
            <time>biographical</time>
            <exit>analytical</exit>
            <scope>local</scope>
          </index>
          <chi>0.40</chi>
          <type>Rope</type>
          <experience>He sees the boy's obedience to his father as a normal and correct part of life, accepting it without resentment as the way things are supposed to be.</experience>
        </character>
      </character_experiences>
      <indexical_variance>The boy experiences the constraint as a painful conflict between love and duty (Tangled Rope), while the old man, from the outside, sees it as a simple, functional rule of society (Rope).</indexical_variance>
      <selection_reason>As a high-centrality Tangled Rope, it embodies the story's core emotional conflict, showing how social rules create impossible choices. It connects the social pressure of C2 to the personal relationship between the characters.</selection_reason>
    </constraint>
  </selected>
  <deferred>
    <constraint id="C4" name="Bodily Decline">
      <hypothesis>Mountain</hypothesis>
      <offstage_function>Provides the physical vulnerability and ticking clock that makes Santiago's adherence to his professional code so poignant and his struggle so desperate. It is the biological reality underpinning the social drama.</offstage_function>
    </constraint>
    <constraint id="C5" name="Material Poverty">
      <hypothesis>Snare</hypothesis>
      <offstage_function>Creates the material desperation that makes catching a fish not just a matter of pride, but of survival, explaining the fictions with the boy and the reliance on community charity.</offstage_function>
    </constraint>
    <constraint id="C6" name="The Cycle of Predation">
      <hypothesis>Mountain</hypothesis>
      <offstage_function>Acts as the ultimate, indifferent arbiter against which the entire human drama plays out, reminding the reader that human victory and defeat are small things in the face of cosmic processes.</offstage_function>
    </constraint>
    <constraint id="C7" name="Reputation Economy">
      <hypothesis>Tangled Rope</hypothesis>
      <offstage_function>This is the invisible operating system of the village that gives the 'Law of Luck' its power. It's the reason being 'salao' has real economic consequences beyond mere superstition.</offstage_function>
    </constraint>
  </deferred>
  <generation_sequence>C1 → C2 → C3</generation_sequence>
  <omegas>
    <omega id="resolution_source">The analysis assumes Santiago's endurance comes from his professional code. However, the text suggests a deeper source ("I may not be as strong as I think, but I know many tricks and I have resolution"). The ultimate origin of this resolution—whether it's pride, love for the boy, or an innate quality—remains an unresolvable core of his character.</omega>
  </omegas>
</constraint_manifest>
```