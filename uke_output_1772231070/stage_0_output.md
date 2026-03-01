```xml
<constraint_manifest>
  <selected>
    <constraint id="C1" name="The New Language" generation_order="1">
      <base_properties>
        <epsilon>0.40</epsilon>
        <suppression>0.30</suppression>
        <coordination>true</coordination>
        <asymmetric>true</asymmetric>
      </base_properties>
      <graph>
        <downstream_of>none</downstream_of>
        <feeds_into>C3</feeds_into>
      </graph>
      <character_experiences>
        <character name="Louise Darling">
          <index>
            <power>powerful</power>
            <time>biographical</time>
            <exit>mobile</exit>
            <scope>local</scope>
          </index>
          <chi>0.19</chi>
          <type>Rope</type>
          <experience>My world is full of exciting people and ideas; knowing about art, books, and politics is how we connect and build our careers. It's a language that opens doors.</experience>
        </character>
        <character name="Christian Darling">
          <index>
            <power>powerless</power>
            <time>biographical</time>
            <exit>trapped</exit>
            <scope>local</scope>
          </index>
          <chi>0.48</chi>
          <type>Tangled Rope</type>
          <experience>My wife and her friends talk about things I don't understand—painters, writers, politics. It's a high storm of words I'm on the edge of. To be with her, I have to be in this world, but I don't belong and can't keep up.</experience>
        </character>
      </character_experiences>
      <indexical_variance>For the wife, knowing about culture is a tool for connection and success. For the husband, that same system of knowledge is a barrier that confirms his alienation from her and her world.</indexical_variance>
      <selection_reason>This constraint represents the new world that renders the protagonist's skills obsolete. Its starkly different impact on the husband and wife is the primary engine of their divergence.</selection_reason>
    </constraint>
    <constraint id="C2" name="The Public Arena" generation_order="2">
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
        <character name="Christian Darling (as a young man)">
          <index>
            <power>powerful</power>
            <time>immediate</time>
            <exit>mobile</exit>
            <scope>local</scope>
          </index>
          <chi>0.34</chi>
          <type>Rope</type>
          <experience>On the football field, everything is clear. If I run well, I am rewarded with applause, respect, and a girl's affection. My body's performance secures my future.</experience>
        </character>
        <character name="Diederich (post-career)">
          <index>
            <power>powerless</power>
            <time>biographical</time>
            <exit>trapped</exit>
            <scope>national</scope>
          </index>
          <chi>1.05</chi>
          <type>Snare</type>
          <experience>I was the best, an All-American. I played professionally until they broke my neck for me. Now I walk around in an iron brace.</experience>
        </character>
      </character_experiences>
      <indexical_variance>The same system of college football that provides a path to glory and status for a young star is revealed to be a system that extracts physical health, leaving another star permanently broken.</indexical_variance>
      <selection_reason>This is the foundational constraint that establishes the protagonist's entire sense of self-worth and defines the "high point" from which he declines. It is the origin