```xml
<constraint_manifest>
  <selected>
    <constraint id="C1" name="The Unspoken Social Divide" generation_order="1">
      <base_properties>
        <epsilon>0.70</epsilon>
        <suppression>0.80</suppression>
        <coordination>false</coordination>
        <asymmetric>true</asymmetric>
      </base_properties>
      <graph>
        <downstream_of>none</downstream_of>
        <feeds_into>C2, C3</feeds_into>
      </graph>
      <character_experiences>
        <character name="Bertie and Billy">
          <index>
            <power>institutional</power>
            <time>biographical</time>
            <exit>arbitrage</exit>
            <scope>national</scope>
          </index>
          <chi>-0.14</chi>
          <type>Rope</type>
          <experience>The world is structured in a way that is comfortable and advantageous; their background provides them with resources, confidence, and a sense of play that others lack.</experience>
        </character>
        <character name="Oscar Maironi">
          <index>
            <power>powerless</power>
            <time>biographical</time>
            <exit>trapped</exit>
            <scope>national</scope>
          </index>
          <chi>1.05</chi>
          <type>Snare</type>
          <experience>His origins define him and limit his options, forcing him into a life of constant calculation and toil to gain entry into a world where others belong by birthright.</experience>
        </character>
      </character_experiences>
      <indexical_variance>For the wealthy students, their social position is a supportive structure that enables their freedom. For the poor student, it is a trap that dictates his every move and interaction.</indexical_variance>
      <selection_reason>This is the primary upstream constraint; it dictates the power dynamics, resources, and worldviews of all characters, making every other interaction a downstream consequence of class.</selection_reason>
    </constraint>
    <constraint id="C2" name="The Meritocracy Myth" generation_order="2">
      <base_properties>
        <epsilon>0.60</epsilon>
        <suppression>0.70</suppression>
        <coordination>true</coordination>
        <asymmetric>true</asymmetric>
      </base_properties>
      <graph>
        <downstream_of>C1</downstream_of>
        <feeds_into>C3</feeds_into>
      </graph>
      <character_experiences>
        <character name="Bertie and Billy">
          <index>
            <power>powerful</power>
            <time>biographical</time>
            <exit>mobile</exit>
            <scope>national</scope>
          </index>
          <chi>0.36</chi>
          <type>Rope</type>
          <experience>The academic system is a game with rules that can be learned and mastered at the last minute; success is about cleverness and insight, not just brute effort.</experience>
        </character>
        <character name="Oscar Maironi">
          <index>
            <power>analytical</power>
            <time>biographical</time>
            <exit>trapped</exit>
            <scope>national</scope>
          </index>
          <chi>0.69</chi>
          <type>Tangled Rope</type>
          <experience>He believes academic success is a direct and fair measure of virtue and hard work, a belief that provides him with a sense of superiority but ultimately traps him by failing to reward his diligence as he expects.</experience>
        </character>
      </character_experiences>
      <indexical_variance>The boys treat the belief that grades equal merit as a useful fiction they can strategically engage with. Oscar treats it as a fundamental law of the universe, making him both an expert navigator of its rules and a victim of its limitations.</indexical_variance>
      <selection_reason>This constraint governs the ideology and beliefs of the characters, providing the central irony of the story when the system rewards a different kind of "merit" than the one the most dedicated character believes in.</selection_reason>
    </constraint>
    <constraint id="C3" name="The Final Examination" generation_order="3">
      <base_properties>
        <epsilon>0.80</epsilon>
        <suppression>0.90</suppression>
        <coordination>false</coordination>
        <asymmetric>true</asymmetric>
      </base_properties>
      <graph>
        <downstream_of>C1, C2</downstream_of>
        <feeds_into>none</feeds_into>
      </graph>
      <character_experiences>
        <character name="Bertie and Billy">
          <index>
            <power>powerful</power>
            <time>immediate</time>
            <exit>constrained</exit>
            <scope>local</scope>
          </index>
          <chi>0.38</chi>
          <type>Rope</type>
          <experience>The exam is a manageable hurdle, a deadline that prompts a burst of focused, game-like activity, but its outcome does not threaten their fundamental place in the world.</experience>
        </character>
        <character name="Oscar Maironi">
          <index>
            <power>powerless</power>
            <time>biographical</time>
            <exit>trapped</exit>
            <scope>local</scope>
          </index>
          <chi>0.96</chi>
          <type>Snare</type>
          <experience>The exam is a day of judgment where his entire year's meticulous labor will be weighed; failure would be a catastrophic setback to his life's ambition.</experience>
        </character>
      </character_experiences>
      <indexical_variance>The same test is experienced as a challenging game by the wealthy students and as a life-or-death trial by the poor one, due to the vast difference in stakes and resources.</indexical_variance>
      <selection_reason>This is the story's central downstream event, the tangible test where the invisible pressures of class (C1) and ideology (C2) are made manifest and judged, producing the story's ironic climax.</selection_reason>
    </constraint>
  </selected>
  <deferred>
    <constraint id="C4" name="The Tutoring Contract">
      <hypothesis>Tangled Rope</hypothesis>
      <offstage_function>This constraint serves as the narrative engine, forcing the characters from different social worlds into a confined space and making their power differential explicit through a commercial transaction (pay for knowledge).</offstage_function>
    </constraint>
    <constraint id="C5" name="The Leisure Gap">
      <hypothesis>Mountain</hypothesis>
      <offstage_function>As a facet of the social divide, this explains the difference in the characters' energy and approach. It's the background condition that allows the boys' adventure and creative thinking, while limiting the tutor to joyless toil.</offstage_function>
    </constraint>
  </deferred>
  <generation_sequence>C1 → C2 → C3</generation_sequence>
  <omegas>
    <omega id="professor_logic">The analysis cannot fully resolve the professor's reasoning. Is he rewarding genuine insight born of lived experience, or is he simply rewarding the familiar, confident style of the upper class, mistaking it for originality?</omega>
  </omegas>
</constraint_manifest>
```