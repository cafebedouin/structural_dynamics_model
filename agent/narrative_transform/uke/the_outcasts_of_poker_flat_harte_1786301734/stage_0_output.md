```xml
<constraint_manifest>
  <selected>
    <constraint id="C1" name="MoralPurge" generation_order="1">
      <base_properties>
        <epsilon>1.0</epsilon>
        <suppression>0.9</suppression>
        <coordination>true</coordination>
        <asymmetric>true</asymmetric>
      </base_properties>
      <graph>
        <downstream_of>none</downstream_of>
        <feeds_into>C2</feeds_into>
      </graph>
      <character_classifications>
        <character name="John Oakhurst">
          <index>
            <power>powerless</power>
            <time>immediate</time>
            <exit>trapped</exit>
            <scope>local</scope>
          </index>
          <chi>1.20</chi>
          <type>Snare</type>
        </character>
        <character name="The Duchess">
          <index>
            <power>powerless</power>
            <time>immediate</time>
            <exit>trapped</exit>
            <scope>local</scope>
          </index>
          <chi>1.20</chi>
          <type>Snare</type>
        </character>
        <character name="Poker Flat Committee">
          <index>
            <power>institutional</power>
            <time>immediate</time>
            <exit>arbitrage</exit>
            <scope>local</scope>
          </index>
          <chi>-0.16</chi>
          <type>Rope</type>
        </character>
      </character_classifications>
      <indexical_variance>For the outcasts, this is a high-extraction Snare from which they cannot escape. For the committee enforcing it, it is a low-cost Rope for coordinating social cleansing.</indexical_variance>
      <selection_reason>Centrality score of 4. This is the story's inciting incident, a clear Snare that establishes the core social conflict and sets all other events in motion.</selection_reason>
    </constraint>
    <constraint id="C2" name="EnvironmentalTrap" generation_order="2">
      <base_properties>
        <epsilon>0.1</epsilon>
        <suppression>0.0</suppression>
        <coordination>false</coordination>
        <asymmetric>false</asymmetric>
      </base_properties>
      <graph>
        <downstream_of>C1</downstream_of>
        <feeds_into>C3</feeds_into>
      </graph>
      <character_classifications>
        <character name="All characters in camp">
          <index>
            <power>powerless</power>
            <time>immediate</time>
            <exit>trapped</exit>
            <scope>universal</scope>
          </index>
          <chi>0.15</chi>
          <type>Mountain</type>
        </character>
      </character_classifications>
      <indexical_variance>None. This constraint affects all characters equally, regardless of their social standing or personal power, classifying as a Mountain for everyone trapped by it.</indexical_variance>
      <selection_reason>Centrality score of 3. It is the most structurally distinct upstream dependency of the highest-centrality constraint, shifting the story from a social conflict to a survival conflict.</selection_reason>
    </constraint>
    <constraint id="C3" name="ResourceScarcity" generation_order="3">
      <base_properties>
        <epsilon>0.4</epsilon>
        <suppression>0.5</suppression>
        <coordination>true</coordination>
        <asymmetric>true</asymmetric>
      </base_properties>
      <graph>
        <downstream_of>C2</downstream_of>
        <feeds_into>none</feeds_into>
      </graph>
      <character_classifications>
        <character name="Mother Shipton">
          <index>
            <power>powerless</power>
            <time>immediate</time>
            <exit>trapped</exit>
            <scope>local</scope>
          </index>
          <chi>0.48</chi>
          <type>Tangled Rope</type>
        </character>
        <character name="John Oakhurst">
          <index>
            <power>moderate</power>
            <time>immediate</time>
            <exit>trapped</exit>
            <scope>local</scope>
          </index>
          <chi>0.32</chi>
          <type>Rope</type>
        </character>
        <character name="Uncle Billy">
          <index>
            <power>institutional</power>
            <time>immediate</time>
            <exit>arbitrage</exit>
            <scope>local</scope>
          </index>
          <chi>-0.06</chi>
          <type>Rope</type>
        </character>
      </character_classifications>
      <indexical_variance>For most, it's a Tangled Rope balancing coordination (rationing) and extraction (starvation). For Oakhurst, who manages it, it's a Rope. For Uncle Billy, who exploits it by stealing the means of replenishment, he briefly becomes an institutional beneficiary (π=-0.2) and exits the constraint.</indexical_variance>
      <selection_reason>Highest centrality score of 5. This Tangled Rope is the primary engine of the drama in the second half of the story, forcing moral choices about cooperation and sacrifice.</selection_reason>
    </constraint>
  </selected>
  <deferred>
    <constraint id="C4" name="SocialCaste">
      <hypothesis>Snare</hypothesis>
      <offstage_function>Provides the underlying ideological justification for C1 (MoralPurge), explaining why the outcasts are grouped together and considered disposable by the town.</offstage_function>
    </constraint>
    <constraint id="C5" name="GamblersCode">
      <hypothesis>Rope</hypothesis>
      <offstage_function>Shapes the protagonist's behavior, making him a stoic and capable leader for the group, while also explaining his final, fatalistic decision.</offstage_function>
    </constraint>
    <constraint id="C6" name="SharedFate">
      <hypothesis>Rope</hypothesis>
      <offstage_function>Acts as the emergent social dynamic resulting from C2 and C3, describing the temporary community that forms without needing to be a separately generated constraint.</offstage_function>
    </constraint>
  </deferred>
  <generation_sequence>C1 → C2 → C3</generation_sequence>
  <invariant_contract>
    <untranslatable_real present="yes" primary="no">A capacity for dignified action exists independent of any socially assigned category.</untranslatable_real>
    <missing_floor present="yes" primary="yes">The legitimacy of one's existence within a group is determined by a dominant faction's arbitrary and privately held standards.</missing_floor>
    <inherent_instrument value="no">No. The constraint is enforced through direct social power, not through a separable system of measurement.</inherent_instrument>
  </invariant_contract>
  <break_contract>
    <original_break>The expectation that socially designated outcasts are incapable of honorable action.</original_break>
    <prior_status>DEAD</prior_status>
    <target_prior>The expectation that an individual's personal code is sufficient to overcome an indifferent, overwhelming system.</target_prior>
  </break_contract>
  <omegas>
    <omega id="O1">The analysis cannot resolve whether Oakhurst's final act was a submission to his code of "luck" or a final, calculated sacrifice to leave a clear message.</omega>
  </omegas>
</constraint_manifest>
```