```xml
<constraint_manifest>
  <selected>
    <constraint id="C1" name="ObjectiveValueUnpossessable" generation_order="1">
      <base_properties>
        <epsilon>0.00</epsilon>
        <suppression>0.00</suppression>
        <coordination>false</coordination>
        <asymmetric>false</asymmetric>
      </base_properties>
      <graph>
        <downstream_of>none</downstream_of>
        <feeds_into>C2</feeds_into>
      </graph>
      <character_classifications>
        <character name="Apprentice">
          <index>
            <power>powerless</power>
            <time>biographical</time>
            <exit>trapped</exit>
            <scope>local</scope>
          </index>
          <chi>0.00</chi>
          <type>Mountain</type>
        </character>
        <character name="Mentor">
          <index>
            <power>analytical</power>
            <time>biographical</time>
            <exit>identity_locked</exit>
            <scope>local</scope>
          </index>
          <chi>0.00</chi>
          <type>Mountain</type>
        </character>
        <character name="Master">
          <index>
            <power>moderate</power>
            <time>biographical</time>
            <exit>identity_locked</exit>
            <scope>local</scope>
          </index>
          <chi>0.00</chi>
          <type>Mountain</type>
        </character>
      </character_classifications>
      <indexical_variance>None. As a law of nature, this constraint is perceived as a Mountain from all character indices.</indexical_variance>
      <selection_reason>Most upstream dependency. It is the philosophical foundation for the entire system, explaining why a perfect, instrumental standard is impossible and setting the stage for the compromised social one.</selection_reason>
    </constraint>
    <constraint id="C2" name="CompromisedStandard" generation_order="2">
      <base_properties>
        <epsilon>0.80</epsilon>
        <suppression>0.90</suppression>
        <coordination>true</coordination>
        <asymmetric>true</asymmetric>
      </base_properties>
      <graph>
        <downstream_of>C1</downstream_of>
        <feeds_into>C3</feeds_into>
      </graph>
      <character_classifications>
        <character name="Apprentice">
          <index>
            <power>powerless</power>
            <time>biographical</time>
            <exit>trapped</exit>
            <scope>local</scope>
          </index>
          <chi>0.96</chi>
          <type>Snare</type>
        </character>
        <character name="Master">
          <index>
            <power>moderate</power>
            <time>biographical</time>
            <exit>identity_locked</exit>
            <scope>local</scope>
          </index>
          <chi>0.64</chi>
          <type>Tangled Rope</type>
        </character>
        <character name="Mentor">
          <index>
            <power>analytical</power>
            <time>biographical</time>
            <exit>identity_locked</exit>
            <scope>local</scope>
          </index>
          <chi>0.74</chi>
          <type>Snare</type>
        </character>
        <character name="Authority">
          <index>
            <power>institutional</power>
            <time>generational</time>
            <exit>arbitrage</exit>
            <scope>national</scope>
          </index>
          <chi>-0.16</chi>
          <type>Rope</type>
        </character>
        <character name="Subject">
          <index>
            <power>powerless</power>
            <time>biographical</time>
            <exit>trapped</exit>
            <scope>local</scope>
          </index>
          <chi>0.96</chi>
          <type>Snare</type>
        </character>
      </character_classifications>
      <indexical_variance>High. The same constraint is a Rope for its beneficiary (Authority), a Snare for its victims (Subject, Apprentice) and those who see its structure clearly (Mentor), and a Tangled Rope for the experienced practitioner (Master) who must navigate both its coordination and extraction aspects.</indexical_variance>
      <selection_reason>Highest centrality score. This is the primary engine of the narrative's conflict, a hybrid system of coordination and extraction whose true nature is revealed over time.</selection_reason>
    </constraint>
    <constraint id="C3" name="MarginalDissent" generation_order="3">
      <base_properties>
        <epsilon>0.00</epsilon>
        <suppression>0.00</suppression>
        <coordination>true</coordination>
        <asymmetric>false</asymmetric>
      </base_properties>
      <graph>
        <downstream_of>C2</downstream_of>
        <feeds_into>none</feeds_into>
      </graph>
      <character_classifications>
        <character name="Master">
          <index>
            <power>moderate</power>
            <time>biographical</time>
            <exit>identity_locked</exit>
            <scope>local</scope>
          </index>
          <chi>0.00</chi>
          <type>Rope</type>
        </character>
        <character name="Mentor">
          <index>
            <power>analytical</power>
            <time>biographical</time>
            <exit>identity_locked</exit>
            <scope>local</scope>
          </index>
          <chi>0.00</chi>
          <type>Rope</type>
        </character>
      </character_classifications>
      <indexical_variance>None. This is a chosen ethical practice; for those who adopt it, it functions as a pure, non-extractive coordination tool for maintaining personal integrity.</indexical_variance>
      <selection_reason>Structurally critical downstream response. It represents the story's terminal state and ethical conclusion, demonstrating an agent's developed response to navigating an unfixable, compromised system.</selection_reason>
    </constraint>
  </selected>

  <deferred>
    <constraint id="C4" name="ArbitraryFoundation">
      <hypothesis>Rope</hypothesis>
      <offstage_function>Acts as the philosophical justification for why rebellion is futile, foreclosing the search for a 'perfect' standard and forcing the protagonist to find an ethical position within an imperfect one.</offstage_function>
    </constraint>
    <constraint id="C5" name="EconomicNecessity">
      <hypothesis>Rope</hypothesis>
      <offstage_function>Provides the constant, pragmatic pressure that justifies complicity in the compromised system, framing participation as a social duty ("keep the port fed") rather than a moral failure.</offstage_function>
    </constraint>
  </deferred>

  <generation_sequence>C1 → C2 → C3</generation_sequence>

  <omegas>
    <omega id="merchant_awareness">The analysis cannot resolve whether the subjects of the extraction are aware of the compromised standard. Their passivity is crucial to the system's stability, but its source (ignorance, powerlessness, or tacit acceptance) is unknown.</omega>
  </omegas>
</constraint_manifest>
```