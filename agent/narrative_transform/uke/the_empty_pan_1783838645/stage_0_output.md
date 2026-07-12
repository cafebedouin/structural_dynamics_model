```xml
<constraint_manifest>
  <selected>
    <constraint id="C1" name="unownable_truth" generation_order="1">
      <base_properties>
        <epsilon>0.05</epsilon>
        <suppression>0.0</suppression>
        <coordination>false</coordination>
        <asymmetric>false</asymmetric>
      </base_properties>
      <graph>
        <downstream_of>none</downstream_of>
        <feeds_into>none</feeds_into>
      </graph>
      <character_classifications>
        <character name="Narrator (apprentice)">
          <index>
            <power>powerless</power>
            <time>biographical</time>
            <exit>trapped</exit>
            <scope>local</scope>
          </index>
          <chi>0.06</chi>
          <type>Mountain</type>
        </character>
        <character name="Verrel">
          <index>
            <power>analytical</power>
            <time>biographical</time>
            <exit>identity_locked</exit>
            <scope>local</scope>
          </index>
          <chi>0.046</chi>
          <type>Mountain</type>
        </character>
        <character name="Crown">
          <index>
            <power>institutional</power>
            <time>generational</time>
            <exit>arbitrage</exit>
            <scope>regional</scope>
          </index>
          <chi>-0.009</chi>
          <type>Mountain</type>
        </character>
      </character_classifications>
      <indexical_variance>None. As a law of nature, this constraint's classification is stable across all indices.</indexical_variance>
      <selection_reason>This constraint represents the story's core metaphysical principle—the background reality that makes the central problem intractable. It is the most upstream and structurally distinct element.</selection_reason>
    </constraint>
    <constraint id="C2" name="foundational_choice" generation_order="2">
      <base_properties>
        <epsilon>0.1</epsilon>
        <suppression>0.8</suppression>
        <coordination>true</coordination>
        <asymmetric>true</asymmetric>
      </base_properties>
      <graph>
        <downstream_of>none</downstream_of>
        <feeds_into>C3</feeds_into>
      </graph>
      <character_classifications>
        <character name="Narrator (apprentice)">
          <index>
            <power>powerless</power>
            <time>biographical</time>
            <exit>trapped</exit>
            <scope>local</scope>
          </index>
          <chi>0.12</chi>
          <type>Rope</type>
        </character>
        <character name="Verrel">
          <index>
            <power>analytical</power>
            <time>biographical</time>
            <exit>identity_locked</exit>
            <scope>local</scope>
          </index>
          <chi>0.092</chi>
          <type>Rope</type>
        </character>
        <character name="Crown">
          <index>
            <power>institutional</power>
            <time>generational</time>
            <exit>arbitrage</exit>
            <scope>regional</scope>
          </index>
          <chi>-0.018</chi>
          <type>Rope</type>
        </character>
      </character_classifications>
      <indexical_variance>None among the specified characters. The principle that a baseline must be chosen is a necessary condition for coordination (Rope) for anyone who understands it, regardless of their position.</indexical_variance>
      <selection_reason>This constraint provides the epistemological foundation for the primary social conflict. It is the upstream dependency that explains how constructed systems can be mistaken for natural ones.</selection_reason>
    </constraint>
    <constraint id="C3" name="unquestionable_standard" generation_order="3">
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
        <character name="Narrator (apprentice)">
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
          <chi>0.736</chi>
          <type>Snare</type>
        </character>
        <character name="Crown">
          <index>
            <power>institutional</power>
            <time>generational</time>
            <exit>arbitrage</exit>
            <scope>regional</scope>
          </index>
          <chi>-0.144</chi>
          <type>Rope</type>
        </character>
        <character name="Merchant">
          <index>
            <power>moderate</power>
            <time>immediate</time>
            <exit>constrained</exit>
            <scope>local</scope>
          </index>
          <chi>0.64</chi>
          <type>Tangled Rope</type>
        </character>
      </character_classifications>
      <indexical_variance>High. The same institutional rule is a Snare for the powerless and the analyst, a functional Rope for its beneficiary, and a Tangled Rope for a typical user of the system.</indexical_variance>
      <selection_reason>This is the central, most active constraint in the narrative, with the highest centrality score (4) and the greatest indexical variance. It is the concrete manifestation of the principles established by C1 and C2.</selection_reason>
    </constraint>
  </selected>

  <deferred>
    <constraint id="C4" name="declared_hand">
      <hypothesis>Rope</hypothesis>
      <offstage_function>Provides the story's ethical resolution and demonstrates how agents can act with integrity inside a system they cannot change, shaping the protagonist's final identity.</offstage_function>
    </constraint>
    <constraint id="C5" name="physical_cold">
      <hypothesis>Mountain</hypothesis>
      <offstage_function>Acts as a constant, physical parallel to the moral and ethical pressure the characters are under, grounding the abstract conflict in bodily experience.</offstage_function>
    </constraint>
    <constraint id="C6" name="guild_culture">
      <hypothesis>Tangled Rope</hypothesis>
      <offstage_function>Provides the institutional inertia and atomization that makes a systemic challenge impossible, forcing characters into the individual, private solutions explored in the narrative.</offstage_function>
    </constraint>
  </deferred>

  <generation_sequence>C1 → C2 → C3</generation_sequence>

  <invariant_contract>
    <untranslatable_real present="yes">There exists an objective, universally coherent state of things that is perceivable but cannot be captured or instrumentalized, as any act of possession introduces a bias that collapses its universality.</untranslatable_real>
    <missing_floor present="yes">Any system of evaluation is founded upon a chosen, unprovable baseline, an initial partition of the world established by an interested party and subsequently treated as a neutral ground.</missing_floor>
    <inherent_instrument value="yes">The extraction is enacted through a certified measurement process; without the official standard and the act of stamping, the constraint has no mechanism.</inherent_instrument>
  </invariant_contract>

  <omegas>
    <omega id="accumulation_effect">It is unresolved whether the private act of documenting the system's bias can ever accumulate into a public or systemic change, or if it is a permanently contained act of personal integrity.</omega>
  </omegas>
</constraint_manifest>
```