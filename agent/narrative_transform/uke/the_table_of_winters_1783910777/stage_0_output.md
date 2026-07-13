```xml
<constraint_manifest>
  <selected>
    <constraint id="C1" name="Mandatory_Insurance_via_Market_Access" generation_order="1">
      <base_properties>
        <epsilon>0.90</epsilon>
        <suppression>0.80</suppression>
        <coordination>false</coordination>
        <asymmetric>true</asymmetric>
      </base_properties>
      <graph>
        <downstream_of>none</downstream_of>
        <feeds_into>C2</feeds_into>
      </graph>
      <character_classifications>
        <character name="sailors">
          <index>
            <power>powerless</power>
            <time>biographical</time>
            <exit>trapped</exit>
            <scope>local</scope>
          </index>
          <chi>1.08</chi>
          <type>Snare</type>
        </character>
        <character name="narrator">
          <index>
            <power>analytical</power>
            <time>biographical</time>
            <exit>analytical</exit>
            <scope>local</scope>
          </index>
          <chi>0.83</chi>
          <type>Snare</type>
        </character>
        <character name="office">
          <index>
            <power>institutional</power>
            <time>generational</time>
            <exit>arbitrage</exit>
            <scope>regional</scope>
          </index>
          <chi>-0.16</chi>
          <type>Rope</type>
        </character>
      </character_classifications>
      <indexical_variance>High. The constraint that traps sailors as a Snare is experienced by its institutional administrators as a functional Rope.</indexical_variance>
      <selection_reason>This is the foundational constraint that makes the entire system coercive; it is the most upstream and enables all subsequent extraction.</selection_reason>
    </constraint>
    <constraint id="C2" name="Class-Based_Risk_Pricing" generation_order="2">
      <base_properties>
        <epsilon>0.80</epsilon>
        <suppression>0.70</suppression>
        <coordination>true</coordination>
        <asymmetric>true</asymmetric>
      </base_properties>
      <graph>
        <downstream_of>C1</downstream_of>
        <feeds_into>C3</feeds_into>
      </graph>
      <character_classifications>
        <character name="sailors">
          <index>
            <power>powerless</power>
            <time>biographical</time>
            <exit>trapped</exit>
            <scope>local</scope>
          </index>
          <chi>0.96</chi>
          <type>Snare</type>
        </character>
        <character name="narrator">
          <index>
            <power>analytical</power>
            <time>biographical</time>
            <exit>analytical</exit>
            <scope>local</scope>
          </index>
          <chi>0.74</chi>
          <type>Snare</type>
        </character>
        <character name="office">
          <index>
            <power>institutional</power>
            <time>generational</time>
            <exit>arbitrage</exit>
            <scope>regional</scope>
          </index>
          <chi>-0.14</chi>
          <type>Rope</type>
        </character>
      </character_classifications>
      <indexical_variance>High. The actuarial table is a Snare from the perspective of those being priced, but a functional coordination mechanism (Rope) for the institution that profits from it.</indexical_variance>
      <selection_reason>This is the central mechanism of the story, with the highest centrality score (5). It translates the economic pressure from C1 into a formal, numerical system that re-shapes behavior.</selection_reason>
    </constraint>
    <constraint id="C3" name="Transactional_Loss_Verification" generation_order="3">
      <base_properties>
        <epsilon>0.50</epsilon>
        <suppression>0.90</suppression>
        <coordination>true</coordination>
        <asymmetric>true</asymmetric>
      </base_properties>
      <graph>
        <downstream_of>C2</downstream_of>
        <feeds_into>none</feeds_into>
      </graph>
      <character_classifications>
        <character name="sailors">
          <index>
            <power>powerless</power>
            <time>biographical</time>
            <exit>trapped</exit>
            <scope>local</scope>
          </index>
          <chi>0.60</chi>
          <type>Tangled Rope</type>
        </character>
        <character name="narrator">
          <index>
            <power>analytical</power>
            <time>biographical</time>
            <exit>analytical</exit>
            <scope>local</scope>
          </index>
          <chi>0.46</chi>
          <type>Tangled Rope</type>
        </character>
        <character name="office">
          <index>
            <power>institutional</power>
            <time>generational</time>
            <exit>arbitrage</exit>
            <scope>regional</scope>
          </index>
          <chi>-0.09</chi>
          <type>Rope</type>
        </character>
      </character_classifications>
      <indexical_variance>Moderate. While it is a functional Rope for the institution, it is a hybrid Tangled Rope for both sailors and the analyst, providing necessary coordination at an extractive cost.</indexical_variance>
      <selection_reason>Selected as the next-highest centrality constraint (4) with a distinct type (Tangled Rope). It demonstrates how the primary system's logic colonizes and commodifies adjacent social practices.</selection_reason>
    </constraint>
  </selected>

  <deferred>
    <constraint id="C4" name="Sailing_to_the_Class">
      <hypothesis>Snare</hypothesis>
      <offstage_function>This is the central tragic behavior resulting from the selected constraints; it is the unseen pressure shaping every decision a sailor makes, forcing them to abandon their expertise.</offstage_function>
    </constraint>
    <constraint id="C5" name="Incommunicable_Expertise">
      <hypothesis>Mountain</hypothesis>
      <offstage_function>This acts as the story's core real, the un-measurable value that the formal system (C2) is blind to and actively destroys, creating the central conflict between two kinds of knowledge.</offstage_function>
    </constraint>
    <constraint id="C6" name="Indifferent_Nature">
      <hypothesis>Mountain</hypothesis>
      <offstage_function>This is the ultimate physical ground truth where the failures of the formal system have fatal consequences, acting as the story's indifferent and final arbiter.</offstage_function>
    </constraint>
  </deferred>

  <generation_sequence>C1 → C2 → C3</generation_sequence>

  <invariant_contract>
    <untranslatable_real present="yes">A lifetime of sensory experience between a person and a complex system develops a non-symbolic understanding that keeps them alive but which ceases to exist upon any attempt at formal measurement or transmission.</untranslatable_real>
    <missing_floor present="yes">A market's refusal to price goods from uncertified producers is a constructed partition that creates the necessity for the certification system it benefits from.</missing_floor>
    <inherent_instrument value="yes">The extraction is performed by a formal chart of figures; without the chart and the institution that honors it, the specific structure of the constraint vanishes.</inherent_instrument>
  </invariant_contract>

  <break_contract>
    <original_break>A system of control is revealed to be not a lie, but a statistically accurate model whose very accuracy is the instrument of harm.</original_break>
    <prior_status>LIVE</prior_status>
    <target_prior>The expectation that harm from a system must stem from its corruption or inaccuracy, not from the logical consequences of its perfect operation.</target_prior>
  </break_contract>

  <omegas>
    <omega id="generational_response">It is unresolved whether the next generation's awareness of the system's blindness constitutes a potential for rupture or is merely the final stage of acceptance before assimilation.</omega>
  </omegas>
</constraint_manifest>
```