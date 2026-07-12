```xml
<constraint_manifest>
  <selected>
    <constraint id="C1" name="TheUnownableTrueGrain" generation_order="1">
      <base_properties>
        <epsilon>0.05</epsilon>
        <suppression>0.0</suppression>
        <coordination>false</coordination>
        <asymmetric>false</asymmetric>
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
            <exit>identity_locked</exit>
            <scope>universal</scope>
          </index>
          <chi>N/A</chi>
          <type>Mountain</type>
        </character>
        <character name="Verrel">
          <index>
            <power>moderate</power>
            <time>biographical</time>
            <exit>identity_locked</exit>
            <scope>universal</scope>
          </index>
          <chi>N/A</chi>
          <type>Mountain</type>
        </character>
      </character_classifications>
      <indexical_variance>None. This constraint is a law of nature within the narrative, affecting all characters equally and qualifying as a Mountain from all perspectives.</indexical_variance>
      <selection_reason>This constraint provides the metaphysical foundation for the central dilemma. It explains why the primary problem (C3) cannot be solved by simply creating a "correct" standard, making the story's negotiated, imperfect ethics necessary.</selection_reason>
    </constraint>
    <constraint id="C2" name="TheHiddenFoundingChoice" generation_order="2">
      <base_properties>
        <epsilon>0.2</epsilon>
        <suppression>0.05</suppression>
        <coordination>true</coordination>
        <asymmetric>true</asymmetric>
      </base_properties>
      <graph>
        <downstream_of>none</downstream_of>
        <feeds_into>C3</feeds_into>
      </graph>
      <character_classifications>
        <character name="Narrator (post-revelation)">
          <index>
            <power>analytical</power>
            <time>historical</time>
            <exit>analytical</exit>
            <scope>universal</scope>
          </index>
          <chi>0.23</chi>
          <type>Mountain</type>
        </character>
        <character name="Verrel">
          <index>
            <power>analytical</power>
            <time>historical</time>
            <exit>analytical</exit>
            <scope>universal</scope>
          </index>
          <chi>0.23</chi>
          <type>Mountain</type>
        </character>
        <character name="Merchants">
          <index>
            <power>powerless</power>
            <time>immediate</time>
            <exit>trapped</exit>
            <scope>local</scope>
          </index>
          <chi>0.24</chi>
          <type>Mountain</type>
        </character>
      </character_classifications>
      <indexical_variance>Low. For those unaware, it is an invisible part of the background. For those who are made aware (Narrator, Verrel), it is revealed as a constructed but immutable fact about their world, a Mountain.</indexical_variance>
      <selection_reason>This constraint is the direct upstream enabler of the story's central conflict. It establishes the principle that all standards are built on an arbitrary, hidden choice, which is the precondition for a standard being co-opted for extraction.</selection_reason>
    </constraint>
    <constraint id="C3" name="TheWeaponizedStandard" generation_order="3">
      <base_properties>
        <epsilon>0.8</epsilon>
        <suppression>0.8</suppression>
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
            <power>moderate</power>
            <time>biographical</time>
            <exit>identity_locked</exit>
            <scope>local</scope>
          </index>
          <chi>0.64</chi>
          <type>Tangled Rope</type>
        </character>
        <character name="King/Guild">
          <index>
            <power>institutional</power>
            <time>generational</time>
            <exit>arbitrage</exit>
            <scope>regional</scope>
          </index>
          <chi>-0.144</chi>
          <type>Rope</type>
        </character>
        <character name="Merchants">
          <index>
            <power>powerless</power>
            <time>immediate</time>
            <exit>trapped</exit>
            <scope>local</scope>
          </index>
          <chi>0.96</chi>
          <type>Snare</type>
        </character>
      </character_classifications>
      <indexical_variance>High. The same constraint is a Snare for the powerless (Merchants, Apprentice), a Tangled Rope for the complicit-but-aware (Verrel), and a Rope for the institutional beneficiary (King/Guild). This variance is the engine of the story's core ethical conflict.</indexical_variance>
      <selection_reason>This is the highest-centrality constraint, representing the primary, operationalized injustice of the story. Its indexical variance drives the narrative by creating different lived realities and ethical burdens for each character.</selection_reason>
    </constraint>
  </selected>

  <deferred>
    <constraint id="C4" name="SystemPreservationImperative">
      <hypothesis>Tangled Rope</hypothesis>
      <offstage_function>This constraint provides the justification for complicity. The need to "keep the port fed" functions as a background pressure that prevents characters from refusing to participate in the flawed system, forcing them to find an ethical solution within it rather than outside of it.</offstage_function>
    </constraint>
    <constraint id="C5" name="TheCostOfPrecision">
      <hypothesis>Mountain</hypothesis>
      <offstage_function>The physical cold of the assay house acts as a constant, sensory reminder that accuracy and measurement have a non-negotiable physical cost, grounding the abstract, ethical conflicts in a tangible, bodily experience.</offstage_function>
    </constraint>
  </deferred>

  <generation_sequence>C1 → C2 → C3</generation_sequence>

  <invariant_contract>
    <untranslatable_real present="yes">A state of perfect equilibrium is real but ceases to exist the moment an interested party attempts to measure, possess, or use it for their own ends.</untranslatable_real>
    <missing_floor present="yes">Any system of value is built upon a foundational act of setting a zero-point, a choice made by a specific actor for a specific purpose which is then hidden to make the system appear objective.</missing_floor>
    <inherent_instrument value="yes">The constraint is mediated by a certified measurement; the harm occurs through the official act of comparing against a compromised reference and declaring it true.</inherent_instrument>
  </invariant_contract>

  <omegas>
    <omega id="origin_of_flaw">The source does not resolve whether the standard's inaccuracy is the result of intentional malice for extractive purposes or the slow, uncorrected drift of a once-honest measure that has since been exploited.</omega>
  </omegas>
</constraint_manifest>
```