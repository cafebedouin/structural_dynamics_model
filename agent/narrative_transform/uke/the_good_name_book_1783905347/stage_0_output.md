```xml
<constraint_manifest>
  <selected>
    <constraint id="C1" name="Value Incommensurability" generation_order="1">
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
      <character_classifications>
        <character name="Keeper">
          <index>
            <power>analytical</power>
            <time>biographical</time>
            <exit>analytical</exit>
            <scope>local</scope>
          </index>
          <chi>0.0</chi>
          <type>Mountain</type>
        </character>
        <character name="Isa Wendl">
          <index>
            <power>powerless</power>
            <time>biographical</time>
            <exit>trapped</exit>
            <scope>local</scope>
          </index>
          <chi>0.0</chi>
          <type>Mountain</type>
        </character>
        <character name="Verentz's Grandson">
          <index>
            <power>institutional</power>
            <time>generational</time>
            <exit>arbitrage</exit>
            <scope>local</scope>
          </index>
          <chi>0.0</chi>
          <type>Mountain</type>
        </character>
      </character_classifications>
      <indexical_variance>none</indexical_variance>
      <selection_reason>This constraint is the foundational, unchangeable law (Mountain) that makes the conflict between the story's two value systems irreconcilable and tragic. It is the most upstream element.</selection_reason>
    </constraint>
    <constraint id="C2" name="Ideology of Objective Measurement" generation_order="2">
      <base_properties>
        <epsilon>0.75</epsilon>
        <suppression>0.7</suppression>
        <coordination>true</coordination>
        <asymmetric>true</asymmetric>
      </base_properties>
      <graph>
        <downstream_of>none</downstream_of>
        <feeds_into>C3</feeds_into>
      </graph>
      <character_classifications>
        <character name="Keeper">
          <index>
            <power>analytical</power>
            <time>biographical</time>
            <exit>constrained</exit>
            <scope>local</scope>
          </index>
          <chi>0.69</chi>
          <type>Tangled Rope</type>
        </character>
        <character name="Isa Wendl">
          <index>
            <power>powerless</power>
            <time>biographical</time>
            <exit>trapped</exit>
            <scope>local</scope>
          </index>
          <chi>0.9</chi>
          <type>Snare</type>
        </character>
        <character name="Verentz's Grandson">
          <index>
            <power>institutional</power>
            <time>generational</time>
            <exit>arbitrage</exit>
            <scope>local</scope>
          </index>
          <chi>-0.12</chi>
          <type>Rope</type>
        </character>
      </character_classifications>
      <indexical_variance>Keeper experiences this as a Tangled Rope (it gives his work meaning but he sees the harm), Isa as a Snare (it justifies the system that traps her), and the Grandson as a Rope (a useful tool for legitimizing his business).</indexical_variance>
      <selection_reason>This ideological constraint enables the primary economic constraint by framing it as neutral and inevitable. It has high centrality and its observable (perception/justification) is distinct from the other selected constraints.</selection_reason>
    </constraint>
    <constraint id="C3" name="Centralized Reputation Ledger" generation_order="3">
      <base_properties>
        <epsilon>0.8</epsilon>
        <suppression>0.1</suppression>
        <coordination>true</coordination>
        <asymmetric>true</asymmetric>
      </base_properties>
      <graph>
        <downstream_of>C1, C2</downstream_of>
        <feeds_into>none</feeds_into>
      </graph>
      <character_classifications>
        <character name="Keeper">
          <index>
            <power>analytical</power>
            <time>biographical</time>
            <exit>constrained</exit>
            <scope>local</scope>
          </index>
          <chi>0.736</chi>
          <type>Snare</type>
        </character>
        <character name="Isa Wendl">
          <index>
            <power>powerless</power>
            <time>biographical</time>
            <exit>trapped</exit>
            <scope>local</scope>
          </index>
          <chi>0.96</chi>
          <type>Snare</type>
        </character>
        <character name="Verentz's Grandson">
          <index>
            <power>institutional</power>
            <time>generational</time>
            <exit>arbitrage</exit>
            <scope>local</scope>
          </index>
          <chi>-0.128</chi>
          <type>Rope</type>
        </character>
        <character name="Isa Wendl (post-entry)">
          <index>
            <power>moderate</power>
            <time>biographical</time>
            <exit>identity_locked</exit>
            <scope>local</scope>
          </index>
          <chi>0.64</chi>
          <type>Tangled Rope</type>
        </character>
      </character_classifications>
      <indexical_variance>From the powerless and analytical views, the system is a Snare. For its owner, it is a Rope. For a successful participant, it becomes a Tangled Rope, providing real coordination value at a high, extractive cost.</indexical_variance>
      <selection_reason>This is the central, downstream mechanism of the story, a Tangled Rope with the highest centrality score, where the effects of the other constraints are made manifest.</selection_reason>
    </constraint>
  </selected>

  <deferred>
    <constraint id="C4" name="Informal Mutual Aid Network">
      <hypothesis>Rope</hypothesis>
      <offstage_function>Acts as the ghost of value; its prior existence and subsequent destruction provides the core contrast that makes the tragedy of the selected constraints visible and meaningful.</offstage_function>
    </constraint>
    <constraint id="C5" name="Professional Role Obligations">
      <hypothesis>Rope</hypothesis>
      <offstage_function>Shapes the narrator's fatalistic voice and provides the analytical viewpoint, creating the psychological texture of someone trapped within and enabling a system they understand to be destructive.</offstage_function>
    </constraint>
    <constraint id="C6" name="System Expansion Mandate">
      <hypothesis>Tangled Rope</hypothesis>
      <offstage_function>Serves as the story's conclusion, demonstrating that the ledger's logic is inherently expansionist and will replicate the local tragedy on a larger scale.</offstage_function>
    </constraint>
  </deferred>

  <generation_sequence>C1 → C2 → C3</generation_sequence>

  <invariant_contract>
    <untranslatable_real present="yes">A form of mutual trust exists only in its direct, unmediated performance; the moment it is measured, named, or converted into a debt to be settled, it ceases to be itself.</untranslatable_real>
    <missing_floor present="yes">The choice to quantify a person's worth on a single numerical scale is treated as a neutral act of recording, but it is a founding choice that creates the reality it claims only to measure.</missing_floor>
    <inherent_instrument value="yes">The extraction is performed by assigning a numerical score based on recorded transactions; without the ledger and its score, the specific mechanism of control and profit does not exist.</inherent_instrument>
  </invariant_contract>

  <break_contract>
    <original_break>The expectation that a system of perfect, objective record-keeping is a moral good and leads to a just outcome.</original_break>
    <prior_status>LIVE</prior_status>
    <target_prior>A system that is technically accurate and internally consistent can still be fundamentally destructive to human value.</target_prior>
  </break_contract>

  <omegas>
    <omega id="community_acquiescence">The analysis does not fully resolve why the community atomizes and adopts the new system's logic so quickly, rather than mounting collective resistance or creating alternatives.</omega>
  </omegas>
</constraint_manifest>
```