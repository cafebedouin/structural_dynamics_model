```xml
<constraint_manifest>
  <selected>
    <constraint id="C1" name="The Static Canon" generation_order="1">
      <base_properties>
        <epsilon>0.30</epsilon>
        <suppression>0.80</suppression>
        <coordination>true</coordination>
        <asymmetric>true</asymmetric>
      </base_properties>
      <graph>
        <downstream_of>none</downstream_of>
        <feeds_into>C2</feeds_into>
      </graph>
      <character_classifications>
        <character name="Narrator (as candidate)">
          <index>
            <power>powerless</power>
            <time>biographical</time>
            <exit>trapped</exit>
            <scope>national</scope>
          </index>
          <chi>0.45</chi>
          <type>Tangled Rope</type>
        </character>
        <character name="Examiners">
          <index>
            <power>institutional</power>
            <time>biographical</time>
            <exit>identity_locked</exit>
            <scope>national</scope>
          </index>
          <chi>-0.06</chi>
          <type>Rope</type>
        </character>
        <character name="Old Magistrate">
          <index>
            <power>powerful</power>
            <time>biographical</time>
            <exit>constrained</exit>
            <scope>local</scope>
          </index>
          <chi>0.14</chi>
          <type>Rope</type>
        </character>
      </character_classifications>
      <indexical_variance>True. The canon is a coordination tool (Rope) for those administering it, but a hybrid of coordination and extraction (Tangled Rope) for those subjected to it for advancement.</indexical_variance>
      <selection_reason>This is the ultimate upstream constraint; the entire system of evaluation and legitimation is built upon the premise of a fixed, authoritative body of text.</selection_reason>
    </constraint>
    <constraint id="C2" name="The Sorting Examination" generation_order="2">
      <base_properties>
        <epsilon>0.65</epsilon>
        <suppression>0.90</suppression>
        <coordination>true</coordination>
        <asymmetric>true</asymmetric>
      </base_properties>
      <graph>
        <downstream_of>C1</downstream_of>
        <feeds_into>C3</feeds_into>
      </graph>
      <character_classifications>
        <character name="Narrator (as candidate)">
          <index>
            <power>powerless</power>
            <time>biographical</time>
            <exit>trapped</exit>
            <scope>national</scope>
          </index>
          <chi>0.98</chi>
          <type>Snare</type>
        </character>
        <character name="Narrator (as magistrate)">
          <index>
            <power>moderate</power>
            <time>biographical</time>
            <exit>constrained</exit>
            <scope>regional</scope>
          </index>
          <chi>0.59</chi>
          <type>Tangled Rope</type>
        </character>
        <character name="Examiners">
          <index>
            <power>institutional</power>
            <time>biographical</time>
            <exit>identity_locked</exit>
            <scope>national</scope>
          </index>
          <chi>-0.13</chi>
          <type>Rope</type>
        </character>
      </character_classifications>
      <indexical_variance>True. The exam is an extraction trap (Snare) for candidates, a coordination/extraction hybrid (Tangled Rope) for officials who must work with its outputs, and a pure coordination tool (Rope) for its administrators.</indexical_variance>
      <selection_reason>This is the central mechanism of the story, translating the abstract authority of the canon (C1) into a concrete social hierarchy that enables extraction (C3).</selection_reason>
    </constraint>
    <constraint id="C3" name="Legitimation of Asymmetric Burdens" generation_order="3">
      <base_properties>
        <epsilon>0.75</epsilon>
        <suppression>0.50</suppression>
        <coordination>true</coordination>
        <asymmetric>true</asymmetric>
      </base_properties>
      <graph>
        <downstream_of>C2</downstream_of>
        <feeds_into>none</feeds_into>
      </graph>
      <character_classifications>
        <character name="Farmer (subject to levy)">
          <index>
            <power>powerless</power>
            <time>biographical</time>
            <exit>trapped</exit>
            <scope>local</scope>
          </index>
          <chi>0.90</chi>
          <type>Snare</type>
        </character>
        <character name="Narrator (as magistrate)">
          <index>
            <power>moderate</power>
            <time>biographical</time>
            <exit>constrained</exit>
            <scope>regional</scope>
          </index>
          <chi>0.68</chi>
          <type>Tangled Rope</type>
        </character>
        <character name="Narrator's Family (with exemption)">
          <index>
            <power>powerful</power>
            <time>biographical</time>
            <exit>mobile</exit>
            <scope>local</scope>
          </index>
          <chi>0.36</chi>
          <type>Rope</type>
        </character>
      </character_classifications>
      <indexical_variance>True. The system that uses exam results to justify who pays a levy is a Snare for those who pay, a Tangled Rope for the official who must administer it, and a Rope for the family that benefits from the exemption.</indexical_variance>
      <selection_reason>Selected as the highest-centrality constraint. It is the ultimate purpose of the system described in the source: a seemingly meritocratic process (C2) provides the justification for a deeply asymmetric system of economic extraction.</selection_reason>
    </constraint>
  </selected>
  <deferred>
    <constraint id="C4" name="The Economic Levy">
      <hypothesis>Snare</hypothesis>
      <offstage_function>Acts as the engine of the plot, providing the non-negotiable material stakes that force all characters to participate in the examination system.</offstage_function>
    </constraint>
    <constraint id="C5" name="Uncodifiable Judgment">
      <hypothesis>Mountain</hypothesis>
      <offstage_function>Serves as the story's immutable moral and practical standard, against which the failures and inhumanity of the formal system are measured.</offstage_function>
    </constraint>
  </deferred>
  <generation_sequence>C1 → C2 → C3</generation_sequence>
  <invariant_contract>
    <untranslatable_real present="yes">The situational wisdom required for just arbitration cannot be codified into a rule without ceasing to be itself, as any rule can be applied without wisdom.</untranslatable_real>
    <missing_floor present="yes">A system for credentialing public administrators was founded on the choice to prioritize scalable, objective grading over the assessment of un-scalable, subjective judgment.</missing_floor>
    <inherent_instrument value="yes">The constraint is mediated by a standardized test scored against a fixed canon; removing the test and its scoring rubric removes the constraint.</inherent_instrument>
  </invariant_contract>
  <break_contract>
    <original_break>A story about a flawed meritocracy is not about the protagonist exposing its corruption, but about him understanding and accepting its inhuman function.</original_break>
    <prior_status>LIVE</prior_status>
    <target_prior>The stated purpose of an evaluative system is not its real purpose; its true function is revealed by the behaviors it selects for, not the values it claims to uphold.</target_prior>
  </break_contract>
  <omegas>
    <omega id="origin_of_canon">The source does not address who created the Code or the Seven Commentaries, or what process could ever alter them, treating them as a fixed external reality.</omega>
    <omega id="prefect_review">The exact standards of the prefect's reviewers are implied to be identical to the examination hall's, but this is an assumption made by the narrator, not a directly observed fact.</omega>
  </omegas>
</constraint_manifest>
```