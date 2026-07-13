```xml
<constraint_manifest>
  <selected>
    <constraint id="C1" name="The Levy Exemption" generation_order="1">
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
        <character name="Non-passing Farmer">
          <index>
            <power>powerless</power>
            <time>biographical</time>
            <exit>trapped</exit>
            <scope>local</scope>
          </index>
          <chi>1.08</chi>
          <type>Snare</type>
        </character>
        <character name="Narrator (as degree-holder)">
          <index>
            <power>institutional</power>
            <time>biographical</time>
            <exit>mobile</exit>
            <scope>local</scope>
          </index>
          <chi>-0.14</chi>
          <type>Rope</type>
        </character>
      </character_classifications>
      <indexical_variance>True. The constraint is a Snare for those who pay the levy and a Rope (pure benefit) for those exempted from it.</indexical_variance>
      <selection_reason>This is the primary upstream driver of the entire system, creating the high-stakes economic pressure that gives the other constraints their power.</selection_reason>
    </constraint>
    <constraint id="C2" name="The Examination System" generation_order="2">
      <base_properties>
        <epsilon>0.70</epsilon>
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
          <chi>1.05</chi>
          <type>Snare</type>
        </character>
        <character name="Examiners">
          <index>
            <power>institutional</power>
            <time>biographical</time>
            <exit>identity_locked</exit>
            <scope>national</scope>
          </index>
          <chi>-0.14</chi>
          <type>Rope</type>
        </character>
      </character_classifications>
      <indexical_variance>True. The system is a Snare for the candidates whose resources it consumes, but a Rope for the institution that administers it and benefits from the sorting function.</indexical_variance>
      <selection_reason>This is the central sorting mechanism of the narrative, connecting the economic pressure of C1 to the ideological pressure of C3. It has the highest centrality in the constraint graph.</selection_reason>
    </constraint>
    <constraint id="C3" name="The Sanctioned Canon" generation_order="3">
      <base_properties>
        <epsilon>0.60</epsilon>
        <suppression>0.90</suppression>
        <coordination>true</coordination>
        <asymmetric>true</asymmetric>
      </base_properties>
      <graph>
        <downstream_of>C2</downstream_of>
        <feeds_into>none</feeds_into>
      </graph>
      <character_classifications>
        <character name="Narrator (as candidate)">
          <index>
            <power>powerless</power>
            <time>biographical</time>
            <exit>trapped</exit>
            <scope>national</scope>
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
          <chi>0.54</chi>
          <type>Tangled Rope</type>
        </character>
      </character_classifications>
      <indexical_variance>True. For a candidate, the demand for textual conformity is a Snare. For a practicing official, it is a Tangled Rope: a tool for getting just outcomes approved, but one that extracts intellectual honesty.</indexical_variance>
      <selection_reason>This constraint represents the ideological mechanism of the system. It is the most distinct downstream consequence of the examination and demonstrates the system's logic extending from qualification into practice.</selection_reason>
    </constraint>
  </selected>
  <deferred>
    <constraint id="C4" name="Covert Judgment">
      <hypothesis>Tangled Rope</hypothesis>
      <offstage_function>This represents the informal, unrewarded labor required to make the formal system functional, showing how agents create pockets of local justice within a rigid, extractive structure.</offstage_function>
    </constraint>
    <constraint id="C5" name="The Review Process">
      <hypothesis>Tangled Rope</hypothesis>
      <offstage_function>This serves as the enforcement loop, ensuring the logic of C2 and C3 persists beyond the initial examination and governs the daily work of officials, thus reinforcing the entire system.</offstage_function>
    </constraint>
  </deferred>
  <generation_sequence>C1 → C2 → C3</generation_sequence>
  <invariant_contract>
    <untranslatable_real present="yes">The quality of judgment that perceives and responds to the specific, living context of a situation cannot be codified into a rule without being destroyed, because any codification becomes a template for mimicry rather than a guide to perception.</untranslatable_real>
    <missing_floor present="yes">The decision to link a group's economic survival to its performance within a closed textual system is a founding partition that the system treats as natural, creating a zero-sum conflict that the system exists to administer rather than solve.</missing_floor>
    <inherent_instrument value="yes">The extraction runs through a formal scoring and ranking process based on a closed canon of texts; removing the examination and its sanctioned answers would remove the constraint.</inherent_instrument>
  </invariant_contract>
  <break_contract>
    <original_break>A system that appears meritocratic is not broken or corrupt; it is functioning perfectly to measure obedience instead of the quality it purports to select for.</original_break>
    <prior_status>LIVE</prior_status>
    <target_prior>The expectation that an institution's stated purpose is its actual function; the break reveals that a system can be perfectly rational while pursuing a hidden, unstated goal.</target_prior>
  </break_contract>
  <omegas>
    <omega id="sustainability">The analysis cannot resolve whether the practice of "smuggling" justice can be sustained over generations or if it is an unstable solution that will eventually be extinguished by the formal system.</omega>
  </omegas>
</constraint_manifest>
```