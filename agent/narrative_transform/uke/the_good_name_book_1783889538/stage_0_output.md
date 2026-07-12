```xml
<constraint_manifest>
  <selected>
    <constraint id="C1" name="Reciprocity Web" generation_order="1">
      <base_properties>
        <epsilon>0.05</epsilon>
        <suppression>0.0</suppression>
        <coordination>true</coordination>
        <asymmetric>false</asymmetric>
      </base_properties>
      <graph>
        <downstream_of>none</downstream_of>
        <feeds_into>none</feeds_into>
      </graph>
      <character_classifications>
        <character name="Isa Wendl">
          <index>
            <power>powerful</power>
            <time>biographical</time>
            <exit>identity_locked</exit>
            <scope>local</scope>
          </index>
          <chi>0.024</chi>
          <type>Mountain</type>
        </character>
        <character name="Street Community">
          <index>
            <power>moderate</power>
            <time>biographical</time>
            <exit>constrained</exit>
            <scope>local</scope>
          </index>
          <chi>0.04</chi>
          <type>Mountain</type>
        </character>
      </character_classifications>
      <indexical_variance>false</indexical_variance>
      <selection_reason>This is the foundational, low-extraction constraint representing the community's original state. Its destruction by C2 is the story's primary dynamic.</selection_reason>
    </constraint>
    <constraint id="C2" name="Centralized Ledger System" generation_order="2">
      <base_properties>
        <epsilon>0.80</epsilon>
        <suppression>0.60</suppression>
        <coordination>true</coordination>
        <asymmetric>true</asymmetric>
      </base_properties>
      <graph>
        <downstream_of>none</downstream_of>
        <feeds_into>C3</feeds_into>
      </graph>
      <character_classifications>
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
        <character name="Keeper">
          <index>
            <power>analytical</power>
            <time>biographical</time>
            <exit>analytical</exit>
            <scope>local</scope>
          </index>
          <chi>0.736</chi>
          <type>Snare</type>
        </character>
        <character name="Verentz Grandson">
          <index>
            <power>institutional</power>
            <time>biographical</time>
            <exit>arbitrage</exit>
            <scope>local</scope>
          </index>
          <chi>-0.128</chi>
          <type>Rope</type>
        </character>
        <character name="Street Community">
          <index>
            <power>moderate</power>
            <time>biographical</time>
            <exit>constrained</exit>
            <scope>local</scope>
          </index>
          <chi>0.64</chi>
          <type>Tangled Rope</type>
        </character>
      </character_classifications>
      <indexical_variance>true</indexical_variance>
      <selection_reason>Highest centrality. This constraint is the engine of the narrative, and its type varies dramatically by index, revealing the core tensions of the source.</selection_reason>
    </constraint>
    <constraint id="C3" name="Value Conversion Harm" generation_order="3">
      <base_properties>
        <epsilon>0.90</epsilon>
        <suppression>0.20</suppression>
        <coordination>false</coordination>
        <asymmetric>true</asymmetric>
      </base_properties>
      <graph>
        <downstream_of>C2</downstream_of>
        <feeds_into>none</feeds_into>
      </graph>
      <character_classifications>
        <character name="Isa Wendl">
          <index>
            <power>powerless</power>
            <time>biographical</time>
            <exit>trapped</exit>
            <scope>local</scope>
          </index>
          <chi>1.08</chi>
          <type>Snare</type>
        </character>
        <character name="Keeper">
          <index>
            <power>analytical</power>
            <time>biographical</time>
            <exit>analytical</exit>
            <scope>local</scope>
          </index>
          <chi>0.828</chi>
          <type>Snare</type>
        </character>
        <character name="Verentz Grandson">
          <index>
            <power>institutional</power>
            <time>biographical</time>
            <exit>arbitrage</exit>
            <scope>local</scope>
          </index>
          <chi>-0.144</chi>
          <type>Rope</type>
        </character>
      </character_classifications>
      <indexical_variance>true</indexical_variance>
      <selection_reason>This is the downstream consequence of C1 and C2 interacting. It is a high-extraction Snare that captures the central tragedy of the story: the destruction of value through its measurement.</selection_reason>
    </constraint>
  </selected>

  <deferred>
    <constraint id="C4" name="Keeper's Duty">
      <hypothesis>Tangled Rope</hypothesis>
      <offstage_function>This constraint explains the narrator's compliance and inaction, framing it as a structural feature of his role rather than a personal moral failure.</offstage_function>
    </constraint>
    <constraint id="C5" name="Unwritten Exclusion">
      <hypothesis>Snare</hypothesis>
      <offstage_function>This constraint provides the inciting pressure for the story's central event, forcing characters from the informal system to engage with the formal one.</offstage_function>
    </constraint>
  </deferred>

  <generation_sequence>C1 → C2 → C3</generation_sequence>

  <invariant_contract>
    <untranslatable_real present="yes">A form of mutual trust exists that is constituted by its own non-transactional performance; to account for it is to convert it into a debt, which is a different thing.</untranslatable_real>
    <missing_floor present="yes">The system's zero-point is 'no recorded history,' forcing everyone to prove their worth in its terms and erasing all value created outside its view.</missing_floor>
    <inherent_instrument value="yes">The extraction of value and destruction of social bonds is enacted entirely through the act of recording a person's standing in a ledger; without the ledger and its score, the harm does not occur.</inherent_instrument>
  </invariant_contract>

  <break_contract>
    <original_break>The expectation that a system of accounting, if performed honestly and accurately, cannot be the primary cause of harm.</original_break>
    <prior_status>LIVE</prior_status>
    <target_prior>The belief that accurate measurement is a neutral, benevolent act that reveals pre-existing truth without altering it.</target_prior>
  </break_contract>

  <omegas>
    <omega id="resistance_potential">The analysis cannot resolve whether the community's atomization is permanent or if a threshold exists at which collective resistance to the ledger system could emerge.</omega>
  </omegas>
</constraint_manifest>
```