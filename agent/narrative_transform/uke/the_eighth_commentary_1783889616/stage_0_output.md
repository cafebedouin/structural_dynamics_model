```xml
<constraint_manifest>
  <selected>
    <constraint id="C1" name="The Credentialed Exemption" generation_order="1">
      <base_properties>
        <epsilon>0.80</epsilon>
        <suppression>0.70</suppression>
        <coordination>false</coordination>
        <asymmetric>true</asymmetric>
      </base_properties>
      <graph>
        <downstream_of>none</downstream_of>
        <feeds_into>C2</feeds_into>
      </graph>
      <character_classifications>
        <character name="Farmers">
          <index>
            <power>powerless</power>
            <time>biographical</time>
            <exit>trapped</exit>
            <scope>local</scope>
          </index>
          <chi>0.96</chi>
          <type>Snare</type>
        </character>
        <character name="Narrator (as candidate)">
          <index>
            <power>powerless</power>
            <time>biographical</time>
            <exit>trapped</exit>
            <scope>regional</scope>
          </index>
          <chi>1.08</chi>
          <type>Snare</type>
        </character>
        <character name="The System">
          <index>
            <power>institutional</power>
            <time>generational</time>
            <exit>arbitrage</exit>
            <scope>national</scope>
          </index>
          <chi>-0.16</chi>
          <type>Rope</type>
        </character>
      </character_classifications>
      <indexical_variance>High. The constraint is an existential Snare for those subject to it, but a functional Rope for the institution that benefits from the revenue stream and social sorting.</indexical_variance>
      <selection_reason>This is the foundational economic pressure. It is the most upstream constraint, creating the high-stakes demand that powers the entire system.</selection_reason>
    </constraint>
    <constraint id="C2" name="The Test of Stillness" generation_order="2">
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
            <scope>regional</scope>
          </index>
          <chi>0.95</chi>
          <type>Snare</type>
        </character>
        <character name="The System">
          <index>
            <power>institutional</power>
            <time>generational</time>
            <exit>arbitrage</exit>
            <scope>national</scope>
          </index>
          <chi>-0.14</chi>
          <type>Rope</type>
        </character>
      </character_classifications>
      <indexical_variance>High. For candidates, it is a Snare that extracts wealth, time, and authentic thought. For the system, it is an efficient Rope for sorting thousands of people into a stable, predictable hierarchy.</indexical_variance>
      <selection_reason>This is the central sorting mechanism. It translates the economic pressure of C1 into a specific set of required behaviors and filters the population accordingly.</selection_reason>
    </constraint>
    <constraint id="C3" name="The Sanctioned Reading" generation_order="3">
      <base_properties>
        <epsilon>0.60</epsilon>
        <suppression>0.60</suppression>
        <coordination>true</coordination>
        <asymmetric>true</asymmetric>
      </base_properties>
      <graph>
        <downstream_of>C2</downstream_of>
        <feeds_into>none</feeds_into>
      </graph>
      <character_classifications>
        <character name="Narrator (as magistrate)">
          <index>
            <power>moderate</power>
            <time>biographical</time>
            <exit>identity_locked</exit>
            <scope>local</scope>
          </index>
          <chi>0.48</chi>
          <type>Tangled Rope</type>
        </character>
        <character name="The System">
          <index>
            <power>institutional</power>
            <time>generational</time>
            <exit>arbitrage</exit>
            <scope>national</scope>
          </index>
          <chi>-0.12</chi>
          <type>Rope</type>
        </character>
      </character_classifications>
      <indexical_variance>Moderate. For the magistrate, this is a Tangled Rope: it provides the genuine coordination needed for a judgment to be upheld, but extracts his ability to reason from first principles. For the system, it is a pure Rope ensuring administrative uniformity.</indexical_variance>
      <selection_reason>Highest centrality score. This is the downstream consequence where the logic of the system (C2) collides with the reality of governance, forcing the protagonist into a state of permanent compromise.</selection_reason>
    </constraint>
  </selected>

  <deferred>
    <constraint id="C4" name="The Frozen Canon">
      <hypothesis>False Mountain</hypothesis>
      <offstage_function>This constraint provides the philosophical justification for the system's rigidity. It acts as background pressure by making the rules of C2 and C3 seem natural and unchangeable, foreclosing any attempt at systemic reform.</offstage_function>
    </constraint>
    <constraint id="C5" name="The Smuggled Judgment">
      <hypothesis>Coping Mechanism</hypothesis>
      <offstage_function>This is not a constraint but a behavioral adaptation to C3. It functions as a narrative observable of the cost C3 imposes, showing the personal effort required to reconcile justice with legality within the system.</offstage_function>
    </constraint>
  </deferred>

  <generation_sequence>C1 → C2 → C3</generation_sequence>

  <invariant_contract>
    <untranslatable_real present="yes">The capacity for situated judgment, which perceives the human reality of a situation, cannot be codified into a rule without ceasing to be itself.</untranslatable_real>
    <missing_floor present="yes">A founding choice arbitrarily links performance in a scholastic domain to material status in an unrelated economic domain, and the system treats this link as a natural law.</missing_floor>
    <inherent_instrument value="yes">The system's extraction is mediated by a formal score, and its enforcement is mediated by a formally documented proceeding; removing the instruments of measurement and documentation would collapse the constraint.</inherent_instrument>
  </invariant_contract>

  <break_contract>
    <original_break>A story about an individual succeeding within a system of merit is revealed to be about the system's success in making that individual its perfect instrument.</original_break>
    <prior_status>LIVE</prior_status>
    <target_prior>Success within a system designed for legibility does not grant one the power to change it; it makes one a perfect transmitter of its logic.</target_prior>
  </break_contract>

  <omegas>
    <omega id="bargain_calculus">The analysis assumes the narrator's assessment of the "bargain" is stable. It cannot resolve whether this equilibrium is sustainable or if the internal cost of smuggling justice will lead to personal or systemic rupture over a longer time horizon.</omega>
  </omegas>
</constraint_manifest>
```