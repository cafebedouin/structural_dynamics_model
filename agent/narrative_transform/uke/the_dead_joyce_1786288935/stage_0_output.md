```xml
<constraint_manifest>
  <selected>
    <constraint id="C1" name="The Unalterable Past" generation_order="1">
      <base_properties>
        <epsilon>0.10</epsilon>
        <suppression>0.00</suppression>
        <coordination>false</coordination>
        <asymmetric>false</asymmetric>
      </base_properties>
      <graph>
        <downstream_of>none</downstream_of>
        <feeds_into>none</feeds_into>
      </graph>
      <character_classifications>
        <character name="Gabriel Conroy">
          <index>
            <power>moderate</power>
            <time>biographical</time>
            <exit>trapped</exit>
            <scope>universal</scope>
          </index>
          <chi>0.10</chi>
          <type>Mountain</type>
        </character>
        <character name="Gretta Conroy">
          <index>
            <power>powerless</power>
            <time>biographical</time>
            <exit>identity_locked</exit>
            <scope>universal</scope>
          </index>
          <chi>0.15</chi>
          <type>Mountain</type>
        </character>
      </character_classifications>
      <indexical_variance>none. This constraint is a background fact of existence that is immutable for all characters within the narrative's frame, classifying as a Mountain regardless of index.</indexical_variance>
      <selection_reason>This constraint is the external, unchangeable reality that ultimately irrupts into and invalidates the constructed social and personal realities of the other constraints. It is the story's foundational real.</selection_reason>
    </constraint>
    <constraint id="C2" name="Compulsory Social Performance" generation_order="2">
      <base_properties>
        <epsilon>0.70</epsilon>
        <suppression>0.65</suppression>
        <coordination>true</coordination>
        <asymmetric>true</asymmetric>
      </base_properties>
      <graph>
        <downstream_of>none</downstream_of>
        <feeds_into>C3</feeds_into>
      </graph>
      <character_classifications>
        <character name="Gabriel Conroy">
          <index>
            <power>moderate</power>
            <time>biographical</time>
            <exit>identity_locked</exit>
            <scope>local</scope>
          </index>
          <chi>0.56</chi>
          <type>Tangled Rope</type>
        </character>
        <character name="Lily">
          <index>
            <power>powerless</power>
            <time>biographical</time>
            <exit>trapped</exit>
            <scope>local</scope>
          </index>
          <chi>0.84</chi>
          <type>Snare</type>
        </character>
        <character name="Miss Kate &amp; Miss Julia Morkan">
          <index>
            <power>moderate</power>
            <time>biographical</time>
            <exit>identity_locked</exit>
            <scope>local</scope>
          </index>
          <chi>0.56</chi>
          <type>Tangled Rope</type>
        </character>
      </character_classifications>
      <indexical_variance>Yes. For the hosts and their primary guest (Gabriel), this is a Tangled Rope that provides the coordination for the party but extracts significant emotional energy. For the servant (Lily), the stakes are higher and the agency lower, making it an extractive Snare.</indexical_variance>
      <selection_reason>This is the highest-centrality constraint, representing the entire social milieu of the story. It establishes the rules, roles, and pressures that shape the characters' behaviors and inflate the protagonist's ego, setting up the final conflict.</selection_reason>
    </constraint>
    <constraint id="C3" name="Presumed Marital Dominion" generation_order="3">
      <base_properties>
        <epsilon>0.95</epsilon>
        <suppression>0.75</suppression>
        <coordination>true</coordination>
        <asymmetric>true</asymmetric>
      </base_properties>
      <graph>
        <downstream_of>C2</downstream_of>
        <feeds_into>none</feeds_into>
      </graph>
      <character_classifications>
        <character name="Gabriel Conroy">
          <index>
            <power>institutional</power>
            <time>biographical</time>
            <exit>identity_locked</exit>
            <scope>local</scope>
          </index>
          <chi>-0.15</chi>
          <type>Rope</type>
        </character>
        <character name="Gretta Conroy">
          <index>
            <power>powerless</power>
            <time>biographical</time>
            <exit>trapped</exit>
            <scope>local</scope>
          </index>
          <chi>1.14</chi>
          <type>Snare</type>
        </character>
      </character_classifications>
      <indexical_variance>Yes. From the husband's institutional position, the marriage is a Rope—a coordination mechanism meant to provide him with intimacy and affirmation. From the wife's powerless position, his expectation of total emotional and physical access is a Snare.</indexical_variance>
      <selection_reason>This constraint is the locus of the story's central conflict. It is the direct downstream consequence of C2's ego-inflation and the structure that is shattered by the revelation of C1, making it the critical point of failure.</selection_reason>
    </constraint>
  </selected>
  <deferred>
    <constraint id="C4" name="Political Identity Test">
      <hypothesis>Snare</hypothesis>
      <offstage_function>Acts as a destabilizing force within the environment of C2, heightening Gabriel's insecurity and demonstrating that the social performance is fragile and subject to external pressures.</offstage_function>
    </constraint>
    <constraint id="C5" name="Polite Insolvency">
      <hypothesis>Tangled Rope</hypothesis>
      <offstage_function>Provides the unspoken economic pressure that makes the successful performance of C2 so critical for the hosts, adding a layer of pathos and desperation to the enforced hospitality.</offstage_function>
    </constraint>
  </deferred>
  <generation_sequence>C1 → C2 → C3</generation_sequence>
  <invariant_contract>
    <untranslatable_real present="yes" primary="yes">A past grief, held without performance or social transaction, invalidates a present built on sentimental self-regard.</untranslatable_real>
    <missing_floor present="yes" primary="no">A person's interior life is presumed to be the property of their partner, rendering its true autonomy a disruptive anomaly.</missing_floor>
    <inherent_instrument value="no">The constraints are enacted through diffuse social norms and internalized roles, not through a specific, removable measuring device.</inherent_instrument>
  </invariant_contract>
  <break_contract>
    <original_break>A narrative of social maneuvering and intellectual insecurity is expected to resolve within that frame, not pivot to a revelation that renders the protagonist's entire world trivial.</original_break>
    <prior_status>DEAD</prior_status>
    <target_prior>A critique of a single person's ego is expected to affirm the stable, authentic self of another; instead, the story reveals that all personal identity is a fragile construct dissolving into a shared, impersonal mortality.</target_prior>
  </break_contract>
  <omegas>
    <omega id="final_state">The analysis cannot resolve whether the protagonist's final state is one of genuine, empathetic connection to others or a final, aestheticized act of sentimental self-absorption that incorporates mortality itself into his worldview.</omega>
  </omegas>
</constraint_manifest>
```