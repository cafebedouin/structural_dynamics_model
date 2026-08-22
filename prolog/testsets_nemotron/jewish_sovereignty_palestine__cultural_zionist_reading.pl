% ============================================================================
% CONSTRAINT STORY: jewish_sovereignty_palestine__cultural_zionist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jewish_sovereignty_palestine__cultural_zionist_reading, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

% --- Constraint Identity Rule (DP-001: ε-Invariance) ---
% Each constraint story must have a single, stable base extractiveness (ε).
% If changing the observable used to evaluate this constraint would change ε,
% you are looking at two distinct constraints. Write separate .pl files for
% each, link them with affects_constraint/2, and document the relationship
% in both files' narrative context sections.
%
% The context tuple is CLOSED at arity 4: (P, T, E, S).
% Do not add measurement_basis, beneficiary/victim, or any other arguments.
% Linter Rule 23 enforces context/4.
%
% See: epsilon_invariance_principle.md

% --- Namespace Hooks (Required for loading) ---
:- multifile
    domain_priors:base_extractiveness/2,
    domain_priors:suppression_score/2,
    domain_priors:theater_ratio/2,
    domain_priors:requires_active_enforcement/1,
    narrative_ontology:has_sunset_clause/1,
    narrative_ontology:interval/3,
    narrative_ontology:measurement/5,
    narrative_ontology:constraint_metric/3,
    narrative_ontology:constraint_beneficiary/2,
    narrative_ontology:constraint_victim/2,
    narrative_ontology:constraint_claim/2,
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: jewish_sovereignty_palestine__cultural_zionist_reading
 *   human_readable: Cultural Zionist Vision: Jewish Spiritual Center in Palestine without Political Sovereignty
 *   domain: political_philosophy/nationalism_studies/postcolonial_theory
 *
 * SUMMARY:
 *   This constraint story models the cultural Zionist reading of the Jewish
 *   sovereignty in Palestine kernel — the vision associated with Ahad Ha'am,
 *   Martin Buber, Judah Magnes, and Brit Shalom: a Jewish spiritual and
 *   cultural center in Palestine that revitalizes Hebrew civilization without
 *   requiring a sovereign state or demographic majority. The constraint
 *   operates as a rope: it coordinates a genuine collective action problem
 *   (diaspora cultural survival + ancestral land connection) with minimal
 *   coercion, mutual benefit, and no structural suppression of alternatives.
 *   The beneficiary structure is non-zero-sum: Jewish cultural revivalists
 *   gain a living Hebrew center; Palestinian Arab inhabitants retain their
 *   land, autonomy, and political rights as co-inhabitants. Extractiveness is
 *   low (0.18) because the arrangement does not require displacement,
 *   expropriation, or political subordination. Suppression is low (0.12)
 *   because the cultural project depends on voluntary immigration and
 *   institution-building, not coercion. Theater ratio is moderate (0.25)
 *   because the cultural frame was increasingly performed by actors
 *   (including the Zionist leadership) who privately pursued sovereignty —
 *   the cultural rhetoric became a cover for the political project. The
 *   interval 1896-1948 spans from Ahad Ha'am's first visit to Palestine
 *   through the establishment of the State of Israel, which foreclosed this
 *   reading politically. The end-state extractiveness (0.18) reflects the
 *   reading's *structural* profile, not the historical outcome — the
 *   constraint is evaluated by its own internal logic, not by what replaced
 *   it.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jewish_sovereignty_palestine__cultural_zionist_reading, 0.18).
domain_priors:suppression_score(jewish_sovereignty_palestine__cultural_zionist_reading, 0.12).
domain_priors:theater_ratio(jewish_sovereignty_palestine__cultural_zionist_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__cultural_zionist_reading, extractiveness, 0.18).
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__cultural_zionist_reading, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__cultural_zionist_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__cultural_zionist_reading, accessibility_collapse, 0.15).
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__cultural_zionist_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jewish_sovereignty_palestine__cultural_zionist_reading, rope).
narrative_ontology:human_readable(jewish_sovereignty_palestine__cultural_zionist_reading, "Cultural Zionist Vision: Jewish Spiritual Center in Palestine without Political Sovereignty").
narrative_ontology:topic_domain(jewish_sovereignty_palestine__cultural_zionist_reading, "political_philosophy/nationalism_studies/postcolonial_theory").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jewish_sovereignty_palestine__cultural_zionist_reading, '95750259-d623-4212-9435-4c5e55ee7d61').
narrative_ontology:cs_kernel_codification('95750259-d623-4212-9435-4c5e55ee7d61', distributed).
narrative_ontology:cs_authority_grounding('95750259-d623-4212-9435-4c5e55ee7d61', lineage).
narrative_ontology:cs_interpretation_layer_present('95750259-d623-4212-9435-4c5e55ee7d61').
narrative_ontology:cs_reading_relation('95750259-d623-4212-9435-4c5e55ee7d61', jewish_sovereignty_palestine__liberal_nationalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('95750259-d623-4212-9435-4c5e55ee7d61', jewish_sovereignty_palestine__religious_zionist_reading, coexists_with).
narrative_ontology:cs_reading_relation('95750259-d623-4212-9435-4c5e55ee7d61', jewish_sovereignty_palestine__settler_colonial_reading, forecloses).
narrative_ontology:cs_reading_relation('95750259-d623-4212-9435-4c5e55ee7d61', jewish_sovereignty_palestine__post_zionist_reading, influences).
narrative_ontology:cs_axiom('95750259-d623-4212-9435-4c5e55ee7d61', foundational, jewish_cultural_vitality_without_sovereignty).
narrative_ontology:cs_axiom_status(jewish_cultural_vitality_without_sovereignty, holdable).
narrative_ontology:cs_axiom_grounding('95750259-d623-4212-9435-4c5e55ee7d61', jewish_cultural_vitality_without_sovereignty, deontological).
narrative_ontology:cs_axiom('95750259-d623-4212-9435-4c5e55ee7d61', foundational, shared_homeland_non_zero_sum).
narrative_ontology:cs_axiom_status(shared_homeland_non_zero_sum, holdable).
narrative_ontology:cs_axiom_grounding('95750259-d623-4212-9435-4c5e55ee7d61', shared_homeland_non_zero_sum, deontological).
narrative_ontology:cs_axiom('95750259-d623-4212-9435-4c5e55ee7d61', secondary, hebrew_culture_as_spiritual_not_political_nationhood).
narrative_ontology:cs_axiom_status(hebrew_culture_as_spiritual_not_political_nationhood, holdable).
narrative_ontology:cs_axiom_grounding('95750259-d623-4212-9435-4c5e55ee7d61', hebrew_culture_as_spiritual_not_political_nationhood, deontological).
narrative_ontology:cs_reference_frame('95750259-d623-4212-9435-4c5e55ee7d61', ahad_haam_spiritual_center).
narrative_ontology:cs_drift_state('95750259-d623-4212-9435-4c5e55ee7d61', post_1948_statehood, gap(authority_erosion, severe, false)).
narrative_ontology:cs_created_at('95750259-d623-4212-9435-4c5e55ee7d61', '').
narrative_ontology:cs_kernel_id(jewish_sovereignty_palestine__cultural_zionist_reading, jewish_sovereignty_palestine).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jewish_sovereignty_palestine__cultural_zionist_reading, jewish_cultural_revivalists).
narrative_ontology:constraint_beneficiary(jewish_sovereignty_palestine__cultural_zionist_reading, palestinian_arab_inhabitants).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(jewish_sovereignty_palestine__cultural_zionist_reading, palestinian_arab_inhabitants).
narrative_ontology:constraint_vindicates(jewish_sovereignty_palestine__cultural_zionist_reading, cultural_autonomy_without_statehood).
narrative_ontology:constraint_vindicates(jewish_sovereignty_palestine__cultural_zionist_reading, shared_homeland_non_zero_sum).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Seek a vibrant Hebrew cultural center in Palestine — language, arts, education, communal life — without requiring political sovereignty or demographic dominance. Their project flourishes through immigration, institution-building, and cultural production. They can pursue cultural vitality elsewhere (diaspora centers, other geographies) but choose Palestine for its historical resonance; exit is costly but feasible.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__cultural_zionist_reading, jewish_cultural_revivalists, beneficiary,
    organized, generational, mobile, global).

% Experience Jewish cultural immigration as a demographic and cultural shift in their homeland. Under this reading, they are co-inhabitants in a shared cultural space, not displaced subjects — they retain their own communal autonomy, land, and political rights. They bear transition costs (demographic change, cultural friction) but are not structurally targeted for removal. Exit is constrained by attachment to land and lack of alternatives.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__cultural_zionist_reading, palestinian_arab_inhabitants, beneficiary,
    moderate, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(jewish_sovereignty_palestine__cultural_zionist_reading, palestinian_arab_inhabitants, payer).

% The organized Zionist movement (WZO, Jewish Agency) that channels immigration, settlement, and institution-building. Under this reading, they administer the cultural project but do not extract rents from it; their authority derives from coordinating a voluntary cultural revival. They could pivot to political sovereignty (and historically did), but the reading describes the moment they *did not*.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__cultural_zionist_reading, zionist_institutional_leadership, agenda_setter,
    institutional, generational, arbitrage, global).

% The mandatory power governing Palestine 1920-1948. They observe and regulate Jewish immigration and land purchase. Their policy (Balfour Declaration, White Papers) oscillates between facilitating a 'Jewish national home' and limiting its scope. They are neither beneficiaries nor payers of the cultural project but hold the coercive apparatus that shapes its conditions.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__cultural_zionist_reading, british_mandate_authorities, observer,
    institutional, biographical, analytical, regional).

% Advocates of Jewish *political* self-determination and statehood (Ben-Gurion, mainstream Labor Zionism). They share the cultural project but insist it requires sovereign statehood for security and normalization. Under the cultural reading, their demand for sovereignty is the excluded alternative that would convert the rope into a tangled_rope or snare. They are present in the room but their core claim is not instantiated in *this* constraint.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__cultural_zionist_reading, liberal_nationalist_zionists, excluded,
    organized, generational, mobile, global).

% Political representatives of the Palestinian Arab national movement (Husseini, Nashashibi factions). They reject any Jewish national home as a violation of Arab self-determination. Under the cultural reading, their objection is to *political* sovereignty claims; a purely cultural presence *might* be negotiable, but they are excluded from the cultural-zionist frame because that frame does not address political representation.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__cultural_zionist_reading, palestinian_nationalist_leadership, excluded,
    organized, generational, constrained, regional).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Creates a shared cultural space where Jewish spiritual renaissance and Palestinian Arab communal life coexist without either requiring the other's political subordination. Solves the coordination problem of how a diaspora people can rebuild a living national culture in its ancestral land without displacing the existing inhabitants — by decoupling cultural vitality from sovereign control.
% TRANSFER_FUNCTION: Moves cultural capital, immigration flows, philanthropic resources, and institutional energy from global Jewish diaspora into Palestinian cultural ecology. No material extraction from Palestinians; the 'cost' they bear is demographic-cultural presence, not expropriation. The transfer is additive (new institutions, Hebrew language revival, agricultural innovation) rather than subtractive.
% ABSENT_VOICES: The liberal nationalist Zionists (who demand statehood) and the Palestinian nationalist leadership (who reject any Jewish national project) are the excluded voices. The former are present in the Zionist movement but their sovereignist claim is not part of *this* constraint; the latter are excluded because the cultural reading does not solve the political representation question. Both would object: the first that culture without sovereignty is defenseless; the second that any Jewish national project is a wedge for displacement.
% DISAPPEARANCE_RATIONALE: If the cultural-zionist constraint vanished overnight, the Hebrew cultural revival in Palestine would lose its coordinating framework — immigration would lack a unifying cultural telos, institutions would lack a shared normative horizon, and the 'shared homeland' imagination would collapse into either sovereignist Zionism or anti-Zionist rejection. The world rearranges because this reading was the *only* framework that made Jewish cultural vitality and Arab co-habitation structurally compatible; its absence forces a binary choice.
% FOUNDING_PROBLEM: The Jewish people faced cultural assimilation and physical persecution in diaspora, while the Arab inhabitants of Palestine faced imperial domination and the threat of displacement by a sovereign Jewish state. The cultural-zionist arrangement was built to solve both: a Jewish spiritual center that revitalizes the people without becoming a state that expels the other.
% FOUNDING_PROBLEM_CORROBORATION: Ahad Ha'am (Asher Ginsberg), the primary architect of cultural Zionism, attested that the 'Jewish state' would be a spiritual center, not a political one. Martin Buber and Judah Magnes (Brit Shalom) corroborated the bi-national, non-sovereign vision from within the Zionist movement. Palestinian intellectuals (e.g., Khalil al-Sakakini) engaged with the cultural project while rejecting political Zionism — their testimony exists but is often filtered through later nationalist historiography. No single corroborator outside the benefiting parties (Jewish cultural revivalists) endorses the status as 'live' today; the founding problem is contested because the sovereignist reading won historically.
narrative_ontology:disappearance_verdict(jewish_sovereignty_palestine__cultural_zionist_reading, world_rearranges).
narrative_ontology:founding_problem_status(jewish_sovereignty_palestine__cultural_zionist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jewish_sovereignty_palestine__cultural_zionist_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(jewish_sovereignty_palestine__cultural_zionist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jewish_sovereignty_palestine__cultural_zionist_reading, 0.18, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jewish_sovereignty_palestine__cultural_zionist_reading_tests).
:- end_tests(jewish_sovereignty_palestine__cultural_zionist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The claimed type is rope because: (1) genuine coordination function — solves diaspora cultural extinction risk + ancestral land connection without zero-sum sovereignty; (2) minimal coercion — immigration and settlement are voluntary, land purchase is consensual (in this reading's ideal), no enforcement machinery against Palestinians; (3) mutual benefit — Jewish cultural revivalists get their center; Palestinian Arabs get demographic-cultural presence without political subordination (in the reading's logic); (4) alternatives not suppressed — political Zionism, bi-nationalism, Arab nationalism all remain live options. The measurement series shows rising extractiveness and suppression 1917-1947 as the cultural project is overtaken by sovereignist forces (Balfour, Mandate, Arab revolt, partition), but the reading's *structural* end-state (1948) returns to low extractiveness because the constraint is defined by its own logic, not by the historical forces that displaced it. The 1948 measurement captures the reading's persistence as a *counterfactual* structural profile — what the constraint *is* when instantiated purely.
 *
 * DIRECTIONALITY LOGIC:
 *   Jewish cultural revivalists are beneficiaries (d ~ 0.15): they collect cultural vitality, Hebrew revival, communal institutions — the constraint subsidizes their project. Palestinian Arab inhabitants are dual-positioned: primary role beneficiary (co-inhabitants in shared cultural space, no displacement, cultural enrichment from Jewish presence), secondary role payer (bear demographic transition costs, cultural friction, land market pressure — d ~ 0.45 for this aspect). Zionist institutional leadership are agenda_setters with arbitrage exit (d ~ 0.1): they coordinate but do not extract; they could pivot to sovereignty (and did). British authorities are observers (d ~ 0.5): they hold coercive power but are not structurally positioned as beneficiaries or victims of the cultural project itself. Liberal nationalist Zionists and Palestinian nationalist leadership are excluded — their structural positions are defined *relative to* this constraint (they want what it does not provide), not *within* it.
 *
 * MANDATROPHY ANALYSIS:
 *   The cultural-zionist reading resolves the mandatrophy tension by refusing the mandate that outlives its function: the mandate 'Jewish national home' was reinterpreted by sovereignist forces into 'Jewish state.' The cultural reading declares the founding problem (cultural survival) solvable *without* the sovereign mandate — thus the mandate (statehood) is recognized as mandatrophic (persisting after its cultural justification is met or bypassed). The classification as rope prevents mislabeling: it is not a snare (no extraction from Palestinians), not a tangled_rope (no asymmetric extraction requiring enforcement), not a scaffold (no declared sunset — the cultural center is meant to be permanent). The mandatrophy is resolved by *not upgrading* the coordination mechanism into a sovereign one.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    cultural_sovereignty_boundary,
    'Can a Jewish cultural center in Palestine maintain its vitality indefinitely without political sovereignty — or does cultural autonomy inevitably require or produce sovereign claims?',
    'Historical counterfactual analysis: if the Yishuv had rejected statehood in 1947-48 and pursued bi-national cultural autonomy under UN trusteeship or Ottoman-style millet system, would Hebrew culture have flourished or atrophied? Comparative cases: Quebec cultural autonomy within Canada; Catalan cultural institutions without statehood; Jewish cultural autonomy in interwar Lithuania/Poland.',
    'If cultural vitality requires sovereignty, the reading''s rope structure collapses into a scaffold (temporary coordination) or tangled_rope (sovereignty as hidden extraction). If cultural autonomy is sustainable, the rope classification holds and the sovereignist readings are the mandatrophic deviations.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cultural_sovereignty_boundary, conceptual, 'Whether cultural autonomy without sovereignty is a stable equilibrium or a transient phase.').

omega_variable(
    palestinian_consent_ambiguity,
    'Does the cultural-zionist reading''s claim of ''mutual benefit'' for Palestinians reflect genuine structural consent, or is it a projection that obscures the demographic transformation Palestinians experienced?',
    'Palestinian Arab intellectual and political discourse 1908-1948: did any significant Palestinian current accept a *cultural* Jewish presence (Hebrew revival, immigration, institutions) while rejecting *political* sovereignty? Test against al-Sakakini, al-Khalidi, Nashashibi faction positions. Distinguish between ''acceptance of Jews as neighbors'' and ''acceptance of Jewish national cultural project as demographic transformation.''',
    'If Palestinians never consented to the cultural project''s demographic logic, the reading''s beneficiary claim for Palestinians is a false summit — the constraint is a snare disguised as rope. If a consenting current existed but was politically marginalized, the reading captures a real but defeated structural possibility.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(palestinian_consent_ambiguity, empirical, 'Whether Palestinian Arabs were genuine beneficiaries of the cultural-zionist arrangement or its unacknowledged victims.').

omega_variable(
    reading_foreclosure_vs_coexistence,
    'Does the cultural-zionist reading logically foreclose the liberal_nationalist_reading, or do they coexist as competing frameworks within Zionism?',
    'Analyze the logical structure: cultural Zionism asserts ''Jewish cultural vitality *does not require* sovereignty.'' Liberal nationalist Zionism asserts ''Jewish self-determination *requires* sovereignty.'' Are these contradictory (one denies the other''s necessity claim) or complementary (cultural vitality *plus* sovereignty)? Test against Ahad Ha''am''s explicit rejection of political Zionism vs. Ben-Gurion''s instrumental use of cultural institutions for state-building.',
    'If forecloses: the readings cannot coexist in one framework; the kernel has a genuine logical fracture. If coexists_with: they are rival factions within a movement, not mutually exclusive truth-claims. The engine''s reading_relations will compute this from the declared axioms.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_foreclosure_vs_coexistence, conceptual, 'Logical relationship between cultural and sovereignist Zionist readings of the same kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jewish_sovereignty_palestine__cultural_zionist_reading, 1896, 1948).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jewi_tr_t1896, jewish_sovereignty_palestine__cultural_zionist_reading, theater_ratio, 1896, 0.1).
narrative_ontology:measurement(jewi_tr_t1917, jewish_sovereignty_palestine__cultural_zionist_reading, theater_ratio, 1917, 0.15).
narrative_ontology:measurement(jewi_tr_t1929, jewish_sovereignty_palestine__cultural_zionist_reading, theater_ratio, 1929, 0.22).
narrative_ontology:measurement(jewi_tr_t1936, jewish_sovereignty_palestine__cultural_zionist_reading, theater_ratio, 1936, 0.3).
narrative_ontology:measurement(jewi_tr_t1947, jewish_sovereignty_palestine__cultural_zionist_reading, theater_ratio, 1947, 0.4).
narrative_ontology:measurement(jewi_tr_t1948, jewish_sovereignty_palestine__cultural_zionist_reading, theater_ratio, 1948, 0.25).

% Extraction over time
narrative_ontology:measurement(jewi_be_t1896, jewish_sovereignty_palestine__cultural_zionist_reading, base_extractiveness, 1896, 0.05).
narrative_ontology:measurement(jewi_be_t1917, jewish_sovereignty_palestine__cultural_zionist_reading, base_extractiveness, 1917, 0.12).
narrative_ontology:measurement(jewi_be_t1929, jewish_sovereignty_palestine__cultural_zionist_reading, base_extractiveness, 1929, 0.18).
narrative_ontology:measurement(jewi_be_t1936, jewish_sovereignty_palestine__cultural_zionist_reading, base_extractiveness, 1936, 0.25).
narrative_ontology:measurement(jewi_be_t1947, jewish_sovereignty_palestine__cultural_zionist_reading, base_extractiveness, 1947, 0.35).
narrative_ontology:measurement(jewi_be_t1948, jewish_sovereignty_palestine__cultural_zionist_reading, base_extractiveness, 1948, 0.18).

% Suppression requirement over time
narrative_ontology:measurement(jewi_su_t1896, jewish_sovereignty_palestine__cultural_zionist_reading, suppression_requirement, 1896, 0.02).
narrative_ontology:measurement(jewi_su_t1917, jewish_sovereignty_palestine__cultural_zionist_reading, suppression_requirement, 1917, 0.08).
narrative_ontology:measurement(jewi_su_t1929, jewish_sovereignty_palestine__cultural_zionist_reading, suppression_requirement, 1929, 0.15).
narrative_ontology:measurement(jewi_su_t1936, jewish_sovereignty_palestine__cultural_zionist_reading, suppression_requirement, 1936, 0.25).
narrative_ontology:measurement(jewi_su_t1947, jewish_sovereignty_palestine__cultural_zionist_reading, suppression_requirement, 1947, 0.35).
narrative_ontology:measurement(jewi_su_t1948, jewish_sovereignty_palestine__cultural_zionist_reading, suppression_requirement, 1948, 0.12).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jewish_sovereignty_palestine__cultural_zionist_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(jewish_sovereignty_palestine__cultural_zionist_reading, 0.08).
narrative_ontology:affects_constraint(jewish_sovereignty_palestine__cultural_zionist_reading, jewish_sovereignty_palestine__liberal_nationalist_reading).
narrative_ontology:affects_constraint(jewish_sovereignty_palestine__cultural_zionist_reading, jewish_sovereignty_palestine__settler_colonial_reading).
narrative_ontology:affects_constraint(jewish_sovereignty_palestine__cultural_zionist_reading, jewish_sovereignty_palestine__religious_zionist_reading).
narrative_ontology:affects_constraint(jewish_sovereignty_palestine__cultural_zionist_reading, jewish_sovereignty_palestine__post_zionist_reading).

% DUAL FORMULATION NOTE:
% This constraint is one member of the jewish_sovereignty_palestine constraint family. The cultural_zionist_reading (this story) has ε=0.18 (rope). The liberal_nationalist_reading has higher ε (tangled_rope/snare) because statehood requires displacement/enforcement. The settler_colonial_reading reads the *same historical process* as a snare (ε~0.8+). The post_zionist_reading reads the *outcome* (State of Israel) as a snare for Palestinians and a piton for Israeli Jews. The religious_zionist_reading reads the kernel as a mountain (divine promise, ε~0). The ε-invariance principle requires separate stories because the referent ('Jewish sovereignty in Palestine') decomposes into structurally distinct claims with different beneficiary/victim structures, different enforcement requirements, and different natural-law vs. constructed status.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(jewish_sovereignty_palestine__cultural_zionist_reading, moderate, 0.45).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
