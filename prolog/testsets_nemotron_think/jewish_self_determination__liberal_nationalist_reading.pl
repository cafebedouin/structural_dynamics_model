% ============================================================================
% CONSTRAINT STORY: jewish_self_determination__liberal_nationalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jewish_self_determination__liberal_nationalist_reading, []).

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
 *   constraint_id: jewish_self_determination__liberal_nationalist_reading
 *   human_readable: Jewish National Self-Determination (Liberal Nationalist Reading)
 *   domain: political/philosophical/nationalism
 *
 * SUMMARY:
 *   The liberal nationalist reading frames Jewish self-determination as a
 *   standard application of the Wilsonian principle: Jews constitute a nation
 *   like any other, entitled to a sovereign state in their historic homeland.
 *   This reading emerged from the European nationalist tradition (Herzl,
 *   Nordau, Ben-Gurion) and became the dominant diplomatic framework through
 *   the UN partition plan (1947) and the two-state paradigm. It claims to
 *   resolve the conflict through mutual recognition and territorial
 *   compromise. The reading's extraction is low in principle (coordination
 *   via partition) but rises in practice when partition fails and the
 *   framework is used to legitimize continued control over disputed
 *   territory. The constraint has no declared victims in its own logic —
 *   partition is supposed to satisfy both national claims — but structurally
 *   the Palestinian national movement and refugee population bear costs that
 *   the reading treats as transitional rather than structural.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jewish_self_determination__liberal_nationalist_reading, 0.35).
domain_priors:suppression_score(jewish_self_determination__liberal_nationalist_reading, 0.2).
domain_priors:theater_ratio(jewish_self_determination__liberal_nationalist_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jewish_self_determination__liberal_nationalist_reading, extractiveness, 0.35).
narrative_ontology:constraint_metric(jewish_self_determination__liberal_nationalist_reading, suppression_requirement, 0.2).
narrative_ontology:constraint_metric(jewish_self_determination__liberal_nationalist_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jewish_self_determination__liberal_nationalist_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(jewish_self_determination__liberal_nationalist_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jewish_self_determination__liberal_nationalist_reading, rope).
narrative_ontology:human_readable(jewish_self_determination__liberal_nationalist_reading, "Jewish National Self-Determination (Liberal Nationalist Reading)").
narrative_ontology:topic_domain(jewish_self_determination__liberal_nationalist_reading, "political/philosophical/nationalism").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jewish_self_determination__liberal_nationalist_reading, 'd3283a4c-1ddd-418a-a0c6-8e5f51f8290d').
narrative_ontology:cs_kernel_codification('d3283a4c-1ddd-418a-a0c6-8e5f51f8290d', distributed).
narrative_ontology:cs_authority_grounding('d3283a4c-1ddd-418a-a0c6-8e5f51f8290d', lineage).
narrative_ontology:cs_interpretation_layer_present('d3283a4c-1ddd-418a-a0c6-8e5f51f8290d').
narrative_ontology:cs_reading_relation('d3283a4c-1ddd-418a-a0c6-8e5f51f8290d', jewish_self_determination__indigenous_return_reading, coexists_with).
narrative_ontology:cs_reading_relation('d3283a4c-1ddd-418a-a0c6-8e5f51f8290d', jewish_self_determination__settler_colonial_reading, influences).
narrative_ontology:cs_reading_relation('d3283a4c-1ddd-418a-a0c6-8e5f51f8290d', jewish_self_determination__religious_covenant_reading, coexists_with).
narrative_ontology:cs_reading_relation('d3283a4c-1ddd-418a-a0c6-8e5f51f8290d', jewish_self_determination__diasporist_reading, forecloses).
narrative_ontology:cs_axiom('d3283a4c-1ddd-418a-a0c6-8e5f51f8290d', foundational, jews_constitute_a_nation_under_international_law).
narrative_ontology:cs_axiom_status(jews_constitute_a_nation_under_international_law, holdable).
narrative_ontology:cs_axiom_grounding('d3283a4c-1ddd-418a-a0c6-8e5f51f8290d', jews_constitute_a_nation_under_international_law, conventional).
narrative_ontology:cs_axiom('d3283a4c-1ddd-418a-a0c6-8e5f51f8290d', foundational, national_self_determination_requires_territorial_sovereignty).
narrative_ontology:cs_axiom_status(national_self_determination_requires_territorial_sovereignty, holdable).
narrative_ontology:cs_axiom_grounding('d3283a4c-1ddd-418a-a0c6-8e5f51f8290d', national_self_determination_requires_territorial_sovereignty, conventional).
narrative_ontology:cs_axiom('d3283a4c-1ddd-418a-a0c6-8e5f51f8290d', secondary, partition_resolves_competing_national_claims).
narrative_ontology:cs_axiom_status(partition_resolves_competing_national_claims, holdable).
narrative_ontology:cs_axiom_grounding('d3283a4c-1ddd-418a-a0c6-8e5f51f8290d', partition_resolves_competing_national_claims, empirically_contingent).
narrative_ontology:cs_reference_frame('d3283a4c-1ddd-418a-a0c6-8e5f51f8290d', liberal_nationalist_framework).
narrative_ontology:cs_drift_state('d3283a4c-1ddd-418a-a0c6-8e5f51f8290d', post_oslo_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('d3283a4c-1ddd-418a-a0c6-8e5f51f8290d', '').
narrative_ontology:cs_kernel_id(jewish_self_determination__liberal_nationalist_reading, jewish_self_determination).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jewish_self_determination__liberal_nationalist_reading, jewish_diaspora_seeking_refuge_and_sovereignty).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(jewish_self_determination__liberal_nationalist_reading, palestinian_national_movement).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Jewish communities historically lacking territorial sovereignty, vulnerable to persecution and statelessness. The constraint provides a normative framework for claiming a national home. Exit means abandoning the national claim for diaspora integration or alternative frameworks, which carries identity and security costs.
narrative_ontology:constraint_stakeholder(jewish_self_determination__liberal_nationalist_reading, jewish_diaspora_seeking_refuge_and_sovereignty, beneficiary,
    organized, generational, constrained, global).

% Palestinian national movement asserting competing claim to the same territory. This reading assumes partition resolves the competition, but structurally the Palestinian movement bears the cost of territorial compromise and ongoing military occupation. Exit from the conflict is constrained by material conditions and international recognition structures.
narrative_ontology:constraint_stakeholder(jewish_self_determination__liberal_nationalist_reading, palestinian_national_movement, payer,
    organized, generational, constrained, regional).

% Israeli state institutions, World Zionist Organization, and affiliated bodies that administer and advocate for the national home. They set the political agenda, control immigration policy, and manage the territorial settlement project. They can shift strategies (e.g., from labor zionism to revisionist) while maintaining the core claim.
narrative_ontology:constraint_stakeholder(jewish_self_determination__liberal_nationalist_reading, zionist_institutions, agenda_setter,
    institutional, generational, arbitrage, global).

% UN member states, international legal bodies, and diplomatic frameworks that recognize, mediate, or enforce the partition paradigm. They provide the international legal scaffolding (UNGA 181, UNSC 242, Oslo Accords) that makes the liberal nationalist reading operational. They can withdraw recognition or shift diplomatic pressure.
narrative_ontology:constraint_stakeholder(jewish_self_determination__liberal_nationalist_reading, international_community, agenda_setter,
    institutional, biographical, analytical, global).
narrative_ontology:stakeholder_secondary_role(jewish_self_determination__liberal_nationalist_reading, international_community, observer).

% Jewish individuals and organizations (e.g., Jewish Voice for Peace, Satmar Hasidim, Bundist tradition) who reject Jewish nationalism as either dangerous or religiously forbidden. They are structurally excluded from the mainstream Jewish institutional consensus and from the international diplomatic framework that treats Zionism as the legitimate Jewish voice.
narrative_ontology:constraint_stakeholder(jewish_self_determination__liberal_nationalist_reading, anti_zionist_jewish_voices, excluded,
    moderate, biographical, mobile, global).

% Palestinian refugees and their descendants displaced in 1948 and 1967, living in camps across the region. The liberal nationalist reading's partition framework treats their right of return as negotiable or symbolic, not absolute. They have no effective exit from their condition and no voice in the negotiations that determine their fate.
narrative_ontology:constraint_stakeholder(jewish_self_determination__liberal_nationalist_reading, palestinian_refugees_and_descendants, excluded,
    powerless, generational, trapped, regional).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Resolving competing national claims to the same territory through mutual recognition and territorial partition, providing a peaceful framework for two peoples to exercise self-determination side by side.
% TRANSFER_FUNCTION: Moves political sovereignty and exclusive territorial control from a contested, zero-sum status to a recognized division where each nation exercises self-determination in a defined homeland, with security guarantees and minority rights protections.
% ABSENT_VOICES: Palestinian refugees and their descendants who would object to partition as resolution of their displacement; Jewish diasporists who reject territorial nationalism as solution to Jewish vulnerability; Mizrahi Jews whose indigeneity to the region complicates the European-nationalist framing.
% DISAPPEARANCE_RATIONALE: If the liberal nationalist framework vanished overnight, the primary secular justification for Jewish statehood would collapse. The claim would retreat to religious covenant or colonial settlement frameworks, fundamentally altering the international legal basis for Israel's legitimacy and the diplomatic architecture of the two-state paradigm.
% FOUNDING_PROBLEM: The problem of Jewish statelessness and vulnerability to persecution in diaspora, requiring a territorial national home for collective security and normal national existence among nations.
% FOUNDING_PROBLEM_CORROBORATION: Historical documentation of pogroms, the Holocaust, and statelessness corroborates the founding problem's reality. The status is contested: Zionist institutions and most Jewish communal bodies attest the problem persists (rising antisemitism, Iran threat); diasporist and integrationist Jewish voices attest the problem was solved by emancipation and liberal democracy; Palestinian voices attest the 'solution' created a new stateless people.
narrative_ontology:disappearance_verdict(jewish_self_determination__liberal_nationalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(jewish_self_determination__liberal_nationalist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jewish_self_determination__liberal_nationalist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(jewish_self_determination__liberal_nationalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jewish_self_determination__liberal_nationalist_reading, 0.35, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jewish_self_determination__liberal_nationalist_reading_tests).
:- end_tests(jewish_self_determination__liberal_nationalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low-to-moderate (0.35) because the reading's core claim is coordinative (mutual recognition via partition), but the metric reflects the historical drift where the framework has been used to legitimize settlement expansion and occupation. Suppression is low (0.2) because the reading does not inherently require coercion — it proposes negotiation — but suppression rises when the framework becomes a cover for facts on the ground. Theater ratio is low (0.15) because the coordination function (diplomatic recognition, security guarantees) is real, though performative elements increase when negotiations stall. Accessibility collapse is moderate (0.4) because alternatives (binational state, diaspora integration, religious frameworks) remain conceptually available but are politically marginalized. Resistance is moderate (0.5) because competing national claims and internal Jewish dissent create persistent opposition.
 *
 * PERSPECTIVAL GAP:
 *   The beneficiary seat (Jewish diaspora) experiences this as a rope: genuine coordination solving statelessness. The payer seat (Palestinian national movement) experiences it as a tangled rope: coordination function exists (partition talks) but extraction is asymmetric (settlements expand during negotiations). The trapped seat (Palestinian refugees) experiences it as a snare: the coordination story is cover for permanent exclusion. The engine computes this divergence from the structural data — the authored claim (rope) does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Jewish diaspora seeking refuge is the primary beneficiary (d near 0.0) — the constraint provides the normative and institutional pathway to sovereignty. Zionist institutions are agenda-setters with arbitrage-grade exit (they control the implementation). Palestinian national movement is a payer (d ~0.6) — they bear territorial compromise and ongoing occupation costs, but have constrained exit (organized national movement). Palestinian refugees are trapped (d ~0.9) — excluded from the framework's benefits, bearing its costs with no exit. Anti-Zionist Jewish voices are mobile-excluded — they can speak but are structurally locked out of the consensus. International community sits near analytical (d ~0.5) — they mediate but also enforce the framework.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (Jewish statelessness/vulnerability) is contested: statehood advocates say it persists (new threats); integrationists say it was solved by liberal democracy; Palestinians say the solution created a new stateless people. The constraint persists despite contested founding problem status because it has become the basis of an established state with powerful institutions — classic mandatrophy where the arrangement outlives consensus on its justification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'This constraint is the liberal_nationalist_reading of the jewish_self_determination kernel. What structural elements distinguish it from the indigenous_return_reading, settler_colonial_reading, religious_covenant_reading, and diasporist_reading?',
    'Comparative analysis of each reading''s beneficiary/victim structure, claimed coordination function, and epsilon referent. The liberal nationalist reading''s distinctive feature is its reliance on the universalist self-determination principle and the partition compromise as the coordination mechanism.',
    'If the readings cannot be cleanly separated, the kernel is not a single contested commitment but a conflation of distinct constraints — each should be authored separately with its own epsilon.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Structural distinctness of this reading from sibling readings of the same kernel').

omega_variable(
    partition_feasibility,
    'Is territorial partition with mutual recognition still a feasible coordination mechanism, or has the settlement project made it physically and politically impossible?',
    'Empirical assessment of territorial contiguity, demographic ratios, and political will on both sides. The 2024 ICJ advisory opinion and the collapse of the Oslo process are key evidence points.',
    'If partition is infeasible, the reading''s coordination function is defunct and its extractiveness rises (the framework becomes cover for one-state reality). The constraint would reclassify from rope toward tangled_rope or snare depending on enforcement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(partition_feasibility, empirical, 'Whether the reading''s core coordination mechanism (partition) remains viable').

omega_variable(
    victim_structure_ambiguity,
    'This reading declares no victims in principle (partition resolves competing claims). Does the structural reality of Palestinian displacement and ongoing occupation make victims structurally necessary, rendering the ''no victims'' declaration a false coordinate?',
    'Analysis of whether the constraint''s operation — not its aspiration — produces identifiable victims. If the constraint''s persistence depends on maintaining a demographic majority through policies that displace or exclude Palestinians, victims are structural.',
    'If victims are structural, the constraint cannot be a pure rope; it must be at minimum tangled_rope (coordination + extraction) or snare (extraction with coordination cover). This would change the classification and mandate beneficiary/victim declarations.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(victim_structure_ambiguity, conceptual, 'Whether the ''no victims in principle'' claim holds structurally').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jewish_self_determination__liberal_nationalist_reading, 1897, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jewish_self_determination__liberal_nationalist_reading_tr_t1897, jewish_self_determination__liberal_nationalist_reading, theater_ratio, 1897, 0.05).
narrative_ontology:measurement(jewish_self_determination__liberal_nationalist_reading_tr_t1917, jewish_self_determination__liberal_nationalist_reading, theater_ratio, 1917, 0.1).
narrative_ontology:measurement(jewish_self_determination__liberal_nationalist_reading_tr_t1947, jewish_self_determination__liberal_nationalist_reading, theater_ratio, 1947, 0.1).
narrative_ontology:measurement(jewish_self_determination__liberal_nationalist_reading_tr_t1967, jewish_self_determination__liberal_nationalist_reading, theater_ratio, 1967, 0.2).
narrative_ontology:measurement(jewish_self_determination__liberal_nationalist_reading_tr_t1993, jewish_self_determination__liberal_nationalist_reading, theater_ratio, 1993, 0.15).
narrative_ontology:measurement(jewish_self_determination__liberal_nationalist_reading_tr_t2000, jewish_self_determination__liberal_nationalist_reading, theater_ratio, 2000, 0.25).
narrative_ontology:measurement(jewish_self_determination__liberal_nationalist_reading_tr_t2024, jewish_self_determination__liberal_nationalist_reading, theater_ratio, 2024, 0.15).

% Extraction over time
narrative_ontology:measurement(jewish_self_determination__liberal_nationalist_reading_be_t1897, jewish_self_determination__liberal_nationalist_reading, base_extractiveness, 1897, 0.15).
narrative_ontology:measurement(jewish_self_determination__liberal_nationalist_reading_be_t1917, jewish_self_determination__liberal_nationalist_reading, base_extractiveness, 1917, 0.2).
narrative_ontology:measurement(jewish_self_determination__liberal_nationalist_reading_be_t1947, jewish_self_determination__liberal_nationalist_reading, base_extractiveness, 1947, 0.25).
narrative_ontology:measurement(jewish_self_determination__liberal_nationalist_reading_be_t1967, jewish_self_determination__liberal_nationalist_reading, base_extractiveness, 1967, 0.4).
narrative_ontology:measurement(jewish_self_determination__liberal_nationalist_reading_be_t1993, jewish_self_determination__liberal_nationalist_reading, base_extractiveness, 1993, 0.3).
narrative_ontology:measurement(jewish_self_determination__liberal_nationalist_reading_be_t2000, jewish_self_determination__liberal_nationalist_reading, base_extractiveness, 2000, 0.35).
narrative_ontology:measurement(jewish_self_determination__liberal_nationalist_reading_be_t2024, jewish_self_determination__liberal_nationalist_reading, base_extractiveness, 2024, 0.35).

% Suppression requirement over time
narrative_ontology:measurement(jewish_self_determination__liberal_nationalist_reading_su_t1897, jewish_self_determination__liberal_nationalist_reading, suppression_requirement, 1897, 0.1).
narrative_ontology:measurement(jewish_self_determination__liberal_nationalist_reading_su_t1917, jewish_self_determination__liberal_nationalist_reading, suppression_requirement, 1917, 0.15).
narrative_ontology:measurement(jewish_self_determination__liberal_nationalist_reading_su_t1947, jewish_self_determination__liberal_nationalist_reading, suppression_requirement, 1947, 0.2).
narrative_ontology:measurement(jewish_self_determination__liberal_nationalist_reading_su_t1967, jewish_self_determination__liberal_nationalist_reading, suppression_requirement, 1967, 0.4).
narrative_ontology:measurement(jewish_self_determination__liberal_nationalist_reading_su_t1993, jewish_self_determination__liberal_nationalist_reading, suppression_requirement, 1993, 0.25).
narrative_ontology:measurement(jewish_self_determination__liberal_nationalist_reading_su_t2000, jewish_self_determination__liberal_nationalist_reading, suppression_requirement, 2000, 0.3).
narrative_ontology:measurement(jewish_self_determination__liberal_nationalist_reading_su_t2024, jewish_self_determination__liberal_nationalist_reading, suppression_requirement, 2024, 0.2).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jewish_self_determination__liberal_nationalist_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(jewish_self_determination__liberal_nationalist_reading, 0.08).
narrative_ontology:affects_constraint(jewish_self_determination__liberal_nationalist_reading, jewish_self_determination__indigenous_return_reading).
narrative_ontology:affects_constraint(jewish_self_determination__liberal_nationalist_reading, jewish_self_determination__settler_colonial_reading).
narrative_ontology:affects_constraint(jewish_self_determination__liberal_nationalist_reading, jewish_self_determination__religious_covenant_reading).
narrative_ontology:affects_constraint(jewish_self_determination__liberal_nationalist_reading, jewish_self_determination__diasporist_reading).

% DUAL FORMULATION NOTE:
% This is one of five constraint stories decomposing the 'Jewish self-determination' kernel. Each reading instantiates a distinct constraint with its own epsilon, beneficiary/victim structure, and claimed type. The liberal nationalist reading claims rope (coordination via partition); indigenous_return_reading claims rope with different beneficiary logic; settler_colonial_reading claims snare; religious_covenant_reading claims mountain (divine law); diasporist_reading claims scaffold (transitional diaspora autonomy).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(jewish_self_determination__liberal_nationalist_reading, powerless, 0.9).
constraint_indexing:directionality_override(jewish_self_determination__liberal_nationalist_reading, organized, 0.6).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
