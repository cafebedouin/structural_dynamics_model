% ============================================================================
% CONSTRAINT STORY: zionist_legitimacy_basis__national_liberation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_zionist_legitimacy_basis__national_liberation_reading, []).

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
    constraint_indexing:constraint_classification/3,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: zionist_legitimacy_basis__national_liberation_reading
 *   human_readable: Zionist Legitimacy Basis: National Liberation Reading
 *   domain: political_history/nationalism/settler_colonialism_studies
 *
 * SUMMARY:
 *   This constraint story models Zionism as a national liberation movement
 *   for a persecuted indigenous people returning to their ancestral homeland.
 *   This reading emphasizes Jewish historical connection to the land and the
 *   imperative for self-determination in the face of historical persecution.
 *   It frames Arab opposition as a denial of Jewish rights, rather than a
 *   response to displacement. The constraint operates as a Tangled Rope,
 *   providing national coordination for Jewish people while simultaneously
 *   extracting land and sovereignty from Palestinians through active
 *   enforcement.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(zionist_legitimacy_basis__national_liberation_reading, 0.65).
domain_priors:suppression_score(zionist_legitimacy_basis__national_liberation_reading, 0.75).
domain_priors:theater_ratio(zionist_legitimacy_basis__national_liberation_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(zionist_legitimacy_basis__national_liberation_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(zionist_legitimacy_basis__national_liberation_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(zionist_legitimacy_basis__national_liberation_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(zionist_legitimacy_basis__national_liberation_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(zionist_legitimacy_basis__national_liberation_reading, resistance, 0.8).

% --- Constraint claim ---
narrative_ontology:constraint_claim(zionist_legitimacy_basis__national_liberation_reading, tangled_rope).
narrative_ontology:human_readable(zionist_legitimacy_basis__national_liberation_reading, "Zionist Legitimacy Basis: National Liberation Reading").
narrative_ontology:topic_domain(zionist_legitimacy_basis__national_liberation_reading, "political_history/nationalism/settler_colonialism_studies").

domain_priors:requires_active_enforcement(zionist_legitimacy_basis__national_liberation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(zionist_legitimacy_basis__national_liberation_reading, '06a0cb01-149c-42eb-97b0-e79a4077d98b').
narrative_ontology:cs_kernel_codification('06a0cb01-149c-42eb-97b0-e79a4077d98b', formalized).
narrative_ontology:cs_authority_grounding('06a0cb01-149c-42eb-97b0-e79a4077d98b', lineage).
narrative_ontology:cs_interpretation_layer_present('06a0cb01-149c-42eb-97b0-e79a4077d98b').
narrative_ontology:cs_reading_relation('06a0cb01-149c-42eb-97b0-e79a4077d98b', zionist_legitimacy_basis__settler_colonial_reading, coexists_with).
narrative_ontology:cs_reading_relation('06a0cb01-149c-42eb-97b0-e79a4077d98b', zionist_legitimacy_basis__religious_restoration_reading, coexists_with).
narrative_ontology:cs_axiom('06a0cb01-149c-42eb-97b0-e79a4077d98b', foundational, jewish_people_are_a_nation).
narrative_ontology:cs_axiom_status(jewish_people_are_a_nation, holdable).
narrative_ontology:cs_axiom_grounding('06a0cb01-149c-42eb-97b0-e79a4077d98b', jewish_people_are_a_nation, conventional).
narrative_ontology:cs_axiom('06a0cb01-149c-42eb-97b0-e79a4077d98b', foundational, right_to_self_determination_in_ancestral_homeland).
narrative_ontology:cs_axiom_status(right_to_self_determination_in_ancestral_homeland, holdable).
narrative_ontology:cs_axiom_grounding('06a0cb01-149c-42eb-97b0-e79a4077d98b', right_to_self_determination_in_ancestral_homeland, deontological).
narrative_ontology:cs_reference_frame('06a0cb01-149c-42eb-97b0-e79a4077d98b', post_holocaust_national_self_determination).
narrative_ontology:cs_drift_state('06a0cb01-149c-42eb-97b0-e79a4077d98b', contemporary_human_rights_discourse, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('06a0cb01-149c-42eb-97b0-e79a4077d98b', '').
narrative_ontology:cs_kernel_id(zionist_legitimacy_basis__national_liberation_reading, zionist_legitimacy_basis).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(zionist_legitimacy_basis__national_liberation_reading, israeli_jewish_citizens).
narrative_ontology:constraint_beneficiary(zionist_legitimacy_basis__national_liberation_reading, zionist_organizations).
narrative_ontology:constraint_victim(zionist_legitimacy_basis__national_liberation_reading, palestinian_arabs).
narrative_ontology:constraint_victim(zionist_legitimacy_basis__national_liberation_reading, international_critics_of_israel).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(zionist_legitimacy_basis__national_liberation_reading, western_governments).
narrative_ontology:constraint_beneficiary(zionist_legitimacy_basis__national_liberation_reading, jewish_diaspora_communities).
narrative_ontology:constraint_victim(zionist_legitimacy_basis__national_liberation_reading, jewish_diaspora_communities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Experience the state as the fulfillment of national self-determination and a refuge from persecution, providing security and cultural continuity. Their identity is deeply intertwined with the state's existence and its national liberation narrative.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__national_liberation_reading, israeli_jewish_citizens, beneficiary,
    institutional, generational, identity_locked, national).

% Actively promote and defend the national liberation narrative, mobilizing political and financial support for Israel. They shape policy and public discourse to reinforce the legitimacy of Jewish return and statehood.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__national_liberation_reading, zionist_organizations, agenda_setter,
    organized, generational, constrained, global).

% Are displaced, dispossessed, and subjected to military occupation or discriminatory laws. Their historical narrative of indigeneity and continuous presence is actively suppressed by the national liberation framing, which delegitimizes their claims.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__national_liberation_reading, palestinian_arabs, payer,
    powerless, generational, trapped, regional).

% Face accusations of antisemitism or anti-Zionism when challenging the national liberation narrative. They bear reputational costs and political pressure for advocating for Palestinian rights or questioning Israeli policies.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__national_liberation_reading, international_critics_of_israel, payer,
    moderate, biographical, mobile, global).

% Benefit from a stable, pro-Western ally in the Middle East and often align with the national liberation narrative for geopolitical reasons, providing diplomatic and military support. They face internal and external pressure to balance this support with human rights concerns.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__national_liberation_reading, western_governments, beneficiary,
    institutional, generational, constrained, global).

% Often identify with Israel as a homeland and a symbol of Jewish resilience, providing political and financial support. Some members experience internal conflict or external pressure when the state's actions diverge from their ethical values, creating a 'payer' aspect through moral distress or reputational spillover.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__national_liberation_reading, jewish_diaspora_communities, beneficiary,
    organized, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(zionist_legitimacy_basis__national_liberation_reading, jewish_diaspora_communities, payer).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the collective action of Jewish people globally towards establishing and maintaining a sovereign state in their ancestral homeland, providing a secure refuge and cultural center.
% TRANSFER_FUNCTION: Transfers land, resources, and political sovereignty from the indigenous Palestinian population to Jewish immigrants and settlers, justified by historical connection and national self-determination.
% ABSENT_VOICES: Palestinian voices, particularly those advocating for a right of return or a single secular state, are systematically excluded from the dominant discourse that frames Zionism as solely a national liberation movement. Their historical narrative is delegitimized as a denial of Jewish rights.
% DISAPPEARANCE_RATIONALE: If the national liberation basis for Zionism vanished, the legitimacy of the State of Israel would be fundamentally challenged, leading to a profound re-evaluation of land claims, citizenship, and the rights of both Jewish and Palestinian populations. The entire geopolitical structure of the region would be forced to rearrange.
% FOUNDING_PROBLEM: The historical persecution and statelessness of the Jewish people, culminating in the Holocaust, necessitated the establishment of a sovereign Jewish state as a national home and refuge.
% FOUNDING_PROBLEM_CORROBORATION: The problem of antisemitism and the need for a Jewish homeland is widely attested by Jewish communities globally and by international bodies recognizing the historical persecution of Jews. However, the specific solution of establishing a state through displacement is contested by Palestinian and many international human rights organizations, who offer alternative historical accounts and solutions.
narrative_ontology:disappearance_verdict(zionist_legitimacy_basis__national_liberation_reading, world_rearranges).
narrative_ontology:founding_problem_status(zionist_legitimacy_basis__national_liberation_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(zionist_legitimacy_basis__national_liberation_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(zionist_legitimacy_basis__national_liberation_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(zionist_legitimacy_basis__national_liberation_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(zionist_legitimacy_basis__national_liberation_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(zionist_legitimacy_basis__national_liberation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.65) reflects the ongoing transfer of land and resources from Palestinians, justified by the national liberation narrative. Suppression (0.75) is high due to the active military, legal, and diplomatic efforts to control Palestinian populations and delegitimize their counter-narratives. The theater ratio (0.20) is low, indicating that the core function of establishing and maintaining a Jewish state is genuinely pursued, though the 'liberation' aspect often serves as a cover for the extractive and suppressive elements.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of Israeli Jewish citizens and Zionist organizations, this is a legitimate national liberation movement, a 'Rope' providing security and self-determination. From the perspective of Palestinian Arabs, it is a 'Snare' of displacement and occupation. The engine's classification as 'Tangled Rope' reflects the dual nature of coordination for one group and extraction/suppression for another, maintained through active enforcement.
 *
 * DIRECTIONALITY LOGIC:
 *   Israeli Jewish citizens and Zionist organizations are primary beneficiaries, experiencing the state as a national home and actively shaping its policies. Palestinian Arabs are the primary victims, bearing the costs of displacement and loss of sovereignty. International critics and some Jewish diaspora members are also targets, facing pressure for challenging the narrative. Western governments are beneficiaries due to geopolitical alignment.
 *
 * MANDATROPHY ANALYSIS:
 *   The national liberation mandate is still considered 'live' by its proponents, preventing a Mandatrophy resolution. However, the increasing extractiveness and suppression over time, particularly post-1967, suggest a drift where the 'liberation' aspect increasingly serves to legitimize ongoing expansion and control, rather than solely providing refuge. The 'contested' status of the founding problem highlights this tension.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    indigeneity_claim_ambiguity,
    'Is the Jewish claim to indigeneity in the land of Israel structurally equivalent to other indigenous claims, or is it primarily a historical-religious claim that functions differently in a modern nation-state context?',
    'Comparative legal and anthropological analysis of indigenous rights frameworks, focusing on land tenure, self-determination, and the impact of return on existing populations.',
    'If the indigeneity claim is found to be structurally distinct or to function primarily as a justification for displacement, it would weaken the national liberation framing and increase the perceived extractiveness and suppression of the constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(indigeneity_claim_ambiguity, conceptual, 'Ambiguity in the structural function of the Jewish indigeneity claim.').

omega_variable(
    legitimacy_of_resistance_ambiguity,
    'Is Palestinian opposition to Zionism primarily a denial of Jewish national rights, or a legitimate resistance to settler-colonialism and occupation?',
    'International legal rulings on self-determination and occupation, and historical analysis of the motivations and actions of both sides, independent of national narratives.',
    'If Palestinian opposition is recognized as legitimate resistance, it would fundamentally challenge the national liberation reading''s justification for suppression and extraction, reclassifying the constraint closer to a ''Snare'' from a global perspective.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legitimacy_of_resistance_ambiguity, preference, 'Ambiguity in the legitimacy of Palestinian resistance.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(zionist_legitimacy_basis__national_liberation_reading, 1948, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(zion_tr_t1948, zionist_legitimacy_basis__national_liberation_reading, theater_ratio, 1948, 0.1).
narrative_ontology:measurement(zion_tr_t1967, zionist_legitimacy_basis__national_liberation_reading, theater_ratio, 1967, 0.15).
narrative_ontology:measurement(zion_tr_t1993, zionist_legitimacy_basis__national_liberation_reading, theater_ratio, 1993, 0.18).
narrative_ontology:measurement(zion_tr_t2024, zionist_legitimacy_basis__national_liberation_reading, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(zion_be_t1948, zionist_legitimacy_basis__national_liberation_reading, base_extractiveness, 1948, 0.55).
narrative_ontology:measurement(zion_be_t1967, zionist_legitimacy_basis__national_liberation_reading, base_extractiveness, 1967, 0.6).
narrative_ontology:measurement(zion_be_t1993, zionist_legitimacy_basis__national_liberation_reading, base_extractiveness, 1993, 0.63).
narrative_ontology:measurement(zion_be_t2024, zionist_legitimacy_basis__national_liberation_reading, base_extractiveness, 2024, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(zion_su_t1948, zionist_legitimacy_basis__national_liberation_reading, suppression_requirement, 1948, 0.6).
narrative_ontology:measurement(zion_su_t1967, zionist_legitimacy_basis__national_liberation_reading, suppression_requirement, 1967, 0.7).
narrative_ontology:measurement(zion_su_t1993, zionist_legitimacy_basis__national_liberation_reading, suppression_requirement, 1993, 0.72).
narrative_ontology:measurement(zion_su_t2024, zionist_legitimacy_basis__national_liberation_reading, suppression_requirement, 2024, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(zionist_legitimacy_basis__national_liberation_reading, identity_coordination).
narrative_ontology:affects_constraint(zionist_legitimacy_basis__national_liberation_reading, zionist_legitimacy_basis__settler_colonial_reading).
narrative_ontology:affects_constraint(zionist_legitimacy_basis__national_liberation_reading, zionist_legitimacy_basis__religious_restoration_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'zionist_legitimacy_basis' kernel. It focuses on the national liberation aspect, distinct from settler-colonial or religious restoration readings, though all are deeply intertwined in practice.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
