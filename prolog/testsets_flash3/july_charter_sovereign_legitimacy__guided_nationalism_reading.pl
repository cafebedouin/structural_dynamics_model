% ============================================================================
% CONSTRAINT STORY: july_charter_sovereign_legitimacy__guided_nationalism_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_july_charter_sovereign_legitimacy__guided_nationalism_reading, []).

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
 *   constraint_id: july_charter_sovereign_legitimacy__guided_nationalism_reading
 *   human_readable: July Charter's Islamic-Nationalist Sovereign Legitimacy (Guided Nationalism Reading)
 *   domain: constitutional_law/political_transitions/state_building
 *
 * SUMMARY:
 *   This constraint describes the 'guided nationalism' reading of a
 *   post-revolutionary July Charter, where the Charter establishes an
 *   Islamic-nationalist framework with religious identity as the primary
 *   ground for sovereign legitimacy. This reading leads to the constitutional
 *   constraint of secular institutions, the elevation of religious law or
 *   norms, and the marginalization of secular civil society and religious
 *   minorities. The claimed type is 'snare' because the coordination story
 *   (national unity) is cover for substantial extraction and suppression of
 *   specific groups.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(july_charter_sovereign_legitimacy__guided_nationalism_reading, 0.85).
domain_priors:suppression_score(july_charter_sovereign_legitimacy__guided_nationalism_reading, 0.92).
domain_priors:theater_ratio(july_charter_sovereign_legitimacy__guided_nationalism_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(july_charter_sovereign_legitimacy__guided_nationalism_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(july_charter_sovereign_legitimacy__guided_nationalism_reading, suppression_requirement, 0.92).
narrative_ontology:constraint_metric(july_charter_sovereign_legitimacy__guided_nationalism_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(july_charter_sovereign_legitimacy__guided_nationalism_reading, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(july_charter_sovereign_legitimacy__guided_nationalism_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(july_charter_sovereign_legitimacy__guided_nationalism_reading, snare).
narrative_ontology:human_readable(july_charter_sovereign_legitimacy__guided_nationalism_reading, "July Charter's Islamic-Nationalist Sovereign Legitimacy (Guided Nationalism Reading)").
narrative_ontology:topic_domain(july_charter_sovereign_legitimacy__guided_nationalism_reading, "constitutional_law/political_transitions/state_building").

domain_priors:requires_active_enforcement(july_charter_sovereign_legitimacy__guided_nationalism_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(july_charter_sovereign_legitimacy__guided_nationalism_reading, '766fff17-d8b0-41c1-8434-602152130399').
narrative_ontology:cs_kernel_codification('766fff17-d8b0-41c1-8434-602152130399', formalized).
narrative_ontology:cs_authority_grounding('766fff17-d8b0-41c1-8434-602152130399', extraction).
narrative_ontology:cs_interpretation_layer_present('766fff17-d8b0-41c1-8434-602152130399').
narrative_ontology:cs_reading_relation('766fff17-d8b0-41c1-8434-602152130399', july_charter_sovereign_legitimacy__secular_democratic_reading, forecloses).
narrative_ontology:cs_reading_relation('766fff17-d8b0-41c1-8434-602152130399', july_charter_sovereign_legitimacy__military_custodian_reading, influences).
narrative_ontology:cs_axiom('766fff17-d8b0-41c1-8434-602152130399', foundational, islamic_identity_as_sovereign_ground).
narrative_ontology:cs_axiom_status(islamic_identity_as_sovereign_ground, holdable).
narrative_ontology:cs_axiom_grounding('766fff17-d8b0-41c1-8434-602152130399', islamic_identity_as_sovereign_ground, deontological).
narrative_ontology:cs_axiom('766fff17-d8b0-41c1-8434-602152130399', secondary, national_unity_through_religious_homogeneity).
narrative_ontology:cs_axiom_status(national_unity_through_religious_homogeneity, holdable).
narrative_ontology:cs_axiom_grounding('766fff17-d8b0-41c1-8434-602152130399', national_unity_through_religious_homogeneity, instrumental).
narrative_ontology:cs_reference_frame('766fff17-d8b0-41c1-8434-602152130399', post_revolutionary_islamic_state).
narrative_ontology:cs_drift_state('766fff17-d8b0-41c1-8434-602152130399', contemporary_global_liberal_order, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('766fff17-d8b0-41c1-8434-602152130399', '').
narrative_ontology:cs_kernel_id(july_charter_sovereign_legitimacy__guided_nationalism_reading, july_charter_sovereign_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(july_charter_sovereign_legitimacy__guided_nationalism_reading, islamic_nationalist_elites).
narrative_ontology:constraint_beneficiary(july_charter_sovereign_legitimacy__guided_nationalism_reading, religious_establishment).
narrative_ontology:constraint_victim(july_charter_sovereign_legitimacy__guided_nationalism_reading, secular_civil_society).
narrative_ontology:constraint_victim(july_charter_sovereign_legitimacy__guided_nationalism_reading, religious_minorities).
narrative_ontology:constraint_victim(july_charter_sovereign_legitimacy__guided_nationalism_reading, liberal_political_factions).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These elites drafted and promulgated the Charter, consolidating power by framing national identity through an Islamic lens. They benefit from the constitutional elevation of religious identity as the primary source of sovereign legitimacy, which marginalizes secular opposition and entrenches their authority. They actively enforce the Charter's provisions.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__guided_nationalism_reading, islamic_nationalist_elites, agenda_setter,
    institutional, generational, arbitrage, national).

% Benefits from the Charter's provisions that grant religious law or norms constitutional status, expanding its influence over public life, education, and legal interpretation. Its authority is enhanced, and its institutions receive state support and protection. Exit options are constrained by its deep integration into the state apparatus.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__guided_nationalism_reading, religious_establishment, beneficiary,
    organized, generational, constrained, national).

% Bears the cost of the Charter's framework, which constrains secular institutions, limits freedom of expression, and marginalizes non-religious political discourse. Their advocacy for a secular state is delegitimized, and their organizations face legal and social pressure. Exit options are severely limited, often leading to emigration or suppression.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__guided_nationalism_reading, secular_civil_society, payer,
    powerless, biographical, trapped, national).

% Experience the Charter as a source of legal and social discrimination, as their religious identity is not recognized as part of the sovereign legitimacy. They face restrictions on practice, representation, and equal citizenship. Their identity is locked into the national fabric, making exit a profound personal and communal loss.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__guided_nationalism_reading, religious_minorities, payer,
    powerless, generational, identity_locked, national).

% Advocate for a more inclusive, democratic, and rights-based constitutional order. The Charter's Islamic-nationalist framework directly undermines their political program, limiting their ability to organize, contest elections, and influence policy. Their options are to conform, resist at high cost, or face political irrelevance.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__guided_nationalism_reading, liberal_political_factions, payer,
    moderate, biographical, constrained, national).

% Monitor the implementation of the Charter and its impact on human rights, particularly for secular civil society and religious minorities. They document abuses, issue reports, and advocate for international pressure or intervention, but have no direct enforcement power over the Charter's operation.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__guided_nationalism_reading, international_human_rights_organizations, observer,
    institutional, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Aims to unify a post-revolutionary society by establishing a shared national identity and legal framework rooted in Islamic principles, providing a clear source of authority and social cohesion after a period of upheaval.
% TRANSFER_FUNCTION: Transfers political and legal authority from secular or pluralistic institutions to those aligned with the Islamic-nationalist framework, channeling resources and legitimacy to the religious establishment and its elites, while extracting rights and autonomy from secular and minority groups.
% ABSENT_VOICES: Secular intellectuals, women's rights advocates, and non-Islamic religious leaders are systematically marginalized or suppressed; they would articulate alternative visions of national identity and constitutional order, but their platforms are restricted or silenced.
% DISAPPEARANCE_RATIONALE: If the Charter's Islamic-nationalist framework vanished overnight, the entire political and social order would undergo a profound rearrangement. Secular and minority groups would immediately assert their rights, the religious establishment would lose its constitutional backing, and a new contest over national identity and governance would erupt, leading to significant instability and re-alignment of power.
% FOUNDING_PROBLEM: The nation faced a crisis of identity and legitimacy after a period of political instability and foreign influence, requiring a strong, unifying framework to establish a stable post-revolutionary state.
% FOUNDING_PROBLEM_CORROBORATION: Islamic-nationalist elites and the religious establishment assert the problem is live, citing ongoing threats to national identity and moral order. Secular civil society and international observers argue the founding problem has been re-framed to justify an extractive power grab, with independent analyses pointing to the suppression of dissent as the primary function, not genuine unification.
narrative_ontology:disappearance_verdict(july_charter_sovereign_legitimacy__guided_nationalism_reading, world_rearranges).
narrative_ontology:founding_problem_status(july_charter_sovereign_legitimacy__guided_nationalism_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(july_charter_sovereign_legitimacy__guided_nationalism_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(july_charter_sovereign_legitimacy__guided_nationalism_reading, 'none', 1).
narrative_ontology:epsilon_provenance(july_charter_sovereign_legitimacy__guided_nationalism_reading, 0.85, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(july_charter_sovereign_legitimacy__guided_nationalism_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(july_charter_sovereign_legitimacy__guided_nationalism_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(july_charter_sovereign_legitimacy__guided_nationalism_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The high extractiveness (0.85) reflects the significant loss of rights, autonomy, and political space for secular and minority groups. Suppression (0.92) is severe, as the state actively enforces the Islamic-nationalist framework, limiting dissent and alternative political expression. The theater ratio (0.40) indicates that while some unifying rhetoric exists, a substantial portion of the Charter's operation is dedicated to maintaining the extractive power structure rather than genuine national cohesion. Accessibility collapse is high (0.75) because the constitutional framework itself delegitimizes alternatives, and resistance (0.70) is significant but met with strong state coercion.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setting elites perceive the Charter as a legitimate and necessary framework for national stability and identity, a 'rope' that unifies. The victim groups experience it as a 'snare' that systematically extracts from them. The engine's classification will reflect this divergence based on the structural data provided.
 *
 * DIRECTIONALITY LOGIC:
 *   Islamic-nationalist elites and the religious establishment are clear beneficiaries and agenda-setters, experiencing low directionality (d near 0.0) as the constraint subsidizes their power. Secular civil society, religious minorities, and liberal political factions are direct targets (d near 1.0), bearing the costs of the constraint's operation. International human rights organizations act as analytical observers.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as a 'snare' prevents mislabeling this as a genuine coordination mechanism (rope) or a temporary support (scaffold). The 'guided nationalism' framing provides a coordination narrative, but the high extractiveness, active suppression, and identifiable victims reveal its true nature. The persistence is due to active enforcement and suppression of alternatives, not a self-sustaining coordination function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    charter_true_intent,
    'Is the Charter''s primary intent genuinely national unification and stability, or is it a strategic re-framing to consolidate power for specific factions?',
    'Historical analysis of pre-Charter negotiations, internal documents of drafting committees, and comparative analysis with other post-revolutionary constitutional processes where power consolidation was a documented outcome.',
    'If primarily power consolidation, the ''snare'' classification is strongly reinforced, and the coordination function is further exposed as theatrical. If genuine unification was the primary intent, the extractiveness might be re-evaluated as a high cost of coordination, potentially shifting towards a ''tangled_rope'' if the coordination function is robust.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(charter_true_intent, conceptual, 'Ambiguity regarding the Charter''s foundational purpose: genuine coordination vs. power consolidation.').

omega_variable(
    suppression_internalized_vs_structural,
    'To what extent is the suppression experienced by secular civil society and religious minorities structural (legal barriers, state coercion) versus internalized (self-censorship, identity fusion with the dominant narrative)?',
    'Post-exit trajectory analysis for individuals who emigrate or find safe spaces for dissent: if suppression persists as self-censorship or identity conflict after leaving the coercive environment, it indicates internalized suppression. Surveys and ethnographic studies within affected communities.',
    'If internalized suppression is significant, the effective suppression is higher than the structural measure suggests, as victims carry the suppression with them. This would deepen the ''snare'' classification by highlighting the constraint''s pervasive psychological impact.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_internalized_vs_structural, empirical, 'Structural vs. internalized suppression mechanism for victim groups.').

omega_variable(
    legitimacy_grounding_contestation,
    'Is the Islamic-nationalist grounding of sovereign legitimacy genuinely accepted by a broad majority, or is it maintained through active suppression of dissent and a manufactured consensus?',
    'Independent, anonymous public opinion surveys (if feasible), analysis of protest movements and their suppression, and the degree of state investment in propaganda and ideological enforcement.',
    'If acceptance is manufactured, the ''snare'' classification is strengthened, as the constraint''s persistence relies more heavily on coercion than on genuine popular consent. If broad acceptance is demonstrated, the constraint might be re-evaluated as a ''tangled_rope'' with a more robust, albeit still extractive, coordination function.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(legitimacy_grounding_contestation, empirical, 'The true extent of popular acceptance of the Charter''s legitimacy grounding.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(july_charter_sovereign_legitimacy__guided_nationalism_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(july_tr_t0, july_charter_sovereign_legitimacy__guided_nationalism_reading, theater_ratio, 0, 0.3).
narrative_ontology:measurement(july_tr_t5, july_charter_sovereign_legitimacy__guided_nationalism_reading, theater_ratio, 5, 0.33).
narrative_ontology:measurement(july_tr_t10, july_charter_sovereign_legitimacy__guided_nationalism_reading, theater_ratio, 10, 0.36).
narrative_ontology:measurement(july_tr_t15, july_charter_sovereign_legitimacy__guided_nationalism_reading, theater_ratio, 15, 0.38).
narrative_ontology:measurement(july_tr_t20, july_charter_sovereign_legitimacy__guided_nationalism_reading, theater_ratio, 20, 0.4).

% Extraction over time
narrative_ontology:measurement(july_be_t0, july_charter_sovereign_legitimacy__guided_nationalism_reading, base_extractiveness, 0, 0.75).
narrative_ontology:measurement(july_be_t5, july_charter_sovereign_legitimacy__guided_nationalism_reading, base_extractiveness, 5, 0.79).
narrative_ontology:measurement(july_be_t10, july_charter_sovereign_legitimacy__guided_nationalism_reading, base_extractiveness, 10, 0.82).
narrative_ontology:measurement(july_be_t15, july_charter_sovereign_legitimacy__guided_nationalism_reading, base_extractiveness, 15, 0.84).
narrative_ontology:measurement(july_be_t20, july_charter_sovereign_legitimacy__guided_nationalism_reading, base_extractiveness, 20, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(july_su_t0, july_charter_sovereign_legitimacy__guided_nationalism_reading, suppression_requirement, 0, 0.8).
narrative_ontology:measurement(july_su_t5, july_charter_sovereign_legitimacy__guided_nationalism_reading, suppression_requirement, 5, 0.85).
narrative_ontology:measurement(july_su_t10, july_charter_sovereign_legitimacy__guided_nationalism_reading, suppression_requirement, 10, 0.89).
narrative_ontology:measurement(july_su_t15, july_charter_sovereign_legitimacy__guided_nationalism_reading, suppression_requirement, 15, 0.91).
narrative_ontology:measurement(july_su_t20, july_charter_sovereign_legitimacy__guided_nationalism_reading, suppression_requirement, 20, 0.92).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(july_charter_sovereign_legitimacy__guided_nationalism_reading, identity_coordination).
narrative_ontology:affects_constraint(july_charter_sovereign_legitimacy__guided_nationalism_reading, july_charter_sovereign_legitimacy__secular_democratic_reading).
narrative_ontology:affects_constraint(july_charter_sovereign_legitimacy__guided_nationalism_reading, july_charter_sovereign_legitimacy__military_custodian_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'July Charter Sovereign Legitimacy' kernel. Its Islamic-nationalist framework directly influences the operational space and legitimacy of secular-democratic and military-custodian readings, often by foreclosing their core premises or creating structural pressure against them.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
