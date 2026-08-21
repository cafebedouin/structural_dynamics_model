% ============================================================================
% CONSTRAINT STORY: rbio_practice_norm_complex__sovereignty_maximalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_rbio_practice_norm_complex__sovereignty_maximalist_reading, []).

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
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   constraint_id: rbio_practice_norm_complex__sovereignty_maximalist_reading
 *   human_readable: Sovereignty Maximalist Reading of RBIO Norms
 *   domain: international_relations/international_law/political_economy
 *
 * SUMMARY:
 *   This constraint represents the 'sovereignty maximalist' reading of the
 *   RBIO (Rules-Based International Order) practice-norm complex. It asserts
 *   that state sovereignty is absolute, international norms are legitimate
 *   only when they protect this sovereignty, and humanitarian interventions
 *   are pretexts for regime change. This reading prioritizes state
 *   non-interference above all else, effectively shielding authoritarian
 *   regimes from external accountability for internal human rights abuses.
 *   This is one reading of the 'rbio_practice_norm_complex' kernel, alongside
 *   'liberal_institutional_reading' and 'hegemonic_extraction_reading'.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(rbio_practice_norm_complex__sovereignty_maximalist_reading, 0.85).
domain_priors:suppression_score(rbio_practice_norm_complex__sovereignty_maximalist_reading, 0.9).
domain_priors:theater_ratio(rbio_practice_norm_complex__sovereignty_maximalist_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(rbio_practice_norm_complex__sovereignty_maximalist_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(rbio_practice_norm_complex__sovereignty_maximalist_reading, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(rbio_practice_norm_complex__sovereignty_maximalist_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(rbio_practice_norm_complex__sovereignty_maximalist_reading, accessibility_collapse, 0.95).
narrative_ontology:constraint_metric(rbio_practice_norm_complex__sovereignty_maximalist_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(rbio_practice_norm_complex__sovereignty_maximalist_reading, snare).
narrative_ontology:human_readable(rbio_practice_norm_complex__sovereignty_maximalist_reading, "Sovereignty Maximalist Reading of RBIO Norms").
narrative_ontology:topic_domain(rbio_practice_norm_complex__sovereignty_maximalist_reading, "international_relations/international_law/political_economy").

domain_priors:requires_active_enforcement(rbio_practice_norm_complex__sovereignty_maximalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(rbio_practice_norm_complex__sovereignty_maximalist_reading, '36c968ac-d170-414d-89ca-b257c3f1be70').
narrative_ontology:cs_kernel_codification('36c968ac-d170-414d-89ca-b257c3f1be70', fixed_text).
narrative_ontology:cs_authority_grounding('36c968ac-d170-414d-89ca-b257c3f1be70', extraction).
narrative_ontology:cs_interpretation_layer_present('36c968ac-d170-414d-89ca-b257c3f1be70').
narrative_ontology:cs_reading_relation('36c968ac-d170-414d-89ca-b257c3f1be70', rbio_practice_norm_complex__liberal_institutional_reading, coexists_with).
narrative_ontology:cs_reading_relation('36c968ac-d170-414d-89ca-b257c3f1be70', rbio_practice_norm_complex__hegemonic_extraction_reading, coexists_with).
narrative_ontology:cs_axiom('36c968ac-d170-414d-89ca-b257c3f1be70', foundational, state_sovereignty_absolute).
narrative_ontology:cs_axiom_status(state_sovereignty_absolute, holdable).
narrative_ontology:cs_axiom_grounding('36c968ac-d170-414d-89ca-b257c3f1be70', state_sovereignty_absolute, deontological).
narrative_ontology:cs_axiom('36c968ac-d170-414d-89ca-b257c3f1be70', foundational, humanitarian_intervention_pretext_for_regime_change).
narrative_ontology:cs_axiom_status(humanitarian_intervention_pretext_for_regime_change, holdable).
narrative_ontology:cs_axiom_grounding('36c968ac-d170-414d-89ca-b257c3f1be70', humanitarian_intervention_pretext_for_regime_change, instrumental).
narrative_ontology:cs_reference_frame('36c968ac-d170-414d-89ca-b257c3f1be70', westphalian_order_un_charter_era).
narrative_ontology:cs_drift_state('36c968ac-d170-414d-89ca-b257c3f1be70', contemporary_r2p_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('36c968ac-d170-414d-89ca-b257c3f1be70', '').
narrative_ontology:cs_kernel_id(rbio_practice_norm_complex__sovereignty_maximalist_reading, rbio_practice_norm_complex).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(rbio_practice_norm_complex__sovereignty_maximalist_reading, authoritarian_regimes).
narrative_ontology:constraint_beneficiary(rbio_practice_norm_complex__sovereignty_maximalist_reading, state_elites).
narrative_ontology:constraint_victim(rbio_practice_norm_complex__sovereignty_maximalist_reading, populations_under_repressive_governments).
narrative_ontology:constraint_victim(rbio_practice_norm_complex__sovereignty_maximalist_reading, human_rights_advocates).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(rbio_practice_norm_complex__sovereignty_maximalist_reading, hegemonic_powers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Actively assert and defend the maximalist interpretation of sovereignty to shield themselves from external scrutiny and intervention. They benefit directly from the lack of accountability for internal affairs.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__sovereignty_maximalist_reading, authoritarian_regimes, agenda_setter,
    institutional, generational, arbitrage, global).

% Benefit from the stability and impunity that absolute sovereignty provides, allowing them to consolidate power and manage internal dissent without fear of international repercussions. Their personal wealth and security are tied to the regime's survival.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__sovereignty_maximalist_reading, state_elites, beneficiary,
    powerful, biographical, mobile, national).

% Bear the full cost of this constraint, trapped under repressive governments with no legitimate external recourse for human rights abuses. Their voices are systematically suppressed internally and ignored externally due to the non-interference principle.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__sovereignty_maximalist_reading, populations_under_repressive_governments, payer,
    powerless, immediate, trapped, national).
narrative_ontology:stakeholder_secondary_role(rbio_practice_norm_complex__sovereignty_maximalist_reading, populations_under_repressive_governments, excluded).

% Work to expose abuses and advocate for intervention or accountability, but their efforts are consistently blocked by the maximalist sovereignty claim. They pay in terms of resources, diplomatic capital, and moral frustration.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__sovereignty_maximalist_reading, human_rights_advocates, payer,
    organized, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(rbio_practice_norm_complex__sovereignty_maximalist_reading, human_rights_advocates, excluded).

% Observe and critique the maximalist reading, advocating for a more conditional view of sovereignty tied to human rights. They are often frustrated by the practical limitations imposed by this reading on international action.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__sovereignty_maximalist_reading, liberal_institutionalists, observer,
    institutional, generational, analytical, global).

% While often critiquing authoritarian regimes, these powers can also selectively invoke sovereignty maximalism when it serves their geopolitical interests to avoid intervention or to shield allies. They benefit from the flexibility this interpretation offers.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__sovereignty_maximalist_reading, hegemonic_powers, beneficiary,
    institutional, generational, arbitrage, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(rbio_practice_norm_complex__sovereignty_maximalist_reading, authoritarian_regimes).
narrative_ontology:fixing_cost_class(rbio_practice_norm_complex__sovereignty_maximalist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates state non-interference in the internal affairs of other states, aiming to prevent external aggression and maintain international order based on territorial integrity.
% TRANSFER_FUNCTION: Transfers security and impunity from vulnerable populations to state elites and authoritarian regimes, by shielding them from external accountability and intervention. It also transfers legitimacy from universal human rights norms to state power.
% ABSENT_VOICES: Populations under repressive governments are structurally excluded from the international conversation; their pleas for external assistance are dismissed as 'internal affairs.' Human rights advocates are present but systematically marginalized by the maximalist framing.
% DISAPPEARANCE_RATIONALE: If this maximalist reading vanished overnight, there would be immediate and intense pressure for international intervention in states with severe human rights abuses. The international legal framework would be reinterpreted to prioritize human security over state sovereignty, leading to a fundamental reorganization of global governance and state accountability.
% FOUNDING_PROBLEM: The founding problem was to prevent colonial and imperial interference in newly independent states and to establish a stable international order based on the principle of sovereign equality and non-aggression.
% FOUNDING_PROBLEM_CORROBORATION: Sovereignty maximalists (e.g., certain states, legal scholars) attest that the problem of external interference remains live, citing historical and contemporary examples of neo-imperialism. Liberal institutionalists and human rights groups argue that while the original problem was valid, the maximalist interpretation has been repurposed to shield human rights abuses, and that the original problem is largely solved or has evolved into new forms of intervention that require different responses. Independent analyses of state behavior and international law support the contested status.
narrative_ontology:disappearance_verdict(rbio_practice_norm_complex__sovereignty_maximalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(rbio_practice_norm_complex__sovereignty_maximalist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(rbio_practice_norm_complex__sovereignty_maximalist_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(rbio_practice_norm_complex__sovereignty_maximalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(rbio_practice_norm_complex__sovereignty_maximalist_reading, 0.85, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(rbio_practice_norm_complex__sovereignty_maximalist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(rbio_practice_norm_complex__sovereignty_maximalist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(rbio_practice_norm_complex__sovereignty_maximalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.85) is very high because this reading enables regimes to extract resources, labor, and compliance from their populations without external checks. Suppression (0.90) is also very high, as it actively legitimizes internal state suppression and blocks external intervention, effectively collapsing alternatives for trapped populations. The theater ratio (0.20) is low because, while diplomatic language may be used, the core function of this reading is a hardline defense of state power, with little performative maintenance of a coordination function that isn't primarily extractive. Accessibility collapse is near total for victims (0.95) as external avenues are closed. Resistance (0.60) is moderate, coming from human rights groups and liberal states, but often ineffective against the entrenched power of this interpretation.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of authoritarian regimes, this constraint is a legitimate defense of national self-determination and a bulwark against neo-imperialism. From the perspective of trapped populations and human rights advocates, it is a snare that enables severe human rights abuses by denying external recourse. The engine's classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Authoritarian regimes and state elites are clear beneficiaries, as the constraint directly protects their power and impunity (low directionality). Populations under repressive governments are the primary victims, bearing the full cost of unchecked state power (high directionality). Human rights advocates and liberal institutionalists are payers, expending resources and diplomatic capital against a deeply entrenched norm. Hegemonic powers can be beneficiaries when this reading aligns with their non-interventionist interests.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification as a Snare prevents mislabeling the maximalist sovereignty claim as a legitimate Rope or Mountain. While the original intent of non-interference (founding problem) had a coordination function, this reading has amplified the extractive component to the point where the coordination story serves as cover for the extraction of impunity by state actors from their own populations. The high extractiveness and suppression, coupled with the identification of clear victims, are key to this distinction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is this constraint a distinct reading of the RBIO practice-norm complex, or merely a rhetorical position within a broader contest?',
    'Analysis of state practice and legal arguments: if this reading consistently guides state behavior and legal interpretations in a structurally distinct way from other readings, it is a distinct constraint.',
    'If not a distinct constraint, its metrics would be subsumed into a broader, more ambiguous ''rbio_practice_norm_complex'' constraint, obscuring its specific extractive dynamics.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Confirms the distinct structural identity of this reading within the RBIO kernel.').

omega_variable(
    intervention_legitimacy_ambiguity,
    'Is the maximalist non-intervention principle a necessary shield against neo-imperialism, or does it primarily serve as a cover for human rights abuses?',
    'Empirical analysis of intervention outcomes: if interventions consistently lead to worse outcomes than non-intervention, the maximalist claim gains empirical support. If non-intervention consistently enables severe abuses, the claim weakens.',
    'Resolution would shift the perceived legitimacy of the constraint, potentially altering its effective suppression and extractiveness by changing the ''cover story'' effectiveness.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intervention_legitimacy_ambiguity, empirical, 'Ambiguity regarding the true function of non-intervention.').

omega_variable(
    conditionality_exit_cost,
    'Can states genuinely exit from international conditionality (e.g., aid, trade agreements) without prohibitive costs, or is the ''choice'' to accept conditionality often coerced?',
    'Case studies of states attempting to exit conditional agreements: if exit is consistently followed by severe economic or political penalties, the claim of ''voluntary consent'' is undermined.',
    'If exit is prohibitively costly, the constraint''s suppression is higher than measured, as apparent alternatives are illusory. This would further solidify its Snare classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(conditionality_exit_cost, empirical, 'Whether exit from international conditionality is truly cost-free.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(rbio_practice_norm_complex__sovereignty_maximalist_reading, 1945, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(rbio_tr_t1945, rbio_practice_norm_complex__sovereignty_maximalist_reading, theater_ratio, 1945, 0.3).
narrative_ontology:measurement(rbio_tr_t1965, rbio_practice_norm_complex__sovereignty_maximalist_reading, theater_ratio, 1965, 0.25).
narrative_ontology:measurement(rbio_tr_t1985, rbio_practice_norm_complex__sovereignty_maximalist_reading, theater_ratio, 1985, 0.22).
narrative_ontology:measurement(rbio_tr_t2005, rbio_practice_norm_complex__sovereignty_maximalist_reading, theater_ratio, 2005, 0.2).
narrative_ontology:measurement(rbio_tr_t2024, rbio_practice_norm_complex__sovereignty_maximalist_reading, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(rbio_be_t1945, rbio_practice_norm_complex__sovereignty_maximalist_reading, base_extractiveness, 1945, 0.6).
narrative_ontology:measurement(rbio_be_t1965, rbio_practice_norm_complex__sovereignty_maximalist_reading, base_extractiveness, 1965, 0.7).
narrative_ontology:measurement(rbio_be_t1985, rbio_practice_norm_complex__sovereignty_maximalist_reading, base_extractiveness, 1985, 0.78).
narrative_ontology:measurement(rbio_be_t2005, rbio_practice_norm_complex__sovereignty_maximalist_reading, base_extractiveness, 2005, 0.82).
narrative_ontology:measurement(rbio_be_t2024, rbio_practice_norm_complex__sovereignty_maximalist_reading, base_extractiveness, 2024, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(rbio_su_t1945, rbio_practice_norm_complex__sovereignty_maximalist_reading, suppression_requirement, 1945, 0.7).
narrative_ontology:measurement(rbio_su_t1965, rbio_practice_norm_complex__sovereignty_maximalist_reading, suppression_requirement, 1965, 0.78).
narrative_ontology:measurement(rbio_su_t1985, rbio_practice_norm_complex__sovereignty_maximalist_reading, suppression_requirement, 1985, 0.85).
narrative_ontology:measurement(rbio_su_t2005, rbio_practice_norm_complex__sovereignty_maximalist_reading, suppression_requirement, 2005, 0.88).
narrative_ontology:measurement(rbio_su_t2024, rbio_practice_norm_complex__sovereignty_maximalist_reading, suppression_requirement, 2024, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(rbio_practice_norm_complex__sovereignty_maximalist_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(rbio_practice_norm_complex__sovereignty_maximalist_reading, un_security_council_veto).
narrative_ontology:affects_constraint(rbio_practice_norm_complex__sovereignty_maximalist_reading, r2p_doctrine).
narrative_ontology:affects_constraint(rbio_practice_norm_complex__sovereignty_maximalist_reading, liberal_institutional_reading).
narrative_ontology:affects_constraint(rbio_practice_norm_complex__sovereignty_maximalist_reading, hegemonic_extraction_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three distinct readings of the 'rbio_practice_norm_complex' kernel, each with different structural properties and classifications. They are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
