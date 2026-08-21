% ============================================================================
% CONSTRAINT STORY: all_men_created_equal__textualist_paradox_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_all_men_created_equal__textualist_paradox_reading, []).

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
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: all_men_created_equal__textualist_paradox_reading
 *   human_readable: Textualist Paradox of 'All Men Are Created Equal'
 *   domain: constitutional_law/political_philosophy/american_studies
 *
 * SUMMARY:
 *   This constraint represents the textualist paradox inherent in the
 *   Declaration of Independence's phrase 'all men are created equal' when
 *   confronted with its historically restricted application. It highlights
 *   the performative contradiction between universal language and limited
 *   practice, serving as a critical tool against rigid originalist
 *   interpretations. This is one reading of the 'all_men_created_equal'
 *   kernel, focusing on the internal textual tension.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(all_men_created_equal__textualist_paradox_reading, 0.65).
domain_priors:suppression_score(all_men_created_equal__textualist_paradox_reading, 0.4).
domain_priors:theater_ratio(all_men_created_equal__textualist_paradox_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(all_men_created_equal__textualist_paradox_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(all_men_created_equal__textualist_paradox_reading, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(all_men_created_equal__textualist_paradox_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(all_men_created_equal__textualist_paradox_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(all_men_created_equal__textualist_paradox_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(all_men_created_equal__textualist_paradox_reading, tangled_rope).
narrative_ontology:human_readable(all_men_created_equal__textualist_paradox_reading, "Textualist Paradox of 'All Men Are Created Equal'").
narrative_ontology:topic_domain(all_men_created_equal__textualist_paradox_reading, "constitutional_law/political_philosophy/american_studies").

domain_priors:requires_active_enforcement(all_men_created_equal__textualist_paradox_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(all_men_created_equal__textualist_paradox_reading, '412b4c6a-3bfe-4461-9384-b6390aca35be').
narrative_ontology:cs_kernel_codification('412b4c6a-3bfe-4461-9384-b6390aca35be', fixed_text).
narrative_ontology:cs_authority_grounding('412b4c6a-3bfe-4461-9384-b6390aca35be', lineage).
narrative_ontology:cs_interpretation_layer_present('412b4c6a-3bfe-4461-9384-b6390aca35be').
narrative_ontology:cs_reading_relation('412b4c6a-3bfe-4461-9384-b6390aca35be', all_men_created_equal__originalist_reading, influences).
narrative_ontology:cs_reading_relation('412b4c6a-3bfe-4461-9384-b6390aca35be', all_men_created_equal__universalist_reading, coexists_with).
narrative_ontology:cs_axiom('412b4c6a-3bfe-4461-9384-b6390aca35be', foundational, universal_language_implies_universal_application).
narrative_ontology:cs_axiom_status(universal_language_implies_universal_application, holdable).
narrative_ontology:cs_axiom_grounding('412b4c6a-3bfe-4461-9384-b6390aca35be', universal_language_implies_universal_application, deontological).
narrative_ontology:cs_axiom('412b4c6a-3bfe-4461-9384-b6390aca35be', foundational, historical_practice_reveals_performative_contradiction).
narrative_ontology:cs_axiom_status(historical_practice_reveals_performative_contradiction, holdable).
narrative_ontology:cs_axiom_grounding('412b4c6a-3bfe-4461-9384-b6390aca35be', historical_practice_reveals_performative_contradiction, empirically_contingent).
narrative_ontology:cs_reference_frame('412b4c6a-3bfe-4461-9384-b6390aca35be', textual_integrity_and_coherence).
narrative_ontology:cs_drift_state('412b4c6a-3bfe-4461-9384-b6390aca35be', contemporary_critical_scholarship, gap(axiom_overriding, substantial, true)).
narrative_ontology:cs_created_at('412b4c6a-3bfe-4461-9384-b6390aca35be', '').
narrative_ontology:cs_kernel_id(all_men_created_equal__textualist_paradox_reading, all_men_created_equal).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(all_men_created_equal__textualist_paradox_reading, critical_legal_scholars).
narrative_ontology:constraint_beneficiary(all_men_created_equal__textualist_paradox_reading, civil_rights_advocates).
narrative_ontology:constraint_victim(all_men_created_equal__textualist_paradox_reading, originalist_interpretive_framework).
narrative_ontology:constraint_victim(all_men_created_equal__textualist_paradox_reading, conservative_judiciary).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from the intellectual leverage provided by exposing the inherent contradiction between the universal language of the Declaration and its historically restricted application. Their work gains salience and provides a basis for critique.
narrative_ontology:constraint_stakeholder(all_men_created_equal__textualist_paradox_reading, critical_legal_scholars, beneficiary,
    analytical, generational, analytical, national).

% Utilize the textual paradox to argue for an expansive, inclusive interpretation of equality, challenging historical injustices and advocating for broader rights. The paradox provides a rhetorical and philosophical tool for their activism.
narrative_ontology:constraint_stakeholder(all_men_created_equal__textualist_paradox_reading, civil_rights_advocates, beneficiary,
    organized, generational, constrained, national).

% Bears the cost of delegitimization as its core premise (fidelity to original intent) is undermined by the inherent contradiction in a foundational text. Its authority is challenged by the performative contradiction.
narrative_ontology:constraint_stakeholder(all_men_created_equal__textualist_paradox_reading, originalist_interpretive_framework, payer,
    institutional, civilizational, identity_locked, national).
narrative_ontology:stakeholder_non_agent(all_men_created_equal__textualist_paradox_reading, originalist_interpretive_framework).

% Faces intellectual and public pressure to reconcile its commitment to originalism with the textual paradox. The contradiction complicates their interpretive methodology and exposes them to charges of hypocrisy or selective application.
narrative_ontology:constraint_stakeholder(all_men_created_equal__textualist_paradox_reading, conservative_judiciary, payer,
    institutional, generational, constrained, national).

% Observe the paradox as evidence for their view that the principle of equality inherently demands iterative expansion, regardless of historical intent. They see the textualist paradox as a stepping stone to a fully universalist reading.
narrative_ontology:constraint_stakeholder(all_men_created_equal__textualist_paradox_reading, universalist_interpreters, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a critical lens for analyzing foundational American texts, enabling a shared framework for identifying and challenging historical inconsistencies in the application of universal principles.
% TRANSFER_FUNCTION: Transfers intellectual and moral authority from originalist interpretations to critical and progressive readings of foundational texts, by exposing inherent contradictions.
% ABSENT_VOICES: The historical beneficiaries of restricted equality (e.g., slaveholders, those who benefited from gender or racial exclusion) are absent from contemporary discourse, but their historical actions are the very subject of the paradox.
% DISAPPEARANCE_RATIONALE: If the textualist paradox vanished (e.g., if the Declaration's language were explicitly qualified historically), a major tool for challenging originalist authority and advocating for expansive rights would be lost. The intellectual and political landscape of constitutional interpretation would fundamentally shift.
% FOUNDING_PROBLEM: The problem of reconciling the aspirational, universal language of the Declaration of Independence with the founders' own practices and the restricted application of equality in the early republic.
% FOUNDING_PROBLEM_CORROBORATION: Historians, political philosophers, and legal scholars across various ideological spectrums acknowledge the historical discrepancy, even if they differ on its interpretive implications. The problem is widely recognized as a central tension in American political thought.
narrative_ontology:disappearance_verdict(all_men_created_equal__textualist_paradox_reading, world_rearranges).
narrative_ontology:founding_problem_status(all_men_created_equal__textualist_paradox_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(all_men_created_equal__textualist_paradox_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(all_men_created_equal__textualist_paradox_reading, 'none', 1).
narrative_ontology:epsilon_provenance(all_men_created_equal__textualist_paradox_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(all_men_created_equal__textualist_paradox_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(all_men_created_equal__textualist_paradox_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(all_men_created_equal__textualist_paradox_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.65) stems from the intellectual and political cost imposed on interpretive frameworks that attempt to reconcile the universal text with historical particularism without acknowledging the contradiction. Suppression (0.40) is moderate; while originalist frameworks actively resist this reading, the paradox itself is a conceptual tool that cannot be fully suppressed. Theater ratio (0.20) is low, as the contradiction is a genuine structural feature, not a performance. The rising extractiveness over time reflects increasing scholarly and public awareness of this paradox.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of originalist interpreters, this reading is an attack on constitutional fidelity, while from critical perspectives, it is an essential truth about American founding documents. The constraint's 'tangled_rope' classification reflects this dual function: it coordinates critical analysis while extracting legitimacy from those who deny the contradiction.
 *
 * DIRECTIONALITY LOGIC:
 *   Critical legal scholars and civil rights advocates are beneficiaries, as this reading provides a powerful argument for their positions. The originalist interpretive framework and conservative judiciary are victims, as their authority is challenged by the paradox. Universalist interpreters are observers, seeing this paradox as a step towards their broader goals.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    originalist_reconciliation_possibility,
    'Can originalist interpretive frameworks genuinely reconcile the universal language of ''all men are created equal'' with its restricted historical application without acknowledging a performative contradiction?',
    'Analysis of new originalist scholarship: if a coherent, non-contradictory reconciliation is widely accepted by non-originalist scholars, the paradox''s force diminishes.',
    'If reconciliation is possible, the extractiveness from originalist frameworks would decrease, potentially shifting this constraint towards a ''rope'' for critical analysis rather than a ''tangled_rope'' that extracts legitimacy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(originalist_reconciliation_possibility, conceptual, 'Whether originalism can resolve the textual paradox internally.').

omega_variable(
    paradox_as_catalyst_or_trap,
    'Does the textualist paradox primarily serve as a catalyst for progressive change (as intended by beneficiaries) or as an intellectual trap that endlessly re-litigates founding contradictions without resolution?',
    'Longitudinal study of legal and political outcomes: if the paradox consistently correlates with concrete expansions of rights, it''s a catalyst; if it leads to interpretive stasis, it''s a trap.',
    'If a trap, the ''beneficiaries'' might be misclassified, as their ''benefit'' is primarily intellectual rather than practical, potentially reducing the overall extractiveness and shifting the constraint towards a ''piton'' of academic debate.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(paradox_as_catalyst_or_trap, empirical, 'The practical impact of the paradox on social change.').

omega_variable(
    kernel_reading_identity,
    'This constraint is the ''textualist_paradox_reading'' of the ''all_men_created_equal'' kernel. What would change structurally if a sibling reading (e.g., ''originalist_reading'') were adopted as the primary interpretive frame?',
    'Conceptual analysis of interpretive shifts: if the ''originalist_reading'' became dominant, the ''originalist_interpretive_framework'' would cease to be a victim and would instead become a beneficiary, as its authority would be affirmed.',
    'The directionality for originalist frameworks would flip from target to beneficiary, and the overall extractiveness of this specific ''textualist_paradox_reading'' constraint would diminish, as its critical force would be blunted.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Impact of adopting a sibling kernel reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(all_men_created_equal__textualist_paradox_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(all__tr_t0, all_men_created_equal__textualist_paradox_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(all__tr_t10, all_men_created_equal__textualist_paradox_reading, theater_ratio, 10, 0.13).
narrative_ontology:measurement(all__tr_t20, all_men_created_equal__textualist_paradox_reading, theater_ratio, 20, 0.16).
narrative_ontology:measurement(all__tr_t30, all_men_created_equal__textualist_paradox_reading, theater_ratio, 30, 0.18).
narrative_ontology:measurement(all__tr_t40, all_men_created_equal__textualist_paradox_reading, theater_ratio, 40, 0.19).
narrative_ontology:measurement(all__tr_t50, all_men_created_equal__textualist_paradox_reading, theater_ratio, 50, 0.2).

% Extraction over time
narrative_ontology:measurement(all__be_t0, all_men_created_equal__textualist_paradox_reading, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(all__be_t10, all_men_created_equal__textualist_paradox_reading, base_extractiveness, 10, 0.55).
narrative_ontology:measurement(all__be_t20, all_men_created_equal__textualist_paradox_reading, base_extractiveness, 20, 0.6).
narrative_ontology:measurement(all__be_t30, all_men_created_equal__textualist_paradox_reading, base_extractiveness, 30, 0.63).
narrative_ontology:measurement(all__be_t40, all_men_created_equal__textualist_paradox_reading, base_extractiveness, 40, 0.64).
narrative_ontology:measurement(all__be_t50, all_men_created_equal__textualist_paradox_reading, base_extractiveness, 50, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(all__su_t0, all_men_created_equal__textualist_paradox_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(all__su_t10, all_men_created_equal__textualist_paradox_reading, suppression_requirement, 10, 0.33).
narrative_ontology:measurement(all__su_t20, all_men_created_equal__textualist_paradox_reading, suppression_requirement, 20, 0.36).
narrative_ontology:measurement(all__su_t30, all_men_created_equal__textualist_paradox_reading, suppression_requirement, 30, 0.38).
narrative_ontology:measurement(all__su_t40, all_men_created_equal__textualist_paradox_reading, suppression_requirement, 40, 0.39).
narrative_ontology:measurement(all__su_t50, all_men_created_equal__textualist_paradox_reading, suppression_requirement, 50, 0.4).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(all_men_created_equal__textualist_paradox_reading, identity_coordination).
narrative_ontology:affects_constraint(all_men_created_equal__textualist_paradox_reading, all_men_created_equal__originalist_reading).
narrative_ontology:affects_constraint(all_men_created_equal__textualist_paradox_reading, all_men_created_equal__universalist_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'all_men_created_equal' kernel. This 'textualist_paradox_reading' focuses on the internal contradiction, while the 'originalist_reading' emphasizes founder intent and the 'universalist_reading' emphasizes iterative expansion of the principle.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
