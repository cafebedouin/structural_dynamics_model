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
 *   confronted with its restricted historical application. It is a reading
 *   that highlights the performative contradiction between universal language
 *   and exclusionary practice, thereby delegitimizing interpretive frameworks
 *   that seek to reconcile the two without acknowledging the tension. The
 *   constraint is claimed as a Tangled Rope because it serves a coordination
 *   function (providing a critical lens) while extracting from (undermining)
 *   originalist authority.
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
narrative_ontology:cs_story_uid(all_men_created_equal__textualist_paradox_reading, 'c0627b6c-6f1d-4b8a-a9ef-1ceefa45f21c').
narrative_ontology:cs_kernel_codification('c0627b6c-6f1d-4b8a-a9ef-1ceefa45f21c', fixed_text).
narrative_ontology:cs_authority_grounding('c0627b6c-6f1d-4b8a-a9ef-1ceefa45f21c', lineage).
narrative_ontology:cs_interpretation_layer_present('c0627b6c-6f1d-4b8a-a9ef-1ceefa45f21c').
narrative_ontology:cs_reading_relation('c0627b6c-6f1d-4b8a-a9ef-1ceefa45f21c', all_men_created_equal__originalist_reading, influences).
narrative_ontology:cs_reading_relation('c0627b6c-6f1d-4b8a-a9ef-1ceefa45f21c', all_men_created_equal__universalist_reading, coexists_with).
narrative_ontology:cs_axiom('c0627b6c-6f1d-4b8a-a9ef-1ceefa45f21c', foundational, universal_language_demands_universal_application).
narrative_ontology:cs_axiom_status(universal_language_demands_universal_application, holdable).
narrative_ontology:cs_axiom_grounding('c0627b6c-6f1d-4b8a-a9ef-1ceefa45f21c', universal_language_demands_universal_application, deontological).
narrative_ontology:cs_axiom('c0627b6c-6f1d-4b8a-a9ef-1ceefa45f21c', foundational, historical_practice_cannot_override_textual_meaning).
narrative_ontology:cs_axiom_status(historical_practice_cannot_override_textual_meaning, holdable).
narrative_ontology:cs_axiom_grounding('c0627b6c-6f1d-4b8a-a9ef-1ceefa45f21c', historical_practice_cannot_override_textual_meaning, conventional).
narrative_ontology:cs_reference_frame('c0627b6c-6f1d-4b8a-a9ef-1ceefa45f21c', textual_integrity_and_coherence).
narrative_ontology:cs_drift_state('c0627b6c-6f1d-4b8a-a9ef-1ceefa45f21c', contemporary_critical_analysis, gap(axiom_overriding, substantial, true)).
narrative_ontology:cs_created_at('c0627b6c-6f1d-4b8a-a9ef-1ceefa45f21c', '').
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

% Benefit from the intellectual leverage provided by exposing the inherent contradiction between the universal language of the Declaration and its historical application. Their work gains salience and provides a basis for critique.
narrative_ontology:constraint_stakeholder(all_men_created_equal__textualist_paradox_reading, critical_legal_scholars, beneficiary,
    analytical, generational, analytical, national).

% Utilize the textual paradox to argue for an expansive, inclusive interpretation of equality, challenging restrictive applications based on historical intent. This reading provides a powerful rhetorical and legal tool for their advocacy.
narrative_ontology:constraint_stakeholder(all_men_created_equal__textualist_paradox_reading, civil_rights_advocates, beneficiary,
    organized, generational, constrained, national).

% Bears the cost of delegitimization as its core premise (fidelity to original intent) is shown to be in performative contradiction with the very text it purports to interpret. This reading undermines its authority and coherence.
narrative_ontology:constraint_stakeholder(all_men_created_equal__textualist_paradox_reading, originalist_interpretive_framework, payer,
    institutional, civilizational, identity_locked, national).

% Faces challenges to its legitimacy when its rulings, grounded in originalist principles, are shown to perpetuate the historical contradiction. This creates internal tension and external pressure for re-evaluation.
narrative_ontology:constraint_stakeholder(all_men_created_equal__textualist_paradox_reading, conservative_judiciary, payer,
    institutional, generational, constrained, national).

% Observe and often agree with the textualist paradox, using it as a stepping stone to argue for a fully universalist reading of equality that transcends historical limitations and original intent. They are not directly extracted from but are aligned with the critique.
narrative_ontology:constraint_stakeholder(all_men_created_equal__textualist_paradox_reading, universalist_interpreters, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a critical analysis of foundational American texts, providing a shared framework for understanding and challenging historical inconsistencies in the application of universal principles.
% TRANSFER_FUNCTION: Transfers intellectual and moral authority from originalist interpretations to critical and progressive ones, by exposing the internal incoherence of the former.
% ABSENT_VOICES: Those who uncritically accept the historical application of the Declaration without acknowledging the textual paradox are effectively silenced by the force of the contradiction itself; their position is rendered intellectually untenable within this framework.
% DISAPPEARANCE_RATIONALE: If the textualist paradox vanished, the intellectual and legal landscape of American constitutionalism would fundamentally shift. Originalist arguments would gain coherence, and critical challenges to historical injustices based on textual inconsistency would lose a powerful tool, altering the dynamics of legal and political debate.
% FOUNDING_PROBLEM: The problem of reconciling the aspirational, universal language of the Declaration of Independence with the historical reality of slavery and other forms of exclusion at the nation's founding.
% FOUNDING_PROBLEM_CORROBORATION: Historians, political philosophers, and legal scholars across the ideological spectrum acknowledge the historical tension. While interpretations of its implications differ, the existence of the paradox itself is widely corroborated by academic consensus outside of any single interpretive camp.
narrative_ontology:disappearance_verdict(all_men_created_equal__textualist_paradox_reading, world_rearranges).
narrative_ontology:founding_problem_status(all_men_created_equal__textualist_paradox_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(all_men_created_equal__textualist_paradox_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
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
 *   Extractiveness is moderate-high because this reading actively undermines the intellectual and moral capital of originalist frameworks, forcing them to either adapt or lose coherence. Suppression is moderate, as this reading doesn't physically prevent originalist arguments but rather makes them harder to defend intellectually and morally. Resistance is high because originalist proponents actively push back against this critique. The rising extractiveness over time reflects the increasing salience of this paradox in contemporary legal and political discourse, particularly as civil rights movements gain ground.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of critical scholars, this reading is a necessary intellectual tool for justice. From the perspective of originalists, it is a destructive attack on foundational principles. The engine's classification will reflect this divergence, showing a beneficial outcome for the former and an extractive one for the latter.
 *
 * DIRECTIONALITY LOGIC:
 *   Critical legal scholars and civil rights advocates are beneficiaries, as this reading provides them with powerful tools for critique and advocacy. The originalist interpretive framework and conservative judiciary are victims, as their authority and legitimacy are challenged by the exposure of this paradox. Universalist interpreters are observers, aligned with the critique but not directly extracted from or benefiting in the same structural way.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification prevents mislabeling the critique as pure extraction. While it extracts from originalist authority, it also coordinates a shared understanding of a historical contradiction, which is a genuine intellectual function. It's not a Snare because it doesn't suppress alternatives through coercion, but rather through intellectual force. It's not a Rope because of the clear victims and active enforcement (intellectual and rhetorical defense of the paradox).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    originalist_framework_resilience,
    'How resilient is the originalist interpretive framework to this textualist critique? Can it adapt without fundamentally altering its core tenets?',
    'Longitudinal study of originalist legal scholarship and judicial opinions: observe whether new arguments emerge that successfully reconcile the paradox within an originalist framework, or if the framework''s influence wanes.',
    'If highly resilient, the extraction from the originalist framework is lower than measured, suggesting the critique is less effective. If it crumbles, the extraction is higher, indicating a more profound delegitimization.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(originalist_framework_resilience, empirical, 'The capacity of originalism to absorb or deflect the textualist paradox.').

omega_variable(
    paradox_as_coordination_or_extraction,
    'Is the primary function of highlighting this paradox to coordinate a more honest historical understanding, or to extract legitimacy from opposing interpretive frameworks?',
    'Analysis of the rhetorical and strategic use of the paradox by different actors: does it primarily lead to constructive dialogue and historical re-evaluation, or to the dismissal of opposing views?',
    'If primarily coordination, the extractiveness might be slightly lower, reflecting a more balanced intellectual exchange. If primarily extraction, the current extractiveness is accurate or even understated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(paradox_as_coordination_or_extraction, conceptual, 'The primary intent and effect of deploying the textualist paradox.').

omega_variable(
    kernel_stability_under_paradox,
    'Does the exposure of this textualist paradox fundamentally destabilize the ''all men are created equal'' kernel itself, or only specific readings of it?',
    'Philosophical analysis of the kernel''s inherent meaning: can a universal principle retain its force even if its historical application was flawed, or does the flaw invalidate the principle itself?',
    'If the kernel itself is destabilized, the impact is far broader, potentially undermining the moral foundation of the nation. If only specific readings are affected, the impact is contained to interpretive debates.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_stability_under_paradox, conceptual, 'Whether the paradox undermines the kernel or only its interpretations.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(all_men_created_equal__textualist_paradox_reading, 1960, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(all__tr_t1960, all_men_created_equal__textualist_paradox_reading, theater_ratio, 1960, 0.1).
narrative_ontology:measurement(all__tr_t1975, all_men_created_equal__textualist_paradox_reading, theater_ratio, 1975, 0.15).
narrative_ontology:measurement(all__tr_t1990, all_men_created_equal__textualist_paradox_reading, theater_ratio, 1990, 0.18).
narrative_ontology:measurement(all__tr_t2005, all_men_created_equal__textualist_paradox_reading, theater_ratio, 2005, 0.19).
narrative_ontology:measurement(all__tr_t2024, all_men_created_equal__textualist_paradox_reading, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(all__be_t1960, all_men_created_equal__textualist_paradox_reading, base_extractiveness, 1960, 0.4).
narrative_ontology:measurement(all__be_t1975, all_men_created_equal__textualist_paradox_reading, base_extractiveness, 1975, 0.5).
narrative_ontology:measurement(all__be_t1990, all_men_created_equal__textualist_paradox_reading, base_extractiveness, 1990, 0.58).
narrative_ontology:measurement(all__be_t2005, all_men_created_equal__textualist_paradox_reading, base_extractiveness, 2005, 0.62).
narrative_ontology:measurement(all__be_t2024, all_men_created_equal__textualist_paradox_reading, base_extractiveness, 2024, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(all__su_t1960, all_men_created_equal__textualist_paradox_reading, suppression_requirement, 1960, 0.25).
narrative_ontology:measurement(all__su_t1975, all_men_created_equal__textualist_paradox_reading, suppression_requirement, 1975, 0.3).
narrative_ontology:measurement(all__su_t1990, all_men_created_equal__textualist_paradox_reading, suppression_requirement, 1990, 0.35).
narrative_ontology:measurement(all__su_t2005, all_men_created_equal__textualist_paradox_reading, suppression_requirement, 2005, 0.38).
narrative_ontology:measurement(all__su_t2024, all_men_created_equal__textualist_paradox_reading, suppression_requirement, 2024, 0.4).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(all_men_created_equal__textualist_paradox_reading, identity_coordination).
narrative_ontology:affects_constraint(all_men_created_equal__textualist_paradox_reading, all_men_created_equal__originalist_reading).
narrative_ontology:affects_constraint(all_men_created_equal__textualist_paradox_reading, all_men_created_equal__universalist_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'all_men_created_equal' kernel. Its exposure of performative contradiction directly influences the legitimacy and coherence of both originalist and universalist readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
