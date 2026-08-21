% ============================================================================
% CONSTRAINT STORY: nuclear_impossibility_kernel__credibility_paradox_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_nuclear_impossibility_kernel__credibility_paradox_reading, []).

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
 *   constraint_id: nuclear_impossibility_kernel__credibility_paradox_reading
 *   human_readable: Nuclear Deterrence Credibility Paradox (Credibility Paradox Reading)
 *   domain: strategic_studies/international_relations/nuclear_deterrence_theory
 *
 * SUMMARY:
 *   This constraint describes the 'credibility paradox' reading of the
 *   nuclear impossibility kernel. It posits that nuclear deterrence, while
 *   aiming for stability, is inherently unstable because the threat of use,
 *   necessary for deterrence, is incredible given the consequences of mutual
 *   destruction. This reading emphasizes that great powers, despite the
 *   paradox, actively seek 'usable' nuclear options (e.g., counterforce,
 *   limited war scenarios) and that the 'unthinkability' of nuclear war is
 *   often rhetorical rather than a structural reality, leaving war reachable
 *   via escalation. The constraint is claimed as a Rope by its proponents
 *   (nuclear powers) but operates as a Tangled Rope due to its inherent
 *   extraction and suppression.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(nuclear_impossibility_kernel__credibility_paradox_reading, 0.78).
domain_priors:suppression_score(nuclear_impossibility_kernel__credibility_paradox_reading, 0.85).
domain_priors:theater_ratio(nuclear_impossibility_kernel__credibility_paradox_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(nuclear_impossibility_kernel__credibility_paradox_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(nuclear_impossibility_kernel__credibility_paradox_reading, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(nuclear_impossibility_kernel__credibility_paradox_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(nuclear_impossibility_kernel__credibility_paradox_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(nuclear_impossibility_kernel__credibility_paradox_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(nuclear_impossibility_kernel__credibility_paradox_reading, tangled_rope).
narrative_ontology:human_readable(nuclear_impossibility_kernel__credibility_paradox_reading, "Nuclear Deterrence Credibility Paradox (Credibility Paradox Reading)").
narrative_ontology:topic_domain(nuclear_impossibility_kernel__credibility_paradox_reading, "strategic_studies/international_relations/nuclear_deterrence_theory").

domain_priors:requires_active_enforcement(nuclear_impossibility_kernel__credibility_paradox_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(nuclear_impossibility_kernel__credibility_paradox_reading, '48c79f2e-b4ee-42c6-b31c-fe49b8b16f83').
narrative_ontology:cs_kernel_codification('48c79f2e-b4ee-42c6-b31c-fe49b8b16f83', formalized).
narrative_ontology:cs_authority_grounding('48c79f2e-b4ee-42c6-b31c-fe49b8b16f83', extraction).
narrative_ontology:cs_interpretation_layer_present('48c79f2e-b4ee-42c6-b31c-fe49b8b16f83').
narrative_ontology:cs_reading_relation('48c79f2e-b4ee-42c6-b31c-fe49b8b16f83', nuclear_impossibility_kernel__structural_contraction_reading, forecloses).
narrative_ontology:cs_reading_relation('48c79f2e-b4ee-42c6-b31c-fe49b8b16f83', nuclear_impossibility_kernel__rational_dropout_reading, coexists_with).
narrative_ontology:cs_axiom('48c79f2e-b4ee-42c6-b31c-fe49b8b16f83', foundational, use_threat_must_be_credible).
narrative_ontology:cs_axiom_status(use_threat_must_be_credible, holdable).
narrative_ontology:cs_axiom_grounding('48c79f2e-b4ee-42c6-b31c-fe49b8b16f83', use_threat_must_be_credible, conventional).
narrative_ontology:cs_axiom('48c79f2e-b4ee-42c6-b31c-fe49b8b16f83', secondary, escalation_is_manageable).
narrative_ontology:cs_axiom_status(escalation_is_manageable, holdable).
narrative_ontology:cs_axiom_grounding('48c79f2e-b4ee-42c6-b31c-fe49b8b16f83', escalation_is_manageable, empirically_contingent).
narrative_ontology:cs_reference_frame('48c79f2e-b4ee-42c6-b31c-fe49b8b16f83', flexible_response_doctrine).
narrative_ontology:cs_drift_state('48c79f2e-b4ee-42c6-b31c-fe49b8b16f83', contemporary_strategic_environment, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('48c79f2e-b4ee-42c6-b31c-fe49b8b16f83', '').
narrative_ontology:cs_kernel_id(nuclear_impossibility_kernel__credibility_paradox_reading, nuclear_impossibility_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(nuclear_impossibility_kernel__credibility_paradox_reading, nuclear_powers).
narrative_ontology:constraint_beneficiary(nuclear_impossibility_kernel__credibility_paradox_reading, military_industrial_complex).
narrative_ontology:constraint_victim(nuclear_impossibility_kernel__credibility_paradox_reading, global_population).
narrative_ontology:constraint_victim(nuclear_impossibility_kernel__credibility_paradox_reading, non_nuclear_states).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintain and modernize nuclear arsenals, develop strategic doctrines, and project a credible threat of use to deter adversaries. They benefit from the perceived stability deterrence provides but are constrained by the inherent paradox of use.
narrative_ontology:constraint_stakeholder(nuclear_impossibility_kernel__credibility_paradox_reading, nuclear_powers, agenda_setter,
    institutional, generational, constrained, global).

% Receives immense funding for research, development, and production of nuclear weapons and delivery systems. Benefits from the continuous need to maintain and 'modernize' arsenals, regardless of the paradox.
narrative_ontology:constraint_stakeholder(nuclear_impossibility_kernel__credibility_paradox_reading, military_industrial_complex, beneficiary,
    institutional, generational, arbitrage, global).

% Bears the existential risk of nuclear war, the psychological burden of living under the threat, and the opportunity cost of resources diverted to nuclear arsenals instead of other societal needs. Has no direct exit from the system.
narrative_ontology:constraint_stakeholder(nuclear_impossibility_kernel__credibility_paradox_reading, global_population, payer,
    powerless, immediate, trapped, universal).

% Live under the nuclear umbrella or threat without direct control over the weapons. They bear the risk and often face pressure to align with nuclear powers, diverting their own resources to conventional defense or seeking their own nuclear capabilities. Their exit options are limited by geopolitical realities.
narrative_ontology:constraint_stakeholder(nuclear_impossibility_kernel__credibility_paradox_reading, non_nuclear_states, payer,
    organized, biographical, constrained, global).

% Argue for disarmament, non-proliferation, and alternative security frameworks. Their voices are often marginalized in strategic planning, as the system prioritizes maintaining the deterrence posture.
narrative_ontology:constraint_stakeholder(nuclear_impossibility_kernel__credibility_paradox_reading, arms_control_advocates, excluded,
    moderate, biographical, constrained, global).

% Analyze the dynamics of nuclear deterrence, including the credibility paradox. They develop doctrines and scenarios, influencing policy debates but not directly controlling the weapons or their use.
narrative_ontology:constraint_stakeholder(nuclear_impossibility_kernel__credibility_paradox_reading, strategic_theorists, observer,
    analytical, biographical, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To maintain a precarious global stability by deterring large-scale conventional war between great powers through the threat of unacceptable nuclear retaliation.
% TRANSFER_FUNCTION: Transfers immense financial and intellectual resources to the development and maintenance of nuclear arsenals and strategic doctrines, while transferring existential risk and geopolitical leverage to the global population and non-nuclear states.
% ABSENT_VOICES: Global civil society, future generations, and many non-nuclear states are largely excluded from the core decision-making processes, despite bearing the ultimate risks. They would advocate for disarmament and alternative security paradigms.
% DISAPPEARANCE_RATIONALE: If the nuclear deterrence constraint vanished overnight, the global security architecture would undergo a radical and immediate transformation. It could lead to widespread conventional conflict, a rapid race for new weapons, or a sudden, unprecedented push for genuine disarmament and new forms of international security.
% FOUNDING_PROBLEM: Preventing a third world war between great powers after the devastation of World War II, by making the cost of such a conflict prohibitively high.
% FOUNDING_PROBLEM_CORROBORATION: Nuclear powers and their defense establishments assert that the founding problem of great power war remains live, justifying the continued existence of nuclear deterrence. Critics, including arms control experts and peace movements, argue that the system has become self-perpetuating and creates new, equally dangerous problems, rather than solving the original one, with independent analysis supporting this shifted-function reading.
narrative_ontology:disappearance_verdict(nuclear_impossibility_kernel__credibility_paradox_reading, world_rearranges).
narrative_ontology:founding_problem_status(nuclear_impossibility_kernel__credibility_paradox_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(nuclear_impossibility_kernel__credibility_paradox_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(nuclear_impossibility_kernel__credibility_paradox_reading, 'none', 1).
narrative_ontology:epsilon_provenance(nuclear_impossibility_kernel__credibility_paradox_reading, 0.78, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(nuclear_impossibility_kernel__credibility_paradox_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(nuclear_impossibility_kernel__credibility_paradox_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(nuclear_impossibility_kernel__credibility_paradox_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.78) due to the immense resources consumed by nuclear arsenals and the political capital extracted from non-nuclear states. Suppression is very high (0.85) as the system actively suppresses alternatives like disarmament and non-proliferation efforts, and limits the agency of non-nuclear actors. Theater ratio is low (0.25) because the threat, while paradoxical, is taken seriously by actors, and the maintenance of arsenals and doctrines is a genuine, albeit dangerous, activity. Accessibility collapse is moderate (0.68) because while alternatives are suppressed, they are not entirely foreclosed. Resistance is high (0.75) from peace movements and non-nuclear states.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of nuclear powers, the system is a necessary, albeit dangerous, coordination mechanism for global stability. From the perspective of the global population and non-nuclear states, it is a highly extractive and suppressive system that imposes existential risk and diverts resources, driven by a paradoxical logic.
 *
 * DIRECTIONALITY LOGIC:
 *   Nuclear powers and the military-industrial complex are clear beneficiaries, extracting resources and political influence. The global population and non-nuclear states are victims, bearing the existential risk and opportunity costs. Arms control advocates are excluded, their proposals often sidelined by the dominant strategic narrative.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    credibility_of_limited_use,
    'Can nuclear threats be made genuinely credible through doctrines of limited nuclear war or counterforce strikes, or does any use inevitably lead to full-scale escalation?',
    'Empirical observation of crisis escalation dynamics (if such data were available without catastrophic failure), or advanced game-theoretic modeling incorporating human and systemic irrationality.',
    'If limited use is truly credible, the constraint''s extractiveness and suppression might be seen as more ''functional'' for deterrence. If escalation is inevitable, the system is more purely extractive, riding on a false premise.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(credibility_of_limited_use, empirical, 'The empirical possibility of controlled nuclear escalation.').

omega_variable(
    rhetoric_vs_operational_doctrine,
    'To what extent does the public rhetoric of ''nuclear unthinkability'' genuinely reflect operational doctrine and planning within nuclear powers, versus serving as a strategic communication tool?',
    'Declassification of historical and contemporary strategic planning documents, or whistleblower accounts from within nuclear command structures.',
    'If rhetoric significantly diverges from doctrine, the ''theater_ratio'' of the constraint is higher than currently assessed, indicating more performative maintenance of a dangerous illusion.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(rhetoric_vs_operational_doctrine, empirical, 'The gap between public statements and actual nuclear war planning.').

omega_variable(
    mandatrophy_of_deterrence,
    'Has the original problem of preventing great power war evolved such that nuclear deterrence is now a self-perpetuating system that creates its own problems, rather than solving an external one?',
    'Historical analysis of post-Cold War conflicts and proliferation dynamics, assessing whether nuclear weapons actively prevent conflict or merely shift its form and location.',
    'If the problem is self-perpetuating, the constraint leans more towards a Snare, with its coordination function atrophied and its persistence driven by institutional inertia and extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mandatrophy_of_deterrence, conceptual, 'Whether nuclear deterrence has outlived its original mandate and become self-serving.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(nuclear_impossibility_kernel__credibility_paradox_reading, 1945, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nucl_tr_t1945, nuclear_impossibility_kernel__credibility_paradox_reading, theater_ratio, 1945, 0.1).
narrative_ontology:measurement(nucl_tr_t1965, nuclear_impossibility_kernel__credibility_paradox_reading, theater_ratio, 1965, 0.15).
narrative_ontology:measurement(nucl_tr_t1985, nuclear_impossibility_kernel__credibility_paradox_reading, theater_ratio, 1985, 0.2).
narrative_ontology:measurement(nucl_tr_t2005, nuclear_impossibility_kernel__credibility_paradox_reading, theater_ratio, 2005, 0.23).
narrative_ontology:measurement(nucl_tr_t2024, nuclear_impossibility_kernel__credibility_paradox_reading, theater_ratio, 2024, 0.25).

% Extraction over time
narrative_ontology:measurement(nucl_be_t1945, nuclear_impossibility_kernel__credibility_paradox_reading, base_extractiveness, 1945, 0.6).
narrative_ontology:measurement(nucl_be_t1965, nuclear_impossibility_kernel__credibility_paradox_reading, base_extractiveness, 1965, 0.7).
narrative_ontology:measurement(nucl_be_t1985, nuclear_impossibility_kernel__credibility_paradox_reading, base_extractiveness, 1985, 0.75).
narrative_ontology:measurement(nucl_be_t2005, nuclear_impossibility_kernel__credibility_paradox_reading, base_extractiveness, 2005, 0.76).
narrative_ontology:measurement(nucl_be_t2024, nuclear_impossibility_kernel__credibility_paradox_reading, base_extractiveness, 2024, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(nucl_su_t1945, nuclear_impossibility_kernel__credibility_paradox_reading, suppression_requirement, 1945, 0.7).
narrative_ontology:measurement(nucl_su_t1965, nuclear_impossibility_kernel__credibility_paradox_reading, suppression_requirement, 1965, 0.78).
narrative_ontology:measurement(nucl_su_t1985, nuclear_impossibility_kernel__credibility_paradox_reading, suppression_requirement, 1985, 0.82).
narrative_ontology:measurement(nucl_su_t2005, nuclear_impossibility_kernel__credibility_paradox_reading, suppression_requirement, 2005, 0.83).
narrative_ontology:measurement(nucl_su_t2024, nuclear_impossibility_kernel__credibility_paradox_reading, suppression_requirement, 2024, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(nuclear_impossibility_kernel__credibility_paradox_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(nuclear_impossibility_kernel__credibility_paradox_reading, nuclear_impossibility_kernel__structural_contraction_reading).
narrative_ontology:affects_constraint(nuclear_impossibility_kernel__credibility_paradox_reading, nuclear_impossibility_kernel__rational_dropout_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'nuclear_impossibility_kernel'. This reading (credibility_paradox_reading) focuses on the inherent instability of deterrence due to the incredible nature of the use-threat, and the active pursuit of 'usable' nuclear options. It contrasts with the structural_contraction_reading (which asserts no rational path to victory) and the rational_dropout_reading (which focuses on costs exceeding benefits).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
