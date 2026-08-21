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
 *   human_readable: Nuclear Deterrence Credibility Paradox
 *   domain: strategic_studies/international_relations
 *
 * SUMMARY:
 *   This constraint describes the 'credibility paradox' reading of nuclear
 *   deterrence, where the threat of nuclear use, necessary for deterrence, is
 *   inherently incredible due to the guaranteed mutual destruction. This
 *   reading emphasizes the instability of deterrence and the continuous
 *   search by great powers for 'usable' nuclear options (counterforce,
 *   limited war), suggesting that the 'unthinkability' of nuclear war is
 *   largely rhetorical, not structural. War remains reachable via escalation
 *   ladders, making deterrence a precarious and actively managed constraint.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(nuclear_impossibility_kernel__credibility_paradox_reading, 0.65).
domain_priors:suppression_score(nuclear_impossibility_kernel__credibility_paradox_reading, 0.78).
domain_priors:theater_ratio(nuclear_impossibility_kernel__credibility_paradox_reading, 0.85).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(nuclear_impossibility_kernel__credibility_paradox_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(nuclear_impossibility_kernel__credibility_paradox_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(nuclear_impossibility_kernel__credibility_paradox_reading, theater_ratio, 0.85).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(nuclear_impossibility_kernel__credibility_paradox_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(nuclear_impossibility_kernel__credibility_paradox_reading, resistance, 0.9).

% --- Constraint claim ---
narrative_ontology:constraint_claim(nuclear_impossibility_kernel__credibility_paradox_reading, tangled_rope).
narrative_ontology:human_readable(nuclear_impossibility_kernel__credibility_paradox_reading, "Nuclear Deterrence Credibility Paradox").
narrative_ontology:topic_domain(nuclear_impossibility_kernel__credibility_paradox_reading, "strategic_studies/international_relations").

domain_priors:requires_active_enforcement(nuclear_impossibility_kernel__credibility_paradox_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(nuclear_impossibility_kernel__credibility_paradox_reading, '77d53794-26d5-43a6-bf7d-e6db3cb1d633').
narrative_ontology:cs_kernel_codification('77d53794-26d5-43a6-bf7d-e6db3cb1d633', implicit).
narrative_ontology:cs_authority_grounding('77d53794-26d5-43a6-bf7d-e6db3cb1d633', extraction).
narrative_ontology:cs_interpretation_layer_present('77d53794-26d5-43a6-bf7d-e6db3cb1d633').
narrative_ontology:cs_reading_relation('77d53794-26d5-43a6-bf7d-e6db3cb1d633', nuclear_impossibility_kernel__structural_contraction_reading, coexists_with).
narrative_ontology:cs_reading_relation('77d53794-26d5-43a6-bf7d-e6db3cb1d633', nuclear_impossibility_kernel__rational_dropout_reading, coexists_with).
narrative_ontology:cs_axiom('77d53794-26d5-43a6-bf7d-e6db3cb1d633', foundational, nuclear_threat_inherently_incredible).
narrative_ontology:cs_axiom_status(nuclear_threat_inherently_incredible, holdable).
narrative_ontology:cs_axiom_grounding('77d53794-26d5-43a6-bf7d-e6db3cb1d633', nuclear_threat_inherently_incredible, deontological).
narrative_ontology:cs_axiom('77d53794-26d5-43a6-bf7d-e6db3cb1d633', secondary, escalation_to_total_war_is_likely).
narrative_ontology:cs_axiom_status(escalation_to_total_war_is_likely, holdable).
narrative_ontology:cs_axiom_grounding('77d53794-26d5-43a6-bf7d-e6db3cb1d633', escalation_to_total_war_is_likely, empirically_contingent).
narrative_ontology:cs_reference_frame('77d53794-26d5-43a6-bf7d-e6db3cb1d633', cold_war_deterrence_doctrine).
narrative_ontology:cs_drift_state('77d53794-26d5-43a6-bf7d-e6db3cb1d633', post_cold_war_proliferation_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('77d53794-26d5-43a6-bf7d-e6db3cb1d633', '').
narrative_ontology:cs_kernel_id(nuclear_impossibility_kernel__credibility_paradox_reading, nuclear_impossibility_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(nuclear_impossibility_kernel__credibility_paradox_reading, nuclear_powers_strategic_planners).
narrative_ontology:constraint_victim(nuclear_impossibility_kernel__credibility_paradox_reading, global_population).
narrative_ontology:constraint_victim(nuclear_impossibility_kernel__credibility_paradox_reading, non_nuclear_states).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Responsible for maintaining credible deterrence postures, which requires developing and articulating scenarios for nuclear use, despite the inherent paradox. Their professional identity is tied to managing this impossible threat.
narrative_ontology:constraint_stakeholder(nuclear_impossibility_kernel__credibility_paradox_reading, nuclear_powers_strategic_planners, agenda_setter,
    institutional, generational, identity_locked, global).

% Lives under the constant, if low-probability, threat of nuclear annihilation. Bears the psychological and economic costs of maintaining nuclear arsenals and the risk of their failure. Has no direct agency in the decision-making.
narrative_ontology:constraint_stakeholder(nuclear_impossibility_kernel__credibility_paradox_reading, global_population, payer,
    powerless, immediate, trapped, universal).

% Subject to the strategic calculations of nuclear powers without possessing the means to influence them directly. May seek their own nuclear capabilities as a result, or align with nuclear powers for protection, incurring costs in sovereignty or resources.
narrative_ontology:constraint_stakeholder(nuclear_impossibility_kernel__credibility_paradox_reading, non_nuclear_states, payer,
    moderate, biographical, constrained, global).

% Analyze the paradox and advocate for disarmament or stricter controls, arguing that the system is inherently unstable and prone to failure. Their work aims to expose the theatricality of deterrence.
narrative_ontology:constraint_stakeholder(nuclear_impossibility_kernel__credibility_paradox_reading, arms_control_advocates, observer,
    organized, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The constraint coordinates the behavior of nuclear-armed states by creating a shared understanding that nuclear use is catastrophic, thus deterring large-scale conventional war between them.
% TRANSFER_FUNCTION: Transfers a sense of 'security' (from conventional war) to nuclear powers, at the cost of existential risk and resource expenditure from the global population and non-nuclear states.
% ABSENT_VOICES: Future generations, who bear the long-term risk of nuclear proliferation and environmental contamination, are entirely absent. Their interests would argue for immediate and complete disarmament.
% DISAPPEARANCE_RATIONALE: If the credibility paradox vanished (e.g., nuclear weapons became credibly usable without mutual destruction), the world would rearrange into a new, highly unstable strategic environment where nuclear war might become a 'thinkable' option, leading to rapid rearmament and potentially direct conflict.
% FOUNDING_PROBLEM: Preventing large-scale conventional war between great powers by making the costs of such conflict unacceptably high.
% FOUNDING_PROBLEM_CORROBORATION: Nuclear powers and many international relations theorists attest that the founding problem of great power war remains live, and nuclear deterrence is still seen as the primary mechanism preventing it. Arms control advocates contest the efficacy and safety of this solution.
narrative_ontology:disappearance_verdict(nuclear_impossibility_kernel__credibility_paradox_reading, world_rearranges).
narrative_ontology:founding_problem_status(nuclear_impossibility_kernel__credibility_paradox_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(nuclear_impossibility_kernel__credibility_paradox_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(nuclear_impossibility_kernel__credibility_paradox_reading, 'none', 1).
narrative_ontology:epsilon_provenance(nuclear_impossibility_kernel__credibility_paradox_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

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
 *   Extractiveness is high (0.65) because the system demands constant investment in arsenals and strategic planning, imposing existential risk on all. Suppression is high (0.78) as the system actively suppresses alternatives to nuclear deterrence (e.g., global disarmament) through institutional inertia and the perceived necessity of maintaining a 'balance of terror.' Theater ratio is very high (0.85) because the 'threat' of nuclear use is largely performative; its actual execution would negate its purpose. The system is maintained through elaborate rituals of threat signaling and counter-signaling, rather than genuine intent to use. Accessibility collapse is moderate (0.4) because while direct alternatives to nuclear deterrence are suppressed, the theoretical possibility of disarmament or alternative security architectures remains, albeit difficult to achieve. Resistance is high (0.9) due to persistent anti-nuclear movements and the inherent instability of the system, which generates continuous pressure for change or collapse.
 *
 * PERSPECTIVAL GAP:
 *   Strategic planners experience this as a complex, high-stakes coordination problem they must manage, justifying their roles and resources. The global population experiences it as an existential threat and a drain on resources, with no clear benefit. The divergence highlights how the same constraint can be perceived as a necessary evil by those who manage it, and a pure extraction by those who bear its ultimate costs.
 *
 * DIRECTIONALITY LOGIC:
 *   Nuclear powers' strategic planners are identity-locked beneficiaries, as their professional existence and national security doctrines are built around managing this paradox. The global population and non-nuclear states are victims, bearing the costs and risks without agency. Arms control advocates are observers, analyzing the system's flaws.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    usability_of_nuclear_weapons,
    'To what extent are ''limited'' nuclear war scenarios or counterforce strikes genuinely feasible and controllable, rather than inevitably escalating to full-scale exchange?',
    'Historical analysis of crisis escalation, wargaming simulations with independent expert review, and declassified strategic planning documents.',
    'If limited use is genuinely feasible, the credibility paradox is weakened, and the system moves closer to a ''rational dropout'' scenario where use is possible but costly. If escalation is inevitable, the paradox holds, and the theatricality of deterrence is reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(usability_of_nuclear_weapons, empirical, 'The empirical possibility of controlled nuclear escalation.').

omega_variable(
    rhetoric_vs_structural_unthinkability,
    'Is the ''unthinkability'' of nuclear war a genuine structural constraint on decision-makers, or primarily a rhetorical device used to manage public perception and internal dissent?',
    'Analysis of declassified decision-making transcripts during crises, psychological studies of leaders under extreme pressure, and comparison of public statements with private strategic planning.',
    'If unthinkability is purely rhetorical, the theater ratio is higher, and the system is more prone to miscalculation. If it''s a genuine structural constraint, the system is more stable than this reading suggests.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(rhetoric_vs_structural_unthinkability, conceptual, 'Distinguishing rhetorical from structural constraints on nuclear use.').

omega_variable(
    kernel_reading_divergence,
    'Given the ''nuclear impossibility kernel,'' what is the primary point of divergence between the ''credibility paradox'' reading and the ''structural contraction'' reading?',
    'Conceptual analysis of the core axioms of each reading and their implications for the possibility of ''victory'' or ''limited use'' in nuclear conflict.',
    'The ''credibility paradox'' reading emphasizes the *psychological and political* impossibility of credible threats, while the ''structural contraction'' reading emphasizes the *physical* impossibility of any rational outcome. Resolving this clarifies whether the constraint is primarily a human-made strategic dilemma or an irreducible physical limit.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_divergence, conceptual, 'Clarifying the core difference between two readings of the nuclear impossibility kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(nuclear_impossibility_kernel__credibility_paradox_reading, 1945, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nucl_tr_t1945, nuclear_impossibility_kernel__credibility_paradox_reading, theater_ratio, 1945, 0.3).
narrative_ontology:measurement(nucl_tr_t1962, nuclear_impossibility_kernel__credibility_paradox_reading, theater_ratio, 1962, 0.7).
narrative_ontology:measurement(nucl_tr_t1980, nuclear_impossibility_kernel__credibility_paradox_reading, theater_ratio, 1980, 0.8).
narrative_ontology:measurement(nucl_tr_t1991, nuclear_impossibility_kernel__credibility_paradox_reading, theater_ratio, 1991, 0.9).
narrative_ontology:measurement(nucl_tr_t2010, nuclear_impossibility_kernel__credibility_paradox_reading, theater_ratio, 2010, 0.88).
narrative_ontology:measurement(nucl_tr_t2024, nuclear_impossibility_kernel__credibility_paradox_reading, theater_ratio, 2024, 0.85).

% Extraction over time
narrative_ontology:measurement(nucl_be_t1945, nuclear_impossibility_kernel__credibility_paradox_reading, base_extractiveness, 1945, 0.5).
narrative_ontology:measurement(nucl_be_t1962, nuclear_impossibility_kernel__credibility_paradox_reading, base_extractiveness, 1962, 0.7).
narrative_ontology:measurement(nucl_be_t1980, nuclear_impossibility_kernel__credibility_paradox_reading, base_extractiveness, 1980, 0.65).
narrative_ontology:measurement(nucl_be_t1991, nuclear_impossibility_kernel__credibility_paradox_reading, base_extractiveness, 1991, 0.55).
narrative_ontology:measurement(nucl_be_t2010, nuclear_impossibility_kernel__credibility_paradox_reading, base_extractiveness, 2010, 0.6).
narrative_ontology:measurement(nucl_be_t2024, nuclear_impossibility_kernel__credibility_paradox_reading, base_extractiveness, 2024, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(nucl_su_t1945, nuclear_impossibility_kernel__credibility_paradox_reading, suppression_requirement, 1945, 0.6).
narrative_ontology:measurement(nucl_su_t1962, nuclear_impossibility_kernel__credibility_paradox_reading, suppression_requirement, 1962, 0.8).
narrative_ontology:measurement(nucl_su_t1980, nuclear_impossibility_kernel__credibility_paradox_reading, suppression_requirement, 1980, 0.75).
narrative_ontology:measurement(nucl_su_t1991, nuclear_impossibility_kernel__credibility_paradox_reading, suppression_requirement, 1991, 0.65).
narrative_ontology:measurement(nucl_su_t2010, nuclear_impossibility_kernel__credibility_paradox_reading, suppression_requirement, 2010, 0.7).
narrative_ontology:measurement(nucl_su_t2024, nuclear_impossibility_kernel__credibility_paradox_reading, suppression_requirement, 2024, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(nuclear_impossibility_kernel__credibility_paradox_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(nuclear_impossibility_kernel__credibility_paradox_reading, nuclear_impossibility_kernel__structural_contraction_reading).
narrative_ontology:affects_constraint(nuclear_impossibility_kernel__credibility_paradox_reading, nuclear_impossibility_kernel__rational_dropout_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'nuclear impossibility kernel.' It focuses on the inherent incredibility of nuclear threats, influencing (and being influenced by) other readings that emphasize physical impossibility or rational cost-benefit analysis.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
