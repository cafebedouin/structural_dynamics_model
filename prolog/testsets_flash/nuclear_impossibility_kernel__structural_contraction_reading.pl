% ============================================================================
% CONSTRAINT STORY: nuclear_impossibility_kernel__structural_contraction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_nuclear_impossibility_kernel__structural_contraction_reading, []).

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
    domain_priors:emerges_naturally/1,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
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
 *   constraint_id: nuclear_impossibility_kernel__structural_contraction_reading
 *   human_readable: Nuclear Impossibility: Structural Contraction of War
 *   domain: strategic_studies/international_relations
 *
 * SUMMARY:
 *   This constraint describes the structural contraction of the strategic
 *   space for direct, large-scale war between nuclear-armed states, due to
 *   the physical impossibility of achieving a 'victory' that outweighs the
 *   costs of mutual annihilation. It is a reading of the 'nuclear
 *   impossibility kernel' that emphasizes the physical limits imposed by
 *   nuclear weapons, rather than the rationality of actors or the credibility
 *   of threats. War, in its traditional sense, exits the set of reachable
 *   outcomes for nuclear powers.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(nuclear_impossibility_kernel__structural_contraction_reading, 0.0).
domain_priors:suppression_score(nuclear_impossibility_kernel__structural_contraction_reading, 1.0).
domain_priors:theater_ratio(nuclear_impossibility_kernel__structural_contraction_reading, 0.0).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(nuclear_impossibility_kernel__structural_contraction_reading, extractiveness, 0.0).
narrative_ontology:constraint_metric(nuclear_impossibility_kernel__structural_contraction_reading, suppression_requirement, 1.0).
narrative_ontology:constraint_metric(nuclear_impossibility_kernel__structural_contraction_reading, theater_ratio, 0.0).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(nuclear_impossibility_kernel__structural_contraction_reading, accessibility_collapse, 1.0).
narrative_ontology:constraint_metric(nuclear_impossibility_kernel__structural_contraction_reading, resistance, 0.0).

% --- Constraint claim ---
narrative_ontology:constraint_claim(nuclear_impossibility_kernel__structural_contraction_reading, mountain).
narrative_ontology:human_readable(nuclear_impossibility_kernel__structural_contraction_reading, "Nuclear Impossibility: Structural Contraction of War").
narrative_ontology:topic_domain(nuclear_impossibility_kernel__structural_contraction_reading, "strategic_studies/international_relations").

domain_priors:emerges_naturally(nuclear_impossibility_kernel__structural_contraction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(nuclear_impossibility_kernel__structural_contraction_reading, '8f761fb2-7ee0-4f1d-9e9b-c6b391fd1033').
narrative_ontology:cs_kernel_codification('8f761fb2-7ee0-4f1d-9e9b-c6b391fd1033', implicit).
narrative_ontology:cs_authority_grounding('8f761fb2-7ee0-4f1d-9e9b-c6b391fd1033', self_enforcing).
narrative_ontology:cs_reading_relation('8f761fb2-7ee0-4f1d-9e9b-c6b391fd1033', nuclear_impossibility_kernel__rational_dropout_reading, influences).
narrative_ontology:cs_reading_relation('8f761fb2-7ee0-4f1d-9e9b-c6b391fd1033', nuclear_impossibility_kernel__credibility_paradox_reading, influences).
narrative_ontology:cs_axiom('8f761fb2-7ee0-4f1d-9e9b-c6b391fd1033', foundational, mutual_annihilation_is_guaranteed).
narrative_ontology:cs_axiom_status(mutual_annihilation_is_guaranteed, holdable).
narrative_ontology:cs_axiom_grounding('8f761fb2-7ee0-4f1d-9e9b-c6b391fd1033', mutual_annihilation_is_guaranteed, empirically_contingent).
narrative_ontology:cs_axiom('8f761fb2-7ee0-4f1d-9e9b-c6b391fd1033', foundational, victory_requires_net_gain).
narrative_ontology:cs_axiom_status(victory_requires_net_gain, holdable).
narrative_ontology:cs_axiom_grounding('8f761fb2-7ee0-4f1d-9e9b-c6b391fd1033', victory_requires_net_gain, deontological).
narrative_ontology:cs_reference_frame('8f761fb2-7ee0-4f1d-9e9b-c6b391fd1033', pre_nuclear_strategic_paradigm).
narrative_ontology:cs_drift_state('8f761fb2-7ee0-4f1d-9e9b-c6b391fd1033', contemporary, gap(practice_drift, minor, true)).
narrative_ontology:cs_created_at('8f761fb2-7ee0-4f1d-9e9b-c6b391fd1033', '').
narrative_ontology:cs_kernel_id(nuclear_impossibility_kernel__structural_contraction_reading, nuclear_impossibility_kernel).

% --- Structural relationships ---
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(nuclear_impossibility_kernel__structural_contraction_reading, non_nuclear_states).
narrative_ontology:constraint_victim(nuclear_impossibility_kernel__structural_contraction_reading, nuclear_armed_states).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These states possess nuclear arsenals, which, by their very existence, impose the impossibility of rational victory in direct conflict. They are 'payers' in the sense that their strategic options are severely constrained, and the cost of violating the constraint is existential. They cannot 'exit' the nuclear reality.
narrative_ontology:constraint_stakeholder(nuclear_impossibility_kernel__structural_contraction_reading, nuclear_armed_states, payer,
    institutional, generational, trapped, global).

% These states benefit from the absence of direct, large-scale conventional war between nuclear powers, as the structural impossibility of victory for nuclear states reduces the likelihood of such conflicts escalating. They are not directly subject to the 'mutual annihilation' aspect but live under its shadow.
narrative_ontology:constraint_stakeholder(nuclear_impossibility_kernel__structural_contraction_reading, non_nuclear_states, beneficiary,
    moderate, generational, mobile, global).

% These agents analyze and articulate the implications of nuclear weapons for international relations and conflict theory. They observe the structural contraction of war and its consequences for state behavior, but do not directly experience the constraint's coercive force.
narrative_ontology:constraint_stakeholder(nuclear_impossibility_kernel__structural_contraction_reading, strategic_theorists, observer,
    analytical, generational, analytical, universal).

% Tasked with developing defense strategies, these planners must operate within the reality of nuclear impossibility. Their 'agenda-setting' is constrained by the fundamental unviability of direct war, forcing them to focus on deterrence, limited conflict, or proxy engagements. They are forced to adapt to the new strategic landscape.
narrative_ontology:constraint_stakeholder(nuclear_impossibility_kernel__structural_contraction_reading, military_planners, agenda_setter,
    institutional, immediate, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: It implicitly coordinates nuclear-armed states to avoid direct, large-scale conflict by making such conflict irrational and self-defeating, thereby preventing mutual annihilation.
% TRANSFER_FUNCTION: It transfers the possibility of 'victory' in direct great-power war from all nuclear-armed states to the realm of impossibility, effectively 'costing' them the option of traditional military triumph.
% ABSENT_VOICES: Historical military strategists who believed in the ultimate utility of total war would object, as their entire framework for conflict resolution is rendered obsolete. They are absent because the physical reality of nuclear weapons has fundamentally altered the strategic landscape.
% DISAPPEARANCE_RATIONALE: If the nuclear impossibility vanished (e.g., through a technological breakthrough rendering nuclear weapons obsolete and harmless), the strategic calculus of great powers would fundamentally shift. The 'shadow of the bomb' would lift, potentially re-legitimizing direct, large-scale conventional conflict as a rational policy option, leading to a complete rearrangement of global security architectures.
% FOUNDING_PROBLEM: The problem of preventing existential, civilization-ending war between great powers.
% FOUNDING_PROBLEM_CORROBORATION: The continued existence of nuclear arsenals and the ongoing efforts in arms control and non-proliferation, attested by international organizations (e.g., UN, IAEA) and non-proliferation experts, corroborate that the problem of preventing nuclear war remains live. No major power or independent analyst disputes the catastrophic consequences of nuclear exchange.
narrative_ontology:disappearance_verdict(nuclear_impossibility_kernel__structural_contraction_reading, world_rearranges).
narrative_ontology:founding_problem_status(nuclear_impossibility_kernel__structural_contraction_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(nuclear_impossibility_kernel__structural_contraction_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(nuclear_impossibility_kernel__structural_contraction_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(nuclear_impossibility_kernel__structural_contraction_reading_tests).

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(nuclear_impossibility_kernel__structural_contraction_reading, ExtMetricName, E),
    domain_priors:suppression_score(nuclear_impossibility_kernel__structural_contraction_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(nuclear_impossibility_kernel__structural_contraction_reading),
    narrative_ontology:constraint_metric(nuclear_impossibility_kernel__structural_contraction_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(nuclear_impossibility_kernel__structural_contraction_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(nuclear_impossibility_kernel__structural_contraction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint is classified as a Mountain because it represents an unchangeable physical reality (the destructive power of nuclear weapons) that fundamentally alters the strategic landscape. Its extractiveness is 0.0 because it doesn't extract rents; rather, it imposes a universal, unavoidable cost (the loss of 'winnable' war). Suppression is 1.0 because the physical reality of mutual annihilation is absolute and cannot be resisted. Accessibility collapse is 1.0 as the option of rational victory is completely foreclosed. Resistance is 0.0 because there is no 'resisting' the physical facts of nuclear physics. Theater ratio is 0.0 as there is no performative aspect to this physical impossibility.
 *
 * PERSPECTIVAL GAP:
 *   There is no significant perspectival gap on the core structural impossibility. All rational actors, regardless of their position, must acknowledge the physical reality of mutual annihilation. Divergence arises in how actors *respond* to this impossibility (e.g., pursuing proxy wars, developing limited strike capabilities), but not on the impossibility itself.
 *
 * DIRECTIONALITY LOGIC:
 *   Nuclear-armed states are 'payers' because they bear the ultimate cost of this impossibility (loss of traditional strategic options, existential risk). Non-nuclear states are 'beneficiaries' as they indirectly benefit from the reduced likelihood of direct great-power conflict. Strategic theorists are 'observers' analyzing this reality. Military planners are 'agenda-setters' who must adapt their strategies to this new, constrained reality.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    physical_vs_rational_impossibility,
    'Is the impossibility of victory a purely physical consequence of nuclear destructive power, or does it depend on the rationality of state actors?',
    'Analysis of historical ''near-miss'' events and decision-making under extreme pressure: if actors consistently chose de-escalation despite perceived advantages, it suggests a rational component. If the physical consequences alone are sufficient to deter, it''s purely physical.',
    'If purely physical, the constraint is a robust Mountain. If dependent on rationality, it might shift towards a more contingent type (e.g., Rope or Tangled Rope) for actors who might deviate from ''rational'' behavior.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(physical_vs_rational_impossibility, conceptual, 'Distinguishing between physical and rational components of nuclear impossibility.').

omega_variable(
    proxy_war_as_substitution_or_continuation,
    'Are proxy wars a structural ''substitution'' for direct conflict (a new form of competition under nuclear impossibility) or a ''continuation'' of traditional great-power rivalry by other means?',
    'Comparative historical analysis of pre- and post-nuclear great power conflicts: if the nature, scale, and objectives of proxy wars fundamentally differ from pre-nuclear conflicts, it suggests substitution. If they largely mirror prior patterns, it suggests continuation.',
    'If substitution, it reinforces the ''structural contraction'' reading. If continuation, it suggests the impossibility is less absolute, and traditional conflict dynamics persist in a modified form, potentially weakening the Mountain classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(proxy_war_as_substitution_or_continuation, empirical, 'Understanding the nature of proxy wars in the nuclear age.').

omega_variable(
    kernel_reading_distinction,
    'Is this constraint truly distinct from the ''rational dropout'' and ''credibility paradox'' readings, or are the distinctions merely semantic?',
    'Formal logical analysis of the core premises of each reading: if the premises are mutually exclusive or lead to fundamentally different strategic implications, the distinction is structural. If they are merely different emphases of the same underlying phenomenon, they are not distinct constraints.',
    'If the distinction is structural, this Mountain classification holds. If not, this constraint might be subsumed under a broader ''nuclear deterrence'' constraint, potentially altering its classification to reflect the more contingent aspects of rationality and credibility.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_distinction, conceptual, 'Clarifying the boundaries between different readings of the nuclear impossibility kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(nuclear_impossibility_kernel__structural_contraction_reading, 1945, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nucl_tr_t1945, nuclear_impossibility_kernel__structural_contraction_reading, theater_ratio, 1945, 0.0).
narrative_ontology:measurement(nucl_tr_t1960, nuclear_impossibility_kernel__structural_contraction_reading, theater_ratio, 1960, 0.0).
narrative_ontology:measurement(nucl_tr_t1980, nuclear_impossibility_kernel__structural_contraction_reading, theater_ratio, 1980, 0.0).
narrative_ontology:measurement(nucl_tr_t2000, nuclear_impossibility_kernel__structural_contraction_reading, theater_ratio, 2000, 0.0).
narrative_ontology:measurement(nucl_tr_t2024, nuclear_impossibility_kernel__structural_contraction_reading, theater_ratio, 2024, 0.0).

% Extraction over time
narrative_ontology:measurement(nucl_be_t1945, nuclear_impossibility_kernel__structural_contraction_reading, base_extractiveness, 1945, 0.0).
narrative_ontology:measurement(nucl_be_t1960, nuclear_impossibility_kernel__structural_contraction_reading, base_extractiveness, 1960, 0.0).
narrative_ontology:measurement(nucl_be_t1980, nuclear_impossibility_kernel__structural_contraction_reading, base_extractiveness, 1980, 0.0).
narrative_ontology:measurement(nucl_be_t2000, nuclear_impossibility_kernel__structural_contraction_reading, base_extractiveness, 2000, 0.0).
narrative_ontology:measurement(nucl_be_t2024, nuclear_impossibility_kernel__structural_contraction_reading, base_extractiveness, 2024, 0.0).

% Suppression requirement over time
narrative_ontology:measurement(nucl_su_t1945, nuclear_impossibility_kernel__structural_contraction_reading, suppression_requirement, 1945, 1.0).
narrative_ontology:measurement(nucl_su_t1960, nuclear_impossibility_kernel__structural_contraction_reading, suppression_requirement, 1960, 1.0).
narrative_ontology:measurement(nucl_su_t1980, nuclear_impossibility_kernel__structural_contraction_reading, suppression_requirement, 1980, 1.0).
narrative_ontology:measurement(nucl_su_t2000, nuclear_impossibility_kernel__structural_contraction_reading, suppression_requirement, 2000, 1.0).
narrative_ontology:measurement(nucl_su_t2024, nuclear_impossibility_kernel__structural_contraction_reading, suppression_requirement, 2024, 1.0).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(nuclear_impossibility_kernel__structural_contraction_reading, global_infrastructure).
narrative_ontology:affects_constraint(nuclear_impossibility_kernel__structural_contraction_reading, nuclear_impossibility_kernel__rational_dropout_reading).
narrative_ontology:affects_constraint(nuclear_impossibility_kernel__structural_contraction_reading, nuclear_impossibility_kernel__credibility_paradox_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'nuclear impossibility kernel,' focusing on the physical contraction of war. It is linked to other readings that emphasize rational choice and threat credibility.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
