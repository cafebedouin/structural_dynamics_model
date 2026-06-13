% ============================================================================
% CONSTRAINT STORY: honor_satisfaction_mechanism__contraction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_honor_satisfaction_mechanism__contraction_reading, []).

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
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: honor_satisfaction_mechanism__contraction_reading
 *   human_readable: Honor Satisfaction via Dueling (Contraction Reading)
 *   domain: historical/normative/cognitive
 *
 * SUMMARY:
 *   The contraction reading claims that dueling became cognitively
 *   unthinkable—a category-level impossibility—rather than merely suppressed
 *   or declined. Under this reading, the constraint was a natural law within
 *   a bounded epistemic framework: given the premises that (1) honor is a
 *   supreme value, (2) honor requires satisfaction when impugned, and (3)
 *   ritual combat is the only intelligible form of satisfaction, dueling
 *   followed with logical necessity. As the framework itself collapsed—as
 *   alternative forms of honor-satisfaction became cognitively available and
 *   legitimate—the constraint evacuated from possibility space. It did not
 *   disappear because enforcement suppressed it or because people chose
 *   alternatives; it disappeared because the cognitive architecture that made
 *   it mandatory dissolved. The founding problem (how to satisfy impugned
 *   honor) remained live, but the answer (dueling) became unthinkable.
 *
 * KEY AGENTS:
 *   - Feudal aristocratic elite: bound by identity-lock to the dueling constraint; their honor could only be satisfied through combat.
 *   - Bourgeois commercial classes: never fully bound; demonstrated through practice that honor could be satisfied non-violently.
 *   - State monopoly authority: emerged as the formal suppressor of dueling, but the contraction reading claims state prohibition was a symptom, not a cause.
 *   - Analytical observer: sees the historical record as showing cognitive evacuation, not decline or suppression.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(honor_satisfaction_mechanism__contraction_reading, 0.0).
domain_priors:suppression_score(honor_satisfaction_mechanism__contraction_reading, 0.0).
domain_priors:theater_ratio(honor_satisfaction_mechanism__contraction_reading, 0.0).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(honor_satisfaction_mechanism__contraction_reading, extractiveness, 0.0).
narrative_ontology:constraint_metric(honor_satisfaction_mechanism__contraction_reading, suppression_requirement, 0.0).
narrative_ontology:constraint_metric(honor_satisfaction_mechanism__contraction_reading, theater_ratio, 0.0).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(honor_satisfaction_mechanism__contraction_reading, accessibility_collapse, 0.95).
narrative_ontology:constraint_metric(honor_satisfaction_mechanism__contraction_reading, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(honor_satisfaction_mechanism__contraction_reading, mountain).
narrative_ontology:human_readable(honor_satisfaction_mechanism__contraction_reading, "Honor Satisfaction via Dueling (Contraction Reading)").
narrative_ontology:topic_domain(honor_satisfaction_mechanism__contraction_reading, "historical/normative/cognitive").

domain_priors:emerges_naturally(honor_satisfaction_mechanism__contraction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(honor_satisfaction_mechanism__contraction_reading, 'f8739850-01c9-4230-997f-50595ffa0551').
narrative_ontology:cs_kernel_codification('f8739850-01c9-4230-997f-50595ffa0551', distributed).
narrative_ontology:cs_authority_grounding('f8739850-01c9-4230-997f-50595ffa0551', extraction).
narrative_ontology:cs_interpretation_layer_present('f8739850-01c9-4230-997f-50595ffa0551').
narrative_ontology:cs_reading_relation('f8739850-01c9-4230-997f-50595ffa0551', honor_satisfaction_mechanism__decline_reading, coexists_with).
narrative_ontology:cs_reading_relation('f8739850-01c9-4230-997f-50595ffa0551', honor_satisfaction_mechanism__composite_reading, influences).
narrative_ontology:cs_axiom('f8739850-01c9-4230-997f-50595ffa0551', foundational, honor_requires_ritual_combat_necessity).
narrative_ontology:cs_axiom_status(honor_requires_ritual_combat_necessity, holdable).
narrative_ontology:cs_axiom_grounding('f8739850-01c9-4230-997f-50595ffa0551', honor_requires_ritual_combat_necessity, deontological).
narrative_ontology:cs_axiom('f8739850-01c9-4230-997f-50595ffa0551', foundational, combat_satisfiability_exhaustiveness).
narrative_ontology:cs_axiom_status(combat_satisfiability_exhaustiveness, overridden).
narrative_ontology:cs_axiom_grounding('f8739850-01c9-4230-997f-50595ffa0551', combat_satisfiability_exhaustiveness, empirically_contingent).
narrative_ontology:cs_reference_frame('f8739850-01c9-4230-997f-50595ffa0551', honor_satisfaction_via_ritual_combat).
narrative_ontology:cs_drift_state('f8739850-01c9-4230-997f-50595ffa0551', enlightenment_bourgeois_challenge, gap(axiom_overriding, severe, false)).
narrative_ontology:cs_created_at('f8739850-01c9-4230-997f-50595ffa0551', '').
narrative_ontology:cs_kernel_id(honor_satisfaction_mechanism__contraction_reading, honor_satisfaction_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(honor_satisfaction_mechanism__contraction_reading, honor_satisfaction_conceptual_framework).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(honor_satisfaction_mechanism__contraction_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(honor_satisfaction_mechanism__contraction_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(honor_satisfaction_mechanism__contraction_reading_tests).

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(honor_satisfaction_mechanism__contraction_reading, ExtMetricName, E),
    domain_priors:suppression_score(honor_satisfaction_mechanism__contraction_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(honor_satisfaction_mechanism__contraction_reading),
    narrative_ontology:constraint_metric(honor_satisfaction_mechanism__contraction_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(honor_satisfaction_mechanism__contraction_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(honor_satisfaction_mechanism__contraction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is zero (0.0) throughout because this reading denies any extractive mechanism. The constraint operated as a natural law within its epistemic frame—inevitable, not extractive. Suppression is zero (0.0) because suppression is coercion imposed against preference; the constraint was constitutive of identity, not externally imposed. Theater ratio is zero (0.0) because there is no performative component—dueling was deadly serious. Accessibility collapse is very high (0.95) because once the premises were accepted, dueling followed with logical necessity; alternatives were literally unthinkable within the framework. Resistance is very low (0.05) because resistance implies agents wanting something else; the bound agents were not resisting—they were being who they were required to be. The measurement series are flat because the contraction reading denies any drift in extractiveness or enforcement: the constraint remained structurally identical until it became categorically impossible. At that point, it did not decline gradually (the decline_reading) or get suppressed (the composite_reading)—it simply ceased to exist as a possible thought.
 *
 * PERSPECTIVAL GAP:
 *   The contraction reading predicts a single, coherent perspectival position: that of agents within the epistemic frame for whom dueling is not experienced as constraint but as self-evident necessity. By definition, observers outside the frame (bourgeoisie, state authorities, modern analysts) would not experience the constraint as mandatory—they would see it as a choice or a compulsion imposed by others. The gap is not between agent seats (as in most constraints) but between frames: inside the dueling-frame, dueling is the only thinkable solution; outside the frame, it is merely one possible practice among others, and a brutal one. The contraction reading implies that once the frame dissolved, even the aristocratic elite would shift seats—they would move from experiencing dueling as inevitable to experiencing it as unthinkable. This is not a perspectival disagreement within one frame but a categorical shift from one frame to another.
 *
 * DIRECTIONALITY LOGIC:
 *   This reading denies directionality in the usual sense. There is no beneficiary in the sense of an agent collecting gains; the 'beneficiary' is the conceptual framework itself (listed as agent=false, a non-agent entity). There are no payers in the sense of agents being extracted from. The aristocratic elite were bound, not paying—their identity was constituted by the constraint, not damaged by external extraction. Directionality would imply an external power imposing costs; the contraction reading locates the constraint entirely within the epistemic frame of the bound agents. This is why the constraint measures as a mountain: it operated as natural law within its bounded frame. The dissolution came not from external suppression (which would imply directionality) but from the frame itself becoming incoherent.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (honor satisfaction) never became mandatrophy in the classical sense—honor remained a live, valued outcome. However, the contraction reading describes a form of functional death that does not fit the mandatrophy category: the problem did not persist while the solution atrophied (that would be the piton/decline pattern). Rather, the problem persisted but the solution became categorically impossible. This is better described as 'solution-space evacuation'—the founding problem remains, but one particular solution-path closes not because alternatives were discovered and chosen, but because the conceptual scaffolding that made that path intelligible collapsed. The constraint disappears not through mandatrophy (maintenance via theater/inertia) but through cognitive discontinuity.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    cognitive_evacuation_vs_enforcement,
    'Did dueling become categorically unthinkable (evacuated from possibility space) before or after state enforcement made it illegal?',
    'Historical analysis of literary production, legal arguments, philosophical discourse, and personal testimony: when did intellectuals first articulate that honor and violence could be decoupled? When did enforcement begin? Which came first?',
    'If cognitive evacuation preceded enforcement by decades, the contraction reading is supported—the constraint died conceptually before state suppression took effect. If enforcement preceded the cognitive shift, the composite_reading (multiple mechanisms) is more plausible. The temporal ordering determines whether the constraint was a mountain-like natural consequence of a frame that was then abandoned, or whether it was actively suppressed and cognitive acceptance followed suppression.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cognitive_evacuation_vs_enforcement, empirical, 'Temporal precedence of cognitive shift vs. legal enforcement in the disappearance of dueling.').

omega_variable(
    frame_collapse_vs_alternative_availability,
    'Did the dueling-frame become unthinkable because internal contradictions made it incoherent, or did it become unthinkable because external alternatives (bourgeois honor, legal remedies, insurance against insult) became visible and attractive?',
    'Analysis of how non-dueling honor systems were perceived by dueling-frame advocates: were they seen as inferior-but-viable alternatives, or as incoherent/non-honorable by definition? At what point did this perception shift? What catalyzed the shift?',
    'If the frame collapsed from internal incoherence, the contraction reading is pure—a natural-law frame that became logically impossible. If the frame was displaced by external alternatives, the dynamic is more synthetic (alternatives-driven shift), closer to the composite_reading. The mechanism of frame-collapse determines whether the constraint''s disappearance is a category-level evacuation or a preference-shift driven by new possibilities.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(frame_collapse_vs_alternative_availability, conceptual, 'Whether frame-collapse was internally-driven (incoherence) or externally-driven (alternative availability).').

omega_variable(
    false_summit_natural_law,
    'Is the dueling constraint genuinely a natural law (emerges_naturally=true) within its epistemic frame, or is the ''naturalness'' itself a constructed ideology that benefited the aristocratic elite by making dueling seem inevitable rather than chosen?',
    'Historical and philosophical analysis: did dueling advocates describe dueling as mandatory by nature/logic, or as a chosen tradition? Did they present the honor-framework as discovered or constructed? What did critics (especially non-duelers) say about the ''naturalness'' claim?',
    'If dueling was genuinely experienced as natural/inevitable by bound agents and only became unthinkable when the frame collapsed, the contraction reading and the false_summit gate align—a mountain within a frame that was then abandoned. If the ''naturalness'' was always an ideology, the constraint is a snare dressed as a mountain (false summit). The distinction determines whether the constraint disappears as a genuine category-level impossibility (mountain logic) or as an exposed ideology whose cover is blown (snare-unmasked logic).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(false_summit_natural_law, conceptual, 'Whether the dueling-frame''s ''naturalness'' was experienced necessity or ideological cover.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(honor_satisfaction_mechanism__contraction_reading, 0, 200).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hono_tr_t0, honor_satisfaction_mechanism__contraction_reading, theater_ratio, 0, 0.0).
narrative_ontology:measurement(hono_tr_t40, honor_satisfaction_mechanism__contraction_reading, theater_ratio, 40, 0.0).
narrative_ontology:measurement(hono_tr_t80, honor_satisfaction_mechanism__contraction_reading, theater_ratio, 80, 0.0).
narrative_ontology:measurement(hono_tr_t120, honor_satisfaction_mechanism__contraction_reading, theater_ratio, 120, 0.0).
narrative_ontology:measurement(hono_tr_t160, honor_satisfaction_mechanism__contraction_reading, theater_ratio, 160, 0.0).
narrative_ontology:measurement(hono_tr_t200, honor_satisfaction_mechanism__contraction_reading, theater_ratio, 200, 0.0).

% Extraction over time
narrative_ontology:measurement(hono_be_t0, honor_satisfaction_mechanism__contraction_reading, base_extractiveness, 0, 0.0).
narrative_ontology:measurement(hono_be_t40, honor_satisfaction_mechanism__contraction_reading, base_extractiveness, 40, 0.0).
narrative_ontology:measurement(hono_be_t80, honor_satisfaction_mechanism__contraction_reading, base_extractiveness, 80, 0.0).
narrative_ontology:measurement(hono_be_t120, honor_satisfaction_mechanism__contraction_reading, base_extractiveness, 120, 0.0).
narrative_ontology:measurement(hono_be_t160, honor_satisfaction_mechanism__contraction_reading, base_extractiveness, 160, 0.0).
narrative_ontology:measurement(hono_be_t200, honor_satisfaction_mechanism__contraction_reading, base_extractiveness, 200, 0.0).

% Suppression requirement over time
narrative_ontology:measurement(hono_su_t0, honor_satisfaction_mechanism__contraction_reading, suppression_requirement, 0, 0.0).
narrative_ontology:measurement(hono_su_t40, honor_satisfaction_mechanism__contraction_reading, suppression_requirement, 40, 0.0).
narrative_ontology:measurement(hono_su_t80, honor_satisfaction_mechanism__contraction_reading, suppression_requirement, 80, 0.0).
narrative_ontology:measurement(hono_su_t120, honor_satisfaction_mechanism__contraction_reading, suppression_requirement, 120, 0.0).
narrative_ontology:measurement(hono_su_t160, honor_satisfaction_mechanism__contraction_reading, suppression_requirement, 160, 0.0).
narrative_ontology:measurement(hono_su_t200, honor_satisfaction_mechanism__contraction_reading, suppression_requirement, 200, 0.0).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(honor_satisfaction_mechanism__contraction_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(honor_satisfaction_mechanism__contraction_reading, 0.0).
narrative_ontology:affects_constraint(honor_satisfaction_mechanism__contraction_reading, honor_satisfaction_mechanism__decline_reading).
narrative_ontology:affects_constraint(honor_satisfaction_mechanism__contraction_reading, honor_satisfaction_mechanism__composite_reading).

% DUAL FORMULATION NOTE:
% The honor_satisfaction_mechanism kernel decomposes into three readings with different causal structures: contraction_reading (cognitive evacuation), decline_reading (gradual persistence and fringe status), and composite_reading (multiple mechanisms: state monopoly, bourgeois norm-shift, insurance, category-shift). Each reading instantiates a different ε and a different mechanism of disappearance. They are linked as sibling interpretations of the same historical fact (dueling ended) under different frames of what ended it and how.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
