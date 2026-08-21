% ============================================================================
% CONSTRAINT STORY: qwerty_persistence_inevitability__path_dependency_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_qwerty_persistence_inevitability__path_dependency_reading, []).

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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: qwerty_persistence_inevitability__path_dependency_reading
 *   human_readable: QWERTY Layout Persistence (Path Dependency Reading)
 *   domain: technology_history/political_economy/institutional_analysis
 *
 * SUMMARY:
 *   This constraint describes the persistence of the QWERTY keyboard layout
 *   as a consequence of accident-driven path dependency, where initial design
 *   choices (made for mechanical reasons) created an installed base and
 *   training infrastructure that became self-reinforcing. This reading
 *   asserts that no specific agent strategically benefits from its
 *   persistence; rather, the efficiency losses are a diffuse externality
 *   borne by users, and manufacturers merely respond to an entrenched market
 *   standard. The constraint is claimed as a Mountain due to its perceived
 *   technological inevitability given initial conditions, with very low
 *   extraction and suppression, and high accessibility collapse for
 *   alternatives.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(qwerty_persistence_inevitability__path_dependency_reading, 0.15).
domain_priors:suppression_score(qwerty_persistence_inevitability__path_dependency_reading, 0.1).
domain_priors:theater_ratio(qwerty_persistence_inevitability__path_dependency_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(qwerty_persistence_inevitability__path_dependency_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(qwerty_persistence_inevitability__path_dependency_reading, suppression_requirement, 0.1).
narrative_ontology:constraint_metric(qwerty_persistence_inevitability__path_dependency_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(qwerty_persistence_inevitability__path_dependency_reading, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(qwerty_persistence_inevitability__path_dependency_reading, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(qwerty_persistence_inevitability__path_dependency_reading, mountain).
narrative_ontology:human_readable(qwerty_persistence_inevitability__path_dependency_reading, "QWERTY Layout Persistence (Path Dependency Reading)").
narrative_ontology:topic_domain(qwerty_persistence_inevitability__path_dependency_reading, "technology_history/political_economy/institutional_analysis").

domain_priors:emerges_naturally(qwerty_persistence_inevitability__path_dependency_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(qwerty_persistence_inevitability__path_dependency_reading, '112a0308-e622-4f03-8af1-12c26d1f04a9').
narrative_ontology:cs_kernel_codification('112a0308-e622-4f03-8af1-12c26d1f04a9', implicit).
narrative_ontology:cs_authority_grounding('112a0308-e622-4f03-8af1-12c26d1f04a9', practice).
narrative_ontology:cs_reading_relation('112a0308-e622-4f03-8af1-12c26d1f04a9', qwerty_persistence_inevitability__strategic_lock_in_reading, coexists_with).
narrative_ontology:cs_axiom('112a0308-e622-4f03-8af1-12c26d1f04a9', foundational, technological_adoption_is_path_dependent).
narrative_ontology:cs_axiom_status(technological_adoption_is_path_dependent, holdable).
narrative_ontology:cs_axiom_grounding('112a0308-e622-4f03-8af1-12c26d1f04a9', technological_adoption_is_path_dependent, empirically_contingent).
narrative_ontology:cs_reference_frame('112a0308-e622-4f03-8af1-12c26d1f04a9', initial_technological_adoption_conditions).
narrative_ontology:cs_drift_state('112a0308-e622-4f03-8af1-12c26d1f04a9', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('112a0308-e622-4f03-8af1-12c26d1f04a9', '').
narrative_ontology:cs_kernel_id(qwerty_persistence_inevitability__path_dependency_reading, qwerty_persistence_inevitability).

% --- Structural relationships ---
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(qwerty_persistence_inevitability__path_dependency_reading, typists).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Produce QWERTY keyboards because it is the established market standard. They respond to demand and existing infrastructure, rather than actively enforcing the standard or extracting rents from its persistence. Shifting to an alternative layout would incur massive retooling and market education costs.
narrative_ontology:constraint_stakeholder(qwerty_persistence_inevitability__path_dependency_reading, keyboard_manufacturers, agenda_setter,
    institutional, generational, constrained, global).

% Are trained on QWERTY and develop muscle memory, making it difficult and costly to switch to alternative layouts. They bear the diffuse efficiency loss from the suboptimal layout, but this is an externality, not a direct extraction by a specific party. Their identity as 'typists' is tied to the QWERTY skill.
narrative_ontology:constraint_stakeholder(qwerty_persistence_inevitability__path_dependency_reading, typists, payer,
    powerless, biographical, identity_locked, global).

% Advocate for more efficient or ergonomic keyboard layouts (e.g., Dvorak, Colemak) but face immense inertia from the installed base and training infrastructure. Their proposals are technically viable but socially and economically suppressed by the QWERTY standard's dominance.
narrative_ontology:constraint_stakeholder(qwerty_persistence_inevitability__path_dependency_reading, ergonomic_designers, excluded,
    moderate, generational, constrained, global).

% Analyze the historical development and persistence of the QWERTY layout, studying the mechanisms of path dependency and the economic implications of technological lock-in. They provide an analytical perspective on the constraint's nature.
narrative_ontology:constraint_stakeholder(qwerty_persistence_inevitability__path_dependency_reading, economic_historians, observer,
    analytical, generational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a universal standard for keyboard layout, enabling mass production, widespread training, and interoperability across devices and users.
% TRANSFER_FUNCTION: The constraint transfers diffuse efficiency losses (slower typing speeds, higher training costs for optimal performance) to typists, but without a direct, concentrated beneficiary capturing these losses. It also transfers the cost of market entry for alternative layouts to their proponents.
% ABSENT_VOICES: Ergonomic designers and proponents of alternative keyboard layouts are largely excluded from the mainstream conversation about keyboard standards, their arguments for efficiency improvements overridden by the inertia of the installed base and training infrastructure.
% DISAPPEARANCE_RATIONALE: If QWERTY vanished overnight, the global typing ecosystem would face immense disruption. Mass retraining would be required, keyboard manufacturing would need complete retooling, and digital interfaces would need to adapt. The world would rearrange around a new, likely more efficient, standard after a period of chaos.
% FOUNDING_PROBLEM: The original problem was to design a robust mechanical typewriter layout that prevented typebars from jamming, given the technological limitations of the late 19th century.
% FOUNDING_PROBLEM_CORROBORATION: Historical records of typewriter patents and early manufacturing, engineering analyses of mechanical typewriter mechanisms, and independent ergonomic studies all corroborate that the original mechanical problem is long solved and no longer justifies the layout's persistence. The persistence is now driven by social and economic inertia.
narrative_ontology:disappearance_verdict(qwerty_persistence_inevitability__path_dependency_reading, world_rearranges).
narrative_ontology:founding_problem_status(qwerty_persistence_inevitability__path_dependency_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(qwerty_persistence_inevitability__path_dependency_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(qwerty_persistence_inevitability__path_dependency_reading, 'none', 1).
narrative_ontology:epsilon_provenance(qwerty_persistence_inevitability__path_dependency_reading, 0.15, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(qwerty_persistence_inevitability__path_dependency_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(qwerty_persistence_inevitability__path_dependency_reading, ExtMetricName, E),
    domain_priors:suppression_score(qwerty_persistence_inevitability__path_dependency_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(qwerty_persistence_inevitability__path_dependency_reading),
    narrative_ontology:constraint_metric(qwerty_persistence_inevitability__path_dependency_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(qwerty_persistence_inevitability__path_dependency_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(qwerty_persistence_inevitability__path_dependency_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness is low because no single party actively extracts rents from QWERTY's persistence; the 'cost' is a diffuse efficiency loss. Suppression is low because it's not actively enforced by a coercive agent, but rather by the inertia of training and manufacturing. Theater ratio is minimal, as its persistence is a functional outcome of path dependency, not a performance. Accessibility collapse is high because the sheer scale of the installed base and training infrastructure makes alternatives practically inaccessible for most users. Resistance is low because most users perceive QWERTY as an unchangeable default.
 *
 * PERSPECTIVAL GAP:
 *   This 'path_dependency_reading' contrasts sharply with a 'strategic_lock_in_reading' which would posit active beneficiaries (e.g., manufacturers colluding to maintain the standard) and higher extraction. This story's metrics reflect the path dependency view, where the constraint is a 'natural' outcome of historical accidents and market dynamics, rather than a deliberately engineered extractive mechanism.
 *
 * DIRECTIONALITY LOGIC:
 *   Keyboard manufacturers act as agenda-setters by producing the standard, but their role is reactive to market demand, not extractive. Typists are payers of the diffuse efficiency cost, locked in by training. Ergonomic designers are excluded voices, their alternatives unable to gain traction against the entrenched standard. Economic historians observe the phenomenon analytically.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    path_dependency_vs_strategic_lock_in,
    'Is QWERTY''s persistence primarily due to accident-driven path dependency (this reading) or strategic lock-in engineered by manufacturers (sibling reading)?',
    'Detailed historical analysis of manufacturer decisions, patent strategies, and marketing efforts, particularly during periods when alternative layouts gained minor traction. Evidence of active suppression of alternatives or coordinated industry-wide resistance to change would support the strategic lock-in reading.',
    'If strategic lock-in is confirmed, the constraint would reclassify from Mountain to Tangled Rope or Snare, with higher extractiveness and identifiable beneficiaries (manufacturers) actively maintaining the standard. If path dependency is confirmed, the Mountain classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(path_dependency_vs_strategic_lock_in, empirical, 'Distinguishing between historical accident and deliberate market manipulation in QWERTY''s persistence.').

omega_variable(
    diffuse_cost_measurement,
    'How accurately can the diffuse efficiency losses borne by typists be quantified and attributed as ''extraction'' in the absence of a direct beneficiary?',
    'Comparative ergonomic studies of typing speed and error rates across QWERTY and optimal layouts, combined with economic modeling of aggregate productivity loss. The challenge is in assigning a ''recipient'' to this ''loss''.',
    'If the diffuse costs are deemed unquantifiable or unassignable to a beneficiary, the extractiveness might be considered even lower, reinforcing the Mountain classification. If a mechanism for ''diffuse extraction'' is identified, it could shift towards a Piton or even a Snare if a ''system'' is identified as the beneficiary.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(diffuse_cost_measurement, conceptual, 'The conceptual challenge of measuring and attributing diffuse efficiency losses as extraction.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(qwerty_persistence_inevitability__path_dependency_reading, 1874, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qwer_tr_t1874, qwerty_persistence_inevitability__path_dependency_reading, theater_ratio, 1874, 0.01).
narrative_ontology:measurement(qwer_tr_t1924, qwerty_persistence_inevitability__path_dependency_reading, theater_ratio, 1924, 0.02).
narrative_ontology:measurement(qwer_tr_t1974, qwerty_persistence_inevitability__path_dependency_reading, theater_ratio, 1974, 0.04).
narrative_ontology:measurement(qwer_tr_t2024, qwerty_persistence_inevitability__path_dependency_reading, theater_ratio, 2024, 0.05).

% Extraction over time
narrative_ontology:measurement(qwer_be_t1874, qwerty_persistence_inevitability__path_dependency_reading, base_extractiveness, 1874, 0.05).
narrative_ontology:measurement(qwer_be_t1924, qwerty_persistence_inevitability__path_dependency_reading, base_extractiveness, 1924, 0.08).
narrative_ontology:measurement(qwer_be_t1974, qwerty_persistence_inevitability__path_dependency_reading, base_extractiveness, 1974, 0.12).
narrative_ontology:measurement(qwer_be_t2024, qwerty_persistence_inevitability__path_dependency_reading, base_extractiveness, 2024, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(qwer_su_t1874, qwerty_persistence_inevitability__path_dependency_reading, suppression_requirement, 1874, 0.05).
narrative_ontology:measurement(qwer_su_t1924, qwerty_persistence_inevitability__path_dependency_reading, suppression_requirement, 1924, 0.08).
narrative_ontology:measurement(qwer_su_t1974, qwerty_persistence_inevitability__path_dependency_reading, suppression_requirement, 1974, 0.09).
narrative_ontology:measurement(qwer_su_t2024, qwerty_persistence_inevitability__path_dependency_reading, suppression_requirement, 2024, 0.1).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(qwerty_persistence_inevitability__path_dependency_reading, information_standard).

% DUAL FORMULATION NOTE:
% This constraint is one of two readings of the 'qwerty_persistence_inevitability' kernel. The sibling reading, 'strategic_lock_in_reading', posits active beneficiaries and higher extraction.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
