% ============================================================================
% CONSTRAINT STORY: electronic_money_emergence__became_thinkable_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_electronic_money_emergence__became_thinkable_reading, []).

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
 *   constraint_id: electronic_money_emergence__became_thinkable_reading
 *   human_readable: Conceptual and Technical Barrier to Digital Money Thinkability
 *   domain: economic_history/monetary_theory/technology_studies
 *
 * SUMMARY:
 *   This constraint describes the conceptual and technical barrier that
 *   limited the 'thinkability' of digital money prior to its institutional
 *   measurement. It is a reading of the 'electronic_money_emergence' kernel,
 *   focusing on the gradual diffusion process of conceptual innovation rather
 *   than a single event. The constraint itself is the 'unthinkability' or
 *   'impossibility' of digital money, which gradually receded as technology
 *   advanced and social imagination expanded.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(electronic_money_emergence__became_thinkable_reading, 0.4).
domain_priors:suppression_score(electronic_money_emergence__became_thinkable_reading, 0.3).
domain_priors:theater_ratio(electronic_money_emergence__became_thinkable_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(electronic_money_emergence__became_thinkable_reading, extractiveness, 0.4).
narrative_ontology:constraint_metric(electronic_money_emergence__became_thinkable_reading, suppression_requirement, 0.3).
narrative_ontology:constraint_metric(electronic_money_emergence__became_thinkable_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(electronic_money_emergence__became_thinkable_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(electronic_money_emergence__became_thinkable_reading, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(electronic_money_emergence__became_thinkable_reading, mountain).
narrative_ontology:human_readable(electronic_money_emergence__became_thinkable_reading, "Conceptual and Technical Barrier to Digital Money Thinkability").
narrative_ontology:topic_domain(electronic_money_emergence__became_thinkable_reading, "economic_history/monetary_theory/technology_studies").

domain_priors:emerges_naturally(electronic_money_emergence__became_thinkable_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(electronic_money_emergence__became_thinkable_reading, '5554e2d4-c7e6-45ce-a229-55e9fe74a435').
narrative_ontology:cs_kernel_codification('5554e2d4-c7e6-45ce-a229-55e9fe74a435', implicit).
narrative_ontology:cs_authority_grounding('5554e2d4-c7e6-45ce-a229-55e9fe74a435', diffuse_epistemic).
narrative_ontology:cs_reading_relation('5554e2d4-c7e6-45ce-a229-55e9fe74a435', electronic_money_emergence__first_held_reading, influences).
narrative_ontology:cs_reading_relation('5554e2d4-c7e6-45ce-a229-55e9fe74a435', electronic_money_emergence__m4_m5_collapse_reading, coexists_with).
narrative_ontology:cs_axiom('5554e2d4-c7e6-45ce-a229-55e9fe74a435', foundational, conceptual_precedes_institutional_adoption).
narrative_ontology:cs_axiom_status(conceptual_precedes_institutional_adoption, holdable).
narrative_ontology:cs_axiom_grounding('5554e2d4-c7e6-45ce-a229-55e9fe74a435', conceptual_precedes_institutional_adoption, empirically_contingent).
narrative_ontology:cs_axiom('5554e2d4-c7e6-45ce-a229-55e9fe74a435', foundational, technological_limits_constrain_thought).
narrative_ontology:cs_axiom_status(technological_limits_constrain_thought, holdable).
narrative_ontology:cs_axiom_grounding('5554e2d4-c7e6-45ce-a229-55e9fe74a435', technological_limits_constrain_thought, empirically_contingent).
narrative_ontology:cs_reference_frame('5554e2d4-c7e6-45ce-a229-55e9fe74a435', pre_digital_conceptual_limits).
narrative_ontology:cs_drift_state('5554e2d4-c7e6-45ce-a229-55e9fe74a435', contemporary_digital_era, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('5554e2d4-c7e6-45ce-a229-55e9fe74a435', '').
narrative_ontology:cs_kernel_id(electronic_money_emergence__became_thinkable_reading, electronic_money_emergence).

% --- Structural relationships ---
narrative_ontology:constraint_victim(electronic_money_emergence__became_thinkable_reading, early_innovators).
narrative_ontology:constraint_victim(electronic_money_emergence__became_thinkable_reading, monetary_theorists).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Individuals and small teams attempting to conceptualize and build new forms of money, whose efforts are limited by the prevailing conceptual and technical boundaries of their era. They bear the cost of 'unthinkability' through failed attempts or delayed breakthroughs.
narrative_ontology:constraint_stakeholder(electronic_money_emergence__became_thinkable_reading, early_innovators, payer,
    moderate, biographical, constrained, global).

% Academics and researchers whose theoretical frameworks for money are constrained by the limits of what is technically and socially conceivable. They struggle to integrate novel concepts until the underlying 'thinkability' barrier recedes.
narrative_ontology:constraint_stakeholder(electronic_money_emergence__became_thinkable_reading, monetary_theorists, payer,
    analytical, generational, analytical, universal).

% The inherent limits of human conceptual frameworks and available technology at a given time, which define the boundary of what is 'thinkable' or 'possible' for new forms of money. This barrier implicitly sets the agenda for innovation by defining what problems can be addressed.
narrative_ontology:constraint_stakeholder(electronic_money_emergence__became_thinkable_reading, conceptual_technical_barrier, agenda_setter,
    institutional, civilizational, trapped, universal).

% Largely unaware of the underlying conceptual and technical constraints, but indirectly affected by the absence of digital money's benefits until the barrier recedes. They are passive recipients of the eventual innovations.
narrative_ontology:constraint_stakeholder(electronic_money_emergence__became_thinkable_reading, general_public, observer,
    powerless, immediate, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Implicitly coordinates research and development efforts by defining the frontier of the possible, pushing innovators towards overcoming the current limits of conceptual and technical feasibility for new monetary forms.
% TRANSFER_FUNCTION: Transfers the cost of conceptual and technical impossibility onto innovators and theorists, who must expend intellectual and material effort to overcome these inherent barriers.
% ABSENT_VOICES: Future innovators and users of digital money, whose needs could not be met until the conceptual barrier was overcome. Their potential contributions and benefits were 'absent' from the historical discourse.
% DISAPPEARANCE_RATIONALE: If the conceptual and technical barrier to digital money's thinkability had never existed, the entire trajectory of monetary history and technological development would be fundamentally different. Digital money would have emerged much earlier, altering economic structures and social interactions profoundly.
% FOUNDING_PROBLEM: The inherent limitations of human conceptual frameworks and available technology to conceive of and implement non-physical, electronic forms of money, leading to a conceptual void for new value transfer mechanisms.
% FOUNDING_PROBLEM_CORROBORATION: Historians of science and technology, and philosophers of mind, corroborate that conceptual breakthroughs are often constrained by prior frameworks and available tools. The specific barrier of 'unthinkability' for digital money has largely been overcome, as evidenced by its widespread adoption.
narrative_ontology:disappearance_verdict(electronic_money_emergence__became_thinkable_reading, world_rearranges).
narrative_ontology:founding_problem_status(electronic_money_emergence__became_thinkable_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(electronic_money_emergence__became_thinkable_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(electronic_money_emergence__became_thinkable_reading, 'none', 1).
narrative_ontology:epsilon_provenance(electronic_money_emergence__became_thinkable_reading, 0.4, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(electronic_money_emergence__became_thinkable_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(electronic_money_emergence__became_thinkable_reading, ExtMetricName, E),
    domain_priors:suppression_score(electronic_money_emergence__became_thinkable_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(electronic_money_emergence__became_thinkable_reading),
    narrative_ontology:constraint_metric(electronic_money_emergence__became_thinkable_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(electronic_money_emergence__became_thinkable_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(electronic_money_emergence__became_thinkable_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint is classified as a Mountain because it represents a fundamental, albeit evolving, limit of what was conceivable and technically feasible at a given time. Its 'extractiveness' and 'suppression' are high at the beginning of the interval, reflecting the significant costs and limitations imposed on innovators by the 'unthinkability' of digital money. These metrics decrease over time as the conceptual barrier is overcome. Theater ratio is low as there is no performative aspect to a conceptual limit. Accessibility collapse is high initially because alternatives (digital money) were conceptually unavailable, gradually lessening as the barrier recedes. Resistance is low because one does not 'resist' a fundamental limit, but rather works to overcome it.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the 'conceptual_technical_barrier' itself, its existence is a natural, immutable fact of the era. From the perspective of the 'early_innovators' and 'monetary_theorists', it is a frustrating, costly limitation that must be overcome. The engine's classification will reflect the objective structural reality of this barrier's impact.
 *
 * DIRECTIONALITY LOGIC:
 *   Early innovators and monetary theorists are positioned as 'payers' (victims) because they bear the costs of this conceptual barrier, struggling to develop or theorize what is not yet 'thinkable'. The 'conceptual_technical_barrier' itself acts as an 'agenda_setter' by defining the boundaries of innovation. The general public is an 'observer', indirectly affected by the absence of digital money's benefits.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_vs_social_limit,
    'Is the ''unthinkability'' of digital money a genuine natural limit of human cognition and available technology, or a socially constructed paradigm that could have been overcome earlier with different cultural or institutional frameworks?',
    'Comparative historical analysis of technological and conceptual development across different societies, or counterfactual history exploring alternative paths of innovation.',
    'If primarily a natural limit, the Mountain classification is robust. If substantially socially constructed, it might lean towards a more constructed type (e.g., a Snare of intellectual inertia or a Rope of shared but limiting paradigms), implying greater agency in its overcoming.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_vs_social_limit, conceptual, 'Ambiguity between inherent limits and social construction of conceptual barriers.').

omega_variable(
    measurement_of_thinkability,
    'How can ''thinkability'' be objectively measured or dated, given its subjective, distributed, and often implicit nature across a population of innovators and theorists?',
    'Development of robust quantitative methods for analyzing historical texts, patents, and scientific discourse for the emergence of specific conceptual primitives, or expert consensus on key conceptual milestones.',
    'Lack of objective measurement introduces irreducible uncertainty into the timing and magnitude of the constraint''s recession, potentially affecting the accuracy of temporal drift analysis.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(measurement_of_thinkability, empirical, 'Challenges in empirically measuring a conceptual state like ''thinkability''.').

omega_variable(
    sibling_reading_impact_first_held,
    'How would the ''first_held_reading'' (digital money emerged when first institutionally held) structurally differ from this ''became_thinkable_reading''?',
    'Analyzing the specific institutional and legal frameworks that enabled the first ''holding'' of dematerialized currency, and comparing their structural properties (e.g., beneficiaries, enforcement) to the conceptual barrier described here.',
    'The ''first_held_reading'' would likely describe a more institutionally-driven constraint, potentially a Tangled Rope or Snare, with identifiable institutional beneficiaries and active enforcement, contrasting with this reading''s focus on pre-institutional conceptual limits.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_impact_first_held, conceptual, 'Structural differences between conceptual emergence and institutional adoption of digital money.').

omega_variable(
    sibling_reading_impact_m4_m5_collapse,
    'How would the ''m4_m5_collapse_reading'' (emergence as a measurement artifact) structurally differ from this ''became_thinkable_reading''?',
    'Examining the specific statistical and regulatory decisions that led to the M4/M5 distinction and its impact on the definition of money, and comparing its structural properties (e.g., agenda-setters, beneficiaries) to the conceptual barrier described here.',
    'The ''m4_m5_collapse_reading'' would likely describe a constraint related to statistical classification and regulatory power, potentially a Rope or Tangled Rope of definitional coordination, with central banks or statistical agencies as agenda-setters, contrasting with this reading''s focus on pre-institutional conceptual limits.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_impact_m4_m5_collapse, conceptual, 'Structural differences between conceptual emergence and definitional/measurement-driven emergence of digital money.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(electronic_money_emergence__became_thinkable_reading, 1950, 2000).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(elec_tr_t1950, electronic_money_emergence__became_thinkable_reading, theater_ratio, 1950, 0.05).
narrative_ontology:measurement(elec_tr_t1960, electronic_money_emergence__became_thinkable_reading, theater_ratio, 1960, 0.05).
narrative_ontology:measurement(elec_tr_t1970, electronic_money_emergence__became_thinkable_reading, theater_ratio, 1970, 0.05).
narrative_ontology:measurement(elec_tr_t1980, electronic_money_emergence__became_thinkable_reading, theater_ratio, 1980, 0.05).
narrative_ontology:measurement(elec_tr_t1990, electronic_money_emergence__became_thinkable_reading, theater_ratio, 1990, 0.05).
narrative_ontology:measurement(elec_tr_t2000, electronic_money_emergence__became_thinkable_reading, theater_ratio, 2000, 0.05).

% Extraction over time
narrative_ontology:measurement(elec_be_t1950, electronic_money_emergence__became_thinkable_reading, base_extractiveness, 1950, 0.8).
narrative_ontology:measurement(elec_be_t1960, electronic_money_emergence__became_thinkable_reading, base_extractiveness, 1960, 0.7).
narrative_ontology:measurement(elec_be_t1970, electronic_money_emergence__became_thinkable_reading, base_extractiveness, 1970, 0.6).
narrative_ontology:measurement(elec_be_t1980, electronic_money_emergence__became_thinkable_reading, base_extractiveness, 1980, 0.5).
narrative_ontology:measurement(elec_be_t1990, electronic_money_emergence__became_thinkable_reading, base_extractiveness, 1990, 0.45).
narrative_ontology:measurement(elec_be_t2000, electronic_money_emergence__became_thinkable_reading, base_extractiveness, 2000, 0.4).

% Suppression requirement over time
narrative_ontology:measurement(elec_su_t1950, electronic_money_emergence__became_thinkable_reading, suppression_requirement, 1950, 0.7).
narrative_ontology:measurement(elec_su_t1960, electronic_money_emergence__became_thinkable_reading, suppression_requirement, 1960, 0.6).
narrative_ontology:measurement(elec_su_t1970, electronic_money_emergence__became_thinkable_reading, suppression_requirement, 1970, 0.5).
narrative_ontology:measurement(elec_su_t1980, electronic_money_emergence__became_thinkable_reading, suppression_requirement, 1980, 0.4).
narrative_ontology:measurement(elec_su_t1990, electronic_money_emergence__became_thinkable_reading, suppression_requirement, 1990, 0.35).
narrative_ontology:measurement(elec_su_t2000, electronic_money_emergence__became_thinkable_reading, suppression_requirement, 2000, 0.3).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
