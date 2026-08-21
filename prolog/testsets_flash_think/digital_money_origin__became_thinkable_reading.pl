% ============================================================================
% CONSTRAINT STORY: digital_money_origin__became_thinkable_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_digital_money_origin__became_thinkable_reading, []).

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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: digital_money_origin__became_thinkable_reading
 *   human_readable: Digital Money's Conceptual Emergence (Became Thinkable Reading)
 *   domain: monetary_history/technology_studies/institutional_economics
 *
 * SUMMARY:
 *   This constraint story represents the 'became_thinkable_reading' of the
 *   'digital_money_origin' kernel. It posits that digital money emerged when
 *   the concept became technically and institutionally conceivable, prior to
 *   widespread implementation. This involves the intellectual and
 *   institutional work of defining what digital money could be, setting the
 *   stage for its eventual development. The constraint is a Tangled Rope
 *   because it coordinates the conceptual space and institutional readiness
 *   for digital money while simultaneously extracting from those whose
 *   alternative conceptualizations are excluded or who are slow to adapt to
 *   the new paradigm.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(digital_money_origin__became_thinkable_reading, 0.3).
domain_priors:suppression_score(digital_money_origin__became_thinkable_reading, 0.4).
domain_priors:theater_ratio(digital_money_origin__became_thinkable_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(digital_money_origin__became_thinkable_reading, extractiveness, 0.3).
narrative_ontology:constraint_metric(digital_money_origin__became_thinkable_reading, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(digital_money_origin__became_thinkable_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(digital_money_origin__became_thinkable_reading, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(digital_money_origin__became_thinkable_reading, resistance, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(digital_money_origin__became_thinkable_reading, tangled_rope).
narrative_ontology:human_readable(digital_money_origin__became_thinkable_reading, "Digital Money's Conceptual Emergence (Became Thinkable Reading)").
narrative_ontology:topic_domain(digital_money_origin__became_thinkable_reading, "monetary_history/technology_studies/institutional_economics").

domain_priors:requires_active_enforcement(digital_money_origin__became_thinkable_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(digital_money_origin__became_thinkable_reading, '355dc29b-b8e5-4e90-93df-0411e883e1e6').
narrative_ontology:cs_kernel_codification('355dc29b-b8e5-4e90-93df-0411e883e1e6', implicit).
narrative_ontology:cs_authority_grounding('355dc29b-b8e5-4e90-93df-0411e883e1e6', expertise).
narrative_ontology:cs_interpretation_layer_present('355dc29b-b8e5-4e90-93df-0411e883e1e6').
narrative_ontology:cs_reading_relation('355dc29b-b8e5-4e90-93df-0411e883e1e6', digital_money_origin__first_held_reading, influences).
narrative_ontology:cs_reading_relation('355dc29b-b8e5-4e90-93df-0411e883e1e6', digital_money_origin__regulatory_recognition_reading, influences).
narrative_ontology:cs_axiom('355dc29b-b8e5-4e90-93df-0411e883e1e6', foundational, technological_precondition_for_money).
narrative_ontology:cs_axiom_status(technological_precondition_for_money, holdable).
narrative_ontology:cs_axiom_grounding('355dc29b-b8e5-4e90-93df-0411e883e1e6', technological_precondition_for_money, empirically_contingent).
narrative_ontology:cs_axiom('355dc29b-b8e5-4e90-93df-0411e883e1e6', foundational, institutional_acceptance_as_legitimacy).
narrative_ontology:cs_axiom_status(institutional_acceptance_as_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('355dc29b-b8e5-4e90-93df-0411e883e1e6', institutional_acceptance_as_legitimacy, conventional).
narrative_ontology:cs_reference_frame('355dc29b-b8e5-4e90-93df-0411e883e1e6', conceptual_technological_frontier).
narrative_ontology:cs_drift_state('355dc29b-b8e5-4e90-93df-0411e883e1e6', contemporary_digital_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('355dc29b-b8e5-4e90-93df-0411e883e1e6', '').
narrative_ontology:cs_kernel_id(digital_money_origin__became_thinkable_reading, digital_money_origin).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(digital_money_origin__became_thinkable_reading, early_institutional_architects).
narrative_ontology:constraint_beneficiary(digital_money_origin__became_thinkable_reading, visionary_technologists).
narrative_ontology:constraint_victim(digital_money_origin__became_thinkable_reading, traditional_financial_institutions_resisting_change).
narrative_ontology:constraint_victim(digital_money_origin__became_thinkable_reading, excluded_conceptual_framers).
narrative_ontology:constraint_vindicates(digital_money_origin__became_thinkable_reading, technological_determinism_narrative).
narrative_ontology:constraint_vindicates(digital_money_origin__became_thinkable_reading, institutional_innovation_theory).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These are the individuals and groups who actively shaped the conceptual framework and institutional readiness for digital money. They benefit from defining the terms of the emerging field and gain influence and resources.
narrative_ontology:constraint_stakeholder(digital_money_origin__became_thinkable_reading, early_institutional_architects, agenda_setter,
    institutional, generational, mobile, global).

% Technologists whose ideas and prototypes align with the emerging conceptual framework for digital money. They gain status, funding, and opportunities as their work becomes relevant and validated by the institutional architects.
narrative_ontology:constraint_stakeholder(digital_money_origin__became_thinkable_reading, visionary_technologists, beneficiary,
    powerful, biographical, arbitrage, global).

% Established financial entities that initially resist or fail to grasp the conceptual shift towards digital money. They bear the cost of lost opportunities, market share, and the eventual need for costly adaptation as the concept solidifies.
narrative_ontology:constraint_stakeholder(digital_money_origin__became_thinkable_reading, traditional_financial_institutions_resisting_change, payer,
    institutional, biographical, constrained, global).

% Individuals or groups who proposed alternative conceptualizations of digital money that were not adopted or recognized by the dominant institutional architects. Their ideas are marginalized, and they lose influence and potential future benefits.
narrative_ontology:constraint_stakeholder(digital_money_origin__became_thinkable_reading, excluded_conceptual_framers, payer,
    moderate, biographical, constrained, global).

% Academics and researchers who analyze the historical development and conceptual shifts in monetary systems, including the emergence of digital money. They provide an analytical perspective without direct participation in the constraint's operation.
narrative_ontology:constraint_stakeholder(digital_money_origin__became_thinkable_reading, monetary_historians, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(digital_money_origin__became_thinkable_reading, early_institutional_architects).
narrative_ontology:fixing_cost_class(digital_money_origin__became_thinkable_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the intellectual and institutional resources necessary to conceive of and legitimize digital money as a viable future, establishing a shared understanding of its potential and challenges.
% TRANSFER_FUNCTION: Transfers intellectual authority, future market positioning, and institutional legitimacy to those who successfully frame and champion the concept of digital money, from those whose alternative conceptualizations are excluded or who are slow to adapt.
% ABSENT_VOICES: Alternative conceptual framers and early skeptics whose ideas were not integrated into the dominant narrative of digital money's emergence. They would argue for different foundational principles or a more cautious approach, but were not part of the core conceptual development.
% DISAPPEARANCE_RATIONALE: If the concept of digital money had never become technically and institutionally conceivable, the entire trajectory of financial innovation, technological development, and institutional adaptation would be fundamentally different, leading to a vastly altered global financial landscape.
% FOUNDING_PROBLEM: The limitations of purely physical or analog monetary systems in an increasingly digitalizing world, and the need for a conceptual and institutional framework to enable future financial innovation and address emerging technological possibilities.
% FOUNDING_PROBLEM_CORROBORATION: Historians of technology and finance, economists, and central bank researchers attest to the historical limitations of analog systems and the ongoing conceptual evolution of digital money, supporting the idea that the core problem of adapting money to a digital age remains relevant, albeit in new forms.
narrative_ontology:disappearance_verdict(digital_money_origin__became_thinkable_reading, world_rearranges).
narrative_ontology:founding_problem_status(digital_money_origin__became_thinkable_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(digital_money_origin__became_thinkable_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(digital_money_origin__became_thinkable_reading, 'none', 1).
narrative_ontology:epsilon_provenance(digital_money_origin__became_thinkable_reading, 0.3, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(digital_money_origin__became_thinkable_reading_tests).
:- end_tests(digital_money_origin__became_thinkable_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate because the 'extraction' here is primarily intellectual and positional, rather than direct financial transfer. It's about who gets to define the future of money. Suppression is also moderate, reflecting the active gatekeeping of conceptual frameworks and institutional buy-in. Theater ratio is low as the core activity is genuine conceptual and institutional development, not performance. Accessibility collapse is high because once the concept is widely accepted, the idea of a purely physical monetary future becomes less viable. Resistance is low as the conceptual shift itself is not widely resisted, but rather the implications of its implementation later.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the early institutional architects, this constraint is a necessary coordination mechanism for progress. From the perspective of excluded conceptual framers, it represents an extractive process that marginalizes alternative visions and concentrates power in the hands of a few. The engine's per-seat classification will reflect this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Early institutional architects and visionary technologists are beneficiaries, gaining influence and resources by shaping the conceptual landscape. Traditional financial institutions resisting change and excluded conceptual framers are victims, bearing the costs of being left behind or having their ideas marginalized. Monetary historians serve as analytical observers.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    conceptual_vs_implementation_origin,
    'Is the true origin of digital money primarily a conceptual breakthrough, or is it inextricably linked to its first practical implementation?',
    'Detailed historical analysis comparing the impact of conceptual papers and institutional frameworks versus the impact of early, widespread practical use cases.',
    'If the origin is more tied to implementation, this reading''s origin date would be too early, and its beneficiaries/victims would shift to those involved in early practical systems, potentially reclassifying it as a different type.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(conceptual_vs_implementation_origin, conceptual, 'Ambiguity regarding whether digital money''s origin is conceptual or practical.').

omega_variable(
    exclusion_mechanism_nature,
    'Was the exclusion of alternative conceptual framings a natural outcome of intellectual competition, or an active institutional suppression of dissenting ideas?',
    'Sociological study of early institutional networks and funding patterns, examining explicit gatekeeping mechanisms versus implicit biases in idea adoption.',
    'If active suppression is dominant, the ''suppression'' metric for this constraint is more coercive than currently assessed, potentially pushing the classification closer to a Snare.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(exclusion_mechanism_nature, empirical, 'Nature of exclusion for alternative digital money conceptualizations.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(digital_money_origin__became_thinkable_reading, 1970, 2000).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(digi_tr_t1970, digital_money_origin__became_thinkable_reading, theater_ratio, 1970, 0.08).
narrative_ontology:measurement(digi_tr_t1976, digital_money_origin__became_thinkable_reading, theater_ratio, 1976, 0.09).
narrative_ontology:measurement(digi_tr_t1982, digital_money_origin__became_thinkable_reading, theater_ratio, 1982, 0.1).
narrative_ontology:measurement(digi_tr_t1988, digital_money_origin__became_thinkable_reading, theater_ratio, 1988, 0.1).
narrative_ontology:measurement(digi_tr_t1994, digital_money_origin__became_thinkable_reading, theater_ratio, 1994, 0.1).
narrative_ontology:measurement(digi_tr_t2000, digital_money_origin__became_thinkable_reading, theater_ratio, 2000, 0.1).

% Extraction over time
narrative_ontology:measurement(digi_be_t1970, digital_money_origin__became_thinkable_reading, base_extractiveness, 1970, 0.2).
narrative_ontology:measurement(digi_be_t1976, digital_money_origin__became_thinkable_reading, base_extractiveness, 1976, 0.23).
narrative_ontology:measurement(digi_be_t1982, digital_money_origin__became_thinkable_reading, base_extractiveness, 1982, 0.25).
narrative_ontology:measurement(digi_be_t1988, digital_money_origin__became_thinkable_reading, base_extractiveness, 1988, 0.27).
narrative_ontology:measurement(digi_be_t1994, digital_money_origin__became_thinkable_reading, base_extractiveness, 1994, 0.29).
narrative_ontology:measurement(digi_be_t2000, digital_money_origin__became_thinkable_reading, base_extractiveness, 2000, 0.3).

% Suppression requirement over time
narrative_ontology:measurement(digi_su_t1970, digital_money_origin__became_thinkable_reading, suppression_requirement, 1970, 0.3).
narrative_ontology:measurement(digi_su_t1976, digital_money_origin__became_thinkable_reading, suppression_requirement, 1976, 0.33).
narrative_ontology:measurement(digi_su_t1982, digital_money_origin__became_thinkable_reading, suppression_requirement, 1982, 0.36).
narrative_ontology:measurement(digi_su_t1988, digital_money_origin__became_thinkable_reading, suppression_requirement, 1988, 0.38).
narrative_ontology:measurement(digi_su_t1994, digital_money_origin__became_thinkable_reading, suppression_requirement, 1994, 0.39).
narrative_ontology:measurement(digi_su_t2000, digital_money_origin__became_thinkable_reading, suppression_requirement, 2000, 0.4).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(digital_money_origin__became_thinkable_reading, identity_coordination).
narrative_ontology:affects_constraint(digital_money_origin__became_thinkable_reading, digital_money_origin__first_held_reading).
narrative_ontology:affects_constraint(digital_money_origin__became_thinkable_reading, digital_money_origin__regulatory_recognition_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'digital_money_origin' kernel, focusing on the conceptual and institutional conceivability. It influences the 'first_held_reading' and 'regulatory_recognition_reading' by establishing the foundational conceptual space.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
