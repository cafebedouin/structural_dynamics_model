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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: digital_money_origin__became_thinkable_reading
 *   human_readable: Digital Money Origin: Conceptually Conceivable
 *   domain: monetary_history/technology_studies/institutional_economics
 *
 * SUMMARY:
 *   This constraint defines the origin of digital money as the point when its
 *   concept became technically and institutionally conceivable, preceding its
 *   actual implementation or widespread use. It is a 'Mountain' because the
 *   conceptual shift, once achieved, is an irreversible historical fact. The
 *   beneficiaries are the early architects and visionaries who shaped this
 *   conceptualization, while those whose alternative framings were excluded
 *   bear a diffuse cost. The metrics reflect a low but present
 *   extractiveness, as intellectual leadership and definitional power are
 *   concentrated, and a low suppression, as the 'constraint' is primarily a
 *   conceptual barrier rather than an actively enforced rule.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(digital_money_origin__became_thinkable_reading, 0.15).
domain_priors:suppression_score(digital_money_origin__became_thinkable_reading, 0.25).
domain_priors:theater_ratio(digital_money_origin__became_thinkable_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(digital_money_origin__became_thinkable_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(digital_money_origin__became_thinkable_reading, suppression_requirement, 0.25).
narrative_ontology:constraint_metric(digital_money_origin__became_thinkable_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(digital_money_origin__became_thinkable_reading, accessibility_collapse, 0.88).
narrative_ontology:constraint_metric(digital_money_origin__became_thinkable_reading, resistance, 0.02).

% --- Constraint claim ---
narrative_ontology:constraint_claim(digital_money_origin__became_thinkable_reading, mountain).
narrative_ontology:human_readable(digital_money_origin__became_thinkable_reading, "Digital Money Origin: Conceptually Conceivable").
narrative_ontology:topic_domain(digital_money_origin__became_thinkable_reading, "monetary_history/technology_studies/institutional_economics").

domain_priors:emerges_naturally(digital_money_origin__became_thinkable_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(digital_money_origin__became_thinkable_reading, 'eb135368-d565-4a0d-bbc7-ce52d0c46a3b').
narrative_ontology:cs_kernel_codification('eb135368-d565-4a0d-bbc7-ce52d0c46a3b', implicit).
narrative_ontology:cs_authority_grounding('eb135368-d565-4a0d-bbc7-ce52d0c46a3b', expertise).
narrative_ontology:cs_interpretation_layer_present('eb135368-d565-4a0d-bbc7-ce52d0c46a3b').
narrative_ontology:cs_reading_relation('eb135368-d565-4a0d-bbc7-ce52d0c46a3b', digital_money_origin__first_held_reading, influences).
narrative_ontology:cs_reading_relation('eb135368-d565-4a0d-bbc7-ce52d0c46a3b', digital_money_origin__regulatory_recognition_reading, influences).
narrative_ontology:cs_axiom('eb135368-d565-4a0d-bbc7-ce52d0c46a3b', foundational, conceptual_precedes_material).
narrative_ontology:cs_axiom_status(conceptual_precedes_material, holdable).
narrative_ontology:cs_axiom_grounding('eb135368-d565-4a0d-bbc7-ce52d0c46a3b', conceptual_precedes_material, conventional).
narrative_ontology:cs_axiom('eb135368-d565-4a0d-bbc7-ce52d0c46a3b', foundational, technical_institutional_feasibility_is_origin).
narrative_ontology:cs_axiom_status(technical_institutional_feasibility_is_origin, holdable).
narrative_ontology:cs_axiom_grounding('eb135368-d565-4a0d-bbc7-ce52d0c46a3b', technical_institutional_feasibility_is_origin, empirically_contingent).
narrative_ontology:cs_reference_frame('eb135368-d565-4a0d-bbc7-ce52d0c46a3b', conceptual_feasibility_paradigm).
narrative_ontology:cs_drift_state('eb135368-d565-4a0d-bbc7-ce52d0c46a3b', contemporary_implementation_era, gap(practice_drift, minor, true)).
narrative_ontology:cs_created_at('eb135368-d565-4a0d-bbc7-ce52d0c46a3b', '').
narrative_ontology:cs_kernel_id(digital_money_origin__became_thinkable_reading, digital_money_origin).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(digital_money_origin__became_thinkable_reading, early_institutional_architects).
narrative_ontology:constraint_beneficiary(digital_money_origin__became_thinkable_reading, technological_visionaries).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(digital_money_origin__became_thinkable_reading, traditional_monetary_theorists).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These are the individuals and groups who first conceptualized digital money within existing institutional frameworks, laying the groundwork for its eventual development and benefiting from the intellectual leadership and influence this early framing provided.
narrative_ontology:constraint_stakeholder(digital_money_origin__became_thinkable_reading, early_institutional_architects, beneficiary,
    institutional, generational, analytical, global).

% Researchers and innovators who foresaw the technical feasibility of digital money, contributing to the conceptual 'conceivability' and gaining reputational and intellectual capital from their foresight.
narrative_ontology:constraint_stakeholder(digital_money_origin__became_thinkable_reading, technological_visionaries, beneficiary,
    powerful, biographical, mobile, global).

% Academics and economists whose existing theories of money were challenged or expanded by the emergence of digital money as a concept. They bore the 'cost' of intellectual re-evaluation and adaptation.
narrative_ontology:constraint_stakeholder(digital_money_origin__became_thinkable_reading, traditional_monetary_theorists, payer,
    organized, generational, constrained, global).

% Alternative or marginalized conceptualizations of digital money that did not gain institutional or technical traction in the early stages. Their ideas were not incorporated into the dominant 'conceivable' framework.
narrative_ontology:constraint_stakeholder(digital_money_origin__became_thinkable_reading, excluded_conceptual_framings, excluded,
    powerless, generational, trapped, global).

% Individuals who would eventually use digital money, but whose perspective was not yet relevant at the conceptualization stage. They observe the historical framing of the constraint.
narrative_ontology:constraint_stakeholder(digital_money_origin__became_thinkable_reading, future_digital_money_users, observer,
    powerless, biographical, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: It established a shared conceptual space and technical vocabulary for discussing and developing digital money, coordinating early research and institutional efforts.
% TRANSFER_FUNCTION: Transferred intellectual leadership and definitional power to those who first conceived and framed digital money within existing technical and institutional paradigms.
% ABSENT_VOICES: Alternative conceptual framings of digital money, particularly those from outside established financial or technological institutions, were absent. They would have argued for different foundational principles or technical architectures.
% DISAPPEARANCE_RATIONALE: The conceptual emergence of digital money, once it became conceivable, is a historical fact. Its disappearance would not alter the subsequent historical trajectory of its development, only our understanding of its origin point.
% FOUNDING_PROBLEM: The problem of how to create a non-physical, electronically transferable form of value that could function as money within existing or emerging technical and institutional constraints.
% FOUNDING_PROBLEM_CORROBORATION: Historians of technology and economics corroborate that the conceptual problem was indeed 'solved' by the mid-20th century, leading to subsequent implementation efforts. The problem itself is no longer 'live' in the same way, having transitioned to implementation challenges.
narrative_ontology:disappearance_verdict(digital_money_origin__became_thinkable_reading, world_unchanged).
narrative_ontology:founding_problem_status(digital_money_origin__became_thinkable_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(digital_money_origin__became_thinkable_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_gemini+stakeholder_backfill', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(digital_money_origin__became_thinkable_reading, 'none', 1).
narrative_ontology:epsilon_provenance(digital_money_origin__became_thinkable_reading, 0.15, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(digital_money_origin__became_thinkable_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(digital_money_origin__became_thinkable_reading, ExtMetricName, E),
    domain_priors:suppression_score(digital_money_origin__became_thinkable_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(digital_money_origin__became_thinkable_reading),
    narrative_ontology:constraint_metric(digital_money_origin__became_thinkable_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(digital_money_origin__became_thinkable_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(digital_money_origin__became_thinkable_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint is classified as a Mountain because the 'conceivability' of digital money, once established, became an unchangeable historical and intellectual reality. Its extractiveness is low (0.15) because the 'cost' is primarily intellectual (re-evaluation, exclusion of alternative ideas), not material. Suppression is low (0.25) as it's about conceptual barriers, not active coercion. Theater ratio is minimal (0.05) as there's little performative maintenance of a historical conceptual shift. Accessibility collapse is high (0.88) because once the concept is understood, the 'alternative' of not conceiving it collapses. Resistance is low (0.02) because the conceptual shift was largely accepted by relevant intellectual communities.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the early architects, this was a natural evolution of thought, a 'mountain' of intellectual progress. From the perspective of excluded framings, it was a 'snare' that foreclosed alternative paths. The engine's classification as a Mountain with beneficiaries and omegas addresses this tension, flagging it as a potential false summit.
 *
 * DIRECTIONALITY LOGIC:
 *   Early institutional architects and technological visionaries are beneficiaries (d near 0.0) as they gained influence and intellectual capital from shaping the concept. Traditional monetary theorists are payers (d near 1.0) as they had to adapt their frameworks. Excluded conceptual framings are also payers, bearing the cost of marginalization. Future digital money users are observers, as their direct relationship to the constraint had not yet materialized.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_vs_constructed_conceivability,
    'Is the ''conceivability'' of digital money a natural, inevitable outcome of technological and intellectual progress, or was it a constructed consensus shaped by specific institutional actors?',
    'Historical counterfactual analysis: could alternative conceptualizations have prevailed given different institutional or technological paths? Examination of suppressed or marginalized early ideas.',
    'If constructed, the constraint''s ''mountain'' classification is a false summit, and the beneficiaries'' role in shaping the narrative becomes more extractive, potentially reclassifying it as a Tangled Rope or Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_vs_constructed_conceivability, conceptual, 'Ambiguity between natural conceptual emergence and institutionally constructed consensus.').

omega_variable(
    origin_point_definition_impact,
    'How does defining the origin of digital money at ''conceivability'' (rather than ''first holding'' or ''regulatory recognition'') impact the perceived legitimacy and historical narrative of subsequent digital money developments?',
    'Comparative historical analysis of narratives: examine how different origin points are used to justify or critique current digital money systems. Analyze the ''founding myths'' of various digital currencies.',
    'An earlier origin point (conceivability) might lend greater historical inevitability and legitimacy to current systems, potentially dampening resistance to their extractive aspects. A later origin point might highlight the contingent and constructed nature of digital money.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(origin_point_definition_impact, preference, 'Impact of origin definition on historical narrative and legitimacy.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(digital_money_origin__became_thinkable_reading, 1940, 1970).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(digi_tr_t1940, digital_money_origin__became_thinkable_reading, theater_ratio, 1940, 0.01).
narrative_ontology:measurement(digi_tr_t1950, digital_money_origin__became_thinkable_reading, theater_ratio, 1950, 0.02).
narrative_ontology:measurement(digi_tr_t1960, digital_money_origin__became_thinkable_reading, theater_ratio, 1960, 0.03).
narrative_ontology:measurement(digi_tr_t1970, digital_money_origin__became_thinkable_reading, theater_ratio, 1970, 0.05).

% Extraction over time
narrative_ontology:measurement(digi_be_t1940, digital_money_origin__became_thinkable_reading, base_extractiveness, 1940, 0.1).
narrative_ontology:measurement(digi_be_t1950, digital_money_origin__became_thinkable_reading, base_extractiveness, 1950, 0.12).
narrative_ontology:measurement(digi_be_t1960, digital_money_origin__became_thinkable_reading, base_extractiveness, 1960, 0.14).
narrative_ontology:measurement(digi_be_t1970, digital_money_origin__became_thinkable_reading, base_extractiveness, 1970, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(digi_su_t1940, digital_money_origin__became_thinkable_reading, suppression_requirement, 1940, 0.15).
narrative_ontology:measurement(digi_su_t1950, digital_money_origin__became_thinkable_reading, suppression_requirement, 1950, 0.2).
narrative_ontology:measurement(digi_su_t1960, digital_money_origin__became_thinkable_reading, suppression_requirement, 1960, 0.23).
narrative_ontology:measurement(digi_su_t1970, digital_money_origin__became_thinkable_reading, suppression_requirement, 1970, 0.25).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
