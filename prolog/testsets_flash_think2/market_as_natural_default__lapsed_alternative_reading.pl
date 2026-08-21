% ============================================================================
% CONSTRAINT STORY: market_as_natural_default__lapsed_alternative_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_market_as_natural_default__lapsed_alternative_reading, []).

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
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   constraint_id: market_as_natural_default__lapsed_alternative_reading
 *   human_readable: Market as Natural Default (Lapsed Alternative Reading)
 *   domain: political_economy/ideology_studies/economic_history
 *
 * SUMMARY:
 *   This constraint, the 'lapsed_alternative_reading' of the
 *   'market_as_natural_default' kernel, posits that the dominance of
 *   market-based economic systems stems primarily from a historical
 *   forgetting of viable alternatives, rather than active, coercive
 *   suppression. It is a Piton because its persistence is due to ideological
 *   inertia and a lack of collective memory, not active maintenance by a
 *   concentrated beneficiary. The low extractiveness reflects that the
 *   'extraction' is primarily in the form of foregone opportunities and
 *   unexamined assumptions, rather than direct, active rent-seeking through
 *   this specific mechanism of forgetting.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(market_as_natural_default__lapsed_alternative_reading, 0.12).
domain_priors:suppression_score(market_as_natural_default__lapsed_alternative_reading, 0.2).
domain_priors:theater_ratio(market_as_natural_default__lapsed_alternative_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(market_as_natural_default__lapsed_alternative_reading, extractiveness, 0.12).
narrative_ontology:constraint_metric(market_as_natural_default__lapsed_alternative_reading, suppression_requirement, 0.2).
narrative_ontology:constraint_metric(market_as_natural_default__lapsed_alternative_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(market_as_natural_default__lapsed_alternative_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(market_as_natural_default__lapsed_alternative_reading, resistance, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(market_as_natural_default__lapsed_alternative_reading, piton).
narrative_ontology:human_readable(market_as_natural_default__lapsed_alternative_reading, "Market as Natural Default (Lapsed Alternative Reading)").
narrative_ontology:topic_domain(market_as_natural_default__lapsed_alternative_reading, "political_economy/ideology_studies/economic_history").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(market_as_natural_default__lapsed_alternative_reading, '67541d80-57b5-48ea-bd0f-b2823f2d03b6').
narrative_ontology:cs_kernel_codification('67541d80-57b5-48ea-bd0f-b2823f2d03b6', implicit).
narrative_ontology:cs_authority_grounding('67541d80-57b5-48ea-bd0f-b2823f2d03b6', practice).
narrative_ontology:cs_reading_relation('67541d80-57b5-48ea-bd0f-b2823f2d03b6', market_as_natural_default__beneficiary_maintained_reading, coexists_with).
narrative_ontology:cs_reading_relation('67541d80-57b5-48ea-bd0f-b2823f2d03b6', market_as_natural_default__hybrid_amnesia_reading, coexists_with).
narrative_ontology:cs_axiom('67541d80-57b5-48ea-bd0f-b2823f2d03b6', foundational, historical_contingency_of_markets).
narrative_ontology:cs_axiom_status(historical_contingency_of_markets, holdable).
narrative_ontology:cs_axiom_grounding('67541d80-57b5-48ea-bd0f-b2823f2d03b6', historical_contingency_of_markets, empirically_contingent).
narrative_ontology:cs_axiom('67541d80-57b5-48ea-bd0f-b2823f2d03b6', secondary, recoverability_of_alternatives).
narrative_ontology:cs_axiom_status(recoverability_of_alternatives, holdable).
narrative_ontology:cs_axiom_grounding('67541d80-57b5-48ea-bd0f-b2823f2d03b6', recoverability_of_alternatives, empirically_contingent).
narrative_ontology:cs_reference_frame('67541d80-57b5-48ea-bd0f-b2823f2d03b6', market_as_efficient_natural_order).
narrative_ontology:cs_drift_state('67541d80-57b5-48ea-bd0f-b2823f2d03b6', contemporary_critical_scholarship, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('67541d80-57b5-48ea-bd0f-b2823f2d03b6', '').
narrative_ontology:cs_kernel_id(market_as_natural_default__lapsed_alternative_reading, market_as_natural_default).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(market_as_natural_default__lapsed_alternative_reading, dominant_firms).
narrative_ontology:constraint_beneficiary(market_as_natural_default__lapsed_alternative_reading, investors).
narrative_ontology:constraint_victim(market_as_natural_default__lapsed_alternative_reading, citizens).
narrative_ontology:constraint_victim(market_as_natural_default__lapsed_alternative_reading, labor).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from the market being the unquestioned default, reducing competitive pressure from alternative economic models. They do not actively maintain the 'forgetting' but profit from its effects.
narrative_ontology:constraint_stakeholder(market_as_natural_default__lapsed_alternative_reading, dominant_firms, beneficiary,
    powerful, biographical, arbitrage, global).

% Benefit from the stability and predictability of the market as the default system, which underpins their asset valuations and investment strategies. Like dominant firms, they do not actively enforce the forgetting.
narrative_ontology:constraint_stakeholder(market_as_natural_default__lapsed_alternative_reading, investors, beneficiary,
    powerful, biographical, arbitrage, global).

% Bear the diffuse costs of a system that is presented as the only option, potentially missing out on benefits from forgotten or suppressed alternatives. Their choices are limited by the perceived naturalness of the market.
narrative_ontology:constraint_stakeholder(market_as_natural_default__lapsed_alternative_reading, citizens, payer,
    moderate, biographical, constrained, national).

% Operates within a framework where market logic often dictates terms, limiting the perceived viability of non-market-based labor arrangements or worker-owned enterprises. They bear the costs of this ideological constraint.
narrative_ontology:constraint_stakeholder(market_as_natural_default__lapsed_alternative_reading, labor, payer,
    organized, biographical, constrained, national).

% Research and document the historical contingency of market structures and the existence of forgotten alternatives. They provide the analytical lens through which the 'lapsed alternative' reading is constructed.
narrative_ontology:constraint_stakeholder(market_as_natural_default__lapsed_alternative_reading, economic_historians, observer,
    analytical, generational, analytical, global).

% Operate within the prevailing ideological framework that treats the market as the natural default, often limiting their consideration of non-market solutions to social and economic problems. They could, in principle, introduce alternatives but are constrained by the dominant narrative.
narrative_ontology:constraint_stakeholder(market_as_natural_default__lapsed_alternative_reading, policy_makers, agenda_setter,
    institutional, biographical, mobile, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(market_as_natural_default__lapsed_alternative_reading, diffuse).
narrative_ontology:fixing_cost_class(market_as_natural_default__lapsed_alternative_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a widely accepted, albeit historically contingent, framework for organizing economic activity, reducing the need for constant re-negotiation of fundamental institutional design.
% TRANSFER_FUNCTION: Diffusely transfers potential gains from forgotten or unconsidered alternative economic arrangements to the existing market structure, benefiting those who thrive within it and imposing opportunity costs on others.
% ABSENT_VOICES: Advocates for historically suppressed or forgotten economic models (e.g., cooperative economies, public utilities, commons-based peer production) are absent from mainstream policy discourse and public imagination due to the pervasive 'natural default' narrative.
% DISAPPEARANCE_RATIONALE: If the historical amnesia were overcome and viable alternatives were widely remembered and presented as legitimate options, the default status of the market would be challenged. This would lead to a re-evaluation of economic structures, potentially new institutional designs, and a significant shift in policy debates.
% FOUNDING_PROBLEM: The need for a stable, widely accepted framework for economic organization in complex societies, particularly after periods of social or political upheaval.
% FOUNDING_PROBLEM_CORROBORATION: Mainstream economists and business leaders often attest that the market efficiently solves the problem of resource allocation and innovation. However, economic historians and critical theorists attest that the 'problem' was often framed in ways that excluded alternatives, and that the current arrangement persists more due to ideological inertia than optimal function. Legislative hearings and academic research from outside the immediate beneficiaries support the contested status.
narrative_ontology:disappearance_verdict(market_as_natural_default__lapsed_alternative_reading, world_rearranges).
narrative_ontology:founding_problem_status(market_as_natural_default__lapsed_alternative_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(market_as_natural_default__lapsed_alternative_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(market_as_natural_default__lapsed_alternative_reading, 'none', 1).
narrative_ontology:epsilon_provenance(market_as_natural_default__lapsed_alternative_reading, 0.12, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(market_as_natural_default__lapsed_alternative_reading_tests).
:- end_tests(market_as_natural_default__lapsed_alternative_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The low extractiveness (0.12) and suppression (0.20) reflect that this constraint operates through ideological inertia and cognitive patterns, not active enforcement or direct rent collection. Alternatives are 'forgotten' rather than 'actively closed.' The moderate theater ratio (0.40) indicates that the narrative of the market's 'naturalness' or 'inevitability' serves a performative function, obscuring its historical contingency. The Piton classification is appropriate as the constraint persists due to a lack of collective memory and the diffuse nature of its 'benefits' (stability for incumbents), rather than active maintenance by a specific, concentrated beneficiary.
 *
 * PERSPECTIVAL GAP:
 *   Those who operate within the market system often perceive its default status as natural or self-evident, while critical scholars and economic historians view it as a historically contingent outcome. The constraint's operation is largely invisible to the former, while it is a central object of analysis for the latter.
 *
 * DIRECTIONALITY LOGIC:
 *   Dominant firms and investors are diffuse beneficiaries; they profit from the stability and lack of challenge to the market system, but do not actively 'maintain' the forgetting itself. Citizens and labor are diffuse payers, bearing the opportunity costs of unexamined alternatives. Policy makers, while agenda-setters, are also constrained by the prevailing ideological default. Economic historians act as analytical observers, attempting to uncover the forgotten alternatives.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's 'mandate' to provide a stable economic framework has atrophied into an unexamined default. The original problem of economic organization is now 'solved' by a single, ideologically dominant solution, whose alternatives have been forgotten. This prevents mislabeling it as a Snare, as there isn't a concentrated beneficiary actively extracting through the 'forgetting' mechanism, but rather a diffuse benefit from the resulting ideological inertia. It's a Piton because the cost of 'fixing' (recovering historical memory and re-introducing alternatives) is prohibitive due to the depth of the ideological entrenchment, even if no single party actively maintains the forgetting.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    active_closure_vs_forgetting,
    'To what extent is market dominance maintained by active, deliberate closure of alternatives versus passive historical forgetting?',
    'Detailed historical case studies of specific policy decisions and institutional designs, analyzing the intent and mechanisms behind the marginalization of non-market alternatives.',
    'If active closure is dominant, the constraint''s suppression and extractiveness would be higher, potentially reclassifying it as a Snare or Tangled Rope. If forgetting is dominant, the Piton classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(active_closure_vs_forgetting, empirical, 'Distinguishing between active suppression and passive amnesia in maintaining market dominance.').

omega_variable(
    recoverability_of_alternatives,
    'Are the ''lapsed alternatives'' genuinely recoverable and viable in contemporary contexts, or are they merely historical curiosities?',
    'Pilot programs, policy experiments, and comparative studies of contemporary non-market economic models in different jurisdictions.',
    'If alternatives are viable, the ''accessibility_collapse'' metric is overstated, and the constraint''s effective extraction (opportunity cost) is higher. If not, the constraint is closer to a Mountain of practical limits.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(recoverability_of_alternatives, empirical, 'Assessing the contemporary viability of historically forgotten economic alternatives.').

omega_variable(
    kernel_framing_underdetermination,
    'Is the ''market_as_natural_default'' kernel best framed as a historical forgetting (this reading), an actively maintained beneficiary structure (sibling: beneficiary_maintained_reading), or a hybrid of both (sibling: hybrid_amnesia_reading)?',
    'A meta-analysis of historical and sociological research, combined with a comparative analysis of policy outcomes in different national contexts, to identify the dominant mechanism of market naturalization.',
    'Adopting the ''beneficiary_maintained_reading'' would significantly increase extractiveness and suppression, likely reclassifying to Snare or Tangled Rope. The ''hybrid_amnesia_reading'' would suggest a more complex, time-varying classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_framing_underdetermination, conceptual, 'Ambiguity in the primary mechanism by which market dominance is naturalized.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(market_as_natural_default__lapsed_alternative_reading, 1980, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mark_tr_t1980, market_as_natural_default__lapsed_alternative_reading, theater_ratio, 1980, 0.3).
narrative_ontology:measurement(mark_tr_t1988, market_as_natural_default__lapsed_alternative_reading, theater_ratio, 1988, 0.35).
narrative_ontology:measurement(mark_tr_t1996, market_as_natural_default__lapsed_alternative_reading, theater_ratio, 1996, 0.38).
narrative_ontology:measurement(mark_tr_t2004, market_as_natural_default__lapsed_alternative_reading, theater_ratio, 2004, 0.4).
narrative_ontology:measurement(mark_tr_t2012, market_as_natural_default__lapsed_alternative_reading, theater_ratio, 2012, 0.4).
narrative_ontology:measurement(mark_tr_t2020, market_as_natural_default__lapsed_alternative_reading, theater_ratio, 2020, 0.4).

% Extraction over time
narrative_ontology:measurement(mark_be_t1980, market_as_natural_default__lapsed_alternative_reading, base_extractiveness, 1980, 0.08).
narrative_ontology:measurement(mark_be_t1988, market_as_natural_default__lapsed_alternative_reading, base_extractiveness, 1988, 0.1).
narrative_ontology:measurement(mark_be_t1996, market_as_natural_default__lapsed_alternative_reading, base_extractiveness, 1996, 0.11).
narrative_ontology:measurement(mark_be_t2004, market_as_natural_default__lapsed_alternative_reading, base_extractiveness, 2004, 0.12).
narrative_ontology:measurement(mark_be_t2012, market_as_natural_default__lapsed_alternative_reading, base_extractiveness, 2012, 0.12).
narrative_ontology:measurement(mark_be_t2020, market_as_natural_default__lapsed_alternative_reading, base_extractiveness, 2020, 0.12).

% Suppression requirement over time
narrative_ontology:measurement(mark_su_t1980, market_as_natural_default__lapsed_alternative_reading, suppression_requirement, 1980, 0.18).
narrative_ontology:measurement(mark_su_t1988, market_as_natural_default__lapsed_alternative_reading, suppression_requirement, 1988, 0.19).
narrative_ontology:measurement(mark_su_t1996, market_as_natural_default__lapsed_alternative_reading, suppression_requirement, 1996, 0.2).
narrative_ontology:measurement(mark_su_t2004, market_as_natural_default__lapsed_alternative_reading, suppression_requirement, 2004, 0.2).
narrative_ontology:measurement(mark_su_t2012, market_as_natural_default__lapsed_alternative_reading, suppression_requirement, 2012, 0.2).
narrative_ontology:measurement(mark_su_t2020, market_as_natural_default__lapsed_alternative_reading, suppression_requirement, 2020, 0.2).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(market_as_natural_default__lapsed_alternative_reading, identity_coordination).
narrative_ontology:affects_constraint(market_as_natural_default__lapsed_alternative_reading, market_as_natural_default__beneficiary_maintained_reading).
narrative_ontology:affects_constraint(market_as_natural_default__lapsed_alternative_reading, market_as_natural_default__hybrid_amnesia_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'market_as_natural_default' kernel. This 'lapsed_alternative_reading' focuses on historical forgetting as the primary mechanism, distinct from active maintenance by beneficiaries or a hybrid of both.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
