% ============================================================================
% CONSTRAINT STORY: market_naturalization__lapsed_alternative_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_market_naturalization__lapsed_alternative_reading, []).

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
    narrative_ontology:cs_created_at/2,
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: market_naturalization__lapsed_alternative_reading
 *   human_readable: Market Dominance as Lapsed Closure
 *   domain: political_economy/economic_history/institutional_analysis
 *
 * SUMMARY:
 *   This constraint describes market dominance as a 'lapsed closure' – a
 *   structural arrangement that once required active effort to establish and
 *   maintain, but now persists primarily due to inertia and the atrophy of
 *   alternatives, rather than ongoing active enforcement or concentrated
 *   beneficiary capture. It is a reading of the 'market_naturalization'
 *   kernel, focusing on the scenario where market structures become
 *   self-perpetuating without explicit maintenance.
 *
 * KEY AGENTS:
 *   - market_regulators: Agenda-setter (institutional/analytical) – could intervene but don't actively maintain.
 *   - diffuse_market_participants: Payer (powerless/constrained) – bear diffuse costs, lack leverage to change.
 *   - potential_entrants: Excluded (powerless/trapped) – locked out by inertia, not active suppression.
 *   - economic_historians: Observer (analytical/analytical) – provide critical analysis of the constraint's origins and persistence.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(market_naturalization__lapsed_alternative_reading, 0.2).
domain_priors:suppression_score(market_naturalization__lapsed_alternative_reading, 0.3).
domain_priors:theater_ratio(market_naturalization__lapsed_alternative_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(market_naturalization__lapsed_alternative_reading, extractiveness, 0.2).
narrative_ontology:constraint_metric(market_naturalization__lapsed_alternative_reading, suppression_requirement, 0.3).
narrative_ontology:constraint_metric(market_naturalization__lapsed_alternative_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(market_naturalization__lapsed_alternative_reading, accessibility_collapse, 0.85).
narrative_ontology:constraint_metric(market_naturalization__lapsed_alternative_reading, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(market_naturalization__lapsed_alternative_reading, piton).
narrative_ontology:human_readable(market_naturalization__lapsed_alternative_reading, "Market Dominance as Lapsed Closure").
narrative_ontology:topic_domain(market_naturalization__lapsed_alternative_reading, "political_economy/economic_history/institutional_analysis").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(market_naturalization__lapsed_alternative_reading, '14d83db7-e834-4d26-ae16-0edcdc715e41').
narrative_ontology:cs_kernel_codification('14d83db7-e834-4d26-ae16-0edcdc715e41', implicit).
narrative_ontology:cs_authority_grounding('14d83db7-e834-4d26-ae16-0edcdc715e41', practice).
narrative_ontology:cs_reading_relation('14d83db7-e834-4d26-ae16-0edcdc715e41', market_naturalization__beneficiary_maintained_reading, forecloses).
narrative_ontology:cs_reading_relation('14d83db7-e834-4d26-ae16-0edcdc715e41', market_naturalization__hybrid_reading, coexists_with).
narrative_ontology:cs_axiom('14d83db7-e834-4d26-ae16-0edcdc715e41', foundational, market_closure_is_inertial).
narrative_ontology:cs_axiom_status(market_closure_is_inertial, holdable).
narrative_ontology:cs_axiom_grounding('14d83db7-e834-4d26-ae16-0edcdc715e41', market_closure_is_inertial, empirically_contingent).
narrative_ontology:cs_axiom('14d83db7-e834-4d26-ae16-0edcdc715e41', foundational, absence_of_active_maintenance).
narrative_ontology:cs_axiom_status(absence_of_active_maintenance, holdable).
narrative_ontology:cs_axiom_grounding('14d83db7-e834-4d26-ae16-0edcdc715e41', absence_of_active_maintenance, empirically_contingent).
narrative_ontology:cs_created_at('14d83db7-e834-4d26-ae16-0edcdc715e41', '').
narrative_ontology:cs_kernel_id(market_naturalization__lapsed_alternative_reading, market_naturalization).

% --- Structural relationships ---
narrative_ontology:constraint_victim(market_naturalization__lapsed_alternative_reading, diffuse_market_participants).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Could theoretically intervene to break up market dominance or foster competition, but currently treat the entrenched market structure as a given, requiring no active maintenance or intervention. Their inaction allows the lapsed closure to persist.
narrative_ontology:constraint_stakeholder(market_naturalization__lapsed_alternative_reading, market_regulators, agenda_setter,
    institutional, generational, analytical, national).

% Experience the effects of reduced competition (e.g., higher prices, fewer choices, slower innovation) but lack the collective organization or individual leverage to challenge the entrenched market structure, which they perceive as 'just how things are'.
narrative_ontology:constraint_stakeholder(market_naturalization__lapsed_alternative_reading, diffuse_market_participants, payer,
    powerless, immediate, constrained, national).

% Face insurmountable barriers to entry due to the existing market dominance, which has become a de facto standard. They are effectively locked out of the market, not by active suppression, but by the sheer inertia and scale of incumbents.
narrative_ontology:constraint_stakeholder(market_naturalization__lapsed_alternative_reading, potential_entrants, excluded,
    powerless, biographical, trapped, national).

% Analyze the historical evolution of market structures, identifying periods of active closure and subsequent lapse. They provide an analytical perspective on whether current dominance is actively maintained or merely inertial.
narrative_ontology:constraint_stakeholder(market_naturalization__lapsed_alternative_reading, economic_historians, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(market_naturalization__lapsed_alternative_reading, diffuse).
narrative_ontology:fixing_cost_class(market_naturalization__lapsed_alternative_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a stable, if uncompetitive, market structure that reduces transaction costs for established players and consumers accustomed to existing offerings, by implicitly coordinating around existing dominant firms.
% TRANSFER_FUNCTION: Diffuses costs (e.g., higher prices, limited innovation, reduced choice) across market participants and consumers, without a clear, concentrated recipient of these 'gains' in the present moment.
% ABSENT_VOICES: Potential new market entrants, innovators, and consumers who would benefit from a more dynamic and competitive market are absent from the conversation, as the current structure is largely taken for granted as a 'natural' outcome.
% DISAPPEARANCE_RATIONALE: If the inertial market dominance vanished, new competitors would rapidly emerge, innovation would accelerate, and market structures would reorganize around new offerings and competitive dynamics, leading to a more dynamic and potentially more equitable distribution of value.
% FOUNDING_PROBLEM: To establish stable and predictable market operations in a nascent industry, reducing initial chaos and risk for early participants and fostering initial growth.
% FOUNDING_PROBLEM_CORROBORATION: Economic historians and antitrust scholars generally agree that the initial problems of market formation were solved decades ago, and the current dominance persists due to inertia rather than ongoing functional necessity. Incumbent firms, however, might implicitly argue for its 'naturalness' or efficiency.
narrative_ontology:disappearance_verdict(market_naturalization__lapsed_alternative_reading, world_rearranges).
narrative_ontology:founding_problem_status(market_naturalization__lapsed_alternative_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(market_naturalization__lapsed_alternative_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(market_naturalization__lapsed_alternative_reading, 'none', 1).
narrative_ontology:epsilon_provenance(market_naturalization__lapsed_alternative_reading, 0.2, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(market_naturalization__lapsed_alternative_reading_tests).
:- end_tests(market_naturalization__lapsed_alternative_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The low extractiveness (0.2) reflects that no single party actively captures significant rents from this lapsed closure; costs are diffuse. Suppression (0.3) is low because there's little active coercion, but accessibility collapse (0.85) is high because alternatives have genuinely atrophied through non-use and lack of investment. Theater ratio (0.05) is minimal, as there's little performative maintenance. Resistance (0.1) is low because there's no clear, active target for it. This profile strongly aligns with a Piton, where function has atrophied and persistence is inertial.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of diffuse market participants, the market dominance is simply 'how things are' – an unchangeable fact of their economic landscape. From an analytical observer's perspective (e.g., economic historians), it is a historical artifact, a structure that once served a purpose but now persists inertially, imposing diffuse costs without clear beneficiaries.
 *
 * DIRECTIONALITY LOGIC:
 *   Diffuse market participants are victims because they bear the costs of reduced competition. Market regulators are nominal agenda-setters who could theoretically alter the structure but do not actively benefit from its persistence. Potential entrants are excluded, trapped by the inertial barriers. Since there are no concentrated beneficiaries, the constraint's directionality is primarily towards diffuse targets.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading directly addresses mandatrophy by positing that the constraint's original mandate (to stabilize a nascent market) is now 'dead,' yet the structure persists. The low theater ratio and diffuse gain flow prevent mislabeling it as a Snare, while the high accessibility collapse and low active enforcement distinguish it from a Tangled Rope. It is a classic case of a Piton, where the mandate has atrophied, but the structure remains due to inertia.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    active_vs_inertial_persistence,
    'Is the persistence of market dominance truly inertial, or is there subtle, diffuse, or unacknowledged active maintenance by incumbent firms or related institutions?',
    'Detailed forensic economic analysis of lobbying efforts, regulatory capture, and strategic non-investment in alternatives by incumbent firms over time. Longitudinal studies of market entry and exit barriers.',
    'If active maintenance is detected, the constraint would shift towards a Tangled Rope or Snare, with higher extractiveness and identifiable beneficiaries. If purely inertial, the Piton classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(active_vs_inertial_persistence, empirical, 'Distinguishing between genuine lapse and subtle, unacknowledged active maintenance.').

omega_variable(
    extent_of_alternative_atrophy,
    'To what extent have alternatives truly atrophied through non-use, versus being actively suppressed or made economically unviable by the dominant structure?',
    'Counterfactual analysis of market dynamics under different regulatory regimes or with hypothetical interventions to foster alternatives. Surveys of potential entrants regarding perceived barriers.',
    'If alternatives are found to be actively suppressed, the suppression metric would increase, and the constraint would lean more towards a Snare. If atrophy is genuine, the high accessibility collapse is accurate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extent_of_alternative_atrophy, empirical, 'Assessing whether alternatives are truly gone or merely suppressed.').

omega_variable(
    kernel_framing_ambiguity,
    'Is market dominance best framed as a natural, inertial outcome (lapsed_alternative_reading), an actively maintained structure (beneficiary_maintained_reading), or a hybrid of both (hybrid_reading)?',
    'Consensus among economic historians and institutional analysts, supported by empirical evidence that clearly distinguishes between active and passive mechanisms of persistence.',
    'The classification of market dominance (and its policy implications) depends critically on which reading is adopted. This reading (Piton) implies different remedies than a Snare or Tangled Rope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_framing_ambiguity, conceptual, 'The fundamental ambiguity in framing the persistence mechanism of market dominance.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(market_naturalization__lapsed_alternative_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mark_tr_t0, market_naturalization__lapsed_alternative_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(mark_tr_t10, market_naturalization__lapsed_alternative_reading, theater_ratio, 10, 0.05).
narrative_ontology:measurement(mark_tr_t20, market_naturalization__lapsed_alternative_reading, theater_ratio, 20, 0.05).
narrative_ontology:measurement(mark_tr_t30, market_naturalization__lapsed_alternative_reading, theater_ratio, 30, 0.05).

% Extraction over time
narrative_ontology:measurement(mark_be_t0, market_naturalization__lapsed_alternative_reading, base_extractiveness, 0, 0.2).
narrative_ontology:measurement(mark_be_t10, market_naturalization__lapsed_alternative_reading, base_extractiveness, 10, 0.2).
narrative_ontology:measurement(mark_be_t20, market_naturalization__lapsed_alternative_reading, base_extractiveness, 20, 0.2).
narrative_ontology:measurement(mark_be_t30, market_naturalization__lapsed_alternative_reading, base_extractiveness, 30, 0.2).

% Suppression requirement over time
narrative_ontology:measurement(mark_su_t0, market_naturalization__lapsed_alternative_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(mark_su_t10, market_naturalization__lapsed_alternative_reading, suppression_requirement, 10, 0.3).
narrative_ontology:measurement(mark_su_t20, market_naturalization__lapsed_alternative_reading, suppression_requirement, 20, 0.3).
narrative_ontology:measurement(mark_su_t30, market_naturalization__lapsed_alternative_reading, suppression_requirement, 30, 0.3).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
