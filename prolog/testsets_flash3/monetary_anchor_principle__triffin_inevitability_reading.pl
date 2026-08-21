% ============================================================================
% CONSTRAINT STORY: monetary_anchor_principle__triffin_inevitability_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_monetary_anchor_principle__triffin_inevitability_reading, []).

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
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: monetary_anchor_principle__triffin_inevitability_reading
 *   human_readable: Triffin Dilemma: Inevitable Gold Standard Collapse
 *   domain: monetary_economics/political_economy/international_finance
 *
 * SUMMARY:
 *   This constraint represents the 'triffin_inevitability_reading' of the
 *   'monetary_anchor_principle' kernel. It posits that the collapse of the
 *   Bretton Woods gold-dollar standard was a structural inevitability due to
 *   the Triffin dilemma: a reserve currency issuer (the US) must run deficits
 *   to supply global liquidity, which eventually exhausts its gold reserves,
 *   forcing abandonment of convertibility. This reading frames the transition
 *   as a 'mountain' – a physical/logical impossibility rather than a policy
 *   choice. The Bretton Woods institutional framework is identified as a
 *   victim, as it was the system that collapsed under this structural
 *   pressure.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(monetary_anchor_principle__triffin_inevitability_reading, 0.05).
domain_priors:suppression_score(monetary_anchor_principle__triffin_inevitability_reading, 0.95).
domain_priors:theater_ratio(monetary_anchor_principle__triffin_inevitability_reading, 0.0).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(monetary_anchor_principle__triffin_inevitability_reading, extractiveness, 0.05).
narrative_ontology:constraint_metric(monetary_anchor_principle__triffin_inevitability_reading, suppression_requirement, 0.95).
narrative_ontology:constraint_metric(monetary_anchor_principle__triffin_inevitability_reading, theater_ratio, 0.0).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(monetary_anchor_principle__triffin_inevitability_reading, accessibility_collapse, 0.98).
narrative_ontology:constraint_metric(monetary_anchor_principle__triffin_inevitability_reading, resistance, 0.02).

% --- Constraint claim ---
narrative_ontology:constraint_claim(monetary_anchor_principle__triffin_inevitability_reading, mountain).
narrative_ontology:human_readable(monetary_anchor_principle__triffin_inevitability_reading, "Triffin Dilemma: Inevitable Gold Standard Collapse").
narrative_ontology:topic_domain(monetary_anchor_principle__triffin_inevitability_reading, "monetary_economics/political_economy/international_finance").

domain_priors:emerges_naturally(monetary_anchor_principle__triffin_inevitability_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(monetary_anchor_principle__triffin_inevitability_reading, '63c1a755-5401-4a25-97e2-7a682eb06640').
narrative_ontology:cs_kernel_codification('63c1a755-5401-4a25-97e2-7a682eb06640', formalized).
narrative_ontology:cs_authority_grounding('63c1a755-5401-4a25-97e2-7a682eb06640', self_enforcing).
narrative_ontology:cs_reading_relation('63c1a755-5401-4a25-97e2-7a682eb06640', monetary_anchor_principle__punctuated_swap_reading, forecloses).
narrative_ontology:cs_reading_relation('63c1a755-5401-4a25-97e2-7a682eb06640', monetary_anchor_principle__overdetermined_composite_reading, forecloses).
narrative_ontology:cs_axiom('63c1a755-5401-4a25-97e2-7a682eb06640', foundational, structural_contradiction_inevitability).
narrative_ontology:cs_axiom_status(structural_contradiction_inevitability, holdable).
narrative_ontology:cs_axiom_grounding('63c1a755-5401-4a25-97e2-7a682eb06640', structural_contradiction_inevitability, empirically_contingent).
narrative_ontology:cs_reference_frame('63c1a755-5401-4a25-97e2-7a682eb06640', gold_standard_structural_logic).
narrative_ontology:cs_drift_state('63c1a755-5401-4a25-97e2-7a682eb06640', post_bretton_woods_collapse, gap(axiom_overriding, severe, true)).
narrative_ontology:cs_created_at('63c1a755-5401-4a25-97e2-7a682eb06640', '').
narrative_ontology:cs_kernel_id(monetary_anchor_principle__triffin_inevitability_reading, monetary_anchor_principle).

% --- Structural relationships ---
narrative_ontology:constraint_victim(monetary_anchor_principle__triffin_inevitability_reading, bretton_woods_institutional_framework).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The overarching system of fixed exchange rates and gold-dollar convertibility that was structurally unable to resolve the inherent contradiction of the Triffin dilemma. It bore the costs of its own internal inconsistency, leading to its collapse.
narrative_ontology:constraint_stakeholder(monetary_anchor_principle__triffin_inevitability_reading, bretton_woods_institutional_framework, payer,
    institutional, generational, trapped, global).
narrative_ontology:stakeholder_non_agent(monetary_anchor_principle__triffin_inevitability_reading, bretton_woods_institutional_framework).

% As the issuer of the reserve currency, the US Treasury was forced to run deficits to supply global liquidity, which depleted its gold reserves. While it made policy choices within the system, the Triffin dilemma presented a fundamental structural limit to its options.
narrative_ontology:constraint_stakeholder(monetary_anchor_principle__triffin_inevitability_reading, united_states_treasury, agenda_setter,
    institutional, biographical, constrained, national).

% The IMF was designed to oversee the Bretton Woods system. From this reading, it observed the structural pressures of the Triffin dilemma but was ultimately powerless to resolve the fundamental contradiction within the existing framework.
narrative_ontology:constraint_stakeholder(monetary_anchor_principle__triffin_inevitability_reading, international_monetary_fund, observer,
    institutional, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The Bretton Woods system aimed to coordinate global monetary stability by pegging currencies to the US dollar, which was in turn convertible to gold, providing a stable anchor for international trade and finance.
% TRANSFER_FUNCTION: The system implicitly transferred the burden of providing global liquidity to the reserve currency issuer (US), which in turn led to a transfer of gold reserves out of the US as deficits accumulated.
% ABSENT_VOICES: Advocates for a truly international reserve asset (e.g., Keynes's Bancor proposal) or a floating exchange rate system were largely excluded from the initial design and subsequent debates, as their proposals would have fundamentally altered the gold-dollar anchor.
% DISAPPEARANCE_RATIONALE: If the Triffin dilemma (as a structural inevitability) had not existed, the Bretton Woods system might have persisted longer or evolved differently, fundamentally altering the trajectory of global monetary policy and international finance. Its 'disappearance' would imply a different structural reality.
% FOUNDING_PROBLEM: The Bretton Woods system was established to prevent the monetary instability and competitive devaluations that characterized the interwar period, aiming for global economic stability and growth.
% FOUNDING_PROBLEM_CORROBORATION: While the problem of monetary instability is still live, the specific problem of competitive devaluations under a gold-exchange standard is largely considered 'dead' by most economists and policymakers, as the system itself collapsed. Independent economic historians and monetary theorists corroborate that the original problem was addressed, but the solution created new, ultimately fatal, structural issues.
narrative_ontology:disappearance_verdict(monetary_anchor_principle__triffin_inevitability_reading, world_rearranges).
narrative_ontology:founding_problem_status(monetary_anchor_principle__triffin_inevitability_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(monetary_anchor_principle__triffin_inevitability_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(monetary_anchor_principle__triffin_inevitability_reading, 'none', 1).
narrative_ontology:epsilon_provenance(monetary_anchor_principle__triffin_inevitability_reading, 0.05, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(monetary_anchor_principle__triffin_inevitability_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(monetary_anchor_principle__triffin_inevitability_reading, ExtMetricName, E),
    domain_priors:suppression_score(monetary_anchor_principle__triffin_inevitability_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(monetary_anchor_principle__triffin_inevitability_reading),
    narrative_ontology:constraint_metric(monetary_anchor_principle__triffin_inevitability_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(monetary_anchor_principle__triffin_inevitability_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(monetary_anchor_principle__triffin_inevitability_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is very low (0.05) because the dilemma is presented as a structural necessity, not a mechanism for rent extraction by any specific party. Suppression is very high (0.95) because the structural contradiction leaves virtually no policy alternatives within the gold standard framework. Theater ratio is zero as there's no performative maintenance of a 'natural law'. Accessibility collapse is high (0.98) because the dilemma implies a complete collapse of viable alternatives for maintaining the system. Resistance is low (0.02) because, from this perspective, the system's collapse was a mathematical certainty, not something that could be resisted by actors.
 *
 * PERSPECTIVAL GAP:
 *   Other readings of the 'monetary_anchor_principle' kernel, such as the 'punctuated_swap_reading' or 'overdetermined_composite_reading', would emphasize policy choices, political factors, or multiple contributing causes. These readings would likely classify the constraint differently (e.g., 'tangled_rope' or 'snare') and identify specific beneficiaries and agenda-setters, creating a significant perspectival gap with this 'mountain' reading.
 *
 * DIRECTIONALITY LOGIC:
 *   There are no direct beneficiaries of a structural inevitability that leads to system collapse. The Bretton Woods institutional framework is the victim, as it was the structure that could not sustain itself under the dilemma's pressure. From this 'mountain' perspective, all actors are subject to the same underlying structural forces, leading to a high directionality for the system itself, but not for specific agents extracting from it.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    triffin_dilemma_kernel_reading,
    'Is the Triffin dilemma a sufficient condition for the collapse of a reserve currency under a gold standard, or merely a contributing factor?',
    'Counterfactual historical analysis: identify periods where the dilemma was present but other factors (e.g., political will, alternative policy choices) averted collapse, or where collapse occurred without the dilemma being the primary driver.',
    'If sufficient, this reading''s ''mountain'' classification is robust. If merely contributing, the classification shifts towards ''tangled_rope'' or ''snare'' as policy choices and institutional power become more salient than structural inevitability.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(triffin_dilemma_kernel_reading, conceptual, 'This constraint is the ''triffin_inevitability_reading'' of the ''monetary_anchor_principle'' kernel. Sibling readings (''punctuated_swap_reading'', ''overdetermined_composite_reading'') emphasize policy choice or multiple causes, which would alter the classification from ''mountain''.').

omega_variable(
    natural_law_vs_constructed_ambiguity,
    'Is the Triffin dilemma a genuine natural law of monetary systems, or a constructed constraint that benefits identifiable agents by framing policy choices as inevitable?',
    'Analysis of historical narratives and policy advocacy: identify who benefited from framing the gold standard''s collapse as inevitable, and whether alternative, non-dilemma-driven solutions were suppressed.',
    'If genuinely natural law, the ''mountain'' classification holds. If constructed, the constraint reclassifies as a ''tangled_rope'' or ''snare'', with beneficiaries identified as those who profited from the ''inevitability'' narrative.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_ambiguity, conceptual, 'Ambiguity between structural inevitability and policy framing for the Triffin dilemma.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(monetary_anchor_principle__triffin_inevitability_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mone_tr_t0, monetary_anchor_principle__triffin_inevitability_reading, theater_ratio, 0, 0.0).
narrative_ontology:measurement(mone_tr_t10, monetary_anchor_principle__triffin_inevitability_reading, theater_ratio, 10, 0.0).
narrative_ontology:measurement(mone_tr_t20, monetary_anchor_principle__triffin_inevitability_reading, theater_ratio, 20, 0.0).
narrative_ontology:measurement(mone_tr_t30, monetary_anchor_principle__triffin_inevitability_reading, theater_ratio, 30, 0.0).

% Extraction over time
narrative_ontology:measurement(mone_be_t0, monetary_anchor_principle__triffin_inevitability_reading, base_extractiveness, 0, 0.05).
narrative_ontology:measurement(mone_be_t10, monetary_anchor_principle__triffin_inevitability_reading, base_extractiveness, 10, 0.05).
narrative_ontology:measurement(mone_be_t20, monetary_anchor_principle__triffin_inevitability_reading, base_extractiveness, 20, 0.05).
narrative_ontology:measurement(mone_be_t30, monetary_anchor_principle__triffin_inevitability_reading, base_extractiveness, 30, 0.05).

% Suppression requirement over time
narrative_ontology:measurement(mone_su_t0, monetary_anchor_principle__triffin_inevitability_reading, suppression_requirement, 0, 0.95).
narrative_ontology:measurement(mone_su_t10, monetary_anchor_principle__triffin_inevitability_reading, suppression_requirement, 10, 0.95).
narrative_ontology:measurement(mone_su_t20, monetary_anchor_principle__triffin_inevitability_reading, suppression_requirement, 20, 0.95).
narrative_ontology:measurement(mone_su_t30, monetary_anchor_principle__triffin_inevitability_reading, suppression_requirement, 30, 0.95).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(monetary_anchor_principle__triffin_inevitability_reading, global_infrastructure).
narrative_ontology:affects_constraint(monetary_anchor_principle__triffin_inevitability_reading, monetary_anchor_principle__punctuated_swap_reading).
narrative_ontology:affects_constraint(monetary_anchor_principle__triffin_inevitability_reading, monetary_anchor_principle__overdetermined_composite_reading).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
