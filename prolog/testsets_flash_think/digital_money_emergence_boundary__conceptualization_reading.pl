% ============================================================================
% CONSTRAINT STORY: digital_money_emergence_boundary__conceptualization_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_digital_money_emergence_boundary__conceptualization_reading, []).

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
 *   constraint_id: digital_money_emergence_boundary__conceptualization_reading
 *   human_readable: Emergence Boundary of Digital Money (Conceptualization Reading)
 *   domain: monetary_economics/financial_history/technology_governance
 *
 * SUMMARY:
 *   This constraint defines the conceptual emergence boundary of digital
 *   money, marking the point when it became theoretically thinkable and
 *   formally defined, rather than merely a speculative idea. This reading
 *   emphasizes the intellectual breakthroughs in telecommunications and
 *   cryptography (e.g., David Chaum's formalization in 1985) as the true
 *   origin point. It is one reading of the 'digital_money_emergence_boundary'
 *   kernel.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(digital_money_emergence_boundary__conceptualization_reading, 0.05).
domain_priors:suppression_score(digital_money_emergence_boundary__conceptualization_reading, 0.02).
domain_priors:theater_ratio(digital_money_emergence_boundary__conceptualization_reading, 0.01).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(digital_money_emergence_boundary__conceptualization_reading, extractiveness, 0.05).
narrative_ontology:constraint_metric(digital_money_emergence_boundary__conceptualization_reading, suppression_requirement, 0.02).
narrative_ontology:constraint_metric(digital_money_emergence_boundary__conceptualization_reading, theater_ratio, 0.01).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(digital_money_emergence_boundary__conceptualization_reading, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(digital_money_emergence_boundary__conceptualization_reading, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(digital_money_emergence_boundary__conceptualization_reading, mountain).
narrative_ontology:human_readable(digital_money_emergence_boundary__conceptualization_reading, "Emergence Boundary of Digital Money (Conceptualization Reading)").
narrative_ontology:topic_domain(digital_money_emergence_boundary__conceptualization_reading, "monetary_economics/financial_history/technology_governance").

domain_priors:emerges_naturally(digital_money_emergence_boundary__conceptualization_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(digital_money_emergence_boundary__conceptualization_reading, 'b8eebaf2-6ad5-433d-9e8a-f94ba7d290b0').
narrative_ontology:cs_kernel_codification('b8eebaf2-6ad5-433d-9e8a-f94ba7d290b0', formalized).
narrative_ontology:cs_authority_grounding('b8eebaf2-6ad5-433d-9e8a-f94ba7d290b0', expertise).
narrative_ontology:cs_reading_relation('b8eebaf2-6ad5-433d-9e8a-f94ba7d290b0', digital_money_emergence_boundary__infrastructure_reading, influences).
narrative_ontology:cs_reading_relation('b8eebaf2-6ad5-433d-9e8a-f94ba7d290b0', digital_money_emergence_boundary__consumer_holdings_reading, influences).
narrative_ontology:cs_axiom('b8eebaf2-6ad5-433d-9e8a-f94ba7d290b0', foundational, theoretical_possibility_precedes_implementation).
narrative_ontology:cs_axiom_status(theoretical_possibility_precedes_implementation, holdable).
narrative_ontology:cs_axiom_grounding('b8eebaf2-6ad5-433d-9e8a-f94ba7d290b0', theoretical_possibility_precedes_implementation, empirically_contingent).
narrative_ontology:cs_axiom('b8eebaf2-6ad5-433d-9e8a-f94ba7d290b0', foundational, formal_mathematical_definition_is_conceptual_birth).
narrative_ontology:cs_axiom_status(formal_mathematical_definition_is_conceptual_birth, holdable).
narrative_ontology:cs_axiom_grounding('b8eebaf2-6ad5-433d-9e8a-f94ba7d290b0', formal_mathematical_definition_is_conceptual_birth, conventional).
narrative_ontology:cs_reference_frame('b8eebaf2-6ad5-433d-9e8a-f94ba7d290b0', chaum_formalization_as_conceptual_origin).
narrative_ontology:cs_drift_state('b8eebaf2-6ad5-433d-9e8a-f94ba7d290b0', contemporary_historical_analysis, gap(stable, minor, true)).
narrative_ontology:cs_created_at('b8eebaf2-6ad5-433d-9e8a-f94ba7d290b0', '').
narrative_ontology:cs_kernel_id(digital_money_emergence_boundary__conceptualization_reading, digital_money_emergence_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(digital_money_emergence_boundary__conceptualization_reading, academic_researchers).
narrative_ontology:constraint_beneficiary(digital_money_emergence_boundary__conceptualization_reading, tech_historians).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from establishing intellectual priority claims and contributing to the historical record of digital money's origins. Their careers and reputations are built on defining and documenting such conceptual breakthroughs.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__conceptualization_reading, academic_researchers, beneficiary,
    powerful, biographical, analytical, global).

% Gain from the clarity this conceptual boundary provides for their narratives of technological and financial evolution. They document the key milestones and figures in the theoretical development of digital money.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__conceptualization_reading, tech_historians, beneficiary,
    moderate, biographical, analytical, global).

% Observe the historical and theoretical underpinnings of digital money to inform their understanding of its evolution and potential future forms, though they do not directly benefit from or pay for its conceptual emergence.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__conceptualization_reading, financial_regulators, observer,
    institutional, generational, analytical, national).

% Are indirectly affected by the eventual widespread adoption of digital money, but the conceptual emergence itself has no direct impact on their daily lives or financial choices. They are passive recipients of the historical outcome.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__conceptualization_reading, general_public, observer,
    powerless, biographical, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(digital_money_emergence_boundary__conceptualization_reading, diffuse).
narrative_ontology:fixing_cost_class(digital_money_emergence_boundary__conceptualization_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a shared intellectual boundary for when digital money became a theoretically coherent concept, enabling coordinated research, historical analysis, and the development of subsequent infrastructure and consumer products.
% TRANSFER_FUNCTION: Transfers intellectual priority, historical understanding, and definitional clarity from the initial conceptualizers and formalizers to the broader academic and historical record.
% ABSENT_VOICES: None directly relevant to the conceptual emergence itself. Those who might have dismissed the idea as impossible before its formalization are simply proven wrong by the historical fact of its conceptualization.
% DISAPPEARANCE_RATIONALE: The historical events and intellectual breakthroughs (telecommunications advances, Chaum's formalization) that made digital money thinkable would still have occurred. The constraint merely marks that conceptual boundary; its disappearance would not erase the history of its theoretical emergence.
% FOUNDING_PROBLEM: To define the earliest point at which digital money transitioned from science fiction to theoretical possibility, enabling a structured historical and economic analysis of its origins.
% FOUNDING_PROBLEM_CORROBORATION: Academic publications, historical records of scientific conferences, patents, and expert consensus among financial historians and computer scientists corroborate the ongoing relevance of this conceptual origin point for understanding the evolution of digital finance.
narrative_ontology:disappearance_verdict(digital_money_emergence_boundary__conceptualization_reading, world_unchanged).
narrative_ontology:founding_problem_status(digital_money_emergence_boundary__conceptualization_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(digital_money_emergence_boundary__conceptualization_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(digital_money_emergence_boundary__conceptualization_reading, 'none', 1).
narrative_ontology:epsilon_provenance(digital_money_emergence_boundary__conceptualization_reading, 0.05, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(digital_money_emergence_boundary__conceptualization_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(digital_money_emergence_boundary__conceptualization_reading, ExtMetricName, E),
    domain_priors:suppression_score(digital_money_emergence_boundary__conceptualization_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(digital_money_emergence_boundary__conceptualization_reading),
    narrative_ontology:constraint_metric(digital_money_emergence_boundary__conceptualization_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(digital_money_emergence_boundary__conceptualization_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(digital_money_emergence_boundary__conceptualization_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   As a Mountain, the constraint's metrics are very low. Extractiveness is minimal, reflecting only the intellectual effort to conceive and formalize the idea. Suppression is negligible, as ideas, once conceived, are difficult to suppress. Theater ratio is near zero, as the constraint describes a historical fact of conceptualization, not a performance. Accessibility collapse is high because once digital money is theoretically thinkable, the 'alternative' of it being impossible collapses. Resistance is low, as there is little active resistance to the historical fact of conceptualization.
 *
 * PERSPECTIVAL GAP:
 *   For a Mountain of conceptual emergence, perspectival gaps are minimal regarding the existence of the boundary itself. Different stakeholders may interpret its significance differently, but the fact of its theoretical emergence is largely stable. The primary 'gap' is between this reading and sibling readings that place emergence at different points (e.g., infrastructure or consumer adoption).
 *
 * DIRECTIONALITY LOGIC:
 *   Academic researchers and tech historians are identified as beneficiaries because they gain intellectual priority, recognition, and a clearer historical framework from the establishment of this conceptual boundary. There are no identifiable victims, as the conceptual emergence itself does not impose costs on any specific group.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint, being a Mountain of conceptual emergence, is not subject to mandatrophy in the traditional sense. Its 'mandate' is to accurately describe a historical intellectual boundary, which does not atrophy. The question of its 'resolution' is tied to the broader kernel contestation over where digital money truly 'emerged'.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    conceptual_vs_practical_emergence,
    'Does ''conceptualization'' truly mark the ''emergence'' of digital money, or is it merely a precursor to its practical emergence?',
    'Consensus among financial historians and economists on the most appropriate definition of ''emergence'' for monetary phenomena, potentially informed by the impact of conceptual breakthroughs on subsequent practical developments.',
    'If practical emergence is deemed the true boundary, this conceptualization reading would be reclassified as a ''precursor'' or ''enabling condition'' rather than the emergence itself, potentially shifting its type from Mountain to Rope (as a coordination standard for research).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(conceptual_vs_practical_emergence, conceptual, 'Ambiguity in defining ''emergence'' for a complex technological and economic phenomenon.').

omega_variable(
    beneficiary_vs_documenter_role,
    'Are academic researchers and tech historians truly ''beneficiaries'' of this conceptual boundary, or are they merely ''documenters'' whose work is enabled by it?',
    'Analysis of career incentives, funding structures, and intellectual property claims within academia and historical research related to digital money''s origins. If tangible gains (grants, priority claims, publications) are directly tied to defining this boundary, the beneficiary role is stronger.',
    'If their role is primarily ''documenter'' with diffuse benefits, the ''beneficiary'' declaration might be weakened or removed, reinforcing the Mountain classification by reducing any perceived ''extraction'' even further.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(beneficiary_vs_documenter_role, preference, 'Distinction between benefiting from a concept and merely documenting its history.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(digital_money_emergence_boundary__conceptualization_reading, 1960, 1985).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(digi_tr_t1960, digital_money_emergence_boundary__conceptualization_reading, theater_ratio, 1960, 0.01).
narrative_ontology:measurement(digi_tr_t1966, digital_money_emergence_boundary__conceptualization_reading, theater_ratio, 1966, 0.01).
narrative_ontology:measurement(digi_tr_t1972, digital_money_emergence_boundary__conceptualization_reading, theater_ratio, 1972, 0.01).
narrative_ontology:measurement(digi_tr_t1978, digital_money_emergence_boundary__conceptualization_reading, theater_ratio, 1978, 0.01).
narrative_ontology:measurement(digi_tr_t1985, digital_money_emergence_boundary__conceptualization_reading, theater_ratio, 1985, 0.01).

% Extraction over time
narrative_ontology:measurement(digi_be_t1960, digital_money_emergence_boundary__conceptualization_reading, base_extractiveness, 1960, 0.05).
narrative_ontology:measurement(digi_be_t1966, digital_money_emergence_boundary__conceptualization_reading, base_extractiveness, 1966, 0.05).
narrative_ontology:measurement(digi_be_t1972, digital_money_emergence_boundary__conceptualization_reading, base_extractiveness, 1972, 0.05).
narrative_ontology:measurement(digi_be_t1978, digital_money_emergence_boundary__conceptualization_reading, base_extractiveness, 1978, 0.05).
narrative_ontology:measurement(digi_be_t1985, digital_money_emergence_boundary__conceptualization_reading, base_extractiveness, 1985, 0.05).

% Suppression requirement over time
narrative_ontology:measurement(digi_su_t1960, digital_money_emergence_boundary__conceptualization_reading, suppression_requirement, 1960, 0.02).
narrative_ontology:measurement(digi_su_t1966, digital_money_emergence_boundary__conceptualization_reading, suppression_requirement, 1966, 0.02).
narrative_ontology:measurement(digi_su_t1972, digital_money_emergence_boundary__conceptualization_reading, suppression_requirement, 1972, 0.02).
narrative_ontology:measurement(digi_su_t1978, digital_money_emergence_boundary__conceptualization_reading, suppression_requirement, 1978, 0.02).
narrative_ontology:measurement(digi_su_t1985, digital_money_emergence_boundary__conceptualization_reading, suppression_requirement, 1985, 0.02).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(digital_money_emergence_boundary__conceptualization_reading, information_standard).
narrative_ontology:affects_constraint(digital_money_emergence_boundary__conceptualization_reading, digital_money_emergence_boundary__infrastructure_reading).
narrative_ontology:affects_constraint(digital_money_emergence_boundary__conceptualization_reading, digital_money_emergence_boundary__consumer_holdings_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'digital_money_emergence_boundary' kernel, each defining emergence at a different historical point. This 'conceptualization_reading' focuses on theoretical possibility and formalization.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
