% ============================================================================
% CONSTRAINT STORY: statute_of_anne_ip_foundation__conceptual_emergence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_statute_of_anne_ip_foundation__conceptual_emergence_reading, []).

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
 *   constraint_id: statute_of_anne_ip_foundation__conceptual_emergence_reading
 *   human_readable: Statute of Anne: Copyright as Limited Regulatory Tool for Learning (Conceptual Emergence Reading)
 *   domain: legal_history/intellectual_property/institutional_economics
 *
 * SUMMARY:
 *   This constraint story interprets the Statute of Anne (1710) as a
 *   foundational event in the conceptual emergence of intellectual property.
 *   It posits that the statute created a new conceptual space where copyright
 *   was understood as a limited regulatory tool to promote learning, rather
 *   than an inherent, perpetual property right. This reading emphasizes the
 *   shift in underlying legal philosophy and the 'thinkability' of IP as a
 *   distinct, time-limited category, benefiting public learning and authors,
 *   while challenging the claims of perpetual monopoly.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(statute_of_anne_ip_foundation__conceptual_emergence_reading, 0.15).
domain_priors:suppression_score(statute_of_anne_ip_foundation__conceptual_emergence_reading, 0.1).
domain_priors:theater_ratio(statute_of_anne_ip_foundation__conceptual_emergence_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(statute_of_anne_ip_foundation__conceptual_emergence_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(statute_of_anne_ip_foundation__conceptual_emergence_reading, suppression_requirement, 0.1).
narrative_ontology:constraint_metric(statute_of_anne_ip_foundation__conceptual_emergence_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(statute_of_anne_ip_foundation__conceptual_emergence_reading, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(statute_of_anne_ip_foundation__conceptual_emergence_reading, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(statute_of_anne_ip_foundation__conceptual_emergence_reading, mountain).
narrative_ontology:human_readable(statute_of_anne_ip_foundation__conceptual_emergence_reading, "Statute of Anne: Copyright as Limited Regulatory Tool for Learning (Conceptual Emergence Reading)").
narrative_ontology:topic_domain(statute_of_anne_ip_foundation__conceptual_emergence_reading, "legal_history/intellectual_property/institutional_economics").

domain_priors:emerges_naturally(statute_of_anne_ip_foundation__conceptual_emergence_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(statute_of_anne_ip_foundation__conceptual_emergence_reading, '3a036d92-0b1f-49c9-b5b8-d0e5fd56ca6c').
narrative_ontology:cs_kernel_codification('3a036d92-0b1f-49c9-b5b8-d0e5fd56ca6c', formalized).
narrative_ontology:cs_authority_grounding('3a036d92-0b1f-49c9-b5b8-d0e5fd56ca6c', lineage).
narrative_ontology:cs_interpretation_layer_present('3a036d92-0b1f-49c9-b5b8-d0e5fd56ca6c').
narrative_ontology:cs_reading_relation('3a036d92-0b1f-49c9-b5b8-d0e5fd56ca6c', statute_of_anne_ip_foundation__institutional_reallocation_reading, influences).
narrative_ontology:cs_reading_relation('3a036d92-0b1f-49c9-b5b8-d0e5fd56ca6c', statute_of_anne_ip_foundation__entangled_event_reading, coexists_with).
narrative_ontology:cs_axiom('3a036d92-0b1f-49c9-b5b8-d0e5fd56ca6c', foundational, copyright_as_limited_regulatory_tool).
narrative_ontology:cs_axiom_status(copyright_as_limited_regulatory_tool, holdable).
narrative_ontology:cs_axiom_grounding('3a036d92-0b1f-49c9-b5b8-d0e5fd56ca6c', copyright_as_limited_regulatory_tool, conventional).
narrative_ontology:cs_axiom('3a036d92-0b1f-49c9-b5b8-d0e5fd56ca6c', foundational, public_learning_as_primary_goal).
narrative_ontology:cs_axiom_status(public_learning_as_primary_goal, holdable).
narrative_ontology:cs_axiom_grounding('3a036d92-0b1f-49c9-b5b8-d0e5fd56ca6c', public_learning_as_primary_goal, instrumental).
narrative_ontology:cs_reference_frame('3a036d92-0b1f-49c9-b5b8-d0e5fd56ca6c', pre_anne_publisher_privilege).
narrative_ontology:cs_drift_state('3a036d92-0b1f-49c9-b5b8-d0e5fd56ca6c', post_anne_conceptual_shift, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('3a036d92-0b1f-49c9-b5b8-d0e5fd56ca6c', '').
narrative_ontology:cs_kernel_id(statute_of_anne_ip_foundation__conceptual_emergence_reading, statute_of_anne_ip_foundation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(statute_of_anne_ip_foundation__conceptual_emergence_reading, public_learning).
narrative_ontology:constraint_beneficiary(statute_of_anne_ip_foundation__conceptual_emergence_reading, authors).
narrative_ontology:constraint_victim(statute_of_anne_ip_foundation__conceptual_emergence_reading, perpetual_monopoly_claimants).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefits from the conceptual shift that frames copyright as a tool to promote learning and knowledge dissemination, rather than an absolute property right. This reading emphasizes the public domain and the limited term of protection.
narrative_ontology:constraint_stakeholder(statute_of_anne_ip_foundation__conceptual_emergence_reading, public_learning, beneficiary,
    organized, generational, analytical, national).

% Gains a recognized, albeit limited, right to their works, distinct from the publishers' prior control. This conceptual space makes their claim to ownership thinkable and legally defensible for a set term.
narrative_ontology:constraint_stakeholder(statute_of_anne_ip_foundation__conceptual_emergence_reading, authors, beneficiary,
    moderate, biographical, mobile, national).

% Loses the conceptual ground for claiming perpetual property rights over published works. This reading directly challenges their prior assumptions about ownership and control, forcing a re-evaluation of their legal position.
narrative_ontology:constraint_stakeholder(statute_of_anne_ip_foundation__conceptual_emergence_reading, perpetual_monopoly_claimants, payer,
    powerful, generational, constrained, national).

% Analyze the Statute of Anne as a foundational moment in the conceptualization of intellectual property, marking a shift from publisher privilege to authorial right and public benefit. Their work articulates and reinforces this conceptual space.
narrative_ontology:constraint_stakeholder(statute_of_anne_ip_foundation__conceptual_emergence_reading, legal_scholars, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a conceptual framework for intellectual property that balances authorial incentive with public access, enabling a more structured approach to knowledge dissemination and creative production.
% TRANSFER_FUNCTION: Conceptually transfers the idea of 'perpetual property' in books to 'limited regulatory tool for learning,' shifting the underlying justification for copyright from publisher privilege to public good.
% ABSENT_VOICES: Those who would advocate for absolute, perpetual property rights in intellectual creations, viewing any limitation as an infringement on fundamental ownership. Their conceptual framework is directly challenged by this reading.
% DISAPPEARANCE_RATIONALE: If this conceptual space vanished, the legal and philosophical foundations of modern copyright would collapse, reverting to a pre-Statute of Anne understanding where authorial rights were less defined and publisher monopolies more absolute. The entire IP landscape would need to be re-conceptualized.
% FOUNDING_PROBLEM: The problem of balancing authorial incentive with public access to knowledge, and the prior conceptual dominance of publisher-centric perpetual monopolies.
% FOUNDING_PROBLEM_CORROBORATION: Legal scholars and public interest groups corroborate that the tension between authorial rights, public access, and potential monopoly remains a live and central problem in intellectual property law, constantly re-negotiated in policy and jurisprudence.
narrative_ontology:disappearance_verdict(statute_of_anne_ip_foundation__conceptual_emergence_reading, world_rearranges).
narrative_ontology:founding_problem_status(statute_of_anne_ip_foundation__conceptual_emergence_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(statute_of_anne_ip_foundation__conceptual_emergence_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(statute_of_anne_ip_foundation__conceptual_emergence_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(statute_of_anne_ip_foundation__conceptual_emergence_reading_tests).

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(statute_of_anne_ip_foundation__conceptual_emergence_reading, ExtMetricName, E),
    domain_priors:suppression_score(statute_of_anne_ip_foundation__conceptual_emergence_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(statute_of_anne_ip_foundation__conceptual_emergence_reading),
    narrative_ontology:constraint_metric(statute_of_anne_ip_foundation__conceptual_emergence_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(statute_of_anne_ip_foundation__conceptual_emergence_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(statute_of_anne_ip_foundation__conceptual_emergence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint is claimed as a Mountain because this reading asserts a fundamental, almost 'natural law' shift in the conceptual landscape of intellectual property. Once this conceptual space emerged, it became a fixed point for subsequent legal development. Extractiveness and suppression are low because the 'extraction' is from a prior, less defined conceptual state (perpetual monopoly), and 'suppression' is of an idea, not an actor. Accessibility collapse is high because, once this conceptual framework is understood, alternatives (like perpetual copyright) become conceptually less viable within the new paradigm. Resistance is low because the conceptual shift, while contested, became a dominant framework.
 *
 * PERSPECTIVAL GAP:
 *   The conceptual shift itself is the constraint. From the perspective of public learning and authors, it's a liberating and enabling framework. From the perspective of those who previously benefited from perpetual monopoly, it's a loss of an assumed right. The 'mountain' classification reflects the enduring nature of this conceptual foundation, regardless of individual perspectives.
 *
 * DIRECTIONALITY LOGIC:
 *   Public learning and authors are beneficiaries as the new conceptual space legitimizes their claims and interests. Perpetual monopoly claimants are victims because their prior conceptual framework is undermined. Legal scholars act as observers, articulating and analyzing this conceptual shift.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    conceptual_vs_institutional_causality,
    'To what extent was the conceptual shift a direct cause of institutional changes, versus a rationalization of pre-existing institutional pressures?',
    'Detailed historical analysis of legislative debates and contemporary legal commentary, tracing the causal pathways between conceptual arguments and institutional design choices.',
    'If primarily a cause, this reading''s ''mountain'' classification is strengthened as a fundamental conceptual shift. If primarily a rationalization, the ''institutional reallocation'' reading gains weight, suggesting the conceptual change was secondary to power dynamics.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(conceptual_vs_institutional_causality, empirical, 'Distinguishing the causal primacy of conceptual change versus institutional dynamics.').

omega_variable(
    natural_law_vs_constructed_concept,
    'Is the idea of ''limited copyright for public learning'' a natural conceptual truth that the Statute of Anne merely recognized, or a socially constructed concept that the statute actively created?',
    'Philosophical analysis of intellectual property rights across different legal traditions and historical periods, examining the universality or contingency of this conceptual framing.',
    'If a natural truth, the ''mountain'' classification is robust. If a constructed concept, the ''mountain'' classification is a ''false summit,'' as the concept''s persistence depends on ongoing social and legal reinforcement, not inherent naturalness.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_concept, conceptual, 'Ambiguity between a discovered conceptual truth and a constructed legal concept.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(statute_of_anne_ip_foundation__conceptual_emergence_reading, 1710, 1710).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(statute_of_anne_ip_foundation__conceptual_emergence_reading, information_standard).
narrative_ontology:affects_constraint(statute_of_anne_ip_foundation__conceptual_emergence_reading, statute_of_anne_ip_foundation__institutional_reallocation_reading).
narrative_ontology:affects_constraint(statute_of_anne_ip_foundation__conceptual_emergence_reading, statute_of_anne_ip_foundation__entangled_event_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'Statute of Anne IP foundation' kernel. This 'conceptual emergence' reading focuses on the creation of a new conceptual space for intellectual property, distinct from institutional or entangled interpretations.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
