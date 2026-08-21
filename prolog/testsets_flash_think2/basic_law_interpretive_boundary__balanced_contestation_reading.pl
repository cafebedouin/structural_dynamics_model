% ============================================================================
% CONSTRAINT STORY: basic_law_interpretive_boundary__balanced_contestation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_basic_law_interpretive_boundary__balanced_contestation_reading, []).

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
 *   constraint_id: basic_law_interpretive_boundary__balanced_contestation_reading
 *   human_readable: Basic Law Interpretive Boundary: Balanced Contestation Reading
 *   domain: constitutional_law/comparative_constitutionalism/judicial_review_theory
 *
 * SUMMARY:
 *   This constraint describes the 'balanced contestation' reading of the
 *   basic law interpretive boundary, where both the legislature and the
 *   judiciary hold legitimate but bounded authority. It emphasizes an ongoing
 *   institutional dialogue and triadic negotiation (including the executive)
 *   over the interpretation and enforcement of constitutional principles.
 *   Neither institution is fully dominant, and the boundary is continually
 *   contested, leading to a dynamic equilibrium rather than a fixed
 *   hierarchy. This reading is one of several interpretations of the kernel
 *   'basic_law_interpretive_boundary'.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(basic_law_interpretive_boundary__balanced_contestation_reading, 0.55).
domain_priors:suppression_score(basic_law_interpretive_boundary__balanced_contestation_reading, 0.65).
domain_priors:theater_ratio(basic_law_interpretive_boundary__balanced_contestation_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(basic_law_interpretive_boundary__balanced_contestation_reading, extractiveness, 0.55).
narrative_ontology:constraint_metric(basic_law_interpretive_boundary__balanced_contestation_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(basic_law_interpretive_boundary__balanced_contestation_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(basic_law_interpretive_boundary__balanced_contestation_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(basic_law_interpretive_boundary__balanced_contestation_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(basic_law_interpretive_boundary__balanced_contestation_reading, tangled_rope).
narrative_ontology:human_readable(basic_law_interpretive_boundary__balanced_contestation_reading, "Basic Law Interpretive Boundary: Balanced Contestation Reading").
narrative_ontology:topic_domain(basic_law_interpretive_boundary__balanced_contestation_reading, "constitutional_law/comparative_constitutionalism/judicial_review_theory").

domain_priors:requires_active_enforcement(basic_law_interpretive_boundary__balanced_contestation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(basic_law_interpretive_boundary__balanced_contestation_reading, 'f7e08453-40ec-4467-92cb-ac2560d58da0').
narrative_ontology:cs_kernel_codification('f7e08453-40ec-4467-92cb-ac2560d58da0', formalized).
narrative_ontology:cs_authority_grounding('f7e08453-40ec-4467-92cb-ac2560d58da0', lineage).
narrative_ontology:cs_interpretation_layer_present('f7e08453-40ec-4467-92cb-ac2560d58da0').
narrative_ontology:cs_reading_relation('f7e08453-40ec-4467-92cb-ac2560d58da0', basic_law_interpretive_boundary__judicial_supremacy_reading, coexists_with).
narrative_ontology:cs_reading_relation('f7e08453-40ec-4467-92cb-ac2560d58da0', basic_law_interpretive_boundary__parliamentary_sovereignty_reading, coexists_with).
narrative_ontology:cs_axiom('f7e08453-40ec-4467-92cb-ac2560d58da0', foundational, inter_institutional_dialogue_is_constitutional_norm).
narrative_ontology:cs_axiom_status(inter_institutional_dialogue_is_constitutional_norm, holdable).
narrative_ontology:cs_axiom_grounding('f7e08453-40ec-4467-92cb-ac2560d58da0', inter_institutional_dialogue_is_constitutional_norm, conventional).
narrative_ontology:cs_axiom('f7e08453-40ec-4467-92cb-ac2560d58da0', foundational, legislative_sovereignty_is_bounded).
narrative_ontology:cs_axiom_status(legislative_sovereignty_is_bounded, holdable).
narrative_ontology:cs_axiom_grounding('f7e08453-40ec-4467-92cb-ac2560d58da0', legislative_sovereignty_is_bounded, deontological).
narrative_ontology:cs_reference_frame('f7e08453-40ec-4467-92cb-ac2560d58da0', constitutional_dialogue_framework).
narrative_ontology:cs_drift_state('f7e08453-40ec-4467-92cb-ac2560d58da0', contemporary_political_polarization, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('f7e08453-40ec-4467-92cb-ac2560d58da0', '').
narrative_ontology:cs_kernel_id(basic_law_interpretive_boundary__balanced_contestation_reading, basic_law_interpretive_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(basic_law_interpretive_boundary__balanced_contestation_reading, legislature).
narrative_ontology:constraint_beneficiary(basic_law_interpretive_boundary__balanced_contestation_reading, judiciary).
narrative_ontology:constraint_beneficiary(basic_law_interpretive_boundary__balanced_contestation_reading, executive).
narrative_ontology:constraint_victim(basic_law_interpretive_boundary__balanced_contestation_reading, citizens).
narrative_ontology:constraint_victim(basic_law_interpretive_boundary__balanced_contestation_reading, political_parties).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Enacts laws and holds ultimate sovereign power, but operates within a framework where its actions are subject to judicial review and international obligations. Engages in dialogue with the judiciary and executive to define interpretive boundaries.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__balanced_contestation_reading, legislature, agenda_setter,
    institutional, generational, constrained, national).

% Interprets Basic Laws and reviews legislation, asserting its authority within its jurisdictional domain. Its interpretations are part of an ongoing dialogue with the legislature and executive, not a final unilateral pronouncement.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__balanced_contestation_reading, judiciary, agenda_setter,
    institutional, generational, constrained, national).

% Implements laws and policies, navigating the interpretive boundaries established through the dialogue between the legislature and judiciary. Benefits from the overall stability of the constitutional framework, even if its policy space is contested.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__balanced_contestation_reading, executive, beneficiary,
    institutional, biographical, constrained, national).

% Live under the laws and judicial interpretations, bearing the costs of legal uncertainty, delays in policy implementation, or the outcomes of institutional negotiations. Their ability to directly influence the interpretive boundary is limited.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__balanced_contestation_reading, citizens, payer,
    powerless, biographical, constrained, national).

% Their policy agendas and legislative initiatives are subject to the interpretive boundaries and potential judicial review, leading to modifications, delays, or the need for inter-institutional negotiation. They bear the political costs of this contestation.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__balanced_contestation_reading, political_parties, payer,
    organized, immediate, constrained, national).

% Monitor the state's compliance with international obligations, which serve as external constraints on the legislature's sovereign power and influence the interpretive dialogue. They provide an external perspective on the balance of authority.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__balanced_contestation_reading, international_bodies, observer,
    institutional, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a framework for inter-institutional dialogue and mutual constraint, ensuring that neither the legislature nor the judiciary can unilaterally dominate the interpretation of basic laws, thereby maintaining constitutional stability through checks and balances.
% TRANSFER_FUNCTION: Transfers interpretive authority and policy implementation power between the legislature, judiciary, and executive, creating a dynamic system of checks and balances. It imposes costs in terms of legal uncertainty and delays in policy finalization, which are borne by citizens and political parties.
% ABSENT_VOICES: Advocates for pure parliamentary sovereignty or pure judicial supremacy are structurally marginalized; they would argue for a clearer, less contested hierarchy of authority, but their positions are not fully accommodated by this reading's emphasis on dialogue and mutual constraint.
% DISAPPEARANCE_RATIONALE: If this contested boundary and the norm of institutional dialogue vanished overnight, one institution would likely assert dominance, leading to a collapse of checks and balances, potential constitutional crisis, and a fundamental restructuring of governance, as the system would lose its mechanism for balancing powers.
% FOUNDING_PROBLEM: To establish a stable constitutional order that balances democratic legitimacy (derived from the legislature) with legal consistency and the protection of fundamental rights (upheld by the judiciary), thereby preventing the arbitrary exercise of power by any single branch.
% FOUNDING_PROBLEM_CORROBORATION: Constitutional scholars, legal practitioners, and political scientists widely attest to the ongoing challenge of balancing these powers in modern democracies, citing historical and comparative examples of constitutional crises when such balance fails. This corroboration comes from independent academic and legal analysis, not solely from the benefiting institutions.
narrative_ontology:disappearance_verdict(basic_law_interpretive_boundary__balanced_contestation_reading, world_rearranges).
narrative_ontology:founding_problem_status(basic_law_interpretive_boundary__balanced_contestation_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(basic_law_interpretive_boundary__balanced_contestation_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(basic_law_interpretive_boundary__balanced_contestation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(basic_law_interpretive_boundary__balanced_contestation_reading, 0.55, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(basic_law_interpretive_boundary__balanced_contestation_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(basic_law_interpretive_boundary__balanced_contestation_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(basic_law_interpretive_boundary__balanced_contestation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint is classified as a Tangled Rope because it serves a genuine coordination function (maintaining constitutional stability through checks and balances) but also involves asymmetric extraction. The 'contestation' itself generates costs (legal uncertainty, delays) borne by citizens and political parties, while the institutions benefit from their continued, albeit bounded, authority. Extractiveness (0.55) reflects these costs and the inherent friction of ongoing negotiation. Suppression (0.65) is present as institutions exert pressure to maintain their interpretive space and prevent unilateral action by others. Resistance (0.7) is high due to the inherent nature of 'contestation' and the active defense of institutional prerogatives. Theater ratio (0.25) is moderate, as the dialogue is largely functional, though some elements may be performative.
 *
 * PERSPECTIVAL GAP:
 *   Each institution (legislature, judiciary, executive) views its own authority as legitimate and bounded, but may perceive the actions of other branches as overreach or an attempt to shift the interpretive boundary. This reading acknowledges and frames this tension as a necessary and ongoing dialogue, rather than a defect. The engine will compute different classifications for each seat based on their structural position, reflecting this perspectival divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   The legislature, judiciary, and executive are all beneficiaries and agenda-setters, as they participate in and benefit from the stable, albeit contested, constitutional framework. Citizens and political parties are primarily payers, bearing the costs of legal uncertainty and the political friction generated by the inter-institutional dialogue. International bodies act as observers, influencing the constraints through external obligations.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    institutional_power_balance_stability,
    'Is the balance of power between the legislature and judiciary truly stable, or is one institution subtly gaining ground over time, shifting the interpretive boundary?',
    'Longitudinal analysis of legislative overrides of judicial decisions, judicial invalidations of legislation, and public opinion on institutional legitimacy over several decades.',
    'If one institution is consistently gaining ground, the constraint might reclassify towards a Snare (if extraction increases) or a Rope (if coordination improves without extraction), reflecting a de facto shift towards supremacy for one branch.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_power_balance_stability, empirical, 'Assessing the true dynamic equilibrium of institutional power.').

omega_variable(
    legitimacy_source_ambiguity,
    'Is the authority of the Basic Laws primarily grounded in democratic mandate (via the legislature) or in a higher-order legal framework (interpreted by the judiciary)?',
    'Analysis of constitutional jurisprudence and political theory debates regarding the ultimate source of constitutional legitimacy in the specific jurisdiction.',
    'A stronger emphasis on democratic mandate might shift the constraint towards a more ''parliamentary sovereignty'' type, while a stronger emphasis on higher-order law might lean towards ''judicial supremacy'', altering the perceived legitimacy of each institution''s claims.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legitimacy_source_ambiguity, conceptual, 'Ambiguity in the foundational source of constitutional legitimacy.').

omega_variable(
    suppression_mechanism_institutional,
    'Is the suppression of unilateral action by either institution primarily structural (e.g., constitutional text, procedural rules) or internalized (e.g., institutional norms, self-restraint)?',
    'Comparative analysis with jurisdictions lacking strong structural checks, or historical case studies where institutional norms were tested under pressure. If unilateralism emerges despite structural checks, internalized suppression is weak.',
    'If suppression is largely internalized, the constraint is more fragile and susceptible to political polarization; if structural, it is more robust. This impacts the long-term stability and the true ''cost'' of maintaining the balance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_institutional, empirical, 'Structural vs. internalized suppression of institutional overreach.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(basic_law_interpretive_boundary__balanced_contestation_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(basi_tr_t0, basic_law_interpretive_boundary__balanced_contestation_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(basi_tr_t10, basic_law_interpretive_boundary__balanced_contestation_reading, theater_ratio, 10, 0.24).
narrative_ontology:measurement(basi_tr_t20, basic_law_interpretive_boundary__balanced_contestation_reading, theater_ratio, 20, 0.25).
narrative_ontology:measurement(basi_tr_t30, basic_law_interpretive_boundary__balanced_contestation_reading, theater_ratio, 30, 0.26).
narrative_ontology:measurement(basi_tr_t40, basic_law_interpretive_boundary__balanced_contestation_reading, theater_ratio, 40, 0.25).
narrative_ontology:measurement(basi_tr_t50, basic_law_interpretive_boundary__balanced_contestation_reading, theater_ratio, 50, 0.25).

% Extraction over time
narrative_ontology:measurement(basi_be_t0, basic_law_interpretive_boundary__balanced_contestation_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(basi_be_t10, basic_law_interpretive_boundary__balanced_contestation_reading, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(basi_be_t20, basic_law_interpretive_boundary__balanced_contestation_reading, base_extractiveness, 20, 0.51).
narrative_ontology:measurement(basi_be_t30, basic_law_interpretive_boundary__balanced_contestation_reading, base_extractiveness, 30, 0.53).
narrative_ontology:measurement(basi_be_t40, basic_law_interpretive_boundary__balanced_contestation_reading, base_extractiveness, 40, 0.54).
narrative_ontology:measurement(basi_be_t50, basic_law_interpretive_boundary__balanced_contestation_reading, base_extractiveness, 50, 0.55).

% Suppression requirement over time
narrative_ontology:measurement(basi_su_t0, basic_law_interpretive_boundary__balanced_contestation_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(basi_su_t10, basic_law_interpretive_boundary__balanced_contestation_reading, suppression_requirement, 10, 0.58).
narrative_ontology:measurement(basi_su_t20, basic_law_interpretive_boundary__balanced_contestation_reading, suppression_requirement, 20, 0.61).
narrative_ontology:measurement(basi_su_t30, basic_law_interpretive_boundary__balanced_contestation_reading, suppression_requirement, 30, 0.63).
narrative_ontology:measurement(basi_su_t40, basic_law_interpretive_boundary__balanced_contestation_reading, suppression_requirement, 40, 0.64).
narrative_ontology:measurement(basi_su_t50, basic_law_interpretive_boundary__balanced_contestation_reading, suppression_requirement, 50, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(basic_law_interpretive_boundary__balanced_contestation_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(basic_law_interpretive_boundary__balanced_contestation_reading, basic_law_interpretive_boundary__judicial_supremacy_reading).
narrative_ontology:affects_constraint(basic_law_interpretive_boundary__balanced_contestation_reading, basic_law_interpretive_boundary__parliamentary_sovereignty_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'basic_law_interpretive_boundary' kernel. Each reading represents a distinct structural claim about the balance of power between the legislature and the judiciary, with different ε values and stakeholder dynamics. They are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
