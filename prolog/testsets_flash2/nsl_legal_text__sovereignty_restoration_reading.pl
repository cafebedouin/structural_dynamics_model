% ============================================================================
% CONSTRAINT STORY: nsl_legal_text__sovereignty_restoration_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_nsl_legal_text__sovereignty_restoration_reading, []).

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
 *   constraint_id: nsl_legal_text__sovereignty_restoration_reading
 *   human_readable: National Security Law as Sovereign Restoration Instrument
 *   domain: constitutional_law/political_sociology/international_relations
 *
 * SUMMARY:
 *   This constraint story instantiates the 'sovereignty restoration' reading
 *   of the National Security Law (NSL). In this reading, the NSL is a
 *   legitimate instrument for the central government to reassert sovereign
 *   authority and restore constitutional order after the 2019 unrest. It is
 *   framed as a necessary security measure, not primarily as a tool for
 *   political suppression or jurisdictional capture. The extractiveness is
 *   moderate, as it targets specific political opposition rather than the
 *   general population, and the suppression is high due to the active
 *   enforcement required to maintain this order.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(nsl_legal_text__sovereignty_restoration_reading, 0.45).
domain_priors:suppression_score(nsl_legal_text__sovereignty_restoration_reading, 0.7).
domain_priors:theater_ratio(nsl_legal_text__sovereignty_restoration_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(nsl_legal_text__sovereignty_restoration_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(nsl_legal_text__sovereignty_restoration_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(nsl_legal_text__sovereignty_restoration_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(nsl_legal_text__sovereignty_restoration_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(nsl_legal_text__sovereignty_restoration_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(nsl_legal_text__sovereignty_restoration_reading, tangled_rope).
narrative_ontology:human_readable(nsl_legal_text__sovereignty_restoration_reading, "National Security Law as Sovereign Restoration Instrument").
narrative_ontology:topic_domain(nsl_legal_text__sovereignty_restoration_reading, "constitutional_law/political_sociology/international_relations").

domain_priors:requires_active_enforcement(nsl_legal_text__sovereignty_restoration_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(nsl_legal_text__sovereignty_restoration_reading, 'd04d5a9f-2be1-417f-978d-641eb66b1db6').
narrative_ontology:cs_kernel_codification('d04d5a9f-2be1-417f-978d-641eb66b1db6', formalized).
narrative_ontology:cs_authority_grounding('d04d5a9f-2be1-417f-978d-641eb66b1db6', lineage).
narrative_ontology:cs_interpretation_layer_present('d04d5a9f-2be1-417f-978d-641eb66b1db6').
narrative_ontology:cs_reading_relation('d04d5a9f-2be1-417f-978d-641eb66b1db6', nsl_legal_text__democratic_enclosure_reading, coexists_with).
narrative_ontology:cs_reading_relation('d04d5a9f-2be1-417f-978d-641eb66b1db6', nsl_legal_text__jurisdictional_capture_reading, coexists_with).
narrative_ontology:cs_axiom('d04d5a9f-2be1-417f-978d-641eb66b1db6', foundational, sovereign_right_to_self_preservation).
narrative_ontology:cs_axiom_status(sovereign_right_to_self_preservation, holdable).
narrative_ontology:cs_axiom_grounding('d04d5a9f-2be1-417f-978d-641eb66b1db6', sovereign_right_to_self_preservation, deontological).
narrative_ontology:cs_axiom('d04d5a9f-2be1-417f-978d-641eb66b1db6', foundational, constitutional_order_requires_stability).
narrative_ontology:cs_axiom_status(constitutional_order_requires_stability, holdable).
narrative_ontology:cs_axiom_grounding('d04d5a9f-2be1-417f-978d-641eb66b1db6', constitutional_order_requires_stability, conventional).
narrative_ontology:cs_reference_frame('d04d5a9f-2be1-417f-978d-641eb66b1db6', post_unrest_constitutional_order).
narrative_ontology:cs_drift_state('d04d5a9f-2be1-417f-978d-641eb66b1db6', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('d04d5a9f-2be1-417f-978d-641eb66b1db6', '').
narrative_ontology:cs_kernel_id(nsl_legal_text__sovereignty_restoration_reading, nsl_legal_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(nsl_legal_text__sovereignty_restoration_reading, central_government_authorities).
narrative_ontology:constraint_beneficiary(nsl_legal_text__sovereignty_restoration_reading, loyalist_political_establishment).
narrative_ontology:constraint_victim(nsl_legal_text__sovereignty_restoration_reading, pro_democracy_activists).
narrative_ontology:constraint_victim(nsl_legal_text__sovereignty_restoration_reading, political_opposition).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Enacts and enforces the NSL, viewing it as a necessary measure to restore stability and constitutional order after periods of unrest. Benefits from increased control over political discourse and suppression of perceived threats to national unity.
narrative_ontology:constraint_stakeholder(nsl_legal_text__sovereignty_restoration_reading, central_government_authorities, agenda_setter,
    institutional, generational, arbitrage, national).

% Supports the NSL as a means to stabilize the political environment and protect their interests. Benefits from the marginalization of opposition and a more predictable governance framework.
narrative_ontology:constraint_stakeholder(nsl_legal_text__sovereignty_restoration_reading, loyalist_political_establishment, beneficiary,
    powerful, biographical, mobile, local).

% Are targeted by the NSL's provisions, facing arrest, prosecution, and severe penalties for activities previously considered legitimate protest. Their ability to organize and express dissent is severely curtailed, leading to self-censorship or exile.
narrative_ontology:constraint_stakeholder(nsl_legal_text__sovereignty_restoration_reading, pro_democracy_activists, payer,
    powerless, immediate, trapped, local).

% Experiences a narrowing of political space and increased legal risks for expressing critical views. While not always directly targeted, the chilling effect of the NSL limits their operational capacity and public support.
narrative_ontology:constraint_stakeholder(nsl_legal_text__sovereignty_restoration_reading, political_opposition, payer,
    moderate, biographical, constrained, local).

% Monitor the implementation of the NSL and its impact on human rights and autonomy. Their analysis often highlights concerns about due process and the erosion of civil liberties, but they have limited direct power to alter the constraint.
narrative_ontology:constraint_stakeholder(nsl_legal_text__sovereignty_restoration_reading, international_observers, observer,
    institutional, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the actions of various security and administrative bodies to enforce national security, ensuring a unified approach to perceived threats and maintaining social stability.
% TRANSFER_FUNCTION: Transfers political power and legal authority from local autonomous institutions to central government control, along with the suppression of dissent and the redefinition of 'security' to include political opposition.
% ABSENT_VOICES: Legal scholars and human rights advocates who would argue for a narrower interpretation of national security that respects civil liberties are marginalized or silenced. International legal bodies, while observing, lack direct enforcement power.
% DISAPPEARANCE_RATIONALE: If the NSL vanished overnight, there would likely be an immediate resurgence of political activism and dissent, a re-evaluation of legal precedents, and a shift in the balance of power between central and local authorities. The political landscape would be significantly altered.
% FOUNDING_PROBLEM: The problem of widespread civil unrest and perceived threats to national sovereignty and stability following large-scale protests in 2019.
% FOUNDING_PROBLEM_CORROBORATION: Central government authorities and loyalist media attest that the problem of external interference and internal subversion remains live. International observers and pro-democracy groups contest this, arguing the original unrest was a response to governance issues, not a security threat, and that the NSL has exacerbated underlying tensions.
narrative_ontology:disappearance_verdict(nsl_legal_text__sovereignty_restoration_reading, world_rearranges).
narrative_ontology:founding_problem_status(nsl_legal_text__sovereignty_restoration_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(nsl_legal_text__sovereignty_restoration_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(nsl_legal_text__sovereignty_restoration_reading, 'none', 1).
narrative_ontology:epsilon_provenance(nsl_legal_text__sovereignty_restoration_reading, 0.45, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(nsl_legal_text__sovereignty_restoration_reading_tests).
:- end_tests(nsl_legal_text__sovereignty_restoration_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness is moderate (0.45) because, from this reading's perspective, the law primarily targets 'subversive' elements and not the general populace, thus limiting the scope of direct extraction. Suppression is high (0.70) due to the active enforcement mechanisms, including arrests and prosecutions, necessary to deter and punish perceived threats to national security. Theater ratio is low (0.20) as the security function is considered genuine and directly applied, with minimal performative elements beyond the necessary public messaging of deterrence.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the central government, the NSL is a legitimate and necessary act of sovereign self-defense, restoring order. From the perspective of activists and opposition, it is an act of political repression. This reading emphasizes the former, framing the law's impact on opposition as a consequence of their 'unlawful' actions rather than an inherent extractive function.
 *
 * DIRECTIONALITY LOGIC:
 *   Central government authorities and the loyalist political establishment are beneficiaries, gaining enhanced control and stability. Pro-democracy activists and the political opposition are victims, bearing the costs of curtailed freedoms and legal risks. International observers maintain an analytical distance, assessing the constraint's impact without direct participation in its operation or enforcement.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    legitimacy_of_sovereign_action,
    'Is the NSL a legitimate exercise of sovereign power to restore order, or an overreach that violates fundamental rights and autonomy?',
    'International legal rulings on human rights and self-determination, or a future plebiscite on the NSL''s provisions if political conditions allow.',
    'If deemed an overreach, the constraint''s legitimacy would collapse, reclassifying it as a Snare. If affirmed as legitimate, its Rope-like coordination function would be strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legitimacy_of_sovereign_action, conceptual, 'Ambiguity regarding the NSL''s fundamental legitimacy as a sovereign act.').

omega_variable(
    scope_of_security_threat,
    'Are the actions targeted by the NSL genuine threats to national security, or are they legitimate expressions of political dissent?',
    'Independent judicial review of NSL cases, or a shift in public discourse and international consensus on what constitutes a ''security threat'' in this context.',
    'If actions are reclassified as legitimate dissent, the NSL''s extractiveness would be seen as higher and its coordination function as a cover, pushing it towards a Snare. If affirmed as security threats, the current classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scope_of_security_threat, empirical, 'Ambiguity regarding the actual nature of the ''threats'' the NSL addresses.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(nsl_legal_text__sovereignty_restoration_reading, 0, 5).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nsl__tr_t0, nsl_legal_text__sovereignty_restoration_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(nsl__tr_t1, nsl_legal_text__sovereignty_restoration_reading, theater_ratio, 1, 0.17).
narrative_ontology:measurement(nsl__tr_t2, nsl_legal_text__sovereignty_restoration_reading, theater_ratio, 2, 0.18).
narrative_ontology:measurement(nsl__tr_t3, nsl_legal_text__sovereignty_restoration_reading, theater_ratio, 3, 0.19).
narrative_ontology:measurement(nsl__tr_t4, nsl_legal_text__sovereignty_restoration_reading, theater_ratio, 4, 0.2).
narrative_ontology:measurement(nsl__tr_t5, nsl_legal_text__sovereignty_restoration_reading, theater_ratio, 5, 0.2).

% Extraction over time
narrative_ontology:measurement(nsl__be_t0, nsl_legal_text__sovereignty_restoration_reading, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(nsl__be_t1, nsl_legal_text__sovereignty_restoration_reading, base_extractiveness, 1, 0.42).
narrative_ontology:measurement(nsl__be_t2, nsl_legal_text__sovereignty_restoration_reading, base_extractiveness, 2, 0.43).
narrative_ontology:measurement(nsl__be_t3, nsl_legal_text__sovereignty_restoration_reading, base_extractiveness, 3, 0.44).
narrative_ontology:measurement(nsl__be_t4, nsl_legal_text__sovereignty_restoration_reading, base_extractiveness, 4, 0.45).
narrative_ontology:measurement(nsl__be_t5, nsl_legal_text__sovereignty_restoration_reading, base_extractiveness, 5, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(nsl__su_t0, nsl_legal_text__sovereignty_restoration_reading, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(nsl__su_t1, nsl_legal_text__sovereignty_restoration_reading, suppression_requirement, 1, 0.64).
narrative_ontology:measurement(nsl__su_t2, nsl_legal_text__sovereignty_restoration_reading, suppression_requirement, 2, 0.67).
narrative_ontology:measurement(nsl__su_t3, nsl_legal_text__sovereignty_restoration_reading, suppression_requirement, 3, 0.69).
narrative_ontology:measurement(nsl__su_t4, nsl_legal_text__sovereignty_restoration_reading, suppression_requirement, 4, 0.7).
narrative_ontology:measurement(nsl__su_t5, nsl_legal_text__sovereignty_restoration_reading, suppression_requirement, 5, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(nsl_legal_text__sovereignty_restoration_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(nsl_legal_text__sovereignty_restoration_reading, nsl_legal_text__democratic_enclosure_reading).
narrative_ontology:affects_constraint(nsl_legal_text__sovereignty_restoration_reading, nsl_legal_text__jurisdictional_capture_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'nsl_legal_text' kernel. This 'sovereignty_restoration_reading' focuses on the NSL as a legitimate security instrument, distinct from readings that emphasize democratic enclosure or jurisdictional capture.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
