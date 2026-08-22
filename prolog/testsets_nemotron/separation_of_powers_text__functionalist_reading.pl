% ============================================================================
% CONSTRAINT STORY: separation_of_powers_text__functionalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_separation_of_powers_text__functionalist_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: separation_of_powers_text__functionalist_reading
 *   human_readable: Functionalist Separation of Powers: Flexible Framework with Overlapping Authority and Intelligible Principle Delegation
 *   domain: constitutional_law/administrative_law/political_theory
 *
 * SUMMARY:
 *   The functionalist reading of separation of powers treats the
 *   Constitution's allocation of legislative, executive, and judicial
 *   authority as a flexible framework where overlapping functions and
 *   delegated authority are legitimate so long as Congress provides an
 *   'intelligible principle' to guide agency action. This reading legitimated
 *   the modern administrative state from the New Deal onward, enabling
 *   Congress to delegate broad rulemaking authority to expert agencies while
 *   retaining oversight. The constraint is the operational constitutional
 *   doctrine that makes the regulatory state legally coherent. It is claimed
 *   as a rope — a coordination mechanism solving the genuine
 *   collective-action problem of governing complexity — and its metrics
 *   reflect low extractiveness and suppression, with moderate theater
 *   reflecting the performative maintenance of 'intelligible principle' as a
 *   meaningful limit.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(separation_of_powers_text__functionalist_reading, 0.28).
domain_priors:suppression_score(separation_of_powers_text__functionalist_reading, 0.15).
domain_priors:theater_ratio(separation_of_powers_text__functionalist_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(separation_of_powers_text__functionalist_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(separation_of_powers_text__functionalist_reading, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(separation_of_powers_text__functionalist_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(separation_of_powers_text__functionalist_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(separation_of_powers_text__functionalist_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(separation_of_powers_text__functionalist_reading, rope).
narrative_ontology:human_readable(separation_of_powers_text__functionalist_reading, "Functionalist Separation of Powers: Flexible Framework with Overlapping Authority and Intelligible Principle Delegation").
narrative_ontology:topic_domain(separation_of_powers_text__functionalist_reading, "constitutional_law/administrative_law/political_theory").

domain_priors:requires_active_enforcement(separation_of_powers_text__functionalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(separation_of_powers_text__functionalist_reading, 'e8b7fa41-beca-4319-a51d-103323a0b8e3').
narrative_ontology:cs_kernel_codification('e8b7fa41-beca-4319-a51d-103323a0b8e3', fixed_text).
narrative_ontology:cs_authority_grounding('e8b7fa41-beca-4319-a51d-103323a0b8e3', lineage).
narrative_ontology:cs_interpretation_layer_present('e8b7fa41-beca-4319-a51d-103323a0b8e3').
narrative_ontology:cs_reading_relation('e8b7fa41-beca-4319-a51d-103323a0b8e3', separation_of_powers_text__formalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('e8b7fa41-beca-4319-a51d-103323a0b8e3', separation_of_powers_text__unitary_executive_reading, influences).
narrative_ontology:cs_axiom('e8b7fa41-beca-4319-a51d-103323a0b8e3', foundational, intelligible_principle_suffices_for_delegation).
narrative_ontology:cs_axiom_status(intelligible_principle_suffices_for_delegation, holdable).
narrative_ontology:cs_axiom_grounding('e8b7fa41-beca-4319-a51d-103323a0b8e3', intelligible_principle_suffices_for_delegation, conventional).
narrative_ontology:cs_axiom('e8b7fa41-beca-4319-a51d-103323a0b8e3', foundational, overlapping_functions_not_violations).
narrative_ontology:cs_axiom_status(overlapping_functions_not_violations, holdable).
narrative_ontology:cs_axiom_grounding('e8b7fa41-beca-4319-a51d-103323a0b8e3', overlapping_functions_not_violations, conventional).
narrative_ontology:cs_axiom('e8b7fa41-beca-4319-a51d-103323a0b8e3', secondary, regulatory_state_constitutionally_legitimate).
narrative_ontology:cs_axiom_status(regulatory_state_constitutionally_legitimate, holdable).
narrative_ontology:cs_axiom_grounding('e8b7fa41-beca-4319-a51d-103323a0b8e3', regulatory_state_constitutionally_legitimate, instrumental).
narrative_ontology:cs_reference_frame('e8b7fa41-beca-4319-a51d-103323a0b8e3', new_deal_constitutional_settlement).
narrative_ontology:cs_drift_state('e8b7fa41-beca-4319-a51d-103323a0b8e3', contemporary_major_questions_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('e8b7fa41-beca-4319-a51d-103323a0b8e3', '').
narrative_ontology:cs_kernel_id(separation_of_powers_text__functionalist_reading, separation_of_powers_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(separation_of_powers_text__functionalist_reading, administrative_agencies).
narrative_ontology:constraint_beneficiary(separation_of_powers_text__functionalist_reading, congress).
narrative_ontology:constraint_beneficiary(separation_of_powers_text__functionalist_reading, executive_branch).
narrative_ontology:constraint_beneficiary(separation_of_powers_text__functionalist_reading, regulated_public).
narrative_ontology:constraint_victim(separation_of_powers_text__functionalist_reading, non_delegation_doctrine_proponents).
narrative_ontology:constraint_vindicates(separation_of_powers_text__functionalist_reading, chevon_deference_doctrine).
narrative_ontology:constraint_vindicates(separation_of_powers_text__functionalist_reading, intelligible_principle_standard).
narrative_ontology:constraint_vindicates(separation_of_powers_text__functionalist_reading, regulatory_state_legitimacy).
narrative_ontology:constraint_vindicates(separation_of_powers_text__functionalist_reading, flexible_separation_of_powers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Receive delegated authority from Congress to implement regulatory programs; exercise combined rulemaking, adjudication, and enforcement functions under Chevron/Skidmore deference frameworks. Their legitimacy and operational scope depend on the functionalist reading's validation of intelligible principle delegation.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__functionalist_reading, administrative_agencies, beneficiary,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(separation_of_powers_text__functionalist_reading, administrative_agencies, agenda_setter).

% Delegates legislative authority to agencies via statutes with intelligible principles; avoids policy-detail paralysis while retaining oversight tools (appropriations, hearings, CRA). Gains political credit for action without micromanagement costs.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__functionalist_reading, congress, beneficiary,
    institutional, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(separation_of_powers_text__functionalist_reading, congress, agenda_setter).

% Controls agency leadership through appointments and removal power; directs regulatory priorities via OIRA review and executive orders. Shares functional executive authority with independent agencies — a feature, not a bug, under functionalism.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__functionalist_reading, executive_branch, beneficiary,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(separation_of_powers_text__functionalist_reading, executive_branch, agenda_setter).

% Receives expert, technically grounded regulation addressing complex externalities (environment, finance, health, safety) that Congress cannot legislate in detail. Bears compliance costs but gains predictability and specialized adjudication.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__functionalist_reading, regulated_public, beneficiary,
    organized, biographical, constrained, national).

% Argue that Congress cannot constitutionally delegate legislative power; seek judicial revival of non-delegation doctrine to invalidate broad statutory grants. Their intellectual project is structurally excluded from operational constitutional law while the functionalist reading prevails — they pay the cost of a framework that treats their core claim as settled against them.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__functionalist_reading, non_delegation_doctrine_proponents, payer,
    organized, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(separation_of_powers_text__functionalist_reading, non_delegation_doctrine_proponents, excluded).

% Adjudicate delegation challenges; apply Chevron/Skidmore/Mead deference doctrines; police the intelligible principle boundary. Their institutional role is to manage the coordination, not to capture its gains — they are the constraint's maintenance apparatus.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__functionalist_reading, federal_courts, agenda_setter,
    institutional, generational, arbitrage, national).

% Produce the theoretical architecture justifying flexible separation of powers; their work legitimates the operational framework but they collect no institutional rents from it.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__functionalist_reading, constitutional_scholars_functionalist, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the problem of governing a complex modern state where Congress lacks the time, expertise, and granular information to legislate detailed regulatory rules for every domain. Enables legislative authority to flow to specialized bodies while preserving intelligible principle accountability and inter-branch checks.
% TRANSFER_FUNCTION: Moves legislative detail-authority from Congress to agencies (rulemaking/adjudication), and moves implementation discretion from President to independent agencies, in exchange for: expert regulation for the public, political cover for Congress, and policy capacity for the Executive. The transfer is bidirectional and recursive — oversight flows back.
% ABSENT_VOICES: Originalist judges and scholars who would impose a strict non-delegation barrier; state governments seeking to reclaim regulatory space preempted by federal agencies; small regulated entities that lack resources to navigate complex administrative processes. They are excluded from the functionalist consensus that treats delegation as presumptively valid.
% DISAPPEARANCE_RATIONALE: If the functionalist framework vanished overnight, the administrative state would face existential legal challenge: every agency rulemaking without explicit statutory detail would be vulnerable to non-delegation invalidation. Congress would need to legislate at impossible granularity or regulatory programs would collapse. The regulatory state as we know it would reorganize or paralyze.
% FOUNDING_PROBLEM: The founding problem was governing a continental republic with complex interstate commerce, technological change, and externalities that the Framers' enumeration of powers could not anticipate in detail. The functionalist reading emerged from the New Deal Court's recognition that rigid separation of powers would paralyze governance — the intelligible principle standard (J.W. Hampton Jr. & Co. v. United States, 1928) was the coordination mechanism.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated by: the sustained operation of the administrative state across 90+ years of changing Court compositions; the failure of non-delegation revival attempts (Gundy v. United States, 2019; West Virginia v. EPA, 2022, used major questions doctrine not non-delegation); congressional practice of routine broad delegation with oversight. Outside the beneficiary set: originalist scholars (e.g., Gorsuch, Thomas) explicitly contest that the founding problem justifies the modern scope — their dissent is the counter-corroboration.
narrative_ontology:disappearance_verdict(separation_of_powers_text__functionalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(separation_of_powers_text__functionalist_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(separation_of_powers_text__functionalist_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(separation_of_powers_text__functionalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(separation_of_powers_text__functionalist_reading, 0.28, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(separation_of_powers_text__functionalist_reading_tests).
:- end_tests(separation_of_powers_text__functionalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.28) because the arrangement's primary beneficiaries (agencies, Congress, Executive, regulated public) are net gainers from the coordination; the non-delegation proponents are a minority intellectual faction whose exclusion is structural but not extractive in the rent-collection sense. Suppression is low (0.15) because alternatives (strict non-delegation, unitary executive) remain legally contestable and are actively litigated — they are not eliminated, merely outvoted in doctrine. Theater ratio (0.22) reflects that the 'intelligible principle' standard performs constraint while rarely invalidating delegations in practice — the constraint's maintenance includes rhetorical performances of limitation that do not bind.
 *
 * PERSPECTIVAL GAP:
 *   From the agency/Congress/Executive seats, this is pure coordination: a working solution to governing complexity. From the non-delegation proponent seat, it is a snare: a framework that extracts their constitutional claim's viability while presenting itself as neutral interpretation. The engine computes this divergence from the declared roles and exit options — the functionalist reading's claim of rope is tested against the structural reality of a permanently excluded interpretive community.
 *
 * DIRECTIONALITY LOGIC:
 *   Agencies, Congress, and the Executive are structural beneficiaries (d near 0.0) — they gain capacity, flexibility, and legitimacy from the framework. The regulated public benefits from expert governance but bears compliance costs (d ~ 0.4-0.5, near symmetric). Non-delegation proponents are the sole payer/excluded seat (d ~ 0.8) — their constitutional vision is structurally displaced by the prevailing doctrine. Courts sit at d ~ 0.3 (coordination managers, not extractors). The functionalist framework's persistence depends on the beneficiary coalition's institutional interest, not on coercing the excluded.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (governing complexity beyond legislative granularity) remains live — if anything, it has intensified with technological and economic change. The arrangement has not atrophied into piton or scaffold; its coordination function is active and its beneficiaries would defend it. The theater ratio's slow rise reflects doctrinal maintenance (major questions doctrine, non-delegation dicta) performing constraint without changing the operational structure — this is coordination maintenance, not mandatrophy.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    intelligible_principle_vacuity,
    'Has the ''intelligible principle'' standard become so permissive that it no longer performs any constraining function — i.e., is the coordination story now pure cover for unguided delegation?',
    'Empirical survey of non-delegation challenges since 1935: count of statutes invalidated vs. upheld; doctrinal analysis of whether any delegation has failed the standard in the modern era.',
    'If the standard is vacuous, the constraint''s extractiveness is understated — the coordination function is theatrical, and the constraint drifts toward tangled_rope (coordination story covering unconstrained legislative abdication). Theater ratio would rise toward 0.5+.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intelligible_principle_vacuity, empirical, 'Whether the intelligible principle standard retains operational bite or has become a performative mantra.').

omega_variable(
    major_questions_doctrine_shift,
    'Does the major questions doctrine (West Virginia v. EPA, 2022) represent a functionalist self-correction or a formalist incursion that changes the constraint''s structural type?',
    'Track subsequent major questions cases: if the doctrine remains a narrow clear-statement rule for ''major questions,'' functionalist coordination holds; if it expands into a general non-delegation revival, the constraint''s ε and suppression rise.',
    'If major questions doctrine expands, the functionalist reading''s coordination function is partially displaced by formalist boundary-policing — extraction shifts from agencies to regulated entities facing regulatory uncertainty, and suppression of non-delegation arguments decreases (they become winning arguments).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(major_questions_doctrine_shift, conceptual, 'Whether the major questions doctrine is a functionalist safety valve or a formalist wedge.').

omega_variable(
    kernel_reading_ambiguity,
    'Is the functionalist reading a genuine alternative constitutional interpretation, or is it a post-hoc rationalization of the administrative state''s existence?',
    'Historical analysis of whether functionalist theory preceded or followed the New Deal Court''s doctrinal shift; whether the intelligible principle standard was discovered in the text or constructed to legitimate delegation.',
    'If post-hoc rationalization, the constraint''s claimed_type (rope) is a false summit — the coordination story covers extraction from constitutional text and original understanding. Would trigger FSM evaluation if beneficiaries (agencies, Congress, Executive) are declared on a mountain-like claim of constitutional necessity.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_ambiguity, conceptual, 'Origin-status ambiguity of the functionalist reading: discovery vs. construction.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(separation_of_powers_text__functionalist_reading, 1928, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sop_functionalist_tr_t1928, separation_of_powers_text__functionalist_reading, theater_ratio, 1928, 0.08).
narrative_ontology:measurement(sop_functionalist_tr_t1945, separation_of_powers_text__functionalist_reading, theater_ratio, 1945, 0.12).
narrative_ontology:measurement(sop_functionalist_tr_t1970, separation_of_powers_text__functionalist_reading, theater_ratio, 1970, 0.18).
narrative_ontology:measurement(sop_functionalist_tr_t1984, separation_of_powers_text__functionalist_reading, theater_ratio, 1984, 0.2).
narrative_ontology:measurement(sop_functionalist_tr_t2000, separation_of_powers_text__functionalist_reading, theater_ratio, 2000, 0.21).
narrative_ontology:measurement(sop_functionalist_tr_t2024, separation_of_powers_text__functionalist_reading, theater_ratio, 2024, 0.22).

% Extraction over time
narrative_ontology:measurement(sop_functionalist_be_t1928, separation_of_powers_text__functionalist_reading, base_extractiveness, 1928, 0.12).
narrative_ontology:measurement(sop_functionalist_be_t1945, separation_of_powers_text__functionalist_reading, base_extractiveness, 1945, 0.18).
narrative_ontology:measurement(sop_functionalist_be_t1970, separation_of_powers_text__functionalist_reading, base_extractiveness, 1970, 0.22).
narrative_ontology:measurement(sop_functionalist_be_t1984, separation_of_powers_text__functionalist_reading, base_extractiveness, 1984, 0.25).
narrative_ontology:measurement(sop_functionalist_be_t2000, separation_of_powers_text__functionalist_reading, base_extractiveness, 2000, 0.27).
narrative_ontology:measurement(sop_functionalist_be_t2024, separation_of_powers_text__functionalist_reading, base_extractiveness, 2024, 0.28).

% Suppression requirement over time
narrative_ontology:measurement(sop_functionalist_su_t1928, separation_of_powers_text__functionalist_reading, suppression_requirement, 1928, 0.05).
narrative_ontology:measurement(sop_functionalist_su_t1945, separation_of_powers_text__functionalist_reading, suppression_requirement, 1945, 0.08).
narrative_ontology:measurement(sop_functionalist_su_t1970, separation_of_powers_text__functionalist_reading, suppression_requirement, 1970, 0.12).
narrative_ontology:measurement(sop_functionalist_su_t1984, separation_of_powers_text__functionalist_reading, suppression_requirement, 1984, 0.14).
narrative_ontology:measurement(sop_functionalist_su_t2000, separation_of_powers_text__functionalist_reading, suppression_requirement, 2000, 0.15).
narrative_ontology:measurement(sop_functionalist_su_t2024, separation_of_powers_text__functionalist_reading, suppression_requirement, 2024, 0.15).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(separation_of_powers_text__functionalist_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(separation_of_powers_text__functionalist_reading, 0.12).
narrative_ontology:affects_constraint(separation_of_powers_text__functionalist_reading, separation_of_powers_text__formalist_reading).
narrative_ontology:affects_constraint(separation_of_powers_text__functionalist_reading, separation_of_powers_text__unitary_executive_reading).
narrative_ontology:affects_constraint(separation_of_powers_text__functionalist_reading, chevron_deference_doctrine).
narrative_ontology:affects_constraint(separation_of_powers_text__functionalist_reading, major_questions_doctrine).
narrative_ontology:affects_constraint(separation_of_powers_text__functionalist_reading, non_delegation_doctrine_revival).

% DUAL FORMULATION NOTE:
% Part of the separation_of_powers_text constraint family (kernel_id: separation_of_powers_text). This reading (functionalist) validates delegation and the regulatory state; formalist_reading invalidates broad delegation; unitary_executive_reading invalidates independent agencies. The three readings compete for the same constitutional text — they are structurally distinct constraints with different ε, different beneficiary/victim structures, and different claimed types.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(separation_of_powers_text__functionalist_reading, organized, 0.75).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
