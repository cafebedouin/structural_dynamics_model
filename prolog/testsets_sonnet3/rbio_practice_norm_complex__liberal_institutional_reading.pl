% ============================================================================
% CONSTRAINT STORY: rbio_practice_norm_complex__liberal_institutional_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_rbio_practice_norm_complex__liberal_institutional_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
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
 *   constraint_id: rbio_practice_norm_complex__liberal_institutional_reading
 *   human_readable: Rules-Based International Order — Liberal Institutional Reading
 *   domain: international_relations/international_law/political_economy
 *
 * SUMMARY:
 *   This story instantiates the liberal institutional reading of the
 *   rules-based international order (RBIO) kernel: the norm set is universal
 *   in scope, grounded in state consent (ratification, Charter membership,
 *   customary practice), and revisable through legitimate multilateral
 *   channels (treaty amendment, evolving custom, Security Council practice,
 *   doctrines like R2P). On this reading, the well-documented pattern of
 *   selective enforcement — intervention in some atrocity situations and not
 *   others, sanctions applied asymmetrically — is a capacity and
 *   political-will problem, not evidence the underlying norms are
 *   illegitimate or captured. This is a distinct constraint from the
 *   hegemonic_extraction_reading (which treats the same selectivity as proof
 *   of a frozen extractive project) and the sovereignty_maximalist_reading
 *   (which treats humanitarian exceptions as pretexts). Per the ε-invariance
 *   principle each reading is authored as its own constraint with its own
 *   extractiveness value; this reading's ε (0.42) reflects genuine
 *   coordination function with real but bounded extraction via enforcement
 *   asymmetry, not the near-zero ε a fully vindicated reading would have, nor
 *   the high ε the extraction reading assigns to the same standing
 *   arrangement.
 *
 * KEY AGENTS:
 *   - intervening_states: Primary agenda-setters and beneficiaries (institutional/arbitrage) — invoke and apply the norms
 *   - reconstruction_and_security_contractors: Secondary beneficiaries (organized/mobile) — profit from enforcement actions
 *   - multilateral_institution_secretariats: Institutional beneficiaries and administrators (institutional/constrained) — depend on the order's perceived legitimacy
 *   - sanctioned_state_civilian_populations: Primary payers (powerless/trapped) — bear humanitarian costs of sanctions
 *   - targeted_state_governments: Payers (moderate/constrained) — face intervention or conditionality
 *   - non_p5_member_states: Excluded from Council-level agenda setting despite formal participation
 *   - international_law_scholars_multilateralist: Analytical observers documenting the formal revisability channels
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(rbio_practice_norm_complex__liberal_institutional_reading, 0.42).
domain_priors:suppression_score(rbio_practice_norm_complex__liberal_institutional_reading, 0.38).
domain_priors:theater_ratio(rbio_practice_norm_complex__liberal_institutional_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(rbio_practice_norm_complex__liberal_institutional_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(rbio_practice_norm_complex__liberal_institutional_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(rbio_practice_norm_complex__liberal_institutional_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(rbio_practice_norm_complex__liberal_institutional_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(rbio_practice_norm_complex__liberal_institutional_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(rbio_practice_norm_complex__liberal_institutional_reading, tangled_rope).
narrative_ontology:human_readable(rbio_practice_norm_complex__liberal_institutional_reading, "Rules-Based International Order — Liberal Institutional Reading").
narrative_ontology:topic_domain(rbio_practice_norm_complex__liberal_institutional_reading, "international_relations/international_law/political_economy").

domain_priors:requires_active_enforcement(rbio_practice_norm_complex__liberal_institutional_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(rbio_practice_norm_complex__liberal_institutional_reading, '23ccd0df-2c61-4692-bace-a685a817ae7e').
narrative_ontology:cs_kernel_codification('23ccd0df-2c61-4692-bace-a685a817ae7e', formalized).
narrative_ontology:cs_authority_grounding('23ccd0df-2c61-4692-bace-a685a817ae7e', lineage).
narrative_ontology:cs_interpretation_layer_present('23ccd0df-2c61-4692-bace-a685a817ae7e').
narrative_ontology:cs_reading_relation('23ccd0df-2c61-4692-bace-a685a817ae7e', rbio_practice_norm_complex__hegemonic_extraction_reading, coexists_with).
narrative_ontology:cs_reading_relation('23ccd0df-2c61-4692-bace-a685a817ae7e', rbio_practice_norm_complex__sovereignty_maximalist_reading, influences).
narrative_ontology:cs_axiom('23ccd0df-2c61-4692-bace-a685a817ae7e', foundational, enforcement_selectivity_is_capacity_limited_not_design_flaw).
narrative_ontology:cs_axiom_status(enforcement_selectivity_is_capacity_limited_not_design_flaw, holdable).
narrative_ontology:cs_axiom_grounding('23ccd0df-2c61-4692-bace-a685a817ae7e', enforcement_selectivity_is_capacity_limited_not_design_flaw, empirically_contingent).
narrative_ontology:cs_axiom('23ccd0df-2c61-4692-bace-a685a817ae7e', foundational, treaty_ratification_and_charter_membership_constitute_valid_consent).
narrative_ontology:cs_axiom_status(treaty_ratification_and_charter_membership_constitute_valid_consent, holdable).
narrative_ontology:cs_axiom_grounding('23ccd0df-2c61-4692-bace-a685a817ae7e', treaty_ratification_and_charter_membership_constitute_valid_consent, conventional).
narrative_ontology:cs_axiom('23ccd0df-2c61-4692-bace-a685a817ae7e', secondary, formal_multilateral_channels_provide_genuine_revisability).
narrative_ontology:cs_axiom_status(formal_multilateral_channels_provide_genuine_revisability, holdable).
narrative_ontology:cs_axiom_grounding('23ccd0df-2c61-4692-bace-a685a817ae7e', formal_multilateral_channels_provide_genuine_revisability, instrumental).
narrative_ontology:cs_reference_frame('23ccd0df-2c61-4692-bace-a685a817ae7e', un_charter_collective_security_framework).
narrative_ontology:cs_drift_state('23ccd0df-2c61-4692-bace-a685a817ae7e', post_cold_war_unipolar_and_multipolar_transition, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('23ccd0df-2c61-4692-bace-a685a817ae7e', '').
narrative_ontology:cs_kernel_id(rbio_practice_norm_complex__liberal_institutional_reading, rbio_practice_norm_complex).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(rbio_practice_norm_complex__liberal_institutional_reading, intervening_states).
narrative_ontology:constraint_beneficiary(rbio_practice_norm_complex__liberal_institutional_reading, reconstruction_and_security_contractors).
narrative_ontology:constraint_beneficiary(rbio_practice_norm_complex__liberal_institutional_reading, multilateral_institution_secretariats).
narrative_ontology:constraint_victim(rbio_practice_norm_complex__liberal_institutional_reading, sanctioned_state_civilian_populations).
narrative_ontology:constraint_victim(rbio_practice_norm_complex__liberal_institutional_reading, targeted_state_governments).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold Security Council seats or leading coalition roles; invoke UNSC authorization or the responsibility-to-protect exception to justify intervention or sanctions regimes. Frame enforcement gaps as resource and political-will shortfalls rather than as evidence the norm set is illegitimate. Retain the option to act outside consensus when they judge atrocity thresholds met, and to decline enforcement elsewhere without conceding the norm is void.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__liberal_institutional_reading, intervening_states, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(rbio_practice_norm_complex__liberal_institutional_reading, intervening_states, beneficiary).

% Win contracts for post-intervention reconstruction, sanctions monitoring, and security-sector assistance that only exist because the intervention or conditionality regime was triggered. Their revenue is downstream of enforcement decisions they do not make but structurally benefit from.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__liberal_institutional_reading, reconstruction_and_security_contractors, beneficiary,
    organized, biographical, mobile, global).

% Administer the UN system, sanctions committees, and treaty bodies that operationalize the norms. Their institutional relevance and budgets depend on the norm complex being treated as a going, legitimate, revisable order rather than a dead letter. They mediate disputes about selectivity by pointing to procedural channels (Security Council reform proposals, treaty review conferences) as evidence of revisability.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__liberal_institutional_reading, multilateral_institution_secretariats, beneficiary,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(rbio_practice_norm_complex__liberal_institutional_reading, multilateral_institution_secretariats, observer).

% Bear the humanitarian cost of comprehensive or sectoral sanctions imposed on their government — currency collapse, medical shortages, food insecurity — regardless of whether they had any voice in the conduct that triggered sanctions. Have no standing to petition the Security Council directly and cannot exit the jurisdiction whose conduct is being sanctioned.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__liberal_institutional_reading, sanctioned_state_civilian_populations, payer,
    powerless, biographical, trapped, national).

% Face sanctions, conditionality, or intervention threats justified under the same norm set that formally recognizes their sovereign equality. Can contest legitimacy rhetorically at the General Assembly or regional bodies but cannot block Security Council action against them and have limited capacity to build counter-coalitions at comparable speed.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__liberal_institutional_reading, targeted_state_governments, payer,
    moderate, biographical, constrained, national).

% Participate formally in General Assembly deliberation and treaty negotiation but hold no veto and limited agenda-setting power over which situations trigger Council action. Under this reading they are full norm-participants whose consent is procedurally secured through ratification and General Assembly votes, even though their substantive influence over selective enforcement is minor.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__liberal_institutional_reading, non_p5_member_states, excluded,
    moderate, generational, constrained, global).

% Study the norm complex's formal amendment procedures, treaty ratification patterns, and the growth of universal jurisdiction and R2P doctrine as evidence the system is a genuine, if imperfect, consent-based and revisable order rather than a fixed hegemonic project.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__liberal_institutional_reading, international_law_scholars_multilateralist, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared, consent-ratified normative vocabulary — sovereignty, non-intervention, exceptions for UNSC authorization and mass-atrocity prevention — that lets states coordinate expectations about when force, sanctions, or intervention are legitimate, reducing the risk of unconstrained great-power war and enabling collective responses to genuine atrocity crimes.
% TRANSFER_FUNCTION: Moves security and reconstruction resources, contracts, and normative legitimacy toward intervening states and the firms and institutions that service intervention and sanctions regimes; moves humanitarian and economic costs onto the populations and governments of sanctioned or intervened-upon states, with the size of the transfer determined by whether a Council majority (including P5 acquiescence) can be assembled.
% ABSENT_VOICES: Civilian populations under sanction have no direct procedural standing before the Security Council; their objections surface only indirectly through humanitarian-impact reporting that arrives after the sanctions regime is already operating. Smaller states without Council seats can vote in the General Assembly but cannot compel Council action or inaction.
% DISAPPEARANCE_RATIONALE: If the RBIO norm complex vanished overnight, the shared vocabulary for justifying or contesting the legitimacy of intervention and sanctions would disappear with it; states would fall back to unmediated power-balancing and ad hoc justification, multilateral institutions administering sanctions and peacekeeping would lose their mandate basis, and the contractor ecosystem built around intervention and reconstruction would lose its legal cover — a substantial rearrangement, not a null change.
% FOUNDING_PROBLEM: Post-1945 architects sought to replace unmediated great-power war and unchecked unilateral aggression with a system of collective security, sovereign equality, and negotiated exceptions (self-defense, Council authorization, later atrocity-prevention norms) that could be updated through treaty amendment and evolving custom rather than fixed permanently.
% FOUNDING_PROBLEM_CORROBORATION: Multilateral secretariats and many international law scholars attest the founding problem — preventing unconstrained great-power war and enabling collective atrocity response — remains live and that the system has genuinely evolved (R2P's emergence, expanded treaty ratification). Sanctioned-state governments and a substantial body of Global South diplomatic testimony and independent political-economy research attest the founding problem has been substantially supplanted by selective enforcement that tracks P5 interest, not universal consent — corroboration outside the beneficiary set is real but contested, not unanimous.
narrative_ontology:disappearance_verdict(rbio_practice_norm_complex__liberal_institutional_reading, world_rearranges).
narrative_ontology:founding_problem_status(rbio_practice_norm_complex__liberal_institutional_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(rbio_practice_norm_complex__liberal_institutional_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(rbio_practice_norm_complex__liberal_institutional_reading, 'none', 1).
narrative_ontology:epsilon_provenance(rbio_practice_norm_complex__liberal_institutional_reading, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(rbio_practice_norm_complex__liberal_institutional_reading_tests).
:- end_tests(rbio_practice_norm_complex__liberal_institutional_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.42) rather than low or high: the coordination function (preventing unconstrained great-power war, providing shared vocabulary for legitimate force) is genuine and substantial, so this reading does not authorize the near-zero ε a pure-coordination story would claim, but the documented pattern of enforcement asymmetry and sanctions harm to civilian populations means real extraction is occurring even under this reading's own more charitable lights — the referent is the standing arrangement as this reading's proponents themselves assess it, not the idealized fully-consensual order they endorse as an aspiration. Suppression (0.38) is moderate: P5 veto power and asymmetric capacity constrain which violations get addressed, but formal exit and voice channels (General Assembly, treaty withdrawal, regional bodies) genuinely exist and are used. Theater ratio (0.28) reflects that most Council and treaty-body activity is substantively functional, though a growing share of diplomatic activity (unenforced resolutions, symbolic condemnations) is performative. Accessibility collapse (0.40) and resistance (0.55) reflect that alternatives to the RBIO framework (regional security arrangements, non-aligned coalitions) remain genuinely available and actively pursued, distinguishing this from a mountain-like closed system.
 *
 * DIRECTIONALITY LOGIC:
 *   Intervening states and their contractor ecosystems sit near the beneficiary end: they set the agenda for when exceptions apply and capture the downstream resources and legitimacy that flow from invoking the norms. Multilateral secretariats are structural beneficiaries whose institutional survival depends on the order being seen as legitimate and functioning. Sanctioned civilian populations sit at the target end — trapped, powerless, bearing costs from decisions in which they have no voice — even under this most charitable reading, the harm is real and undeniable; what the reading disputes is whether that harm indicts the norm structure itself or merely its uneven application. Targeted governments are constrained but not fully trapped: they retain diplomatic and coalition-building options this reading treats as evidence of the system's genuine openness to contestation.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification prevents two mislabeling errors: treating the RBIO complex as pure Rope (ignoring the real, uneven extraction that even proponents should concede occurs in sanctioned populations) and treating it as pure Snare (which would require declaring the coordination function fictional — a claim this reading explicitly denies and which the hegemonic_extraction_reading, not this one, is built to test). Tangled Rope captures that a genuine collective-action problem (preventing unconstrained war, coordinating collective atrocity response) is solved by the same structure that produces asymmetric transfer toward powerful states and their contractors and away from powerless sanctioned populations — both halves must be true simultaneously for this reading to be coherent, and the schema's requirement of both beneficiaries and victims plus active enforcement is satisfied honestly rather than tuned.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    selectivity_capacity_vs_design,
    'Is the well-documented pattern of selective enforcement (intervention in some atrocity situations, inaction in others with comparable severity) best explained by genuine capacity and political-will constraints operating on an otherwise legitimate order, or is the selectivity itself evidence that the order was designed to serve the interests of veto-holding states from the outset?',
    'Comparative case analysis holding atrocity severity constant while varying the interests of P5 members in the target state; if selectivity tracks P5 strategic interest more strongly than it tracks atrocity severity or humanitarian need across a large sample, the capacity-constraint explanation weakens substantially.',
    'If selectivity tracks P5 interest rather than capacity, this reading''s central claim (legitimacy problem vs. capacity problem) becomes difficult to sustain and the hegemonic_extraction_reading''s classification of the same standing arrangement becomes the better-supported account.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(selectivity_capacity_vs_design, empirical, 'Whether enforcement selectivity is caused by capacity limits or by design serving powerful-state interests.').

omega_variable(
    consent_thickness_ambiguity,
    'Does formal treaty ratification and Charter membership constitute meaningful consent to the RBIO norm complex when many states ratified under significant power asymmetry, decolonization-era pressure, or as a condition of international financial and diplomatic inclusion?',
    'Historical analysis of ratification conditions and subsequent withdrawal/reservation patterns; genuine consent would be evidenced by states retaining and exercising real exit or renegotiation options without severe cost, not merely by formal ratification records.',
    'If consent was substantially coerced or conditioned by structural power asymmetry at the point of ratification, the ''consent-based'' premise central to this reading weakens, moving the classification toward the hegemonic reading''s account of the same arrangement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(consent_thickness_ambiguity, conceptual, 'Whether formal ratification constitutes substantively meaningful consent given the power conditions under which it occurred.').

omega_variable(
    reform_channel_efficacy,
    'Are the formal multilateral revision channels (Security Council reform proposals, treaty amendment procedures, General Assembly resolutions) genuine live pathways to change the norm complex, or symbolic channels that have never produced substantive reallocation of enforcement power away from the P5?',
    'Track record analysis: count substantive Council-composition or veto-power reforms actually adopted since 1945 versus the number of serious reform proposals introduced; a near-zero adoption rate over 80 years would undercut the ''revisable'' premise.',
    'A demonstrated near-zero reform track record would support classifying the revisability claim itself as theatrical, pushing the computed type toward snare at the payer seat and strengthening the case for the hegemonic_extraction_reading as the better-fitting account.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reform_channel_efficacy, empirical, 'Whether formal revision channels have ever produced substantive redistribution of enforcement authority.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(rbio_practice_norm_complex__liberal_institutional_reading, 1945, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(rbio_tr_t1945, rbio_practice_norm_complex__liberal_institutional_reading, theater_ratio, 1945, 0.18).
narrative_ontology:measurement(rbio_tr_t1971, rbio_practice_norm_complex__liberal_institutional_reading, theater_ratio, 1971, 0.2).
narrative_ontology:measurement(rbio_tr_t1991, rbio_practice_norm_complex__liberal_institutional_reading, theater_ratio, 1991, 0.22).
narrative_ontology:measurement(rbio_tr_t2003, rbio_practice_norm_complex__liberal_institutional_reading, theater_ratio, 2003, 0.25).
narrative_ontology:measurement(rbio_tr_t2014, rbio_practice_norm_complex__liberal_institutional_reading, theater_ratio, 2014, 0.27).
narrative_ontology:measurement(rbio_tr_t2024, rbio_practice_norm_complex__liberal_institutional_reading, theater_ratio, 2024, 0.28).

% Extraction over time
narrative_ontology:measurement(rbio_be_t1945, rbio_practice_norm_complex__liberal_institutional_reading, base_extractiveness, 1945, 0.3).
narrative_ontology:measurement(rbio_be_t1971, rbio_practice_norm_complex__liberal_institutional_reading, base_extractiveness, 1971, 0.33).
narrative_ontology:measurement(rbio_be_t1991, rbio_practice_norm_complex__liberal_institutional_reading, base_extractiveness, 1991, 0.36).
narrative_ontology:measurement(rbio_be_t2003, rbio_practice_norm_complex__liberal_institutional_reading, base_extractiveness, 2003, 0.4).
narrative_ontology:measurement(rbio_be_t2014, rbio_practice_norm_complex__liberal_institutional_reading, base_extractiveness, 2014, 0.41).
narrative_ontology:measurement(rbio_be_t2024, rbio_practice_norm_complex__liberal_institutional_reading, base_extractiveness, 2024, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(rbio_su_t1945, rbio_practice_norm_complex__liberal_institutional_reading, suppression_requirement, 1945, 0.25).
narrative_ontology:measurement(rbio_su_t1971, rbio_practice_norm_complex__liberal_institutional_reading, suppression_requirement, 1971, 0.28).
narrative_ontology:measurement(rbio_su_t1991, rbio_practice_norm_complex__liberal_institutional_reading, suppression_requirement, 1991, 0.32).
narrative_ontology:measurement(rbio_su_t2003, rbio_practice_norm_complex__liberal_institutional_reading, suppression_requirement, 2003, 0.36).
narrative_ontology:measurement(rbio_su_t2014, rbio_practice_norm_complex__liberal_institutional_reading, suppression_requirement, 2014, 0.37).
narrative_ontology:measurement(rbio_su_t2024, rbio_practice_norm_complex__liberal_institutional_reading, suppression_requirement, 2024, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(rbio_practice_norm_complex__liberal_institutional_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(rbio_practice_norm_complex__liberal_institutional_reading, 0.1).
narrative_ontology:affects_constraint(rbio_practice_norm_complex__liberal_institutional_reading, hegemonic_extraction_reading).
narrative_ontology:affects_constraint(rbio_practice_norm_complex__liberal_institutional_reading, sovereignty_maximalist_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the rbio_practice_norm_complex kernel, each authored as a structurally distinct constraint per the ε-invariance principle. liberal_institutional_reading (this file, ε=0.42, tangled_rope) affirms genuine coordination function with bounded, capacity-driven extraction. hegemonic_extraction_reading treats the identical standing arrangement as fixed hegemonic extraction with theatrical revisability (expected higher ε, snare or tangled_rope with much higher suppression). sovereignty_maximalist_reading denies the coordination premise for humanitarian intervention specifically, treating it as pretext (expected snare classification for the intervention-justification component). All three share the same underlying practice (UN Charter system, Security Council practice, sanctions regimes) but diverge on what the practice IS structurally — this is a kernel-reading decomposition, not a measurement-parameter difference.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
