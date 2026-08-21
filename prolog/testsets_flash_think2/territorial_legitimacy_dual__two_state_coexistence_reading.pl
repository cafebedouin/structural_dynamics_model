% ============================================================================
% CONSTRAINT STORY: territorial_legitimacy_dual__two_state_coexistence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_territorial_legitimacy_dual__two_state_coexistence_reading, []).

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
 *   constraint_id: territorial_legitimacy_dual__two_state_coexistence_reading
 *   human_readable: Two-State Coexistence Framework (1967 Borders)
 *   domain: political_theory/international_relations/territorial_sovereignty
 *
 * SUMMARY:
 *   This constraint is the 'two_state_coexistence_reading' of the
 *   'territorial_legitimacy_dual' kernel. It posits mutual recognition of
 *   1948 legitimacy for both peoples, 1967 boundaries as the basis for
 *   partition, a limited right of return for Palestinian refugees, and
 *   security cooperation to replace zero-sum competition. Sibling readings
 *   include 'zionist_refuge_reading' and 'palestinian_autochthony_reading',
 *   which emphasize different foundational claims and maximalist outcomes.
 *   This reading is claimed as a Tangled Rope because it offers a genuine
 *   coordination function (coexistence) but involves significant concessions
 *   (extraction) from both sides' maximal claims, requiring active
 *   enforcement against rejectionist elements.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(territorial_legitimacy_dual__two_state_coexistence_reading, 0.75).
domain_priors:suppression_score(territorial_legitimacy_dual__two_state_coexistence_reading, 0.85).
domain_priors:theater_ratio(territorial_legitimacy_dual__two_state_coexistence_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(territorial_legitimacy_dual__two_state_coexistence_reading, extractiveness, 0.75).
narrative_ontology:constraint_metric(territorial_legitimacy_dual__two_state_coexistence_reading, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(territorial_legitimacy_dual__two_state_coexistence_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(territorial_legitimacy_dual__two_state_coexistence_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(territorial_legitimacy_dual__two_state_coexistence_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(territorial_legitimacy_dual__two_state_coexistence_reading, tangled_rope).
narrative_ontology:human_readable(territorial_legitimacy_dual__two_state_coexistence_reading, "Two-State Coexistence Framework (1967 Borders)").
narrative_ontology:topic_domain(territorial_legitimacy_dual__two_state_coexistence_reading, "political_theory/international_relations/territorial_sovereignty").

domain_priors:requires_active_enforcement(territorial_legitimacy_dual__two_state_coexistence_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(territorial_legitimacy_dual__two_state_coexistence_reading, '4027cf59-c4d0-4d5f-a80a-7c172a788e7c').
narrative_ontology:cs_kernel_codification('4027cf59-c4d0-4d5f-a80a-7c172a788e7c', formalized).
narrative_ontology:cs_authority_grounding('4027cf59-c4d0-4d5f-a80a-7c172a788e7c', lineage).
narrative_ontology:cs_interpretation_layer_present('4027cf59-c4d0-4d5f-a80a-7c172a788e7c').
narrative_ontology:cs_reading_relation('4027cf59-c4d0-4d5f-a80a-7c172a788e7c', territorial_legitimacy_dual__zionist_refuge_reading, coexists_with).
narrative_ontology:cs_reading_relation('4027cf59-c4d0-4d5f-a80a-7c172a788e7c', territorial_legitimacy_dual__palestinian_autochthony_reading, coexists_with).
narrative_ontology:cs_axiom('4027cf59-c4d0-4d5f-a80a-7c172a788e7c', foundational, mutual_recognition_of_national_rights).
narrative_ontology:cs_axiom_status(mutual_recognition_of_national_rights, holdable).
narrative_ontology:cs_axiom_grounding('4027cf59-c4d0-4d5f-a80a-7c172a788e7c', mutual_recognition_of_national_rights, deontological).
narrative_ontology:cs_axiom('4027cf59-c4d0-4d5f-a80a-7c172a788e7c', foundational, land_for_peace_principle).
narrative_ontology:cs_axiom_status(land_for_peace_principle, holdable).
narrative_ontology:cs_axiom_grounding('4027cf59-c4d0-4d5f-a80a-7c172a788e7c', land_for_peace_principle, conventional).
narrative_ontology:cs_reference_frame('4027cf59-c4d0-4d5f-a80a-7c172a788e7c', oslo_accords_framework).
narrative_ontology:cs_drift_state('4027cf59-c4d0-4d5f-a80a-7c172a788e7c', contemporary_stalemate, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('4027cf59-c4d0-4d5f-a80a-7c172a788e7c', '').
narrative_ontology:cs_kernel_id(territorial_legitimacy_dual__two_state_coexistence_reading, territorial_legitimacy_dual).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(territorial_legitimacy_dual__two_state_coexistence_reading, israeli_moderates).
narrative_ontology:constraint_beneficiary(territorial_legitimacy_dual__two_state_coexistence_reading, palestinian_moderates).
narrative_ontology:constraint_beneficiary(territorial_legitimacy_dual__two_state_coexistence_reading, international_diplomacy).
narrative_ontology:constraint_victim(territorial_legitimacy_dual__two_state_coexistence_reading, israeli_settler_movement).
narrative_ontology:constraint_victim(territorial_legitimacy_dual__two_state_coexistence_reading, palestinian_rejectionist_factions).
narrative_ontology:constraint_victim(territorial_legitimacy_dual__two_state_coexistence_reading, palestinian_refugees).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from the promise of security, international legitimacy, and a defined border for Israel, but must concede territorial claims beyond the 1967 lines and accept a Palestinian state. Their political power is often challenged by hardline factions.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__two_state_coexistence_reading, israeli_moderates, beneficiary,
    powerful, biographical, constrained, national).

% Benefit from the promise of statehood, international recognition, and self-determination, but must concede the full right of return for refugees and maximal territorial claims. Their legitimacy is often challenged by rejectionist factions.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__two_state_coexistence_reading, palestinian_moderates, beneficiary,
    powerful, biographical, constrained, national).

% Actively promotes and enforces the two-state framework as the only viable path to peace and stability. Invests significant political capital and resources, but faces challenges in implementation and enforcement against local resistance.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__two_state_coexistence_reading, international_diplomacy, agenda_setter,
    institutional, generational, analytical, global).

% Views the 1967 borders and the concept of a Palestinian state as an ideological and territorial concession that undermines their religious and historical claims. Actively resists any withdrawal or dismantling of settlements, making them a target of the framework's enforcement.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__two_state_coexistence_reading, israeli_settler_movement, payer,
    organized, generational, identity_locked, local).

% Rejects any recognition of Israel and views the two-state solution as a betrayal of the Palestinian cause, including the full right of return and control over all of historical Palestine. Actively undermines the framework through political and sometimes violent means.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__two_state_coexistence_reading, palestinian_rejectionist_factions, payer,
    organized, generational, identity_locked, local).

% Their full right of return to their ancestral homes is limited or foreclosed by this framework, representing a significant and deeply felt concession. They are often marginalized in the diplomatic process but remain a potent symbol of the conflict's unresolved issues.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__two_state_coexistence_reading, palestinian_refugees, payer,
    powerless, generational, identity_locked, regional).

% Are directly affected by the stability or instability of the Israeli-Palestinian conflict. They may offer diplomatic support, financial aid, or security cooperation to either side, influencing the viability of the framework.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__two_state_coexistence_reading, regional_powers, observer,
    institutional, generational, mobile, regional).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To provide a mutually recognized framework for two distinct national groups to coexist with secure, defined borders (based on 1967 lines) and reciprocal security arrangements, aiming to prevent perpetual conflict and achieve lasting peace.
% TRANSFER_FUNCTION: Transfers maximal territorial and historical claims from both Israeli and Palestinian sides into a compromise framework, with security, international recognition, and limited self-determination as the reciprocal gains. It also transfers the burden of concessions onto rejectionist elements and refugees.
% ABSENT_VOICES: Hardline rejectionist groups on both Israeli and Palestinian sides are structurally excluded from the diplomatic process that underpins this framework. Their maximalist claims are foreclosed by the very premises of mutual recognition and partition, and they would vehemently object to its terms.
% DISAPPEARANCE_RATIONALE: If this framework vanished overnight, the conflict would immediately escalate, leading to renewed violence, regional instability, and a collapse of international diplomatic efforts. The political landscape would reorganize around maximalist claims and direct confrontation, with severe humanitarian and geopolitical consequences.
% FOUNDING_PROBLEM: The intractable, zero-sum conflict over the same land, driven by competing historical narratives, national aspirations, and cycles of violence, which lacked a mutually acceptable path to lasting peace and security for both peoples.
% FOUNDING_PROBLEM_CORROBORATION: International bodies (e.g., UN, EU, Quartet), numerous peace initiatives, and a broad consensus among political scientists and historians outside the immediate parties consistently attest to the severity and persistence of the founding problem, even as the framework struggles to address it.
narrative_ontology:disappearance_verdict(territorial_legitimacy_dual__two_state_coexistence_reading, world_rearranges).
narrative_ontology:founding_problem_status(territorial_legitimacy_dual__two_state_coexistence_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(territorial_legitimacy_dual__two_state_coexistence_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(territorial_legitimacy_dual__two_state_coexistence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(territorial_legitimacy_dual__two_state_coexistence_reading, 0.75, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(territorial_legitimacy_dual__two_state_coexistence_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(territorial_legitimacy_dual__two_state_coexistence_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(territorial_legitimacy_dual__two_state_coexistence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The base extractiveness is high (0.75) because both Israeli and Palestinian maximalist claims are significantly curtailed, and the limited right of return for Palestinian refugees is a substantial loss. Suppression is very high (0.85) as the framework requires continuous international and local enforcement against rejectionist factions, settlement expansion, and other actions that undermine its premises. The theater ratio is moderate (0.45) reflecting that while diplomatic efforts continue, substantive progress has often been replaced by performative statements and stalled negotiations. The temporal measurements show a clear trend of increasing extractiveness, suppression, and theatricality as the framework has struggled to be implemented over time.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of international diplomacy and moderates on both sides, this framework is a necessary, if difficult, path to peace. From the perspective of rejectionist factions and many Palestinian refugees, it is an imposed extraction that demands unacceptable concessions and perpetuates injustice. The engine's per-seat classification will reflect this divergence, with beneficiaries experiencing a Rope-like function and victims experiencing a Snare-like function, despite the overall Tangled Rope classification.
 *
 * DIRECTIONALITY LOGIC:
 *   International diplomacy acts as the agenda-setter and a beneficiary, gaining stability and a framework for engagement. Israeli and Palestinian moderates are beneficiaries, gaining security and statehood/recognition respectively, but also payers through the concessions they make. Israeli settler movements and Palestinian rejectionist factions are clear targets (payers), as the framework directly forecloses their maximalist claims. Palestinian refugees are also targets (payers) due to the limitation of their right of return. All these targets are identity-locked, making their exit options severely constrained.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    framework_viability_vs_truce,
    'Is the two-state coexistence framework a genuinely viable long-term solution, or is it a temporary truce that merely defers deeper structural conflicts?',
    'Sustained implementation of core agreements (e.g., border demarcation, security arrangements, refugee compensation) over a generational time horizon, leading to normalized relations and reduced reliance on external enforcement.',
    'If it proves genuinely viable, the framework''s classification would shift closer to a Rope, with lower long-term extractiveness and suppression. If it remains a temporary truce, its Tangled Rope classification would be reinforced, with potential drift towards a Snare if extraction increases without corresponding coordination benefits.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(framework_viability_vs_truce, empirical, 'Long-term viability of the two-state framework.').

omega_variable(
    right_of_return_justice_vs_compromise,
    'Is the limitation of the Palestinian right of return, as stipulated by this framework, a necessary and just compromise for peace, or an unjust extraction that perpetuates historical grievances?',
    'A comprehensive, internationally mediated process that addresses the historical claims and material losses of Palestinian refugees, leading to a mutually agreed-upon resolution that is perceived as just by the affected population.',
    'If perceived as unjust, the extractiveness from Palestinian refugees would be confirmed as high, reinforcing the Snare-like experience for this group. If a just resolution is achieved, the perceived extraction would decrease, potentially shifting the framework''s overall classification towards a more balanced Tangled Rope or even a Rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(right_of_return_justice_vs_compromise, preference, 'Ethical evaluation of the limited right of return.').

omega_variable(
    security_cooperation_efficacy,
    'Can security cooperation truly replace zero-sum competition and deep mistrust between the parties, or is it inherently unstable and prone to collapse under pressure?',
    'Long-term empirical observation of security arrangements, including joint operations, intelligence sharing, and de-escalation mechanisms, demonstrating resilience during periods of high tension and a sustained reduction in conflict-related fatalities.',
    'If security cooperation proves robust, the coordination function of the framework would be strengthened, potentially reducing the overall suppression requirement. If it remains fragile, the high suppression and extractiveness would be confirmed as necessary to maintain a precarious balance, reinforcing the Tangled Rope classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(security_cooperation_efficacy, empirical, 'Effectiveness of security cooperation in replacing competition.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(territorial_legitimacy_dual__two_state_coexistence_reading, 1993, 2023).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(terr_tr_t1993, territorial_legitimacy_dual__two_state_coexistence_reading, theater_ratio, 1993, 0.2).
narrative_ontology:measurement(terr_tr_t1998, territorial_legitimacy_dual__two_state_coexistence_reading, theater_ratio, 1998, 0.25).
narrative_ontology:measurement(terr_tr_t2003, territorial_legitimacy_dual__two_state_coexistence_reading, theater_ratio, 2003, 0.3).
narrative_ontology:measurement(terr_tr_t2008, territorial_legitimacy_dual__two_state_coexistence_reading, theater_ratio, 2008, 0.35).
narrative_ontology:measurement(terr_tr_t2013, territorial_legitimacy_dual__two_state_coexistence_reading, theater_ratio, 2013, 0.4).
narrative_ontology:measurement(terr_tr_t2018, territorial_legitimacy_dual__two_state_coexistence_reading, theater_ratio, 2018, 0.43).
narrative_ontology:measurement(terr_tr_t2023, territorial_legitimacy_dual__two_state_coexistence_reading, theater_ratio, 2023, 0.45).

% Extraction over time
narrative_ontology:measurement(terr_be_t1993, territorial_legitimacy_dual__two_state_coexistence_reading, base_extractiveness, 1993, 0.6).
narrative_ontology:measurement(terr_be_t1998, territorial_legitimacy_dual__two_state_coexistence_reading, base_extractiveness, 1998, 0.65).
narrative_ontology:measurement(terr_be_t2003, territorial_legitimacy_dual__two_state_coexistence_reading, base_extractiveness, 2003, 0.68).
narrative_ontology:measurement(terr_be_t2008, territorial_legitimacy_dual__two_state_coexistence_reading, base_extractiveness, 2008, 0.7).
narrative_ontology:measurement(terr_be_t2013, territorial_legitimacy_dual__two_state_coexistence_reading, base_extractiveness, 2013, 0.72).
narrative_ontology:measurement(terr_be_t2018, territorial_legitimacy_dual__two_state_coexistence_reading, base_extractiveness, 2018, 0.74).
narrative_ontology:measurement(terr_be_t2023, territorial_legitimacy_dual__two_state_coexistence_reading, base_extractiveness, 2023, 0.75).

% Suppression requirement over time
narrative_ontology:measurement(terr_su_t1993, territorial_legitimacy_dual__two_state_coexistence_reading, suppression_requirement, 1993, 0.7).
narrative_ontology:measurement(terr_su_t1998, territorial_legitimacy_dual__two_state_coexistence_reading, suppression_requirement, 1998, 0.75).
narrative_ontology:measurement(terr_su_t2003, territorial_legitimacy_dual__two_state_coexistence_reading, suppression_requirement, 2003, 0.78).
narrative_ontology:measurement(terr_su_t2008, territorial_legitimacy_dual__two_state_coexistence_reading, suppression_requirement, 2008, 0.8).
narrative_ontology:measurement(terr_su_t2013, territorial_legitimacy_dual__two_state_coexistence_reading, suppression_requirement, 2013, 0.82).
narrative_ontology:measurement(terr_su_t2018, territorial_legitimacy_dual__two_state_coexistence_reading, suppression_requirement, 2018, 0.84).
narrative_ontology:measurement(terr_su_t2023, territorial_legitimacy_dual__two_state_coexistence_reading, suppression_requirement, 2023, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(territorial_legitimacy_dual__two_state_coexistence_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(territorial_legitimacy_dual__two_state_coexistence_reading, territorial_legitimacy_dual__zionist_refuge_reading).
narrative_ontology:affects_constraint(territorial_legitimacy_dual__two_state_coexistence_reading, territorial_legitimacy_dual__palestinian_autochthony_reading).
narrative_ontology:affects_constraint(territorial_legitimacy_dual__two_state_coexistence_reading, international_aid_to_palestine).
narrative_ontology:affects_constraint(territorial_legitimacy_dual__two_state_coexistence_reading, israeli_settlement_expansion).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'territorial_legitimacy_dual' kernel, focusing on a two-state solution with 1967 borders. It is linked to sibling readings that represent alternative, often maximalist, interpretations of territorial legitimacy.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
