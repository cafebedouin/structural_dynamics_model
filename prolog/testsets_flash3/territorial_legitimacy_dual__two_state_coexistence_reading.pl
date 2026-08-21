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
 *   This constraint represents the 'two-state coexistence' reading of the
 *   broader kernel of 'territorial_legitimacy_dual'. It posits a framework
 *   where both Israeli and Palestinian national claims are recognized, with
 *   1967 borders as the basis for a two-state solution, a limited right of
 *   return for Palestinian refugees, and security cooperation. It is a
 *   compromise framework, inherently extractive from maximalist positions on
 *   both sides, and requires active enforcement by the international
 *   community and moderate leaderships.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(territorial_legitimacy_dual__two_state_coexistence_reading, 0.45).
domain_priors:suppression_score(territorial_legitimacy_dual__two_state_coexistence_reading, 0.6).
domain_priors:theater_ratio(territorial_legitimacy_dual__two_state_coexistence_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(territorial_legitimacy_dual__two_state_coexistence_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(territorial_legitimacy_dual__two_state_coexistence_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(territorial_legitimacy_dual__two_state_coexistence_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(territorial_legitimacy_dual__two_state_coexistence_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(territorial_legitimacy_dual__two_state_coexistence_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(territorial_legitimacy_dual__two_state_coexistence_reading, tangled_rope).
narrative_ontology:human_readable(territorial_legitimacy_dual__two_state_coexistence_reading, "Two-State Coexistence Framework (1967 Borders)").
narrative_ontology:topic_domain(territorial_legitimacy_dual__two_state_coexistence_reading, "political_theory/international_relations/territorial_sovereignty").

domain_priors:requires_active_enforcement(territorial_legitimacy_dual__two_state_coexistence_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(territorial_legitimacy_dual__two_state_coexistence_reading, '87171d0a-d06c-4483-aa14-036ef43b7c59').
narrative_ontology:cs_kernel_codification('87171d0a-d06c-4483-aa14-036ef43b7c59', formalized).
narrative_ontology:cs_authority_grounding('87171d0a-d06c-4483-aa14-036ef43b7c59', lineage).
narrative_ontology:cs_interpretation_layer_present('87171d0a-d06c-4483-aa14-036ef43b7c59').
narrative_ontology:cs_reading_relation('87171d0a-d06c-4483-aa14-036ef43b7c59', territorial_legitimacy_dual__zionist_refuge_reading, coexists_with).
narrative_ontology:cs_reading_relation('87171d0a-d06c-4483-aa14-036ef43b7c59', territorial_legitimacy_dual__palestinian_autochthony_reading, coexists_with).
narrative_ontology:cs_axiom('87171d0a-d06c-4483-aa14-036ef43b7c59', foundational, mutual_recognition_of_national_rights).
narrative_ontology:cs_axiom_status(mutual_recognition_of_national_rights, holdable).
narrative_ontology:cs_axiom_grounding('87171d0a-d06c-4483-aa14-036ef43b7c59', mutual_recognition_of_national_rights, conventional).
narrative_ontology:cs_axiom('87171d0a-d06c-4483-aa14-036ef43b7c59', foundational, territorial_partition_on_1967_lines).
narrative_ontology:cs_axiom_status(territorial_partition_on_1967_lines, holdable).
narrative_ontology:cs_axiom_grounding('87171d0a-d06c-4483-aa14-036ef43b7c59', territorial_partition_on_1967_lines, conventional).
narrative_ontology:cs_reference_frame('87171d0a-d06c-4483-aa14-036ef43b7c59', oslo_accords_framework).
narrative_ontology:cs_drift_state('87171d0a-d06c-4483-aa14-036ef43b7c59', contemporary_era_of_settlement_expansion, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('87171d0a-d06c-4483-aa14-036ef43b7c59', '').
narrative_ontology:cs_kernel_id(territorial_legitimacy_dual__two_state_coexistence_reading, territorial_legitimacy_dual).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(territorial_legitimacy_dual__two_state_coexistence_reading, international_diplomatic_community).
narrative_ontology:constraint_beneficiary(territorial_legitimacy_dual__two_state_coexistence_reading, israeli_security_establishment).
narrative_ontology:constraint_beneficiary(territorial_legitimacy_dual__two_state_coexistence_reading, palestinian_authority_leadership).
narrative_ontology:constraint_victim(territorial_legitimacy_dual__two_state_coexistence_reading, palestinian_refugees).
narrative_ontology:constraint_victim(territorial_legitimacy_dual__two_state_coexistence_reading, israeli_settlers).
narrative_ontology:constraint_victim(territorial_legitimacy_dual__two_state_coexistence_reading, hardline_factions_on_both_sides).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Advocates for and attempts to enforce the two-state solution based on 1967 borders, viewing it as the most viable path to regional stability. Provides aid and diplomatic pressure to both sides.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__two_state_coexistence_reading, international_diplomatic_community, agenda_setter,
    institutional, generational, mobile, global).

% Benefits from the framework's emphasis on security cooperation and the limitation of the right of return, which addresses key Israeli concerns. However, it faces internal resistance from hardline elements.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__two_state_coexistence_reading, israeli_security_establishment, beneficiary,
    institutional, biographical, constrained, national).

% Gains international recognition and a path to statehood, along with security and economic support. However, it must compromise on the full right of return and faces internal legitimacy challenges from those who reject the 1967 borders.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__two_state_coexistence_reading, palestinian_authority_leadership, beneficiary,
    organized, biographical, constrained, regional).

% Bear the cost of a limited right of return, which means many will not return to their ancestral homes within Israel. Their claims are partially acknowledged but ultimately constrained by the framework's compromises.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__two_state_coexistence_reading, palestinian_refugees, payer,
    powerless, generational, trapped, regional).

% Face potential displacement or loss of land in the West Bank if 1967 borders are strictly implemented. Their identity is often deeply tied to the land, making exit options extremely difficult.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__two_state_coexistence_reading, israeli_settlers, payer,
    organized, biographical, identity_locked, local).

% Reject the premise of mutual recognition and compromise, advocating for maximalist claims. They are actively excluded from the diplomatic process and often engage in resistance to the framework.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__two_state_coexistence_reading, hardline_factions_on_both_sides, excluded,
    organized, generational, identity_locked, regional).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a mutually acceptable framework for two distinct national entities to coexist peacefully, by defining borders, security arrangements, and limited rights of return, thereby preventing perpetual conflict.
% TRANSFER_FUNCTION: Transfers territorial claims and full right of return from Palestinians, and full territorial control and security autonomy from Israelis, in exchange for mutual recognition and a path to two sovereign states.
% ABSENT_VOICES: Hardline factions on both sides, particularly those advocating for a single state or maximalist territorial claims, are excluded. Palestinian refugees who demand an unrestricted right of return are also largely marginalized in this framework.
% DISAPPEARANCE_RATIONALE: If the two-state coexistence framework vanished, the region would likely revert to intensified conflict, with renewed maximalist claims from all parties, increased violence, and a collapse of international diplomatic efforts, leading to significant geopolitical instability.
% FOUNDING_PROBLEM: The core problem was the irreconcilable claims to the same land by two national groups, leading to cycles of violence, displacement, and a lack of secure borders or recognized sovereignty for either people.
% FOUNDING_PROBLEM_CORROBORATION: The international diplomatic community, UN resolutions, and a significant portion of the populations on both sides (though often a silent majority) corroborate that the fundamental problem of competing national claims remains live, and that this framework attempts to address it, even if imperfectly.
narrative_ontology:disappearance_verdict(territorial_legitimacy_dual__two_state_coexistence_reading, world_rearranges).
narrative_ontology:founding_problem_status(territorial_legitimacy_dual__two_state_coexistence_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(territorial_legitimacy_dual__two_state_coexistence_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(territorial_legitimacy_dual__two_state_coexistence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(territorial_legitimacy_dual__two_state_coexistence_reading, 0.45, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(territorial_legitimacy_dual__two_state_coexistence_reading_tests).
:- end_tests(territorial_legitimacy_dual__two_state_coexistence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.45) reflects the significant compromises required from both sides, particularly on territorial claims and the right of return. Suppression (0.6) is necessary to contain hardline resistance and maintain the diplomatic process. The theater ratio (0.2) is relatively low, as the diplomatic efforts are genuine, but there's an element of performative commitment from parties who internally resist the framework. The claimed type is 'tangled_rope' because it genuinely attempts to coordinate coexistence while extracting significant concessions from both sides, requiring active enforcement to hold.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the international diplomatic community, this is a necessary and beneficial coordination mechanism. From the perspective of Palestinian refugees and Israeli settlers, it is an extractive imposition that denies their full historical claims. The engine's per-seat classification will reflect these divergent experiences.
 *
 * DIRECTIONALITY LOGIC:
 *   The international diplomatic community, Israeli security establishment, and Palestinian Authority leadership are beneficiaries, as they gain a framework for stability, security, and statehood, respectively. Palestinian refugees and Israeli settlers are victims, as they bear the direct costs of territorial compromise and limitations on historical claims. Hardline factions are excluded, as their maximalist positions are incompatible with the framework.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    enforcement_capacity_vs_resistance,
    'Is the international community''s enforcement capacity sufficient to overcome resistance from hardline factions and maintain the framework?',
    'Empirical observation of the long-term effectiveness of sanctions, diplomatic pressure, and security cooperation in preventing unilateral actions that undermine the framework.',
    'If enforcement capacity is insufficient, the framework will degrade into a ''piton'' or ''snare'' as it fails to coordinate and becomes a cover for continued unilateral extraction. If sufficient, it could stabilize as a ''rope'' or ''tangled_rope''.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_capacity_vs_resistance, empirical, 'Assesses the real-world power of external actors to enforce the compromise.').

omega_variable(
    legitimacy_of_compromise,
    'Is the compromise framework (1967 borders, limited right of return) genuinely accepted as legitimate by a critical mass of both populations, or is it merely tolerated due to external pressure?',
    'Longitudinal polling, analysis of political discourse, and observation of grassroots movements on both sides to gauge genuine buy-in versus reluctant compliance.',
    'If legitimacy is low, the framework is a ''snare'' sustained by external coercion. If high, it functions as a ''rope'' or ''tangled_rope'' with internal support, making it more resilient to external shocks.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legitimacy_of_compromise, conceptual, 'Examines the internal acceptance of the framework''s core tenets.').

omega_variable(
    right_of_return_limitation_justice,
    'Is the limitation of the Palestinian right of return, as stipulated by this framework, a just and equitable compromise, or an unjust imposition?',
    'This is a preference-based question, resolvable only through a normative ethical framework that weighs competing claims of historical justice, national self-determination, and pragmatic coexistence.',
    'A judgment of injustice would reframe the framework''s extraction from Palestinian refugees as morally illegitimate, potentially shifting its classification towards ''snare'' from a justice-oriented perspective, regardless of its coordination function.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(right_of_return_limitation_justice, preference, 'Evaluates the ethical implications of the right of return compromise.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(territorial_legitimacy_dual__two_state_coexistence_reading, 1993, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(terr_tr_t1993, territorial_legitimacy_dual__two_state_coexistence_reading, theater_ratio, 1993, 0.1).
narrative_ontology:measurement(terr_tr_t2000, territorial_legitimacy_dual__two_state_coexistence_reading, theater_ratio, 2000, 0.15).
narrative_ontology:measurement(terr_tr_t2008, territorial_legitimacy_dual__two_state_coexistence_reading, theater_ratio, 2008, 0.18).
narrative_ontology:measurement(terr_tr_t2016, territorial_legitimacy_dual__two_state_coexistence_reading, theater_ratio, 2016, 0.19).
narrative_ontology:measurement(terr_tr_t2024, territorial_legitimacy_dual__two_state_coexistence_reading, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(terr_be_t1993, territorial_legitimacy_dual__two_state_coexistence_reading, base_extractiveness, 1993, 0.35).
narrative_ontology:measurement(terr_be_t2000, territorial_legitimacy_dual__two_state_coexistence_reading, base_extractiveness, 2000, 0.4).
narrative_ontology:measurement(terr_be_t2008, territorial_legitimacy_dual__two_state_coexistence_reading, base_extractiveness, 2008, 0.42).
narrative_ontology:measurement(terr_be_t2016, territorial_legitimacy_dual__two_state_coexistence_reading, base_extractiveness, 2016, 0.44).
narrative_ontology:measurement(terr_be_t2024, territorial_legitimacy_dual__two_state_coexistence_reading, base_extractiveness, 2024, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(terr_su_t1993, territorial_legitimacy_dual__two_state_coexistence_reading, suppression_requirement, 1993, 0.5).
narrative_ontology:measurement(terr_su_t2000, territorial_legitimacy_dual__two_state_coexistence_reading, suppression_requirement, 2000, 0.55).
narrative_ontology:measurement(terr_su_t2008, territorial_legitimacy_dual__two_state_coexistence_reading, suppression_requirement, 2008, 0.58).
narrative_ontology:measurement(terr_su_t2016, territorial_legitimacy_dual__two_state_coexistence_reading, suppression_requirement, 2016, 0.59).
narrative_ontology:measurement(terr_su_t2024, territorial_legitimacy_dual__two_state_coexistence_reading, suppression_requirement, 2024, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(territorial_legitimacy_dual__two_state_coexistence_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(territorial_legitimacy_dual__two_state_coexistence_reading, regional_security_cooperation_protocols).
narrative_ontology:affects_constraint(territorial_legitimacy_dual__two_state_coexistence_reading, international_aid_distribution_mechanisms).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'territorial_legitimacy_dual' kernel, focusing on the two-state coexistence framework. It is linked to the 'zionist_refuge_reading' and 'palestinian_autochthony_reading' as sibling interpretations of the same core conflict.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
