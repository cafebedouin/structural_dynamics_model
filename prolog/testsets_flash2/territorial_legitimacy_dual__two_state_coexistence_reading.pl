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
 *   for mutual recognition of Israeli and Palestinian states based on 1967
 *   borders, with a limited right of return for Palestinians and security
 *   cooperation. While presented as a coordination mechanism by the
 *   international community, its implementation involves significant
 *   extraction from those whose maximalist claims (e.g., full right of
 *   return, all of Jerusalem) are foreclosed. The metrics reflect the
 *   fluctuating political will and enforcement capacity over time, with a
 *   recent dip in extractiveness and suppression reflecting a period of
 *   reduced diplomatic engagement rather than resolution.
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
narrative_ontology:cs_story_uid(territorial_legitimacy_dual__two_state_coexistence_reading, '6dd94043-c956-4cb0-b3fa-649c8fed4dec').
narrative_ontology:cs_kernel_codification('6dd94043-c956-4cb0-b3fa-649c8fed4dec', formalized).
narrative_ontology:cs_authority_grounding('6dd94043-c956-4cb0-b3fa-649c8fed4dec', lineage).
narrative_ontology:cs_interpretation_layer_present('6dd94043-c956-4cb0-b3fa-649c8fed4dec').
narrative_ontology:cs_reading_relation('6dd94043-c956-4cb0-b3fa-649c8fed4dec', territorial_legitimacy_dual__zionist_refuge_reading, coexists_with).
narrative_ontology:cs_reading_relation('6dd94043-c956-4cb0-b3fa-649c8fed4dec', territorial_legitimacy_dual__palestinian_autochthony_reading, coexists_with).
narrative_ontology:cs_axiom('6dd94043-c956-4cb0-b3fa-649c8fed4dec', foundational, mutual_recognition_of_national_rights).
narrative_ontology:cs_axiom_status(mutual_recognition_of_national_rights, holdable).
narrative_ontology:cs_axiom_grounding('6dd94043-c956-4cb0-b3fa-649c8fed4dec', mutual_recognition_of_national_rights, conventional).
narrative_ontology:cs_axiom('6dd94043-c956-4cb0-b3fa-649c8fed4dec', foundational, territorial_division_on_1967_lines).
narrative_ontology:cs_axiom_status(territorial_division_on_1967_lines, holdable).
narrative_ontology:cs_axiom_grounding('6dd94043-c956-4cb0-b3fa-649c8fed4dec', territorial_division_on_1967_lines, conventional).
narrative_ontology:cs_reference_frame('6dd94043-c956-4cb0-b3fa-649c8fed4dec', oslo_accords_framework).
narrative_ontology:cs_drift_state('6dd94043-c956-4cb0-b3fa-649c8fed4dec', contemporary, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('6dd94043-c956-4cb0-b3fa-649c8fed4dec', '').
narrative_ontology:cs_kernel_id(territorial_legitimacy_dual__two_state_coexistence_reading, territorial_legitimacy_dual).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(territorial_legitimacy_dual__two_state_coexistence_reading, international_diplomatic_community).
narrative_ontology:constraint_beneficiary(territorial_legitimacy_dual__two_state_coexistence_reading, israeli_security_establishment).
narrative_ontology:constraint_beneficiary(territorial_legitimacy_dual__two_state_coexistence_reading, palestinian_authority_leadership).
narrative_ontology:constraint_victim(territorial_legitimacy_dual__two_state_coexistence_reading, palestinian_refugees).
narrative_ontology:constraint_victim(territorial_legitimacy_dual__two_state_coexistence_reading, israeli_settlers).
narrative_ontology:constraint_victim(territorial_legitimacy_dual__two_state_coexistence_reading, hardline_political_factions).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Actively promotes and mediates the two-state solution based on 1967 borders, viewing it as the only viable path to regional stability. Invests significant diplomatic capital and aid, but faces limited enforcement mechanisms beyond sanctions and political pressure.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__two_state_coexistence_reading, international_diplomatic_community, agenda_setter,
    institutional, generational, constrained, global).

% Benefits from the framework's emphasis on security cooperation and the potential for a stable, recognized border. However, it views the 1967 borders as a security risk and resists full withdrawal without robust guarantees. It is a beneficiary of the stability but a payer of the territorial compromise.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__two_state_coexistence_reading, israeli_security_establishment, beneficiary,
    institutional, biographical, constrained, national).

% Seeks statehood and international recognition, which the two-state framework offers. It benefits from international aid and diplomatic support tied to this framework, but faces internal resistance for compromising on the right of return and 1948 legitimacy.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__two_state_coexistence_reading, palestinian_authority_leadership, beneficiary,
    organized, biographical, constrained, regional).

% Bear the cost of the limited right of return, which is a core tenet of this framework. Their historical claims and aspirations for return to 1948 territories are largely foreclosed by the 1967-based partition. They have no direct exit from this imposed compromise.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__two_state_coexistence_reading, palestinian_refugees, payer,
    powerless, generational, trapped, regional).

% Face potential displacement or loss of land under a 1967-borders framework. Their ideological commitment to settlement expansion and historical claims to the West Bank makes this framework a direct threat to their way of life and identity. Their exit is identity-locked to their religious and national narratives.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__two_state_coexistence_reading, israeli_settlers, payer,
    organized, biographical, identity_locked, local).

% On both sides, these factions reject the premise of mutual recognition and compromise. They are excluded from the diplomatic process but exert significant internal political pressure, actively resisting any moves towards this framework. Their exclusion is a condition for the framework's progress.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__two_state_coexistence_reading, hardline_political_factions, excluded,
    powerful, generational, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a mutually recognized framework for two sovereign states to coexist peacefully, resolving territorial disputes and security concerns through negotiation and international guarantees.
% TRANSFER_FUNCTION: Transfers territorial claims and historical narratives into a pragmatic compromise: land for peace, security for recognition, and limited return for statehood. It aims to transfer the burden of conflict into the benefits of stability.
% ABSENT_VOICES: Hardline factions on both sides, as well as those whose historical narratives are marginalized by the compromise (e.g., Palestinian refugees' full right of return, Israeli claims to all of Jerusalem), are excluded from the direct negotiation table. They would argue for maximalist positions.
% DISAPPEARANCE_RATIONALE: If the two-state coexistence framework vanished, the region would likely revert to intensified conflict, unilateral actions, and a complete breakdown of diplomatic efforts, leading to significant geopolitical instability and humanitarian crises. The current, albeit fragile, arrangements depend on its continued (even if aspirational) existence.
% FOUNDING_PROBLEM: The intractable conflict between Israelis and Palestinians over land, sovereignty, and historical narratives, leading to cycles of violence, occupation, and lack of self-determination for Palestinians.
% FOUNDING_PROBLEM_CORROBORATION: The international diplomatic community, numerous UN resolutions, and a significant portion of both Israeli and Palestinian civil society attest that the core problem of conflict and lack of resolution remains live, despite the framework's contested implementation. Independent human rights organizations and conflict analysts corroborate the ongoing nature of the conflict.
narrative_ontology:disappearance_verdict(territorial_legitimacy_dual__two_state_coexistence_reading, world_rearranges).
narrative_ontology:founding_problem_status(territorial_legitimacy_dual__two_state_coexistence_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(territorial_legitimacy_dual__two_state_coexistence_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
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
 *   Extractiveness (0.45) is moderate because the framework demands significant concessions from both sides, particularly from Palestinian refugees and Israeli settlers, whose core claims are compromised. Suppression (0.6) is high due to the active diplomatic and political pressure required to maintain adherence to the framework, and the suppression of alternative, more maximalist, narratives. Theater ratio (0.2) is low, as the framework, despite its challenges, remains a genuine diplomatic objective, not merely a performance. The recent decrease in extractiveness and suppression reflects a period where the framework is less actively enforced or pursued, leading to less direct pressure on the 'victims' but also less progress towards resolution.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the international diplomatic community, this is a necessary, albeit difficult, coordination mechanism. From the perspective of Palestinian refugees and Israeli settlers, it is an extractive imposition that denies their fundamental rights or historical claims. The engine's per-seat classification will reflect these divergent experiences.
 *
 * DIRECTIONALITY LOGIC:
 *   The international diplomatic community acts as the agenda-setter, pushing for this framework. Israeli security establishment and Palestinian Authority leadership are beneficiaries of the stability and recognition it offers, but also bear costs of compromise. Palestinian refugees and Israeli settlers are primary payers, as their core claims are directly curtailed. Hardline political factions are excluded, as their positions are antithetical to the compromise.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    enforcement_capacity_ambiguity,
    'Is the international community''s capacity to enforce the 1967-borders framework sufficient to overcome internal resistance from hardline factions on both sides?',
    'Observation of sustained, coordinated international pressure (e.g., sanctions, diplomatic recognition) leading to tangible territorial adjustments or cessation of settlement expansion.',
    'If enforcement capacity is low, the framework remains aspirational, increasing its theater ratio and effective extractiveness on those who make concessions without receiving promised benefits. If high, it moves closer to a functional rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_capacity_ambiguity, empirical, 'Uncertainty regarding the actual power of international actors to implement the framework.').

omega_variable(
    legitimacy_of_compromise_vs_maximalism,
    'Is the compromise inherent in the 1967-borders framework (e.g., limited right of return, territorial concessions) genuinely legitimate for all parties, or is it perceived as an imposed solution by those whose maximalist claims are foreclosed?',
    'Longitudinal studies of public opinion and political discourse within both Israeli and Palestinian societies, assessing shifts in acceptance of the compromise over generations.',
    'If perceived as illegitimate by significant populations, the framework will continue to face high resistance and require high suppression, pushing it towards a snare. If internal legitimacy grows, it could transition towards a more stable rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(legitimacy_of_compromise_vs_maximalism, conceptual, 'The conceptual tension between pragmatic compromise and historical/ideological maximalist claims.').

omega_variable(
    security_cooperation_sustainability,
    'Can security cooperation between Israeli and Palestinian entities be sustained and deepened in the absence of a final political settlement, or is it inherently fragile and subject to political breakdowns?',
    'Analysis of historical periods of cooperation and breakdown, identifying triggers and conditions for resilience or failure, particularly during periods of heightened conflict.',
    'If security cooperation is inherently fragile, the framework''s coordination function is weakened, increasing perceived risk for beneficiaries and potentially leading to unilateral actions that undermine the 1967 borders. If robust, it strengthens the framework''s rope-like qualities.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(security_cooperation_sustainability, empirical, 'The durability of security cooperation as a pillar of the two-state framework.').


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
narrative_ontology:measurement(terr_tr_t2008, territorial_legitimacy_dual__two_state_coexistence_reading, theater_ratio, 2008, 0.25).
narrative_ontology:measurement(terr_tr_t2016, territorial_legitimacy_dual__two_state_coexistence_reading, theater_ratio, 2016, 0.3).
narrative_ontology:measurement(terr_tr_t2024, territorial_legitimacy_dual__two_state_coexistence_reading, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(terr_be_t1993, territorial_legitimacy_dual__two_state_coexistence_reading, base_extractiveness, 1993, 0.3).
narrative_ontology:measurement(terr_be_t2000, territorial_legitimacy_dual__two_state_coexistence_reading, base_extractiveness, 2000, 0.4).
narrative_ontology:measurement(terr_be_t2008, territorial_legitimacy_dual__two_state_coexistence_reading, base_extractiveness, 2008, 0.48).
narrative_ontology:measurement(terr_be_t2016, territorial_legitimacy_dual__two_state_coexistence_reading, base_extractiveness, 2016, 0.55).
narrative_ontology:measurement(terr_be_t2024, territorial_legitimacy_dual__two_state_coexistence_reading, base_extractiveness, 2024, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(terr_su_t1993, territorial_legitimacy_dual__two_state_coexistence_reading, suppression_requirement, 1993, 0.5).
narrative_ontology:measurement(terr_su_t2000, territorial_legitimacy_dual__two_state_coexistence_reading, suppression_requirement, 2000, 0.6).
narrative_ontology:measurement(terr_su_t2008, territorial_legitimacy_dual__two_state_coexistence_reading, suppression_requirement, 2008, 0.65).
narrative_ontology:measurement(terr_su_t2016, territorial_legitimacy_dual__two_state_coexistence_reading, suppression_requirement, 2016, 0.7).
narrative_ontology:measurement(terr_su_t2024, territorial_legitimacy_dual__two_state_coexistence_reading, suppression_requirement, 2024, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(territorial_legitimacy_dual__two_state_coexistence_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(territorial_legitimacy_dual__two_state_coexistence_reading, zionist_refuge_reading).
narrative_ontology:affects_constraint(territorial_legitimacy_dual__two_state_coexistence_reading, palestinian_autochthony_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'territorial_legitimacy_dual' kernel. It represents the two-state coexistence framework based on 1967 borders, which influences and is influenced by other readings of legitimacy in the region.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
