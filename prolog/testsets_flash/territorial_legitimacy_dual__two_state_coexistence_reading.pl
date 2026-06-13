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
 *   territorial_legitimacy_dual kernel. It posits mutual recognition of
 *   Israeli and Palestinian legitimacy, with a future Palestinian state based
 *   on 1967 borders and a limited right of return for Palestinian refugees.
 *   This framework is actively promoted by the international community as a
 *   compromise solution, but it entails significant extraction from those
 *   whose claims (e.g., full right of return, undivided land) are curtailed.
 *   The claimed type is 'tangled_rope' because it genuinely attempts to
 *   coordinate coexistence while imposing asymmetric costs on specific
 *   groups.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(territorial_legitimacy_dual__two_state_coexistence_reading, 0.4).
domain_priors:suppression_score(territorial_legitimacy_dual__two_state_coexistence_reading, 0.6).
domain_priors:theater_ratio(territorial_legitimacy_dual__two_state_coexistence_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(territorial_legitimacy_dual__two_state_coexistence_reading, extractiveness, 0.4).
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
narrative_ontology:cs_story_uid(territorial_legitimacy_dual__two_state_coexistence_reading, 'ae4c1b43-6c03-4133-9b90-b94271a1c0c6').
narrative_ontology:cs_kernel_codification('ae4c1b43-6c03-4133-9b90-b94271a1c0c6', formalized).
narrative_ontology:cs_authority_grounding('ae4c1b43-6c03-4133-9b90-b94271a1c0c6', lineage).
narrative_ontology:cs_interpretation_layer_present('ae4c1b43-6c03-4133-9b90-b94271a1c0c6').
narrative_ontology:cs_reading_relation('ae4c1b43-6c03-4133-9b90-b94271a1c0c6', territorial_legitimacy_dual__zionist_refuge_reading, coexists_with).
narrative_ontology:cs_reading_relation('ae4c1b43-6c03-4133-9b90-b94271a1c0c6', territorial_legitimacy_dual__palestinian_autochthony_reading, coexists_with).
narrative_ontology:cs_axiom('ae4c1b43-6c03-4133-9b90-b94271a1c0c6', foundational, mutual_national_self_determination).
narrative_ontology:cs_axiom_status(mutual_national_self_determination, holdable).
narrative_ontology:cs_axiom_grounding('ae4c1b43-6c03-4133-9b90-b94271a1c0c6', mutual_national_self_determination, deontological).
narrative_ontology:cs_axiom('ae4c1b43-6c03-4133-9b90-b94271a1c0c6', foundational, territorial_integrity_1967_lines).
narrative_ontology:cs_axiom_status(territorial_integrity_1967_lines, holdable).
narrative_ontology:cs_axiom_grounding('ae4c1b43-6c03-4133-9b90-b94271a1c0c6', territorial_integrity_1967_lines, conventional).
narrative_ontology:cs_reference_frame('ae4c1b43-6c03-4133-9b90-b94271a1c0c6', oslo_accords_framework).
narrative_ontology:cs_drift_state('ae4c1b43-6c03-4133-9b90-b94271a1c0c6', contemporary, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('ae4c1b43-6c03-4133-9b90-b94271a1c0c6', '').
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

% Actively promotes and attempts to enforce the two-state solution based on 1967 borders, viewing it as the most viable path to regional stability and international law compliance. Provides diplomatic pressure, aid, and sanctions.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__two_state_coexistence_reading, international_diplomatic_community, agenda_setter,
    institutional, generational, mobile, global).

% Benefits from the security cooperation aspect of the framework, which aims to prevent attacks and maintain stability. However, it faces internal resistance from hardline elements regarding territorial concessions.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__two_state_coexistence_reading, israeli_security_establishment, beneficiary,
    institutional, biographical, constrained, national).

% Gains international recognition and a framework for statehood, along with financial aid. However, it struggles with internal legitimacy due to perceived compromises on core Palestinian rights and faces challenges from rival factions.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__two_state_coexistence_reading, palestinian_authority_leadership, beneficiary,
    organized, biographical, constrained, regional).

% Bear the cost of a limited right of return, primarily to a future Palestinian state, rather than to their ancestral homes in Israel. Their historical claims are partially acknowledged but structurally constrained by the framework's compromises.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__two_state_coexistence_reading, palestinian_refugees, payer,
    powerless, generational, trapped, regional).

% Face displacement or integration into a Palestinian state, which they resist based on religious and historical claims to the land. Their identity is deeply tied to their presence in the settlements, making exit unthinkable.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__two_state_coexistence_reading, israeli_settlers, payer,
    organized, biographical, identity_locked, local).

% Are excluded from the diplomatic process as they reject the core premises of mutual recognition and territorial compromise. They actively resist the framework through political and sometimes violent means, viewing it as a betrayal of their national aspirations.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__two_state_coexistence_reading, hardline_factions_on_both_sides, excluded,
    organized, generational, identity_locked, regional).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To establish a framework for peaceful coexistence and mutual recognition between Israelis and Palestinians, preventing perpetual conflict and providing a basis for two sovereign states.
% TRANSFER_FUNCTION: Transfers territorial control (from Israel to a Palestinian state based on 1967 lines), limits the right of return (from Palestinian refugees to a future Palestinian state), and demands security cooperation (from both sides to the international community).
% ABSENT_VOICES: Hardline factions on both sides, who reject the premise of mutual recognition and compromise, are excluded. Palestinian refugees, whose right of return is curtailed, and Israeli settlers, facing displacement, are also largely excluded from direct negotiation, their interests mediated by their respective leaderships.
% DISAPPEARANCE_RATIONALE: If the two-state framework disappeared, the region would likely revert to intensified conflict, unilateral actions, and a complete breakdown of diplomatic efforts, leading to significant geopolitical instability and humanitarian crises. The current (unstable) equilibrium depends on this framework, however contested.
% FOUNDING_PROBLEM: The intractable conflict between Israeli and Palestinian national aspirations over the same land, leading to cycles of violence, occupation, and displacement, with no clear path to a just and lasting peace.
% FOUNDING_PROBLEM_CORROBORATION: The international diplomatic community, numerous UN resolutions, and a significant portion of the populations on both sides (as evidenced by polls and civil society movements) corroborate that the core problem of conflict resolution remains live, even if the proposed solution is contested. Independent human rights organizations also attest to the ongoing human cost of the unresolved conflict.
narrative_ontology:disappearance_verdict(territorial_legitimacy_dual__two_state_coexistence_reading, world_rearranges).
narrative_ontology:founding_problem_status(territorial_legitimacy_dual__two_state_coexistence_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(territorial_legitimacy_dual__two_state_coexistence_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(territorial_legitimacy_dual__two_state_coexistence_reading, 'none', 1).

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
 *   The extractiveness (0.4) is moderate, reflecting the compromise nature where both sides give up some claims, but the burden is not evenly distributed. Suppression (0.6) is high because the framework requires active enforcement against rejectionist elements on both sides and relies on external diplomatic pressure. Theater ratio (0.2) is relatively low, as the diplomatic efforts are genuine, though often ineffective. The cyclical nature of extractiveness and suppression reflects periods of intense diplomatic engagement and relative calm, often punctuated by conflict, which shifts the perceived costs and enforcement needs.
 *
 * PERSPECTIVAL GAP:
 *   The international diplomatic community and moderate leaderships on both sides perceive this as a necessary, if imperfect, coordination mechanism. However, Palestinian refugees and Israeli settlers experience it as a highly extractive and suppressive constraint, as it directly challenges their core claims and identities. Hardline factions view it as an illegitimate imposition.
 *
 * DIRECTIONALITY LOGIC:
 *   The international community, Israeli security establishment, and Palestinian Authority leadership are beneficiaries (d near 0.0-0.3) as they gain a framework for stability, security, and statehood, respectively. Palestinian refugees and Israeli settlers are payers (d near 0.7-1.0) as their maximalist claims are curtailed. Hardline factions are excluded (d=1.0) as the framework's existence actively suppresses their alternative visions.
 *
 * MANDATROPHY ANALYSIS:
 *   The framework's mandate (peaceful coexistence) is still live, preventing it from being a Piton. However, its effectiveness is contested, and its persistence relies on active enforcement and suppression of alternatives, rather than universal buy-in. It avoids being a pure Snare by offering genuine, albeit compromised, coordination benefits to key actors and the broader region.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    enforceability_vs_consent,
    'To what extent does the framework''s persistence rely on external enforcement and suppression of dissent, versus genuine consent and buy-in from all affected parties?',
    'Longitudinal study of compliance rates in the absence of external pressure, and surveys of public opinion on both sides regarding the framework''s legitimacy and fairness.',
    'If primarily reliant on enforcement, the framework is more extractive and less stable than claimed, potentially shifting its classification closer to a Snare. If consent is high, it moves closer to a Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforceability_vs_consent, empirical, 'Assesses the balance between coercion and voluntary acceptance.').

omega_variable(
    right_of_return_scope,
    'Is the limitation of the right of return to a Palestinian state a necessary compromise for a two-state solution, or an unjust extraction from Palestinian refugees?',
    'Conceptual analysis of international law on refugee rights versus state sovereignty, and comparative studies of post-conflict refugee integration models.',
    'If deemed an unjust extraction, the framework''s extractiveness for Palestinian refugees is higher, and the overall classification leans more towards Snare. If deemed a necessary compromise, the extractiveness is viewed as a cost of coordination.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(right_of_return_scope, conceptual, 'Examines the ethical and legal justification for limiting refugee rights within the framework.').

omega_variable(
    settler_identity_lock,
    'Is the identity-lock of Israeli settlers a genuine, irreducible commitment, or a politically constructed position that could be altered by different incentives or narratives?',
    'Sociological studies of settler communities, analysis of political discourse shaping settler identity, and examination of historical precedents for population transfers or integration.',
    'If irreducible, the framework''s cost to settlers is a fundamental, unavoidable extraction. If constructed, the ''identity_locked'' exit option is less fixed, and the potential for alternative solutions (e.g., compensation, integration) increases, potentially reducing perceived extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(settler_identity_lock, empirical, 'Investigates the nature and malleability of settler identity and its impact on the framework''s viability.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(territorial_legitimacy_dual__two_state_coexistence_reading, 1993, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(terr_tr_t1993, territorial_legitimacy_dual__two_state_coexistence_reading, theater_ratio, 1993, 0.1).
narrative_ontology:measurement(terr_tr_t2000, territorial_legitimacy_dual__two_state_coexistence_reading, theater_ratio, 2000, 0.25).
narrative_ontology:measurement(terr_tr_t2007, territorial_legitimacy_dual__two_state_coexistence_reading, theater_ratio, 2007, 0.35).
narrative_ontology:measurement(terr_tr_t2014, territorial_legitimacy_dual__two_state_coexistence_reading, theater_ratio, 2014, 0.3).
narrative_ontology:measurement(terr_tr_t2024, territorial_legitimacy_dual__two_state_coexistence_reading, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(terr_be_t1993, territorial_legitimacy_dual__two_state_coexistence_reading, base_extractiveness, 1993, 0.3).
narrative_ontology:measurement(terr_be_t2000, territorial_legitimacy_dual__two_state_coexistence_reading, base_extractiveness, 2000, 0.45).
narrative_ontology:measurement(terr_be_t2007, territorial_legitimacy_dual__two_state_coexistence_reading, base_extractiveness, 2007, 0.55).
narrative_ontology:measurement(terr_be_t2014, territorial_legitimacy_dual__two_state_coexistence_reading, base_extractiveness, 2014, 0.5).
narrative_ontology:measurement(terr_be_t2024, territorial_legitimacy_dual__two_state_coexistence_reading, base_extractiveness, 2024, 0.4).

% Suppression requirement over time
narrative_ontology:measurement(terr_su_t1993, territorial_legitimacy_dual__two_state_coexistence_reading, suppression_requirement, 1993, 0.5).
narrative_ontology:measurement(terr_su_t2000, territorial_legitimacy_dual__two_state_coexistence_reading, suppression_requirement, 2000, 0.7).
narrative_ontology:measurement(terr_su_t2007, territorial_legitimacy_dual__two_state_coexistence_reading, suppression_requirement, 2007, 0.8).
narrative_ontology:measurement(terr_su_t2014, territorial_legitimacy_dual__two_state_coexistence_reading, suppression_requirement, 2014, 0.75).
narrative_ontology:measurement(terr_su_t2024, territorial_legitimacy_dual__two_state_coexistence_reading, suppression_requirement, 2024, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(territorial_legitimacy_dual__two_state_coexistence_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(territorial_legitimacy_dual__two_state_coexistence_reading, territorial_legitimacy_dual__zionist_refuge_reading).
narrative_ontology:affects_constraint(territorial_legitimacy_dual__two_state_coexistence_reading, territorial_legitimacy_dual__palestinian_autochthony_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'territorial_legitimacy_dual' kernel. It proposes a two-state solution based on 1967 borders and mutual recognition, influencing and coexisting with other readings that emphasize different historical and legal claims.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
