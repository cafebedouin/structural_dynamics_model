% ============================================================================
% CONSTRAINT STORY: zionist_legitimacy_basis__settler_colonial_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_zionist_legitimacy_basis__settler_colonial_reading, []).

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
 *   constraint_id: zionist_legitimacy_basis__settler_colonial_reading
 *   human_readable: Zionist Legitimacy Basis: Settler-Colonial Reading
 *   domain: political_history/nationalism/settler_colonialism
 *
 * SUMMARY:
 *   This constraint models Zionism as a European settler-colonial movement,
 *   focusing on its structural role in establishing an ethno-state through
 *   the displacement and dispossession of indigenous Palestinians. The core
 *   claim is that the colonial structure is constitutive of the state's
 *   legitimacy, not an incidental outcome. This reading emphasizes the active
 *   enforcement required to maintain this structure and the high extraction
 *   from the indigenous population. The claimed type is 'snare' because the
 *   coordination narrative (Jewish self-determination) is seen as cover for a
 *   fundamentally extractive and coercive project.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(zionist_legitimacy_basis__settler_colonial_reading, 0.92).
domain_priors:suppression_score(zionist_legitimacy_basis__settler_colonial_reading, 0.95).
domain_priors:theater_ratio(zionist_legitimacy_basis__settler_colonial_reading, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(zionist_legitimacy_basis__settler_colonial_reading, extractiveness, 0.92).
narrative_ontology:constraint_metric(zionist_legitimacy_basis__settler_colonial_reading, suppression_requirement, 0.95).
narrative_ontology:constraint_metric(zionist_legitimacy_basis__settler_colonial_reading, theater_ratio, 0.65).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(zionist_legitimacy_basis__settler_colonial_reading, accessibility_collapse, 0.88).
narrative_ontology:constraint_metric(zionist_legitimacy_basis__settler_colonial_reading, resistance, 0.9).

% --- Constraint claim ---
narrative_ontology:constraint_claim(zionist_legitimacy_basis__settler_colonial_reading, snare).
narrative_ontology:human_readable(zionist_legitimacy_basis__settler_colonial_reading, "Zionist Legitimacy Basis: Settler-Colonial Reading").
narrative_ontology:topic_domain(zionist_legitimacy_basis__settler_colonial_reading, "political_history/nationalism/settler_colonialism").

domain_priors:requires_active_enforcement(zionist_legitimacy_basis__settler_colonial_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(zionist_legitimacy_basis__settler_colonial_reading, 'ceb983da-1639-41b2-ab1e-1cad7a87463b').
narrative_ontology:cs_kernel_codification('ceb983da-1639-41b2-ab1e-1cad7a87463b', formalized).
narrative_ontology:cs_authority_grounding('ceb983da-1639-41b2-ab1e-1cad7a87463b', extraction).
narrative_ontology:cs_interpretation_layer_present('ceb983da-1639-41b2-ab1e-1cad7a87463b').
narrative_ontology:cs_reading_relation('ceb983da-1639-41b2-ab1e-1cad7a87463b', zionist_legitimacy_basis__national_liberation_reading, forecloses).
narrative_ontology:cs_reading_relation('ceb983da-1639-41b2-ab1e-1cad7a87463b', zionist_legitimacy_basis__religious_restoration_reading, coexists_with).
narrative_ontology:cs_axiom('ceb983da-1639-41b2-ab1e-1cad7a87463b', foundational, colonial_settlement_is_illegitimate).
narrative_ontology:cs_axiom_status(colonial_settlement_is_illegitimate, holdable).
narrative_ontology:cs_axiom_grounding('ceb983da-1639-41b2-ab1e-1cad7a87463b', colonial_settlement_is_illegitimate, deontological).
narrative_ontology:cs_axiom('ceb983da-1639-41b2-ab1e-1cad7a87463b', foundational, indigenous_displacement_is_constitutive).
narrative_ontology:cs_axiom_status(indigenous_displacement_is_constitutive, holdable).
narrative_ontology:cs_axiom_grounding('ceb983da-1639-41b2-ab1e-1cad7a87463b', indigenous_displacement_is_constitutive, empirically_contingent).
narrative_ontology:cs_reference_frame('ceb983da-1639-41b2-ab1e-1cad7a87463b', post_colonial_critique_framework).
narrative_ontology:cs_drift_state('ceb983da-1639-41b2-ab1e-1cad7a87463b', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('ceb983da-1639-41b2-ab1e-1cad7a87463b', '').
narrative_ontology:cs_kernel_id(zionist_legitimacy_basis__settler_colonial_reading, zionist_legitimacy_basis).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(zionist_legitimacy_basis__settler_colonial_reading, israeli_state_institutions).
narrative_ontology:constraint_beneficiary(zionist_legitimacy_basis__settler_colonial_reading, jewish_israeli_citizens).
narrative_ontology:constraint_victim(zionist_legitimacy_basis__settler_colonial_reading, indigenous_palestinians).
narrative_ontology:constraint_victim(zionist_legitimacy_basis__settler_colonial_reading, palestinian_diaspora).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The state apparatus that codifies and enforces laws and policies that privilege Jewish citizens, dispossess Palestinians, and maintain control over land and resources. Its legitimacy is tied to the settler-colonial project.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__settler_colonial_reading, israeli_state_institutions, agenda_setter,
    institutional, generational, identity_locked, national).

% Benefit from preferential access to land, housing, resources, and legal protections. Their identity and security are often framed as dependent on the continuation of the ethno-state, making exit from the system difficult without perceived loss.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__settler_colonial_reading, jewish_israeli_citizens, beneficiary,
    organized, biographical, constrained, national).

% Bear the direct costs of displacement, land confiscation, denial of self-determination, and systemic discrimination. Their existence is actively suppressed within the framework of the ethno-state, with no viable exit.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__settler_colonial_reading, indigenous_palestinians, payer,
    powerless, generational, trapped, local).

% Denied the right of return and excluded from the political and territorial arrangements. Their identity is tied to their ancestral land, making 'exit' from the struggle for return a profound loss of self.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__settler_colonial_reading, palestinian_diaspora, excluded,
    moderate, generational, identity_locked, global).

% Document and condemn human rights abuses, land confiscation, and discriminatory policies. They provide critical analysis but lack direct enforcement power over the state.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__settler_colonial_reading, international_human_rights_organizations, observer,
    organized, biographical, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(zionist_legitimacy_basis__settler_colonial_reading, israeli_state_institutions).
narrative_ontology:fixing_cost_class(zionist_legitimacy_basis__settler_colonial_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the establishment and maintenance of a Jewish ethno-state by mobilizing Jewish immigration, consolidating control over land, and managing the indigenous population through displacement and control.
% TRANSFER_FUNCTION: Transfers land, resources, and political sovereignty from indigenous Palestinians to Jewish settlers and the Israeli state, along with the associated benefits of statehood and national identity.
% ABSENT_VOICES: The voices of indigenous Palestinians, particularly those displaced and living in diaspora, are systematically excluded from the foundational narratives and decision-making processes that define the state's legitimacy and territorial claims. Their historical narrative is suppressed.
% DISAPPEARANCE_RATIONALE: If the settler-colonial basis of Zionism vanished, the entire structure of the Israeli state, its land ownership, citizenship laws, and relationship with the indigenous population would fundamentally rearrange. It would necessitate a decolonization process, leading to a radically different political and social order.
% FOUNDING_PROBLEM: The problem of Jewish persecution and statelessness in Europe, which sought a territorial solution for Jewish self-determination.
% FOUNDING_PROBLEM_CORROBORATION: While the historical problem of Jewish persecution is widely acknowledged, this reading argues that the solution chosen (settler-colonialism) created a new problem of indigenous displacement. Historians and critical scholars of settler-colonialism, as well as Palestinian narratives, corroborate this framing, often in direct opposition to the state's self-justification.
narrative_ontology:disappearance_verdict(zionist_legitimacy_basis__settler_colonial_reading, world_rearranges).
narrative_ontology:founding_problem_status(zionist_legitimacy_basis__settler_colonial_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(zionist_legitimacy_basis__settler_colonial_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(zionist_legitimacy_basis__settler_colonial_reading, 'none', 1).
narrative_ontology:epsilon_provenance(zionist_legitimacy_basis__settler_colonial_reading, 0.92, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(zionist_legitimacy_basis__settler_colonial_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(zionist_legitimacy_basis__settler_colonial_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(zionist_legitimacy_basis__settler_colonial_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is very high (0.92) because the entire project is understood as a transfer of land and sovereignty from one population to another. Suppression is also very high (0.95) due to the ongoing military occupation, legal discrimination, and denial of rights necessary to maintain the ethno-state. Theater ratio is high (0.65) as the state's actions are often framed as defensive or for security, while the underlying function is seen as maintaining colonial control. The historical measurements show a sharp increase in extractiveness and suppression around 1948 (Nakba) and 1967 (occupation of West Bank/Gaza), reflecting key moments of territorial expansion and indigenous displacement.
 *
 * PERSPECTIVAL GAP:
 *   The settler-colonial reading fundamentally diverges from national liberation or religious restoration readings. From the perspective of the Israeli state and many Jewish Israelis, the project is one of self-determination and return to an ancestral homeland. From the settler-colonial reading, this narrative is a justification for an extractive and oppressive system, and the 'coordination' it provides is for the benefit of the colonizers at the expense of the colonized. The engine's classification will highlight this structural asymmetry.
 *
 * DIRECTIONALITY LOGIC:
 *   Israeli state institutions and Jewish Israeli citizens are the primary beneficiaries, gaining land, security, and national identity from the constraint (low directionality). Indigenous Palestinians and the Palestinian diaspora are the primary targets, experiencing dispossession, statelessness, and denial of rights (high directionality). International human rights organizations act as observers, documenting the structural violence.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    historical_causality_of_displacement,
    'To what extent was indigenous displacement an inherent, constitutive outcome of the Zionist project from its inception, versus a contingent outcome of specific historical conflicts?',
    'Detailed historical analysis of early Zionist planning documents, land acquisition strategies, and demographic goals, compared with the actual events of 1948 and subsequent periods.',
    'If displacement was constitutive, it strengthens the settler-colonial framing and the high extractiveness/suppression. If largely contingent, it might allow for a reading where the initial project was less inherently extractive, though still resulting in displacement.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(historical_causality_of_displacement, empirical, 'Determining if displacement was an inherent goal or a contingent outcome.').

omega_variable(
    legitimacy_of_self_determination_vs_colonialism,
    'Can a national liberation movement for one people simultaneously constitute a settler-colonial project for another, or are these categories mutually exclusive in this context?',
    'Conceptual analysis of post-colonial theory and international law regarding indigenous rights and self-determination, applied to the specific historical and demographic context.',
    'If mutually exclusive, this reading''s ''snare'' classification is strongly affirmed, foreclosing the national liberation reading. If they can coexist, it introduces complexity, potentially shifting the classification towards a ''tangled_rope'' for some seats, acknowledging a dual function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legitimacy_of_self_determination_vs_colonialism, conceptual, 'Conceptual tension between national liberation and settler-colonialism.').

omega_variable(
    internalized_suppression_among_palestinians,
    'Beyond structural barriers, to what extent has the prolonged occupation and displacement led to internalized suppression or psychological impacts that hinder collective resistance?',
    'Sociological and psychological studies on the effects of prolonged conflict, trauma, and dispossession on collective agency and identity formation within Palestinian communities.',
    'If internalized suppression is significant, the effective suppression is higher than structural measures suggest, as it persists even if external barriers are theoretically removed. This would deepen the ''snare'' classification by highlighting the profound impact on victims'' agency.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(internalized_suppression_among_palestinians, empirical, 'Structural vs. internalized suppression mechanism for Palestinians.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(zionist_legitimacy_basis__settler_colonial_reading, 1900, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(zion_tr_t1900, zionist_legitimacy_basis__settler_colonial_reading, theater_ratio, 1900, 0.1).
narrative_ontology:measurement(zion_tr_t1920, zionist_legitimacy_basis__settler_colonial_reading, theater_ratio, 1920, 0.25).
narrative_ontology:measurement(zion_tr_t1948, zionist_legitimacy_basis__settler_colonial_reading, theater_ratio, 1948, 0.4).
narrative_ontology:measurement(zion_tr_t1967, zionist_legitimacy_basis__settler_colonial_reading, theater_ratio, 1967, 0.6).
narrative_ontology:measurement(zion_tr_t2000, zionist_legitimacy_basis__settler_colonial_reading, theater_ratio, 2000, 0.68).
narrative_ontology:measurement(zion_tr_t2024, zionist_legitimacy_basis__settler_colonial_reading, theater_ratio, 2024, 0.65).

% Extraction over time
narrative_ontology:measurement(zion_be_t1900, zionist_legitimacy_basis__settler_colonial_reading, base_extractiveness, 1900, 0.6).
narrative_ontology:measurement(zion_be_t1920, zionist_legitimacy_basis__settler_colonial_reading, base_extractiveness, 1920, 0.7).
narrative_ontology:measurement(zion_be_t1948, zionist_legitimacy_basis__settler_colonial_reading, base_extractiveness, 1948, 0.9).
narrative_ontology:measurement(zion_be_t1967, zionist_legitimacy_basis__settler_colonial_reading, base_extractiveness, 1967, 0.95).
narrative_ontology:measurement(zion_be_t2000, zionist_legitimacy_basis__settler_colonial_reading, base_extractiveness, 2000, 0.93).
narrative_ontology:measurement(zion_be_t2024, zionist_legitimacy_basis__settler_colonial_reading, base_extractiveness, 2024, 0.92).

% Suppression requirement over time
narrative_ontology:measurement(zion_su_t1900, zionist_legitimacy_basis__settler_colonial_reading, suppression_requirement, 1900, 0.5).
narrative_ontology:measurement(zion_su_t1920, zionist_legitimacy_basis__settler_colonial_reading, suppression_requirement, 1920, 0.65).
narrative_ontology:measurement(zion_su_t1948, zionist_legitimacy_basis__settler_colonial_reading, suppression_requirement, 1948, 0.9).
narrative_ontology:measurement(zion_su_t1967, zionist_legitimacy_basis__settler_colonial_reading, suppression_requirement, 1967, 0.98).
narrative_ontology:measurement(zion_su_t2000, zionist_legitimacy_basis__settler_colonial_reading, suppression_requirement, 2000, 0.96).
narrative_ontology:measurement(zion_su_t2024, zionist_legitimacy_basis__settler_colonial_reading, suppression_requirement, 2024, 0.95).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(zionist_legitimacy_basis__settler_colonial_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(zionist_legitimacy_basis__settler_colonial_reading, zionist_legitimacy_basis__national_liberation_reading).
narrative_ontology:affects_constraint(zionist_legitimacy_basis__settler_colonial_reading, zionist_legitimacy_basis__religious_restoration_reading).
narrative_ontology:affects_constraint(zionist_legitimacy_basis__settler_colonial_reading, israeli_citizenship_law).
narrative_ontology:affects_constraint(zionist_legitimacy_basis__settler_colonial_reading, palestinian_right_of_return).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'zionist_legitimacy_basis' kernel. This settler-colonial reading emphasizes indigenous displacement and colonial structure, contrasting with national liberation and religious restoration framings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
