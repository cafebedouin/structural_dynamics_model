% ============================================================================
% CONSTRAINT STORY: jewish_sovereignty_palestine__religious_zionist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jewish_sovereignty_palestine__religious_zionist_reading, []).

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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: jewish_sovereignty_palestine__religious_zionist_reading
 *   human_readable: Divine Promise of Eretz Yisrael (Religious Zionist Reading)
 *   domain: political_philosophy/nationalism_studies/postcolonial_theory
 *
 * SUMMARY:
 *   This constraint story instantiates the 'religious Zionist reading' of the
 *   contested kernel 'jewish_sovereignty_palestine'. It describes the belief
 *   that the divine promise of Eretz Yisrael to the Jewish people constitutes
 *   an inalienable territorial claim, and that the establishment and
 *   expansion of the State of Israel is a theological fulfillment. From this
 *   reading's perspective, the claim is a fundamental, unchangeable truth,
 *   hence the 'mountain' claimed_type. However, its operation involves very
 *   high extraction and suppression of the Palestinian people, leading to a
 *   likely 'false summit' classification by the engine.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jewish_sovereignty_palestine__religious_zionist_reading, 0.92).
domain_priors:suppression_score(jewish_sovereignty_palestine__religious_zionist_reading, 0.95).
domain_priors:theater_ratio(jewish_sovereignty_palestine__religious_zionist_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__religious_zionist_reading, extractiveness, 0.92).
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__religious_zionist_reading, suppression_requirement, 0.95).
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__religious_zionist_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__religious_zionist_reading, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__religious_zionist_reading, resistance, 0.88).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jewish_sovereignty_palestine__religious_zionist_reading, mountain).
narrative_ontology:human_readable(jewish_sovereignty_palestine__religious_zionist_reading, "Divine Promise of Eretz Yisrael (Religious Zionist Reading)").
narrative_ontology:topic_domain(jewish_sovereignty_palestine__religious_zionist_reading, "political_philosophy/nationalism_studies/postcolonial_theory").

domain_priors:requires_active_enforcement(jewish_sovereignty_palestine__religious_zionist_reading).
domain_priors:emerges_naturally(jewish_sovereignty_palestine__religious_zionist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jewish_sovereignty_palestine__religious_zionist_reading, '496a4856-2b67-4624-af13-6416d49ff717').
narrative_ontology:cs_kernel_codification('496a4856-2b67-4624-af13-6416d49ff717', fixed_text).
narrative_ontology:cs_authority_grounding('496a4856-2b67-4624-af13-6416d49ff717', lineage).
narrative_ontology:cs_interpretation_layer_present('496a4856-2b67-4624-af13-6416d49ff717').
narrative_ontology:cs_reading_relation('496a4856-2b67-4624-af13-6416d49ff717', jewish_sovereignty_palestine__liberal_nationalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('496a4856-2b67-4624-af13-6416d49ff717', jewish_sovereignty_palestine__settler_colonial_reading, forecloses).
narrative_ontology:cs_reading_relation('496a4856-2b67-4624-af13-6416d49ff717', jewish_sovereignty_palestine__cultural_zionist_reading, influences).
narrative_ontology:cs_reading_relation('496a4856-2b67-4624-af13-6416d49ff717', jewish_sovereignty_palestine__post_zionist_reading, forecloses).
narrative_ontology:cs_axiom('496a4856-2b67-4624-af13-6416d49ff717', foundational, divine_land_grant_inalienable).
narrative_ontology:cs_axiom_status(divine_land_grant_inalienable, holdable).
narrative_ontology:cs_axiom_grounding('496a4856-2b67-4624-af13-6416d49ff717', divine_land_grant_inalienable, theological).
narrative_ontology:cs_axiom('496a4856-2b67-4624-af13-6416d49ff717', foundational, jewish_sovereignty_theological_fulfillment).
narrative_ontology:cs_axiom_status(jewish_sovereignty_theological_fulfillment, holdable).
narrative_ontology:cs_axiom_grounding('496a4856-2b67-4624-af13-6416d49ff717', jewish_sovereignty_theological_fulfillment, theological).
narrative_ontology:cs_reference_frame('496a4856-2b67-4624-af13-6416d49ff717', covenantal_land_inheritance).
narrative_ontology:cs_drift_state('496a4856-2b67-4624-af13-6416d49ff717', contemporary_political_reality, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('496a4856-2b67-4624-af13-6416d49ff717', '').
narrative_ontology:cs_kernel_id(jewish_sovereignty_palestine__religious_zionist_reading, jewish_sovereignty_palestine).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jewish_sovereignty_palestine__religious_zionist_reading, jewish_people_as_covenant_community).
narrative_ontology:constraint_beneficiary(jewish_sovereignty_palestine__religious_zionist_reading, state_of_israel_religious_institutions).
narrative_ontology:constraint_victim(jewish_sovereignty_palestine__religious_zionist_reading, palestinian_people).
narrative_ontology:constraint_victim(jewish_sovereignty_palestine__religious_zionist_reading, secular_israelis_seeking_peace).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% As the recipients of the divine promise, their collective identity and spiritual fulfillment are seen as intrinsically tied to the land of Eretz Yisrael. They benefit from the political realization of this claim.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__religious_zionist_reading, jewish_people_as_covenant_community, beneficiary,
    institutional, generational, identity_locked, universal).

% These institutions interpret, promote, and actively work to implement the theological claim of divine land ownership, influencing state policy, settlement expansion, and legal frameworks. They derive significant legitimacy and power from this role.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__religious_zionist_reading, state_of_israel_religious_institutions, agenda_setter,
    institutional, generational, constrained, national).

% The indigenous population whose land, self-determination, and national aspirations are directly denied and suppressed by the theological claim of exclusive divine ownership. They bear the primary costs of displacement, occupation, and statelessness.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__religious_zionist_reading, palestinian_people, payer,
    powerless, generational, trapped, regional).

% While part of the benefiting state, they bear the costs of ongoing conflict, international isolation, and internal social division stemming from the maximalist territorial claim. Their vision of a secure, democratic state is challenged by the theological imperative.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__religious_zionist_reading, secular_israelis_seeking_peace, payer,
    moderate, biographical, constrained, national).

% These bodies (e.g., UN, ICJ) assess the conflict based on international law, human rights, and self-determination principles, which often directly contradict the theological claim of exclusive divine right. Their rulings are frequently rejected by adherents of this reading.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__religious_zionist_reading, international_law_bodies, observer,
    institutional, civilizational, analytical, global).

% Advocate for a two-state solution, civic equality, and a more secular, democratic vision for Israel. Their perspectives are often marginalized or actively opposed by the religious Zionist movement, which views their positions as undermining the divine mandate.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__religious_zionist_reading, liberal_zionists, excluded,
    moderate, biographical, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(jewish_sovereignty_palestine__religious_zionist_reading, state_of_israel_religious_institutions).
narrative_ontology:fixing_cost_class(jewish_sovereignty_palestine__religious_zionist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Unifies a segment of the Jewish people around a shared, absolute theological-territorial vision for Eretz Yisrael, providing a coherent framework for national identity and political action.
% TRANSFER_FUNCTION: Justifies the transfer of land, sovereignty, and resources from the Palestinian people to the Jewish people, based on a claim of divine title and historical covenant.
% ABSENT_VOICES: Palestinian voices, international legal frameworks, and secular/liberal Jewish perspectives that challenge the theological basis of the claim are structurally excluded or dismissed as illegitimate, preventing any negotiation or alternative framing of territorial rights.
% DISAPPEARANCE_RATIONALE: If the divine promise as an inalienable territorial claim vanished overnight, the foundational legitimacy of the State of Israel, as understood by this reading, would collapse. The entire political, legal, and social structure supporting settlement expansion and denial of Palestinian rights would be fundamentally undermined, leading to a radical reorganization of the conflict and the region.
% FOUNDING_PROBLEM: The historical exile, persecution, and vulnerability of the Jewish people, coupled with the theological imperative to return to and reclaim Eretz Yisrael as their divinely promised homeland.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem is attested by religious texts, rabbinic tradition, and historical narratives within the religious Zionist community. External corroboration is contested by international bodies and Palestinian historians, who offer alternative historical and legal framings.
narrative_ontology:disappearance_verdict(jewish_sovereignty_palestine__religious_zionist_reading, world_rearranges).
narrative_ontology:founding_problem_status(jewish_sovereignty_palestine__religious_zionist_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jewish_sovereignty_palestine__religious_zionist_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(jewish_sovereignty_palestine__religious_zionist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jewish_sovereignty_palestine__religious_zionist_reading, 0.92, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jewish_sovereignty_palestine__religious_zionist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(jewish_sovereignty_palestine__religious_zionist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(jewish_sovereignty_palestine__religious_zionist_reading, ExtMetricName, E),
    domain_priors:suppression_score(jewish_sovereignty_palestine__religious_zionist_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(jewish_sovereignty_palestine__religious_zionist_reading),
    narrative_ontology:constraint_metric(jewish_sovereignty_palestine__religious_zionist_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(jewish_sovereignty_palestine__religious_zionist_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(jewish_sovereignty_palestine__religious_zionist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is very high (0.92) because the claim justifies maximalist territorial control and denies the self-determination of another people, leading to significant material and existential costs for the victims. Suppression is extremely high (0.95) as the claim is actively enforced through military, legal, and political means, with severe restrictions on Palestinian movement, land use, and political expression. Theater ratio is low (0.10) because the theological claim is genuinely held and forms a core ideological driver for action; there is little performative maintenance without real function. Accessibility collapse is high (0.90) as the divine nature of the claim is presented as non-negotiable, severely limiting alternatives for resolution. Resistance is high (0.88) due to the ongoing, active opposition from the Palestinian people and international bodies.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the religious Zionist reading, the constraint is a divinely ordained truth, a 'mountain' that simply exists. From the perspective of the Palestinian people, it is an actively enforced 'snare' that extracts their land and suppresses their existence. The engine's classification will highlight this divergence between the claimed naturalness and the observed extractive operation.
 *
 * DIRECTIONALITY LOGIC:
 *   The Jewish people as a covenant community and the State of Israel's religious institutions are the primary beneficiaries, gaining land, sovereignty, and legitimacy from the claim. The Palestinian people are the primary targets/victims, bearing the costs of displacement and denial of rights. Secular Israelis seeking peace also bear costs in terms of conflict and international standing. International law bodies and liberal Zionists are observers or excluded, as their frameworks often conflict with the theological claim.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    divine_mandate_empirical_status,
    'Is the divine mandate for Eretz Yisrael an empirically verifiable truth, a theological belief, or a political claim framed as divine?',
    'Analysis of the claim''s epistemic grounding: if its force depends on non-falsifiable theological tenets, it is a belief; if it is used to justify political actions with material consequences, it functions as a political claim. No empirical test can ''disprove'' a divine mandate, but its operationalization can be analyzed.',
    'If purely theological, its ''mountain'' claim is internally consistent but externally uncorroborated, triggering FSM. If primarily a political claim, its ''mountain'' framing is a cover story for a Snare or Tangled Rope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(divine_mandate_empirical_status, conceptual, 'Ambiguity of the divine mandate''s epistemic status.').

omega_variable(
    territorial_maximalism_vs_security,
    'To what extent is the territorial maximalism driven by the theological imperative of divine fulfillment versus pragmatic security concerns?',
    'Analysis of policy decisions and public statements: if territorial expansion continues even when security arguments are weak or counterproductive, the theological driver is dominant. If expansion halts or reverses in response to security assessments, pragmatic concerns are stronger.',
    'If primarily theological, the constraint''s extractiveness is inherent to the reading. If primarily security-driven, the constraint might be re-evaluated as a Tangled Rope with a coordination function (security) and extraction (territorial control).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(territorial_maximalism_vs_security, empirical, 'Drivers of territorial maximalism: theology vs. security.').

omega_variable(
    indigenous_rights_vs_divine_title,
    'How do the claims of indigenous rights and self-determination for the Palestinian people interact with the theological claim of divine title?',
    'Conceptual analysis of legal and ethical frameworks: can both claims be simultaneously upheld within a single framework, or does one logically foreclose the other? This is a question of legal and moral philosophy, not empirical data.',
    'If the divine title logically forecloses indigenous rights, the constraint is inherently extractive and suppressive. If a framework could accommodate both, the constraint''s current operation is a choice, not an inevitability, potentially reclassifying it as a Snare.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(indigenous_rights_vs_divine_title, conceptual, 'Compatibility of divine title with indigenous rights.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jewish_sovereignty_palestine__religious_zionist_reading, 1948, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jewi_tr_t1948, jewish_sovereignty_palestine__religious_zionist_reading, theater_ratio, 1948, 0.15).
narrative_ontology:measurement(jewi_tr_t1967, jewish_sovereignty_palestine__religious_zionist_reading, theater_ratio, 1967, 0.12).
narrative_ontology:measurement(jewi_tr_t1987, jewish_sovereignty_palestine__religious_zionist_reading, theater_ratio, 1987, 0.1).
narrative_ontology:measurement(jewi_tr_t2000, jewish_sovereignty_palestine__religious_zionist_reading, theater_ratio, 2000, 0.1).
narrative_ontology:measurement(jewi_tr_t2014, jewish_sovereignty_palestine__religious_zionist_reading, theater_ratio, 2014, 0.09).
narrative_ontology:measurement(jewi_tr_t2024, jewish_sovereignty_palestine__religious_zionist_reading, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(jewi_be_t1948, jewish_sovereignty_palestine__religious_zionist_reading, base_extractiveness, 1948, 0.8).
narrative_ontology:measurement(jewi_be_t1967, jewish_sovereignty_palestine__religious_zionist_reading, base_extractiveness, 1967, 0.85).
narrative_ontology:measurement(jewi_be_t1987, jewish_sovereignty_palestine__religious_zionist_reading, base_extractiveness, 1987, 0.88).
narrative_ontology:measurement(jewi_be_t2000, jewish_sovereignty_palestine__religious_zionist_reading, base_extractiveness, 2000, 0.9).
narrative_ontology:measurement(jewi_be_t2014, jewish_sovereignty_palestine__religious_zionist_reading, base_extractiveness, 2014, 0.91).
narrative_ontology:measurement(jewi_be_t2024, jewish_sovereignty_palestine__religious_zionist_reading, base_extractiveness, 2024, 0.92).

% Suppression requirement over time
narrative_ontology:measurement(jewi_su_t1948, jewish_sovereignty_palestine__religious_zionist_reading, suppression_requirement, 1948, 0.8).
narrative_ontology:measurement(jewi_su_t1967, jewish_sovereignty_palestine__religious_zionist_reading, suppression_requirement, 1967, 0.88).
narrative_ontology:measurement(jewi_su_t1987, jewish_sovereignty_palestine__religious_zionist_reading, suppression_requirement, 1987, 0.9).
narrative_ontology:measurement(jewi_su_t2000, jewish_sovereignty_palestine__religious_zionist_reading, suppression_requirement, 2000, 0.92).
narrative_ontology:measurement(jewi_su_t2014, jewish_sovereignty_palestine__religious_zionist_reading, suppression_requirement, 2014, 0.94).
narrative_ontology:measurement(jewi_su_t2024, jewish_sovereignty_palestine__religious_zionist_reading, suppression_requirement, 2024, 0.95).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jewish_sovereignty_palestine__religious_zionist_reading, identity_coordination).
narrative_ontology:affects_constraint(jewish_sovereignty_palestine__religious_zionist_reading, israeli_settlement_policy).
narrative_ontology:affects_constraint(jewish_sovereignty_palestine__religious_zionist_reading, palestinian_right_of_return).
narrative_ontology:affects_constraint(jewish_sovereignty_palestine__religious_zionist_reading, israeli_citizenship_law).

% DUAL FORMULATION NOTE:
% This constraint is one of multiple readings of the 'jewish_sovereignty_palestine' kernel, each representing a distinct structural claim. This reading emphasizes divine mandate and theological fulfillment, leading to high extraction and suppression.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
