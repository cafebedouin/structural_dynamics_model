% ============================================================================
% CONSTRAINT STORY: jewish_sovereignty_palestine__religious_zionist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
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
 *   human_readable: Religious Zionist Divine Title Reading of Eretz Yisrael
 *   domain: political/philosophical/nationalism
 *
 * SUMMARY:
 *   This constraint instantiates the religious Zionist reading of the Jewish
 *   sovereignty kernel: the theological claim that the Land of Israel (Eretz
 *   Yisrael) was promised by divine covenant to the Jewish people, rendering
 *   the territory inalienable and non-negotiable. Statehood is interpreted
 *   not merely as a political achievement but as theological fulfillment of
 *   that promise. Within this reading, Palestinian Arab presence and claims
 *   are structurally subordinated or erased from the legitimate
 *   beneficiary/victim calculus; the land is held by divine title that cannot
 *   be morally partitioned or relinquished. The constraint operates as a
 *   commitment system anchored in a fixed textual kernel interpreted through
 *   a lineage of religious Zionist rabbinic authority. It generates genuine
 *   coordination for the Jewish covenant community while enforcing asymmetric
 *   extraction on the Palestinian population through active military, legal,
 *   and settlement apparatus.
 *
 * KEY AGENTS:
 *   - religious_zionist_institutions: Agenda-setter (institutional/identity_locked) â administers theological-political enforcement
 *   - jewish_covenanted_community: Primary beneficiary (organized/identity_locked) â receives territorial-sovereign fulfillment
 *   - palestinian_arabs: Primary target (powerless/trapped) â bears dispossession and exclusion
 *   - israeli_secular_dissenters: Excluded voice (moderate/mobile) â marginalized within the theological framework
 *   - international_human_rights_bodies: Analytical observer (institutional/analytical) â contests the constraint via universal rights frameworks
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jewish_sovereignty_palestine__religious_zionist_reading, 0.88).
domain_priors:suppression_score(jewish_sovereignty_palestine__religious_zionist_reading, 0.85).
domain_priors:theater_ratio(jewish_sovereignty_palestine__religious_zionist_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__religious_zionist_reading, extractiveness, 0.88).
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__religious_zionist_reading, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__religious_zionist_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__religious_zionist_reading, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__religious_zionist_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jewish_sovereignty_palestine__religious_zionist_reading, tangled_rope).
narrative_ontology:human_readable(jewish_sovereignty_palestine__religious_zionist_reading, "Religious Zionist Divine Title Reading of Eretz Yisrael").
narrative_ontology:topic_domain(jewish_sovereignty_palestine__religious_zionist_reading, "political/philosophical/nationalism").

domain_priors:requires_active_enforcement(jewish_sovereignty_palestine__religious_zionist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jewish_sovereignty_palestine__religious_zionist_reading, '12eb803e-1d16-4425-9668-3c40b917f678').
narrative_ontology:cs_kernel_codification('12eb803e-1d16-4425-9668-3c40b917f678', fixed_text).
narrative_ontology:cs_authority_grounding('12eb803e-1d16-4425-9668-3c40b917f678', lineage).
narrative_ontology:cs_interpretation_layer_present('12eb803e-1d16-4425-9668-3c40b917f678').
narrative_ontology:cs_reading_relation('12eb803e-1d16-4425-9668-3c40b917f678', jewish_sovereignty_palestine__liberal_nationalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('12eb803e-1d16-4425-9668-3c40b917f678', jewish_sovereignty_palestine__cultural_zionist_reading, influences).
narrative_ontology:cs_reading_relation('12eb803e-1d16-4425-9668-3c40b917f678', jewish_sovereignty_palestine__post_zionist_reading, forecloses).
narrative_ontology:cs_reading_relation('12eb803e-1d16-4425-9668-3c40b917f678', jewish_sovereignty_palestine__settler_colonial_reading, coexists_with).
narrative_ontology:cs_axiom('12eb803e-1d16-4425-9668-3c40b917f678', foundational, divine_inalienable_grant_eretz_yisrael).
narrative_ontology:cs_axiom_status(divine_inalienable_grant_eretz_yisrael, holdable).
narrative_ontology:cs_axiom_grounding('12eb803e-1d16-4425-9668-3c40b917f678', divine_inalienable_grant_eretz_yisrael, theological).
narrative_ontology:cs_axiom('12eb803e-1d16-4425-9668-3c40b917f678', foundational, jewish_statehood_as_theological_fulfillment).
narrative_ontology:cs_axiom_status(jewish_statehood_as_theological_fulfillment, holdable).
narrative_ontology:cs_axiom_grounding('12eb803e-1d16-4425-9668-3c40b917f678', jewish_statehood_as_theological_fulfillment, theological).
narrative_ontology:cs_reference_frame('12eb803e-1d16-4425-9668-3c40b917f678', divine_promise_fulfillment_state).
narrative_ontology:cs_drift_state('12eb803e-1d16-4425-9668-3c40b917f678', post_1967_territorial_expansion, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('12eb803e-1d16-4425-9668-3c40b917f678', '').
narrative_ontology:cs_kernel_id(jewish_sovereignty_palestine__religious_zionist_reading, jewish_sovereignty_palestine).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jewish_sovereignty_palestine__religious_zionist_reading, jewish_covenanted_community).
narrative_ontology:constraint_victim(jewish_sovereignty_palestine__religious_zionist_reading, palestinian_arabs).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administer and enforce the divine territorial claim through state policy, religious law, settlement planning, and military-civilian coordination. They set the theological-political agenda that declares the land inalienable and Palestinian counter-claims illegitimate, interpreting scripture through a lineage of rabbinic authority.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__religious_zionist_reading, religious_zionist_institutions, agenda_setter,
    institutional, generational, identity_locked, national).

% Receive collective theological-political fulfillment, statehood, and territorial control justified by divine covenant. Their national-religious identity is constituted through attachment to the land as patrimony; exit from this framework means abandoning a core collective redemptive narrative.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__religious_zionist_reading, jewish_covenanted_community, beneficiary,
    organized, generational, identity_locked, national).

% Bear the costs of territorial dispossession, military occupation, settlement encirclement, and denial of self-determination. Their presence is treated as subordinate to the divine title, and their political claims are structurally excluded from legitimacy. Exit is blocked by military and legal barriers; remaining means living under a sovereignty framework that explicitly excludes them.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__religious_zionist_reading, palestinian_arabs, payer,
    powerless, biographical, trapped, local).

% Israeli Jews and civic organizations who reject divine territorial maximalism and support partition or civic equality. They are marginalized within the religious Zionist political framework but retain democratic voice and physical exit options unavailable to Palestinians.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__religious_zionist_reading, israeli_secular_dissenters, excluded,
    moderate, biographical, mobile, national).

% Monitor and contest the constraint through international law and human rights frameworks. They document the structural exclusion of Palestinians and challenge the compatibility of theological non-negotiability with universal rights norms.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__religious_zionist_reading, international_human_rights_bodies, observer,
    institutional, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(jewish_sovereignty_palestine__religious_zionist_reading, jewish_covenanted_community).
narrative_ontology:fixing_cost_class(jewish_sovereignty_palestine__religious_zionist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates Jewish national-religious collective existence and state-building around a shared covenantal narrative that obviates ongoing negotiation over the land's status, providing a unified theological-political framework for diaspora ingathering and territorial settlement.
% TRANSFER_FUNCTION: Moves territorial control, political sovereignty, and demographic dominance from the Palestinian resident population to the Jewish covenant community, justified by an inalienable divine title that brooks no partition or relinquishment.
% ABSENT_VOICES: Palestinians whose presence is theologized as temporary or irrelevant to the covenant; secular and post-Zionist Jewish voices who reject divine territorial maximalism; liberal nationalist voices who accept Jewish statehood but reject the inalienable land theology as a barrier to peace.
% DISAPPEARANCE_RATIONALE: If the divine inalienable title claim vanished overnight, the theological barrier to territorial partition and Palestinian sovereignty would collapse. The Israeli polity would likely rearrange toward negotiated borders, binational frameworks, or civic equality models, and the religious Zionist institutional architecture would lose its foundational justification.
% FOUNDING_PROBLEM: Jewish statelessness and vulnerability in diaspora; the theological-historical claim that Eretz Yisrael is the exclusive legitimate locus of Jewish self-determination and collective redemption.
% FOUNDING_PROBLEM_CORROBORATION: Religious Zionist rabbinic authorities and institutions attest the founding problem from within the beneficiary community. No external, non-beneficiary corroborating source affirms the theological founding problem as a live justification for the current territorial arrangement; international law and human rights frameworks affirm Palestinian self-determination rather than the divine-title claim.
narrative_ontology:disappearance_verdict(jewish_sovereignty_palestine__religious_zionist_reading, world_rearranges).
narrative_ontology:founding_problem_status(jewish_sovereignty_palestine__religious_zionist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jewish_sovereignty_palestine__religious_zionist_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(jewish_sovereignty_palestine__religious_zionist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jewish_sovereignty_palestine__religious_zionist_reading, 0.88, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jewish_sovereignty_palestine__religious_zionist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(jewish_sovereignty_palestine__religious_zionist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(jewish_sovereignty_palestine__religious_zionist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is very high (0.88) because the constraint categorically denies Palestinian territorial claims and legitimizes ongoing dispossession under a non-negotiable divine title. Suppression is high (0.85) because the arrangement requires active military occupation, settlement expansion, and legal regimes that suppress Palestinian self-determination alternatives. Theater ratio is moderate (0.40): much of the theological commitment is genuine and deeply held, but state rituals (biblical citations in policy, religious tourism as sovereignty performance) add performative maintenance that exceeds pure functional necessity. Accessibility collapse is high (0.80) for Palestinians because the divine-title framing makes partition or equal citizenship structurally illegitimate; resistance is high (0.70) due to sustained Palestinian opposition and international condemnation.
 *
 * PERSPECTIVAL GAP:
 *   From the religious Zionist institutional seat, the constraint is experienced as covenantal fidelity and national salvation â a rope-like coordination of Jewish destiny. From the Palestinian seat, the identical structure is experienced as totalizing extraction and exclusion â a snare. From the secular Israeli seat, it is experienced as a mix of security coordination and unwanted theological coercion. The engine computes these divergent seat classifications from the same structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   The Jewish covenanted community is the declared beneficiary (low d, subsidized by the constraint's territorial allocation). Religious Zionist institutions administer the constraint and are locked into its theological logic (moderate d, subsidized by power but constrained by their own identity commitments). Palestinian Arabs are the declared victims (high d, full targets): they bear the extraction of dispossession and exclusion with exit blocked by military and legal barriers. Israeli secular dissenters are excluded from the theological calculus but retain mobile exit options. International observers sit at analytical distance.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint prevents mislabeling by requiring both coordination and extraction markers. A pure mountain reading would be false: the territorial claim is not a natural law but a constructed theological-political commitment with identifiable beneficiaries and victims. A pure rope reading would be false because Palestinian victims are structurally necessary to the arrangement's operation. A pure snare reading would undercount the genuine coordination the constraint provides to the Jewish community. Tangled rope captures the hybrid: it coordinates one community through the same structure that extracts from another, and requires active enforcement to hold.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'Does the religious Zionist reading exhaust the legitimate interpretation of Jewish territorial claims, or does it compete with structurally distinct sibling readings?',
    'Comparative analysis of the five sibling readings'' axioms, beneficiary structures, and empirical predictions.',
    'If the reading is one of several coherent framings, its epsilon is reading-specific and the kernel must be modeled as a constraint family; if it is the only coherent framing, the kernel collapses to this single constraint.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Whether this constraint is one reading of a contested kernel or the sole valid framing.').

omega_variable(
    theological_belief_vs_instrumental_extraction,
    'Is the constraint''s persistence driven primarily by genuine theological conviction within the Jewish community, or by the material and political benefits that accrue to the institutions administering the territorial claim?',
    'Longitudinal analysis of institutional behavior when theological mandates conflict with material interests; survey of rank-and-file adherence versus elite capture.',
    'If instrumental extraction dominates, classification shifts toward snare; if genuine coordination dominates, tangled_rope is robust.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(theological_belief_vs_instrumental_extraction, empirical, 'Genuine belief versus instrumental extraction in constraint persistence.').

omega_variable(
    palestinian_exclusion_mechanism,
    'Are Palestinians structurally excluded from the constraint''s beneficiary/victim calculus by theological design, or by political expedience layered atop theology?',
    'Textual analysis of foundational religious Zionist texts versus subsequent political justifications; comparison with pre-state religious Zionist positions on Arab presence.',
    'If exclusion is by theological design, the constraint''s suppression is inherent and stable; if by expedience, the constraint may be more mutable than it appears.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(palestinian_exclusion_mechanism, conceptual, 'Whether Palestinian exclusion is theological or instrumental.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jewish_sovereignty_palestine__religious_zionist_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jzsrzr_tr_t0, jewish_sovereignty_palestine__religious_zionist_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(jzsrzr_tr_t10, jewish_sovereignty_palestine__religious_zionist_reading, theater_ratio, 10, 0.28).
narrative_ontology:measurement(jzsrzr_tr_t20, jewish_sovereignty_palestine__religious_zionist_reading, theater_ratio, 20, 0.3).
narrative_ontology:measurement(jzsrzr_tr_t30, jewish_sovereignty_palestine__religious_zionist_reading, theater_ratio, 30, 0.33).
narrative_ontology:measurement(jzsrzr_tr_t40, jewish_sovereignty_palestine__religious_zionist_reading, theater_ratio, 40, 0.37).
narrative_ontology:measurement(jzsrzr_tr_t50, jewish_sovereignty_palestine__religious_zionist_reading, theater_ratio, 50, 0.4).

% Extraction over time
narrative_ontology:measurement(jzsrzr_be_t0, jewish_sovereignty_palestine__religious_zionist_reading, base_extractiveness, 0, 0.72).
narrative_ontology:measurement(jzsrzr_be_t10, jewish_sovereignty_palestine__religious_zionist_reading, base_extractiveness, 10, 0.76).
narrative_ontology:measurement(jzsrzr_be_t20, jewish_sovereignty_palestine__religious_zionist_reading, base_extractiveness, 20, 0.8).
narrative_ontology:measurement(jzsrzr_be_t30, jewish_sovereignty_palestine__religious_zionist_reading, base_extractiveness, 30, 0.84).
narrative_ontology:measurement(jzsrzr_be_t40, jewish_sovereignty_palestine__religious_zionist_reading, base_extractiveness, 40, 0.86).
narrative_ontology:measurement(jzsrzr_be_t50, jewish_sovereignty_palestine__religious_zionist_reading, base_extractiveness, 50, 0.88).

% Suppression requirement over time
narrative_ontology:measurement(jzsrzr_su_t0, jewish_sovereignty_palestine__religious_zionist_reading, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(jzsrzr_su_t10, jewish_sovereignty_palestine__religious_zionist_reading, suppression_requirement, 10, 0.68).
narrative_ontology:measurement(jzsrzr_su_t20, jewish_sovereignty_palestine__religious_zionist_reading, suppression_requirement, 20, 0.74).
narrative_ontology:measurement(jzsrzr_su_t30, jewish_sovereignty_palestine__religious_zionist_reading, suppression_requirement, 30, 0.79).
narrative_ontology:measurement(jzsrzr_su_t40, jewish_sovereignty_palestine__religious_zionist_reading, suppression_requirement, 40, 0.83).
narrative_ontology:measurement(jzsrzr_su_t50, jewish_sovereignty_palestine__religious_zionist_reading, suppression_requirement, 50, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jewish_sovereignty_palestine__religious_zionist_reading, identity_coordination).
narrative_ontology:affects_constraint(jewish_sovereignty_palestine__religious_zionist_reading, liberal_nationalist_reading).
narrative_ontology:affects_constraint(jewish_sovereignty_palestine__religious_zionist_reading, settler_colonial_reading).
narrative_ontology:affects_constraint(jewish_sovereignty_palestine__religious_zionist_reading, cultural_zionist_reading).
narrative_ontology:affects_constraint(jewish_sovereignty_palestine__religious_zionist_reading, post_zionist_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the jewish_sovereignty_palestine kernel. The religious_zionist_reading instantiates the theological-commitment version of the territorial claim, distinguished by its divine-title axiom and its foreclosure of post-Zionist civic equality. Sibling readings decompose the same natural-language concept into structurally distinct claims with different epsilon values and beneficiary/victim structures.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
