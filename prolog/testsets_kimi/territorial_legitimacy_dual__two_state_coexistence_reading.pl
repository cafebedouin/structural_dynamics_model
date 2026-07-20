% ============================================================================
% CONSTRAINT STORY: territorial_legitimacy_dual__two_state_coexistence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
 *   constraint_id: territorial_legitimacy_dual__two_state_coexistence_reading
 *   human_readable: Two-State Coexistence with 1967 Partition
 *   domain: political/international_relations
 *
 * SUMMARY:
 *   The two-state solution framework posits that Israeli and Palestinian
 *   national claims can be reconciled through mutual recognition of dual
 *   legitimacy, with the 1967 armistice lines forming the basis for
 *   territorial partition and security cooperation replacing military
 *   confrontation. This constraint is one reading of the contested
 *   territorial_legitimacy_dual kernel, instantiating the compromise position
 *   that both peoples hold legitimate self-determination claims and that
 *   partition along 1967 lines is the operative framework for realizing them.
 *   Sibling readings include Zionist maximalism (refuge, divine promise,
 *   unpartitioned land) and Palestinian autochthony (continuous habitation,
 *   right of return to all pre-1948 homes). The framework imposes asymmetric
 *   costs on Israeli settlers (territorial evacuation) and Palestinian
 *   refugees (limited return), while delivering coordination gains
 *   (sovereignty, security) to the broader citizenries. It requires active
 *   enforcement (peacekeeping, security cooperation, settlement
 *   dismantlement) and faces high resistance from spoiler groups excluded
 *   from the framework.
 *
 * KEY AGENTS:
 *   - israeli_government: Agenda-setter (institutional/constrained) â enforces security cooperation, administers settlement dismantlement
 *   - palestinian_authority: Agenda-setter (institutional/constrained) â administers Palestinian state functions, coordinates security
 *   - israeli_citizens: Beneficiary (organized/constrained) â gains international recognition and security
 *   - palestinian_citizens: Beneficiary (organized/constrained) â gains sovereign statehood
 *   - israeli_settlers: Primary target (moderate/identity_locked) â bears territorial evacuation and identity costs
 *   - palestinian_refugees: Primary target (powerless/trapped) â bears restricted return rights
 *   - international_arbiters: Observer (institutional/analytical) â mediates and legitimizes framework
 *   - nonstate_spoilers: Excluded (organized/constrained) â resist framework, excluded from recognition
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(territorial_legitimacy_dual__two_state_coexistence_reading, 0.58).
domain_priors:suppression_score(territorial_legitimacy_dual__two_state_coexistence_reading, 0.72).
domain_priors:theater_ratio(territorial_legitimacy_dual__two_state_coexistence_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(territorial_legitimacy_dual__two_state_coexistence_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(territorial_legitimacy_dual__two_state_coexistence_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(territorial_legitimacy_dual__two_state_coexistence_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(territorial_legitimacy_dual__two_state_coexistence_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(territorial_legitimacy_dual__two_state_coexistence_reading, resistance, 0.78).

% --- Constraint claim ---
narrative_ontology:constraint_claim(territorial_legitimacy_dual__two_state_coexistence_reading, tangled_rope).
narrative_ontology:human_readable(territorial_legitimacy_dual__two_state_coexistence_reading, "Two-State Coexistence with 1967 Partition").
narrative_ontology:topic_domain(territorial_legitimacy_dual__two_state_coexistence_reading, "political/international_relations").

domain_priors:requires_active_enforcement(territorial_legitimacy_dual__two_state_coexistence_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(territorial_legitimacy_dual__two_state_coexistence_reading, '8f6b66ad-5e98-442b-ae02-986c9e8d2e2d').
narrative_ontology:cs_kernel_codification('8f6b66ad-5e98-442b-ae02-986c9e8d2e2d', formalized).
narrative_ontology:cs_authority_grounding('8f6b66ad-5e98-442b-ae02-986c9e8d2e2d', lineage).
narrative_ontology:cs_interpretation_layer_present('8f6b66ad-5e98-442b-ae02-986c9e8d2e2d').
narrative_ontology:cs_reading_relation('8f6b66ad-5e98-442b-ae02-986c9e8d2e2d', territorial_legitimacy_dual__zionist_refuge_reading, coexists_with).
narrative_ontology:cs_reading_relation('8f6b66ad-5e98-442b-ae02-986c9e8d2e2d', territorial_legitimacy_dual__palestinian_autochthony_reading, coexists_with).
narrative_ontology:cs_axiom('8f6b66ad-5e98-442b-ae02-986c9e8d2e2d', foundational, mutual_recognition_dual_legitimacy).
narrative_ontology:cs_axiom_status(mutual_recognition_dual_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('8f6b66ad-5e98-442b-ae02-986c9e8d2e2d', mutual_recognition_dual_legitimacy, deontological).
narrative_ontology:cs_axiom('8f6b66ad-5e98-442b-ae02-986c9e8d2e2d', foundational, territorial_partition_1967_lines).
narrative_ontology:cs_axiom_status(territorial_partition_1967_lines, holdable).
narrative_ontology:cs_axiom_grounding('8f6b66ad-5e98-442b-ae02-986c9e8d2e2d', territorial_partition_1967_lines, conventional).
narrative_ontology:cs_reference_frame('8f6b66ad-5e98-442b-ae02-986c9e8d2e2d', dual_sovereignty_partition_1967).
narrative_ontology:cs_drift_state('8f6b66ad-5e98-442b-ae02-986c9e8d2e2d', contemporary, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('8f6b66ad-5e98-442b-ae02-986c9e8d2e2d', '').
narrative_ontology:cs_kernel_id(territorial_legitimacy_dual__two_state_coexistence_reading, territorial_legitimacy_dual).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(territorial_legitimacy_dual__two_state_coexistence_reading, israeli_citizens).
narrative_ontology:constraint_beneficiary(territorial_legitimacy_dual__two_state_coexistence_reading, palestinian_citizens).
narrative_ontology:constraint_victim(territorial_legitimacy_dual__two_state_coexistence_reading, israeli_settlers).
narrative_ontology:constraint_victim(territorial_legitimacy_dual__two_state_coexistence_reading, palestinian_refugees).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the Israeli state, negotiates partition terms, and enforces security cooperation with Palestinian counterparts. Must politically manage settlement evacuation and domestic hardline opposition. Exit from the framework means abandoning international legitimacy and risking perpetual conflict.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__two_state_coexistence_reading, israeli_government, agenda_setter,
    institutional, generational, constrained, national).

% Administers limited Palestinian self-governance under the framework and coordinates security with Israeli forces. Seeks full sovereignty within 1967 lines but operates under conditions of continued settlement expansion and conditional aid. Exit risks complete loss of institutional structure and international recognition.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__two_state_coexistence_reading, palestinian_authority, agenda_setter,
    institutional, generational, constrained, national).

% Receive international recognition of Israel's legitimacy and security coordination that reduces terrorism and regional military threats. Bear indirect costs of military service and taxation for defense, but the framework secures their state's demographic and territorial core.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__two_state_coexistence_reading, israeli_citizens, beneficiary,
    organized, biographical, constrained, national).

% Receive the promise of sovereign statehood and self-determination under the framework. Currently live under occupation or blockade with sovereignty deferred, but the framework is the only diplomatic vehicle that recognizes their national claim to a state.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__two_state_coexistence_reading, palestinian_citizens, beneficiary,
    organized, biographical, constrained, national).

% Inhabit settlements beyond the 1967 lines and would face forced evacuation and loss of homes, communities, and biblical/ideological attachment to the land under full framework implementation. Their identity is fused with territorial maximalism, making exit from the constraint politically and spiritually unthinkable.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__two_state_coexistence_reading, israeli_settlers, payer,
    moderate, biographical, identity_locked, regional).

% Descendants of those displaced in 1948 who are denied return to homes inside Israel proper under the framework, with return limited to a future Palestinian state. Live in camps or diaspora with legal limbo, bearing the intergenerational cost of a political compromise they did not choose.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__two_state_coexistence_reading, palestinian_refugees, payer,
    powerless, generational, trapped, regional).

% UN bodies, the Quartet, and mediating powers maintain the diplomatic architecture of the framework, fund Palestinian institutions, and legitimate the 1967 boundary discourse. They neither pay the constraint's costs nor collect its direct benefits, but invest geopolitical capital in its persistence.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__two_state_coexistence_reading, international_arbiters, observer,
    institutional, generational, analytical, global).

% Militant and hardline political organizations on both sides that reject partition and mutual recognition. They are structurally excluded from the negotiating framework and actively resist it through violence, settlement expansion, or political subversion.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__two_state_coexistence_reading, nonstate_spoilers, excluded,
    organized, biographical, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(territorial_legitimacy_dual__two_state_coexistence_reading, diffuse).
narrative_ontology:fixing_cost_class(territorial_legitimacy_dual__two_state_coexistence_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Enables two peoples with competing territorial claims to exercise self-determination in separate sovereign states, replacing perpetual armed conflict with mutual recognition, diplomatic relations, and security cooperation.
% TRANSFER_FUNCTION: Transfers territorial control (Israeli withdrawal toward 1967 lines, Palestinian acceptance of truncated sovereignty), security burdens (demilitarization, counter-terrorism cooperation), and demographic rights (limitation of Palestinian refugee return to the Palestinian state, preservation of Israeli demographic majority).
% ABSENT_VOICES: Bi-nationalists, confederalists, and one-state advocates on both sides who reject partition as itself a colonial imposition; maximalist Zionists who reject Palestinian sovereignty; maximalist Palestinian nationalists who reject Israeli legitimacy. These voices are structurally excluded from the formal negotiating framework dominated by partition discourse.
% DISAPPEARANCE_RATIONALE: Without the mutual recognition and territorial partition framework, the current arrangement of security cooperation and territorial division collapses into either unilateral domination (annexation/apartheid) or total war over the entire mandate territory; the political architecture of the region depends on this constraint.
% FOUNDING_PROBLEM: The 1948 war and subsequent conflicts created two peoples with irreconcilable maximalist claims to the same territory, producing perpetual low-intensity conflict, refugee statelessness, and regional instability absent a legitimate boundary.
% FOUNDING_PROBLEM_CORROBORATION: International historians and conflict scholars attest the competing self-determination problem remains unresolved. However, critical geographers and Palestinian intellectuals outside the PA beneficiary structure attest the framework now legitimates asymmetric domination rather than solving the founding conflict. Israeli settler movements and anti-Zionist critics also attest from outside the citizen-beneficiary structure that the framework is obsolete. No outside-neutral consensus exists.
narrative_ontology:disappearance_verdict(territorial_legitimacy_dual__two_state_coexistence_reading, world_rearranges).
narrative_ontology:founding_problem_status(territorial_legitimacy_dual__two_state_coexistence_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(territorial_legitimacy_dual__two_state_coexistence_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(territorial_legitimacy_dual__two_state_coexistence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(territorial_legitimacy_dual__two_state_coexistence_reading, 0.58, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness (0.58) reflects the heavy asymmetric costs imposed on refugees and settlers to maintain the partition. Suppression (0.72) is high because the framework requires active security cooperation, border enforcement, and containment of nonstate spoilers who reject partition. Theater ratio (0.45) captures the performative peace process (repeated negotiations, roadmaps, conferences) that produced limited territorial change while sustaining the framework discursively. Accessibility collapse (0.65) reflects how alternative frameworks (confederation, one-state, full annexation) are marginalized in formal diplomatic discourse. Resistance (0.78) is high due to persistent violent and political rejection from hardliners on both sides.
 *
 * PERSPECTIVAL GAP:
 *   Israeli citizens experience the framework as security coordination and international legitimacy; Palestinian citizens experience it as sovereignty delayed or conditional; settlers experience it as existential threat to identity and home; refugees experience it as permanent dispossession legitimized by international consensus. The engine computes these divergent classifications from the same structural constraint.
 *
 * DIRECTIONALITY LOGIC:
 *   Israeli and Palestinian citizens are beneficiaries (low d), receiving sovereignty and recognition. Settlers and refugees are victims (high d), bearing the concentrated costs of territorial compromise. Governments sit near symmetric: they gain authority from the arrangement but must pay the enforcement costs and political risks of implementing unpopular compromises. International arbiters are near analytical (low extraction).
 *
 * MANDATROPHY ANALYSIS:
 *   The framework prevents mandatrophy mislabeling by requiring both coordination and extraction: it is not a pure snare because it solves a genuine coordination problem (ending perpetual war, providing self-determination), but it is not a pure rope because identifiable agents (refugees, settlers) bear severe asymmetric costs. Without the victim declarations, the framework might compute as rope; without the coordination function, it might compute as snare. The structural data force tangled_rope classification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_containment,
    'Does the two-state reading structurally contain the maximalist sibling readings, or does it merely delay their reassertion as the framework erodes?',
    'Historical trajectory analysis: if withdrawal from the framework consistently produces reversion to zionist_refuge or palestinian_autochthony maximalism rather than alternative compromise frameworks, the reading is containment; if it produces binational or confederal alternatives, it is displacement.',
    'If containment, classification as tangled_rope with rising extraction is correct; if displacement, the framework may be a scaffold whose sunset should have activated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_containment, conceptual, 'Whether two-state is stable equilibrium or temporary containment').

omega_variable(
    settlement_viability_threshold,
    'Has Israeli settlement expansion crossed a geographical threshold that makes the 1967 partition materially impossible, rendering the framework performative?',
    'Geographic and demographic audit of settlement blocs relative to 1967 lines compared against viable contiguity and capital-sovereignty requirements for a Palestinian state.',
    'If the threshold is crossed, theater_ratio is higher than measured and the coordination function is largely mythical; effective extractiveness falls on Palestinian citizens who receive sovereignty in name only.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(settlement_viability_threshold, empirical, 'Whether settlement expansion has eliminated territorial viability').

omega_variable(
    enforcement_asymmetry,
    'Is the enforcement of the framework (security cooperation, territorial control) applied symmetrically, or does it fall disproportionately on Palestinian sovereignty limitations while exempting settlement consolidation?',
    'Comparative enforcement audit: measure Israeli settlement removal versus Palestinian Authority security suppression of dissent; assess whether demilitarization and border control are reciprocal.',
    'If enforcement is asymmetric, the constraint shifts from tangled_rope toward snare for the Palestinian seat, as the coordination benefit is conditional and the extraction is one-sided.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(enforcement_asymmetry, empirical, 'Asymmetric enforcement between the two parties').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(territorial_legitimacy_dual__two_state_coexistence_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(territorial_legitimacy_dual_tscr_tr_t0, territorial_legitimacy_dual__two_state_coexistence_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(territorial_legitimacy_dual_tscr_tr_t7, territorial_legitimacy_dual__two_state_coexistence_reading, theater_ratio, 7, 0.3).
narrative_ontology:measurement(territorial_legitimacy_dual_tscr_tr_t15, territorial_legitimacy_dual__two_state_coexistence_reading, theater_ratio, 15, 0.42).
narrative_ontology:measurement(territorial_legitimacy_dual_tscr_tr_t22, territorial_legitimacy_dual__two_state_coexistence_reading, theater_ratio, 22, 0.52).
narrative_ontology:measurement(territorial_legitimacy_dual_tscr_tr_t30, territorial_legitimacy_dual__two_state_coexistence_reading, theater_ratio, 30, 0.6).

% Extraction over time
narrative_ontology:measurement(territorial_legitimacy_dual_tscr_be_t0, territorial_legitimacy_dual__two_state_coexistence_reading, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(territorial_legitimacy_dual_tscr_be_t7, territorial_legitimacy_dual__two_state_coexistence_reading, base_extractiveness, 7, 0.44).
narrative_ontology:measurement(territorial_legitimacy_dual_tscr_be_t15, territorial_legitimacy_dual__two_state_coexistence_reading, base_extractiveness, 15, 0.52).
narrative_ontology:measurement(territorial_legitimacy_dual_tscr_be_t22, territorial_legitimacy_dual__two_state_coexistence_reading, base_extractiveness, 22, 0.58).
narrative_ontology:measurement(territorial_legitimacy_dual_tscr_be_t30, territorial_legitimacy_dual__two_state_coexistence_reading, base_extractiveness, 30, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(territorial_legitimacy_dual_tscr_su_t0, territorial_legitimacy_dual__two_state_coexistence_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(territorial_legitimacy_dual_tscr_su_t7, territorial_legitimacy_dual__two_state_coexistence_reading, suppression_requirement, 7, 0.62).
narrative_ontology:measurement(territorial_legitimacy_dual_tscr_su_t15, territorial_legitimacy_dual__two_state_coexistence_reading, suppression_requirement, 15, 0.7).
narrative_ontology:measurement(territorial_legitimacy_dual_tscr_su_t22, territorial_legitimacy_dual__two_state_coexistence_reading, suppression_requirement, 22, 0.75).
narrative_ontology:measurement(territorial_legitimacy_dual_tscr_su_t30, territorial_legitimacy_dual__two_state_coexistence_reading, suppression_requirement, 30, 0.82).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(territorial_legitimacy_dual__two_state_coexistence_reading, territorial_legitimacy_dual__zionist_refuge_reading).
narrative_ontology:affects_constraint(territorial_legitimacy_dual__two_state_coexistence_reading, territorial_legitimacy_dual__palestinian_autochthony_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the territorial_legitimacy_dual kernel. Each reading carries a distinct epsilon, stakeholder structure, and classification. They are linked as a constraint family because the colloquial label 'Israeli-Palestinian conflict resolution' conflates structurally distinct legitimacy claims.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
