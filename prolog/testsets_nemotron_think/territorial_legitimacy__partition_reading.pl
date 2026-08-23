% ============================================================================
% CONSTRAINT STORY: territorial_legitimacy__partition_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_territorial_legitimacy__partition_reading, []).

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
 *   constraint_id: territorial_legitimacy__partition_reading
 *   human_readable: Legitimacy via International Legal Partition (UN Resolution 181, 1948 Borders)
 *   domain: political/international_law/territorial_sovereignty
 *
 * SUMMARY:
 *   The partition reading grounds territorial legitimacy in UN General
 *   Assembly Resolution 181 (1947), which recommended partitioning Mandate
 *   Palestine into independent Arab and Jewish states with an international
 *   corpus separatum for Jerusalem. Israel's 1948 Declaration of Independence
 *   explicitly cites Resolution 181 as legal basis; the 1988 Palestinian
 *   Declaration of Independence does the same. The reading holds that both
 *   states are legitimate within 1948/1967 borders, settlements beyond 1967
 *   are illegal, and a two-state solution remains structurally possible. This
 *   is one of three contested readings of the territorial_legitimacy kernel —
 *   the others being security_necessity_reading (legitimacy via defensive
 *   control) and indigenous_continuity_reading (legitimacy via anti-colonial
 *   self-determination). The partition reading operates as a tangled rope: it
 *   coordinates mutual recognition and diplomatic engagement (genuine
 *   coordination function) while legitimating the 1948 displacement and
 *   extracting compliance from settlers beyond 1967 (asymmetric extraction).
 *   Active enforcement via UN resolutions, ICJ opinions, and diplomatic
 *   pressure is required to maintain the framework against settlement
 *   expansion and rejectionist violence.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(territorial_legitimacy__partition_reading, 0.65).
domain_priors:suppression_score(territorial_legitimacy__partition_reading, 0.55).
domain_priors:theater_ratio(territorial_legitimacy__partition_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(territorial_legitimacy__partition_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(territorial_legitimacy__partition_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(territorial_legitimacy__partition_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(territorial_legitimacy__partition_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(territorial_legitimacy__partition_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(territorial_legitimacy__partition_reading, tangled_rope).
narrative_ontology:human_readable(territorial_legitimacy__partition_reading, "Legitimacy via International Legal Partition (UN Resolution 181, 1948 Borders)").
narrative_ontology:topic_domain(territorial_legitimacy__partition_reading, "political/international_law/territorial_sovereignty").

domain_priors:requires_active_enforcement(territorial_legitimacy__partition_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(territorial_legitimacy__partition_reading, '4a507ee8-2801-43bb-b348-77e90b7470cf').
narrative_ontology:cs_kernel_codification('4a507ee8-2801-43bb-b348-77e90b7470cf', formalized).
narrative_ontology:cs_authority_grounding('4a507ee8-2801-43bb-b348-77e90b7470cf', lineage).
narrative_ontology:cs_interpretation_layer_present('4a507ee8-2801-43bb-b348-77e90b7470cf').
narrative_ontology:cs_reading_relation('4a507ee8-2801-43bb-b348-77e90b7470cf', territorial_legitimacy__security_necessity_reading, coexists_with).
narrative_ontology:cs_reading_relation('4a507ee8-2801-43bb-b348-77e90b7470cf', territorial_legitimacy__indigenous_continuity_reading, influences).
narrative_ontology:cs_axiom('4a507ee8-2801-43bb-b348-77e90b7470cf', foundational, partition_creates_mutual_legitimacy).
narrative_ontology:cs_axiom_status(partition_creates_mutual_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('4a507ee8-2801-43bb-b348-77e90b7470cf', partition_creates_mutual_legitimacy, conventional).
narrative_ontology:cs_axiom('4a507ee8-2801-43bb-b348-77e90b7470cf', foundational, borders_1948_are_legal_baseline).
narrative_ontology:cs_axiom_status(borders_1948_are_legal_baseline, holdable).
narrative_ontology:cs_axiom_grounding('4a507ee8-2801-43bb-b348-77e90b7470cf', borders_1948_are_legal_baseline, conventional).
narrative_ontology:cs_axiom('4a507ee8-2801-43bb-b348-77e90b7470cf', secondary, settlements_beyond_1967_illegitimate).
narrative_ontology:cs_axiom_status(settlements_beyond_1967_illegitimate, holdable).
narrative_ontology:cs_axiom_grounding('4a507ee8-2801-43bb-b348-77e90b7470cf', settlements_beyond_1967_illegitimate, empirically_contingent).
narrative_ontology:cs_reference_frame('4a507ee8-2801-43bb-b348-77e90b7470cf', un_resolution_181_partition_plan).
narrative_ontology:cs_drift_state('4a507ee8-2801-43bb-b348-77e90b7470cf', post_oslo_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('4a507ee8-2801-43bb-b348-77e90b7470cf', '').
narrative_ontology:cs_kernel_id(territorial_legitimacy__partition_reading, territorial_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(territorial_legitimacy__partition_reading, israeli_state_within_1948_borders).
narrative_ontology:constraint_beneficiary(territorial_legitimacy__partition_reading, palestinian_state_within_1948_borders).
narrative_ontology:constraint_beneficiary(territorial_legitimacy__partition_reading, international_legal_order).
narrative_ontology:constraint_victim(territorial_legitimacy__partition_reading, settlers_beyond_1967).
narrative_ontology:constraint_victim(territorial_legitimacy__partition_reading, palestinian_refugees_1948).
narrative_ontology:constraint_victim(territorial_legitimacy__partition_reading, israeli_settlement_enterprise).
narrative_ontology:constraint_vindicates(territorial_legitimacy__partition_reading, two_state_solution_legal_basis).
narrative_ontology:constraint_vindicates(territorial_legitimacy__partition_reading, territorial_integrity_via_un_resolution).
narrative_ontology:constraint_vindicates(territorial_legitimacy__partition_reading, mutual_recognition_through_partition).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Claims legitimacy within 1948 borders via UN Resolution 181 and Declaration of Independence. Administers territory, enforces laws, engages in diplomacy. Exit from partition framework means losing international legal basis for sovereignty; constrained by need for recognition and security guarantees.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__partition_reading, israeli_state_within_1948_borders, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(territorial_legitimacy__partition_reading, israeli_state_within_1948_borders, beneficiary).

% Claims statehood within 1948/1967 borders via Resolution 181 and 1988 Declaration. Depends on international recognition and negotiation for actual sovereignty. Exit means abandoning legal framework for statehood; constrained by occupation, settlement expansion, and internal division.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__partition_reading, palestinian_state_within_1948_borders, beneficiary,
    organized, generational, constrained, national).

% Israeli civilians living in settlements beyond 1967 lines. Their presence is deemed illegal under this reading's legal framework. Bear costs of potential evacuation, legal vulnerability, international condemnation. Identity fused with settlement project (religious, ideological, security); exit is psychologically and socially near-impossible.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__partition_reading, settlers_beyond_1967, payer,
    organized, biographical, identity_locked, local).

% Descendants of those displaced in 1948 war. Partition reading legitimates the 1948 borders that exclude their return. Bear costs of statelessness, camp life, denied right of return. No meaningful exit from refugee status; trapped by host state policies, UNRWA dependency, and the partition framework that treats 1948 as settled.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__partition_reading, palestinian_refugees_1948, payer,
    powerless, generational, trapped, regional).
narrative_ontology:stakeholder_secondary_role(territorial_legitimacy__partition_reading, palestinian_refugees_1948, excluded).

% Institutional complex (government ministries, settlement councils, funding bodies, ideological movements) that builds and maintains settlements beyond 1967. Extracts resources (land, water, state funding) but faces legal illegitimacy under this reading. Has mobility — can shift strategy, but ideological commitment constrains exit.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__partition_reading, israeli_settlement_enterprise, payer,
    powerful, biographical, mobile, local).

% UN system, ICJ, ICC, major states' foreign ministries, international legal academia. Authors, interprets, and attempts to enforce the partition framework through resolutions, opinions, diplomatic pressure. Collects no direct rents but maintains the legal architecture. Exit is analytical — can shift interpretation but not easily abandon the framework.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__partition_reading, international_legal_order, agenda_setter,
    institutional, civilizational, analytical, universal).
narrative_ontology:stakeholder_secondary_role(territorial_legitimacy__partition_reading, international_legal_order, observer).

% Hamas, Islamic Jihad, Palestinian rejectionist left; Israeli religious right, maximalist settlers. Deny legitimacy of partition entirely — seek all land (river to sea) for their respective nation. Excluded from the partition framework by definition; their exclusion is what the framework's enforcement maintains. Identity locked to total victory narratives.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__partition_reading, rejectionist_factions_both_sides, excluded,
    organized, biographical, identity_locked, national).

% International lawyers, political theorists, historians analyzing the partition framework's validity, evolution, and alternatives. Neither collect nor pay; provide the interpretive labor that sustains or challenges the reading's authority.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__partition_reading, academic_legal_observers, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes mutual recognition and defined borders for two states through international legal partition, replacing open-ended conflict with a legal framework that allocates sovereignty, enables diplomacy, and provides a baseline for security arrangements.
% TRANSFER_FUNCTION: Transfers territorial sovereignty claims from conquest and settlement to legal title grounded in UN Resolution 181; transfers the burden of proof from 'who controls the land' to 'who holds legal title within recognized borders'; transfers legitimacy from facts on the ground to international legal authorization.
% ABSENT_VOICES: Palestinian refugees displaced in 1948 (Nakba victims) whose right of return is extinguished by the partition's legitimization of 1948 borders; rejectionist factions on both sides (Hamas, Israeli religious maximalists) who deny partition's legitimacy entirely and would replace it with total territorial claims; settler movements beyond 1967 lines whose project the reading declares illegal.
% DISAPPEARANCE_RATIONALE: If the partition reading's legitimacy vanished overnight, the only internationally recognized legal basis for two states would collapse. Borders would revert to raw contestation; settlements beyond 1967 would gain equal legal standing with pre-1967 Israel; the Palestinian claim to statehood would lose its UN anchor; the conflict would become a pure power struggle with no agreed legal framework for resolution.
% FOUNDING_PROBLEM: The 1947-48 Palestine conflict required an internationally legitimate framework to divide territory between two national movements (Zionist and Palestinian Arab) and end the British Mandate, preventing regional war and providing legal basis for statehood.
% FOUNDING_PROBLEM_CORROBORATION: UN General Assembly records (Resolution 181 vote 33-13-10) and Israeli Declaration of Independence (explicitly cites Resolution 181) corroborate the partition as founding framework. Palestinian Declaration of Independence (1988, Algiers) also cites Resolution 181. However, both sides' rejectionist factions and the refugee population dispute that the founding problem was solved — the 1948 war created new displacement, 1967 war left borders unsettled, and the two-state solution remains unrealized. No single external arbiter confirms the problem is 'live' or 'dead'; the status is structurally contested.
narrative_ontology:disappearance_verdict(territorial_legitimacy__partition_reading, world_rearranges).
narrative_ontology:founding_problem_status(territorial_legitimacy__partition_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(territorial_legitimacy__partition_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(territorial_legitimacy__partition_reading, 'none', 1).
narrative_ontology:epsilon_provenance(territorial_legitimacy__partition_reading, 0.65, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(territorial_legitimacy__partition_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(territorial_legitimacy__partition_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(territorial_legitimacy__partition_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.65) reflects the constraint's dual character: it extracts from settlers beyond 1967 (legal vulnerability, potential evacuation costs) and from 1948 refugees (denied return), while providing coordination benefits to recognized states. The 1948 baseline shows lower extraction (0.35) when the framework was fresh and borders unsettled; extraction rises as settlements expand (post-1967, post-Oslo) and the gap between legal title and facts on ground widens. Suppression (0.55) is moderate — the framework suppresses alternatives (one-state, annexation, total rejection) through international law and diplomacy, but cannot eliminate them; resistance (0.75) is high from settlers, rejectionists, and refugee advocates. Theater ratio (0.30) grows over time as the two-state solution recedes while diplomatic rituals continue. Accessibility collapse (0.60) reflects that alternatives are legally foreclosed but politically persistent.
 *
 * PERSPECTIVAL GAP:
 *   The Israeli and Palestinian state seats (within 1948 borders) experience this as coordination — they gain mutual recognition and legal standing. Settlers beyond 1967 and 1948 refugees experience it as extraction — their claims are legally nullified. The international legal order experiences it as coordination maintenance. Rejectionists experience it as suppression of their total claims. The engine will compute per-seat types from these structural positions; the claimed_type (tangled_rope) reflects the aggregate structure.
 *
 * DIRECTIONALITY LOGIC:
 *   Israeli and Palestinian states within 1948 borders are beneficiaries (d ~ 0.2-0.3) — they collect legitimacy, recognition, diplomatic standing. Settlers beyond 1967 are targets (d ~ 0.8) — bear legal illegitimacy, evacuation risk, international sanctions. 1948 refugees are targets (d ~ 0.9) — trapped, denied return, partition legitimizes their exclusion. Settlement enterprise is a powerful payer (d ~ 0.7) — extracts resources but constrained by legal framework. International legal order is near-symmetric (d ~ 0.5) — maintains framework, bears diplomatic costs. Rejectionists are identity-locked excluded (d ~ 0.95) — framework exists to suppress them. Academics are analytical (d ~ 0.5).
 *
 * MANDATROPHY ANALYSIS:
 *   The partition reading's founding problem (ending mandate, dividing territory between two national movements) is contested — partially solved (two states declared, one realized) but unresolved (borders, refugees, Jerusalem). The constraint persists not because the founding problem is live, but because no alternative legal framework commands international consensus. This is not pure mandatrophy (the coordination function remains real) nor pure extraction (the legal framework enables diplomacy). The tangled_rope classification captures this: genuine coordination (mutual recognition pathway) layered with asymmetric extraction (settler illegitimacy, refugee exclusion).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    partition_legitimates_nakba,
    'Does the partition reading''s legitimization of 1948 borders inherently legitimize the 1948 displacement (Nakba), or can the legal framework be separated from the historical displacement it ratified?',
    'Legal-historical analysis of whether Resolution 181''s partition map presupposes population transfer; ICJ advisory opinions on whether legal title derived from partition extinguishes refugee return rights; comparative study of other partition settlements (India/Pakistan, Cyprus) where legal partition did/did not legitimate displacement.',
    'If partition inherently legitimates displacement, the reading carries irreducible extraction from 1948 refugees that cannot be reformed away — the constraint is structurally a snare for that population. If separable, the reading could evolve to accommodate return/compensation within the partition framework, reducing its extractiveness.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(partition_legitimates_nakba, conceptual, 'Whether the partition framework''s legal title extinguishes refugee rights or leaves them negotiable.').

omega_variable(
    viability_of_two_state_solution,
    'Is the two-state solution structurally possible under current facts on the ground (settlement population ~700k, fragmented Palestinian territory), or has the partition reading become a performative cover for a de facto one-state reality?',
    'Geospatial analysis of settlement blocs vs. contiguous Palestinian territory; demographic modeling of separation feasibility; political economy of evacuation costs vs. annexation costs; observation of whether diplomatic actors treat two-state as operational goal or rhetorical placeholder.',
    'If two-state is no longer viable, the partition reading''s coordination function is hollow — theater_ratio approaches 1.0, classification shifts toward piton or snare. If viable, the coordination function remains live and tangled_rope holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(viability_of_two_state_solution, empirical, 'Whether the partition reading''s coordination function (two-state solution) survives factual erosion.').

omega_variable(
    international_law_as_cover,
    'Does the international legal order genuinely coordinate mutual recognition, or does it function as a cover maintaining Western/Israeli control while managing Palestinian expectations?',
    'Track ICJ/ICC actual enforcement vs. advisory opinions; measure diplomatic pressure symmetry (sanctions on settlements vs. sanctions on rejectionist violence); analyze whether the legal framework constrains Israeli policy or legitimates ''process'' without outcome.',
    'If cover, the international_legal_order stakeholder is a covert beneficiary (d lower than declared) and the constraint is more extractive than measured. If genuine coordination, the current classification holds.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(international_law_as_cover, preference, 'Whether the international legal enforcement is sincere coordination or managed extraction.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(territorial_legitimacy__partition_reading, 0, 77).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(territorial_legitimacy__partition_reading_tr_t0, territorial_legitimacy__partition_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(territorial_legitimacy__partition_reading_tr_t20, territorial_legitimacy__partition_reading, theater_ratio, 20, 0.15).
narrative_ontology:measurement(territorial_legitimacy__partition_reading_tr_t46, territorial_legitimacy__partition_reading, theater_ratio, 46, 0.22).
narrative_ontology:measurement(territorial_legitimacy__partition_reading_tr_t53, territorial_legitimacy__partition_reading, theater_ratio, 53, 0.25).
narrative_ontology:measurement(territorial_legitimacy__partition_reading_tr_t73, territorial_legitimacy__partition_reading, theater_ratio, 73, 0.28).
narrative_ontology:measurement(territorial_legitimacy__partition_reading_tr_t77, territorial_legitimacy__partition_reading, theater_ratio, 77, 0.3).

% Extraction over time
narrative_ontology:measurement(territorial_legitimacy__partition_reading_be_t0, territorial_legitimacy__partition_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(territorial_legitimacy__partition_reading_be_t20, territorial_legitimacy__partition_reading, base_extractiveness, 20, 0.45).
narrative_ontology:measurement(territorial_legitimacy__partition_reading_be_t46, territorial_legitimacy__partition_reading, base_extractiveness, 46, 0.55).
narrative_ontology:measurement(territorial_legitimacy__partition_reading_be_t53, territorial_legitimacy__partition_reading, base_extractiveness, 53, 0.6).
narrative_ontology:measurement(territorial_legitimacy__partition_reading_be_t73, territorial_legitimacy__partition_reading, base_extractiveness, 73, 0.63).
narrative_ontology:measurement(territorial_legitimacy__partition_reading_be_t77, territorial_legitimacy__partition_reading, base_extractiveness, 77, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(territorial_legitimacy__partition_reading_su_t0, territorial_legitimacy__partition_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(territorial_legitimacy__partition_reading_su_t20, territorial_legitimacy__partition_reading, suppression_requirement, 20, 0.5).
narrative_ontology:measurement(territorial_legitimacy__partition_reading_su_t46, territorial_legitimacy__partition_reading, suppression_requirement, 46, 0.52).
narrative_ontology:measurement(territorial_legitimacy__partition_reading_su_t53, territorial_legitimacy__partition_reading, suppression_requirement, 53, 0.53).
narrative_ontology:measurement(territorial_legitimacy__partition_reading_su_t73, territorial_legitimacy__partition_reading, suppression_requirement, 73, 0.54).
narrative_ontology:measurement(territorial_legitimacy__partition_reading_su_t77, territorial_legitimacy__partition_reading, suppression_requirement, 77, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(territorial_legitimacy__partition_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(territorial_legitimacy__partition_reading, 0.12).
narrative_ontology:affects_constraint(territorial_legitimacy__partition_reading, territorial_legitimacy__security_necessity_reading).
narrative_ontology:affects_constraint(territorial_legitimacy__partition_reading, territorial_legitimacy__indigenous_continuity_reading).

% DUAL FORMULATION NOTE:
% This constraint (partition_reading) and its siblings (security_necessity_reading, indigenous_continuity_reading) form the territorial_legitimacy constraint family. Each reading instantiates a different ε: partition_reading ε=0.65 (tangled_rope — coordination + extraction); security_necessity_reading ε≈0.45 (rope/coordination-heavy, extracts from rejectionists); indigenous_continuity_reading ε≈0.80 (snare — high extraction from settlers/Israelis, coordination only for indigenous claim). They share the kernel_id 'territorial_legitimacy' and differ in beneficiary/victim structure and claimed_type. The partition reading is upstream: its legal framework is cited by both siblings as either baseline (security) or injustice (indigenous).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(territorial_legitimacy__partition_reading, institutional, 0.25).
constraint_indexing:directionality_override(territorial_legitimacy__partition_reading, organized, 0.75).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
