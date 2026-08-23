% ============================================================================
% CONSTRAINT STORY: territorial_legitimacy_dual__two_state_coexistence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   human_readable: Two-State Coexistence: Mutual Recognition with 1967 Boundaries
 *   domain: political/international/territorial
 *
 * SUMMARY:
 *   This constraint instantiates the two-state coexistence reading of the
 *   territorial_legitimacy_dual kernel. It accepts 1948 as legitimating both
 *   peoples' claims, takes the 1967 armistice lines as the partition basis,
 *   limits Palestinian right of return to the future Palestinian state, and
 *   replaces zero-sum territorial competition with security cooperation. The
 *   constraint operates through the Oslo framework, Quartet diplomacy, and
 *   repeated negotiation rounds — it is actively enforced by international
 *   guarantors and bilateral security coordination. The reading presents
 *   itself as a Rope (mutual coordination for peace) but carries asymmetric
 *   extraction: refugees bear the cost of the return limitation, settlers
 *   bear relocation costs, and security arrangements privilege Israeli
 *   operational control. The engine will compute per-seat types from the
 *   structural data authored here.
 *
 * KEY AGENTS:
 *   - israeli_state: Primary agenda_setter and partial beneficiary (institutional/arbitrage) — sets security terms, receives recognition and security cooperation
 *   - palestinian_authority_leadership: Beneficiary and partial payer (institutional/constrained) — receives statehood recognition and territorial basis, concedes right of return and accepts security subordination
 *   - palestinian_refugees: Primary payer (powerless/trapped) — right of return limited to Palestinian state, not ancestral homes in Israel; no exit from this limitation
 *   - israeli_settlers_west_bank: Payer and partial beneficiary (organized/constrained) — face potential relocation but gain recognized legitimacy for remaining blocs
 *   - international_quartet: Observer (institutional/analytical) — monitors, guarantees, and enforces the framework
 *   - rejectionist_factions_both_sides: Excluded (organized/trapped) — would object to mutual recognition and 1967 basis but are not seated at the table
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(territorial_legitimacy_dual__two_state_coexistence_reading, 0.58).
domain_priors:suppression_score(territorial_legitimacy_dual__two_state_coexistence_reading, 0.52).
domain_priors:theater_ratio(territorial_legitimacy_dual__two_state_coexistence_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(territorial_legitimacy_dual__two_state_coexistence_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(territorial_legitimacy_dual__two_state_coexistence_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(territorial_legitimacy_dual__two_state_coexistence_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(territorial_legitimacy_dual__two_state_coexistence_reading, accessibility_collapse, 0.63).
narrative_ontology:constraint_metric(territorial_legitimacy_dual__two_state_coexistence_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(territorial_legitimacy_dual__two_state_coexistence_reading, tangled_rope).
narrative_ontology:human_readable(territorial_legitimacy_dual__two_state_coexistence_reading, "Two-State Coexistence: Mutual Recognition with 1967 Boundaries").
narrative_ontology:topic_domain(territorial_legitimacy_dual__two_state_coexistence_reading, "political/international/territorial").

domain_priors:requires_active_enforcement(territorial_legitimacy_dual__two_state_coexistence_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(territorial_legitimacy_dual__two_state_coexistence_reading, '904f406a-d719-4c93-861c-16676c973793').
narrative_ontology:cs_kernel_codification('904f406a-d719-4c93-861c-16676c973793', formalized).
narrative_ontology:cs_authority_grounding('904f406a-d719-4c93-861c-16676c973793', lineage).
narrative_ontology:cs_interpretation_layer_present('904f406a-d719-4c93-861c-16676c973793').
narrative_ontology:cs_reading_relation('904f406a-d719-4c93-861c-16676c973793', territorial_legitimacy_dual__zionist_refuge_reading, coexists_with).
narrative_ontology:cs_reading_relation('904f406a-d719-4c93-861c-16676c973793', territorial_legitimacy_dual__palestinian_autochthony_reading, coexists_with).
narrative_ontology:cs_axiom('904f406a-d719-4c93-861c-16676c973793', foundational, mutual_recognition_1948_legitimacy).
narrative_ontology:cs_axiom_status(mutual_recognition_1948_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('904f406a-d719-4c93-861c-16676c973793', mutual_recognition_1948_legitimacy, conventional).
narrative_ontology:cs_axiom('904f406a-d719-4c93-861c-16676c973793', foundational, id_1967_boundaries_partition_basis).
narrative_ontology:cs_axiom_status(id_1967_boundaries_partition_basis, holdable).
narrative_ontology:cs_axiom_grounding('904f406a-d719-4c93-861c-16676c973793', id_1967_boundaries_partition_basis, conventional).
narrative_ontology:cs_axiom('904f406a-d719-4c93-861c-16676c973793', foundational, right_of_return_limited_to_palestinian_state).
narrative_ontology:cs_axiom_status(right_of_return_limited_to_palestinian_state, holdable).
narrative_ontology:cs_axiom_grounding('904f406a-d719-4c93-861c-16676c973793', right_of_return_limited_to_palestinian_state, conventional).
narrative_ontology:cs_axiom('904f406a-d719-4c93-861c-16676c973793', foundational, security_cooperation_replaces_zero_sum).
narrative_ontology:cs_axiom_status(security_cooperation_replaces_zero_sum, holdable).
narrative_ontology:cs_axiom_grounding('904f406a-d719-4c93-861c-16676c973793', security_cooperation_replaces_zero_sum, instrumental).
narrative_ontology:cs_reference_frame('904f406a-d719-4c93-861c-16676c973793', oslo_accords_framework).
narrative_ontology:cs_drift_state('904f406a-d719-4c93-861c-16676c973793', post_second_intifada_stagnation, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('904f406a-d719-4c93-861c-16676c973793', '').
narrative_ontology:cs_kernel_id(territorial_legitimacy_dual__two_state_coexistence_reading, territorial_legitimacy_dual).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(territorial_legitimacy_dual__two_state_coexistence_reading, israeli_state).
narrative_ontology:constraint_beneficiary(territorial_legitimacy_dual__two_state_coexistence_reading, palestinian_authority_leadership).
narrative_ontology:constraint_victim(territorial_legitimacy_dual__two_state_coexistence_reading, palestinian_refugees).
narrative_ontology:constraint_victim(territorial_legitimacy_dual__two_state_coexistence_reading, israeli_settlers_west_bank).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(territorial_legitimacy_dual__two_state_coexistence_reading, israeli_settlers_west_bank).
narrative_ontology:constraint_victim(territorial_legitimacy_dual__two_state_coexistence_reading, palestinian_authority_leadership).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets the security terms of the framework, controls borders, airspace, and electromagnetic spectrum. Receives international recognition, security cooperation from PA, and preservation of Jewish demographic majority. Can absorb costs of the framework through its military-economic strength and international alliances. Exit would mean unilateral withdrawal or annexation — both costly but structurally possible.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__two_state_coexistence_reading, israeli_state, agenda_setter,
    institutional, generational, arbitrage, national).

% Receives international recognition as state-in-formation, territorial basis on 1967 lines, and institutional authority over Areas A/B. Concedes right of return to Israel, accepts security coordination under Israeli oversight, depends on Israeli permit regime for movement and trade, and relies on international aid conditioned on framework compliance. Exit would mean dissolving the PA or unilateral declaration — both risk collapse of governing capacity.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__two_state_coexistence_reading, palestinian_authority_leadership, beneficiary,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(territorial_legitimacy_dual__two_state_coexistence_reading, palestinian_authority_leadership, payer).

% Hold UNRWA refugee status and right-of-return claim to 1948 lands inside Israel. Under this reading, return is limited to the future Palestinian state (West Bank/Gaza), extinguishing individual claims to specific ancestral homes. No exit from this limitation — the framework binds them through PA representation they did not choose and international guarantors they cannot access. Identity-locked to return narrative; the constraint fuses their political identity to a claim the reading explicitly limits.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__two_state_coexistence_reading, palestinian_refugees, payer,
    powerless, generational, trapped, universal).

% Approximately 500,000 settlers in West Bank (excluding East Jerusalem). Under 1967 partition, major blocs likely annexed to Israel (beneficiary — recognized legitimacy), isolated settlements face relocation (payer — loss of homes, ideological defeat). Exit constrained by ideological commitment to 'Judea and Samaria', state subsidies, and lack of viable return communities inside Green Line. The framework's ambiguity on final borders maintains their position but extracts psychological and political cost.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__two_state_coexistence_reading, israeli_settlers_west_bank, payer,
    organized, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(territorial_legitimacy_dual__two_state_coexistence_reading, israeli_settlers_west_bank, beneficiary).

% UN, US, EU, Russia as Quartet — monitors implementation, issues reports, sets parameters for negotiations, provides aid conditioning. No direct extraction or payment; analytical seat that guarantees the framework's enforcement. Exit means diplomatic disengagement — structurally possible but politically costly for great-power credibility.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__two_state_coexistence_reading, international_quartet, observer,
    institutional, generational, analytical, global).

% Hamas, Islamic Jihad, Israeli religious-nationalist parties — reject mutual recognition, 1967 basis, and security cooperation. Would object to the framework's core premises if seated. Structurally excluded by Oslo architecture (PA committed to suppressing armed resistance; Israel refuses to negotiate with Hamas). Their exclusion is the enforcement object: the framework's suppression targets them to maintain the coordination function.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__two_state_coexistence_reading, rejectionist_factions_hamas_israeli_right, excluded,
    organized, biographical, trapped, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(territorial_legitimacy_dual__two_state_coexistence_reading, diffuse).
narrative_ontology:fixing_cost_class(territorial_legitimacy_dual__two_state_coexistence_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Ends zero-sum territorial war between two national movements claiming the same land by establishing mutual recognition, a defined partition line (1967), security cooperation replacing mutual threat, and a framework for ending claims. Solves the coordination problem of how two peoples can exercise self-determination in one territory without perpetual war.
% TRANSFER_FUNCTION: Moves territorial sovereignty over 78% of mandate Palestine to Israeli recognition, 22% to Palestinian statehood; moves right of return from individual claim to Israel into collective claim to Palestinian state; moves security responsibility from unilateral deterrence to bilateral cooperation with Israeli operational primacy; moves refugee compensation burden to international community.
% ABSENT_VOICES: Palestinian refugees (especially 1948 generation and diaspora) — would object to return limitation but are represented only by PA which conceded it. Israeli settlers in isolated settlements — would object to relocation but are represented by Israeli government which may trade them. Palestinian citizens of Israel — their status as minority in Jewish state is unaddressed. All are structurally excluded from the negotiation table that produced this reading.
% DISAPPEARANCE_RATIONALE: If this constraint vanished, the Oslo architecture would collapse: PA security cooperation would end, international guarantors would withdraw, 1967 lines would lose their status as negotiated border, and the default would revert to military occupation without peace process — or to one-state dynamics (annexation or binationalism). The territorial regime would fundamentally reorganize.
% FOUNDING_PROBLEM: The 1948 war created two national movements with mutually exclusive territorial claims on the same land. The 1967 war placed the entire territory under Israeli control. The founding problem: how to end the zero-sum war and enable both peoples' self-determination without either's destruction or expulsion. The two-state framework was built to solve this by partition along the 1967 ceasefire lines.
% FOUNDING_PROBLEM_CORROBORATION: Israeli peace camp and centrist parties attest the problem is live — two-state remains the only viable separation. Palestinian non-violent resistance and civil society attest the problem is live but the framework has failed. Israeli right and Hamas attest the problem is dead — the framework cannot deliver security or justice. International Quartet and UN attest the problem is live but the implementation has stalled. No single corroborating voice outside the benefiting leaderships confirms the problem is solved; the status is genuinely contested across all seats.
narrative_ontology:disappearance_verdict(territorial_legitimacy_dual__two_state_coexistence_reading, world_rearranges).
narrative_ontology:founding_problem_status(territorial_legitimacy_dual__two_state_coexistence_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(territorial_legitimacy_dual__two_state_coexistence_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(territorial_legitimacy_dual__two_state_coexistence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(territorial_legitimacy_dual__two_state_coexistence_reading, 0.58, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Extractiveness (0.58) reflects the asymmetric concessions: refugees lose return to 1948 lands, settlers face relocation, but both leaderships gain recognized statehood. Suppression (0.52) is moderate — the constraint persists through international guarantors and bilateral security enforcement, not pure coercion, but alternatives (one-state, armed resistance) are actively suppressed. Theater ratio (0.42) captures the performative peace process: repeated negotiations, ceremony, and 'final status' talks that sustain the framework while facts on ground (settlements, separation barrier) shift the baseline. Accessibility collapse (0.63) is significant — the two-state paradigm dominates diplomatic imagination, making one-state or confederation alternatives structurally marginal. Resistance (0.71) is high from rejectionist factions on both sides and from refugee populations. The constraint is Tangled Rope: genuine coordination (security cooperation, mutual recognition, ending zero-sum war) coexists with asymmetric extraction (return limitation, security subordination).
 *
 * PERSPECTIVAL GAP:
 *   The Israeli state seat experiences this as Rope verging on Mountain (security coordination works, recognition achieved, demographic balance protected). The Palestinian refugee seat experiences it as Snare (return extinguished, no exit, enforcement via PA security cooperation). The PA leadership seat experiences Tangled Rope (statehood gained at cost of return). The settler seat experiences Scaffold (temporary arrangement until facts-on-ground change it). The engine computes these divergences from the structural data — the single claimed_type cannot capture all seats.
 *
 * DIRECTIONALITY LOGIC:
 *   Israeli state: beneficiary (d ~ 0.2) — receives recognition, security cooperation, demographic majority preservation; sets agenda via security control. Palestinian Authority leadership: mixed (d ~ 0.45) — gains statehood and territorial basis but concedes return and accepts security limits; exit constrained by dependence on international aid and Israeli permits. Palestinian refugees: payer (d ~ 0.85) — bear the return limitation with no exit; identity-locked to return narrative. Israeli settlers: mixed payer/beneficiary (d ~ 0.6) — some blocs retained (beneficiary), isolated settlers relocated (payer); exit constrained by ideological commitment and state subsidies. International Quartet: observer (d ~ 0.1) — analytical seat, no extraction. Rejectionist factions: excluded — would be payers if seated but are structurally kept out.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (ending zero-sum territorial war via partition) remains contested — violence persists, but the core coordination function (preventing interstate war, enabling mutual recognition) is live. The constraint has not resolved its mandatrophy: it persists because no alternative coordination framework has emerged, not because its original function is fulfilled. The sunset clause is absent (no has_sunset_clause), so it is not a Scaffold. The theater ratio rise (0.25→0.42) suggests Goodhart drift: the peace process became a proxy for the peace itself.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is this constraint one reading of a contested kernel (territorial_legitimacy_dual) rather than a standalone constraint?',
    'Structural decomposition: if changing the reading (e.g., to zionist_refuge_reading or palestinian_autochthony_reading) changes the beneficiary/victim structure and ε without changing the territorial referent, the kernel frame is confirmed.',
    'Confirms this story must be authored as a single reading with its own ε, not a blend of readings. Sibling readings become separate constraint stories linked by network.affects_constraints.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Commitment to the kernel/reading frame for this constraint').

omega_variable(
    boundary_naturalness_ambiguity,
    'Are the 1967 lines a natural/legal boundary (Mountain-like) or a constructed ceasefire line repurposed as partition border?',
    'Legal-historical analysis: if 1967 lines have independent legal status (armistice lines only, no sovereignty), they are constructed; if they acquired legal force through subsequent practice, they approach natural-law status.',
    'If constructed, the boundary''s legitimacy is contingent on the reading''s acceptance — extraction from those who reject it is structural. If natural-law-adjacent, the constraint leans toward Rope/Mountain from the analytical seat.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(boundary_naturalness_ambiguity, conceptual, 'Whether the 1967 boundary has intrinsic legal force or derives force from this reading''s acceptance').

omega_variable(
    right_of_return_extraction_mechanism,
    'Does limiting right of return to the Palestinian state constitute asymmetric extraction from refugees, or is it a necessary coordination cost of partition?',
    'Comparative analysis: if other partition settlements (India/Pakistan, Cyprus, Ireland) permitted return to ancestral homes within the new states, the limitation is extractive; if return was universally limited to the new polity, it is a coordination cost.',
    'If extractive, ε rises and the constraint is Tangled Rope/Snare from the refugee seat. If coordination cost, ε falls and the constraint approaches Rope from all seats.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(right_of_return_extraction_mechanism, empirical, 'Whether the right-of-return limitation is extraction or coordination overhead').

omega_variable(
    security_cooperation_asymmetry,
    'Does security cooperation replace zero-sum competition symmetrically, or does it embed Israeli security primacy as structural extraction?',
    'Operational analysis of security arrangements: if Palestinian security forces operate under Israeli oversight, intelligence sharing is one-way, and airspace/EM spectrum control remains Israeli, the cooperation is asymmetric extraction.',
    'Asymmetric security cooperation raises ε for Palestinian seat, lowers it for Israeli seat — seat divergence increases. Symmetric cooperation supports Tangled Rope with balanced extraction.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(security_cooperation_asymmetry, empirical, 'Whether security cooperation is genuine coordination or extraction mechanism').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(territorial_legitimacy_dual__two_state_coexistence_reading, 1993, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(terr_tr_t1993, territorial_legitimacy_dual__two_state_coexistence_reading, theater_ratio, 1993, 0.25).
narrative_ontology:measurement(terr_tr_t2000, territorial_legitimacy_dual__two_state_coexistence_reading, theater_ratio, 2000, 0.32).
narrative_ontology:measurement(terr_tr_t2005, territorial_legitimacy_dual__two_state_coexistence_reading, theater_ratio, 2005, 0.38).
narrative_ontology:measurement(terr_tr_t2010, territorial_legitimacy_dual__two_state_coexistence_reading, theater_ratio, 2010, 0.4).
narrative_ontology:measurement(terr_tr_t2015, territorial_legitimacy_dual__two_state_coexistence_reading, theater_ratio, 2015, 0.41).
narrative_ontology:measurement(terr_tr_t2020, territorial_legitimacy_dual__two_state_coexistence_reading, theater_ratio, 2020, 0.42).
narrative_ontology:measurement(terr_tr_t2024, territorial_legitimacy_dual__two_state_coexistence_reading, theater_ratio, 2024, 0.42).

% Extraction over time
narrative_ontology:measurement(terr_be_t1993, territorial_legitimacy_dual__two_state_coexistence_reading, base_extractiveness, 1993, 0.35).
narrative_ontology:measurement(terr_be_t2000, territorial_legitimacy_dual__two_state_coexistence_reading, base_extractiveness, 2000, 0.45).
narrative_ontology:measurement(terr_be_t2005, territorial_legitimacy_dual__two_state_coexistence_reading, base_extractiveness, 2005, 0.52).
narrative_ontology:measurement(terr_be_t2010, territorial_legitimacy_dual__two_state_coexistence_reading, base_extractiveness, 2010, 0.55).
narrative_ontology:measurement(terr_be_t2015, territorial_legitimacy_dual__two_state_coexistence_reading, base_extractiveness, 2015, 0.57).
narrative_ontology:measurement(terr_be_t2020, territorial_legitimacy_dual__two_state_coexistence_reading, base_extractiveness, 2020, 0.58).
narrative_ontology:measurement(terr_be_t2024, territorial_legitimacy_dual__two_state_coexistence_reading, base_extractiveness, 2024, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(terr_su_t1993, territorial_legitimacy_dual__two_state_coexistence_reading, suppression_requirement, 1993, 0.3).
narrative_ontology:measurement(terr_su_t2000, territorial_legitimacy_dual__two_state_coexistence_reading, suppression_requirement, 2000, 0.42).
narrative_ontology:measurement(terr_su_t2005, territorial_legitimacy_dual__two_state_coexistence_reading, suppression_requirement, 2005, 0.48).
narrative_ontology:measurement(terr_su_t2010, territorial_legitimacy_dual__two_state_coexistence_reading, suppression_requirement, 2010, 0.5).
narrative_ontology:measurement(terr_su_t2015, territorial_legitimacy_dual__two_state_coexistence_reading, suppression_requirement, 2015, 0.51).
narrative_ontology:measurement(terr_su_t2020, territorial_legitimacy_dual__two_state_coexistence_reading, suppression_requirement, 2020, 0.52).
narrative_ontology:measurement(terr_su_t2024, territorial_legitimacy_dual__two_state_coexistence_reading, suppression_requirement, 2024, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(territorial_legitimacy_dual__two_state_coexistence_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(territorial_legitimacy_dual__two_state_coexistence_reading, 0.12).
narrative_ontology:affects_constraint(territorial_legitimacy_dual__two_state_coexistence_reading, territorial_legitimacy_dual__zionist_refuge_reading).
narrative_ontology:affects_constraint(territorial_legitimacy_dual__two_state_coexistence_reading, territorial_legitimacy_dual__palestinian_autochthony_reading).
narrative_ontology:affects_constraint(territorial_legitimacy_dual__two_state_coexistence_reading, oslo_security_coordination).
narrative_ontology:affects_constraint(territorial_legitimacy_dual__two_state_coexistence_reading, refugee_compensation_mechanism).
narrative_ontology:affects_constraint(territorial_legitimacy_dual__two_state_coexistence_reading, settlement_bloc_annexation_framework).

% DUAL FORMULATION NOTE:
% This constraint is the two_state_coexistence_reading of the territorial_legitimacy_dual kernel. The zionist_refuge_reading and palestinian_autochthony_reading are sibling constraints with different beneficiary/victim structures and ε values. All three share the territorial referent but instantiate different constraints. This reading's ε (0.58) is intermediate: higher than zionist_refuge_reading from Israeli seat (lower extraction for Israel), lower than palestinian_autochthony_reading from refugee seat (higher extraction for refugees). The network edges reflect that this reading's enforcement machinery (security coordination, Quartet) structurally influences the operating space of the sibling readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(territorial_legitimacy_dual__two_state_coexistence_reading, institutional, 0.2).
constraint_indexing:directionality_override(territorial_legitimacy_dual__two_state_coexistence_reading, powerless, 0.85).
constraint_indexing:directionality_override(territorial_legitimacy_dual__two_state_coexistence_reading, organized, 0.6).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
