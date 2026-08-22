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
 *   human_readable: Two-State Coexistence Framework: Mutual Recognition with 1967 Boundaries
 *   domain: political/international/territorial
 *
 * SUMMARY:
 *   The two-state coexistence reading instantiates a specific constraint:
 *   mutual recognition of 1948 legitimacy for both peoples, 1967 boundaries
 *   as the partition basis, refugee return limited to the Palestinian state,
 *   and security cooperation replacing zero-sum competition. This is ONE
 *   reading of the contested kernel 'territorial_legitimacy_dual' — not the
 *   kernel itself. The sibling readings (zionist_refuge_reading,
 *   palestinian_autochthony_reading) are separate constraints with different
 *   ε, different victim/beneficiary structures, different classifications.
 *   This story authors only the two-state reading's structural properties.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(territorial_legitimacy_dual__two_state_coexistence_reading, 0.42).
domain_priors:suppression_score(territorial_legitimacy_dual__two_state_coexistence_reading, 0.58).
domain_priors:theater_ratio(territorial_legitimacy_dual__two_state_coexistence_reading, 0.31).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(territorial_legitimacy_dual__two_state_coexistence_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(territorial_legitimacy_dual__two_state_coexistence_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(territorial_legitimacy_dual__two_state_coexistence_reading, theater_ratio, 0.31).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(territorial_legitimacy_dual__two_state_coexistence_reading, accessibility_collapse, 0.38).
narrative_ontology:constraint_metric(territorial_legitimacy_dual__two_state_coexistence_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(territorial_legitimacy_dual__two_state_coexistence_reading, tangled_rope).
narrative_ontology:human_readable(territorial_legitimacy_dual__two_state_coexistence_reading, "Two-State Coexistence Framework: Mutual Recognition with 1967 Boundaries").
narrative_ontology:topic_domain(territorial_legitimacy_dual__two_state_coexistence_reading, "political/international/territorial").

domain_priors:requires_active_enforcement(territorial_legitimacy_dual__two_state_coexistence_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(territorial_legitimacy_dual__two_state_coexistence_reading, 'bdccb1e5-a887-4e8f-9475-2b7dfe1d9e42').
narrative_ontology:cs_kernel_codification('bdccb1e5-a887-4e8f-9475-2b7dfe1d9e42', formalized).
narrative_ontology:cs_authority_grounding('bdccb1e5-a887-4e8f-9475-2b7dfe1d9e42', lineage).
narrative_ontology:cs_interpretation_layer_present('bdccb1e5-a887-4e8f-9475-2b7dfe1d9e42').
narrative_ontology:cs_reading_relation('bdccb1e5-a887-4e8f-9475-2b7dfe1d9e42', territorial_legitimacy_dual__zionist_refuge_reading, coexists_with).
narrative_ontology:cs_reading_relation('bdccb1e5-a887-4e8f-9475-2b7dfe1d9e42', territorial_legitimacy_dual__palestinian_autochthony_reading, coexists_with).
narrative_ontology:cs_axiom('bdccb1e5-a887-4e8f-9475-2b7dfe1d9e42', foundational, mutual_1948_legitimacy_recognition).
narrative_ontology:cs_axiom_status(mutual_1948_legitimacy_recognition, holdable).
narrative_ontology:cs_axiom_grounding('bdccb1e5-a887-4e8f-9475-2b7dfe1d9e42', mutual_1948_legitimacy_recognition, conventional).
narrative_ontology:cs_axiom('bdccb1e5-a887-4e8f-9475-2b7dfe1d9e42', foundational, id_1967_boundaries_as_final_partition_basis).
narrative_ontology:cs_axiom_status(id_1967_boundaries_as_final_partition_basis, holdable).
narrative_ontology:cs_axiom_grounding('bdccb1e5-a887-4e8f-9475-2b7dfe1d9e42', id_1967_boundaries_as_final_partition_basis, conventional).
narrative_ontology:cs_axiom('bdccb1e5-a887-4e8f-9475-2b7dfe1d9e42', foundational, refugee_return_limited_to_palestinian_state).
narrative_ontology:cs_axiom_status(refugee_return_limited_to_palestinian_state, holdable).
narrative_ontology:cs_axiom_grounding('bdccb1e5-a887-4e8f-9475-2b7dfe1d9e42', refugee_return_limited_to_palestinian_state, instrumental).
narrative_ontology:cs_axiom('bdccb1e5-a887-4e8f-9475-2b7dfe1d9e42', foundational, security_cooperation_replaces_zero_sum_competition).
narrative_ontology:cs_axiom_status(security_cooperation_replaces_zero_sum_competition, holdable).
narrative_ontology:cs_axiom_grounding('bdccb1e5-a887-4e8f-9475-2b7dfe1d9e42', security_cooperation_replaces_zero_sum_competition, instrumental).
narrative_ontology:cs_reference_frame('bdccb1e5-a887-4e8f-9475-2b7dfe1d9e42', oslo_accords_framework).
narrative_ontology:cs_drift_state('bdccb1e5-a887-4e8f-9475-2b7dfe1d9e42', post_second_intifada_stalemate, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('bdccb1e5-a887-4e8f-9475-2b7dfe1d9e42', '').
narrative_ontology:cs_kernel_id(territorial_legitimacy_dual__two_state_coexistence_reading, territorial_legitimacy_dual).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(territorial_legitimacy_dual__two_state_coexistence_reading, israeli_state_institutions).
narrative_ontology:constraint_beneficiary(territorial_legitimacy_dual__two_state_coexistence_reading, palestinian_authority_institutions).
narrative_ontology:constraint_beneficiary(territorial_legitimacy_dual__two_state_coexistence_reading, international_diplomatic_establishment).
narrative_ontology:constraint_victim(territorial_legitimacy_dual__two_state_coexistence_reading, palestinian_refugees_diaspora).
narrative_ontology:constraint_victim(territorial_legitimacy_dual__two_state_coexistence_reading, israeli_settlers_west_bank).
narrative_ontology:constraint_victim(territorial_legitimacy_dual__two_state_coexistence_reading, palestinian_residents_area_c).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the framework through diplomatic recognition, security coordination, and border management. Gains recognized sovereignty within 1967 lines and international legitimacy. Bears costs of territorial withdrawal, security risk, and domestic political opposition. Exit means abandoning the diplomatic architecture that underwrites its regional standing.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__two_state_coexistence_reading, israeli_state_institutions, agenda_setter,
    institutional, generational, constrained, national).

% Receives recognized governing institutions, territorial basis for statehood, and international standing. Depends on Israeli security cooperation and international aid flows. Bears costs of limited sovereignty, security subcontracting, and domestic legitimacy deficits. Exit means losing the only internationally recognized path to statehood.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__two_state_coexistence_reading, palestinian_authority_institutions, agenda_setter,
    organized, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(territorial_legitimacy_dual__two_state_coexistence_reading, palestinian_authority_institutions, beneficiary).

% Gains a stable, manageable framework for conflict resolution that fits existing institutional tools (UNSC resolutions, Quartet, bilateral aid). Avoids open-ended crisis management. Low direct cost; cost is borne by parties on the ground. Exit is easy — the framework is one item in a portfolio of managed conflicts.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__two_state_coexistence_reading, international_diplomatic_establishment, beneficiary,
    institutional, generational, arbitrage, global).

% Right of return is limited to the new Palestinian state, not original homes inside 1948 boundaries. The framework treats their core claim as resolved by compensation and symbolic return, not physical return. No meaningful exit — their identity is constituted by the displacement the framework declares settled. They are not party to the agreement that extinguishes their claim.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__two_state_coexistence_reading, palestinian_refugees_diaspora, payer,
    powerless, biographical, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(territorial_legitimacy_dual__two_state_coexistence_reading, palestinian_refugees_diaspora, excluded).

% Framework requires evacuation from settlements beyond 1967 lines. Their presence is the primary physical obstacle to implementation. They reject the framework's legitimacy and have veto power through domestic Israeli politics. Exit means abandoning homes and ideological commitment; staying means preventing the framework's realization.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__two_state_coexistence_reading, israeli_settlers_west_bank, payer,
    organized, biographical, trapped, regional).
narrative_ontology:stakeholder_secondary_role(territorial_legitimacy_dual__two_state_coexistence_reading, israeli_settlers_west_bank, excluded).

% Live under Israeli military control in Area C pending final status. The framework promises eventual sovereignty but delivers continued permit regime, demolitions, and settlement expansion in the interim. No exit — cannot leave Area C without losing land rights, cannot stay without submitting to military law. The framework's interim period is their permanent condition.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__two_state_coexistence_reading, palestinian_residents_area_c, payer,
    powerless, immediate, trapped, local).

% Reject the framework's core premise (1948 legitimacy for Israel, 1967 boundaries as final). Hold veto through control of Gaza and armed capacity. Identity fused to resistance framework that forecloses this reading. Not consulted, not consenting; their exclusion is structural to the framework's coherence.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__two_state_coexistence_reading, hamas_islamist_factions, excluded,
    organized, generational, identity_locked, regional).

% View 1967 boundaries as religiously illegitimate concession of biblical land. Hold veto through settler movement and influence on Israeli right. Identity fused to land-redemption narrative that forecloses territorial partition. Excluded by design — their inclusion would fracture the Israeli consensus the framework requires.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__two_state_coexistence_reading, religious_zionist_factions, excluded,
    organized, generational, identity_locked, national).

% Sees the full structure: a genuine coordination function (ending zero-sum war) fused with asymmetric extraction (refugees lose return, settlers lose homes, Area C residents lose time). The framework's persistence depends on active enforcement (security coordination, aid conditionality, diplomatic pressure) against excluded parties on both sides.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__two_state_coexistence_reading, analytical_observer, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Ends the zero-sum contest over the land by establishing mutually recognized sovereign spaces: Israel within 1967 lines, Palestine in West Bank/Gaza. Solves the security dilemma through cooperation instead of conquest; solves the legitimacy dilemma through reciprocal recognition instead of elimination.
% TRANSFER_FUNCTION: Transfers territorial control (West Bank/Gaza) from Israeli military administration to Palestinian sovereignty; transfers security responsibility from unilateral Israeli control to joint mechanisms; transfers refugee claims from right-of-return-to-original-homes to right-of-return-to-Palestinian-state-plus-compensation; transfers international legitimacy from contested to recognized for both states.
% ABSENT_VOICES: Palestinian refugees (right of return extinguished), West Bank settlers (evacuation required), Area C residents (indefinite military rule), Hamas/Islamist factions (ideologically foreclosed), Religious Zionist factions (ideologically foreclosed). These voices are not in the room because their inclusion would make the compromise impossible — the framework's coherence requires their exclusion.
% DISAPPEARANCE_RATIONALE: If the two-state framework vanished overnight, the default would not be status quo ante but uncontrolled confrontation: no agreed borders, no security coordination, no recognized Palestinian address for diplomacy, no international template for resolution. The region would revert to raw power competition with higher violence and no diplomatic off-ramp. The framework's disappearance rearranges the world because it is the only structured alternative to unlimited conflict.
% FOUNDING_PROBLEM: How to resolve two peoples' legitimate claims to the same territory without endless war, ethnic cleansing, or permanent apartheid. The 1947 UN partition failed; 1948 war created refugee crisis and armistice lines; 1967 war brought all territory under Israeli control. The founding problem: a framework that grants both peoples self-determination in partitioned space while addressing security and refugee dimensions.
% FOUNDING_PROBLEM_CORROBORATION: International consensus (UN, EU, US, Arab League) attests the problem remains live — the two-state solution is still the official framework. Israeli and Palestinian leaderships (Rabin/Peres, Arafat/Abbas) attested it was live at Oslo but diverge on whether it remains viable. Settler movement, Hamas, and refugee advocates attest the problem is misdiagnosed — the land is not partitionable, or the partition legitimizes injustice. No consensus outside the benefiting institutional centers.
narrative_ontology:disappearance_verdict(territorial_legitimacy_dual__two_state_coexistence_reading, world_rearranges).
narrative_ontology:founding_problem_status(territorial_legitimacy_dual__two_state_coexistence_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(territorial_legitimacy_dual__two_state_coexistence_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(territorial_legitimacy_dual__two_state_coexistence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(territorial_legitimacy_dual__two_state_coexistence_reading, 0.42, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Base extractiveness (0.42) reflects that the framework extracts real concessions from excluded parties (refugees' return, settlers' homes, Area C residents' time) while delivering genuine coordination (ending war, mutual recognition). Suppression (0.58) is substantial because implementation requires active enforcement against spoilers on both sides — security cooperation, aid conditionality, diplomatic pressure, military control of Area C. Theater ratio (0.31) has risen steadily: the peace process machinery (negotiations, summits, roadmaps) increasingly performs the framework while settlement expansion and refugee exclusion make implementation recede. Accessibility collapse (0.38) is moderate — alternatives (one-state, confederation, status quo) remain conceptually available but are politically suppressed. Resistance (0.62) is high from excluded parties on both sides who reject the framework's legitimacy.
 *
 * PERSPECTIVAL GAP:
 *   From the Israeli/Palestinian institutional seats, this is a rope (genuine coordination solving an existential security dilemma). From the refugee/settler/Area C seats, it is a snare (extraction enforced by excluding their voices). From the international establishment seat, it is a scaffold (transitional support meant to produce a final status that never arrives). The engine computes these per-seat divergences from the structural data — the claimed type (tangled_rope) names the hybrid at the system level.
 *
 * DIRECTIONALITY LOGIC:
 *   Israeli and Palestinian institutions are agenda-setters with constrained exit — they built and administer the framework but cannot easily abandon it without losing its benefits (recognition, security cooperation, international standing). International establishment is a beneficiary with arbitrage exit — it gains a manageable conflict framework at near-zero cost. Refugees are identity-locked victims: their self-concept is constituted by the displacement the framework declares settled. Settlers are trapped victims: physically embedded in the territory the framework requires evacuating. Area C residents are trapped victims: no exit without losing rights. Hamas and religious Zionists are identity-locked excluded: their ideological frameworks foreclose this reading entirely. The analytical observer sees the full extraction/coordination hybrid.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (partition with mutual recognition) remains contested — not dead, not universally live. The framework persists because the institutional beneficiaries (both states' establishments, international diplomacy) have enough power to maintain it, while the excluded victims lack the power to force its revision or replacement. Mandatrophy is unresolved: the coordination function (ending war) is real but the extraction function (managing refugees/settlers/Area C through indefinite deferral) has become the framework's actual steady state. The sunset clause (final status agreement) was never triggered — the scaffold became a tangled rope.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coordination_extraction_boundary,
    'Where does the genuine coordination function (ending zero-sum war) end and the extractive function (managing excluded populations through indefinite deferral) begin?',
    'Counterfactual: if security cooperation and mutual recognition were fully implemented WITHOUT refugee exclusion, settler evacuation, and Area C military rule, would the framework still extract? If yes, extraction is intrinsic to the partition logic; if no, extraction is contingent on implementation failures.',
    'If extraction is intrinsic, the tangled_rope classification is structural — no implementation can purify it. If contingent, a ''pure'' two-state implementation could be a rope, and the current classification reflects a degraded instance.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_extraction_boundary, conceptual, 'Whether the extraction-coordination hybrid is essential to partition or a contingent degradation.').

omega_variable(
    refugee_return_ambiguity,
    'Is ''right of return limited to Palestinian state'' a coherent compromise or a structural denial that renders the framework extractive toward refugees?',
    'Refugee survey data on acceptance of compensation/return-to-Palestinian-state vs. return-to-original-homes; legal analysis of whether international law permits extinguishing individual right of return via inter-state agreement.',
    'If refugees would accept the compromise under fair conditions, the framework''s victimization of refugees is contingent on political manipulation. If the right is individually inalienable under international law, the framework''s treatment of refugees is structurally extractive regardless of political conditions.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(refugee_return_ambiguity, empirical, 'Whether the refugee compromise is a genuine settlement or a rights extinguishment.').

omega_variable(
    settler_evacuation_feasibility,
    'Can the framework''s requirement to evacuate 500,000+ settlers be implemented without civil conflict in Israel, and does the answer affect the framework''s classification?',
    'Historical precedents (Gaza 2005, Sinai 1982) for evacuation scale and domestic fallout; Israeli political sociology of the settler movement''s veto capacity; scenario modeling of evacuation orders.',
    'If evacuation is infeasible without civil war, the framework''s coordination function is theoretically unrealizable — it extracts from refugees and Area C residents while delivering a Palestinian state that cannot materialize. This would push classification toward snare. If feasible, the tangled_rope stands.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(settler_evacuation_feasibility, empirical, 'Whether the framework''s core territorial requirement is implementable within the Israeli polity.').

omega_variable(
    kernel_reading_relation_zionist,
    'Does the two-state reading''s core premise (1948 legitimacy for both peoples) logically foreclose the zionist_refuge_reading''s premise (Israel''s legitimacy from persecution/divine promise, Palestinian claims secondary), or do they coexist as competing frameworks?',
    'Analyze whether a single political framework could simultaneously hold: (a) Israel''s legitimacy derives from Jewish persecution and divine promise, AND (b) Palestinian legitimacy derives from 1948 displacement with equal moral weight. If mutually exclusive, forecloses. If different factions hold each within the same polity, coexists_with.',
    'If forecloses, the two-state reading structurally displaces the zionist reading within any unified framework — the zionist reading can only persist in a separate, unreconciled discourse. If coexists_with, both remain live in Israeli politics (as they currently do), creating permanent structural tension.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_relation_zionist, conceptual, 'Structural relation between two-state and zionist_refuge readings of the territorial legitimacy kernel.').

omega_variable(
    kernel_reading_relation_palestinian,
    'Does the two-state reading''s core premise (right of return limited to Palestinian state) logically foreclose the palestinian_autochthony_reading''s premise (right of return to original homes as inalienable), or do they coexist?',
    'Analyze whether a single Palestinian national framework could simultaneously hold: (a) return is to the Palestinian state only, AND (b) return is to original homes inside 1948 boundaries as individual inalienable right. Test against PLO/PA historical positions and refugee movement positions.',
    'If forecloses, the two-state reading structurally displaces the autochthony reading within Palestinian national discourse — the autochthony reading survives only in diaspora/excluded spaces. If coexists_with, both remain live (as they currently do), with PA endorsing two-state while refugee movements maintain autochthony.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_relation_palestinian, conceptual, 'Structural relation between two-state and palestinian_autochthony readings of the territorial legitimacy kernel.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the framework''s suppression structural (Israeli military control, PA security coordination, international aid conditionality) or internalized (Palestinian/Israeli publics accepting the framework''s exclusions as ''realistic'')?',
    'Post-exit suppression trajectory: if suppression persists after the framework collapses (e.g., if two-state dies, do refugees still self-censor return claims? do settlers still self-justify presence?), the internalized component is significant. Compare pre-Oslo and post-Oslo discourse boundaries.',
    'If internalized suppression is significant, the framework''s effective suppression is higher than structural measures suggest — it has shaped the imagination of the possible. This would increase the constraint''s extractiveness from excluded parties even without active enforcement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression in the two-state framework''s maintenance.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(territorial_legitimacy_dual__two_state_coexistence_reading, 1993, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(terr_tr_t1993, territorial_legitimacy_dual__two_state_coexistence_reading, theater_ratio, 1993, 0.12).
narrative_ontology:measurement(terr_tr_t2000, territorial_legitimacy_dual__two_state_coexistence_reading, theater_ratio, 2000, 0.18).
narrative_ontology:measurement(terr_tr_t2005, territorial_legitimacy_dual__two_state_coexistence_reading, theater_ratio, 2005, 0.22).
narrative_ontology:measurement(terr_tr_t2010, territorial_legitimacy_dual__two_state_coexistence_reading, theater_ratio, 2010, 0.25).
narrative_ontology:measurement(terr_tr_t2015, territorial_legitimacy_dual__two_state_coexistence_reading, theater_ratio, 2015, 0.28).
narrative_ontology:measurement(terr_tr_t2020, territorial_legitimacy_dual__two_state_coexistence_reading, theater_ratio, 2020, 0.3).
narrative_ontology:measurement(terr_tr_t2024, territorial_legitimacy_dual__two_state_coexistence_reading, theater_ratio, 2024, 0.31).

% Extraction over time
narrative_ontology:measurement(terr_be_t1993, territorial_legitimacy_dual__two_state_coexistence_reading, base_extractiveness, 1993, 0.28).
narrative_ontology:measurement(terr_be_t2000, territorial_legitimacy_dual__two_state_coexistence_reading, base_extractiveness, 2000, 0.35).
narrative_ontology:measurement(terr_be_t2005, territorial_legitimacy_dual__two_state_coexistence_reading, base_extractiveness, 2005, 0.38).
narrative_ontology:measurement(terr_be_t2010, territorial_legitimacy_dual__two_state_coexistence_reading, base_extractiveness, 2010, 0.4).
narrative_ontology:measurement(terr_be_t2015, territorial_legitimacy_dual__two_state_coexistence_reading, base_extractiveness, 2015, 0.41).
narrative_ontology:measurement(terr_be_t2020, territorial_legitimacy_dual__two_state_coexistence_reading, base_extractiveness, 2020, 0.42).
narrative_ontology:measurement(terr_be_t2024, territorial_legitimacy_dual__two_state_coexistence_reading, base_extractiveness, 2024, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(terr_su_t1993, territorial_legitimacy_dual__two_state_coexistence_reading, suppression_requirement, 1993, 0.45).
narrative_ontology:measurement(terr_su_t2000, territorial_legitimacy_dual__two_state_coexistence_reading, suppression_requirement, 2000, 0.55).
narrative_ontology:measurement(terr_su_t2005, territorial_legitimacy_dual__two_state_coexistence_reading, suppression_requirement, 2005, 0.58).
narrative_ontology:measurement(terr_su_t2010, territorial_legitimacy_dual__two_state_coexistence_reading, suppression_requirement, 2010, 0.57).
narrative_ontology:measurement(terr_su_t2015, territorial_legitimacy_dual__two_state_coexistence_reading, suppression_requirement, 2015, 0.58).
narrative_ontology:measurement(terr_su_t2020, territorial_legitimacy_dual__two_state_coexistence_reading, suppression_requirement, 2020, 0.58).
narrative_ontology:measurement(terr_su_t2024, territorial_legitimacy_dual__two_state_coexistence_reading, suppression_requirement, 2024, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(territorial_legitimacy_dual__two_state_coexistence_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(territorial_legitimacy_dual__two_state_coexistence_reading, 0.12).
narrative_ontology:affects_constraint(territorial_legitimacy_dual__two_state_coexistence_reading, territorial_legitimacy_dual__zionist_refuge_reading).
narrative_ontology:affects_constraint(territorial_legitimacy_dual__two_state_coexistence_reading, territorial_legitimacy_dual__palestinian_autochthony_reading).
narrative_ontology:affects_constraint(territorial_legitimacy_dual__two_state_coexistence_reading, oslo_accords_architecture).
narrative_ontology:affects_constraint(territorial_legitimacy_dual__two_state_coexistence_reading, palestinian_statehood_recognition).
narrative_ontology:affects_constraint(territorial_legitimacy_dual__two_state_coexistence_reading, israeli_settlement_enterprise).
narrative_ontology:affects_constraint(territorial_legitimacy_dual__two_state_coexistence_reading, palestinian_refugee_regime).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the territorial_legitimacy_dual kernel. The zionist_refuge_reading and palestinian_autochthony_readings are sibling constraints with different ε, different victim/beneficiary structures, and different claimed types. The ε-invariance principle requires separate stories: the two-state reading has moderate extractiveness (0.42) because it coordinates genuine security/legitimacy exchange while extracting from refugees/settlers/Area C residents. The zionist reading likely has higher extractiveness (Palestinian claims subordinated). The autochthony reading likely has different extraction profile (Israeli legitimacy contested). All three linked via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(territorial_legitimacy_dual__two_state_coexistence_reading, organized, 0.75).
constraint_indexing:directionality_override(territorial_legitimacy_dual__two_state_coexistence_reading, powerless, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
