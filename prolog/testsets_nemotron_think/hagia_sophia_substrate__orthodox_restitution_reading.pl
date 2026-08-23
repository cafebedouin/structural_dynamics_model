% ============================================================================
% CONSTRAINT STORY: hagia_sophia_substrate__orthodox_restitution_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hagia_sophia_substrate__orthodox_restitution_reading, []).

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
 *   constraint_id: hagia_sophia_substrate__orthodox_restitution_reading
 *   human_readable: Hagia Sophia Orthodox Restitution Claim
 *   domain: cultural_heritage/sovereignty/religious_authority
 *
 * SUMMARY:
 *   The Orthodox restitution reading of the Hagia Sophia substrate asserts
 *   that the site's legitimacy derives exclusively from its 537 CE founding
 *   as the Great Church of Constantinople, and that this founding confers a
 *   perpetual ecclesiastical authority that no conquest, conversion, or
 *   secular decree can extinguish. The reading demands either return to
 *   Orthodox liturgical use or neutral/museum status as the only arrangements
 *   honoring Byzantine origins. Structurally, this is an external normative
 *   claim with no enforcement pathway — Turkey exercises de facto and de jure
 *   control, the site has functioned as a mosque since 2020, and no
 *   international mechanism can compel restitution. Yet the claim persists as
 *   a powerful symbolic constraint: it structures Orthodox diaspora identity,
 *   fuels Greek diplomatic narratives, and generates continuous friction in
 *   Turkish-Western relations. The claim's extractiveness is low materially
 *   (no resource transfer) but high symbolically (it extracts legitimacy from
 *   the current arrangement and transfers it to the claimants).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hagia_sophia_substrate__orthodox_restitution_reading, 0.28).
domain_priors:suppression_score(hagia_sophia_substrate__orthodox_restitution_reading, 0.35).
domain_priors:theater_ratio(hagia_sophia_substrate__orthodox_restitution_reading, 0.72).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hagia_sophia_substrate__orthodox_restitution_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(hagia_sophia_substrate__orthodox_restitution_reading, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(hagia_sophia_substrate__orthodox_restitution_reading, theater_ratio, 0.72).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hagia_sophia_substrate__orthodox_restitution_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(hagia_sophia_substrate__orthodox_restitution_reading, resistance, 0.78).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hagia_sophia_substrate__orthodox_restitution_reading, mountain).
narrative_ontology:human_readable(hagia_sophia_substrate__orthodox_restitution_reading, "Hagia Sophia Orthodox Restitution Claim").
narrative_ontology:topic_domain(hagia_sophia_substrate__orthodox_restitution_reading, "cultural_heritage/sovereignty/religious_authority").

domain_priors:emerges_naturally(hagia_sophia_substrate__orthodox_restitution_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hagia_sophia_substrate__orthodox_restitution_reading, 'da5a4dfa-7940-4696-9e4b-bc2873665ec6').
narrative_ontology:cs_kernel_codification('da5a4dfa-7940-4696-9e4b-bc2873665ec6', fixed_text).
narrative_ontology:cs_authority_grounding('da5a4dfa-7940-4696-9e4b-bc2873665ec6', lineage).
narrative_ontology:cs_interpretation_layer_present('da5a4dfa-7940-4696-9e4b-bc2873665ec6').
narrative_ontology:cs_reading_relation('da5a4dfa-7940-4696-9e4b-bc2873665ec6', hagia_sophia_substrate__islamic_sovereignty_reading, forecloses).
narrative_ontology:cs_reading_relation('da5a4dfa-7940-4696-9e4b-bc2873665ec6', hagia_sophia_substrate__universal_heritage_reading, coexists_with).
narrative_ontology:cs_axiom('da5a4dfa-7940-4696-9e4b-bc2873665ec6', foundational, byzantine_founding_confers_perpetual_ecclesiastical_authority).
narrative_ontology:cs_axiom_status(byzantine_founding_confers_perpetual_ecclesiastical_authority, holdable).
narrative_ontology:cs_axiom_grounding('da5a4dfa-7940-4696-9e4b-bc2873665ec6', byzantine_founding_confers_perpetual_ecclesiastical_authority, deontological).
narrative_ontology:cs_axiom('da5a4dfa-7940-4696-9e4b-bc2873665ec6', secondary, neutral_status_honors_byzantine_origins).
narrative_ontology:cs_axiom_status(neutral_status_honors_byzantine_origins, holdable).
narrative_ontology:cs_axiom_grounding('da5a4dfa-7940-4696-9e4b-bc2873665ec6', neutral_status_honors_byzantine_origins, conventional).
narrative_ontology:cs_reference_frame('da5a4dfa-7940-4696-9e4b-bc2873665ec6', byzantine_ecclesiastical_primacy).
narrative_ontology:cs_drift_state('da5a4dfa-7940-4696-9e4b-bc2873665ec6', post_1453_conquest, gap(authority_erosion, severe, false)).
narrative_ontology:cs_created_at('da5a4dfa-7940-4696-9e4b-bc2873665ec6', '').
narrative_ontology:cs_kernel_id(hagia_sophia_substrate__orthodox_restitution_reading, hagia_sophia_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hagia_sophia_substrate__orthodox_restitution_reading, eastern_orthodox_diaspora).
narrative_ontology:constraint_beneficiary(hagia_sophia_substrate__orthodox_restitution_reading, greek_state).
narrative_ontology:constraint_victim(hagia_sophia_substrate__orthodox_restitution_reading, turkish_sovereignty).
narrative_ontology:constraint_victim(hagia_sophia_substrate__orthodox_restitution_reading, islamic_worship_continuity).
narrative_ontology:constraint_vindicates(hagia_sophia_substrate__orthodox_restitution_reading, byzantine_founding_confers_perpetual_ecclesiastical_authority).
narrative_ontology:constraint_vindicates(hagia_sophia_substrate__orthodox_restitution_reading, historical_injustice_requires_restitution).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Global Orthodox communities (Greek, Russian, Serbian, Romanian, etc.) for whom the Great Church is the symbolic center of Orthodox Christianity. The restitution claim structures collective identity, liturgical memory, and transnational solidarity. Exit from the claim would mean abandoning a core identity anchor; the claim is internalized as constitutive of Orthodox self-understanding.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__orthodox_restitution_reading, eastern_orthodox_diaspora, beneficiary,
    organized, generational, identity_locked, global).

% Uses the restitution claim as diplomatic leverage in Greek-Turkish relations (Aegean disputes, Cyprus, EU accession). The claim generates soft power and mobilizes diaspora support. Exit would mean surrendering a key bargaining chip; constrained by domestic political cost of appearing to abandon Orthodox heritage.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__orthodox_restitution_reading, greek_state, beneficiary,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(hagia_sophia_substrate__orthodox_restitution_reading, greek_state, agenda_setter).

% The Turkish Republic inherits Ottoman sovereignty over the site. The restitution claim constitutes an external normative demand on Turkish territory, treated as a violation of sovereign equality and the Treaty of Lausanne framework. Exit is trapped: conceding the claim would be seen as existential surrender; resisting it incurs diplomatic costs and fuels the very narrative the claim generates.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__orthodox_restitution_reading, turkish_sovereignty, payer,
    institutional, generational, trapped, national).

% Continuous Islamic worship since 1453 (mosque 1453-1934, museum 1934-2020, mosque since 2020). The waqf (endowment) of Mehmed II establishes Islamic legal title. The restitution claim threatens to interrupt this continuity again. Exit is identity-locked: the site's Islamic character is fused with Turkish Muslim identity and Ottoman legitimacy narrative.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__orthodox_restitution_reading, islamic_worship_continuity, payer,
    organized, generational, identity_locked, national).

% Spiritual center of Orthodox Christianity, based in Istanbul (Phanar). Formally advocates restitution but operates under Turkish state surveillance. The claim legitimizes the Patriarchate's ecumenical primacy. Exit constrained: pushing too hard risks expulsion or further restrictions; silence undermines its raison d'être.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__orthodox_restitution_reading, ecumenical_patriarchate, agenda_setter,
    institutional, generational, constrained, global).

% Monitors the site's Outstanding Universal Value under the 1985 World Heritage inscription. Officially neutral, pressured by both sides. Its technical reports on conservation and access are the only neutral epistemic ground. Exit is analytical: it can only observe and report, not adjudicate sovereignty.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__orthodox_restitution_reading, unesco_world_heritage_committee, observer,
    institutional, generational, analytical, global).

% Descendants of the early republican elite who supported the 1934 museum conversion as a symbol of secular modernity. Would object to both Islamic reconversion and Orthodox restitution as instrumentalizations of heritage. Excluded from the binary discourse; their voice is marginalized by the polarized framing.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__orthodox_restitution_reading, secular_turkish_citizens, excluded,
    moderate, biographical, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates transnational Orthodox identity and Greek diplomatic posture around a historical justice claim, providing a stable focal point for mobilization across generations despite zero implementation probability.
% TRANSFER_FUNCTION: Moves symbolic capital (moral authority, victimhood status, civilizational legitimacy) and diplomatic leverage from Turkish sovereignty and Islamic worship continuity to the Eastern Orthodox diaspora and Greek state.
% ABSENT_VOICES: Secular Turkish citizens who view the site as shared human heritage beyond religious claims; minority Muslim communities in Greece who see symmetry in mosque/church conversions; other autocephalous Orthodox churches (e.g., Moscow) that prioritize different geopolitical equlibria over Constantinople's claim.
% DISAPPEARANCE_RATIONALE: If the claim vanished overnight: Greek diplomatic leverage on Hagia Sophia would evaporate; Orthodox diaspora mobilization would lose its primary symbolic anchor; Turkish sovereignty would face one less external normative challenge; Islamic worship continuity would be uncontested. But the material status quo (mosque under Turkish control) would not change — the claim is already practically dormant. The parties dispute whether the symbolic rearrangement constitutes a real world change.
% FOUNDING_PROBLEM: The 1453 conversion of the Great Church of Hagia Sophia into a mosque was an illegitimate seizure of Christianity's greatest cathedral; the founding problem is the unrectified historical injustice of that conversion and the subsequent denial of the site's primordial ecclesiastical character.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated by: (1) Historical scholarship on the 1453 conquest terms (critics note the surrender terms promised protection of churches, not conversion of the Great Church); (2) Orthodox theological tradition treating Hagia Sophia as the 'Great Church' whose desecration is a continuing wound; (3) International legal scholars of cultural heritage (e.g., Francesco Francioni, Ana Filipa Vrdoljak) who argue that conversion of sacred sites violates the 1954 Hague Convention's spirit. Contested by: Turkish state historiography (conquest = legitimate title transfer), Islamic waqf tradition (Mehmed II's endowment is perpetual), and UNESCO's procedural neutrality.
narrative_ontology:disappearance_verdict(hagia_sophia_substrate__orthodox_restitution_reading, contested).
narrative_ontology:founding_problem_status(hagia_sophia_substrate__orthodox_restitution_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hagia_sophia_substrate__orthodox_restitution_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(hagia_sophia_substrate__orthodox_restitution_reading, 'none', 1).
narrative_ontology:epsilon_provenance(hagia_sophia_substrate__orthodox_restitution_reading, 0.28, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hagia_sophia_substrate__orthodox_restitution_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(hagia_sophia_substrate__orthodox_restitution_reading, ExtMetricName, E),
    domain_priors:suppression_score(hagia_sophia_substrate__orthodox_restitution_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(hagia_sophia_substrate__orthodox_restitution_reading),
    narrative_ontology:constraint_metric(hagia_sophia_substrate__orthodox_restitution_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(hagia_sophia_substrate__orthodox_restitution_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(hagia_sophia_substrate__orthodox_restitution_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.28) reflects symbolic/diplomatic extraction rather than material: the claim extracts moral authority from Turkey's sovereignty and Islamic continuity, converting it into diaspora cohesion and Greek leverage. Suppression (0.35) is moderate: the claim suppresses the legitimacy of the current arrangement in international discourse but lacks enforcement machinery. Theater ratio (0.72) is high because the claim's primary function is performative — it sustains identity and diplomatic posture rather than achieving its stated goal. Accessibility collapse (0.45) is moderate: alternatives (universal heritage, shared stewardship) remain discursively available but are politically marginalized by the binary framing. Resistance (0.78) is high: Turkey treats the claim as an existential sovereignty threat and actively counters it through state media, diplomacy, and domestic law.
 *
 * PERSPECTIVAL GAP:
 *   From the beneficiary seats (diaspora, Greece), the constraint appears as a rope/mountain — a coordination structure for identity and a fixed historical truth. From the payer seats (Turkey, Islamic continuity), it appears as a snare — an external extraction claim that suppresses their legitimate title and worship. The Patriarchate sits in between: it sets the agenda but is captive to Turkish tolerance. The engine will compute this divergence from the declared power/exit structures.
 *
 * DIRECTIONALITY LOGIC:
 *   Eastern Orthodox diaspora and Greek state are structural beneficiaries: they collect symbolic capital and diplomatic leverage (d near 0.1-0.2). Turkish sovereignty and Islamic worship continuity are structural payers: they bear the legitimacy cost and defensive burden (d near 0.8-0.9). Ecumenical Patriarchate is agenda_setter but constrained — it advocates the claim but cannot exit the Turkish surveillance context (d ~0.4). UNESCO is analytical observer (d=0.5). Secular Turkish citizens are excluded — they would reject the binary but have no seat (d undefined).
 *
 * MANDATROPHY ANALYSIS:
 *   The claim's founding problem (1453 conversion as injustice) is live in the reading's internal logic but dead in material reality — the site has been under Islamic/Turkish control for 570+ years, the waqf is legally entrenched, and no power can reverse it. The arrangement persists as a zombie constraint: it generates real diplomatic friction and identity mobilization while its stated purpose (restitution) is impossible. This is mandatrophy in pure form — the mandate (restore ecclesiastical control) has outlived all implementation pathways, yet the constraint thrives symbolically.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_structure,
    'How does the contested kernel structure (three readings of one substrate) affect the classification of each reading''s constraint?',
    'Cross-reading comparison of ε, beneficiary/victim sets, and authority grounding. The engine''s inferred_coupling_protocol will detect shared stakeholder names and structural interference.',
    'If readings are treated as independent constraints, each may classify differently (e.g., this reading as mountain→FSM→tangled_rope, Islamic reading as rope, universal as mountain). If the kernel is treated as a unit, cross-contamination may alter all three.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_structure, conceptual, 'Whether the kernel''s multi-reading structure requires joint analysis or permits independent classification.').

omega_variable(
    symbolic_extraction_measurement,
    'Can symbolic/diplomatic extraction be measured on the same ε scale as material extraction, or does it require a separate axis?',
    'Decompose ε into material_extractiveness and symbolic_extractiveness; test whether the engine''s χ computation (which scales by directionality and scope) produces coherent results when ε mixes both types.',
    'If symbolic extraction is commensurable, this reading''s ε=0.28 is defensible. If not, the constraint may need decomposition into material_constraint (ε≈0) and symbolic_constraint (ε≈0.6).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(symbolic_extraction_measurement, conceptual, 'Commensurability of symbolic vs. material extraction in the ε metric.').

omega_variable(
    identity_locked_mechanism,
    'Is the identity_locked exit option for Orthodox diaspora and Islamic worship continuity driven by theological identity fusion, political mobilization, or both — and does the mechanism differ between the two groups?',
    'Comparative sociological study of Orthodox diaspora attachment to Hagia Sophia vs. Turkish Muslim attachment; test whether exit costs are theological (salvific), communal (belonging), or political (loyalty).',
    'If theological, identity_locked is structural and persistent. If political, it may shift with generational change or geopolitical realignment, altering directionality and χ over time.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_locked_mechanism, empirical, 'Mechanism of identity lock for the two primary identity-locked stakeholder groups.').

omega_variable(
    cs_framing_underdetermination,
    'Does the CS framing (kernel=Byzantine founding, authority=lineage) represent the only defensible commitment-system reading, or does a rival framing (kernel=Ottoman waqf, authority=practice) produce a different cs_pattern?',
    'Author the Islamic sovereignty reading as a separate CS constraint and compare cs_pattern outputs. If patterns differ, the framing choice is under-determined and the omega documents the alternative.',
    'If framings produce different patterns, the kernel''s CS structure is inherently contested and no single reading can claim definitive CS classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(cs_framing_underdetermination, conceptual, 'Whether the CS structure declaration commits to one framing of a multi-framed kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hagia_sophia_substrate__orthodox_restitution_reading, 1934, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hagi_tr_t1934, hagia_sophia_substrate__orthodox_restitution_reading, theater_ratio, 1934, 0.3).
narrative_ontology:measurement(hagi_tr_t1955, hagia_sophia_substrate__orthodox_restitution_reading, theater_ratio, 1955, 0.45).
narrative_ontology:measurement(hagi_tr_t1974, hagia_sophia_substrate__orthodox_restitution_reading, theater_ratio, 1974, 0.55).
narrative_ontology:measurement(hagi_tr_t1990, hagia_sophia_substrate__orthodox_restitution_reading, theater_ratio, 1990, 0.62).
narrative_ontology:measurement(hagi_tr_t2006, hagia_sophia_substrate__orthodox_restitution_reading, theater_ratio, 2006, 0.68).
narrative_ontology:measurement(hagi_tr_t2020, hagia_sophia_substrate__orthodox_restitution_reading, theater_ratio, 2020, 0.7).
narrative_ontology:measurement(hagi_tr_t2024, hagia_sophia_substrate__orthodox_restitution_reading, theater_ratio, 2024, 0.72).

% Extraction over time
narrative_ontology:measurement(hagi_be_t1934, hagia_sophia_substrate__orthodox_restitution_reading, base_extractiveness, 1934, 0.1).
narrative_ontology:measurement(hagi_be_t1955, hagia_sophia_substrate__orthodox_restitution_reading, base_extractiveness, 1955, 0.15).
narrative_ontology:measurement(hagi_be_t1974, hagia_sophia_substrate__orthodox_restitution_reading, base_extractiveness, 1974, 0.22).
narrative_ontology:measurement(hagi_be_t1990, hagia_sophia_substrate__orthodox_restitution_reading, base_extractiveness, 1990, 0.25).
narrative_ontology:measurement(hagi_be_t2006, hagia_sophia_substrate__orthodox_restitution_reading, base_extractiveness, 2006, 0.27).
narrative_ontology:measurement(hagi_be_t2020, hagia_sophia_substrate__orthodox_restitution_reading, base_extractiveness, 2020, 0.28).
narrative_ontology:measurement(hagi_be_t2024, hagia_sophia_substrate__orthodox_restitution_reading, base_extractiveness, 2024, 0.28).

% Suppression requirement over time
narrative_ontology:measurement(hagi_su_t1934, hagia_sophia_substrate__orthodox_restitution_reading, suppression_requirement, 1934, 0.1).
narrative_ontology:measurement(hagi_su_t1955, hagia_sophia_substrate__orthodox_restitution_reading, suppression_requirement, 1955, 0.2).
narrative_ontology:measurement(hagi_su_t1974, hagia_sophia_substrate__orthodox_restitution_reading, suppression_requirement, 1974, 0.25).
narrative_ontology:measurement(hagi_su_t1990, hagia_sophia_substrate__orthodox_restitution_reading, suppression_requirement, 1990, 0.3).
narrative_ontology:measurement(hagi_su_t2006, hagia_sophia_substrate__orthodox_restitution_reading, suppression_requirement, 2006, 0.32).
narrative_ontology:measurement(hagi_su_t2020, hagia_sophia_substrate__orthodox_restitution_reading, suppression_requirement, 2020, 0.34).
narrative_ontology:measurement(hagi_su_t2024, hagia_sophia_substrate__orthodox_restitution_reading, suppression_requirement, 2024, 0.35).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hagia_sophia_substrate__orthodox_restitution_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(hagia_sophia_substrate__orthodox_restitution_reading, 0.08).
narrative_ontology:affects_constraint(hagia_sophia_substrate__orthodox_restitution_reading, hagia_sophia_substrate__islamic_sovereignty_reading).
narrative_ontology:affects_constraint(hagia_sophia_substrate__orthodox_restitution_reading, hagia_sophia_substrate__universal_heritage_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the hagia_sophia_substrate kernel. The Orthodox restitution reading declares Byzantine founding as the kernel's legitimacy source (authority_grounding=lineage). The Islamic sovereignty reading declares the 1453 conquest/waqf as the legitimacy source (authority_grounding=practice). The universal heritage reading declares UNESCO OUV as the legitimacy source (authority_grounding=expertise). The three readings form a constraint family linked by affects_constraints. ε values diverge: this reading ε=0.28 (symbolic), Islamic reading ε≈0.15 (material enforcement), Universal reading ε≈0.05 (coordination).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(hagia_sophia_substrate__orthodox_restitution_reading, institutional, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
