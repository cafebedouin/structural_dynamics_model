% ============================================================================
% CONSTRAINT STORY: territorial_legitimacy__security_necessity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_territorial_legitimacy__security_necessity_reading, []).

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
    constraint_indexing:constraint_classification/3,
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
 *   constraint_id: territorial_legitimacy__security_necessity_reading
 *   human_readable: Territorial Legitimacy via Security Necessity (Strategic Depth Doctrine)
 *   domain: political/legal/territorial
 *
 * SUMMARY:
 *   The security-necessity reading holds that Israel's control of the West
 *   Bank and presence in the Golan Heights are legitimated by
 *   defensible-borders logic: the 1967 lines left Israeli population centers
 *   within a narrow, easily-overrun strip, and strategic depth is a
 *   proportionate defensive response, not territorial acquisition. Under this
 *   reading, Palestinian sovereignty is legitimate in principle but
 *   conditional on demilitarization and continued Israeli security control
 *   (air space, border crossings, early warning stations), and settlements
 *   are read as a forward security presence rather than as colonization. The
 *   claim (tangled_rope) and the authored metrics diverge deliberately: the
 *   doctrine genuinely coordinates a real historical military concern for the
 *   beneficiary population, while the same structure imposes escalating
 *   administrative extraction on Palestinian and Golan residents who bear its
 *   costs without having agreed to its terms.
 *
 * KEY AGENTS:
 *   - israeli_state_security_apparatus: agenda_setter/beneficiary (institutional/arbitrage) — administers and justifies control
 *   - settlement_movement_residents: beneficiary (organized/mobile) — receives state investment framed as forward security
 *   - west_bank_palestinian_residents: payer (powerless/trapped) — bears the administrative and movement costs
 *   - golan_syrian_residents_and_displaced: payer (powerless/trapped) — subordinated claim under strategic-depth rationale
 *   - international_legal_bodies: excluded (institutional/analytical) — occupation-law rulings not treated as binding
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(territorial_legitimacy__security_necessity_reading, 0.68).
domain_priors:suppression_score(territorial_legitimacy__security_necessity_reading, 0.74).
domain_priors:theater_ratio(territorial_legitimacy__security_necessity_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(territorial_legitimacy__security_necessity_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(territorial_legitimacy__security_necessity_reading, suppression_requirement, 0.74).
narrative_ontology:constraint_metric(territorial_legitimacy__security_necessity_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(territorial_legitimacy__security_necessity_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(territorial_legitimacy__security_necessity_reading, resistance, 0.78).

% --- Constraint claim ---
narrative_ontology:constraint_claim(territorial_legitimacy__security_necessity_reading, tangled_rope).
narrative_ontology:human_readable(territorial_legitimacy__security_necessity_reading, "Territorial Legitimacy via Security Necessity (Strategic Depth Doctrine)").
narrative_ontology:topic_domain(territorial_legitimacy__security_necessity_reading, "political/legal/territorial").

domain_priors:requires_active_enforcement(territorial_legitimacy__security_necessity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(territorial_legitimacy__security_necessity_reading, 'cbcf33ae-c3bc-48f4-b54c-dffed28287cc').
narrative_ontology:cs_kernel_codification('cbcf33ae-c3bc-48f4-b54c-dffed28287cc', distributed).
narrative_ontology:cs_authority_grounding('cbcf33ae-c3bc-48f4-b54c-dffed28287cc', distributed).
narrative_ontology:cs_reading_relation('cbcf33ae-c3bc-48f4-b54c-dffed28287cc', territorial_legitimacy__partition_reading, coexists_with).
narrative_ontology:cs_reading_relation('cbcf33ae-c3bc-48f4-b54c-dffed28287cc', territorial_legitimacy__indigenous_continuity_reading, coexists_with).
narrative_ontology:cs_axiom('cbcf33ae-c3bc-48f4-b54c-dffed28287cc', foundational, defensible_borders_supersede_prior_lines).
narrative_ontology:cs_axiom_status(defensible_borders_supersede_prior_lines, holdable).
narrative_ontology:cs_axiom_grounding('cbcf33ae-c3bc-48f4-b54c-dffed28287cc', defensible_borders_supersede_prior_lines, instrumental).
narrative_ontology:cs_axiom('cbcf33ae-c3bc-48f4-b54c-dffed28287cc', foundational, sovereignty_conditional_on_demilitarization).
narrative_ontology:cs_axiom_status(sovereignty_conditional_on_demilitarization, holdable).
narrative_ontology:cs_axiom_grounding('cbcf33ae-c3bc-48f4-b54c-dffed28287cc', sovereignty_conditional_on_demilitarization, instrumental).
narrative_ontology:cs_reference_frame('cbcf33ae-c3bc-48f4-b54c-dffed28287cc', post_1967_defensible_borders_doctrine).
narrative_ontology:cs_drift_state('cbcf33ae-c3bc-48f4-b54c-dffed28287cc', post_oslo_permanent_status_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('cbcf33ae-c3bc-48f4-b54c-dffed28287cc', '').
narrative_ontology:cs_kernel_id(territorial_legitimacy__security_necessity_reading, territorial_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(territorial_legitimacy__security_necessity_reading, israeli_state_security_apparatus).
narrative_ontology:constraint_beneficiary(territorial_legitimacy__security_necessity_reading, settlement_movement_residents).
narrative_ontology:constraint_victim(territorial_legitimacy__security_necessity_reading, west_bank_palestinian_residents).
narrative_ontology:constraint_victim(territorial_legitimacy__security_necessity_reading, golan_syrian_residents_and_displaced).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(territorial_legitimacy__security_necessity_reading, israeli_civilian_population_pre67_lines).
narrative_ontology:constraint_vindicates(territorial_legitimacy__security_necessity_reading, defensible_borders_doctrine).
narrative_ontology:constraint_vindicates(territorial_legitimacy__security_necessity_reading, demilitarization_precondition_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers military control over the West Bank and maintains a presence on the Golan Heights, justifying both as necessary to prevent hostile forces from reaching pre-1967 population centers or the Jordan Valley approach. Sets the terms under which any Palestinian sovereignty would be recognized (demilitarized, without an independent military, air control retained by Israel). Controls the checkpoints, permit regimes, and closure policies that operationalize the security-buffer claim, and can adjust the scope of control unilaterally.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__security_necessity_reading, israeli_state_security_apparatus, agenda_setter,
    institutional, generational, arbitrage, regional).
narrative_ontology:stakeholder_secondary_role(territorial_legitimacy__security_necessity_reading, israeli_state_security_apparatus, beneficiary).

% Live in West Bank settlements whose existence is framed, within this reading, as forward security presence rather than civilian annexation. Receive state subsidies, infrastructure, and military protection tied directly to the security-necessity justification. Retain full Israeli citizenship, freedom of movement, and courts of appeal; their exit from the territory, unlike Palestinian residents', is unconstrained by permit regimes.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__security_necessity_reading, settlement_movement_residents, beneficiary,
    organized, generational, mobile, regional).

% Live under a military administration justified by the security-buffer logic: subject to checkpoints, movement permits, separate legal systems from settlers, land use and building restrictions, and periodic closures. Any prospective sovereignty is made conditional on demilitarization terms they did not negotiate. Cannot exit the arrangement — statelessness and permit dependency bind them to the territory the security doctrine governs.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__security_necessity_reading, west_bank_palestinian_residents, payer,
    powerless, biographical, trapped, local).

% Druze residents remaining on the Golan live under Israeli administration justified as strategic depth against artillery range on northern communities; Syrian families displaced in 1967 hold no path of return, their claim subordinated entirely to the security-buffer rationale. Neither population has a forum in which the strategic-depth justification is tested against their residency or property claims.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__security_necessity_reading, golan_syrian_residents_and_displaced, payer,
    powerless, generational, trapped, local).

% Population centers along the coastal plain and in the Galilee whose narrow topographic margin to hostile lines is the empirical anchor of the strategic-depth argument. Benefit from reduced rocket/artillery range and early-warning distance provided by held territory, without directly administering the control regime or bearing its costs of enforcement.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__security_necessity_reading, israeli_civilian_population_pre67_lines, beneficiary,
    organized, biographical, mobile, regional).

% UN bodies, the ICJ, and most states treat occupation-for-security as time-limited under international humanitarian law and do not accept indefinite strategic depth as a legitimating title to territory or settlement. Their rulings and resolutions are treated by the security-necessity reading as advisory rather than binding, and they have no enforcement mechanism inside the territory itself.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__security_necessity_reading, international_legal_bodies, excluded,
    institutional, generational, analytical, global).

% Assess whether the strategic-depth requirement is still militarily load-bearing given changed missile-range realities, satellite surveillance, and shifting regional alliances (e.g. normalization agreements), or whether it has become a legitimating vocabulary for a territorial position no longer justified by the original military calculus that produced it after 1967.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__security_necessity_reading, regional_security_analysts, observer,
    analytical, generational, analytical, regional).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(territorial_legitimacy__security_necessity_reading, israeli_state_security_apparatus).
narrative_ontology:fixing_cost_class(territorial_legitimacy__security_necessity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides early warning distance and reduced exposure to conventional armor and artillery threats along the eastern and northern approaches, a genuine military concern rooted in the 1967 and 1973 wars when hostile forces reached within a short drive of major population centers.
% TRANSFER_FUNCTION: Moves control over land, water, movement, and building rights from West Bank Palestinian residents and Golan's displaced/remaining population to the Israeli state and settlement population, justified as the price of the security margin the state claims it requires.
% ABSENT_VOICES: Palestinian residents subject to the permit and checkpoint regime, Syrian Golan claimants displaced in 1967, and international legal bodies whose occupation-law rulings are not treated as binding within this reading are structurally outside the room where the security-necessity terms are set.
% DISAPPEARANCE_RATIONALE: If the security-necessity justification were withdrawn overnight, the legal and political basis for continued military administration, settlement expansion, and conditional-sovereignty terms would collapse, forcing renegotiation of borders, settlement status, and security arrangements under a different legitimating frame (likely the partition or indigenous-continuity reading) — the territorial status quo does not survive the removal of this reading.
% FOUNDING_PROBLEM: In 1967, Israeli population centers were within a narrow, artillery-exposed strip near hostile force concentrations in the West Bank and Golan; the strategic-depth doctrine was built to prevent a repeat of that exposure and of the pre-war vulnerability to surprise attack.
% FOUNDING_PROBLEM_CORROBORATION: Israeli security establishment figures and some regional security analysts attest the buffer function remains partially live given rocket proliferation and regional instability. Independent military analysts, several former IDF officials, and international bodies attest that missile range, precision-strike capability, and changed alliance structures have substantially eroded the original topographic rationale, and that the doctrine now functions primarily to justify settlement and administrative continuity rather than to solve the 1967 exposure problem — this corroboration comes from voices outside the settlement and administrative beneficiary set.
narrative_ontology:disappearance_verdict(territorial_legitimacy__security_necessity_reading, world_rearranges).
narrative_ontology:founding_problem_status(territorial_legitimacy__security_necessity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(territorial_legitimacy__security_necessity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(territorial_legitimacy__security_necessity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(territorial_legitimacy__security_necessity_reading, 0.68, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(territorial_legitimacy__security_necessity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(territorial_legitimacy__security_necessity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(territorial_legitimacy__security_necessity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is authored as substantial and rising (0.38 to 0.68 across 1967-2024) because the security rationale has increasingly operated to sustain an administrative and settlement structure whose costs to Palestinian and Golan residents grow even as the original topographic threat calculus (pre-precision-missile, pre-satellite era) has changed. Suppression is high (0.74) and rises steadily because the checkpoint, permit, and closure infrastructure that operationalizes the buffer claim has hardened over decades rather than remaining a temporary wartime posture. Theater ratio rises to 0.42, reflecting a growing share of the security framing devoted to legitimating settlement and administrative permanence rather than addressing the load-bearing military threat that originally justified it. All three metrics share the same interval grid (1967-2024) at six aligned time points.
 *
 * PERSPECTIVAL GAP:
 *   From the security apparatus's own seat, the arrangement is a proportionate, continuously-justified defensive necessity. From the payer seats (West Bank and Golan residents), the same structure operates as an open-ended, unilaterally-administered extraction regime whose 'temporary' security rationale has outlived any sunset. The engine computes this divergence from the structural data (trapped vs. arbitrage exit, powerless vs. institutional power) rather than from either seat's self-description.
 *
 * DIRECTIONALITY LOGIC:
 *   The Israeli security apparatus and settler population sit near the beneficiary end: they set the terms, receive investment and protection, and retain full exit and mobility. West Bank Palestinian residents and Golan's displaced/remaining population sit near the full-target end: trapped exit options, no voice in setting demilitarization terms, and a permit/checkpoint regime that directly operationalizes the doctrine's costs onto them. Israeli civilian populations near the pre-1967 lines are genuine beneficiaries of the reduced-exposure coordination function without directly administering its costs — a distinct seat from the security apparatus itself.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope classification (rather than snare) is chosen because a genuine coordination function existed at founding — real topographic vulnerability in 1967 and 1973 — which this reading holds still partially applies. Reading it as pure extraction would erase the historically genuine security concern; reading it as pure coordination (rope) would erase the asymmetric, escalating cost this reading's own metrics show falling on trapped, powerless populations who never consented to the demilitarization terms set unilaterally by the beneficiary seat. The founding_problem_status is authored contested rather than dead or live, because whether the original military rationale remains load-bearing under 2024 missile-range and alliance realities is precisely the question this reading and its critics dispute.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_disagreement_location,
    'This constraint is one reading (security_necessity_reading) of the contested territorial_legitimacy kernel, alongside partition_reading and indigenous_continuity_reading. The disagreement is located at the founding moment: this reading treats 1967 (not 1948) as the operative legitimating boundary, and treats military topography (not international partition law or continuous habitation) as the legitimating criterion.',
    'No single empirical resolution exists; the readings differ in which historical moment and which normative criterion (international law recognition, indigenous continuity, military necessity) is treated as legitimacy-conferring. Adjudication would require agreement on a shared normative framework that the parties currently do not share.',
    'Under the partition_reading, the 1967 territories are properly understood as occupied territory of a state (Jordan/Egypt) or as territory whose status was never legitimately transferred, making Israeli control extractive from the outset rather than a security buffer. Under the indigenous_continuity_reading, the entire post-1948 territorial framework is read through continuous Palestinian habitation and 1948 displacement, making the security-necessity doctrine a legitimating vocabulary layered atop a prior, unaddressed dispossession. Adopting either sibling reading would substantially lower or eliminate this reading''s authored extractiveness score for the pre-67 state while raising it for the settlement and buffer components as pure extraction.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_disagreement_location, conceptual, 'Where the three kernel readings structurally disagree: which founding moment and legitimating criterion governs.').

omega_variable(
    strategic_depth_military_obsolescence,
    'Does the original topographic strategic-depth rationale (narrow pre-1967 lines, artillery/armor exposure) remain militarily load-bearing given precision-guided missiles, satellite surveillance, and changed regional alliance structures, or has it become primarily a legitimating vocabulary for settlement and administrative permanence?',
    'Independent military-technical assessment of whether physical territorial depth still meaningfully changes warning time or defensive capability against current missile and drone threat profiles, compared against assessments from the security establishment itself.',
    'If the doctrine is substantially militarily obsolete, the founding_problem_status shifts from contested toward dead, strengthening the case that the arrangement now functions as inertial or extractive rather than coordinative — pushing the computed classification toward snare. If the rationale remains load-bearing, the tangled_rope reading (genuine coordination plus asymmetric cost) is better supported.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(strategic_depth_military_obsolescence, empirical, 'Whether the strategic-depth doctrine''s military rationale has been overtaken by technological and geopolitical change.').

omega_variable(
    settlement_as_security_or_civilian_expansion,
    'Is settlement construction and population growth in the West Bank structurally a security presence (forward positioning, buffer maintenance) or is it civilian territorial expansion using security language as legitimating cover?',
    'Comparison of settlement siting patterns against actual defensive/strategic value (proximity to security-relevant terrain) versus settlement growth patterns driven by housing policy, ideological settlement movements, and land-value considerations unrelated to defense.',
    'If settlement patterns track strategic terrain, the beneficiary declaration for settlement_movement_residents under this reading is well-supported. If settlement patterns primarily track ideological and demographic goals unrelated to defensible terrain, the security-necessity framing for settlements specifically (as distinct from military buffer zones) is a cover story, and the FSM-adjacent divergence between claim and metric widens further.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(settlement_as_security_or_civilian_expansion, empirical, 'Whether settlement expansion tracks genuine strategic terrain or ideological/demographic goals dressed in security language.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(territorial_legitimacy__security_necessity_reading, 1967, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(terr_tr_t1967, territorial_legitimacy__security_necessity_reading, theater_ratio, 1967, 0.15).
narrative_ontology:measurement(terr_tr_t1978, territorial_legitimacy__security_necessity_reading, theater_ratio, 1978, 0.22).
narrative_ontology:measurement(terr_tr_t1993, territorial_legitimacy__security_necessity_reading, theater_ratio, 1993, 0.28).
narrative_ontology:measurement(terr_tr_t2005, territorial_legitimacy__security_necessity_reading, theater_ratio, 2005, 0.33).
narrative_ontology:measurement(terr_tr_t2015, territorial_legitimacy__security_necessity_reading, theater_ratio, 2015, 0.38).
narrative_ontology:measurement(terr_tr_t2024, territorial_legitimacy__security_necessity_reading, theater_ratio, 2024, 0.42).

% Extraction over time
narrative_ontology:measurement(terr_be_t1967, territorial_legitimacy__security_necessity_reading, base_extractiveness, 1967, 0.38).
narrative_ontology:measurement(terr_be_t1978, territorial_legitimacy__security_necessity_reading, base_extractiveness, 1978, 0.45).
narrative_ontology:measurement(terr_be_t1993, territorial_legitimacy__security_necessity_reading, base_extractiveness, 1993, 0.52).
narrative_ontology:measurement(terr_be_t2005, territorial_legitimacy__security_necessity_reading, base_extractiveness, 2005, 0.58).
narrative_ontology:measurement(terr_be_t2015, territorial_legitimacy__security_necessity_reading, base_extractiveness, 2015, 0.64).
narrative_ontology:measurement(terr_be_t2024, territorial_legitimacy__security_necessity_reading, base_extractiveness, 2024, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(terr_su_t1967, territorial_legitimacy__security_necessity_reading, suppression_requirement, 1967, 0.45).
narrative_ontology:measurement(terr_su_t1978, territorial_legitimacy__security_necessity_reading, suppression_requirement, 1978, 0.55).
narrative_ontology:measurement(terr_su_t1993, territorial_legitimacy__security_necessity_reading, suppression_requirement, 1993, 0.6).
narrative_ontology:measurement(terr_su_t2005, territorial_legitimacy__security_necessity_reading, suppression_requirement, 2005, 0.68).
narrative_ontology:measurement(terr_su_t2015, territorial_legitimacy__security_necessity_reading, suppression_requirement, 2015, 0.72).
narrative_ontology:measurement(terr_su_t2024, territorial_legitimacy__security_necessity_reading, suppression_requirement, 2024, 0.74).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(territorial_legitimacy__security_necessity_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(territorial_legitimacy__security_necessity_reading, territorial_legitimacy__partition_reading).
narrative_ontology:affects_constraint(territorial_legitimacy__security_necessity_reading, territorial_legitimacy__indigenous_continuity_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the territorial_legitimacy kernel, each authored as a separate, epsilon-invariant constraint per the decomposition principle: security_necessity_reading (this file), partition_reading, and indigenous_continuity_reading. They share a contested kernel (the legitimating basis for territorial control after 1948/1967) but instantiate structurally distinct constraints with different founding moments, different beneficiary/victim sets, and different epsilon values. Do not average or reconcile ε across the three files; each is a complete, standalone reading.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
