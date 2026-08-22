% ============================================================================
% CONSTRAINT STORY: nuclear_impossibility_kernel__credibility_paradox_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_nuclear_impossibility_kernel__credibility_paradox_reading, []).

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
 *   constraint_id: nuclear_impossibility_kernel__credibility_paradox_reading
 *   human_readable: Standing Nuclear Deterrence Posture — Credibility Paradox Reading
 *   domain: strategic studies / international security
 *
 * SUMMARY:
 *   The standing arrangement under contest is the nuclear deterrence posture
 *   maintained by the armed great powers since the early 1950s: declaratory
 *   use-threats, survivable triads, extended-deterrence umbrellas, and the
 *   continuous machinery of credibility signaling. This story instantiates
 *   the credibility_paradox_reading of the nuclear_impossibility_kernel: the
 *   threat that sustains the arrangement cannot be executed without
 *   self-destruction, hence is inherently incredible; the arrangement
 *   persists by continuously manufacturing credibility — usable low-yield
 *   options, counterforce modernization, escalation ladders, rhetorical
 *   'unthinkability' — and each credibility-repair measure lowers the
 *   threshold at which actual war becomes reachable. The claim/metric split
 *   is deliberate: claimed_type is my structural assessment (tangled_rope — a
 *   real coordination achievement braided with heavy asymmetric extraction
 *   through the same structure); the metrics are authored descriptively of
 *   the posture's actual operation. KEY AGENTS (by structural relationship):
 *   strategic_weapons_establishment — agenda-setter
 *   (institutional/identity_locked), administers the posture and collects its
 *   budgets; nuclear_powers_executive_leaderships — beneficiary with
 *   agenda-setting power (powerful/constrained); hostage_city_populations —
 *   primary target (powerless/trapped); umbrella_host_states — dual
 *   beneficiary-payer (organized/constrained); budapest_assurance_states —
 *   payer whose assurance was tested and found wanting (moderate/trapped);
 *   disarmament_movements — excluded voice (organized/constrained);
 *   strategic_studies_community — analytical observer
 *   (analytical/civilizational).
 *
 * KEY AGENTS:
 *   - strategic_weapons_establishment: agenda_setter (institutional/identity_locked) — runs commands, labs, and industry; budgets and institutional purpose flow through continuation of the posture
 *   - nuclear_powers_executive_leaderships: beneficiary + agenda_setter (powerful/constrained) — collect coercive leverage and status; reproduce the posture each administration
 *   - hostage_city_populations: payer (powerless/trapped) — bear annihilation risk and funding without consent; no residence escapes the target sets
 *   - umbrella_host_states: beneficiary + payer (organized/constrained) — subsidized security shadowed by hostage status and alliance lock-in
 *   - budapest_assurance_states: payer (moderate/trapped) — traded nuclear options for assurances that failed on contact with a nuclear-armed aggressor
 *   - disarmament_movements: excluded (organized/constrained) — abolition campaigns kept outside the posture-managing rooms
 *   - strategic_studies_community: observer (analytical/civilizational) — produces the concepts and documents the theory-practice gap
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(nuclear_impossibility_kernel__credibility_paradox_reading, 0.7).
domain_priors:suppression_score(nuclear_impossibility_kernel__credibility_paradox_reading, 0.64).
domain_priors:theater_ratio(nuclear_impossibility_kernel__credibility_paradox_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(nuclear_impossibility_kernel__credibility_paradox_reading, extractiveness, 0.7).
narrative_ontology:constraint_metric(nuclear_impossibility_kernel__credibility_paradox_reading, suppression_requirement, 0.64).
narrative_ontology:constraint_metric(nuclear_impossibility_kernel__credibility_paradox_reading, theater_ratio, 0.55).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(nuclear_impossibility_kernel__credibility_paradox_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(nuclear_impossibility_kernel__credibility_paradox_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(nuclear_impossibility_kernel__credibility_paradox_reading, tangled_rope).
narrative_ontology:human_readable(nuclear_impossibility_kernel__credibility_paradox_reading, "Standing Nuclear Deterrence Posture — Credibility Paradox Reading").
narrative_ontology:topic_domain(nuclear_impossibility_kernel__credibility_paradox_reading, "strategic studies / international security").

domain_priors:requires_active_enforcement(nuclear_impossibility_kernel__credibility_paradox_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(nuclear_impossibility_kernel__credibility_paradox_reading, '7db48817-4ebb-4c88-a1ee-4894572946cc').
narrative_ontology:cs_kernel_codification('7db48817-4ebb-4c88-a1ee-4894572946cc', distributed).
narrative_ontology:cs_authority_grounding('7db48817-4ebb-4c88-a1ee-4894572946cc', expertise).
narrative_ontology:cs_interpretation_layer_present('7db48817-4ebb-4c88-a1ee-4894572946cc').
narrative_ontology:cs_reading_relation('7db48817-4ebb-4c88-a1ee-4894572946cc', nuclear_impossibility_kernel__structural_contraction_reading, influences).
narrative_ontology:cs_reading_relation('7db48817-4ebb-4c88-a1ee-4894572946cc', nuclear_impossibility_kernel__rational_dropout_reading, coexists_with).
narrative_ontology:cs_axiom('7db48817-4ebb-4c88-a1ee-4894572946cc', foundational, deterrent_threat_inherently_incredible).
narrative_ontology:cs_axiom_status(deterrent_threat_inherently_incredible, holdable).
narrative_ontology:cs_axiom_grounding('7db48817-4ebb-4c88-a1ee-4894572946cc', deterrent_threat_inherently_incredible, empirically_contingent).
narrative_ontology:cs_axiom('7db48817-4ebb-4c88-a1ee-4894572946cc', secondary, usable_options_restore_credibility_at_war_risk).
narrative_ontology:cs_axiom_status(usable_options_restore_credibility_at_war_risk, holdable).
narrative_ontology:cs_axiom_grounding('7db48817-4ebb-4c88-a1ee-4894572946cc', usable_options_restore_credibility_at_war_risk, instrumental).
narrative_ontology:cs_reference_frame('7db48817-4ebb-4c88-a1ee-4894572946cc', credible_threat_deterrence_baseline).
narrative_ontology:cs_drift_state('7db48817-4ebb-4c88-a1ee-4894572946cc', contemporary_usable_options_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('7db48817-4ebb-4c88-a1ee-4894572946cc', '').
narrative_ontology:cs_kernel_id(nuclear_impossibility_kernel__credibility_paradox_reading, nuclear_impossibility_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(nuclear_impossibility_kernel__credibility_paradox_reading, strategic_weapons_establishment).
narrative_ontology:constraint_beneficiary(nuclear_impossibility_kernel__credibility_paradox_reading, nuclear_powers_executive_leaderships).
narrative_ontology:constraint_beneficiary(nuclear_impossibility_kernel__credibility_paradox_reading, umbrella_host_states).
narrative_ontology:constraint_victim(nuclear_impossibility_kernel__credibility_paradox_reading, hostage_city_populations).
narrative_ontology:constraint_victim(nuclear_impossibility_kernel__credibility_paradox_reading, budapest_assurance_states).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(nuclear_impossibility_kernel__credibility_paradox_reading, umbrella_host_states).
narrative_ontology:constraint_vindicates(nuclear_impossibility_kernel__credibility_paradox_reading, credible_deterrence_doctrine).
narrative_ontology:constraint_vindicates(nuclear_impossibility_kernel__credibility_paradox_reading, escalation_ladder_theory).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Operates the arsenals: strategic commands set readiness and targeting, weapons laboratories design and refurbish warheads, and the surrounding industrial base builds delivery systems. Budgets, missions, promotions, and institutional purpose all flow through the posture's continuation. Personnel are recruited, trained, and socially formed inside the mission; the organization's identity and the nation's security identity have merged with the arsenal's existence, and no career path or institutional alternative exists for the skills and structures involved.
narrative_ontology:constraint_stakeholder(nuclear_impossibility_kernel__credibility_paradox_reading, strategic_weapons_establishment, agenda_setter,
    institutional, generational, identity_locked, global).

% Heads of government and defense ministers of nuclear-armed states set declaratory policy and hold sole release authority. The posture confers coercive diplomatic weight, domestic security credentials, and crisis leverage no other instrument provides. Individuals rotate out of office, but the state cannot relinquish the posture without accepting the risks of unilateral disarmament, so each administration inherits and reproduces it regardless of party.
narrative_ontology:constraint_stakeholder(nuclear_impossibility_kernel__credibility_paradox_reading, nuclear_powers_executive_leaderships, beneficiary,
    powerful, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(nuclear_impossibility_kernel__credibility_paradox_reading, nuclear_powers_executive_leaderships, agenda_setter).

% Urban populations of nuclear-armed and adversary states live inside one another's targeting plans. They receive nothing from the posture except as incidental byproduct, bear the annihilation risk, fund it through taxation, and were never asked to consent. Relocation does not exit the arrangement: every large city sits in someone's plan, and the risk follows membership in a society, not a residence.
narrative_ontology:constraint_stakeholder(nuclear_impossibility_kernel__credibility_paradox_reading, hostage_city_populations, payer,
    powerless, generational, trapped, global).

% Allies under extended deterrence (NATO members hosting nuclear sharing, Japan, South Korea) receive security against nuclear-armed neighbors at a fraction of the cost of independent arsenals. The same basing arrangements and alliance visibility place their territory on adversary target lists, and dependence forecloses independent options: leaving the umbrella means either acquiring weapons of their own at prohibitive cost and treaty rupture, or accepting conventional inferiority.
narrative_ontology:constraint_stakeholder(nuclear_impossibility_kernel__credibility_paradox_reading, umbrella_host_states, beneficiary,
    organized, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(nuclear_impossibility_kernel__credibility_paradox_reading, umbrella_host_states, payer).

% States that surrendered nuclear options or weapons programs in exchange for security assurances anchored in great-power deterrence promises — Ukraine above all. When the assuring powers' deterrent was tested against a nuclear-armed aggressor, the assurance did not convert into protection; the state absorbed invasion. Its subsequent security now depends on the very arrangement whose promise failed, and wartime reality forecloses exit in any direction.
narrative_ontology:constraint_stakeholder(nuclear_impossibility_kernel__credibility_paradox_reading, budapest_assurance_states, payer,
    moderate, biographical, trapped, regional).

% Transnational civil-society coalitions (ICAN and predecessor movements, hibakusha testimony networks) campaign for abolition and stigmatization. They negotiated the Treaty on the Prohibition of Nuclear Weapons without a single nuclear-armed state participating, and their proposals enter the posture-managing institutions only as objects of dismissal. Their influence operates from outside the rooms where posture decisions are made.
narrative_ontology:constraint_stakeholder(nuclear_impossibility_kernel__credibility_paradox_reading, disarmament_movements, excluded,
    organized, generational, constrained, global).

% Deterrence theorists, arms-control analysts, and security-studies scholars who produce the concepts practitioners borrow (credible minimum deterrence, escalation ladders, stability-instability paradox) and who document the gap between declaratory theory and operational practice. They analyze from outside the chain of command and hold no lever over the posture.
narrative_ontology:constraint_stakeholder(nuclear_impossibility_kernel__credibility_paradox_reading, strategic_studies_community, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(nuclear_impossibility_kernel__credibility_paradox_reading, strategic_weapons_establishment).
narrative_ontology:fixing_cost_class(nuclear_impossibility_kernel__credibility_paradox_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Prevents nuclear war between armed great powers by making attack self-destructive (mutual vulnerability), and supplies alliance security collectively through extended deterrence that would otherwise drive widespread proliferation. Coordinates great-power conduct through threat rather than agreement.
% TRANSFER_FUNCTION: Moves several percent of great-power GDP annually from taxpayers to the strategic weapons establishment (forces, warheads, command-and-control, industry); moves existential risk onto hostage city populations and umbrella territories without their consent; moves coercive diplomatic leverage and compressed crisis decision authority into executive hands; moved security assurances to non-nuclear states in exchange for foregone nuclear options.
% ABSENT_VOICES: Hostage populations — the people whose cities sit in targeting plans — were never consulted and hold no seat; disarmament advocates are structurally outside the room (the TPNW process concluded with zero nuclear-armed states participating); umbrella publics learn their hostage status only episodically. Their objection: the arrangement trades their lives and cities for elite stability without consent.
% DISAPPEARANCE_RATIONALE: Alliance architectures (NATO nuclear sharing, the Japan and Korea umbrellas), great-power crisis diplomacy, defense industrial bases, and the nonproliferation bargain all presuppose the posture. Overnight removal would trigger proliferation cascades among threatened states, alliance collapse or reinvention, and immediate conventional rebalancing in Europe and East Asia.
% FOUNDING_PROBLEM: After 1945 the weapons could not be uninvented: the founding problem was how to prevent nuclear war between rival blocs — first how to stop Soviet conventional superiority from triggering Western nuclear use, then how to stabilize mutual vulnerability against first-strike incentives, then how to extend protection to allies without universal proliferation.
% FOUNDING_PROBLEM_CORROBORATION: Attested from outside the benefiting parties by hibakusha organizations (whose testimony frames the problem as unresolved), the Bulletin of the Atomic Scientists' Science and Security Board (Doomsday Clock assessments), TPNW state parties and ICAN, and former senior officials turned critics (e.g., the Perry–Shultz–Kissinger–Nunn op-ed cohort) — none of whom collect from the posture. The weapons establishments also attest liveness, but the external seats carry the corroboration.
narrative_ontology:disappearance_verdict(nuclear_impossibility_kernel__credibility_paradox_reading, world_rearranges).
narrative_ontology:founding_problem_status(nuclear_impossibility_kernel__credibility_paradox_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(nuclear_impossibility_kernel__credibility_paradox_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(nuclear_impossibility_kernel__credibility_paradox_reading, 'none', 1).
narrative_ontology:epsilon_provenance(nuclear_impossibility_kernel__credibility_paradox_reading, 0.7, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(nuclear_impossibility_kernel__credibility_paradox_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(nuclear_impossibility_kernel__credibility_paradox_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(nuclear_impossibility_kernel__credibility_paradox_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.70: the referent is the standing deterrence posture itself, assessed by this reading's own lights — a decades-long resource transfer to the weapons establishment, unconsented existential risk imposed on hostage populations, and, per this reading, a maintenance cycle (usable options, lowered thresholds) that raises the very catastrophe probability the posture claims to suppress. Suppression 0.64 is authored as a RAW structural property (the engine scales only extractiveness): secrecy regimes, the Oppenheimer-era loyalty machinery, containment of the freeze movement, alliance lock-in, and the absence of any individual exit from target sets. Theater_ratio 0.55: per this reading roughly half of visible activity is credibility performance — exercises, parades, declaratory statements, rhetorical unthinkability — while hardware and C2 remain real function; the 1983 peak (Able Archer: performance nearly consumed by reality) and the 1991 trough mark the series. Accessibility_collapse 0.55: once the paradox is understood, alternatives narrow to within-frame choices (tolerate incredible-threat instability, or engineer usable options); disarmament exits persist in discourse but are foreclosed by verification distrust and breakout fear. Resistance 0.55: TPNW (122 state parties), hibakusha testimony, the 1980s freeze movement — real, cyclical, and so far structurally ineffective against the core posture. All three metric series run on ONE shared eight-point grid (1952–2025) so the engine samples aligned rows; the 1991 dip is enforcement relaxation, not resolution, and the post-2001 re-hardening tracks the usable-options ratchet resuming. The rising base_extractiveness series is authored honestly and will feed accumulation hypotheses (T17-class) — that is intended data, not a defect.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the agenda-setter seat compute differently from the same structure. From inside the weapons establishment the posture is mission, budget, and professional identity — the arrangement looks like the thing that stands between order and chaos. From the hostage-city seat it is unconsented existential risk administered by others. Umbrella-host seats experience subsidized protection shadowed by hostage status; Budapest-assurance seats experienced the promise failing precisely when tested. Coalition prospects for the powerless victims are poor: hostage populations are dispersed across adversarial borders with no shared forum, which is why their resistance registers only through the excluded advocacy seat. The engine computes these per-seat classifications from the structural data; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   The strategic_weapons_establishment sits at the full-beneficiary end (d near 0): budgets, missions, and authority flow to it, and its identity_locked exit amplifies its stake in continuation. Executive leaderships sit near the beneficiary end with a secondary agenda-setting pull. Umbrella_host_states occupy a genuinely mid-range position — the derivation should land them near symmetric, since the same basing that subsidizes their security places them on target lists. Hostage_city_populations sit at the full-target end (d near 1), amplified by trapped exit: they cannot arbitrage, relocate, or opt out. Budapest_assurance_states are full targets of a crueler kind — they already paid (foregone options) and collected a failed assurance. The posture's global spatial scope modestly amplifies effective extraction on the target seats via verification difficulty, per the engine's scope modifier.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification guards against two symmetrical mislabels. Reading the posture as pure rope ('deterrence kept the peace for eighty years') erases the extraction: the budgets, the unconsented risk, the lock-in, and this reading's core finding that the peace is maintained by machinery that makes war more reachable. Reading it as pure snare ('state terror with no coordination content') erases the genuine collective good the structure plausibly supplies — great-power war absence and proliferation suppression through extended deterrence. Tangled_rope holds both horns. On obsolescence: the founding problem (preventing nuclear war while the weapons exist) is live, so no mandatrophy-resolved flag is declared; but the theater_ratio trajectory (0.30 to 0.55) is the symptom to watch — if credibility rhetoric detaches fully from function, the posture drifts toward piton-shaped performance maintained by inertia and ritual, with the establishment as residual administrator and hostage populations as diffuse bearers.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_position,
    'This story instantiates the credibility_paradox_reading of the nuclear_impossibility_kernel; would instantiating structural_contraction_reading or rational_dropout_reading instead change the computed classification?',
    'Generate the two sibling stories against the same standing arrangement and compare computed types; divergence localizes the disagreement to whether mutual annihilation is physically guaranteed (contraction) or merely cost-dominated (dropout).',
    'The contraction reading would drive epsilon toward negligible (physics, no parties, no beneficiaries); the dropout reading would yield a cost-benefit insurance profile; this reading yields the highest epsilon of the three and the tangled_rope profile.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_position, conceptual, 'Committer structure: this constraint is one of three readings of the nuclear-impossibility kernel; classification is reading-indexed.').

omega_variable(
    coordination_success_vs_survivorship,
    'Does the absence of great-power nuclear war since 1945 evidence the posture''s genuine coordination function, or is it survivorship under an inherently incredible threat — luck, near misses (Able Archer 1983, the Petrov incident), and crisis brinkmanship that succeeded by margin?',
    'Systematic near-miss frequency analysis against base rates, archival crisis reconstruction, and calibrated expert elicitation comparing deterred and undeterred rivalry trajectories.',
    'If survivorship dominates, the coordination-function gate weakens and the arrangement computes nearer pure extraction; if a genuine deterrence effect dominates, the tangled_rope profile holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_success_vs_survivorship, empirical, 'Whether the posture''s peace record reflects coordination success or survival under an incredible threat.').

omega_variable(
    escalation_ladder_reachability,
    'How reachable is nuclear war via escalation ladders in contemporary crises (European land war, Taiwan contingencies), given the usable-options investments this reading identifies?',
    'Cross-institution war-game replication, crisis archival analysis, and calibrated elicitation on threshold-crossing probabilities at each ladder rung.',
    'High reachability confirms the reading''s instability claim and raises effective extraction on hostage populations; low reachability would shift weight toward the sibling readings'' profiles.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(escalation_ladder_reachability, empirical, 'Empirical reachability of nuclear use through escalation, the quantity this reading claims is rising.').

omega_variable(
    umbrella_net_position,
    'Are umbrella host states net beneficiaries (security subsidy exceeding hostage risk and dependence costs) or net payers?',
    'Counterfactual independent-defense cost analysis versus actuarial nuclear-risk exposure, plus revealed preference in alliance burden-sharing disputes and basing renegotiations.',
    'If net payers, the beneficiary declaration overstates the coordination constituency and the extraction asymmetry deepens; if net beneficiaries, the dual-role derivation stands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(umbrella_net_position, empirical, 'Net structural position of extended-deterrence allies between subsidy and hostagehood.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(nuclear_impossibility_kernel__credibility_paradox_reading, 1952, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nucl_tr_t1952, nuclear_impossibility_kernel__credibility_paradox_reading, theater_ratio, 1952, 0.3).
narrative_ontology:measurement(nucl_tr_t1962, nuclear_impossibility_kernel__credibility_paradox_reading, theater_ratio, 1962, 0.38).
narrative_ontology:measurement(nucl_tr_t1972, nuclear_impossibility_kernel__credibility_paradox_reading, theater_ratio, 1972, 0.42).
narrative_ontology:measurement(nucl_tr_t1983, nuclear_impossibility_kernel__credibility_paradox_reading, theater_ratio, 1983, 0.5).
narrative_ontology:measurement(nucl_tr_t1991, nuclear_impossibility_kernel__credibility_paradox_reading, theater_ratio, 1991, 0.35).
narrative_ontology:measurement(nucl_tr_t2001, nuclear_impossibility_kernel__credibility_paradox_reading, theater_ratio, 2001, 0.4).
narrative_ontology:measurement(nucl_tr_t2019, nuclear_impossibility_kernel__credibility_paradox_reading, theater_ratio, 2019, 0.52).
narrative_ontology:measurement(nucl_tr_t2025, nuclear_impossibility_kernel__credibility_paradox_reading, theater_ratio, 2025, 0.55).

% Extraction over time
narrative_ontology:measurement(nucl_be_t1952, nuclear_impossibility_kernel__credibility_paradox_reading, base_extractiveness, 1952, 0.5).
narrative_ontology:measurement(nucl_be_t1962, nuclear_impossibility_kernel__credibility_paradox_reading, base_extractiveness, 1962, 0.58).
narrative_ontology:measurement(nucl_be_t1972, nuclear_impossibility_kernel__credibility_paradox_reading, base_extractiveness, 1972, 0.6).
narrative_ontology:measurement(nucl_be_t1983, nuclear_impossibility_kernel__credibility_paradox_reading, base_extractiveness, 1983, 0.68).
narrative_ontology:measurement(nucl_be_t1991, nuclear_impossibility_kernel__credibility_paradox_reading, base_extractiveness, 1991, 0.55).
narrative_ontology:measurement(nucl_be_t2001, nuclear_impossibility_kernel__credibility_paradox_reading, base_extractiveness, 2001, 0.58).
narrative_ontology:measurement(nucl_be_t2019, nuclear_impossibility_kernel__credibility_paradox_reading, base_extractiveness, 2019, 0.66).
narrative_ontology:measurement(nucl_be_t2025, nuclear_impossibility_kernel__credibility_paradox_reading, base_extractiveness, 2025, 0.7).

% Suppression requirement over time
narrative_ontology:measurement(nucl_su_t1952, nuclear_impossibility_kernel__credibility_paradox_reading, suppression_requirement, 1952, 0.55).
narrative_ontology:measurement(nucl_su_t1962, nuclear_impossibility_kernel__credibility_paradox_reading, suppression_requirement, 1962, 0.62).
narrative_ontology:measurement(nucl_su_t1972, nuclear_impossibility_kernel__credibility_paradox_reading, suppression_requirement, 1972, 0.58).
narrative_ontology:measurement(nucl_su_t1983, nuclear_impossibility_kernel__credibility_paradox_reading, suppression_requirement, 1983, 0.68).
narrative_ontology:measurement(nucl_su_t1991, nuclear_impossibility_kernel__credibility_paradox_reading, suppression_requirement, 1991, 0.42).
narrative_ontology:measurement(nucl_su_t2001, nuclear_impossibility_kernel__credibility_paradox_reading, suppression_requirement, 2001, 0.5).
narrative_ontology:measurement(nucl_su_t2019, nuclear_impossibility_kernel__credibility_paradox_reading, suppression_requirement, 2019, 0.6).
narrative_ontology:measurement(nucl_su_t2025, nuclear_impossibility_kernel__credibility_paradox_reading, suppression_requirement, 2025, 0.64).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(nuclear_impossibility_kernel__credibility_paradox_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(nuclear_impossibility_kernel__credibility_paradox_reading, structural_contraction_reading).
narrative_ontology:affects_constraint(nuclear_impossibility_kernel__credibility_paradox_reading, rational_dropout_reading).
narrative_ontology:affects_constraint(nuclear_impossibility_kernel__credibility_paradox_reading, npt_nonproliferation_regime).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition of the nuclear_impossibility_kernel (epsilon-invariance): the colloquial claim 'nuclear weapons made great-power war impossible' covers three structurally distinct constraints. structural_contraction_reading asserts physical/logical impossibility (negligible epsilon, no parties); rational_dropout_reading asserts a rational-choice cost barrier (insurance-like profile); this credibility_paradox_reading asserts the sustaining threat is inherently incredible (highest epsilon, tangled_rope profile, full party structure). Each is a separate story with its own epsilon, beneficiaries, and stakeholders; this file links both siblings and the downstream nonproliferation regime, which the assurance-failure dynamic (Budapest precedent) feeds by raising proliferation incentives among exposed states.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
