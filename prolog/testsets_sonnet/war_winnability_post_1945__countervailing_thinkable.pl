% ============================================================================
% CONSTRAINT STORY: war_winnability_post_1945__countervailing_thinkable
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_war_winnability_post_1945__countervailing_thinkable, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: war_winnability_post_1945__countervailing_thinkable
 *   human_readable: Countervailing Strategy Doctrine: Limited Nuclear Victory Through Counterforce Targeting
 *   domain: strategic_studies/nuclear_deterrence/international_relations
 *
 * SUMMARY:
 *   This story instantiates the countervailing_thinkable reading of the
 *   war-winnability kernel: the claim, operative in US strategic doctrine
 *   from the Schlesinger Doctrine (1974) through PD-59 (1980) and persisting
 *   in modernized form in post-Cold War and current nuclear posture reviews,
 *   that nuclear weapons raise the cost and narrow the path to victory in
 *   great-power war without foreclosing it as a coherent planning category.
 *   Under this reading, counterforce targeting of an adversary's military
 *   assets (not cities) preserves escalation control and offers policymakers
 *   something between capitulation and civilizational suicide. The doctrine's
 *   persistence is read here as a tangled rope: it genuinely solves the
 *   flexible-response coordination problem for political and military
 *   leadership, but it does so by sustaining and legitimizing a
 *   targeting-and-procurement apparatus whose institutional survival depends
 *   on winnability remaining thinkable, at direct cost to arms-control
 *   efforts and crisis stability. The theater ratio rose steadily as the
 *   doctrine persisted past the Cold War's end without a corresponding live
 *   adversary scenario matching the original justification, suggesting
 *   increasing performative maintenance of victory-planning infrastructure.
 *   This is a distinct constraint from the deterrence_unthinkable reading
 *   (which holds the premise that no coherent victory concept survives) and
 *   from the rhetorical_contraction reading (which holds that winnability
 *   became unsayable while remaining operationally live) — each of those is
 *   authored as its own story with its own epsilon.
 *
 * KEY AGENTS:
 *   - military_industrial_complex: primary beneficiary (institutional/arbitrage) — captures procurement and mission continuity
 *   - counterforce_planning_communities: agenda-setter (institutional/identity_locked) — professional identity fused to winnability doctrine
 *   - arms_control_regimes: primary payer (organized/constrained) — treaty architecture undermined by winnable-war force postures
 *   - civilian_populations_in_targeted_regions: diffuse payer (powerless/trapped) — bear collateral risk with no voice
 *   - strategic_studies_observers: analytical observer — traces doctrine/procurement/rhetoric interaction across administrations
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(war_winnability_post_1945__countervailing_thinkable, 0.62).
domain_priors:suppression_score(war_winnability_post_1945__countervailing_thinkable, 0.58).
domain_priors:theater_ratio(war_winnability_post_1945__countervailing_thinkable, 0.44).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(war_winnability_post_1945__countervailing_thinkable, extractiveness, 0.62).
narrative_ontology:constraint_metric(war_winnability_post_1945__countervailing_thinkable, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(war_winnability_post_1945__countervailing_thinkable, theater_ratio, 0.44).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(war_winnability_post_1945__countervailing_thinkable, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(war_winnability_post_1945__countervailing_thinkable, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(war_winnability_post_1945__countervailing_thinkable, tangled_rope).
narrative_ontology:human_readable(war_winnability_post_1945__countervailing_thinkable, "Countervailing Strategy Doctrine: Limited Nuclear Victory Through Counterforce Targeting").
narrative_ontology:topic_domain(war_winnability_post_1945__countervailing_thinkable, "strategic_studies/nuclear_deterrence/international_relations").

domain_priors:requires_active_enforcement(war_winnability_post_1945__countervailing_thinkable).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(war_winnability_post_1945__countervailing_thinkable, '9b4cd7bb-b685-4b1d-bc7d-2f0275a80535').
narrative_ontology:cs_kernel_codification('9b4cd7bb-b685-4b1d-bc7d-2f0275a80535', distributed).
narrative_ontology:cs_authority_grounding('9b4cd7bb-b685-4b1d-bc7d-2f0275a80535', practice).
narrative_ontology:cs_interpretation_layer_present('9b4cd7bb-b685-4b1d-bc7d-2f0275a80535').
narrative_ontology:cs_reading_relation('9b4cd7bb-b685-4b1d-bc7d-2f0275a80535', war_winnability_post_1945__deterrence_unthinkable, coexists_with).
narrative_ontology:cs_reading_relation('9b4cd7bb-b685-4b1d-bc7d-2f0275a80535', war_winnability_post_1945__rhetorical_contraction, influences).
narrative_ontology:cs_axiom('9b4cd7bb-b685-4b1d-bc7d-2f0275a80535', foundational, limited_nuclear_exchange_is_a_coherent_planning_object).
narrative_ontology:cs_axiom_status(limited_nuclear_exchange_is_a_coherent_planning_object, holdable).
narrative_ontology:cs_axiom_grounding('9b4cd7bb-b685-4b1d-bc7d-2f0275a80535', limited_nuclear_exchange_is_a_coherent_planning_object, instrumental).
narrative_ontology:cs_axiom('9b4cd7bb-b685-4b1d-bc7d-2f0275a80535', secondary, escalation_control_through_counterforce_discrimination_is_achievable).
narrative_ontology:cs_axiom_status(escalation_control_through_counterforce_discrimination_is_achievable, holdable).
narrative_ontology:cs_axiom_grounding('9b4cd7bb-b685-4b1d-bc7d-2f0275a80535', escalation_control_through_counterforce_discrimination_is_achievable, empirically_contingent).
narrative_ontology:cs_reference_frame('9b4cd7bb-b685-4b1d-bc7d-2f0275a80535', flexible_response_schlesinger_doctrine).
narrative_ontology:cs_drift_state('9b4cd7bb-b685-4b1d-bc7d-2f0275a80535', post_cold_war_unipolar_and_multipolar_transition, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('9b4cd7bb-b685-4b1d-bc7d-2f0275a80535', '').
narrative_ontology:cs_kernel_id(war_winnability_post_1945__countervailing_thinkable, war_winnability_post_1945).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(war_winnability_post_1945__countervailing_thinkable, military_industrial_complex).
narrative_ontology:constraint_beneficiary(war_winnability_post_1945__countervailing_thinkable, counterforce_planning_communities).
narrative_ontology:constraint_beneficiary(war_winnability_post_1945__countervailing_thinkable, strategic_targeting_analysts).
narrative_ontology:constraint_victim(war_winnability_post_1945__countervailing_thinkable, arms_control_regimes).
narrative_ontology:constraint_victim(war_winnability_post_1945__countervailing_thinkable, crisis_stability_norms).
narrative_ontology:constraint_victim(war_winnability_post_1945__countervailing_thinkable, civilian_populations_in_targeted_regions).
narrative_ontology:constraint_vindicates(war_winnability_post_1945__countervailing_thinkable, flexible_response_doctrine).
narrative_ontology:constraint_vindicates(war_winnability_post_1945__countervailing_thinkable, escalation_dominance_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Designs, funds, and lobbies for counterforce-capable systems (accurate MIRVed warheads, silo-hardening, command-and-control redundancy) justified by the doctrine that limited nuclear war is winnable if fought correctly. Procurement budgets, weapons-lab funding, and officer career tracks all depend on the continued plausibility of victory-oriented planning. Can shift contracts and rhetoric to whichever administration is receptive; largely insulated from the consequences of the doctrine being wrong.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__countervailing_thinkable, military_industrial_complex, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(war_winnability_post_1945__countervailing_thinkable, military_industrial_complex, agenda_setter).

% Strategic planners, targeteers, and war-college theorists whose professional identity and career advancement are built on producing SIOP/OPLAN-style victory scenarios. Their expertise only has value if winnability is a coherent planning object; treating nuclear war as categorically unwinnable would dissolve their discipline. They draft the targeting doctrine and brief it upward as operationally sound.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__countervailing_thinkable, counterforce_planning_communities, agenda_setter,
    institutional, generational, identity_locked, national).

% Technical staff who build damage-limitation models and counterforce target sets. Employment, publication venues, and think-tank standing depend on the doctrine remaining a live research and policy area rather than being foreclosed as incoherent.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__countervailing_thinkable, strategic_targeting_analysts, beneficiary,
    moderate, biographical, constrained, national).

% Treaty architectures (SALT, START, New START) and the diplomatic infrastructure around them are directly undermined every time a state's declared or leaked doctrine treats limited nuclear victory as achievable, because counterforce planning drives warhead accuracy and quantity requirements that arms-control ceilings are designed to cap. Negotiators must constantly argue against force postures justified by winnability doctrine; they cannot exit the negotiation table without abandoning the entire project of numerical limits.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__countervailing_thinkable, arms_control_regimes, payer,
    organized, generational, constrained, global).

% The informal but load-bearing set of expectations (no first strike advantage, no use-it-or-lose-it pressure) that keep crises from escalating. Counterforce doctrine, by making disarming first strikes appear strategically rational, erodes exactly the incentive structure these norms depend on. Not an actor itself but a structural condition damaged by the doctrine's persistence.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__countervailing_thinkable, crisis_stability_norms, payer,
    analytical, civilizational, analytical, global).
narrative_ontology:stakeholder_non_agent(war_winnability_post_1945__countervailing_thinkable, crisis_stability_norms).

% Populations near hardened military installations, command bunkers, and industrial targets that counterforce doctrine designates as legitimate strike objects. They bear the collateral risk of any operationalization of the doctrine and have no voice in targeting decisions, no ability to relocate meaningfully, and no seat at the strategic planning table.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__countervailing_thinkable, civilian_populations_in_targeted_regions, payer,
    powerless, civilizational, trapped, global).

% Scholars and former officials who hold that nuclear war is categorically unwinnable (the sibling reading) are professionally present in the discourse but structurally excluded from operational planning rooms, where countervailing-strategy assumptions are treated as the working premise regardless of the academic debate's outcome.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__countervailing_thinkable, deterrence_theorists, excluded,
    organized, civilizational, mobile, global).

% Historians and net-assessment analysts who trace how doctrine, procurement, and rhetoric interact across administrations, without a direct stake in either the planning apparatus or the arms-control apparatus.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__countervailing_thinkable, strategic_studies_observers, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(war_winnability_post_1945__countervailing_thinkable, military_industrial_complex).
narrative_ontology:fixing_cost_class(war_winnability_post_1945__countervailing_thinkable, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a coherent operational answer to the question 'what does the military do if deterrence fails' — without a counterforce/limited-victory doctrine, escalation to countervalue exchange (city-killing) would be the only scripted option, which political leaders have historically found unacceptable to commit to in advance. The doctrine coordinates weapons design, targeting, and command authority around graduated response options.
% TRANSFER_FUNCTION: Moves budgetary authority, institutional prestige, and doctrinal legitimacy toward the counterforce-planning and weapons-development apparatus, and moves risk (of crisis instability, of arms racing, of targeting-driven civilian exposure) onto arms-control regimes and populations near military targets who have no say in the doctrine's adoption.
% ABSENT_VOICES: Civilian populations near targeted military infrastructure have no representation in doctrinal debates. Arms-control negotiators are present in diplomatic fora but structurally unable to contest the doctrine at its source — the classified targeting and procurement process that generates the force postures they must then negotiate limits around.
% DISAPPEARANCE_RATIONALE: If the countervailing/limited-victory reading of winnability vanished from strategic planning overnight and deterrence-unthinkable became the sole operative premise, counterforce procurement programs would lose their justification, targeting doctrine would collapse toward pure assured-destruction/deterrence-only postures, arms-control negotiations would have a far narrower disagreement space, and large portions of the strategic-planning bureaucracy would need to be reorganized or dissolved.
% FOUNDING_PROBLEM: Pure mutual assured destruction left political and military leaders with no credible intermediate response to limited Soviet aggression or a partial nuclear exchange — the only scripted options were capitulation or full countervalue retaliation, both considered strategically and morally intolerable, especially after Soviet counterforce capability grew in the 1970s.
% FOUNDING_PROBLEM_CORROBORATION: Proponents within the planning community (Schlesinger Doctrine architects, later countervailing-strategy authors under PD-59) attest the flexible-response gap remains live given persisting nuclear-armed adversaries with hardened, mobile forces. Independent arms-control scholars and several former defense officials outside the procurement chain (e.g., in Nuclear Posture Review commentary and academic net assessments) attest that the credible-intermediate-response problem has been substantially addressed by conventional precision-strike and cyber options, and that continued nuclear counterforce planning now serves institutional continuity more than an unmet strategic gap — this is a genuinely contested corroboration, not a settled one.
narrative_ontology:disappearance_verdict(war_winnability_post_1945__countervailing_thinkable, world_rearranges).
narrative_ontology:founding_problem_status(war_winnability_post_1945__countervailing_thinkable, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(war_winnability_post_1945__countervailing_thinkable, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(war_winnability_post_1945__countervailing_thinkable, 'none', 1).
narrative_ontology:epsilon_provenance(war_winnability_post_1945__countervailing_thinkable, 0.62, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(war_winnability_post_1945__countervailing_thinkable_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(war_winnability_post_1945__countervailing_thinkable, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(war_winnability_post_1945__countervailing_thinkable_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62) reflects that counterforce doctrine's institutional benefits (procurement, prestige, career continuity) are real and concentrated while its costs (arms-race pressure, crisis instability, targeting risk to civilians) are diffuse and largely externalized onto parties who did not choose the doctrine. Suppression (0.58) is moderate-high: the doctrine is maintained through classification, bureaucratic entrenchment, and the political difficulty of publicly conceding that victory planning is illusory, though it is not enforced by direct coercion against dissenting voices the way a snare would be. Theater ratio (0.44) climbed from 0.25 to 0.44 over the interval as the doctrine's original strategic rationale (a symmetric peer competitor with matching counterforce capability) weakened after 1991 while the planning apparatus persisted. Accessibility collapse (0.5) is moderate: alternative doctrinal frameworks (deterrence-only, minimum deterrence) remain articulable and are held by real institutional actors, so the countervailing reading has not achieved the near-total alternative-foreclosure of a mountain. Resistance (0.6) is substantial: arms-control advocates, minimum-deterrence theorists, and periodic congressional skepticism have persistently contested the doctrine without displacing it.
 *
 * PERSPECTIVAL GAP:
 *   From the counterforce-planning seat, the doctrine is a necessary and coherent response to a real strategic gap (what happens between capitulation and Armageddon). From the arms-control seat, the same doctrine is the mechanism by which that gap is perpetually re-manufactured to justify continued warhead accuracy and quantity, undermining the very treaty architecture meant to reduce nuclear risk. The engine should compute these seats to different types given their divergent power, exit options, and structural position relative to the same targeting apparatus.
 *
 * DIRECTIONALITY LOGIC:
 *   Military-industrial and planning-community stakeholders sit near the beneficiary end: institutional power, generational time horizon, and either arbitrage-grade or identity-locked exit (the latter because professional identity is fused to the doctrine's continued plausibility). Arms-control regimes and crisis-stability norms sit near the target end: organized but constrained, unable to exit the negotiating architecture without abandoning their entire mission. Civilian populations near targeted military infrastructure sit at the extreme target end: powerless, trapped, civilizational time horizon, zero input into the doctrine that defines their risk exposure.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (a credible intermediate response between capitulation and full nuclear exchange) was live and genuinely unaddressed at the doctrine's 1970s founding. Whether it remains live in 2024 is contested: conventional precision-strike and cyber capabilities now offer intermediate response options that did not exist in 1974, which some outside observers read as having substantially displaced the original nuclear-counterforce rationale — yet the planning apparatus and procurement commitments persist at comparable or greater scale. This is precisely the mandatrophy signature: classifying the doctrine as a clean rope would miss that its beneficiaries have structural reasons to declare the founding problem eternally live regardless of its actual status; classifying it as pure snare would miss that a genuine coordination problem (what do you do if deterrence fails) was real at founding and has not been fully resolved even by conventional alternatives. Tangled rope captures both facts simultaneously.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    genuine_gap_vs_manufactured_gap,
    'Is the ''credible intermediate response'' gap that countervailing doctrine claims to fill a genuine and persisting strategic problem, or has it been substantially closed by conventional precision-strike, cyber, and missile-defense capabilities such that continued nuclear counterforce planning now serves institutional continuity rather than an unmet need?',
    'Comparative net assessment of declared adversary force postures against current US/allied conventional and nuclear intermediate-response options; declassification review of internal targeting rationale documents across administrations to see whether justifications track adversary capability changes or track budget-cycle and institutional-survival pressures.',
    'If the gap is substantially closed, the tangled_rope classification should drift toward snare as the coordination function atrophies while extraction (procurement, career, prestige capture) persists — a piton-adjacent trajectory. If the gap remains genuinely open, tangled_rope with an intact coordination function is the more defensible classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(genuine_gap_vs_manufactured_gap, empirical, 'Whether the founding strategic gap that justifies countervailing doctrine still exists or has been closed by non-nuclear alternatives.').

omega_variable(
    committer_reading_selection,
    'Is countervailing_thinkable the operative reading of the war-winnability kernel in actual state practice, or is rhetorical_contraction more accurate — i.e., do planners genuinely believe limited nuclear victory is achievable, or do they maintain the operational planning apparatus while publicly disavowing ''winnability'' as a sayable concept?',
    'Comparison of declassified internal planning documents (which would reveal genuine operational belief) against public statements and doctrine documents (which would reveal rhetorical position) across the same time period; divergence between the two would support the rhetorical_contraction sibling reading over this one.',
    'If internal planning language consistently disavows victory framing even in classified settings, this story''s premise (that winnability is genuinely held as thinkable, not merely operationally retained under rhetorical cover) would be undermined, and the rhetorical_contraction sibling would better describe the actual constraint — this story would then need to be understood as describing a narrower community (targeting theorists) rather than the doctrine as a whole.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(committer_reading_selection, conceptual, 'Whether the countervailing reading accurately describes genuine planner belief versus a rhetorical/operational split better captured by the sibling reading.').

omega_variable(
    counterforce_civilian_risk_measurement,
    'How much genuine civilian collateral risk does counterforce targeting doctrine impose in practice, given that many counterforce targets (missile silos, command bunkers) are deliberately sited away from population centers specifically to support the doctrine''s claim of discrimination?',
    'Independent targeting and fallout-modeling analysis of declared and inferred target sets against population density data, compared across successive iterations of the doctrine from Schlesinger through current posture reviews.',
    'If siting genuinely minimizes civilian exposure, the victim classification for civilian populations should be narrowed or weighted down; if siting choices are substantially symbolic while yield and numbers of weapons still produce large civilian casualty estimates in war-gaming, the victim designation is strongly supported.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(counterforce_civilian_risk_measurement, empirical, 'Whether counterforce targeting''s discrimination claim genuinely reduces civilian risk or is largely rhetorical given weapon yields and numbers involved.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(war_winnability_post_1945__countervailing_thinkable, 1974, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(war__tr_t1974, war_winnability_post_1945__countervailing_thinkable, theater_ratio, 1974, 0.25).
narrative_ontology:measurement_basis(war__tr_t1974, observed).
narrative_ontology:measurement(war__tr_t1984, war_winnability_post_1945__countervailing_thinkable, theater_ratio, 1984, 0.3).
narrative_ontology:measurement_basis(war__tr_t1984, observed).
narrative_ontology:measurement(war__tr_t1994, war_winnability_post_1945__countervailing_thinkable, theater_ratio, 1994, 0.4).
narrative_ontology:measurement_basis(war__tr_t1994, observed).
narrative_ontology:measurement(war__tr_t2004, war_winnability_post_1945__countervailing_thinkable, theater_ratio, 2004, 0.42).
narrative_ontology:measurement_basis(war__tr_t2004, observed).
narrative_ontology:measurement(war__tr_t2014, war_winnability_post_1945__countervailing_thinkable, theater_ratio, 2014, 0.43).
narrative_ontology:measurement_basis(war__tr_t2014, observed).
narrative_ontology:measurement(war__tr_t2024, war_winnability_post_1945__countervailing_thinkable, theater_ratio, 2024, 0.44).
narrative_ontology:measurement_basis(war__tr_t2024, observed).

% Extraction over time
narrative_ontology:measurement(war__be_t1974, war_winnability_post_1945__countervailing_thinkable, base_extractiveness, 1974, 0.48).
narrative_ontology:measurement_basis(war__be_t1974, observed).
narrative_ontology:measurement(war__be_t1984, war_winnability_post_1945__countervailing_thinkable, base_extractiveness, 1984, 0.58).
narrative_ontology:measurement_basis(war__be_t1984, observed).
narrative_ontology:measurement(war__be_t1994, war_winnability_post_1945__countervailing_thinkable, base_extractiveness, 1994, 0.52).
narrative_ontology:measurement_basis(war__be_t1994, observed).
narrative_ontology:measurement(war__be_t2004, war_winnability_post_1945__countervailing_thinkable, base_extractiveness, 2004, 0.55).
narrative_ontology:measurement_basis(war__be_t2004, observed).
narrative_ontology:measurement(war__be_t2014, war_winnability_post_1945__countervailing_thinkable, base_extractiveness, 2014, 0.6).
narrative_ontology:measurement_basis(war__be_t2014, observed).
narrative_ontology:measurement(war__be_t2024, war_winnability_post_1945__countervailing_thinkable, base_extractiveness, 2024, 0.62).
narrative_ontology:measurement_basis(war__be_t2024, observed).

% Suppression requirement over time
narrative_ontology:measurement(war__su_t1974, war_winnability_post_1945__countervailing_thinkable, suppression_requirement, 1974, 0.45).
narrative_ontology:measurement_basis(war__su_t1974, observed).
narrative_ontology:measurement(war__su_t1984, war_winnability_post_1945__countervailing_thinkable, suppression_requirement, 1984, 0.55).
narrative_ontology:measurement_basis(war__su_t1984, observed).
narrative_ontology:measurement(war__su_t1994, war_winnability_post_1945__countervailing_thinkable, suppression_requirement, 1994, 0.4).
narrative_ontology:measurement_basis(war__su_t1994, observed).
narrative_ontology:measurement(war__su_t2004, war_winnability_post_1945__countervailing_thinkable, suppression_requirement, 2004, 0.42).
narrative_ontology:measurement_basis(war__su_t2004, observed).
narrative_ontology:measurement(war__su_t2014, war_winnability_post_1945__countervailing_thinkable, suppression_requirement, 2014, 0.5).
narrative_ontology:measurement_basis(war__su_t2014, observed).
narrative_ontology:measurement(war__su_t2024, war_winnability_post_1945__countervailing_thinkable, suppression_requirement, 2024, 0.58).
narrative_ontology:measurement_basis(war__su_t2024, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(war_winnability_post_1945__countervailing_thinkable, enforcement_mechanism).
narrative_ontology:affects_constraint(war_winnability_post_1945__countervailing_thinkable, war_winnability_post_1945__deterrence_unthinkable).
narrative_ontology:affects_constraint(war_winnability_post_1945__countervailing_thinkable, war_winnability_post_1945__rhetorical_contraction).

% DUAL FORMULATION NOTE:
% This story is one of three sibling readings of the war_winnability_post_1945 kernel, decomposed per the ε-invariance principle: the colloquial phrase 'is nuclear war winnable' conflates a categorical-impossibility claim (deterrence_unthinkable), a discourse-versus-practice claim (rhetorical_contraction), and this story's operational-persistence claim (countervailing_thinkable). Each has a distinct epsilon, distinct beneficiary/victim structure, and distinct classification (this story: tangled_rope; the sibling readings are authored separately). All three are linked bidirectionally via affects_constraints because doctrinal shifts in one reading (e.g., a president publicly disavowing winnability rhetoric) directly affect the political viability and resourcing of the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(war_winnability_post_1945__countervailing_thinkable, organized, 0.7).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
