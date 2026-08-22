% ============================================================================
% CONSTRAINT STORY: war_winnability_post_1945__countervailing_thinkable
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: war_winnability_post_1945__countervailing_thinkable
 *   human_readable: Countervailing Strategy: Nuclear War Remains Winnable Through Counterforce Targeting
 *   domain: strategic_studies/nuclear_deterrence/international_relations
 *
 * SUMMARY:
 *   This story instantiates the 'countervailing_thinkable' reading of the
 *   contested war-winnability kernel: the claim, held continuously within
 *   U.S. and allied strategic planning communities since the Schlesinger
 *   Doctrine (1974) through PD-59 (1980), NSDD-13 (1981), and successive
 *   Nuclear Posture Reviews, that nuclear weapons narrow but do not close the
 *   space of winnable war — that counterforce targeting, escalation control,
 *   and damage-limitation postures make limited nuclear victory an
 *   achievable, planned-for outcome rather than a logical impossibility. This
 *   is NOT the claim that great-power nuclear war is categorically unwinnable
 *   (the sibling 'deterrence_unthinkable' reading), nor the claim that
 *   winnability is operationally intact but has become publicly unsayable
 *   (the sibling 'rhetorical_contraction' reading). Those are separate
 *   constraints with separate ε values, linked here via
 *   network.affects_constraints. The extractiveness measured here (0.66 by
 *   2024) reflects a real coordination function — deterrence credibility
 *   against limited aggression — riding alongside a persistent institutional
 *   rent: sustained procurement and command missions whose survival depends
 *   on the winnability premise remaining live, at direct cost to arms control
 *   stability.
 *
 * KEY AGENTS:
 *   - military_industrial_complex: institutional beneficiary, mission and budget continuity depend on the winnable-war premise
 *   - counterforce_planning_establishment: agenda-setter, writes and defends the targeting doctrine
 *   - strategic_forces_commanders: institutional beneficiary and co-administrator, career and command relevance riding on warfighting mission
 *   - arms_control_regimes: primary institutional victim, negotiating logic undermined by winnable-war planning
 *   - crisis_stability_advocates: diffuse payer, absorbs elevated inadvertent-escalation risk without doctrinal authority
 *   - civilian_populations_in_targeted_states: ultimate powerless payer, bears tail risk of any 'limited' exchange
 *   - declaratory_policy_officials: excluded/observer, must reconcile public MAD rhetoric with classified counterforce planning without full visibility into either
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(war_winnability_post_1945__countervailing_thinkable, 0.66).
domain_priors:suppression_score(war_winnability_post_1945__countervailing_thinkable, 0.58).
domain_priors:theater_ratio(war_winnability_post_1945__countervailing_thinkable, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(war_winnability_post_1945__countervailing_thinkable, extractiveness, 0.66).
narrative_ontology:constraint_metric(war_winnability_post_1945__countervailing_thinkable, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(war_winnability_post_1945__countervailing_thinkable, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(war_winnability_post_1945__countervailing_thinkable, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(war_winnability_post_1945__countervailing_thinkable, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(war_winnability_post_1945__countervailing_thinkable, tangled_rope).
narrative_ontology:human_readable(war_winnability_post_1945__countervailing_thinkable, "Countervailing Strategy: Nuclear War Remains Winnable Through Counterforce Targeting").
narrative_ontology:topic_domain(war_winnability_post_1945__countervailing_thinkable, "strategic_studies/nuclear_deterrence/international_relations").

domain_priors:requires_active_enforcement(war_winnability_post_1945__countervailing_thinkable).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(war_winnability_post_1945__countervailing_thinkable, '0eaa9ac0-53dc-4823-9a65-3090a9ac81e3').
narrative_ontology:cs_kernel_codification('0eaa9ac0-53dc-4823-9a65-3090a9ac81e3', distributed).
narrative_ontology:cs_authority_grounding('0eaa9ac0-53dc-4823-9a65-3090a9ac81e3', extraction).
narrative_ontology:cs_interpretation_layer_present('0eaa9ac0-53dc-4823-9a65-3090a9ac81e3').
narrative_ontology:cs_reading_relation('0eaa9ac0-53dc-4823-9a65-3090a9ac81e3', war_winnability_post_1945__deterrence_unthinkable, coexists_with).
narrative_ontology:cs_reading_relation('0eaa9ac0-53dc-4823-9a65-3090a9ac81e3', war_winnability_post_1945__rhetorical_contraction, influences).
narrative_ontology:cs_axiom('0eaa9ac0-53dc-4823-9a65-3090a9ac81e3', foundational, escalation_control_is_achievable).
narrative_ontology:cs_axiom_status(escalation_control_is_achievable, holdable).
narrative_ontology:cs_axiom_grounding('0eaa9ac0-53dc-4823-9a65-3090a9ac81e3', escalation_control_is_achievable, empirically_contingent).
narrative_ontology:cs_axiom('0eaa9ac0-53dc-4823-9a65-3090a9ac81e3', foundational, limited_nuclear_exchange_is_a_coherent_policy_option).
narrative_ontology:cs_axiom_status(limited_nuclear_exchange_is_a_coherent_policy_option, holdable).
narrative_ontology:cs_axiom_grounding('0eaa9ac0-53dc-4823-9a65-3090a9ac81e3', limited_nuclear_exchange_is_a_coherent_policy_option, instrumental).
narrative_ontology:cs_reference_frame('0eaa9ac0-53dc-4823-9a65-3090a9ac81e3', schlesinger_doctrine_flexible_response).
narrative_ontology:cs_drift_state('0eaa9ac0-53dc-4823-9a65-3090a9ac81e3', post_cold_war_multipolar_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('0eaa9ac0-53dc-4823-9a65-3090a9ac81e3', '').
narrative_ontology:cs_kernel_id(war_winnability_post_1945__countervailing_thinkable, war_winnability_post_1945).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(war_winnability_post_1945__countervailing_thinkable, military_industrial_complex).
narrative_ontology:constraint_beneficiary(war_winnability_post_1945__countervailing_thinkable, counterforce_planning_establishment).
narrative_ontology:constraint_beneficiary(war_winnability_post_1945__countervailing_thinkable, strategic_forces_commanders).
narrative_ontology:constraint_victim(war_winnability_post_1945__countervailing_thinkable, arms_control_regimes).
narrative_ontology:constraint_victim(war_winnability_post_1945__countervailing_thinkable, crisis_stability_advocates).
narrative_ontology:constraint_victim(war_winnability_post_1945__countervailing_thinkable, civilian_populations_in_targeted_states).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Designs, produces, and continually upgrades counterforce-capable delivery systems (MIRVed ICBMs, hard-target-kill warheads, precision guidance) justified by the doctrine that limited nuclear war remains winnable if enough of the adversary's forces can be destroyed first. Contracts, budgets, and institutional missions are sustained by keeping the winnability question open rather than settled.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__countervailing_thinkable, military_industrial_complex, beneficiary,
    institutional, generational, arbitrage, global).

% Writes the targeting plans (SIOP/OPLAN successors), war-gaming scenarios, and doctrinal literature that treat nuclear exchange as a spectrum with discrete rungs where escalation can be controlled and victory-adjacent outcomes achieved through disarming first strikes on military targets. Administers the doctrine's persistence by continually refining escalation-control theory in response to critics.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__countervailing_thinkable, counterforce_planning_establishment, agenda_setter,
    institutional, generational, arbitrage, global).

% Command structures whose relevance, budget authority, and career trajectories depend on nuclear forces having an actual warfighting mission beyond pure deterrent posture. A pure 'assured destruction only, no winnable scenarios' doctrine would shrink their institutional footprint; countervailing doctrine sustains it.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__countervailing_thinkable, strategic_forces_commanders, beneficiary,
    institutional, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(war_winnability_post_1945__countervailing_thinkable, strategic_forces_commanders, agenda_setter).

% Treaty architectures (START-lineage, non-proliferation frameworks) depend on the premise that nuclear exchange has no winnable outcome, making arsenal reduction rational for all sides. Counterforce planning that treats limited victory as achievable directly undermines the logic that reductions are safe, since a smaller arsenal is more vulnerable to a disarming first strike — this pressures force levels upward and treaty compliance downward. Cannot exit the doctrinal environment they operate within; their negotiating leverage erodes as counterforce planning persists.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__countervailing_thinkable, arms_control_regimes, payer,
    organized, generational, constrained, global).

% Analysts and former officials who argue that counterforce/winnability doctrine incentivizes launch-on-warning postures and first-strike temptations during crises, raising the probability of inadvertent escalation. Their warnings are absorbed into doctrine review processes but rarely change the underlying targeting logic; they bear the diffuse cost of living in a higher-crisis-instability world without power to rewrite doctrine.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__countervailing_thinkable, crisis_stability_advocates, payer,
    moderate, biographical, constrained, global).

% Live in proximity to military and industrial targets that counterforce doctrine designates as legitimate strike sets in a 'limited' exchange; bear the full mortality and fallout risk of any war the doctrine claims is winnable and controllable. Have no voice in targeting policy and no exit from geographic exposure.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__countervailing_thinkable, civilian_populations_in_targeted_states, payer,
    powerless, civilizational, trapped, national).

% Civilian officials who must publicly speak in the register of pure deterrence and mutual vulnerability (arms control diplomacy, public reassurance) while classified targeting doctrine underneath continues to plan for graduated, winnable exchanges. They are structurally excluded from reconciling the two registers in public and often are not read into the operational detail their public statements implicitly contradict.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__countervailing_thinkable, declaratory_policy_officials, excluded,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(war_winnability_post_1945__countervailing_thinkable, declaratory_policy_officials, observer).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(war_winnability_post_1945__countervailing_thinkable, military_industrial_complex).
narrative_ontology:fixing_cost_class(war_winnability_post_1945__countervailing_thinkable, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides the strategic forces community with a coherent warfighting doctrine that allows nuclear weapons to remain instruments of policy — deterrence backed by a credible theory of controlled escalation and disarming counterforce options — rather than pure taboo weapons whose only use is mutual suicide, which the doctrine's proponents argue would itself be strategically incoherent and unable to deter limited aggression.
% TRANSFER_FUNCTION: Moves budgetary authority, institutional mission, and doctrinal legitimacy toward the counterforce planning and procurement establishment; moves crisis stability and arms-reduction leverage away from arms control regimes; moves catastrophic tail risk onto civilian populations near military and industrial targets who have no say in targeting policy.
% ABSENT_VOICES: Civilian populations in prospective target states have no representation in doctrine formation. Crisis stability researchers are consulted but rarely empowered to alter targeting plans. Declaratory policy officials are excluded from reconciling public arms-control rhetoric with classified operational planning, producing a split they did not create and cannot resolve.
% DISAPPEARANCE_RATIONALE: If counterforce/winnability doctrine were abandoned in favor of pure minimum/assured-destruction deterrence, force structure would shrink toward smaller, more survivable arsenals, arms control negotiations would gain a coherent stabilizing logic, targeting plans would collapse toward countervalue-only postures, and substantial procurement and command missions tied to warfighting capability would lose their rationale.
% FOUNDING_PROBLEM: Pure mutual assured destruction (MAD) was criticized in the 1970s-80s as strategically and morally unstable — a doctrine offering only civilian annihilation as a response to any aggression lacked credibility for deterring limited or regional provocations, and left no rungs on the escalation ladder short of total war. Countervailing strategy (Schlesinger Doctrine, PD-59, NSDD-13) was built to give nuclear forces flexible, graduated, war-fighting options addressed at this credibility gap.
% FOUNDING_PROBLEM_CORROBORATION: Doctrine architects and current strategic command officials attest the credibility gap remains live — adversary leaders might not believe a pure countervalue threat. Independent arms control scholars, several former defense officials (e.g., in post-retirement memoirs and Nuclear Posture Review critiques), and crisis-stability researchers outside the counterforce establishment attest the doctrine now functions primarily to sustain modernization budgets and institutional missions rather than to solve a live deterrence-credibility problem, and that its persistence measurably degrades arms control prospects.
narrative_ontology:disappearance_verdict(war_winnability_post_1945__countervailing_thinkable, world_rearranges).
narrative_ontology:founding_problem_status(war_winnability_post_1945__countervailing_thinkable, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(war_winnability_post_1945__countervailing_thinkable, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(war_winnability_post_1945__countervailing_thinkable, 'none', 1).
narrative_ontology:epsilon_provenance(war_winnability_post_1945__countervailing_thinkable, 0.66, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness is authored at 0.66 (rising from 0.5 in 1974) because while a genuine deterrence-credibility problem existed at the doctrine's founding, the persistence and expansion of counterforce planning increasingly outpaces any independent measure of adversary aggression risk, tracking instead with procurement cycles and institutional budget competition — the classic rent-seeking-layered-on-coordination signature. Suppression (0.58) reflects the doctrine's dependence on classification: targeting plans are not subject to public or even full congressional debate, and declaratory policy is deliberately decoupled from operational doctrine to prevent public and allied scrutiny of the winnability premise. Theater ratio (0.42) captures a meaningful proxy-substitution dynamic: 'escalation control' war-gaming and 'flexible response' briefings increasingly serve career and budget-justification functions distinct from any operational likelihood the scenarios will be executed as gamed. Accessibility collapse is moderate (0.4) — alternative doctrines (minimum deterrence, no-first-use) remain articulable and are actively argued by named critics, unlike a true mountain where alternatives become nearly unthinkable. Resistance is substantial (0.55): arms control advocates, several retired flag officers, and academic strategists have persistently challenged counterforce doctrine for five decades without dislodging it.
 *
 * PERSPECTIVAL GAP:
 *   From the counterforce planning establishment's seat, this is a rope or at worst a scaffold: a necessary, bounded doctrinal adaptation to the reality that pure MAD is not credible against limited aggression, sustained only as long as adversary capabilities require it. From the arms control regime's seat, the identical structure computes as tangled rope shading toward snare: a genuine deterrence problem (coordination) has become cover for open-ended force modernization and targeting flexibility that directly degrades the stability the arms control apparatus exists to build. The engine should compute these divergently from the shared structural data — the claim I author (tangled_rope) sits between the two seat-level extremes, reflecting that a real coordination function persists but a substantial extractive residue rides on it that the counterforce establishment's own framing does not surface.
 *
 * DIRECTIONALITY LOGIC:
 *   Military-industrial and command beneficiaries sit near the low-d end: they collect budget, mission, and institutional legitimacy from the doctrine's persistence and have arbitrage-grade exit into adjacent defense missions if the doctrine were ever formally abandoned. Arms control regimes and crisis stability advocates sit toward the high-d end: they bear the doctrine's costs (degraded negotiating leverage, elevated crisis instability) with constrained exit — they cannot simply stop being arms control institutions or reposition into a different portfolio. Civilian populations in targeted states sit at the furthest high-d, trapped end: they have zero agency over targeting doctrine and bear catastrophic tail risk purely by geography. Declaratory policy officials occupy an unusual excluded/observer hybrid — they are structurally positioned between the public and classified registers without full access to either, which is why they cannot be cleanly classified as agenda_setter despite nominal seniority.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (MAD's lack of credibility against limited aggression) was genuinely live in the 1970s-80s détente-to-Reagan-buildup period. Whether it remains live in 2024 against a very different threat environment (multipolar nuclear competition, cyber and hypersonic delivery, different adversary risk calculus) is contested rather than settled — this is precisely why founding_problem_status is authored as 'contested' rather than 'dead': doctrine architects have live arguments that the credibility gap persists, while critics argue the doctrine has decoupled from any updated threat assessment and now functions to protect existing modernization programs. Classifying this as tangled_rope rather than snare prevents mislabeling a doctrine with a real historical coordination rationale as pure extraction; classifying it as tangled_rope rather than rope prevents treating five decades of budget-tracking, classification-shielded doctrinal persistence as costless pure coordination. The mandatrophy risk here is specifically that a mandate genuinely responsive to 1970s bipolar strategic conditions has outlived clean justification without anyone being forced to relitigate it, because classification prevents the kind of public cost-benefit reassessment that would surface the question.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    credibility_gap_still_live,
    'Does the original 1970s deterrence-credibility problem (MAD''s incoherence against limited aggression) remain empirically live in the current multipolar, multi-domain threat environment, or has the doctrine decoupled from any updated threat assessment and now persist primarily to protect existing modernization programs and command missions?',
    'Independent declassification review comparing targeting plan evolution against documented adversary capability and doctrine changes over the interval; comparison with allied states that maintain minimum-deterrence postures without counterforce doctrine to assess whether the credibility gap is empirically necessary or doctrine-specific.',
    'If the credibility gap is empirically resolved as no longer live, this reading''s coordination function collapses and the constraint should reclassify toward snare; if genuinely live, tangled_rope with a real coordination core is the more defensible read.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(credibility_gap_still_live, empirical, 'Whether the doctrine''s founding rationale remains empirically active or has become a self-sustaining institutional artifact.').

omega_variable(
    kernel_reading_selection_evidence,
    'What observable evidence distinguishes the countervailing_thinkable reading (winnability operationally intact, openly planned and discussed in doctrine documents) from the rhetorical_contraction reading (winnability operationally intact but publicly unsayable) for this specific doctrinal period?',
    'Compare declassified internal doctrine documents (which speak openly of graduated nuclear options and limited victory) against contemporaneous public declaratory statements (which almost uniformly avoid ''winnable'' language) — the degree of divergence between the two registers determines whether this is better modeled as openly-thinkable-and-planned (this reading) or thinkable-but-unsayable (the sibling reading).',
    'If the internal/external divergence is large and systematic, the rhetorical_contraction reading may be the more accurate structural account for the same historical material, shifting which constraint file the doctrinal record properly belongs to.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_selection_evidence, conceptual, 'Framing under-determination between this reading and the rhetorical_contraction sibling, given that both describe the same underlying doctrinal record from different observational angles.').

omega_variable(
    counterforce_stability_effect_magnitude,
    'How large is the crisis-instability effect attributed to counterforce/winnability doctrine (via launch-on-warning incentives and first-strike temptation) relative to baseline crisis instability that would exist under any nuclear deterrent posture?',
    'Historical case analysis of documented near-miss crises (1983 Able Archer, 1995 Norwegian rocket incident) coded for the specific role counterforce/first-strike-vulnerability reasoning played in decision-maker behavior, compared against counterfactual minimum-deterrence baselines from game-theoretic and historical models.',
    'A large attributable effect would strengthen the case that arms_control_regimes and crisis_stability_advocates are victims of a specifically doctrinal choice rather than of nuclear weapons per se; a small effect would suggest the extraction attributed to this doctrine is overstated relative to inherent nuclear-age instability.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(counterforce_stability_effect_magnitude, empirical, 'Whether counterforce doctrine measurably adds crisis instability beyond baseline nuclear deterrence risk.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(war_winnability_post_1945__countervailing_thinkable, 1974, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(war__tr_t1974, war_winnability_post_1945__countervailing_thinkable, theater_ratio, 1974, 0.28).
narrative_ontology:measurement(war__tr_t1984, war_winnability_post_1945__countervailing_thinkable, theater_ratio, 1984, 0.3).
narrative_ontology:measurement(war__tr_t1994, war_winnability_post_1945__countervailing_thinkable, theater_ratio, 1994, 0.4).
narrative_ontology:measurement(war__tr_t2004, war_winnability_post_1945__countervailing_thinkable, theater_ratio, 2004, 0.38).
narrative_ontology:measurement(war__tr_t2014, war_winnability_post_1945__countervailing_thinkable, theater_ratio, 2014, 0.4).
narrative_ontology:measurement(war__tr_t2024, war_winnability_post_1945__countervailing_thinkable, theater_ratio, 2024, 0.42).

% Extraction over time
narrative_ontology:measurement(war__be_t1974, war_winnability_post_1945__countervailing_thinkable, base_extractiveness, 1974, 0.5).
narrative_ontology:measurement(war__be_t1984, war_winnability_post_1945__countervailing_thinkable, base_extractiveness, 1984, 0.58).
narrative_ontology:measurement(war__be_t1994, war_winnability_post_1945__countervailing_thinkable, base_extractiveness, 1994, 0.52).
narrative_ontology:measurement(war__be_t2004, war_winnability_post_1945__countervailing_thinkable, base_extractiveness, 2004, 0.56).
narrative_ontology:measurement(war__be_t2014, war_winnability_post_1945__countervailing_thinkable, base_extractiveness, 2014, 0.61).
narrative_ontology:measurement(war__be_t2024, war_winnability_post_1945__countervailing_thinkable, base_extractiveness, 2024, 0.66).

% Suppression requirement over time
narrative_ontology:measurement(war__su_t1974, war_winnability_post_1945__countervailing_thinkable, suppression_requirement, 1974, 0.45).
narrative_ontology:measurement(war__su_t1984, war_winnability_post_1945__countervailing_thinkable, suppression_requirement, 1984, 0.55).
narrative_ontology:measurement(war__su_t1994, war_winnability_post_1945__countervailing_thinkable, suppression_requirement, 1994, 0.42).
narrative_ontology:measurement(war__su_t2004, war_winnability_post_1945__countervailing_thinkable, suppression_requirement, 2004, 0.4).
narrative_ontology:measurement(war__su_t2014, war_winnability_post_1945__countervailing_thinkable, suppression_requirement, 2014, 0.5).
narrative_ontology:measurement(war__su_t2024, war_winnability_post_1945__countervailing_thinkable, suppression_requirement, 2024, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(war_winnability_post_1945__countervailing_thinkable, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(war_winnability_post_1945__countervailing_thinkable, 0.12).
narrative_ontology:affects_constraint(war_winnability_post_1945__countervailing_thinkable, deterrence_unthinkable).
narrative_ontology:affects_constraint(war_winnability_post_1945__countervailing_thinkable, rhetorical_contraction).

% DUAL FORMULATION NOTE:
% This file is one of three sibling readings of the war_winnability_post_1945 kernel, decomposed per the ε-invariance principle: 'deterrence_unthinkable' claims great-power nuclear war is categorically unwinnable (near-mountain, low extraction, victim set near-empty); 'rhetorical_contraction' claims winnability remains operationally planned but has become publicly unsayable (extraction concentrated in the discourse-suppression mechanism itself rather than in force planning); this file, 'countervailing_thinkable', claims winnability remains openly, operationally achievable through counterforce targeting, with extraction concentrated in institutional mission-continuity and arms-control degradation. All three describe the same historical doctrinal record from different structural angles and must not be averaged into a single ε.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
