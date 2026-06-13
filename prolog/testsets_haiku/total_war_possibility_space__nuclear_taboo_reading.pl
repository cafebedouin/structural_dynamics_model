% ============================================================================
% CONSTRAINT STORY: total_war_possibility_space__nuclear_taboo_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_total_war_possibility_space__nuclear_taboo_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: total_war_possibility_space__nuclear_taboo_reading
 *   human_readable: Nuclear Taboo Against Total War (Normative Reading)
 *   domain: international_relations/strategic_studies
 *
 * SUMMARY:
 *   The nuclear taboo against total war is a normative constraint: states
 *   with the capability to annihilate each other have constructed and
 *   maintain a collective commitment that nuclear weapons must never be used,
 *   independent of their material strategic value. This reading treats the
 *   taboo as the primary mechanism preventing total war, distinct from (and
 *   potentially in tension with) deterrence theory, which explains restraint
 *   through mutual vulnerability. The taboo is enforced through treaty
 *   regimes (NPT, no-first-use pledges), norm entrepreneurship (disarmament
 *   movements, treaty verification), and the internalization of the
 *   unthinkable into military planning doctrine. The constraint benefits
 *   civilian populations and non-nuclear states while extracting opportunity
 *   costs from proliferation-motivated states and constraining military
 *   planners' cognitive freedom. The measurement series shows the
 *   constraint's machinery matured from 1945–1980 (rising suppression as
 *   enforcement infrastructure built, theater ratio rising as performative
 *   norm maintenance became more visible) and plateaued from 1980–2026
 *   (suppression and extraction stable, theater ratio rising further as the
 *   substantive function atrophied and maintenance became increasingly
 *   theatrical). The extracted component is low in absolute terms (0.31
 *   endpoint) because the constraint's primary function is coordination
 *   (keeping total war normatively foreclosed), not rent collection; however,
 *   suppression is higher (0.68) because holding the taboo intact requires
 *   actively excluding alternative readings and constraining who can speak
 *   about nuclear use strategically.
 *
 * KEY AGENTS:
 *   - Nuclear-armed states: Set and maintain the taboo; face reputational destruction if they break it or allow it to weaken.
 *   - Civilian populations: Benefit from the taboo but are structurally powerless over its persistence.
 *   - Non-nuclear states: Pay opportunity costs (barred deterrence options) to sustain the norm they depend on.
 *   - Norm entrepreneurs: Actively reconstruct and reinforce the narrative; derive influence and legitimacy from stewardship.
 *   - Military planners: Bear the cognitive cost of operating under the constraint; identity-locked into taboo compliance.
 *   - Proliferation-motivated states: Structurally excluded from legitimate challenge to the taboo; pay costs for deterrent options barred.
 *   - Revisionist powers: Cannot openly contest the taboo without being read as civilizationally unhinged.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(total_war_possibility_space__nuclear_taboo_reading, 0.31).
domain_priors:suppression_score(total_war_possibility_space__nuclear_taboo_reading, 0.68).
domain_priors:theater_ratio(total_war_possibility_space__nuclear_taboo_reading, 0.52).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(total_war_possibility_space__nuclear_taboo_reading, extractiveness, 0.31).
narrative_ontology:constraint_metric(total_war_possibility_space__nuclear_taboo_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(total_war_possibility_space__nuclear_taboo_reading, theater_ratio, 0.52).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(total_war_possibility_space__nuclear_taboo_reading, accessibility_collapse, 0.89).
narrative_ontology:constraint_metric(total_war_possibility_space__nuclear_taboo_reading, resistance, 0.41).

% --- Constraint claim ---
narrative_ontology:constraint_claim(total_war_possibility_space__nuclear_taboo_reading, rope).
narrative_ontology:human_readable(total_war_possibility_space__nuclear_taboo_reading, "Nuclear Taboo Against Total War (Normative Reading)").
narrative_ontology:topic_domain(total_war_possibility_space__nuclear_taboo_reading, "international_relations/strategic_studies").

domain_priors:requires_active_enforcement(total_war_possibility_space__nuclear_taboo_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(total_war_possibility_space__nuclear_taboo_reading, 'c11495c9-c288-483f-9051-e803c53f63b1').
narrative_ontology:cs_kernel_codification('c11495c9-c288-483f-9051-e803c53f63b1', fixed_text).
narrative_ontology:cs_authority_grounding('c11495c9-c288-483f-9051-e803c53f63b1', lineage).
narrative_ontology:cs_interpretation_layer_present('c11495c9-c288-483f-9051-e803c53f63b1').
narrative_ontology:cs_reading_relation('c11495c9-c288-483f-9051-e803c53f63b1', total_war_possibility_space__deterrence_equilibrium_reading, coexists_with).
narrative_ontology:cs_reading_relation('c11495c9-c288-483f-9051-e803c53f63b1', total_war_possibility_space__space_contraction_reading, coexists_with).
narrative_ontology:cs_axiom('c11495c9-c288-483f-9051-e803c53f63b1', foundational, nuclear_war_categorically_normatively_prohibited).
narrative_ontology:cs_axiom_status(nuclear_war_categorically_normatively_prohibited, holdable).
narrative_ontology:cs_axiom_grounding('c11495c9-c288-483f-9051-e803c53f63b1', nuclear_war_categorically_normatively_prohibited, deontological).
narrative_ontology:cs_axiom('c11495c9-c288-483f-9051-e803c53f63b1', secondary, taboo_autonomy_from_material_vulnerability).
narrative_ontology:cs_axiom_status(taboo_autonomy_from_material_vulnerability, holdable).
narrative_ontology:cs_axiom_grounding('c11495c9-c288-483f-9051-e803c53f63b1', taboo_autonomy_from_material_vulnerability, deontological).
narrative_ontology:cs_reference_frame('c11495c9-c288-483f-9051-e803c53f63b1', nuclear_weapons_as_civilization_ending_unthinkable).
narrative_ontology:cs_drift_state('c11495c9-c288-483f-9051-e803c53f63b1', contemporary_2026, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('c11495c9-c288-483f-9051-e803c53f63b1', '').
narrative_ontology:cs_kernel_id(total_war_possibility_space__nuclear_taboo_reading, total_war_possibility_space).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(total_war_possibility_space__nuclear_taboo_reading, civilian_populations).
narrative_ontology:constraint_beneficiary(total_war_possibility_space__nuclear_taboo_reading, non_nuclear_states).
narrative_ontology:constraint_beneficiary(total_war_possibility_space__nuclear_taboo_reading, international_norm_entrepreneurs).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(total_war_possibility_space__nuclear_taboo_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(total_war_possibility_space__nuclear_taboo_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(total_war_possibility_space__nuclear_taboo_reading_tests).
:- end_tests(total_war_possibility_space__nuclear_taboo_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored as moderate (0.31) because the constraint's primary output is coordination (solving the mutual-annihilation problem), not wealth transfer. The extraction that is present operates through the non-proliferation regime: non-nuclear states forgo deterrent options in exchange for extended guarantees they cannot enforce. Suppression is higher (0.68) because the taboo's persistence depends on actively excluding alternative strategic readings from legitimate discourse — nuclear war planning is quarantined from strategy, proliferation-seeking states are barred from renegotiating the framework, and military planners cannot openly explore scenarios the taboo forbids. Theater ratio (0.52) has risen over the interval: the taboo's early phase (1945–1962) was substantive coordination work (constructing the never-again narrative after Hiroshima and Nagasaki). Since 1980, a growing share of enforcement activity has been performative — ceremonial reaffirmations of no-first-use, symbolic treaty signings, rhetorical recommitment by leaders — because the substantive coordination problem (preventing mutual annihilation) was solved decades ago. The rising theater ratio indicates the constraint is beginning to show signs of a piton (atrophying function, persistent performance, institutional inertia). The accessibility_collapse is high (0.89) because once you understand the taboo, alternatives genuinely collapse — a state cannot openly threaten total war without triggering international isolation, domestic political crisis, and self-defeat. Resistance is moderate (0.41) because there IS real resistance from proliferation-motivated states and from military planners who find the constraint operationally constraining, but that resistance is structurally suppressed and excluded from legitimate forums.
 *
 * PERSPECTIVAL GAP:
 *   This constraint computes differently from different seats. From the nuclear-armed-state agenda-setter seat, the taboo is genuine coordination — a problem they solved and now maintain through their own commitment. Their effective extraction is near zero; they are net beneficiaries of the coordination. From the non-nuclear-state seat, the constraint is a tangled arrangement: they benefit from the taboo's protection but pay opportunity costs they cannot negotiate. Their d-value is higher (more target-like) because they are barred from a strategic option. From the proliferation-motivated-state seat, the constraint is extractive (they bear costs while others capture deterrent capacity); they are excluded from the conversation. From the civilian-population seat, the constraint is pure rope — they depend on it utterly and have zero leverage. The engine computes this per-seat divergence from the power atoms, exit options, and beneficiary/victim declarations; the authored claim (rope) represents the nuclear-armed-states' frame, while the metrics represent the constraint's actual operation including its extraction and suppression components.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries: civilian_populations (depend on taboo for survival, zero exit); non_nuclear_states (protected by taboo though at opportunity cost); international_norm_entrepreneurs (gain status and influence from stewardship). Victims: proliferation_pressure_states (barred deterrent options, pay non-proliferation constraints without guaranteed security return); military_planners (identity-locked into cognitive restraint). The nuclear_armed_states are listed as agenda_setter, not beneficiary, because they run the constraint, not primarily because they collect from it — their economic benefit is nil, their reputational/strategic benefit is mixed (taboo prevents war but also restrains their own options). Directionality derivation: non_nuclear_states with 'organized' power but 'constrained' exit and declared victim status → d near 0.6–0.7 (target-like despite organized power, because exit is constrained by dependence on extended deterrence); proliferation_pressure_states with 'powerful' power but 'constrained' exit and clear victim status → d near 0.65–0.75 (material power does not overcome structural entrapment by non-proliferation); military_planners with 'powerful' power but 'identity_locked' exit → d near 0.55–0.65 (powerful in domain but unable to leave the frame). The taboo's suppression is not scaled by these directionalities — suppression is a structural property (the exclusion of alternative readings from legitimate discourse), authored at 0.68 and constant across seats.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding_problem (mutual annihilation via nuclear weapons) remains live by nuclear-armed states' own framing and by disarmament advocates' accounts. However, the constraint's mechanism for solving that problem may have atrophied: the early taboo (1945–1970) solved the problem through emotional reconstruction of nuclear weapons as civilization-ending horror, binding leaders to never-again commitments. By 2000–2026, the taboo persists but the mechanism is increasingly performative — leaders reaffirm no-first-use pledges as ritual, treaties are signed and largely unimplemented, and the substantive work of preventing total war has shifted toward technical measures (verification, transparency) that do not require the taboo's emotional force. The theater_ratio's rise from 0.25 (1945) to 0.52 (2026) captures this drift. The constraint shows early signs of mandatrophy: the founding problem remains (mutual vulnerability is still civilization-ending), but the constraint's operation increasingly consists of reassuring the public and allied states that the problem is being managed, rather than actually managing it. Mandatrophy is not yet resolved — the taboo still prevents open discussion of nuclear war as usable strategy — but the measurement trajectory flags it as a monitoring priority. If the theater ratio continues rising while suppression plateaus, the constraint will cross into piton territory.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    taboo_autonomy_vs_deterrence_riding,
    'Does the nuclear taboo rest on an independent normative commitment to the unthinkability of total war, or does it rest entirely on the material fact of mutual vulnerability and would collapse if a credible first-strike capability emerged?',
    'Examine taboo stability under changing material conditions: (a) if taboo weakens when mutual vulnerability is broken (e.g., successful ballistic missile defense), the taboo is materially dependent; (b) if taboo persists despite first-strike possibility, it is normatively autonomous.',
    'If autonomous, the constraint is primarily rope-like coordination enforced through norm maintenance. If dependent on deterrence, the constraint is tangled—the rope function (normative coordination) extracts from those barred from nuclear deterrence while benefiting from material vulnerability that norm entrepreneurs neither control nor acknowledge.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(taboo_autonomy_vs_deterrence_riding, conceptual, 'Whether the taboo is causally independent of or dependent on deterrence equilibrium.').

omega_variable(
    norm_entrepreneur_extraction,
    'Do norm entrepreneurs (disarmament activists, treaty verifiers, NPT administrators) gain status, funding, and institutional power from the taboo''s persistence, creating incentive structures that may suppress alternative readings of total war possibility?',
    'Trace career advancement and institutional resource flows for disarmament advocates vs. deterrence strategists; examine citation patterns and funding competition; interview norm entrepreneurs about exit conditions.',
    'If extraction is present, the taboo contains a snare component: norm entrepreneurs benefit from its maintenance while those seeking deterrent options bear the costs and are structurally excluded from renegotiating the rules.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(norm_entrepreneur_extraction, empirical, 'Whether norm entrepreneurship creates concentrated benefits from taboo maintenance.').

omega_variable(
    proliferation_state_coercion_mechanism,
    'Is the non-proliferation regime''s suppression of proliferation-motivated states structural (economic sanctions, technical barriers, inspection regimes) or internalized (the states themselves accept the taboo''s legitimacy and forgo deterrence as morally binding)?',
    'Post-sanctions suppression trajectory: if proliferation motivation persists or escalates after sanctions are lifted (or if states openly contest the taboo when enforcement pressure eases), suppression is primarily structural; if suppression persists after pressure removal, it is partially internalized.',
    'If internalized, the taboo has captured military and political elites in non-nuclear states, making the constraint more extractive than structural mechanisms alone suggest. If structural, the constraint depends on continuous enforcement machinery; weakening that machinery would reveal the suppression underneath.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(proliferation_state_coercion_mechanism, empirical, 'Whether proliferation suppression is structural or internalized in state decision-making.').

omega_variable(
    kernel_reading_contest_ambiguity,
    'This constraint instantiates the NORMATIVE-PROHIBITION reading of the total-war kernel. Sibling readings interpret the same kernel differently: deterrence_equilibrium (total war is reachable but deterred by vulnerability) and space_contraction (nuclear weapons made it unthinkable strategically, not merely normatively prohibited). Which reading captures the causal mechanism actually holding the constraint?',
    'Examine what breaks the taboo if one element fails: (a) if removing the normative narrative (declaring nuclear use thinkable but deterred) breaks the constraint, this reading is correct; (b) if only removing material parity breaks it, the deterrence reading is correct; (c) if strategic unthinkability (removing nuclear weapons from military planning doctrines) is the lock, the space_contraction reading is correct.',
    'Each reading implies different vulnerability points and different policy recommendations. Identifying the true reading determines where enforcement effort should be focused: norm maintenance (this reading), force structure balance (deterrence reading), or doctrinal exclusion (space_contraction reading).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_contest_ambiguity, conceptual, 'Which causal reading of the total-war kernel this constraint correctly instantiates.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(total_war_possibility_space__nuclear_taboo_reading, 1945, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tota_tr_t1945, total_war_possibility_space__nuclear_taboo_reading, theater_ratio, 1945, 0.25).
narrative_ontology:measurement(tota_tr_t1962, total_war_possibility_space__nuclear_taboo_reading, theater_ratio, 1962, 0.35).
narrative_ontology:measurement(tota_tr_t1980, total_war_possibility_space__nuclear_taboo_reading, theater_ratio, 1980, 0.42).
narrative_ontology:measurement(tota_tr_t2000, total_war_possibility_space__nuclear_taboo_reading, theater_ratio, 2000, 0.5).
narrative_ontology:measurement(tota_tr_t2015, total_war_possibility_space__nuclear_taboo_reading, theater_ratio, 2015, 0.52).
narrative_ontology:measurement(tota_tr_t2026, total_war_possibility_space__nuclear_taboo_reading, theater_ratio, 2026, 0.52).

% Extraction over time
narrative_ontology:measurement(tota_be_t1945, total_war_possibility_space__nuclear_taboo_reading, base_extractiveness, 1945, 0.15).
narrative_ontology:measurement(tota_be_t1962, total_war_possibility_space__nuclear_taboo_reading, base_extractiveness, 1962, 0.24).
narrative_ontology:measurement(tota_be_t1980, total_war_possibility_space__nuclear_taboo_reading, base_extractiveness, 1980, 0.28).
narrative_ontology:measurement(tota_be_t2000, total_war_possibility_space__nuclear_taboo_reading, base_extractiveness, 2000, 0.29).
narrative_ontology:measurement(tota_be_t2015, total_war_possibility_space__nuclear_taboo_reading, base_extractiveness, 2015, 0.31).
narrative_ontology:measurement(tota_be_t2026, total_war_possibility_space__nuclear_taboo_reading, base_extractiveness, 2026, 0.31).

% Suppression requirement over time
narrative_ontology:measurement(tota_su_t1945, total_war_possibility_space__nuclear_taboo_reading, suppression_requirement, 1945, 0.42).
narrative_ontology:measurement(tota_su_t1962, total_war_possibility_space__nuclear_taboo_reading, suppression_requirement, 1962, 0.58).
narrative_ontology:measurement(tota_su_t1980, total_war_possibility_space__nuclear_taboo_reading, suppression_requirement, 1980, 0.64).
narrative_ontology:measurement(tota_su_t2000, total_war_possibility_space__nuclear_taboo_reading, suppression_requirement, 2000, 0.67).
narrative_ontology:measurement(tota_su_t2015, total_war_possibility_space__nuclear_taboo_reading, suppression_requirement, 2015, 0.68).
narrative_ontology:measurement(tota_su_t2026, total_war_possibility_space__nuclear_taboo_reading, suppression_requirement, 2026, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(total_war_possibility_space__nuclear_taboo_reading, global_infrastructure).
narrative_ontology:boltzmann_floor_override(total_war_possibility_space__nuclear_taboo_reading, 0.18).
narrative_ontology:affects_constraint(total_war_possibility_space__nuclear_taboo_reading, total_war_possibility_space__deterrence_equilibrium_reading).
narrative_ontology:affects_constraint(total_war_possibility_space__nuclear_taboo_reading, total_war_possibility_space__space_contraction_reading).
narrative_ontology:affects_constraint(total_war_possibility_space__nuclear_taboo_reading, non_proliferation_regime_enforcement).
narrative_ontology:affects_constraint(total_war_possibility_space__nuclear_taboo_reading, extended_deterrence_alliance_stability).

% DUAL FORMULATION NOTE:
% The total_war_possibility_space kernel has three structurally distinct readings. This story (nuclear_taboo_reading) treats the causal lock as normative commitment; deterrence_equilibrium_reading treats it as material vulnerability; space_contraction_reading treats it as strategic unthinkability. Each reading has a different ε (extraction profile), different beneficiary/victim structure, and different suppression mechanism. The readings coexist as live positions held by different analytical schools and policy communities; none logically forecloses the others, but each creates structural pressure on alternatives. This constraint affects non_proliferation_regime_enforcement (non-proliferation legitimacy depends on taboo authority) and extended_deterrence_alliance_stability (extended deterrence works only if the taboo binds both guarantor and client states).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(total_war_possibility_space__nuclear_taboo_reading, powerful, 0.68).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
