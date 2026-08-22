% ============================================================================
% CONSTRAINT STORY: caa_section_111d_delegation__facility_constraint_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_caa_section_111d_delegation__facility_constraint_reading, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: caa_section_111d_delegation__facility_constraint_reading
 *   human_readable: CAA Section 111(d) 'Best System' Limited to Facility-Level Measures
 *   domain: administrative law/environmental regulation/constitutional interpretation
 *
 * SUMMARY:
 *   Section 111(d) of the Clean Air Act grants EPA authority to set emission
 *   standards based on the 'best system of emission reduction.' This
 *   constraint story instantiates the facility-constraint reading: EPA
 *   authority is limited to emission measures implementable at individual
 *   power plants (heat-rate improvements, carbon capture, fuel switching
 *   within existing generation) and does NOT extend to generation-shifting,
 *   plant retirement, or renewable substitution mandates. This reading was
 *   endorsed by the Supreme Court in West Virginia v. EPA (2022) on
 *   major-questions and constitutional-avoidance grounds. It creates a
 *   regulatory ceiling that protects fossil-fuel operators and coal-dependent
 *   states while foreclosing the fastest federal decarbonization pathway,
 *   creating extraction asymmetry: fossil operators and state policymakers
 *   benefit from regulatory predictability and preserved operational
 *   lifetime; climate advocates and air-quality communities bear the cost of
 *   slower climate progress. The constraint is CLAIMED as tangled_rope
 *   (coordination of regulatory authority + asymmetric extraction) and the
 *   authored metrics reflect that: high extractiveness (0.68) because the
 *   ceiling prevents the most effective policy option; high suppression
 *   (0.72) because the constraint's persistence depends on active enforcement
 *   against climate advocates seeking EPA expansion; moderate theater (0.41)
 *   because the facility-level improvement requirement is real but
 *   increasingly performative relative to what climate science indicates is
 *   necessary.
 *
 * KEY AGENTS:
 *   - fossil_fuel_operators: benefit from regulatory ceiling, protected operational lifetime (organized/constrained exit)
 *   - state_energy_policymakers: benefit from preserved state autonomy over generation mix (institutional/mobile exit)
 *   - climate_advocates: bear cost of regulatory ceiling foreclosing generation-shifting (organized/constrained exit)
 *   - air_quality_dependent_communities: bear cost of continued fossil generation and slower decarbonization (powerless/trapped exit)
 *   - EPA: administers the delegated authority boundary (institutional/analytical)
 *   - courts: enforce the textual and constitutional interpretation (institutional/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(caa_section_111d_delegation__facility_constraint_reading, 0.68).
domain_priors:suppression_score(caa_section_111d_delegation__facility_constraint_reading, 0.72).
domain_priors:theater_ratio(caa_section_111d_delegation__facility_constraint_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(caa_section_111d_delegation__facility_constraint_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(caa_section_111d_delegation__facility_constraint_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(caa_section_111d_delegation__facility_constraint_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(caa_section_111d_delegation__facility_constraint_reading, accessibility_collapse, 0.64).
narrative_ontology:constraint_metric(caa_section_111d_delegation__facility_constraint_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(caa_section_111d_delegation__facility_constraint_reading, tangled_rope).
narrative_ontology:human_readable(caa_section_111d_delegation__facility_constraint_reading, "CAA Section 111(d) 'Best System' Limited to Facility-Level Measures").
narrative_ontology:topic_domain(caa_section_111d_delegation__facility_constraint_reading, "administrative law/environmental regulation/constitutional interpretation").

domain_priors:requires_active_enforcement(caa_section_111d_delegation__facility_constraint_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(caa_section_111d_delegation__facility_constraint_reading, '0616aa46-3f84-4dfa-80a3-e97770d6d26c').
narrative_ontology:cs_kernel_codification('0616aa46-3f84-4dfa-80a3-e97770d6d26c', fixed_text).
narrative_ontology:cs_authority_grounding('0616aa46-3f84-4dfa-80a3-e97770d6d26c', extraction).
narrative_ontology:cs_interpretation_layer_present('0616aa46-3f84-4dfa-80a3-e97770d6d26c').
narrative_ontology:cs_reading_relation('0616aa46-3f84-4dfa-80a3-e97770d6d26c', caa_section_111d_delegation__systemic_transformation_reading, coexists_with).
narrative_ontology:cs_axiom('0616aa46-3f84-4dfa-80a3-e97770d6d26c', foundational, epa_authority_limited_to_facility_measures).
narrative_ontology:cs_axiom_status(epa_authority_limited_to_facility_measures, holdable).
narrative_ontology:cs_axiom_grounding('0616aa46-3f84-4dfa-80a3-e97770d6d26c', epa_authority_limited_to_facility_measures, deontological).
narrative_ontology:cs_axiom('0616aa46-3f84-4dfa-80a3-e97770d6d26c', secondary, major_questions_doctrine_applies_to_generation_shifting).
narrative_ontology:cs_axiom_status(major_questions_doctrine_applies_to_generation_shifting, holdable).
narrative_ontology:cs_axiom_grounding('0616aa46-3f84-4dfa-80a3-e97770d6d26c', major_questions_doctrine_applies_to_generation_shifting, deontological).
narrative_ontology:cs_reference_frame('0616aa46-3f84-4dfa-80a3-e97770d6d26c', facility_level_delegation_boundary).
narrative_ontology:cs_drift_state('0616aa46-3f84-4dfa-80a3-e97770d6d26c', post_west_virginia_v_epa, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('0616aa46-3f84-4dfa-80a3-e97770d6d26c', '').
narrative_ontology:cs_kernel_id(caa_section_111d_delegation__facility_constraint_reading, caa_section_111d_delegation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(caa_section_111d_delegation__facility_constraint_reading, fossil_fuel_operators).
narrative_ontology:constraint_beneficiary(caa_section_111d_delegation__facility_constraint_reading, state_energy_policymakers).
narrative_ontology:constraint_victim(caa_section_111d_delegation__facility_constraint_reading, climate_advocates).
narrative_ontology:constraint_victim(caa_section_111d_delegation__facility_constraint_reading, air_quality_dependent_communities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Coal, natural gas, and petroleum-fired power plants benefit from a regulatory ceiling that permits only facility-level efficiency improvements and carbon capture—not generation substitution or plant retirement. This protects their operational lifetime, avoids stranded-asset risk, and preserves market share against renewable competition. They defend the reading as a textualist limit on EPA's delegated authority.
narrative_ontology:constraint_stakeholder(caa_section_111d_delegation__facility_constraint_reading, fossil_fuel_operators, beneficiary,
    organized, generational, constrained, national).

% States retain authority over the energy mix under this reading; EPA cannot impose generation-shifting mandates. Coal-producing and coal-dependent states (WV, KY, WY, MT) benefit most from the preservation of their fuel-based economies. States that have committed to renewables still have autonomy to do so without federal mandate. The reading preserves state-level policy discretion.
narrative_ontology:constraint_stakeholder(caa_section_111d_delegation__facility_constraint_reading, state_energy_policymakers, beneficiary,
    institutional, generational, mobile, national).

% Face a regulatory ceiling that blocks the most effective decarbonization pathway: generation-shifting away from fossil fuels. Heat-rate improvements and carbon capture are costly and limited in impact; retirement of existing coal capacity and substitution with renewables is foreclosed at the federal regulatory level. They bear the cost of slower climate progress and cannot exit the constraint except through constitutional amendment or legislative override.
narrative_ontology:constraint_stakeholder(caa_section_111d_delegation__facility_constraint_reading, climate_advocates, payer,
    organized, generational, constrained, global).

% Communities downwind of coal plants and in regions with high fossil-fuel generation dependency experience localized air pollution, health burden, and limited health-based relief because the constraint allows marginal efficiency gains rather than elimination of polluting sources. Their exit options are geographic relocation (economically infeasible for low-income residents) or lobbying for state-level action (blocked by state coalitions benefiting from the reading).
narrative_ontology:constraint_stakeholder(caa_section_111d_delegation__facility_constraint_reading, air_quality_dependent_communities, payer,
    powerless, biographical, trapped, regional).

% Under this reading, EPA administers a constrained delegation: it can require facility-level best-practice improvements but cannot mandate grid-level generation-shifting. The agency enforces the ceiling through rulemaking, litigation, and state-delegation oversight. The boundary of EPA authority is set by the textual reading of 'best system of emission reduction' as facility-implementable only.
narrative_ontology:constraint_stakeholder(caa_section_111d_delegation__facility_constraint_reading, epa_agency, agenda_setter,
    institutional, generational, analytical, national).

% Adjudicate the scope of Section 111(d)'s delegation to EPA. The facility-constraint reading rests on a specific textual and constitutional interpretation. Courts enforce or narrow the boundary through statutory construction and chevron deference analysis. Their role is to determine whether EPA has exceeded its delegated authority by mandating generation-shifting.
narrative_ontology:constraint_stakeholder(caa_section_111d_delegation__facility_constraint_reading, courts, observer,
    institutional, generational, analytical, national).

% Is excluded from the beneficiary-compensation mechanism: the constraint does not mandate renewable substitution at the federal level, leaving deployment dependent on state policy and market forces. Renewables can still compete but without the accelerating demand that a federal generation-shifting requirement would create. They would advocate for a systemic-transformation reading but are not party to the administrative constraint.
narrative_ontology:constraint_stakeholder(caa_section_111d_delegation__facility_constraint_reading, renewable_energy_sector, excluded,
    organized, generational, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(caa_section_111d_delegation__facility_constraint_reading, fossil_fuel_operators).
narrative_ontology:fixing_cost_class(caa_section_111d_delegation__facility_constraint_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the boundary of EPA's delegated authority under the Clean Air Act: establishes a bright-line rule that 'best system' means facility-level emission controls (heat-rate improvements, carbon capture) rather than generation-shifting or plant retirement. This provides regulatory certainty for power operators, states, and EPA itself—each knows what the rule permits and forbids.
% TRANSFER_FUNCTION: Transfers decarbonization effort from the fastest path (generation-shifting) to slower, facility-contained paths (efficiency, capture). The cost of slower climate progress accrues to climate advocates and air-quality-dependent communities; the benefit of regulatory predictability and preserved operational lifetime accrues to fossil-fuel operators and coal-dependent states.
% ABSENT_VOICES: Renewable-energy firms, climate scientists with urgent decarbonization timelines, and future generations (climate impact beneficiaries of faster transition) are excluded from direct participation in EPA rulemaking under this constraint. They can lobby Congress or support litigation but cannot compel EPA rulemaking within the facility-constraint frame.
% DISAPPEARANCE_RATIONALE: If this constraint disappeared—i.e., if EPA authority expanded to include generation-shifting mandates—the power sector would face accelerated coal retirement, capital reallocation toward renewables, and state-level energy-mix decisions would be constrained by federal decarbonization targets. Coal-dependent regions would experience economic disruption; renewable deployment would accelerate; climate advocates would gain regulatory leverage.
% FOUNDING_PROBLEM: Section 111(d) of the Clean Air Act (1970) delegated authority to EPA to set emission standards based on the 'best system of emission reduction,' but the statute did not specify whether EPA could mandate changes to the power generation mix itself or only improvements within existing generation technologies and facilities.
% FOUNDING_PROBLEM_CORROBORATION: The Obama EPA (2015 Clean Power Plan) argued the problem required generation-shifting interpretation; the Trump EPA and coal-industry stakeholders argued for facility-constraint interpretation. The Supreme Court's 2022 decision in West Virginia v. EPA sided with the facility-constraint reading, citing major questions doctrine and textualism. Coal operators and coal-state coalitions attest the founding problem is answered by the Court; climate advocates attest the Court's answer is a misreading that forecloses the necessary policy response. Independent energy economists have documented that facility-only measures are insufficient to meet climate targets under most scenarios.
narrative_ontology:disappearance_verdict(caa_section_111d_delegation__facility_constraint_reading, world_rearranges).
narrative_ontology:founding_problem_status(caa_section_111d_delegation__facility_constraint_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(caa_section_111d_delegation__facility_constraint_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(caa_section_111d_delegation__facility_constraint_reading, 'none', 1).
narrative_ontology:epsilon_provenance(caa_section_111d_delegation__facility_constraint_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(caa_section_111d_delegation__facility_constraint_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(caa_section_111d_delegation__facility_constraint_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(caa_section_111d_delegation__facility_constraint_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises from 0.55 to 0.68 over the interval as the constraint's cost becomes clearer: facility-level improvements deliver marginal emissions reductions while climate impacts accelerate and renewable costs decline, making the regulatory ceiling increasingly extractive. Suppression rises from 0.58 to 0.72 as fossil operators invest in litigation defense and lobbying to maintain the boundary against EPA expansion attempts or legislative override. Theater is moderate-rising from 0.32 to 0.41 because the facility-improvement requirement is real but increasingly theatrical as a decarbonization strategy relative to climate targets—EPA rulemaking on heat-rate standards receives media attention, but the constraint's function is primarily to prevent generation-shifting, not to drive emissions reductions. Accessibility collapse is moderate (0.64) because alternatives exist (state-level renewable mandates, market-driven transition, legislative override) but are constrained by the ceiling's binding effect on federal authority. Resistance is high (0.71) because climate advocates mount sustained litigation, legislative advocacy, and public mobilization against the constraint, and the courts themselves have shown ideological division (3–6 split in West Virginia v. EPA).
 *
 * PERSPECTIVAL GAP:
 *   Payer seat (climate advocates) and agenda-setter seat (EPA, constrained by the reading) experience structurally different types. Climate advocates perceive tangled_rope or snare (coordination of regulatory authority is a cover story; the real function is extraction via regulatory ceiling). EPA and fossil operators perceive rope or even mountain (natural-law boundary set by constitutional structure and statutory text). Fossil operators perceive mountain (statutory text as discovered, not constructed). The engine computes these per-seat divergences from power/exit/beneficiary-victim structure: EPA has institutional power and analytical exit (can reinterpret within legal bounds); fossil operators have organized power and constrained exit (rely on the ceiling, cannot exit without regulatory change); climate advocates have organized power but constrained exit (cannot unilaterally change EPA's legal interpretation). The divergence is structural and measurable.
 *
 * DIRECTIONALITY LOGIC:
 *   Fossil-fuel operators: d approaches 0.1 (full beneficiary; the constraint subsidizes their operational lifetime by blocking the generation-shifting mandate). State energy policymakers: d approaches 0.15–0.25 (beneficiary; state autonomy is preserved, though coal-dependent states benefit more than renewables-committed states). Climate advocates: d approaches 0.85 (full target; the constraint is a ceiling that forecloses their preferred policy). Air-quality communities: d approaches 0.9 (full target; trapped by geography and economic constraints, they bear costs without exit). EPA: d approaches 0.5 (symmetric; the agency coordinates the boundary and enforces facility-level standards, which provides some benefit to all seats, but the ceiling's binding effect asymmetrically favors fossil operators). Courts: d approaches 0.0 (observer/analytical; no extraction benefit, pure adjudicatory role). The directionality derives from beneficiary/victim declarations: fossil operators and states are declared beneficiaries (they collect regulatory certainty and preserved options); climate advocates and air-quality communities are declared victims (they pay in delayed decarbonization and health costs).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem was statutory ambiguity: does Section 111(d) authorize generation-shifting or only facility-level measures? The facility-constraint reading answers: facility-level only. But the founding problem's status has drifted from live to contested, then toward dead. Climate science and renewable-cost trajectories have shifted the problem: the founding ambiguity is no longer the binding constraint; the Supreme Court's (2022) settlement of the ambiguity in favor of facility-constraint is the new constraint. The original mandatrophy question was 'Did EPA exceed its delegated authority?' but the constraint now persists because fossil operators and states have institutional interest in maintaining the ceiling, not because the founding statutory ambiguity remains unresolved. The mandate has been superseded by the institutional interest in regulatory stability. This is a tangled_rope, not a mountain or rope, because the coordination (regulatory boundary-setting) is real but the extraction (foreclosure of generation-shifting) is asymmetric and requires active enforcement (litigation, lobbying, judicial deference to the reading).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    statutory_construction_contest,
    'Does ''best system of emission reduction'' in Section 111(d) refer to measures implementable at individual facilities, or to any system that achieves emissions reduction regardless of implementation locus?',
    'The Supreme Court in West Virginia v. EPA (2022) resolved this in favor of facility-constraint by textualism and constitutional-avoidance reasoning. But the resolution is precedential, not empirical; future courts or a revised statute could overturn it. The contest persists in constitutional law scholarship and environmental advocacy circles.',
    'If systemic-transformation reading prevails (through legislative amendment or Court reversal), the constraint flips from tangled_rope to rope or even evaporates; climate advocates move from payer to beneficiary; fossil operators move from beneficiary to victim. The change is type-determining.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(statutory_construction_contest, conceptual, 'Whether EPA''s delegated authority includes generation-shifting or is limited to facility-level measures—the core constitutional/statutory contest that defines this reading.').

omega_variable(
    major_questions_doctrine_scope,
    'Does the major-questions doctrine apply to Section 111(d) such that generation-shifting mandates would require clear statutory authorization?',
    'Future Supreme Court rulings on major-questions scope, or legislative clarification of EPA''s delegated authority. The doctrine itself is evolving (Dobbs, Student Loan Cases, Biden v. Nebraska all refined its scope).',
    'If major-questions doctrine narrows, EPA''s leeway expands and systemic-transformation becomes more defensible; if it widens, facility-constraint is reinforced. The doctrine is outcome-determinative for this constraint''s type.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(major_questions_doctrine_scope, conceptual, 'Constitutional canon applied to limit EPA''s delegated authority in this reading.').

omega_variable(
    technological_performativity,
    'Are facility-level measures (heat-rate improvement, carbon capture) sufficient to meet climate targets, or do they become purely theatrical/performative as the constraint''s time horizon extends?',
    'Long-term empirical measurement of emissions reductions achieved under facility-level constraints vs. climate targets; IEA, IPCC assessments on feasibility of net-zero under generation-shifting prohibition.',
    'If facility measures prove insufficient, theater_ratio rises toward 1.0 and the constraint becomes closer to piton (persistence through institutional inertia and litigation defense, not genuine function). If sufficient, extractiveness decreases and the constraint approaches rope (real coordination with acceptable costs).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(technological_performativity, empirical, 'Whether facility-level improvements can deliver meaningful emissions reductions or are structurally limited.').

omega_variable(
    identity_lock_in_coal_states,
    'To what extent do coal-dependent state policymakers remain locked into facility-constraint reading via political identity (coal-economy constituents, extractive-industry lobbying, path-dependent energy infrastructure) rather than via genuine policy preference?',
    'Post-constraint-removal scenarios: if energy-transition support emerges in coal states once federal mandate is removed (or returns), identity-lock was substantial; if opposition persists, preferences are genuine. Long-term tracking of coal-state political evolution and energy-policy shifts.',
    'If identity-lock is high, state policymakers are targets (d toward 1.0) rather than beneficiaries; if preferences are genuine, they are beneficiaries (d near 0.0). This affects whether the constraint is tangled_rope (asymmetric extraction with some beneficiaries) or snare (pure extraction via identity capture).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_lock_in_coal_states, empirical, 'Whether coal-state allegiance to the constraint is structural preference or identity-locked capture.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(caa_section_111d_delegation__facility_constraint_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(caa__tr_t0, caa_section_111d_delegation__facility_constraint_reading, theater_ratio, 0, 0.32).
narrative_ontology:measurement_basis(caa__tr_t0, observed).
narrative_ontology:measurement(caa__tr_t3, caa_section_111d_delegation__facility_constraint_reading, theater_ratio, 3, 0.35).
narrative_ontology:measurement_basis(caa__tr_t3, observed).
narrative_ontology:measurement(caa__tr_t6, caa_section_111d_delegation__facility_constraint_reading, theater_ratio, 6, 0.38).
narrative_ontology:measurement_basis(caa__tr_t6, observed).
narrative_ontology:measurement(caa__tr_t10, caa_section_111d_delegation__facility_constraint_reading, theater_ratio, 10, 0.4).
narrative_ontology:measurement_basis(caa__tr_t10, projected).
narrative_ontology:measurement(caa__tr_t15, caa_section_111d_delegation__facility_constraint_reading, theater_ratio, 15, 0.41).
narrative_ontology:measurement_basis(caa__tr_t15, projected).
narrative_ontology:measurement(caa__tr_t20, caa_section_111d_delegation__facility_constraint_reading, theater_ratio, 20, 0.41).
narrative_ontology:measurement_basis(caa__tr_t20, projected).

% Extraction over time
narrative_ontology:measurement(caa__be_t0, caa_section_111d_delegation__facility_constraint_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement_basis(caa__be_t0, observed).
narrative_ontology:measurement(caa__be_t3, caa_section_111d_delegation__facility_constraint_reading, base_extractiveness, 3, 0.58).
narrative_ontology:measurement_basis(caa__be_t3, observed).
narrative_ontology:measurement(caa__be_t6, caa_section_111d_delegation__facility_constraint_reading, base_extractiveness, 6, 0.62).
narrative_ontology:measurement_basis(caa__be_t6, observed).
narrative_ontology:measurement(caa__be_t10, caa_section_111d_delegation__facility_constraint_reading, base_extractiveness, 10, 0.66).
narrative_ontology:measurement_basis(caa__be_t10, projected).
narrative_ontology:measurement(caa__be_t15, caa_section_111d_delegation__facility_constraint_reading, base_extractiveness, 15, 0.68).
narrative_ontology:measurement_basis(caa__be_t15, projected).
narrative_ontology:measurement(caa__be_t20, caa_section_111d_delegation__facility_constraint_reading, base_extractiveness, 20, 0.68).
narrative_ontology:measurement_basis(caa__be_t20, projected).

% Suppression requirement over time
narrative_ontology:measurement(caa__su_t0, caa_section_111d_delegation__facility_constraint_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement_basis(caa__su_t0, observed).
narrative_ontology:measurement(caa__su_t3, caa_section_111d_delegation__facility_constraint_reading, suppression_requirement, 3, 0.63).
narrative_ontology:measurement_basis(caa__su_t3, observed).
narrative_ontology:measurement(caa__su_t6, caa_section_111d_delegation__facility_constraint_reading, suppression_requirement, 6, 0.67).
narrative_ontology:measurement_basis(caa__su_t6, observed).
narrative_ontology:measurement(caa__su_t10, caa_section_111d_delegation__facility_constraint_reading, suppression_requirement, 10, 0.71).
narrative_ontology:measurement_basis(caa__su_t10, projected).
narrative_ontology:measurement(caa__su_t15, caa_section_111d_delegation__facility_constraint_reading, suppression_requirement, 15, 0.72).
narrative_ontology:measurement_basis(caa__su_t15, projected).
narrative_ontology:measurement(caa__su_t20, caa_section_111d_delegation__facility_constraint_reading, suppression_requirement, 20, 0.72).
narrative_ontology:measurement_basis(caa__su_t20, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(caa_section_111d_delegation__facility_constraint_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(caa_section_111d_delegation__facility_constraint_reading, 0.12).
narrative_ontology:affects_constraint(caa_section_111d_delegation__facility_constraint_reading, caa_section_111d_delegation__systemic_transformation_reading).

% DUAL FORMULATION NOTE:
% This constraint and systemic_transformation_reading are the two structurally distinct interpretations of a single contested kernel: Clean Air Act Section 111(d) delegation authority. They share the same text (the statute) but arrive at different ε values, beneficiary/victim sets, and types because they read the text differently. The facility-constraint reading limits EPA to facility-level measures (high extraction, regulatory ceiling); the systemic-transformation reading authorizes generation-shifting mandates (lower extraction on climate advocates' seat, higher on fossil operators' seat). These are not observations of the same constraint from different angles—they are different constraints instantiated by different readings of the same kernel. They are linked here as family members, not as one constraint with observer-relative classification.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(caa_section_111d_delegation__facility_constraint_reading, institutional, 0.52).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
