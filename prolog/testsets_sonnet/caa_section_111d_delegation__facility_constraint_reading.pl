% ============================================================================
% CONSTRAINT STORY: caa_section_111d_delegation__facility_constraint_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
 *   constraint_id: caa_section_111d_delegation__facility_constraint_reading
 *   human_readable: Section 111(d) 'Best System of Emission Reduction' Confined to Inside-the-Fenceline Measures
 *   domain: administrative_law/environmental_regulation/constitutional_interpretation
 *
 * SUMMARY:
 *   This story instantiates the facility-constraint reading of Section
 *   111(d)'s 'best system of emission reduction' clause: the position,
 *   adopted by the Supreme Court majority in West Virginia v. EPA (2022) via
 *   the major questions doctrine, that EPA's authority under this provision
 *   is limited to measures implementable within the fenceline of an
 *   individual regulated facility — heat-rate efficiency improvements and
 *   carbon capture — and cannot extend to grid-wide generation-shifting
 *   strategies such as mandated renewable substitution or accelerated coal
 *   retirement. This is a distinct constraint from the systemic
 *   transformation reading of the same statutory text (a sibling constraint,
 *   not part of this one); the two readings produce materially different ε
 *   values because they gate entirely different sets of regulatory tools and
 *   produce different victim classes. Under this reading, coal generators and
 *   coal-dependent states are structural beneficiaries of a regulatory
 *   ceiling; downwind communities, future populations, and low-carbon
 *   developers are structural victims of the emissions that the ceiling
 *   permits to persist longer than an alternative reading would allow.
 *
 * KEY AGENTS:
 *   - incumbent_coal_generators: primary beneficiary (organized/arbitrage) — retains normal operation under a bounded compliance obligation
 *   - coal_dependent_state_governments: beneficiary and agenda-setter (institutional/mobile) — retains discretion over generation mix
 *   - epa: agenda_setter constrained by the reading (institutional/constrained) — administers a narrowed 'best system' standard it did not choose
 *   - downwind_communities: primary victim (powerless/trapped) — bears health costs of prolonged emissions
 *   - climate_advocacy_organizations: excluded litigant (organized/constrained) — loses the interpretive question, retains no rulemaking leverage
 *   - federal_courts: analytical observer/decider — constitutes the reading through the major questions doctrine
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(caa_section_111d_delegation__facility_constraint_reading, 0.58).
domain_priors:suppression_score(caa_section_111d_delegation__facility_constraint_reading, 0.52).
domain_priors:theater_ratio(caa_section_111d_delegation__facility_constraint_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(caa_section_111d_delegation__facility_constraint_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(caa_section_111d_delegation__facility_constraint_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(caa_section_111d_delegation__facility_constraint_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(caa_section_111d_delegation__facility_constraint_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(caa_section_111d_delegation__facility_constraint_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(caa_section_111d_delegation__facility_constraint_reading, tangled_rope).
narrative_ontology:human_readable(caa_section_111d_delegation__facility_constraint_reading, "Section 111(d) 'Best System of Emission Reduction' Confined to Inside-the-Fenceline Measures").
narrative_ontology:topic_domain(caa_section_111d_delegation__facility_constraint_reading, "administrative_law/environmental_regulation/constitutional_interpretation").

domain_priors:requires_active_enforcement(caa_section_111d_delegation__facility_constraint_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(caa_section_111d_delegation__facility_constraint_reading, 'b19f3b97-fa8a-41fd-b21b-d2862eb76eb0').
narrative_ontology:cs_kernel_codification('b19f3b97-fa8a-41fd-b21b-d2862eb76eb0', fixed_text).
narrative_ontology:cs_authority_grounding('b19f3b97-fa8a-41fd-b21b-d2862eb76eb0', lineage).
narrative_ontology:cs_interpretation_layer_present('b19f3b97-fa8a-41fd-b21b-d2862eb76eb0').
narrative_ontology:cs_reading_relation('b19f3b97-fa8a-41fd-b21b-d2862eb76eb0', caa_section_111d_delegation__systemic_transformation_reading, forecloses).
narrative_ontology:cs_axiom('b19f3b97-fa8a-41fd-b21b-d2862eb76eb0', foundational, delegation_requires_clear_statement_for_major_economic_questions).
narrative_ontology:cs_axiom_status(delegation_requires_clear_statement_for_major_economic_questions, holdable).
narrative_ontology:cs_axiom_grounding('b19f3b97-fa8a-41fd-b21b-d2862eb76eb0', delegation_requires_clear_statement_for_major_economic_questions, conventional).
narrative_ontology:cs_axiom('b19f3b97-fa8a-41fd-b21b-d2862eb76eb0', foundational, best_system_means_technology_applicable_at_the_source).
narrative_ontology:cs_axiom_status(best_system_means_technology_applicable_at_the_source, holdable).
narrative_ontology:cs_axiom_grounding('b19f3b97-fa8a-41fd-b21b-d2862eb76eb0', best_system_means_technology_applicable_at_the_source, conventional).
narrative_ontology:cs_reference_frame('b19f3b97-fa8a-41fd-b21b-d2862eb76eb0', pre_clean_power_plan_source_specific_regulation).
narrative_ontology:cs_drift_state('b19f3b97-fa8a-41fd-b21b-d2862eb76eb0', post_west_virginia_v_epa_2022, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('b19f3b97-fa8a-41fd-b21b-d2862eb76eb0', '').
narrative_ontology:cs_kernel_id(caa_section_111d_delegation__facility_constraint_reading, caa_section_111d_delegation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(caa_section_111d_delegation__facility_constraint_reading, incumbent_coal_generators).
narrative_ontology:constraint_beneficiary(caa_section_111d_delegation__facility_constraint_reading, coal_dependent_state_governments).
narrative_ontology:constraint_beneficiary(caa_section_111d_delegation__facility_constraint_reading, fossil_fuel_dependent_labor_unions).
narrative_ontology:constraint_victim(caa_section_111d_delegation__facility_constraint_reading, downwind_communities).
narrative_ontology:constraint_victim(caa_section_111d_delegation__facility_constraint_reading, future_generations_bearing_climate_costs).
narrative_ontology:constraint_victim(caa_section_111d_delegation__facility_constraint_reading, climate_advocacy_organizations).
narrative_ontology:constraint_victim(caa_section_111d_delegation__facility_constraint_reading, low_carbon_generation_developers).
narrative_ontology:constraint_vindicates(caa_section_111d_delegation__facility_constraint_reading, major_questions_doctrine).
narrative_ontology:constraint_vindicates(caa_section_111d_delegation__facility_constraint_reading, cooperative_federalism_in_energy_policy).
narrative_ontology:constraint_vindicates(caa_section_111d_delegation__facility_constraint_reading, narrow_agency_delegation_construction).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Operate existing coal-fired plants that would face mandatory retirement or capacity-factor reduction under a generation-shifting standard. Under the facility-constraint reading, EPA can only require heat-rate efficiency upgrades or carbon capture retrofits at the plant itself, both of which are technically marginal and often uneconomic at the site level. Effectively continues normal operation with modest compliance costs rather than an existential threat to the fleet.
narrative_ontology:constraint_stakeholder(caa_section_111d_delegation__facility_constraint_reading, incumbent_coal_generators, beneficiary,
    organized, biographical, arbitrage, national).

% Retain full discretion over their generation mix and can decline to accelerate the retirement of coal capacity that anchors local tax bases and employment. The facility-constraint reading confirms their authority to set state implementation plans without being forced into renewable procurement or coal-to-gas switching schedules set by EPA.
narrative_ontology:constraint_stakeholder(caa_section_111d_delegation__facility_constraint_reading, coal_dependent_state_governments, beneficiary,
    institutional, generational, mobile, regional).
narrative_ontology:stakeholder_secondary_role(caa_section_111d_delegation__facility_constraint_reading, coal_dependent_state_governments, agenda_setter).

% Represent plant and mining workers whose jobs depend on continued coal-fired generation. The narrow reading delays or prevents forced plant closures that would otherwise displace their members; their leverage comes from political mobilization in coal-producing states rather than legal standing in the underlying dispute.
narrative_ontology:constraint_stakeholder(caa_section_111d_delegation__facility_constraint_reading, fossil_fuel_dependent_labor_unions, beneficiary,
    organized, biographical, constrained, regional).

% Administers Section 111(d) and must set the 'best system of emission reduction' standard. Under this reading, EPA's authority is confined to measures achievable at the individual source — heat-rate improvements, carbon capture and storage — foreclosing the more ambitious Clean Power Plan-style approach of counting grid-wide generation shifting as part of the 'system.' EPA did not choose this confinement; it was imposed by judicial interpretation of the statute's scope.
narrative_ontology:constraint_stakeholder(caa_section_111d_delegation__facility_constraint_reading, epa, agenda_setter,
    institutional, generational, constrained, national).

% Bear the ongoing public health burden of particulate and criteria pollutant emissions co-located with the greenhouse gas emissions the facility-constraint reading permits to continue at higher aggregate levels than a generation-shifting standard would allow. They have no standing role in setting the 'best system' standard and cannot relocate away from emission sources without significant personal cost.
narrative_ontology:constraint_stakeholder(caa_section_111d_delegation__facility_constraint_reading, downwind_communities, payer,
    powerless, biographical, trapped, regional).

% Litigate and lobby for the systemic transformation reading of the same statutory text, arguing that limiting 'best system' to fenceline measures guts the statute's capacity to address a whole-of-grid pollutant. Under the facility-constraint reading their preferred policy instrument is foreclosed as a matter of law, not merely disfavored as policy, and their only remaining paths are new legislation or a different judicial coalition.
narrative_ontology:constraint_stakeholder(caa_section_111d_delegation__facility_constraint_reading, climate_advocacy_organizations, excluded,
    organized, generational, constrained, national).

% Inherit the cumulative atmospheric loading that a fenceline-limited compliance regime permits to accrue more slowly to abate than a generation-shifting regime would achieve. They have no representation in the current adjudication and cannot bargain over the discount rate applied to their exposure.
narrative_ontology:constraint_stakeholder(caa_section_111d_delegation__facility_constraint_reading, future_generations_bearing_climate_costs, payer,
    powerless, civilizational, trapped, global).

% Build wind, solar, and gas-replacement capacity that would gain regulatory-driven market share under a generation-shifting standard. Under the facility-constraint reading, EPA cannot use Section 111(d) to accelerate that substitution, so these developers must compete on price and state-level policy alone rather than benefiting from a federal emissions-driven dispatch shift.
narrative_ontology:constraint_stakeholder(caa_section_111d_delegation__facility_constraint_reading, low_carbon_generation_developers, payer,
    moderate, biographical, constrained, national).

% Adjudicate the scope of 'best system of emission reduction' under the major questions doctrine, deciding whether Congress spoke clearly enough to delegate grid-wide transformation authority to EPA. Their ruling constitutes the reading itself rather than merely observing it, but they hold no stake in the underlying energy or climate outcome.
narrative_ontology:constraint_stakeholder(caa_section_111d_delegation__facility_constraint_reading, federal_courts, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Confines EPA's Section 111(d) rulemaking to measures a single regulated source can implement on its own footprint, giving covered facilities and states a predictable, bounded compliance obligation instead of an open-ended mandate to reshape the regional generation mix.
% TRANSFER_FUNCTION: Moves the burden of climate mitigation cost away from incumbent fossil generators and the states that host them, and onto downwind and future populations who absorb the emissions that a broader 'best system' standard would have reduced faster; it also transfers market share away from low-carbon developers who would have gained under a generation-shifting rule.
% ABSENT_VOICES: Downwind communities and future populations bearing accumulated climate damages have no seat in the statutory-interpretation dispute; climate advocacy organizations are present as litigants but structurally lose the interpretive question and are excluded from the actual rulemaking authority they sought to expand.
% DISAPPEARANCE_RATIONALE: If the facility-constraint reading were abandoned in favor of the systemic transformation reading, EPA could again design a plan that credits generation-shifting and renewable substitution as part of 'best system,' materially accelerating coal retirement schedules, reallocating market share toward low-carbon developers, and lowering aggregate emissions faster — the entire compliance architecture for existing power plants would be rebuilt.
% FOUNDING_PROBLEM: Congress enacted Section 111(d) to give EPA a residual authority to regulate stationary-source pollutants, including greenhouse gases, that are not otherwise covered by the Clean Air Act's other programs — the founding problem was closing a gap in pollutant coverage for existing sources.
% FOUNDING_PROBLEM_CORROBORATION: EPA under the Clean Power Plan attested the founding problem required grid-wide flexibility to be solved effectively for a pollutant that disperses regionally; the Supreme Court majority in West Virginia v. EPA and coal-state attorneys general attest the founding problem is properly solved only through fenceline measures absent clearer congressional authorization; independent administrative law scholars outside both camps have documented that the statutory text and 1990 legislative history are genuinely ambiguous on which reading Congress intended, corroborating that the status is contested rather than settled by either side's account.
narrative_ontology:disappearance_verdict(caa_section_111d_delegation__facility_constraint_reading, world_rearranges).
narrative_ontology:founding_problem_status(caa_section_111d_delegation__facility_constraint_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(caa_section_111d_delegation__facility_constraint_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(caa_section_111d_delegation__facility_constraint_reading, 'none', 1).
narrative_ontology:epsilon_provenance(caa_section_111d_delegation__facility_constraint_reading, 0.58, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness (0.58) reflects substantial but not maximal transfer: the facility-constraint reading does not eliminate emission reduction, it caps the toolkit EPA may deploy, which produces slower abatement and a real cost borne by downwind and future populations relative to the counterfactual systemic-transformation regime. Suppression (0.52) is moderate — the ceiling is enforced through judicial precedent (major questions doctrine) rather than direct coercive apparatus, but it does foreclose an entire category of regulatory design as a matter of law, which is a strong form of suppression against the excluded advocacy coalition. Theater ratio (0.42) is elevated because a growing share of subsequent EPA rulemaking activity is now visibly organized around demonstrating fenceline-only compliance mechanisms (marginal heat-rate gains, contested-feasibility carbon capture mandates) whose emissions impact is small relative to the political and litigation resources spent defending or attacking the boundary itself. Accessibility collapse (0.6) is moderate-high: once the major questions doctrine framing is accepted, the alternative reading is not merely disfavored but treated as constitutionally suspect, closing off the systemic-transformation path short of new legislation. Resistance (0.72) is high because well-resourced state attorneys general, industry associations, and climate litigators continue to contest the boundary in ongoing rulemakings and litigation.
 *
 * DIRECTIONALITY LOGIC:
 *   Incumbent coal generators and coal-dependent states sit near the beneficiary end: the ceiling directly subsidizes their continued operation and policy discretion by removing an entire category of federal leverage. Downwind communities and future populations sit near the full-target end: they bear diffuse, delayed, and non-negotiable costs from the emissions the ceiling permits, with no exit — geography and time both trap them. Climate advocacy organizations are victims of a narrower kind: they are organized and resourced but structurally excluded from the tool they sought, which caps their achievable outcome regardless of continued mobilization. Low-carbon generation developers are moderate-power payers: they lose a federal demand driver but retain state-level and market-based paths, so their exit is constrained rather than trapped.
 *
 * MANDATROPHY ANALYSIS:
 *   The facility-constraint reading is not itself an obsolete mandate — the underlying statutory delegation question (how much authority Congress gave EPA over stationary-source emissions) remains live and contested, which is why founding_problem_status is authored as contested rather than dead. The classification as tangled_rope (not snare) is deliberate: EPA's fenceline authority is genuine coordination — it solves a real problem of preventing inconsistent, source-by-source pollution control chaos — while the confinement of that authority to fenceline measures simultaneously and asymmetrically protects incumbent coal capital and imposes costs on downwind and future populations through the same statutory mechanism. Reading this as a pure snare would miss the real coordination function Section 111(d) performs for regulated sources generally; reading it as a pure rope would miss the asymmetric extraction the ceiling imposes on populations with no voice in the rulemaking.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    major_questions_doctrine_scope_ambiguity,
    'Does the major questions doctrine correctly identify Section 111(d)''s ''best system'' language as a case requiring clear congressional authorization for generation-shifting measures, or does it improperly import a clear-statement rule into ordinary statutory construction to reach a policy-preferred outcome?',
    'Comparison with subsequent major questions doctrine applications across other regulatory domains (e.g., OSHA vaccine mandate, student loan forgiveness) to determine whether the doctrine is applied with a consistent, principled threshold or selectively to environmental and economically significant regulations.',
    'If the doctrine is applied principledly, the facility-constraint reading is a legitimate check on unauthorized delegation; if applied selectively to block particular policy outcomes, the reading functions as judicially manufactured extraction protecting a specific industry rather than a neutral interpretive rule.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(major_questions_doctrine_scope_ambiguity, conceptual, 'Whether the major questions doctrine is a principled interpretive constraint or an outcome-driven judicial override in this application.').

omega_variable(
    committer_reading_disagreement_location,
    'Where exactly do the facility-constraint and systemic-transformation readings of the caa_section_111d_delegation kernel diverge — is it the definition of ''system'' (a single facility''s technology vs. the interconnected grid it participates in), or is it a background theory of how much delegated discretion Congress can grant an agency for a rapidly evolving pollutant like greenhouse gases?',
    'Close textual analysis of the 1990 Clean Air Act Amendments'' legislative history alongside comparison to how ''system'' is used in adjacent Clean Air Act provisions (e.g., Section 111(a)''s new-source performance standards) to determine whether ''system'' has a settled technical meaning that resolves the ambiguity.',
    'If ''system'' has a settled technical meaning favoring facility-level measures, the systemic-transformation reading is the outlier construction; if the term is genuinely open-textured and historically applied at varying scopes, the facility-constraint reading is the narrowing construction and the disagreement is properly located in delegation theory, not textual meaning.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(committer_reading_disagreement_location, conceptual, 'Locating whether the reading split is fundamentally textual (meaning of ''system'') or theoretical (scope of permissible delegation).').

omega_variable(
    coal_sector_protection_durability,
    'Is the coal sector protection produced by this reading a durable structural outcome, or a temporary reprieve that market economics (natural gas price competition, renewable cost declines) will override regardless of the regulatory ceiling?',
    'Track coal plant retirement rates and capacity factors under the facility-constraint regime versus market-driven baseline retirement projections absent any Section 111(d) constraint.',
    'If retirement proceeds at similar rates regardless of the regulatory reading, the constraint''s practical extraction from climate victims is smaller than the ε value suggests, since market forces would achieve similar reductions independently; if retirement is meaningfully slower under the ceiling, the extraction is substantially attributable to the regulatory reading itself.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coal_sector_protection_durability, empirical, 'Whether the ceiling''s coal-protective effect is causally significant or largely superseded by market dynamics.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(caa_section_111d_delegation__facility_constraint_reading, 2015, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(caa__tr_t2015, caa_section_111d_delegation__facility_constraint_reading, theater_ratio, 2015, 0.2).
narrative_ontology:measurement(caa__tr_t2017, caa_section_111d_delegation__facility_constraint_reading, theater_ratio, 2017, 0.25).
narrative_ontology:measurement(caa__tr_t2019, caa_section_111d_delegation__facility_constraint_reading, theater_ratio, 2019, 0.3).
narrative_ontology:measurement(caa__tr_t2021, caa_section_111d_delegation__facility_constraint_reading, theater_ratio, 2021, 0.35).
narrative_ontology:measurement(caa__tr_t2022, caa_section_111d_delegation__facility_constraint_reading, theater_ratio, 2022, 0.4).
narrative_ontology:measurement(caa__tr_t2024, caa_section_111d_delegation__facility_constraint_reading, theater_ratio, 2024, 0.42).

% Extraction over time
narrative_ontology:measurement(caa__be_t2015, caa_section_111d_delegation__facility_constraint_reading, base_extractiveness, 2015, 0.3).
narrative_ontology:measurement(caa__be_t2017, caa_section_111d_delegation__facility_constraint_reading, base_extractiveness, 2017, 0.34).
narrative_ontology:measurement(caa__be_t2019, caa_section_111d_delegation__facility_constraint_reading, base_extractiveness, 2019, 0.4).
narrative_ontology:measurement(caa__be_t2021, caa_section_111d_delegation__facility_constraint_reading, base_extractiveness, 2021, 0.48).
narrative_ontology:measurement(caa__be_t2022, caa_section_111d_delegation__facility_constraint_reading, base_extractiveness, 2022, 0.55).
narrative_ontology:measurement(caa__be_t2024, caa_section_111d_delegation__facility_constraint_reading, base_extractiveness, 2024, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(caa__su_t2015, caa_section_111d_delegation__facility_constraint_reading, suppression_requirement, 2015, 0.35).
narrative_ontology:measurement(caa__su_t2017, caa_section_111d_delegation__facility_constraint_reading, suppression_requirement, 2017, 0.38).
narrative_ontology:measurement(caa__su_t2019, caa_section_111d_delegation__facility_constraint_reading, suppression_requirement, 2019, 0.42).
narrative_ontology:measurement(caa__su_t2021, caa_section_111d_delegation__facility_constraint_reading, suppression_requirement, 2021, 0.46).
narrative_ontology:measurement(caa__su_t2022, caa_section_111d_delegation__facility_constraint_reading, suppression_requirement, 2022, 0.5).
narrative_ontology:measurement(caa__su_t2024, caa_section_111d_delegation__facility_constraint_reading, suppression_requirement, 2024, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(caa_section_111d_delegation__facility_constraint_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(caa_section_111d_delegation__facility_constraint_reading, 0.1).
narrative_ontology:affects_constraint(caa_section_111d_delegation__facility_constraint_reading, caa_section_111d_delegation__systemic_transformation_reading).

% DUAL FORMULATION NOTE:
% This story and caa_section_111d_delegation__systemic_transformation_reading are the two readings of a single contested kernel (caa_section_111d_delegation): the scope of EPA's 'best system of emission reduction' authority under Clean Air Act Section 111(d). They are not the same constraint measured two ways — per the epsilon-invariance principle, each reading gates a structurally distinct regulatory toolkit, produces a distinct beneficiary/victim set, and carries its own epsilon. The facility-constraint reading (this story) protects incumbent coal capital and state energy-mix autonomy at the cost of downwind and future populations; the systemic-transformation reading would reverse that allocation, protecting climate outcomes and low-carbon developers at the cost of coal sector continuity. Both are linked here so contamination and drift analysis can track how developments in one reading's legitimacy (e.g., new litigation, legislative clarification) propagate pressure onto the other.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(caa_section_111d_delegation__facility_constraint_reading, organized, 0.05).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
