% ============================================================================
% CONSTRAINT STORY: caa_section_111d_delegation__systemic_transformation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_caa_section_111d_systemic_transformation, []).

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
 *   constraint_id: caa_section_111d_delegation__systemic_transformation_reading
 *   human_readable: Section 111(d) Best System Systemic Transformation Reading
 *   domain: administrative_law/environmental_regulation/constitutional_interpretation
 *
 * SUMMARY:
 *   Section 111(d) of the Clean Air Act authorizes EPA to regulate emissions
 *   from existing fossil fuel power plants. The statute's 'best system of
 *   emissions reduction' language is ambiguous: it can be read to permit
 *   facility-level measures only (facility_constraint_reading: heat-rate
 *   improvements, efficiency retrofits, carbon capture) or grid-level
 *   systemic transformation (systemic_transformation_reading: renewable
 *   substitution, coal retirement, demand-side shifts). This story
 *   instantiates the systemic transformation reading—the interpretation that
 *   EPA can mandate state-level decarbonization pathways by requiring fossil
 *   generators to reduce their generation share, retire coal plants, and cede
 *   market share to renewables. Under this reading, the extraction is
 *   substantial: fossil fuel generators and coal-dependent states become
 *   victims of regulatory wealth transfer to renewable developers. Coal
 *   mining workers face identity-locked displacement. Environmental
 *   regulators and renewable developers become beneficiaries. The constraint
 *   is CLAIMED as tangled rope (coordination function + asymmetric extraction
 *   + active enforcement) while the authored metrics describe substantial and
 *   accelerating extractive operation. The sibling reading
 *   (facility_constraint_reading) would classify as a rope or mountain
 *   (limited coordination, minimal extraction, lower enforcement need)
 *   because it constrains EPA to facility-level optimization rather than
 *   systemic reallocation. This story does not address that reading—it
 *   generates the systemic transformation reading only, per Rule 1.
 *
 * KEY AGENTS:
 *   - EPA: agenda-setter interpreting statutory language; instantiates the systemic reading through rulemaking
 *   - Coal-dependent states: victims bearing exit costs from mandated generation retirement
 *   - Fossil fuel generators: victims with constrained exit facing asset stranding
 *   - Coal mining workforce: powerless, identity-locked victims facing occupational displacement
 *   - Renewable developers: beneficiaries capturing regulatory demand
 *   - Climate advocacy coalitions: beneficiaries vindicated by expansive delegation doctrine
 *   - Judiciary: excluded observer whose West Virginia v. EPA ruling partially constrained scope
 *   - Congress: observer; could amend statute to clarify scope but has not
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(caa_section_111d_delegation__systemic_transformation_reading, 0.68).
domain_priors:suppression_score(caa_section_111d_delegation__systemic_transformation_reading, 0.72).
domain_priors:theater_ratio(caa_section_111d_delegation__systemic_transformation_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(caa_section_111d_delegation__systemic_transformation_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(caa_section_111d_delegation__systemic_transformation_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(caa_section_111d_delegation__systemic_transformation_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(caa_section_111d_delegation__systemic_transformation_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(caa_section_111d_delegation__systemic_transformation_reading, resistance, 0.78).

% --- Constraint claim ---
narrative_ontology:constraint_claim(caa_section_111d_delegation__systemic_transformation_reading, tangled_rope).
narrative_ontology:human_readable(caa_section_111d_delegation__systemic_transformation_reading, "Section 111(d) Best System Systemic Transformation Reading").
narrative_ontology:topic_domain(caa_section_111d_delegation__systemic_transformation_reading, "administrative_law/environmental_regulation/constitutional_interpretation").

domain_priors:requires_active_enforcement(caa_section_111d_delegation__systemic_transformation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(caa_section_111d_delegation__systemic_transformation_reading, '23496b26-d000-4e6d-b800-d01f5d324d05').
narrative_ontology:cs_kernel_codification('23496b26-d000-4e6d-b800-d01f5d324d05', fixed_text).
narrative_ontology:cs_authority_grounding('23496b26-d000-4e6d-b800-d01f5d324d05', extraction).
narrative_ontology:cs_interpretation_layer_present('23496b26-d000-4e6d-b800-d01f5d324d05').
narrative_ontology:cs_reading_relation('23496b26-d000-4e6d-b800-d01f5d324d05', caa_section_111d_delegation__facility_constraint_reading, forecloses).
narrative_ontology:cs_axiom('23496b26-d000-4e6d-b800-d01f5d324d05', foundational, best_system_includes_generation_shifting).
narrative_ontology:cs_axiom_status(best_system_includes_generation_shifting, holdable).
narrative_ontology:cs_axiom_grounding('23496b26-d000-4e6d-b800-d01f5d324d05', best_system_includes_generation_shifting, empirically_contingent).
narrative_ontology:cs_axiom('23496b26-d000-4e6d-b800-d01f5d324d05', foundational, epa_jurisdiction_extends_to_grid_architecture).
narrative_ontology:cs_axiom_status(epa_jurisdiction_extends_to_grid_architecture, holdable).
narrative_ontology:cs_axiom_grounding('23496b26-d000-4e6d-b800-d01f5d324d05', epa_jurisdiction_extends_to_grid_architecture, deontological).
narrative_ontology:cs_reference_frame('23496b26-d000-4e6d-b800-d01f5d324d05', clean_air_act_expansive_delegation_framework).
narrative_ontology:cs_drift_state('23496b26-d000-4e6d-b800-d01f5d324d05', post_west_virginia_v_epa_2022, gap(authority_erosion, substantial, true)).
narrative_ontology:cs_created_at('23496b26-d000-4e6d-b800-d01f5d324d05', '').
narrative_ontology:cs_kernel_id(caa_section_111d_delegation__systemic_transformation_reading, caa_section_111d_delegation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(caa_section_111d_delegation__systemic_transformation_reading, renewable_energy_developers).
narrative_ontology:constraint_beneficiary(caa_section_111d_delegation__systemic_transformation_reading, climate_advocacy_coalitions).
narrative_ontology:constraint_beneficiary(caa_section_111d_delegation__systemic_transformation_reading, environmental_regulators).
narrative_ontology:constraint_victim(caa_section_111d_delegation__systemic_transformation_reading, coal_dependent_states).
narrative_ontology:constraint_victim(caa_section_111d_delegation__systemic_transformation_reading, fossil_fuel_generators).
narrative_ontology:constraint_victim(caa_section_111d_delegation__systemic_transformation_reading, coal_mining_workforce).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(caa_section_111d_delegation__systemic_transformation_reading, coal_mining_workforce).
narrative_ontology:constraint_victim(caa_section_111d_delegation__systemic_transformation_reading, fossil_fuel_investors).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets Section 111(d) to authorize grid-level decarbonization mandates. Sets compliance standards requiring state-level deployment of renewable generation, early retirement of coal plants, and demand-side efficiency. Justifies the interpretation as necessary to meet the statute's broad 'best system of emissions reduction' language and the Clean Air Act's environmental mandate. Issues regulations that shift capital flows away from coal infrastructure toward renewable deployment.
narrative_ontology:constraint_stakeholder(caa_section_111d_delegation__systemic_transformation_reading, environmental_protection_agency, agenda_setter,
    institutional, generational, analytical, national).

% Face mandates to close or retire coal-fired generation capacity within compliance timelines, stranding assets and tax base. Must deploy renewable generation or purchase renewable energy credits to meet EPA standards. Their exit options are constrained by their geography (coal resources, grid architecture), existing workforce and fiscal dependence on coal revenues, and the cost of rapid transition. They argue the regulation exceeds EPA authority and treats coal as a victim of an agency power grab rather than a legitimate target of environmental policy.
narrative_ontology:constraint_stakeholder(caa_section_111d_delegation__systemic_transformation_reading, coal_dependent_states, payer,
    powerful, generational, constrained, regional).

% Own and operate coal and natural gas generation assets. Face accelerated retirement timelines and compliance costs that render marginal facilities uneconomic. They cannot simply relocate generation (sunk infrastructure) or exit the electricity market (long-term contracts, FERC obligations). Their options are absorbing costs, litigation, or divesting assets at depressed valuations. They coordinate opposition through trade groups and regulatory proceedings.
narrative_ontology:constraint_stakeholder(caa_section_111d_delegation__systemic_transformation_reading, fossil_fuel_generators, payer,
    organized, biographical, constrained, national).

% Employees of coal mining operations and coal-dependent power plants whose jobs disappear as infrastructure retires. Identity is tied to mining communities, occupational training, and family legacy. They face displacement and retraining costs; geographic mobility is limited by property values and community ties. They receive minimal direct transition support from regulatory design and are structurally locked in through occupational identity and regional economic dependence.
narrative_ontology:constraint_stakeholder(caa_section_111d_delegation__systemic_transformation_reading, coal_mining_workforce, payer,
    powerless, biographical, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(caa_section_111d_delegation__systemic_transformation_reading, coal_mining_workforce, beneficiary).

% Benefit from EPA interpretation that creates regulatory demand for renewable generation. The mandate to deploy renewables or purchase renewable energy credits drives capital flows toward solar, wind, and battery storage projects. They can site projects across multiple geographies and have lower sunk-cost exposure than coal generators. They benefit from investor certainty that compliance obligations will sustain demand for their output.
narrative_ontology:constraint_stakeholder(caa_section_111d_delegation__systemic_transformation_reading, renewable_energy_developers, beneficiary,
    organized, biographical, mobile, national).

% Argue that systemic grid transformation is necessary to meet climate targets and that Section 111(d)'s 'best system' language authorizes EPA to mandate it. They vindicate an expansive delegation doctrine and environmental protection prioritization. They benefit from regulatory outcomes aligned with their policy goals, though they do not collect extraction rents—they benefit through policy alignment.
narrative_ontology:constraint_stakeholder(caa_section_111d_delegation__systemic_transformation_reading, climate_advocacy_coalitions, beneficiary,
    organized, generational, mobile, global).

% State environmental agencies benefit from EPA's systemic interpretation because it validates broad regulatory authority and provides federal backing for decarbonization mandates they favor. They become agents for implementing grid transformation at the state level, expanding their institutional scope and justifying resource allocation. Their exit options are analytical—they enforce within a legal framework they partly shaped through rulemaking participation.
narrative_ontology:constraint_stakeholder(caa_section_111d_delegation__systemic_transformation_reading, environmental_regulators, beneficiary,
    institutional, generational, analytical, national).

% Would adjudicate the scope of EPA authority under the Administrative Procedure Act and constitutional non-delegation doctrine if challenged. Currently excluded from the initial rulemaking and implementation, though multiple fossil-fuel interests and states are contesting EPA's reading in federal court. The major question doctrine and Chevron deference are points of contention that would structure judicial review if the case reaches appellate resolution.
narrative_ontology:constraint_stakeholder(caa_section_111d_delegation__systemic_transformation_reading, judiciary, excluded,
    institutional, generational, trapped, national).

% Experience stranded asset write-downs and portfolio pressure as coal infrastructure becomes economically obsolete faster than market fundamentals would otherwise require. They face fiduciary exposure and reputational costs. However, they retain exit options by liquidating and redeploying capital to renewable infrastructure or other sectors—mobility is constrained by timing and valuation losses, not by binding identity or geography.
narrative_ontology:constraint_stakeholder(caa_section_111d_delegation__systemic_transformation_reading, fossil_fuel_investors, payer,
    powerful, biographical, arbitrage, global).

% Enacted the Clean Air Act and Section 111(d) with ambiguous delegation language ('best system of emissions reduction'). Congress could amend the statute to clarify EPA's scope (facility-based vs. systemic), but has not done so despite decades of regulatory evolution. Congress observes the regulatory interpretation; the legislative grid has not shifted in response.
narrative_ontology:constraint_stakeholder(caa_section_111d_delegation__systemic_transformation_reading, congress, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(caa_section_111d_delegation__systemic_transformation_reading, renewable_energy_developers).
narrative_ontology:fixing_cost_class(caa_section_111d_delegation__systemic_transformation_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates national decarbonization across state and federal regulatory levels: EPA sets performance standards that force states to adopt renewable generation, retire coal, and invest in efficiency. The coordination solves the collective-action problem of carbon externalities that individual facilities and states cannot internalize unilaterally—decarbonization requires grid-scale infrastructure shifts, not facility-level optimization alone.
% TRANSFER_FUNCTION: Transfers wealth from fossil fuel generators and coal-dependent regions to renewable developers and climate-aligned jurisdictions. The regulatory mandate mandates capital deployment toward renewable infrastructure (solar, wind, battery storage) by making fossil generation economically untenable within compliance timelines. States and utilities must purchase renewable energy or build renewable capacity, creating revenue streams for renewable developers that would not exist without the regulatory acceleration.
% ABSENT_VOICES: Fossil fuel industry representatives participate in formal rulemaking, but their preferred reading (facility_constraint_reading) has been rejected by EPA. Coal-dependent communities and coal-mining workers have minimal institutional voice in rulemaking—they are documented in comment record but structurally excluded from agenda-setting. Congressional representatives from coal states voice opposition but lack statutory tools to override EPA interpretation without legislative amendment. Future generations (for whom climate mitigation is primarily beneficial) are abstractly represented by climate advocacy groups but have no direct voice.
% DISAPPEARANCE_RATIONALE: If the systemic transformation reading disappeared and were replaced by the facility_constraint_reading, capital flows would reverse: coal plants would retire on market timelines rather than regulatory mandate, renewable deployment would slow to market rates (lower subsidized demand), and state-level climate commitments would weaken. The U.S. electricity system would decarbonize more slowly. If Section 111(d) disappeared entirely, fossil fuel generation would face only fragmented state-level regulation and economic pressure—grid-scale coordination would dissolve.
% FOUNDING_PROBLEM: The Clean Air Act Section 111 was enacted to control air pollution from power plants. The founding problem for Section 111(d) specifically was managing emissions from new and existing fossil fuel generators. The systemic transformation reading interprets 'best system' to encompass generation-shifting (retiring high-carbon generation, deploying renewables) as the most effective emissions reduction strategy at grid scale, rather than limiting regulation to efficiency measures at individual facilities.
% FOUNDING_PROBLEM_CORROBORATION: EPA and environmental advocacy groups attest that systemic grid transformation is necessary to meet the statute's emissions reduction mandate and is the most effective 'best system' available. The fossil fuel industry and coal-dependent states attest that the statute intended facility-level measures only, and that systemic transformation exceeds EPA authority. Federal courts have not issued final rulings on the specific scope question (West Virginia v. EPA, 142 S.Ct. 2587 (2022) restricted EPA authority but did not resolve the facility-constraint vs. systemic-transformation boundary). Academic administrative law literature and the Congressional Record show persistent disagreement on the original statutory intent.
narrative_ontology:disappearance_verdict(caa_section_111d_delegation__systemic_transformation_reading, world_rearranges).
narrative_ontology:founding_problem_status(caa_section_111d_delegation__systemic_transformation_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(caa_section_111d_delegation__systemic_transformation_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(caa_section_111d_delegation__systemic_transformation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(caa_section_111d_delegation__systemic_transformation_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(caa_section_111d_delegation__systemic_transformation_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(caa_section_111d_delegation__systemic_transformation_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(caa_section_111d_delegation__systemic_transformation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness measures at 0.68 (interval end) because EPA's interpretation reallocates substantial capital flows away from fossil generation toward renewables, with limited compensation for stranded assets or transition workers. The extraction accelerates over the interval (0.45 to 0.68) as compliance timelines tighten and asset retirement deadlines approach. Suppression measures at 0.72 because EPA must actively defend its interpretation against legal challenges (West Virginia v. EPA, ongoing litigation in multiple federal courts) and must enforce compliance via administrative orders and penalty mechanisms—the constraint persists not by participant preference but by regulatory force. Theater is moderate (0.41) because a genuine coordination function (grid-scale decarbonization) exists, but a growing share of the regulatory apparatus defends the capital reallocation rather than the original pollution-control objective. The measurement series models the constraint's trajectory: extraction rises as EPA regulations tighten timelines and as market actors internalize the irreversibility of the mandates; theater ratios rise as regulatory rhetoric about environmental necessity increases relative to facility-level cost-minimization; suppression plateaus at t=25 as the judicial and regulatory machinery required to maintain the constraint reaches steady state. All measurements share one time grid per the alignment rule (every metric authored at every examined time point from t=0 to t=35).
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the agenda-setter seat compute radically different directionality. EPA (d near 0.0, full beneficiary of its own authority expansion) perceives the arrangement as genuine coordination necessary for climate goals. Coal generators and coal states (d near 1.0, full targets of regulatory wealth transfer) perceive the same structure as illegitimate extraction, a power grab dressed in environmental language. Renewable developers (d near 0.0, beneficiaries of regulatory demand creation) perceive alignment with environmental necessity. Coal mining workers (d near 1.0, identity-locked, powerless) perceive displacement uncompensated. The engine computes these divergences from power × exit × beneficiary/victim declarations; the authored metrics provide the empirical content the engine reads.
 *
 * DIRECTIONALITY LOGIC:
 *   EPA instantiates this reading and sets its standards; beneficiary status derives from alignment with EPA's interpretation (renewables developers, climate groups, environmental regulators). Victim status derives from bearing the exit costs: fossil generators face asset stranding (constrained exit, sunk infrastructure); coal states face fiscal shock and mandated capacity retirement (constrained exit, geographic lock); coal workers face occupational displacement (identity-locked exit, regional economic dependence). Beneficiaries like renewable developers have higher exit options (arbitrage, mobile) because renewable assets are fungible across geographies and timing is flexible compared to coal plant retirement. Victims like coal workers have low exit options (identity_locked: occupational identity, family legacy in mining communities, regional property asset depreciation). The directionality derivation maps these declarations to d values: full-target victims get d near 1.0, full-beneficiary developers get d near 0.0, EPA (analytical, institutional, perpetual horizon) sits at d near 0.2—it perceives alignment with the statute and environmental law, though it extracts authority from the ambiguity.
 *
 * MANDATROPHY ANALYSIS:
 *   This story resolves a mandatrophy risk: does the constraint persist because the founding problem (power-plant emissions) remains live, or because the beneficiary institutions (EPA, renewable developers, climate advocates) sustain the regulatory apparatus even as the founding problem shifts? The founding_problem_status is CONTESTED because EPA attests the problem is still live (climate change, air quality) while fossil interests attest the problem is solved (modern coal plants already control most classical air pollutants, and the statute never intended climate regulation). The divergence prevents false certification as a rope (genuine coordination only). The theater_ratio rising from 0.25 to 0.41 models the mandatrophy question: as compliance progresses, more EPA effort defends the regulatory interpretation against legal challenge (West Virginia v. EPA, pending cases) than optimizes emissions reduction at given infrastructure. The constraint persists via active enforcement (suppression = 0.72) even as the founding problem's status fragments. An omega variable documents this irreducible uncertainty: whether the systemic transformation reading is justified by genuine climate necessity (founding_problem live) or by institutional self-perpetuation (mandatrophy resolved).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    founding_problem_obsolescence_vs_mandatrophy,
    'Is Section 111(d) sustained by genuine ongoing need for power-sector emissions reduction (founding problem live), or by institutional beneficiaries (EPA, renewable industry, climate advocates) perpetuating the regulatory apparatus after the founding problem (classical air pollution from power plants) has been substantially solved (mandatrophy resolved)?',
    'Track the composition of EPA enforcement: if EPA enforcement effort increasingly targets generation-shifting and retirement (system-level reallocation) rather than facility-level pollutant control, that signals mandatrophy. Compare regulatory justifications in pre-2015 Section 111(d) proceedings (pollution control dominant) to post-2015 (climate, system-level goals dominant). Survey whether coal plants still generate classical air pollution violations absent the systemic reading''s pressure.',
    'If mandatrophy is resolved, the constraint reclassifies from tangled_rope (coordination + asymmetric extraction) to snare (extraction with cover story). The beneficiary/victim structure remains unchanged, but the legitimacy claim collapses.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(founding_problem_obsolescence_vs_mandatrophy, empirical, 'Whether the constraint is sustained by genuine founding-problem persistence or by institutional self-perpetuation after the original problem has been solved.').

omega_variable(
    statutory_delegation_scope_reading_contest,
    'Does the Clean Air Act Section 111(d) ''best system of emissions reduction'' language genuinely authorize EPA to mandate grid-level generation-shifting, or does the facility_constraint_reading represent the statute''s actual original meaning and the systemic transformation reading is a post-hoc interpretive expansion?',
    'Legislative history analysis, historical EPA rulemaking before the systemic transformation reading emerged (pre-2014 Section 111(d) history), and eventual Supreme Court resolution if major question doctrine review occurs. A Congressional amendment clarifying scope would be the definitive resolution.',
    'If the facility-constraint reading is adjudicated correct, this constraint''s ε and type become determined by the judicial verdict rather than authorial claim. The systemic transformation reading would become invalid, and capital flows would reverse. This is a core jurisdictional question, not an empirical uncertainty.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(statutory_delegation_scope_reading_contest, conceptual, 'Whether the systemic transformation reading is a correct interpretation of statutory language or a post-hoc institutional expansion.').

omega_variable(
    coal_worker_transition_suppression_mechanism,
    'Is the measured suppression (0.72) primarily structural (economic barriers to exit, geographic lock-in, sunk skills, limited job availability) or internalized (coal mining workers have been isolated from alternative-path information, have fused occupational identity with fossil fuel industry, trust industry messaging that the constraint is illegitimate)?',
    'Post-exit trajectory studies: if workers who leave coal mining regions maintain suppressed opportunity-seeking and lower economic resilience years after relocation, suppression is partly internalized. If workers exposed to transition support and alternative-career pathways change occupational identity and exit rapidly, suppression is primarily structural.',
    'If suppression is primarily structural, the constraint''s effective suppression is accurately measured at 0.72. If suppression is substantially internalized, the actual constraint is higher than 0.72 because workers carry suppression with them after exit. This affects whether coal workers remain victims of the constraint after leaving coal-dependent regions.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coal_worker_transition_suppression_mechanism, empirical, 'Whether coal-worker suppression is structural (economic barriers) or internalized (fused identity, isolated information).').

omega_variable(
    reading_foreclosure_vs_coexistence,
    'Does the systemic transformation reading logically foreclose the facility-constraint reading (they cannot coexist in a single consistent statutory interpretation), or do they coexist as competing readings held by different institutional actors (EPA vs. courts, different administrations, different stakeholders)?',
    'If a Supreme Court ruling definitively selects one reading and declares the other inconsistent with the statute, foreclosure is resolved. If competing readings persist across administrations, different courts, and legislative ambiguity, they coexist. The presence of West Virginia v. EPA''s major question doctrine concern (which did not resolve the facility-vs-systemic question but limited EPA''s authority generally) suggests coexistence rather than foreclosure so far.',
    'Foreclosure would mean the constraint''s entire identity depends on which reading prevails; coexistence means both readings remain live positions with different institutional homes, creating bifurcated regulatory landscape and persistent legal uncertainty.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_foreclosure_vs_coexistence, conceptual, 'Whether the facility-constraint reading is logically incompatible with this systemic-transformation reading or whether both can be held simultaneously by different institutional actors.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(caa_section_111d_delegation__systemic_transformation_reading, 0, 35).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(caa__tr_t0, caa_section_111d_delegation__systemic_transformation_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(caa__tr_t5, caa_section_111d_delegation__systemic_transformation_reading, theater_ratio, 5, 0.28).
narrative_ontology:measurement(caa__tr_t10, caa_section_111d_delegation__systemic_transformation_reading, theater_ratio, 10, 0.32).
narrative_ontology:measurement(caa__tr_t15, caa_section_111d_delegation__systemic_transformation_reading, theater_ratio, 15, 0.37).
narrative_ontology:measurement(caa__tr_t20, caa_section_111d_delegation__systemic_transformation_reading, theater_ratio, 20, 0.4).
narrative_ontology:measurement(caa__tr_t25, caa_section_111d_delegation__systemic_transformation_reading, theater_ratio, 25, 0.41).
narrative_ontology:measurement(caa__tr_t30, caa_section_111d_delegation__systemic_transformation_reading, theater_ratio, 30, 0.41).
narrative_ontology:measurement(caa__tr_t35, caa_section_111d_delegation__systemic_transformation_reading, theater_ratio, 35, 0.41).

% Extraction over time
narrative_ontology:measurement(caa__be_t0, caa_section_111d_delegation__systemic_transformation_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(caa__be_t5, caa_section_111d_delegation__systemic_transformation_reading, base_extractiveness, 5, 0.52).
narrative_ontology:measurement(caa__be_t10, caa_section_111d_delegation__systemic_transformation_reading, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(caa__be_t15, caa_section_111d_delegation__systemic_transformation_reading, base_extractiveness, 15, 0.63).
narrative_ontology:measurement(caa__be_t20, caa_section_111d_delegation__systemic_transformation_reading, base_extractiveness, 20, 0.66).
narrative_ontology:measurement(caa__be_t25, caa_section_111d_delegation__systemic_transformation_reading, base_extractiveness, 25, 0.67).
narrative_ontology:measurement(caa__be_t30, caa_section_111d_delegation__systemic_transformation_reading, base_extractiveness, 30, 0.68).
narrative_ontology:measurement(caa__be_t35, caa_section_111d_delegation__systemic_transformation_reading, base_extractiveness, 35, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(caa__su_t0, caa_section_111d_delegation__systemic_transformation_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(caa__su_t5, caa_section_111d_delegation__systemic_transformation_reading, suppression_requirement, 5, 0.6).
narrative_ontology:measurement(caa__su_t10, caa_section_111d_delegation__systemic_transformation_reading, suppression_requirement, 10, 0.65).
narrative_ontology:measurement(caa__su_t15, caa_section_111d_delegation__systemic_transformation_reading, suppression_requirement, 15, 0.69).
narrative_ontology:measurement(caa__su_t20, caa_section_111d_delegation__systemic_transformation_reading, suppression_requirement, 20, 0.71).
narrative_ontology:measurement(caa__su_t25, caa_section_111d_delegation__systemic_transformation_reading, suppression_requirement, 25, 0.72).
narrative_ontology:measurement(caa__su_t30, caa_section_111d_delegation__systemic_transformation_reading, suppression_requirement, 30, 0.72).
narrative_ontology:measurement(caa__su_t35, caa_section_111d_delegation__systemic_transformation_reading, suppression_requirement, 35, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(caa_section_111d_delegation__systemic_transformation_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(caa_section_111d_delegation__systemic_transformation_reading, 0.18).
narrative_ontology:affects_constraint(caa_section_111d_delegation__systemic_transformation_reading, caa_section_111d_delegation__facility_constraint_reading).
narrative_ontology:affects_constraint(caa_section_111d_delegation__systemic_transformation_reading, renewable_energy_subsidy_via_regulatory_mandate).
narrative_ontology:affects_constraint(caa_section_111d_delegation__systemic_transformation_reading, coal_plant_stranded_assets_regulatory_mechanism).

% DUAL FORMULATION NOTE:
% The caa_section_111d_delegation kernel decomposes into two structurally distinct constraints via the ε-invariance principle. The systemic_transformation_reading (this story) authorizes EPA to mandate grid-level decarbonization (high extraction, 0.68 ε, tangled_rope); the facility_constraint_reading limits EPA to facility-level measures (low extraction, estimated ~0.15 ε, rope). The ε values differ by a wide margin because the scope of EPA authority determines whether fossil generators bear reallocation costs (systemic) or only efficiency costs (facility). These are not observer-relative variations—they are structurally distinct constraints with different victim sets and different authority structures. Both readings are live in current law; the major question doctrine concern in West Virginia v. EPA (142 S.Ct. 2587, 2022) did not resolve the facility-vs-systemic boundary, leaving the kernel contest unresolved in appellate law.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(caa_section_111d_delegation__systemic_transformation_reading, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
