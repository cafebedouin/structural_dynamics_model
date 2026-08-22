% ============================================================================
% CONSTRAINT STORY: caa_section_111d_delegation__systemic_transformation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:measurement_basis/2,
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
 *   human_readable: Clean Air Act Section 111(d) Systemic Transformation Reading: Grid-Wide Generation Shifting
 *   domain: administrative_law/environmental_regulation/constitutional_interpretation
 *
 * SUMMARY:
 *   Under the systemic-transformation reading, Section 111(d)'s 'best system'
 *   authorizes EPA to mandate state-level decarbonization achieved through
 *   generation shifting (renewable substitution, coal retirement), not merely
 *   facility-level efficiency improvements. This reading interprets broad EPA
 *   regulatory authority to redesign grid-wide generation portfolios.
 *   Coal-dependent regions and fossil fuel operators bear stranded-asset and
 *   compliance costs; renewable developers and states with renewable
 *   resources benefit from federally-protected market capture. The constraint
 *   operates as tangled rope: genuine coordination function (grid-wide
 *   decarbonization coordination across state and market boundaries) AND
 *   asymmetric extraction (costs concentrated on coal regions, benefits
 *   concentrated on renewable capital). This is a one-reading constraint: the
 *   facility-constraint-reading is a separate constraint
 *   (constraint_caa_section_111d_delegation__facility_constraint_reading)
 *   with different ε and different victim/beneficiary structure. Both
 *   readings contest the same kernel (what 'best system' authorizes); they
 *   are structurally distinct constraints and instantiate different authority
 *   relationships.
 *
 * KEY AGENTS:
 *   - EPA regulatory authority: sets and enforces the systemic-transformation interpretation; agenda-setter institutional power
 *   - coal-dependent regions: bear compliance burden, stranded assets, workforce transition costs; trapped exit
 *   - fossil fuel industry: faces accelerated asset stranding, foreclosed return timelines; constrained exit
 *   - renewable energy developers: gain regulatory demand signals, protected market, no geographic lock; mobile power
 *   - states with renewable resources: benefit from alignment with mandate, protected deployment markets; organized power
 *   - facility-constraint reading proponents: structurally excluded from EPA's decision-making once systemic reading is adopted; institutional power but no seat in implementation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(caa_section_111d_delegation__systemic_transformation_reading, 0.68).
domain_priors:suppression_score(caa_section_111d_delegation__systemic_transformation_reading, 0.62).
domain_priors:theater_ratio(caa_section_111d_delegation__systemic_transformation_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(caa_section_111d_delegation__systemic_transformation_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(caa_section_111d_delegation__systemic_transformation_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(caa_section_111d_delegation__systemic_transformation_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(caa_section_111d_delegation__systemic_transformation_reading, accessibility_collapse, 0.71).
narrative_ontology:constraint_metric(caa_section_111d_delegation__systemic_transformation_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(caa_section_111d_delegation__systemic_transformation_reading, tangled_rope).
narrative_ontology:human_readable(caa_section_111d_delegation__systemic_transformation_reading, "Clean Air Act Section 111(d) Systemic Transformation Reading: Grid-Wide Generation Shifting").
narrative_ontology:topic_domain(caa_section_111d_delegation__systemic_transformation_reading, "administrative_law/environmental_regulation/constitutional_interpretation").

domain_priors:requires_active_enforcement(caa_section_111d_delegation__systemic_transformation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(caa_section_111d_delegation__systemic_transformation_reading, '67789d91-0cd9-423f-a390-130ebfd42e86').
narrative_ontology:cs_kernel_codification('67789d91-0cd9-423f-a390-130ebfd42e86', fixed_text).
narrative_ontology:cs_authority_grounding('67789d91-0cd9-423f-a390-130ebfd42e86', extraction).
narrative_ontology:cs_interpretation_layer_present('67789d91-0cd9-423f-a390-130ebfd42e86').
narrative_ontology:cs_reading_relation('67789d91-0cd9-423f-a390-130ebfd42e86', caa_section_111d_delegation__facility_constraint_reading, forecloses).
narrative_ontology:cs_axiom('67789d91-0cd9-423f-a390-130ebfd42e86', foundational, epa_authority_generation_portfolio_design).
narrative_ontology:cs_axiom_status(epa_authority_generation_portfolio_design, holdable).
narrative_ontology:cs_axiom_grounding('67789d91-0cd9-423f-a390-130ebfd42e86', epa_authority_generation_portfolio_design, empirically_contingent).
narrative_ontology:cs_axiom('67789d91-0cd9-423f-a390-130ebfd42e86', foundational, generation_shifting_necessary_for_climate_compliance).
narrative_ontology:cs_axiom_status(generation_shifting_necessary_for_climate_compliance, holdable).
narrative_ontology:cs_axiom_grounding('67789d91-0cd9-423f-a390-130ebfd42e86', generation_shifting_necessary_for_climate_compliance, empirically_contingent).
narrative_ontology:cs_reference_frame('67789d91-0cd9-423f-a390-130ebfd42e86', epa_broad_section_111d_authority).
narrative_ontology:cs_drift_state('67789d91-0cd9-423f-a390-130ebfd42e86', post_loper_light_deference_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('67789d91-0cd9-423f-a390-130ebfd42e86', '').
narrative_ontology:cs_kernel_id(caa_section_111d_delegation__systemic_transformation_reading, caa_section_111d_delegation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(caa_section_111d_delegation__systemic_transformation_reading, renewable_energy_developers).
narrative_ontology:constraint_beneficiary(caa_section_111d_delegation__systemic_transformation_reading, states_with_renewable_resources).
narrative_ontology:constraint_beneficiary(caa_section_111d_delegation__systemic_transformation_reading, environmental_advocacy_constituencies).
narrative_ontology:constraint_victim(caa_section_111d_delegation__systemic_transformation_reading, coal_dependent_regions).
narrative_ontology:constraint_victim(caa_section_111d_delegation__systemic_transformation_reading, fossil_fuel_industry).
narrative_ontology:constraint_victim(caa_section_111d_delegation__systemic_transformation_reading, states_locked_to_coal_generation).
narrative_ontology:constraint_vindicates(caa_section_111d_delegation__systemic_transformation_reading, broad_epa_regulatory_authority).
narrative_ontology:constraint_vindicates(caa_section_111d_delegation__systemic_transformation_reading, systemic_environmental_crisis_response).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets and enforces Section 111(d) as authorizing EPA to mandate state emission performance standards achievable through grid-wide generation shifting. Sets the regulatory framework, designs the compliance pathway, and adjudicates state implementation plans. Under this reading, EPA authority extends to prescribing technology deployment and grid composition, not merely facility-level improvements.
narrative_ontology:constraint_stakeholder(caa_section_111d_delegation__systemic_transformation_reading, epa_regulatory_authority, agenda_setter,
    institutional, generational, analytical, national).

% Face accelerated retirement mandates for existing coal plants and cannot replicate generation capacity through equivalent coal investments. Their workforce, tax base, and grid reliability strategies are locked into coal infrastructure. Exit costs include stranded assets, community economic collapse, and decade-scale workforce transition that exceeds local adaptive capacity. Regulatory compliance requires replacing coal with renewables they may not have geographic advantage in developing.
narrative_ontology:constraint_stakeholder(caa_section_111d_delegation__systemic_transformation_reading, coal_dependent_regions, payer,
    moderate, generational, trapped, regional).

% Faces accelerated stranding of coal generation assets, foreclosure of return timelines on recent investments, and regulatory pathways that redirect capital flows toward renewables. Their exit from the coal market is forced (early retirement mandates); their transition to renewable deployment requires surrendering market position to competitors already embedded in that sector. Financial models built on 30-40 year coal plant lifecycles become non-viable.
narrative_ontology:constraint_stakeholder(caa_section_111d_delegation__systemic_transformation_reading, fossil_fuel_industry, payer,
    powerful, biographical, constrained, global).

% Gain regulatory demand signals guaranteeing market for renewable generation through EPA-mandated state compliance pathways. Receive de facto subsidization through regulatory prohibition of coal competitors rather than direct capital subsidy. Can deploy across multiple states; not geographically locked. Their entry and scaling are enabled by the constraint; their competitive position is protected by the regulatory suppression of coal alternatives.
narrative_ontology:constraint_stakeholder(caa_section_111d_delegation__systemic_transformation_reading, renewable_energy_developers, beneficiary,
    powerful, generational, mobile, global).

% Must implement EPA-mandated decarbonization pathways despite lacking geographic renewable resources comparable to their coal endowments. Their compliance cost is not marginal efficiency improvement but wholesale energy infrastructure replacement. They cannot exit the constraint (EPA authority applies nationwide); they can only absorb the compliance burden or litigate the regulatory authority. Their state-level policy autonomy is substantially constrained by the federal mandate.
narrative_ontology:constraint_stakeholder(caa_section_111d_delegation__systemic_transformation_reading, states_locked_to_coal_generation, payer,
    organized, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(caa_section_111d_delegation__systemic_transformation_reading, states_locked_to_coal_generation, excluded).

% Have natural endowments (wind, solar irradiance) that align with EPA-mandated generation shifts. Their compliance burden is lower; their renewable deployment becomes federally-guaranteed market capture. They benefit from regulatory demand signals that would not exist under purely market-driven energy transitions. Their advantage is geographically contingent but structurally protected by the mandate.
narrative_ontology:constraint_stakeholder(caa_section_111d_delegation__systemic_transformation_reading, states_with_renewable_resources, beneficiary,
    organized, generational, mobile, regional).

% Benefit from regulatory architecture that decarbonizes grid without requiring political consensus on climate policy or carbon pricing. The constraint vindicates their policy goals through judicial/administrative action rather than legislative process. Their exit is purely analytical (they can shift focus); they do not bear compliance costs. They benefit from the suppression of coal alternatives.
narrative_ontology:constraint_stakeholder(caa_section_111d_delegation__systemic_transformation_reading, environmental_advocacy_constituencies, beneficiary,
    organized, generational, mobile, global).

% Argue the Section 111(d) 'best system' is limited to facility-level improvements and cannot authorize grid-wide generation shifting. They are excluded from the decision-making process under this reading's regulatory framework; their interpretive authority is overridden by EPA's determination. Their access to the constraint's administrative machinery is blocked by the prior interpretive commitment to systemic transformation authority.
narrative_ontology:constraint_stakeholder(caa_section_111d_delegation__systemic_transformation_reading, facility_constraint_reading_proponents, excluded,
    institutional, generational, analytical, national).

% Adjudicate the constitutionality of EPA's Section 111(d) interpretation under Chevron (pre-Loper Bright) or de novo review (post-Loper Bright). They see testimony from all stakeholders, economic analysis of transition costs, and constitutional/administrative law arguments. Their verdict determines whether the constraint persists with full force, is narrowed, or is struck down.
narrative_ontology:constraint_stakeholder(caa_section_111d_delegation__systemic_transformation_reading, courts_of_appellate_jurisdiction, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(caa_section_111d_delegation__systemic_transformation_reading, renewable_energy_developers).
narrative_ontology:fixing_cost_class(caa_section_111d_delegation__systemic_transformation_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a federal performance standard for grid-wide emission reductions that coordinates state implementation across multiple energy markets, preventing regulatory arbitrage (states competitive-disadvantaging each other through lax coal regulation) and creating unified decarbonization pathways. Solves the collective-action problem of cross-border pollution and competitive dynamics in regional power markets.
% TRANSFER_FUNCTION: Transfers compliance burden (and stranded asset costs) from federal environmental constituency to coal-dependent regions and fossil fuel operators. Transfers regulatory-protected market capture from fossil fuel generation to renewable energy developers. Transfers policy autonomy from states to EPA. The underlying transfer is from concentrated, geographically-locked energy producers to dispersed environmental beneficiaries and mobile renewable capital.
% ABSENT_VOICES: Fossil fuel workers in coal communities are not direct decision-makers in regulatory design and have limited seat in EPA administrative process; they would argue for transition time and worker protection not built into the constraint. Facility-constraint-reading proponents (industry legal authorities, originalist statutory interpreters) are structurally excluded from EPA's decision machinery once the systemic-transformation interpretation is adopted; they remain present in court but not in implementation.
% DISAPPEARANCE_RATIONALE: If Section 111(d) were reinterpreted to permit only facility-level measures (early retirement prohibited, generation shifting prohibited), or if the constraint were struck down, coal generation would persist for decades longer, renewable deployment would slow without regulatory demand signals, and fossil fuel stranded-asset losses would reverse. Regional energy markets, state decarbonization timelines, and energy-dependent workforce transitions would reorganize around coal-extended lifecycles.
% FOUNDING_PROBLEM: Atmospheric CO2 concentration and grid-wide emission trajectories required rapid decarbonization not achievable through facility-level efficiency improvements alone; the constraint was built to solve the problem of generation-level decarbonization at the scale required to meet climate commitments and air-quality standards.
% FOUNDING_PROBLEM_CORROBORATION: EPA and environmental science communities attest the founding problem is live and generation-level shifts are necessary. Fossil fuel industry and coal-region state governments attest the founding problem can be solved through facility improvements and market-driven transition without mandated generation shifting, or that the problem requires legislative action (carbon pricing) rather than regulatory interpretation. Independent climate science confirms rapid decarbonization is necessary; contested is whether generation-shifting mandates are the legitimate tool.
narrative_ontology:disappearance_verdict(caa_section_111d_delegation__systemic_transformation_reading, world_rearranges).
narrative_ontology:founding_problem_status(caa_section_111d_delegation__systemic_transformation_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(caa_section_111d_delegation__systemic_transformation_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
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
 *   Extractiveness is high (0.68) and rising over the interval because the regulatory framework creates increasingly visible wealth transfers: renewable developers capture market rents through regulatory prohibition of coal competition; coal regions bear visible stranded-asset losses. Early measurements (t=0-10) show extractiveness rising as the constraint is applied (EPA implementation begins), then plateau (t=20-30) as coal retirements become accepted fact and the transfer mechanisms stabilize. Suppression is high (0.62) because the constraint's persistence depends on active enforcement: EPA must continuously defend the systemic-transformation interpretation against litigation and narrow alternative interpretations, must monitor state implementation plans, must prohibit state regulatory arbitrage. Suppression would fall if the facility-constraint reading prevailed or if courts struck the interpretation down. Theater is moderate-low (0.28) because the coordination function is genuine (grid-level decarbonization requires coordination) but growing share of enforcement machinery (t=5-15 rise) defends the anti-coal exclusivity rather than the coordination goal itself. By t=25-30, theater plateaus as the coal transition becomes normalized and enforcement focuses on ensuring renewable deployment rather than defending coal exclusion — the theatrical element stabilizes.
 *
 * PERSPECTIVAL GAP:
 *   From EPA's analytical seat, the constraint is genuine coordination: it solves a collective-action problem (cross-border pollution, state competitive dynamics, generation-level emissions requiring coordinated pathway). From coal-region seats, the same structure operates as enforced extraction: compliance is imposed without bargaining, exit is closed, the coordination benefit is diffuse (lower national emissions) while extraction is concentrated (lost jobs, stranded assets, regional decline). From renewable-developer seats, it is straightforward subsidy masked as coordination (regulatory prohibition of competitors creates artificial scarcity and protected returns). The engine computes these divergences from per-seat directionality; the perspectival gap is the structural asymmetry the metrics aim to capture.
 *
 * DIRECTIONALITY LOGIC:
 *   Coal-dependent regions and fossil fuel operators compute high d (near 1.0, full targets) because they bear concentrated, unavoidable compliance costs and face suppressed alternatives (coal is prohibited, not merely disadvantaged). They cannot arbitrage or exit; they are trapped and identity-locked (state grid reliability, regional energy independence narratives). Renewable developers compute low d (near 0.2, beneficiaries) because they benefit directly from regulatory demand signals and face no suppression — their exit is purely mobile, purely analytical. States with renewable resources compute d near 0.3 (beneficiaries with modest coordination burden) because they benefit from alignment but bear some implementation cost. EPA computes analytically (d=0.5) because it is the agenda-setter but not a direct beneficiary or victim of the constraint's operation — it bears political/legal risk but not financial extraction. The engine derives this per-seat computation from the structural data declared here; the authored claim of tangled rope reflects the aggregate structure.
 *
 * MANDATROPHY ANALYSIS:
 *   The claim is tangled rope, not snare, because the coordination function is genuine: grid-wide decarbonization at required speed cannot be achieved through market prices alone, and states have incentive to free-ride on federal climate commitment. The mandate solves that coordination problem. Snare classification would require the coordination story to be cover for pure extraction; instead, the extraction (coal stranding, fossil-fuel-company foreclosure) is a structural byproduct of solving a real coordination problem, not the reason the constraint exists. The constraint could be mandatrophy-resolved (the founding problem solved, constraint persists) if: (1) grid-wide decarbonization pathways were no longer necessary (climate targets achieved, energy transition complete), and (2) the constraint persisted despite the founding problem being satisfied. Current status: founding problem is live, mandatrophy is not yet triggered. If coal retirements occur as mandated and grid transitions by t=30-35, and EPA continues to apply generation-shifting mandates post-transition, mandatrophy would be declared.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    systemic_vs_facility_authority_boundary,
    'Does Section 111(d)''s ''best system of emission reduction'' authorize EPA to prescribe grid-wide generation-level strategies, or only facility-level technology improvements?',
    'Supreme Court judicial review (post-Loper Bright, de novo interpretation of statutory language and authority boundaries; pre-Loper Bright, Chevron deference to EPA reading). Direct evidence: statutory text, legislative history, prior EPA interpretations, comparable regulatory authorities in Clean Air Act sections.',
    'If facility-constraint reading prevails: constraint dissolves or substantially narrows (coal retirement mandates prohibited, generation shifting prohibited). Extractiveness drops by 0.35-0.45. Fossil fuel stranded-asset losses reverse. If systemic-transformation reading upheld: constraint persists with current force, extractiveness stabilizes at current level (0.68). This is the primary existential question for the constraint.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(systemic_vs_facility_authority_boundary, conceptual, 'Whether EPA''s authority extends to generation-level mandate or is limited to facility-level improvements.').

omega_variable(
    coal_region_adaptive_capacity,
    'Can coal-dependent regions complete workforce transition and economic diversification within the timeframe required by the mandate, or does the constraint''s exit cost exceed regional adaptive capacity?',
    'Post-mandate economic data: regional employment transitions, new-sector job creation, infrastructure investment success, population migration patterns, community resilience metrics. Regional GDP and tax-base trajectories.',
    'If regions adapt successfully: the suppression mechanism softens (exit becomes ''constrained'' rather than ''trapped'' in later periods); mandatrophy may eventually resolve as the constraint transitions from extraction to mature coordination. If regions fail to adapt: suppression hardens (becomes internalized in workforce behavior, regional identity); constraint operates as snare-like extraction rather than temporary tangled rope; political pressure for regulatory reversal intensifies.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coal_region_adaptive_capacity, empirical, 'Whether coal-region adaptive capacity permits transition completion within regulatory timelines.').

omega_variable(
    renewable_deployment_cost_trajectory,
    'Do renewable deployment costs decline sufficiently to make the mandate''s compliance achievable without subsidy beyond regulatory prohibition of coal?',
    'Engineering cost tracking: solar/wind capacity buildout costs, grid integration costs, transmission network upgrades, storage costs. Comparison of observed deployment costs against EPA compliance-cost estimates.',
    'If costs decline as projected: the mandate becomes achievable without collateral extraction; tangled rope softens toward rope (coordination cost is material but not asymmetric). If costs stagnate: renewable developers require direct subsidy in addition to coal prohibition; extraction becomes more visible (transfer is coal stranding + subsidy requirement); constraint may reclassify toward snare.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(renewable_deployment_cost_trajectory, empirical, 'Whether renewable deployment cost trajectory supports compliance without direct subsidy.').

omega_variable(
    facility_constraint_reading_legal_viability,
    'Is the facility-constraint reading genuinely a live alternative in contemporary legal doctrine, or has it been foreclosed by prior holdings and current court composition?',
    'Judicial signals (circuit court decisions pre-Supreme Court cert, academic commentary, EPA internal memo discussion), statutory analysis by generalist administrative law scholars outside energy practice, comparative analysis of Chevron-era vs. post-Loper Light jurisprudence on EPA authority scope.',
    'If facility-constraint reading is foreclosed: systemic-transformation reading is not genuinely contested; the constraint operates with higher legitimacy and lower active suppression (not defending against a live alternative). If facility-constraint reading remains live: litigation pressure persists, suppression_requirement stabilizes or rises as EPA continuously defends the interpretation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(facility_constraint_reading_legal_viability, conceptual, 'Whether the facility-constraint reading remains a live legal alternative or has been foreclosed by doctrine.').

omega_variable(
    inter_reading_foreclosure_mechanism,
    'Do the systemic-transformation and facility-constraint readings genuinely foreclose each other (logically incompatible core premises), or do they coexist as different judicial coalitions'' interpretations (not logically contradictory, but institutionally exclusive)?',
    'Detailed statutory reconstruction: what logical proposition must be true for each reading to hold? If the propositions contradict at the root level, genuine foreclosure exists. If they differ only on policy judgment or risk tolerance, they coexist (different authority factions hold them).',
    'If genuine foreclosure: only one reading can survive long-term; the constraint space will eventually collapse to one constraint per reading (not both active). If coexistence: both readings persist in different institutional seats (EPA holds systemic, some state AGs hold facility); the constraint family remains bipartitioned and unstable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(inter_reading_foreclosure_mechanism, conceptual, 'Whether the two readings logically foreclose each other or merely represent different institutional factions'' positions.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(caa_section_111d_delegation__systemic_transformation_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(caa__tr_t0, caa_section_111d_delegation__systemic_transformation_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement_basis(caa__tr_t0, observed).
narrative_ontology:measurement(caa__tr_t5, caa_section_111d_delegation__systemic_transformation_reading, theater_ratio, 5, 0.16).
narrative_ontology:measurement_basis(caa__tr_t5, observed).
narrative_ontology:measurement(caa__tr_t10, caa_section_111d_delegation__systemic_transformation_reading, theater_ratio, 10, 0.21).
narrative_ontology:measurement_basis(caa__tr_t10, observed).
narrative_ontology:measurement(caa__tr_t15, caa_section_111d_delegation__systemic_transformation_reading, theater_ratio, 15, 0.25).
narrative_ontology:measurement_basis(caa__tr_t15, observed).
narrative_ontology:measurement(caa__tr_t20, caa_section_111d_delegation__systemic_transformation_reading, theater_ratio, 20, 0.27).
narrative_ontology:measurement_basis(caa__tr_t20, observed).
narrative_ontology:measurement(caa__tr_t25, caa_section_111d_delegation__systemic_transformation_reading, theater_ratio, 25, 0.28).
narrative_ontology:measurement_basis(caa__tr_t25, projected).
narrative_ontology:measurement(caa__tr_t30, caa_section_111d_delegation__systemic_transformation_reading, theater_ratio, 30, 0.28).
narrative_ontology:measurement_basis(caa__tr_t30, projected).

% Extraction over time
narrative_ontology:measurement(caa__be_t0, caa_section_111d_delegation__systemic_transformation_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement_basis(caa__be_t0, observed).
narrative_ontology:measurement(caa__be_t5, caa_section_111d_delegation__systemic_transformation_reading, base_extractiveness, 5, 0.54).
narrative_ontology:measurement_basis(caa__be_t5, observed).
narrative_ontology:measurement(caa__be_t10, caa_section_111d_delegation__systemic_transformation_reading, base_extractiveness, 10, 0.59).
narrative_ontology:measurement_basis(caa__be_t10, observed).
narrative_ontology:measurement(caa__be_t15, caa_section_111d_delegation__systemic_transformation_reading, base_extractiveness, 15, 0.64).
narrative_ontology:measurement_basis(caa__be_t15, observed).
narrative_ontology:measurement(caa__be_t20, caa_section_111d_delegation__systemic_transformation_reading, base_extractiveness, 20, 0.66).
narrative_ontology:measurement_basis(caa__be_t20, observed).
narrative_ontology:measurement(caa__be_t25, caa_section_111d_delegation__systemic_transformation_reading, base_extractiveness, 25, 0.67).
narrative_ontology:measurement_basis(caa__be_t25, projected).
narrative_ontology:measurement(caa__be_t30, caa_section_111d_delegation__systemic_transformation_reading, base_extractiveness, 30, 0.68).
narrative_ontology:measurement_basis(caa__be_t30, projected).

% Suppression requirement over time
narrative_ontology:measurement(caa__su_t0, caa_section_111d_delegation__systemic_transformation_reading, suppression_requirement, 0, 0.44).
narrative_ontology:measurement_basis(caa__su_t0, observed).
narrative_ontology:measurement(caa__su_t5, caa_section_111d_delegation__systemic_transformation_reading, suppression_requirement, 5, 0.5).
narrative_ontology:measurement_basis(caa__su_t5, observed).
narrative_ontology:measurement(caa__su_t10, caa_section_111d_delegation__systemic_transformation_reading, suppression_requirement, 10, 0.56).
narrative_ontology:measurement_basis(caa__su_t10, observed).
narrative_ontology:measurement(caa__su_t15, caa_section_111d_delegation__systemic_transformation_reading, suppression_requirement, 15, 0.6).
narrative_ontology:measurement_basis(caa__su_t15, observed).
narrative_ontology:measurement(caa__su_t20, caa_section_111d_delegation__systemic_transformation_reading, suppression_requirement, 20, 0.61).
narrative_ontology:measurement_basis(caa__su_t20, observed).
narrative_ontology:measurement(caa__su_t25, caa_section_111d_delegation__systemic_transformation_reading, suppression_requirement, 25, 0.62).
narrative_ontology:measurement_basis(caa__su_t25, projected).
narrative_ontology:measurement(caa__su_t30, caa_section_111d_delegation__systemic_transformation_reading, suppression_requirement, 30, 0.62).
narrative_ontology:measurement_basis(caa__su_t30, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(caa_section_111d_delegation__systemic_transformation_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(caa_section_111d_delegation__systemic_transformation_reading, 0.18).
narrative_ontology:affects_constraint(caa_section_111d_delegation__systemic_transformation_reading, caa_section_111d_delegation__facility_constraint_reading).
narrative_ontology:affects_constraint(caa_section_111d_delegation__systemic_transformation_reading, fossil_fuel_stranded_asset_regime).
narrative_ontology:affects_constraint(caa_section_111d_delegation__systemic_transformation_reading, state_renewable_energy_procurement).

% DUAL FORMULATION NOTE:
% Section 111(d) 'best system' constraint family contains two structurally distinct constraints corresponding to two interpretive readings: systemic_transformation_reading (this file) and facility_constraint_reading. The readings contest what EPA authority encompasses — generation-shifting mandates vs. facility-level improvements. This reading authorizes grid-wide decarbonization pathways via generation shifting; the sibling reading constrains authority to facility efficiency. These are not alternative measurements of one constraint — their ε values, victim/beneficiary structures, and type classifications diverge fundamentally. The readings coexist in ongoing litigation. One will eventually prevail (or be split into narrowed holdings). Both are active in the constraint corpus pending judicial resolution. Family links: systemic_transformation→affects→facility_constraint (upstream to downstream, more established reading to more contested); both→affect→state_renewable_energy_procurement (down-stream consequence for state-level deployment).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(caa_section_111d_delegation__systemic_transformation_reading, organized, 0.32).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
