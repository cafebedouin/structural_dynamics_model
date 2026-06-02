% ============================================================================
% CONSTRAINT STORY: caa_section_111d_delegation__state_compliance_pathway_constraint
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_caa_section_111d_state_compliance_pathway, []).

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
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:cs_kernel_id/2,
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
 *   constraint_id: caa_section_111d_delegation__state_compliance_pathway_constraint
 *   human_readable: CAA Section 111(d) State Compliance Pathway Constraint
 *   domain: environmental_regulation/constitutional_law/administrative_delegation
 *
 * SUMMARY:
 *   Section 111(d) of the Clean Air Act authorizes the EPA to establish
 *   performance standards for greenhouse gas emissions from existing
 *   coal-fired and natural gas power plants. The statute permits states to
 *   develop and submit compliance pathways that allow regulated entities to
 *   meet the federal standards through alternative mechanisms — energy
 *   efficiency improvements, renewable energy deployment, demand-side
 *   management — rather than direct emission reductions. This delegation of
 *   compliance pathway design to states creates a structural tension: the
 *   federal government retains the authority to set stringency (the emission
 *   reduction target) but delegates discretion over the means (compliance
 *   pathways) to state regulators who face different political coalitions,
 *   geographic constraints, and economic dependencies. This constraint
 *   examines the state-level compliance pathway authority as a distinct
 *   institutional arrangement that exhibits extraction (states gain
 *   regulatory discretion; EPA loses implementation authority; regulated
 *   entities gain flexibility to influence state-level standards),
 *   coordination (subsidiarity assigns standards-setting to jurisdictions
 *   with local knowledge; diverse pathways accommodate regional energy
 *   infrastructure), and suppression (political capture risk, monitoring
 *   burden, variation across states). The constraint is constitutionally
 *   grounded in the delegated authority text of the statute and legitimacy
 *   claims rooted in federalism doctrine.
 *
 * KEY AGENTS:
 *   - State Environmental Agencies: Primary beneficiary (institutional/arbitrage) — gain delegated authority, resource control, and regulatory discretion over compliance pathway design
 *   - EPA (Central Authority): Primary victim (institutional/constrained) — retains standard-setting authority but loses implementation discretion; must monitor state compliance and manage capture risk
 *   - Fossil Fuel Regulated Entities: Secondary beneficiary (powerful/mobile) — benefit from state-level flexibility and can employ regulatory arbitrage; also face coordination burden of state-by-state variation
 *   - Public Health Communities: Secondary victim (powerless/trapped) — dependent on state-level compliance outcomes with no direct federal recourse if states set weak standards
 *   - Environmental Advocacy Coalition: Organized actors (organized/constrained) — challenging the delegation through litigation and legislative pressure; building exit pathways
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing federalism as constitutional law rather than recognizing it as a contestable institutional arrangement
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(caa_section_111d_delegation__state_compliance_pathway_constraint, 0.52).
domain_priors:suppression_score(caa_section_111d_delegation__state_compliance_pathway_constraint, 0.48).
domain_priors:theater_ratio(caa_section_111d_delegation__state_compliance_pathway_constraint, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(caa_section_111d_delegation__state_compliance_pathway_constraint, extractiveness, 0.52).
narrative_ontology:constraint_metric(caa_section_111d_delegation__state_compliance_pathway_constraint, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(caa_section_111d_delegation__state_compliance_pathway_constraint, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(caa_section_111d_delegation__state_compliance_pathway_constraint, tangled_rope).
narrative_ontology:human_readable(caa_section_111d_delegation__state_compliance_pathway_constraint, "CAA Section 111(d) State Compliance Pathway Constraint").
narrative_ontology:topic_domain(caa_section_111d_delegation__state_compliance_pathway_constraint, "environmental_regulation/constitutional_law/administrative_delegation").

domain_priors:requires_active_enforcement(caa_section_111d_delegation__state_compliance_pathway_constraint).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(caa_section_111d_delegation__state_compliance_pathway_constraint, 'c67bba74-538f-457a-8179-e34c735388a5').
narrative_ontology:cs_kernel_codification('c67bba74-538f-457a-8179-e34c735388a5', formalized).
narrative_ontology:cs_authority_grounding('c67bba74-538f-457a-8179-e34c735388a5', extraction).
narrative_ontology:cs_interpretation_layer_present('c67bba74-538f-457a-8179-e34c735388a5').
narrative_ontology:cs_axiom('c67bba74-538f-457a-8179-e34c735388a5', foundational, state_discretion_enables_genuine_adaptation).
narrative_ontology:cs_axiom_status(state_discretion_enables_genuine_adaptation, holdable).
narrative_ontology:cs_axiom_grounding('c67bba74-538f-457a-8179-e34c735388a5', state_discretion_enables_genuine_adaptation, empirically_contingent).
narrative_ontology:cs_axiom('c67bba74-538f-457a-8179-e34c735388a5', foundational, regulatory_capture_at_state_level_is_probable).
narrative_ontology:cs_axiom_status(regulatory_capture_at_state_level_is_probable, holdable).
narrative_ontology:cs_axiom_grounding('c67bba74-538f-457a-8179-e34c735388a5', regulatory_capture_at_state_level_is_probable, empirically_contingent).
narrative_ontology:cs_reference_frame('c67bba74-538f-457a-8179-e34c735388a5', federal_standard_setting_delegated_to_states).
narrative_ontology:cs_drift_state('c67bba74-538f-457a-8179-e34c735388a5', contemporary, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('c67bba74-538f-457a-8179-e34c735388a5', '2026-02-26T14:32:00Z').
narrative_ontology:cs_kernel_id(caa_section_111d_delegation__state_compliance_pathway_constraint, caa_section_111d_delegation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(caa_section_111d_delegation__state_compliance_pathway_constraint, state_environmental_agencies).
narrative_ontology:constraint_beneficiary(caa_section_111d_delegation__state_compliance_pathway_constraint, fossil_fuel_regulated_entities).
narrative_ontology:constraint_victim(caa_section_111d_delegation__state_compliance_pathway_constraint, federal_epa_authority).
narrative_ontology:constraint_victim(caa_section_111d_delegation__state_compliance_pathway_constraint, public_health_communities).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PUBLIC HEALTH COMMUNITIES (SNARE) — Powerless to exit the state compliance framework once delegated. Trapped by the outcome of the regulatory delegation: if states capture the process, public health costs accumulate with no recourse at the federal level. Experiences the constraint as pure extraction masked by federalism rhetoric.
constraint_indexing:constraint_classification(caa_section_111d_delegation__state_compliance_pathway_constraint, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: STATE ENVIRONMENTAL AGENCIES (ROPE) — Institutional beneficiaries with arbitrage options. Receive delegated authority to set compliance pathways, gain resource control and regulatory discretion, and can leverage clean-energy or fossil-fuel-friendly standards based on state politics. Experience the constraint as pure coordination — solves the federal/state governance problem with appropriate resource allocation to states. Effective extraction flows toward these agencies.
constraint_indexing:constraint_classification(caa_section_111d_delegation__state_compliance_pathway_constraint, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 3: REGULATED FOSSIL FUEL ENTITIES (TANGLED ROPE) — Powerful but mobile agents. Benefit from state-level discretion (weak state standards = competitive advantage). Also face compliance coordination burden and potential state-by-state variation cost. Mix of coordination (states setting standards creates regulatory clarity) and extraction (states may impose stricter standards than federal baseline). Mobility allows sector-wide relocation strategies (regulatory arbitrage between states).
constraint_indexing:constraint_classification(caa_section_111d_delegation__state_compliance_pathway_constraint, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 4: EPA AS DELEGATING INSTITUTION (TANGLED ROPE) — Constrained by statutory mandate and political/judicial pressure. Delegates compliance pathway design to states (coordination function: subsidiarity, local knowledge) while retaining oversight authority (extraction function: EPA sets minimum floors, retains disapproval power). The constraint coordinates federal-state division of labor but extracts institutional legitimacy from EPA to states. EPA bears suppression costs (monitoring state compliance, managing capture risk) while gaining political cover (devolving controversial standards).
constraint_indexing:constraint_classification(caa_section_111d_delegation__state_compliance_pathway_constraint, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: ENVIRONMENTAL ADVOCACY COALITION (SCAFFOLD) — Organized agents (environmental NGOs, public health groups) see the state compliance pathway as a temporary institutional arrangement with sunset potential. Coalition activities (litigation, legislative pressure, state-level organizing) are building exit paths by either recentralizing authority to EPA (congressional amendment) or imposing federal minimum standards that pre-empt state variation. Sunset timeframe: 5-15 years depending on litigation outcomes and political coalition shifts.
constraint_indexing:constraint_classification(caa_section_111d_delegation__state_compliance_pathway_constraint, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / FEDERALISM AS NATURAL LAW (MOUNTAIN) — From a civilizational/universal perspective, federal-state division of authority is an unchangeable feature of constitutional structure. The constraint appears as natural law grounded in enumerated powers (Commerce Clause limits, Tenth Amendment structural limits). However, the structural data contradicts the mountain classification: identifiable beneficiaries (state agencies, regulated entities), victim dynamics (EPA authority loss, public health costs), and active extraction mechanisms (state discretion used to weaken standards) reveal this as a false summit — federalism rhetoric naturalizes what is actually a contestable institutional arrangement.
constraint_indexing:constraint_classification(caa_section_111d_delegation__state_compliance_pathway_constraint, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(caa_section_111d_delegation__state_compliance_pathway_constraint_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(caa_section_111d_delegation__state_compliance_pathway_constraint, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(caa_section_111d_delegation__state_compliance_pathway_constraint, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(caa_section_111d_delegation__state_compliance_pathway_constraint, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(caa_section_111d_delegation__state_compliance_pathway_constraint_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The delegation transfers regulatory discretion from EPA to states, which is experienced as extraction by EPA and public health communities (who lose federal standard-setting clarity). However, the extraction is not maximal because: (1) EPA retains disapproval authority over state pathways that fail to achieve the federal standard; (2) the federal target itself remains binding; (3) states have incentive to achieve standards (regulatory pressure, litigation risk). The value of 0.52 reflects that the constraint creates asymmetric extraction (states gain discretion; EPA loses it) without eliminating federal authority entirely. The trajectory shows increasing extractiveness over the 10-year interval as states develop pathways that approach but do not exceed the federal minimum, suggesting regulatory capture accumulation. Suppression (0.48): Moderate. Barriers to non-compliance by states include federal disapproval threat, Clean Air Act enforcement provisions, and federal-state political dynamics. However, suppression is not total because states have meaningful discretion and EPA's enforcement capacity is constrained by political considerations. Theater ratio (0.58): Moderate-high. State compliance pathways involve significant procedural theater: notice-and-comment rulemaking, stakeholder participation, technical analyses that perform legitimacy for pathways that may already be predetermined by political coalitions. However, the theater is not maximal because some state processes do generate genuine alternative pathway designs. The measurement trajectory shows increasing theater, reflecting accumulating procedural complexity without corresponding increase in substantive alternative generation.
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits stark perspectival divergence across institutional contexts. EPA sees the delegation as a loss of authority masked by federalism rhetoric (institutional/constrained → tangled_rope: loses discretion but retains supervisory duty). State agencies see it as legitimate subsidiarity and resource allocation (institutional/arbitrage → rope: genuine coordination of federal-state authority). Regulated entities see mixed coordination (clarity on pathways) and extraction (state discretion can cut both ways) (powerful/mobile → tangled_rope: can arbitrage across states but face variation costs). Public health communities see pure extraction through the powerless/trapped lens: they have no direct role in state standard-setting and bear the costs of weak state pathways (powerless/trapped → snare). The environmental advocacy coalition sees a temporary institutional arrangement with litigation-driven sunset potential (organized/constrained → scaffold). The analytical observer risks naturalizing federalism as constitutional law rather than recognizing the delegation as a contestable institutional choice (analytical/analytical → mountain, flagged as false summit). The perspectival gap reveals that the constraint's type depends critically on which institutional actor's structural position defines the analysis.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality vector (d) for each perspective is derived from the agent's structural position: power level, exit options, and relationship to the extraction flow. State environmental agencies (institutional/arbitrage) experience low d because they are beneficiaries with exit options (can propose pathways that serve state interests; if EPA disapproves, appeal to political processes). EPA (institutional/constrained) experiences moderate d because it retains authority but faces high costs to enforce disapproval (political capital). Regulated entities (powerful/mobile) experience moderate d: they benefit from state discretion but face variation costs; arbitrage exit options reduce perceived extraction. Public health communities (powerless/trapped) experience high d: they are victims with no exit from state regulatory outcomes and no direct federal recourse. The analytical observer (analytical/analytical) operates outside the extraction flow and derives d from the structure's typical target status (moderate d ≈ 0.72 by canonical fallback). The pipeline computes effective extractiveness chi = ε × f(d) × σ(S) where σ(national) = 1.0. EPA's institutional status is institutional; state agencies' status is institutional — but their directionality differs because of beneficiary/victim declarations and exit options.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint does NOT resolve the mandatrophy (mandatrophy_resolved: false) because the tension between coordination and extraction remains genuinely irreducible at the current epistemic state. Federalism doctrine treats the delegation as legitimate subsidiarity (coordination function). Empirical outcomes reveal state regulatory capture patterns (extraction function). The two readings are not contradictory — they describe different causal chains operating simultaneously: the delegation coordinates federal-state authority assignment (genuine coordination) AND enables state capture of weak standards (genuine extraction). The mandatrophy persists because: (1) capture probability is uncertain (omega 1); (2) EPA's enforcement capacity is politically ambiguous (omega 2); (3) constitutional doctrine treating the delegation as permissible vs. subject to revisionary doctrine is unsettled (omega 3). The constraint remains tangled_rope (both coordination and extraction present) rather than collapsing to rope or snare because the structural evidence supports both functions. The false summit risk at the analytical level (naturalizing the delegation as unchangeable law) is the mandatrophy's most acute form: if federalism doctrine is treated as natural law, the extraction mechanism becomes invisible and the constraint appears as pure coordination.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    state_capture_probability,
    'What is the actual probability that state-level regulatory discretion will be captured by regulated industries across different state jurisdictions?',
    'Empirical analysis of state compliance pathways post-delegation: comparison of state-proposed standards to EPA baseline expectations; tracking of state regulatory changes; analysis of industry lobbying expenditure correlation with state standard-setting.',
    'If capture probability > 0.6: constraint is structurally a snare (state pathways lock in weak standards). If capture probability < 0.3: constraint is structural rope (genuine subsidiarity). Current estimates center ~0.55, justifying tangled_rope classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(state_capture_probability, empirical, 'Empirical probability of industry capture at state regulatory level').

omega_variable(
    federal_minimum_floor_enforceability,
    'Can the EPA effectively enforce minimum federal standards against state non-compliance, or do political/judicial constraints make the disapproval power illusory?',
    'Historical analysis of EPA disapproval patterns; judicial review of EPA enforcement actions; investigation of political costs to EPA of using disapproval authority.',
    'If enforceability is high: EPA retains real supervisory power and constraint is rope+extraction hybrid (tangled_rope). If enforceability is low: EPA''s authority is theatrical and constraint is closer to pure state discretion (snare from public health perspective).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(federal_minimum_floor_enforceability, empirical, 'Whether EPA minimum-floor enforcement is structurally viable').

omega_variable(
    constitutional_delegation_doctrine_stability,
    'Is the constitutional permissibility of delegating standard-setting to states a settled question, or is it subject to revisionary doctrine (e.g., non-delegation doctrine revival, commandeering doctrine)?',
    'Analysis of current constitutional law scholarship; tracking of Supreme Court signals regarding delegation and federalism doctrines; identification of which legal communities treat state delegation as changeable.',
    'If doctrine is stable: the constraint''s legitimacy is secure from legal revisionary pressure. If doctrine is unsettled: the scaffold perspective (litigation-driven sunset) becomes more plausible.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(constitutional_delegation_doctrine_stability, conceptual, 'Stability of constitutional doctrine permitting state-level delegation').

omega_variable(
    reading_kernel_ambiguity,
    'Is Section 111(d) properly read as authorizing state compliance pathways as a genuine expression of federalism subsidiarity, or as a constraint imposed on EPA''s standard-setting authority that operates to reduce federal environmental stringency?',
    'Legal interpretation of statutory text and legislative history; analysis of constitutional principles underlying delegation; examination of empirical outcomes post-delegation to assess whether subsidiarity or extraction is the primary structural function.',
    'If subsidiarity reading is correct: this reading (state_compliance_pathway_constraint) accurately captures the constraint as tangled_rope with genuine coordination function. If extraction reading dominates: the constraint''s primary function is extracting authority from EPA, and tangled_rope classification requires much higher victim-side recognition.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_kernel_ambiguity, conceptual, 'Whether state pathways embody subsidiarity or extract EPA authority').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(caa_section_111d_delegation__state_compliance_pathway_constraint, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(caa_111d_state_theater_t0, caa_section_111d_delegation__state_compliance_pathway_constraint, theater_ratio, 0, 0.45).
narrative_ontology:measurement(caa_111d_state_theater_t5, caa_section_111d_delegation__state_compliance_pathway_constraint, theater_ratio, 5, 0.55).
narrative_ontology:measurement(caa_111d_state_theater_t10, caa_section_111d_delegation__state_compliance_pathway_constraint, theater_ratio, 10, 0.58).

% Extraction over time
narrative_ontology:measurement(caa_111d_state_extractiveness_t0, caa_section_111d_delegation__state_compliance_pathway_constraint, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(caa_111d_state_extractiveness_t5, caa_section_111d_delegation__state_compliance_pathway_constraint, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(caa_111d_state_extractiveness_t10, caa_section_111d_delegation__state_compliance_pathway_constraint, base_extractiveness, 10, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(caa_111d_state_suppression_t0, caa_section_111d_delegation__state_compliance_pathway_constraint, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(caa_111d_state_suppression_t5, caa_section_111d_delegation__state_compliance_pathway_constraint, suppression_requirement, 5, 0.46).
narrative_ontology:measurement(caa_111d_state_suppression_t10, caa_section_111d_delegation__state_compliance_pathway_constraint, suppression_requirement, 10, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(caa_section_111d_delegation__state_compliance_pathway_constraint, enforcement_mechanism).
narrative_ontology:affects_constraint(caa_section_111d_delegation__state_compliance_pathway_constraint, epa_carbon_rule_authority_limits).
narrative_ontology:affects_constraint(caa_section_111d_delegation__state_compliance_pathway_constraint, state_regulatory_capture_dynamics).

% DUAL FORMULATION NOTE:
% The state compliance pathway constraint is downstream of EPA's delegating authority but represents a structurally distinct constraint with its own directionality dynamics. A separate story analyzing EPA's authority to set the federal standard (the stringency level rather than the pathway design) would have different ε and different perspectives. The two stories are linked: EPA's standard-setting authority constrains the floor that states must meet, but the state pathway constraint operates independently as a coordination-plus-extraction hybrid.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(caa_section_111d_delegation__state_compliance_pathway_constraint, institutional, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
