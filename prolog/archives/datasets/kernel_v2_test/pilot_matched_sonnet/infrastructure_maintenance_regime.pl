% ============================================================================
% CONSTRAINT STORY: infrastructure_maintenance_regime
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-01-10
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_infrastructure_maintenance_regime, []).

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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
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
 *   constraint_id: infrastructure_maintenance_regime
 *   human_readable: Infrastructure Maintenance Regime in Dutch Flood Defense
 *   domain: disaster_preparedness/institutional_memory/civil_engineering
 *
 * SUMMARY:
 *   The Dutch infrastructure maintenance regime emerged from the 1953 North
 *   Sea flood disaster, which killed 1,836 people and exposed catastrophic
 *   failures in dike maintenance and flood preparedness. Rijkswaterstaat, the
 *   national water management authority, institutionalized a systematic
 *   inspection and remediation protocol: six-year inspection cycles for
 *   primary flood defenses, risk-based prioritization of structural defects,
 *   and centralized coordination of remediation budgets. This constraint is
 *   the PHYSICAL INFRASTRUCTURE component of the broader post-1953
 *   preparedness commitment — distinct from drill-based preparedness
 *   (evacuation exercises, communication protocols) and public awareness
 *   campaigns. The structural delta this story measures is whether the
 *   inspection regime maintains operational competence or has drifted into
 *   memorial performance. The competence_reading holds that engineering
 *   inspection protocols remain functionally robust — the regime is a genuine
 *   coordination mechanism (Rope). The husk_reading holds that form persists
 *   while function atrophies — inspections become ritualized, remediation
 *   lags accumulate, and the regime becomes theater (Piton). The
 *   hybrid_reading holds stratification: engineering inspection remains
 *   competent while other preparedness components (drills, public awareness)
 *   have ritualized. This story authors the competence_reading: the
 *   infrastructure maintenance regime as a functional coordination mechanism
 *   with low extraction and minimal theater. Sibling readings (husk and
 *   hybrid) would be separate constraint stories with different epsilon
 *   values, linked via network.affects_constraints.
 *
 * KEY AGENTS:
 *   - Population at Flood Risk: Primary beneficiary (powerless/trapped) — geographically locked to flood-prone regions; cannot exit but benefits from coordinated infrastructure maintenance that no individual could provide
 *   - Rijkswaterstaat Engineering Corps: Co-beneficiary and operator (institutional/constrained) — career-locked to flood defense domain; benefits from professional legitimacy while bearing operational responsibility for inspection quality and remediation prioritization
 *   - Municipal Water Boards: Organized beneficiary (organized/mobile) — local authorities that benefit from centralized technical standards enabling local implementation without independent expertise development
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — sees the regime as coordination solving the generational-timescale problem of maintaining complex civil engineering systems
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(infrastructure_maintenance_regime, 0.15).
domain_priors:suppression_score(infrastructure_maintenance_regime, 0.2).
domain_priors:theater_ratio(infrastructure_maintenance_regime, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(infrastructure_maintenance_regime, extractiveness, 0.15).
narrative_ontology:constraint_metric(infrastructure_maintenance_regime, suppression_requirement, 0.2).
narrative_ontology:constraint_metric(infrastructure_maintenance_regime, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(infrastructure_maintenance_regime, rope).
narrative_ontology:human_readable(infrastructure_maintenance_regime, "Infrastructure Maintenance Regime in Dutch Flood Defense").
narrative_ontology:topic_domain(infrastructure_maintenance_regime, "disaster_preparedness/institutional_memory/civil_engineering").

domain_priors:requires_active_enforcement(infrastructure_maintenance_regime).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(infrastructure_maintenance_regime, 'a70060ff-bc71-4024-b07a-fddfec550d7b').
narrative_ontology:cs_kernel_codification('a70060ff-bc71-4024-b07a-fddfec550d7b', formalized).
narrative_ontology:cs_authority_grounding('a70060ff-bc71-4024-b07a-fddfec550d7b', expertise).
narrative_ontology:cs_interpretation_layer_present('a70060ff-bc71-4024-b07a-fddfec550d7b').
narrative_ontology:cs_reading_relation('a70060ff-bc71-4024-b07a-fddfec550d7b', infrastructure_maintenance_regime__husk_reading, coexists_with).
narrative_ontology:cs_reading_relation('a70060ff-bc71-4024-b07a-fddfec550d7b', infrastructure_maintenance_regime__hybrid_reading, coexists_with).
narrative_ontology:cs_axiom('a70060ff-bc71-4024-b07a-fddfec550d7b', foundational, engineering_inspection_maintains_competence).
narrative_ontology:cs_axiom_status(engineering_inspection_maintains_competence, holdable).
narrative_ontology:cs_axiom_grounding('a70060ff-bc71-4024-b07a-fddfec550d7b', engineering_inspection_maintains_competence, empirically_contingent).
narrative_ontology:cs_axiom('a70060ff-bc71-4024-b07a-fddfec550d7b', secondary, centralized_coordination_necessary).
narrative_ontology:cs_axiom_status(centralized_coordination_necessary, holdable).
narrative_ontology:cs_axiom_grounding('a70060ff-bc71-4024-b07a-fddfec550d7b', centralized_coordination_necessary, instrumental).
narrative_ontology:cs_reference_frame('a70060ff-bc71-4024-b07a-fddfec550d7b', post_1953_trauma_institutionalization).
narrative_ontology:cs_drift_state('a70060ff-bc71-4024-b07a-fddfec550d7b', contemporary, gap(practice_drift, minor, true)).
narrative_ontology:cs_created_at('a70060ff-bc71-4024-b07a-fddfec550d7b', '2025-01-10T00:00:00Z').

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(infrastructure_maintenance_regime, population_at_flood_risk).
narrative_ontology:constraint_beneficiary(infrastructure_maintenance_regime, rijkswaterstaat_engineering_corps).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(infrastructure_maintenance_regime, municipal_water_boards).
narrative_ontology:constraint_vindicates(infrastructure_maintenance_regime, high_reliability_organization_theory).
narrative_ontology:constraint_vindicates(infrastructure_maintenance_regime, engineering_inspection_sufficiency).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Households and communities in flood-prone regions below sea level. Geographically locked — relocation is prohibitively costly for most. Cannot inspect or maintain dike infrastructure individually. Depend entirely on the centralized regime for flood protection. Benefit from coordinated inspection and remediation that no decentralized alternative could provide at comparable scale.
narrative_ontology:constraint_stakeholder(infrastructure_maintenance_regime, population_at_flood_risk, beneficiary,
    powerless, biographical, trapped, regional).

% National water management authority with technical-administrative control over flood defense infrastructure. Sets inspection protocols, prioritizes remediation, allocates budget. Career paths locked to this domain — engineers build professional identity through institutional continuity. Benefits from legitimacy grounded in 1953 trauma memory and engineering expertise. Bears operational responsibility for inspection quality and remediation effectiveness. Dual-positioned: both runs the regime and collects professional legitimacy from it.
narrative_ontology:constraint_stakeholder(infrastructure_maintenance_regime, rijkswaterstaat_engineering_corps, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(infrastructure_maintenance_regime, rijkswaterstaat_engineering_corps, beneficiary).

% Local authorities responsible for regional water management. Implement centralized inspection standards at local level. Benefit from technical coordination — do not need to develop independent engineering expertise for complex structural assessment. Retain autonomy over local water management decisions outside flood defense. Could advocate for alternative governance structures (decentralization, privatization) but choose not to because coordination benefits outweigh autonomy costs.
narrative_ontology:constraint_stakeholder(infrastructure_maintenance_regime, municipal_water_boards, beneficiary,
    organized, generational, mobile, regional).

% The body of civil engineering knowledge and practice that the regime instantiates. Not an agent — a discipline cannot collect rents. Included for narrative completeness: the regime vindicates high-reliability organization theory and engineering inspection sufficiency doctrine, but these are propositions, not beneficiaries.
narrative_ontology:constraint_stakeholder(infrastructure_maintenance_regime, structural_engineering_discipline, observer,
    analytical, civilizational, analytical, global).
narrative_ontology:stakeholder_non_agent(infrastructure_maintenance_regime, structural_engineering_discipline).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The regime coordinates distributed engineering knowledge into systematic infrastructure maintenance across generational timescales. Solves the problem: no individual household or municipality can inspect or maintain complex dike systems; centralized technical standards enable local implementation without each locality developing independent expertise.
% TRANSFER_FUNCTION: Budget flows from national taxation to Rijkswaterstaat for inspection operations and remediation projects. Engineering expertise flows from Rijkswaterstaat to municipal water boards via standardized protocols. Flood protection (the public good) flows from the regime to the protected population. Professional legitimacy flows to Rijkswaterstaat engineers from institutional continuity.
% ABSENT_VOICES: Future generations whose flood risk depends on current maintenance decisions but who have no seat in budget allocation or remediation prioritization. Climate adaptation advocates who argue that inspection protocols designed for 20th-century hydrology are inadequate for 21st-century climate stress. Privatization advocates (largely absent from Dutch flood defense discourse) who would argue for market-based inspection services. These voices are structurally excluded — not because of active suppression but because the regime's legitimacy is grounded in 1953 trauma memory and engineering expertise, which forecloses alternative framings.
% DISAPPEARANCE_RATIONALE: If the inspection regime disappeared overnight, flood defense infrastructure would degrade within years. Dike subsidence, erosion, and structural defects would accumulate undetected. Municipal water boards lack the technical capacity to replace centralized inspection. Within a decade, flood risk would return to pre-1953 levels. The protected population would face catastrophic vulnerability. The regime is not natural law — it is an institutional arrangement that prevents a coordination failure.
% FOUNDING_PROBLEM: The 1953 North Sea flood exposed catastrophic failures in dike maintenance: no systematic inspection protocol, no centralized remediation prioritization, no coordination across municipal boundaries. The founding problem was the absence of a mechanism to maintain complex civil engineering systems across generational timescales when no single locality had the expertise or resources to do so independently.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem remains live, corroborated by: (1) Engineering risk assessments showing ongoing structural defect accumulation in aging infrastructure (Rijkswaterstaat technical reports, 2000-2020). (2) Climate adaptation studies projecting increased flood risk from sea-level rise and storm intensity (Delta Programme, 2010-present). (3) International disaster studies showing that decentralized flood defense systems (e.g., pre-Katrina New Orleans) fail catastrophically when coordination mechanisms are absent (academic literature, multiple sources outside the beneficiary set). The problem is not gone — the regime is the solution that keeps the problem from recurring.
narrative_ontology:disappearance_verdict(infrastructure_maintenance_regime, world_rearranges).
narrative_ontology:founding_problem_status(infrastructure_maintenance_regime, live).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PROTECTED POPULATION (ROPE) — Trapped by geography but benefits from the coordination function. The inspection regime solves a genuine collective action problem: no individual household can inspect or maintain dike infrastructure. Low effective extraction because the constraint delivers real protection.
constraint_indexing:constraint_classification(infrastructure_maintenance_regime, rope,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: RIJKSWATERSTAAT ENGINEERING CORPS (ROPE) — Institutional actor with constrained exit (career path locked to this domain). Experiences the regime as coordination: the inspection cycle and remediation protocols organize distributed engineering knowledge into systematic infrastructure maintenance. Benefits from professional legitimacy but also bears operational responsibility.
constraint_indexing:constraint_classification(infrastructure_maintenance_regime, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: MUNICIPAL WATER BOARDS (ROPE) — Organized local authorities with mobile exit options (can advocate for alternative governance structures). See the regime as coordination: centralized technical standards enable local implementation without each municipality developing independent expertise. Low extraction because coordination benefits are real.
constraint_indexing:constraint_classification(infrastructure_maintenance_regime, rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(regional))).

% PERSPECTIVE 4: ANALYTICAL OBSERVER (ROPE) — From a civilizational perspective, the infrastructure maintenance regime is a coordination mechanism that solves the genuine problem of maintaining complex civil engineering systems across generational timescales. The regime coordinates inspection frequency, remediation protocols, and budget allocation in ways that no decentralized alternative could match. Extraction is minimal — the constraint delivers what it claims to deliver.
constraint_indexing:constraint_classification(infrastructure_maintenance_regime, rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(infrastructure_maintenance_regime_tests).
:- end_tests(infrastructure_maintenance_regime_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.15): Low. The regime extracts modestly through administrative overhead and through the career lock-in of Rijkswaterstaat engineers whose professional identity is fused with this institutional structure. But the extraction is minor relative to the coordination function delivered — the regime genuinely maintains infrastructure that protects millions. The gradual increase from 0.08 (1953) to 0.15 (2013) reflects slow accumulation of administrative overhead and institutional inertia, not a fundamental shift to extraction. Suppression (0.20): Low. Alternative governance structures are possible (fully decentralized municipal control, privatized inspection services, EU-level coordination), and municipal water boards retain significant autonomy. The regime is not coercive — it persists because the coordination benefits are real and widely recognized. Theater ratio (0.35): Moderate-low. Some performative elements exist (anniversary commemorations, public-facing inspection reports, political visibility around major remediation projects), but the core function — structural defect identification and remediation — remains operational. The increase from 0.20 (1953) to 0.35 (2013) reflects generational distance from the founding trauma: as lived memory of 1953 fades, some inspection activity shifts toward memorial performance. But theater remains well below the 0.50 threshold where performance dominates function.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits minimal perspectival gap — all four perspectives classify as Rope. This uniformity is diagnostically significant: it distinguishes the infrastructure maintenance regime from the broader preparedness commitment (which includes drill-based preparedness and public awareness, where perspectival gaps are larger). The uniformity reflects that physical infrastructure inspection has a clear, measurable coordination function (defect detection and remediation) with low ambiguity about whether the function is being delivered. The gradual increase in theater_ratio and extractiveness over 60 years suggests slow drift toward Piton, but the regime has not crossed the threshold where any perspective would reclassify. The omega variables identify the empirical tests that would trigger reclassification: if inspection quality degrades (competence_persistence_vs_ritual_drift), if budget allocation shifts toward procedural theater (physical_vs_procedural_budget_allocation), or if remediation lags accumulate (remediation_lag_threshold), the regime would reclassify to Piton from institutional and analytical perspectives.
 *
 * DIRECTIONALITY LOGIC:
 *   All four perspectives classify as Rope because all agents are net beneficiaries of the coordination function. The protected population (powerless/trapped) experiences low effective extraction despite being trapped — they cannot exit the flood zone, but the regime delivers real protection, so d is low and chi is low. Rijkswaterstaat (institutional/constrained) is both beneficiary (professional legitimacy, career stability) and operator (bears responsibility for inspection quality), but the net flow is toward them — they collect more than they pay. Municipal water boards (organized/mobile) benefit from centralized standards without bearing full coordination costs. The analytical observer sees pure coordination with minimal extraction. No victim group is declared because no agent bears asymmetric costs — the regime's administrative overhead is distributed across beneficiaries in proportion to benefits received. This is the structural signature of a genuine coordination mechanism (Rope) rather than a hybrid with embedded extraction (Tangled Rope).
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by distinguishing physical infrastructure maintenance (this story) from drill-based preparedness (a separate constraint with higher theater_ratio and different beneficiary structure). The mandate — 'maintain flood defenses to prevent 1953-scale catastrophe' — remains live because the physical infrastructure genuinely requires continuous inspection and remediation. The regime has not outlived its function. However, the broader preparedness commitment (drills, public awareness) may exhibit mandatrophy if those components have ritualized while infrastructure maintenance remains functional. The stratification hypothesis (hybrid_reading in the kernel context) would be tested by decomposing the preparedness commitment into multiple constraint stories and comparing their epsilon values. If drill-based preparedness has epsilon > 0.50 while infrastructure maintenance has epsilon < 0.20, the stratification is confirmed and the broader commitment exhibits partial mandatrophy.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    competence_persistence_vs_ritual_drift,
    'Does the inspection regime maintain operational competence, or has it drifted into memorial performance where form persists but function atrophies?',
    'Longitudinal analysis of inspection quality: defect detection rates over time, remediation response lag trends, correlation between inspection findings and actual structural failures. Compare pre-2000 vs post-2010 cohorts of inspectors for tacit knowledge retention.',
    'If competence persists: Rope classification confirmed across all perspectives. If ritual drift: reclassify to Piton from institutional and analytical perspectives — the regime becomes theater maintained through inertia rather than functional coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(competence_persistence_vs_ritual_drift, empirical, 'Whether inspection regime maintains competence or has ritualized').

omega_variable(
    physical_vs_procedural_budget_allocation,
    'What proportion of disaster preparedness budget flows to physical infrastructure maintenance vs procedural drills and exercises?',
    'Budget decomposition analysis: track allocation to structural inspection/remediation vs evacuation drills, communication exercises, and administrative coordination over 20-year period. Identify inflection points where allocation ratios shifted.',
    'If physical infrastructure receives <40% of preparedness budget: extraction structure emerges — procedural theater is crowding out functional maintenance. If >60%: coordination function dominates.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(physical_vs_procedural_budget_allocation, empirical, 'Budget allocation ratio between physical and procedural preparedness').

omega_variable(
    inspection_cycle_adherence_vs_political_pressure,
    'Does inspection frequency respond to engineering risk assessment or to political visibility cycles?',
    'Correlation analysis: inspection frequency vs (a) structural risk indicators (age, soil subsidence, climate stress) vs (b) political visibility events (elections, media coverage, anniversary commemorations). Test whether inspection scheduling is risk-driven or event-driven.',
    'If risk-driven: Rope confirmed — technical coordination dominates. If event-driven: Tangled Rope — genuine coordination function exists but is contaminated by political extraction (visibility theater).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(inspection_cycle_adherence_vs_political_pressure, empirical, 'Whether inspection scheduling is risk-driven or politically driven').

omega_variable(
    remediation_lag_threshold,
    'What remediation response time distinguishes functional maintenance from deferred-maintenance extraction?',
    'Historical analysis of identified defects: time-to-remediation distribution, correlation with defect severity, comparison to engineering risk models. Identify whether lag is driven by resource constraints (coordination problem) or by institutional incentives (extraction).',
    'If median lag <6 months for critical defects: coordination function intact. If >2 years: extraction structure — institutional actors benefit from deferring costly remediation while maintaining inspection theater.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(remediation_lag_threshold, empirical, 'Remediation lag threshold for functional vs extractive maintenance').

omega_variable(
    cs_framing_underdetermination,
    'Is the kernel ''post-1953 preparedness commitment'' or the narrower ''Rijkswaterstaat technical authority''? Does the reading''s structural delta depend on which framing is adopted?',
    'Compare classifications under both framings: (a) kernel = broad preparedness commitment (includes drills, public awareness, infrastructure) vs (b) kernel = narrow technical authority (inspection protocols only). Test whether beneficiary/victim structure shifts.',
    'If broad framing: preparedness is a multi-constraint family (infrastructure + drills + public awareness), each with distinct epsilon. If narrow framing: infrastructure maintenance is the sole constraint, and drill-based preparedness is a separate story. Framing choice determines decomposition strategy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cs_framing_underdetermination, conceptual, 'Kernel framing ambiguity: broad preparedness vs narrow technical authority').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(infrastructure_maintenance_regime, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(infra_maint_theater_1953, infrastructure_maintenance_regime, theater_ratio, 0, 0.2).
narrative_ontology:measurement(infra_maint_tr_t10, infrastructure_maintenance_regime, theater_ratio, 10, 0.22).
narrative_ontology:measurement(infra_maint_tr_t20, infrastructure_maintenance_regime, theater_ratio, 20, 0.25).
narrative_ontology:measurement(infra_maint_tr_t30, infrastructure_maintenance_regime, theater_ratio, 30, 0.28).
narrative_ontology:measurement(infra_maint_tr_t40, infrastructure_maintenance_regime, theater_ratio, 40, 0.3).
narrative_ontology:measurement(infra_maint_tr_t50, infrastructure_maintenance_regime, theater_ratio, 50, 0.32).
narrative_ontology:measurement(infra_maint_tr_t60, infrastructure_maintenance_regime, theater_ratio, 60, 0.35).

% Extraction over time
narrative_ontology:measurement(infra_maint_extract_1953, infrastructure_maintenance_regime, base_extractiveness, 0, 0.08).
narrative_ontology:measurement(infra_maint_be_t10, infrastructure_maintenance_regime, base_extractiveness, 10, 0.09).
narrative_ontology:measurement(infra_maint_be_t20, infrastructure_maintenance_regime, base_extractiveness, 20, 0.1).
narrative_ontology:measurement(infra_maint_be_t30, infrastructure_maintenance_regime, base_extractiveness, 30, 0.11).
narrative_ontology:measurement(infra_maint_be_t40, infrastructure_maintenance_regime, base_extractiveness, 40, 0.13).
narrative_ontology:measurement(infra_maint_be_t50, infrastructure_maintenance_regime, base_extractiveness, 50, 0.14).
narrative_ontology:measurement(infra_maint_be_t60, infrastructure_maintenance_regime, base_extractiveness, 60, 0.15).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(infrastructure_maintenance_regime, enforcement_mechanism).

% DUAL FORMULATION NOTE:
% This constraint is the physical infrastructure component of the broader post-1953 preparedness commitment. Drill-based preparedness (evacuation exercises, communication protocols) and public awareness campaigns are structurally distinct constraints with different epsilon values and should be authored as separate stories. The infrastructure maintenance regime has low epsilon (0.15) because the coordination function (defect detection and remediation) is measurable and functional. Drill-based preparedness likely has higher epsilon because the coordination function (maintaining operational readiness for evacuation) is harder to verify without catastrophe and more vulnerable to ritual drift. If the corpus includes a drill-based preparedness story, link it here via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
