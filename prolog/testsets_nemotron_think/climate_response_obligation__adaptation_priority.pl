% ============================================================================
% CONSTRAINT STORY: climate_response_obligation__adaptation_priority
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_climate_response_obligation__adaptation_priority, []).

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
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
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
 *   constraint_id: climate_response_obligation__adaptation_priority
 *   human_readable: Adaptation-Priority Climate Response (Accept 2-3°C Warming)
 *   domain: climate_policy/political_economy/intergenerational_ethics
 *
 * SUMMARY:
 *   The adaptation_priority reading of the climate_response_obligation kernel
 *   frames 2-3°C warming as inevitable and directs policy toward resilience
 *   investment rather than costly prevention. This reading emerged in the
 *   1990s-2000s as mitigation costs were emphasized by fossil interests and
 *   some economists, and gained traction as emissions continued rising. The
 *   constraint coordinates adaptation planning and finance (genuine
 *   coordination function) while extracting from future generations and the
 *   Global South who bear unmitigated impacts without having benefited from
 *   the emissions that caused them. Fossil capital and current wealthy
 *   generations avoid transition costs. The claim/metric gap is deliberate:
 *   the reading is CLAIMED as rope/coordination (pragmatic resilience) while
 *   authored metrics describe substantially extractive operation with active
 *   suppression of mitigation alternatives — the engine measures that
 *   divergence.
 *
 * KEY AGENTS:
 *   - current_generation_wealthy: Primary beneficiary (avoids transition costs) — institutional/mobile
 *   - fossil_capital: Primary beneficiary (protected asset values, delayed stranding) — institutional/arbitrage
 *   - future_generations: Primary victim (bear climate impacts without prevention) — powerless/trapped
 *   - global_south_populations: Primary victim (disproportionate impacts, minimal adaptation finance) — powerless/constrained
 *   - climate_policy_elite: Agenda setter (frames inevitability, directs finance) — institutional/identity_locked
 *   - climate_justice_advocates: Excluded (demand mitigation, loss-and-damage) — organized/trapped
 *   - scientific_community: Observer (provides evidence, contested framing) — analytical/analytical
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_response_obligation__adaptation_priority, 0.72).
domain_priors:suppression_score(climate_response_obligation__adaptation_priority, 0.65).
domain_priors:theater_ratio(climate_response_obligation__adaptation_priority, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_response_obligation__adaptation_priority, extractiveness, 0.72).
narrative_ontology:constraint_metric(climate_response_obligation__adaptation_priority, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(climate_response_obligation__adaptation_priority, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_response_obligation__adaptation_priority, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(climate_response_obligation__adaptation_priority, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_response_obligation__adaptation_priority, tangled_rope).
narrative_ontology:human_readable(climate_response_obligation__adaptation_priority, "Adaptation-Priority Climate Response (Accept 2-3°C Warming)").
narrative_ontology:topic_domain(climate_response_obligation__adaptation_priority, "climate_policy/political_economy/intergenerational_ethics").

domain_priors:requires_active_enforcement(climate_response_obligation__adaptation_priority).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_response_obligation__adaptation_priority, '450b83e4-e0bd-445e-a527-e0708024102a').
narrative_ontology:cs_kernel_codification('450b83e4-e0bd-445e-a527-e0708024102a', distributed).
narrative_ontology:cs_authority_grounding('450b83e4-e0bd-445e-a527-e0708024102a', distributed).
narrative_ontology:cs_reading_relation('450b83e4-e0bd-445e-a527-e0708024102a', climate_response_obligation__mitigation_priority, coexists_with).
narrative_ontology:cs_reading_relation('450b83e4-e0bd-445e-a527-e0708024102a', climate_response_obligation__degrowth_reading, coexists_with).
narrative_ontology:cs_axiom('450b83e4-e0bd-445e-a527-e0708024102a', foundational, warming_inevitability_accepted).
narrative_ontology:cs_axiom_status(warming_inevitability_accepted, holdable).
narrative_ontology:cs_axiom_grounding('450b83e4-e0bd-445e-a527-e0708024102a', warming_inevitability_accepted, empirically_contingent).
narrative_ontology:cs_axiom('450b83e4-e0bd-445e-a527-e0708024102a', foundational, adaptation_over_mitigation_efficiency).
narrative_ontology:cs_axiom_status(adaptation_over_mitigation_efficiency, holdable).
narrative_ontology:cs_axiom_grounding('450b83e4-e0bd-445e-a527-e0708024102a', adaptation_over_mitigation_efficiency, instrumental).
narrative_ontology:cs_reference_frame('450b83e4-e0bd-445e-a527-e0708024102a', pragmatic_adaptation_framework).
narrative_ontology:cs_drift_state('450b83e4-e0bd-445e-a527-e0708024102a', paris_agreement_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('450b83e4-e0bd-445e-a527-e0708024102a', '').
narrative_ontology:cs_kernel_id(climate_response_obligation__adaptation_priority, climate_response_obligation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_response_obligation__adaptation_priority, current_generation_wealthy).
narrative_ontology:constraint_beneficiary(climate_response_obligation__adaptation_priority, fossil_capital).
narrative_ontology:constraint_victim(climate_response_obligation__adaptation_priority, future_generations).
narrative_ontology:constraint_victim(climate_response_obligation__adaptation_priority, global_south_populations).
narrative_ontology:constraint_vindicates(climate_response_obligation__adaptation_priority, cost_effective_adaptation).
narrative_ontology:constraint_vindicates(climate_response_obligation__adaptation_priority, technological_optimism).
narrative_ontology:constraint_vindicates(climate_response_obligation__adaptation_priority, economic_growth_primacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Avoids the transition costs of rapid decarbonization (energy system transformation, stranded asset losses, lifestyle changes). Benefits from continued fossil energy access and economic growth. Capital mobility and political influence provide arbitrage-grade exit from climate policy costs — can relocate assets, influence regulation, insure against risks.
narrative_ontology:constraint_stakeholder(climate_response_obligation__adaptation_priority, current_generation_wealthy, beneficiary,
    institutional, biographical, arbitrage, global).

% Asset values protected by delayed stranding; continued demand for fossil products under adaptation-priority framing. Directly shapes climate policy through lobbying, funding think tanks, revolving-door personnel. Capital mobility and state capture provide arbitrage exit — can shift jurisdictions, influence regulatory capture, diversify into 'green' assets while maintaining core extraction.
narrative_ontology:constraint_stakeholder(climate_response_obligation__adaptation_priority, fossil_capital, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(climate_response_obligation__adaptation_priority, fossil_capital, agenda_setter).

% Inherit 2-3°C+ world with locked-in impacts (sea-level rise, extreme heat, ecosystem collapse, agricultural disruption). No voice in current policy; cannot exit the climate system. Bear costs of adaptation limits, residual damages, and potential tipping points. The constraint's extraction from them is total — they receive no benefits from current emissions and cannot opt out.
narrative_ontology:constraint_stakeholder(climate_response_obligation__adaptation_priority, future_generations, payer,
    powerless, civilizational, trapped, universal).

% Face disproportionate climate impacts (geographic vulnerability, limited adaptive capacity) while having contributed least to historical emissions. Adaptation finance flows are insufficient and often misdirected (loans not grants, mitigation-tagged projects). Exit options are constrained — migration is costly and politically blocked; adaptation is underfunded. Some agency through climate justice movements and UNFCCC negotiation blocs, but structural power asymmetry remains extreme.
narrative_ontology:constraint_stakeholder(climate_response_obligation__adaptation_priority, global_south_populations, payer,
    powerless, generational, constrained, global).

% Frames climate response around adaptation priority; staffs IPCC working groups, UNFCCC secretariats, national climate ministries, development bank climate units. Professional identity and career capital are fused to the adaptation_priority paradigm — shifting framing would invalidate expertise and networks. They administer the constraint (coordination of adaptation finance, NAP processes, resilience metrics) and benefit from its institutional maintenance.
narrative_ontology:constraint_stakeholder(climate_response_obligation__adaptation_priority, climate_policy_elite, agenda_setter,
    institutional, biographical, identity_locked, global).

% Demand mitigation priority, loss-and-damage finance, and equity-based burden sharing. Structurally excluded from core climate finance and adaptation governance (e.g., Green Climate Fund board composition, NAP drafting). Their exclusion is functional — the adaptation_priority framing requires their demands to remain marginal to sustain the inevitability narrative. They are trapped in advocacy because the constraint's persistence depends on their marginalization.
narrative_ontology:constraint_stakeholder(climate_response_obligation__adaptation_priority, climate_justice_advocates, excluded,
    organized, biographical, trapped, global).

% Produces the evidence base (IPCC reports, IAM scenarios, attribution studies) that all three readings cite selectively. Some scientists advocate for mitigation_priority; others emphasize adaptation limits. The community as a whole holds analytical exit — it can reframe the evidence — but individual careers are tied to specific research programs. The adaptation_priority reading selectively cites 'feasibility' assessments that accept high warming.
narrative_ontology:constraint_stakeholder(climate_response_obligation__adaptation_priority, scientific_community, observer,
    analytical, generational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(climate_response_obligation__adaptation_priority, fossil_capital).
narrative_ontology:fixing_cost_class(climate_response_obligation__adaptation_priority, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinating adaptation investment and resilience planning across jurisdictions, sectors, and timescales: building sea walls, developing drought-resistant crops, relocating vulnerable communities, insuring climate risks — solving the collective-action problem of who pays for what protection where.
% TRANSFER_FUNCTION: Transfers climate risk and adaptation costs from current wealthy generation and fossil capital to future generations and Global South populations. Transfers avoided mitigation costs (energy transition, stranded assets, consumption changes) to fossil capital and current consumers. Transfers political responsibility from present decision-makers to future bearers of consequences.
% ABSENT_VOICES: Future generations (temporally excluded — cannot participate in current governance). Global South populations (politically marginalized in UNFCCC consensus processes, underrepresented in climate finance governance). Non-human species and ecosystems (no standing in climate policy). Frontline communities in wealthy nations (Indigenous, low-income coastal) — present but not empowered.
% DISAPPEARANCE_RATIONALE: If the adaptation_priority framing vanished overnight, mitigation would become the central policy objective: fossil capital would face immediate transition pressure (carbon pricing, regulation, litigation), Global South would gain leverage for loss-and-damage finance and technology transfer, adaptation finance would be reallocated from asset protection to vulnerability reduction, and the climate policy regime would reorganize around intergenerational equity rather than cost avoidance.
% FOUNDING_PROBLEM: How to maintain economic growth and energy access for current populations while responding to climate change, given perceived infeasibility of rapid decarbonization.
% FOUNDING_PROBLEM_CORROBORATION: Adaptation_priority proponents (OECD governments, fossil majors, some economists) attest the problem remains live — mitigation is still costly and politically difficult. Mitigation_priority proponents (IPCC WGIII, IEA net-zero roadmap, climate justice movements) attest the founding problem was framed to protect incumbent interests — mitigation is affordable and the 'infeasibility' was manufactured. Degrowth_reading proponents attest the founding problem accepts growth as non-negotiable, which is the root constraint. No single corroboration exists outside the beneficiary set; the contestation itself is the structural fact.
narrative_ontology:disappearance_verdict(climate_response_obligation__adaptation_priority, world_rearranges).
narrative_ontology:founding_problem_status(climate_response_obligation__adaptation_priority, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_response_obligation__adaptation_priority, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(climate_response_obligation__adaptation_priority, 'none', 1).
narrative_ontology:epsilon_provenance(climate_response_obligation__adaptation_priority, 0.72, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(climate_response_obligation__adaptation_priority_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(climate_response_obligation__adaptation_priority, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(climate_response_obligation__adaptation_priority_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.72) is high because the constraint transfers climate risk and avoided mitigation costs from current wealthy actors to future and vulnerable populations. Suppression (0.65) is substantial because the inevitability framing actively marginalizes mitigation pathways and loss-and-damage claims — not merely passive neglect. Theater ratio (0.45) is moderate: adaptation finance exists but is performative relative to need (e.g., Green Climate Fund shortfalls, private insurance retreat). Accessibility collapse (0.55) reflects that mitigation alternatives remain technically feasible but are politically/institutionally suppressed. Resistance (0.58) is significant from climate movements, vulnerable nations, and increasingly from financial regulators — but has not shifted the dominant framing. Measurements use a shared time grid (1990-2030, 5-year intervals) so all metrics are authored at every point.
 *
 * PERSPECTIVAL GAP:
 *   The agenda_setter seat (climate_policy_elite) experiences the constraint as genuine coordination — they built the adaptation architecture and maintain it. The payer seats (future_generations, global_south_populations) experience it as enforced extraction with no voice in its design. The beneficiary seats (current_generation_wealthy, fossil_capital) experience it as subsidized coordination — they get resilience investment without paying full mitigation costs. The engine computes this divergence from the structural data; the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Current_generation_wealthy and fossil_capital are structural beneficiaries (d near 0.0-0.2): they collect avoided transition costs and protected asset values, with arbitrage-grade exit (capital mobility, political influence). Future_generations are full targets (d = 1.0): powerless, trapped, identity-locked into the climate system — they cannot exit the constraint. Global_south_populations are near-full targets (d = 0.85-0.95): powerless but with some constrained exit (migration, adaptation), though exit is itself costly extraction. Climate_policy_elite are agenda_setters with identity_locked exit — their professional identity fuses with the adaptation_priority framing. Climate_justice_advocates are excluded (trapped in the discourse). Scientific_community are analytical observers (d = 0.5).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (maintaining growth/energy access while responding to climate change) is contested: mitigation_priority and degrowth_reading proponents argue the problem was framed to protect incumbent interests, while adaptation_priority proponents argue mitigation was infeasible. The constraint persists because the agenda_setters (climate_policy_elite) are identity-locked to the framing, and the payers (future_generations, Global South) lack coalition power to force revision. Mandatrophy is unresolved — the arrangement's original justification (pragmatic necessity) is contested but the constraint persists via institutional inertia and suppressed alternatives.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is the adaptation_priority reading a distinct constraint from mitigation_priority and degrowth_reading, or a contingent policy emphasis within a single climate_response_obligation?',
    'Trace whether the three readings instantiate different ε values, different beneficiary/victim sets, and different coordination/extraction structures when evaluated against the same standing arrangement (the climate policy regime). If ε and structural data diverge, they are distinct constraints per ε-invariance.',
    'If distinct, each reading gets its own classification and the kernel is a family of linked constraints. If not distinct, the kernel collapses to one constraint with observer-axis variance.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Whether the three declared readings are structurally distinct constraints or observer variants of one constraint.').

omega_variable(
    warming_inevitability_naturalness,
    'Is the claim that 2-3°C warming is ''inevitable'' a physical/empirical constraint (mountain-like) or a policy choice presented as necessity (constructed)?',
    'Compare integrated assessment model (IAM) scenarios that achieve <2°C with those that accept 2-3°C; assess whether the gap is technological feasibility, political economy, or framing. Track how ''inevitability'' claims shift as mitigation technology costs fall.',
    'If physical necessity, the adaptation_priority reading has a mountain-like coordination core. If constructed, the inevitability claim is cover for extraction — the constraint is snare/tangled_rope with higher ε.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(warming_inevitability_naturalness, empirical, 'Whether warming acceptance reflects irreducible physics or suppressible alternatives.').

omega_variable(
    adaptation_investment_distribution,
    'Does adaptation investment actually flow to the most vulnerable (Global South, future generations) or concentrate in wealthy regions protecting fossil capital assets?',
    'Track climate finance flows (Green Climate Fund, national adaptation plans, private insurance markets) by recipient region and sector. Compare per-capita adaptation spending in OECD vs. Global South; assess whether ''resilience'' spending protects existing asset values or reduces vulnerability.',
    'If investment concentrates in wealthy regions, the coordination function is partial and extraction dominates — tangled_rope or snare. If broadly distributed, coordination function is genuine — rope or tangled_rope with lower ε.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(adaptation_investment_distribution, empirical, 'Whether the adaptation coordination function serves the declared beneficiaries or the structural beneficiaries.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is suppression of mitigation alternatives structural (institutional lock-in, fossil subsidies, infrastructure inertia) or internalized (normalized discourse, career incentives, cognitive capture of policy elites)?',
    'Post-policy-shift suppression trajectory: if a jurisdiction adopts strong mitigation policy, measure whether suppression of alternatives persists (internalized) or decays (structural). Track discourse networks and career pathways in climate policy institutions.',
    'If internalized, effective suppression is higher than structural measure — the constraint travels with agents after institutional exit. This raises χ for analytical and institutional seats.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism in climate policy.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_response_obligation__adaptation_priority, 1990, 2030).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(climate_response_obligation__adaptation_priority_tr_t1990, climate_response_obligation__adaptation_priority, theater_ratio, 1990, 0.2).
narrative_ontology:measurement(climate_response_obligation__adaptation_priority_tr_t1995, climate_response_obligation__adaptation_priority, theater_ratio, 1995, 0.22).
narrative_ontology:measurement(climate_response_obligation__adaptation_priority_tr_t2000, climate_response_obligation__adaptation_priority, theater_ratio, 2000, 0.28).
narrative_ontology:measurement(climate_response_obligation__adaptation_priority_tr_t2005, climate_response_obligation__adaptation_priority, theater_ratio, 2005, 0.32).
narrative_ontology:measurement(climate_response_obligation__adaptation_priority_tr_t2010, climate_response_obligation__adaptation_priority, theater_ratio, 2010, 0.36).
narrative_ontology:measurement(climate_response_obligation__adaptation_priority_tr_t2015, climate_response_obligation__adaptation_priority, theater_ratio, 2015, 0.4).
narrative_ontology:measurement(climate_response_obligation__adaptation_priority_tr_t2020, climate_response_obligation__adaptation_priority, theater_ratio, 2020, 0.43).
narrative_ontology:measurement(climate_response_obligation__adaptation_priority_tr_t2025, climate_response_obligation__adaptation_priority, theater_ratio, 2025, 0.45).
narrative_ontology:measurement(climate_response_obligation__adaptation_priority_tr_t2030, climate_response_obligation__adaptation_priority, theater_ratio, 2030, 0.46).

% Extraction over time
narrative_ontology:measurement(climate_response_obligation__adaptation_priority_be_t1990, climate_response_obligation__adaptation_priority, base_extractiveness, 1990, 0.35).
narrative_ontology:measurement(climate_response_obligation__adaptation_priority_be_t1995, climate_response_obligation__adaptation_priority, base_extractiveness, 1995, 0.38).
narrative_ontology:measurement(climate_response_obligation__adaptation_priority_be_t2000, climate_response_obligation__adaptation_priority, base_extractiveness, 2000, 0.42).
narrative_ontology:measurement(climate_response_obligation__adaptation_priority_be_t2005, climate_response_obligation__adaptation_priority, base_extractiveness, 2005, 0.48).
narrative_ontology:measurement(climate_response_obligation__adaptation_priority_be_t2010, climate_response_obligation__adaptation_priority, base_extractiveness, 2010, 0.55).
narrative_ontology:measurement(climate_response_obligation__adaptation_priority_be_t2015, climate_response_obligation__adaptation_priority, base_extractiveness, 2015, 0.62).
narrative_ontology:measurement(climate_response_obligation__adaptation_priority_be_t2020, climate_response_obligation__adaptation_priority, base_extractiveness, 2020, 0.68).
narrative_ontology:measurement(climate_response_obligation__adaptation_priority_be_t2025, climate_response_obligation__adaptation_priority, base_extractiveness, 2025, 0.72).
narrative_ontology:measurement(climate_response_obligation__adaptation_priority_be_t2030, climate_response_obligation__adaptation_priority, base_extractiveness, 2030, 0.74).

% Suppression requirement over time
narrative_ontology:measurement(climate_response_obligation__adaptation_priority_su_t1990, climate_response_obligation__adaptation_priority, suppression_requirement, 1990, 0.4).
narrative_ontology:measurement(climate_response_obligation__adaptation_priority_su_t1995, climate_response_obligation__adaptation_priority, suppression_requirement, 1995, 0.42).
narrative_ontology:measurement(climate_response_obligation__adaptation_priority_su_t2000, climate_response_obligation__adaptation_priority, suppression_requirement, 2000, 0.45).
narrative_ontology:measurement(climate_response_obligation__adaptation_priority_su_t2005, climate_response_obligation__adaptation_priority, suppression_requirement, 2005, 0.48).
narrative_ontology:measurement(climate_response_obligation__adaptation_priority_su_t2010, climate_response_obligation__adaptation_priority, suppression_requirement, 2010, 0.52).
narrative_ontology:measurement(climate_response_obligation__adaptation_priority_su_t2015, climate_response_obligation__adaptation_priority, suppression_requirement, 2015, 0.58).
narrative_ontology:measurement(climate_response_obligation__adaptation_priority_su_t2020, climate_response_obligation__adaptation_priority, suppression_requirement, 2020, 0.62).
narrative_ontology:measurement(climate_response_obligation__adaptation_priority_su_t2025, climate_response_obligation__adaptation_priority, suppression_requirement, 2025, 0.65).
narrative_ontology:measurement(climate_response_obligation__adaptation_priority_su_t2030, climate_response_obligation__adaptation_priority, suppression_requirement, 2030, 0.66).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_response_obligation__adaptation_priority, resource_allocation).
narrative_ontology:boltzmann_floor_override(climate_response_obligation__adaptation_priority, 0.15).
narrative_ontology:affects_constraint(climate_response_obligation__adaptation_priority, climate_finance_architecture).
narrative_ontology:affects_constraint(climate_response_obligation__adaptation_priority, loss_and_damage_governance).
narrative_ontology:affects_constraint(climate_response_obligation__adaptation_priority, fossil_fuel_subsidy_regime).
narrative_ontology:affects_constraint(climate_response_obligation__adaptation_priority, climate_migration_governance).

% DUAL FORMULATION NOTE:
% The climate_response_obligation kernel decomposes into three constraint stories: adaptation_priority (this), mitigation_priority, and degrowth_reading. They form a constraint family linked by affects_constraints. adaptation_priority and mitigation_priority coexist as live positions in climate governance; degrowth_reading is marginalized but structurally distinct. The ε values differ: adaptation_priority has high ε (0.72) because it accepts warming and transfers risk; mitigation_priority has lower ε (coordination-dominant); degrowth_reading has distinct ε (throughput reduction as coordination).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(climate_response_obligation__adaptation_priority, institutional, 0.15).
constraint_indexing:directionality_override(climate_response_obligation__adaptation_priority, powerless, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
