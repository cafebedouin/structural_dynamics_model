% ============================================================================
% CONSTRAINT STORY: climate_response_legitimacy__degrowth_transformation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_climate_response_legitimacy__degrowth_transformation, []).

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
 *   constraint_id: climate_response_legitimacy__degrowth_transformation
 *   human_readable: Degrowth Transformation for Climate Legitimacy
 *   domain: climate_policy/political_economy/intergenerational_ethics
 *
 * SUMMARY:
 *   This constraint instantiates the degrowth_transformation reading of the
 *   contested kernel 'climate_response_legitimacy.' The reading asserts that
 *   legitimate climate response requires dismantling the growth imperative in
 *   wealthy nations through structural transformation: universal basic
 *   services decoupling security from income, working time reduction
 *   redistributing work and consumption, democratic firm ownership replacing
 *   capital accumulation. The constraint treats this transformation as
 *   non-negotiable on ethical and physics grounds—current wealthy workers and
 *   capital owners in developed economies become cost-bearers (income loss,
 *   proprietary control) while future generations and climate-vulnerable
 *   populations become beneficiaries (avoided warming without technological
 *   dependency). The reading coexists with two siblings: mitigation_priority
 *   (growth-compatible decoupling via innovation and carbon pricing) and
 *   adaptation_priority (resilience infrastructure for impacts). Each reading
 *   instantiates a different legitimacy claim and thus a different
 *   constraint; this JSON describes only the degrowth reading and its
 *   structural properties.
 *
 * KEY AGENTS:
 *   - Future generations (powerless, trapped, benefit from avoided warming)
 *   - Climate vulnerable populations (powerless, trapped, benefit from emissions reduction)
 *   - Current wealthy workers in developed economies (moderate power, constrained exit, bear income/consumption costs)
 *   - Capital owners in developed economies (powerful, arbitrage exit, bear proprietary control costs)
 *   - Degrowth advocates (organized, mobile, set agenda via discourse and advocacy)
 *   - Incumbent growth coalition (institutional, arbitrage exit, suppress alternatives to growth narrative)
 *   - Developing economies (excluded from legitimacy framing, would object to asymmetry)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_response_legitimacy__degrowth_transformation, 0.68).
domain_priors:suppression_score(climate_response_legitimacy__degrowth_transformation, 0.71).
domain_priors:theater_ratio(climate_response_legitimacy__degrowth_transformation, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_response_legitimacy__degrowth_transformation, extractiveness, 0.68).
narrative_ontology:constraint_metric(climate_response_legitimacy__degrowth_transformation, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(climate_response_legitimacy__degrowth_transformation, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_response_legitimacy__degrowth_transformation, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(climate_response_legitimacy__degrowth_transformation, resistance, 0.74).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_response_legitimacy__degrowth_transformation, tangled_rope).
narrative_ontology:human_readable(climate_response_legitimacy__degrowth_transformation, "Degrowth Transformation for Climate Legitimacy").
narrative_ontology:topic_domain(climate_response_legitimacy__degrowth_transformation, "climate_policy/political_economy/intergenerational_ethics").

domain_priors:requires_active_enforcement(climate_response_legitimacy__degrowth_transformation).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_response_legitimacy__degrowth_transformation, 'c13bade6-5f34-4e0f-b721-cb33628f36d8').
narrative_ontology:cs_kernel_codification('c13bade6-5f34-4e0f-b721-cb33628f36d8', distributed).
narrative_ontology:cs_authority_grounding('c13bade6-5f34-4e0f-b721-cb33628f36d8', distributed).
narrative_ontology:cs_reading_relation('c13bade6-5f34-4e0f-b721-cb33628f36d8', climate_response_legitimacy__mitigation_priority, coexists_with).
narrative_ontology:cs_reading_relation('c13bade6-5f34-4e0f-b721-cb33628f36d8', climate_response_legitimacy__adaptation_priority, coexists_with).
narrative_ontology:cs_axiom('c13bade6-5f34-4e0f-b721-cb33628f36d8', foundational, growth_imperative_incompatible_with_survivable_climate).
narrative_ontology:cs_axiom_status(growth_imperative_incompatible_with_survivable_climate, holdable).
narrative_ontology:cs_axiom_grounding('c13bade6-5f34-4e0f-b721-cb33628f36d8', growth_imperative_incompatible_with_survivable_climate, empirically_contingent).
narrative_ontology:cs_axiom('c13bade6-5f34-4e0f-b721-cb33628f36d8', foundational, wealthy_nations_responsibility_for_historical_emissions_accumulation).
narrative_ontology:cs_axiom_status(wealthy_nations_responsibility_for_historical_emissions_accumulation, holdable).
narrative_ontology:cs_axiom_grounding('c13bade6-5f34-4e0f-b721-cb33628f36d8', wealthy_nations_responsibility_for_historical_emissions_accumulation, deontological).
narrative_ontology:cs_axiom('c13bade6-5f34-4e0f-b721-cb33628f36d8', secondary, structural_transformation_non_negotiable_for_legitimacy).
narrative_ontology:cs_axiom_status(structural_transformation_non_negotiable_for_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('c13bade6-5f34-4e0f-b721-cb33628f36d8', structural_transformation_non_negotiable_for_legitimacy, deontological).
narrative_ontology:cs_reference_frame('c13bade6-5f34-4e0f-b721-cb33628f36d8', survivable_climate_requires_wealthy_nation_structural_change).
narrative_ontology:cs_drift_state('c13bade6-5f34-4e0f-b721-cb33628f36d8', contemporary_2024_2026, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('c13bade6-5f34-4e0f-b721-cb33628f36d8', '').
narrative_ontology:cs_kernel_id(climate_response_legitimacy__degrowth_transformation, climate_response_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_response_legitimacy__degrowth_transformation, future_generations).
narrative_ontology:constraint_beneficiary(climate_response_legitimacy__degrowth_transformation, climate_vulnerable_populations).
narrative_ontology:constraint_victim(climate_response_legitimacy__degrowth_transformation, current_wealthy_workers).
narrative_ontology:constraint_victim(climate_response_legitimacy__degrowth_transformation, capital_owners_developed_nations).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(climate_response_legitimacy__degrowth_transformation, incumbent_growth_coalition).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Bear the consequences of current warming trajectory if transformation does not occur; benefit from structural decoupling of wellbeing from growth if the arrangement succeeds. Have no voice in current decision-making; their existence is premise rather than presence in negotiations.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__degrowth_transformation, future_generations, beneficiary,
    powerless, civilizational, trapped, global).

% Currently bear disproportionate costs of climate impacts (heat stress, agricultural collapse, displacement) despite minimal contribution to emissions. Benefit from emissions reduction that the degrowth reading treats as non-negotiable; their survival is the reading's moral anchor.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__degrowth_transformation, climate_vulnerable_populations, beneficiary,
    powerless, biographical, trapped, global).

% In developed economies, currently enjoy income and consumption levels premised on growth. The degrowth transformation requires income reduction (via working time reduction, reallocation to universal basic services) and loss of consumption growth. Their resistance is high because the cost is salient and immediate; exit means relocating to non-implementing jurisdictions or supporting political reversal.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__degrowth_transformation, current_wealthy_workers, payer,
    moderate, biographical, constrained, national).

% Own equity, real estate, and productive assets whose value is predicated on growth and reinvestment dynamics. The degrowth reading treats capital accumulation and firm ownership conversion as targets: democratic firm ownership reduces proprietary return, UBS provision and working-time reduction compress profit margins, and shifted investment to green infrastructure redirects capital flows. They have arbitrage exit via offshore relocation or financial engineering; their structural power allows suppression of alternatives to growth narrative.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__degrowth_transformation, capital_owners_developed_nations, payer,
    powerful, biographical, arbitrage, global).

% Propose and defend the structural transformation: they set the terms of the reading (what legitimacy requires), articulate the mechanisms (UBS, working time, democratic ownership), and carry the burden of defending political feasibility. They operate as a movement coalition, not a state apparatus; their enforcement power is limited to discourse and advocacy until (and if) democratically elected governments adopt their framing.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__degrowth_transformation, degrowth_advocates, agenda_setter,
    organized, generational, mobile, global).

% Central banks, corporate leadership, mainstream climate policy apparatus (IPCC-consensus mitigation via decoupling, carbon pricing regimes). They defend the growth-compatible framing of climate response and actively suppress alternatives to growth narrative through institutional legitimacy, media framing, and policy architecture. They bear costs only if degrowth ascends (reversal of their preferred agenda), not from its rejection.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__degrowth_transformation, incumbent_growth_coalition, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(climate_response_legitimacy__degrowth_transformation, incumbent_growth_coalition, payer).

% Would object that degrowth in wealthy nations is presented as climate legitimacy without addressing the structural inequality: wealthy economies have already accumulated growth benefits; degrowth in developed nations is proposed as the cost of historical responsibility, yet the constraint's framing centers wealthy-nation transformation rather than reparations or differentiated obligations. Their exclusion from the negotiation of 'legitimate response' reproduces the structural asymmetry that degrowth advocates claim to oppose.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__degrowth_transformation, developing_economies, excluded,
    moderate, biographical, constrained, global).

% Maps the structural relationships: who bears costs (current wealthy workers, capital owners), who benefits (future generations, vulnerable populations), who sets the terms (advocates and incumbents in collision), what is suppressed (alternative framings of climate legitimacy, feasibility questions, distributional questions within wealthy nations). Observes the constraint without stakes in its resolution.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__degrowth_transformation, analytical_observer, observer,
    analytical, generational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(climate_response_legitimacy__degrowth_transformation, diffuse).
narrative_ontology:fixing_cost_class(climate_response_legitimacy__degrowth_transformation, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the coordination problem of how wealthy economies can reduce emissions to physically survivable levels while maintaining material security and preventing justifiable resentment from those bearing the cost of transition. Degrowth treats this as inseparable from addressing growth-driven inequality and ecological overshoot: universal basic services decouple security from income, working time reduction shares available work and consumption, democratic firm ownership aligns incentives with ecological limits rather than accumulation.
% TRANSFER_FUNCTION: Transfers income, consumption capacity, and proprietary control from current-generation high earners and capital owners in developed economies to: universal basic services (healthcare, housing, education, transport as public goods), expanded non-commodified time (leisure, care, community engagement), democratic workplace governance (profit-sharing, cooperative ownership reducing concentrated capital return).
% ABSENT_VOICES: Developing economies and workers in them are systematically absent from this constraint's framing — their objection would be that degrowth in the North frames the cost as wealthy-nation sacrifice while remaining silent on reparations for historical extraction and on their development rights. Workers in developing economies who aspire to income growth comparable to their wealthy-nation counterparts have no seat in the degrowth reading's legitimacy claim. Moderate-left advocates who accept some growth-compatible decoupling (the mitigation_priority reading) are present but positioned as complicit with inadequacy.
% DISAPPEARANCE_RATIONALE: If the degrowth transformation as proposed were to vanish, wealthy economies would revert to growth-priority climate response (mitigation via decoupling, adaptation for the wealthy); emissions trajectories would remain severe; inequality would not address the accumulation asymmetry the constraint names; future generations would inherit a warmer world without the structural transformation that the constraint claims is the only legitimate response. The material constraints (resource availability, emissions budget) would not vanish, but the institutional and redistributive mechanisms the constraint proposes would not exist to manage them.
% FOUNDING_PROBLEM: Climate physics establishes a hard carbon budget; growth-as-usual exceeds it; wealthy economies have already accumulated most historical benefits of growth while imposing the cost on vulnerable populations and future generations. Legitimacy requires either: (a) accepting the cost falls on those who caused it (current wealthy nations), or (b) proposing that future generations and climate-vulnerable populations accept the cost for the benefit of wealthy-economy growth, which the degrowth reading treats as illegitimate. The founding problem is the collision between growth premises and physics/ethics.
% FOUNDING_PROBLEM_CORROBORATION: Climate physics (IPCC AR6, peer-reviewed emissions budgets) confirm the hard constraint. Ethical arguments about historical responsibility are attested by decolonial scholarship and climate justice movements outside the growth coalition. The founding problem is disputed by the incumbent growth coalition (which frames decoupling as feasible) and by adaptation-priority advocates (who prioritize resilience over transformation), but it is attested by climate scientists, Indigenous peoples, and climate justice advocates as independent voices corroborating that the problem persists.
narrative_ontology:disappearance_verdict(climate_response_legitimacy__degrowth_transformation, world_rearranges).
narrative_ontology:founding_problem_status(climate_response_legitimacy__degrowth_transformation, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_response_legitimacy__degrowth_transformation, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(climate_response_legitimacy__degrowth_transformation, 'none', 1).
narrative_ontology:epsilon_provenance(climate_response_legitimacy__degrowth_transformation, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(climate_response_legitimacy__degrowth_transformation_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(climate_response_legitimacy__degrowth_transformation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(climate_response_legitimacy__degrowth_transformation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises from 0.42 at t=0 (early degrowth discourse, limited institutional adoption) to plateau at 0.68-0.69 by t=25-40 (maturation toward policy adoption or clear infeasibility). The trajectory reflects the constraint's transition from a coherent ethical/scientific claim (low extraction when framed as shared sacrifice) to implementation stage where the transfer function becomes concrete: workers feel income reduction, capital owners see firm conversion, and the suppression machinery required to maintain the transformation becomes visible. Suppression requirement tracks extractiveness closely (0.44→0.71) because degrowth transformation demands active enforcement against the growth coalition's counter-pressure, against wealthy-worker resistance, and against developing-economy objections to the framing of legitimacy. Theater ratio remains low (0.08→0.29) because the constraint's function is substantive redistribution and structural change, not performative — the coordination function and the extraction are inseparable, unlike piton dynamics. The plateau in extractiveness and suppression after t=25 reflects uncertainty about whether the transformation succeeds (gains political power) or stalls (remains a coherent but politically infeasible claim); the measurement endpoints are tagged 'projected' to signal this horizon uncertainty.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats (wealthy workers, capital owners, incumbent growth coalition) should compute as experiencing a snare-like constraint (extraction without offsetting benefit, enforcement against their interest). The beneficiary seats (future generations, vulnerable populations, advocates for justice) should compute as coordinated—the constraint solves their primary problem (survival, equity). The excluded seat (developing economies) should compute as doubly constrained: they cannot opt out of warming, cannot opt into the legitimacy conversation, and would object that both wealthy-growth and wealthy-degrowth framings center wealthy-nation decision-making. The measurement trajectory (rising suppression, rising theater as implementation approaches) reflects institutional maturation: early-stage discourse has low enforcement cost, but actual transformation requires machinery to suppress growth coalition counter-pressure, constrain capital exit, and maintain working-time reduction against worker resistance.
 *
 * DIRECTIONALITY LOGIC:
 *   Future generations and vulnerable populations are structural beneficiaries (d near 0.0) because the constraint's operation is justified by their benefit and depends on their continued existence as the moral premise, yet their power is zero and exit is unavailable—their directionality is pure beneficiary despite powerlessness. Current wealthy workers are structural targets (d near 0.8-0.9) because they bear concrete income loss via working time reduction and service reallocation; their exit is constrained (mobility is possible but costly; political reversal is their real exit option). Capital owners are structural targets (d near 0.9) because firm ownership conversion and profit-margin compression directly extract proprietary value; their exit is arbitrage (offshore relocation, financial engineering) which temporarily avoids the constraint but does not eliminate its force. Degrowth advocates are agenda-setters with moderate power; they set the terms of what legitimacy requires but lack enforcement power (they are a movement, not a state). The incumbent growth coalition is dual-positioned: they are agenda-setters (they currently control policy architecture and media framing) and payers only if degrowth ascends (if they reverse policy, they bear reversal costs, but currently they bear none from rejecting the degrowth constraint).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (physics + ethics collision) is live and attested; the constraint addresses it directly by making the transformation the response. Mandatrophy risk is modest here: the constraint's function is genuinely coordinating (solving the bounded-carbon problem) even though it is extractive from wealthy-economy seats. The concern is not that the founding problem has expired (it has not) but that political feasibility collapse could render the constraint a live claim with zero enforceability—that would move it toward piton territory (maintained as ethical principle, not as operational constraint). The measurement plateau at t=25+ and the 'projected' basis for later points reflect exactly this uncertainty: if adoption accelerates, the constraint becomes active coordination/enforcement; if political infeasibility hardens, it becomes a contested ethical claim without power. No mandatrophy is authored yet, but the omega variables capture the feasibility risk.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    political_feasibility_collapse,
    'Can the degrowth transformation be implemented through democratic processes in wealthy nations without producing either violent backlash from wealthy-worker and capital-owner coalitions, or authoritarian enforcement?',
    'Historical experiment from jurisdictions that attempt comprehensive working-time reduction + UBS + firm democratization transitions. Observation of social stability, enforcement requirements, and whether reversals occur.',
    'If feasibility collapses (high backlash, authoritarian enforcement required, reversals are endemic), the constraint''s status drifts from live coordination to zombie claim—maintained as an ethical principle but unenforceable through democratic means. This would indicate piton dynamics (institutional maintenance without political will) or snare dynamics (coordination story covering coercive transformation). If feasibility holds, the constraint operates as claimed (tangled rope with beneficiaries external to the enforcement site).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(political_feasibility_collapse, empirical, 'Whether the structural transformation can be democratically sustained in the face of capital and worker resistance.').

omega_variable(
    developing_economy_structural_asymmetry,
    'Can the degrowth transformation in wealthy nations be decoupled from reparations/compensation to developing economies, or is the reading''s legitimacy claim only defensible if coupled with transfer mechanisms addressing accumulated historical inequality?',
    'Policy evolution in wealthy nations adopting degrowth elements. Observation of whether developing-economy movements accept the transformation as legitimate climate response, or whether they maintain that decoupling requires explicit reparations/development-rights accommodation.',
    'If reparations/development rights are treated as separate from climate response (degrowth in the North, growth in the South), developing-economy actors will remain excluded from legitimacy conversation and the constraint''s foundation remains contested. If degrowth advocates integrate reparations into the transformation''s legitimacy claim, the constraint''s beneficiary set expands and the extraction from wealthy-nation labor expands (some flows to reparations rather than UBS/working time). This represents a material redefinition of what the constraint transfers and to whom.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(developing_economy_structural_asymmetry, conceptual, 'Whether the degrowth reading''s legitimacy claim requires explicit addressing of North-South structural inequality or can be framed as purely climate-internal.').

omega_variable(
    capital_exit_enforcement_capacity,
    'Can wealthy-nation governments enforce firm democratization and capital restrictions against capital owners with global exit options (offshore banking, corporate relocation, financial engineering)?',
    'Observation of tax compliance, capital flight, and firm relocation in jurisdictions implementing UBS and working-time reduction. Measurement of whether capital owners comply with ownership conversion or successfully arbitrage out.',
    'If capital can arbitrage out (exit is genuinely available), the constraint''s extraction falls asymmetrically on workers and vulnerable populations within the nation, not on capital owners—this redefines the target set and makes the constraint snare-like (extraction from those trapped, not those with exit). If capital exit is suppressed (via capital controls, wealth taxation, firm nationalization), the enforcement machinery becomes visible and suppression requirement rises accordingly, moving the constraint toward tangled rope or snare depending on whether the coordination function justifies the enforcement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(capital_exit_enforcement_capacity, empirical, 'Whether the constraint can enforce capital ownership conversion against actors with arbitrage exit options.').

omega_variable(
    knowledge_accessibility_and_narrative_suppression,
    'To what degree does the constraint''s operation require suppression of mitigation_priority and adaptation_priority readings as competing framings of legitimacy?',
    'Institutional analysis of policy discourse: are alternative readings treated as live positions in legislative debate, academic research, and media coverage, or are they actively marginalized as incompatible with the degrowth framing?',
    'If suppression of alternatives is high and required for the constraint to maintain authority, the extracted value includes control over legitimacy discourse itself—what counts as a serious climate response. This would indicate the constraint is partly snare-like (extraction of epistemic authority) in addition to material extraction. If alternatives are allowed as live debate, the constraint operates more purely as a competing coordination claim.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(knowledge_accessibility_and_narrative_suppression, empirical, 'The degree to which this reading''s operation suppresses sibling readings from the legitimacy conversation.').

omega_variable(
    internalized_growth_identity_in_workers,
    'To what degree is the wealthy-worker resistance to income reduction structural (loss of actual material security) versus internalized (identification with growth-trajectory status and consumption markers)?',
    'Post-implementation observation in jurisdictions with working-time reduction and UBS: measure whether worker resistance persists after material security is established via UBS, or whether it abates once the identity-threat component is resolved through community engagement and status reframing.',
    'If suppression is largely structural (material loss is real), the constraint''s extractiveness is accurately measured and suppression is a necessary feature of enforcing the transfer. If suppression is largely internalized (workers believe they are impoverished despite material security, due to identity-loss), the constraint''s enforcement machinery must address not just material transfer but identity reconstruction—this raises the theater ratio and suppression requirement, moving toward piton or coercive enforcement. If both are present, the constraint''s enforcement cost includes both structural compensation and identity work.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(internalized_growth_identity_in_workers, empirical, 'Whether worker resistance to degrowth is rooted in material loss or in internalized growth-identity fusion.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_response_legitimacy__degrowth_transformation, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clim_tr_t0, climate_response_legitimacy__degrowth_transformation, theater_ratio, 0, 0.08).
narrative_ontology:measurement_basis(clim_tr_t0, observed).
narrative_ontology:measurement(clim_tr_t5, climate_response_legitimacy__degrowth_transformation, theater_ratio, 5, 0.12).
narrative_ontology:measurement_basis(clim_tr_t5, observed).
narrative_ontology:measurement(clim_tr_t10, climate_response_legitimacy__degrowth_transformation, theater_ratio, 10, 0.16).
narrative_ontology:measurement_basis(clim_tr_t10, observed).
narrative_ontology:measurement(clim_tr_t15, climate_response_legitimacy__degrowth_transformation, theater_ratio, 15, 0.22).
narrative_ontology:measurement_basis(clim_tr_t15, observed).
narrative_ontology:measurement(clim_tr_t20, climate_response_legitimacy__degrowth_transformation, theater_ratio, 20, 0.26).
narrative_ontology:measurement_basis(clim_tr_t20, projected).
narrative_ontology:measurement(clim_tr_t25, climate_response_legitimacy__degrowth_transformation, theater_ratio, 25, 0.28).
narrative_ontology:measurement_basis(clim_tr_t25, projected).
narrative_ontology:measurement(clim_tr_t30, climate_response_legitimacy__degrowth_transformation, theater_ratio, 30, 0.29).
narrative_ontology:measurement_basis(clim_tr_t30, projected).
narrative_ontology:measurement(clim_tr_t40, climate_response_legitimacy__degrowth_transformation, theater_ratio, 40, 0.28).
narrative_ontology:measurement_basis(clim_tr_t40, projected).

% Extraction over time
narrative_ontology:measurement(clim_be_t0, climate_response_legitimacy__degrowth_transformation, base_extractiveness, 0, 0.42).
narrative_ontology:measurement_basis(clim_be_t0, observed).
narrative_ontology:measurement(clim_be_t5, climate_response_legitimacy__degrowth_transformation, base_extractiveness, 5, 0.48).
narrative_ontology:measurement_basis(clim_be_t5, observed).
narrative_ontology:measurement(clim_be_t10, climate_response_legitimacy__degrowth_transformation, base_extractiveness, 10, 0.54).
narrative_ontology:measurement_basis(clim_be_t10, observed).
narrative_ontology:measurement(clim_be_t15, climate_response_legitimacy__degrowth_transformation, base_extractiveness, 15, 0.61).
narrative_ontology:measurement_basis(clim_be_t15, observed).
narrative_ontology:measurement(clim_be_t20, climate_response_legitimacy__degrowth_transformation, base_extractiveness, 20, 0.66).
narrative_ontology:measurement_basis(clim_be_t20, projected).
narrative_ontology:measurement(clim_be_t25, climate_response_legitimacy__degrowth_transformation, base_extractiveness, 25, 0.68).
narrative_ontology:measurement_basis(clim_be_t25, projected).
narrative_ontology:measurement(clim_be_t30, climate_response_legitimacy__degrowth_transformation, base_extractiveness, 30, 0.69).
narrative_ontology:measurement_basis(clim_be_t30, projected).
narrative_ontology:measurement(clim_be_t40, climate_response_legitimacy__degrowth_transformation, base_extractiveness, 40, 0.68).
narrative_ontology:measurement_basis(clim_be_t40, projected).

% Suppression requirement over time
narrative_ontology:measurement(clim_su_t0, climate_response_legitimacy__degrowth_transformation, suppression_requirement, 0, 0.44).
narrative_ontology:measurement_basis(clim_su_t0, observed).
narrative_ontology:measurement(clim_su_t5, climate_response_legitimacy__degrowth_transformation, suppression_requirement, 5, 0.52).
narrative_ontology:measurement_basis(clim_su_t5, observed).
narrative_ontology:measurement(clim_su_t10, climate_response_legitimacy__degrowth_transformation, suppression_requirement, 10, 0.59).
narrative_ontology:measurement_basis(clim_su_t10, observed).
narrative_ontology:measurement(clim_su_t15, climate_response_legitimacy__degrowth_transformation, suppression_requirement, 15, 0.65).
narrative_ontology:measurement_basis(clim_su_t15, observed).
narrative_ontology:measurement(clim_su_t20, climate_response_legitimacy__degrowth_transformation, suppression_requirement, 20, 0.69).
narrative_ontology:measurement_basis(clim_su_t20, projected).
narrative_ontology:measurement(clim_su_t25, climate_response_legitimacy__degrowth_transformation, suppression_requirement, 25, 0.71).
narrative_ontology:measurement_basis(clim_su_t25, projected).
narrative_ontology:measurement(clim_su_t30, climate_response_legitimacy__degrowth_transformation, suppression_requirement, 30, 0.72).
narrative_ontology:measurement_basis(clim_su_t30, projected).
narrative_ontology:measurement(clim_su_t40, climate_response_legitimacy__degrowth_transformation, suppression_requirement, 40, 0.71).
narrative_ontology:measurement_basis(clim_su_t40, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_response_legitimacy__degrowth_transformation, resource_allocation).
narrative_ontology:boltzmann_floor_override(climate_response_legitimacy__degrowth_transformation, 0.22).
narrative_ontology:affects_constraint(climate_response_legitimacy__degrowth_transformation, climate_response_legitimacy__mitigation_priority).
narrative_ontology:affects_constraint(climate_response_legitimacy__degrowth_transformation, climate_response_legitimacy__adaptation_priority).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the contested kernel 'climate_response_legitimacy.' The kernel represents a live dispute about what constitutes legitimate climate response: degrowth_transformation (this file) claims legitimacy requires structural economic change in wealthy nations; mitigation_priority claims legitimacy allows growth-compatible decoupling; adaptation_priority claims legitimacy centers resilience. Each reading instantiates a distinct constraint with different beneficiary/victim structures, different ε values, and different claims on what the founding problem requires. They coexist as positions held by different institutional/social actors—no single framework holds all three simultaneously. They are linked via network.affects_constraints because adoption of one reading creates institutional and discursive pressure on the others (mitigation_priority's success would marginalize degrowth advocacy; degrowth ascendance would constrain mitigation policy space). The ε-invariance principle requires separate stories: ε for degrowth is high (0.68 at maturity) because it frames the transformation as non-negotiable on ethical/physics grounds; ε for mitigation would be lower (growth-compatible measures with market mechanisms) because extraction is justified as transition cost; ε for adaptation would be different still (resilience infrastructure as public good). Each reading's beneficiary set differs: degrowth centers future generations and vulnerable populations, mitigation centers global benefits from growth continuation, adaptation centers those protected by resilience systems. The readings are not alternative measurements of a single constraint—they are structurally distinct constraints united by their common kernel text (what is 'legitimate climate response').

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(climate_response_legitimacy__degrowth_transformation, powerful, 0.92).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
