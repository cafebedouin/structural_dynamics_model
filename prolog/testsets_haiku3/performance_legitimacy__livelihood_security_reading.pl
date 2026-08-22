% ============================================================================
% CONSTRAINT STORY: performance_legitimacy__livelihood_security_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_performance_legitimacy__livelihood_security_reading, []).

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
 *   constraint_id: performance_legitimacy__livelihood_security_reading
 *   human_readable: Performance Legitimacy via Livelihood Security (Service Delivery Reading)
 *   domain: political_economy/development_planning/state_capitalism
 *
 * SUMMARY:
 *   The livelihood security reading of performance legitimacy grounds state
 *   authority in delivering tangible improvements in citizens' daily lives —
 *   expanded healthcare access, education quality, elderly care support,
 *   employment in service sectors, and consumption security. This is ONE
 *   reading of a contested kernel (performance_legitimacy). The regime
 *   actively channels resources toward these visible services rather than
 *   capital-intensive industrial expansion, technological self-sufficiency,
 *   or raw GDP growth. The constraint redistributes from capital-intensive
 *   sectors and infrastructure projects toward service workers and household
 *   consumption. It requires active enforcement: suppressing competing
 *   legitimacy narratives (growth, innovation, nationalist capability)
 *   through budget control and institutional pressure. The constraint sits on
 *   the boundary between genuine coordination (the state solves the problem
 *   of grounding authority through material security) and asymmetric
 *   extraction (redistribution that harms capital sectors and infrastructure
 *   investment creates predictable losers). Structurally, this is a tangled
 *   rope: it solves a real coordination problem (legitimacy crisis) while
 *   simultaneously extracting from specific sectors and benefiting others.
 *   The measurements show extractiveness rising and then plateauing — initial
 *   acceleration as the regime commits to service-sector prioritization, then
 *   stabilization as the new equilibrium holds.
 *
 * KEY AGENTS:
 *   - regime_authority: institutional power, civilizational horizon — sets and enforces the livelihood-security prioritization; supplies the enforcement that suppresses competing narratives and redirects budget
 *   - service_sector_workers: moderate power, biographical horizon — direct beneficiaries; employment grows as healthcare, education, elderly care expand; identity fuses with the regime's legitimacy narrative
 *   - household_consumption_base: powerless, biographical horizon — primary beneficiary; experiences material security directly through expanded healthcare, education, elderly care; trapped by dependence on state provision
 *   - urban_middle_class: organized power, generational horizon — beneficiary of service-sector employment and visible amenities; partly identity-locked by professional status and service provision narratives
 *   - capital_intensive_industry: organized power, generational horizon — primary victim; state de-prioritizes industrial expansion and infrastructure investment; constrained exit due to embedded supply chains and labor markets
 *   - local_government_infrastructure: institutional power, generational horizon — victim through budget constraints; under-resourced for long-term capital projects while pressured to deliver visible service outcomes locally
 *   - export_oriented_manufacturing: organized power, generational horizon — victim through underinvestment in export-support infrastructure and labor competition from service sectors; higher exit costs for multinational supply chains
 *   - competing_growth_advocates: organized power, generational horizon — excluded from budget decisions and narrative authority; argue livelihood security is unsustainable without high-capital investment and technological dominance
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(performance_legitimacy__livelihood_security_reading, 0.62).
domain_priors:suppression_score(performance_legitimacy__livelihood_security_reading, 0.41).
domain_priors:theater_ratio(performance_legitimacy__livelihood_security_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(performance_legitimacy__livelihood_security_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(performance_legitimacy__livelihood_security_reading, suppression_requirement, 0.41).
narrative_ontology:constraint_metric(performance_legitimacy__livelihood_security_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(performance_legitimacy__livelihood_security_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(performance_legitimacy__livelihood_security_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(performance_legitimacy__livelihood_security_reading, tangled_rope).
narrative_ontology:human_readable(performance_legitimacy__livelihood_security_reading, "Performance Legitimacy via Livelihood Security (Service Delivery Reading)").
narrative_ontology:topic_domain(performance_legitimacy__livelihood_security_reading, "political_economy/development_planning/state_capitalism").

domain_priors:requires_active_enforcement(performance_legitimacy__livelihood_security_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(performance_legitimacy__livelihood_security_reading, '5757536e-862e-48a3-a625-3db90352d6fd').
narrative_ontology:cs_kernel_codification('5757536e-862e-48a3-a625-3db90352d6fd', formalized).
narrative_ontology:cs_authority_grounding('5757536e-862e-48a3-a625-3db90352d6fd', extraction).
narrative_ontology:cs_interpretation_layer_present('5757536e-862e-48a3-a625-3db90352d6fd').
narrative_ontology:cs_reading_relation('5757536e-862e-48a3-a625-3db90352d6fd', performance_legitimacy__quantitative_growth_reading, coexists_with).
narrative_ontology:cs_reading_relation('5757536e-862e-48a3-a625-3db90352d6fd', performance_legitimacy__qualitative_development_reading, influences).
narrative_ontology:cs_reading_relation('5757536e-862e-48a3-a625-3db90352d6fd', performance_legitimacy__techno_nationalist_reading, coexists_with).
narrative_ontology:cs_axiom('5757536e-862e-48a3-a625-3db90352d6fd', foundational, legitimacy_grounded_in_experienced_material_security).
narrative_ontology:cs_axiom_status(legitimacy_grounded_in_experienced_material_security, holdable).
narrative_ontology:cs_axiom_grounding('5757536e-862e-48a3-a625-3db90352d6fd', legitimacy_grounded_in_experienced_material_security, deontological).
narrative_ontology:cs_axiom('5757536e-862e-48a3-a625-3db90352d6fd', foundational, service_delivery_and_consumption_support_prioritized_over_capital_investment).
narrative_ontology:cs_axiom_status(service_delivery_and_consumption_support_prioritized_over_capital_investment, holdable).
narrative_ontology:cs_axiom_grounding('5757536e-862e-48a3-a625-3db90352d6fd', service_delivery_and_consumption_support_prioritized_over_capital_investment, instrumental).
narrative_ontology:cs_reference_frame('5757536e-862e-48a3-a625-3db90352d6fd', performance_legitimacy_through_livelihood_security).
narrative_ontology:cs_drift_state('5757536e-862e-48a3-a625-3db90352d6fd', contemporary_post_pandemic_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('5757536e-862e-48a3-a625-3db90352d6fd', '').
narrative_ontology:cs_kernel_id(performance_legitimacy__livelihood_security_reading, performance_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(performance_legitimacy__livelihood_security_reading, service_sector_workers).
narrative_ontology:constraint_beneficiary(performance_legitimacy__livelihood_security_reading, household_consumption_base).
narrative_ontology:constraint_beneficiary(performance_legitimacy__livelihood_security_reading, urban_middle_class).
narrative_ontology:constraint_victim(performance_legitimacy__livelihood_security_reading, capital_intensive_industry).
narrative_ontology:constraint_victim(performance_legitimacy__livelihood_security_reading, local_government_infrastructure_programs).
narrative_ontology:constraint_victim(performance_legitimacy__livelihood_security_reading, export_oriented_manufacturing).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(performance_legitimacy__livelihood_security_reading, urban_middle_class).
narrative_ontology:constraint_vindicates(performance_legitimacy__livelihood_security_reading, social_stability_requires_material_security).
narrative_ontology:constraint_vindicates(performance_legitimacy__livelihood_security_reading, direct_experience_legitimates_authority).
narrative_ontology:constraint_vindicates(performance_legitimacy__livelihood_security_reading, service_delivery_justifies_state_power).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Healthcare workers, educators, elderly care staff, social service providers. Benefit from state commitment to expand and fund these sectors as demonstration of legitimacy. Employment grows because the regime prioritizes visible, citizen-facing service delivery. Their livelihoods depend on continued state investment in their sectors; their work is framed as the proof of regime performance.
narrative_ontology:constraint_stakeholder(performance_legitimacy__livelihood_security_reading, service_sector_workers, beneficiary,
    moderate, biographical, constrained, national).

% Low- and middle-income households. Direct beneficiaries of expanded healthcare access, education availability, elderly care support, and subsidized basic services. The constraint channels state resources toward immediate material security and visible consumption support rather than capital investment. They experience the legitimacy claim directly through their daily lives.
narrative_ontology:constraint_stakeholder(performance_legitimacy__livelihood_security_reading, household_consumption_base, beneficiary,
    powerless, biographical, trapped, national).

% Urban professionals, administrators, service sector managers. Benefit from stable employment in expanded health, education, and care sectors. Also benefit from state resources directed toward visible urban amenities and services that demonstrate regime competence. Their exit options exist (emigration, private provision) but are costly; they are partly locked by status attachment to the service-provision narrative.
narrative_ontology:constraint_stakeholder(performance_legitimacy__livelihood_security_reading, urban_middle_class, beneficiary,
    organized, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(performance_legitimacy__livelihood_security_reading, urban_middle_class, payer).

% Heavy manufacturing, industrial base, resource extraction. State budget constraints force reallocation from capital investment and industrial subsidies toward service delivery. The regime de-prioritizes large infrastructure projects, industrial expansion, and high-capital-intensity development in favor of immediate consumption support. These sectors see lower state support and fiercer competition for capital budget allocation.
narrative_ontology:constraint_stakeholder(performance_legitimacy__livelihood_security_reading, capital_intensive_industry, payer,
    organized, generational, constrained, global).

% Regional and municipal governments tasked with construction, industrial parks, transportation networks, urban expansion. The constraint prioritizes central budget toward service sectors, leaving local governments with insufficient funds for capital projects and infrastructure maintenance. They face pressure to demonstrate service delivery locally while lacking resources for long-term structural development.
narrative_ontology:constraint_stakeholder(performance_legitimacy__livelihood_security_reading, local_government_infrastructure_programs, payer,
    institutional, generational, trapped, regional).

% Export-focused manufacturing enterprises, multinational supply chains, firms dependent on infrastructure and investment. Disadvantaged by state budget reallocation toward service delivery and consumption support. Competition for capital, skilled labor directed to service sectors, and underinvestment in export-support infrastructure raise operational costs and reduce competitiveness. Higher exit costs for firms deeply embedded in local labor markets.
narrative_ontology:constraint_stakeholder(performance_legitimacy__livelihood_security_reading, export_oriented_manufacturing, payer,
    organized, generational, mobile, global).

% State apparatus that sets and enforces the performance legitimacy framework. Chooses to ground legitimacy in visible service delivery and livelihood security rather than GDP growth or technological leadership. Enforces budget allocation priorities and regulatory pressure directing resources toward healthcare, education, elderly care, and consumption support. Actively suppresses competing legitimacy narratives (growth-focused, innovation-focused, nationalist) through budget control, propaganda, and institutional incentives.
narrative_ontology:constraint_stakeholder(performance_legitimacy__livelihood_security_reading, regime_authority, agenda_setter,
    institutional, civilizational, analytical, national).

% Industrial planners, infrastructure developers, techno-nationalist elites, economists championing high-speed growth or technological dominance. Structurally excluded from resource allocation decisions and budget priorities. They argue that livelihood security is unsustainable without high-capital-intensity development and that long-term legitimacy requires technological leadership or geopolitical capability. Their frameworks are marginalized by the regime's prioritization of direct service delivery.
narrative_ontology:constraint_stakeholder(performance_legitimacy__livelihood_security_reading, competing_growth_advocates, excluded,
    organized, generational, constrained, national).

% Foreign analysts, development economists, human-rights monitors, international organizations. Assess the constraint's operation and legitimacy grounding. They measure whether visible service delivery actually delivers material security and whether the livelihood security reading is a genuine shift in resource allocation or performative redistribution that masks persistent extraction.
narrative_ontology:constraint_stakeholder(performance_legitimacy__livelihood_security_reading, international_observers, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(performance_legitimacy__livelihood_security_reading, service_sector_workers).
narrative_ontology:fixing_cost_class(performance_legitimacy__livelihood_security_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the coordination problem of legitimacy grounding: the state must justify its authority and power through something citizens experience directly; this reading channels state capacity toward services and security visible in daily life rather than abstract metrics (GDP) or future capability (technology).
% TRANSFER_FUNCTION: Moves state budget resources from capital-intensive industrial expansion and infrastructure toward healthcare, education, elderly care, and consumption subsidies. Redistributes wealth from capital-holding industrial classes and export sectors toward service workers and low-income households. Moves time and attention of state apparatus from long-term industrial planning toward immediate service delivery and safety-net maintenance.
% ABSENT_VOICES: Growth-focused planners, industrial elites, techno-nationalists, and export-sector champions are structurally excluded from the constraint's operation. They would object that livelihood security without high-capital investment is a path to stagnation and geopolitical decline; they are prevented from shaping budget priorities and legitimacy narratives by the same mechanism that establishes service-delivery primacy.
% DISAPPEARANCE_RATIONALE: If livelihood security ceased to ground legitimacy and service delivery was de-prioritized, the state would redirect resources toward industrial expansion, infrastructure megaprojects, and technological investment; employment in service sectors would contract; safety nets would weaken; citizens would experience material insecurity in daily life; the regime's legitimacy claim would shift to growth metrics or nationalist capability. The entire budget allocation, sector priorities, and class relationships would reorganize around a different organizing principle.
% FOUNDING_PROBLEM: Post-industrial societies and middle-income developing states faced legitimacy crises when GDP growth alone failed to produce broad-based material security. Growth could concentrate in capital sectors while service accessibility collapsed for majority populations, eroding the regime's claim to deliver on the implicit social contract. The founding problem: how to ground state authority in something that majority citizens directly experience as improving their lives, rather than relying on aggregate growth statistics.
% FOUNDING_PROBLEM_CORROBORATION: Independent development analysts and social-movement research document persistent service deficits in healthcare, education, and elderly care creating legitimacy crises across middle-income states. Regime internal documentation (budget reviews, planning meetings) records explicit choices to prioritize service delivery as a legitimacy strategy. Opposition movements demand exactly this reorientation (expanded healthcare, education funding, elderly support) as evidence of regime failure when it is not delivered. The founding problem remains live as long as states face pressure to justify authority through material security in daily life.
narrative_ontology:disappearance_verdict(performance_legitimacy__livelihood_security_reading, world_rearranges).
narrative_ontology:founding_problem_status(performance_legitimacy__livelihood_security_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(performance_legitimacy__livelihood_security_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(performance_legitimacy__livelihood_security_reading, 'none', 1).
narrative_ontology:epsilon_provenance(performance_legitimacy__livelihood_security_reading, 0.62, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(performance_legitimacy__livelihood_security_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(performance_legitimacy__livelihood_security_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(performance_legitimacy__livelihood_security_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness at 0.62 reflects the constraint's asymmetric redistribution: resources flow from capital sectors to service delivery, a structural transfer that harms industrial actors regardless of efficiency arguments. Suppression at 0.41 is moderate because the regime must actively block competing legitimacy narratives (growth, innovation, nationalism) to maintain service-delivery prioritization — institutional pressure, budget control, propaganda maintain the constraint, but the suppression is not extreme because service delivery has genuine constituency support (beneficiaries accept the framing). Theater at 0.28 is relatively low because service delivery is materially real — healthcare clinics exist, teachers are employed, elderly receive care — but theaters begin when the regime claims perfect service delivery amid persistent gaps or when media coverage inflates small improvements. Accessibility collapse at 0.48: alternatives to state service provision exist (private healthcare, private education) but are expensive and available only to higher-income groups; for powerless and moderate-power agents, alternatives have largely collapsed once the service-provision framing is internalized. Resistance at 0.58 reflects real pushback: industrial lobbies resist budget reallocation, export sectors resist infrastructure underinvestment, local governments resist central capture of their budgets, and growth-focused economists argue the strategy is unsustainable. The measurement series show extractiveness and suppression rising together in the interval, then stabilizing — acceleration phase as the regime commits to the new framing, then stabilization as the equilibrium holds and competing narratives are sufficiently suppressed. Theater rises and stabilizes as performance-management systems mature around service metrics.
 *
 * PERSPECTIVAL GAP:
 *   From the regime's and beneficiary seats, this is genuine coordination: the state solves a legitimacy crisis by visibly delivering on material security, and the population experiences real improvements in their lives. From the industrial and infrastructure seats, the same constraint is enforced extraction: resources redirected from productive investment sectors to consumption support, creating long-term growth constraints and competitive decline. The urban middle class sits at the boundary — genuine beneficiary of service-sector employment and visible amenities, but also aware that underinvestment in capital infrastructure may undermine long-term competitiveness and their children's prospects. The engine computes directionality separately for each seat from the structural data; the divergence between how the regime experiences this as coordination and how industrial actors experience it as extraction is exactly what per-seat classification captures.
 *
 * DIRECTIONALITY LOGIC:
 *   Service-sector workers and household consumption: low d (toward beneficiary), because they receive resource flows and the constraint subsidizes their material security. Urban middle class: d near 0.4 (slightly beneficial) because they gain employment and amenities but face future constraints from underinvestment. Capital-intensive industry: high d (toward target), because state de-prioritizes their sectors and redirects budgets away from industrial expansion. Local government infrastructure: high d (toward target), because central budget capture leaves them underfunded for capital projects. Export manufacturing: high d (toward target), because infrastructure underinvestment and labor reallocation raise their costs. Competing-growth advocates: excluded from the directionality calculus entirely; they are shut out of resource allocation and narrative authority by the same constraint mechanism that establishes service-delivery primacy. Regime authority: d toward beneficiary (0.05) because the constraint is the regime's chosen legitimacy strategy and enables continued authority; exit is analytical, not real.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (legitimacy crisis when growth fails to deliver material security) remains live. The constraint solves it through genuine service delivery. Mandatrophy is not evident in the authored metrics because the founding problem has not outlived the constraint's function — material security delivery remains a live source of legitimacy grounding. However, the measurement series shows potential decay: if extractiveness plateaus and theater rises (performance management inflates metrics without real service delivery), mandatrophy could emerge later in the interval. The divergence between the claimed type (tangled_rope: coordination + extraction) and potential engine computation reflects the underlying structural contest: the constraint genuinely coordinates around legitimacy grounding while simultaneously extracting from industrial sectors. This is exactly tangled_rope's profile.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    service_delivery_sustainability,
    'Can livelihood security delivered through service-sector expansion and consumption support be sustained without capital-intensive industrial investment and export-oriented growth?',
    'Long-term fiscal trajectory: if state revenues remain stable or decline while service sectors expand, budget pressure forces eventual reallocation or service contraction. If alternative revenue sources (resource extraction, foreign investment, debt) emerge, sustainability holds longer.',
    'If unsustainable, the constraint faces mandatrophy: the founding problem (legitimacy crisis) re-emerges when service quality declines due to fiscal exhaustion, and the regime must choose between reverting to growth prioritization or radical restructuring. If sustainable, the constraint persists as a stable equilibrium.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(service_delivery_sustainability, empirical, 'Whether service-delivery-focused legitimacy grounding remains fiscally viable long-term without capital investment.').

omega_variable(
    beneficiary_identity_lock_depth,
    'How deeply identity-fused are service-sector workers and urban middle-class beneficiaries to the livelihood-security narrative? Would their exit remain constrained if suppression weakened?',
    'Post-reallocation scenario: if the regime shifts toward growth prioritization and reduces service investment, do beneficiary groups actively resist (identity-locked suppression), passively accommodate (exit options), or organize opposition (resistance mobilization)?',
    'High identity-lock would indicate the suppression measured here is partly internalized (targets believe the narrative legitimacy is true); if externalized-only, suppression would weaken quickly if enforcement relaxed. Identity lock increases constraint persistence independent of regime enforcement.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(beneficiary_identity_lock_depth, conceptual, 'Whether beneficiary groups'' identification with service-delivery legitimacy is structurally internalized or enforcement-dependent.').

omega_variable(
    competing_narratives_suppression_mechanism,
    'What is the primary mechanism suppressing growth-focused and techno-nationalist competing narratives? Budget control, institutional incentives, propaganda, or identity fusion within the regime itself?',
    'Regime internal analysis, policy reversals, or dissent emergence: if growth advocates resurface during fiscal stress, budget control is the primary mechanism; if they remain suppressed even when resources become available, institutional identity or propaganda is dominant.',
    'If budget control is primary, the constraint''s stability depends on resource scarcity; if institutional identity or propaganda is dominant, the constraint may persist through resource cycles. Directs remedies toward either fiscal restructuring or narrative contestation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(competing_narratives_suppression_mechanism, empirical, 'Whether suppression of competing legitimacy narratives operates through budget leverage, institutional culture, or propaganda.').

omega_variable(
    redistribution_genuine_vs_performance,
    'Does the constraint represent genuine redistribution of resources and power toward service sectors and household consumption, or is it performative redistribution (visible allocation to service sectors while actual extraction mechanisms persist underground)?',
    'Fiscal audit and service-quality measurement: compare budgeted allocations to service sectors versus actual spending; measure whether service quality, access, and outcomes improve proportionally to claimed budget increases.',
    'Genuine redistribution increases beneficiary identification and reduces theater; performative redistribution increases theater_ratio and sets up later mandatrophy (founding problem re-emerges when material security fails to materialize). Theater trajectory in measurements provides initial signal.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(redistribution_genuine_vs_performance, empirical, 'Whether service-sector prioritization is genuine resource redistribution or performative allocation masking persistent extraction.').

omega_variable(
    kernel_reading_stability_under_shock,
    'If external crisis (financial shock, pandemic, geopolitical pressure) forces temporary reallocation away from service delivery, does the livelihood_security reading survive re-commitment afterward, or does the regime permanently shift toward competing readings (quantitative_growth, techno_nationalist)?',
    'Post-shock budget and narrative tracking: compare regime''s legitimacy claims before and after crisis; measure whether service-delivery prioritization is restored or replaced by alternative performance metrics.',
    'If the reading survives shocks, it is institutionally robust; if shocks cause permanent shifts, the reading is contextually contingent on stable external conditions. Directs assessment of constraint durability.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_stability_under_shock, empirical, 'Whether the livelihood-security reading of performance legitimacy is institutionally stable or contingent on external conditions.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(performance_legitimacy__livelihood_security_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(perf_liv_sec_tr_t0, performance_legitimacy__livelihood_security_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement_basis(perf_liv_sec_tr_t0, observed).
narrative_ontology:measurement(perf_liv_sec_tr_t5, performance_legitimacy__livelihood_security_reading, theater_ratio, 5, 0.21).
narrative_ontology:measurement_basis(perf_liv_sec_tr_t5, observed).
narrative_ontology:measurement(perf_liv_sec_tr_t10, performance_legitimacy__livelihood_security_reading, theater_ratio, 10, 0.24).
narrative_ontology:measurement_basis(perf_liv_sec_tr_t10, observed).
narrative_ontology:measurement(perf_liv_sec_tr_t15, performance_legitimacy__livelihood_security_reading, theater_ratio, 15, 0.27).
narrative_ontology:measurement_basis(perf_liv_sec_tr_t15, observed).
narrative_ontology:measurement(perf_liv_sec_tr_t20, performance_legitimacy__livelihood_security_reading, theater_ratio, 20, 0.28).
narrative_ontology:measurement_basis(perf_liv_sec_tr_t20, observed).
narrative_ontology:measurement(perf_liv_sec_tr_t25, performance_legitimacy__livelihood_security_reading, theater_ratio, 25, 0.28).
narrative_ontology:measurement_basis(perf_liv_sec_tr_t25, observed).

% Extraction over time
narrative_ontology:measurement(perf_liv_sec_be_t0, performance_legitimacy__livelihood_security_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement_basis(perf_liv_sec_be_t0, observed).
narrative_ontology:measurement(perf_liv_sec_be_t5, performance_legitimacy__livelihood_security_reading, base_extractiveness, 5, 0.53).
narrative_ontology:measurement_basis(perf_liv_sec_be_t5, observed).
narrative_ontology:measurement(perf_liv_sec_be_t10, performance_legitimacy__livelihood_security_reading, base_extractiveness, 10, 0.58).
narrative_ontology:measurement_basis(perf_liv_sec_be_t10, observed).
narrative_ontology:measurement(perf_liv_sec_be_t15, performance_legitimacy__livelihood_security_reading, base_extractiveness, 15, 0.61).
narrative_ontology:measurement_basis(perf_liv_sec_be_t15, observed).
narrative_ontology:measurement(perf_liv_sec_be_t20, performance_legitimacy__livelihood_security_reading, base_extractiveness, 20, 0.62).
narrative_ontology:measurement_basis(perf_liv_sec_be_t20, observed).
narrative_ontology:measurement(perf_liv_sec_be_t25, performance_legitimacy__livelihood_security_reading, base_extractiveness, 25, 0.62).
narrative_ontology:measurement_basis(perf_liv_sec_be_t25, observed).

% Suppression requirement over time
narrative_ontology:measurement(perf_liv_sec_su_t0, performance_legitimacy__livelihood_security_reading, suppression_requirement, 0, 0.32).
narrative_ontology:measurement_basis(perf_liv_sec_su_t0, observed).
narrative_ontology:measurement(perf_liv_sec_su_t5, performance_legitimacy__livelihood_security_reading, suppression_requirement, 5, 0.36).
narrative_ontology:measurement_basis(perf_liv_sec_su_t5, observed).
narrative_ontology:measurement(perf_liv_sec_su_t10, performance_legitimacy__livelihood_security_reading, suppression_requirement, 10, 0.39).
narrative_ontology:measurement_basis(perf_liv_sec_su_t10, observed).
narrative_ontology:measurement(perf_liv_sec_su_t15, performance_legitimacy__livelihood_security_reading, suppression_requirement, 15, 0.41).
narrative_ontology:measurement_basis(perf_liv_sec_su_t15, observed).
narrative_ontology:measurement(perf_liv_sec_su_t20, performance_legitimacy__livelihood_security_reading, suppression_requirement, 20, 0.41).
narrative_ontology:measurement_basis(perf_liv_sec_su_t20, observed).
narrative_ontology:measurement(perf_liv_sec_su_t25, performance_legitimacy__livelihood_security_reading, suppression_requirement, 25, 0.41).
narrative_ontology:measurement_basis(perf_liv_sec_su_t25, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(performance_legitimacy__livelihood_security_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(performance_legitimacy__livelihood_security_reading, 0.18).
narrative_ontology:affects_constraint(performance_legitimacy__livelihood_security_reading, performance_legitimacy__quantitative_growth_reading).
narrative_ontology:affects_constraint(performance_legitimacy__livelihood_security_reading, performance_legitimacy__qualitative_development_reading).
narrative_ontology:affects_constraint(performance_legitimacy__livelihood_security_reading, performance_legitimacy__techno_nationalist_reading).

% DUAL FORMULATION NOTE:
% The performance_legitimacy kernel decomposes into four structurally distinct constraints corresponding to competing readings of how state authority should be grounded and demonstrated. The livelihood_security_reading uniquely prioritizes direct material improvements in daily life (employment, healthcare, education, elderly care) and visible service delivery over aggregate growth metrics, sustainable development transformation, or technological leadership. Each reading generates different ε values, different beneficiary/victim sets, and different suppression mechanisms because each reading instantiates different extraction patterns: this reading extracts from capital-intensive sectors and infrastructure investment; the quantitative_growth_reading extracts from service sectors and consumption support; the techno_nationalist_reading extracts from foreign-oriented sectors and immediate consumption; the qualitative_development_reading extracts from legacy industrial sectors and extractive industries. The readings coexist as competing claims held by different institutional factions (regime planning bodies, industrial ministries, financial authorities) and are related through influences and coexists_with relations rather than foreclosure. Each reading is authored in a separate constraint story with its own ε, stakeholder structure, and classification; links are maintained through network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(performance_legitimacy__livelihood_security_reading, organized, 0.68).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
