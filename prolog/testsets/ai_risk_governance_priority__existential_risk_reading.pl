% ============================================================================
% CONSTRAINT STORY: ai_risk_governance_priority__existential_risk_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ai_risk_governance_priority__existential_risk_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: ai_risk_governance_priority__existential_risk_reading
 *   human_readable: AI Risk Governance: Existential Risk Priority Reading
 *   domain: ai_governance/technology_ethics/existential_risk
 *
 * SUMMARY:
 *   AI risk governance institutions (regulatory bodies, research
 *   organizations, policy forums) must allocate limited resources and
 *   institutional attention across a contested landscape of AI-related risks.
 *   The existential-risk reading frames the constraint as follows:
 *   superintelligence scenarios — artificial systems achieving self-directed
 *   capability growth beyond human course-correction capacity — pose an
 *   existential threat to humanity's future. This reading prioritizes
 *   governance frameworks for AGI-scenario contingencies,
 *   alignment-as-control research, adversarial testing, and
 *   recursive-improvement safeguards. Resources flow toward x-risk research
 *   institutions, AI labs claiming safety leadership, and policy frameworks
 *   addressing hypothetical superintelligence emergence. This constraint
 *   exhibits tangled-rope structure because it combines genuine coordination
 *   (shared governance standards, alignment research, risk assessment
 *   frameworks) with asymmetric extraction (resources diverted away from
 *   documented present harms, near-term safety research subordinated,
 *   marginalized populations' immediate documented discrimination
 *   deprioritized relative to hypothetical scenarios). The measurement
 *   trajectory shows extraction rising as the reading consolidates
 *   institutional power: from 0.35 (balanced resource allocation) to 0.58
 *   (existential-risk priority entrenched). Theater ratio rises
 *   correspondingly from 0.42 to 0.68, indicating increasing performative
 *   governance (AGI safety protocols aspirational, actual operational
 *   capacity low). Suppression rises from 0.45 to 0.62 as alternative
 *   readings lose institutional legitimacy and uncertainty about
 *   superintelligence becomes a barrier to alternative framing.
 *
 * KEY AGENTS:
 *   - Future Humanity: All generations beyond the current era (civilizational scope). Victim. Trapped; cannot exit world shaped by present governance choices. Bears extraction through deprioritization of preventable harms.
 *   - Present Marginalized Populations: Communities experiencing algorithmic bias, surveillance, employment displacement, denial of credit and benefits (biographical scope). Victim. Trapped in AI-mediated systems (credit, employment, public services); cannot exit. Extraction: institutional attention redirected away from documented discrimination toward speculative scenarios.
 *   - Near-Term AI Safety Research Community: Researchers and organizations focused on bias remediation, fairness, transparency, labor impacts (biographical scope). Moderate power, constrained. Victim-beneficiary hybrid: benefits from coordination frameworks; extraction occurs through subordination of this research relative to existential-risk work.
 *   - X-Risk Research Institutions: Academic centers, think tanks, and research organizations focused on existential-risk analysis and superintelligence governance. Primary beneficiary. Institutional power, arbitrage exit. Captures funding priority, policy influence, narrative authority.
 *   - Frontier AI Labs Claiming Safety Leadership: OpenAI, Anthropic, DeepMind, and similar organizations framing their internal governance and safety research as AGI-scenario preparation. Secondary beneficiary. Institutional power, arbitrage exit. Legitimates resource allocation decisions (compute allocation, model scaling, capability control) as existential-risk mitigation.
 *   - Unified AI Governance Movement (Bridge Advocates): Civil society organizations, impacted communities, alternative-reading researchers advocating integrated frameworks addressing both present harms and existential risks. Organized power, constrained (by institutional inertia favoring existing reading). Sees constraint as temporary (scaffold with sunset), not permanent priority.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_risk_governance_priority__existential_risk_reading, 0.58).
domain_priors:suppression_score(ai_risk_governance_priority__existential_risk_reading, 0.62).
domain_priors:theater_ratio(ai_risk_governance_priority__existential_risk_reading, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_risk_governance_priority__existential_risk_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(ai_risk_governance_priority__existential_risk_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(ai_risk_governance_priority__existential_risk_reading, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_risk_governance_priority__existential_risk_reading, tangled_rope).
narrative_ontology:human_readable(ai_risk_governance_priority__existential_risk_reading, "AI Risk Governance: Existential Risk Priority Reading").
narrative_ontology:topic_domain(ai_risk_governance_priority__existential_risk_reading, "ai_governance/technology_ethics/existential_risk").

domain_priors:requires_active_enforcement(ai_risk_governance_priority__existential_risk_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_risk_governance_priority__existential_risk_reading, '4cfd973d-3538-4d21-8fc6-b6754c9f0eef').
narrative_ontology:cs_kernel_codification('4cfd973d-3538-4d21-8fc6-b6754c9f0eef', distributed).
narrative_ontology:cs_authority_grounding('4cfd973d-3538-4d21-8fc6-b6754c9f0eef', extraction).
narrative_ontology:cs_reading_relation('4cfd973d-3538-4d21-8fc6-b6754c9f0eef', ai_risk_governance_priority__near_term_harms_reading, coexists_with).
narrative_ontology:cs_reading_relation('4cfd973d-3538-4d21-8fc6-b6754c9f0eef', ai_risk_governance_priority__bridge_reading, influences).
narrative_ontology:cs_axiom('4cfd973d-3538-4d21-8fc6-b6754c9f0eef', foundational, superintelligence_poses_existential_threat).
narrative_ontology:cs_axiom_status(superintelligence_poses_existential_threat, holdable).
narrative_ontology:cs_axiom_grounding('4cfd973d-3538-4d21-8fc6-b6754c9f0eef', superintelligence_poses_existential_threat, empirically_contingent).
narrative_ontology:cs_axiom('4cfd973d-3538-4d21-8fc6-b6754c9f0eef', foundational, existential_risk_supersedes_present_harm_prioritization).
narrative_ontology:cs_axiom_status(existential_risk_supersedes_present_harm_prioritization, holdable).
narrative_ontology:cs_axiom_grounding('4cfd973d-3538-4d21-8fc6-b6754c9f0eef', existential_risk_supersedes_present_harm_prioritization, instrumental).
narrative_ontology:cs_reference_frame('4cfd973d-3538-4d21-8fc6-b6754c9f0eef', pre_scaling_era_ai_governance).
narrative_ontology:cs_drift_state('4cfd973d-3538-4d21-8fc6-b6754c9f0eef', post_large_language_model_emergence, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('4cfd973d-3538-4d21-8fc6-b6754c9f0eef', '').
narrative_ontology:cs_kernel_id(ai_risk_governance_priority__existential_risk_reading, ai_risk_governance_priority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_risk_governance_priority__existential_risk_reading, x_risk_research_institutions).
narrative_ontology:constraint_beneficiary(ai_risk_governance_priority__existential_risk_reading, ai_labs_claiming_safety_leadership).
narrative_ontology:constraint_victim(ai_risk_governance_priority__existential_risk_reading, future_humanity).
narrative_ontology:constraint_victim(ai_risk_governance_priority__existential_risk_reading, present_marginalized_populations).
narrative_ontology:constraint_victim(ai_risk_governance_priority__existential_risk_reading, near_term_ai_safety_research).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: FUTURE HUMANITY (SNARE) — Cannot exit; bears maximal extraction through resource reallocation away from present harms toward speculative superintelligence scenarios. Future generations have no voice in governance and no capacity to exit a world shaped by present prioritization choices. High suppression: the very uncertainty about superintelligence prevents alternative framings from gaining institutional legitimacy.
constraint_indexing:constraint_classification(ai_risk_governance_priority__existential_risk_reading, snare,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: PRESENT MARGINALIZED POPULATIONS (SNARE) — Currently harmed by algorithmic bias, surveillance, labor displacement, and misinformation. Trapped by dependence on AI-mediated systems (employment, credit, public services, benefits eligibility). Extraction mechanism: existential-risk framing subordinates their immediate documented harms to hypothetical future scenarios, redirecting institutional attention and resources away from mitigating present discrimination.
constraint_indexing:constraint_classification(ai_risk_governance_priority__existential_risk_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: NEAR-TERM AI SAFETY RESEARCH COMMUNITY (TANGLED ROPE) — Experiences both coordination and extraction. The existential-risk reading creates coordination around governance frameworks (alignment protocols, testing standards). But coordination is asymmetric: resources flow toward AGI-scenario work; near-term harms research is subordinated. Career advancement favors existential-risk scholars. Constrained exit: researchers depend on institutional funding that increasingly prioritizes superintelligence scenarios.
constraint_indexing:constraint_classification(ai_risk_governance_priority__existential_risk_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 4: X-RISK RESEARCH INSTITUTIONS (ROPE) — Primary beneficiary. The existential-risk reading creates legitimate coordination function: identifies genuine structural risks (recursive self-improvement, instrumental convergence, corrigibility). But also captures institutional benefits: funding priority, research autonomy, policy influence, narrative authority over 'responsible AI' frames. Arbitrage: can exit to alternative AI domains; chooses this framing because it maximizes institutional position.
constraint_indexing:constraint_classification(ai_risk_governance_priority__existential_risk_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: FRONTIER AI LABS CLAIMING SAFETY LEADERSHIP (ROPE) — Secondary beneficiary. The existential-risk reading legitimates their internal control structures (alignment research, adversarial testing, capability throttling for AGI scenarios) as prudent risk management rather than business or competitive decision-making. Coordination function: establishes shared discourse about responsible scaling. Arbitrage exit: can frame any resource allocation decision as existential-risk mitigation.
constraint_indexing:constraint_classification(ai_risk_governance_priority__existential_risk_reading, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: UNIFIED AI GOVERNANCE MOVEMENT (SCAFFOLD) — Organized advocates (civil society, impacted communities, alternative-reading researchers) see the existential-risk reading as one structurally necessary but temporally localized focus. The reading should have a sunset: as governance frameworks mature for AGI-scenario contingency planning, institutional attention should rebalance toward documented present harms. This perspective sees the constraint as temporary rather than permanent priority. Constrained exit: institutional inertia means the existential-risk frame persists even as governance capacity develops.
constraint_indexing:constraint_classification(ai_risk_governance_priority__existential_risk_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: POLICY DISCOURSE ON AGI GOVERNANCE (PITON) — The existential-risk reading has been institutionalized through policy language, government testimony, and regulatory frameworks, but its primary function is now performative. Policymakers invoke superintelligence scenarios to justify broad regulatory authority while actual governance mechanisms remain ambiguous or non-functional. The constraint persists through theatrical invocation of hypothetical risks rather than through active capacity to manage those risks. Theater ratio high: AGI safety protocols are largely aspirational.
constraint_indexing:constraint_classification(ai_risk_governance_priority__existential_risk_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER / TECHNICAL INEVITABILITY VIEW (MOUNTAIN) — From a technical and civilizational view, recursive self-improvement in AI systems may be a structural inevitability with known terminal states (superintelligence or capability plateau). Some risks (instrumental convergence, value alignment difficulty) may be irreducible features of the optimization landscape. This perspective risks naturalizing what is actually a contingent institutional allocation of resources and institutional power over the AI risk narrative.
constraint_indexing:constraint_classification(ai_risk_governance_priority__existential_risk_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ai_risk_governance_priority__existential_risk_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(ai_risk_governance_priority__existential_risk_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(ai_risk_governance_priority__existential_risk_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(ai_risk_governance_priority__existential_risk_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(ai_risk_governance_priority__existential_risk_reading, TR),
    TR >= 0.70.

:- end_tests(ai_risk_governance_priority__existential_risk_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The reading creates genuine coordination benefits (shared governance language, alignment research, risk assessment frameworks) but channels resources asymmetrically toward superintelligence scenarios. The extraction is not as severe as a pure snare (0.72+) because some beneficiaries (AI labs) face real technical problems requiring the resources directed to them. But extraction is substantial because documented present harms are deprioritized despite higher certainty of impact. Suppression (0.62): Moderate-high. Uncertainty about superintelligence is deployed as a suppression mechanism: alternative readings struggle to gain institutional legitimacy because existential-risk framers claim unprecedented stakes. Documented harms have less narrative weight than hypothetical scenarios. Career advancement and funding favor the reading. Theater ratio (0.68): Moderate-high. AGI safety protocols, governance frameworks, and policy statements are largely aspirational — actual operational capacity for managing superintelligence scenarios remains ambiguous. The constraint persists partly through performative policy (government testimony, regulatory language) rather than functional governance mechanisms. Claimed type (Tangled Rope): Justified by the combination of genuine coordination function (alignment research, shared governance standards) and asymmetric resource extraction (present-harm mitigation deprioritized). The suppression (0.62) and enforcement requirement exceed the threshold for Rope (0.45); the beneficiary/victim structure confirms asymmetry.
 *
 * PERSPECTIVAL GAP:
 *   The existential-risk reading classifies identically as Tangled Rope from the near-term safety researcher perspective (moderate power, constrained exit, mixed benefit-and-extraction experience) and identically as Rope from the x-risk institution perspective (institutional power, arbitrage exit, net beneficiary). But from the future humanity perspective (civilizational scope, powerless, trapped), the same constraint is pure Snare — maximal extraction with no coordination benefit to the victim. From the present marginalized populations perspective (biographical scope, powerless, trapped in AI systems), the constraint also appears as Snare — their documented harms are subordinated to hypothetical future scenarios. The unified governance coalition sees it as Scaffold (temporary, with sunset when governance frameworks mature). The policy discourse sees it as Piton (performative ritual). The technical-inevitability analytical view risks seeing it as Mountain (natural law about AI optimization difficulty) — a false summit that naturalizes institutional choice as technical necessity. This gap reveals that the constraint's structural character depends critically on which victim set and which probability estimate one adopts.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) values reflect each agent's structural position relative to this specific constraint. Future humanity: d ≈ 0.98 (full target of extraction, no capacity for exit or resistance). Present marginalized populations: d ≈ 0.92 (primary victims of resource reallocation, trapped in AI-mediated systems, powerless to influence governance). Near-term safety researchers: d ≈ 0.55 (organizational victim through subordination, but also benefit from coordination frameworks; moderate power allows some agency in contested discourse). X-risk institutions: d ≈ 0.08 (net beneficiary, institutional power, high arbitrage capacity — can exit to other AI domains but choose this reading because it maximizes their position). Frontier AI labs: d ≈ 0.12 (secondary beneficiary through legitimation, arbitrage exit, institutional power). The engine derives these from the declared beneficiary/victim groups and exit-option data; the chi formula then applies f(d) to produce experienced extractiveness. The high d values for trapped agents (future humanity, present marginalized populations) produce high f(d) ≈ 1.42, amplifying their experienced extraction. The low d values for beneficiaries produce negative f(d) ≈ -0.12, meaning they experience negative extraction (subsidy). The moderate d values for constrained near-term researchers produce mid-range f(d) ≈ 0.65, reflecting their mixed experience.
 *
 * MANDATROPHY ANALYSIS:
 *   KERNEL READING CONTEST DIAGNOSTICS: The mandatrophy in AI risk governance is not a classification uncertainty but a *legitimacy contest*. All three readings (existential-risk, near-term-harms, bridge) classify as real constraint types from their respective perspectives: the existential-risk reading is Tangled Rope (coordination + extraction); the near-term-harms reading would be Snare or Tangled Rope (extraction subordinating present remedy efforts); the bridge reading would be Scaffold (temporary priority imbalance to be rebalanced as governance matures). The mandatrophy resolves through the omega variables, not by declaring a single 'true' type. The resolution depends on: (1) superintelligence probability and timeline (omega_1), which determines whether the existential-risk reading's claimed urgency is justified; (2) control possibility (omega_2), which determines whether the existential-risk reading is even coherent as a governance strategy; (3) resource allocation counterfactual (omega_3), which determines whether the extraction is ethically justified by comparative harm reduction; (4) institutional capture analysis (omega_4), which determines whether the reading's institutional dominance reflects empirical warrant or power asymmetry; (5) present-harm attribution (omega_6), which determines whether algorithmic systems are genuinely causing new harms or surfacing prior discrimination. The committer-frame omega_5 asks whether the three readings are genuinely live or whether evidence has foreclosed some of them. If empirical work narrows the possibility space, the contest resolves. If empirical work widens uncertainty, the contest persists and resource allocation becomes an ethical/political question, not an epistemic one.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    superintelligence_probability_and_timeline,
    'What is the actual probability and timeline for artificial superintelligence capable of self-improvement beyond human capacity for course correction?',
    'Historical tracking of capability growth trajectories; analysis of scaling laws and architectural limits; expert forecasts with calibration scoring; comparison of model predictions against realized outcomes',
    'Low probability (< 10% by 2050): the existential-risk reading becomes aspirational rather than structural; resource priority becomes difficult to justify ethically. High probability (> 60% by 2050): the constraint strengthens from Tangled Rope to Mountain; present-harm mitigation becomes secondary. Timing difference critical: 10 years vs 50+ years shifts optimal governance strategy completely.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(superintelligence_probability_and_timeline, empirical, 'Probability and timeline of artificial superintelligence emergence').

omega_variable(
    capability_threshold_and_control_possibility,
    'At what capability level does AI system control become theoretically impossible due to instrumental convergence or deceptive alignment, and is this threshold approaching or distant?',
    'Formal analysis of alignment difficulty across capability domains; empirical testing of deception and goal-hiding in current systems; theoretical work on value learning and corrigibility; comparison of alignment difficulty curves to capability growth rates',
    'If control remains theoretically possible at all realistic capability levels: the constraint is primarily a coordination/governance problem (Rope or Tangled Rope remains stable). If control becomes impossible at achievable capability levels: existential-risk reading becomes mandatory (Mountain-like certainty); present-harm mitigation is tragic but necessary sacrifice.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(capability_threshold_and_control_possibility, empirical, 'Whether AI control remains theoretically feasible at advanced capability levels').

omega_variable(
    resource_allocation_counterfactual,
    'What proportion of harm reduction (bias, surveillance, labor displacement) could be achieved with resources currently allocated to existential-risk scenarios, and what is the marginal return differential?',
    'Cost-benefit analysis of bias remediation vs alignment research funding; case studies of programs redirected from near-term to existential-risk focus; modeling of harm trajectories under alternative funding allocations; comparative effectiveness analysis',
    'If near-term mitigation has significantly higher harm-reduction-per-dollar: existential-risk priority becomes ethically indefensible without stronger evidence of superintelligence probability. If marginal returns are comparable: both readings remain tenable; the constraint is genuinely a two-reading contest about empirical estimates, not structural conflict.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(resource_allocation_counterfactual, empirical, 'Comparative harm reduction from resources allocated to existential vs near-term AI risks').

omega_variable(
    institutional_capture_by_x_risk_framing,
    'To what extent has the existential-risk reading captured AI governance institutions, funding bodies, and policy attention through an asymmetric discourse power (founders, elite researchers, venture funding) rather than through superior evidence?',
    'Analysis of funding flows by reading (existential vs near-term); demographic composition of AI governance bodies and research leadership; citation patterns and narrative authority in policy documents; comparison of institutional resources defending each reading against exogenous institutional pressures',
    'High capture: the constraint is partly a Snare/Piton (institutional extraction + theater) rather than purely Tangled Rope (genuine governance problem). Institutional capture would justify the false-summit detector and FSM reclassification pressure. Low capture: the existential-risk reading''s institutional dominance is proportional to its empirical warrant.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_capture_by_x_risk_framing, empirical, 'Degree of institutional capture by existential-risk framing in AI governance').

omega_variable(
    kernel_reading_contest_resolution,
    'Is this one of three genuinely live readings of the AI risk governance kernel, or does the empirical evidence and ethical reasoning support one reading''s core claim while foreclosing others?',
    'Systematic comparison of each reading''s foundational axioms against available evidence; analysis of which axioms remain ''holdable'' vs ''overridden'' as empirical work accumulates; tracking of authority/institutional support shifts across readings over time',
    'If all three readings remain holdable: the constraint continues as a coexisting three-way contest (network effects, institutional friction). If evidence forecloses one or more readings: the kernel contest resolves; the foreclosed reading''s supporting institutions face legitimacy crisis. This is the committer-frame signal: whether the divergence is fundamental or empirically resolvable.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_contest_resolution, conceptual, 'Whether the three readings of the AI risk governance kernel are logically compatible or mutually foreclosing').

omega_variable(
    present_harm_measurement_and_attribution,
    'Can present harms from AI systems (bias, misinformation, labor displacement, surveillance) be conclusively attributed to the AI systems themselves rather than to prior institutional discrimination and power asymmetries?',
    'Comparative analysis of harm baselines before AI deployment vs after; studies of algorithmic systems deployed in contexts with vs without prior discrimination; measurement of counterfactual harms if the same institutional decisions had been made with human discretion instead of algorithmic mediation',
    'If harms are primarily algorithmic (AI-caused): near-term harms reading strengthens; present populations are victims of AI specifically, not just prior inequality. If harms are primarily institutional (AI is tool in unjust system): the constraint''s victim classification for present populations weakens slightly; but the extraction mechanism (resource reallocation away from remediation) remains valid. Either way, this determines whether the constraint is primarily about AI''s impact or about institutional choice to prioritize hypothetical future risks over documented present ones.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(present_harm_measurement_and_attribution, empirical, 'Attribution of present AI harms to algorithmic design vs institutional context').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_risk_governance_priority__existential_risk_reading, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(airg_exist_tr_t0, ai_risk_governance_priority__existential_risk_reading, theater_ratio, 0, 0.42).
narrative_ontology:measurement(airg_exist_tr_t3, ai_risk_governance_priority__existential_risk_reading, theater_ratio, 3, 0.61).
narrative_ontology:measurement(airg_exist_tr_t6, ai_risk_governance_priority__existential_risk_reading, theater_ratio, 6, 0.68).

% Extraction over time
narrative_ontology:measurement(airg_exist_be_t0, ai_risk_governance_priority__existential_risk_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(airg_exist_be_t3, ai_risk_governance_priority__existential_risk_reading, base_extractiveness, 3, 0.48).
narrative_ontology:measurement(airg_exist_be_t6, ai_risk_governance_priority__existential_risk_reading, base_extractiveness, 6, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(airg_exist_su_t0, ai_risk_governance_priority__existential_risk_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(airg_exist_su_t3, ai_risk_governance_priority__existential_risk_reading, suppression_requirement, 3, 0.58).
narrative_ontology:measurement(airg_exist_su_t6, ai_risk_governance_priority__existential_risk_reading, suppression_requirement, 6, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_risk_governance_priority__existential_risk_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(ai_risk_governance_priority__existential_risk_reading, ai_risk_governance_priority__near_term_harms_reading).
narrative_ontology:affects_constraint(ai_risk_governance_priority__existential_risk_reading, ai_risk_governance_priority__bridge_reading).

% DUAL FORMULATION NOTE:
% The existential-risk reading is one of three structurally distinct constraints arising from the same kernel: AI risk governance priority. The three readings differ in victim sets (all future humanity vs present marginalized populations vs both), beneficiary structures (x-risk institutions vs impacted communities vs integrated governance bodies), and resource flows (AGI-scenario focus vs present-harm mitigation vs unified frameworks). Each reading has its own ε value, its own measurement trajectory, and its own perspectives. They are linked through the network as coexisting institutional readings of the same governance kernel. The empirical facts about superintelligence probability, control possibility, and resource allocation counterfactuals will determine whether one reading eventually forecloses the others, or whether the three readings remain live competing positions.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
