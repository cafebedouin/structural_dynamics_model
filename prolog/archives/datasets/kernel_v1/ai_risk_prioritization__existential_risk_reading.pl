% ============================================================================
% CONSTRAINT STORY: ai_risk_prioritization__existential_risk_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ai_risk_prioritization__existential_risk_reading, []).

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
 *   constraint_id: ai_risk_prioritization__existential_risk_reading
 *   human_readable: AI Risk Prioritization: Existential Risk Reading
 *   domain: ai_safety/technology_governance/risk_assessment
 *
 * SUMMARY:
 *   This constraint models the existential-risk reading of AI safety: the
 *   position that misaligned AGI poses an extinction-level threat to
 *   humanity, warranting prioritization of alignment research, capability
 *   control, and longtermist risk mitigation over near-term algorithmic
 *   justice, labor protection, and algorithmic auditing. The reading
 *   constructs a specific victim set (future humanity, including non-existent
 *   persons), a specific beneficiary set (x-risk research institutions and
 *   longtermist funders), and a specific suppression mechanism (framing
 *   near-term harms as a distraction from existential priority). This is ONE
 *   reading of a contested kernel. The sibling reading ('near-term harms
 *   reading') instantiates a different victim set (current marginalized
 *   communities), different beneficiary set (labor movements, algorithmic
 *   justice advocates), and reverses the suppression direction (treats
 *   existential-risk framing as a distraction from present-day harm
 *   mitigation). Neither reading logically forecloses the other—they
 *   represent genuinely different normative commitments and different
 *   empirical assumptions about AGI timelines and capability-control
 *   feasibility. The constraint is a Tangled Rope: it exhibits a genuine
 *   coordination function (mobilizing resources for AI safety research,
 *   building institutional capacity for governance) alongside asymmetric
 *   extraction (near-term harm communities bear suppression costs while
 *   resources and attention flow to existential-risk work).
 *
 * KEY AGENTS:
 *   - X-Risk Research Establishment: Primary beneficiary (institutional/arbitrage) — institutional funding and career pathways flow to existential-risk work. Sees the constraint as coordination: existential framing mobilizes resources and establishes research legitimacy.
 *   - Longtermist Funding Bodies: Primary beneficiary (institutional/arbitrage) — Open Philanthropy, Future of Humanity Institute, Centre for Effective Altruism allocate substantial resources based on existential-risk priority. Direct financial beneficiary of the framing.
 *   - Near-Term Harmed Populations (Workers, Marginalized Communities): Primary victims (powerless/trapped) — face algorithmic displacement, bias, surveillance harms in present day. Suppressed by existential-risk framing, which treats their concerns as lower priority. Cannot exit—must live within AI-driven labor markets and governance systems.
 *   - Policy Community (Labor & Justice): Secondary victims (moderate/constrained) — resource-constrained by existential-risk dominance in AI safety narrative. Face barriers to implementing near-term governance mechanisms. Constrained but have some agency through coalition-building.
 *   - Advanced AI Capability Developers: Mixed position (powerful/mobile) — benefit from existential-risk framing (capability investment framed as necessary for control), face some constraints (safety requirements), mobile exit (can shift narrative if regulatory pressure increases). Tangled rope perspective.
 *   - Multi-Stakeholder Governance Coalition: Organized actors (organized/constrained) — pushing for integrated governance addressing both timescales. See existential-risk bottleneck as temporary. Constrained by resource allocation away from near-term governance, but have strategic agency and sunset logic.
 *   - Traditional AI Ethics Frameworks: Institutional actors (institutional/arbitrage) — corporate and academic ethics structures maintaining performative legitimacy. Theater ratio measures their limited substantive impact. Piton perspective: degraded institutions sustained by inertia.
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing the existential-risk prioritization as a law of rational risk management under deep uncertainty, when it is actually a constructed institutional choice shaped by specific normative axioms and empirical assumptions.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_risk_prioritization__existential_risk_reading, 0.58).
domain_priors:suppression_score(ai_risk_prioritization__existential_risk_reading, 0.62).
domain_priors:theater_ratio(ai_risk_prioritization__existential_risk_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_risk_prioritization__existential_risk_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(ai_risk_prioritization__existential_risk_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(ai_risk_prioritization__existential_risk_reading, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_risk_prioritization__existential_risk_reading, tangled_rope).
narrative_ontology:human_readable(ai_risk_prioritization__existential_risk_reading, "AI Risk Prioritization: Existential Risk Reading").
narrative_ontology:topic_domain(ai_risk_prioritization__existential_risk_reading, "ai_safety/technology_governance/risk_assessment").

domain_priors:requires_active_enforcement(ai_risk_prioritization__existential_risk_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_risk_prioritization__existential_risk_reading, '2ae31404-7706-425f-a77f-1eec8d2c9be5').
narrative_ontology:cs_kernel_codification('2ae31404-7706-425f-a77f-1eec8d2c9be5', distributed).
narrative_ontology:cs_authority_grounding('2ae31404-7706-425f-a77f-1eec8d2c9be5', distributed).
narrative_ontology:cs_reading_relation('2ae31404-7706-425f-a77f-1eec8d2c9be5', ai_risk_prioritization__near_term_harms_reading, coexists_with).
narrative_ontology:cs_axiom('2ae31404-7706-425f-a77f-1eec8d2c9be5', foundational, agn_extinction_risk_dominant).
narrative_ontology:cs_axiom_status(agn_extinction_risk_dominant, holdable).
narrative_ontology:cs_axiom_grounding('2ae31404-7706-425f-a77f-1eec8d2c9be5', agn_extinction_risk_dominant, empirically_contingent).
narrative_ontology:cs_axiom('2ae31404-7706-425f-a77f-1eec8d2c9be5', foundational, longtermist_resource_allocation_justified).
narrative_ontology:cs_axiom_status(longtermist_resource_allocation_justified, holdable).
narrative_ontology:cs_axiom_grounding('2ae31404-7706-425f-a77f-1eec8d2c9be5', longtermist_resource_allocation_justified, instrumental).
narrative_ontology:cs_reference_frame('2ae31404-7706-425f-a77f-1eec8d2c9be5', alignment_urgent_prioritization).
narrative_ontology:cs_drift_state('2ae31404-7706-425f-a77f-1eec8d2c9be5', contemporary_2026, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('2ae31404-7706-425f-a77f-1eec8d2c9be5', '2026-02-26T14:32:00Z').
narrative_ontology:cs_kernel_id(ai_risk_prioritization__existential_risk_reading, ai_risk_prioritization).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_risk_prioritization__existential_risk_reading, x_risk_research_institutions).
narrative_ontology:constraint_beneficiary(ai_risk_prioritization__existential_risk_reading, longtermist_funding_bodies).
narrative_ontology:constraint_beneficiary(ai_risk_prioritization__existential_risk_reading, capability_control_advocates).
narrative_ontology:constraint_victim(ai_risk_prioritization__existential_risk_reading, near_term_algorithmic_justice).
narrative_ontology:constraint_victim(ai_risk_prioritization__existential_risk_reading, labor_displacement_communities).
narrative_ontology:constraint_victim(ai_risk_prioritization__existential_risk_reading, current_marginalized_populations).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: NEAR-TERM HARMED POPULATIONS (SNARE) — Workers facing algorithmic displacement, communities experiencing present-day bias in AI systems, marginalized groups now subjected to surveillance and automated harm. Trapped by economic dependence on AI-driven labor markets; cannot exit the constraint while living. Extraction is immediate and severe. The existential-risk framing suppresses attention to harms occurring now. Maximum chi — no exit options, no coordinating power.
constraint_indexing:constraint_classification(ai_risk_prioritization__existential_risk_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: POLICY COMMUNITY (LABOR & ALGORITHMIC JUSTICE) (TANGLED ROPE) — Governments and civil society organizations tasked with labor protections, algorithmic auditing, and bias mitigation. Constrained by resource allocation: existential-risk funding dominates AI safety discourse, pulling resources and attention away from near-term governance mechanisms. Moderate extraction — some coordination benefit (universal agreement that some AI governance is needed) but asymmetric cost allocation (near-term harm communities bear disproportionate risk while resources flow to longtermist research).
constraint_indexing:constraint_classification(ai_risk_prioritization__existential_risk_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: X-RISK RESEARCH ESTABLISHMENT (ROPE) — Institutions and researchers committed to AGI alignment, existential risk mitigation, and capability control research. Primary beneficiary of the existential-risk framing. Experiences the constraint as coordination: communicating the magnitude of existential threat mobilizes resources, recruits talent, and aligns institutional priorities around AGI alignment work. Arbitrage exit — can reallocate to other safety domains if existential framing loses salience. Net coordination role: establishes research agenda, convenes expertise, builds institutional infrastructure.
constraint_indexing:constraint_classification(ai_risk_prioritization__existential_risk_reading, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: ADVANCED AI CAPABILITY DEVELOPERS (TANGLED ROPE) — Frontier AI labs and corporations building increasingly capable models. Benefit from existential-risk framing: justifies capability investment as necessary for understanding and controlling AGI (capability-control narrative). Face some cost: alignment requirements and safety constraints. Mobile exit — can pivot to different markets or shift narrative framing. Mixed experience: strong coordination function (safety investments can be genuine, not purely theatrical) alongside extraction (existential priority suppresses regulation of near-term harms from current systems).
constraint_indexing:constraint_classification(ai_risk_prioritization__existential_risk_reading, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: MULTI-STAKEHOLDER GOVERNANCE COALITION (SCAFFOLD) — Coalitions pushing for near-term AI governance structures (auditing, transparency, labor protection, algorithmic impact assessments, participatory design). See existential-risk framing as a temporary bottleneck that will be superseded. Expect sunset: as existential AGI timelines extend or capability-control mechanisms mature, integrated governance structures addressing both near-term and long-term risks become standard. Constrained by current resource allocation away from multi-stakeholder governance. Low chi because coalition has strategic agency and sees an exit path.
constraint_indexing:constraint_classification(ai_risk_prioritization__existential_risk_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: TRADITIONAL AI ETHICS FRAMEWORKS (PITON) — Institutional ethics reviews, responsible AI principles, fairness-accountability-transparency (FAT) research, and corporate ethics boards. Largely performative in their current instantiation: principles are articulated but governance mechanisms remain weak, enforcement is minimal, and trade-offs between ethics and capability development favor capability. Maintains theaters of legitimacy (ethics committees, impact assessments) without substantive constraint on deployment. High theater ratio (0.75) because the institutional response is maintaining the appearance of ethical consideration while near-term harms continue and existential risk debates monopolize the safety narrative.
constraint_indexing:constraint_classification(ai_risk_prioritization__existential_risk_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilization-scale perspective, this perspective argues that some concentration of attention on extreme downside risks is an inherent feature of rational risk management under deep uncertainty. The shape-of-distribution argument: when facing potentially infinite-loss scenarios with low-but-non-negligible probability, rational resource allocation devotes substantial weight to tail-risk mitigation, regardless of institutional arrangements. This reading naturalizes the prioritization as a universal law of Bayesian decision-making under existential uncertainty. However, structural data indicates this is a false summit: the existential-risk framing is a constructed framework that shapes what counts as evidence, what timescales are 'relevant,' and which communities bear visibility costs — these are all contingent, not natural.
constraint_indexing:constraint_classification(ai_risk_prioritization__existential_risk_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ai_risk_prioritization__existential_risk_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(ai_risk_prioritization__existential_risk_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(ai_risk_prioritization__existential_risk_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(ai_risk_prioritization__existential_risk_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(ai_risk_prioritization__existential_risk_reading, TR),
    TR >= 0.70.

:- end_tests(ai_risk_prioritization__existential_risk_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The existential-risk reading extracts resources from near-term harm mitigation by subordinating those domains institutionally and narratively. The extraction is not maximal (0.72+) because genuine coordination functions exist—safety research is genuinely important, capability control mechanisms have real robustness benefits—so extraction is wrapped in authentic coordination logic rather than pure predation. The value reflects upward drift: the constraint's extractiveness has increased from 0.35 (2015) to 0.58 (2025) as existential-risk framing has consolidated institutional power in AI safety and funding landscapes. Suppression (0.62): High. The suppression operates through narrative and resource allocation: near-term algorithmic justice is explicitly subordinated ('distraction from existential priority'), funding directed away from labor-protection governance, policy attention concentrated on capability control and alignment. Suppression is high but not maximal because near-term harm communities have some independent organizing capacity, some policy allies, and alternative funding sources (though strained). Theater ratio (0.55): Moderate. The existential-risk framework includes both functional and theatrical elements. Functional: alignment research produces genuine technical progress (interpretability, robustness, formal verification). Theatrical: much of the institutional response (AI safety institutes, governance pronouncements) performs concern without substantive constraint on capability development. The theater ratio has risen from 0.38 (2015) to 0.55 (2025) as existential-risk frames have proliferated without corresponding near-term policy implementation.
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits maximum perspectival divergence across readings. The x-risk establishment sees Rope (coordination) because existential framing genuinely mobilizes resources and research. Near-term victims see Snare (maximum extraction with no exit) because they face present-day algorithmic harms while attention is directed elsewhere. The policy community sees Tangled Rope (mixed coordination and extraction) because some genuine governance infrastructure is built, but resource allocation is skewed toward existential work. The capability developers see Tangled Rope (they benefit from the framing but also face alignment constraints). The governance coalition sees Scaffold (the existential bottleneck is temporary; integrated governance will supersede it). The ethics frameworks see Piton (performative ethics maintain legitimacy while substantive constraint remains weak). The analytical observer sees a false-summit Mountain (naturalizing a constructed prioritization as an inherent law of rational risk management). The gap is not incidental—it is the core analytical finding. The same constraint appears as coordination, extraction, mixed hybrid, temporary bottleneck, degraded ritual, and false naturalization from different positions because the reading's structural logic genuinely subordinates near-term harm reduction to existential priority, and that subordination is experienced differently depending on whether you are the beneficiary, the victim, or the observer.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality d is derived from the agent's structural position relative to the extraction flow. X-risk researchers benefit from existential framing and have arbitrage exit options (low d → negative chi). Near-term victims bear costs with no exit (high d → maximum chi). Policy communities face resource constraints but have some agency (moderate d → moderate chi). Capability developers benefit narratively but face alignment constraints (moderate d, but asymmetric—they capture benefit while bearing some cost). Governance coalitions have exit paths (constrained but mobile d). Ethics frameworks are maintained by inertia with minimal functional impact (moderate d, but high theater masks low function). The analytical observer occupies the structural position of potential false-summit naturalization (high d for the natural-law framing, which means the framing itself extracts credibility without substantive epistemic warrant). The engine computes d from beneficiary/victim declarations and exits; the directionality logic here confirms that structure: x-risk institutions are beneficiaries with mobility; near-term victims are trapped; policy communities are constrained co-coordinators.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    agn_timescale_uncertainty,
    'What is the credible probability distribution over AGI timescales? Do credible AGI arrival estimates genuinely warrant the 10-100 year horizon, or is the extended timeline a theoretical construct that sustains longtermist funding independent of empirical AGI progress?',
    'Bayesian meta-analysis of AGI timeline estimates from diverse research communities (AI safety, ML capability, neuroscience, AI governance); retrospective calibration of past timeline predictions; structural incentive analysis (who benefits from extended timelines? who from compressed timelines?)',
    'If timelines are empirically shorter (5-20 years): existential-risk frame is proportionate, near-term governance becomes synchronous with existential mitigation. If timelines are empirically longer or highly uncertain (50+ years, distribution unknown): existential framing may constitute unjustified suppression of near-term harm reduction and diversion of resources from problems with known, addressable harms.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(agn_timescale_uncertainty, empirical, 'Empirical validity of AGI timescale estimates underlying existential risk prioritization').

omega_variable(
    alignment_near_term_overlap,
    'To what extent do the research and governance directions required for AGI alignment overlap with the mechanisms needed to mitigate near-term algorithmic harms? Are these genuinely competing resource pools, or can investments in interpretability, robustness, transparency, and formal verification serve both timescales?',
    'Structured comparison of technical and governance requirements: alignment research roadmaps vs near-term governance requirements; analysis of capability control mechanisms as potential near-term harm mitigation tools (transparency enforcing accountability, robustness reducing failure cascades); resource allocation patterns in funding (are funds truly zero-sum, or is existential framing suppressing simultaneous near-term investment?)',
    'If high overlap: existential prioritization is unjustified suppression—aligned research serves both horizons. If low/zero overlap: genuine resource trade-off exists, and existential framing is a justified prioritization given unknown timeline. If overlap is real but suppressed by current framing: institutional/funding landscape issue, not fundamental.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alignment_near_term_overlap, empirical, 'Structural overlap between AGI alignment requirements and near-term AI governance needs').

omega_variable(
    capability_control_feasibility_horizon,
    'Is robust capability control technically feasible within the timescale of AGI development (whether 10-100 years or shorter)? Or is the existential-risk reading''s assumption of capability-control solutions resting on speculative technical claims that may not materialize?',
    'Technical feasibility analysis: progress on interpretability, formal verification, and alignment research over past 5-10 years; expert consensus on capability-control roadmaps; conditional probability estimates (given AGI arrival at T=15 years, what is the probability of robust control mechanisms being available by T=15?)',
    'If capability control is plausible: existential-risk prioritization is justified and actionable. If capability control is technically infeasible: existential-risk frame becomes a naturalized doom narrative that may suppress near-term governance attempts by framing them as irrelevant. The constraint would shift from justified prioritization to demoralizing fatalism.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(capability_control_feasibility_horizon, empirical, 'Technical feasibility of capability control mechanisms within AGI development timescale').

omega_variable(
    existential_suppression_mechanism,
    'Is the existential-risk reading''s subordination of near-term harm governance a logical consequence of the risk mathematics, or a social-institutional choice that could be decoupled from the existential-risk claim itself?',
    'Institutional analysis: examine cases where existential-risk and near-term-harm communities coordinate successfully vs fail; analyze funding allocation decisions and implicit trade-off assumptions; trace the logical chain from ''AGI alignment is important'' to ''near-term algorithmic justice is less important''—is this step forced by the mathematics or by resource scarcity and narrative dominance?',
    'If suppression is a logical consequence: existential prioritization genuinely requires subordinating near-term governance. If suppression is an institutional choice: near-term harm reduction and existential alignment could be pursued simultaneously; the single-priority constraint is constructed, not inherent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(existential_suppression_mechanism, conceptual, 'Whether suppression of near-term AI harm governance is logically forced by existential-risk mathematics or an institutional choice').

omega_variable(
    reading_contest_kernel,
    'This constraint is one reading of the contested kernel ''AI risk prioritization.'' What distinguishes this reading from the ''near-term harms reading''? Is the distinction empirical (different facts about AI timelines and capability), axiomatic (different normative commitments), or definitional (different framing of what counts as ''AI risk'')?',
    'Structured comparison of the two readings: identify empirical claims unique to each, identify normative axioms each assumes, trace the logical dependencies. Document the ''irreducible'' contest point—the place where the readings cannot be reconciled by additional evidence alone.',
    'If distinction is empirical: evidence about AGI timelines and capability control feasibility could resolve the contest. If distinction is axiomatic: both readings can remain live simultaneously, bounded by different normative commitments. If distinction is definitional: the readings are about different phenomena (one about existential risk, one about AI harms) and should not compete for the same resources.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_contest_kernel, conceptual, 'The specific nature of the kernel contest between existential and near-term risk readings').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_risk_prioritization__existential_risk_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(airp_ext_theater_2015, ai_risk_prioritization__existential_risk_reading, theater_ratio, 0, 0.38).
narrative_ontology:measurement(airp_ext_theater_2020, ai_risk_prioritization__existential_risk_reading, theater_ratio, 5, 0.48).
narrative_ontology:measurement(airp_ext_theater_2025, ai_risk_prioritization__existential_risk_reading, theater_ratio, 10, 0.55).

% Extraction over time
narrative_ontology:measurement(airp_ext_extractiveness_2015, ai_risk_prioritization__existential_risk_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(airp_ext_extractiveness_2020, ai_risk_prioritization__existential_risk_reading, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(airp_ext_extractiveness_2025, ai_risk_prioritization__existential_risk_reading, base_extractiveness, 10, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(airp_ext_suppression_2015, ai_risk_prioritization__existential_risk_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(airp_ext_suppression_2020, ai_risk_prioritization__existential_risk_reading, suppression_requirement, 5, 0.55).
narrative_ontology:measurement(airp_ext_suppression_2025, ai_risk_prioritization__existential_risk_reading, suppression_requirement, 10, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_risk_prioritization__existential_risk_reading, resource_allocation).
narrative_ontology:affects_constraint(ai_risk_prioritization__existential_risk_reading, ai_risk_prioritization__near_term_harms_reading).
narrative_ontology:affects_constraint(ai_risk_prioritization__existential_risk_reading, algorithmic_governance_implementation).
narrative_ontology:affects_constraint(ai_risk_prioritization__existential_risk_reading, labor_displacement_from_ai).

% DUAL FORMULATION NOTE:
% The AI risk prioritization kernel bifurcates into two distinct constraints with different ε values and victim/beneficiary structures. The existential_risk_reading (this file) treats future humanity as victim and extracts from near-term harm mitigation. The near_term_harms_reading treats present-day marginalized communities as victim and subordinates existential risk. These are not the same constraint viewed from two angles—their ε values and structural properties genuinely differ. Both readings influence downstream constraints (governance implementation, labor protection) but in opposite directions: existential reading emphasizes capability control and alignment; near-term reading emphasizes transparency and labor rights. Network links document the structural coupling.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(ai_risk_prioritization__existential_risk_reading, moderate, 0.68).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
