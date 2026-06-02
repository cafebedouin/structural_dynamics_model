% ============================================================================
% CONSTRAINT STORY: average_is_over_2026
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-27
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_average_is_over_2026, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: average_is_over_2026
 *   human_readable: AI-Talent Barbell Economy: Dual-Tier Labor Market Separation
 *   domain: economic/technological/labor
 *
 * SUMMARY:
 *   The AI-talent barbell economy of 2026 represents a structural bifurcation
 *   of the labor market with minimal mobility between tiers. The top
 *   tier—composed of elite cognitive talent with deep AI literacy and access
 *   to cutting-edge models—experiences exponential productivity multipliers
 *   (5-50x depending on task domain), while the median-skill tier faces
 *   displacement without viable transition paths. The constraint exhibits
 *   genuine coordination benefits for the elite tier (problems previously
 *   requiring teams of 50 now solvable by 5) while simultaneously extracting
 *   from median-skill workers through credential inflation, job elimination,
 *   and the removal of traditional middle-skill work (paralegals, junior
 *   analysts, radiologists, copywriters). The barbell is maintained actively
 *   through multiple mechanisms: immigration policy gatekeeping (visa
 *   restrictions concentrate talent in global hubs), credential requirements
 *   (traditional degree requirements persist despite weak correlation to AI
 *   capability), platform lock-in (API pricing and switching costs), and
 *   information asymmetries about model capability improvements. The
 *   constraint shows rising extractiveness over the 6-year measurement window
 *   (0.32 → 0.64) as displacement accelerates and retraining cohorts discover
 *   the velocity gap—the time required to achieve elite-tier capability
 *   exceeds the job-search window before savings deplete. Theater ratio
 *   remains low (0.35) because the barbell is driven by genuine economic
 *   incentives, not performative mechanisms, though credentialing systems
 *   within the barbell are increasingly theatrical.
 *
 * KEY AGENTS:
 *   - Elite AI-Literate Workers: Primary beneficiary (institutional/arbitrage) — capture exponential productivity gains; always have alternative employment options; experience the constraint as coordination mechanism
 *   - Median-Skill Workers: Primary victim (powerless/trapped) — face displacement with no viable transition; structured barriers to elite-tier entry; cannot organize effectively across dispersed competition
 *   - Transition Cohorts: Secondary victim (moderate/constrained) — workers with sufficient resources to attempt retraining; face credential inflation, time costs, and employer preference for native elite-tier candidates
 *   - AI Platform Owners: Primary beneficiary (institutional/constrained) — benefit from user lock-in, API pricing power, and network effects; constrained by open-source competition and regulatory risk
 *   - Venture Capital Allocators: Primary beneficiary (institutional/arbitrage) — concentrate capital in elite-tier talent and platform companies; capture returns through equity positions
 *   - Open-Source AI Coalition: Organized actor (organized/constrained) — building alternative infrastructure pathways; constrained by funding scarcity and talented volunteer bandwidth; building sunset trajectory for platform lock-in
 *   - Nation-States and Policymakers: Institutional actor (powerful/mobile) — face coordination problem (brain drain, competitive disadvantage); constrained by capital mobility and tech-sector political influence; have mobility in policy levers
 *   - Traditional Credentialing Systems: Institutional actor (institutional/arbitrage) — persist through inertia; face slow obsolescence as portfolios and demonstrations replace credentials
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(average_is_over_2026, 0.58).
domain_priors:suppression_score(average_is_over_2026, 0.68).
domain_priors:theater_ratio(average_is_over_2026, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(average_is_over_2026, extractiveness, 0.58).
narrative_ontology:constraint_metric(average_is_over_2026, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(average_is_over_2026, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(average_is_over_2026, tangled_rope).
narrative_ontology:human_readable(average_is_over_2026, "AI-Talent Barbell Economy: Dual-Tier Labor Market Separation").
narrative_ontology:topic_domain(average_is_over_2026, "economic/technological/labor").

domain_priors:requires_active_enforcement(average_is_over_2026).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(average_is_over_2026, elite_ai_literate_workers).
narrative_ontology:constraint_beneficiary(average_is_over_2026, ai_platform_owners).
narrative_ontology:constraint_beneficiary(average_is_over_2026, venture_capital_allocators).
narrative_ontology:constraint_victim(average_is_over_2026, median_skill_workers).
narrative_ontology:constraint_victim(average_is_over_2026, transition_cohorts).
narrative_ontology:constraint_victim(average_is_over_2026, displaced_knowledge_workers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: MEDIAN SKILL WORKER (SNARE) — Trapped between obsolescence and unattainability. Traditional middle-skill work (paralegals, radiologists, junior analysts, copywriters) faces displacement with no viable transition path. Retraining requires sustained education and income during learning period; elite-tier entry requires both credentials and proximity to cutting-edge infrastructure not available at their income level. Suppression is extreme: the economic incentive to employ cheaper, faster AI is structural and absolute. No viable exit except downward mobility or geographic arbitrage to lower cost-of-living regions. Maximum extraction — this agent cannot organize effectively across dispersed, competing peers.
constraint_indexing:constraint_classification(average_is_over_2026, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: TRANSITION COHORT (TANGLED ROPE) — Workers with sufficient resources and circumstance to retrain but facing substantial cost and risk. Can access bootcamps, online learning, mentorship networks. Benefits from genuine coordination: cross-skill learning, community support, peer accountability structures. But extraction is real: credential inflation, job-market competition for seats in selective programs, employer preference for native elite-tier candidates even after retraining. Time cost during learning period (1-2 years) creates severe financial pressure. Constrained exit — possible but expensive.
constraint_indexing:constraint_classification(average_is_over_2026, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: ELITE AI-LITERATE WORKERS (ROPE) — Experience the constraint as pure coordination gain. Each unit of their cognitive effort is multiplied by 5-50x through model-augmentation. Their productivity gains are shared (somewhat) with employers and collaborators, enabling coordination on problems previously intractable. The constraint solves a genuine collective action problem: complex projects that required huge teams are now possible with smaller, better-coordinated groups. Low extraction experienced by this cohort because their exit capacity is maximal (always have offers from competing firms) and their gain is directly correlated with system productivity growth.
constraint_indexing:constraint_classification(average_is_over_2026, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: AI PLATFORM OWNERS (TANGLED ROPE) — Primary beneficiaries of the barbell structure. Genuine coordination function: platforms must maintain model quality, API reliability, and feature development — all require solving real collective action problems with users and internal teams. But extraction is substantial: platform fees (15-40% of margin on many enterprise uses), lock-in (high switching costs once workflows built on a platform), and information asymmetries (users cannot see model improvements or degradations without significant audit). Enforced actively through terms-of-service and technical architecture. Exit constrained by switching costs and lack of genuine alternatives at comparable scale.
constraint_indexing:constraint_classification(average_is_over_2026, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: OPEN-SOURCE AI COALITION (SCAFFOLD) — Community-driven alternatives (Hugging Face open models, local-running systems, open-weights checkpoints) see the barbell as a temporary market configuration being undermined by distributed model development and inference. As model-hosting costs drop and local GPU clusters become economically competitive, platform lock-in decreases. The sunset logic: within 5-10 years, fine-tuning and deployment of state-equivalent models on local infrastructure becomes cost-competitive with proprietary APIs. Coalition has agency and organized membership (researchers, engineers, institutions). Theater is low — actual functional coordination on model release, documentation, and community standards. Suppression required to maintain sunset timeline: platform monopoly pricing, strategic model withholding, API terms-of-service designed to prevent self-hosting comparisons.
constraint_indexing:constraint_classification(average_is_over_2026, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: TRADITIONAL CREDENTIALING SYSTEMS (PITON) — Universities, professional licensing bodies, and certification programs are largely performative in the AI-talent barbell context. Elite hiring now depends on portfolio work, model capability demonstrations, and peer reputation rather than transcripts or degrees. Yet credentialing persists through institutional inertia: employers still require degrees (litigation risk, HR bureaucracy), gate-keepers maintain accreditation requirements (institutional survival depends on tuition revenue and alumni networks), and the prestige hierarchy of universities persists despite weak correlation to AI-era skill demonstration. Theater ratio is high (0.65): the credential validates nothing about AI capability but persists because alternatives haven't fully displaced it. No real suppression — the system simply hasn't been disrupted yet.
constraint_indexing:constraint_classification(average_is_over_2026, piton,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 7: NATION-STATES AND POLICYMAKERS (TANGLED ROPE) — States face a coordination problem: AI talent concentration in global tech hubs creates competitive disadvantage for countries with distributed talent. Genuine need for coordination on education, infrastructure investment, visa/immigration policy, and public R&D funding. But extraction is embedded in the structure: states that invest heavily in STEM pipelines create talent pools that migrate to higher-wage global markets (brain drain); concentration of AI services in a few jurisdictions creates dependency; unilateral policy changes face capital flight or tech-sector resistance. Exit constrained by capital mobility and network effects in tech hubs. Active enforcement via immigration policy, visa restrictions, regulatory capture by tech firms.
constraint_indexing:constraint_classification(average_is_over_2026, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From civilizational distance, the barbell appears as a natural consequence of exponential returns to scale and network effects in AI systems: whoever has access to the best models and largest datasets compounds advantages at accelerating rates. This appears immutable — a law-like outcome of information economics. However, the structural data reveals false summitry: identifiable beneficiaries extract from designed mechanisms (API pricing, credential requirements, visa restrictions, knowledge gatekeeping). The 'natural' framing obscures policy choices (open-source funding levels, visa policy, education investment, antitrust enforcement) that could reshape the constraint.
constraint_indexing:constraint_classification(average_is_over_2026, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(average_is_over_2026_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(average_is_over_2026, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(average_is_over_2026, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(average_is_over_2026, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(average_is_over_2026, TR),
    TR >= 0.70.

:- end_tests(average_is_over_2026_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate, trending toward high. The barbell creates asymmetric returns: elite-tier productivity gains do not translate to proportional wage increases (platform owners and venture capitalists capture much of the surplus), while median-skill workers face absolute income loss. The measurement trajectory (0.32 → 0.64) captures the acceleration of displacement as models improve and adoption spreads. Extraction is primarily economic (lost wages, credential inflation costs, migration barriers) rather than physical coercion. Suppression (0.68): High. Barriers to exit include: structural (requires 18-24 months of sustained learning and income loss for retraining), institutional (credential requirements, visa gatekeeping, information asymmetries about model capabilities), and psychological (internalized beliefs about AI being 'magic' accessible only to PhDs). Suppression is not exogenously maintained—it emerges from the rational incentives of employers and platforms to automate rather than retrain median-skill workers. Theater ratio (0.35): Low. The barbell is driven by genuine economic incentive structures (automation reduces costs, AI-augmented workers are 5-50x more productive) rather than performative mechanisms. Credentialing persists as theater, but the core constraint is functional—the economic incentives are real. Rising theater ratio over the interval (0.22 → 0.38) reflects increasing reliance on certification-program theater as employers use credentials as proxy signals when actual capability assessment is costly.
 *
 * PERSPECTIVAL GAP:
 *   The analytical observer risks viewing the barbell as a natural law (Mountain)—an immutable consequence of exponential returns and network effects in information economics. But the structural data reveals false summitry: identifiable beneficiaries (elite workers, platform owners, capital allocators), identifiable victims (median-skill workers, displaced cohorts), and active enforcement mechanisms (visa policy, credential requirements, platform terms-of-service). The barbell appears 'natural' from the elite perspective (experiencing genuine coordination gains) and 'natural' from the capital allocator perspective (capital flows rationally to highest-return investments). But from the victim's perspective, it is extraction—the structure could be reshaped by policy (education investment, open-source funding, visa liberalization, antitrust enforcement, wage insurance) but is not being reshaped because beneficiaries have political power. The scaffold perspective sees the open-source coalition building an exit path (5-10 year sunset), but the sunset timeline depends on policy support that may not materialize. The piton perspective sees credentialing as degraded theater—degrees validate nothing about AI capability but persist through institutional inertia.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values (d) derive from each agent's structural position relative to the extraction flow. Elite workers with arbitrage exit options (always have offers) experience low d (~0.15), resulting in negative f(d) and low effective extraction chi despite high base extractiveness—they benefit from the constraint. Median-skill workers with trapped exit options (cannot afford retraining, face displacement) experience high d (~0.92), resulting in high f(d) and maximum chi—extraction is severe from their perspective. Transition cohorts with constrained exit (can retrain but at high cost) experience moderate d (~0.55), resulting in moderate chi—the constraint is partially extractive, partially enabling. Platform owners with constrained exit (locked into their own platforms through switching costs and network effects) experience d ~0.40, resulting in moderate positive chi—they benefit overall but face lock-in. The scaffold coalition with organized exit (building alternatives) experiences d ~0.35, resulting in lower chi—they have agency and see a path forward. Policymakers with mobile exit options in policy levers experience moderate d (~0.60) but their classification depends on which policy levers they actually exercise; most are constrained by political economy factors.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION — Extractiveness is 0.58, below the 0.70 threshold requiring explicit mandatrophy_resolved, but the trajectory suggests it will reach high extractiveness by year 8-10. The constraint exemplifies the mandatrophy by presenting simultaneously as Rope (elite workers experiencing genuine coordination), Tangled Rope (multiple perspectives showing mixed coordination and extraction), and Snare (median workers experiencing pure extraction). The classification varies across perspectives, not because of measurement ambiguity but because the constraint structurally benefits some agents while harming others. The mandatrophy is resolved by recognizing that TANGLED ROPE is the accurate summary classification: the constraint provides genuine coordination benefits (elite productivity gains, problem-solving capacity) while simultaneously extracting from median-skill workers through displacement and credential inflation. The beneficiaries (elite workers, platforms) have stronger political economy than the victims (dispersed, unorganized median workers), so the extraction persists even when policy alternatives exist that could reshape the structure. The false summit risk is high—analysts may naturalize the barbell as inevitable AI economics rather than contingent policy choice. The open-source coalition represents a genuine alternative pathway (Scaffold logic with sunset), but policy choices (funding levels, visa policy, education investment) determine whether the sunset materializes.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    skill_transfer_velocity,
    'Can transition cohorts acquire elite-tier AI capabilities fast enough to remain economically viable before displacement accelerates?',
    'Longitudinal tracking of retraining cohort employment outcomes; comparison of time-to-productivity for different learning pathways; correlation between bootcamp completion and sustainable employment at above-median wages',
    'If velocity < 18 months: transition paths are illusory; snare perspective dominates and suppression increases. If velocity > 24 months: retraining becomes structurally infeasible during typical job-search window; snare classification confirmed. If velocity 18-24 months: tangled rope dynamics genuine — constrained but possible exits exist.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(skill_transfer_velocity, empirical, 'Whether retraining velocity permits viable transition for median-skill workers').

omega_variable(
    open_source_capability_parity,
    'Do open-source models and local deployment achieve functional parity with proprietary platforms within the 5-10 year scaffold sunset window?',
    'Performance benchmarking of open-weights models vs proprietary flagship systems; cost analysis of local GPU infrastructure vs API pricing; user migration tracking from proprietary to open platforms',
    'If parity achieved: scaffold perspective validated; platform lock-in breaks and extraction mechanism loses force. If parity not achieved: open-source remains speculative alternative; scaffold is aspirational, not structural.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(open_source_capability_parity, empirical, 'Whether open-source models achieve functional parity with proprietary systems').

omega_variable(
    wage_bifurcation_permanence,
    'Is the dual-tier wage structure temporary (10-20 year transition) or permanent (civilization-scale separation)?',
    'Historical comparison to prior technological transitions (electricity, computing, internet); analysis of whether retraining cohorts stabilize at median tier or continue cascading downward; measurement of intergenerational mobility for children of displaced workers; policy intervention outcomes (education funding, visa policy, wage support)',
    'If temporary: constraint is Scaffold or Piton (degraded system being replaced). If permanent: constraint is Mountain (immutable consequence of exponential returns) or Snare (sustained extraction mechanism). Policy choice (antitrust, open-source funding, education investment) determines outcome within empirical bounds.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(wage_bifurcation_permanence, empirical, 'Whether wage bifurcation is temporary or permanent').

omega_variable(
    coordination_deficit_vs_market_failure,
    'Is the barbell structure a coordination problem (market could solve with right incentives) or an inherent market failure (no set of incentives produces equitable outcome)?',
    'Mechanism design analysis: can targeted education subsidies, wage insurance, or geographic redistribution policies produce sustainable broad-based AI literacy? Or do network effects and capital concentration make dispersed talent development economically irrational from investor perspective?',
    'If coordination problem: policy intervention (public investment, visa liberalization, education funding) can reshape constraint toward Rope or Scaffold. If market failure: constraint may be immutable given capital markets; reframing as Mountain or Snare may be structural rather than analytical.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_deficit_vs_market_failure, conceptual, 'Whether barbell is coordination problem or structural market failure').

omega_variable(
    elite_tier_saturation,
    'Does elite-tier talent face its own capacity ceiling? Do returns to AI-augmentation continue exponentially, or plateau at some talent-to-model ratio?',
    'Measurement of productivity gains for elite cohorts over time; identification of diminishing returns thresholds; analysis of whether elite-tier job creation scales with productivity gains or whether productivity gains concentrate in fewer positions',
    'If plateau emerges: elite tier faces internal differentiation and potential saturation; secondary tiers may develop as elite productivity gains slow. If exponential continues: extraction mechanism remains structural for decades. Returns plateau would suggest barbell is transitory (Scaffold logic); continued exponential returns suggest permanence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(elite_tier_saturation, empirical, 'Whether elite-tier productivity gains face saturation or continue exponential growth').

omega_variable(
    false_summit_natural_law,
    'Is the barbell structure a natural consequence of exponential returns (immutable law) or a constructed institutional arrangement (contingent, reversible)?',
    'Policy counterfactual analysis: identifying which policy levers (open-source funding, education investment, visa liberalization, antitrust enforcement, wage support) could reshape the structure. If multiple levers exist with material effect: false summit (constructed). If structure persists despite policy intervention: genuine mountain (natural law).',
    'If false summit confirmed: the ''naturalness'' framing is cover for policy choices benefiting elite-tier agents and platform owners. Reclassify perspective and reframe as Snare or Tangled Rope from analytical perspective. If genuine mountain: constraint is immutable and policy focus should shift to adaptation and damage mitigation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(false_summit_natural_law, conceptual, 'Whether barbell is natural law or constructed institutional arrangement (FSM candidate)').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(average_is_over_2026, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(aio_tr_t0, average_is_over_2026, theater_ratio, 0, 0.22).
narrative_ontology:measurement(aio_tr_t2, average_is_over_2026, theater_ratio, 2, 0.28).
narrative_ontology:measurement(aio_tr_t4, average_is_over_2026, theater_ratio, 4, 0.35).
narrative_ontology:measurement(aio_tr_t6, average_is_over_2026, theater_ratio, 6, 0.38).

% Extraction over time
narrative_ontology:measurement(aio_be_t0, average_is_over_2026, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(aio_be_t2, average_is_over_2026, base_extractiveness, 2, 0.45).
narrative_ontology:measurement(aio_be_t4, average_is_over_2026, base_extractiveness, 4, 0.58).
narrative_ontology:measurement(aio_be_t6, average_is_over_2026, base_extractiveness, 6, 0.64).

% Suppression requirement over time
narrative_ontology:measurement(aio_su_t0, average_is_over_2026, suppression_requirement, 0, 0.52).
narrative_ontology:measurement(aio_su_t2, average_is_over_2026, suppression_requirement, 2, 0.6).
narrative_ontology:measurement(aio_su_t4, average_is_over_2026, suppression_requirement, 4, 0.68).
narrative_ontology:measurement(aio_su_t6, average_is_over_2026, suppression_requirement, 6, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(average_is_over_2026, resource_allocation).
narrative_ontology:affects_constraint(average_is_over_2026, credential_inflation_spiral).
narrative_ontology:affects_constraint(average_is_over_2026, geographic_brain_drain).
narrative_ontology:affects_constraint(average_is_over_2026, platform_lock_in_dynamics).
narrative_ontology:affects_constraint(average_is_over_2026, open_source_model_access).

% DUAL FORMULATION NOTE:
% The AI-talent barbell decomposes into multiple constraint stories: (1) Elite-tier productivity gains and coordination benefits (Rope, ε=0.15); (2) Median-skill displacement and retraining barriers (Snare, ε=0.72); (3) Platform lock-in and API pricing extraction (Snare, ε=0.65); (4) Open-source competition and platform sunset pathway (Scaffold, ε=0.30, 5-10 year horizon); (5) Credential inflation and signaling cascades (Piton, ε=0.35). The aggregate barbell constraint (this story, ε=0.58, Tangled Rope) integrates these components. Each decomposed story has different ε values and different measurement trajectories. The barbell story links them through network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(average_is_over_2026, institutional, 0.4).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
