% ============================================================================
% CONSTRAINT STORY: ai_research_globalization
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ai_research_globalization, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: ai_research_globalization
 *   human_readable: AI Research Globalization and Knowledge Extraction Asymmetry
 *   domain: technology/international_research_policy
 *
 * SUMMARY:
 *   AI research globalization presents a structural constraint where
 *   knowledge flows, model access, and research opportunity concentrate in
 *   high-income institutions while emerging market researchers bear costs of
 *   dependency without corresponding benefits. The constraint exhibits the
 *   full spectrum of DR classification: to the powerless emerging market
 *   researcher, it is a snare (no exit, maximum extraction); to developing
 *   nation institutions, it is a tangled rope (genuine coordination benefits
 *   coexist with asymmetric extraction); to high-income institutions, it is
 *   coordination (rope); to multinational AI companies, it is tangled rope
 *   (benefit + control); to open-source coalitions, it is a temporary
 *   scaffold with a sunset; to academic publishing, it is a degraded piton
 *   (theater > function); and to the civilizational analyst, it risks
 *   appearing as a natural law of global development until structural data
 *   reveals it as contingent on compute concentration, IP regimes, and visa
 *   policy. The extractiveness has increased from 0.35 to 0.58 over the
 *   measurement interval, driven by compute concentration and the
 *   accumulation of proprietary model advantages. Theater ratio increases
 *   from 0.42 to 0.58 as publication prestige maintains performance rituals
 *   despite shifted verification mechanisms (preprints, code release, model
 *   weights becoming primary validation).
 *
 * KEY AGENTS:
 *   - Emerging Market Researchers: Primary victims (powerless/trapped) — participate in global research but structurally dependent on high-income institutional access
 *   - Resource-Constrained Institutions: Secondary victims (moderate/constrained) — receive coordination benefits (training, access) but pay asymmetric extraction costs (brain drain, dependency)
 *   - High-Income Research Institutions: Primary beneficiaries (institutional/arbitrage) — capture talent, citations, and positioning advantages with minimal constraints
 *   - Multinational AI Companies: Secondary beneficiaries (powerful/constrained) — extract value through model provision and compute control while coordinating global research
 *   - Open-Source AI Coalition: Organized agents (organized/mobile) — building alternative pathways to reduce dependency asymmetry
 *   - Developing Nation Policymakers: Institutional actors (organized/constrained) — attempting to build local AI capacity in competition with brain drain
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing contingent asymmetry as inherent to global development
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_research_globalization, 0.58).
domain_priors:suppression_score(ai_research_globalization, 0.65).
domain_priors:theater_ratio(ai_research_globalization, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_research_globalization, extractiveness, 0.58).
narrative_ontology:constraint_metric(ai_research_globalization, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(ai_research_globalization, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_research_globalization, tangled_rope).
narrative_ontology:human_readable(ai_research_globalization, "AI Research Globalization and Knowledge Extraction Asymmetry").
narrative_ontology:topic_domain(ai_research_globalization, "technology/international_research_policy").

domain_priors:requires_active_enforcement(ai_research_globalization).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_research_globalization, high_income_research_institutions).
narrative_ontology:constraint_beneficiary(ai_research_globalization, multinational_ai_companies).
narrative_ontology:constraint_beneficiary(ai_research_globalization, compute_resource_providers).
narrative_ontology:constraint_victim(ai_research_globalization, emerging_market_researchers).
narrative_ontology:constraint_victim(ai_research_globalization, resource_constrained_institutions).
narrative_ontology:constraint_victim(ai_research_globalization, developing_nation_ai_capacity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EMERGING MARKET RESEARCHER (SNARE) — Trapped by dependence on open-source models, compute access, and publication pathways controlled by high-income research institutions. Cannot build competitive research programs without participating in ecosystems that extract their labor and insights. Knowledge contributions become products captured upstream. No alternative institutional pathway; exit means abandoning research career entirely.
constraint_indexing:constraint_classification(ai_research_globalization, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: DEVELOPING NATION RESEARCH INSTITUTION (TANGLED ROPE) — Receives genuine coordination benefits (access to frontier models, training, collaborative networks) while bearing asymmetric extraction (brain drain, resource costs, technology dependency). High cost to exit (loss of research capacity) but structurally possible (build independent AI capacity). Both coordination and asymmetric extraction present simultaneously.
constraint_indexing:constraint_classification(ai_research_globalization, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: HIGH-INCOME RESEARCH INSTITUTION (ROPE) — Experiences the globalization constraint as pure coordination: access to talent from emerging markets, distributed compute resources, and global citation networks all enable research advancement. No meaningful extraction experienced; all benefits. Can arbitrage to alternative research partnerships if needed. Net beneficiary with high exit optionality.
constraint_indexing:constraint_classification(ai_research_globalization, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: MULTINATIONAL AI COMPANY (TANGLED ROPE) — Benefits from global research talent and knowledge diffusion (coordination) while extracting value through compute provision, model control, and commercialization rights (asymmetric extraction). Constrained by regulatory environments and talent competition; cannot exit without losing market position but has significant agency.
constraint_indexing:constraint_classification(ai_research_globalization, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: OPEN-SOURCE AI COALITION (SCAFFOLD) — Organized actors (Meta, Hugging Face, open-source communities) see the globalization constraint as a temporary problem with structural solutions: democratized model access, capacity-building initiatives, and distributed training infrastructure are building alternative pathways that reduce dependency and asymmetry. Mobile exit (shift to open models) is becoming available. Sunset clause: 5-10 years as open-source parity with proprietary models increases.
constraint_indexing:constraint_classification(ai_research_globalization, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: ACADEMIC PUBLISHING SYSTEM (PITON) — The traditional gate-keeper role of academic publishing in AI research is increasingly decorative: preprints dominate dissemination, community validation via code release and reproducibility has replaced journal peer review as the verification mechanism, and direct author-to-practitioner communication through repositories and datasets supersedes journals. The system persists through inertia and hiring incentives despite minimal functional verification role. Theater ratio reflects this degradation.
constraint_indexing:constraint_classification(ai_research_globalization, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, knowledge asymmetries between regions with differential compute access and institutional maturity might be seen as inherent to global asymmetric development. However, structural data reveals this as false naturalization: the asymmetry is contingent on compute concentration, visa policy, IP regime design, and institutional network effects — not immutable laws. Engine identifies as false summit.
constraint_indexing:constraint_classification(ai_research_globalization, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ai_research_globalization_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(ai_research_globalization, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(ai_research_globalization, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(ai_research_globalization, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(ai_research_globalization, TR),
    TR >= 0.70.

:- end_tests(ai_research_globalization_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Rising over the interval from 0.35, reflecting compute concentration acceleration and the maturation of proprietary model advantages that create dependency. The value reflects genuine asymmetric extraction: emerging market researchers contribute labor, data, and insights that flow upstream to high-income institutions which build defensible model positions and citation advantage. Suppression (0.65): Moderate-high, driven by compute resource scarcity, visa policy restrictions, funding concentration, and the internalization of hierarchies (emerging market researchers self-select deference to high-income institutional leadership). Theater ratio (0.58): Increasing as traditional publication metrics maintain prestige despite preprints and code release becoming primary dissemination and validation mechanisms. The gap between what counts officially (journal publication) and how research actually circulates (arXiv, GitHub, Hugging Face) grows — theater increases. Claimed type (Tangled Rope): The constraint has genuine coordination (shared models, collaborative networks, knowledge diffusion) AND genuine extraction (brain drain, resource asymmetry, dependency). Both elements are structural and required.
 *
 * PERSPECTIVAL GAP:
 *   High-income institution (rope, immediate horizon, arbitrage exit) experiences the constraint as enabling: global access to talent, compute, and citation networks. They see 'AI research globalization' as beneficial coordination. Emerging market researcher (snare, generational horizon, trapped exit) experiences it as limiting: participation requires accepting subordinate role, resource constraints, and brain drain loss. Developing institution (tangled rope, biographical horizon, constrained exit) experiences both: genuine access to frontier knowledge AND genuine extraction through dependency. Open-source coalition (scaffold, generational horizon, mobile exit) sees the constraint as temporary — distributed models and training infrastructure are creating alternatives with 5-10 year sunset. Academic publishing system (piton) is a separate axis: the perception that journal publication validates AI research is increasingly decorative as preprints and code become primary validation, but journals persist through hiring incentives and institutional prestige rituals. Analytical observer at civilizational scope risks seeing the asymmetry as inherent to global development (mountain) — but structural data reveals it as contingent on compute concentration, IP regimes, visa policy, and institutional network effects.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality computation follows from beneficiary/victim declarations and exit options. High-income institutions benefit (d ≈ 0.10) with arbitrage exit → f(d) ≈ -0.01 → χ ≈ -0.01 × 1.0 × 1.2 = negative (they experience coordination subsidy). Emerging market researchers are victims (d ≈ 0.92) with trapped exit → f(d) ≈ 1.37 → χ ≈ 0.58 × 1.37 × 1.2 ≈ 0.96 (maximum experienced extraction). Developing institutions: victims with constrained exit (d ≈ 0.70) → f(d) ≈ 1.02 → χ ≈ 0.58 × 1.02 × 1.0 ≈ 0.59 (moderate-high extraction). Multinational companies: beneficiaries with constrained exit (d ≈ 0.45) → f(d) ≈ 0.56 → χ ≈ 0.58 × 0.56 × 1.2 ≈ 0.39 (moderate extraction due to constraints). Global scope σ(S) = 1.2 amplifies extractiveness across all perspectives; this reflects that the asymmetry operates at planetary scale and cannot be evaded by local action.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint resolves mandatrophy by showing that Tangled Rope is the correct analytical classification: genuine coordination (access to models, training, collaborative networks) coexists with genuine asymmetric extraction (brain drain, dependency, resource capture). The snare perspective from the powerless agent is their experiential reality, not a misclassification — they experience maximum extraction despite coordination benefits because their exit is trapped. The rope perspective from high-income institutions is also their experiential reality — they experience pure coordination despite the system being extractive overall. The mandatrophy is resolved by recognizing that the same structural constraint produces different experienced types depending on position: this is exactly what indexical classification is designed to capture. The analytic observer's temptation to see this as a natural law (mountain) is the false summit that the engine detects — the asymmetry is real but contingent, not immutable.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    compute_concentration_trajectory,
    'Will compute resource concentration continue to increase or converge toward geographic distribution?',
    'Tracking of GPU/TPU manufacturing capacity, energy infrastructure, and cloud compute pricing across regions; monitoring of domestic AI chip development (China, EU, India, Japan)',
    'If concentration increases: extraction mechanism strengthens; emerging market dependency deepens. If distributed: extraction weakens; independent research capacity becomes feasible.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(compute_concentration_trajectory, empirical, 'Trajectory of global compute resource concentration').

omega_variable(
    brain_drain_reversibility,
    'Is brain drain from emerging markets to high-income institutions reversible through improved local opportunity structures?',
    'Tracking of researcher mobility patterns; measurement of salary/opportunity/prestige gaps between regions; adoption of local AI policy incentives',
    'If reversible: emerging market capacity building can reduce dependency; snare classification downshifts to tangled rope. If irreversible: extraction mechanism becomes structural and generational.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(brain_drain_reversibility, empirical, 'Reversibility of researcher brain drain asymmetry').

omega_variable(
    open_model_convergence_timeline,
    'Will open-source AI models achieve functional parity with proprietary systems within 5-10 years, enabling the scaffold sunset?',
    'Comparative benchmarking of open vs proprietary models; adoption metrics in academic vs commercial settings; capability convergence curves',
    'If convergence occurs: scaffold sunset is real; dependency asymmetry weakens. If convergence stalls: proprietary advantage persists; dependency remains structural.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(open_model_convergence_timeline, empirical, 'Timeline for open-source AI model functional parity').

omega_variable(
    institutional_identity_lock_mechanism,
    'Are high-income institutions identity-locked to their role as research leaders, making them unable to accept distributed authority even if distributed capacity becomes available?',
    'Ethnographic analysis of institutional resistance to collaborative models; tracking of institutional identity statements and funding alignment with ''leadership'' positioning',
    'If identity-locked: institutions will maintain extractive control mechanisms even after necessity fades. If not: transition to distributed models becomes feasible.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(institutional_identity_lock_mechanism, conceptual, 'Whether institutional identity lock prevents adoption of distributed research models').

omega_variable(
    suppression_mechanism_source,
    'Is high suppression (0.65) primarily structural (compute/resource barriers) or internalized (emerging market researchers accept subordinate role)?',
    'Post-barrier removal analysis: if compute becomes accessible, do emerging market researchers exhibit increased independence or do they maintain deference patterns? Do younger researchers from emerging markets show different behaviors than established researchers?',
    'If structural: barrier removal (compute access, capital) solves the problem. If internalized: capacity building without identity/epistemic decolonization perpetuates extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_source, empirical, 'Source of suppression: structural vs internalized').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_research_globalization, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(airg_tr_t0, ai_research_globalization, theater_ratio, 0, 0.42).
narrative_ontology:measurement(airg_tr_t5, ai_research_globalization, theater_ratio, 5, 0.52).
narrative_ontology:measurement(airg_tr_t10, ai_research_globalization, theater_ratio, 10, 0.58).

% Extraction over time
narrative_ontology:measurement(airg_be_t0, ai_research_globalization, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(airg_be_t5, ai_research_globalization, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(airg_be_t10, ai_research_globalization, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_research_globalization, resource_allocation).
narrative_ontology:affects_constraint(ai_research_globalization, compute_resource_concentration).
narrative_ontology:affects_constraint(ai_research_globalization, researcher_visa_and_mobility_policy).
narrative_ontology:affects_constraint(ai_research_globalization, ai_model_ip_regimes).

% DUAL FORMULATION NOTE:
% AI research globalization is upstream of three specific structural constraints: compute concentration (the physical base), visa/mobility policy (the enforcement mechanism), and IP regime design (the control mechanism). Each constraint in the family has its own extractiveness value reflecting distinct observables. The globalization story is the integrated effect of all three; decomposed stories enable analysis of which mechanism is most constraining.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
