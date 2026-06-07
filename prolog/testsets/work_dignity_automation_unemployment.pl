% ============================================================================
% CONSTRAINT STORY: work_dignity_automation_unemployment
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_work_dignity_automation_unemployment, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: work_dignity_automation_unemployment
 *   human_readable: AI-Driven Automation and the Erosion of Work as Vocation
 *   domain: catholic_social_teaching/technology_ethics/labor_economics
 *
 * SUMMARY:
 *   AI-driven automation presents a structural challenge to Catholic Social
 *   Teaching's understanding of work as vocation, participation in creation,
 *   and path to human flourishing. The constraint operates at multiple
 *   scales: individual workers lose employment or autonomy; communities
 *   dependent on routine labor face economic collapse; the global economy
 *   concentrates wealth and decision-making power in capital owners and
 *   platform companies. The encyclical tradition (Rerum Novarum through
 *   Laudato Si' and recent AI-focused documents) identifies work's threefold
 *   dignity: subjective (expression of human person), objective (contribution
 *   to common good), and social (participation in community). AI automation
 *   as currently structured threatens all three: deskilling removes
 *   subjective dignity, algorithmic management fragments objective
 *   contribution, and unemployment or precarity erodes social participation.
 *   The constraint is not technology itself but the social arrangement: who
 *   owns the tools, who captures the productivity gains, and whether workers
 *   have voice in how automation is deployed. The measurements show rising
 *   extraction and suppression over 20 years (2000-2020) as AI capabilities
 *   expanded, platform business models matured, and labor protections
 *   weakened in many jurisdictions. Theater ratio rises modestly, reflecting
 *   the gap between corporate rhetoric about 'empowering workers' and actual
 *   deskilling and displacement.
 *
 * KEY AGENTS:
 *   - Displaced Workers: Primary victims (powerless/trapped) — face job loss with limited retraining access, geographic immobility, age discrimination
 *   - Deskilled Workers: Primary victims (powerless/identity_locked) — remain employed but under algorithmic management that strips vocational dignity; identity constituted through craft or care work now reduced to task execution
 *   - Mid-Skill Workers in Transition: Secondary victims (moderate/constrained) — face genuine reallocation challenge but bear asymmetric cost of retraining risk
 *   - Capital Owners: Primary beneficiaries (powerful/arbitrage) — capture productivity gains, reduce labor costs, maintain full exit options across sectors and geographies
 *   - AI Platform Companies: Primary beneficiaries (institutional/arbitrage) — provide genuine efficiency tools but extract value through platform rents and data accumulation
 *   - Labor Unions / Worker Cooperatives: Organized agents (organized/constrained) — building alternative pathways (worker ownership, algorithmic transparency, platform co-ops) with sunset logic
 *   - Catholic Magisterium: Institutional observer (institutional/constrained) — recognizes coordination function but identifies asymmetric extraction; calls for regulation and reorientation toward common good
 *   - Analytical Observer (CST Framework): Civilizational view (analytical/analytical) — classifies as snare based on violations of subsidiarity, solidarity, and work dignity
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(work_dignity_automation_unemployment, 0.68).
domain_priors:suppression_score(work_dignity_automation_unemployment, 0.72).
domain_priors:theater_ratio(work_dignity_automation_unemployment, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(work_dignity_automation_unemployment, extractiveness, 0.68).
narrative_ontology:constraint_metric(work_dignity_automation_unemployment, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(work_dignity_automation_unemployment, theater_ratio, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(work_dignity_automation_unemployment, snare).
narrative_ontology:human_readable(work_dignity_automation_unemployment, "AI-Driven Automation and the Erosion of Work as Vocation").
narrative_ontology:topic_domain(work_dignity_automation_unemployment, "catholic_social_teaching/technology_ethics/labor_economics").

domain_priors:requires_active_enforcement(work_dignity_automation_unemployment).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(work_dignity_automation_unemployment, 'abdac857-9f28-4baa-97c0-aee4aea46e42').
narrative_ontology:cs_kernel_codification('abdac857-9f28-4baa-97c0-aee4aea46e42', formalized).
narrative_ontology:cs_authority_grounding('abdac857-9f28-4baa-97c0-aee4aea46e42', lineage).
narrative_ontology:cs_interpretation_layer_present('abdac857-9f28-4baa-97c0-aee4aea46e42').
narrative_ontology:cs_created_at('abdac857-9f28-4baa-97c0-aee4aea46e42', '').

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(work_dignity_automation_unemployment, capital_owners).
narrative_ontology:constraint_beneficiary(work_dignity_automation_unemployment, ai_platform_companies).
narrative_ontology:constraint_beneficiary(work_dignity_automation_unemployment, high_skill_technical_workers).
narrative_ontology:constraint_victim(work_dignity_automation_unemployment, displaced_workers).
narrative_ontology:constraint_victim(work_dignity_automation_unemployment, deskilled_labor_force).
narrative_ontology:constraint_victim(work_dignity_automation_unemployment, communities_dependent_on_routine_labor).
narrative_ontology:constraint_vindicates(work_dignity_automation_unemployment, labor_market_efficiency_doctrine).
narrative_ontology:constraint_vindicates(work_dignity_automation_unemployment, technological_inevitability_thesis).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DISPLACED WORKER (SNARE) — Trapped by geographic immobility, skill obsolescence, and age discrimination. Retraining programs are inaccessible (cost, time, prerequisite education) or ineffective (training for jobs that will also automate). The constraint extracts livelihood, vocational identity, and community participation with no viable exit. Maximum experienced extraction.
constraint_indexing:constraint_classification(work_dignity_automation_unemployment, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: DESKILLED WORKER (SNARE) — Remains employed but under algorithmic management that fragments tasks, removes discretion, and subordinates human pace to machine rhythm. Identity-locked: professional identity was constituted through craft mastery or relational care work, now reduced to executing machine-dictated micro-tasks. Exit would require abandoning vocational identity entirely. Extraction is cognitive and spiritual — the work remains but its dignity is stripped.
constraint_indexing:constraint_classification(work_dignity_automation_unemployment, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(identity_locked),
            spatial_scope(regional))).

% PERSPECTIVE 3: MID-SKILL WORKER (TANGLED ROPE) — Faces genuine coordination problem (economy needs labor reallocation as technology shifts) but bears asymmetric cost. Retraining exists but requires financial sacrifice, time away from family, and risk of failure. Benefits from wage gains IF successfully retrained, but many do not clear the barrier. Coordination function is real; extraction is also real.
constraint_indexing:constraint_classification(work_dignity_automation_unemployment, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: CAPITAL OWNER (ROPE) — Automation solves genuine coordination problem: reducing labor costs, increasing productivity, enabling global competition. Experiences constraint as pure coordination — labor displacement is an externality borne by others. Net beneficiary with full exit options (can shift capital across sectors, geographies, asset classes).
constraint_indexing:constraint_classification(work_dignity_automation_unemployment, rope,
    context(agent_power(powerful),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: AI PLATFORM COMPANY (ROPE) — Provides tools that genuinely increase efficiency and solve coordination problems (logistics optimization, medical diagnosis support, language translation). Captures value through platform rents and data accumulation. Experiences constraint as coordination — the displacement and deskilling are side effects, not the business model's core function. Arbitrage-level exit: can pivot to new markets, geographies, or product lines.
constraint_indexing:constraint_classification(work_dignity_automation_unemployment, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: LABOR UNION / COOPERATIVE MOVEMENT (SCAFFOLD) — Organized agents building alternative pathways: worker ownership of automation tools, collective bargaining for algorithmic transparency, platform cooperatives, universal basic services. Sees current extraction as temporary — the sunset is worker control over technology rather than technology controlling workers. Constrained by political and capital barriers but has agency and a structural exit path.
constraint_indexing:constraint_classification(work_dignity_automation_unemployment, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: CATHOLIC MAGISTERIUM (TANGLED ROPE) — Recognizes genuine coordination function (technology can reduce drudgery, expand human capacity) but identifies asymmetric extraction (subordination of human dignity to efficiency, concentration of wealth, erosion of subsidiarity). Constrained by lack of direct political power but possesses moral authority. Calls for regulation, redistribution, and reorientation of technology toward common good. Mixed experience: coordination is real, extraction is real, enforcement of dignity-preserving norms is incomplete.
constraint_indexing:constraint_classification(work_dignity_automation_unemployment, tangled_rope,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER / CST FRAMEWORK (SNARE) — From a civilizational/universal perspective grounded in Catholic Social Doctrine, the constraint is a snare: AI-driven automation as currently structured violates subsidiarity (concentrates decision-making power), solidarity (externalizes costs onto the vulnerable), and the dignity of work (reduces labor to mere factor of production). The coordination story (efficiency gains) is real but insufficient to justify the extraction. The analytical classification is snare because the structural data shows identifiable victims, suppression of alternatives (worker ownership, democratic governance of AI), and extraction that exceeds coordination value.
constraint_indexing:constraint_classification(work_dignity_automation_unemployment, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(work_dignity_automation_unemployment_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(work_dignity_automation_unemployment, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(work_dignity_automation_unemployment, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(work_dignity_automation_unemployment, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(work_dignity_automation_unemployment_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. Capital owners and platform companies capture productivity gains while displaced workers bear costs of unemployment, retraining failure, and community collapse. Deskilled workers lose vocational dignity even when employed. The extraction exceeds coordination value because alternatives (worker ownership, democratic governance of AI, universal basic services) are suppressed. Suppression (0.72): High. Barriers include: capital concentration (workers cannot afford to own automation tools), regulatory capture (labor protections weakened), network effects (platform monopolies), retraining inaccessibility (cost, time, prerequisite education), and ideological naturalization (technological inevitability thesis). Worker ownership models and platform cooperatives face structural barriers, not just intrinsic inefficiency. Theater ratio (0.45): Moderate. Corporate rhetoric about 'empowering workers' and 'human-AI collaboration' coexists with actual deskilling, surveillance, and displacement. Retraining programs are announced but underfunded or ineffective. The theater is real but not dominant — much of the extraction is openly acknowledged as 'creative destruction' or 'labor market adjustment.'
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates extreme perspectival divergence. Capital owners and platform companies experience pure coordination (Rope) — automation solves genuine efficiency problems and they capture the gains. Displaced and deskilled workers experience pure extraction (Snare) — they bear costs with no exit and minimal benefit. Mid-skill workers and the Magisterium experience mixed coordination-extraction (Tangled Rope) — the efficiency gains are real but asymmetrically distributed. Labor unions and cooperatives see a temporary problem with a sunset (Scaffold) — worker ownership and democratic governance of AI are the exit path. The analytical observer using CST framework classifies as Snare because the structural data shows violations of subsidiarity (concentration of power), solidarity (externalization of costs), and work dignity (reduction of labor to factor of production). The gap between the capital owner's Rope and the displaced worker's Snare is the core extraction mechanism: one agent's coordination is another's dispossession.
 *
 * DIRECTIONALITY LOGIC:
 *   Displaced workers are full victims with trapped exit options — maximum directionality toward target (d ≈ 1.0), producing maximum effective extraction. Deskilled workers are victims with identity_locked exit — high directionality (d ≈ 0.85) because identity fusion prevents exit even when structural mobility exists. Mid-skill workers in transition are victims but with constrained exit (retraining is possible at high cost) — moderate-high directionality (d ≈ 0.65). Capital owners and platform companies are beneficiaries with arbitrage exit — directionality toward beneficiary (d ≈ 0.1), producing negative or minimal effective extraction (they experience the constraint as subsidy). Labor unions and cooperatives are organized agents building alternatives — moderate directionality (d ≈ 0.5) because they face real barriers but have agency. Catholic Magisterium is institutional observer with constrained exit (moral authority but limited political power) — moderate directionality (d ≈ 0.55). The analytical observer using CST framework has analytical exit but classifies as snare based on structural data, not personal experience.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint resolves mandatrophy by showing that 'work dignity' is not a single fixed property but a perspectival structure. From the capital owner's seat, automation is pure coordination — it solves the problem of labor costs and global competition. From the displaced worker's seat, it is pure extraction — livelihood and vocational identity are stripped with no viable alternative. From the CST analytical framework, it is a snare because the coordination story (efficiency) does not justify the extraction (dignity violation, wealth concentration, suppression of alternatives). The mandatrophy is not 'which type is correct?' but 'which perspective reveals the structural injustice?' The analytical classification as Snare is grounded in CST's normative framework: human dignity, common good, subsidiarity, and solidarity are non-negotiable constraints on economic arrangements. When automation violates these, it is extractive regardless of efficiency gains. The presheaf over observation sites includes the capital owner's Rope, the worker's Snare, and the analytical Snare — all are structurally true from their respective seats.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    retraining_effectiveness_threshold,
    'What proportion of displaced workers must successfully retrain for the constraint to qualify as coordination rather than extraction?',
    'Longitudinal tracking of displaced workers: employment outcomes, wage trajectories, and subjective well-being 5-10 years post-displacement. Compare successful retraining rates to baseline labor market mobility.',
    'If >60% successfully retrain with wage recovery: coordination function dominates, classification shifts toward tangled_rope from more perspectives. If <30%: extraction dominates, classification remains snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(retraining_effectiveness_threshold, empirical, 'Threshold for retraining success rates distinguishing coordination from extraction').

omega_variable(
    algorithmic_management_dignity_floor,
    'Is there a minimum level of worker discretion and autonomy below which algorithmic management categorically violates human dignity, regardless of wage or productivity gains?',
    'Philosophical and theological analysis within Catholic Social Teaching framework; empirical studies of worker well-being, mental health, and vocational satisfaction under varying degrees of algorithmic control.',
    'If a categorical floor exists and current systems violate it: the constraint is a snare from the CST perspective regardless of efficiency gains. If dignity is a continuous variable: the constraint is tangled_rope, balancing coordination and extraction.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(algorithmic_management_dignity_floor, conceptual, 'Whether human dignity sets a categorical floor on acceptable algorithmic management').

omega_variable(
    alternative_ownership_suppression,
    'Are worker ownership models and platform cooperatives genuinely suppressed by capital and regulatory structures, or do they fail due to intrinsic coordination costs?',
    'Comparative analysis of cooperative success rates in different regulatory environments; identification of legal and financial barriers to worker ownership; case studies of successful cooperatives (Mondragon, platform co-ops).',
    'If suppression is structural (legal barriers, capital access, network effects): snare classification confirmed. If cooperatives fail due to intrinsic inefficiency: rope classification gains support.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(alternative_ownership_suppression, empirical, 'Whether alternative ownership models are suppressed or intrinsically uncompetitive').

omega_variable(
    technological_determinism_vs_social_choice,
    'Is the current trajectory of AI-driven automation technologically inevitable, or is it the result of policy choices, capital allocation, and power structures that could be redirected?',
    'Historical analysis of technology adoption patterns; comparative policy analysis across nations with different labor protections and technology governance; identification of decision points where alternative paths were available.',
    'If technologically inevitable: mountain classification gains support (though false summit likely given beneficiary concentration). If socially constructed: snare classification confirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(technological_determinism_vs_social_choice, conceptual, 'Whether automation trajectory is technologically determined or socially constructed').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(work_dignity_automation_unemployment, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(theater_2000, work_dignity_automation_unemployment, theater_ratio, 0, 0.25).
narrative_ontology:measurement(theater_2005, work_dignity_automation_unemployment, theater_ratio, 5, 0.3).
narrative_ontology:measurement(theater_2010, work_dignity_automation_unemployment, theater_ratio, 10, 0.35).
narrative_ontology:measurement(theater_2015, work_dignity_automation_unemployment, theater_ratio, 15, 0.4).
narrative_ontology:measurement(theater_2020, work_dignity_automation_unemployment, theater_ratio, 20, 0.45).

% Extraction over time
narrative_ontology:measurement(extract_2000, work_dignity_automation_unemployment, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(extract_2005, work_dignity_automation_unemployment, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(extract_2010, work_dignity_automation_unemployment, base_extractiveness, 10, 0.52).
narrative_ontology:measurement(extract_2015, work_dignity_automation_unemployment, base_extractiveness, 15, 0.6).
narrative_ontology:measurement(extract_2020, work_dignity_automation_unemployment, base_extractiveness, 20, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(suppress_2000, work_dignity_automation_unemployment, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(suppress_2005, work_dignity_automation_unemployment, suppression_requirement, 5, 0.48).
narrative_ontology:measurement(suppress_2010, work_dignity_automation_unemployment, suppression_requirement, 10, 0.55).
narrative_ontology:measurement(suppress_2015, work_dignity_automation_unemployment, suppression_requirement, 15, 0.65).
narrative_ontology:measurement(suppress_2020, work_dignity_automation_unemployment, suppression_requirement, 20, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(work_dignity_automation_unemployment, resource_allocation).

% DUAL FORMULATION NOTE:
% This constraint is downstream of 'technocratic_paradigm_vs_human_dignity' (the broader structural pattern of subordinating human values to technical efficiency). The upstream constraint establishes the ideological and institutional context; this constraint instantiates it in the specific domain of labor and automation. The two constraints have different ε values: the upstream paradigm is more diffuse and harder to measure; this constraint has concrete observables (job displacement rates, wage trends, algorithmic management metrics).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
