% ============================================================================
% CONSTRAINT STORY: work_displacement_dignity
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-01-09
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_work_displacement_dignity, []).

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
 *   constraint_id: work_displacement_dignity
 *   human_readable: AI-Driven Work Displacement and Human Dignity
 *   domain: catholic_social_teaching/technology_ethics/labor_economics
 *
 * SUMMARY:
 *   AI-driven automation threatens the dignity-conferring function of work
 *   that Catholic Social Teaching identifies as essential to human
 *   flourishing, family stability, and social participation. The constraint
 *   operates through labor market displacement, wage suppression via
 *   automation threat, and degradation of remaining work into precarious gig
 *   arrangements under algorithmic management. CST's encyclical Antiqua et
 *   Nova frames this as a conflict between the 'technocratic paradigm'
 *   (efficiency, profit maximization, instrumental rationality) and 'human
 *   primacy' (dignity, solidarity, common good). The structural tension:
 *   capital captures productivity gains while workers bear adjustment costs
 *   (displacement, retraining burden, wage stagnation, loss of bargaining
 *   power) that are inadequately compensated by existing social safety nets
 *   or retraining programs. The coordination story (efficiency gains,
 *   consumer benefits, innovation) naturalizes extraction as technological
 *   progress. Alternatives (worker ownership, democratic AI governance,
 *   universal basic income, job guarantees) exist but are suppressed by
 *   capital mobility, political capture, and ideological framing of market
 *   outcomes as inevitable. The constraint's theater_ratio (0.58) reflects
 *   that retraining programs and corporate responsibility pledges are
 *   substantially performative: completion rates are low, job matching is
 *   poor, funding is inadequate, and the programs serve more to legitimate
 *   displacement than to restore dignified work. The measurements show
 *   extraction and suppression rising sharply from 2010-2019 (AI adoption
 *   acceleration) then plateauing at high levels, while theater ratio rises
 *   in parallel as performative responses proliferate without structural
 *   change.
 *
 * KEY AGENTS:
 *   - Displaced Workers: Primary victims (powerless/trapped) — face job loss with minimal retraining access, age/skill barriers, inadequate safety nets; cannot exit labor market; experience maximum extraction through loss of livelihood, dignity, family stability
 *   - Precarious Workers: Primary victims (powerless/identity_locked) — remain employed in degraded conditions (gig economy, algorithmic management, wage suppression); identity fused with occupational role; exit requires abandoning social world
 *   - Mid-Career Professionals: Mixed position (moderate/constrained) — benefit from AI augmentation but face skill obsolescence risk and credential treadmill; constrained by family obligations and industry-specific human capital
 *   - Capital Owners: Primary beneficiaries (powerful/arbitrage) — capture productivity gains as profit; experience pure coordination; externalize displacement costs to workers and society
 *   - AI Platform Companies: Primary beneficiaries (institutional/arbitrage) — capture market share, data, network effects; externalize social costs; naturalize extraction as innovation
 *   - Labor Unions / Social Movements: Organized resistance (organized/constrained) — building alternative frameworks (UBI, job guarantees, worker ownership, AI governance); constrained by political opposition and capital mobility but have agency and theory of change
 *   - CST Magisterium: Institutional actor (institutional/constrained) — provides moral framework critiquing technocratic paradigm; benefits institutionally from moral arbiter role; constrained by secular state sovereignty and capital mobility; mixed coordination and extraction
 *   - Analytical Observer: Civilizational view (analytical/analytical) — sees through efficiency narrative to extraction mechanism; identifies suppression of alternatives and coercive persistence
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(work_displacement_dignity, 0.68).
domain_priors:suppression_score(work_displacement_dignity, 0.72).
domain_priors:theater_ratio(work_displacement_dignity, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(work_displacement_dignity, extractiveness, 0.68).
narrative_ontology:constraint_metric(work_displacement_dignity, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(work_displacement_dignity, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(work_displacement_dignity, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(work_displacement_dignity, resistance, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(work_displacement_dignity, snare).
narrative_ontology:human_readable(work_displacement_dignity, "AI-Driven Work Displacement and Human Dignity").
narrative_ontology:topic_domain(work_displacement_dignity, "catholic_social_teaching/technology_ethics/labor_economics").

domain_priors:requires_active_enforcement(work_displacement_dignity).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(work_displacement_dignity, '7257c56c-91fe-456d-a368-a29185e1e85b').
narrative_ontology:cs_kernel_codification('7257c56c-91fe-456d-a368-a29185e1e85b', formalized).
narrative_ontology:cs_authority_grounding('7257c56c-91fe-456d-a368-a29185e1e85b', lineage).
narrative_ontology:cs_interpretation_layer_present('7257c56c-91fe-456d-a368-a29185e1e85b').
narrative_ontology:cs_created_at('7257c56c-91fe-456d-a368-a29185e1e85b', '2025-01-09T00:00:00Z').

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(work_displacement_dignity, capital_owners).
narrative_ontology:constraint_beneficiary(work_displacement_dignity, ai_platform_companies).
narrative_ontology:constraint_beneficiary(work_displacement_dignity, high_skill_workers).
narrative_ontology:constraint_victim(work_displacement_dignity, displaced_workers).
narrative_ontology:constraint_victim(work_displacement_dignity, precarious_workers).
narrative_ontology:constraint_victim(work_displacement_dignity, youth_entering_labor_market).
narrative_ontology:constraint_victim(work_displacement_dignity, family_stability).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DISPLACED WORKER (SNARE) — Faces job loss to automation with minimal retraining access, age/skill barriers to re-employment, and inadequate social safety nets. Cannot exit the labor market (biographical survival depends on income) and has no alternative to the automating economy. Experiences maximum extraction: loss of livelihood, dignity, family stability, and social participation. The coordination story (efficiency gains, consumer benefits) is cover for asymmetric extraction.
constraint_indexing:constraint_classification(work_displacement_dignity, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: PRECARIOUS WORKER (SNARE) — Remains employed but in degraded conditions: gig economy, algorithmic management, wage suppression via automation threat. Structurally could exit to other precarious work but identity is fused with occupational role and community (e.g., truck driver, retail worker). Exit would require abandoning not just the job but the identity and social world built around it. Experiences high extraction through wage stagnation, loss of bargaining power, and erosion of work quality while automation threat disciplines labor.
constraint_indexing:constraint_classification(work_displacement_dignity, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(regional))).

% PERSPECTIVE 3: MID-CAREER PROFESSIONAL (TANGLED ROPE) — Benefits from AI tools (productivity gains, augmentation) but faces skill obsolescence risk and credential treadmill pressure. Constrained by mortgage, family obligations, and industry-specific human capital. Experiences mixed extraction: genuine coordination (AI as tool) alongside extraction (constant reskilling burden, job insecurity, wage pressure from automation threat). The constraint both enables and extracts.
constraint_indexing:constraint_classification(work_displacement_dignity, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: CAPITAL OWNER (ROPE) — Captures productivity gains from automation as profit. Experiences the constraint as pure coordination: AI adoption solves the problem of labor costs and enables capital accumulation. Has arbitrage-level exit (can shift capital across sectors, geographies, asset classes). Net beneficiary with negligible extraction. The labor displacement is externalized to workers and society.
constraint_indexing:constraint_classification(work_displacement_dignity, rope,
    context(agent_power(powerful),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: AI PLATFORM COMPANY (ROPE) — Captures market share, data, and network effects from AI deployment. Experiences the constraint as coordination: automation solves customer problems and creates new markets. Has arbitrage-level exit (can pivot to other technologies, markets, business models). Net beneficiary. The social costs of displacement are externalized; the efficiency narrative naturalizes extraction as progress.
constraint_indexing:constraint_classification(work_displacement_dignity, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: LABOR UNION / SOCIAL MOVEMENT (SCAFFOLD) — Organized agents (unions, worker centers, social justice coalitions) see the displacement crisis as a temporary coordination failure with a sunset: universal basic income, job guarantees, worker ownership models, and AI governance frameworks are being built to reclaim work as a site of dignity rather than extraction. Constrained by political opposition and capital mobility but has agency and sees an exit path. Experiences moderate extraction because the coalition has organizing capacity and a theory of change.
constraint_indexing:constraint_classification(work_displacement_dignity, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: CST MAGISTERIUM (TANGLED ROPE) — The Church's teaching authority experiences the constraint as mixed coordination and extraction. Coordination function: CST provides moral framework for critiquing technocratic paradigm and defending human dignity. Extraction: the Church benefits institutionally from positioning itself as moral arbiter of AI governance (relevance, authority, influence) while the structural power to enforce its principles is constrained by secular state sovereignty and capital mobility. The encyclical itself is both genuine moral witness and institutional positioning. Requires active enforcement (moral suasion, political advocacy) to hold.
constraint_indexing:constraint_classification(work_displacement_dignity, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER (SNARE) — From a civilizational perspective, AI-driven work displacement is a snare: the coordination story (efficiency, innovation, consumer welfare) is cover for asymmetric extraction from labor to capital. Alternatives (worker ownership, democratic AI governance, universal basic income, job guarantees) are suppressed by capital mobility, political capture, and ideological naturalization of market outcomes. The constraint persists through coercion (economic dependency, inadequate safety nets) and suppression of exits (retraining access barriers, geographic immobility, credential inflation). Identifiable victims exist and bear costs disproportionately. The analytical perspective sees through the efficiency narrative to the extraction mechanism.
constraint_indexing:constraint_classification(work_displacement_dignity, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(work_displacement_dignity_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(work_displacement_dignity, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(work_displacement_dignity, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(work_displacement_dignity, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(work_displacement_dignity_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. Capital captures productivity gains from automation while workers bear adjustment costs (job loss, wage stagnation, retraining burden, precarity) that are inadequately compensated. The labor share of income has declined as AI adoption has accelerated. Wage suppression operates even for workers who remain employed, as automation threat disciplines labor and erodes bargaining power. The extraction is asymmetric and substantial but not total — some workers benefit from augmentation, and some retraining programs provide genuine pathways. Suppression (0.72): High. Alternatives to extractive automation (worker ownership, democratic AI governance, job guarantees, strong retraining with income support) are suppressed by capital mobility (firms can relocate to low-regulation jurisdictions), political capture (lobbying against worker protections), inadequate social safety nets (US/UK model vs. Nordic model), credential inflation (retraining requirements exceed actual job skill needs), and ideological naturalization (market outcomes framed as inevitable technological progress). Exit options for displaced workers are severely constrained: age discrimination, skill mismatch, geographic immobility, family obligations. Theater ratio (0.58): Moderate-high. Retraining programs are substantially performative: low completion rates (30-40%), poor job matching (retraining for non-existent jobs), inadequate funding (short duration, no income support), and serve primarily to legitimate displacement rather than restore dignified work. Corporate responsibility pledges (ethical AI principles, stakeholder capitalism rhetoric) are largely theater — not binding, not enforced, not measured. The theater has increased over the interval as AI adoption has accelerated and performative responses have proliferated without structural change. Accessibility collapse (0.35): Low-moderate. Alternatives to the extractive automation model are not fully collapsed — worker ownership models exist (Mondragon, platform cooperatives), some nations have stronger safety nets and retraining systems (Denmark, Sweden), and organized labor is building political coalitions for UBI and job guarantees. The alternatives are suppressed but not eliminated. Resistance (0.65): Moderate-high. The constraint meets substantial resistance from displaced workers, labor unions, social movements, and religious institutions (including CST). Strikes, organizing campaigns, political advocacy for worker protections, and moral critiques of the technocratic paradigm are active and visible. The resistance has not yet succeeded in structural change but is not negligible.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates extreme perspectival divergence. Capital owners and AI platform companies experience pure coordination (rope) — automation solves the problem of labor costs and creates profit opportunities. They see efficiency gains and innovation, with displacement costs externalized. Displaced and precarious workers experience pure extraction (snare) — they bear the costs of automation (job loss, wage stagnation, precarity) with minimal compensation, inadequate retraining, and no exit options. The coordination story is cover for asymmetric extraction. Mid-career professionals and the CST Magisterium experience mixed coordination and extraction (tangled_rope) — genuine benefits (productivity tools, moral framework) alongside real costs (skill obsolescence risk, institutional constraints on enforcement). Organized labor and social movements see a temporary problem with a sunset (scaffold) — they are building alternative frameworks (UBI, job guarantees, worker ownership) and have a theory of change, though constrained by political opposition. The analytical observer sees through the efficiency narrative to the extraction mechanism and identifies the constraint as a snare at the civilizational level. The gap between the capital owner's rope and the displaced worker's snare is the core structural fact: the same automation process that creates profit for capital destroys livelihoods for labor, and the asymmetry is naturalized as technological progress.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is determined by each agent's structural position relative to the extraction flow. Capital owners and AI platform companies are primary beneficiaries — they capture productivity gains and profit from automation. The engine derives low d (near 0.0) from beneficiary status + arbitrage exit, producing negative or near-zero effective extraction (they experience subsidy, not cost). Displaced workers and precarious workers are primary victims — they bear the costs of automation (job loss, wage stagnation, precarity) with minimal compensation. The engine derives high d (near 1.0) from victim status + trapped/identity_locked exit, producing maximum effective extraction. Mid-career professionals are mixed — they benefit from AI augmentation but face skill obsolescence risk and wage pressure. The engine derives moderate d from mixed beneficiary/victim status + constrained exit. Organized labor and the CST Magisterium are institutional actors with agency but constraints — they have organizing capacity and moral authority but face political opposition and capital mobility. The engine derives moderate d from their constrained exit and mixed structural position. The analytical observer has analytical exit and sees the full extraction structure, deriving d from the civilizational perspective that identifies asymmetric extraction as the core mechanism.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint resolves the mandatrophy by showing that the snare classification from the analytical and victim perspectives is not a misidentification of coordination as extraction. The coordination function (efficiency gains, productivity enhancement, consumer benefits) is real but asymmetrically distributed — capital captures the gains while labor bears the costs. The mandate (technological progress, innovation, economic growth) has not outlived its function from capital's perspective, but from labor's perspective the mandate was never aligned with their interests. The CST critique identifies this asymmetry: the technocratic paradigm treats efficiency as the sole criterion, naturalizing extraction as progress. The alternative (human primacy, dignity, solidarity, common good) would require structural change (worker ownership, democratic governance, strong safety nets) that is suppressed by capital mobility and political capture. The snare classification is accurate for the victims' structural position — they are trapped in an extractive arrangement with suppressed alternatives. The rope classification is accurate for the beneficiaries' structural position — they experience genuine coordination. The mandatrophy is resolved by recognizing that both are true from their respective perspectives, and the perspectival gap is the extraction mechanism.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    retraining_effectiveness_threshold,
    'At what scale and quality does retraining access convert the constraint from snare to tangled_rope for displaced workers?',
    'Longitudinal tracking of displaced worker outcomes: re-employment rates, wage recovery, job quality, time to re-employment. Cross-national comparison of retraining program effectiveness (Denmark/Sweden vs. US/UK models).',
    'If retraining is effective and accessible: constraint shifts toward tangled_rope (genuine coordination with extraction). If retraining is theater (low completion rates, poor job matching, inadequate funding): snare classification confirmed.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(retraining_effectiveness_threshold, empirical, 'Whether retraining access is genuine coordination or performative').

omega_variable(
    automation_substitution_vs_augmentation,
    'Is AI deployment primarily substituting for human labor (displacement) or augmenting it (productivity enhancement)?',
    'Sector-by-sector analysis of AI adoption patterns: task-level substitution vs. augmentation; employment elasticity to AI investment; wage effects conditional on skill level. Distinguish between genuine augmentation (workers using AI tools) and disguised substitution (AI tools enabling workforce reduction).',
    'If primarily augmentation: lower extractiveness, more perspectives see coordination. If primarily substitution: higher extractiveness, snare classification from more perspectives. Current evidence suggests substitution dominates in routine cognitive and manual tasks.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(automation_substitution_vs_augmentation, empirical, 'Whether AI adoption is primarily substitution or augmentation').

omega_variable(
    dignity_operationalization_ambiguity,
    'Does CST''s ''human dignity'' principle provide actionable constraints on AI deployment, or does its abstractness permit capture by efficiency narratives?',
    'Analysis of CST invocations in actual AI governance debates: do dignity claims translate into binding constraints (e.g., employment guarantees, worker ownership requirements) or remain aspirational rhetoric? Track whether CST-influenced policies differ structurally from secular efficiency-maximizing policies.',
    'If dignity principle is operationalizable: CST provides genuine alternative framework (scaffold perspective strengthened). If dignity remains abstract: CST becomes legitimation theater for extraction (piton risk).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dignity_operationalization_ambiguity, conceptual, 'Whether CST dignity principle constrains or legitimates extraction').

omega_variable(
    ubi_vs_job_guarantee_sufficiency,
    'Do universal basic income or job guarantee programs restore the dignity-conferring function of work, or do they merely subsidize survival while extraction continues?',
    'Pilot program evaluation: psychological well-being, social participation, family stability, sense of purpose among UBI/job guarantee recipients vs. traditional employment. Distinguish between income security (necessary) and meaningful work (CST''s dignity claim).',
    'If UBI/job guarantees restore dignity: scaffold sunset is real, constraint is transitional. If they provide only income security without meaning: extraction persists in new form, snare classification remains.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(ubi_vs_job_guarantee_sufficiency, empirical, 'Whether income support programs restore work''s dignity function').

omega_variable(
    capital_mobility_constraint_enforceability,
    'Can national or regional AI governance frameworks constrain capital mobility enough to enforce worker protections, or does capital flight render such frameworks unenforceable?',
    'Analysis of regulatory arbitrage patterns: do firms relocate AI operations to low-regulation jurisdictions when faced with worker protection requirements? Effectiveness of cross-border coordination (EU AI Act, OECD principles) in preventing race-to-bottom dynamics.',
    'If capital mobility is constrainable: organized labor and state actors have leverage (scaffold/tangled_rope). If capital mobility is unconstrained: suppression is structural and global, snare classification from more perspectives.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(capital_mobility_constraint_enforceability, empirical, 'Whether capital mobility undermines worker protection enforcement').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(work_displacement_dignity, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(work_disp_theater_2010, work_displacement_dignity, theater_ratio, 0, 0.35).
narrative_ontology:measurement(work_disp_theater_2013, work_displacement_dignity, theater_ratio, 3, 0.42).
narrative_ontology:measurement(work_disp_theater_2016, work_displacement_dignity, theater_ratio, 6, 0.5).
narrative_ontology:measurement(work_disp_theater_2019, work_displacement_dignity, theater_ratio, 9, 0.58).
narrative_ontology:measurement(work_disp_theater_2022, work_displacement_dignity, theater_ratio, 12, 0.58).
narrative_ontology:measurement(work_disp_theater_2025, work_displacement_dignity, theater_ratio, 15, 0.58).

% Extraction over time
narrative_ontology:measurement(work_disp_extract_2010, work_displacement_dignity, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(work_disp_extract_2013, work_displacement_dignity, base_extractiveness, 3, 0.52).
narrative_ontology:measurement(work_disp_extract_2016, work_displacement_dignity, base_extractiveness, 6, 0.61).
narrative_ontology:measurement(work_disp_extract_2019, work_displacement_dignity, base_extractiveness, 9, 0.68).
narrative_ontology:measurement(work_disp_extract_2022, work_displacement_dignity, base_extractiveness, 12, 0.68).
narrative_ontology:measurement(work_disp_extract_2025, work_displacement_dignity, base_extractiveness, 15, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(work_disp_suppress_2010, work_displacement_dignity, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(work_disp_suppress_2013, work_displacement_dignity, suppression_requirement, 3, 0.6).
narrative_ontology:measurement(work_disp_suppress_2016, work_displacement_dignity, suppression_requirement, 6, 0.66).
narrative_ontology:measurement(work_disp_suppress_2019, work_displacement_dignity, suppression_requirement, 9, 0.72).
narrative_ontology:measurement(work_disp_suppress_2022, work_displacement_dignity, suppression_requirement, 12, 0.72).
narrative_ontology:measurement(work_disp_suppress_2025, work_displacement_dignity, suppression_requirement, 15, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(work_displacement_dignity, resource_allocation).

% DUAL FORMULATION NOTE:
% This constraint is downstream of technocratic_paradigm_vs_human_primacy (the broader ideological conflict) and ai_governance_accountability (the institutional framework question). The work displacement constraint is one specific instantiation of the technocratic paradigm's extraction mechanism, focused on labor market impacts. The upstream constraints have their own extractiveness values reflecting the ideological and governance dimensions; this constraint has its own extractiveness reflecting the specific labor market asymmetry.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
