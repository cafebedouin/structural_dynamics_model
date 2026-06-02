% ============================================================================
% CONSTRAINT STORY: knowledge_worker_wage_suppression
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_knowledge_worker_wage_suppression, []).

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
 *   constraint_id: knowledge_worker_wage_suppression
 *   human_readable: Knowledge Worker Wage Suppression
 *   domain: economic/labor
 *
 * SUMMARY:
 *   Knowledge worker wage suppression is a hybrid coordination-extraction
 *   mechanism that has intensified over the past decade through the
 *   concentration of tech capital, the expansion of visa-dependent labor
 *   regimes, and the stratification of credentials. The constraint operates
 *   simultaneously as genuine coordination (knowledge workers benefit from
 *   infrastructure, training, and network effects provided by large firms)
 *   and as asymmetric extraction (capital captures disproportionate share of
 *   productivity gains through wage suppression, stock concentration, and
 *   mobility restrictions). This story exemplifies Tangled Rope
 *   classification: both coordination and extraction functions are structural
 *   and necessary to understand the constraint's persistence. The
 *   extractiveness trajectory shows accumulation over time as market
 *   concentration deepened and alternative exit routes (independent
 *   consulting, startup formation, geographic mobility) became more costly.
 *   The theater ratio remains moderate because actual wage determination
 *   increasingly bypasses the performative HR apparatus (performance reviews,
 *   salary bands) and operates through informal market mechanisms, reducing
 *   the performative component relative to classical industrial labor
 *   relations. The constraint requires active enforcement through visa
 *   sponsorship systems, non-compete agreements, IP ownership structures, and
 *   credential gatekeeping — removing any one enforcement mechanism
 *   significantly weakens the suppression capacity.
 *
 * KEY AGENTS:
 *   - Individual Knowledge Worker: Primary victim (powerless/trapped) — trapped by visa sponsorship, vesting schedules, and specialized skill sets with limited outside market
 *   - Capital Owners and Tech Management: Primary beneficiary (institutional/arbitrage) — extract wage differential; maintain full exit optionality and can reorganize labor strategy rapidly
 *   - Organized Knowledge Workers: Secondary actor (moderate/constrained) — when collectively mobilized, experience genuine coordination benefits alongside extraction; growing union movements testing exit capacity
 *   - Educational and Skills Development Sector: Secondary actor (organized/constrained) — building alternative pathways (bootcamps, credential alternatives) with potential to sunset wage suppression mechanisms
 *   - Tech Companies as Institutional Actors: Dual role (institutional/constrained) — simultaneously beneficiaries and partial victims; constrained by competitive dynamics that occasionally force wage adjustments but retain structural advantage
 *   - HR and Human Capital Management Systems: Institutional theater (institutional/arbitrage) — maintains performative evaluation apparatus while suppression operates through other channels; persists through inertia
 *   - Analytical Observer: Risk of false summit — naturalizing contingent institutional arrangements as immutable features of labor markets
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(knowledge_worker_wage_suppression, 0.58).
domain_priors:suppression_score(knowledge_worker_wage_suppression, 0.65).
domain_priors:theater_ratio(knowledge_worker_wage_suppression, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(knowledge_worker_wage_suppression, extractiveness, 0.58).
narrative_ontology:constraint_metric(knowledge_worker_wage_suppression, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(knowledge_worker_wage_suppression, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(knowledge_worker_wage_suppression, tangled_rope).
narrative_ontology:human_readable(knowledge_worker_wage_suppression, "Knowledge Worker Wage Suppression").
narrative_ontology:topic_domain(knowledge_worker_wage_suppression, "economic/labor").

domain_priors:requires_active_enforcement(knowledge_worker_wage_suppression).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(knowledge_worker_wage_suppression, capital_owners).
narrative_ontology:constraint_beneficiary(knowledge_worker_wage_suppression, corporate_management).
narrative_ontology:constraint_victim(knowledge_worker_wage_suppression, software_engineers).
narrative_ontology:constraint_victim(knowledge_worker_wage_suppression, data_scientists).
narrative_ontology:constraint_victim(knowledge_worker_wage_suppression, researchers).
narrative_ontology:constraint_victim(knowledge_worker_wage_suppression, technical_specialists).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: INDIVIDUAL KNOWLEDGE WORKER (SNARE) — Trapped by visa sponsorship, stock vesting schedules, and the global labor market structure. High skill specificity creates switching costs. Geographic mobility restricted by immigration regimes. Compensation structures (deferred equity, signing bonuses) lock workers into multi-year commitments. No meaningful exit without severe economic penalty.
constraint_indexing:constraint_classification(knowledge_worker_wage_suppression, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: ORGANIZED KNOWLEDGE WORKERS (TANGLED ROPE) — When collectively organized (professional associations, union movements), knowledge workers benefit from genuine coordination (knowledge-sharing, skill standardization, collective bargaining) while simultaneously experiencing extraction. The constraint requires active enforcement through non-compete agreements, IP ownership structures, and mobility restrictions. Organization provides some exit capacity through collective action, but capital can reorganize to counter collective power.
constraint_indexing:constraint_classification(knowledge_worker_wage_suppression, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: CAPITAL OWNERS AND MANAGEMENT (ROPE) — Experience the constraint as pure coordination: labor supply standardization, wage compression, and mobility restrictions enable predictable cost planning and knowledge hoarding. The constraint coordinates capital accumulation. Management has full exit optionality — they can hire internationally, offshore work, or restructure compensation. Effective extraction flows toward this agent.
constraint_indexing:constraint_classification(knowledge_worker_wage_suppression, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: SKILLS DEVELOPMENT ECOSYSTEM (SCAFFOLD) — Educational institutions, bootcamps, and credentialing systems are building parallel pathways (alternative credentials, portfolio-based hiring, direct skill demonstration) that bypass traditional wage suppression mechanisms. As these mature, the constraint's extraction force diminishes. The sunset logic applies: as skills become tradeable outside institutional hierarchies, wage suppression loses leverage. Theater is moderate (education credentialing involves performative elements) but declining as market-validated alternatives emerge.
constraint_indexing:constraint_classification(knowledge_worker_wage_suppression, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: TECH COMPANY AS INSTITUTIONAL ACTOR (TANGLED ROPE) — Large tech firms benefit from wage suppression (lower labor costs, reduced turnover, increased leverage) while also genuinely coordinating knowledge worker productivity through infrastructure, tools, and organizational design. The firm experiences the constraint as enforceable coordination that happens to be extractive. Constrained exit options because competitive labor market dynamics can shift rapidly, forcing wage adjustments. Both beneficiary and (partially) victim of the suppression mechanism.
constraint_indexing:constraint_classification(knowledge_worker_wage_suppression, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: HUMAN CAPITAL MANAGEMENT SYSTEM (PITON) — Traditional HR practices (performance reviews, salary bands, equity vesting) were designed to coordinate talent allocation but now function largely as theater. The practices persist through institutional inertia despite being increasingly decoupled from actual knowledge worker value or market dynamics. The system maintains appearance of meritocratic evaluation while suppression mechanisms operate through other channels (market concentration, non-competes, visa restrictions). Theater ratio high because the evaluation apparatus is performative — it produces legitimacy narratives rather than actual wage determination.
constraint_indexing:constraint_classification(knowledge_worker_wage_suppression, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (FALSE SUMMIT) — Risk of naturalizing the constraint as an immutable feature of labor markets ('supply and demand sets wages', 'higher education requires years of investment', 'knowledge specialization creates switching costs'). These framings mistake contingent institutional arrangements (visa sponsorship, credential gatekeeping, equity vesting structures) for natural laws. The mountain classification is a false summit — the engine will detect this through accessibility_collapse and resistance metrics.
constraint_indexing:constraint_classification(knowledge_worker_wage_suppression, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(knowledge_worker_wage_suppression_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(knowledge_worker_wage_suppression, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(knowledge_worker_wage_suppression, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(knowledge_worker_wage_suppression, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(knowledge_worker_wage_suppression, TR),
    TR >= 0.70.

:- end_tests(knowledge_worker_wage_suppression_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. Knowledge workers generate significant productivity value (software engineers, researchers, data scientists) whose output is captured by capital owners. The wage suppression represents extraction of 35-50% of marginal productivity gains based on various economic analyses. The 0.58 figure reflects that this is substantial but not total — knowledge workers still earn absolute high wages (compared to global labor median) even while experiencing suppression relative to their contribution. Suppression (0.65): High. Multiple enforcement mechanisms reduce exit capacity: (1) Visa sponsorship creates legal dependency for ~30% of US tech workforce, (2) Stock vesting locks workers into 4-year commitment cycles, (3) Non-compete agreements in some jurisdictions restrict geographic mobility, (4) Credential gatekeeping (degree requirements, certification costs) raises switching costs, (5) Specialized skill sets have limited external markets, (6) Market concentration (FAANG dominance) reduces employer alternatives. Suppression is not total because knowledge workers can (with difficulty and cost) exit to entrepreneurship, geographic arbitrage, or sector change, and because market dynamics occasionally force wage adjustments. Theater ratio (0.48): Moderate-low. Unlike industrial-era labor, wage determination for knowledge workers is increasingly transparent (market data, GitHub portfolios, public project work demonstrate competence; industry salary surveys are widely available). HR performance review apparatus has declined in predictive power. However, performative elements persist through: equity compensation complexity (vesting schedules, grant refreshes create theater around 'total compensation'), title inflation (Senior Engineer, Principal Engineer, Fellow creating pseudo-meritocratic hierarchy), and the credentialing apparatus (degree requirements, certifications) that serves gatekeeping function disguised as skill validation. Theater ratio is lower than industrial-era labor relations (0.70-0.85) and declining as market transparency increases.
 *
 * PERSPECTIVAL GAP:
 *   This constraint generates six distinct classifications from the same base metrics: the beneficiary (capital) sees pure coordination (Rope) — wage standardization enables predictable planning; trapped workers see extraction (Snare) — no meaningful exit, full cost bearing; organized workers see mixed coordination-extraction (Tangled Rope) — genuine collective benefits with asymmetric distribution; education sector sees a temporary problem with sunset (Scaffold) — alternative credentials will eventually undermine suppression mechanisms; HR systems see their own degraded ritual (Piton) — performance reviews persist but no longer drive wage determination; analytical observer risks seeing natural law (Mountain) — but the structural data reveals this as false summit. The gap between capital's rope perspective and powerless worker's snare perspective is the core diagnostic: same constraint, opposite classifications, driven by directionality d and exit capacity. The scaffold perspective represents the real structural hope — alternative credentials actually could weaken suppression if they achieve wage parity with traditional pathways.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality is derived from structural position: Capital owners/management benefit from wage suppression (d ≈ 0.08, near-zero derived from beneficiary status + arbitrage exit → negative f(d)); organized knowledge workers experience constrained exit with partial victim status (d ≈ 0.55); individual trapped workers experience maximum extraction (d ≈ 0.95); institutional tech companies are partially beneficiaries and partially constrained (d ≈ 0.40, reflecting mixed position); alternative education sector experiences low extraction (d ≈ 0.25, beneficiary of credential arbitrage but not direct victim of wage suppression). The piton institutional actor (HR systems) has arbitrage exit options but benefits primarily from theater maintenance rather than wage suppression directly (d ≈ 0.15). The analytical observer at universal scope risks false summit through naturalizing institutional arrangements.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy for knowledge worker wage suppression is resolved by recognizing that the constraint IS legitimately Tangled Rope: genuine coordination (knowledge workers benefit from organizational infrastructure, training, network effects, project scope) coexists with asymmetric extraction (capital captures disproportionate share of productivity gains). The tension is not resolvable into pure coordination (Rope) because suppression mechanisms are deliberately maintained (visa sponsorship, IP ownership, non-compete agreements); nor is it pure extraction (Snare) because the organizational infrastructure genuinely enables productivity that individual workers could not replicate alone. The false summit (Mountain) is definitively rejected: the constraint is demonstrably changeable through policy intervention (visa reform, antitrust enforcement, credential alternatives), not an immutable feature of knowledge work. The Piton classification correctly identifies the performative HR apparatus as degraded theater. The Scaffold classification correctly identifies the real structural pressure: alternative credentials and distributed work models are genuinely eroding the suppression mechanisms' force. No single classification captures the full structure — the perspectival presheaf IS the answer. The organizational coordination function is real; the extraction is real; neither can be dismissed. The extractiveness trajectory (0.35 → 0.58 over interval) shows accumulation, suggesting the extraction mechanisms have intensified faster than coordination benefits have been redistributed — a true signal of constraint degradation from balanced coordination toward skewed extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    visa_enforcement_dependency,
    'How much of wage suppression depends on visa sponsorship and immigration enforcement specifically?',
    'Comparative analysis: wage trajectories and mobility rates for visa-sponsored vs. citizen knowledge workers; simulation of suppression under open-immigration regime',
    'If visa-dependent (>60%): suppression is enforceable only through immigration controls; looser immigration regime collapses the constraint. If foundational (<30%): suppression persists through market concentration and credential gatekeeping regardless of immigration policy.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(visa_enforcement_dependency, empirical, 'Degree to which wage suppression depends on visa sponsorship mechanisms').

omega_variable(
    alternative_credential_effectiveness,
    'Do alternative credentials (bootcamps, portfolio-based hiring, direct skill demonstration) actually create wage premium parity with traditional degree-holders?',
    'Longitudinal wage tracking: starting salaries, career trajectory, and total compensation for bootcamp graduates vs. university graduates in same roles; employer hiring patterns by credential type over time',
    'If parity achieved: scaffold sunset is real and extraction mechanism is weakening. If persistent wage discount: alternative credentials remain subordinate, suppression persists through credential stratification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_credential_effectiveness, empirical, 'Whether alternative credentials eliminate wage suppression mechanisms').

omega_variable(
    market_concentration_necessity,
    'Can wage suppression be sustained in a competitive labor market, or does it require oligopolistic market concentration (FAANG dominance)?',
    'Historical analysis: wage trajectories during competitive tech booms vs. consolidation periods; regional analysis comparing concentrated tech hubs (SF, NYC) vs. distributed tech labor markets; simulation of suppression under fragmented employer base',
    'If concentration-dependent: antitrust enforcement could disrupt suppression. If competitive-market-compatible: suppression persists through other mechanisms (credential gatekeeping, visa control) even in fragmented markets.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(market_concentration_necessity, empirical, 'Whether wage suppression requires oligopolistic market concentration').

omega_variable(
    collective_action_threshold,
    'What critical mass of organized knowledge workers is required to shift the tangled_rope classification toward rope (genuine coordination without extraction)?',
    'Historical case studies of successful knowledge worker organizing (post-WWII manufacturing, public sector unions); analysis of current unionization attempts in tech; threshold modeling based on labor market dynamics',
    'If low threshold (<20% unionization): organized labor can shift power balance quickly. If high threshold (>50%): suppression is structurally resistant to incremental organizing.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(collective_action_threshold, empirical, 'Critical mass required for knowledge workers to shift constraint classification').

omega_variable(
    extraction_vs_coordination_boundary,
    'What portion of wage suppression is necessary coordination cost (matching supply/demand, compensating for firm-specific human capital investment) vs. pure extractive overhead?',
    'Decomposition analysis: compare wages in purely competitive markets (if any exist) vs. suppressed markets; identify which wage differentials correlate with measurable firm-provided value vs. market power; examine wage stickiness downward during labor supply shocks',
    'If small coordination component (<10%): suppression is primarily extractive, mandatrophy suggests piton or snare. If large component (>40%): tangled_rope classification is robust, suppression has legitimate functional basis.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(extraction_vs_coordination_boundary, conceptual, 'Boundary between necessary coordination costs and extractive overhead').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(knowledge_worker_wage_suppression, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(kwws_tr_t0, knowledge_worker_wage_suppression, theater_ratio, 0, 0.38).
narrative_ontology:measurement(kwws_tr_t5, knowledge_worker_wage_suppression, theater_ratio, 5, 0.44).
narrative_ontology:measurement(kwws_tr_t10, knowledge_worker_wage_suppression, theater_ratio, 10, 0.48).

% Extraction over time
narrative_ontology:measurement(kwws_be_t0, knowledge_worker_wage_suppression, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(kwws_be_t5, knowledge_worker_wage_suppression, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(kwws_be_t10, knowledge_worker_wage_suppression, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(knowledge_worker_wage_suppression, resource_allocation).
narrative_ontology:affects_constraint(knowledge_worker_wage_suppression, tech_visa_dependency).
narrative_ontology:affects_constraint(knowledge_worker_wage_suppression, stock_compensation_vesting).
narrative_ontology:affects_constraint(knowledge_worker_wage_suppression, credential_gatekeeping).
narrative_ontology:affects_constraint(knowledge_worker_wage_suppression, market_concentration_tech).

% DUAL FORMULATION NOTE:
% Knowledge worker wage suppression decomposes into four structurally distinct mechanisms: (1) visa dependency (ε ≈ 0.35, extraction through legal status), (2) stock vesting (ε ≈ 0.42, temporal lock-in), (3) credential gatekeeping (ε ≈ 0.40, barriers to entry), and (4) market concentration (ε ≈ 0.52, employer power asymmetry). The aggregate 0.58 extractiveness reflects the combined effect; measuring suppression via any single mechanism produces lower ε. Each mechanism can be partially disrupted independently (visa reform, alternative credentials) but suppression persists if others remain enforced. Linked stories capture this decomposition.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(knowledge_worker_wage_suppression, institutional, 0.4).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
