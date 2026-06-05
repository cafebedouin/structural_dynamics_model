% ============================================================================
% CONSTRAINT STORY: knowledge_worker_wage_compression
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_knowledge_worker_wage_compression, []).

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
 *   constraint_id: knowledge_worker_wage_compression
 *   human_readable: Knowledge Worker Wage Compression
 *   domain: labor_economics/compensation
 *
 * SUMMARY:
 *   Knowledge worker wage compression is the structural constraint that keeps
 *   compensation for credentialed labor (software engineers, lawyers,
 *   accountants, engineers, medical professionals) below the historical
 *   premium despite decades of rising educational requirements and years of
 *   skill investment. The constraint operates through multiple institutional
 *   enforcement mechanisms: immigration policy (H1B quotas, visa restrictions
 *   that limit offshore competition while allowing temporary outsourcing),
 *   professional licensing (credential recognition barriers that partition
 *   labor markets), and information technology (globalization of talent pools
 *   and offshoring of routine professional work). The constraint exhibits the
 *   full range of DR types from different perspectives because it genuinely
 *   performs coordination (reducing costs, allocating labor efficiently
 *   across borders) while extracting from knowledge workers themselves.
 *   Theater ratio has increased from 0.32 to 0.58 over the interval as the
 *   educational system maintains credential signaling despite wage
 *   collapse—universities still market degrees as economic investments,
 *   professional associations still gate-keep via licensing, but the actual
 *   wage premium has eroded. The extractiveness has risen from 0.35 to 0.58
 *   as offshoring and automation have intensified, making the wage
 *   compression deeper and more structural.
 *
 * KEY AGENTS:
 *   - Knowledge Workers: Primary victims (powerless/trapped) — trapped in careers requiring credential investment with compressed returns; limited exit options due to credential lock-in and visa restrictions
 *   - Mid-Career Professionals: Secondary victims (moderate/constrained) — experience mixed coordination (consumer benefits from low costs) and extraction (own wages compressed); have some negotiation and mobility capacity
 *   - Capital Holders & Employers: Primary beneficiaries (institutional/arbitrage) — directly benefit from lower labor costs; have full exit options (offshore, automate, relocate production)
 *   - Consuming Public: Secondary beneficiaries (moderate/mobile) — benefit from lower costs of professional services and information goods; mobile across consumption patterns
 *   - Organized Labor & Professional Associations: Organized actors (organized/constrained) — building protective mechanisms (licensing barriers, visa restrictions, credential recognition treaties) to sunset the compression
 *   - Educational Credentialing System: Institutional actor (institutional/arbitrage) — maintains credential signal despite wage collapse through institutional inertia; benefits from continued enrollment
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing the compression as immutable technological law rather than contingent policy choice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(knowledge_worker_wage_compression, 0.58).
domain_priors:suppression_score(knowledge_worker_wage_compression, 0.62).
domain_priors:theater_ratio(knowledge_worker_wage_compression, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(knowledge_worker_wage_compression, extractiveness, 0.58).
narrative_ontology:constraint_metric(knowledge_worker_wage_compression, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(knowledge_worker_wage_compression, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(knowledge_worker_wage_compression, tangled_rope).
narrative_ontology:human_readable(knowledge_worker_wage_compression, "Knowledge Worker Wage Compression").
narrative_ontology:topic_domain(knowledge_worker_wage_compression, "labor_economics/compensation").

domain_priors:requires_active_enforcement(knowledge_worker_wage_compression).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(knowledge_worker_wage_compression, capital_holders).
narrative_ontology:constraint_beneficiary(knowledge_worker_wage_compression, consuming_public).
narrative_ontology:constraint_victim(knowledge_worker_wage_compression, knowledge_workers).
narrative_ontology:constraint_victim(knowledge_worker_wage_compression, skill_differentiation_premium).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CREDENTIALED KNOWLEDGE WORKER (SNARE) — Trapped in global wage compression despite high skill investment. The worker has spent years acquiring specialized knowledge (CS degree, legal certification, medical training) expecting differentiated compensation. But global labor supply, offshoring, and automation undercut that premium. Exit options are severely constrained: retraining takes years and capital; relocation requires visa sponsorship; professional certification is jurisdiction-locked. The worker bears the full extraction — decades of below-expected returns on human capital investment.
constraint_indexing:constraint_classification(knowledge_worker_wage_compression, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: MID-CAREER PROFESSIONAL (TANGLED ROPE) — Experiences both coordination and extraction. The wage compression reflects genuine coordination: global labor markets do allocate resources efficiently, reducing costs for consumers and capital holders. The professional benefits from lower-cost inputs, services, and information goods — they are themselves a consumer. But they also bear extraction: their own compensation is compressed by the same forces that make inputs cheaper. They have more exit capacity than the trapped worker (can negotiate, switch sectors, migrate with experience) but faces real costs (opportunity cost, career disruption). The constraint requires active institutional enforcement through visa policies, labor law, and professional licensing to maintain the wage compression.
constraint_indexing:constraint_classification(knowledge_worker_wage_compression, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: CAPITAL HOLDERS & EMPLOYERS (ROPE) — Experience the constraint as beneficial coordination. Lower labor costs increase profitability and capital returns. The employer has exit options (can relocate production, offshore work, hire from global talent pool) and directly benefits from compression. No meaningful extraction experienced — the constraint subsidizes this agent's interests. The coordination function is real: global labor markets do reduce costs and increase efficiency.
constraint_indexing:constraint_classification(knowledge_worker_wage_compression, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: CONSUMING PUBLIC (ROPE) — Experiences pure coordination benefit. Knowledge worker wage compression lowers costs of professional services (legal, medical, software, consulting), IT goods, information products. The consumer mobile population can shift consumption patterns and benefits directly from lower prices. This agent experiences no extraction — the constraint coordinates cost reduction and broad-based consumption benefit.
constraint_indexing:constraint_classification(knowledge_worker_wage_compression, rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: ORGANIZED LABOR & PROFESSIONAL ASSOCIATIONS (SCAFFOLD) — See the constraint as a temporary crisis with potential sunset mechanisms. Licensing requirements, visa restrictions, credential recognition treaties, and union organization represent alternative verification pathways that could restore skill differentiation by reducing global labor competition. The organized coalition has agency and a generational timeframe to build these protections. Suppression is real (globalization, capital mobility, tech automation) but not absolute — protective policies could limit scope. Low effective extraction because the agent sees and is building an exit pathway.
constraint_indexing:constraint_classification(knowledge_worker_wage_compression, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: EDUCATIONAL CREDENTIALING SYSTEM (PITON) — The credential signal (degree, certification) persists despite degraded wage premium. Universities and professional boards continue producing and verifying credentials — the ritual is maintained through institutional inertia and social expectation rather than because it reliably predicts labor market value. Theater ratio is high: the educational system still signals competence and gatekeeps entry, but the economic return on that signal has collapsed due to global supply of credentialed workers. The constraint is performative — maintained because the alternatives (open hiring, skill tests, apprenticeships) haven't fully replaced the degree signal, not because it works.
constraint_indexing:constraint_classification(knowledge_worker_wage_compression, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / TECHNOLOGICAL DETERMINISM (MOUNTAIN) — From a civilizational perspective, wage compression for knowledge work is an immutable consequence of information technology and global communication: once knowledge becomes digitizable and transmissible, the scarcity rent on knowledge work cannot persist. The constraint appears as a natural law of economics — the skill premium must compress as supply increases. However, the structural data contradicts this naturalization: the compression is actively maintained through institutional choices (visa policy, offshoring incentives, professional licensing leverage). The mountain classification is a false summit revealing how technological determinism naturalizes contingent policy.
constraint_indexing:constraint_classification(knowledge_worker_wage_compression, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(knowledge_worker_wage_compression_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(knowledge_worker_wage_compression, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(knowledge_worker_wage_compression, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(knowledge_worker_wage_compression, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(knowledge_worker_wage_compression, TR),
    TR >= 0.70.

:- end_tests(knowledge_worker_wage_compression_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high and increasing. The constraint extracts from knowledge workers by suppressing their compensation below the historical skill premium while still requiring capital investment in credentials. The extractiveness has risen from 0.35 to 0.58 over 20 years as offshoring and automation have intensified, indicating accumulation rather than saturation. The extraction is real but not complete—knowledge workers still earn above subsistence and retain professional autonomy; hence 0.58 rather than 0.75+. Suppression (0.62): Moderate-high. Multiple barriers constrain workers' exit options: credential lock-in (years invested in specific professional qualification), visa restrictions (employment permits tied to specific employers or visa status), geographic barriers (professional licensing varies by jurisdiction), and skill obsoletion risk (rapid automation of routine knowledge work). But suppression is not total—some workers do migrate, retrain, or negotiate; some occupations maintain premiums (specialized medicine, patent law, elite tech). Theater ratio (0.58): Moderate-high and increasing. Educational institutions, professional boards, and employers all maintain performative signaling around credentials despite wage compression: universities market degrees as lifetime earnings investments (contradicted by actual wage data); professional associations tout licensing as quality assurance (while wages have compressed); employers emphasize 'we hire only top credentials' (while paying compressed rates). The theater has risen from 0.32 to 0.58 as the gap between credential value proposition and actual labor market outcomes has widened.
 *
 * PERSPECTIVAL GAP:
 *   The measurement gap between beneficiary and victim is stark. Capital holders see a coordination mechanism that improves allocation efficiency—wages are lower because global labor supply has increased, which is efficient. Knowledge workers see pure extraction—the constraint suppresses their compensation below their expected return on human capital investment. Both are correct about the coordination and extraction functions; the disagreement is about whether the extraction is justified by the coordination benefit. This gap cannot be resolved by better measurement of ε—it reflects genuine opposed interests. The tangled rope perspective is the most honest: yes, there is coordination (market efficiency), and yes, there is extraction (wage suppression below historical premium), and the two are structurally coupled. You cannot have the cost reduction without the wage compression.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values are derived from structural relationship to the wage compression. Capital holders receive low directionality (d ≈ 0.05) because they are full beneficiaries with arbitrage options—offshore labor, automation, and capital mobility all work in their direction. Trapped knowledge workers receive high directionality (d ≈ 0.95) because they are full targets with no exit: credential lock-in, visa restrictions, and lack of alternative skills mean they cannot escape the compression. Mid-career professionals receive moderate directionality (d ≈ 0.55) because they experience both benefit (as consumers) and cost (as workers with some negotiation capacity). The consuming public receives low directionality (d ≈ 0.15) because they are net beneficiaries—the compression subsidizes lower prices. Professional associations get higher directionality (d ≈ 0.60) when classified as victims because they bear the cost of credential devaluation and declining professional prestige, but lower (d ≈ 0.35) when classified as organized agents building alternative protections. The analytical observer gets d ≈ 0.72 as a default (observational position), but the mountain classification at analytical position is false because it naturalizes contingent policy.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by showing that tangled rope is the most accurate classification from the analytical perspective. The constraint performs genuine coordination (allocating labor efficiently across borders, reducing costs) while simultaneously extracting from knowledge workers (suppressing their compensation). The scaffold perspective is not wishful thinking—professional licensing, visa restrictions, and credential recognition barriers genuinely could restore some skill premium by limiting supply. But the sunset is not guaranteed; it requires organized action and policy change, not just market forces. The piton perspective reveals that the educational system is performatively maintaining credential signaling despite wage collapse—the degree still signals minimum competence and gates entry, but no longer predicts wage premium. The mountain perspective is a false summit: the constraint is not immutable law but a policy choice. If visa restrictions tightened, licensing recognition barriers rose, or apprenticeships became viable alternatives, the wage premium could be partially restored. The temporal measurements show increasing extractiveness and theater ratio over 20 years, indicating that the constraint is not in equilibrium—it is either accumulating (wage compression will deepen) or approaching a breaking point (organized labor will successfully reverse it). The omegas reveal critical uncertainties: if automation is the primary driver rather than offshoring, then even strict visa policy won't restore the premium. If geographic arbitrage is exhausting as offshore labor costs rise, the constraint may naturally collapse. The most likely scenario is mixed causation and partial sunset—some protective policies will succeed in limiting compression, but automation will continue reducing the absolute wage level for knowledge work.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    offshoring_versus_automation,
    'Is wage compression primarily driven by offshore labor supply or by automation/AI replacement of knowledge work?',
    'Longitudinal wage data decomposition: track wage trends for automation-exposed vs offshoring-exposed occupations; measure timing of compression onset relative to automation adoption vs offshore hiring waves; estimate elasticity of substitution between offshore labor and automation',
    'If primarily offshoring: constraint is coordination problem with possible sunset (visa restrictions, nearshoring, credential barriers could partially reverse). If primarily automation: constraint approaches mountain-like immutability (wage compression will continue regardless of offshoring policy). If mixed: scaffold sunset is partial but not complete.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(offshoring_versus_automation, empirical, 'Whether compression is driven by offshoring labor supply or automation').

omega_variable(
    skill_differentiation_collapse_threshold,
    'At what global talent pool size does the wage premium for knowledge workers collapse to near-subsistence levels? Does a threshold exist, or is compression continuous?',
    'Cross-national wage data for same occupations; analysis of wage convergence in countries with different ratios of educated workers to total population; modeling of labor market equilibrium with globalization parameter',
    'If sharp threshold: there is a stable interior equilibrium and institutional interventions can operate near it. If continuous: compression will persist until all knowledge work converges to marginal product, creating permanent extraction for all but the most scarce specialties.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(skill_differentiation_collapse_threshold, empirical, 'Whether wage compression has a convergence threshold or continuous slope').

omega_variable(
    credential_signaling_replacement,
    'Are alternative credentialing mechanisms (skills-based hiring, apprenticeships, portfolio assessment, continuous certification) actually replacing the degree signal, or is the education system using institutional inertia to maintain the signal''s gatekeeping function despite wage collapse?',
    'Labor market hiring data: measure adoption rate of skills-based hiring vs degree-required positions; track wage outcomes for degree-holders vs alternative-credential holders over 5-10 year windows; analysis of education system investment in signaling maintenance (marketing, credential enhancement, accreditation standards)',
    'If replacement is real: piton classification confirmed — the credential is performative ritual. If maintenance is dominant: piton is accurate and the educational system is enforcing extraction. If both: constraint is Piton with partial sunset (alternative signals rising but degree signal persisting).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(credential_signaling_replacement, empirical, 'Whether alternative credentials are replacing traditional degree signaling').

omega_variable(
    geographic_arbitrage_exhaustion,
    'Is offshore wage compression reaching saturation as lower-income countries develop and their domestic labor markets tighten, reducing the arbitrage available to capital?',
    'Time-series analysis of wage differentials between advanced and developing economies for same occupations; measurement of domestic wage inflation in major offshore labor pools (India, Philippines, Vietnam, Eastern Europe); tracking of capital relocation patterns and costs',
    'If arbitrage is exhausting: wage compression may reverse as offshore options become expensive, potentially collapsing the constraint. If infinite: there are always lower-cost labor pools and compression will continue indefinitely.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(geographic_arbitrage_exhaustion, empirical, 'Whether geographic wage arbitrage is reaching exhaustion').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(knowledge_worker_wage_compression, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(kwwc_tr_t0, knowledge_worker_wage_compression, theater_ratio, 0, 0.32).
narrative_ontology:measurement(kwwc_tr_t10, knowledge_worker_wage_compression, theater_ratio, 10, 0.45).
narrative_ontology:measurement(kwwc_tr_t20, knowledge_worker_wage_compression, theater_ratio, 20, 0.58).

% Extraction over time
narrative_ontology:measurement(kwwc_be_t0, knowledge_worker_wage_compression, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(kwwc_be_t10, knowledge_worker_wage_compression, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(kwwc_be_t20, knowledge_worker_wage_compression, base_extractiveness, 20, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(knowledge_worker_wage_compression, resource_allocation).
narrative_ontology:affects_constraint(knowledge_worker_wage_compression, professional_credential_devaluation).
narrative_ontology:affects_constraint(knowledge_worker_wage_compression, geographic_labor_arbitrage).

% DUAL FORMULATION NOTE:
% Knowledge worker wage compression is downstream of both geographic labor arbitrage (offshoring and visa policy) and technological substitution (automation of routine professional work). These upstream constraints feed into wage compression as a distinct constraint family. Each constraint in the family has different ε values reflecting different causal mechanisms.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(knowledge_worker_wage_compression, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
