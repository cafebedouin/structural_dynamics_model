% ============================================================================
% CONSTRAINT STORY: creator_compensation_automation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_creator_compensation_automation, []).

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
 *   constraint_id: creator_compensation_automation
 *   human_readable: Creator Compensation Automation
 *   domain: digital_economics/creative_labor
 *
 * SUMMARY:
 *   Creator compensation automation represents the structural coupling of
 *   three forces: (1) platform operators' need to process payments at scale
 *   across millions of creators with minimal transaction overhead; (2)
 *   creators' structural dependence on platform distribution with no viable
 *   alternative channels; (3) algorithmic systems whose compensation formulas
 *   are simultaneously essential (enabling marketplace function) and opaque
 *   (protecting proprietary ranking and payout logic). The constraint
 *   exhibits genuine coordination function — automated payment processing
 *   enables creator markets that would be economically infeasible with manual
 *   accounting — while simultaneously enabling extraction through formula
 *   opacity and creator powerlessness. The theater ratio has declined over
 *   the interval (0.62 → 0.48) as platforms have developed increasingly
 *   sophisticated compensation facades: creator support programs,
 *   transparency dashboards, fairness commitments. Yet extractiveness has
 *   increased (0.35 → 0.62) as algorithmic optimization has tightened payout
 *   formulas and as more creators have become dependent on platform income
 *   with fewer exit options. This divergence signals a shift from
 *   theater-dominant extraction (performative fairness rhetoric masking
 *   opaque systems) toward formula-dominant extraction (increasingly
 *   sophisticated algorithms that appear fair but concentrate value toward
 *   platforms).
 *
 * KEY AGENTS:
 *   - Independent Creators: Primary victim (powerless/trapped) — structurally dependent on platform distribution; no exit options; subject to algorithmic compensation with minimal comprehension
 *   - Platform Operators: Primary beneficiary (institutional/arbitrage) — benefit from automated payment processing that enables marketplace scale; technical ability to modify compensation formulas unilaterally
 *   - Creator Advocacy Coalition: Secondary actor (moderate/constrained) — organizing collective action to demand transparency; achieving incremental gains but constrained by technical complexity and platform lobbying
 *   - Regulatory Authority: Inter-institutional actor (institutional/constrained) — attempting to establish fairness standards and transparency requirements; constrained by technical expertise gaps and lobbying pressure
 *   - Wage-Labor Compensation Framework: Institutional vestige (institutional/arbitrage) — traditional employer-employee norms applied to contractually independent creators; maintains extraction through institutional inertia
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing automation opacity as inherent feature rather than design choice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(creator_compensation_automation, 0.58).
domain_priors:suppression_score(creator_compensation_automation, 0.52).
domain_priors:theater_ratio(creator_compensation_automation, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(creator_compensation_automation, extractiveness, 0.58).
narrative_ontology:constraint_metric(creator_compensation_automation, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(creator_compensation_automation, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(creator_compensation_automation, tangled_rope).
narrative_ontology:human_readable(creator_compensation_automation, "Creator Compensation Automation").
narrative_ontology:topic_domain(creator_compensation_automation, "digital_economics/creative_labor").

domain_priors:requires_active_enforcement(creator_compensation_automation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(creator_compensation_automation, platform_operators).
narrative_ontology:constraint_beneficiary(creator_compensation_automation, algorithmic_efficiency_advocates).
narrative_ontology:constraint_victim(creator_compensation_automation, independent_creators).
narrative_ontology:constraint_victim(creator_compensation_automation, compensation_transparency).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: INDEPENDENT CREATOR (SNARE) — Trapped within platform ecosystems that dictate compensation formulas algorithmically. No meaningful exit: alternative platforms have identical structures, reliance on platform distribution is inescapable, and compensation mechanisms are opaque black boxes. Maximum extraction with minimal coordination benefit — the creator must accept whatever formula the algorithm produces or lose their livelihood entirely.
constraint_indexing:constraint_classification(creator_compensation_automation, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: CREATOR ADVOCACY COALITION (TANGLED ROPE) — Constrained by resource limitations and lack of technical expertise to audit algorithms, but benefits from collective action that has produced some transparency gains (disclosure requirements, appeal mechanisms). Experiences both coordination function (collective bargaining effects) and asymmetric extraction (compensation formulas still heavily favor platforms). Significant agency but meaningful remaining capture.
constraint_indexing:constraint_classification(creator_compensation_automation, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: PLATFORM OPERATOR (ROPE) — Experiences the constraint as pure coordination: algorithmic compensation automates creator payment processing at scale, reducing transaction costs and enabling marketplace function. Net beneficiary with arbitrage options (can pivot to alternative compensation models or exit the platform business). Sees automation as coordination infrastructure, not extraction.
constraint_indexing:constraint_classification(creator_compensation_automation, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: REGULATORY AUTHORITY (TANGLED ROPE) — Constrained by technical complexity (auditing algorithmic compensation is resource-intensive) and lobbying pressure from platforms, but gaining agency through transparency mandates and fairness requirements. Experiences both coordination function (standards that enable platform operation) and asymmetric extraction (lobbying influence tilts standards toward platforms). Increasing agency over time as technical expertise develops.
constraint_indexing:constraint_classification(creator_compensation_automation, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: WAGE-LABOR COMPENSATION FRAMEWORK (PITON) — The traditional employer-employee compensation model persists as a vestigial institutional frame applied to platform creators who are structurally independent contractors. The theater of 'employment' (benefits messaging, creator support programs, community guidelines framed as workplace norms) masks extraction through algorithmic opacity. Degraded by structural mismatch but maintained through institutional inertia and lack of alternative legal categories for creator relationships.
constraint_indexing:constraint_classification(creator_compensation_automation, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, some compensation opacity is inherent to algorithmic systems: complex ranking and payout formulas are difficult to explain in human terms, and complete transparency of recommendation algorithms could enable gaming. This perspective sees the bottleneck as a natural feature of scaled digital marketplaces. However, structural data contradicts mountain classification — the opaqueness is partly intentional design choice (protecting proprietary algorithms), not an immutable property of automation itself.
constraint_indexing:constraint_classification(creator_compensation_automation, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(creator_compensation_automation_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(creator_compensation_automation, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(creator_compensation_automation, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(creator_compensation_automation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(creator_compensation_automation, TR),
    TR >= 0.70.

:- end_tests(creator_compensation_automation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The constraint has genuine coordination function (automated payment processing is more efficient than manual accounting), but extractiveness has increased over time as creator economic dependence has deepened and algorithmic optimization has tightened payout concentration. The initial value (0.35) reflected the early era when creator platforms competed partly on compensation generosity; current value (0.62) reflects mature market consolidation where creators have fewer exit options and platforms have greater ability to optimize formulas downward. Suppression (0.52): Moderate-high. Multiple barriers constrain creators: platform-specific ecosystem lock-in (reputation, audience, content investment), lack of technical knowledge to audit algorithms, career risk of platform expulsion, absence of alternative distribution channels with equivalent reach, and atomization preventing collective action. But suppression is not total — some creators maintain presence across platforms, advocacy organizations are building technical expertise, and regulatory pressure is increasing transparency. Theater ratio (0.48): Moderate. Initial theater was high (0.62) due to fairness messaging and creator support program staging; current theater is lower (0.48) because sophisticated algorithmic design has reduced need for performative rhetoric. The shift toward lower theater while extractiveness rises indicates transition from theater-dependent to algorithm-dependent extraction mechanism.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates sharp perspectival divergence. Platform operators see pure coordination (Rope) — algorithmic automation solves genuine marketplace problems. Creators see pure extraction (Snare) — opaque formulas concentrate value extraction with no exit option. Creator advocates see mixed extraction and emerging coordination (Tangled Rope) — transparency mandates and fairness standards are creating some agency, but extraction remains. Regulators see a constrained coordination problem (Tangled Rope) — they recognize legitimate automation needs while trying to limit extraction through standards, but face technical and political constraints. The wage-labor framework persists as a degraded institutional form (Piton) — employment language ('creator partners,' 'creator support,' community governance) masks contractor relationships and enables continued extraction. The civilizational analytical observer risks seeing inherent technical limits (Mountain) when the opacity and extraction are partly intentional design choices.
 *
 * DIRECTIONALITY LOGIC:
 *   Platform operators (institutional/arbitrage) derive low directionality values because they are beneficiaries with exit options — they can pivot to alternative compensation models or leave the creator marketplace business. Creators (powerless/trapped) derive high directionality values because they are victims with no exit options — structural dependence on platform distribution. Creator advocacy coalitions (moderate/constrained) derive intermediate directionality because they are victims but with some agency through collective organization and partial exit optionality (can shift creators' platform mix). Regulatory authorities (institutional/constrained) derive moderate-high directionality because they are partially captured (lobbying pressure constrains their agency) yet represent public interest against extraction. The piton classification derives from the theater gate: traditional wage-labor compensation norms applied to platform creators are largely performative, maintained by institutional inertia rather than functional necessity.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by distinguishing genuine coordination function (scalable payment processing) from extraction mechanism (formula opacity and creator trappage). The classification rejects false coordination claims: platforms cannot simply call algorithmic optimization 'efficiency' and avoid extraction analysis. It also prevents misclassification as pure snare: the constraint genuinely enables creator markets that would be infeasible otherwise. The Tangled Rope classification captures the hybrid nature: real coordination function coupled with asymmetric extraction. The perspectival gap is diagnostic — the constraint looks like pure coordination from the beneficiary's view and pure extraction from the victim's view, which is precisely the signature of a constraint that combines both functions. The rising extractiveness with declining theater signals that platforms are increasingly willing to be transparent about optimization while maintaining structural opacity — the extraction mechanism is shifting from performative cover to algorithmic inevitability.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    opacity_necessity_vs_choice,
    'Is algorithmic compensation opacity a technical necessity or an intentional design choice that protects platform interests?',
    'Comparative analysis of transparency levels across platforms with equivalent technical constraints; examination of historical documentation showing whether opacity was designed or emerged from complexity',
    'If necessity: suppression and theater_ratio are lower than measured. If choice: platforms are intentionally maintaining opacity as extraction mechanism; classification remains Snare from creator perspective.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(opacity_necessity_vs_choice, empirical, 'Whether algorithmic compensation opacity is technical necessity or intentional design').

omega_variable(
    algorithmic_fairness_sufficiency,
    'Can algorithmic auditing and fairness standards create genuine creator compensation transparency, or does automation inherently prevent real human comprehension of payouts?',
    'Post-audit creator understanding surveys; comparison of creator satisfaction with compensation pre- and post-transparency mandate; analysis of whether auditable algorithms reduce exploitation relative to black-box systems',
    'If auditable algorithms suffice: constraint can shift toward Scaffold (temporary problem being solved by technical standards). If comprehension remains elusive: problem is structural to algorithmic mediation, not solvable by transparency alone.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(algorithmic_fairness_sufficiency, empirical, 'Whether algorithmic fairness standards can create genuine compensation transparency').

omega_variable(
    creator_collective_organization,
    'Can independent creators achieve sufficient collective organization to negotiate compensation terms, or does platform atomization prevent credible threat of collective exit?',
    'Historical analysis of creator union organizing efforts; measurement of collective action capacity across platforms; comparison with traditional labor organizing in high-atomization sectors',
    'If yes: creator power atom should upgrade from powerless to organized; classification could shift from Snare to Tangled Rope from their perspective. If no: atomization is structural barrier preventing Hirschman exit/voice options.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(creator_collective_organization, empirical, 'Whether creators can achieve sufficient collective organization to negotiate compensation').

omega_variable(
    alternative_compensation_models_viability,
    'Are alternative compensation models (direct patronage, collective ownership, cooperatives) genuinely viable at scale, or is platform-mediated automation the only economically efficient system?',
    'Comparative analysis of transaction costs, creator revenue stability, and sustainable operations across compensation model types; measurement of which models retain creators long-term',
    'If viable alternatives exist: Scaffold classification is supported — exit pathway is real. If platforms are economically superior: creator trappage is structural necessity, not extractive choice.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_compensation_models_viability, empirical, 'Whether alternative compensation models are viable at scale').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(creator_compensation_automation, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cca_tr_t0, creator_compensation_automation, theater_ratio, 0, 0.62).
narrative_ontology:measurement(cca_tr_t3, creator_compensation_automation, theater_ratio, 3, 0.55).
narrative_ontology:measurement(cca_tr_t6, creator_compensation_automation, theater_ratio, 6, 0.48).
narrative_ontology:measurement(cca_tr_t9, creator_compensation_automation, theater_ratio, 9, 0.43).

% Extraction over time
narrative_ontology:measurement(cca_be_t0, creator_compensation_automation, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(cca_be_t3, creator_compensation_automation, base_extractiveness, 3, 0.48).
narrative_ontology:measurement(cca_be_t6, creator_compensation_automation, base_extractiveness, 6, 0.58).
narrative_ontology:measurement(cca_be_t9, creator_compensation_automation, base_extractiveness, 9, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(creator_compensation_automation, resource_allocation).
narrative_ontology:affects_constraint(creator_compensation_automation, algorithmic_opacity_labor_markets).
narrative_ontology:affects_constraint(creator_compensation_automation, platform_algorithmic_ranking_opacity).

% DUAL FORMULATION NOTE:
% Creator compensation automation couples three distinct constraints: (1) algorithmic_opacity_labor_markets — the general problem of opaque formulas in labor mediation; (2) platform_algorithmic_ranking_opacity — the opacity of recommendation algorithms that determine creator visibility; (3) creator_compensation_automation — the specific constraint arising from automating payouts. Each has distinct epsilon values: ranking opacity is higher (more purely extractive), compensation automation is intermediate (mixed coordination-extraction), labor market opacity is intermediate (depends on labor market structure). All three affect each other — ranking opacity enables compensation extraction by making payouts opaque; compensation automation increases power asymmetry in creator-platform relationships.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(creator_compensation_automation, institutional, 0.32).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
