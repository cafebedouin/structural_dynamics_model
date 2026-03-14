% ============================================================================
% CONSTRAINT STORY: organizational_bloat_equilibrium
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_organizational_bloat_equilibrium, []).

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
 *   constraint_id: organizational_bloat_equilibrium
 *   human_readable: Organizational Bloat Equilibrium
 *   domain: organizational_dynamics/institutional_inertia
 *
 * SUMMARY:
 *   Organizational bloat equilibrium describes the self-reinforcing
 *   constraint that maintains excess administrative overhead, redundant
 *   middle-management layers, and performative processes in large
 *   organizations. The constraint exhibits structural tension between its
 *   coordination function (legitimate hierarchical management of complex
 *   work) and its extractive function (career protection for middle tiers,
 *   insulation of senior leadership, suppression of frontline autonomy). The
 *   bloat equilibrium is maintained not primarily by external coercion but by
 *   internal beneficiary incentives: middle managers benefit from the layers
 *   that create their roles; senior leadership benefits from buffer
 *   insulation; HR benefits from the processes that justify HR's existence.
 *   Frontline workers and the organization's productive output bear the
 *   costs: navigating bureaucratic inefficiency, complying with redundant
 *   requirements, and accepting constraints on autonomy. The constraint is
 *   analyzable through all six DR types simultaneously, revealing the
 *   perspectival nature of the same structural phenomenon.
 *
 * KEY AGENTS:
 *   - Frontline Workers: Primary victims (powerless/trapped) — bear costs of bureaucratic inefficiency without authority to change systems; economically dependent on employment
 *   - Middle Management: Primary beneficiaries (institutional/arbitrage) — expansion of middle layers creates career paths and status roles; have external job mobility options
 *   - Senior Leadership: Mixed actor (powerful/constrained) — benefits from buffer layers insulating from accountability; constrained by the institutional weight they maintain through active enforcement
 *   - Human Resources Function: Institutional actor (institutional/arbitrage) — maintains performative processes that justify HR's existence; processes are largely theater
 *   - Remote-Work and Startup Coalition: Organized alternative (organized/mobile) — demonstrates lean organizational models as viable; creates competitive pressure and exit options
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing bloat as inevitable law of organizational scale rather than contingent institutional choice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(organizational_bloat_equilibrium, 0.52).
domain_priors:suppression_score(organizational_bloat_equilibrium, 0.58).
domain_priors:theater_ratio(organizational_bloat_equilibrium, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(organizational_bloat_equilibrium, extractiveness, 0.52).
narrative_ontology:constraint_metric(organizational_bloat_equilibrium, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(organizational_bloat_equilibrium, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(organizational_bloat_equilibrium, tangled_rope).
narrative_ontology:human_readable(organizational_bloat_equilibrium, "Organizational Bloat Equilibrium").
narrative_ontology:topic_domain(organizational_bloat_equilibrium, "organizational_dynamics/institutional_inertia").

domain_priors:requires_active_enforcement(organizational_bloat_equilibrium).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(organizational_bloat_equilibrium, middle_management_tier).
narrative_ontology:constraint_beneficiary(organizational_bloat_equilibrium, administrative_staff).
narrative_ontology:constraint_beneficiary(organizational_bloat_equilibrium, institutional_gatekeepers).
narrative_ontology:constraint_victim(organizational_bloat_equilibrium, productive_output).
narrative_ontology:constraint_victim(organizational_bloat_equilibrium, organizational_efficiency).
narrative_ontology:constraint_victim(organizational_bloat_equilibrium, frontline_workers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: FRONTLINE WORKER (SNARE) — Trapped within organizational structures that extract labor while providing minimal autonomy or recognition. Cannot exit without severe economic penalty. Experiences bloat as pure extraction: navigating bureaucratic layers, complying with redundant processes, bearing costs of organizational inefficiency without authority to change systems. Maximum suppression through economic dependency and employment law.
constraint_indexing:constraint_classification(organizational_bloat_equilibrium, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: MIDDLE MANAGEMENT (ROPE) — Experiences bloat as beneficial coordination. Expanded middle layers create career paths, status differentiation, and supervisory roles that would not exist in lean organizations. Managers coordinate work allocation, information flow, and resource distribution. This tier has arbitrage options: skills transfer across sectors, mobility upward or lateral. Sees organizational bloat as functional hierarchical coordination.
constraint_indexing:constraint_classification(organizational_bloat_equilibrium, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: SENIOR LEADERSHIP (TANGLED ROPE) — Constrained by the institutional weight they have created. Leadership faces genuine coordination problems: managing distributed teams, allocating resources, maintaining organizational identity and culture. Yet bloat also provides insulation from external pressure and creates buffer layers that protect executives from direct accountability. Active enforcement (performance metrics, hiring frozen at entry-level only, mandatory process reviews) maintains bloat while claiming to manage it. Mixed coordination-extraction structure.
constraint_indexing:constraint_classification(organizational_bloat_equilibrium, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: HUMAN RESOURCES (PITON) — HR processes and policies persist through institutional inertia despite limited functional value. Annual reviews, competency assessments, compliance training, and benefit administration maintain the appearance of systematic people management but are largely performative. HR enforces existing bloat layers (hiring requires multiple approvals, position creation requires committees) while claiming to optimize workforce. Theater ratio very high: process theaters actual outcome.
constraint_indexing:constraint_classification(organizational_bloat_equilibrium, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: REMOTE-WORK AND STARTUP ALTERNATIVES (SCAFFOLD) — Organized sector (distributed teams, flat hierarchies, async communication tools) demonstrates that bloat is not structurally necessary. Remote work, project-based organizations, and startup models show viable coordination with minimal middle-management overhead. This creates a sunset pathway: as competitive pressure forces lean organizational models, bloat equilibrium degrades. Organized agents have exit options (job mobility, entrepreneurship) and see bloat as a temporary coordination failure with technical solutions.
constraint_indexing:constraint_classification(organizational_bloat_equilibrium, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational timescale, bureaucratic bloat appears as an immutable law of large organizations. The Parkinson's Law view (work expands to fill available time and resources) treats bloat as inherent to coordination at scale. Information processing in large hierarchies requires overhead, communication layers, and redundancy. This perspective sees bloat as unavoidable physical law of organizational complexity. However, the structural data contradicts this — alternative organizing models (flat startups, decentralized networks, algorithmic coordination) demonstrate bloat is contingent, not immutable. This is a false summit.
constraint_indexing:constraint_classification(organizational_bloat_equilibrium, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(organizational_bloat_equilibrium_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(organizational_bloat_equilibrium, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(organizational_bloat_equilibrium, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(organizational_bloat_equilibrium, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(organizational_bloat_equilibrium, TR),
    TR >= 0.70.

:- end_tests(organizational_bloat_equilibrium_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The organization extracts from frontline workers through inefficient bureaucratic processes, suppressed autonomy, and effort devoted to compliance versus productive output. However, extraction is not maximal (as in snare) because some coordination genuinely requires overhead and middle-management layers do perform real functions. The extraction is layered onto legitimate coordination. Suppression (0.58): Moderate-high. Frontline workers face economic dependency (primary suppressant), internal mobility barriers (credentialing and advancement tied to management roles), cultural normalization of hierarchy, and limited information about alternatives. However, external job markets do provide some exit paths, so suppression is not total. Theater ratio (0.68): High. HR processes, performance reviews, compliance documentation, and approval chains are substantially performative. These processes create the appearance of systematic management while actual decisions often bypass formal channels. Process theater has increased over the measurement interval as regulatory requirements and risk-management culture have expanded.
 *
 * PERSPECTIVAL GAP:
 *   The largest gap exists between beneficiary (middle management rope) and victim (frontline worker snare). Managers experience bloat as legitimate coordination creating status and career opportunity; workers experience it as pure extraction of labor in service of bureaucratic machinery. This gap is measurable through exit options: managers have arbitrage mobility (can transfer skills across organizations, move up or lateral); workers face trapped barriers (economic dependency, credentialism, limited information). The second gap is between senior leadership (tangled rope: mixed coordination and extraction) and the scaffold perspective (alternative models available that reduce bloat). This gap reveals whether bloat is necessary or contingent — if viable lean models exist, bloat is not an unavoidable coordination cost. The third gap is between the civilizational mountain (bloat as natural law) and empirical observation: flat startups, distributed remote teams, and algorithmic task allocation demonstrate coordination without bloat. This gap reveals false summit — the naturalization of institutional choice as structural necessity.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality flows from beneficiary and victim declarations mapped to exit options. Middle management (beneficiary + arbitrage exit) derives low d → low chi. Frontline workers (victim + trapped exit) derive high d → high f(d) → high experienced extractiveness. Senior leadership (mixed: benefits from buffer yet constrained by institutional weight) occupies intermediate d space. The remote-work coalition (organized exit + mobile) derives moderate d with access to alternatives. The piton perspective emerges not from high extraction but from high theater: the function (coordination) is real but increasingly performative (theater_ratio 0.68). The mountain perspective arises from risk of naturalizing contingent institutional choices as immutable laws of organizational scale.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by showing why bloat equilibrium persists despite its inefficiency: it is maintained by beneficiary incentives (middle management career protection, senior leadership insulation) rather than by its coordination function. The coordination function is real — coordination does require some overhead — but the actual overhead (estimated 20-30% of current middle-management tier) is much lower than the normalized bloat (50%+ of current tier). The excess is extractive overhead maintained by those who benefit from it, not by coordination necessity. The constraint satisfies the tangled rope gates: (1) beneficiaries exist and benefit from coordination function, (2) victims exist and bear extraction costs, (3) active enforcement maintains the structure. The middle-management tier actively enforces bloat through hiring approval chains, promotion gatekeeping, and process complexity. The scaffold perspective (lean alternatives) proves the bloat is contingent rather than immutable, preventing the false-mountain classification from becoming naturalized. Mandatrophy is resolved by recognizing bloat as a mixed-motive equilibrium where coordination need is real but extractive overhead dominates the actual constraint structure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coordination_overhead_threshold,
    'What is the actual minimum coordination overhead required to manage organizational complexity at a given scale, versus habitual bloat normalized as necessary?',
    'Comparative analysis of lean organizations (startups, remote-first companies) versus traditional hierarchies of equivalent functional scope; measurement of decision-making latency and communication path length',
    'If true overhead < 30% of current middle-management tier: bloat is extractive overhead, not coordination. If true overhead > 50% of current tier: bloat is partially functional, classification shifts to tangled_rope for broader population. Diagnostic determines whether snare or tangled_rope from frontline worker perspective.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_overhead_threshold, empirical, 'Minimum actual coordination overhead versus normalized bloat').

omega_variable(
    exit_option_availability,
    'Are exits from bloated organizations structurally available (job mobility, skill transferability) or suppressed through credentialism and market consolidation?',
    'Job market analysis: labor mobility rates, wage penalties for job-switching, credential inflation in hiring, concentration of large employers in sectors',
    'If exits mobile: trapped exit classification should downgrade to constrained; frontline worker perspective reclassifies from snare to tangled_rope. If exits suppressed: trapped is correct; suppression metric should increase to 0.70+',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(exit_option_availability, empirical, 'Availability and cost of exit options for trapped workers').

omega_variable(
    cultural_identity_lock,
    'To what degree is organizational bloat maintained by internalized identity (pride in company size/prestige, sunk career identity in hierarchical advancement) rather than structural barriers?',
    'Post-layoff behavior analysis: do laid-off middle managers seek equivalent roles in lean organizations or gravitates toward larger hierarchies despite lower compensation; turnover analysis in flat organizations after hire from traditional firms',
    'If identity-locked: managers'' exit_options should be identity_locked rather than arbitrage; suppression mechanism becomes partially internalized; this is the cognitive bind that sustains bloat through voluntary perpetuation by beneficiaries. Frontline worker perspective may involve identity_locked (internalized acceptance of hierarchical place) rather than trapped.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cultural_identity_lock, empirical, 'Identity fusion in bloat maintenance versus structural lock').

omega_variable(
    process_efficacy_measurement,
    'Do organizational processes (approval chains, compliance reviews, performance management) actually achieve their stated coordination goals or are they pure theater?',
    'Process audit: trace decision outcomes with and without approval layers; measure whether compliance processes change behavior versus creating documentary artifacts; compare process-compliant versus process-avoidant teams on actual metrics (output quality, error rates, innovation)',
    'If processes are theater: HR piton classification is correct. If processes have efficacy: theater_ratio should decrease and bloat reclassifies toward rope (legitimate coordination). If mixed: tangled_rope is correct. This determines whether bloat is maintainable as coordination or will degrade as theater.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(process_efficacy_measurement, empirical, 'Whether organizational processes are functionally efficacious or performative').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(organizational_bloat_equilibrium, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bloat_tr_t0, organizational_bloat_equilibrium, theater_ratio, 0, 0.52).
narrative_ontology:measurement(bloat_tr_t5, organizational_bloat_equilibrium, theater_ratio, 5, 0.62).
narrative_ontology:measurement(bloat_tr_t10, organizational_bloat_equilibrium, theater_ratio, 10, 0.68).

% Extraction over time
narrative_ontology:measurement(bloat_be_t0, organizational_bloat_equilibrium, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(bloat_be_t5, organizational_bloat_equilibrium, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(bloat_be_t10, organizational_bloat_equilibrium, base_extractiveness, 10, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(organizational_bloat_equilibrium, resource_allocation).
narrative_ontology:affects_constraint(organizational_bloat_equilibrium, organizational_decision_latency).
narrative_ontology:affects_constraint(organizational_bloat_equilibrium, frontline_worker_autonomy_suppression).
narrative_ontology:affects_constraint(organizational_bloat_equilibrium, institutional_innovation_ceiling).

% DUAL FORMULATION NOTE:
% Organizational bloat decomposition: the coordination function (resource allocation, team management, communication hierarchy) should be modeled separately from the extraction mechanism (career gatekeeping, approval chain theater, autonomy suppression). A lean organizational story would show coordination_type resource_allocation with ε ≈ 0.08 (pure rope); this story models bloat_equilibrium as the overlay of extraction onto that coordination base, yielding the tangled rope classification.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(organizational_bloat_equilibrium, powerful, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
