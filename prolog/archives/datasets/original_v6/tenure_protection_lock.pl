% ============================================================================
% CONSTRAINT STORY: tenure_protection_lock
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_tenure_protection_lock, []).

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
 *   constraint_id: tenure_protection_lock
 *   human_readable: Tenure Protection Lock in Academic Employment
 *   domain: labor/institutional/education
 *
 * SUMMARY:
 *   Tenure protection in academic employment creates a structural tension
 *   between faculty security (coordination benefit) and institutional
 *   flexibility (extraction cost) that distributes asymmetrically across
 *   labor market tiers. The constraint exhibits multiple legitimate
 *   classifications from different structural positions: tenured faculty and
 *   administrators perceive rope (pure coordination with security benefits);
 *   early-career researchers perceive tangled rope (genuine coordination for
 *   research planning alongside asymmetric labor extraction); the adjunct
 *   precariat perceives snare (no exit path, pure extraction through job
 *   insecurity); organizing contingent faculty perceive scaffold
 *   (unionization and alternative contracts as sunset mechanisms);
 *   institutional analysis perceives piton (performative faculty governance
 *   while operationally sidelining shared governance); and analytical
 *   observers risk perceiving mountain (naturalizing tenure as inherent to
 *   academic freedom). The extractiveness metric has increased from 0.35 to
 *   0.58 over the interval as budget pressures have forced institutions to
 *   substitute tenured positions with adjunct contracts, intensifying the
 *   bifurcation. The theater ratio has risen from 0.42 to 0.68 as
 *   institutions have performed 'shared governance' and 'academic autonomy'
 *   while operationally concentrating power in administration and reducing
 *   actual faculty voice.
 *
 * KEY AGENTS:
 *   - Tenured Faculty: Primary beneficiary (institutional/arbitrage) — captures security, procedural protection, and voice; can renegotiate or exit
 *   - Adjunct Precariat: Primary victim (powerless/trapped) — bears full cost of precarity through contract insecurity, salary suppression, no due process
 *   - Early-Career Researchers: Secondary victim (moderate/constrained) — experience mixed coordination (research stability) and extraction (up-or-out pressure, service exploitation)
 *   - University Administration: Secondary beneficiary (institutional/arbitrage) — benefits from labor supply control and long-term planning; manages costs through adjunct hiring
 *   - Contingent Faculty Organizing: Organized agent (organized/constrained) — union and collective bargaining efforts building alternative pathways with sunset logic
 *   - Institutional Analysis: Observes piton degradation — performative governance rituals persisting through inertia
 *   - Analytical Observer: Risks false summit (mountain) by naturalizing contingent institutional choice as requirement for academic freedom
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(tenure_protection_lock, 0.58).
domain_priors:suppression_score(tenure_protection_lock, 0.65).
domain_priors:theater_ratio(tenure_protection_lock, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(tenure_protection_lock, extractiveness, 0.58).
narrative_ontology:constraint_metric(tenure_protection_lock, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(tenure_protection_lock, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(tenure_protection_lock, tangled_rope).
narrative_ontology:human_readable(tenure_protection_lock, "Tenure Protection Lock in Academic Employment").
narrative_ontology:topic_domain(tenure_protection_lock, "labor/institutional/education").

domain_priors:requires_active_enforcement(tenure_protection_lock).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(tenure_protection_lock, tenured_faculty).
narrative_ontology:constraint_beneficiary(tenure_protection_lock, university_administration).
narrative_ontology:constraint_victim(tenure_protection_lock, adjunct_precariat).
narrative_ontology:constraint_victim(tenure_protection_lock, early_career_researchers).
narrative_ontology:constraint_victim(tenure_protection_lock, institutional_flexibility).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ADJUNCT PRECARIAT (SNARE) — Trapped in perpetual contract renewal cycles with no path to security. Bears full cost of tenure system's inflexibility: job precarity, salary suppression, no due process. Cannot exit without abandoning academic career. Maximum experienced extraction with minimal coordination benefit.
constraint_indexing:constraint_classification(tenure_protection_lock, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: EARLY-CAREER RESEARCHERS (TANGLED ROPE) — Constrained by the tenure track itself: must prove research productivity while managing teaching load and service obligations. The tenure system provides genuine coordination (long-term stability for research planning) but enforces asymmetric extraction (up-or-out pressure, low salary during probation, exploitation of labor-intensive service roles). Exit requires leaving academia entirely.
constraint_indexing:constraint_classification(tenure_protection_lock, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: TENURED FACULTY (ROPE) — Benefits from security and procedural protection. Experiences tenure as pure coordination: stability enables long-term research commitment, academic freedom protection, and institutional voice. Can exit or renegotiate terms with minimal cost. Net beneficiary.
constraint_indexing:constraint_classification(tenure_protection_lock, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: UNIVERSITY ADMINISTRATION (ROPE) — Benefits from tenure lock as coordination mechanism: enables long-term institutional planning, reduces recruitment costs (once in place, tenured faculty are sticky), and creates implicit incentive structure. Manages labor supply through adjunct hiring. Can exit by changing hiring policy. Net beneficiary.
constraint_indexing:constraint_classification(tenure_protection_lock, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: CONTINGENT FACULTY ORGANIZING (SCAFFOLD) — Unionization and collective bargaining efforts (Service Employees International Union, Faculty Senate advocacy) are building alternative pathways: multi-year contracts, just-cause protection, benefits access without tenure track. These represent sunset mechanisms for the traditional tenure lock — converting precarity into structured contingency with procedural protections. Organized agents see the constraint as temporary and replaceable.
constraint_indexing:constraint_classification(tenure_protection_lock, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: TENURE AS INSTITUTIONAL INERTIA (PITON) — From a civilizational perspective, tenure has atrophied from its original function (protecting academic freedom from political interference) to a mechanism for labor market segmentation. Modern challenges (institutional cost-cutting, research evaluation metrics, curriculum flexibility) require the flexibility tenure prevents. The tenure ritual persists through accreditation requirements and historical legitimacy despite functional degradation. Theater is high because institutions perform 'faculty governance' and 'academic autonomy' while operationally sidelining both.
constraint_indexing:constraint_classification(tenure_protection_lock, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a universal perspective, employment security is an immutable requirement for intellectual work: without tenure protection, academic freedom cannot survive institutional pressure. This perspective naturalizes tenure as inherent to the epistemological function of universities. However, this classification is a false summit: many high-performing research institutions (private sector labs, international collaborations, grant-funded networks) operate without tenure while maintaining research quality. The 'natural law' framing obscures the contingent institutional choice.
constraint_indexing:constraint_classification(tenure_protection_lock, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(tenure_protection_lock_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(tenure_protection_lock, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(tenure_protection_lock, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(tenure_protection_lock, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(tenure_protection_lock, TR),
    TR >= 0.70.

:- end_tests(tenure_protection_lock_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high and rising. The constraint extracts from early-career researchers (up-or-out pressure, low probationary salaries) and adjuncts (precarity premium, labor suppression) while stabilizing tenured faculty. The increase over 30 years reflects institutional cost-cutting that substitutes tenure-track positions with adjunct contracts, intensifying asymmetry. The extractiveness is not as high as a pure snare (0.72+) because the coordination benefits are genuine — tenure protection does enable long-term research commitment and provides institutional stability. Suppression (0.65): High. Multiple barriers limit exit: career path dependence (sunk investments in disciplinary credential), geographic constraints (academic positions are concentrated), identity fusion with academic role, legal barriers to due process for non-tenure-track faculty. However, suppression is not total — some academics do exit, and contingent faculty organizing is creating structured pathways (unionization) that reduce suppression through procedural protection. Theater ratio (0.68): High. Faculty governance and shared decision-making are performed extensively (faculty senate meetings, curriculum committees, hiring deliberations) while actual power concentrates in administration (budget decisions, strategic planning, evaluation metrics). The theater has increased as institutions have professionalized the appearance of faculty voice while reducing substantive input. This is the classic piton signature.
 *
 * PERSPECTIVAL GAP:
 *   The classification gap between tenured faculty (rope) and adjunct precariat (snare) is maximized: the same constraint appears as coordination from one perspective and pure extraction from another. This gap is diagnostic of asymmetric distribution — if both groups experienced the same classification, the constraint would be either pure coordination (rope for both) or pure extraction (snare for both). The gap indicates that the constraint's benefits and costs are not shared. The tangled rope classification for early-career researchers is the bridge: they experience both coordination (research planning stability) and extraction (up-or-out pressure), and this mixed experience reveals that the constraint is not pure coordination masked by perspective — there is genuine asymmetry in the structure itself. The piton and mountain perspectives reveal risks of naturalization: institutions perform 'tenure is necessary for academic freedom' (mountain risk) or 'shared governance is real' (piton performance) to justify institutional choices that are actually contingent and under pressure.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality derives from structural position and exit capacity. Tenured faculty with arbitrage options (can renegotiate, have outside offers, are hard to replace) experience low d and negative chi — the constraint subsidizes them. University administration also has arbitrage (can change hiring policy, design alternative contracts) and benefits from labor control — low d. Adjuncts with trapped exit (no industry alternatives, identity-locked to academic career, geographic immobility) experience high d and high chi — maximum extraction. Early-career researchers with constrained exit (high investment in credentials, moderate job market options, career risk of leaving) experience moderate-high d and moderate chi — significant extraction but not maximum. Organizing contingent faculty with constrained exit but collective bargaining power experience moderate d — their chi is reduced by coalition power (the 'organized' power atom) even though exit costs are high. The analytical observer with analytical exit (can choose any frame) experiences moderate d by canonical fallback — they are outside the extraction flow and derive d from their analytical position, not from beneficiary/victim status.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by clarifying that tenure is a hybrid mechanism: it genuinely coordinates research activity (tenured faculty can plan long-term, take intellectual risks) AND asymmetrically extracts from precarious labor (adjuncts subsidize tenure positions through underpayment and precarity). The mandatrophy resolves through the scaffold perspective: alternative structures (unionized multi-year contracts with just-cause protection) can achieve most coordination benefits without the distributive asymmetry. The classification is not 'tenure is good coordination' or 'tenure is bad extraction' but rather 'tenure's coordination benefits are real, and the extractive asymmetry is a choice contingent on budget structure, not a necessary requirement.' The piton perspective reveals institutional risk: as tenure's functional value (protecting research investment) declines relative to its cost (labor market inflexibility), the constraint persists through inertia and ritual, not because it works. This creates vulnerability — if institutions succeed in hollowing out tenure's meaning (through contingent hiring, reducing sabbatical support, concentrating resources), tenure becomes pure theater (high piton classification) without coordination content.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    academic_freedom_necessity,
    'Is lifetime employment protection (tenure) a necessary condition for academic freedom, or does it merely provide sufficient protection that can be achieved through alternative mechanisms?',
    'Cross-national comparison of research freedom and institutional autonomy in tenure vs non-tenure systems; analysis of academic freedom violations and their correlation with tenure presence/absence',
    'If necessary: tenure lock is mountain (natural requirement). If sufficient but not necessary: tenure is a contingent institutional arrangement (rope/snare/scaffold depending on directionality). Classification changes entire constraint interpretation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(academic_freedom_necessity, conceptual, 'Whether tenure is necessary or merely sufficient for academic freedom').

omega_variable(
    institutional_flexibility_tradeoff,
    'What is the actual cost of tenure inflexibility to institutional adaptation and research portfolio evolution? Is the cost imposed by tenure greater than the cost of losing faculty stability?',
    'Longitudinal analysis of research output changes post-tenure; comparison of institutional agility metrics between high-tenure and high-contingency institutions; cost analysis of redundant positions held to maintain tenure ratios',
    'If flexibility cost > stability benefit: tenure is extractive imbalance (tangled rope/snare). If stability benefit > flexibility cost: tenure is net positive coordination (rope). If costs are symmetric: tangled rope classification confirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_flexibility_tradeoff, empirical, 'Institutional cost-benefit of tenure inflexibility').

omega_variable(
    precariat_substitution_mechanism,
    'Does the tenure system structurally require a precarious adjunct layer to function, or is adjunct dependence an emergent outcome of budget pressures independent of tenure protection?',
    'Historical analysis of tenure/adjunct ratio shifts; comparison of institutions with identical tenure policies but different adjunct hiring rates; controlled analysis of budget cuts and corresponding tenure vs precarity expansion',
    'If structural requirement: tenure extraction of adjuncts is endemic to the system (snare classification confirmed). If budget-driven: tenure and adjunctification are separable, and alternative labor structures can coexist with tenure protection.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(precariat_substitution_mechanism, empirical, 'Whether tenure structurally requires precarious labor').

omega_variable(
    identity_lock_mechanism,
    'For academics in the tenure track, is the constraint binding through material incentives (career risk, salary dependence) or through identity fusion with institutional academic identity?',
    'Analysis of exit patterns: do departures from academia correspond to career disruptions (visible material barriers) or to gradual identity redefinition (internal frame shift)? Interview/ethnographic data on self-concept in relation to academic institution.',
    'If material: tenure is a constrained exit situation (tangled rope from constrained perspective). If identity: tenure is an identity_locked situation (rope from identity_locked perspective, snare from trapped perspective). Different omega resolution produces different perspective tuples.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_mechanism, empirical, 'Whether tenure binds through material or identity-based mechanisms').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(tenure_protection_lock, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tenure_tr_t0, tenure_protection_lock, theater_ratio, 0, 0.42).
narrative_ontology:measurement(tenure_tr_t15, tenure_protection_lock, theater_ratio, 15, 0.58).
narrative_ontology:measurement(tenure_tr_t30, tenure_protection_lock, theater_ratio, 30, 0.68).

% Extraction over time
narrative_ontology:measurement(tenure_be_t0, tenure_protection_lock, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(tenure_be_t15, tenure_protection_lock, base_extractiveness, 15, 0.48).
narrative_ontology:measurement(tenure_be_t30, tenure_protection_lock, base_extractiveness, 30, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(tenure_protection_lock, enforcement_mechanism).
narrative_ontology:affects_constraint(tenure_protection_lock, adjunct_precarity_cycle).
narrative_ontology:affects_constraint(tenure_protection_lock, research_freedom_protection).
narrative_ontology:affects_constraint(tenure_protection_lock, institutional_cost_containment).

% DUAL FORMULATION NOTE:
% Tenure protection lock is upstream of adjunct precarity (tenure positions structurally require adjunct substitution under budget pressure) and interacts with research freedom protection (both mechanisms provide similar benefits through different institutional structures). Institutional cost-containment is downstream (budget constraints force tenure/adjunct tradeoff). The three constraints form a family where tenure's coordination benefit is distributed asymmetrically relative to its cost, with adjunctification as the mechanism for cost transfer.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(tenure_protection_lock, institutional, 0.22).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
