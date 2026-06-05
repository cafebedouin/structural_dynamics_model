% ============================================================================
% CONSTRAINT STORY: labor_organizing_barriers
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_labor_organizing_barriers, []).

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
 *   constraint_id: labor_organizing_barriers
 *   human_readable: Labor Organizing Barriers in Contemporary Workplaces
 *   domain: labor/economic/political
 *
 * SUMMARY:
 *   Labor organizing barriers in contemporary workplaces constitute a
 *   structural constraint that extracts value from workers through
 *   suppression of collective bargaining capacity while maintaining a
 *   coordination facade. The constraint combines genuine worker coordination
 *   challenges (fragmented labor, skill heterogeneity, turnover) with
 *   engineered legal and institutional barriers (at-will employment,
 *   right-to-work laws, union-busting consultants, surveillance technology).
 *   The measurement trajectory shows extractiveness increasing from 0.42 to
 *   0.58 over a decade, reflecting the intensification of precarity
 *   mechanisms (gig work proliferation, temp agency expansion, union decline)
 *   alongside rising theater (worker empowerment rhetoric, company culture
 *   messaging, engagement survey theater). The constraint exhibits all six
 *   classification types from different structural positions: a natural-law
 *   false summit from the analytical observer (markets are competitive, labor
 *   fragmentation is inevitable), degraded legal theater (right-to-work),
 *   temporary scaffolding from global advocacy, genuine coordination benefit
 *   for capital owners, constrained agency for organized labor, and pure
 *   extraction for precarious workers.
 *
 * KEY AGENTS:
 *   - Precarious Workers: Primary victim (powerless/trapped) — economic dependency, at-will employment, wage-loss risk, termination threats, surveillance, legal liability
 *   - Organized Labor Coalition: Secondary victim and partial beneficiary (moderate/constrained) — resource barriers but capable of strike coordination and mutual aid; trades concessions for wage gains
 *   - Capital Owners and Management Layers: Primary beneficiary (institutional/arbitrage) — benefit from wage suppression, unilateral work-rule authority, labor cost control; high arbitrage options including capital mobility
 *   - Anti-Union Consultants and Professional Networks: Secondary beneficiary (powerful/arbitrage) — specialize in decertification campaigns, legal strategies, surveillance systems; direct economic interest in barrier persistence
 *   - Global Labor Rights Advocacy (ILO, union federations, worker-rights NGOs): Organized advocates (organized/mobile) — building alternative pathways through international standards, supply-chain accountability, worker visa protections
 *   - Right-to-Work Legal Framework: Institutional actor (institutional/arbitrage) — maintains performative ideological role despite limited functional justification; perpetuated through legislative inertia
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(labor_organizing_barriers, 0.58).
domain_priors:suppression_score(labor_organizing_barriers, 0.68).
domain_priors:theater_ratio(labor_organizing_barriers, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(labor_organizing_barriers, extractiveness, 0.58).
narrative_ontology:constraint_metric(labor_organizing_barriers, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(labor_organizing_barriers, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(labor_organizing_barriers, tangled_rope).
narrative_ontology:human_readable(labor_organizing_barriers, "Labor Organizing Barriers in Contemporary Workplaces").
narrative_ontology:topic_domain(labor_organizing_barriers, "labor/economic/political").

domain_priors:requires_active_enforcement(labor_organizing_barriers).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(labor_organizing_barriers, capital_owners).
narrative_ontology:constraint_beneficiary(labor_organizing_barriers, management_layers).
narrative_ontology:constraint_beneficiary(labor_organizing_barriers, anti_union_consultants).
narrative_ontology:constraint_victim(labor_organizing_barriers, precarious_workers).
narrative_ontology:constraint_victim(labor_organizing_barriers, wage_earners).
narrative_ontology:constraint_victim(labor_organizing_barriers, collective_bargaining_capacity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PRECARIOUS WORKER (SNARE) — Trapped by economic dependency, at-will employment doctrine, and immediate wage loss. Cannot exit without risking unemployment, housing instability, and healthcare loss. Faces termination threats, surveillance, and legal liability for organizing activity. Maximum experienced extraction with zero degrees of freedom.
constraint_indexing:constraint_classification(labor_organizing_barriers, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: ORGANIZED LABOR COALITION (TANGLED ROPE) — Constrained by resource limitations, employer legal countermeasures, and geographic fragmentation, yet capable of coordinating strikes, mutual aid, and organizing drives. Genuine coordination function (contract negotiation) alongside asymmetric extraction (concession extraction from employers). Agency exists but extraction costs are substantial.
constraint_indexing:constraint_classification(labor_organizing_barriers, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: CAPITAL OWNERS / MANAGEMENT (ROPE) — Benefits from the barrier constraint through labor cost suppression and unilateral work-rule authority. Experiences the organizing barrier as a coordination mechanism: the ability to set wages and conditions without worker input is itself a form of coordination that concentrates authority. Net beneficiary with high arbitrage options.
constraint_indexing:constraint_classification(labor_organizing_barriers, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: GLOBAL LABOR RIGHTS ADVOCACY (SCAFFOLD) — Organized trans-national actors (ILO, union federations, worker-rights NGOs) see organizing barriers as a temporary policy failure with a sunset: international labor standards, worker visa protections, and supply-chain accountability mechanisms are building alternative coordination pathways. Mobilization capacity and exit visibility (strategic relocation, public pressure) reduce effective extraction.
constraint_indexing:constraint_classification(labor_organizing_barriers, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: RIGHT-TO-WORK LEGAL FRAMEWORK (PITON) — The legal architecture restricting union security agreements persists through legislative inertia despite limited functional justification. The framework's stated coordination goal (labor market flexibility, worker freedom to choose) is substantially performative — right-to-work states show higher wage suppression and lower worker voice despite the freedom rhetoric. The constraint maintains itself through ideological performance rather than structural necessity.
constraint_indexing:constraint_classification(labor_organizing_barriers, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, organizing barriers may appear as inherent to markets: wage competition and labor supply dynamics 'naturally' suppress collective bargaining capacity. Capital mobility and labor fragmentation are treated as immutable features of economic systems. This perspective risks naturalizing what are actually contingent legal and institutional arrangements (at-will employment, anti-injunction doctrine limits, right-to-work laws). The engine will flag this as a false summit.
constraint_indexing:constraint_classification(labor_organizing_barriers, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(labor_organizing_barriers_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(labor_organizing_barriers, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(labor_organizing_barriers, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(labor_organizing_barriers, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(labor_organizing_barriers, TR),
    TR >= 0.70.

:- end_tests(labor_organizing_barriers_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High. The constraint systematically suppresses worker bargaining power, allowing employers to capture wage gains from productivity increases and to externalize costs (wage stagnation, benefit cuts, unsafe conditions) onto workers and public systems. The measurement trajectory (0.42→0.58) reflects intensification of precarity mechanisms: gig work, temp agencies, immigrant labor stratification, skill-based wage compression all increase extractiveness. This is not maximal extraction (0.70+) because some coordination function persists — collective agreements do occur, some sectors maintain unionization — but the trend is unambiguously upward. Suppression (0.68): Very high. Multiple reinforcing barriers to organizing include: immediate wage loss threat (economic coercion), at-will employment doctrine (legal barrier), employer surveillance technology (information asymmetry), geographic fragmentation of labor (coordination cost), skill heterogeneity creating internal divisions (structural fragmentation), and anti-union consultant networks (professional barrier ecosystem). Each barrier alone would be substantial; their interaction creates near-total suppression from the precarious worker perspective. Theater ratio (0.55): Moderate-high. Right-to-work legal framework maintains ideological theater ('worker freedom to choose') despite empirical evidence that right-to-work states show higher wage suppression. Company culture messaging ('we're family,' 'your voice matters') and worker engagement surveys create performative voice-capture. However, the theater is not maximal (0.70+) because real strikes and organizing drives still occur and visibly challenge the framing — the performance is contested.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gaps in this constraint are extreme, revealing the full range of DR types. Capital owners see coordination: they genuinely are solving the problem of setting wages and work rules without worker input — from their perspective, the barrier constraint solves a real coordination problem and should be maintained. Organized labor sees mixed extraction and coordination: they can negotiate contracts (genuine coordination) but only at high cost (extraction). Precarious workers see pure extraction: the barriers give them no agency and no benefit. Global advocates see a temporary problem: international standards and supply-chain enforcement are building exit paths (scaffold sunset). The legal framework sees itself as performative (piton): right-to-work ideology persists despite its minimal functional justification. The analytical observer at civilizational scale risks seeing natural law: 'markets just naturally suppress labor organization through competition and capital mobility.' This natural-law framing naturalizes what are contingent institutional arrangements. The engine will identify this as a false summit: the core barriers (at-will employment, legal injunction against secondary boycotts, right-to-work) are policy choices, not laws of nature.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derives from each agent's structural position relative to extraction flow. Precarious workers are trapped with high d (0.95), producing high f(d) ≈ 1.42, maximizing experienced extraction chi. Organized labor is constrained with moderate d (0.65), producing f(d) ≈ 1.00, yielding moderate chi. Capital owners benefit with low d (0.15), producing f(d) ≈ -0.01, yielding negative chi (they experience the barrier as a subsidy). The anti-union consultant network has d ≈ 0.10 (beneficiary with arbitrage), also experiencing negative chi. Global advocates, though organized, are pursuing mobile options (exit capacity), placing them at d ≈ 0.55, producing moderate experienced extraction. The right-to-work legal framework from the institutional perspective operates with institutional power and arbitrage exit (d ≈ 0.05), giving it the lowest chi — the framework experiences itself as barely extractive, which is why it persists despite powering the constraint. This directionality profile explains why the constraint is so stable: the beneficiaries experience it as nearly costless, while the victims experience it as all-consuming.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY STRUCTURE: This constraint resolves the mandatrophy by showing that organizing barriers serve as a persistent coordination mechanism for capital owners (rope/scaffold for them) while simultaneously functioning as pure extraction for workers (snare). The constraint is not mislabeled as pure extraction when it has a real coordination function — the coordination function is genuine, just asymmetrically captured. The tangled-rope classification is correct because the constraint BOTH solves a coordination problem (wage/work-rule setting without worker input) AND asymmetrically extracts (workers bear the cost of suppressed bargaining). The mandatrophy asks: 'Is this really extraction or is it just coordination?' The answer is: it is both. For capital owners, it solves coordination. For workers, it is extraction. The classification depends on the perspective, not on the constraint's intrinsic nature. The increasing extractiveness (0.42→0.58) and theater ratio (0.38→0.55) over the decade reveal gradual degradation into snare territory — the coordination function (negotiating shared workplace standards) is being replaced by pure extraction (wage suppression, externalization, precarity management). At the current trajectory, the constraint may cross 0.70 extractiveness within 15 years, requiring mandatrophy resolution to prevent false summit (natural law) classification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    structural_vs_legal_barriers,
    'Are organizing barriers primarily structural (economics, capital mobility) or legal (at-will doctrine, union-busting consultants, injunction regimes)?',
    'Comparative analysis: jurisdictions with strong legal protections (Nordic model) vs. weak protections (US right-to-work states). If legal barriers are primary, coordinated organizing should be faster and cheaper in high-protection jurisdictions. Measure organizing costs and success rates.',
    'If legal: barriers are reversible through legislative reform. Classification may shift from Snare to Scaffold if sunset clauses become salient. If structural: barriers are more intractable; classification remains Snare at biographical horizon. Mandatrophy implications differ.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(structural_vs_legal_barriers, empirical, 'Extent to which barriers are legal vs. structural').

omega_variable(
    precarious_identity_lock,
    'To what extent is worker fragmentation due to identity_locked acceptance of precarity (internalized powerlessness, individualized career framing) vs. structural trapped conditions (genuine economic coercion)?',
    'Post-unionization longitudinal studies: do workers whose organizing drive succeeds exhibit persistent belief in their own powerlessness (suggesting identity lock) or immediately mobilize once structural barriers are lifted (suggesting trapped)? Interview data on self-perception of collective agency before and after successful organizing.',
    'If identity_locked: classification shifts to Rope from biographical perspective (constraint perceived as changeable in principle); exit pathway is cognitive reframing of collective identity. If trapped: classification remains Snare; exit pathway is material barrier removal. Therapeutic vs. organizational intervention implications.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(precarious_identity_lock, empirical, 'Whether worker passivity is identity-locked or structurally trapped').

omega_variable(
    global_arbitrage_sustainability,
    'Can global labor rights advocacy (Scaffold perspective) actually deliver a sunset, or does capital mobility perpetually reset organizing barriers as firms relocate to weaker-protection jurisdictions?',
    'Longitudinal tracking: measure organizing success rates in offshoring-vulnerable sectors vs. non-offshoring sectors. Test whether supply-chain standards enforcement actually reduces organizing barriers or merely displaces them geographically. Monitor whether ILO conventions increase organizing success in signatory nations.',
    'If mobile capital can circumvent global standards: scaffold sunset is illusory; the constraint persists indefinitely at generational scale. Classification remains Snare/Tangled Rope despite advocacy effort. If standards enforcement works: scaffold is real; sunset is plausible within generational horizon.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(global_arbitrage_sustainability, empirical, 'Whether global labor advocacy can overcome capital mobility').

omega_variable(
    union_rent_extraction,
    'Do union security agreements in high-union jurisdictions themselves extract rents (union dues, political control, seniority restrictions) that replicate the extraction pattern employers impose, just captured by different beneficiaries?',
    'Comparative worker satisfaction and wage equity within unionized vs. non-unionized workplaces, controlling for industry and skill level. Measure union representation of precarious vs. core workers; identify whether unions perpetuate secondary labor markets. Survey data on worker agency within union governance.',
    'If unions replicate extraction patterns: the constraint shifts from ''organizing barriers prevent collective voice'' to ''available organizing structures substitute one extraction regime for another.'' Classification becomes Piton (institutional inertia of union form) rather than Snare. Mandatrophy: the resolution (unionization) does not resolve the underlying extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(union_rent_extraction, empirical, 'Whether union structures replicate extraction dynamics').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(labor_organizing_barriers, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(labor_org_tr_t0, labor_organizing_barriers, theater_ratio, 0, 0.38).
narrative_ontology:measurement(labor_org_tr_t5, labor_organizing_barriers, theater_ratio, 5, 0.48).
narrative_ontology:measurement(labor_org_tr_t10, labor_organizing_barriers, theater_ratio, 10, 0.55).

% Extraction over time
narrative_ontology:measurement(labor_org_be_t0, labor_organizing_barriers, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(labor_org_be_t5, labor_organizing_barriers, base_extractiveness, 5, 0.52).
narrative_ontology:measurement(labor_org_be_t10, labor_organizing_barriers, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(labor_organizing_barriers, resource_allocation).
narrative_ontology:affects_constraint(labor_organizing_barriers, wage_stagnation_despite_productivity).
narrative_ontology:affects_constraint(labor_organizing_barriers, gig_economy_labor_stratification).
narrative_ontology:affects_constraint(labor_organizing_barriers, union_membership_decline).

% DUAL FORMULATION NOTE:
% Labor organizing barriers form a constraint family with three downstream effects: wage stagnation (extractiveness 0.65, because the barrier prevents workers from capturing productivity gains), gig economy stratification (extractiveness 0.72, because the barrier prevents alternative organizing models), and union decline (extractiveness 0.55, because the barrier increases union dues/control burden, creating piton dynamics). Each story in the family has distinct epsilon reflecting different observable pathways; they are linked by shared structural root cause (organizing barriers).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(labor_organizing_barriers, institutional, 0.05).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
