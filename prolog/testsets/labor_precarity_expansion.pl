% ============================================================================
% CONSTRAINT STORY: labor_precarity_expansion
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_labor_precarity_expansion, []).

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
 *   constraint_id: labor_precarity_expansion
 *   human_readable: Labor Precarity Expansion as Coordinated Extraction
 *   domain: economic/labor
 *
 * SUMMARY:
 *   Labor precarity expansion represents a structural shift from post-war
 *   Fordist labor arrangements (stable employment, collective bargaining,
 *   welfare state decoupling) toward risk-shifting, flexibility-maximizing,
 *   benefit-stripping labor regimes. This constraint exhibits hybrid
 *   coordination-extraction characteristics: it genuinely coordinates
 *   flexible workforce allocation, enables consumer access at lower prices,
 *   and allows rapid scaling of service provision (coordination function).
 *   But it achieves this through asymmetric extraction: risk is shifted from
 *   capital to workers, social security is individualized, bargaining power
 *   is suppressed through fragmentation, and the burden of economic
 *   volatility falls on the most vulnerable (extraction function). The
 *   constraint is maintained through both market mechanisms (competitive
 *   pressure) and active enforcement (union-busting, contractor
 *   misclassification, regulatory capture, anti-worker legislation). The
 *   theater ratio (0.55) reflects that labor regulation, union recognition,
 *   and social protection frameworks persist ceremonially while their
 *   enforcement is substantially gutted. Labor law appears to govern work; in
 *   practice, capital arbitrages around it. The measurement trajectory shows
 *   extractiveness rising from 0.35 to 0.62 over thirty years, tracking the
 *   decline of unionization, the shift to platform and gig work, the erosion
 *   of defined-benefit pensions, and the rise of individual responsibility
 *   for healthcare and retirement.
 *
 * KEY AGENTS:
 *   - Precarious Workers: Primary victims (powerless/trapped) — bear full extraction cost through income volatility, benefit loss, and risk exposure. No exit options short of structural economic change.
 *   - Capital Owners & Platform Corporations: Primary beneficiaries (institutional/arbitrage) — capture profit benefits of cost-cutting, flexibility, and risk-shifting. Arbitrage exit: can relocate supply chains, move to lower-regulation jurisdictions, or pivot business models.
 *   - Labor Solidarity Movements: Mixed actor (moderate/constrained) — benefit from growing worker consciousness and capacity to organize; constrained by fragmentation, union decline, and platform worker isolation. Experience genuine coordination function alongside extraction.
 *   - Consumer Base: Secondary beneficiary (organized/constrained) — benefit from precarity through lower prices; constrained by awareness of labor externalities and diffuse responsibility for the system.
 *   - Labor Regulation System: Institutional actor (institutional/arbitrage) — maintains ceremonial labor law while enforcement is gutted. Piton classification reflects performative compliance mechanisms.
 *   - Alternative Labor Models: Organized actor (organized/constrained) — worker cooperatives, platform cooperatives, sectoral bargaining, and portable benefits represent sunset mechanisms for traditional precarity.
 *   - Analytical Observer: Civilizational view (analytical/analytical) — sees the constraint as genuinely hybrid coordination-extraction with both functions structurally real, neither reducible to the other.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(labor_precarity_expansion, 0.58).
domain_priors:suppression_score(labor_precarity_expansion, 0.72).
domain_priors:theater_ratio(labor_precarity_expansion, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(labor_precarity_expansion, extractiveness, 0.58).
narrative_ontology:constraint_metric(labor_precarity_expansion, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(labor_precarity_expansion, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(labor_precarity_expansion, tangled_rope).
narrative_ontology:human_readable(labor_precarity_expansion, "Labor Precarity Expansion as Coordinated Extraction").
narrative_ontology:topic_domain(labor_precarity_expansion, "economic/labor").

domain_priors:requires_active_enforcement(labor_precarity_expansion).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(labor_precarity_expansion, capital_owners).
narrative_ontology:constraint_beneficiary(labor_precarity_expansion, platform_corporations).
narrative_ontology:constraint_beneficiary(labor_precarity_expansion, consumer_base).
narrative_ontology:constraint_victim(labor_precarity_expansion, precarious_workers).
narrative_ontology:constraint_victim(labor_precarity_expansion, labor_solidarity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PRECARIOUS WORKER (SNARE) — No stable employment, dependent on gig platforms or short-term contracts. Trapped by survival necessity and lacking collective bargaining power. Bears full extraction cost: unstable income, no benefits, no job security, labor commodification. Exit appears impossible without structural economic change. Experienced extractiveness is maximum.
constraint_indexing:constraint_classification(labor_precarity_expansion, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: LABOR SOLIDARITY MOVEMENTS (TANGLED ROPE) — Benefit from growing consciousness of labor extraction and capacity to organize collective action. But constrained by union decline, jurisdictional fragmentation, and the difficulty of organizing platform workers. The constraint provides both coordination function (solidarity organizing) and extraction mechanism (repression of organizing, erosion of negotiating power). Mixed experience of the same constraint.
constraint_indexing:constraint_classification(labor_precarity_expansion, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: PLATFORM CORPORATIONS & CAPITAL (ROPE) — Experience the constraint as pure coordination: labor precarity enables workforce flexibility, cost minimization, and rapid scaling. No enforcement costs; the constraint operates through market mechanisms and regulatory arbitrage. Net beneficiary. Arbitrage exit: can relocate to lower-labor-cost jurisdictions, lobby for favorable regulation, or pivot business models.
constraint_indexing:constraint_classification(labor_precarity_expansion, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: CONSUMER BASE (TANGLED ROPE) — Benefit from precarity through lower prices for goods and services enabled by cost-cutting labor practices. But constrained by awareness that precarity is generationally unsustainable, and by diffuse responsibility for the system. Genuine coordination (accessible services at scale) embedded in extraction (unknown labor costs externalized).
constraint_indexing:constraint_classification(labor_precarity_expansion, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: LABOR REGULATION SYSTEM (PITON) — Labor law, minimum wage enforcement, benefits mandates, and union recognition are ceremonially maintained but substantially degraded. Jurisdictional arbitrage allows capital to circumvent enforcement; platforms exploit legal gray zones (independent contractor classification). Theater persists through institutional inertia — the regulatory framework appears to govern labor but is hollowed out. High theater ratio reflects performative compliance mechanisms.
constraint_indexing:constraint_classification(labor_precarity_expansion, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ALTERNATIVE MODELS (SCAFFOLD) — Worker cooperatives, platform cooperatives, portable benefits systems, sectoral bargaining, and gig-worker organizing represent sunset mechanisms for the precarity constraint. These alternatives have genuine coordination function and explicit sunset logic: as alternative models mature, traditional precarity's extraction mechanism loses force. Scaffold classification reflects the declining theater and emerging exit pathways.
constraint_indexing:constraint_classification(labor_precarity_expansion, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — From a civilizational view, labor precarity expansion is a hybrid coordination-extraction system. It genuinely coordinates flexible workforce allocation and consumer access at scale (coordination function). But it extracts asymmetrically from workers by shifting risk, stripping benefits, and suppressing bargaining power (extraction mechanism). The constraint is neither pure coordination nor pure extraction — both functions are structurally real. Active enforcement is required to maintain the asymmetry (labor law circumvention, classification gaming, anti-union activity).
constraint_indexing:constraint_classification(labor_precarity_expansion, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(labor_precarity_expansion_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(labor_precarity_expansion, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(labor_precarity_expansion, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(labor_precarity_expansion, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(labor_precarity_expansion, TR),
    TR >= 0.70.

:- end_tests(labor_precarity_expansion_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high, reflecting that the constraint extracts significantly from workers (income volatility, benefit loss, risk exposure) but maintains enough coordination function to justify the benign framing ('flexibility,' 'entrepreneurship,' 'efficiency'). The value increased from 0.35 to 0.58 over the measurement interval, tracking the intensification of precarity through gig economy expansion. Suppression (0.72): High. Multiple mechanisms suppress worker exit and collective action: (1) Structural barriers — fragmentation (workers isolated from each other), legal classification (contractor misclassification undermines collective bargaining rights), competition for gig work (race-to-the-bottom pricing), lack of benefits portability. (2) Institutional barriers — union decline, anti-union activity, hostile labor law, regulatory capture. (3) Internalized barriers — workers accept precarity as inevitable, identify with gig work 'independence,' lack consciousness of collective power. Theater ratio (0.55): Moderate. Labor law persists ceremonially — minimum wage, hour limits, safety standards, union recognition rights are on the books. But enforcement is gutted: contractors are misclassified to avoid legal employment status; platforms use algorithmic management to maintain plausible deniability of employment control; wage theft is endemic; union-busting is routine; regulatory agencies are underfunded. The constraint operates through regulatory arbitrage and enforcement vacuum, not through transparent rules.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap between beneficiary (Rope) and victim (Snare) is maximal. The beneficiary genuinely experiences pure coordination — platform algorithms match workers to tasks, consumers access services, capital scales efficiently. The victim genuinely experiences pure extraction — unstable income, no benefits, no job security, commodified labor. Both perspectives are phenomenologically accurate from their positions. The gap is not a measurement error but a feature of the constraint's structure: it works as coordination for those who benefit and as extraction for those who bear costs. The analytical observer resolves the gap by insisting that both aspects are structurally real — the constraint is Tangled Rope, not Rope, because the coordination function is built on top of systematic extraction. The beneficiary's insistence on pure coordination requires ignoring the asymmetry; the victim's insistence on pure extraction requires ignoring the genuine coordination function. The Tangled Rope classification forces acknowledgment of both.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values are derived from structural position within the constraint. Platform corporations and capital owners experience low d (beneficiaries with arbitrage exit options: can relocate, lobby, pivot). Precarious workers experience high d (victims with trapped exit options: survival-dependent on the system, no alternatives). Labor movements experience moderate d (mixed actors with constrained exit: can organize but face significant barriers). Consumers experience moderate d (secondary beneficiaries with constrained exit: benefit from low prices but constrained by awareness of exploitation). The piton perspective derives from institutional actors (arbitrage exit) experiencing degraded function — they maintain the constraint through inertia despite reduced faith in its legitimacy. The scaffold perspective derives from organized actors (constrained exit) seeing the constraint as temporary — alternative models are emerging, and the sunset date is visible.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy in labor precarity is the tension between calling the constraint 'efficient coordination' (beneficiary Rope) and calling it 'worker exploitation' (victim Snare). The Tangled Rope classification resolves this: the constraint is both genuine coordination and genuine extraction. The coordination is not a side effect of the extraction — workforce flexibility is a real, valued outcome that markets reward. The extraction is not incidental to the coordination — the asymmetry is necessary to produce the benefits the beneficiary captures. The constraint requires active enforcement to maintain the asymmetry: labor law circumvention, contractor misclassification, union-busting, regulatory capture. If the enforcement lapses (stronger labor organizing, regulatory re-convergence, political pressure), the extraction mechanism weakens without destroying the coordination function — a postcapitalist labor system could maintain flexibility while distributing risk and benefits symmetrically. The mandatrophy is resolved by recognizing that 'efficient coordination' and 'asymmetric extraction' are not competing labels but joint properties of a hybrid constraint. The debate over whether precarity is justified coordination or unjustified extraction reduces to a value question: should the coordination function privilege beneficiary interests (capital) or be redesigned to distribute benefits symmetrically (workers)?
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    contractor_classification_ambiguity,
    'Does independent contractor status reflect genuine autonomy or functional employment masked by legal classification?',
    'Control test analysis: degree of platform control over work process, scheduling, pricing, and worker alternatives. Comparison with formal employment criteria across jurisdictions.',
    'If genuine autonomy: constraint is legitimate coordination mechanism (Rope). If functional employment: classification is legal fiction sustaining extraction (Snare).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(contractor_classification_ambiguity, empirical, 'Whether contractor classification reflects genuine autonomy or disguised employment').

omega_variable(
    precarity_necessity_test,
    'Is labor precarity expansion structurally necessary for efficient resource allocation, or contingent on profit maximization preferences?',
    'Comparative analysis: economies with strong labor protections vs precarity-based models; efficiency metrics (productivity, innovation, distribution efficiency); historical labor organization pre-precarity expansion.',
    'If necessary: classify as mountain-adjacent (immutable economic law). If contingent: confirms Snare/Tangled Rope classification — the constraint serves profit interests, not systemic efficiency.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(precarity_necessity_test, conceptual, 'Whether precarity expansion is structurally necessary or profit-driven').

omega_variable(
    exit_coalition_threshold,
    'What fraction of the workforce must shift to alternative labor models (cooperatives, sectoral bargaining, portable benefits) before traditional precarity loses its extraction dominance?',
    'Longitudinal tracking of alternative model adoption rates; critical mass analysis for cooperative alternatives; measurement of bargaining power recovery as alternative models scale.',
    'If threshold < 20%: scaffold perspective confirmed — alternative models can sunset precarity rapidly. If threshold > 50%: sunset is aspirational, not structural.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(exit_coalition_threshold, empirical, 'Critical mass threshold for alternative labor models to undermine precarity extraction').

omega_variable(
    suppression_mechanism_structural_vs_internalized,
    'Is labor precarity''s suppression (0.72) primarily structural (barriers to collective action, legal constraints) or internalized (workers accept precarity as inevitable, identity fusion with gig economy)?',
    'Post-successful organizing trajectory: measurement of suppression after workers gain collective power; analysis of how rapidly consciousness shifts when alternative models succeed.',
    'If structural: suppression declines sharply with organizing success. If internalized: suppression persists after collective barriers fall — worker consciousness is part of the extraction mechanism.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_internalized, empirical, 'Whether suppression is structural or internalized in workers'' worldviews').

omega_variable(
    regulatory_arbitrage_sustainability,
    'Can capital indefinitely maintain precarity through jurisdictional arbitrage and regulatory circumvention, or do political economy limits exist?',
    'Tracking of regulatory re-convergence attempts (EU labor directives, national minimum standards); cost analysis of regulatory arbitrage as automation and labor scarcity increase; political analysis of precarity backlash.',
    'If sustainable: precarity constraint endures long-term. If limited: political pressure will eventually force re-regulation, collapsing the extraction mechanism.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(regulatory_arbitrage_sustainability, preference, 'Whether capital can sustain regulatory arbitrage indefinitely').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(labor_precarity_expansion, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(labor_precarity_tr_t0, labor_precarity_expansion, theater_ratio, 0, 0.42).
narrative_ontology:measurement(labor_precarity_tr_t10, labor_precarity_expansion, theater_ratio, 10, 0.48).
narrative_ontology:measurement(labor_precarity_tr_t20, labor_precarity_expansion, theater_ratio, 20, 0.55).
narrative_ontology:measurement(labor_precarity_tr_t30, labor_precarity_expansion, theater_ratio, 30, 0.58).

% Extraction over time
narrative_ontology:measurement(labor_precarity_be_t0, labor_precarity_expansion, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(labor_precarity_be_t10, labor_precarity_expansion, base_extractiveness, 10, 0.47).
narrative_ontology:measurement(labor_precarity_be_t20, labor_precarity_expansion, base_extractiveness, 20, 0.58).
narrative_ontology:measurement(labor_precarity_be_t30, labor_precarity_expansion, base_extractiveness, 30, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(labor_precarity_expansion, resource_allocation).
narrative_ontology:affects_constraint(labor_precarity_expansion, social_safety_net_degradation).
narrative_ontology:affects_constraint(labor_precarity_expansion, union_decline_and_fragmentation).
narrative_ontology:affects_constraint(labor_precarity_expansion, automation_driven_wage_suppression).

% DUAL FORMULATION NOTE:
% Labor precarity expansion is a constraint family. The coordination function (workforce flexibility, market efficiency) and the extraction function (risk-shifting, benefit loss) are structurally distinct but mutually reinforcing within this story. Decomposed variants would address: (1) platform coordination mechanisms (separate ε for algorithmic matching), (2) benefit system collapse (separate ε for welfare state fragmentation), (3) union power erosion (separate ε for bargaining capacity decline). All three are downstream of the primary constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(labor_precarity_expansion, institutional, 0.1).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
