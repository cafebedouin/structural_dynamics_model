% ============================================================================
% CONSTRAINT STORY: non_compete_enforcement
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_non_compete_enforcement, []).

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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: non_compete_enforcement
 *   human_readable: Non-Compete Agreement Enforcement
 *   domain: labor/employment/economic
 *
 * SUMMARY:
 *   Non-compete agreement enforcement creates a structural constraint between
 *   employer firms' interest in protecting training investments and human
 *   capital, and departing employees' interest in labor market mobility. The
 *   constraint exhibits properties of both coordination and extraction: firms
 *   genuinely face a problem (how to protect firm-specific knowledge and
 *   training investment without permanent binding), and non-competes provide
 *   one solution. However, the enforcement mechanism also suppresses employee
 *   exit options and restricts labor market competition, generating
 *   extraction that benefits firms at the expense of worker mobility and
 *   market efficiency. The constraint has intensified over 30 years as
 *   enforceability has expanded geographically and temporally (theater ratio
 *   rising from 0.35 to 0.55) and extractiveness has increased (0.42 to 0.58)
 *   through broader covenant language and more aggressive litigation.
 *   Regulatory reform efforts (California ban, Massachusetts restrictions,
 *   FTC challenge to non-competes) represent organized attempts to replace
 *   the current enforcement regime with alternatives that achieve
 *   coordination without maximal suppression.
 *
 * KEY AGENTS:
 *   - Departing Specialist: Primary victim (powerless/trapped) — locked into covenant by legal liability; cannot exercise specialized skills in competing firm
 *   - Skilled Professional: Secondary victim (moderate/constrained) — faces high but surmountable costs to exit; can relocate or retrain but at substantial loss
 *   - Employer Firm: Primary beneficiary (institutional/arbitrage) — captures benefit of covenant protection; can arbitrage by requiring covenants as hire condition
 *   - Labor Reform Coalition: Organized agents (organized/constrained) — unions, legislatures, worker advocates; building alternative coordination pathways (state bans, skills certification, public training investment)
 *   - Courts/Legal System: Institutional actor (institutional/arbitrage) — maintains reasonableness doctrine and enforcement mechanism; sees enforcement as degraded ritual (piton perspective)
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing non-compete enforcement as inherent to labor markets and training investment
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(non_compete_enforcement, 0.58).
domain_priors:suppression_score(non_compete_enforcement, 0.68).
domain_priors:theater_ratio(non_compete_enforcement, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(non_compete_enforcement, extractiveness, 0.58).
narrative_ontology:constraint_metric(non_compete_enforcement, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(non_compete_enforcement, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(non_compete_enforcement, tangled_rope).
narrative_ontology:human_readable(non_compete_enforcement, "Non-Compete Agreement Enforcement").
narrative_ontology:topic_domain(non_compete_enforcement, "labor/employment/economic").

domain_priors:requires_active_enforcement(non_compete_enforcement).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(non_compete_enforcement, employer_firm).
narrative_ontology:constraint_victim(non_compete_enforcement, departing_employee).
narrative_ontology:constraint_victim(non_compete_enforcement, market_competition).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DEPARTING SPECIALIST (SNARE) — Trapped by non-compete covenant. Cannot exit to competing employer without legal liability and financial ruin. Specialized skills make alternative career paths costly. Suppression is structural: enforcement via litigation, injunctive relief, and liquidated damages makes exit materially impossible. Experiences pure extraction — the constraint prevents the departing employee from exercising their human capital.
constraint_indexing:constraint_classification(non_compete_enforcement, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: SKILLED MID-CAREER PROFESSIONAL (TANGLED ROPE) — Faces high but surmountable barriers to exit. Can relocate to different geographic market, retrain into adjacent field, or negotiate covenant buyout. But costs are substantial: relocation burden, career disruption, financial loss. Also benefits from non-compete enforcement elsewhere — the norm protects their own firm's investments in them. Mixed coordination (protecting firm investment in training) and extraction (blocking lucrative lateral moves).
constraint_indexing:constraint_classification(non_compete_enforcement, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: EMPLOYER FIRM (ROPE) — Primary beneficiary. Non-compete covenants enable coordination: firms can invest in training specialists without fear of immediate defection to rivals. Experiences the constraint as a coordination mechanism that solves a genuine problem (how to protect firm-specific knowledge and training investment). Can arbitrage by requiring non-competes as a condition of hire; can exit by not requiring them.
constraint_indexing:constraint_classification(non_compete_enforcement, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 4: LABOR REFORM COALITION (SCAFFOLD) — Organized agents (labor unions, state legislatures, worker advocacy groups) perceive non-compete enforcement as a temporary coordination failure with a sunset. State-level non-compete bans (California, North Dakota, and recent reforms in Massachusetts, Virginia) represent alternative coordination pathways: public investment in training, skills certification systems, and competition-as-driver-of-innovation frame replace private firm capture of human capital. Estimated sunset: 15-25 years as regulatory landscape shifts toward non-enforce or severe limitation.
constraint_indexing:constraint_classification(non_compete_enforcement, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: OUTDATED LEGAL DOCTRINE (PITON) — Reasonable-ness doctrine (geographic scope, duration, legitimate business interest) persists as a performative filter despite weak enforcement and conflicting policy goals. Courts maintain the ritual of balancing while systematically upholding broad covenants in practice. The legal doctrine sees itself as degraded — recognized as inefficient and inequitable but maintained through institutional inertia and lobbying pressure. Theater ratio reflects the gap between judicial pronouncements of reasonableness and actual enforcement patterns (high theater, moderate function).
constraint_indexing:constraint_classification(non_compete_enforcement, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, some form of employee-employer coordination around knowledge and training investment is inherent to human capital formation: any firm investing heavily in an employee faces risk of immediate defection. This perspective naturalizes non-compete enforcement as an immutable constraint of economic exchange. However, the structural data reveals this as false summit: non-compete doctrine is highly contingent (California ban proves enforceability is policy choice, not natural law), varies by jurisdiction, and is increasingly contested as inefficient.
constraint_indexing:constraint_classification(non_compete_enforcement, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(non_compete_enforcement_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(non_compete_enforcement, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(non_compete_enforcement, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(non_compete_enforcement, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(non_compete_enforcement, TR),
    TR >= 0.70.

:- end_tests(non_compete_enforcement_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. Firms capture significant benefit during covenant window (typically 1-3 years but increasingly 5+ years). The benefit is not purely extractive — some is fair coordination reward for training investment — but the magnitude has expanded beyond what legitimate business interest justifies. Measurement trajectory (0.42→0.58) reflects broadening scope and duration of covenants. Suppression (0.68): High. Significant structural barriers to exit include legal liability (injunctive relief, liquidated damages, attorney fees), specialized skill loss if forced into unrelated field, regional market concentration in some industries (tech, finance), and information asymmetry (employees often don't assess covenant severity until post-hire). Theater ratio (0.55): Moderate-high. Courts apply reasonableness doctrine (geographic scope, temporal duration, legitimate business interest test) that produces appearance of balanced filtering. In practice, enforcement is substantial and broad covenants are regularly upheld, creating theater gap between judicial rhetoric and actual enforcement.
 *
 * PERSPECTIVAL GAP:
 *   The departing specialist and employer firm perceive radically different constraints: the employee sees a snare (extraction with no exit), the employer sees a rope (coordination solving a legitimate problem). The organized reform coalition sees a scaffold with a sunset (regulatory alternatives maturing over 15-25 years). The courts see a piton (the reasonableness doctrine performing a ritual they acknowledge is imperfect). The analytical observer risks seeing a mountain (training investment protection inherent to labor markets) but the structural data reveals high policy contingency. The gap between rope (firm perception) and snare (employee perception) is the core diagnostic signal: the same constraint solves coordination for one agent while suppressing exit for another.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is driven by structural position and beneficiary/victim status. The employer firm (institutional/arbitrage) experiences low d → negative chi because it is the beneficiary and has exit capacity (can choose to require or not require covenants). The departing employee (powerless/trapped) experiences high d → high chi because they are the victim and lack exit capacity (covenant is binding). Mid-career professional (moderate/constrained) experiences moderate d because exit is possible at cost. The labor reform coalition (organized/constrained) experiences lower d because they have organized power and policy exit routes. Courts (institutional/arbitrage) see themselves as beneficiaries of the enforcement mechanism they maintain, despite judicial acknowledgment that the doctrine is imperfectly balanced.
 *
 * MANDATROPHY ANALYSIS:
 *   TANGLED ROPE RESOLUTION: Non-compete enforcement is genuinely tangled because it combines coordination (protecting firm training investment) and extraction (suppressing employee exit options). The coordination function is real: firms face a legitimate collective action problem (if every employee can defect immediately to rivals after training, firms underinvest in development). The extraction is also real: employees lose geographic/sectoral mobility options and often cannot recover the value of their specialized skills. The mandatrophy is resolved by accepting both functions as structural. The constraint is not 'really' a snare masquerading as coordination, nor 'really' pure coordination with unfortunate side effects. It is genuinely hybrid. The tangled_rope classification flags that this constraint requires active enforcement and produces both coordination and asymmetric extraction. Policy choices (narrow vs. broad covenants, geographic/temporal limits, alternative mechanisms) determine the ratio of coordination to extraction, not the fundamental nature of the constraint.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    legitimate_business_interest_scope,
    'What constitutes a legitimate business interest justifying non-compete restriction? Trade secrets, client relationships, competitive strategy, or general human capital?',
    'Comparative analysis of state enforcement standards and empirical outcomes: which definitional boundaries correlate with faster innovation, employee mobility, and firm stability?',
    'If narrowly defined (trade secrets only): non-competes reclassify as snare from more perspectives. If broadly defined (general competitive advantage): tangled_rope classification holds but coordination function dominates over extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legitimate_business_interest_scope, conceptual, 'Definition of legitimate business interest').

omega_variable(
    geographic_temporal_reasonableness,
    'What geographic scope and duration are necessary and sufficient to protect legitimate firm interests? One year or five years? City or continental?',
    'Empirical tracking: do covenants with binding temporal/geographic limits (vs. overly broad limits) achieve protection while preserving exit options? Do California-model bans reduce firm investment in training?',
    'If narrow bounds are sufficient: suppression metric drops 0.15-0.20 (exit becomes genuinely constrained rather than trapped). If broad bounds necessary: suppression persists, snare classification dominates.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(geographic_temporal_reasonableness, empirical, 'Necessary scope and duration of covenants').

omega_variable(
    training_investment_recovery_mechanism,
    'Does non-compete enforcement actually recover firm training investment, or does it primarily extract future earning potential from employees?',
    'Longitudinal data: correlation between non-compete enforcement intensity and firm spending on employee development; wage premium analysis for employees under covenants vs. without',
    'If covenants enable training investment: tangled_rope classification confirmed (genuine coordination). If covenants reduce training but extract future earnings: snare reclassifies from employer perspective.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(training_investment_recovery_mechanism, empirical, 'Whether non-competes enable or extract from training investment').

omega_variable(
    alternative_coordination_sufficiency,
    'Do alternative mechanisms (non-solicitation covenants, trade secret law, equity vesting schedules) achieve the same coordination function without suppression?',
    'Comparative institutional analysis: tech sector (high non-competes vs. low-enforcement jurisdictions); academic/research sectors (alternative IP mechanisms); geographic comparison across state enforcement regimes',
    'If alternatives sufficient: non-compete enforcement reclassifies as redundant extraction (snare). If alternatives inadequate: tangled_rope classification solidified.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(alternative_coordination_sufficiency, empirical, 'Sufficiency of alternative coordination mechanisms').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(non_compete_enforcement, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nce_tr_t0, non_compete_enforcement, theater_ratio, 0, 0.35).
narrative_ontology:measurement(nce_tr_t15, non_compete_enforcement, theater_ratio, 15, 0.48).
narrative_ontology:measurement(nce_tr_t30, non_compete_enforcement, theater_ratio, 30, 0.55).

% Extraction over time
narrative_ontology:measurement(nce_be_t0, non_compete_enforcement, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(nce_be_t15, non_compete_enforcement, base_extractiveness, 15, 0.52).
narrative_ontology:measurement(nce_be_t30, non_compete_enforcement, base_extractiveness, 30, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(non_compete_enforcement, resource_allocation).
narrative_ontology:boltzmann_floor_override(non_compete_enforcement, 0.18).
narrative_ontology:affects_constraint(non_compete_enforcement, labor_market_mobility).
narrative_ontology:affects_constraint(non_compete_enforcement, trade_secret_protection).
narrative_ontology:affects_constraint(non_compete_enforcement, employee_equity_vesting).

% DUAL FORMULATION NOTE:
% Non-compete enforcement decomposes into multiple structurally distinct constraints: (1) trade secret protection (ε≈0.08, Mountain) — legitimate business interest in confidential information; (2) employee poaching deterrence (ε≈0.35, Rope) — coordination benefit from protecting client relationships; (3) future earnings restriction (ε≈0.72, Snare) — extraction from blocking market mobility in low-secret roles. This story models the integrated enforcement mechanism (ε=0.58, Tangled Rope) that combines all three functions. Upstream: trade_secret_protection (lower ε, clearer coordination). Downstream: labor_market_mobility (higher ε, extraction dominates).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(non_compete_enforcement, institutional, 0.22).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
