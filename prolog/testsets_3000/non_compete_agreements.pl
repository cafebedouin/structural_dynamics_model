% ============================================================================
% CONSTRAINT STORY: non_compete_agreements
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_non_compete_agreements, []).

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
 *   constraint_id: non_compete_agreements
 *   human_readable: Non-Compete Agreements (Post-Employment Restraint)
 *   domain: economic/political
 *
 * SUMMARY:
 *   Non-compete agreements represent a contractual constraint on
 *   post-employment competition that extracts labor mobility from departing
 *   employees while providing legitimate trade-secret protection for
 *   incumbent employers. The constraint sits at the boundary between genuine
 *   coordination (protecting confidential information) and pure extraction
 *   (restricting job market entry and wage competition). Over the past
 *   decade, extractiveness has increased (0.42 to 0.58) as employers have
 *   expanded restriction scopes beyond trade secrets to general competitive
 *   positions. Theater ratio has remained relatively low (0.32-0.38) because
 *   the mechanism operates through explicit legal contracts and injunctions
 *   rather than performative ritual — when enforcement fails, it fails
 *   transparently. The regulatory landscape is shifting: multiple U.S. states
 *   have banned or restricted non-competes, and the FTC proposed a nationwide
 *   ban in 2023, signaling a collapse of institutional consensus on the
 *   constraint's legitimacy. This creates a Scaffold-like temporal structure
 *   where the constraint is being deliberately dismantled by organized
 *   regulatory actors.
 *
 * KEY AGENTS:
 *   - Departing Employee: Primary victim (powerless/trapped) — bears full restriction cost with limited geographic/temporal exit
 *   - Job Market Entrants: Secondary victim (moderate/constrained) — face reduced hiring pools and competition, constrained but not trapped
 *   - Incumbent Employer: Primary beneficiary (institutional/arbitrage) — captures trade-secret protection and employee retention; can choose enforcement level
 *   - Competitive Market Coalition: Organized actor (organized/mobile) — labor advocates, startup ecosystems, new competitors organizing to dismantle constraint; mobile and can lobby for regulatory change
 *   - Legal/Regulatory System: Institutional enforcer (institutional/arbitrage) — maintains through injunction machinery but increasingly skeptical; sees own role as degraded
 *   - Regulatory Reform Movement: Organized reformer (organized/mobile) — state legislatures and FTC building alternative employment relationship norms with sunset logic
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing legal doctrine as economic necessity
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(non_compete_agreements, 0.58).
domain_priors:suppression_score(non_compete_agreements, 0.72).
domain_priors:theater_ratio(non_compete_agreements, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(non_compete_agreements, extractiveness, 0.58).
narrative_ontology:constraint_metric(non_compete_agreements, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(non_compete_agreements, theater_ratio, 0.38).

% --- Constraint claim ---
narrative_ontology:constraint_claim(non_compete_agreements, snare).
narrative_ontology:human_readable(non_compete_agreements, "Non-Compete Agreements (Post-Employment Restraint)").
narrative_ontology:topic_domain(non_compete_agreements, "economic/political").

domain_priors:requires_active_enforcement(non_compete_agreements).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(non_compete_agreements, incumbent_employer).
narrative_ontology:constraint_beneficiary(non_compete_agreements, capital_holders).
narrative_ontology:constraint_victim(non_compete_agreements, departing_employee).
narrative_ontology:constraint_victim(non_compete_agreements, job_market_entrants).
narrative_ontology:constraint_victim(non_compete_agreements, competitive_innovation).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DEPARTING EMPLOYEE (SNARE) — Trapped by geographic and temporal restrictions that limit job opportunities within their specialization. Limited exit options: either relocate to outside the restriction zone (high personal cost), wait out the restriction period (foregone income), or accept reduced-wage employment outside their expertise. The constraint extracts career mobility and future earning potential. High suppression because legal enforcement with financial penalties and damages creates coercive atmosphere.
constraint_indexing:constraint_classification(non_compete_agreements, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: JOB MARKET ENTRANT (SNARE) — Entry-level and early-career workers in competitive fields (software, pharmaceuticals, finance) face restricted hiring pools because departing employees cannot enter. This reduces competition for positions, raises wages for incumbent workers, but limits total opportunity creation. Constrained exit: can still enter but with fewer employers recruiting them; must accept geographic mobility or career pivots.
constraint_indexing:constraint_classification(non_compete_agreements, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: INCUMBENT EMPLOYER (ROPE) — Experiences non-competes as a coordination mechanism: protecting trade secrets, customer relationships, and confidential processes. Benefits from reduced poaching of key personnel and customer defection. Arbitrage exit: can choose not to enforce or negotiate terms. Net beneficiary of the constraint structure — extraction runs toward this agent.
constraint_indexing:constraint_classification(non_compete_agreements, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: COMPETITIVE MARKET COALITION (TANGLED ROPE) — Labor advocates, startup ecosystems, and competitive employers organizing to oppose non-competes see genuine coordination function (trade secret protection) but also recognize asymmetric extraction: the mechanism privileges incumbents over market entrants and labor mobility. Mobile exit: workers and new competitors can advocate for regulation, relocate to non-compete-hostile jurisdictions, or build alternative ecosystems. This perspective sees both real coordination (IP protection) and real extraction (labor immobility).
constraint_indexing:constraint_classification(non_compete_agreements, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: LEGAL/REGULATORY SYSTEM (PITON) — Enforces non-competes through injunctions and damages, maintaining the constraint through institutional machinery. But the system sees itself as degraded: enforcement is erratic, courts increasingly skeptical of overbroad restrictions, and legislative push in multiple U.S. states to ban or restrict non-competes signals loss of functional consensus. Theater ratio high (0.38 is lower than many pitons, but the legal theater of 'reasonableness' standards, scope negotiations, and confidentiality claims surrounds a core that is performative). Maintained through inertia and contract law doctrine rather than genuine function.
constraint_indexing:constraint_classification(non_compete_agreements, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: REGULATORY REFORM MOVEMENT (SCAFFOLD) — State-level policy changes (California ban, recent FTC proposal) represent organized agents building temporary enforcement coordination with clear sunset: transition from universal enforceability to targeted restrictions on trade-secret-adjacent roles. This perspective sees the constraint as a temporary coordination mechanism being deliberately dismantled. Sunset rationale: as transparent hiring practices and employment law modernize, the need for restrictive covenants diminishes. Estimated sunset: 10-15 years as state-by-state bans normalize.
constraint_indexing:constraint_classification(non_compete_agreements, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURALIZATION VIEW (MOUNTAIN) — From a universal/civilizational perspective, some post-employment restraint is inherent to any employment relationship: employers cannot prevent hiring patterns forever, but some temporary protection of confidential information appears structurally necessary. This perspective risks naturalizing what is actually a contingent legal doctrine (English common law, imported to U.S. with variable adoption). The engine's false summit detector will flag this: non-competes are not immutable laws of nature, but negotiated legal institutions.
constraint_indexing:constraint_classification(non_compete_agreements, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(non_compete_agreements_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(non_compete_agreements, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(non_compete_agreements, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(non_compete_agreements, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(non_compete_agreements, TR),
    TR >= 0.70.

:- end_tests(non_compete_agreements_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. Non-competes extract significant value: departed employees lose earning potential (5-20% wage discount over restriction period in studies), job market entrants face restricted hiring pools, and labor mobility is reduced. However, the extraction is not maximal because it is contractually explicit and geographically scoped (not universal). Base extractiveness has risen from 0.42 to 0.58 over the interval as employers have expanded restriction durations and geographic scope. Suppression (0.72): High. Multiple enforcement mechanisms create coercive atmosphere: legal liability for breach (damages + injunctive relief), career signaling effects (departing employees labeled as violators), and collective knowledge that courts will enforce. Alternatives are costly: relocation outside restriction zone, career delay during restriction period, or career pivoting to outside specialty. Theater ratio (0.38): Low-moderate. The constraint operates through explicit contractual language and adjudication rather than performative ritual. When courts reject non-competes as overbroad, they do so transparently. The theater component includes 'reasonableness' standards that vary by jurisdiction and the claim that restrictions protect trade secrets (which ranges from legitimate to pretext depending on scope). Theater has increased slightly (0.32 to 0.38) as legal uncertainty has grown and more restrictions are challenged in court.
 *
 * PERSPECTIVAL GAP:
 *   The constraint produces maximum perspectival divergence across power levels. The beneficiary (incumbent employer) sees coordination — legitimate trade-secret protection and relationship stability. The powerless victim (departing employee) sees pure extraction — job mobility loss with no offsetting benefit. The organized coalitions (labor advocates, startup ecosystems) see both: genuine coordination function for narrow restrictions but systematic extraction through scope creep. The legal system sees degradation (Piton) — it enforces non-competes but increasingly doubts their legitimacy. The regulatory reform movement sees a temporary problem with a sunset (Scaffold) — state bans and FTC action are deliberately dismantling the constraint. The analytical observer risks seeing an economic necessity (Mountain) but the structural data reveals this as naturalization of a contingent legal doctrine. The perspectival gap reflects deep disagreement about whether post-employment restraints are economically essential or primarily extractive.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values (d) are derived from structural position: power level, exit options, and beneficiary/victim status. Departing employees are victims with trapped exit → high d (~0.90) → high f(d) (~1.35) → high experienced extraction. Job market entrants are victims with constrained exit → high-moderate d (~0.75) → moderate f(d) (~1.10) → moderate experienced extraction. Incumbent employers are beneficiaries with arbitrage exit → low d (~0.20) → low/negative f(d) (~0.10) → low effective extraction for beneficiary. Organized coalitions are mobile → d around 0.55 → moderate f(d) (~0.75) → moderate experienced extraction, but their mobility and organization allow exit and advocacy. The pipeline correctly classifies disparate agent experiences from unified base metrics by varying (P,T,E,S) tuples: same constraint, different power atoms, different exit options, different structural positions → different classifications.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by recognizing that non-competes are legitimately Tangled Rope at moderate scope (narrow geographic, trade-secret-focused) but degrade into Snare as scope expands (broad geographic, general competitive position, extended duration). The coordination function (protecting confidential information) is real but increasingly subsumed by extraction (restricting labor mobility without legitimate IP justification). The constraint resolves mandatrophy by showing that the type depends critically on enforcement scope: a narrow non-compete with enforceable trade-secret provisions is closer to Rope; a broad non-compete with generic competitive restrictions is closer to Snare. The current extractiveness estimate (0.58) reflects averaging across diverse non-competes — some legitimately Rope-adjacent, others clearly Snare. As regulatory reform narrows permitted scopes, the average will shift back toward Tangled Rope or Rope. The Piton perspective (legal system sees degradation) is key: the system is enforcing a constraint it no longer believes in, sustained by doctrinal inertia rather than functional consensus.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    trade_secret_necessity,
    'What percentage of non-compete enforcement actually protects genuine trade secrets vs. general competitive advantage or employee relationships?',
    'Case law analysis: review injunction decisions and damage awards, categorize by claimed injury (trade secret, customer list, general competition); survey employer data on actual harm from departing employees',
    'If >70% involve genuine trade secrets: coordination function is real, classification shifts toward Tangled Rope across more perspectives. If <40%: mechanism is primarily extraction with thin justification, classification shifts toward Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(trade_secret_necessity, empirical, 'Proportion of non-compete enforcement protecting genuine trade secrets').

omega_variable(
    labor_mobility_elasticity,
    'What percentage of job transitions are prevented or delayed by non-compete restrictions, and how much does this reduce wage growth and innovation?',
    'Longitudinal tracking of post-employment outcomes across state jurisdictions with different non-compete enforceability; analysis of wage trajectories and startup formation rates pre/post restrictive covenant laws',
    'If mobility reduction < 5%: suppression is overstated, extractiveness should be lower. If > 25%: suppression and extractiveness values confirmed, Snare classification strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(labor_mobility_elasticity, empirical, 'Percentage of job transitions prevented by non-compete restrictions').

omega_variable(
    geographic_scope_variation,
    'Do narrow geographic restrictions (city-level, office location) function legitimately to protect trade secrets, while broad restrictions (statewide, national) are primarily extractive?',
    'Meta-analysis of injunction outcomes by restriction scope; comparison of trade secret disclosure incidents between narrow and broad restriction groups',
    'If true: non-competes should decompose into two constraints — narrow-scope (Rope-leaning) and broad-scope (Snare-pure). Current story averages; decomposition required.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(geographic_scope_variation, empirical, 'Whether geographic scope differentiates coordination from extraction').

omega_variable(
    state_regulatory_convergence,
    'Will state-level non-compete restrictions converge toward a national standard, or will jurisdictional arbitrage (employee relocation, employer recruitment) sustain divergence?',
    '10-year forward tracking of state law adoption and amendment rates; employer location decisions in response to non-compete bans; cross-border mobility of restricted employees',
    'If convergence toward bans: Scaffold sunset becomes real and imminent (5-7 years). If sustained divergence: Scaffold classification is aspirational; constraint persists in low-restriction jurisdictions (Snare indefinitely).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(state_regulatory_convergence, empirical, 'Whether state-level regulation will converge toward uniform standards').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(non_compete_agreements, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(noncompete_tr_t0, non_compete_agreements, theater_ratio, 0, 0.32).
narrative_ontology:measurement(noncompete_tr_t5, non_compete_agreements, theater_ratio, 5, 0.35).
narrative_ontology:measurement(noncompete_tr_t10, non_compete_agreements, theater_ratio, 10, 0.38).

% Extraction over time
narrative_ontology:measurement(noncompete_be_t0, non_compete_agreements, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(noncompete_be_t5, non_compete_agreements, base_extractiveness, 5, 0.5).
narrative_ontology:measurement(noncompete_be_t10, non_compete_agreements, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(non_compete_agreements, enforcement_mechanism).
narrative_ontology:affects_constraint(non_compete_agreements, labor_monopsony_power).
narrative_ontology:affects_constraint(non_compete_agreements, knowledge_worker_wage_suppression).
narrative_ontology:affects_constraint(non_compete_agreements, startup_ecosystem_accessibility).

% DUAL FORMULATION NOTE:
% Non-compete agreements have broader systemic effects through reduced labor mobility. Three downstream constraints capture: (1) monopsony power enabled by geographic labor market restriction, (2) wage suppression from reduced job-switching competition, (3) startup ecosystem effects from talent immobility. Each downstream constraint has its own extractiveness reflecting the secondary effects; the non-compete itself has extractiveness reflecting the direct restriction mechanism.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(non_compete_agreements, institutional, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
