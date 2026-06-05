% ============================================================================
% CONSTRAINT STORY: isa_education_scaffold
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_isa_education_scaffold, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: isa_education_scaffold
 *   human_readable: Income Share Agreement (ISA) Funding for Education
 *   domain: economic/educational
 *
 * SUMMARY:
 *   Income Share Agreements (ISAs) have emerged as an alternative to
 *   traditional student loans, allowing students to fund education by
 *   committing a fixed percentage of post-graduation income for a set term.
 *   The constraint exhibits Scaffold classification: it solves the real
 *   coordination problem of upfront education access barriers (beneficiaries
 *   = education providers and high-income graduates who can absorb the
 *   repayment burden easily), while imposing asymmetric costs on low-income
 *   graduates and career-changers who face prolonged earnings capture. The
 *   key structural feature marking it as Scaffold rather than pure extraction
 *   (Snare) is the built-in sunset clause: ISAs are term-limited (typically
 *   10-25 years), and the policy ecosystem increasingly includes tuition-free
 *   alternatives, competency-based hiring, and income-based repayment
 *   hardship provisions. However, the theater ratio (0.52, rising from 0.35)
 *   indicates growing performative content: ISA marketing emphasizes
 *   'risk-sharing' and 'access' while underspecifying the income-contingency
 *   burden for low earners. The extractiveness (0.38, rising from 0.22)
 *   reflects that the mechanism is accumulating rent-seeking behavior:
 *   securitization, investor incentives to maximize income collection, and
 *   administrative barriers to income verification hardship claims are
 *   layering extraction onto the underlying coordination function. The
 *   constraint is transitional: whether it remains a temporary scaffold or
 *   degrades into a Piton depends on whether alternative education funding
 *   models mature as projected.
 *
 * KEY AGENTS:
 *   - Low-income graduates: Primary victim (powerless/trapped) — face prolonged income-contingent obligation with minimal exit options; bear disproportionate repayment burden
 *   - Mid-career graduates: Secondary victim (moderate/constrained) — can relocate or underreport income but face friction and risk; experience mixed coordination (access) and extraction (income share)
 *   - High-income graduates: Primary beneficiary (powerful/arbitrage) — access to education is valuable; repayment burden is manageable; can exit via full repayment
 *   - ISA education providers: Institutional beneficiary (institutional/arbitrage) — coordinate capital flows; benefit from asset securitization and portfolio effects
 *   - Education access coalition: Organized agents (organized/mobile) — advocate for free/alternative education models; see ISA as temporary solution with a sunset
 *   - Student debt system legacy: Institutional actor (institutional/constrained) — perpetuates income-contingent earning capture through inertia; maintains ISA despite alternatives
 *   - Analytical observer: Civilizational view (analytical/analytical) — risks naturalizing contingent financing design as inherent to education funding
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(isa_education_scaffold, 0.38).
domain_priors:suppression_score(isa_education_scaffold, 0.48).
domain_priors:theater_ratio(isa_education_scaffold, 0.52).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(isa_education_scaffold, extractiveness, 0.38).
narrative_ontology:constraint_metric(isa_education_scaffold, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(isa_education_scaffold, theater_ratio, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(isa_education_scaffold, scaffold).
narrative_ontology:human_readable(isa_education_scaffold, "Income Share Agreement (ISA) Funding for Education").
narrative_ontology:topic_domain(isa_education_scaffold, "economic/educational").

domain_priors:requires_active_enforcement(isa_education_scaffold).
narrative_ontology:has_sunset_clause(isa_education_scaffold).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(isa_education_scaffold, isa_education_providers).
narrative_ontology:constraint_beneficiary(isa_education_scaffold, high_income_graduates).
narrative_ontology:constraint_victim(isa_education_scaffold, low_income_graduates).
narrative_ontology:constraint_victim(isa_education_scaffold, career_changers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: LOW-INCOME GRADUATE (SNARE) — Trapped by income-contingent obligation with no escape: even career disruption, illness, or involuntary unemployment does not suspend the ISA claim. Cannot refinance or exit. d≈0.92, f(d)≈1.40, σ=1.0 → χ≈0.53.
constraint_indexing:constraint_classification(isa_education_scaffold, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: MID-CAREER GRADUATE (TANGLED ROPE) — Benefits from access to education without upfront cost; constrained by income-share obligation during earning years. Can exit via relocation or underreporting, but with friction and risk. d≈0.65, f(d)≈1.00, σ=1.0 → χ≈0.38.
constraint_indexing:constraint_classification(isa_education_scaffold, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: HIGH-INCOME GRADUATE (ROPE) — ISA functions as coordination mechanism: deferred payment enables access; high income makes repayment manageable. Can exit via full repayment. d≈0.10, f(d)≈0.05, σ=1.0 → χ≈0.02. Effective extraction near zero — net beneficiary from both access and favorable burden distribution.
constraint_indexing:constraint_classification(isa_education_scaffold, rope,
    context(agent_power(powerful),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: EDUCATION ACCESS COALITION (SCAFFOLD) — Organized advocates (community colleges, equity organizations, open-source education platforms) see ISA as a temporary solution enabling access while superior alternatives (free public higher education, skills-based hiring, apprenticeships) mature. d≈0.35, f(d)≈0.35, σ=1.0 → χ≈0.13. Low extraction because the coalition has agency and a clear exit path.
constraint_indexing:constraint_classification(isa_education_scaffold, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: ISA EDUCATION PROVIDER (ROPE) — Coordinates capital flows: enables student access in exchange for income-contingent repayment. Sees the mechanism as fair burden-sharing. d≈0.08, f(d)≈-0.06, σ=1.0 → χ≈-0.02. Negative extraction: provider benefits from asset bundling and portfolio effects; institutional arbitrage (lending, securitization, income forecasting) is the coordination function.
constraint_indexing:constraint_classification(isa_education_scaffold, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: STUDENT DEBT SYSTEM LEGACY (PITON) — ISA is partly rhetorical replacement for traditional student loans: the performative innovation narrative ('income-contingent is humane!') masks that the underlying extraction (prolonged earnings capture) persists. theater_ratio=0.52 reflects that ISA marketing emphasizes access benefits while underspecifying income-contingency severity. Debt system inertia maintains ISA despite emergence of tuition-free alternatives. d≈0.70, f(d)≈1.15, σ=1.0 → χ≈0.44.
constraint_indexing:constraint_classification(isa_education_scaffold, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (MOUNTAIN RISK) — From a civilizational view, some form of capital-allocation mechanism for education funding appears immutable: education requires investment, and someone must bear initial cost. This perspective risks naturalizing the ISA as an inherent feature of education financing. However, the structural data (ε=0.38, suppression=0.48, theater=0.52) reveals this is contingent institutional design, not natural law. The engine will flag this as a false summit.
constraint_indexing:constraint_classification(isa_education_scaffold, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(isa_education_scaffold_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(isa_education_scaffold, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(isa_education_scaffold, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(isa_education_scaffold, TR),
    TR >= 0.70.

:- end_tests(isa_education_scaffold_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. ISAs extract earnings through income-contingent repayment, but the extraction is not as severe as traditional predatory lending (ε > 0.50) because (a) repayment is capped at a fixed percentage, (b) term-limits exist, (c) hardship provisions reduce burden in genuine cases, and (d) some borrowers benefit from lower overall cost vs debt-plus-interest models. The rising trajectory (0.22 → 0.38) reflects that securitization and administrative barriers are increasing effective extraction over time. Suppression (0.48): Moderate. Barriers to exit include: (i) income-contingency creates disincentive to career advancement (earnings suppression via moral hazard), (ii) underreporting income carries legal risk, (iii) relocation may trigger early repayment, (iv) hardship provisions are underutilized due to administrative friction. But suppression is not total — borrowers can exit via geographic relocation, career switching, or full repayment, unlike trapped debt where default is the only exit. Theater ratio (0.52): Moderate. ISA marketing emphasizes access and risk-sharing benefits while underspecifying income-contingency burden for low earners. Hardship provisions exist but are rhetorically downplayed. The rising trajectory (0.35 → 0.52) indicates that as ISA portfolios mature, rhetoric diverges more from operational reality — 'income-contingent risk-sharing' becomes the performative narrative masking 'prolonged earnings capture.'
 *
 * PERSPECTIVAL GAP:
 *   This constraint produces a sharp perspectival gap between high-income and low-income graduates. For high earners, ISA is pure coordination (Rope): it solves the access problem, repayment burden is light, and they can exit via full repayment. For low earners, ISA is extraction (Snare): they face prolonged income-contingency with no meaningful exit, income suppression via moral hazard, and administrative barriers to hardship relief. The mid-range graduate sees Tangled Rope: they benefit from access but constrain their own career decisions to minimize repayment burden. The education access coalition sees a temporary scaffold with a clear sunset as alternatives mature. The ISA provider sees Rope: they coordinate capital flows and benefit from asset securitization. The student debt system legacy sees Piton: ISA perpetuates income-contingency capture through institutional inertia, despite the availability of tuition-free alternatives that would make ISAs obsolete. The analytical observer at civilizational scale risks seeing a Mountain — education funding always requires capital allocation — but the structural data reveals this as a false summit: the specific design choice to impose income-contingency on individuals rather than funding education through public investment is contingent, not inevitable.
 *
 * DIRECTIONALITY LOGIC:
 *   Low-income graduate: Victim + trapped → d≈0.92, f(d)≈1.40. Maximum extraction. No exit options; repayment obligation persists even during income disruption. High-income graduate: Beneficiary + arbitrage → d≈0.10, f(d)≈0.05. Net beneficiary. Can exit via full repayment; burden is light relative to income. Mid-career graduate: Victim + constrained → d≈0.65, f(d)≈1.00. Significant extraction. Can exit but faces friction (relocation cost, underreporting risk, hardship form complexity). ISA provider: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.06. Negative extraction: provider benefits from portfolio effects and securitization. Education access coalition: Organized + mobile → d≈0.35, f(d)≈0.35. Low extraction; coalition has agency and alternative pathways. Student debt system: Institutional + constrained → d≈0.70, f(d)≈1.15. Piton derivation from theater ratio, not pure directionality — the system is constrained by institutional inertia despite having alternatives. Analytical observer: analytical → d≈0.72, f(d)≈1.15. False summit risk: naturalizes contingent design.
 *
 * MANDATROPHY ANALYSIS:
 *   SCAFFOLD RESOLUTION: ISA resolves the mandatrophy by demonstrating that a constraint can be simultaneously a coordination solution (for those who benefit from access) AND extraction (for those bearing prolonged repayment burden) AND temporary (because alternatives are maturing). The mandatrophy question is: 'Is this a Rope (coordination) or a Snare (extraction)?' The answer is both, depending on income trajectory and career path. The Scaffold classification captures this hybrid nature with a temporal trajectory: ISA functions as a legitimate scaffold (temporary access mechanism) only if alternative education funding actually matures within the term. If alternatives stall, ISA degrades into a Piton (theatrical persistence) or Snare (open extraction). The theater ratio (0.52, rising) is the key diagnostic: as performative content increases and operational reality (earnings suppression, hardship barriers) becomes clearer, the constraint's legitimacy as a coordination mechanism declines. The measurement trajectory (extractiveness rising from 0.22 to 0.38) shows accumulating rent-seeking behavior (securitization layers, administrative barriers, investor incentives to maximize collection) that transforms the constraint from pure access coordination into mixed coordination-extraction. The mandatrophy is resolved by recognizing that ISA's classification depends on whether the sunset clause is real: if tuition-free education, income-based repayment, or apprenticeships mature to provide genuine alternative access pathways, ISA remains a Scaffold. If those alternatives stall or are blocked by policy, ISA becomes a permanent Snare.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    income_contingency_exit_threshold,
    'At what income threshold does ISA function as coordination versus extraction?',
    'Longitudinal income tracking of ISA cohorts; modeling of welfare outcomes under ISA vs alternatives by income quintile; comparison of repayment burden across income distributions',
    'If threshold ≤ 1.5x poverty line: ISA is extraction for most borrowers (Snare). If threshold ≥ 3x median income: ISA is pure coordination (Rope). Current design shows mixed outcomes suggesting Scaffold/Tangled Rope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(income_contingency_exit_threshold, empirical, 'Income level at which ISA transitions from extraction to coordination').

omega_variable(
    alternative_education_funding_maturation,
    'Will free public higher education or skills-based hiring displace ISA, or will ISA become permanent?',
    'Policy tracking of tuition-free programs; measurement of employer adoption of competency-based hiring; longitudinal comparison of educational outcomes across funding models',
    'If alternatives mature: scaffold thesis confirmed — ISA has a real sunset. If alternatives stall: scaffold becomes aspirational — ISA may become Piton (degraded but persistent). If ISA expands despite alternatives: ISA is Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_education_funding_maturation, empirical, 'Whether alternative education funding models will displace ISA').

omega_variable(
    income_share_moral_hazard,
    'Do ISA terms create perverse incentives for career suppression or earnings underreporting?',
    'Comparison of career outcomes (job selection, further education, side income) for ISA vs loan-funded graduates; audit analysis of income reporting compliance; survey data on awareness of income-contingency effects',
    'If moral hazard is severe: ISA becomes a Snare (suppresses earnings potential for some cohorts). If minimal: ISA is genuinely Tangled Rope (mixed benefits and costs, but not purely extractive).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(income_share_moral_hazard, empirical, 'Whether ISA creates perverse incentives for earnings underreporting').

omega_variable(
    securitization_and_secondary_extraction,
    'Does ISA securitization and secondary market trading create extraction layers beyond the student-provider relationship?',
    'Analysis of ISA portfolio structures, securitization terms, and third-party investor incentives; measurement of total value transfer from graduates to secondary market actors',
    'If secondary extraction is significant: ISA graduates bear costs beyond nominal income share (true χ higher than apparent). If minimal: ISA is closer to pure coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(securitization_and_secondary_extraction, empirical, 'Degree of secondary market extraction in ISA structures').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(isa_education_scaffold, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(isa_tr_t0, isa_education_scaffold, theater_ratio, 0, 0.35).
narrative_ontology:measurement(isa_tr_t5, isa_education_scaffold, theater_ratio, 5, 0.45).
narrative_ontology:measurement(isa_tr_t10, isa_education_scaffold, theater_ratio, 10, 0.52).

% Extraction over time
narrative_ontology:measurement(isa_be_t0, isa_education_scaffold, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(isa_be_t5, isa_education_scaffold, base_extractiveness, 5, 0.3).
narrative_ontology:measurement(isa_be_t10, isa_education_scaffold, base_extractiveness, 10, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(isa_education_scaffold, resource_allocation).
narrative_ontology:affects_constraint(isa_education_scaffold, student_debt_burden).
narrative_ontology:affects_constraint(isa_education_scaffold, education_access_inequality).
narrative_ontology:affects_constraint(isa_education_scaffold, income_based_repayment_compliance).

% DUAL FORMULATION NOTE:
% ISA is downstream of the general education access problem (ε_high) but represents a specific financing mechanism with its own ε=0.38. The constraint family includes student loans (higher ε, pure Snare), income-based repayment (similar ε, Tangled Rope), and tuition-free alternatives (ε≈0.05, Rope). ISA occupies the middle ground: less extractive than traditional loans, more extractive than public funding, dependent on the maturation of alternatives for its legitimacy as a temporary scaffold.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
