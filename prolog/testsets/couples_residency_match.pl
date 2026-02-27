% ============================================================================
% CONSTRAINT STORY: couples_residency_match
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_couples_residency_match, []).

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
 *   constraint_id: couples_residency_match
 *   human_readable: The Medical Residency Couples Match Algorithm
 *   domain: technological/economic
 *
 * SUMMARY:
 *   The National Resident Matching Program's Couples Match algorithm
 *   exemplifies how a coordination mechanism can gradually accrete extraction
 *   properties through institutional ossification. Established in the 1990s
 *   to solve a real collective action problem—high-quality applicants
 *   declining the match or leaving medicine entirely when forced to choose
 *   between career and relationship—the algorithm has become a hybrid
 *   mechanism that delivers genuine coordination benefit to institutions
 *   while exacting increasing extraction costs from coupled applicants. The
 *   constraint operates through a technological apparatus (the matching
 *   algorithm) and economic incentive structure (program director
 *   preferences, applicant desperation, career irreversibility). Different
 *   stakeholders experience the same mechanism radically differently: program
 *   directors see pure coordination value; coupled applicants see an
 *   inescapable asymmetric cost structure; single applicants see a secondary
 *   harm; lower-tier programs see competitive disadvantage; and the
 *   governance authority sees an increasingly ceremonial legitimacy ritual.
 *   The mechanism's theater has risen over three decades as algorithmic
 *   complexity has increased without corresponding transparency or appeals
 *   mechanisms, and as the underlying matching criterion has drifted from
 *   stated couple welfare maximization toward de facto program stability
 *   optimization.
 *
 * KEY AGENTS:
 *   - Coupled Applicants: Primary victims (powerless/trapped) — bear maximum extraction cost through algorithmic lock-in during critical career decision year
 *   - Program Directors / NRMP Leadership: Primary beneficiaries (institutional/arbitrage) — experience algorithm as coordination mechanism solving recruitment problem
 *   - Single Applicants: Secondary victims (moderate/constrained) — harmed through pool composition changes but also benefit from reduced competition
 *   - Elite Programs: Secondary beneficiaries (powerful/mobile) — increase match stability and leverage over applicants
 *   - Lower-Tier Programs: Secondary victims (moderate/constrained) — structurally disadvantaged by couples matching, left with reduced candidate pools
 *   - Algorithm Governance Authority: Institutional actor (institutional/arbitrage) — maintains mechanism through procedural legitimacy despite increasing opacity
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(couples_residency_match, 0.38).
domain_priors:suppression_score(couples_residency_match, 0.52).
domain_priors:theater_ratio(couples_residency_match, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(couples_residency_match, extractiveness, 0.38).
narrative_ontology:constraint_metric(couples_residency_match, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(couples_residency_match, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(couples_residency_match, tangled_rope).
narrative_ontology:human_readable(couples_residency_match, "The Medical Residency Couples Match Algorithm").
narrative_ontology:topic_domain(couples_residency_match, "technological/economic").

domain_priors:requires_active_enforcement(couples_residency_match).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(couples_residency_match, program_directors).
narrative_ontology:constraint_beneficiary(couples_residency_match, institutional_match_stability).
narrative_ontology:constraint_victim(couples_residency_match, coupled_applicants).
narrative_ontology:constraint_victim(couples_residency_match, single_applicants).
narrative_ontology:constraint_victim(couples_residency_match, lower_tier_programs).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: COUPLED APPLICANT (SNARE) — Trapped within the match algorithm with no exit option during the critical matching year. Must accept whatever joint outcome the algorithm produces or face costly delays (postponing match, geographic separation, leaving medicine). Bears asymmetric cost: if match fails, career and life plans collapse simultaneously. Cannot negotiate, cannot exit during match window, cannot reverse decision. Maximum experienced extraction.
constraint_indexing:constraint_classification(couples_residency_match, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: SINGLE APPLICANT (TANGLED ROPE) — Benefits from the couples match's existence because it removes some highly-qualified applicants from the single pool (improving their relative position). Also harmed because the algorithm's prioritization of couple stability reduces total matching efficiency, leaving more positions unfilled and creating cascading effects. Constrained exit: can apply multiple times or delay, but this is costly. Mixed extraction and coordination benefit.
constraint_indexing:constraint_classification(couples_residency_match, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: PROGRAM DIRECTORS / NRMP (ROPE) — Experience the couples match as a pure coordination mechanism. The algorithm solves a collective action problem: without couples matching, many high-quality applicants withdraw or mismatch. With it, programs obtain stable placements and reduce vacancy rates. Directors can arbitrage (adjust ROL strategy year-to-year). Net benefit with low extraction cost — the mechanism coordinates a Pareto-efficient outcome from their institutional vantage point.
constraint_indexing:constraint_classification(couples_residency_match, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: ELITE PROGRAMS (TANGLED ROPE) — Benefit from couples matching because it increases match stability, reducing post-match attrition and improving cohort quality. Also benefit from increased leverage over applicants: couples have lower bargaining power (must choose between couple security and individual program preference). Can extract through misaligned preference signaling. Mobile exit: can modify their ROL strategy across years. Both coordination benefit and asymmetric extraction present.
constraint_indexing:constraint_classification(couples_residency_match, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: LOWER-TIER PROGRAMS (SNARE) — Structurally harmed by couples matching. High-quality applicants who couple are more likely to rank elite programs, reducing competition for lower-tier programs. Vacancy rates increase. Cannot exit: must participate in NRMP system to recruit residents. Forced to accept whatever remaining applicants couple-match processes leave behind. Suppression: regulatory requirement to use NRMP.
constraint_indexing:constraint_classification(couples_residency_match, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: MATCH ALGORITHM AUTHORITY / NRMP GOVERNANCE (PITON) — Maintains the couples match algorithm largely through institutional inertia and ceremonial oversight. The algorithm performs its stated function (matching couples) but increasingly serves as theater for managing market stability. The governance structure is degraded: algorithmic decisions are not subject to real transparency, appeals processes are limited, and the underlying matching criterion (couple welfare maximization vs. Pareto efficiency) is never publicly debated. The mechanism persists because no coalitional force has successfully challenged the NRMP's monopoly. Theater ratio is moderate but rising as algorithmic complexity increases without corresponding accountability.
constraint_indexing:constraint_classification(couples_residency_match, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(couples_residency_match_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(couples_residency_match, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(couples_residency_match, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(couples_residency_match, TR),
    TR >= 0.70.

:- end_tests(couples_residency_match_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The couples match algorithm extracts from coupled applicants through (a) reduced bargaining power during matching (programs can offer suboptimal couple matches knowing couples have fewer exit options), (b) geographic lock-in (acceptance commits both parties to one location regardless of individual preference), and (c) temporal irreversibility (a failed couple match costs an entire year). However, extraction is not maximal because the algorithm does deliver some genuine coordination value (preventing couples from leaving medicine entirely, enabling some couple matches that would not occur under single matching). The value of 0.38 reflects moderate extraction with partial legitimacy. Suppression (0.52): Moderate-high. Couples face significant barriers to exit or negotiation: the matching window is compressed, the algorithm is opaque, appeals are limited, and the regulatory environment (NRMP monopoly plus medical licensure requirements) creates trapped conditions. However, suppression is not total because couples can theoretically delay match, pursue individual matching, or leave medicine—these are costly but not impossible. Theater ratio (0.48): Moderate. The algorithm's stated purpose (couple welfare maximization) has drifted toward actual purpose (program stability optimization) without formal acknowledgment. The governance ritual around the algorithm emphasizes transparency and fairness, but the actual mechanism is increasingly a black box. Theater has risen over time as the algorithm has become more complex and less subject to public scrutiny.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates maximum perspectival divergence. Program directors perceive pure coordination (Rope) and experience no extraction—the algorithm solves a genuine recruitment problem. Coupled applicants perceive asymmetric extraction (Snare) and experience maximum cost. Elite programs perceive mixed coordination and extraction advantage (Tangled Rope). Single applicants perceive a mechanism that both helps and harms them (Tangled Rope). Lower-tier programs perceive pure extraction disadvantage (Snare). The algorithm authority perceives an increasingly ceremonial mechanism (Piton) maintained through institutional inertia. The gap between the beneficiary's view (this solves a real problem) and the victim's view (this locks us into suboptimal outcomes) is unbridgeable without examining the structural asymmetries in bargaining power, exit options, and information distribution.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is determined by structural position within the matching ecosystem. Coupled applicants occupy the highest-d position (0.90+): they are maximally dependent on the algorithm's outcome, have trapped exit options, and bear concentrated costs. Program directors occupy the lowest-d position (0.10-0.20): they are beneficiaries with arbitrage options (can adjust ROL strategy year-to-year), face low cost from mechanism malfunction, and capture the coordination benefit. Single applicants occupy a middle position (0.55): they benefit from the reduced competition of coupled applicants but are harmed by reduced matching efficiency. The derivation chain maps these structural positions through f(d), producing experienced extractiveness that matches each group's actual perceptions of fairness and burden.
 *
 * MANDATROPHY ANALYSIS:
 *   The couples match resolves the mandatrophy by demonstrating that legitimate coordination needs can become vehicles for extraction when structural asymmetries are present. The algorithm genuinely solves the problem it claims to solve (enabling couples to match), which is a coordination benefit. However, the solution creates extracted costs that program directors do not bear. The mandatrophy question becomes: 'Is the extraction incidental to coordination, or is the extraction the mechanism's actual function disguised as coordination?' The data suggests the answer has shifted over time. In the 1990s-2000s, couples match was primarily coordination (Rope or light Tangled Rope). By 2020s, program director behavior has increasingly exploited the reduced bargaining power of couples, and algorithmic opacity has increased theater ratio. The constraint is transitioning from Tangled Rope toward Snare as the coordination function atrophies and the extraction function becomes dominant. This lifecycle drift is captured in the measurements: base_extractiveness rising from 0.22 to 0.38, theater_ratio rising from 0.35 to 0.48 over 30 years.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    algorithmic_coupling_stability,
    'Does the couples match algorithm actually maximize couple welfare, or does it primarily maximize program director convenience while claiming to serve couples?',
    'Longitudinal tracking of couple outcomes post-match: geographic separation rates, career trajectory divergence, relationship dissolution. Comparison with counterfactual (couples independently matching vs. coupled matching). Survey data on couple satisfaction with algorithm-proposed outcomes.',
    'If couples genuinely benefit: tangled_rope classification is correct, and suppression/extraction is moderate justified coordination cost. If couples are worse off: couples match is a pure extraction mechanism disguised as beneficence (snare reclassification across all perspectives).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(algorithmic_coupling_stability, empirical, 'Whether couples match maximizes couple welfare or program stability').

omega_variable(
    alternative_matching_feasibility,
    'Could a decoupled algorithm (each member of couple submits independent ROL with geographic or timing constraints) achieve comparable stability outcomes with less extraction?',
    'Computational modeling of alternative matching mechanisms. Historical analysis of pre-couples-match era match outcomes. Simulation of constraint-based matching on actual NRMP data.',
    'If feasible: couples match is a form of unnecessary extraction, and classification shifts toward snare from all victim perspectives. If infeasible: coupling mechanism genuinely solves an intractable coordination problem, justifying current extraction as necessary suppression cost.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(alternative_matching_feasibility, empirical, 'Feasibility of decoupled matching with geographic constraints').

omega_variable(
    information_asymmetry_extraction,
    'Do program directors systematically exploit couples'' reduced bargaining power through preference signaling (e.g., offering couple sub-optimal joint matches that individual negotiation would reject)?',
    'Econometric analysis of couple rank list clustering vs. single applicant clustering. Interview data with couples and program directors. Pattern analysis of acceptance rates: do couples accept lower-preference matches at higher rates than singles?',
    'If systematic exploitation detected: extractiveness increases to 0.55+, suppression rises to 0.65+. Classification becomes pure snare from couple perspective. If exploitation absent: extraction is incidental rather than structural, classification remains tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(information_asymmetry_extraction, empirical, 'Whether programs exploit couple bargaining weakness').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(couples_residency_match, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(couples_tr_t0, couples_residency_match, theater_ratio, 0, 0.35).
narrative_ontology:measurement(couples_tr_t15, couples_residency_match, theater_ratio, 15, 0.42).
narrative_ontology:measurement(couples_tr_t30, couples_residency_match, theater_ratio, 30, 0.48).

% Extraction over time
narrative_ontology:measurement(couples_be_t0, couples_residency_match, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(couples_be_t15, couples_residency_match, base_extractiveness, 15, 0.3).
narrative_ontology:measurement(couples_be_t30, couples_residency_match, base_extractiveness, 30, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(couples_residency_match, resource_allocation).
narrative_ontology:affects_constraint(couples_residency_match, medical_labor_market_monopsony).
narrative_ontology:affects_constraint(couples_residency_match, physician_geographic_distribution).
narrative_ontology:affects_constraint(couples_residency_match, dual_career_household_economics).

% DUAL FORMULATION NOTE:
% The couples match algorithm is downstream of structural market conditions (physician labor monopsony, geographic scarcity of high-quality training programs) but represents a distinct constraint operating at the algorithmic level. Upstream constraints (monopsony power of large institutions, geographic concentration of top-tier residencies) create the conditions under which couples matching becomes an extractive mechanism rather than a pure coordination tool.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(couples_residency_match, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
