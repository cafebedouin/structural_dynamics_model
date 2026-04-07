% ============================================================================
% CONSTRAINT STORY: ergodic_theorems
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ergodic_theorems, []).

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
 *   constraint_id: ergodic_theorems
 *   human_readable: The Misapplication of Ergodic Theorems in Non-Ergodic Systems
 *   domain: economic/scientific
 *
 * SUMMARY:
 *   The misapplication of ergodic theorems in non-ergodic systems creates a
 *   structural epistemic constraint with both coordination and extraction
 *   components. Ergodic theorems are mathematically correct statements about
 *   systems where time-averaging equals ensemble-averaging. However, their
 *   application to economic systems, financial markets, and complex adaptive
 *   systems often violates the core assumption: most real systems exhibit
 *   path dependence, irreversibility, and non-ergodic dynamics where ensemble
 *   averages diverge from individual trajectories. This constraint operates
 *   as a tangled rope: mainstream economic modelers benefit from ergodic
 *   frameworks that provide mathematical tractability and institutional
 *   prestige (coordination function), while non-ergodic researchers,
 *   financial risk managers, and economically vulnerable populations bear the
 *   costs of policy failures arising from systemic underestimation of tail
 *   risks, inequality amplification, and path-dependent outcomes. The
 *   constraint is enforced through peer review gatekeeping, funding
 *   concentration, and curriculum design that treats ergodic assumptions as
 *   foundational. Theater ratio (0.65) reflects that peer review assesses
 *   mathematical form rather than empirical validity of ergodic assumptions.
 *   The constraint exhibits a clear temporal arc: extractiveness has risen
 *   from 0.22 to 0.38 over the interval as computational complexity has
 *   increased the modeler's reliance on ergodic simplifications despite
 *   growing empirical evidence of non-ergodicity in real systems.
 *
 * KEY AGENTS:
 *   - Mainstream Economic Modelers: Primary beneficiary (institutional/arbitrage) — gain citation advantage, funding concentration, and institutional prestige from ergodic frameworks
 *   - Non-Ergodic System Researchers: Primary victim (powerless/trapped) — face publication bias against non-equilibrium claims; cannot exit academic pipeline without career termination
 *   - Economically Vulnerable Populations: Primary victim (powerless/trapped) — subject to ergodic-based policies that systematically misestimate tail risks and underestimate path-dependent inequality amplification
 *   - Financial Risk Managers: Mixed experience (powerful/mobile) — benefit from quick ergodic models but face extraction through systematic underestimation of tail risks; have some exit via alternative models
 *   - Econophysics Coalition: Organized agents (organized/constrained) — building non-ergodic frameworks with clear sunset logic as computational capacity grows
 *   - Academic Peer Review System: Institutional gatekeeper (institutional/arbitrage) — maintains ergodic gatekeeping through performative review; persists through inertia
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ergodic_theorems, 0.38).
domain_priors:suppression_score(ergodic_theorems, 0.52).
domain_priors:theater_ratio(ergodic_theorems, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ergodic_theorems, extractiveness, 0.38).
narrative_ontology:constraint_metric(ergodic_theorems, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(ergodic_theorems, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ergodic_theorems, tangled_rope).
narrative_ontology:human_readable(ergodic_theorems, "The Misapplication of Ergodic Theorems in Non-Ergodic Systems").
narrative_ontology:topic_domain(ergodic_theorems, "economic/scientific").

domain_priors:requires_active_enforcement(ergodic_theorems).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ergodic_theorems, mainstream_economic_modelers).
narrative_ontology:constraint_beneficiary(ergodic_theorems, policy_advocates_for_equilibrium_frameworks).
narrative_ontology:constraint_victim(ergodic_theorems, non_ergodic_system_researchers).
narrative_ontology:constraint_victim(ergodic_theorems, long_term_risk_analysts).
narrative_ontology:constraint_victim(ergodic_theorems, economically_vulnerable_populations).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: NON-ERGODIC SYSTEM RESEARCHER (SNARE) — Trapped by institutional gatekeeping, peer review bias against non-equilibrium claims, and funding structures that reward equilibrium-based models. Cannot exit without career termination. The constraint extracts by suppressing alternative frameworks and forcing adoption of ergodic assumptions in publication pipelines. Maximum experienced extraction.
constraint_indexing:constraint_classification(ergodic_theorems, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: ECONOMICALLY VULNERABLE POPULATIONS (SNARE) — Subject to policy designed under ergodic assumptions (e.g., ergodic utility functions, time-reversible risk models, ensemble averaging that ignores path dependence). Cannot exit the economic system; bears full cost of policy errors arising from ergodic misapplication. Extraction operates through structural policy failure rather than explicit coercion.
constraint_indexing:constraint_classification(ergodic_theorems, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: MAINSTREAM ECONOMIC MODELERS (ROPE) — Benefits from ergodic-theorem-based frameworks that provide mathematical tractability, publication venues, and institutional prestige. Experiences the constraint as coordination: using ergodic tools solves the problem of making complex systems mathematically manageable. Net beneficiary through citation advantage and funding concentration. Can arbitrage between ergodic and non-ergodic frameworks.
constraint_indexing:constraint_classification(ergodic_theorems, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: FINANCIAL RISK MANAGERS (TANGLED ROPE) — Experience both coordination benefits (ergodic models provide quick risk estimates) and extraction costs (models systematically underestimate tail risks in non-ergodic markets, leading to portfolio failures). Have partial exit via alternative models (Nassim Taleb's fragility/antifragility, historical simulation) but face pressure to conform to ergodic consensus in regulatory reporting. Mixed experience with moderate effective extraction.
constraint_indexing:constraint_classification(ergodic_theorems, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: ACADEMIC PEER REVIEW AND PUBLISHING (PITON) — Maintains ergodic-theorem gatekeeping through publication bias and reviewer preferences, but the mechanism is largely performative. Reviewers assess mathematical rigor of ergodic assumptions rather than empirical validity of assumptions themselves. Theater ratio (0.65) reflects that peer review evaluates form rather than substance. The editorial system persists through institutional inertia despite recognized limitations in catching fundamental assumptions.
constraint_indexing:constraint_classification(ergodic_theorems, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / PHYSICAL LAW VIEW (MOUNTAIN) — From a universal perspective, ergodic theorems are mathematical truths: IF a system is ergodic, THEN time-averaging equals ensemble-averaging. This is a natural law. However, the constraint's actual impact comes from the misapplication of this law to non-ergodic systems where the assumption fails. The engine will detect this as a false summit — the mathematical truth is being naturalized into false claims about system structure.
constraint_indexing:constraint_classification(ergodic_theorems, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 7: ECONOPHYSICS COALITION (SCAFFOLD) — Organized agents (complexity economists, agent-based modelers, path-dependent systems researchers) are building alternative frameworks with explicit non-ergodic foundations. See the ergodic-theorem constraint as a temporary coordination failure with a sunset: as computational power grows and non-equilibrium methods mature, institutions will shift toward non-ergodic foundations. Sunset estimated at 15-25 years as undergraduate curricula change. Low effective extraction due to organized exit pathway.
constraint_indexing:constraint_classification(ergodic_theorems, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ergodic_theorems_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(ergodic_theorems, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(ergodic_theorems, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(ergodic_theorems, TR),
    TR >= 0.70.

:- end_tests(ergodic_theorems_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The constraint extracts through enforced use of mathematically convenient but empirically false assumptions. The extraction mechanism is not direct coercion but structural policy failure: decision-makers using ergodic models for non-ergodic systems systematically underestimate tail risks and miscalculate inequality dynamics. Extractiveness is not higher (0.60+) because alternative frameworks exist and some institutions (quant funds, complexity economists) have partially escaped. Suppression (0.52): Moderate-high. Barriers include publication bias against non-equilibrium claims, curriculum gatekeeping, reviewer preferences for ergodic mathematics, and funding concentration in equilibrium-based research. However, suppression is declining as computational methods mature and empirical failures of ergodic models become undeniable (2008, pandemic disruptions). Theater ratio (0.65): Moderate-high. Peer review evaluates mathematical rigor of ergodic assumptions rather than empirical validity of the assumptions themselves. The gatekeeping is performative — reviewers assess whether an ergodic model is well-executed, not whether ergodicity actually holds for the system being modeled. This represents Goodhart drift: the publication process has substituted assessment of model elegance for assessment of empirical correctness.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates clear perspectival bifurcation. The beneficiary (mainstream modeler) sees coordination and mathematical elegance. The victim (non-ergodic researcher) sees suppression and institutional gatekeeping. The vulnerable population sees policy failure: ergodic utility theory predicts stable equilibrium welfare; non-ergodic analysis reveals path-dependent inequality amplification and welfare floors. The analytical observer confronts a choice: either naturalize the constraint as inherent to mathematics (false summit) or recognize it as a contingent institutional enforcement of one framework despite empirical evidence of its failure. The scaffold perspective (econophysics coalition) identifies the real structural trajectory: as computational power increases and non-equilibrium methods mature, institutions will shift. This is not inevitable but likely if empirical pressure accumulates and alternative institutional incentives emerge.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is determined by structural position relative to extraction. Mainstream modelers benefit from ergodic frameworks (d ≈ 0.1) — they gain tractability, citation advantage, and institutional standing. They experience low or negative effective extraction because the constraint subsidizes their work. Non-ergodic researchers face trapping through publication bias and curriculum gatekeeping (d ≈ 0.90) — they experience high effective extraction as their work is systematically suppressed. Vulnerable populations cannot exit the economic system and must accept policies designed under false ergodic assumptions (d ≈ 0.95) — maximum extraction. Financial risk managers have partial exit via alternative models (d ≈ 0.55) — moderate extraction. Organized econophysics researchers have exit pathways and coalition support (d ≈ 0.35) — reduced extraction. The academic peer review system itself maintains the constraint through inertia (piton perspective) — the theater ratio reveals that review is form-checking rather than substance-checking.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy resolves by identifying the constraint as genuinely tangled: it provides both coordination (mathematical tractability for complex systems) and extraction (systematic bias against non-ergodic frameworks and the researchers who pursue them). The constraint is not 'pure extraction hiding as coordination' but rather 'coordination that has been monopolized by a particular mathematical school through institutional gatekeeping.' The resolution requires recognizing that non-ergodic mathematics can also provide tractability (agent-based models, network models, fractal geometries) but this requires different pedagogical and institutional infrastructure. The false summit (analytical observer seeing mathematical necessity) is debunked by the existence of thriving non-ergodic research programs in complexity science and econophysics — these researchers face extraction through institutional suppression, not because non-ergodic mathematics is intractable but because ergodic-trained reviewers and administrators discount it as less rigorous. The mandatrophy is resolved by distinguishing the mathematical property (ergodicity) from the institutional enforcement (gatekeeping), and by recognizing that institutional reform can shift which frameworks receive prestige and funding without compromising mathematical standards.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    ergodicity_testability_threshold,
    'What empirical signature definitively distinguishes ergodic from non-ergodic behavior in real economic and physical systems?',
    'High-frequency data analysis; correlation structure in returns/outcomes across long time horizons; tests for irreversibility and path dependence',
    'If testability is high: constraint weakens because empirical falsification becomes routine. If testability is low: the ergodic assumption persists as an analytical default despite empirical uneasiness.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ergodicity_testability_threshold, empirical, 'Empirical threshold for distinguishing ergodic from non-ergodic systems').

omega_variable(
    policy_error_attribution,
    'How many documented economic policy failures (2008 crisis, income inequality, pandemic disruption) can be traced to ergodic-theorem misapplication rather than other modeling errors?',
    'Post-hoc analysis of policy models used in decision-making; comparison of predictions from ergodic vs non-ergodic models on historical datasets; expert consensus on attribution',
    'If attribution is high (>50%): constraint represents major epistemic hazard and institutional reform pressure. If attribution is low (<20%): ergodic theorems are not the bottleneck — other factors dominate policy failure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(policy_error_attribution, empirical, 'Attribution of economic policy failures to ergodic-theorem misapplication').

omega_variable(
    mathematical_pedagogy_path_dependence,
    'Is the entrenchment of ergodic theorems in undergraduate curricula mathematically necessary or historically contingent?',
    'Curriculum history analysis; comparison of countries with different mathematical traditions; feasibility assessments for non-ergodic-first pedagogy',
    'If contingent: curriculum reform becomes plausible within a generation. If necessary: change requires breakthrough pedagogical innovations before institutional shift.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(mathematical_pedagogy_path_dependence, conceptual, 'Whether ergodic pedagogy is mathematically necessary or historically contingent').

omega_variable(
    institutional_incentive_reform,
    'Can peer review and funding mechanisms be reformed to reward non-ergodic frameworks without destabilizing mathematical standards?',
    'Design of alternative review criteria; pilot programs with differential funding streams; measurement of publication rates and citation impact for non-ergodic work',
    'If reformable: scaffold perspective is structural. If not: constraint persists through institutional inertia (piton).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_incentive_reform, preference, 'Feasibility of institutional incentive reform for non-ergodic work').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ergodic_theorems, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ergodic_tr_t0, ergodic_theorems, theater_ratio, 0, 0.48).
narrative_ontology:measurement(ergodic_tr_t5, ergodic_theorems, theater_ratio, 5, 0.58).
narrative_ontology:measurement(ergodic_tr_t10, ergodic_theorems, theater_ratio, 10, 0.65).

% Extraction over time
narrative_ontology:measurement(ergodic_be_t0, ergodic_theorems, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(ergodic_be_t5, ergodic_theorems, base_extractiveness, 5, 0.3).
narrative_ontology:measurement(ergodic_be_t10, ergodic_theorems, base_extractiveness, 10, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ergodic_theorems, information_standard).
narrative_ontology:affects_constraint(ergodic_theorems, financial_tail_risk_underestimation).
narrative_ontology:affects_constraint(ergodic_theorems, inequality_amplification_mechanisms).
narrative_ontology:affects_constraint(ergodic_theorems, policy_design_under_false_equilibrium_assumptions).

% DUAL FORMULATION NOTE:
% The ergodic-theorems constraint decomposes into multiple structurally distinct claims: (1) the mathematical truth that ergodic systems exhibit time-ensemble equivalence (Mountain, ε ≈ 0.05); (2) the empirical claim that specific economic/financial systems are ergodic (Tangled Rope, ε ≈ 0.38, contested); (3) the institutional enforcement that ergodic frameworks should dominate policy design despite non-ergodicity (Snare from victim perspective, ε ≈ 0.52). This story addresses the third formulation. The mathematical truth is invariant; the empirical claim has lower extractiveness and is being resolved by data; the institutional enforcement is the binding constraint on alternative frameworks.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
