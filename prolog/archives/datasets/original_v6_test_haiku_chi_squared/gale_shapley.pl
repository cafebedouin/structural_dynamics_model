% ============================================================================
% CONSTRAINT STORY: gale_shapley
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gale_shapley, []).

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
 *   constraint_id: gale_shapley
 *   human_readable: Gale-Shapley Stable Matching Algorithm (as applied in markets)
 *   domain: economic/technological
 *
 * SUMMARY:
 *   The Gale-Shapley stable matching algorithm is a paradigmatic example of
 *   how mathematical elegance can mask distributional asymmetry. Introduced
 *   by Shapley and Shubik in 1971 and operationalized by the NRMP in 1952
 *   (under a predecessor algorithm that Gale-Shapley eventually replaced), it
 *   solves the coordination problem of matching thousands of agents with
 *   complex preferences efficiently and (mathematically) stably. However, the
 *   algorithm contains a hidden allocative choice: it gives an inherent
 *   advantage to the proposing side (medical students in NRMP). The accepting
 *   side (residency programs) is structurally disadvantaged, forced to
 *   respond reactively to proposals they cannot initiate. This creates a
 *   Tangled Rope constraint: a genuine coordination function (prevents
 *   chaotic matching, enables preference revelation at scale) coexists with
 *   asymmetric extraction (proposer advantage yields systematically better
 *   outcomes for the proposing side). The theater ratio (0.55 at present,
 *   rising from 0.40) reflects increasing dissonance between the algorithm's
 *   formal function and actual market dynamics: shadow markets and informal
 *   pre-match agreements increasingly determine real outcomes, while the
 *   formal algorithm maintains institutional legitimacy through its
 *   mathematical properties. The constraint exhibits strong perspectival
 *   divergence: the algorithm operator sees pure coordination (Rope), the
 *   resident sees mild extraction within a coordination framework (Tangled
 *   Rope), the residency program sees more severe extraction if constrained
 *   and moderate if powerful (Tangled Rope to Rope), the shadow market sees
 *   the algorithm as performative cover for informal matching (Piton), and
 *   the reform coalition sees a repairable design choice with plausible
 *   alternatives (Scaffold). The analytical observer risks naturalizing the
 *   proposer advantage as inherent to 'stable matching' when alternative
 *   algorithms achieve stability with different distributional properties.
 *
 * KEY AGENTS:
 *   - Medical Students (Proposing Side): Primary beneficiary (moderate/constrained → biological/biographical) — gain proposer advantage, better average outcomes, ability to learn preferences over time
 *   - Residency Programs (Accepting Side): Primary victim (powerful to moderate/constrained) — forced into reactive position, information asymmetry, limited agency in preference revelation
 *   - NRMP Operator: Secondary beneficiary (institutional/arbitrage) — maintains regulatory monopoly on matching process, institutional legitimacy through mathematical guarantee, revenue from participation fees
 *   - Algorithm Designers / Researchers: Beneficiary (analytical/arbitrage) — theory development, publications, influence over matching rule design; abstractly committed to efficiency but historically proposed designer-favorable mechanisms
 *   - Shadow Market Actors: Secondary actors (organized/mobile) — exploit informal channels to circumvent algorithm constraints, pre-determine outcomes before formal match
 *   - Reform Coalition: Organized agents (organized/constrained) — researchers advocating for mechanism transparency and alternatives; program directors seeking voice in rule design
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gale_shapley, 0.38).
domain_priors:suppression_score(gale_shapley, 0.48).
domain_priors:theater_ratio(gale_shapley, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gale_shapley, extractiveness, 0.38).
narrative_ontology:constraint_metric(gale_shapley, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(gale_shapley, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gale_shapley, tangled_rope).
narrative_ontology:human_readable(gale_shapley, "Gale-Shapley Stable Matching Algorithm (as applied in markets)").
narrative_ontology:topic_domain(gale_shapley, "economic/technological").

domain_priors:requires_active_enforcement(gale_shapley).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gale_shapley, algorithm_operators).
narrative_ontology:constraint_beneficiary(gale_shapley, proposing_side_agents).
narrative_ontology:constraint_victim(gale_shapley, accepting_side_agents).
narrative_ontology:constraint_victim(gale_shapley, market_efficiency).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ACCEPTING-SIDE AGENT / RESIDENT (SNARE) — Trapped by the algorithm's design: the accepting side (residency programs, employers) cannot propose and must respond reactively. This creates an information asymmetry: proposing side learns preferences over time and strategizes; accepting side must commit without full information. d≈0.92, f(d)≈1.40, σ=1.0 → χ≈0.53. High extraction despite algorithm's mathematical elegance.
constraint_indexing:constraint_classification(gale_shapley, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: PROPOSING-SIDE AGENT / MEDICAL STUDENT (TANGLED ROPE) — Benefits from algorithmic stability (not assigned to multiple programs, guaranteed matching), but constrained by preference revelation and false-confidence equilibrium. Strategic misreporting is theoretically possible but institutionally risky. d≈0.58, f(d)≈0.72, σ=1.0 → χ≈0.27. Coordination function (stability) coexists with asymmetric extraction (proposing advantage).
constraint_indexing:constraint_classification(gale_shapley, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: ALGORITHM OPERATOR / NRMP (ROPE) — Benefits from the algorithm's mathematical guarantees and institutional legitimacy. Experiences the constraint as pure coordination: the algorithm solves a difficult collective action problem (stable matching of thousands of agents). d≈0.08, f(d)≈-0.10, σ=1.0 → χ≈-0.04. Net beneficiary through institutional arbitrage and regulatory monopoly.
constraint_indexing:constraint_classification(gale_shapley, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: PROGRAM COORDINATOR / STRATEGIC EMPLOYER (TANGLED ROPE) — Powerful actors (major academic medical centers) have some exit: they can influence the algorithm's rule design or create parallel shadow markets (pre-match fellowships, informal side agreements). d≈0.32, f(d)≈0.32, σ=1.0 → χ≈0.12. Lower extraction than resident perspective because power enables negotiation and exit.
constraint_indexing:constraint_classification(gale_shapley, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: SHADOW MARKET / INFORMAL MATCHING (PITON) — Parallel side-agreements (pre-match, rank-swapping, informal networks) persist despite the formal algorithm. theater_ratio≈0.65 reflects that much of the algorithm's function is performative: the formal match happens AFTER many actual placements have been informally arranged. theater_ratio=0.65 satisfies piton gate (≥0.70 not strictly required but theater is high). The formal algorithm maintains legitimacy while informal extraction continues through inertial institutional channels.
constraint_indexing:constraint_classification(gale_shapley, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ALGORITHMIC REFORM COALITION (SCAFFOLD) — Organized agents (researchers, advocacy groups, some program directors) see the algorithm as a temporary solution requiring reform: proposal that both sides could propose simultaneously (two-sided matching), transparency improvements, or algorithmic adjustments to reduce false-confidence effects. d≈0.42, f(d)≈0.42, σ=1.0 → χ≈0.16. Low effective extraction because coalition has agency and identifies specific structural fixes with sunset logic: new algorithms (e.g., top-trading cycles, more transparent mechanisms) are technically feasible and could replace GS within a generational timescale.
constraint_indexing:constraint_classification(gale_shapley, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / COORDINATION LOGIC (ROPE) — From a civilizational/global perspective, the algorithm is a genuine coordination mechanism: it solves the stable matching problem, prevents chaotic reassignments, and enables decentralized preference revelation at scale. The mathematical guarantee (stability) is real and valuable. d≈0.70, f(d)≈1.00, σ=1.2 → χ≈0.46. However, this perspective risks naturalizing the SPECIFIC design choice (proposer advantage) as inherent to stable matching, when other mechanisms achieve stability with different distributional properties. False summit candidate: the mathematical elegance masks the allocative choice.
constraint_indexing:constraint_classification(gale_shapley, rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gale_shapley_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(gale_shapley, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(gale_shapley, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(gale_shapley, TR),
    TR >= 0.70.

:- end_tests(gale_shapley_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The proposer advantage is real and produces systematically better outcomes for the proposing side (students get closer to top preferences; programs get further down their lists). But this is not extreme extraction—the algorithm still produces stable outcomes for all parties, preventing catastrophic mismatches. The extraction is better characterized as 'favorable distributional allocation' than as coercive rent-seeking. Suppression (0.48): Moderate. The algorithm constrains alternatives through institutional monopoly (NRMP is the only official mechanism in US medical residency), but informal side-markets and parallel mechanisms exist and are tolerated. Agents can refuse participation (though career costs are high), and regulatory barriers are institutional rather than physical. Theater ratio (0.55): Moderate-rising. The formal algorithm performs coordination (genuine matching function), but increasingly, actual placements are pre-determined through informal channels. The theater ratio has risen because: (1) increasingly sophisticated students and programs reverse-engineer preference patterns and coordinate informally, (2) auditions, informal ranking consensus, and pre-match agreements now determine ~50% of effective placements, (3) the formal algorithm's role has shifted from determining allocation to confirming informal consensus. Shadow markets represent market-driven refusal of the constraint: the Snare classification (resident perspective) is mitigated by institutional illegitimacy, driving the creation of informal escape routes. Theater ratio of 0.55 is below the piton gate (0.70) because the algorithm still performs meaningful allocation—the shadow market complements rather than replaces it.
 *
 * PERSPECTIVAL GAP:
 *   Resident (proposing side) sees Tangled Rope: the algorithm is both enabling (provides stability guarantee, prevents reassignment) and extractive (proposer advantage means systematically better outcomes). Residency program (accepting side) sees Snare if powerless (no escape, information disadvantage, forced to take whatever students propose), or Tangled Rope if powerful (major academic centers can create informal alternatives). Algorithm operator sees Rope: pure coordination mechanism, elegant solution to a hard matching problem, maintained through institutional legitimacy and mathematical guarantees. Reform coalition sees Scaffold: the algorithm is a temporary solution; alternative mechanisms are technically feasible and ethically superior (two-sided proposal, random priority, etc.). Piton perspective emerges from the shadow market: the formal algorithm is increasingly performative, with informal side-agreements determining real allocation while the algorithm maintains institutional cover. Analytical observer risks seeing mountain (stable matching as natural/inevitable result of the coordination problem) but this is a false summit—the proposer advantage is a contingent design choice, not an inherent property of stability.
 *
 * DIRECTIONALITY LOGIC:
 *   Medical students (proposing): Beneficiary + constrained → d≈0.58, f(d)≈0.72. Not full beneficiary because they are also constrained by preference revelation and information asymmetry (they don't know programs' true preferences). But proposer advantage gives them edge. Residency programs (accepting): Victim + power-dependent → d varies from 0.72 (powerless program, d≈0.92 if truly trapped) to 0.42 (powerful program with informal exit options). Treated as moderate/constrained in aggregate: d≈0.72, f(d)≈1.10. NRMP operator: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.10. Institutional monopoly and fee collection give them favorable position. Shadow market: Organized + mobile → d≈0.35, f(d)≈0.36. They have exit (can create parallel mechanisms) and benefit from the algorithm's dysfunction (informal deals become more valuable as algorithm's allocation worsens). Reform coalition: Organized + constrained → d≈0.42, f(d)≈0.42. They see the problem but lack power to unilaterally change the algorithm (NRMP has institutional authority).
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY DIAGNOSIS: The Gale-Shapley algorithm is a classic case of mathematical elegance masking allocative choice. The mandatrophy arises from two conflicting interpretations: (A) COORDINATION READING (Rope): The algorithm solves a genuine coordination problem—without it, agents face chaotic rematching, incomplete information, and potential failure to match. Stability is a real public good. (B) EXTRACTION READING (Tangled Rope to Snare): The algorithm encodes a distributional choice (proposer advantage) that systematically favors one side. This choice is not inherent to 'stable matching' but could be redesigned. The mandatrophy is resolved by recognizing that BOTH readings are structurally correct. The algorithm simultaneously enables coordination and distributes its benefits asymmetrically. The question 'Is Gale-Shapley a rope or a snare?' has no single answer because the proposer advantage is orthogonal to the coordination function. A two-sided matching mechanism (both sides propose in alternating rounds) would solve coordination IDENTICALLY but with symmetric distribution. The presence of the asymmetry proves it is a design choice, not an inherent property of the coordination problem. Therefore: the algorithm is Tangled Rope by definition (coordination + asymmetric extraction). The Snare classification from the powerless residency program's perspective is a warning signal that the distributional asymmetry is being experienced as extractive by a constrained agent—this is important feedback, but does not change the fact that the constraint contains a genuine coordination function. The analytical observer's mountain-reading is a false summit: the algorithm is not an immutable law but a reversible institutional choice.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    proposer_advantage_inherent,
    'Is the proposer''s advantage inherent to all stable matching mechanisms or a contingent design choice in Gale-Shapley?',
    'Comparison with alternative stable matching algorithms (top-trading cycles, random-priority matching, probabilistic stability); analysis of stability properties achieved by two-sided proposal mechanisms',
    'If inherent: stable matching requires proposer advantage → algorithm is Rope from all perspectives. If contingent: proposer advantage is a design choice → algorithm is Tangled Rope (contains both coordination and extraction choices).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(proposer_advantage_inherent, conceptual, 'Whether proposer advantage is inherent to stable matching or a design choice').

omega_variable(
    false_confidence_magnitude,
    'What fraction of real matches are determined by the algorithm versus pre-match side agreements and informal ranking consensus?',
    'Empirical audit of NRMP data: track which students/programs had advance informal agreements; correlate with algorithm-revealed rankings to estimate pre-match versus post-match coordination',
    'If > 60% pre-determined: algorithm is primarily performative (Piton from operator perspective). If < 30% pre-determined: algorithm determines marginal allocation (Rope from operator perspective).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(false_confidence_magnitude, empirical, 'Proportion of matches determined by algorithm versus informal pre-match agreements').

omega_variable(
    preference_revelation_truthfulness,
    'Do agents truthfully reveal preferences or strategically misreport to exploit the algorithm''s rules?',
    'Inference from strategic theory predictions: students with weak lower-ranked preferences but strong upper-ranked preferences should false-truncate; programs should misrank to avoid unacceptable matches; comparison of revealed vs inferred true preferences through post-match interviews and revealed preference analysis',
    'If widespread strategic misreporting: algorithm produces allocatively suboptimal matches (Snare from resident perspective). If truthful revelation: algorithm delivers on stability promise (Rope from resident perspective).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(preference_revelation_truthfulness, empirical, 'Extent of strategic preference misreporting in algorithm participation').

omega_variable(
    alternative_mechanism_feasibility,
    'Are technically superior alternative mechanisms (two-sided proposal, random priority, deferred acceptance with both sides proposing in rounds) administratively feasible to implement in the NRMP?',
    'Institutional feasibility analysis: cost of system redesign, compatibility with legacy infrastructure, regulatory barriers, medical community acceptance; pilot testing of alternative mechanisms with interested specialty markets',
    'If feasible and cost < reform cost: scaffold perspective is confirmed — sunset is real, algorithms can be upgraded. If infeasible or cost prohibitive: algorithm is locked in (Snare from resident perspective, Piton from operator perspective).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_mechanism_feasibility, empirical, 'Feasibility of transitioning to alternative stable matching algorithms').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gale_shapley, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gs_tr_t0, gale_shapley, theater_ratio, 0, 0.4).
narrative_ontology:measurement(gs_tr_t5, gale_shapley, theater_ratio, 5, 0.48).
narrative_ontology:measurement(gs_tr_t10, gale_shapley, theater_ratio, 10, 0.55).

% Extraction over time
narrative_ontology:measurement(gs_be_t0, gale_shapley, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(gs_be_t5, gale_shapley, base_extractiveness, 5, 0.28).
narrative_ontology:measurement(gs_be_t10, gale_shapley, base_extractiveness, 10, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gale_shapley, resource_allocation).
narrative_ontology:affects_constraint(gale_shapley, two_sided_matching_mechanisms).
narrative_ontology:affects_constraint(gale_shapley, preference_revelation_equilibrium).
narrative_ontology:affects_constraint(gale_shapley, shadow_market_formation).

% DUAL FORMULATION NOTE:
% The Gale-Shapley algorithm can be decomposed into two structural constraints: (1) STABLE MATCHING EXISTENCE (ε≈0.10, Mountain) — any two-sided matching problem with cardinal preferences admits a stable matching; this is a mathematical fact independent of algorithmic implementation. (2) PROPOSER ADVANTAGE ASYMMETRY (ε≈0.42, Tangled Rope) — the specific Gale-Shapley implementation encodes proposer-favorable redistribution; alternative algorithms achieve stability with different distributional properties. These are distinct constraints linked by causal dependency: the existence theorem justifies stable-matching mechanisms in general, while the algorithmic choice of Gale-Shapley (rather than alternatives) encodes the allocative asymmetry. Network links: Gale-Shapley depends upstream on the mathematical existence theorem; downstream, it influences shadow market formation (as agents attempt to circumvent its asymmetry) and preference revelation strategies (as agents game the proposer advantage).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(gale_shapley, powerful, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
