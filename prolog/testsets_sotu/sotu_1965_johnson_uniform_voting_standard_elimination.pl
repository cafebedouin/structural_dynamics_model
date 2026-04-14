% ============================================================================
% CONSTRAINT STORY: sotu_1965_johnson_uniform_voting_standard_elimination
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sotu_1965_johnson_uniform_voting_standard_elimination, []).

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
 *   constraint_id: sotu_1965_johnson_uniform_voting_standard_elimination
 *   human_readable: Elimination of Discriminatory Voter Qualification Tests and Federal Uniform Voting Standards (1965)
 *   domain: governance/voting_rights
 *
 * SUMMARY:
 *   The elimination of discriminatory voter qualification tests and
 *   establishment of federal uniform voting standards (1965) represents a
 *   forcible correction of a snare that extracted political power from Black
 *   citizens through ostensibly neutral but selectively enforced barriers.
 *   Literacy tests, poll taxes, grandfather clauses, and registrar discretion
 *   to reject applications created an elaborate suppression system that
 *   locked millions of disenfranchised citizens out of the electoral process.
 *   The federal constraint eliminates these tools and imposes objective,
 *   uniform standards administered by federal oversight where necessary. This
 *   constraint exhibits the full structural tension between elimination of
 *   discriminatory tools and loss of local control: disenfranchised citizens
 *   gain access (snare elimination), but state and local officials lose
 *   discretionary power, and citizens accustomed to local control experience
 *   federal imposition. The constraint's evolution shows declining
 *   extractiveness (0.72 → 0.38) as federal enforcement becomes internalized,
 *   but persistent suppression (0.72 throughout) reflecting the ongoing
 *   conflict over whether federal uniformity or local flexibility should
 *   govern voting rules.
 *
 * KEY AGENTS:
 *   - Disenfranchised Black Citizens: Primary victim (powerless/trapped) — locked out of voting by literacy tests, poll taxes, and registrar discretion; benefit directly from federal uniform standards and loss of registrar discretion
 *   - State and Local Registrars: Primary extractors (institutional/trapped) — lose discretionary power to exclude voters; experience federal mandate as suppressive constraint; cannot exit federal jurisdiction
 *   - Poor White Citizens in Literacy-Test States: Secondary actor (moderate/constrained) — bear literacy test requirement but benefit from registrar discretion leniently applied; lose preferential treatment under federal uniformity
 *   - Federal Voting Rights Administration: Secondary beneficiary (institutional/arbitrage) — gains institutional authority, oversight power, and ability to adjudicate voting rights disputes clearly
 *   - White Citizens Accustomed to Local Control: Secondary victim (powerful/constrained) — constrained by federal mandate but retain exit options (relocation, litigation, political organizing); experience loss of local control over registration rules
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing federal uniformity as immutable democratic law while obscuring its active enforcement requirement and suppression of alternatives
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sotu_1965_johnson_uniform_voting_standard_elimination, 0.38).
domain_priors:suppression_score(sotu_1965_johnson_uniform_voting_standard_elimination, 0.72).
domain_priors:theater_ratio(sotu_1965_johnson_uniform_voting_standard_elimination, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sotu_1965_johnson_uniform_voting_standard_elimination, extractiveness, 0.38).
narrative_ontology:constraint_metric(sotu_1965_johnson_uniform_voting_standard_elimination, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(sotu_1965_johnson_uniform_voting_standard_elimination, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sotu_1965_johnson_uniform_voting_standard_elimination, tangled_rope).
narrative_ontology:human_readable(sotu_1965_johnson_uniform_voting_standard_elimination, "Elimination of Discriminatory Voter Qualification Tests and Federal Uniform Voting Standards (1965)").
narrative_ontology:topic_domain(sotu_1965_johnson_uniform_voting_standard_elimination, "governance/voting_rights").

domain_priors:requires_active_enforcement(sotu_1965_johnson_uniform_voting_standard_elimination).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sotu_1965_johnson_uniform_voting_standard_elimination, disenfranchised_black_citizens).
narrative_ontology:constraint_beneficiary(sotu_1965_johnson_uniform_voting_standard_elimination, voters_in_low_literacy_regions).
narrative_ontology:constraint_victim(sotu_1965_johnson_uniform_voting_standard_elimination, state_registrars_with_discretionary_power).
narrative_ontology:constraint_victim(sotu_1965_johnson_uniform_voting_standard_elimination, white_citizens_accustomed_to_local_control).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DISENFRANCHISED BLACK CITIZEN (SNARE) — Trapped by literacy test requirements, poll tax, grandfather clause, good character requirements, and registrar discretion to reject applications for pretextual reasons. No exit from the constraint system without federal intervention. Bears maximum suppression without coordination benefit. The constraint's suppression (0.72) reflects the multiple overlapping barriers designed to exclude this agent regardless of literacy or qualification.
constraint_indexing:constraint_classification(sotu_1965_johnson_uniform_voting_standard_elimination, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: POOR WHITE CITIZEN IN LITERACY-TEST STATES (TANGLED ROPE) — Constrained by literacy test barriers but benefits from implementation discretion: registrars typically administered tests leniently for white applicants while rejecting identical responses from Black applicants. Bears some suppression (the test exists) but gains coordination benefit from the local discretion system and often receives preferential treatment. The federal standard extracts this agent's preferential access while removing the registrar's discretionary tool.
constraint_indexing:constraint_classification(sotu_1965_johnson_uniform_voting_standard_elimination, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: FEDERAL VOTING RIGHTS ADMINISTRATION (ROPE) — Benefits from the uniform standard through institutional legitimacy, simplified federal oversight, and authority expansion. Experiences the constraint as coordination: establishing objective criteria enables federal courts to adjudicate voting rights disputes clearly. Net beneficiary — this institutional actor gains power and clarity through federal standardization.
constraint_indexing:constraint_classification(sotu_1965_johnson_uniform_voting_standard_elimination, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: STATE AND LOCAL REGISTRARS (SNARE) — Trapped by federal mandate eliminating the discretionary tools (literacy tests, good character clauses, middle-name requirements) that enabled selective registration. Loss of administrative discretion is experienced as severe suppression. The constraint requires active enforcement (federal examiners can be sent to oversee registration in covered jurisdictions). High extraction from this institutional actor: they lose power, discretion, and the ability to implement community preferences about voter qualification.
constraint_indexing:constraint_classification(sotu_1965_johnson_uniform_voting_standard_elimination, snare,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 5: WHITE CITIZENS ACCUSTOMED TO LOCAL CONTROL (PITON) — Perceive the federal mandate as imposing external rules on local practice. Constrained by federal enforcement but retain substantial exit options (relocation, private organizing, litigation). The constraint's theater_ratio (0.35) reflects that the federal standard is functionally justified (preventing discriminatory administration) rather than performative. This perspective is piton because the constraint replaces localized discretion with legible federal rules, eliminating the prior theater of 'local knowledge' and 'community standards.' The theatrical covering is gone; the extraction is now visible.
constraint_indexing:constraint_classification(sotu_1965_johnson_uniform_voting_standard_elimination, piton,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / UNIVERSAL SUFFRAGE VIEW (MOUNTAIN) — From a long-term universal perspective, the elimination of literacy tests and discretionary voter qualification represents a move toward a natural law of democratic governance: one-person-one-vote, independent of administrators' subjective judgments about qualification. This perspective sees the constraint as converging on an immutable principle of democratic legitimacy. However, the structural data reveals a false summit: this constraint is not a natural law but a forced equilibrium requiring active federal enforcement. The 'naturalness' of uniform suffrage is maintained by ongoing suppression of alternatives (literacy-test advocates, local-control proponents).
constraint_indexing:constraint_classification(sotu_1965_johnson_uniform_voting_standard_elimination, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sotu_1965_johnson_uniform_voting_standard_elimination_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(sotu_1965_johnson_uniform_voting_standard_elimination, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(sotu_1965_johnson_uniform_voting_standard_elimination, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(sotu_1965_johnson_uniform_voting_standard_elimination, TR),
    TR >= 0.70.

:- end_tests(sotu_1965_johnson_uniform_voting_standard_elimination_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The constraint primarily corrects prior extraction (literacy tests extracting voting power from Black citizens) rather than creating new extraction. However, residual extraction persists through federal enforcement burden on states, lost local control, and the ongoing need to suppress alternative qualification schemes (literacy tests, character clauses). The extractiveness value reflects this middle ground: not the maximal extraction of a snare (0.72 pre-elimination) but substantial constraint on state discretion. Suppression (0.72): High. The constraint actively suppresses registrar discretion, local control preferences, and alternative voter qualification schemes. Federal enforcement mechanisms (federal examiners, Justice Department pre-clearance) represent active coercion preventing backsliding. The suppression is structural and persistent — without federal enforcement, the prior discriminatory system re-emerges (as documented by multiple Voting Rights Act challenges and subsequent suppression). Theater ratio (0.35): Low-moderate. The federal uniform standard is functionally justified (prevents discriminatory administration) rather than performative. Literacy tests had high theater (0.88 in t0) because they provided cover for discretionary exclusion; the federal standard eliminates this theatrical cover by removing the registrar's discretionary tool entirely. The remaining theater (0.35) reflects the modest performance of federal enforcement and bureaucratic administration, but the core function (preventing discrimination) is genuine.
 *
 * PERSPECTIVAL GAP:
 *   The fundamental perspectival gap is between correction and suppression: the constraint corrects historical extraction (disenfranchisement) but does so by suppressing alternatives (local control, registrar discretion, community governance). The disenfranchised citizen sees snare elimination; the registrar sees snare imposition. Neither perspective is wrong — they are structurally accurate from their position. The tangled-rope classification at moderate/constrained reflects this hybrid: the constraint has a genuine coordination function (enabling access to voting), a genuine extractive function (suppressing local control), and requires active enforcement. The piton classification from the powerful/constrained position reflects that federal uniformity strips away the theatrical cover of local knowledge and community standards, making the extraction visible. The mountain classification from the analytical perspective risks naturalizing this forced equilibrium as an immutable principle of democratic governance.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality computation: Disenfranchised citizens (beneficiary + trapped exit) derive d ≈ 1.0 (they are maximally trapped victims who benefit from constraint removal) → f(d) ≈ 1.42 → experienced extraction is high suppression elimination. State registrars (victim of federal mandate + trapped exit) derive d ≈ 0.95 (they are trapped by federal constraint, cannot exit jurisdiction) → f(d) ≈ 1.42 → experienced extraction is maximum. Federal administrators (beneficiary + arbitrage) derive d ≈ 0.05 (they benefit and can shift enforcement priorities) → f(d) ≈ -0.12 → experienced extraction is negative (institutional gain). Poor white citizens (victim of lost preference + constrained exit) derive d ≈ 0.55 (they bear some suppression but have relocation/litigation options) → f(d) ≈ 0.75 → moderate experienced extraction. Analytical observer (neither beneficiary nor victim, observational position) derives d ≈ 0.72 → f(d) ≈ 1.15 → analytical extraction value (sees the structure without experiencing its suppression).
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint partially resolves mandatrophy through temporal measurement: base_extractiveness declines from 0.72 (pre-elimination, maximum extraction via disenfranchisement) to 0.38 (current, residual extraction via federal suppression of alternatives). Theater_ratio declines from 0.88 (literacy test cover story) to 0.35 (federal uniform standard removes theatrical cover). The constraint is not a resolution of mandatrophy but a transformation of extraction: from discriminatory discretion (high theater, massive extraction) to federal uniformity (low theater, moderate extraction). The remaining extractiveness reflects that federal enforcement itself requires suppression of alternatives — there is no exit from federal jurisdiction, no way to restore local control without restoring discrimination. The tangled-rope classification reflects this: genuine coordination on voting access exists alongside genuine extraction of local control. The constraint cannot be pure rope (coordination without extraction) because removing one suppression mechanism (registrar discretion) requires imposing another (federal enforcement). The mandatrophy is resolved not by finding pure coordination but by recognizing that this constraint will always be tangled — it corrects historical extraction while imposing new suppression, and both aspects are structurally necessary.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    literacy_test_necessity_contested,
    'Were literacy tests a genuine qualification for informed voting or purely pretextual tools for selective exclusion?',
    'Historical analysis of pass rates and essay evaluations for white vs. Black applicants using identical literacy assessments; administrative records showing registrar discretion; comparative literacy rates in voting-eligible populations',
    'If purely pretextual: constraint is unambiguously corrective and beneficiary-protective (snare elimination). If partly justified: constraint imposes uniform standards that prevent abuse but eliminate potential legitimate qualification gate (snare/rope boundary becomes ambiguous).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(literacy_test_necessity_contested, empirical, 'Whether literacy tests were pretextual or had genuine qualification purpose').

omega_variable(
    voter_registration_outcome_asymmetry,
    'Did federal uniform standards actually produce equivalent registration rates across racial groups, or did registrars develop new pretextual barriers to preserve de facto discrimination?',
    'Longitudinal voter registration data by race and county pre/post-federal standardization; analysis of alternative barriers (address verification, form rejection, citizenship challenges) that emerged post-1965; inference from registration rates whether suppression mechanism merely shifted',
    'If federal standard succeeded: constraint is rope (genuine coordination on registration access). If registrars adapted by shifting barriers: constraint is scaffolding a temporary fix, not eliminating suppression (scaffold rather than rope). If both: tangled rope with residual suppression mechanism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(voter_registration_outcome_asymmetry, empirical, 'Whether federal standards eliminated or merely redirected discriminatory registration barriers').

omega_variable(
    federal_enforcement_necessity_duration,
    'How long must federal enforcement (federal examiners, Justice Department review) persist to guarantee voting rights, or is it an indefinite requirement indicating the constraint cannot self-stabilize?',
    'Timeline of federal examiner deployment in covered jurisdictions; analysis of voting rights violations in jurisdictions where federal oversight was withdrawn; interstate comparative analysis of voting discrimination post-oversight removal',
    'If enforcement can be removed after internalization: constraint is rope with a temporary enforcement phase. If violations resurface immediately upon removal: constraint is snare requiring indefinite coercion (federal enforcement IS the suppression mechanism preventing backsliding).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(federal_enforcement_necessity_duration, empirical, 'Whether federal enforcement can eventually be withdrawn or must be indefinite').

omega_variable(
    local_voter_qualification_vs_federal_uniformity,
    'Is legitimate democratic governance better served by federal uniformity (prevents discrimination but removes local control) or local flexibility (enables community voice but permits discrimination)?',
    'Comparative analysis of voting system design in federal vs. unitary democracies; case studies of federal standards backsliding or local control re-emergence; normative democratic theory literature on federalism vs. universal rights',
    'If federal uniformity is normatively superior: constraint converges on natural law (mountain). If legitimate value in local control: constraint is forcible equilibrium (tangled rope or snare, depending on whose values are suppressed).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(local_voter_qualification_vs_federal_uniformity, preference, 'Normative tension between federal uniformity and local democratic control').

omega_variable(
    true_neutral_standard_impossibility,
    'Can any voter qualification standard (even ''objective'' ones) be administered free from discriminatory intent, or does the power to qualify voters inherently enable discrimination?',
    'Analysis of supposedly neutral standards (voter ID, address verification, citizenship documentation) and their disparate impact across demographic groups; documentation of registrar discretion in applying neutral criteria; philosophical analysis of administrative discretion under uncertainty',
    'If neutral standard is possible: constraint can eventually become rope (pure coordination). If discrimination inheres in any qualification gate: constraint can only suppress alternatives, never eliminate discrimination mechanism (permanent snare).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(true_neutral_standard_impossibility, conceptual, 'Whether truly neutral voter qualification standards can be administered without discrimination').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sotu_1965_johnson_uniform_voting_standard_elimination, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(uvse_theater_t0_preelimination, sotu_1965_johnson_uniform_voting_standard_elimination, theater_ratio, 0, 0.88).
narrative_ontology:measurement(uvse_theater_t5_immediate_enforcement, sotu_1965_johnson_uniform_voting_standard_elimination, theater_ratio, 5, 0.55).
narrative_ontology:measurement(uvse_theater_t10_current, sotu_1965_johnson_uniform_voting_standard_elimination, theater_ratio, 10, 0.35).

% Extraction over time
narrative_ontology:measurement(uvse_extractiveness_t0_preelimination, sotu_1965_johnson_uniform_voting_standard_elimination, base_extractiveness, 0, 0.72).
narrative_ontology:measurement(uvse_extractiveness_t5_immediate_enforcement, sotu_1965_johnson_uniform_voting_standard_elimination, base_extractiveness, 5, 0.52).
narrative_ontology:measurement(uvse_extractiveness_t10_current, sotu_1965_johnson_uniform_voting_standard_elimination, base_extractiveness, 10, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sotu_1965_johnson_uniform_voting_standard_elimination, enforcement_mechanism).
narrative_ontology:affects_constraint(sotu_1965_johnson_uniform_voting_standard_elimination, voter_id_requirements_disparate_impact).
narrative_ontology:affects_constraint(sotu_1965_johnson_uniform_voting_standard_elimination, federal_preclearance_jurisdiction_boundaries).

% DUAL FORMULATION NOTE:
% This constraint is upstream of voter ID and registration barriers that emerged post-1965 as registrars adapted to circumvent federal uniformity. The uniform voting standard story should be read alongside stories documenting how supposedly neutral requirements (voter ID, address verification, citizenship documentation) replicate the discriminatory mechanism through different tools.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(sotu_1965_johnson_uniform_voting_standard_elimination, institutional, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
