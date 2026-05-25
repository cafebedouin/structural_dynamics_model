% ============================================================================
% CONSTRAINT STORY: electoral_system_fragmentation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_electoral_system_fragmentation, []).

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
 *   constraint_id: electoral_system_fragmentation
 *   human_readable: Electoral System Fragmentation
 *   domain: political/governance
 *
 * SUMMARY:
 *   Electoral system fragmentation creates a structural constraint where the
 *   organizational incentives of establishment parties, the technical
 *   complexity of ballot access and candidate qualification, and the
 *   mathematical properties of voting systems interact to generate barriers
 *   against new political entrants and minority representation. The
 *   constraint exhibits all major DR types from different structural
 *   positions, making it a diagnostic case for institutional extraction.
 *   Minor parties face trapped positions with wasted votes and high
 *   compliance costs. Voters experience constrained choice and strategic
 *   voting pressure. Establishment parties benefit from coordination without
 *   internal discipline. Electoral administrators maintain complex regulatory
 *   apparatus through inertia. Reform movements see a solvable problem with
 *   clear sunset mechanisms. From a civilizational perspective, the
 *   constraint appears as natural law (Duverger's Law), but international
 *   evidence contradicts this — countries with similar electoral mechanics
 *   have maintained viable multi-party systems through institutional design
 *   choices. The constraint's theater_ratio (0.68) reflects that much of
 *   electoral administration consists of procedural legitimacy maintenance
 *   rather than achieving stated coordination goals. The extractiveness
 *   trajectory (0.42 → 0.58) documents the progressive tightening of
 *   fragmentation barriers as establishment parties adapt to challenge.
 *
 * KEY AGENTS:
 *   - Minor Party Activists: Primary victims (powerless/trapped) — face ballot access barriers, debate thresholds, funding disadvantages, institutional lock-in
 *   - Voters at Coalition Boundaries: Mixed agents (moderate/constrained) — experience coordination benefits alongside agency constraint through strategic voting pressure
 *   - Establishment Parties: Primary beneficiaries (institutional/arbitrage) — benefit from vote concentration, reduced coalition discipline burden, favorable rule interpretation
 *   - Electoral Reform Coalition: Organized advocates (organized/constrained) — see fragmentation as solvable problem but constrained by establishment party control of electoral rules
 *   - Electoral Administration Apparatus: Institutional actor (institutional/arbitrage) — maintains fragmentation through regulatory complexity maintained by procedural inertia
 *   - Proportional Representation Movement: Reform actors (organized/mobile) — have achieved state-level victories, possess clear alternative design, building path to sunset
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing contingent institutional choices as mechanical inevitability
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(electoral_system_fragmentation, 0.58).
domain_priors:suppression_score(electoral_system_fragmentation, 0.62).
domain_priors:theater_ratio(electoral_system_fragmentation, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(electoral_system_fragmentation, extractiveness, 0.58).
narrative_ontology:constraint_metric(electoral_system_fragmentation, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(electoral_system_fragmentation, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(electoral_system_fragmentation, tangled_rope).
narrative_ontology:human_readable(electoral_system_fragmentation, "Electoral System Fragmentation").
narrative_ontology:topic_domain(electoral_system_fragmentation, "political/governance").

domain_priors:requires_active_enforcement(electoral_system_fragmentation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(electoral_system_fragmentation, establishment_parties).
narrative_ontology:constraint_beneficiary(electoral_system_fragmentation, electoral_administrators).
narrative_ontology:constraint_victim(electoral_system_fragmentation, minor_parties).
narrative_ontology:constraint_victim(electoral_system_fragmentation, voter_participation_quality).
narrative_ontology:constraint_victim(electoral_system_fragmentation, democratic_representation).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: MINOR PARTY ACTIVIST (SNARE) — Trapped by structural barriers: ballot access requirements, debate thresholds, winner-take-all mechanics, and funding disadvantages. Cannot exit the political system without abandoning democratic participation entirely. Bears full cost of fragmentation: votes wasted, resources diverted to compliance, organizational energy consumed by institutional navigation.
constraint_indexing:constraint_classification(electoral_system_fragmentation, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: VOTER AT SYSTEM BOUNDARIES (TANGLED ROPE) — Constrained by coordination failure: electoral fragmentation genuinely solves coalition-building problems for parties (they don't need internal agreement on all issues), but this same fragmentation locks voters into two-party coalition choices or strategic voting. Constrained exit: can abstain (high political cost) or switch parties (identity/alignment cost). Mixed experience: legitimate coordination function alongside extraction of voter agency.
constraint_indexing:constraint_classification(electoral_system_fragmentation, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: ESTABLISHMENT PARTY (ROPE) — Benefits from fragmentation through reduced coalition discipline burden and vote concentration from third-party spoilage effects. Experiences the constraint as coordination: fragmentation allows the party to maintain internal heterogeneity without formal schism. Net beneficiary with arbitrage: can influence electoral rules, field candidates strategically, leverage complexity for advantage.
constraint_indexing:constraint_classification(electoral_system_fragmentation, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: ELECTORAL REFORM COALITION (TANGLED ROPE) — Organized agents (NGOs, civic groups, election scientists) see fragmentation as a solvable coordination problem (ranked choice voting, proportional representation, open primaries). They benefit from fragmentation as a legitimating crisis that justifies their intervention, but are constrained by establishment party control of rule-making. Active enforcement required to maintain fragmentation against reform pressure.
constraint_indexing:constraint_classification(electoral_system_fragmentation, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: ELECTORAL ADMINISTRATION APPARATUS (PITON) — Maintains complex ballot access requirements, counting procedures, and regulation enforcement that sustain fragmentation through inertia. The apparatus treats this complexity as inherent to electoral legitimacy (theater: 'we must verify all candidates, follow all procedures'), but the primary function has atrophied — fragmentation persists because the regulatory structure is locked in, not because the rules serve their stated purpose. Theater ratio high: procedures justify themselves rather than achieving stated coordination goals.
constraint_indexing:constraint_classification(electoral_system_fragmentation, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: PROPORTIONAL REPRESENTATION MOVEMENT (SCAFFOLD) — Sees fragmentation as temporary institutional failure with clear sunset mechanism: adoption of proportional representation, ranked choice voting, or open primary systems. Movement has real agency (state-level electoral reforms occurring), clear exit path (policy change), and genuine benefit to broader participation. Low theater because the proposed alternatives are substantive, not performative. Sunset is estimated 15-25 years in North American context as norms shift toward representation equity.
constraint_indexing:constraint_classification(electoral_system_fragmentation, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / DUVERGER'S LAW (MOUNTAIN) — From a civilizational/universal perspective, fragmentation appears as immutable natural law: Duverger's Law predicts that first-past-the-post electoral systems inevitably collapse toward two-party equilibrium, and this collapse mechanism cannot be escaped by design choice alone. However, structural data contradicts the mountain classification: countries with FPTP systems have maintained multi-party competition through institutional arrangements (primary systems, candidate discipline rules, coalition incentives). The apparent natural law conceals contingent institutional choices.
constraint_indexing:constraint_classification(electoral_system_fragmentation, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(electoral_system_fragmentation_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(electoral_system_fragmentation, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(electoral_system_fragmentation, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(electoral_system_fragmentation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(electoral_system_fragmentation, TR),
    TR >= 0.70.

:- end_tests(electoral_system_fragmentation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Elevated. The primary extraction mechanism is the concentration of political voice: establishment parties capture disproportionate representation through vote multiplication effects and ballot access barriers, while minority viewpoints face suppression through strategic voting incentives and low-threshold exclusion. The extraction is not maximal (snare territory ≥ 0.66) because genuine coordination functions exist — the two-party structure does solve coalition-building problems. The intermediate value reflects that extraction occurs within a functional coordination system. Suppression (0.62): Moderate-high. Barriers to minor party entry and voter preference expression are substantial: ballot access requirements, debate participation thresholds, winner-take-all mechanics, and psychological strategic voting costs. However, suppression is not total — minor parties do emerge, voters do participate, and reform has occurred in some jurisdictions. The value reflects significant structural barriers without complete entrenchment. Theater ratio (0.68): Moderate-high. Electoral administration consists substantially of procedural legitimacy maintenance: complex ballot access procedures, voter verification processes, and vote counting protocols justify themselves as necessary for electoral integrity, but much of this complexity persists through institutional inertia rather than necessity. Jurisdictions with simpler procedures (open primaries, proportional systems) report comparable or superior integrity without the procedural theater. Theater has increased from 0.52 to 0.68 as the system has become more administratively complex while serving the same coordination goals.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates sharp perspectival divergence. Establishment parties (Rope) perceive fragmentation as legitimate coordination mechanism solving coalition discipline problems. Voters (Tangled Rope) experience both coordination benefit (forced coalition formation) and extraction (constrained choice, strategic voting). Minor party activists (Snare) experience pure extraction and institutional lock-in. The reform movement (Scaffold) sees a temporary problem with clear policy solutions and momentum. Electoral administration (Piton) sees its own degraded procedures as inherent necessity. The natural law perspective (Mountain) risks naturalizing contingent institutional choices as mechanical inevitability. The largest gap: between the establishment party's experience (Rope — low extraction) and the minor party activist's experience (Snare — high extraction) emerges from the same constraint, revealing that fragmentation extracts asymmetrically based on institutional position. This gap is the central diagnostic signal: when the same constraint generates snare vs rope divergence, the extraction is systemic.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values flow from structural position relative to the extraction mechanism. Establishment parties with arbitrage exit options experience low or negative directionality (d ≈ 0.10-0.20) — they can manipulate ballot access rules, interpret regulations favorably, and leverage fragmentation strategically. Voters face constrained exit (switching parties, strategic voting, abstention all carry costs), resulting in moderate directionality (d ≈ 0.55-0.70). Minor party activists face trapped exit (abandoning democratic participation entirely), resulting in high directionality (d ≈ 0.90-0.95). Electoral administrators have institutional arbitrage options (procedure modification, rule interpretation), resulting in low directionality (d ≈ 0.15-0.25). The reform coalition has mobile exit (can exit through policy change implementation), resulting in moderate directionality (d ≈ 0.45-0.60). These derived d values feed the sigmoid function to produce experienced chi: low d beneficiaries see low chi, high d victims see high chi, all from the same constraint.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    duvergers_law_mechanism,
    'Is Duverger''s Law a mechanical necessity of first-past-the-post systems or a contingent outcome dependent on specific institutional configurations?',
    'Comparative analysis of FPTP democracies with varying party system outcomes (Canada multi-party vs US two-party); investigation of institutional variables that permit multi-party persistence despite FPTP incentives',
    'If mechanical necessity: constraint approaches mountain (ε → 0.15). If contingent: constraint remains tangled_rope (ε ≈ 0.55-0.65) with identifiable institutional targets for reform.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(duvergers_law_mechanism, empirical, 'Whether Duverger''s Law is mechanical or contingent').

omega_variable(
    strategic_voting_prevalence,
    'What proportion of voters engage in strategic voting rather than sincere preference voting, and how does this vary with system fragmentation level?',
    'Survey data on voter intent vs reported vote; exit polling on candidate preference vs ballot choice; correlational analysis with fragmentation metrics across electoral cycles',
    'If strategic voting > 40%: suppression metric increases (voter agency constraint is severe). If strategic voting < 20%: suppression may be overstated, constraint shifts toward rope (coordination without coercion).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(strategic_voting_prevalence, empirical, 'Prevalence of strategic voting behavior').

omega_variable(
    reform_adoption_trajectory,
    'Are electoral reforms (ranked choice, proportional representation) actually being adopted and implemented, and do they substantially reduce fragmentation effects?',
    'Tracking of ballot initiatives and legislative votes on electoral reform; pre/post analysis of jurisdictions adopting alternatives (Maine, Alaska, select international cases); measurement of voter participation and representation quality changes',
    'If adoption accelerating: scaffold perspective confirmed, sunset is real (constraint may degrade to rope or dissolution within 20 years). If adoption stalled: scaffold is aspirational, constraint may intensify toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reform_adoption_trajectory, empirical, 'Electoral reform adoption and effectiveness').

omega_variable(
    voter_disengagement_causality,
    'Does electoral fragmentation cause voter disengagement and declining participation, or does participation decline cause fragmentation to become more visible as a problem?',
    'Time series analysis of voter participation rates vs fragmentation indices; causal inference from natural experiments (electoral reform implementation); cross-national comparison of participation trends in proportional vs FPTP systems',
    'If fragmentation causes disengagement: suppression metric is justified (ε ≥ 0.58). If causality reversed: suppression may reflect manifestation of prior disengagement, constraint may be secondary to other political constraints.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(voter_disengagement_causality, empirical, 'Causal direction between fragmentation and participation').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(electoral_system_fragmentation, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(esf_tr_t0, electoral_system_fragmentation, theater_ratio, 0, 0.52).
narrative_ontology:measurement(esf_tr_t5, electoral_system_fragmentation, theater_ratio, 5, 0.62).
narrative_ontology:measurement(esf_tr_t10, electoral_system_fragmentation, theater_ratio, 10, 0.68).

% Extraction over time
narrative_ontology:measurement(esf_be_t0, electoral_system_fragmentation, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(esf_be_t5, electoral_system_fragmentation, base_extractiveness, 5, 0.52).
narrative_ontology:measurement(esf_be_t10, electoral_system_fragmentation, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(electoral_system_fragmentation, enforcement_mechanism).
narrative_ontology:affects_constraint(electoral_system_fragmentation, political_polarization).
narrative_ontology:affects_constraint(electoral_system_fragmentation, voter_participation_decline).
narrative_ontology:affects_constraint(electoral_system_fragmentation, campaign_finance_concentration).

% DUAL FORMULATION NOTE:
% Electoral system fragmentation is upstream of several political constraints: polarization intensifies as parties must appeal to narrower coalitions, participation declines as strategic voting suppresses sincere preferences, and campaign finance concentrates as minor parties require disproportionate resources for ballot access. Each downstream constraint has its own ε value; network edges establish causal dependency.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(electoral_system_fragmentation, institutional, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
