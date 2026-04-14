% ============================================================================
% CONSTRAINT STORY: voting_system_exclusion
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_voting_system_exclusion, []).

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
 *   constraint_id: voting_system_exclusion
 *   human_readable: Voting System Exclusion Mechanisms
 *   domain: political/electoral
 *
 * SUMMARY:
 *   Voting system exclusion mechanisms operate through layered barriers that
 *   prevent segments of the population from exercising electoral
 *   participation rights. These include residency requirements, citizenship
 *   documentation, felony disenfranchisement, registration deadlines, voter
 *   ID requirements, polling place accessibility, and language barriers. The
 *   constraint exhibits strong extraction from the perspective of excluded
 *   populations and high suppression through legal and administrative
 *   barriers. The primary beneficiary is the incumbent political coalition
 *   whose electoral outcomes are secured by selective participation. The
 *   theater ratio reflects that much of the justification language (fraud
 *   prevention, administrative integrity, residency verification) provides
 *   legitimation for mechanisms whose primary function is exclusion-based
 *   power consolidation. Extractiveness has increased over the measured
 *   interval as mechanisms have become more sophisticated and their
 *   application more targeted.
 *
 * KEY AGENTS:
 *   - Excluded Voter Populations: Primary victims (powerless/trapped) — bear full cost of participation barriers; inability to exit through individual action
 *   - Incumbent Political Interests: Primary beneficiary (institutional/arbitrage) — maintain power through reduced opposition turnout and predictable electorate
 *   - Electoral Administration System: Institutional actor (institutional/arbitrage) — maintains and enforces exclusion mechanisms; benefits from reduced administrative complexity
 *   - Voting Rights Coalition: Secondary organized actor (organized/constrained) — mobilizes against exclusion but constrained by resource limitations and institutional resistance
 *   - Electoral Legitimacy: Abstract victim (powerless/trapped) — reduced by systematic exclusion; cannot organize or exit
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(voting_system_exclusion, 0.68).
domain_priors:suppression_score(voting_system_exclusion, 0.72).
domain_priors:theater_ratio(voting_system_exclusion, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(voting_system_exclusion, extractiveness, 0.68).
narrative_ontology:constraint_metric(voting_system_exclusion, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(voting_system_exclusion, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(voting_system_exclusion, snare).
narrative_ontology:human_readable(voting_system_exclusion, "Voting System Exclusion Mechanisms").
narrative_ontology:topic_domain(voting_system_exclusion, "political/electoral").

domain_priors:requires_active_enforcement(voting_system_exclusion).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(voting_system_exclusion, incumbent_political_interests).
narrative_ontology:constraint_victim(voting_system_exclusion, excluded_voter_populations).
narrative_ontology:constraint_victim(voting_system_exclusion, electoral_legitimacy).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EXCLUDED VOTER (SNARE) — Trapped by legal barriers (residency requirements, citizenship restrictions, felony disenfranchisement, registration deadlines). Cannot exit without physical relocation, legal status change, or restoration of rights. Experiences maximum extraction: participation rights are conditionally withheld, and suppression mechanisms (documentation requirements, polling place accessibility, language barriers) ensure continued exclusion even when technical eligibility exists.
constraint_indexing:constraint_classification(voting_system_exclusion, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: DISENFRANCHISED COMMUNITY (SNARE) — Faces systemic barriers across generations: intergenerational poverty traps, educational access constraints, geographic isolation from voting locations. While some individuals can overcome individual barriers through sustained effort, the community as a whole cannot exit the exclusionary structure without coalitional action and resources. Experiences high extraction: political power is systematically unrepresented, policy preferences are ignored, and the exclusion mechanism reproduces across generations.
constraint_indexing:constraint_classification(voting_system_exclusion, snare,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: INCUMBENT POWER COALITION (ROPE) — Benefits from exclusion through reduced mobilization costs and predictable electoral outcomes. Experiences the constraint as coordination: exclusion mechanisms select the voter pool that will support existing power structures. The constraint solves a coordination problem for incumbents (maintaining power without policy responsiveness). Net beneficiary — extraction runs toward this agent.
constraint_indexing:constraint_classification(voting_system_exclusion, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: VOTING RIGHTS COALITION (TANGLED ROPE) — Organized agents (civil rights groups, voter registration drives, legal advocates) experience both coordination and extraction. The constraint coordinates their mobilization (voting rights campaigns focus on overcoming exclusion mechanisms) while extracting through persistent barriers that resist removal. The coalition benefits from heightened organization and donor support during enfranchisement campaigns, but the constraint persists because exclusion mechanisms are actively maintained by incumbent interests. Low effective extraction experienced by the coalition because they have resources and agency, but high extraction imposed on victim populations they represent.
constraint_indexing:constraint_classification(voting_system_exclusion, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: ELECTORAL ADMINISTRATION SYSTEM (PITON) — Maintenance of voting eligibility rules (residency, citizenship, registration deadlines, felony restrictions) is largely performative theater. The stated justification (preventing fraud, ensuring residency, maintaining integrity) masks extraction mechanisms that disproportionately remove poor, mobile, minority, and formerly incarcerated populations from voting rolls. The administrative system sees its own processes as degraded — exclusion persists through institutional inertia and path dependence rather than active design. Piton classification derives from theater ratio (0.55): significant portion of administrative effort is performative legitimation of exclusionary outcomes rather than functional anti-fraud mechanisms.
constraint_indexing:constraint_classification(voting_system_exclusion, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — From a civilizational/global perspective, voting exclusion mechanisms coordinate state capacity (verification of voter eligibility, prevention of multiple voting) while extracting through asymmetric application favoring incumbent-aligned populations. Comparative analysis reveals that exclusion mechanisms with identical stated justifications produce dramatically different exclusion rates across jurisdictions (felony disenfranchisement affects 5.2% of US population but up to 21% of Black male population; voter ID requirements exclude 11% of citizens nationwide but 25% of Black citizens). This perspectival gap reveals extraction disguised as coordination.
constraint_indexing:constraint_classification(voting_system_exclusion, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(voting_system_exclusion_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(voting_system_exclusion, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(voting_system_exclusion, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(voting_system_exclusion, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(voting_system_exclusion, TR),
    TR >= 0.70.

:- end_tests(voting_system_exclusion_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. Exclusion mechanisms directly transfer political power from targeted populations to incumbent beneficiaries. The extraction is not merely the opportunity cost of non-participation but the active suppression of competitive electoral dynamics. The measured increase from 0.45 to 0.68 over 40 years reflects cumulative mechanism sophistication and application intensity. Suppression (0.72): High. Multiple overlapping barriers operate simultaneously: legal eligibility requirements, administrative registration barriers, physical accessibility constraints, and information asymmetries. Suppression mechanisms are structurally embedded (residency requirements, citizenship documentation cannot be overcome by individual effort alone) and deliberately maintained (states actively resist federal voting rights enforcement). Theater ratio (0.55): Moderate-high. Approximately 55% of the institutional discourse around voting eligibility is performative: fraud prevention language masks exclusionary function; administrative integrity justifications obscure selective application; integrity of elections framing legitimates mechanisms whose primary effect is demographic targeting. Real fraud rates are negligible (documented at <0.0001% in most jurisdictions) while exclusion rates reach 5-21% depending on demographic group. The theater has increased as mechanisms have become more technically sophisticated, allowing greater precision in demographic targeting while maintaining plausible denial of intentionality.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates dramatic perspectival divergence. Incumbent beneficiaries experience coordination (rope perspective): the mechanism solves the problem of maintaining power without policy responsiveness. Electoral administrators experience routine administration (piton perspective): the rules persist through path dependence and institutional inertia. The Voting Rights Coalition sees mixed dynamics (tangled rope perspective): genuine coordination of voting rights infrastructure alongside persistent extraction barriers. Excluded populations see pure extraction (snare perspective): participation rights are systematically withheld with no benefit and no exit option. The analytical observer sees the false naturalization: voting exclusion is sometimes presented as inherent to elections administration or as necessary to prevent fraud, but comparative and historical analysis reveals these mechanisms as contingent policy choices with demonstrably lower-cost alternatives that would achieve stated goals (fraud prevention, administrative integrity) without demographic targeting.
 *
 * DIRECTIONALITY LOGIC:
 *   Excluded populations experience maximum extraction through trapped exit options: they cannot relocate to different jurisdictions without abandoning their social/economic bases; cannot change citizenship status through individual action; cannot restore felony disenfranchisement without navigating opaque restoration processes. Incumbent beneficiaries experience extraction as coordination: selectivity of the voter pool solves the power consolidation problem, and the mechanism requires minimal enforcement cost because it is embedded in routine administrative processes. The Voting Rights Coalition occupies an intermediate position: constrained (they can mobilize and seek legal remedies) but facing systematic institutional resistance that prevents escape. The Electoral Administration System benefits from arbitrage: it can enforce rules selectively while claiming neutrality. The Analytical Observer sees the perspectival gap: coordination language for beneficiaries vs extraction reality for victims reveals the constraint as pure extraction (snare) rather than coordination.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by demonstrating that voting exclusion is structurally a snare (extraction without coordination benefit) rather than rope (coordination with minimal extraction). The mandatrophy resolution shows: (1) exclusion mechanisms do not solve a genuine coordination problem — they solve the incumbent power consolidation problem; (2) alternative mechanisms exist that would achieve fraud prevention and administrative integrity with 95%+ lower exclusion rates; (3) the persistence of exclusion mechanisms despite available alternatives indicates extraction rather than necessary coordination; (4) the discrepancy between stated justification (fraud prevention at rates <0.0001%) and scale of exclusion (5-21% of target populations) reveals the theater ratio as legitimation of extraction rather than functional necessity. The classification as snare (not tangled rope) follows from the absence of genuine coordination benefit: excluded populations receive zero benefit from the mechanism, and the mechanism's primary function is not solving a shared coordination problem but redistributing political power.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    fraud_prevention_vs_access_tradeoff,
    'Do documented voter fraud rates justify the exclusion mechanisms employed, or do exclusion rates far exceed fraud rates?',
    'Empirical comparison: documented voter fraud cases vs persons disenfranchised by each mechanism. Analysis of fraud types actually prevented vs theoretical fraud prevented.',
    'If fraud justifies exclusion: mechanisms may be necessary coordination cost (Rope). If fraud rates are negligible while exclusion is systematic: mechanisms are extraction disguised as coordination (Snare).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(fraud_prevention_vs_access_tradeoff, empirical, 'Whether voter fraud rates justify exclusion mechanism scale').

omega_variable(
    intentionality_vs_disparate_impact,
    'Are exclusion mechanisms intentionally designed to suppress specific populations, or do they produce disparate impact through neutral application?',
    'Historical documentation of mechanism design and intent; comparison of exclusion outcomes across demographic groups; analysis of policy alternatives that would achieve stated goals with lower exclusion.',
    'If intentional: extraction is deliberate (Snare confirmed). If neutral with disparate impact: classification may shift to Tangled Rope if incumbents genuinely perceive mechanisms as coordination rather than extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intentionality_vs_disparate_impact, empirical, 'Whether exclusion is intentional design or disparate impact').

omega_variable(
    remediation_cost_allocation,
    'Who bears the cost of remediation if exclusion mechanisms produce errors (e.g., eligible voters blocked by faulty registration databases)?',
    'Analysis of restoration procedures: administrative burden on individual voter vs institutional cost to election administrators. Tracking of wrongly excluded voters and time to restoration.',
    'If remediation burden falls on excluded voters: suppression mechanism confirmed (high chi). If institutional burden: extraction cost is partially internalized by beneficiary, reducing net chi.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(remediation_cost_allocation, empirical, 'Allocation of remediation costs for exclusion mechanism errors').

omega_variable(
    temporal_exclusion_lock_in,
    'Do exclusion mechanisms create temporal lock-in: once excluded, are voters permanently removed from rolls or re-blocked at each election cycle?',
    'Longitudinal tracking of excluded voter registration status across 2-4 election cycles; analysis of automatic re-registration mechanisms.',
    'If permanent removal: suppression is high (trapped gate confirmed). If re-blocking each cycle: suppression is sustained through ongoing mechanism operation (snare confirmed). If automatic re-registration exists: suppression may be lower than measured.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(temporal_exclusion_lock_in, empirical, 'Whether exclusion is one-time or recurring mechanism').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(voting_system_exclusion, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(voteexcl_tr_t0, voting_system_exclusion, theater_ratio, 0, 0.38).
narrative_ontology:measurement(voteexcl_tr_t20, voting_system_exclusion, theater_ratio, 20, 0.48).
narrative_ontology:measurement(voteexcl_tr_t40, voting_system_exclusion, theater_ratio, 40, 0.55).
narrative_ontology:measurement(voteexcl_tr_t10, voting_system_exclusion, theater_ratio, 10, 0.42).

% Extraction over time
narrative_ontology:measurement(voteexcl_be_t0, voting_system_exclusion, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(voteexcl_be_t20, voting_system_exclusion, base_extractiveness, 20, 0.58).
narrative_ontology:measurement(voteexcl_be_t40, voting_system_exclusion, base_extractiveness, 40, 0.68).
narrative_ontology:measurement(voteexcl_be_t10, voting_system_exclusion, base_extractiveness, 10, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(voting_system_exclusion, enforcement_mechanism).
narrative_ontology:affects_constraint(voting_system_exclusion, electoral_representation_disparity).
narrative_ontology:affects_constraint(voting_system_exclusion, ballot_access_restrictions).

% DUAL FORMULATION NOTE:
% Voting system exclusion operates through distinct mechanisms with different ε values. Registration-based exclusion (ε≈0.55) and felony disenfranchisement (ε≈0.72) are separate constraints sharing common beneficiaries but operating through different suppression channels. They are linked here as components of a constraint family but could be decomposed into separate stories if mechanism-specific analysis is required.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(voting_system_exclusion, institutional, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
