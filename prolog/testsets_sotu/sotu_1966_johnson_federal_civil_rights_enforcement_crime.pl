% ============================================================================
% CONSTRAINT STORY: sotu_1966_johnson_federal_civil_rights_enforcement_crime
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sotu_1966_johnson_federal_civil_rights_enforcement_crime, []).

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
 *   constraint_id: sotu_1966_johnson_federal_civil_rights_enforcement_crime
 *   human_readable: Federal Criminal Jurisdiction Over Civil Rights Obstruction (1966 Johnson Administration)
 *   domain: social_policy/federal_enforcement
 *
 * SUMMARY:
 *   The 1966 Johnson administration proposal to establish federal criminal
 *   jurisdiction over civil rights obstruction, enforce nondiscrimination in
 *   jury selection, and outlaw housing discrimination creates a structural
 *   shift in the enforcement apparatus from fragmented state and local
 *   jurisdictions to centralized federal authority. The constraint exhibits
 *   the classic tangled_rope structure: genuine coordination benefit (unified
 *   enforcement standards, elimination of state-level fragmentation) combined
 *   with asymmetric extraction (federal authority override of state autonomy,
 *   criminal liability imposed on discriminatory actors without their
 *   consent, expansion of federal police power). The base extractiveness
 *   value (0.52) reflects that the statute imposes significant criminal
 *   exposure on discriminatory private entities and state-level resistance
 *   systems, but the coordination function is genuine — the prior system of
 *   fragmented enforcement allowed systematic evasion of nondiscrimination
 *   obligations. Theater ratio declines over the measurement interval (0.42 →
 *   0.35) because federal enforcement mechanisms, while imperfect, involve
 *   actual investigation and prosecution rather than purely performative
 *   review. The constraint benefits racial minorities seeking equal access
 *   through criminal enforcement authority while imposing extraction costs on
 *   discriminatory entities and federalism-protective state systems.
 *
 * KEY AGENTS:
 *   - Racial Minorities Seeking Access: Primary beneficiary (powerless/trapped) — gain nominal federal enforcement rights but face persistent discrimination through proxy mechanisms and bureaucratic burden
 *   - Federal Department of Justice: Primary beneficiary and enforcer (institutional/arbitrage) — gains jurisdiction, budget, and institutional authority
 *   - Discriminatory Private Entities: Primary victim (powerful/constrained) — face criminal liability for discrimination; lose discretionary denial ability
 *   - State Systems Resisting Integration: Secondary victim (institutional/constrained) — lose autonomy and face federal criminal enforcement; maintain performance of compliance
 *   - Civil Rights Coalition: Organized actors (organized/constrained) — see statute as temporary enforcement tool with sunset as integration norms solidify
 *   - State-Level Enforcers: Secondary institutional actor (moderate/constrained) — coordinate with federal authority while losing exclusive jurisdictional control
 *   - Federalism Principle: Abstract structural victim (analytical/analytical) — abstraction that loses institutional protection in this constraint
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sotu_1966_johnson_federal_civil_rights_enforcement_crime, 0.52).
domain_priors:suppression_score(sotu_1966_johnson_federal_civil_rights_enforcement_crime, 0.68).
domain_priors:theater_ratio(sotu_1966_johnson_federal_civil_rights_enforcement_crime, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sotu_1966_johnson_federal_civil_rights_enforcement_crime, extractiveness, 0.52).
narrative_ontology:constraint_metric(sotu_1966_johnson_federal_civil_rights_enforcement_crime, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(sotu_1966_johnson_federal_civil_rights_enforcement_crime, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sotu_1966_johnson_federal_civil_rights_enforcement_crime, tangled_rope).
narrative_ontology:human_readable(sotu_1966_johnson_federal_civil_rights_enforcement_crime, "Federal Criminal Jurisdiction Over Civil Rights Obstruction (1966 Johnson Administration)").
narrative_ontology:topic_domain(sotu_1966_johnson_federal_civil_rights_enforcement_crime, "social_policy/federal_enforcement").

domain_priors:requires_active_enforcement(sotu_1966_johnson_federal_civil_rights_enforcement_crime).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sotu_1966_johnson_federal_civil_rights_enforcement_crime, racial_minorities_seeking_equal_access).
narrative_ontology:constraint_beneficiary(sotu_1966_johnson_federal_civil_rights_enforcement_crime, federal_enforcement_apparatus).
narrative_ontology:constraint_victim(sotu_1966_johnson_federal_civil_rights_enforcement_crime, discriminatory_private_entities).
narrative_ontology:constraint_victim(sotu_1966_johnson_federal_civil_rights_enforcement_crime, state_systems_resisting_integration).
narrative_ontology:constraint_victim(sotu_1966_johnson_federal_civil_rights_enforcement_crime, federalism_principle).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DISCRIMINATED APPLICANT (SNARE) — African Americans seeking housing, jury participation, or access to services face total suppression from state and local enforcement mechanisms. The federal crime statute creates a new exit path but does not remove the material barriers: housing discrimination persists through proxy mechanisms (redlining, racial covenants), jury selection through peremptory challenges and structural bias, public accommodation discrimination through creative loopholes. The applicant is trapped between continuing discrimination and the bureaucratic burden of federal complaint. Federal enforcement creates nominal rights but does not eliminate the extraction mechanism (discriminatory denial of access).
constraint_indexing:constraint_classification(sotu_1966_johnson_federal_civil_rights_enforcement_crime, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: STATE-LEVEL ENFORCER (TANGLED ROPE) — State attorneys general and local prosecutors operate under dual constraints: they must enforce state civil rights law (where such laws exist and are funded) while federal criminal jurisdiction creates parallel authority. The constraint is hybrid — genuine coordination problem (preventing chaos from overlapping enforcement) combined with asymmetric extraction (federal authority supersedes state authority, reducing state autonomy and funding). State enforcers benefit from federal standards (coordination value) while losing jurisdictional control (extraction cost).
constraint_indexing:constraint_classification(sotu_1966_johnson_federal_civil_rights_enforcement_crime, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: FEDERAL DEPARTMENT OF JUSTICE (ROPE) — The DOJ gains institutional capacity, budget allocation, and enforcement authority. The constraint from the DOJ's perspective is pure coordination: the federal crime statute solves the collective action problem of fragmented state enforcement by centralizing authority. The DOJ experiences the constraint as enabling its institutional mission. No meaningful extraction from the DOJ's viewpoint — the federal apparatus is the primary beneficiary and the primary agent empowered by the statute.
constraint_indexing:constraint_classification(sotu_1966_johnson_federal_civil_rights_enforcement_crime, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: CIVIL RIGHTS COALITION (SCAFFOLD) — Organized civil rights groups (NAACP, CORE, SCLC, etc.) see the federal crime statute as a temporary but essential enforcement tool. The constraint has sunset logic: as integration norms solidify and state-level enforcement capacity improves, federal criminal jurisdiction becomes less necessary. The coalition experiences constrained exit (limited ability to reduce reliance on federal enforcement while discrimination persists) but perceives a clear termination pathway. Theater is low for the coalition — they understand the statute as functional enforcement, not performative commitment.
constraint_indexing:constraint_classification(sotu_1966_johnson_federal_civil_rights_enforcement_crime, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: SOUTHERN STATE RESISTANCE (PITON) — States resisting integration experience the federal crime statute as degraded coercive authority: they continue performative compliance (token integration, cosmetic civil rights offices) while the extraction mechanism (criminal liability) persists through institutional inertia and continued discrimination. Theater ratio is high — state legislatures pass 'civil rights protection' statutes that preserve segregation through technical compliance. The piton classification reflects that the resistance system is maintained not because it functions (it does not prevent federal intervention) but because institutional inertia and political costs keep the structure in place.
constraint_indexing:constraint_classification(sotu_1966_johnson_federal_civil_rights_enforcement_crime, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: DISCRIMINATORY PRIVATE ENTITY (TANGLED ROPE) — Private landlords, housing sellers, and business owners face federal criminal liability for discrimination. The constraint is hybrid: it genuinely coordinates market access (preventing monopolistic gatekeeping that was fragmenting housing and service markets by race) while imposing asymmetric extraction (criminal penalties and forced compliance). The private entity experiences both the coordination benefit (stable market rules that enable transactions) and the extraction cost (loss of discriminatory monopoly premium and criminal exposure).
constraint_indexing:constraint_classification(sotu_1966_johnson_federal_civil_rights_enforcement_crime, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a universalist analytical perspective, the federal crime statute appears to encode an immutable principle: equal protection under law is a bedrock constitutional principle, and federal enforcement is the necessary structural consequence of state-level failure to protect. This perspective risks naturalizing the statute as inevitable rather than contingent — treating federalism expansion as a law of governance rather than a specific institutional choice. The false summit detector will flag this as naturalization of a constructed policy choice.
constraint_indexing:constraint_classification(sotu_1966_johnson_federal_civil_rights_enforcement_crime, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sotu_1966_johnson_federal_civil_rights_enforcement_crime_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(sotu_1966_johnson_federal_civil_rights_enforcement_crime, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(sotu_1966_johnson_federal_civil_rights_enforcement_crime, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(sotu_1966_johnson_federal_civil_rights_enforcement_crime, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(sotu_1966_johnson_federal_civil_rights_enforcement_crime, TR),
    TR >= 0.70.

:- end_tests(sotu_1966_johnson_federal_civil_rights_enforcement_crime_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The statute creates criminal liability for discrimination, shifting enforcement from victim complaint to federal prosecution. The extraction mechanism is genuine — private entities and resistant states bear costs they did not voluntarily accept. However, extraction is not as severe as a pure snare (0.66+) because the coordination function (unified standards, elimination of fragmentation) provides real benefit. The beneficiary (federal apparatus and racial minorities gaining nominal rights) receives genuine value. Suppression (0.68): High. Discriminatory actors face criminal penalties, deportation from housing/service markets, and loss of discretionary gatekeeping authority. State-level systems face federal override of autonomy. Victims (powerless minorities) face continued discrimination through proxy mechanisms despite nominal criminal liability. Theater ratio (0.35): Moderate-low, declining. Federal enforcement mechanisms are substantive (investigation, prosecution, conviction) rather than performative. However, proxy discrimination mechanisms persist (redlining through credit scoring, jury exclusion through felony strikes, housing through selective lending), indicating that the formal criminal liability masks continued extraction through indirect means. The decline over time reflects increasing federal enforcement capacity and improving compliance rates.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap reveals the constraint's hybrid nature. The federal enforcer (DOJ) sees pure coordination (Rope) — unified standards solving fragmentation. The organized civil rights coalition sees temporary enforcement with sunset (Scaffold) — criminal jurisdiction as a transition tool. Discriminatory private entities see mixed extraction and coordination (Tangled Rope) — they lose gatekeeping discretion but gain stable market rules. Powerless minorities see continued extraction despite nominal rights (Snare) — proxy mechanisms persist and federal enforcement is imperfect. State systems see degraded coercive authority (Piton) — maintaining performative resistance through technical compliance rather than functional obstruction. The analytical observer risks seeing immutable constitutional principle (Mountain) — federal enforcement as inevitable consequence of equal protection — when in fact the statute represents a specific political choice to expand federal criminal authority. The false summit indicator flags this naturalization.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is derived from beneficiary/victim declarations and exit capacity. The federal apparatus (institutional/arbitrage) benefits from expanded jurisdiction with minimal exit barrier — they experience d ≈ 0.05 (strong beneficiary). Discriminatory private entities (powerful/constrained) face criminal liability with high cost to exit (relocate business, abandon market position) — they experience d ≈ 0.85 (strong target). State-level systems (institutional/constrained) lose autonomy but retain significant discretion through proxy enforcement — d ≈ 0.55 (mixed). Powerless minorities (powerless/trapped) have nominal federal rights but face persistent practical barriers — d ≈ 0.90 (target by proxy mechanisms). The chi formula χ = ε × f(d) × σ(S) produces high effective extraction for trapped minorities (d=0.90 → f(d)≈1.42) and low for beneficiaries (d=0.05 → f(d)≈-0.12), reflecting the perspectival gap.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved through recognizing the genuine coordination function (unified enforcement standards, elimination of fragmented evasion) alongside asymmetric extraction (federal override of state autonomy, criminal liability imposed without consent). The Tangled Rope classification is appropriate — neither pure coordination (Rope) nor pure extraction (Snare) accurately captures the constraint. The civil rights coalition's Scaffold perspective documents realistic sunset logic: as integration norms solidify and state-level enforcement capacity improves, federal criminal jurisdiction becomes less necessary. The South's Piton perspective reveals degraded institutional function: performative resistance persists through institutional inertia even as federal override becomes permanent. The powerless minority's Snare perspective reveals that nominal criminal rights do not eliminate proxy discrimination mechanisms — the formal enforcement apparatus masks continued extraction through indirect channels. The mandatrophy is resolved by acknowledging that all six perspectives are structurally valid readings of the same constraint from different positions.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    federalism_extraction_vs_coordination,
    'Does federal criminal jurisdiction over civil rights primarily coordinate fragmented enforcement, or does it primarily extract authority from states?',
    'Analysis of federal enforcement capacity before/after statute; comparison of state-level enforcement outcomes in federal vs non-federal crimes; historical trajectories of state delegation vs state resistance',
    'If primarily coordination: statute classifies as Rope from state perspective. If primarily extraction: Snare from state perspective.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(federalism_extraction_vs_coordination, empirical, 'Whether federal jurisdiction is coordination or extraction').

omega_variable(
    private_entity_enforcement_mechanism,
    'Does criminal liability for discrimination actually deter private discrimination, or does it persist through proxy mechanisms (redlining via credit scoring, jury exclusion via felony strikes, housing via selective lending)?',
    'Longitudinal data on discrimination complaint rates before/after statute; comparison of overt vs proxy discrimination mechanisms; analysis of conviction rates for civil rights obstruction crimes',
    'If effective deterrent: snare classification overstates extraction. If proxy mechanisms persist: snare classification understates extraction burden on victims.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(private_entity_enforcement_mechanism, empirical, 'Whether criminal liability deters or displaces discrimination').

omega_variable(
    state_enforcement_capacity_improvement,
    'Does federal criminal authority improve state-level enforcement capacity (by setting standards and providing resources) or crowd out state enforcement (by substituting federal for state mechanisms)?',
    'Measurement of state civil rights office staffing and budget before/after federal statute; analysis of federal vs state enforcement actions; timing of state law improvements relative to federal statute',
    'If complementary: scaffold sunset timeline is realistic. If crowding-out: federal constraint becomes permanent rather than temporary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(state_enforcement_capacity_improvement, empirical, 'Whether federal authority complements or crowds out state enforcement').

omega_variable(
    norms_vs_enforcement_mechanism,
    'Is integration progress driven primarily by criminal enforcement (snare extraction cost) or by norm shift (rope coordination function)?',
    'Comparative analysis of integration rates in jurisdictions with strong vs weak enforcement; before/after analysis of federal statute impact controlling for ongoing civil rights movement and economic integration; survey data on compliance motivation',
    'If enforcement-driven: statute is primary extraction mechanism. If norm-driven: statute is performative accompaniment to norm shift.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(norms_vs_enforcement_mechanism, empirical, 'Whether enforcement or norms drive integration progress').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sotu_1966_johnson_federal_civil_rights_enforcement_crime, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fcrj_theater_1964, sotu_1966_johnson_federal_civil_rights_enforcement_crime, theater_ratio, 0, 0.42).
narrative_ontology:measurement(fcrj_theater_1969, sotu_1966_johnson_federal_civil_rights_enforcement_crime, theater_ratio, 3, 0.38).
narrative_ontology:measurement(fcrj_theater_1972, sotu_1966_johnson_federal_civil_rights_enforcement_crime, theater_ratio, 6, 0.35).

% Extraction over time
narrative_ontology:measurement(fcrj_extractiveness_1964, sotu_1966_johnson_federal_civil_rights_enforcement_crime, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(fcrj_extractiveness_1969, sotu_1966_johnson_federal_civil_rights_enforcement_crime, base_extractiveness, 3, 0.45).
narrative_ontology:measurement(fcrj_extractiveness_1972, sotu_1966_johnson_federal_civil_rights_enforcement_crime, base_extractiveness, 6, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sotu_1966_johnson_federal_civil_rights_enforcement_crime, enforcement_mechanism).
narrative_ontology:affects_constraint(sotu_1966_johnson_federal_civil_rights_enforcement_crime, voting_rights_act_1965_federal_preclearance).
narrative_ontology:affects_constraint(sotu_1966_johnson_federal_civil_rights_enforcement_crime, civil_rights_act_1964_public_accommodation).
narrative_ontology:affects_constraint(sotu_1966_johnson_federal_civil_rights_enforcement_crime, fair_housing_act_1968_lending_discrimination).

% DUAL FORMULATION NOTE:
% This constraint is upstream of Fair Housing Act lending discrimination enforcement (1968) and related to Voting Rights Act preclearance mechanism (1965) and Civil Rights Act public accommodation enforcement (1964). The constraint family decomposes into: voting rights enforcement (higher ε, pure federal jurisdiction), public accommodation coordination (lower ε, genuine coordination), and housing discrimination (moderate ε, mixed criminal and civil remedies). Each has distinct enforcement mechanisms and beneficiary/victim structures.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(sotu_1966_johnson_federal_civil_rights_enforcement_crime, institutional, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
