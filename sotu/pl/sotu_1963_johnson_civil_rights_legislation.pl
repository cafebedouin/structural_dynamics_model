% ============================================================================
% CONSTRAINT STORY: sotu_1963_johnson_civil_rights_legislation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sotu_1963_johnson_civil_rights_legislation, []).

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
 *   constraint_id: sotu_1963_johnson_civil_rights_legislation
 *   human_readable: Federal Civil Rights Legislation (1963-1964): Elimination of Race and Color Discrimination
 *   domain: social_policy/civil_rights/federal_legislation
 *
 * SUMMARY:
 *   The Civil Rights Act of 1963-1964, proposed by Johnson and framed as a
 *   memorial to Kennedy, represents a fundamental structural transition in
 *   how racial discrimination is governed in the United States. Prior to this
 *   legislation, racial segregation and discrimination were enforced through
 *   state and local law, backed by violence and exclusion. The new federal
 *   legislation shifts enforcement from voluntary compliance and local
 *   discretion to mandatory federal standards backed by federal authority and
 *   litigation mechanisms. This constraint exhibits the full spectrum of
 *   classification types, depending on the observer's structural position.
 *   For racially marginalized individuals, segregation before the legislation
 *   is a snare; the legislation breaks that snare but creates a tangled rope
 *   during implementation (legal rights exist but informal discrimination
 *   persists and individuals must enforce their own rights). For
 *   segregationist institutions, the legislation is pure snare (extraction
 *   from their profit model and authority). For the federal government, it is
 *   rope (coordination mechanism with no experienced extraction). For civil
 *   rights organizations, it is scaffold (temporary structure with sunset
 *   logic). For the international diplomatic audience, it is piton
 *   (performative function). For the analytical observer, the temptation to
 *   frame civil rights as a natural law reveals itself as false summit — the
 *   constraint is deeply political and contested. The measurements track
 *   extractiveness declining from 0.72 (pre-legislation segregation system,
 *   pure snare for marginalized individuals) to 0.52 (post-implementation,
 *   formal legal protection in place but informal discrimination and
 *   enforcement costs remain). Theater ratio increases from 0.20 to 0.38,
 *   reflecting the growing diplomatic and performative dimensions of civil
 *   rights as international attention intensifies.
 *
 * KEY AGENTS:
 *   - Racially Marginalized Groups: Primary victims pre-legislation (powerless/trapped) — transition to moderate/constrained post-legislation as legal protections emerge but informal discrimination persists
 *   - Segregationist Institutions and Businesses: Primary victims of legislation (powerful/trapped) — lose discriminatory profit models and enforcement authority; face federal mandate and compliance costs
 *   - Federal Executive Branch: Primary beneficiary (institutional/arbitrage) — consolidates civil rights enforcement authority, expands federal administrative reach, enhances international credibility
 *   - Civil Rights Organizations and Activists: Organized agents (organized/constrained) — shape implementation, drive enforcement, coordinate resistance to segregationist circumvention
 *   - State and Local Authorities: Secondary victims (institutional/constrained) — lose enforcement discretion over segregation, must comply with federal standards, face federalism conflict
 *   - International Diplomatic Corps and Cold War Audience: Tertiary beneficiary (institutional/arbitrage) — receives US credibility signal; legislation performs US commitment to freedom and democracy against Soviet ideology
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sotu_1963_johnson_civil_rights_legislation, 0.58).
domain_priors:suppression_score(sotu_1963_johnson_civil_rights_legislation, 0.75).
domain_priors:theater_ratio(sotu_1963_johnson_civil_rights_legislation, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sotu_1963_johnson_civil_rights_legislation, extractiveness, 0.58).
narrative_ontology:constraint_metric(sotu_1963_johnson_civil_rights_legislation, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(sotu_1963_johnson_civil_rights_legislation, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sotu_1963_johnson_civil_rights_legislation, tangled_rope).
narrative_ontology:human_readable(sotu_1963_johnson_civil_rights_legislation, "Federal Civil Rights Legislation (1963-1964): Elimination of Race and Color Discrimination").
narrative_ontology:topic_domain(sotu_1963_johnson_civil_rights_legislation, "social_policy/civil_rights/federal_legislation").

domain_priors:requires_active_enforcement(sotu_1963_johnson_civil_rights_legislation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sotu_1963_johnson_civil_rights_legislation, marginalized_racial_groups).
narrative_ontology:constraint_beneficiary(sotu_1963_johnson_civil_rights_legislation, nation_international_credibility).
narrative_ontology:constraint_beneficiary(sotu_1963_johnson_civil_rights_legislation, federal_institutional_power).
narrative_ontology:constraint_victim(sotu_1963_johnson_civil_rights_legislation, segregationist_institutions).
narrative_ontology:constraint_victim(sotu_1963_johnson_civil_rights_legislation, discriminatory_business_interests).
narrative_ontology:constraint_victim(sotu_1963_johnson_civil_rights_legislation, state_autonomy_doctrine).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: RACIALLY DISCRIMINATED INDIVIDUAL (SNARE) — Before legislation, the discriminated individual is trapped by systematic exclusion from housing, employment, public accommodations, and education with no legal recourse. State and local authorities actively enforce segregation. The individual has no exit option and bears maximum suppression from institutional racism backed by law. However, this perspective also shows the legislation as breaking the snare — the constraint story documents the moment of transition, where federal mandate shifts the power balance. From the discriminated individual's pre-legislation position, the existing segregation system IS a snare (ε > 0.66, suppression ≥ 0.60). The legislation itself becomes the intervention that reclassifies this constraint.
constraint_indexing:constraint_classification(sotu_1963_johnson_civil_rights_legislation, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: NEWLY PROTECTED GROUP — TRANSITIONAL (TANGLED ROPE) — During implementation, marginalized groups gain formal legal rights while facing continued informal discrimination and enforcement costs. The constraint provides genuine coordination function (establishes common legal standards for non-discrimination across states) while imposing asymmetric extraction: the newly protected groups must litigate violations, bear witness risk, and navigate hostile enforcement environments. Some benefit (legal recourse now exists); significant costs (enforcement requires individual courage and resources). Exit options are constrained by the social risk of challenging discrimination even after legalization.
constraint_indexing:constraint_classification(sotu_1963_johnson_civil_rights_legislation, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: FEDERAL GOVERNMENT EXECUTIVE (ROPE) — The federal government experiences the legislation as a pure coordination mechanism with no experienced extraction. The legislation consolidates federal authority over civil rights, enabling federal agencies (EEOC, DOJ, HUD) to enforce uniform standards. The executive branch benefits institutionally (increased enforcement power, administrative reach, international credibility). No extraction from federal perspective — gains coordination clarity and organizational capacity. Arbitrage exit reflects federal government's ability to modify implementation and regulatory approach.
constraint_indexing:constraint_classification(sotu_1963_johnson_civil_rights_legislation, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: SEGREGATIONIST BUSINESS AND STATE INTERESTS (SNARE) — Segregationist institutions and businesses profiting from discriminatory systems experience the legislation as pure extraction without coordination benefit. They lose market control, must incur compliance costs, face potential litigation and penalties. State authorities lose enforcement discretion and face federal override of state law. These actors experience maximum extraction — the legislation is structured specifically to eliminate their profit model and authority. Suppression is high (federal mandate is coercive, backed by enforcement authority). From their structural position, this is maximum snare: high extraction, high suppression, no exit option within the legal framework.
constraint_indexing:constraint_classification(sotu_1963_johnson_civil_rights_legislation, snare,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 5: CIVIL RIGHTS ORGANIZATIONS AND COALITION (SCAFFOLD) — Organized civil rights groups (NAACP, SCLC, SNCC) see the legislation as a temporary structure with sunset logic: the legislation is designed to transition society from segregation to integration, with built-in review and adaptation mechanisms (Title VII sunset clauses, periodic renewal requirements). These organizations have agency in shaping implementation and enforcement policy. They experience the constraint as legitimate coordination with decreasing extraction as compliance mechanisms mature. The theater ratio for this perspective is lower (implementation theater exists, but the legislation's core function is substantive rather than performative). Exit options are constrained by ongoing discrimination, but the coalition has mechanisms to press for enforcement escalation.
constraint_indexing:constraint_classification(sotu_1963_johnson_civil_rights_legislation, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: INTERNATIONAL DIPLOMATIC CORPS (PITON) — From a civilizational/global perspective, the legislation serves a largely performative diplomatic function: it signals US commitment to civil rights on the international stage during the Cold War ideological competition with the Soviet Union. The actual enforcement mechanisms are secondary to the symbolic value. Johnson frames the legislation as strengthening US credibility and soft power. From this perspective, the constraint has high theater (international diplomatic performance, framing as Kennedy memorial, positioning as source of national strength). The underlying coordination function (establishing uniform civil rights standards) is real but subordinate to the diplomatic theater. Piton classification: high theater, moderate extraction (federal authority expansion), institutional inertia in implementation (many agencies become performers of compliance rather than enforcers of rights).
constraint_indexing:constraint_classification(sotu_1963_johnson_civil_rights_legislation, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER — NATURAL LAW FRAME (MOUNTAIN) — From a civilizational/analytical perspective, one might frame civil rights as a natural law: the principle of human equality, non-discrimination based on race, equal protection under law emerge naturally from liberal democratic theory and ethical commitments that transcend specific institutional arrangements. From this view, the legislation is merely the institutional expression of an immutable principle. However, the structural data reveals this as a false summit: the constraint has clear beneficiaries (marginalized groups, federal authority), clear victims (segregationist interests), and clear enforcement mechanisms (federal mandate backed by coercion). The 'natural law' framing naturalizes a deeply political contestation between federal and state power, between segregation and integration, between profit models based on discrimination and alternative models. The false summit reveals that 'equal protection' is not naturally inevitable but contingently achieved through institutional struggle.
constraint_indexing:constraint_classification(sotu_1963_johnson_civil_rights_legislation, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sotu_1963_johnson_civil_rights_legislation_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(sotu_1963_johnson_civil_rights_legislation, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(sotu_1963_johnson_civil_rights_legislation, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(sotu_1963_johnson_civil_rights_legislation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(sotu_1963_johnson_civil_rights_legislation, TR),
    TR >= 0.70.

:- end_tests(sotu_1963_johnson_civil_rights_legislation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The legislation imposes significant costs on segregationist interests (loss of profit model, compliance burden, federal penalties) while generating substantial benefits for marginalized groups (legal recourse, formal equality) and federal authority (enforcement power, institutional expansion). However, extractiveness is not maximum because the coordination function is genuine — the legislation solves a real coordination problem (establishing uniform non-discrimination standards across jurisdictions) that segregationist interests had also been solving, albeit through segregation rather than integration. The trade-off between extraction and coordination prevents maximum snare classification. Suppression (0.75): High. The legislation is backed by federal authority and coercive enforcement mechanisms (EPA, DOJ litigation, agency enforcement). Segregationist interests cannot exit the legal framework. Marginalized groups face high suppression from informal discrimination despite legal protections. The state/local autonomy suppression comes from federal override of state law. Theater ratio (0.35): Moderate-low. The legislation's primary function is substantive legal change, not performance. However, international diplomatic theater is present (framing as Kennedy memorial, emphasizing national strength and freedom). The theater increases over time as the diplomatic significance grows.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap is dramatic. The racially marginalized individual sees the legislation as breaking a snare (pre-legislation: pure extraction and suppression with no legal recourse; post-legislation: legal protection exists but informal discrimination and enforcement costs remain, creating tangled rope). The segregationist institution sees it as pure extraction — the legislation eliminates the institutional basis of their profit model and authority. The federal government sees pure coordination — no extraction, only administrative capacity expansion. The civil rights organization sees temporary problem-solving with sunset (scaffold). The international observer sees performative signaling (piton). The analytical observer risks seeing natural law (mountain) but the structural data reveals false summit. This perspectival range across six types from identical structural data demonstrates the diagnostic power of indexical classification.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) is determined by each agent's structural position relative to the extraction flow. Marginalized groups are beneficiaries at biographical time (legislation provides legal recourse) but face high suppression from informal discrimination and enforcement costs (constrained exit), yielding d ≈ 0.70-0.80 (high experienced extraction initially, declining as informal discrimination adapts). Segregationist interests are full victims of legislation (lose discrimination profit model and authority) with trapped exit (cannot exit legal mandate), yielding d ≈ 0.95 (maximum extraction). Federal government is beneficiary with arbitrage exit (can modify implementation), yielding d ≈ 0.10-0.20 (low/negative extraction, pure coordination benefit). Civil rights organizations are moderate beneficiaries constrained by ongoing discrimination, yielding d ≈ 0.50-0.60 (moderate extraction). The directionality variation across perspectives produces the perspectival gap in experienced extractiveness (chi) despite identical base extractiveness (ε).
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by recognizing that the legislation is genuinely tangled rope: it provides coordination function (uniform non-discrimination standards across jurisdictions) while extracting from segregationist interests and federal authorities (who bear compliance costs). The coordination function is real — before legislation, different states and localities had different segregation regimes, creating coordination problems for national commerce and labor mobility. After legislation, uniform standards enable efficient interstate commerce and labor allocation. However, the extraction is also real — segregationist interests lose their profit model and the ability to enforce segregation. The legislation solves the coordination problem BY extracting from segregationists. This is exactly the tangled rope signature: 0.30 ≤ χ ≤ 0.90, base extraction ε ≥ 0.30, suppression ≥ 0.40, requires_active_enforcement=true, beneficiaries + victims both present. The mandatrophy is not 'is this coordination or extraction?' but 'how much coordination benefit justify how much extraction cost?' The policy answer (yes, civil rights are worth federal enforcement against state autonomy and business interests) resolves the structural tension by accepting the tangled rope classification as legitimate — some extraction is acceptable when it solves a crucial coordination problem and remedies systematic injustice.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    voluntary_vs_coercive_compliance_threshold,
    'Does federal mandate-backed enforcement genuinely establish non-discrimination, or does it shift discrimination from formal/legal to informal/social practices without reducing actual harm?',
    'Longitudinal tracking of formal compliance rates vs actual integration outcomes; comparison of litigation rates vs behavioral change; analysis of de jure vs de facto segregation persistence post-legislation',
    'If coercive enforcement produces genuine integration: legislation succeeds as tangled rope (coordination + extraction trade). If discrimination persists informally: legislation is performative (shifts to piton), and underlying snare remains structurally intact.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(voluntary_vs_coercive_compliance_threshold, empirical, 'Whether federal mandate eliminates actual discrimination or shifts to informal enforcement').

omega_variable(
    federal_extraction_costs_to_beneficiaries,
    'Do the enforcement and compliance costs imposed on newly protected groups (litigation burden, risk exposure, navigation of hostile environments) constitute a secondary extraction layer that reduces the net benefit of legislation?',
    'Analysis of individual and group litigation costs; tracking of enforcement burden distribution; measurement of psychological and social costs of navigating discriminatory environments despite legal protection',
    'If enforcement costs are high relative to benefits: the constraint remains tangled rope but with extraction bias toward newly protected groups. If costs are low and offset by legal recourse: tangled rope with more balanced benefit distribution.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(federal_extraction_costs_to_beneficiaries, empirical, 'Enforcement and navigation costs borne by protected groups').

omega_variable(
    state_autonomy_vs_federal_authority_structural_conflict,
    'Is the conflict between state autonomy and federal civil rights enforcement a permanent structural feature of federal systems, or a transitional problem as state norms align with federal standards?',
    'Historical tracking of state-federal conflict over civil rights enforcement across decades; measurement of state internalization of federal standards; analysis of voluntary vs mandated state compliance over time',
    'If permanent structural feature: legislation is permanently tangled rope (coordination + federal power extraction). If transitional: legislation transforms to rope as state-federal alignment improves and enforcement costs become normalized.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(state_autonomy_vs_federal_authority_structural_conflict, conceptual, 'Whether state-federal conflict is structural or transitional in federal systems').

omega_variable(
    segregationist_adaptation_and_circumvention,
    'Do segregationist interests successfully adapt discrimination mechanisms to circumvent federal law (through formal compliance + informal exclusion, private exclusion, economic gatekeeping), thereby reducing the actual extraction impact on these actors?',
    'Tracking of segregationist adaptation strategies post-legislation; measurement of de facto discrimination persistence; analysis of private discrimination substitution for legal discrimination; study of white flight and private-sector racial exclusion',
    'If circumvention is effective: segregationist extraction costs are lower than anticipated, and the constraint''s actual extractiveness may decline (shifting toward balanced tangled rope). If circumvention fails: extraction remains high and sustained.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(segregationist_adaptation_and_circumvention, empirical, 'Success of segregationist circumvention and adaptation strategies').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sotu_1963_johnson_civil_rights_legislation, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cra63_pre_legislation_theater, sotu_1963_johnson_civil_rights_legislation, theater_ratio, 0, 0.2).
narrative_ontology:measurement(cra63_tr_t1, sotu_1963_johnson_civil_rights_legislation, theater_ratio, 1, 0.28).
narrative_ontology:measurement(cra63_tr_t5, sotu_1963_johnson_civil_rights_legislation, theater_ratio, 5, 0.35).
narrative_ontology:measurement(cra63_tr_t10, sotu_1963_johnson_civil_rights_legislation, theater_ratio, 10, 0.38).

% Extraction over time
narrative_ontology:measurement(cra63_pre_legislation_extractiveness, sotu_1963_johnson_civil_rights_legislation, base_extractiveness, 0, 0.72).
narrative_ontology:measurement(cra63_be_t1, sotu_1963_johnson_civil_rights_legislation, base_extractiveness, 1, 0.68).
narrative_ontology:measurement(cra63_be_t5, sotu_1963_johnson_civil_rights_legislation, base_extractiveness, 5, 0.58).
narrative_ontology:measurement(cra63_be_t10, sotu_1963_johnson_civil_rights_legislation, base_extractiveness, 10, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sotu_1963_johnson_civil_rights_legislation, enforcement_mechanism).
narrative_ontology:affects_constraint(sotu_1963_johnson_civil_rights_legislation, state_autonomy_federalism_doctrine).
narrative_ontology:affects_constraint(sotu_1963_johnson_civil_rights_legislation, segregationist_economic_models).
narrative_ontology:affects_constraint(sotu_1963_johnson_civil_rights_legislation, informal_racial_discrimination_persistence).
narrative_ontology:affects_constraint(sotu_1963_johnson_civil_rights_legislation, voting_rights_enforcement_capacity).

% DUAL FORMULATION NOTE:
% This constraint is upstream of voting rights legislation (VRA 1965) and fair housing legislation (FHA 1968). The federal civil rights enforcement mechanism established here becomes the institutional basis for downstream civil rights constraints. The constraint also decomposes into sub-constraints: federal-mandate-for-formal-equality (higher ε, mountain properties) vs. segregationist-profit-model-elimination (snare for segregationists) vs. federal-power-expansion (rope for federal perspective). These are linked through network affects but represent distinct structural claims.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(sotu_1963_johnson_civil_rights_legislation, powerless, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
