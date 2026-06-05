% ============================================================================
% CONSTRAINT STORY: dk_foreign_convict_expulsion
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_dk_foreign_convict_expulsion, []).

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
 *   constraint_id: dk_foreign_convict_expulsion
 *   human_readable: Denmark's Foreign Convict Expulsion Law
 *   domain: political/social
 *
 * SUMMARY:
 *   Denmark's foreign convict expulsion law mandates the automatic expulsion
 *   of any non-citizen sentenced to at least one year in prison. The policy
 *   operationalizes a stark principle: criminal conviction of a foreign
 *   national results in immediate loss of residency and deportation, with
 *   minimal discretion for judicial override, family reunification claims, or
 *   humanitarian exceptions. The constraint exhibits classic snare
 *   characteristics: high suppression (criminal conviction eliminates
 *   bargaining power), significant extraction (deportation and family
 *   separation), and theater (framing expulsion as crime prevention rather
 *   than political messaging on immigration control). Over the past decade,
 *   extractiveness has risen as the government has tightened enforcement and
 *   enforcement messaging has become more prominent in political campaigns,
 *   suggesting the policy functions partly as extraction (political capital
 *   from visibility) alongside crime control (genuine coordination). Theater
 *   ratio has also increased as the law has become a symbol of tough
 *   immigration stance rather than a quiet administrative procedure,
 *   indicating degradation from coordination mechanism toward political
 *   theater.
 *
 * KEY AGENTS:
 *   - Convicted foreign nationals: Primary victims (powerless/trapped) — face automatic expulsion with minimal appeal mechanisms; experience separation from family, employment, and legal status
 *   - Immigrant communities: Secondary victims (moderate/constrained) — experience stigmatization, family disruption, and uncertainty about legal future; can exit but at high family/economic cost
 *   - Danish political establishment: Primary beneficiaries (institutional/arbitrage) — gain electoral capital, crime prevention narrative, and enforcement visibility; can modify or repeal law
 *   - Danish native citizens: Mixed experience (moderate/constrained) — benefit from crime prevention coordination but bear costs of increased surveillance and integration disruption
 *   - International human rights norms: Degraded observer (institutional/arbitrage) — monitors violations but enforcement is weak and Denmark has arbitrage options
 *   - Analytical observer: Risk of naturalization (analytical/analytical) — may view expulsion authority as inherent to sovereignty rather than contingent policy choice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dk_foreign_convict_expulsion, 0.58).
domain_priors:suppression_score(dk_foreign_convict_expulsion, 0.68).
domain_priors:theater_ratio(dk_foreign_convict_expulsion, 0.62).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dk_foreign_convict_expulsion, extractiveness, 0.58).
narrative_ontology:constraint_metric(dk_foreign_convict_expulsion, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(dk_foreign_convict_expulsion, theater_ratio, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dk_foreign_convict_expulsion, snare).
narrative_ontology:human_readable(dk_foreign_convict_expulsion, "Denmark's Foreign Convict Expulsion Law").
narrative_ontology:topic_domain(dk_foreign_convict_expulsion, "political/social").

domain_priors:requires_active_enforcement(dk_foreign_convict_expulsion).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dk_foreign_convict_expulsion, danish_native_citizens).
narrative_ontology:constraint_beneficiary(dk_foreign_convict_expulsion, political_establishment).
narrative_ontology:constraint_victim(dk_foreign_convict_expulsion, foreign_nationals_in_denmark).
narrative_ontology:constraint_victim(dk_foreign_convict_expulsion, immigrant_communities).
narrative_ontology:constraint_victim(dk_foreign_convict_expulsion, family_reunification_principle).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CONVICTED FOREIGN NATIONAL (SNARE) — Sentenced criminal faces automatic expulsion with minimal discretion; cannot appeal on family reunification grounds or humanitarian basis. Exit is permanent separation from family, employment, and legal residency. Suppression is maximal: the criminal conviction itself eliminates bargaining power, and the mandatory nature prevents negotiation. This agent experiences pure extraction — loss of residence, family separation, deportation — with no coordination benefit.
constraint_indexing:constraint_classification(dk_foreign_convict_expulsion, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: IMMIGRANT COMMUNITIES & FAMILY MEMBERS (SNARE) — Extended family networks experience the constraint through loss of relatives and legal uncertainty about their own status. Communities face stigmatization and increased surveillance risk. Constrained rather than trapped: some can leave Denmark, but family bonds and economic integration create high exit costs. Extraction manifests as community destabilization and fear.
constraint_indexing:constraint_classification(dk_foreign_convict_expulsion, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: DANISH POLITICAL ESTABLISHMENT (ROPE) — Government benefits from law enforcement coordination and public satisfaction. Experiences the constraint as a coordination mechanism: removing criminals prevents repeat victimization, satisfies constituent demands for law enforcement, and operationalizes border sovereignty. Has arbitrage options (can modify or repeal law). Net beneficiary — extraction flows toward this agent in the form of political capital and electoral support.
constraint_indexing:constraint_classification(dk_foreign_convict_expulsion, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: DANISH NATIVE CITIZENS (TANGLED ROPE) — Coordination benefit: law addresses crime prevention and public safety concerns. But also experience extraction through increased police resources, higher surveillance, and potential chilling effects on immigrant integration and reporting of crimes to authorities. Exit is constrained — cannot easily withdraw from the national security framework. Mixed experience: real coordination gain alongside coercive mechanism.
constraint_indexing:constraint_classification(dk_foreign_convict_expulsion, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: INTERNATIONAL HUMAN RIGHTS NORMS (PITON) — The law violates the principle of proportionality in punishment and family reunification rights enshrined in international law (ECHR, ICCPR). Yet enforcement of these norms against Denmark is weak — the nation has arbitrage options (selective compliance, withdrawal threats). International human rights monitoring continues performatively, but actual enforcement is degraded. Theater ratio high: human rights reports are generated but sanctions are minimal.
constraint_indexing:constraint_classification(dk_foreign_convict_expulsion, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (SOVEREIGNTY VIEW) — From a universal perspective, national sovereignty over border control and criminal expulsion is viewed as an immutable feature of the state system. The constraint appears as a natural law of nation-state organization. However, this classification is likely a false summit: the mandatory nature (as opposed to discretionary expulsion) is a contingent policy choice, not inherent to sovereignty itself. The engine's false summit detector should flag this as naturalization.
constraint_indexing:constraint_classification(dk_foreign_convict_expulsion, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(dk_foreign_convict_expulsion_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(dk_foreign_convict_expulsion, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(dk_foreign_convict_expulsion, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(dk_foreign_convict_expulsion, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(dk_foreign_convict_expulsion, TR),
    TR >= 0.70.

:- end_tests(dk_foreign_convict_expulsion_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The policy extracts residency and family relationships from convicted foreign nationals in exchange for crime control coordination. The extraction is genuine (deportation is irreversible) and significant (affects community networks), but not maximal because some convicted individuals are retried/resentenced and the policy has limits (applies only to ≥1 year sentences). The measurement trajectory from 0.35 to 0.58 reflects increasing enforcement and political visibility over the interval, suggesting the extraction mechanism has been activated more aggressively. Suppression (0.68): High. Criminal conviction itself is a suppression mechanism — once sentenced, the foreign national has minimal legal recourse. The mandatory nature eliminates judicial discretion to weigh proportionality or family impact. Media and political messaging suppress alternative framings (e.g., rehabilitation, integration investment). Theater ratio (0.62): Moderate-high. The law generates political messaging and media visibility disproportionate to its actual crime prevention effect. The 1-year threshold is arbitrary (why not 6 months or 3 years?) and the expulsion mechanism is more symbolic than effective for crime prevention (deported individuals may commit crimes elsewhere; integration disruption may increase recidivism). The government gains political capital from the perception of toughness more than from measurable public safety outcomes.
 *
 * PERSPECTIVAL GAP:
 *   The constraint displays a stark perspectival gap between the beneficiary view (rope/coordination) and the victim view (snare/extraction). The Danish political establishment and native citizens see the law as coordination against crime — a mechanism for solving the collective action problem of criminal deterrence. The convicted foreign national sees pure extraction: loss of residence, family separation, and legal status, with no coordination benefit to them. The intermediate view (immigrant communities, moderate exit constraints) classifies as tangled rope — experiencing both the coordination mechanism (crime reduction) and the extraction (community destabilization). The international human rights observer sees degraded enforcement (piton), and the civilizational sovereignty view risks a false summit (naturalizing a contingent policy choice as inherent sovereignty). The perspectival gap between beneficiary and victim is maximal, indicating genuine snare structure rather than pure coordination.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (Danish political establishment, native citizens) experience low or negative effective extractiveness — the law creates coordination benefits (crime prevention, security assurance) that exceed the costs to them. Their directionality d values are low (0.1-0.3), mapping to negative or near-zero χ after applying f(d). Victims (convicted foreign nationals) experience maximum directionality — they are the target of the extraction mechanism (deportation), have no exit options (mandatory law), and are powerless to negotiate. Their d values are high (0.90-1.0), mapping to maximum χ via f(d). The mandatory nature (requires_active_enforcement: true) and victim declaration (victims: [foreign_nationals_in_denmark, immigrant_communities]) drive the snare classification from the victim perspective.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by recognizing that the constraint exhibits genuine coordination (crime prevention) ALONGSIDE genuine extraction (political capital from deportation visibility). The snare classification is appropriate for victims; the rope classification is appropriate for beneficiaries. The question 'is this coordination or extraction?' has a perspectival answer: for the political establishment, it is primarily coordination (with extraction as a secondary benefit); for the convicted foreign national, it is pure extraction (with no coordination benefit). The piton perspective reveals that theater has increased over time, suggesting the coordination function may be degrading and the extraction/political messaging function may be growing. The false summit risk in the sovereignty perspective highlights that the mandatory nature is a policy choice, not an inherent feature of border control. The mandatrophy is fully resolved by the perspectival architecture: all six types are legitimate readings of the same constraint from different structural positions.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    mandatory_vs_discretionary_distinction,
    'Is the mandatory nature of expulsion a feature of sovereignty itself, or a contingent policy choice that other democracies handle with discretion?',
    'Comparative analysis of expulsion regimes across democracies (Canada, UK, Germany, France) examining discretionary vs mandatory structures; documentation of judicial override mechanisms in other countries',
    'If mandatory is inherent: Mountain classification confirmed. If discretionary regimes exist: mandatory regime is Snare (policy extraction), not natural law.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(mandatory_vs_discretionary_distinction, empirical, 'Whether mandatory expulsion is inherent to sovereignty').

omega_variable(
    integration_vs_public_safety_extraction,
    'Does the law represent genuine crime prevention (coordination) or is it extracting integration-related costs from foreign communities as political benefit?',
    'Empirical analysis: crime rates pre/post law implementation; reoffending rates of deported vs non-deported cohorts; correlation between expulsions and political campaign messaging on immigration; investigation of whether expulsion reduces victimization or merely exports crime/deportees',
    'If crime reduction is substantial: Rope classification stronger. If marginal or negative (deportees reoffend elsewhere, integration costs increase): Snare classification confirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(integration_vs_public_safety_extraction, empirical, 'Whether expulsion achieves crime prevention or extracts costs').

omega_variable(
    family_reunification_right_status,
    'Is the right to family reunification a natural constraint on immigration policy, or is it a negotiable principle that can be overridden by security interests?',
    'Legal interpretation of ECHR Article 8 (family life) and its application in case law; analysis of proportionality tests in other democracies; identification of alternative expulsion thresholds that preserve family units while maintaining security',
    'If family reunification is inviolable: law is unconstitutional extraction. If negotiable: theater ratio increases (framing as security theater rather than justice).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(family_reunification_right_status, conceptual, 'Status of family reunification as a constraint on expulsion').

omega_variable(
    conversion_rate_to_actual_extraction,
    'What fraction of foreign nationals sentenced to 1+ year prison actually face enforcement of expulsion, and how does political context affect enforcement rates?',
    'Tracking of expulsion enforcement statistics by government agency; correlation between enforcement rates and political election cycles; analysis of judicial discretion in sentencing that avoids the 1-year threshold',
    'If enforcement is inconsistent: suppression metric decreases (alternatives exist in practice). If enforcement is near-total: suppression and extractiveness both confirmed.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(conversion_rate_to_actual_extraction, empirical, 'Enforcement rate of expulsion for eligible convicts').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dk_foreign_convict_expulsion, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dkfce_tr_t0, dk_foreign_convict_expulsion, theater_ratio, 0, 0.45).
narrative_ontology:measurement(dkfce_tr_t5, dk_foreign_convict_expulsion, theater_ratio, 5, 0.54).
narrative_ontology:measurement(dkfce_tr_t10, dk_foreign_convict_expulsion, theater_ratio, 10, 0.62).

% Extraction over time
narrative_ontology:measurement(dkfce_be_t0, dk_foreign_convict_expulsion, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(dkfce_be_t5, dk_foreign_convict_expulsion, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(dkfce_be_t10, dk_foreign_convict_expulsion, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dk_foreign_convict_expulsion, enforcement_mechanism).
narrative_ontology:affects_constraint(dk_foreign_convict_expulsion, eu_free_movement_directive).
narrative_ontology:affects_constraint(dk_foreign_convict_expulsion, danish_immigration_policy).

% DUAL FORMULATION NOTE:
% The foreign convict expulsion law decomposes into two structurally distinct claims: (1) the principle that nations can expel criminals (low-extractiveness coordination for sovereignty), and (2) the mandatory nature and political use of the law (high-extractiveness extraction mechanism). This story focuses on claim (2) — the actual policy implementation. The sovereignty principle is a separate constraint (appropriate to model as Rope or Mountain depending on perspective). The decomposition explains why extractiveness rises over time: the underlying policy (expulsion authority) is stable, but its use as political messaging (theater) increases, raising the experienced extraction.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(dk_foreign_convict_expulsion, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
