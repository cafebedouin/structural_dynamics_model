% ============================================================================
% CONSTRAINT STORY: rights_catalog_facade__social_rights_substance_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_rights_catalog_facade__social_rights_substance_reading, []).

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
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: rights_catalog_facade__social_rights_substance_reading
 *   human_readable: Rights Catalog Facade: Social Rights Substance Reading
 *   domain: legal/constitutional/political
 *
 * SUMMARY:
 *   This constraint story instantiates ONE READING of the contested rights
 *   catalog kernel: the social-rights-substance reading. This reading
 *   interprets the catalog's legitimacy through the measurable delivery of
 *   employment, housing, and education — goods actually provided to the
 *   population — rather than through the formal civil rights clauses that
 *   lack functional substance. The kernel itself is ambiguous: the same text
 *   contains both conditional limitations on civil liberties ('in conformity
 *   with the interests of the working people') AND measurable social
 *   guarantees (full employment, universal housing, universal education).
 *   This constraint story captures how the social-rights reading makes sense
 *   of this ambiguity: the catalog is selectively true (social substance),
 *   and the selectivity is not accidental but structural — the civil rights
 *   are formally present but functionally void, their void compensated by the
 *   social delivery. The core extractive mechanism is labor contingency:
 *   welfare recipients receive housing, employment, and education on the
 *   condition of labor obligation. The victim set includes both civil liberty
 *   claimants (trapped in the formal rights without functional remedy) and
 *   labor contingency bearers (whose autonomy is constrained by the condition
 *   that work is mandatory for provision). The beneficiary set includes
 *   welfare recipients (who receive measurable goods) and the state
 *   administrative apparatus (which coordinates the vast logistics of
 *   universal provision through conditional labor allocation). This reading
 *   coexists with two sibling readings: the conditional-clause reading (which
 *   emphasizes the pre-limitations built into the civil rights themselves)
 *   and the external-showcase reading (which emphasizes the catalog as a
 *   propaganda artifact written for international performance rather than
 *   domestic function). The three readings are not contradictory — they are
 *   three different aspects of the same institutional phenomenon, each
 *   emphasizing different structural features. This story emphasizes the
 *   delivery function; the conditional-clause reading would emphasize the
 *   limitation function; the external-showcase reading would emphasize the
 *   performative function.
 *
 * KEY AGENTS:
 *   - Civil Liberty Claimants: Primary victim (powerless/trapped) — granted formal civil rights that cannot be exercised due to conditional limitations; trapped in illusion of protection without substance
 *   - Welfare Recipients: Primary beneficiary (moderate/constrained) — receive measurable provision (housing, employment, education) in exchange for labor obligation; constrained by work requirement but genuine beneficiary of delivered minima
 *   - Labor Collective: Organized victim/participant (organized/constrained) — structured as both participants in socialist coordination and bearers of labor contingency; see both genuine provision and embedded extraction
 *   - State Administrative Apparatus: Institutional beneficiary (institutional/arbitrage) — coordinates the massive logistics of universal housing, employment, and education provision; experiences the constraint as coordination achievement
 *   - International Anti-Colonial Movement: Organized observer (organized/constrained) — sees the social rights as proof of socialist superiority over colonial/liberal orders; has agency and sees a development pathway forward
 *   - Liberal Constitutional Tradition: Institutional observer (institutional/arbitrage) — sees the catalog as maintaining constitutional form while substance has relocated to social guarantees; observes its own functional displacement
 *   - Analytical Observer: Civilizational analytical (analytical/analytical) — risks naturalizing the civil-vs-social tradeoff as inevitable law rather than contingent institutional design
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(rights_catalog_facade__social_rights_substance_reading, 0.54).
domain_priors:suppression_score(rights_catalog_facade__social_rights_substance_reading, 0.62).
domain_priors:theater_ratio(rights_catalog_facade__social_rights_substance_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(rights_catalog_facade__social_rights_substance_reading, extractiveness, 0.54).
narrative_ontology:constraint_metric(rights_catalog_facade__social_rights_substance_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(rights_catalog_facade__social_rights_substance_reading, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(rights_catalog_facade__social_rights_substance_reading, tangled_rope).
narrative_ontology:human_readable(rights_catalog_facade__social_rights_substance_reading, "Rights Catalog Facade: Social Rights Substance Reading").
narrative_ontology:topic_domain(rights_catalog_facade__social_rights_substance_reading, "legal/constitutional/political").

domain_priors:requires_active_enforcement(rights_catalog_facade__social_rights_substance_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(rights_catalog_facade__social_rights_substance_reading, 'ef5a5f16-29a7-4c61-9a99-478c1cf54661').
narrative_ontology:cs_kernel_codification('ef5a5f16-29a7-4c61-9a99-478c1cf54661', fixed_text).
narrative_ontology:cs_authority_grounding('ef5a5f16-29a7-4c61-9a99-478c1cf54661', lineage).
narrative_ontology:cs_interpretation_layer_present('ef5a5f16-29a7-4c61-9a99-478c1cf54661').
narrative_ontology:cs_reading_relation('ef5a5f16-29a7-4c61-9a99-478c1cf54661', rights_catalog_facade__conditional_clause_reading, coexists_with).
narrative_ontology:cs_reading_relation('ef5a5f16-29a7-4c61-9a99-478c1cf54661', rights_catalog_facade__external_showcase_reading, influences).
narrative_ontology:cs_axiom('ef5a5f16-29a7-4c61-9a99-478c1cf54661', foundational, social_provision_substantive_reality).
narrative_ontology:cs_axiom_status(social_provision_substantive_reality, holdable).
narrative_ontology:cs_axiom_grounding('ef5a5f16-29a7-4c61-9a99-478c1cf54661', social_provision_substantive_reality, empirically_contingent).
narrative_ontology:cs_axiom('ef5a5f16-29a7-4c61-9a99-478c1cf54661', foundational, labor_contingency_structural_feature).
narrative_ontology:cs_axiom_status(labor_contingency_structural_feature, holdable).
narrative_ontology:cs_axiom_grounding('ef5a5f16-29a7-4c61-9a99-478c1cf54661', labor_contingency_structural_feature, empirically_contingent).
narrative_ontology:cs_reference_frame('ef5a5f16-29a7-4c61-9a99-478c1cf54661', socialist_legitimate_provision_architecture).
narrative_ontology:cs_drift_state('ef5a5f16-29a7-4c61-9a99-478c1cf54661', post_mandate_consolidation, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('ef5a5f16-29a7-4c61-9a99-478c1cf54661', '').
narrative_ontology:cs_kernel_id(rights_catalog_facade__social_rights_substance_reading, rights_catalog_facade).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(rights_catalog_facade__social_rights_substance_reading, welfare_recipients_social_minima).
narrative_ontology:constraint_victim(rights_catalog_facade__social_rights_substance_reading, civil_liberty_claimants).
narrative_ontology:constraint_victim(rights_catalog_facade__social_rights_substance_reading, labor_contingency_bearers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CIVIL LIBERTY CLAIMANT (SNARE) — Granted rights that are formally stated but functionally void. The constraint extracts obedience through the illusion of protection without substance. Cannot exit the legal framework; bears the full cost of the conditional clauses embedded in the civil rights catalog ('in conformity with the interests of the working people' — the state defines the boundaries). Maximum extraction: formal freedom without functional exercise.
constraint_indexing:constraint_classification(rights_catalog_facade__social_rights_substance_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: WELFARE RECIPIENT (ROPE) — Receives measurable substance: employment guarantees, housing allocation, education access. The constraint functions as coordination of basic needs provision. Constrained exit (cannot simply reject housing or employment and maintain subsistence), but genuine benefit from delivered minima. The coordination function is real — the state actually provides these goods — though the coordination is asymmetric (the state allocates, the recipient receives). Net beneficiary, though with reduced autonomy.
constraint_indexing:constraint_classification(rights_catalog_facade__social_rights_substance_reading, rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: LABOR COLLECTIVE (TANGLED ROPE) — Organized agents (unions, workplace councils) see both coordination function and asymmetric extraction. The social rights are genuine coordination of production and welfare, but the extraction mechanism is embedded: labor contingency (you work, you eat; you refuse, you starve) is built into the rights themselves. The state enforces both the coordination and the asymmetry. Requires active enforcement of both the delivery and the labor obligation.
constraint_indexing:constraint_classification(rights_catalog_facade__social_rights_substance_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: STATE ADMINISTRATIVE APPARATUS (ROPE) — Experiences the constraint as pure coordination: the state allocates employment, housing, education; the system functions by distributing these goods. From the administrative view, the social rights catalog is a coordination achievement — it solves the massive logistical problem of providing universal housing, employment, and education to the entire population. The illusion (civil rights without substance) and the extraction (labor contingency) are not experienced as extraction from the state's perspective; they are experienced as necessary conditions for the coordination to work. Maximum beneficiary.
constraint_indexing:constraint_classification(rights_catalog_facade__social_rights_substance_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: INTERNATIONAL ANTI-COLONIAL MOVEMENT (SCAFFOLD) — Anti-colonial states and non-aligned movements see the social rights catalog as a temporary but genuinely superior alternative to colonial liberal formalism. The catalog proves that socialist governance can deliver universal provision where colonial and liberal orders could not. This perspective sees the social rights as a sunset: the catalog is a development stage that will be superseded when other socialist states achieve similar delivery. Theater is moderate — the international showcase aspect is real, but the domestic delivery is also substantively real. Low effective extraction from the movement's perspective because they have agency and see an exit (full socialist development pathway).
constraint_indexing:constraint_classification(rights_catalog_facade__social_rights_substance_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: LIBERAL CONSTITUTIONAL TRADITION (PITON) — The civil rights clauses in the catalog are largely performative from the liberal tradition's viewpoint. The formal rights of speech and assembly exist in the text but are functionally void. This perspective sees the catalog as maintaining the appearance of constitutionalism (rights, limits on power, rule of law) while the substance has been relocated to the social guarantees. The liberal tradition's own criteria for legitimacy (meaningful civil liberty) are not met, yet the institutional form (a constitution, a catalog, formal rights) persists. Theater ratio is high (the form of liberal constitutionalism without substance). The catalog maintains piton status through institutional inertia — the socialist state keeps the constitutional form because it provides international legitimacy and internal psychological satisfaction (the citizens believe in the constitution) even though the functional architecture of constraint and coordination runs through the social rights, not the civil rights.
constraint_indexing:constraint_classification(rights_catalog_facade__social_rights_substance_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, the tradeoff between civil liberty and social provision is presented as a necessary structural law: a political system cannot simultaneously guarantee both formal freedoms and material security — you must choose. This perspective sees the social rights emphasis as a rational response to material scarcity; the suppression of civil liberties is treated as an inevitable cost of achieving universal provision. However, the structural data contradicts the mountain classification: the suppression of civil liberties is not a natural law but a contingent institutional design choice. The engine's false summit detector will identify this as naturalization of what is actually a doctrinal reading that prioritizes social over civil goods.
constraint_indexing:constraint_classification(rights_catalog_facade__social_rights_substance_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(rights_catalog_facade__social_rights_substance_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(rights_catalog_facade__social_rights_substance_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(rights_catalog_facade__social_rights_substance_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(rights_catalog_facade__social_rights_substance_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(rights_catalog_facade__social_rights_substance_reading, TR),
    TR >= 0.70.

:- end_tests(rights_catalog_facade__social_rights_substance_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.54): Moderate-high. The state extracts labor obligation (the condition for welfare receipt) in exchange for the delivery of housing, employment, and education. The extractiveness is not maximal because the welfare recipients do receive the promised goods — the labor is not pure extraction (work without pay or benefit), but rather a condition on the benefit delivery. The trajectory shows rising extractiveness over time (0.35 → 0.54) as the state tightens enforcement of labor obligation and expands the scope of what activities count as 'acceptable' work. Suppression (0.62): Moderate-high. Two suppression mechanisms operate: (1) civil rights are formally granted but functionally suppressed through conditional clauses ('in conformity with state interests'), creating an illusion of freedom without substance; (2) labor contingency suppresses exit options — you cannot refuse work and maintain welfare receipt. The suppression is not total because the social provision is real (not pure illusion) and because there are informal workarounds and de facto exemptions. Theater Ratio (0.58): Moderate-high. The civil rights catalog contains substantial performative content — formal rights without functional remedy. The theater serves two purposes: (1) it provides internal legitimation (the citizens believe in the constitutional form even though the substance is in social provision), and (2) it provides international legitimation (the state can cite the civil rights catalog to claim constitutionalism). However, the theater is not complete — the social rights delivery is functionally real, not purely performative. The rising trajectory (0.42 → 0.58) reflects increasing emphasis on the catalog's international showcasing function as the state seeks external legitimation.
 *
 * PERSPECTIVAL GAP:
 *   This reading reveals multiple structural gaps. The civil liberty claimant experiences the constraint as a snare (formal rights without substance), while the welfare recipient experiences it as a rope (genuine coordination of provision). The state administrative apparatus experiences the constraint as pure coordination, while the labor collective experiences it as tangled rope (coordination + extraction). The international anti-colonial movement experiences it as a temporary scaffold (superior to alternatives but unsustainable), while the liberal constitutional tradition experiences it as a piton (degraded form of liberalism, maintained through inertia). The analytical observer risks naturalizing the tradeoff as a mountain (inevitable law that you cannot have both civil freedom and social provision), but the structural data reveals this as a false summit — the tradeoff is a contingent institutional design choice, not a natural law.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values are derived from each agent's structural position relative to the extraction flow. Civil liberty claimants are victims (d=0.95) with no exit (trapped): maximum extraction. Welfare recipients are beneficiaries (d=0.15) with some exit cost (constrained labor): moderate benefit. State administrative apparatus are beneficiaries (d=0.05) with full arbitrage (can withdraw provision and still maintain institutional position): maximum benefit. Labor collective are both participants and contingency-bearers (d=0.55): moderate extraction. The piton and mountain perspectives are institutional actors (institutional atom, d=0.0 canonical, overridden by the specific structural position — the liberal tradition has arbitrage exit but is observing its own functional displacement, so derived d ≈ 0.40; the analytical observer has analytical exit and derives d ≈ 0.72).
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by recognizing that this constraint is a genuine tangled rope: it contains both a coordination function (the state delivers housing, employment, education to the population — a massive coordination achievement) and an extraction mechanism (labor contingency — the beneficiary of provision must labor for the state). The coordination function is real: the state actually solves the logistical problem of providing universal welfare. The extraction mechanism is also real: the welfare is conditional on labor, and labor obligation constrains recipient autonomy. The tension between these two is not a logical contradiction but a structural feature of the constraint. The civil liberty claimants experience this as a snare (pure extraction of formal rights without substance) because their exit options are nil — they have no functional civil remedy and no welfare alternative if they refuse the labor regime. The welfare recipients experience this as a rope (coordination) because they benefit from the provision even though it is conditional. The analytical observer risks seeing a mountain (natural law of political economy: you can have social provision or civil freedom, not both) but the structural data reveals this as a false summit — the civil void is not a necessary consequence of the social provision but a contingent design choice (conditional clauses could have been narrower, civil remedies could have been provided, alternatives to labor contingency could have been structured). The reading's correctness rests on the empirical question of whether the social delivery is actually measurable and genuine (omega: delivery_measurement_definition). If the delivery is real, the reading is correct, and the constraint is a tangled rope. If the delivery is illusory or vastly lower than official reports, the reading fails, and the constraint approaches pure snare.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    delivery_measurement_definition,
    'What constitutes measurable delivery of social rights? Are the measurement standards the same as the state''s official reporting, or can independent verification establish different delivery rates?',
    'Comparative analysis: official state reports vs. independent census, household surveys, and institutional audits; cross-reference with international labor organization and UN habitat assessments for the period',
    'If official measures are accurate: social rights delivery validates the reading''s core premise (social substance > civil void). If independent measures show lower delivery rates: extractiveness increases (the state extracts labor promise while under-delivering welfare), and classification may shift toward pure snare.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(delivery_measurement_definition, empirical, 'Measurement discrepancies between official state reports and independent assessment of social rights delivery').

omega_variable(
    conditional_labor_binding,
    'Is the labor contingency (employment guarantee coupled with work obligation) binding as written, or are there de facto exemptions and non-enforcement that reduce the actual suppression?',
    'Historical analysis of enforcement: what fraction of welfare recipients faced actual labor sanctions for refusal? What were the appeal mechanisms? Were exemptions granted (illness, age, childcare)? Comparison with published policy to actual practice.',
    'If binding enforcement: suppression and extractiveness remain high. If de facto permissive: the constraint shifts from extraction mechanism toward coordination mechanism, potentially reclassifying from snare to rope for welfare recipients.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(conditional_labor_binding, empirical, 'Enforcement and de facto leniency in labor contingency for welfare recipients').

omega_variable(
    reading_kernel_contestation,
    'Which reading of the rights catalog kernel is actually operative in the state''s institutional practice and legitimacy claims: the conditional clauses reading (freedoms pre-limited by state definition), the external showcase reading (domestic constraint masked by international performance), or the social substance reading (measurable delivery redeeming the catalog)?',
    'Institutional analysis: examine the state''s own legal doctrine, judicial interpretation, propaganda materials, and internal administrative guidance. What legitimacy claim is primary? Which constraint (conditional limitation, international performance, social delivery) is most actively enforced and defended?',
    'If social substance reading is primary: this constraint story captures the operative legitimacy and extraction mechanism. If conditional clauses reading is primary: the state''s functional constraint is the built-in limitation on civil rights, not the delivery of social rights. If external showcase is primary: the entire catalog is performative theater, and all readings are piton-tier inertia.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_kernel_contestation, conceptual, 'Which reading of the rights catalog kernel is operative in state institutional practice').

omega_variable(
    socialist_promise_asymmetry,
    'Does the social rights delivery redeem the socialist legitimacy claim, or does the contingency of labor obligation constitute a broken promise that undermines the entire catalog''s legitimacy?',
    'Ideological text analysis and reception history: how did welfare recipients and civil liberty claimants interpret the social rights? Did the delivery of housing and employment satisfy the promise, or was the labor obligation perceived as transforming promised rights into wages/compensation? Did the state''s own ideology frame social rights as gifts from the benevolent state or as earned compensation for labor?',
    'If delivery redeemed promise: the social substance reading is correct, and the constraint is a genuine tangled rope (coordination + extraction in balance). If contingency undermined promise: the extraction exceeds the coordination, and the constraint is closer to snare; the reading conflates justification with function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(socialist_promise_asymmetry, preference, 'Whether social rights delivery redeems or breaks the socialist legitimacy promise').

omega_variable(
    civil_void_non_enforcement,
    'Are civil rights void because they are formally absent from effective legal remedy (no courts will hear speech claims), or because the conditional clauses are sufficiently broad to encompass all possible claimant behavior (the limitations are definitionally complete)?',
    'Doctrinal analysis: survey of attempted civil rights claims in the state''s legal system; examine whether claimants lost on the grounds of formal absence of protection or on the grounds that their conduct violated the ''conformity with state interest'' conditions. Analyze the interpretive evolution of the conditional clauses.',
    'If formal absence: the civil rights are pure illusion, and the snare classification for civil claimants is maximal. If conditional clauses are complete: the civil rights are nominally present but logically always-denied (the conditions cover all speech/assembly outcomes). Classification remains snare but through different mechanism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(civil_void_non_enforcement, empirical, 'Mechanism of civil rights non-enforceability: formal absence vs. complete conditional clauses').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(rights_catalog_facade__social_rights_substance_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(righ_tr_t0, rights_catalog_facade__social_rights_substance_reading, theater_ratio, 0, 0.42).
narrative_ontology:measurement(righ_tr_t5, rights_catalog_facade__social_rights_substance_reading, theater_ratio, 5, 0.52).
narrative_ontology:measurement(righ_tr_t10, rights_catalog_facade__social_rights_substance_reading, theater_ratio, 10, 0.58).

% Extraction over time
narrative_ontology:measurement(righ_be_t0, rights_catalog_facade__social_rights_substance_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(righ_be_t5, rights_catalog_facade__social_rights_substance_reading, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(righ_be_t10, rights_catalog_facade__social_rights_substance_reading, base_extractiveness, 10, 0.54).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(rights_catalog_facade__social_rights_substance_reading, resource_allocation).
narrative_ontology:affects_constraint(rights_catalog_facade__social_rights_substance_reading, rights_catalog_facade__conditional_clause_reading).
narrative_ontology:affects_constraint(rights_catalog_facade__social_rights_substance_reading, rights_catalog_facade__external_showcase_reading).

% DUAL FORMULATION NOTE:
% The rights catalog kernel is contested: three separate constraint stories capture three different readings of the same ambiguous text. The SOCIAL RIGHTS SUBSTANCE READING (this story) emphasizes measurable welfare delivery. The CONDITIONAL CLAUSE READING emphasizes pre-limitations on civil rights. The EXTERNAL SHOWCASE READING emphasizes performative function. These are not three different constraints — they are three different readings of one kernel. The network edges record that this reading affects the interpretability of its siblings: if the social delivery is real (validates this reading), it constrains the external-showcase reading's claim that the entire catalog is pure performance.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
