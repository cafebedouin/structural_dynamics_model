% ============================================================================
% CONSTRAINT STORY: equal_rights_amendment__anti_subordination_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-27
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_era_anti_subordination, []).

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
 *   constraint_id: equal_rights_amendment__anti_subordination_reading
 *   human_readable: Equal Rights Amendment (Anti-Subordination Reading)
 *   domain: constitutional_law/gender_equality
 *
 * SUMMARY:
 *   The Equal Rights Amendment's anti-subordination reading constitutes one
 *   authoritative interpretation of a contested constitutional kernel — the
 *   meaning and scope of the ERA's guarantee of equal rights. This reading
 *   holds that the ERA forbids law that perpetuates women's inferior status,
 *   regardless of whether the law classifies by sex on its face. This
 *   interpretation diverges sharply from the sex-blind reading, which holds
 *   that the ERA commands sex-blindness in law analogous to race-blindness
 *   under strict scrutiny, permitting only classifications that do not
 *   mention sex. The anti-subordination reading is structurally distinct: it
 *   targets the extractive mechanism (hierarchy) rather than the formal
 *   mechanism (classification), permitting affirmative measures to dismantle
 *   subordination while forbidding structures that maintain inferior status
 *   even when facially neutral. The constraint exhibits tangled-rope
 *   characteristics: it coordinates institutional action toward hierarchy
 *   dismantling while extracting political and economic costs from
 *   beneficiaries of existing hierarchy. It also shows piton features (formal
 *   equality doctrine persisting through institutional inertia) and scaffold
 *   features (legislatures using anti-subordination framing to justify
 *   transitional dismantling measures). The measurement trajectory shows
 *   extraction rising over time (t0: 0.42 → t50: 0.68) as the reading gains
 *   doctrinal prominence and courts attempt to operationalize hierarchy
 *   identification, while theater ratio declines (t0: 0.68 → t50: 0.51),
 *   reflecting genuine structural change in legal doctrine rather than
 *   performative commitment to equality.
 *
 * KEY AGENTS:
 *   - Women Trapped in Subordination (powerless/trapped): primary victims bearing extraction from hierarchy-perpetuating structures; central beneficiaries of anti-subordination doctrine
 *   - Dismantling Coalitions (organized/constrained): organized equality advocates, courts, legislatures pursuing substantive equality; benefit from doctrinal framework but constrained by institutional resistance
 *   - Courts Interpreting Doctrine (institutional/arbitrage): benefit from anti-subordination framing as coordination mechanism; experience doctrine as primarily coordinating (rope)
 *   - Formal Equality Tradition (institutional/arbitrage): sex-blind doctrine maintained through institutional inertia; increasingly recognized as inadequate to address structural hierarchy (piton)
 *   - State Legislatures (organized/constrained): actively restructuring laws to dismantle subordination; use anti-subordination framing to justify transitional measures (scaffold perspective)
 *   - Beneficiaries of Status Quo Hierarchy (powerful/arbitrage): institutions and actors benefiting from existing hierarchy; experience extraction of hierarchy-derived privilege; constrained by law permitting dismantling measures (tangled rope)
 *   - Analytical Observer (analytical/analytical): risks naturalizing the classification/subordination distinction as ontologically grounded rather than recognizing it as a choice about which extractive mechanism to target
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(equal_rights_amendment__anti_subordination_reading, 0.58).
domain_priors:suppression_score(equal_rights_amendment__anti_subordination_reading, 0.62).
domain_priors:theater_ratio(equal_rights_amendment__anti_subordination_reading, 0.51).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(equal_rights_amendment__anti_subordination_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(equal_rights_amendment__anti_subordination_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(equal_rights_amendment__anti_subordination_reading, theater_ratio, 0.51).

% --- Constraint claim ---
narrative_ontology:constraint_claim(equal_rights_amendment__anti_subordination_reading, tangled_rope).
narrative_ontology:human_readable(equal_rights_amendment__anti_subordination_reading, "Equal Rights Amendment (Anti-Subordination Reading)").
narrative_ontology:topic_domain(equal_rights_amendment__anti_subordination_reading, "constitutional_law/gender_equality").

domain_priors:requires_active_enforcement(equal_rights_amendment__anti_subordination_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(equal_rights_amendment__anti_subordination_reading, '10b1775a-b457-4378-bfa6-99608e52a213').
narrative_ontology:cs_kernel_codification('10b1775a-b457-4378-bfa6-99608e52a213', formalized).
narrative_ontology:cs_authority_grounding('10b1775a-b457-4378-bfa6-99608e52a213', lineage).
narrative_ontology:cs_interpretation_layer_present('10b1775a-b457-4378-bfa6-99608e52a213').
narrative_ontology:cs_reading_relation('10b1775a-b457-4378-bfa6-99608e52a213', equal_rights_amendment__sex_blind_reading, coexists_with).
narrative_ontology:cs_axiom('10b1775a-b457-4378-bfa6-99608e52a213', foundational, hierarchy_perpetuation_is_unconstitutional).
narrative_ontology:cs_axiom_status(hierarchy_perpetuation_is_unconstitutional, holdable).
narrative_ontology:cs_axiom_grounding('10b1775a-b457-4378-bfa6-99608e52a213', hierarchy_perpetuation_is_unconstitutional, deontological).
narrative_ontology:cs_axiom('10b1775a-b457-4378-bfa6-99608e52a213', secondary, affirmative_dismantling_is_permitted).
narrative_ontology:cs_axiom_status(affirmative_dismantling_is_permitted, holdable).
narrative_ontology:cs_axiom_grounding('10b1775a-b457-4378-bfa6-99608e52a213', affirmative_dismantling_is_permitted, deontological).
narrative_ontology:cs_reference_frame('10b1775a-b457-4378-bfa6-99608e52a213', substantive_equality_hierarchy_elimination).
narrative_ontology:cs_drift_state('10b1775a-b457-4378-bfa6-99608e52a213', contemporary_doctrinal_heterogeneity, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('10b1775a-b457-4378-bfa6-99608e52a213', '2026-02-27T14:23:45Z').
narrative_ontology:cs_kernel_id(equal_rights_amendment__anti_subordination_reading, equal_rights_amendment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(equal_rights_amendment__anti_subordination_reading, substantive_equality_claimants).
narrative_ontology:constraint_beneficiary(equal_rights_amendment__anti_subordination_reading, dismantling_coalitions).
narrative_ontology:constraint_victim(equal_rights_amendment__anti_subordination_reading, hierarchy_maintenance_institutions).
narrative_ontology:constraint_victim(equal_rights_amendment__anti_subordination_reading, facially_neutral_discriminators).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: WOMEN TRAPPED IN SUBORDINATION (SNARE) — Subjected to hierarchical structures that appear facially neutral (educational tracking, workplace gatekeeping, caregiving burden allocation) but perpetuate inferior status. Cannot exit without bearing catastrophic personal cost (economic destitution, social exclusion, loss of family bonds). The anti-subordination reading forbids the structures themselves, not merely overt classification. Powerless agents experience the full extractiveness of the hierarchy with no alternatives.
constraint_indexing:constraint_classification(equal_rights_amendment__anti_subordination_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: DISMANTLING COALITION (TANGLED ROPE) — Courts, legislatures, and advocacy groups pursuing substantive equality through anti-subordination doctrine. Benefit from the ERA as a doctrinal tool (strengthens equal protection claims, shifts burden of proof toward dismantling hierarchy). Constrained by institutional inertia (existing legal precedent, political resistance, resource limitations). The constraint is both coordinating (enables organized mobilization around shared hierarchy-dismantling goals) and extractive (requires institutional actors to absorb political cost of challenging entrenched practices).
constraint_indexing:constraint_classification(equal_rights_amendment__anti_subordination_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: COURTS AS COORDINATING INSTITUTIONS (ROPE) — Judicial actors benefit from anti-subordination framing: it provides a principled framework for resolving gender equality cases, establishes clear doctrinal boundaries (hierarchy vs. classification), and creates precedent authority that enhances judicial power. Experience the constraint as primarily coordinating — the doctrine solves the legitimacy problem of deciding which gender-based rules are permissible. Institutional power and arbitrage capacity (can choose between readings or defer to legislature) mean low effective extraction.
constraint_indexing:constraint_classification(equal_rights_amendment__anti_subordination_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: FORMAL EQUALITY TRADITION (PITON) — Traditional sex-blindness doctrine (the sex_blind_reading) appears neutral but is increasingly recognized as performative when applied to structures of subordination. The formal equality framework persists through institutional inertia and doctrinal precedent, but its functional verification capacity has eroded — mechanical application of sex-blindness fails to detect or remedy hierarchy. This perspective views formal equality as a degraded constraint, maintained because the transition to anti-subordination analysis is politically costly and institutionally disruptive, not because the doctrine remains functionally adequate.
constraint_indexing:constraint_classification(equal_rights_amendment__anti_subordination_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: STATE-LEVEL DISMANTLING INITIATIVES (SCAFFOLD) — Legislatures that are actively restructuring laws to dismantle subordination (equal pay laws, paid family leave, caregiving equity provisions) see the anti-subordination reading as enabling temporary measures with sunset rationale. These reforms are scaffolding: they coordinate new institutional arrangements (shared caregiving, economic inclusion) that will eventually become baseline expectations rather than remedial corrections. Low theater because the structural outcome (dismantled hierarchy) is the functional goal, not performative demonstration of commitment.
constraint_indexing:constraint_classification(equal_rights_amendment__anti_subordination_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: INSTITUTIONAL BENEFICIARIES OF STATUS QUO (TANGLED ROPE) — Actors and institutions that benefit from existing hierarchy (gatekeeping professions, family structures built on gendered labor division, industries relying on uncompensated caregiving externalities). The anti-subordination reading permits measures that restructure hierarchy but often coordinates with institutions to ease transition (phase-in periods, grandfathering clauses). Beneficiaries experience both extraction (loss of hierarchy-derived advantage) and coordination benefit (predictable legal framework for adaptation).
constraint_indexing:constraint_classification(equal_rights_amendment__anti_subordination_reading, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a universal standpoint, one can argue that classification and subordination are analytically distinct concepts: classification is a formal property of law (does the text mention sex?), while subordination is a structural property of social relations (does the practice perpetuate inferior status?). From this view, the distinction is not merely doctrinal but ontologically grounded — classification and subordination are separable dimensions. However, the structural data contradicts this: the anti-subordination reading's extractiveness (0.58) and suppression (0.62) reveal that hierarchy is not an immutable natural law but a structured extractive constraint. The analytical observer risks naturalizing the separation between law and hierarchy as an immutable distinction when it is actually a choice about which mechanism to target.
constraint_indexing:constraint_classification(equal_rights_amendment__anti_subordination_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(equal_rights_amendment__anti_subordination_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(equal_rights_amendment__anti_subordination_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(equal_rights_amendment__anti_subordination_reading, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(equal_rights_amendment__anti_subordination_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(equal_rights_amendment__anti_subordination_reading, TR),
    TR >= 0.70.

:- end_tests(equal_rights_amendment__anti_subordination_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The anti-subordination reading targets the extractive mechanism of hierarchy itself rather than merely formal classification, enabling courts and legislatures to identify and restructure practices that maintain inferior status. This increases extractiveness for institutions benefiting from hierarchy (they lose privilege previously treated as natural or neutral) while reducing extractiveness for women trapped in subordination (structures maintaining their inferiority become targetable). The net extractiveness reflects that operationalizing hierarchy dismantling is costlier and more institutionally disruptive than simply requiring classification neutrality. The trajectory (0.42 → 0.68) reflects rising enforcement intensity as courts develop doctrine and legislatures implement dismantling measures. Suppression (0.62): High. The reading requires identifying and challenging deeply embedded structural patterns (division of labor, social norms, institutional gatekeeping) that have been naturalized as inevitable rather than constructed. Suppression mechanisms include: (a) judicial deference to precedent treating sex-blindness as constitutional standard, (b) political resistance from beneficiaries of hierarchy, (c) conceptual difficulty distinguishing hierarchy from inevitable gender differences, (d) institutional inertia in legal education and practice. Theater ratio (0.51): Moderate-low. The anti-subordination reading shows declining theater over time, reflecting genuine structural shifts rather than performative commitment. Early doctrinal development relied more on rhetorical commitment to equality while hierarchy-perpetuating structures persisted (theater-heavy). As courts and legislatures operationalize the reading, actual restructuring increases and theater declines. A theater ratio of 0.51 indicates that roughly half the enforcement activity produces substantive hierarchy dismantling and half remains performative (identification of hierarchy without effective remedy, reversals on appeal, implementation gaps).
 *
 * PERSPECTIVAL GAP:
 *   The anti-subordination reading produces maximal perspectival divergence. Powerless women trapped in subordination experience snare classification (pure extraction from hierarchy with no exit). Organized dismantling coalitions experience tangled rope (coordinating benefit plus institutional cost). Courts experience rope (coordination benefit with no significant extraction). Formal equality doctrine experiences piton (degraded constraint maintained by inertia). State-level dismantling initiatives experience scaffold (temporary measures building toward hierarchy-free baseline). Beneficiaries of hierarchy experience tangled rope (forced concession of privilege alongside coordination of new arrangements). The analytical observer risks mountain classification (naturalizing the classification/subordination distinction), but the structural data reveals this as a false summit: hierarchy is an extractive constraint, not an immutable feature of law or nature. The perspectival gap reveals that the ERA is not a unitary constraint but a contested kernel with different readings instantiating different constraint types depending on institutional commitments.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is determined by each agent's relationship to the hierarchical extraction. Women trapped in subordination have d ≈ 0.95 (full target — they bear the entire weight of hierarchy). Dismantling coalitions have d ≈ 0.55 (victim + organized exit — they absorb institutional costs of dismantling but have capacity to organize and some agency). Courts have d ≈ 0.10 (beneficiary + arbitrage — they benefit from doctrinal clarity and retain option to defer to legislature). Beneficiaries of hierarchy have d ≈ 0.70 (mixed: they are targets of extraction [hierarchy removal] but retain institutional power and can bargain for transition terms). Formal equality doctrine has d ≈ 0.88 (high target — it is progressively displaced by anti-subordination framework). The sigmoid f(d) transforms these values, producing the experienced extractiveness (χ) from which perspectives classify.
 *
 * MANDATROPHY ANALYSIS:
 *   The anti-subordination reading resolves mandatrophy by clarifying that the ERA is not a unitary constraint with a single classification but a contested kernel with multiple legitimate readings. No single type is 'correct' — instead, the type depends on which reading's institutional commitment structure one enters. Within anti-subordination institutional commitment (courts, legislatures, advocacy groups adopting this reading), the constraint is tangled rope: it both coordinates hierarchy dismantling and extracts institutional costs. Within sex-blind institutional commitment (courts or jurisdictions defending formal equality), the constraint is piton: the doctrinal tradition persists through inertia while its functional capacity to address hierarchy erodes. The mandatrophy is resolved by recognizing that different institutional actors are validly instantiating different readings, and the contest between readings is itself a structural feature of constitutional law, not a failure to achieve the true classification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    subordination_operationalization,
    'How does one operationalize ''perpetuates inferior status'' in doctrine without conflating structural outcome with individual intent or formal classification?',
    'Doctrinal mapping of lower courts applying anti-subordination analysis; comparison of outcomes under anti-subordination vs. sex-blind frameworks in identical factual scenarios; analysis of which practices courts identify as hierarchy-perpetuating and which they exempt',
    'If subordination operationalization is vague: courts revert to sex-blind analysis, and anti-subordination reading degrades into performative doctrine (piton). If subordination operationalization is precise: reading enables targeted dismantling of hierarchy while permitting hierarchy-reducing measures that sex-blindness would forbid.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(subordination_operationalization, conceptual, 'Operationalization of ''perpetuates inferior status'' in legal doctrine').

omega_variable(
    dismantling_measures_permissibility,
    'Which affirmative measures aimed at dismantling hierarchy does the anti-subordination reading permit, and what criteria distinguish permissible from impermissible reconstruction?',
    'Analysis of cases approving or invalidating affirmative action, remedial hiring, caregiving restructuring, and other hierarchy-dismantling policies under anti-subordination logic; comparison with sex-blind framework outcomes',
    'If anti-subordination reading permits broad dismantling (substantive equality priority): beneficiaries of status quo face severe extraction and strong incentive to oppose reading. If reading is narrow (hierarchy only when intention to subordinate is clear): dismantling coalitions have limited leverage, and status quo hierarchy persists under guise of formal neutrality.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(dismantling_measures_permissibility, conceptual, 'Scope of permissible affirmative action under anti-subordination reading').

omega_variable(
    relationship_to_sex_blind_precedent,
    'Does the anti-subordination reading foreclose the sex-blind reading by logical contradiction, or do both readings coexist as live doctrinal options held by different courts and parties?',
    'Survey of judicial opinions explicitly comparing anti-subordination and sex-blind frameworks; identification of whether courts treat the readings as mutually exclusive or as alternative tools for different doctrinal contexts',
    'If readings foreclose each other: whichever reading prevails structurally eliminates the other (bifurcation of doctrine). If readings coexist: multiple frameworks operate simultaneously, creating doctrinal instability and venue-shopping opportunities. Coexistence favors status quo (parties choose sex-blind forum); foreclosure favors hierarchy dismantling (single dominant framework applies universally).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(relationship_to_sex_blind_precedent, conceptual, 'Logical relationship between anti-subordination and sex-blind readings').

omega_variable(
    hierarchy_identification_legitimacy,
    'What authority structure legitimately identifies which practices perpetuate hierarchy? Courts interpreting doctrine, legislatures establishing facts, communities of affected women, social science evidence, or some combination?',
    'Analysis of which institutional actors courts defer to when determining whether a practice perpetuates subordination; comparison of results when courts identify hierarchy vs. when legislatures do vs. when affected communities testify',
    'If courts have sole authority: hierarchy identification depends on judicial ideology and precedent, potentially locking in conservative definitions. If legislatures or communities have authority: hierarchy identification is more responsive to structural reality but creates inconsistency across jurisdictions. If social science evidence dominates: hierarchy identification becomes empirical and revisable but may lag real-world structural change.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(hierarchy_identification_legitimacy, empirical, 'Authority structure for identifying hierarchy-perpetuating practices').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(equal_rights_amendment__anti_subordination_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(era_antisub_theater_t0, equal_rights_amendment__anti_subordination_reading, theater_ratio, 0, 0.68).
narrative_ontology:measurement(era_antisub_theater_t25, equal_rights_amendment__anti_subordination_reading, theater_ratio, 25, 0.55).
narrative_ontology:measurement(era_antisub_theater_t50, equal_rights_amendment__anti_subordination_reading, theater_ratio, 50, 0.51).

% Extraction over time
narrative_ontology:measurement(era_antisub_extract_t0, equal_rights_amendment__anti_subordination_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(era_antisub_extract_t25, equal_rights_amendment__anti_subordination_reading, base_extractiveness, 25, 0.58).
narrative_ontology:measurement(era_antisub_extract_t50, equal_rights_amendment__anti_subordination_reading, base_extractiveness, 50, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(era_antisub_suppress_t0, equal_rights_amendment__anti_subordination_reading, suppression_requirement, 0, 0.48).
narrative_ontology:measurement(era_antisub_suppress_t25, equal_rights_amendment__anti_subordination_reading, suppression_requirement, 25, 0.62).
narrative_ontology:measurement(era_antisub_suppress_t50, equal_rights_amendment__anti_subordination_reading, suppression_requirement, 50, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(equal_rights_amendment__anti_subordination_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(equal_rights_amendment__anti_subordination_reading, equal_rights_amendment__sex_blind_reading).
narrative_ontology:affects_constraint(equal_rights_amendment__anti_subordination_reading, strict_scrutiny_gender_classification).
narrative_ontology:affects_constraint(equal_rights_amendment__anti_subordination_reading, family_structure_subordination).
narrative_ontology:affects_constraint(equal_rights_amendment__anti_subordination_reading, workplace_gatekeeping_hierarchy).

% DUAL FORMULATION NOTE:
% The anti-subordination reading and sex-blind reading are sibling constraints within the contested kernel 'equal_rights_amendment'. Each reading instantiates a different constraint with different extractiveness values, different beneficiary/victim structures, and different institutional commitments. The readings do not coexist within a single legal framework — courts adopt one reading or the other, though jurisdictional variation means both remain live at the national level. This constraint (anti-subordination) structurally influences the sex-blind reading by shifting the burden of justification and creating downstream pressure toward hierarchy-focused analysis.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(equal_rights_amendment__anti_subordination_reading, institutional, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
