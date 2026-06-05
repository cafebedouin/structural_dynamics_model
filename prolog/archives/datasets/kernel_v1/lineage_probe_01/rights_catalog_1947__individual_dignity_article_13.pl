% ============================================================================
% CONSTRAINT STORY: rights_catalog_1947__individual_dignity_article_13
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_rights_catalog_1947__individual_dignity_article_13, []).

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
 *   constraint_id: rights_catalog_1947__individual_dignity_article_13
 *   human_readable: Individual Dignity and Personal Autonomy (Article 13, Rights Catalog 1947)
 *   domain: constitutional_law/human_rights_doctrine
 *
 * SUMMARY:
 *   Article 13 of the Rights Catalog (1947) declares that all people are
 *   entitled to respect as individuals and to the right to pursue happiness.
 *   This constraint instantiates one specific reading of a contested
 *   doctrinal kernel — the rights catalog itself — that anchors individual
 *   dignity and personal autonomy as the foundation of constitutional rights
 *   architecture. The reading differs fundamentally from two sibling
 *   readings: Article 24's reconstitution of the family through equal spousal
 *   consent (subordinating family hierarchy to individual choice) and Article
 *   25's promise of social minimum welfare (obligating the state to provide
 *   conditions for dignified existence). This story analyzes Article 13
 *   specifically as the doctrinal anchor from which privacy doctrine,
 *   unenumerated rights jurisprudence, and personal autonomy claims grow. The
 *   constraint exhibits tangled rope structure: genuine coordination function
 *   (enabling privacy doctrine, anchoring judicial authority over personal
 *   autonomy) coexists with asymmetric extraction (family-state legal logic
 *   is suppressed, but the suppression is contested and incompletely
 *   enforced; collective subordination of the individual by family and state
 *   persists despite the clause). The extractiveness has declined over the
 *   20-year interval (0.62 to 0.38) as doctrinal authority has expanded and
 *   welfare systems have begun individualizing. Theater ratio has also
 *   declined (0.72 to 0.58) as the piton phase (family law performing
 *   compliance while maintaining subordination) transitions toward
 *   substantive application. The constraint is best understood as a scaffold
 *   with civilizational endpoint: the individual dignity reading is building
 *   new legal pathways (privacy doctrine, autonomy rights, individualized
 *   welfare) that will eventually replace family-state mediation entirely.
 *
 * KEY AGENTS:
 *   - Individual Subordinated by Family-State Logic: Primary victim (powerless/trapped) — person trapped within family-state structures; no independent legal existence; bears full cost of asserting individual dignity claims
 *   - Unenumerated Rights Claimants: Primary beneficiary (organized/constrained) — privacy advocates, autonomy rights movements, marginalized groups claiming dignity-derived rights; benefit from Article 13 as doctrinal anchor but constrained by interpretive boundaries and institutional resistance
 *   - Courts and Constitutional Authorities: Institutional beneficiary (institutional/arbitrage) — gain authority over privacy and autonomy questions; courts experience Article 13 as enabling doctrinal foundation for judicial review of family and welfare law
 *   - Family Law Administration Machinery: Institutional actor (institutional/arbitrage) — traditional registration systems, inheritance rules, spousal authority frameworks that predate Article 13; persists through institutional inertia; performs compliance while substantively maintaining collectivist subordination (piton phase)
 *   - Welfare State Administrators: Transitional actor (organized/constrained) — redesigning welfare distribution from family-head mediation to individual claimant recognition; managing sunset transition from family-mediated to individualized welfare
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing contingent doctrinal construction as inherent human right; false summit candidate
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(rights_catalog_1947__individual_dignity_article_13, 0.38).
domain_priors:suppression_score(rights_catalog_1947__individual_dignity_article_13, 0.52).
domain_priors:theater_ratio(rights_catalog_1947__individual_dignity_article_13, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(rights_catalog_1947__individual_dignity_article_13, extractiveness, 0.38).
narrative_ontology:constraint_metric(rights_catalog_1947__individual_dignity_article_13, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(rights_catalog_1947__individual_dignity_article_13, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(rights_catalog_1947__individual_dignity_article_13, tangled_rope).
narrative_ontology:human_readable(rights_catalog_1947__individual_dignity_article_13, "Individual Dignity and Personal Autonomy (Article 13, Rights Catalog 1947)").
narrative_ontology:topic_domain(rights_catalog_1947__individual_dignity_article_13, "constitutional_law/human_rights_doctrine").

domain_priors:requires_active_enforcement(rights_catalog_1947__individual_dignity_article_13).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(rights_catalog_1947__individual_dignity_article_13, 'e5a680f6-8300-4bec-89ff-2be03ef09fd6').
narrative_ontology:cs_kernel_codification('e5a680f6-8300-4bec-89ff-2be03ef09fd6', fixed_text).
narrative_ontology:cs_authority_grounding('e5a680f6-8300-4bec-89ff-2be03ef09fd6', lineage).
narrative_ontology:cs_interpretation_layer_present('e5a680f6-8300-4bec-89ff-2be03ef09fd6').
narrative_ontology:cs_reading_relation('e5a680f6-8300-4bec-89ff-2be03ef09fd6', rights_catalog_1947__equality_and_family_article_24, coexists_with).
narrative_ontology:cs_reading_relation('e5a680f6-8300-4bec-89ff-2be03ef09fd6', rights_catalog_1947__social_minimum_article_25, influences).
narrative_ontology:cs_axiom('e5a680f6-8300-4bec-89ff-2be03ef09fd6', foundational, individual_is_unit_of_legal_personality).
narrative_ontology:cs_axiom_status(individual_is_unit_of_legal_personality, holdable).
narrative_ontology:cs_axiom_grounding('e5a680f6-8300-4bec-89ff-2be03ef09fd6', individual_is_unit_of_legal_personality, deontological).
narrative_ontology:cs_axiom('e5a680f6-8300-4bec-89ff-2be03ef09fd6', foundational, autonomy_as_intrinsic_to_dignity).
narrative_ontology:cs_axiom_status(autonomy_as_intrinsic_to_dignity, holdable).
narrative_ontology:cs_axiom_grounding('e5a680f6-8300-4bec-89ff-2be03ef09fd6', autonomy_as_intrinsic_to_dignity, deontological).
narrative_ontology:cs_reference_frame('e5a680f6-8300-4bec-89ff-2be03ef09fd6', individual_dignity_primacy).
narrative_ontology:cs_drift_state('e5a680f6-8300-4bec-89ff-2be03ef09fd6', contemporary_welfare_individualization, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('e5a680f6-8300-4bec-89ff-2be03ef09fd6', '2026-02-26T14:32:00Z').
narrative_ontology:cs_kernel_id(rights_catalog_1947__individual_dignity_article_13, rights_catalog_1947).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(rights_catalog_1947__individual_dignity_article_13, unenumerated_rights_claimants).
narrative_ontology:constraint_beneficiary(rights_catalog_1947__individual_dignity_article_13, privacy_doctrine_jurisprudence).
narrative_ontology:constraint_victim(rights_catalog_1947__individual_dignity_article_13, family_state_legal_logic).
narrative_ontology:constraint_victim(rights_catalog_1947__individual_dignity_article_13, collective_subordination_enforcement).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SUBORDINATED INDIVIDUAL (SNARE) — Person trapped within family-state legal logic that treats the household collective as the unit of legal personality. Individual has no independent legal existence; all rights flow through family head or state welfare designee. Exit is material impossibility within the regime. Experiences maximum extraction: the right to be treated as an individual is denied, and the cost of claiming it is expulsion from legal recognition entirely.
constraint_indexing:constraint_classification(rights_catalog_1947__individual_dignity_article_13, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: PRIVACY RIGHTS MOVEMENT (TANGLED ROPE) — Organized claimants (women's rights advocates, privacy doctrine jurisprudence) benefit from Article 13 as a doctrinal anchor for expanding unenumerated rights. Simultaneously, the constraint extracts costs: mobilizing the individual-dignity reading requires suppressing competing readings (collectivist, family-welfare-centered) and managing the doctrinal instability that arises when individual rights conflict with family unity or state welfare mandates. Mixed coordination and extraction — genuine coordination function (enabling privacy doctrine) coexists with asymmetric extraction (movement bears labor of doctrinal construction while courts/legislatures extract benefit from settled jurisprudence).
constraint_indexing:constraint_classification(rights_catalog_1947__individual_dignity_article_13, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: INSTITUTIONAL ADJUDICATORS (ROPE) — Courts, constitutional scholars, and doctrinal authorities benefit from Article 13 as a stable doctrinal foundation. The clause enables judicial authority over privacy and autonomy questions, generates case law that extends institutional reach, and provides legitimacy for judicial review of family and welfare law. The coordination function is real: Article 13 provides the doctrinal anchor that enables courts to hear these claims at all. Net beneficiary position — extraction runs toward institutional authority, not away. Courts experience the constraint as enabling rather than coercive.
constraint_indexing:constraint_classification(rights_catalog_1947__individual_dignity_article_13, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: FAMILY LAW ADMINISTRATION (PITON) — Traditional family law machinery (registration systems, inheritance rules, spousal authority frameworks) that preceded Article 13 persists through institutional inertia despite the clause's contradiction to family-state logic. The constraint's doctrinal force has not replaced the machinery; instead, the machinery continues on the grounds that 'individual dignity applies only within the family structure,' a performative reading that preserves family subordination under the appearance of compliance. Theater ratio high: the family law system performs compliance with Article 13 while substantively maintaining collectivist subordination. The mechanism persists because alternatives (full individualization of family law) have not fully replaced it.
constraint_indexing:constraint_classification(rights_catalog_1947__individual_dignity_article_13, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: WELFARE STATE REDESIGN (SCAFFOLD) — Social welfare administration that previously distributed benefits through family head (patriarchal welfare model) is being redesigned to recognize individuals as welfare claimants. This transition has a sunset: as welfare systems individualize (direct payments, independent benefit entitlements, non-family-mediated access), the extractive family-welfare mediation mechanism loses force. The scaffold is temporary: it coordinates the transition from family-mediated welfare to individualized welfare while preserving enough family-law continuity to avoid institutional collapse. Once the transition is complete, the extractive suppression of individual welfare claims becomes unnecessary and will atrophy.
constraint_indexing:constraint_classification(rights_catalog_1947__individual_dignity_article_13, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From the civilizational perspective, individual dignity and the capacity for autonomous choice are presented as inherent to the human condition, prior to and independent of any legal system. This reading sees Article 13 as recognition of a pre-political natural right rather than as creation of a new right. Suppression of individual agency by collective subordination appears as a violation of human nature itself. However, the structural data reveals this as a false summit: the constraint's measurable extractiveness (0.38), suppression (0.52), and theater ratio (0.58) indicate that individual dignity is NOT inherent or naturally emergent but rather contested, doctrinally constructed, and actively suppressed by competing legal logics.
constraint_indexing:constraint_classification(rights_catalog_1947__individual_dignity_article_13, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(rights_catalog_1947__individual_dignity_article_13_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(rights_catalog_1947__individual_dignity_article_13, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(rights_catalog_1947__individual_dignity_article_13, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(rights_catalog_1947__individual_dignity_article_13, TR),
    TR >= 0.70.

:- end_tests(rights_catalog_1947__individual_dignity_article_13_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   EXTRACTIVENESS (0.38, declined from 0.62): Moderate declining. The constraint measures the degree to which individual agency is subordinated by family-state legal logic. At t=0 (immediately post-1947), extractiveness was high (0.62) because Article 13 was primarily aspirational — doctrinal force was weak, family-state legal machinery remained dominant, and asserting individual dignity claims carried severe costs (legal exclusion, economic dependency on family head). Over 20 years, doctrinal authority has expanded, courts have recognized privacy and autonomy rights, and welfare systems have begun individualizing. Extractiveness declines as the extractive mechanism (subordination enforced through legal non-recognition) loses efficacy. The trajectory is consistent with a constraint that is being actively transformed by doctrinal construction. SUPPRESSION (0.52, declined from 0.68): Moderate declining. Suppression measures barriers to claiming individual rights: lack of legal standing for individuals (family head holds rights), economic dependency on family unit, social stigma for claiming autonomy against family authority, institutional resistance from family-law machinery. These barriers remain substantial but have weakened as doctrine has shifted and welfare systems have individualized. THEATER RATIO (0.58, declined from 0.72): Moderate declining. Theater measures the performative content of legal compliance. At t=0, family law performed Article 13 compliance ('of course we respect individuals... within family structures') while substantively maintaining subordination — high theater. As substantive doctrinal shifts have occurred (privacy recognized, autonomy protected in family law, welfare individualized), the performance layer has thinned, though it has not disappeared entirely. The theater decline indicates the piton phase is transitioning toward genuine structural replacement.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates stark perspectival divergence across power positions and exit options. The subordinated individual experiences a snare (no exit, maximum extraction, rights denied). The organized rights movement experiences tangled rope (genuine coordination benefit from having an anchor clause, but constrained by doctrinal boundaries and institutional resistance). Courts experience rope (coordination function, net beneficiary). Family-law machinery experiences piton (performing compliance, degraded function, persisting through inertia). Welfare redesign initiatives experience scaffold (temporary transitional coordination with sunset endpoint as individualization completes). The analytical observer risks the false summit — seeing individual dignity as natural law rather than as actively constructed, doctrinally contested, and materially suppressed by institutional machinery. The perspectival gap reveals that 'individual dignity' is not a settled concept but a site of ongoing doctrinal struggle where different institutional actors and claimant groups experience the constraint with radically different extractiveness and classification.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality (d) is derived from the agent's power level, exit options, and structural relationship to the individual-dignity principle. SUBORDINATED INDIVIDUAL: powerless power, trapped exit, victim status → d ≈ 0.95 → high f(d) → high effective extraction experienced. The person has no structural escape from family-state subordination within the regime. RIGHTS MOVEMENT: organized power, constrained exit, beneficiary status → d ≈ 0.45 → moderate f(d) → moderate extraction despite beneficiary position because the movement's capacity to claim benefits is constrained by doctrinal boundaries and institutional gatekeeping. COURTS: institutional power, arbitrage exit, beneficiary status → d ≈ 0.08 → negative f(d) → negative/minimal extraction (subsidy effect) because institutional authority gains jurisdiction and doctrinal legitimacy. FAMILY-LAW MACHINERY: institutional power, arbitrage exit, victim status (subordination is the target of the constraint) → d ≈ 0.65 → moderate f(d); but the piton classification derives from theater (0.58) rather than chi, so effective extraction is masked by performative compliance. WELFARE REDESIGN: organized power, constrained exit, beneficiary status but with sunset → d ≈ 0.50 → moderate f(d); the scaffold classification reflects that this agent has agency and a defined exit path (completion of individualization). ANALYTICAL OBSERVER: analytical power, analytical exit → d ≈ 0.73 (canonical for analytical) → high f(d) but classification is mountain (false summit candidate), not derived from chi.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    unenumerated_rights_scope_boundary,
    'What counts as an ''unenumerated right'' derivable from Article 13''s individual dignity principle? Where is the boundary between legitimate privacy doctrine and unwarranted judicial expansion?',
    'Comparative constitutional jurisprudence: analysis of which rights have been recognized in different jurisdictions, patterns of recognition, and the doctrinal limits courts have set; identification of rejected unenumerated claims and their reasoning',
    'If boundary is narrow (only privacy, autonomy, bodily integrity): Article 13 functions as a focused constitutional anchor with limited extractiveness. If boundary is expansive (lifestyle choice, dignity in recognition, relational autonomy): Article 13 becomes an open doctrinal frontier with higher extractiveness as different movements compete to define the principle.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(unenumerated_rights_scope_boundary, conceptual, 'Scope and boundaries of unenumerated rights derivable from individual dignity principle').

omega_variable(
    family_state_logic_compatibility,
    'Is Article 13''s individual dignity principle compatible with family-law structures that recognize family unit legal personality, or does individual dignity necessarily foreclose family-mediated rights, duties, and welfare?',
    'Doctrinal analysis: whether courts have found coherent frameworks that honor both individual autonomy and family unit structure; empirical examination of which family-law jurisdictions have attempted dual-recognition and their outcomes; analysis of irreducible conflicts between individual and family-unit claims',
    'If compatible: the piton perspective (performative family-law compliance) is sustainable, and suppression of individual claims within family law is reformable rather than structural. If incompatible: individual dignity reading FORECLOSES family-state legal logic, and the tangled rope resolves into a snare as the family-state system defends itself through doctrinal suppression.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(family_state_logic_compatibility, conceptual, 'Logical compatibility between individual dignity and family-unit legal structures').

omega_variable(
    positive_vs_negative_dignity_obligation,
    'Does Article 13 create only negative rights (freedom from subordination, freedom from state interference) or also positive rights (state obligation to enable individual autonomy, provide resources for dignity, ensure substantive equality)?',
    'Jurisprudential analysis: what obligations courts have read into Article 13; comparison with Article 25 (social minimum/welfare rights) to determine whether positive dignity obligations are separate claims or derivable from Article 13 itself; fiscal and administrative impact studies of positive dignity interpretations',
    'If negative only: extractiveness remains moderate (suppression suppressed but no affirmative duties); state''s role is permissive. If positive: extractiveness increases (state obligated to actively construct individual autonomy); welfare redesign (scaffold perspective) becomes mandatory rather than optional; resource distribution becomes justiciable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(positive_vs_negative_dignity_obligation, conceptual, 'Whether Article 13 imposes negative or positive obligations on the state').

omega_variable(
    reading_foreclosure_relation_to_article_24,
    'Does the individual dignity reading of Article 13 foreclose the family-centered reading instantiated in Article 24, or can both readings coexist with different scopes of application?',
    'Constitutional interpretation doctrine: analysis of how courts have handled potential conflicts between individual autonomy (Article 13) and family rights (Article 24); examination of whether ''individual within family context'' is a coherent jurisprudential position or a performative compromise masking genuine conflict',
    'If they foreclose: this reading (Article 13) and the Article 24 reading are in logically incompatible positions; one framework cannot hold both. If they coexist: both readings remain live options, and the constraint oscillates between perspectives depending on which frame is activated. Determines the reading_relations value for the Article 24 sibling.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_foreclosure_relation_to_article_24, conceptual, 'Whether Article 13 individual dignity reading logically forecloses Article 24 family-centered reading').

omega_variable(
    doctrine_performance_vs_substance_gap,
    'Does the persistent family-law machinery represent genuine institutional lag (piton: ritual performing compliance) or successful institutional resistance (the family-state logic has successfully subordinated individual dignity doctrine to family-unit authority)?',
    'Empirical analysis of case outcomes: do individual autonomy claims succeed or fail when brought against family-law machinery? Measurement of substitution rate — how often do courts invoke Article 13 to override family-law defaults vs how often family law prevails despite Article 13 invocation? Generational change: does the piton trajectory show declining family-law theater, or is theater stable or increasing?',
    'If genuine lag: piton classification confirmed, theater will decline over time, individual dignity doctrine will eventually replace family-law machinery. If resistance is successful: the piton is mislabeled; family-law machinery is the active equilibrium, Article 13 is the performative overlay, and the theater ratio will not decline without active intervention.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(doctrine_performance_vs_substance_gap, empirical, 'Whether family-law machinery persistence is lag or active institutional resistance').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(rights_catalog_1947__individual_dignity_article_13, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(rights_1947_art13_tr_t0, rights_catalog_1947__individual_dignity_article_13, theater_ratio, 0, 0.72).
narrative_ontology:measurement(rights_1947_art13_tr_t10, rights_catalog_1947__individual_dignity_article_13, theater_ratio, 10, 0.65).
narrative_ontology:measurement(rights_1947_art13_tr_t20, rights_catalog_1947__individual_dignity_article_13, theater_ratio, 20, 0.58).

% Extraction over time
narrative_ontology:measurement(rights_1947_art13_be_t0, rights_catalog_1947__individual_dignity_article_13, base_extractiveness, 0, 0.62).
narrative_ontology:measurement(rights_1947_art13_be_t10, rights_catalog_1947__individual_dignity_article_13, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(rights_1947_art13_be_t20, rights_catalog_1947__individual_dignity_article_13, base_extractiveness, 20, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(rights_1947_art13_su_t0, rights_catalog_1947__individual_dignity_article_13, suppression_requirement, 0, 0.68).
narrative_ontology:measurement(rights_1947_art13_su_t10, rights_catalog_1947__individual_dignity_article_13, suppression_requirement, 10, 0.6).
narrative_ontology:measurement(rights_1947_art13_su_t20, rights_catalog_1947__individual_dignity_article_13, suppression_requirement, 20, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(rights_catalog_1947__individual_dignity_article_13, identity_coordination).
narrative_ontology:affects_constraint(rights_catalog_1947__individual_dignity_article_13, rights_catalog_1947__equality_and_family_article_24).
narrative_ontology:affects_constraint(rights_catalog_1947__individual_dignity_article_13, rights_catalog_1947__social_minimum_article_25).
narrative_ontology:affects_constraint(rights_catalog_1947__individual_dignity_article_13, privacy_doctrine_unenumerated_rights).

% DUAL FORMULATION NOTE:
% The rights catalog kernel (1947) contains three structurally distinct constraints corresponding to three readings: individual dignity (Article 13), family equality (Article 24), and social welfare (Article 25). Each reading has its own extractiveness, beneficiary/victim structure, and institutional carriers. They are linked as a constraint family because they share a common kernel (the rights catalog itself) but differ in which aspects of the kernel's authority and doctrine they emphasize. Article 13 is upstream to Article 25 (individual dignity provides the doctrinal basis for individualized welfare rights) and in tension with Article 24 (both claim authority over the individual-family boundary but from opposite directions). The three stories together form the complete picture of the kernel's contradictory commitments.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(rights_catalog_1947__individual_dignity_article_13, analytical, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
