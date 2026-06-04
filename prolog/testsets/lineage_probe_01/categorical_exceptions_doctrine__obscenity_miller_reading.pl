% ============================================================================
% CONSTRAINT STORY: categorical_exceptions_doctrine__obscenity_miller_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_categorical_exceptions_doctrine__obscenity_miller_reading, []).

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
 *   constraint_id: categorical_exceptions_doctrine__obscenity_miller_reading
 *   human_readable: Miller Obscenity Doctrine: Categorical Exception by Community Standards
 *   domain: legal/constitutional/first_amendment
 *
 * SUMMARY:
 *   The Miller obscenity doctrine (Miller v. California, 413 U.S. 15 [1973])
 *   is a doctrinal reading of the categorical exception to First Amendment
 *   protection — one of several competing readings of how free speech
 *   doctrine should handle sexual expression. Miller established a three-part
 *   test: material is obscene (and thus outside protection) if (1) the
 *   average person applying contemporary community standards would find it
 *   appeals predominantly to prurient interest, (2) it depicts sexual conduct
 *   in a patently offensive way, and (3) it lacks serious literary, artistic,
 *   political, or scientific value. This constraint represents one
 *   institutional choice for policing the boundary of protected speech. The
 *   reading instantiates a federal structure: prosecution discretion flows to
 *   local authorities with permission to apply their own community standards,
 *   while the Supreme Court maintains appellate review (and jury deference)
 *   over factual findings. The Miller reading coexists with two sibling
 *   readings within the categorical exception doctrine: the Brandenburg
 *   incitement reading (speech becomes unprotected only when directed to
 *   imminent lawless action and likely to incite it) and the Stevens
 *   no-new-categories reading (the list of categorical exceptions is closed;
 *   new harms do not create new unprotected categories). These three readings
 *   compete in constitutional law, but Miller survives institutionally
 *   despite widespread judicial dissatisfaction with its internal coherence —
 *   the three-part test nobody loves persists through a combination of
 *   precedent, federalism structure, and organized constituency pressure.
 *
 * KEY AGENTS:
 *   - Borderline Expression Creators: Artists, authors, performers producing sexual content, erotica, avant-garde work near the Miller boundary (powerless/trapped) — face prosecution and marketplace exclusion with no exit path
 *   - Expression Distributors: Publishers, bookstores, online platforms, video producers (moderate/constrained) — experience mixed costs (compliance, geographic variation) and benefits (categorical clarity reducing ambiguity)
 *   - Local Enforcement Authorities: District attorneys, sheriffs, obscenity units (institutional/arbitrage) — primary beneficiaries gaining prosecutorial discretion and constituency responsiveness
 *   - Major Publishers and Studios: Large media corporations (powerful/mobile) — can avoid Miller risk through market segmentation and platform differentiation; experience rule as coordination rather than extraction
 *   - Federal Appellate Courts: Judges applying Miller in appellate review (institutional/constrained) — perform ritual deference to jury verdicts; experience the doctrine as performatively degraded but institutionally sticky
 *   - Supreme Court (Analytical Observer): The framing authority that chose Miller as one possible reading of the categorical exception — risks naturalizing institutional choice as constitutional necessity
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(categorical_exceptions_doctrine__obscenity_miller_reading, 0.52).
domain_priors:suppression_score(categorical_exceptions_doctrine__obscenity_miller_reading, 0.68).
domain_priors:theater_ratio(categorical_exceptions_doctrine__obscenity_miller_reading, 0.61).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(categorical_exceptions_doctrine__obscenity_miller_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(categorical_exceptions_doctrine__obscenity_miller_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(categorical_exceptions_doctrine__obscenity_miller_reading, theater_ratio, 0.61).

% --- Constraint claim ---
narrative_ontology:constraint_claim(categorical_exceptions_doctrine__obscenity_miller_reading, tangled_rope).
narrative_ontology:human_readable(categorical_exceptions_doctrine__obscenity_miller_reading, "Miller Obscenity Doctrine: Categorical Exception by Community Standards").
narrative_ontology:topic_domain(categorical_exceptions_doctrine__obscenity_miller_reading, "legal/constitutional/first_amendment").

domain_priors:requires_active_enforcement(categorical_exceptions_doctrine__obscenity_miller_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(categorical_exceptions_doctrine__obscenity_miller_reading, 'ad2f62eb-f591-4665-814a-ff2186203194').
narrative_ontology:cs_kernel_codification('ad2f62eb-f591-4665-814a-ff2186203194', formalized).
narrative_ontology:cs_authority_grounding('ad2f62eb-f591-4665-814a-ff2186203194', extraction).
narrative_ontology:cs_interpretation_layer_present('ad2f62eb-f591-4665-814a-ff2186203194').
narrative_ontology:cs_reading_relation('ad2f62eb-f591-4665-814a-ff2186203194', categorical_exceptions_doctrine__incitement_brandenburg_reading, coexists_with).
narrative_ontology:cs_reading_relation('ad2f62eb-f591-4665-814a-ff2186203194', categorical_exceptions_doctrine__no_new_categories_reading, coexists_with).
narrative_ontology:cs_axiom('ad2f62eb-f591-4665-814a-ff2186203194', foundational, community_standards_localism_legitimate).
narrative_ontology:cs_axiom_status(community_standards_localism_legitimate, holdable).
narrative_ontology:cs_axiom_grounding('ad2f62eb-f591-4665-814a-ff2186203194', community_standards_localism_legitimate, conventional).
narrative_ontology:cs_axiom('ad2f62eb-f591-4665-814a-ff2186203194', secondary, prurience_offensiveness_value_test_operationalizes_boundary).
narrative_ontology:cs_axiom_status(prurience_offensiveness_value_test_operationalizes_boundary, holdable).
narrative_ontology:cs_axiom_grounding('ad2f62eb-f591-4665-814a-ff2186203194', prurience_offensiveness_value_test_operationalizes_boundary, deontological).
narrative_ontology:cs_reference_frame('ad2f62eb-f591-4665-814a-ff2186203194', federalist_jury_deference_framework).
narrative_ontology:cs_drift_state('ad2f62eb-f591-4665-814a-ff2186203194', contemporary_digital_distribution_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('ad2f62eb-f591-4665-814a-ff2186203194', '2026-02-26T00:00:00Z').
narrative_ontology:cs_kernel_id(categorical_exceptions_doctrine__obscenity_miller_reading, categorical_exceptions_doctrine).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(categorical_exceptions_doctrine__obscenity_miller_reading, community_standards_regulation).
narrative_ontology:constraint_beneficiary(categorical_exceptions_doctrine__obscenity_miller_reading, local_enforcement_authorities).
narrative_ontology:constraint_victim(categorical_exceptions_doctrine__obscenity_miller_reading, borderline_expression_creators).
narrative_ontology:constraint_victim(categorical_exceptions_doctrine__obscenity_miller_reading, expression_distributors).
narrative_ontology:constraint_victim(categorical_exceptions_doctrine__obscenity_miller_reading, marginalized_cultural_producers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: BORDERLINE EXPRESSION CREATOR (SNARE) — Creators of material near the Miller boundary (erotic art, adult comics, sexual satire, avant-garde literature) face legal jeopardy and cannot exit the regime. The standard is both rigid (three-part test) and indeterminate (community standards, prurience, serious value are jury facts not legal rules). Maximum extraction: prosecution costs, marketplace exclusion, self-censorship under uncertainty. No coordination function benefits this agent.
constraint_indexing:constraint_classification(categorical_exceptions_doctrine__obscenity_miller_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: DISTRIBUTOR OR PLATFORM HOST (TANGLED ROPE) — Publishers, bookstores, online platforms, and video distributors experience mixed costs and benefits. They benefit from the categorical clarity that obscenity is outside protection (reduces total litigation surface). But they bear high compliance costs: geographic variation in community standards, jury unpredictability, and the need for content review infrastructure. Significant extraction but some coordination function (the category does reduce ambiguity at the margins). Exit is constrained by liability rules and business model dependence on platform access.
constraint_indexing:constraint_classification(categorical_exceptions_doctrine__obscenity_miller_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: LOCAL ENFORCEMENT AUTHORITY (ROPE) — District attorneys, sheriffs, and obscenity prosecutors see the Miller rule as enabling coordination: it provides prosecutorial discretion (different communities have different standards) while maintaining a formal legal boundary. Benefits: legitimacy of local enforcement, constituency responsiveness, operational flexibility. Extraction is minimal for this agent — they are beneficiaries. The rule hands them prosecutorial leverage while protecting them from federal override.
constraint_indexing:constraint_classification(categorical_exceptions_doctrine__obscenity_miller_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 4: MAJOR PUBLISHER OR STUDIO (TANGLED_ROPE/MOBILE) — Large media corporations can largely avoid Miller prosecution through market segmentation: they distribute borderline content to specialized platforms (subscription services, adult channels), mainstream content to broadcast networks. They experience the rule as coordination (predictability) rather than extraction because they have capacity to operate in multiple jurisdictions. Some extraction occurs at the margins (self-censorship, geographic compliance costs), but mobility softens it. Coordination function is real: the rule enables market segmentation without creating a uniform suppression regime.
constraint_indexing:constraint_classification(categorical_exceptions_doctrine__obscenity_miller_reading, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a universal civilizational view, some boundary between protected speech and unprotected obscenity is an inherent feature of any legal system. The view naturalizes the Miller standard as a rational equilibrium: communities must be able to suppress material that violates local sexual norms, yet protection for speech about sex remains. However, this mountain is a false summit. The Miller rule is a doctrinal choice (emerged 1973) among alternative framings (no category at all, broad balancing test, national standard). The analytical observer risks naturalizing contingent doctrine as natural law.
constraint_indexing:constraint_classification(categorical_exceptions_doctrine__obscenity_miller_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 6: FEDERAL APPELLATE COURT SYSTEM (PITON) — Federal judges applying Miller experience the doctrine as largely performative. The three-part test provides ritual deference to jury verdicts while rarely overturning convictions. Theater_ratio is high (0.68): judges hold mandatory jury deference on the factual elements (prurience, offensiveness, community standards), making appellate review largely formalistic. The doctrine persists through institutional inertia — it is theoretically coherent but institutionally degraded. Judges see the rule as both necessary (to maintain federalism) and dysfunctional (jury geography determines First Amendment protection), but lack doctrine to revise it.
constraint_indexing:constraint_classification(categorical_exceptions_doctrine__obscenity_miller_reading, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(categorical_exceptions_doctrine__obscenity_miller_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(categorical_exceptions_doctrine__obscenity_miller_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(categorical_exceptions_doctrine__obscenity_miller_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(categorical_exceptions_doctrine__obscenity_miller_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(categorical_exceptions_doctrine__obscenity_miller_reading, TR),
    TR >= 0.70.

:- end_tests(categorical_exceptions_doctrine__obscenity_miller_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The Miller rule extracts value from borderline expression creators through prosecution risk, marketplace exclusion, and self-censorship under legal uncertainty. The three-part test creates a chilling effect: creators cannot predict how a jury in jurisdiction X will assess prurience, offensiveness, or value for their material. This extraction accrues to prosecutors (via conviction leverage) and to creators of mainstream content (who avoid the borderline zone and thus face reduced competition). The extractiveness is not maximal because: (1) the serious value element provides a real, if uncertain, defense; (2) major distributors can segment markets and avoid the worst zones; (3) appellate review creates some reversal probability. Suppression (0.68): High. The rule suppresses borderline expression through legal prohibition, prosecution threat, and jury unpredictability. Creators and small distributors cannot exit the rule's jurisdiction without losing market access. The suppression is structural (written law) and incentive-based (the jeopardy of prosecution). Theater ratio (0.61): Moderate-high. The appellate application of Miller is substantially performative. Judges apply mandatory jury deference to factual findings (prurience, offensiveness, community standards), which means appellate review rarely overturns jury verdicts. The ritual of three-part analysis occurs, but the real decision power resides in jury composition and prosecutor venue selection. The theater has increased over 50 years as judicial dissatisfaction has grown while the doctrine persists. Claimed type: Tangled Rope. The constraint exhibits both genuine coordination (categories reduce ambiguity for mainstream expression; federalism structure enables local responsiveness) and asymmetric extraction (the boundary zone is extracted from through uncertainty). The constraint requires active enforcement through prosecutorial and jury action. Beneficiaries exist (local authorities, mainstream creators). Victims exist (borderline creators, small distributors).
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates a core perspectival gap between the beneficiary (local enforcement) and victims (borderline creators). The beneficiary sees coordination: a rule that enables prosecutorial discretion while providing formal legal structure. The victim sees extraction: a rule designed to eliminate competitors in the cultural marketplace through legal jeopardy. The federal appellate courts see theater: a doctrine that provides ritual legitimacy while actual power flows through jury composition and prosecutor discretion. The major distributors see a mobile resource: they can navigate the rule through market segmentation. The analytical observer risks seeing a natural law: the conviction that communities must maintain some power to suppress obscenity naturalizes what is actually an institutional choice (the Miller reading) among alternatives (Brandenburg, no-new-categories, or a balancing standard). The perspectival gap is maximal between the trapped borderline creator (snare) and the arbitrage-enabled prosecutor (rope).
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values for each agent are derived from their structural position relative to the constraint. Local enforcement authorities are beneficiaries with arbitrage options (they can choose which cases to prosecute, which jurisdictions to focus on) — derived d ≈ 0.15, f(d) negative or near-zero, resulting in rope classification and minimal experienced extraction. Borderline expression creators are victims with trapped exit options (cannot exit the law without abandoning their art form and location) — derived d ≈ 0.95, f(d) ≈ 1.42, resulting in snare classification and maximum experienced extraction. Distributors are mixed: partial beneficiary (clarity on mainstream content), partial victim (compliance burden) with constrained exit (can move to subscription platforms but not fully escape jurisdiction) — derived d ≈ 0.55, f(d) ≈ 0.75, resulting in tangled rope and moderate experienced extraction. Major publishers with mobile options (arbitrage) experience lower d because they can segment markets — derived d ≈ 0.35, f(d) ≈ 0.35, resulting in tangled rope or rope classification. The scope modifier σ(S) applies: regional scope (Miller standards vary by jurisdiction) gives σ ≈ 0.9, dampening chi slightly compared to national (σ=1.0).
 *
 * MANDATROPHY ANALYSIS:
 *   The Miller constraint resolves mandatrophy by acknowledging that the three-part test is both functionally incoherent (nobody loves the definition) and institutionally stable (the category persists despite dissatisfaction). Mandatrophy asks: 'Is this coordination or extraction disguised as coordination?' Miller exhibits both. The coordination function is real: the categorical clarity does reduce ambiguity for mainstream expression and enables federalism. The extraction function is equally real: the boundary zone is harvested through legal uncertainty and jury geography. The constraint is not a false choice between types. Rather, it is a legitimate tangled rope that bundles coordination (reducing total ambiguity) with extraction (harvesting the borderline). The piton perspective (federal courts) shows why the doctrine survives despite internal dissatisfaction: it provides ritual legitimacy while distributing actual power to local actors (juries, prosecutors) in ways that federal judges cannot override without revising the whole structure. The mandatrophy is thus resolved by the honest acknowledgment that the constraint serves both functions and that neither function can be isolated without destroying the whole institutional arrangement.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    community_standards_geographic_arbitrariness,
    'Are ''community standards'' a meaningful constraint on obscenity prosecution, or a doctrine that permits arbitrary variation in First Amendment protection based on jury location?',
    'Empirical analysis: comparison of obscenity conviction rates and jury verdicts across jurisdictions for identical materials; documentation of prosecutor venue-shopping behavior; appellate reversal rates for jury verdicts on community standards findings',
    'If meaningful constraint: Miller maintains federalism while preventing total suppression (tangled_rope classification stable). If arbitrary: the doctrine is extracted value for prosecutors using jury geography as a weapon (snare classification from distributor perspective strengthens).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(community_standards_geographic_arbitrariness, empirical, 'Whether community standards doctrine constrains or weaponizes prosecution').

omega_variable(
    serious_value_as_escape_hatch,
    'Does the ''serious literary, artistic, political, or scientific value'' element provide genuine protection for borderline materials, or is it absorbed into the prurience and offensiveness gates such that few materials survive?',
    'Doctrinal history: count materials found obscene despite value claims; analyze appellate reasoning on value element; map categories of expression (sexual satire, erotic art, avant-garde literature) and their historic treatment under the value prong',
    'If genuine protection: the value element limits extraction and tangled_rope classification holds. If absorbed: the value element is performative theater, and extractiveness should be higher (0.52 → 0.65+), reclassifying the constraint more toward snare from distributor and creator perspectives.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(serious_value_as_escape_hatch, empirical, 'Functional scope of the serious value defense').

omega_variable(
    miller_rule_doctrinal_contingency,
    'Is the Miller three-part test a necessary reading of the categorical exception, or one contingent choice among defensible alternatives (national standard, balancing test, no category)?',
    'Jurisprudential analysis: document competing doctrinal proposals (Roth-Memoirs, balancing proposals, no-category proposals); assess whether Miller''s three parts are compelled by constitutional text or chosen for policy reasons; map other democracies'' obscenity doctrines',
    'If necessary: Miller reading forecloses alternatives; the constraint is structurally inevitable given the categorical exception. If contingent: the reading coexists with alternatives; other readings remain live and the Miller reading is one institutional choice, not a natural law.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(miller_rule_doctrinal_contingency, conceptual, 'Whether Miller three-part test is doctrinal necessity or institutional choice').

omega_variable(
    obscenity_category_survival_legitimacy,
    'What sustains the obscenity category''s legitimacy despite the doctrine''s internal incoherence (nobody loves the definition, yet the category persists)?',
    'Institutional analysis: track judicial statements about dissatisfaction with Miller; examine proposals for revision and why they fail; analyze non-legal pressures (religious constituencies, federalism structure, path dependence) sustaining the category',
    'If legitimacy rests on formal doctrine: the rule is teachable and stable, tangled_rope classification reflects functional reality. If legitimacy rests on non-legal pressure (constituency demand, institutional inertia): the piton perspective becomes more prominent, suggesting the constraint is degraded theater maintained by forces outside doctrine.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(obscenity_category_survival_legitimacy, empirical, 'Sources of the obscenity category''s institutional survival despite doctrinal dissatisfaction').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(categorical_exceptions_doctrine__obscenity_miller_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(obscenity_miller_tr_t0, categorical_exceptions_doctrine__obscenity_miller_reading, theater_ratio, 0, 0.45).
narrative_ontology:measurement(obscenity_miller_tr_t10, categorical_exceptions_doctrine__obscenity_miller_reading, theater_ratio, 10, 0.55).
narrative_ontology:measurement(obscenity_miller_tr_t20, categorical_exceptions_doctrine__obscenity_miller_reading, theater_ratio, 20, 0.61).

% Extraction over time
narrative_ontology:measurement(obscenity_miller_be_t0, categorical_exceptions_doctrine__obscenity_miller_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(obscenity_miller_be_t10, categorical_exceptions_doctrine__obscenity_miller_reading, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(obscenity_miller_be_t20, categorical_exceptions_doctrine__obscenity_miller_reading, base_extractiveness, 20, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(obscenity_miller_su_t0, categorical_exceptions_doctrine__obscenity_miller_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement(obscenity_miller_su_t10, categorical_exceptions_doctrine__obscenity_miller_reading, suppression_requirement, 10, 0.64).
narrative_ontology:measurement(obscenity_miller_su_t20, categorical_exceptions_doctrine__obscenity_miller_reading, suppression_requirement, 20, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(categorical_exceptions_doctrine__obscenity_miller_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(categorical_exceptions_doctrine__obscenity_miller_reading, categorical_exceptions_doctrine__incitement_brandenburg_reading).
narrative_ontology:affects_constraint(categorical_exceptions_doctrine__obscenity_miller_reading, categorical_exceptions_doctrine__no_new_categories_reading).

% DUAL FORMULATION NOTE:
% The Miller obscenity reading is part of the categorical_exceptions_doctrine constraint family. Each reading (Brandenburg incitement, Stevens no-new-categories, Miller obscenity) represents a different institutional choice for policing the boundary between protected and unprotected speech. They share the same kernel (categorical exception exists) but diverge on authority grounding, reference frame, and drift state. The family includes three separate constraint stories linked by affects_constraints. Each story has its own ε value reflecting different levels of extractiveness: Brandenburg incitement has lower extractiveness (the boundary is tight and judicially policed), Stevens no-new-categories has minimal extractiveness (the category is closed), and Miller obscenity has moderate-high extractiveness (community standards create geographic arbitrariness). The constraint family decomposes because each reading has a different structure of beneficiaries, victims, and enforcement mechanisms.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(categorical_exceptions_doctrine__obscenity_miller_reading, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
