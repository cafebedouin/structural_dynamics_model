% ============================================================================
% CONSTRAINT STORY: study_as_performance
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-27
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_study_as_performance, []).

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
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: study_as_performance
 *   human_readable: Study of Sacrifice Law as Performance of Divine Commandment
 *   domain: religious_studies/halakhic_theory
 *
 * SUMMARY:
 *   This constraint instantiates ONE READING of the contested kernel
 *   'sacrifice_commandment.' The reading is: intellectual engagement with
 *   halakhic texts concerning the Temple sacrifices constitutes the
 *   performance of a divine obligation; to study the laws of sacrifice is to
 *   fulfill the commandment to bring sacrifices. This reading emerged as a
 *   creative theological response to the destruction of the Second Temple in
 *   70 CE. When the Temple ceased to exist, the physical performance of
 *   sacrifice became impossible for diaspora Jewry. The rabbinical
 *   authorities reinterpreted the commandment through the category of study:
 *   the scholar who engages intellectually with the sacrifice laws fulfills
 *   the obligation originally performed by the priest in the Temple. The
 *   constraint is zero-extractiveness pure coordination at its origin (70
 *   CE): it solved a genuine collective action problem (how to maintain legal
 *   and ritual knowledge when the external referent is destroyed) with
 *   minimal overhead. Over two millennia, the reading's theater ratio has
 *   risen from 0.0 to 0.70 as the original generative crisis has become
 *   historically distant. The rise in theater does not indicate rising
 *   extraction—extractiveness remains zero—but rather the increasing
 *   performative character of the reading's invocation: the theological
 *   innovation that was urgent and creative in the first century has become a
 *   formal obligation maintained through institutional continuity rather than
 *   living theological engagement. This constraint must be read alongside its
 *   sibling readings: 'performance_only' (the commandment requires actual
 *   sacrifice and cannot be fulfilled through study) and
 *   'archive_maintenance' (study preserves knowledge for potential future use
 *   but does not itself fulfill the commandment). These three readings
 *   partition the logical space of responses to the Temple destruction and
 *   represent live theological positions held by different streams within
 *   Judaism and Christianity.
 *
 * KEY AGENTS:
 *   - Scholar-Worshipper (moderate/mobile): The individual who engages with halakhic texts and experiences the study as direct fulfillment of divine obligation. Structurally both beneficiary and agent; extractiveness is zero because the constraint aligns with intrinsic motivation.
 *   - Yeshiva Institution (institutional/constrained): The transmission structure that maintains the textual tradition and the interpretive framework across generations. Benefits from coordination function (continuity of practice, identity maintenance); genuinely coordinates diaspora learning across time and space.
 *   - Halakhic Authority (powerful/arbitrage): The authority structure that maintains and enforces the reading through interpretive tradition. Sees the reading as having a sunset clause implicit in its origin (contingent on Temple absence). Can exit through declaration of obsolescence but maintains the reading through active enforcement.
 *   - Diasporic Jewish Polity (powerless/trapped): The collective that cannot rebuild the Temple or choose alternative frameworks; experiences the reading as an inescapable constraint framed as fulfillment. Trapped in the reading's logic: exit requires abandoning group identity or adopting a non-halakhic framework.
 *   - Rabbinical Interpretive Authority (institutional/constrained): The active maintainer of the reading who benefits from its coordination function and from the authority it concentrates. Constrained by its own framework—cannot declare the reading obsolete without self-rupture.
 *   - Analytical Observer (analytical/analytical): Sees the reading as either pure coordination (Rope, systems-theory view) or institutional inertia (Piton, historical degradation view). The two analytical perspectives represent genuine alternative framings of the same structural phenomenon.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(study_as_performance, 0.0).
domain_priors:suppression_score(study_as_performance, 0.0).
domain_priors:theater_ratio(study_as_performance, 0.0).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(study_as_performance, extractiveness, 0.0).
narrative_ontology:constraint_metric(study_as_performance, suppression_requirement, 0.0).
narrative_ontology:constraint_metric(study_as_performance, theater_ratio, 0.0).

% --- Constraint claim ---
narrative_ontology:constraint_claim(study_as_performance, rope).
narrative_ontology:human_readable(study_as_performance, "Study of Sacrifice Law as Performance of Divine Commandment").
narrative_ontology:topic_domain(study_as_performance, "religious_studies/halakhic_theory").

% --- Commitment system structure ---
narrative_ontology:cs_kernel_codification(study_as_performance, fixed_text).
narrative_ontology:cs_authority_grounding(study_as_performance, lineage).
narrative_ontology:cs_interpretation_layer_present(study_as_performance).
narrative_ontology:cs_kernel_id(study_as_performance, sacrifice_commandment).
narrative_ontology:cs_reading_relation(study_as_performance, performance_only, coexists_with).
narrative_ontology:cs_reading_relation(study_as_performance, archive_maintenance, coexists_with).
narrative_ontology:cs_axiom(study_as_performance, foundational, study_fulfills_sacrifice_commandment).
narrative_ontology:cs_axiom_status(study_fulfills_sacrifice_commandment, holdable).
narrative_ontology:cs_axiom(study_as_performance, secondary, diaspora_existence_is_permanent_condition).
narrative_ontology:cs_axiom_status(diaspora_existence_is_permanent_condition, holdable).
narrative_ontology:cs_reference_frame(study_as_performance, diaspora_continuity_through_study).
narrative_ontology:cs_drift_state(study_as_performance, contemporary_post_industrial_late_modernity, gap(practice_drift, substantial, true)).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(study_as_performance, scholar_worshipper).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ENGAGED HALAKHIC SCHOLAR (ROPE) — Views intellectual engagement with sacrifice law as direct fulfillment of divine obligation (study=worship). No extraction experienced; the constraint is pure coordination between the scholar's internal orientation and the commandment structure. The scholar is both beneficiary and agent; exit is mobile but not desired because the constraint aligns with intrinsic motivation. The scholar perceives the study mandate as enabling rather than constraining.
constraint_indexing:constraint_classification(study_as_performance, rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(local))).

% PERSPECTIVE 2: YESHIVA INSTITUTION (ROPE) — The institution coordinates transmission of textual tradition across generations. Study of sacrifice law maintains the conceptual framework even in the post-Temple period. The institution benefits from this coordination (continuity of practice, identity maintenance); scholars benefit from the structural framework for meaningful engagement. No asymmetric extraction; the constraint solves a collective action problem: how to preserve juridical knowledge when the external referent (the Temple) no longer exists. Suppression is minimal — scholars can exit (mobile option exists) but institutional continuity constrains the generational horizon.
constraint_indexing:constraint_classification(study_as_performance, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: HALAKHIC AUTHORITY / RESTRUCTURING VIEW (SCAFFOLD) — From the authority structure's vantage, this reading emerged as a temporary solution to the catastrophic problem posed by Temple destruction: how to maintain divine commandments when their original performative context vanished. Study-as-performance was a scaffolding mechanism that enabled continuity during the diasporic period. This perspective recognizes the reading as having a sunset implicit in its origin: if the Temple were rebuilt, the original performance obligation would return, and study would revert to preparatory status. The authority maintains the reading through active enforcement (interpretive tradition) but sees it as contingent on the ongoing absence of the Temple.
constraint_indexing:constraint_classification(study_as_performance, scaffold,
    context(agent_power(powerful),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: EXILIC/DIASPORIC JEWISH POLITY (SNARE) — The powerless collective (diaspora Jewry without Temple or sovereign priesthood) experiences this reading as a trap: study becomes the obligatory substitute for sacrifice, but the substitution is asymmetrical. The scholar-individual may experience it as Rope, but the collective cannot exit—they cannot rebuild the Temple, cannot restore the original performance, cannot choose other frameworks. The study mandate is framed as fulfillment, but it is a fulfillment of a constraint they did not choose and cannot escape. The suppression is ideological: the reading is enforced through reverence for textual authority, not physical coercion, but exit (adopting a non-halakhic framework) carries existential cost to group identity.
constraint_indexing:constraint_classification(study_as_performance, snare,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 5: RABBINICAL AUTHORITY / ACTIVE INTERPRETER (TANGLED ROPE) — The rabbinical authority that maintains and enforces the study-as-performance reading benefits from its coordination function (preserves legal continuity, maintains community cohesion) and from its extractive function (concentrates interpretive authority, controls the framework for legitimate engagement with divine law). The authority genuinely coordinates diaspora practice; it also genuinely extracts deference and compliance. The constraint requires active enforcement (interpretive authority must continuously reaffirm the reading's validity), distinguishing it from pure coordination. The authority is constrained by its own framework—it cannot simply declare the reading obsolete without rupturing its legitimacy structure.
constraint_indexing:constraint_classification(study_as_performance, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / INSTITUTIONAL INERTIA VIEW (PITON) — The civilizational analytical observer sees this reading as having degraded from active theological response to institutional inertia. The original reading (post-70 CE) was a live creative solution to an unprecedented catastrophe: the destruction of the only locus where the commandment could be performed. The reading was theoretically potent, generating centuries of Talmudic innovation. By late modernity, the reading has become performative—maintained through reverence for the interpretive tradition rather than through active theological engagement with the crisis it was designed to address. The theater ratio is high: the study mandate is performed with solemn ritual authority, but the original generative crisis is no longer present (the Temple has been absent for 2000 years; few contemporary scholars genuinely believe it will be rebuilt). The reading persists through institutional inertia, not through ongoing theodicy.
constraint_indexing:constraint_classification(study_as_performance, piton,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / COORDINATION VIEW (ROPE) — From a systems-theory perspective, the study-as-performance reading solves a genuine coordination problem: how to maintain a legal system whose external referent (the Temple) has been destroyed. The reading enables transmission of juridical knowledge, maintains group identity, and coordinates behavior across diaspora communities spanning centuries and continents. The solution is elegant and minimal: it requires no new institutions, no external resources, only a reinterpretation of the category 'performance.' The coordination function is real, not degraded. The theater is present but not pathological—it is the necessary formalism of any legal system. This perspective sees the reading as robust across time horizons, not inert.
constraint_indexing:constraint_classification(study_as_performance, rope,
    context(agent_power(analytical),
            time_horizon(generational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(study_as_performance_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(study_as_performance, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(study_as_performance, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(study_as_performance, TR),
    TR >= 0.70.

:- end_tests(study_as_performance_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness is 0.0: the reading generates no asymmetric extraction. The scholar-worshipper experiences pure coordination—they benefit from a framework that aligns intellectual engagement with divine obligation. The yeshiva institution benefits from coordination (continuity, identity maintenance) without extraction. There is no victim set: no group bears asymmetric costs. Suppression is 0.0: there is no coercion mechanism. Scholars can exit the framework (can become secular, can adopt non-halakhic Judaism, can migrate to performance_only reading). The suppression appears minimal because the constraint aligns with identity and intrinsic motivation for those within it. Theater ratio measurement: rises from 0.0 at origin (70 CE, when the reading was a live creative response to catastrophe) to 0.70 by modernity (when the reading is maintained as formal obligation, but the original generative crisis is 2000 years distant). This rise in theater reflects the Piton diagnosis: the reading has degraded from active theological response to institutional inertia. However, this degradation does NOT increase extractiveness—the reading remains pure coordination. The theatrical quality is a feature of all legal systems, not a sign of extraction. The constraint is classified as Rope (pure coordination) from primary perspectives, Scaffold from the authority perspective (recognizing sunset contingency on Temple absence), Snare from the diasporic collective perspective (powerless, trapped, no exit), and Piton from the Analytical/civilizational perspective (recognizing institutional inertia). These are all legitimate readings of the same zero-extractiveness structural data.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap between the engaged scholar (Rope, low power but mobile exit) and the diaspora collective (Snare, powerless, trapped exit) reveals the crucial distinction between experiencing a constraint individually and experiencing it collectively. The individual scholar finds pure coordination—study fulfills obligation, intrinsic motivation aligns with the commandment structure. The collective cannot exit—it cannot rebuild the Temple, cannot escape the reading through individual choice. The reading is framed as fulfillment for the individual but experienced as inescapability for the group. The analytical perspectives (Rope/coordination vs. Piton/inertia) represent genuine alternative framings: is this reading a living theological solution to a permanent problem (diaspora existence), or an inert institutional ritual maintaining a defunct crisis response? The answer depends on whether the reading is read as permanently addressing the condition of exile (Rope) or as a temporary solution whose urgency has been lost (Piton). The yeshiva institution and halakhic authority perspectives both recognize the Scaffold logic: the reading has a sunset implicit in its origin (Temple absence). But the collective cannot activate that sunset—it cannot choose to exit or declare the reading obsolete. The authority is constrained by legitimacy (cannot declare the reading false without self-rupture); the collective is constrained by powerlessness (cannot exit without abandoning identity).
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) is derived from beneficiary/victim status and exit options. The scholar-worshipper is a beneficiary (the reading creates a framework for meaningful engagement) with mobile exit options, yielding low d and negative/low chi. The authority is a beneficiary (maintains legitimacy structure) with arbitrage exit (can reinterpret at will), yielding very low d. The yeshiva institution is a beneficiary (coordination function) with constrained exit (cannot simply abandon tradition), yielding low d. The diasporic collective is a victim (cannot choose frameworks) with trapped exit, yielding very high d. But extractiveness is zero, so chi = 0 × f(d) × σ(S) = 0 regardless of d. The directionality reveals the *structure* of the constraint even though extraction is absent: different groups are positioned differently relative to the reading's logic, creating asymmetric experience despite zero-sum extraction. This is the key insight: a zero-extractiveness constraint can still produce profoundly asymmetric experience because some groups are structurally positioned to benefit (align with intrinsic motivation, gain authority, gain continuity) while others are positioned to be trapped (powerless, cannot exit, cannot choose alternatives). The constraint is not unjust in the extraction sense, but it is asymmetrical in the structural sense.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint does not trigger mandatrophy (extractiveness = 0). However, the six perspectives reveal the full complexity of the classification system. Mandatrophy would arise if we tried to compress all six perspectives into a single type—we would be forced to ask 'is this really pure coordination or is it actually extraction disguised as coordination?' The framework resolves this not by averaging or defaulting, but by preserving all perspectives and noting that the classification depends on the observer's position. The Rope (pure coordination) perspectives are correct from the scholar and institution standpoints. The Scaffold perspective is correct from the authority standpoint (recognizing contingency on Temple absence). The Piton perspective is correct from the civilizational analytical standpoint (recognizing institutional inertia). The Snare perspective is correct from the diasporic collective standpoint (powerless, trapped). These are not contradictions; they are perspectival truths reflecting different structural positions. The constraint illustrates why mandatrophy is necessary: we must preserve all perspectives rather than trying to reduce them to a single type.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    temple_restoration_contingency,
    'If the Temple were restored and sacrifice recommenced, would the study-as-performance reading revert to preparatory status or maintain equal obligation?',
    'Examination of classical halakhic sources for explicit contingency statements; analysis of contemporary Orthodox and Conservative halakhic discourse on Temple reconstruction scenarios; theological and philosophical arguments for permanent vs. contingent status of the reading',
    'If study reverts to preparatory: reading is genuinely scaffolding (temporary solution), and the Scaffold perspective is correct. If study maintains equal obligation: reading is foundational (permanent reinterpretation), and the Rope/pure-coordination perspectives are correct. The constraint''s classification depends critically on this resolution.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(temple_restoration_contingency, conceptual, 'Whether study-as-performance obligation is contingent on Temple absence or foundational').

omega_variable(
    reading_origin_date_authenticity,
    'Is the study-as-performance reading an authentic Talmudic doctrine emerging from the post-70 CE response to Temple destruction, or a later medieval/modern reinterpretation imposed on earlier rabbinic sources?',
    'Philological analysis of Talmudic sources (Berakhot 32b, Menachot 110a, and related passages); comparison of classical vs. medieval vs. modern commentarial tradition; examination of whether early Tannaim explicitly framed study as substitute performance or merely as preparation',
    'If authentic post-70 doctrine: the reading represents genuine creative theological response to catastrophe, and the Piton ''institutional inertia'' perspective is anachronistic. If later reinterpretation: the reading is retroactively projected onto earlier sources, and the Piton diagnosis is partially correct—the reading''s theological urgency has indeed degraded from its later reinterpretation. Classification of the reading''s theater ratio depends on this resolution.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_origin_date_authenticity, empirical, 'Authenticity and dating of study-as-performance reading in classical rabbinic sources').

omega_variable(
    kernel_identity_ambiguity,
    'What is the kernel that this reading reinterprets: the divine commandment to bring sacrifices (the original Temple practice), or the category ''performance'' itself?',
    'Formal analysis of the three sibling readings (study_as_performance, performance_only, archive_maintenance) to identify their common reference. If the kernel is ''bring sacrifices,'' then the readings compete on how to fulfill it when the Temple is gone. If the kernel is broader (the category of divinely-mandated practice), then the readings partition a complex conceptual space rather than competing directly.',
    'If kernel is ''bring sacrifices'': the three readings are in genuine logical competition, and performance_only forecloses study_as_performance. If kernel is broader category: the readings coexist and influence rather than foreclose. The network.affects_constraints edges depend on this resolution.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_identity_ambiguity, conceptual, 'Identity of the contested kernel underlying the three sibling readings').

omega_variable(
    extractiveness_zero_validity,
    'Is extractiveness genuinely zero from all perspectives, or is this reading a cover story for rabbinical authority extraction that this reading obscures?',
    'Structural analysis: does the study-as-performance reading provide legitimate coordination benefits that could not be achieved through other means? Does it concentrate authority in ways that pure coordination mechanisms would not require? Historical analysis of how the reading was used to consolidate rabbinical authority post-70 CE.',
    'If extractiveness is genuinely zero: the reading is pure coordination (Rope confirmed from primary perspectives). If the reading masks extraction: it is actually Tangled Rope with very high theater ratio (performative invocation of coordination to justify hidden extraction). The base_properties.extractiveness value depends on this resolution.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extractiveness_zero_validity, empirical, 'Whether study-as-performance reading masks extractive consolidation of rabbinical authority').

omega_variable(
    reading_sibling_logical_structure,
    'What is the precise logical relationship among the three sibling readings (study_as_performance, performance_only, archive_maintenance)? Do any pair foreclose each other, or do all three coexist as live options?',
    'Formal analysis of the axioms each reading depends on. If performance_only requires ''sacrifice can only occur in Temple,'' and study_as_performance requires ''sacrifice can be fulfilled through study,'' then the two forecloses each other IF both are read as necessary conditions. If either can be read as sufficient but not necessary (i.e., ''either performance or study fulfills the commandment''), then they coexist. Archive_maintenance may be compatible with both depending on whether it asserts ''study fulfills commandment'' or only ''knowledge must be preserved.''',
    'If any sibling foreclose each other: the constraint is part of a zero-sum theological competition, and the reading_relations should reflect foreclosure. If all coexist: the kernel accommodates multiple readings simultaneously (reflects actual Jewish theological pluralism), and reading_relations should reflect coexistence or influence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_sibling_logical_structure, conceptual, 'Logical relationships among sibling readings of sacrifice commandment').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(study_as_performance, 0, 2000).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(theater_70ce_baseline, study_as_performance, theater_ratio, 0, 0.0).
narrative_ontology:measurement(theater_400ce_classical_period, study_as_performance, theater_ratio, 400, 0.15).
narrative_ontology:measurement(theater_1200ce_medieval_period, study_as_performance, theater_ratio, 1200, 0.35).
narrative_ontology:measurement(theater_1800ce_early_modern, study_as_performance, theater_ratio, 1800, 0.55).
narrative_ontology:measurement(theater_2000ce_contemporary, study_as_performance, theater_ratio, 2000, 0.7).

% Extraction over time
narrative_ontology:measurement(extract_70ce_baseline, study_as_performance, base_extractiveness, 0, 0.0).
narrative_ontology:measurement(extract_400ce_classical, study_as_performance, base_extractiveness, 400, 0.0).
narrative_ontology:measurement(extract_1200ce_medieval, study_as_performance, base_extractiveness, 1200, 0.0).
narrative_ontology:measurement(extract_1800ce_early_modern, study_as_performance, base_extractiveness, 1800, 0.0).
narrative_ontology:measurement(extract_2000ce_contemporary, study_as_performance, base_extractiveness, 2000, 0.0).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(study_as_performance, identity_coordination).
narrative_ontology:affects_constraint(study_as_performance, performance_only).
narrative_ontology:affects_constraint(study_as_performance, archive_maintenance).

% DUAL FORMULATION NOTE:
% The sacrifice_commandment kernel is instantiated by three separate constraint stories: study_as_performance (this story, ε=0.0, Rope primary), performance_only (ε unknown, likely Mountain or Snare primary), and archive_maintenance (ε unknown, likely Rope or Piton primary). Each story represents a complete structural reading of the kernel with its own beneficiary/victim configuration, perspectives, and temporal dynamics. They are linked via network.affects_constraints to reflect the fact that each reading's viability affects the others' legitimacy conditions. study_as_performance is the dominant reading in mainstream Judaism; performance_only represents a minority theological position; archive_maintenance represents a scholarly or non-committal position. The network edges indicate that each reading's success in legitimacy affects the others' structural plausibility.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
