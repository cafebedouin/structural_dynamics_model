% ============================================================================
% CONSTRAINT STORY: kodashim_obligation__performance_prerequisite
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_kodashim_obligation__performance_prerequisite, []).

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
 *   constraint_id: kodashim_obligation__performance_prerequisite
 *   human_readable: Kodashim Obligation: Performance Prerequisite Reading
 *   domain: religious_law/commitment_systems/jewish_halakhah
 *
 * SUMMARY:
 *   The kodashim obligation (laws of Temple sacrifice) represents one of the
 *   most complex commitment-system constraints in Jewish law. This particular
 *   reading instantiates the 'performance prerequisite' interpretation: the
 *   obligation to perform sacrifices binds the obligated agent, but
 *   performance requires Temple reconstruction as a material precondition.
 *   During the Temple absence (since 70 CE), the obligation persists in
 *   halakhic validity but is materially suspended. Current study of the
 *   detailed sacrificial procedures is framed as a preparatory holding
 *   pattern — maintaining knowledge and intention so that when the Temple is
 *   reconstructed, performance can resume. This reading creates a tangled
 *   rope structure: genuine coordination (the law defines what must be
 *   studied, what knowledge must be preserved) layered with asymmetric
 *   extraction (obligated agents bear the burden of an obligation they cannot
 *   fulfill; institutional actors benefit from maintaining the obligation's
 *   validity). The constraint is distinguished from sibling readings by its
 *   core claim that performance is NOT substitutable by study, and that the
 *   obligation's force derives from the future performance prerequisite, not
 *   from current practice.
 *
 * KEY AGENTS:
 *   - Obligated Performers (Kohenim, Jews in Temple period, conceptual extension to present): Victims (powerless/trapped) — bear the obligation to perform; cannot exit; cannot perform due to Temple absence
 *   - Talmudic/Rabbinic Institution: Beneficiary (institutional/arbitrage) — maintains authority to interpret the obligation, arbitrates what counts as preparatory study, benefits from institutional validity of the obligation
 *   - Temple Reconstructionist Movements: Victim/Beneficiary (moderate/constrained) — want to rebuild Temple (victim status: constrained by halakhic opposition and practical barriers); justified by the persistent obligation (beneficiary status: obligation validates the project)
 *   - Halakhic Reform/Conservative Movements: Organized agent (organized/mobile) — effectively exit or reframe the obligation; see it as a scaffold with practical sunset
 *   - Orthodox Institutional Framework: Institutional actor (institutional/constrained) — maintains the obligation formally; constrained by theological commitment to Messianic restoration; theater has increased as functional knowledge has atrophied
 *   - Analytical Observer: Analytical position (analytical/analytical) — risks naturalizing a commitment-specific constraint as a logical immutability
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(kodashim_obligation__performance_prerequisite, 0.38).
domain_priors:suppression_score(kodashim_obligation__performance_prerequisite, 0.65).
domain_priors:theater_ratio(kodashim_obligation__performance_prerequisite, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(kodashim_obligation__performance_prerequisite, extractiveness, 0.38).
narrative_ontology:constraint_metric(kodashim_obligation__performance_prerequisite, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(kodashim_obligation__performance_prerequisite, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(kodashim_obligation__performance_prerequisite, tangled_rope).
narrative_ontology:human_readable(kodashim_obligation__performance_prerequisite, "Kodashim Obligation: Performance Prerequisite Reading").
narrative_ontology:topic_domain(kodashim_obligation__performance_prerequisite, "religious_law/commitment_systems/jewish_halakhah").

domain_priors:requires_active_enforcement(kodashim_obligation__performance_prerequisite).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(kodashim_obligation__performance_prerequisite, '4c8cad79-99b3-46f3-89d8-07912e3a1920').
narrative_ontology:cs_kernel_codification('4c8cad79-99b3-46f3-89d8-07912e3a1920', formalized).
narrative_ontology:cs_authority_grounding('4c8cad79-99b3-46f3-89d8-07912e3a1920', lineage).
narrative_ontology:cs_interpretation_layer_present('4c8cad79-99b3-46f3-89d8-07912e3a1920').
narrative_ontology:cs_reading_relation('4c8cad79-99b3-46f3-89d8-07912e3a1920', kodashim_obligation__study_as_occupation, forecloses).
narrative_ontology:cs_reading_relation('4c8cad79-99b3-46f3-89d8-07912e3a1920', kodashim_obligation__memorial_archival, coexists_with).
narrative_ontology:cs_axiom('4c8cad79-99b3-46f3-89d8-07912e3a1920', foundational, performance_is_unseparable_prerequisite).
narrative_ontology:cs_axiom_status(performance_is_unseparable_prerequisite, holdable).
narrative_ontology:cs_axiom_grounding('4c8cad79-99b3-46f3-89d8-07912e3a1920', performance_is_unseparable_prerequisite, deontological).
narrative_ontology:cs_axiom('4c8cad79-99b3-46f3-89d8-07912e3a1920', secondary, temple_reconstruction_historically_possible).
narrative_ontology:cs_axiom_status(temple_reconstruction_historically_possible, holdable).
narrative_ontology:cs_axiom_grounding('4c8cad79-99b3-46f3-89d8-07912e3a1920', temple_reconstruction_historically_possible, empirically_contingent).
narrative_ontology:cs_reference_frame('4c8cad79-99b3-46f3-89d8-07912e3a1920', temple_service_operational).
narrative_ontology:cs_drift_state('4c8cad79-99b3-46f3-89d8-07912e3a1920', contemporary_diaspora_post_2000_ce, gap(codification_collapse, substantial, false)).
narrative_ontology:cs_created_at('4c8cad79-99b3-46f3-89d8-07912e3a1920', '').
narrative_ontology:cs_kernel_id(kodashim_obligation__performance_prerequisite, kodashim_obligation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(kodashim_obligation__performance_prerequisite, talmudic_institution).
narrative_ontology:constraint_beneficiary(kodashim_obligation__performance_prerequisite, interpretive_authority).
narrative_ontology:constraint_victim(kodashim_obligation__performance_prerequisite, obligated_performers).
narrative_ontology:constraint_victim(kodashim_obligation__performance_prerequisite, temple_reconstructionist_movements).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: OBLIGATED PERFORMER (SNARE) — Bears the halakhic obligation to perform Temple sacrifice yet is structurally prevented from performing it. Cannot exit the obligation (it is a perpetual mitzvah); cannot fulfill it (no Temple); cannot delegate (personal duty). Trapped across generations. Maximum suppression: the obligation persists in law while performance is foreclosed by material absence. This agent experiences the constraint as pure extraction — the duty without the means, enforced by religious authority that maintains the obligation's validity.
constraint_indexing:constraint_classification(kodashim_obligation__performance_prerequisite, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: TEMPLE RECONSTRUCTIONIST MOVEMENT (TANGLED ROPE) — Constrained by halakhic opposition, political barriers to Temple rebuilding, and cost/infrastructure requirements. Yet also benefits from the obligation's maintenance: reconstructionists' legitimacy partly derives from the law's expectation that performance will resume. The constraint enforces the obligation that justifies their project. Significant extraction (constrained exit, victim status for current inability to perform) but also genuine coordination function (the law structures what must be rebuilt and how).
constraint_indexing:constraint_classification(kodashim_obligation__performance_prerequisite, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: TALMUDIC/RABBINIC INSTITUTION (ROPE) — Benefits from maintaining the obligation's validity in the absence of performance. The rabbinical framework that declares 'study is as if you performed the sacrifice' (substitution doctrine) is itself a form of extraction that justifies the institution's authority to interpret the law and arbitrate practice. Yet genuine coordination occurs: the obligation creates structure for Jewish practice identity and continuity across diaspora. The institution experiences this as coordination — defining what the law requires, what counts as fulfillment in the interim, and how to structure religious practice around an obligation whose performance is deferred.
constraint_indexing:constraint_classification(kodashim_obligation__performance_prerequisite, rope,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: HALAKHIC REFORM MOVEMENT (SCAFFOLD) — Organized agents (Conservative and Reform movements) have effectively modified the obligation's scope by reinterpreting performance prerequisites: they have narrowed the obligated cohort (historically men only; now inclusive), reframed study as fulfillment rather than placeholder, or declared the obligation superseded by historical conditions. This represents a scaffold with a de facto sunset: the obligation persists in Orthodox halakhah but has been effectively bracketed in practice. Mobile exit because reformists can leave the obligatory framework entirely; low theater because the reframing is explicit doctrine, not performative ritual.
constraint_indexing:constraint_classification(kodashim_obligation__performance_prerequisite, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: ORTHODOX INSTITUTIONAL FRAMEWORK (PITON) — The obligation persists in Orthodox halakhah as a formal rule (obligation to study, intention to perform, prayer for restoration), but the functional content has substantially atrophied: the specific sacrificial knowledge required for Temple service is no longer transmitted systematically; the detailed halakhic procedures are studied archaeologically rather than operationally; the constraint maintains its formal authority through institutional inertia and theological commitment (awaiting Messianic restoration) rather than through active enforcement. Theater ratio is moderate-high because performance of study is ritualized (textual engagement, prayer services including Temple-focused liturgy) even though the functional performance (actual sacrifice) is suspended.
constraint_indexing:constraint_classification(kodashim_obligation__performance_prerequisite, piton,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / MATERIALIST VIEW (MOUNTAIN) — From a purely logical perspective, if performance is a prerequisite for an obligation to have content, and performance is impossible, then the obligation itself is vacuous — a logical category error. The obligation cannot bind anyone if its precondition cannot be met. This perspective reads the constraint as a logical immutability: material absence of the Temple makes the obligation inherently non-binding. However, this misses the committer frame: the reading's core claim is precisely that performance IS the prerequisite, and the law DOES bind even during the preparatory interval. The mountain classification represents a failure of the analytical frame to see that the constraint's meaning derives from commitment, not from abstract logic.
constraint_indexing:constraint_classification(kodashim_obligation__performance_prerequisite, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(kodashim_obligation__performance_prerequisite_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(kodashim_obligation__performance_prerequisite, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(kodashim_obligation__performance_prerequisite, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(kodashim_obligation__performance_prerequisite, TR),
    TR >= 0.70.

:- end_tests(kodashim_obligation__performance_prerequisite_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The reading declares that the obligation persists despite material impossibility of performance. This creates extraction for obligated agents (burden of an unfulfillable duty) and institutional benefit for rabbinical authorities (who maintain the obligation's legal validity and interpretive authority). However, extractiveness is not high because genuine coordination exists: the law defines a coherent structure for practice (study, intention, prayer toward restoration), and this structure has substantial religious meaning beyond institutional extraction. The trajectory from t=0 (immediate post-Temple period, ε ≈ 0.22: genuine coordination, obligation just given) to t=1900 (modern period, ε ≈ 0.38: institutionalization complete, theater increased) shows extraction accumulating as the institutional apparatus grows around the obligation. Suppression (0.65): Moderate-high. Obligated agents face multiple barriers: the material absence of the Temple (structural immovability), halakhic authority that maintains the obligation's validity (institutional suppression), and identity fusion with the obligation (internalized suppression — obligated agents often cannot conceive of themselves as Jews without this obligation, even if they disagree with it). Suppression has increased from t=0 (0.55, primarily structural) to t=1900 (0.65, structural plus institutional plus internalized). Theater ratio (0.58): Moderate-high. The obligation is maintained through scholarly study, liturgical references to Temple service, and prayer for restoration — all substantially performative given that the functional performance cannot occur. The theater has increased dramatically from t=0 (0.20, obligation was operationally performed in Temple) to t=1900 (0.58, obligation is ritualized textual engagement without operative content). This trajectory is diagnostic: rising theater signals constraint degradation from functional institution (Temple service) to maintained form (study and prayer).
 *
 * PERSPECTIVAL GAP:
 *   The primary perspectival gap lies between the obligated performer's snare classification and the rabbinic institution's rope classification. For the obligated performer, the constraint is pure extraction: they bear the obligation perpetually, cannot fulfill it, cannot exit it, and receive no coordination benefit — the obligation simply persists as law. For the institutional beneficiary, the constraint is coordination: it defines practice, structures Jewish law and identity, and maintains the institution's interpretive authority. The gap reveals that the same constraint is experienced as extractive from the victim's position and coordinative from the beneficiary's position. The scaffold perspective (Reform movements) shows that exit is possible through reinterpretation, revealing that the powerless perspective's 'trapped' exit is not absolute but depends on accepting the institution's framing. The piton perspective (Orthodox maintenance) reveals that the obligation has become substantially theatrical: the functional knowledge (how to actually perform sacrifices) has atrophied, and the constraint is maintained through institutional inertia and theological commitment rather than through active, operative authority. The mountain perspective risks misclassifying the constraint as a logical necessity ('if performance is a prerequisite, and performance is impossible, then the obligation is void') when in fact the constraint's meaning derives entirely from the committer frame: the rabbinical decision that the obligation persists despite material impossibility.
 *
 * DIRECTIONALITY LOGIC:
 *   Obligated performers (powerless/trapped): d ≈ 0.92 (victim + trapped = maximum experienced extraction). Rabbinic institution (institutional/arbitrage): d ≈ 0.08 (beneficiary + arbitrage = minimum experienced extraction, possibly negative). Reform movements (organized/mobile): d ≈ 0.45 (organized power + mobile exit = moderate extraction experienced, lower than trapped but higher than beneficiary arbitrage). The chi scaling follows: obligated performers experience high effective extraction despite moderate base ε (0.38) because their d is high and their power is low — the constraint extracts more severely from them. Institutional beneficiaries experience low effective extraction because their d is low (beneficiary position) — the constraint coordinates their authority. The scope modifier applies globally (σ ≈ 1.2) because this is a foundational obligation in Jewish law affecting all obligated agents regardless of location.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by showing that the tangled_rope classification reflects a genuine structural hybrid: the obligation contains a real coordination function (defining what knowledge must be preserved, how practice must be structured) that cannot be separated from its extractive function (obligated agents bear an unsatisfiable duty; institutional actors benefit from its perpetuation). The constraint could be misclassified as pure snare (from the victim's perspective) or pure rope (from the institutional perspective) if the observer's position is taken as privileged. The tangled_rope classification holds both: it acknowledges the coordination (the law defines a meaningful structure) and the asymmetric extraction (victims and beneficiaries experience opposite directionality). The mandatrophy itself is endemic to the constraint: the obligation's meaning derives from a core theological claim (performance is the prerequisite, and will eventually occur) that is neither provable nor disprovable from within the halakhic framework. The engine cannot resolve whether this is genuine coordination around a future event or sophisticated institutional extraction of an impossible obligation — that resolution depends on meta-halakhic commitments (whether the framework's theological claims are credited). Declaring this as unresolved mandatrophy in `mandatrophy_resolved: false` (if ε > 0.70) would be appropriate if extractiveness were higher; at ε=0.38, the constraint is structurally stable (both coordination and extraction components are visible and measured).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    performance_prerequisite_scope,
    'Does the obligation to perform obligate ONLY when performance is materially possible, or does the obligation persist independent of performance possibility?',
    'Textual analysis of Mishnah Kodashim and Talmudic disputations; comparison of how the obligation is framed during Temple period vs. post-destruction era; examination of whether obligated agents bear guilt for non-performance during Temple absence',
    'If performance-prerequisite (THIS reading): obligation persists as binding, study is preparatory scaffold, extractiveness remains moderate. If obligation-independent (study-as-occupation reading): study itself fulfills the obligation, extractiveness drops, suppression decreases. If memorial-archival (third reading): obligation is historical memorial, extractiveness near zero.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(performance_prerequisite_scope, conceptual, 'Whether the obligation binds independent of material performance possibility').

omega_variable(
    temple_reconstruction_timeline,
    'Is Temple reconstruction viewed as historically possible (within human agency and effort) or eschatologically deferred (dependent on divine action outside human control)?',
    'Survey of halakhic and theological positions on rebuilding preconditions; analysis of historical Temple reconstruction attempts and their halakhic status; examination of whether the obligation''s validity is indexed to reconstruction probability',
    'If historically possible: the scaffold is a real temporal structure with achievable sunset; extractiveness remains justified as temporary coordination. If eschatologically deferred: the obstacle is ontological, not temporal; the constraint may shift toward mountain (immutable condition) or become purely extractive (infinite obligation without resolution path).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(temple_reconstruction_timeline, conceptual, 'Whether Temple reconstruction is historically or eschatologically framed').

omega_variable(
    substitution_doctrine_validity,
    'Does the rabbinical doctrine that ''study is as if you performed the sacrifice'' constitute actual fulfillment of the obligation, or is study a placeholder that does not discharge the underlying duty?',
    'Textual examination of Talmudic sources (particularly Menachot 110a) establishing substitution doctrine; comparison with post-destruction halakhic rulings on how the obligation is satisfied; analysis of whether guilt for non-performance persists despite study',
    'If study genuinely fulfills: obligation is satisfiable in current conditions, suppression decreases, extractiveness drops toward rope. If study is placeholder: obligation persists unsatisfied, suppression remains high, extractiveness remains high (institutional extraction of deferred obligation).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(substitution_doctrine_validity, conceptual, 'Whether study doctrine actually discharges the obligation or merely suspends it').

omega_variable(
    identity_fusion_rabbinical_authority,
    'To what extent is the obligated agent''s identity (as Jew, as ritual practitioner) constituted through the obligation to perform sacrifices, even during Temple absence?',
    'Analysis of how the obligation shapes daily practice, prayer, and identity claims; examination of whether abandoning the obligation (as Reform movements have) constitutes religious exit or identity rupture; ethnographic study of how obligated agents narrate their relationship to Temple sacrifice',
    'If identity-fused: obligated agents are identity_locked, even if structurally mobile (could exit the obligation but cannot exit the identity it shapes); trapped exits become constrained or mobile at different temporal horizons. If instrumental: obligations are external rules, exit is accessible, suppression is primarily structural (not internalized).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_fusion_rabbinical_authority, empirical, 'Whether identity fusion locks obligated agents into the constraint').

omega_variable(
    extractive_intent_of_institution,
    'Does the rabbinic institution maintain the obligation''s validity primarily to preserve religious authority (institutional extraction), or primarily to preserve continuity of Jewish practice and obligation (genuine coordination)?',
    'Historical analysis of how rabbinic positions on Temple obligation evolved in response to institutional challenges; examination of whether institutional benefit (authority, textual centrality) correlates with strictness of obligation maintenance; comparison with how other post-Temple obligations were modified or dropped',
    'If institutional extraction: beneficiary classification (talmudic_institution) is primary; tangled_rope classification is correct. If genuine coordination: beneficiaries should be removed or reframed; classification may shift toward scaffold or rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extractive_intent_of_institution, conceptual, 'Whether institutional maintenance is extractive or coordinative').

omega_variable(
    diasporic_obligation_indexing,
    'Is the obligation to perform Temple sacrifice indexed to Eretz Yisrael (geographically/nationally bound) or universal (binding on all Jews regardless of location)?',
    'Examination of whether the obligation applies equally in diaspora and in the Land of Israel; analysis of how geographic location affects halakhic classification of obligated actors; comparison of obligation scope pre- and post-dispersion',
    'If geographically indexed: Temple absence may affect obligation validity for diaspora agents differently than for agents in Eretz Yisrael; suppression, victims, and beneficiaries may be geographically differentiated. This could split the constraint into two stories (diasporic vs. territorial readings).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(diasporic_obligation_indexing, conceptual, 'Whether the obligation is geographically indexed or universal').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(kodashim_obligation__performance_prerequisite, 0, 1900).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(kodash_perf_tr_t0, kodashim_obligation__performance_prerequisite, theater_ratio, 0, 0.2).
narrative_ontology:measurement(kodash_perf_tr_t250, kodashim_obligation__performance_prerequisite, theater_ratio, 250, 0.48).
narrative_ontology:measurement(kodash_perf_tr_t1900, kodashim_obligation__performance_prerequisite, theater_ratio, 1900, 0.58).

% Extraction over time
narrative_ontology:measurement(kodash_perf_be_t0, kodashim_obligation__performance_prerequisite, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(kodash_perf_be_t250, kodashim_obligation__performance_prerequisite, base_extractiveness, 250, 0.38).
narrative_ontology:measurement(kodash_perf_be_t1900, kodashim_obligation__performance_prerequisite, base_extractiveness, 1900, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(kodash_perf_su_t0, kodashim_obligation__performance_prerequisite, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(kodash_perf_su_t250, kodashim_obligation__performance_prerequisite, suppression_requirement, 250, 0.62).
narrative_ontology:measurement(kodash_perf_su_t1900, kodashim_obligation__performance_prerequisite, suppression_requirement, 1900, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(kodashim_obligation__performance_prerequisite, enforcement_mechanism).
narrative_ontology:affects_constraint(kodashim_obligation__performance_prerequisite, kodashim_obligation__study_as_occupation).
narrative_ontology:affects_constraint(kodashim_obligation__performance_prerequisite, kodashim_obligation__memorial_archival).

% DUAL FORMULATION NOTE:
% The kodashim obligation kernel is decomposed into three constraint stories representing different readings of the same textual and halakhic foundation. Each reading has its own ε value: performance_prerequisite (ε=0.38, tangled_rope), study_as_occupation (ε≈0.15, scaffold or rope), memorial_archival (ε≈0.05, rope or piton). The readings are not observational variants of one constraint but structurally distinct claims about what the law requires. They are linked by network dependency: performance_prerequisite forecloses study_as_occupation (if performance is required, study cannot substitute); both coexist with memorial_archival (different communities hold different readings simultaneously). This is a canonical example of ε-invariance: the observable used to evaluate the constraint (is study fulfillment or placeholder? is performance possible or eschatologically deferred? is the obligation binding or historical?) directly determines ε. No single ε value spans all readings — each must be authored as a separate constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
