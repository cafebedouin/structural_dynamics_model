% ============================================================================
% CONSTRAINT STORY: birth_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_birth_reading, []).

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
 *   constraint_id: birth_reading
 *   human_readable: Moral Status Begins at Birth (Physical Separation Reading)
 *   domain: moral_philosophy/constitutional_law/bioethics
 *
 * SUMMARY:
 *   The birth reading of the personhood boundary kernel holds that moral and
 *   legal status attach at the moment of physical separation from the
 *   maternal body. This reading emerged as the dominant framework in most
 *   Western secular law (Roe v. Wade, 1973; subsequent abortion
 *   jurisprudence) and reflects a particular philosophical tradition
 *   emphasizing observable boundaries and institutional clarity. Under this
 *   reading, the pregnant person holds exclusive moral authority and medical
 *   decision-making power throughout pregnancy, as the fetus has no moral
 *   standing prior to birth. The constraint operates as pure coordination
 *   between the pregnant person and medical providers — enabling clear
 *   protocols for informed consent, abortion access, and prenatal care —
 *   rather than as a mechanism of extraction. The reading's strength is its
 *   administrative clarity: birth is an unambiguous, observable event that
 *   does not require ongoing empirical reassessment (unlike viability, which
 *   varies by gestational age and medical technology). Its philosophical
 *   vulnerability lies in the arbitrariness of the boundary choice: why is
 *   physical separation morally significant if the fetus has no morally
 *   relevant property that suddenly changes at birth? This omega frames the
 *   core tension within the kernel.
 *
 * KEY AGENTS:
 *   - Pregnant Person: Primary rights-holder (institutional/arbitrage) — holds exclusive moral authority and medical autonomy under this reading; zero suppression of autonomous choice
 *   - Medical Authority: Institutional actor (institutional/constrained) — benefits from clear boundary but experiences institutional extraction via state-level regulatory capture and liability frameworks
 *   - Fetus (Pre-Birth): No status, no voice, no exit (powerless/trapped) — logical consequence of the reading's axiom, not hidden extraction
 *   - Medical-Legal Framework: Beneficiary (institutional/arbitrage) — coordinate actor that benefits from administrable boundary condition
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing a normative choice as intrinsic biological fact
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(birth_reading, 0.15).
domain_priors:suppression_score(birth_reading, 0.08).
domain_priors:theater_ratio(birth_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(birth_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(birth_reading, suppression_requirement, 0.08).
narrative_ontology:constraint_metric(birth_reading, theater_ratio, 0.25).

% --- Constraint claim ---
narrative_ontology:constraint_claim(birth_reading, rope).
narrative_ontology:human_readable(birth_reading, "Moral Status Begins at Birth (Physical Separation Reading)").
narrative_ontology:topic_domain(birth_reading, "moral_philosophy/constitutional_law/bioethics").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(birth_reading, '1db1deec-a2b0-425d-b27f-c230e0784544').
narrative_ontology:cs_created_at('1db1deec-a2b0-425d-b27f-c230e0784544', '').
narrative_ontology:cs_kernel_codification('1db1deec-a2b0-425d-b27f-c230e0784544', formalized).
narrative_ontology:cs_authority_grounding('1db1deec-a2b0-425d-b27f-c230e0784544', lineage).
narrative_ontology:cs_interpretation_layer_present('1db1deec-a2b0-425d-b27f-c230e0784544').
narrative_ontology:cs_kernel_id(birth_reading, personhood_boundary).
narrative_ontology:cs_reading_relation('1db1deec-a2b0-425d-b27f-c230e0784544', conception_reading, forecloses).
narrative_ontology:cs_reading_relation('1db1deec-a2b0-425d-b27f-c230e0784544', viability_reading, forecloses).
narrative_ontology:cs_axiom('1db1deec-a2b0-425d-b27f-c230e0784544', foundational, axiom_discontinuous_personhood).
narrative_ontology:cs_axiom_status(axiom_discontinuous_personhood, holdable).
narrative_ontology:cs_axiom('1db1deec-a2b0-425d-b27f-c230e0784544', foundational, axiom_birth_as_moral_boundary).
narrative_ontology:cs_axiom_status(axiom_birth_as_moral_boundary, holdable).
narrative_ontology:cs_reference_frame('1db1deec-a2b0-425d-b27f-c230e0784544', post_birth_moral_personhood).
narrative_ontology:cs_drift_state('1db1deec-a2b0-425d-b27f-c230e0784544', contemporary, gap(authority_erosion, substantial, false)).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(birth_reading, pregnant_person).
narrative_ontology:constraint_beneficiary(birth_reading, medical_autonomy_framework).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PREGNANT PERSON (ROPE) — Under the birth reading, the pregnant person holds exclusive moral authority and medical rights throughout pregnancy. The constraint is pure coordination: communicating boundaries of fetal vs. maternal rights enables cooperation between pregnant person and medical providers. No extraction occurs — suppression is zero because the framework grants full autonomy to the pregnant person. Low theater because the reading's logic is transparent: birth is a clear, observable event (physical separation). This perspective experiences the constraint as enabling coordination, not constraining choice.
constraint_indexing:constraint_classification(birth_reading, rope,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 2: MEDICAL AUTHORITY (TANGLED ROPE) — Medical providers experience genuine coordination function (protocols for informed consent, prenatal care, abortion access) alongside embedded extraction: institutional liability frameworks and regulatory capture (state restrictions on abortion access and clinical judgment) constrain medical autonomy even in jurisdictions adopting the birth reading. The reading legitimates abortion access, but institutional barriers to implementation persist. Theater ratio reflects performative compliance with nominally protective regulations. Moderate suppression stems from state-level variation in policy and provider licensing vulnerability.
constraint_indexing:constraint_classification(birth_reading, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: FETUS PRE-BIRTH (SNARE) — Under the birth reading, the fetus has no moral status pre-birth and therefore no voice, no representation, no exit option. This is not hidden extraction — it is a logical consequence of the reading's core axiom. The fetus cannot be a beneficiary (no status to benefit), cannot organize opposition (no moral standing), cannot exit (not yet an agent). The reading's classification logic assigns this perspective a snare because it instantiates complete voicelessness under this framework. NOTE: This perspective does not imply the fetus actually suffers in the morally relevant sense — it marks the logical consequence of the reading's axiom that moral status = post-birth existence. Whether this is ethically acceptable is orthogonal to the structural classification.
constraint_indexing:constraint_classification(birth_reading, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 4: MEDICAL-LEGAL FRAMEWORK (ROPE) — The birth reading coordinates medical practice and legal doctrine through a simple, administrable rule: moral and legal status attach at birth/physical separation. This creates low cognitive overhead for institutional actors (legislatures, courts, providers) compared to alternative readings that require line-drawing at conception or viability. The institutional framework experiences this as pure coordination — clear boundary conditions reduce litigation, clarify regulatory scope, and enable consistent policy. No suppression needed because the rule is transparent and widely accepted in jurisdictions adopting this reading.
constraint_indexing:constraint_classification(birth_reading, rope,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: ANALYTICAL OBSERVER / BIRTH AS NATURAL BOUNDARY (MOUNTAIN) — From a civilizational perspective, birth (physical separation from the maternal body) is an observable, unambiguous biological event — far clearer than conception (not reliably observable at the moment) or viability (variable across pregnancies and medical technology). This perspective risks naturalizing what is actually a normative choice: the selection of this particular biological boundary as the relevant moral threshold. The reading frames birth as *the* natural division point, but the analytical observer must recognize that this naturalness is conferred by the reading, not intrinsic to the biology. The engine's false summit detector will flag this if beneficiaries are identified (they are: pregnant person and medical-legal framework).
constraint_indexing:constraint_classification(birth_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(birth_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(birth_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(birth_reading, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

:- end_tests(birth_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.15): Very low. This reading generates minimal extractive mechanism because it grants the pregnant person full authority and requires no suppression of her choice. The small nonzero value (0.15 rather than 0.0) reflects that the reading embeds an implicit institutional preference for the medical-legal framework's clarity — not an extraction from the pregnant person, but a coordination benefit to institutions. Suppression (0.08): Minimal. The reading does not require suppression of anyone's autonomy because it does not pit competing moral claims against each other. The pregnant person's wishes are not overridden; the fetus has no claims to suppress. Theater ratio (0.25): Low. The reading's logic is transparent: birth is a clear, observable boundary with no hidden machinery. The small theater value reflects routine institutional performance (informed consent protocols, documentation) but not performative justification of contested boundaries. The measurements show stability over 30 years — the reading has maintained consistent low extraction and theater even as institutional adoption has expanded.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap reveals the reading's philosophical tension. The pregnant person and medical-legal framework experience rope (pure coordination with no extraction). The fetus experiences snare (voiceless under this framework, though the classification is a logical consequence of the axiom rather than hidden extraction). The analytical observer risks mountain (naturalizing the boundary as intrinsic to biology rather than a chosen interpretive framework). The institutional medical authority experiences tangled rope because institutional extraction persists despite the reading's intention to grant autonomy — state-level policy variation and regulatory capture constrain medical practice even in jurisdictions formally adopting this reading. The gap between 'the reading' (granting pregnant person authority) and 'institutional implementation' (constrained by state interference) is not a failure of the reading but a distinction between the normative claim and its realization.
 *
 * DIRECTIONALITY LOGIC:
 *   Under the birth reading, directionality is straightforward. The pregnant person is the primary beneficiary (d approaches 0.0 — full beneficiary with arbitrage exit) because the reading grants her exclusive authority. The medical-legal framework is a secondary beneficiary (institutional actor with arbitrage exit, experiencing the reading as coordination). The fetus has no structural position in the framework — it has no moral status and therefore no directionality value (it cannot bear extraction of something it does not possess). The institutional medical authority is partially constrained (d ≈ 0.35) because while the reading theoretically grants provider autonomy, actual practice faces state-level regulatory barriers that the reading does not address.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading resolves mandatrophy by demonstrating that the perspectival classification depends on the observer's structural position relative to the reading's axioms. There is no single 'correct' type for the personhood boundary — there are multiple legitimate types depending on whether you occupy the pregnant person's position (rope), the medical authority's position (tangled_rope), the fetus's position under this axiom (snare), the institutional framework's position (rope), or the analytical position (mountain with FSM risk). The mandatrophy is not a problem to solve but a signal that the kernel admits multiple readings, each with its own structural coherence. The birth reading is internally consistent: it grants pregnant person authority, enables institutional coordination, and assigns the fetus no moral standing. That internal consistency makes it a coherent reading. Whether it is *true* (whether birth really is the morally relevant boundary) is a different question — one the constraint framework does not adjudicate.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    boundary_choice_arbitrariness,
    'Is physical separation (birth) a uniquely justified moral boundary, or is the choice of this boundary over conception or viability a normative decision?',
    'Comparative analysis across philosophical frameworks: what criteria justify selection of birth over viability (46% survival without intervention) or conception (genetic distinctness)? Are the criteria empirical, normative, or institutional?',
    'If birth is uniquely justified: the reading is a legitimate reflection of moral reality. If the choice is normative: the reading instantiates one legitimate but not uniquely privileged interpretation of the personhood kernel.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(boundary_choice_arbitrariness, conceptual, 'Whether birth is a uniquely justified vs. normatively chosen boundary').

omega_variable(
    moral_status_continuity,
    'Does moral status attach discontinuously at birth, or does the fetus have some morally relevant status (dignity, potential, relational status) that birth reading denies?',
    'Philosophical analysis of what properties confer moral status; comparison of fetus post-viability to newborn to determine whether relevant differentiating properties exist across the birth boundary.',
    'If fetus has morally relevant status pre-birth: birth reading is incomplete or false. If moral status is genuinely discontinuous: the reading accurately captures the metaphysics.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(moral_status_continuity, conceptual, 'Whether moral status attaches discontinuously at birth').

omega_variable(
    pregnant_person_relational_authority,
    'Is the pregnant person''s unlimited authority pre-birth a consequence of fetal non-personhood, or does it derive from a separate principle of bodily autonomy that would hold even if the fetus had some moral status?',
    'Philosophical reconstruction of the reading''s authority structure: does it rest on the premise that fetus has zero moral weight (axiom_discontinuous_personhood), or on the premise that bodily autonomy trumps competing claims (axiom_maternal_bodily_autonomy)? If the latter, the reading could coexist with acknowledgment of fetal moral status.',
    'If authority rests on fetal non-personhood alone: this reading forecloses readings that acknowledge fetal status. If authority rests on bodily autonomy principle: the reading coexists with other status-granting readings that also value bodily autonomy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(pregnant_person_relational_authority, conceptual, 'Whether pregnant person''s authority derives from fetal non-personhood or from bodily autonomy principle').

omega_variable(
    kernel_reading_vs_empirical_claim,
    'Is this constraint a reading of the personhood boundary kernel (a normative/interpretive choice), or an empirical claim about when personhood actually begins?',
    'If instantiated as a kernel reading: document in committer frame via cs_structure (done). If the author intended an empirical claim: reclassify as a non-kernel constraint and remove committer framing. Current specification assumes kernel reading; verify intent before compilation.',
    'If empirical claim: ε should reflect evidentiary uncertainty (possibly higher). If normative reading: ε reflects transparency of the boundary (0.15 is appropriate for a clear, administrable rule).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_vs_empirical_claim, conceptual, 'Whether constraint is a kernel reading or an empirical factual claim').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(birth_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(birth_tr_t0, birth_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(birth_tr_t15, birth_reading, theater_ratio, 15, 0.25).
narrative_ontology:measurement(birth_tr_t30, birth_reading, theater_ratio, 30, 0.25).

% Extraction over time
narrative_ontology:measurement(birth_be_t0, birth_reading, base_extractiveness, 0, 0.12).
narrative_ontology:measurement(birth_be_t15, birth_reading, base_extractiveness, 15, 0.14).
narrative_ontology:measurement(birth_be_t30, birth_reading, base_extractiveness, 30, 0.15).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(birth_reading, information_standard).
narrative_ontology:affects_constraint(birth_reading, conception_reading).
narrative_ontology:affects_constraint(birth_reading, viability_reading).

% DUAL FORMULATION NOTE:
% The birth_reading, conception_reading, and viability_reading are three readings of the single personhood_boundary kernel. Each generates a distinct constraint with different ε, different beneficiary/victim structures, and different institutional pressures. They are not alternative measurements of a single constraint — they are alternative normative interpretations that cannot be averaged or combined. Network edges show that the birth reading influences (and is influenced by) the other readings: institutional adoption of birth reading creates regulatory pressures on viability reading (tightens medical timelines), and empirical advances in fetal viability affect the institutional plausibility of the birth reading by making the boundary choice seem more arbitrary.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
