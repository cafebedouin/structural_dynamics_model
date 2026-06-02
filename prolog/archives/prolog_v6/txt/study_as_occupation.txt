% ============================================================================
% CONSTRAINT STORY: study_as_occupation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_study_as_occupation, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: study_as_occupation
 *   human_readable: Sacrifice Law Study as Living Occupation of the Halakhic Covenant
 *   domain: religious_studies/halakhic_authority/commitment_systems
 *
 * SUMMARY:
 *   After the destruction of the Second Temple in 70 CE, Jewish law faced a
 *   structural crisis: the sacrifice law, foundational to the halakhic
 *   covenant, became performatively impossible. Rabbinic Judaism resolved
 *   this through a commitment-system reading: the law remains valid and
 *   binding not through physical performance but through intellectual
 *   occupation. Study of the sacrifice law IS the performance that keeps the
 *   covenant alive. This constraint models that reading — the
 *   'study_as_occupation' interpretation — as a pure coordination mechanism
 *   with minimal extractiveness. The law's kernel (its binding force) is
 *   occupied continuously through textual engagement, interpretation, and
 *   scholarly transmission. No systematic extraction occurs because all
 *   participants — rabbinic authorities, scholars, studying communities —
 *   benefit symmetrically from maintaining the covenant's validity. The
 *   constraint is nearly theater-free because the study function is direct:
 *   engaging with the text produces genuine halakhic knowledge, community
 *   participation, and continuity of tradition.
 *
 * KEY AGENTS:
 *   - Rabbinic Authority: Institutional beneficiary (institutional/arbitrage) — derives legitimacy from ongoing textual occupation of the law; functions as guardian and interpreter of the covenant
 *   - Studying Community: Powerful participants (powerful/mobile) — scholars and students; voluntarily engaged in textual study; benefit from intellectual engagement and community status
 *   - Jewish Textual Continuity: Diffuse beneficiary (institutional/arbitrage) — the tradition itself; remains alive through study; perpetuated through scholarly transmission
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — sees the constraint as an elegant solution to a commitment crisis; recognizes the pure coordination function
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(study_as_occupation, 0.12).
domain_priors:suppression_score(study_as_occupation, 0.08).
domain_priors:theater_ratio(study_as_occupation, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(study_as_occupation, extractiveness, 0.12).
narrative_ontology:constraint_metric(study_as_occupation, suppression_requirement, 0.08).
narrative_ontology:constraint_metric(study_as_occupation, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(study_as_occupation, rope).
narrative_ontology:human_readable(study_as_occupation, "Sacrifice Law Study as Living Occupation of the Halakhic Covenant").
narrative_ontology:topic_domain(study_as_occupation, "religious_studies/halakhic_authority/commitment_systems").

% --- Commitment system structure ---
narrative_ontology:cs_kernel_codification(study_as_occupation, formalized).
narrative_ontology:cs_authority_grounding(study_as_occupation, lineage).
narrative_ontology:cs_interpretation_layer_present(study_as_occupation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(study_as_occupation, rabbinic_authority).
narrative_ontology:constraint_beneficiary(study_as_occupation, jewish_textual_continuity).
narrative_ontology:constraint_beneficiary(study_as_occupation, studying_community).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: RABBINIC AUTHORITY (ROPE) — Institutional authority structure derives legitimacy from ongoing textual engagement with sacrifice law. Study IS the performance that keeps the halakhic covenant occupied and valid. No extraction occurs because the authority's function (preserving the kernel's legal force through intellectual engagement) is identical to the constraint's function. Pure coordination: the law remains binding through rabbinic occupation of it.
constraint_indexing:constraint_classification(study_as_occupation, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 2: STUDYING COMMUNITY (ROPE) — Individual scholars and study groups experience this constraint as genuine coordination: engaging with sacrifice law texts produces meaning, legitimacy, and community participation. The community is mobile (can exit) but chooses to participate because the coordination function is real. Extractiveness is low because participation yields both personal and collective benefit — textual mastery, institutional status within the community, intellectual life.
constraint_indexing:constraint_classification(study_as_occupation, rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(regional))).

% PERSPECTIVE 3: ANALYTICAL OBSERVER (ROPE) — From a civilizational vantage, this constraint represents a pure coordination solution to the theological problem of maintaining a binding covenant after the Temple's destruction. The mechanism is elegant: the law remains in force (not archived, not suspended) through continuous intellectual occupation. No agent is systematically extracted from; the constraint binds all participants symmetrically. The kernel (the sacrifice law's validity) is kept alive through the community's ongoing engagement.
constraint_indexing:constraint_classification(study_as_occupation, rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(study_as_occupation_tests).
:- end_tests(study_as_occupation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.12): Very low. The constraint's mechanism is pure coordination — study keeps the law binding, and all participants benefit from that outcome. The original research community (scholars) receives institutional legitimacy and intellectual life; the rabbinic authority maintains its authority structure; the tradition continues. No group is systematically extracted from because the study function produces genuine value for all parties. The low extractiveness reflects that this reading treats study as occupation rather than substitution or performance degradation. Suppression (0.08): Very low. Entry into the studying community is largely open — there are economic and time barriers but not structural suppressions. Scholars can exit (mobility is high), and the tradition accommodates diverse levels of engagement. Theater ratio (0.35): Low-moderate. The study function is direct and non-performative — textual engagement produces genuine halakhic knowledge. But some theater is present: public study sessions, formal recitation of sacrificial passages, and the institutional structure of rabbinic authority add performative elements that are not strictly necessary for the covenant occupation function.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap in this constraint is minimal — all perspectives classify the constraint as Rope because the coordination function is genuine and symmetric. The rabbinic authority sees their own legitimacy maintained through the studying community's engagement. The studying community sees themselves as participants in a valued coordination mechanism, not victims of extraction. The analytical observer sees an elegant commitment-system solution. Unlike constraints with high perspectival gaps (where powerless agents see Snare while institutional beneficiaries see Rope), this constraint exhibits low gap because the studying community is powerful and mobile — they are not trapped into participation. The minimal gap is diagnostic: it suggests that the study_as_occupation reading is structurally sound. If there were high perspectival gaps, or if substantial victims appeared in any perspective, the constraint would likely be misclassified or would belong to a sibling reading (study_as_archive or messianic_suspension).
 *
 * DIRECTIONALITY LOGIC:
 *   Each agent's experienced extractiveness is derived from their structural position relative to the constraint. Rabbinic authorities occupy an arbitrage position (institutional/arbitrage) — they can exit the role but choose to occupy it because it provides authority and legitimacy. This produces a low or negative d value (they are net beneficiaries). The studying community is powerful and mobile — they participate voluntarily in a coordination mechanism that serves their interests (intellectual life, community status, tradition participation). This produces a moderate d value around 0.50, but with high f(d) benefit because mobility reduces experienced extraction. The analytical observer sees the structure from a civilizational vantage where all positions are symmetric — no agent is trapped or extracted from. The constraint is a symmetric coordination solution to a theological problem.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is this constraint the ''study_as_occupation'' reading or a different reading of the temple_sacrifice_law kernel?',
    'This constraint instantiates ONLY the ''study_as_occupation'' reading: the law remains valid and binding through intellectual engagement. Alternative readings (''study_as_archive'' — the law is preserved but no longer binding; ''messianic_suspension'' — the law is valid but suspended pending messianic restoration) represent structurally distinct constraints with different extractiveness values and beneficiary/victim configurations.',
    'The ''study_as_occupation'' reading implies zero extractiveness from non-performance: performing the study IS the constraint''s realization. Sibling readings imply different structural relationships: ''archive'' implies high theater (0.65+) with victims (alienated scholars); ''suspension'' implies moderate extraction (0.30-0.45) from those bound by suspension. Each reading is a separate constraint story.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Which reading of the temple_sacrifice_law kernel this constraint instantiates').

omega_variable(
    occupation_vs_performance_ambiguity,
    'Does ''occupation through study'' constitute genuine performance of the sacrifice law, or is it a symbolic substitute that naturalizes the loss of actual Temple sacrifice?',
    'Textual analysis of rabbinic sources (Mishnah Avot 1:17, Maimonides'' Mishneh Torah Korbanot, Kiddushin 37b) distinguishing between ''study standing in place of sacrifice'' (study_as_occupation) vs. ''study serving as remembrance/archive'' (study_as_archive). The interpretive tradition determines which reading dominates.',
    'If study genuinely occupies the constraint (study_as_occupation reading, this story): extractiveness ≈ 0.12, type = Rope, rabbinic authority remains fully legitimate. If study is a substitute legitimizing a loss (study_as_archive reading, separate story): extractiveness ≈ 0.50+, type = Tangled Rope, rabbinic authority is partly sustained through theater.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(occupation_vs_performance_ambiguity, conceptual, 'Whether occupation through study constitutes genuine performance or substitution').

omega_variable(
    commitment_kernel_occupancy_mechanism,
    'What counts as ''occupying'' the halakhic covenant? Is continuous scholarly study sufficient, or does the law require institutional enforcement and observance beyond textual engagement?',
    'Comparative study of post-destruction rabbinic practice vs. contemporary observance: does the law''s validity depend only on textual occupation (study), or on widespread observance of derivative halakhic rulings? Are scholars who study the law functionally equivalent to priests performing sacrifice?',
    'If textual occupation alone suffices: the constraint operates at low extractiveness (0.12), low suppression (0.08), and pure Rope classification holds. If institutional enforcement and observance are required: the constraint involves active enforcement (requires_active_enforcement = true) and may classify as Tangled Rope in some perspectives.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(commitment_kernel_occupancy_mechanism, empirical, 'Mechanism for occupying the halakhic covenant through study').

omega_variable(
    sibling_reading_not_present,
    'What would change if the ''study_as_archive'' reading (law preserved but not binding) or ''messianic_suspension'' reading (law valid but suspended) were applied instead?',
    'Separate constraint story files (constraint_study_as_archive.json, constraint_messianic_suspension.json) instantiate the sibling readings with distinct ε values, beneficiary/victim configurations, and classification types. Each reading is a complete, ε-invariant constraint.',
    'This constraint (study_as_occupation) assumes the law remains fully binding through study. Study_as_archive would have ε ≈ 0.50+, classify as Tangled Rope, identify victims (alienated scholars), and show high theater. Messianic_suspension would have ε ≈ 0.35-0.45, classify as Scaffold, include a sunset clause (pending messianic restoration), and show medium theater.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_not_present, conceptual, 'Sibling readings as separate constraints with different structural properties').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(study_as_occupation, 0, 1000).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stud_tr_t0, study_as_occupation, theater_ratio, 0, 0.32).
narrative_ontology:measurement(stud_tr_t500, study_as_occupation, theater_ratio, 500, 0.35).
narrative_ontology:measurement(stud_tr_t1000, study_as_occupation, theater_ratio, 1000, 0.35).

% Extraction over time
narrative_ontology:measurement(stud_be_t0, study_as_occupation, base_extractiveness, 0, 0.1).
narrative_ontology:measurement(stud_be_t500, study_as_occupation, base_extractiveness, 500, 0.12).
narrative_ontology:measurement(stud_be_t1000, study_as_occupation, base_extractiveness, 1000, 0.12).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(study_as_occupation, identity_coordination).
narrative_ontology:affects_constraint(study_as_occupation, study_as_archive).
narrative_ontology:affects_constraint(study_as_occupation, messianic_suspension).

% DUAL FORMULATION NOTE:
% Temple sacrifice law decomposition: Three structurally distinct readings of the halakhic commitment kernel. study_as_occupation (this file) models the reading where study IS occupation and the law remains binding. study_as_archive (separate file) models the reading where study preserves but does not bind. messianic_suspension (separate file) models the reading where the law is valid but suspended. Each reading has distinct ε, beneficiary/victim configuration, and type. Network links show family relationships but each constraint is ε-invariant in isolation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
