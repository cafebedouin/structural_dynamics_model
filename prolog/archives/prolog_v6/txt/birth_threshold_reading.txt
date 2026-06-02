% ============================================================================
% CONSTRAINT STORY: birth_threshold_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_birth_threshold_reading, []).

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
 *   constraint_id: birth_threshold_reading
 *   human_readable: Birth Threshold Moral Standing (Equal Protection Reading)
 *   domain: moral_philosophy/commitment_systems/personhood
 *
 * SUMMARY:
 *   The birth-threshold reading of the personhood boundary establishes that
 *   all infants born alive possess equal moral standing and rights,
 *   regardless of physical or cognitive capacity, disability, or prognosis.
 *   This reading generates institutional commitment to parental protection
 *   duty and state prohibition of infanticide. The constraint is ONE READING
 *   of the contested kernel 'personhood_boundary,' distinct from
 *   conditional-fitness and viability-threshold readings. This reading
 *   produces a tangled-rope classification: it coordinates protection against
 *   infanticide and eugenic selection (genuine coordination function), yet
 *   simultaneously enforces medical interventions that may contradict
 *   parental judgment and infant interests (asymmetric extraction). The
 *   constraint exhibits rising extractiveness over its historical interval,
 *   driven by increasing medical capacity to prolong life and intensifying
 *   state authority to enforce the constraint through intensive-care
 *   mandates. Theater ratio remains low, indicating the constraint has
 *   genuine philosophical and legal content rather than being primarily
 *   performative — unlike ritual mechanisms, the birth threshold is enforced
 *   through prosecutorial action and institutional override.
 *
 * KEY AGENTS:
 *   - Born infants: Primary victims under the reading (powerless/trapped) — defined as having moral standing requiring protection, yet dependent on intermediaries to exercise rights
 *   - Parents, especially those with severely disabled newborns: Primary victims (powerless to constrained/trapped) — stripped of medical autonomy when state enforces intensive intervention
 *   - State legal authority: Institutional beneficiary (institutional/arbitrage) — gains legitimacy from clear equal-protection boundary; coordinates human rights regime
 *   - Clinical physicians and neonatologists: Secondary victim and beneficiary (moderate/constrained) — benefit from clarity of the rule; constrained by liability and institutional protocol preventing palliative care decisions
 *   - Disability rights advocates: Organized secondary beneficiary (organized/constrained) — benefit from protection against infanticide; constrained by inflexible enforcement that may mandate intervention against interests of dying infants
 *   - Analytical observer: Civilizational perspective (analytical/analytical) — risks naturalizing the birth threshold as immutable law when it is actually a contested institutional choice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(birth_threshold_reading, 0.52).
domain_priors:suppression_score(birth_threshold_reading, 0.68).
domain_priors:theater_ratio(birth_threshold_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(birth_threshold_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(birth_threshold_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(birth_threshold_reading, theater_ratio, 0.41).

% --- Constraint claim ---
narrative_ontology:constraint_claim(birth_threshold_reading, tangled_rope).
narrative_ontology:human_readable(birth_threshold_reading, "Birth Threshold Moral Standing (Equal Protection Reading)").
narrative_ontology:topic_domain(birth_threshold_reading, "moral_philosophy/commitment_systems/personhood").

domain_priors:requires_active_enforcement(birth_threshold_reading).

% --- Commitment system structure ---
narrative_ontology:cs_kernel_codification(birth_threshold_reading, formalized).
narrative_ontology:cs_authority_grounding(birth_threshold_reading, extraction).
narrative_ontology:cs_interpretation_layer_present(birth_threshold_reading).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(birth_threshold_reading, born_infants).
narrative_ontology:constraint_beneficiary(birth_threshold_reading, human_rights_advocates).
narrative_ontology:constraint_victim(birth_threshold_reading, parental_autonomy).
narrative_ontology:constraint_victim(birth_threshold_reading, state_medical_authority).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: NEWBORN INFANT (SNARE) — The infant has no exit from the constraint and depends entirely on external agents for survival and protection. Maximum experienced extraction: the constraint classifies the infant as possessing rights, triggering state oversight and parental obligation, yet the infant cannot assert these rights without intermediaries. The infant is trapped in complete dependency with no capacity to negotiate the boundary that defines their moral status.
constraint_indexing:constraint_classification(birth_threshold_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(universal))).

% PERSPECTIVE 2: PARENTS FACING INTENSIVE MEDICAL BURDEN (SNARE) — Parents with severely disabled newborns face state enforcement of the constraint: refusal of intensive life support is prosecuted as neglect or murder, even when medical prognosis indicates inevitable death within months and the intervention causes suffering. The constraint extracts parental autonomy in medical decision-making with high suppression — legal jeopardy, social condemnation, institutional override of parental judgment. Parents are trapped between competing duties (to the child, to themselves, to other children) with no legal exit.
constraint_indexing:constraint_classification(birth_threshold_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(universal))).

% PERSPECTIVE 3: STATE LEGAL AUTHORITY / HUMAN RIGHTS FRAME (ROPE) — From the perspective of states that have adopted the birth-threshold reading as constitutional principle, the constraint functions as coordination: establishing a clear, administrable boundary (birth) that prevents arbitrary infanticide and grounds a universal equal-protection regime. The state benefits from the clarity and legitimacy of the rule. This perspective experiences the constraint as pure coordination — a baseline commitment that enables human rights enforcement. No extraction is perceived; the state sees itself as enforcing, not extracting.
constraint_indexing:constraint_classification(birth_threshold_reading, rope,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: CLINICAL PHYSICIANS AND NEONATOLOGISTS (TANGLED ROPE) — Physicians experience genuine coordination: the constraint provides a clear framework for informed consent and withdrawal-of-treatment discussions. They also experience extraction: state and institutional enforcement of the constraint restricts their medical judgment in cases where continuing intervention clearly contradicts patient interests. Physicians are constrained by liability risk, institutional protocol, and legal oversight. They both benefit from the clarity of the rule and bear costs from its inflexibility in edge cases.
constraint_indexing:constraint_classification(birth_threshold_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: DISABILITY RIGHTS AND MEDICAL ETHICS COMMUNITIES (TANGLED ROPE) — These organized actors benefit from the constraint's protection of born individuals with disabilities against infanticide — a genuine coordination function that prevents eugenic selection. They simultaneously experience extraction: the constraint, as enforced, can mandate intensive intervention against the wishes of dying infants and bereaved parents, treating all impairment as requiring maximal extension of life. The constraint generates asymmetric extraction through inflexible application. These actors are constrained by institutional and legal barriers but retain organizational voice.
constraint_indexing:constraint_classification(birth_threshold_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL MORAL LAW VIEW (MOUNTAIN) — From a universalizing civilizational perspective, the birth threshold appears as a natural moral law: all humans from birth possess equal intrinsic dignity and rights, grounded in rationality, embodiment, or species membership. This view treats the constraint as foundational to human morality itself, unchangeable and universal. However, the structural data contradicts this: the constraint has identifiable beneficiaries, exhibits high suppression, and creates asymmetric extraction. The engine will detect this as a false summit — the 'natural law' framing naturalizes what is actually a contested institutional commitment.
constraint_indexing:constraint_classification(birth_threshold_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(birth_threshold_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(birth_threshold_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(birth_threshold_reading, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(birth_threshold_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(birth_threshold_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-to-high. The constraint extracts parental autonomy in medical decision-making, especially in cases where newborns face inevitable death within months and intensive intervention causes suffering. The extraction has increased over time (from 0.38 to 0.52) as medical capacity to prolong life has expanded, making the state's enforcement power more consequential. The constraint also extracts from the state medical authority by restricting its judgment to criteria of prognosis and parental consent rather than fitness assessment. Suppression (0.68): Moderate-to-high. Parents face legal jeopardy (prosecution for murder or neglect) if they refuse intervention for severely disabled newborns. The constraint uses law and institutional authority to override parental medical judgment. However, suppression is not total — some jurisdictions recognize palliative care exceptions, prosecutorial discretion, and parental authority in low-prognosis cases. The suppression is enforced but not absolute. Theater ratio (0.41): Low. The constraint has genuine philosophical and legal content grounded in foundational equal-protection commitments. It is enforced through prosecutorial action, not ritual compliance. The relatively low theater reflects that this is a commitment-system constraint with substantive authority grounding, not a performative mechanism.
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits maximum perspectival divergence. Born infants and severely-burden parents experience pure extraction (Snare) — they are trapped by the constraint with no exit. State authority experiences pure coordination (Rope) — the constraint solves the collective action problem of preventing infanticide. Physicians experience mixed coordination and extraction (Tangled Rope) — the constraint provides clarity but restricts medical judgment. Disability rights advocates experience tangled coordination and extraction (Tangled Rope) — they value the protection against infanticide but oppose inflexible enforcement. The analytical observer risks perceiving natural law (Mountain) when the structure reveals a contested institutional choice. The gap reveals that the birth threshold is neither natural law nor pure extraction, but a commitment that creates asymmetric costs and benefits depending on structural position.
 *
 * DIRECTIONALITY LOGIC:
 *   The birth-threshold constraint exhibits asymmetric directionality across positions. Parents facing intensive-care mandates for dying newborns experience high directionality toward victimhood (d ≈ 0.88): they are structurally trapped (no legal exit), bear high costs (medical autonomy, emotional burden, resource expense), and receive minimal coordination benefit. State authority experiences low directionality toward victimhood (d ≈ 0.12): it is institutionally positioned, has exit through legal interpretation or policy change, and receives significant benefit from the constraint's legitimacy and human rights coordination. Physicians experience moderate directionality (d ≈ 0.52): they experience both constraint (liability, protocol) and benefit (clarity, shared responsibility). The perspectival gap between the state's low-d perception (rope/coordination) and parents' high-d perception (snare/extraction) reflects that the same constraint structure produces opposite experienced asymmetries depending on exit capacity and power. The constraint's tangled-rope classification emerges from the coexistence of genuine coordination (preventing infanticide) and genuine asymmetric extraction (restricting parental autonomy).
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by distinguishing coordination function from extraction mechanism. The birth-threshold principle coordinates protection against infanticide — a genuine collective action problem requiring a bright-line rule. The enforcement regime extracts parental autonomy in medical decision-making — an asymmetric cost imposed through state authority. The tangled-rope classification captures both: the constraint is not pure extraction (it does solve a real coordination problem) and not pure coordination (it does impose asymmetric costs on agents without meaningful exit). The mandatrophy dissolves when we recognize that the constraint simultaneously achieves a coordination goal and extracts autonomy through that same mechanism. The bright-line rule that prevents infanticide also prevents parental medical judgment in cases where intervention is futile. The constraint would fail as a snare (pure extraction) because it genuinely reduces infanticide risk; it would fail as a rope (pure coordination) because it imposes asymmetric costs on parents and physicians with no exit. The tangled-rope classification is the only coherent fit to the structural data.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    birth_threshold_kernel_reading,
    'Is the birth threshold a reading of a contested kernel (personhood_boundary) or a foundational natural law?',
    'Historical and comparative analysis: the birth threshold is ONE reading among multiple defensible readings of when moral standing begins (conception, quickening, viability, birth, sentience). The sibling readings (conditional_fitness_reading, viability_threshold_reading) are structurally distinct constraint stories with different ε values, beneficiary/victim sets, and classifications. The fact that alternative readings exist and have been institutionalized in some jurisdictions demonstrates that the birth threshold is a contingent choice, not a natural law.',
    'If contingent reading: the mountain classification from the analytical perspective is a false summit (naturalization). The constraint is a commitment choice, not a natural law. Reclassification target: tangled_rope. If natural law: the beneficiary/victim structure is interpretively derived, not causally real, and the false summit signature is a measurement artifact.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(birth_threshold_kernel_reading, conceptual, 'Whether birth threshold is a reading or a natural law').

omega_variable(
    suppression_mechanism_state_vs_parental,
    'Is the suppression mechanism enforced state authority (legal jeopardy for parental refusal) or internalized parental moral obligation (parents believing the constraint is correct)?',
    'Analysis of enforcement patterns: jurisdictions with weakly enforced birth thresholds (e.g., palliative care exemptions, parental refusal authority, prosecutorial discretion) show lower effective suppression than jurisdictions with strict enforcement. Examine separation of suppression due to legal jeopardy vs. suppression due to moral conviction in parent decision-making surveys.',
    'If primarily state-enforced: suppression is a structural property of the constraint''s implementation, not its logical content. If primarily internalized: the constraint has deeper cultural legitimacy but may mask subtle extraction. If mixed: omega variable resolution requires jurisdiction-specific decomposition.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_state_vs_parental, empirical, 'Mechanism and source of suppression (state enforcement vs. internalized obligation)').

omega_variable(
    extractiveness_medical_vs_philosophical,
    'Is the measured extractiveness (0.52) driven by the constraint''s philosophical content (birth defines moral standing) or by its medical implementation (state authority to enforce intensive intervention)?',
    'Decompose constraint stories: write separate constraints for (a) birth-threshold-as-philosophical-principle (lower ε) and (b) intensive-life-support-enforcement-regime (higher ε). If ε values differ significantly, the single constraint story was conflating two structurally distinct mechanisms. The philosophical principle is coordination; the enforcement regime is extraction.',
    'If decomposition reveals two constraints: extractiveness should be assigned to the enforcement regime story, not the principle. The principle itself may be rope or mountain. If single constraint is correct: the constraint''s extractiveness is inherent to making the birth threshold actionable in medical decision-making.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extractiveness_medical_vs_philosophical, conceptual, 'Whether extractiveness is intrinsic to the principle or to its medical implementation').

omega_variable(
    comparative_moral_standing_thresholds,
    'What empirical or conceptual differences between the birth-threshold reading and sibling readings (conditional_fitness_reading, viability_threshold_reading) determine which reading applies to a specific case?',
    'Explicit comparison of the three constraint stories: (1) birth_threshold_reading (this file) — all born infants have equal standing; (2) conditional_fitness_reading — moral standing is conditional on fitness or developmental capacity; (3) viability_threshold_reading — moral standing begins at viability. Map the differences in ε, suppression, beneficiary/victim sets, and perspectival classifications. Identify the empirical or conceptual cruxes that determine reading selection.',
    'If readings are incommensurable (cannot be compared): each reading is valid in its own institutional context; the kernel is genuinely contested and may never reach full consensus. If readings are ranked by empirical evidence: the reading with strongest evidential support should dominate jurisdictions claiming to follow empirical reasoning. If readings differ on value premises: the choice is irreducibly political/philosophical.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(comparative_moral_standing_thresholds, conceptual, 'Structural and empirical differences between the three personhood_boundary readings').

omega_variable(
    equal_protection_vs_individual_autonomy,
    'When the birth-threshold constraint''s equal-protection function conflicts with parental medical autonomy, which principle has priority, and why?',
    'Case law analysis: examine jurisdictions with explicit hierarchy of principles (e.g., child welfare supersedes parental autonomy above some threshold of harm; parental autonomy supersedes state authority below some threshold of prognosis). Identify whether the hierarchy is stated or emergent from enforcement patterns. Compare stated hierarchies across jurisdictions.',
    'If equal protection takes priority in all cases: the constraint''s suppression mechanism is necessary to its function, and suppression = 0.68 is justified. If parental autonomy is restored as priority in some cases (e.g., palliative care exemptions): effective suppression is lower than 0.68 in those jurisdictions, suggesting constraint stories should be jurisdiction-specific.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(equal_protection_vs_individual_autonomy, preference, 'Hierarchy of equal protection vs. parental autonomy in the constraint').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(birth_threshold_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(birt_tr_t0, birth_threshold_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement(birt_tr_t50, birth_threshold_reading, theater_ratio, 50, 0.34).
narrative_ontology:measurement(birt_tr_t100, birth_threshold_reading, theater_ratio, 100, 0.41).

% Extraction over time
narrative_ontology:measurement(birt_be_t0, birth_threshold_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(birt_be_t50, birth_threshold_reading, base_extractiveness, 50, 0.45).
narrative_ontology:measurement(birt_be_t100, birth_threshold_reading, base_extractiveness, 100, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(birth_threshold_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(birth_threshold_reading, conditional_fitness_reading).
narrative_ontology:affects_constraint(birth_threshold_reading, viability_threshold_reading).
narrative_ontology:affects_constraint(birth_threshold_reading, infanticide_prohibition_enforcement).
narrative_ontology:affects_constraint(birth_threshold_reading, parental_medical_autonomy).

% DUAL FORMULATION NOTE:
% The birth-threshold reading is one member of the personhood_boundary kernel family. All three readings (birth_threshold_reading, conditional_fitness_reading, viability_threshold_reading) instantiate different constraints from the same kernel because they differ in ε, suppression, and beneficiary/victim structure. They are not alternative perspectives on a single constraint; they are structurally distinct commitments with different enforcement regimes. They are linked through the kernel, not through causal dependence. The birth-threshold reading affects constraints on infanticide prohibition (downstream: enforcement becomes applicable to all born infants) and parental autonomy (downstream: medical decision-making is constrained by equal-protection requirements).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(birth_threshold_reading, institutional, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
