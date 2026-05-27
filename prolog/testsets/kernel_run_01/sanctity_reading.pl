% ============================================================================
% CONSTRAINT STORY: sanctity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sanctity_reading, []).

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
 *   constraint_id: sanctity_reading
 *   human_readable: Sanctity of Life Reading: Intrinsic Value Prohibition on Intentional Life-Ending
 *   domain: bioethics/end_of_life_policy
 *
 * SUMMARY:
 *   The sanctity of life reading of end-of-life authority holds that human
 *   life has intrinsic, non-negotiable value that prohibits intentional
 *   killing regardless of individual preference, circumstances, or
 *   consequences. This reading instantiates a constraint that combines
 *   genuine coordination (preventing coercive death, protecting vulnerable
 *   populations from pressure) with asymmetric extraction (denying autonomous
 *   choice to individuals with terminal illness, suffering, or clear
 *   preference for death). The sanctity principle originated in religious
 *   theology (Judeo-Christian traditions, Islamic bioethics) but has been
 *   institutionalized in secular medical law, creating a constraint that
 *   extracts from populations seeking death with dignity while appearing to
 *   benefit the medical establishment through clarified role boundaries. The
 *   constraint shows increasing extractiveness over the 40-year measurement
 *   interval (0.42 → 0.58), reflecting growing tension between medical
 *   capability to extend life and patient autonomy movements demanding
 *   choice. Theater ratio remains relatively low (0.32 → 0.38) because the
 *   sanctity prohibition operates through direct legal prohibition and
 *   institutional gatekeeping rather than performative ritual — it is a
 *   snare, not a piton.
 *
 * KEY AGENTS:
 *   - Pressured Vulnerable Populations (elderly, disabled, economically disadvantaged): Primary victims (powerless/trapped) — bears full cost of categorical prohibition; faces coercive structural pressure to continue living despite perceived unacceptability of life quality
 *   - Individual Seeking Death with Dignity: Primary victim (moderate/constrained) — autonomous preference is nullified by legal and institutional barriers rooted in sanctity principle
 *   - Medical Establishment & Institutional Authority: Primary beneficiary (institutional/arbitrage) — clarified role boundaries, simplified ethical gatekeeping, alignment with religious institutional interests, reduced legal exposure
 *   - Religious Institutional Authority (Church/Theology): Secondary beneficiary (organized/constrained) — grounds legitimacy in sanctity doctrine; constrained by secularizing frameworks
 *   - Right-to-Die Advocacy Coalition: Organized challenger (organized/mobile) — sees constraint as extracting autonomous choice; has capacity to organize and exit (relocate to permissive jurisdictions, lobby for change)
 *   - Analytical Observer: Civilizational position (analytical/analytical) — risks naturalizing contingent institutional arrangement as metaphysical law
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sanctity_reading, 0.58).
domain_priors:suppression_score(sanctity_reading, 0.72).
domain_priors:theater_ratio(sanctity_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sanctity_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(sanctity_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(sanctity_reading, theater_ratio, 0.38).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sanctity_reading, tangled_rope).
narrative_ontology:human_readable(sanctity_reading, "Sanctity of Life Reading: Intrinsic Value Prohibition on Intentional Life-Ending").
narrative_ontology:topic_domain(sanctity_reading, "bioethics/end_of_life_policy").

domain_priors:requires_active_enforcement(sanctity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sanctity_reading, '8d9eefdd-7a63-4c3c-bfb5-23ac394a1389').
narrative_ontology:cs_created_at('8d9eefdd-7a63-4c3c-bfb5-23ac394a1389', '').
narrative_ontology:cs_kernel_codification('8d9eefdd-7a63-4c3c-bfb5-23ac394a1389', fixed_text).
narrative_ontology:cs_authority_grounding('8d9eefdd-7a63-4c3c-bfb5-23ac394a1389', lineage).
narrative_ontology:cs_interpretation_layer_present('8d9eefdd-7a63-4c3c-bfb5-23ac394a1389').
narrative_ontology:cs_kernel_id(sanctity_reading, end_of_life_authority).
narrative_ontology:cs_reading_relation('8d9eefdd-7a63-4c3c-bfb5-23ac394a1389', autonomy_reading, forecloses).
narrative_ontology:cs_reading_relation('8d9eefdd-7a63-4c3c-bfb5-23ac394a1389', slippery_slope_mechanism, influences).
narrative_ontology:cs_axiom('8d9eefdd-7a63-4c3c-bfb5-23ac394a1389', foundational, life_intrinsic_dignity_inviolable).
narrative_ontology:cs_axiom_status(life_intrinsic_dignity_inviolable, holdable).
narrative_ontology:cs_axiom('8d9eefdd-7a63-4c3c-bfb5-23ac394a1389', secondary, vulnerable_population_protection_primacy).
narrative_ontology:cs_axiom_status(vulnerable_population_protection_primacy, holdable).
narrative_ontology:cs_reference_frame('8d9eefdd-7a63-4c3c-bfb5-23ac394a1389', sanctity_as_categorical_imperative).
narrative_ontology:cs_drift_state('8d9eefdd-7a63-4c3c-bfb5-23ac394a1389', contemporary_autonomy_ascendant, gap(authority_erosion, substantial, false)).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sanctity_reading, medical_establishment).
narrative_ontology:constraint_beneficiary(sanctity_reading, institutional_authority_structures).
narrative_ontology:constraint_victim(sanctity_reading, pressured_vulnerable_populations).
narrative_ontology:constraint_victim(sanctity_reading, autonomous_decision_making_capacity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PRESSURED VULNERABLE POPULATIONS (SNARE) — Trapped by the categorical prohibition. Faces coercive structural pressure: family burden, economic dependency, social marginalization. The sanctity framework prohibits their exit option (assisted dying) even when they perceive it as merciful. High suppression (0.72); no organized capacity to challenge the constraint. Maximum extraction experienced — bearing the full cost of the prohibition without input into its legitimacy.
constraint_indexing:constraint_classification(sanctity_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: INDIVIDUAL SEEKING DEATH WITH DIGNITY (SNARE) — Constrained by legal and institutional barriers rooted in the sanctity principle. Even with articulate preference, access to physician-assisted dying is blocked. The constraint frames their autonomous choice as morally impermissible regardless of circumstances. High experienced extraction — their agency is nullified by categorical prohibition.
constraint_indexing:constraint_classification(sanctity_reading, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: MEDICAL ESTABLISHMENT & INSTITUTIONAL AUTHORITY (ROPE) — Benefits from the sanctity framework. Clarifies physician role (life preservation only); simplifies ethical gatekeeping; aligns with religious institutional interests; reduces legal exposure for institutions. Experiences the constraint as coordination of medical practice around a bright-line rule. Net beneficiary — extraction flows toward this agent; they have arbitrage (can practice medicine within the sanctity framework worldwide).
constraint_indexing:constraint_classification(sanctity_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: RIGHT-TO-DIE ADVOCACY COALITION (TANGLED ROPE) — Organized agents (patient advocates, bioethicists, some physicians) see the sanctity prohibition as both extracting AND coordinating. Extraction: denies autonomous choice to terminally ill patients. Coordination: the framework does solve the problem of preventing coerced death and protecting vulnerable populations from pressure. Mobile exit (can relocate to jurisdictions with medical assistance in dying, can lobby for legal change). Moderate-high extraction chi because the advocacy coalition has agency and clear alternative framings.
constraint_indexing:constraint_classification(sanctity_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: RELIGIOUS INSTITUTIONAL AUTHORITY (TANGLED ROPE) — Constrained by secularizing legal frameworks and declining institutional power in pluralistic societies. The sanctity principle originates in theological commitments but is being decomposed into secular/medical framings. The constraint coordinates religious bioethics (prevents instrumental death; maintains human dignity doctrine) while also extracting by claiming institutional authority in plural secular societies. Constrained exit because religious legitimacy itself is under pressure in secular polities.
constraint_indexing:constraint_classification(sanctity_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER — NATURAL LAW VIEW (MOUNTAIN) — From a civilizational/universal perspective, human life has intrinsic, non-negotiable value that cannot be instrumentalized for any purpose, including ending suffering. This perspective sees the sanctity principle as a fundamental metaphysical truth grounded in human dignity that transcends utility calculus. However, the structural data contradicts this mountain classification: identified beneficiaries (medical establishment, institutional authority), victims (pressured vulnerable, autonomous capacity), and active enforcement (legal prohibition, institutional gatekeeping) reveal this as a false summit — a constructed institutional arrangement naturalized as metaphysical law.
constraint_indexing:constraint_classification(sanctity_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sanctity_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(sanctity_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(sanctity_reading, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(sanctity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(sanctity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The sanctity reading's core mechanism extracts autonomous choice from individuals with terminal illness or unbearable suffering. However, the extraction is not maximal (0.70+) because the constraint genuinely solves a real coordination problem — preventing coercive death and protecting vulnerable populations from pressure. The constraint has a legitimate coordination function (protective), not pure extraction. The measurement interval shows increasing extractiveness (0.42 → 0.58) reflecting growing divergence between medical capability to extend life indefinitely and patient demand for choice over timing and manner of death. Suppression (0.72): High. Powerful structural barriers prevent exit: legal prohibition, institutional gatekeeping, physician role constraints, social stigma, religious authority backing. For the elderly and disabled, suppression is near-total — they cannot access assisted dying even with clear preference and full capacity. Theater ratio (0.38): Low. The sanctity prohibition operates through direct legal and institutional mechanisms rather than performative ritual. Enforcement is straightforward and unambiguous — prohibited by law, not hidden behind complex procedures. This low theater distinguishes the constraint from piton (degraded ritual) classification.
 *
 * PERSPECTIVAL GAP:
 *   The sanctity reading produces maximum perspectival divergence across power positions. The medical establishment sees coordination (Rope) — the sanctity principle provides a clear ethical boundary and simplifies decision-making. The pressured vulnerable see pure extraction (Snare) — they are denied choice and bear the full cost. The right-to-die coalition sees hybrid extraction-coordination (Tangled Rope) — the principle both protects (prevents coerced death) and extracts (denies autonomous choice). The analytical observer risks seeing metaphysical truth (Mountain) — intrinsic human value — but structural analysis reveals this as a false summit: the principle is maintained by identifiable institutional beneficiaries (medical establishment, religious authority), not by laws of nature. The perspectival gap reflects that the 'sanctity' framing means different things to different structural positions: for the medical establishment, it is a coordination tool; for the powerless, it is a trap; for advocates, it is an unjust extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) measures each agent's structural position relative to the constraint. Pressured vulnerable populations are full targets (d ≈ 0.95) — trapped with no exit, bearing maximum extraction. The medical establishment are beneficiaries (d ≈ 0.10) — arbitrage exit option, net benefit from sanctity framework. Right-to-die advocates are organized challengers (d ≈ 0.60) — mobile exit (can relocate, can organize for change), partial victim/partial beneficiary (extract in denying choice, benefit in protecting vulnerable). The sigmoid f(d) converts these position measures into experienced extractiveness multipliers. Trapped agents with d ≈ 0.95 experience f(d) ≈ 1.42 (maximum multiplier), making the effective extraction chi = 0.58 × 1.42 ≈ 0.82 from their perspective. Institutional beneficiaries with d ≈ 0.10 experience f(d) ≈ -0.01 (slight negative chi), perceiving net coordination. This explains why the snare perspective (powerless/trapped) and rope perspective (institutional/arbitrage) disagree so sharply on constraint type — their directionalities are opposite ends of the scale.
 *
 * MANDATROPHY ANALYSIS:
 *   The sanctity reading resolves mandatrophy by explicitly modeling the constraint as Tangled Rope: it has a genuine coordination function (preventing coercive death, protecting vulnerable populations) AND asymmetric extraction (denying autonomous choice). The constraint cannot be reduced to pure extraction (Snare) because it genuinely solves a real problem. It cannot be reduced to pure coordination (Rope) because identified beneficiaries clearly benefit from the prohibition while victims bear the cost. The true classification is hybrid. The false summit signal (mountain perspective) is diagnostically important — the constraint risks being naturalized as metaphysical law ('human life has intrinsic value') rather than understood as an institutional arrangement that coordinates one value (protection of life) while extracting on another (autonomous choice). The mandatrophy is resolved when the analytical observer recognizes that 'intrinsic value' is a normative commitment, not a metaphysical fact that would justify the mountain classification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sanctity_vs_autonomy_foreclosure,
    'Does the sanctity reading''s core premise (intrinsic value of life is inviolable) logically foreclose the autonomy reading''s core premise (individual self-determination is inviolable)?',
    'Conceptual analysis of whether any single coherent normative framework could hold both premises. Examination of attempted reconciliations (e.g., ''autonomy within constraints of sanctity'') and their logical stability.',
    'If forecloses: the readings are genuinely incompatible; only one can be institutionalized in a single legal system. If coexists_with: different parties can hold both simultaneously, and the contest is political rather than logical.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sanctity_vs_autonomy_foreclosure, conceptual, 'Whether sanctity and autonomy readings logically foreclose each other').

omega_variable(
    vulnerable_population_protection_mechanism,
    'Does the categorical prohibition on assisted dying actually protect vulnerable populations from coercion, or does it simply render their coercion invisible by framing it as natural constraint?',
    'Comparative outcomes analysis: jurisdictions with categorical prohibition vs. jurisdictions with safeguarded access (informed consent, waiting periods, psychological evaluation). Measurement of actual coercion rates, depression in terminally ill populations, and pressure-induced deaths.',
    'If protection mechanism works: sanctity reading''s victim claim (preventing vulnerable coercion) is substantiated. If mechanism fails or is invisible: sanctity prohibition may actually extract from vulnerable populations by denying them a recognized exit option.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(vulnerable_population_protection_mechanism, empirical, 'Whether categorical prohibition protects or invisibly harms vulnerable populations').

omega_variable(
    intrinsic_vs_instrumental_value_grounding,
    'Is the sanctity reading''s claim of ''intrinsic value'' of human life grounded in metaphysics, theology, or contingent institutional history?',
    'Genealogical analysis of the sanctity principle: origins in religious doctrine vs. secular bioethics; historical contingency of the categorical prohibition (not present in all cultures or historical periods); examination of whether ''intrinsic value'' can be articulated independent of religious commitments.',
    'If metaphysically grounded: mountain classification is more defensible — the sanctity principle would apply universally and invariantly. If theologically or historically contingent: the false summit signal is confirmed — the constraint naturalizes a particular institutional tradition.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intrinsic_vs_instrumental_value_grounding, conceptual, 'Whether sanctity claim is metaphysically universal or institutionally contingent').

omega_variable(
    slippery_slope_empirical_validity,
    'Does permitting medical assistance in dying for clearly-defined categories (terminal illness, persistent suffering, informed consent) actually lead to expansion to non-terminal populations, or is the slippery slope mechanism a discursive artifact?',
    'Historical analysis of jurisdictions that implemented safeguarded assisted dying: tracking changes in eligibility criteria over time; comparison of actual expansion rates vs. predicted expansion rates; examination of whether expansions followed from logical inconsistency or from discrete political choices.',
    'If slippery slope is real: sanctity reading''s argument against assisted dying has empirical support. If slope is discursive: the prohibition may extract from current populations based on hypothetical future harm.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(slippery_slope_empirical_validity, empirical, 'Whether safeguarded assisted dying leads to actual slippery slope or if concern is discursive').

omega_variable(
    reading_kernel_ambiguity,
    'This constraint is one reading of the contested end-of-life authority kernel. Which reading — sanctity, autonomy, or slippery-slope mechanism — represents the kernel''s reference frame (t0)?',
    'Historical analysis of legal and institutional authority: when was the sanctity prohibition codified in Western medicine? What authority structure did it displace? Is the reference frame ''sanctity has always governed end-of-life decisions'' (reads as t0) or ''physician discretion is the reference and sanctity is a recent constraint''?',
    'If sanctity is t0: the autonomy reading represents drift away from reference frame. If physician discretion or family authority is t0: sanctity reading represents authority imposition. The reference frame choice determines drift direction in cs_structure.drift_state.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_kernel_ambiguity, conceptual, 'Which reading represents the kernel''s reference state vs. subsequent drift').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sanctity_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sanc_tr_t0, sanctity_reading, theater_ratio, 0, 0.32).
narrative_ontology:measurement(sanc_tr_t20, sanctity_reading, theater_ratio, 20, 0.35).
narrative_ontology:measurement(sanc_tr_t40, sanctity_reading, theater_ratio, 40, 0.38).

% Extraction over time
narrative_ontology:measurement(sanc_be_t0, sanctity_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(sanc_be_t20, sanctity_reading, base_extractiveness, 20, 0.5).
narrative_ontology:measurement(sanc_be_t40, sanctity_reading, base_extractiveness, 40, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sanctity_reading, identity_coordination).
narrative_ontology:affects_constraint(sanctity_reading, autonomy_reading).
narrative_ontology:affects_constraint(sanctity_reading, slippery_slope_mechanism).

% DUAL FORMULATION NOTE:
% The sanctity reading is one of three constraint stories decomposed from the contested end-of-life authority kernel. Each reading instantiates a different set of beneficiaries/victims and produces different classifications. The sanctity reading (this file) emphasizes protection of vulnerable populations and institutional authority. The autonomy reading emphasizes individual choice and decision-making capacity. The slippery-slope mechanism emphasizes empirical risk of criteria expansion. All three are upstream of actual end-of-life policy outcomes; network links show that change in one reading's institutional dominance affects the others' resource availability and legitimacy conditions.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
