% ============================================================================
% CONSTRAINT STORY: honor_settlement_legitimacy__contraction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_honor_settlement_legitimacy__contraction_reading, []).

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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    domain_priors:emerges_naturally/1,
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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: honor_settlement_legitimacy__contraction_reading
 *   human_readable: Cognitive Unthinkability of Dueling as Honor Settlement (Contraction Reading)
 *   domain: historical_sociology/legal_history/cultural_anthropology
 *
 * SUMMARY:
 *   This constraint story captures the contraction reading of the
 *   honor_settlement_legitimacy kernel: the historical process by which
 *   dueling and honor-based violence became not merely illegal but
 *   cognitively unthinkable within the dominant cultural framework of
 *   modernizing European societies (1750-1900). The constraint is the
 *   cultural-cognitive boundary that renders honor violence incomprehensible
 *   as legitimate action. From inside the transformed framework, this
 *   boundary appears as a Mountain — a natural limit of civilized
 *   consciousness. Historically, it was constructed through state monopoly on
 *   violence, commercial pacification, bourgeois cultural hegemony, and the
 *   redefinition of honor from vertical (aristocratic) to horizontal
 *   (civic/dignity-based) modalities. The reading claims this transformation
 *   was thoroughgoing: honor culture itself exits the normative possibility
 *   space for the dominant population.
 *
 * KEY AGENTS:
 *   - modern_state_legal_monopoly: Primary agenda_setter and beneficiary (institutional/generational/arbitrage/global) — establishes and benefits from the monopoly on legitimate violence
 *   - commercial_civil_society: Primary beneficiary (organized/biographical/mobile/global) — gains predictable, non-violent dispute resolution essential for commerce
 *   - vulnerable_populations_protected_from_honor_violence: Primary beneficiary (powerless/biographical/constrained/national) — women, lower classes, minorities historically subjected to honor violence gain protection
 *   - residual_honor_adherents: Primary payer/victim (moderate/biographical/identity_locked/local) — fringe groups for whom honor violence remains meaningful (dueling clubs, rural/regional holdouts, military sub-cultures)
 *   - traditional_aristocratic_military_classes: Historical payer (powerful/generational/constrained/national) — lost their distinctive honor settlement mechanism and status privilege
 *   - historians_anthropologists: Observer (analytical/civilizational/analytical/universal) — analyze the transformation from outside the constraint
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(honor_settlement_legitimacy__contraction_reading, 0.15).
domain_priors:suppression_score(honor_settlement_legitimacy__contraction_reading, 0.1).
domain_priors:theater_ratio(honor_settlement_legitimacy__contraction_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(honor_settlement_legitimacy__contraction_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(honor_settlement_legitimacy__contraction_reading, suppression_requirement, 0.1).
narrative_ontology:constraint_metric(honor_settlement_legitimacy__contraction_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(honor_settlement_legitimacy__contraction_reading, accessibility_collapse, 0.95).
narrative_ontology:constraint_metric(honor_settlement_legitimacy__contraction_reading, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(honor_settlement_legitimacy__contraction_reading, mountain).
narrative_ontology:human_readable(honor_settlement_legitimacy__contraction_reading, "Cognitive Unthinkability of Dueling as Honor Settlement (Contraction Reading)").
narrative_ontology:topic_domain(honor_settlement_legitimacy__contraction_reading, "historical_sociology/legal_history/cultural_anthropology").

domain_priors:emerges_naturally(honor_settlement_legitimacy__contraction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(honor_settlement_legitimacy__contraction_reading, 'cbe0ae6e-953a-4356-8463-6503f64dedef').
narrative_ontology:cs_kernel_codification('cbe0ae6e-953a-4356-8463-6503f64dedef', distributed).
narrative_ontology:cs_authority_grounding('cbe0ae6e-953a-4356-8463-6503f64dedef', practice).
narrative_ontology:cs_interpretation_layer_present('cbe0ae6e-953a-4356-8463-6503f64dedef').
narrative_ontology:cs_reading_relation('cbe0ae6e-953a-4356-8463-6503f64dedef', honor_settlement_legitimacy__drop_reading, coexists_with).
narrative_ontology:cs_reading_relation('cbe0ae6e-953a-4356-8463-6503f64dedef', honor_settlement_legitimacy__composite_reading, influences).
narrative_ontology:cs_axiom('cbe0ae6e-953a-4356-8463-6503f64dedef', foundational, cultural_framework_transformation_eliminates_honor_category).
narrative_ontology:cs_axiom_status(cultural_framework_transformation_eliminates_honor_category, holdable).
narrative_ontology:cs_axiom_grounding('cbe0ae6e-953a-4356-8463-6503f64dedef', cultural_framework_transformation_eliminates_honor_category, empirically_contingent).
narrative_ontology:cs_axiom('cbe0ae6e-953a-4356-8463-6503f64dedef', secondary, cognitive_unthinkability_is_self_enforcing_coordination).
narrative_ontology:cs_axiom_status(cognitive_unthinkability_is_self_enforcing_coordination, holdable).
narrative_ontology:cs_axiom_grounding('cbe0ae6e-953a-4356-8463-6503f64dedef', cognitive_unthinkability_is_self_enforcing_coordination, instrumental).
narrative_ontology:cs_reference_frame('cbe0ae6e-953a-4356-8463-6503f64dedef', pre_modern_honor_settlement_order).
narrative_ontology:cs_drift_state('cbe0ae6e-953a-4356-8463-6503f64dedef', modern_civilizational_framework, gap(authority_erosion, severe, false)).
narrative_ontology:cs_created_at('cbe0ae6e-953a-4356-8463-6503f64dedef', '').
narrative_ontology:cs_kernel_id(honor_settlement_legitimacy__contraction_reading, honor_settlement_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(honor_settlement_legitimacy__contraction_reading, modern_state_legal_monopoly).
narrative_ontology:constraint_beneficiary(honor_settlement_legitimacy__contraction_reading, commercial_civil_society).
narrative_ontology:constraint_beneficiary(honor_settlement_legitimacy__contraction_reading, vulnerable_populations_protected_from_honor_violence).
narrative_ontology:constraint_victim(honor_settlement_legitimacy__contraction_reading, residual_honor_adherents).
narrative_ontology:constraint_victim(honor_settlement_legitimacy__contraction_reading, traditional_aristocratic_military_classes).
narrative_ontology:constraint_vindicates(honor_settlement_legitimacy__contraction_reading, state_monopoly_on_violence).
narrative_ontology:constraint_vindicates(honor_settlement_legitimacy__contraction_reading, rule_of_law_supremacy).
narrative_ontology:constraint_vindicates(honor_settlement_legitimacy__contraction_reading, equal_protection_under_law).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Establishes and maintains the monopoly on legitimate violence through courts, police, and legal codes. Benefits from the cognitive unthinkability of private violence — it makes the monopoly self-enforcing and reduces enforcement costs. Can change the legal framework but has no incentive to restore honor violence; exit from this constraint would mean abandoning the foundational basis of state authority.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__contraction_reading, modern_state_legal_monopoly, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(honor_settlement_legitimacy__contraction_reading, modern_state_legal_monopoly, beneficiary).

% Merchants, professionals, urban populations who rely on predictable, non-violent dispute resolution for contracts, credit, and daily coordination. The cognitive unthinkability of honor violence lowers transaction costs and enables impersonal exchange. They benefit without administering the constraint. Exit is mobile — they could relocate to jurisdictions with different norms, but the constraint is near-universal in modern commercial societies.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__contraction_reading, commercial_civil_society, beneficiary,
    organized, biographical, mobile, global).

% Women, lower classes, ethnic/religious minorities, and others historically subjected to honor violence (honor killings, forced marriages, duel challenges they cannot refuse). The constraint protects them by making such violence not just illegal but unthinkable. Their exit is constrained — they depend on the state monopoly for protection and have limited ability to opt out of the cultural framework.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__contraction_reading, vulnerable_populations_protected_from_honor_violence, beneficiary,
    powerless, biographical, constrained, national).

% Fringe groups for whom honor violence remains meaningful: traditional dueling clubs (Mensur in German universities), rural/regional honor cultures (Corsica, Crete, American South, Caucasus), military sub-cultures preserving officer honor codes. They experience the constraint as extraction — their meaningful practice is rendered deviant and incomprehensible. Exit is identity_locked: abandoning honor violence means abandoning their self-concept and communal identity. They are structurally trapped in a constraint that declares their worldview nonexistent.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__contraction_reading, residual_honor_adherents, payer,
    moderate, biographical, identity_locked, local).

% Historical elites (nobility, officer corps) whose status and conflict resolution depended on the legitimacy of honor violence. They lost their distinctive privilege and the cultural framework that gave their violence meaning. By 1900 this class is largely transformed or extinct as a coherent group. Their exit was constrained — they could not individually opt out of the cultural transformation, though some emigrated or converted to the new civic honor.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__contraction_reading, traditional_aristocratic_military_classes, payer,
    powerful, generational, constrained, national).

% Analysts who study the transformation from outside the constraint. They see the full structural picture: the historical construction of the cognitive boundary, the beneficiaries and victims, the contested kernel. They neither collect from nor pay into the constraint. Their analytical exit is complete — they can adopt any reading of the kernel without personal cost.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__contraction_reading, historians_anthropologists, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(honor_settlement_legitimacy__contraction_reading, modern_state_legal_monopoly).
narrative_ontology:fixing_cost_class(honor_settlement_legitimacy__contraction_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates peaceful dispute resolution across a complex, impersonal society by monopolizing legitimate violence in the state and rendering private violence cognitively unthinkable — eliminating the need for constant negotiation of honor boundaries and violent retaliation.
% TRANSFER_FUNCTION: Transfers the legitimacy and practice of violence from private actors (aristocrats, honor groups, individuals) to the state legal apparatus. Transfers the risk and cost of violence from vulnerable populations (who bore it under honor culture) to the state (which bears it through policing and courts). Transfers status authority from vertical honor (birth, caste, martial prowess) to horizontal dignity (citizenship, legal equality, commercial reputation).
% ABSENT_VOICES: Residual honor adherents (dueling clubs, regional honor cultures, military traditionalists) are structurally excluded from the dominant cultural conversation — their objection would be treated as psychopathology or criminality, not a legitimate cultural claim. Historical aristocratic and military classes are extinct as coherent political voices. The drop reading's subjects are the absent voices here.
% DISAPPEARANCE_RATIONALE: If the cognitive unthinkability of honor violence vanished overnight, the legal prohibition would remain but lose its self-enforcing character. Honor violence would re-emerge in segments of the population where the cultural transformation was shallow or imposed (residual adherents, post-conflict zones, communities with weak state penetration). The state would face dramatically higher enforcement costs. Commercial trust would degrade in affected sectors. The world would rearrange toward a fragmented landscape of competing normative orders — the composite reading's world, not the contraction reading's.
% FOUNDING_PROBLEM: The problem of private honor violence (dueling, feuding, vendetta) destabilizing early modern states: aristocratic challenges to state authority, unpredictable retaliation cycles disrupting commerce, vulnerable populations (women, lower classes) subjected to violence they could not refuse or reciprocate, and the impossibility of impersonal market exchange under constant threat of honor-based coercion.
% FOUNDING_PROBLEM_CORROBORATION: Norbert Elias (The Civilizing Process) documents the long-term pacification of European societies. Max Weber (monopoly on legitimate violence) identifies the state's successful consolidation. Historical criminologists (e.g., Pieter Spierenburg) show the decline of elite violence and rise of state justice. Feminist historians (e.g., Elizabeth Foyster) document the gendered transition from honor violence to legal protection. These sources are outside the benefiting parties (they are analysts, not the state/commerce/vulnerable populations themselves) and corroborate that the founding problem of honor violence as a systemic threat to order is largely resolved in modern societies.
narrative_ontology:disappearance_verdict(honor_settlement_legitimacy__contraction_reading, world_rearranges).
narrative_ontology:founding_problem_status(honor_settlement_legitimacy__contraction_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(honor_settlement_legitimacy__contraction_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(honor_settlement_legitimacy__contraction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(honor_settlement_legitimacy__contraction_reading, 0.15, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(honor_settlement_legitimacy__contraction_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(honor_settlement_legitimacy__contraction_reading, ExtMetricName, E),
    domain_priors:suppression_score(honor_settlement_legitimacy__contraction_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(honor_settlement_legitimacy__contraction_reading),
    narrative_ontology:constraint_metric(honor_settlement_legitimacy__contraction_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(honor_settlement_legitimacy__contraction_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(honor_settlement_legitimacy__contraction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The contraction reading authors low extractiveness (0.15) because from its own lights, the constraint is not experienced as extraction — it is the water modern subjects swim in. Suppression is near-zero (0.10) because no active enforcement is needed against a cognitively unthinkable action. Theater ratio is minimal (0.05) — there is no performative maintenance of a dead norm. Accessibility collapse is near-total (0.95) — the alternative (honor culture) has vanished from the normative imagination of the dominant framework. Resistance is negligible (0.05) — there is no organized resistance to a constraint that structures the very categories of thought. The measurement series shows the historical trajectory: high extraction/suppression/theater in 1750 (active dueling culture with state attempts to suppress) declining to near-zero by 1900 (cognitive unthinkability achieved). The claimed_type is Mountain, but beneficiaries are declared, making this an FSM candidate — the omegas document the natural-law vs. constructed ambiguity.
 *
 * PERSPECTIVAL GAP:
 *   The engine will compute per-seat classifications from the structural data. For modern_state_legal_monopoly and commercial_civil_society (beneficiaries with high power, mobile exit), the constraint computes as Mountain or Rope — genuine coordination with negligible extraction. For vulnerable_populations_protected_from_honor_violence (beneficiary with low power, constrained exit), it computes as Mountain — a protective natural law. For residual_honor_adherents (payer with identity_locked exit), it computes as Snare or Tangled Rope — a constraint that extracts their meaningful practice while presenting as natural law. For traditional_aristocratic_military_classes (historical payer), it computed as Snare during the transition but they are largely extinct as a class by 1900. The divergence between the dominant-framework seats (Mountain) and the residual-honor seat (Snare) is the measurement the corpus exists to take.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (state, commerce, vulnerable populations) sit at d≈0.1-0.2 — the constraint subsidizes them by providing order, predictability, and protection. Victims (residual honor adherents, traditional elites) sit at d≈0.8-0.9 — the constraint extracts their cultural practice and status. The identity_locked exit of residual honor adherents amplifies their effective extraction: they cannot exit the constraint without exiting their identity. The state's arbitrage-grade exit (it could change the law but chooses not to) damps its extraction to near-zero. The directionality derivation from beneficiary/victim declarations + power + exit produces this gradient automatically.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (private honor violence destabilizing social order) is largely dead in modern societies — state monopoly on violence has succeeded. Yet the constraint persists not as a scaffold (no sunset clause) but as a cognitive Mountain. This is not mandatrophy in the classic sense (a function that atrophied while the form remains) — the function (peaceful dispute resolution) is live and the form (legal/cultural prohibition) serves it. However, the FSM risk is real: the constraint presents as natural law while benefiting identifiable agents. The mandatrophy question here is whether the COGNITIVE UNTHINKABILITY itself — not the legal prohibition — serves a current function or is an inertial remnant of the transformation. The contraction reading says the unthinkability IS the function (it makes the legal prohibition self-enforcing). The drop and composite readings suggest the unthinkability is incomplete or overdetermined.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_constructed_cognitive_limit,
    'Is the cognitive unthinkability of dueling a genuine natural law of modern consciousness, or a constructed constraint that benefits identifiable agents (state, commerce, vulnerable populations)?',
    'Cross-cultural comparison: if societies without the specific historical trajectory of European state formation also develop cognitive unthinkability of honor violence at similar modernization thresholds, the natural-law reading gains support. If the pattern tracks specific institutional histories, the constructed reading gains support.',
    'If natural law, the constraint is a genuine Mountain with ε≈0 for all seats. If constructed, it is a False Summit Mountain (FSM candidate) that extracts from residual honor adherents and traditional elites while presenting as inevitable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_cognitive_limit, empirical, 'Whether cognitive unthinkability is a universal developmental milestone or a historically contingent construction with beneficiaries.').

omega_variable(
    kernel_reading_contraction_vs_drop,
    'Does the contraction reading''s claim that honor culture exits the normative possibility space foreclose the drop reading''s claim that dueling persisted as fringe practice, or do they describe different population segments at the same historical moment?',
    'Demographic and geographic granularity: if contraction describes the dominant cultural framework while drop describes statistically marginal but socially visible subgroups, the readings coexist. If contraction claims total population coverage, it forecloses drop.',
    'If forecloses, the kernel has a logical contradiction between readings. If coexists_with, the kernel describes a fragmented cultural landscape where both readings are partially true for different groups.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contraction_vs_drop, conceptual, 'Structural relationship between contraction and drop readings of the honor_settlement_legitimacy kernel.').

omega_variable(
    kernel_reading_contraction_vs_composite,
    'Does the composite reading''s overdetermination thesis (multiple mechanisms including contraction) structurally incorporate the contraction reading as a component, or does it offer a competing explanation that dilutes contraction''s causal primacy?',
    'Causal weight analysis: if historical evidence shows contraction as a necessary but insufficient mechanism that only operates in combination with state coercion, economic transformation, and cultural diffusion, then contraction influences composite. If contraction alone suffices to explain the outcome, composite is redundant.',
    'If influences, contraction creates downstream pressure on composite''s legitimacy conditions. If coexists_with, they are parallel explanations for different audiences.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contraction_vs_composite, conceptual, 'Structural relationship between contraction and composite readings of the honor_settlement_legitimacy kernel.').

omega_variable(
    suppression_mechanism_internalized_vs_structural,
    'Is the near-zero suppression metric structural (no enforcement needed because alternatives genuinely collapsed) or internalized (the target population has absorbed the constraint so thoroughly that they self-police)?',
    'Counterfactual perturbation: if the state monopoly on violence were credibly threatened, would honor violence re-emerge rapidly (suggesting internalized suppression) or remain dormant (suggesting genuine cognitive transformation)? Historical evidence from state collapse episodes (e.g., post-Soviet, post-colonial) informs this.',
    'If internalized, effective suppression is higher than the structural measure suggests — the constraint travels with the subject. If structural, the Mountain classification is more robust.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalized_vs_structural, empirical, 'Whether the absence of visible enforcement reflects genuine alternative collapse or internalized suppression carried by transformed subjects.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(honor_settlement_legitimacy__contraction_reading, 1750, 1900).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(honor_settlement_legitimacy__contraction_reading_tr_t1750, honor_settlement_legitimacy__contraction_reading, theater_ratio, 1750, 0.3).
narrative_ontology:measurement(honor_settlement_legitimacy__contraction_reading_tr_t1775, honor_settlement_legitimacy__contraction_reading, theater_ratio, 1775, 0.22).
narrative_ontology:measurement(honor_settlement_legitimacy__contraction_reading_tr_t1800, honor_settlement_legitimacy__contraction_reading, theater_ratio, 1800, 0.15).
narrative_ontology:measurement(honor_settlement_legitimacy__contraction_reading_tr_t1825, honor_settlement_legitimacy__contraction_reading, theater_ratio, 1825, 0.1).
narrative_ontology:measurement(honor_settlement_legitimacy__contraction_reading_tr_t1850, honor_settlement_legitimacy__contraction_reading, theater_ratio, 1850, 0.07).
narrative_ontology:measurement(honor_settlement_legitimacy__contraction_reading_tr_t1875, honor_settlement_legitimacy__contraction_reading, theater_ratio, 1875, 0.06).
narrative_ontology:measurement(honor_settlement_legitimacy__contraction_reading_tr_t1900, honor_settlement_legitimacy__contraction_reading, theater_ratio, 1900, 0.05).

% Extraction over time
narrative_ontology:measurement(honor_settlement_legitimacy__contraction_reading_be_t1750, honor_settlement_legitimacy__contraction_reading, base_extractiveness, 1750, 0.45).
narrative_ontology:measurement(honor_settlement_legitimacy__contraction_reading_be_t1775, honor_settlement_legitimacy__contraction_reading, base_extractiveness, 1775, 0.35).
narrative_ontology:measurement(honor_settlement_legitimacy__contraction_reading_be_t1800, honor_settlement_legitimacy__contraction_reading, base_extractiveness, 1800, 0.28).
narrative_ontology:measurement(honor_settlement_legitimacy__contraction_reading_be_t1825, honor_settlement_legitimacy__contraction_reading, base_extractiveness, 1825, 0.22).
narrative_ontology:measurement(honor_settlement_legitimacy__contraction_reading_be_t1850, honor_settlement_legitimacy__contraction_reading, base_extractiveness, 1850, 0.18).
narrative_ontology:measurement(honor_settlement_legitimacy__contraction_reading_be_t1875, honor_settlement_legitimacy__contraction_reading, base_extractiveness, 1875, 0.16).
narrative_ontology:measurement(honor_settlement_legitimacy__contraction_reading_be_t1900, honor_settlement_legitimacy__contraction_reading, base_extractiveness, 1900, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(honor_settlement_legitimacy__contraction_reading_su_t1750, honor_settlement_legitimacy__contraction_reading, suppression_requirement, 1750, 0.6).
narrative_ontology:measurement(honor_settlement_legitimacy__contraction_reading_su_t1775, honor_settlement_legitimacy__contraction_reading, suppression_requirement, 1775, 0.45).
narrative_ontology:measurement(honor_settlement_legitimacy__contraction_reading_su_t1800, honor_settlement_legitimacy__contraction_reading, suppression_requirement, 1800, 0.3).
narrative_ontology:measurement(honor_settlement_legitimacy__contraction_reading_su_t1825, honor_settlement_legitimacy__contraction_reading, suppression_requirement, 1825, 0.2).
narrative_ontology:measurement(honor_settlement_legitimacy__contraction_reading_su_t1850, honor_settlement_legitimacy__contraction_reading, suppression_requirement, 1850, 0.15).
narrative_ontology:measurement(honor_settlement_legitimacy__contraction_reading_su_t1875, honor_settlement_legitimacy__contraction_reading, suppression_requirement, 1875, 0.12).
narrative_ontology:measurement(honor_settlement_legitimacy__contraction_reading_su_t1900, honor_settlement_legitimacy__contraction_reading, suppression_requirement, 1900, 0.1).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(honor_settlement_legitimacy__contraction_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(honor_settlement_legitimacy__contraction_reading, 0.08).
narrative_ontology:affects_constraint(honor_settlement_legitimacy__contraction_reading, honor_settlement_legitimacy__drop_reading).
narrative_ontology:affects_constraint(honor_settlement_legitimacy__contraction_reading, honor_settlement_legitimacy__composite_reading).

% DUAL FORMULATION NOTE:
% This constraint family (honor_settlement_legitimacy) decomposes the kernel into three readings with distinct ε values and structural profiles. Contraction reading: ε≈0.15, Mountain (FSM candidate). Drop reading: ε≈0.40 for residual adherents, Snare/Tangled Rope for that subgroup. Composite reading: ε≈0.30, Tangled Rope (multiple mechanisms, active enforcement during transition). The contraction reading's claim of cognitive unthinkability forecloses the drop reading's claim of persistent fringe practice IF contraction claims total population coverage; they coexist if contraction describes only the dominant framework. The composite reading incorporates contraction as one mechanism, so contraction influences composite.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(honor_settlement_legitimacy__contraction_reading, moderate, 0.85).
constraint_indexing:directionality_override(honor_settlement_legitimacy__contraction_reading, powerful, 0.75).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
