% ============================================================================
% CONSTRAINT STORY: qwerty_persistence_inevitability__strategic_lock_in_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_qwerty_persistence_inevitability__strategic_lock_in_reading, []).

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
 *   constraint_id: qwerty_persistence_inevitability__strategic_lock_in_reading
 *   human_readable: QWERTY Persistence as Manufacturer-Engineered Lock-In
 *   domain: technology_history/political_economy/institutional_analysis
 *
 * SUMMARY:
 *   This constraint story instantiates the strategic_lock_in_reading of the
 *   QWERTY persistence kernel. It reads QWERTY's 150-year dominance not as
 *   accidental path dependency but as manufacturer-engineered lock-in: the
 *   1893 Typewriter Trust cartel used training partnerships (Remington typing
 *   schools, certification pipelines) and active suppression of competing
 *   layouts to manufacture inevitability. The coordination function
 *   (universal typing skill standard) was real but time-bound; the extraction
 *   function (monopoly rents from an entrenched standard, ergonomic costs
 *   externalized to typists) persisted long after the Trust dissolved. The
 *   claimed type is tangled_rope — genuine coordination hybridized with
 *   asymmetric extraction maintained by active enforcement (Trust patents,
 *   training exclusivity, procurement standards). The path_dependency_reading
 *   is the sibling; this reading forecloses it structurally.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(qwerty_persistence_inevitability__strategic_lock_in_reading, 0.68).
domain_priors:suppression_score(qwerty_persistence_inevitability__strategic_lock_in_reading, 0.72).
domain_priors:theater_ratio(qwerty_persistence_inevitability__strategic_lock_in_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(qwerty_persistence_inevitability__strategic_lock_in_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(qwerty_persistence_inevitability__strategic_lock_in_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(qwerty_persistence_inevitability__strategic_lock_in_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(qwerty_persistence_inevitability__strategic_lock_in_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(qwerty_persistence_inevitability__strategic_lock_in_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(qwerty_persistence_inevitability__strategic_lock_in_reading, tangled_rope).
narrative_ontology:human_readable(qwerty_persistence_inevitability__strategic_lock_in_reading, "QWERTY Persistence as Manufacturer-Engineered Lock-In").
narrative_ontology:topic_domain(qwerty_persistence_inevitability__strategic_lock_in_reading, "technology_history/political_economy/institutional_analysis").

domain_priors:requires_active_enforcement(qwerty_persistence_inevitability__strategic_lock_in_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(qwerty_persistence_inevitability__strategic_lock_in_reading, '11bf7f9e-1789-4fb8-b48e-cc1bdebd4fcb').
narrative_ontology:cs_kernel_codification('11bf7f9e-1789-4fb8-b48e-cc1bdebd4fcb', distributed).
narrative_ontology:cs_authority_grounding('11bf7f9e-1789-4fb8-b48e-cc1bdebd4fcb', extraction).
narrative_ontology:cs_interpretation_layer_present('11bf7f9e-1789-4fb8-b48e-cc1bdebd4fcb').
narrative_ontology:cs_reading_relation('11bf7f9e-1789-4fb8-b48e-cc1bdebd4fcb', qwerty_persistence_inevitability__path_dependency_reading, forecloses).
narrative_ontology:cs_axiom('11bf7f9e-1789-4fb8-b48e-cc1bdebd4fcb', foundational, qwerty_persistence_is_manufacturer_engineered).
narrative_ontology:cs_axiom_status(qwerty_persistence_is_manufacturer_engineered, holdable).
narrative_ontology:cs_axiom_grounding('11bf7f9e-1789-4fb8-b48e-cc1bdebd4fcb', qwerty_persistence_is_manufacturer_engineered, empirically_contingent).
narrative_ontology:cs_axiom('11bf7f9e-1789-4fb8-b48e-cc1bdebd4fcb', secondary, cartel_standardization_drove_lock_in).
narrative_ontology:cs_axiom_status(cartel_standardization_drove_lock_in, holdable).
narrative_ontology:cs_axiom_grounding('11bf7f9e-1789-4fb8-b48e-cc1bdebd4fcb', cartel_standardization_drove_lock_in, empirically_contingent).
narrative_ontology:cs_reference_frame('11bf7f9e-1789-4fb8-b48e-cc1bdebd4fcb', engineered_standardization_cartel_control).
narrative_ontology:cs_drift_state('11bf7f9e-1789-4fb8-b48e-cc1bdebd4fcb', post_scholarly_revisionist_era, gap(revival_pressure, substantial, false)).
narrative_ontology:cs_created_at('11bf7f9e-1789-4fb8-b48e-cc1bdebd4fcb', '').
narrative_ontology:cs_kernel_id(qwerty_persistence_inevitability__strategic_lock_in_reading, qwerty_persistence_inevitability).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(qwerty_persistence_inevitability__strategic_lock_in_reading, typewriter_trust_cartel_1893).
narrative_ontology:constraint_beneficiary(qwerty_persistence_inevitability__strategic_lock_in_reading, remington_typewriter_company).
narrative_ontology:constraint_beneficiary(qwerty_persistence_inevitability__strategic_lock_in_reading, underwood_typewriter_company).
narrative_ontology:constraint_beneficiary(qwerty_persistence_inevitability__strategic_lock_in_reading, typing_school_certification_bodies).
narrative_ontology:constraint_victim(qwerty_persistence_inevitability__strategic_lock_in_reading, professional_typists).
narrative_ontology:constraint_victim(qwerty_persistence_inevitability__strategic_lock_in_reading, clerical_workers).
narrative_ontology:constraint_victim(qwerty_persistence_inevitability__strategic_lock_in_reading, office_workers_generational).
narrative_ontology:constraint_victim(qwerty_persistence_inevitability__strategic_lock_in_reading, alternative_layout_innovators).
narrative_ontology:constraint_vindicates(qwerty_persistence_inevitability__strategic_lock_in_reading, manufactured_inevitability_doctrine).
narrative_ontology:constraint_vindicates(qwerty_persistence_inevitability__strategic_lock_in_reading, strategic_standardization_theory).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% A syndicate of major typewriter manufacturers (Remington, Underwood, Smith Premier, others) that coordinated production quotas, pricing, and keyboard standardization. They controlled the Typewriter Trust (1893-1910) and used training school partnerships to cement QWERTY as the only taught layout. They extracted monopoly rents from the installed base and training pipeline.
narrative_ontology:constraint_stakeholder(qwerty_persistence_inevitability__strategic_lock_in_reading, typewriter_trust_cartel_1893, agenda_setter,
    institutional, generational, arbitrage, global).

% The original QWERTY manufacturer (from 1873). Funded and operated Remington typing schools, certified typists exclusively on QWERTY, and leveraged the Trust to suppress competing keyboard layouts. Collected both typewriter sales revenue and ecosystem rents from the trained typist pool.
narrative_ontology:constraint_stakeholder(qwerty_persistence_inevitability__strategic_lock_in_reading, remington_typewriter_company, beneficiary,
    powerful, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(qwerty_persistence_inevitability__strategic_lock_in_reading, remington_typewriter_company, agenda_setter).

% Major Trust member that adopted QWERTY to access the Remington-trained typist pool. Benefited from the standardization cartel without bearing the initial R&D cost. Used Trust enforcement to prevent layout fragmentation that would have diluted their market position.
narrative_ontology:constraint_stakeholder(qwerty_persistence_inevitability__strategic_lock_in_reading, underwood_typewriter_company, beneficiary,
    powerful, generational, mobile, global).

% Commercial typing schools (Remington-affiliated and independent) that taught touch-typing exclusively on QWERTY. Their business model depended on certifying a uniform skill transferable across employers. They lobbied for QWERTY standardization in civil service exams and commercial curricula, collecting tuition and certification fees.
narrative_ontology:constraint_stakeholder(qwerty_persistence_inevitability__strategic_lock_in_reading, typing_school_certification_bodies, beneficiary,
    organized, biographical, constrained, national).

% Clerical workers (overwhelmingly women by 1900) who invested months learning QWERTY touch-typing. Their professional identity and employability fused with the layout. Retraining to an alternative layout meant months of lost wages and identity disruption. They bore ergonomic costs (higher finger travel, left-hand bias) with no viable exit.
narrative_ontology:constraint_stakeholder(qwerty_persistence_inevitability__strategic_lock_in_reading, professional_typists, payer,
    moderate, biographical, identity_locked, global).

% Office workers who typed incidentally but were required to use QWERTY equipment. Had less identity investment than professional typists but faced the same equipment lock-in. Could not individually choose alternative layouts because procurement was centralized and training assumed QWERTY.
narrative_ontology:constraint_stakeholder(qwerty_persistence_inevitability__strategic_lock_in_reading, clerical_workers, payer,
    powerless, biographical, constrained, global).

% Successive cohorts entering office work who found QWERTY as the only available skill standard. Each generation's training investment reinforced the lock-in for the next. The ergonomic penalty compounded across generations as keyboard use expanded from specialized typists to universal computer users.
narrative_ontology:constraint_stakeholder(qwerty_persistence_inevitability__strategic_lock_in_reading, office_workers_generational, payer,
    moderate, generational, constrained, global).

% Inventors of competing layouts (Dvorak 1930s, Blickensderfer, others) who were blocked by the Trust's control of manufacturing, distribution, and training pipelines. Even after the Trust dissolved (1911), the installed base and training infrastructure they created persisted as a de facto barrier.
narrative_ontology:constraint_stakeholder(qwerty_persistence_inevitability__strategic_lock_in_reading, alternative_layout_innovators, excluded,
    moderate, biographical, trapped, global).

% Scholars (Paul David, Brian Arthur, W. Brian Arthur) who advanced the path dependency reading: QWERTY persisted due to historical accident and increasing returns, not strategic action. Their framework became dominant in economics (1985-2000s) and shaped policy understanding of technology lock-in.
narrative_ontology:constraint_stakeholder(qwerty_persistence_inevitability__strategic_lock_in_reading, economic_historians_path_dependency_school, observer,
    analytical, civilizational, analytical, global).

% Scholars (Stan Liebowitz, Stephen Margolis, others) who challenged path dependency using empirical evidence: QWERTY efficiency parity, lack of Trust enforcement after 1911, market tests of Dvorak. They read the constraint as manufactured inevitability rather than accident.
narrative_ontology:constraint_stakeholder(qwerty_persistence_inevitability__strategic_lock_in_reading, revisionist_economic_historians, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The typewriter manufacturers solved a genuine coordination problem: creating a universal typing skill standard so that any typist could use any machine, and any employer could hire any certified typist. This reduced transaction costs in the emerging clerical labor market.
% TRANSFER_FUNCTION: Moves ergonomic efficiency and retraining option value from all keyboard users (typists, clerical workers, generations of office workers) to the manufacturer cartel and training ecosystem, via enforced standardization that prevents layout competition.
% ABSENT_VOICES: Early female typists (1880s-1910s) who bore the ergonomic costs but had no professional representation; Dvorak and other layout innovators excluded from manufacturing and training channels; contemporary RSI sufferers whose ergonomic burden traces to the 1893 standardization decision but are not consulted in keyboard standards.
% DISAPPEARANCE_RATIONALE: If the engineered lock-in constraint vanished overnight, keyboard layout competition would resume immediately. Alternative layouts (Dvorak, Colemak, ergonomic splits) would gain market share through open competition. Training pipelines would diversify. The ergonomic penalty paid by billions of keyboard users over 130+ years would become a policy question rather than an inherited fact.
% FOUNDING_PROBLEM: The typewriter industry (1870s-1890s) faced fragmented keyboard layouts, incompatible machines, and no standard for typing skill. Manufacturers could not achieve scale; employers could not hire interchangeable typists; training schools had no stable curriculum.
% FOUNDING_PROBLEM_CORROBORATION: The coordination problem (fragmented layouts, no skill standard) was genuinely solved by 1900. The revisionist historians (Liebowitz & Margolis 1990, 1995; David 1985 rebuttal) document that the Trust dissolved in 1911, yet QWERTY persisted and deepened — the arrangement outlived its founding problem by a century. No independent source attests the original coordination problem persists; the constraint's current form serves extraction, not coordination.
narrative_ontology:disappearance_verdict(qwerty_persistence_inevitability__strategic_lock_in_reading, world_rearranges).
narrative_ontology:founding_problem_status(qwerty_persistence_inevitability__strategic_lock_in_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(qwerty_persistence_inevitability__strategic_lock_in_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(qwerty_persistence_inevitability__strategic_lock_in_reading, 'none', 1).
narrative_ontology:epsilon_provenance(qwerty_persistence_inevitability__strategic_lock_in_reading, 0.68, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(qwerty_persistence_inevitability__strategic_lock_in_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(qwerty_persistence_inevitability__strategic_lock_in_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(qwerty_persistence_inevitability__strategic_lock_in_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68) reflects the massive ergonomic penalty and retraining barrier imposed on billions of users, extracted as monopoly rents by the cartel and its training ecosystem. Suppression (0.72) is high because the constraint required active enforcement: Trust patents blocked competing keyboards, training schools taught only QWERTY, civil service exams mandated QWERTY certification. Theater ratio (0.45) rose after 1911 — the Trust dissolved but the 'inevitability' narrative became self-sustaining; the coordination function atrophied while the extraction function persisted via institutional inertia. Accessibility collapse (0.65) is substantial: once QWERTY was universal, alternatives collapsed not from inferiority but from network effects the cartel engineered. Resistance (0.55) is moderate: Dvorak and other challengers mounted real but structurally doomed challenges.
 *
 * PERSPECTIVAL GAP:
 *   The cartel seats experience the constraint as coordination they built and maintained; the typist seats experience it as enforced extraction they cannot escape. The engine computes this divergence from the structural data: identity_locked exit for typists drives their directionality toward full target, while arbitrage exit for manufacturers drives theirs toward beneficiary. The path dependency reading obscures this asymmetry by treating all agents as symmetric victims of historical accident.
 *
 * DIRECTIONALITY LOGIC:
 *   The cartel members (Trust, Remington, Underwood) are structural beneficiaries (d near 0.0): they collected rents, controlled the standard, had arbitrage-grade exit (could have adopted better layouts but chose not to). Typing schools are secondary beneficiaries (d ~0.2): they profited from the certification monopoly but were constrained by the manufacturers' control. Professional typists are identity-locked payers (d near 1.0): their professional identity fused with QWERTY skill, making exit psychologically and economically prohibitive. Clerical workers are constrained payers (d ~0.7): less identity investment but same structural trap. Alternative innovators are trapped (d=1.0): structurally excluded. The two observer seats (path dependency historians, revisionist historians) are analytical (d=0.5) — they analyze the structure from outside.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (fragmented layouts, no skill standard) was solved by ~1900. The Trust dissolved in 1911. Yet the constraint persisted and deepened — the arrangement outlived its mandate by over a century. This is classic mandatrophy: a coordination scaffold that became a tangled rope when the extraction function (monopoly rents, ergonomic externalization) detached from the coordination function and persisted via institutional inertia and narrative capture ('QWERTY is inevitable'). The founding_problem_status=dead with disappearance_verdict=world_rearranges flags this as a zombie constraint.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_relationship,
    'Is the strategic_lock_in_reading a distinct constraint from the path_dependency_reading, or are they observables of the same constraint evaluated differently?',
    'Apply the epsilon-invariance test: if the two readings produce different beneficiary/victim structures and different ε values for the same historical period, they are distinct constraints linked by network.affects_constraints, not one constraint with measurement ambiguity.',
    'If distinct, each gets its own classification (tangled_rope vs rope) and the kernel decomposition is validated. If not distinct, the framework''s core ε-invariance principle is violated.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_relationship, conceptual, 'Whether the kernel decomposes into two constraints or one constraint with observer-dependent classification.').

omega_variable(
    trust_enforcement_persistence,
    'Did the Typewriter Trust''s active enforcement (patents, training exclusivity, procurement control) persist in structural form after its formal 1911 dissolution, or did the lock-in become self-sustaining via network effects alone?',
    'Archival research on post-1911 keyboard procurement standards, typing curriculum mandates, and patent litigation. If enforcement mechanisms persisted without the Trust, suppression_requirement trajectory is endogenous; if not, the post-1911 suppression is inertial theater.',
    'If enforcement persisted structurally, the tangled_rope classification holds throughout. If enforcement decayed and only network effects remained, the constraint may transition toward piton (degraded coordination, inertial persistence) after 1911.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(trust_enforcement_persistence, empirical, 'Whether active enforcement or inertial network effects drove post-1911 lock-in persistence.').

omega_variable(
    ergonomic_cost_measurement,
    'What is the quantified ergonomic penalty of QWERTY vs. optimized layouts (Dvorak, Colemak) in finger travel, same-finger bigrams, and RSI incidence?',
    'Biomechanical studies comparing layouts; epidemiological data on RSI rates by layout adoption (limited by selection effects).',
    'A large measured penalty strengthens the extraction claim; a small or zero penalty weakens it and supports the path_dependency_reading''s efficiency parity argument.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ergonomic_cost_measurement, empirical, 'Whether the extraction manifest in ergonomic harm is empirically substantiated.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (Trust patents, training mandates, procurement rules) or internalized (typists believing QWERTY is ''natural'' or ''the only way'')?',
    'Post-exit suppression trajectory: if suppression persists after the extractive mechanism is removed (e.g., Dvorak available but not adopted), reclassify as partially internalized.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — the target carries the suppression with them after exit. This would increase the constraint''s extractiveness for identity-locked agents.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism in the QWERTY lock-in.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(qwerty_persistence_inevitability__strategic_lock_in_reading, 1873, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qwer_tr_t1873, qwerty_persistence_inevitability__strategic_lock_in_reading, theater_ratio, 1873, 0.1).
narrative_ontology:measurement(qwer_tr_t1888, qwerty_persistence_inevitability__strategic_lock_in_reading, theater_ratio, 1888, 0.25).
narrative_ontology:measurement(qwer_tr_t1893, qwerty_persistence_inevitability__strategic_lock_in_reading, theater_ratio, 1893, 0.35).
narrative_ontology:measurement(qwer_tr_t1911, qwerty_persistence_inevitability__strategic_lock_in_reading, theater_ratio, 1911, 0.42).
narrative_ontology:measurement(qwer_tr_t1936, qwerty_persistence_inevitability__strategic_lock_in_reading, theater_ratio, 1936, 0.44).
narrative_ontology:measurement(qwer_tr_t1985, qwerty_persistence_inevitability__strategic_lock_in_reading, theater_ratio, 1985, 0.45).
narrative_ontology:measurement(qwer_tr_t2024, qwerty_persistence_inevitability__strategic_lock_in_reading, theater_ratio, 2024, 0.45).

% Extraction over time
narrative_ontology:measurement(qwer_be_t1873, qwerty_persistence_inevitability__strategic_lock_in_reading, base_extractiveness, 1873, 0.15).
narrative_ontology:measurement(qwer_be_t1888, qwerty_persistence_inevitability__strategic_lock_in_reading, base_extractiveness, 1888, 0.35).
narrative_ontology:measurement(qwer_be_t1893, qwerty_persistence_inevitability__strategic_lock_in_reading, base_extractiveness, 1893, 0.55).
narrative_ontology:measurement(qwer_be_t1911, qwerty_persistence_inevitability__strategic_lock_in_reading, base_extractiveness, 1911, 0.62).
narrative_ontology:measurement(qwer_be_t1936, qwerty_persistence_inevitability__strategic_lock_in_reading, base_extractiveness, 1936, 0.65).
narrative_ontology:measurement(qwer_be_t1985, qwerty_persistence_inevitability__strategic_lock_in_reading, base_extractiveness, 1985, 0.68).
narrative_ontology:measurement(qwer_be_t2024, qwerty_persistence_inevitability__strategic_lock_in_reading, base_extractiveness, 2024, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(qwer_su_t1873, qwerty_persistence_inevitability__strategic_lock_in_reading, suppression_requirement, 1873, 0.2).
narrative_ontology:measurement(qwer_su_t1888, qwerty_persistence_inevitability__strategic_lock_in_reading, suppression_requirement, 1888, 0.45).
narrative_ontology:measurement(qwer_su_t1893, qwerty_persistence_inevitability__strategic_lock_in_reading, suppression_requirement, 1893, 0.65).
narrative_ontology:measurement(qwer_su_t1911, qwerty_persistence_inevitability__strategic_lock_in_reading, suppression_requirement, 1911, 0.55).
narrative_ontology:measurement(qwer_su_t1936, qwerty_persistence_inevitability__strategic_lock_in_reading, suppression_requirement, 1936, 0.6).
narrative_ontology:measurement(qwer_su_t1985, qwerty_persistence_inevitability__strategic_lock_in_reading, suppression_requirement, 1985, 0.7).
narrative_ontology:measurement(qwer_su_t2024, qwerty_persistence_inevitability__strategic_lock_in_reading, suppression_requirement, 2024, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(qwerty_persistence_inevitability__strategic_lock_in_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(qwerty_persistence_inevitability__strategic_lock_in_reading, 0.12).
narrative_ontology:affects_constraint(qwerty_persistence_inevitability__strategic_lock_in_reading, qwerty_persistence_inevitability__path_dependency_reading).
narrative_ontology:affects_constraint(qwerty_persistence_inevitability__strategic_lock_in_reading, computer_keyboard_standardization).
narrative_ontology:affects_constraint(qwerty_persistence_inevitability__strategic_lock_in_reading, touch_typing_pedagogy_standard).

% DUAL FORMULATION NOTE:
% This constraint and path_dependency_reading form the QWERTY kernel family. The strategic reading identifies a cartel-driven extraction layer (tangled_rope) that the path dependency reading treats as coordination-only (rope). The ε values differ: this reading authors ε=0.68 (high extraction from ergonomic externalization and retraining barriers); the path dependency reading would author ε≈0.15 (coordination costs only). They are linked as sibling readings of the same kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(qwerty_persistence_inevitability__strategic_lock_in_reading, organized, 0.15).
constraint_indexing:directionality_override(qwerty_persistence_inevitability__strategic_lock_in_reading, moderate, 0.75).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
