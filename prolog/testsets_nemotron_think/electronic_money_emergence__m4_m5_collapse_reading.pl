% ============================================================================
% CONSTRAINT STORY: electronic_money_emergence__m4_m5_collapse_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_electronic_money_emergence__m4_m5_collapse_reading, []).

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
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: electronic_money_emergence__m4_m5_collapse_reading
 *   human_readable: M4/M5 Statistical Distinction as Retroactive Category Creator for Electronic Money
 *   domain: economic/historical/epistemic
 *
 * SUMMARY:
 *   The M4/M5 monetary aggregate distinction, introduced by central banks in
 *   the 1980s as a statistical tool for tracking financial innovation, has
 *   retroactively become the ontological boundary that defines 'electronic
 *   money' as a historical category. This reading argues there was no genuine
 *   emergence event — no moment when money 'became electronic' in the physics
 *   of monetary practice. Rather, the statistical convention created the
 *   category ex post, and the category then stabilized itself through
 *   textbook repetition, regulatory adoption, and the professional identity
 *   of monetary economists. The constraint is a classificatory piton: its
 *   original coordination function (comparable measurement) is dead, but the
 *   distinction persists through institutional inertia and the theatrical
 *   maintenance of a natural-kind ontology.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(electronic_money_emergence__m4_m5_collapse_reading, 0.38).
domain_priors:suppression_score(electronic_money_emergence__m4_m5_collapse_reading, 0.22).
domain_priors:theater_ratio(electronic_money_emergence__m4_m5_collapse_reading, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(electronic_money_emergence__m4_m5_collapse_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(electronic_money_emergence__m4_m5_collapse_reading, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(electronic_money_emergence__m4_m5_collapse_reading, theater_ratio, 0.68).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(electronic_money_emergence__m4_m5_collapse_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(electronic_money_emergence__m4_m5_collapse_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(electronic_money_emergence__m4_m5_collapse_reading, piton).
narrative_ontology:human_readable(electronic_money_emergence__m4_m5_collapse_reading, "M4/M5 Statistical Distinction as Retroactive Category Creator for Electronic Money").
narrative_ontology:topic_domain(electronic_money_emergence__m4_m5_collapse_reading, "economic/historical/epistemic").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(electronic_money_emergence__m4_m5_collapse_reading, 'dc9ffe36-c47d-4bf6-924b-7f2c81765021').
narrative_ontology:cs_kernel_codification('dc9ffe36-c47d-4bf6-924b-7f2c81765021', formalized).
narrative_ontology:cs_authority_grounding('dc9ffe36-c47d-4bf6-924b-7f2c81765021', lineage).
narrative_ontology:cs_interpretation_layer_present('dc9ffe36-c47d-4bf6-924b-7f2c81765021').
narrative_ontology:cs_reading_relation('dc9ffe36-c47d-4bf6-924b-7f2c81765021', electronic_money_emergence__became_thinkable_reading, forecloses).
narrative_ontology:cs_reading_relation('dc9ffe36-c47d-4bf6-924b-7f2c81765021', electronic_money_emergence__first_held_reading, coexists_with).
narrative_ontology:cs_axiom('dc9ffe36-c47d-4bf6-924b-7f2c81765021', foundational, statistical_categories_construct_their_objects).
narrative_ontology:cs_axiom_status(statistical_categories_construct_their_objects, holdable).
narrative_ontology:cs_axiom_grounding('dc9ffe36-c47d-4bf6-924b-7f2c81765021', statistical_categories_construct_their_objects, empirically_contingent).
narrative_ontology:cs_axiom('dc9ffe36-c47d-4bf6-924b-7f2c81765021', secondary, measurement_conventions_naturalize_through_repetition).
narrative_ontology:cs_axiom_status(measurement_conventions_naturalize_through_repetition, holdable).
narrative_ontology:cs_axiom_grounding('dc9ffe36-c47d-4bf6-924b-7f2c81765021', measurement_conventions_naturalize_through_repetition, conventional).
narrative_ontology:cs_reference_frame('dc9ffe36-c47d-4bf6-924b-7f2c81765021', statistical_measurement_convention_1980s).
narrative_ontology:cs_drift_state('dc9ffe36-c47d-4bf6-924b-7f2c81765021', contemporary_monetary_ontology, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('dc9ffe36-c47d-4bf6-924b-7f2c81765021', '').
narrative_ontology:cs_kernel_id(electronic_money_emergence__m4_m5_collapse_reading, electronic_money_emergence).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(electronic_money_emergence__m4_m5_collapse_reading, central_bank_statistical_departments).
narrative_ontology:constraint_beneficiary(electronic_money_emergence__m4_m5_collapse_reading, monetary_economists_textbook_authors).
narrative_ontology:constraint_beneficiary(electronic_money_emergence__m4_m5_collapse_reading, financial_stability_regulators).
narrative_ontology:constraint_victim(electronic_money_emergence__m4_m5_collapse_reading, monetary_historians_pre_digital).
narrative_ontology:constraint_victim(electronic_money_emergence__m4_m5_collapse_reading, alternative_monetary_theorists).
narrative_ontology:constraint_victim(electronic_money_emergence__m4_m5_collapse_reading, pre_m4_m5_archival_record).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(electronic_money_emergence__m4_m5_collapse_reading, financial_stability_regulators).
narrative_ontology:constraint_vindicates(electronic_money_emergence__m4_m5_collapse_reading, measurement_categories_are_ontologically_neutral).
narrative_ontology:constraint_vindicates(electronic_money_emergence__m4_m5_collapse_reading, statistical_conventions_stabilize_themselves).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Produce and maintain the M4/M5 monetary aggregates that define what counts as electronic money. The distinction originated as a practical reporting convention for tracking financial innovation but has become the authoritative ontology. They face no cost to maintaining it and gain analytical coherence for policy.
narrative_ontology:constraint_stakeholder(electronic_money_emergence__m4_m5_collapse_reading, central_bank_statistical_departments, agenda_setter,
    institutional, generational, arbitrage, global).

% Inherit a clean analytical category — 'electronic money' — that structures teaching, modeling, and policy discourse. The category appears as a discovered natural kind rather than a constructed boundary. Changing frameworks would require rewriting decades of literature and curricula.
narrative_ontology:constraint_stakeholder(electronic_money_emergence__m4_m5_collapse_reading, monetary_economists_textbook_authors, beneficiary,
    organized, biographical, constrained, global).

% Use the M4/M5 boundary to monitor systemic risk from non-bank money creation. They benefit from a stable measuring rod but pay when the category obscures novel financial forms that don't fit the 1980s-era distinction (e.g., stablecoins, tokenized deposits).
narrative_ontology:constraint_stakeholder(electronic_money_emergence__m4_m5_collapse_reading, financial_stability_regulators, beneficiary,
    institutional, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(electronic_money_emergence__m4_m5_collapse_reading, financial_stability_regulators, payer).

% Must write monetary history through a category that did not exist for the period they study. The M4/M5 lens retroactively imposes 'electronic money' on arrangements (book-entry transfers, telegraphic settlements, ledger money) that contemporaries understood differently. Their professional identity is fused to the archive; exit means abandoning their field's central objects.
narrative_ontology:constraint_stakeholder(electronic_money_emergence__m4_m5_collapse_reading, monetary_historians_pre_digital, payer,
    moderate, generational, identity_locked, global).

% Develop theories (credit theory, chartalism, crypto-monetary) that treat money as socially constituted rather than statistically defined. The M4/M5 framework marginalizes these approaches by making the statistical category the default ontology. They can publish but face citation and funding barriers.
narrative_ontology:constraint_stakeholder(electronic_money_emergence__m4_m5_collapse_reading, alternative_monetary_theorists, payer,
    moderate, biographical, constrained, global).

% The historical documentary record of monetary practice before the M4/M5 distinction (clearinghouse ledgers, correspondent banking telegraphy, Eurodollar certificates). It cannot speak; it is read only through the categories imposed later. The 'emergence' of electronic money is a reading imposed on this silent record.
narrative_ontology:constraint_stakeholder(electronic_money_emergence__m4_m5_collapse_reading, pre_m4_m5_archival_record, excluded,
    powerless, civilizational, trapped, universal).
narrative_ontology:stakeholder_non_agent(electronic_money_emergence__m4_m5_collapse_reading, pre_m4_m5_archival_record).

% Sees the M4/M5 distinction as a measurement convention that achieved ontological status through institutional repetition. The constraint is the stabilization of a statistical artifact into a perceived natural kind — a classificatory piton.
narrative_ontology:constraint_stakeholder(electronic_money_emergence__m4_m5_collapse_reading, analytical_observer_monetary_epistemology, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provided a stable, internationally comparable statistical framework for tracking the shift from physical to book-entry money in the 1980s–1990s, enabling central banks to monitor broad money aggregates consistently across jurisdictions.
% TRANSFER_FUNCTION: Moves epistemic authority from historical monetary practice (what actors actually did with ledgers, telegraphs, deposits) to statistical classification (what aggregates count as M4 vs M5). The transfer is retrospective: the category claims to describe a pre-existing reality but actually constitutes it.
% ABSENT_VOICES: Monetary historians of the pre-statistical era, practitioners of pre-digital payment systems (clearinghouse clerks, telegraph operators, Eurodollar market makers), and monetary theorists who reject statistical ontology — all excluded because the M4/M5 framework presents itself as description rather than construction.
% DISAPPEARANCE_RATIONALE: If the M4/M5 distinction vanished, monetary history would lose its dominant periodization ('the emergence of electronic money'), central banks would need new reporting frameworks, and textbook monetary economics would lose its clean 'cash vs electronic' dichotomy. The analytical landscape would reorganize around functional or institutional criteria rather than statistical aggregates.
% FOUNDING_PROBLEM: Central banks in the 1980s needed a consistent way to measure broad money as financial innovation (money market funds, Eurodollars, electronic transfers) blurred the line between money and near-money. The M4/M5 split was a pragmatic reporting solution.
% FOUNDING_PROBLEM_CORROBORATION: Central bank archival records (Bank of England, Bundesbank, Fed) confirm the M4/M5 distinction was introduced as a statistical convenience for tracking financial innovation, not as an ontological claim. Independent monetary historians (e.g., Goodhart, Schularick) corroborate that the founding measurement problem was solved by the 1990s, yet the distinction persists as the master category for 'electronic money emergence.'
narrative_ontology:disappearance_verdict(electronic_money_emergence__m4_m5_collapse_reading, world_rearranges).
narrative_ontology:founding_problem_status(electronic_money_emergence__m4_m5_collapse_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(electronic_money_emergence__m4_m5_collapse_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(electronic_money_emergence__m4_m5_collapse_reading, 'none', 1).
narrative_ontology:epsilon_provenance(electronic_money_emergence__m4_m5_collapse_reading, 0.38, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(electronic_money_emergence__m4_m5_collapse_reading_tests).
:- end_tests(electronic_money_emergence__m4_m5_collapse_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38) is moderate: the constraint extracts epistemic labor from historians and alternative theorists who must translate their objects into the M4/M5 frame. Suppression (0.22) is low: no one is legally barred from using other frameworks, but the statistical ontology dominates journals, curricula, and policy. Theater ratio (0.68) is high and rising: the distinction is performed as a discovered natural boundary long after its measurement utility faded. Accessibility collapse (0.42) is moderate — alternative periodizations exist but are marginal. Resistance (0.35) is present but fragmented across disconnected communities (historians, crypto theorists, institutionalists). The measurement series shows the classic piton trajectory: low initial extraction, rising theater, stable low suppression.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda_setter seat (central bank statisticians), the constraint is a solved coordination problem — a rope that became a mountain of convenience. From the payer seats (historians, alternative theorists), it is a snare-like epistemic extraction that closes off alternative monetary ontologies. From the observer seat, it is a piton: the coordination function is dead, the extraction is real but diffuse, and the persistence is theatrical. The engine computes this divergence from the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Central bank statistical departments are agenda_setters with arbitrage-grade exit — they could change the framework but gain institutional coherence from stability. Monetary economists are beneficiaries with constrained exit — their human capital is specific to the framework. Financial stability regulators are dual-positioned: they benefit from stability but pay when the category misses novel risks. Monetary historians are payers with identity_locked exit — their professional self-concept is constituted through the archive the constraint re-reads. Alternative theorists are payers with constrained exit — they can publish but not set the agenda. The pre-M4/M5 archival record is a non-agent excluded stakeholder — it bears the retrospective imposition silently.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (comparable measurement of broad money amid 1980s financial innovation) is dead — corroborated by central bank archives and independent monetary historians. Yet the arrangement persists and has expanded its scope (now defining 'electronic money' for crypto regulation, CBDC design, and monetary history). This is mandatrophy: the mandate outlived its function, and the constraint survives by converting its measurement convention into an ontological claim. The piton classification captures this: the constraint no longer coordinates measurement; it performs the stability of the category it created.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    statistical_convention_vs_ontological_claim,
    'At what point did the M4/M5 reporting convention become treated as an ontological boundary rather than a measurement tool?',
    'Citation network analysis of central bank publications, textbooks, and regulatory documents tracking the shift from ''M5 includes...'' to ''electronic money is...''',
    'If the shift was gradual and unmarked, the piton classification is confirmed — the constraint naturalized itself. If a specific institutional decision reified the category, the constraint has a deliberative origin that changes its extraction profile.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(statistical_convention_vs_ontological_claim, empirical, 'Whether the category''s ontological status emerged gradually or by decision.').

omega_variable(
    pre_m4_m5_electronic_practices_coherence,
    'Do pre-1980s electronic payment practices (telegraphic transfers, book-entry securities, Eurodollar ledgers) form a coherent monetary phenomenon that the M4/M5 distinction merely labeled, or are they retrospectively unified by the category?',
    'Comparative archival work on whether contemporaries treated these practices as a unified ''electronic money'' phenomenon or as distinct technical operations.',
    'If contemporaries saw unity, the M4/M5 distinction recognized a real phenomenon (rope-like). If unity is retrospective, the distinction created its object (piton/snare).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(pre_m4_m5_electronic_practices_coherence, conceptual, 'Whether the category carves nature at its joints or constructs its object.').

omega_variable(
    kernel_framing_underdetermination,
    'Does the ''electronic money emergence'' kernel admit a single coherent framing, or do the sibling readings instantiate genuinely different constraints with different ε values?',
    'Structural comparison of the three readings'' beneficiary/victim sets, coordination functions, and metric profiles — if ε differs substantially across readings, the kernel is a conflation of distinct constraints.',
    'If readings have irreconcilably different ε, the kernel_id is a category error and each reading should stand alone. If ε converges, the kernel is a genuine contested interpretation of one constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_framing_underdetermination, conceptual, 'Whether the kernel decomposition follows ε-invariance or masks distinct constraints.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(electronic_money_emergence__m4_m5_collapse_reading, 1980, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(elec_tr_t1980, electronic_money_emergence__m4_m5_collapse_reading, theater_ratio, 1980, 0.15).
narrative_ontology:measurement(elec_tr_t1990, electronic_money_emergence__m4_m5_collapse_reading, theater_ratio, 1990, 0.28).
narrative_ontology:measurement(elec_tr_t2000, electronic_money_emergence__m4_m5_collapse_reading, theater_ratio, 2000, 0.42).
narrative_ontology:measurement(elec_tr_t2010, electronic_money_emergence__m4_m5_collapse_reading, theater_ratio, 2010, 0.55).
narrative_ontology:measurement(elec_tr_t2020, electronic_money_emergence__m4_m5_collapse_reading, theater_ratio, 2020, 0.63).
narrative_ontology:measurement(elec_tr_t2025, electronic_money_emergence__m4_m5_collapse_reading, theater_ratio, 2025, 0.68).

% Extraction over time
narrative_ontology:measurement(elec_be_t1980, electronic_money_emergence__m4_m5_collapse_reading, base_extractiveness, 1980, 0.12).
narrative_ontology:measurement(elec_be_t1990, electronic_money_emergence__m4_m5_collapse_reading, base_extractiveness, 1990, 0.18).
narrative_ontology:measurement(elec_be_t2000, electronic_money_emergence__m4_m5_collapse_reading, base_extractiveness, 2000, 0.25).
narrative_ontology:measurement(elec_be_t2010, electronic_money_emergence__m4_m5_collapse_reading, base_extractiveness, 2010, 0.32).
narrative_ontology:measurement(elec_be_t2020, electronic_money_emergence__m4_m5_collapse_reading, base_extractiveness, 2020, 0.36).
narrative_ontology:measurement(elec_be_t2025, electronic_money_emergence__m4_m5_collapse_reading, base_extractiveness, 2025, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(elec_su_t1980, electronic_money_emergence__m4_m5_collapse_reading, suppression_requirement, 1980, 0.08).
narrative_ontology:measurement(elec_su_t1990, electronic_money_emergence__m4_m5_collapse_reading, suppression_requirement, 1990, 0.12).
narrative_ontology:measurement(elec_su_t2000, electronic_money_emergence__m4_m5_collapse_reading, suppression_requirement, 2000, 0.15).
narrative_ontology:measurement(elec_su_t2010, electronic_money_emergence__m4_m5_collapse_reading, suppression_requirement, 2010, 0.18).
narrative_ontology:measurement(elec_su_t2020, electronic_money_emergence__m4_m5_collapse_reading, suppression_requirement, 2020, 0.22).
narrative_ontology:measurement(elec_su_t2025, electronic_money_emergence__m4_m5_collapse_reading, suppression_requirement, 2025, 0.22).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(electronic_money_emergence__m4_m5_collapse_reading, information_standard).
narrative_ontology:boltzmann_floor_override(electronic_money_emergence__m4_m5_collapse_reading, 0.02).
narrative_ontology:affects_constraint(electronic_money_emergence__m4_m5_collapse_reading, electronic_money_emergence__became_thinkable_reading).
narrative_ontology:affects_constraint(electronic_money_emergence__m4_m5_collapse_reading, electronic_money_emergence__first_held_reading).

% DUAL FORMULATION NOTE:
% This reading decomposes the 'electronic money emergence' kernel by treating the M4/M5 distinction as the constraint itself (a classificatory piton), whereas sibling readings treat emergence as a historical event located in conceptual history or institutional practice. The ε values differ: this reading shows rising extractiveness/theater from a dead coordination function; became_thinkable_reading likely shows low extraction (conceptual availability as coordination); first_held_reading likely shows moderate extraction (institutional first-mover advantage).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(electronic_money_emergence__m4_m5_collapse_reading, institutional, 0.15).
constraint_indexing:directionality_override(electronic_money_emergence__m4_m5_collapse_reading, moderate, 0.75).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
