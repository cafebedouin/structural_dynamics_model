% ============================================================================
% CONSTRAINT STORY: legitimacy_of_practice_standardization__dual_practice_equilibrium_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, []).

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
 *   constraint_id: legitimacy_of_practice_standardization__dual_practice_equilibrium_reading
 *   human_readable: Dual Practice Equilibrium: State/Traditional Legitimacy Partition
 *   domain: political/historical/institutional
 *
 * SUMMARY:
 *   This constraint describes the dual-practice equilibrium reading of the
 *   legitimacy_of_practice_standardization kernel: state authority governs
 *   public/administrative domains (taxes, bureaucracy, civil law, Gregorian
 *   calendar, Western dress for official functions) while traditional
 *   authority governs private/ritual domains (festivals, agriculture,
 *   kinship, lunar calendar, traditional dress for ceremonies). The
 *   equilibrium is stable — no convergence expected, compliance is strategic
 *   rather than internalized. This reading coexists with two sibling
 *   readings: endogenous_displacement (change from voluntary adoption) and
 *   exogenous_override (state decrees for collective benefit).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, 0.42).
domain_priors:suppression_score(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, 0.28).
domain_priors:theater_ratio(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, suppression_requirement, 0.28).
narrative_ontology:constraint_metric(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, theater_ratio, 0.35).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, resistance, 0.38).

% --- Constraint claim ---
narrative_ontology:constraint_claim(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, tangled_rope).
narrative_ontology:human_readable(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, "Dual Practice Equilibrium: State/Traditional Legitimacy Partition").
narrative_ontology:topic_domain(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, "political/historical/institutional").

domain_priors:requires_active_enforcement(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, '063de2e3-57bf-4841-824e-c60d21f6d6dc').
narrative_ontology:cs_kernel_codification('063de2e3-57bf-4841-824e-c60d21f6d6dc', distributed).
narrative_ontology:cs_authority_grounding('063de2e3-57bf-4841-824e-c60d21f6d6dc', lineage).
narrative_ontology:cs_interpretation_layer_present('063de2e3-57bf-4841-824e-c60d21f6d6dc').
narrative_ontology:cs_reading_relation('063de2e3-57bf-4841-824e-c60d21f6d6dc', legitimacy_of_practice_standardization__endogenous_displacement_reading, coexists_with).
narrative_ontology:cs_reading_relation('063de2e3-57bf-4841-824e-c60d21f6d6dc', legitimacy_of_practice_standardization__exogenous_override_reading, influences).
narrative_ontology:cs_axiom('063de2e3-57bf-4841-824e-c60d21f6d6dc', foundational, dual_legitimacy_partition_is_stable).
narrative_ontology:cs_axiom_status(dual_legitimacy_partition_is_stable, holdable).
narrative_ontology:cs_axiom_grounding('063de2e3-57bf-4841-824e-c60d21f6d6dc', dual_legitimacy_partition_is_stable, conventional).
narrative_ontology:cs_axiom('063de2e3-57bf-4841-824e-c60d21f6d6dc', foundational, compliance_is_strategic_not_internalized).
narrative_ontology:cs_axiom_status(compliance_is_strategic_not_internalized, holdable).
narrative_ontology:cs_axiom_grounding('063de2e3-57bf-4841-824e-c60d21f6d6dc', compliance_is_strategic_not_internalized, empirically_contingent).
narrative_ontology:cs_reference_frame('063de2e3-57bf-4841-824e-c60d21f6d6dc', meiji_era_settlement).
narrative_ontology:cs_drift_state('063de2e3-57bf-4841-824e-c60d21f6d6dc', postwar_occupation_reforms, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('063de2e3-57bf-4841-824e-c60d21f6d6dc', '2026-08-03T14:30:00Z').
narrative_ontology:cs_kernel_id(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, legitimacy_of_practice_standardization).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, state_bureaucracy).
narrative_ontology:constraint_beneficiary(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, traditional_ritual_authorities).
narrative_ontology:constraint_beneficiary(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, merchant_class).
narrative_ontology:constraint_victim(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, rural_households).
narrative_ontology:constraint_victim(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, migrant_workers).
narrative_ontology:constraint_victim(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, minority_communities).
narrative_ontology:constraint_vindicates(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, dual_legitimacy_equilibrium).
narrative_ontology:constraint_vindicates(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, strategic_compliance_over_internalization).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the public/administrative domain using Gregorian calendar, Western legal forms, standardized weights/measures, and bureaucratic dress codes. Legitimacy derives from fiscal efficiency, international legibility, and state capacity. Collects compliance through tax systems, licensing, and public employment.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, state_bureaucracy, agenda_setter,
    institutional, generational, arbitrage, national).

% Govern the private/ritual domain: lunar calendar for festivals/agriculture, traditional dress for ceremonies, kinship-based dispute resolution. Legitimacy derives from ancestral continuity and cosmological order. Their authority persists because the state does not contest this domain — the partition is the settlement.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, traditional_ritual_authorities, agenda_setter,
    organized, generational, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, traditional_ritual_authorities, beneficiary).

% Operates fluently in both domains: uses state-standard contracts and Gregorian schedules for commerce, traditional gift-giving calendars and kinship networks for trust. Gains from the partition because each domain's legitimacy reduces transaction costs in its sphere — no single standard imposes universal compliance costs.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, merchant_class, beneficiary,
    powerful, biographical, mobile, national).

% Must navigate both calendars for planting/harvest (lunar) and tax deadlines/loan repayments (Gregorian). Bear the cognitive and coordination cost of maintaining dual fluency. State demands (conscription, taxes, schooling) pull labor from ritual cycles; traditional demands (festivals, ancestor rites) pull labor from wage cycles. No exit from either domain.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, rural_households, payer,
    moderate, biographical, constrained, local).

% Leave rural dual-domain world for urban state-domain work. Lose traditional ritual coordination (lunar festivals become unpaid leave they cannot take); gain state-domain vulnerability (labor law, housing permits, ID systems) without traditional safety nets. The partition extracts from them twice: state domain demands full compliance, traditional domain offers no portable protection.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, migrant_workers, payer,
    powerless, immediate, trapped, national).

% Their ritual calendars and dress norms are neither the state standard nor the recognized traditional authority's standard. The partition recognizes only one traditional authority; their practices are invisible to both domains. They comply strategically with state domain, maintain hidden ritual domain, and bear suppression from both sides for non-conformity.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, minority_communities, excluded,
    moderate, generational, identity_locked, regional).

% Observes the equilibrium as a stable institutional configuration rather than a transitional phase. Notes that strategic compliance — performing the right practice in the right domain without internalizing either as universally legitimate — is the dominant mode. The partition persists because neither authority can fully displace the other, and the cost of unification exceeds the cost of maintaining the boundary.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, historian_analyst, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Resolves the legitimacy crisis of modernization by partitioning domains: state authority gains fiscal/administrative legibility without destroying the traditional authority's ritual/kinship coherence. Each domain solves its own coordination problem (tax collection vs. festival timing, contract enforcement vs. marriage alliance) using the authority that holds legitimacy there.
% TRANSFER_FUNCTION: Moves compliance costs onto rural households and migrant workers who must maintain dual fluency; moves legitimacy rents to state bureaucracy (administrative control) and traditional ritual authorities (ritual monopoly); moves transaction-cost savings to merchant class who arbitrage the boundary.
% ABSENT_VOICES: Minority communities whose ritual practices fall outside the recognized traditional authority; urban poor who lack both state-domain protections and traditional-domain safety nets; women whose domestic labor bridges both domains but whose voices are mediated by male household heads in traditional domain and individual contracts in state domain.
% DISAPPEARANCE_RATIONALE: If the partition vanished, either state authority would extend into ritual domain (triggering resistance from traditional authorities and minority communities) or traditional authority would expand into administrative domain (undermining fiscal capacity and international legibility). The current equilibrium depends on the boundary; its removal forces a unification conflict.
% FOUNDING_PROBLEM: Late 19th/early 20th century modernization drive: state needed fiscal/administrative standardization (Gregorian calendar, metric system, civil law) to build capacity, but direct imposition on ritual/kinship domains provoked rebellions (e.g., calendar riots, dress code resistance). The partition emerged as the settlement that let state standardize administration while conceding ritual domain to traditional authorities.
% FOUNDING_PROBLEM_CORROBORATION: State archives document the fiscal/administrative necessity (tax collection, conscription rolls, international treaties). Traditional authority records (temple registers, clan chronicles) document concession of administrative domain in exchange for ritual autonomy. Rural household diaries and migrant letters (collected in oral history projects) attest the lived cost of dual compliance — a source outside both benefiting authorities.
narrative_ontology:disappearance_verdict(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, world_rearranges).
narrative_ontology:founding_problem_status(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, 'none', 1).
narrative_ontology:epsilon_provenance(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, 0.42, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading_tests).
:- end_tests(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.42) reflects real but bounded extraction: rural households and migrants pay dual-compliance costs, but the partition also reduces universal compliance burden. Suppression (0.28) is moderate — the state does not actively crush traditional domain, traditional authorities do not contest state domain, but minority practices are suppressed by both. Theater ratio (0.35) captures performative maintenance: state rituals (parades, holidays) and traditional ceremonies (now partly staged for tourism/state recognition) exceed functional need. Accessibility collapse (0.45) is partial — alternatives exist (pure state or pure traditional) but are costly. Resistance (0.38) is present but channeled: calendar riots early, later quiet non-compliance and migration.
 *
 * PERSPECTIVAL GAP:
 *   From the state bureaucracy seat, the constraint is a pragmatic settlement enabling capacity building. From traditional authorities, it is a defensive preservation of cosmological order. From rural households, it is a double bind. From minority communities, it is erasure. The engine computes these divergent per-seat types from the structural data — the claimed tangled_rope reflects the coordination/extraction hybrid at the system level.
 *
 * DIRECTIONALITY LOGIC:
 *   State bureaucracy and traditional ritual authorities are agenda_setters (d near 0.0-0.2) — they set and enforce their respective domains. Merchant class is beneficiary (d ~0.3) — gains from both domains with mobile exit. Rural households and migrant workers are payers (d ~0.7-0.9) — bear costs with constrained/trapped exit. Minority communities are excluded (d ~0.8) — invisible to both domains, identity-locked. Historian analyst is observer (d=0.5). The partition creates asymmetric extraction: state extracts administrative compliance, traditional authorities extract ritual conformity, but neither extracts from the other's domain.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (modernization without rebellion) was live in 1870-1910. By 1930-1950, state capacity is established, traditional authority is weakened but ritually entrenched. The arrangement persists not because the founding problem remains acute, but because the partition now benefits both agenda_setters (state keeps administrative control; traditional authorities keep ritual monopoly) and the merchant class (boundary arbitrage). Mandatrophy is contested: the arrangement has outlived its original justification but acquired new beneficiaries.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    partition_stability_vs_contingent_truce,
    'Is the domain partition a stable equilibrium or a contingent truce that will collapse when state capacity or traditional authority crosses a threshold?',
    'Longitudinal analysis of boundary disputes: if state encroachment on ritual domain (e.g., regulating festival dates, licensing ritual specialists) increases over decades, the partition is a truce. If boundary disputes remain constant in frequency and intensity, it is an equilibrium.',
    'If truce, the constraint is a scaffold with an undeclared sunset. If equilibrium, tangled_rope is structurally correct. Reclassification affects whether the theater_ratio trend (rising) signals drift toward piton or stable maintenance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(partition_stability_vs_contingent_truce, empirical, 'Whether the dual-practice structure is a permanent settlement or a temporary pause in unification.').

omega_variable(
    minority_erasure_as_structural_feature,
    'Is the exclusion of minority ritual practices from both domains a bug (incidental omission) or a feature (the partition requires a single recognized traditional authority)?',
    'Comparative analysis: in polities with multiple recognized traditional authorities (e.g., millet systems), does the partition structure replicate per-authority, or does it collapse? If it replicates, minority exclusion is a bug. If it collapses, the partition structurally requires a monopoly traditional authority.',
    'If feature, the constraint is more extractive toward minorities than the base metrics capture — suppression is systematically targeted. If bug, the constraint could be reformed without destabilizing the partition.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(minority_erasure_as_structural_feature, conceptual, 'Whether minority exclusion is structurally necessary to the dual-authority partition.').

omega_variable(
    strategic_compliance_as_extraction_mechanism,
    'Does strategic compliance (performing the right practice in each domain without internalization) itself function as an extraction mechanism — forcing actors to invest in costly signaling of dual loyalty?',
    'Measure the signaling cost: time/money spent on domain-appropriate dress, calendar fluency, ritual participation, bureaucratic paperwork. Compare to the cost of a unified standard. If dual signaling cost exceeds unified compliance cost, strategic compliance is extractive overhead.',
    'If extractive, the theater_ratio understates true extraction — the performance IS the extraction. This would push the constraint toward snare classification for payer seats.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(strategic_compliance_as_extraction_mechanism, empirical, 'Whether the cognitive/performative burden of strategic compliance constitutes hidden extraction.').

omega_variable(
    kernel_reading_foreclosure_boundary,
    'Does the dual_practice_equilibrium_reading logically foreclose the endogenous_displacement_reading, or do they coexist as descriptions of different phases/social strata?',
    'Test whether a single actor can simultaneously hold: (a) the partition is a stable equilibrium, and (b) practice change is legitimate when emerging from voluntary adoption. If yes, they coexist. If (a) requires that NO voluntary adoption can cross the domain boundary, then equilibrium forecloses endogenous displacement at the boundary.',
    'If forecloses, the kernel has a genuine structural contradiction between readings. If coexists_with, the readings describe different layers (system-level equilibrium vs. micro-level change mechanism).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_foreclosure_boundary, conceptual, 'Logical relationship between the equilibrium reading and the endogenous displacement reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, 1870, 1950).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dual_practice_equilibrium_tr_t1870, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, theater_ratio, 1870, 0.15).
narrative_ontology:measurement(dual_practice_equilibrium_tr_t1890, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, theater_ratio, 1890, 0.22).
narrative_ontology:measurement(dual_practice_equilibrium_tr_t1910, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, theater_ratio, 1910, 0.28).
narrative_ontology:measurement(dual_practice_equilibrium_tr_t1930, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, theater_ratio, 1930, 0.32).
narrative_ontology:measurement(dual_practice_equilibrium_tr_t1940, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, theater_ratio, 1940, 0.34).
narrative_ontology:measurement(dual_practice_equilibrium_tr_t1950, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, theater_ratio, 1950, 0.35).

% Extraction over time
narrative_ontology:measurement(dual_practice_equilibrium_be_t1870, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, base_extractiveness, 1870, 0.25).
narrative_ontology:measurement(dual_practice_equilibrium_be_t1890, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, base_extractiveness, 1890, 0.32).
narrative_ontology:measurement(dual_practice_equilibrium_be_t1910, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, base_extractiveness, 1910, 0.38).
narrative_ontology:measurement(dual_practice_equilibrium_be_t1930, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, base_extractiveness, 1930, 0.4).
narrative_ontology:measurement(dual_practice_equilibrium_be_t1940, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, base_extractiveness, 1940, 0.42).
narrative_ontology:measurement(dual_practice_equilibrium_be_t1950, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, base_extractiveness, 1950, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(dual_practice_equilibrium_su_t1870, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, suppression_requirement, 1870, 0.15).
narrative_ontology:measurement(dual_practice_equilibrium_su_t1890, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, suppression_requirement, 1890, 0.2).
narrative_ontology:measurement(dual_practice_equilibrium_su_t1910, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, suppression_requirement, 1910, 0.25).
narrative_ontology:measurement(dual_practice_equilibrium_su_t1930, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, suppression_requirement, 1930, 0.27).
narrative_ontology:measurement(dual_practice_equilibrium_su_t1940, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, suppression_requirement, 1940, 0.28).
narrative_ontology:measurement(dual_practice_equilibrium_su_t1950, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, suppression_requirement, 1950, 0.28).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, 0.1).
narrative_ontology:affects_constraint(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, legitimacy_of_practice_standardization__endogenous_displacement_reading).
narrative_ontology:affects_constraint(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, legitimacy_of_practice_standardization__exogenous_override_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the legitimacy_of_practice_standardization kernel. The kernel's contested claim — what makes practice change legitimate — decomposes into: (1) dual_practice_equilibrium: legitimacy is domain-partitioned between state and traditional authority; (2) endogenous_displacement: legitimacy from voluntary adoption; (3) exogenous_override: legitimacy from state decree. Each reading has distinct beneficiary/victim structures and ε values. This reading has ε=0.42 (moderate extraction, bounded by partition); endogenous_displacement likely has lower ε (voluntary adoption minimizes coercion); exogenous_override likely has higher ε (state decree overrides existing practices). The readings are linked via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, organized, 0.15).
constraint_indexing:directionality_override(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, moderate, 0.75).
constraint_indexing:directionality_override(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, powerless, 0.9).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
