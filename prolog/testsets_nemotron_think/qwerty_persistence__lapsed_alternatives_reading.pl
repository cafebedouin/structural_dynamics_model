% ============================================================================
% CONSTRAINT STORY: qwerty_persistence__lapsed_alternatives_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_qwerty_persistence__lapsed_alternatives_reading, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
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
 *   constraint_id: qwerty_persistence__lapsed_alternatives_reading
 *   human_readable: QWERTY Persistence via Coordination Value (Lapsed Alternatives Reading)
 *   domain: technology_history/industrial_standards/path_dependence
 *
 * SUMMARY:
 *   This reading of QWERTY persistence treats the standard as a rope
 *   constraint: a genuine coordination solution where the layout's value
 *   comes entirely from its universality. Alternatives (Dvorak, Colemak) fail
 *   not because incumbents actively suppress them, but because they cannot
 *   overcome the critical mass threshold — the coordination value of the
 *   incumbent is too high for any challenger to displace without massive
 *   coordinated switching. No party extracts rents from QWERTY itself;
 *   manufacturers, typists, and software developers all bear symmetric costs
 *   if the standard changed. The epsilon (0.12) reflects switching costs
 *   alone, not extraction.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(qwerty_persistence__lapsed_alternatives_reading, 0.12).
domain_priors:suppression_score(qwerty_persistence__lapsed_alternatives_reading, 0.08).
domain_priors:theater_ratio(qwerty_persistence__lapsed_alternatives_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(qwerty_persistence__lapsed_alternatives_reading, extractiveness, 0.12).
narrative_ontology:constraint_metric(qwerty_persistence__lapsed_alternatives_reading, suppression_requirement, 0.08).
narrative_ontology:constraint_metric(qwerty_persistence__lapsed_alternatives_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(qwerty_persistence__lapsed_alternatives_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(qwerty_persistence__lapsed_alternatives_reading, resistance, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(qwerty_persistence__lapsed_alternatives_reading, rope).
narrative_ontology:human_readable(qwerty_persistence__lapsed_alternatives_reading, "QWERTY Persistence via Coordination Value (Lapsed Alternatives Reading)").
narrative_ontology:topic_domain(qwerty_persistence__lapsed_alternatives_reading, "technology_history/industrial_standards/path_dependence").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(qwerty_persistence__lapsed_alternatives_reading, 'ea15fea9-edce-4958-8765-01c38bb05273').
narrative_ontology:cs_kernel_codification('ea15fea9-edce-4958-8765-01c38bb05273', distributed).
narrative_ontology:cs_authority_grounding('ea15fea9-edce-4958-8765-01c38bb05273', practice).
narrative_ontology:cs_reading_relation('ea15fea9-edce-4958-8765-01c38bb05273', qwerty_persistence__incumbent_preservation_reading, coexists_with).
narrative_ontology:cs_axiom('ea15fea9-edce-4958-8765-01c38bb05273', foundational, coordination_value_sufficiency).
narrative_ontology:cs_axiom_status(coordination_value_sufficiency, holdable).
narrative_ontology:cs_axiom_grounding('ea15fea9-edce-4958-8765-01c38bb05273', coordination_value_sufficiency, empirically_contingent).
narrative_ontology:cs_axiom('ea15fea9-edce-4958-8765-01c38bb05273', secondary, critical_mass_as_coordination_barrier).
narrative_ontology:cs_axiom_status(critical_mass_as_coordination_barrier, holdable).
narrative_ontology:cs_axiom_grounding('ea15fea9-edce-4958-8765-01c38bb05273', critical_mass_as_coordination_barrier, empirically_contingent).
narrative_ontology:cs_reference_frame('ea15fea9-edce-4958-8765-01c38bb05273', path_dependence_coordination_frame).
narrative_ontology:cs_drift_state('ea15fea9-edce-4958-8765-01c38bb05273', contemporary_historical_consensus, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('ea15fea9-edce-4958-8765-01c38bb05273', '').
narrative_ontology:cs_kernel_id(qwerty_persistence__lapsed_alternatives_reading, qwerty_persistence).

% --- Structural relationships ---
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(qwerty_persistence__lapsed_alternatives_reading, touch_typists).
narrative_ontology:constraint_beneficiary(qwerty_persistence__lapsed_alternatives_reading, keyboard_manufacturers).
narrative_ontology:constraint_beneficiary(qwerty_persistence__lapsed_alternatives_reading, software_input_developers).
narrative_ontology:constraint_victim(qwerty_persistence__lapsed_alternatives_reading, keyboard_manufacturers).
narrative_ontology:constraint_vindicates(qwerty_persistence__lapsed_alternatives_reading, path_dependence_in_standard_adoption).
narrative_ontology:constraint_vindicates(qwerty_persistence__lapsed_alternatives_reading, critical_mass_coordination_threshold).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Invested in QWERTY motor skills; benefit from universal layout compatibility across devices. Switching costs are high but symmetric — they bear the cost of learning any new layout, not extraction by a beneficiary.
narrative_ontology:constraint_stakeholder(qwerty_persistence__lapsed_alternatives_reading, touch_typists, beneficiary,
    organized, biographical, constrained, global).

% Standardize production around QWERTY; benefit from economies of scale but also bear retooling costs if layout changes. No manufacturer extracts rent from the layout itself — all face identical coordination incentives.
narrative_ontology:constraint_stakeholder(qwerty_persistence__lapsed_alternatives_reading, keyboard_manufacturers, beneficiary,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(qwerty_persistence__lapsed_alternatives_reading, keyboard_manufacturers, payer).

% Promote Dvorak, Colemak, and other layouts claiming efficiency gains. Their alternatives fail to reach critical mass because network effects favor the incumbent, not because they are actively suppressed. They can and do use alternative layouts individually.
narrative_ontology:constraint_stakeholder(qwerty_persistence__lapsed_alternatives_reading, alternative_layout_advocates, excluded,
    moderate, biographical, mobile, global).

% Build input methods, autocomplete, and text prediction around QWERTY's statistical properties. Benefit from stable target; would bear adaptation costs for any layout shift but extract no rent from QWERTY itself.
narrative_ontology:constraint_stakeholder(qwerty_persistence__lapsed_alternatives_reading, software_input_developers, beneficiary,
    organized, biographical, mobile, global).

% Study QWERTY as a canonical case of path dependence and coordination-driven lock-in. Their analysis shapes the discourse but they neither collect from nor pay into the constraint.
narrative_ontology:constraint_stakeholder(qwerty_persistence__lapsed_alternatives_reading, historical_economists, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a universal keyboard layout enabling anyone to use any keyboard without relearning — a pure coordination solution where the value is the shared convention itself.
% TRANSFER_FUNCTION: No net transfer; all parties bear symmetric switching costs if the standard changes. The arrangement moves coordination value from 'potential' to 'realized' without extracting from one group to another.
% ABSENT_VOICES: Users who never learned touch-typing (hunt-and-peck typists) have low switching costs but are rarely consulted; their preferences would not shift the equilibrium because they don't drive the installed base.
% DISAPPEARANCE_RATIONALE: If QWERTY vanished overnight, a chaotic period of competing layouts would follow until a new standard emerged — the world rearranges because the coordination function is real and necessary, not because a beneficiary would lose rents.
% FOUNDING_PROBLEM: Late 19th century typewriter manufacturers needed a layout that minimized mechanical key jams while enabling interoperability across brands and typing pools.
% FOUNDING_PROBLEM_CORROBORATION: The mechanical anti-jamming rationale is acknowledged by both path dependence proponents (David 1985) and critics (Liebowitz & Margolis 1990) — the founding problem is historically documented and no longer operative.
narrative_ontology:disappearance_verdict(qwerty_persistence__lapsed_alternatives_reading, world_rearranges).
narrative_ontology:founding_problem_status(qwerty_persistence__lapsed_alternatives_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(qwerty_persistence__lapsed_alternatives_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(qwerty_persistence__lapsed_alternatives_reading, 'none', 1).
narrative_ontology:epsilon_provenance(qwerty_persistence__lapsed_alternatives_reading, 0.12, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(qwerty_persistence__lapsed_alternatives_reading_tests).
:- end_tests(qwerty_persistence__lapsed_alternatives_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Low extractiveness (0.12) because the constraint's persistence is explained by coordination value, not rent extraction. Low suppression (0.08) because alternatives are legally and technically available — anyone can use Dvorak today — they simply lack network adoption. Near-zero theater (0.05) because no performative maintenance is needed; the standard persists through use itself. Moderate accessibility collapse (0.35) because once you learn QWERTY, alternatives feel costly, but the collapse is incomplete (many do switch individually). Low resistance (0.15) because no enforcement mechanism exists to resist.
 *
 * PERSPECTIVAL GAP:
 *   The incumbent_preservation_reading sees the same structure and identifies active beneficiary defense (Remington, IBM, Microsoft) — this reading sees only coordination inertia. The engine computes different seat types from the same structural data depending on which reading's beneficiary/victim declarations are input. This divergence IS the kernel contest.
 *
 * DIRECTIONALITY LOGIC:
 *   All stakeholders sit near symmetric directionality (d ≈ 0.5). Touch typists, manufacturers, and software developers all benefit from the coordination AND bear the costs of any transition. Alternative layout advocates are excluded from the coordination equilibrium but not extracted from — they can use their preferred layouts individually. The engine will derive near-symmetric d values from the empty beneficiary/victim arrays and constrained-but-not-trapped exit options.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (mechanical key jams) is dead, but the constraint persists because its coordination function remains live — this is not mandatrophy (persistence after function loss) but function transformation (mechanical coordination → digital coordination). The rope classification correctly captures this: the constraint still solves a real coordination problem.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    critical_mass_threshold_empirical,
    'What is the actual critical mass threshold for keyboard layout displacement — is it a stable structural property or historically contingent?',
    'Large-scale natural experiments (e.g., mobile keyboard layouts, non-Latin script input methods) where new standards achieved adoption without incumbent resistance.',
    'If threshold is low and historically contingent, the rope reading is strengthened; if threshold is structurally high and stable, the constraint approaches mountain-like immovability despite being conventional.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(critical_mass_threshold_empirical, empirical, 'Whether the coordination barrier is a stable structural feature or a contingent historical outcome.').

omega_variable(
    coordination_vs_extraction_boundary,
    'At what point do switching costs become extraction rather than coordination cost?',
    'Counterfactual analysis: if a layout with demonstrably superior efficiency (e.g., 20% speed gain) failed to displace QWERTY, would the persistence be reclassified as extractive?',
    'If superior alternatives failing implies extraction, then the rope/snare boundary depends on empirical efficiency comparisons — making epsilon reading-dependent in a way that challenges ε-invariance.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_vs_extraction_boundary, conceptual, 'Whether the coordination/extraction distinction collapses when efficiency gains are foregone.').

omega_variable(
    kernel_reading_disagreement_location,
    'Where exactly do the two readings of qwerty_persistence structurally disagree?',
    'Map each reading''s causal chain: this reading = coordination_value → critical_mass_failure → persistence; sibling reading = incumbent_capital → active_defense → persistence. The disagreement is on the middle link.',
    'Confirms the readings differ on the transfer_function (active defense vs. passive failure), not on the coordination_function (both agree QWERTY coordinates). This structural delta justifies separate constraint stories.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_disagreement_location, conceptual, 'The precise structural element where the sibling readings diverge.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(qwerty_persistence__lapsed_alternatives_reading, 1873, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qwer_tr_t1873, qwerty_persistence__lapsed_alternatives_reading, theater_ratio, 1873, 0.02).
narrative_ontology:measurement(qwer_tr_t1900, qwerty_persistence__lapsed_alternatives_reading, theater_ratio, 1900, 0.03).
narrative_ontology:measurement(qwer_tr_t1930, qwerty_persistence__lapsed_alternatives_reading, theater_ratio, 1930, 0.04).
narrative_ontology:measurement(qwer_tr_t1960, qwerty_persistence__lapsed_alternatives_reading, theater_ratio, 1960, 0.04).
narrative_ontology:measurement(qwer_tr_t1990, qwerty_persistence__lapsed_alternatives_reading, theater_ratio, 1990, 0.05).
narrative_ontology:measurement(qwer_tr_t2025, qwerty_persistence__lapsed_alternatives_reading, theater_ratio, 2025, 0.05).

% Extraction over time
narrative_ontology:measurement(qwer_be_t1873, qwerty_persistence__lapsed_alternatives_reading, base_extractiveness, 1873, 0.05).
narrative_ontology:measurement(qwer_be_t1900, qwerty_persistence__lapsed_alternatives_reading, base_extractiveness, 1900, 0.08).
narrative_ontology:measurement(qwer_be_t1930, qwerty_persistence__lapsed_alternatives_reading, base_extractiveness, 1930, 0.1).
narrative_ontology:measurement(qwer_be_t1960, qwerty_persistence__lapsed_alternatives_reading, base_extractiveness, 1960, 0.11).
narrative_ontology:measurement(qwer_be_t1990, qwerty_persistence__lapsed_alternatives_reading, base_extractiveness, 1990, 0.12).
narrative_ontology:measurement(qwer_be_t2025, qwerty_persistence__lapsed_alternatives_reading, base_extractiveness, 2025, 0.12).

% Suppression requirement over time
narrative_ontology:measurement(qwer_su_t1873, qwerty_persistence__lapsed_alternatives_reading, suppression_requirement, 1873, 0.05).
narrative_ontology:measurement(qwer_su_t1900, qwerty_persistence__lapsed_alternatives_reading, suppression_requirement, 1900, 0.06).
narrative_ontology:measurement(qwer_su_t1930, qwerty_persistence__lapsed_alternatives_reading, suppression_requirement, 1930, 0.07).
narrative_ontology:measurement(qwer_su_t1960, qwerty_persistence__lapsed_alternatives_reading, suppression_requirement, 1960, 0.08).
narrative_ontology:measurement(qwer_su_t1990, qwerty_persistence__lapsed_alternatives_reading, suppression_requirement, 1990, 0.08).
narrative_ontology:measurement(qwer_su_t2025, qwerty_persistence__lapsed_alternatives_reading, suppression_requirement, 2025, 0.08).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(qwerty_persistence__lapsed_alternatives_reading, information_standard).
narrative_ontology:affects_constraint(qwerty_persistence__lapsed_alternatives_reading, qwerty_persistence__incumbent_preservation_reading).

% DUAL FORMULATION NOTE:
% This constraint and incumbent_preservation_reading form a constraint family decomposing the colloquial 'QWERTY persistence' claim. This reading (rope, ε≈0.12, no beneficiaries) and the sibling (tangled_rope/snare, ε≈0.4+, manufacturer beneficiaries) have fundamentally different ε values because they model different causal structures. They are linked via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
