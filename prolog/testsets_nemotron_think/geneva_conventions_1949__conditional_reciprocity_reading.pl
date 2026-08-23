% ============================================================================
% CONSTRAINT STORY: geneva_conventions_1949__conditional_reciprocity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_geneva_conventions_1949__conditional_reciprocity_reading, []).

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
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: geneva_conventions_1949__conditional_reciprocity_reading
 *   human_readable: Geneva Conventions 1949: Conditional Reciprocity Reading
 *   domain: international_humanitarian_law
 *
 * SUMMARY:
 *   This story captures the conditional reciprocity reading of the 1949
 *   Geneva Conventions: the Conventions function as reciprocal restraints
 *   that apply fully only when adversaries comply; non-compliance by
 *   irregular forces permits proportional degradation of protections. The
 *   reading is advanced by state militaries to justify denying POW status to
 *   irregulars, narrowing civilian immunity via proportionality, and
 *   classifying detainees as unlawful combatants. It coexists with the
 *   humanitarian_ceiling_reading (absolute minimums regardless of
 *   reciprocity) and the security_maximization_reading (Conventions yield to
 *   operational necessity). The ε-invariance principle requires separate
 *   stories for each reading because their extractiveness profiles differ:
 *   this reading's ε is moderate (0.55) because it preserves some
 *   coordination for regular forces while extracting from irregulars and
 *   civilians; the humanitarian ceiling reading would have near-zero ε; the
 *   security maximization reading would have high ε.
 *
 * KEY AGENTS:
 *   - state_militaries: Primary agenda setter (institutional/arbitrage) — authors the reading, controls classification
 *   - regular_armed_forces: Primary beneficiary (organized/constrained) — receives full protections conditional on reciprocity
 *   - irregular_combatants: Primary victim (powerless/trapped) — denied POW status, exposed to prosecution
 *   - civilian_populations: Victim (powerless/trapped) — immunity narrowed by proportionality in asymmetric zones
 *   - detained_irregulars: Victim (powerless/trapped) — held without full Convention protections
 *   - humanitarian_organizations: Observer (organized/analytical) — documents degradation, advocates ceiling reading
 *   - international_courts: Observer (institutional/analytical) — jurisprudence gradually rejects blanket unlawful-combatant categories
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(geneva_conventions_1949__conditional_reciprocity_reading, 0.55).
domain_priors:suppression_score(geneva_conventions_1949__conditional_reciprocity_reading, 0.6).
domain_priors:theater_ratio(geneva_conventions_1949__conditional_reciprocity_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(geneva_conventions_1949__conditional_reciprocity_reading, extractiveness, 0.55).
narrative_ontology:constraint_metric(geneva_conventions_1949__conditional_reciprocity_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(geneva_conventions_1949__conditional_reciprocity_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(geneva_conventions_1949__conditional_reciprocity_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(geneva_conventions_1949__conditional_reciprocity_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(geneva_conventions_1949__conditional_reciprocity_reading, tangled_rope).
narrative_ontology:human_readable(geneva_conventions_1949__conditional_reciprocity_reading, "Geneva Conventions 1949: Conditional Reciprocity Reading").
narrative_ontology:topic_domain(geneva_conventions_1949__conditional_reciprocity_reading, "international_humanitarian_law").

domain_priors:requires_active_enforcement(geneva_conventions_1949__conditional_reciprocity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(geneva_conventions_1949__conditional_reciprocity_reading, 'a7c48c68-12bc-47bd-a887-68396c717c28').
narrative_ontology:cs_kernel_codification('a7c48c68-12bc-47bd-a887-68396c717c28', formalized).
narrative_ontology:cs_authority_grounding('a7c48c68-12bc-47bd-a887-68396c717c28', lineage).
narrative_ontology:cs_interpretation_layer_present('a7c48c68-12bc-47bd-a887-68396c717c28').
narrative_ontology:cs_reading_relation('a7c48c68-12bc-47bd-a887-68396c717c28', geneva_conventions_1949__humanitarian_ceiling_reading, coexists_with).
narrative_ontology:cs_reading_relation('a7c48c68-12bc-47bd-a887-68396c717c28', geneva_conventions_1949__security_maximization_reading, coexists_with).
narrative_ontology:cs_axiom('a7c48c68-12bc-47bd-a887-68396c717c28', foundational, reciprocity_conditionality).
narrative_ontology:cs_axiom_status(reciprocity_conditionality, holdable).
narrative_ontology:cs_axiom_grounding('a7c48c68-12bc-47bd-a887-68396c717c28', reciprocity_conditionality, conventional).
narrative_ontology:cs_axiom('a7c48c68-12bc-47bd-a887-68396c717c28', foundational, proportionality_degradation).
narrative_ontology:cs_axiom_status(proportionality_degradation, holdable).
narrative_ontology:cs_axiom_grounding('a7c48c68-12bc-47bd-a887-68396c717c28', proportionality_degradation, instrumental).
narrative_ontology:cs_reference_frame('a7c48c68-12bc-47bd-a887-68396c717c28', reciprocal_treaty_regime).
narrative_ontology:cs_drift_state('a7c48c68-12bc-47bd-a887-68396c717c28', contemporary_asymmetric_warfare_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('a7c48c68-12bc-47bd-a887-68396c717c28', '').
narrative_ontology:cs_kernel_id(geneva_conventions_1949__conditional_reciprocity_reading, geneva_conventions_1949).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(geneva_conventions_1949__conditional_reciprocity_reading, state_militaries).
narrative_ontology:constraint_beneficiary(geneva_conventions_1949__conditional_reciprocity_reading, regular_armed_forces).
narrative_ontology:constraint_victim(geneva_conventions_1949__conditional_reciprocity_reading, irregular_combatants).
narrative_ontology:constraint_victim(geneva_conventions_1949__conditional_reciprocity_reading, civilian_populations).
narrative_ontology:constraint_victim(geneva_conventions_1949__conditional_reciprocity_reading, detained_irregulars).
narrative_ontology:constraint_vindicates(geneva_conventions_1949__conditional_reciprocity_reading, reciprocity_in_treaty_law).
narrative_ontology:constraint_vindicates(geneva_conventions_1949__conditional_reciprocity_reading, distinction_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Draft, ratify, and interpret the Conventions; decide when adversary non-compliance triggers proportional degradation; control classification of detainees and targeting decisions. Benefit from operational flexibility while retaining the Convention's legitimacy shield.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__conditional_reciprocity_reading, state_militaries, agenda_setter,
    institutional, generational, arbitrage, global).

% Receive full POW protections and combatant immunity when they meet Article 4 criteria (command, insignia, open carry). Their protections are contingent on reciprocal adversary compliance, creating pressure to maintain status distinction.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__conditional_reciprocity_reading, regular_armed_forces, beneficiary,
    organized, biographical, constrained, global).

% Fighters who do not meet Article 4 criteria (no fixed insignia, no open carry, no responsible command). Classified as unlawful combatants; denied POW status, subject to domestic prosecution, and exposed to degraded protections. Cannot exit the classification without adopting regular-force structures.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__conditional_reciprocity_reading, irregular_combatants, payer,
    powerless, immediate, trapped, local).

% Civilian immunity is preserved in principle but narrowed by proportionality calculations that weigh military advantage against civilian harm. In asymmetric conflicts where irregulars operate among civilians, the reading permits higher collateral damage thresholds.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__conditional_reciprocity_reading, civilian_populations, payer,
    powerless, immediate, trapped, local).

% Held without full POW protections (no guaranteed repatriation, limited judicial guarantees, no protecting power access). Their treatment falls under domestic security law rather than the Third Convention. Exit requires status review that the detaining power controls.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__conditional_reciprocity_reading, detained_irregulars, payer,
    powerless, biographical, trapped, local).

% Monitor compliance, advocate for humanitarian ceiling reading, provide services to victims. Their access depends on state consent; they document degradation but cannot enforce the Conventions.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__conditional_reciprocity_reading, humanitarian_organizations, observer,
    organized, generational, analytical, global).

% Adjudicate war crimes and interpret Convention obligations. Their jurisprudence has gradually rejected blanket unlawful-combatant categories, creating tension with the conditional reciprocity reading. Enforcement depends on state cooperation.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__conditional_reciprocity_reading, international_courts, observer,
    institutional, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a reciprocal framework for limiting violence between regular state armies: mutual restraint on targeting, detention, and treatment in exchange for mutual compliance. Solves the coordination problem of 'who shoots first' by making protections conditional on adversary behavior.
% TRANSFER_FUNCTION: Transfers protection from irregular combatants and civilian populations in asymmetric zones to state militaries (operational flexibility, reduced legal risk) and regular armed forces (preserved POW status). The transfer is mediated by classification decisions and proportionality calculations controlled by state militaries.
% ABSENT_VOICES: Irregular combatants and affected civilian populations are structurally excluded from the interpretation process; they have no standing in treaty bodies, no vote in diplomatic conferences, and no access to the classification mechanisms that determine their protections. Their objections appear only in NGO reports and dissenting judicial opinions.
% DISAPPEARANCE_RATIONALE: If the conditional reciprocity reading vanished, states would lose the legal basis for degrading protections based on adversary status. Detention regimes for irregulars would require new legal frameworks; targeting rules would revert to the humanitarian ceiling standard; the reciprocal bargain between regular armies would need renegotiation.
% FOUNDING_PROBLEM: The 1949 Conventions were built to regulate warfare between regular state armies after WWII, ensuring mutual restraint through reciprocal POW protections and civilian immunity. The founding problem was inter-state war, not asymmetric irregular warfare.
% FOUNDING_PROBLEM_CORROBORATION: State military doctrines and diplomatic records from 1949 corroborate the inter-state focus. The ICRC's official commentary and subsequent Additional Protocol I negotiations (1977) attest that the founding problem has shifted: asymmetric conflict now dominates, but parties dispute whether the Conventions' core obligations survive that shift.
narrative_ontology:disappearance_verdict(geneva_conventions_1949__conditional_reciprocity_reading, world_rearranges).
narrative_ontology:founding_problem_status(geneva_conventions_1949__conditional_reciprocity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(geneva_conventions_1949__conditional_reciprocity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(geneva_conventions_1949__conditional_reciprocity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(geneva_conventions_1949__conditional_reciprocity_reading, 0.55, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(geneva_conventions_1949__conditional_reciprocity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(geneva_conventions_1949__conditional_reciprocity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(geneva_conventions_1949__conditional_reciprocity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (0.55) reflects the reading's dual character: it coordinates restraint between regular armies (low extraction for them) while extracting protections from irregulars and civilians in asymmetric conflicts (high extraction for them). Suppression (0.6) is structural: the classification machinery (Article 4 criteria, proportionality assessments, military commissions) actively enforces the degradation. Theater ratio (0.3) captures performative compliance — states invoke the Conventions' legitimacy while applying the conditional reading. Accessibility collapse (0.4) is moderate because alternative readings (humanitarian ceiling) remain live in courts and NGOs. Resistance (0.5) comes from judicial pushback and NGO advocacy. The claim (tangled_rope) and metrics are independent: the reading claims to be a coordination mechanism (rope), but the metrics reveal asymmetric extraction (tangled_rope).
 *
 * PERSPECTIVAL GAP:
 *   From the state_militaries seat, the arrangement is a rope: reciprocal coordination that prevents unlimited war. From the irregular_combatants and civilian_populations seats, it is a snare: protections are withheld based on criteria they cannot meet. From the regular_armed_forces seat, it is a tangled rope: they gain POW status but only if adversaries comply, creating fragility. The engine computes per-seat types from the structural data; this gap is the measurement.
 *
 * DIRECTIONALITY LOGIC:
 *   State militaries are structural beneficiaries (d near 0.0): they control the reading, gain flexibility, and collect legitimacy. Regular armed forces are symmetric (d ~0.5): they benefit from POW protections but bear the risk of reciprocity failure. Irregular combatants, civilian populations, and detained irregulars are full targets (d near 1.0): they bear the degradation with trapped exit options. Humanitarian organizations and courts are observers (d=0.5 analytically) — they neither collect nor pay but see the full structure.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (regulating inter-state war) is contested: some say it's dead (asymmetric conflict dominates), others say it's live (major-power conflict returns). The reading persists because it serves state militaries' current operational needs — not because it solves the founding problem. This is mandatrophy: the Convention's mandate has outlived its original function, but the reading extracts value from the Convention's continued legitimacy.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reciprocity_threshold_ambiguity,
    'What level of adversary non-compliance triggers proportional degradation? Is a single irregular unit''s non-compliance sufficient, or must the adversary''s entire force structure fail Article 4?',
    'State practice and judicial decisions on ''organized armed groups'' and ''responsible command'' — e.g., ICTY Tadic, ICJ Nicaragua, Hamdan v. Rumsfeld.',
    'A low threshold makes the reading a snare for most non-state actors; a high threshold preserves more coordination. The classification shifts if the threshold is shown to be manipulable by the adversary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reciprocity_threshold_ambiguity, empirical, 'Whether the reciprocity trigger is a genuine coordination signal or a manipulable pretext.').

omega_variable(
    proportionality_calculation_opacity,
    'How are proportionality calculations actually performed in asymmetric conflicts? Are they genuine good-faith assessments or post-hoc justifications for civilian harm?',
    'Military targeting doctrines, after-action reviews, and court findings on specific strikes (e.g., ICC Afghanistan investigation, national inquiries).',
    'If calculations are performative, the reading''s coordination claim collapses and extraction rises toward snare. If genuine, the reading retains a tangled_rope character with real coordination overhead.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(proportionality_calculation_opacity, conceptual, 'Whether the proportionality mechanism is a real restraint or a ritualized cover.').

omega_variable(
    committer_frame_underdetermination,
    'Does the conditional reciprocity reading foreclose the humanitarian ceiling reading within a single state''s legal framework, or do states maintain both as alternative interpretive options for different conflicts?',
    'Survey of state military manuals and legal advisories: do they treat the readings as mutually exclusive doctrines or as context-dependent tools?',
    'If states foreclose the ceiling reading internally, the relation is ''forecloses''; if they keep both live for different scenarios, it is ''coexists_with''. This affects the engine''s cross-reading coupling analysis.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(committer_frame_underdetermination, conceptual, 'Structural relationship between this reading and the humanitarian_ceiling_reading within state practice.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(geneva_conventions_1949__conditional_reciprocity_reading, 0, 76).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gc1949_cond_recip_tr_t0, geneva_conventions_1949__conditional_reciprocity_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(gc1949_cond_recip_tr_t38, geneva_conventions_1949__conditional_reciprocity_reading, theater_ratio, 38, 0.2).
narrative_ontology:measurement(gc1949_cond_recip_tr_t76, geneva_conventions_1949__conditional_reciprocity_reading, theater_ratio, 76, 0.3).

% Extraction over time
narrative_ontology:measurement(gc1949_cond_recip_be_t0, geneva_conventions_1949__conditional_reciprocity_reading, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(gc1949_cond_recip_be_t38, geneva_conventions_1949__conditional_reciprocity_reading, base_extractiveness, 38, 0.45).
narrative_ontology:measurement(gc1949_cond_recip_be_t76, geneva_conventions_1949__conditional_reciprocity_reading, base_extractiveness, 76, 0.55).

% Suppression requirement over time
narrative_ontology:measurement(gc1949_cond_recip_su_t0, geneva_conventions_1949__conditional_reciprocity_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(gc1949_cond_recip_su_t38, geneva_conventions_1949__conditional_reciprocity_reading, suppression_requirement, 38, 0.5).
narrative_ontology:measurement(gc1949_cond_recip_su_t76, geneva_conventions_1949__conditional_reciprocity_reading, suppression_requirement, 76, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(geneva_conventions_1949__conditional_reciprocity_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(geneva_conventions_1949__conditional_reciprocity_reading, 0.1).
narrative_ontology:affects_constraint(geneva_conventions_1949__conditional_reciprocity_reading, geneva_conventions_1949__humanitarian_ceiling_reading).
narrative_ontology:affects_constraint(geneva_conventions_1949__conditional_reciprocity_reading, geneva_conventions_1949__security_maximization_reading).

% DUAL FORMULATION NOTE:
% This reading, the humanitarian_ceiling_reading, and the security_maximization_reading form a constraint family decomposing the geneva_conventions_1949 kernel. They differ in ε: this reading (moderate, 0.55), humanitarian ceiling (near-zero), security maximization (high). The conditional reciprocity reading influences the security maximization reading by providing a 'middle ground' that legitimates partial degradation; it coexists with the humanitarian ceiling reading as a competing interpretation in courts and NGOs.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
