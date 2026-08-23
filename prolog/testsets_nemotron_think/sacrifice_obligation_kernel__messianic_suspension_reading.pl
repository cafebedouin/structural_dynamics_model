% ============================================================================
% CONSTRAINT STORY: sacrifice_obligation_kernel__messianic_suspension_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sacrifice_obligation_kernel__messianic_suspension_reading, []).

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
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   constraint_id: sacrifice_obligation_kernel__messianic_suspension_reading
 *   human_readable: Messianic Suspension of Sacrifice Obligation with Study Readiness
 *   domain: religious_law/halakhic_authority
 *
 * SUMMARY:
 *   Following the destruction of the Second Temple (70 CE), the biblical
 *   obligation to offer sacrifices became impossible to perform. The
 *   messianic_suspension_reading holds that this obligation was divinely
 *   suspended — not transformed, abolished, or fulfilled through substitute —
 *   until messianic restoration when the Temple will be rebuilt and
 *   sacrifices resume. During suspension, study of sacrifice laws (kodashim)
 *   maintains operational readiness: preserving the knowledge, procedures,
 *   and priestly lineages necessary for immediate restoration. The constraint
 *   is a scaffold: temporary support for a halted practice, with a declared
 *   sunset (messianic restoration). Extraction is near-zero during suspension
 *   — no one is compelled to perform impossible acts, study is voluntary
 *   intellectual engagement, and no victim set exists. Beneficiaries are
 *   future generations and the restoration priesthood who inherit intact
 *   operational knowledge.
 *
 * KEY AGENTS:
 *   - rabbinic_authorities: agenda_setter (institutional/biographical/arbitrage/global) — maintain study framework, interpret suspension doctrine
 *   - current_practitioners: beneficiary (organized/biographical/mobile/global) — obligation suspended, study meaningful but not burdensome
 *   - future_generations: beneficiary (analytical/generational/analytical/universal) — receive preserved operational knowledge
 *   - restoration_priesthood: beneficiary (organized/generational/constrained/global) — lineage preserved for future service
 *   - historical_priesthood: excluded (powerless/immediate/trapped/local) — would have performed but cannot; not in current conversation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sacrifice_obligation_kernel__messianic_suspension_reading, 0.08).
domain_priors:suppression_score(sacrifice_obligation_kernel__messianic_suspension_reading, 0.05).
domain_priors:theater_ratio(sacrifice_obligation_kernel__messianic_suspension_reading, 0.12).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__messianic_suspension_reading, extractiveness, 0.08).
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__messianic_suspension_reading, suppression_requirement, 0.05).
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__messianic_suspension_reading, theater_ratio, 0.12).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__messianic_suspension_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__messianic_suspension_reading, resistance, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sacrifice_obligation_kernel__messianic_suspension_reading, scaffold).
narrative_ontology:human_readable(sacrifice_obligation_kernel__messianic_suspension_reading, "Messianic Suspension of Sacrifice Obligation with Study Readiness").
narrative_ontology:topic_domain(sacrifice_obligation_kernel__messianic_suspension_reading, "religious_law/halakhic_authority").

narrative_ontology:has_sunset_clause(sacrifice_obligation_kernel__messianic_suspension_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sacrifice_obligation_kernel__messianic_suspension_reading, '0fd94dc2-5319-40c8-857c-0610c3857dac').
narrative_ontology:cs_kernel_codification('0fd94dc2-5319-40c8-857c-0610c3857dac', fixed_text).
narrative_ontology:cs_authority_grounding('0fd94dc2-5319-40c8-857c-0610c3857dac', lineage).
narrative_ontology:cs_interpretation_layer_present('0fd94dc2-5319-40c8-857c-0610c3857dac').
narrative_ontology:cs_reading_relation('0fd94dc2-5319-40c8-857c-0610c3857dac', sacrifice_obligation_kernel__study_as_exercise_reading, coexists_with).
narrative_ontology:cs_reading_relation('0fd94dc2-5319-40c8-857c-0610c3857dac', sacrifice_obligation_kernel__performance_only_reading, forecloses).
narrative_ontology:cs_reading_relation('0fd94dc2-5319-40c8-857c-0610c3857dac', sacrifice_obligation_kernel__symbolic_archive_reading, coexists_with).
narrative_ontology:cs_axiom('0fd94dc2-5319-40c8-857c-0610c3857dac', foundational, divine_suspension_preserves_obligation_structure).
narrative_ontology:cs_axiom_status(divine_suspension_preserves_obligation_structure, holdable).
narrative_ontology:cs_axiom_grounding('0fd94dc2-5319-40c8-857c-0610c3857dac', divine_suspension_preserves_obligation_structure, theological).
narrative_ontology:cs_axiom('0fd94dc2-5319-40c8-857c-0610c3857dac', foundational, study_maintains_operational_readiness_not_fulfillment).
narrative_ontology:cs_axiom_status(study_maintains_operational_readiness_not_fulfillment, holdable).
narrative_ontology:cs_axiom_grounding('0fd94dc2-5319-40c8-857c-0610c3857dac', study_maintains_operational_readiness_not_fulfillment, instrumental).
narrative_ontology:cs_reference_frame('0fd94dc2-5319-40c8-857c-0610c3857dac', divine_command_suspended_until_restoration).
narrative_ontology:cs_drift_state('0fd94dc2-5319-40c8-857c-0610c3857dac', post_temple_destruction_rabbinic_crystallization, gap(stable, minor, true)).
narrative_ontology:cs_created_at('0fd94dc2-5319-40c8-857c-0610c3857dac', '').
narrative_ontology:cs_kernel_id(sacrifice_obligation_kernel__messianic_suspension_reading, sacrifice_obligation_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sacrifice_obligation_kernel__messianic_suspension_reading, future_generations).
narrative_ontology:constraint_beneficiary(sacrifice_obligation_kernel__messianic_suspension_reading, restoration_priesthood).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(sacrifice_obligation_kernel__messianic_suspension_reading, current_practitioners).
narrative_ontology:constraint_vindicates(sacrifice_obligation_kernel__messianic_suspension_reading, divine_suspension_preserves_obligation_integrity).
narrative_ontology:constraint_vindicates(sacrifice_obligation_kernel__messianic_suspension_reading, study_as_operational_readiness_not_substitution).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintain the halakhic framework of suspension and the study curriculum (kodashim). They interpret divine suspension, define study parameters, and preserve priestly lineage records. They hold institutional authority over the constraint's administration but collect no material extraction from it.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__messianic_suspension_reading, rabbinic_authorities, agenda_setter,
    institutional, generational, arbitrage, global).

% Engage in study of sacrifice laws voluntarily. The biblical obligation is suspended, so they bear no burden of impossible performance. Study provides intellectual-spiritual engagement and maintains connection to the Temple tradition. Exit is mobile — they can participate or not without penalty.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__messianic_suspension_reading, current_practitioners, beneficiary,
    organized, biographical, mobile, global).

% The ultimate beneficiaries of the suspension framework. They will inherit the preserved operational knowledge, priestly lineages, and halakhic structure needed for immediate sacrificial restoration when the messianic condition obtains.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__messianic_suspension_reading, future_generations, beneficiary,
    analytical, generational, analytical, universal).

% Priestly lineages (kohanim/leviim) maintained through halakhic status rules. They benefit from the suspension framework because it preserves their future ritual role. Their exit is constrained — lineage is ascribed, not chosen — but they bear no current extraction.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__messianic_suspension_reading, restoration_priesthood, beneficiary,
    organized, generational, constrained, global).

% The priesthood that would have performed sacrifices had the Temple stood. They are structurally excluded from the current conversation (historically absent). Had the obligation remained active without suspension, they would be payers (bound to impossible performance). Under suspension, their seat is vacant.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__messianic_suspension_reading, historical_priesthood, excluded,
    powerless, immediate, trapped, local).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(sacrifice_obligation_kernel__messianic_suspension_reading, diffuse).
narrative_ontology:fixing_cost_class(sacrifice_obligation_kernel__messianic_suspension_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Preserves the operational knowledge, procedural details, and priestly lineage integrity of the biblical sacrifice system across the exile period, so that restoration can occur immediately when the messianic condition obtains.
% TRANSFER_FUNCTION: Moves intellectual effort and educational resources from current practitioners (voluntary study) to future restoration capacity (preserved knowledge and lineage). No material transfer occurs during suspension.
% ABSENT_VOICES: The historical priesthood (who would have performed but cannot) and reform/liberal voices that argue the obligation is transformed (prayer substitutes) or obsolete. The former are historically absent; the latter are outside the halakhic commitment system.
% DISAPPEARANCE_RATIONALE: If the suspension framework vanished overnight, the halakhic structure preserving sacrifice law for restoration would collapse. Priestly lineage rules would lose their ritual rationale, kodashim study would lose its teleological anchor, and the restoration-ready knowledge system would fragment — the world of halakhic anticipation would rearrange.
% FOUNDING_PROBLEM: The destruction of the Second Temple (70 CE) made the biblical sacrifice obligation impossible to perform, creating a crisis: how to maintain a divine command that cannot be fulfilled without violating it or transforming it beyond recognition.
% FOUNDING_PROBLEM_CORROBORATION: Maimonides (Hilchot Melachim 11:1) and the Talmud (Yoma 5b, Menachot 110a) attest that sacrifices will resume in the messianic era and study preserves readiness. Modern academic scholars (e.g., Jacob Neusner, Haym Soloveitchik) corroborate from outside the beneficiary set that the suspension doctrine crystallized in rabbinic literature as a response to Temple destruction, not as an original biblical feature.
narrative_ontology:disappearance_verdict(sacrifice_obligation_kernel__messianic_suspension_reading, world_rearranges).
narrative_ontology:founding_problem_status(sacrifice_obligation_kernel__messianic_suspension_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(sacrifice_obligation_kernel__messianic_suspension_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(sacrifice_obligation_kernel__messianic_suspension_reading, 'none', 1).
narrative_ontology:epsilon_provenance(sacrifice_obligation_kernel__messianic_suspension_reading, 0.08, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sacrifice_obligation_kernel__messianic_suspension_reading_tests).
:- end_tests(sacrifice_obligation_kernel__messianic_suspension_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.08: obligation suspended, no transfer of resources from current agents. Suppression 0.05: no coercion — suspension is divine decree, not human enforcement. Theater 0.12: study is genuine readiness maintenance, though some academic drift appears in later centuries. Accessibility collapse 0.35: alternatives (e.g., prayer as substitute) exist but are halakhically distinct; suspension preserves the specific sacrifice structure. Resistance 0.15: minimal — the framework is widely accepted in traditional circles; dissent comes from outside the commitment system (reform, secular). The claimed_type scaffold fits: temporary support (suspension + study framework) with sunset (messianic restoration), coordination function (knowledge preservation).
 *
 * PERSPECTIVAL GAP:
 *   From the rabbinic authority seat, the constraint is genuine coordination (preserving Torah structure through exile). From a secular/historical seat, it may appear as identity_coordination maintaining group cohesion. From a reform seat, it appears as obsolete ritual preserved theatrically. The engine computes per-seat types from power/exit: rabbinic authorities (institutional/arbitrage) see rope/scaffold; current practitioners (organized/mobile) see scaffold; excluded historical priesthood (powerless/trapped) would see snare if obligation were active — but under suspension, their seat is vacant.
 *
 * DIRECTIONALITY LOGIC:
 *   Rabbinc authorities (agenda_setter) hold institutional power but extract near-zero — they maintain the framework without collecting rents. Current practitioners are beneficiaries (d ~0.1): suspended obligation removes burden, study provides meaning. Future generations and restoration priesthood are beneficiaries (d ~0.05): they inherit preserved knowledge. No payers exist during suspension — the victim set is empty per the reading's own structural claim. Historical priesthood is excluded (would have been payers if obligation active).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (Temple destruction making sacrifice impossible) remains live — messianic restoration has not occurred. The scaffold has not atrophied into piton because the sunset condition is explicit and anticipated, study maintains genuine function (not mere performance), and no concentrated beneficiary captures extraction. The mandate persists because its function (readiness preservation) is still needed until restoration.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    committer_kernel_reading_identity,
    'Is the messianic suspension reading a distinct constraint from other readings of the sacrifice obligation kernel, or a measurement variant of the same constraint?',
    'Compare ε values across readings: if study_as_exercise_reading shows substantially higher extractiveness (study as fulfillment creates performance pressure) or performance_only_reading shows victims (those who fail impossible obligation), they are distinct constraints per ε-invariance.',
    'If distinct, each reading gets its own constraint story with own ε, stakeholders, classification. If same constraint, the kernel would need a single ε — but the declared structural deltas (victim presence/absence, extraction level) suggest decomposition is correct.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(committer_kernel_reading_identity, conceptual, 'Whether the four declared readings of sacrifice_obligation_kernel are one constraint with observational variance or four structurally distinct constraints.').

omega_variable(
    divine_suspension_naturalness,
    'Is the divine suspension of sacrifice obligation a genuine metaphysical status (mountain-like) or a constructed halakhic category that benefits identifiable agents?',
    'Historical analysis of when suspension doctrine crystallized; whether alternative frameworks (e.g., early Christian supersession, Karaites rejection of rabbinic suspension) were suppressed; whether current priesthood lineages benefit from suspension''s preservation of their future role.',
    'If constructed with beneficiaries (priesthood lineages, rabbinic authority), false_summit_mountain signature could trigger reclassification to tangled_rope. If genuine metaphysical status, mountain certification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(divine_suspension_naturalness, empirical, 'Natural-law vs. constructed status of the divine suspension doctrine.').

omega_variable(
    study_instrumentality_boundary,
    'Does study of sacrifice law genuinely maintain operational readiness, or does it functionally substitute for performance (making the obligation ''occupied'' without restoration)?',
    'Analyze halakhic discourse: if authorities treat study as discharging the obligation (even partially), extraction rises (performance pressure on students). If study is explicitly preparatory only, extraction stays near zero.',
    'If study substitutes, extractiveness increases toward tangled_rope territory; if purely instrumental, scaffold classification with low ε holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(study_instrumentality_boundary, conceptual, 'Whether the instrumental study framework leaks into substitutive fulfillment.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sacrifice_obligation_kernel__messianic_suspension_reading, 0, 1950).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sacrifice_messianic_suspension_tr_t0, sacrifice_obligation_kernel__messianic_suspension_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(sacrifice_messianic_suspension_tr_t500, sacrifice_obligation_kernel__messianic_suspension_reading, theater_ratio, 500, 0.08).
narrative_ontology:measurement(sacrifice_messianic_suspension_tr_t1000, sacrifice_obligation_kernel__messianic_suspension_reading, theater_ratio, 1000, 0.1).
narrative_ontology:measurement(sacrifice_messianic_suspension_tr_t1500, sacrifice_obligation_kernel__messianic_suspension_reading, theater_ratio, 1500, 0.11).
narrative_ontology:measurement(sacrifice_messianic_suspension_tr_t1950, sacrifice_obligation_kernel__messianic_suspension_reading, theater_ratio, 1950, 0.12).

% Extraction over time
narrative_ontology:measurement(sacrifice_messianic_suspension_be_t0, sacrifice_obligation_kernel__messianic_suspension_reading, base_extractiveness, 0, 0.05).
narrative_ontology:measurement(sacrifice_messianic_suspension_be_t500, sacrifice_obligation_kernel__messianic_suspension_reading, base_extractiveness, 500, 0.06).
narrative_ontology:measurement(sacrifice_messianic_suspension_be_t1000, sacrifice_obligation_kernel__messianic_suspension_reading, base_extractiveness, 1000, 0.07).
narrative_ontology:measurement(sacrifice_messianic_suspension_be_t1500, sacrifice_obligation_kernel__messianic_suspension_reading, base_extractiveness, 1500, 0.08).
narrative_ontology:measurement(sacrifice_messianic_suspension_be_t1950, sacrifice_obligation_kernel__messianic_suspension_reading, base_extractiveness, 1950, 0.08).

% Suppression requirement over time
narrative_ontology:measurement(sacrifice_messianic_suspension_su_t0, sacrifice_obligation_kernel__messianic_suspension_reading, suppression_requirement, 0, 0.02).
narrative_ontology:measurement(sacrifice_messianic_suspension_su_t500, sacrifice_obligation_kernel__messianic_suspension_reading, suppression_requirement, 500, 0.03).
narrative_ontology:measurement(sacrifice_messianic_suspension_su_t1000, sacrifice_obligation_kernel__messianic_suspension_reading, suppression_requirement, 1000, 0.04).
narrative_ontology:measurement(sacrifice_messianic_suspension_su_t1500, sacrifice_obligation_kernel__messianic_suspension_reading, suppression_requirement, 1500, 0.05).
narrative_ontology:measurement(sacrifice_messianic_suspension_su_t1950, sacrifice_obligation_kernel__messianic_suspension_reading, suppression_requirement, 1950, 0.05).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sacrifice_obligation_kernel__messianic_suspension_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(sacrifice_obligation_kernel__messianic_suspension_reading, 0.08).
narrative_ontology:affects_constraint(sacrifice_obligation_kernel__messianic_suspension_reading, sacrifice_obligation_kernel__study_as_exercise_reading).
narrative_ontology:affects_constraint(sacrifice_obligation_kernel__messianic_suspension_reading, sacrifice_obligation_kernel__performance_only_reading).
narrative_ontology:affects_constraint(sacrifice_obligation_kernel__messianic_suspension_reading, sacrifice_obligation_kernel__symbolic_archive_reading).

% DUAL FORMULATION NOTE:
% This constraint family decomposes the 'sacrifice obligation' label into four structurally distinct readings with divergent ε values and victim/beneficiary structures. The messianic_suspension_reading has near-zero ε and empty victim set; study_as_exercise_reading likely has higher ε (study-as-fulfillment creates performance pressure); performance_only_reading has victims (those bound by impossible obligation); symbolic_archive_reading has no halakhic extraction but identity_coordination extraction.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(sacrifice_obligation_kernel__messianic_suspension_reading, institutional, 0.15).
constraint_indexing:directionality_override(sacrifice_obligation_kernel__messianic_suspension_reading, organized, 0.1).
constraint_indexing:directionality_override(sacrifice_obligation_kernel__messianic_suspension_reading, powerless, 0.9).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
