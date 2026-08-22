% ============================================================================
% CONSTRAINT STORY: sacrifice_obligation_kernel__symbolic_archive_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sacrifice_obligation_kernel__symbolic_archive_reading, []).

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
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: sacrifice_obligation_kernel__symbolic_archive_reading
 *   human_readable: Sacrifice Law as Cultural-Historical Archive (Symbolic Archive Reading)
 *   domain: religious_law/halakhic_authority/commitment_system_dynamics
 *
 * SUMMARY:
 *   This constraint story models the symbolic_archive_reading of the
 *   sacrifice_obligation_kernel: sacrifice law (korbanot) is understood as a
 *   cultural-historical archive whose study preserves Jewish collective
 *   memory and identity continuity, but which makes no binding halakhic claim
 *   on contemporary practitioners. Study is voluntary cultural practice; no
 *   obligation exists to be violated; there is no victim set. The beneficiary
 *   is Jewish collective memory and identity — a non-coercive preservation
 *   function. This reading stands in structural contrast to three sibling
 *   readings: messianic_suspension_reading (obligation divinely suspended,
 *   study maintains readiness), performance_only_reading (physical
 *   performance required, study is preparatory), and
 *   study_as_exercise_reading (study genuinely occupies the mitzvah). The
 *   zero-extraction profile is the defining structural feature.
 *
 * KEY AGENTS:
 *   - jewish_collective_memory: Primary beneficiary (non-coercive preservation) — receives identity continuity through voluntary study
 *   - jewish_identity_continuity: Primary beneficiary (non-coercive preservation) — maintained through cultural transmission of sacrifice law
 *   - study_practitioners: Voluntary participants (analytical/mobile exit) — engage with archive without halakhic compulsion
 *   - halakhic_authorities_symbolic: Observer/agenda_setter (institutional/analytical) — articulate the reading but enforce nothing
 *   - messianic_suspension_adherents: Excluded from this reading's frame (would object to zero-obligation claim) — hold sibling reading
 *   - performance_only_adherents: Excluded from this reading's frame (would object to zero-obligation claim) — hold sibling reading
 *   - study_as_exercise_adherents: Excluded from this reading's frame (would object to 'no halakhic claim') — hold sibling reading
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sacrifice_obligation_kernel__symbolic_archive_reading, 0.02).
domain_priors:suppression_score(sacrifice_obligation_kernel__symbolic_archive_reading, 0.05).
domain_priors:theater_ratio(sacrifice_obligation_kernel__symbolic_archive_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__symbolic_archive_reading, extractiveness, 0.02).
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__symbolic_archive_reading, suppression_requirement, 0.05).
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__symbolic_archive_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__symbolic_archive_reading, accessibility_collapse, 0.25).
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__symbolic_archive_reading, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sacrifice_obligation_kernel__symbolic_archive_reading, rope).
narrative_ontology:human_readable(sacrifice_obligation_kernel__symbolic_archive_reading, "Sacrifice Law as Cultural-Historical Archive (Symbolic Archive Reading)").
narrative_ontology:topic_domain(sacrifice_obligation_kernel__symbolic_archive_reading, "religious_law/halakhic_authority/commitment_system_dynamics").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sacrifice_obligation_kernel__symbolic_archive_reading, '1f6c8125-5f1d-4b72-bcd3-1b0fd429286a').
narrative_ontology:cs_kernel_codification('1f6c8125-5f1d-4b72-bcd3-1b0fd429286a', fixed_text).
narrative_ontology:cs_authority_grounding('1f6c8125-5f1d-4b72-bcd3-1b0fd429286a', lineage).
narrative_ontology:cs_interpretation_layer_present('1f6c8125-5f1d-4b72-bcd3-1b0fd429286a').
narrative_ontology:cs_reading_relation('1f6c8125-5f1d-4b72-bcd3-1b0fd429286a', sacrifice_obligation_kernel__messianic_suspension_reading, coexists_with).
narrative_ontology:cs_reading_relation('1f6c8125-5f1d-4b72-bcd3-1b0fd429286a', sacrifice_obligation_kernel__performance_only_reading, coexists_with).
narrative_ontology:cs_reading_relation('1f6c8125-5f1d-4b72-bcd3-1b0fd429286a', sacrifice_obligation_kernel__study_as_exercise_reading, coexists_with).
narrative_ontology:cs_axiom('1f6c8125-5f1d-4b72-bcd3-1b0fd429286a', foundational, sacrifice_law_is_cultural_archive).
narrative_ontology:cs_axiom_status(sacrifice_law_is_cultural_archive, holdable).
narrative_ontology:cs_axiom_grounding('1f6c8125-5f1d-4b72-bcd3-1b0fd429286a', sacrifice_law_is_cultural_archive, conventional).
narrative_ontology:cs_axiom('1f6c8125-5f1d-4b72-bcd3-1b0fd429286a', foundational, study_carries_no_halakhic_obligation).
narrative_ontology:cs_axiom_status(study_carries_no_halakhic_obligation, holdable).
narrative_ontology:cs_axiom_grounding('1f6c8125-5f1d-4b72-bcd3-1b0fd429286a', study_carries_no_halakhic_obligation, deontological).
narrative_ontology:cs_reference_frame('1f6c8125-5f1d-4b72-bcd3-1b0fd429286a', post_temple_cultural_preservation).
narrative_ontology:cs_drift_state('1f6c8125-5f1d-4b72-bcd3-1b0fd429286a', contemporary_voluntary_study_practice, gap(stable, minor, true)).
narrative_ontology:cs_created_at('1f6c8125-5f1d-4b72-bcd3-1b0fd429286a', '').
narrative_ontology:cs_kernel_id(sacrifice_obligation_kernel__symbolic_archive_reading, sacrifice_obligation_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sacrifice_obligation_kernel__symbolic_archive_reading, jewish_collective_memory).
narrative_ontology:constraint_beneficiary(sacrifice_obligation_kernel__symbolic_archive_reading, jewish_identity_continuity).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(sacrifice_obligation_kernel__symbolic_archive_reading, study_practitioners).
narrative_ontology:constraint_victim(sacrifice_obligation_kernel__symbolic_archive_reading, study_practitioners).
narrative_ontology:constraint_vindicates(sacrifice_obligation_kernel__symbolic_archive_reading, cultural_preservation_through_study).
narrative_ontology:constraint_vindicates(sacrifice_obligation_kernel__symbolic_archive_reading, voluntary_identity_continuity).
narrative_ontology:constraint_vindicates(sacrifice_obligation_kernel__symbolic_archive_reading, non_coercive_halakhic_memory).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Receives preservation value when sacrifice law is studied voluntarily. No agency to enforce study; benefits accumulate from aggregate voluntary participation. Not a rent-collecting actor.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__symbolic_archive_reading, jewish_collective_memory, beneficiary,
    analytical, civilizational, analytical, universal).

% Maintained through cultural transmission of sacrifice law archive. Benefits from voluntary study but cannot compel it. Identity continuity is the coordination output, not an extracting agent.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__symbolic_archive_reading, jewish_identity_continuity, beneficiary,
    analytical, civilizational, analytical, universal).

% Voluntarily study sacrifice law for cultural, intellectual, or spiritual reasons. Pay time and attention (low cost, self-selected). Receive identity connection and cultural literacy. Exit is trivial — stop studying. No halakhic penalty for non-participation.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__symbolic_archive_reading, study_practitioners, payer,
    moderate, biographical, mobile, global).
narrative_ontology:stakeholder_secondary_role(sacrifice_obligation_kernel__symbolic_archive_reading, study_practitioners, beneficiary).

% Articulate and teach the symbolic_archive_reading: sacrifice law is an archive, study preserves identity, no halakhic claim. They coordinate the cultural transmission but enforce nothing. Their authority is epistemic, not coercive.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__symbolic_archive_reading, halakhic_authorities_symbolic, agenda_setter,
    institutional, generational, analytical, global).

% Hold the sibling reading: sacrifice obligation is divinely suspended, study maintains operational readiness for messianic restoration. They would object to the claim that study makes 'no halakhic claim.' Their exclusion from this reading's frame is structural — they inhabit a different constraint.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__symbolic_archive_reading, messianic_suspension_adherents, excluded,
    organized, generational, identity_locked, global).

% Hold the sibling reading: sacrifice requires physical performance in the Temple; study is preparatory but does not fulfill the mitzvah. They would object to any reading that treats study as sufficient or obligation as transformed. Excluded from this reading's frame.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__symbolic_archive_reading, performance_only_adherents, excluded,
    organized, generational, identity_locked, global).

% Hold the sibling reading: study of sacrifice law genuinely occupies the mitzvah; the obligation is fulfilled through intellectual engagement. They would object to 'no halakhic claim' — for them, study IS the halakhic claim. Excluded from this reading's frame.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__symbolic_archive_reading, study_as_exercise_adherents, excluded,
    organized, generational, identity_locked, global).

% Observes the kernel and all four readings structurally. Sees the zero-extraction profile of this reading, the nonzero extraction of siblings, and the contested beneficiary/victim structures. Not governed by any reading.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__symbolic_archive_reading, analytical_observer, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(sacrifice_obligation_kernel__symbolic_archive_reading, diffuse).
narrative_ontology:fixing_cost_class(sacrifice_obligation_kernel__symbolic_archive_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Preserves Jewish collective memory and identity continuity through voluntary study of sacrifice law as a cultural-historical archive. Solves the coordination problem of transmitting a discontinued practice's meaning across generations without coercion.
% TRANSFER_FUNCTION: Moves voluntary attention and study time from practitioners to the cultural archive (collective memory/identity). No mandatory transfer; no extraction from non-participants. The flow is opt-in cultural participation.
% ABSENT_VOICES: Adherents of the three sibling readings (messianic_suspension, performance_only, study_as_exercise) are structurally excluded from this reading's frame. They would object to the zero-obligation claim and the 'no halakhic claim' framing. They are not absent from the discourse — they are present in sibling constraints.
% DISAPPEARANCE_RATIONALE: If this reading vanished overnight, the cultural archive (sacrifice law texts, commentaries, traditions) would persist. Voluntary study would continue or not based on individual/communal choice. No binding arrangements depend on this reading; no enforcement would cease. The world stays roughly the same because the constraint never compelled anything.
% FOUNDING_PROBLEM: After the Temple's destruction (70 CE), sacrifice practice became physically impossible. The founding problem was how to preserve the meaning, structure, and memory of sacrifice law without the Temple — preventing total loss of a central Torah domain.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem is attested by the historical record (Talmudic discussions of post-Temple substitution, Maimonides' treatment of sacrifice law as theoretical study, modern scholars of Jewish cultural memory). No beneficiary group (collective memory/identity) can self-attest — the corroboration comes from historians, textual scholars, and the continuous transmission record across communities that do not hold this reading exclusively.
narrative_ontology:disappearance_verdict(sacrifice_obligation_kernel__symbolic_archive_reading, world_unchanged).
narrative_ontology:founding_problem_status(sacrifice_obligation_kernel__symbolic_archive_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(sacrifice_obligation_kernel__symbolic_archive_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(sacrifice_obligation_kernel__symbolic_archive_reading, 'none', 1).
narrative_ontology:epsilon_provenance(sacrifice_obligation_kernel__symbolic_archive_reading, 0.02, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sacrifice_obligation_kernel__symbolic_archive_reading_tests).
:- end_tests(sacrifice_obligation_kernel__symbolic_archive_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is near-zero (0.02) because the constraint creates no binding obligation and extracts no transfer from non-participants. Suppression is minimal (0.05) — no enforcement machinery exists; non-participation carries no formal sanction. Theater ratio is low but nonzero (0.15) because some communal settings perform study ritually while endorsing the voluntary framing — a gap between performance and avowed voluntariness. Accessibility collapse is low (0.25): alternatives (non-study, other identity practices) remain fully open. Resistance is near-zero (0.1): no one resists a voluntary archive. The claimed type is rope — a genuine coordination function (cultural transmission, identity continuity) with minimal coercive overhead and no extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (jewish_collective_memory, jewish_identity_continuity) are abstract collective goods, not rent-collecting actors. They receive preservation value from voluntary study. Study_practitioners are mobile/analytical — they participate by choice, exit trivially. Halakhic authorities who articulate this reading hold institutional power but exercise no enforcement (d near 0.5 — symmetric, they coordinate without extracting). The excluded sibling-reading adherents are not governed by this constraint; they inhabit different constraints (the sibling readings). Directionality derivation yields near-beneficiary d for all governed seats, consistent with zero effective extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint carries no mandatrophy risk because it makes no claim to solve a problem that has disappeared — it never claimed to be a binding obligation. Its founding problem (preserving cultural memory of a discontinued practice) remains live. The mandate (if any) is simply 'remember,' which is perpetually relevant to identity continuity. No sunset clause needed because no temporary measure was enacted.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reading_of_sacrifice_obligation_kernel,
    'This constraint is one reading (symbolic_archive_reading) of the contested kernel sacrifice_obligation_kernel. What structural elements distinguish this reading from its siblings?',
    'Comparative structural analysis of all four declared readings: messianic_suspension_reading, performance_only_reading, study_as_exercise_reading, and this reading. The disagreement is located in whether study of sacrifice law carries halakhic obligation and what happens when study is absent.',
    'This reading instantiates zero extractiveness and no victim set; sibling readings instantiate obligation structures with nonzero extraction and defined victim sets (those who fail the obligation). The kernel_id and reading_id structure must be preserved in the corpus to enable cross-reading contamination analysis.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_of_sacrifice_obligation_kernel, conceptual, 'Commitment-system framing: this reading treats sacrifice law as a cultural archive with no binding halakhic claim; study is voluntary preservation of identity.').

omega_variable(
    study_voluntariness_vs_communal_expectation,
    'Is study of sacrifice law truly voluntary (zero coercion), or does communal identity pressure create de facto obligation?',
    'Sociological study of communities that center this reading: measure participation rates, social sanctions for non-participation, and internal narratives about study. If social pressure is negligible, extraction remains near zero; if measurable, reclassify toward tangled_rope.',
    'If communal expectation creates de facto obligation, the reading''s self-description as zero-extraction is a false summit; the engine would detect nonzero effective extraction from identity-locked participants.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(study_voluntariness_vs_communal_expectation, empirical, 'Whether the ''voluntary'' framing holds under social pressure analysis.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sacrifice_obligation_kernel__symbolic_archive_reading, 0, 2000).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sacr_tr_t0, sacrifice_obligation_kernel__symbolic_archive_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(sacr_tr_t500, sacrifice_obligation_kernel__symbolic_archive_reading, theater_ratio, 500, 0.12).
narrative_ontology:measurement(sacr_tr_t1000, sacrifice_obligation_kernel__symbolic_archive_reading, theater_ratio, 1000, 0.13).
narrative_ontology:measurement(sacr_tr_t1500, sacrifice_obligation_kernel__symbolic_archive_reading, theater_ratio, 1500, 0.14).
narrative_ontology:measurement(sacr_tr_t2000, sacrifice_obligation_kernel__symbolic_archive_reading, theater_ratio, 2000, 0.15).

% Extraction over time
narrative_ontology:measurement(sacr_be_t0, sacrifice_obligation_kernel__symbolic_archive_reading, base_extractiveness, 0, 0.02).
narrative_ontology:measurement(sacr_be_t500, sacrifice_obligation_kernel__symbolic_archive_reading, base_extractiveness, 500, 0.02).
narrative_ontology:measurement(sacr_be_t1000, sacrifice_obligation_kernel__symbolic_archive_reading, base_extractiveness, 1000, 0.02).
narrative_ontology:measurement(sacr_be_t1500, sacrifice_obligation_kernel__symbolic_archive_reading, base_extractiveness, 1500, 0.02).
narrative_ontology:measurement(sacr_be_t2000, sacrifice_obligation_kernel__symbolic_archive_reading, base_extractiveness, 2000, 0.02).

% Suppression requirement over time
narrative_ontology:measurement(sacr_su_t0, sacrifice_obligation_kernel__symbolic_archive_reading, suppression_requirement, 0, 0.03).
narrative_ontology:measurement(sacr_su_t500, sacrifice_obligation_kernel__symbolic_archive_reading, suppression_requirement, 500, 0.04).
narrative_ontology:measurement(sacr_su_t1000, sacrifice_obligation_kernel__symbolic_archive_reading, suppression_requirement, 1000, 0.05).
narrative_ontology:measurement(sacr_su_t1500, sacrifice_obligation_kernel__symbolic_archive_reading, suppression_requirement, 1500, 0.05).
narrative_ontology:measurement(sacr_su_t2000, sacrifice_obligation_kernel__symbolic_archive_reading, suppression_requirement, 2000, 0.05).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sacrifice_obligation_kernel__symbolic_archive_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(sacrifice_obligation_kernel__symbolic_archive_reading, 0.08).
narrative_ontology:affects_constraint(sacrifice_obligation_kernel__symbolic_archive_reading, sacrifice_obligation_kernel__messianic_suspension_reading).
narrative_ontology:affects_constraint(sacrifice_obligation_kernel__symbolic_archive_reading, sacrifice_obligation_kernel__performance_only_reading).
narrative_ontology:affects_constraint(sacrifice_obligation_kernel__symbolic_archive_reading, sacrifice_obligation_kernel__study_as_exercise_reading).

% DUAL FORMULATION NOTE:
% This reading and its three siblings form the sacrifice_obligation_kernel constraint family. All four share the kernel (sacrifice law matters) but instantiate different constraints with different ε values, different victim sets, and different claimed types. This reading (symbolic_archive) has ε≈0.02, claimed_type=rope, no victims. messianic_suspension_reading has ε>0 (readiness maintenance extracts study effort), victims=those who fail readiness. performance_only_reading has ε>0 (physical performance required, study insufficient), victims=those who don't perform. study_as_exercise_reading has ε>0 (study is the obligation), victims=those who don't study. The ε-invariance principle requires separate stories; they are linked here via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
