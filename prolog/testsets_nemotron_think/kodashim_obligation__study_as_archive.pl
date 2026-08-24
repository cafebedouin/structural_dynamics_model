% ============================================================================
% CONSTRAINT STORY: kodashim_obligation__study_as_archive
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-04
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_kodashim_obligation__study_as_archive, []).

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
 *   constraint_id: kodashim_obligation__study_as_archive
 *   human_readable: Kodashim Study as Historical Archive and Identity Maintenance
 *   domain: religious_studies/jewish_law/textual_preservation
 *
 * SUMMARY:
 *   Kodashim (the fifth order of the Mishnah) details the sacrificial system
 *   that operated in the Jerusalem Temple until 70 CE. The 'study_as_archive'
 *   reading holds that the ongoing obligation to study these tractates in
 *   traditional yeshivas is not a legal requirement for a future Temple, nor
 *   a cosmic enactment, but a historical preservation practice that serves
 *   communal identity. The constraint extracts intellectual resources from
 *   applicable halakhah (moderate extractiveness, rising over time as the
 *   corpus expands and curricula ossify) while providing a coordination
 *   benefit: a shared, portable textual heritage that binds a diasporic
 *   people. The theater ratio is high and rising because the study is
 *   increasingly performed as ritualized curriculum rather than engaged legal
 *   analysis. Suppression is low-moderate: no one is jailed for skipping
 *   Kodashim, but the social and institutional costs of opting out are
 *   significant.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(kodashim_obligation__study_as_archive, 0.42).
domain_priors:suppression_score(kodashim_obligation__study_as_archive, 0.28).
domain_priors:theater_ratio(kodashim_obligation__study_as_archive, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(kodashim_obligation__study_as_archive, extractiveness, 0.42).
narrative_ontology:constraint_metric(kodashim_obligation__study_as_archive, suppression_requirement, 0.28).
narrative_ontology:constraint_metric(kodashim_obligation__study_as_archive, theater_ratio, 0.55).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(kodashim_obligation__study_as_archive, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(kodashim_obligation__study_as_archive, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(kodashim_obligation__study_as_archive, tangled_rope).
narrative_ontology:human_readable(kodashim_obligation__study_as_archive, "Kodashim Study as Historical Archive and Identity Maintenance").
narrative_ontology:topic_domain(kodashim_obligation__study_as_archive, "religious_studies/jewish_law/textual_preservation").

domain_priors:requires_active_enforcement(kodashim_obligation__study_as_archive).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(kodashim_obligation__study_as_archive, 'f43cfe33-bfa2-4364-af56-772c37cfad91').
narrative_ontology:cs_kernel_codification('f43cfe33-bfa2-4364-af56-772c37cfad91', distributed).
narrative_ontology:cs_authority_grounding('f43cfe33-bfa2-4364-af56-772c37cfad91', lineage).
narrative_ontology:cs_interpretation_layer_present('f43cfe33-bfa2-4364-af56-772c37cfad91').
narrative_ontology:cs_reading_relation('f43cfe33-bfa2-4364-af56-772c37cfad91', kodashim_obligation__study_as_performance, coexists_with).
narrative_ontology:cs_reading_relation('f43cfe33-bfa2-4364-af56-772c37cfad91', kodashim_obligation__study_as_preparation, coexists_with).
narrative_ontology:cs_axiom('f43cfe33-bfa2-4364-af56-772c37cfad91', foundational, temple_restoration_structurally_impossible).
narrative_ontology:cs_axiom_status(temple_restoration_structurally_impossible, holdable).
narrative_ontology:cs_axiom_grounding('f43cfe33-bfa2-4364-af56-772c37cfad91', temple_restoration_structurally_impossible, empirically_contingent).
narrative_ontology:cs_axiom('f43cfe33-bfa2-4364-af56-772c37cfad91', foundational, textual_preservation_sufficient_for_memory).
narrative_ontology:cs_axiom_status(textual_preservation_sufficient_for_memory, holdable).
narrative_ontology:cs_axiom_grounding('f43cfe33-bfa2-4364-af56-772c37cfad91', textual_preservation_sufficient_for_memory, conventional).
narrative_ontology:cs_reference_frame('f43cfe33-bfa2-4364-af56-772c37cfad91', post_churban_rabbinic_transmutation).
narrative_ontology:cs_drift_state('f43cfe33-bfa2-4364-af56-772c37cfad91', contemporary_yeshiva_curriculum, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('f43cfe33-bfa2-4364-af56-772c37cfad91', '').
narrative_ontology:cs_kernel_id(kodashim_obligation__study_as_archive, kodashim_obligation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(kodashim_obligation__study_as_archive, jewish_communal_identity).
narrative_ontology:constraint_victim(kodashim_obligation__study_as_archive, talmudic_scholars).
narrative_ontology:constraint_victim(kodashim_obligation__study_as_archive, yeshiva_students).
narrative_ontology:constraint_vindicates(kodashim_obligation__study_as_archive, torah_study_as_identity_anchor).
narrative_ontology:constraint_vindicates(kodashim_obligation__study_as_archive, historical_continuity_through_text).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The collective Jewish community derives cohesion and self-definition from the shared practice of studying Kodashim, even though the sacrificial system it describes has been defunct for two millennia. The study signals belonging and continuity; opting out weakens the communal narrative but carries no formal penalty.
narrative_ontology:constraint_stakeholder(kodashim_obligation__study_as_archive, jewish_communal_identity, beneficiary,
    organized, generational, identity_locked, global).

% Scholars invest significant intellectual labor in mastering and teaching Kodashim tractates — labor that could be directed toward applicable halakhah (e.g., Nashim, Nezikin). Career advancement in yeshiva and kollel systems often depends on facility with the entire Talmud, including Kodashim, creating structural pressure to allocate time to a system with no operative legal output.
narrative_ontology:constraint_stakeholder(kodashim_obligation__study_as_archive, talmudic_scholars, payer,
    moderate, biographical, constrained, global).

% Students in traditional yeshiva curricula spend years on Kodashim because the canonical curriculum treats the entire Talmud as a closed, mandatory corpus. Their exit options are limited: leaving the yeshiva world entails severe social and economic costs, while staying requires mastering material they will never apply in practice.
narrative_ontology:constraint_stakeholder(kodashim_obligation__study_as_archive, yeshiva_students, payer,
    powerless, biographical, constrained, global).

% Contemporary poskim (decisors) rarely cite Kodashim for practical rulings; they treat it as a historical layer. They observe the communal investment but do not themselves enforce the study obligation — their role is analytical, noting the gap between curriculum and applicability.
narrative_ontology:constraint_stakeholder(kodashim_obligation__study_as_archive, modern_halakhic_decisors, observer,
    institutional, generational, analytical, global).

% University scholars study Kodashim as ancient history, ritual theory, and textual criticism. They would argue that the traditional yeshiva approach misrepresents the text's nature and diverts talent from critical scholarship, but they are structurally excluded from the yeshiva curriculum-setting process.
narrative_ontology:constraint_stakeholder(kodashim_obligation__study_as_archive, academic_judaica_scholars, excluded,
    moderate, biographical, mobile, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains a shared textual heritage that anchors Jewish communal identity across diaspora and modernity; the study of a defunct sacrificial system serves as a portable, non-geographic symbol of collective continuity.
% TRANSFER_FUNCTION: Diverts intellectual labor (scholarly attention, student years, curricular space) from applicable halakhic domains (family law, civil law, ethics) to the mastery of a system with no current legal force. The transfer is from present-day legal needs to historical preservation.
% ABSENT_VOICES: Academic Judaica scholars and progressive Jewish educators who would reframe Kodashim as an object of critical study rather than a canonical obligation are excluded from yeshiva governance. Their objection — that the curriculum fossilizes a historical artifact at the expense of living law — is not heard in the rooms where curricula are set.
% DISAPPEARANCE_RATIONALE: If the expectation to study Kodashim vanished overnight, yeshiva curricula would shift dramatically toward applicable halakhah; scholarly careers built on Kodashim expertise would lose their primary institutional justification; the communal identity marker would weaken, prompting a search for alternative cohesion mechanisms (e.g., Zionism, ethics, liturgy). The world of traditional Torah study would reorganize.
% FOUNDING_PROBLEM: After the Temple's destruction (70 CE), the rabbis faced a crisis: how to preserve the sacrificial system's memory and theological significance when its performance was impossible. Studying its laws became a substitute for performance, maintaining the system's place in Jewish consciousness.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem — preserving sacrificial memory against oblivion — is attested by the Talmud itself (Menachot 110a: "Whoever studies the laws of the sin-offering is as if he offered one"). Modern historians (e.g., Jacob Neusner, Shaye Cohen) corroborate that the rabbinic project was precisely to transmute cult into text. No contemporary voice outside the traditionalist camp claims the original crisis (Temple loss) is still live; the Temple has not stood for 1,950 years and mainstream Orthodox theology does not expect imminent restoration.
narrative_ontology:disappearance_verdict(kodashim_obligation__study_as_archive, world_rearranges).
narrative_ontology:founding_problem_status(kodashim_obligation__study_as_archive, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(kodashim_obligation__study_as_archive, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(kodashim_obligation__study_as_archive, 'none', 1).
narrative_ontology:epsilon_provenance(kodashim_obligation__study_as_archive, 0.42, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(kodashim_obligation__study_as_archive_tests).
:- end_tests(kodashim_obligation__study_as_archive_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.42) reflects the diversion of scarce scholarly attention from live legal domains to a dead system. Suppression (0.28) captures the soft coercion of curriculum mandates and social expectations. Theater (0.55) indicates that over half the study activity is performative — maintaining the appearance of comprehensive Talmud mastery rather than producing applicable knowledge. Accessibility collapse (0.35) is moderate: alternatives (focus on applicable halakhah) exist and are pursued in some modern Orthodox and non-Orthodox settings, but the traditional yeshiva world treats the full Talmud as a closed set. Resistance (0.45) is notable: there have been periodic reform movements (e.g., Hildesheimer, Rav Kook's yeshiva vision) that sought to reduce Kodashim's curricular weight, but they were marginalized.
 *
 * PERSPECTIVAL GAP:
 *   From the communal-identity seat, the constraint is a rope (pure coordination: we study together to remember who we are). From the scholar/student seats, it is a snare (extraction: our labor is taken for a system that does not serve our legal needs). The engine will compute this divergence; the claimed_type 'tangled_rope' captures the structural hybridity.
 *
 * DIRECTIONALITY LOGIC:
 *   The primary beneficiary is jewish_communal_identity (organized, identity-locked, generational horizon) — it gains cohesion from the shared practice. The primary payers are talmudic_scholars and yeshiva_students (moderate/powerless, constrained exit, biographical horizon) — they bear the opportunity cost. Modern halakhic decisors are observers; they see the structure but do not enforce it. Academic scholars are excluded; they would challenge the framing but have no voice in yeshiva governance. The directionality derivation from beneficiary/victim + exit options places the payers at high d (target) and the communal identity at low d (beneficiary).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (preserving sacrificial memory after the Temple's destruction) is dead — the memory is securely preserved in the texts themselves, and no living authority claims the Temple's imminent restoration. Yet the study obligation persists and has intensified (rising extractiveness, rising theater). This is a classic mandatrophy case: a scaffold (substitute for performance) that lost its sunset condition (the Temple's restoration) and became a permanent extraction on the living legal system. The constraint is not a piton because it still has active enforcement (curriculum mandates) and a concentrated beneficiary (communal identity maintenance).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_ambiguity,
    'Is the ''study_as_archive'' reading a distinct constraint from the other two readings, or do they describe the same practice from different interpretive angles?',
    'Compare the beneficiary/victim structures and extractiveness profiles across the three readings. If the victim sets differ (e.g., study_as_preparation may not divert resources from applicable law if it is seen as preparatory for future practice), they are distinct constraints.',
    'If they are the same constraint, the ε-invariance principle is violated — one constraint would have multiple ε values. If distinct, they form a constraint family linked by network.affects_constraints.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_ambiguity, conceptual, 'Whether the three declared readings of kodashim_obligation are structurally distinct constraints or observational perspectives on one constraint.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression (0.28) primarily structural (curriculum mandates, social sanctions) or internalized (students and scholars genuinely believe Kodashim study is intrinsically valuable)?',
    'Post-exit interviews with former yeshiva students: if suppression persists after leaving the yeshiva world (guilt, identity crisis), internalized component is significant.',
    'If internalized, effective suppression is higher than the structural measure suggests — the constraint travels with the agent after exit, increasing its extractive reach.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression in the yeshiva curriculum context.').

omega_variable(
    identity_coordination_floor,
    'Does the identity_coordination function (communal cohesion through shared study) have a genuine coordination cost that justifies the current extraction level, or is the identity framing a cover for resource extraction?',
    'Counterfactual: if Kodashim were removed from the curriculum but replaced with another shared text (e.g., Ethics of the Fathers, Maimonides'' Guide), would communal cohesion degrade? If not, the specific text is not the coordination mechanism.',
    'If the specific text is replaceable, the extraction is not necessary for coordination — the constraint is more snare-like. If irreplaceable, the extraction may be the price of a genuine coordination function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_coordination_floor, conceptual, 'Whether the identity coordination function is text-specific or fungible.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(kodashim_obligation__study_as_archive, 0, 2000).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(koda_tr_t0, kodashim_obligation__study_as_archive, theater_ratio, 0, 0.2).
narrative_ontology:measurement(koda_tr_t500, kodashim_obligation__study_as_archive, theater_ratio, 500, 0.3).
narrative_ontology:measurement(koda_tr_t1000, kodashim_obligation__study_as_archive, theater_ratio, 1000, 0.4).
narrative_ontology:measurement(koda_tr_t1500, kodashim_obligation__study_as_archive, theater_ratio, 1500, 0.48).
narrative_ontology:measurement(koda_tr_t2000, kodashim_obligation__study_as_archive, theater_ratio, 2000, 0.55).

% Extraction over time
narrative_ontology:measurement(koda_be_t0, kodashim_obligation__study_as_archive, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(koda_be_t500, kodashim_obligation__study_as_archive, base_extractiveness, 500, 0.22).
narrative_ontology:measurement(koda_be_t1000, kodashim_obligation__study_as_archive, base_extractiveness, 1000, 0.3).
narrative_ontology:measurement(koda_be_t1500, kodashim_obligation__study_as_archive, base_extractiveness, 1500, 0.38).
narrative_ontology:measurement(koda_be_t2000, kodashim_obligation__study_as_archive, base_extractiveness, 2000, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(koda_su_t0, kodashim_obligation__study_as_archive, suppression_requirement, 0, 0.1).
narrative_ontology:measurement(koda_su_t500, kodashim_obligation__study_as_archive, suppression_requirement, 500, 0.15).
narrative_ontology:measurement(koda_su_t1000, kodashim_obligation__study_as_archive, suppression_requirement, 1000, 0.2).
narrative_ontology:measurement(koda_su_t1500, kodashim_obligation__study_as_archive, suppression_requirement, 1500, 0.25).
narrative_ontology:measurement(koda_su_t2000, kodashim_obligation__study_as_archive, suppression_requirement, 2000, 0.28).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(kodashim_obligation__study_as_archive, identity_coordination).
narrative_ontology:boltzmann_floor_override(kodashim_obligation__study_as_archive, 0.08).
narrative_ontology:affects_constraint(kodashim_obligation__study_as_archive, kodashim_obligation__study_as_performance).
narrative_ontology:affects_constraint(kodashim_obligation__study_as_archive, kodashim_obligation__study_as_preparation).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the kodashim_obligation kernel. The three readings differ in their beneficiary/victim structures and extractiveness profiles: study_as_archive (moderate extraction, identity beneficiary, scholar/student victims), study_as_performance (low extraction, cosmic beneficiary, no clear victims), study_as_preparation (low-moderate extraction, future-restoration beneficiary, current scholars as investors). They are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
