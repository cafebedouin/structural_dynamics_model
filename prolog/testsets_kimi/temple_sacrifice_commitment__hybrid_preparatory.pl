% ============================================================================
% CONSTRAINT STORY: temple_sacrifice_commitment__hybrid_preparatory
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_temple_sacrifice_commitment__hybrid_preparatory, []).

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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:coordination_type/2,
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
 *   constraint_id: temple_sacrifice_commitment__hybrid_preparatory
 *   human_readable: Temple Sacrifice Commitment â Hybrid Preparatory Reading
 *   domain: religious_law/halakhic_tradition
 *
 * SUMMARY:
 *   The hybrid_preparatory reading holds that the biblical commandment of
 *   temple sacrifice remains binding but is temporarily suspended due to the
 *   absence of the Temple; study of sacrificial law is neither mere archival
 *   preservation (as performance_only claims) nor itself the fulfillment of
 *   the command (as study_as_exercise claims), but a preparatory occupation
 *   that maintains communal competence for messianic restoration. This
 *   reading sustains a massive institutional apparatusâyeshivot, rabbinic
 *   courts, fundraising networksâdedicated to a non-performable practice.
 *   The constraint extracts material resources from donors and cognitive/life
 *   resources from students, while coordinating the community around a shared
 *   eschatological horizon. It is claimed as transitional (preparatory) but
 *   lacks a sunset clause, operating instead on an indefinitely deferred
 *   messianic trigger.
 *
 * KEY AGENTS:
 *   - torah_study_institutions: Agenda-setter (institutional/constrained) â administer the suspended commitment through curricula and funding
 *   - rabbinic_jurists: Beneficiary (institutional/constrained) â authority and livelihood tied to expertise in non-performable law
 *   - donor_community: Payer (moderate/constrained) â funds study of law with no present outlet
 *   - students_of_temple_law: Payer (moderate/identity_locked) â devote life and cognition to non-performable domain
 *   - reformist_circles: Excluded (moderate/mobile) â would argue for transformation or abandonment
 *   - temple_activist_groups: Excluded (moderate/constrained) â would argue for immediate material performance
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(temple_sacrifice_commitment__hybrid_preparatory, 0.55).
domain_priors:suppression_score(temple_sacrifice_commitment__hybrid_preparatory, 0.45).
domain_priors:theater_ratio(temple_sacrifice_commitment__hybrid_preparatory, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(temple_sacrifice_commitment__hybrid_preparatory, extractiveness, 0.55).
narrative_ontology:constraint_metric(temple_sacrifice_commitment__hybrid_preparatory, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(temple_sacrifice_commitment__hybrid_preparatory, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(temple_sacrifice_commitment__hybrid_preparatory, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(temple_sacrifice_commitment__hybrid_preparatory, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(temple_sacrifice_commitment__hybrid_preparatory, tangled_rope).
narrative_ontology:human_readable(temple_sacrifice_commitment__hybrid_preparatory, "Temple Sacrifice Commitment â Hybrid Preparatory Reading").
narrative_ontology:topic_domain(temple_sacrifice_commitment__hybrid_preparatory, "religious_law/halakhic_tradition").

domain_priors:requires_active_enforcement(temple_sacrifice_commitment__hybrid_preparatory).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(temple_sacrifice_commitment__hybrid_preparatory, 'cbbcb20f-fdfb-4644-a3d8-4ff336583402').
narrative_ontology:cs_kernel_codification('cbbcb20f-fdfb-4644-a3d8-4ff336583402', fixed_text).
narrative_ontology:cs_authority_grounding('cbbcb20f-fdfb-4644-a3d8-4ff336583402', lineage).
narrative_ontology:cs_interpretation_layer_present('cbbcb20f-fdfb-4644-a3d8-4ff336583402').
narrative_ontology:cs_reading_relation('cbbcb20f-fdfb-4644-a3d8-4ff336583402', temple_sacrifice_commitment__study_as_exercise, coexists_with).
narrative_ontology:cs_reading_relation('cbbcb20f-fdfb-4644-a3d8-4ff336583402', temple_sacrifice_commitment__performance_only, coexists_with).
narrative_ontology:cs_reading_relation('cbbcb20f-fdfb-4644-a3d8-4ff336583402', temple_sacrifice_commitment__symbolic_transformation, coexists_with).
narrative_ontology:cs_axiom('cbbcb20f-fdfb-4644-a3d8-4ff336583402', foundational, suspended_obligation_requires_preparatory_study).
narrative_ontology:cs_axiom_status(suspended_obligation_requires_preparatory_study, holdable).
narrative_ontology:cs_axiom_grounding('cbbcb20f-fdfb-4644-a3d8-4ff336583402', suspended_obligation_requires_preparatory_study, deontological).
narrative_ontology:cs_axiom('cbbcb20f-fdfb-4644-a3d8-4ff336583402', foundational, messianic_restoration_maintains_validity).
narrative_ontology:cs_axiom_status(messianic_restoration_maintains_validity, holdable).
narrative_ontology:cs_axiom_grounding('cbbcb20f-fdfb-4644-a3d8-4ff336583402', messianic_restoration_maintains_validity, theological).
narrative_ontology:cs_reference_frame('cbbcb20f-fdfb-4644-a3d8-4ff336583402', temple_centric_torah_observance).
narrative_ontology:cs_drift_state('cbbcb20f-fdfb-4644-a3d8-4ff336583402', post_talmudic_diaspora, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('cbbcb20f-fdfb-4644-a3d8-4ff336583402', '').
narrative_ontology:cs_kernel_id(temple_sacrifice_commitment__hybrid_preparatory, temple_sacrifice_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(temple_sacrifice_commitment__hybrid_preparatory, torah_study_institutions).
narrative_ontology:constraint_beneficiary(temple_sacrifice_commitment__hybrid_preparatory, rabbinic_jurists).
narrative_ontology:constraint_victim(temple_sacrifice_commitment__hybrid_preparatory, donor_community).
narrative_ontology:constraint_victim(temple_sacrifice_commitment__hybrid_preparatory, students_of_temple_law).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administer curricula devoted to sacrificial law, train successive generations of scholars, and secure communal funding on the premise that this study maintains a covenantal commitment in suspended animation until messianic restoration.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__hybrid_preparatory, torah_study_institutions, agenda_setter,
    institutional, generational, constrained, global).

% Derive authority and livelihood from expertise in a corpus of law that cannot currently be performed; their juridical role depends on maintaining the normative weight of this suspended practice.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__hybrid_preparatory, rabbinic_jurists, beneficiary,
    institutional, generational, constrained, global).

% Contribute material resources to support full-time study of temple law; they bear opportunity costs of funds not directed to immediate welfare or performable commandments, sustained by the theological framing of preparation.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__hybrid_preparatory, donor_community, payer,
    moderate, biographical, constrained, national).

% Devote cognitive resources and prime life years to mastering intricate sacrificial law with no present performative outlet; exit is costly because professional and identity formation is entirely specific to this non-performable domain.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__hybrid_preparatory, students_of_temple_law, payer,
    moderate, biographical, identity_locked, national).

% Would argue that the sacrificial commitment has been transformed or superseded and that resources devoted to its preparatory study are misallocated; they are not in the halakhic conversation that sets curricula or funding priorities.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__hybrid_preparatory, reformist_circles, excluded,
    moderate, biographical, mobile, national).

% Would argue for immediate material performance of sacrifice on the Temple Mount; they are excluded from the mainstream rabbinic consensus that maintains the suspended, preparatory state rather than actualizing performance.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__hybrid_preparatory, temple_activist_groups, excluded,
    moderate, biographical, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Preserves intergenerational competence in sacrificial law and maintains communal unity around a shared eschatological horizon during a prolonged period when the practice cannot be materially performed.
% TRANSFER_FUNCTION: Moves material wealth from donors and cognitive/life resources from students to study institutions and rabbinic jurists, sustaining an expert class and institutional infrastructure around a non-performable practice.
% ABSENT_VOICES: Reformist scholars who would treat the sacrificial law as historically superseded; temple activist groups who demand immediate material performance; secular critics who would redirect resources to present welfare needs. All are structurally excluded from the halakhic agenda-setting process.
% DISAPPEARANCE_RATIONALE: If the commitment to preparatory study vanished, yeshivot would close or redirect curricula, rabbinic ordination tracks would collapse or restructure, donor streams would shift to performable commandments or welfare, and thousands of scholars would face identity and livelihood crisisâthe communal economy would rearrange.
% FOUNDING_PROBLEM: The destruction of the Second Temple in 70 CE eliminated the physical and ritual context for biblical sacrifice, creating a crisis of continuity for a central covenantal practice.
% FOUNDING_PROBLEM_CORROBORATION: The historical rupture of 70 CE is corroborated by outside historians and archaeologists. However, the claim that this rupture necessitates ongoing preparatory study rather than transformation or abandonment is attested primarily by the rabbinic beneficiaries themselves. Secular Jewish studies scholars and reform theologians attest alternative framings (historical obsolescence or symbolic transformation) from outside the benefiting parties.
narrative_ontology:disappearance_verdict(temple_sacrifice_commitment__hybrid_preparatory, world_rearranges).
narrative_ontology:founding_problem_status(temple_sacrifice_commitment__hybrid_preparatory, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(temple_sacrifice_commitment__hybrid_preparatory, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(temple_sacrifice_commitment__hybrid_preparatory, 'none', 1).
narrative_ontology:epsilon_provenance(temple_sacrifice_commitment__hybrid_preparatory, 0.55, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(temple_sacrifice_commitment__hybrid_preparatory_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(temple_sacrifice_commitment__hybrid_preparatory, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(temple_sacrifice_commitment__hybrid_preparatory_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.55) because the constraint diverts substantial cognitive and material resources to a practice with no current performative outlet, but the transfer is partly reciprocated with communal coherence and maintained identity. Suppression (0.45) reflects moderate coercion: the 'archive or abandon' alternative is socially and theologically collapsed within the Orthodox ecosystem, but not violently suppressed. Theater ratio (0.40) captures the preparatory framingâstudy is functional for a restoration that has been deferred for two millennia, suggesting a significant performative component in maintaining the suspension. Accessibility collapse (0.60) is high because, once inside the halakhic framework, treating the sacrificial corpus as historically obsolete is nearly cognitively inaccessible; resistance (0.30) is moderate because reformist and activist dissent exists but is marginalized. The measurement series run on one shared time grid so every metric is authored at every examined time point.
 *
 * PERSPECTIVAL GAP:
 *   From the institutional seat (study institutions, rabbinic jurists), the constraint is genuine coordinationâpreserving covenantal continuity across a rupture. From the donor and student seats, the same structure extracts resources for a benefit (messianic restoration) whose probability and timeline are unverifiable. The engine computes this divergence from the structural data: identical spatial_scope and similar power levels, but opposite beneficiary/victim roles.
 *
 * DIRECTIONALITY LOGIC:
 *   Torah_study_institutions and rabbinic_jurists are declared beneficiariesâthey collect authority, livelihood, and institutional purposeâso the engine derives low directionality (beneficiary side). Donor_community and students_of_temple_law are declared victimsâthey bear material and cognitive costs without present performative returnâso the engine derives high directionality (target side). The moderate extractiveness is amplified for students and donors and damped for institutions and jurists.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint avoids mandatrophy mislabeling because it names both a genuine coordination function (intergenerational preservation of legal competence) and an asymmetric extraction (resource transfers to sustain non-performable study). A pure coordination reading (rope) would require no victims and no enforcement; a pure extraction reading (snare) would deny the coordination function. The tangled_rope claim captures both. The absence of a sunset clause prevents scaffold classification, and the active institutional maintenance prevents piton classification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'Is the hybrid_preparatory reading structurally distinct from the study_as_exercise and performance_only readings, or does the institutional self-interest of study institutions collapse the distinction?',
    'Comparative institutional analysis: examine whether yeshiva funding and rabbinic authority depend on the ''preparatory'' frame specifically, or whether they would persist unchanged under a study-as-exercise or performance-only frame.',
    'If institutional interests are identical across readings, the hybrid_preparatory reading may be indistinguishable from its siblings in practice, undermining the epsilon-invariance decomposition.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Whether the hybrid reading is structurally distinct from sibling readings.').

omega_variable(
    messianic_temporality,
    'Is the messianic restoration a finite-horizon expectation, or has the preparatory state become indefinitely sustained?',
    'Sociological measurement of eschatological belief intensity across generations; if deferral is accepted as perpetual, the ''preparatory'' framing functions as a permanent extraction mechanism.',
    'If indefinite, the constraint''s lack of a sunset clause transforms a purportedly transitional structure into a steady-state extraction, shifting computed classification toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(messianic_temporality, empirical, 'Whether the messianic horizon is finite or indefinitely deferred.').

omega_variable(
    resource_extraction_ambiguity,
    'Are donated resources and student cognition genuinely extracted, or voluntarily offered as religious fulfillment?',
    'Exit-interview and economic analysis: measure opportunity cost and identity-lock effects for students and donors who leave the system.',
    'If exit reveals low opportunity cost and easy identity reformation, the extraction measure is overstated; if exit is traumatic and costly, suppression and extraction are higher than surface metrics suggest.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(resource_extraction_ambiguity, empirical, 'Whether resource transfers are coerced extraction or voluntary religious investment.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression of ''archive/forget'' alternatives structural (institutional funding, social sanction) or internalized (theological conviction that makes exit unthinkable)?',
    'Post-exit trajectory analysis: observe whether individuals who leave the study framework continue to self-suppress critique of the preparatory model, or whether suppression dissipates upon institutional disaffiliation.',
    'If internalized, effective suppression exceeds the structural measure and the constraint operates partly through cognitive capture, amplifying extractiveness for identity-locked agents.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural versus internalized suppression mechanism.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(temple_sacrifice_commitment__hybrid_preparatory, 0, 80).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tsc_hp_tr_t0, temple_sacrifice_commitment__hybrid_preparatory, theater_ratio, 0, 0.3).
narrative_ontology:measurement(tsc_hp_tr_t20, temple_sacrifice_commitment__hybrid_preparatory, theater_ratio, 20, 0.32).
narrative_ontology:measurement(tsc_hp_tr_t40, temple_sacrifice_commitment__hybrid_preparatory, theater_ratio, 40, 0.36).
narrative_ontology:measurement(tsc_hp_tr_t60, temple_sacrifice_commitment__hybrid_preparatory, theater_ratio, 60, 0.38).
narrative_ontology:measurement(tsc_hp_tr_t80, temple_sacrifice_commitment__hybrid_preparatory, theater_ratio, 80, 0.4).

% Extraction over time
narrative_ontology:measurement(tsc_hp_be_t0, temple_sacrifice_commitment__hybrid_preparatory, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(tsc_hp_be_t20, temple_sacrifice_commitment__hybrid_preparatory, base_extractiveness, 20, 0.5).
narrative_ontology:measurement(tsc_hp_be_t40, temple_sacrifice_commitment__hybrid_preparatory, base_extractiveness, 40, 0.52).
narrative_ontology:measurement(tsc_hp_be_t60, temple_sacrifice_commitment__hybrid_preparatory, base_extractiveness, 60, 0.54).
narrative_ontology:measurement(tsc_hp_be_t80, temple_sacrifice_commitment__hybrid_preparatory, base_extractiveness, 80, 0.55).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(temple_sacrifice_commitment__hybrid_preparatory, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(temple_sacrifice_commitment__hybrid_preparatory, identity_coordination).
narrative_ontology:affects_constraint(temple_sacrifice_commitment__hybrid_preparatory, temple_sacrifice_commitment__study_as_exercise).
narrative_ontology:affects_constraint(temple_sacrifice_commitment__hybrid_preparatory, temple_sacrifice_commitment__performance_only).
narrative_ontology:affects_constraint(temple_sacrifice_commitment__hybrid_preparatory, temple_sacrifice_commitment__symbolic_transformation).

% DUAL FORMULATION NOTE:
% This constraint is one member of the temple_sacrifice_commitment kernel family, decomposed per the epsilon-invariance principle because each reading (hybrid_preparatory, study_as_exercise, performance_only, symbolic_transformation) carries a distinct beneficiary/victim structure and epsilon profile.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
