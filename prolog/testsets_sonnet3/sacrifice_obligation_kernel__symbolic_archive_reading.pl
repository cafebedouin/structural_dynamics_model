% ============================================================================
% CONSTRAINT STORY: sacrifice_obligation_kernel__symbolic_archive_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   domain: religious_law/halakhic_authority/commitment_system
 *
 * SUMMARY:
 *   This story instantiates the symbolic_archive_reading of the
 *   sacrifice_obligation_kernel: the position, held within parts of the
 *   modern (particularly liberal, secular-cultural, and academically
 *   oriented) Jewish study community, that the corpus of sacrifice law
 *   (korbanot) — its texts, structures, and interpretive tradition —
 *   functions as a cultural-historical archive rather than as an active or
 *   dormant halakhic obligation. On this reading, engaging with the material
 *   (through study, commemoration, or liturgical recitation of the relevant
 *   texts) is valuable because it preserves collective memory, textual
 *   literacy, and continuity with the pre-Temple tradition, not because doing
 *   so discharges, exercises, or maintains readiness toward any binding
 *   commandment. Crucially, this reading denies that there is a live
 *   obligation in the first place — there is nothing to violate by non-study,
 *   nothing to be punished for, and no claim of readiness being maintained
 *   for a future performance. This is structurally distinct from its three
 *   sibling readings, which either treat study as active fulfillment
 *   (study_as_exercise_reading), treat study as preparatory to a
 *   still-binding future performance (performance_only_reading), or treat the
 *   obligation as divinely suspended but intact, with study maintaining
 *   operational readiness for restoration (messianic_suspension_reading).
 *   Those are separate constraints, authored separately, linked here via
 *   network.affects_constraints per the ε-invariance principle — this story's
 *   ε is stable and low precisely because, on this reading's own terms, there
 *   is no obligation-shaped structure generating extraction.
 *
 * KEY AGENTS:
 *   - Study community adherents to the archival reading — engage voluntarily, bear no halakhic risk, benefit through identity/continuity
 *   - Jewish collective memory / diaspora identity continuity — the diffuse, non-actor beneficiary of preserved textual tradition
 *   - Rabbinic authorities holding sibling readings (messianic_suspension, study_as_exercise, performance_only) — structurally excluded from this reading's own framework, since they would dispute its core premise that no obligation exists
 *   - Academic and cultural historians — observers/secondary beneficiaries who use the archival framing as an entry point into the textual corpus without adjudicating halakhic status
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sacrifice_obligation_kernel__symbolic_archive_reading, 0.02).
domain_priors:suppression_score(sacrifice_obligation_kernel__symbolic_archive_reading, 0.03).
domain_priors:theater_ratio(sacrifice_obligation_kernel__symbolic_archive_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__symbolic_archive_reading, extractiveness, 0.02).
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__symbolic_archive_reading, suppression_requirement, 0.03).
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__symbolic_archive_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__symbolic_archive_reading, accessibility_collapse, 0.05).
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__symbolic_archive_reading, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sacrifice_obligation_kernel__symbolic_archive_reading, rope).
narrative_ontology:human_readable(sacrifice_obligation_kernel__symbolic_archive_reading, "Sacrifice Law as Cultural-Historical Archive (Symbolic Archive Reading)").
narrative_ontology:topic_domain(sacrifice_obligation_kernel__symbolic_archive_reading, "religious_law/halakhic_authority/commitment_system").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sacrifice_obligation_kernel__symbolic_archive_reading, '584dbf6f-d8ef-4fa4-981a-b11de76a569c').
narrative_ontology:cs_kernel_codification('584dbf6f-d8ef-4fa4-981a-b11de76a569c', distributed).
narrative_ontology:cs_authority_grounding('584dbf6f-d8ef-4fa4-981a-b11de76a569c', practice).
narrative_ontology:cs_interpretation_layer_present('584dbf6f-d8ef-4fa4-981a-b11de76a569c').
narrative_ontology:cs_reading_relation('584dbf6f-d8ef-4fa4-981a-b11de76a569c', sacrifice_obligation_kernel__messianic_suspension_reading, forecloses).
narrative_ontology:cs_reading_relation('584dbf6f-d8ef-4fa4-981a-b11de76a569c', sacrifice_obligation_kernel__performance_only_reading, forecloses).
narrative_ontology:cs_reading_relation('584dbf6f-d8ef-4fa4-981a-b11de76a569c', sacrifice_obligation_kernel__study_as_exercise_reading, coexists_with).
narrative_ontology:cs_axiom('584dbf6f-d8ef-4fa4-981a-b11de76a569c', foundational, no_binding_obligation_survives_temple_destruction).
narrative_ontology:cs_axiom_status(no_binding_obligation_survives_temple_destruction, holdable).
narrative_ontology:cs_axiom_grounding('584dbf6f-d8ef-4fa4-981a-b11de76a569c', no_binding_obligation_survives_temple_destruction, conventional).
narrative_ontology:cs_axiom('584dbf6f-d8ef-4fa4-981a-b11de76a569c', foundational, textual_engagement_value_is_memorial_not_juridical).
narrative_ontology:cs_axiom_status(textual_engagement_value_is_memorial_not_juridical, holdable).
narrative_ontology:cs_axiom_grounding('584dbf6f-d8ef-4fa4-981a-b11de76a569c', textual_engagement_value_is_memorial_not_juridical, instrumental).
narrative_ontology:cs_reference_frame('584dbf6f-d8ef-4fa4-981a-b11de76a569c', post_temple_textual_preservation_practice).
narrative_ontology:cs_drift_state('584dbf6f-d8ef-4fa4-981a-b11de76a569c', contemporary_liberal_and_secular_jewish_study, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('584dbf6f-d8ef-4fa4-981a-b11de76a569c', '').
narrative_ontology:cs_kernel_id(sacrifice_obligation_kernel__symbolic_archive_reading, sacrifice_obligation_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sacrifice_obligation_kernel__symbolic_archive_reading, jewish_collective_memory).
narrative_ontology:constraint_beneficiary(sacrifice_obligation_kernel__symbolic_archive_reading, diaspora_identity_continuity).
narrative_ontology:constraint_beneficiary(sacrifice_obligation_kernel__symbolic_archive_reading, secular_and_liberal_jewish_communities).
narrative_ontology:constraint_beneficiary(sacrifice_obligation_kernel__symbolic_archive_reading, academic_and_cultural_historians).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(sacrifice_obligation_kernel__symbolic_archive_reading, study_community_adherents).
narrative_ontology:constraint_vindicates(sacrifice_obligation_kernel__symbolic_archive_reading, textual_continuity_without_coercive_obligation).
narrative_ontology:constraint_vindicates(sacrifice_obligation_kernel__symbolic_archive_reading, identity_preservation_through_voluntary_study).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Engage with sacrifice law texts as part of ongoing Torah study or cultural education, treating the material as historically and culturally significant rather than as a live legal demand on their conduct. They can begin, pause, or stop this study at any time without believing they have incurred any halakhic liability, and often frame it as continuous with broader historical and literary study rather than religious obligation-discharge.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__symbolic_archive_reading, study_community_adherents, beneficiary,
    moderate, biographical, mobile, global).

% A non-actor collective good: the continuity of textual tradition, communal identity, and historical self-understanding that is sustained when the sacrifice-law corpus continues to be read, taught, and transmitted, independent of whether any individual reader treats the material as legally binding.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__symbolic_archive_reading, jewish_collective_memory, beneficiary,
    analytical, civilizational, analytical, global).
narrative_ontology:stakeholder_non_agent(sacrifice_obligation_kernel__symbolic_archive_reading, jewish_collective_memory).

% Rabbinic figures and institutions who hold the messianic_suspension or performance_only readings would dispute this reading's core premise that no obligation exists; they are not part of the archival reading's own community and are structurally excluded from its framework, though they remain active voices within the wider kernel contest represented by the sibling constraints.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__symbolic_archive_reading, rabbinic_authorities_of_sibling_readings, excluded,
    institutional, generational, identity_locked, global).

% Study the same texts as historical and literary artifacts, using the archival framing as a neutral entry point that lets them engage the material without adjudicating its halakhic status, and often cite the endurance of study practice as evidence of the corpus's continuing cultural significance.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__symbolic_archive_reading, academic_and_cultural_historians, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(sacrifice_obligation_kernel__symbolic_archive_reading, diffuse).
narrative_ontology:fixing_cost_class(sacrifice_obligation_kernel__symbolic_archive_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared, low-cost mechanism for transmitting a body of textual tradition and communal memory across generations, without requiring participants to accept a binding legal claim as the price of engagement — this widens who can participate (including secular and liberal Jews who would not accept the obligation-claim readings) while still sustaining continuity of the underlying corpus.
% TRANSFER_FUNCTION: No material transfer occurs between parties under this reading; what is 'moved' is intergenerational transmission of textual literacy and identity-continuity from past to present community, not a flow of value, obligation-discharge, or cost from one party to another.
% ABSENT_VOICES: Rabbinic authorities holding the messianic_suspension_reading or performance_only_reading would object that this reading understates the theological stakes of the material and risks eroding belief in eventual Temple restoration; they are not part of the archival study community's internal conversation and their objection is represented structurally by the sibling constraints rather than as suppressed dissent within this one.
% DISAPPEARANCE_RATIONALE: If the archival study practice ceased overnight, no binding obligation would be violated, no institution's authority would be undermined, and no material transfer would stop, because none exists under this reading's own terms. Some loss of cultural transmission and continuity would occur, but no party's arrangements (income, status, legal standing) depend on this reading being practiced, distinguishing it sharply from constraints where disappearance would force structural rearrangement.
% FOUNDING_PROBLEM: After the destruction of the Second Temple, sacrifice could no longer be physically performed; the archival reading emerged (or was retrospectively adopted by parts of the community) to give continued textual engagement with the sacrifice-law corpus a meaningful, non-obligation-dependent rationale — preserving memory and identity rather than claiming ongoing legal force.
% FOUNDING_PROBLEM_CORROBORATION: Academic historians of Jewish liturgy and identity (a source outside the study community that benefits from this reading) corroborate that cultural-memory transmission through textual study is a documented and ongoing function independent of halakhic framing; this corroboration comes from outside the beneficiary community itself, satisfying the R5 requirement for non-self-asserted attestation.
narrative_ontology:disappearance_verdict(sacrifice_obligation_kernel__symbolic_archive_reading, world_unchanged).
narrative_ontology:founding_problem_status(sacrifice_obligation_kernel__symbolic_archive_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(sacrifice_obligation_kernel__symbolic_archive_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(sacrifice_obligation_kernel__symbolic_archive_reading, 'none', 1).
narrative_ontology:epsilon_provenance(sacrifice_obligation_kernel__symbolic_archive_reading, 0.02, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness is authored near zero (0.02-0.05 across the interval) because the defining structural feature of this reading is the ABSENCE of a binding obligation: there is no mitzvah being under- or over-performed, no punishment structure, no debt owed. Suppression is similarly near-zero because study is voluntary and non-study carries no halakhic consequence under this reading's own premises. Theater ratio is modest (0.10-0.15) reflecting that some communal/liturgical performance of study-as-commemoration persists (e.g., recitation of korbanot passages in daily liturgy) even under an archival self-understanding, but this is honest ritual practice, not enforcement theater standing in for a decayed coercive function. Accessibility collapse and resistance are both authored low: alternatives to studying the material remain fully open (one can simply not engage with korbanot texts without consequence), and there is little active resistance to the archival framing from within the communities that hold it, precisely because it makes no demand.
 *
 * DIRECTIONALITY LOGIC:
 *   There are no victims under this reading because there is no extraction to bear — the beneficiary/victim asymmetry that the Tangled Rope and Snare gates require simply does not exist here. Jewish collective memory and identity-continuity are named as beneficiaries in the diffuse, non-actor sense (a value preserved, not a party collecting rents), consistent with treating cultural continuity as a genuine collective good rather than a captured resource. Study community adherents are direct human beneficiaries who participate freely; they are directionally close to full-beneficiary status (low d) because engagement is voluntary, low-cost, and reversible at will — there is no trapped or identity-locked exit position generated by this reading, unlike readings where non-performance carries theological stakes.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading is the clearest case in the kernel family of a constraint whose founding problem (preserving continuity with Temple-era practice and textual tradition after the Temple's destruction) remains genuinely live, while explicitly declining to claim that the ORIGINAL obligation itself remains live. This prevents a specific mislabeling: without the archive reading available as a distinct structural position, an observer might either (a) collapse this practice into 'quietly making a weak halakhic claim' and then flag it as extractive for producing guilt without formal backing, or (b) dismiss the practice's genuine value because it makes no binding claim. By authoring the archive reading as its own ε-invariant constraint with a stable near-zero ε, the classification correctly identifies non-coercive cultural preservation as structurally different from dormant-obligation or active-obligation readings, even though all four readings share surface vocabulary ('study of sacrifice law').
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    archive_vs_dormant_obligation,
    'Is sacrifice law genuinely a closed cultural-historical archive, or is it a dormant binding obligation that this reading merely treats as archival for interpretive convenience?',
    'Comparative textual analysis of how the reading''s own community treats related dormant mitzvot (e.g., jubilee, kingship laws) — consistent archival treatment across all suspended-performance mitzvot would support the archive reading; selective archival treatment applied only where performance is currently impossible would suggest motivated reasoning.',
    'If the archive framing is selectively applied only to sacrifice law (versus other dormant obligations treated as live-but-suspended), this reading would be better understood as a rationalization that lowers psychological stakes rather than a structurally distinct halakhic position.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(archive_vs_dormant_obligation, conceptual, 'Whether the archive framing is a principled hermeneutic or a convenience rationalization.').

omega_variable(
    kernel_reading_committer_structure,
    'Which of the four readings of the sacrifice_obligation_kernel does a given study community actually hold, and can communities hold more than one simultaneously (e.g., liturgically performing messianic_suspension language while privately treating the material as archival)?',
    'Ethnographic/textual survey of liturgical practice versus stated theological commitment across denominations; look for internal inconsistency between prayer-book language (which often assumes messianic restoration) and study-hall framing (which may be archival).',
    'If communities hold multiple readings simultaneously without resolving the tension, the kernel is genuinely distributed/ambiguous rather than cleanly partitioned into four discrete camps, and this story''s ε=0.02 applies only to the subset of practice that is purely archival.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_committer_structure, conceptual, 'Whether the four kernel readings are held as discrete positions or blended in practice.').

omega_variable(
    beneficiary_coercion_boundary,
    'Does the identity-preservation benefit this reading produces for the Jewish collective ever curdle into informal social pressure to study or affirm the material, even without halakhic backing?',
    'Survey communities that adopt the archive framing for evidence of social (non-halakhic) pressure to participate in study — e.g., communal expectation, exclusion of non-participants from certain cultural or educational settings.',
    'If informal social suppression exists despite the absence of halakhic obligation, the true suppression score is higher than the near-zero value authored here, and the constraint would need a companion social-pressure story rather than being folded into this one (ε-invariance).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(beneficiary_coercion_boundary, empirical, 'Whether non-halakhic identity benefit ever produces informal coercive pressure.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sacrifice_obligation_kernel__symbolic_archive_reading, 0, 200).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sacr_tr_t0, sacrifice_obligation_kernel__symbolic_archive_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(sacr_tr_t40, sacrifice_obligation_kernel__symbolic_archive_reading, theater_ratio, 40, 0.14).
narrative_ontology:measurement(sacr_tr_t80, sacrifice_obligation_kernel__symbolic_archive_reading, theater_ratio, 80, 0.12).
narrative_ontology:measurement(sacr_tr_t120, sacrifice_obligation_kernel__symbolic_archive_reading, theater_ratio, 120, 0.11).
narrative_ontology:measurement(sacr_tr_t160, sacrifice_obligation_kernel__symbolic_archive_reading, theater_ratio, 160, 0.1).
narrative_ontology:measurement(sacr_tr_t200, sacrifice_obligation_kernel__symbolic_archive_reading, theater_ratio, 200, 0.1).

% Extraction over time
narrative_ontology:measurement(sacr_be_t0, sacrifice_obligation_kernel__symbolic_archive_reading, base_extractiveness, 0, 0.05).
narrative_ontology:measurement(sacr_be_t40, sacrifice_obligation_kernel__symbolic_archive_reading, base_extractiveness, 40, 0.04).
narrative_ontology:measurement(sacr_be_t80, sacrifice_obligation_kernel__symbolic_archive_reading, base_extractiveness, 80, 0.03).
narrative_ontology:measurement(sacr_be_t120, sacrifice_obligation_kernel__symbolic_archive_reading, base_extractiveness, 120, 0.03).
narrative_ontology:measurement(sacr_be_t160, sacrifice_obligation_kernel__symbolic_archive_reading, base_extractiveness, 160, 0.02).
narrative_ontology:measurement(sacr_be_t200, sacrifice_obligation_kernel__symbolic_archive_reading, base_extractiveness, 200, 0.02).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(sacrifice_obligation_kernel__symbolic_archive_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sacrifice_obligation_kernel__symbolic_archive_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(sacrifice_obligation_kernel__symbolic_archive_reading, 0.08).
narrative_ontology:affects_constraint(sacrifice_obligation_kernel__symbolic_archive_reading, sacrifice_obligation_kernel__study_as_exercise_reading).
narrative_ontology:affects_constraint(sacrifice_obligation_kernel__symbolic_archive_reading, sacrifice_obligation_kernel__performance_only_reading).
narrative_ontology:affects_constraint(sacrifice_obligation_kernel__symbolic_archive_reading, sacrifice_obligation_kernel__messianic_suspension_reading).

% DUAL FORMULATION NOTE:
% This story is one of four decomposed readings of the sacrifice_obligation_kernel, split per the epsilon-invariance principle because the natural-language label 'study of sacrifice law' covers structurally distinct claims with widely different epsilon values. This reading (symbolic_archive_reading) authors the lowest epsilon in the family (near 0.02-0.05) because it denies any binding obligation exists. study_as_exercise_reading and performance_only_reading both retain an obligation-shaped structure (differing on whether study itself discharges it), and messianic_suspension_reading retains a fully intact but divinely paused obligation. All four are linked bidirectionally via affects_constraints; each carries its own claimed_type, stakeholders, and omega set.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
