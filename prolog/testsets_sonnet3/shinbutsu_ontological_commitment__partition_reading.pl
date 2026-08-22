% ============================================================================
% CONSTRAINT STORY: shinbutsu_ontological_commitment__partition_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_shinbutsu_ontological_commitment__partition_reading, []).

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
    constraint_indexing:constraint_classification/3,
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
 *   constraint_id: shinbutsu_ontological_commitment__partition_reading
 *   human_readable: Shinbutsu Domain Partition: Life-Cycle Shinto / Afterlife Buddhism
 *   domain: religious/historical/ontological
 *
 * SUMMARY:
 *   This story instantiates the partition reading of the shinbutsu-shugo
 *   kernel: Shinto and Buddhism are held to occupy structurally separate
 *   domains of ritual competence — life-cycle affirmation on one side, death
 *   and afterlife on the other — with no requirement that practitioners or
 *   institutions reconcile kami cosmology and Buddhist soteriology into one
 *   metaphysical system. This is functional coexistence, not metaphysical
 *   synthesis (that is the syncretic_reading, a different constraint) and not
 *   institutionally tolerated incoherence (that is the incoherence_reading,
 *   also a different constraint). Under this reading, extraction and
 *   suppression are both low: households retain full ritual autonomy to move
 *   between shrine and temple as occasions demand, and no single actor
 *   collects disproportionately from maintaining the partition. The
 *   near-flat, low measurement series reflects that this reading depicts a
 *   durable low-friction division of labor across thirteen centuries, not a
 *   rising rent-extraction pattern.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(shinbutsu_ontological_commitment__partition_reading, 0.18).
domain_priors:suppression_score(shinbutsu_ontological_commitment__partition_reading, 0.12).
domain_priors:theater_ratio(shinbutsu_ontological_commitment__partition_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(shinbutsu_ontological_commitment__partition_reading, extractiveness, 0.18).
narrative_ontology:constraint_metric(shinbutsu_ontological_commitment__partition_reading, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(shinbutsu_ontological_commitment__partition_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(shinbutsu_ontological_commitment__partition_reading, accessibility_collapse, 0.25).
narrative_ontology:constraint_metric(shinbutsu_ontological_commitment__partition_reading, resistance, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(shinbutsu_ontological_commitment__partition_reading, rope).
narrative_ontology:human_readable(shinbutsu_ontological_commitment__partition_reading, "Shinbutsu Domain Partition: Life-Cycle Shinto / Afterlife Buddhism").
narrative_ontology:topic_domain(shinbutsu_ontological_commitment__partition_reading, "religious/historical/ontological").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(shinbutsu_ontological_commitment__partition_reading, '2aae5e60-fbeb-43a3-8182-2ce862d39389').
narrative_ontology:cs_kernel_codification('2aae5e60-fbeb-43a3-8182-2ce862d39389', distributed).
narrative_ontology:cs_authority_grounding('2aae5e60-fbeb-43a3-8182-2ce862d39389', practice).
narrative_ontology:cs_interpretation_layer_present('2aae5e60-fbeb-43a3-8182-2ce862d39389').
narrative_ontology:cs_reading_relation('2aae5e60-fbeb-43a3-8182-2ce862d39389', shinbutsu_ontological_commitment__syncretic_reading, coexists_with).
narrative_ontology:cs_reading_relation('2aae5e60-fbeb-43a3-8182-2ce862d39389', shinbutsu_ontological_commitment__incoherence_reading, coexists_with).
narrative_ontology:cs_axiom('2aae5e60-fbeb-43a3-8182-2ce862d39389', foundational, ritual_domain_separability_without_metaphysical_resolution).
narrative_ontology:cs_axiom_status(ritual_domain_separability_without_metaphysical_resolution, holdable).
narrative_ontology:cs_axiom_grounding('2aae5e60-fbeb-43a3-8182-2ce862d39389', ritual_domain_separability_without_metaphysical_resolution, conventional).
narrative_ontology:cs_axiom('2aae5e60-fbeb-43a3-8182-2ce862d39389', secondary, practitioner_autonomy_does_not_require_cosmological_unity).
narrative_ontology:cs_axiom_status(practitioner_autonomy_does_not_require_cosmological_unity, holdable).
narrative_ontology:cs_axiom_grounding('2aae5e60-fbeb-43a3-8182-2ce862d39389', practitioner_autonomy_does_not_require_cosmological_unity, instrumental).
narrative_ontology:cs_reference_frame('2aae5e60-fbeb-43a3-8182-2ce862d39389', pre_meiji_dual_institutional_ritual_division).
narrative_ontology:cs_drift_state('2aae5e60-fbeb-43a3-8182-2ce862d39389', meiji_shinbutsu_bunri_edicts, gap(codification_collapse, severe, true)).
narrative_ontology:cs_created_at('2aae5e60-fbeb-43a3-8182-2ce862d39389', '').
narrative_ontology:cs_kernel_id(shinbutsu_ontological_commitment__partition_reading, shinbutsu_ontological_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(shinbutsu_ontological_commitment__partition_reading, shinto_shrine_lineages).
narrative_ontology:constraint_beneficiary(shinbutsu_ontological_commitment__partition_reading, buddhist_temple_institutions).
narrative_ontology:constraint_beneficiary(shinbutsu_ontological_commitment__partition_reading, lay_practitioner_households).
narrative_ontology:constraint_vindicates(shinbutsu_ontological_commitment__partition_reading, domain_separability_of_ritual_function).
narrative_ontology:constraint_vindicates(shinbutsu_ontological_commitment__partition_reading, practitioner_autonomy_without_metaphysical_resolution).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administer birth, coming-of-age, marriage, and harvest rites tied to kami and locality. They retain exclusive ritual authority over life-cycle events without needing to reconcile kami cosmology with Buddhist afterlife doctrine. They lose nothing by leaving the afterlife domain to temples and gain a stable, uncontested jurisdiction.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_commitment__partition_reading, shinto_shrine_lineages, beneficiary,
    institutional, generational, mobile, national).

% Administer funerary rites, ancestor memorialization, and afterlife-oriented practice. They hold exclusive jurisdiction over death and postmortem fate without needing kami cosmology to explain or ground it. Their institutional continuity does not depend on winning an ontological argument against Shinto.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_commitment__partition_reading, buddhist_temple_institutions, beneficiary,
    institutional, generational, mobile, national).

% Participate in both shrine rites for life events and temple rites for death without being asked to hold a single coherent cosmology reconciling kami and buddhas. They move fluidly between the two domains as occasions demand, treating the partition as simply how ritual life is organized rather than as a doctrinal commitment requiring resolution.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_commitment__partition_reading, lay_practitioner_households, beneficiary,
    moderate, biographical, mobile, local).

% Historically produced honji-suijaku theology attempting to unify kami and buddhas under one metaphysical order. Under the partition reading their systematizing project is unnecessary rather than wrong — the domains function adequately without their integrative labor, which marginalizes their theoretical contribution relative to what practice actually requires.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_commitment__partition_reading, doctrinal_systematizers, excluded,
    moderate, biographical, constrained, national).

% Analyze whether shinbutsu-shugo represents genuine metaphysical synthesis, functional domain partition, or institutionally tolerated incoherence. They read temple and shrine records, ritual calendars, and doctrinal tracts to adjudicate among the three readings without themselves being ritual participants.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_commitment__partition_reading, religious_studies_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(shinbutsu_ontological_commitment__partition_reading, diffuse).
narrative_ontology:fixing_cost_class(shinbutsu_ontological_commitment__partition_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Divides ritual labor by life-stage: Shinto institutions handle birth, growth, and worldly continuity; Buddhist institutions handle death and postmortem fate. Households get comprehensive ritual coverage across a full life without either institution needing jurisdiction over the other's domain.
% TRANSFER_FUNCTION: Moves ritual fees, land endowments, and community loyalty to whichever institution presides over the relevant life event — shrines for life-cycle occasions, temples for funerary and memorial occasions — with no systematic transfer between the two institutional types.
% ABSENT_VOICES: Doctrinal systematizers who built honji-suijaku theology to unify the two systems are structurally sidelined by this reading: their integrative metaphysics is treated as surplus theorizing rather than as evidence of a genuinely unified cosmology. State Shinto ideologues who later insisted on institutional separation for nationalist reasons are also absent from this ritual-functional account.
% DISAPPEARANCE_RATIONALE: If the domain partition dissolved, households would still need life-cycle and death rites performed by someone; whether that requires two institutions or could be absorbed by one is disputed. Shrine and temple institutions would likely contest any merger that threatened their separate jurisdictional revenue bases, but the ritual functions themselves are portable across institutional form — hence contested rather than a clean rearrange/unchanged verdict.
% FOUNDING_PROBLEM: Pre-modern Japanese communities needed both life-affirming, locality-bound rites (harvest, birth, community continuity) and death-oriented, salvation-oriented rites (funerals, ancestor veneration) without either indigenous kami practice or imported Buddhist doctrine natively covering both domains.
% FOUNDING_PROBLEM_CORROBORATION: Shrine and temple institutions themselves attest the partition remains functionally necessary. Independent religious studies scholarship is divided: some scholars (outside either institution's interest) corroborate that the domains remain practically separable in ritual calendars and record-keeping through the present, while others argue the partition was a retrospective simplification imposed by Meiji-era shinbutsu bunri (separation edicts) rather than a stable pre-modern reality — meaning the corroborating evidence itself has a contested origin point.
narrative_ontology:disappearance_verdict(shinbutsu_ontological_commitment__partition_reading, contested).
narrative_ontology:founding_problem_status(shinbutsu_ontological_commitment__partition_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(shinbutsu_ontological_commitment__partition_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(shinbutsu_ontological_commitment__partition_reading, 'none', 1).
narrative_ontology:epsilon_provenance(shinbutsu_ontological_commitment__partition_reading, 0.18, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(shinbutsu_ontological_commitment__partition_reading_tests).
:- end_tests(shinbutsu_ontological_commitment__partition_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness stays low throughout (0.10 to 0.18) because the partition, under this reading, genuinely solves a coordination problem — two institutions each competent in a domain the other does not natively cover — rather than manufacturing dependency. Suppression is low (0.12) because nothing coercive is required to maintain the division; it persists because it works, not because alternatives are blocked. Theater ratio rises only slightly (0.08 to 0.15) reflecting modest accretion of ceremonial elaboration over centuries without indicating functional hollowing. accessibility_collapse (0.25) and resistance (0.20) are both moderate-low: alternative organizings of ritual life were conceivable and occasionally attempted (single-institution movements, honji-suijaku synthesis attempts) but did not displace the partition because the partition worked well enough that resistance to it stayed low.
 *
 * PERSPECTIVAL GAP:
 *   Doctrinal systematizers occupy the one seat where this reading produces genuine loss relative to their interests: their integrative theological labor is rendered structurally superfluous by a reading that says the domains never needed unifying in the first place. Religious studies scholars, as analytical observers, see the full contest among readings and can trace how each reading's ε and beneficiary structure differ — this partition reading is the low-extraction, low-suppression member of the family precisely because it denies both the unifying metaphysical claim (syncretic_reading) and the coercive-incoherence claim (incoherence_reading).
 *
 * DIRECTIONALITY LOGIC:
 *   Both shrine lineages and temple institutions are declared beneficiaries because each retains uncontested jurisdiction over its domain without needing to defend a shared cosmology — this is low-cost coordination, not asymmetric extraction. Lay households are also beneficiaries: they receive comprehensive ritual coverage across the life course without being forced into doctrinal choice. No victim group is named under this reading, consistent with its expected structural delta ('no single beneficiary' in the sense that no one profits at another's expense) — the beneficiaries all gain from the SAME low-overhead division of labor, not from extracting from each other.
 *
 * MANDATROPHY ANALYSIS:
 *   The partition reading resists mandatrophy mislabeling in both directions: it is not pure extraction dressed as coordination (there is no active enforcement, no victim group, no suppressed alternative institution being kept out), and it is not falsely claimed as metaphysical necessity (it explicitly denies that ontological integration is required for the arrangement to function). The founding problem — comprehensive ritual coverage without doctrinal reconciliation — remains contested as live/dead because whether it is 'solved' depends on whether one thinks doctrinal reconciliation was ever the actual goal, which is exactly the question the sibling readings dispute.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    partition_vs_syncretic_evidentiary_boundary,
    'Does the historical record of honji-suijaku doctrine represent genuine felt need for ontological integration (favoring the syncretic reading) or post-hoc theological elaboration layered onto an already-functioning practical partition (favoring this partition reading)?',
    'Comparative analysis of ritual practice records versus doctrinal tract production: if lay ritual behavior shows stable domain-partition patterns predating and unaffected by honji-suijaku theological development, that supports partition as the operative reality with syncretic theology as elite superstructure.',
    'If the syncretic reading''s metaphysics were operative for ordinary practitioners rather than confined to elite theological discourse, this partition reading understates the degree of doctrinal integration and misattributes autonomy to practitioners who in fact operated within an internalized unified cosmology.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(partition_vs_syncretic_evidentiary_boundary, conceptual, 'Whether honji-suijaku doctrine reflects lived integration or elite theological overlay on a practical partition.').

omega_variable(
    meiji_retrospective_construction_of_partition,
    'Is the clean life-cycle/afterlife partition an accurate description of pre-Meiji practice, or is it substantially a retrospective simplification produced by the Meiji shinbutsu bunri (forced separation) edicts of 1868, which this story''s own interval end-point sits directly against?',
    'Examine pre-1868 temple-shrine joint complexes (jingu-ji) and mixed ritual calendars for evidence of domain-crossing (e.g., shrines involved in death ritual, temples involved in life-cycle events) that would complicate a clean partition claim for any period before the forced separation.',
    'If jingu-ji complexes routinely crossed the claimed domain boundary before 1868, the partition reading''s ''low doctrinal integration, clean domain separation'' delta is itself partly an artifact of the state-imposed separation that ended this constraint''s interval — meaning the partition reading may be more accurate for the Meiji aftermath than for the twelve preceding centuries it is authored to cover.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(meiji_retrospective_construction_of_partition, empirical, 'Whether the partition was a stable pre-modern reality or a retrospective effect of Meiji-era state separation policy.').

omega_variable(
    no_single_beneficiary_versus_dual_institutional_capture,
    'Does the absence of a single beneficiary genuinely indicate low-extraction coordination, or does it mask two parallel, independently extractive institutional monopolies (shrine and temple each locally extractive within its own domain) that simply do not compete with each other?',
    'Examine whether shrine and temple institutions individually extracted disproportionate value from captive local populations within their respective domains, independent of the cross-domain partition question.',
    'If either institution behaved extractively within its own domain (e.g., compulsory temple registration systems (terauke) used for population control and revenue extraction under the Tokugawa shogunate), the partition reading''s low ε may correctly describe the CROSS-domain relationship while missing WITHIN-domain extraction that a fuller accounting would need a separate constraint to capture.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(no_single_beneficiary_versus_dual_institutional_capture, empirical, 'Whether low cross-domain extraction coexists with high within-domain extraction not captured by this reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(shinbutsu_ontological_commitment__partition_reading, 700, 1868).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(shin_tr_t700, shinbutsu_ontological_commitment__partition_reading, theater_ratio, 700, 0.08).
narrative_ontology:measurement(shin_tr_t900, shinbutsu_ontological_commitment__partition_reading, theater_ratio, 900, 0.1).
narrative_ontology:measurement(shin_tr_t1100, shinbutsu_ontological_commitment__partition_reading, theater_ratio, 1100, 0.11).
narrative_ontology:measurement(shin_tr_t1300, shinbutsu_ontological_commitment__partition_reading, theater_ratio, 1300, 0.12).
narrative_ontology:measurement(shin_tr_t1500, shinbutsu_ontological_commitment__partition_reading, theater_ratio, 1500, 0.13).
narrative_ontology:measurement(shin_tr_t1700, shinbutsu_ontological_commitment__partition_reading, theater_ratio, 1700, 0.14).
narrative_ontology:measurement(shin_tr_t1868, shinbutsu_ontological_commitment__partition_reading, theater_ratio, 1868, 0.15).

% Extraction over time
narrative_ontology:measurement(shin_be_t700, shinbutsu_ontological_commitment__partition_reading, base_extractiveness, 700, 0.1).
narrative_ontology:measurement(shin_be_t900, shinbutsu_ontological_commitment__partition_reading, base_extractiveness, 900, 0.12).
narrative_ontology:measurement(shin_be_t1100, shinbutsu_ontological_commitment__partition_reading, base_extractiveness, 1100, 0.14).
narrative_ontology:measurement(shin_be_t1300, shinbutsu_ontological_commitment__partition_reading, base_extractiveness, 1300, 0.15).
narrative_ontology:measurement(shin_be_t1500, shinbutsu_ontological_commitment__partition_reading, base_extractiveness, 1500, 0.17).
narrative_ontology:measurement(shin_be_t1700, shinbutsu_ontological_commitment__partition_reading, base_extractiveness, 1700, 0.18).
narrative_ontology:measurement(shin_be_t1868, shinbutsu_ontological_commitment__partition_reading, base_extractiveness, 1868, 0.18).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(shinbutsu_ontological_commitment__partition_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(shinbutsu_ontological_commitment__partition_reading, shinbutsu_ontological_commitment__syncretic_reading).
narrative_ontology:affects_constraint(shinbutsu_ontological_commitment__partition_reading, shinbutsu_ontological_commitment__incoherence_reading).

% DUAL FORMULATION NOTE:
% This story is one of three siblings decomposing the natural-language label 'shinbutsu-shugo ontological status' per the ε-invariance principle. The syncretic reading claims genuine metaphysical unification (different beneficiaries: doctrinal systematizers, unifying institutions; likely different, non-zero suppression if unification was contested); the incoherence reading claims institutionally tolerated inconsistency (likely higher ε and suppression, since tolerated ambiguity typically persists because some party benefits from unresolved status); this partition reading claims low-integration functional coexistence with no single beneficiary and low ε/suppression. All three describe the same historical practice of shinbutsu-shugo but are structurally distinct constraints, each requiring its own metrics rather than one story averaged across readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
