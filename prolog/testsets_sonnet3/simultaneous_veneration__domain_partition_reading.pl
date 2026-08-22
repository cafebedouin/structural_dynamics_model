% ============================================================================
% CONSTRAINT STORY: simultaneous_veneration__domain_partition_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_simultaneous_veneration__domain_partition_reading, []).

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
 *   constraint_id: simultaneous_veneration__domain_partition_reading
 *   human_readable: Domain-Partition Reading of Kami-Buddha Coexistence (Shinbutsu Shugo)
 *   domain: religious_studies/comparative_religion/japanese_history
 *
 * SUMMARY:
 *   This constraint instantiates the domain-partition reading of the
 *   shinbutsu shugo (kami-buddha combinatory) kernel: the claim that kami and
 *   buddhas were functionally distinct, non-competing specialists — kami
 *   governing this-worldly prosperity and protection, buddhas governing
 *   afterlife salvation — such that simultaneous veneration by the same
 *   household or at combined shrine-temple complexes represented coherent
 *   domain-appropriate specialization rather than confusion,
 *   syncretism-as-fusion, or unresolved contradiction. This reading is
 *   generated as its own clean, ε-invariant constraint per the ε-invariance
 *   principle: the sibling readings (ontological fusion via honji-suijaku,
 *   and pragmatic incoherence) are separate constraints with their own ε
 *   values, not alternative measurements of this one. Under this reading's
 *   own lights the arrangement reads as low-extraction coordination: neither
 *   tradition captures rents from the other via the partition, and no party
 *   is a structural victim of the combinatory practice itself.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(simultaneous_veneration__domain_partition_reading, 0.12).
domain_priors:suppression_score(simultaneous_veneration__domain_partition_reading, 0.08).
domain_priors:theater_ratio(simultaneous_veneration__domain_partition_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(simultaneous_veneration__domain_partition_reading, extractiveness, 0.12).
narrative_ontology:constraint_metric(simultaneous_veneration__domain_partition_reading, suppression_requirement, 0.08).
narrative_ontology:constraint_metric(simultaneous_veneration__domain_partition_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(simultaneous_veneration__domain_partition_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(simultaneous_veneration__domain_partition_reading, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(simultaneous_veneration__domain_partition_reading, rope).
narrative_ontology:human_readable(simultaneous_veneration__domain_partition_reading, "Domain-Partition Reading of Kami-Buddha Coexistence (Shinbutsu Shugo)").
narrative_ontology:topic_domain(simultaneous_veneration__domain_partition_reading, "religious_studies/comparative_religion/japanese_history").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(simultaneous_veneration__domain_partition_reading, '1ac9c3bd-d1dc-4152-afcf-67436d034bae').
narrative_ontology:cs_kernel_codification('1ac9c3bd-d1dc-4152-afcf-67436d034bae', distributed).
narrative_ontology:cs_authority_grounding('1ac9c3bd-d1dc-4152-afcf-67436d034bae', practice).
narrative_ontology:cs_interpretation_layer_present('1ac9c3bd-d1dc-4152-afcf-67436d034bae').
narrative_ontology:cs_reading_relation('1ac9c3bd-d1dc-4152-afcf-67436d034bae', simultaneous_veneration__ontological_fusion_reading, coexists_with).
narrative_ontology:cs_reading_relation('1ac9c3bd-d1dc-4152-afcf-67436d034bae', simultaneous_veneration__pragmatic_incoherence_reading, coexists_with).
narrative_ontology:cs_axiom('1ac9c3bd-d1dc-4152-afcf-67436d034bae', foundational, kami_buddha_functional_distinctness).
narrative_ontology:cs_axiom_status(kami_buddha_functional_distinctness, holdable).
narrative_ontology:cs_axiom_grounding('1ac9c3bd-d1dc-4152-afcf-67436d034bae', kami_buddha_functional_distinctness, conventional).
narrative_ontology:cs_axiom('1ac9c3bd-d1dc-4152-afcf-67436d034bae', secondary, combinatory_veneration_is_specialization_not_confusion).
narrative_ontology:cs_axiom_status(combinatory_veneration_is_specialization_not_confusion, holdable).
narrative_ontology:cs_axiom_grounding('1ac9c3bd-d1dc-4152-afcf-67436d034bae', combinatory_veneration_is_specialization_not_confusion, instrumental).
narrative_ontology:cs_reference_frame('1ac9c3bd-d1dc-4152-afcf-67436d034bae', dual_domain_specialization_practice).
narrative_ontology:cs_drift_state('1ac9c3bd-d1dc-4152-afcf-67436d034bae', meiji_shinbutsu_bunri_era, gap(repudiation_pressure, severe, true)).
narrative_ontology:cs_created_at('1ac9c3bd-d1dc-4152-afcf-67436d034bae', '').
narrative_ontology:cs_kernel_id(simultaneous_veneration__domain_partition_reading, simultaneous_veneration).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(simultaneous_veneration__domain_partition_reading, lay_households).
narrative_ontology:constraint_beneficiary(simultaneous_veneration__domain_partition_reading, shrine_temple_complexes).
narrative_ontology:constraint_beneficiary(simultaneous_veneration__domain_partition_reading, village_ritual_specialists).
narrative_ontology:constraint_vindicates(simultaneous_veneration__domain_partition_reading, functional_domain_specialization_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Petition kami at the local shrine for harvest, fertility, and protection from this-worldly misfortune, and separately arrange Buddhist funeral and memorial rites through the household's affiliated temple for the deceased. Under this reading they are not hedging or double-booking a single spiritual need; they are addressing two genuinely different problems with two genuinely different specialists, the way one would consult a midwife and a undertaker without contradiction.
narrative_ontology:constraint_stakeholder(simultaneous_veneration__domain_partition_reading, lay_households, beneficiary,
    powerless, biographical, mobile, local).

% Jointly administer combined shrine-temple precincts (jingu-ji), scheduling kami festivals for agricultural and community welfare alongside Buddhist memorial and salvific rites, dividing ritual labor along the this-worldly/afterlife line. They set the practical calendar and division of religious labor but do not extract from either party by maintaining the division — the division is the coordination mechanism itself, not a toll on it.
narrative_ontology:constraint_stakeholder(simultaneous_veneration__domain_partition_reading, shrine_temple_complexes, agenda_setter,
    organized, generational, mobile, regional).
narrative_ontology:stakeholder_secondary_role(simultaneous_veneration__domain_partition_reading, shrine_temple_complexes, beneficiary).

% Kami priests (kannushi) and Buddhist clergy each maintain a distinct, non-competing ritual specialization; the domain partition protects both livelihoods by ensuring neither displaces the other's function. Exit is available in principle (either tradition could in theory claim the whole of religious life) but is not exercised because the partition serves both.
narrative_ontology:constraint_stakeholder(simultaneous_veneration__domain_partition_reading, village_ritual_specialists, beneficiary,
    moderate, generational, mobile, local).

% Medieval and early-modern commentators (and modern scholars of Japanese religion) who articulate and defend the functional-differentiation account, distinguishing it from honji-suijaku metaphysical fusion claims and from later charges of incoherence. They analyze the practice rather than participate in it as devotees.
narrative_ontology:constraint_stakeholder(simultaneous_veneration__domain_partition_reading, doctrinal_systematizers, observer,
    institutional, civilizational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Allows a single household or village to address two structurally distinct life-problems — securing this-worldly welfare (harvest, health, fertility, protection) and securing a good afterlife/salvation — without requiring either kami cult or Buddhist institution to claim total jurisdiction over religious life. Each tradition specializes in what it does well; neither is asked to do the other's job.
% TRANSFER_FUNCTION: Minimal transfer in either direction: households give offerings and labor to shrines for this-worldly ends and separately give support (labor, land, funerary fees) to temples for afterlife ends. Under this reading nothing is extracted through the coexistence itself — resources flow to each institution in exchange for its own domain-specific service, not as a toll for permission to combine traditions.
% ABSENT_VOICES: Sectarian purists on either side (kami-exclusivist nativists, Buddhist exclusivist reform sects) who would object that the partition dilutes or subordinates their tradition are not organized voices within this reading's own frame — they belong to the sibling incoherence/fusion readings and are treated here as external critics rather than parties to this arrangement.
% DISAPPEARANCE_RATIONALE: If the domain-partition understanding vanished, households would still need both this-worldly ritual services and afterlife ritual services — the practical arrangement (shrine for one need, temple for another) might persist by inertia even without the doctrinal partition, but the Meiji-era forced separation (shinbutsu bunri) suggests that once the partition is administratively/doctrinally denied, the coordination is vulnerable to state reclassification and asset seizure. Whether 'the world rearranges' depends on whether one asks about lived practice (largely persists) or institutional standing (does not).
% FOUNDING_PROBLEM: Pre-modern Japanese religious life needed both agricultural/this-worldly protective ritual and a coherent afterlife/salvation framework, and neither indigenous kami cult nor imported Buddhism alone supplied both; the partition let each institution do what it already did well without forcing a doctrinal takeover by either.
% FOUNDING_PROBLEM_CORROBORATION: Medieval Buddhist and Shinto institutional records and jingu-ji administrative documents attest to the partition being lived practice by both clergies. Meiji-era state ideologues and some modern historians of religion (outside either shrine or temple's own beneficiary interest) attest the partition was doctrinally under-specified rather than a settled functional theory, supporting the sibling incoherence reading; this reading treats that dispute as a live, unresolved corroboration gap rather than settling it in its own favor.
narrative_ontology:disappearance_verdict(simultaneous_veneration__domain_partition_reading, contested).
narrative_ontology:founding_problem_status(simultaneous_veneration__domain_partition_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(simultaneous_veneration__domain_partition_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(simultaneous_veneration__domain_partition_reading, 'none', 1).
narrative_ontology:epsilon_provenance(simultaneous_veneration__domain_partition_reading, 0.12, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(simultaneous_veneration__domain_partition_reading_tests).
:- end_tests(simultaneous_veneration__domain_partition_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored low (0.12 at 1868) because, under this reading, the partition is not a toll mechanism — each institution collects only for its own domain-specific service. Suppression is low (0.08) because nothing coercive maintains the division; households could in principle patronize only one tradition, and some did. Theater ratio stays low-moderate (0.15) reflecting some ritual elaboration but not functional hollowing. Resistance is low because, under this reading, there was little organized objection to the partition itself during the interval — resistance to combinatory religion as such (Meiji shinbutsu bunri) is a later external shock to the reading's own frame, not evidence generated from within it. Accessibility collapse is moderate (0.35): once households adopted the dual-track pattern it became the default practical arrangement in most regions, though pure kami-only or pure Buddhist-only households remained possible.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat (joint shrine-temple administration) and the beneficiary seats (households, ritual specialists) should compute similarly under this reading — there is little structural asymmetry to diverge on, since the reading's central claim is precisely that no one is extracting from anyone through the partition. This near-symmetry is itself diagnostic: a reading claiming genuine coordination should show flat, low-divergence seat computations; sharp divergence would be evidence the fusion or incoherence readings better describe the underlying structure.
 *
 * DIRECTIONALITY LOGIC:
 *   All named beneficiaries (households, ritual specialists, shrine-temple complexes) sit near the coordination/beneficiary end of directionality because the reading declares no victim group — the domain partition, on its own account, extracts from no one. No victims are declared for this constraint; this is a structural fact of the reading, not an oversight, and distinguishes it sharply from the pragmatic-incoherence sibling, which would locate confused or exploited practitioners as an implicit victim class.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (need for both this-worldly ritual efficacy and afterlife salvation doctrine) plausibly remains live for many contemporary Japanese households who still visit shrines for New Year and weddings while using Buddhist temples for funerals — so under this reading the arrangement is not mandatrophic; the function it names persists in a debased institutional form after Meiji's forced administrative separation, but the underlying coordination logic (two domains, two specialists) still describes lived practice reasonably well.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    domain_partition_vs_post_hoc_rationalization,
    'Was the this-worldly/afterlife functional division a genuine organizing principle that practitioners and clergy consciously held and used to structure ritual practice, or is it a coherence imposed retrospectively by scholars (and by this very reading) onto what was actually looser, more improvisational combinatory practice?',
    'Close reading of medieval doctrinal texts (e.g., temple-shrine administrative charters, ritual calendars, sermon literature) for explicit statements of functional differentiation versus texts that treat kami and buddhas as interchangeable or as simply co-present without articulated division of labor.',
    'If the division is substantially a modern scholarly systematization, this reading''s claim to describe the historical arrangement accurately weakens relative to the pragmatic_incoherence_reading, which would better fit an actually looser, unsystematized practice.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(domain_partition_vs_post_hoc_rationalization, empirical, 'Whether the functional domain partition was a lived emic category or a scholarly etic reconstruction.').

omega_variable(
    regional_and_class_variation_in_partition_coherence,
    'Did the domain-partition understanding hold uniformly across regions and social classes, or did some communities (e.g., elite court religion versus village folk practice) operate with a more fused or more incoherent understanding, meaning the ''reading'' that best fits Japan varies by locale and class rather than being singular?',
    'Comparative regional ethnographic and historical study of shrine-temple complex records across multiple provinces and social strata during the interval.',
    'If coherence varied substantially by region/class, no single reading (domain-partition, fusion, or incoherence) is uniformly true of ''shinbutsu shugo'' as a national phenomenon, and this constraint''s scope claim (implicitly national) should be narrowed or itself decomposed further.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(regional_and_class_variation_in_partition_coherence, conceptual, 'Whether a single reading can validly describe a geographically and socially heterogeneous practice.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(simultaneous_veneration__domain_partition_reading, 0, 1868).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(simu_tr_t0, simultaneous_veneration__domain_partition_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(simu_tr_t300, simultaneous_veneration__domain_partition_reading, theater_ratio, 300, 0.1).
narrative_ontology:measurement(simu_tr_t600, simultaneous_veneration__domain_partition_reading, theater_ratio, 600, 0.12).
narrative_ontology:measurement(simu_tr_t900, simultaneous_veneration__domain_partition_reading, theater_ratio, 900, 0.13).
narrative_ontology:measurement(simu_tr_t1200, simultaneous_veneration__domain_partition_reading, theater_ratio, 1200, 0.14).
narrative_ontology:measurement(simu_tr_t1500, simultaneous_veneration__domain_partition_reading, theater_ratio, 1500, 0.15).
narrative_ontology:measurement(simu_tr_t1868, simultaneous_veneration__domain_partition_reading, theater_ratio, 1868, 0.15).

% Extraction over time
narrative_ontology:measurement(simu_be_t0, simultaneous_veneration__domain_partition_reading, base_extractiveness, 0, 0.08).
narrative_ontology:measurement(simu_be_t300, simultaneous_veneration__domain_partition_reading, base_extractiveness, 300, 0.09).
narrative_ontology:measurement(simu_be_t600, simultaneous_veneration__domain_partition_reading, base_extractiveness, 600, 0.1).
narrative_ontology:measurement(simu_be_t900, simultaneous_veneration__domain_partition_reading, base_extractiveness, 900, 0.11).
narrative_ontology:measurement(simu_be_t1200, simultaneous_veneration__domain_partition_reading, base_extractiveness, 1200, 0.11).
narrative_ontology:measurement(simu_be_t1500, simultaneous_veneration__domain_partition_reading, base_extractiveness, 1500, 0.12).
narrative_ontology:measurement(simu_be_t1868, simultaneous_veneration__domain_partition_reading, base_extractiveness, 1868, 0.12).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(simultaneous_veneration__domain_partition_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(simultaneous_veneration__domain_partition_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(simultaneous_veneration__domain_partition_reading, 0.1).
narrative_ontology:affects_constraint(simultaneous_veneration__domain_partition_reading, ontological_fusion_reading).
narrative_ontology:affects_constraint(simultaneous_veneration__domain_partition_reading, pragmatic_incoherence_reading).
narrative_ontology:affects_constraint(simultaneous_veneration__domain_partition_reading, meiji_shinbutsu_bunri_separation_edict).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the simultaneous_veneration kernel, each authored as an independent constraint with its own ε per the ε-invariance principle. domain_partition_reading (this file) authors low extraction and a rope-coordination structure premised on genuine functional differentiation. ontological_fusion_reading would author a distinct ε for the honji-suijaku metaphysical-identity claim, likely also low-extraction but with a different beneficiary structure (favoring syncretic institutional authority claims). pragmatic_incoherence_reading would author a higher extraction/theater profile, treating the coexistence as sustained by absence of enforcement rather than genuine coordination, with practitioners as an implicit diffuse victim class of unresolved doctrinal confusion. All three link to meiji_shinbutsu_bunri_separation_edict as the downstream event that forcibly resolved (or attempted to resolve) the underlying kernel contest by state fiat.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
