% ============================================================================
% CONSTRAINT STORY: shinbutsu_ontological_commitment__partition_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:suppression_profile/2,
    narrative_ontology:measurement_basis/2,
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
 *   constraint_id: shinbutsu_ontological_commitment__partition_reading
 *   human_readable: Shinto-Buddhist Domain Partition (Life-Cycle vs. Afterlife)
 *   domain: religious_studies/japanese_history/ontology_of_practice
 *
 * SUMMARY:
 *   This story instantiates the partition reading of the shinbutsu-shugo
 *   kernel: from roughly the Nara period through the Meiji-era shinbutsu
 *   bunri edicts, Shinto shrines and Buddhist temples divided ritual labor
 *   along a life-cycle (Shinto) versus afterlife (Buddhist) axis without
 *   requiring practitioners, clergy, or households to hold a unified
 *   metaphysical account of how kami and buddhas relate. This is one of three
 *   competing readings of the same historical kernel — see kernel_context.
 *   Under this reading, extraction and suppression are both low: no
 *   institution collects rents by enforcing the boundary, and lay
 *   practitioners cross freely between systems. The claimed type is rope:
 *   genuine coordination (avoiding jurisdictional conflict, dividing labor
 *   efficiently) with negligible extraction and no single party running the
 *   arrangement for its own benefit.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(shinbutsu_ontological_commitment__partition_reading, 0.18).
domain_priors:suppression_score(shinbutsu_ontological_commitment__partition_reading, 0.12).
domain_priors:theater_ratio(shinbutsu_ontological_commitment__partition_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(shinbutsu_ontological_commitment__partition_reading, extractiveness, 0.18).
narrative_ontology:constraint_metric(shinbutsu_ontological_commitment__partition_reading, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(shinbutsu_ontological_commitment__partition_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(shinbutsu_ontological_commitment__partition_reading, accessibility_collapse, 0.25).
narrative_ontology:constraint_metric(shinbutsu_ontological_commitment__partition_reading, resistance, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(shinbutsu_ontological_commitment__partition_reading, rope).
narrative_ontology:human_readable(shinbutsu_ontological_commitment__partition_reading, "Shinto-Buddhist Domain Partition (Life-Cycle vs. Afterlife)").
narrative_ontology:topic_domain(shinbutsu_ontological_commitment__partition_reading, "religious_studies/japanese_history/ontology_of_practice").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(shinbutsu_ontological_commitment__partition_reading, '6f5e7394-ab7e-4f34-b053-36b88c99c5b4').
narrative_ontology:cs_kernel_codification('6f5e7394-ab7e-4f34-b053-36b88c99c5b4', distributed).
narrative_ontology:cs_authority_grounding('6f5e7394-ab7e-4f34-b053-36b88c99c5b4', distributed).
narrative_ontology:cs_reading_relation('6f5e7394-ab7e-4f34-b053-36b88c99c5b4', shinbutsu_ontological_commitment__syncretic_reading, coexists_with).
narrative_ontology:cs_reading_relation('6f5e7394-ab7e-4f34-b053-36b88c99c5b4', shinbutsu_ontological_commitment__incoherence_reading, influences).
narrative_ontology:cs_axiom('6f5e7394-ab7e-4f34-b053-36b88c99c5b4', foundational, functional_domains_require_no_metaphysical_unification).
narrative_ontology:cs_axiom_status(functional_domains_require_no_metaphysical_unification, holdable).
narrative_ontology:cs_axiom_grounding('6f5e7394-ab7e-4f34-b053-36b88c99c5b4', functional_domains_require_no_metaphysical_unification, conventional).
narrative_ontology:cs_axiom('6f5e7394-ab7e-4f34-b053-36b88c99c5b4', secondary, practitioner_ritual_choice_is_autonomous_not_doctrinally_governed).
narrative_ontology:cs_axiom_status(practitioner_ritual_choice_is_autonomous_not_doctrinally_governed, holdable).
narrative_ontology:cs_axiom_grounding('6f5e7394-ab7e-4f34-b053-36b88c99c5b4', practitioner_ritual_choice_is_autonomous_not_doctrinally_governed, empirically_contingent).
narrative_ontology:cs_reference_frame('6f5e7394-ab7e-4f34-b053-36b88c99c5b4', dual_institutional_jurisdiction).
narrative_ontology:cs_drift_state('6f5e7394-ab7e-4f34-b053-36b88c99c5b4', meiji_shinbutsu_bunri_edicts, gap(repudiation_pressure, severe, true)).
narrative_ontology:cs_created_at('6f5e7394-ab7e-4f34-b053-36b88c99c5b4', '').
narrative_ontology:cs_kernel_id(shinbutsu_ontological_commitment__partition_reading, shinbutsu_ontological_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(shinbutsu_ontological_commitment__partition_reading, shinto_shrine_lineages).
narrative_ontology:constraint_beneficiary(shinbutsu_ontological_commitment__partition_reading, buddhist_temple_lineages).
narrative_ontology:constraint_beneficiary(shinbutsu_ontological_commitment__partition_reading, lay_households).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(shinbutsu_ontological_commitment__partition_reading, lay_households).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administer kami rites for birth, coming-of-age, marriage, and agricultural/community festivals. Under the partition reading they retain undisputed jurisdiction over this-worldly life-cycle events and communal purity, without needing to explain kami in Buddhist cosmological terms. They lose nothing by not claiming afterlife authority; their institutional position is stable within their domain.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_commitment__partition_reading, shinto_shrine_lineages, beneficiary,
    organized, generational, constrained, national).

% Administer funerary rites, ancestral memorialization, and afterlife soteriology. Under the partition reading they hold exclusive functional authority over death and what follows it, while ceding birth and this-worldly ritual to shrines. Temples derive stable income and social role from funerary monopoly without contesting shrine jurisdiction.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_commitment__partition_reading, buddhist_temple_lineages, beneficiary,
    organized, generational, constrained, national).

% Move fluidly between shrine and temple according to occasion — shrine visits for birth and New Year, temple rites for funerals and memorial services — without needing either institution to resolve what a kami or a buddha ultimately is. They pay fees and offerings to both institutions across a lifetime but face no doctrinal test, no requirement to reconcile the two systems, and no penalty for using both.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_commitment__partition_reading, lay_households, beneficiary,
    moderate, biographical, mobile, local).
narrative_ontology:stakeholder_secondary_role(shinbutsu_ontological_commitment__partition_reading, lay_households, payer).

% Theologians and scholar-monks who sought (in various periods) to construct a unified cosmology linking kami and buddhas (e.g., honji-suijaku formulations) find that everyday practice does not require or reward their systematizing work. Their integrative project is neither refuted nor adopted at the practice level; they are structurally sidelined by a division of labor that works without them.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_commitment__partition_reading, doctrinal_systematizers, excluded,
    moderate, biographical, mobile, national).

% Analyze historical and ethnographic evidence to determine whether shinbutsu-shugo reflects genuine functional partition, unified metaphysics, or tolerated incoherence. Their disagreement constitutes the kernel contest itself; this story represents the partition-reading camp's account.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_commitment__partition_reading, religious_studies_scholars, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Divides ritual labor along a life-cycle/afterlife axis so that two institutionally distinct religious traditions can each hold uncontested authority over a portion of human experience, avoiding jurisdictional conflict and duplicated ritual infrastructure.
% TRANSFER_FUNCTION: No systematic transfer between the two institutions; each collects fees, offerings, and social standing directly from lay households for services rendered within its own domain. Households move resources to whichever institution governs the occasion at hand.
% ABSENT_VOICES: Doctrinal systematizers who wanted a unified metaphysical account (predecessors and contemporaries of honji-suijaku theorists) are structurally absent from the practical arrangement — their integrative ambitions are neither required nor rewarded by a system that functions perfectly well without theological resolution.
% DISAPPEARANCE_RATIONALE: If the partition dissolved, either fusion (moving toward the syncretic reading) or open jurisdictional conflict (moving toward incoherence becoming untenable) would follow — shrines and temples might compete for funerary or birth rites, disrupting institutional stability. Whether the world 'rearranges' depends on which sibling reading is correct about what the partition was actually doing; the partition reading itself predicts function without deep metaphysical dependency, so disappearance would mainly require renegotiating labor division, not resolving cosmology.
% FOUNDING_PROBLEM: Two independently rooted religious traditions arrived at overlapping social terrain (life passages, community identity, mortality) without a conquest or displacement of either; a working arrangement was needed so households and communities could use both without institutional conflict or forced doctrinal choice.
% FOUNDING_PROBLEM_CORROBORATION: Contemporary ethnographers of Japanese religious practice (outside both shrine and temple institutional structures) document ongoing lay use of both systems along life-cycle/afterlife lines into the present, corroborating that the functional division persists independent of either institution's own self-description; Shinto and Buddhist clergy each independently affirm their own domain's authority without claiming the other's, which is itself external-to-each-other corroboration of a boundary rather than a merger.
narrative_ontology:disappearance_verdict(shinbutsu_ontological_commitment__partition_reading, contested).
narrative_ontology:founding_problem_status(shinbutsu_ontological_commitment__partition_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(shinbutsu_ontological_commitment__partition_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
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
 *   Extractiveness stays low and rises only marginally over the interval (0.10 to 0.18) reflecting gradual institutional consolidation (temple registration systems, shrine-temple complexes) without the arrangement becoming predatory. Theater ratio is modest (0.15 to 0.22): some ritual elaboration occurred as institutions competed for prestige, but the underlying functional division remained real rather than becoming a performance masking capture. Suppression is low (0.12) because practitioners were never coerced into using one tradition exclusively for either domain — coexistence was voluntary and stable. Accessibility collapse is modest (0.25): households retained real alternatives (folk religion, other Buddhist sects, regional variation) even as the general life-cycle/afterlife pattern became customary.
 *
 * DIRECTIONALITY LOGIC:
 *   Both shrine and temple lineages are coded as beneficiaries because each gains exclusive, uncontested jurisdiction over a domain of ritual life without needing to compete with or subordinate the other. Lay households are also beneficiaries (the arrangement solves a real coordination problem for them — no forced doctrinal choice) while simultaneously being payers in the ordinary sense of paying ritual fees to both institutions across a lifetime, which is why they carry a secondary payer role rather than appearing as victims. There is no victim group under this reading, which is the central structural difference from the incoherence reading (where tolerated contradiction might itself be read as costly) and from readings that posit institutional capture.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (avoiding conflict between two independently rooted traditions occupying overlapping social terrain) remains live: contemporary Japanese households still typically use Shinto rites for birth and marriage and Buddhist rites for funerals, corroborated by external ethnographic observation rather than self-report from either institution. This is not a case of an arrangement outliving its function under institutional inertia — the partition, on this reading, continues to do the coordination work it always did.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    partition_vs_syncretic_evidentiary_boundary,
    'Does the historical record show practitioners and clergy actively maintaining a doctrine-free functional boundary (partition), or does it show an operative honji-suijaku metaphysics that most practitioners simply did not articulate explicitly (syncretic, just tacit)?',
    'Close reading of medieval ritual manuals, temple-shrine complex (jingu-ji) administrative records, and doctrinal treatises to determine whether explicit metaphysical claims about kami-buddha identity were operative in ordinary practice or confined to a specialist theological stratum.',
    'If tacit syncretism was in fact operative for most practitioners, this partition reading overstates practitioner autonomy and the correct constraint is closer to the syncretic reading, with temple lineages as concentrated beneficiaries via honji-suijaku''s typical subordination of kami to buddhas.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(partition_vs_syncretic_evidentiary_boundary, conceptual, 'Whether the partition was a principled functional division or an unexamined surface over tacit syncretic metaphysics.').

omega_variable(
    partition_vs_incoherence_stability_test,
    'Was the life-cycle/afterlife division a stable, reproducible institutional norm, or did it vary so much by region, sect, and period that ''partition'' overstates a pattern that was really just contingent, untheorized variation (incoherence)?',
    'Comparative regional and sectarian survey across the Heian, Kamakura, and Edo periods to test whether the life-cycle/afterlife division held as a consistent norm or fragmented into locally divergent, unprincipled arrangements.',
    'If the division was highly unstable and contingent rather than a reproducible norm, the incoherence reading is better supported and this story''s claim of genuine functional coordination overstates the arrangement''s coherence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(partition_vs_incoherence_stability_test, empirical, 'Whether the observed division was a stable institutional norm or contingent regional variation mislabeled as partition.').

omega_variable(
    no_single_beneficiary_robustness,
    'Does the absence of a single concentrated beneficiary hold across the full interval, or did later periods (e.g., Edo-era danka temple registration system) create asymmetric extraction favoring Buddhist temples specifically?',
    'Examine Edo-period danka seido (temple registration) records for evidence of compulsory temple affiliation and fee extraction that would shift the arrangement from a two-sided partition toward temple-favoring tangled coordination.',
    'If danka registration created compulsory, non-optional temple affiliation with penalty for non-compliance, the later portion of this interval may better classify as tangled_rope with Buddhist institutions as concentrated beneficiaries and households as victims, requiring interval splitting.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(no_single_beneficiary_robustness, empirical, 'Whether Edo-era temple registration requirements undermine the no-single-beneficiary claim in the later interval.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(shinbutsu_ontological_commitment__partition_reading, 0, 1868).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(shin_tr_t0, shinbutsu_ontological_commitment__partition_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement_basis(shin_tr_t0, projected).
narrative_ontology:measurement(shin_tr_t300, shinbutsu_ontological_commitment__partition_reading, theater_ratio, 300, 0.16).
narrative_ontology:measurement_basis(shin_tr_t300, projected).
narrative_ontology:measurement(shin_tr_t600, shinbutsu_ontological_commitment__partition_reading, theater_ratio, 600, 0.18).
narrative_ontology:measurement_basis(shin_tr_t600, observed).
narrative_ontology:measurement(shin_tr_t900, shinbutsu_ontological_commitment__partition_reading, theater_ratio, 900, 0.19).
narrative_ontology:measurement_basis(shin_tr_t900, observed).
narrative_ontology:measurement(shin_tr_t1300, shinbutsu_ontological_commitment__partition_reading, theater_ratio, 1300, 0.2).
narrative_ontology:measurement_basis(shin_tr_t1300, observed).
narrative_ontology:measurement(shin_tr_t1600, shinbutsu_ontological_commitment__partition_reading, theater_ratio, 1600, 0.21).
narrative_ontology:measurement_basis(shin_tr_t1600, observed).
narrative_ontology:measurement(shin_tr_t1868, shinbutsu_ontological_commitment__partition_reading, theater_ratio, 1868, 0.22).
narrative_ontology:measurement_basis(shin_tr_t1868, observed).

% Extraction over time
narrative_ontology:measurement(shin_be_t0, shinbutsu_ontological_commitment__partition_reading, base_extractiveness, 0, 0.1).
narrative_ontology:measurement_basis(shin_be_t0, projected).
narrative_ontology:measurement(shin_be_t300, shinbutsu_ontological_commitment__partition_reading, base_extractiveness, 300, 0.12).
narrative_ontology:measurement_basis(shin_be_t300, projected).
narrative_ontology:measurement(shin_be_t600, shinbutsu_ontological_commitment__partition_reading, base_extractiveness, 600, 0.14).
narrative_ontology:measurement_basis(shin_be_t600, observed).
narrative_ontology:measurement(shin_be_t900, shinbutsu_ontological_commitment__partition_reading, base_extractiveness, 900, 0.15).
narrative_ontology:measurement_basis(shin_be_t900, observed).
narrative_ontology:measurement(shin_be_t1300, shinbutsu_ontological_commitment__partition_reading, base_extractiveness, 1300, 0.16).
narrative_ontology:measurement_basis(shin_be_t1300, observed).
narrative_ontology:measurement(shin_be_t1600, shinbutsu_ontological_commitment__partition_reading, base_extractiveness, 1600, 0.17).
narrative_ontology:measurement_basis(shin_be_t1600, observed).
narrative_ontology:measurement(shin_be_t1868, shinbutsu_ontological_commitment__partition_reading, base_extractiveness, 1868, 0.18).
narrative_ontology:measurement_basis(shin_be_t1868, observed).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(shinbutsu_ontological_commitment__partition_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(shinbutsu_ontological_commitment__partition_reading, identity_coordination).
narrative_ontology:affects_constraint(shinbutsu_ontological_commitment__partition_reading, shinbutsu_ontological_commitment__syncretic_reading).
narrative_ontology:affects_constraint(shinbutsu_ontological_commitment__partition_reading, shinbutsu_ontological_commitment__incoherence_reading).

% DUAL FORMULATION NOTE:
% This story is one of three constraint stories decomposing the shinbutsu_ontological_commitment kernel per the ε-invariance principle: the natural-language label 'shinbutsu-shugo' covers at least three structurally distinct claims about the relationship between Shinto and Buddhist ontology in premodern Japan. The partition_reading (this story) asserts functional coexistence without doctrinal integration and no concentrated beneficiary. The syncretic_reading asserts a unified honji-suijaku cosmology with buddhas as ground and kami as manifestation, implying Buddhist institutional primacy as beneficiary. The incoherence_reading asserts no stable ontological commitment existed at all, framing the coexistence as tolerated contradiction rather than principled arrangement. Each reading would compute different extraction and beneficiary profiles from the same underlying historical record, which is why they are authored as separate ε-invariant constraints rather than one story with a measurement parameter.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
