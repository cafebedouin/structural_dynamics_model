% ============================================================================
% CONSTRAINT STORY: shinbutsu_ontological_substrate__domain_partition_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_shinbutsu_ontological_substrate__domain_partition_reading, []).

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
 *   constraint_id: shinbutsu_ontological_substrate__domain_partition_reading
 *   human_readable: Shinbutsu Domain-Partition Reading: Functional Coexistence of Kami and Buddha Cults
 *   domain: religious_studies/japanese_history/commitment_systems
 *
 * SUMMARY:
 *   This constraint authors the domain-partition reading of the
 *   shinbutsu-shugo (kami-buddha coexistence) kernel in premodern and
 *   early-modern Japan: kami govern this-worldly matters and buddhas govern
 *   afterlife/salvation matters, and their long institutional cohabitation
 *   (shared shrine-temple complexes, combined ritual calendars, layered
 *   patronage) reflects a functional division of ritual labor rather than a
 *   metaphysical claim that kami and buddhas are the same beings under
 *   different names. This is one of three readings of a single contested
 *   kernel (shinbutsu_ontological_substrate). The syncretic_fusion_reading
 *   holds that honji suijaku describes genuine ontological identity (kami as
 *   local manifestations of buddhas); the incoherent_bundle_reading holds
 *   that no coherent kernel exists at all — the coexistence is accumulated
 *   institutional drift enforced by state power, retroactively narrativized.
 *   This story authors ONLY the domain-partition reading: low institutional
 *   entanglement, an easily separable functional division, and syncretism
 *   understood as pragmatic coexistence rather than fusion or incoherence.
 *   Per the ε-invariance principle, the other two readings are separate
 *   constraint files with their own ε, linked via
 *   network.affects_constraints.
 *
 * KEY AGENTS:
 *   - shrine_priests: this-world ritual authority (moderate/constrained) — beneficiary of jurisdictional clarity
 *   - temple_clergy: afterlife ritual authority (moderate/constrained) — beneficiary of jurisdictional clarity
 *   - lay_villagers: pragmatic dual-users (powerless/constrained) — beneficiaries of not having to choose a cosmology
 *   - local_ruling_houses: patrons of both institutions (powerful/mobile) — beneficiaries of not adjudicating a metaphysical dispute
 *   - doctrinal_systematizers: excluded from this reading's account — their unification project is treated as unnecessary
 *   - historians_of_religion: analytical observer of the institutional record
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(shinbutsu_ontological_substrate__domain_partition_reading, 0.28).
domain_priors:suppression_score(shinbutsu_ontological_substrate__domain_partition_reading, 0.22).
domain_priors:theater_ratio(shinbutsu_ontological_substrate__domain_partition_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(shinbutsu_ontological_substrate__domain_partition_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(shinbutsu_ontological_substrate__domain_partition_reading, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(shinbutsu_ontological_substrate__domain_partition_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(shinbutsu_ontological_substrate__domain_partition_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(shinbutsu_ontological_substrate__domain_partition_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(shinbutsu_ontological_substrate__domain_partition_reading, rope).
narrative_ontology:human_readable(shinbutsu_ontological_substrate__domain_partition_reading, "Shinbutsu Domain-Partition Reading: Functional Coexistence of Kami and Buddha Cults").
narrative_ontology:topic_domain(shinbutsu_ontological_substrate__domain_partition_reading, "religious_studies/japanese_history/commitment_systems").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(shinbutsu_ontological_substrate__domain_partition_reading, '0e874e47-29b6-4c51-a27a-ec1753d0fcd4').
narrative_ontology:cs_kernel_codification('0e874e47-29b6-4c51-a27a-ec1753d0fcd4', distributed).
narrative_ontology:cs_authority_grounding('0e874e47-29b6-4c51-a27a-ec1753d0fcd4', practice).
narrative_ontology:cs_interpretation_layer_present('0e874e47-29b6-4c51-a27a-ec1753d0fcd4').
narrative_ontology:cs_reading_relation('0e874e47-29b6-4c51-a27a-ec1753d0fcd4', shinbutsu_ontological_substrate__syncretic_fusion_reading, coexists_with).
narrative_ontology:cs_reading_relation('0e874e47-29b6-4c51-a27a-ec1753d0fcd4', shinbutsu_ontological_substrate__incoherent_bundle_reading, influences).
narrative_ontology:cs_axiom('0e874e47-29b6-4c51-a27a-ec1753d0fcd4', foundational, kami_and_buddha_jurisdictions_are_functionally_separable).
narrative_ontology:cs_axiom_status(kami_and_buddha_jurisdictions_are_functionally_separable, holdable).
narrative_ontology:cs_axiom_grounding('0e874e47-29b6-4c51-a27a-ec1753d0fcd4', kami_and_buddha_jurisdictions_are_functionally_separable, conventional).
narrative_ontology:cs_axiom('0e874e47-29b6-4c51-a27a-ec1753d0fcd4', foundational, coexistence_requires_no_ontological_reconciliation).
narrative_ontology:cs_axiom_status(coexistence_requires_no_ontological_reconciliation, holdable).
narrative_ontology:cs_axiom_grounding('0e874e47-29b6-4c51-a27a-ec1753d0fcd4', coexistence_requires_no_ontological_reconciliation, instrumental).
narrative_ontology:cs_reference_frame('0e874e47-29b6-4c51-a27a-ec1753d0fcd4', premodern_functional_division_of_ritual_labor).
narrative_ontology:cs_drift_state('0e874e47-29b6-4c51-a27a-ec1753d0fcd4', meiji_shinbutsu_bunri_edict, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('0e874e47-29b6-4c51-a27a-ec1753d0fcd4', '').
narrative_ontology:cs_kernel_id(shinbutsu_ontological_substrate__domain_partition_reading, shinbutsu_ontological_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(shinbutsu_ontological_substrate__domain_partition_reading, shrine_priests).
narrative_ontology:constraint_beneficiary(shinbutsu_ontological_substrate__domain_partition_reading, temple_clergy).
narrative_ontology:constraint_beneficiary(shinbutsu_ontological_substrate__domain_partition_reading, lay_villagers).
narrative_ontology:constraint_beneficiary(shinbutsu_ontological_substrate__domain_partition_reading, local_ruling_houses).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administer kami rites concerned with this-worldly matters — harvest, purification, community protection, birth. Under the domain-partition reading, their ritual authority over this-world affairs is left intact and undiluted by Buddhist claims over the afterlife; they participate in shared festival calendars and combined shrine-temple complexes (jingu-ji) without ceding jurisdiction. Their livelihood and status depend on the this-world domain remaining kami territory.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_substrate__domain_partition_reading, shrine_priests, beneficiary,
    moderate, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(shinbutsu_ontological_substrate__domain_partition_reading, shrine_priests, agenda_setter).

% Administer Buddhist rites concerned with salvation, karma, and the afterlife. Under this reading they gain unchallenged jurisdiction over death, memorial, and rebirth practice while leaving this-world kami functions to shrine priests. Their institutional position benefits from a clean division of labor that avoids doctrinal competition over the same ritual ground.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_substrate__domain_partition_reading, temple_clergy, beneficiary,
    moderate, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(shinbutsu_ontological_substrate__domain_partition_reading, temple_clergy, agenda_setter).

% Draw on kami ritual for this-life concerns (harvest, illness, marriage, community protection) and on Buddhist ritual for death and ancestral memorial, without needing to resolve any doctrinal tension between the two. The domain partition lets them use both systems pragmatically as needed, each for its proper occasion, without a felt requirement to choose one cosmology over the other.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_substrate__domain_partition_reading, lay_villagers, beneficiary,
    powerless, biographical, constrained, local).

% Patronize both shrine and temple institutions as sources of legitimacy and social order. The domain-partition arrangement lets them fund and be seen supporting both without having to adjudicate a metaphysical dispute, since the functional division removes any need to declare one system doctrinally superior to the other.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_substrate__domain_partition_reading, local_ruling_houses, beneficiary,
    powerful, generational, mobile, regional).

% Scholar-monks and theologians (associated with honji suijaku metaphysical schemes) who want a unified ontological account of kami as buddha-manifestations are not required by, and gain little from, a reading that treats the coexistence as merely functional. Their systematizing project is sidelined by this reading, which treats their metaphysical claims as unnecessary to explain the observed practice.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_substrate__domain_partition_reading, doctrinal_systematizers, excluded,
    moderate, generational, mobile, national).

% Examine the historical record of shrine-temple complexes, ritual calendars, and doctrinal texts to determine whether the coexistence reflects genuine metaphysical synthesis, pragmatic functional division, or unexamined institutional accretion. Their scholarly account does not itself alter the practice but shapes how later generations interpret it.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_substrate__domain_partition_reading, historians_of_religion, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Divides religious labor cleanly: kami cults handle this-world concerns (fertility, purification, protection, community order) and Buddhist institutions handle afterlife concerns (salvation, memorial, karma), letting a single community draw on both without doctrinal collision or the need to choose an exclusive cosmology.
% TRANSFER_FUNCTION: Moves ritual authority and lay patronage along a functional line rather than a doctrinal one: kami priests receive this-world ritual business and Buddhist clergy receive death-and-afterlife ritual business, with combined shrine-temple complexes (jingu-ji) sharing physical and calendrical infrastructure.
% ABSENT_VOICES: Doctrinal systematizers pursuing a unified honji suijaku metaphysics are not part of this reading's account — their ontological synthesis project is treated as a separate, non-necessary layer built atop a coexistence that this reading holds needs no such synthesis to function.
% DISAPPEARANCE_RATIONALE: If the domain partition were to disappear, shrine and temple institutions would either need to compete directly over the same ritual ground or actively merge doctrinally; practitioners disagree about whether the historical shinbutsu bunri (Meiji-era forced separation) demonstrates the partition was load-bearing institutional infrastructure (world_rearranges) or whether the partition was already so functionally thin that formal separation changed little in lived practice (world_unchanged) — hence contested.
% FOUNDING_PROBLEM: Early Japanese religious life needed to accommodate an indigenous kami cosmology alongside an imported Buddhist soteriology without either community abandoning its existing this-world ritual practice or its emerging afterlife doctrine.
% FOUNDING_PROBLEM_CORROBORATION: Shrine priests and temple clergy (the benefiting parties) attest the domain division as a stable, functioning arrangement; independent corroboration comes from historians of religion studying jingu-ji institutional records and ritual calendars, who document that the this-world/afterlife division tracked observable practice for centuries, though these same historians note the division was never formally doctrinally settled and was itself vulnerable to reinterpretation, which is part of why the status remains contested rather than simply live.
narrative_ontology:disappearance_verdict(shinbutsu_ontological_substrate__domain_partition_reading, contested).
narrative_ontology:founding_problem_status(shinbutsu_ontological_substrate__domain_partition_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(shinbutsu_ontological_substrate__domain_partition_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(shinbutsu_ontological_substrate__domain_partition_reading, 'none', 1).
narrative_ontology:epsilon_provenance(shinbutsu_ontological_substrate__domain_partition_reading, 0.28, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(shinbutsu_ontological_substrate__domain_partition_reading_tests).
:- end_tests(shinbutsu_ontological_substrate__domain_partition_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored low (0.28 at interval end) because, under this reading, the arrangement is genuinely low-coercion: no party is forced to accept a metaphysical claim, and clergy on both sides retain autonomous jurisdiction rather than one side capturing the other's domain. Suppression is correspondingly low (0.22) — the domain partition does not require active enforcement to hold; it persists because it is convenient and non-competing, not because dissent is punished. A modest rise in both metrics across the interval reflects the historically real, if slow, encroachment of institutional entanglement (jingu-ji administrative fusion, joint landholding) that made later separation (the Meiji shinbutsu bunri) more disruptive than a purely functional division would predict — this is authored honestly as a measured drift, not tuned to fit the claimed_type. Accessibility collapse is moderate (0.35): alternatives (exclusive kami practice, exclusive Buddhist practice, syncretic fusion) remained visibly available and practiced by some communities throughout the interval, so collapse is real but partial. Resistance is moderate-low (0.30): the arrangement met periodic doctrinal resistance from purists on both sides (kami-only nativist strands, Buddhist exclusivist strands) but not sustained structural resistance, consistent with a rope rather than an enforced hybrid.
 *
 * PERSPECTIVAL GAP:
 *   The claimed_type (rope) and the authored metrics are in general agreement here, which is itself worth flagging as a fact about this particular reading rather than a property of the underlying phenomenon: a reader who instead adopted the syncretic_fusion_reading would need to explain the same historical record (jingu-ji complexes, honji suijaku vocabulary) with a very different ε, because under that reading the vocabulary of fusion is doing real ontological work rather than merely convenient labeling — that divergence across readings is exactly what the kernel/reading decomposition is for, and it is why this story does not attempt to average across the three readings.
 *
 * DIRECTIONALITY LOGIC:
 *   All four active-institution seats (shrine_priests, temple_clergy, lay_villagers, local_ruling_houses) are declared beneficiaries because, under the domain-partition reading, the functional division genuinely reduces the coordination costs each would otherwise bear (doctrinal competition, forced cosmological choice, ritual jurisdiction disputes). No victims are declared under this reading because the low-suppression, low-extraction, easily-separable structure the reading describes does not identify any party who is made worse off by the arrangement — this is a structural consequence of the reading's own premises, not an oversight. doctrinal_systematizers are marked excluded rather than victim because their exclusion from this reading's account is an interpretive omission, not a material cost imposed on them by the arrangement itself.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding_problem (accommodating an indigenous cosmology alongside an imported soteriology without institutional collision) has an ambiguous status precisely because the domain-partition reading holds it is durably solved by an ongoing, low-cost functional division rather than a one-time fix that could go stale — there is no obvious point at which the division would become obsolete while still being enforced, because on this reading it was never chiefly an enforcement mechanism. The Meiji-era shinbutsu bunri (forced state separation) is the natural test case for whether mandatrophy ever applied: if the pre-Meiji partition was genuinely functional and low-cost, the forced separation should have been comparatively easy to execute cleanly, and to the extent it was not (temples closed, artifacts destroyed, syncretic practices persisted underground), that is evidence favoring the sibling incoherent_bundle_reading over this one, which the corpus can weigh by comparing the two stories' metric profiles.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_choice_among_three_readings,
    'Is the shinbutsu-shugo coexistence best modeled as (a) a functional domain partition, (b) a genuine ontological fusion, or (c) an incoherent institutional bundle with no unifying commitment at all?',
    'Comparative analysis of primary doctrinal texts (honji suijaku treatises), institutional records of jingu-ji administration, and the disruption profile of the Meiji shinbutsu bunri — a clean, low-cost separation favors (a); deep metaphysical commitment surviving separation attempts favors (b); persistent incoherence and dependence on state enforcement favors (c).',
    'If (b) or (c) is favored by the historical record, this story''s low-extraction, low-suppression rope classification would not describe the actual arrangement, and the corpus should weight the sibling stories (syncretic_fusion_reading, incoherent_bundle_reading) more heavily as the operative account.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_choice_among_three_readings, conceptual, 'Committer-frame ambiguity: which of the three kernel readings best fits the historical shinbutsu-shugo record.').

omega_variable(
    meiji_separation_as_natural_experiment,
    'Does the disruption caused by the Meiji-era forced shinbutsu bunri (separation edict) indicate that the pre-Meiji coexistence was load-bearing institutional infrastructure (favoring incoherent_bundle_reading or syncretic_fusion_reading) or that it was genuinely separable and the disruption was primarily political/violent rather than structural (favoring this domain_partition_reading)?',
    'Historical case studies of specific shrine-temple complexes before and after the 1868 edict, tracking whether ritual function, lay practice, and clergy livelihoods reorganized smoothly along the claimed domain lines or required substantial doctrinal and economic restructuring.',
    'A smooth reorganization supports this reading''s claim of low institutional entanglement; substantial restructuring costs would support a higher ε and favor the sibling readings.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(meiji_separation_as_natural_experiment, empirical, 'Whether the 1868 forced separation confirms or undermines the domain-partition reading''s low-entanglement claim.').

omega_variable(
    excluded_systematizers_significance,
    'Does the marginalization of doctrinal systematizers (honji suijaku theorists) under this reading reflect their genuine institutional irrelevance to lived practice, or does it understate a real metaphysical commitment that shaped ritual behavior even among ordinary practitioners?',
    'Analysis of lay religious texts, prayers, and ex-votos for evidence of metaphysical (fusion) versus purely functional (partition) framing in everyday practice, not just elite doctrinal writing.',
    'If lay practice shows embedded fusion-language, the domain_partition_reading understates the ontological commitment actually operative, weakening its claim relative to syncretic_fusion_reading.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(excluded_systematizers_significance, empirical, 'Whether excluding doctrinal systematizers from this reading''s account is analytically justified or elides real popular metaphysical commitment.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(shinbutsu_ontological_substrate__domain_partition_reading, 0, 1200).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(shin_tr_t0, shinbutsu_ontological_substrate__domain_partition_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(shin_tr_t200, shinbutsu_ontological_substrate__domain_partition_reading, theater_ratio, 200, 0.17).
narrative_ontology:measurement(shin_tr_t400, shinbutsu_ontological_substrate__domain_partition_reading, theater_ratio, 400, 0.2).
narrative_ontology:measurement(shin_tr_t600, shinbutsu_ontological_substrate__domain_partition_reading, theater_ratio, 600, 0.22).
narrative_ontology:measurement(shin_tr_t800, shinbutsu_ontological_substrate__domain_partition_reading, theater_ratio, 800, 0.25).
narrative_ontology:measurement(shin_tr_t1000, shinbutsu_ontological_substrate__domain_partition_reading, theater_ratio, 1000, 0.28).
narrative_ontology:measurement(shin_tr_t1200, shinbutsu_ontological_substrate__domain_partition_reading, theater_ratio, 1200, 0.3).

% Extraction over time
narrative_ontology:measurement(shin_be_t0, shinbutsu_ontological_substrate__domain_partition_reading, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(shin_be_t200, shinbutsu_ontological_substrate__domain_partition_reading, base_extractiveness, 200, 0.2).
narrative_ontology:measurement(shin_be_t400, shinbutsu_ontological_substrate__domain_partition_reading, base_extractiveness, 400, 0.22).
narrative_ontology:measurement(shin_be_t600, shinbutsu_ontological_substrate__domain_partition_reading, base_extractiveness, 600, 0.24).
narrative_ontology:measurement(shin_be_t800, shinbutsu_ontological_substrate__domain_partition_reading, base_extractiveness, 800, 0.26).
narrative_ontology:measurement(shin_be_t1000, shinbutsu_ontological_substrate__domain_partition_reading, base_extractiveness, 1000, 0.27).
narrative_ontology:measurement(shin_be_t1200, shinbutsu_ontological_substrate__domain_partition_reading, base_extractiveness, 1200, 0.28).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(shinbutsu_ontological_substrate__domain_partition_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(shinbutsu_ontological_substrate__domain_partition_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(shinbutsu_ontological_substrate__domain_partition_reading, 0.08).
narrative_ontology:affects_constraint(shinbutsu_ontological_substrate__domain_partition_reading, shinbutsu_ontological_substrate__syncretic_fusion_reading).
narrative_ontology:affects_constraint(shinbutsu_ontological_substrate__domain_partition_reading, shinbutsu_ontological_substrate__incoherent_bundle_reading).

% DUAL FORMULATION NOTE:
% Three constraint files instantiate the shinbutsu_ontological_substrate kernel: this one (domain_partition_reading, low ε ~0.28, rope), syncretic_fusion_reading (expected higher institutional entanglement from genuine metaphysical commitment), and incoherent_bundle_reading (expected higher suppression from reliance on state enforcement to hold together a non-coherent bundle). Each authors its own ε from its own premises about the same historical record (shrine-temple coexistence, honji suijaku vocabulary, the Meiji separation); none averages over the others. Network edges recorded here to preserve family linkage for contamination/comparison analysis.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
