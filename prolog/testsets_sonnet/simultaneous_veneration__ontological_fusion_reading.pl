% ============================================================================
% CONSTRAINT STORY: simultaneous_veneration__ontological_fusion_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_simultaneous_veneration__ontological_fusion_reading, []).

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
 *   constraint_id: simultaneous_veneration__ontological_fusion_reading
 *   human_readable: Honji-Suijaku Ontological Fusion Doctrine
 *   domain: religious/institutional/historical
 *
 * SUMMARY:
 *   This story instantiates the ontological_fusion_reading of the
 *   simultaneous_veneration kernel: the claim that honji-suijaku theory is
 *   not a functional accommodation or an unresolved contradiction but a
 *   metaphysical discovery — kami and buddhas are literally the same beings,
 *   apprehended differently by different audiences. Under this reading, the
 *   doctrine's authority rests on it being TRUE, not merely useful, which is
 *   precisely what licenses Buddhist clerical hierarchies to administer
 *   shrine institutions: if the buddha is what the kami ontologically IS,
 *   then the interpreter of Buddhist cosmology is the more authoritative
 *   interpreter of the kami as well. This is a substantively different
 *   constraint from the domain_partition_reading (which claims only
 *   functional specialization, no ontological subordination, and therefore no
 *   interpretive monopoly) and from the pragmatic_incoherence_reading (which
 *   denies the doctrine was ever a coherent claim at all, treating its long
 *   tenure as an artifact of low enforcement pressure rather than settled
 *   truth). Each reading has its own epsilon and its own beneficiary/victim
 *   structure; only this one, the fusion reading, generates a truth-claim
 *   strong enough to ground genuine institutional extraction, since claiming
 *   metaphysical priority over a pre-existing tradition's deities is a much
 *   higher-stakes move than partitioning ritual domains or acknowledging
 *   incoherence.
 *
 * KEY AGENTS:
 *   - buddhist_institutional_hierarchy: agenda_setter (institutional/arbitrage) - promulgates and administers the ontological fusion doctrine
 *   - shingon_tendai_temple_networks: beneficiary (institutional/arbitrage) - gains administrative control of combined shrine-temple complexes
 *   - kami_cult_autonomy: payer (powerless/trapped) - loses independent metaphysical standing
 *   - shrine_priest_lineages: payer (moderate/constrained) - must operate within Buddhist-supremacist ritual frameworks
 *   - localized_kami_traditions: excluded (powerless/trapped) - not consulted in doctrinal formation despite being its object
 *   - imperial_court_patrons: beneficiary (powerful/mobile) - uses fusion doctrine to unify state legitimation ideology
 *   - comparative_religion_scholars: observer (analytical/analytical) - sees the historically contingent construction beneath the metaphysical claim
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(simultaneous_veneration__ontological_fusion_reading, 0.71).
domain_priors:suppression_score(simultaneous_veneration__ontological_fusion_reading, 0.62).
domain_priors:theater_ratio(simultaneous_veneration__ontological_fusion_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(simultaneous_veneration__ontological_fusion_reading, extractiveness, 0.71).
narrative_ontology:constraint_metric(simultaneous_veneration__ontological_fusion_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(simultaneous_veneration__ontological_fusion_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(simultaneous_veneration__ontological_fusion_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(simultaneous_veneration__ontological_fusion_reading, resistance, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(simultaneous_veneration__ontological_fusion_reading, tangled_rope).
narrative_ontology:human_readable(simultaneous_veneration__ontological_fusion_reading, "Honji-Suijaku Ontological Fusion Doctrine").
narrative_ontology:topic_domain(simultaneous_veneration__ontological_fusion_reading, "religious/institutional/historical").

domain_priors:requires_active_enforcement(simultaneous_veneration__ontological_fusion_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(simultaneous_veneration__ontological_fusion_reading, '58aed5ad-7ed8-49a5-b233-52c89bcee66f').
narrative_ontology:cs_kernel_codification('58aed5ad-7ed8-49a5-b233-52c89bcee66f', distributed).
narrative_ontology:cs_authority_grounding('58aed5ad-7ed8-49a5-b233-52c89bcee66f', lineage).
narrative_ontology:cs_interpretation_layer_present('58aed5ad-7ed8-49a5-b233-52c89bcee66f').
narrative_ontology:cs_reading_relation('58aed5ad-7ed8-49a5-b233-52c89bcee66f', simultaneous_veneration__domain_partition_reading, coexists_with).
narrative_ontology:cs_reading_relation('58aed5ad-7ed8-49a5-b233-52c89bcee66f', simultaneous_veneration__pragmatic_incoherence_reading, influences).
narrative_ontology:cs_axiom('58aed5ad-7ed8-49a5-b233-52c89bcee66f', foundational, kami_and_buddha_share_single_underlying_ontology).
narrative_ontology:cs_axiom_status(kami_and_buddha_share_single_underlying_ontology, holdable).
narrative_ontology:cs_axiom_grounding('58aed5ad-7ed8-49a5-b233-52c89bcee66f', kami_and_buddha_share_single_underlying_ontology, theological).
narrative_ontology:cs_axiom('58aed5ad-7ed8-49a5-b233-52c89bcee66f', secondary, buddhist_cosmology_holds_explanatory_priority_over_kami_cosmology).
narrative_ontology:cs_axiom_status(buddhist_cosmology_holds_explanatory_priority_over_kami_cosmology, holdable).
narrative_ontology:cs_axiom_grounding('58aed5ad-7ed8-49a5-b233-52c89bcee66f', buddhist_cosmology_holds_explanatory_priority_over_kami_cosmology, conventional).
narrative_ontology:cs_reference_frame('58aed5ad-7ed8-49a5-b233-52c89bcee66f', heian_period_scholastic_syncretism).
narrative_ontology:cs_drift_state('58aed5ad-7ed8-49a5-b233-52c89bcee66f', meiji_shinbutsu_bunri, gap(repudiation_pressure, severe, true)).
narrative_ontology:cs_created_at('58aed5ad-7ed8-49a5-b233-52c89bcee66f', '').
narrative_ontology:cs_kernel_id(simultaneous_veneration__ontological_fusion_reading, simultaneous_veneration).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(simultaneous_veneration__ontological_fusion_reading, buddhist_institutional_hierarchy).
narrative_ontology:constraint_beneficiary(simultaneous_veneration__ontological_fusion_reading, shingon_tendai_temple_networks).
narrative_ontology:constraint_victim(simultaneous_veneration__ontological_fusion_reading, kami_cult_autonomy).
narrative_ontology:constraint_victim(simultaneous_veneration__ontological_fusion_reading, shrine_priest_lineages).
narrative_ontology:constraint_victim(simultaneous_veneration__ontological_fusion_reading, localized_kami_traditions).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(simultaneous_veneration__ontological_fusion_reading, imperial_court_patrons).
narrative_ontology:constraint_vindicates(simultaneous_veneration__ontological_fusion_reading, metaphysical_unity_of_kami_and_buddhas).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Shingon and Tendai temple networks develop and promulgate honji-suijaku doctrine, formally designating specific buddhas and bodhisattvas as the 'original ground' (honji) of which kami are mere 'traces' (suijaku). This doctrinal architecture is administered through temple-shrine complexes (jingu-ji) where Buddhist clergy oversee combined ritual life, positioning Buddhist cosmology as metaphysically prior and Buddhist clergy as the only competent interpreters of what a kami 'really is.'
narrative_ontology:constraint_stakeholder(simultaneous_veneration__ontological_fusion_reading, buddhist_institutional_hierarchy, agenda_setter,
    institutional, civilizational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(simultaneous_veneration__ontological_fusion_reading, buddhist_institutional_hierarchy, beneficiary).

% Gain administrative and doctrinal control over shrine properties, revenues, and ritual calendars through the jingu-ji system justified by the ontological fusion claim. Their institutional standing and land income depend on the claim holding as settled metaphysics rather than as one interpretation among several.
narrative_ontology:constraint_stakeholder(simultaneous_veneration__ontological_fusion_reading, shingon_tendai_temple_networks, beneficiary,
    institutional, generational, arbitrage, national).

% Local kami veneration traditions — often tied to specific geographic features, clan lineages, or agricultural cycles — lose independent metaphysical standing once reclassified as 'traces' of a Buddhist original. Their cosmological particularity is subordinated to a Buddhist explanatory frame they had no role in authoring, and reversal requires displacing an entrenched clerical hierarchy.
narrative_ontology:constraint_stakeholder(simultaneous_veneration__ontological_fusion_reading, kami_cult_autonomy, payer,
    powerless, generational, trapped, regional).

% Kannushi and shrine-keeping families operating within jingu-ji complexes must accommodate Buddhist ritual and doctrinal supremacy to retain state and aristocratic patronage. They can resist quietly through local practice but cannot publicly contest the honji-suijaku framework without risking patronage and legal standing under the ritsuryo-era religious administration.
narrative_ontology:constraint_stakeholder(simultaneous_veneration__ontological_fusion_reading, shrine_priest_lineages, payer,
    moderate, biographical, constrained, regional).

% Village and clan-level kami cults with no representation in the courtly or monastic doctrinal debates that produced honji-suijaku theory. Their own cosmological self-understanding — that kami are autonomous, uncaused, and prior to any imported frame — is not consulted; the fusion doctrine is applied to them from outside and above.
narrative_ontology:constraint_stakeholder(simultaneous_veneration__ontological_fusion_reading, localized_kami_traditions, excluded,
    powerless, generational, trapped, local).

% The court finds honji-suijaku doctrine useful for legitimating imperial rule by fusing kami-based claims to divine descent with Buddhist cosmological universalism, gaining a syncretic ideology that unifies disparate regional cults under a coherent state framework administered through allied temple institutions.
narrative_ontology:constraint_stakeholder(simultaneous_veneration__ontological_fusion_reading, imperial_court_patrons, beneficiary,
    powerful, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(simultaneous_veneration__ontological_fusion_reading, imperial_court_patrons, observer).

% Examine honji-suijaku as a historically contingent doctrinal formation rather than a metaphysical discovery, comparing it to other syncretic schemes (interpretatio graeca, orisha-saint correspondences) where a dominant tradition's cosmology annexes a subordinate one's deities as partial or derivative manifestations of its own.
narrative_ontology:constraint_stakeholder(simultaneous_veneration__ontological_fusion_reading, comparative_religion_scholars, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a unifying cosmological vocabulary that lets a single ritual complex (jingu-ji) serve both kami and Buddhist worship without requiring practitioners to choose between traditions, reducing the coordination cost of a religiously plural society under one administrative system.
% TRANSFER_FUNCTION: Moves interpretive authority, land revenue, and ritual precedence from independent shrine institutions and local kami cults to Buddhist temple hierarchies, who administer the combined complexes and determine which buddha corresponds to which kami.
% ABSENT_VOICES: Village-level kami cult practitioners and shrine priest lineages predating Buddhist arrival are not present in the courtly and monastic circles that formalized honji-suijaku theory; their own accounts of kami as autonomous, non-derivative beings are absent from the doctrinal record that claims to describe them.
% DISAPPEARANCE_RATIONALE: The Meiji-era shinbutsu bunri (separation of kami and buddhas) decree in 1868 demonstrates empirically what happens when this doctrine is withdrawn by state fiat: jingu-ji complexes were forcibly dismantled, shrine and temple properties were separated, clergy were reassigned or defrocked, and independent Shinto institutional identity was reconstructed almost overnight — showing the fusion claim had been load-bearing institutional architecture, not free-floating metaphysical description.
% FOUNDING_PROBLEM: Buddhist missionaries and institutions arriving in Japan needed a framework to explain their relationship to pre-existing, deeply entrenched kami worship without provoking rejection — honji-suijaku offered a way to absorb kami into Buddhist cosmology rather than displacing them outright.
% FOUNDING_PROBLEM_CORROBORATION: The Meiji state's own shinbutsu bunri decree (1868) and the subsequent State Shinto reconstruction attest, from outside the Buddhist institutional beneficiaries, that the fusion could be administratively unwound without cosmological catastrophe — implying the 'problem' of two traditions coexisting had other viable solutions (domain partition, or acknowledged non-resolution) and that ontological fusion was a historically specific institutional accommodation, not a metaphysical necessity. Independent comparative religion scholarship (e.g. studies of honji-suijaku as ideology of medieval Japanese religious institutions) corroborates this reading from outside both the Buddhist and Shinto institutional interests.
narrative_ontology:disappearance_verdict(simultaneous_veneration__ontological_fusion_reading, world_rearranges).
narrative_ontology:founding_problem_status(simultaneous_veneration__ontological_fusion_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(simultaneous_veneration__ontological_fusion_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(simultaneous_veneration__ontological_fusion_reading, 'none', 1).
narrative_ontology:epsilon_provenance(simultaneous_veneration__ontological_fusion_reading, 0.71, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(simultaneous_veneration__ontological_fusion_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(simultaneous_veneration__ontological_fusion_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(simultaneous_veneration__ontological_fusion_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises across the measured interval (0.35 to 0.71) as jingu-ji institutional consolidation matures from an initial period of genuine syncretic experimentation into an entrenched doctrinal-administrative order where Buddhist clergy hold formal precedence over shrine institutions and revenues. Suppression climbs similarly (0.30 to 0.62) as the doctrine moves from voluntary theological synthesis to state-sanctioned administrative fact backed by ritsuryo-era religious governance, making dissent from local kami traditions costlier over time. Theater ratio rises moderately (0.15 to 0.40) reflecting a growing gap between the doctrine's professed function (harmonizing two traditions) and its administrative reality (concentrating clerical authority) as the institutional apparatus around it thickens without corresponding growth in genuine metaphysical inquiry. All three metrics are authored on one shared grid.
 *
 * PERSPECTIVAL GAP:
 *   From the Buddhist institutional seat, the fusion doctrine looks like completed metaphysical inquiry backed by centuries of scholastic elaboration — a settled cosmological fact, not a claim requiring ongoing defense. From the kami cult autonomy and shrine priest seats, the same doctrine looks like an imposed hierarchy that happens to be dressed in metaphysical language; the 1868 shinbutsu bunri separation revealed how quickly the 'settled fact' could be administratively unwound once state incentives shifted, which is difficult to explain if the doctrine had genuinely tracked mind-independent metaphysical truth rather than institutional convenience.
 *
 * DIRECTIONALITY LOGIC:
 *   Buddhist institutional hierarchies and their allied temple networks sit at the beneficiary end: they administer the doctrine, control resulting institutions, and their standing depends on the ontological claim being treated as settled rather than contested. The imperial court is a secondary beneficiary, gaining a unifying state ideology, and its mobility (multiple legitimation strategies available) puts it nearer symmetric than the temple networks. Kami cult autonomy and shrine priest lineages sit at the target end: their exit options are trapped or constrained respectively, since abandoning the fusion framework means losing patronage, legal standing, or institutional continuity built up over centuries. Localized kami traditions are structurally excluded rather than merely extracted from — they are the object of a doctrine formed entirely without their participation, which is the sharpest form of the asymmetry this reading names.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as tangled_rope rather than pure snare preserves the genuine coordination function honji-suijaku performed: it did allow religiously plural communities to worship at shared sites without violent conflict for many centuries, and jingu-ji complexes provided real infrastructural and ritual services. Treating it as pure extraction would erase that real coordination achievement. But the ontological fusion reading specifically claims something stronger than coordination — metaphysical truth — and that stronger claim is exactly what licenses the asymmetric interpretive authority Buddhist institutions exercised over kami traditions. The mandatrophy question is whether the metaphysical claim outlived whatever genuine syncretic function it once served, becoming primarily a vehicle for institutional precedence by the doctrine's later centuries — the rising extraction and theater-ratio trajectory suggests it did.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    metaphysical_truth_vs_institutional_convenience,
    'Did honji-suijaku theory articulate a genuine metaphysical discovery about the underlying identity of kami and buddhas, or was the ontological-identity framing itself constructed to serve Buddhist institutional interests in an already-existing syncretic practice?',
    'Compare doctrinal development against institutional consolidation timelines: if formal identity-claims (specific honji-suijaku pairings) intensify in step with jingu-ji administrative and revenue consolidation rather than preceding it theologically, the construction reading is favored. Textual analysis of earlier syncretic sources (before systematic honji-suijaku pairing lists) for whether they assert identity or merely co-presence would also bear on this.',
    'If the doctrine was constructed for institutional convenience, the ontological_fusion_reading''s core premise dissolves into either the domain_partition_reading or the pragmatic_incoherence_reading, and this story''s high epsilon is properly attributed to power dynamics rather than to any metaphysical claim being ''true.'' If a genuine independent metaphysical case can be made, the extraction remains but the vindicated_propositions entry gains stronger independent support.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(metaphysical_truth_vs_institutional_convenience, conceptual, 'Whether the ontological identity claim is genuine metaphysics or institutionally motivated doctrine construction').

omega_variable(
    kernel_reading_selection_evidence,
    'What textual and institutional evidence would distinguish this fusion reading from the domain_partition_reading, given that both readings are compatible with the same surface-level practice of shared worship at jingu-ji complexes?',
    'Examine whether historical honji-suijaku texts assert identity claims (kami IS buddha, apprehended differently) versus functional claims (kami handles this-worldly matters, buddha handles salvation, with no claim about their underlying nature) — the specific theological vocabulary used in Tendai hongaku thought versus more instrumental shrine-temple administrative agreements would be diagnostic.',
    'If most historical sources use functional rather than identity language, this fusion reading describes a minority or later scholastic elaboration rather than the dominant historical practice, and the domain_partition_reading would be the better-evidenced sibling for most of the interval.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_selection_evidence, empirical, 'Textual evidence bearing on which kernel reading better matches the historical doctrinal record').

omega_variable(
    beneficiary_hierarchy_internal_contestation,
    'Was the Buddhist institutional hierarchy itself unified in advancing the ontological fusion claim, or did different sects (Shingon ryobu shinto vs. Tendai sanno shinto) advance competing and mutually inconsistent fusion schemes that undermine the claim to a single settled metaphysical truth?',
    'Comparative analysis of Shingon and Tendai honji-suijaku pairing schemes for the same kami — documented disagreements about which buddha corresponds to which kami would show the ''discovery'' varied by sectarian affiliation rather than converging on a single truth.',
    'Documented inter-sectarian disagreement about specific pairings would weaken the metaphysical-truth claim considerably, since a genuine ontological fact should not vary by which Buddhist school is doing the identifying — this would push the story toward the pragmatic_incoherence_reading for at least the periods of sectarian dispute.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_hierarchy_internal_contestation, empirical, 'Whether sectarian disagreement about specific honji-suijaku pairings undermines the singular-truth claim').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(simultaneous_veneration__ontological_fusion_reading, 0, 1200).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(simu_tr_t0, simultaneous_veneration__ontological_fusion_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement_basis(simu_tr_t0, observed).
narrative_ontology:measurement(simu_tr_t200, simultaneous_veneration__ontological_fusion_reading, theater_ratio, 200, 0.2).
narrative_ontology:measurement_basis(simu_tr_t200, observed).
narrative_ontology:measurement(simu_tr_t400, simultaneous_veneration__ontological_fusion_reading, theater_ratio, 400, 0.28).
narrative_ontology:measurement_basis(simu_tr_t400, observed).
narrative_ontology:measurement(simu_tr_t600, simultaneous_veneration__ontological_fusion_reading, theater_ratio, 600, 0.33).
narrative_ontology:measurement_basis(simu_tr_t600, observed).
narrative_ontology:measurement(simu_tr_t800, simultaneous_veneration__ontological_fusion_reading, theater_ratio, 800, 0.37).
narrative_ontology:measurement_basis(simu_tr_t800, observed).
narrative_ontology:measurement(simu_tr_t1000, simultaneous_veneration__ontological_fusion_reading, theater_ratio, 1000, 0.4).
narrative_ontology:measurement_basis(simu_tr_t1000, observed).
narrative_ontology:measurement(simu_tr_t1200, simultaneous_veneration__ontological_fusion_reading, theater_ratio, 1200, 0.4).
narrative_ontology:measurement_basis(simu_tr_t1200, observed).

% Extraction over time
narrative_ontology:measurement(simu_be_t0, simultaneous_veneration__ontological_fusion_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement_basis(simu_be_t0, observed).
narrative_ontology:measurement(simu_be_t200, simultaneous_veneration__ontological_fusion_reading, base_extractiveness, 200, 0.48).
narrative_ontology:measurement_basis(simu_be_t200, observed).
narrative_ontology:measurement(simu_be_t400, simultaneous_veneration__ontological_fusion_reading, base_extractiveness, 400, 0.58).
narrative_ontology:measurement_basis(simu_be_t400, observed).
narrative_ontology:measurement(simu_be_t600, simultaneous_veneration__ontological_fusion_reading, base_extractiveness, 600, 0.65).
narrative_ontology:measurement_basis(simu_be_t600, observed).
narrative_ontology:measurement(simu_be_t800, simultaneous_veneration__ontological_fusion_reading, base_extractiveness, 800, 0.7).
narrative_ontology:measurement_basis(simu_be_t800, observed).
narrative_ontology:measurement(simu_be_t1000, simultaneous_veneration__ontological_fusion_reading, base_extractiveness, 1000, 0.71).
narrative_ontology:measurement_basis(simu_be_t1000, observed).
narrative_ontology:measurement(simu_be_t1200, simultaneous_veneration__ontological_fusion_reading, base_extractiveness, 1200, 0.71).
narrative_ontology:measurement_basis(simu_be_t1200, observed).

% Suppression requirement over time
narrative_ontology:measurement(simu_su_t0, simultaneous_veneration__ontological_fusion_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement_basis(simu_su_t0, observed).
narrative_ontology:measurement(simu_su_t200, simultaneous_veneration__ontological_fusion_reading, suppression_requirement, 200, 0.4).
narrative_ontology:measurement_basis(simu_su_t200, observed).
narrative_ontology:measurement(simu_su_t400, simultaneous_veneration__ontological_fusion_reading, suppression_requirement, 400, 0.5).
narrative_ontology:measurement_basis(simu_su_t400, observed).
narrative_ontology:measurement(simu_su_t600, simultaneous_veneration__ontological_fusion_reading, suppression_requirement, 600, 0.58).
narrative_ontology:measurement_basis(simu_su_t600, observed).
narrative_ontology:measurement(simu_su_t800, simultaneous_veneration__ontological_fusion_reading, suppression_requirement, 800, 0.62).
narrative_ontology:measurement_basis(simu_su_t800, observed).
narrative_ontology:measurement(simu_su_t1000, simultaneous_veneration__ontological_fusion_reading, suppression_requirement, 1000, 0.62).
narrative_ontology:measurement_basis(simu_su_t1000, observed).
narrative_ontology:measurement(simu_su_t1200, simultaneous_veneration__ontological_fusion_reading, suppression_requirement, 1200, 0.62).
narrative_ontology:measurement_basis(simu_su_t1200, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(simultaneous_veneration__ontological_fusion_reading, identity_coordination).
narrative_ontology:affects_constraint(simultaneous_veneration__ontological_fusion_reading, domain_partition_reading).
narrative_ontology:affects_constraint(simultaneous_veneration__ontological_fusion_reading, pragmatic_incoherence_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the simultaneous_veneration kernel. domain_partition_reading claims only functional specialization (lower epsilon, no interpretive monopoly claim). pragmatic_incoherence_reading denies a stable claim was ever held, attributing persistence to absent enforcement pressure. ontological_fusion_reading (this story) claims the strongest position — literal metaphysical identity — which grounds the highest epsilon and the clearest beneficiary/victim asymmetry, since a truth-claim of identity is what licenses one tradition's clergy to hold interpretive authority over another's deities. The three stories share the same historical substrate (honji-suijaku practice and jingu-ji institutions) but diverge sharply in what they claim was actually happening metaphysically, which is why they are authored as separate constraints per the ε-invariance principle rather than as one story with a measurement parameter.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
