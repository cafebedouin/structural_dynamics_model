% ============================================================================
% CONSTRAINT STORY: simultaneous_veneration__ontological_fusion_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
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
 *   constraint_id: simultaneous_veneration__ontological_fusion_reading
 *   human_readable: Honji-Suijaku Ontological Fusion Doctrine (Buddha-Kami Identity Reading)
 *   domain: religious/institutional/Japanese_history
 *
 * SUMMARY:
 *   This story authors the ONTOLOGICAL FUSION reading of the honji-suijaku
 *   (本地垂迹) kernel: the claim that kami and buddhas are not merely coexisting
 *   or functionally partitioned but are ONE metaphysical reality apprehended
 *   through different cultural-historical lenses, with buddhas/bodhisattvas
 *   as the 'original ground' (honji) and kami as their 'manifest traces'
 *   (suijaku). This reading treats honji-suijaku theory as capturing literal
 *   metaphysical truth, not as a diplomatic fiction or a
 *   domain-specialization convention. Under this reading, the doctrine
 *   functions as a coordination device (it lets Buddhist institutions and
 *   kami cults share ritual space without doctrinal war) that simultaneously
 *   enacts an asymmetric ontological ranking: kami are always the 'trace,'
 *   never the 'ground,' which structurally subordinates indigenous cosmology
 *   to an imported metaphysical hierarchy administered by Buddhist
 *   institutions. This is a Tangled Rope, not a Rope: the coordination
 *   function is real (it did reduce religious conflict during Buddhism's
 *   expansion) but it rides on an asymmetric extraction (interpretive
 *   authority, land, and ritual precedence flow one direction) that requires
 *   active enforcement — administrative fusion of shrine-temple complexes,
 *   doctrinal instruction, and (per the founding-problem corroboration)
 *   eventual forcible reversal at the Meiji separation, which is itself
 *   evidence that the arrangement was maintained rather than simply true.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(simultaneous_veneration__ontological_fusion_reading, 0.71).
domain_priors:suppression_score(simultaneous_veneration__ontological_fusion_reading, 0.62).
domain_priors:theater_ratio(simultaneous_veneration__ontological_fusion_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(simultaneous_veneration__ontological_fusion_reading, extractiveness, 0.71).
narrative_ontology:constraint_metric(simultaneous_veneration__ontological_fusion_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(simultaneous_veneration__ontological_fusion_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(simultaneous_veneration__ontological_fusion_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(simultaneous_veneration__ontological_fusion_reading, resistance, 0.47).

% --- Constraint claim ---
narrative_ontology:constraint_claim(simultaneous_veneration__ontological_fusion_reading, tangled_rope).
narrative_ontology:human_readable(simultaneous_veneration__ontological_fusion_reading, "Honji-Suijaku Ontological Fusion Doctrine (Buddha-Kami Identity Reading)").
narrative_ontology:topic_domain(simultaneous_veneration__ontological_fusion_reading, "religious/institutional/Japanese_history").

domain_priors:requires_active_enforcement(simultaneous_veneration__ontological_fusion_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(simultaneous_veneration__ontological_fusion_reading, 'cbd90c70-b807-4b0e-b853-945a1fd2cbfc').
narrative_ontology:cs_kernel_codification('cbd90c70-b807-4b0e-b853-945a1fd2cbfc', distributed).
narrative_ontology:cs_authority_grounding('cbd90c70-b807-4b0e-b853-945a1fd2cbfc', lineage).
narrative_ontology:cs_interpretation_layer_present('cbd90c70-b807-4b0e-b853-945a1fd2cbfc').
narrative_ontology:cs_reading_relation('cbd90c70-b807-4b0e-b853-945a1fd2cbfc', simultaneous_veneration__domain_partition_reading, forecloses).
narrative_ontology:cs_reading_relation('cbd90c70-b807-4b0e-b853-945a1fd2cbfc', simultaneous_veneration__pragmatic_incoherence_reading, forecloses).
narrative_ontology:cs_axiom('cbd90c70-b807-4b0e-b853-945a1fd2cbfc', foundational, kami_buddha_numerical_identity).
narrative_ontology:cs_axiom_status(kami_buddha_numerical_identity, holdable).
narrative_ontology:cs_axiom_grounding('cbd90c70-b807-4b0e-b853-945a1fd2cbfc', kami_buddha_numerical_identity, theological).
narrative_ontology:cs_axiom('cbd90c70-b807-4b0e-b853-945a1fd2cbfc', foundational, honji_ontologically_prior_to_suijaku).
narrative_ontology:cs_axiom_status(honji_ontologically_prior_to_suijaku, holdable).
narrative_ontology:cs_axiom_grounding('cbd90c70-b807-4b0e-b853-945a1fd2cbfc', honji_ontologically_prior_to_suijaku, theological).
narrative_ontology:cs_reference_frame('cbd90c70-b807-4b0e-b853-945a1fd2cbfc', heian_combinatory_orthodoxy).
narrative_ontology:cs_drift_state('cbd90c70-b807-4b0e-b853-945a1fd2cbfc', post_meiji_shinbutsu_bunri, gap(repudiation_pressure, severe, true)).
narrative_ontology:cs_created_at('cbd90c70-b807-4b0e-b853-945a1fd2cbfc', '').
narrative_ontology:cs_kernel_id(simultaneous_veneration__ontological_fusion_reading, simultaneous_veneration).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(simultaneous_veneration__ontological_fusion_reading, buddhist_institutional_hierarchy).
narrative_ontology:constraint_beneficiary(simultaneous_veneration__ontological_fusion_reading, shingon_tendai_temple_networks).
narrative_ontology:constraint_victim(simultaneous_veneration__ontological_fusion_reading, kami_cult_autonomy).
narrative_ontology:constraint_victim(simultaneous_veneration__ontological_fusion_reading, local_shrine_lineages).
narrative_ontology:constraint_victim(simultaneous_veneration__ontological_fusion_reading, non_doctrinal_kami_practitioners).
narrative_ontology:constraint_vindicates(simultaneous_veneration__ontological_fusion_reading, honji_suijaku_metaphysical_truth_claim).
narrative_ontology:constraint_vindicates(simultaneous_veneration__ontological_fusion_reading, buddhist_cosmological_completeness).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Major temple-shrine complexes (jingu-ji) and their doctrinal specialists formulate and enforce the honji-suijaku identity claim, ranking kami as local manifestations (suijaku) of buddhas and bodhisattvas (honji). They administer combinatory shrine-temple institutions, control ordination and doctrinal instruction, and receive tribute, land grants, and patronage that flow through the fused cosmology they authored. They can revise the ranking hierarchy at will since they hold the interpretive apparatus.
narrative_ontology:constraint_stakeholder(simultaneous_veneration__ontological_fusion_reading, buddhist_institutional_hierarchy, agenda_setter,
    institutional, civilizational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(simultaneous_veneration__ontological_fusion_reading, buddhist_institutional_hierarchy, beneficiary).

% Esoteric Buddhist schools extend their cosmological and ritual technology (mandalas, initiation rites) over kami worship sites, absorbing shrine revenue and personnel into temple administrative structures. They benefit from the claim that a single metaphysical order underlies all local cults, since this justifies temple oversight of shrines as a matter of doctrinal necessity rather than political convenience.
narrative_ontology:constraint_stakeholder(simultaneous_veneration__ontological_fusion_reading, shingon_tendai_temple_networks, beneficiary,
    institutional, generational, arbitrage, national).

% Kami cults with independent cosmologies, myths, and ritual practices predating systematic Buddhist absorption find their deities reclassified as provisional or lesser manifestations of buddhas. Their own accounts of what a kami is and does are subordinated to a foreign metaphysical schema. Practitioners can continue local rites, but only within a hierarchy that treats their tradition's self-understanding as incomplete or preparatory.
narrative_ontology:constraint_stakeholder(simultaneous_veneration__ontological_fusion_reading, kami_cult_autonomy, payer,
    moderate, generational, constrained, regional).

% Hereditary shrine priests (kannushi) at smaller shrines depend on temple networks for legitimacy, funding, and protection once combinatory institutions dominate the religious landscape. Refusing the honji-suijaku framing risks loss of patronage, administrative marginalization, or absorption without consultation. Their local knowledge of the kami's meaning is overwritten by the imported ranking system.
narrative_ontology:constraint_stakeholder(simultaneous_veneration__ontological_fusion_reading, local_shrine_lineages, payer,
    powerless, biographical, trapped, local).

% Ordinary villagers whose relationship to the kami is practical and unsystematized (harvest rites, purification, local protection) have no voice in the doctrinal debate about what their kami 'really' is. Their lived practice does not require or produce a metaphysical identity claim, but the fusion doctrine is imposed as the authoritative account of what they have always been doing.
narrative_ontology:constraint_stakeholder(simultaneous_veneration__ontological_fusion_reading, non_doctrinal_kami_practitioners, excluded,
    powerless, biographical, trapped, local).

% Later political actors who forcibly separated kami and buddhas (shinbutsu bunri, 1868) treat the fusion doctrine as an artificial historical accretion to be dismantled, not a metaphysical discovery. Their own agenda (state-sponsored Shinto nationalism) is not neutral, but their intervention supplies outside testimony that the fusion claim was administratively constructed rather than simply found.
narrative_ontology:constraint_stakeholder(simultaneous_veneration__ontological_fusion_reading, meiji_state_shinto_reformers, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single interpretive framework allowing Buddhist temple institutions and pre-existing kami cults to coexist within one ritual and administrative system rather than compete as rival, mutually exclusive religions — reducing conflict over sacred sites, land, and legitimacy during Buddhism's expansion into regions with entrenched kami worship.
% TRANSFER_FUNCTION: Moves interpretive authority, ritual precedence, land revenue, and patronage from independent kami shrine lineages to combinatory shrine-temple institutions administered by Buddhist clergy, under the claim that the transfer merely reveals a pre-existing metaphysical unity rather than redistributing religious authority.
% ABSENT_VOICES: Non-doctrinal kami practitioners and most local shrine lineages left no systematic counter-doctrine; their practice was oral, localized, and non-metaphysical, so the ontological fusion claim was formulated entirely within Buddhist scholastic institutions without their participation. Where kami-side self-articulation survives (in norito prayers, local myth-cycles), it does not employ or require the honji-suijaku framework at all.
% DISAPPEARANCE_RATIONALE: If the ontological fusion doctrine were withdrawn, combinatory shrine-temple institutions would lose their metaphysical warrant for administrative fusion; land and revenue currently routed through jingu-ji complexes would need re-justification or would revert to independent shrine control, as in fact occurred (imperfectly and coercively) at the Meiji separation.
% FOUNDING_PROBLEM: Buddhism arriving in Japan needed to explain its relationship to already-entrenched, locally powerful kami cults without either declaring them false (provoking resistance) or admitting a rival, equally valid cosmology (undermining Buddhist claims to universal truth).
% FOUNDING_PROBLEM_CORROBORATION: Meiji-era state Shinto reformers and modern historians of Japanese religion (outside both the Buddhist institutional hierarchy and kami shrine lineages) attest that the doctrinal need to manage Buddhist-kami coexistence during the Nara-Heian expansion no longer exists as a live problem; the doctrine's persistence in surviving combinatory sites is documented as institutional inertia and tourism/heritage framing rather than active metaphysical adjudication. No source outside the Buddhist hierarchy attests the identity claim as an ongoing live metaphysical necessity.
narrative_ontology:disappearance_verdict(simultaneous_veneration__ontological_fusion_reading, world_rearranges).
narrative_ontology:founding_problem_status(simultaneous_veneration__ontological_fusion_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(simultaneous_veneration__ontological_fusion_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
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
 *   Extractiveness (0.71) reflects that the fusion claim consistently produces one-directional institutional consequences: temple networks absorb shrine administration and revenue, never the reverse. Suppression (0.62) reflects the active doctrinal and administrative work required to maintain the ranking (jingu-ji institution-building, doctrinal instruction overriding local kami self-understanding) rather than mere passive coexistence. Theater ratio rises over the interval (0.12 to 0.38) as the doctrine's coordination function reduces in practical necessity — Buddhist institutional dominance becomes established fact rather than a live negotiation — while doctrinal maintenance and ritual performance of the fusion (mandala representations of kami as buddha-manifestations, combinatory liturgy) persists as institutional signaling. Accessibility collapse (0.58) is moderate rather than near-total: local practice continued to exist alongside the doctrine, but the METAPHYSICAL framing of what kami 'really are' became difficult to articulate outside the fusion vocabulary once it became institutionally dominant. Resistance (0.47) reflects real but uneven pushback — some kokugaku (nativist) scholars and eventually the Meiji state mounted direct challenges, but ordinary shrine practitioners had little capacity to resist doctrinal reclassification.
 *
 * DIRECTIONALITY LOGIC:
 *   The Buddhist institutional hierarchy and temple networks sit at the beneficiary end: they author the fusion doctrine, administer its institutional consequences, and collect its material benefits (land, tribute, ordination authority) — d near full-beneficiary. Kami cult autonomy and local shrine lineages sit at the target end: their cosmology is reclassified as provisional/partial without their participation, and their institutional standing becomes dependent on temple validation — d near full-target, particularly for local shrine lineages whose exit options are trapped (dependent on patronage networks they do not control). Non-doctrinal practitioners are excluded rather than coordinated: the metaphysical debate happens entirely above their heads, in a scholastic register their practice never required.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (managing Buddhist-kami coexistence during Nara-Heian era religious pluralism) is dead by external corroboration (Meiji reformers, modern historians), yet remnant combinatory institutions and doctrinal defenses of honji-suijaku's 'truth' persist in some contemporary sectarian contexts as a claim about metaphysical reality rather than as historical religious diplomacy. The Tangled Rope classification prevents mislabeling this as pure Snare (it did solve a genuine coordination problem in its founding period) or as pure Rope (the coordination was never symmetric — kami were structurally always the lesser term). The classification also resists reading it as inevitable natural fact (which the fusion doctrine's own metaphysical framing invites) by insisting on the beneficiary/victim asymmetry as a structural, not merely interpretive, feature.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    metaphysical_truth_vs_institutional_convenience,
    'Did premodern Japanese Buddhist theorists who articulated honji-suijaku genuinely believe it as metaphysical truth, or was it consciously understood (at least by some) as institutional accommodation dressed in metaphysical language?',
    'Close textual analysis of doctrinal treatises (e.g. Ryobu Shinto texts, Tendai/Shingon commentaries) for internal markers distinguishing sincere ontological commitment from strategic syncretism; comparison with contemporaneous debates where the ranking was explicitly contested by kami-affiliated scholars.',
    'If demonstrably strategic, the ontological_fusion_reading''s claimed_type moves further toward snare (the metaphysical claim is pure cover for institutional absorption); if demonstrably sincere doctrinal belief held even by disinterested theologians, the coordination function is more genuine and the tangled_rope classification''s coordination half is better supported.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(metaphysical_truth_vs_institutional_convenience, conceptual, 'Whether the fusion doctrine was sincere metaphysics or strategic institutional cover.').

omega_variable(
    kernel_reading_incommensurability,
    'Are the three kernel readings (fusion, partition, incoherence) genuinely describing the same historical phenomenon, or does each pick out a different subset of practices/periods/regions that got collapsed under the single label ''honji-suijaku''?',
    'Regional and temporal disaggregation of combinatory shrine-temple practice: does the fusion doctrine dominate in some periods/schools (e.g. medieval Tendai) while partition-style reasoning dominates in others (e.g. certain Yoshida Shinto strands), suggesting the kernel itself is a retrospective unification of distinct historical arrangements?',
    'If the three readings map to genuinely distinct historical practices rather than three interpretations of one practice, the kernel model itself may be doing the collapsing work that the ε-invariance principle warns against — in which case this is not three readings of one kernel but three separate historical constraints wearing one modern label (''honji-suijaku'').',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_incommensurability, conceptual, 'Whether the kernel unifies genuinely distinct historical arrangements under one retrospective label.').

omega_variable(
    kami_autonomy_counterfactual,
    'Absent Buddhist doctrinal absorption, would indigenous kami cults have developed their own systematic theology, or does the fusion doctrine''s imposition of a metaphysical framework represent the only historical route by which kami worship acquired doctrinal systematicity at all?',
    'Comparative study of kami worship in regions/periods with minimal Buddhist institutional penetration, examining whether independent systematization occurred.',
    'If kami cults would likely have remained non-systematic without Buddhist contact, part of what is coded here as ''extraction'' (the imposition of metaphysical rank) is inseparable from the only historical process that gave kami worship a theology at all — complicating the victim/beneficiary framing without eliminating the institutional asymmetry.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kami_autonomy_counterfactual, empirical, 'Whether doctrinal systematization of kami cults could have occurred independent of Buddhist absorption.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(simultaneous_veneration__ontological_fusion_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(simu_tr_t0, simultaneous_veneration__ontological_fusion_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(simu_tr_t20, simultaneous_veneration__ontological_fusion_reading, theater_ratio, 20, 0.18).
narrative_ontology:measurement(simu_tr_t40, simultaneous_veneration__ontological_fusion_reading, theater_ratio, 40, 0.25).
narrative_ontology:measurement(simu_tr_t60, simultaneous_veneration__ontological_fusion_reading, theater_ratio, 60, 0.31).
narrative_ontology:measurement(simu_tr_t80, simultaneous_veneration__ontological_fusion_reading, theater_ratio, 80, 0.35).
narrative_ontology:measurement(simu_tr_t100, simultaneous_veneration__ontological_fusion_reading, theater_ratio, 100, 0.38).

% Extraction over time
narrative_ontology:measurement(simu_be_t0, simultaneous_veneration__ontological_fusion_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(simu_be_t20, simultaneous_veneration__ontological_fusion_reading, base_extractiveness, 20, 0.55).
narrative_ontology:measurement(simu_be_t40, simultaneous_veneration__ontological_fusion_reading, base_extractiveness, 40, 0.63).
narrative_ontology:measurement(simu_be_t60, simultaneous_veneration__ontological_fusion_reading, base_extractiveness, 60, 0.68).
narrative_ontology:measurement(simu_be_t80, simultaneous_veneration__ontological_fusion_reading, base_extractiveness, 80, 0.7).
narrative_ontology:measurement(simu_be_t100, simultaneous_veneration__ontological_fusion_reading, base_extractiveness, 100, 0.71).

% Suppression requirement over time
narrative_ontology:measurement(simu_su_t0, simultaneous_veneration__ontological_fusion_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(simu_su_t20, simultaneous_veneration__ontological_fusion_reading, suppression_requirement, 20, 0.44).
narrative_ontology:measurement(simu_su_t40, simultaneous_veneration__ontological_fusion_reading, suppression_requirement, 40, 0.52).
narrative_ontology:measurement(simu_su_t60, simultaneous_veneration__ontological_fusion_reading, suppression_requirement, 60, 0.58).
narrative_ontology:measurement(simu_su_t80, simultaneous_veneration__ontological_fusion_reading, suppression_requirement, 80, 0.6).
narrative_ontology:measurement(simu_su_t100, simultaneous_veneration__ontological_fusion_reading, suppression_requirement, 100, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(simultaneous_veneration__ontological_fusion_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(simultaneous_veneration__ontological_fusion_reading, 0.1).
narrative_ontology:affects_constraint(simultaneous_veneration__ontological_fusion_reading, domain_partition_reading).
narrative_ontology:affects_constraint(simultaneous_veneration__ontological_fusion_reading, pragmatic_incoherence_reading).

% DUAL FORMULATION NOTE:
% Three constraints decompose the natural-language label 'honji-suijaku theory' / 'the kami-buddha relationship' per the ε-invariance principle: this story (ontological_fusion_reading, high ε, tangled_rope — ranked metaphysical identity with institutional asymmetry), domain_partition_reading (lower ε expected — functional specialization without ontological ranking, closer to genuine rope), and pragmatic_incoherence_reading (ε reflecting unenforced contradiction rather than doctrinal extraction — likely piton or low-extraction classification given absence of active enforcement until Meiji forced resolution). Each reading is a distinct structural claim about the same historical kernel (simultaneous_veneration), not the same constraint viewed through different observables; they are linked here because Meiji-era shinbutsu bunri separation policy responded to all three readings at once by abolishing the combinatory institutions regardless of which reading any given practitioner held.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
