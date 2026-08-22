% ============================================================================
% CONSTRAINT STORY: kami_buddha_ontology__honji_suijaku_monism
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_kami_buddha_ontology__honji_suijaku_monism, []).

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
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: kami_buddha_ontology__honji_suijaku_monism
 *   human_readable: Honji Suijaku Monism — Kami as Phenomenal Traces of Buddhist Ground
 *   domain: religious_studies/philosophy_of_religion/japanese_cultural_history
 *
 * SUMMARY:
 *   Honji suijaku (original ground, manifest traces) is the dominant
 *   theoretical framework of medieval Japanese religion (c. 750–1868),
 *   asserting that kami are local manifestations (suijaku) of
 *   buddhas/bodhisattvas who are the true ontological ground (honji).
 *   Originating in Tendai and Shingon esoteric systematizations, it became
 *   the operating theology of shrine-temple complexes (jingū-ji), imperial
 *   ritual, and monastic education. The constraint coordinates the entire
 *   religious field under a single hierarchical ontology while extracting
 *   authority, land, and interpretive control from indigenous kami lineages.
 *   Its persistence long after its founding integration problem dissolved
 *   makes it a tangled rope: genuine coordination (a unified metaphysical map
 *   for a fragmented archipelago) fused with asymmetric extraction (Buddhist
 *   institutions as permanent beneficiaries, kami lineages as permanent
 *   payers). The Meiji shinbutsu bunri (1868) violently dismantled it,
 *   confirming the world_rearranges verdict.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(kami_buddha_ontology__honji_suijaku_monism, 0.68).
domain_priors:suppression_score(kami_buddha_ontology__honji_suijaku_monism, 0.72).
domain_priors:theater_ratio(kami_buddha_ontology__honji_suijaku_monism, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(kami_buddha_ontology__honji_suijaku_monism, extractiveness, 0.68).
narrative_ontology:constraint_metric(kami_buddha_ontology__honji_suijaku_monism, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(kami_buddha_ontology__honji_suijaku_monism, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(kami_buddha_ontology__honji_suijaku_monism, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(kami_buddha_ontology__honji_suijaku_monism, resistance, 0.63).

% --- Constraint claim ---
narrative_ontology:constraint_claim(kami_buddha_ontology__honji_suijaku_monism, tangled_rope).
narrative_ontology:human_readable(kami_buddha_ontology__honji_suijaku_monism, "Honji Suijaku Monism — Kami as Phenomenal Traces of Buddhist Ground").
narrative_ontology:topic_domain(kami_buddha_ontology__honji_suijaku_monism, "religious_studies/philosophy_of_religion/japanese_cultural_history").

domain_priors:requires_active_enforcement(kami_buddha_ontology__honji_suijaku_monism).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(kami_buddha_ontology__honji_suijaku_monism, 'befebbf8-c753-4905-b9d8-046e77385a30').
narrative_ontology:cs_kernel_codification('befebbf8-c753-4905-b9d8-046e77385a30', formalized).
narrative_ontology:cs_authority_grounding('befebbf8-c753-4905-b9d8-046e77385a30', lineage).
narrative_ontology:cs_interpretation_layer_present('befebbf8-c753-4905-b9d8-046e77385a30').
narrative_ontology:cs_reading_relation('befebbf8-c753-4905-b9d8-046e77385a30', kami_buddha_ontology__domain_partition, coexists_with).
narrative_ontology:cs_reading_relation('befebbf8-c753-4905-b9d8-046e77385a30', kami_buddha_ontology__incoherent_bundle, influences).
narrative_ontology:cs_axiom('befebbf8-c753-4905-b9d8-046e77385a30', foundational, buddha_as_sole_ontological_ground).
narrative_ontology:cs_axiom_status(buddha_as_sole_ontological_ground, holdable).
narrative_ontology:cs_axiom_grounding('befebbf8-c753-4905-b9d8-046e77385a30', buddha_as_sole_ontological_ground, deontological).
narrative_ontology:cs_axiom('befebbf8-c753-4905-b9d8-046e77385a30', foundational, theoretical_systematization_as_soteriological_necessity).
narrative_ontology:cs_axiom_status(theoretical_systematization_as_soteriological_necessity, holdable).
narrative_ontology:cs_axiom_grounding('befebbf8-c753-4905-b9d8-046e77385a30', theoretical_systematization_as_soteriological_necessity, instrumental).
narrative_ontology:cs_reference_frame('befebbf8-c753-4905-b9d8-046e77385a30', heian_esoteric_integration).
narrative_ontology:cs_drift_state('befebbf8-c753-4905-b9d8-046e77385a30', kamakura_systematization, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('befebbf8-c753-4905-b9d8-046e77385a30', '2026-08-15T14:32:17Z').
narrative_ontology:cs_kernel_id(kami_buddha_ontology__honji_suijaku_monism, kami_buddha_ontology).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(kami_buddha_ontology__honji_suijaku_monism, buddhist_institutional_hierarchy).
narrative_ontology:constraint_beneficiary(kami_buddha_ontology__honji_suijaku_monism, monastic_scholastic_traditions).
narrative_ontology:constraint_beneficiary(kami_buddha_ontology__honji_suijaku_monism, imperial_court_buddhist_factions).
narrative_ontology:constraint_victim(kami_buddha_ontology__honji_suijaku_monism, indigenous_kami_ritual_lineages).
narrative_ontology:constraint_victim(kami_buddha_ontology__honji_suijaku_monism, local_shrine_communities).
narrative_ontology:constraint_victim(kami_buddha_ontology__honji_suijaku_monism, kami_centered_practice_holders).
narrative_ontology:constraint_vindicates(kami_buddha_ontology__honji_suijaku_monism, buddhist_ontological_priority).
narrative_ontology:constraint_vindicates(kami_buddha_ontology__honji_suijaku_monism, hierarchical_universalism).
narrative_ontology:constraint_vindicates(kami_buddha_ontology__honji_suijaku_monism, theoretical_systematization_as_liberation).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Controls the doctrinal interpretation of kami-buddha relations through temple networks, monastic education, and imperially sanctioned syncretic systems. Gains legitimacy, patronage, and structural authority by positioning Buddhist ontology as the universal ground. Can deploy state support to enforce doctrinal coherence.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__honji_suijaku_monism, buddhist_institutional_hierarchy, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(kami_buddha_ontology__honji_suijaku_monism, buddhist_institutional_hierarchy, beneficiary).

% Produce and transmit the honji suijaku theoretical corpus (Ryōbu Shintō, Sannō Ichijitsu, etc.). Their scholarly identity and institutional position are fused with the systematization project; exit means abandoning the intellectual framework that constitutes their vocation. Benefit from the constraint's demand for continuous doctrinal refinement.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__honji_suijaku_monism, monastic_scholastic_traditions, beneficiary,
    organized, biographical, identity_locked, regional).

% Use honji suijaku as a political theology to integrate local cults into a unified imperial cosmology. The constraint legitimizes court authority over both Buddhist and kami ritual spheres. Exit is constrained by the institutional embeddedness of this theology in court ritual and appointment systems.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__honji_suijaku_monism, imperial_court_buddhist_factions, beneficiary,
    institutional, generational, constrained, national).

% Bear the cost of ontological subordination: their deities are redefined as mere traces of Buddhist originals, their ritual autonomy is absorbed into temple-shrine complexes (jingū-ji), and their hereditary priestly authority is overridden by Buddhist clerics. Exit is trapped — the constraint reshapes the very categories through which their tradition understands itself.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__honji_suijaku_monism, indigenous_kami_ritual_lineages, payer,
    moderate, generational, trapped, local).
narrative_ontology:stakeholder_secondary_role(kami_buddha_ontology__honji_suijaku_monism, indigenous_kami_ritual_lineages, excluded).

% Experience the constraint through material reorganization: shrine lands reassigned to temples, festivals restructured around Buddhist calendars, communal identity reframed as provisional. Their exit options are constrained by geographic embeddedness and the constraint's penetration of daily ritual life. Some resist through cryptic preservation of pre-syncretic forms.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__honji_suijaku_monism, local_shrine_communities, payer,
    powerless, biographical, constrained, local).

% Practitioners for whom kami are primary and self-subsisting. The constraint demands they reinterpret their direct experience as derivative. Identity-locked because their practice constitutes their self-understanding; to accept honji suijaku is to dissolve the ground of their practice. Some develop counter-systematizations (e.g., suijaku honji inversion) but remain within the constraint's conceptual gravity.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__honji_suijaku_monism, kami_centered_practice_holders, payer,
    moderate, biographical, identity_locked, regional).

% Meiji-era and later thinkers who construct State Shinto by violently rejecting honji suijaku. They are excluded from the constraint's operational period but define themselves against it. Their exit is mobile — they build a new institutional order — but their entire project is a reaction formation to this constraint's historical dominance.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__honji_suijaku_monism, modern_shinto_nationalist_theologians, excluded,
    organized, generational, mobile, national).

% Analyze the constraint as a case study in religious syncretism, theoretical imperialism, and the politics of ontological categorization. No structural stake in the constraint's operation; their exit is analytical by definition.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__honji_suijaku_monism, comparative_religion_scholars, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single metaphysical framework that integrates the entire Japanese religious field — every kami, every buddha, every ritual, every institution — into one hierarchical ontology centered on Buddhist enlightenment as the ultimate referent. Solves the coordination problem of pluralistic religious practice by subsuming it under a master system.
% TRANSFER_FUNCTION: Moves ontological priority, ritual authority, land tenure, and interpretive control from indigenous kami lineages to Buddhist institutions. The transfer is not merely symbolic: it reorganizes the material economy of sacred sites, the hereditary transmission of priestly office, and the cognitive categories through which communities experience the sacred.
% ABSENT_VOICES: Pre-systematized local cultic practices that left no textual record; women's ritual traditions (miko, itako) that operated outside both Buddhist monastic and shrine priestly structures; the dead whose post-mortem care was reorganized by the constraint. These voices are absent because the constraint's theoretical apparatus requires literacy, institutional position, and doctrinal articulation to be heard.
% DISAPPEARANCE_RATIONALE: If honji suijaku vanished overnight, the entire institutional architecture of medieval Japanese religion — temple-shrine complexes, doctrinal curricula, imperial ritual calendars, the legal status of shrine lands — would lose its organizing principle. Communities would revert to kami-centered autonomy or fragment into competing systematizations. The Meiji separation (shinbutsu bunri) is historical evidence: the constraint's collapse triggered violent rearrangement.
% FOUNDING_PROBLEM: How to integrate the proliferating, locally autonomous kami cults of the archipelago into a universal soteriological framework without denying their efficacy or the authority of the Buddhist institutions that claimed to offer final liberation.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem is dead: the historical conditions that made honji suijaku a live solution — fragmented local cults needing integration into a Buddhist universalism — no longer obtain. Modern scholars (Teeuwen, Rambelli, Faure) outside the Buddhist institutional lineage confirm the problem was historically contingent. The constraint persisted centuries after its founding problem dissolved, maintained by institutional inertia and the identity-lock of scholastic traditions.
narrative_ontology:disappearance_verdict(kami_buddha_ontology__honji_suijaku_monism, world_rearranges).
narrative_ontology:founding_problem_status(kami_buddha_ontology__honji_suijaku_monism, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(kami_buddha_ontology__honji_suijaku_monism, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(kami_buddha_ontology__honji_suijaku_monism, 'none', 1).
narrative_ontology:epsilon_provenance(kami_buddha_ontology__honji_suijaku_monism, 0.68, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(kami_buddha_ontology__honji_suijaku_monism_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(kami_buddha_ontology__honji_suijaku_monism, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(kami_buddha_ontology__honji_suijaku_monism_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68) reflects the structural transfer of ontological priority and material resources from kami-centered to Buddhist-centered institutions. Suppression (0.72) is high because the constraint requires active doctrinal enforcement (monastic curricula, imperially sanctioned temple-shrine administration, textual polemics against kami-centered views) and because alternatives are cognitively collapsed — the very categories of 'kami' and 'buddha' are reshaped by the theory. Theater ratio (0.41) is substantial: by the Kamakura period, much scholastic energy goes into elaborating honji suijaku correspondences (which buddha for which kami) rather than the original integration task. Accessibility collapse (0.58) and resistance (0.63) are moderate — the constraint never fully eliminates kami-centered experience, and counter-systematizations (suijaku honji, Yoshida Shintō) persist.
 *
 * PERSPECTIVAL GAP:
 *   From the Buddhist agenda-setter seat, honji suijaku appears as compassionate skillful means (upāya): the Buddha adapts to local conditions to lead beings to liberation. From the kami-lineage payer seat, it appears as ontological colonization: their deities are demoted, their authority usurped, their ritual language colonized by Sanskrit terminology. The engine computes this divergence from the structural data — the constraint's coordination function is real from the beneficiary side, its extraction real from the payer side.
 *
 * DIRECTIONALITY LOGIC:
 *   Buddhist institutional hierarchy and monastic scholastics are structural beneficiaries (d ~ 0.15): they collect ontological authority, patronage, and the right to define the field. Their exit is arbitrage/identity_locked — they could theoretically abandon the system but their institutional identity is constituted by it. Imperial court factions are beneficiaries with constrained exit (d ~ 0.25): the theology serves their political cosmology but they are locked into it by ritual precedent. Indigenous kami lineages, shrine communities, and kami-centered practitioners are payers/victims (d ~ 0.85): they bear the ontological subordination and material dispossession. Their exit ranges from trapped (local communities) to identity_locked (practice holders for whom accepting the constraint dissolves their practice's ground). Modern Shinto theologians are excluded — they emerge after the interval but define themselves by total rejection.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (integrating fragmented kami cults into Buddhist universalism) was live in the early Heian period but dead by the late Kamakura — yet the constraint persisted 500+ years after. This mandatrophy is resolved not by sunset but by violent external rupture (Meiji). The constraint's persistence mechanism shifted from coordination to institutional self-reproduction: monastic curricula required honji suijaku mastery, temple-shrine complexes generated revenue from the integrated system, and the theoretical edifice became a career structure. No internal actor had both the incentive and power to dismantle it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coordination_extraction_boundary,
    'Where does the genuine coordination function (providing a unified metaphysical map for a fragmented religious field) end and the extractive function (permanent Buddhist institutional priority) begin? Are they separable in practice or only in retrospective analysis?',
    'Comparative analysis of periods when the constraint operated with lower extraction (early Heian, before full jingū-ji institutionalization) vs. higher extraction (Kamakura-Muromachi, when correspondence tables became rigid). Also: counterfactual — could a non-hierarchical integration have solved the coordination problem?',
    'If separable, the constraint is a tangled rope with a removable extractive layer; if inseparable, the coordination itself is structurally extractive — the ''map'' requires a centered hierarchy that necessarily subordinates the periphery.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_extraction_boundary, conceptual, 'Whether coordination and extraction are structurally separable in honji suijaku.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the constraint''s suppression primarily structural (institutional enforcement, land reassignment, textual authority) or internalized (kami practitioners coming to experience their own deities as derivative)?',
    'Ethnographic and textual evidence from crypto-kami traditions (kakure kirishitan parallels), folk practices that preserve pre-honji suijaku forms, and the rapidity of Meiji-era reversion to kami-centered autonomy. If suppression persists after structural enforcement is removed, it is internalized.',
    'If substantially internalized, the constraint''s effective suppression exceeds the structural measure — the payer seats carry the constraint within their own cognitive categories. This would amplify the extraction experienced by identity_locked agents.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression in honji suijaku''s operation.').

omega_variable(
    kernel_framing_underdetermination,
    'Does the kernel ''kami_buddha_ontology'' admit a single coherent framing, or do the three declared readings (honji_suijaku_monism, domain_partition, incoherent_bundle) represent genuinely different kernels that have been retroactively unified by modern scholarship?',
    'Historical analysis of whether medieval actors experienced these as readings of one kernel or as distinct commitments. If medieval Tendai monks saw honji suijaku and domain_partition as compatible (context-dependent deployment), they are not sibling readings of one kernel but tools in a toolkit.',
    'If the kernel is a modern scholarly construct, the committer frame (reading_relations, axioms) imposes a structural unity that did not exist historically. The constraint story would then model a retrospective systematization, not a lived structural relation.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_framing_underdetermination, conceptual, 'Whether the declared kernel unity is historical or scholarly.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(kami_buddha_ontology__honji_suijaku_monism, 750, 1868).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(kbhsm_tr_t750, kami_buddha_ontology__honji_suijaku_monism, theater_ratio, 750, 0.18).
narrative_ontology:measurement(kbhsm_tr_t850, kami_buddha_ontology__honji_suijaku_monism, theater_ratio, 850, 0.24).
narrative_ontology:measurement(kbhsm_tr_t950, kami_buddha_ontology__honji_suijaku_monism, theater_ratio, 950, 0.33).
narrative_ontology:measurement(kbhsm_tr_t1050, kami_buddha_ontology__honji_suijaku_monism, theater_ratio, 1050, 0.39).
narrative_ontology:measurement(kbhsm_tr_t1150, kami_buddha_ontology__honji_suijaku_monism, theater_ratio, 1150, 0.42).
narrative_ontology:measurement(kbhsm_tr_t1250, kami_buddha_ontology__honji_suijaku_monism, theater_ratio, 1250, 0.45).
narrative_ontology:measurement(kbhsm_tr_t1350, kami_buddha_ontology__honji_suijaku_monism, theater_ratio, 1350, 0.43).
narrative_ontology:measurement(kbhsm_tr_t1450, kami_buddha_ontology__honji_suijaku_monism, theater_ratio, 1450, 0.4).
narrative_ontology:measurement(kbhsm_tr_t1550, kami_buddha_ontology__honji_suijaku_monism, theater_ratio, 1550, 0.41).
narrative_ontology:measurement(kbhsm_tr_t1650, kami_buddha_ontology__honji_suijaku_monism, theater_ratio, 1650, 0.42).
narrative_ontology:measurement(kbhsm_tr_t1750, kami_buddha_ontology__honji_suijaku_monism, theater_ratio, 1750, 0.4).
narrative_ontology:measurement(kbhsm_tr_t1868, kami_buddha_ontology__honji_suijaku_monism, theater_ratio, 1868, 0.41).

% Extraction over time
narrative_ontology:measurement(kbhsm_be_t750, kami_buddha_ontology__honji_suijaku_monism, base_extractiveness, 750, 0.32).
narrative_ontology:measurement(kbhsm_be_t850, kami_buddha_ontology__honji_suijaku_monism, base_extractiveness, 850, 0.41).
narrative_ontology:measurement(kbhsm_be_t950, kami_buddha_ontology__honji_suijaku_monism, base_extractiveness, 950, 0.53).
narrative_ontology:measurement(kbhsm_be_t1050, kami_buddha_ontology__honji_suijaku_monism, base_extractiveness, 1050, 0.61).
narrative_ontology:measurement(kbhsm_be_t1150, kami_buddha_ontology__honji_suijaku_monism, base_extractiveness, 1150, 0.67).
narrative_ontology:measurement(kbhsm_be_t1250, kami_buddha_ontology__honji_suijaku_monism, base_extractiveness, 1250, 0.71).
narrative_ontology:measurement(kbhsm_be_t1350, kami_buddha_ontology__honji_suijaku_monism, base_extractiveness, 1350, 0.69).
narrative_ontology:measurement(kbhsm_be_t1450, kami_buddha_ontology__honji_suijaku_monism, base_extractiveness, 1450, 0.65).
narrative_ontology:measurement(kbhsm_be_t1550, kami_buddha_ontology__honji_suijaku_monism, base_extractiveness, 1550, 0.68).
narrative_ontology:measurement(kbhsm_be_t1650, kami_buddha_ontology__honji_suijaku_monism, base_extractiveness, 1650, 0.7).
narrative_ontology:measurement(kbhsm_be_t1750, kami_buddha_ontology__honji_suijaku_monism, base_extractiveness, 1750, 0.69).
narrative_ontology:measurement(kbhsm_be_t1868, kami_buddha_ontology__honji_suijaku_monism, base_extractiveness, 1868, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(kbhsm_su_t750, kami_buddha_ontology__honji_suijaku_monism, suppression_requirement, 750, 0.45).
narrative_ontology:measurement(kbhsm_su_t850, kami_buddha_ontology__honji_suijaku_monism, suppression_requirement, 850, 0.52).
narrative_ontology:measurement(kbhsm_su_t950, kami_buddha_ontology__honji_suijaku_monism, suppression_requirement, 950, 0.61).
narrative_ontology:measurement(kbhsm_su_t1050, kami_buddha_ontology__honji_suijaku_monism, suppression_requirement, 1050, 0.68).
narrative_ontology:measurement(kbhsm_su_t1150, kami_buddha_ontology__honji_suijaku_monism, suppression_requirement, 1150, 0.73).
narrative_ontology:measurement(kbhsm_su_t1250, kami_buddha_ontology__honji_suijaku_monism, suppression_requirement, 1250, 0.76).
narrative_ontology:measurement(kbhsm_su_t1350, kami_buddha_ontology__honji_suijaku_monism, suppression_requirement, 1350, 0.74).
narrative_ontology:measurement(kbhsm_su_t1450, kami_buddha_ontology__honji_suijaku_monism, suppression_requirement, 1450, 0.71).
narrative_ontology:measurement(kbhsm_su_t1550, kami_buddha_ontology__honji_suijaku_monism, suppression_requirement, 1550, 0.72).
narrative_ontology:measurement(kbhsm_su_t1650, kami_buddha_ontology__honji_suijaku_monism, suppression_requirement, 1650, 0.74).
narrative_ontology:measurement(kbhsm_su_t1750, kami_buddha_ontology__honji_suijaku_monism, suppression_requirement, 1750, 0.73).
narrative_ontology:measurement(kbhsm_su_t1868, kami_buddha_ontology__honji_suijaku_monism, suppression_requirement, 1868, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(kami_buddha_ontology__honji_suijaku_monism, identity_coordination).
narrative_ontology:boltzmann_floor_override(kami_buddha_ontology__honji_suijaku_monism, 0.1).
narrative_ontology:affects_constraint(kami_buddha_ontology__honji_suijaku_monism, kami_buddha_ontology__domain_partition).
narrative_ontology:affects_constraint(kami_buddha_ontology__honji_suijaku_monism, kami_buddha_ontology__incoherent_bundle).
narrative_ontology:affects_constraint(kami_buddha_ontology__honji_suijaku_monism, meiji_shinbutsu_bunri).
narrative_ontology:affects_constraint(kami_buddha_ontology__honji_suijaku_monism, state_shinto_formation).

% DUAL FORMULATION NOTE:
% This constraint (honji_suijaku_monism) and its sibling domain_partition are not two views of one constraint but two distinct constraints linked by the kernel kami_buddha_ontology. The ε-invariance principle applies: honji_suijaku_monism has high extractiveness (0.68) because it subordinates kami to Buddhist ontology; domain_partition has low extractiveness because it grants kami autonomous domains. They have different victim/beneficiary structures, different enforcement requirements, and different temporal profiles. The kernel label 'shinbutsu-shugo' conflates them.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(kami_buddha_ontology__honji_suijaku_monism, organized, 0.35).
constraint_indexing:directionality_override(kami_buddha_ontology__honji_suijaku_monism, moderate, 0.78).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
