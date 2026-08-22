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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: simultaneous_veneration__ontological_fusion_reading
 *   human_readable: Honji-Suijaku Ontological Fusion: Buddhist Interpretive Monopoly over Kami Worship
 *   domain: religious_studies/comparative_religion/japanese_history
 *
 * SUMMARY:
 *   The honji-suijaku (original nature-manifest trace) framework in Japanese
 *   religious history posits that kami are local Japanese manifestations of
 *   universal Buddhist realitiesâultimately, that kami and buddhas are
 *   ontologically identical viewed through different cultural lenses. This
 *   ontological fusion reading instantiates one constraint from the contested
 *   kernel of simultaneous veneration. Structurally, the reading functions as
 *   an enforced interpretive monopoly: the Buddhist institutional hierarchy
 *   claims the sole authority to decode kami identity through Buddhist
 *   cosmology, while indigenous shrine traditions lose autonomous theological
 *   standing. The constraint coordinates dual worship (genuine integration
 *   function) but asymmetrically extracts interpretive sovereignty from
 *   shrine priesthoods. This story authors the ontological_fusion_reading
 *   only; sibling readings (domain_partition_reading,
 *   pragmatic_incoherence_reading) are separate constraints.
 *
 * KEY AGENTS:
 *   - buddhist_institutional_hierarchy: Primary agenda-setter (institutional/arbitrage) â enforces ontological fusion and collects interpretive monopoly rents
 *   - shrine_priesthoods: Primary payer (organized/constrained) â bears the cost of lost autonomous kami theology
 *   - syncretic_laity: Beneficiary (moderate/constrained) â receives coordination benefit of unified worship framework
 *   - shinto_independence_advocates: Excluded voice (organized/trapped) â asserts kami autonomy but barred from orthodox discourse
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(simultaneous_veneration__ontological_fusion_reading, 0.82).
domain_priors:suppression_score(simultaneous_veneration__ontological_fusion_reading, 0.78).
domain_priors:theater_ratio(simultaneous_veneration__ontological_fusion_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(simultaneous_veneration__ontological_fusion_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(simultaneous_veneration__ontological_fusion_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(simultaneous_veneration__ontological_fusion_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(simultaneous_veneration__ontological_fusion_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(simultaneous_veneration__ontological_fusion_reading, resistance, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(simultaneous_veneration__ontological_fusion_reading, tangled_rope).
narrative_ontology:human_readable(simultaneous_veneration__ontological_fusion_reading, "Honji-Suijaku Ontological Fusion: Buddhist Interpretive Monopoly over Kami Worship").
narrative_ontology:topic_domain(simultaneous_veneration__ontological_fusion_reading, "religious_studies/comparative_religion/japanese_history").

domain_priors:requires_active_enforcement(simultaneous_veneration__ontological_fusion_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(simultaneous_veneration__ontological_fusion_reading, '89ff1b3f-e501-48c4-bdd1-ba3e5edfdae1').
narrative_ontology:cs_kernel_codification('89ff1b3f-e501-48c4-bdd1-ba3e5edfdae1', fixed_text).
narrative_ontology:cs_authority_grounding('89ff1b3f-e501-48c4-bdd1-ba3e5edfdae1', lineage).
narrative_ontology:cs_interpretation_layer_present('89ff1b3f-e501-48c4-bdd1-ba3e5edfdae1').
narrative_ontology:cs_reading_relation('89ff1b3f-e501-48c4-bdd1-ba3e5edfdae1', simultaneous_veneration__domain_partition_reading, forecloses).
narrative_ontology:cs_reading_relation('89ff1b3f-e501-48c4-bdd1-ba3e5edfdae1', simultaneous_veneration__pragmatic_incoherence_reading, coexists_with).
narrative_ontology:cs_axiom('89ff1b3f-e501-48c4-bdd1-ba3e5edfdae1', foundational, kami_buddha_ontological_identity).
narrative_ontology:cs_axiom_status(kami_buddha_ontological_identity, holdable).
narrative_ontology:cs_axiom_grounding('89ff1b3f-e501-48c4-bdd1-ba3e5edfdae1', kami_buddha_ontological_identity, theological).
narrative_ontology:cs_axiom('89ff1b3f-e501-48c4-bdd1-ba3e5edfdae1', foundational, universal_dharma_local_trace).
narrative_ontology:cs_axiom_status(universal_dharma_local_trace, holdable).
narrative_ontology:cs_axiom_grounding('89ff1b3f-e501-48c4-bdd1-ba3e5edfdae1', universal_dharma_local_trace, theological).
narrative_ontology:cs_reference_frame('89ff1b3f-e501-48c4-bdd1-ba3e5edfdae1', buddhist_soteriological_universality).
narrative_ontology:cs_drift_state('89ff1b3f-e501-48c4-bdd1-ba3e5edfdae1', late_edo_kokugaku_challenge, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('89ff1b3f-e501-48c4-bdd1-ba3e5edfdae1', '').
narrative_ontology:cs_kernel_id(simultaneous_veneration__ontological_fusion_reading, simultaneous_veneration).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(simultaneous_veneration__ontological_fusion_reading, buddhist_institutional_hierarchy).
narrative_ontology:constraint_beneficiary(simultaneous_veneration__ontological_fusion_reading, syncretic_laity).
narrative_ontology:constraint_victim(simultaneous_veneration__ontological_fusion_reading, shrine_priesthoods).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the honji-suijaku doctrinal framework, interpreting all kami phenomena through Buddhist cosmology. Controls temple-shrine organizational fusion (jingu-ji), claims sole authority to decode kami nature, and collects land endowments, imperial patronage, and ritual supremacy through this interpretive monopoly.
narrative_ontology:constraint_stakeholder(simultaneous_veneration__ontological_fusion_reading, buddhist_institutional_hierarchy, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(simultaneous_veneration__ontological_fusion_reading, buddhist_institutional_hierarchy, beneficiary).

% Maintain shrines and conduct kami rituals but must doctrinally accept subordination to Buddhist temples. Their independent theological claims are recast as local traces of universal Buddhist truth; autonomous shrine theology is marginalized, and priestly status depends on acceptance of the Buddhist interpretive frame.
narrative_ontology:constraint_stakeholder(simultaneous_veneration__ontological_fusion_reading, shrine_priesthoods, payer,
    organized, generational, constrained, national).

% Ordinary practitioners who worship both kami and buddhas. Benefit from a unified theological framework that permits dual veneration without requiring an explicit choice between traditions, though they indirectly support the Buddhist hierarchy through donations, ritual fees, and the subordination of their local shrines.
narrative_ontology:constraint_stakeholder(simultaneous_veneration__ontological_fusion_reading, syncretic_laity, beneficiary,
    moderate, biographical, constrained, local).

% Shrine priests and early nativist scholars who assert kami as autonomous divine powers independent of Buddhist ontology. Structurally excluded from imperial recognition and orthodox doctrinal discourse as long as the ontological fusion framework prevails; their theological claims are rendered illegitimate within the Buddhist-dominated interpretive field.
narrative_ontology:constraint_stakeholder(simultaneous_veneration__ontological_fusion_reading, shinto_independence_advocates, excluded,
    organized, generational, trapped, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(simultaneous_veneration__ontological_fusion_reading, buddhist_institutional_hierarchy).
narrative_ontology:fixing_cost_class(simultaneous_veneration__ontological_fusion_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Integrates indigenous Japanese kami worship and imported Buddhist practice into a single ontological framework, resolving potential theological conflict by positing a unified metaphysical reality behind both traditions.
% TRANSFER_FUNCTION: Moves interpretive authority, ontological status, and ritual patronage from indigenous shrine priesthoods to the Buddhist institutional hierarchy; kami are reclassified as local traces of universal Buddhist truths, transferring theological sovereignty to the temple system.
% ABSENT_VOICES: Shrine priests and scholars asserting autonomous kami divinity independent of Buddhist ontology are structurally excluded from doctrinal authority; their theological claims are subordinated or silenced within the Buddhist interpretive monopoly.
% DISAPPEARANCE_RATIONALE: If the ontological fusion claim disappeared, shrine priesthoods would reclaim independent theological authority, Buddhist institutions would lose interpretive monopoly over Japanese cultic life, and the unified temple-shrine ritual economy would fracture into competing religious authorities.
% FOUNDING_PROBLEM: How to integrate indigenous Japanese kami worship with imported Buddhist soteriology and cosmology without triggering persistent religious conflict or alienating the local population.
% FOUNDING_PROBLEM_CORROBORATION: Buddhist institutional chronicles attest the problem remains live (kami require Buddhist salvation). Independent shrine records and later Kokugaku scholars attest the problem was solved through political domination rather than metaphysical truth, and that the arrangement persists as institutional extraction. No fully independent corroboration exists from the Heian founding period; all attestations are retrospective and seated.
narrative_ontology:disappearance_verdict(simultaneous_veneration__ontological_fusion_reading, world_rearranges).
narrative_ontology:founding_problem_status(simultaneous_veneration__ontological_fusion_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(simultaneous_veneration__ontological_fusion_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(simultaneous_veneration__ontological_fusion_reading, 'none', 1).
narrative_ontology:epsilon_provenance(simultaneous_veneration__ontological_fusion_reading, 0.82, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness is high (0.82 at interval peak) because the constraint transfers full ontological and interpretive authority from indigenous shrines to Buddhist institutions; kami are not merely coordinated but redefined as subsidiary manifestations. Suppression is high (0.78) because the arrangement required active doctrinal enforcement, temple-shrine organizational fusion (jingu-ji), and the marginalization of non-Buddhist kami theology. Theater_ratio rises from 0.20 to 0.52, indicating that over time an increasing share of activity involved performative maintenance of the hierarchy rather than genuine theological innovation. Resistance is moderate (0.48) because shrine traditions persisted locally and Kokugaku eventually mounted a sustained intellectual challenge, but for centuries the framework faced little organized institutional resistance. The temporal grid shows extraction intensifying through the medieval period and stabilizing in the Edo period as the hierarchy became fully theatrical.
 *
 * PERSPECTIVAL GAP:
 *   From the Buddhist hierarchy's seat, the constraint is legitimate lineage transmission and metaphysical truth; the engine will compute a different type from that seat than from the shrine priesthood seat, where the same structure reads as enforced subordination. The divergence is structural: same power asymmetry, same exit options, but reversed beneficiary/victim roles produce opposed directionalities.
 *
 * DIRECTIONALITY LOGIC:
 *   The Buddhist institutional hierarchy sits at low directionality (near beneficiary): they define the constraint, enforce it through doctrinal authority and temple networks, and collect interpretive monopoly rents (land, patronage, ritual supremacy). Shrine priesthoods sit at high directionality (near full target): they must accept Buddhist ontological framing to retain institutional legitimacy, pay through lost theological autonomy, and have constrained exit because leaving the framework means losing imperial recognition and community standing. Syncretic laity sit near symmetric (moderate d): they gain genuine coordination (permission to worship both) but pay indirect costs through ritual fees and subordinated shrine status. Excluded independence advocates have no legitimate exit within the framework.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problemâintegrating imported Buddhism with indigenous kami worshipâwas genuinely live in the early Heian period. However, by the Muromachi period the problem was largely solved through institutional fusion, yet the ontological fusion reading persisted as an extractive arrangement. The (founding_problem_status=contested, disappearance_verdict=world_rearranges) pair signals that the arrangement's persistence is contested: Buddhist institutions claim the problem remains live (kami still need Buddhist interpretation), while shrine traditions and later scholars argue the problem is dead and the constraint persists as monopoly. The engine's piton/snare detection will evaluate whether the theater_ratio and cost asymmetry support mandatrophy.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    ontological_unity_vs_domain_separation,
    'Is honji-suijaku best understood as asserting genuine ontological identity (fusion) or as merely functional domain partition (specialization) between distinct entities?',
    'Historical-textual analysis of medieval doctrinal sources: do they assert literal ontological identity or merely functional equivalence across domains?',
    'If the sources support only functional partition, the ontological fusion reading is a later imposition or misreading, and the constraint''s extraction level may be lower than claimed. If they assert genuine ontological identity, the fusion reading is historically grounded and its high extraction is the price of its coordination function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ontological_unity_vs_domain_separation, conceptual, 'Whether the kernel supports ontological fusion or domain partition as the primary reading.').

omega_variable(
    metaphysical_truth_vs_institutional_construction,
    'Is the ontological fusion of kami and buddhas a discovered metaphysical truth or a constructed doctrinal tool for institutional integration and monopoly?',
    'Comparative analysis of how the doctrine shifted with the political imperatives of temple-shrine power; whether the truth claim correlates with periods of maximal institutional consolidation.',
    'If the doctrine is shown to be a constructed tool, the constraint is exposed as a snare or highly extractive tangled rope. If it is a genuine metaphysical discovery, it would approach mountain territory (though declared beneficiaries would still trigger FSM evaluation).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(metaphysical_truth_vs_institutional_construction, conceptual, 'Natural-law ambiguity: constructed extraction or discovered metaphysical truth.').

omega_variable(
    coercion_mechanism_doctrinal_vs_political,
    'Was the enforcement of ontological fusion achieved primarily through doctrinal authority and theological argument, or through political and economic control of shrine institutions?',
    'Archival analysis of temple-shrine land grants, imperial edicts, and priestly ordination records across the Heian to Edo periods.',
    'Political-economic enforcement would raise measured suppression and support a snare classification; doctrinal authority alone might suggest a softer coordination function with lower effective extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coercion_mechanism_doctrinal_vs_political, empirical, 'Structural ambiguity in the suppression mechanism.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(simultaneous_veneration__ontological_fusion_reading, 0, 900).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(simu_tr_t0, simultaneous_veneration__ontological_fusion_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(simu_tr_t150, simultaneous_veneration__ontological_fusion_reading, theater_ratio, 150, 0.28).
narrative_ontology:measurement(simu_tr_t300, simultaneous_veneration__ontological_fusion_reading, theater_ratio, 300, 0.35).
narrative_ontology:measurement(simu_tr_t450, simultaneous_veneration__ontological_fusion_reading, theater_ratio, 450, 0.42).
narrative_ontology:measurement(simu_tr_t600, simultaneous_veneration__ontological_fusion_reading, theater_ratio, 600, 0.48).
narrative_ontology:measurement(simu_tr_t750, simultaneous_veneration__ontological_fusion_reading, theater_ratio, 750, 0.5).
narrative_ontology:measurement(simu_tr_t900, simultaneous_veneration__ontological_fusion_reading, theater_ratio, 900, 0.52).

% Extraction over time
narrative_ontology:measurement(simu_be_t0, simultaneous_veneration__ontological_fusion_reading, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(simu_be_t150, simultaneous_veneration__ontological_fusion_reading, base_extractiveness, 150, 0.62).
narrative_ontology:measurement(simu_be_t300, simultaneous_veneration__ontological_fusion_reading, base_extractiveness, 300, 0.72).
narrative_ontology:measurement(simu_be_t450, simultaneous_veneration__ontological_fusion_reading, base_extractiveness, 450, 0.8).
narrative_ontology:measurement(simu_be_t600, simultaneous_veneration__ontological_fusion_reading, base_extractiveness, 600, 0.82).
narrative_ontology:measurement(simu_be_t750, simultaneous_veneration__ontological_fusion_reading, base_extractiveness, 750, 0.8).
narrative_ontology:measurement(simu_be_t900, simultaneous_veneration__ontological_fusion_reading, base_extractiveness, 900, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(simu_su_t0, simultaneous_veneration__ontological_fusion_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(simu_su_t150, simultaneous_veneration__ontological_fusion_reading, suppression_requirement, 150, 0.58).
narrative_ontology:measurement(simu_su_t300, simultaneous_veneration__ontological_fusion_reading, suppression_requirement, 300, 0.68).
narrative_ontology:measurement(simu_su_t450, simultaneous_veneration__ontological_fusion_reading, suppression_requirement, 450, 0.75).
narrative_ontology:measurement(simu_su_t600, simultaneous_veneration__ontological_fusion_reading, suppression_requirement, 600, 0.78).
narrative_ontology:measurement(simu_su_t750, simultaneous_veneration__ontological_fusion_reading, suppression_requirement, 750, 0.76).
narrative_ontology:measurement(simu_su_t900, simultaneous_veneration__ontological_fusion_reading, suppression_requirement, 900, 0.74).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(simultaneous_veneration__ontological_fusion_reading, identity_coordination).
narrative_ontology:affects_constraint(simultaneous_veneration__ontological_fusion_reading, domain_partition_reading).
narrative_ontology:affects_constraint(simultaneous_veneration__ontological_fusion_reading, pragmatic_incoherence_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the simultaneous_veneration kernel. The ontological_fusion_reading decomposes from the colloquial label 'honji-suijaku' by isolating the specific claim of ontological identity, which carries distinct epsilon, beneficiaries, and enforcement structure from the domain-partition or pragmatic-incoherence readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
