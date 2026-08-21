% ============================================================================
% CONSTRAINT STORY: kami_buddha_ontology__incoherent_bundle
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_kami_buddha_ontology__incoherent_bundle, []).

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
 *   constraint_id: kami_buddha_ontology__incoherent_bundle
 *   human_readable: Shinbutsu-shugo as an Incoherent Institutional Bundle
 *   domain: religious_studies/philosophy_of_religion/japanese_cultural_history
 *
 * SUMMARY:
 *   This constraint describes Shinbutsu-shugo not as a coherent theological
 *   system, but as an institutionally sustained bundle of contradictory
 *   commitments. It encompasses simultaneous fusion and separation of kami
 *   and buddhas, hierarchical and reciprocal relationships, and both
 *   systematized and unsystematized practices. This reading emphasizes that
 *   no single, unified ontology underpins the phenomenon; rather,
 *   institutional inertia, practical efficacy, and ritual success sustain
 *   these contradictions, masking theoretical incoherence. Attempts to impose
 *   strict separation or a singular coherent framework have historically
 *   failed or been resisted.
 *
 * KEY AGENTS:
 *   - religious_institutions: Agenda-setter/Beneficiary (institutional/constrained)
 *   - local_communities: Beneficiary (organized/constrained)
 *   - theologians_scholars: Payer (moderate/constrained)
 *   - practitioners_seeking_coherence: Payer (powerless/identity_locked)
 *   - state_authorities: Observer/Agenda-setter (institutional/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(kami_buddha_ontology__incoherent_bundle, 0.65).
domain_priors:suppression_score(kami_buddha_ontology__incoherent_bundle, 0.7).
domain_priors:theater_ratio(kami_buddha_ontology__incoherent_bundle, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(kami_buddha_ontology__incoherent_bundle, extractiveness, 0.65).
narrative_ontology:constraint_metric(kami_buddha_ontology__incoherent_bundle, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(kami_buddha_ontology__incoherent_bundle, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(kami_buddha_ontology__incoherent_bundle, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(kami_buddha_ontology__incoherent_bundle, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(kami_buddha_ontology__incoherent_bundle, tangled_rope).
narrative_ontology:human_readable(kami_buddha_ontology__incoherent_bundle, "Shinbutsu-shugo as an Incoherent Institutional Bundle").
narrative_ontology:topic_domain(kami_buddha_ontology__incoherent_bundle, "religious_studies/philosophy_of_religion/japanese_cultural_history").

domain_priors:requires_active_enforcement(kami_buddha_ontology__incoherent_bundle).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(kami_buddha_ontology__incoherent_bundle, '06f27fb9-f845-46f9-9c70-c6d4f21f2945').
narrative_ontology:cs_kernel_codification('06f27fb9-f845-46f9-9c70-c6d4f21f2945', implicit).
narrative_ontology:cs_authority_grounding('06f27fb9-f845-46f9-9c70-c6d4f21f2945', practice).
narrative_ontology:cs_interpretation_layer_present('06f27fb9-f845-46f9-9c70-c6d4f21f2945').
narrative_ontology:cs_reading_relation('06f27fb9-f845-46f9-9c70-c6d4f21f2945', kami_buddha_ontology__honji_suijaku_monism, forecloses).
narrative_ontology:cs_reading_relation('06f27fb9-f845-46f9-9c70-c6d4f21f2945', kami_buddha_ontology__domain_partition, coexists_with).
narrative_ontology:cs_axiom('06f27fb9-f845-46f9-9c70-c6d4f21f2945', foundational, ontological_coherence_is_not_a_primary_value).
narrative_ontology:cs_axiom_status(ontological_coherence_is_not_a_primary_value, holdable).
narrative_ontology:cs_axiom_grounding('06f27fb9-f845-46f9-9c70-c6d4f21f2945', ontological_coherence_is_not_a_primary_value, conventional).
narrative_ontology:cs_axiom('06f27fb9-f845-46f9-9c70-c6d4f21f2945', foundational, ritual_efficacy_trumps_theoretical_consistency).
narrative_ontology:cs_axiom_status(ritual_efficacy_trumps_theoretical_consistency, holdable).
narrative_ontology:cs_axiom_grounding('06f27fb9-f845-46f9-9c70-c6d4f21f2945', ritual_efficacy_trumps_theoretical_consistency, instrumental).
narrative_ontology:cs_reference_frame('06f27fb9-f845-46f9-9c70-c6d4f21f2945', syncretic_practice_efficacy).
narrative_ontology:cs_drift_state('06f27fb9-f845-46f9-9c70-c6d4f21f2945', contemporary_globalized_era, gap(stable, minor, false)).
narrative_ontology:cs_created_at('06f27fb9-f845-46f9-9c70-c6d4f21f2945', '').
narrative_ontology:cs_kernel_id(kami_buddha_ontology__incoherent_bundle, kami_buddha_ontology).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(kami_buddha_ontology__incoherent_bundle, religious_institutions).
narrative_ontology:constraint_beneficiary(kami_buddha_ontology__incoherent_bundle, local_communities).
narrative_ontology:constraint_victim(kami_buddha_ontology__incoherent_bundle, theologians_scholars).
narrative_ontology:constraint_victim(kami_buddha_ontology__incoherent_bundle, practitioners_seeking_coherence).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administer shrines and temples, maintaining the syncretic practices of Shinbutsu-shugo. They benefit from the flexibility and broad appeal of the bundled commitments, which allows them to serve diverse spiritual needs without resolving underlying contradictions. They actively resist attempts to impose strict ontological coherence or separation.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__incoherent_bundle, religious_institutions, agenda_setter,
    institutional, generational, constrained, national).

% Benefit from the practical efficacy and cultural continuity provided by Shinbutsu-shugo. The flexible nature of the bundle allows for diverse local interpretations and rituals, integrating kami and buddhas into daily life and seasonal festivals without requiring deep theological understanding or adherence to a single, coherent doctrine.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__incoherent_bundle, local_communities, beneficiary,
    organized, biographical, constrained, local).

% Bear the cost of theoretical incoherence, struggling to construct a consistent philosophical or theological framework for Shinbutsu-shugo. Their attempts to systematize or resolve contradictions are often marginalized by the practical, institutionally sustained nature of the bundle, leading to intellectual frustration and limited influence on practice.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__incoherent_bundle, theologians_scholars, payer,
    moderate, biographical, constrained, global).

% Seek clear, consistent spiritual guidance and ontological understanding, but find themselves navigating a system of contradictory commitments. They may experience cognitive dissonance or a sense of spiritual ambiguity, as the institutional bundle prioritizes ritual success and cultural integration over theoretical clarity. Their identity is often deeply tied to these practices, making exit difficult.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__incoherent_bundle, practitioners_seeking_coherence, payer,
    powerless, immediate, identity_locked, local).

% Historically and currently regulate religious institutions, sometimes attempting to enforce separation (e.g., Meiji era Shinbutsu-bunri) or manage their integration. They observe the practical functioning of Shinbutsu-shugo and can impose legal or administrative constraints that influence its institutional form, though rarely its underlying ontological ambiguity.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__incoherent_bundle, state_authorities, observer,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(kami_buddha_ontology__incoherent_bundle, state_authorities, agenda_setter).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(kami_buddha_ontology__incoherent_bundle, religious_institutions).
narrative_ontology:fixing_cost_class(kami_buddha_ontology__incoherent_bundle, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a flexible, culturally embedded framework for diverse religious practices and beliefs, allowing for the simultaneous veneration of kami and buddhas, and integrating spiritual life with social and seasonal cycles without requiring strict doctrinal adherence.
% TRANSFER_FUNCTION: Transfers institutional stability, cultural continuity, and ritual efficacy to religious institutions and local communities, while transferring the burden of theoretical incoherence and ambiguity to scholars and practitioners seeking systematic understanding.
% ABSENT_VOICES: Strict monotheists or proponents of a singular, coherent ontology would object to the fundamental contradictions, arguing for a unified theological system. They are absent from the dominant discourse because the institutional and practical success of the bundle marginalizes such theoretical demands.
% DISAPPEARANCE_RATIONALE: If the institutional bundle of Shinbutsu-shugo vanished overnight, the religious landscape of Japan would be profoundly disrupted. Many shrines and temples would lose their integrated functions, local festivals would lose their syncretic meaning, and a significant portion of cultural identity would be fractured, leading to a major reorganization of religious and social life.
% FOUNDING_PROBLEM: The historical challenge of integrating indigenous Japanese kami worship with the imported Buddhist tradition, creating a framework that allowed both to flourish and adapt within the same cultural sphere.
% FOUNDING_PROBLEM_CORROBORATION: The problem of integrating diverse spiritual traditions remains live, as new cultural influences and philosophical inquiries continue to challenge existing frameworks. Anthropological studies of contemporary Japanese religious practice and historical analyses of syncretism, from outside the directly benefiting religious institutions, corroborate the ongoing nature of this integration challenge.
narrative_ontology:disappearance_verdict(kami_buddha_ontology__incoherent_bundle, world_rearranges).
narrative_ontology:founding_problem_status(kami_buddha_ontology__incoherent_bundle, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(kami_buddha_ontology__incoherent_bundle, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(kami_buddha_ontology__incoherent_bundle, 'none', 1).
narrative_ontology:epsilon_provenance(kami_buddha_ontology__incoherent_bundle, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(kami_buddha_ontology__incoherent_bundle_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(kami_buddha_ontology__incoherent_bundle, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(kami_buddha_ontology__incoherent_bundle_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The `extractiveness` (0.65) is moderate-high, reflecting the cost borne by those seeking coherence or a unified understanding, as the system extracts intellectual effort and suppresses alternative coherent framings. `Suppression` (0.70) is high because the institutional and cultural inertia actively marginalizes attempts to resolve the contradictions or impose a singular ontology. `Theater_ratio` (0.45) is moderate, as ritual success and practical efficacy often serve to mask the underlying theoretical incoherence. `Accessibility_collapse` (0.40) is moderate, as alternative coherent interpretations exist but are difficult to establish against the dominant bundle. `Resistance` (0.30) is moderate-low, primarily from scholars and some practitioners, but not strong enough to fundamentally alter the institutionalized incoherence.
 *
 * PERSPECTIVAL GAP:
 *   Religious institutions and local communities experience this as a beneficial, flexible system that effectively coordinates diverse spiritual needs. For them, the 'incoherence' is a feature, allowing broad participation and adaptation. Theologians and scholars, however, experience it as a frustrating intellectual problem, where the lack of a coherent kernel extracts significant effort and suppresses their attempts at systematization. The engine's per-seat classification would reflect this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Religious institutions and local communities are beneficiaries, as the bundle's flexibility and broad appeal sustain their functions and cultural identity. Theologians and practitioners seeking coherence are targets, as they bear the intellectual and spiritual costs of navigating contradictory commitments. State authorities act as observers but can also become agenda-setters when attempting to regulate or reform religious structures, influencing the constraint's enforcement.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is classified as a Tangled Rope because it genuinely coordinates diverse religious practices and beliefs (benefiting institutions and communities) while simultaneously extracting a cost in theoretical incoherence and suppressing alternative coherent framings (from scholars and some practitioners). The 'incoherent bundle' is actively maintained by institutional inertia and practical efficacy, not merely by atrophy. The classification prevents mislabeling it as a Piton, which would imply a degraded function, when the 'incoherence' is itself a functional aspect of its persistence.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    incoherence_as_feature_or_bug,
    'Is the observed incoherence of Shinbutsu-shugo a functional feature (enabling flexibility and broad appeal) or a theoretical bug (a lack of rigor and clarity)?',
    'Comparative studies of religious systems with varying degrees of ontological coherence and their respective social/cultural functions; analysis of practitioner satisfaction with ambiguity vs. clarity.',
    'If primarily a feature, the ''extraction'' from scholars is a necessary cost of a highly adaptive coordination mechanism. If primarily a bug, the extraction is a pure cost of institutional inertia and suppression of intellectual inquiry.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(incoherence_as_feature_or_bug, conceptual, 'Ambiguity regarding the functional role of ontological incoherence.').

omega_variable(
    institutional_inertia_vs_active_maintenance,
    'To what extent is the persistence of the ''incoherent bundle'' due to passive institutional inertia versus active, deliberate maintenance by religious authorities?',
    'Historical analysis of institutional responses to attempts at reform or systematization; ethnographic studies of decision-making processes within religious organizations regarding syncretic practices.',
    'If primarily passive inertia, the constraint leans more towards a Piton. If active maintenance, it reinforces the Tangled Rope classification due to ongoing enforcement of the contradictory status quo.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_inertia_vs_active_maintenance, empirical, 'Distinguishing passive inertia from active institutional defense of the incoherent bundle.').

omega_variable(
    kernel_reading_incoherent_bundle,
    'This constraint is the ''incoherent_bundle'' reading of the ''kami_buddha_ontology'' kernel. What would change if a sibling reading were adopted?',
    'Analysis of the structural implications of ''honji_suijaku_monism'' (ontological identity) or ''domain_partition'' (strict separation) being universally adopted by institutions and practitioners.',
    'If ''honji_suijaku_monism'' were adopted, the extraction from theoretical incoherence would vanish, and the constraint would likely reclassify as a Rope. If ''domain_partition'' were adopted, the coordination function would shift to managing distinct spheres, potentially reducing extraction but increasing suppression of syncretic practices.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_incoherent_bundle, conceptual, 'Impact of alternative readings of the kami_buddha_ontology kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(kami_buddha_ontology__incoherent_bundle, 0, 1500).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(kami_tr_t0, kami_buddha_ontology__incoherent_bundle, theater_ratio, 0, 0.2).
narrative_ontology:measurement(kami_tr_t300, kami_buddha_ontology__incoherent_bundle, theater_ratio, 300, 0.3).
narrative_ontology:measurement(kami_tr_t600, kami_buddha_ontology__incoherent_bundle, theater_ratio, 600, 0.38).
narrative_ontology:measurement(kami_tr_t900, kami_buddha_ontology__incoherent_bundle, theater_ratio, 900, 0.42).
narrative_ontology:measurement(kami_tr_t1200, kami_buddha_ontology__incoherent_bundle, theater_ratio, 1200, 0.44).
narrative_ontology:measurement(kami_tr_t1500, kami_buddha_ontology__incoherent_bundle, theater_ratio, 1500, 0.45).

% Extraction over time
narrative_ontology:measurement(kami_be_t0, kami_buddha_ontology__incoherent_bundle, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(kami_be_t300, kami_buddha_ontology__incoherent_bundle, base_extractiveness, 300, 0.5).
narrative_ontology:measurement(kami_be_t600, kami_buddha_ontology__incoherent_bundle, base_extractiveness, 600, 0.58).
narrative_ontology:measurement(kami_be_t900, kami_buddha_ontology__incoherent_bundle, base_extractiveness, 900, 0.62).
narrative_ontology:measurement(kami_be_t1200, kami_buddha_ontology__incoherent_bundle, base_extractiveness, 1200, 0.64).
narrative_ontology:measurement(kami_be_t1500, kami_buddha_ontology__incoherent_bundle, base_extractiveness, 1500, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(kami_su_t0, kami_buddha_ontology__incoherent_bundle, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(kami_su_t300, kami_buddha_ontology__incoherent_bundle, suppression_requirement, 300, 0.58).
narrative_ontology:measurement(kami_su_t600, kami_buddha_ontology__incoherent_bundle, suppression_requirement, 600, 0.65).
narrative_ontology:measurement(kami_su_t900, kami_buddha_ontology__incoherent_bundle, suppression_requirement, 900, 0.68).
narrative_ontology:measurement(kami_su_t1200, kami_buddha_ontology__incoherent_bundle, suppression_requirement, 1200, 0.69).
narrative_ontology:measurement(kami_su_t1500, kami_buddha_ontology__incoherent_bundle, suppression_requirement, 1500, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(kami_buddha_ontology__incoherent_bundle, identity_coordination).
narrative_ontology:affects_constraint(kami_buddha_ontology__incoherent_bundle, shinto_ritual_practice).
narrative_ontology:affects_constraint(kami_buddha_ontology__incoherent_bundle, buddhist_funeral_rites).
narrative_ontology:affects_constraint(kami_buddha_ontology__incoherent_bundle, japanese_cultural_identity).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'kami_buddha_ontology' kernel. This 'incoherent_bundle' reading emphasizes the institutional maintenance of contradictions, contrasting with 'honji_suijaku_monism' (ontological identity) and 'domain_partition' (strict separation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
