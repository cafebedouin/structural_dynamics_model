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
 *   constraint_id: kami_buddha_ontology__incoherent_bundle
 *   human_readable: Kami-Buddha Ontology: Incoherent Bundle Reading
 *   domain: religious_studies/philosophy_of_religion/japanese_cultural_history
 *
 * SUMMARY:
 *   This constraint represents the 'incoherent bundle' reading of
 *   Shinbutsu-shugo, where the fusion of Kami and Buddhist deities is
 *   understood not as a coherent theological system, but as an
 *   institutionally sustained set of contradictory commitments. This reading
 *   emphasizes the practical efficacy and institutional inertia that maintain
 *   the syncretic practices despite their theoretical inconsistencies. The
 *   constraint is claimed as a Tangled Rope because it provides a
 *   coordination function (religious integration) but also involves
 *   asymmetric extraction (cognitive burden on scholars, institutional
 *   benefits for religious authorities) and requires active enforcement to
 *   suppress ontological challenges.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(kami_buddha_ontology__incoherent_bundle, 0.6).
domain_priors:suppression_score(kami_buddha_ontology__incoherent_bundle, 0.7).
domain_priors:theater_ratio(kami_buddha_ontology__incoherent_bundle, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(kami_buddha_ontology__incoherent_bundle, extractiveness, 0.6).
narrative_ontology:constraint_metric(kami_buddha_ontology__incoherent_bundle, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(kami_buddha_ontology__incoherent_bundle, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(kami_buddha_ontology__incoherent_bundle, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(kami_buddha_ontology__incoherent_bundle, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(kami_buddha_ontology__incoherent_bundle, tangled_rope).
narrative_ontology:human_readable(kami_buddha_ontology__incoherent_bundle, "Kami-Buddha Ontology: Incoherent Bundle Reading").
narrative_ontology:topic_domain(kami_buddha_ontology__incoherent_bundle, "religious_studies/philosophy_of_religion/japanese_cultural_history").

domain_priors:requires_active_enforcement(kami_buddha_ontology__incoherent_bundle).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(kami_buddha_ontology__incoherent_bundle, '8ddbaa0d-5cf0-4368-8367-88c2362fc8e5').
narrative_ontology:cs_kernel_codification('8ddbaa0d-5cf0-4368-8367-88c2362fc8e5', distributed).
narrative_ontology:cs_authority_grounding('8ddbaa0d-5cf0-4368-8367-88c2362fc8e5', practice).
narrative_ontology:cs_interpretation_layer_present('8ddbaa0d-5cf0-4368-8367-88c2362fc8e5').
narrative_ontology:cs_reading_relation('8ddbaa0d-5cf0-4368-8367-88c2362fc8e5', kami_buddha_ontology__honji_suijaku_monism, coexists_with).
narrative_ontology:cs_reading_relation('8ddbaa0d-5cf0-4368-8367-88c2362fc8e5', kami_buddha_ontology__domain_partition, coexists_with).
narrative_ontology:cs_axiom('8ddbaa0d-5cf0-4368-8367-88c2362fc8e5', foundational, ontological_contradiction_is_sustained).
narrative_ontology:cs_axiom_status(ontological_contradiction_is_sustained, holdable).
narrative_ontology:cs_axiom_grounding('8ddbaa0d-5cf0-4368-8367-88c2362fc8e5', ontological_contradiction_is_sustained, empirically_contingent).
narrative_ontology:cs_axiom('8ddbaa0d-5cf0-4368-8367-88c2362fc8e5', foundational, institutional_inertia_drives_persistence).
narrative_ontology:cs_axiom_status(institutional_inertia_drives_persistence, holdable).
narrative_ontology:cs_axiom_grounding('8ddbaa0d-5cf0-4368-8367-88c2362fc8e5', institutional_inertia_drives_persistence, empirically_contingent).
narrative_ontology:cs_reference_frame('8ddbaa0d-5cf0-4368-8367-88c2362fc8e5', pre_modern_syncretic_practice).
narrative_ontology:cs_drift_state('8ddbaa0d-5cf0-4368-8367-88c2362fc8e5', modern_critical_scholarship, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('8ddbaa0d-5cf0-4368-8367-88c2362fc8e5', '').
narrative_ontology:cs_kernel_id(kami_buddha_ontology__incoherent_bundle, kami_buddha_ontology).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(kami_buddha_ontology__incoherent_bundle, shinto_shrines).
narrative_ontology:constraint_beneficiary(kami_buddha_ontology__incoherent_bundle, buddhist_temples).
narrative_ontology:constraint_beneficiary(kami_buddha_ontology__incoherent_bundle, religious_authorities).
narrative_ontology:constraint_victim(kami_buddha_ontology__incoherent_bundle, theological_scholars).
narrative_ontology:constraint_victim(kami_buddha_ontology__incoherent_bundle, lay_practitioners).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from the institutional stability and continued patronage derived from the bundled practices, even if the underlying ontology is contradictory. They maintain rituals that fuse kami and buddhas, attracting diverse adherents.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__incoherent_bundle, shinto_shrines, beneficiary,
    institutional, generational, constrained, national).

% Benefit from the same institutional stability and patronage as Shinto shrines. They integrate kami worship into Buddhist practices, leveraging the cultural embeddedness of both traditions.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__incoherent_bundle, buddhist_temples, beneficiary,
    institutional, generational, constrained, national).

% Administer and perpetuate the bundled practices, often implicitly or explicitly discouraging attempts to resolve the ontological contradictions, as such resolution could destabilize their institutional power and patronage. Their authority is tied to the existing, successful syncretic system.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__incoherent_bundle, religious_authorities, agenda_setter,
    institutional, generational, identity_locked, national).

% Bear the cost of intellectual incoherence, struggling to construct a consistent theological framework for Shinbutsu-shugo. Their attempts at systematization are often resisted by institutional inertia, making their work difficult to integrate into practice.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__incoherent_bundle, theological_scholars, payer,
    moderate, biographical, constrained, global).

% Experience the bundled practices as a given, often without deep engagement with the underlying ontological contradictions. They may feel a diffuse sense of confusion or cognitive dissonance when confronted with the inconsistencies, but their primary concern is practical efficacy and cultural belonging.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__incoherent_bundle, lay_practitioners, payer,
    powerless, biographical, identity_locked, local).

% Analyze the Shinbutsu-shugo phenomenon from a philosophical perspective, identifying the logical contradictions and the mechanisms by which they are sustained. They are external to the religious institutions and seek theoretical clarity.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__incoherent_bundle, analytical_philosophers, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The bundled practices coordinate diverse religious and cultural needs within a single, institutionally stable framework, allowing for simultaneous engagement with both Shinto and Buddhist traditions without requiring explicit ontological reconciliation from practitioners.
% TRANSFER_FUNCTION: Transfers cultural legitimacy, patronage, and institutional stability to Shinto shrines and Buddhist temples by maintaining a broad, inclusive religious framework. It transfers cognitive burden and theoretical incoherence to scholars and, diffusely, to practitioners.
% ABSENT_VOICES: Strict monotheists or purists from either Shinto or Buddhist traditions who would demand ontological consistency and separation are marginalized or historically suppressed; they would argue for a clear, non-contradictory theological framework.
% DISAPPEARANCE_RATIONALE: If the institutional mechanisms sustaining the incoherent bundle vanished, the existing religious landscape would fragment. Shrines and temples would lose a significant portion of their patronage, and practitioners would be forced to choose or reconcile traditions, leading to a major reorganization of religious practice and institutional power.
% FOUNDING_PROBLEM: The need to integrate indigenous kami worship with the newly introduced Buddhist tradition in ancient Japan, creating a syncretic system that could accommodate both without overt conflict.
% FOUNDING_PROBLEM_CORROBORATION: Historians and cultural anthropologists corroborate the historical necessity of syncretism. Religious authorities attest that the need for cultural and spiritual integration remains, even if the specific forms of integration are contested by scholars.
narrative_ontology:disappearance_verdict(kami_buddha_ontology__incoherent_bundle, world_rearranges).
narrative_ontology:founding_problem_status(kami_buddha_ontology__incoherent_bundle, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(kami_buddha_ontology__incoherent_bundle, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(kami_buddha_ontology__incoherent_bundle, 'none', 1).
narrative_ontology:epsilon_provenance(kami_buddha_ontology__incoherent_bundle, 0.6, 'gemini-2.5-flash', 'none', direct).

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
 *   Extractiveness is moderate-high (0.6) because the institutional benefits derived from maintaining the bundle come at the cost of intellectual clarity and consistency for those who seek it. Suppression is high (0.7) as active enforcement, often through social and institutional pressure, is required to prevent challenges to the ontological incoherence from destabilizing the system. Theater ratio is moderate (0.4) because while genuine religious practices occur, a significant portion of institutional activity involves maintaining the appearance of coherence or simply ignoring the contradictions, rather than engaging with them directly. The historical measurements show a gradual increase in extractiveness and suppression as the system became more entrenched and the contradictions more apparent over centuries.
 *
 * PERSPECTIVAL GAP:
 *   Religious authorities and institutions experience this as a successful, stable coordination mechanism, while scholars and critical practitioners experience it as a source of intellectual frustration and suppressed alternatives. The engine's per-seat classification should reflect this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Shinto shrines and Buddhist temples are beneficiaries, gaining institutional stability and patronage. Religious authorities are agenda-setters, actively maintaining the bundle for their own institutional benefit. Theological scholars and lay practitioners are payers, bearing the cognitive and practical costs of the incoherence. Analytical philosophers are observers, external to the system.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as a Tangled Rope prevents mislabeling the constraint as a pure Snare by acknowledging its genuine coordination function (integrating diverse religious practices). However, it also prevents mislabeling it as a pure Rope by highlighting the asymmetric extraction and active enforcement required to sustain the ontological incoherence. The mandate has not atrophied, but its function has shifted from genuine theological integration to institutional maintenance of a successful, albeit contradictory, syncretic system.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    institutional_vs_theological_coherence,
    'To what extent is the observed ''incoherence'' a feature of institutional practice rather than a fundamental theological impossibility?',
    'Comparative study of other syncretic traditions that have achieved greater theological coherence, or analysis of historical attempts within Japan to systematize Shinbutsu-shugo.',
    'If primarily institutional, the constraint is more amenable to reform through changes in religious governance. If fundamentally theological, the ''incoherent bundle'' is an irreducible feature, making the constraint more Mountain-like in its persistence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_vs_theological_coherence, conceptual, 'Distinguishing institutional maintenance from inherent theological contradiction.').

omega_variable(
    practitioner_cognitive_dissonance,
    'What is the actual level of cognitive dissonance experienced by lay practitioners due to the ontological contradictions, and how does it impact their religious engagement?',
    'Sociological surveys and qualitative interviews with practitioners, distinguishing between those who are aware of the contradictions and those who are not, and measuring their responses.',
    'If dissonance is high and impacts engagement, the ''payer'' role for practitioners is more pronounced, increasing the constraint''s effective extraction. If dissonance is low or easily resolved through practical faith, the extraction from this seat is lower.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(practitioner_cognitive_dissonance, empirical, 'Measuring the impact of ontological incoherence on lay practitioners.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression of ontological challenges structural (institutional power, historical precedent) or internalized (cultural norms, identity fusion with syncretic practice)?',
    'Post-separation analysis: if challenges persist after institutional barriers are removed (e.g., during periods of state-mandated separation), reclassify as partially internalized. If challenges immediately emerge, suppression is primarily structural.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — practitioners carry the suppression with them. If structural, removing institutional barriers would more directly lead to resolution or fragmentation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for ontological challenges.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(kami_buddha_ontology__incoherent_bundle, 0, 1200).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(kami_tr_t0, kami_buddha_ontology__incoherent_bundle, theater_ratio, 0, 0.2).
narrative_ontology:measurement(kami_tr_t300, kami_buddha_ontology__incoherent_bundle, theater_ratio, 300, 0.3).
narrative_ontology:measurement(kami_tr_t600, kami_buddha_ontology__incoherent_bundle, theater_ratio, 600, 0.35).
narrative_ontology:measurement(kami_tr_t900, kami_buddha_ontology__incoherent_bundle, theater_ratio, 900, 0.38).
narrative_ontology:measurement(kami_tr_t1200, kami_buddha_ontology__incoherent_bundle, theater_ratio, 1200, 0.4).

% Extraction over time
narrative_ontology:measurement(kami_be_t0, kami_buddha_ontology__incoherent_bundle, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(kami_be_t300, kami_buddha_ontology__incoherent_bundle, base_extractiveness, 300, 0.5).
narrative_ontology:measurement(kami_be_t600, kami_buddha_ontology__incoherent_bundle, base_extractiveness, 600, 0.55).
narrative_ontology:measurement(kami_be_t900, kami_buddha_ontology__incoherent_bundle, base_extractiveness, 900, 0.58).
narrative_ontology:measurement(kami_be_t1200, kami_buddha_ontology__incoherent_bundle, base_extractiveness, 1200, 0.6).

% Suppression requirement over time
narrative_ontology:measurement(kami_su_t0, kami_buddha_ontology__incoherent_bundle, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(kami_su_t300, kami_buddha_ontology__incoherent_bundle, suppression_requirement, 300, 0.6).
narrative_ontology:measurement(kami_su_t600, kami_buddha_ontology__incoherent_bundle, suppression_requirement, 600, 0.65).
narrative_ontology:measurement(kami_su_t900, kami_buddha_ontology__incoherent_bundle, suppression_requirement, 900, 0.68).
narrative_ontology:measurement(kami_su_t1200, kami_buddha_ontology__incoherent_bundle, suppression_requirement, 1200, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(kami_buddha_ontology__incoherent_bundle, identity_coordination).
narrative_ontology:affects_constraint(kami_buddha_ontology__incoherent_bundle, honji_suijaku_monism).
narrative_ontology:affects_constraint(kami_buddha_ontology__incoherent_bundle, domain_partition).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the kami_buddha_ontology kernel. This 'incoherent bundle' reading emphasizes institutional maintenance of contradictions, influencing (and being influenced by) the 'honji_suijaku_monism' and 'domain_partition' readings, which attempt to resolve the ontology in different ways.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
