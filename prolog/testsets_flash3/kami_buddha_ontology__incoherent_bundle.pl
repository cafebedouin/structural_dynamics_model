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
 *   human_readable: Kami-Buddha Ontology: The Incoherent Bundle Reading
 *   domain: religious_studies/philosophy_of_religion/japanese_cultural_history
 *
 * SUMMARY:
 *   This constraint describes the 'incoherent bundle' reading of
 *   Shinbutsu-shugo, where the fusion of Kami and Buddhas is understood not
 *   as a coherent theological system, but as an institutionally sustained set
 *   of contradictory commitments. This reading emphasizes the practical,
 *   ritualistic, and political efficacy of the syncretic practices over any
 *   underlying ontological unity. The constraint's persistence is driven by
 *   institutional inertia and the benefits derived by religious organizations
 *   from this ambiguity, rather than by a clear, shared understanding of its
 *   nature. The claimed type is 'tangled_rope' because it provides a
 *   coordination function (allowing diverse practices to coexist) but also
 *   extracts from those seeking coherence through the maintenance of
 *   ambiguity and active suppression of alternative, more coherent framings.
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
narrative_ontology:constraint_metric(kami_buddha_ontology__incoherent_bundle, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(kami_buddha_ontology__incoherent_bundle, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(kami_buddha_ontology__incoherent_bundle, tangled_rope).
narrative_ontology:human_readable(kami_buddha_ontology__incoherent_bundle, "Kami-Buddha Ontology: The Incoherent Bundle Reading").
narrative_ontology:topic_domain(kami_buddha_ontology__incoherent_bundle, "religious_studies/philosophy_of_religion/japanese_cultural_history").

domain_priors:requires_active_enforcement(kami_buddha_ontology__incoherent_bundle).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(kami_buddha_ontology__incoherent_bundle, '21233a0a-380e-49ee-ac82-8b9073a70dab').
narrative_ontology:cs_kernel_codification('21233a0a-380e-49ee-ac82-8b9073a70dab', distributed).
narrative_ontology:cs_authority_grounding('21233a0a-380e-49ee-ac82-8b9073a70dab', practice).
narrative_ontology:cs_interpretation_layer_present('21233a0a-380e-49ee-ac82-8b9073a70dab').
narrative_ontology:cs_reading_relation('21233a0a-380e-49ee-ac82-8b9073a70dab', kami_buddha_ontology__honji_suijaku_monism, coexists_with).
narrative_ontology:cs_reading_relation('21233a0a-380e-49ee-ac82-8b9073a70dab', kami_buddha_ontology__domain_partition, coexists_with).
narrative_ontology:cs_axiom('21233a0a-380e-49ee-ac82-8b9073a70dab', foundational, ontological_ambiguity_is_functional).
narrative_ontology:cs_axiom_status(ontological_ambiguity_is_functional, holdable).
narrative_ontology:cs_axiom_grounding('21233a0a-380e-49ee-ac82-8b9073a70dab', ontological_ambiguity_is_functional, conventional).
narrative_ontology:cs_axiom('21233a0a-380e-49ee-ac82-8b9073a70dab', foundational, ritual_efficacy_trumps_theoretical_coherence).
narrative_ontology:cs_axiom_status(ritual_efficacy_trumps_theoretical_coherence, holdable).
narrative_ontology:cs_axiom_grounding('21233a0a-380e-49ee-ac82-8b9073a70dab', ritual_efficacy_trumps_theoretical_coherence, instrumental).
narrative_ontology:cs_reference_frame('21233a0a-380e-49ee-ac82-8b9073a70dab', pragmatic_syncretic_tradition).
narrative_ontology:cs_drift_state('21233a0a-380e-49ee-ac82-8b9073a70dab', contemporary_globalized_discourse, gap(stable, minor, false)).
narrative_ontology:cs_created_at('21233a0a-380e-49ee-ac82-8b9073a70dab', '').
narrative_ontology:cs_kernel_id(kami_buddha_ontology__incoherent_bundle, kami_buddha_ontology).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(kami_buddha_ontology__incoherent_bundle, shinto_shrines).
narrative_ontology:constraint_beneficiary(kami_buddha_ontology__incoherent_bundle, buddhist_temples).
narrative_ontology:constraint_beneficiary(kami_buddha_ontology__incoherent_bundle, religious_institutions).
narrative_ontology:constraint_victim(kami_buddha_ontology__incoherent_bundle, theological_scholars).
narrative_ontology:constraint_victim(kami_buddha_ontology__incoherent_bundle, lay_practitioners_seeking_coherence).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from the institutional arrangements that allow for the co-existence and intermingling of kami and buddha worship, often sharing precincts or ritual functions. This allows for broader appeal and resource access, even if the underlying ontology is contradictory.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__incoherent_bundle, shinto_shrines, beneficiary,
    institutional, generational, constrained, national).

% Benefit from the historical fusion with indigenous kami worship, which facilitated the spread of Buddhism in Japan and integrated it into local spiritual landscapes. The ambiguity allows for flexible adaptation and continued relevance.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__incoherent_bundle, buddhist_temples, beneficiary,
    institutional, generational, constrained, national).

% Collectively administer and perpetuate the practices of Shinbutsu-shugo, often prioritizing ritual efficacy and institutional stability over strict theological coherence. Their identity is deeply intertwined with the historical continuity of these bundled practices.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__incoherent_bundle, religious_institutions, agenda_setter,
    institutional, generational, identity_locked, national).

% Bear the cost of attempting to reconcile or systematize the inherent contradictions within Shinbutsu-shugo. Their intellectual work is complicated by the lack of a coherent underlying ontology, leading to ongoing debates and interpretive challenges.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__incoherent_bundle, theological_scholars, payer,
    moderate, biographical, constrained, global).

% Experience cognitive dissonance or confusion when seeking a unified understanding of their spiritual practices, as the institutional framework offers contradictory explanations or simply defers to ritual tradition. Their identity is often tied to these practices, making exit difficult.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__incoherent_bundle, lay_practitioners_seeking_coherence, payer,
    powerless, biographical, identity_locked, local).

% Historically attempted to enforce a clear separation of Kami and Buddhas (Shinbutsu Bunri) during the Meiji Restoration, viewing the bundle as incoherent and an obstacle to national identity. Their efforts largely failed to eradicate the underlying syncretic practices.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__incoherent_bundle, state_authorities_meiji_era, excluded,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Allows for the practical co-existence and intermingling of Shinto and Buddhist practices, rituals, and beliefs within Japanese society, facilitating a broad spiritual landscape that caters to diverse needs without requiring strict doctrinal adherence.
% TRANSFER_FUNCTION: Transfers institutional stability and broad societal acceptance to Shinto shrines and Buddhist temples by allowing them to share patrons and ritual functions, at the cost of theoretical coherence for scholars and practitioners.
% ABSENT_VOICES: Strict monotheists or purists from either Shinto or Buddhist traditions, who would demand a clear, non-contradictory ontological framework, are marginalized by the prevailing institutional inertia that favors practical syncretism.
% DISAPPEARANCE_RATIONALE: If the institutional tolerance for ontological incoherence vanished, forcing a strict, unified doctrine, it would fundamentally alter the landscape of Japanese religious practice, potentially leading to schisms, loss of patronage, and a redefinition of religious identity for millions.
% FOUNDING_PROBLEM: The historical challenge of integrating foreign Buddhism with indigenous Kami worship without either fully supplanting the other, leading to a pragmatic, often contradictory, fusion.
% FOUNDING_PROBLEM_CORROBORATION: Historians of Japanese religion and cultural anthropologists attest to the ongoing tension between theoretical coherence and practical syncretism, noting that attempts at strict separation (e.g., Meiji era) have largely failed to resolve the underlying bundle of contradictions.
narrative_ontology:disappearance_verdict(kami_buddha_ontology__incoherent_bundle, world_rearranges).
narrative_ontology:founding_problem_status(kami_buddha_ontology__incoherent_bundle, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(kami_buddha_ontology__incoherent_bundle, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
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
 *   Extractiveness is moderate (0.6) because the ambiguity imposes intellectual and spiritual costs on those seeking coherence, while benefiting institutions. Suppression is high (0.7) because the institutional apparatus actively resists attempts to impose a single, coherent ontology, preferring the flexibility of the bundle. Theater ratio is moderate (0.4) as some efforts are made to present a unified front, but the primary function is to maintain the beneficial ambiguity. The historical trajectory shows a relatively stable level of extraction and suppression, indicating the enduring nature of this institutional arrangement.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of religious institutions, the 'incoherent bundle' is a highly effective, pragmatic solution for managing diverse spiritual needs and maintaining institutional power. From the perspective of scholars and some practitioners, it is a source of frustration and intellectual dishonesty. The engine's classification will highlight this divergence, showing how the same structure is experienced as beneficial coordination by some and as extractive incoherence by others.
 *
 * DIRECTIONALITY LOGIC:
 *   Shinto shrines and Buddhist temples are beneficiaries, gaining institutional stability and broader appeal from the flexible, bundled approach. Religious institutions, as agenda-setters, actively maintain this ambiguity. Theological scholars and lay practitioners seeking coherence are payers, bearing the costs of intellectual and spiritual frustration. State authorities from the Meiji era are 'excluded' as their attempts to enforce separation were largely unsuccessful in altering the underlying practices.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    institutional_vs_theological_priority,
    'To what extent is the persistence of the ''incoherent bundle'' driven by institutional self-interest (maintaining power, resources, and broad appeal) versus a genuine, albeit complex, theological understanding that embraces paradox?',
    'Comparative analysis of institutional responses to external pressures for doctrinal clarity (e.g., state-mandated separation attempts, internal reform movements). If institutional resistance correlates strongly with threats to power/resources, it supports the self-interest hypothesis.',
    'If primarily institutional self-interest, the extractiveness and suppression metrics are more accurately attributed to rent-seeking. If genuine theological embrace of paradox, the ''incoherence'' might be re-read as a different form of ''coherence'' (e.g., apophatic), potentially lowering perceived extractiveness for some seats.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_vs_theological_priority, conceptual, 'Distinguishing institutional pragmatism from theological paradox.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression of coherent ontological alternatives structural (institutional power, historical precedent) or internalized (cognitive patterns, cultural identity that resists clear distinctions)?',
    'Post-exit suppression trajectory: if individuals or groups attempting to articulate a coherent ontology continue to face internal resistance or social ostracism even after leaving formal institutions, it suggests internalized suppression. If resistance is primarily from institutional gatekeepers, it''s structural.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — the target carries the suppression with them after exit, making genuine ''exit'' from the incoherent bundle more difficult than it appears.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for ontological coherence.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(kami_buddha_ontology__incoherent_bundle, 0, 150).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(kami_tr_t0, kami_buddha_ontology__incoherent_bundle, theater_ratio, 0, 0.3).
narrative_ontology:measurement(kami_tr_t30, kami_buddha_ontology__incoherent_bundle, theater_ratio, 30, 0.35).
narrative_ontology:measurement(kami_tr_t60, kami_buddha_ontology__incoherent_bundle, theater_ratio, 60, 0.4).
narrative_ontology:measurement(kami_tr_t90, kami_buddha_ontology__incoherent_bundle, theater_ratio, 90, 0.38).
narrative_ontology:measurement(kami_tr_t120, kami_buddha_ontology__incoherent_bundle, theater_ratio, 120, 0.4).
narrative_ontology:measurement(kami_tr_t150, kami_buddha_ontology__incoherent_bundle, theater_ratio, 150, 0.4).

% Extraction over time
narrative_ontology:measurement(kami_be_t0, kami_buddha_ontology__incoherent_bundle, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(kami_be_t30, kami_buddha_ontology__incoherent_bundle, base_extractiveness, 30, 0.55).
narrative_ontology:measurement(kami_be_t60, kami_buddha_ontology__incoherent_bundle, base_extractiveness, 60, 0.6).
narrative_ontology:measurement(kami_be_t90, kami_buddha_ontology__incoherent_bundle, base_extractiveness, 90, 0.58).
narrative_ontology:measurement(kami_be_t120, kami_buddha_ontology__incoherent_bundle, base_extractiveness, 120, 0.6).
narrative_ontology:measurement(kami_be_t150, kami_buddha_ontology__incoherent_bundle, base_extractiveness, 150, 0.6).

% Suppression requirement over time
narrative_ontology:measurement(kami_su_t0, kami_buddha_ontology__incoherent_bundle, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(kami_su_t30, kami_buddha_ontology__incoherent_bundle, suppression_requirement, 30, 0.65).
narrative_ontology:measurement(kami_su_t60, kami_buddha_ontology__incoherent_bundle, suppression_requirement, 60, 0.7).
narrative_ontology:measurement(kami_su_t90, kami_buddha_ontology__incoherent_bundle, suppression_requirement, 90, 0.68).
narrative_ontology:measurement(kami_su_t120, kami_buddha_ontology__incoherent_bundle, suppression_requirement, 120, 0.7).
narrative_ontology:measurement(kami_su_t150, kami_buddha_ontology__incoherent_bundle, suppression_requirement, 150, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(kami_buddha_ontology__incoherent_bundle, identity_coordination).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
