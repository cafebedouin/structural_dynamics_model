% ============================================================================
% CONSTRAINT STORY: simultaneous_veneration__pragmatic_incoherence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_simultaneous_veneration__pragmatic_incoherence_reading, []).

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
 *   constraint_id: simultaneous_veneration__pragmatic_incoherence_reading
 *   human_readable: Simultaneous Veneration (Pragmatic Incoherence Reading)
 *   domain: religious_studies/japanese_history
 *
 * SUMMARY:
 *   This constraint describes the state of simultaneous veneration in
 *   pre-Meiji Japan from the perspective that it was never a coherent system,
 *   but rather a pragmatic incoherence sustained by a lack of enforcement
 *   pressure. Practitioners held contradictory beliefs without resolution,
 *   leading to a high degree of latent extraction (cognitive dissonance,
 *   intellectual stagnation) that was only revealed when the Meiji government
 *   imposed the Shinbutsu-bunri (separation of Kami and Buddhas) policy. This
 *   reading views the separation not as an arbitrary rupture, but as the
 *   surfacing of an underlying, unaddressed structural contradiction. The
 *   constraint is claimed as a 'snare' because the coordination story (social
 *   stability) was cover for the extraction of cognitive coherence from
 *   practitioners and intellectual integrity from theologians, with no
 *   genuine resolution offered.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(simultaneous_veneration__pragmatic_incoherence_reading, 0.7).
domain_priors:suppression_score(simultaneous_veneration__pragmatic_incoherence_reading, 0.8).
domain_priors:theater_ratio(simultaneous_veneration__pragmatic_incoherence_reading, 0.6).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(simultaneous_veneration__pragmatic_incoherence_reading, extractiveness, 0.7).
narrative_ontology:constraint_metric(simultaneous_veneration__pragmatic_incoherence_reading, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(simultaneous_veneration__pragmatic_incoherence_reading, theater_ratio, 0.6).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(simultaneous_veneration__pragmatic_incoherence_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(simultaneous_veneration__pragmatic_incoherence_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(simultaneous_veneration__pragmatic_incoherence_reading, snare).
narrative_ontology:human_readable(simultaneous_veneration__pragmatic_incoherence_reading, "Simultaneous Veneration (Pragmatic Incoherence Reading)").
narrative_ontology:topic_domain(simultaneous_veneration__pragmatic_incoherence_reading, "religious_studies/japanese_history").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(simultaneous_veneration__pragmatic_incoherence_reading, 'dcd8384b-422d-4602-8f04-2bb7e9606fd4').
narrative_ontology:cs_kernel_codification('dcd8384b-422d-4602-8f04-2bb7e9606fd4', implicit).
narrative_ontology:cs_authority_grounding('dcd8384b-422d-4602-8f04-2bb7e9606fd4', distributed).
narrative_ontology:cs_reading_relation('dcd8384b-422d-4602-8f04-2bb7e9606fd4', simultaneous_veneration__ontological_fusion_reading, forecloses).
narrative_ontology:cs_reading_relation('dcd8384b-422d-4602-8f04-2bb7e9606fd4', simultaneous_veneration__domain_partition_reading, forecloses).
narrative_ontology:cs_axiom('dcd8384b-422d-4602-8f04-2bb7e9606fd4', foundational, doctrinal_coherence_is_necessary).
narrative_ontology:cs_axiom_status(doctrinal_coherence_is_necessary, holdable).
narrative_ontology:cs_axiom_grounding('dcd8384b-422d-4602-8f04-2bb7e9606fd4', doctrinal_coherence_is_necessary, deontological).
narrative_ontology:cs_axiom('dcd8384b-422d-4602-8f04-2bb7e9606fd4', foundational, unresolved_contradiction_is_extractive).
narrative_ontology:cs_axiom_status(unresolved_contradiction_is_extractive, holdable).
narrative_ontology:cs_axiom_grounding('dcd8384b-422d-4602-8f04-2bb7e9606fd4', unresolved_contradiction_is_extractive, instrumental).
narrative_ontology:cs_reference_frame('dcd8384b-422d-4602-8f04-2bb7e9606fd4', coherent_religious_system).
narrative_ontology:cs_drift_state('dcd8384b-422d-4602-8f04-2bb7e9606fd4', pre_meiji_era, gap(codification_collapse, severe, false)).
narrative_ontology:cs_created_at('dcd8384b-422d-4602-8f04-2bb7e9606fd4', '').
narrative_ontology:cs_kernel_id(simultaneous_veneration__pragmatic_incoherence_reading, simultaneous_veneration).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(simultaneous_veneration__pragmatic_incoherence_reading, local_religious_institutions).
narrative_ontology:constraint_beneficiary(simultaneous_veneration__pragmatic_incoherence_reading, ruling_elites).
narrative_ontology:constraint_victim(simultaneous_veneration__pragmatic_incoherence_reading, common_practitioners).
narrative_ontology:constraint_victim(simultaneous_veneration__pragmatic_incoherence_reading, intellectual_theologians).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefited from the lack of clear doctrinal boundaries, allowing them to serve diverse spiritual needs and collect offerings from both Shinto and Buddhist adherents without needing to resolve contradictions. Their authority was diffuse but stable.
narrative_ontology:constraint_stakeholder(simultaneous_veneration__pragmatic_incoherence_reading, local_religious_institutions, beneficiary,
    organized, generational, constrained, local).

% Benefited from the social stability and lack of religious conflict that the ambiguous, non-enforced simultaneous veneration provided. It allowed for a flexible system of legitimation without requiring costly doctrinal enforcement or choosing sides.
narrative_ontology:constraint_stakeholder(simultaneous_veneration__pragmatic_incoherence_reading, ruling_elites, beneficiary,
    institutional, generational, mobile, national).

% Paid the cost of cognitive dissonance, holding contradictory beliefs simultaneously without a coherent framework. Their spiritual practice was sustained by local custom and ritual, but lacked intellectual integrity or a clear path to resolution, making them vulnerable to later doctrinal impositions.
narrative_ontology:constraint_stakeholder(simultaneous_veneration__pragmatic_incoherence_reading, common_practitioners, payer,
    powerless, biographical, identity_locked, local).

% Struggled to reconcile the inherent contradictions within simultaneous veneration, often developing complex but ultimately unstable syncretic theories. Their intellectual efforts were constrained by the prevailing pragmatic ambiguity, and their attempts at resolution were largely ignored or suppressed by the ruling elites who preferred stability over coherence.
narrative_ontology:constraint_stakeholder(simultaneous_veneration__pragmatic_incoherence_reading, intellectual_theologians, payer,
    moderate, biographical, constrained, national).

% Later imposed the Shinbutsu-bunri (separation of Kami and Buddhas) policy, which this reading sees as revealing the latent incoherence rather than creating a new one. The Meiji state acted to rationalize religious practice for nationalistic purposes, ending the pragmatic ambiguity.
narrative_ontology:constraint_stakeholder(simultaneous_veneration__pragmatic_incoherence_reading, meiji_state, agenda_setter,
    institutional, generational, arbitrage, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Allowed for a broad, inclusive religious landscape where diverse spiritual needs could be met without forcing doctrinal uniformity or conflict, facilitating social cohesion through shared ritual practice.
% TRANSFER_FUNCTION: Transferred spiritual authority and material offerings from common practitioners to local religious institutions and ruling elites, in exchange for diffuse spiritual comfort and social stability, while obscuring underlying contradictions.
% ABSENT_VOICES: Strict doctrinal purists from either Shinto or Buddhist traditions, who would have argued for clear distinctions and exclusive veneration, were marginalized by the prevailing pragmatic ambiguity. Their arguments were not actively suppressed but simply lacked institutional support.
% DISAPPEARANCE_RATIONALE: If the pragmatic incoherence had vanished overnight (e.g., through a sudden, widespread demand for doctrinal clarity), the entire religious landscape of pre-Meiji Japan would have reorganized. Local institutions would have been forced to choose affiliations, ruling elites would have lost a flexible tool for legitimation, and practitioners would have faced a crisis of belief, leading to widespread religious conflict or reform.
% FOUNDING_PROBLEM: The need to integrate indigenous Kami worship with imported Buddhism, and to maintain social and political stability without costly religious conflict or rigid doctrinal enforcement.
% FOUNDING_PROBLEM_CORROBORATION: Historical records and scholarly analysis from outside the directly benefiting religious institutions confirm that the problem of integrating diverse religious practices was a persistent challenge. The Meiji state's later actions to separate Shinto and Buddhism further corroborate that the 'solution' of pragmatic incoherence was ultimately unsustainable and led to a latent, rather than resolved, problem.
narrative_ontology:disappearance_verdict(simultaneous_veneration__pragmatic_incoherence_reading, world_rearranges).
narrative_ontology:founding_problem_status(simultaneous_veneration__pragmatic_incoherence_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(simultaneous_veneration__pragmatic_incoherence_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_gemini+stakeholder_backfill', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(simultaneous_veneration__pragmatic_incoherence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(simultaneous_veneration__pragmatic_incoherence_reading, 0.7, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(simultaneous_veneration__pragmatic_incoherence_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(simultaneous_veneration__pragmatic_incoherence_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(simultaneous_veneration__pragmatic_incoherence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.7) because the system extracted cognitive coherence and intellectual integrity from practitioners and theologians, forcing them to hold contradictory beliefs without resolution. Suppression is also high (0.8) because the lack of enforcement pressure meant there was no institutional mechanism to resolve these contradictions, effectively suppressing any attempts at doctrinal clarity. The theater ratio is moderate (0.6) as much of the 'coordination' was performative ritual that masked underlying incoherence. Accessibility collapse is moderate (0.4) as alternatives (e.g., choosing one tradition over another, developing truly syncretic philosophies) were conceptually available but practically difficult due to social inertia and institutional beneficiaries. Resistance is low (0.3) because the diffuse nature of the incoherence made organized resistance difficult until the Meiji state provided an external catalyst.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the beneficiaries (local institutions, ruling elites), simultaneous veneration was a successful 'rope' that ensured social harmony and flexible spiritual practice. From the perspective of the victims (practitioners, theologians), it was a 'snare' that extracted coherence and intellectual honesty. The engine's classification will highlight this divergence based on the declared roles and metrics.
 *
 * DIRECTIONALITY LOGIC:
 *   Local religious institutions and ruling elites were beneficiaries, as the ambiguity allowed them to maintain power and social stability without costly doctrinal disputes. Common practitioners and intellectual theologians were victims, bearing the costs of cognitive dissonance and intellectual frustration. The Meiji state, while acting as an agenda-setter to resolve the situation, is seen in this reading as revealing the latent extraction rather than initiating it.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    latent_vs_active_extraction,
    'To what extent was the extraction (cognitive dissonance, intellectual incoherence) actively imposed by the system, versus being a latent consequence of unresolved historical development?',
    'Analysis of primary sources for evidence of active suppression of attempts at doctrinal clarity versus passive institutional inertia.',
    'If actively imposed, the ''snare'' classification is strengthened. If primarily latent, the ''theater_ratio'' might be higher, indicating a ''piton'' where the system persisted more by inertia than active benefit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(latent_vs_active_extraction, empirical, 'Distinguishing active imposition from passive consequence in extraction.').

omega_variable(
    meiji_rupture_or_revelation,
    'Was the Meiji Shinbutsu-bunri an external rupture imposed on a functional system, or did it merely reveal the underlying incoherence that this reading posits?',
    'Comparative historical analysis of similar religious separations in other contexts, and detailed examination of pre-Meiji intellectual discourse for signs of internal pressure for resolution.',
    'If a rupture, this reading''s ''snare'' classification might be too strong, and the ''ontological_fusion_reading'' or ''domain_partition_reading'' might gain credence. If a revelation, this reading is strongly supported.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(meiji_rupture_or_revelation, conceptual, 'The nature of the Meiji separation: rupture or revelation of latent incoherence.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(simultaneous_veneration__pragmatic_incoherence_reading, 1600, 1868).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(simu_tr_t1600, simultaneous_veneration__pragmatic_incoherence_reading, theater_ratio, 1600, 0.5).
narrative_ontology:measurement(simu_tr_t1650, simultaneous_veneration__pragmatic_incoherence_reading, theater_ratio, 1650, 0.53).
narrative_ontology:measurement(simu_tr_t1700, simultaneous_veneration__pragmatic_incoherence_reading, theater_ratio, 1700, 0.55).
narrative_ontology:measurement(simu_tr_t1750, simultaneous_veneration__pragmatic_incoherence_reading, theater_ratio, 1750, 0.57).
narrative_ontology:measurement(simu_tr_t1800, simultaneous_veneration__pragmatic_incoherence_reading, theater_ratio, 1800, 0.59).
narrative_ontology:measurement(simu_tr_t1868, simultaneous_veneration__pragmatic_incoherence_reading, theater_ratio, 1868, 0.6).

% Extraction over time
narrative_ontology:measurement(simu_be_t1600, simultaneous_veneration__pragmatic_incoherence_reading, base_extractiveness, 1600, 0.6).
narrative_ontology:measurement(simu_be_t1650, simultaneous_veneration__pragmatic_incoherence_reading, base_extractiveness, 1650, 0.63).
narrative_ontology:measurement(simu_be_t1700, simultaneous_veneration__pragmatic_incoherence_reading, base_extractiveness, 1700, 0.65).
narrative_ontology:measurement(simu_be_t1750, simultaneous_veneration__pragmatic_incoherence_reading, base_extractiveness, 1750, 0.67).
narrative_ontology:measurement(simu_be_t1800, simultaneous_veneration__pragmatic_incoherence_reading, base_extractiveness, 1800, 0.68).
narrative_ontology:measurement(simu_be_t1868, simultaneous_veneration__pragmatic_incoherence_reading, base_extractiveness, 1868, 0.7).

% Suppression requirement over time
narrative_ontology:measurement(simu_su_t1600, simultaneous_veneration__pragmatic_incoherence_reading, suppression_requirement, 1600, 0.7).
narrative_ontology:measurement(simu_su_t1650, simultaneous_veneration__pragmatic_incoherence_reading, suppression_requirement, 1650, 0.73).
narrative_ontology:measurement(simu_su_t1700, simultaneous_veneration__pragmatic_incoherence_reading, suppression_requirement, 1700, 0.75).
narrative_ontology:measurement(simu_su_t1750, simultaneous_veneration__pragmatic_incoherence_reading, suppression_requirement, 1750, 0.77).
narrative_ontology:measurement(simu_su_t1800, simultaneous_veneration__pragmatic_incoherence_reading, suppression_requirement, 1800, 0.79).
narrative_ontology:measurement(simu_su_t1868, simultaneous_veneration__pragmatic_incoherence_reading, suppression_requirement, 1868, 0.8).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(simultaneous_veneration__pragmatic_incoherence_reading, identity_coordination).
narrative_ontology:affects_constraint(simultaneous_veneration__pragmatic_incoherence_reading, meiji_state_shinto_supremacy).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'simultaneous_veneration' kernel. This 'pragmatic_incoherence_reading' emphasizes the unresolved contradictions and latent extraction, which the Meiji state's 'shinto_supremacy' constraint later revealed and exploited.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
