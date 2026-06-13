% ============================================================================
% CONSTRAINT STORY: shinbutsu_ontological_commitment__syncretic_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_shinbutsu_ontological_commitment__syncretic_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: shinbutsu_ontological_commitment__syncretic_reading
 *   human_readable: Honji-Suijaku Metaphysics: Kami and Buddhas as Unified Cosmological Order (Syncretic Reading)
 *   domain: religious_studies/japanese_history/ontology_of_practice
 *
 * SUMMARY:
 *   This constraint describes the 'syncretic reading' of the relationship
 *   between kami (Shinto deities) and buddhas (Buddhist enlightened beings)
 *   under honji-suijaku (original ground and trace manifestation) metaphysics
 *   in pre-modern Japan. In this reading, kami are understood as local
 *   manifestations or 'traces' of universal buddhas, who are the 'original
 *   ground.' This framework facilitated the integration of Shinto and
 *   Buddhism, but often resulted in the subordination of Shinto institutions
 *   and practices to Buddhist ones. The constraint is claimed as a Rope by
 *   its proponents (a beneficial coordination), but its operation was
 *   substantially extractive and suppressive for Shinto elements, making it a
 *   Tangled Rope in practice.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(shinbutsu_ontological_commitment__syncretic_reading, 0.65).
domain_priors:suppression_score(shinbutsu_ontological_commitment__syncretic_reading, 0.7).
domain_priors:theater_ratio(shinbutsu_ontological_commitment__syncretic_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(shinbutsu_ontological_commitment__syncretic_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(shinbutsu_ontological_commitment__syncretic_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(shinbutsu_ontological_commitment__syncretic_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(shinbutsu_ontological_commitment__syncretic_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(shinbutsu_ontological_commitment__syncretic_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(shinbutsu_ontological_commitment__syncretic_reading, tangled_rope).
narrative_ontology:human_readable(shinbutsu_ontological_commitment__syncretic_reading, "Honji-Suijaku Metaphysics: Kami and Buddhas as Unified Cosmological Order (Syncretic Reading)").
narrative_ontology:topic_domain(shinbutsu_ontological_commitment__syncretic_reading, "religious_studies/japanese_history/ontology_of_practice").

domain_priors:requires_active_enforcement(shinbutsu_ontological_commitment__syncretic_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(shinbutsu_ontological_commitment__syncretic_reading, '6fcf50a4-e962-4ccc-9252-ed4f9c646848').
narrative_ontology:cs_kernel_codification('6fcf50a4-e962-4ccc-9252-ed4f9c646848', formalized).
narrative_ontology:cs_authority_grounding('6fcf50a4-e962-4ccc-9252-ed4f9c646848', lineage).
narrative_ontology:cs_interpretation_layer_present('6fcf50a4-e962-4ccc-9252-ed4f9c646848').
narrative_ontology:cs_reading_relation('6fcf50a4-e962-4ccc-9252-ed4f9c646848', shinbutsu_ontological_commitment__partition_reading, forecloses).
narrative_ontology:cs_reading_relation('6fcf50a4-e962-4ccc-9252-ed4f9c646848', shinbutsu_ontological_commitment__incoherence_reading, forecloses).
narrative_ontology:cs_axiom('6fcf50a4-e962-4ccc-9252-ed4f9c646848', foundational, kami_are_buddha_traces).
narrative_ontology:cs_axiom_status(kami_are_buddha_traces, holdable).
narrative_ontology:cs_axiom_grounding('6fcf50a4-e962-4ccc-9252-ed4f9c646848', kami_are_buddha_traces, theological).
narrative_ontology:cs_axiom('6fcf50a4-e962-4ccc-9252-ed4f9c646848', foundational, buddhist_cosmology_is_universal).
narrative_ontology:cs_axiom_status(buddhist_cosmology_is_universal, holdable).
narrative_ontology:cs_axiom_grounding('6fcf50a4-e962-4ccc-9252-ed4f9c646848', buddhist_cosmology_is_universal, theological).
narrative_ontology:cs_reference_frame('6fcf50a4-e962-4ccc-9252-ed4f9c646848', unified_buddhist_shinto_cosmology).
narrative_ontology:cs_drift_state('6fcf50a4-e962-4ccc-9252-ed4f9c646848', meiji_restoration_shinbutsu_bunri, gap(repudiation_pressure, severe, true)).
narrative_ontology:cs_created_at('6fcf50a4-e962-4ccc-9252-ed4f9c646848', '').
narrative_ontology:cs_kernel_id(shinbutsu_ontological_commitment__syncretic_reading, shinbutsu_ontological_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(shinbutsu_ontological_commitment__syncretic_reading, buddhist_institutions).
narrative_ontology:constraint_beneficiary(shinbutsu_ontological_commitment__syncretic_reading, buddhist_clergy).
narrative_ontology:constraint_victim(shinbutsu_ontological_commitment__syncretic_reading, shinto_shrines).
narrative_ontology:constraint_victim(shinbutsu_ontological_commitment__syncretic_reading, shinto_priests).
narrative_ontology:constraint_victim(shinbutsu_ontological_commitment__syncretic_reading, local_kami_cults).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(shinbutsu_ontological_commitment__syncretic_reading, imperial_court).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Promoted and enforced the honji-suijaku doctrine, integrating kami into a Buddhist-centric cosmology. Benefited from increased patronage, landholdings, and doctrinal authority over local kami cults and Shinto shrines.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_commitment__syncretic_reading, buddhist_institutions, agenda_setter,
    institutional, generational, arbitrage, national).

% Gained prestige, intellectual framework, and access to new ritual domains by incorporating kami worship into Buddhist practice. Their interpretive authority was enhanced by the syncretic framework.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_commitment__syncretic_reading, buddhist_clergy, beneficiary,
    powerful, biographical, mobile, national).

% Often became subordinate to Buddhist temples, losing autonomy, land, and direct control over their own rituals and finances. Their kami were reinterpreted as local manifestations of universal buddhas, diminishing their unique identity.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_commitment__syncretic_reading, shinto_shrines, payer,
    moderate, generational, constrained, local).

% Saw their traditional roles diminished or absorbed into Buddhist frameworks. Many became administrators of shrines under Buddhist control, or their practices were recontextualized. Their professional identity was often fused with the syncretic order.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_commitment__syncretic_reading, shinto_priests, payer,
    powerless, biographical, identity_locked, local).

% Were often forcibly integrated or reinterpreted, losing their distinct local traditions and direct connection to their kami. Their practices were either suppressed or subsumed into the dominant Buddhist narrative.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_commitment__syncretic_reading, local_kami_cults, payer,
    powerless, generational, trapped, local).

% Supported the syncretic framework as a means of consolidating religious and political authority, benefiting from a unified cosmological order that legitimized imperial rule. Their support provided institutional backing for Buddhist dominance.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_commitment__syncretic_reading, imperial_court, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(shinbutsu_ontological_commitment__syncretic_reading, imperial_court, beneficiary).

% Would later emerge to reject the syncretic reading, advocating for a pure, independent Shinto. Their voices were absent during the period of honji-suijaku dominance, but their later arguments highlight the historical suppression of Shinto autonomy.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_commitment__syncretic_reading, modern_shinto_revivalists, excluded,
    organized, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provided a coherent cosmological framework that integrated diverse local kami cults and imported Buddhism into a single, unified religious system, reducing doctrinal conflict and facilitating institutional cooperation across religious lines.
% TRANSFER_FUNCTION: Transferred doctrinal authority, institutional resources, and ritual control from local Shinto shrines and priests to Buddhist temples and clergy, in exchange for cosmological legitimation and integration into a broader religious order.
% ABSENT_VOICES: Early proponents of a distinct, independent Shinto tradition, and local kami cults whose unique practices and beliefs were subsumed or suppressed. They would have argued for the inherent autonomy and distinctness of kami worship, separate from Buddhist metaphysics.
% DISAPPEARANCE_RATIONALE: If the honji-suijaku framework vanished overnight, the entire religious landscape of pre-modern Japan would be unrecognizable. The institutional structures, landholdings, ritual practices, and doctrinal justifications of both Shinto and Buddhism were deeply intertwined with this syncretic understanding. Its disappearance would necessitate a complete reorganization of religious authority and practice.
% FOUNDING_PROBLEM: The challenge of integrating an indigenous animistic tradition (Shinto) with a sophisticated foreign religion (Buddhism) without either completely displacing the other, while also providing a unified spiritual basis for political authority.
% FOUNDING_PROBLEM_CORROBORATION: The problem of initial integration is largely solved, and the syncretic framework was formally dismantled during the Meiji Restoration's Shinbutsu-Bunri (separation of kami and buddhas). Modern historians and religious scholars, from outside the historical Buddhist institutions, corroborate that the original problem is dead, and the constraint persisted due to institutional inertia and power dynamics rather than ongoing necessity.
narrative_ontology:disappearance_verdict(shinbutsu_ontological_commitment__syncretic_reading, world_rearranges).
narrative_ontology:founding_problem_status(shinbutsu_ontological_commitment__syncretic_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(shinbutsu_ontological_commitment__syncretic_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(shinbutsu_ontological_commitment__syncretic_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(shinbutsu_ontological_commitment__syncretic_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(shinbutsu_ontological_commitment__syncretic_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(shinbutsu_ontological_commitment__syncretic_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.65) because Buddhist institutions gained significant land, patronage, and doctrinal authority at the expense of Shinto autonomy. Suppression is also high (0.7) as the framework was actively enforced by powerful Buddhist institutions and the imperial court, often leading to the forced integration or marginalization of independent Shinto practices. The theater ratio is low (0.2) because the metaphysical framework was genuinely believed and actively shaped religious practice, even if it served extractive ends. The historical trajectory shows increasing extractiveness and suppression as Buddhist institutions consolidated power over centuries.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of Buddhist institutions and clergy, this was a successful coordination mechanism that brought order and coherence to the religious landscape, allowing for the peaceful coexistence and mutual enrichment of two traditions. From the perspective of Shinto shrines, priests, and local kami cults, it was a system of subordination and extraction, where their indigenous traditions were reinterpreted and controlled by a dominant foreign religion. The engine's per-seat classification would reflect this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Buddhist institutions and clergy are clear beneficiaries (d near 0.0) as they gained authority and resources. Shinto shrines, priests, and local kami cults are victims/targets (d near 1.0) as they lost autonomy and resources. The Imperial Court acted as both an agenda-setter and beneficiary, using the syncretic order to consolidate political control. Modern Shinto revivalists are 'excluded' voices, representing a later rejection of this historical power dynamic.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's founding problem (integrating Shinto and Buddhism) was arguably 'solved' by the syncretic framework, but the framework itself became a mechanism for sustained extraction. The 'dead' status of the founding problem, coupled with the 'world_rearranges' verdict for its disappearance, indicates a mandatrophic state where the constraint's original function atrophied, but its persistence was maintained by the beneficiaries for extractive purposes. This prevents mislabeling it as a pure Rope, highlighting the embedded extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    syncretic_vs_partition_coexistence,
    'To what extent did the syncretic reading truly unify Shinto and Buddhist cosmologies, versus merely creating a functional partition where they operated in separate domains (e.g., life-cycle events vs. afterlife)?',
    'Detailed analysis of local religious practices and textual interpretations across different regions and time periods, focusing on whether practitioners genuinely perceived a unified cosmology or simply a division of labor.',
    'If a functional partition was dominant, the ''syncretic reading'' was more of a rhetorical cover for institutional coexistence, and its extractiveness might be lower than measured, as Shinto autonomy was less metaphysically ''suppressed'' and more institutionally ''circumscribed.'' This would shift the classification closer to a Rope or even a Mountain of cultural practice.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(syncretic_vs_partition_coexistence, empirical, 'Ambiguity between genuine cosmological unification and functional division of religious labor.').

omega_variable(
    institutional_vs_doctrinal_suppression,
    'Was the suppression of Shinto autonomy primarily institutional (e.g., land confiscation, forced temple-shrine mergers) or doctrinal (e.g., reinterpretation of kami as Buddhist traces, diminishing their unique identity)?',
    'Historical analysis distinguishing between periods and regions where institutional control was paramount versus where doctrinal reinterpretation was the primary mechanism of subordination. Examination of resistance movements and their targets.',
    'If primarily institutional, the constraint''s suppression is more directly coercive and less ''identity-locked'' for Shinto priests. If primarily doctrinal, the suppression is more insidious, shaping the very identity and self-understanding of Shinto practitioners, making exit (reasserting pure Shinto identity) more difficult and the constraint more deeply embedded.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_vs_doctrinal_suppression, empirical, 'Distinguishing between institutional and doctrinal mechanisms of suppression.').

omega_variable(
    kernel_reading_identity,
    'Is this constraint a genuine ''syncretic reading'' of the shinbutsu ontological commitment, or is it better understood as a ''Buddhist hegemonic reading'' that merely used syncretism as a tool for dominance?',
    'Analysis of primary sources from both Buddhist and Shinto perspectives, focusing on the intent and self-understanding of the proponents of honji-suijaku. If the intent was genuinely mutual integration, it''s syncretic; if the intent was primarily to absorb and control, it''s hegemonic.',
    'If a ''Buddhist hegemonic reading,'' the extractiveness and suppression metrics are more accurately attributed to a Snare, as the coordination story (syncretism) would be a cover for pure extraction. If a genuine ''syncretic reading,'' the Tangled Rope classification holds, acknowledging a real, albeit asymmetric, coordination function.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Distinguishing between a genuine syncretic reading and a hegemonic reading using syncretism as a tool.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(shinbutsu_ontological_commitment__syncretic_reading, 700, 1868).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(shin_tr_t700, shinbutsu_ontological_commitment__syncretic_reading, theater_ratio, 700, 0.1).
narrative_ontology:measurement(shin_tr_t900, shinbutsu_ontological_commitment__syncretic_reading, theater_ratio, 900, 0.15).
narrative_ontology:measurement(shin_tr_t1200, shinbutsu_ontological_commitment__syncretic_reading, theater_ratio, 1200, 0.2).
narrative_ontology:measurement(shin_tr_t1500, shinbutsu_ontological_commitment__syncretic_reading, theater_ratio, 1500, 0.2).
narrative_ontology:measurement(shin_tr_t1868, shinbutsu_ontological_commitment__syncretic_reading, theater_ratio, 1868, 0.2).

% Extraction over time
narrative_ontology:measurement(shin_be_t700, shinbutsu_ontological_commitment__syncretic_reading, base_extractiveness, 700, 0.4).
narrative_ontology:measurement(shin_be_t900, shinbutsu_ontological_commitment__syncretic_reading, base_extractiveness, 900, 0.5).
narrative_ontology:measurement(shin_be_t1200, shinbutsu_ontological_commitment__syncretic_reading, base_extractiveness, 1200, 0.6).
narrative_ontology:measurement(shin_be_t1500, shinbutsu_ontological_commitment__syncretic_reading, base_extractiveness, 1500, 0.65).
narrative_ontology:measurement(shin_be_t1868, shinbutsu_ontological_commitment__syncretic_reading, base_extractiveness, 1868, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(shin_su_t700, shinbutsu_ontological_commitment__syncretic_reading, suppression_requirement, 700, 0.3).
narrative_ontology:measurement(shin_su_t900, shinbutsu_ontological_commitment__syncretic_reading, suppression_requirement, 900, 0.5).
narrative_ontology:measurement(shin_su_t1200, shinbutsu_ontological_commitment__syncretic_reading, suppression_requirement, 1200, 0.65).
narrative_ontology:measurement(shin_su_t1500, shinbutsu_ontological_commitment__syncretic_reading, suppression_requirement, 1500, 0.7).
narrative_ontology:measurement(shin_su_t1868, shinbutsu_ontological_commitment__syncretic_reading, suppression_requirement, 1868, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(shinbutsu_ontological_commitment__syncretic_reading, identity_coordination).
narrative_ontology:affects_constraint(shinbutsu_ontological_commitment__syncretic_reading, shinbutsu_ontological_commitment__partition_reading).
narrative_ontology:affects_constraint(shinbutsu_ontological_commitment__syncretic_reading, shinbutsu_ontological_commitment__incoherence_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'shinbutsu_ontological_commitment' kernel. It represents the 'syncretic_reading' where kami are seen as traces of buddhas. Sibling readings include 'partition_reading' (Shinto and Buddhism occupy separate domains) and 'incoherence_reading' (no stable ontological commitment existed).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
