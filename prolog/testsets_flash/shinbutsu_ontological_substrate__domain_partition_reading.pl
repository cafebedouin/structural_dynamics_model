% ============================================================================
% CONSTRAINT STORY: shinbutsu_ontological_substrate__domain_partition_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
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
    narrative_ontology:constraint_vindicates/2,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: shinbutsu_ontological_substrate__domain_partition_reading
 *   human_readable: Kami and Buddhas Govern Separate Domains (Domain Partition Reading)
 *   domain: religious_studies/japanese_history/commitment_systems
 *
 * SUMMARY:
 *   This constraint describes the 'domain partition' reading of the
 *   relationship between Kami (Shinto deities) and Buddhas (Buddhist figures)
 *   in Japan, where each governs separate, non-overlapping domains (e.g.,
 *   this-world purity/fertility vs. afterlife salvation). This reading
 *   emphasizes functional coexistence and institutional separation, rather
 *   than ontological fusion. It is a 'rope' because it facilitates
 *   coordination between distinct religious practices without significant
 *   inherent extraction, allowing both to flourish by avoiding direct
 *   conflict over ultimate authority or domain.
 *
 * KEY AGENTS:
 *   - shinto_priesthood: Agenda setter (institutional/generational) — administers Kami rituals, maintains shrines.
 *   - buddhist_clergy: Agenda setter (institutional/generational) — administers Buddhist rites, maintains temples.
 *   - local_communities: Beneficiary/Payer (organized/biographical) — participate in both traditions, benefit from clear roles, bear costs of maintenance.
 *   - state_authorities: Observer (institutional/generational) — historically influenced, but not the primary agent of this reading's internal logic.
 *   - scholarly_observers: Analytical (analytical/civilizational) — analyze the historical and theological underpinnings.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(shinbutsu_ontological_substrate__domain_partition_reading, 0.15).
domain_priors:suppression_score(shinbutsu_ontological_substrate__domain_partition_reading, 0.2).
domain_priors:theater_ratio(shinbutsu_ontological_substrate__domain_partition_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(shinbutsu_ontological_substrate__domain_partition_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(shinbutsu_ontological_substrate__domain_partition_reading, suppression_requirement, 0.2).
narrative_ontology:constraint_metric(shinbutsu_ontological_substrate__domain_partition_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(shinbutsu_ontological_substrate__domain_partition_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(shinbutsu_ontological_substrate__domain_partition_reading, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(shinbutsu_ontological_substrate__domain_partition_reading, rope).
narrative_ontology:human_readable(shinbutsu_ontological_substrate__domain_partition_reading, "Kami and Buddhas Govern Separate Domains (Domain Partition Reading)").
narrative_ontology:topic_domain(shinbutsu_ontological_substrate__domain_partition_reading, "religious_studies/japanese_history/commitment_systems").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(shinbutsu_ontological_substrate__domain_partition_reading, '6d3941b7-72a7-4fe4-b3c1-4f193e90078a').
narrative_ontology:cs_kernel_codification('6d3941b7-72a7-4fe4-b3c1-4f193e90078a', implicit).
narrative_ontology:cs_authority_grounding('6d3941b7-72a7-4fe4-b3c1-4f193e90078a', practice).
narrative_ontology:cs_interpretation_layer_present('6d3941b7-72a7-4fe4-b3c1-4f193e90078a').
narrative_ontology:cs_reading_relation('6d3941b7-72a7-4fe4-b3c1-4f193e90078a', shinbutsu_ontological_substrate__syncretic_fusion_reading, coexists_with).
narrative_ontology:cs_reading_relation('6d3941b7-72a7-4fe4-b3c1-4f193e90078a', shinbutsu_ontological_substrate__incoherent_bundle_reading, coexists_with).
narrative_ontology:cs_axiom('6d3941b7-72a7-4fe4-b3c1-4f193e90078a', foundational, kami_buddha_domain_distinction).
narrative_ontology:cs_axiom_status(kami_buddha_domain_distinction, holdable).
narrative_ontology:cs_axiom_grounding('6d3941b7-72a7-4fe4-b3c1-4f193e90078a', kami_buddha_domain_distinction, conventional).
narrative_ontology:cs_axiom('6d3941b7-72a7-4fe4-b3c1-4f193e90078a', foundational, functional_coexistence_over_ontological_unity).
narrative_ontology:cs_axiom_status(functional_coexistence_over_ontological_unity, holdable).
narrative_ontology:cs_axiom_grounding('6d3941b7-72a7-4fe4-b3c1-4f193e90078a', functional_coexistence_over_ontological_unity, conventional).
narrative_ontology:cs_reference_frame('6d3941b7-72a7-4fe4-b3c1-4f193e90078a', pre_meiji_functional_separation).
narrative_ontology:cs_drift_state('6d3941b7-72a7-4fe4-b3c1-4f193e90078a', contemporary_pluralistic_era, gap(stable, minor, true)).
narrative_ontology:cs_created_at('6d3941b7-72a7-4fe4-b3c1-4f193e90078a', '').
narrative_ontology:cs_kernel_id(shinbutsu_ontological_substrate__domain_partition_reading, shinbutsu_ontological_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(shinbutsu_ontological_substrate__domain_partition_reading, shinto_priesthood).
narrative_ontology:constraint_beneficiary(shinbutsu_ontological_substrate__domain_partition_reading, buddhist_clergy).
narrative_ontology:constraint_beneficiary(shinbutsu_ontological_substrate__domain_partition_reading, local_communities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(shinbutsu_ontological_substrate__domain_partition_reading, local_communities).
narrative_ontology:constraint_vindicates(shinbutsu_ontological_substrate__domain_partition_reading, shinto_purity_doctrine).
narrative_ontology:constraint_vindicates(shinbutsu_ontological_substrate__domain_partition_reading, buddhist_soteriology).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers Shinto shrines and rituals, focusing on this-worldly concerns, purity, and fertility. Benefits from clear domain separation that preserves Shinto identity and avoids direct competition with Buddhism. Their authority is grounded in lineage and tradition.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_substrate__domain_partition_reading, shinto_priesthood, agenda_setter,
    institutional, generational, constrained, national).

% Administers Buddhist temples and rites, focusing on afterlife salvation, merit, and ancestral veneration. Benefits from clear domain separation that preserves Buddhist identity and avoids direct competition with Shinto. Their authority is grounded in lineage and doctrine.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_substrate__domain_partition_reading, buddhist_clergy, agenda_setter,
    institutional, generational, constrained, national).

% Participate in both Shinto and Buddhist practices, often for different life events (e.g., Shinto for birth/marriage, Buddhist for funerals). They benefit from the clear functional division and cultural stability, but bear the costs of supporting both institutions. Their identity is deeply intertwined with these traditions.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_substrate__domain_partition_reading, local_communities, beneficiary,
    organized, biographical, identity_locked, local).
narrative_ontology:stakeholder_secondary_role(shinbutsu_ontological_substrate__domain_partition_reading, local_communities, payer).

% Historically, state authorities have sometimes intervened to enforce or disrupt the separation of Kami and Buddhas (e.g., Shinbutsu-bunri in the Meiji era). In this reading, their role is external, observing and occasionally influencing the religious landscape, but not defining the internal logic of the domain partition itself.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_substrate__domain_partition_reading, state_authorities, observer,
    institutional, generational, analytical, national).

% Academics and researchers who study the history, theology, and sociology of Japanese religions. They analyze the various readings of Shinbutsu relations and their historical implications, providing an external, analytical perspective on the constraint.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_substrate__domain_partition_reading, scholarly_observers, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: It coordinates the coexistence of distinct religious traditions (Shinto and Buddhism) by assigning them separate, non-overlapping domains of influence and ritual practice, preventing conflict over adherents, resources, or ultimate authority.
% TRANSFER_FUNCTION: It facilitates the transfer of spiritual services and cultural practices to local communities from both Shinto and Buddhist institutions, in exchange for their support and adherence, without one tradition extracting from the other's core domain.
% ABSENT_VOICES: Those who advocate for a complete ontological fusion of Kami and Buddhas (the 'syncretic_fusion_reading') or those who see the entire system as an incoherent, state-enforced bundle (the 'incoherent_bundle_reading') would object. They are present in academic discourse but often excluded from the internal self-understanding of institutions adhering to the domain partition.
% DISAPPEARANCE_RATIONALE: If the domain partition vanished, the clear functional boundaries between Shinto and Buddhist practices would dissolve, leading to direct competition, confusion in ritual roles, and a potential loss of distinct identity for both traditions. Religious institutions and local communities would need to renegotiate their roles and relationships, fundamentally altering the religious landscape.
% FOUNDING_PROBLEM: The problem of how two distinct religious traditions (indigenous Shinto and imported Buddhism) could coexist and thrive in Japan without constant conflict over adherents, resources, or theological supremacy.
% FOUNDING_PROBLEM_CORROBORATION: The continued distinct institutional structures of Shinto shrines and Buddhist temples, alongside the ongoing academic debate about the nature of Shinbutsu relations, corroborates that the problem of coexistence and identity maintenance remains live. Scholarly observers and historical records attest to the historical and ongoing need for such a framework, even if its specific interpretation is contested.
narrative_ontology:disappearance_verdict(shinbutsu_ontological_substrate__domain_partition_reading, world_rearranges).
narrative_ontology:founding_problem_status(shinbutsu_ontological_substrate__domain_partition_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(shinbutsu_ontological_substrate__domain_partition_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(shinbutsu_ontological_substrate__domain_partition_reading, 'none', 1).

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
 *   The extractiveness is low (0.15) because the constraint primarily defines boundaries for functional coexistence, minimizing direct competition for resources or adherents. Suppression is low (0.2) as adherence is largely voluntary, driven by cultural practice rather than coercion. Theater ratio is low (0.1) as the functional separation is generally observed in practice. Accessibility collapse is moderate (0.7) because while the domains are distinct, individuals can easily access both traditions. Resistance is low (0.1) as this reading provides a stable framework for religious life.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of both Shinto and Buddhist clergy, this reading provides a clear, functional division of labor that minimizes conflict and allows each tradition to maintain its distinct identity and practices. Local communities also benefit from this clarity. The 'domain partition' reading is largely self-reinforcing through cultural practice, though historical state interventions (like Shinbutsu-bunri) have sometimes attempted to enforce or disrupt this partition, leading to periods of higher suppression.
 *
 * DIRECTIONALITY LOGIC:
 *   Both the Shinto priesthood and Buddhist clergy are beneficiaries and agenda-setters, as they administer their respective domains and benefit from the clear boundaries. Local communities are beneficiaries of the functional coordination but also payers through their support of both institutions. State authorities are observers in this reading, as their role is external to the internal logic of domain partition, though they can influence its enforcement. There are no direct 'victims' in this reading, as the separation is seen as mutually beneficial for coexistence.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint avoids mislabeling coordination as extraction by emphasizing the functional benefits of clear domain separation. The low extractiveness and suppression metrics reflect a system where distinct religious practices can coexist without one systematically exploiting the other. The 'contested' status of the founding problem acknowledges historical shifts and external pressures, but within this specific reading, the coordination function remains primary.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    domain_partition_vs_fusion_ambiguity,
    'Is the relationship between Kami and Buddhas one of distinct, partitioned domains, or an ontological fusion?',
    'Analysis of pre-Meiji theological texts and ritual practices for explicit statements on ontological status versus functional coexistence. Examination of post-Meiji state Shinto rhetoric for evidence of forced separation.',
    'If a genuine ontological fusion (syncretic_fusion_reading) is found, this constraint''s low extractiveness and high coordination function would be re-evaluated as potentially masking a deeper, more complex, and potentially more extractive identity coordination. If the incoherent_bundle_reading is correct, the constraint is a historical artifact of state enforcement, not a genuine coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(domain_partition_vs_fusion_ambiguity, conceptual, 'Ambiguity between domain partition and ontological fusion of Kami and Buddhas.').

omega_variable(
    state_enforcement_vs_natural_coexistence,
    'To what extent was the ''domain partition'' a natural, functional coexistence, versus a product of state-enforced separation (e.g., Shinbutsu-bunri)?',
    'Historical analysis of institutional records, legal decrees, and popular religious practice before and after periods of state intervention. Focus on evidence of spontaneous separation or integration at the local level.',
    'If primarily state-enforced, the constraint''s ''rope'' classification would shift towards ''tangled_rope'' or ''snare'', reflecting the coercive origin and maintenance. If natural, the ''rope'' classification is strengthened.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(state_enforcement_vs_natural_coexistence, empirical, 'Role of state enforcement in establishing/maintaining the domain partition.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(shinbutsu_ontological_substrate__domain_partition_reading, 0, 200).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(shin_tr_t0, shinbutsu_ontological_substrate__domain_partition_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement(shin_tr_t100, shinbutsu_ontological_substrate__domain_partition_reading, theater_ratio, 100, 0.09).
narrative_ontology:measurement(shin_tr_t200, shinbutsu_ontological_substrate__domain_partition_reading, theater_ratio, 200, 0.1).

% Extraction over time
narrative_ontology:measurement(shin_be_t0, shinbutsu_ontological_substrate__domain_partition_reading, base_extractiveness, 0, 0.1).
narrative_ontology:measurement(shin_be_t100, shinbutsu_ontological_substrate__domain_partition_reading, base_extractiveness, 100, 0.12).
narrative_ontology:measurement(shin_be_t200, shinbutsu_ontological_substrate__domain_partition_reading, base_extractiveness, 200, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(shin_su_t0, shinbutsu_ontological_substrate__domain_partition_reading, suppression_requirement, 0, 0.15).
narrative_ontology:measurement(shin_su_t100, shinbutsu_ontological_substrate__domain_partition_reading, suppression_requirement, 100, 0.18).
narrative_ontology:measurement(shin_su_t200, shinbutsu_ontological_substrate__domain_partition_reading, suppression_requirement, 200, 0.2).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(shinbutsu_ontological_substrate__domain_partition_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(shinbutsu_ontological_substrate__domain_partition_reading, 0.08).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'shinbutsu_ontological_substrate' kernel, emphasizing domain partition. It contrasts with the 'syncretic_fusion_reading' (ontological unity) and 'incoherent_bundle_reading' (state-enforced accumulation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
