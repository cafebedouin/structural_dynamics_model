% ============================================================================
% CONSTRAINT STORY: anthropological_record__naturalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_anthropological_record__naturalist_reading, []).

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
 *   constraint_id: anthropological_record__naturalist_reading
 *   human_readable: Naturalist Reading of Human Origins (Scientific Method)
 *   domain: epistemology/philosophy_of_science/anthropology
 *
 * SUMMARY:
 *   This constraint represents the naturalist reading of human origins,
 *   asserting that human origins are materialist (evolution, migration) and
 *   knowable via scientific method. It is one reading of the broader
 *   'anthropological_record' kernel, which also includes creationist and
 *   indigenous epistemology readings. This reading, while foundational to
 *   modern science, operates with high extractiveness and suppression due to
 *   its gatekeeping function, which excludes non-scientific interpretations
 *   and non-credentialed interpreters from authoritative discourse.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(anthropological_record__naturalist_reading, 0.7).
domain_priors:suppression_score(anthropological_record__naturalist_reading, 0.8).
domain_priors:theater_ratio(anthropological_record__naturalist_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(anthropological_record__naturalist_reading, extractiveness, 0.7).
narrative_ontology:constraint_metric(anthropological_record__naturalist_reading, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(anthropological_record__naturalist_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(anthropological_record__naturalist_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(anthropological_record__naturalist_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(anthropological_record__naturalist_reading, tangled_rope).
narrative_ontology:human_readable(anthropological_record__naturalist_reading, "Naturalist Reading of Human Origins (Scientific Method)").
narrative_ontology:topic_domain(anthropological_record__naturalist_reading, "epistemology/philosophy_of_science/anthropology").

domain_priors:requires_active_enforcement(anthropological_record__naturalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(anthropological_record__naturalist_reading, '6c567fb9-07df-4f48-8750-ad79382bc050').
narrative_ontology:cs_kernel_codification('6c567fb9-07df-4f48-8750-ad79382bc050', formalized).
narrative_ontology:cs_authority_grounding('6c567fb9-07df-4f48-8750-ad79382bc050', expertise).
narrative_ontology:cs_interpretation_layer_present('6c567fb9-07df-4f48-8750-ad79382bc050').
narrative_ontology:cs_reading_relation('6c567fb9-07df-4f48-8750-ad79382bc050', anthropological_record__creationist_reading, forecloses).
narrative_ontology:cs_reading_relation('6c567fb9-07df-4f48-8750-ad79382bc050', anthropological_record__indigenous_epistemology_reading, coexists_with).
narrative_ontology:cs_axiom('6c567fb9-07df-4f48-8750-ad79382bc050', foundational, materialist_causation_axiom).
narrative_ontology:cs_axiom_status(materialist_causation_axiom, holdable).
narrative_ontology:cs_axiom_grounding('6c567fb9-07df-4f48-8750-ad79382bc050', materialist_causation_axiom, empirically_contingent).
narrative_ontology:cs_axiom('6c567fb9-07df-4f48-8750-ad79382bc050', foundational, scientific_method_epistemic_primacy).
narrative_ontology:cs_axiom_status(scientific_method_epistemic_primacy, holdable).
narrative_ontology:cs_axiom_grounding('6c567fb9-07df-4f48-8750-ad79382bc050', scientific_method_epistemic_primacy, conventional).
narrative_ontology:cs_reference_frame('6c567fb9-07df-4f48-8750-ad79382bc050', scientific_consensus_paradigm).
narrative_ontology:cs_drift_state('6c567fb9-07df-4f48-8750-ad79382bc050', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('6c567fb9-07df-4f48-8750-ad79382bc050', '').
narrative_ontology:cs_kernel_id(anthropological_record__naturalist_reading, anthropological_record).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(anthropological_record__naturalist_reading, credentialed_scientists).
narrative_ontology:constraint_beneficiary(anthropological_record__naturalist_reading, academic_institutions).
narrative_ontology:constraint_victim(anthropological_record__naturalist_reading, non_credentialed_interpreters).
narrative_ontology:constraint_victim(anthropological_record__naturalist_reading, proponents_of_supernatural_origins).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(anthropological_record__naturalist_reading, scientific_community_at_large).
narrative_ontology:constraint_beneficiary(anthropological_record__naturalist_reading, public_seeking_knowledge).
narrative_ontology:constraint_victim(anthropological_record__naturalist_reading, public_seeking_knowledge).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These are academic anthropologists, evolutionary biologists, and archaeologists who define, research, and interpret the material record of human origins using scientific methods. They benefit from epistemic authority, funding, and career opportunities within this framework.
narrative_ontology:constraint_stakeholder(anthropological_record__naturalist_reading, credentialed_scientists, agenda_setter,
    institutional, biographical, constrained, global).

% Universities, research centers, and museums that house the material record, employ credentialed scientists, and propagate the naturalist understanding of human origins. They benefit from prestige, funding, and control over knowledge production.
narrative_ontology:constraint_stakeholder(anthropological_record__naturalist_reading, academic_institutions, beneficiary,
    institutional, generational, arbitrage, global).

% Individuals or groups who attempt to interpret human origins outside of established scientific methods or academic credentials. They bear the cost of being dismissed, ignored, or actively excluded from authoritative discourse and funding.
narrative_ontology:constraint_stakeholder(anthropological_record__naturalist_reading, non_credentialed_interpreters, payer,
    powerless, immediate, trapped, local).
narrative_ontology:stakeholder_secondary_role(anthropological_record__naturalist_reading, non_credentialed_interpreters, excluded).

% Advocates for creationist or other supernatural explanations of human origins. Their claims are systematically excluded from scientific discourse, and they are often marginalized in public education and policy debates, bearing the cost of epistemic invalidation.
narrative_ontology:constraint_stakeholder(anthropological_record__naturalist_reading, proponents_of_supernatural_origins, payer,
    powerless, generational, identity_locked, universal).
narrative_ontology:stakeholder_secondary_role(anthropological_record__naturalist_reading, proponents_of_supernatural_origins, excluded).

% Communities whose oral traditions and relational epistemologies offer alternative accounts of human origins and continuity with place. They are often excluded from the scientific framework's authoritative interpretation, despite holding deep, sustained knowledge of specific regions and histories.
narrative_ontology:constraint_stakeholder(anthropological_record__naturalist_reading, indigenous_knowledge_holders, excluded,
    organized, civilizational, identity_locked, regional).

% The broader community of scientists who benefit from a coherent, evidence-based framework for understanding human origins, which underpins related fields and contributes to a unified scientific worldview.
narrative_ontology:constraint_stakeholder(anthropological_record__naturalist_reading, scientific_community_at_large, beneficiary,
    organized, generational, constrained, global).

% Individuals who rely on scientific institutions for authoritative information about human origins. They benefit from a consistent, evidence-based narrative but may pay indirectly through taxes or tuition that support the academic system, and by having alternative narratives suppressed.
narrative_ontology:constraint_stakeholder(anthropological_record__naturalist_reading, public_seeking_knowledge, beneficiary,
    moderate, biographical, constrained, universal).
narrative_ontology:stakeholder_secondary_role(anthropological_record__naturalist_reading, public_seeking_knowledge, payer).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Organizes a coherent, evidence-based understanding of human origins, providing a shared framework for research, education, and public discourse based on material evidence and scientific methodology.
% TRANSFER_FUNCTION: Transfers epistemic authority, research funding, and educational resources from alternative explanations to the scientific establishment; transfers prestige and career opportunities to credentialed researchers who adhere to the naturalist framework.
% ABSENT_VOICES: Proponents of creationism, indigenous knowledge holders, and other non-scientific interpreters are structurally excluded from the authoritative discourse. They would argue for the validity of their own epistemologies and interpretations, but their methods and conclusions are deemed invalid by the dominant scientific framework.
% DISAPPEARANCE_RATIONALE: If the naturalist reading and its institutional enforcement vanished overnight, the entire scientific and educational apparatus around human origins would collapse. Research programs, university departments, museum exhibits, and public education curricula would need to be fundamentally rethought or replaced, leading to a massive epistemic and institutional reorganization.
% FOUNDING_PROBLEM: To establish a reliable, verifiable, and universally applicable method for understanding the natural world, including human origins, free from dogma, superstition, and unprovable claims.
% FOUNDING_PROBLEM_CORROBORATION: The ongoing success of scientific inquiry in other domains, the predictive power of evolutionary theory, and the consensus of the global scientific community (outside of those directly benefiting from this specific constraint's gatekeeping) corroborate the utility and necessity of the scientific method for understanding natural phenomena.
narrative_ontology:disappearance_verdict(anthropological_record__naturalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(anthropological_record__naturalist_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(anthropological_record__naturalist_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(anthropological_record__naturalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(anthropological_record__naturalist_reading, 0.7, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(anthropological_record__naturalist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(anthropological_record__naturalist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(anthropological_record__naturalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The high extractiveness (0.70) reflects the concentration of epistemic authority, funding, and career opportunities within the scientific establishment, at the expense of alternative frameworks. Suppression (0.80) is high because the scientific method actively enforces its boundaries, systematically invalidating or ignoring non-scientific claims and non-credentialed voices. The theater ratio (0.20) is relatively low, indicating that the scientific process is largely functional in its stated aims, though some performativity exists in defending its epistemic boundaries. The claimed type is 'tangled_rope' because it genuinely coordinates a shared understanding of human origins (beneficiaries) but does so through asymmetric extraction and active enforcement that marginalizes other ways of knowing (victims).
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of credentialed scientists, this constraint is a necessary 'rope' for rigorous knowledge production. From the perspective of excluded groups, it functions as a 'snare' that actively suppresses alternative narratives and gatekeeps access to resources and legitimacy. The engine's computation of per-seat classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Credentialed scientists and academic institutions are clear beneficiaries and agenda-setters, controlling the narrative and resources. Non-credentialed interpreters and proponents of supernatural origins are targets, bearing the costs of exclusion and epistemic invalidation. Indigenous knowledge holders are also excluded, their distinct epistemologies not recognized as authoritative within this framework. The broader scientific community and the public benefit from a coherent, evidence-based narrative, but the public also bears indirect costs of limited epistemic pluralism.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    epistemic_pluralism_vs_rigor,
    'Is the exclusion of non-scientific epistemologies a necessary boundary for scientific rigor, or an extractive gatekeeping mechanism that suppresses valid alternative ways of knowing?',
    'Analysis of cases where scientific and indigenous epistemologies have successfully collaborated or where non-credentialed contributions have advanced scientific understanding, assessing whether such integration compromises scientific rigor.',
    'If exclusion is primarily extractive, the constraint''s effective suppression is higher than necessary for coordination, suggesting a ''snare'' component. If necessary for rigor, the suppression is a legitimate cost of ''rope'' coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(epistemic_pluralism_vs_rigor, conceptual, 'Whether epistemic boundaries are for rigor or extraction.').

omega_variable(
    credentialing_quality_vs_barrier,
    'Is the academic credentialing system primarily a quality control mechanism ensuring expertise, or a barrier to entry that extracts rents (prestige, funding) from those outside the academic establishment?',
    'Empirical studies comparing the quality and impact of research produced by credentialed vs. non-credentialed individuals (where non-credentialed work gains visibility), or analysis of the economic costs and benefits of academic credentialing.',
    'If credentialing is primarily a barrier, the extractiveness metric is justified, and the constraint leans more towards ''snare''. If it''s primarily quality control, the extractiveness is a necessary cost of ''rope'' coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(credentialing_quality_vs_barrier, empirical, 'Role of credentialing in scientific authority.').

omega_variable(
    kernel_reading_identity,
    'This constraint is the ''naturalist_reading'' of the ''anthropological_record'' kernel. What would change if a sibling reading were adopted as the primary framework?',
    'Conceptual analysis of the structural implications of adopting the ''creationist_reading'' (e.g., shift to theological authority, rejection of material evidence) or the ''indigenous_epistemology_reading'' (e.g., shift to relational authority, oral tradition as primary evidence).',
    'Adopting the ''creationist_reading'' would fundamentally alter the authority grounding and render current scientific methods invalid. Adopting the ''indigenous_epistemology_reading'' would shift the primary beneficiaries and victims, and redefine what constitutes ''evidence'' and ''knowledge''.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Impact of alternative kernel readings on constraint structure.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(anthropological_record__naturalist_reading, 0, 165).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(anth_tr_t0, anthropological_record__naturalist_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(anth_tr_t33, anthropological_record__naturalist_reading, theater_ratio, 33, 0.15).
narrative_ontology:measurement(anth_tr_t66, anthropological_record__naturalist_reading, theater_ratio, 66, 0.18).
narrative_ontology:measurement(anth_tr_t99, anthropological_record__naturalist_reading, theater_ratio, 99, 0.2).
narrative_ontology:measurement(anth_tr_t132, anthropological_record__naturalist_reading, theater_ratio, 132, 0.2).
narrative_ontology:measurement(anth_tr_t165, anthropological_record__naturalist_reading, theater_ratio, 165, 0.2).

% Extraction over time
narrative_ontology:measurement(anth_be_t0, anthropological_record__naturalist_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(anth_be_t33, anthropological_record__naturalist_reading, base_extractiveness, 33, 0.55).
narrative_ontology:measurement(anth_be_t66, anthropological_record__naturalist_reading, base_extractiveness, 66, 0.62).
narrative_ontology:measurement(anth_be_t99, anthropological_record__naturalist_reading, base_extractiveness, 99, 0.67).
narrative_ontology:measurement(anth_be_t132, anthropological_record__naturalist_reading, base_extractiveness, 132, 0.69).
narrative_ontology:measurement(anth_be_t165, anthropological_record__naturalist_reading, base_extractiveness, 165, 0.7).

% Suppression requirement over time
narrative_ontology:measurement(anth_su_t0, anthropological_record__naturalist_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(anth_su_t33, anthropological_record__naturalist_reading, suppression_requirement, 33, 0.6).
narrative_ontology:measurement(anth_su_t66, anthropological_record__naturalist_reading, suppression_requirement, 66, 0.7).
narrative_ontology:measurement(anth_su_t99, anthropological_record__naturalist_reading, suppression_requirement, 99, 0.75).
narrative_ontology:measurement(anth_su_t132, anthropological_record__naturalist_reading, suppression_requirement, 132, 0.78).
narrative_ontology:measurement(anth_su_t165, anthropological_record__naturalist_reading, suppression_requirement, 165, 0.8).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
