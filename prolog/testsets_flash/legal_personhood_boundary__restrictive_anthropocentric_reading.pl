% ============================================================================
% CONSTRAINT STORY: legal_personhood_boundary__restrictive_anthropocentric_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_legal_personhood_boundary__restrictive_anthropocentric_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: legal_personhood_boundary__restrictive_anthropocentric_reading
 *   human_readable: Legal Personhood Boundary: Restrictive Anthropocentric Reading
 *   domain: legal_philosophy/constitutional_law/rights_theory
 *
 * SUMMARY:
 *   This constraint defines legal personhood as limited to born humans with
 *   demonstrable cognitive capacity. It is a specific reading of the broader
 *   'legal_personhood_boundary' kernel, prioritizing human autonomy and legal
 *   clarity by excluding fetuses, non-human animals, ecosystems, and
 *   artificial intelligences from rights-bearing status. The constraint is
 *   claimed as a Rope due to its coordination function in legal systems, but
 *   its metrics reflect a low level of inherent extraction and suppression,
 *   as it largely aligns with established legal traditions.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(legal_personhood_boundary__restrictive_anthropocentric_reading, 0.3).
domain_priors:suppression_score(legal_personhood_boundary__restrictive_anthropocentric_reading, 0.2).
domain_priors:theater_ratio(legal_personhood_boundary__restrictive_anthropocentric_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(legal_personhood_boundary__restrictive_anthropocentric_reading, extractiveness, 0.3).
narrative_ontology:constraint_metric(legal_personhood_boundary__restrictive_anthropocentric_reading, suppression_requirement, 0.2).
narrative_ontology:constraint_metric(legal_personhood_boundary__restrictive_anthropocentric_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(legal_personhood_boundary__restrictive_anthropocentric_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(legal_personhood_boundary__restrictive_anthropocentric_reading, resistance, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(legal_personhood_boundary__restrictive_anthropocentric_reading, rope).
narrative_ontology:human_readable(legal_personhood_boundary__restrictive_anthropocentric_reading, "Legal Personhood Boundary: Restrictive Anthropocentric Reading").
narrative_ontology:topic_domain(legal_personhood_boundary__restrictive_anthropocentric_reading, "legal_philosophy/constitutional_law/rights_theory").

domain_priors:requires_active_enforcement(legal_personhood_boundary__restrictive_anthropocentric_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(legal_personhood_boundary__restrictive_anthropocentric_reading, '141dce8f-7982-451a-bf31-234d030175d7').
narrative_ontology:cs_kernel_codification('141dce8f-7982-451a-bf31-234d030175d7', formalized).
narrative_ontology:cs_authority_grounding('141dce8f-7982-451a-bf31-234d030175d7', lineage).
narrative_ontology:cs_interpretation_layer_present('141dce8f-7982-451a-bf31-234d030175d7').
narrative_ontology:cs_reading_relation('141dce8f-7982-451a-bf31-234d030175d7', legal_personhood_boundary__developmental_potentiality_reading, forecloses).
narrative_ontology:cs_reading_relation('141dce8f-7982-451a-bf31-234d030175d7', legal_personhood_boundary__functional_capacity_reading, coexists_with).
narrative_ontology:cs_axiom('141dce8f-7982-451a-bf31-234d030175d7', foundational, personhood_requires_born_human_cognition).
narrative_ontology:cs_axiom_status(personhood_requires_born_human_cognition, holdable).
narrative_ontology:cs_axiom_grounding('141dce8f-7982-451a-bf31-234d030175d7', personhood_requires_born_human_cognition, deontological).
narrative_ontology:cs_axiom('141dce8f-7982-451a-bf31-234d030175d7', foundational, bodily_autonomy_precedes_fetal_rights).
narrative_ontology:cs_axiom_status(bodily_autonomy_precedes_fetal_rights, holdable).
narrative_ontology:cs_axiom_grounding('141dce8f-7982-451a-bf31-234d030175d7', bodily_autonomy_precedes_fetal_rights, deontological).
narrative_ontology:cs_reference_frame('141dce8f-7982-451a-bf31-234d030175d7', post_enlightenment_human_rights_framework).
narrative_ontology:cs_drift_state('141dce8f-7982-451a-bf31-234d030175d7', contemporary_rights_expansion_era, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('141dce8f-7982-451a-bf31-234d030175d7', '').
narrative_ontology:cs_kernel_id(legal_personhood_boundary__restrictive_anthropocentric_reading, legal_personhood_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(legal_personhood_boundary__restrictive_anthropocentric_reading, born_humans_with_cognitive_capacity).
narrative_ontology:constraint_beneficiary(legal_personhood_boundary__restrictive_anthropocentric_reading, pregnant_persons).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(legal_personhood_boundary__restrictive_anthropocentric_reading, rights_advocates_for_non_humans).
narrative_ontology:constraint_vindicates(legal_personhood_boundary__restrictive_anthropocentric_reading, human_autonomy_doctrine).
narrative_ontology:constraint_vindicates(legal_personhood_boundary__restrictive_anthropocentric_reading, cognitive_superiority_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These are the primary rights-holders under this reading, whose interests and autonomy are prioritized. They benefit from a clear, stable definition of personhood that grants them full legal protection and agency, without extending it to entities lacking demonstrable cognitive function.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__restrictive_anthropocentric_reading, born_humans_with_cognitive_capacity, beneficiary,
    institutional, generational, analytical, universal).

% Benefit from the maximal recognition of their bodily autonomy and decision-making rights, as the fetus is not considered a separate legal person with competing rights. This minimizes state intervention in reproductive choices.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__restrictive_anthropocentric_reading, pregnant_persons, beneficiary,
    powerful, biographical, mobile, national).

% Administer and enforce the legal framework that defines personhood. They benefit from the clarity and stability of this restrictive definition, which simplifies adjudication by avoiding complex questions of pre-natal or non-human rights.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__restrictive_anthropocentric_reading, legal_systems, agenda_setter,
    institutional, civilizational, constrained, global).

% Are explicitly excluded from legal personhood and its associated rights under this reading. Their interests are mediated through the pregnant person or not recognized independently.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__restrictive_anthropocentric_reading, fetuses, excluded,
    powerless, immediate, trapped, local).

% Are not granted legal personhood, meaning their protection relies on instrumental arguments (e.g., benefit to humans) rather than inherent rights. This minimizes legal avenues for environmental protection based on intrinsic value.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__restrictive_anthropocentric_reading, ecosystems, excluded,
    powerless, generational, trapped, global).

% Are excluded from legal personhood, regardless of their advanced capabilities, due to the emphasis on biological human origin and/or specific forms of cognitive capacity. This prevents legal challenges for AI rights.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__restrictive_anthropocentric_reading, artificial_intelligences, excluded,
    moderate, generational, constrained, global).

% Bear the cost of having their arguments for expanded personhood (e.g., for animals, AI, or ecosystems) rejected by the prevailing legal framework. They must work within or against a system that denies their core premise.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__restrictive_anthropocentric_reading, rights_advocates_for_non_humans, payer,
    organized, generational, mobile, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a clear, stable, and widely understood boundary for legal personhood, simplifying legal and ethical frameworks by limiting rights-bearing entities to born humans with demonstrable cognitive capacity.
% TRANSFER_FUNCTION: Transfers full legal autonomy and protection to born humans with cognitive capacity, while denying it to fetuses, non-human animals, ecosystems, and artificial intelligences. This implicitly transfers decision-making power over these excluded entities to recognized persons.
% ABSENT_VOICES: Advocates for fetal rights, animal rights, environmental personhood, and AI rights are structurally excluded from the foundational definition of personhood. They would argue for a broader, more inclusive definition based on potentiality, sentience, or ecological interdependence.
% DISAPPEARANCE_RATIONALE: If this restrictive definition of personhood vanished, legal systems would face immediate chaos regarding abortion rights, environmental protection, animal welfare, and the status of advanced AI. The entire edifice of rights and responsibilities would need to be re-negotiated, leading to profound societal reorganization.
% FOUNDING_PROBLEM: To establish a clear and manageable basis for legal rights and responsibilities, avoiding ambiguity and potential conflicts arising from extending personhood to entities without fully developed human cognitive functions.
% FOUNDING_PROBLEM_CORROBORATION: Legal scholars and constitutional lawyers, independent of specific advocacy groups, corroborate that the need for a clear and stable definition of legal personhood remains a live problem, even if the specific boundaries are contested. The practical necessity of a defined legal subject is widely acknowledged.
narrative_ontology:disappearance_verdict(legal_personhood_boundary__restrictive_anthropocentric_reading, world_rearranges).
narrative_ontology:founding_problem_status(legal_personhood_boundary__restrictive_anthropocentric_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(legal_personhood_boundary__restrictive_anthropocentric_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(legal_personhood_boundary__restrictive_anthropocentric_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(legal_personhood_boundary__restrictive_anthropocentric_reading_tests).
:- end_tests(legal_personhood_boundary__restrictive_anthropocentric_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.3) because this reading primarily coordinates legal definitions rather than extracting resources directly. Suppression is also low (0.2) as it largely reflects existing legal consensus, though it actively suppresses alternative personhood claims. Theater ratio is minimal (0.1) as the definition is genuinely functional for legal systems. Accessibility collapse is high (0.7) because once this definition is adopted, alternatives for personhood are largely foreclosed within that legal framework. Resistance is low (0.15) from within the established legal system, though significant from external advocacy groups.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of born humans with cognitive capacity and pregnant persons, this constraint is a beneficial Rope, providing clear rights and autonomy. From the perspective of excluded entities (fetuses, ecosystems, AI) and their advocates, it is a Snare, denying fundamental rights. The engine's classification will reflect the aggregate structural position, but the per-seat classification will show this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Born humans with cognitive capacity and pregnant persons are clear beneficiaries (d=0.0-0.1) as their rights are maximized. Legal systems are agenda-setters (d=0.2-0.3) benefiting from clarity. Excluded entities (fetuses, ecosystems, AI) are targets (d=0.9-1.0) as they are denied personhood. Rights advocates for non-humans are payers (d=0.7-0.8) as they bear the cost of challenging the established definition.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint's mandate (providing legal clarity for rights) remains live. The low extractiveness and suppression suggest it has not significantly atrophied into a purely extractive mechanism, though the contestation around its boundaries indicates ongoing pressure. The classification as a Rope, despite some extractive elements, prevents mislabeling a foundational legal coordination mechanism as pure extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is this constraint a genuine, independent legal principle, or merely one reading of the ''legal_personhood_boundary'' kernel?',
    'Analysis of legal precedent and philosophical arguments: if the core tenets of this reading are consistently applied without reference to alternative framings, it strengthens its claim as an independent principle. If it is primarily defined in opposition to other readings, it reinforces its status as a reading.',
    'If an independent principle, its classification stands alone. If a reading, its stability and legitimacy are inherently tied to the ongoing contestation of the kernel, potentially increasing its effective extractiveness due to the resources expended in defending its boundaries.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'This constraint is one reading of the ''legal_personhood_boundary'' kernel, specifically the restrictive_anthropocentric_reading. Sibling readings include developmental_potentiality_reading and functional_capacity_reading. This reading excludes fetuses, ecosystems, and AI from the victim set, maximizes pregnant person autonomy, and minimizes state intervention in reproduction and environmental law.').

omega_variable(
    scope_of_cognitive_capacity,
    'What specific criteria define ''cognitive capacity'' for legal personhood, and how are these criteria applied to edge cases (e.g., individuals with severe cognitive impairments, advanced AI)?',
    'Judicial rulings and legislative definitions clarifying the thresholds and tests for cognitive capacity. Philosophical debate on the nature of consciousness and sentience.',
    'Ambiguity in ''cognitive capacity'' could lead to arbitrary exclusion or inclusion, increasing effective extractiveness for those on the margins. Clearer definitions would reduce this ambiguity, potentially lowering effective extractiveness for those clearly outside the boundary, but hardening the exclusion for those just below it.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scope_of_cognitive_capacity, empirical, 'The precise definition and application of ''cognitive capacity'' as a criterion for legal personhood.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(legal_personhood_boundary__restrictive_anthropocentric_reading, 1970, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lega_tr_t1970, legal_personhood_boundary__restrictive_anthropocentric_reading, theater_ratio, 1970, 0.05).
narrative_ontology:measurement(lega_tr_t1990, legal_personhood_boundary__restrictive_anthropocentric_reading, theater_ratio, 1990, 0.08).
narrative_ontology:measurement(lega_tr_t2010, legal_personhood_boundary__restrictive_anthropocentric_reading, theater_ratio, 2010, 0.09).
narrative_ontology:measurement(lega_tr_t2024, legal_personhood_boundary__restrictive_anthropocentric_reading, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(lega_be_t1970, legal_personhood_boundary__restrictive_anthropocentric_reading, base_extractiveness, 1970, 0.25).
narrative_ontology:measurement(lega_be_t1990, legal_personhood_boundary__restrictive_anthropocentric_reading, base_extractiveness, 1990, 0.28).
narrative_ontology:measurement(lega_be_t2010, legal_personhood_boundary__restrictive_anthropocentric_reading, base_extractiveness, 2010, 0.29).
narrative_ontology:measurement(lega_be_t2024, legal_personhood_boundary__restrictive_anthropocentric_reading, base_extractiveness, 2024, 0.3).

% Suppression requirement over time
narrative_ontology:measurement(lega_su_t1970, legal_personhood_boundary__restrictive_anthropocentric_reading, suppression_requirement, 1970, 0.15).
narrative_ontology:measurement(lega_su_t1990, legal_personhood_boundary__restrictive_anthropocentric_reading, suppression_requirement, 1990, 0.18).
narrative_ontology:measurement(lega_su_t2010, legal_personhood_boundary__restrictive_anthropocentric_reading, suppression_requirement, 2010, 0.19).
narrative_ontology:measurement(lega_su_t2024, legal_personhood_boundary__restrictive_anthropocentric_reading, suppression_requirement, 2024, 0.2).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(legal_personhood_boundary__restrictive_anthropocentric_reading, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
