% ============================================================================
% CONSTRAINT STORY: ost_article_ii_non_appropriation__commons_conservation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ost_article_ii_non_appropriation__commons_conservation, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: ost_article_ii_non_appropriation__commons_conservation
 *   human_readable: OST Article II: Commons Conservation Reading
 *   domain: international_space_law/treaty_interpretation/commons_governance
 *
 * SUMMARY:
 *   This constraint represents the 'commons conservation' reading of Article
 *   II of the Outer Space Treaty (OST), which interprets the 'not subject to
 *   national appropriation by claim of sovereignty, by means of use or
 *   occupation, or by any other means' language as prohibiting de facto
 *   appropriation through resource extraction, extending this principle to
 *   both states and private actors. From this reading's perspective, it acts
 *   as a 'Wall constraint,' preventing unilateral extraction absent
 *   multilateral authorization, thereby stranding first-mover mining
 *   investments and preserving a veto for non-spacefaring states over
 *   enclosure. Benefits are intended to be distributed by negotiation rather
 *   than capability.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ost_article_ii_non_appropriation__commons_conservation, 0.15).
domain_priors:suppression_score(ost_article_ii_non_appropriation__commons_conservation, 0.85).
domain_priors:theater_ratio(ost_article_ii_non_appropriation__commons_conservation, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ost_article_ii_non_appropriation__commons_conservation, extractiveness, 0.15).
narrative_ontology:constraint_metric(ost_article_ii_non_appropriation__commons_conservation, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(ost_article_ii_non_appropriation__commons_conservation, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ost_article_ii_non_appropriation__commons_conservation, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(ost_article_ii_non_appropriation__commons_conservation, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ost_article_ii_non_appropriation__commons_conservation, rope).
narrative_ontology:human_readable(ost_article_ii_non_appropriation__commons_conservation, "OST Article II: Commons Conservation Reading").
narrative_ontology:topic_domain(ost_article_ii_non_appropriation__commons_conservation, "international_space_law/treaty_interpretation/commons_governance").

domain_priors:requires_active_enforcement(ost_article_ii_non_appropriation__commons_conservation).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ost_article_ii_non_appropriation__commons_conservation, '440e7aed-b3f1-4e04-b21b-c1742bbfb3e0').
narrative_ontology:cs_kernel_codification('440e7aed-b3f1-4e04-b21b-c1742bbfb3e0', fixed_text).
narrative_ontology:cs_authority_grounding('440e7aed-b3f1-4e04-b21b-c1742bbfb3e0', lineage).
narrative_ontology:cs_interpretation_layer_present('440e7aed-b3f1-4e04-b21b-c1742bbfb3e0').
narrative_ontology:cs_reading_relation('440e7aed-b3f1-4e04-b21b-c1742bbfb3e0', ost_article_ii_non_appropriation__extraction_permissive, forecloses).
narrative_ontology:cs_reading_relation('440e7aed-b3f1-4e04-b21b-c1742bbfb3e0', ost_article_ii_non_appropriation__international_regime, coexists_with).
narrative_ontology:cs_axiom('440e7aed-b3f1-4e04-b21b-c1742bbfb3e0', foundational, space_resources_as_common_heritage).
narrative_ontology:cs_axiom_status(space_resources_as_common_heritage, holdable).
narrative_ontology:cs_axiom_grounding('440e7aed-b3f1-4e04-b21b-c1742bbfb3e0', space_resources_as_common_heritage, deontological).
narrative_ontology:cs_axiom('440e7aed-b3f1-4e04-b21b-c1742bbfb3e0', foundational, prohibition_on_unilateral_appropriation).
narrative_ontology:cs_axiom_status(prohibition_on_unilateral_appropriation, holdable).
narrative_ontology:cs_axiom_grounding('440e7aed-b3f1-4e04-b21b-c1742bbfb3e0', prohibition_on_unilateral_appropriation, conventional).
narrative_ontology:cs_reference_frame('440e7aed-b3f1-4e04-b21b-c1742bbfb3e0', common_heritage_principle_1967).
narrative_ontology:cs_drift_state('440e7aed-b3f1-4e04-b21b-c1742bbfb3e0', contemporary_resource_rush, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('440e7aed-b3f1-4e04-b21b-c1742bbfb3e0', '').
narrative_ontology:cs_kernel_id(ost_article_ii_non_appropriation__commons_conservation, ost_article_ii_non_appropriation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ost_article_ii_non_appropriation__commons_conservation, non_spacefaring_states).
narrative_ontology:constraint_beneficiary(ost_article_ii_non_appropriation__commons_conservation, future_generations).
narrative_ontology:constraint_beneficiary(ost_article_ii_non_appropriation__commons_conservation, global_commons).
narrative_ontology:constraint_victim(ost_article_ii_non_appropriation__commons_conservation, first_mover_mining_companies).
narrative_ontology:constraint_victim(ost_article_ii_non_appropriation__commons_conservation, spacefaring_states_with_extraction_plans).
narrative_ontology:constraint_vindicates(ost_article_ii_non_appropriation__commons_conservation, common_heritage_of_mankind_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These states lack the capability for space resource extraction but benefit from the principle that prevents unilateral appropriation, ensuring future access and equitable benefit distribution through multilateral negotiation rather than capability alone.
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__commons_conservation, non_spacefaring_states, beneficiary,
    moderate, generational, trapped, global).

% As a non-agent entity, this group benefits from the preservation of space resources and the prevention of their irreversible depletion or enclosure by current actors, ensuring a common heritage for posterity.
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__commons_conservation, future_generations, beneficiary,
    powerless, civilizational, trapped, universal).
narrative_ontology:stakeholder_non_agent(ost_article_ii_non_appropriation__commons_conservation, future_generations).

% The space environment and its resources, as a non-agent entity, are preserved from unilateral exploitation and degradation under this interpretation, maintaining their status as a shared heritage.
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__commons_conservation, global_commons, beneficiary,
    powerless, civilizational, trapped, universal).
narrative_ontology:stakeholder_non_agent(ost_article_ii_non_appropriation__commons_conservation, global_commons).

% These private entities invest heavily in technologies for space resource extraction. This reading of Article II directly prohibits their planned activities, stranding investments and forcing them to seek multilateral authorization, which is currently unavailable.
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__commons_conservation, first_mover_mining_companies, payer,
    organized, biographical, constrained, global).

% States with advanced space capabilities and strategic interests in resource extraction are constrained by this interpretation. It prevents them from unilaterally authorizing or conducting appropriation activities, requiring them to engage in multilateral frameworks where their power might be diluted.
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__commons_conservation, spacefaring_states_with_extraction_plans, payer,
    institutional, generational, constrained, global).

% Academics and legal experts who analyze and interpret international space law, contributing to the discourse around Article II's meaning and implications for space resource governance.
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__commons_conservation, international_legal_scholars, observer,
    analytical, biographical, analytical, global).

% This UN body serves as a forum for international cooperation in space activities. Under this reading, it would be responsible for facilitating multilateral agreements that uphold the non-appropriation principle and govern resource use, effectively setting the agenda for future space governance.
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__commons_conservation, un_committee_on_peaceful_uses_of_outer_space, agenda_setter,
    institutional, generational, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ost_article_ii_non_appropriation__commons_conservation, diffuse).
narrative_ontology:fixing_cost_class(ost_article_ii_non_appropriation__commons_conservation, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To coordinate the conservation of outer space and its resources as a global commons, preventing unilateral appropriation by states or private entities and ensuring equitable access and benefit for all.
% TRANSFER_FUNCTION: This constraint prevents the transfer of valuable space resources and potential wealth from the global commons to individual states or private actors, effectively preserving it for collective, negotiated benefit.
% ABSENT_VOICES: Private space resource prospectors and their lobbying groups, who advocate for property rights in space and would argue that the non-appropriation principle should not extend to extracted resources, are largely excluded from the authoritative interpretive discourse of this reading.
% DISAPPEARANCE_RATIONALE: If this interpretation vanished, it would likely trigger a rapid 'gold rush' for space resources, leading to unilateral claims, potential conflicts, environmental degradation of celestial bodies, and the permanent enclosure of resources by a few technologically advanced actors, fundamentally altering the governance and accessibility of space.
% FOUNDING_PROBLEM: The original Outer Space Treaty aimed to prevent a militarization and territorial 'land grab' in space, ensuring that outer space would be used for the benefit and in the interests of all countries, and preventing its appropriation by any single state.
% FOUNDING_PROBLEM_CORROBORATION: Non-spacefaring states, many international legal scholars, and environmental advocacy groups attest that the founding problem of preventing unilateral appropriation and ensuring equitable access remains highly relevant, especially with emerging space resource technologies. This is corroborated by ongoing debates in UNCOPUOS and academic literature, outside the immediate interests of potential extractors.
narrative_ontology:disappearance_verdict(ost_article_ii_non_appropriation__commons_conservation, world_rearranges).
narrative_ontology:founding_problem_status(ost_article_ii_non_appropriation__commons_conservation, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ost_article_ii_non_appropriation__commons_conservation, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(ost_article_ii_non_appropriation__commons_conservation, 'none', 1).
narrative_ontology:epsilon_provenance(ost_article_ii_non_appropriation__commons_conservation, 0.15, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ost_article_ii_non_appropriation__commons_conservation_tests).
:- end_tests(ost_article_ii_non_appropriation__commons_conservation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The `extractiveness` is low (0.15) because this reading's primary function is to *prevent* extraction and enclosure, thus minimizing the 'cost' of the constraint itself from a conservation perspective. `suppression` is high (0.85) because it actively seeks to block and deter unilateral resource extraction, requiring continuous enforcement against powerful economic and state interests. `theater_ratio` is low (0.10) as this interpretation is a genuine and active effort to uphold a core principle, not a performative one. `accessibility_collapse` is high (0.90) as it aims to completely collapse the legal and practical avenues for unilateral appropriation. `resistance` is high (0.75) due to strong opposition from states and private entities seeking to exploit space resources.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of states and companies seeking to extract space resources (the 'extraction_permissive' reading), this constraint would be perceived as a Snare, actively blocking their economic activities and imposing significant costs. However, from the 'commons conservation' perspective, it functions as a Rope, coordinating the preservation of a shared resource and ensuring equitable future access, preventing a tragedy of the commons.
 *
 * DIRECTIONALITY LOGIC:
 *   Non-spacefaring states and future generations are clear beneficiaries, as the constraint protects their long-term interests in equitable access and resource preservation. First-mover mining companies and spacefaring states with extraction plans are the primary targets/payers, as their unilateral activities are directly suppressed. The UNCOPUOS acts as an agenda-setter, facilitating the multilateral governance consistent with this reading.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    scope_of_appropriation_ambiguity,
    'Does ''use or occupation'' in Article II unambiguously extend to resource extraction and ownership, or is it limited to territorial claims?',
    'A definitive ruling by an international court or a universally adopted interpretive protocol among states.',
    'If limited to territorial claims, this reading''s prohibition on extraction would be weakened, potentially reclassifying it as a Piton or even a Snare from the perspective of those seeking to extract. If confirmed to include extraction, its status as a robust Rope would be strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scope_of_appropriation_ambiguity, conceptual, 'Ambiguity regarding the scope of ''appropriation'' in OST Article II.').

omega_variable(
    enforcement_against_private_actors,
    'How effectively can the non-appropriation principle be enforced against private actors operating under national licenses, given the lack of a dedicated international enforcement body?',
    'Observation of state practice in regulating private space mining, and the outcomes of any international disputes arising from such activities.',
    'If enforcement proves difficult, the constraint''s effective suppression would be lower, potentially shifting its classification towards a Piton due to lack of functional effect. Strong enforcement would reinforce its Rope classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_against_private_actors, empirical, 'Challenges in enforcing non-appropriation against private entities.').

omega_variable(
    mandate_vs_technological_advancement,
    'Is the original mandate of Article II still relevant and robust enough to address the challenges posed by rapidly advancing space resource extraction technologies?',
    'Ongoing international legal and political discourse, and the development of new multilateral agreements or protocols that explicitly address resource extraction.',
    'If the mandate is deemed insufficient, the constraint could be seen as a Piton, with its original function atrophied. If it adapts or is reinforced by new agreements, its Rope classification would be sustained.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(mandate_vs_technological_advancement, empirical, 'Relevance of Article II''s mandate in light of new technologies.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ost_article_ii_non_appropriation__commons_conservation, 1967, 2050).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ost__tr_t1967, ost_article_ii_non_appropriation__commons_conservation, theater_ratio, 1967, 0.05).
narrative_ontology:measurement(ost__tr_t1980, ost_article_ii_non_appropriation__commons_conservation, theater_ratio, 1980, 0.07).
narrative_ontology:measurement(ost__tr_t2000, ost_article_ii_non_appropriation__commons_conservation, theater_ratio, 2000, 0.08).
narrative_ontology:measurement(ost__tr_t2020, ost_article_ii_non_appropriation__commons_conservation, theater_ratio, 2020, 0.09).
narrative_ontology:measurement(ost__tr_t2035, ost_article_ii_non_appropriation__commons_conservation, theater_ratio, 2035, 0.1).
narrative_ontology:measurement(ost__tr_t2050, ost_article_ii_non_appropriation__commons_conservation, theater_ratio, 2050, 0.1).

% Extraction over time
narrative_ontology:measurement(ost__be_t1967, ost_article_ii_non_appropriation__commons_conservation, base_extractiveness, 1967, 0.1).
narrative_ontology:measurement(ost__be_t1980, ost_article_ii_non_appropriation__commons_conservation, base_extractiveness, 1980, 0.12).
narrative_ontology:measurement(ost__be_t2000, ost_article_ii_non_appropriation__commons_conservation, base_extractiveness, 2000, 0.13).
narrative_ontology:measurement(ost__be_t2020, ost_article_ii_non_appropriation__commons_conservation, base_extractiveness, 2020, 0.14).
narrative_ontology:measurement(ost__be_t2035, ost_article_ii_non_appropriation__commons_conservation, base_extractiveness, 2035, 0.15).
narrative_ontology:measurement(ost__be_t2050, ost_article_ii_non_appropriation__commons_conservation, base_extractiveness, 2050, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(ost__su_t1967, ost_article_ii_non_appropriation__commons_conservation, suppression_requirement, 1967, 0.7).
narrative_ontology:measurement(ost__su_t1980, ost_article_ii_non_appropriation__commons_conservation, suppression_requirement, 1980, 0.75).
narrative_ontology:measurement(ost__su_t2000, ost_article_ii_non_appropriation__commons_conservation, suppression_requirement, 2000, 0.8).
narrative_ontology:measurement(ost__su_t2020, ost_article_ii_non_appropriation__commons_conservation, suppression_requirement, 2020, 0.83).
narrative_ontology:measurement(ost__su_t2035, ost_article_ii_non_appropriation__commons_conservation, suppression_requirement, 2035, 0.84).
narrative_ontology:measurement(ost__su_t2050, ost_article_ii_non_appropriation__commons_conservation, suppression_requirement, 2050, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ost_article_ii_non_appropriation__commons_conservation, enforcement_mechanism).
narrative_ontology:affects_constraint(ost_article_ii_non_appropriation__commons_conservation, ost_article_ix_harmful_contamination).
narrative_ontology:affects_constraint(ost_article_ii_non_appropriation__commons_conservation, ost_article_vi_state_responsibility).
narrative_ontology:affects_constraint(ost_article_ii_non_appropriation__commons_conservation, ost_article_xi_international_cooperation).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'ost_article_ii_non_appropriation' kernel. It asserts a strong prohibition on resource appropriation, contrasting with 'extraction_permissive' and 'international_regime' readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
