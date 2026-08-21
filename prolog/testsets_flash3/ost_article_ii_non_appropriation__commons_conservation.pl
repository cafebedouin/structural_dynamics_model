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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: ost_article_ii_non_appropriation__commons_conservation
 *   human_readable: Outer Space Treaty Article II Non-Appropriation (Commons Conservation Reading)
 *   domain: international_space_law/treaty_interpretation/commons_governance
 *
 * SUMMARY:
 *   This constraint represents the 'commons conservation' reading of Outer
 *   Space Treaty (OST) Article II, which interprets the 'not subject to
 *   national appropriation by claim of sovereignty, by means of use or
 *   occupation, or by any other means' language as prohibiting de facto
 *   appropriation of space resources through extraction by both states and
 *   private actors. This reading acts as a 'wall constraint,' effectively
 *   prohibiting unilateral space mining absent a multilateral authorization
 *   regime. It benefits non-spacefaring states by preserving their future
 *   access and veto power, while imposing significant costs on spacefaring
 *   states and private companies with extraction plans.
 *
 * KEY AGENTS:
 *   - non_spacefaring_states: Primary beneficiary (organized/constrained) — preserve veto over enclosure.
 *   - future_generations: Primary beneficiary (powerless/trapped) — long-term preservation of resources.
 *   - space_mining_companies: Primary target (powerful/constrained) — stranded investments, foregone profits.
 *   - spacefaring_states_with_extraction_plans: Primary target (institutional/constrained) — limitations on national programs.
 *   - international_legal_scholars_conservationist: Agenda setter (analytical/analytical) — shape legal discourse.
 *   - first_mover_investors: Payer (moderate/trapped) — direct financial losses.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ost_article_ii_non_appropriation__commons_conservation, 0.85).
domain_priors:suppression_score(ost_article_ii_non_appropriation__commons_conservation, 0.9).
domain_priors:theater_ratio(ost_article_ii_non_appropriation__commons_conservation, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ost_article_ii_non_appropriation__commons_conservation, extractiveness, 0.85).
narrative_ontology:constraint_metric(ost_article_ii_non_appropriation__commons_conservation, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(ost_article_ii_non_appropriation__commons_conservation, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ost_article_ii_non_appropriation__commons_conservation, accessibility_collapse, 0.95).
narrative_ontology:constraint_metric(ost_article_ii_non_appropriation__commons_conservation, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ost_article_ii_non_appropriation__commons_conservation, snare).
narrative_ontology:human_readable(ost_article_ii_non_appropriation__commons_conservation, "Outer Space Treaty Article II Non-Appropriation (Commons Conservation Reading)").
narrative_ontology:topic_domain(ost_article_ii_non_appropriation__commons_conservation, "international_space_law/treaty_interpretation/commons_governance").

domain_priors:requires_active_enforcement(ost_article_ii_non_appropriation__commons_conservation).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ost_article_ii_non_appropriation__commons_conservation, 'e887ecf6-607a-41d6-aa1c-9befdd801193').
narrative_ontology:cs_kernel_codification('e887ecf6-607a-41d6-aa1c-9befdd801193', fixed_text).
narrative_ontology:cs_authority_grounding('e887ecf6-607a-41d6-aa1c-9befdd801193', lineage).
narrative_ontology:cs_interpretation_layer_present('e887ecf6-607a-41d6-aa1c-9befdd801193').
narrative_ontology:cs_reading_relation('e887ecf6-607a-41d6-aa1c-9befdd801193', ost_article_ii_non_appropriation__extraction_permissive, forecloses).
narrative_ontology:cs_reading_relation('e887ecf6-607a-41d6-aa1c-9befdd801193', ost_article_ii_non_appropriation__international_regime, coexists_with).
narrative_ontology:cs_axiom('e887ecf6-607a-41d6-aa1c-9befdd801193', foundational, outer_space_as_global_commons).
narrative_ontology:cs_axiom_status(outer_space_as_global_commons, holdable).
narrative_ontology:cs_axiom_grounding('e887ecf6-607a-41d6-aa1c-9befdd801193', outer_space_as_global_commons, deontological).
narrative_ontology:cs_axiom('e887ecf6-607a-41d6-aa1c-9befdd801193', foundational, de_facto_appropriation_prohibited).
narrative_ontology:cs_axiom_status(de_facto_appropriation_prohibited, holdable).
narrative_ontology:cs_axiom_grounding('e887ecf6-607a-41d6-aa1c-9befdd801193', de_facto_appropriation_prohibited, conventional).
narrative_ontology:cs_reference_frame('e887ecf6-607a-41d6-aa1c-9befdd801193', original_treaty_intent_prevent_enclosure).
narrative_ontology:cs_drift_state('e887ecf6-607a-41d6-aa1c-9befdd801193', contemporary_space_mining_proposals, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('e887ecf6-607a-41d6-aa1c-9befdd801193', '').
narrative_ontology:cs_kernel_id(ost_article_ii_non_appropriation__commons_conservation, ost_article_ii_non_appropriation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ost_article_ii_non_appropriation__commons_conservation, non_spacefaring_states).
narrative_ontology:constraint_beneficiary(ost_article_ii_non_appropriation__commons_conservation, future_generations).
narrative_ontology:constraint_victim(ost_article_ii_non_appropriation__commons_conservation, space_mining_companies).
narrative_ontology:constraint_victim(ost_article_ii_non_appropriation__commons_conservation, spacefaring_states_with_extraction_plans).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(ost_article_ii_non_appropriation__commons_conservation, first_mover_investors).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from the preservation of outer space as a global commons, preventing its enclosure by technologically advanced states or private entities. They lack the capability to extract resources themselves and thus seek to maintain a veto over unilateral appropriation.
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__commons_conservation, non_spacefaring_states, beneficiary,
    organized, generational, constrained, global).

% Benefit from the long-term preservation of outer space resources and environments, ensuring equitable access and preventing irreversible damage from early, unregulated exploitation. Their interests are represented by conservation advocates.
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__commons_conservation, future_generations, beneficiary,
    powerless, civilizational, trapped, universal).
narrative_ontology:stakeholder_non_agent(ost_article_ii_non_appropriation__commons_conservation, future_generations).

% Bear the cost of stranded investments and foregone profits due to the prohibition on unilateral resource extraction. They seek legal clarity or a permissive interpretation to legitimize their operations and secure property rights over extracted materials.
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__commons_conservation, space_mining_companies, payer,
    powerful, immediate, constrained, global).

% Bear the cost of limitations on their national space programs' ability to secure resources for economic or strategic advantage. They often support permissive interpretations of Article II to enable their private sector or state-backed entities to proceed with extraction.
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__commons_conservation, spacefaring_states_with_extraction_plans, payer,
    institutional, biographical, constrained, global).

% Actively interpret and advocate for the commons conservation reading, shaping legal discourse and influencing policy debates. They provide the intellectual framework for challenging unilateral appropriation claims.
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__commons_conservation, international_legal_scholars_conservationist, agenda_setter,
    analytical, generational, analytical, global).

% Have made significant financial and technological investments in anticipation of space resource extraction. This reading directly threatens their business model, potentially stranding their capital without a clear path to return on investment.
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__commons_conservation, first_mover_investors, payer,
    moderate, immediate, trapped, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a framework for managing outer space as a global commons, preventing a 'tragedy of the commons' scenario where individual actors deplete or enclose resources for private gain, ensuring equitable access and benefit-sharing for all states.
% TRANSFER_FUNCTION: Transfers the right to unilaterally appropriate space resources from individual states and private entities to the collective international community, requiring multilateral authorization for any extraction activities. It also transfers potential future resource wealth from first-movers to a collectively managed pool.
% ABSENT_VOICES: Space resource prospectors and early-stage mining companies, who would argue for a 'finders keepers' approach to resource ownership, are largely excluded from the formal treaty interpretation process, though their lobbying efforts influence national positions.
% DISAPPEARANCE_RATIONALE: If this reading of Article II vanished, it would open the door to unilateral claims of ownership over extracted space resources. This would trigger a 'land rush' in space, leading to rapid enclosure, potential conflict over prime locations, and the marginalization of non-spacefaring nations, fundamentally altering the governance of outer space.
% FOUNDING_PROBLEM: The original Outer Space Treaty aimed to prevent a new colonial scramble for celestial bodies and ensure outer space remained the 'province of all mankind,' avoiding conflict and promoting peaceful exploration.
% FOUNDING_PROBLEM_CORROBORATION: Non-spacefaring states, UN committees, and many international legal scholars corroborate that the problem of preventing unilateral appropriation and ensuring equitable access remains live, especially with advancing space technology. This is attested through UN resolutions, academic publications, and diplomatic statements from outside the immediate beneficiaries of extraction.
narrative_ontology:disappearance_verdict(ost_article_ii_non_appropriation__commons_conservation, world_rearranges).
narrative_ontology:founding_problem_status(ost_article_ii_non_appropriation__commons_conservation, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ost_article_ii_non_appropriation__commons_conservation, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(ost_article_ii_non_appropriation__commons_conservation, 'none', 1).
narrative_ontology:epsilon_provenance(ost_article_ii_non_appropriation__commons_conservation, 0.85, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ost_article_ii_non_appropriation__commons_conservation_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(ost_article_ii_non_appropriation__commons_conservation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(ost_article_ii_non_appropriation__commons_conservation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness is high (0.85) because this reading effectively 'extracts' the right to unilateral resource exploitation from those with the capability to do so, transferring it to the collective. Suppression is also high (0.90) as it requires active legal and diplomatic enforcement to prevent unilateral actions and suppress alternative interpretations. The theater ratio is low (0.10) because the constraint's function is genuinely to prevent appropriation, not merely to perform. Accessibility collapse is high (0.95) as it aims to completely collapse the option of unilateral appropriation. Resistance is high (0.70) due to strong opposition from states and private entities seeking to exploit space resources.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of non-spacefaring states and conservation advocates, this reading is a vital 'rope' for global commons governance, ensuring equity and sustainability. However, from the perspective of space mining companies and spacefaring states, it operates as a 'snare,' trapping their investments and capabilities without a clear path to legitimate operation. The engine's classification will reflect this divergence based on the declared structural positions.
 *
 * DIRECTIONALITY LOGIC:
 *   Non-spacefaring states and future generations are clear beneficiaries (low d) as this reading protects their interests in a shared commons. Space mining companies, spacefaring states with extraction plans, and first-mover investors are targets (high d) as their unilateral actions are directly curtailed. International legal scholars advocating this reading act as agenda-setters, shaping the interpretation and enforcement.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading prevents the mislabeling of unilateral resource enclosure as 'development' or 'progress.' By framing it as a snare for those seeking to appropriate, it highlights the extractive nature of such actions from the perspective of the global commons, preventing mandatrophy where the original mandate of preventing appropriation is subverted by new technological capabilities.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    de_facto_appropriation_threshold,
    'At what scale or intensity does resource extraction constitute ''de facto appropriation'' under this reading?',
    'Development of international legal precedent or a multilateral agreement defining thresholds for ''use or occupation'' that cross into appropriation.',
    'A clear threshold would provide predictability for space actors but might also legitimize some level of extraction. Ambiguity maintains the ''wall'' but increases legal uncertainty.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(de_facto_appropriation_threshold, conceptual, 'Defining the boundary between permissible use and prohibited appropriation.').

omega_variable(
    private_actor_applicability,
    'Does Article II''s prohibition on ''national appropriation'' directly apply to private commercial entities, or only to states?',
    'International Court of Justice advisory opinion or a new UN resolution explicitly clarifying the scope of ''national appropriation'' to include private actors operating under state jurisdiction.',
    'If private actors are directly covered, the constraint''s suppressive force is significantly higher. If only states are covered, states could potentially authorize private appropriation, weakening the constraint.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(private_actor_applicability, empirical, 'Scope of Article II''s non-appropriation principle to private entities.').

omega_variable(
    multilateral_authorization_feasibility,
    'Is a multilateral authorization regime for space resource extraction politically and practically feasible in the near term?',
    'Progress in UN COPUOS negotiations, successful establishment of a new international treaty, or a clear failure of such efforts over a defined period.',
    'If feasible, the ''wall'' constraint could transition to a ''scaffold'' for a new regime. If infeasible, the current ''snare'' persists, increasing tension and potential for unilateral action.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(multilateral_authorization_feasibility, empirical, 'Feasibility of a future international regime for space resource governance.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ost_article_ii_non_appropriation__commons_conservation, 1967, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ost__tr_t1967, ost_article_ii_non_appropriation__commons_conservation, theater_ratio, 1967, 0.05).
narrative_ontology:measurement(ost__tr_t1980, ost_article_ii_non_appropriation__commons_conservation, theater_ratio, 1980, 0.07).
narrative_ontology:measurement(ost__tr_t1995, ost_article_ii_non_appropriation__commons_conservation, theater_ratio, 1995, 0.08).
narrative_ontology:measurement(ost__tr_t2010, ost_article_ii_non_appropriation__commons_conservation, theater_ratio, 2010, 0.09).
narrative_ontology:measurement(ost__tr_t2020, ost_article_ii_non_appropriation__commons_conservation, theater_ratio, 2020, 0.1).
narrative_ontology:measurement(ost__tr_t2024, ost_article_ii_non_appropriation__commons_conservation, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(ost__be_t1967, ost_article_ii_non_appropriation__commons_conservation, base_extractiveness, 1967, 0.8).
narrative_ontology:measurement(ost__be_t1980, ost_article_ii_non_appropriation__commons_conservation, base_extractiveness, 1980, 0.82).
narrative_ontology:measurement(ost__be_t1995, ost_article_ii_non_appropriation__commons_conservation, base_extractiveness, 1995, 0.83).
narrative_ontology:measurement(ost__be_t2010, ost_article_ii_non_appropriation__commons_conservation, base_extractiveness, 2010, 0.84).
narrative_ontology:measurement(ost__be_t2020, ost_article_ii_non_appropriation__commons_conservation, base_extractiveness, 2020, 0.85).
narrative_ontology:measurement(ost__be_t2024, ost_article_ii_non_appropriation__commons_conservation, base_extractiveness, 2024, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(ost__su_t1967, ost_article_ii_non_appropriation__commons_conservation, suppression_requirement, 1967, 0.75).
narrative_ontology:measurement(ost__su_t1980, ost_article_ii_non_appropriation__commons_conservation, suppression_requirement, 1980, 0.8).
narrative_ontology:measurement(ost__su_t1995, ost_article_ii_non_appropriation__commons_conservation, suppression_requirement, 1995, 0.85).
narrative_ontology:measurement(ost__su_t2010, ost_article_ii_non_appropriation__commons_conservation, suppression_requirement, 2010, 0.88).
narrative_ontology:measurement(ost__su_t2020, ost_article_ii_non_appropriation__commons_conservation, suppression_requirement, 2020, 0.9).
narrative_ontology:measurement(ost__su_t2024, ost_article_ii_non_appropriation__commons_conservation, suppression_requirement, 2024, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ost_article_ii_non_appropriation__commons_conservation, enforcement_mechanism).
narrative_ontology:affects_constraint(ost_article_ii_non_appropriation__commons_conservation, ost_article_ii_non_appropriation__extraction_permissive).
narrative_ontology:affects_constraint(ost_article_ii_non_appropriation__commons_conservation, ost_article_ii_non_appropriation__international_regime).

% DUAL FORMULATION NOTE:
% This is one of three readings of the Outer Space Treaty's Article II non-appropriation principle. This 'commons conservation' reading directly influences the 'extraction permissive' and 'international regime' readings by presenting a strong counter-interpretation that challenges their legitimacy and operational space.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
