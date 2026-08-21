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
 *   human_readable: Outer Space Treaty Article II: Commons Conservation Reading
 *   domain: international_space_law/treaty_interpretation/commons_governance
 *
 * SUMMARY:
 *   This constraint is the 'commons_conservation' reading of the
 *   `ost_article_ii_non_appropriation` kernel. It asserts that Article II of
 *   the Outer Space Treaty (OST) prohibits de facto appropriation via
 *   resource extraction, applying to both states and private actors. This
 *   interpretation functions as a 'wall' constraint, requiring multilateral
 *   authorization for any resource utilization that could lead to exclusive
 *   claims, thereby preserving outer space as a global commons. The metrics
 *   reflect the operational impact of this interpretation, which is actively
 *   contested by those seeking unilateral extraction rights.
 *
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
narrative_ontology:constraint_metric(ost_article_ii_non_appropriation__commons_conservation, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(ost_article_ii_non_appropriation__commons_conservation, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ost_article_ii_non_appropriation__commons_conservation, tangled_rope).
narrative_ontology:human_readable(ost_article_ii_non_appropriation__commons_conservation, "Outer Space Treaty Article II: Commons Conservation Reading").
narrative_ontology:topic_domain(ost_article_ii_non_appropriation__commons_conservation, "international_space_law/treaty_interpretation/commons_governance").

domain_priors:requires_active_enforcement(ost_article_ii_non_appropriation__commons_conservation).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ost_article_ii_non_appropriation__commons_conservation, 'eb27f610-34f3-4982-ac59-29e219185326').
narrative_ontology:cs_kernel_codification('eb27f610-34f3-4982-ac59-29e219185326', fixed_text).
narrative_ontology:cs_authority_grounding('eb27f610-34f3-4982-ac59-29e219185326', lineage).
narrative_ontology:cs_interpretation_layer_present('eb27f610-34f3-4982-ac59-29e219185326').
narrative_ontology:cs_reading_relation('eb27f610-34f3-4982-ac59-29e219185326', ost_article_ii_non_appropriation__extraction_permissive, forecloses).
narrative_ontology:cs_reading_relation('eb27f610-34f3-4982-ac59-29e219185326', ost_article_ii_non_appropriation__international_regime, influences).
narrative_ontology:cs_axiom('eb27f610-34f3-4982-ac59-29e219185326', foundational, outer_space_as_global_commons).
narrative_ontology:cs_axiom_status(outer_space_as_global_commons, holdable).
narrative_ontology:cs_axiom_grounding('eb27f610-34f3-4982-ac59-29e219185326', outer_space_as_global_commons, deontological).
narrative_ontology:cs_axiom('eb27f610-34f3-4982-ac59-29e219185326', foundational, de_facto_appropriation_prohibited).
narrative_ontology:cs_axiom_status(de_facto_appropriation_prohibited, holdable).
narrative_ontology:cs_axiom_grounding('eb27f610-34f3-4982-ac59-29e219185326', de_facto_appropriation_prohibited, conventional).
narrative_ontology:cs_reference_frame('eb27f610-34f3-4982-ac59-29e219185326', province_of_all_mankind_principle).
narrative_ontology:cs_drift_state('eb27f610-34f3-4982-ac59-29e219185326', contemporary_space_resource_rush, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('eb27f610-34f3-4982-ac59-29e219185326', '').
narrative_ontology:cs_kernel_id(ost_article_ii_non_appropriation__commons_conservation, ost_article_ii_non_appropriation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ost_article_ii_non_appropriation__commons_conservation, international_community).
narrative_ontology:constraint_beneficiary(ost_article_ii_non_appropriation__commons_conservation, non_spacefaring_states).
narrative_ontology:constraint_beneficiary(ost_article_ii_non_appropriation__commons_conservation, future_generations).
narrative_ontology:constraint_victim(ost_article_ii_non_appropriation__commons_conservation, first_mover_space_mining_companies).
narrative_ontology:constraint_victim(ost_article_ii_non_appropriation__commons_conservation, spacefaring_states_with_extraction_plans).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Upholds the principle of outer space as the province of all mankind, advocating for multilateral governance and equitable benefit sharing. Seeks to prevent unilateral appropriation of space resources.
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__commons_conservation, international_community, agenda_setter,
    institutional, civilizational, analytical, universal).

% Benefit from the prohibition of unilateral appropriation, as it preserves their potential future access to space resources and prevents powerful states/companies from establishing exclusive claims. They lack the capability for unilateral extraction.
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__commons_conservation, non_spacefaring_states, beneficiary,
    organized, generational, constrained, global).

% Are the ultimate beneficiaries of a conserved global commons in outer space, ensuring resources and access are not depleted or monopolized before their time. Their interests are represented by the international community.
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__commons_conservation, future_generations, beneficiary,
    powerless, civilizational, trapped, universal).
narrative_ontology:stakeholder_non_agent(ost_article_ii_non_appropriation__commons_conservation, future_generations).

% Are prevented from unilaterally claiming and extracting space resources, stranding their investment in technologies and missions designed for such activities. They face significant legal and political barriers to their business model.
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__commons_conservation, first_mover_space_mining_companies, payer,
    powerful, biographical, constrained, global).

% Are constrained from supporting or engaging in unilateral resource extraction by their national entities, as this reading of the treaty directly opposes such actions. They seek to interpret Article II more permissively to enable their space industry.
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__commons_conservation, spacefaring_states_with_extraction_plans, payer,
    institutional, generational, constrained, global).

% Analyze and interpret the Outer Space Treaty, contributing to the legal discourse around appropriation and resource extraction. Their work informs policy debates and judicial opinions.
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__commons_conservation, international_law_scholars, observer,
    analytical, biographical, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates states and private actors to refrain from unilateral appropriation of space resources, preventing a 'tragedy of the commons' and ensuring outer space remains a shared heritage for all, to be used for peaceful purposes.
% TRANSFER_FUNCTION: Prevents the transfer of exclusive resource rights from the international community (and future generations) to first-mover states or private entities. It effectively transfers the 'right to veto' unilateral extraction to the broader international community.
% ABSENT_VOICES: Private space resource extractors and states with advanced space mining capabilities would argue for a more permissive interpretation, but this reading actively excludes their unilateral claims by asserting a strong prohibition.
% DISAPPEARANCE_RATIONALE: If this interpretation vanished, a rapid 'land grab' for space resources would likely ensue, leading to conflict, environmental degradation in space, and the exclusion of non-spacefaring nations from any benefits. The governance structure of outer space would fundamentally shift towards a 'first-come, first-served' model.
% FOUNDING_PROBLEM: Preventing a chaotic and inequitable scramble for celestial resources, ensuring outer space remains the 'province of all mankind' and is used for peaceful purposes, avoiding the conflicts and inequalities seen in terrestrial resource exploitation.
% FOUNDING_PROBLEM_CORROBORATION: Non-spacefaring states, many international legal scholars, and UN committees consistently corroborate the ongoing relevance of preventing unilateral appropriation and preserving the commons, citing the increasing technological capabilities for space resource utilization and the potential for conflict.
narrative_ontology:disappearance_verdict(ost_article_ii_non_appropriation__commons_conservation, world_rearranges).
narrative_ontology:founding_problem_status(ost_article_ii_non_appropriation__commons_conservation, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ost_article_ii_non_appropriation__commons_conservation, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
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
 *   The high extractiveness (0.85) reflects the constraint's impact on those seeking unilateral resource claims, effectively 'extracting' their perceived right to do so. High suppression (0.90) is necessary to actively prevent such claims from materializing against increasing technological capabilities and commercial interest. The low theater ratio (0.10) indicates that the constraint's function is direct and prohibitory, not performative. The claimed type is 'tangled_rope' because it coordinates the international community around a shared commons while simultaneously extracting from (and suppressing) those who wish to exploit it unilaterally, requiring active enforcement to maintain this balance.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of non-spacefaring states, this interpretation is a vital protective mechanism, ensuring equity and preventing a 'land grab.' From the perspective of a space mining company, it's a significant barrier that prevents profitable ventures. The engine will compute these divergent classifications based on the structural data provided.
 *
 * DIRECTIONALITY LOGIC:
 *   The international community, non-spacefaring states, and future generations are the structural beneficiaries, as the constraint protects their shared interest in the commons and equitable access. First-mover space mining companies and spacefaring states with extraction plans are the targets/victims, as their unilateral ambitions are directly curtailed and their investments in such capabilities are stranded. International law scholars act as observers, analyzing the legal landscape.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    appropriation_scope_ambiguity,
    'Does ''non-appropriation'' in Article II apply only to territorial claims, or does it extend to de facto appropriation through resource extraction?',
    'A definitive ruling by an international court or a new multilateral treaty explicitly clarifying the scope of Article II regarding resource extraction.',
    'If limited to territorial claims, this reading''s extractiveness and suppression would significantly decrease for resource extractors, potentially reclassifying it as a ''rope'' or ''piton'' for them. If confirmed to include resource extraction, its ''tangled_rope'' classification would be strengthened.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(appropriation_scope_ambiguity, conceptual, 'Ambiguity regarding whether Article II''s non-appropriation principle covers resource extraction.').

omega_variable(
    private_actor_applicability,
    'Does Article II''s prohibition on appropriation apply directly to private commercial entities, or only to states?',
    'International legal precedent or state practice explicitly extending or limiting the application of Article II to private actors.',
    'If private actors are exempt, the constraint''s effective suppression and extractiveness would decrease significantly for companies, potentially shifting the classification towards a ''piton'' or ''rope'' for them. If confirmed to apply, the current classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(private_actor_applicability, conceptual, 'Whether Article II''s non-appropriation principle applies to private entities.').

omega_variable(
    multilateral_authorization_feasibility,
    'Is multilateral authorization for space resource extraction a genuinely feasible and equitable mechanism, or a de facto prohibition due to political gridlock?',
    'Successful negotiation and implementation of a multilateral framework for space resource governance that is widely ratified and operational.',
    'If feasible, the constraint could evolve towards a ''rope'' or ''scaffold'' as a functional coordination mechanism. If it remains a de facto prohibition, its ''tangled_rope'' nature (extracting opportunity) would be reinforced, potentially drifting towards a ''snare'' for those with extraction capabilities.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(multilateral_authorization_feasibility, empirical, 'Feasibility of multilateral authorization for space resource extraction.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ost_article_ii_non_appropriation__commons_conservation, 1967, 2017).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ost__tr_t1967, ost_article_ii_non_appropriation__commons_conservation, theater_ratio, 1967, 0.1).
narrative_ontology:measurement(ost__tr_t1977, ost_article_ii_non_appropriation__commons_conservation, theater_ratio, 1977, 0.1).
narrative_ontology:measurement(ost__tr_t1987, ost_article_ii_non_appropriation__commons_conservation, theater_ratio, 1987, 0.1).
narrative_ontology:measurement(ost__tr_t1997, ost_article_ii_non_appropriation__commons_conservation, theater_ratio, 1997, 0.1).
narrative_ontology:measurement(ost__tr_t2007, ost_article_ii_non_appropriation__commons_conservation, theater_ratio, 2007, 0.1).
narrative_ontology:measurement(ost__tr_t2017, ost_article_ii_non_appropriation__commons_conservation, theater_ratio, 2017, 0.1).

% Extraction over time
narrative_ontology:measurement(ost__be_t1967, ost_article_ii_non_appropriation__commons_conservation, base_extractiveness, 1967, 0.7).
narrative_ontology:measurement(ost__be_t1977, ost_article_ii_non_appropriation__commons_conservation, base_extractiveness, 1977, 0.75).
narrative_ontology:measurement(ost__be_t1987, ost_article_ii_non_appropriation__commons_conservation, base_extractiveness, 1987, 0.78).
narrative_ontology:measurement(ost__be_t1997, ost_article_ii_non_appropriation__commons_conservation, base_extractiveness, 1997, 0.8).
narrative_ontology:measurement(ost__be_t2007, ost_article_ii_non_appropriation__commons_conservation, base_extractiveness, 2007, 0.83).
narrative_ontology:measurement(ost__be_t2017, ost_article_ii_non_appropriation__commons_conservation, base_extractiveness, 2017, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(ost__su_t1967, ost_article_ii_non_appropriation__commons_conservation, suppression_requirement, 1967, 0.75).
narrative_ontology:measurement(ost__su_t1977, ost_article_ii_non_appropriation__commons_conservation, suppression_requirement, 1977, 0.8).
narrative_ontology:measurement(ost__su_t1987, ost_article_ii_non_appropriation__commons_conservation, suppression_requirement, 1987, 0.83).
narrative_ontology:measurement(ost__su_t1997, ost_article_ii_non_appropriation__commons_conservation, suppression_requirement, 1997, 0.86).
narrative_ontology:measurement(ost__su_t2007, ost_article_ii_non_appropriation__commons_conservation, suppression_requirement, 2007, 0.88).
narrative_ontology:measurement(ost__su_t2017, ost_article_ii_non_appropriation__commons_conservation, suppression_requirement, 2017, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ost_article_ii_non_appropriation__commons_conservation, enforcement_mechanism).
narrative_ontology:affects_constraint(ost_article_ii_non_appropriation__commons_conservation, ost_article_ii_non_appropriation__extraction_permissive).
narrative_ontology:affects_constraint(ost_article_ii_non_appropriation__commons_conservation, ost_article_ii_non_appropriation__international_regime).
narrative_ontology:affects_constraint(ost_article_ii_non_appropriation__commons_conservation, space_resource_utilization_act_2015).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the `ost_article_ii_non_appropriation` kernel. This 'commons_conservation' reading directly opposes the 'extraction_permissive' reading and influences the 'international_regime' reading by advocating for a conservation-oriented framework.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
