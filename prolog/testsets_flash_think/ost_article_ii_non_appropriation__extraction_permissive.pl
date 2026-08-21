% ============================================================================
% CONSTRAINT STORY: ost_article_ii_non_appropriation__extraction_permissive
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ost_article_ii_non_appropriation__extraction_permissive, []).

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
 *   constraint_id: ost_article_ii_non_appropriation__extraction_permissive
 *   human_readable: OST Article II: Extraction-Permissive Reading
 *   domain: international_space_law/commons_governance
 *
 * SUMMARY:
 *   This constraint story models the 'extraction-permissive' reading of
 *   Article II of the Outer Space Treaty (OST), which states that outer space
 *   is not subject to national appropriation. This reading interprets Article
 *   II as barring only sovereign territorial claims, but not private
 *   ownership of resources extracted from celestial bodies. This
 *   interpretation enables technologically advanced states and private
 *   companies to engage in space resource extraction, leading to de facto
 *   enclosure via fait accompli, without a mechanism for compensation or
 *   benefit-sharing with excluded states. The high extractiveness and
 *   suppression reflect the structural exclusion of non-spacefaring nations
 *   and the active legal and technological enforcement of private claims.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ost_article_ii_non_appropriation__extraction_permissive, 0.85).
domain_priors:suppression_score(ost_article_ii_non_appropriation__extraction_permissive, 0.78).
domain_priors:theater_ratio(ost_article_ii_non_appropriation__extraction_permissive, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ost_article_ii_non_appropriation__extraction_permissive, extractiveness, 0.85).
narrative_ontology:constraint_metric(ost_article_ii_non_appropriation__extraction_permissive, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(ost_article_ii_non_appropriation__extraction_permissive, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ost_article_ii_non_appropriation__extraction_permissive, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(ost_article_ii_non_appropriation__extraction_permissive, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ost_article_ii_non_appropriation__extraction_permissive, snare).
narrative_ontology:human_readable(ost_article_ii_non_appropriation__extraction_permissive, "OST Article II: Extraction-Permissive Reading").
narrative_ontology:topic_domain(ost_article_ii_non_appropriation__extraction_permissive, "international_space_law/commons_governance").

domain_priors:requires_active_enforcement(ost_article_ii_non_appropriation__extraction_permissive).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ost_article_ii_non_appropriation__extraction_permissive, '37e7ddb4-0fc2-49d6-aeb6-3c0bae75e578').
narrative_ontology:cs_kernel_codification('37e7ddb4-0fc2-49d6-aeb6-3c0bae75e578', fixed_text).
narrative_ontology:cs_authority_grounding('37e7ddb4-0fc2-49d6-aeb6-3c0bae75e578', extraction).
narrative_ontology:cs_interpretation_layer_present('37e7ddb4-0fc2-49d6-aeb6-3c0bae75e578').
narrative_ontology:cs_reading_relation('37e7ddb4-0fc2-49d6-aeb6-3c0bae75e578', ost_article_ii_non_appropriation__commons_conservation, forecloses).
narrative_ontology:cs_reading_relation('37e7ddb4-0fc2-49d6-aeb6-3c0bae75e578', ost_article_ii_non_appropriation__international_regime, influences).
narrative_ontology:cs_axiom('37e7ddb4-0fc2-49d6-aeb6-3c0bae75e578', foundational, private_property_rights_in_extracted_resources_are_valid).
narrative_ontology:cs_axiom_status(private_property_rights_in_extracted_resources_are_valid, holdable).
narrative_ontology:cs_axiom_grounding('37e7ddb4-0fc2-49d6-aeb6-3c0bae75e578', private_property_rights_in_extracted_resources_are_valid, conventional).
narrative_ontology:cs_axiom('37e7ddb4-0fc2-49d6-aeb6-3c0bae75e578', foundational, article_ii_applies_only_to_sovereign_territorial_claims).
narrative_ontology:cs_axiom_status(article_ii_applies_only_to_sovereign_territorial_claims, holdable).
narrative_ontology:cs_axiom_grounding('37e7ddb4-0fc2-49d6-aeb6-3c0bae75e578', article_ii_applies_only_to_sovereign_territorial_claims, conventional).
narrative_ontology:cs_reference_frame('37e7ddb4-0fc2-49d6-aeb6-3c0bae75e578', technological_frontier_resource_access).
narrative_ontology:cs_drift_state('37e7ddb4-0fc2-49d6-aeb6-3c0bae75e578', contemporary_space_mining_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('37e7ddb4-0fc2-49d6-aeb6-3c0bae75e578', '').
narrative_ontology:cs_kernel_id(ost_article_ii_non_appropriation__extraction_permissive, ost_article_ii_non_appropriation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ost_article_ii_non_appropriation__extraction_permissive, space_resource_extractors).
narrative_ontology:constraint_beneficiary(ost_article_ii_non_appropriation__extraction_permissive, technologically_advanced_states).
narrative_ontology:constraint_victim(ost_article_ii_non_appropriation__extraction_permissive, developing_nations).
narrative_ontology:constraint_victim(ost_article_ii_non_appropriation__extraction_permissive, non_spacefaring_states).
narrative_ontology:constraint_victim(ost_article_ii_non_appropriation__extraction_permissive, future_generations).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Private companies and state-backed enterprises with the technological capability to access and extract resources from celestial bodies. They interpret Article II as permitting their activities, securing private property rights over extracted resources, and actively lobby for national legal frameworks that support this interpretation.
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__extraction_permissive, space_resource_extractors, agenda_setter,
    powerful, biographical, arbitrage, universal).

% Nations with advanced space programs and the capacity to support or conduct resource extraction. They benefit from the economic and strategic advantages of resource access and often enact domestic laws that legitimize private extraction under this interpretation of the OST.
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__extraction_permissive, technologically_advanced_states, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(ost_article_ii_non_appropriation__extraction_permissive, technologically_advanced_states, agenda_setter).

% Nations lacking independent space capabilities or the economic resources to participate in space resource extraction. They bear the cost of potential future resource scarcity or environmental degradation without any share in the benefits, and are largely excluded from the interpretive discourse.
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__extraction_permissive, developing_nations, payer,
    powerless, generational, trapped, global).
narrative_ontology:stakeholder_secondary_role(ost_article_ii_non_appropriation__extraction_permissive, developing_nations, excluded).

% Similar to developing nations, these states have no current or foreseeable capacity to engage in space activities. They are structurally excluded from the benefits of space resource utilization and have no effective means to contest the extraction-permissive interpretation.
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__extraction_permissive, non_spacefaring_states, payer,
    powerless, generational, trapped, global).
narrative_ontology:stakeholder_secondary_role(ost_article_ii_non_appropriation__extraction_permissive, non_spacefaring_states, excluded).

% Academics and legal experts who analyze the Outer Space Treaty and its interpretations. They often highlight the ambiguities in Article II and the implications of various readings for the long-term governance of space, but lack direct enforcement power.
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__extraction_permissive, international_law_scholars, observer,
    analytical, civilizational, analytical, universal).

% The unborn populations who will inherit the consequences of current space resource policies, including potential depletion of accessible resources or environmental impacts. They have no voice in current debates and are structurally unable to protect their interests.
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__extraction_permissive, future_generations, payer,
    powerless, civilizational, trapped, universal).
narrative_ontology:stakeholder_secondary_role(ost_article_ii_non_appropriation__extraction_permissive, future_generations, excluded).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: This reading coordinates resource access by establishing a de facto 'first-come, first-served' principle for technologically capable actors, thereby avoiding direct state-on-state territorial disputes over celestial bodies themselves.
% TRANSFER_FUNCTION: Transfers potential economic and strategic wealth from the global commons (celestial resources) to private entities and the technologically advanced states that support them, at the expense of excluded nations and future generations.
% ABSENT_VOICES: Developing nations, non-spacefaring states, and future generations are largely absent from the decision-making and interpretive processes. They would advocate for a more equitable distribution of benefits, a common heritage regime, or stronger environmental protections.
% DISAPPEARANCE_RATIONALE: If this extraction-permissive reading vanished overnight, the legal basis for private space resource extraction would collapse. This would likely lead to a moratorium on such activities, intense international negotiations for a new regime, or a chaotic scramble for resources under a different interpretive framework.
% FOUNDING_PROBLEM: The Outer Space Treaty was established to prevent the militarization and national appropriation of outer space, ensuring its use for the benefit of all mankind and promoting international cooperation.
% FOUNDING_PROBLEM_CORROBORATION: Spacefaring nations and industry groups corroborate that the treaty successfully prevents direct territorial claims. However, developing nations and many international law scholars, citing the 'common heritage of mankind' principle from other treaties, argue that the original problem of equitable access and benefit has been subverted, making the status contested.
narrative_ontology:disappearance_verdict(ost_article_ii_non_appropriation__extraction_permissive, world_rearranges).
narrative_ontology:founding_problem_status(ost_article_ii_non_appropriation__extraction_permissive, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ost_article_ii_non_appropriation__extraction_permissive, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(ost_article_ii_non_appropriation__extraction_permissive, 'none', 1).
narrative_ontology:epsilon_provenance(ost_article_ii_non_appropriation__extraction_permissive, 0.85, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ost_article_ii_non_appropriation__extraction_permissive_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(ost_article_ii_non_appropriation__extraction_permissive, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(ost_article_ii_non_appropriation__extraction_permissive_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.85) is high because this reading allows for the unilateral capture of resources from a global commons without a compensatory mechanism. Suppression (0.78) is also high, as the exclusion of non-spacefaring nations is maintained through technological barriers, legal interpretations by powerful states, and the absence of an alternative international regime. The theater ratio (0.40) reflects that while the rhetoric of 'non-appropriation' is maintained, a significant portion of the interpretive and enforcement effort is directed towards legitimizing and protecting private extraction rather than upholding the spirit of common benefit. Accessibility collapse (0.65) is substantial due to the high technological and capital barriers to entry for space resource extraction, effectively collapsing alternatives for most nations. Resistance (0.55) is moderate, primarily from developing nations and international legal scholars, but lacks the power to effectively challenge the established practice.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of space resource extractors and advanced states, this reading provides a clear, albeit minimalist, legal framework for economic activity in space, seen as a 'rope' or 'scaffold' for innovation. From the perspective of excluded nations, the same reading functions as a 'snare,' enabling the appropriation of common resources by a select few, perpetuating global inequalities.
 *
 * DIRECTIONALITY LOGIC:
 *   Space resource extractors and technologically advanced states are clear beneficiaries (low d), as they gain exclusive access to valuable resources. Developing and non-spacefaring nations, along with future generations, are the primary targets (high d), bearing the costs of exclusion and potential resource depletion without benefit. International law scholars act as observers, analyzing the structural implications.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    intended_scope_of_non_appropriation,
    'Does ''national appropriation'' in Article II implicitly or explicitly extend to de facto appropriation by private entities under national jurisdiction, or is it strictly limited to sovereign territorial claims?',
    'A definitive ruling by the International Court of Justice or a new, universally ratified international treaty clarifying the scope of Article II.',
    'If extended to private entities, this reading would be reclassified as a ''snare'' with higher suppression, as its current operation would be in direct violation of the treaty''s spirit. If strictly limited, the ''extraction_permissive'' reading would gain legitimacy, potentially shifting towards a ''tangled_rope'' if some coordination function is acknowledged.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(intended_scope_of_non_appropriation, conceptual, 'Ambiguity regarding whether Article II''s non-appropriation principle covers private resource extraction.').

omega_variable(
    legitimacy_of_fait_accompli,
    'To what extent does the establishment of de facto private property rights through technological capability and national legislation constitute a legitimate interpretation of international law, absent explicit international consensus?',
    'The emergence of a widely accepted customary international law supporting or rejecting private space resource rights, or a UN General Assembly resolution with broad support.',
    'If the fait accompli is widely accepted as legitimate, the constraint''s suppression might decrease over time as resistance wanes. If widely rejected, resistance would increase, and the constraint''s ''snare'' classification would be reinforced, potentially leading to calls for stronger enforcement against it.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legitimacy_of_fait_accompli, empirical, 'The legal and political legitimacy of establishing private space resource rights through unilateral action.').

omega_variable(
    future_generations_standing,
    'Do future generations have a legal or moral standing to contest the current appropriation of space resources, and what mechanisms could enable their voice?',
    'The development of international legal frameworks recognizing intergenerational equity in space resources, or the establishment of a ''guardian'' institution for future generations'' interests.',
    'If future generations gain standing, the ''victims'' set would be empowered, increasing resistance and potentially forcing a re-evaluation of the constraint''s legitimacy and extractiveness.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(future_generations_standing, preference, 'The recognition and empowerment of future generations as stakeholders in space resource governance.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ost_article_ii_non_appropriation__extraction_permissive, 1967, 2040).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ost__tr_t1967, ost_article_ii_non_appropriation__extraction_permissive, theater_ratio, 1967, 0.1).
narrative_ontology:measurement(ost__tr_t1980, ost_article_ii_non_appropriation__extraction_permissive, theater_ratio, 1980, 0.15).
narrative_ontology:measurement(ost__tr_t1995, ost_article_ii_non_appropriation__extraction_permissive, theater_ratio, 1995, 0.25).
narrative_ontology:measurement(ost__tr_t2010, ost_article_ii_non_appropriation__extraction_permissive, theater_ratio, 2010, 0.35).
narrative_ontology:measurement(ost__tr_t2025, ost_article_ii_non_appropriation__extraction_permissive, theater_ratio, 2025, 0.38).
narrative_ontology:measurement(ost__tr_t2040, ost_article_ii_non_appropriation__extraction_permissive, theater_ratio, 2040, 0.4).

% Extraction over time
narrative_ontology:measurement(ost__be_t1967, ost_article_ii_non_appropriation__extraction_permissive, base_extractiveness, 1967, 0.3).
narrative_ontology:measurement(ost__be_t1980, ost_article_ii_non_appropriation__extraction_permissive, base_extractiveness, 1980, 0.45).
narrative_ontology:measurement(ost__be_t1995, ost_article_ii_non_appropriation__extraction_permissive, base_extractiveness, 1995, 0.6).
narrative_ontology:measurement(ost__be_t2010, ost_article_ii_non_appropriation__extraction_permissive, base_extractiveness, 2010, 0.75).
narrative_ontology:measurement(ost__be_t2025, ost_article_ii_non_appropriation__extraction_permissive, base_extractiveness, 2025, 0.82).
narrative_ontology:measurement(ost__be_t2040, ost_article_ii_non_appropriation__extraction_permissive, base_extractiveness, 2040, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(ost__su_t1967, ost_article_ii_non_appropriation__extraction_permissive, suppression_requirement, 1967, 0.2).
narrative_ontology:measurement(ost__su_t1980, ost_article_ii_non_appropriation__extraction_permissive, suppression_requirement, 1980, 0.35).
narrative_ontology:measurement(ost__su_t1995, ost_article_ii_non_appropriation__extraction_permissive, suppression_requirement, 1995, 0.5).
narrative_ontology:measurement(ost__su_t2010, ost_article_ii_non_appropriation__extraction_permissive, suppression_requirement, 2010, 0.65).
narrative_ontology:measurement(ost__su_t2025, ost_article_ii_non_appropriation__extraction_permissive, suppression_requirement, 2025, 0.75).
narrative_ontology:measurement(ost__su_t2040, ost_article_ii_non_appropriation__extraction_permissive, suppression_requirement, 2040, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ost_article_ii_non_appropriation__extraction_permissive, resource_allocation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
