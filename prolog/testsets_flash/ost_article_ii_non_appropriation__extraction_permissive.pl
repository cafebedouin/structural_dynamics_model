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
 *   constraint_id: ost_article_ii_non_appropriation__extraction_permissive
 *   human_readable: Outer Space Treaty Article II Non-Appropriation (Extraction-Permissive Reading)
 *   domain: international_space_law/treaty_interpretation/commons_governance
 *
 * SUMMARY:
 *   This constraint represents the 'extraction-permissive' reading of Outer
 *   Space Treaty (OST) Article II, which states that outer space is 'not
 *   subject to national appropriation by claim of sovereignty, by means of
 *   use or occupation, or by any other means.' This reading interprets the
 *   prohibition as applying only to sovereign territorial claims, not to the
 *   private ownership of resources extracted from celestial bodies. This
 *   interpretation is primarily advanced by technologically advanced states
 *   and private space resource companies, enabling a 'first-come,
 *   first-served' approach to space resources.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ost_article_ii_non_appropriation__extraction_permissive, 0.65).
domain_priors:suppression_score(ost_article_ii_non_appropriation__extraction_permissive, 0.7).
domain_priors:theater_ratio(ost_article_ii_non_appropriation__extraction_permissive, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ost_article_ii_non_appropriation__extraction_permissive, extractiveness, 0.65).
narrative_ontology:constraint_metric(ost_article_ii_non_appropriation__extraction_permissive, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(ost_article_ii_non_appropriation__extraction_permissive, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ost_article_ii_non_appropriation__extraction_permissive, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(ost_article_ii_non_appropriation__extraction_permissive, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ost_article_ii_non_appropriation__extraction_permissive, tangled_rope).
narrative_ontology:human_readable(ost_article_ii_non_appropriation__extraction_permissive, "Outer Space Treaty Article II Non-Appropriation (Extraction-Permissive Reading)").
narrative_ontology:topic_domain(ost_article_ii_non_appropriation__extraction_permissive, "international_space_law/treaty_interpretation/commons_governance").

domain_priors:requires_active_enforcement(ost_article_ii_non_appropriation__extraction_permissive).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ost_article_ii_non_appropriation__extraction_permissive, 'b4cebd53-266f-4ec9-80a8-24a0b21a2617').
narrative_ontology:cs_kernel_codification('b4cebd53-266f-4ec9-80a8-24a0b21a2617', fixed_text).
narrative_ontology:cs_authority_grounding('b4cebd53-266f-4ec9-80a8-24a0b21a2617', lineage).
narrative_ontology:cs_interpretation_layer_present('b4cebd53-266f-4ec9-80a8-24a0b21a2617').
narrative_ontology:cs_reading_relation('b4cebd53-266f-4ec9-80a8-24a0b21a2617', ost_article_ii_non_appropriation__commons_conservation, coexists_with).
narrative_ontology:cs_reading_relation('b4cebd53-266f-4ec9-80a8-24a0b21a2617', ost_article_ii_non_appropriation__international_regime, coexists_with).
narrative_ontology:cs_axiom('b4cebd53-266f-4ec9-80a8-24a0b21a2617', foundational, private_property_rights_in_extracted_resources).
narrative_ontology:cs_axiom_status(private_property_rights_in_extracted_resources, holdable).
narrative_ontology:cs_axiom_grounding('b4cebd53-266f-4ec9-80a8-24a0b21a2617', private_property_rights_in_extracted_resources, conventional).
narrative_ontology:cs_axiom('b4cebd53-266f-4ec9-80a8-24a0b21a2617', foundational, sovereign_appropriation_only_prohibited).
narrative_ontology:cs_axiom_status(sovereign_appropriation_only_prohibited, holdable).
narrative_ontology:cs_axiom_grounding('b4cebd53-266f-4ec9-80a8-24a0b21a2617', sovereign_appropriation_only_prohibited, conventional).
narrative_ontology:cs_reference_frame('b4cebd53-266f-4ec9-80a8-24a0b21a2617', state_centric_property_rights).
narrative_ontology:cs_drift_state('b4cebd53-266f-4ec9-80a8-24a0b21a2617', contemporary_space_commercialization_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('b4cebd53-266f-4ec9-80a8-24a0b21a2617', '').
narrative_ontology:cs_kernel_id(ost_article_ii_non_appropriation__extraction_permissive, ost_article_ii_non_appropriation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ost_article_ii_non_appropriation__extraction_permissive, space_resource_companies).
narrative_ontology:constraint_beneficiary(ost_article_ii_non_appropriation__extraction_permissive, technologically_advanced_states).
narrative_ontology:constraint_victim(ost_article_ii_non_appropriation__extraction_permissive, developing_nations).
narrative_ontology:constraint_victim(ost_article_ii_non_appropriation__extraction_permissive, future_generations).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These states possess the technology and capital to access and extract space resources. They interpret Article II as permitting private resource ownership, facilitating their national space industries and securing strategic resources. They actively license and support private companies in this endeavor.
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__extraction_permissive, technologically_advanced_states, agenda_setter,
    institutional, generational, mobile, global).

% These private entities invest in and develop technologies for space resource extraction. They benefit directly from the permissive interpretation of Article II, which allows them to claim ownership of extracted resources and profit from their sale, without a clear international compensation mechanism.
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__extraction_permissive, space_resource_companies, beneficiary,
    organized, biographical, arbitrage, global).

% These nations lack the technological capability to access space resources and are effectively excluded from the benefits of space resource utilization under this interpretation. They bear the cost of potential resource depletion and the establishment of de facto monopolies without any compensatory mechanism or share in the commons.
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__extraction_permissive, developing_nations, payer,
    powerless, generational, trapped, global).

% As a collective, future generations bear the long-term costs of resource depletion and the establishment of precedents for private appropriation of what was once considered a common heritage, potentially limiting their access and benefit from space resources.
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__extraction_permissive, future_generations, payer,
    powerless, civilizational, trapped, universal).

% These scholars analyze the legal implications of various interpretations of Article II, highlighting potential conflicts with the 'common heritage of mankind' principle and the need for a more robust international regulatory framework. They do not directly benefit or pay but influence discourse.
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__extraction_permissive, international_legal_scholars, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a legal framework that, for technologically advanced states and private companies, coordinates investment and operational certainty for space resource extraction by clarifying that extracted resources can be privately owned, thereby incentivizing commercial activity.
% TRANSFER_FUNCTION: Transfers potential wealth from the collective 'common heritage' of outer space to private entities and their sponsoring states, by allowing them to claim ownership of extracted resources without a mechanism for sharing benefits with non-spacefaring nations.
% ABSENT_VOICES: Nations without space capabilities, particularly those advocating for a 'common heritage of mankind' approach, are effectively excluded from the decision-making process that shapes this interpretation. They would argue for a more equitable distribution of benefits and a moratorium on appropriation until a comprehensive regime is established.
% DISAPPEARANCE_RATIONALE: If this permissive interpretation vanished, space resource companies would face immense legal uncertainty, likely halting investment and operations. Technologically advanced states would need to renegotiate international agreements, and the entire nascent space resource industry would be forced to reorganize under a new, potentially more restrictive, legal framework.
% FOUNDING_PROBLEM: The Outer Space Treaty was established to prevent national appropriation of celestial bodies and to ensure the peaceful exploration and use of outer space, avoiding a 'wild west' scenario in space.
% FOUNDING_PROBLEM_CORROBORATION: Technologically advanced states and space resource companies argue that the permissive reading is consistent with incentivizing peaceful use and preventing a resource grab by a single state. Developing nations and many international legal scholars, from outside the benefiting parties, contend that this reading undermines the original intent of non-appropriation and creates a new form of enclosure, making the founding problem of equitable access and peaceful use still very much 'live' but unaddressed by this interpretation.
narrative_ontology:disappearance_verdict(ost_article_ii_non_appropriation__extraction_permissive, world_rearranges).
narrative_ontology:founding_problem_status(ost_article_ii_non_appropriation__extraction_permissive, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ost_article_ii_non_appropriation__extraction_permissive, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(ost_article_ii_non_appropriation__extraction_permissive, 'none', 1).

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
 *   The extractiveness (0.65) is high because this reading allows for significant private gain from a common resource without a clear benefit-sharing mechanism for all humanity. Suppression (0.7) is also high, as it effectively suppresses the claims and participation of non-spacefaring nations through a legal interpretation backed by technological and economic power. The 'tangled_rope' classification reflects a genuine coordination function (enabling commercial space activity) intertwined with asymmetric extraction (benefiting a few at the expense of many).
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of technologically advanced states and space resource companies, this reading provides necessary legal clarity to incentivize innovation and investment in space. From the perspective of developing nations, it represents a new form of colonial enclosure, perpetuating inequality in access to global commons. The engine's per-seat classification will reflect this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Technologically advanced states and space resource companies are clear beneficiaries (low d) as they gain access to resources and profit. Developing nations and future generations are victims (high d) as they are excluded from benefits and face resource depletion. International legal scholars act as observers (analytical d).
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    de_facto_appropriation_ambiguity,
    'Does the private ownership of extracted resources, especially on a large scale, constitute a de facto ''appropriation by means of use or occupation'' under Article II, even if not a sovereign claim?',
    'An International Court of Justice advisory opinion or a new multilateral treaty explicitly clarifying the scope of ''appropriation'' for private actors and extracted resources.',
    'If deemed de facto appropriation, this reading would be reclassified as a Snare, as its coordination function (incentivizing private activity) would be revealed as cover for pure extraction, and the beneficiaries would become clear agenda-setters of an extractive regime. If not, the current Tangled Rope classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(de_facto_appropriation_ambiguity, conceptual, 'Ambiguity over whether private resource ownership constitutes de facto appropriation.').

omega_variable(
    common_heritage_principle_conflict,
    'How does this extraction-permissive reading reconcile with the ''common heritage of mankind'' principle, which, while not explicitly in the OST, is a widely recognized principle of international space law?',
    'Development of a benefit-sharing mechanism for space resources, or a formal declaration by a majority of UN member states on the applicability and interpretation of the common heritage principle to space resources.',
    'If a strong common heritage principle is affirmed, this reading''s extractiveness would be seen as illegitimate, pushing it towards a Snare or requiring significant modification to incorporate equitable sharing. If rejected, the current classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(common_heritage_principle_conflict, preference, 'Conflict between permissive extraction and common heritage principle.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ost_article_ii_non_appropriation__extraction_permissive, 1967, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Extraction over time
narrative_ontology:measurement(ost__be_t1967, ost_article_ii_non_appropriation__extraction_permissive, base_extractiveness, 1967, 0.3).
narrative_ontology:measurement(ost__be_t1980, ost_article_ii_non_appropriation__extraction_permissive, base_extractiveness, 1980, 0.4).
narrative_ontology:measurement(ost__be_t1995, ost_article_ii_non_appropriation__extraction_permissive, base_extractiveness, 1995, 0.5).
narrative_ontology:measurement(ost__be_t2010, ost_article_ii_non_appropriation__extraction_permissive, base_extractiveness, 2010, 0.6).
narrative_ontology:measurement(ost__be_t2024, ost_article_ii_non_appropriation__extraction_permissive, base_extractiveness, 2024, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(ost__su_t1967, ost_article_ii_non_appropriation__extraction_permissive, suppression_requirement, 1967, 0.2).
narrative_ontology:measurement(ost__su_t1980, ost_article_ii_non_appropriation__extraction_permissive, suppression_requirement, 1980, 0.35).
narrative_ontology:measurement(ost__su_t1995, ost_article_ii_non_appropriation__extraction_permissive, suppression_requirement, 1995, 0.5).
narrative_ontology:measurement(ost__su_t2010, ost_article_ii_non_appropriation__extraction_permissive, suppression_requirement, 2010, 0.6).
narrative_ontology:measurement(ost__su_t2024, ost_article_ii_non_appropriation__extraction_permissive, suppression_requirement, 2024, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ost_article_ii_non_appropriation__extraction_permissive, resource_allocation).
narrative_ontology:affects_constraint(ost_article_ii_non_appropriation__extraction_permissive, ost_article_ii_non_appropriation__commons_conservation).
narrative_ontology:affects_constraint(ost_article_ii_non_appropriation__extraction_permissive, ost_article_ii_non_appropriation__international_regime).
narrative_ontology:affects_constraint(ost_article_ii_non_appropriation__extraction_permissive, moon_agreement_ratification_barrier).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
