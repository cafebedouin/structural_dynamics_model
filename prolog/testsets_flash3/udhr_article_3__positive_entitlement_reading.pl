% ============================================================================
% CONSTRAINT STORY: udhr_article_3__positive_entitlement_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_udhr_article_3__positive_entitlement_reading, []).

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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: udhr_article_3__positive_entitlement_reading
 *   human_readable: UDHR Article 3: Positive Entitlement to Material Conditions
 *   domain: constitutional_law/human_rights/political_philosophy
 *
 * SUMMARY:
 *   This constraint represents the 'positive entitlement' reading of Article
 *   3 of the Universal Declaration of Human Rights (UDHR), which interprets
 *   'life, liberty and security of person' as obligating states to provide
 *   the material conditions necessary for these rights. This includes
 *   welfare, healthcare, and housing. This reading is highly contested by
 *   those who advocate for a 'negative liberty' interpretation, which focuses
 *   on freedom from state interference. The high extractiveness reflects the
 *   significant resource transfer required, and the high suppression reflects
 *   the active enforcement needed to overcome resistance from those whose
 *   property rights or individual liberties are curtailed.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(udhr_article_3__positive_entitlement_reading, 0.85).
domain_priors:suppression_score(udhr_article_3__positive_entitlement_reading, 0.7).
domain_priors:theater_ratio(udhr_article_3__positive_entitlement_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(udhr_article_3__positive_entitlement_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(udhr_article_3__positive_entitlement_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(udhr_article_3__positive_entitlement_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(udhr_article_3__positive_entitlement_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(udhr_article_3__positive_entitlement_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(udhr_article_3__positive_entitlement_reading, tangled_rope).
narrative_ontology:human_readable(udhr_article_3__positive_entitlement_reading, "UDHR Article 3: Positive Entitlement to Material Conditions").
narrative_ontology:topic_domain(udhr_article_3__positive_entitlement_reading, "constitutional_law/human_rights/political_philosophy").

domain_priors:requires_active_enforcement(udhr_article_3__positive_entitlement_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(udhr_article_3__positive_entitlement_reading, '245b9bc7-a4a8-48b1-9495-fc1a2ddd8f02').
narrative_ontology:cs_kernel_codification('245b9bc7-a4a8-48b1-9495-fc1a2ddd8f02', fixed_text).
narrative_ontology:cs_authority_grounding('245b9bc7-a4a8-48b1-9495-fc1a2ddd8f02', lineage).
narrative_ontology:cs_interpretation_layer_present('245b9bc7-a4a8-48b1-9495-fc1a2ddd8f02').
narrative_ontology:cs_reading_relation('245b9bc7-a4a8-48b1-9495-fc1a2ddd8f02', udhr_article_3__negative_liberty_reading, forecloses).
narrative_ontology:cs_reading_relation('245b9bc7-a4a8-48b1-9495-fc1a2ddd8f02', udhr_article_3__procedural_hybrid_reading, influences).
narrative_ontology:cs_axiom('245b9bc7-a4a8-48b1-9495-fc1a2ddd8f02', foundational, material_conditions_are_prerequisites_for_rights).
narrative_ontology:cs_axiom_status(material_conditions_are_prerequisites_for_rights, holdable).
narrative_ontology:cs_axiom_grounding('245b9bc7-a4a8-48b1-9495-fc1a2ddd8f02', material_conditions_are_prerequisites_for_rights, deontological).
narrative_ontology:cs_axiom('245b9bc7-a4a8-48b1-9495-fc1a2ddd8f02', foundational, state_has_positive_obligation_to_provide).
narrative_ontology:cs_axiom_status(state_has_positive_obligation_to_provide, holdable).
narrative_ontology:cs_axiom_grounding('245b9bc7-a4a8-48b1-9495-fc1a2ddd8f02', state_has_positive_obligation_to_provide, deontological).
narrative_ontology:cs_reference_frame('245b9bc7-a4a8-48b1-9495-fc1a2ddd8f02', post_wwii_social_democratic_consensus).
narrative_ontology:cs_drift_state('245b9bc7-a4a8-48b1-9495-fc1a2ddd8f02', contemporary_neoliberal_era, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('245b9bc7-a4a8-48b1-9495-fc1a2ddd8f02', '').
narrative_ontology:cs_kernel_id(udhr_article_3__positive_entitlement_reading, udhr_article_3).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(udhr_article_3__positive_entitlement_reading, vulnerable_populations).
narrative_ontology:constraint_beneficiary(udhr_article_3__positive_entitlement_reading, social_justice_advocates).
narrative_ontology:constraint_victim(udhr_article_3__positive_entitlement_reading, property_owners).
narrative_ontology:constraint_victim(udhr_article_3__positive_entitlement_reading, individual_libertarians).
narrative_ontology:constraint_victim(udhr_article_3__positive_entitlement_reading, taxpayers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Directly benefit from state provision of welfare, healthcare, and housing, which are seen as essential for their life and security. Their well-being is directly tied to the enforcement of this reading.
narrative_ontology:constraint_stakeholder(udhr_article_3__positive_entitlement_reading, vulnerable_populations, beneficiary,
    powerless, immediate, trapped, national).

% Actively champion this reading, seeing it as a moral imperative and a pathway to a more equitable society. They benefit from the legitimacy this reading grants to their policy proposals and activism.
narrative_ontology:constraint_stakeholder(udhr_article_3__positive_entitlement_reading, social_justice_advocates, beneficiary,
    organized, generational, constrained, global).

% Bear the costs of wealth redistribution and state-funded social programs through taxation and potential restrictions on property use. They view this as an infringement on their property rights.
narrative_ontology:constraint_stakeholder(udhr_article_3__positive_entitlement_reading, property_owners, payer,
    powerful, biographical, constrained, national).

% Oppose state intervention in economic and social life, viewing positive entitlements as coercive and undermining individual freedom. They bear the cost of living under a system that enforces such obligations.
narrative_ontology:constraint_stakeholder(udhr_article_3__positive_entitlement_reading, individual_libertarians, payer,
    moderate, generational, mobile, national).

% Fund the state provision of material conditions through various taxes. While some may benefit from specific services, the overall burden is perceived as a cost of this interpretation of Article 3.
narrative_ontology:constraint_stakeholder(udhr_article_3__positive_entitlement_reading, taxpayers, payer,
    organized, biographical, constrained, national).

% Are obligated to implement and enforce policies that provide welfare, healthcare, and housing. They manage the collection of resources and their distribution, balancing competing demands and legal interpretations.
narrative_ontology:constraint_stakeholder(udhr_article_3__positive_entitlement_reading, state_institutions, agenda_setter,
    institutional, generational, constrained, national).

% Would argue that Article 3 primarily protects freedom from state interference, not a right to state provision. Their interpretation is marginalized in policy debates driven by this positive entitlement reading.
narrative_ontology:constraint_stakeholder(udhr_article_3__positive_entitlement_reading, negative_liberty_advocates, excluded,
    organized, generational, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates state action and resource allocation to ensure a baseline of material conditions for all citizens, preventing extreme poverty and ensuring social stability.
% TRANSFER_FUNCTION: Transfers wealth and resources from higher-income individuals and property owners to vulnerable populations through taxation and state-funded social programs.
% ABSENT_VOICES: Advocates for a purely negative liberty interpretation of Article 3 are excluded from the policy-making process that implements positive entitlements, as their core premise is rejected by this reading's framework.
% DISAPPEARANCE_RATIONALE: If this reading vanished, state welfare programs, universal healthcare, and public housing initiatives would lose their constitutional grounding, leading to a rapid dismantling of social safety nets and a significant increase in poverty and inequality, fundamentally altering the social contract.
% FOUNDING_PROBLEM: The problem of widespread poverty, lack of access to basic necessities, and social insecurity, particularly in the aftermath of global conflicts and economic depressions.
% FOUNDING_PROBLEM_CORROBORATION: International human rights organizations, UN agencies, and numerous academic studies corroborate the ongoing existence of these problems globally, supporting the continued relevance of this reading's mandate. While some argue for market-based solutions, the underlying problems of material deprivation persist.
narrative_ontology:disappearance_verdict(udhr_article_3__positive_entitlement_reading, world_rearranges).
narrative_ontology:founding_problem_status(udhr_article_3__positive_entitlement_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(udhr_article_3__positive_entitlement_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(udhr_article_3__positive_entitlement_reading, 'none', 1).
narrative_ontology:epsilon_provenance(udhr_article_3__positive_entitlement_reading, 0.85, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(udhr_article_3__positive_entitlement_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(udhr_article_3__positive_entitlement_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(udhr_article_3__positive_entitlement_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.85) is high due to the extensive state intervention and wealth redistribution implied by this reading. Suppression (0.7) is also high, as the implementation of these entitlements often faces strong political and economic resistance, requiring active enforcement mechanisms (e.g., tax collection, regulatory bodies). The theater ratio (0.2) is relatively low, indicating that while there might be some performative aspects, the core function of resource provision is genuinely pursued. Accessibility collapse (0.4) is moderate, as alternative private solutions exist but are often insufficient for vulnerable populations. Resistance (0.75) is high, reflecting ongoing political and legal challenges to state-mandated welfare provisions.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of vulnerable populations, this reading is a vital Rope, providing essential coordination for survival. From the perspective of property owners and libertarians, it is a Snare, coercively extracting resources and limiting freedom. The engine's classification as Tangled Rope reflects the hybrid nature: genuine coordination for beneficiaries, but with significant asymmetric extraction from payers, requiring active enforcement.
 *
 * DIRECTIONALITY LOGIC:
 *   Vulnerable populations and social justice advocates are clear beneficiaries, receiving direct support and validation for their cause. Property owners, individual libertarians, and taxpayers are the primary payers, bearing the financial and ideological costs. State institutions act as agenda-setters, mediating the implementation. Negative liberty advocates are excluded, as their core premise is incompatible with this reading's framework.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    scope_of_material_conditions,
    'What specific material conditions are ''necessary for life and security'' and how are these defined and measured across different socio-economic contexts?',
    'International consensus-building through human rights committees, national legislative processes, and judicial interpretation, informed by expert reports on minimum living standards.',
    'A broader definition would increase extractiveness and suppression, expanding the scope of state obligation. A narrower definition would reduce these, potentially leaving more individuals without essential support.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scope_of_material_conditions, conceptual, 'Ambiguity in defining the precise scope of ''material conditions'' for positive entitlements.').

omega_variable(
    balancing_positive_negative_rights,
    'How should the positive entitlements derived from this reading be balanced against traditional negative liberties (e.g., property rights, freedom of expression) when they conflict?',
    'Judicial review establishing a hierarchy or proportionality test for conflicting rights, or constitutional amendments clarifying the relationship between different categories of rights.',
    'Prioritizing positive entitlements would amplify extraction from property owners. Prioritizing negative liberties would diminish the state''s obligation to provide material conditions, shifting the burden to individuals.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(balancing_positive_negative_rights, preference, 'The fundamental tension between positive entitlements and negative liberties.').

omega_variable(
    state_capacity_for_provision,
    'To what extent do states possess the actual capacity (financial, administrative, political) to fulfill the extensive obligations implied by this positive entitlement reading?',
    'Empirical studies of state budgets, administrative efficiency, and political will in different countries, alongside comparative analysis of policy outcomes.',
    'If state capacity is low, the constraint''s theater ratio would increase (promises without delivery), and its effective extractiveness might be lower than intended due to implementation failures. If capacity is high, the constraint operates as intended, with high extractiveness and suppression.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(state_capacity_for_provision, empirical, 'The gap between the normative obligation and the practical capacity of states to deliver.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(udhr_article_3__positive_entitlement_reading, 1948, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(udhr_tr_t1948, udhr_article_3__positive_entitlement_reading, theater_ratio, 1948, 0.1).
narrative_ontology:measurement(udhr_tr_t1970, udhr_article_3__positive_entitlement_reading, theater_ratio, 1970, 0.15).
narrative_ontology:measurement(udhr_tr_t1990, udhr_article_3__positive_entitlement_reading, theater_ratio, 1990, 0.18).
narrative_ontology:measurement(udhr_tr_t2010, udhr_article_3__positive_entitlement_reading, theater_ratio, 2010, 0.2).
narrative_ontology:measurement(udhr_tr_t2024, udhr_article_3__positive_entitlement_reading, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(udhr_be_t1948, udhr_article_3__positive_entitlement_reading, base_extractiveness, 1948, 0.6).
narrative_ontology:measurement(udhr_be_t1970, udhr_article_3__positive_entitlement_reading, base_extractiveness, 1970, 0.7).
narrative_ontology:measurement(udhr_be_t1990, udhr_article_3__positive_entitlement_reading, base_extractiveness, 1990, 0.78).
narrative_ontology:measurement(udhr_be_t2010, udhr_article_3__positive_entitlement_reading, base_extractiveness, 2010, 0.82).
narrative_ontology:measurement(udhr_be_t2024, udhr_article_3__positive_entitlement_reading, base_extractiveness, 2024, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(udhr_su_t1948, udhr_article_3__positive_entitlement_reading, suppression_requirement, 1948, 0.5).
narrative_ontology:measurement(udhr_su_t1970, udhr_article_3__positive_entitlement_reading, suppression_requirement, 1970, 0.58).
narrative_ontology:measurement(udhr_su_t1990, udhr_article_3__positive_entitlement_reading, suppression_requirement, 1990, 0.65).
narrative_ontology:measurement(udhr_su_t2010, udhr_article_3__positive_entitlement_reading, suppression_requirement, 2010, 0.68).
narrative_ontology:measurement(udhr_su_t2024, udhr_article_3__positive_entitlement_reading, suppression_requirement, 2024, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(udhr_article_3__positive_entitlement_reading, resource_allocation).
narrative_ontology:affects_constraint(udhr_article_3__positive_entitlement_reading, udhr_article_25__right_to_adequate_standard_of_living).
narrative_ontology:affects_constraint(udhr_article_3__positive_entitlement_reading, iccescr_article_11__right_to_adequate_standard_of_living).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
