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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
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
 *   constraint_id: udhr_article_3__positive_entitlement_reading
 *   human_readable: UDHR Article 3: Positive Entitlement to Material Conditions
 *   domain: constitutional_law/human_rights/political_philosophy
 *
 * SUMMARY:
 *   This constraint represents the 'positive entitlement' reading of Article
 *   3 of the Universal Declaration of Human Rights (UDHR), which interprets
 *   the right to life, liberty, and security of person as obligating states
 *   to provide the material conditions necessary for these rights (e.g.,
 *   welfare, healthcare, housing). This reading necessitates significant
 *   state intervention, often involving wealth redistribution and regulation,
 *   leading to high extractiveness and suppression for certain groups, while
 *   providing substantial benefits to others. It is claimed as a Tangled Rope
 *   due to its dual function of coordinating social welfare provision and
 *   extracting resources to do so, requiring active enforcement.
 *
 * KEY AGENTS:
 *   - vulnerable_citizens: Primary beneficiary (powerless/constrained) — receives state provision
 *   - social_welfare_agencies: Secondary beneficiary/agenda_setter (institutional/analytical) — administers and benefits from expanded mandate
 *   - property_owners: Primary payer (powerful/constrained) — bears wealth redistribution
 *   - taxpayers: Payer (moderate/constrained) — bears the financial burden of state provision
 *   - free_speech_advocates: Victim (organized/constrained) — may experience suppression of expression deemed harmful to 'security'
 *   - state_legislatures: Agenda setter (institutional/mobile) — enacts laws to implement entitlements
 *   - constitutional_courts: Agenda setter/observer (institutional/analytical) — adjudicates the scope and limits of these entitlements
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(udhr_article_3__positive_entitlement_reading, 0.85).
domain_priors:suppression_score(udhr_article_3__positive_entitlement_reading, 0.75).
domain_priors:theater_ratio(udhr_article_3__positive_entitlement_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(udhr_article_3__positive_entitlement_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(udhr_article_3__positive_entitlement_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(udhr_article_3__positive_entitlement_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(udhr_article_3__positive_entitlement_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(udhr_article_3__positive_entitlement_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(udhr_article_3__positive_entitlement_reading, tangled_rope).
narrative_ontology:human_readable(udhr_article_3__positive_entitlement_reading, "UDHR Article 3: Positive Entitlement to Material Conditions").
narrative_ontology:topic_domain(udhr_article_3__positive_entitlement_reading, "constitutional_law/human_rights/political_philosophy").

domain_priors:requires_active_enforcement(udhr_article_3__positive_entitlement_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(udhr_article_3__positive_entitlement_reading, '3a97bbc4-70f0-452d-8bf1-a87ab9473310').
narrative_ontology:cs_kernel_codification('3a97bbc4-70f0-452d-8bf1-a87ab9473310', fixed_text).
narrative_ontology:cs_authority_grounding('3a97bbc4-70f0-452d-8bf1-a87ab9473310', lineage).
narrative_ontology:cs_interpretation_layer_present('3a97bbc4-70f0-452d-8bf1-a87ab9473310').
narrative_ontology:cs_reading_relation('3a97bbc4-70f0-452d-8bf1-a87ab9473310', udhr_article_3__negative_liberty_reading, coexists_with).
narrative_ontology:cs_reading_relation('3a97bbc4-70f0-452d-8bf1-a87ab9473310', udhr_article_3__procedural_hybrid_reading, coexists_with).
narrative_ontology:cs_axiom('3a97bbc4-70f0-452d-8bf1-a87ab9473310', foundational, state_has_positive_obligations).
narrative_ontology:cs_axiom_status(state_has_positive_obligations, holdable).
narrative_ontology:cs_axiom_grounding('3a97bbc4-70f0-452d-8bf1-a87ab9473310', state_has_positive_obligations, deontological).
narrative_ontology:cs_axiom('3a97bbc4-70f0-452d-8bf1-a87ab9473310', foundational, material_conditions_are_prerequisite_for_rights).
narrative_ontology:cs_axiom_status(material_conditions_are_prerequisite_for_rights, holdable).
narrative_ontology:cs_axiom_grounding('3a97bbc4-70f0-452d-8bf1-a87ab9473310', material_conditions_are_prerequisite_for_rights, empirically_contingent).
narrative_ontology:cs_reference_frame('3a97bbc4-70f0-452d-8bf1-a87ab9473310', welfare_state_paradigm).
narrative_ontology:cs_drift_state('3a97bbc4-70f0-452d-8bf1-a87ab9473310', contemporary_neoliberal_era, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('3a97bbc4-70f0-452d-8bf1-a87ab9473310', '').
narrative_ontology:cs_kernel_id(udhr_article_3__positive_entitlement_reading, udhr_article_3).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(udhr_article_3__positive_entitlement_reading, vulnerable_citizens).
narrative_ontology:constraint_beneficiary(udhr_article_3__positive_entitlement_reading, social_welfare_agencies).
narrative_ontology:constraint_victim(udhr_article_3__positive_entitlement_reading, property_owners).
narrative_ontology:constraint_victim(udhr_article_3__positive_entitlement_reading, taxpayers).
narrative_ontology:constraint_victim(udhr_article_3__positive_entitlement_reading, free_speech_advocates).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(udhr_article_3__positive_entitlement_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(udhr_article_3__positive_entitlement_reading, 'none', 1).

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
 *   The high extractiveness (0.85) reflects the substantial resources transferred from property owners and taxpayers to fund social welfare programs. Suppression (0.75) is high due to the coercive nature of taxation and potential restrictions on other rights (e.g., hate speech laws justified by 'security'). The theater ratio (0.20) is relatively low, indicating that the state's actions are largely functional in providing services, though some performative aspects exist in justifying interventions. Accessibility collapse (0.40) is moderate; while state provision aims to ensure access, alternatives (private healthcare, housing) still exist for those who can afford them. Resistance (0.70) is significant, stemming from those who bear the costs or perceive their rights as being infringed.
 *
 * PERSPECTIVAL GAP:
 *   Vulnerable citizens experience this as a vital Rope or even a Mountain, providing essential support. Property owners and taxpayers, however, experience it as a Snare, extracting resources coercively. State legislatures and courts navigate the tension, attempting to balance competing rights and obligations. The engine's per-seat classification will reflect these divergences based on the declared structural relationships.
 *
 * DIRECTIONALITY LOGIC:
 *   Vulnerable citizens are full beneficiaries (d=0.0) as they receive direct state provision. Social welfare agencies are also beneficiaries (d=0.1) due to expanded mandates and resources. Property owners and taxpayers are targets (d=0.9-1.0) as they bear the direct costs of redistribution and taxation. Free speech advocates are also targets (d=0.8) if their expression is curtailed for 'security'. State legislatures and constitutional courts, while agenda-setters, have a more symmetric directionality (d=0.5) as they balance competing interests and uphold the constitutional framework.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading of Article 3 is not subject to mandatrophy in the traditional sense, as the 'founding problem' of ensuring basic human dignity and security remains live. However, the *means* of addressing it (e.g., specific welfare programs, levels of taxation) are constantly contested. The classification as Tangled Rope prevents mislabeling it as a pure Rope (ignoring extraction) or a pure Snare (ignoring coordination function).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identification,
    'Is this constraint a genuine interpretation of UDHR Article 3, or an overreach of state power?',
    'Analysis of international jurisprudence and state practice, particularly in jurisdictions with strong social rights provisions.',
    'If a genuine interpretation, it strengthens the legitimacy of state intervention for social welfare. If an overreach, it highlights the potential for human rights instruments to be used for extractive purposes.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'This constraint is the ''positive_entitlement_reading'' of the ''udhr_article_3'' kernel. Sibling readings include ''negative_liberty_reading'' (prohibits state deprivation) and ''procedural_hybrid_reading'' (due process guarantees). This reading differs by asserting active state obligations.').

omega_variable(
    scope_of_entitlement,
    'What is the precise scope of ''material conditions necessary for life and security'' and how are these determined?',
    'Legislative action, judicial precedent, and expert consensus on minimum standards for welfare, healthcare, and housing.',
    'A broad interpretation increases state obligations and potential extraction from taxpayers; a narrow interpretation limits state intervention and potential benefits to vulnerable groups.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scope_of_entitlement, empirical, 'The specific definition of ''material conditions'' is subject to ongoing debate and empirical measurement of societal needs.').

omega_variable(
    balancing_rights,
    'How are the positive entitlements of this reading balanced against other rights, such as property rights and freedom of expression, which may be curtailed by state action to fulfill these entitlements?',
    'Constitutional review processes, proportionality tests in legal adjudication, and public discourse on competing rights claims.',
    'The balance struck determines the effective extractiveness and suppression experienced by different stakeholder groups. A strong emphasis on positive entitlements may lead to greater limitations on other rights.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(balancing_rights, preference, 'The inherent tension between positive entitlements and negative liberties, particularly concerning property and speech, is a core conceptual ambiguity.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(udhr_article_3__positive_entitlement_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(udhr_tr_t0, udhr_article_3__positive_entitlement_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(udhr_tr_t5, udhr_article_3__positive_entitlement_reading, theater_ratio, 5, 0.23).
narrative_ontology:measurement(udhr_tr_t10, udhr_article_3__positive_entitlement_reading, theater_ratio, 10, 0.22).
narrative_ontology:measurement(udhr_tr_t15, udhr_article_3__positive_entitlement_reading, theater_ratio, 15, 0.21).
narrative_ontology:measurement(udhr_tr_t20, udhr_article_3__positive_entitlement_reading, theater_ratio, 20, 0.2).

% Extraction over time
narrative_ontology:measurement(udhr_be_t0, udhr_article_3__positive_entitlement_reading, base_extractiveness, 0, 0.6).
narrative_ontology:measurement(udhr_be_t5, udhr_article_3__positive_entitlement_reading, base_extractiveness, 5, 0.68).
narrative_ontology:measurement(udhr_be_t10, udhr_article_3__positive_entitlement_reading, base_extractiveness, 10, 0.75).
narrative_ontology:measurement(udhr_be_t15, udhr_article_3__positive_entitlement_reading, base_extractiveness, 15, 0.8).
narrative_ontology:measurement(udhr_be_t20, udhr_article_3__positive_entitlement_reading, base_extractiveness, 20, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(udhr_su_t0, udhr_article_3__positive_entitlement_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(udhr_su_t5, udhr_article_3__positive_entitlement_reading, suppression_requirement, 5, 0.58).
narrative_ontology:measurement(udhr_su_t10, udhr_article_3__positive_entitlement_reading, suppression_requirement, 10, 0.65).
narrative_ontology:measurement(udhr_su_t15, udhr_article_3__positive_entitlement_reading, suppression_requirement, 15, 0.7).
narrative_ontology:measurement(udhr_su_t20, udhr_article_3__positive_entitlement_reading, suppression_requirement, 20, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(udhr_article_3__positive_entitlement_reading, resource_allocation).

% DUAL FORMULATION NOTE:
% This constraint is one of three distinct readings of UDHR Article 3, each with different structural implications and classifications. This reading focuses on positive state obligations.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
