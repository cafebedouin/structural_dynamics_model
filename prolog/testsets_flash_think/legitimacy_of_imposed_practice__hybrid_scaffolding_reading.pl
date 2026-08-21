% ============================================================================
% CONSTRAINT STORY: legitimacy_of_imposed_practice__hybrid_scaffolding_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_legitimacy_of_imposed_practice__hybrid_scaffolding_reading, []).

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
 *   constraint_id: legitimacy_of_imposed_practice__hybrid_scaffolding_reading
 *   human_readable: Hybrid Scaffolding of Imposed Cultural Practices (Dress Reform)
 *   domain: political_history/state_formation/cultural_imposition
 *
 * SUMMARY:
 *   This constraint describes the 'hybrid scaffolding' approach to cultural
 *   imposition, exemplified by dress reform, where a top-down state mandate
 *   is reinforced by ideological messaging and elite modeling to generate a
 *   'quasi-endogenous pull' for new practices. This reading contrasts with
 *   pure decree (which fails) and pure endogenous climb (which is slow). The
 *   constraint is claimed as a Scaffold due to its transitional intent to
 *   displace prior practices, even if the transition is partial and
 *   indefinite. The metrics reflect a moderately extractive and suppressive
 *   system, with functional (not purely theatrical) ideological components.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, 0.65).
domain_priors:suppression_score(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, 0.55).
domain_priors:theater_ratio(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, scaffold).
narrative_ontology:human_readable(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, "Hybrid Scaffolding of Imposed Cultural Practices (Dress Reform)").
narrative_ontology:topic_domain(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, "political_history/state_formation/cultural_imposition").

domain_priors:requires_active_enforcement(legitimacy_of_imposed_practice__hybrid_scaffolding_reading).
narrative_ontology:has_sunset_clause(legitimacy_of_imposed_practice__hybrid_scaffolding_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, '6b1339a9-ba45-4644-a3ae-c1379e1e45e8').
narrative_ontology:cs_kernel_codification('6b1339a9-ba45-4644-a3ae-c1379e1e45e8', formalized).
narrative_ontology:cs_authority_grounding('6b1339a9-ba45-4644-a3ae-c1379e1e45e8', extraction).
narrative_ontology:cs_interpretation_layer_present('6b1339a9-ba45-4644-a3ae-c1379e1e45e8').
narrative_ontology:cs_reading_relation('6b1339a9-ba45-4644-a3ae-c1379e1e45e8', legitimacy_of_imposed_practice__exogenous_override_reading, coexists_with).
narrative_ontology:cs_reading_relation('6b1339a9-ba45-4644-a3ae-c1379e1e45e8', legitimacy_of_imposed_practice__endogenous_climb_reading, coexists_with).
narrative_ontology:cs_axiom('6b1339a9-ba45-4644-a3ae-c1379e1e45e8', foundational, state_guided_cultural_evolution_is_necessary).
narrative_ontology:cs_axiom_status(state_guided_cultural_evolution_is_necessary, holdable).
narrative_ontology:cs_axiom_grounding('6b1339a9-ba45-4644-a3ae-c1379e1e45e8', state_guided_cultural_evolution_is_necessary, instrumental).
narrative_ontology:cs_axiom('6b1339a9-ba45-4644-a3ae-c1379e1e45e8', secondary, ideological_framing_generates_legitimacy).
narrative_ontology:cs_axiom_status(ideological_framing_generates_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('6b1339a9-ba45-4644-a3ae-c1379e1e45e8', ideological_framing_generates_legitimacy, empirically_contingent).
narrative_ontology:cs_reference_frame('6b1339a9-ba45-4644-a3ae-c1379e1e45e8', modern_national_identity_framework).
narrative_ontology:cs_drift_state('6b1339a9-ba45-4644-a3ae-c1379e1e45e8', contemporary_postcolonial_critique, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('6b1339a9-ba45-4644-a3ae-c1379e1e45e8', '').
narrative_ontology:cs_kernel_id(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, legitimacy_of_imposed_practice).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, state_modernization_project).
narrative_ontology:constraint_beneficiary(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, urban_elites_adopting_western_identity_markers).
narrative_ontology:constraint_victim(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, rural_traditionalists).
narrative_ontology:constraint_victim(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, cultural_heritage_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The overarching state apparatus driving modernization. It initiates and enforces the top-down mandates, leveraging ideological messaging and elite modeling to achieve cultural shifts, such as in dress codes, to align with a 'modern' national identity. It benefits from increased internal cohesion and international legitimacy.
narrative_ontology:constraint_stakeholder(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, state_modernization_project, agenda_setter,
    institutional, generational, arbitrage, national).

% Urban populations, particularly those with social and political influence, who adopt the new, imposed cultural practices (e.g., Western dress). They benefit from enhanced social mobility, access to state resources, and alignment with the 'modern' national identity promoted by the state. Their adoption provides a visible model for the scaffolding.
narrative_ontology:constraint_stakeholder(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, urban_elites_adopting_western_identity_markers, beneficiary,
    powerful, biographical, mobile, national).

% Rural populations and communities deeply rooted in traditional cultural practices. They bear the cost of cultural displacement, social marginalization, and economic exclusion for not conforming to the imposed norms. Their traditional dress and customs are devalued, and they lack access to the scaffolding infrastructure (e.g., state media, elite schools) that promotes the new norms.
narrative_ontology:constraint_stakeholder(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, rural_traditionalists, payer,
    powerless, generational, trapped, local).

% Individuals and groups who actively resist the imposition of new cultural practices and advocate for the preservation of traditional heritage. They face social pressure, academic marginalization, and sometimes direct suppression from the state. They bear the cost of defending devalued cultural forms.
narrative_ontology:constraint_stakeholder(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, cultural_heritage_advocates, payer,
    moderate, generational, constrained, national).

% State-sponsored intellectuals, media figures, and educators responsible for crafting and disseminating the ideological messaging that reinforces the top-down mandates. They benefit from state patronage and influence, and their role is crucial in generating the 'quasi-endogenous pull' for the imposed practices.
narrative_ontology:constraint_stakeholder(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, ideological_propagandists, agenda_setter,
    organized, biographical, constrained, national).

% Academics, NGOs, and foreign governments who analyze and comment on the state's modernization efforts, often from a perspective of human rights, cultural preservation, or development. They can influence international opinion and provide external corroboration or critique of the state's claims.
narrative_ontology:constraint_stakeholder(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, international_observers, observer,
    analytical, biographical, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To align the population, particularly urban elites, with a new national identity and modernizing vision through cultural practices like dress, facilitating social cohesion around state goals and presenting a unified image to the international community.
% TRANSFER_FUNCTION: Transfers social capital, legitimacy, and economic opportunities towards those adopting the new practices (urban elites) and away from those adhering to traditional ones (rural traditionalists). It also transfers cultural authority from traditional institutions to the state.
% ABSENT_VOICES: Rural populations and traditional cultural leaders, whose perspectives on the value of existing practices and the costs of imposition are marginalized or suppressed. Their voices are often framed as 'backward' or 'anti-progress' by the state's ideological messaging.
% DISAPPEARANCE_RATIONALE: If the scaffolding (top-down mandates, ideological messaging, elite modeling) vanished overnight, the imposed practices would likely recede, and traditional practices would reassert themselves, leading to a re-evaluation of national identity and cultural norms. The 'partial displacement' would reverse, and the state's legitimacy based on this modernization project would be challenged.
% FOUNDING_PROBLEM: To overcome perceived 'backwardness' and fragmentation, and to forge a unified, modern national identity aligned with international (Western) standards, thereby strengthening the state's legitimacy and capacity in a rapidly changing global order.
% FOUNDING_PROBLEM_CORROBORATION: State narratives and some urban intellectuals attest to the problem's live status, citing ongoing needs for national unity and international competitiveness. Rural communities, cultural historians, and international observers often contest this, arguing the 'problem' was a construct to justify state power and cultural imposition, and that the original problem is substantially solved or was misdiagnosed.
narrative_ontology:disappearance_verdict(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, world_rearranges).
narrative_ontology:founding_problem_status(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, 'none', 1).
narrative_ontology:epsilon_provenance(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(legitimacy_of_imposed_practice__hybrid_scaffolding_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(legitimacy_of_imposed_practice__hybrid_scaffolding_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate-high (0.65) because the imposition devalues traditional cultural forms and creates social/economic costs for non-compliance, particularly for rural populations. Suppression is moderate (0.55) as it relies on a combination of state mandates, social pressure, and ideological conditioning rather than overt coercion. The theater ratio is low-moderate (0.25) because the ideological messaging and elite modeling are genuinely functional in generating adoption, rather than being purely performative. The 'has_sunset_clause: true' reflects the structural intent of scaffolding as a temporary support for a transition, even if the actual timeline for 'endogenous' adoption is indefinite.
 *
 * PERSPECTIVAL GAP:
 *   From the state's perspective, this is a necessary and beneficial modernization effort, a Scaffold for national progress. From the perspective of rural traditionalists, it is an extractive imposition that erodes their cultural identity. The engine's classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   The state modernization project and urban elites are beneficiaries, gaining legitimacy and social capital from the new practices. Rural traditionalists and cultural heritage advocates are targets, bearing the costs of cultural displacement and marginalization. Ideological propagandists are agenda-setters, actively shaping the narrative and benefiting from state patronage. International observers provide an analytical, external perspective.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    scaffolding_permanence_vs_transition,
    'Is the ''scaffolding'' truly intended as a temporary support for transition, or has it become a permanent mechanism for maintaining state control and cultural hegemony?',
    'Longitudinal study of state policy and resource allocation: if the state continues to invest heavily in enforcement and ideological propagation after initial ''displacement'' goals are met, it suggests permanence. If support is gradually withdrawn as practices become self-sustaining, it suggests genuine transition.',
    'If permanent, the constraint shifts from a Scaffold (transitional support) towards a Tangled Rope or Snare (ongoing extraction/control). If genuinely transitional, its Scaffold classification is reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scaffolding_permanence_vs_transition, conceptual, 'Ambiguity of the scaffolding''s intended duration and function.').

omega_variable(
    internalized_vs_performative_adoption,
    'To what extent is the adoption of new cultural practices (e.g., dress) a result of genuine internalization of the new ideology, versus performative compliance driven by social pressure or economic incentives?',
    'Sociological surveys and ethnographic studies examining private vs. public behavior, and attitudes towards traditional practices in contexts free from state surveillance or social pressure. Analysis of economic mobility linked to adoption.',
    'If adoption is primarily performative, the constraint''s effective suppression and extractiveness are higher, as it relies more on external pressure than genuine buy-in. If internalized, the constraint''s coordination function is stronger, and its long-term persistence is more robust.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(internalized_vs_performative_adoption, empirical, 'Distinguishing genuine cultural shift from coerced conformity.').

omega_variable(
    cultural_cost_vs_modernization_benefit,
    'How should the loss or devaluation of traditional cultural forms be weighed against the perceived benefits of modernization, national unity, and international legitimacy claimed by the state?',
    'This is a preference-based question, requiring a normative framework for evaluating cultural value, human rights, and state sovereignty. It cannot be resolved empirically but requires explicit value judgments.',
    'The classification''s ''goodness'' or ''badness'' depends on this underlying value judgment. A framework prioritizing cultural diversity might see higher extraction, while one prioritizing state-led development might see lower.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(cultural_cost_vs_modernization_benefit, preference, 'Normative trade-off between cultural preservation and state-defined modernization.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(legi_tr_t0, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(legi_tr_t10, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, theater_ratio, 10, 0.22).
narrative_ontology:measurement(legi_tr_t20, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, theater_ratio, 20, 0.24).
narrative_ontology:measurement(legi_tr_t30, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, theater_ratio, 30, 0.25).
narrative_ontology:measurement(legi_tr_t40, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, theater_ratio, 40, 0.24).
narrative_ontology:measurement(legi_tr_t50, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, theater_ratio, 50, 0.25).

% Extraction over time
narrative_ontology:measurement(legi_be_t0, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(legi_be_t10, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, base_extractiveness, 10, 0.6).
narrative_ontology:measurement(legi_be_t20, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, base_extractiveness, 20, 0.63).
narrative_ontology:measurement(legi_be_t30, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, base_extractiveness, 30, 0.65).
narrative_ontology:measurement(legi_be_t40, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, base_extractiveness, 40, 0.64).
narrative_ontology:measurement(legi_be_t50, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, base_extractiveness, 50, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(legi_su_t0, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(legi_su_t10, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, suppression_requirement, 10, 0.5).
narrative_ontology:measurement(legi_su_t20, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, suppression_requirement, 20, 0.53).
narrative_ontology:measurement(legi_su_t30, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, suppression_requirement, 30, 0.55).
narrative_ontology:measurement(legi_su_t40, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, suppression_requirement, 40, 0.54).
narrative_ontology:measurement(legi_su_t50, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, suppression_requirement, 50, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, identity_coordination).
narrative_ontology:affects_constraint(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, legitimacy_of_imposed_practice__exogenous_override_reading).
narrative_ontology:affects_constraint(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, legitimacy_of_imposed_practice__endogenous_climb_reading).
narrative_ontology:affects_constraint(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, state_education_system_curriculum).
narrative_ontology:affects_constraint(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, national_media_censorship).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'legitimacy_of_imposed_practice' kernel, focusing on the hybrid scaffolding mechanism. It is structurally distinct from the 'exogenous_override_reading' (pure decree) and 'endogenous_climb_reading' (pure bottom-up adoption), which represent alternative theories of cultural change and state formation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
