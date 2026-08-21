% ============================================================================
% CONSTRAINT STORY: technology_reformation_causality__co_constitution_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_technology_reformation_causality__co_constitution_reading, []).

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
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: technology_reformation_causality__co_constitution_reading
 *   human_readable: Co-Constitutive Causality of Technology and Reformation
 *   domain: history_of_technology/religious_history/media_studies
 *
 * SUMMARY:
 *   This constraint story instantiates the 'co-constitution' reading of the
 *   broader 'technology_reformation_causality' kernel. It posits that
 *   technology (like the printing press) and social actors (like Reformation
 *   reformers) co-evolved, with the press enabling but not solely determining
 *   the Reformation's trajectory, and reformers actively shaping what the
 *   press produced. This reading emphasizes bidirectional causality and
 *   mutual shaping, challenging simpler, monocausal explanations. The claimed
 *   type is 'rope' because it describes a coordination of complex forces and
 *   explanatory elements.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(technology_reformation_causality__co_constitution_reading, 0.45).
domain_priors:suppression_score(technology_reformation_causality__co_constitution_reading, 0.25).
domain_priors:theater_ratio(technology_reformation_causality__co_constitution_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(technology_reformation_causality__co_constitution_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(technology_reformation_causality__co_constitution_reading, suppression_requirement, 0.25).
narrative_ontology:constraint_metric(technology_reformation_causality__co_constitution_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(technology_reformation_causality__co_constitution_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(technology_reformation_causality__co_constitution_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(technology_reformation_causality__co_constitution_reading, rope).
narrative_ontology:human_readable(technology_reformation_causality__co_constitution_reading, "Co-Constitutive Causality of Technology and Reformation").
narrative_ontology:topic_domain(technology_reformation_causality__co_constitution_reading, "history_of_technology/religious_history/media_studies").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(technology_reformation_causality__co_constitution_reading, '96c9bcb3-7070-4ae2-9735-5fbf34d3869c').
narrative_ontology:cs_kernel_codification('96c9bcb3-7070-4ae2-9735-5fbf34d3869c', implicit).
narrative_ontology:cs_authority_grounding('96c9bcb3-7070-4ae2-9735-5fbf34d3869c', expertise).
narrative_ontology:cs_interpretation_layer_present('96c9bcb3-7070-4ae2-9735-5fbf34d3869c').
narrative_ontology:cs_reading_relation('96c9bcb3-7070-4ae2-9735-5fbf34d3869c', technology_reformation_causality__technological_determinism_reading, forecloses).
narrative_ontology:cs_reading_relation('96c9bcb3-7070-4ae2-9735-5fbf34d3869c', technology_reformation_causality__beneficiary_agency_reading, influences).
narrative_ontology:cs_axiom('96c9bcb3-7070-4ae2-9735-5fbf34d3869c', foundational, bidirectional_causality_between_tech_and_society).
narrative_ontology:cs_axiom_status(bidirectional_causality_between_tech_and_society, holdable).
narrative_ontology:cs_axiom_grounding('96c9bcb3-7070-4ae2-9735-5fbf34d3869c', bidirectional_causality_between_tech_and_society, empirically_contingent).
narrative_ontology:cs_axiom('96c9bcb3-7070-4ae2-9735-5fbf34d3869c', foundational, technology_enables_but_does_not_determine).
narrative_ontology:cs_axiom_status(technology_enables_but_does_not_determine, holdable).
narrative_ontology:cs_axiom_grounding('96c9bcb3-7070-4ae2-9735-5fbf34d3869c', technology_enables_but_does_not_determine, empirically_contingent).
narrative_ontology:cs_reference_frame('96c9bcb3-7070-4ae2-9735-5fbf34d3869c', complex_historical_interaction_framework).
narrative_ontology:cs_drift_state('96c9bcb3-7070-4ae2-9735-5fbf34d3869c', contemporary_interdisciplinary_scholarship, gap(stable, minor, true)).
narrative_ontology:cs_created_at('96c9bcb3-7070-4ae2-9735-5fbf34d3869c', '').
narrative_ontology:cs_kernel_id(technology_reformation_causality__co_constitution_reading, technology_reformation_causality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(technology_reformation_causality__co_constitution_reading, co_constitution_scholars).
narrative_ontology:constraint_beneficiary(technology_reformation_causality__co_constitution_reading, reformation_reformers).
narrative_ontology:constraint_victim(technology_reformation_causality__co_constitution_reading, technological_determinists).
narrative_ontology:constraint_victim(technology_reformation_causality__co_constitution_reading, beneficiary_agency_advocates).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(technology_reformation_causality__co_constitution_reading, reformation_reformers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Propose and refine the co-constitution framework, benefiting from its explanatory power and intellectual rigor. They actively shape the discourse around technology and social change.
narrative_ontology:constraint_stakeholder(technology_reformation_causality__co_constitution_reading, co_constitution_scholars, agenda_setter,
    institutional, generational, analytical, global).
narrative_ontology:stakeholder_secondary_role(technology_reformation_causality__co_constitution_reading, co_constitution_scholars, beneficiary).

% Adhere to a view where technology is the primary, unidirectional driver of social change. Their explanatory model is challenged and partially displaced by the co-constitution reading, making them 'pay' in terms of intellectual capital and influence.
narrative_ontology:constraint_stakeholder(technology_reformation_causality__co_constitution_reading, technological_determinists, payer,
    powerful, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(technology_reformation_causality__co_constitution_reading, technological_determinists, excluded).

% Emphasize the sole agency of social actors in deploying technology as a tool. Their model, while acknowledging agency, is contextualized and limited by the co-constitution reading, leading to a 'cost' in the scope of their claims.
narrative_ontology:constraint_stakeholder(technology_reformation_causality__co_constitution_reading, beneficiary_agency_advocates, payer,
    powerful, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(technology_reformation_causality__co_constitution_reading, beneficiary_agency_advocates, excluded).

% Historically, they shaped the content and distribution of printed materials, but were also enabled and constrained by the capabilities and spread of the printing press. Their agency is understood as situated within this co-evolution, rather than purely independent. The 'piton' aspect refers to the atrophied alternatives for their agency outside this co-constitutive dynamic.
narrative_ontology:constraint_stakeholder(technology_reformation_causality__co_constitution_reading, reformation_reformers, beneficiary,
    organized, biographical, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(technology_reformation_causality__co_constitution_reading, reformation_reformers, payer).

% The technology itself, as a force that enabled new forms of communication and organization, but whose development and application were also shaped by social demands and innovations. It 'benefited' by being widely adopted and integrated into social processes.
narrative_ontology:constraint_stakeholder(technology_reformation_causality__co_constitution_reading, printing_press_technology, beneficiary,
    moderate, generational, trapped, continental).
narrative_ontology:stakeholder_non_agent(technology_reformation_causality__co_constitution_reading, printing_press_technology).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a framework for understanding how technological capabilities (e.g., printing press) and social actors (e.g., Reformation reformers) mutually adapted, enabled, and constrained each other, leading to complex historical outcomes like the spread of the Reformation.
% TRANSFER_FUNCTION: Transfers explanatory power from monocausal or unidirectional historical narratives to a more nuanced, interactive, and bidirectional understanding of causality between technology and society.
% ABSENT_VOICES: Scholars who prefer simpler, more direct causal explanations (either purely technological or purely social) are conceptually excluded from the internal logic of the co-constitution framework, as it directly challenges their foundational premises.
% DISAPPEARANCE_RATIONALE: The co-constitution reading is a scholarly interpretation of historical events. If this specific framework vanished, the historical events themselves would remain unchanged, though the dominant explanatory narratives might revert to simpler, less nuanced forms.
% FOUNDING_PROBLEM: To accurately explain the complex interplay between technological innovation and social change during periods like the Reformation, avoiding the pitfalls of both technological determinism and pure social constructivism.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated by interdisciplinary historical research, media studies, and sociology of technology, which consistently find evidence of mutual shaping rather than unidirectional causality. This is attested by a broad academic consensus outside the immediate proponents of this specific reading.
narrative_ontology:disappearance_verdict(technology_reformation_causality__co_constitution_reading, world_unchanged).
narrative_ontology:founding_problem_status(technology_reformation_causality__co_constitution_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(technology_reformation_causality__co_constitution_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(technology_reformation_causality__co_constitution_reading, 'none', 1).
narrative_ontology:epsilon_provenance(technology_reformation_causality__co_constitution_reading, 0.45, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(technology_reformation_causality__co_constitution_reading_tests).
:- end_tests(technology_reformation_causality__co_constitution_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness is moderate because while the framework offers significant explanatory benefits, it also demands intellectual effort to grapple with complexity, and it 'extracts' explanatory simplicity from competing narratives. Suppression is low but present, as the framework implicitly pushes back against the adoption of simpler, less nuanced causal models. Theater ratio is low, reflecting the genuine scholarly effort behind this interpretation. Accessibility collapse is moderate, as it makes monocausal explanations less intellectually viable without fully eliminating them. Resistance is moderate, as proponents of simpler models continue to advocate for their views.
 *
 * PERSPECTIVAL GAP:
 *   The primary perspectival gap exists between scholars who embrace this complex, co-constitutive view and those who prefer simpler, monocausal explanations. The former see a more accurate and robust understanding of history, while the latter may perceive an unnecessary complication or a dilution of their preferred causal emphasis.
 *
 * DIRECTIONALITY LOGIC:
 *   Scholars adopting the co-constitution view are beneficiaries, as it provides a robust explanatory framework. Reformation reformers are beneficiaries in that their historical impact is well-explained, but also payers in that their agency is contextualized rather than absolute. Proponents of technological determinism and pure beneficiary agency are 'victims' in the sense that their explanatory models are challenged and partially displaced by this more complex view.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    balance_of_agency_in_co_constitution,
    'Within the co-constitution framework, what is the precise balance and relative weight of technological agency versus social agency in shaping historical outcomes?',
    'Further detailed historical case studies and quantitative analyses that attempt to disentangle and measure the specific contributions of each factor in different contexts.',
    'A clearer understanding of the balance could refine the ''extractiveness'' metric, as a more balanced view might imply less ''cost'' to any single factor''s agency. It could also influence the ''influences'' relation with the beneficiary_agency_reading, making it closer to ''coexists_with'' if agency is found to be more robust.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(balance_of_agency_in_co_constitution, empirical, 'Ambiguity in the precise weighting of technological vs. social forces within the co-constitutive process.').

omega_variable(
    piton_status_of_reformers_agency,
    'Is the ''piton'' aspect of reformers'' agency (atrophied alternatives) a true structural feature of the co-constitution, or merely a re-contextualization that still allows for robust, albeit situated, agency?',
    'Conceptual clarification within the co-constitution framework, potentially by developing sub-theories that distinguish between ''situated agency'' and ''atrophied alternatives'' based on the degree of constraint and available counterfactuals.',
    'If ''atrophied alternatives'' is deemed too strong, the ''payer'' role for reformers might be softened, and their ''exit_options'' might shift from ''identity_locked'' to ''constrained'', reflecting more latent agency. This would slightly reduce the perceived ''extractiveness'' of the framework from their perspective.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(piton_status_of_reformers_agency, conceptual, 'Ambiguity regarding the extent to which reformers'' agency is truly ''atrophied'' or merely re-contextualized within the co-constitutive framework.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(technology_reformation_causality__co_constitution_reading, 1950, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tech_tr_t1950, technology_reformation_causality__co_constitution_reading, theater_ratio, 1950, 0.1).
narrative_ontology:measurement(tech_tr_t1970, technology_reformation_causality__co_constitution_reading, theater_ratio, 1970, 0.1).
narrative_ontology:measurement(tech_tr_t1990, technology_reformation_causality__co_constitution_reading, theater_ratio, 1990, 0.1).
narrative_ontology:measurement(tech_tr_t2020, technology_reformation_causality__co_constitution_reading, theater_ratio, 2020, 0.1).

% Extraction over time
narrative_ontology:measurement(tech_be_t1950, technology_reformation_causality__co_constitution_reading, base_extractiveness, 1950, 0.35).
narrative_ontology:measurement(tech_be_t1970, technology_reformation_causality__co_constitution_reading, base_extractiveness, 1970, 0.4).
narrative_ontology:measurement(tech_be_t1990, technology_reformation_causality__co_constitution_reading, base_extractiveness, 1990, 0.43).
narrative_ontology:measurement(tech_be_t2020, technology_reformation_causality__co_constitution_reading, base_extractiveness, 2020, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(tech_su_t1950, technology_reformation_causality__co_constitution_reading, suppression_requirement, 1950, 0.15).
narrative_ontology:measurement(tech_su_t1970, technology_reformation_causality__co_constitution_reading, suppression_requirement, 1970, 0.2).
narrative_ontology:measurement(tech_su_t1990, technology_reformation_causality__co_constitution_reading, suppression_requirement, 1990, 0.23).
narrative_ontology:measurement(tech_su_t2020, technology_reformation_causality__co_constitution_reading, suppression_requirement, 2020, 0.25).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(technology_reformation_causality__co_constitution_reading, information_standard).
narrative_ontology:affects_constraint(technology_reformation_causality__co_constitution_reading, technology_reformation_causality__technological_determinism_reading).
narrative_ontology:affects_constraint(technology_reformation_causality__co_constitution_reading, technology_reformation_causality__beneficiary_agency_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'technology_reformation_causality' kernel, each representing a distinct causal framework for the relationship between technology and the Reformation. They are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
