% ============================================================================
% CONSTRAINT STORY: commerce_clause_text__substantial_effects_limited_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_commerce_clause_text__substantial_effects_limited_reading, []).

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
 *   constraint_id: commerce_clause_text__substantial_effects_limited_reading
 *   human_readable: Commerce Clause: Substantial Effects Doctrine (Limited Reading)
 *   domain: constitutional_law/federalism
 *
 * SUMMARY:
 *   This constraint represents the 'substantial effects, limited reading' of
 *   the Commerce Clause, a judicial interpretation that emerged after the New
 *   Deal era. It grants the federal government power to regulate intrastate
 *   economic activity that substantially affects interstate commerce, but
 *   crucially, it imposes limits: the activity must be genuinely economic,
 *   and the regulation must not be a pretext for exercising general police
 *   powers reserved to the states. This reading seeks a middle ground between
 *   expansive federal power and a narrow originalist view, leading to a
 *   constantly contested boundary.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(commerce_clause_text__substantial_effects_limited_reading, 0.68).
domain_priors:suppression_score(commerce_clause_text__substantial_effects_limited_reading, 0.78).
domain_priors:theater_ratio(commerce_clause_text__substantial_effects_limited_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(commerce_clause_text__substantial_effects_limited_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(commerce_clause_text__substantial_effects_limited_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(commerce_clause_text__substantial_effects_limited_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(commerce_clause_text__substantial_effects_limited_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(commerce_clause_text__substantial_effects_limited_reading, resistance, 0.8).

% --- Constraint claim ---
narrative_ontology:constraint_claim(commerce_clause_text__substantial_effects_limited_reading, tangled_rope).
narrative_ontology:human_readable(commerce_clause_text__substantial_effects_limited_reading, "Commerce Clause: Substantial Effects Doctrine (Limited Reading)").
narrative_ontology:topic_domain(commerce_clause_text__substantial_effects_limited_reading, "constitutional_law/federalism").

domain_priors:requires_active_enforcement(commerce_clause_text__substantial_effects_limited_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(commerce_clause_text__substantial_effects_limited_reading, 'f1104761-5deb-44b1-b5a1-c9b7cb988f6e').
narrative_ontology:cs_kernel_codification('f1104761-5deb-44b1-b5a1-c9b7cb988f6e', fixed_text).
narrative_ontology:cs_authority_grounding('f1104761-5deb-44b1-b5a1-c9b7cb988f6e', lineage).
narrative_ontology:cs_interpretation_layer_present('f1104761-5deb-44b1-b5a1-c9b7cb988f6e').
narrative_ontology:cs_reading_relation('f1104761-5deb-44b1-b5a1-c9b7cb988f6e', commerce_clause_text__expansive_federal_reading, coexists_with).
narrative_ontology:cs_reading_relation('f1104761-5deb-44b1-b5a1-c9b7cb988f6e', commerce_clause_text__originalist_narrow_reading, coexists_with).
narrative_ontology:cs_axiom('f1104761-5deb-44b1-b5a1-c9b7cb988f6e', foundational, intrastate_economic_activity_subject_to_federal_power).
narrative_ontology:cs_axiom_status(intrastate_economic_activity_subject_to_federal_power, holdable).
narrative_ontology:cs_axiom_grounding('f1104761-5deb-44b1-b5a1-c9b7cb988f6e', intrastate_economic_activity_subject_to_federal_power, conventional).
narrative_ontology:cs_axiom('f1104761-5deb-44b1-b5a1-c9b7cb988f6e', foundational, non_pretextual_economic_regulation_required).
narrative_ontology:cs_axiom_status(non_pretextual_economic_regulation_required, holdable).
narrative_ontology:cs_axiom_grounding('f1104761-5deb-44b1-b5a1-c9b7cb988f6e', non_pretextual_economic_regulation_required, conventional).
narrative_ontology:cs_reference_frame('f1104761-5deb-44b1-b5a1-c9b7cb988f6e', post_new_deal_balancing_framework).
narrative_ontology:cs_drift_state('f1104761-5deb-44b1-b5a1-c9b7cb988f6e', contemporary_judicial_review, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('f1104761-5deb-44b1-b5a1-c9b7cb988f6e', '').
narrative_ontology:cs_kernel_id(commerce_clause_text__substantial_effects_limited_reading, commerce_clause_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(commerce_clause_text__substantial_effects_limited_reading, federal_government).
narrative_ontology:constraint_beneficiary(commerce_clause_text__substantial_effects_limited_reading, states).
narrative_ontology:constraint_beneficiary(commerce_clause_text__substantial_effects_limited_reading, economic_actors).
narrative_ontology:constraint_victim(commerce_clause_text__substantial_effects_limited_reading, federal_government).
narrative_ontology:constraint_victim(commerce_clause_text__substantial_effects_limited_reading, states).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(commerce_clause_text__substantial_effects_limited_reading, economic_actors).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Seeks to regulate national economic problems, benefiting from the ability to address issues beyond state borders. However, it is constrained by this reading from regulating non-economic intrastate activity or using commerce power as a pretext for police power, thus 'paying' by having its regulatory ambitions curtailed.
narrative_ontology:constraint_stakeholder(commerce_clause_text__substantial_effects_limited_reading, federal_government, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(commerce_clause_text__substantial_effects_limited_reading, federal_government, payer).

% Benefits from the preservation of traditional state police powers against federal encroachment. However, it 'pays' by being subject to legitimate federal regulation of genuinely economic intrastate activity that substantially affects interstate commerce, limiting its autonomy in those areas.
narrative_ontology:constraint_stakeholder(commerce_clause_text__substantial_effects_limited_reading, states, beneficiary,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(commerce_clause_text__substantial_effects_limited_reading, states, payer).

% The primary interpreter and enforcer of the Commerce Clause, defining the boundaries of federal power. Its decisions shape the application of this reading, balancing competing claims of federal and state authority.
narrative_ontology:constraint_stakeholder(commerce_clause_text__substantial_effects_limited_reading, supreme_court, agenda_setter,
    institutional, generational, analytical, national).

% Benefits from a predictable national market and the federal government's ability to address interstate economic problems. However, it 'pays' by being subject to federal regulation, which can impose compliance costs and limit business practices.
narrative_ontology:constraint_stakeholder(commerce_clause_text__substantial_effects_limited_reading, economic_actors, beneficiary,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(commerce_clause_text__substantial_effects_limited_reading, economic_actors, payer).

% Analyze and critique the Court's Commerce Clause jurisprudence, influencing legal discourse and potential future interpretations. They provide an external, analytical perspective on the constraint's operation and evolution.
narrative_ontology:constraint_stakeholder(commerce_clause_text__substantial_effects_limited_reading, legal_scholars, observer,
    analytical, generational, analytical, universal).

% Advocate for a broader interpretation of federal power under the Commerce Clause, believing it should extend to all activity with substantial aggregate effects on national markets, regardless of its economic nature. This reading's limits on federal power are a direct constraint on their preferred policy outcomes.
narrative_ontology:constraint_stakeholder(commerce_clause_text__substantial_effects_limited_reading, expansive_federalists, excluded,
    organized, generational, identity_locked, national).

% Advocate for a much narrower interpretation, limiting federal power to direct trade crossing state borders and instrumentalities of interstate movement. This reading's allowance for federal regulation of intrastate economic activity is a direct constraint on their preferred constitutional framework.
narrative_ontology:constraint_stakeholder(commerce_clause_text__substantial_effects_limited_reading, originalist_narrow_federalists, excluded,
    organized, generational, identity_locked, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To coordinate the division of regulatory authority between the federal government and the states, ensuring a functioning national economic market while preserving the states' traditional police powers and preventing federal overreach into non-economic local affairs.
% TRANSFER_FUNCTION: Transfers regulatory jurisdiction and the associated economic impacts between the federal government and the states, based on judicial determinations of whether intrastate activity is genuinely economic and has a substantial effect on interstate commerce.
% ABSENT_VOICES: Advocates for either a purely expansive federal power or a purely narrow originalist interpretation are structurally excluded from the core premise of this reading, which seeks a specific balance. Their arguments are heard in broader constitutional debates but are not central to the internal logic of this particular balancing framework.
% DISAPPEARANCE_RATIONALE: If this specific balancing act vanished, federal power would either become virtually unlimited (leading to a highly centralized regulatory state) or severely curtailed (leading to a fragmented national market and inability to address national economic problems), fundamentally altering the structure of American federalism and the economy.
% FOUNDING_PROBLEM: To define the legitimate scope of federal power under the Commerce Clause, preventing both federal overreach into traditional state police powers and state-level economic protectionism that harms the national market, while adapting to a complex, integrated national economy.
% FOUNDING_PROBLEM_CORROBORATION: Legal scholars, state attorneys general, and federal agencies (when their interests align with the balance) consistently corroborate the ongoing nature of this problem through litigation, legislative debates, and academic discourse. The Supreme Court's continued engagement with Commerce Clause cases further attests to its live status.
narrative_ontology:disappearance_verdict(commerce_clause_text__substantial_effects_limited_reading, world_rearranges).
narrative_ontology:founding_problem_status(commerce_clause_text__substantial_effects_limited_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(commerce_clause_text__substantial_effects_limited_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(commerce_clause_text__substantial_effects_limited_reading, 'none', 1).
narrative_ontology:epsilon_provenance(commerce_clause_text__substantial_effects_limited_reading, 0.68, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(commerce_clause_text__substantial_effects_limited_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(commerce_clause_text__substantial_effects_limited_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(commerce_clause_text__substantial_effects_limited_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint is classified as a Tangled Rope because it serves a genuine coordination function (balancing federal and state regulatory authority for a national economy) but involves asymmetric extraction. The federal government benefits from its ability to regulate national economic issues but is extracted from when its attempts to regulate non-economic activity are curtailed. States benefit from protected police powers but are extracted from when their intrastate economic activity falls under federal purview. The high resistance and suppression reflect the ongoing, active contestation and enforcement of these boundaries by the Supreme Court. The low theater ratio indicates that judicial interpretation is a genuine function, not mere performance.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the federal government seeking to regulate broadly, this constraint is extractive, limiting its power. From the perspective of states defending their autonomy, it is beneficial, protecting their police powers. The engine will compute these divergent classifications based on the declared roles and structural positions, reflecting the inherent tension in federalism.
 *
 * DIRECTIONALITY LOGIC:
 *   The federal government's directionality is complex: it benefits when its legitimate economic regulations are upheld (low d) but is a target when its overreach is struck down (high d). Similarly, states benefit when their police powers are protected (low d) but are targets when their economic activity is federally regulated (high d). Economic actors benefit from a stable national market (low d) but bear the costs of regulation (high d). The Supreme Court, as the arbiter, acts as an agenda-setter, defining the terms of this balance. Expansive and originalist federalists are excluded from the core framing of this reading, as their preferred outcomes are directly constrained by its balancing act.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    economic_vs_non_economic_distinction,
    'What constitutes ''economic activity'' versus ''non-economic activity'' for the purpose of Commerce Clause regulation, and how stable is this distinction?',
    'Further Supreme Court jurisprudence clarifying the boundaries, or empirical studies on how different activities are categorized and regulated across jurisdictions.',
    'If the distinction becomes clearer and more consistently applied, the constraint''s predictability increases, potentially reducing resistance. If it remains ambiguous or shifts frequently, the constraint''s effective suppression and extractiveness (from the perspective of those whose activities are reclassified) will remain high.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(economic_vs_non_economic_distinction, conceptual, 'Ambiguity in defining the scope of ''economic activity'' under the Commerce Clause.').

omega_variable(
    jurisdictional_nexus_strength,
    'How strong must the ''jurisdictional nexus'' be between intrastate activity and interstate commerce to justify federal regulation?',
    'New Supreme Court cases that explicitly define or refine the required strength of the nexus, or legislative action by Congress to clarify its intent regarding specific types of regulation.',
    'A clearer, more stringent nexus requirement would further limit federal power, increasing its extractiveness from the federal government and benefiting states. A looser requirement would have the opposite effect.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(jurisdictional_nexus_strength, empirical, 'Uncertainty regarding the required strength of the jurisdictional nexus for federal Commerce Clause power.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(commerce_clause_text__substantial_effects_limited_reading, 1937, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comm_tr_t1937, commerce_clause_text__substantial_effects_limited_reading, theater_ratio, 1937, 0.1).
narrative_ontology:measurement(comm_tr_t1960, commerce_clause_text__substantial_effects_limited_reading, theater_ratio, 1960, 0.1).
narrative_ontology:measurement(comm_tr_t1980, commerce_clause_text__substantial_effects_limited_reading, theater_ratio, 1980, 0.1).
narrative_ontology:measurement(comm_tr_t2000, commerce_clause_text__substantial_effects_limited_reading, theater_ratio, 2000, 0.1).
narrative_ontology:measurement(comm_tr_t2024, commerce_clause_text__substantial_effects_limited_reading, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(comm_be_t1937, commerce_clause_text__substantial_effects_limited_reading, base_extractiveness, 1937, 0.5).
narrative_ontology:measurement(comm_be_t1960, commerce_clause_text__substantial_effects_limited_reading, base_extractiveness, 1960, 0.55).
narrative_ontology:measurement(comm_be_t1980, commerce_clause_text__substantial_effects_limited_reading, base_extractiveness, 1980, 0.6).
narrative_ontology:measurement(comm_be_t2000, commerce_clause_text__substantial_effects_limited_reading, base_extractiveness, 2000, 0.65).
narrative_ontology:measurement(comm_be_t2024, commerce_clause_text__substantial_effects_limited_reading, base_extractiveness, 2024, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(comm_su_t1937, commerce_clause_text__substantial_effects_limited_reading, suppression_requirement, 1937, 0.6).
narrative_ontology:measurement(comm_su_t1960, commerce_clause_text__substantial_effects_limited_reading, suppression_requirement, 1960, 0.65).
narrative_ontology:measurement(comm_su_t1980, commerce_clause_text__substantial_effects_limited_reading, suppression_requirement, 1980, 0.7).
narrative_ontology:measurement(comm_su_t2000, commerce_clause_text__substantial_effects_limited_reading, suppression_requirement, 2000, 0.75).
narrative_ontology:measurement(comm_su_t2024, commerce_clause_text__substantial_effects_limited_reading, suppression_requirement, 2024, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(commerce_clause_text__substantial_effects_limited_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(commerce_clause_text__substantial_effects_limited_reading, commerce_clause_text__expansive_federal_reading).
narrative_ontology:affects_constraint(commerce_clause_text__substantial_effects_limited_reading, commerce_clause_text__originalist_narrow_reading).
narrative_ontology:affects_constraint(commerce_clause_text__substantial_effects_limited_reading, federal_police_power_limits).
narrative_ontology:affects_constraint(commerce_clause_text__substantial_effects_limited_reading, state_sovereignty_doctrine).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
