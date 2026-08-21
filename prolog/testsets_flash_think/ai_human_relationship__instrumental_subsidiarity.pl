% ============================================================================
% CONSTRAINT STORY: ai_human_relationship__instrumental_subsidiarity
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ai_human_relationship__instrumental_subsidiarity, []).

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
 *   constraint_id: ai_human_relationship__instrumental_subsidiarity
 *   human_readable: AI as Neutral Tool: Instrumental Subsidiarity
 *   domain: ethics/technology/political_theology
 *
 * SUMMARY:
 *   This constraint story models the 'instrumental subsidiarity' reading of
 *   the AI-human relationship, rooted in Catholic Social Teaching and
 *   technology ethics. It posits AI as a morally neutral tool whose ethical
 *   valence is determined by its use and governance. The constraint
 *   emphasizes that technology must serve human ends, with human dignity
 *   protected through robust legal and ethical frameworks, and subsidiarity
 *   acting as a procedural safeguard. This reading contrasts with views that
 *   see AI as inherently good/bad or as a purely optimizing force.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_human_relationship__instrumental_subsidiarity, 0.35).
domain_priors:suppression_score(ai_human_relationship__instrumental_subsidiarity, 0.65).
domain_priors:theater_ratio(ai_human_relationship__instrumental_subsidiarity, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_human_relationship__instrumental_subsidiarity, extractiveness, 0.35).
narrative_ontology:constraint_metric(ai_human_relationship__instrumental_subsidiarity, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(ai_human_relationship__instrumental_subsidiarity, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ai_human_relationship__instrumental_subsidiarity, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(ai_human_relationship__instrumental_subsidiarity, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_human_relationship__instrumental_subsidiarity, rope).
narrative_ontology:human_readable(ai_human_relationship__instrumental_subsidiarity, "AI as Neutral Tool: Instrumental Subsidiarity").
narrative_ontology:topic_domain(ai_human_relationship__instrumental_subsidiarity, "ethics/technology/political_theology").

domain_priors:requires_active_enforcement(ai_human_relationship__instrumental_subsidiarity).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_human_relationship__instrumental_subsidiarity, '735437a9-f5a0-410e-8362-bb392ef03add').
narrative_ontology:cs_kernel_codification('735437a9-f5a0-410e-8362-bb392ef03add', formalized).
narrative_ontology:cs_authority_grounding('735437a9-f5a0-410e-8362-bb392ef03add', lineage).
narrative_ontology:cs_interpretation_layer_present('735437a9-f5a0-410e-8362-bb392ef03add').
narrative_ontology:cs_reading_relation('735437a9-f5a0-410e-8362-bb392ef03add', ai_human_relationship__technocratic_optimization, coexists_with).
narrative_ontology:cs_reading_relation('735437a9-f5a0-410e-8362-bb392ef03add', ai_human_relationship__incarnational_humanism, coexists_with).
narrative_ontology:cs_axiom('735437a9-f5a0-410e-8362-bb392ef03add', foundational, technology_is_morally_neutral).
narrative_ontology:cs_axiom_status(technology_is_morally_neutral, holdable).
narrative_ontology:cs_axiom_grounding('735437a9-f5a0-410e-8362-bb392ef03add', technology_is_morally_neutral, deontological).
narrative_ontology:cs_axiom('735437a9-f5a0-410e-8362-bb392ef03add', foundational, human_dignity_is_protected_by_law_and_ethics).
narrative_ontology:cs_axiom_status(human_dignity_is_protected_by_law_and_ethics, holdable).
narrative_ontology:cs_axiom_grounding('735437a9-f5a0-410e-8362-bb392ef03add', human_dignity_is_protected_by_law_and_ethics, deontological).
narrative_ontology:cs_reference_frame('735437a9-f5a0-410e-8362-bb392ef03add', human_dignity_as_ethical_anchor).
narrative_ontology:cs_drift_state('735437a9-f5a0-410e-8362-bb392ef03add', contemporary_ai_acceleration, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('735437a9-f5a0-410e-8362-bb392ef03add', '').
narrative_ontology:cs_kernel_id(ai_human_relationship__instrumental_subsidiarity, ai_human_relationship).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_human_relationship__instrumental_subsidiarity, global_human_community).
narrative_ontology:constraint_beneficiary(ai_human_relationship__instrumental_subsidiarity, ethical_regulators).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(ai_human_relationship__instrumental_subsidiarity, ai_developers).
narrative_ontology:constraint_vindicates(ai_human_relationship__instrumental_subsidiarity, technology_as_instrument).
narrative_ontology:constraint_vindicates(ai_human_relationship__instrumental_subsidiarity, human_dignity_as_ethical_anchor).
narrative_ontology:constraint_vindicates(ai_human_relationship__instrumental_subsidiarity, subsidiarity_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Develops and enforces ethical guidelines and legal frameworks for AI, aiming to ensure technology serves human flourishing. Bears the cost of policy development and oversight.
narrative_ontology:constraint_stakeholder(ai_human_relationship__instrumental_subsidiarity, ethical_regulators, agenda_setter,
    institutional, generational, analytical, global).

% Must comply with ethical and legal regulations, incurring costs for responsible design, transparency, and impact assessments. Benefits from public trust and market stability that regulation can provide.
narrative_ontology:constraint_stakeholder(ai_human_relationship__instrumental_subsidiarity, ai_developers, payer,
    powerful, biographical, constrained, global).

% Benefits from AI development being guided by ethical principles and legal safeguards, protecting human dignity and promoting the common good. Bears indirect costs of regulatory overhead.
narrative_ontology:constraint_stakeholder(ai_human_relationship__instrumental_subsidiarity, global_human_community, beneficiary,
    organized, generational, constrained, universal).

% Are prevented from deploying AI systems that violate ethical norms or legal standards. Their profit motives are suppressed by the regulatory framework.
narrative_ontology:constraint_stakeholder(ai_human_relationship__instrumental_subsidiarity, unethical_ai_actors, excluded,
    powerless, immediate, trapped, global).

% Analyze the ethical implications of AI from a theological and philosophical perspective, contributing to the development and critique of regulatory frameworks. They do not directly benefit or pay.
narrative_ontology:constraint_stakeholder(ai_human_relationship__instrumental_subsidiarity, political_theologians, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the global development and deployment of AI by establishing shared ethical principles and legal boundaries, ensuring technology is used to serve human ends and respects subsidiarity.
% TRANSFER_FUNCTION: Transfers the burden of ethical responsibility and compliance costs to AI developers and deployers, in exchange for societal trust and the prevention of harm to the global human community.
% ABSENT_VOICES: Those who advocate for purely market-driven or technologically deterministic approaches to AI development, rejecting ethical or legal constraints, are excluded from the conversation. Also, those who believe AI is inherently good or evil, rather than a neutral tool, would object to this framing.
% DISAPPEARANCE_RATIONALE: If this ethical and legal framework vanished, AI development would likely accelerate without sufficient safeguards, leading to increased risks of harm, exploitation, and erosion of human dignity, forcing society to reactively implement new, potentially more coercive, constraints.
% FOUNDING_PROBLEM: The rapid advancement of AI technology presented significant risks to human dignity, autonomy, and social justice, necessitating a framework to guide its development towards human flourishing and prevent its misuse.
% FOUNDING_PROBLEM_CORROBORATION: International bodies (e.g., UNESCO, EU), academic ethicists, and civil society organizations consistently corroborate the ongoing need for ethical governance of AI, citing emerging risks and the potential for societal disruption if left unregulated. This corroboration comes from outside the direct beneficiaries of the regulatory bodies themselves.
narrative_ontology:disappearance_verdict(ai_human_relationship__instrumental_subsidiarity, world_rearranges).
narrative_ontology:founding_problem_status(ai_human_relationship__instrumental_subsidiarity, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ai_human_relationship__instrumental_subsidiarity, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(ai_human_relationship__instrumental_subsidiarity, 'none', 1).
narrative_ontology:epsilon_provenance(ai_human_relationship__instrumental_subsidiarity, 0.35, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ai_human_relationship__instrumental_subsidiarity_tests).
:- end_tests(ai_human_relationship__instrumental_subsidiarity_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The `claimed_type` is 'rope' because this reading frames the constraint as a genuine coordination mechanism designed for the common good, with net benefits for society. `Extractiveness` is moderate (0.35) reflecting the necessary costs of regulation and compliance, which are seen as legitimate overhead for ethical governance. `Suppression` is moderate-high (0.65) as the framework actively restricts unethical or harmful AI applications. `Theater_ratio` is moderate (0.30), acknowledging that while genuine ethical work occurs, some aspects of 'ethical AI' discourse can be performative without deep structural change. `Accessibility_collapse` is moderate (0.55) as it limits certain AI development paths, and `resistance` is moderate (0.45) from actors who prefer less regulation.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the `global_human_community` and `ethical_regulators`, this framework is a necessary and beneficial 'rope' for guiding AI. From the perspective of `AI developers`, it is a 'tangled rope' or even a 'snare' due to compliance costs and limitations on innovation, even if they acknowledge some benefits. The engine will compute these per-seat classifications.
 *
 * DIRECTIONALITY LOGIC:
 *   The `global_human_community` and `ethical_regulators` are beneficiaries, as the framework aims to protect and guide AI for human flourishing. `AI developers` are payers, bearing the costs of compliance. `Unethical AI actors` are excluded, as their activities are directly suppressed by the constraint. The constraint's structure is intended to create a net positive sum for humanity by preventing negative externalities.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_appropriateness,
    'Is the ''instrumental subsidiarity'' reading of AI''s relationship to humanity the most appropriate, or do sibling readings offer better frameworks for ethical governance?',
    'Long-term societal outcomes of AI development under different dominant ethical frameworks, assessed by interdisciplinary panels of ethicists, sociologists, and technologists.',
    'If a sibling reading (e.g., ''incarnational_humanism'') proves more robust in practice, this reading''s classification might shift to reflect its limitations or its role as a component within a broader framework.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_appropriateness, conceptual, 'Assesses the overall fitness of this ethical framework compared to alternatives.').

omega_variable(
    ai_moral_neutrality_ambiguity,
    'Is AI truly a morally neutral tool, or does its inherent structure, autonomy, or emergent properties carry intrinsic moral valence that challenges the ''instrumental'' premise?',
    'Philosophical and theological debate, coupled with empirical observation of AI''s societal impacts, particularly in areas where its agency or emergent behavior challenges human control or understanding.',
    'If AI is found to possess intrinsic moral valence, the foundational axiom of this reading would be challenged, potentially shifting the constraint''s classification towards a ''tangled_rope'' or ''snare'' if the ''neutrality'' claim serves to obscure inherent harms.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ai_moral_neutrality_ambiguity, conceptual, 'Examines the foundational premise of AI''s moral neutrality.').

omega_variable(
    regulatory_capacity_effectiveness,
    'Can law and ethics truly govern AI to serve human ends, or will technological advancement outpace regulatory capacity, rendering frameworks like this largely performative?',
    'Empirical analysis of regulatory lag, enforcement effectiveness, and the actual impact of ethical guidelines on AI development and deployment over time, particularly in rapidly evolving sub-fields.',
    'If regulation consistently lags or proves ineffective, the `theater_ratio` would increase, and the constraint might reclassify towards a ''piton'' (if function atrophies) or ''snare'' (if performativity masks extraction).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_capacity_effectiveness, empirical, 'Assesses the practical effectiveness of ethical and legal governance of AI.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_human_relationship__instrumental_subsidiarity, 2020, 2050).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ai_h_tr_t2020, ai_human_relationship__instrumental_subsidiarity, theater_ratio, 2020, 0.25).
narrative_ontology:measurement(ai_h_tr_t2025, ai_human_relationship__instrumental_subsidiarity, theater_ratio, 2025, 0.28).
narrative_ontology:measurement(ai_h_tr_t2030, ai_human_relationship__instrumental_subsidiarity, theater_ratio, 2030, 0.3).
narrative_ontology:measurement(ai_h_tr_t2035, ai_human_relationship__instrumental_subsidiarity, theater_ratio, 2035, 0.32).
narrative_ontology:measurement(ai_h_tr_t2040, ai_human_relationship__instrumental_subsidiarity, theater_ratio, 2040, 0.31).
narrative_ontology:measurement(ai_h_tr_t2045, ai_human_relationship__instrumental_subsidiarity, theater_ratio, 2045, 0.3).
narrative_ontology:measurement(ai_h_tr_t2050, ai_human_relationship__instrumental_subsidiarity, theater_ratio, 2050, 0.3).

% Extraction over time
narrative_ontology:measurement(ai_h_be_t2020, ai_human_relationship__instrumental_subsidiarity, base_extractiveness, 2020, 0.3).
narrative_ontology:measurement(ai_h_be_t2025, ai_human_relationship__instrumental_subsidiarity, base_extractiveness, 2025, 0.32).
narrative_ontology:measurement(ai_h_be_t2030, ai_human_relationship__instrumental_subsidiarity, base_extractiveness, 2030, 0.35).
narrative_ontology:measurement(ai_h_be_t2035, ai_human_relationship__instrumental_subsidiarity, base_extractiveness, 2035, 0.37).
narrative_ontology:measurement(ai_h_be_t2040, ai_human_relationship__instrumental_subsidiarity, base_extractiveness, 2040, 0.38).
narrative_ontology:measurement(ai_h_be_t2045, ai_human_relationship__instrumental_subsidiarity, base_extractiveness, 2045, 0.37).
narrative_ontology:measurement(ai_h_be_t2050, ai_human_relationship__instrumental_subsidiarity, base_extractiveness, 2050, 0.35).

% Suppression requirement over time
narrative_ontology:measurement(ai_h_su_t2020, ai_human_relationship__instrumental_subsidiarity, suppression_requirement, 2020, 0.55).
narrative_ontology:measurement(ai_h_su_t2025, ai_human_relationship__instrumental_subsidiarity, suppression_requirement, 2025, 0.6).
narrative_ontology:measurement(ai_h_su_t2030, ai_human_relationship__instrumental_subsidiarity, suppression_requirement, 2030, 0.65).
narrative_ontology:measurement(ai_h_su_t2035, ai_human_relationship__instrumental_subsidiarity, suppression_requirement, 2035, 0.68).
narrative_ontology:measurement(ai_h_su_t2040, ai_human_relationship__instrumental_subsidiarity, suppression_requirement, 2040, 0.67).
narrative_ontology:measurement(ai_h_su_t2045, ai_human_relationship__instrumental_subsidiarity, suppression_requirement, 2045, 0.66).
narrative_ontology:measurement(ai_h_su_t2050, ai_human_relationship__instrumental_subsidiarity, suppression_requirement, 2050, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_human_relationship__instrumental_subsidiarity, enforcement_mechanism).
narrative_ontology:affects_constraint(ai_human_relationship__instrumental_subsidiarity, ai_development_regulation).
narrative_ontology:affects_constraint(ai_human_relationship__instrumental_subsidiarity, data_privacy_laws).
narrative_ontology:affects_constraint(ai_human_relationship__instrumental_subsidiarity, digital_ethics_education).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
