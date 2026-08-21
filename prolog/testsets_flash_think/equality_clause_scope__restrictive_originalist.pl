% ============================================================================
% CONSTRAINT STORY: equality_clause_scope__restrictive_originalist
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_equality_clause_scope__restrictive_originalist, []).

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
 *   constraint_id: equality_clause_scope__restrictive_originalist
 *   human_readable: Restrictive Originalist Reading of Equality Clause Scope
 *   domain: constitutional_law/political_philosophy/civil_rights_history
 *
 * SUMMARY:
 *   This constraint represents the restrictive originalist reading of the
 *   equality clause (e.g., in the US Constitution), which holds that equality
 *   applies primarily to propertied white males as political actors within
 *   the 18th-century social contract framework. This interpretation limits
 *   the scope of rights and political participation to those explicitly or
 *   implicitly included at the time of the founding, requiring formal
 *   constitutional amendments for any expansion of rights to other groups. It
 *   is one reading of the broader 'equality_clause_scope' kernel.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(equality_clause_scope__restrictive_originalist, 0.75).
domain_priors:suppression_score(equality_clause_scope__restrictive_originalist, 0.7).
domain_priors:theater_ratio(equality_clause_scope__restrictive_originalist, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(equality_clause_scope__restrictive_originalist, extractiveness, 0.75).
narrative_ontology:constraint_metric(equality_clause_scope__restrictive_originalist, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(equality_clause_scope__restrictive_originalist, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(equality_clause_scope__restrictive_originalist, accessibility_collapse, 0.85).
narrative_ontology:constraint_metric(equality_clause_scope__restrictive_originalist, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(equality_clause_scope__restrictive_originalist, tangled_rope).
narrative_ontology:human_readable(equality_clause_scope__restrictive_originalist, "Restrictive Originalist Reading of Equality Clause Scope").
narrative_ontology:topic_domain(equality_clause_scope__restrictive_originalist, "constitutional_law/political_philosophy/civil_rights_history").

domain_priors:requires_active_enforcement(equality_clause_scope__restrictive_originalist).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(equality_clause_scope__restrictive_originalist, '2bd4fa52-3259-40f9-a4e3-b0a6970c354d').
narrative_ontology:cs_kernel_codification('2bd4fa52-3259-40f9-a4e3-b0a6970c354d', fixed_text).
narrative_ontology:cs_authority_grounding('2bd4fa52-3259-40f9-a4e3-b0a6970c354d', lineage).
narrative_ontology:cs_interpretation_layer_present('2bd4fa52-3259-40f9-a4e3-b0a6970c354d').
narrative_ontology:cs_reading_relation('2bd4fa52-3259-40f9-a4e3-b0a6970c354d', equality_clause_scope__expansive_universalist, forecloses).
narrative_ontology:cs_reading_relation('2bd4fa52-3259-40f9-a4e3-b0a6970c354d', equality_clause_scope__progressive_textualist, coexists_with).
narrative_ontology:cs_axiom('2bd4fa52-3259-40f9-a4e3-b0a6970c354d', foundational, original_intent_supremacy).
narrative_ontology:cs_axiom_status(original_intent_supremacy, holdable).
narrative_ontology:cs_axiom_grounding('2bd4fa52-3259-40f9-a4e3-b0a6970c354d', original_intent_supremacy, conventional).
narrative_ontology:cs_axiom('2bd4fa52-3259-40f9-a4e3-b0a6970c354d', secondary, enumerated_rights_only).
narrative_ontology:cs_axiom_status(enumerated_rights_only, holdable).
narrative_ontology:cs_axiom_grounding('2bd4fa52-3259-40f9-a4e3-b0a6970c354d', enumerated_rights_only, conventional).
narrative_ontology:cs_reference_frame('2bd4fa52-3259-40f9-a4e3-b0a6970c354d', founding_era_political_compact).
narrative_ontology:cs_drift_state('2bd4fa52-3259-40f9-a4e3-b0a6970c354d', contemporary_challenges, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('2bd4fa52-3259-40f9-a4e3-b0a6970c354d', '').
narrative_ontology:cs_kernel_id(equality_clause_scope__restrictive_originalist, equality_clause_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(equality_clause_scope__restrictive_originalist, propertied_white_males).
narrative_ontology:constraint_victim(equality_clause_scope__restrictive_originalist, non_propertied_males).
narrative_ontology:constraint_victim(equality_clause_scope__restrictive_originalist, women).
narrative_ontology:constraint_victim(equality_clause_scope__restrictive_originalist, racial_minorities).
narrative_ontology:constraint_victim(equality_clause_scope__restrictive_originalist, indigenous_peoples).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(equality_clause_scope__restrictive_originalist, expansive_universalist_advocates).
narrative_ontology:constraint_vindicates(equality_clause_scope__restrictive_originalist, original_intent_doctrine).
narrative_ontology:constraint_vindicates(equality_clause_scope__restrictive_originalist, judicial_restraint).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Historically defined as the primary political actors and beneficiaries of the equality clause, holding full rights and political power. They set the terms of the social contract.
narrative_ontology:constraint_stakeholder(equality_clause_scope__restrictive_originalist, propertied_white_males, beneficiary,
    powerful, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(equality_clause_scope__restrictive_originalist, propertied_white_males, agenda_setter).

% Excluded from the full scope of equality rights due to property qualifications, bearing the cost of limited political participation and social status.
narrative_ontology:constraint_stakeholder(equality_clause_scope__restrictive_originalist, non_propertied_males, payer,
    powerless, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(equality_clause_scope__restrictive_originalist, non_propertied_males, excluded).

% Systematically excluded from political and many civil rights, bearing the cost of legal and social subordination. Their path to inclusion required significant struggle and constitutional amendment.
narrative_ontology:constraint_stakeholder(equality_clause_scope__restrictive_originalist, women, payer,
    powerless, generational, trapped, national).
narrative_ontology:stakeholder_secondary_role(equality_clause_scope__restrictive_originalist, women, excluded).

% Subject to slavery, segregation, and systemic discrimination, bearing the most severe costs of exclusion from equality. Their struggle for rights was protracted and violent.
narrative_ontology:constraint_stakeholder(equality_clause_scope__restrictive_originalist, racial_minorities, payer,
    powerless, generational, trapped, national).
narrative_ontology:stakeholder_secondary_role(equality_clause_scope__restrictive_originalist, racial_minorities, excluded).

% Often not considered citizens of the nation-state, subject to distinct legal frameworks and dispossession, bearing the costs of colonial expansion and denial of sovereignty.
narrative_ontology:constraint_stakeholder(equality_clause_scope__restrictive_originalist, indigenous_peoples, payer,
    powerless, civilizational, trapped, regional).
narrative_ontology:stakeholder_secondary_role(equality_clause_scope__restrictive_originalist, indigenous_peoples, excluded).

% Interpret and enforce the equality clause strictly according to the perceived original intent of the framers, resisting expansive applications and requiring formal amendments for new rights.
narrative_ontology:constraint_stakeholder(equality_clause_scope__restrictive_originalist, restrictive_originalist_judges, agenda_setter,
    institutional, civilizational, analytical, national).

% Actively challenge the restrictive interpretation, advocating for a universal application of equality based on inherent human dignity. They bear the costs of political and legal struggle.
narrative_ontology:constraint_stakeholder(equality_clause_scope__restrictive_originalist, expansive_universalist_advocates, payer,
    organized, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(equality_clause_scope__restrictive_originalist, expansive_universalist_advocates, observer).

% Analyze the text of the equality clause, arguing that while it contains an evolving principle, its expansion should primarily occur through democratic processes and amendments, not judicial reinterpretation.
narrative_ontology:constraint_stakeholder(equality_clause_scope__restrictive_originalist, progressive_textualist_scholars, observer,
    analytical, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To define the legitimate political actors and the scope of their rights within the original 18th-century social contract framework, ensuring a stable political order among the designated citizenry.
% TRANSFER_FUNCTION: Transfers political power, civil rights, and social status to propertied white males, while denying or severely limiting these for non-propertied males, women, and racial/indigenous minorities.
% ABSENT_VOICES: All groups historically excluded from the original social contract (non-propertied males, women, racial minorities, indigenous peoples) would object, arguing for a universal and inclusive application of equality. Their voices were systematically suppressed or ignored in the founding era.
% DISAPPEARANCE_RATIONALE: If this restrictive interpretation vanished overnight, the entire legal and political framework of rights and citizenship would need fundamental re-evaluation. The basis for political participation, property rights, and social status would be radically altered, leading to a complete reorganization of constitutional jurisprudence and social order.
% FOUNDING_PROBLEM: To establish a stable republican government and a defined body of citizens with equal rights among themselves, following the American Revolution.
% FOUNDING_PROBLEM_CORROBORATION: Historians and legal scholars (outside the originalist camp) acknowledge the historical context of limited suffrage and rights at the founding, but contest whether this historical reality should serve as a prescriptive limit on contemporary equality. Legislative debates and social movements throughout history corroborate the ongoing contestation of the founding problem's contemporary relevance.
narrative_ontology:disappearance_verdict(equality_clause_scope__restrictive_originalist, world_rearranges).
narrative_ontology:founding_problem_status(equality_clause_scope__restrictive_originalist, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(equality_clause_scope__restrictive_originalist, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(equality_clause_scope__restrictive_originalist, 'none', 1).
narrative_ontology:epsilon_provenance(equality_clause_scope__restrictive_originalist, 0.75, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(equality_clause_scope__restrictive_originalist_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(equality_clause_scope__restrictive_originalist, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(equality_clause_scope__restrictive_originalist_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness is high because this reading systematically denies rights and political power to large segments of the population, effectively extracting their potential participation and status. Suppression is also high, as this interpretation requires active judicial and political enforcement to resist challenges and maintain its narrow scope. The theater ratio is low because this interpretation is genuinely held and actively applied, not merely performative; its proponents sincerely believe in its structural validity. Accessibility collapse is high because, from this perspective, alternatives for those outside the original scope are fundamentally foreclosed without explicit constitutional change. Resistance is high due to ongoing challenges from civil rights movements and legal scholars.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the original beneficiaries and their judicial interpreters, this constraint is a legitimate and stable framework for political order. From the perspective of the excluded groups, it is a deeply extractive and suppressive mechanism that denies fundamental human rights. The engine's computation of per-seat classification will reflect this divergence, showing a 'tangled_rope' or 'snare' for the victims and a 'rope' or even 'mountain' for the beneficiaries/agenda-setters.
 *
 * DIRECTIONALITY LOGIC:
 *   Propertied white males are the clear beneficiaries and historical agenda-setters, as the constraint defines their privileged position. Non-propertied males, women, racial minorities, and indigenous peoples are the primary targets and victims, bearing the costs of exclusion and denied rights. Restrictive originalist judges act as agenda-setters, enforcing this interpretation. Advocates for broader equality bear the costs of challenging this constraint, while scholars observe and analyze its implications.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification as a 'tangled_rope' prevents mislabeling by acknowledging both its historical coordination function (for the original beneficiaries) and its asymmetric extraction from excluded groups. It avoids treating the historical exclusion as a 'natural law' (mountain) or a purely benign coordination (rope) by highlighting the active enforcement and identifiable victims, while also not reducing it to pure extraction (snare) without acknowledging its foundational role for a specific historical group.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    historical_context_prescriptive_or_descriptive,
    'Is the 18th-century historical context of the equality clause a prescriptive limit on its contemporary application, or merely a descriptive starting point for its evolution?',
    'Legal and philosophical consensus on constitutional interpretation, or a definitive constitutional amendment clarifying the role of original intent versus evolving societal values.',
    'If prescriptive, this reading''s narrow scope is reinforced. If descriptive, the constraint''s extractiveness and suppression are amplified, as its persistence relies on an outdated framework.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(historical_context_prescriptive_or_descriptive, conceptual, 'Ambiguity regarding the normative force of historical context in constitutional interpretation.').

omega_variable(
    original_intent_discoverability,
    'Can a singular, coherent ''original intent'' of the framers regarding equality be objectively discovered, or is it always a contemporary construction influenced by present-day values?',
    'Exhaustive historical and textual analysis yielding an uncontested, singular intent, or a demonstration that such an intent is inherently ambiguous or contradictory.',
    'If objectively discoverable, this reading gains epistemic authority. If constructed, its claims of ''naturalness'' or ''fidelity'' are weakened, potentially reclassifying it closer to a ''snare'' due to its reliance on an interpretive fiction.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(original_intent_discoverability, empirical, 'The epistemic status of ''original intent'' as a basis for legal interpretation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(equality_clause_scope__restrictive_originalist, 1787, 2023).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(equa_tr_t1787, equality_clause_scope__restrictive_originalist, theater_ratio, 1787, 0.1).
narrative_ontology:measurement(equa_tr_t1865, equality_clause_scope__restrictive_originalist, theater_ratio, 1865, 0.12).
narrative_ontology:measurement(equa_tr_t1920, equality_clause_scope__restrictive_originalist, theater_ratio, 1920, 0.14).
narrative_ontology:measurement(equa_tr_t1965, equality_clause_scope__restrictive_originalist, theater_ratio, 1965, 0.15).
narrative_ontology:measurement(equa_tr_t2023, equality_clause_scope__restrictive_originalist, theater_ratio, 2023, 0.15).

% Extraction over time
narrative_ontology:measurement(equa_be_t1787, equality_clause_scope__restrictive_originalist, base_extractiveness, 1787, 0.95).
narrative_ontology:measurement(equa_be_t1865, equality_clause_scope__restrictive_originalist, base_extractiveness, 1865, 0.88).
narrative_ontology:measurement(equa_be_t1920, equality_clause_scope__restrictive_originalist, base_extractiveness, 1920, 0.82).
narrative_ontology:measurement(equa_be_t1965, equality_clause_scope__restrictive_originalist, base_extractiveness, 1965, 0.78).
narrative_ontology:measurement(equa_be_t2023, equality_clause_scope__restrictive_originalist, base_extractiveness, 2023, 0.75).

% Suppression requirement over time
narrative_ontology:measurement(equa_su_t1787, equality_clause_scope__restrictive_originalist, suppression_requirement, 1787, 0.9).
narrative_ontology:measurement(equa_su_t1865, equality_clause_scope__restrictive_originalist, suppression_requirement, 1865, 0.85).
narrative_ontology:measurement(equa_su_t1920, equality_clause_scope__restrictive_originalist, suppression_requirement, 1920, 0.78).
narrative_ontology:measurement(equa_su_t1965, equality_clause_scope__restrictive_originalist, suppression_requirement, 1965, 0.72).
narrative_ontology:measurement(equa_su_t2023, equality_clause_scope__restrictive_originalist, suppression_requirement, 2023, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(equality_clause_scope__restrictive_originalist, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
