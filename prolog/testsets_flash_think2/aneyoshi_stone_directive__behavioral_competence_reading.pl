% ============================================================================
% CONSTRAINT STORY: aneyoshi_stone_directive__behavioral_competence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_aneyoshi_stone_directive__behavioral_competence_reading, []).

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
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    domain_priors:emerges_naturally/1,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
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
 *   constraint_id: aneyoshi_stone_directive__behavioral_competence_reading
 *   human_readable: Aneyoshi Stone Directive: Behavioral Competence Reading
 *   domain: Disaster Anthropology / Institutional Memory / Land-Use Governance
 *
 * SUMMARY:
 *   This constraint represents the 'behavioral competence' reading of the
 *   Aneyoshi stone directive, which views the stone markers as a binding
 *   land-use constraint based on intergenerational wisdom and the immutable
 *   physical threat of tsunamis. This reading asserts the directive's
 *   continued efficacy and relevance, seeing it as a successful, low-cost,
 *   and highly effective disaster mitigation strategy. The metrics reflect
 *   this reading's perspective: very low extraction and suppression,
 *   consistent with a natural law that is respected rather than coercively
 *   enforced, and a high accessibility collapse due to the catastrophic
 *   consequences of ignoring it.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(aneyoshi_stone_directive__behavioral_competence_reading, 0.05).
domain_priors:suppression_score(aneyoshi_stone_directive__behavioral_competence_reading, 0.1).
domain_priors:theater_ratio(aneyoshi_stone_directive__behavioral_competence_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(aneyoshi_stone_directive__behavioral_competence_reading, extractiveness, 0.05).
narrative_ontology:constraint_metric(aneyoshi_stone_directive__behavioral_competence_reading, suppression_requirement, 0.1).
narrative_ontology:constraint_metric(aneyoshi_stone_directive__behavioral_competence_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(aneyoshi_stone_directive__behavioral_competence_reading, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(aneyoshi_stone_directive__behavioral_competence_reading, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(aneyoshi_stone_directive__behavioral_competence_reading, mountain).
narrative_ontology:human_readable(aneyoshi_stone_directive__behavioral_competence_reading, "Aneyoshi Stone Directive: Behavioral Competence Reading").
narrative_ontology:topic_domain(aneyoshi_stone_directive__behavioral_competence_reading, "Disaster Anthropology / Institutional Memory / Land-Use Governance").

domain_priors:emerges_naturally(aneyoshi_stone_directive__behavioral_competence_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(aneyoshi_stone_directive__behavioral_competence_reading, '417c1764-c58d-44db-a956-71fd40fa3ebe').
narrative_ontology:cs_kernel_codification('417c1764-c58d-44db-a956-71fd40fa3ebe', fixed_text).
narrative_ontology:cs_authority_grounding('417c1764-c58d-44db-a956-71fd40fa3ebe', lineage).
narrative_ontology:cs_reading_relation('417c1764-c58d-44db-a956-71fd40fa3ebe', aneyoshi_stone_directive__commemorative_husk_reading, forecloses).
narrative_ontology:cs_axiom('417c1764-c58d-44db-a956-71fd40fa3ebe', foundational, past_disaster_experience_is_binding_precedent).
narrative_ontology:cs_axiom_status(past_disaster_experience_is_binding_precedent, holdable).
narrative_ontology:cs_axiom_grounding('417c1764-c58d-44db-a956-71fd40fa3ebe', past_disaster_experience_is_binding_precedent, empirically_contingent).
narrative_ontology:cs_axiom('417c1764-c58d-44db-a956-71fd40fa3ebe', foundational, geographical_vulnerability_is_immutable).
narrative_ontology:cs_axiom_status(geographical_vulnerability_is_immutable, holdable).
narrative_ontology:cs_axiom_grounding('417c1764-c58d-44db-a956-71fd40fa3ebe', geographical_vulnerability_is_immutable, empirically_contingent).
narrative_ontology:cs_reference_frame('417c1764-c58d-44db-a956-71fd40fa3ebe', intergenerational_tsunami_safety_protocol).
narrative_ontology:cs_drift_state('417c1764-c58d-44db-a956-71fd40fa3ebe', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('417c1764-c58d-44db-a956-71fd40fa3ebe', '').
narrative_ontology:cs_kernel_id(aneyoshi_stone_directive__behavioral_competence_reading, aneyoshi_stone_directive).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(aneyoshi_stone_directive__behavioral_competence_reading, local_community_residents).
narrative_ontology:constraint_beneficiary(aneyoshi_stone_directive__behavioral_competence_reading, local_government).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(aneyoshi_stone_directive__behavioral_competence_reading, local_community_residents).
narrative_ontology:constraint_vindicates(aneyoshi_stone_directive__behavioral_competence_reading, disaster_preparedness_efficacy).
narrative_ontology:constraint_vindicates(aneyoshi_stone_directive__behavioral_competence_reading, intergenerational_wisdom).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Adheres to the stone directive by not building below the marked line, thereby benefiting from protection against tsunamis. Pays the cost of restricted land use and potential economic development in vulnerable areas. Their adherence is largely voluntary, driven by historical memory and community norms.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_directive__behavioral_competence_reading, local_community_residents, beneficiary,
    moderate, generational, constrained, local).
narrative_ontology:stakeholder_secondary_role(aneyoshi_stone_directive__behavioral_competence_reading, local_community_residents, payer).

% Administers land-use regulations that align with the stone directive, ensuring community safety and reducing future disaster response burdens. Benefits from a resilient community and reduced infrastructure damage. Their role is to uphold the directive's intent through policy.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_directive__behavioral_competence_reading, local_government, agenda_setter,
    organized, generational, constrained, local).
narrative_ontology:stakeholder_secondary_role(aneyoshi_stone_directive__behavioral_competence_reading, local_government, beneficiary).

% Study the efficacy of traditional disaster mitigation strategies like the Aneyoshi stone directive. They provide scientific corroboration for the underlying tsunami risk and the directive's protective function, viewing it as a successful example of intergenerational risk management.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_directive__behavioral_competence_reading, disaster_risk_scientists, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(aneyoshi_stone_directive__behavioral_competence_reading, diffuse).
narrative_ontology:fixing_cost_class(aneyoshi_stone_directive__behavioral_competence_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates land-use decisions within the community to prevent construction in areas vulnerable to tsunamis, ensuring collective safety and preserving historical memory of disaster risk.
% TRANSFER_FUNCTION: Transfers the opportunity cost of restricted land development from individual residents to the collective benefit of enhanced community safety and resilience against natural disasters.
% ABSENT_VOICES: Future generations who might forget the original disaster's lessons and seek to develop restricted land for economic gain, or external developers prioritizing profit over long-term safety.
% DISAPPEARANCE_RATIONALE: If the directive vanished, the historical memory of tsunami risk might fade over time, leading to development in vulnerable areas. This would inevitably result in catastrophic loss of life and property in future tsunami events, fundamentally reorganizing the community's relationship with its environment.
% FOUNDING_PROBLEM: Catastrophic loss of life and property from historical tsunamis (e.g., 1896 Meiji Sanriku and 1933 Showa Sanriku tsunamis) due to human settlement in low-lying, vulnerable coastal areas.
% FOUNDING_PROBLEM_CORROBORATION: Historical records of past tsunamis, geological evidence of inundation zones, and contemporary disaster risk scientists corroborate the ongoing and immutable tsunami threat to the region. The 2011 Tohoku tsunami further validated the directive's efficacy, as areas behind the stone markers were largely spared.
narrative_ontology:disappearance_verdict(aneyoshi_stone_directive__behavioral_competence_reading, world_rearranges).
narrative_ontology:founding_problem_status(aneyoshi_stone_directive__behavioral_competence_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(aneyoshi_stone_directive__behavioral_competence_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(aneyoshi_stone_directive__behavioral_competence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(aneyoshi_stone_directive__behavioral_competence_reading, 0.05, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(aneyoshi_stone_directive__behavioral_competence_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(aneyoshi_stone_directive__behavioral_competence_reading, ExtMetricName, E),
    domain_priors:suppression_score(aneyoshi_stone_directive__behavioral_competence_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(aneyoshi_stone_directive__behavioral_competence_reading),
    narrative_ontology:constraint_metric(aneyoshi_stone_directive__behavioral_competence_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(aneyoshi_stone_directive__behavioral_competence_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(aneyoshi_stone_directive__behavioral_competence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The low extractiveness (0.05) reflects the minimal cost of compliance relative to the immense benefit of safety. Suppression (0.1) is low because adherence is primarily driven by historical memory and community consensus, not active coercion. The theater ratio (0.05) is negligible, as the directive's function is genuinely about safety. Accessibility collapse (0.9) is high because building below the stone line is understood to lead to catastrophic outcomes, effectively collapsing safe alternatives. Resistance (0.1) is low due to the community's deep-seated respect for the directive, reinforced by historical events like the 2011 tsunami.
 *
 * PERSPECTIVAL GAP:
 *   This reading emphasizes the directive's enduring practical value. The 'commemorative husk' reading, by contrast, would see the directive as having lost its behavioral force, becoming merely a memorial. The engine's classification will highlight how these different interpretations of the same physical marker lead to vastly different structural assessments.
 *
 * DIRECTIONALITY LOGIC:
 *   Local community residents and local government are beneficiaries of the safety provided by the directive, while also bearing the minor costs of restricted land use. Disaster risk scientists act as analytical observers, corroborating the directive's efficacy. No party is a 'victim' in an extractive sense, as the constraint's purpose is collective survival.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_constructed_ambiguity,
    'Is the Aneyoshi stone directive a genuine natural law (an immutable physical boundary dictated by tsunami risk) or a constructed constraint (a human decision to respect that boundary)?',
    'Analysis of community adherence patterns over time in the absence of active enforcement: if adherence persists across generations without explicit legal backing, it leans towards internalized natural law; if it requires continuous reinforcement, it''s more constructed.',
    'If primarily a natural law, its mountain classification is robust. If primarily constructed, the presence of beneficiaries (community safety) would trigger a false summit reclassification, likely to a Rope or Tangled Rope, indicating a coordination function that benefits identifiable parties.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_ambiguity, conceptual, 'Ambiguity between physical reality and human interpretation of the directive.').

omega_variable(
    behavioral_competence_vs_commemorative_husk,
    'Is the Aneyoshi stone directive still a binding behavioral constraint, or has it atrophied into a mere commemorative artifact?',
    'Empirical observation of land-use decisions and development patterns in the designated areas, particularly during periods of economic pressure or generational turnover. The 2011 Tohoku tsunami provided a natural experiment, validating its behavioral force.',
    'If it remains a binding constraint (this reading), its classification as a Mountain is appropriate. If it has become a commemorative husk (sibling reading), its classification would shift towards a Piton, reflecting a constraint maintained by inertia or theatricality rather than active function.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(behavioral_competence_vs_commemorative_husk, empirical, 'The core contest over the directive''s active status.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the community''s adherence to the directive primarily due to internalized respect for historical wisdom and shared risk, or due to structural land-use regulations and social pressure?',
    'Sociological studies on community norms, interviews with residents, and analysis of local government enforcement records. If adherence is high even where formal enforcement is weak, it suggests internalized mechanisms.',
    'If internalized, the effective suppression is higher than structural measures suggest, as the constraint operates through self-regulation. If structural, the constraint''s persistence is more dependent on formal governance mechanisms.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for community adherence.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(aneyoshi_stone_directive__behavioral_competence_reading, 1933, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(aney_tr_t1933, aneyoshi_stone_directive__behavioral_competence_reading, theater_ratio, 1933, 0.05).
narrative_ontology:measurement(aney_tr_t1960, aneyoshi_stone_directive__behavioral_competence_reading, theater_ratio, 1960, 0.05).
narrative_ontology:measurement(aney_tr_t1985, aneyoshi_stone_directive__behavioral_competence_reading, theater_ratio, 1985, 0.05).
narrative_ontology:measurement(aney_tr_t2011, aneyoshi_stone_directive__behavioral_competence_reading, theater_ratio, 2011, 0.05).
narrative_ontology:measurement(aney_tr_t2026, aneyoshi_stone_directive__behavioral_competence_reading, theater_ratio, 2026, 0.05).

% Extraction over time
narrative_ontology:measurement(aney_be_t1933, aneyoshi_stone_directive__behavioral_competence_reading, base_extractiveness, 1933, 0.05).
narrative_ontology:measurement(aney_be_t1960, aneyoshi_stone_directive__behavioral_competence_reading, base_extractiveness, 1960, 0.05).
narrative_ontology:measurement(aney_be_t1985, aneyoshi_stone_directive__behavioral_competence_reading, base_extractiveness, 1985, 0.05).
narrative_ontology:measurement(aney_be_t2011, aneyoshi_stone_directive__behavioral_competence_reading, base_extractiveness, 2011, 0.05).
narrative_ontology:measurement(aney_be_t2026, aneyoshi_stone_directive__behavioral_competence_reading, base_extractiveness, 2026, 0.05).

% Suppression requirement over time
narrative_ontology:measurement(aney_su_t1933, aneyoshi_stone_directive__behavioral_competence_reading, suppression_requirement, 1933, 0.1).
narrative_ontology:measurement(aney_su_t1960, aneyoshi_stone_directive__behavioral_competence_reading, suppression_requirement, 1960, 0.1).
narrative_ontology:measurement(aney_su_t1985, aneyoshi_stone_directive__behavioral_competence_reading, suppression_requirement, 1985, 0.1).
narrative_ontology:measurement(aney_su_t2011, aneyoshi_stone_directive__behavioral_competence_reading, suppression_requirement, 2011, 0.1).
narrative_ontology:measurement(aney_su_t2026, aneyoshi_stone_directive__behavioral_competence_reading, suppression_requirement, 2026, 0.1).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(aneyoshi_stone_directive__behavioral_competence_reading, enforcement_mechanism).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'aneyoshi_stone_directive' kernel. The 'behavioral_competence_reading' emphasizes its active role in disaster prevention, while the 'commemorative_husk_reading' (a sibling constraint) views it as a historical artifact.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
