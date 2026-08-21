% ============================================================================
% CONSTRAINT STORY: article_9_war_renunciation__inherent_right_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_article_9_war_renunciation__inherent_right_reading, []).

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
 *   constraint_id: article_9_war_renunciation__inherent_right_reading
 *   human_readable: Article 9 War Renunciation (Inherent Self-Defense Reading)
 *   domain: constitutional_law/security_policy
 *
 * SUMMARY:
 *   This constraint represents the 'inherent right to self-defense' reading
 *   of Japan's Article 9, which renounces 'war' but is interpreted to permit
 *   a 'minimum necessary' defensive capacity. This reading allows for the
 *   existence of the Self-Defense Forces (SDF) for territorial defense,
 *   establishing a proportionality constraint on military capabilities. It is
 *   a contested interpretation, balancing the constitutional text with
 *   perceived security needs. The claimed type is 'tangled_rope' because it
 *   genuinely coordinates national security while extracting from those who
 *   prefer either absolute pacifism or full re-militarization.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(article_9_war_renunciation__inherent_right_reading, 0.35).
domain_priors:suppression_score(article_9_war_renunciation__inherent_right_reading, 0.45).
domain_priors:theater_ratio(article_9_war_renunciation__inherent_right_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(article_9_war_renunciation__inherent_right_reading, extractiveness, 0.35).
narrative_ontology:constraint_metric(article_9_war_renunciation__inherent_right_reading, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(article_9_war_renunciation__inherent_right_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(article_9_war_renunciation__inherent_right_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(article_9_war_renunciation__inherent_right_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(article_9_war_renunciation__inherent_right_reading, tangled_rope).
narrative_ontology:human_readable(article_9_war_renunciation__inherent_right_reading, "Article 9 War Renunciation (Inherent Self-Defense Reading)").
narrative_ontology:topic_domain(article_9_war_renunciation__inherent_right_reading, "constitutional_law/security_policy").

domain_priors:requires_active_enforcement(article_9_war_renunciation__inherent_right_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(article_9_war_renunciation__inherent_right_reading, '1343698b-73e0-4cb9-b291-e494924238f5').
narrative_ontology:cs_kernel_codification('1343698b-73e0-4cb9-b291-e494924238f5', fixed_text).
narrative_ontology:cs_authority_grounding('1343698b-73e0-4cb9-b291-e494924238f5', lineage).
narrative_ontology:cs_interpretation_layer_present('1343698b-73e0-4cb9-b291-e494924238f5').
narrative_ontology:cs_reading_relation('1343698b-73e0-4cb9-b291-e494924238f5', article_9_war_renunciation__strict_pacifist_reading, coexists_with).
narrative_ontology:cs_reading_relation('1343698b-73e0-4cb9-b291-e494924238f5', article_9_war_renunciation__collective_self_defense_reading, coexists_with).
narrative_ontology:cs_axiom('1343698b-73e0-4cb9-b291-e494924238f5', foundational, sovereign_right_to_self_defense).
narrative_ontology:cs_axiom_status(sovereign_right_to_self_defense, holdable).
narrative_ontology:cs_axiom_grounding('1343698b-73e0-4cb9-b291-e494924238f5', sovereign_right_to_self_defense, deontological).
narrative_ontology:cs_axiom('1343698b-73e0-4cb9-b291-e494924238f5', foundational, article_9_prohibits_aggressive_war_only).
narrative_ontology:cs_axiom_status(article_9_prohibits_aggressive_war_only, holdable).
narrative_ontology:cs_axiom_grounding('1343698b-73e0-4cb9-b291-e494924238f5', article_9_prohibits_aggressive_war_only, conventional).
narrative_ontology:cs_reference_frame('1343698b-73e0-4cb9-b291-e494924238f5', post_war_constitutional_settlement).
narrative_ontology:cs_drift_state('1343698b-73e0-4cb9-b291-e494924238f5', contemporary_geopolitical_tensions, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('1343698b-73e0-4cb9-b291-e494924238f5', '').
narrative_ontology:cs_kernel_id(article_9_war_renunciation__inherent_right_reading, article_9_war_renunciation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(article_9_war_renunciation__inherent_right_reading, japanese_government).
narrative_ontology:constraint_beneficiary(article_9_war_renunciation__inherent_right_reading, japanese_citizens).
narrative_ontology:constraint_victim(article_9_war_renunciation__inherent_right_reading, strict_pacifist_advocates).
narrative_ontology:constraint_victim(article_9_war_renunciation__inherent_right_reading, military_expansionists).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(article_9_war_renunciation__inherent_right_reading, international_allies).
narrative_ontology:constraint_vindicates(article_9_war_renunciation__inherent_right_reading, sovereign_self_defense_principle).
narrative_ontology:constraint_vindicates(article_9_war_renunciation__inherent_right_reading, constitutional_pacifism_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets Article 9 to permit a 'minimum necessary' self-defense capacity, justifying the existence and operations of the Self-Defense Forces (SDF). Benefits from maintaining national security while adhering to a constitutional interpretation that avoids outright re-militarization. Constrained by public opinion and legal challenges.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__inherent_right_reading, japanese_government, agenda_setter,
    institutional, generational, constrained, national).

% Benefit from national security provided by the SDF under this interpretation, which balances defense needs with a commitment to peace. Bear the costs of maintaining the SDF through taxes. Their exit options are limited to political action or emigration.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__inherent_right_reading, japanese_citizens, beneficiary,
    organized, biographical, constrained, national).

% View any military force, even for self-defense, as a violation of Article 9's absolute renunciation of war. They bear the 'cost' of living in a state that maintains armed forces, which contradicts their core identity and interpretation of the constitution. Their exit is identity-locked, as their stance is fundamental to their political and moral identity.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__inherent_right_reading, strict_pacifist_advocates, payer,
    moderate, generational, identity_locked, national).

% Advocate for a more robust military capacity, including offensive capabilities, arguing that the 'minimum necessary' interpretation is too restrictive for modern security challenges. They bear the cost of a constrained military that they believe is insufficient. Their exit is constrained by the constitutional interpretation and public sentiment.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__inherent_right_reading, military_expansionists, payer,
    moderate, generational, constrained, national).

% Benefit from Japan's defensive capacity as a stabilizing force in the region, without requiring Japan to engage in aggressive actions. Their relationship is one of strategic alignment, with options to adjust alliances based on Japan's security posture.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__inherent_right_reading, international_allies, beneficiary,
    institutional, generational, mobile, global).

% Analyze the legal and historical evolution of Article 9 interpretations, assessing the coherence and implications of the 'inherent right to self-defense' reading within the broader constitutional framework. Their role is purely analytical.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__inherent_right_reading, constitutional_scholars, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates national security policy by defining the permissible scope of military force, allowing for territorial defense while maintaining a constitutional commitment to peace. This provides a stable framework for defense planning and international relations.
% TRANSFER_FUNCTION: Transfers the responsibility for national defense to the SDF, funded by citizens, while limiting the scope of military action. It also transfers a sense of security to citizens and allies, at the cost of potential military capabilities desired by some factions.
% ABSENT_VOICES: Those who advocate for a fully unconstrained military, capable of offensive projection, are largely excluded from the mainstream constitutional discourse, as their views directly contradict the core tenets of Article 9, even under this interpretation.
% DISAPPEARANCE_RATIONALE: If this interpretation vanished, Japan would either revert to a strict pacifist stance (leaving it vulnerable or requiring new security arrangements) or fully re-militarize (fundamentally altering its regional and global role). Either outcome would necessitate a significant rearrangement of national policy and international relations.
% FOUNDING_PROBLEM: To prevent Japan from ever again engaging in aggressive warfare, while acknowledging the practical necessity of defending its territory and sovereignty in a post-WWII world.
% FOUNDING_PROBLEM_CORROBORATION: The Japanese government and a majority of citizens attest that the problem of balancing peace with security remains live, citing ongoing regional tensions. Strict pacifist advocates argue the problem is only partially solved due to the SDF's existence, while military expansionists argue it's not adequately solved due to current limitations. International observers corroborate the ongoing tension between these poles.
narrative_ontology:disappearance_verdict(article_9_war_renunciation__inherent_right_reading, world_rearranges).
narrative_ontology:founding_problem_status(article_9_war_renunciation__inherent_right_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(article_9_war_renunciation__inherent_right_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(article_9_war_renunciation__inherent_right_reading, 'none', 1).
narrative_ontology:epsilon_provenance(article_9_war_renunciation__inherent_right_reading, 0.35, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(article_9_war_renunciation__inherent_right_reading_tests).
:- end_tests(article_9_war_renunciation__inherent_right_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.35) is moderate, reflecting the costs borne by those who disagree with the interpretation (pacifists who see any military as a violation, and expansionists who see it as too restrictive). Suppression (0.45) is moderate, as this interpretation is actively enforced through legal and political means, limiting alternative security postures. Theater ratio (0.20) is low, as the SDF's defensive function is largely genuine, though some performative aspects exist in maintaining the 'minimum necessary' narrative. The metrics show a slight increase over time, reflecting growing regional security concerns and the gradual expansion of SDF roles within this interpretation.
 *
 * PERSPECTIVAL GAP:
 *   The government and citizens experience this as a necessary and beneficial coordination, while pacifists perceive it as a betrayal of the constitution's spirit, and expansionists see it as an insufficient compromise. The engine will compute these divergent classifications based on the declared structural positions.
 *
 * DIRECTIONALITY LOGIC:
 *   The Japanese government and citizens are beneficiaries, gaining security and a stable constitutional framework. Strict pacifist advocates and military expansionists are payers, as their preferred interpretations are suppressed by this dominant reading. International allies benefit from regional stability. Constitutional scholars observe and analyze the dynamics.
 *
 * MANDATROPHY ANALYSIS:
 *   This interpretation prevents mislabeling genuine self-defense coordination as pure extraction by acknowledging the security function. However, it also avoids mislabeling the constraint as a pure 'rope' by recognizing the extraction from those with alternative, suppressed interpretations. The 'live' status of the founding problem, despite the contested solution, indicates that the mandate has not atrophied, but its resolution remains a point of contention.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    minimum_necessary_ambiguity,
    'What constitutes ''minimum necessary'' defensive capacity in a rapidly evolving geopolitical landscape?',
    'Ongoing legislative debate, judicial review, and public discourse, potentially informed by expert military and security assessments. International legal precedents on self-defense.',
    'If ''minimum necessary'' expands significantly, this reading could drift towards the ''collective_self_defense_reading'' or even ''military_expansionist'' positions, increasing extractiveness for pacifists. If it contracts, it would increase extraction for those seeking greater security.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(minimum_necessary_ambiguity, conceptual, 'Ambiguity in the scope of permissible self-defense under Article 9.').

omega_variable(
    pacifist_identity_lock_strength,
    'How deeply is the strict pacifist interpretation of Article 9 integrated into the identity of its advocates, and what would be the cost of abandoning it?',
    'Sociological studies of political identity, analysis of activist group cohesion and longevity, and individual testimonials regarding the role of Article 9 in their worldview.',
    'If identity-lock is extremely strong, the effective suppression and extraction for pacifists are higher than structural measures suggest, as exit is not merely constrained but psychologically foreclosed. This would push their seat classification towards ''snare''.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(pacifist_identity_lock_strength, empirical, 'Strength of identity-lock for strict pacifist advocates.').

omega_variable(
    structural_vs_internalized_suppression_pacifists,
    'Is the suppression experienced by strict pacifist advocates primarily structural (legal/political barriers) or internalized (cognitive patterns, social pressure)?',
    'Analysis of legal challenges and their outcomes, public opinion surveys on pacifism, and qualitative interviews with pacifist activists regarding their perceived barriers and internal motivations.',
    'If internalized suppression is a significant component, the constraint''s effective suppression is higher for pacifists, as the ''cost'' of non-compliance extends beyond external enforcement. This would amplify the perceived extractiveness from their seat.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(structural_vs_internalized_suppression_pacifists, empirical, 'Structural vs. internalized suppression mechanism for strict pacifist advocates.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(article_9_war_renunciation__inherent_right_reading, 0, 70).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(arti_tr_t0, article_9_war_renunciation__inherent_right_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(arti_tr_t10, article_9_war_renunciation__inherent_right_reading, theater_ratio, 10, 0.12).
narrative_ontology:measurement(arti_tr_t20, article_9_war_renunciation__inherent_right_reading, theater_ratio, 20, 0.15).
narrative_ontology:measurement(arti_tr_t30, article_9_war_renunciation__inherent_right_reading, theater_ratio, 30, 0.17).
narrative_ontology:measurement(arti_tr_t40, article_9_war_renunciation__inherent_right_reading, theater_ratio, 40, 0.18).
narrative_ontology:measurement(arti_tr_t50, article_9_war_renunciation__inherent_right_reading, theater_ratio, 50, 0.19).
narrative_ontology:measurement(arti_tr_t60, article_9_war_renunciation__inherent_right_reading, theater_ratio, 60, 0.2).
narrative_ontology:measurement(arti_tr_t70, article_9_war_renunciation__inherent_right_reading, theater_ratio, 70, 0.2).

% Extraction over time
narrative_ontology:measurement(arti_be_t0, article_9_war_renunciation__inherent_right_reading, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(arti_be_t10, article_9_war_renunciation__inherent_right_reading, base_extractiveness, 10, 0.28).
narrative_ontology:measurement(arti_be_t20, article_9_war_renunciation__inherent_right_reading, base_extractiveness, 20, 0.3).
narrative_ontology:measurement(arti_be_t30, article_9_war_renunciation__inherent_right_reading, base_extractiveness, 30, 0.32).
narrative_ontology:measurement(arti_be_t40, article_9_war_renunciation__inherent_right_reading, base_extractiveness, 40, 0.33).
narrative_ontology:measurement(arti_be_t50, article_9_war_renunciation__inherent_right_reading, base_extractiveness, 50, 0.34).
narrative_ontology:measurement(arti_be_t60, article_9_war_renunciation__inherent_right_reading, base_extractiveness, 60, 0.35).
narrative_ontology:measurement(arti_be_t70, article_9_war_renunciation__inherent_right_reading, base_extractiveness, 70, 0.35).

% Suppression requirement over time
narrative_ontology:measurement(arti_su_t0, article_9_war_renunciation__inherent_right_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(arti_su_t10, article_9_war_renunciation__inherent_right_reading, suppression_requirement, 10, 0.38).
narrative_ontology:measurement(arti_su_t20, article_9_war_renunciation__inherent_right_reading, suppression_requirement, 20, 0.4).
narrative_ontology:measurement(arti_su_t30, article_9_war_renunciation__inherent_right_reading, suppression_requirement, 30, 0.42).
narrative_ontology:measurement(arti_su_t40, article_9_war_renunciation__inherent_right_reading, suppression_requirement, 40, 0.43).
narrative_ontology:measurement(arti_su_t50, article_9_war_renunciation__inherent_right_reading, suppression_requirement, 50, 0.44).
narrative_ontology:measurement(arti_su_t60, article_9_war_renunciation__inherent_right_reading, suppression_requirement, 60, 0.45).
narrative_ontology:measurement(arti_su_t70, article_9_war_renunciation__inherent_right_reading, suppression_requirement, 70, 0.45).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(article_9_war_renunciation__inherent_right_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(article_9_war_renunciation__inherent_right_reading, collective_self_defense_reading).
narrative_ontology:affects_constraint(article_9_war_renunciation__inherent_right_reading, strict_pacifist_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the Article 9 war renunciation kernel. Its interpretation of 'minimum necessary' self-defense influences, and is influenced by, other readings such as strict pacifism and collective self-defense.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
