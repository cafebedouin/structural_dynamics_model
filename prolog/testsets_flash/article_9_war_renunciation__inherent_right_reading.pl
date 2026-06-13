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
 *   constraint_id: article_9_war_renunciation__inherent_right_reading
 *   human_readable: Article 9 War Renunciation: Inherent Right to Self-Defense Reading
 *   domain: constitutional_law/security_policy/institutional_legitimacy
 *
 * SUMMARY:
 *   This constraint represents the 'inherent right to self-defense' reading
 *   of Japan's Article 9, which renounces 'war' but is interpreted to permit
 *   a 'minimum necessary' defensive capacity. This reading allows for the
 *   existence of the Self-Defense Forces (SDF) while strictly limiting their
 *   scope to territorial defense, avoiding aggressive military action. It is
 *   a contested interpretation of a foundational constitutional text,
 *   balancing pacifist ideals with sovereign security needs.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(article_9_war_renunciation__inherent_right_reading, 0.3).
domain_priors:suppression_score(article_9_war_renunciation__inherent_right_reading, 0.4).
domain_priors:theater_ratio(article_9_war_renunciation__inherent_right_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(article_9_war_renunciation__inherent_right_reading, extractiveness, 0.3).
narrative_ontology:constraint_metric(article_9_war_renunciation__inherent_right_reading, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(article_9_war_renunciation__inherent_right_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(article_9_war_renunciation__inherent_right_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(article_9_war_renunciation__inherent_right_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(article_9_war_renunciation__inherent_right_reading, tangled_rope).
narrative_ontology:human_readable(article_9_war_renunciation__inherent_right_reading, "Article 9 War Renunciation: Inherent Right to Self-Defense Reading").
narrative_ontology:topic_domain(article_9_war_renunciation__inherent_right_reading, "constitutional_law/security_policy/institutional_legitimacy").

domain_priors:requires_active_enforcement(article_9_war_renunciation__inherent_right_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(article_9_war_renunciation__inherent_right_reading, 'b649225b-d5ec-4615-afc3-8a41fb6da150').
narrative_ontology:cs_kernel_codification('b649225b-d5ec-4615-afc3-8a41fb6da150', fixed_text).
narrative_ontology:cs_authority_grounding('b649225b-d5ec-4615-afc3-8a41fb6da150', lineage).
narrative_ontology:cs_interpretation_layer_present('b649225b-d5ec-4615-afc3-8a41fb6da150').
narrative_ontology:cs_reading_relation('b649225b-d5ec-4615-afc3-8a41fb6da150', article_9_war_renunciation__strict_pacifist_reading, forecloses).
narrative_ontology:cs_reading_relation('b649225b-d5ec-4615-afc3-8a41fb6da150', article_9_war_renunciation__collective_self_defense_reading, coexists_with).
narrative_ontology:cs_axiom('b649225b-d5ec-4615-afc3-8a41fb6da150', foundational, sovereign_right_to_self_defense).
narrative_ontology:cs_axiom_status(sovereign_right_to_self_defense, holdable).
narrative_ontology:cs_axiom_grounding('b649225b-d5ec-4615-afc3-8a41fb6da150', sovereign_right_to_self_defense, deontological).
narrative_ontology:cs_axiom('b649225b-d5ec-4615-afc3-8a41fb6da150', foundational, war_renunciation_is_aggressive_action_only).
narrative_ontology:cs_axiom_status(war_renunciation_is_aggressive_action_only, holdable).
narrative_ontology:cs_axiom_grounding('b649225b-d5ec-4615-afc3-8a41fb6da150', war_renunciation_is_aggressive_action_only, conventional).
narrative_ontology:cs_reference_frame('b649225b-d5ec-4615-afc3-8a41fb6da150', post_wwii_sovereign_self_defense).
narrative_ontology:cs_drift_state('b649225b-d5ec-4615-afc3-8a41fb6da150', contemporary_geopolitical_shifts, gap(practice_drift, minor, false)).
narrative_ontology:cs_created_at('b649225b-d5ec-4615-afc3-8a41fb6da150', '').
narrative_ontology:cs_kernel_id(article_9_war_renunciation__inherent_right_reading, article_9_war_renunciation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(article_9_war_renunciation__inherent_right_reading, japanese_state).
narrative_ontology:constraint_beneficiary(article_9_war_renunciation__inherent_right_reading, japanese_citizens).
narrative_ontology:constraint_victim(article_9_war_renunciation__inherent_right_reading, military_expansionists).
narrative_ontology:constraint_victim(article_9_war_renunciation__inherent_right_reading, strict_pacifist_advocates).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(article_9_war_renunciation__inherent_right_reading, international_allies).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets Article 9 to allow for a 'minimum necessary' self-defense capacity, maintaining the Self-Defense Forces (SDF) for territorial defense. Benefits from national security and stability, but is constrained by constitutional interpretation and public opinion against aggressive military action.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__inherent_right_reading, japanese_state, agenda_setter,
    institutional, generational, constrained, national).

% Benefit from the security provided by the SDF and the constitutional commitment to peace. Bear the costs of maintaining the SDF through taxes, but generally support the current interpretation as a balance between peace and security.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__inherent_right_reading, japanese_citizens, beneficiary,
    organized, biographical, constrained, national).

% Advocate for a more robust military and broader interpretation of Article 9, including offensive capabilities. They are constrained by the current constitutional reading and public sentiment, viewing it as an impediment to Japan's full sovereign power.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__inherent_right_reading, military_expansionists, payer,
    moderate, biographical, constrained, national).

% Believe Article 9 absolutely prohibits any military force, including the SDF. They bear the cost of the SDF's existence and the state's interpretation, seeing it as a violation of the constitutional spirit. Their options are limited to political advocacy and legal challenges.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__inherent_right_reading, strict_pacifist_advocates, payer,
    organized, generational, constrained, national).

% Benefit from Japan's stable security posture and its contribution to regional stability, without requiring Japan to engage in aggressive military actions. They support Japan's right to self-defense within its constitutional limits.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__inherent_right_reading, international_allies, beneficiary,
    institutional, generational, mobile, global).

% Analyze the legal and historical evolution of Article 9's interpretation, debating the scope of self-defense and the implications for Japan's sovereignty. Their work informs public and political discourse but does not directly enforce the constraint.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__inherent_right_reading, constitutional_scholars, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates national security policy by defining the permissible scope of military force, balancing the constitutional renunciation of war with the inherent right of a sovereign state to defend its territory and citizens.
% TRANSFER_FUNCTION: Transfers the burden of maintaining a defensive military capacity (SDF) from a potentially aggressive military posture to one strictly limited to self-defense, from taxpayers to the state, and from potential external threats to a stable, constitutionally-defined security framework.
% ABSENT_VOICES: Those advocating for a full-fledged offensive military capability are largely excluded from mainstream constitutional discourse, as are those who demand absolute demilitarization. Both are present in public debate but lack institutional power to shift the dominant interpretation.
% DISAPPEARANCE_RATIONALE: If this interpretation vanished, Japan's security policy would immediately destabilize. Either the nation would fully remilitarize, leading to regional tensions, or it would disarm completely, leaving it vulnerable. The current balance, however contested, is foundational to its modern statehood.
% FOUNDING_PROBLEM: To prevent Japan from ever again engaging in aggressive warfare, while acknowledging the practical necessity of defending its territory and people in a post-WWII world.
% FOUNDING_PROBLEM_CORROBORATION: The Japanese government and a majority of citizens attest that the problem of balancing peace and security remains live. International observers and constitutional scholars, from outside the direct beneficiaries, corroborate the ongoing relevance of this foundational tension in Japan's security policy.
narrative_ontology:disappearance_verdict(article_9_war_renunciation__inherent_right_reading, world_rearranges).
narrative_ontology:founding_problem_status(article_9_war_renunciation__inherent_right_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(article_9_war_renunciation__inherent_right_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(article_9_war_renunciation__inherent_right_reading, 'none', 1).

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
 *   The constraint is classified as a Tangled Rope because it genuinely coordinates national security while also extracting from those who advocate for either full remilitarization or absolute pacifism. Extractiveness is moderate (0.3) as it imposes limits on military power and foreign policy options. Suppression is also moderate (0.4) as it requires active enforcement of constitutional interpretation against alternative readings, but public consensus provides some stability. Theater ratio is low (0.2) because the SDF's defensive function is real, not merely performative, though its constitutional justification is often debated.
 *
 * PERSPECTIVAL GAP:
 *   The Japanese state and citizens largely experience this as a beneficial coordination mechanism, providing security within a peaceful framework. However, military expansionists and strict pacifist advocates experience it as an extractive constraint, limiting their preferred security or ethical postures. The engine's per-seat classification will reflect these divergent experiences based on their declared power, exit options, and roles.
 *
 * DIRECTIONALITY LOGIC:
 *   The Japanese state and citizens are beneficiaries, as this reading provides a stable security framework. Military expansionists and strict pacifist advocates are victims, as their preferred interpretations are suppressed. International allies benefit from regional stability. Constitutional scholars are observers. The 'minimum necessary' clause acts as a proportionality constraint, allowing for a functional defense while renouncing aggressive war.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    minimum_necessary_scope_ambiguity,
    'What constitutes ''minimum necessary'' defensive capacity, and how does this evolve with geopolitical changes?',
    'Judicial review of specific SDF deployments or capabilities, or a constitutional amendment clarifying the scope.',
    'If ''minimum necessary'' expands significantly, the constraint could drift towards a more extractive ''collective_self_defense_reading'' or even a ''snare'' for pacifists. If it contracts, it could approach the ''strict_pacifist_reading''.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(minimum_necessary_scope_ambiguity, conceptual, 'Ambiguity in the scope of permissible self-defense under Article 9.').

omega_variable(
    natural_law_vs_constructed_right,
    'Is the ''inherent right to self-defense'' a natural law principle, or a constructed legal interpretation designed to justify the SDF?',
    'Philosophical and legal debate, potentially influenced by international legal norms and precedents.',
    'If purely constructed, the constraint''s legitimacy is more vulnerable to political challenge. If a natural law, its persistence is more robust, but its application remains subject to interpretation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_right, conceptual, 'The ontological status of the ''inherent right to self-defense''.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression of alternative interpretations structural (legal/political barriers) or internalized (public consensus/identity fusion)?',
    'Analysis of public opinion shifts after major geopolitical events or constitutional debates; if suppression persists after legal challenges, it suggests internalization.',
    'If internalized, the effective suppression is higher than structural measures suggest, as citizens self-regulate against alternative readings. If purely structural, legal changes could more easily shift the interpretation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for Article 9 interpretations.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(article_9_war_renunciation__inherent_right_reading, 1947, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(arti_tr_t1947, article_9_war_renunciation__inherent_right_reading, theater_ratio, 1947, 0.1).
narrative_ontology:measurement(arti_tr_t1960, article_9_war_renunciation__inherent_right_reading, theater_ratio, 1960, 0.12).
narrative_ontology:measurement(arti_tr_t1980, article_9_war_renunciation__inherent_right_reading, theater_ratio, 1980, 0.15).
narrative_ontology:measurement(arti_tr_t2000, article_9_war_renunciation__inherent_right_reading, theater_ratio, 2000, 0.18).
narrative_ontology:measurement(arti_tr_t2024, article_9_war_renunciation__inherent_right_reading, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(arti_be_t1947, article_9_war_renunciation__inherent_right_reading, base_extractiveness, 1947, 0.2).
narrative_ontology:measurement(arti_be_t1960, article_9_war_renunciation__inherent_right_reading, base_extractiveness, 1960, 0.22).
narrative_ontology:measurement(arti_be_t1980, article_9_war_renunciation__inherent_right_reading, base_extractiveness, 1980, 0.25).
narrative_ontology:measurement(arti_be_t2000, article_9_war_renunciation__inherent_right_reading, base_extractiveness, 2000, 0.28).
narrative_ontology:measurement(arti_be_t2024, article_9_war_renunciation__inherent_right_reading, base_extractiveness, 2024, 0.3).

% Suppression requirement over time
narrative_ontology:measurement(arti_su_t1947, article_9_war_renunciation__inherent_right_reading, suppression_requirement, 1947, 0.3).
narrative_ontology:measurement(arti_su_t1960, article_9_war_renunciation__inherent_right_reading, suppression_requirement, 1960, 0.32).
narrative_ontology:measurement(arti_su_t1980, article_9_war_renunciation__inherent_right_reading, suppression_requirement, 1980, 0.35).
narrative_ontology:measurement(arti_su_t2000, article_9_war_renunciation__inherent_right_reading, suppression_requirement, 2000, 0.38).
narrative_ontology:measurement(arti_su_t2024, article_9_war_renunciation__inherent_right_reading, suppression_requirement, 2024, 0.4).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(article_9_war_renunciation__inherent_right_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(article_9_war_renunciation__inherent_right_reading, article_9_war_renunciation__strict_pacifist_reading).
narrative_ontology:affects_constraint(article_9_war_renunciation__inherent_right_reading, article_9_war_renunciation__collective_self_defense_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the Article 9 war renunciation kernel. This 'inherent right' reading influences the other readings by establishing a precedent for a functional SDF, but is distinct in its scope and justification.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
