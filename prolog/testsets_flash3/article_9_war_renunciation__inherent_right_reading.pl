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
 *   domain: constitutional_law/security_policy/institutional_legitimacy
 *
 * SUMMARY:
 *   This constraint represents the 'inherent right to self-defense' reading
 *   of Japan's Article 9, which renounces 'war' but is interpreted to permit
 *   a minimum necessary defensive capacity. This reading allows for the
 *   existence of the Self-Defense Forces (SDF) for territorial defense, while
 *   strictly prohibiting offensive military action. It is a pragmatic
 *   interpretation that seeks to balance constitutional ideals with
 *   geopolitical realities. The constraint is claimed as a Rope by its
 *   proponents, but its active enforcement against alternative
 *   interpretations and the costs borne by those advocating for stricter
 *   pacifism or broader military roles make it a Tangled Rope.
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
narrative_ontology:topic_domain(article_9_war_renunciation__inherent_right_reading, "constitutional_law/security_policy/institutional_legitimacy").

domain_priors:requires_active_enforcement(article_9_war_renunciation__inherent_right_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(article_9_war_renunciation__inherent_right_reading, '40fdb945-7050-42c7-8614-8098f739c715').
narrative_ontology:cs_kernel_codification('40fdb945-7050-42c7-8614-8098f739c715', fixed_text).
narrative_ontology:cs_authority_grounding('40fdb945-7050-42c7-8614-8098f739c715', lineage).
narrative_ontology:cs_interpretation_layer_present('40fdb945-7050-42c7-8614-8098f739c715').
narrative_ontology:cs_reading_relation('40fdb945-7050-42c7-8614-8098f739c715', article_9_war_renunciation__strict_pacifist_reading, coexists_with).
narrative_ontology:cs_reading_relation('40fdb945-7050-42c7-8614-8098f739c715', article_9_war_renunciation__collective_self_defense_reading, coexists_with).
narrative_ontology:cs_axiom('40fdb945-7050-42c7-8614-8098f739c715', foundational, inherent_right_of_self_defense).
narrative_ontology:cs_axiom_status(inherent_right_of_self_defense, holdable).
narrative_ontology:cs_axiom_grounding('40fdb945-7050-42c7-8614-8098f739c715', inherent_right_of_self_defense, deontological).
narrative_ontology:cs_axiom('40fdb945-7050-42c7-8614-8098f739c715', foundational, proportionality_of_defensive_force).
narrative_ontology:cs_axiom_status(proportionality_of_defensive_force, holdable).
narrative_ontology:cs_axiom_grounding('40fdb945-7050-42c7-8614-8098f739c715', proportionality_of_defensive_force, conventional).
narrative_ontology:cs_reference_frame('40fdb945-7050-42c7-8614-8098f739c715', post_war_constitutional_settlement).
narrative_ontology:cs_drift_state('40fdb945-7050-42c7-8614-8098f739c715', contemporary_geopolitical_context, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('40fdb945-7050-42c7-8614-8098f739c715', '').
narrative_ontology:cs_kernel_id(article_9_war_renunciation__inherent_right_reading, article_9_war_renunciation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(article_9_war_renunciation__inherent_right_reading, japanese_government).
narrative_ontology:constraint_beneficiary(article_9_war_renunciation__inherent_right_reading, japanese_citizens).
narrative_ontology:constraint_victim(article_9_war_renunciation__inherent_right_reading, strict_pacifist_advocates).
narrative_ontology:constraint_victim(article_9_war_renunciation__inherent_right_reading, military_expansionists).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(article_9_war_renunciation__inherent_right_reading, regional_allies).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets Article 9 to permit a 'minimum necessary' self-defense capacity, maintaining the Self-Defense Forces (SDF) for territorial defense. Benefits from maintaining national security while adhering to constitutional principles, but faces pressure from both pacifist and expansionist factions.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__inherent_right_reading, japanese_government, agenda_setter,
    institutional, generational, constrained, national).

% Benefit from national security provided by the SDF under a constitution that renounces aggressive war. Bear the costs of maintaining the SDF through taxes and potential involvement in defensive actions. Their interpretation often aligns with a pragmatic view of self-preservation.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__inherent_right_reading, japanese_citizens, beneficiary,
    organized, biographical, constrained, national).

% Bear the cost of a perceived constitutional violation by the existence of any armed forces. Their identity is often tied to an absolute interpretation of Article 9, making compromise difficult. They actively resist any expansion of military capacity.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__inherent_right_reading, strict_pacifist_advocates, payer,
    moderate, generational, identity_locked, national).

% Bear the cost of a constrained military capacity, arguing it limits Japan's ability to respond to regional threats or project influence. They advocate for a broader interpretation of self-defense, including collective self-defense, and face resistance from the current constitutional reading.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__inherent_right_reading, military_expansionists, payer,
    powerful, generational, constrained, national).

% Benefit from Japan's defensive capacity contributing to regional stability, without the perceived threat of an aggressive Japanese military. Their security arrangements are influenced by Japan's interpretation of Article 9.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__inherent_right_reading, regional_allies, beneficiary,
    institutional, generational, mobile, regional).

% Analyze the legal and historical evolution of Article 9's interpretation, assessing its consistency with international law and domestic constitutional principles. Their work informs public and political debate.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__inherent_right_reading, constitutional_scholars, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates national security policy by defining the permissible scope of military force, balancing the constitutional renunciation of war with the inherent right of self-defense, thereby providing a framework for the SDF's existence and operation.
% TRANSFER_FUNCTION: Transfers the burden of maintaining a purely defensive military capacity (SDF) to the Japanese taxpayer, in exchange for national security and adherence to a non-aggressive constitutional posture. It also transfers the political cost of constitutional interpretation to the government.
% ABSENT_VOICES: Those advocating for a full-fledged offensive military capacity are largely excluded from mainstream constitutional discourse, as their proposals directly contradict the 'war renunciation' clause, even under this reading. Similarly, those advocating for absolute demilitarization find their views marginalized by the pragmatic necessity of defense.
% DISAPPEARANCE_RATIONALE: If this interpretation vanished, Japan's security policy would face immediate crisis. Either the nation would be left without a legal basis for self-defense (leading to vulnerability), or it would move towards unrestricted militarization (violating the spirit of Article 9), fundamentally altering its geopolitical role and domestic political landscape.
% FOUNDING_PROBLEM: To prevent Japan from ever again engaging in aggressive warfare, while acknowledging the practical need for national self-preservation in a post-WWII world.
% FOUNDING_PROBLEM_CORROBORATION: International observers and a broad consensus of Japanese citizens corroborate the ongoing need to balance pacifism with defense. While the specific interpretation is contested, the underlying problem of national security within constitutional limits remains live.
narrative_ontology:disappearance_verdict(article_9_war_renunciation__inherent_right_reading, world_rearranges).
narrative_ontology:founding_problem_status(article_9_war_renunciation__inherent_right_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(article_9_war_renunciation__inherent_right_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
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
 *   The extractiveness (0.35) reflects the cost of maintaining the SDF and the political capital expended in defending this interpretation against both pacifist and expansionist views. Suppression (0.45) is moderate, as active legal and political enforcement is required to prevent either absolute demilitarization or unrestricted military expansion. The theater ratio (0.20) is low, indicating that the SDF's defensive function is genuine, though the constitutional debate itself can sometimes appear performative. The increasing extractiveness and suppression over time reflect growing regional tensions and the ongoing political effort to maintain this delicate balance.
 *
 * PERSPECTIVAL GAP:
 *   The government and citizens experience this as a necessary and legitimate coordination mechanism for national security. However, both pacifist and expansionist groups perceive it as an extractive constraint that limits their desired outcomes, highlighting the inherent tension in balancing constitutional text with practical defense needs.
 *
 * DIRECTIONALITY LOGIC:
 *   The Japanese government and citizens are beneficiaries, gaining security and constitutional adherence. Strict pacifist advocates and military expansionists are victims, as their preferred interpretations are suppressed by this reading. Regional allies benefit from a stable, defensively-oriented Japan. Constitutional scholars act as observers.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    proportionality_threshold_ambiguity,
    'What constitutes ''minimum necessary'' defensive capacity, and how is this threshold determined and enforced against mission creep?',
    'Judicial review of SDF deployments and capabilities, legislative oversight, and public debate informed by expert analysis of regional threats and defensive technologies.',
    'If the ''minimum necessary'' threshold is found to be consistently exceeded without clear justification, the constraint''s extractiveness and suppression would increase, potentially reclassifying it towards a Snare as the coordination story weakens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(proportionality_threshold_ambiguity, conceptual, 'Ambiguity in defining the permissible scope of self-defense under Article 9.').

omega_variable(
    internalized_pacifism_suppression,
    'To what extent is the suppression of military expansionism due to internalized pacifist norms among the populace, versus active legal and political enforcement?',
    'Public opinion surveys tracking attitudes towards military roles, analysis of political discourse, and observation of resistance to proposed constitutional amendments.',
    'If suppression is largely internalized, the constraint is more robust against external pressures for reinterpretation. If it''s primarily external enforcement, the constraint is more vulnerable to shifts in political power or public sentiment.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(internalized_pacifism_suppression, empirical, 'Structural vs. internalized suppression mechanism for military expansionism.').

omega_variable(
    mandatrophy_of_pacifist_ideal,
    'Has the original pacifist ideal of Article 9 atrophied, with the ''inherent right'' reading serving to maintain a military capacity that is increasingly decoupled from the original intent?',
    'Historical analysis of constitutional debates, comparison of SDF capabilities with original post-war defense needs, and assessment of public perception regarding the SDF''s role.',
    'If the pacifist ideal is found to be largely atrophied, the constraint''s theater_ratio would increase, and its classification might drift towards a Piton, as its primary function becomes performative adherence to a historical mandate rather than genuine coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mandatrophy_of_pacifist_ideal, empirical, 'Whether the pacifist ideal has atrophied, leading to a performative maintenance of the ''inherent right'' reading.').


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
narrative_ontology:measurement(arti_tr_t2010, article_9_war_renunciation__inherent_right_reading, theater_ratio, 2010, 0.19).
narrative_ontology:measurement(arti_tr_t2024, article_9_war_renunciation__inherent_right_reading, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(arti_be_t1947, article_9_war_renunciation__inherent_right_reading, base_extractiveness, 1947, 0.2).
narrative_ontology:measurement(arti_be_t1960, article_9_war_renunciation__inherent_right_reading, base_extractiveness, 1960, 0.25).
narrative_ontology:measurement(arti_be_t1980, article_9_war_renunciation__inherent_right_reading, base_extractiveness, 1980, 0.3).
narrative_ontology:measurement(arti_be_t2000, article_9_war_renunciation__inherent_right_reading, base_extractiveness, 2000, 0.32).
narrative_ontology:measurement(arti_be_t2010, article_9_war_renunciation__inherent_right_reading, base_extractiveness, 2010, 0.34).
narrative_ontology:measurement(arti_be_t2024, article_9_war_renunciation__inherent_right_reading, base_extractiveness, 2024, 0.35).

% Suppression requirement over time
narrative_ontology:measurement(arti_su_t1947, article_9_war_renunciation__inherent_right_reading, suppression_requirement, 1947, 0.3).
narrative_ontology:measurement(arti_su_t1960, article_9_war_renunciation__inherent_right_reading, suppression_requirement, 1960, 0.35).
narrative_ontology:measurement(arti_su_t1980, article_9_war_renunciation__inherent_right_reading, suppression_requirement, 1980, 0.4).
narrative_ontology:measurement(arti_su_t2000, article_9_war_renunciation__inherent_right_reading, suppression_requirement, 2000, 0.42).
narrative_ontology:measurement(arti_su_t2010, article_9_war_renunciation__inherent_right_reading, suppression_requirement, 2010, 0.44).
narrative_ontology:measurement(arti_su_t2024, article_9_war_renunciation__inherent_right_reading, suppression_requirement, 2024, 0.45).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(article_9_war_renunciation__inherent_right_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(article_9_war_renunciation__inherent_right_reading, collective_self_defense_reading).
narrative_ontology:affects_constraint(article_9_war_renunciation__inherent_right_reading, strict_pacifist_reading).
narrative_ontology:affects_constraint(article_9_war_renunciation__inherent_right_reading, japan_us_security_treaty).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the Article 9 war renunciation kernel. Its interpretation of self-defense influences, and is influenced by, other readings and related security treaties.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
