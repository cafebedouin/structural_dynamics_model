% ============================================================================
% CONSTRAINT STORY: article_9_war_renunciation__strict_pacifist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_article_9_war_renunciation__strict_pacifist_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: article_9_war_renunciation__strict_pacifist_reading
 *   human_readable: Article 9 War Renunciation (Strict Pacifist Reading)
 *   domain: constitutional_law/security_policy/institutional_legitimacy
 *
 * SUMMARY:
 *   This constraint represents the 'strict pacifist' reading of Article 9 of
 *   the Japanese Constitution, which absolutely renounces war and prohibits
 *   the maintenance of any armed forces, even for self-defense. This reading
 *   interprets the textual language 'never be maintained' as a categorical
 *   prohibition, making organized military forces impermissible. Self-defense
 *   is seen as achievable only through non-military means or alliance
 *   dependence, with the Japanese state's security autonomy being a primary
 *   victim. The constraint is classified as a Snare due to its high
 *   extractiveness from the state's security function and the high
 *   suppression required to maintain this interpretation against evolving
 *   geopolitical realities.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(article_9_war_renunciation__strict_pacifist_reading, 0.85).
domain_priors:suppression_score(article_9_war_renunciation__strict_pacifist_reading, 0.9).
domain_priors:theater_ratio(article_9_war_renunciation__strict_pacifist_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(article_9_war_renunciation__strict_pacifist_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(article_9_war_renunciation__strict_pacifist_reading, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(article_9_war_renunciation__strict_pacifist_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(article_9_war_renunciation__strict_pacifist_reading, accessibility_collapse, 0.95).
narrative_ontology:constraint_metric(article_9_war_renunciation__strict_pacifist_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(article_9_war_renunciation__strict_pacifist_reading, snare).
narrative_ontology:human_readable(article_9_war_renunciation__strict_pacifist_reading, "Article 9 War Renunciation (Strict Pacifist Reading)").
narrative_ontology:topic_domain(article_9_war_renunciation__strict_pacifist_reading, "constitutional_law/security_policy/institutional_legitimacy").

domain_priors:requires_active_enforcement(article_9_war_renunciation__strict_pacifist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(article_9_war_renunciation__strict_pacifist_reading, '76c681bc-1e99-4810-aa0e-414052028d51').
narrative_ontology:cs_kernel_codification('76c681bc-1e99-4810-aa0e-414052028d51', fixed_text).
narrative_ontology:cs_authority_grounding('76c681bc-1e99-4810-aa0e-414052028d51', lineage).
narrative_ontology:cs_interpretation_layer_present('76c681bc-1e99-4810-aa0e-414052028d51').
narrative_ontology:cs_reading_relation('76c681bc-1e99-4810-aa0e-414052028d51', article_9_war_renunciation__inherent_right_reading, forecloses).
narrative_ontology:cs_reading_relation('76c681bc-1e99-4810-aa0e-414052028d51', article_9_war_renunciation__collective_self_defense_reading, forecloses).
narrative_ontology:cs_axiom('76c681bc-1e99-4810-aa0e-414052028d51', foundational, absolute_prohibition_on_military_force).
narrative_ontology:cs_axiom_status(absolute_prohibition_on_military_force, holdable).
narrative_ontology:cs_axiom_grounding('76c681bc-1e99-4810-aa0e-414052028d51', absolute_prohibition_on_military_force, deontological).
narrative_ontology:cs_axiom('76c681bc-1e99-4810-aa0e-414052028d51', secondary, national_identity_rooted_in_pacifism).
narrative_ontology:cs_axiom_status(national_identity_rooted_in_pacifism, holdable).
narrative_ontology:cs_axiom_grounding('76c681bc-1e99-4810-aa0e-414052028d51', national_identity_rooted_in_pacifism, conventional).
narrative_ontology:cs_reference_frame('76c681bc-1e99-4810-aa0e-414052028d51', post_war_absolute_pacifism).
narrative_ontology:cs_drift_state('76c681bc-1e99-4810-aa0e-414052028d51', contemporary_geopolitical_realities, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('76c681bc-1e99-4810-aa0e-414052028d51', '').
narrative_ontology:cs_kernel_id(article_9_war_renunciation__strict_pacifist_reading, article_9_war_renunciation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(article_9_war_renunciation__strict_pacifist_reading, pacifist_advocates).
narrative_ontology:constraint_beneficiary(article_9_war_renunciation__strict_pacifist_reading, anti_militarist_movements).
narrative_ontology:constraint_victim(article_9_war_renunciation__strict_pacifist_reading, japanese_state_security_apparatus).
narrative_ontology:constraint_victim(article_9_war_renunciation__strict_pacifist_reading, japanese_citizens_seeking_national_defense_autonomy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Actively interpret Article 9 as an absolute prohibition on any military force, including defensive. They lobby for strict adherence to this interpretation, viewing any deviation as a betrayal of constitutional principles. Their identity is deeply tied to this pacifist stance.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__strict_pacifist_reading, pacifist_advocates, agenda_setter,
    organized, generational, identity_locked, national).

% Benefit from the strict pacifist reading as it aligns with their broader goals of demilitarization and non-intervention. They use this interpretation to resist any expansion of Japan's military capabilities or role in international security.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__strict_pacifist_reading, anti_militarist_movements, beneficiary,
    organized, generational, constrained, national).

% Bears the cost of this reading by being denied the ability to maintain conventional armed forces for national defense. It must navigate complex legal and political challenges to justify even minimal self-defense capabilities, often relying on euphemisms and reinterpretation. Its institutional identity is constrained by this constitutional interpretation.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__strict_pacifist_reading, japanese_state_security_apparatus, payer,
    institutional, generational, identity_locked, national).

% Experience a perceived lack of autonomous national defense, relying heavily on alliances for security. They bear the psychological and practical costs of this dependence, feeling that the state's ability to protect its citizens is compromised by the strict pacifist interpretation.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__strict_pacifist_reading, japanese_citizens_seeking_national_defense_autonomy, payer,
    moderate, biographical, constrained, national).

% Observe the debate with concern, as the strict pacifist reading impacts Japan's ability to contribute to collective security arrangements. They exert diplomatic pressure for a more flexible interpretation that would allow for greater military cooperation.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__strict_pacifist_reading, international_allies, observer,
    institutional, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates national identity around a post-war commitment to peace and non-aggression, aiming to prevent any return to militarism.
% TRANSFER_FUNCTION: Transfers the burden of national defense from an autonomous military to reliance on alliances and diplomatic solutions, while transferring political capital to pacifist movements.
% ABSENT_VOICES: Future generations who might face direct threats requiring autonomous defense capabilities are absent from the original drafting and ongoing interpretation, their potential needs unrepresented in the current debate.
% DISAPPEARANCE_RATIONALE: If the strict pacifist reading vanished overnight, Japan would immediately face pressure to establish a conventional military, re-evaluate its alliances, and redefine its role in regional security. The entire security policy and constitutional framework would undergo a profound reorganization.
% FOUNDING_PROBLEM: The problem of preventing a resurgence of Japanese militarism and aggression after World War II, and establishing a new national identity rooted in peace.
% FOUNDING_PROBLEM_CORROBORATION: Pacifist advocates attest the problem is still live, citing historical precedents and ongoing geopolitical tensions. Proponents of a more flexible interpretation, including elements within the state security apparatus and some citizens, argue the original problem has been sufficiently addressed and new security challenges necessitate a re-evaluation; international allies corroborate the need for a more robust defense posture.
narrative_ontology:disappearance_verdict(article_9_war_renunciation__strict_pacifist_reading, world_rearranges).
narrative_ontology:founding_problem_status(article_9_war_renunciation__strict_pacifist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(article_9_war_renunciation__strict_pacifist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_gemini+stakeholder_backfill', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(article_9_war_renunciation__strict_pacifist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(article_9_war_renunciation__strict_pacifist_reading, 0.85, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(article_9_war_renunciation__strict_pacifist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(article_9_war_renunciation__strict_pacifist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(article_9_war_renunciation__strict_pacifist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness is high (0.85) because this reading imposes a severe limitation on the state's sovereign right to self-defense, forcing reliance on external powers or non-military strategies even in the face of direct threats. Suppression is also very high (0.90) as this interpretation requires active legal, political, and social enforcement to prevent any rearmament or reinterpretation that would allow for a conventional military. The theater ratio is low (0.10) because, from this reading's perspective, the prohibition is absolute and not merely performative; any existing 'self-defense forces' are seen as a violation, not a theatrical maintenance of the original intent.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of pacifist advocates, this constraint is a foundational Rope, coordinating a peaceful national identity. However, from the perspective of the state security apparatus, it operates as a Snare, extracting sovereign defense autonomy and requiring constant reinterpretation and suppression to maintain. The engine's classification as Snare reflects the structural reality of high extraction and suppression from the state's perspective, despite the claimed coordination function by its beneficiaries.
 *
 * DIRECTIONALITY LOGIC:
 *   Pacifist advocates and anti-militarist movements are the primary beneficiaries, as this reading aligns with their ideological commitments and grants them significant political leverage. The Japanese state security apparatus and citizens seeking national defense autonomy are the primary payers, bearing the costs of limited defense capabilities and reliance on alliances. International allies act as observers, advocating for a more flexible interpretation.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    absolute_prohibition_vs_inherent_right,
    'Is the textual language ''never be maintained'' an absolute prohibition on any armed forces, or does it permit minimum necessary defensive capacity as an inherent right of sovereignty?',
    'A definitive ruling by the Supreme Court of Japan or a constitutional amendment clarifying the scope of Article 9''s prohibition.',
    'If interpreted as permitting inherent self-defense, the extractiveness from the state security apparatus would decrease significantly, potentially reclassifying the constraint towards a Tangled Rope or even a Rope, depending on the scope of permitted forces.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(absolute_prohibition_vs_inherent_right, conceptual, 'Ambiguity regarding the absolute nature of the prohibition on armed forces.').

omega_variable(
    self_defense_through_alliances_efficacy,
    'Is self-defense through non-military means or alliance dependence genuinely sufficient for national security in the current geopolitical climate?',
    'Empirical analysis of regional security threats, military capabilities of potential adversaries, and the reliability of alliance commitments under various scenarios.',
    'If found insufficient, the perceived extractiveness from citizens seeking national defense autonomy would increase, intensifying pressure for reinterpretation or amendment. If found sufficient, it would strengthen the pacifist reading''s legitimacy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(self_defense_through_alliances_efficacy, empirical, 'Efficacy of non-military and alliance-dependent self-defense.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(article_9_war_renunciation__strict_pacifist_reading, 1947, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(arti_tr_t1947, article_9_war_renunciation__strict_pacifist_reading, theater_ratio, 1947, 0.05).
narrative_ontology:measurement(arti_tr_t1960, article_9_war_renunciation__strict_pacifist_reading, theater_ratio, 1960, 0.07).
narrative_ontology:measurement(arti_tr_t1980, article_9_war_renunciation__strict_pacifist_reading, theater_ratio, 1980, 0.09).
narrative_ontology:measurement(arti_tr_t2000, article_9_war_renunciation__strict_pacifist_reading, theater_ratio, 2000, 0.11).
narrative_ontology:measurement(arti_tr_t2010, article_9_war_renunciation__strict_pacifist_reading, theater_ratio, 2010, 0.1).
narrative_ontology:measurement(arti_tr_t2024, article_9_war_renunciation__strict_pacifist_reading, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(arti_be_t1947, article_9_war_renunciation__strict_pacifist_reading, base_extractiveness, 1947, 0.8).
narrative_ontology:measurement(arti_be_t1960, article_9_war_renunciation__strict_pacifist_reading, base_extractiveness, 1960, 0.82).
narrative_ontology:measurement(arti_be_t1980, article_9_war_renunciation__strict_pacifist_reading, base_extractiveness, 1980, 0.85).
narrative_ontology:measurement(arti_be_t2000, article_9_war_renunciation__strict_pacifist_reading, base_extractiveness, 2000, 0.87).
narrative_ontology:measurement(arti_be_t2010, article_9_war_renunciation__strict_pacifist_reading, base_extractiveness, 2010, 0.86).
narrative_ontology:measurement(arti_be_t2024, article_9_war_renunciation__strict_pacifist_reading, base_extractiveness, 2024, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(arti_su_t1947, article_9_war_renunciation__strict_pacifist_reading, suppression_requirement, 1947, 0.95).
narrative_ontology:measurement(arti_su_t1960, article_9_war_renunciation__strict_pacifist_reading, suppression_requirement, 1960, 0.92).
narrative_ontology:measurement(arti_su_t1980, article_9_war_renunciation__strict_pacifist_reading, suppression_requirement, 1980, 0.9).
narrative_ontology:measurement(arti_su_t2000, article_9_war_renunciation__strict_pacifist_reading, suppression_requirement, 2000, 0.88).
narrative_ontology:measurement(arti_su_t2010, article_9_war_renunciation__strict_pacifist_reading, suppression_requirement, 2010, 0.89).
narrative_ontology:measurement(arti_su_t2024, article_9_war_renunciation__strict_pacifist_reading, suppression_requirement, 2024, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(article_9_war_renunciation__strict_pacifist_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(article_9_war_renunciation__strict_pacifist_reading, 0.08).
narrative_ontology:affects_constraint(article_9_war_renunciation__strict_pacifist_reading, inherent_right_reading).
narrative_ontology:affects_constraint(article_9_war_renunciation__strict_pacifist_reading, collective_self_defense_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of Article 9 of the Japanese Constitution. It represents the strict pacifist interpretation, which is in direct tension with the inherent right and collective self-defense readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
