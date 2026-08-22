% ============================================================================
% CONSTRAINT STORY: article_9_war_renunciation__strict_pacifist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   human_readable: Article 9 Strict Pacifist Reading: Categorical Prohibition on Armed Forces
 *   domain: constitutional_law/security_policy/institutional_legitimacy
 *
 * SUMMARY:
 *   Article 9 of the Japanese Constitution, as read by the strict pacifist
 *   interpretation, categorically prohibits the maintenance of any armed
 *   forces through the phrase 'never be maintained.' This reading treats the
 *   war renunciation as absolute, foreclosing both inherent self-defense
 *   capacity and collective self-defense action. The constraint structurally
 *   extracts security autonomy from the Japanese state and defense
 *   establishment while coordinating pacifist constituencies around a
 *   constitutional peace identity. It is actively enforced by a political and
 *   interpretive coalition that blocks constitutional revision and
 *   normalization of military capacity. The reading is one of three contested
 *   readings of the Article 9 kernel; it logically forecloses its siblings by
 *   asserting that no military forces whatsoever are textually permissible.
 *
 * KEY AGENTS:
 *   - pacifist_interpretive_coalition: Primary agenda-setter (organized/constrained) â enforces the strict reading through political opposition and interpretive work.
 *   - state_security_executive: Primary target (institutional/constrained) â bears the loss of security autonomy and operates under alliance dependence.
 *   - defense_autonomy_advocates: Secondary target (organized/constrained) â politically blocked from achieving constitutional revision.
 *   - pacifist_electorate: Primary beneficiary (moderate/mobile) â collects the coordination benefit of guaranteed non-militarization.
 *   - constitutional_scholars_observer: Analytical observer (analytical/analytical) â assesses text-practice divergence.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(article_9_war_renunciation__strict_pacifist_reading, 0.75).
domain_priors:suppression_score(article_9_war_renunciation__strict_pacifist_reading, 0.73).
domain_priors:theater_ratio(article_9_war_renunciation__strict_pacifist_reading, 0.7).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(article_9_war_renunciation__strict_pacifist_reading, extractiveness, 0.75).
narrative_ontology:constraint_metric(article_9_war_renunciation__strict_pacifist_reading, suppression_requirement, 0.73).
narrative_ontology:constraint_metric(article_9_war_renunciation__strict_pacifist_reading, theater_ratio, 0.7).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(article_9_war_renunciation__strict_pacifist_reading, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(article_9_war_renunciation__strict_pacifist_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(article_9_war_renunciation__strict_pacifist_reading, tangled_rope).
narrative_ontology:human_readable(article_9_war_renunciation__strict_pacifist_reading, "Article 9 Strict Pacifist Reading: Categorical Prohibition on Armed Forces").
narrative_ontology:topic_domain(article_9_war_renunciation__strict_pacifist_reading, "constitutional_law/security_policy/institutional_legitimacy").

domain_priors:requires_active_enforcement(article_9_war_renunciation__strict_pacifist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(article_9_war_renunciation__strict_pacifist_reading, '7e47cf2f-9a27-4de0-bc62-9afc6d8e3f90').
narrative_ontology:cs_kernel_codification('7e47cf2f-9a27-4de0-bc62-9afc6d8e3f90', fixed_text).
narrative_ontology:cs_authority_grounding('7e47cf2f-9a27-4de0-bc62-9afc6d8e3f90', lineage).
narrative_ontology:cs_interpretation_layer_present('7e47cf2f-9a27-4de0-bc62-9afc6d8e3f90').
narrative_ontology:cs_reading_relation('7e47cf2f-9a27-4de0-bc62-9afc6d8e3f90', article_9_war_renunciation__inherent_right_reading, forecloses).
narrative_ontology:cs_reading_relation('7e47cf2f-9a27-4de0-bc62-9afc6d8e3f90', article_9_war_renunciation__collective_self_defense_reading, forecloses).
narrative_ontology:cs_axiom('7e47cf2f-9a27-4de0-bc62-9afc6d8e3f90', foundational, organized_violence_categorically_impermissible).
narrative_ontology:cs_axiom_status(organized_violence_categorically_impermissible, holdable).
narrative_ontology:cs_axiom_grounding('7e47cf2f-9a27-4de0-bc62-9afc6d8e3f90', organized_violence_categorically_impermissible, conventional).
narrative_ontology:cs_axiom('7e47cf2f-9a27-4de0-bc62-9afc6d8e3f90', foundational, textual_prohibition_overrides_security_necessity).
narrative_ontology:cs_axiom_status(textual_prohibition_overrides_security_necessity, holdable).
narrative_ontology:cs_axiom_grounding('7e47cf2f-9a27-4de0-bc62-9afc6d8e3f90', textual_prohibition_overrides_security_necessity, deontological).
narrative_ontology:cs_reference_frame('7e47cf2f-9a27-4de0-bc62-9afc6d8e3f90', absolute_renunciation_framework).
narrative_ontology:cs_drift_state('7e47cf2f-9a27-4de0-bc62-9afc6d8e3f90', contemporary_reinterpretation_era, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('7e47cf2f-9a27-4de0-bc62-9afc6d8e3f90', '').
narrative_ontology:cs_kernel_id(article_9_war_renunciation__strict_pacifist_reading, article_9_war_renunciation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(article_9_war_renunciation__strict_pacifist_reading, pacifist_interpretive_coalition).
narrative_ontology:constraint_beneficiary(article_9_war_renunciation__strict_pacifist_reading, pacifist_electorate).
narrative_ontology:constraint_victim(article_9_war_renunciation__strict_pacifist_reading, state_security_executive).
narrative_ontology:constraint_victim(article_9_war_renunciation__strict_pacifist_reading, defense_autonomy_advocates).
narrative_ontology:constraint_vindicates(article_9_war_renunciation__strict_pacifist_reading, constitutional_pacifism_doctrine).
narrative_ontology:constraint_vindicates(article_9_war_renunciation__strict_pacifist_reading, civilian_supremacy_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Organized political and civil society coalition that defends the categorical interpretation of Article 9. They enforce the reading through opposition to constitutional revision, legal scholarship, and electoral mobilization. They collect political legitimacy and generational continuity from maintaining the peace constitution.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__strict_pacifist_reading, pacifist_interpretive_coalition, agenda_setter,
    organized, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(article_9_war_renunciation__strict_pacifist_reading, pacifist_interpretive_coalition, beneficiary).

% The Japanese Cabinet and defense bureaucracy, which administers state functions but is constitutionally prohibited from maintaining war potential. Must operate the Self-Defense Forces in a legal gray zone and rely on the US-Japan alliance for existential security. Cannot unilaterally exit the constraint without constitutional amendment.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__strict_pacifist_reading, state_security_executive, payer,
    institutional, immediate, constrained, national).

% Diffuse body of citizens who benefit from the constitutional bar against militarization. They support the strict reading as a guarantee against war and military coups. While individually mobile, their collective electoral behavior sustains the constraint's political defense.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__strict_pacifist_reading, pacifist_electorate, beneficiary,
    moderate, generational, mobile, national).

% Security policy hawks, revisionist politicians, and commentators who argue for normalized military capacity and constitutional revision. They bear the political and strategic costs of alliance dependence and constrained defense posture. Their exit is blocked by the strict reading's institutional and political entrenchment.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__strict_pacifist_reading, defense_autonomy_advocates, payer,
    organized, biographical, constrained, national).

% Academic and juridical analysts who trace the divergence between the strict textual reading of Article 9 and the evolved practice of the Self-Defense Forces and security legislation. They do not enforce or directly benefit from the constraint, but provide the analytical vocabulary in which the contest is conducted.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__strict_pacifist_reading, constitutional_scholars_observer, observer,
    analytical, civilizational, analytical, national).

narrative_ontology:fixing_cost_class(article_9_war_renunciation__strict_pacifist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Prevents the re-emergence of Japanese militarism by constitutionally prohibiting the maintenance of armed forces, coordinating the polity around a pacifist security identity and alliance-dependent defense posture.
% TRANSFER_FUNCTION: Transfers security autonomy from the Japanese state to dependence on the United States-Japan alliance; transfers political legitimacy to pacifist constituencies who are guaranteed protection from state militarization.
% ABSENT_VOICES: Defense hawks and constitutional revisionists are present in government but their preferred policy is structurally excluded by the strict reading's defenders; the Self-Defense Forces personnel occupy a constitutional gray zone without textual authorization.
% DISAPPEARANCE_RATIONALE: If the categorical prohibition vanished overnight, the Japanese state would reorganize its security architecture, normalize military capacity, pursue constitutional revision, and reduce alliance dependence â the postwar pacifist security order would unravel and rearrange.
% FOUNDING_PROBLEM: Japanese militarism and imperial aggression culminating in WWII, requiring a constitutional break from the military-dominated state and prevention of future war potential.
% FOUNDING_PROBLEM_CORROBORATION: Original drafters and occupation authorities attest the problem as genuine militarism. Post-Cold War security officials and international relations scholars outside the pacifist beneficiary set attest the problem is solved and the constraint now extracts necessary security autonomy; they corroborate the dead-problem reading.
narrative_ontology:disappearance_verdict(article_9_war_renunciation__strict_pacifist_reading, world_rearranges).
narrative_ontology:founding_problem_status(article_9_war_renunciation__strict_pacifist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(article_9_war_renunciation__strict_pacifist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(article_9_war_renunciation__strict_pacifist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(article_9_war_renunciation__strict_pacifist_reading, 0.75, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness is high (0.75) because the categorical prohibition extracts full security autonomy from the state, forcing reliance on alliance dependence. Suppression is high (0.73) because the constraint's persistence depends on actively blocking constitutional revision and suppressing normalization of military capacity. Theater ratio is high (0.70) because seven decades of Self-Defense Forces existence and 2015 security legislation have created a massive gap between the strict textual reading and actual state practice, rendering much of the strict reading's defense performative. Accessibility collapse is high (0.78) because constitutional amendment is structurally very difficult in Japan, collapsing alternatives for revisionists. Resistance is substantial (0.68) from defense hawks and revisionist politicians who actively contest the constraint. The measurement series share one time grid to prevent misaligned drift detection.
 *
 * PERSPECTIVAL GAP:
 *   The pacifist interpretive coalition experiences this constraint as genuine coordination (protection against militarization) with negligible extraction; the state security executive experiences it as severe extraction of sovereign security capacity. The engine computes this divergence from the same structural data â the coalition has organized power but constrained exit because their political identity is fused to the peace constitution, while the institutional state has vast resources but is textually trapped.
 *
 * DIRECTIONALITY LOGIC:
 *   The pacifist interpretive coalition and electorate sit at the beneficiary end (low d): they gain political legitimacy and security-from-militarization. The state security executive and defense autonomy advocates sit at the target end (high d): they bear the costs of alliance dependence and legal incapacity. The divergence is driven by the beneficiary/victim declarations combined with exit options â the institutional state is powerful but exit is structurally blocked by amendment rules, amplifying its effective extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as tangled_rope prevents mislabeling the constraint as pure extraction (snare) â the pacifist coordination function is genuine and historically grounded in the WWII founding problem. It also prevents mislabeling it as pure coordination (rope) â the asymmetric extraction of security autonomy is real, and the state security executive is a genuine payer. The temporal measurements show rising theater ratio, indicating that the coordination function is atrophying into performance as practice drifts, but the constraint has not yet degraded to piton because the interpretive coalition still actively enforces it and believes in its mission.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    practice_text_divergence,
    'Has the strict pacifist reading been effectively overridden by seven decades of SDF existence and reinterpretation, or does it retain latent structural authority?',
    'Constitutional court ruling explicitly affirming or rejecting SDF constitutionality; successful constitutional amendment.',
    'If overridden in practice, the constraint''s effective extractiveness is lower than the textual reading suggests; if latent authority remains, extraction is ongoing despite appearance of drift.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(practice_text_divergence, empirical, 'Whether strict textual authority survives contrary state practice.').

omega_variable(
    security_autonomy_benefit_transfer,
    'Does the constraint''s extraction of security autonomy genuinely benefit domestic pacifist constituencies, or does it primarily benefit external security guarantors by locking Japan into alliance dependence?',
    'Comparative analysis of security policy autonomy under alternative constitutional frameworks; alliance bargaining dynamics.',
    'If gains flow to external actors, the beneficiary structure is misattributed; if domestic pacifists are the true beneficiaries, the coordination function is locally validated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(security_autonomy_benefit_transfer, conceptual, 'Whether alliance dependence redirects the constraint''s gains externally.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(article_9_war_renunciation__strict_pacifist_reading, 0, 77).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(arti_tr_t0, article_9_war_renunciation__strict_pacifist_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(arti_tr_t10, article_9_war_renunciation__strict_pacifist_reading, theater_ratio, 10, 0.3).
narrative_ontology:measurement(arti_tr_t20, article_9_war_renunciation__strict_pacifist_reading, theater_ratio, 20, 0.4).
narrative_ontology:measurement(arti_tr_t30, article_9_war_renunciation__strict_pacifist_reading, theater_ratio, 30, 0.48).
narrative_ontology:measurement(arti_tr_t45, article_9_war_renunciation__strict_pacifist_reading, theater_ratio, 45, 0.58).
narrative_ontology:measurement(arti_tr_t60, article_9_war_renunciation__strict_pacifist_reading, theater_ratio, 60, 0.65).
narrative_ontology:measurement(arti_tr_t77, article_9_war_renunciation__strict_pacifist_reading, theater_ratio, 77, 0.7).

% Extraction over time
narrative_ontology:measurement(arti_be_t0, article_9_war_renunciation__strict_pacifist_reading, base_extractiveness, 0, 0.65).
narrative_ontology:measurement(arti_be_t10, article_9_war_renunciation__strict_pacifist_reading, base_extractiveness, 10, 0.62).
narrative_ontology:measurement(arti_be_t20, article_9_war_renunciation__strict_pacifist_reading, base_extractiveness, 20, 0.65).
narrative_ontology:measurement(arti_be_t30, article_9_war_renunciation__strict_pacifist_reading, base_extractiveness, 30, 0.7).
narrative_ontology:measurement(arti_be_t45, article_9_war_renunciation__strict_pacifist_reading, base_extractiveness, 45, 0.72).
narrative_ontology:measurement(arti_be_t60, article_9_war_renunciation__strict_pacifist_reading, base_extractiveness, 60, 0.74).
narrative_ontology:measurement(arti_be_t77, article_9_war_renunciation__strict_pacifist_reading, base_extractiveness, 77, 0.75).

% Suppression requirement over time
narrative_ontology:measurement(arti_su_t0, article_9_war_renunciation__strict_pacifist_reading, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(arti_su_t10, article_9_war_renunciation__strict_pacifist_reading, suppression_requirement, 10, 0.58).
narrative_ontology:measurement(arti_su_t20, article_9_war_renunciation__strict_pacifist_reading, suppression_requirement, 20, 0.62).
narrative_ontology:measurement(arti_su_t30, article_9_war_renunciation__strict_pacifist_reading, suppression_requirement, 30, 0.68).
narrative_ontology:measurement(arti_su_t45, article_9_war_renunciation__strict_pacifist_reading, suppression_requirement, 45, 0.72).
narrative_ontology:measurement(arti_su_t60, article_9_war_renunciation__strict_pacifist_reading, suppression_requirement, 60, 0.75).
narrative_ontology:measurement(arti_su_t77, article_9_war_renunciation__strict_pacifist_reading, suppression_requirement, 77, 0.73).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(article_9_war_renunciation__strict_pacifist_reading, identity_coordination).
narrative_ontology:affects_constraint(article_9_war_renunciation__strict_pacifist_reading, article_9_war_renunciation__inherent_right_reading).
narrative_ontology:affects_constraint(article_9_war_renunciation__strict_pacifist_reading, article_9_war_renunciation__collective_self_defense_reading).

% DUAL FORMULATION NOTE:
% The Article 9 kernel decomposes into three structurally distinct constraints per the epsilon-invariance principle. The strict pacifist reading claims categorical prohibition and high extraction from security autonomy; the inherent right reading claims minimal extraction for defensive coordination; the collective self-defense reading claims extended military coordination. Each has a distinct epsilon, beneficiary/victim structure, and type classification.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
