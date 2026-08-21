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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
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
 *   human_readable: Japan's Article 9 War Renunciation (Strict Pacifist Reading)
 *   domain: constitutional_law/security_policy/institutional_legitimacy
 *
 * SUMMARY:
 *   This constraint story instantiates the 'strict pacifist' reading of
 *   Japan's Article 9 of the Constitution, which absolutely renounces war and
 *   prohibits the maintenance of any armed forces, even for self-defense.
 *   This reading views the textual language 'never be maintained' as a
 *   categorical prohibition. While proponents claim this interpretation as a
 *   fundamental, unchangeable constitutional truth (hence the 'mountain'
 *   claim), its operation imposes significant extraction and suppression on
 *   the state's security autonomy, leading to identifiable victims. The
 *   engine's classification will measure the divergence between the claimed
 *   type and the operational metrics.
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
narrative_ontology:constraint_claim(article_9_war_renunciation__strict_pacifist_reading, mountain).
narrative_ontology:human_readable(article_9_war_renunciation__strict_pacifist_reading, "Japan's Article 9 War Renunciation (Strict Pacifist Reading)").
narrative_ontology:topic_domain(article_9_war_renunciation__strict_pacifist_reading, "constitutional_law/security_policy/institutional_legitimacy").

domain_priors:requires_active_enforcement(article_9_war_renunciation__strict_pacifist_reading).
domain_priors:emerges_naturally(article_9_war_renunciation__strict_pacifist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(article_9_war_renunciation__strict_pacifist_reading, '65fe65e3-ef2d-4a52-99d0-d275748f744d').
narrative_ontology:cs_kernel_codification('65fe65e3-ef2d-4a52-99d0-d275748f744d', fixed_text).
narrative_ontology:cs_authority_grounding('65fe65e3-ef2d-4a52-99d0-d275748f744d', lineage).
narrative_ontology:cs_interpretation_layer_present('65fe65e3-ef2d-4a52-99d0-d275748f744d').
narrative_ontology:cs_reading_relation('65fe65e3-ef2d-4a52-99d0-d275748f744d', article_9_war_renunciation__inherent_right_reading, forecloses).
narrative_ontology:cs_reading_relation('65fe65e3-ef2d-4a52-99d0-d275748f744d', article_9_war_renunciation__collective_self_defense_reading, forecloses).
narrative_ontology:cs_axiom('65fe65e3-ef2d-4a52-99d0-d275748f744d', foundational, military_forces_categorically_prohibited).
narrative_ontology:cs_axiom_status(military_forces_categorically_prohibited, holdable).
narrative_ontology:cs_axiom_grounding('65fe65e3-ef2d-4a52-99d0-d275748f744d', military_forces_categorically_prohibited, deontological).
narrative_ontology:cs_axiom('65fe65e3-ef2d-4a52-99d0-d275748f744d', foundational, war_renunciation_absolute).
narrative_ontology:cs_axiom_status(war_renunciation_absolute, holdable).
narrative_ontology:cs_axiom_grounding('65fe65e3-ef2d-4a52-99d0-d275748f744d', war_renunciation_absolute, deontological).
narrative_ontology:cs_reference_frame('65fe65e3-ef2d-4a52-99d0-d275748f744d', post_wwii_demilitarization).
narrative_ontology:cs_drift_state('65fe65e3-ef2d-4a52-99d0-d275748f744d', contemporary_geopolitical_shifts, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('65fe65e3-ef2d-4a52-99d0-d275748f744d', '').
narrative_ontology:cs_kernel_id(article_9_war_renunciation__strict_pacifist_reading, article_9_war_renunciation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(article_9_war_renunciation__strict_pacifist_reading, pacifist_advocates).
narrative_ontology:constraint_beneficiary(article_9_war_renunciation__strict_pacifist_reading, anti_war_movements).
narrative_ontology:constraint_victim(article_9_war_renunciation__strict_pacifist_reading, japanese_state_security).
narrative_ontology:constraint_victim(article_9_war_renunciation__strict_pacifist_reading, defense_industry).
narrative_ontology:constraint_victim(article_9_war_renunciation__strict_pacifist_reading, military_personnel).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(article_9_war_renunciation__strict_pacifist_reading, allied_nations).
narrative_ontology:constraint_victim(article_9_war_renunciation__strict_pacifist_reading, allied_nations).
narrative_ontology:constraint_vindicates(article_9_war_renunciation__strict_pacifist_reading, absolute_pacifism_doctrine).
narrative_ontology:constraint_vindicates(article_9_war_renunciation__strict_pacifist_reading, constitutional_supremacy_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Proponents of the strict pacifist reading, who interpret Article 9 as an absolute prohibition on any military capacity, even for self-defense. They actively campaign against rearmament and constitutional revision, seeing the constraint as a moral imperative and a safeguard against past militarism.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__strict_pacifist_reading, pacifist_advocates, agenda_setter,
    organized, generational, identity_locked, national).

% Benefit from the constitutional backing for their anti-war stance, which limits the state's ability to engage in military action. They are a key constituency for maintaining the strict interpretation.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__strict_pacifist_reading, anti_war_movements, beneficiary,
    organized, generational, constrained, national).

% Bears the cost of limited autonomy in defense policy, relying heavily on alliances (e.g., with the US) for collective security. This reading restricts its ability to develop independent military capabilities or respond directly to regional threats, impacting strategic planning and national sovereignty.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__strict_pacifist_reading, japanese_state_security, payer,
    institutional, civilizational, trapped, national).

% Faces severe limitations on its growth and scope due to the constitutional prohibition on offensive military capabilities and the ambiguity around defensive ones. It operates under strict export controls and domestic procurement limits.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__strict_pacifist_reading, defense_industry, payer,
    powerful, biographical, constrained, national).

% Serve in the 'Self-Defense Forces' (SDF), an organization whose very name reflects the constitutional constraint. Their roles, training, and deployment are strictly limited, leading to professional identity challenges and operational constraints compared to conventional militaries.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__strict_pacifist_reading, military_personnel, payer,
    moderate, biographical, identity_locked, national).

% Benefit from Japan's non-aggressive posture, which contributes to regional stability. However, they also bear increased responsibility for regional security due to Japan's limited military capacity, potentially incurring costs or strategic dependencies.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__strict_pacifist_reading, allied_nations, beneficiary,
    institutional, civilizational, constrained, global).
narrative_ontology:stakeholder_secondary_role(article_9_war_renunciation__strict_pacifist_reading, allied_nations, payer).

% Analyze and debate the meaning and implications of Article 9, contributing to the interpretive tradition. They do not directly benefit or pay but shape the intellectual landscape within which the constraint is understood and contested.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__strict_pacifist_reading, constitutional_scholars, observer,
    analytical, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(article_9_war_renunciation__strict_pacifist_reading, diffuse).
narrative_ontology:fixing_cost_class(article_9_war_renunciation__strict_pacifist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates national policy around an absolute commitment to peace and the renunciation of war, aiming to prevent any re-emergence of militarism and to foster a non-aggressive international stance.
% TRANSFER_FUNCTION: Transfers the state's sovereign right to maintain military forces and engage in war (even for self-defense) to a constitutional prohibition, effectively transferring defense responsibility to alliances or non-military means. It also transfers potential defense spending to other sectors or reduces it.
% ABSENT_VOICES: Future generations facing unforeseen security threats, who might find their options for national defense severely curtailed by this absolute interpretation. Realpolitik strategists who prioritize national power and independent defense capabilities would also be excluded from the policy-making framework this reading imposes.
% DISAPPEARANCE_RATIONALE: If this strict pacifist reading vanished overnight, Japan would likely move towards a more robust, independent military capacity, potentially including offensive capabilities. This would fundamentally alter Japan's domestic political landscape, its regional power dynamics, and its alliances, leading to a significant reorganization of the global security architecture.
% FOUNDING_PROBLEM: To prevent a recurrence of aggressive militarism and war, which led to immense suffering and destruction in Japan's past, by constitutionally prohibiting the maintenance of any war potential.
% FOUNDING_PROBLEM_CORROBORATION: Historians and international relations scholars widely corroborate the historical context of preventing militarism as the founding problem. However, contemporary security analysts, some political factions, and a segment of the public contest whether the problem of *defensive* security is still adequately addressed by this strict reading, especially given evolving regional threats. Legislative hearing testimony and public opinion polls reflect this contestation.
narrative_ontology:disappearance_verdict(article_9_war_renunciation__strict_pacifist_reading, world_rearranges).
narrative_ontology:founding_problem_status(article_9_war_renunciation__strict_pacifist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(article_9_war_renunciation__strict_pacifist_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(article_9_war_renunciation__strict_pacifist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(article_9_war_renunciation__strict_pacifist_reading, 0.85, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(article_9_war_renunciation__strict_pacifist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(article_9_war_renunciation__strict_pacifist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(article_9_war_renunciation__strict_pacifist_reading, ExtMetricName, E),
    domain_priors:suppression_score(article_9_war_renunciation__strict_pacifist_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(article_9_war_renunciation__strict_pacifist_reading),
    narrative_ontology:constraint_metric(article_9_war_renunciation__strict_pacifist_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(article_9_war_renunciation__strict_pacifist_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(article_9_war_renunciation__strict_pacifist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is very high (0.85) because this reading imposes a severe limitation on state sovereignty regarding defense, forcing reliance on external alliances or non-military means. Suppression is also very high (0.90) as the constitutional text, under this interpretation, actively suppresses any moves towards rearmament or independent military capacity. Theater ratio is low (0.10) because this reading is a principled, direct interpretation, not one maintained for performative reasons; its proponents genuinely believe in its absolute application. Accessibility collapse is near total (0.95) as this reading aims to eliminate military alternatives entirely. Resistance is high (0.70) from those who advocate for a more flexible interpretation or constitutional revision.
 *
 * PERSPECTIVAL GAP:
 *   Proponents of this reading perceive it as a fundamental safeguard for peace and a moral triumph, a 'mountain' of constitutional principle. From the perspective of state security and military personnel, it operates as a severe 'snare' or 'tangled_rope', imposing significant costs and limiting essential functions. The engine's per-seat classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Pacifist advocates and anti-war movements are beneficiaries, as the constraint aligns with their core values and objectives, and they actively work to maintain this interpretation. Japanese state security, the defense industry, and military personnel are victims, bearing the direct costs of limited autonomy, restricted markets, and constrained professional roles. Allied nations are complex: they benefit from Japan's non-aggression but may incur costs or strategic dependencies due to Japan's limited defense capabilities.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_policy_choice,
    'Is the strict pacifist reading of Article 9 a self-evident, natural law of the constitution, or a specific policy choice embedded in the text that could be reinterpreted or amended?',
    'Analysis of constitutional amendment processes, judicial precedent on interpretation, and comparative constitutional law regarding ''unalterable'' clauses. If it can be amended or reinterpreted through established legal processes, it is a policy choice, not a natural law.',
    'If a policy choice, the ''mountain'' claim is a false summit, and the constraint would reclassify to a ''snare'' or ''tangled_rope'' for the state, reflecting its constructed and extractive nature.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(natural_law_vs_policy_choice, conceptual, 'Ambiguity between constitutional interpretation as natural law vs. policy.').

omega_variable(
    self_defense_interpretation_ambiguity,
    'Is ''self-defense'' inherently a military concept, or can it be achieved solely through non-military means or alliance dependence, as implied by this reading?',
    'Empirical analysis of state security in the absence of military forces, and the effectiveness of non-military defense strategies in contemporary geopolitical contexts. Comparative case studies of states with similar constitutional prohibitions.',
    'If self-defense is found to require military means, the constraint''s suppression of military capacity becomes a direct suppression of a fundamental state function, increasing its effective extractiveness and solidifying its ''snare'' characteristics.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(self_defense_interpretation_ambiguity, empirical, 'Whether self-defense is inherently military or can be non-military.').

omega_variable(
    alliance_dependence_cost,
    'What are the full, unacknowledged costs (economic, political, strategic) of Japan''s reliance on allied nations for its defense, a consequence of this strict reading?',
    'Comprehensive economic modeling of alliance contributions, analysis of strategic autonomy limitations, and assessment of political leverage exerted by allies due to defense dependency.',
    'If these costs are substantial and unacknowledged, the effective extraction from ''japanese_state_security'' is higher than currently measured, as a hidden transfer of resources or autonomy occurs.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alliance_dependence_cost, empirical, 'Hidden costs of defense reliance on allies.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(article_9_war_renunciation__strict_pacifist_reading, 0, 75).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(arti_tr_t0, article_9_war_renunciation__strict_pacifist_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(arti_tr_t15, article_9_war_renunciation__strict_pacifist_reading, theater_ratio, 15, 0.1).
narrative_ontology:measurement(arti_tr_t30, article_9_war_renunciation__strict_pacifist_reading, theater_ratio, 30, 0.1).
narrative_ontology:measurement(arti_tr_t45, article_9_war_renunciation__strict_pacifist_reading, theater_ratio, 45, 0.1).
narrative_ontology:measurement(arti_tr_t60, article_9_war_renunciation__strict_pacifist_reading, theater_ratio, 60, 0.1).
narrative_ontology:measurement(arti_tr_t75, article_9_war_renunciation__strict_pacifist_reading, theater_ratio, 75, 0.1).

% Extraction over time
narrative_ontology:measurement(arti_be_t0, article_9_war_renunciation__strict_pacifist_reading, base_extractiveness, 0, 0.8).
narrative_ontology:measurement(arti_be_t15, article_9_war_renunciation__strict_pacifist_reading, base_extractiveness, 15, 0.82).
narrative_ontology:measurement(arti_be_t30, article_9_war_renunciation__strict_pacifist_reading, base_extractiveness, 30, 0.83).
narrative_ontology:measurement(arti_be_t45, article_9_war_renunciation__strict_pacifist_reading, base_extractiveness, 45, 0.84).
narrative_ontology:measurement(arti_be_t60, article_9_war_renunciation__strict_pacifist_reading, base_extractiveness, 60, 0.85).
narrative_ontology:measurement(arti_be_t75, article_9_war_renunciation__strict_pacifist_reading, base_extractiveness, 75, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(arti_su_t0, article_9_war_renunciation__strict_pacifist_reading, suppression_requirement, 0, 0.85).
narrative_ontology:measurement(arti_su_t15, article_9_war_renunciation__strict_pacifist_reading, suppression_requirement, 15, 0.87).
narrative_ontology:measurement(arti_su_t30, article_9_war_renunciation__strict_pacifist_reading, suppression_requirement, 30, 0.88).
narrative_ontology:measurement(arti_su_t45, article_9_war_renunciation__strict_pacifist_reading, suppression_requirement, 45, 0.89).
narrative_ontology:measurement(arti_su_t60, article_9_war_renunciation__strict_pacifist_reading, suppression_requirement, 60, 0.9).
narrative_ontology:measurement(arti_su_t75, article_9_war_renunciation__strict_pacifist_reading, suppression_requirement, 75, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(article_9_war_renunciation__strict_pacifist_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(article_9_war_renunciation__strict_pacifist_reading, japan_us_security_treaty).
narrative_ontology:affects_constraint(article_9_war_renunciation__strict_pacifist_reading, regional_security_architecture).
narrative_ontology:affects_constraint(article_9_war_renunciation__strict_pacifist_reading, inherent_right_reading).
narrative_ontology:affects_constraint(article_9_war_renunciation__strict_pacifist_reading, collective_self_defense_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three distinct readings of Japan's Article 9 war renunciation. Each reading has a different ε value and structural implications, and they are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
