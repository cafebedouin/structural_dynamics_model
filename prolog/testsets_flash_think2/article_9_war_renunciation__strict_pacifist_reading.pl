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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   human_readable: Article 9 War Renunciation (Strict Pacifist Reading)
 *   domain: constitutional_law/security_policy/institutional_legitimacy
 *
 * SUMMARY:
 *   This constraint represents the strict pacifist reading of a
 *   constitutional article renouncing war and prohibiting the maintenance of
 *   armed forces. This reading interprets the text as a categorical
 *   prohibition on any military capacity, even for self-defense, requiring
 *   reliance on non-military means or alliances. While claimed as a
 *   'Mountain' (the true, unchangeable meaning of the constitution), its high
 *   extractiveness and suppression, coupled with identifiable beneficiaries,
 *   position it as a false summit candidate. The metrics reflect the
 *   substantial costs imposed on state security autonomy and the active
 *   suppression of military alternatives.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(article_9_war_renunciation__strict_pacifist_reading, 0.85).
domain_priors:suppression_score(article_9_war_renunciation__strict_pacifist_reading, 0.92).
domain_priors:theater_ratio(article_9_war_renunciation__strict_pacifist_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(article_9_war_renunciation__strict_pacifist_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(article_9_war_renunciation__strict_pacifist_reading, suppression_requirement, 0.92).
narrative_ontology:constraint_metric(article_9_war_renunciation__strict_pacifist_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(article_9_war_renunciation__strict_pacifist_reading, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(article_9_war_renunciation__strict_pacifist_reading, resistance, 0.8).

% --- Constraint claim ---
narrative_ontology:constraint_claim(article_9_war_renunciation__strict_pacifist_reading, mountain).
narrative_ontology:human_readable(article_9_war_renunciation__strict_pacifist_reading, "Article 9 War Renunciation (Strict Pacifist Reading)").
narrative_ontology:topic_domain(article_9_war_renunciation__strict_pacifist_reading, "constitutional_law/security_policy/institutional_legitimacy").

domain_priors:requires_active_enforcement(article_9_war_renunciation__strict_pacifist_reading).
domain_priors:emerges_naturally(article_9_war_renunciation__strict_pacifist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(article_9_war_renunciation__strict_pacifist_reading, 'c4397e42-ace9-4f7f-b2f9-b50f6d870284').
narrative_ontology:cs_kernel_codification('c4397e42-ace9-4f7f-b2f9-b50f6d870284', fixed_text).
narrative_ontology:cs_authority_grounding('c4397e42-ace9-4f7f-b2f9-b50f6d870284', lineage).
narrative_ontology:cs_interpretation_layer_present('c4397e42-ace9-4f7f-b2f9-b50f6d870284').
narrative_ontology:cs_reading_relation('c4397e42-ace9-4f7f-b2f9-b50f6d870284', article_9_war_renunciation__inherent_right_reading, forecloses).
narrative_ontology:cs_reading_relation('c4397e42-ace9-4f7f-b2f9-b50f6d870284', article_9_war_renunciation__collective_self_defense_reading, forecloses).
narrative_ontology:cs_axiom('c4397e42-ace9-4f7f-b2f9-b50f6d870284', foundational, war_renunciation_absolute).
narrative_ontology:cs_axiom_status(war_renunciation_absolute, holdable).
narrative_ontology:cs_axiom_grounding('c4397e42-ace9-4f7f-b2f9-b50f6d870284', war_renunciation_absolute, deontological).
narrative_ontology:cs_axiom('c4397e42-ace9-4f7f-b2f9-b50f6d870284', foundational, no_military_forces_ever).
narrative_ontology:cs_axiom_status(no_military_forces_ever, holdable).
narrative_ontology:cs_axiom_grounding('c4397e42-ace9-4f7f-b2f9-b50f6d870284', no_military_forces_ever, deontological).
narrative_ontology:cs_reference_frame('c4397e42-ace9-4f7f-b2f9-b50f6d870284', post_war_pacifist_constitution).
narrative_ontology:cs_drift_state('c4397e42-ace9-4f7f-b2f9-b50f6d870284', contemporary_security_environment, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('c4397e42-ace9-4f7f-b2f9-b50f6d870284', '').
narrative_ontology:cs_kernel_id(article_9_war_renunciation__strict_pacifist_reading, article_9_war_renunciation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(article_9_war_renunciation__strict_pacifist_reading, pacifist_advocates).
narrative_ontology:constraint_beneficiary(article_9_war_renunciation__strict_pacifist_reading, anti_war_movements).
narrative_ontology:constraint_victim(article_9_war_renunciation__strict_pacifist_reading, state_security_autonomy).
narrative_ontology:constraint_victim(article_9_war_renunciation__strict_pacifist_reading, defense_ministry).
narrative_ontology:constraint_victim(article_9_war_renunciation__strict_pacifist_reading, military_personnel).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Actively promote and defend the strict pacifist interpretation of Article 9, viewing it as the moral and legal foundation of the nation. They benefit from the legitimacy and influence of this reading in public discourse and policy.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__strict_pacifist_reading, pacifist_advocates, beneficiary,
    organized, generational, identity_locked, national).

% Align with the strict pacifist reading, using it as a legal basis for opposing military expansion, overseas deployments, and defense budget increases. They gain political leverage from this interpretation.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__strict_pacifist_reading, anti_war_movements, beneficiary,
    moderate, biographical, constrained, local).

% Bears the cost of being unable to independently maintain military forces for self-defense or participate in collective security arrangements, relying instead on alliances or non-military means. This reading severely constrains its options for national security.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__strict_pacifist_reading, state_security_autonomy, payer,
    institutional, civilizational, trapped, national).

% Operates under severe constitutional constraints, unable to develop or deploy conventional military capabilities that would be considered 'war potential' under this reading. Its mandate is fundamentally curtailed.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__strict_pacifist_reading, defense_ministry, payer,
    institutional, generational, constrained, national).

% Serve in forces that are constitutionally limited to a non-military, purely defensive role, often facing ambiguity about their legal status and operational scope. Their professional identity and career paths are shaped by these restrictions.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__strict_pacifist_reading, military_personnel, payer,
    powerless, biographical, identity_locked, national).

% Must navigate domestic and international pressures while adhering to (or seeking to reinterpret) Article 9. They are responsible for policy decisions that either uphold or challenge this strict reading, balancing constitutional fidelity with security needs.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__strict_pacifist_reading, government_officials, agenda_setter,
    powerful, immediate, constrained, national).

% Are forced to bear a disproportionate share of regional security burdens due to the constitutional limitations on their ally. They would advocate for a more flexible interpretation allowing for collective self-defense but are not direct parties to the domestic constitutional debate.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__strict_pacifist_reading, international_allies, excluded,
    institutional, generational, mobile, global).

% Analyze the historical context, textual meaning, and evolving interpretations of Article 9. They provide academic commentary on the legal implications of the strict pacifist reading and its alternatives.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__strict_pacifist_reading, constitutional_scholars, observer,
    analytical, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates national identity and foreign policy around a commitment to absolute pacifism, aiming to prevent re-engagement in war and foster international peace through non-military means.
% TRANSFER_FUNCTION: Transfers the burden of national defense and security autonomy from the state to either non-military diplomatic efforts or reliance on external alliances, while transferring legitimacy and moral authority to pacifist movements.
% ABSENT_VOICES: Military strategists and proponents of a robust, independent defense capability are often marginalized in public discourse that prioritizes the strict pacifist reading; they would argue for the necessity of conventional military forces for national survival.
% DISAPPEARANCE_RATIONALE: If the strict pacifist reading vanished overnight, the nation's security policy, defense posture, and international relations would fundamentally reorganize. A conventional military would likely be established, defense budgets would increase, and alliance structures would shift to reflect greater self-reliance.
% FOUNDING_PROBLEM: The problem of preventing a recurrence of aggressive war and militarism, which had led to immense suffering and national devastation.
% FOUNDING_PROBLEM_CORROBORATION: Pacifist advocates and anti-war movements attest that the problem of militarism remains live. However, government officials and international allies, citing evolving geopolitical threats, argue that the original problem has been superseded by new security challenges that require a different approach, with independent security analysts corroborating the shift in threat landscape.
narrative_ontology:disappearance_verdict(article_9_war_renunciation__strict_pacifist_reading, world_rearranges).
narrative_ontology:founding_problem_status(article_9_war_renunciation__strict_pacifist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(article_9_war_renunciation__strict_pacifist_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
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
 *   Extractiveness is high (0.85) because this reading imposes severe limitations on a state's ability to defend itself, forcing reliance on others or non-military solutions, which can be costly in terms of sovereignty and security. Suppression is very high (0.92) as it categorically prohibits the development of military capacity, actively suppressing any moves towards conventional defense. Theater ratio is low (0.1) because the strict reading is direct and leaves little room for performative maintenance; its enforcement is primarily through legal and political means, not theatrical displays. Accessibility collapse is high (0.9) as it aims to eliminate military options entirely. Resistance is high (0.8) due to ongoing debates about national security in a changing geopolitical landscape.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of pacifist advocates, this reading is a foundational 'Mountain' ensuring peace and preventing militarism. From the perspective of the defense ministry and state security autonomy, it is a highly extractive and suppressive 'Snare' that compromises national security. The engine's classification will highlight this divergence from the claimed type.
 *
 * DIRECTIONALITY LOGIC:
 *   Pacifist advocates and anti-war movements are beneficiaries (d near 0.0) as this reading aligns with their ideological goals and grants them significant moral and political authority. State security autonomy, the defense ministry, and military personnel are victims (d near 1.0) as they bear the direct costs of curtailed capabilities and ambiguous roles. Government officials are agenda-setters, navigating the constraint's enforcement and interpretation. International allies are excluded, as their security interests are affected but they have no direct say in the constitutional interpretation.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    textual_absolutism_vs_implied_necessity,
    'Is the textual language ''never be maintained'' an absolute, categorical prohibition, or does it implicitly allow for a minimum necessary defensive capacity for sovereign survival?',
    'Constitutional court rulings that explicitly address the scope of ''war potential'' and ''self-defense'' in light of international law and evolving security threats, or a constitutional amendment clarifying the text.',
    'If an implicit allowance for defensive capacity is recognized, the constraint''s extractiveness and suppression would decrease, potentially reclassifying it from a false summit to a more flexible ''Rope'' or ''Tangled Rope'' for national security.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(textual_absolutism_vs_implied_necessity, conceptual, 'Ambiguity regarding the absolute nature of the constitutional prohibition on armed forces.').

omega_variable(
    security_cost_vs_peace_dividend,
    'Does the strict pacifist reading genuinely contribute to national and regional peace, or does it create a security vacuum that increases reliance on external powers and potentially destabilizes the region?',
    'Long-term empirical studies comparing security outcomes, defense spending, and alliance dynamics in states with similar constitutional constraints versus those with conventional defense capabilities, controlling for geopolitical factors.',
    'If the reading is found to create a security vacuum, its perceived ''peace dividend'' would diminish, increasing its effective extractiveness and strengthening arguments for reinterpretation or amendment. If it demonstrably fosters peace, its coordination function would be reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(security_cost_vs_peace_dividend, empirical, 'Whether the security costs of the strict reading outweigh its peace-promoting benefits.').

omega_variable(
    false_summit_natural_law_or_political_choice,
    'Is the strict pacifist reading a genuine ''Mountain'' (an unchangeable, natural law of the constitution), or is it a ''Tangled Rope'' or ''Snare'' that benefits specific political groups by framing a policy choice as an immutable constitutional truth?',
    'Analysis of the historical process of interpretation, the political interests of its proponents, and the degree to which alternative interpretations are suppressed. If the interpretation''s persistence relies on active enforcement and suppression of alternatives rather than universal acceptance, it points to a constructed constraint.',
    'Reclassification from ''Mountain'' to ''Tangled Rope'' or ''Snare'' would expose the political and extractive dimensions of the constraint, shifting the debate from constitutional fidelity to policy choice and power dynamics.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(false_summit_natural_law_or_political_choice, conceptual, 'Whether the strict pacifist reading is a natural constitutional law or a politically constructed constraint.').


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
narrative_ontology:measurement(arti_be_t0, article_9_war_renunciation__strict_pacifist_reading, base_extractiveness, 0, 0.75).
narrative_ontology:measurement(arti_be_t15, article_9_war_renunciation__strict_pacifist_reading, base_extractiveness, 15, 0.78).
narrative_ontology:measurement(arti_be_t30, article_9_war_renunciation__strict_pacifist_reading, base_extractiveness, 30, 0.81).
narrative_ontology:measurement(arti_be_t45, article_9_war_renunciation__strict_pacifist_reading, base_extractiveness, 45, 0.83).
narrative_ontology:measurement(arti_be_t60, article_9_war_renunciation__strict_pacifist_reading, base_extractiveness, 60, 0.84).
narrative_ontology:measurement(arti_be_t75, article_9_war_renunciation__strict_pacifist_reading, base_extractiveness, 75, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(arti_su_t0, article_9_war_renunciation__strict_pacifist_reading, suppression_requirement, 0, 0.85).
narrative_ontology:measurement(arti_su_t15, article_9_war_renunciation__strict_pacifist_reading, suppression_requirement, 15, 0.87).
narrative_ontology:measurement(arti_su_t30, article_9_war_renunciation__strict_pacifist_reading, suppression_requirement, 30, 0.89).
narrative_ontology:measurement(arti_su_t45, article_9_war_renunciation__strict_pacifist_reading, suppression_requirement, 45, 0.9).
narrative_ontology:measurement(arti_su_t60, article_9_war_renunciation__strict_pacifist_reading, suppression_requirement, 60, 0.91).
narrative_ontology:measurement(arti_su_t75, article_9_war_renunciation__strict_pacifist_reading, suppression_requirement, 75, 0.92).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(article_9_war_renunciation__strict_pacifist_reading, identity_coordination).
narrative_ontology:affects_constraint(article_9_war_renunciation__strict_pacifist_reading, alliance_treaties_interpretation).
narrative_ontology:affects_constraint(article_9_war_renunciation__strict_pacifist_reading, national_defense_budget_allocation).
narrative_ontology:affects_constraint(article_9_war_renunciation__strict_pacifist_reading, regional_security_architecture).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the Article 9 war renunciation kernel. Its ε value differs significantly from the 'inherent_right_reading' and 'collective_self_defense_reading' due to its absolute prohibition on military forces, leading to distinct structural implications and classifications.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
