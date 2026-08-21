% ============================================================================
% CONSTRAINT STORY: jihad_quranic_corpus__revolutionary_vanguard_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jihad_quranic_corpus__revolutionary_vanguard_reading, []).

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
    narrative_ontology:stakeholder_gain_flow/2,
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
 *   constraint_id: jihad_quranic_corpus__revolutionary_vanguard_reading
 *   human_readable: Jihad as Immediate Individual Obligation (Revolutionary Vanguard Reading)
 *   domain: Islamic Jurisprudence / Political Theology
 *
 * SUMMARY:
 *   This constraint story instantiates the 'revolutionary vanguard' reading
 *   of the 'jihad_quranic_corpus' kernel. It describes the interpretation of
 *   jihad as an immediate individual obligation (fard 'ayn) against existing
 *   Muslim rulers deemed apostate and against foreign occupiers. This reading
 *   bypasses traditional state authority and classical jurisprudential
 *   safeguards, often employing takfir (excommunication) and emergency
 *   jurisprudence to justify decentralized, non-state violence. The
 *   coordination story (liberation of the Ummah from oppression) serves as
 *   cover for the vanguard's ideological and political objectives.
 *
 * KEY AGENTS:
 *   - revolutionary_vanguard_leaders: Primary beneficiary/agenda_setter (institutional/identity_locked) — benefits from constraint, directs action
 *   - committed_mujahidin: Secondary beneficiary (organized/identity_locked) — benefits from constraint, carries out action
 *   - apostate_rulers: Primary target (institutional/trapped) — bears extraction, faces direct challenge
 *   - occupying_forces: Primary target (institutional/constrained) — bears extraction, faces direct challenge
 *   - civilians_in_target_areas: Primary victim (powerless/trapped) — bears severe costs
 *   - classical_ulema: Excluded voice (institutional/constrained) — marginalized by the constraint
 *   - general_muslim_populace: Secondary victim (powerless/constrained) — bears diffuse costs
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jihad_quranic_corpus__revolutionary_vanguard_reading, 0.85).
domain_priors:suppression_score(jihad_quranic_corpus__revolutionary_vanguard_reading, 0.9).
domain_priors:theater_ratio(jihad_quranic_corpus__revolutionary_vanguard_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jihad_quranic_corpus__revolutionary_vanguard_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(jihad_quranic_corpus__revolutionary_vanguard_reading, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(jihad_quranic_corpus__revolutionary_vanguard_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jihad_quranic_corpus__revolutionary_vanguard_reading, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(jihad_quranic_corpus__revolutionary_vanguard_reading, resistance, 0.88).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jihad_quranic_corpus__revolutionary_vanguard_reading, snare).
narrative_ontology:human_readable(jihad_quranic_corpus__revolutionary_vanguard_reading, "Jihad as Immediate Individual Obligation (Revolutionary Vanguard Reading)").
narrative_ontology:topic_domain(jihad_quranic_corpus__revolutionary_vanguard_reading, "Islamic Jurisprudence / Political Theology").

domain_priors:requires_active_enforcement(jihad_quranic_corpus__revolutionary_vanguard_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jihad_quranic_corpus__revolutionary_vanguard_reading, '3c6aa5a9-0a59-45c4-b338-1dceecbeb56f').
narrative_ontology:cs_kernel_codification('3c6aa5a9-0a59-45c4-b338-1dceecbeb56f', fixed_text).
narrative_ontology:cs_authority_grounding('3c6aa5a9-0a59-45c4-b338-1dceecbeb56f', extraction).
narrative_ontology:cs_interpretation_layer_present('3c6aa5a9-0a59-45c4-b338-1dceecbeb56f').
narrative_ontology:cs_reading_relation('3c6aa5a9-0a59-45c4-b338-1dceecbeb56f', jihad_quranic_corpus__defensive_spiritual_reading, forecloses).
narrative_ontology:cs_reading_relation('3c6aa5a9-0a59-45c4-b338-1dceecbeb56f', jihad_quranic_corpus__expansionist_legalist_reading, forecloses).
narrative_ontology:cs_axiom('3c6aa5a9-0a59-45c4-b338-1dceecbeb56f', foundational, takfir_legitimizes_rebellion).
narrative_ontology:cs_axiom_status(takfir_legitimizes_rebellion, holdable).
narrative_ontology:cs_axiom_grounding('3c6aa5a9-0a59-45c4-b338-1dceecbeb56f', takfir_legitimizes_rebellion, conventional).
narrative_ontology:cs_axiom('3c6aa5a9-0a59-45c4-b338-1dceecbeb56f', foundational, jihad_fard_ayn_without_state_authority).
narrative_ontology:cs_axiom_status(jihad_fard_ayn_without_state_authority, holdable).
narrative_ontology:cs_axiom_grounding('3c6aa5a9-0a59-45c4-b338-1dceecbeb56f', jihad_fard_ayn_without_state_authority, conventional).
narrative_ontology:cs_reference_frame('3c6aa5a9-0a59-45c4-b338-1dceecbeb56f', early_islamic_community_ideal).
narrative_ontology:cs_drift_state('3c6aa5a9-0a59-45c4-b338-1dceecbeb56f', contemporary_muslim_world, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('3c6aa5a9-0a59-45c4-b338-1dceecbeb56f', '2024-07-30T12:00:00Z').
narrative_ontology:cs_kernel_id(jihad_quranic_corpus__revolutionary_vanguard_reading, jihad_quranic_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jihad_quranic_corpus__revolutionary_vanguard_reading, revolutionary_vanguard_leaders).
narrative_ontology:constraint_beneficiary(jihad_quranic_corpus__revolutionary_vanguard_reading, committed_mujahidin).
narrative_ontology:constraint_victim(jihad_quranic_corpus__revolutionary_vanguard_reading, apostate_rulers).
narrative_ontology:constraint_victim(jihad_quranic_corpus__revolutionary_vanguard_reading, occupying_forces).
narrative_ontology:constraint_victim(jihad_quranic_corpus__revolutionary_vanguard_reading, civilians_in_target_areas).
narrative_ontology:constraint_victim(jihad_quranic_corpus__revolutionary_vanguard_reading, classical_ulema).
narrative_ontology:constraint_victim(jihad_quranic_corpus__revolutionary_vanguard_reading, general_muslim_populace).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These leaders interpret religious texts to declare existing rulers apostate and foreign forces as occupiers, legitimizing immediate, decentralized armed struggle. They gain authority, legitimacy, and control over resources by mobilizing followers and challenging established powers. Their identity is fused with the cause.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__revolutionary_vanguard_reading, revolutionary_vanguard_leaders, agenda_setter,
    institutional, civilizational, identity_locked, global).

% Individuals who commit to the revolutionary vanguard's interpretation, seeing themselves as fulfilling a divine obligation. They gain spiritual reward, social status within the movement, and a sense of purpose. Exit is difficult due to ideological commitment and social ties.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__revolutionary_vanguard_reading, committed_mujahidin, beneficiary,
    organized, biographical, identity_locked, global).

% Existing state authorities, often Muslim, who are declared apostate by the revolutionary vanguard. They face direct armed rebellion, loss of legitimacy, and challenges to their governance. Their options are to fight, concede, or be overthrown.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__revolutionary_vanguard_reading, apostate_rulers, payer,
    institutional, biographical, trapped, national).

% Foreign military or political entities present in Muslim lands, declared as occupiers. They face armed resistance, casualties, and erosion of their strategic objectives. Their options are to withdraw, escalate, or negotiate under duress.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__revolutionary_vanguard_reading, occupying_forces, payer,
    institutional, biographical, constrained, regional).

% Populations living in areas targeted by the revolutionary vanguard or caught between warring factions. They suffer violence, displacement, loss of livelihood, and the collapse of public services. Their options are limited to fleeing or enduring.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__revolutionary_vanguard_reading, civilians_in_target_areas, payer,
    powerless, immediate, trapped, local).

% Traditional Islamic scholars who adhere to classical jurisprudence, emphasizing state authority, proportionality, and non-combatant immunity. Their interpretations are bypassed or rejected by the revolutionary vanguard, marginalizing their influence and authority in favor of radical readings.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__revolutionary_vanguard_reading, classical_ulema, excluded,
    institutional, generational, constrained, global).

% The broader Muslim community, many of whom are not directly involved in the conflict but are affected by its consequences, including societal polarization, increased surveillance, and the tarnishing of Islam's image. They are pressured to choose sides or face suspicion.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__revolutionary_vanguard_reading, general_muslim_populace, payer,
    powerless, biographical, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(jihad_quranic_corpus__revolutionary_vanguard_reading, revolutionary_vanguard_leaders).
narrative_ontology:fixing_cost_class(jihad_quranic_corpus__revolutionary_vanguard_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Mobilizes individuals and small groups for direct, decentralized armed action against perceived apostate rulers and foreign occupiers, bypassing traditional state authority and jurisprudential safeguards.
% TRANSFER_FUNCTION: Transfers religious and political authority from established states and classical religious scholars to the revolutionary vanguard. It also transfers lives, resources, and societal stability from targeted entities and civilian populations to the cause of the vanguard.
% ABSENT_VOICES: Classical jurisprudential scholars (ulema) who emphasize state authority, proportionality, and non-combatant immunity are excluded. Non-combatant civilians, who bear the brunt of the violence, are also excluded from the decision-making process that defines their collective guilt.
% DISAPPEARANCE_RATIONALE: If this reading vanished overnight, a major ideological justification for decentralized, non-state violence against Muslim rulers and occupiers would be removed. This would lead to a re-evaluation of legitimate authority, methods of resistance, and the role of traditional jurisprudence, significantly altering the landscape of political Islam and conflict.
% FOUNDING_PROBLEM: The perceived oppression, corruption, and apostasy of existing Muslim rulers, coupled with the presence of foreign occupiers, created a state of emergency for the global Muslim community (Ummah) that traditional, state-centric approaches to jihad could not address.
% FOUNDING_PROBLEM_CORROBORATION: The problem is attested as 'live' by revolutionary ideologues and their committed followers, who point to ongoing political grievances and foreign interventions. However, classical scholars and state authorities dispute this framing, arguing for alternative solutions, denying the premise of apostasy, or emphasizing the destructive consequences of this approach. Independent observers note the persistence of the grievances but contest the proposed solution.
narrative_ontology:disappearance_verdict(jihad_quranic_corpus__revolutionary_vanguard_reading, world_rearranges).
narrative_ontology:founding_problem_status(jihad_quranic_corpus__revolutionary_vanguard_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jihad_quranic_corpus__revolutionary_vanguard_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(jihad_quranic_corpus__revolutionary_vanguard_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jihad_quranic_corpus__revolutionary_vanguard_reading, 0.85, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jihad_quranic_corpus__revolutionary_vanguard_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(jihad_quranic_corpus__revolutionary_vanguard_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(jihad_quranic_corpus__revolutionary_vanguard_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is very high (0.85) because this reading directly challenges and seeks to overthrow existing political and religious authority, extracting their legitimacy, power, and physical presence. Suppression is also very high (0.90) as it actively suppresses any alternative interpretations of jihad, state authority, or peaceful resistance, often through violence and intimidation. Theater ratio is low (0.10) because the constraint is a direct call to action and violent confrontation, with little performative maintenance; its function is direct, not theatrical. Accessibility collapse is high (0.92) as the narrative frames the situation as an existential emergency, leaving little room for alternatives to immediate armed struggle. Resistance is high (0.88) because the constraint directly challenges powerful state and foreign actors, leading to intense and prolonged conflict.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the revolutionary vanguard and committed mujahidin, this constraint is a necessary act of liberation and religious duty, a 'rope' to coordinate righteous struggle. From the perspective of apostate rulers, occupying forces, and classical ulema, it is a 'snare' that destabilizes society, usurps legitimate authority, and causes immense suffering. The engine's classification as 'snare' reflects the structural reality of coercion and extraction, regardless of the claimed coordination function.
 *
 * DIRECTIONALITY LOGIC:
 *   The revolutionary vanguard leaders and committed mujahidin are beneficiaries, gaining power, legitimacy, and spiritual fulfillment from the constraint's operation (low d). Apostate rulers, occupying forces, and civilians in target areas are clear targets/victims, bearing the brunt of violence, loss of authority, and societal collapse (high d). Classical ulema are excluded, their authority undermined. The general Muslim populace is a diffuse victim, caught in the conflict's fallout.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification prevents mislabeling a highly extractive and suppressive ideology as mere coordination. While it claims to coordinate the Ummah's liberation, its reliance on takfir, bypassing state authority, and emergency jurisprudence creates a structure of coercion and victimhood that aligns with a snare. The 'live' status of the founding problem (perceived oppression) is contested, but even if live, the chosen 'solution' is structurally extractive, not coordinative.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    legitimacy_of_takfir,
    'Is the declaration of apostasy (takfir) against Muslim rulers and populations legitimate according to mainstream Islamic jurisprudence, or an extremist overreach?',
    'Consensus of recognized, independent Islamic legal scholars (ulema) on the application of takfir in contemporary contexts, or historical analysis of its use in similar situations.',
    'If takfir is deemed illegitimate, the entire ideological foundation of this reading collapses, reclassifying it from a ''snare'' (with a claimed coordination function) to a ''piton'' (ideological inertia) or even dissolving it as a constraint. If deemed legitimate, it reinforces the reading''s internal coherence, though not its ethical standing.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(legitimacy_of_takfir, conceptual, 'The foundational legitimacy of declaring apostasy to justify rebellion.').

omega_variable(
    emergency_jurisprudence_scope,
    'Is the state of emergency (darura) truly universal and severe enough to justify bypassing classical jurisprudential safeguards for jihad, including state authority and non-combatant immunity?',
    'Independent assessment of the objective conditions in Muslim lands against classical Islamic legal definitions of ''darura'' (necessity/emergency), and comparison with historical precedents.',
    'If the emergency is not universally recognized or is deemed insufficient to override classical rules, the justification for decentralized, indiscriminate violence weakens, potentially reclassifying the constraint towards a ''piton'' or ''tangled_rope'' as its coordination claims become more transparently extractive. If the emergency is corroborated, it strengthens the internal logic of the reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(emergency_jurisprudence_scope, empirical, 'The objective justification for emergency jurisprudence overriding classical rules.').

omega_variable(
    civilian_combatant_distinction,
    'Is the blurring of civilian and combatant lines, often justified by collective guilt or association with ''apostate'' regimes, consistent with Islamic ethical principles and international humanitarian law?',
    'Analysis of Islamic ethical texts and historical practice regarding non-combatant immunity, alongside comparative analysis with international humanitarian law principles.',
    'If the blurring of lines is deemed inconsistent, it exposes a core ethical flaw in the reading, increasing its effective extractiveness and suppression, and potentially leading to a reclassification as a ''snare'' with even less legitimate coordination function. If deemed consistent, it would reinforce the reading''s internal consistency, though not its external acceptance.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(civilian_combatant_distinction, conceptual, 'Ethical justification for targeting civilians based on collective guilt.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jihad_quranic_corpus__revolutionary_vanguard_reading, 1960, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jiha_tr_t1960, jihad_quranic_corpus__revolutionary_vanguard_reading, theater_ratio, 1960, 0.15).
narrative_ontology:measurement(jiha_tr_t1975, jihad_quranic_corpus__revolutionary_vanguard_reading, theater_ratio, 1975, 0.12).
narrative_ontology:measurement(jiha_tr_t1990, jihad_quranic_corpus__revolutionary_vanguard_reading, theater_ratio, 1990, 0.1).
narrative_ontology:measurement(jiha_tr_t2005, jihad_quranic_corpus__revolutionary_vanguard_reading, theater_ratio, 2005, 0.08).
narrative_ontology:measurement(jiha_tr_t2024, jihad_quranic_corpus__revolutionary_vanguard_reading, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(jiha_be_t1960, jihad_quranic_corpus__revolutionary_vanguard_reading, base_extractiveness, 1960, 0.6).
narrative_ontology:measurement(jiha_be_t1975, jihad_quranic_corpus__revolutionary_vanguard_reading, base_extractiveness, 1975, 0.7).
narrative_ontology:measurement(jiha_be_t1990, jihad_quranic_corpus__revolutionary_vanguard_reading, base_extractiveness, 1990, 0.8).
narrative_ontology:measurement(jiha_be_t2005, jihad_quranic_corpus__revolutionary_vanguard_reading, base_extractiveness, 2005, 0.88).
narrative_ontology:measurement(jiha_be_t2024, jihad_quranic_corpus__revolutionary_vanguard_reading, base_extractiveness, 2024, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(jiha_su_t1960, jihad_quranic_corpus__revolutionary_vanguard_reading, suppression_requirement, 1960, 0.65).
narrative_ontology:measurement(jiha_su_t1975, jihad_quranic_corpus__revolutionary_vanguard_reading, suppression_requirement, 1975, 0.75).
narrative_ontology:measurement(jiha_su_t1990, jihad_quranic_corpus__revolutionary_vanguard_reading, suppression_requirement, 1990, 0.85).
narrative_ontology:measurement(jiha_su_t2005, jihad_quranic_corpus__revolutionary_vanguard_reading, suppression_requirement, 2005, 0.92).
narrative_ontology:measurement(jiha_su_t2024, jihad_quranic_corpus__revolutionary_vanguard_reading, suppression_requirement, 2024, 0.9).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=1960, tn=2024
narrative_ontology:measurement(jiha_grid_01, jihad_quranic_corpus__revolutionary_vanguard_reading, accessibility_collapse(class), 1960, 0.25).
narrative_ontology:measurement(jiha_grid_02, jihad_quranic_corpus__revolutionary_vanguard_reading, accessibility_collapse(class), 2024, 0.7).
narrative_ontology:measurement(jiha_grid_03, jihad_quranic_corpus__revolutionary_vanguard_reading, accessibility_collapse(individual), 1960, 0.2).
narrative_ontology:measurement(jiha_grid_04, jihad_quranic_corpus__revolutionary_vanguard_reading, accessibility_collapse(individual), 2024, 0.9).
narrative_ontology:measurement(jiha_grid_05, jihad_quranic_corpus__revolutionary_vanguard_reading, accessibility_collapse(organizational), 1960, 0.3).
narrative_ontology:measurement(jiha_grid_06, jihad_quranic_corpus__revolutionary_vanguard_reading, accessibility_collapse(organizational), 2024, 0.85).
narrative_ontology:measurement(jiha_grid_07, jihad_quranic_corpus__revolutionary_vanguard_reading, accessibility_collapse(structural), 1960, 0.4).
narrative_ontology:measurement(jiha_grid_08, jihad_quranic_corpus__revolutionary_vanguard_reading, accessibility_collapse(structural), 2024, 0.92).
narrative_ontology:measurement(jiha_grid_09, jihad_quranic_corpus__revolutionary_vanguard_reading, resistance(class), 1960, 0.05).
narrative_ontology:measurement(jiha_grid_10, jihad_quranic_corpus__revolutionary_vanguard_reading, resistance(class), 2024, 0.75).
narrative_ontology:measurement(jiha_grid_11, jihad_quranic_corpus__revolutionary_vanguard_reading, resistance(individual), 1960, 0.1).
narrative_ontology:measurement(jiha_grid_12, jihad_quranic_corpus__revolutionary_vanguard_reading, resistance(individual), 2024, 0.8).
narrative_ontology:measurement(jiha_grid_13, jihad_quranic_corpus__revolutionary_vanguard_reading, resistance(organizational), 1960, 0.15).
narrative_ontology:measurement(jiha_grid_14, jihad_quranic_corpus__revolutionary_vanguard_reading, resistance(organizational), 2024, 0.88).
narrative_ontology:measurement(jiha_grid_15, jihad_quranic_corpus__revolutionary_vanguard_reading, resistance(structural), 1960, 0.1).
narrative_ontology:measurement(jiha_grid_16, jihad_quranic_corpus__revolutionary_vanguard_reading, resistance(structural), 2024, 0.82).
narrative_ontology:measurement(jiha_grid_17, jihad_quranic_corpus__revolutionary_vanguard_reading, stakes_inflation(class), 1960, 0.3).
narrative_ontology:measurement(jiha_grid_18, jihad_quranic_corpus__revolutionary_vanguard_reading, stakes_inflation(class), 2024, 0.8).
narrative_ontology:measurement(jiha_grid_19, jihad_quranic_corpus__revolutionary_vanguard_reading, stakes_inflation(individual), 1960, 0.4).
narrative_ontology:measurement(jiha_grid_20, jihad_quranic_corpus__revolutionary_vanguard_reading, stakes_inflation(individual), 2024, 0.95).
narrative_ontology:measurement(jiha_grid_21, jihad_quranic_corpus__revolutionary_vanguard_reading, stakes_inflation(organizational), 1960, 0.5).
narrative_ontology:measurement(jiha_grid_22, jihad_quranic_corpus__revolutionary_vanguard_reading, stakes_inflation(organizational), 2024, 0.9).
narrative_ontology:measurement(jiha_grid_23, jihad_quranic_corpus__revolutionary_vanguard_reading, stakes_inflation(structural), 1960, 0.35).
narrative_ontology:measurement(jiha_grid_24, jihad_quranic_corpus__revolutionary_vanguard_reading, stakes_inflation(structural), 2024, 0.85).
narrative_ontology:measurement(jiha_grid_25, jihad_quranic_corpus__revolutionary_vanguard_reading, suppression(class), 1960, 0.35).
narrative_ontology:measurement(jiha_grid_26, jihad_quranic_corpus__revolutionary_vanguard_reading, suppression(class), 2024, 0.8).
narrative_ontology:measurement(jiha_grid_27, jihad_quranic_corpus__revolutionary_vanguard_reading, suppression(individual), 1960, 0.3).
narrative_ontology:measurement(jiha_grid_28, jihad_quranic_corpus__revolutionary_vanguard_reading, suppression(individual), 2024, 0.85).
narrative_ontology:measurement(jiha_grid_29, jihad_quranic_corpus__revolutionary_vanguard_reading, suppression(organizational), 1960, 0.5).
narrative_ontology:measurement(jiha_grid_30, jihad_quranic_corpus__revolutionary_vanguard_reading, suppression(organizational), 2024, 0.9).
narrative_ontology:measurement(jiha_grid_31, jihad_quranic_corpus__revolutionary_vanguard_reading, suppression(structural), 1960, 0.6).
narrative_ontology:measurement(jiha_grid_32, jihad_quranic_corpus__revolutionary_vanguard_reading, suppression(structural), 2024, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jihad_quranic_corpus__revolutionary_vanguard_reading, identity_coordination).
narrative_ontology:affects_constraint(jihad_quranic_corpus__revolutionary_vanguard_reading, state_monopoly_on_violence_in_muslim_lands).
narrative_ontology:affects_constraint(jihad_quranic_corpus__revolutionary_vanguard_reading, classical_jihad_jurisprudence).
narrative_ontology:affects_constraint(jihad_quranic_corpus__revolutionary_vanguard_reading, islamic_non_combatant_immunity).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'jihad_quranic_corpus' kernel. It represents the revolutionary vanguard interpretation, which directly challenges and forecloses the premises of the defensive-spiritual and expansionist-legalist readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
