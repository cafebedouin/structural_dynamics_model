% ============================================================================
% CONSTRAINT STORY: july_charter_sovereign_legitimacy__military_custodian_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_july_charter_sovereign_legitimacy__military_custodian_reading, []).

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
 *   constraint_id: july_charter_sovereign_legitimacy__military_custodian_reading
 *   human_readable: July Charter: Military Custodian of Sovereign Legitimacy
 *   domain: constitutional_law/political_transitions/post_revolutionary_state_building
 *
 * SUMMARY:
 *   This constraint describes the 'military custodian' reading of the July
 *   Charter, where the military is enshrined as the ultimate guarantor of
 *   national stability, effectively subordinating civilian institutions. This
 *   reading is characterized by high extraction of political autonomy and
 *   severe suppression of dissent, with a significant theatrical component in
 *   maintaining the facade of civilian governance. The claimed type is
 *   'snare' because the coordination story (stability) is a cover for the
 *   military's extraction of power and resources, enforced through coercion
 *   and suppression of alternatives. This is one reading of the
 *   'july_charter_sovereign_legitimacy' kernel.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(july_charter_sovereign_legitimacy__military_custodian_reading, 0.85).
domain_priors:suppression_score(july_charter_sovereign_legitimacy__military_custodian_reading, 0.92).
domain_priors:theater_ratio(july_charter_sovereign_legitimacy__military_custodian_reading, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(july_charter_sovereign_legitimacy__military_custodian_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(july_charter_sovereign_legitimacy__military_custodian_reading, suppression_requirement, 0.92).
narrative_ontology:constraint_metric(july_charter_sovereign_legitimacy__military_custodian_reading, theater_ratio, 0.65).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(july_charter_sovereign_legitimacy__military_custodian_reading, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(july_charter_sovereign_legitimacy__military_custodian_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(july_charter_sovereign_legitimacy__military_custodian_reading, snare).
narrative_ontology:human_readable(july_charter_sovereign_legitimacy__military_custodian_reading, "July Charter: Military Custodian of Sovereign Legitimacy").
narrative_ontology:topic_domain(july_charter_sovereign_legitimacy__military_custodian_reading, "constitutional_law/political_transitions/post_revolutionary_state_building").

domain_priors:requires_active_enforcement(july_charter_sovereign_legitimacy__military_custodian_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(july_charter_sovereign_legitimacy__military_custodian_reading, 'e810e508-15b7-4a65-8d0d-09e45862da86').
narrative_ontology:cs_kernel_codification('e810e508-15b7-4a65-8d0d-09e45862da86', formalized).
narrative_ontology:cs_authority_grounding('e810e508-15b7-4a65-8d0d-09e45862da86', extraction).
narrative_ontology:cs_interpretation_layer_present('e810e508-15b7-4a65-8d0d-09e45862da86').
narrative_ontology:cs_reading_relation('e810e508-15b7-4a65-8d0d-09e45862da86', july_charter_sovereign_legitimacy__secular_democratic_reading, forecloses).
narrative_ontology:cs_reading_relation('e810e508-15b7-4a65-8d0d-09e45862da86', july_charter_sovereign_legitimacy__guided_nationalism_reading, coexists_with).
narrative_ontology:cs_axiom('e810e508-15b7-4a65-8d0d-09e45862da86', foundational, military_as_ultimate_guardian).
narrative_ontology:cs_axiom_status(military_as_ultimate_guardian, holdable).
narrative_ontology:cs_axiom_grounding('e810e508-15b7-4a65-8d0d-09e45862da86', military_as_ultimate_guardian, conventional).
narrative_ontology:cs_axiom('e810e508-15b7-4a65-8d0d-09e45862da86', foundational, stability_over_democracy).
narrative_ontology:cs_axiom_status(stability_over_democracy, holdable).
narrative_ontology:cs_axiom_grounding('e810e508-15b7-4a65-8d0d-09e45862da86', stability_over_democracy, instrumental).
narrative_ontology:cs_reference_frame('e810e508-15b7-4a65-8d0d-09e45862da86', post_revolutionary_military_intervention).
narrative_ontology:cs_drift_state('e810e508-15b7-4a65-8d0d-09e45862da86', contemporary_political_landscape, gap(stable, minor, true)).
narrative_ontology:cs_created_at('e810e508-15b7-4a65-8d0d-09e45862da86', '').
narrative_ontology:cs_kernel_id(july_charter_sovereign_legitimacy__military_custodian_reading, july_charter_sovereign_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(july_charter_sovereign_legitimacy__military_custodian_reading, military_high_command).
narrative_ontology:constraint_beneficiary(july_charter_sovereign_legitimacy__military_custodian_reading, state_security_apparatus).
narrative_ontology:constraint_victim(july_charter_sovereign_legitimacy__military_custodian_reading, autonomous_political_parties).
narrative_ontology:constraint_victim(july_charter_sovereign_legitimacy__military_custodian_reading, student_movement).
narrative_ontology:constraint_victim(july_charter_sovereign_legitimacy__military_custodian_reading, civil_society_organizations).
narrative_ontology:constraint_victim(july_charter_sovereign_legitimacy__military_custodian_reading, independent_media).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets the Charter as granting it ultimate authority to intervene in political affairs to ensure 'stability' and 'national security'. Benefits from extensive resources, immunity from civilian oversight, and control over key state functions. Exit options are effectively non-existent as they are the ultimate arbiter of the system.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__military_custodian_reading, military_high_command, agenda_setter,
    institutional, generational, arbitrage, national).

% Operates under the military's authority, enforcing its interpretations of the Charter. Benefits from expanded powers, lack of accountability, and a mandate to suppress dissent. Its existence is tied to the military's role as custodian.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__military_custodian_reading, state_security_apparatus, beneficiary,
    institutional, biographical, constrained, national).

% Are formally permitted but operate under constant threat of dissolution or intervention by the military. Their political activities are bounded by the security apparatus's interpretation of the Charter, limiting their ability to challenge the military's role. Exit means ceasing to exist as a political force.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__military_custodian_reading, autonomous_political_parties, payer,
    moderate, immediate, trapped, national).

% Faces severe repression for any activism perceived as challenging the military's authority. Their demands for greater freedoms and civilian rule are systematically suppressed. Exit means abandoning their political aspirations and civic engagement.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__military_custodian_reading, student_movement, payer,
    powerless, immediate, trapped, local).

% Operate within narrow confines, with their activities subject to state approval and surveillance. Advocacy for human rights or democratic reforms is often met with harassment or closure. Their ability to influence policy is severely limited by the military's overarching authority.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__military_custodian_reading, civil_society_organizations, payer,
    moderate, biographical, constrained, national).

% Faces censorship, arrests of journalists, and closure for reporting that challenges the official narrative or criticizes the military. Their ability to inform the public and hold power accountable is severely curtailed. Exit means self-censorship or ceasing operations.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__military_custodian_reading, independent_media, payer,
    moderate, immediate, constrained, national).

% Monitor human rights abuses and democratic backsliding, issuing reports and condemnations. Their influence is limited by the military's internal focus and its ability to deflect external pressure.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__military_custodian_reading, international_observers, observer,
    institutional, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a framework for 'stability' and 'national security' by centralizing ultimate authority in the military, ostensibly preventing political fragmentation and external interference.
% TRANSFER_FUNCTION: Transfers political power, civilian oversight, and public resources to the military and security apparatus, in exchange for a claimed guarantee of stability. It extracts autonomy and freedom from political parties, civil society, and media.
% ABSENT_VOICES: Exiled opposition leaders, human rights activists, and a significant portion of the population who desire genuine civilian rule are excluded from the political discourse. They would advocate for a fully democratic, civilian-led state.
% DISAPPEARANCE_RATIONALE: If the military's constitutional role as custodian vanished overnight, there would be an immediate power vacuum, likely leading to intense political contestation, demands for new elections, and a fundamental restructuring of state-society relations. Civilian institutions would assert supremacy, and the security apparatus would face calls for accountability.
% FOUNDING_PROBLEM: The Charter was established in the aftermath of a period of political instability and perceived threats to national unity, aiming to prevent a return to chaos and ensure a strong, unified state.
% FOUNDING_PROBLEM_CORROBORATION: The military and its supporters claim the founding problem of instability is still live, justifying their continued role. Autonomous political parties, student movements, and international observers argue that the initial instability has been replaced by military entrenchment, and the 'problem' now serves as a pretext for maintaining power; their corroboration comes from independent human rights reports and analyses of political repression.
narrative_ontology:disappearance_verdict(july_charter_sovereign_legitimacy__military_custodian_reading, world_rearranges).
narrative_ontology:founding_problem_status(july_charter_sovereign_legitimacy__military_custodian_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(july_charter_sovereign_legitimacy__military_custodian_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(july_charter_sovereign_legitimacy__military_custodian_reading, 'none', 1).
narrative_ontology:epsilon_provenance(july_charter_sovereign_legitimacy__military_custodian_reading, 0.85, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(july_charter_sovereign_legitimacy__military_custodian_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(july_charter_sovereign_legitimacy__military_custodian_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(july_charter_sovereign_legitimacy__military_custodian_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.85) because the military's role allows it to appropriate significant state resources and political power without accountability. Suppression is extremely high (0.92) due to the active and often violent repression of any challenge to the military's authority, including arrests, censorship, and dissolution of organizations. Theater ratio is substantial (0.65) as civilian institutions exist but are largely performative, lacking genuine power to challenge the military's veto. Accessibility collapse is high (0.78) as viable alternatives to the military-dominated political system are systematically eliminated. Resistance is also high (0.70) reflecting ongoing, though suppressed, efforts by various groups to challenge the military's role.
 *
 * PERSPECTIVAL GAP:
 *   From the military's perspective, the Charter is a 'rope' or even a 'mountain' – a necessary, natural arrangement for national survival. From the perspective of political parties and civil society, it is a 'snare' designed to perpetuate military rule. The engine's classification will reflect the latter due to the high extractiveness and suppression, despite the military's self-justifying narrative.
 *
 * DIRECTIONALITY LOGIC:
 *   The military high command and state security apparatus are clear beneficiaries (d near 0.0), as they directly gain power, resources, and immunity. Autonomous political parties, student movements, civil society organizations, and independent media are targets (d near 1.0), as they bear the brunt of suppression and extraction of their autonomy. International observers are analytical, neither directly benefiting nor being targeted by the constraint's operation.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint prevents mislabeling by highlighting the divergence between the claimed coordination function (stability) and the actual operation (extraction and suppression). The 'founding_problem_status' being 'contested' further indicates that the original mandate may have atrophied, with the constraint persisting due to the beneficiaries' interest in maintaining power rather than solving a genuine, ongoing collective-action problem.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    military_role_naturalness,
    'Is the military''s role as ultimate guarantor of stability a natural, inevitable feature of the state''s political landscape, or a constructed arrangement serving specific interests?',
    'Comparative analysis with other post-revolutionary states that successfully transitioned to full civilian rule, examining the specific historical and structural factors that enabled or prevented military disengagement.',
    'If natural, the constraint might be reclassified closer to a Mountain (though still extractive due to beneficiaries). If constructed, it reinforces the Snare classification, highlighting the agency in its perpetuation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(military_role_naturalness, conceptual, 'Ambiguity regarding the ''naturalness'' of the military''s constitutional role.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (legal barriers, physical force) or internalized (fear, self-censorship by victims)?',
    'Post-intervention trajectory: if external suppressive mechanisms are removed (e.g., by international pressure), does resistance immediately surge, or does a period of internalized suppression persist before full political mobilization?',
    'If internalized suppression is significant, the constraint''s effective suppression is higher than the structural measure suggests, as victims carry the suppression with them even after external barriers are weakened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for political actors.').

omega_variable(
    civilian_institutions_agency,
    'To what extent do civilian institutions (parliament, judiciary) possess latent agency that could challenge the military''s authority, despite the Charter''s provisions?',
    'Analysis of historical instances of civilian pushback or judicial rulings that subtly reinterpreted the Charter, even if ultimately overridden. Examination of internal divisions within civilian elites regarding the military''s role.',
    'If significant latent agency exists, the ''theater_ratio'' might be lower, and the ''snare'' classification might have a more ''tangled_rope'' aspect, indicating a more complex, albeit unequal, power dynamic.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(civilian_institutions_agency, empirical, 'The true extent of agency within civilian institutions.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(july_charter_sovereign_legitimacy__military_custodian_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(july_tr_t0, july_charter_sovereign_legitimacy__military_custodian_reading, theater_ratio, 0, 0.5).
narrative_ontology:measurement(july_tr_t5, july_charter_sovereign_legitimacy__military_custodian_reading, theater_ratio, 5, 0.55).
narrative_ontology:measurement(july_tr_t10, july_charter_sovereign_legitimacy__military_custodian_reading, theater_ratio, 10, 0.6).
narrative_ontology:measurement(july_tr_t15, july_charter_sovereign_legitimacy__military_custodian_reading, theater_ratio, 15, 0.63).
narrative_ontology:measurement(july_tr_t20, july_charter_sovereign_legitimacy__military_custodian_reading, theater_ratio, 20, 0.65).

% Extraction over time
narrative_ontology:measurement(july_be_t0, july_charter_sovereign_legitimacy__military_custodian_reading, base_extractiveness, 0, 0.75).
narrative_ontology:measurement(july_be_t5, july_charter_sovereign_legitimacy__military_custodian_reading, base_extractiveness, 5, 0.8).
narrative_ontology:measurement(july_be_t10, july_charter_sovereign_legitimacy__military_custodian_reading, base_extractiveness, 10, 0.83).
narrative_ontology:measurement(july_be_t15, july_charter_sovereign_legitimacy__military_custodian_reading, base_extractiveness, 15, 0.84).
narrative_ontology:measurement(july_be_t20, july_charter_sovereign_legitimacy__military_custodian_reading, base_extractiveness, 20, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(july_su_t0, july_charter_sovereign_legitimacy__military_custodian_reading, suppression_requirement, 0, 0.8).
narrative_ontology:measurement(july_su_t5, july_charter_sovereign_legitimacy__military_custodian_reading, suppression_requirement, 5, 0.85).
narrative_ontology:measurement(july_su_t10, july_charter_sovereign_legitimacy__military_custodian_reading, suppression_requirement, 10, 0.89).
narrative_ontology:measurement(july_su_t15, july_charter_sovereign_legitimacy__military_custodian_reading, suppression_requirement, 15, 0.91).
narrative_ontology:measurement(july_su_t20, july_charter_sovereign_legitimacy__military_custodian_reading, suppression_requirement, 20, 0.92).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(july_charter_sovereign_legitimacy__military_custodian_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(july_charter_sovereign_legitimacy__military_custodian_reading, july_charter_sovereign_legitimacy__secular_democratic_reading).
narrative_ontology:affects_constraint(july_charter_sovereign_legitimacy__military_custodian_reading, july_charter_sovereign_legitimacy__guided_nationalism_reading).
narrative_ontology:affects_constraint(july_charter_sovereign_legitimacy__military_custodian_reading, freedom_of_assembly_restrictions).
narrative_ontology:affects_constraint(july_charter_sovereign_legitimacy__military_custodian_reading, media_censorship_laws).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'july_charter_sovereign_legitimacy' kernel. This 'military_custodian_reading' emphasizes the military's permanent role, contrasting with the 'secular_democratic_reading' (civilian supremacy) and the 'guided_nationalism_reading' (Islamic-nationalist framework). Each reading represents a distinct structural constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
