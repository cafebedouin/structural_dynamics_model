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
 *   human_readable: July Charter: Military as Sovereign Custodian
 *   domain: constitutional_law/political_transitions/post_revolutionary_state_building
 *
 * SUMMARY:
 *   This constraint describes the 'military custodian' reading of the July
 *   Charter, where the military is ratified as a permanent institutional
 *   guardian ensuring stability. This reading subordinates civilian
 *   institutions to military veto authority and bounds political contestation
 *   by the security apparatus. It is one of three competing readings of the
 *   same kernel, 'july_charter_sovereign_legitimacy'. The victim set includes
 *   autonomous political parties, the student movement, and civilian
 *   government officials. The claimed type is 'snare' because the
 *   coordination story (stability) is cover for substantial, actively
 *   enforced extraction of political power and resources by the military.
 *
 * KEY AGENTS:
 *   - military_high_command: Primary beneficiary/agenda_setter (institutional/arbitrage)
 *   - state_security_apparatus: Beneficiary (institutional/constrained)
 *   - civilian_government_officials: Primary target (moderate/identity_locked)
 *   - autonomous_political_parties: Primary target (powerless/trapped)
 *   - student_movement: Primary target (powerless/trapped)
 *   - human_rights_advocates: Secondary target (moderate/constrained)
 *   - international_observers: Analytical observer (institutional/analytical)
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
narrative_ontology:constraint_metric(july_charter_sovereign_legitimacy__military_custodian_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(july_charter_sovereign_legitimacy__military_custodian_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(july_charter_sovereign_legitimacy__military_custodian_reading, snare).
narrative_ontology:human_readable(july_charter_sovereign_legitimacy__military_custodian_reading, "July Charter: Military as Sovereign Custodian").
narrative_ontology:topic_domain(july_charter_sovereign_legitimacy__military_custodian_reading, "constitutional_law/political_transitions/post_revolutionary_state_building").

domain_priors:requires_active_enforcement(july_charter_sovereign_legitimacy__military_custodian_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(july_charter_sovereign_legitimacy__military_custodian_reading, '85caf707-6502-4588-9a2d-e61770d4bd81').
narrative_ontology:cs_kernel_codification('85caf707-6502-4588-9a2d-e61770d4bd81', formalized).
narrative_ontology:cs_authority_grounding('85caf707-6502-4588-9a2d-e61770d4bd81', extraction).
narrative_ontology:cs_interpretation_layer_present('85caf707-6502-4588-9a2d-e61770d4bd81').
narrative_ontology:cs_reading_relation('85caf707-6502-4588-9a2d-e61770d4bd81', july_charter_sovereign_legitimacy__secular_democratic_reading, forecloses).
narrative_ontology:cs_reading_relation('85caf707-6502-4588-9a2d-e61770d4bd81', july_charter_sovereign_legitimacy__guided_nationalism_reading, coexists_with).
narrative_ontology:cs_axiom('85caf707-6502-4588-9a2d-e61770d4bd81', foundational, military_as_ultimate_guardian).
narrative_ontology:cs_axiom_status(military_as_ultimate_guardian, holdable).
narrative_ontology:cs_axiom_grounding('85caf707-6502-4588-9a2d-e61770d4bd81', military_as_ultimate_guardian, conventional).
narrative_ontology:cs_axiom('85caf707-6502-4588-9a2d-e61770d4bd81', foundational, stability_over_democracy).
narrative_ontology:cs_axiom_status(stability_over_democracy, holdable).
narrative_ontology:cs_axiom_grounding('85caf707-6502-4588-9a2d-e61770d4bd81', stability_over_democracy, instrumental).
narrative_ontology:cs_reference_frame('85caf707-6502-4588-9a2d-e61770d4bd81', post_revolutionary_military_stabilization).
narrative_ontology:cs_drift_state('85caf707-6502-4588-9a2d-e61770d4bd81', contemporary_entrenchment_era, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('85caf707-6502-4588-9a2d-e61770d4bd81', '').
narrative_ontology:cs_kernel_id(july_charter_sovereign_legitimacy__military_custodian_reading, july_charter_sovereign_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(july_charter_sovereign_legitimacy__military_custodian_reading, military_high_command).
narrative_ontology:constraint_beneficiary(july_charter_sovereign_legitimacy__military_custodian_reading, state_security_apparatus).
narrative_ontology:constraint_victim(july_charter_sovereign_legitimacy__military_custodian_reading, autonomous_political_parties).
narrative_ontology:constraint_victim(july_charter_sovereign_legitimacy__military_custodian_reading, student_movement).
narrative_ontology:constraint_victim(july_charter_sovereign_legitimacy__military_custodian_reading, civilian_government_officials).
narrative_ontology:constraint_victim(july_charter_sovereign_legitimacy__military_custodian_reading, human_rights_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets the Charter as granting it ultimate authority to intervene in political affairs to ensure 'stability' and 'national security'. Benefits from unchecked power, control over state resources, and immunity from civilian oversight. Actively suppresses dissent and maintains a pervasive security apparatus.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__military_custodian_reading, military_high_command, agenda_setter,
    institutional, generational, arbitrage, national).

% Operates under the military's authority, benefiting from expanded powers, resources, and legal protections. Enforces the military's interpretation of the Charter, conducting surveillance, arrests, and suppression of opposition. Its existence is tied to the military's role as custodian.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__military_custodian_reading, state_security_apparatus, beneficiary,
    institutional, biographical, constrained, national).

% Hold nominal positions of power but are ultimately subordinate to the military's veto and oversight. Their policy decisions are constrained, and their tenure is precarious. They are identity-locked by their professional roles and the perceived necessity of maintaining a semblance of civilian rule.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__military_custodian_reading, civilian_government_officials, payer,
    moderate, immediate, identity_locked, national).

% Are permitted to exist but operate under severe restrictions, facing arbitrary arrests, bans, and surveillance. Their ability to mobilize or contest military authority is systematically suppressed. Exit means abandoning political aspirations or facing exile/imprisonment.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__military_custodian_reading, autonomous_political_parties, payer,
    powerless, biographical, trapped, national).

% A key force in the initial revolution, now a primary target of military suppression. Their protests are violently dispersed, leaders arrested, and organizations infiltrated. Exit means abandoning their ideals and accepting the military's authority.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__military_custodian_reading, student_movement, payer,
    powerless, immediate, trapped, local).

% Document abuses and advocate for civilian rule, but face harassment, legal persecution, and limited domestic impact. Their work is critical for international awareness but carries high personal risk. Exit means abandoning their mission.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__military_custodian_reading, human_rights_advocates, payer,
    moderate, generational, constrained, global).

% Monitor the political situation, issue reports, and apply diplomatic pressure. Their influence is limited by the military's internal control and claims of sovereign prerogative. They provide an external analytical perspective on the constraint's operation.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__military_custodian_reading, international_observers, observer,
    institutional, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Claims to coordinate national stability and security by providing a 'guardian' institution above partisan politics, preventing civil unrest and external interference.
% TRANSFER_FUNCTION: Transfers ultimate political authority, control over state resources, and the right to define 'national interest' from civilian institutions to the military high command, in exchange for a claimed guarantee of stability.
% ABSENT_VOICES: The original revolutionary councils and civilian leaders who envisioned a fully democratic transition are now largely silenced or imprisoned; they would argue for military subordination to elected authority. International legal bodies and human rights organizations are present but their recommendations are routinely ignored.
% DISAPPEARANCE_RATIONALE: If the military's constitutional role as custodian vanished overnight, the political landscape would immediately open up. Autonomous political parties and the student movement would rapidly re-mobilize, civilian institutions would assert full authority, and the state security apparatus would face immediate restructuring and accountability demands. The entire power structure would be reconfigured.
% FOUNDING_PROBLEM: The Charter was established in the aftermath of a revolution to prevent a return to authoritarianism and ensure national unity amidst deep political divisions and external threats.
% FOUNDING_PROBLEM_CORROBORATION: The military high command and state security apparatus assert the founding problem (instability, external threats) is still live and requires their continued custodianship. Autonomous political parties, the student movement, and human rights advocates contend the problem has been superseded by the military's own consolidation of power, and that the Charter now serves to perpetuate a new form of authoritarianism. Independent historians and political analysts corroborate the shift from a genuine post-revolutionary stabilization effort to a mechanism for military entrenchment.
narrative_ontology:disappearance_verdict(july_charter_sovereign_legitimacy__military_custodian_reading, world_rearranges).
narrative_ontology:founding_problem_status(july_charter_sovereign_legitimacy__military_custodian_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(july_charter_sovereign_legitimacy__military_custodian_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
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
 *   Extractiveness is very high (0.85) as the military seizes ultimate political power and resources, effectively nullifying civilian sovereignty. Suppression is extreme (0.92) due to pervasive surveillance, arrests, and violent crackdowns on dissent, making alternatives nearly impossible. Theater ratio is high (0.65) because while a civilian government exists, its functions are largely performative, masking the military's true control. The rising trend in extractiveness, suppression, and theater over the interval reflects the military's increasing entrenchment and the erosion of any genuine coordination function.
 *
 * PERSPECTIVAL GAP:
 *   From the military's perspective, this is a necessary 'rope' or even 'mountain' for national survival, ensuring stability. From the perspective of political parties, students, and human rights advocates, it is a 'snare' that actively extracts their rights and autonomy. Civilian government officials experience it as a 'tangled rope' – they are nominally coordinated into governance but pay a heavy price in terms of real power.
 *
 * DIRECTIONALITY LOGIC:
 *   The military high command and state security apparatus are full beneficiaries (d near 0.0) as they gain power and resources. Autonomous political parties, the student movement, and human rights advocates are full targets (d near 1.0) as their political agency and rights are systematically extracted. Civilian government officials are targets (d near 0.8) due to their constrained power and identity-locked exit options.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as a 'snare' prevents mislabeling this as coordination. The military's claim of ensuring stability (a coordination function) is demonstrably a cover for its own extraction of power. The 'contested' status of the founding problem and the 'world_rearranges' disappearance verdict, combined with high extractiveness and suppression, strongly indicate a snare, not a rope or scaffold. The rising theater ratio further confirms that the performative aspects of civilian rule increasingly mask the underlying extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    military_intent_ambiguity,
    'Is the military''s primary intent genuinely national stability, or is it self-preservation and power consolidation?',
    'Analysis of military spending priorities (defense vs. internal security/economic ventures), and the military''s response to genuine external threats vs. internal political dissent.',
    'If primarily self-preservation, the ''snare'' classification is reinforced; if genuinely stability-focused, it might lean towards a ''tangled_rope'' with high extraction as a cost of coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(military_intent_ambiguity, empirical, 'Ambiguity of military''s core motivation.').

omega_variable(
    civilian_identity_lock_strength,
    'How strong is the identity-lock on civilian government officials? Would they genuinely exit if the costs of compliance became too high, or is their identity fused with the state apparatus?',
    'Observation of defection rates or open resistance from civilian officials under increased military pressure, or the formation of alternative civilian leadership structures.',
    'If the identity-lock is weak, their effective extraction is lower due to more viable exit options; if strong, their effective extraction is higher, reinforcing their ''payer'' role.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(civilian_identity_lock_strength, empirical, 'Strength of identity-lock on civilian officials.').

omega_variable(
    charter_interpretive_flexibility,
    'How much interpretive flexibility does the Charter genuinely allow for alternative readings, given the military''s enforcement power?',
    'Analysis of judicial rulings on constitutional challenges, and the military''s response to attempts to re-interpret the Charter in civilian-centric ways.',
    'If flexibility is minimal, the military custodian reading is more entrenched; if some flexibility exists, it offers a potential pathway for ''rope'' or ''scaffold'' reclassification in the long term.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(charter_interpretive_flexibility, conceptual, 'Degree to which the Charter''s text permits alternative interpretations.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(july_charter_sovereign_legitimacy__military_custodian_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(july_tr_t0, july_charter_sovereign_legitimacy__military_custodian_reading, theater_ratio, 0, 0.3).
narrative_ontology:measurement(july_tr_t5, july_charter_sovereign_legitimacy__military_custodian_reading, theater_ratio, 5, 0.45).
narrative_ontology:measurement(july_tr_t10, july_charter_sovereign_legitimacy__military_custodian_reading, theater_ratio, 10, 0.55).
narrative_ontology:measurement(july_tr_t15, july_charter_sovereign_legitimacy__military_custodian_reading, theater_ratio, 15, 0.6).
narrative_ontology:measurement(july_tr_t20, july_charter_sovereign_legitimacy__military_custodian_reading, theater_ratio, 20, 0.65).

% Extraction over time
narrative_ontology:measurement(july_be_t0, july_charter_sovereign_legitimacy__military_custodian_reading, base_extractiveness, 0, 0.6).
narrative_ontology:measurement(july_be_t5, july_charter_sovereign_legitimacy__military_custodian_reading, base_extractiveness, 5, 0.7).
narrative_ontology:measurement(july_be_t10, july_charter_sovereign_legitimacy__military_custodian_reading, base_extractiveness, 10, 0.8).
narrative_ontology:measurement(july_be_t15, july_charter_sovereign_legitimacy__military_custodian_reading, base_extractiveness, 15, 0.83).
narrative_ontology:measurement(july_be_t20, july_charter_sovereign_legitimacy__military_custodian_reading, base_extractiveness, 20, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(july_su_t0, july_charter_sovereign_legitimacy__military_custodian_reading, suppression_requirement, 0, 0.7).
narrative_ontology:measurement(july_su_t5, july_charter_sovereign_legitimacy__military_custodian_reading, suppression_requirement, 5, 0.8).
narrative_ontology:measurement(july_su_t10, july_charter_sovereign_legitimacy__military_custodian_reading, suppression_requirement, 10, 0.88).
narrative_ontology:measurement(july_su_t15, july_charter_sovereign_legitimacy__military_custodian_reading, suppression_requirement, 15, 0.9).
narrative_ontology:measurement(july_su_t20, july_charter_sovereign_legitimacy__military_custodian_reading, suppression_requirement, 20, 0.92).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(july_charter_sovereign_legitimacy__military_custodian_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(july_charter_sovereign_legitimacy__military_custodian_reading, july_charter_sovereign_legitimacy__secular_democratic_reading).
narrative_ontology:affects_constraint(july_charter_sovereign_legitimacy__military_custodian_reading, july_charter_sovereign_legitimacy__guided_nationalism_reading).
narrative_ontology:affects_constraint(july_charter_sovereign_legitimacy__military_custodian_reading, freedom_of_assembly_restrictions).
narrative_ontology:affects_constraint(july_charter_sovereign_legitimacy__military_custodian_reading, political_party_registration_laws).

% DUAL FORMULATION NOTE:
% This constraint is the 'military custodian' reading of the 'july_charter_sovereign_legitimacy' kernel. It competes with 'secular_democratic_reading' and 'guided_nationalism_reading' for interpretive dominance over the Charter's meaning and the nation's foundational legitimacy.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
