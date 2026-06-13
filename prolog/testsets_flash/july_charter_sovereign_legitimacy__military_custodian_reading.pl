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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: july_charter_sovereign_legitimacy__military_custodian_reading
 *   human_readable: July Charter: Military Custodian Reading
 *   domain: constitutional_law/political_transitions/state_building
 *
 * SUMMARY:
 *   This constraint describes the 'military custodian' reading of a
 *   post-revolutionary national charter, where the military is ratified as
 *   the permanent institutional guardian of stability. This interpretation
 *   subordinates civilian institutions to military veto authority and bounds
 *   political contestation by the security apparatus. It is one of several
 *   contested readings of the same foundational document. The constraint is
 *   claimed as a Snare due to its high extraction and suppression, targeting
 *   autonomous political parties and student movements.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(july_charter_sovereign_legitimacy__military_custodian_reading, 0.85).
domain_priors:suppression_score(july_charter_sovereign_legitimacy__military_custodian_reading, 0.92).
domain_priors:theater_ratio(july_charter_sovereign_legitimacy__military_custodian_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(july_charter_sovereign_legitimacy__military_custodian_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(july_charter_sovereign_legitimacy__military_custodian_reading, suppression_requirement, 0.92).
narrative_ontology:constraint_metric(july_charter_sovereign_legitimacy__military_custodian_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(july_charter_sovereign_legitimacy__military_custodian_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(july_charter_sovereign_legitimacy__military_custodian_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(july_charter_sovereign_legitimacy__military_custodian_reading, snare).
narrative_ontology:human_readable(july_charter_sovereign_legitimacy__military_custodian_reading, "July Charter: Military Custodian Reading").
narrative_ontology:topic_domain(july_charter_sovereign_legitimacy__military_custodian_reading, "constitutional_law/political_transitions/state_building").

domain_priors:requires_active_enforcement(july_charter_sovereign_legitimacy__military_custodian_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(july_charter_sovereign_legitimacy__military_custodian_reading, '96702cd8-c95c-43b5-8936-ad45596820fc').
narrative_ontology:cs_kernel_codification('96702cd8-c95c-43b5-8936-ad45596820fc', fixed_text).
narrative_ontology:cs_authority_grounding('96702cd8-c95c-43b5-8936-ad45596820fc', extraction).
narrative_ontology:cs_interpretation_layer_present('96702cd8-c95c-43b5-8936-ad45596820fc').
narrative_ontology:cs_reading_relation('96702cd8-c95c-43b5-8936-ad45596820fc', july_charter_sovereign_legitimacy__secular_democratic_reading, forecloses).
narrative_ontology:cs_reading_relation('96702cd8-c95c-43b5-8936-ad45596820fc', july_charter_sovereign_legitimacy__guided_nationalism_reading, coexists_with).
narrative_ontology:cs_axiom('96702cd8-c95c-43b5-8936-ad45596820fc', foundational, military_as_ultimate_guardian_of_stability).
narrative_ontology:cs_axiom_status(military_as_ultimate_guardian_of_stability, holdable).
narrative_ontology:cs_axiom_grounding('96702cd8-c95c-43b5-8936-ad45596820fc', military_as_ultimate_guardian_of_stability, conventional).
narrative_ontology:cs_axiom('96702cd8-c95c-43b5-8936-ad45596820fc', secondary, civilian_subordination_to_national_security).
narrative_ontology:cs_axiom_status(civilian_subordination_to_national_security, holdable).
narrative_ontology:cs_axiom_grounding('96702cd8-c95c-43b5-8936-ad45596820fc', civilian_subordination_to_national_security, instrumental).
narrative_ontology:cs_reference_frame('96702cd8-c95c-43b5-8936-ad45596820fc', post_revolutionary_military_intervention).
narrative_ontology:cs_drift_state('96702cd8-c95c-43b5-8936-ad45596820fc', contemporary_political_landscape, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('96702cd8-c95c-43b5-8936-ad45596820fc', '').
narrative_ontology:cs_kernel_id(july_charter_sovereign_legitimacy__military_custodian_reading, july_charter_sovereign_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(july_charter_sovereign_legitimacy__military_custodian_reading, military_high_command).
narrative_ontology:constraint_beneficiary(july_charter_sovereign_legitimacy__military_custodian_reading, state_security_apparatus).
narrative_ontology:constraint_victim(july_charter_sovereign_legitimacy__military_custodian_reading, autonomous_political_parties).
narrative_ontology:constraint_victim(july_charter_sovereign_legitimacy__military_custodian_reading, student_movement).
narrative_ontology:constraint_victim(july_charter_sovereign_legitimacy__military_custodian_reading, civilian_government_officials).
narrative_ontology:constraint_victim(july_charter_sovereign_legitimacy__military_custodian_reading, human_rights_activists).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets the Charter as granting it ultimate authority to intervene in politics to ensure 'stability' and 'national security'. Benefits from unchecked power, control over state resources, and immunity from civilian oversight. Actively suppresses dissent and maintains a veto over civilian legislation.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__military_custodian_reading, military_high_command, agenda_setter,
    institutional, generational, arbitrage, national).

% Operates under the military's authority, benefiting from expanded powers of surveillance, arrest, and detention without accountability. Its existence and methods are legitimized by the military's 'custodian' role, ensuring its budget and operational freedom.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__military_custodian_reading, state_security_apparatus, beneficiary,
    institutional, generational, constrained, national).

% Hold nominal positions of power but operate under the constant threat of military intervention or dismissal. Their legislative and executive actions are subject to military approval or veto, limiting their autonomy and policy scope. They bear the cost of political impotence and public distrust.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__military_custodian_reading, civilian_government_officials, payer,
    moderate, biographical, constrained, national).

% Are permitted to exist but face severe restrictions on their activities, including arbitrary arrests of members, bans on public gatherings, and censorship. Their ability to contest elections or influence policy is severely curtailed by the military's overarching authority.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__military_custodian_reading, autonomous_political_parties, payer,
    powerless, biographical, trapped, national).

% Routinely targeted for protesting military rule and advocating for democratic reforms. Members face detention, violence, and academic expulsion. Their collective action is met with overwhelming force, making organized resistance extremely costly.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__military_custodian_reading, student_movement, payer,
    powerless, immediate, trapped, local).

% Document abuses and advocate for international pressure, but face severe repression, including imprisonment, torture, and forced exile. Their work is criminalized under 'national security' laws, making their existence precarious.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__military_custodian_reading, human_rights_activists, payer,
    powerless, biographical, trapped, national).

% Monitor the political situation, issue reports, and sometimes impose sanctions, but have limited direct power to alter the military's interpretation or enforcement of the Charter. Their influence is primarily diplomatic and informational.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__military_custodian_reading, international_observers, observer,
    institutional, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Claims to provide national stability and prevent political fragmentation by centralizing ultimate authority in the military, thereby coordinating security and governance functions during a post-revolutionary transition.
% TRANSFER_FUNCTION: Transfers sovereign authority and control over state resources from civilian institutions to the military, in exchange for a claimed guarantee of stability and order.
% ABSENT_VOICES: Exiled political leaders, suppressed opposition parties, and independent media would object, arguing that the Charter has been reinterpreted to legitimize authoritarian rule and that genuine democratic participation is systematically denied.
% DISAPPEARANCE_RATIONALE: If the military's 'custodian' role vanished, the entire political structure would collapse. Civilian institutions would immediately assert full authority, suppressed political parties would re-emerge, and the security apparatus would face demands for accountability. The state's power dynamics would fundamentally reorganize.
% FOUNDING_PROBLEM: The nation faced severe instability, political infighting, and the threat of civil war following a revolution, necessitating a strong, unifying force to restore order and guide the transition.
% FOUNDING_PROBLEM_CORROBORATION: The military and its allies assert the problem is still live, citing ongoing regional threats and internal dissent. Autonomous political parties, student movements, and international human rights organizations contend that the initial instability has been overcome, and the military's continued role is now a source of instability and repression, not a solution. Independent historical analysis and human rights reports corroborate the latter view.
narrative_ontology:disappearance_verdict(july_charter_sovereign_legitimacy__military_custodian_reading, world_rearranges).
narrative_ontology:founding_problem_status(july_charter_sovereign_legitimacy__military_custodian_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(july_charter_sovereign_legitimacy__military_custodian_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(july_charter_sovereign_legitimacy__military_custodian_reading, 'none', 1).

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
 *   Extractiveness is very high (0.85) as the military appropriates sovereign power and resources without accountability. Suppression is extreme (0.92) due to active enforcement against any challenge to military authority, including arrests, censorship, and bans on political activity. Theater ratio is moderate (0.4): while the military performs a 'stability' function, a significant portion of its activity is dedicated to maintaining its own power and suppressing legitimate political expression. The metrics show a clear trend of increasing extraction and suppression over time, indicating an entrenchment of military control.
 *
 * PERSPECTIVAL GAP:
 *   From the military high command's perspective, this is a necessary 'Rope' or even 'Mountain' for national survival, ensuring order. From the perspective of civilian government officials, political parties, and activists, it is a 'Snare' that actively extracts their autonomy and suppresses their rights. The engine's classification will reflect this divergence based on the declared structural relationships and metrics.
 *
 * DIRECTIONALITY LOGIC:
 *   The military high command and state security apparatus are clear beneficiaries (d near 0.0), as they gain unchecked power and resources. Civilian government officials, autonomous political parties, the student movement, and human rights activists are direct targets (d near 1.0), bearing the full cost of suppressed political freedom and personal risk. International observers are analytical (d=0.5), neither directly benefiting nor paying, but analyzing the constraint's impact.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate (ensuring stability post-revolution) is contested. While initially framed as a temporary Scaffold, the military custodian reading has allowed it to persist as a Snare, where the 'stability' narrative serves as cover for permanent extraction of political power. The rising extractiveness and suppression over time indicate a clear shift from any initial coordination function to pure extraction, preventing the constraint from being mislabeled as a Rope or a degraded Piton.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    military_custodian_vs_secular_democratic_reading,
    'Is the Charter''s text genuinely ambiguous, allowing for the military custodian reading, or is this reading a deliberate reinterpretation that contradicts the original intent?',
    'Historical analysis of the Charter''s drafting process, comparison with contemporary constitutional documents, and legal scholarly consensus on original intent.',
    'If a deliberate reinterpretation, the constraint''s legitimacy is further undermined, strengthening its classification as a Snare and highlighting the active suppression of alternative interpretations. If genuinely ambiguous, it points to a flaw in the Charter''s design that enabled this outcome.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(military_custodian_vs_secular_democratic_reading, conceptual, 'Ambiguity of Charter text vs. deliberate reinterpretation.').

omega_variable(
    mandate_obsolescence_vs_ongoing_threat,
    'Is the founding problem of post-revolutionary instability still a live threat, or has the military''s ''custodian'' role outlived its original justification and become a self-perpetuating mechanism?',
    'Independent assessment of national security threats by non-military experts, analysis of political violence trends, and comparison with other post-revolutionary transitions that successfully civilianized.',
    'If the threat is no longer live, the constraint''s persistence is purely extractive, reinforcing its Snare classification. If a genuine threat persists, it complicates the classification by introducing a (contested) coordination function, potentially pushing it towards a Tangled Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mandate_obsolescence_vs_ongoing_threat, empirical, 'Founding problem''s status: live threat or obsolete justification.').

omega_variable(
    civilian_resistance_potential,
    'What is the true potential for organized civilian resistance if external suppression were reduced, and how much of the current ''stability'' is due to genuine consent versus coercion?',
    'Analysis of underground political networks, public opinion surveys (if feasible and reliable), and historical precedents of popular uprisings in similar contexts.',
    'Higher latent resistance would indicate that the current ''stability'' is a direct product of extreme suppression, further solidifying the Snare classification. Lower latent resistance might suggest a degree of internalized acceptance or resignation, though not necessarily consent.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(civilian_resistance_potential, empirical, 'Latent civilian resistance vs. coerced stability.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(july_charter_sovereign_legitimacy__military_custodian_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(july_tr_t0, july_charter_sovereign_legitimacy__military_custodian_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(july_tr_t5, july_charter_sovereign_legitimacy__military_custodian_reading, theater_ratio, 5, 0.25).
narrative_ontology:measurement(july_tr_t10, july_charter_sovereign_legitimacy__military_custodian_reading, theater_ratio, 10, 0.3).
narrative_ontology:measurement(july_tr_t15, july_charter_sovereign_legitimacy__military_custodian_reading, theater_ratio, 15, 0.35).
narrative_ontology:measurement(july_tr_t20, july_charter_sovereign_legitimacy__military_custodian_reading, theater_ratio, 20, 0.4).

% Extraction over time
narrative_ontology:measurement(july_be_t0, july_charter_sovereign_legitimacy__military_custodian_reading, base_extractiveness, 0, 0.7).
narrative_ontology:measurement(july_be_t5, july_charter_sovereign_legitimacy__military_custodian_reading, base_extractiveness, 5, 0.75).
narrative_ontology:measurement(july_be_t10, july_charter_sovereign_legitimacy__military_custodian_reading, base_extractiveness, 10, 0.8).
narrative_ontology:measurement(july_be_t15, july_charter_sovereign_legitimacy__military_custodian_reading, base_extractiveness, 15, 0.83).
narrative_ontology:measurement(july_be_t20, july_charter_sovereign_legitimacy__military_custodian_reading, base_extractiveness, 20, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(july_su_t0, july_charter_sovereign_legitimacy__military_custodian_reading, suppression_requirement, 0, 0.8).
narrative_ontology:measurement(july_su_t5, july_charter_sovereign_legitimacy__military_custodian_reading, suppression_requirement, 5, 0.85).
narrative_ontology:measurement(july_su_t10, july_charter_sovereign_legitimacy__military_custodian_reading, suppression_requirement, 10, 0.88).
narrative_ontology:measurement(july_su_t15, july_charter_sovereign_legitimacy__military_custodian_reading, suppression_requirement, 15, 0.9).
narrative_ontology:measurement(july_su_t20, july_charter_sovereign_legitimacy__military_custodian_reading, suppression_requirement, 20, 0.92).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(july_charter_sovereign_legitimacy__military_custodian_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(july_charter_sovereign_legitimacy__military_custodian_reading, july_charter_sovereign_legitimacy__secular_democratic_reading).
narrative_ontology:affects_constraint(july_charter_sovereign_legitimacy__military_custodian_reading, july_charter_sovereign_legitimacy__guided_nationalism_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three distinct readings of the 'July Charter Sovereign Legitimacy' kernel. This 'military custodian' reading emphasizes the military's permanent guardianship role, subordinating civilian authority. It directly influences and is influenced by the 'secular democratic' and 'guided nationalism' readings, as they represent competing interpretations of the same foundational document.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
