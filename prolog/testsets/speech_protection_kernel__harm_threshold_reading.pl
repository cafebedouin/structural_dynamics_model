% ============================================================================
% CONSTRAINT STORY: speech_protection_kernel__harm_threshold_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_speech_protection_kernel__harm_threshold_reading, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: speech_protection_kernel__harm_threshold_reading
 *   human_readable: Speech Protection Conditional on Demonstrable Harm Threshold
 *   domain: constitutional/political_philosophy
 *
 * SUMMARY:
 *   This constraint instantiates one reading of the contested
 *   speech-protection kernel: the harm-threshold reading. The constraint
 *   holds that speech protection is conditional on the absence of
 *   demonstrable harm to identifiable victims. When a speech act crosses the
 *   threshold into causing harm, it loses or forfeits constitutional
 *   protection, and enforcement authorities may restrict it. This reading
 *   directly challenges the absolutist reading (which holds speech protection
 *   near-categorical regardless of harm) and creates a different
 *   institutional structure than the marketplace reading (which relies on
 *   counter-speech) or the dignity reading (which focuses on subordination of
 *   groups rather than demonstrable injury). The constraint is CLAIMED as
 *   tangled_rope because it requires coordination (victim protection +
 *   speaker responsibility) while operating as asymmetric extraction
 *   (speakers bear expanded restriction risk; victims gain enforcement
 *   recourse). The authored metrics reflect the constraint as substantially
 *   extractive and actively enforced — a boundary-drawing mechanism with
 *   rising suppression intensity as case law expands what counts as harm.
 *
 * KEY AGENTS:
 *   - speech_subjects (powerless, identity-locked): bear the costs of being targets; their harm claim becomes the enforcement ground
 *   - speakers_testing_boundaries (moderate, constrained): navigate an uncertain protection boundary; face restriction risk if classified as causing harm
 *   - harm_victims (organized, mobile): benefit from legal recourse and enforcement machinery; depend on state's harm determination
 *   - state_enforcement_authorities (institutional, analytical): interpret the harm threshold and enforce restrictions; gain institutional authority by positioning as victim-protectors
 *   - absolutist_speakers (excluded): advocate near-categorical protection; their principle is overridden by this reading's design
 *   - courts (institutional observers): adjudicate harm claims and refine the boundary over time
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(speech_protection_kernel__harm_threshold_reading, 0.68).
domain_priors:suppression_score(speech_protection_kernel__harm_threshold_reading, 0.72).
domain_priors:theater_ratio(speech_protection_kernel__harm_threshold_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(speech_protection_kernel__harm_threshold_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(speech_protection_kernel__harm_threshold_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(speech_protection_kernel__harm_threshold_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(speech_protection_kernel__harm_threshold_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(speech_protection_kernel__harm_threshold_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(speech_protection_kernel__harm_threshold_reading, tangled_rope).
narrative_ontology:human_readable(speech_protection_kernel__harm_threshold_reading, "Speech Protection Conditional on Demonstrable Harm Threshold").
narrative_ontology:topic_domain(speech_protection_kernel__harm_threshold_reading, "constitutional/political_philosophy").

domain_priors:requires_active_enforcement(speech_protection_kernel__harm_threshold_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(speech_protection_kernel__harm_threshold_reading, '45a2132c-4765-48af-943b-8b7af4dbc82c').
narrative_ontology:cs_kernel_codification('45a2132c-4765-48af-943b-8b7af4dbc82c', fixed_text).
narrative_ontology:cs_authority_grounding('45a2132c-4765-48af-943b-8b7af4dbc82c', lineage).
narrative_ontology:cs_interpretation_layer_present('45a2132c-4765-48af-943b-8b7af4dbc82c').
narrative_ontology:cs_reading_relation('45a2132c-4765-48af-943b-8b7af4dbc82c', speech_protection_kernel__absolutist_reading, coexists_with).
narrative_ontology:cs_reading_relation('45a2132c-4765-48af-943b-8b7af4dbc82c', speech_protection_kernel__marketplace_reading, coexists_with).
narrative_ontology:cs_reading_relation('45a2132c-4765-48af-943b-8b7af4dbc82c', speech_protection_kernel__dignity_reading, coexists_with).
narrative_ontology:cs_reading_relation('45a2132c-4765-48af-943b-8b7af4dbc82c', speech_protection_kernel__democratic_participation_reading, coexists_with).
narrative_ontology:cs_axiom('45a2132c-4765-48af-943b-8b7af4dbc82c', foundational, harm_threshold_overrides_speaker_autonomy).
narrative_ontology:cs_axiom_status(harm_threshold_overrides_speaker_autonomy, holdable).
narrative_ontology:cs_axiom_grounding('45a2132c-4765-48af-943b-8b7af4dbc82c', harm_threshold_overrides_speaker_autonomy, deontological).
narrative_ontology:cs_axiom('45a2132c-4765-48af-943b-8b7af4dbc82c', foundational, demonstrable_victim_injury_justifies_restriction).
narrative_ontology:cs_axiom_status(demonstrable_victim_injury_justifies_restriction, holdable).
narrative_ontology:cs_axiom_grounding('45a2132c-4765-48af-943b-8b7af4dbc82c', demonstrable_victim_injury_justifies_restriction, empirically_contingent).
narrative_ontology:cs_reference_frame('45a2132c-4765-48af-943b-8b7af4dbc82c', speech_protection_with_victim_harm_boundary).
narrative_ontology:cs_drift_state('45a2132c-4765-48af-943b-8b7af4dbc82c', contemporary_expanded_harm_interpretation, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('45a2132c-4765-48af-943b-8b7af4dbc82c', '').
narrative_ontology:cs_kernel_id(speech_protection_kernel__harm_threshold_reading, speech_protection_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(speech_protection_kernel__harm_threshold_reading, harm_victims).
narrative_ontology:constraint_beneficiary(speech_protection_kernel__harm_threshold_reading, state_enforcement_authorities).
narrative_ontology:constraint_victim(speech_protection_kernel__harm_threshold_reading, speech_subjects).
narrative_ontology:constraint_victim(speech_protection_kernel__harm_threshold_reading, speakers_testing_boundaries).
narrative_ontology:constraint_vindicates(speech_protection_kernel__harm_threshold_reading, victim_protection_doctrine).
narrative_ontology:constraint_vindicates(speech_protection_kernel__harm_threshold_reading, harm_principle_constitutional).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Targeted by speech acts that the constraint classifies as harmful. They bear the emotional, reputational, and material costs of the speech directed at them. Under this reading, their harm claim becomes grounds for restricting the speaker's expression. The constraint ties their social standing and safety to the state's determination of what constitutes 'demonstrable harm,' binding them to remain the subject of state-mediated speech regulation — they cannot exit the status of being speech targets without removing the harm claim itself.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__harm_threshold_reading, speech_subjects, payer,
    powerless, biographical, identity_locked, national).

% Bear the risk that their expression will be classified as causing demonstrable harm and thus fall outside protection. They must navigate an uncertain, evolving boundary between protected and unprotected speech — the boundary shifts as courts and authorities interpret what 'demonstrable harm' entails. They retain the ability to self-censor or choose safer topics, but avoiding the constraint entirely requires silence on contested subjects.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__harm_threshold_reading, speakers_testing_boundaries, payer,
    moderate, biographical, constrained, national).

% Benefit from a constraint that recognizes their injury as grounds for legal restriction of speech. They can petition enforcement authorities and litigate harm claims; success grants them the power to have specific speech acts suppressed. They depend on the state's interpretation of 'demonstrable harm,' which creates a partnership with enforcement authorities but also exposes them to the risk that their harm claim will be deemed insufficiently proven.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__harm_threshold_reading, harm_victims, beneficiary,
    organized, biographical, mobile, national).

% Interpret and apply the harm threshold, deciding which speech acts meet the standard and thus lose protection. They administer the constraint by evaluating victim claims, determining what constitutes demonstrable harm, and enforcing restrictions on unprotected speech. They gain institutional power and legitimacy authority by positioning themselves as protectors of victims; they bear the burden of defining and proving harm in adjudication.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__harm_threshold_reading, state_enforcement_authorities, agenda_setter,
    institutional, generational, analytical, national).

% Advocates for near-categorical speech protection who are structurally cut out of the constraint's decision process. They argue that victim harm is not a legitimate ground for restricting speech and that the harm threshold reading betrays the foundational commitment to speaker autonomy. Their objections are litigated but their core principle — speaker autonomy as prior to harm consideration — is overridden by this reading's design.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__harm_threshold_reading, absolutist_speakers, excluded,
    moderate, biographical, constrained, national).

% Advocate that harmful or false speech is best countered by more speech, not restriction. They argue that restricting speech on harm grounds prevents the marketplace of ideas from functioning. They are excluded from the constraint's legitimacy structure because the harm threshold reading subordinates the discovery-function to victim protection.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__harm_threshold_reading, truth_marketplace_theorists, excluded,
    analytical, generational, analytical, national).

% Adjudicate disputes over whether speech meets the harm threshold. They are the mechanism through which the constraint's boundary is tested and refined. They see claims from speakers, victims, and state authorities; their rulings update what 'demonstrable harm' means and thus shift the constraint's effective scope over time.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__harm_threshold_reading, courts_and_appeal_bodies, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(speech_protection_kernel__harm_threshold_reading, state_enforcement_authorities).
narrative_ontology:fixing_cost_class(speech_protection_kernel__harm_threshold_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Balances speaker autonomy against victim protection: creates a legal framework that recognizes injury from speech while preserving a category of protected expression. Coordinates the state's role as arbiter of harm claims, victims' recourse to courts, and speakers' incentive to consider the impact of their expression.
% TRANSFER_FUNCTION: Moves decision-making power over speech boundaries from speakers (who decide what to say) to enforcement authorities and courts (who decide what constitutes unprotected harm). Transfers social authority to define harm from the market of individual judgment to the state's judicial interpretation. Transfers reputational cost from speech subjects to speakers of harmful expression.
% ABSENT_VOICES: Absolutist speakers who believe speech protection must be categorical and independent of harm; marketplace-of-ideas theorists who believe harmful speech is best countered by more speech, not restriction; speakers from marginalized communities whose expression is most likely to be classified as harmful under broad interpretations of the threshold.
% DISAPPEARANCE_RATIONALE: If the harm threshold constraint vanished, the boundary between protected and unprotected speech would shift immediately: some currently restricted speech would return to public circulation, victim remedies would narrow to civil liability rather than criminal/injunctive restriction, and the state's role in mediating speech boundaries would contract. The political, legal, and cultural discourse would reorganize around a different protection model.
% FOUNDING_PROBLEM: Speech can function as a tool of subordination and injury, inflicting demonstrable harm on vulnerable targets. The constraint was built to create legal grounds for victims to seek remedy and for the state to restrict speech that crosses into injury.
% FOUNDING_PROBLEM_CORROBORATION: Harm-focused advocates, victim-protection organizations, and courts applying harm-threshold doctrine attest the problem is live. Absolutist speakers and marketplace theorists attest that the harms attributed to speech are often exaggerated and that restriction causes greater harm by suppressing counter-speech and chilling legitimate expression. Independent empirical research on speech-related injury is contested — some studies support harm claims; others find effect sizes smaller than advocates claim or argue that the causal chain (speech→harm→injury) requires stronger proof than current law typically demands.
narrative_ontology:disappearance_verdict(speech_protection_kernel__harm_threshold_reading, world_rearranges).
narrative_ontology:founding_problem_status(speech_protection_kernel__harm_threshold_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(speech_protection_kernel__harm_threshold_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(speech_protection_kernel__harm_threshold_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(speech_protection_kernel__harm_threshold_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(speech_protection_kernel__harm_threshold_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(speech_protection_kernel__harm_threshold_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises from 0.48 to 0.68 over the interval, then plateaus. The rise reflects doctrine expanding what counts as demonstrable harm: early jurisprudence applies the threshold narrowly (high protection, low extraction); over time courts accept broader definitions of harm (psychological injury, reputational damage, persistent targeting), lowering the threshold and expanding unprotected categories. The plateau at t=20 suggests the expansion hits a stable equilibrium. Suppression rises similarly and remains high: the constraint requires active enforcement machinery to identify harm, adjudicate claims, and suppress unprotected speech. Theater is moderate (0.41 end state): some enforcement activity is genuine victim protection; some is state boundary-drawing and speech suppression under the guise of harm prevention. The theater rise (t=0–15) then plateau reflects courts developing clearer harm standards that reduce the performative uncertainty. Accessibility collapse at 0.62 reflects that speakers retain alternatives (self-censor, choose safer topics, litigate the boundary) but face real costs; harm victims face genuine barriers to exit the status of being speech targets.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and beneficiary seats compute different types. From the state and victim perspective, the constraint solves a real problem (injury from speech) and operates as protective coordination (acceptable cost for safety). From the speaker perspective, the same structure operates as enforced boundary-tightening and suppression risk (they lose autonomy; harm claims override their choice). Speakers testing boundaries and absolutist speakers see the constraint as extractive — it concentrates power in the state to define harm and suppress speech. Victims see it as enabling justice. The engine computes per-seat: state/victims likely compute near rope; speakers compute toward snare.
 *
 * DIRECTIONALITY LOGIC:
 *   Harm victims benefit (d near 0.0): they gain legal recourse and enforcement authority recognizes their injury. State authorities benefit (d near 0.1): they gain institutional power and legitimacy as victim-protectors. Speakers and speech subjects pay (d near 0.9 and 0.8 respectively): speakers face restriction risk; speech subjects remain bound to the status of targets. Speakers have constrained exit (self-censor or litigate); speech subjects are identity-locked (they cannot exit the status of being targeted without the harm claim losing its object). The asymmetry is structural: the constraint redistributes authority from speaker autonomy to victim-protection, concentrating power in the state's harm determination. Courts are near the midpoint (d~0.5): they enforce the constraint but are neutral seats that serve both speakers and victims.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint avoids collapsing into pure snare by maintaining a genuine coordination function (victim protection + speaker responsibility) and a real alternative (litigation, appeal, boundary clarification). It is not pure extraction because victims have legitimate injury claims and speakers retain some exit (though constrained). The tangled-rope classification holds as long as: (1) the harm threshold is applied with neutral intent; (2) speakers retain meaningful litigation recourse; (3) victims' harm claims are substantively tested rather than presumed. If enforcement becomes asymmetrically captured by political actors to suppress disfavored speech, or if harm standards drift so broadly that almost all speech triggers restriction, the constraint transitions toward snare. The measurement plateau at t=20 suggests the doctrine has stabilized, which supports the tangled-rope classification — it is not continuously tightening toward pure extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    demonstrable_harm_definitional_instability,
    'What counts as ''demonstrable harm'' — how much proof is required, and who bears the burden of proof? Does psychological injury count equally with physical harm?',
    'Appellate doctrine development over time, comparative law study of how different jurisdictions define harm thresholds, empirical measurement of what courts actually accept as demonstrable.',
    'A narrower definition (harm must be direct, severe, temporally immediate) keeps more speech protected and limits victims'' recourse; a broader definition (reputational injury, persistent denigration, identity-based targeting count) expands unprotected categories and concentrates enforcement power in the state.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(demonstrable_harm_definitional_instability, empirical, 'The boundary between demonstrable and disputed/exaggerated harm is not fixed in the constraint''s logic and drifts with judicial interpretation.').

omega_variable(
    structural_asymmetry_between_readings,
    'Can the harm threshold reading coexist in a single constitutional order with the absolutist reading, or do they logically foreclose each other?',
    'Historical constitutional practice: did the absolutist and harm-threshold readings ever coexist as live doctrines in the same jurisdiction? Or does adoption of one necessitate rejecting the other?',
    'If they coexist, the kernel permits multiple readings and the constraint is one option among others. If they foreclose each other, the constraint represents a foundational choice that excludes the absolutist commitment.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(structural_asymmetry_between_readings, conceptual, 'Whether harm-threshold and absolutist readings are compatible within a single framework or mutually exclusive.').

omega_variable(
    victim_identity_and_suppression_mechanism,
    'Does the constraint''s protection of victims rest on structural (legal barriers that prevent exit) or internalized (victims accept harm as inevitable and internalize the classification) suppression?',
    'Post-restriction empirical study: if harm victims'' ability to engage in public counter-speech or exit the role improves when they have legal recourse, suppression was partly structural; if victim status persists even with legal victories, suppression is partly internalized.',
    'Structural suppression suggests the constraint addresses a real barrier and victims gain agency through its operation. Internalized suppression suggests the constraint''s operation may reinforce victimhood identity and bind victims to the state''s harm determination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(victim_identity_and_suppression_mechanism, empirical, 'Whether victim suppression in this constraint is structural or internalized.').

omega_variable(
    enforcement_power_concentration,
    'Does the constraint concentrate speech-boundary authority in the state, and does that concentration itself become extractive?',
    'Measurement of who initiates harm claims, success rates by speaker/victim power level, and drift in the state''s use of the harm threshold to suppress political or marginalized speech over time.',
    'If enforcement authority is captured by political actors or used asymmetrically against powerless speakers, the constraint transitions from protecting victims to extracting silence from disfavored speakers.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_power_concentration, empirical, 'Whether the constraint''s enforcement machinery remains neutral or becomes a tool for suppressing disfavored speakers.').

omega_variable(
    reading_vs_absolutist_logical_structure,
    'Does this reading''s foundational axiom (harm can override speaker autonomy) necessarily foreclose the absolutist reading''s core (speaker autonomy is prior to harm), or can both axioms coexist in different constitutional contexts?',
    'Jurisprudential analysis of whether the two readings represent incompatible first principles or different policy weightings of compatible principles.',
    'If they foreclose each other, the constraint represents a foundational constitutional choice. If they coexist, the constraint is one reading among live alternatives.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_vs_absolutist_logical_structure, conceptual, 'Logical structure of the relationship between harm-threshold and absolutist readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(speech_protection_kernel__harm_threshold_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(spee_tr_t0, speech_protection_kernel__harm_threshold_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement_basis(spee_tr_t0, observed).
narrative_ontology:measurement(spee_tr_t5, speech_protection_kernel__harm_threshold_reading, theater_ratio, 5, 0.28).
narrative_ontology:measurement_basis(spee_tr_t5, observed).
narrative_ontology:measurement(spee_tr_t10, speech_protection_kernel__harm_threshold_reading, theater_ratio, 10, 0.35).
narrative_ontology:measurement_basis(spee_tr_t10, observed).
narrative_ontology:measurement(spee_tr_t15, speech_protection_kernel__harm_threshold_reading, theater_ratio, 15, 0.4).
narrative_ontology:measurement_basis(spee_tr_t15, observed).
narrative_ontology:measurement(spee_tr_t20, speech_protection_kernel__harm_threshold_reading, theater_ratio, 20, 0.41).
narrative_ontology:measurement_basis(spee_tr_t20, observed).
narrative_ontology:measurement(spee_tr_t25, speech_protection_kernel__harm_threshold_reading, theater_ratio, 25, 0.41).
narrative_ontology:measurement_basis(spee_tr_t25, observed).

% Extraction over time
narrative_ontology:measurement(spee_be_t0, speech_protection_kernel__harm_threshold_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement_basis(spee_be_t0, observed).
narrative_ontology:measurement(spee_be_t5, speech_protection_kernel__harm_threshold_reading, base_extractiveness, 5, 0.54).
narrative_ontology:measurement_basis(spee_be_t5, observed).
narrative_ontology:measurement(spee_be_t10, speech_protection_kernel__harm_threshold_reading, base_extractiveness, 10, 0.61).
narrative_ontology:measurement_basis(spee_be_t10, observed).
narrative_ontology:measurement(spee_be_t15, speech_protection_kernel__harm_threshold_reading, base_extractiveness, 15, 0.65).
narrative_ontology:measurement_basis(spee_be_t15, observed).
narrative_ontology:measurement(spee_be_t20, speech_protection_kernel__harm_threshold_reading, base_extractiveness, 20, 0.68).
narrative_ontology:measurement_basis(spee_be_t20, observed).
narrative_ontology:measurement(spee_be_t25, speech_protection_kernel__harm_threshold_reading, base_extractiveness, 25, 0.68).
narrative_ontology:measurement_basis(spee_be_t25, observed).

% Suppression requirement over time
narrative_ontology:measurement(spee_su_t0, speech_protection_kernel__harm_threshold_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement_basis(spee_su_t0, observed).
narrative_ontology:measurement(spee_su_t5, speech_protection_kernel__harm_threshold_reading, suppression_requirement, 5, 0.63).
narrative_ontology:measurement_basis(spee_su_t5, observed).
narrative_ontology:measurement(spee_su_t10, speech_protection_kernel__harm_threshold_reading, suppression_requirement, 10, 0.68).
narrative_ontology:measurement_basis(spee_su_t10, observed).
narrative_ontology:measurement(spee_su_t15, speech_protection_kernel__harm_threshold_reading, suppression_requirement, 15, 0.71).
narrative_ontology:measurement_basis(spee_su_t15, observed).
narrative_ontology:measurement(spee_su_t20, speech_protection_kernel__harm_threshold_reading, suppression_requirement, 20, 0.72).
narrative_ontology:measurement_basis(spee_su_t20, observed).
narrative_ontology:measurement(spee_su_t25, speech_protection_kernel__harm_threshold_reading, suppression_requirement, 25, 0.72).
narrative_ontology:measurement_basis(spee_su_t25, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(speech_protection_kernel__harm_threshold_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(speech_protection_kernel__harm_threshold_reading, 0.12).
narrative_ontology:affects_constraint(speech_protection_kernel__harm_threshold_reading, speech_protection_kernel__absolutist_reading).
narrative_ontology:affects_constraint(speech_protection_kernel__harm_threshold_reading, speech_protection_kernel__marketplace_reading).
narrative_ontology:affects_constraint(speech_protection_kernel__harm_threshold_reading, speech_protection_kernel__dignity_reading).
narrative_ontology:affects_constraint(speech_protection_kernel__harm_threshold_reading, speech_protection_kernel__democratic_participation_reading).

% DUAL FORMULATION NOTE:
% The speech protection kernel contests what grounds speech protection and where its boundaries lie. This story represents one reading (harm-threshold): protection is conditional on absence of demonstrable harm. Sibling readings adopt different boundary-drawing logic (absolutist: protection near-categorical; marketplace: counter-speech suffices; dignity: focus on subordination; democratic_participation: strongest for self-governance speech). Each reading instantiates a different constraint with different ε, beneficiary/victim structures, and classifications. The network links these related but distinct constraints, enabling cross-reading contamination and coupling analysis.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(speech_protection_kernel__harm_threshold_reading, powerless, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
