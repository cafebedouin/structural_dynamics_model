% ============================================================================
% CONSTRAINT STORY: speech_protection_boundary__absolutist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_speech_protection_boundary__absolutist_reading, []).

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
 *   constraint_id: speech_protection_boundary__absolutist_reading
 *   human_readable: Brandenburg Absolutist Speech Protection Standard
 *   domain: constitutional_law/political_philosophy
 *
 * SUMMARY:
 *   This constraint instantiates the absolutist reading of the
 *   speech_protection_boundary kernel: the First Amendment, as interpreted in
 *   Brandenburg v. Ohio (1969) and its progeny, erects a near-absolute
 *   barrier against content-based speech regulation, permitting punishment
 *   only for speech directed to inciting or producing imminent lawless
 *   action. The reading coordinates broad protection for political dissent,
 *   press freedom, and offensive speech, but structurally externalizes the
 *   costs of hate speech, targeted harassment, and stochastic violence onto
 *   minoritized communities by collapsing their legal alternatives for
 *   redress. It is claimed as a rope-like coordination mechanism while the
 *   metrics capture its tangled operation: genuine coordination for speakers,
 *   asymmetric extraction for communities bearing unaddressed harm.
 *
 * KEY AGENTS:
 *   - federal_judiciary: Agenda-setter (institutional/analytical) â interprets and enforces the Brandenburg standard through constitutional adjudication
 *   - dissident_speakers: Primary beneficiary (moderate/constrained) â protected from government censorship for political and offensive speech
 *   - press_entities: Secondary beneficiary (powerful/mobile) â shielded in publishing controversial content
 *   - minoritized_communities: Primary payer (powerless/trapped) â absorb aggregate harm from speech falling below the Brandenburg threshold
 *   - state_legislatures: Secondary payer (institutional/constrained) â prevented from enacting hate-speech or content-regulation legislation
 *   - critical_scholars: Analytical observer (analytical/analytical) â document the externality and argue for alternative frameworks
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(speech_protection_boundary__absolutist_reading, 0.62).
domain_priors:suppression_score(speech_protection_boundary__absolutist_reading, 0.62).
domain_priors:theater_ratio(speech_protection_boundary__absolutist_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(speech_protection_boundary__absolutist_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(speech_protection_boundary__absolutist_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(speech_protection_boundary__absolutist_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(speech_protection_boundary__absolutist_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(speech_protection_boundary__absolutist_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(speech_protection_boundary__absolutist_reading, tangled_rope).
narrative_ontology:human_readable(speech_protection_boundary__absolutist_reading, "Brandenburg Absolutist Speech Protection Standard").
narrative_ontology:topic_domain(speech_protection_boundary__absolutist_reading, "constitutional_law/political_philosophy").

domain_priors:requires_active_enforcement(speech_protection_boundary__absolutist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(speech_protection_boundary__absolutist_reading, 'fa1c33d4-7560-4533-8668-d3ab299c311a').
narrative_ontology:cs_kernel_codification('fa1c33d4-7560-4533-8668-d3ab299c311a', fixed_text).
narrative_ontology:cs_authority_grounding('fa1c33d4-7560-4533-8668-d3ab299c311a', lineage).
narrative_ontology:cs_interpretation_layer_present('fa1c33d4-7560-4533-8668-d3ab299c311a').
narrative_ontology:cs_reading_relation('fa1c33d4-7560-4533-8668-d3ab299c311a', speech_protection_boundary__harm_limited_reading, forecloses).
narrative_ontology:cs_reading_relation('fa1c33d4-7560-4533-8668-d3ab299c311a', speech_protection_boundary__balancing_reading, coexists_with).
narrative_ontology:cs_axiom('fa1c33d4-7560-4533-8668-d3ab299c311a', foundational, government_may_not_weight_speech_value).
narrative_ontology:cs_axiom_status(government_may_not_weight_speech_value, holdable).
narrative_ontology:cs_axiom_grounding('fa1c33d4-7560-4533-8668-d3ab299c311a', government_may_not_weight_speech_value, deontological).
narrative_ontology:cs_axiom('fa1c33d4-7560-4533-8668-d3ab299c311a', foundational, imminent_violence_as_sole_unprotected_category).
narrative_ontology:cs_axiom_status(imminent_violence_as_sole_unprotected_category, holdable).
narrative_ontology:cs_axiom_grounding('fa1c33d4-7560-4533-8668-d3ab299c311a', imminent_violence_as_sole_unprotected_category, conventional).
narrative_ontology:cs_reference_frame('fa1c33d4-7560-4533-8668-d3ab299c311a', categorical_speech_immunity).
narrative_ontology:cs_drift_state('fa1c33d4-7560-4533-8668-d3ab299c311a', contemporary_digital_speech_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('fa1c33d4-7560-4533-8668-d3ab299c311a', '').
narrative_ontology:cs_kernel_id(speech_protection_boundary__absolutist_reading, speech_protection_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(speech_protection_boundary__absolutist_reading, dissident_speakers).
narrative_ontology:constraint_beneficiary(speech_protection_boundary__absolutist_reading, press_entities).
narrative_ontology:constraint_victim(speech_protection_boundary__absolutist_reading, minoritized_communities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(speech_protection_boundary__absolutist_reading, state_legislatures).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets and enforces the First Amendment through constitutional review, applying the Brandenburg imminent-lawless-action test to strike down federal and state speech restrictions that fail the standard.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__absolutist_reading, federal_judiciary, agenda_setter,
    institutional, generational, analytical, national).

% Engage in political advocacy, protest, and offensive expression without fear of government prosecution, provided their speech does not directly incite imminent violence.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__absolutist_reading, dissident_speakers, beneficiary,
    moderate, biographical, constrained, national).

% Publish controversial and offensive content under a broad constitutional shield; commercial and editorial operations are protected by the near-absolute barrier against content-based regulation.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__absolutist_reading, press_entities, beneficiary,
    powerful, biographical, mobile, global).

% Exposed to hate speech, targeted harassment, and stochastic violence that falls short of the Brandenburg threshold; legal avenues to secure redress or state protection are foreclosed by the breadth of the standard.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__absolutist_reading, minoritized_communities, payer,
    powerless, biographical, trapped, national).

% Enact laws reflecting democratic majorities' preferences on speech regulation, but see those laws preempted when they address protected categories of harm outside imminent incitement.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__absolutist_reading, state_legislatures, payer,
    institutional, generational, constrained, national).

% Document the aggregate harm externalized onto minoritized communities and analyze the structural divergence between the absolutist doctrine and equality-centered speech frameworks in comparative law.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__absolutist_reading, critical_scholars, observer,
    analytical, civilizational, analytical, global).

narrative_ontology:fixing_cost_class(speech_protection_boundary__absolutist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Prevents majoritarian governments from suppressing political dissent, press criticism, and unpopular opinion by requiring an almost insurmountable evidentiary threshold (imminent lawless action) before speech can be punished.
% TRANSFER_FUNCTION: Moves the risk and material costs of verbal hostility and social injury from the stateâwhich is barred from regulating itâonto minoritized communities and targeted groups, who must absorb the costs of legally protected harmful speech.
% ABSENT_VOICES: Minoritized community members and equality advocates who would argue for dignity-based or harassment-protective speech limits are structurally excluded from the doctrinal framework; their injuries are recognized only as non-compensable externalities.
% DISAPPEARANCE_RATIONALE: If the Brandenburg standard vanished overnight, state legislatures would regain authority to enact hate-speech and anti-harassment laws, minoritized communities would gain legal recourse against targeted hostility, and the landscape of permissible expression would contract for political speakers and media entities.
% FOUNDING_PROBLEM: Government suppression of dissident political speech, particularly during wartime and social unrest, when legislatures and executives routinely punished critics, socialists, labor organizers, and civil rights activists.
% FOUNDING_PROBLEM_CORROBORATION: Civil liberties historians and First Amendment scholars outside the immediate beneficiary set attest to the historical suppression problem. Critical race theorists, international human rights bodies, and comparative constitutional scholars attest that the current framework has outlived its original anti-censorship function and now operates to protect speakers at the expense of equality.
narrative_ontology:disappearance_verdict(speech_protection_boundary__absolutist_reading, world_rearranges).
narrative_ontology:founding_problem_status(speech_protection_boundary__absolutist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(speech_protection_boundary__absolutist_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(speech_protection_boundary__absolutist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(speech_protection_boundary__absolutist_reading, 0.62, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(speech_protection_boundary__absolutist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(speech_protection_boundary__absolutist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(speech_protection_boundary__absolutist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62) reflects the structural externality: the constraint actively prevents the state from mitigating speech-based harms to minoritized communities, transferring those costs onto populations with trapped exit options. Suppression (0.62) measures the foreclosure of legislative alternatives â hate speech laws, anti-discrimination codes, and content regulations are structurally barred. Accessibility collapse (0.72) is high because once the Brandenburg framework is accepted, alternative regulatory pathways disappear from the legal imagination for protected categories of speech. Resistance (0.55) captures sustained scholarly, international-human-rights, and social-movement opposition. Theater ratio (0.25) is relatively low: judicial opinions engage in genuine doctrinal work, though some ritualistic recitation of marketplace-of-ideas rhetoric serves legitimizing functions. The measurement series share a single time grid (1969â2024) showing gradual extraction accumulation as digital speech environments expanded the volume and reach of harmful expression that falls short of imminent incitement.
 *
 * PERSPECTIVAL GAP:
 *   From the federal judiciary's seat, the constraint is a necessary coordination device preventing majoritarian legislatures from suppressing dissent; the engine should compute a rope-leaning classification there (low d, subsidized). From minoritized communities' seat, the same constraint operates as an enforced exposure to harm with no exit; the engine should compute a snare-leaning classification there (high d, amplified extraction). The divergence is the core datum. State legislatures experience a middle position: they are constrained but not personally harmed, yielding a moderate d and symmetric-to-target classification.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations (dissident_speakers, press_entities) feed low directionality: these agents are subsidized by the constraint's immunity. Victim declaration (minoritized_communities) feeds high directionality: this agent is the structural target of the externality. The federal judiciary is agenda_setter but not beneficiary in the rent-collection sense; its directionality is analytically mediated. Press_entities have mobile exit, which damps their derived d slightly. Minoritized_communities have trapped exit, amplifying their effective extraction. No overrides are needed because the structural derivation matches the true relationships.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint was built to solve live government suppression of dissidents (founding problem: mid-20th century anti-communist and civil-rights-era prosecutions). That problem is contested as still live, but the constraint has not atrophied into a piton: it retains a strong coordination function and concentrated beneficiaries who would fight to preserve it. The theater ratio is too low for piton classification. The risk of mandatrophy mislabeling here is low because the genuine coordination benefit (protecting dissent) is structurally separable from the extractive externality (harm to minoritized communities), which is exactly the tangled_rope signature.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    harm_externality_quantification,
    'Can the aggregate harm borne by minoritized communities under the Brandenburg standard be empirically distinguished from harm that would exist under alternative speech regimes?',
    'Comparative longitudinal studies across jurisdictions with different speech standards (e.g., US vs. Canada/EU), controlling for baseline discrimination rates and measuring reported harassment outcomes.',
    'If harm is irreducible across regimes, the extraction metric is lower and the coordination function dominates; if harm is regime-dependent, extraction is higher and the tangled_rope classification is reinforced.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(harm_externality_quantification, empirical, 'Whether externality is regime-dependent or baseline-persistent').

omega_variable(
    absolutism_as_doctrine_vs_practice,
    'Does the absolutist reading describe the actual operative constraint in US courts, or is it a doctrinal ideal that conceals ad hoc balancing in practice?',
    'Quantitative content analysis of judicial opinions to measure frequency of categorical Brandenburg application versus covert interest-balancing in speech cases.',
    'If covert balancing is common, the constraint is less extractive than claimed and the reading misdescribes the kernel; if truly categorical, the externality is structurally locked.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(absolutism_as_doctrine_vs_practice, empirical, 'Whether absolutism is operative doctrine or legitimizing cover').

omega_variable(
    kernel_reading_incommensurability,
    'Does the absolutist reading foreclose the harm-limited reading entirely, or do they represent incommensurable normative frameworks that cannot be evaluated within a single metric space?',
    'Jurisprudential analysis of whether the two readings share enough common premises to permit comparative epsilon assessment, or whether epsilon-invariance requires treating them as separate constraints entirely.',
    'If incommensurable, the decomposition into separate constraints is fully warranted; if commensurable, the kernel approach stands and comparative classification is valid.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_incommensurability, conceptual, 'Whether sibling readings are structurally comparable or incommensurable').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(speech_protection_boundary__absolutist_reading, 0, 55).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(spb_abs_tr_t0, speech_protection_boundary__absolutist_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement(spb_abs_tr_t9, speech_protection_boundary__absolutist_reading, theater_ratio, 9, 0.1).
narrative_ontology:measurement(spb_abs_tr_t18, speech_protection_boundary__absolutist_reading, theater_ratio, 18, 0.13).
narrative_ontology:measurement(spb_abs_tr_t27, speech_protection_boundary__absolutist_reading, theater_ratio, 27, 0.16).
narrative_ontology:measurement(spb_abs_tr_t36, speech_protection_boundary__absolutist_reading, theater_ratio, 36, 0.19).
narrative_ontology:measurement(spb_abs_tr_t45, speech_protection_boundary__absolutist_reading, theater_ratio, 45, 0.22).
narrative_ontology:measurement(spb_abs_tr_t55, speech_protection_boundary__absolutist_reading, theater_ratio, 55, 0.25).

% Extraction over time
narrative_ontology:measurement(spb_abs_be_t0, speech_protection_boundary__absolutist_reading, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(spb_abs_be_t9, speech_protection_boundary__absolutist_reading, base_extractiveness, 9, 0.38).
narrative_ontology:measurement(spb_abs_be_t18, speech_protection_boundary__absolutist_reading, base_extractiveness, 18, 0.44).
narrative_ontology:measurement(spb_abs_be_t27, speech_protection_boundary__absolutist_reading, base_extractiveness, 27, 0.5).
narrative_ontology:measurement(spb_abs_be_t36, speech_protection_boundary__absolutist_reading, base_extractiveness, 36, 0.56).
narrative_ontology:measurement(spb_abs_be_t45, speech_protection_boundary__absolutist_reading, base_extractiveness, 45, 0.6).
narrative_ontology:measurement(spb_abs_be_t55, speech_protection_boundary__absolutist_reading, base_extractiveness, 55, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(spb_abs_su_t0, speech_protection_boundary__absolutist_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(spb_abs_su_t9, speech_protection_boundary__absolutist_reading, suppression_requirement, 9, 0.45).
narrative_ontology:measurement(spb_abs_su_t18, speech_protection_boundary__absolutist_reading, suppression_requirement, 18, 0.48).
narrative_ontology:measurement(spb_abs_su_t27, speech_protection_boundary__absolutist_reading, suppression_requirement, 27, 0.52).
narrative_ontology:measurement(spb_abs_su_t36, speech_protection_boundary__absolutist_reading, suppression_requirement, 36, 0.56).
narrative_ontology:measurement(spb_abs_su_t45, speech_protection_boundary__absolutist_reading, suppression_requirement, 45, 0.6).
narrative_ontology:measurement(spb_abs_su_t55, speech_protection_boundary__absolutist_reading, suppression_requirement, 55, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(speech_protection_boundary__absolutist_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(speech_protection_boundary__absolutist_reading, speech_protection_boundary__harm_limited_reading).
narrative_ontology:affects_constraint(speech_protection_boundary__absolutist_reading, speech_protection_boundary__balancing_reading).

% DUAL FORMULATION NOTE:
% This constraint is the absolutist reading of the speech_protection_boundary kernel, which decomposes into three structurally distinct claims: absolutist (near-absolute protection with only imminent violence exception), balancing (case-by-case weighing), and harm-limited (dignity/equality conditional). The absolutist reading has the lowest epsilon among the three due to its genuine coordination function protecting dissent, but extracts via externality onto minoritized communities.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
