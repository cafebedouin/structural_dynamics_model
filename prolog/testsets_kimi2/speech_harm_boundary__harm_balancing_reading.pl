% ============================================================================
% CONSTRAINT STORY: speech_harm_boundary__harm_balancing_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_speech_harm_boundary__harm_balancing_reading, []).

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
 *   constraint_id: speech_harm_boundary__harm_balancing_reading
 *   human_readable: Speech Harm Boundary â Proportionality Balancing Reading
 *   domain: constitutional_law/communication_ethics
 *
 * SUMMARY:
 *   This constraint instantiates the harm_balancing_reading of the contested
 *   kernel speech_harm_boundary. Under this reading, constitutional speech
 *   protection is presumptive but yields to demonstrated harm through
 *   judicial proportionality analysis, producing broader unprotected
 *   categories (hate speech, group libel, harassment) than an absolutist
 *   reading would allow, while rejecting the categorical subordination of
 *   speech to dignity. The arrangement coordinates public discourse by
 *   supplying a predictable harm-redress mechanism, yet asymmetrically
 *   extracts expressive liberty from speakers who lose in the balancing
 *   calculus. Sibling readings include absolutist_reading (near-absolute
 *   protection) and dignity_reading (categorical subordination to dignity).
 *
 * KEY AGENTS:
 *   - constitutional_courts: agenda-setter (institutional/analytical) â administers proportionality methodology
 *   - speakers_subject_to_restriction: primary target (moderate/constrained) â bears restriction costs and chilling effects
 *   - media_publishers: secondary target (powerful/constrained) â faces liability and prior restraints
 *   - harms_claimants: primary beneficiary (moderate/constrained) â obtains remedies and legal recognition
 *   - civil_liberties_bar: analytical observer (organized/analytical) â scrutinizes doctrine from rights frame
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(speech_harm_boundary__harm_balancing_reading, 0.48).
domain_priors:suppression_score(speech_harm_boundary__harm_balancing_reading, 0.52).
domain_priors:theater_ratio(speech_harm_boundary__harm_balancing_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(speech_harm_boundary__harm_balancing_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(speech_harm_boundary__harm_balancing_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(speech_harm_boundary__harm_balancing_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(speech_harm_boundary__harm_balancing_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(speech_harm_boundary__harm_balancing_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(speech_harm_boundary__harm_balancing_reading, tangled_rope).
narrative_ontology:human_readable(speech_harm_boundary__harm_balancing_reading, "Speech Harm Boundary â Proportionality Balancing Reading").
narrative_ontology:topic_domain(speech_harm_boundary__harm_balancing_reading, "constitutional_law/communication_ethics").

domain_priors:requires_active_enforcement(speech_harm_boundary__harm_balancing_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(speech_harm_boundary__harm_balancing_reading, '7b55cbac-c1a4-477b-b994-82fd4abcf7a3').
narrative_ontology:cs_kernel_codification('7b55cbac-c1a4-477b-b994-82fd4abcf7a3', formalized).
narrative_ontology:cs_authority_grounding('7b55cbac-c1a4-477b-b994-82fd4abcf7a3', lineage).
narrative_ontology:cs_interpretation_layer_present('7b55cbac-c1a4-477b-b994-82fd4abcf7a3').
narrative_ontology:cs_reading_relation('7b55cbac-c1a4-477b-b994-82fd4abcf7a3', speech_harm_boundary__absolutist_reading, forecloses).
narrative_ontology:cs_reading_relation('7b55cbac-c1a4-477b-b994-82fd4abcf7a3', speech_harm_boundary__dignity_reading, coexists_with).
narrative_ontology:cs_axiom('7b55cbac-c1a4-477b-b994-82fd4abcf7a3', foundational, presumptive_speech_protection).
narrative_ontology:cs_axiom_status(presumptive_speech_protection, holdable).
narrative_ontology:cs_axiom_grounding('7b55cbac-c1a4-477b-b994-82fd4abcf7a3', presumptive_speech_protection, conventional).
narrative_ontology:cs_axiom('7b55cbac-c1a4-477b-b994-82fd4abcf7a3', foundational, demonstrated_harm_override).
narrative_ontology:cs_axiom_status(demonstrated_harm_override, holdable).
narrative_ontology:cs_axiom_grounding('7b55cbac-c1a4-477b-b994-82fd4abcf7a3', demonstrated_harm_override, empirically_contingent).
narrative_ontology:cs_reference_frame('7b55cbac-c1a4-477b-b994-82fd4abcf7a3', proportionality_balancing_framework).
narrative_ontology:cs_drift_state('7b55cbac-c1a4-477b-b994-82fd4abcf7a3', contemporary_human_rights_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('7b55cbac-c1a4-477b-b994-82fd4abcf7a3', '').
narrative_ontology:cs_kernel_id(speech_harm_boundary__harm_balancing_reading, speech_harm_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(speech_harm_boundary__harm_balancing_reading, harms_claimants).
narrative_ontology:constraint_victim(speech_harm_boundary__harm_balancing_reading, speakers_subject_to_restriction).
narrative_ontology:constraint_victim(speech_harm_boundary__harm_balancing_reading, media_publishers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Adjudicate speech restrictions through proportionality analysis: weighing the severity of demonstrated harm against the value of the expression and the narrowness of the proposed limit. They set the doctrinal thresholds for what counts as demonstrable harm and what balancing methodology applies.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__harm_balancing_reading, constitutional_courts, agenda_setter,
    institutional, generational, analytical, national).

% Individual speakers who face injunctions, damages, or criminal sanctions when courts find their expression causes demonstrated harm proportionate to the restriction. They bear litigation costs, chilling effects, and the burden of proving the value of their speech in the balancing calculus.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__harm_balancing_reading, speakers_subject_to_restriction, payer,
    moderate, biographical, constrained, national).

% Institutional media actors subject to broader liability exposure and prior restraint risks. They self-censor to avoid proportionality litigation costs and face prior restraints when courts judge harm imminent and irreparable; their market access depends on compliance.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__harm_balancing_reading, media_publishers, payer,
    powerful, biographical, constrained, national).

% Individuals or groups who seek and obtain legal remedies against speech that has caused them demonstrated harm. They benefit from a legal framework that recognizes their injury as a legitimate limit on expression, shifting remedial power to their position.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__harm_balancing_reading, harms_claimants, beneficiary,
    moderate, biographical, constrained, national).

% Legal advocates and scholars who scrutinize proportionality decisions, represent speakers, and argue for narrower harm categories. They observe the constraint's operation from a rights-protection frame without being primary beneficiaries or direct payers.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__harm_balancing_reading, civil_liberties_bar, observer,
    organized, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Enables a public discourse system by protecting speech as a default while providing a predictable legal mechanism to redress demonstrated harms, allowing pluralistic expression without degenerating into unregulated conflict.
% TRANSFER_FUNCTION: Moves the cost of speech restriction from society at large to specific speakers when their expression is adjudicated to cause demonstrable harm disproportionate to its value; transfers remedial power and legal standing to harms claimants.
% ABSENT_VOICES: Absolutist free speech advocates who reject any harm-based exception are structurally marginalized in proportionality-balancing jurisdictions; dignity-reading advocates who seek categorical bans on personhood-denying speech are also sidelined by the case-by-case balancing method. Both occupy doctrinal positions that the proportionality framework treats as extremes rather than live options.
% DISAPPEARANCE_RATIONALE: If the proportionality balancing framework vanished overnight, defamation plaintiffs would lose a primary cause of action, hate speech restrictions would collapse into either categorical bans or near-absolutism, and media organizations would reorganize their editorial legal review around a different liability standard; the constitutional speech ecology would shift dramatically.
% FOUNDING_PROBLEM: How to protect expressive freedom as a democratic prerequisite while preventing speech from causing tangible, severe harm to individuals and social cohesion.
% FOUNDING_PROBLEM_CORROBORATION: Constitutional courts attest the problem from within the legal tradition. Independent free-speech scholars and constitutional historians outside the harms-claimant beneficiary set corroborate that harmful speech exists but contest whether proportionality balancing is the appropriate solution; their testimony from a non-beneficiary seat confirms the contested status.
narrative_ontology:disappearance_verdict(speech_harm_boundary__harm_balancing_reading, world_rearranges).
narrative_ontology:founding_problem_status(speech_harm_boundary__harm_balancing_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(speech_harm_boundary__harm_balancing_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(speech_harm_boundary__harm_balancing_reading, 'none', 1).
narrative_ontology:epsilon_provenance(speech_harm_boundary__harm_balancing_reading, 0.48, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(speech_harm_boundary__harm_balancing_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(speech_harm_boundary__harm_balancing_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(speech_harm_boundary__harm_balancing_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.48) because speakers genuinely bear restriction costs when harm is demonstrated, but the constraint also delivers a real coordination function (legal predictability). Suppression is moderate (0.52) because persistence depends on courts actively enforcing proportionality tests and rejecting absolutist defenses. Theater ratio is moderate-low (0.30): judicial reasoning is partly performative formalism, but the harm-assessment function is substantively operational. Accessibility collapse (0.45) reflects that alternative frameworks (absolutism, dignity-based categorical bans) remain visible in comparative constitutional discourse even if locally collapsed. Resistance (0.58) is moderate-to-high because speakers and publishers consistently litigate against restrictions. Metrics and claim are authored independently: the constraint is claimed as tangled_rope because it combines genuine coordination with asymmetric extraction, while the metrics describe its actual operation without tuning toward that claim.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats (speakers, publishers) and the agenda-setter seat (courts) should compute different per-seat types: courts experience the constraint as a coordination mechanism they administer, while speakers experience it as an actively enforced limit that extracts expressive liberty. The engine derives this divergence from the structural data â identical text produces opposite directionalities depending on role and exit options.
 *
 * DIRECTIONALITY LOGIC:
 *   Harms_claimants are structural beneficiaries (low d) because the constraint subsidizes their remedial position and recognition. Speakers_subject_to_restriction and media_publishers are structural targets (high d) because the constraint extracts expressive costs from them; publishers have more global power but are similarly constrained in this doctrinal context. Constitutional_courts sit near the analytical/beneficiary end as administrators, though they do not collect rents. Civil_liberties_bar sits at the observer position with analytical exit.
 *
 * MANDATROPHY ANALYSIS:
 *   The proportionality-balancing framework was built to solve a genuine coordination problem (protecting speech while redressing harm). It is not a piton because the founding problem remains live and contested, and the beneficiary set actively gains from its operation. It is not a snare because the coordination function is real and not merely cover: courts do apply genuine balancing tests, and the constraint is not sustained solely by suppressing alternatives. The tangled_rope classification captures the hybrid nature: the same structure that coordinates discourse also extracts from speakers.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_position,
    'This constraint is one reading (harm_balancing_reading) of the contested kernel speech_harm_boundary. Sibling readings include absolutist_reading and dignity_reading. What would change structurally if either sibling reading were adopted in place of this one?',
    'Comparative constitutional analysis across jurisdictions adopting absolutist, dignity, or proportionality frameworks; measurement of restriction rates, harm incidence, and speaker liability distributions.',
    'An absolutist reading would eliminate the victim set (no speakers restricted by state) but increase unredressed harms; a dignity reading would eliminate proportionality in favor of categorical bans, shifting the victim set to speakers in personhood-denying categories and removing the empirical-harm demonstration requirement.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_position, conceptual, 'Reading position within the speech_harm_boundary kernel and structural deltas').

omega_variable(
    proportionality_indeterminacy,
    'Does proportionality balancing provide a genuine coordination function (predictable legal standard) or does its indeterminacy enable extraction by shifting costs to less powerful speakers?',
    'Empirical analysis of judicial outcomes: variance in restriction rates across judges, jurisdictions, and speaker power levels; if outcomes cluster by judicial ideology rather than harm severity, indeterminacy is high.',
    'High indeterminacy would indicate the constraint functions as extraction-through-ambiguity; low indeterminacy supports the coordination framing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(proportionality_indeterminacy, empirical, 'Whether proportionality balancing is sufficiently determinate to coordinate').

omega_variable(
    harm_demonstration_threshold,
    'What evidentiary threshold counts as demonstrated harm under proportionality balancing, and does this threshold vary systematically with speaker power?',
    'Quantitative content analysis of judicial opinions measuring the evidentiary standard applied and cross-referencing speaker identity and power.',
    'If thresholds are lower for powerless speakers and higher for powerful institutional publishers, effective extraction is asymmetrically amplified for the powerless â a structural feature invisible in the abstract doctrine.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(harm_demonstration_threshold, empirical, 'Evidentiary threshold asymmetry in harm demonstration').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(speech_harm_boundary__harm_balancing_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(spee_tr_t0, speech_harm_boundary__harm_balancing_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(spee_tr_t8, speech_harm_boundary__harm_balancing_reading, theater_ratio, 8, 0.22).
narrative_ontology:measurement(spee_tr_t16, speech_harm_boundary__harm_balancing_reading, theater_ratio, 16, 0.25).
narrative_ontology:measurement(spee_tr_t24, speech_harm_boundary__harm_balancing_reading, theater_ratio, 24, 0.28).
narrative_ontology:measurement(spee_tr_t32, speech_harm_boundary__harm_balancing_reading, theater_ratio, 32, 0.3).
narrative_ontology:measurement(spee_tr_t40, speech_harm_boundary__harm_balancing_reading, theater_ratio, 40, 0.3).

% Extraction over time
narrative_ontology:measurement(spee_be_t0, speech_harm_boundary__harm_balancing_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(spee_be_t8, speech_harm_boundary__harm_balancing_reading, base_extractiveness, 8, 0.38).
narrative_ontology:measurement(spee_be_t16, speech_harm_boundary__harm_balancing_reading, base_extractiveness, 16, 0.42).
narrative_ontology:measurement(spee_be_t24, speech_harm_boundary__harm_balancing_reading, base_extractiveness, 24, 0.45).
narrative_ontology:measurement(spee_be_t32, speech_harm_boundary__harm_balancing_reading, base_extractiveness, 32, 0.47).
narrative_ontology:measurement(spee_be_t40, speech_harm_boundary__harm_balancing_reading, base_extractiveness, 40, 0.48).

% Suppression requirement over time
narrative_ontology:measurement(spee_su_t0, speech_harm_boundary__harm_balancing_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(spee_su_t8, speech_harm_boundary__harm_balancing_reading, suppression_requirement, 8, 0.44).
narrative_ontology:measurement(spee_su_t16, speech_harm_boundary__harm_balancing_reading, suppression_requirement, 16, 0.48).
narrative_ontology:measurement(spee_su_t24, speech_harm_boundary__harm_balancing_reading, suppression_requirement, 24, 0.5).
narrative_ontology:measurement(spee_su_t32, speech_harm_boundary__harm_balancing_reading, suppression_requirement, 32, 0.52).
narrative_ontology:measurement(spee_su_t40, speech_harm_boundary__harm_balancing_reading, suppression_requirement, 40, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(speech_harm_boundary__harm_balancing_reading, speech_harm_boundary__absolutist_reading).
narrative_ontology:affects_constraint(speech_harm_boundary__harm_balancing_reading, speech_harm_boundary__dignity_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the speech_harm_boundary kernel. The kernel decomposes into three structurally distinct constraints (absolutist, dignity, harm-balancing) because each reading assigns a different epsilon, different beneficiary/victim structures, and different directionality profiles to the same constitutional text. They are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
