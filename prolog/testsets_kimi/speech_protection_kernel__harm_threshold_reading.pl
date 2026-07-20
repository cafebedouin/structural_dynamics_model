% ============================================================================
% CONSTRAINT STORY: speech_protection_kernel__harm_threshold_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
    narrative_ontology:suppression_profile/2,
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
 *   constraint_id: speech_protection_kernel__harm_threshold_reading
 *   human_readable: Speech Protection Conditional on Absence of Demonstrable Harm
 *   domain: constitutional_law/political_philosophy/communication_rights
 *
 * SUMMARY:
 *   This constraint instantiates the harm_threshold_reading of the
 *   speech_protection_kernel: a constitutional and legal doctrine holding
 *   that speech is protected only where it does not cause demonstrable harm
 *   to victims. It is one of five contested readings of the same kernel,
 *   distinguished by its narrower protection boundary and its subordination
 *   of speaker autonomy to victim harm claims. The constraint is actively
 *   enforced by courts and regulators, coordinates harm prevention, and
 *   asymmetrically extracts expressive freedom from speakers who cross the
 *   threshold.
 *
 * KEY AGENTS:
 *   - harm_protected_parties (moderate/constrained): Benefit from legal restriction of harmful speech; can invoke harm claims to limit expression directed at them.
 *   - restricted_speakers (moderate/constrained): Bear costs through chilled expression, legal penalties, and self-censorship when harm thresholds are met.
 *   - judiciary (institutional/analytical): Administers the harm threshold through constitutional interpretation and precedent.
 *   - constitutional_scholars (organized/analytical): Observe and theorize the constraint's operation without direct cost or benefit.
 *   - absolutist_advocates (organized/constrained): Excluded from the operative framework; reject harm-based restrictions categorically.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(speech_protection_kernel__harm_threshold_reading, 0.62).
domain_priors:suppression_score(speech_protection_kernel__harm_threshold_reading, 0.58).
domain_priors:theater_ratio(speech_protection_kernel__harm_threshold_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(speech_protection_kernel__harm_threshold_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(speech_protection_kernel__harm_threshold_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(speech_protection_kernel__harm_threshold_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(speech_protection_kernel__harm_threshold_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(speech_protection_kernel__harm_threshold_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(speech_protection_kernel__harm_threshold_reading, tangled_rope).
narrative_ontology:human_readable(speech_protection_kernel__harm_threshold_reading, "Speech Protection Conditional on Absence of Demonstrable Harm").
narrative_ontology:topic_domain(speech_protection_kernel__harm_threshold_reading, "constitutional_law/political_philosophy/communication_rights").

domain_priors:requires_active_enforcement(speech_protection_kernel__harm_threshold_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(speech_protection_kernel__harm_threshold_reading, 'e73c0580-1576-45bf-b87d-5248413c402f').
narrative_ontology:cs_kernel_codification('e73c0580-1576-45bf-b87d-5248413c402f', fixed_text).
narrative_ontology:cs_authority_grounding('e73c0580-1576-45bf-b87d-5248413c402f', lineage).
narrative_ontology:cs_interpretation_layer_present('e73c0580-1576-45bf-b87d-5248413c402f').
narrative_ontology:cs_reading_relation('e73c0580-1576-45bf-b87d-5248413c402f', speech_protection_kernel__absolutist_reading, forecloses).
narrative_ontology:cs_reading_relation('e73c0580-1576-45bf-b87d-5248413c402f', speech_protection_kernel__marketplace_reading, influences).
narrative_ontology:cs_reading_relation('e73c0580-1576-45bf-b87d-5248413c402f', speech_protection_kernel__dignity_reading, coexists_with).
narrative_ontology:cs_reading_relation('e73c0580-1576-45bf-b87d-5248413c402f', speech_protection_kernel__democratic_participation_reading, coexists_with).
narrative_ontology:cs_axiom('e73c0580-1576-45bf-b87d-5248413c402f', foundational, demonstrable_harm_overrides_expression).
narrative_ontology:cs_axiom_status(demonstrable_harm_overrides_expression, holdable).
narrative_ontology:cs_axiom_grounding('e73c0580-1576-45bf-b87d-5248413c402f', demonstrable_harm_overrides_expression, deontological).
narrative_ontology:cs_reference_frame('e73c0580-1576-45bf-b87d-5248413c402f', harm_bounded_liberty).
narrative_ontology:cs_drift_state('e73c0580-1576-45bf-b87d-5248413c402f', contemporary_culture_war_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('e73c0580-1576-45bf-b87d-5248413c402f', '').
narrative_ontology:cs_kernel_id(speech_protection_kernel__harm_threshold_reading, speech_protection_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(speech_protection_kernel__harm_threshold_reading, harm_protected_parties).
narrative_ontology:constraint_victim(speech_protection_kernel__harm_threshold_reading, restricted_speakers).
narrative_ontology:constraint_vindicates(speech_protection_kernel__harm_threshold_reading, harm_principle).
narrative_ontology:constraint_vindicates(speech_protection_kernel__harm_threshold_reading, listener_protection_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Individuals or groups who can invoke claims of demonstrable harm to seek legal restriction of speech directed at them. They receive protection from the constraint but do not administer it; their ability to obtain redress depends entirely on judicial recognition and enforcement of their harm claims.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__harm_threshold_reading, harm_protected_parties, beneficiary,
    moderate, biographical, constrained, national).

% Speakers whose expression is chilled, penalized, or prohibited when courts or regulators determine it crosses the threshold of demonstrable harm to victims. They bear the direct cost of the constraint through lost expressive opportunities, legal penalties, platform deamplification, or self-censorship.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__harm_threshold_reading, restricted_speakers, payer,
    moderate, biographical, constrained, national).

% Courts and tribunals that interpret what constitutes demonstrable harm, set evidentiary standards for harm claims, and determine when speech protection is overridden. They administer the constraint through constitutional interpretation and precedent, exercising significant discretion in calibrating the threshold.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__harm_threshold_reading, judiciary, agenda_setter,
    institutional, generational, analytical, national).

% Legal academics who analyze the coherence of the harm threshold, debate its boundaries against competing readings of the speech protection kernel, and produce interpretive frameworks that influence judicial reasoning. They neither benefit directly from restriction nor pay its costs.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__harm_threshold_reading, constitutional_scholars, observer,
    organized, generational, analytical, global).

% Free speech absolutists and civil liberties organizations who argue that listener harm should never ground content-based speech restriction. They are structurally excluded from the operative interpretive framework when the harm threshold reading is adopted as doctrine, though they continue to dissent in public and academic discourse.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__harm_threshold_reading, absolutist_advocates, excluded,
    organized, generational, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Prevents demonstrable harm to individuals from expressive conduct by creating a legal boundary that restricts speech when victim harm is proven; coordinates social coexistence by balancing expressive liberty against injury prevention.
% TRANSFER_FUNCTION: Moves expressive freedom from speakers to the protected security of potential victims whenever a court or regulator finds demonstrable harm; simultaneously moves interpretive authority from the constitutional text to the judiciary to define the content and evidentiary standard of harm.
% ABSENT_VOICES: Absolutist advocates who reject harm as a valid basis for restriction, and speakers from subordinated groups whose dissent is more readily classified as harmful by majoritarian institutions, are structurally disadvantaged in the interpretive framework.
% DISAPPEARANCE_RATIONALE: If the harm threshold vanished overnight, speakers currently restricted under this standard would regain expressive freedom, harm-protected parties would lose a primary legal remedy for injurious speech, and courts would need to revert to alternative frameworksâabsolutist, dignity-based, or democratic-participation readingsâto adjudicate speech cases.
% FOUNDING_PROBLEM: How to permit open expression while protecting individuals from tangible injury caused by speechâsuch as defamation, incitement, targeted harassment, and severe psychological harmâin a pluralistic society.
% FOUNDING_PROBLEM_CORROBORATION: Victims' rights advocates and some constitutional scholars attest the problem is live and justifies the constraint. Free speech organizations and absolutist theorists attest the problem is overstated or solvable through non-legal means such as counterspeech and social norms. No neutral consensus exists outside the benefiting parties; the corroboration is partisan.
narrative_ontology:disappearance_verdict(speech_protection_kernel__harm_threshold_reading, world_rearranges).
narrative_ontology:founding_problem_status(speech_protection_kernel__harm_threshold_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(speech_protection_kernel__harm_threshold_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(speech_protection_kernel__harm_threshold_reading, 'none', 1).
narrative_ontology:epsilon_provenance(speech_protection_kernel__harm_threshold_reading, 0.62, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness is moderate-high (0.62) because the constraint systematically removes expressive freedom from speakers and reallocates it to harm prevention. Suppression is moderate-high (0.58) because the constraint depends on legal penalties and injunctive power to enforce the threshold. Theater is moderate-low (0.30): the harm prevention function is genuine, but a portion of enforcement activity is performative signaling of victim sensitivity. Accessibility collapse (0.65) reflects that once the harm threshold is adopted as doctrine, absolutist alternatives are legally closed within that framework. Resistance (0.55) captures sustained opposition from free speech advocates and civil liberties organizations.
 *
 * PERSPECTIVAL GAP:
 *   The beneficiary seat (harm_protected_parties) experiences the constraint as protective coordination that secures their dignity and safety. The payer seat (restricted_speakers) experiences the same structure as enforced extraction that limits autonomy. The judiciary occupies a structurally symmetric position: it does not collect the extraction but exercises interpretive power over its allocation. Constitutional scholars observe the divergence without occupying either seat. The engine computes this per-seat divergence from the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   harm_protected_parties are structural beneficiaries (gain legal protection, low directionality). restricted_speakers are structural targets (lose expressive freedom, high directionality). The judiciary is an agenda_setter with analytical exit; its directionality is structurally moderate because it wields interpretive authority without being a direct financial beneficiary. absolutist_advocates are excluded from the operative conversation and sit near the target end due to their constrained exit and structural opposition.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problemâprotecting individuals from speech-based harmâremains contested rather than dead. Because the problem is live and the constraint is actively enforced to address it, the constraint is not a piton. The classification as tangled_rope captures both the genuine coordination (harm prevention) and the asymmetric extraction (speaker restriction). If the harm threshold were primarily performative (high theater_ratio with a dead founding_problem), it would compute toward piton; the measurements show moderate theater and contested status, supporting the current classification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    demonstrable_harm_boundary,
    'What evidentiary and conceptual standard qualifies as ''demonstrable harm'' sufficient to override speech protection?',
    'Comparative jurisprudential analysis across jurisdictions and meta-analysis of judicial outcomes to identify whether harm claims are applied consistently or selectively.',
    'If harm is defined expansively or arbitrarily, the constraint computes as more extractive (snare-ward); if narrowly and consistently, it remains tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(demonstrable_harm_boundary, conceptual, 'Ambiguity in the harm threshold boundary.').

omega_variable(
    kernel_reading_contest_ambiguity,
    'Is the harm threshold reading the correct interpretation of the speech protection kernel, or does it improperly constrain a right that should be governed by absolutist, dignity, or marketplace principles?',
    'Historical and textual analysis of the kernel''s codification combined with empirical study of which reading produces better social outcomes.',
    'If the kernel is shown to structurally privilege one reading, other readings become misreadings; if the kernel is genuinely underdetermined, the contest is irreducible and the constraint remains one of several valid readings.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_contest_ambiguity, conceptual, 'This constraint is one reading of a contested kernel.').

omega_variable(
    majoritarian_capture_of_harm,
    'Does the harm threshold disproportionately restrict speech from marginalized or dissenting speakers while protecting dominant groups, indicating majoritarian capture of the harm standard?',
    'Empirical analysis of speech restriction outcomes by speaker identity and viewpoint, controlling for harm claim type.',
    'If majoritarian capture is demonstrated, the constraint''s coordination function is partially cover for asymmetric extraction (snare features); if outcomes are viewpoint-neutral, the coordination function is genuine.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(majoritarian_capture_of_harm, empirical, 'Whether harm claims are captured by majoritarian preferences.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(speech_protection_kernel__harm_threshold_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(spk_ht_tr_t0, speech_protection_kernel__harm_threshold_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(spk_ht_tr_t10, speech_protection_kernel__harm_threshold_reading, theater_ratio, 10, 0.22).
narrative_ontology:measurement(spk_ht_tr_t20, speech_protection_kernel__harm_threshold_reading, theater_ratio, 20, 0.24).
narrative_ontology:measurement(spk_ht_tr_t30, speech_protection_kernel__harm_threshold_reading, theater_ratio, 30, 0.26).
narrative_ontology:measurement(spk_ht_tr_t40, speech_protection_kernel__harm_threshold_reading, theater_ratio, 40, 0.28).
narrative_ontology:measurement(spk_ht_tr_t50, speech_protection_kernel__harm_threshold_reading, theater_ratio, 50, 0.3).

% Extraction over time
narrative_ontology:measurement(spk_ht_be_t0, speech_protection_kernel__harm_threshold_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(spk_ht_be_t10, speech_protection_kernel__harm_threshold_reading, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(spk_ht_be_t20, speech_protection_kernel__harm_threshold_reading, base_extractiveness, 20, 0.52).
narrative_ontology:measurement(spk_ht_be_t30, speech_protection_kernel__harm_threshold_reading, base_extractiveness, 30, 0.56).
narrative_ontology:measurement(spk_ht_be_t40, speech_protection_kernel__harm_threshold_reading, base_extractiveness, 40, 0.6).
narrative_ontology:measurement(spk_ht_be_t50, speech_protection_kernel__harm_threshold_reading, base_extractiveness, 50, 0.62).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(speech_protection_kernel__harm_threshold_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(speech_protection_kernel__harm_threshold_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(speech_protection_kernel__harm_threshold_reading, absolutist_reading).
narrative_ontology:affects_constraint(speech_protection_kernel__harm_threshold_reading, marketplace_reading).
narrative_ontology:affects_constraint(speech_protection_kernel__harm_threshold_reading, dignity_reading).
narrative_ontology:affects_constraint(speech_protection_kernel__harm_threshold_reading, democratic_participation_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the speech_protection_kernel. The kernel decomposes into multiple structurally distinct constraints because each reading assigns a different epsilon, beneficiary/victim structure, and classification. This reading (harm_threshold) influences the interpretive space of its siblings but does not logically foreclose all of them.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
