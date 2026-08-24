% ============================================================================
% CONSTRAINT STORY: speech_harm_boundary__absolutist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_speech_harm_boundary__absolutist_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: speech_harm_boundary__absolutist_reading
 *   human_readable: Absolutist Speech Protection Doctrine (Near-Absolute Harm Override Threshold)
 *   domain: constitutional_law/political_philosophy/communication_ethics
 *
 * SUMMARY:
 *   The absolutist reading of the speech-harm boundary holds that the First
 *   Amendment protects virtually all speech from government restriction, with
 *   only a handful of historically recognized exceptions (incitement, true
 *   threats, defamation, obscenity). Harm to listeners — including hate
 *   speech, harassment, dignitary injury, and psychological trauma — is not a
 *   valid ground for restriction. This reading treats speaker autonomy as a
 *   near-trump; the harm override threshold is set so high that it is
 *   practically never met. The constraint coordinates a diverse speech
 *   ecosystem by giving speakers categorical certainty, but it extracts the
 *   cost of that certainty from those whom the protected speech harms. The
 *   reading claims to be a pure coordination mechanism (rope); the metrics
 *   reveal asymmetric extraction (tangled_rope).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(speech_harm_boundary__absolutist_reading, 0.72).
domain_priors:suppression_score(speech_harm_boundary__absolutist_reading, 0.35).
domain_priors:theater_ratio(speech_harm_boundary__absolutist_reading, 0.12).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(speech_harm_boundary__absolutist_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(speech_harm_boundary__absolutist_reading, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(speech_harm_boundary__absolutist_reading, theater_ratio, 0.12).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(speech_harm_boundary__absolutist_reading, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(speech_harm_boundary__absolutist_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(speech_harm_boundary__absolutist_reading, tangled_rope).
narrative_ontology:human_readable(speech_harm_boundary__absolutist_reading, "Absolutist Speech Protection Doctrine (Near-Absolute Harm Override Threshold)").
narrative_ontology:topic_domain(speech_harm_boundary__absolutist_reading, "constitutional_law/political_philosophy/communication_ethics").

domain_priors:requires_active_enforcement(speech_harm_boundary__absolutist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(speech_harm_boundary__absolutist_reading, 'ee53495b-f3d1-4cd7-bc50-a581db4b6eac').
narrative_ontology:cs_kernel_codification('ee53495b-f3d1-4cd7-bc50-a581db4b6eac', formalized).
narrative_ontology:cs_authority_grounding('ee53495b-f3d1-4cd7-bc50-a581db4b6eac', lineage).
narrative_ontology:cs_interpretation_layer_present('ee53495b-f3d1-4cd7-bc50-a581db4b6eac').
narrative_ontology:cs_reading_relation('ee53495b-f3d1-4cd7-bc50-a581db4b6eac', speech_harm_boundary__harm_balancing_reading, coexists_with).
narrative_ontology:cs_reading_relation('ee53495b-f3d1-4cd7-bc50-a581db4b6eac', speech_harm_boundary__dignity_reading, forecloses).
narrative_ontology:cs_axiom('ee53495b-f3d1-4cd7-bc50-a581db4b6eac', foundational, speech_autonomy_near_absolute).
narrative_ontology:cs_axiom_status(speech_autonomy_near_absolute, holdable).
narrative_ontology:cs_axiom_grounding('ee53495b-f3d1-4cd7-bc50-a581db4b6eac', speech_autonomy_near_absolute, deontological).
narrative_ontology:cs_axiom('ee53495b-f3d1-4cd7-bc50-a581db4b6eac', foundational, harm_threshold_extremely_high).
narrative_ontology:cs_axiom_status(harm_threshold_extremely_high, holdable).
narrative_ontology:cs_axiom_grounding('ee53495b-f3d1-4cd7-bc50-a581db4b6eac', harm_threshold_extremely_high, deontological).
narrative_ontology:cs_reference_frame('ee53495b-f3d1-4cd7-bc50-a581db4b6eac', classical_liberal_free_speech).
narrative_ontology:cs_drift_state('ee53495b-f3d1-4cd7-bc50-a581db4b6eac', contemporary_hate_speech_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('ee53495b-f3d1-4cd7-bc50-a581db4b6eac', '').
narrative_ontology:cs_kernel_id(speech_harm_boundary__absolutist_reading, speech_harm_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(speech_harm_boundary__absolutist_reading, speakers).
narrative_ontology:constraint_beneficiary(speech_harm_boundary__absolutist_reading, press_institutions).
narrative_ontology:constraint_beneficiary(speech_harm_boundary__absolutist_reading, advocacy_organizations).
narrative_ontology:constraint_victim(speech_harm_boundary__absolutist_reading, targets_of_harmful_speech).
narrative_ontology:constraint_victim(speech_harm_boundary__absolutist_reading, marginalized_groups_disproportionately_targeted).
narrative_ontology:constraint_vindicates(speech_harm_boundary__absolutist_reading, marketplace_of_ideas_doctrine).
narrative_ontology:constraint_vindicates(speech_harm_boundary__absolutist_reading, autonomy_based_free_speech_theory).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Enjoy near-absolute protection for expression including offensive, hateful, or harmful speech. The doctrine shields them from liability and state restriction except in the narrowest categories (incitement, true threats, defamation, obscenity). They can speak freely across platforms and venues; exit from any single forum is easy but the doctrinal protection follows them.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__absolutist_reading, speakers, beneficiary,
    organized, biographical, mobile, national).

% Operate as professional speakers with institutional infrastructure. The absolutist doctrine protects their newsgathering, editorial judgment, and publication decisions from prior restraint and most post-publication liability. They shape the doctrinal boundary through litigation and amicus practice; their exit options include jurisdictional forum shopping and platform diversification.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__absolutist_reading, press_institutions, beneficiary,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(speech_harm_boundary__absolutist_reading, press_institutions, agenda_setter).

% Use absolutist protection to advance controversial causes (civil rights, anti-war, pro-life, etc.). The doctrine lets them deploy provocative rhetoric without fear of suppression. Their exit is constrained by dependence on the same doctrinal shield — if the boundary shifts, their tactical repertoire shrinks.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__absolutist_reading, advocacy_organizations, beneficiary,
    organized, biographical, constrained, national).

% Bear the costs of harmful speech (harassment, hate speech, doxxing, reputation destruction) with virtually no doctrinal remedy. The absolutist threshold means only the narrowest categories are actionable; most harm falls outside legal recourse. Exit is identity-locked — they cannot change the characteristics (race, gender, religion, orientation) that make them targets, and leaving the public sphere means surrendering civic participation.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__absolutist_reading, targets_of_harmful_speech, payer,
    powerless, biographical, identity_locked, national).

% Experience compounded harm because the absolutist doctrine protects the speech that most intensely targets their immutable characteristics. Historical exclusion from speech markets means they lack counterspeech capacity. The doctrinal structure extracts their dignity and safety as the price of others' autonomy. Exit is structurally impossible — the constraint operates on their identity, not their choices.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__absolutist_reading, marginalized_groups_disproportionately_targeted, payer,
    powerless, generational, identity_locked, national).

% Administer the absolutist doctrine through categorical rules (Brandenburg incitement, true threats, defamation actual malice, Miller obscenity). They police the boundary of the narrow unprotected categories and resist expansion. Their enforcement is active — striking down hate speech laws, invalidating campus codes, narrowing tort liability. They have analytical exit (can reason counterfactually) but institutional lock-in to the precedent structure.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__absolutist_reading, courts, agenda_setter,
    institutional, generational, analytical, national).

% Argue for broader harm-based exceptions (hate speech laws, dignity protections, harassment torts). They are structurally excluded from the doctrinal framework — the absolutist categories leave no room for proportionality balancing. Their exit is constrained: they can lobby for constitutional amendment (prohibitively difficult), seek state-level experimentation (preempted), or shift to non-legal remedies (platform policy, social norms).
narrative_ontology:constraint_stakeholder(speech_harm_boundary__absolutist_reading, harm_reduction_advocates, excluded,
    organized, biographical, constrained, national).

% Analyze the doctrine's coherence, history, and comparative alternatives. They produce the theoretical vocabulary (autonomy, marketplace, democracy, dignity) that frames the contest. Their seat is purely analytical — they neither collect nor pay, but their work shapes the legitimacy conditions for all other seats.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__absolutist_reading, legal_scholars, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the coordination problem of enabling diverse speakers to participate in public discourse without fear of state suppression or majoritarian censorship. The categorical rules create predictable boundaries so speakers know ex ante what is protected.
% TRANSFER_FUNCTION: Transfers the cost of harmful speech from speakers (who would face liability or restraint under a balancing regime) to targets (who absorb harassment, hate, reputational injury, and dignitary harm without legal remedy). The transfer is structured through doctrinal immunity: speakers gain; targets pay.
% ABSENT_VOICES: Targets of harmful speech — especially those targeted for immutable characteristics — are structurally absent from the doctrinal calculus. The categorical rules treat their harm as irrelevant unless it fits a pre-1960s category (defamation, obscenity). They are not represented in the courtroom when the boundary is drawn; their harm is the externality the doctrine externalizes.
% DISAPPEARANCE_RATIONALE: If the absolutist doctrine vanished overnight, legislatures would enact hate speech laws, harassment statutes, and dignity protections within months. Platforms would adopt European-style content moderation. The speech environment would reorganize around harm-reduction defaults rather than autonomy-maximization defaults. Speakers would lose categorical immunity; targets would gain actionable remedies.
% FOUNDING_PROBLEM: The absolutist doctrine was built to solve the problem of state censorship of political dissent — particularly seditious libel prosecutions, anti-war suppression, and civil rights protest criminalization. The founding generation (Holmes, Brandeis, Black, Douglas) feared that any balancing test would become a tool for the powerful to silence the powerless.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem (state censorship of dissent) is attested as substantially diminished by historians of the First Amendment (Rabban, Healy, Weinrib) who document the doctrine's success in securing robust political speech. However, civil rights advocates (Matsuda, Lawrence, Delgado, Crenshaw) attest that the same doctrine now protects the speech that subordinates historically excluded groups — a harm the founders did not anticipate. The corroboration is split: the original problem is largely solved, but the solution has become a new problem.
narrative_ontology:disappearance_verdict(speech_harm_boundary__absolutist_reading, world_rearranges).
narrative_ontology:founding_problem_status(speech_harm_boundary__absolutist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(speech_harm_boundary__absolutist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(speech_harm_boundary__absolutist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(speech_harm_boundary__absolutist_reading, 0.72, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(speech_harm_boundary__absolutist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(speech_harm_boundary__absolutist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(speech_harm_boundary__absolutist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.72) is high because the doctrinal structure systematically transfers harm costs from speakers to targets. The narrow exception categories capture only a tiny fraction of harmful speech; the vast majority of hate speech, harassment, and dignitary harm falls inside the protected zone. Suppression (0.35) is moderate — the constraint suppresses remedies, not speech, and courts actively strike down harm-based regulations. Theater ratio (0.12) is low — the doctrine is genuinely operational, not performative; courts really do invalidate hate speech laws. Accessibility collapse (0.78) is high for targets — once the absolutist frame is accepted, alternatives (balancing, dignity protections) are treated as categorically illegitimate. Resistance (0.55) is moderate — harm-reduction advocates persist but have no doctrinal foothold.
 *
 * PERSPECTIVAL GAP:
 *   From the beneficiary seats (speakers, press), the constraint appears as a genuine coordination triumph — predictable rules, no chilling effect, maximum discourse. From the payer seats (targets, marginalized groups), the same structure operates as a snare — their harm is legally invisible, their exit is blocked by identity, and the doctrine's persistence depends on their continued silence. The engine computes this divergence from the structural data; the absolutist reading's claim of pure coordination is the measurement the corpus exists to take.
 *
 * DIRECTIONALITY LOGIC:
 *   Speakers, press, and advocacy organizations are beneficiaries (d near 0.0) — they collect the full value of categorical immunity. Targets and marginalized groups are payers (d near 1.0) — they bear the full harm cost with identity-locked exit. Courts are agenda_setters (d ~ 0.5) — they administer the rule but are constrained by precedent. Harm-reduction advocates are excluded (d undefined) — they are not in the doctrinal conversation. Legal scholars are observers (d = 0.5 by definition). The directionality derivation from beneficiary/victim declarations + exit options produces this gradient automatically.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (state censorship of dissent) is substantially solved — the absolutist doctrine succeeded in its original mission. But the arrangement persists unchanged despite the problem's transformation. The doctrine now protects speech that the founders would have recognized as the functional equivalent of the suppression they feared (private power using speech to silence the powerless). The mandatrophy is not resolved: the constraint continues to operate as if the 1919 problem were the 2024 problem. The founding_problem_status = contested captures this: originalists say the problem is live (new censorship threats); critical scholars say it's dead (the doctrine has become the censor).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Does the absolutist reading instantiate a distinct constraint from the harm_balancing and dignity readings, or are they interpretive variants of a single constraint?',
    'Test ε-invariance: if measuring extraction under the absolutist reading yields a different ε than under the balancing reading for the same standing arrangement, they are distinct constraints per DP-001. The structural delta (narrow exceptions vs. balancing; categorical vs. proportional) suggests distinct ε.',
    'If distinct, each reading gets its own constraint story with its own classification. If variant, they are seats within one constraint. The ε-invariance principle requires decomposition when ε differs.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Whether the kernel''s readings are distinct constraints or perspectival seats.').

omega_variable(
    harm_measurement_ambiguity,
    'How should ''harm'' be measured for the extraction calculus — only legally cognizable harm, or all documented psychological, dignitary, and material harm?',
    'Compare extraction scores using legal-harm-only vs. empirical-harm-inclusive metrics. If the gap is large, the constraint''s ε is measurement-dependent, violating ε-invariance — requiring further decomposition.',
    'Legal-harm-only yields lower ε (only defamation/obscenity count). Empirical-harm-inclusive yields ε > 0.8. The measurement choice changes the classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(harm_measurement_ambiguity, empirical, 'Whether extraction measurement includes only legal harm or all empirically documented harm.').

omega_variable(
    coordination_extraction_boundary,
    'Is the absolutist doctrine''s coordination function (predictable speech rules) structurally separable from its extraction function (immunizing harmful speech), or are they inseparable?',
    'Natural experiment: jurisdictions with hate speech laws (Germany, Canada) — do they maintain robust public discourse? If yes, coordination and extraction are separable; the absolutist extraction is not the price of coordination.',
    'If separable, the constraint is a tangled_rope with separable components. If inseparable, the extraction is the necessary cost of the coordination — a different structural diagnosis.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_extraction_boundary, conceptual, 'Whether the coordination and extraction components of absolutist speech doctrine are structurally separable.').

omega_variable(
    identity_lock_mechanism_targets,
    'Is the identity_locked exit for targets_of_harmful_speech structural (immutable characteristics) or internalized (they believe they deserve the harm)?',
    'Post-exit suppression trajectory: if targets who leave public discourse still experience harm (online pursuit, doxxing, offline consequences), the lock is structural. If harm ceases upon exit, internalized component may dominate.',
    'If structural, the constraint''s effective suppression for targets is higher than the doctrinal measure suggests — they carry the targeting with them. If partially internalized, the constraint''s extraction includes psychological colonization.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_mechanism_targets, empirical, 'Structural vs. internalized mechanism of identity-locked exit for speech targets.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(speech_harm_boundary__absolutist_reading, 1919, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(spee_tr_t1919, speech_harm_boundary__absolutist_reading, theater_ratio, 1919, 0.05).
narrative_ontology:measurement(spee_tr_t1940, speech_harm_boundary__absolutist_reading, theater_ratio, 1940, 0.08).
narrative_ontology:measurement(spee_tr_t1969, speech_harm_boundary__absolutist_reading, theater_ratio, 1969, 0.1).
narrative_ontology:measurement(spee_tr_t1989, speech_harm_boundary__absolutist_reading, theater_ratio, 1989, 0.12).
narrative_ontology:measurement(spee_tr_t2000, speech_harm_boundary__absolutist_reading, theater_ratio, 2000, 0.12).
narrative_ontology:measurement(spee_tr_t2010, speech_harm_boundary__absolutist_reading, theater_ratio, 2010, 0.12).
narrative_ontology:measurement(spee_tr_t2024, speech_harm_boundary__absolutist_reading, theater_ratio, 2024, 0.12).

% Extraction over time
narrative_ontology:measurement(spee_be_t1919, speech_harm_boundary__absolutist_reading, base_extractiveness, 1919, 0.15).
narrative_ontology:measurement(spee_be_t1940, speech_harm_boundary__absolutist_reading, base_extractiveness, 1940, 0.2).
narrative_ontology:measurement(spee_be_t1969, speech_harm_boundary__absolutist_reading, base_extractiveness, 1969, 0.35).
narrative_ontology:measurement(spee_be_t1989, speech_harm_boundary__absolutist_reading, base_extractiveness, 1989, 0.52).
narrative_ontology:measurement(spee_be_t2000, speech_harm_boundary__absolutist_reading, base_extractiveness, 2000, 0.6).
narrative_ontology:measurement(spee_be_t2010, speech_harm_boundary__absolutist_reading, base_extractiveness, 2010, 0.66).
narrative_ontology:measurement(spee_be_t2024, speech_harm_boundary__absolutist_reading, base_extractiveness, 2024, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(spee_su_t1919, speech_harm_boundary__absolutist_reading, suppression_requirement, 1919, 0.6).
narrative_ontology:measurement(spee_su_t1940, speech_harm_boundary__absolutist_reading, suppression_requirement, 1940, 0.55).
narrative_ontology:measurement(spee_su_t1969, speech_harm_boundary__absolutist_reading, suppression_requirement, 1969, 0.4).
narrative_ontology:measurement(spee_su_t1989, speech_harm_boundary__absolutist_reading, suppression_requirement, 1989, 0.35).
narrative_ontology:measurement(spee_su_t2000, speech_harm_boundary__absolutist_reading, suppression_requirement, 2000, 0.35).
narrative_ontology:measurement(spee_su_t2010, speech_harm_boundary__absolutist_reading, suppression_requirement, 2010, 0.35).
narrative_ontology:measurement(spee_su_t2024, speech_harm_boundary__absolutist_reading, suppression_requirement, 2024, 0.35).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(speech_harm_boundary__absolutist_reading, information_standard).
narrative_ontology:boltzmann_floor_override(speech_harm_boundary__absolutist_reading, 0.03).
narrative_ontology:affects_constraint(speech_harm_boundary__absolutist_reading, speech_harm_boundary__harm_balancing_reading).
narrative_ontology:affects_constraint(speech_harm_boundary__absolutist_reading, speech_harm_boundary__dignity_reading).

% DUAL FORMULATION NOTE:
% This constraint is the absolutist_reading of the speech_harm_boundary kernel. It links to its two sibling readings. The kernel family shares the same constitutional text (First Amendment) but instantiates different constraints with different ε values, different beneficiary/victim structures, and different claimed types. The absolutist reading claims rope but computes tangled_rope; the balancing reading claims scaffold; the dignity reading claims mountain (dignity as natural law).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(speech_harm_boundary__absolutist_reading, institutional, 0.25).
constraint_indexing:directionality_override(speech_harm_boundary__absolutist_reading, powerless, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
