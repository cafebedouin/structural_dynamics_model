% ============================================================================
% CONSTRAINT STORY: speech_protection_boundary__absolutist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
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
 *   constraint_id: speech_protection_boundary__absolutist_reading
 *   human_readable: Absolutist Speech Protection (Brandenburg Standard)
 *   domain: constitutional_law/political_philosophy
 *
 * SUMMARY:
 *   The Brandenburg standard (imminent-lawless-action test) instantiates one
 *   constitutional reading of speech protection: speech is nearly absolutely
 *   protected except for direct incitement to immediate violence. This
 *   reading maximizes the protected set and minimizes the unprotected set to
 *   an extremely narrow domain. The constraint coordinates a genuine public
 *   interest—preventing government censorship of dissent—but structurally
 *   externalizes the costs of unregulated hate speech onto marginalized
 *   communities. The beneficiary set is asymmetric: institutional speakers
 *   and civil liberties organizations defending the standard benefit;
 *   powerless communities bearing the psychological and social harms of hate
 *   speech pay. This is the ABSOLUTIST READING, not the balancing or
 *   harm-limitation alternatives. The constraint is CLAIMED as tangled_rope
 *   because it coordinates protection-from-censorship while simultaneously
 *   extracting through exclusion of harm-based remedies. The authored metrics
 *   describe substantially extractive, actively enforced operation with
 *   moderate theater (the language of protection masks the exclusion
 *   mechanism).
 *
 * KEY AGENTS:
 *   - constitutional_originalist_jurists: institutional agenda-setters (power=institutional, exit=analytical) — set and enforce the Brandenburg doctrine through judicial opinion
 *   - speech_maximalists: organized beneficiaries (power=organized, exit=arbitrage) — institutionally dependent on the standard, defend it in courts and media
 *   - marginalized_communities_target_of_hate_speech: powerless payers (power=powerless, exit=identity_locked) — bear aggregate harm of unremediable hate speech
 *   - hate_speech_speakers: moderate beneficiaries (power=moderate, exit=mobile) — directly benefit from legal immunity for dehumanizing expression
 *   - competing_constitutional_frameworks_advocates: excluded organizers (power=organized, exit=constrained) — propose alternative readings but lack judicial authority to implement them
 *   - courts_enforcing_brandenburg: institutional agenda-setters (power=institutional, exit=analytical) — enforce through adjudication, ruling against harm-based claims
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(speech_protection_boundary__absolutist_reading, 0.68).
domain_priors:suppression_score(speech_protection_boundary__absolutist_reading, 0.41).
domain_priors:theater_ratio(speech_protection_boundary__absolutist_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(speech_protection_boundary__absolutist_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(speech_protection_boundary__absolutist_reading, suppression_requirement, 0.41).
narrative_ontology:constraint_metric(speech_protection_boundary__absolutist_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(speech_protection_boundary__absolutist_reading, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(speech_protection_boundary__absolutist_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(speech_protection_boundary__absolutist_reading, tangled_rope).
narrative_ontology:human_readable(speech_protection_boundary__absolutist_reading, "Absolutist Speech Protection (Brandenburg Standard)").
narrative_ontology:topic_domain(speech_protection_boundary__absolutist_reading, "constitutional_law/political_philosophy").

domain_priors:requires_active_enforcement(speech_protection_boundary__absolutist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(speech_protection_boundary__absolutist_reading, '2ff78a75-a19a-4c25-b0fd-01b0dd774115').
narrative_ontology:cs_kernel_codification('2ff78a75-a19a-4c25-b0fd-01b0dd774115', fixed_text).
narrative_ontology:cs_authority_grounding('2ff78a75-a19a-4c25-b0fd-01b0dd774115', lineage).
narrative_ontology:cs_interpretation_layer_present('2ff78a75-a19a-4c25-b0fd-01b0dd774115').
narrative_ontology:cs_reading_relation('2ff78a75-a19a-4c25-b0fd-01b0dd774115', speech_protection_boundary__balancing_reading, coexists_with).
narrative_ontology:cs_reading_relation('2ff78a75-a19a-4c25-b0fd-01b0dd774115', speech_protection_boundary__harm_limited_reading, coexists_with).
narrative_ontology:cs_axiom('2ff78a75-a19a-4c25-b0fd-01b0dd774115', foundational, speech_near_absolute_except_incitement).
narrative_ontology:cs_axiom_status(speech_near_absolute_except_incitement, holdable).
narrative_ontology:cs_axiom_grounding('2ff78a75-a19a-4c25-b0fd-01b0dd774115', speech_near_absolute_except_incitement, deontological).
narrative_ontology:cs_axiom('2ff78a75-a19a-4c25-b0fd-01b0dd774115', foundational, government_censorship_threat_primacy).
narrative_ontology:cs_axiom_status(government_censorship_threat_primacy, holdable).
narrative_ontology:cs_axiom_grounding('2ff78a75-a19a-4c25-b0fd-01b0dd774115', government_censorship_threat_primacy, empirically_contingent).
narrative_ontology:cs_reference_frame('2ff78a75-a19a-4c25-b0fd-01b0dd774115', first_amendment_absolutist_protection).
narrative_ontology:cs_drift_state('2ff78a75-a19a-4c25-b0fd-01b0dd774115', contemporary_pluralistic_democracy, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('2ff78a75-a19a-4c25-b0fd-01b0dd774115', '').
narrative_ontology:cs_kernel_id(speech_protection_boundary__absolutist_reading, speech_protection_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(speech_protection_boundary__absolutist_reading, speech_maximalists).
narrative_ontology:constraint_beneficiary(speech_protection_boundary__absolutist_reading, powerful_institutional_speakers).
narrative_ontology:constraint_victim(speech_protection_boundary__absolutist_reading, marginalized_communities_target_of_hate_speech).
narrative_ontology:constraint_victim(speech_protection_boundary__absolutist_reading, harassed_individuals_excluded_from_public_discourse).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(speech_protection_boundary__absolutist_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(speech_protection_boundary__absolutist_reading, 'none', 1).

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
 *   Extractiveness is high (0.68) because the constraint systematically excludes legal remedies for a category of harm (dignity violations, harassment, dehumanization) that other constitutional democracies address through balancing or harm exceptions. The exclusion is not incidental—it is the core of the absolutist reading. Suppression is moderate (0.41) because the constraint does not suppress the speech itself (hate speech is legal) but suppresses legal recourse for those harmed by it; the suppression is structural (legal bars to remedy) rather than violent. Theater is moderate (0.28): the language emphasizes protection-from-government-censorship (the genuine coordination function) while the operational effect is immunity-for-hate-speech (the extraction mechanism). The measurement series show extractiveness rising from t0 through t40 as the standard became more firmly entrenched and more fully internalized by younger cohorts of judges, then stabilizing at t50. Theater rose through t40 (increasing rhetorical emphasis on anti-censorship virtues) but stabilizes as the rhetoric becomes normalized. Suppression requirement is flat—the constraint requires consistent legal exclusion of harm remedies, which does not intensify or decay over this interval.
 *
 * PERSPECTIVAL GAP:
 *   The originalist jurists' seat perceives the constraint as pure coordination (protecting robust discourse against government abuse). The marginalized communities' seat perceives it as pure extraction (immunity for hate speech at the cost of their own legal remedies and social participation). Institutional speakers' seat perceives it as beneficial coordination (legal clarity, protected speech). Harassed individuals' seat perceives it as coercive (silencing through legal immunity for harassers). The engine computes per-seat directionality from beneficiary/victim declarations and exit options: originalists and institutional speakers compute near-beneficiary d (low extraction perceived from their position); marginalized communities compute near-target d (high extraction experienced from their position). The claim/metric gap is deliberate and diagnostic: the constraint is CLAIMED as tangled_rope (hybrid) because it genuinely coordinates against censorship while genuinely extracting through harm exclusion. The metrics describe the operational extractiveness and enforcement. This divergence is the point—false summits and mandate drift are detected by claim/metric misalignment.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries: speech_maximalists and powerful_institutional_speakers. They benefit from the widest protected set and highest threshold for legal restriction. They derive d near the beneficiary end because they have arbitrage-grade exit (can move their speech infrastructure) and they collect from the standard (institutional prestige, legal certainty, speech amplification). Victims: marginalized_communities_target_of_hate_speech and harassed_individuals_excluded_from_public_discourse. They bear the costs of the excluded remedies and experience identity-lock exit (cannot exit their community membership, so cannot exit the exposure to hate speech). They derive d near the target end because the constraint extracts from their legal entitlements and social standing. Agenda-setters: originalist jurists and courts. They have institutional power (can reinterpret or hold the standard) and analytical exit (theory-dependent, not material-dependent). Their d is modulated by their position as enforcers—they benefit from the standard's clarity and from defending constitutional doctrine, so d leans toward moderate benefit, but they experience professional resistance (critical scholars, policy pressure) that raises it toward symmetric. No directionality override is needed; the structural derivation captures the asymmetry.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint does not exhibit mandatrophy (atrophy of founding function). The founding problem was government censorship of political dissent; the founding function was to prevent that. That function remains live—courts do use Brandenburg to protect controversial speech from government prosecution. However, the constraint is exhibiting MANDATE DRIFT: the founding problem was narrow (protecting against state suppression of dissent) but the maintained scope is maximal (protecting against ANY legal restriction including harm-based civil remedies from non-state actors). The standard's proponents argue that maintaining the maximal scope is necessary to prevent government weaponization; critics argue the founding problem is solved and the maintained scope now functions primarily to shield institutional speakers and hate-speech producers from accountability. This is not mandatrophy (the function still exists and is still invoked) but rather FUNCTIONAL DRIFT (the primary beneficiary has shifted from dissidents-against-the-state to institutional-speakers-against-accountability). The tangled_rope classification captures this: the coordination function (protection from censorship) is real; the extraction function (exclusion of harm remedies) is also real and has become increasingly salient as the state suppression threat has receded and private hate speech has proliferated.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    founding_problem_persistence,
    'Is the founding problem (government suppression of political dissent) still a live threat, or has it been substantially solved by institutional checks and democratic norms?',
    'Comparative historical analysis of government censorship attempts across decades; empirical study of whether Brandenburg actually prevents suppression vs. whether institutional and democratic norms do. Cross-national comparison with balancing frameworks to assess whether they increase or decrease actual government suppression of dissent.',
    'If the founding problem is solved, the constraint''s scope becomes difficult to justify—it would be a mountain whose top has been climbed, now maintained for institutional reasons. If the problem is live, the maximal scope is proportional to a real threat. Classification uncertainty: snare (extraction maintained after founding problem solved) vs. tangled_rope (real coordination, collateral harms accepted). Measurement would shift this ambiguity.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(founding_problem_persistence, empirical, 'Whether the Brandenburg standard''s founding justification (preventing government censorship) remains applicable.').

omega_variable(
    harm_externality_magnitude,
    'What is the aggregate psychological, social, and speech-chilling harm experienced by marginalized communities from legally unremediable hate speech?',
    'Longitudinal studies of mental health outcomes, public-discourse participation, and educational/economic outcomes in cohorts exposed to coordinated hate speech. Comparative analysis with jurisdictions using balancing or harm-limited frameworks to measure whether inclusion of harm exceptions reduces documented harms. Testimony from harassed communities about lived experience of chilling effects.',
    'A substantial aggregate harm (measured in documented depression, social withdrawal, educational attainment loss) would establish the externality as non-negligible and raise questions about whether the coordination benefit justifies the cost. If harm is minor, the current framework is more defensible. High aggregate harm + measured benefits of Brandenburg suggests the constraint should be classified as snare (extraction exceeds coordination). Moderate harm + measured benefits suggests tangled_rope (genuine coordination with collateral costs). Low harm suggests rope (coordination with acceptable incidental effects).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(harm_externality_magnitude, empirical, 'Quantification of the harm externalized onto marginalized communities by the Brandenburg standard''s scope.').

omega_variable(
    institutional_lock_in_mechanism,
    'Is the Brandenburg standard maintained by its genuine legal/constitutional defensibility, or is it locked in by institutional inertia and the professional interests of the legal establishment?',
    'Historical analysis of how originalism became the dominant judicial philosophy; study of judicial turnover and nomination dynamics; comparative analysis of how other constitutional democracies arrived at different speech standards. Interview data from jurists about their reasoning for maintaining or challenging Brandenburg.',
    'If lock-in is primarily institutional/professional (originalist judges maintain Brandenburg because their interpretive framework privileges formalism), then the constraint is partially piton-like—maintained by theater and career incentives rather than substance. If lock-in is by genuine constitutional reasoning, the classification remains tangled_rope with debate over whether harms justify the scope. A finding of strong institutional lock-in would upgrade piton evidence and suggest the constraint is more performative than substantive.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_lock_in_mechanism, conceptual, 'Whether the Brandenburg standard persists because it is constitutionally correct or because it is institutionally entrenched.').

omega_variable(
    identity_lock_vs_structural_suppression,
    'Is the measured suppression (0.41) experienced by harassed individuals primarily structural (legal bars to remedy) or internalized (they have internalized the view that hate speech is normal and remedies are illegitimate)?',
    'Post-remedy thought experiment: if harm-exception provisions were enacted and harassed communities gained legal remedies, would their participation and psychological outcomes improve, or do they remain chilled because they have internalized suppression? Comparative study of communities in jurisdictions with harm exceptions—do they participate more robustly?',
    'If suppression is purely structural, remedies (legal recourse, platform moderation) would relieve it. If internalized, structural remedies are necessary but insufficient—psychological/social intervention also needed. High internalization + high structural suppression = effective suppression near the target end. The measured 0.41 may understate actual suppression if measurement captures structural barriers only and misses internalization.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_vs_structural_suppression, empirical, 'Whether suppression of harassed individuals'' speech is externally structural or partially self-perpetuating.').

omega_variable(
    kernel_reading_contest,
    'Which of the three constitutional readings (absolutist, balancing, harm-limited) is most defensible as an interpretation of the First Amendment text and original understanding?',
    'Constitutional scholarship and jurisprudence; originalist exegesis of the text and Founding-era understanding; living-constitution scholarship on constitutional evolution; international human-rights analysis of comparative speech protections; empirical study of which framework best serves democratic flourishing and equality.',
    'This is the kernel-level contestation. The absolutist reading claims it is the constitutionally correct interpretation; balancing and harm-limited readings claim to be more faithful. Resolution would not eliminate the other readings (they are held by different constituencies) but would clarify which has stronger constitutional grounding. This is CONCEPTUAL + PREFERENCE: the question is partly about what the text means (empirical) and partly about what constitutional values should be prioritized (normative).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'The foundational constitutional dispute: whether Brandenburg is the correct interpretation of the First Amendment.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(speech_protection_boundary__absolutist_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(spee_tr_t0, speech_protection_boundary__absolutist_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(spee_tr_t8, speech_protection_boundary__absolutist_reading, theater_ratio, 8, 0.15).
narrative_ontology:measurement(spee_tr_t16, speech_protection_boundary__absolutist_reading, theater_ratio, 16, 0.18).
narrative_ontology:measurement(spee_tr_t24, speech_protection_boundary__absolutist_reading, theater_ratio, 24, 0.22).
narrative_ontology:measurement(spee_tr_t32, speech_protection_boundary__absolutist_reading, theater_ratio, 32, 0.26).
narrative_ontology:measurement(spee_tr_t40, speech_protection_boundary__absolutist_reading, theater_ratio, 40, 0.29).
narrative_ontology:measurement(spee_tr_t50, speech_protection_boundary__absolutist_reading, theater_ratio, 50, 0.28).

% Extraction over time
narrative_ontology:measurement(spee_be_t0, speech_protection_boundary__absolutist_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(spee_be_t8, speech_protection_boundary__absolutist_reading, base_extractiveness, 8, 0.48).
narrative_ontology:measurement(spee_be_t16, speech_protection_boundary__absolutist_reading, base_extractiveness, 16, 0.55).
narrative_ontology:measurement(spee_be_t24, speech_protection_boundary__absolutist_reading, base_extractiveness, 24, 0.62).
narrative_ontology:measurement(spee_be_t32, speech_protection_boundary__absolutist_reading, base_extractiveness, 32, 0.67).
narrative_ontology:measurement(spee_be_t40, speech_protection_boundary__absolutist_reading, base_extractiveness, 40, 0.71).
narrative_ontology:measurement(spee_be_t50, speech_protection_boundary__absolutist_reading, base_extractiveness, 50, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(spee_su_t0, speech_protection_boundary__absolutist_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(spee_su_t8, speech_protection_boundary__absolutist_reading, suppression_requirement, 8, 0.37).
narrative_ontology:measurement(spee_su_t16, speech_protection_boundary__absolutist_reading, suppression_requirement, 16, 0.39).
narrative_ontology:measurement(spee_su_t24, speech_protection_boundary__absolutist_reading, suppression_requirement, 24, 0.41).
narrative_ontology:measurement(spee_su_t32, speech_protection_boundary__absolutist_reading, suppression_requirement, 32, 0.42).
narrative_ontology:measurement(spee_su_t40, speech_protection_boundary__absolutist_reading, suppression_requirement, 40, 0.41).
narrative_ontology:measurement(spee_su_t50, speech_protection_boundary__absolutist_reading, suppression_requirement, 50, 0.41).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(speech_protection_boundary__absolutist_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(speech_protection_boundary__absolutist_reading, 0.12).
narrative_ontology:affects_constraint(speech_protection_boundary__absolutist_reading, speech_protection_boundary__balancing_reading).
narrative_ontology:affects_constraint(speech_protection_boundary__absolutist_reading, speech_protection_boundary__harm_limited_reading).
narrative_ontology:affects_constraint(speech_protection_boundary__absolutist_reading, hate_speech_legal_liability).
narrative_ontology:affects_constraint(speech_protection_boundary__absolutist_reading, platform_content_moderation_policy).
narrative_ontology:affects_constraint(speech_protection_boundary__absolutist_reading, government_censorship_boundary).

% DUAL FORMULATION NOTE:
% The speech_protection_boundary kernel has three distinct constraint readings: absolutist (this file), balancing, and harm-limited. Each instantiates a different ε and beneficiary/victim structure from the same constitutional commitment. They are linked as a constraint family because each reading cites the others as alternatives it rejects. The absolutist reading influences the balancing and harm-limited readings by setting the current judicial standard they must work against; in turn, those readings' growing scholarly and advocacy support creates pressure on the absolutist reading's institutional legitimacy.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
