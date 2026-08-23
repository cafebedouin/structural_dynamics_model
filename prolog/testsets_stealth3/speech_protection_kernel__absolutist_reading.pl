% ============================================================================
% CONSTRAINT STORY: speech_protection_kernel__absolutist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_speech_protection_kernel__absolutist_reading, []).

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
 *   constraint_id: speech_protection_kernel__absolutist_reading
 *   human_readable: Near-Categorical Speech Protection — Absolutist Reading
 *   domain: constitutional_law/political_philosophy/communication_rights
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the speech-protection kernel: the
 *   absolutist reading, under which expression is protected
 *   near-categorically and listener harm — offense, distress, felt
 *   insecurity, group-targeted degradation short of the narrow exclusions —
 *   is inadmissible as a ground for restriction. The standing arrangement
 *   under contest is that regime itself, and every metric below describes
 *   that regime as the absolutist framing assesses it. The colloquial label
 *   'free speech protection' decomposes, per the epsilon-invariance
 *   principle, into five structurally distinct readings held by different
 *   parties; this file is the widest-boundary member of that family, linked
 *   to its siblings through the network block. KEY AGENTS (by structural
 *   relationship): - constitutional_courts: agenda-setter
 *   (institutional/constrained) — administers the rule, defines the narrow
 *   exclusions - speakers_at_large: diffuse beneficiary (moderate/mobile) -
 *   dissident_minority_speakers: concentrated intended beneficiary
 *   (powerless/constrained) - commercial_speech_carriers: beneficiary
 *   capturing material gains (powerful/arbitrage) -
 *   targets_of_group_directed_harmful_speech: primary payer
 *   (powerless/trapped) - severe_harassment_targets: secondary payer
 *   (moderate/trapped) - harm_regulation_advocates: excluded seat — their
 *   premise is inadmissible inside the framework -
 *   comparative_constitutional_scholars: analytical observer.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(speech_protection_kernel__absolutist_reading, 0.4).
domain_priors:suppression_score(speech_protection_kernel__absolutist_reading, 0.54).
domain_priors:theater_ratio(speech_protection_kernel__absolutist_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(speech_protection_kernel__absolutist_reading, extractiveness, 0.4).
narrative_ontology:constraint_metric(speech_protection_kernel__absolutist_reading, suppression_requirement, 0.54).
narrative_ontology:constraint_metric(speech_protection_kernel__absolutist_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(speech_protection_kernel__absolutist_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(speech_protection_kernel__absolutist_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(speech_protection_kernel__absolutist_reading, tangled_rope).
narrative_ontology:human_readable(speech_protection_kernel__absolutist_reading, "Near-Categorical Speech Protection — Absolutist Reading").
narrative_ontology:topic_domain(speech_protection_kernel__absolutist_reading, "constitutional_law/political_philosophy/communication_rights").

domain_priors:requires_active_enforcement(speech_protection_kernel__absolutist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(speech_protection_kernel__absolutist_reading, 'be03d579-2c84-42aa-89bf-9aabcd10db32').
narrative_ontology:cs_kernel_codification('be03d579-2c84-42aa-89bf-9aabcd10db32', fixed_text).
narrative_ontology:cs_authority_grounding('be03d579-2c84-42aa-89bf-9aabcd10db32', lineage).
narrative_ontology:cs_interpretation_layer_present('be03d579-2c84-42aa-89bf-9aabcd10db32').
narrative_ontology:cs_reading_relation('be03d579-2c84-42aa-89bf-9aabcd10db32', speech_protection_kernel__harm_threshold_reading, forecloses).
narrative_ontology:cs_reading_relation('be03d579-2c84-42aa-89bf-9aabcd10db32', speech_protection_kernel__dignity_reading, forecloses).
narrative_ontology:cs_reading_relation('be03d579-2c84-42aa-89bf-9aabcd10db32', speech_protection_kernel__marketplace_reading, influences).
narrative_ontology:cs_reading_relation('be03d579-2c84-42aa-89bf-9aabcd10db32', speech_protection_kernel__democratic_participation_reading, coexists_with).
narrative_ontology:cs_axiom('be03d579-2c84-42aa-89bf-9aabcd10db32', foundational, listener_harm_not_a_restrictive_ground).
narrative_ontology:cs_axiom_status(listener_harm_not_a_restrictive_ground, holdable).
narrative_ontology:cs_axiom_grounding('be03d579-2c84-42aa-89bf-9aabcd10db32', listener_harm_not_a_restrictive_ground, deontological).
narrative_ontology:cs_axiom('be03d579-2c84-42aa-89bf-9aabcd10db32', secondary, viewpoint_neutral_near_categorical_protection).
narrative_ontology:cs_axiom_status(viewpoint_neutral_near_categorical_protection, holdable).
narrative_ontology:cs_axiom_grounding('be03d579-2c84-42aa-89bf-9aabcd10db32', viewpoint_neutral_near_categorical_protection, conventional).
narrative_ontology:cs_reference_frame('be03d579-2c84-42aa-89bf-9aabcd10db32', categorical_speaker_autonomy_baseline).
narrative_ontology:cs_drift_state('be03d579-2c84-42aa-89bf-9aabcd10db32', contemporary_platform_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('be03d579-2c84-42aa-89bf-9aabcd10db32', '').
narrative_ontology:cs_kernel_id(speech_protection_kernel__absolutist_reading, speech_protection_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(speech_protection_kernel__absolutist_reading, speakers_at_large).
narrative_ontology:constraint_beneficiary(speech_protection_kernel__absolutist_reading, dissident_minority_speakers).
narrative_ontology:constraint_beneficiary(speech_protection_kernel__absolutist_reading, commercial_speech_carriers).
narrative_ontology:constraint_victim(speech_protection_kernel__absolutist_reading, targets_of_group_directed_harmful_speech).
narrative_ontology:constraint_victim(speech_protection_kernel__absolutist_reading, severe_harassment_targets).
narrative_ontology:constraint_vindicates(speech_protection_kernel__absolutist_reading, speaker_autonomy_doctrine).
narrative_ontology:constraint_vindicates(speech_protection_kernel__absolutist_reading, viewpoint_neutrality_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interpret the free-expression guarantee, strike down restriction proposals that rest on listener-offense or listener-distress grounds, and define the short list of categorical exclusions (imminent incitement, true threats and the like). Their institutional prestige is bound up with the stability of the doctrine; revising it toward harm-balancing carries legitimacy costs they rarely pay, so the doctrine tends to be maintained as-is and adjusted only at the margins.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__absolutist_reading, constitutional_courts, agenda_setter,
    institutional, generational, constrained, national).

% Speak in public without needing to anticipate that someone's offense or distress at their expression could become a legal ground for suppressing it. The benefit is diffuse and unconditional: whatever they want to say, the rule covers it the same way. They bear essentially none of the rule's costs.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__absolutist_reading, speakers_at_large, beneficiary,
    moderate, biographical, mobile, national).

% Hold views that majorities find harmful, offensive, or dangerous and that would be the first candidates for restriction under any harm-balancing standard. The near-categorical rule is what keeps their expression lawful; without it their realistic options are self-silencing or risking prosecution. They gain the most and can offer nothing in return except compliance with the narrow exclusions.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__absolutist_reading, dissident_minority_speakers, beneficiary,
    powerless, biographical, constrained, national).

% Members of groups routinely addressed by degrading, dehumanizing, or subordinating public expression. The injury is recurring and cumulative across generations of the group, but under this arrangement their harm is defined as inadmissible as a ground for restriction. Their available responses reduce to answering speech, private avoidance, and exit from shared discursive spaces — and the exposure itself is ambient, so leaving one venue does not leave the environment.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__absolutist_reading, targets_of_group_directed_harmful_speech, payer,
    powerless, biographical, trapped, national).

% Individuals subjected to sustained, coordinated harassing expression that falls short of the true-threat and incitement exclusions. They obtain no legal relief under the near-categorical rule; the conduct follows them across venues and platforms, and the practical exit is withdrawal from public online life, which carries its own professional and social costs.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__absolutist_reading, severe_harassment_targets, payer,
    moderate, biographical, trapped, national).

% Platforms, broadcasters, and publishers that move protected expression at scale. The widest protection boundary minimizes their legal exposure for carried speech, and engagement-driven economics concentrate attention — and revenue — on emotionally charged protected content. They operate across jurisdictions, can relocate or lobby, and bear little of the rule's cost burden themselves.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__absolutist_reading, commercial_speech_carriers, beneficiary,
    powerful, biographical, arbitrage, global).

% Civil-rights organizers and dignity-tradition jurists who argue that demonstrable harm to listeners and target groups should count as a ground for limiting expression. Within this arrangement their core premise is inadmissible, so they pursue their aims through politics, private-pressure campaigns, and comparative citation of jurisdictions with different arrangements rather than through the doctrinal channel.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__absolutist_reading, harm_regulation_advocates, excluded,
    organized, biographical, constrained, national).

% Study how differently constituted speech-protection arrangements perform across jurisdictions and over time — dissident-protection records, target-group outcomes, carrier behavior. They take no side in the arrangement and neither collect from it nor pay into it.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__absolutist_reading, comparative_constitutional_scholars, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(speech_protection_kernel__absolutist_reading, commercial_speech_carriers).
narrative_ontology:fixing_cost_class(speech_protection_kernel__absolutist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: A bright-line, near-categorical rule solves the who-defines-harm problem: any discretionary power to restrict expression on harm or offense grounds is available to majorities and incumbents, and history shows it is used first against dissenters. Fixing the boundary in advance removes that discretion, lowers case-by-case adjudication costs, and prevents the chilling effect that vague balancing standards produce.
% TRANSFER_FUNCTION: Moves expressive freedom maximally to speakers and carriers, and moves the costs of harmful-but-protected expression — offense, fear, subordination, harassment burden — onto targeted listeners and groups, who receive no legal remedy short of the narrow categorical exclusions.
% ABSENT_VOICES: Targets of group-directed subordinating speech and severe harassment victims: their injury claims are defined out of the framework as inadmissible grounds, so the seats that bear the arrangement's costs are constitutionally unable to state them inside the doctrinal conversation. Dignity-tradition voices from other jurisdictions are likewise outside the admissible premise set.
% DISAPPEARANCE_RATIONALE: If the near-categorical rule vanished overnight, legislatures would enact offense- and harm-based restrictions within a session; dissident and minority expression would become regulable on case-by-case judgments; carriers would face liability exposure and begin pre-filtering; the entire expressive landscape would reorganize around whoever controls the harm definition.
% FOUNDING_PROBLEM: The arrangement was built against the historical record of seditious libel, blasphemy prosecution, licensing regimes, and morality-based suppression — recurring episodes in which 'harm to listeners or society' was the stated ground and silencing dissent was the effect.
% FOUNDING_PROBLEM_CORROBORATION: Historians of censorship and comparative constitutional scholars document continuing sedition, blasphemy, and disinformation prosecutions across jurisdictions outside the benefiting parties; the pattern that motivated the rule recurs in each media era. Target-group advocates attest a second, different problem the rule does not address, but the founding problem itself — censorial abuse of harm discretion — is corroborated from outside the beneficiary set.
narrative_ontology:disappearance_verdict(speech_protection_kernel__absolutist_reading, world_rearranges).
narrative_ontology:founding_problem_status(speech_protection_kernel__absolutist_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(speech_protection_kernel__absolutist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(speech_protection_kernel__absolutist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(speech_protection_kernel__absolutist_reading, 0.4, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(speech_protection_kernel__absolutist_reading_tests).
:- end_tests(speech_protection_kernel__absolutist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is authored at 0.40 through the reading's own lights: the arrangement is fundamentally protective of speaker autonomy, but the reading itself concedes a real, recurring cost-shift — injuries it declares inadmissible land on identifiable people with no remedy, and media density has amplified the reach of protected harmful expression over the interval, which the rising series records. Suppression is 0.54 as a RAW structural property, unscaled by power or scope: the rule operates by actively striking down an entire class of legislative attempts, so its persistence requires continuous enforcement effort against would-be restrictors; only extractiveness is scaled by directionality and scope in the engine's computation. Theater is low-to-moderate (0.25) and rising slowly: the doctrine performs real work, but rhetorical absolutism increasingly coexists with operational carve-out expansion, so a growing share of absolutist activity is ceremonial. Accessibility collapse is 0.55 — within the framework, accepting the axiom collapses harm-balancing alternatives almost entirely, but counter-speech, content-neutral regulation, private moderation, and rival-jurisdiction arrangements remain genuinely available. Resistance is 0.60: sustained advocacy for harm-based limits and divergent dignity-tradition jurisdictions contest the boundary continuously. All three tracked series run on one shared time grid (points 0/20/40/60/80/100) so no metric row borrows another's end-state values.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute very different experiences of the same rule. From the dissident-speaker seat the arrangement is the thing that makes their public existence legally possible; from the target-group seat the identical structure is an enforced immunity for those who degrade them, with the courthouse door closed by design; from the carrier seat it is a liability shield that monetizes well; from the bench it is the discipline that keeps the state out of the opinion business. The engine computes per-seat classifications from the structural data — the trapped exit of the payer seats versus the arbitrage exit of the carrier seat is what drives the divergence, not any authored verdict.
 *
 * DIRECTIONALITY LOGIC:
 *   Speakers at large and dissident minority speakers sit near the beneficiary end: the rule subsidizes their expression and costs them nothing measurable. Commercial carriers also derive low directionality from their beneficiary position, but unlike the speaker seats they demonstrably capture the arrangement's material gains — hence the receipt surface names them. Targets of group-directed harmful speech and severe harassment targets sit near the full-target end: they bear the transferred costs, their exit is trapped (ambient exposure cannot be exited venue-by-venue), and trapping amplifies their effective extraction. The courts, as agenda-setters, collect legitimacy rents from administering the rule but bear few of its costs; the derivation from their role and constrained exit places them moderately, and no override was needed because role-plus-exit already yields the right relationship.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — censorial abuse of harm discretion — is still live: every media era generates new harm claims (sedition, blasphemy, disinformation, offense) pressed into restriction, and the historical record corroborates this from outside the benefiting parties. The arrangement therefore has not outlived its mandate and is not mandatrophy-resolved. The classification work this story performs is boundary-keeping in both directions: the named victims and the asymmetric cost-transfer prevent mislabeling the arrangement as a pure coordination device, while the genuine, primary who-defines-harm coordination function — which the dissident seat experiences directly — prevents mislabeling it as pure extraction. Both facts are structural and simultaneous; that conjunction is what the tangled-rope claim asserts.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sibling_reading_structural_delta,
    'This constraint is one reading of speech_protection_kernel — what structurally changes if a sibling reading is adopted instead?',
    'Compile and compare the five sibling stories: boundary width, victim remediability, seat-level directionality, and per-seat computed types under each reading.',
    'Adopting the harm_threshold or dignity readings shrinks the protected boundary and converts the current payer seats into remediable claimants, collapsing this reading''s asymmetric cost-transfer; adopting the marketplace or democratic_participation readings preserves a wide boundary but re-grounds it, changing which seats the arrangement protects most.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_structural_delta, conceptual, 'Committer structure: reading-indexed delta across the speech_protection_kernel family.').

omega_variable(
    disagreement_location_margin,
    'Where exactly in the structure do the readings disagree — and is the absolutist''s narrow categorical exclusions list a stable stopping point or an arbitrary line on a continuum?',
    'Doctrinal analysis of which grounds the exclusions turn on (imminence, specificity, identifiability of threat) versus the grounds sibling readings admit (demonstrable harm, subordinating function, participatory value).',
    'If the exclusions rest on principled criteria, the absolutist boundary is a stable constraint; if they rest on administrability alone, the boundary is a moving line and the reading''s near-categorical character erodes from inside.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(disagreement_location_margin, conceptual, 'Location of the inter-reading contest: admissibility of listener harm as a restriction ground.').

omega_variable(
    coordination_function_vs_carrier_capture,
    'Does the bright-line rule still primarily solve the who-defines-harm coordination problem, or has gain capture by commercial carriers become the arrangement''s dominant operative effect?',
    'Cross-jurisdiction and cross-era comparison: dissident-protection outcomes under wide-boundary versus harm-threshold arrangements, set against concentration of attention-economy revenue under wide-boundary arrangements.',
    'If dissident protection dominates, the coordination function is primary and the tangled-rope reading holds; if carrier revenue effects dominate while dissident protection is incidental, the arrangement drifts toward extraction wearing a coordination story.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_function_vs_carrier_capture, empirical, 'Whether the coordination function or carrier capture is the arrangement''s dominant effect.').

omega_variable(
    exclusion_boundary_stability,
    'Are the narrow categorical exclusions stable over time, or is the set expanding — true-threat refinement, harassment doctrines, incitement reinterpretation — such that practice has drifted from the near-categorical reference frame?',
    'Time-series of recognized exclusions and of successful harm-adjacent restrictions within wide-boundary jurisdictions, controlling for headline doctrine versus applied outcomes.',
    'A steadily widening exclusion set would date a practice-drift transition and progressively shrink the beneficiary-side subsidy; a stable set supports the reading''s self-description.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(exclusion_boundary_stability, empirical, 'Stability of the categorical-exclusion boundary over the interval.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(speech_protection_kernel__absolutist_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(spee_tr_t0, speech_protection_kernel__absolutist_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement_basis(spee_tr_t0, observed).
narrative_ontology:measurement(spee_tr_t20, speech_protection_kernel__absolutist_reading, theater_ratio, 20, 0.14).
narrative_ontology:measurement_basis(spee_tr_t20, observed).
narrative_ontology:measurement(spee_tr_t40, speech_protection_kernel__absolutist_reading, theater_ratio, 40, 0.17).
narrative_ontology:measurement_basis(spee_tr_t40, observed).
narrative_ontology:measurement(spee_tr_t60, speech_protection_kernel__absolutist_reading, theater_ratio, 60, 0.2).
narrative_ontology:measurement_basis(spee_tr_t60, observed).
narrative_ontology:measurement(spee_tr_t80, speech_protection_kernel__absolutist_reading, theater_ratio, 80, 0.23).
narrative_ontology:measurement_basis(spee_tr_t80, observed).
narrative_ontology:measurement(spee_tr_t100, speech_protection_kernel__absolutist_reading, theater_ratio, 100, 0.25).
narrative_ontology:measurement_basis(spee_tr_t100, observed).

% Extraction over time
narrative_ontology:measurement(spee_be_t0, speech_protection_kernel__absolutist_reading, base_extractiveness, 0, 0.26).
narrative_ontology:measurement_basis(spee_be_t0, observed).
narrative_ontology:measurement(spee_be_t20, speech_protection_kernel__absolutist_reading, base_extractiveness, 20, 0.29).
narrative_ontology:measurement_basis(spee_be_t20, observed).
narrative_ontology:measurement(spee_be_t40, speech_protection_kernel__absolutist_reading, base_extractiveness, 40, 0.32).
narrative_ontology:measurement_basis(spee_be_t40, observed).
narrative_ontology:measurement(spee_be_t60, speech_protection_kernel__absolutist_reading, base_extractiveness, 60, 0.35).
narrative_ontology:measurement_basis(spee_be_t60, observed).
narrative_ontology:measurement(spee_be_t80, speech_protection_kernel__absolutist_reading, base_extractiveness, 80, 0.38).
narrative_ontology:measurement_basis(spee_be_t80, observed).
narrative_ontology:measurement(spee_be_t100, speech_protection_kernel__absolutist_reading, base_extractiveness, 100, 0.4).
narrative_ontology:measurement_basis(spee_be_t100, observed).

% Suppression requirement over time
narrative_ontology:measurement(spee_su_t0, speech_protection_kernel__absolutist_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement_basis(spee_su_t0, observed).
narrative_ontology:measurement(spee_su_t20, speech_protection_kernel__absolutist_reading, suppression_requirement, 20, 0.48).
narrative_ontology:measurement_basis(spee_su_t20, observed).
narrative_ontology:measurement(spee_su_t40, speech_protection_kernel__absolutist_reading, suppression_requirement, 40, 0.58).
narrative_ontology:measurement_basis(spee_su_t40, observed).
narrative_ontology:measurement(spee_su_t60, speech_protection_kernel__absolutist_reading, suppression_requirement, 60, 0.62).
narrative_ontology:measurement_basis(spee_su_t60, observed).
narrative_ontology:measurement(spee_su_t80, speech_protection_kernel__absolutist_reading, suppression_requirement, 80, 0.58).
narrative_ontology:measurement_basis(spee_su_t80, observed).
narrative_ontology:measurement(spee_su_t100, speech_protection_kernel__absolutist_reading, suppression_requirement, 100, 0.54).
narrative_ontology:measurement_basis(spee_su_t100, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(speech_protection_kernel__absolutist_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(speech_protection_kernel__absolutist_reading, speech_protection_kernel__harm_threshold_reading).
narrative_ontology:affects_constraint(speech_protection_kernel__absolutist_reading, speech_protection_kernel__marketplace_reading).
narrative_ontology:affects_constraint(speech_protection_kernel__absolutist_reading, speech_protection_kernel__dignity_reading).
narrative_ontology:affects_constraint(speech_protection_kernel__absolutist_reading, speech_protection_kernel__democratic_participation_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'speech protection' decomposes into five structurally distinct readings of the speech_protection_kernel, each with its own epsilon, beneficiary/victim structure, and classification. This file instantiates the widest-boundary member (absolutist_reading). The upstream members (marketplace, democratic_participation) supply rationales that legitimize wide boundaries; the downstream members (harm_threshold, dignity) contest the boundary's width and would restructure the victim set. Edges here record this reading's structural influence on its siblings; each sibling story links back.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
