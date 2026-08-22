% ============================================================================
% CONSTRAINT STORY: first_amendment_speech_protection__harm_limited_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_first_amendment_speech_protection__harm_limited_reading, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: first_amendment_speech_protection__harm_limited_reading
 *   human_readable: First Amendment Speech Protection Yields to Demonstrable Harm
 *   domain: constitutional_law/political_philosophy
 *
 * SUMMARY:
 *   This constraint instantiates the harm-limited reading of First Amendment
 *   protection: speech retains constitutional protection only insofar as it
 *   does not cause demonstrable unconsented-to harm. The reading vests courts
 *   and legislatures with authority to regulate speech beyond the narrow
 *   historical exclusions (incitement, true threats, defamation) when
 *   evidence of harm is produced. Vulnerable minorities benefit from
 *   institutional protection; speakers whose expression causes harm become
 *   regulatory targets. The reading is contested by absolutists (who hold the
 *   First Amendment permits no harm-balancing) and by civil libertarians (who
 *   worry the harm category becomes a cover for suppressing disfavored
 *   speech). The claim and metrics are intentionally divergent: CLAIMED as
 *   tangled_rope (genuine coordination function—protecting harm victims—AND
 *   asymmetric extraction—regulating speakers), but authored metrics show
 *   high suppression (0.71) and rising extractiveness (0.48→0.68 over 40
 *   periods), suggesting the regulatory mechanism is increasingly deployed as
 *   content control beyond harm-mitigation.
 *
 * KEY AGENTS:
 *   - vulnerable_minorities: powerless targets of unconsented-to harmful speech; benefit from institutional protection under the reading; trapped (cannot exit national discourse); zero bargaining power
 *   - speakers_causing_harm: moderate power; face regulatory liability and speech restriction; constrained exit (can relocate but at cost)
 *   - state_regulatory_authority: institutional agenda-setter; determines harm categories and enforces boundaries; directs the constraint's operation
 *   - speech_absolutists: organized advocates excluded from the consensus the reading instantiates; attest the reading violates constitutional text
 *   - platforms_and_intermediaries: institutional dual position—bear enforcement costs (payer) while enjoying liability protection (beneficiary); mobile exit via geographic arbitrage
 *   - courts_and_judges: analytical seat; adjudicate disputes and establish precedent; interpret the reading's scope and application
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(first_amendment_speech_protection__harm_limited_reading, 0.68).
domain_priors:suppression_score(first_amendment_speech_protection__harm_limited_reading, 0.71).
domain_priors:theater_ratio(first_amendment_speech_protection__harm_limited_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(first_amendment_speech_protection__harm_limited_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(first_amendment_speech_protection__harm_limited_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(first_amendment_speech_protection__harm_limited_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(first_amendment_speech_protection__harm_limited_reading, accessibility_collapse, 0.63).
narrative_ontology:constraint_metric(first_amendment_speech_protection__harm_limited_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(first_amendment_speech_protection__harm_limited_reading, tangled_rope).
narrative_ontology:human_readable(first_amendment_speech_protection__harm_limited_reading, "First Amendment Speech Protection Yields to Demonstrable Harm").
narrative_ontology:topic_domain(first_amendment_speech_protection__harm_limited_reading, "constitutional_law/political_philosophy").

domain_priors:requires_active_enforcement(first_amendment_speech_protection__harm_limited_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(first_amendment_speech_protection__harm_limited_reading, 'e55e8dbf-293f-48e4-a5e4-eae5cba07f26').
narrative_ontology:cs_kernel_codification('e55e8dbf-293f-48e4-a5e4-eae5cba07f26', fixed_text).
narrative_ontology:cs_authority_grounding('e55e8dbf-293f-48e4-a5e4-eae5cba07f26', lineage).
narrative_ontology:cs_interpretation_layer_present('e55e8dbf-293f-48e4-a5e4-eae5cba07f26').
narrative_ontology:cs_reading_relation('e55e8dbf-293f-48e4-a5e4-eae5cba07f26', first_amendment_speech_protection__absolutist_reading, coexists_with).
narrative_ontology:cs_reading_relation('e55e8dbf-293f-48e4-a5e4-eae5cba07f26', first_amendment_speech_protection__categorical_balancing_reading, influences).
narrative_ontology:cs_axiom('e55e8dbf-293f-48e4-a5e4-eae5cba07f26', foundational, demonstrable_harm_legitimate_boundary).
narrative_ontology:cs_axiom_status(demonstrable_harm_legitimate_boundary, holdable).
narrative_ontology:cs_axiom_grounding('e55e8dbf-293f-48e4-a5e4-eae5cba07f26', demonstrable_harm_legitimate_boundary, empirically_contingent).
narrative_ontology:cs_axiom('e55e8dbf-293f-48e4-a5e4-eae5cba07f26', foundational, protection_yields_to_harm_proof).
narrative_ontology:cs_axiom_status(protection_yields_to_harm_proof, holdable).
narrative_ontology:cs_axiom_grounding('e55e8dbf-293f-48e4-a5e4-eae5cba07f26', protection_yields_to_harm_proof, deontological).
narrative_ontology:cs_reference_frame('e55e8dbf-293f-48e4-a5e4-eae5cba07f26', harm_proof_limiting_principle).
narrative_ontology:cs_drift_state('e55e8dbf-293f-48e4-a5e4-eae5cba07f26', contemporary_regulatory_expansion, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('e55e8dbf-293f-48e4-a5e4-eae5cba07f26', '').
narrative_ontology:cs_kernel_id(first_amendment_speech_protection__harm_limited_reading, first_amendment_speech_protection).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(first_amendment_speech_protection__harm_limited_reading, vulnerable_minorities).
narrative_ontology:constraint_beneficiary(first_amendment_speech_protection__harm_limited_reading, harm_victims).
narrative_ontology:constraint_victim(first_amendment_speech_protection__harm_limited_reading, speakers_causing_unconsented_harm).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(first_amendment_speech_protection__harm_limited_reading, platforms_and_intermediaries).
narrative_ontology:constraint_victim(first_amendment_speech_protection__harm_limited_reading, platforms_and_intermediaries).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Groups historically subject to targeted speech that causes material harm—incitement to violence, doxxing, harassment, defamatory targeting. Under the harm-limited reading, their exposure to such speech is reduced by regulatory intervention. They cannot opt out of public discourse or leave the jurisdiction; the constraint protects them in place.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__harm_limited_reading, vulnerable_minorities, beneficiary,
    powerless, generational, trapped, national).

% Individuals and organizations whose speech is regulated or prohibited when it causes demonstrable unconsented-to harm. They bear the cost of constraint enforcement: legal liability, content takedown, platform suspension, or criminal prosecution. Their exit option is to relocate to jurisdictions with weaker harm-balancing standards, but exit carries reputational and economic cost.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__harm_limited_reading, speakers_causing_unconsented_harm, payer,
    moderate, biographical, constrained, national).

% Courts and legislatures that determine what counts as 'demonstrable harm' and enforce the boundary. They set the evidentiary standard, define harm categories, adjudicate disputed claims, and levy penalties. The harm-limited reading vests them with authority to regulate speech beyond the narrow historical exclusions (incitement, true threats, defamation).
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__harm_limited_reading, state_regulatory_authority, agenda_setter,
    institutional, generational, arbitrage, national).

% Advocates who hold that the First Amendment permits no balancing between speech protection and harm reduction—that categorical protection is the only coherent reading. They are excluded from the consensus the harm-limited reading instantiates; their position is that the reading itself violates constitutional text.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__harm_limited_reading, speech_absolutists, excluded,
    organized, generational, constrained, national).

% Private online platforms that must enforce harm-based content rules under the reading's framework. They bear enforcement costs (moderation, legal review, appeals processing) and face liability if they fail to remove harm-causing speech. They also benefit from reduced liability if they promptly remove flagged harmful content. Their exit is geographic—they can operate under different jurisdictional standards in different regions.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__harm_limited_reading, platforms_and_intermediaries, payer,
    institutional, biographical, mobile, global).
narrative_ontology:stakeholder_secondary_role(first_amendment_speech_protection__harm_limited_reading, platforms_and_intermediaries, beneficiary).

% Institutions charged with interpreting the First Amendment and determining when speech protection yields to harm-mitigation. They adjudicate disputes, review regulatory classifications, and establish precedent. Their seat is analytical—they observe the constraint's operation across all other seats.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__harm_limited_reading, courts_and_judges, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(first_amendment_speech_protection__harm_limited_reading, state_regulatory_authority).
narrative_ontology:fixing_cost_class(first_amendment_speech_protection__harm_limited_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Balances individual expression rights against collective protection from demonstrable harm. Provides a rule-of-law framework for distinguishing harmful speech (subject to regulation) from protected speech (subject to no regulation except the narrowest historical exclusions). Coordinates between speakers' liberty and harm victims' security.
% TRANSFER_FUNCTION: Transfers regulatory authority to state institutions to suppress speech deemed to cause demonstrable unconsented-to harm. The cost borne by speakers is legal liability and reduced speech scope; the benefit accrues to vulnerable populations and harm-defined victims as reduced exposure and institutional protection.
% ABSENT_VOICES: Absolutist speech advocates who reject harm-balancing on principle are structurally excluded from the consensus this reading constitutes. They would testify that the reading betrays the First Amendment's text and historical meaning. Civil libertarians who worry about regulatory scope creep and mission drift in 'harm' definitions are also partly excluded—their concerns are heard in litigation but the harm-limited framework institutionalizes the balance rather than preserving the tension.
% DISAPPEARANCE_RATIONALE: If the harm-limited constraint vanished—if speakers' liability for demonstrable harm evaporated and the First Amendment reverted to absolute categorical protection—vulnerable minorities would face unchecked targeted speech without legal recourse. The regulatory infrastructure (takedown procedures, harassment prosecutions, defamation remedies conditioned on harm proof) would collapse. The institutional coordination between speakers and harm victims would dissolve.
% FOUNDING_PROBLEM: Early First Amendment jurisprudence left unresolved the boundary between protected expression and expression that causes demonstrable harm. Historical precedent recognized narrow categorical exclusions (incitement, true threats, defamation), but the harm-limited reading extends the harm principle to modern harms not contemplated in historical cases: online harassment, algorithmic amplification targeting, deepfake targeting, coordinated doxxing campaigns.
% FOUNDING_PROBLEM_CORROBORATION: Victims of targeted online harassment and vulnerable minority advocates attest the founding problem is live and urgent—the historical categories inadequately cover modern harm vectors. Courts in several jurisdictions have adopted harm-balancing language. Absolutist scholars and civil liberties organizations attest the problem is a misunderstanding of the First Amendment's categorical protection—they argue the founding problem is not under-regulation but mission creep in the definition of 'harm' that weaponizes regulation against disfavored speech.
narrative_ontology:disappearance_verdict(first_amendment_speech_protection__harm_limited_reading, world_rearranges).
narrative_ontology:founding_problem_status(first_amendment_speech_protection__harm_limited_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(first_amendment_speech_protection__harm_limited_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(first_amendment_speech_protection__harm_limited_reading, 'none', 1).
narrative_ontology:epsilon_provenance(first_amendment_speech_protection__harm_limited_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(first_amendment_speech_protection__harm_limited_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(first_amendment_speech_protection__harm_limited_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(first_amendment_speech_protection__harm_limited_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises 0.48→0.68 over the interval because the harm-limited reading's regulatory scope has expanded. Early applications focused on narrow harms (incitement, defamation, true threats tied to imminent lawless action). Recent application extends to online harassment, algorithmic targeting, coordinated doxxing, and reputational harm—category expansion that increases the speech population subject to regulation. Suppression is high (0.71) and stable because the constraint's enforcement depends on active institutional suppression of speech (court orders, criminal sanctions, platform takedowns) to maintain the harm boundary. Theater rises 0.25→0.42 because the evidentiary practice of harm-determination becomes increasingly ritualized: regulatory hearings, expert testimony on 'harm metrics,' platform appeals processes—more procedural theater proportional to the regulatory scope growth. Measurements are on a shared time grid; each metric is authored at every interval point examined. The constraint is CLAIMED tangled_rope (coordination function protecting harm victims + asymmetric extraction from speakers) but the authored metrics suggest the extractive component is outpacing the coordination function as harm categories expand.
 *
 * PERSPECTIVAL GAP:
 *   From the vulnerable_minorities seat, the constraint is protective and necessary: it provides institutional recourse against targeted harm they cannot escape. From the speakers_causing_harm seat, the constraint is suppressive and mission-creeping: the harm standard expands steadily, regulatory liability grows unpredictable, and state authority over speech scope increases. From the state_regulatory_authority seat, the constraint is a workable balance: harm-limitation provides a rule-of-law boundary, prevents absolute prohibition, and permits regulation only when evidence of demonstrable harm is produced. The engine computes these divergences from the stakeholders' power levels, exit options, and positions as beneficiary or payer. The authored claim (tangled_rope) reflects the reading's institutional framing; the authored metrics reflect the historical trajectory—rising extractiveness and suppression suggest the balance is drifting toward greater regulatory scope.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (vulnerable_minorities, harm_victims) sit near d=0.0 (low extraction, high subsidy): the constraint transfers regulatory protection to them without requiring them to pay or exit. Their directionality is fully subsidized by the extraction from speakers. Victims (speakers_causing_harm) sit near d=1.0 (full extraction): they bear the regulatory cost—liability, speech restriction, enforcement pressure. Their exit is constrained (geographic relocation possible but costly in reputational and economic terms). The state_regulatory_authority has high power and arbitrage options (can enforce or not, can tighten or loosen harm standards), so it sits at moderate d reflecting its control over the constraint's scope and application. Platforms sit at d≈0.5 (symmetric): they bear enforcement costs (payer) but enjoy liability protection if they respond promptly to harm claims (beneficiary). The derivation chain from beneficiary/victim declarations + exit options produces directionality that reflects power asymmetry.
 *
 * MANDATROPHY ANALYSIS:
 *   The harm-limited reading avoids mandatrophy confusion (live vs. dead founding problem) via the R5 trio: the founding_problem_status is CONTESTED because absolutists attest the problem is a misreading of constitutional text, while harm victims attest the problem is live and urgent. The constraint does NOT expire when the founding problem is 'solved'—the problem is permanently contestable. The disappearance_verdict is world_rearranges, confirming that arrangements depend on the constraint—regulatory infrastructure, platform policies, speaker self-censorship all dissolve if the constraint vanishes. This prevents misclassification as a piton (theatrical inertia without real function): the constraint genuinely coordinates between speech protection and harm-mitigation, even as the regulatory scope drifts.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    demonstrable_harm_boundary,
    'What constitutes ''demonstrable harm'' under the reading? Does the standard remain stable or drift to encompass offense, emotional distress, and reputational harm?',
    'Longitudinal analysis of court decisions classifying harm; regulatory rulemaking defining harm categories; empirical documentation of whether harm categories expand beyond imminent physical/economic/reputational loss to include subjective distress and offense.',
    'If harm expands to subjective categories (offense, emotional distress without concrete loss), the reading becomes indistinguishable from categorical harm-balancing and loses its boundary-specificity. If the boundary holds at demonstrable concrete loss, the reading remains a workable limiting principle. This determines whether the constraint is a genuine coordinating rope with boundaries or a snare deploying ''harm'' as a pretext for speech suppression.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(demonstrable_harm_boundary, empirical, 'Whether the harm boundary remains concrete or drifts to subjective categories').

omega_variable(
    suppression_structural_vs_internalized,
    'Is the measured suppression (0.71) structural (legal liability, platform enforcement, regulatory power asymmetry) or partially internalized (speakers self-censor from fear of sanction)?',
    'Post-exit analysis: do speakers who flee to jurisdictions with weaker harm-balancing standards continue self-censoring? If yes, suppression is internalized; if no, suppression is structural. Comparison of speech patterns in high-enforcement vs. low-enforcement jurisdictions.',
    'If suppression is structural, removing the regulatory mechanism relieves it; if internalized, speakers carry the suppression with them even outside the jurisdiction. Internalized suppression suggests the reading''s enforcement has created lasting cognitive patterns that persist after the mechanism is removed—a higher effective suppression than the authored scalar suggests.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'Whether suppression is structural or internalized').

omega_variable(
    reading_foreclosure_by_empirical_category_drift,
    'If empirical evidence accumulates that the harm category drifts steadily toward subjective categories (offense, distress), does the reading foreclose itself—i.e., does the drift undermine the reading''s own boundary-specificity claim?',
    'Monitoring of harm-category expansion over 10+ year timescale. If drift is systematic and acknowledged by courts, the reading''s core axiom (demonstrable harm as a limiting boundary) fails empirically.',
    'If the reading''s own premises fail empirically (harm boundaries collapse into subjective categories), the reading itself becomes incoherent—the harm-limited framing collapses and the constraint reverts to the categorical_balancing_reading (category creation via case-by-case balancing, not harm-proof). This is not foreclosure BY a sibling reading, but self-foreclosure via empirical failure of the axiom.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_foreclosure_by_empirical_category_drift, empirical, 'Whether the reading self-forecloses if its boundary-specificity axiom fails empirically').

omega_variable(
    regulatory_capture_risk,
    'Does the state_regulatory_authority become captured by either harm victims seeking maximalist regulation or by speakers seeking minimal harm standards?',
    'Institutional analysis of regulatory agency composition, funding sources, and decision patterns. Do court decisions systematically favor harm victims'' interests or speakers'' interests over neutral harm-proof application?',
    'Capture by harm victims would increase extractiveness beyond the 0.68 endpoint as harm categories expand and enforcement intensifies. Capture by speakers would decrease extractiveness and weaken the coordination function. Either form of capture converts tangled_rope toward snare (pure extraction) or rope (pure coordination without extraction).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_capture_risk, empirical, 'Whether regulatory authority becomes captured by one party''s interests').

omega_variable(
    kernel_reading_contest_absolutist_foreclosure,
    'Does the absolutist reading''s core premise (categorical protection, no harm-balancing) logically foreclose this reading''s core premise (demonstrable harm as a legitimate boundary)?',
    'Constitutional text analysis and jurisprudential examination. Does the First Amendment''s phrase ''no law abridging the freedom of speech'' permit ANY harm-balancing, or does it categorically forbid it?',
    'If the absolutist reading is correct, this reading is not coexistent but foreclosed—the two readings cannot both be true of the same constitutional text. If the harm-limited reading is correct, the absolutist reading misreads the text. This is the foundational ambiguity of the kernel itself.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_contest_absolutist_foreclosure, conceptual, 'Whether absolutist and harm-limited readings are logically incompatible').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(first_amendment_speech_protection__harm_limited_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(firs_tr_t0, first_amendment_speech_protection__harm_limited_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(firs_tr_t5, first_amendment_speech_protection__harm_limited_reading, theater_ratio, 5, 0.28).
narrative_ontology:measurement(firs_tr_t10, first_amendment_speech_protection__harm_limited_reading, theater_ratio, 10, 0.31).
narrative_ontology:measurement(firs_tr_t15, first_amendment_speech_protection__harm_limited_reading, theater_ratio, 15, 0.35).
narrative_ontology:measurement(firs_tr_t20, first_amendment_speech_protection__harm_limited_reading, theater_ratio, 20, 0.39).
narrative_ontology:measurement(firs_tr_t25, first_amendment_speech_protection__harm_limited_reading, theater_ratio, 25, 0.41).
narrative_ontology:measurement(firs_tr_t30, first_amendment_speech_protection__harm_limited_reading, theater_ratio, 30, 0.42).
narrative_ontology:measurement(firs_tr_t40, first_amendment_speech_protection__harm_limited_reading, theater_ratio, 40, 0.42).

% Extraction over time
narrative_ontology:measurement(firs_be_t0, first_amendment_speech_protection__harm_limited_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(firs_be_t5, first_amendment_speech_protection__harm_limited_reading, base_extractiveness, 5, 0.52).
narrative_ontology:measurement(firs_be_t10, first_amendment_speech_protection__harm_limited_reading, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(firs_be_t15, first_amendment_speech_protection__harm_limited_reading, base_extractiveness, 15, 0.63).
narrative_ontology:measurement(firs_be_t20, first_amendment_speech_protection__harm_limited_reading, base_extractiveness, 20, 0.65).
narrative_ontology:measurement(firs_be_t25, first_amendment_speech_protection__harm_limited_reading, base_extractiveness, 25, 0.67).
narrative_ontology:measurement(firs_be_t30, first_amendment_speech_protection__harm_limited_reading, base_extractiveness, 30, 0.68).
narrative_ontology:measurement(firs_be_t40, first_amendment_speech_protection__harm_limited_reading, base_extractiveness, 40, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(firs_su_t0, first_amendment_speech_protection__harm_limited_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(firs_su_t5, first_amendment_speech_protection__harm_limited_reading, suppression_requirement, 5, 0.6).
narrative_ontology:measurement(firs_su_t10, first_amendment_speech_protection__harm_limited_reading, suppression_requirement, 10, 0.64).
narrative_ontology:measurement(firs_su_t15, first_amendment_speech_protection__harm_limited_reading, suppression_requirement, 15, 0.68).
narrative_ontology:measurement(firs_su_t20, first_amendment_speech_protection__harm_limited_reading, suppression_requirement, 20, 0.7).
narrative_ontology:measurement(firs_su_t25, first_amendment_speech_protection__harm_limited_reading, suppression_requirement, 25, 0.71).
narrative_ontology:measurement(firs_su_t30, first_amendment_speech_protection__harm_limited_reading, suppression_requirement, 30, 0.71).
narrative_ontology:measurement(firs_su_t40, first_amendment_speech_protection__harm_limited_reading, suppression_requirement, 40, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(first_amendment_speech_protection__harm_limited_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(first_amendment_speech_protection__harm_limited_reading, 0.12).
narrative_ontology:affects_constraint(first_amendment_speech_protection__harm_limited_reading, first_amendment_speech_protection__absolutist_reading).
narrative_ontology:affects_constraint(first_amendment_speech_protection__harm_limited_reading, first_amendment_speech_protection__categorical_balancing_reading).

% DUAL FORMULATION NOTE:
% The harm-limited reading decomposes from the contested kernel first_amendment_speech_protection. Sibling readings (absolutist and categorical_balancing) are separate constraint stories with different ε values, beneficiary/victim structures, and type classifications. The absolutist reading has near-zero ε (categorical protection has no extraction, only coordination for speech freedom). The categorical_balancing reading has moderate ε (discretionary balancing permits extraction depending on which category the court assigns). The harm-limited reading has higher ε (regulatory proof-of-harm creates institutional authority over speech scope, increasing extraction potential). All three readings share the same kernel (the First Amendment) but instantiate different constraints via different interpretive commitments.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(first_amendment_speech_protection__harm_limited_reading, powerless, 0.15).
constraint_indexing:directionality_override(first_amendment_speech_protection__harm_limited_reading, moderate, 0.82).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
