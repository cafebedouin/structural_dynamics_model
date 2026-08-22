% ============================================================================
% CONSTRAINT STORY: speech_protection_boundary__balancing_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_speech_protection_boundary__balancing_reading, []).

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
 *   constraint_id: speech_protection_boundary__balancing_reading
 *   human_readable: Speech Protection Boundary — Balancing Reading
 *   domain: constitutional/political
 *
 * SUMMARY:
 *   The balancing reading of the speech-protection boundary treats the First
 *   Amendment's scope as context-dependent and subject to judicial weighing
 *   against other constitutional values (equality, dignity, freedom from
 *   harassment, national security) and demonstrated harms. The constraint
 *   structures how courts adjudicate speech cases: instead of applying
 *   categorical rules (speech is either protected or banned), judges engage
 *   in multi-factor analysis where First Amendment interests compete against
 *   other interests. This reading benefits institutional judiciary (which
 *   retains gatekeeper authority), institutional speakers who can litigate
 *   (and whose speech typically clears the balancing test), and civil
 *   liberties coalitions that champion robust protection while permitting
 *   narrow exceptions. It imposes costs on marginalized speakers (whose
 *   speech is subject to ad-hoc assessment and institutional bias), targeted
 *   groups (whose protective interests are contingent on favorable judicial
 *   balancing), and equality interests (which must be weighed against, not
 *   automatically enforced against, speech). The reading is a live
 *   constitutional interpretation — courts apply balancing doctrine in First
 *   Amendment cases — but it is contested by absolutists (who argue balancing
 *   is politicized suppression) and harm-first advocates (who argue equality
 *   should take priority in clear cases). The measurement series shows
 *   extraction rising from 0.52 to 0.71 over the interval (observed through
 *   year 15, projected to year 45), suggesting the balancing framework is
 *   accumulating implicit costs (increased litigation, greater lower-court
 *   discretion burden, expanding theater ratio) even as its coordination
 *   function persists.
 *
 * KEY AGENTS:
 *   - institutional_judiciary: Sets the balancing test via case-by-case adjudication; controls application and future development of doctrine
 *   - moderate_speech_advocates: Benefit from a framework where speech protection is robust but contextual; retain arbitrage exit options
 *   - marginalized_speakers: Pay the cost of ad-hoc assessment and institutional bias; trapped with identity-locked exit (cannot leave the speech market without abandoning their voice)
 *   - equality_interests: Bear the cost of speech-equality conflicts where balancing can override equality protections
 *   - targeted_groups: Subject to harassment, incitement, coded speech where balancing may not capture harm
 *   - lower_courts: Observe the framework; apply balancing tests to novel cases; bear burden of discretion and reversal risk
 *   - absolutist_coalition: Excluded from the balancing reading's framework; contest the kernel itself
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(speech_protection_boundary__balancing_reading, 0.68).
domain_priors:suppression_score(speech_protection_boundary__balancing_reading, 0.72).
domain_priors:theater_ratio(speech_protection_boundary__balancing_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(speech_protection_boundary__balancing_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(speech_protection_boundary__balancing_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(speech_protection_boundary__balancing_reading, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(speech_protection_boundary__balancing_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(speech_protection_boundary__balancing_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(speech_protection_boundary__balancing_reading, tangled_rope).
narrative_ontology:human_readable(speech_protection_boundary__balancing_reading, "Speech Protection Boundary — Balancing Reading").
narrative_ontology:topic_domain(speech_protection_boundary__balancing_reading, "constitutional/political").

domain_priors:requires_active_enforcement(speech_protection_boundary__balancing_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(speech_protection_boundary__balancing_reading, '8a9ad309-2111-421b-92a7-2009ecf50dd1').
narrative_ontology:cs_kernel_codification('8a9ad309-2111-421b-92a7-2009ecf50dd1', fixed_text).
narrative_ontology:cs_authority_grounding('8a9ad309-2111-421b-92a7-2009ecf50dd1', lineage).
narrative_ontology:cs_interpretation_layer_present('8a9ad309-2111-421b-92a7-2009ecf50dd1').
narrative_ontology:cs_reading_relation('8a9ad309-2111-421b-92a7-2009ecf50dd1', speech_protection_boundary__absolutist_reading, coexists_with).
narrative_ontology:cs_reading_relation('8a9ad309-2111-421b-92a7-2009ecf50dd1', speech_protection_boundary__harm_limited_reading, coexists_with).
narrative_ontology:cs_axiom('8a9ad309-2111-421b-92a7-2009ecf50dd1', foundational, first_amendment_contextually_commensurable).
narrative_ontology:cs_axiom_status(first_amendment_contextually_commensurable, holdable).
narrative_ontology:cs_axiom_grounding('8a9ad309-2111-421b-92a7-2009ecf50dd1', first_amendment_contextually_commensurable, deontological).
narrative_ontology:cs_axiom('8a9ad309-2111-421b-92a7-2009ecf50dd1', secondary, judicial_discretion_yields_principled_outcomes).
narrative_ontology:cs_axiom_status(judicial_discretion_yields_principled_outcomes, holdable).
narrative_ontology:cs_axiom_grounding('8a9ad309-2111-421b-92a7-2009ecf50dd1', judicial_discretion_yields_principled_outcomes, empirically_contingent).
narrative_ontology:cs_reference_frame('8a9ad309-2111-421b-92a7-2009ecf50dd1', brandenburg_plus_harms).
narrative_ontology:cs_drift_state('8a9ad309-2111-421b-92a7-2009ecf50dd1', post_digital_amplification_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('8a9ad309-2111-421b-92a7-2009ecf50dd1', '').
narrative_ontology:cs_kernel_id(speech_protection_boundary__balancing_reading, speech_protection_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(speech_protection_boundary__balancing_reading, institutional_judiciary).
narrative_ontology:constraint_beneficiary(speech_protection_boundary__balancing_reading, moderate_speech_advocates).
narrative_ontology:constraint_beneficiary(speech_protection_boundary__balancing_reading, regulated_speaker_interests).
narrative_ontology:constraint_victim(speech_protection_boundary__balancing_reading, marginalized_speakers).
narrative_ontology:constraint_victim(speech_protection_boundary__balancing_reading, equality_interests).
narrative_ontology:constraint_victim(speech_protection_boundary__balancing_reading, targeted_groups).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(speech_protection_boundary__balancing_reading, civil_liberties_organizations).
narrative_ontology:constraint_victim(speech_protection_boundary__balancing_reading, regulated_speaker_interests).
narrative_ontology:constraint_victim(speech_protection_boundary__balancing_reading, lower_courts).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets the interpretive framework through case-by-case adjudication, weighing First Amendment interests against other constitutional values. Controls the balancing test, which factors dominate, and how future courts apply the precedent. Administers the constraint and maintains discretion over application across contexts.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__balancing_reading, institutional_judiciary, agenda_setter,
    institutional, generational, analytical, national).

% Benefit from a framework where speech protection is robust but contextual — they can make moderate claims about harm and equality and expect judicial consideration without needing to prove imminent lawless action or accepting near-absolute speech immunity. Their speech typically clears the balancing test; they retain ability to forum-shop or appeal if a lower court weighs against them.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__balancing_reading, moderate_speech_advocates, beneficiary,
    powerful, biographical, arbitrage, national).

% Some speech sectors (commercial, professional, time-place-manner restricted) benefit from predictable intermediate scrutiny — clearer than pure balancing, easier to comply with than absolute protection. They must adjust speech conduct to passing judicial review, but the categorical clarity they lose is offset by the targeting they avoid under an absolutist regime.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__balancing_reading, regulated_speaker_interests, beneficiary,
    institutional, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(speech_protection_boundary__balancing_reading, regulated_speaker_interests, payer).

% Bear the costs of the balancing test's context-dependence: their speech is subject to ad-hoc judicial assessment where powerful speakers' speech is routinely protected. Lack resources to litigate favorable precedent or appeal unfavorable rulings. Silenced speech is their primary loss, but institutional bias in balancing (favoring institutional speakers, disfavoring dissent) extracts disproportionate suppression.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__balancing_reading, marginalized_speakers, payer,
    powerless, biographical, trapped, local).

% Bear the cost of speech protection when it conflicts with equality guarantees (14th Amendment). Under balancing, equality claims must be weighed against First Amendment interests, not automatically enforced. Exclusion from certain speech domains is possible in principle but uncertain in practice, creating chronic conflict with anti-discrimination commitments.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__balancing_reading, equality_interests, payer,
    powerful, generational, constrained, national).

% Groups targeted by harassment, incitement, or coded speech that the balancing framework may not capture as harmful. The balancing test can weight dignity, freedom from harassment, and systemic harm, but it also permits counter-weights (speaker's interest, listener autonomy, chilling effect concerns) that can override. Their protective interest is contingent, not guaranteed.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__balancing_reading, targeted_groups, payer,
    organized, biographical, constrained, national).

% Organizations that champion speech protection as a structural value benefit from a reading that maintains broad protection while permitting exceptional narrowing. They can litigate successfully against categorical restrictions on speech while conceding narrow harm-based exceptions where the balancing outcome is favorable.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__balancing_reading, civil_liberties_organizations, beneficiary,
    powerful, biographical, mobile, national).

% Tasked with applying the balancing test to novel fact patterns without clear algorithmic guidance. Bear the cost of uncertainty — inconsistent outcomes create precedent conflicts and reversal risk. Must guess Supreme Court's weighting of factors and account for appellate overruling. The delegation of interpretive authority to case-by-case judgment creates institutional burden and discretionary power simultaneously.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__balancing_reading, lower_courts, observer,
    institutional, biographical, analytical, national).
narrative_ontology:stakeholder_secondary_role(speech_protection_boundary__balancing_reading, lower_courts, payer).

% Theorists and advocates who hold that speech protection should be near-absolute are excluded from the balancing reading's framework — they refuse the premise that competing constitutional values can outweigh First Amendment interests and argue balancing is a disguise for politicized suppression. They would challenge the constraint itself.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__balancing_reading, absolutist_coalition, excluded,
    powerful, biographical, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(speech_protection_boundary__balancing_reading, institutional_judiciary).
narrative_ontology:fixing_cost_class(speech_protection_boundary__balancing_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates institutional authority to interpret fundamental rights: disperses the gatekeeper role across the judiciary rather than concentrating it in a categorical rule, permitting context-responsive adjudication that accounts for competing constitutional values and demonstrated harms in specific cases.
% TRANSFER_FUNCTION: Transfers power over speech boundaries from explicit categorical rules to implicit judicial discretion. Distributes legitimacy to decide 'where the line is' across the judiciary (hierarchically, with appellate courts setting precedent but lower courts applying it case-by-case). Extracts suppression from marginalized speakers (who bear the cost of ad-hoc adjudication and institutional bias) and redistributes deference to institutional speakers.
% ABSENT_VOICES: Absolutists (who reject balancing as a frame entirely) and harm-first advocates (who would weight equality and dignity protection above speech immunity) are structural non-participants: the absolutist reading forecloses balancing; the harm_limited reading inverts the priority. They contest the kernel itself, not just the sitting reading. Also largely absent: powerless and marginalized speakers who lack the resources to litigate and establish favorable precedent.
% DISAPPEARANCE_RATIONALE: If the balancing framework disappeared (replaced by absolutism or harm-first rule), the entire architecture of intermediate scrutiny, contextual exception-making, and judicial discretion would collapse. Speech doctrine would revert to categorical rule (imminent lawless action standard) or shift to harm-outcome-focused protection. Institutions downstream (universities, employers, platforms, governments) that have organized policies around the current balancing boundary would reorganize around new rules.
% FOUNDING_PROBLEM: Early absolute protection doctrine (Brandenburg) could not account for forms of harm that fell short of imminent lawless action — persistent inequality perpetuated through speech, harassment of targeted groups, incitement through coded language and systemic amplification. Harm-prevention interests had no doctrinal home. The balancing framework was constructed to permit judicial recognition of these harms without collapsing into pure regulation.
% FOUNDING_PROBLEM_CORROBORATION: Civil rights organizations and equality advocates attest the founding problem is live — coded speech, inequality-perpetuating speech, and systemic harassment remain inadequately addressed by Brandenburg absolutism. Absolutists contest that these ARE harms deserving doctrinal weight; they argue the founding problem was artificially constructed. Lower courts' reported difficulty applying balancing across contexts (academic freedom, campus speech, content moderation) attests the founding problem remains contested in practice.
narrative_ontology:disappearance_verdict(speech_protection_boundary__balancing_reading, world_rearranges).
narrative_ontology:founding_problem_status(speech_protection_boundary__balancing_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(speech_protection_boundary__balancing_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(speech_protection_boundary__balancing_reading, 'none', 1).
narrative_ontology:epsilon_provenance(speech_protection_boundary__balancing_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(speech_protection_boundary__balancing_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(speech_protection_boundary__balancing_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(speech_protection_boundary__balancing_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is 0.68 (high) because the balancing framework extracts suppression from speakers who lose judicial weighing — their speech is not categorically protected but contingent on favorable balancing. The contingency creates a suppression effect both directly (lost speech) and indirectly (preemptive self-censorship in anticipation of unfavorable outcomes). Suppression is 0.72 (high) because the framework requires active judicial enforcement of the boundary — courts must police which speech is permitted and which is subject to regulation, and the discretionary nature of balancing creates opportunities for institutional bias. Theater ratio is 0.58 (moderate-high) because the balancing doctrine performs a legitimacy function (appears principled, accounts for competing values) while outcomes often reflect institutional preferences and speaker-type asymmetries. Accessibility_collapse is 0.42 (moderate-low) because alternatives to the balancing reading are live — absolutism and harm-first approaches remain viable constitutional interpretations held by serious theorists and institutional actors. Resistance is 0.71 (high) because the balancing reading generates substantial opposition: absolutists argue it is a disguise for politicized suppression; harm-first advocates argue it fails to protect equality; marginalized speakers resist through direct action and counterargument. The measurement series shows the constraint accumulating extraction and theater (the coordination burden is increasing even as the coordination function persists) — a trajectory consistent with a tangled rope that is slowly degrading toward piton status.
 *
 * PERSPECTIVAL GAP:
 *   From the institutional judiciary seat, the balancing reading is a sophisticated coordination mechanism that permits principled adjudication while remaining flexible enough to account for new forms of harm and competing rights. From the marginalized_speaker seat, the same mechanism is an extraction device — ad-hoc discretion deployed against their voice because they lack the power to influence the balance or litigate expensive cases. From the equality_interests seat, the balancing reading appears to subordinate equality to speech — a constitutional hierarchy problem. From the civil_liberties_organizations seat, the balancing reading is a successful defense of speech protection against categorical restrictions. The engine should compute these differences from the structural data: agenda-setter vs. payer positions, power asymmetries, exit options — the authored metrics describe one constraint, but the per-seat classification should surface the perspectival gaps.
 *
 * DIRECTIONALITY LOGIC:
 *   The institutional judiciary (agenda_setter, institutional power, analytical exit) has d near 0.0 — the constraint gives them authority and minimal cost. Moderate_speech_advocates (beneficiary, powerful power, arbitrage exit) have d near 0.2 — they benefit from robust protection and retain exit options if a particular judgment goes against them. Regulated_speaker_interests (beneficiary/payer, institutional power, mobile exit) have d near 0.5 — the balancing test constrains them but they can adjust conduct to pass review and have the power to litigate. Marginalized_speakers (payer, powerless, trapped exit) have d near 0.95 — they bear the full cost of the balancing test without power to influence it and cannot exit (speech is their primary political tool). Equality_interests (payer, powerful, constrained exit) have d near 0.7 — their protective interests are routinely overweighed in balancing and they cannot exit (equality is a constitutional commitment). Targeted_groups (payer, organized, constrained exit) have d near 0.75 — they can organize to advocate for stricter harm-prevention but the balancing test often overrides their claims. The directional asymmetry between agenda_setter (d≈0.0) and the payer seats (d≈0.7–0.95) drives the tangled_rope classification: genuine coordination (the balancing framework permits context-responsive rights adjudication) coupled with asymmetric extraction (the cost-bearers are systematically disfavored in the balance).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem was recognizing forms of harm (inequality perpetuated through speech, coded incitement, systemic harassment) that fell outside Brandenburg's imminent lawless action exception. The balancing reading was constructed to permit judicial recognition of these harms without abandoning speech protection entirely. However, the measurement trajectory shows extraction and theater rising over the interval (extractiveness: 0.52→0.68; theater: 0.48→0.58) while resistance remains high (0.71). This suggests the founding problem may be partially addressed (courts do weigh harm concerns) but the solution is generating new costs (increased litigation, institutional burden, marginalized-speaker suppression) that were not part of the founding diagnosis. The rising theater ratio indicates the constraint is increasingly maintained through performative balancing (the appearance of reasoned weighing) rather than consistent application. A mandate-checking analysis would ask: Is the balancing framework still solving the founding problem of harm recognition, or has it become a container for institutional discretion that claims to weigh harm but systematically favors institutional speakers? The measured extraction suggests the latter is partially true — the constraint has captured some of the legitimacy it was meant to provide while extracting suppression from speakers without power to influence the balance.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    institutional_capture_by_moderate_speakers,
    'To what extent does the balancing framework serve as institutional capture, where powerful moderate speakers have colonized the judicial discretion and use it to exclude more radical speech and protect their own moderate platform?',
    'Analysis of which speakers win balancing tests by speaker ideology (radical, moderate, conservative); study of whether judicial outcomes cluster around protecting institutional moderation; examination of whether the moderate speech advocates have disproportionate litigation access and success rates.',
    'If capture is substantial, the balancing reading''s purported coordination function is partly illusory — it is a mechanism for moderate institutional capture disguised as principled balancing. The constraint would reclassify toward pure snare from the radical-speaker perspective (institutional extraction of their voice) even while remaining tangled rope from the moderate-speaker perspective.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_capture_by_moderate_speakers, empirical, 'Whether the balancing framework protects speech generally or serves institutional moderation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(speech_protection_boundary__balancing_reading, 0, 45).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(spee_tr_t0, speech_protection_boundary__balancing_reading, theater_ratio, 0, 0.48).
narrative_ontology:measurement(spee_tr_t7, speech_protection_boundary__balancing_reading, theater_ratio, 7, 0.51).
narrative_ontology:measurement(spee_tr_t15, speech_protection_boundary__balancing_reading, theater_ratio, 15, 0.55).
narrative_ontology:measurement(spee_tr_t22, speech_protection_boundary__balancing_reading, theater_ratio, 22, 0.58).
narrative_ontology:measurement(spee_tr_t30, speech_protection_boundary__balancing_reading, theater_ratio, 30, 0.6).
narrative_ontology:measurement(spee_tr_t45, speech_protection_boundary__balancing_reading, theater_ratio, 45, 0.58).

% Extraction over time
narrative_ontology:measurement(spee_be_t0, speech_protection_boundary__balancing_reading, base_extractiveness, 0, 0.52).
narrative_ontology:measurement(spee_be_t7, speech_protection_boundary__balancing_reading, base_extractiveness, 7, 0.58).
narrative_ontology:measurement(spee_be_t15, speech_protection_boundary__balancing_reading, base_extractiveness, 15, 0.64).
narrative_ontology:measurement(spee_be_t22, speech_protection_boundary__balancing_reading, base_extractiveness, 22, 0.68).
narrative_ontology:measurement(spee_be_t30, speech_protection_boundary__balancing_reading, base_extractiveness, 30, 0.71).
narrative_ontology:measurement(spee_be_t45, speech_protection_boundary__balancing_reading, base_extractiveness, 45, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(spee_su_t0, speech_protection_boundary__balancing_reading, suppression_requirement, 0, 0.65).
narrative_ontology:measurement(spee_su_t7, speech_protection_boundary__balancing_reading, suppression_requirement, 7, 0.68).
narrative_ontology:measurement(spee_su_t15, speech_protection_boundary__balancing_reading, suppression_requirement, 15, 0.7).
narrative_ontology:measurement(spee_su_t22, speech_protection_boundary__balancing_reading, suppression_requirement, 22, 0.72).
narrative_ontology:measurement(spee_su_t30, speech_protection_boundary__balancing_reading, suppression_requirement, 30, 0.73).
narrative_ontology:measurement(spee_su_t45, speech_protection_boundary__balancing_reading, suppression_requirement, 45, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(speech_protection_boundary__balancing_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(speech_protection_boundary__balancing_reading, 0.12).
narrative_ontology:affects_constraint(speech_protection_boundary__balancing_reading, speech_protection_boundary__absolutist_reading).
narrative_ontology:affects_constraint(speech_protection_boundary__balancing_reading, speech_protection_boundary__harm_limited_reading).
narrative_ontology:affects_constraint(speech_protection_boundary__balancing_reading, institutional_trust_in_judiciary).
narrative_ontology:affects_constraint(speech_protection_boundary__balancing_reading, equality_enforcement_doctrine).

% DUAL FORMULATION NOTE:
% The speech_protection_boundary kernel decomposes into three constraint stories representing three live readings: absolutist_reading (Brandenburg-standard near-absolute protection), balancing_reading (context-dependent judicial weighing, this story), and harm_limited_reading (equality and dignity-prioritizing protection). Each reading produces a different ε, different beneficiary/victim structure, and different classification. The readings coexist across institutional seats — they are not temporal stages but parallel factions. All three stories link via network.affects_constraints to indicate kernel membership and mutual constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(speech_protection_boundary__balancing_reading, powerless, 0.95).
constraint_indexing:directionality_override(speech_protection_boundary__balancing_reading, institutional, 0.05).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
