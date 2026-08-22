% ============================================================================
% CONSTRAINT STORY: speech_protection_boundary__balancing_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-04
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:measurement_basis/2,
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
 *   human_readable: First Amendment Balancing Test (Case-by-Case Weighing)
 *   domain: constitutional_law/political_philosophy/speech_regulation
 *
 * SUMMARY:
 *   The balancing reading of the First Amendment holds that speech protection
 *   is not categorical but determined case-by-case through weighing
 *   expressive interests against competing constitutional values (equality,
 *   dignity, privacy, public safety) and demonstrated harms. Unlike the
 *   absolutist reading (near-absolute protection, narrow
 *   imminent-lawless-action exception) or the harm-limited reading
 *   (protection conditional on absence of significant harm to protected
 *   groups), the balancing reading distributes gatekeeping across the
 *   judiciary: judges weigh context, speaker identity, audience
 *   vulnerability, and harm specificity in each case. Coded speech (dog
 *   whistles, stochastic terrorism) and systemic harm (cumulative harassment,
 *   structural inequality) receive intermediate scrutiny rather than
 *   categorical protection or categorical suppression. The constraint claims
 *   to be a rope (genuine coordination: a flexible standard that protects
 *   speech while preventing harm) but operates as a tangled rope: the
 *   judiciary and regulatory state benefit from discretionary authority,
 *   while marginalized speakers, dissenters, and whistleblowers bear
 *   context-dependent suppression that tracks judicial composition and
 *   political moment rather than stable rules.
 *
 * KEY AGENTS:
 *   - judiciary: Primary agenda_setter (institutional/biographical/analytical) — distributes gatekeeping, collects institutional legitimacy
 *   - regulatory_state: Beneficiary (institutional/generational/arbitrage) — gains flexible enforcement authority
 *   - vulnerable_groups_advocacy: Beneficiary (organized/biographical/constrained) — gains recognition of systemic harm
 *   - law_enforcement: Beneficiary (institutional/immediate/mobile) — gains contextual authority to restrict threatening speech
 *   - speakers_marginalized: Victim (powerless/biographical/trapped) — face unpredictable suppression when speech challenges power
 *   - political_dissenters: Victim (moderate/biographical/constrained) — suppressed when dissent coded as harm
 *   - artists_creators: Victim (moderate/biographical/constrained) — work restricted by contextual harm readings
 *   - whistleblowers: Victim (powerless/biographical/trapped) — disclosures weighed against institutional 'harm'
 *   - constitutional_scholars: Observer (analytical/civilizational/analytical) — analyze structural dynamics
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(speech_protection_boundary__balancing_reading, 0.38).
domain_priors:suppression_score(speech_protection_boundary__balancing_reading, 0.32).
domain_priors:theater_ratio(speech_protection_boundary__balancing_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(speech_protection_boundary__balancing_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(speech_protection_boundary__balancing_reading, suppression_requirement, 0.32).
narrative_ontology:constraint_metric(speech_protection_boundary__balancing_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(speech_protection_boundary__balancing_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(speech_protection_boundary__balancing_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(speech_protection_boundary__balancing_reading, tangled_rope).
narrative_ontology:human_readable(speech_protection_boundary__balancing_reading, "First Amendment Balancing Test (Case-by-Case Weighing)").
narrative_ontology:topic_domain(speech_protection_boundary__balancing_reading, "constitutional_law/political_philosophy/speech_regulation").

domain_priors:requires_active_enforcement(speech_protection_boundary__balancing_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(speech_protection_boundary__balancing_reading, '69bd0a2c-26f3-46a4-808a-9ab137a59dfe').
narrative_ontology:cs_kernel_codification('69bd0a2c-26f3-46a4-808a-9ab137a59dfe', fixed_text).
narrative_ontology:cs_authority_grounding('69bd0a2c-26f3-46a4-808a-9ab137a59dfe', lineage).
narrative_ontology:cs_interpretation_layer_present('69bd0a2c-26f3-46a4-808a-9ab137a59dfe').
narrative_ontology:cs_reading_relation('69bd0a2c-26f3-46a4-808a-9ab137a59dfe', speech_protection_boundary__absolutist_reading, coexists_with).
narrative_ontology:cs_reading_relation('69bd0a2c-26f3-46a4-808a-9ab137a59dfe', speech_protection_boundary__harm_limited_reading, influences).
narrative_ontology:cs_axiom('69bd0a2c-26f3-46a4-808a-9ab137a59dfe', foundational, contextual_weighing_as_constitutional_method).
narrative_ontology:cs_axiom_status(contextual_weighing_as_constitutional_method, holdable).
narrative_ontology:cs_axiom_grounding('69bd0a2c-26f3-46a4-808a-9ab137a59dfe', contextual_weighing_as_constitutional_method, conventional).
narrative_ontology:cs_axiom('69bd0a2c-26f3-46a4-808a-9ab137a59dfe', foundational, systemic_harm_and_coded_speech_receive_intermediate_scrutiny).
narrative_ontology:cs_axiom_status(systemic_harm_and_coded_speech_receive_intermediate_scrutiny, holdable).
narrative_ontology:cs_axiom_grounding('69bd0a2c-26f3-46a4-808a-9ab137a59dfe', systemic_harm_and_coded_speech_receive_intermediate_scrutiny, empirically_contingent).
narrative_ontology:cs_reference_frame('69bd0a2c-26f3-46a4-808a-9ab137a59dfe', classical_first_amendment_categorical_framework).
narrative_ontology:cs_drift_state('69bd0a2c-26f3-46a4-808a-9ab137a59dfe', contemporary_digital_speech_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('69bd0a2c-26f3-46a4-808a-9ab137a59dfe', '').
narrative_ontology:cs_kernel_id(speech_protection_boundary__balancing_reading, speech_protection_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(speech_protection_boundary__balancing_reading, judiciary).
narrative_ontology:constraint_beneficiary(speech_protection_boundary__balancing_reading, regulatory_state).
narrative_ontology:constraint_beneficiary(speech_protection_boundary__balancing_reading, vulnerable_groups_advocacy).
narrative_ontology:constraint_beneficiary(speech_protection_boundary__balancing_reading, law_enforcement).
narrative_ontology:constraint_victim(speech_protection_boundary__balancing_reading, speakers_marginalized).
narrative_ontology:constraint_victim(speech_protection_boundary__balancing_reading, political_dissenters).
narrative_ontology:constraint_victim(speech_protection_boundary__balancing_reading, artists_creators).
narrative_ontology:constraint_victim(speech_protection_boundary__balancing_reading, whistleblowers).
narrative_ontology:constraint_vindicates(speech_protection_boundary__balancing_reading, living_constitutionalism).
narrative_ontology:constraint_vindicates(speech_protection_boundary__balancing_reading, contextual_first_amendment).
narrative_ontology:constraint_vindicates(speech_protection_boundary__balancing_reading, harm_prevention_legitimacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Distributes gatekeeping authority across federal and state courts through case-by-case balancing. Collects institutional legitimacy as the authoritative interpreter of the First Amendment. Individual judges exercise discretion within precedent constraints; the institution as a whole maintains the balancing framework against categorical alternatives. Exit is analytical: judges can articulate different balancing methodologies but cannot exit the role of balancer without rejecting the reading itself.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__balancing_reading, judiciary, agenda_setter,
    institutional, biographical, analytical, national).

% Gains flexible enforcement authority to regulate speech in novel contexts (digital platforms, harassment, disinformation) without waiting for categorical rules. The balancing standard lets agencies argue for restrictions based on demonstrated harm in specific contexts. Can shift enforcement strategies across administrations — arbitrage-grade exit at the institutional level, though individual officials are constrained.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__balancing_reading, regulatory_state, beneficiary,
    institutional, generational, arbitrage, national).

% Gains recognition of systemic harm and coded speech as legitimate grounds for speech restriction — a structural win over absolutist frameworks. But remains excluded from gatekeeping: advocacy groups litigate and file amicus briefs but do not weigh interests; judges do. Exit is constrained: they can push for harm-limited reading but cannot unilaterally change the balancing framework.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__balancing_reading, vulnerable_groups_advocacy, beneficiary,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(speech_protection_boundary__balancing_reading, vulnerable_groups_advocacy, excluded).

% Gains contextual authority to restrict speech coded as threatening (stochastic terrorism, incitement-adjacent rhetoric) without meeting Brandenburg's imminence requirement. The balancing test lets them argue cumulative harm rather than immediate danger. Mobile exit: can choose when to invoke balancing vs. other authorities.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__balancing_reading, law_enforcement, beneficiary,
    institutional, immediate, mobile, national).

% Bear context-dependent suppression when their speech challenges power structures. The balancing test's 'context' and 'harm' weights systematically disadvantage speakers without institutional credibility: their speech is read as more harmful, less valuable, and more regulable. Trapped exit: they must speak in the very forums where balancing applies; alternative forums are suppressed or inaccessible.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__balancing_reading, speakers_marginalized, payer,
    powerless, biographical, trapped, national).

% Face suppression when dissent is coded as harm (disruption, threats to public order, harm to vulnerable groups). The balancing test lets decision-makers weigh the 'value' of dissent against asserted harms — a weighing that tracks political moment. Constrained exit: can shift forums or tactics but cannot exit the balancing regime that governs all public speech.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__balancing_reading, political_dissenters, payer,
    moderate, biographical, constrained, national).

% Work restricted by contextual harm readings: art depicting violence, sexuality, or marginalized experiences weighed against community harm claims. The balancing test provides no categorical safe harbor for artistic expression. Constrained exit: can self-censor, shift venues, or litigate — each costly and uncertain.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__balancing_reading, artists_creators, payer,
    moderate, biographical, constrained, national).

% Disclosures weighed against institutional 'harm' (national security, privacy, operational integrity). The balancing test lets courts treat whistleblowing as speech whose harm may outweigh its value — especially when the disclosed harm is structural rather than immediate. Trapped exit: the act of whistleblowing occurs within the constraint's jurisdiction; there is no forum where balancing does not apply.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__balancing_reading, whistleblowers, payer,
    powerless, biographical, trapped, national).

% Analyze the structural dynamics of the balancing framework across time. Track how the weighing function shifts with judicial composition, how coded speech and systemic harm doctrines evolve, and whether the constraint drifts toward coordination or extraction. Analytical exit: they observe from outside the gatekeeping role but their analyses shape the discourse that feeds back into the constraint.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__balancing_reading, constitutional_scholars, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(speech_protection_boundary__balancing_reading, judiciary).
narrative_ontology:fixing_cost_class(speech_protection_boundary__balancing_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the genuine problem of applying a fixed constitutional text to novel speech contexts (digital communication, coded rhetoric, systemic harassment) where categorical rules either over-protect (absolutist) or over-restrict (harm-limited). The balancing test coordinates by providing a single methodological framework — weigh interests, assess harm, consider context — that all decision-makers can apply.
% TRANSFER_FUNCTION: Moves speech opportunities and protection from speakers (especially marginalized, dissenting, artistic, whistleblowing speakers) to gatekeepers (judges, regulators, law enforcement) through the discretionary weighing function. The transfer is not monetary but capacitative: the power to decide what speech counts as 'valuable' vs. 'harmful' in each context.
% ABSENT_VOICES: Absolutist advocates (who would reject weighing entirely) and harm-limited advocates (who would make harm categorical rather than contextual) are structurally excluded from the gatekeeping role — the balancing framework defines them out of authority by making 'contextual weighing' the only legitimate methodology. Also excluded: speakers in non-judicial forums (private platforms, international jurisdictions) who are subject to balancing's downstream effects but have no voice in its development.
% DISAPPEARANCE_RATIONALE: If the balancing test vanished overnight, speech regulation would fracture into competing categorical regimes: some jurisdictions adopting absolutist rules, others harm-limited rules, others hybrid standards. The judiciary would lose its distributed gatekeeping role; the regulatory state would lose its flexible enforcement authority; marginalized speakers would face either stronger protection (under absolutist regimes) or stronger suppression (under harm-limited regimes). The mobile software economy of speech governance would reorganize around categorical rather than contextual standards.
% FOUNDING_PROBLEM: The First Amendment's fixed text ('Congress shall make no law... abridging the freedom of speech') was written for a world of pamphlets, public squares, and printing presses. The founding problem is how to faithfully apply this text to radio, television, internet, social media, coded rhetoric, stochastic terrorism, and systemic harassment — contexts the framers could not anticipate — without either abandoning the text's constraint (absolutist drift) or expanding it beyond recognition (harm-limited drift).
% FOUNDING_PROBLEM_CORROBORATION: The judiciary and legal academy attest the problem is live: novel speech contexts (AI-generated speech, deepfakes, algorithmic amplification, cross-border platform governance) require ongoing contextual judgment. Absolutist scholars (e.g., Volokh, Epstein) and harm-limited scholars (e.g., Matsuda, Delgado) attest the problem is substantially solved by their respective frameworks and that balancing persists as judicial self-aggrandizement. Legislative history (Congressional hearings on platform regulation, Section 230 reform) shows bipartisan recognition that categorical rules fail for novel contexts — but disagreement on whether balancing or legislative categorization is the solution.
narrative_ontology:disappearance_verdict(speech_protection_boundary__balancing_reading, world_rearranges).
narrative_ontology:founding_problem_status(speech_protection_boundary__balancing_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(speech_protection_boundary__balancing_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(speech_protection_boundary__balancing_reading, 'none', 1).
narrative_ontology:epsilon_provenance(speech_protection_boundary__balancing_reading, 0.38, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(speech_protection_boundary__balancing_reading_tests).
:- end_tests(speech_protection_boundary__balancing_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38) reflects that the constraint transfers substantive speech opportunities from speakers to gatekeepers through discretionary balancing — not zero (there is genuine harm prevention) but not negligible (the weighing function is the extraction mechanism). Suppression (0.32) is moderate: the constraint does not categorically ban speech categories but creates chilling effects through unpredictable application. Theater ratio (0.28) captures that balancing rhetoric ('weighing interests') increasingly covers judicial preference rather than neutral rule application. Accessibility collapse (0.42) is moderate: alternatives (absolutist rules, harm-based categories) exist conceptually but are suppressed by the balancing framework's claim to be the only 'realistic' approach. Resistance (0.55) is significant: absolutist and harm-limited advocates actively contest the balancing framework. The measurement series (100-year interval from Schenck era to present) shows extraction and suppression rising as balancing displaces categorical rules, and theater increasing as the coordination justification thins.
 *
 * PERSPECTIVAL GAP:
 *   From the judiciary's seat (agenda_setter, institutional power, analytical exit), the constraint is a rope: it coordinates the genuine problem of applying fixed text to novel contexts. From marginalized speakers' seat (victim, powerless, trapped), it is a snare: the same discretion that allows contextual protection also allows contextual suppression, and the power to weigh 'harm' systematically disadvantages those with less institutional credibility. From the regulatory state's seat (beneficiary, institutional, arbitrage), it is a scaffold: the flexible standard serves transitional governance needs but has no sunset. The engine computes these seat divergences from the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   The judiciary and regulatory state are structural beneficiaries: they collect institutional authority, legitimacy, and enforcement flexibility (d → 0.0-0.2). Vulnerable groups advocacy benefits from harm recognition but lacks gatekeeping power (d → 0.3). Law enforcement benefits situationally (d → 0.2-0.4). Marginalized speakers, dissenters, artists, and whistleblowers are structural targets: they bear the cost of unpredictable weighing, face higher suppression when their speech challenges power, and lack exit (trapped or constrained by the very forums where balancing occurs). The excluded seat (absolutist advocates) would reject the weighing premise entirely but are not in the gatekeeping conversation.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (applying 18th-century text to modern speech contexts) remains live, but the balancing solution has accumulated extraction: judicial discretion now serves as a power reservoir rather than a coordination tool. The constraint prevents mislabeling by naming both the coordination function (contextual application) and the extraction (discretionary suppression of disfavored speech). The mandatrophy is unresolved: the original mandate (faithful application) has drifted into a self-justifying discretionary regime.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is this constraint one reading of the contested kernel ''speech_protection_boundary'', or is it the sole authoritative interpretation?',
    'Compare structural predictions across sibling readings (absolutist_reading, harm_limited_reading). If they produce different beneficiary/victim sets and different ε values for the same standing arrangement, the kernel is confirmed as contested.',
    'Confirms this is a kernel reading — the ε-invariance principle requires separate stories for each reading rather than one story with measurement-dependent classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Whether speech_protection_boundary is a kernel with multiple readings').

omega_variable(
    balancing_vs_absolutist_structural_delta,
    'Does the balancing reading''s case-by-case structure produce a materially different extraction profile from the absolutist reading''s categorical rule?',
    'Compare the two constraint stories'' base_properties: the balancing reading should show higher suppression (judicial discretion as enforcement) and different victim groups (marginalized speakers face context-dependent suppression) than the absolutist reading''s near-zero extraction for most speakers.',
    'If ε and victim/beneficiary structures differ significantly, the two readings are structurally distinct constraints — validating the kernel decomposition.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(balancing_vs_absolutist_structural_delta, empirical, 'Structural distinctness between balancing and absolutist readings').

omega_variable(
    harm_limited_reading_convergence,
    'Does the harm_limited_reading converge with the balancing reading on coded speech and systemic harm, or do they remain structurally distinct?',
    'Examine whether harm_limited_reading''s dignity/equality framework produces the same intermediate scrutiny for coded speech as balancing''s contextual weighing, or whether harm_limited_reading''s conditional protection creates systematically higher suppression for identity-based speech.',
    'If they converge on coded speech treatment but diverge on political speech, the kernel has a two-dimensional structure: one axis for harm sensitivity, one for categorical vs. contextual methodology.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(harm_limited_reading_convergence, conceptual, 'Whether harm_limited_reading and balancing_reading are siblings or overlapping variants').

omega_variable(
    judicial_gatekeeper_extraction,
    'Does distributed judicial gatekeeping constitute a coordination function (predictable standard) or extraction (discretionary power to suppress disfavored speech)?',
    'Track case outcomes over time: if similarly situated speakers receive different protection based on judicial composition rather than rule application, the gatekeeper role is extractive. If outcomes cluster around predictable contextual factors, it is coordinative.',
    'Determines whether the constraint''s coordination function is genuine (rope/tangled_rope) or cover for judicial preference (snare).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(judicial_gatekeeper_extraction, empirical, 'Whether judicial discretion in balancing is coordination or extraction').

omega_variable(
    systemic_harm_operationalization,
    'Can ''systemic harm'' and ''coded speech'' be operationalized into a stable legal standard, or do they inherently require case-by-case judgment that cannot be reduced to rules?',
    'Assess whether lower courts develop consistent sub-rules for systemic harm over time, or whether the standard remains irreducibly contextual. Empirical study of circuit splits and reversal rates on systemic harm claims.',
    'If irreducibly contextual, the constraint''s coordination function is inherently limited — it coordinates the *process* of judgment but not the *outcome*, making it structurally closer to a scaffold than a rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(systemic_harm_operationalization, conceptual, 'Whether systemic harm and coded speech admit stable sub-rules').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(speech_protection_boundary__balancing_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(spee_tr_t0, speech_protection_boundary__balancing_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement_basis(spee_tr_t0, observed).
narrative_ontology:measurement(spee_tr_t25, speech_protection_boundary__balancing_reading, theater_ratio, 25, 0.18).
narrative_ontology:measurement_basis(spee_tr_t25, observed).
narrative_ontology:measurement(spee_tr_t50, speech_protection_boundary__balancing_reading, theater_ratio, 50, 0.22).
narrative_ontology:measurement_basis(spee_tr_t50, observed).
narrative_ontology:measurement(spee_tr_t75, speech_protection_boundary__balancing_reading, theater_ratio, 75, 0.25).
narrative_ontology:measurement_basis(spee_tr_t75, observed).
narrative_ontology:measurement(spee_tr_t100, speech_protection_boundary__balancing_reading, theater_ratio, 100, 0.28).
narrative_ontology:measurement_basis(spee_tr_t100, observed).

% Extraction over time
narrative_ontology:measurement(spee_be_t0, speech_protection_boundary__balancing_reading, base_extractiveness, 0, 0.22).
narrative_ontology:measurement_basis(spee_be_t0, observed).
narrative_ontology:measurement(spee_be_t25, speech_protection_boundary__balancing_reading, base_extractiveness, 25, 0.28).
narrative_ontology:measurement_basis(spee_be_t25, observed).
narrative_ontology:measurement(spee_be_t50, speech_protection_boundary__balancing_reading, base_extractiveness, 50, 0.32).
narrative_ontology:measurement_basis(spee_be_t50, observed).
narrative_ontology:measurement(spee_be_t75, speech_protection_boundary__balancing_reading, base_extractiveness, 75, 0.35).
narrative_ontology:measurement_basis(spee_be_t75, observed).
narrative_ontology:measurement(spee_be_t100, speech_protection_boundary__balancing_reading, base_extractiveness, 100, 0.38).
narrative_ontology:measurement_basis(spee_be_t100, observed).

% Suppression requirement over time
narrative_ontology:measurement(spee_su_t0, speech_protection_boundary__balancing_reading, suppression_requirement, 0, 0.18).
narrative_ontology:measurement_basis(spee_su_t0, observed).
narrative_ontology:measurement(spee_su_t25, speech_protection_boundary__balancing_reading, suppression_requirement, 25, 0.22).
narrative_ontology:measurement_basis(spee_su_t25, observed).
narrative_ontology:measurement(spee_su_t50, speech_protection_boundary__balancing_reading, suppression_requirement, 50, 0.26).
narrative_ontology:measurement_basis(spee_su_t50, observed).
narrative_ontology:measurement(spee_su_t75, speech_protection_boundary__balancing_reading, suppression_requirement, 75, 0.29).
narrative_ontology:measurement_basis(spee_su_t75, observed).
narrative_ontology:measurement(spee_su_t100, speech_protection_boundary__balancing_reading, suppression_requirement, 100, 0.32).
narrative_ontology:measurement_basis(spee_su_t100, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(speech_protection_boundary__balancing_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(speech_protection_boundary__balancing_reading, 0.12).
narrative_ontology:affects_constraint(speech_protection_boundary__balancing_reading, speech_protection_boundary__absolutist_reading).
narrative_ontology:affects_constraint(speech_protection_boundary__balancing_reading, speech_protection_boundary__harm_limited_reading).
narrative_ontology:affects_constraint(speech_protection_boundary__balancing_reading, hate_speech_regulation).
narrative_ontology:affects_constraint(speech_protection_boundary__balancing_reading, campaign_finance_speech).
narrative_ontology:affects_constraint(speech_protection_boundary__balancing_reading, national_security_speech_restrictions).

% DUAL FORMULATION NOTE:
% The speech_protection_boundary kernel decomposes into three constraint stories: absolutist_reading (categorical protection, near-zero extraction for most speakers), balancing_reading (this story — contextual weighing, moderate extraction distributed through judicial discretion), and harm_limited_reading (conditional protection, high extraction for speech deemed harmful to protected groups). The balancing reading sits structurally between the other two: it shares the absolutist reading's commitment to case-by-case adjudication but the harm_limited reading's openness to harm-based restriction. The ε values differ substantially across readings because the referent (the standing arrangement of speech regulation) is assessed differently: absolutist sees minimal extraction, balancing sees moderate contextual extraction, harm_limited sees high structural extraction for identity-targeted speech.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(speech_protection_boundary__balancing_reading, institutional, 0.15).
constraint_indexing:directionality_override(speech_protection_boundary__balancing_reading, powerless, 0.85).
constraint_indexing:directionality_override(speech_protection_boundary__balancing_reading, moderate, 0.65).
constraint_indexing:directionality_override(speech_protection_boundary__balancing_reading, organized, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
