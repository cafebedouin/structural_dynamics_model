% ============================================================================
% CONSTRAINT STORY: speech_protection_boundary__harm_limited_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_speech_protection_boundary__harm_limited_reading, []).

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
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: speech_protection_boundary__harm_limited_reading
 *   human_readable: Speech Protection Conditional on Harm Limitation (Dignity-Equality-Harassment Reading)
 *   domain: constitutional_law/political_philosophy
 *
 * SUMMARY:
 *   This constraint instantiates the 'harm-limited reading' of the contested
 *   speech-protection kernel: it holds that First Amendment protection does
 *   not extend to speech causing significant harm to dignity, equal
 *   citizenship, and freedom from harassment. This is one of three
 *   structurally distinct readings of the same constitutional commitment (the
 *   First Amendment text). The harm-limited reading narrows the protected set
 *   by requiring state gatekeepers to assess whether speech meets a harm
 *   threshold before constitutional protection applies. The reading
 *   vindicates dignity, equality, and harassment-freedom as fundamental
 *   values that can limit speech; the absolutist reading treats those values
 *   as subordinate to near-absolute speech protection; the balancing reading
 *   treats the relationship as case-by-case contestable. The claim/metric gap
 *   is intentional: this reading is CLAIMED as tangled_rope (coordination of
 *   protected-group safety and speaker regulation paired with asymmetric harm
 *   to gatekeeping subjects) while the metrics show substantial
 *   extractiveness (0.68 by interval end) and rising suppression (0.71),
 *   reflecting the structural concern that harm-limitation criteria are
 *   vague, unevenly enforced, and subject to state capture.
 *
 * KEY AGENTS:
 *   - Protected minorities: benefit from recourse against harm-causing speech; face no legal barriers to participation
 *   - State gatekeepers (courts, legislatures, agencies): set and enforce harm criteria; bear accountability for asymmetric application
 *   - Speakers subject to gatekeeping: bear regulatory costs; constrained by harm-limitation threshold; face legal and social sanction
 *   - Platform operators: operationalize harm criteria; benefit from reduced liability and community trust; hold enforcement authority
 *   - Absolutist civil libertarians: excluded from this reading's legitimacy frame; actively contest the framework
 *   - Harm-definition philosophers: provide evidence for and against specific harm criteria; surface empirical and definitional uncertainties
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(speech_protection_boundary__harm_limited_reading, 0.68).
domain_priors:suppression_score(speech_protection_boundary__harm_limited_reading, 0.71).
domain_priors:theater_ratio(speech_protection_boundary__harm_limited_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(speech_protection_boundary__harm_limited_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(speech_protection_boundary__harm_limited_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(speech_protection_boundary__harm_limited_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(speech_protection_boundary__harm_limited_reading, accessibility_collapse, 0.64).
narrative_ontology:constraint_metric(speech_protection_boundary__harm_limited_reading, resistance, 0.78).

% --- Constraint claim ---
narrative_ontology:constraint_claim(speech_protection_boundary__harm_limited_reading, tangled_rope).
narrative_ontology:human_readable(speech_protection_boundary__harm_limited_reading, "Speech Protection Conditional on Harm Limitation (Dignity-Equality-Harassment Reading)").
narrative_ontology:topic_domain(speech_protection_boundary__harm_limited_reading, "constitutional_law/political_philosophy").

domain_priors:requires_active_enforcement(speech_protection_boundary__harm_limited_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(speech_protection_boundary__harm_limited_reading, '87862d51-e14c-4c15-b0aa-a285a9f57813').
narrative_ontology:cs_kernel_codification('87862d51-e14c-4c15-b0aa-a285a9f57813', formalized).
narrative_ontology:cs_authority_grounding('87862d51-e14c-4c15-b0aa-a285a9f57813', lineage).
narrative_ontology:cs_interpretation_layer_present('87862d51-e14c-4c15-b0aa-a285a9f57813').
narrative_ontology:cs_reading_relation('87862d51-e14c-4c15-b0aa-a285a9f57813', speech_protection_boundary__absolutist_reading, coexists_with).
narrative_ontology:cs_reading_relation('87862d51-e14c-4c15-b0aa-a285a9f57813', speech_protection_boundary__balancing_reading, influences).
narrative_ontology:cs_axiom('87862d51-e14c-4c15-b0aa-a285a9f57813', foundational, harm_to_dignity_limits_speech).
narrative_ontology:cs_axiom_status(harm_to_dignity_limits_speech, holdable).
narrative_ontology:cs_axiom_grounding('87862d51-e14c-4c15-b0aa-a285a9f57813', harm_to_dignity_limits_speech, deontological).
narrative_ontology:cs_axiom('87862d51-e14c-4c15-b0aa-a285a9f57813', foundational, equal_citizenship_constrains_harassment).
narrative_ontology:cs_axiom_status(equal_citizenship_constrains_harassment, holdable).
narrative_ontology:cs_axiom_grounding('87862d51-e14c-4c15-b0aa-a285a9f57813', equal_citizenship_constrains_harassment, deontological).
narrative_ontology:cs_reference_frame('87862d51-e14c-4c15-b0aa-a285a9f57813', speech_protection_with_harm_exception).
narrative_ontology:cs_drift_state('87862d51-e14c-4c15-b0aa-a285a9f57813', contemporary_capture_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('87862d51-e14c-4c15-b0aa-a285a9f57813', '').
narrative_ontology:cs_kernel_id(speech_protection_boundary__harm_limited_reading, speech_protection_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(speech_protection_boundary__harm_limited_reading, protected_minorities).
narrative_ontology:constraint_beneficiary(speech_protection_boundary__harm_limited_reading, historically_marginalized_groups).
narrative_ontology:constraint_victim(speech_protection_boundary__harm_limited_reading, speech_regulated_under_harm_criteria).
narrative_ontology:constraint_victim(speech_protection_boundary__harm_limited_reading, speakers_subject_to_gatekeeping).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(speech_protection_boundary__harm_limited_reading, platform_operators).
narrative_ontology:constraint_vindicates(speech_protection_boundary__harm_limited_reading, dignity_as_fundamental_right).
narrative_ontology:constraint_vindicates(speech_protection_boundary__harm_limited_reading, equality_as_constitutional_value).
narrative_ontology:constraint_vindicates(speech_protection_boundary__harm_limited_reading, freedom_from_harassment_as_liberty_interest).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Groups historically targeted by hate speech, harassment, and coded dog-whistle rhetoric (religious minorities, racial minorities, gender minorities, immigrants). This reading provides legal recourse against speech that degrades their dignity, undermines equal citizenship, or creates conditions of persistent harassment. They are insulated from harm-causing expression without needing to prove imminent violence. Their exit option is participation in civic life; the constraint narrows speech that drives them out.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__harm_limited_reading, protected_minorities, beneficiary,
    organized, generational, constrained, national).

% Courts, legislatures, and administrative agencies tasked with determining which speech crosses the harm threshold (dignity, equality, harassment). They adjudicate contested cases, define 'significant harm,' and set precedent. They hold the interpretive power to expand or contract the category of harm-limited speech. They bear the accountability risk if gatekeeping criteria are abused or applied asymmetrically.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__harm_limited_reading, state_gatekeepers, agenda_setter,
    institutional, generational, analytical, national).

% Speakers whose expression (hate speech, harassment, dehumanizing rhetoric, coded epithets) falls under the harm-limitation reading. They face legal liability, platforms removal, social sanction. Their exit option is self-censorship or migration to unregulated platforms. The constraint requires them to verify their speech does not cause significant harm before speaking publicly.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__harm_limited_reading, speakers_subject_to_gatekeeping, payer,
    moderate, biographical, constrained, national).

% Organizations and thinkers (ACLU civil liberties wing, originalist scholars, anti-censorship advocates) would argue that narrowing the harm exception to Brandenburg standard is essential to prevent state capture of speech regulation. They are excluded from the harm-limiting reading's legitimacy frame but actively contest it in litigation and policy discourse.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__harm_limited_reading, absolutist_civil_libertarians, excluded,
    organized, generational, constrained, national).

% Social media and content platforms enforce harm-limitation rules via their terms of service, which typically track or exceed the legal threshold. They benefit from reduced liability exposure and community-trust credibility; they set and police the harm criteria operationally. Their enforcement is decentralized compared to state gatekeeping but follows the same conceptual framework.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__harm_limited_reading, platform_operators, agenda_setter,
    institutional, biographical, mobile, global).
narrative_ontology:stakeholder_secondary_role(speech_protection_boundary__harm_limited_reading, platform_operators, beneficiary).

% The structural possibility that state gatekeepers asymmetrically apply harm criteria to suppress minority or dissenting speech while permitting majority or regime-aligned harm. This is an abstract risk, not an agent, but is central to the constraint's contestation.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__harm_limited_reading, regulatory_capture_risk, observer,
    analytical, generational, analytical, national).
narrative_ontology:stakeholder_non_agent(speech_protection_boundary__harm_limited_reading, regulatory_capture_risk).

% Theorists and empiricists who study whether dignity harm, equality-degradation, and harassment causally produce documented individual and social harms (mental health outcomes, civic withdrawal, educational disparities). Their evidence-gathering feeds the legitimacy of harm-limitation criteria but also surfaces evidentiary uncertainties and value-laden definitions.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__harm_limited_reading, harm_definition_philosophers, observer,
    powerful, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(speech_protection_boundary__harm_limited_reading, state_gatekeepers).
narrative_ontology:fixing_cost_class(speech_protection_boundary__harm_limited_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Prevents speech-driven systemic harm to protected groups' dignity, equal citizenship standing, and freedom from targeted harassment. Solves the collective-action problem of individual speakers' autonomy competing with victims' safety and equal standing — a framework that coordinates protection without requiring unanimous consent from all speakers.
% TRANSFER_FUNCTION: Moves regulatory authority from speakers (who decide what to say) to state gatekeepers and platforms (who decide what speech is permissible). The arrangement also transfers reputational and social costs from speakers (who face legal or platform sanction) to targeted groups' accusers (who bear burden of proving significant harm). Transfers epistemic authority about what counts as 'dignity' or 'equal citizenship' from lived experience to legal definitions.
% ABSENT_VOICES: Absolutist civil libertarians, speakers whose speech is caught by the harm net, and those concerned about state capture of definitional authority are structurally excluded from framing this reading's core legitimacy. They would testify that the gatekeeping criteria are vague, selectively enforced, and prone to suppressing minority dissent masked as 'harm reduction'. Speakers in marginalized groups who fear they will be weaponized against (minorities silenced by hostile gatekeeping of 'harm') would also object.
% DISAPPEARANCE_RATIONALE: If the harm-limitation reading vanished and absolute speech protection (Brandenburg standard) resumed, protected groups would lose statutory recourse against hate speech and harassment; civic participation would shift for targeted minorities; platform policies would revert to narrower liability exemptions; the court docket would reorganize around different speech disputes. The removal would be consequential and contested.
% FOUNDING_PROBLEM: Racist, misogynist, transphobic, and antisemitic speech caused documented cascading harms to dignity and equal citizenship: exclusion from civic spaces, targeted harassment, radicalization pipelines, and erosion of subordinated groups' sense of equal standing. Speech protection as written left these harms legal, creating a gap between formal equality and lived experience.
% FOUNDING_PROBLEM_CORROBORATION: Civil rights organizations, targeted communities, and empirical researchers document the harms and testify the founding problem is live. Absolutist scholars and civil libertarians attest the problem is real but argue the cure (narrowed speech protection) is worse than the disease (state gatekeeping risk). Legislative history in EU jurisdictions that adopted harm-limitation frameworks (e.g., hate speech bans, harassment laws) provides corroboration from outside the US beneficiary set, though US absolutists contest the framing. Universities and corporate HR departments report that speech-harm incidents drive student/employee departures and affect equal access.
narrative_ontology:disappearance_verdict(speech_protection_boundary__harm_limited_reading, world_rearranges).
narrative_ontology:founding_problem_status(speech_protection_boundary__harm_limited_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(speech_protection_boundary__harm_limited_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(speech_protection_boundary__harm_limited_reading, 'none', 1).
narrative_ontology:epsilon_provenance(speech_protection_boundary__harm_limited_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(speech_protection_boundary__harm_limited_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(speech_protection_boundary__harm_limited_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(speech_protection_boundary__harm_limited_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.68) because state gatekeepers and platforms hold asymmetric authority to define 'significant harm' and enforce it, while speakers subject to the criteria have constrained exit (self-censorship or migration to unregulated spaces) and bear reputational/legal costs. Suppression is also high (0.71) because the constraint depends on actively excluding speech that falls below the harm threshold — the enforcement machinery sustains the boundary. Theater rises over the interval (0.25 to 0.42) because as harm-limitation criteria proliferate, definitional work increases: what counts as 'dignity harm' or 'coded dog whistle' becomes contested, and enforcement activity increasingly defends the gatekeeping authority itself rather than protecting the safety of targeted groups. Accessibility collapse is moderate (0.64): speakers can exit through self-censorship or code-switching, but meaningful alternatives (unregulated platforms, private speech) have lower reach and credibility. Resistance is high (0.78): absolutist legal scholars, civil libertarians, speakers, and regulatory-capture critics actively challenge the harm-limitation framework through litigation, legislation, and cultural contestation. The measurement series tracks the constraint over 40 units: extractiveness plateaus after t=30, theater stabilizes around 0.42, and suppression reaches steady-state 0.71 — the constraint matures into a stable institutional arrangement after initial phase-in.
 *
 * PERSPECTIVAL GAP:
 *   From the state gatekeeper's seat, this is genuine coordination: it protects vulnerable groups, upholds equal dignity, and solves the collective-action problem of individual speaker autonomy overwhelming targets' safety. From the speaker's seat (especially those whose speech is caught by the harm net), this is extractive state gatekeeping with vague criteria and asymmetric enforcement. From the protected-minority seat, it is coordination that restores their equal standing and safe civic participation. The engine computes these three divergent types from the structural data: agenda-setter/beneficiary seats compute lower χ; payer seats compute higher χ. No reconciliation is attempted — the divergence IS the measurement.
 *
 * DIRECTIONALITY LOGIC:
 *   Protected minorities have low directionality (near 0.0, beneficiary end): they gain direct safety and equal standing; exit is civic participation restored, not exit from the constraint. State gatekeepers and platforms sit near symmetric (d ≈ 0.5): they both coordinate protection and extract gatekeeping authority; they bear accountability risk proportional to their power. Speakers subject to the harm net have high directionality (near 1.0, target end): they pay regulatory costs, face constrained exit, and have their expression bounded; they gain no coordination benefit — the coordination is for protected groups' safety, not theirs. Absolutist civil libertarians, though excluded, would compute high d if included: the constraint directly contradicts their core premise (near-absolute speech protection). The divergence between agenda-setter seats (moderate d → moderate χ) and payer seats (high d → high χ) is the key perspectival gap: state gatekeepers experience the constraint as coordinating public safety; speakers experience it as enforced extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (harm to dignity, equal citizenship, and safety from harassment) is live and widely corroborated by civil rights organizations and empirical researchers. However, the mandate of the harm-limitation reading has partially outlived its founding problem in contexts where it has matured into gatekeeping authority: enforcement activity increasingly concerns itself with policing the harm boundary itself rather than addressing concrete harms. Theater-ratio rise (0.25 to 0.42) reflects this: definitional work and contestation now consume more activity than the baseline protection. This is not mandatrophy resolution (the founding problem has not disappeared), but it signals scope-creep: the harm-limitation framework now regulates not just hate speech but coded language, satire, parody, and political opposition speech that falls ambiguously under 'dignity harm' or 'harassment.' The constraint remains justified by its founding problem but increasingly operates as a gatekeeper for ambiguous cases, extracting from speakers in the margin.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    harm_definition_contestation,
    'What counts as ''significant harm'' to dignity, equal citizenship, and freedom from harassment? Is the threshold empirically determinable or fundamentally contested?',
    'Empirical study of causal pathways from speech exposure to documented harms (psychological, civic, educational). Philosophical and political analysis of whether dignity and equality admit precise definitions or remain inherently contestable. Audit of gatekeeping decisions to assess whether harm criteria are applied consistently or vary with speaker identity and content politics.',
    'If harm is empirically determinable and consistently applied, the constraint credibly balances speaker freedom and target safety. If harm is fundamentally contested or applied asymmetrically, gatekeeping becomes capture-prone and the constraint shifts toward snare classification (pure extraction masked as coordination). If thresholds vary with speaker identity, the constraint exhibits facially neutral rules applied with discriminatory effect.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(harm_definition_contestation, empirical, 'Boundaries of ''significant harm'' and consistency of gatekeeper application.').

omega_variable(
    state_capture_risk_structural,
    'Can harm-limitation criteria be administered without asymmetric or retaliatory application by state actors seeking to suppress minority or dissenting speech?',
    'Historical audit of speech prosecutions and sanctions under harm-limitation regimes (EU hate-speech bans, platform moderation). Empirical measurement of whether speech by majority groups receives differential leniency. Analysis of feedback loops: if state applies criteria asymmetrically against minorities, does that fact itself become a harm justifying further speech restrictions (a ratchet)?',
    'If state/platform capture is systemic (not occasional), the constraint functions as a snare: harm-limitation framing provides legitimacy cover for suppressing disfavored speech regardless of actual harm. If capture is marginal and correctable by oversight, the constraint credibly operates as tangled_rope (coordination with manageable extraction). If capture is absent, the harm-limitation reading succeeds structurally.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(state_capture_risk_structural, empirical, 'Asymmetric or retaliatory application of harm criteria by state/platform gatekeepers.').

omega_variable(
    reading_foreclosure_test,
    'Does the harm-limited reading logically foreclose the absolutist reading within the same constitutional framework, or do they coexist as competing interpretations?',
    'Close textual and originalist analysis of the First Amendment. Examination of whether the harm-limited reading can be squared with the original meaning or whether it requires departing from it. Historical survey of constitutional amendments or jurisprudential shifts that might have resolved the conflict.',
    'If the readings foreclose each other (one cannot be both absolutist and harm-limited about the same speech), the contest is binary and one reading must be rejected. If they coexist as interpretive live options held by different constitutional actors (courts, legislatures, movements), the constraint remains in contestation. Foreclosure status affects the engine''s reading_relations classification and omega probability.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_foreclosure_test, conceptual, 'Whether absolutist and harm-limited readings are logically incompatible or coexisting interpretive live options.').

omega_variable(
    beneficiary_identity_lock,
    'Do protected minorities become identity-locked to the harm-limitation framework, such that exit (rejecting the constraint) would require abandoning their group identity or advocacy?',
    'Survey and interview research on whether protected minorities perceive the constraint as liberatory (exit into harm) or coercive (trapped by the state''s definition of their protection). Historical analysis of whether minority groups have chosen opt-out from legal protection (suggesting voluntary association) or have been forced to accept it.',
    'If identity-lock is high, protected minorities compute as trapped/identity_locked on the exit dimension, raising directionality toward the payer end despite their nominal beneficiary role. This would reclassify the constraint as extractive toward those who are supposed to benefit (a false-beneficiary scenario). If minorities have agency in the framework and can navigate it or contest it, exit remains constrained but not identity-locked.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(beneficiary_identity_lock, empirical, 'Whether protected minorities are identity-locked to the harm-limitation framework or retain agency to contest it.').

omega_variable(
    absolutist_coexistence,
    'This reading coexists with the absolutist_reading as competing judicial and political factions hold each. Neither reading logically forecloses the other within the broad tradition of First Amendment interpretation — both are live options for US constitutional actors. What conditions would cause one to overcome the other?',
    'Constitutional amendment, major jurisprudential shift (e.g., Supreme Court reversal), or institutional capture by one faction. Empirical measure of which reading gains ground in legislative, judicial, and public discourse over time.',
    'If coexistence holds, the kernel remains in active contestation and both constraints remain live. If one reading is institutionally marginalized (loses court backing, legislative support, public credibility), the other may approach dominance, reducing the constraint''s contested status and changing its effective classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(absolutist_coexistence, empirical, 'Institutional stability of coexistence between harm-limited and absolutist readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(speech_protection_boundary__harm_limited_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(spee_tr_t0, speech_protection_boundary__harm_limited_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement_basis(spee_tr_t0, observed).
narrative_ontology:measurement(spee_tr_t5, speech_protection_boundary__harm_limited_reading, theater_ratio, 5, 0.28).
narrative_ontology:measurement_basis(spee_tr_t5, observed).
narrative_ontology:measurement(spee_tr_t10, speech_protection_boundary__harm_limited_reading, theater_ratio, 10, 0.31).
narrative_ontology:measurement_basis(spee_tr_t10, observed).
narrative_ontology:measurement(spee_tr_t15, speech_protection_boundary__harm_limited_reading, theater_ratio, 15, 0.35).
narrative_ontology:measurement_basis(spee_tr_t15, observed).
narrative_ontology:measurement(spee_tr_t20, speech_protection_boundary__harm_limited_reading, theater_ratio, 20, 0.38).
narrative_ontology:measurement_basis(spee_tr_t20, observed).
narrative_ontology:measurement(spee_tr_t25, speech_protection_boundary__harm_limited_reading, theater_ratio, 25, 0.4).
narrative_ontology:measurement_basis(spee_tr_t25, observed).
narrative_ontology:measurement(spee_tr_t30, speech_protection_boundary__harm_limited_reading, theater_ratio, 30, 0.41).
narrative_ontology:measurement_basis(spee_tr_t30, observed).
narrative_ontology:measurement(spee_tr_t40, speech_protection_boundary__harm_limited_reading, theater_ratio, 40, 0.42).
narrative_ontology:measurement_basis(spee_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(spee_be_t0, speech_protection_boundary__harm_limited_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement_basis(spee_be_t0, observed).
narrative_ontology:measurement(spee_be_t5, speech_protection_boundary__harm_limited_reading, base_extractiveness, 5, 0.48).
narrative_ontology:measurement_basis(spee_be_t5, observed).
narrative_ontology:measurement(spee_be_t10, speech_protection_boundary__harm_limited_reading, base_extractiveness, 10, 0.54).
narrative_ontology:measurement_basis(spee_be_t10, observed).
narrative_ontology:measurement(spee_be_t15, speech_protection_boundary__harm_limited_reading, base_extractiveness, 15, 0.6).
narrative_ontology:measurement_basis(spee_be_t15, observed).
narrative_ontology:measurement(spee_be_t20, speech_protection_boundary__harm_limited_reading, base_extractiveness, 20, 0.64).
narrative_ontology:measurement_basis(spee_be_t20, observed).
narrative_ontology:measurement(spee_be_t25, speech_protection_boundary__harm_limited_reading, base_extractiveness, 25, 0.66).
narrative_ontology:measurement_basis(spee_be_t25, observed).
narrative_ontology:measurement(spee_be_t30, speech_protection_boundary__harm_limited_reading, base_extractiveness, 30, 0.68).
narrative_ontology:measurement_basis(spee_be_t30, observed).
narrative_ontology:measurement(spee_be_t40, speech_protection_boundary__harm_limited_reading, base_extractiveness, 40, 0.68).
narrative_ontology:measurement_basis(spee_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(spee_su_t0, speech_protection_boundary__harm_limited_reading, suppression_requirement, 0, 0.54).
narrative_ontology:measurement_basis(spee_su_t0, observed).
narrative_ontology:measurement(spee_su_t5, speech_protection_boundary__harm_limited_reading, suppression_requirement, 5, 0.59).
narrative_ontology:measurement_basis(spee_su_t5, observed).
narrative_ontology:measurement(spee_su_t10, speech_protection_boundary__harm_limited_reading, suppression_requirement, 10, 0.63).
narrative_ontology:measurement_basis(spee_su_t10, observed).
narrative_ontology:measurement(spee_su_t15, speech_protection_boundary__harm_limited_reading, suppression_requirement, 15, 0.67).
narrative_ontology:measurement_basis(spee_su_t15, observed).
narrative_ontology:measurement(spee_su_t20, speech_protection_boundary__harm_limited_reading, suppression_requirement, 20, 0.69).
narrative_ontology:measurement_basis(spee_su_t20, observed).
narrative_ontology:measurement(spee_su_t25, speech_protection_boundary__harm_limited_reading, suppression_requirement, 25, 0.7).
narrative_ontology:measurement_basis(spee_su_t25, observed).
narrative_ontology:measurement(spee_su_t30, speech_protection_boundary__harm_limited_reading, suppression_requirement, 30, 0.71).
narrative_ontology:measurement_basis(spee_su_t30, observed).
narrative_ontology:measurement(spee_su_t40, speech_protection_boundary__harm_limited_reading, suppression_requirement, 40, 0.71).
narrative_ontology:measurement_basis(spee_su_t40, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(speech_protection_boundary__harm_limited_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(speech_protection_boundary__harm_limited_reading, 0.18).
narrative_ontology:affects_constraint(speech_protection_boundary__harm_limited_reading, speech_protection_boundary__absolutist_reading).
narrative_ontology:affects_constraint(speech_protection_boundary__harm_limited_reading, speech_protection_boundary__balancing_reading).

% DUAL FORMULATION NOTE:
% This constraint is part of a constraint family decomposing the contested kernel 'speech_protection_boundary' into three structurally distinct readings. The absolutist reading holds near-absolute speech protection (ε ≈ 0.15); the balancing reading treats speech protection as case-by-case contestable (ε ≈ 0.45, moderate extraction); the harm-limited reading (this one) narrows protected speech through harm criteria (ε ≈ 0.68, higher extraction). Each reading has different beneficiaries, victims, and gatekeeping mechanisms. They are linked by the shared kernel and compete for institutional dominance. The harm-limited reading influences both siblings by narrowing the speech space available to speak and the criteria by which absolutist and balancing arguments can be made.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(speech_protection_boundary__harm_limited_reading, organized, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
