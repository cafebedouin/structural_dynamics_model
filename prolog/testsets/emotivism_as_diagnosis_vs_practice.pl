% ============================================================================
% CONSTRAINT STORY: emotivism_as_diagnosis_vs_practice
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_emotivism_as_diagnosis_vs_practice, []).

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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: emotivism_as_diagnosis_vs_practice
 *   human_readable: Emotivism Diagnosed as False While Performed as Practice
 *   domain: moral_philosophy/metaethics
 *
 * SUMMARY:
 *   This constraint tracks a specific structural gap within a broader
 *   metaethical argument (upstream: authority_vacuum_incommensurability, the
 *   MacIntyre-style diagnosis that competing moral frameworks share no
 *   adjudicating authority). The author explicitly argues that emotivism —
 *   the theory that moral utterances are disguised expressions of attitude
 *   rather than truth-apt claims — is false as a general account of moral
 *   language. But in practice, when characterizing certain people or
 *   positions, the author uses precisely the linguistic form emotivism
 *   predicts: bare attitude-expression functioning as social command
 *   ('garbage people who should be shamed and shunned'), with no accompanying
 *   argument establishing the moral verdict on independent grounds. The gap
 *   between the stated metaethical commitment (moral realism or cognitivism,
 *   at minimum anti-emotivism) and the revealed practice (performing
 *   emotivism) is the constraint. This is downstream of
 *   authority_vacuum_incommensurability because the vacuum of adjudicating
 *   authority is precisely what allows the practice/diagnosis gap to go
 *   unpoliced — there is no shared standard by which the author's
 *   inconsistency could be authoritatively called out and corrected, only
 *   competing readings of whether it matters.
 *
 * KEY AGENTS:
 *   - author_self_consistency_narrative: beneficiary/agenda_setter (institutional/arbitrage) — accrues rhetorical authority from the diagnosis while exempting own practice from it
 *   - argumentative_coherence: payer (analytical/trapped) — the standard violated, cannot exit its own violation
 *   - readers_persuaded_by_diagnosis: beneficiary/payer (moderate/constrained) — gains explanatory frame, absorbs unargued shaming as if argued
 *   - targets_of_shaming_language: excluded (powerless/trapped) — condemned without argument, no seat in the text
 *   - metaethical_realist_critics: observer (analytical/analytical) — sees the gap, cannot compel consistency
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(emotivism_as_diagnosis_vs_practice, 0.62).
domain_priors:suppression_score(emotivism_as_diagnosis_vs_practice, 0.48).
domain_priors:theater_ratio(emotivism_as_diagnosis_vs_practice, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(emotivism_as_diagnosis_vs_practice, extractiveness, 0.62).
narrative_ontology:constraint_metric(emotivism_as_diagnosis_vs_practice, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(emotivism_as_diagnosis_vs_practice, theater_ratio, 0.55).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(emotivism_as_diagnosis_vs_practice, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(emotivism_as_diagnosis_vs_practice, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(emotivism_as_diagnosis_vs_practice, tangled_rope).
narrative_ontology:human_readable(emotivism_as_diagnosis_vs_practice, "Emotivism Diagnosed as False While Performed as Practice").
narrative_ontology:topic_domain(emotivism_as_diagnosis_vs_practice, "moral_philosophy/metaethics").

domain_priors:requires_active_enforcement(emotivism_as_diagnosis_vs_practice).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(emotivism_as_diagnosis_vs_practice, author_self_consistency_narrative).
narrative_ontology:constraint_victim(emotivism_as_diagnosis_vs_practice, argumentative_coherence).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(emotivism_as_diagnosis_vs_practice, readers_persuaded_by_diagnosis).
narrative_ontology:constraint_victim(emotivism_as_diagnosis_vs_practice, readers_persuaded_by_diagnosis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The author occupies the rhetorically privileged position of the anti-emotivist diagnostician: MacIntyre-style argument that emotivism is a false metaethical theory of moral language, used to explain why post-Enlightenment moral debate is interminable. Having established this diagnostic authority, the author is free to deploy pure attitude-expression language ('garbage people who should be shamed and shunned') without it being read as emotivist practice, because the reader has already been told emotivism is what OTHER people's moral language does. The author's own utterances are exempted from the diagnosis by narrative fiat, not by argument.
narrative_ontology:constraint_stakeholder(emotivism_as_diagnosis_vs_practice, author_self_consistency_narrative, beneficiary,
    institutional, biographical, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(emotivism_as_diagnosis_vs_practice, author_self_consistency_narrative, agenda_setter).

% Coherence itself bears the cost: the argument's own standards (that moral utterances of the form 'X is bad' are either genuine truth-claims answerable to reasons, or mere expressions of attitude functioning as social commands to align feeling) are violated by the author's practice. There is no exit for coherence — it either holds across the whole text or it does not; it cannot selectively excuse the diagnostician's own sentences. The victim here is a standard, not a person, but the standard is what makes the anti-emotivist argument worth anything.
narrative_ontology:constraint_stakeholder(emotivism_as_diagnosis_vs_practice, argumentative_coherence, payer,
    analytical, civilizational, trapped, universal).

% Readers accept the metaethical diagnosis (moral disagreement is interminable because interlocutors are talking past each other, expressing rival attitudes while claiming objectivity) as a genuine explanatory gain. They benefit from the explanatory frame but simultaneously absorb the author's shaming language as though it were argument-backed judgment rather than attitude-expression — they are recruited into treating 'shamed and shunned' as a conclusion rather than a performative act the author's own theory says it is.
narrative_ontology:constraint_stakeholder(emotivism_as_diagnosis_vs_practice, readers_persuaded_by_diagnosis, beneficiary,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(emotivism_as_diagnosis_vs_practice, readers_persuaded_by_diagnosis, payer).

% Whoever is labeled 'garbage people' by the author has no seat in the argument. They are the object of the performative attitude-expression, not a party being reasoned with. If emotivism is true of this specific utterance (as the author's own performance suggests), then no argument was actually offered against them — only an attitude was expressed and social pressure invoked. If emotivism is false in general (as the author argues), then the author owes them an actual argument, which is not supplied.
narrative_ontology:constraint_stakeholder(emotivism_as_diagnosis_vs_practice, targets_of_shaming_language, excluded,
    powerless, immediate, trapped, local).

% Philosophers committed to moral realism or to careful anti-emotivist argument (in the MacIntyre/Anscombe/Foot tradition) can observe the gap directly: they hold the same conclusion (emotivism is false) as the author but object to the author's practice discrediting the position by exhibiting the very disorder it diagnoses. They have no power to compel consistency, only to name it.
narrative_ontology:constraint_stakeholder(emotivism_as_diagnosis_vs_practice, metaethical_realist_critics, observer,
    analytical, generational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(emotivism_as_diagnosis_vs_practice, author_self_consistency_narrative).
narrative_ontology:fixing_cost_class(emotivism_as_diagnosis_vs_practice, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The anti-emotivist argument, taken seriously, coordinates a genuine philosophical function: distinguishing moral utterances that are truth-apt and reason-responsive from utterances that are merely attitude-expressive social pressure dressed as judgment. This distinction, if honored, would raise the bar for what counts as a moral argument and protect targets of moral condemnation from being condemned by mere attitude-expression masquerading as reasoned verdict.
% TRANSFER_FUNCTION: Rhetorical authority is transferred from the argument's actual content to the author's diagnostic posture: by naming emotivism as the failure mode of OTHERS, the author accrues license to perform emotivist moves without being held to the same charge, at the expense of the coherence the argument claims to establish and at the expense of whoever is condemned by the unargued attitude-expression.
% ABSENT_VOICES: The people called 'garbage people who should be shamed and shunned' have no voice in the text at all — they are named as targets of an attitude, not addressed as interlocutors who might have a defense. Their absence is exactly what licenses the emotivist move: there is no one present to ask 'what is the argument for this, specifically?'
% DISAPPEARANCE_RATIONALE: If the gap disappeared — if the author's practice were brought into line with the stated metaethical commitment — the specific rhetorical effect (borrowed authority of 'I have refuted emotivism, therefore my moral judgments are not mere attitude-expression') would collapse, and the author would need to supply actual arguments for the shaming conclusions. Readers who found the diagnosis persuasive would still have the diagnosis; they would lose the free pass currently extended to the author's own attitude-laden conclusions. Whether this constitutes 'the world rearranging' is disputed: defenders would say nothing changes because the diagnosis stands on its own merits regardless of the author's practice; critics would say the rhetorical force of the whole passage depends on the borrowed authority and would deflate substantially.
% FOUNDING_PROBLEM: The anti-emotivist argument was built to solve a real problem: explain why modern moral disagreements (about abortion, justice, war) are structurally interminable, by showing that participants use realist-sounding language while actually only expressing and trying to universalize personal or communal attitudes, with no shared rational method to adjudicate between them.
% FOUNDING_PROBLEM_CORROBORATION: The diagnostic claim about interminable moral disagreement is corroborated by independent historians of ethics (MacIntyre's own scholarly reception, debated but taken seriously outside his own school) and by descriptive linguists of moral discourse. The specific charge that THIS author's own practice reproduces emotivism is corroborated only by close readers and critics outside the author's own framework — no one within the author's own rhetorical frame, which benefits from the exemption, attests to the gap; that silence from the beneficiary side is itself notable.
narrative_ontology:disappearance_verdict(emotivism_as_diagnosis_vs_practice, contested).
narrative_ontology:founding_problem_status(emotivism_as_diagnosis_vs_practice, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(emotivism_as_diagnosis_vs_practice, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-22',
    'unspecified', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'unspecified').
narrative_ontology:story_seed(emotivism_as_diagnosis_vs_practice, 'none', 1).
narrative_ontology:epsilon_provenance(emotivism_as_diagnosis_vs_practice, 0.62, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(emotivism_as_diagnosis_vs_practice_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(emotivism_as_diagnosis_vs_practice, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(emotivism_as_diagnosis_vs_practice_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62) and theater_ratio (0.55) are both moderate-to-high and rising over the interval because the constraint's persistence depends on repeated performance: each new instance of unargued shaming language re-extracts the borrowed authority of 'I have already refuted emotivism' without re-earning it through argument. Theater ratio is high because the anti-emotivist argument's genuine philosophical content (a real, defensible position in metaethics) increasingly serves as cover/decoration for practice that doesn't need to meet its own bar. Suppression (0.48) is moderate — there is no formal enforcement mechanism silencing critics, but the social cost of pointing out the inconsistency (accusing a serious philosophical interlocutor of hypocrisy) creates real friction. Accessibility collapse is only moderate (0.4) because the inconsistency is visible to careful readers; it has not fully foreclosed critique, which is why resistance (0.6) is substantial — critics within the same broad tradition object.
 *
 * PERSPECTIVAL GAP:
 *   From the author's own rhetorical position, there is no gap at all — the shaming language is read as a conclusion following naturally from established moral seriousness, not as unargued attitude-expression. From argumentative coherence's position (and the critic's observer position), the gap is exactly the emotivist move the author claims to have refuted. This is the seat divergence the engine should surface: same text, computed differently from the agenda-setter/beneficiary seat versus the payer/observer seats.
 *
 * DIRECTIONALITY LOGIC:
 *   The author benefits (low d) because the diagnostic move — 'emotivism is false, and I have shown why' — grants exemption-by-association: readers extend the presumption of rigor from the argued portions of the text to the unargued shaming portions. Argumentative coherence is the victim (high d) in a metaphorical but structurally real sense: it is the standard that is spent down every time the gap widens without correction. Readers occupy a genuinely mixed position — real explanatory benefit, real absorption of unearned conclusions. Targets of the shaming language are excluded rather than merely victimized in the classic sense: they are not even in the argument to be harmed by a bad argument; they are simply named and dismissed.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification prevents two symmetrical mislabelings. First, it prevents treating the entire anti-emotivist argument as pure extraction/theater (a pure snare reading) — the coordination function is real: distinguishing reason-responsive from attitude-expressive moral language is a genuine philosophical service, independent of whether this particular author lives up to it. Second, it prevents treating the gap as a harmless rhetorical flourish with no structural cost — the tangled_rope classification insists that the coordination function (real distinction, real diagnostic value) and the extraction (borrowed unearned authority for unargued condemnation) coexist in the SAME textual structure and cannot be separated by charitable reading alone. Active enforcement in this frame is social/reputational: the author's standing as a serious anti-emotivist thinker actively suppresses the salience of the gap for readers already persuaded by the diagnosis.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    intentional_vs_incidental_inconsistency,
    'Is the emotivist practice a deliberate rhetorical strategy (borrowing the diagnosis''s authority while knowing the shaming language is unargued) or an incidental lapse inconsistent with the author''s own stated commitments but not strategically exploited?',
    'Close textual analysis across the author''s full corpus for whether the pattern is isolated or systematic, and whether the author elsewhere acknowledges or defends the apparent inconsistency when challenged by critics.',
    'If deliberate and systematic, the tangled_rope classification is strongly warranted — active enforcement via reputational borrowing is a real mechanism. If incidental and rare, the constraint may be closer to a minor scaffold-like lapse than a persistent extractive structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intentional_vs_incidental_inconsistency, empirical, 'Whether the gap between diagnosis and practice is strategic or incidental.').

omega_variable(
    emotivism_truth_value_stakes,
    'Does it matter, for evaluating the gap, whether emotivism is actually TRUE or FALSE as a general metaethical theory — i.e., is the constraint''s severity independent of the underlying metaethical question?',
    'Philosophical analysis of whether the charge of inconsistency survives under either metaethical answer: if emotivism is true, the author''s shaming language is simply what all moral language does (no special fault, but the anti-emotivist argument itself fails); if emotivism is false, the author owes an argument not supplied.',
    'Under either resolution the author''s specific practice is not vindicated, but the framing of WHY it fails differs — this affects whether the constraint is best described as inconsistency (false metaethics, unmet standard) or as unwitting confirmation (true metaethics, standard doesn''t apply to anyone).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(emotivism_truth_value_stakes, conceptual, 'Whether the constraint''s severity depends on the actual truth of emotivism.').

omega_variable(
    downstream_authority_vacuum_dependency,
    'Would this practice/diagnosis gap be correctable if the upstream authority vacuum (no adjudicating body for competing metaethical frameworks) were resolved — or is the gap independent of that vacuum, arising purely from individual rhetorical incentive?',
    'Compare cases where a strong disciplinary consensus DOES exist (e.g., within a tight analytic philosophy subfield with active peer review) — does the same author-practice gap persist or get corrected faster under stronger adjudicating authority?',
    'If the gap is fully explained by the vacuum, resolving authority_vacuum_incommensurability would substantially resolve this constraint too, strengthening the network link. If the gap is independent, this constraint has its own persistence mechanism separate from its upstream constraint.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(downstream_authority_vacuum_dependency, empirical, 'Whether resolving the upstream authority vacuum would resolve this downstream inconsistency.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(emotivism_as_diagnosis_vs_practice, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(emot_tr_t0, emotivism_as_diagnosis_vs_practice, theater_ratio, 0, 0.3).
narrative_ontology:measurement(emot_tr_t4, emotivism_as_diagnosis_vs_practice, theater_ratio, 4, 0.38).
narrative_ontology:measurement(emot_tr_t8, emotivism_as_diagnosis_vs_practice, theater_ratio, 8, 0.44).
narrative_ontology:measurement(emot_tr_t12, emotivism_as_diagnosis_vs_practice, theater_ratio, 12, 0.48).
narrative_ontology:measurement(emot_tr_t16, emotivism_as_diagnosis_vs_practice, theater_ratio, 16, 0.51).
narrative_ontology:measurement(emot_tr_t20, emotivism_as_diagnosis_vs_practice, theater_ratio, 20, 0.53).
narrative_ontology:measurement(emot_tr_t24, emotivism_as_diagnosis_vs_practice, theater_ratio, 24, 0.55).

% Extraction over time
narrative_ontology:measurement(emot_be_t0, emotivism_as_diagnosis_vs_practice, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(emot_be_t4, emotivism_as_diagnosis_vs_practice, base_extractiveness, 4, 0.42).
narrative_ontology:measurement(emot_be_t8, emotivism_as_diagnosis_vs_practice, base_extractiveness, 8, 0.5).
narrative_ontology:measurement(emot_be_t12, emotivism_as_diagnosis_vs_practice, base_extractiveness, 12, 0.55).
narrative_ontology:measurement(emot_be_t16, emotivism_as_diagnosis_vs_practice, base_extractiveness, 16, 0.58).
narrative_ontology:measurement(emot_be_t20, emotivism_as_diagnosis_vs_practice, base_extractiveness, 20, 0.6).
narrative_ontology:measurement(emot_be_t24, emotivism_as_diagnosis_vs_practice, base_extractiveness, 24, 0.62).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(emotivism_as_diagnosis_vs_practice, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(emotivism_as_diagnosis_vs_practice, identity_coordination).
narrative_ontology:affects_constraint(emotivism_as_diagnosis_vs_practice, authority_vacuum_incommensurability).

% DUAL FORMULATION NOTE:
% This constraint is downstream of authority_vacuum_incommensurability. The upstream constraint concerns the structural fact that, post-Enlightenment, no single authority adjudicates between competing moral frameworks (autonomy, universalizability, sanctity-of-life readings of the personhood_boundary_kernel, per MacIntyre). This downstream constraint concerns a specific consequence enabled by that vacuum: an individual arguer can diagnose emotivism as a general failure mode of OTHERS' moral language while performing it themselves, because no adjudicating authority exists to hold the practice to the diagnosis's own standard. The upstream constraint is about the absence of a referee for competing frameworks; this constraint is about what an individual arguer can get away with in the referee's absence. ε differs sharply: the upstream constraint's extraction is diffuse and structural (interminable public disagreement), while this constraint's extraction is concentrated and rhetorical (borrowed authority for a specific arguer's unargued conclusions) — hence two separate stories rather than one.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
