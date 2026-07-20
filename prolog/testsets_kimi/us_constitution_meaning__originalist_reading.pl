% ============================================================================
% CONSTRAINT STORY: us_constitution_meaning__originalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_us_constitution_meaning__originalist_reading, []).

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
 *   constraint_id: us_constitution_meaning__originalist_reading
 *   human_readable: Originalist Reading: Constitutional Meaning Fixed at Ratification
 *   domain: constitutional law / legal theory / political philosophy
 *
 * SUMMARY:
 *   This constraint instantiates the originalist reading of constitutional
 *   meaning: the communicative content of the Constitution's text was fixed
 *   at the moment of ratification (and subsequent amendments), and judges are
 *   bound to apply that historical public meaning. The constraint coordinates
 *   by limiting judicial discretion and addressing the counter-majoritarian
 *   difficulty, but it also extracts from modern rights claimants whose
 *   constitutional arguments lack 18th-century or amendment-era historical
 *   support. The claim is tangled_rope because the coordination function
 *   (checking judicial overreach) and the extraction function (denying
 *   protection to historically unsupported claims) are inseparable and
 *   enforced through the same institutional structure.
 *
 * KEY AGENTS:
 *   - counter_majoritarian_constraint_advocates: Primary beneficiary (organized/mobile) â benefits from judicial constraint and policy outcomes aligned with originalist methodology
 *   - originalist_judiciary: Agenda setter (institutional/constrained) â enforces historical public meaning methodology while being bound by it
 *   - rights_claimants_without_historical_support: Primary target (powerless/trapped) â bear extraction through denied constitutional protections
 *   - living_constitutionalist_jurists: Excluded voice (organized/constrained) â structurally silenced in courts committed to originalism
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(us_constitution_meaning__originalist_reading, 0.68).
domain_priors:suppression_score(us_constitution_meaning__originalist_reading, 0.85).
domain_priors:theater_ratio(us_constitution_meaning__originalist_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(us_constitution_meaning__originalist_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(us_constitution_meaning__originalist_reading, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(us_constitution_meaning__originalist_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(us_constitution_meaning__originalist_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(us_constitution_meaning__originalist_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(us_constitution_meaning__originalist_reading, tangled_rope).
narrative_ontology:human_readable(us_constitution_meaning__originalist_reading, "Originalist Reading: Constitutional Meaning Fixed at Ratification").
narrative_ontology:topic_domain(us_constitution_meaning__originalist_reading, "constitutional law / legal theory / political philosophy").

domain_priors:requires_active_enforcement(us_constitution_meaning__originalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(us_constitution_meaning__originalist_reading, '56dbad8b-19d4-40ac-bd9d-f9a36d08c154').
narrative_ontology:cs_kernel_codification('56dbad8b-19d4-40ac-bd9d-f9a36d08c154', fixed_text).
narrative_ontology:cs_authority_grounding('56dbad8b-19d4-40ac-bd9d-f9a36d08c154', lineage).
narrative_ontology:cs_interpretation_layer_present('56dbad8b-19d4-40ac-bd9d-f9a36d08c154').
narrative_ontology:cs_reading_relation('56dbad8b-19d4-40ac-bd9d-f9a36d08c154', us_constitution_meaning__living_constitutionalist_reading, forecloses).
narrative_ontology:cs_reading_relation('56dbad8b-19d4-40ac-bd9d-f9a36d08c154', us_constitution_meaning__positivist_reading, coexists_with).
narrative_ontology:cs_axiom('56dbad8b-19d4-40ac-bd9d-f9a36d08c154', foundational, ratification_fixation_thesis).
narrative_ontology:cs_axiom_status(ratification_fixation_thesis, holdable).
narrative_ontology:cs_axiom_grounding('56dbad8b-19d4-40ac-bd9d-f9a36d08c154', ratification_fixation_thesis, conventional).
narrative_ontology:cs_axiom('56dbad8b-19d4-40ac-bd9d-f9a36d08c154', secondary, judicial_bound_by_historical_meaning).
narrative_ontology:cs_axiom_status(judicial_bound_by_historical_meaning, holdable).
narrative_ontology:cs_axiom_grounding('56dbad8b-19d4-40ac-bd9d-f9a36d08c154', judicial_bound_by_historical_meaning, deontological).
narrative_ontology:cs_reference_frame('56dbad8b-19d4-40ac-bd9d-f9a36d08c154', ratification_moment_public_meaning).
narrative_ontology:cs_drift_state('56dbad8b-19d4-40ac-bd9d-f9a36d08c154', contemporary_constitutional_practice, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('56dbad8b-19d4-40ac-bd9d-f9a36d08c154', '').
narrative_ontology:cs_kernel_id(us_constitution_meaning__originalist_reading, us_constitution_meaning).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(us_constitution_meaning__originalist_reading, counter_majoritarian_constraint_advocates).
narrative_ontology:constraint_victim(us_constitution_meaning__originalist_reading, rights_claimants_without_historical_support).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Promote originalism as the only legitimate interpretive methodology, gaining institutional influence and policy outcomes aligned with their preferences when courts adopt historical fixation. They collect political and jurisprudential victories when non-originalist rights claims fail for lack of historical support.
narrative_ontology:constraint_stakeholder(us_constitution_meaning__originalist_reading, counter_majoritarian_constraint_advocates, beneficiary,
    organized, generational, mobile, national).

% Interpret the Constitution by recovering the public meaning of its text at ratification or amendment dates. They enforce the constraint by issuing rulings that suppress non-originalist arguments and are themselves bound by the availability and quality of historical evidence.
narrative_ontology:constraint_stakeholder(us_constitution_meaning__originalist_reading, originalist_judiciary, agenda_setter,
    institutional, biographical, constrained, national).

% Seek constitutional protection for rights recognized by contemporary morality but unsupported by documentary evidence from 1788 or relevant amendment eras. Their claims fail not on the merits of justice but on the absence of historically demonstrable public meaning.
narrative_ontology:constraint_stakeholder(us_constitution_meaning__originalist_reading, rights_claimants_without_historical_support, payer,
    powerless, immediate, trapped, national).

% Argue that constitutional meaning legitimately evolves with social circumstances and contemporary values. Their methodology is treated as legally illegitimate in courts committed to originalism, structurally barring their interpretive framework from judicial outcomes.
narrative_ontology:constraint_stakeholder(us_constitution_meaning__originalist_reading, living_constitutionalist_jurists, excluded,
    organized, generational, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Prevents judicial overreach by tethering constitutional interpretation to a fixed historical public meaning, ensuring judges do not substitute contemporary moral preferences for democratically enacted constitutional text.
% TRANSFER_FUNCTION: Moves interpretive authority from contemporary moral reasoning and evolving social norms to historical evidence of 1788 and amendment-era public meaning; moves constitutional protection away from rights claimants who cannot demonstrate historical support and toward those who can.
% ABSENT_VOICES: Living constitutionalist jurists and rights claimants relying on evolving standards of dignity, privacy, or equality are structurally disadvantaged; their arguments are ruled legally illegitimate rather than merely unpersuasive in originalist courts.
% DISAPPEARANCE_RATIONALE: If the fixation thesis vanished, constitutional doctrine would reorganize around contemporary values, living tradition, or positivist proceduralism; rights claimants currently denied protection would gain standing; originalist scholarship and institutions would lose their anchor; the judiciary's relationship to democratic majorities would shift.
% FOUNDING_PROBLEM: The counter-majoritarian difficulty: unelected federal judges exercising broad interpretive discretion risk substituting their own moral and political views for those of democratically elected representatives, undermining democratic legitimacy.
% FOUNDING_PROBLEM_CORROBORATION: The counter-majoritarian difficulty is widely attested in constitutional theory (Bickel, Ely) from seats outside the immediate beneficiary set, though its severity and the appropriateness of originalism as a remedy are contested by critical legal theorists and living constitutionalist scholars who argue the problem is misdiagnosed.
narrative_ontology:disappearance_verdict(us_constitution_meaning__originalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(us_constitution_meaning__originalist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(us_constitution_meaning__originalist_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(us_constitution_meaning__originalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(us_constitution_meaning__originalist_reading, 0.68, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(us_constitution_meaning__originalist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(us_constitution_meaning__originalist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(us_constitution_meaning__originalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68) reflects the systematic transfer of interpretive authority to historical evidence and the corresponding denial of constitutional protection to contemporary rights claims that cannot demonstrate ratification-era support. Suppression (0.85) is high because the constraint's persistence depends on treating non-originalist interpretive methodologies as legally illegitimate in the courts that adopt it. Theater ratio (0.45) captures the mix of genuine historical inquiry and selective or performative invocation of history to justify predetermined outcomes. Accessibility collapse (0.60) is moderate because alternative interpretive methods persist in academia and some courts, though they are formally excluded in originalist jurisdictions. Resistance (0.55) reflects sustained scholarly and advocacy contestation from living constitutionalists and affected rights claimants.
 *
 * PERSPECTIVAL GAP:
 *   The originalist judiciary experiences the constraint as a source of methodological clarity and democratic legitimacy â they are constrained by law, not personal preference. Rights claimants experience the same structure as an arbitrary historical lottery that denies them protection based on documentary accidents. Counter-majoritarian advocates see it as a necessary bulwark against elitist judicial activism. The engine computes these divergent seat classifications from the same structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Counter-majoritarian constraint advocates are structural beneficiaries: the constraint advances their institutional and political agenda, yielding low directionality. Rights claimants without historical support are structural targets: the constraint extracts constitutional protection from them, yielding high directionality. The originalist judiciary sits near the middle â they gain professional identity and methodological discipline but surrender interpretive discretion, placing them closer to symmetric than to either pole.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem â the counter-majoritarian difficulty â remains a perennial feature of constitutional democracy, so the mandate has not clearly outlived its function. However, the specific mechanism (fixation on 1788 historical meaning) is contested as either a genuine solution or a new form of judicial activism dressed in historical rhetoric. The mismatch between a live founding problem and contested persistence mechanism is characteristic of tangled_rope: the coordination justification is real, but the extraction layered onto it is disputed.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contingency,
    'Is this constraint a genuine feature of legal metaphysics, or is it one contingent reading of a kernel that could be replaced by living constitutionalist or positivist readings with no loss of institutional coherence?',
    'Comparative institutional analysis of jurisdictions or courts that have abandoned originalism without collapsing constitutional legitimacy; identification of whether the constraint''s effects persist under alternative readings.',
    'If alternative readings produce equivalent or superior constitutional coordination without the same extraction from modern rights claimants, the originalist reading is revealed as a contingent snare or tangled rope rather than a necessary enforcement mechanism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contingency, conceptual, 'Whether the originalist reading is a necessary or contingent instantiation of constitutional meaning.').

omega_variable(
    historical_meaning_recoverability,
    'Is the historical public meaning of 1787-1788 texts empirically recoverable with sufficient precision to genuinely constrain judicial discretion, or does the method collapse into judges'' disguised contemporary preferences?',
    'Corpus linguistics studies, archival deep-dives, and historiographical review of originalist opinions to measure the gap between claimed historical meaning and available evidence; testing whether different originalist judges converge on the same historical meaning.',
    'If historical meaning is irrecoverable or indeterminate, the constraint''s coordination claim fails and the structure functions primarily as extraction by another name; if recoverable and convergent, the coordination claim is strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(historical_meaning_recoverability, empirical, 'Whether historical public meaning is sufficiently determinate to perform the coordination work claimed.').

omega_variable(
    originalism_as_cover,
    'Does originalist methodology genuinely constrain judges, or does selective invocation of fragmentary historical evidence function as a rhetorical layer for conservative political outcomes?',
    'Outcome-mapping studies comparing originalist judicial votes with the political preferences of appointing presidents; analysis of originalist opinions in cases where historical evidence is thin or contradictory.',
    'If originalism is primarily cover, the constraint''s theater_ratio would need upward revision and its claimed coordination function would be demoted; if genuine, the tangled_rope classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(originalism_as_cover, empirical, 'Whether originalism operates as genuine methodological constraint or as performative cover for politically preferred outcomes.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(us_constitution_meaning__originalist_reading, 0, 44).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(us_c_tr_t0, us_constitution_meaning__originalist_reading, theater_ratio, 0, 0.4).
narrative_ontology:measurement(us_c_tr_t8, us_constitution_meaning__originalist_reading, theater_ratio, 8, 0.38).
narrative_ontology:measurement(us_c_tr_t16, us_constitution_meaning__originalist_reading, theater_ratio, 16, 0.35).
narrative_ontology:measurement(us_c_tr_t24, us_constitution_meaning__originalist_reading, theater_ratio, 24, 0.4).
narrative_ontology:measurement(us_c_tr_t32, us_constitution_meaning__originalist_reading, theater_ratio, 32, 0.43).
narrative_ontology:measurement(us_c_tr_t40, us_constitution_meaning__originalist_reading, theater_ratio, 40, 0.45).
narrative_ontology:measurement(us_c_tr_t44, us_constitution_meaning__originalist_reading, theater_ratio, 44, 0.45).

% Extraction over time
narrative_ontology:measurement(us_c_be_t0, us_constitution_meaning__originalist_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(us_c_be_t8, us_constitution_meaning__originalist_reading, base_extractiveness, 8, 0.38).
narrative_ontology:measurement(us_c_be_t16, us_constitution_meaning__originalist_reading, base_extractiveness, 16, 0.5).
narrative_ontology:measurement(us_c_be_t24, us_constitution_meaning__originalist_reading, base_extractiveness, 24, 0.6).
narrative_ontology:measurement(us_c_be_t32, us_constitution_meaning__originalist_reading, base_extractiveness, 32, 0.64).
narrative_ontology:measurement(us_c_be_t40, us_constitution_meaning__originalist_reading, base_extractiveness, 40, 0.67).
narrative_ontology:measurement(us_c_be_t44, us_constitution_meaning__originalist_reading, base_extractiveness, 44, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(us_c_su_t0, us_constitution_meaning__originalist_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(us_c_su_t8, us_constitution_meaning__originalist_reading, suppression_requirement, 8, 0.55).
narrative_ontology:measurement(us_c_su_t16, us_constitution_meaning__originalist_reading, suppression_requirement, 16, 0.65).
narrative_ontology:measurement(us_c_su_t24, us_constitution_meaning__originalist_reading, suppression_requirement, 24, 0.75).
narrative_ontology:measurement(us_c_su_t32, us_constitution_meaning__originalist_reading, suppression_requirement, 32, 0.8).
narrative_ontology:measurement(us_c_su_t40, us_constitution_meaning__originalist_reading, suppression_requirement, 40, 0.84).
narrative_ontology:measurement(us_c_su_t44, us_constitution_meaning__originalist_reading, suppression_requirement, 44, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(us_constitution_meaning__originalist_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(us_constitution_meaning__originalist_reading, living_constitutionalist_reading).
narrative_ontology:affects_constraint(us_constitution_meaning__originalist_reading, positivist_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the kernel us_constitution_meaning, decomposed per the Îµ-invariance principle because the natural-language label 'constitutional meaning' conflates structurally distinct claims: originalist fixation on ratification-meaning, living constitutionalist evolution with social attitudes, and positivist procedural validity. Each reading carries a distinct Îµ, stakeholder structure, and classification.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
