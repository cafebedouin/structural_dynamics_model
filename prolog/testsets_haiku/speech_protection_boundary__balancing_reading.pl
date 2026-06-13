% ============================================================================
% CONSTRAINT STORY: speech_protection_boundary__balancing_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
 *   constraint_id: speech_protection_boundary__balancing_reading
 *   human_readable: First Amendment Balancing Test — Context-Dependent Speech Protection
 *   domain: constitutional/political
 *
 * SUMMARY:
 *   The balancing test for First Amendment protection emerged from
 *   mid-20th-century constitutional practice as courts recognized that speech
 *   protection and other constitutional values (equality, dignity, fair
 *   procedure) sometimes conflict genuinely, not through bad-faith
 *   suppression but through competing legitimate interests. Under this
 *   reading, the boundary between protected and unprotected speech shifts
 *   with context: courts weigh speaker interests (expression value, speaker
 *   autonomy), listener interests (dignity, equal access, freedom from
 *   harassment), state interests (preventing demonstrable harms, maintaining
 *   public order), and the availability of narrower alternatives. The reading
 *   claims to coordinate plural constitutional values; critics argue it
 *   creates incoherence and chilling effects because speakers cannot predict
 *   in advance whether their speech is protected. The claim/metric gap is
 *   intentional: the constraint is CLAIMED as tangled_rope (coordination +
 *   asymmetric extraction) while its precise extractiveness is contested
 *   across the stakeholder seats — the engine measures where the Court's
 *   institutional position produces net extraction despite coordination
 *   function.
 *
 * KEY AGENTS:
 *   - Federal Judiciary (institutional agenda-setter, defines the balancing test and its application)
 *   - Vulnerable populations seeking harm limitation (beneficiaries of harm-based limitations, identity_locked in exposure to speech)
 *   - Speakers with marginalized perspectives (payers, face uncertain protection, constrained exit)
 *   - Institutional speakers (dual role: some benefit from institutional stability weighting, some pay through reputational/regulatory asymmetry)
 *   - Civil libertarian advocates (organized payers, argue for categorical rules, arbitrage exit through appellate strategy)
 *   - Equal dignity advocates (organized beneficiaries, argue for recognition of harm interests, arbitrage exit through litigation and legislation)
 *   - Lower court judges (payers, bear decision uncertainty and reversal risk)
 *   - Comparative law observers (analytical seat, external vantage on whether balancing is structurally necessary)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(speech_protection_boundary__balancing_reading, 0.62).
domain_priors:suppression_score(speech_protection_boundary__balancing_reading, 0.48).
domain_priors:theater_ratio(speech_protection_boundary__balancing_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(speech_protection_boundary__balancing_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(speech_protection_boundary__balancing_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(speech_protection_boundary__balancing_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(speech_protection_boundary__balancing_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(speech_protection_boundary__balancing_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(speech_protection_boundary__balancing_reading, tangled_rope).
narrative_ontology:human_readable(speech_protection_boundary__balancing_reading, "First Amendment Balancing Test — Context-Dependent Speech Protection").
narrative_ontology:topic_domain(speech_protection_boundary__balancing_reading, "constitutional/political").

domain_priors:requires_active_enforcement(speech_protection_boundary__balancing_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(speech_protection_boundary__balancing_reading, 'cf593c64-a227-47e8-9bef-8f1cb560eaae').
narrative_ontology:cs_kernel_codification('cf593c64-a227-47e8-9bef-8f1cb560eaae', fixed_text).
narrative_ontology:cs_authority_grounding('cf593c64-a227-47e8-9bef-8f1cb560eaae', lineage).
narrative_ontology:cs_interpretation_layer_present('cf593c64-a227-47e8-9bef-8f1cb560eaae').
narrative_ontology:cs_reading_relation('cf593c64-a227-47e8-9bef-8f1cb560eaae', speech_protection_boundary__absolutist_reading, coexists_with).
narrative_ontology:cs_reading_relation('cf593c64-a227-47e8-9bef-8f1cb560eaae', speech_protection_boundary__harm_limited_reading, influences).
narrative_ontology:cs_axiom('cf593c64-a227-47e8-9bef-8f1cb560eaae', foundational, plural_constitutional_values).
narrative_ontology:cs_axiom_status(plural_constitutional_values, holdable).
narrative_ontology:cs_axiom_grounding('cf593c64-a227-47e8-9bef-8f1cb560eaae', plural_constitutional_values, deontological).
narrative_ontology:cs_axiom('cf593c64-a227-47e8-9bef-8f1cb560eaae', foundational, context_dependent_protection_boundary).
narrative_ontology:cs_axiom_status(context_dependent_protection_boundary, holdable).
narrative_ontology:cs_axiom_grounding('cf593c64-a227-47e8-9bef-8f1cb560eaae', context_dependent_protection_boundary, deontological).
narrative_ontology:cs_reference_frame('cf593c64-a227-47e8-9bef-8f1cb560eaae', post_civil_rights_pluralist_authority).
narrative_ontology:cs_drift_state('cf593c64-a227-47e8-9bef-8f1cb560eaae', contemporary_institutional_capture_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('cf593c64-a227-47e8-9bef-8f1cb560eaae', '').
narrative_ontology:cs_kernel_id(speech_protection_boundary__balancing_reading, speech_protection_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(speech_protection_boundary__balancing_reading, judiciary_gatekeeper_role).
narrative_ontology:constraint_beneficiary(speech_protection_boundary__balancing_reading, vulnerable_populations_seeking_harm_limitation).
narrative_ontology:constraint_victim(speech_protection_boundary__balancing_reading, speakers_with_marginalized_perspectives).
narrative_ontology:constraint_victim(speech_protection_boundary__balancing_reading, institutional_speakers_facing_asymmetric_scrutiny).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(speech_protection_boundary__balancing_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(speech_protection_boundary__balancing_reading, 'none', 1).

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
 *   Extractiveness measures how much this framework redistributes power toward courts and away from speakers who could rely on rules. Starting at 0.45 (the framework permits genuine flexibility for plural values) and rising to 0.62 (as courts apply it, marginal speakers lose predictability and judicial discretion increases), with plateauing at 0.62 because the balancing framework has stabilized as doctrine — no further ratchet of judicial power without doctrinal shift. Suppression measures how much active judicial gatekeeping is needed: rising from 0.38 to 0.48 and plateauing, indicating stable enforcement machinery (lower courts must apply the test, appellate review enforces consistency). Theater_ratio rising from 0.22 to 0.41 reflects increasing symbolic use of the 'balancing' language in cases that reach predetermined outcomes — the operative gateway is not the balancing but how courts weigh values, and describing it as balance becomes theatrical cover. All three metrics share one time grid so temporal coherence is maintained. The constraint meets the tangled_rope requirement: beneficiaries exist (vulnerable populations benefit from openness to harm-based limitations, judiciary benefits from gatekeeping role), victims exist (marginal speakers, some institutional speakers), and active enforcement is required (courts must conduct the test, appellate review enforces consistency, lower court reversals police deviation from standard).
 *
 * PERSPECTIVAL GAP:
 *   The payer seats (marginal speakers, lower court judges) should compute to higher extractiveness and suppression than the beneficiary seats (vulnerable populations, judiciary). Marginal speakers experience the framework as a loss of predictability; federal judges experience it as authoritative gatekeeping; lower court judges experience it as constraint and uncertainty. The engine computes this divergence from the structural roles and exit options — the authored metrics are the story-level average across all seats, which masks the seat-specific variation the engine is designed to reveal.
 *
 * DIRECTIONALITY LOGIC:
 *   Judiciary is the agenda-setter (role 'agenda_setter', power 'institutional', exit 'analytical'): they set and enforce the balancing test, so d derives from their beneficiary status — the test centralizes speech-protection gatekeeping in courts. Judiciary benefits from the arrangement without bearing extraction costs (d near 0.2). Vulnerable populations are beneficiaries (role 'beneficiary', power 'moderate', exit 'trapped'): they benefit from openness to harm-based limitations but cannot exit exposure to speech, making them partly dependent on courts for remedies (d near 0.3). Speakers with marginalized perspectives are payers (role 'payer', power 'moderate', exit 'constrained'): they face uncertain protection and cannot easily exit — if their speech is characterized as harmful, the framework provides no categorical shelter (d near 0.75). Institutional speakers are dual-positioned (role 'payer' + secondary 'beneficiary'): institutional stability is valued in balancing, but institutional speakers also face reputational and regulatory costs from harm-based litigation (d around 0.5, or override to 0.55 if asymmetry is empirically confirmed). Civil libertarian advocates are payers with arbitrage exit (role 'payer', power 'organized', exit 'arbitrage'): they pay by operating under uncertainty but can litigate for doctrinal change (d near 0.4). Equal dignity advocates are beneficiaries with arbitrage exit (role 'beneficiary', power 'organized', exit 'arbitrage'): they benefit from the framework but can choose litigation strategies (d near 0.25). Lower court judges are payers (role 'payer', power 'institutional', exit 'constrained'): they must apply the test without categorical guidance and bear reversal risk (d near 0.70).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (post-civil rights era: categorical absolutism leaves vulnerable populations with no constitutional recourse) is contested in its current status. Equal dignity advocates attest the problem is live — speech protections still serve dominant groups. Civil libertarian scholars attest the problem is obsolete — the balancing framework creates more suppression than it solves. This contestation is structural to the reading itself: the balancing framework was built to resolve the tension between absolutist categorical rules and recognition of equal dignity interests, but it has not resolved the tension — it has institutionalized it in judicial gatekeeping. The disappearance verdict is 'world_rearranges': if the balancing framework vanished, the U.S. would revert to categorical rules (absolutist or narrow-exception), and litigation strategy, regulatory approaches, and vulnerable-population access to courts would reorganize. The founding_problem_status x disappearance_verdict mismatch (contested/world_rearranges) suggests the framework is mandatrophic: built for a live problem (plural constitutional values) that remains live (not resolved by case-by-case determination, only managed by gatekeeping), yet the framework persists because it gives courts institutional authority while claiming to solve the underlying conflict. This is not a dead mandate kept alive by inertia (piton), but a contested mandate that both sides use for legitimacy — equal dignity advocates use it to argue for harm recognition, civil libertarians use it to argue for categorical rules, and the judiciary uses it to justify institutional power. The mandatrophy is structural, not theatrical.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    balancing_incoherence_vs_necessity,
    'Does the case-by-case balancing framework produce incoherence (chilling effects, unpredictability, bias in judicial application) because it lacks categorical guidance, or is the framework necessary precisely because speech/dignity conflicts are genuinely multivalent and cannot be resolved by categorical rule?',
    'Longitudinal study of speech patterns and litigation before/after shifts to categorical rules (jurisdictions adopting absolutist or narrow-exception regimes); empirical analysis of appellate reversals under balancing standard to measure inconsistency; comparative constitutional analysis of regimes using categorical bans versus balancing.',
    'If incoherence is the problem, the constraint should be reclassified as performative (theater_ratio rises, suppression does not track enforcement necessity). If necessity is the answer, the framework is genuinely coordinating plural values and theater_ratio would be lower. The reading''s classification depends on this factual question.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(balancing_incoherence_vs_necessity, empirical, 'Whether the balancing framework''s uncertainty is a bug (incoherence) or a feature (necessary flexibility for plural values).').

omega_variable(
    marginal_voice_chilling_effect,
    'Do speakers with marginalized perspectives experience higher chilling effects under the balancing framework than under categorical absolutism, because their speech is more likely to be characterized as harmful by institutional gatekeepers?',
    'Comparative analysis of self-censorship rates for marginalized speakers across jurisdictions with different speech standards; litigation data on asymmetric scrutiny by speaker type; interviews with advocates for marginalized groups on strategic communication choices.',
    'High chilling effect on marginalized speakers would establish the framework as primarily extractive (suppression rises asymmetrically), supporting a snare classification and reclassification of victims. Low asymmetric chilling would support the framework''s coordination narrative — equal access to the balancing test regardless of viewpoint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(marginal_voice_chilling_effect, empirical, 'Whether the balancing standard distributes its gatekeeping burden equally or asymmetrically.').

omega_variable(
    kernel_reading_boundary_stability,
    'Is this reading (balancing across case-by-case contexts) logically and materially distinct from the harm_limited_reading (speech conditional on absence of significant harm), or do they collapse into each other once applied to concrete cases?',
    'Comparative analysis of judicial outcomes in cases decided under explicit balancing language versus explicit harm-based language; whether courts produce systematically different results or converge on the same protective boundaries; examination of whether the reading labels predict doctrinal direction.',
    'If they collapse empirically, the readings are not genuinely distinct constraints — the kernel decomposition fails and the constraint family must be re-authored. If they diverge, the balancing reading is defined by its proceduralism (case-by-case weight), distinct from the harm_limited reading''s substantive focus on harm elimination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_boundary_stability, empirical, 'Whether the balancing reading remains distinct from the harm_limited reading under application, or whether they are functionally equivalent.').

omega_variable(
    institutional_speaker_asymmetry,
    'Are institutional speakers systematically disadvantaged under the balancing framework because courts weigh institutional interests (stability, efficiency) less heavily than individual speaker interests or vulnerable-population interests, creating a side-effect extraction mechanism?',
    'Litigation data on reversal rates for cases involving institutional speakers versus individual speakers; empirical analysis of how courts balance institutional versus individual interests; comparative study of outcomes when the same speech type is uttered by individual versus institutional speakers.',
    'If institutional speakers face systematically lower protection, the framework extracts from them (institutional_speakers bears higher effective extraction despite some beneficiary roles) and this should be reflected in the directionality override. If balancing treats institutional and individual speakers symmetrically, the current directionality assignment is accurate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_speaker_asymmetry, empirical, 'Whether the balancing framework systemically disadvantages institutional speakers despite their power.').

omega_variable(
    reading_vs_absolutist_foreclosure,
    'Does the balancing reading logically foreclose the absolutist reading (speech protection near-absolute except imminent lawless action), or do they coexist as different judicial commitments held by different court coalitions?',
    'Constitutional text analysis: whether text permits both readings or requires choosing one; historical practice under each reading; whether Supreme Court has issued holdings that logically rule out the absolutist premise while affirming balancing, or whether different Court eras simply apply different doctrines without foreclosure.',
    'If balancing forecloses absolutism, the reading_relations entry for absolutist_reading should be ''forecloses''. If they coexist as different judicial commitments without logical contradiction, the entry should be ''coexists_with''. This affects the cs_structure authority dynamics — whether the kernel can sustain both readings simultaneously or whether institutional authority must choose.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_vs_absolutist_foreclosure, conceptual, 'Whether the balancing reading logically forecloses or coexists with the absolutist reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(speech_protection_boundary__balancing_reading, 0, 35).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(spee_tr_t0, speech_protection_boundary__balancing_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement(spee_tr_t5, speech_protection_boundary__balancing_reading, theater_ratio, 5, 0.26).
narrative_ontology:measurement(spee_tr_t10, speech_protection_boundary__balancing_reading, theater_ratio, 10, 0.31).
narrative_ontology:measurement(spee_tr_t15, speech_protection_boundary__balancing_reading, theater_ratio, 15, 0.36).
narrative_ontology:measurement(spee_tr_t20, speech_protection_boundary__balancing_reading, theater_ratio, 20, 0.39).
narrative_ontology:measurement(spee_tr_t25, speech_protection_boundary__balancing_reading, theater_ratio, 25, 0.41).
narrative_ontology:measurement(spee_tr_t30, speech_protection_boundary__balancing_reading, theater_ratio, 30, 0.41).
narrative_ontology:measurement(spee_tr_t35, speech_protection_boundary__balancing_reading, theater_ratio, 35, 0.41).

% Extraction over time
narrative_ontology:measurement(spee_be_t0, speech_protection_boundary__balancing_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(spee_be_t5, speech_protection_boundary__balancing_reading, base_extractiveness, 5, 0.51).
narrative_ontology:measurement(spee_be_t10, speech_protection_boundary__balancing_reading, base_extractiveness, 10, 0.56).
narrative_ontology:measurement(spee_be_t15, speech_protection_boundary__balancing_reading, base_extractiveness, 15, 0.59).
narrative_ontology:measurement(spee_be_t20, speech_protection_boundary__balancing_reading, base_extractiveness, 20, 0.61).
narrative_ontology:measurement(spee_be_t25, speech_protection_boundary__balancing_reading, base_extractiveness, 25, 0.62).
narrative_ontology:measurement(spee_be_t30, speech_protection_boundary__balancing_reading, base_extractiveness, 30, 0.62).
narrative_ontology:measurement(spee_be_t35, speech_protection_boundary__balancing_reading, base_extractiveness, 35, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(spee_su_t0, speech_protection_boundary__balancing_reading, suppression_requirement, 0, 0.38).
narrative_ontology:measurement(spee_su_t5, speech_protection_boundary__balancing_reading, suppression_requirement, 5, 0.41).
narrative_ontology:measurement(spee_su_t10, speech_protection_boundary__balancing_reading, suppression_requirement, 10, 0.44).
narrative_ontology:measurement(spee_su_t15, speech_protection_boundary__balancing_reading, suppression_requirement, 15, 0.46).
narrative_ontology:measurement(spee_su_t20, speech_protection_boundary__balancing_reading, suppression_requirement, 20, 0.47).
narrative_ontology:measurement(spee_su_t25, speech_protection_boundary__balancing_reading, suppression_requirement, 25, 0.48).
narrative_ontology:measurement(spee_su_t30, speech_protection_boundary__balancing_reading, suppression_requirement, 30, 0.48).
narrative_ontology:measurement(spee_su_t35, speech_protection_boundary__balancing_reading, suppression_requirement, 35, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(speech_protection_boundary__balancing_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(speech_protection_boundary__balancing_reading, 0.18).
narrative_ontology:affects_constraint(speech_protection_boundary__balancing_reading, speech_protection_boundary__absolutist_reading).
narrative_ontology:affects_constraint(speech_protection_boundary__balancing_reading, speech_protection_boundary__harm_limited_reading).

% DUAL FORMULATION NOTE:
% The speech_protection_boundary kernel has three readings, each a distinct constraint with its own ε and structural dynamics. The balancing reading (this constraint) is procedural: it specifies how the boundary is determined (case-by-case weighing) rather than where the boundary sits. The absolutist reading specifies that the boundary is near-absolute (Brandenburg standard); the harm-limited reading specifies that the boundary is conditional on absence of significant harm. All three readings interpret the same constitutional text (First Amendment) but instantiate different constraints with different beneficiary/victim structures and different extraction profiles. They are linked by network.affects_constraints because changes to one reading affect litigation strategy and institutional authority in the others. The balancing reading influences both siblings: it is the dominant institutional practice (most U.S. courts currently apply some form of balancing), so absolutists and harm-limiters must work within or against balancing doctrine.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(speech_protection_boundary__balancing_reading, institutional, 0.2).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
