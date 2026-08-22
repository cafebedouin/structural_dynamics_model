% ============================================================================
% CONSTRAINT STORY: speech_protection_kernel__harm_threshold_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_speech_protection_kernel__harm_threshold_reading, []).

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
 *   constraint_id: speech_protection_kernel__harm_threshold_reading
 *   human_readable: Speech Protection Conditional on Harm Threshold
 *   domain: constitutional/political
 *
 * SUMMARY:
 *   This constraint instantiates the harm-threshold reading of the
 *   speech-protection kernel: speech protection operates conditionally, with
 *   victim harm claims taking priority over speaker autonomy when
 *   demonstrable harm can be shown. The reading sits between absolutism (no
 *   harm threshold) and dignity/subordination readings (structural effects
 *   matter more than demonstrable individual harm). Under this reading,
 *   courts and protective institutions adjudicate whether harm has occurred,
 *   and when it has, speech restriction follows. The constraint is presented
 *   as protecting vulnerability; the tension it creates is that speakers face
 *   ex ante uncertainty about what speech crosses the threshold, and
 *   institutions wield power to determine the boundary. The claim asserts
 *   this is Tangled Rope (genuine coordination between victim protection and
 *   speaker autonomy, plus asymmetric extraction through institutional
 *   gatekeeping); the metrics describe high suppression and moderate-to-high
 *   extraction, reflecting the institutional power to restrict speech and the
 *   burden it places on speakers.
 *
 * KEY AGENTS:
 *   - harm_victims: powerless, trapped exit — benefit from protective authority but cannot control how the constraint is applied
 *   - speakers_in_contested_domains: moderate power, constrained exit — bear the cost of navigating the threshold and uncertainty about what is prohibited
 *   - protective_state_institutions: institutional power, analytical exit — set and enforce the threshold, benefit from expanded authority
 *   - political_dissidents: organized, constrained exit — asymmetrically vulnerable to weaponized harm claims despite nominal protection
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(speech_protection_kernel__harm_threshold_reading, 0.68).
domain_priors:suppression_score(speech_protection_kernel__harm_threshold_reading, 0.72).
domain_priors:theater_ratio(speech_protection_kernel__harm_threshold_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(speech_protection_kernel__harm_threshold_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(speech_protection_kernel__harm_threshold_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(speech_protection_kernel__harm_threshold_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(speech_protection_kernel__harm_threshold_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(speech_protection_kernel__harm_threshold_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(speech_protection_kernel__harm_threshold_reading, tangled_rope).
narrative_ontology:human_readable(speech_protection_kernel__harm_threshold_reading, "Speech Protection Conditional on Harm Threshold").
narrative_ontology:topic_domain(speech_protection_kernel__harm_threshold_reading, "constitutional/political").

domain_priors:requires_active_enforcement(speech_protection_kernel__harm_threshold_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(speech_protection_kernel__harm_threshold_reading, '1b816b11-a6f1-4757-b867-1d73715bc352').
narrative_ontology:cs_kernel_codification('1b816b11-a6f1-4757-b867-1d73715bc352', fixed_text).
narrative_ontology:cs_authority_grounding('1b816b11-a6f1-4757-b867-1d73715bc352', lineage).
narrative_ontology:cs_interpretation_layer_present('1b816b11-a6f1-4757-b867-1d73715bc352').
narrative_ontology:cs_reading_relation('1b816b11-a6f1-4757-b867-1d73715bc352', speech_protection_kernel__absolutist_reading, forecloses).
narrative_ontology:cs_reading_relation('1b816b11-a6f1-4757-b867-1d73715bc352', speech_protection_kernel__marketplace_reading, coexists_with).
narrative_ontology:cs_reading_relation('1b816b11-a6f1-4757-b867-1d73715bc352', speech_protection_kernel__dignity_reading, influences).
narrative_ontology:cs_reading_relation('1b816b11-a6f1-4757-b867-1d73715bc352', speech_protection_kernel__democratic_participation_reading, coexists_with).
narrative_ontology:cs_axiom('1b816b11-a6f1-4757-b867-1d73715bc352', foundational, victim_harm_threshold_overrides_speaker_autonomy).
narrative_ontology:cs_axiom_status(victim_harm_threshold_overrides_speaker_autonomy, holdable).
narrative_ontology:cs_axiom_grounding('1b816b11-a6f1-4757-b867-1d73715bc352', victim_harm_threshold_overrides_speaker_autonomy, deontological).
narrative_ontology:cs_axiom('1b816b11-a6f1-4757-b867-1d73715bc352', secondary, demonstrable_harm_is_adjudicable).
narrative_ontology:cs_axiom_status(demonstrable_harm_is_adjudicable, holdable).
narrative_ontology:cs_axiom_grounding('1b816b11-a6f1-4757-b867-1d73715bc352', demonstrable_harm_is_adjudicable, empirically_contingent).
narrative_ontology:cs_reference_frame('1b816b11-a6f1-4757-b867-1d73715bc352', speech_protection_with_victim_harm_limit).
narrative_ontology:cs_drift_state('1b816b11-a6f1-4757-b867-1d73715bc352', contemporary_institutional_practice, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('1b816b11-a6f1-4757-b867-1d73715bc352', '').
narrative_ontology:cs_kernel_id(speech_protection_kernel__harm_threshold_reading, speech_protection_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(speech_protection_kernel__harm_threshold_reading, harm_victims).
narrative_ontology:constraint_beneficiary(speech_protection_kernel__harm_threshold_reading, protective_state_institutions).
narrative_ontology:constraint_victim(speech_protection_kernel__harm_threshold_reading, speakers_in_contested_domains).
narrative_ontology:constraint_victim(speech_protection_kernel__harm_threshold_reading, political_dissidents_facing_harm_claims).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Individuals or groups whose harm is claimed by others on their behalf. The harm-threshold reading protects them by placing victim status above speaker autonomy when demonstrable harm can be shown. They collect protection through court intervention and speech restriction targeting speakers. Their structural position is that their vulnerability justifies restricting speech that targets them, even when speaker intent is not malicious.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__harm_threshold_reading, harm_victims, beneficiary,
    powerless, biographical, trapped, national).

% Speakers whose utterances fall into contested categories (criticism of protected groups, statements that could be construed as inciting harm, speech adjacent to domains marked as high-harm-risk). They bear the cost of navigating the harm threshold: speech must be vetted ex ante for victim harm potential, and the burden of proving harmlessness can fall on them. Exit options include self-censorship, moving to jurisdictions with different thresholds, or ceasing to speak in contested domains entirely.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__harm_threshold_reading, speakers_in_contested_domains, payer,
    moderate, biographical, constrained, national).

% Courts, legislatures, and administrative bodies that apply and enforce the harm threshold. They adjudicate whether demonstrable harm exists, set standards for what counts as harm, and determine which speech crosses the threshold. They benefit from this reading by gaining authority to intervene in speech disputes on behalf of vulnerable groups, expanding protective jurisdiction. The enforcement machinery—judicial review, administrative notice-and-takedown, prior restraint authority—depends on the threshold remaining operable.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__harm_threshold_reading, protective_state_institutions, agenda_setter,
    institutional, generational, analytical, national).

% Groups whose speech criticizing power structures or dominant groups is vulnerable to reframing as harmful. They face the unique risk that regime-aligned entities can weaponize harm claims against dissenting speech. The harm threshold is applicable to them just as to other speakers, but the asymmetry of power means harm accusations are more likely to result in their speech being restricted than the speech of those criticizing dissidents. Their exit is costly: silence or jurisdictional exit.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__harm_threshold_reading, political_dissidents_facing_harm_claims, payer,
    organized, biographical, constrained, national).

% Institutional actors who advocate for counter-speech as the remedy to harmful speech, arguing that more speech rather than restriction is the appropriate response. They are excluded from the harm-threshold reading's justification apparatus—their core claim (that truth emerges from unrestricted competition among speakers) is not the grounding for this constraint. They would argue the threshold pre-judges the marketplace outcome, preventing the discovery process from operating.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__harm_threshold_reading, marketplace_advocates, excluded,
    powerful, generational, mobile, national).

% Academic and legal scholars who study harm mechanisms in speech: psychological harm, subordination pathways, cumulative effects across utterances. They provide the empirical and conceptual frameworks courts and legislatures use to determine what counts as demonstrable harm and set the threshold. Their role is epistemic: they produce the knowledge the constraint's operation depends on for adjudication.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__harm_threshold_reading, vulnerability_scholars, observer,
    institutional, generational, analytical, national).

% Institutional actors who oppose any harm-based threshold on speech, arguing that categorical speaker autonomy is foundational. They contest the reading's core premise at the constitutional level. They are excluded from this constraint's legitimacy apparatus—they do not sit in the institutions that apply the threshold and their philosophical stance rejects the framework entirely.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__harm_threshold_reading, absolutist_advocates, excluded,
    powerful, generational, mobile, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(speech_protection_kernel__harm_threshold_reading, protective_state_institutions).
narrative_ontology:fixing_cost_class(speech_protection_kernel__harm_threshold_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Operates a boundary protecting both speaker autonomy and victim welfare: speakers may express themselves freely unless demonstrable harm to victims occurs, at which point victim protection takes priority. Solves the collective action problem of defining when harm justifies state intervention.
% TRANSFER_FUNCTION: Transfers authority over speech decisions from individual speakers to state institutions (courts, agencies); transfers protective standing from victims (who must prove harm individually) to state institutions (who adjudicate on behalf of protected classes and can issue restrictions preemptively based on category).
% ABSENT_VOICES: Absolutist constitutional scholars and jurists who reject any harm-based limit are excluded from this reading's justification—they contest the constraint's core premise. Speakers with asymmetric power (those whose speech is rarely subject to harm scrutiny) are also absent from the conversation, as is the structural asymmetry itself that causes dissidents to bear more restriction than mainstream speakers.
% DISAPPEARANCE_RATIONALE: The entire institutional apparatus for victim protection through speech restriction would collapse. Courts would lose jurisdiction; protective agencies would lose statutory authority; vulnerable groups would lose a primary tool. Speech would revert to categorical protection, and victims would need to pursue redress through non-speech channels (tort, criminal prosecution, civil rights on other grounds). The political and legal landscape would reorganize.
% FOUNDING_PROBLEM: Early speech-protection regimes treated speaker autonomy as near-absolute, leaving vulnerable groups unprotected against speech that harmed them—whether through incitement, defamation, sustained denigration, or structural subordination. The constraint was built to protect victims without abandoning speaker autonomy entirely.
% FOUNDING_PROBLEM_CORROBORATION: Harm-threshold advocates and protective-institution practitioners attest the problem is live—vulnerable groups continue to suffer harm and need institutional protection. Absolutist and marketplace advocates counter that harms are overstated or require non-speech remedies. Independent harm-measurement research (vulnerability scholars, neuroscience, empirical legal studies on speech effects) from outside the benefiting parties confirms harms occur, but contests whether the threshold is calibrated correctly.
narrative_ontology:disappearance_verdict(speech_protection_kernel__harm_threshold_reading, world_rearranges).
narrative_ontology:founding_problem_status(speech_protection_kernel__harm_threshold_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(speech_protection_kernel__harm_threshold_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(speech_protection_kernel__harm_threshold_reading, 'none', 1).
narrative_ontology:epsilon_provenance(speech_protection_kernel__harm_threshold_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(speech_protection_kernel__harm_threshold_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(speech_protection_kernel__harm_threshold_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(speech_protection_kernel__harm_threshold_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate-high (0.68) because the constraint grants institutional gatekeeping power: speakers must either self-censor preemptively or face post-hoc restriction. The measurement series shows initial low extraction (0.51) rising as institutional practice settles thresholds—this models the learning curve of the doctrine. Suppression (0.72) is high because the constraint requires active enforcement: courts must actively adjudicate harm, develop standards for what counts, and issue restrictions. Theater (0.41) is moderate because the harm-determination process involves genuine deliberation, but increasing reliance on formulaic harm categories (slurs, victim-group membership) reduces true case-by-case analysis. The temporal plateau at t=30 models the doctrine stabilizing once major case law settles the threshold. Accessibility_collapse (0.48) is moderate because speakers have some clarity (harm definitions eventually stabilize) but cannot fully predict which utterances cross the line, and exit options exist (jurisdictional migration, silence, alternative venues) even if costly. Resistance (0.71) is high because absolutist and marketplace advocates mount sustained constitutional challenges, dissidents deploy counter-speech and jurisdictional arbitrage, and speakers' rights organizations litigate boundary cases.
 *
 * PERSPECTIVAL GAP:
 *   From the protective-institution seat, the constraint is genuine Tangled Rope: it coordinates victim protection (Rope function) with speaker authority (the coordination problem), though institutions must actively enforce the boundary (Tangled_rope marker). From the victim seat, it is pure Rope: they gain coordination of their protection without running the system themselves. From the speaker in an uncontested domain (e.g., art, science, personal narrative), it is Rope: they benefit from legal clarity and rarely hit the threshold. From the politically dissident speaker's seat, the constraint computes as Snare: the same nominal threshold systematically targets their speech, and their organized power is insufficient to prevent restriction. The engine computes this divergence: the same structural d-values applied to dissidents and mainstream speakers yield different per-seat types because their exit options and power atoms differ. This divergence IS the measurement the constraint story exists to capture.
 *
 * DIRECTIONALITY LOGIC:
 *   Harm victims have low directionality (beneficiaries: d ≈ 0.2)—they gain protection but are powerless and cannot operate the constraint themselves; they depend on institutions to recognize and remedy harm. Protective institutions have near-zero directionality (beneficiaries: d ≈ 0.05)—they gain authority and budgetary justification but frame the expansion as serving public protection, not self-aggrandizement. Speakers in contested domains have high directionality (victims: d ≈ 0.75)—they face constraints on their autonomy, cannot easily exit, and bear suppression costs. Political dissidents are the crucial asymmetry seat (victims: d ≈ 0.85)—they face the same nominal threshold as other speakers, but the structural power differential means harm accusations are more likely to target their speech, making them the de facto primary targets. The divergence from the beneficiary seat is stark: harm victims experience the constraint as protective; dissidents experience it as a weapon.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint avoids mandatrophy so long as demonstrable harm remains a live evidentiary question in adjudication. If courts stop asking whether harm occurred (mandate dies) and simply presume harm from categories (speaker identity, topic), the constraint would zombify: the coordination function (victim protection + speaker autonomy coexisting at a boundary) would disappear, leaving only extraction (institutional gatekeeping over speech). The measurement series models the drift toward mandatrophy: theater_ratio rises slowly because institutional practice converges on category-based rather than case-based harm determinations. A formal workshop assessment would check whether harm adjudication has become ritualistic (checking boxes rather than examining evidence). The 0.41 theater_ratio at interval end suggests the constraint is approaching but has not yet fully zombified.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    harm_demonstrability_boundary,
    'What standard defines ''demonstrable harm''? Is the threshold objective (measurable psychological or material injury), subjective (victim''s reported experience), or categorical (membership in a protected group with known vulnerability)?',
    'Case-law development and empirical research on harm pathways. Jurisdictions that narrow harm definitions to objective measures vs. those that accept subjective/categorical measures will produce divergent type classifications over time.',
    'If courts migrate from subjective/categorical harm toward objective demonstration, extractiveness declines and the constraint becomes more Rope-like; if courts expand categorical harm (speech to/about protected groups presumed harmful), extractiveness rises and the constraint becomes more Snare-like. The threshold definition is the operational moving part.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(harm_demonstrability_boundary, empirical, 'Definition of demonstrable harm determines threshold width and extractiveness trajectory.').

omega_variable(
    asymmetric_targeting_suppression,
    'Is the suppression measured here structural (courts have formal legal authority) or internalized (speakers have learned to self-censor the harm categories)? And if internalized, does it persist after the institutional threat is removed?',
    'Post-exit suppression tracking: if speakers who move to absolute-protection jurisdictions continue avoiding contested speech, suppression is substantially internalized; if they revert to pre-constraint speech patterns, suppression is primarily structural.',
    'If internalized, the constraint''s true suppression is higher than measured and removal would not restore pre-constraint speech levels. If structural, removal would restore speaker autonomy relatively quickly. This affects piton/theater diagnosis: internalized suppression is harder to dislodge and indicates deeper institutional entrenchment.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(asymmetric_targeting_suppression, empirical, 'Structural vs. internalized suppression mechanism and persistence under removal.').

omega_variable(
    dissident_asymmetry_weaponization,
    'Is the constraint applied symmetrically to all speakers, or do protective institutions systematically target political dissidents and marginalized speakers while sparing dominant-group speakers from equal scrutiny?',
    'Comparative case analysis: comparing harm verdicts on similar utterances from politically aligned vs. disaligned speakers; studying institution backgrounds and political commitments of adjudicators; tracking complaint origins (who initiates harm claims, against whom).',
    'If systematic asymmetry exists, the constraint reclassifies as Snare for dissident speakers even though it is Tangled Rope for mainstream speakers—the per-seat divergence becomes the measurement. If application is symmetric, the constraint remains Tangled Rope across seats (with different directionalities but same type). This is the crux of the dissident-seat analysis.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(dissident_asymmetry_weaponization, empirical, 'Whether harm threshold is applied symmetrically or weaponized against political dissidents.').

omega_variable(
    kernel_reading_contest_framing,
    'This reading interprets the speech-protection kernel via a harm-threshold boundary. Is this boundary one legitimate reading of a stable kernel, or does it represent a fundamental reinterpretation that displaces other readings?',
    'Historical and textual analysis: comparing this reading''s founding premises to the kernel''s (constitutional text or founding doctrine) explicit commitments. If the kernel''s language is ambiguous on harm thresholds, the reading is legitimate; if the kernel explicitly rejects harm-based limits, the reading is a reinterpretation.',
    'If legitimate reading: the constraint operates within the kernel''s authority; if reinterpretation: the constraint represents doctrinal drift and may face constitutional challenge on the grounds that it exceeds the kernel''s scope. This determines whether the constraint is foundational or vulnerable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest_framing, conceptual, 'Whether harm-threshold is a legitimate kernel reading or a doctrinal reinterpretation that exceeds the kernel''s scope.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(speech_protection_kernel__harm_threshold_reading, 0, 35).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(spee_tr_t0, speech_protection_kernel__harm_threshold_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement(spee_tr_t5, speech_protection_kernel__harm_threshold_reading, theater_ratio, 5, 0.32).
narrative_ontology:measurement(spee_tr_t10, speech_protection_kernel__harm_threshold_reading, theater_ratio, 10, 0.36).
narrative_ontology:measurement(spee_tr_t15, speech_protection_kernel__harm_threshold_reading, theater_ratio, 15, 0.38).
narrative_ontology:measurement(spee_tr_t20, speech_protection_kernel__harm_threshold_reading, theater_ratio, 20, 0.39).
narrative_ontology:measurement(spee_tr_t25, speech_protection_kernel__harm_threshold_reading, theater_ratio, 25, 0.4).
narrative_ontology:measurement(spee_tr_t30, speech_protection_kernel__harm_threshold_reading, theater_ratio, 30, 0.41).
narrative_ontology:measurement(spee_tr_t35, speech_protection_kernel__harm_threshold_reading, theater_ratio, 35, 0.41).

% Extraction over time
narrative_ontology:measurement(spee_be_t0, speech_protection_kernel__harm_threshold_reading, base_extractiveness, 0, 0.51).
narrative_ontology:measurement(spee_be_t5, speech_protection_kernel__harm_threshold_reading, base_extractiveness, 5, 0.56).
narrative_ontology:measurement(spee_be_t10, speech_protection_kernel__harm_threshold_reading, base_extractiveness, 10, 0.61).
narrative_ontology:measurement(spee_be_t15, speech_protection_kernel__harm_threshold_reading, base_extractiveness, 15, 0.64).
narrative_ontology:measurement(spee_be_t20, speech_protection_kernel__harm_threshold_reading, base_extractiveness, 20, 0.66).
narrative_ontology:measurement(spee_be_t25, speech_protection_kernel__harm_threshold_reading, base_extractiveness, 25, 0.67).
narrative_ontology:measurement(spee_be_t30, speech_protection_kernel__harm_threshold_reading, base_extractiveness, 30, 0.68).
narrative_ontology:measurement(spee_be_t35, speech_protection_kernel__harm_threshold_reading, base_extractiveness, 35, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(spee_su_t0, speech_protection_kernel__harm_threshold_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement(spee_su_t5, speech_protection_kernel__harm_threshold_reading, suppression_requirement, 5, 0.62).
narrative_ontology:measurement(spee_su_t10, speech_protection_kernel__harm_threshold_reading, suppression_requirement, 10, 0.66).
narrative_ontology:measurement(spee_su_t15, speech_protection_kernel__harm_threshold_reading, suppression_requirement, 15, 0.69).
narrative_ontology:measurement(spee_su_t20, speech_protection_kernel__harm_threshold_reading, suppression_requirement, 20, 0.7).
narrative_ontology:measurement(spee_su_t25, speech_protection_kernel__harm_threshold_reading, suppression_requirement, 25, 0.71).
narrative_ontology:measurement(spee_su_t30, speech_protection_kernel__harm_threshold_reading, suppression_requirement, 30, 0.72).
narrative_ontology:measurement(spee_su_t35, speech_protection_kernel__harm_threshold_reading, suppression_requirement, 35, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(speech_protection_kernel__harm_threshold_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(speech_protection_kernel__harm_threshold_reading, 0.12).
narrative_ontology:affects_constraint(speech_protection_kernel__harm_threshold_reading, speech_protection_kernel__absolutist_reading).
narrative_ontology:affects_constraint(speech_protection_kernel__harm_threshold_reading, speech_protection_kernel__marketplace_reading).
narrative_ontology:affects_constraint(speech_protection_kernel__harm_threshold_reading, speech_protection_kernel__dignity_reading).
narrative_ontology:affects_constraint(speech_protection_kernel__harm_threshold_reading, speech_protection_kernel__democratic_participation_reading).

% DUAL FORMULATION NOTE:
% The speech_protection_kernel family decomposes a contested natural-language concept (speech protection) into five structurally distinct readings, each with its own ε, beneficiary/victim structure, and type. This is the harm_threshold_reading, which interprets protection as conditional on absence of victim harm. The ε values differ substantially across readings because what counts as the constraint differs: for absolutist, the constraint is near-categorical protection (low ε); for harm-threshold, the constraint is protection conditional on harm threshold (higher ε, more extractive due to institutional gatekeeping). These are not the same constraint viewed differently—they are genuinely different constraints grounded in the same kernel. All five readings are linked via network.affects_constraints; the kernel_context in each reading documents its siblings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
