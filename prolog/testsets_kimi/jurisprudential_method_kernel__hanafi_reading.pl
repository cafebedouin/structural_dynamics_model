% ============================================================================
% CONSTRAINT STORY: jurisprudential_method_kernel__hanafi_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jurisprudential_method_kernel__hanafi_reading, []).

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
    constraint_indexing:constraint_classification/3,
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
 *   constraint_id: jurisprudential_method_kernel__hanafi_reading
 *   human_readable: Hanafi Jurisprudential Method (Qiyas and Istihsan Reading)
 *   domain: legal/religious/institutional
 *
 * SUMMARY:
 *   This constraint instantiates the Hanafi reading of the
 *   jurisprudential_method_kernel: law derives from Qur'an and Hadith
 *   filtered through extensive analogical reasoning (qiyas) and juristic
 *   preference (istihsan), with reason treated as a legitimate tool for
 *   extending divine intent to novel cases. The constraint is authored as a
 *   Tangled Rope â it provides genuine coordination (systematic legal
 *   extension for unprecedented cases) while concentrating interpretive
 *   authority in the class of trained Hanafi jurists and structurally
 *   marginalizing textualist claims to exclusive textual authenticity. This
 *   is one of four structurally distinct readings of the same kernel; sibling
 *   constraints instantiate Maliki (Medinan practice), Shafi'i (four-tier
 *   hierarchy), and Hanbali (literalist rejection of qiyas) readings.
 *
 * KEY AGENTS:
 *   - hanafi_jurists: Primary agenda-setter and beneficiary (institutional/identity-locked) â controls analogical method and occupies teaching and judicial posts
 *   - textualist_scholars: Primary payer and victim (moderate/constrained) â bears costs of exclusion from legitimate discourse and institutional legal roles
 *   - state_judiciary: Secondary beneficiary (institutional/constrained) â applies the flexible methodology in governance and benefits from its adaptability
 *   - lay_muslim_communities: Dual-positioned payer/beneficiary (organized/constrained) â benefits from legal continuity, pays through jurist-mediated access and dependency
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jurisprudential_method_kernel__hanafi_reading, 0.76).
domain_priors:suppression_score(jurisprudential_method_kernel__hanafi_reading, 0.64).
domain_priors:theater_ratio(jurisprudential_method_kernel__hanafi_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jurisprudential_method_kernel__hanafi_reading, extractiveness, 0.76).
narrative_ontology:constraint_metric(jurisprudential_method_kernel__hanafi_reading, suppression_requirement, 0.64).
narrative_ontology:constraint_metric(jurisprudential_method_kernel__hanafi_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jurisprudential_method_kernel__hanafi_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(jurisprudential_method_kernel__hanafi_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jurisprudential_method_kernel__hanafi_reading, tangled_rope).
narrative_ontology:human_readable(jurisprudential_method_kernel__hanafi_reading, "Hanafi Jurisprudential Method (Qiyas and Istihsan Reading)").
narrative_ontology:topic_domain(jurisprudential_method_kernel__hanafi_reading, "legal/religious/institutional").

domain_priors:requires_active_enforcement(jurisprudential_method_kernel__hanafi_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jurisprudential_method_kernel__hanafi_reading, 'd5c0e8c1-94c6-4dc2-9df7-157640ad110d').
narrative_ontology:cs_kernel_codification('d5c0e8c1-94c6-4dc2-9df7-157640ad110d', fixed_text).
narrative_ontology:cs_authority_grounding('d5c0e8c1-94c6-4dc2-9df7-157640ad110d', lineage).
narrative_ontology:cs_interpretation_layer_present('d5c0e8c1-94c6-4dc2-9df7-157640ad110d').
narrative_ontology:cs_reading_relation('d5c0e8c1-94c6-4dc2-9df7-157640ad110d', jurisprudential_method_kernel__maliki_reading, coexists_with).
narrative_ontology:cs_reading_relation('d5c0e8c1-94c6-4dc2-9df7-157640ad110d', jurisprudential_method_kernel__shafii_reading, influences).
narrative_ontology:cs_reading_relation('d5c0e8c1-94c6-4dc2-9df7-157640ad110d', jurisprudential_method_kernel__hanbali_reading, forecloses).
narrative_ontology:cs_axiom('d5c0e8c1-94c6-4dc2-9df7-157640ad110d', foundational, analogical_reasoning_legitimate).
narrative_ontology:cs_axiom_status(analogical_reasoning_legitimate, holdable).
narrative_ontology:cs_axiom_grounding('d5c0e8c1-94c6-4dc2-9df7-157640ad110d', analogical_reasoning_legitimate, deontological).
narrative_ontology:cs_axiom('d5c0e8c1-94c6-4dc2-9df7-157640ad110d', foundational, juristic_preference_authoritative).
narrative_ontology:cs_axiom_status(juristic_preference_authoritative, holdable).
narrative_ontology:cs_axiom_grounding('d5c0e8c1-94c6-4dc2-9df7-157640ad110d', juristic_preference_authoritative, deontological).
narrative_ontology:cs_reference_frame('d5c0e8c1-94c6-4dc2-9df7-157640ad110d', hanafi_rationalist_jurisprudence).
narrative_ontology:cs_drift_state('d5c0e8c1-94c6-4dc2-9df7-157640ad110d', modern_textualist_challenge, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('d5c0e8c1-94c6-4dc2-9df7-157640ad110d', '').
narrative_ontology:cs_kernel_id(jurisprudential_method_kernel__hanafi_reading, jurisprudential_method_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jurisprudential_method_kernel__hanafi_reading, hanafi_jurists).
narrative_ontology:constraint_beneficiary(jurisprudential_method_kernel__hanafi_reading, state_judiciary).
narrative_ontology:constraint_victim(jurisprudential_method_kernel__hanafi_reading, textualist_scholars).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(jurisprudential_method_kernel__hanafi_reading, lay_muslim_communities).
narrative_ontology:constraint_victim(jurisprudential_method_kernel__hanafi_reading, lay_muslim_communities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Control the analogical reasoning apparatus (qiyas) and juristic preference (istihsan), train successors in madrasa and judicial circuits, derive rulings for novel cases, and occupy posts whose authority depends on the legitimacy of rationalist extension. Their professional identity, livelihood, and religious self-concept are fused with the Hanafi methodological tradition.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__hanafi_reading, hanafi_jurists, agenda_setter,
    institutional, generational, identity_locked, global).

% Assert that law derives exclusively from the literal text of Qur'an and Hadith; are marginalized in institutional legal education, judicial appointment, and scholarly discourse where Hanafi rationalism dominates. Their claims to textual authenticity are treated as simplistic, and their method is excluded from official curricula and state courts.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__hanafi_reading, textualist_scholars, payer,
    moderate, generational, constrained, global).

% Applies Hanafi methodology in state courts and official fatwa offices; benefits from flexible legal tools to govern diverse populations and novel administrative problems. Locked into the madhhab system by state adoption and endowment structures, with limited ability to switch methodological frameworks without political upheaval.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__hanafi_reading, state_judiciary, beneficiary,
    institutional, generational, constrained, continental).

% Seek legal resolution for marriage, commerce, and inheritance; depend on jurist-mediated access to law and cannot independently verify analogical derivations. They benefit from systematic answers to novel questions but pay through fees, deferred autonomy, and dependence on a gatekeeping scholarly class.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__hanafi_reading, lay_muslim_communities, payer,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(jurisprudential_method_kernel__hanafi_reading, lay_muslim_communities, beneficiary).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(jurisprudential_method_kernel__hanafi_reading, hanafi_jurists).
narrative_ontology:fixing_cost_class(jurisprudential_method_kernel__hanafi_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Extends divine legal guidance to novel cases not explicitly addressed by Qur'an or Hadith, enabling systematic legal continuity across changing social, economic, and geographic contexts.
% TRANSFER_FUNCTION: Moves interpretive authority from the scriptural text to trained jurists who control analogical reasoning (qiyas) and juristic preference (istihsan); moves institutional legitimacy from textualist literalism to rationalist legal extension.
% ABSENT_VOICES: Textualist scholars who reject analogical reasoning as religious innovation (bid'ah), and lay Muslims who might prefer direct unmediated textual access but are excluded from juristic discourse by training and language barriers.
% DISAPPEARANCE_RATIONALE: The Hanafi legal tradition would lose its primary methodological engine for novel cases; legal continuity would depend on migration to literalist or tradition-based methods, the institutional role of rationalist jurists would collapse, and state courts would face an unprocessed backlog of unprecedented cases.
% FOUNDING_PROBLEM: The finite textual sources (Qur'an and Hadith) leave the vast majority of practical legal questions unanswered; early Muslim communities in Iraq and elsewhere faced an expanding range of novel cases (nawazil) requiring authoritative determination.
% FOUNDING_PROBLEM_CORROBORATION: Hanafi jurists attest the problem remains live through centuries of fatwa literature. Textualist critics outside the beneficiary set argue that the textual corpus is sufficient if correctly understood and that qiyas is an unnecessary innovation; historical records of early Kufan legal development corroborate the novel-case pressure, while textualist counter-narratives corroborate the contested status.
narrative_ontology:disappearance_verdict(jurisprudential_method_kernel__hanafi_reading, world_rearranges).
narrative_ontology:founding_problem_status(jurisprudential_method_kernel__hanafi_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jurisprudential_method_kernel__hanafi_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(jurisprudential_method_kernel__hanafi_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jurisprudential_method_kernel__hanafi_reading, 0.76, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jurisprudential_method_kernel__hanafi_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(jurisprudential_method_kernel__hanafi_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(jurisprudential_method_kernel__hanafi_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.76) because the analogical method places a trained jurist class between scriptural sources and legal outcomes, extracting status, employment, and gatekeeping rents. Suppression (0.64) reflects the institutional marginalization of textualist alternatives in Hanafi-dominated educational and judicial systems, not physical coercion. Theater ratio is moderate (0.30): jurists maintain performative fidelity to Qur'an and Hadith while extensively deploying ra'y (individual reasoning), creating a visible gap between claimed and actual method. Accessibility collapse (0.50) is moderate because alternative schools exist globally, but within the Hanafi framework alternatives to qiyas are largely closed. Resistance (0.62) is substantial due to persistent textualist critique and inter-madhhab competition.
 *
 * PERSPECTIVAL GAP:
 *   From the Hanafi jurist seat, the constraint appears as necessary coordination: without qiyas, the law cannot address novel cases, and the jurist performs a service of extension. From the textualist scholar seat, the same structure appears as extraction: human reason has been illegitimately elevated to parity with divine text, creating a self-propagating priestly class of rationalist jurists.
 *
 * DIRECTIONALITY LOGIC:
 *   Hanafi jurists are structural beneficiaries (low d): the constraint subsidizes their professional existence and authority. State judiciary is also low-to-mid d, gaining governance flexibility. Textualist scholars are structural victims (high d): the constraint extracts legitimacy from their literalist claims and suppresses their method. Lay communities sit near symmetric (d ~ 0.5), receiving genuine coordination benefit while paying diffuse gatekeeping costs.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem â unanswered novel cases beyond explicit text â remains structurally live and is corroborated by historical legal development. This prevents classification as a pure Snare (the coordination is not merely cover) and as a Piton (the function has not atrophied). However, the high extractiveness and moderate theater indicate that coordination and extraction are coupled: the same method that solves novel cases also entrenches a jurist class. Mandatrophy would appear if the volume of genuinely novel cases collapsed while the jurist apparatus persisted; current metrics suggest the problem remains contested rather than dead.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_scope,
    'This constraint is one reading of a contested kernel; how do sibling readings change the structural classification?',
    'Generate and compare the four sibling constraints to map the kernel''s epsilon variance and beneficiary displacement across the madhhab family.',
    'Sibling readings may compute as Mountain (Maliki tradition-as-practice), Rope (Shafi''i hierarchy), or Snare (Hanbali literalism), revealing the kernel as a false summit if treated as a monolithic natural law.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_scope, conceptual, 'Committer-frame omega: this constraint is one reading of a contested kernel with structurally distinct siblings.').

omega_variable(
    analogical_extension_authenticity,
    'Does analogical reasoning (qiyas) discover pre-existing divine intent or construct human legal authority?',
    'Historical-philological analysis of early Hanafi texts distinguishing discovery vs invention metaphors; sociological measurement of jurist-class reproduction rates across generations.',
    'If construction, the extractiveness metric is fully warranted; if discovery, the jurist class is more akin to a Mountain''s guardian than an extractor, warranting lower epsilon.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(analogical_extension_authenticity, conceptual, 'Whether qiyas is epistemic discovery or authority construction.').

omega_variable(
    textualist_suppression_mechanism,
    'Is the marginalization of textualist scholars structural (exclusion from courts and endowments) or epistemic (their method fails to solve cases)?',
    'Quantitative analysis of judicial appointments and madrasa curriculum by methodological orientation; comparison of case-resolution rates between analogical and literalist approaches.',
    'Structural exclusion would raise suppression; epistemic failure would lower it and shift victim classification toward incidental rather than targeted.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(textualist_suppression_mechanism, empirical, 'Structural vs epistemic mechanism of textualist marginalization.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jurisprudential_method_kernel__hanafi_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(juri_tr_t0, jurisprudential_method_kernel__hanafi_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(juri_tr_t8, jurisprudential_method_kernel__hanafi_reading, theater_ratio, 8, 0.16).
narrative_ontology:measurement(juri_tr_t16, jurisprudential_method_kernel__hanafi_reading, theater_ratio, 16, 0.2).
narrative_ontology:measurement(juri_tr_t24, jurisprudential_method_kernel__hanafi_reading, theater_ratio, 24, 0.25).
narrative_ontology:measurement(juri_tr_t32, jurisprudential_method_kernel__hanafi_reading, theater_ratio, 32, 0.28).
narrative_ontology:measurement(juri_tr_t40, jurisprudential_method_kernel__hanafi_reading, theater_ratio, 40, 0.3).

% Extraction over time
narrative_ontology:measurement(juri_be_t0, jurisprudential_method_kernel__hanafi_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(juri_be_t8, jurisprudential_method_kernel__hanafi_reading, base_extractiveness, 8, 0.61).
narrative_ontology:measurement(juri_be_t16, jurisprudential_method_kernel__hanafi_reading, base_extractiveness, 16, 0.66).
narrative_ontology:measurement(juri_be_t24, jurisprudential_method_kernel__hanafi_reading, base_extractiveness, 24, 0.71).
narrative_ontology:measurement(juri_be_t32, jurisprudential_method_kernel__hanafi_reading, base_extractiveness, 32, 0.74).
narrative_ontology:measurement(juri_be_t40, jurisprudential_method_kernel__hanafi_reading, base_extractiveness, 40, 0.76).

% Suppression requirement over time
narrative_ontology:measurement(juri_su_t0, jurisprudential_method_kernel__hanafi_reading, suppression_requirement, 0, 0.42).
narrative_ontology:measurement(juri_su_t8, jurisprudential_method_kernel__hanafi_reading, suppression_requirement, 8, 0.5).
narrative_ontology:measurement(juri_su_t16, jurisprudential_method_kernel__hanafi_reading, suppression_requirement, 16, 0.57).
narrative_ontology:measurement(juri_su_t24, jurisprudential_method_kernel__hanafi_reading, suppression_requirement, 24, 0.62).
narrative_ontology:measurement(juri_su_t32, jurisprudential_method_kernel__hanafi_reading, suppression_requirement, 32, 0.64).
narrative_ontology:measurement(juri_su_t40, jurisprudential_method_kernel__hanafi_reading, suppression_requirement, 40, 0.64).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


% DUAL FORMULATION NOTE:
% This constraint is one reading of the jurisprudential_method_kernel. The kernel decomposes into four structurally distinct constraints (Hanafi, Maliki, Shafii, Hanbali readings) because each reading assigns a different epsilon, different beneficiary/victim structure, and different authority grounding to the same nominal source material. Each family member links to its siblings via network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
