% ============================================================================
% CONSTRAINT STORY: speech_harm_boundary__dignity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_speech_harm_boundary__dignity_reading, []).

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
 *   constraint_id: speech_harm_boundary__dignity_reading
 *   human_readable: Dignity-Subordinate Speech Restriction (Dignity Reading)
 *   domain: constitutional_law/political_philosophy
 *
 * SUMMARY:
 *   This constraint instantiates the dignity_reading of the
 *   speech_harm_boundary kernel: a constitutional framework that subordinates
 *   speech protection to human dignity, categorically excluding Holocaust
 *   denial, hate speech, and group defamation from protected expression. The
 *   kernel is contested by an absolutist_reading (near-absolute speech
 *   protection) and a harm_balancing_reading (presumptive speech subject to
 *   proportionality). Under the dignity reading, targeted identity groups are
 *   the structural beneficiaries of state-enforced expressive limits, while
 *   prohibited speakers bear heavy asymmetric costs. The framework is
 *   actively enforced through constitutional courts and criminal law,
 *   generating high extractiveness with a genuine but asymmetric coordination
 *   function.
 *
 * KEY AGENTS:
 *   - constitutional_court: Agenda setter (institutional/constrained) â administers the dignity boundary
 *   - targeted_identity_groups: Primary beneficiary (moderate/constrained) â receives protection from dignity-violating speech
 *   - prohibited_speakers: Primary target (powerless/trapped) â bears expressive restriction and penalties
 *   - mainstream_media: Secondary payer (powerful/constrained) â absorbs chilling effects and compliance uncertainty
 *   - free_speech_advocates: Excluded voice (organized/constrained) â structurally overruled in dignity-first frameworks
 *   - comparative_legal_scholars: Analytical observer (analytical/analytical) â documents cross-jurisdictional divergence
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(speech_harm_boundary__dignity_reading, 0.82).
domain_priors:suppression_score(speech_harm_boundary__dignity_reading, 0.78).
domain_priors:theater_ratio(speech_harm_boundary__dignity_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(speech_harm_boundary__dignity_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(speech_harm_boundary__dignity_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(speech_harm_boundary__dignity_reading, theater_ratio, 0.35).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(speech_harm_boundary__dignity_reading, accessibility_collapse, 0.85).
narrative_ontology:constraint_metric(speech_harm_boundary__dignity_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(speech_harm_boundary__dignity_reading, tangled_rope).
narrative_ontology:human_readable(speech_harm_boundary__dignity_reading, "Dignity-Subordinate Speech Restriction (Dignity Reading)").
narrative_ontology:topic_domain(speech_harm_boundary__dignity_reading, "constitutional_law/political_philosophy").

domain_priors:requires_active_enforcement(speech_harm_boundary__dignity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(speech_harm_boundary__dignity_reading, '3015ab4b-291a-495a-ac27-5f3ea49786cc').
narrative_ontology:cs_kernel_codification('3015ab4b-291a-495a-ac27-5f3ea49786cc', formalized).
narrative_ontology:cs_authority_grounding('3015ab4b-291a-495a-ac27-5f3ea49786cc', lineage).
narrative_ontology:cs_interpretation_layer_present('3015ab4b-291a-495a-ac27-5f3ea49786cc').
narrative_ontology:cs_reading_relation('3015ab4b-291a-495a-ac27-5f3ea49786cc', speech_harm_boundary__absolutist_reading, forecloses).
narrative_ontology:cs_reading_relation('3015ab4b-291a-495a-ac27-5f3ea49786cc', speech_harm_boundary__harm_balancing_reading, coexists_with).
narrative_ontology:cs_axiom('3015ab4b-291a-495a-ac27-5f3ea49786cc', foundational, human_dignity_trumps_expression).
narrative_ontology:cs_axiom_status(human_dignity_trumps_expression, holdable).
narrative_ontology:cs_axiom_grounding('3015ab4b-291a-495a-ac27-5f3ea49786cc', human_dignity_trumps_expression, deontological).
narrative_ontology:cs_axiom('3015ab4b-291a-495a-ac27-5f3ea49786cc', foundational, personhood_denial_excluded_from_speech_protection).
narrative_ontology:cs_axiom_status(personhood_denial_excluded_from_speech_protection, holdable).
narrative_ontology:cs_axiom_grounding('3015ab4b-291a-495a-ac27-5f3ea49786cc', personhood_denial_excluded_from_speech_protection, deontological).
narrative_ontology:cs_reference_frame('3015ab4b-291a-495a-ac27-5f3ea49786cc', dignity_supremacy_framework).
narrative_ontology:cs_drift_state('3015ab4b-291a-495a-ac27-5f3ea49786cc', contemporary_human_rights_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('3015ab4b-291a-495a-ac27-5f3ea49786cc', '').
narrative_ontology:cs_kernel_id(speech_harm_boundary__dignity_reading, speech_harm_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(speech_harm_boundary__dignity_reading, targeted_identity_groups).
narrative_ontology:constraint_victim(speech_harm_boundary__dignity_reading, prohibited_speakers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(speech_harm_boundary__dignity_reading, mainstream_media).
narrative_ontology:constraint_vindicates(speech_harm_boundary__dignity_reading, dignity_supremacy_doctrine).
narrative_ontology:constraint_vindicates(speech_harm_boundary__dignity_reading, group_defamation_illegitimacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets constitutional dignity clauses to delimit protected speech; establishes categorical exclusions for Holocaust denial, hate speech, and group defamation; its jurisprudence sets the binding boundary between permissible expression and dignity violations.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__dignity_reading, constitutional_court, agenda_setter,
    institutional, generational, constrained, national).

% Receive legal protection from speech that denies their personhood or defames their group identity; rely on state enforcement to shield their social standing and secure their participatory equality.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__dignity_reading, targeted_identity_groups, beneficiary,
    moderate, generational, constrained, national).

% Face criminal penalties, civil liability, and censorship for expressing views classified as dignity-violating; their expressive freedom is categorically curtailed without proportionality assessment at the individual level.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__dignity_reading, prohibited_speakers, payer,
    powerless, biographical, trapped, national).

% Operate under legal uncertainty near the dignity boundary; self-censor to avoid liability, absorbing compliance costs and chilling effects even when not directly targeted.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__dignity_reading, mainstream_media, payer,
    powerful, biographical, constrained, national).

% Advance absolutist and libertarian arguments against categorical exclusions; formally present in constitutional discourse but systematically overruled in dignity-first frameworks.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__dignity_reading, free_speech_advocates, excluded,
    organized, generational, constrained, national).

% Analyze cross-jurisdictional variation in speech-dignity balancing; document divergence between dignity-supremacy and absolutist constitutional traditions.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__dignity_reading, comparative_legal_scholars, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Protects the social preconditions for democratic equality by preventing the dissemination of speech that denies personhood or degrades group identity, thereby shielding vulnerable communities from dignitary harm.
% TRANSFER_FUNCTION: Transfers expressive freedom from speakers of identity-harm content to targeted identity groups in the form of state-backed dignitary protection and social standing security.
% ABSENT_VOICES: Free speech absolutists and civil libertarian organizations are formally present in legal discourse but systematically overruled; prohibited speakers are procedurally excluded from norm-setting and only encountered as defendants.
% DISAPPEARANCE_RATIONALE: Without the dignity-subordinate framework, categorical exclusions for hate speech and Holocaust denial would collapse, targeted identity groups would lose structured legal protection against dignitary attack, and constitutional jurisprudence would reorganize around speech-presumptive or absolutist principles.
% FOUNDING_PROBLEM: The historical collapse of democratic order under Nazi Germany and apartheid South Africa demonstrated that unchecked personhood-denying speech destroys the social and moral preconditions for minority participation, necessitating constitutional barriers against group defamation and dignitary degradation.
% FOUNDING_PROBLEM_CORROBORATION: Constitutional courts in Germany and South Africa corroborate the problem from within the dignity tradition; however, U.S. constitutional scholars and free speech advocates from outside the beneficiary set dispute that the founding problem warrants categorical exclusion, arguing that counterspeech and democratic resilience are sufficient protections.
narrative_ontology:disappearance_verdict(speech_harm_boundary__dignity_reading, world_rearranges).
narrative_ontology:founding_problem_status(speech_harm_boundary__dignity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(speech_harm_boundary__dignity_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(speech_harm_boundary__dignity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(speech_harm_boundary__dignity_reading, 0.82, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(speech_harm_boundary__dignity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(speech_harm_boundary__dignity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(speech_harm_boundary__dignity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.82) is high because the constraint categorically removes whole categories of expression from protection, imposing criminal and civil penalties without individual proportionality. Suppression (0.78) is high because alternatives (speaking prohibited content) are closed off by active state enforcement. Theater ratio (0.35) reflects moderate performative maintenance: some enforcement is genuine protection, but a growing share involves ritual condemnation and symbolic prosecution that does not materially reduce harm. Accessibility collapse (0.85) is high because once the categorical boundary is established, legal and social pathways for prohibited speech nearly vanish. Resistance (0.55) reflects persistent but institutionally overruled opposition from free speech advocates.
 *
 * PERSPECTIVAL GAP:
 *   The constitutional court experiences the constraint as legitimate coordination it maintains to protect democratic foundations; prohibited speakers experience it as extraction of their expressive liberty. Targeted identity groups experience protection that they may read as justice rather than extraction, while mainstream media experiences diffuse costs through chilling effects. The engine will compute divergent per-seat types from these structural positions.
 *
 * DIRECTIONALITY LOGIC:
 *   Targeted identity groups are declared beneficiaries (low d), receiving state-backed protection that subsidizes their social standing. Prohibited speakers are declared victims (high d), paying the cost of categorical exclusion with near-zero exit options. Mainstream media, though not a declared victim, faces constrained exit and chilling costs, placing its derived d in the mid-range. The constitutional court, as agenda setter with constrained doctrinal exit, sits near the coordination-administration pole rather than the extraction pole.
 *
 * MANDATROPHY ANALYSIS:
 *   The framework prevents mislabeling by requiring both beneficiaries and victims for tangled rope certification. If we observed only the coordination function (protecting minorities), we might classify it as rope; if we observed only the extraction (silencing speakers), we might classify it as snare. The structural requirement of both parties, plus active enforcement, forces the tangled rope classification that captures the hybrid reality.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'Is the dignity-subordinate reading of the speech harm boundary the structurally correct instantiation of the constitutional kernel, or does the absolutist reading capture the kernel''s true architecture?',
    'Comparative constitutional analysis tracing the original framing debates and subsequent interpretive traditions across dignity-based and absolutist jurisdictions.',
    'If the absolutist reading is the kernel''s true structure, this dignity reading is a false summit or a snare using dignity as cover. If dignity is the true kernel, the high extraction is the price of a genuine coordination function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Which reading correctly instantiates the contested speech-dignity kernel.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression of prohibited speech achieved primarily through structural legal penalties or through internalized self-censorship driven by vague dignity standards?',
    'Empirical measurement of speech volume pre- and post-enforcement, combined with qualitative studies of speaker self-censorship motivations.',
    'If internalized, effective extraction exceeds structural measures because speakers police their own expression beyond legal minima, pushing the constraint toward snare-like behavior.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural versus internalized suppression mechanism.').

omega_variable(
    coordination_extraction_boundary,
    'Does the dignity framework genuinely coordinate the protection of vulnerable groups, or does it primarily legitimate state control over political discourse?',
    'Historical case studies examining whether dignity-based speech restrictions correlate with improved minority social outcomes or with expanded state policing of dissent.',
    'If the latter, the constraint is a snare using dignity as cover. If the former, the high extraction is the asymmetric cost of genuine coordination.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_extraction_boundary, empirical, 'Whether the dignity constraint coordinates protection or extracts liberty for state control.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(speech_harm_boundary__dignity_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(shb_dignity_tr_t0, speech_harm_boundary__dignity_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(shb_dignity_tr_t8, speech_harm_boundary__dignity_reading, theater_ratio, 8, 0.22).
narrative_ontology:measurement(shb_dignity_tr_t16, speech_harm_boundary__dignity_reading, theater_ratio, 16, 0.25).
narrative_ontology:measurement(shb_dignity_tr_t24, speech_harm_boundary__dignity_reading, theater_ratio, 24, 0.28).
narrative_ontology:measurement(shb_dignity_tr_t32, speech_harm_boundary__dignity_reading, theater_ratio, 32, 0.32).
narrative_ontology:measurement(shb_dignity_tr_t40, speech_harm_boundary__dignity_reading, theater_ratio, 40, 0.35).

% Extraction over time
narrative_ontology:measurement(shb_dignity_be_t0, speech_harm_boundary__dignity_reading, base_extractiveness, 0, 0.58).
narrative_ontology:measurement(shb_dignity_be_t8, speech_harm_boundary__dignity_reading, base_extractiveness, 8, 0.64).
narrative_ontology:measurement(shb_dignity_be_t16, speech_harm_boundary__dignity_reading, base_extractiveness, 16, 0.7).
narrative_ontology:measurement(shb_dignity_be_t24, speech_harm_boundary__dignity_reading, base_extractiveness, 24, 0.76).
narrative_ontology:measurement(shb_dignity_be_t32, speech_harm_boundary__dignity_reading, base_extractiveness, 32, 0.8).
narrative_ontology:measurement(shb_dignity_be_t40, speech_harm_boundary__dignity_reading, base_extractiveness, 40, 0.82).

% Suppression requirement over time
narrative_ontology:measurement(shb_dignity_su_t0, speech_harm_boundary__dignity_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(shb_dignity_su_t8, speech_harm_boundary__dignity_reading, suppression_requirement, 8, 0.52).
narrative_ontology:measurement(shb_dignity_su_t16, speech_harm_boundary__dignity_reading, suppression_requirement, 16, 0.6).
narrative_ontology:measurement(shb_dignity_su_t24, speech_harm_boundary__dignity_reading, suppression_requirement, 24, 0.68).
narrative_ontology:measurement(shb_dignity_su_t32, speech_harm_boundary__dignity_reading, suppression_requirement, 32, 0.74).
narrative_ontology:measurement(shb_dignity_su_t40, speech_harm_boundary__dignity_reading, suppression_requirement, 40, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(speech_harm_boundary__dignity_reading, speech_harm_boundary__absolutist_reading).
narrative_ontology:affects_constraint(speech_harm_boundary__dignity_reading, speech_harm_boundary__harm_balancing_reading).

% DUAL FORMULATION NOTE:
% This constraint is the dignity-subordinate reading of the speech_harm_boundary kernel. Its sibling readings (absolutist, harm-balancing) instantiate the same kernel with different epsilon values and beneficiary/victim structures.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
