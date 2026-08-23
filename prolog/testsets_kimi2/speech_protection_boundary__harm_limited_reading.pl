% ============================================================================
% CONSTRAINT STORY: speech_protection_boundary__harm_limited_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:coordination_type/2,
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
 *   constraint_id: speech_protection_boundary__harm_limited_reading
 *   human_readable: Speech Protection Conditional on Dignity, Equality, and Harassment Absence (Harm-Limited Reading)
 *   domain: constitutional_law/political_philosophy
 *
 * SUMMARY:
 *   This constraint story captures the harm_limited_reading of the
 *   speech_protection_boundary kernel: a constitutional or legal arrangement
 *   under which speech is protected only insofar as it does not cause
 *   significant harm to dignity, equality, or freedom from harassment. Under
 *   this reading, hate speech, targeted harassment, and coded dog whistles
 *   fall outside the protected set, and the state or its delegated
 *   institutions become the gatekeeper of permissible expression. This
 *   creates a genuine coordination function for marginalized groups seeking
 *   protection from hostile expression, but simultaneously concentrates
 *   interpretive power in the state, generating asymmetric extraction from
 *   speakers and chilling effects on public discourse. The story is authored
 *   as a Tangled Rope: the coordination (dignity protection) and extraction
 *   (gatekeeper power, speech restriction) are structurally inseparable and
 *   require active enforcement. The sibling readingsâabsolutist_reading and
 *   balancing_readingâare modeled as separate constraints in the same
 *   family.
 *
 * KEY AGENTS:
 *   - state_gatekeeper (institutional/analytical): Administers the speech/harm boundary and holds discretionary interpretive authority.
 *   - marginalized_groups (moderate/constrained): Beneficiaries of protection from dignity-impairing expression.
 *   - speakers_of_controversial_views (moderate/constrained): Payers who bear sanctions and chilling effects.
 *   - civil_liberties_advocates (organized/analytical): Observers who resist overreach.
 *   - absolutist_advocates (organized/analytical): Excluded voices under this reading.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(speech_protection_boundary__harm_limited_reading, 0.72).
domain_priors:suppression_score(speech_protection_boundary__harm_limited_reading, 0.78).
domain_priors:theater_ratio(speech_protection_boundary__harm_limited_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(speech_protection_boundary__harm_limited_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(speech_protection_boundary__harm_limited_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(speech_protection_boundary__harm_limited_reading, theater_ratio, 0.35).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(speech_protection_boundary__harm_limited_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(speech_protection_boundary__harm_limited_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(speech_protection_boundary__harm_limited_reading, tangled_rope).
narrative_ontology:human_readable(speech_protection_boundary__harm_limited_reading, "Speech Protection Conditional on Dignity, Equality, and Harassment Absence (Harm-Limited Reading)").
narrative_ontology:topic_domain(speech_protection_boundary__harm_limited_reading, "constitutional_law/political_philosophy").

domain_priors:requires_active_enforcement(speech_protection_boundary__harm_limited_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(speech_protection_boundary__harm_limited_reading, '7dffee6f-7b9a-4430-adf8-673c24522819').
narrative_ontology:cs_kernel_codification('7dffee6f-7b9a-4430-adf8-673c24522819', formalized).
narrative_ontology:cs_authority_grounding('7dffee6f-7b9a-4430-adf8-673c24522819', lineage).
narrative_ontology:cs_interpretation_layer_present('7dffee6f-7b9a-4430-adf8-673c24522819').
narrative_ontology:cs_reading_relation('7dffee6f-7b9a-4430-adf8-673c24522819', speech_protection_boundary__absolutist_reading, forecloses).
narrative_ontology:cs_reading_relation('7dffee6f-7b9a-4430-adf8-673c24522819', speech_protection_boundary__balancing_reading, coexists_with).
narrative_ontology:cs_axiom('7dffee6f-7b9a-4430-adf8-673c24522819', foundational, dignity_harm_as_speech_boundary).
narrative_ontology:cs_axiom_status(dignity_harm_as_speech_boundary, holdable).
narrative_ontology:cs_axiom_grounding('7dffee6f-7b9a-4430-adf8-673c24522819', dignity_harm_as_speech_boundary, deontological).
narrative_ontology:cs_axiom('7dffee6f-7b9a-4430-adf8-673c24522819', secondary, state_gatekeeper_legitimacy).
narrative_ontology:cs_axiom_status(state_gatekeeper_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('7dffee6f-7b9a-4430-adf8-673c24522819', state_gatekeeper_legitimacy, conventional).
narrative_ontology:cs_reference_frame('7dffee6f-7b9a-4430-adf8-673c24522819', constitutional_dignity_framework).
narrative_ontology:cs_drift_state('7dffee6f-7b9a-4430-adf8-673c24522819', contemporary_political_polarization_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('7dffee6f-7b9a-4430-adf8-673c24522819', '').
narrative_ontology:cs_kernel_id(speech_protection_boundary__harm_limited_reading, speech_protection_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(speech_protection_boundary__harm_limited_reading, marginalized_groups).
narrative_ontology:constraint_victim(speech_protection_boundary__harm_limited_reading, speakers_of_controversial_views).
narrative_ontology:constraint_vindicates(speech_protection_boundary__harm_limited_reading, dignity_supremacy_in_speech_regulation).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the boundary between protected and unprotected speech through courts, administrative tribunals, or regulatory bodies. Defines and interprets 'significant harm' to dignity, equality, and harassment. Holds discretionary power to classify expression as outside protection, with attendant risks of viewpoint-based application.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__harm_limited_reading, state_gatekeeper, agenda_setter,
    institutional, generational, analytical, national).

% Receive institutional protection from expression that demeans their dignity or promotes harassment. Their equality and freedom from harassment are formally vindicated by the constraint, but they depend on state gatekeepers to activate and sustain the protection, and cannot unilaterally enforce the boundary.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__harm_limited_reading, marginalized_groups, beneficiary,
    moderate, biographical, constrained, national).

% Bear the risk of sanctions, investigation, or censorship when their expression is deemed to impair dignity or constitute harassment. Face substantial chilling effects due to uncertain and evolving definitions of harm. Cannot exit the jurisdiction's speech regime without relocating or silencing themselves.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__harm_limited_reading, speakers_of_controversial_views, payer,
    moderate, immediate, constrained, national).

% Monitor and challenge expansions of harm-based speech restrictions. They argue that dignity-limitations are susceptible to political capture and chilling of legitimate dissent, but their litigation and advocacy do not prevent the gatekeeper structure from operating.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__harm_limited_reading, civil_liberties_advocates, observer,
    organized, generational, analytical, national).

% Advocate for near-absolute speech protection and reject dignity-based exceptions. Their position is treated as legally irrelevant within the harm-limited framework and they are not invited to adjudicate the boundary, though they may protest from outside the institutional process.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__harm_limited_reading, absolutist_advocates, excluded,
    organized, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(speech_protection_boundary__harm_limited_reading, state_gatekeeper).
narrative_ontology:fixing_cost_class(speech_protection_boundary__harm_limited_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Protects the dignity, equality, and freedom from harassment of marginalized and targeted groups by creating a legal mechanism to sanction expression that causes significant harm to these interests, thereby coordinating a social environment with reduced discriminatory hostility.
% TRANSFER_FUNCTION: Transfers the cost of uncertain speech boundaries from protected groups to speakers, and transfers discretionary interpretive authority over permissible expression to state or institutional gatekeepers.
% ABSENT_VOICES: Absolutist free speech advocates and speakers from jurisdictions with near-absolute protections are structurally marginalized in the framing; their objections to the gatekeeper model are treated as external to the dignity-equilibrium the constraint establishes. Also, voices advocating for community-based rather than state-based resolution of harmful speech are underrepresented.
% DISAPPEARANCE_RATIONALE: If the constraint vanished, previously prohibited hate speech and harassment would no longer be sanctionable under this framework; protected groups would lose a designated legal remedy, speakers would face reduced chilling effects but potentially increased exposure to hostile expression, and the gatekeeper institutions would lose a significant domain of interpretive authority over public discourse.
% FOUNDING_PROBLEM: The historical failure of near-absolute speech regimes to prevent systematic dignity harms, targeted harassment, and the silencing of marginalized groups through hostile expression; the absence of legal recourse for structural inequality reproduced through speech.
% FOUNDING_PROBLEM_CORROBORATION: Marginalized groups and equality scholars attest the problem remains live. Civil liberties advocates and some constitutional historians attest the founding problem was overstated or that the cure creates worse harms; legislative records and comparative constitutional studies from outside the benefiting parties provide mixed corroboration depending on jurisdiction.
narrative_ontology:disappearance_verdict(speech_protection_boundary__harm_limited_reading, world_rearranges).
narrative_ontology:founding_problem_status(speech_protection_boundary__harm_limited_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(speech_protection_boundary__harm_limited_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(speech_protection_boundary__harm_limited_reading, 'none', 1).
narrative_ontology:epsilon_provenance(speech_protection_boundary__harm_limited_reading, 0.72, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness (0.72) is high because the constraint systematically removes categories of speech from protection and concentrates gatekeeper discretion, but it is not maximal because the coordination function (protecting dignity) is structurally genuine and not merely cover. Suppression (0.78) is high because the arrangement requires active judicial or administrative enforcement to maintain the boundary against both prohibited speakers and would-be absolutist challengers. Theater ratio (0.35) reflects moderate performative maintenance: some enforcement actions serve symbolic legitimization of the dignity regime, but a substantial portion directly punishes or deters prohibited expression. Accessibility collapse (0.68) captures the near-total legal exclusion of absolute-speech alternatives within the jurisdiction once the doctrine is entrenched. Resistance (0.70) reflects robust and organized opposition from civil liberties constituencies. The temporal series show a ratchet: extractiveness and suppression rise as the gatekeeper's interpretive scope expands and enforcement infrastructure matures.
 *
 * PERSPECTIVAL GAP:
 *   The beneficiary seat (marginalized_groups) experiences the constraint as protective coordination that reduces hostile expression and vindicates equality. The payer seat (speakers_of_controversial_views) experiences the same structure as extractive constraint: uncertain boundaries, chilling effects, and exposure to state discipline. The state_gatekeeper seat experiences the arrangement as a legitimate juridical function. The engine will compute divergent per-seat classifications from this structural asymmetry.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries are marginalized_groups, who receive legal protection and dignity vindication; their directionality sits near the beneficiary end (low d). Victims are speakers_of_controversial_views, who bear the costs of restriction and chilling; their directionality sits near the target end (high d). The state_gatekeeper is not declared as either beneficiary or victim; its directionality falls back to the institutional canonical default, reflecting its administrative rather than captured position, though the gain_flow records that extracted discretion accrues to this seat. Civil liberties advocates and absolutist advocates sit outside the primary beneficiary/victim axis.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as tangled_rope prevents mislabeling the constraint as pure extraction (snare) by requiring identification of the genuine coordination function: the protection of marginalized groups from harassment and dignity harms is real and desired by the beneficiaries. Conversely, it prevents mislabeling as pure coordination (rope) by requiring identification of victims (speakers facing restrictions) and active enforcement. If the founding problem (dignity harms in unregulated speech environments) were dead and the constraint persisted solely as state power projection, it would drift toward piton or snare; the founding_problem_status is contested, and the measurements show extraction accumulation rather than mandate resolution, indicating the coordination function remains live but increasingly entangled with gatekeeper expansion.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_committer,
    'This constraint is the harm_limited_reading of the speech_protection_boundary kernel; does the structural analysis change if the absolutist or balancing reading is adopted instead?',
    'Compare the three constraint stories in the family; divergence in epsilon, beneficiary sets, and directionality confirms the reading-dependence of the classification.',
    'If the kernel is reading-dependent, no single classification applies to ''speech protection'' as a whole; the corpus must treat each reading as a distinct constraint.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_committer, conceptual, 'Committee frame omega: this constraint is one reading of a contested kernel.').

omega_variable(
    gatekeeper_abuse_risk,
    'Does the state gatekeeper function extract more disciplinary power than the coordination function (dignity protection) requires?',
    'Comparative analysis of jurisdictions with and without dignity-based speech limits: measure rates of viewpoint-based prosecution, chilling effect surveys, and expansion of ''harm'' definitions over time.',
    'If abuse systematically exceeds protection, the constraint shifts toward snare; if tightly coupled to demonstrated harm, it remains tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(gatekeeper_abuse_risk, empirical, 'Whether gatekeeper power exceeds protective necessity.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (legal penalties and enforcement) or internalized (self-censorship and chilling effects)?',
    'Post-reform or post-judicial-review trajectory: if speech behavior changes rapidly after legal standard relaxation, suppression was primarily structural; if reluctance persists, internalization is significant.',
    'If internalized, effective suppression is higher than structural measure suggests, and the constraint''s extractive impact on speakers is deeper.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs internalized suppression mechanism.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(speech_protection_boundary__harm_limited_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(speech_harm_limited_tr_t0, speech_protection_boundary__harm_limited_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(speech_harm_limited_tr_t8, speech_protection_boundary__harm_limited_reading, theater_ratio, 8, 0.2).
narrative_ontology:measurement(speech_harm_limited_tr_t16, speech_protection_boundary__harm_limited_reading, theater_ratio, 16, 0.25).
narrative_ontology:measurement(speech_harm_limited_tr_t24, speech_protection_boundary__harm_limited_reading, theater_ratio, 24, 0.3).
narrative_ontology:measurement(speech_harm_limited_tr_t32, speech_protection_boundary__harm_limited_reading, theater_ratio, 32, 0.33).
narrative_ontology:measurement(speech_harm_limited_tr_t40, speech_protection_boundary__harm_limited_reading, theater_ratio, 40, 0.35).

% Extraction over time
narrative_ontology:measurement(speech_harm_limited_be_t0, speech_protection_boundary__harm_limited_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(speech_harm_limited_be_t8, speech_protection_boundary__harm_limited_reading, base_extractiveness, 8, 0.52).
narrative_ontology:measurement(speech_harm_limited_be_t16, speech_protection_boundary__harm_limited_reading, base_extractiveness, 16, 0.6).
narrative_ontology:measurement(speech_harm_limited_be_t24, speech_protection_boundary__harm_limited_reading, base_extractiveness, 24, 0.66).
narrative_ontology:measurement(speech_harm_limited_be_t32, speech_protection_boundary__harm_limited_reading, base_extractiveness, 32, 0.7).
narrative_ontology:measurement(speech_harm_limited_be_t40, speech_protection_boundary__harm_limited_reading, base_extractiveness, 40, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(speech_harm_limited_su_t0, speech_protection_boundary__harm_limited_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(speech_harm_limited_su_t8, speech_protection_boundary__harm_limited_reading, suppression_requirement, 8, 0.62).
narrative_ontology:measurement(speech_harm_limited_su_t16, speech_protection_boundary__harm_limited_reading, suppression_requirement, 16, 0.68).
narrative_ontology:measurement(speech_harm_limited_su_t24, speech_protection_boundary__harm_limited_reading, suppression_requirement, 24, 0.74).
narrative_ontology:measurement(speech_harm_limited_su_t32, speech_protection_boundary__harm_limited_reading, suppression_requirement, 32, 0.77).
narrative_ontology:measurement(speech_harm_limited_su_t40, speech_protection_boundary__harm_limited_reading, suppression_requirement, 40, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(speech_protection_boundary__harm_limited_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(speech_protection_boundary__harm_limited_reading, absolutist_reading).
narrative_ontology:affects_constraint(speech_protection_boundary__harm_limited_reading, balancing_reading).

% DUAL FORMULATION NOTE:
% This constraint is the harm_limited_reading of the speech_protection_boundary kernel, decomposed from the absolutist_reading and balancing_reading per the epsilon-invariance principle. Each reading carries a distinct epsilon and stakeholder structure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
