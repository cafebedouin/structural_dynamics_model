% ============================================================================
% CONSTRAINT STORY: state_commitment_installation_mechanism__exogenous_imposition_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_state_commitment_installation_mechanism__exogenous_imposition_reading, []).

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
 *   constraint_id: state_commitment_installation_mechanism__exogenous_imposition_reading
 *   human_readable: Top-Down State Commitment Installation (Exogenous Imposition Reading)
 *   domain: historical_sociology/state_formation/cultural_authority
 *
 * SUMMARY:
 *   This constraint story models the exogenous_imposition_reading of the
 *   state commitment installation mechanism kernel: the thesis that new
 *   normative, legal, or cultural commitments acquire legitimacy primarily
 *   through top-down decree by a state authority claiming a transformation
 *   mandate. The state installs commitments abruptly, without grassroots
 *   advocacy, and enforces them against base-level resistance. The reading
 *   treats the state as the structural beneficiary of this mechanism,
 *   extracting compliance and centralized legitimacy, while local populations
 *   and customary institutions bear the costs of abrupt adoption. This is one
 *   of three contested readings of the kernel; the endogenous_climb and
 *   hybrid_cascade readings are structurally excluded here and treated as
 *   sibling constraints.
 *
 * KEY AGENTS:
 *   - state_authority: Primary beneficiary and agenda-setter (institutional/arbitrage) â holds the transformation mandate, issues decrees, and accrues centralized legitimacy.
 *   - administrative_elite: Secondary beneficiary (powerful/mobile) â implements decrees and gains expanded bureaucratic remit.
 *   - subject_population: Primary target and payer (powerless/trapped) â must comply with abruptly imposed commitments, bearing behavioral and cultural costs.
 *   - local_customary_institutions: Secondary target and payer (moderate/constrained) â lose public normative authority to state decrees.
 *   - comparative_historical_analyst: Analytical observer (analytical/analytical) â evaluates patterns across state formation cases.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(state_commitment_installation_mechanism__exogenous_imposition_reading, 0.72).
domain_priors:suppression_score(state_commitment_installation_mechanism__exogenous_imposition_reading, 0.78).
domain_priors:theater_ratio(state_commitment_installation_mechanism__exogenous_imposition_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(state_commitment_installation_mechanism__exogenous_imposition_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(state_commitment_installation_mechanism__exogenous_imposition_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(state_commitment_installation_mechanism__exogenous_imposition_reading, theater_ratio, 0.35).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(state_commitment_installation_mechanism__exogenous_imposition_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(state_commitment_installation_mechanism__exogenous_imposition_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(state_commitment_installation_mechanism__exogenous_imposition_reading, tangled_rope).
narrative_ontology:human_readable(state_commitment_installation_mechanism__exogenous_imposition_reading, "Top-Down State Commitment Installation (Exogenous Imposition Reading)").
narrative_ontology:topic_domain(state_commitment_installation_mechanism__exogenous_imposition_reading, "historical_sociology/state_formation/cultural_authority").

domain_priors:requires_active_enforcement(state_commitment_installation_mechanism__exogenous_imposition_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(state_commitment_installation_mechanism__exogenous_imposition_reading, '12d8b721-2216-41d8-8001-a3e874158fff').
narrative_ontology:cs_kernel_codification('12d8b721-2216-41d8-8001-a3e874158fff', implicit).
narrative_ontology:cs_authority_grounding('12d8b721-2216-41d8-8001-a3e874158fff', extraction).
narrative_ontology:cs_interpretation_layer_present('12d8b721-2216-41d8-8001-a3e874158fff').
narrative_ontology:cs_reading_relation('12d8b721-2216-41d8-8001-a3e874158fff', state_commitment_installation_mechanism__endogenous_climb_reading, forecloses).
narrative_ontology:cs_reading_relation('12d8b721-2216-41d8-8001-a3e874158fff', state_commitment_installation_mechanism__hybrid_cascade_reading, influences).
narrative_ontology:cs_axiom('12d8b721-2216-41d8-8001-a3e874158fff', foundational, legitimacy_from_transformation_mandate).
narrative_ontology:cs_axiom_status(legitimacy_from_transformation_mandate, holdable).
narrative_ontology:cs_axiom_grounding('12d8b721-2216-41d8-8001-a3e874158fff', legitimacy_from_transformation_mandate, conventional).
narrative_ontology:cs_axiom('12d8b721-2216-41d8-8001-a3e874158fff', foundational, popular_consent_obviated_by_decree).
narrative_ontology:cs_axiom_status(popular_consent_obviated_by_decree, holdable).
narrative_ontology:cs_axiom_grounding('12d8b721-2216-41d8-8001-a3e874158fff', popular_consent_obviated_by_decree, conventional).
narrative_ontology:cs_reference_frame('12d8b721-2216-41d8-8001-a3e874158fff', centralized_transformation_authority).
narrative_ontology:cs_drift_state('12d8b721-2216-41d8-8001-a3e874158fff', post_empirical_challenge, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('12d8b721-2216-41d8-8001-a3e874158fff', '').
narrative_ontology:cs_kernel_id(state_commitment_installation_mechanism__exogenous_imposition_reading, state_commitment_installation_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(state_commitment_installation_mechanism__exogenous_imposition_reading, state_authority).
narrative_ontology:constraint_beneficiary(state_commitment_installation_mechanism__exogenous_imposition_reading, administrative_elite).
narrative_ontology:constraint_victim(state_commitment_installation_mechanism__exogenous_imposition_reading, subject_population).
narrative_ontology:constraint_victim(state_commitment_installation_mechanism__exogenous_imposition_reading, local_customary_institutions).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds the transformation mandate and issues decrees installing new commitments. Derives legitimacy from the mandate itself and from successful imposition. Can alter or abandon the mechanism if it serves state interests.
narrative_ontology:constraint_stakeholder(state_commitment_installation_mechanism__exogenous_imposition_reading, state_authority, agenda_setter,
    institutional, generational, arbitrage, national).

% Implements and administers the top-down installation. Benefits from expanded bureaucratic remit, career advancement, and resource flows attached to the transformation program. Can transfer to other administrative roles or jurisdictions.
narrative_ontology:constraint_stakeholder(state_commitment_installation_mechanism__exogenous_imposition_reading, administrative_elite, beneficiary,
    powerful, generational, mobile, national).

% Receives new commitments by decree. Must comply or face sanctions. Bears the costs of abrupt behavioral change, disrupted local practices, and suppressed dissent. Exit is limited to covert non-compliance or migration if possible.
narrative_ontology:constraint_stakeholder(state_commitment_installation_mechanism__exogenous_imposition_reading, subject_population, payer,
    powerless, biographical, trapped, local).

% Traditional councils, religious authorities, or kinship structures whose norms are overridden by state decrees. Lose jurisdictional authority and normative influence. Can retreat to informal spheres but are barred from public legitimacy.
narrative_ontology:constraint_stakeholder(state_commitment_installation_mechanism__exogenous_imposition_reading, local_customary_institutions, payer,
    moderate, generational, constrained, local).

% Studies patterns of state formation and commitment installation across cases. Neither benefits nor pays within the constraint.
narrative_ontology:constraint_stakeholder(state_commitment_installation_mechanism__exogenous_imposition_reading, comparative_historical_analyst, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the creation of a unified national normative, legal, or cultural field by overriding fragmented local practices through centralized decree, solving the fragmentation and potential disorder that the state claims would result from bottom-up negotiation.
% TRANSFER_FUNCTION: Moves compliance, labor, tax obedience, and normative adherence from local populations and customary institutions to the state authority and its administrative apparatus; also transfers legitimacy upward so that the state's decree is treated as the source of valid commitment.
% ABSENT_VOICES: Grassroots advocates, local customary leaders not co-opted by the state, and endogenous institutional innovators are structurally excluded; they would argue for incremental, practice-validated legitimacy but are silenced by the decree mechanism that recognizes only top-down authorization.
% DISAPPEARANCE_RATIONALE: If the top-down installation mechanism disappeared overnight, state capacity to unilaterally transform commitments would collapse; local customary institutions would resurface as public normative authorities, the administrative elite's transformation remit would evaporate, and the subject population's compliance obligations would revert to local negotiation or alternative legitimacy sources.
% FOUNDING_PROBLEM: Fragmented authority and heterogeneous local commitments impede centralized state-building, nation-building, or rapid modernization in the face of external military-economic competition or internal disorder.
% FOUNDING_PROBLEM_CORROBORATION: State historians and administrative elites attest to the necessity of centralization. Comparative historical sociologists and subaltern studies scholars from outside the benefiting parties argue the fragmentation was often manageable through local coordination and that the 'disorder' was manufactured or exaggerated to justify state expansion; corroboration is split and contested.
narrative_ontology:disappearance_verdict(state_commitment_installation_mechanism__exogenous_imposition_reading, world_rearranges).
narrative_ontology:founding_problem_status(state_commitment_installation_mechanism__exogenous_imposition_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(state_commitment_installation_mechanism__exogenous_imposition_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(state_commitment_installation_mechanism__exogenous_imposition_reading, 'none', 1).
narrative_ontology:epsilon_provenance(state_commitment_installation_mechanism__exogenous_imposition_reading, 0.72, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(state_commitment_installation_mechanism__exogenous_imposition_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(state_commitment_installation_mechanism__exogenous_imposition_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(state_commitment_installation_mechanism__exogenous_imposition_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness is high (0.72) because the mechanism concentrates legitimacy and compliance at the center while displacing local normative orders; suppression is higher (0.78) because the abrupt, non-consensual nature of the installation requires active coercive enforcement and exclusion of grassroots alternatives. Theater ratio is moderate (0.35) because much of the state activity is functional (tax collection, legal unification), but a growing share is performative display of the mandate (pageantry, ideological education) as resistance persists. Accessibility collapse is substantial (0.65): once the state commits to top-down installation, local alternatives are driven underground or delegitimized, though not fully eradicated. Resistance is moderate-high (0.58) because base-level resistance is structurally expected and documented. The temporal series show extraction and suppression ratcheting upward as the state consolidates, with theater increasing as performative maintenance intensifies.
 *
 * PERSPECTIVAL GAP:
 *   The state_authority seat experiences the constraint as necessary coordination: overcoming fragmentation, modernizing society, and preventing chaos. The subject_population and local_customary_institutions seats experience the same structure as asymmetric extraction: their practices are criminalized or delegitimized without their consent. The administrative_elite occupies an intermediate position, experiencing both coordination (career rewards) and extraction (dependence on the state's continued imposition project). The engine computes this divergence from structural data rather than reconciling it.
 *
 * DIRECTIONALITY LOGIC:
 *   state_authority is declared as beneficiary with arbitrage-grade exit (can alter strategy, abandon the mandate, or switch to hybrid techniques), placing it near the full-beneficiary end (low d). administrative_elite is a beneficiary with mobile exit, also low d. subject_population is a victim/payer with trapped exit, placing it near the full-target end (high d). local_customary_institutions are victims/payers with constrained exit (can retreat to informal spheres but lose public standing), also high d. No override is needed because beneficiary/victim declarations plus exit options accurately map the structural relationships.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading prevents mislabeling by requiring both a coordination function (state-building, legal unification, transformation) and asymmetric extraction (compliance costs on base, suppression of customary institutions). Without the coordination gate, pure resistance might be read as a snare; without the victim gate, state centralization might be read as a rope. The Tangled Rope classification captures that the mechanism genuinely coordinates (a national legal field, a modern bureaucracy) but only by extracting from and suppressing local alternatives. If the transformation mandate were dead but the installation mechanism persisted purely by inertia, it would drift toward piton; the measurements show active enforcement intensifying, indicating the mandate is still live.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reading_sibling_delta,
    'How would the classification change if the endogenous_climb_reading or hybrid_cascade_reading were adopted as the operative mechanism?',
    'Comparative case analysis across state formation episodes where bottom-up legitimation versus hybrid mechanisms were operative.',
    'Would shift beneficiary/victim structure and directionality: endogenous climb positions grassroots innovators as beneficiaries and state authority as paying attention/resources; hybrid distributes extraction more diffusely.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_sibling_delta, conceptual, 'Sibling reading structural delta uncertainty').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (coercive enforcement by state apparatus) or internalized (populace acceptance of the transformation mandate as legitimate)?',
    'Post-reform or post-revolutionary regime-change studies: does compliance persist when the coercive apparatus is removed?',
    'If internalized, effective extraction is higher than structural measure suggests; if purely structural, the constraint may collapse rapidly upon enforcement decay.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs internalized suppression mechanism').

omega_variable(
    founding_problem_genuineness,
    'Was the fragmentation or backwardness the transformation mandate addressed a genuine coordination problem, or manufactured to justify extraction?',
    'Archival and ethnographic recovery of pre-imposition local institutions: were they functioning coordination mechanisms prior to state disruption?',
    'If genuine, the constraint is tangled_rope; if manufactured, it reclassifies toward snare.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(founding_problem_genuineness, empirical, 'Whether the founding coordination problem was real or constructed').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(state_commitment_installation_mechanism__exogenous_imposition_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stat_tr_t0, state_commitment_installation_mechanism__exogenous_imposition_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(stat_tr_t10, state_commitment_installation_mechanism__exogenous_imposition_reading, theater_ratio, 10, 0.23).
narrative_ontology:measurement(stat_tr_t20, state_commitment_installation_mechanism__exogenous_imposition_reading, theater_ratio, 20, 0.27).
narrative_ontology:measurement(stat_tr_t30, state_commitment_installation_mechanism__exogenous_imposition_reading, theater_ratio, 30, 0.31).
narrative_ontology:measurement(stat_tr_t40, state_commitment_installation_mechanism__exogenous_imposition_reading, theater_ratio, 40, 0.33).
narrative_ontology:measurement(stat_tr_t50, state_commitment_installation_mechanism__exogenous_imposition_reading, theater_ratio, 50, 0.35).

% Extraction over time
narrative_ontology:measurement(stat_be_t0, state_commitment_installation_mechanism__exogenous_imposition_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(stat_be_t10, state_commitment_installation_mechanism__exogenous_imposition_reading, base_extractiveness, 10, 0.55).
narrative_ontology:measurement(stat_be_t20, state_commitment_installation_mechanism__exogenous_imposition_reading, base_extractiveness, 20, 0.63).
narrative_ontology:measurement(stat_be_t30, state_commitment_installation_mechanism__exogenous_imposition_reading, base_extractiveness, 30, 0.68).
narrative_ontology:measurement(stat_be_t40, state_commitment_installation_mechanism__exogenous_imposition_reading, base_extractiveness, 40, 0.71).
narrative_ontology:measurement(stat_be_t50, state_commitment_installation_mechanism__exogenous_imposition_reading, base_extractiveness, 50, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(stat_su_t0, state_commitment_installation_mechanism__exogenous_imposition_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(stat_su_t10, state_commitment_installation_mechanism__exogenous_imposition_reading, suppression_requirement, 10, 0.6).
narrative_ontology:measurement(stat_su_t20, state_commitment_installation_mechanism__exogenous_imposition_reading, suppression_requirement, 20, 0.68).
narrative_ontology:measurement(stat_su_t30, state_commitment_installation_mechanism__exogenous_imposition_reading, suppression_requirement, 30, 0.74).
narrative_ontology:measurement(stat_su_t40, state_commitment_installation_mechanism__exogenous_imposition_reading, suppression_requirement, 40, 0.77).
narrative_ontology:measurement(stat_su_t50, state_commitment_installation_mechanism__exogenous_imposition_reading, suppression_requirement, 50, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(state_commitment_installation_mechanism__exogenous_imposition_reading, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
