% ============================================================================
% CONSTRAINT STORY: imposition_mechanism_kernel__exogenous_override_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_imposition_mechanism_kernel__exogenous_override_reading, []).

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
 *   constraint_id: imposition_mechanism_kernel__exogenous_override_reading
 *   human_readable: Exogenous State Norm Imposition via Monopoly on Violence
 *   domain: historical_sociology/state_formation/cultural_authority
 *
 * SUMMARY:
 *   In processes of state formation, new legal and administrative norms are
 *   imposed from the center through the claim to a monopoly on legitimate
 *   violence. The exogenous_override_reading treats this not as a consensual
 *   social contract but as a coercive override: compliance is a function of
 *   monitoring and enforcement costs, legitimacy is contested by subjugated
 *   populations and local authorities but overridden by state capacity, and
 *   the coordination function (uniform law and order) is inseparable from
 *   asymmetric extraction (taxes, labor, autonomy). The constraint's
 *   persistence depends on active suppression of rival authorities and
 *   resistance.
 *
 * KEY AGENTS:
 *   - state_elite (institutional/mobile): agenda-setter and primary beneficiary â imposes norms and captures extraction
 *   - central_bureaucracy (organized/constrained): secondary beneficiary â administers enforcement and gains institutional position
 *   - subjugated_populations (powerless/trapped): primary target â comply under threat, bear tribute and labor extraction
 *   - local_autonomy_holders (powerful/constrained): secondary target â lose jurisdictional authority to central norms
 *   - rival_normative_authorities (organized/trapped): excluded â displaced legal systems suppressed by state override
 *   - comparative_historical_sociologist (analytical/analytical): observer â evaluates whether compliance is coerced or consensual
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(imposition_mechanism_kernel__exogenous_override_reading, 0.76).
domain_priors:suppression_score(imposition_mechanism_kernel__exogenous_override_reading, 0.82).
domain_priors:theater_ratio(imposition_mechanism_kernel__exogenous_override_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(imposition_mechanism_kernel__exogenous_override_reading, extractiveness, 0.76).
narrative_ontology:constraint_metric(imposition_mechanism_kernel__exogenous_override_reading, suppression_requirement, 0.82).
narrative_ontology:constraint_metric(imposition_mechanism_kernel__exogenous_override_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(imposition_mechanism_kernel__exogenous_override_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(imposition_mechanism_kernel__exogenous_override_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(imposition_mechanism_kernel__exogenous_override_reading, tangled_rope).
narrative_ontology:human_readable(imposition_mechanism_kernel__exogenous_override_reading, "Exogenous State Norm Imposition via Monopoly on Violence").
narrative_ontology:topic_domain(imposition_mechanism_kernel__exogenous_override_reading, "historical_sociology/state_formation/cultural_authority").

domain_priors:requires_active_enforcement(imposition_mechanism_kernel__exogenous_override_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(imposition_mechanism_kernel__exogenous_override_reading, '7dd4a7b9-645a-47fe-8479-82ef05365c34').
narrative_ontology:cs_kernel_codification('7dd4a7b9-645a-47fe-8479-82ef05365c34', formalized).
narrative_ontology:cs_authority_grounding('7dd4a7b9-645a-47fe-8479-82ef05365c34', extraction).
narrative_ontology:cs_interpretation_layer_present('7dd4a7b9-645a-47fe-8479-82ef05365c34').
narrative_ontology:cs_reading_relation('7dd4a7b9-645a-47fe-8479-82ef05365c34', imposition_mechanism_kernel__endogenous_climb_reading, coexists_with).
narrative_ontology:cs_reading_relation('7dd4a7b9-645a-47fe-8479-82ef05365c34', imposition_mechanism_kernel__hybrid_legitimation_reading, influences).
narrative_ontology:cs_axiom('7dd4a7b9-645a-47fe-8479-82ef05365c34', foundational, state_monopoly_constitutes_legitimacy).
narrative_ontology:cs_axiom_status(state_monopoly_constitutes_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('7dd4a7b9-645a-47fe-8479-82ef05365c34', state_monopoly_constitutes_legitimacy, conventional).
narrative_ontology:cs_axiom('7dd4a7b9-645a-47fe-8479-82ef05365c34', foundational, coercive_capacity_obviates_consent).
narrative_ontology:cs_axiom_status(coercive_capacity_obviates_consent, holdable).
narrative_ontology:cs_axiom_grounding('7dd4a7b9-645a-47fe-8479-82ef05365c34', coercive_capacity_obviates_consent, instrumental).
narrative_ontology:cs_reference_frame('7dd4a7b9-645a-47fe-8479-82ef05365c34', monopoly_violence_authority).
narrative_ontology:cs_drift_state('7dd4a7b9-645a-47fe-8479-82ef05365c34', post_weberian_critique, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('7dd4a7b9-645a-47fe-8479-82ef05365c34', '').
narrative_ontology:cs_kernel_id(imposition_mechanism_kernel__exogenous_override_reading, imposition_mechanism_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(imposition_mechanism_kernel__exogenous_override_reading, state_elite).
narrative_ontology:constraint_beneficiary(imposition_mechanism_kernel__exogenous_override_reading, central_bureaucracy).
narrative_ontology:constraint_victim(imposition_mechanism_kernel__exogenous_override_reading, subjugated_populations).
narrative_ontology:constraint_victim(imposition_mechanism_kernel__exogenous_override_reading, local_autonomy_holders).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Imposes new legal, fiscal, and administrative norms through centralized edicts and armed enforcement, claiming legitimacy from a monopoly on violence. Extracts tribute, labor, and political autonomy from subject populations while consolidating territorial control and eliminating competing authorities.
narrative_ontology:constraint_stakeholder(imposition_mechanism_kernel__exogenous_override_reading, state_elite, agenda_setter,
    institutional, generational, mobile, national).

% Administers and enforces the imposed norms through tax collection, conscription, and judicial oversight. Gains institutional position, salaries, and career security from the coercive apparatus; their livelihood is tied to the persistence of the override mechanism.
narrative_ontology:constraint_stakeholder(imposition_mechanism_kernel__exogenous_override_reading, central_bureaucracy, beneficiary,
    organized, generational, constrained, national).

% Comply with new tax, conscription, and legal norms under explicit threat of state violence. Alternatives such as flight, evasion, or customary practice are actively suppressed; compliance is conditional on state monitoring and enforcement presence in the territory.
narrative_ontology:constraint_stakeholder(imposition_mechanism_kernel__exogenous_override_reading, subjugated_populations, payer,
    powerless, biographical, trapped, regional).

% Traditional nobility, municipal corporations, or tribal leaders losing autonomous jurisdiction and taxation rights to central state norms. They bear the cost of diminished authority and may face armed suppression, dispossession, or legal disenfranchisement if they resist.
narrative_ontology:constraint_stakeholder(imposition_mechanism_kernel__exogenous_override_reading, local_autonomy_holders, payer,
    powerful, biographical, constrained, regional).

% Religious courts, tribal councils, merchant guilds, and customary legal systems displaced by state-imposed uniform norms. They are structurally excluded from the legitimation conversation and actively suppressed as competitors to state jurisdiction.
narrative_ontology:constraint_stakeholder(imposition_mechanism_kernel__exogenous_override_reading, rival_normative_authorities, excluded,
    organized, generational, trapped, national).

% Analyzes whether norm compliance in state-formation contexts stems from cultural acceptance or coercive monitoring. Observes that under the exogenous override reading, enforcement costs remain persistently high and legitimacy is contested but overridden by the state's capacity for violence.
narrative_ontology:constraint_stakeholder(imposition_mechanism_kernel__exogenous_override_reading, comparative_historical_sociologist, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(imposition_mechanism_kernel__exogenous_override_reading, state_elite).
narrative_ontology:fixing_cost_class(imposition_mechanism_kernel__exogenous_override_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides uniform legal, fiscal, and military coordination across a territory that would otherwise fragment into localized customary regimes; solves the collective-action problem of large-scale public order, resource mobilization, and predictability required for complex administration.
% TRANSFER_FUNCTION: Moves tribute, corvÃ©e labor, and autonomous jurisdictional authority from local communities and traditional authorities to the central state elite and its administrative apparatus.
% ABSENT_VOICES: Rival normative authorities and subjugated populations who would reject the imposed norms if not for coercion are structurally excluded from the legitimation conversation; their absence is manufactured by the suppression machinery that defines the constraint's operation.
% DISAPPEARANCE_RATIONALE: If the state coercive apparatus and its imposed norms vanished overnight, local autonomous authorities would reassert jurisdiction, subjugated populations would revert to customary practice, and the centralized resource extraction and legal uniformity would collapse into fragmented regional regimes.
% FOUNDING_PROBLEM: Territorial fragmentation under customary law and local autonomy prevented centralized resource extraction, large-scale military coordination, and uniform legal predictability required for state consolidation and external competition.
% FOUNDING_PROBLEM_CORROBORATION: State chronicles and elite archives attest the fragmentation problem from the beneficiary side. Comparative historical sociologists such as Tilly and Mann corroborate the coordination problem from outside the benefiting parties, though they contest whether the coercive solution remains necessary or legitimate once territorial consolidation is achieved.
narrative_ontology:disappearance_verdict(imposition_mechanism_kernel__exogenous_override_reading, world_rearranges).
narrative_ontology:founding_problem_status(imposition_mechanism_kernel__exogenous_override_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(imposition_mechanism_kernel__exogenous_override_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(imposition_mechanism_kernel__exogenous_override_reading, 'none', 1).
narrative_ontology:epsilon_provenance(imposition_mechanism_kernel__exogenous_override_reading, 0.76, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(imposition_mechanism_kernel__exogenous_override_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(imposition_mechanism_kernel__exogenous_override_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(imposition_mechanism_kernel__exogenous_override_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.76) because the state extracts material surplus, labor, and political autonomy from subject populations; suppression is higher (0.82) because the constraint's persistence requires active repression of alternative normative orders and periodic resistance. Theater_ratio is moderate (0.30) because state imposition includes performative displays of power (ceremonial law, public punishment) but relies heavily on functional enforcement infrastructure. Accessibility_collapse is substantial (0.70) because state consolidation systematically displaces customary alternatives, though remnants persist in covert practice. Resistance (0.55) reflects endemic but suppressed peasant and elite rebellions against centralization.
 *
 * PERSPECTIVAL GAP:
 *   The state elite and bureaucracy experience the constraint as necessary coordination and legitimate authority, whereas subjugated populations and local autonomy holders experience it as domination. The engine computes this divergence from structural data: the agenda-setter has mobile exit and generational time horizon, while payers are trapped or constrained with biographical horizons. The analytical observer seat sees the full asymmetry.
 *
 * DIRECTIONALITY LOGIC:
 *   State elite and central bureaucracy are declared beneficiaries with institutional power and mobile or constrained exit â their directionality sits near the beneficiary pole (low d). Subjugated populations and local autonomy holders are declared victims with low power and trapped or constrained exit â their directionality sits near the target pole (high d). The asymmetry is intentional: the same norm imposition extracts differentially based on power and exit options.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem â territorial fragmentation and inability to mobilize resources for large-scale coordination â may eventually be solved, yet the coercive apparatus persists. However, this reading does not claim the mandate has fully atrophied; it claims the mechanism remains coercive regardless of founding-problem status. The tangled_rope classification prevents mislabeling: there is genuine coordination (public order, standard weights and measures), but it is not a Rope because the enforcement costs are high and extraction is asymmetric. It is not a Snare because the coordination function is real and not merely cover. The Tangled Rope captures the hybrid reality of state formation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    exogenous_vs_endogenous_framing,
    'Does the norm''s persistence depend on continued coercive enforcement, or has it been internalized such that the exogenous override reading mischaracterizes a now-endogenous legitimacy?',
    'Longitudinal compliance studies measuring norm adherence before and after state enforcement capacity collapses or withdraws from a region.',
    'If compliance collapses with enforcement, the exogenous reading holds and the constraint remains highly extractive; if compliance persists, the endogenous or hybrid readings gain validity and effective extraction is lower than structural measures suggest.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(exogenous_vs_endogenous_framing, empirical, 'Whether norm compliance is truly coercive or has become culturally embedded').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (state military and police apparatus) or internalized (subject populations believing state authority is natural or legitimate)?',
    'Post-regime-collapse ethnography: if compliance and deference persist after state enforcement disappears, suppression was partially internalized; if immediate norm rejection occurs, suppression was structural.',
    'If internalized, effective extraction is higher than structural measures suggest because the target population carries the constraint after exit; if structural, extraction is conditional on visible enforcement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs internalized suppression in state norm imposition').

omega_variable(
    public_order_vs_domination,
    'To what extent does the state-imposed norm solve a genuine large-scale coordination problem versus serving as a vehicle for elite extraction?',
    'Comparative analysis of stateless vs state-governed regions on public goods provision, controlling for extraction levels.',
    'High genuine coordination would support a tangled_rope classification; negligible coordination with high extraction would support reclassification toward snare.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(public_order_vs_domination, conceptual, 'Coordination function versus extraction in state norm imposition').

omega_variable(
    kernel_reading_boundary,
    'Does this constraint''s high extractiveness reflect the essential nature of state norm imposition, or is it an artifact of the exogenous_override reading''s selective attention to coercion while backgrounding consent?',
    'Cross-reading comparison: if the endogenous_climb_reading of the same kernel shows substantially lower extractiveness and suppression, the difference is a reading effect; if both readings show high extraction, the kernel itself is structurally extractive.',
    'If the difference is reading-dependent, the kernel should be analyzed as a family with divergent epsilon values; if kernel-invariant, all readings converge on high extraction and the coordination story is cover.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_boundary, conceptual, 'Whether extraction is kernel-invariant or reading-dependent').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(imposition_mechanism_kernel__exogenous_override_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ime_tr_t0, imposition_mechanism_kernel__exogenous_override_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(ime_tr_t12, imposition_mechanism_kernel__exogenous_override_reading, theater_ratio, 12, 0.18).
narrative_ontology:measurement(ime_tr_t24, imposition_mechanism_kernel__exogenous_override_reading, theater_ratio, 24, 0.22).
narrative_ontology:measurement(ime_tr_t36, imposition_mechanism_kernel__exogenous_override_reading, theater_ratio, 36, 0.25).
narrative_ontology:measurement(ime_tr_t48, imposition_mechanism_kernel__exogenous_override_reading, theater_ratio, 48, 0.28).
narrative_ontology:measurement(ime_tr_t60, imposition_mechanism_kernel__exogenous_override_reading, theater_ratio, 60, 0.3).

% Extraction over time
narrative_ontology:measurement(ime_be_t0, imposition_mechanism_kernel__exogenous_override_reading, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(ime_be_t12, imposition_mechanism_kernel__exogenous_override_reading, base_extractiveness, 12, 0.6).
narrative_ontology:measurement(ime_be_t24, imposition_mechanism_kernel__exogenous_override_reading, base_extractiveness, 24, 0.68).
narrative_ontology:measurement(ime_be_t36, imposition_mechanism_kernel__exogenous_override_reading, base_extractiveness, 36, 0.72).
narrative_ontology:measurement(ime_be_t48, imposition_mechanism_kernel__exogenous_override_reading, base_extractiveness, 48, 0.75).
narrative_ontology:measurement(ime_be_t60, imposition_mechanism_kernel__exogenous_override_reading, base_extractiveness, 60, 0.76).

% Suppression requirement over time
narrative_ontology:measurement(ime_su_t0, imposition_mechanism_kernel__exogenous_override_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(ime_su_t12, imposition_mechanism_kernel__exogenous_override_reading, suppression_requirement, 12, 0.6).
narrative_ontology:measurement(ime_su_t24, imposition_mechanism_kernel__exogenous_override_reading, suppression_requirement, 24, 0.7).
narrative_ontology:measurement(ime_su_t36, imposition_mechanism_kernel__exogenous_override_reading, suppression_requirement, 36, 0.76).
narrative_ontology:measurement(ime_su_t48, imposition_mechanism_kernel__exogenous_override_reading, suppression_requirement, 48, 0.8).
narrative_ontology:measurement(ime_su_t60, imposition_mechanism_kernel__exogenous_override_reading, suppression_requirement, 60, 0.82).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(imposition_mechanism_kernel__exogenous_override_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(imposition_mechanism_kernel__exogenous_override_reading, endogenous_climb_reading).
narrative_ontology:affects_constraint(imposition_mechanism_kernel__exogenous_override_reading, hybrid_legitimation_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the imposition_mechanism_kernel, which decomposes the natural-language concept of 'how new norms achieve legitimacy in state formation' into three structurally distinct claims. The exogenous_override_reading posits that legitimacy derives from monopoly on violence and coercive imposition, whereas the endogenous_climb_reading posits bottom-up cultural adoption, and the hybrid_legitimation_reading combines symbolic authority with institutional incentives. Their epsilon values and stakeholder structures differ substantially.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
