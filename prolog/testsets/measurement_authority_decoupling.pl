% ============================================================================
% CONSTRAINT STORY: measurement_authority_decoupling
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_measurement_authority_decoupling, []).

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
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: measurement_authority_decoupling
 *   human_readable: Ritualized Measurement Decoupled from Selection Authority
 *   domain: organizational/institutional
 *
 * SUMMARY:
 *   An institution operates a genuinely accurate, publicly ritualized
 *   measurement instrument — the readings are true, technically verifiable,
 *   and produced with real integrity by the technicians who run it. Selection
 *   decisions (who is protected, promoted, or destroyed) occur in a separate
 *   process controlled by the authority, and outcome data collected over the
 *   interval shows zero statistical correlation between measurement readings
 *   and selection outcomes, despite both occurring within the same ceremonial
 *   and institutional register. The measurement ritual's coordination
 *   function (producing a credible public record) is real; the extraction is
 *   that this credibility is borrowed to legitimate a selection process it
 *   does not actually govern. This is the tangled rope's defining feature: a
 *   genuine coordination mechanism (accurate measurement) coexisting with,
 *   and providing cover for, a distinct extractive mechanism (discretionary
 *   selection insulated from that measurement).
 *
 * KEY AGENTS:
 *   - authority_that_controls_selection_criteria: primary beneficiary (institutional/arbitrage) — controls both ritual and hidden decision, collects legitimacy without accountability
 *   - those_selected_without_recourse_to_measurable_merit: primary target (powerless/trapped) — bears the cost of a decision the ritual cannot be used to contest
 *   - measurement_technicians: secondary actor (moderate/constrained) — supplies genuine technical integrity that is then borrowed for cover
 *   - outcome_observers: analytical observer (analytical/analytical) — sees the zero-correlation structure the institution's own reporting does not surface
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(measurement_authority_decoupling, 0.71).
domain_priors:suppression_score(measurement_authority_decoupling, 0.68).
domain_priors:theater_ratio(measurement_authority_decoupling, 0.79).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(measurement_authority_decoupling, extractiveness, 0.71).
narrative_ontology:constraint_metric(measurement_authority_decoupling, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(measurement_authority_decoupling, theater_ratio, 0.79).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(measurement_authority_decoupling, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(measurement_authority_decoupling, resistance, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(measurement_authority_decoupling, tangled_rope).
narrative_ontology:human_readable(measurement_authority_decoupling, "Ritualized Measurement Decoupled from Selection Authority").
narrative_ontology:topic_domain(measurement_authority_decoupling, "organizational/institutional").

domain_priors:requires_active_enforcement(measurement_authority_decoupling).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(measurement_authority_decoupling, authority_that_controls_selection_criteria).
narrative_ontology:constraint_victim(measurement_authority_decoupling, those_selected_without_recourse_to_measurable_merit).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(measurement_authority_decoupling, those_selected_by_favorable_undisclosed_criteria).
narrative_ontology:constraint_vindicates(measurement_authority_decoupling, meritocratic_selection_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Commissions and publicly displays a perfectly accurate measurement instrument — the weighing, scoring, or ranking ceremony is genuine and technically precise. Separately, and without published linkage, decides who is protected, promoted, or destroyed using undisclosed criteria. Retains discretion by never formally tying the two processes together, so the accurate numbers cannot be cited against any specific outcome.
narrative_ontology:constraint_stakeholder(measurement_authority_decoupling, authority_that_controls_selection_criteria, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(measurement_authority_decoupling, authority_that_controls_selection_criteria, beneficiary).

% Operate the instrument with genuine technical integrity and take professional pride in its accuracy. Have no visibility into, and no input on, how or whether their readings inform the actual selection decisions made elsewhere in the institution. Their expertise legitimizes the ritual but does not extend to the decision that matters.
narrative_ontology:constraint_stakeholder(measurement_authority_decoupling, measurement_technicians, agenda_setter,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(measurement_authority_decoupling, measurement_technicians, excluded).

% Undergo the measurement ritual, receive an accurate score, and are then selected for exclusion or destruction by a decision that does not correlate with that score. Cannot appeal on the basis of measured merit because the appeal channel only recognizes the measurement, not the actual decision logic. Experience the ritual as procedurally fair and the outcome as arbitrary, with no bridge between the two.
narrative_ontology:constraint_stakeholder(measurement_authority_decoupling, those_selected_without_recourse_to_measurable_merit, payer,
    powerless, biographical, trapped, local).

% Are protected or advanced by the undisclosed selection process regardless of their measured score. Some genuinely score well and experience the system as validating; others score poorly but are protected anyway and have strong incentive never to draw attention to the decoupling, since the ritual's legitimacy also legitimizes their own favorable treatment.
narrative_ontology:constraint_stakeholder(measurement_authority_decoupling, those_selected_by_favorable_undisclosed_criteria, beneficiary,
    moderate, biographical, constrained, local).

% Are permitted to audit the measurement instrument's accuracy — and confirm it is genuinely precise — but are denied access to the separate selection-decision process, which is classified as discretionary judgment outside audit scope. Their clean audit of the measurement ritual is then cited by the authority as evidence the whole system is fair.
narrative_ontology:constraint_stakeholder(measurement_authority_decoupling, external_auditors, excluded,
    organized, biographical, constrained, national).

% Track the statistical relationship between measurement readings and selection outcomes over time, from outside the institution. Their data shows the zero-correlation structure that the institution's internal reporting never surfaces.
narrative_ontology:constraint_stakeholder(measurement_authority_decoupling, outcome_observers, observer,
    analytical, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(measurement_authority_decoupling, authority_that_controls_selection_criteria).
narrative_ontology:fixing_cost_class(measurement_authority_decoupling, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The measurement ritual genuinely solves an information problem: it produces a public, technically accurate, verifiable record of a quantity that would otherwise be disputed or invisible. This coordination function is real and independently valuable.
% TRANSFER_FUNCTION: Legitimacy generated by the accurate, publicly witnessed measurement ritual is transferred to cover selection decisions that are actually made on separate, undisclosed grounds — moving the appearance of merit-based fairness from the measurement process to outcomes the measurement does not determine.
% ABSENT_VOICES: Those selected against have no seat in the room where selection criteria are actually set; measurement technicians, whose integrity underwrites the ritual's credibility, have no visibility into how their numbers are or are not used downstream.
% DISAPPEARANCE_RATIONALE: If the measurement ritual disappeared, the authority would lose its primary legitimating cover and would either have to disclose actual selection criteria (triggering contestation) or invent a replacement ritual. If instead the DECOUPLING disappeared (i.e., the measurement actually determined outcomes), the authority would lose its current discretionary control entirely — selection would become contestable and predictable, which is precisely what the decoupling exists to prevent.
% FOUNDING_PROBLEM: The institution needed a publicly credible, dispute-resistant way to demonstrate that selection was principled rather than arbitrary, in a context where actual outcomes had to remain flexible to competing internal pressures the institution did not want to expose.
% FOUNDING_PROBLEM_CORROBORATION: The authority attests the measurement ritual demonstrates ongoing commitment to merit and that selection judgment must remain discretionary to handle cases the instrument cannot capture. External auditors and outcome observers, from outside the beneficiary set, attest via longitudinal correlation data that the founding problem (demonstrating principled selection) is not being solved by the current arrangement — the ritual and the decision have been empirically decoupled for the full observed interval.
narrative_ontology:disappearance_verdict(measurement_authority_decoupling, world_rearranges).
narrative_ontology:founding_problem_status(measurement_authority_decoupling, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(measurement_authority_decoupling, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-09',
    'uke_narrative', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'temperature=0.2,max_tokens=8192').
narrative_ontology:story_seed(measurement_authority_decoupling, 'none', 1).
narrative_ontology:epsilon_provenance(measurement_authority_decoupling, 0.71, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(measurement_authority_decoupling_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(measurement_authority_decoupling, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(measurement_authority_decoupling_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises from 0.42 to 0.71 over the interval as the gap between measurement ritual and actual selection outcome widens and becomes more entrenched. Theater ratio starts already high (0.48) — the ritual was never purely functional — and climbs to 0.79 as an increasing share of institutional activity becomes about maintaining the appearance of measurement-driven selection rather than the measurement itself. Suppression tracks the enforcement needed to keep the two processes formally separate: audit scope must be actively defined to exclude the selection decision, and appeal channels must be actively restricted to measurement-only grounds. Accessibility collapse is moderate (0.58), not extreme, because the measurement data itself remains visible and auditable — what collapses is the ability to use it. Resistance (0.52) reflects real but partially blunted pushback from those selected against, whose grievances are absorbed by a functioning appeals process that only ever adjudicates measurement accuracy, never selection logic.
 *
 * PERSPECTIVAL GAP:
 *   From the authority's seat, this looks like a rope: a well-functioning, accurate measurement system operating alongside necessary human judgment. From the seat of those selected against, the same structure looks like a snare: an accurate-seeming ritual that provides zero actual protection. The engine's tangled-rope computation should reflect that both readings are structurally correct simultaneously — the coordination function (accurate measurement) and the extraction function (unaccountable selection) are both real and co-located in the same institutional apparatus, which is exactly the tangled rope signature rather than either pure type.
 *
 * DIRECTIONALITY LOGIC:
 *   The authority sits at the extreme beneficiary end: it designs both processes, controls their non-linkage, and collects the legitimacy dividend from the ritual without bearing the accountability cost the ritual implies. Those selected without recourse sit at the extreme target end: trapped, powerless, and structurally denied the one channel (measured merit) that could contest their outcome. Those selected by favorable undisclosed criteria are beneficiaries but at moderate power — they benefit passively and have every incentive to preserve the decoupling, since their own protection depends on it remaining invisible. Measurement technicians are structurally ambiguous: they are neither beneficiaries nor victims of the decoupling itself, but their professional integrity is the resource being extracted for legitimacy — hence agenda_setter (over the measurement) with excluded as secondary (from the decision that matters).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — demonstrating principled, non-arbitrary selection — is genuinely still live as an institutional need (organizations still need legitimacy for consequential decisions), so this is not simple mandatrophy where the function has vanished. Instead the mandate has been captured: the FORM that once served the function (public accurate measurement) persists and even intensifies, while the function itself (measurement actually determining outcomes) has been severed. Classifying this as tangled_rope rather than pure snare preserves the fact that the measurement instrument is not fraudulent — it is real, accurate, and audited — while still registering that its coordination value has been decoupled from and instrumentalized by an independent extractive selection process.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    decoupling_intentionality,
    'Was the zero-correlation structure designed deliberately by the authority, or did it emerge as an unintended drift between two originally linked processes?',
    'Internal institutional records or testimony regarding the original design intent of the measurement-to-selection pipeline, and any documented point at which linkage was formally severed or allowed to lapse.',
    'Deliberate design would support classification closer to snare (extraction is the primary function, coordination is manufactured cover); unintended drift would support tangled_rope with a stronger emphasis on institutional inertia and lower culpability, closer to piton dynamics over time.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(decoupling_intentionality, empirical, 'Whether the measurement/selection decoupling was designed or drifted.').

omega_variable(
    measurement_naturalness_boundary,
    'Is the measurement instrument''s technical accuracy itself evidence that the overall system is fundamentally sound (a mountain-like natural-fact anchor within the system), or is even the instrument''s precision instrumentalized as part of the extraction — i.e., is precision maintained specifically because it makes the decoupling harder to detect?',
    'Compare institutional investment in measurement precision against institutional investment in linking measurement to outcomes; disproportionate investment in precision-without-linkage would support the instrumentalization reading.',
    'If precision is maintained partly to obscure the decoupling, the measurement ritual itself should be read as an active extraction instrument, not a neutral coordination mechanism riding alongside extraction — this would push the classification toward a more extraction-dominant tangled rope or even snare.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(measurement_naturalness_boundary, conceptual, 'Whether measurement precision is neutral coordination or itself weaponized for cover.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(measurement_authority_decoupling, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(meas_tr_t0, measurement_authority_decoupling, theater_ratio, 0, 0.48).
narrative_ontology:measurement(meas_tr_t4, measurement_authority_decoupling, theater_ratio, 4, 0.56).
narrative_ontology:measurement(meas_tr_t8, measurement_authority_decoupling, theater_ratio, 8, 0.63).
narrative_ontology:measurement(meas_tr_t12, measurement_authority_decoupling, theater_ratio, 12, 0.69).
narrative_ontology:measurement(meas_tr_t16, measurement_authority_decoupling, theater_ratio, 16, 0.73).
narrative_ontology:measurement(meas_tr_t20, measurement_authority_decoupling, theater_ratio, 20, 0.76).
narrative_ontology:measurement(meas_tr_t24, measurement_authority_decoupling, theater_ratio, 24, 0.79).

% Extraction over time
narrative_ontology:measurement(meas_be_t0, measurement_authority_decoupling, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(meas_be_t4, measurement_authority_decoupling, base_extractiveness, 4, 0.49).
narrative_ontology:measurement(meas_be_t8, measurement_authority_decoupling, base_extractiveness, 8, 0.55).
narrative_ontology:measurement(meas_be_t12, measurement_authority_decoupling, base_extractiveness, 12, 0.61).
narrative_ontology:measurement(meas_be_t16, measurement_authority_decoupling, base_extractiveness, 16, 0.65).
narrative_ontology:measurement(meas_be_t20, measurement_authority_decoupling, base_extractiveness, 20, 0.68).
narrative_ontology:measurement(meas_be_t24, measurement_authority_decoupling, base_extractiveness, 24, 0.71).

% Suppression requirement over time
narrative_ontology:measurement(meas_su_t0, measurement_authority_decoupling, suppression_requirement, 0, 0.44).
narrative_ontology:measurement(meas_su_t4, measurement_authority_decoupling, suppression_requirement, 4, 0.5).
narrative_ontology:measurement(meas_su_t8, measurement_authority_decoupling, suppression_requirement, 8, 0.55).
narrative_ontology:measurement(meas_su_t12, measurement_authority_decoupling, suppression_requirement, 12, 0.59).
narrative_ontology:measurement(meas_su_t16, measurement_authority_decoupling, suppression_requirement, 16, 0.63).
narrative_ontology:measurement(meas_su_t20, measurement_authority_decoupling, suppression_requirement, 20, 0.66).
narrative_ontology:measurement(meas_su_t24, measurement_authority_decoupling, suppression_requirement, 24, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(measurement_authority_decoupling, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(measurement_authority_decoupling, 0.1).

% DUAL FORMULATION NOTE:
% This story treats the measurement ritual and the selection decision as a single constraint (measurement_authority_decoupling) rather than decomposing into two stories, because the extraction is precisely the RELATIONSHIP (or non-relationship) between the two processes, not either process viewed in isolation. Decomposing into 'measurement_accuracy' (which would read as a mountain/rope, ε near zero) and 'selection_process' (which would read as a snare) would lose the diagnostic structure entirely — the whole point is that both are ceremonially performed as one integrated system while causally operating as two. This is the inverse of the BGS decomposition guidance: here the single constraint IS the decoupling itself, and forcing a split would eliminate the very structure under analysis.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(measurement_authority_decoupling, moderate, 0.38).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
