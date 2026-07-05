% ============================================================================
% CONSTRAINT STORY: basic_law_interpretive_boundary__balanced_contestation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_basic_law_interpretive_boundary__balanced_contestation_reading, []).

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
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
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
 *   constraint_id: basic_law_interpretive_boundary__balanced_contestation_reading
 *   human_readable: Basic Law Interpretive Boundary — Balanced Contestation Reading
 *   domain: constitutional_law/comparative_constitutionalism
 *
 * SUMMARY:
 *   This story instantiates the balanced-contestation reading of the Basic
 *   Law interpretive boundary kernel: neither the Supreme Court nor the
 *   Knesset holds settled final authority over the scope of judicial review
 *   of Basic Laws. Courts interpret within jurisdictional bounds they claim
 *   as legitimate but cannot unilaterally extend without political and
 *   institutional cost; the legislature retains formal sovereign lawmaking
 *   power but operates under practical constraints from international
 *   obligations, judicial independence norms, and the reputational cost of
 *   overriding rulings. The result is genuine institutional dialogue rather
 *   than a settled hierarchy — extraction and suppression rise gradually as
 *   each side probes the boundary (legislative override attempts, judicial
 *   expansion of standing and justiciability), but no single equilibrium has
 *   locked in. This is a distinct constraint from the
 *   judicial_supremacy_reading (where court invalidation binds the Knesset
 *   outright) and the parliamentary_sovereignty_reading (where the Knesset
 *   can override review at will) — those readings describe different, more
 *   settled institutional configurations with different beneficiary/victim
 *   structures and different ε trajectories.
 *
 * KEY AGENTS:
 *   - supreme_court_of_israel: Primary interpretive authority within bounded jurisdiction (institutional/constrained) — reviews legislation, depends on continued institutional acceptance
 *   - knesset_governing_coalition: Sovereign lawmaker constrained by review and international norms (institutional/constrained) — bears cost of negotiating rather than unilaterally overriding
 *   - minority_rights_litigants: Primary beneficiary of judicial review avenue (powerless/trapped) — depends entirely on court's willingness to hear claims
 *   - policy_reform_movements: Bears cost of review-induced delay/narrowing of enacted legislation (organized/constrained)
 *   - international_treaty_partners: Benefit from maintained rights-protective baseline (institutional/arbitrage) — can impose costs if boundary shifts
 *   - executive_branch_ministries: Analytical/administrative observer forced to implement contested outcomes
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(basic_law_interpretive_boundary__balanced_contestation_reading, 0.42).
domain_priors:suppression_score(basic_law_interpretive_boundary__balanced_contestation_reading, 0.38).
domain_priors:theater_ratio(basic_law_interpretive_boundary__balanced_contestation_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(basic_law_interpretive_boundary__balanced_contestation_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(basic_law_interpretive_boundary__balanced_contestation_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(basic_law_interpretive_boundary__balanced_contestation_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(basic_law_interpretive_boundary__balanced_contestation_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(basic_law_interpretive_boundary__balanced_contestation_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(basic_law_interpretive_boundary__balanced_contestation_reading, tangled_rope).
narrative_ontology:human_readable(basic_law_interpretive_boundary__balanced_contestation_reading, "Basic Law Interpretive Boundary — Balanced Contestation Reading").
narrative_ontology:topic_domain(basic_law_interpretive_boundary__balanced_contestation_reading, "constitutional_law/comparative_constitutionalism").

domain_priors:requires_active_enforcement(basic_law_interpretive_boundary__balanced_contestation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(basic_law_interpretive_boundary__balanced_contestation_reading, 'e9b8cb10-a4c0-4588-8675-fe56bb436f37').
narrative_ontology:cs_kernel_codification('e9b8cb10-a4c0-4588-8675-fe56bb436f37', distributed).
narrative_ontology:cs_authority_grounding('e9b8cb10-a4c0-4588-8675-fe56bb436f37', distributed).
narrative_ontology:cs_reading_relation('e9b8cb10-a4c0-4588-8675-fe56bb436f37', basic_law_interpretive_boundary__judicial_supremacy_reading, coexists_with).
narrative_ontology:cs_reading_relation('e9b8cb10-a4c0-4588-8675-fe56bb436f37', basic_law_interpretive_boundary__parliamentary_sovereignty_reading, coexists_with).
narrative_ontology:cs_axiom('e9b8cb10-a4c0-4588-8675-fe56bb436f37', foundational, dual_bounded_legitimacy).
narrative_ontology:cs_axiom_status(dual_bounded_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('e9b8cb10-a4c0-4588-8675-fe56bb436f37', dual_bounded_legitimacy, conventional).
narrative_ontology:cs_axiom('e9b8cb10-a4c0-4588-8675-fe56bb436f37', secondary, international_norms_as_soft_constraint).
narrative_ontology:cs_axiom_status(international_norms_as_soft_constraint, holdable).
narrative_ontology:cs_axiom_grounding('e9b8cb10-a4c0-4588-8675-fe56bb436f37', international_norms_as_soft_constraint, instrumental).
narrative_ontology:cs_reference_frame('e9b8cb10-a4c0-4588-8675-fe56bb436f37', post_1992_constitutional_revolution_equilibrium).
narrative_ontology:cs_drift_state('e9b8cb10-a4c0-4588-8675-fe56bb436f37', post_2023_judicial_reform_crisis, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('e9b8cb10-a4c0-4588-8675-fe56bb436f37', '').
narrative_ontology:cs_kernel_id(basic_law_interpretive_boundary__balanced_contestation_reading, basic_law_interpretive_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(basic_law_interpretive_boundary__balanced_contestation_reading, constitutional_stability_seekers).
narrative_ontology:constraint_beneficiary(basic_law_interpretive_boundary__balanced_contestation_reading, minority_rights_litigants).
narrative_ontology:constraint_beneficiary(basic_law_interpretive_boundary__balanced_contestation_reading, international_treaty_partners).
narrative_ontology:constraint_victim(basic_law_interpretive_boundary__balanced_contestation_reading, legislative_majority_coalitions).
narrative_ontology:constraint_victim(basic_law_interpretive_boundary__balanced_contestation_reading, policy_reform_movements).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(basic_law_interpretive_boundary__balanced_contestation_reading, supreme_court_of_israel).
narrative_ontology:constraint_victim(basic_law_interpretive_boundary__balanced_contestation_reading, knesset_governing_coalition).
narrative_ontology:constraint_victim(basic_law_interpretive_boundary__balanced_contestation_reading, executive_branch_ministries).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets Basic Laws to review legislation for compatibility with fundamental rights, exercising judicial review it claims as bounded but essential. Its authority depends on continued acceptance by the other branches and the public; it cannot enforce its rulings without executive cooperation, and its legitimacy erodes if perceived as usurping legislative prerogative.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__balanced_contestation_reading, supreme_court_of_israel, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(basic_law_interpretive_boundary__balanced_contestation_reading, supreme_court_of_israel, beneficiary).

% Holds electoral mandate and formal sovereign lawmaking power, but finds its legislative agenda subject to judicial review and international scrutiny. Can amend Basic Laws by ordinary majority in principle, yet doing so to override a court ruling triggers domestic legitimacy costs and international diplomatic friction, so the coalition negotiates rather than simply overriding.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__balanced_contestation_reading, knesset_governing_coalition, payer,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(basic_law_interpretive_boundary__balanced_contestation_reading, knesset_governing_coalition, agenda_setter).

% Rely on judicial review as their primary avenue against majoritarian legislation that would otherwise pass unchecked. Have no independent capacity to compel the legislature and depend entirely on the court's willingness to hear and enforce claims within its bounded jurisdiction.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__balanced_contestation_reading, minority_rights_litigants, beneficiary,
    powerless, biographical, trapped, national).

% Build electoral coalitions to pass reform legislation, only to see enacted measures delayed, narrowed, or struck down under judicial review. Experience the boundary as a tax on legislative victory — winning an election is not sufficient to secure policy change if the change touches domains the court treats as rights-sensitive.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__balanced_contestation_reading, policy_reform_movements, payer,
    organized, biographical, constrained, national).

% Trade agreements, human rights conventions, and diplomatic relationships assume Israel's domestic institutions will maintain rights-protective baselines. They benefit when domestic judicial review keeps legislation within internationally recognized norms, and can impose reputational or economic costs when the boundary shifts sharply toward parliamentary sovereignty.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__balanced_contestation_reading, international_treaty_partners, beneficiary,
    institutional, generational, arbitrage, global).

% Must implement whichever outcome the court-legislature negotiation produces, absorbing administrative and diplomatic costs of compliance ambiguity. Positioned between both institutions, executing policy under conditions of contested authority rather than settled rules.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__balanced_contestation_reading, executive_branch_ministries, observer,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(basic_law_interpretive_boundary__balanced_contestation_reading, executive_branch_ministries, payer).

% Future Knessets will inherit whatever equilibrium the current contestation settles into, without having had any voice in shaping it. If the boundary hardens toward either judicial or parliamentary supremacy now, future legislative majorities lose optionality they never got to negotiate for themselves.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__balanced_contestation_reading, unrepresented_future_legislatures, excluded,
    powerless, generational, trapped, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(basic_law_interpretive_boundary__balanced_contestation_reading, diffuse).
narrative_ontology:fixing_cost_class(basic_law_interpretive_boundary__balanced_contestation_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Allocates interpretive authority so that neither institution can unilaterally resolve contested questions of fundamental rights versus majoritarian policy — courts check legislative overreach into entrenched rights domains, while the legislature retains formal sovereignty and the practical capacity to legislate, amend, and eventually override through sustained political effort.
% TRANSFER_FUNCTION: Moves policy-implementation certainty away from legislative majorities (who cannot be sure enacted law will survive review) toward litigants and treaty partners who gain a forum to contest majoritarian outcomes; moves diplomatic and reputational costs toward the legislature when it presses against the boundary; moves administrative burden onto the executive, which must operate under the resulting ambiguity.
% ABSENT_VOICES: Future legislatures have no seat in the present negotiation over where the boundary sits, yet inherit its settled or unsettled state. Ordinary citizens outside organized litigation or reform movements have no direct voice in the institutional dialogue and experience only its downstream policy effects.
% DISAPPEARANCE_RATIONALE: If the contested boundary dissolved into a clean settlement — either full judicial supremacy or full parliamentary sovereignty — the triadic negotiation dynamic would collapse. Litigation strategy, coalition legislative calculus, and international treaty-compliance assumptions all currently price in genuine uncertainty about which institution prevails on a given policy domain; removing that uncertainty would reallocate power decisively to whichever institution won the settlement, restructuring incentives for all named seats.
% FOUNDING_PROBLEM: Israel's absence of a single entrenched constitution left open who adjudicates conflicts between ordinary legislation and fundamental rights or structural principles; the Basic Laws were enacted piecemeal without a clear supremacy clause, leaving the court-legislature boundary constitutionally underdetermined from the outset.
% FOUNDING_PROBLEM_CORROBORATION: Comparative constitutional scholars outside Israel (analyzing the 1992 Basic Laws and subsequent 'constitutional revolution' jurisprudence) and international bodies monitoring judicial independence norms both attest the boundary question remains structurally open rather than resolved in either institution's favor — this is not merely asserted by the court seeking to preserve review power or by legislative coalitions seeking to preserve sovereignty.
narrative_ontology:disappearance_verdict(basic_law_interpretive_boundary__balanced_contestation_reading, world_rearranges).
narrative_ontology:founding_problem_status(basic_law_interpretive_boundary__balanced_contestation_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(basic_law_interpretive_boundary__balanced_contestation_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(basic_law_interpretive_boundary__balanced_contestation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(basic_law_interpretive_boundary__balanced_contestation_reading, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(basic_law_interpretive_boundary__balanced_contestation_reading_tests).
:- end_tests(basic_law_interpretive_boundary__balanced_contestation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is moderate (0.42) and rising slowly because the boundary genuinely oscillates rather than settling into one-sided extraction — each institution wins some domains and loses others, so the aggregate transfer is real but bounded compared to a settled-supremacy configuration. Suppression is likewise moderate (0.38): the constraint requires active enforcement (court rulings must be respected, legislative restraint must be exercised) but does not rest on foreclosing alternatives outright — both institutions retain formal capacity to act, they simply bear costs for doing so unilaterally. Accessibility collapse is comparatively low (0.35) because the contested nature of the boundary means alternative institutional configurations (toward either pole) remain live and are actively argued for by different political coalitions — the boundary has not foreclosed its own contestability. Resistance is elevated (0.55) precisely because this is a live political fight in Israel, not a settled arrangement.
 *
 * PERSPECTIVAL GAP:
 *   From the Supreme Court's seat, the arrangement looks like principled bounded interpretation defending fundamental rights against majoritarian overreach. From a policy reform movement's seat, the identical structure looks like an unelected body imposing unpredictable delay costs on a democratically mandated program. Both readings are structurally accurate from their respective positions — the engine computing divergent per-seat classifications from the same underlying data is the correct output for a genuinely contested boundary, not an error to be reconciled.
 *
 * DIRECTIONALITY LOGIC:
 *   Minority rights litigants and international treaty partners sit near the beneficiary end: they gain from the existence of a genuine check on majoritarian legislation, even an uncertain one, because it creates leverage they would not otherwise have. Legislative majority coalitions and policy reform movements sit nearer the target end: they experience the boundary as a tax on their electoral mandate, since enacted policy can be delayed, narrowed, or invalidated by a court whose scope of authority they cannot definitively confine. The Supreme Court and the Knesset coalition both hold dual roles — each is simultaneously an agenda-setter (shaping where the boundary sits through its own actions) and a payer (bearing costs when it loses a contest with the other institution). This dual positioning is exactly what distinguishes the balanced-contestation reading from its siblings: in the judicial_supremacy_reading the court would be pure agenda-setter/beneficiary and the Knesset pure payer; in the parliamentary_sovereignty_reading the roles invert.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — an underdetermined constitutional hierarchy following piecemeal Basic Law enactment — remains genuinely live rather than dead: there is no settled supremacy clause, and the political fight over judicial review reform (2023 and ongoing) demonstrates the boundary question has not been resolved by either institutional capture or negotiated settlement. Classifying this as tangled_rope rather than snare or pure rope prevents two mislabeling errors: treating the arrangement as pure extraction (which would ignore the genuine coordination function of preventing either institution from becoming unchecked) and treating it as pure voluntary coordination (which would ignore that reform movements and legislative majorities bear real, non-consensual costs from an uncertain review scope they did not choose and cannot unilaterally exit).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indeterminacy,
    'Is the balanced-contestation reading a stable equilibrium description of the current Israeli constitutional order, or is it a transitional snapshot en route to settlement as either judicial_supremacy_reading or parliamentary_sovereignty_reading?',
    'Track the outcome of ongoing judicial reform legislation and subsequent court rulings on Basic Law amendments over the next decade; a durable settlement toward either pole would falsify the balanced-contestation reading as a going description, while continued oscillation would corroborate it.',
    'If the system settles toward one pole, this story''s stakeholder structure and ε trajectory become historical rather than descriptive of the present order, and the sibling reading that the system settled into becomes the operative constraint going forward.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_indeterminacy, conceptual, 'Whether balanced contestation is a stable reading or a transitional state between the two polar readings.').

omega_variable(
    sibling_reading_structural_delta,
    'What specific structural element do the three sibling readings of the basic_law_interpretive_boundary kernel disagree on, and where precisely is that disagreement located?',
    'Compare each reading''s stakeholder role assignments for the Supreme Court and Knesset: judicial_supremacy_reading assigns the court sole agenda-setter/beneficiary status with binding invalidation power; parliamentary_sovereignty_reading assigns the Knesset that same status with override power; this reading assigns both institutions dual agenda-setter/payer status simultaneously. The disagreement is located in whether either institution''s authority is bounded-but-final or genuinely contestable.',
    'Choosing this reading over its siblings changes which institution is treated as the structural beneficiary of the arrangement and therefore changes the direction of the derived directionality values for the court and coalition seats.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_structural_delta, conceptual, 'Locates the precise structural disagreement between the three kernel readings for clarity on why this is a distinct constraint rather than a measurement variant.').

omega_variable(
    international_leverage_asymmetry,
    'Does the leverage international treaty partners exercise over the boundary constitute genuine external constraint on legislative sovereignty, or is it itself a contested and potentially withdrawable form of soft power that a sufficiently resolved legislature could disregard?',
    'Observe legislative and diplomatic responses to episodes where the Knesset has pressed against judicial review in ways that drew international rebuke — did material costs follow, or only rhetorical costs?',
    'If international constraint proves largely rhetorical, the beneficiary status of international_treaty_partners in this story overstates their actual structural leverage, and the boundary is closer to a purely domestic two-party negotiation than a triadic one.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(international_leverage_asymmetry, empirical, 'Whether international constraint on the legislature is materially binding or primarily reputational.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(basic_law_interpretive_boundary__balanced_contestation_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(basi_tr_t0, basic_law_interpretive_boundary__balanced_contestation_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(basi_tr_t6, basic_law_interpretive_boundary__balanced_contestation_reading, theater_ratio, 6, 0.2).
narrative_ontology:measurement(basi_tr_t12, basic_law_interpretive_boundary__balanced_contestation_reading, theater_ratio, 12, 0.22).
narrative_ontology:measurement(basi_tr_t18, basic_law_interpretive_boundary__balanced_contestation_reading, theater_ratio, 18, 0.24).
narrative_ontology:measurement(basi_tr_t24, basic_law_interpretive_boundary__balanced_contestation_reading, theater_ratio, 24, 0.26).
narrative_ontology:measurement(basi_tr_t30, basic_law_interpretive_boundary__balanced_contestation_reading, theater_ratio, 30, 0.28).

% Extraction over time
narrative_ontology:measurement(basi_be_t0, basic_law_interpretive_boundary__balanced_contestation_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(basi_be_t6, basic_law_interpretive_boundary__balanced_contestation_reading, base_extractiveness, 6, 0.33).
narrative_ontology:measurement(basi_be_t12, basic_law_interpretive_boundary__balanced_contestation_reading, base_extractiveness, 12, 0.36).
narrative_ontology:measurement(basi_be_t18, basic_law_interpretive_boundary__balanced_contestation_reading, base_extractiveness, 18, 0.39).
narrative_ontology:measurement(basi_be_t24, basic_law_interpretive_boundary__balanced_contestation_reading, base_extractiveness, 24, 0.41).
narrative_ontology:measurement(basi_be_t30, basic_law_interpretive_boundary__balanced_contestation_reading, base_extractiveness, 30, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(basi_su_t0, basic_law_interpretive_boundary__balanced_contestation_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(basi_su_t6, basic_law_interpretive_boundary__balanced_contestation_reading, suppression_requirement, 6, 0.28).
narrative_ontology:measurement(basi_su_t12, basic_law_interpretive_boundary__balanced_contestation_reading, suppression_requirement, 12, 0.31).
narrative_ontology:measurement(basi_su_t18, basic_law_interpretive_boundary__balanced_contestation_reading, suppression_requirement, 18, 0.34).
narrative_ontology:measurement(basi_su_t24, basic_law_interpretive_boundary__balanced_contestation_reading, suppression_requirement, 24, 0.36).
narrative_ontology:measurement(basi_su_t30, basic_law_interpretive_boundary__balanced_contestation_reading, suppression_requirement, 30, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(basic_law_interpretive_boundary__balanced_contestation_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(basic_law_interpretive_boundary__balanced_contestation_reading, basic_law_interpretive_boundary__judicial_supremacy_reading).
narrative_ontology:affects_constraint(basic_law_interpretive_boundary__balanced_contestation_reading, basic_law_interpretive_boundary__parliamentary_sovereignty_reading).

% DUAL FORMULATION NOTE:
% These three stories decompose the natural-language label 'the Basic Law interpretive boundary' per the ε-invariance principle: each reading assigns different beneficiary/victim structures and different stakeholder role configurations to the same two institutions (Supreme Court, Knesset), and therefore has a different intrinsic ε rather than being a different observation angle on one constraint. This story (balanced_contestation_reading) claims ε≈0.42 with dual agenda-setter/payer roles on both institutions; judicial_supremacy_reading would claim a lower ε for the court and higher ε burden on the legislature with the court as pure beneficiary; parliamentary_sovereignty_reading would invert that assignment. All three are linked here so contamination/propagation analysis can trace how a purity shift in one reading's empirical corroboration affects confidence in the sibling readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
