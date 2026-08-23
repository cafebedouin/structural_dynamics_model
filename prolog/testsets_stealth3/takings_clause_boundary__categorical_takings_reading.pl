% ============================================================================
% CONSTRAINT STORY: takings_clause_boundary__categorical_takings_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_takings_clause_boundary__categorical_takings_reading, []).

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
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: takings_clause_boundary__categorical_takings_reading
 *   human_readable: Takings Clause Boundary - Categorical Poles with Penn Central Middle (Categorical Reading)
 *   domain: constitutional/legal/political
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the contested
 *   takings-clause-boundary kernel: the categorical reading, under which
 *   permanent physical occupations and total economic value elimination
 *   trigger compensation automatically, while every other regulation is
 *   adjudicated through the Penn Central three-factor balancing test. The
 *   arrangement is a deliberate hybrid - bright-line certainty at the poles
 *   purchased with contextual discretion everywhere else. Its coordination
 *   function is real: it sorts an unbounded stream of regulatory disputes
 *   into administrable classes and stabilizes both governmental planning and
 *   large-scale investment. Its extraction is equally real: the balancing
 *   zone transfers regulatory costs onto owners without compensation, and the
 *   burden concentrates on owners least able to litigate. Time mapping: T0
 *   approximates 1982 (Loretto establishes the physical-occupation trigger),
 *   T8 approximates 1990, T10 approximates 1992 (Lucas establishes the
 *   total-elimination trigger), T24 approximates 2006 (post-Kelo backlash
 *   era), T40 approximates 2022. KEY AGENTS (by structural relationship): -
 *   federal_judiciary: Agenda setter (institutional/constrained) - drew and
 *   polices the trigger categories and administers the balancing test. -
 *   state_and_local_regulators: Primary beneficiary
 *   (institutional/constrained) - receive uncompensated regulatory capacity
 *   in the balancing zone. - institutional_property_owners: Dual-positioned
 *   beneficiary/payer (powerful/mobile) - buy pole certainty, litigate middle
 *   cases, absorb litigation costs. - small_parcel_owners: Primary target
 *   (powerless/trapped) - bear uncompensated balancing-zone losses. -
 *   taxpayers: Symmetric payer/beneficiary (moderate/constrained) - fund pole
 *   compensation, receive balancing-zone public goods. -
 *   environmental_preservation_coalitions: Secondary beneficiary
 *   (organized/constrained) - depend on the balancing zone for their
 *   principal policy tool. - takings_theorists: Analytical observer - maps
 *   the boundary's behavior without collecting or paying.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(takings_clause_boundary__categorical_takings_reading, 0.62).
domain_priors:suppression_score(takings_clause_boundary__categorical_takings_reading, 0.55).
domain_priors:theater_ratio(takings_clause_boundary__categorical_takings_reading, 0.36).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(takings_clause_boundary__categorical_takings_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(takings_clause_boundary__categorical_takings_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(takings_clause_boundary__categorical_takings_reading, theater_ratio, 0.36).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(takings_clause_boundary__categorical_takings_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(takings_clause_boundary__categorical_takings_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(takings_clause_boundary__categorical_takings_reading, tangled_rope).
narrative_ontology:human_readable(takings_clause_boundary__categorical_takings_reading, "Takings Clause Boundary - Categorical Poles with Penn Central Middle (Categorical Reading)").
narrative_ontology:topic_domain(takings_clause_boundary__categorical_takings_reading, "constitutional/legal/political").

domain_priors:requires_active_enforcement(takings_clause_boundary__categorical_takings_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(takings_clause_boundary__categorical_takings_reading, '0acd20a6-b7b4-49d7-b23f-f4a053185ee5').
narrative_ontology:cs_kernel_codification('0acd20a6-b7b4-49d7-b23f-f4a053185ee5', fixed_text).
narrative_ontology:cs_authority_grounding('0acd20a6-b7b4-49d7-b23f-f4a053185ee5', lineage).
narrative_ontology:cs_interpretation_layer_present('0acd20a6-b7b4-49d7-b23f-f4a053185ee5').
narrative_ontology:cs_reading_relation('0acd20a6-b7b4-49d7-b23f-f4a053185ee5', takings_clause_boundary__physical_appropriation_reading, coexists_with).
narrative_ontology:cs_reading_relation('0acd20a6-b7b4-49d7-b23f-f4a053185ee5', takings_clause_boundary__regulatory_takings_reading, influences).
narrative_ontology:cs_axiom('0acd20a6-b7b4-49d7-b23f-f4a053185ee5', foundational, extreme_cases_resolved_by_rule_not_balance).
narrative_ontology:cs_axiom_status(extreme_cases_resolved_by_rule_not_balance, holdable).
narrative_ontology:cs_axiom_grounding('0acd20a6-b7b4-49d7-b23f-f4a053185ee5', extreme_cases_resolved_by_rule_not_balance, conventional).
narrative_ontology:cs_axiom('0acd20a6-b7b4-49d7-b23f-f4a053185ee5', foundational, non_extreme_regulation_adjudicated_by_factors).
narrative_ontology:cs_axiom_status(non_extreme_regulation_adjudicated_by_factors, holdable).
narrative_ontology:cs_axiom_grounding('0acd20a6-b7b4-49d7-b23f-f4a053185ee5', non_extreme_regulation_adjudicated_by_factors, conventional).
narrative_ontology:cs_reference_frame('0acd20a6-b7b4-49d7-b23f-f4a053185ee5', categorical_poles_balanced_middle).
narrative_ontology:cs_drift_state('0acd20a6-b7b4-49d7-b23f-f4a053185ee5', contemporary_post_total_elimination_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('0acd20a6-b7b4-49d7-b23f-f4a053185ee5', '2026-08-05T14:30:00Z').
narrative_ontology:cs_kernel_id(takings_clause_boundary__categorical_takings_reading, takings_clause_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(takings_clause_boundary__categorical_takings_reading, state_and_local_regulators).
narrative_ontology:constraint_beneficiary(takings_clause_boundary__categorical_takings_reading, institutional_property_owners).
narrative_ontology:constraint_beneficiary(takings_clause_boundary__categorical_takings_reading, environmental_preservation_coalitions).
narrative_ontology:constraint_victim(takings_clause_boundary__categorical_takings_reading, small_parcel_owners).
narrative_ontology:constraint_victim(takings_clause_boundary__categorical_takings_reading, taxpayers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(takings_clause_boundary__categorical_takings_reading, taxpayers).
narrative_ontology:constraint_victim(takings_clause_boundary__categorical_takings_reading, institutional_property_owners).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets the constitutional compensation clause and drew the two automatic-trigger categories (permanent physical occupation, total value elimination) while assigning everything else to a three-factor balancing inquiry. Gains adjudicative authority from being the sole arbiter of where the line sits. Can redraw the categories, but each revision spends accumulated precedent capital and invites legislative and political retaliation.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__categorical_takings_reading, federal_judiciary, agenda_setter,
    institutional, generational, constrained, national).

% Impose zoning, landmark, environmental, and rent rules on privately held land. In the balancing zone they act without paying owners for resulting value losses; they must design around the two automatic triggers, which caps how far any single rule can go. Receives the practical regulatory capacity that the balancing zone affords.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__categorical_takings_reading, state_and_local_regulators, beneficiary,
    institutional, generational, constrained, national).

% Large developers, real estate trusts, timber and energy companies holding portfolios across jurisdictions. Obtain certainty whenever a rule crosses one of the two triggers and litigate balancing-zone cases with retained counsel and repeat-player familiarity. Absorb substantial litigation costs and occasional losses, and can shift capital toward jurisdictions with lighter rules.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__categorical_takings_reading, institutional_property_owners, beneficiary,
    powerful, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(takings_clause_boundary__categorical_takings_reading, institutional_property_owners, payer).

% Homeowners and smallholders whose parcels lose value under rules that stop short of either trigger. Cannot finance multi-year constitutional litigation over a single lot; the land cannot relocate; selling under the diminished condition is the only exit and locks the loss in. Bears the balancing zone's costs without compensation.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__categorical_takings_reading, small_parcel_owners, payer,
    powerless, biographical, trapped, local).

% Fund compensation whenever a rule crosses one of the two triggers, and simultaneously receive the public goods produced by uncompensated balancing-zone rules such as preserved districts and protected habitat. Cannot exit the tax base that carries the contingent liability.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__categorical_takings_reading, taxpayers, payer,
    moderate, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(takings_clause_boundary__categorical_takings_reading, taxpayers, beneficiary).

% Advocacy organizations whose principal achievements (landmark designation, wetland protection, conservation zoning) depend on imposing value-reducing rules without paying. Defend the balancing framework in court and in comment periods; would lose their main policy tool if value-reducing rules generally required payment.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__categorical_takings_reading, environmental_preservation_coalitions, beneficiary,
    organized, generational, constrained, national).

% Legal scholars and economists who map how the boundary behaves, document the unpredictability of balancing-zone adjudication, and propose alternative instantiations of the compensation clause. Neither collects from nor pays into the arrangement.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__categorical_takings_reading, takings_theorists, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(takings_clause_boundary__categorical_takings_reading, state_and_local_regulators).
narrative_ontology:fixing_cost_class(takings_clause_boundary__categorical_takings_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Sorts an unbounded stream of disputes between public regulatory power and private land ownership into two administrable classes: cases resolved by fixed rule at the extremes, and cases resolved by structured multi-factor judgment everywhere else. Stabilizes governmental planning capacity and large-scale investment expectations simultaneously.
% TRANSFER_FUNCTION: Moves regulatory cost (lost land value) from the general public onto individual owners in the balancing zone; moves compensation cost from owners to taxpayers whenever a rule crosses a trigger; moves adjudicative authority and discretionary judgment to the judiciary.
% ABSENT_VOICES: Owners who suffer sub-trigger value losses but never litigate, because the cost of a multi-year constitutional case exceeds any recoverable amount, are structurally silent; their losses never enter the adjudicative record. Future generations bearing cumulative regulatory erosion, and the diffuse public paying indirectly through constrained development, likewise have no seat.
% DISAPPEARANCE_RATIONALE: If the categorical-plus-balancing framework vanished overnight, every value-reducing rule would become either automatically compensable or automatically immune depending on which sibling reading replaced it: under a pure physical-appropriation replacement, owners would absorb nearly all regulatory losses with no recourse and regulatory practice would expand; under a broad regulatory-diminution replacement, municipal budgets would face catastrophic contingent liability and value-reducing regulation would largely stop. Land-use law, municipal finance, and development patterns would all reorganize.
% FOUNDING_PROBLEM: Determine how far government regulation may diminish private property before it becomes functionally equivalent to formal seizure and must be paid for - protecting owners from regulatory confiscation while preserving the police-power regulation that dense society requires.
% FOUNDING_PROBLEM_CORROBORATION: Property-rights litigation organizations outside the benefiting parties attest the problem is live, arguing the balancing zone systematically underprotects owners; state attorneys general defending land-use rules from the opposite side also attest liveness, arguing compensation demands would paralyze regulation; peer-reviewed scholarship across the spectrum treats the boundary as unresolved. Corroboration exists from multiple seats external to the beneficiary set.
narrative_ontology:disappearance_verdict(takings_clause_boundary__categorical_takings_reading, world_rearranges).
narrative_ontology:founding_problem_status(takings_clause_boundary__categorical_takings_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(takings_clause_boundary__categorical_takings_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(takings_clause_boundary__categorical_takings_reading, 'none', 1).
narrative_ontology:epsilon_provenance(takings_clause_boundary__categorical_takings_reading, 0.62, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(takings_clause_boundary__categorical_takings_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(takings_clause_boundary__categorical_takings_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(takings_clause_boundary__categorical_takings_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is authored at 0.62 because the balancing zone is where most regulatory action lives and its costs land on owners without payment, while the two triggers cap the worst single-rule exposures. Suppression is authored at 0.55 as a raw structural property - it is NOT scaled by power or scope in the engine's computation; only extractiveness is scaled, by directionality and spatial scope. Suppression here is doctrinal foreclosure: precedent machinery makes the sibling readings unavailable as operating law even though they persist as live positions. Theater rises over the interval because balancing-zone opinions increasingly invoke the three factors while outcomes track bench composition and repeat-player advantage - the factor language persists as ritual over a substantively outcome-driven process. The suppression_requirement series is authored deliberately: the story specifically tracks enforcement-capacity change, as the framework required escalating active judicial defense against the property-rights litigation campaign, Kelo-era backlash, and state compensation-ballot measures, plateauing once the defense hardened. Coalition check: the powerless target seat has attempted coalition twice - Oregon Measure 37 (2004) briefly forced compensation for retroactive regulatory losses before Measure 49 (2007) dismantled most of it, and Arizona Proposition 207 (2006) produced a weaker durable version. Coalition power is real but fragile: it surges after visible confiscation episodes and decays as fiscal exposure becomes salient. Identity-lock dynamic: the judiciary's self-concept as neutral arbiter is constituted through the factor-balancing idiom; if that frame broke and adjudication were openly acknowledged as outcome-driven, the arrangement's legitimacy cost structure would change sharply. All three tracked metric series run on one shared time grid (points 0, 8, 16, 24, 32, 40) so every metric is authored at every examined time point.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute differently. From the federal_judiciary seat the arrangement is a principled, administrable settlement it built and defends; from the state_and_local_regulator seat it is working regulatory room; from the institutional_property_owner seat it is a manageable risk landscape with purchasable certainty at the poles; from the small_parcel_owner seat the identical structure operates as uncompensated confiscation with no affordable recourse. The engine computes this per-seat divergence from the structural data - the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary and victim declarations drive the derivation: state_and_local_regulators and environmental_preservation_coalitions sit near the beneficiary end (the arrangement subsidizes their activity); small_parcel_owners sit near the full-target end (they bear the transfer with trapped exit); taxpayers are structurally symmetric (they fund pole compensation and receive balancing-zone public goods). Two overrides are declared where the derivation would err: powerful is overridden to 0.35 because institutional_property_owners carry a primary beneficiary role that would derive a very low d, yet they demonstrably bear litigation costs and occasional uncompensated losses - a net-subsidized but meaningfully-taxed position; moderate is overridden to 0.5 because taxpayers' dual payer/beneficiary position is genuinely symmetric and the derivation cannot express the balance from either single role. The institutional atom is left to derivation because both agents carrying it (judiciary, regulators) are genuinely near-beneficiary.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification prevents two symmetrical mislabels. Reading the arrangement as pure extraction would erase its genuine coordination half - the trigger categories really do stabilize expectations and the sorting function really does resolve disputes that would otherwise be litigated without any shared frame. Reading it as pure coordination would erase the asymmetric half - the balancing zone's costs concentrate on owners who cannot litigate while its benefits accrue to regulators and organized advocacy. The founding problem (how far regulation may go before it must be paid for) remains live and corroborated from outside the beneficiary set, so the arrangement is not running on an expired mandate; the rising theater_ratio signals proxy-drift inside the balancing zone, not obsolescence of the whole structure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'Is the categorical poles-plus-Penn-Central hybrid the correct instantiation of the takings_clause_boundary kernel, or do the sibling readings (pure physical appropriation; broad regulatory diminution) better capture where the compensation trigger sits?',
    'Doctrinal trajectory under changing bench composition, combined with sustained litigative and scholarly pressure testing each reading''s victim set against the constitutional text.',
    'The three readings assign the same regulatory acts to opposite sides of the compensation line. Switching to the physical reading moves epsilon toward near-total owner absorption; switching to the regulatory reading moves it toward near-total taxpayer absorption. Every downstream classification of this constraint is conditional on which reading governs.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Which reading of the contested compensation-clause kernel governs the boundary.').

omega_variable(
    middle_ground_uncertainty_cost,
    'What is the aggregate magnitude of uncompensated value losses borne by balancing-zone owners, compared against compensation actually paid when the triggers fire?',
    'Systematic cross-jurisdictional accounting of regulatory value losses (assessment studies, transaction-price analyses around rule impositions) set against condemnation and trigger-compensation payouts.',
    'If net uncompensated losses dominate, owners as a class are net targets and the arrangement''s extraction component is larger than the pole protections suggest; if trigger payments dominate, the arrangement nets as owner protection.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(middle_ground_uncertainty_cost, empirical, 'Whether the arrangement transfers net value from owners or to owners in aggregate.').

omega_variable(
    penn_central_capture_question,
    'Does balancing-zone adjudication actually track the three published factors, or does it track repeat-player advantage and bench ideology behind factor language?',
    'Code balancing-zone outcomes against their factor profiles; compare win rates and reversal rates for repeat players versus one-shot litigants; correlate outcomes with appointing-cohort across bench turnover.',
    'Capture-dominated adjudication would shift the arrangement from a managed hybrid toward enforced extraction wearing procedural dress, raising effective suppression and theater simultaneously.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(penn_central_capture_question, conceptual, 'Whether the balancing test is substantive method or legitimating performance.').

omega_variable(
    pole_category_expansion_trajectory,
    'Will the automatic-trigger categories expand (for example toward severe-but-subtotal value diminution, as some appellate experiments attempt) or contract back toward physical occupation alone?',
    'Track appellate doctrine following the total-elimination precedent and subsequent partial-diminution cases across circuits.',
    'Expansion shifts marginal costs toward taxpayers and enlarges the protected owner set; contraction shifts them back toward owners and shrinks the beneficiary set at the poles, moving seats across the directionality spectrum.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(pole_category_expansion_trajectory, empirical, 'Trajectory of the bright-line categories that define the arrangement''s poles.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(takings_clause_boundary__categorical_takings_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(taki_tr_t0, takings_clause_boundary__categorical_takings_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(taki_tr_t8, takings_clause_boundary__categorical_takings_reading, theater_ratio, 8, 0.22).
narrative_ontology:measurement(taki_tr_t16, takings_clause_boundary__categorical_takings_reading, theater_ratio, 16, 0.26).
narrative_ontology:measurement(taki_tr_t24, takings_clause_boundary__categorical_takings_reading, theater_ratio, 24, 0.31).
narrative_ontology:measurement(taki_tr_t32, takings_clause_boundary__categorical_takings_reading, theater_ratio, 32, 0.34).
narrative_ontology:measurement(taki_tr_t40, takings_clause_boundary__categorical_takings_reading, theater_ratio, 40, 0.36).

% Extraction over time
narrative_ontology:measurement(taki_be_t0, takings_clause_boundary__categorical_takings_reading, base_extractiveness, 0, 0.46).
narrative_ontology:measurement(taki_be_t8, takings_clause_boundary__categorical_takings_reading, base_extractiveness, 8, 0.52).
narrative_ontology:measurement(taki_be_t16, takings_clause_boundary__categorical_takings_reading, base_extractiveness, 16, 0.56).
narrative_ontology:measurement(taki_be_t24, takings_clause_boundary__categorical_takings_reading, base_extractiveness, 24, 0.59).
narrative_ontology:measurement(taki_be_t32, takings_clause_boundary__categorical_takings_reading, base_extractiveness, 32, 0.61).
narrative_ontology:measurement(taki_be_t40, takings_clause_boundary__categorical_takings_reading, base_extractiveness, 40, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(taki_su_t0, takings_clause_boundary__categorical_takings_reading, suppression_requirement, 0, 0.38).
narrative_ontology:measurement(taki_su_t8, takings_clause_boundary__categorical_takings_reading, suppression_requirement, 8, 0.43).
narrative_ontology:measurement(taki_su_t16, takings_clause_boundary__categorical_takings_reading, suppression_requirement, 16, 0.49).
narrative_ontology:measurement(taki_su_t24, takings_clause_boundary__categorical_takings_reading, suppression_requirement, 24, 0.53).
narrative_ontology:measurement(taki_su_t32, takings_clause_boundary__categorical_takings_reading, suppression_requirement, 32, 0.55).
narrative_ontology:measurement(taki_su_t40, takings_clause_boundary__categorical_takings_reading, suppression_requirement, 40, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(takings_clause_boundary__categorical_takings_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(takings_clause_boundary__categorical_takings_reading, takings_clause_boundary__physical_appropriation_reading).
narrative_ontology:affects_constraint(takings_clause_boundary__categorical_takings_reading, takings_clause_boundary__regulatory_takings_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition of the colloquial label 'the takings clause boundary'. The natural-language concept covers three structurally distinct claims with different epsilon values and different victim sets: the physical_appropriation_reading (narrowest trigger; owners absorb nearly all regulatory cost), this categorical_takings_reading (hybrid trigger; costs split asymmetrically between owners and taxpayers), and the regulatory_takings_reading (broadest trigger; taxpayers absorb most regulatory cost). The physical reading is the upstream baseline from which this reading borrows its pole logic while extending coverage to total value elimination; the regulatory reading is the downstream contest that attacks this reading's balancing zone. All three files link one another via affects_constraints; each carries its own epsilon, beneficiaries, and victims per the epsilon-invariance principle.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(takings_clause_boundary__categorical_takings_reading, powerful, 0.35).
constraint_indexing:directionality_override(takings_clause_boundary__categorical_takings_reading, moderate, 0.5).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
