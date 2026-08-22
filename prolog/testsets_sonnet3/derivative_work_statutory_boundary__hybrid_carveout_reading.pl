% ============================================================================
% CONSTRAINT STORY: derivative_work_statutory_boundary__hybrid_carveout_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_derivative_work_statutory_boundary__hybrid_carveout_reading, []).

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
 *   constraint_id: derivative_work_statutory_boundary__hybrid_carveout_reading
 *   human_readable: Commercial-Use Carveout Reading of the Derivative Work Boundary
 *   domain: intellectual_property_law/technology_governance/information_economics
 *
 * SUMMARY:
 *   This story instantiates the hybrid_carveout_reading of the
 *   derivative-work-statutory-boundary kernel: the boundary is drawn
 *   categorically by commercial exploitation rather than by degree of
 *   expressive incorporation (the coordination_reading) or by presence of any
 *   copyrighted-expression use (the enclosure_reading). Under this reading,
 *   non-commercial transformative use is categorically exempt from clearance,
 *   while any commercial exploitation — regardless of how transformative —
 *   triggers a licensing requirement. This produces a genuine coordination
 *   function (protecting a low-stakes commons of non-commercial reuse) bolted
 *   to an asymmetric extraction mechanism (commercial developers, especially
 *   small ones without negotiating leverage, pay licensing fees calibrated
 *   unilaterally by rightsholders). The categorical trigger is coarser than a
 *   substantiality test and sweeps in commercially modest activity that poses
 *   little market substitution risk, which is the extraction the metrics
 *   describe.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(derivative_work_statutory_boundary__hybrid_carveout_reading, 0.52).
domain_priors:suppression_score(derivative_work_statutory_boundary__hybrid_carveout_reading, 0.58).
domain_priors:theater_ratio(derivative_work_statutory_boundary__hybrid_carveout_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(derivative_work_statutory_boundary__hybrid_carveout_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(derivative_work_statutory_boundary__hybrid_carveout_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(derivative_work_statutory_boundary__hybrid_carveout_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(derivative_work_statutory_boundary__hybrid_carveout_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(derivative_work_statutory_boundary__hybrid_carveout_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(derivative_work_statutory_boundary__hybrid_carveout_reading, tangled_rope).
narrative_ontology:human_readable(derivative_work_statutory_boundary__hybrid_carveout_reading, "Commercial-Use Carveout Reading of the Derivative Work Boundary").
narrative_ontology:topic_domain(derivative_work_statutory_boundary__hybrid_carveout_reading, "intellectual_property_law/technology_governance/information_economics").

domain_priors:requires_active_enforcement(derivative_work_statutory_boundary__hybrid_carveout_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(derivative_work_statutory_boundary__hybrid_carveout_reading, '7967d29b-afa8-4ab5-8d15-6b8692092478').
narrative_ontology:cs_kernel_codification('7967d29b-afa8-4ab5-8d15-6b8692092478', distributed).
narrative_ontology:cs_authority_grounding('7967d29b-afa8-4ab5-8d15-6b8692092478', distributed).
narrative_ontology:cs_reading_relation('7967d29b-afa8-4ab5-8d15-6b8692092478', derivative_work_statutory_boundary__coordination_reading, coexists_with).
narrative_ontology:cs_reading_relation('7967d29b-afa8-4ab5-8d15-6b8692092478', derivative_work_statutory_boundary__enclosure_reading, coexists_with).
narrative_ontology:cs_axiom('7967d29b-afa8-4ab5-8d15-6b8692092478', foundational, commercial_exploitation_is_the_operative_line).
narrative_ontology:cs_axiom_status(commercial_exploitation_is_the_operative_line, holdable).
narrative_ontology:cs_axiom_grounding('7967d29b-afa8-4ab5-8d15-6b8692092478', commercial_exploitation_is_the_operative_line, conventional).
narrative_ontology:cs_axiom('7967d29b-afa8-4ab5-8d15-6b8692092478', secondary, noncommercial_transformation_categorically_exempt).
narrative_ontology:cs_axiom_status(noncommercial_transformation_categorically_exempt, holdable).
narrative_ontology:cs_axiom_grounding('7967d29b-afa8-4ab5-8d15-6b8692092478', noncommercial_transformation_categorically_exempt, instrumental).
narrative_ontology:cs_reference_frame('7967d29b-afa8-4ab5-8d15-6b8692092478', commercial_exploitation_trigger_doctrine).
narrative_ontology:cs_drift_state('7967d29b-afa8-4ab5-8d15-6b8692092478', post_platform_monetization_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('7967d29b-afa8-4ab5-8d15-6b8692092478', '').
narrative_ontology:cs_kernel_id(derivative_work_statutory_boundary__hybrid_carveout_reading, derivative_work_statutory_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(derivative_work_statutory_boundary__hybrid_carveout_reading, rightsholder_licensing_entities).
narrative_ontology:constraint_beneficiary(derivative_work_statutory_boundary__hybrid_carveout_reading, noncommercial_transformative_creators).
narrative_ontology:constraint_victim(derivative_work_statutory_boundary__hybrid_carveout_reading, commercial_derivative_developers).
narrative_ontology:constraint_victim(derivative_work_statutory_boundary__hybrid_carveout_reading, small_commercial_remix_studios).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(derivative_work_statutory_boundary__hybrid_carveout_reading, platform_intermediaries).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold copyright in the underlying works and operate licensing desks that clear commercial derivative use. They lobby for and litigate around the commercial/non-commercial line, collect licensing fees from any exploitation crossing into commercial use, and tolerate non-commercial transformative use as a goodwill and enforcement-cost-saving concession.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__hybrid_carveout_reading, rightsholder_licensing_entities, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(derivative_work_statutory_boundary__hybrid_carveout_reading, rightsholder_licensing_entities, beneficiary).

% Fan artists, remixers, educators, and hobbyists who transform existing works without seeking payment. Under this reading they operate free of licensing requirements as long as no commercial exploitation occurs; their exit option is simply staying non-commercial, which costs them nothing but forecloses monetization.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__hybrid_carveout_reading, noncommercial_transformative_creators, beneficiary,
    powerless, biographical, mobile, global).

% Studios and individual creators who want to monetize transformative works built on existing IP (adaptations, commercial fan projects, derivative software built on copyrighted assets). The moment they cross into commercial exploitation they must obtain authorization and pay licensing fees calibrated by the rightsholder, with no negotiating leverage; their alternative is abandoning monetization or risking infringement litigation.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__hybrid_carveout_reading, commercial_derivative_developers, payer,
    moderate, biographical, constrained, national).

% Small operations that scaled a non-commercial transformative practice into a modest commercial one (merchandise, paid commissions, monetized platforms) and are now caught by the commercial-use trigger. They lack the capital to negotiate licenses on favorable terms or to litigate the line's application to their specific use, and abandoning commercialization means losing accumulated audience investment.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__hybrid_carveout_reading, small_commercial_remix_studios, payer,
    powerless, biographical, trapped, regional).

% Hosting platforms that must classify user content as commercial or non-commercial to apply the boundary, often via automated detection of monetization signals. They benefit from a bright-line-adjacent test that is easier to automate than a pure substantiality test, and shape enforcement through their classification tooling without bearing the licensing costs themselves.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__hybrid_carveout_reading, platform_intermediaries, observer,
    organized, biographical, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(derivative_work_statutory_boundary__hybrid_carveout_reading, platform_intermediaries, beneficiary).

% Consumers of transformative works who have no voice in where the commercial/non-commercial line is drawn, yet experience reduced availability of commercially-produced derivative works when licensing costs make projects unviable, and reduced quality/polish among surviving non-commercial works that cannot invest resources without commercial return.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__hybrid_carveout_reading, downstream_audiences, excluded,
    powerless, immediate, trapped, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(derivative_work_statutory_boundary__hybrid_carveout_reading, rightsholder_licensing_entities).
narrative_ontology:fixing_cost_class(derivative_work_statutory_boundary__hybrid_carveout_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Distinguishes a genuinely low-stakes commons (non-commercial transformation, hobbyist and educational reuse) that needs no clearance from a genuinely rivalrous economic exploitation of the underlying work that the rightsholder has a real stake in controlling and pricing.
% TRANSFER_FUNCTION: Moves licensing fees from commercial derivative developers to rightsholder licensing entities whenever exploitation crosses the commercial threshold; moves nothing from non-commercial creators, who operate the transformative privilege at zero direct cost.
% ABSENT_VOICES: Downstream audiences who bear the diffuse cost of suppressed commercial derivative supply have no seat in setting where the commercial/non-commercial line falls; small commercial remix studios who fall on the wrong side of an ambiguous line are rarely present when platforms or courts draw it.
% DISAPPEARANCE_RATIONALE: If the commercial/non-commercial carveout vanished and a single uniform rule applied instead, either commercial developers would gain free rein (collapsing the licensing revenue rightsholders currently extract) or non-commercial creators would suddenly need clearance (collapsing the participatory commons the carveout currently protects) — either direction reorganizes a substantial licensing and enforcement apparatus.
% FOUNDING_PROBLEM: Courts and legislators needed a workable line to separate transformative fan and educational activity, which was proliferating with digital tools and posed little economic threat to rightsholders, from commercial exploitation that directly competed with or substituted for licensed markets.
% FOUNDING_PROBLEM_CORROBORATION: Rightsholder licensing entities attest the line remains necessary to protect licensing markets from erosion. Independent legal scholarship and amicus filings from digital-rights organizations attest that the commercial trigger has drifted from protecting markets to functioning as a fee-collection tripwire applied to marginal commercial activity (small merchandise, modest paid commissions) that poses negligible market substitution risk — corroboration exists outside the beneficiary set, though it is contested rather than settled.
narrative_ontology:disappearance_verdict(derivative_work_statutory_boundary__hybrid_carveout_reading, world_rearranges).
narrative_ontology:founding_problem_status(derivative_work_statutory_boundary__hybrid_carveout_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(derivative_work_statutory_boundary__hybrid_carveout_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(derivative_work_statutory_boundary__hybrid_carveout_reading, 'none', 1).
narrative_ontology:epsilon_provenance(derivative_work_statutory_boundary__hybrid_carveout_reading, 0.52, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(derivative_work_statutory_boundary__hybrid_carveout_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(derivative_work_statutory_boundary__hybrid_carveout_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(derivative_work_statutory_boundary__hybrid_carveout_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (0.52 at interval end) is moderate: real licensing revenue flows from commercial developers to rightsholders, but the non-commercial carveout genuinely exempts a large population from any extraction, so the constraint is not purely extractive. Suppression (0.58) reflects that platform classification tooling and litigation threat actively enforce the commercial/non-commercial line rather than leaving it to voluntary compliance. Theater (0.28) is present but secondary — some enforcement activity (takedown notices against ambiguous small commercial use) functions more as in terrorem signaling than substantive market protection, but the core licensing-fee mechanism is functionally real, not merely performative.
 *
 * DIRECTIONALITY LOGIC:
 *   Rightsholder licensing entities sit at the beneficiary end: they set the categorical line, administer the licensing desk, and collect the transfer. Non-commercial creators also sit near the beneficiary end (zero-cost exemption) even though they do not collect revenue, because the constraint subsidizes their activity relative to a stricter regime. Commercial derivative developers and especially small commercial remix studios sit near the target end: they bear the transfer, and small studios in particular are trapped once they have built an audience around a formerly non-commercial practice — reversing to non-commercial status destroys the value they built.
 *
 * MANDATROPHY ANALYSIS:
 *   The categorical commercial/non-commercial line was built to solve a real problem (distinguishing hobbyist reuse from market-competing exploitation) and that problem remains partly live — hence founding_problem_status is contested rather than dead. Classifying this as tangled_rope rather than snare prevents mislabeling the whole arrangement as pure extraction: the non-commercial carveout is a real, uncoerced benefit to a large population, not merely cover. But classifying it as tangled_rope rather than rope prevents treating the commercial-side licensing fees as a fairly negotiated coordination cost, when in fact small commercial actors have no negotiating leverage over rightsholder-set terms and face trapped exit once commercialized.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    commercial_trigger_versus_substantiality_line,
    'Is the commercial/non-commercial trigger a legitimate, administrable proxy for market-substitution risk, or does it diverge structurally from the coordination_reading''s substantiality test in ways that let rightsholders capture rents from activity that would be non-infringing under a transformative-use analysis?',
    'Comparative case analysis: identify commercial derivative uses found to require licensing under this reading that would have been found non-infringing (highly transformative, minimal market substitution) under the coordination_reading''s substantiality test. A large divergent set would indicate the commercial trigger is over-inclusive relative to the coordination function it claims.',
    'A large divergent set supports reclassifying substantial portions of this reading''s operation as closer to snare (categorical fee extraction untethered from actual market harm); a small divergent set supports the tangled_rope classification as authored, where the categorical line tracks genuine market-substitution risk closely enough to remain a defensible coordination proxy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(commercial_trigger_versus_substantiality_line, empirical, 'Whether the commercial-use trigger tracks real market substitution or over-extracts relative to a transformative-use test.').

omega_variable(
    kernel_reading_selection_pressure,
    'Is the hybrid_carveout_reading a stable equilibrium reading of the kernel, or is it a transitional compromise that courts and legislators are actively moving away from toward one of the sibling readings?',
    'Track case law and legislative drafting trends: increasing reliance on transformative-use substantiality analysis (moving toward coordination_reading) versus increasing reliance on bright-line commercial/non-commercial classification in licensing regimes and platform terms of service (entrenching hybrid_carveout_reading) versus expansion of derivative-work findings to any expressive use regardless of commercial status (moving toward enclosure_reading).',
    'If courts are drifting toward the coordination_reading, this reading''s extraction level is a historical artifact that should decline over time; if platforms and rightsholders are entrenching the commercial trigger contractually (via platform terms of service) independent of case law, this reading may persist and intensify regardless of doctrinal drift.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_selection_pressure, conceptual, 'Whether this reading is a stable doctrinal equilibrium or a transitional compromise between the two sibling readings.').

omega_variable(
    small_actor_capture_scope,
    'How much of the commercial-side extraction falls on well-resourced commercial developers who can absorb licensing costs versus small commercial remix studios who cannot negotiate and are effectively trapped once they commercialize?',
    'Survey licensing fee schedules and negotiation outcomes across commercial developer size classes; measure the rate at which small studios abandon commercialization versus pay unfavorable licensing terms versus face infringement action.',
    'If extraction concentrates heavily on small, powerless commercial actors with no negotiating leverage, this strengthens the tangled_rope classification''s victim declaration and may warrant treating small_commercial_remix_studios as a more severely extracted sub-population than commercial_derivative_developers generally.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(small_actor_capture_scope, empirical, 'Distribution of licensing-fee extraction across commercial actor size and bargaining power.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(derivative_work_statutory_boundary__hybrid_carveout_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(deri_tr_t0, derivative_work_statutory_boundary__hybrid_carveout_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(deri_tr_t4, derivative_work_statutory_boundary__hybrid_carveout_reading, theater_ratio, 4, 0.15).
narrative_ontology:measurement(deri_tr_t8, derivative_work_statutory_boundary__hybrid_carveout_reading, theater_ratio, 8, 0.18).
narrative_ontology:measurement(deri_tr_t12, derivative_work_statutory_boundary__hybrid_carveout_reading, theater_ratio, 12, 0.21).
narrative_ontology:measurement(deri_tr_t16, derivative_work_statutory_boundary__hybrid_carveout_reading, theater_ratio, 16, 0.24).
narrative_ontology:measurement(deri_tr_t20, derivative_work_statutory_boundary__hybrid_carveout_reading, theater_ratio, 20, 0.26).
narrative_ontology:measurement(deri_tr_t24, derivative_work_statutory_boundary__hybrid_carveout_reading, theater_ratio, 24, 0.28).

% Extraction over time
narrative_ontology:measurement(deri_be_t0, derivative_work_statutory_boundary__hybrid_carveout_reading, base_extractiveness, 0, 0.34).
narrative_ontology:measurement(deri_be_t4, derivative_work_statutory_boundary__hybrid_carveout_reading, base_extractiveness, 4, 0.39).
narrative_ontology:measurement(deri_be_t8, derivative_work_statutory_boundary__hybrid_carveout_reading, base_extractiveness, 8, 0.43).
narrative_ontology:measurement(deri_be_t12, derivative_work_statutory_boundary__hybrid_carveout_reading, base_extractiveness, 12, 0.46).
narrative_ontology:measurement(deri_be_t16, derivative_work_statutory_boundary__hybrid_carveout_reading, base_extractiveness, 16, 0.49).
narrative_ontology:measurement(deri_be_t20, derivative_work_statutory_boundary__hybrid_carveout_reading, base_extractiveness, 20, 0.51).
narrative_ontology:measurement(deri_be_t24, derivative_work_statutory_boundary__hybrid_carveout_reading, base_extractiveness, 24, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(deri_su_t0, derivative_work_statutory_boundary__hybrid_carveout_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(deri_su_t4, derivative_work_statutory_boundary__hybrid_carveout_reading, suppression_requirement, 4, 0.44).
narrative_ontology:measurement(deri_su_t8, derivative_work_statutory_boundary__hybrid_carveout_reading, suppression_requirement, 8, 0.48).
narrative_ontology:measurement(deri_su_t12, derivative_work_statutory_boundary__hybrid_carveout_reading, suppression_requirement, 12, 0.51).
narrative_ontology:measurement(deri_su_t16, derivative_work_statutory_boundary__hybrid_carveout_reading, suppression_requirement, 16, 0.54).
narrative_ontology:measurement(deri_su_t20, derivative_work_statutory_boundary__hybrid_carveout_reading, suppression_requirement, 20, 0.56).
narrative_ontology:measurement(deri_su_t24, derivative_work_statutory_boundary__hybrid_carveout_reading, suppression_requirement, 24, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(derivative_work_statutory_boundary__hybrid_carveout_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(derivative_work_statutory_boundary__hybrid_carveout_reading, 0.12).
narrative_ontology:affects_constraint(derivative_work_statutory_boundary__hybrid_carveout_reading, derivative_work_statutory_boundary__coordination_reading).
narrative_ontology:affects_constraint(derivative_work_statutory_boundary__hybrid_carveout_reading, derivative_work_statutory_boundary__enclosure_reading).

% DUAL FORMULATION NOTE:
% This story is the middle member of a three-story kernel decomposition of the derivative_work_statutory_boundary. The coordination_reading (substantiality-based, transformative use categorically non-infringing) carries substantially lower ε; the enclosure_reading (any expressive use requires clearance) carries substantially higher ε. This hybrid_carveout_reading occupies moderate ε by drawing the operative line at commercial exploitation rather than at expressive substantiality or at any use whatsoever. All three are linked via affects_constraints; none should be treated as an average or blend of the others — each is authored as its own constraint with its own stable ε per the ε-invariance principle.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
