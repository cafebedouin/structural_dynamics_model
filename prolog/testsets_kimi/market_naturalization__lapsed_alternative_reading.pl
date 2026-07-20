% ============================================================================
% CONSTRAINT STORY: market_naturalization__lapsed_alternative_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_market_naturalization__lapsed_alternative_reading, []).

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
 *   constraint_id: market_naturalization__lapsed_alternative_reading
 *   human_readable: Market Dominance as Lapsed Closure (Lapsed Alternative Reading)
 *   domain: political_economy/economic_history/institutional_analysis
 *
 * SUMMARY:
 *   This constraint instantiates the lapsed_alternative_reading of the
 *   contested market_naturalization kernel. The reading holds that observed
 *   market dominance is not actively maintained by incumbent capital holders,
 *   but is rather a piton â a lapsed closure where the original extractive
 *   or coordinating function has atrophied, leaving only inertial persistence
 *   and residual coordination costs. No identifiable beneficiary class
 *   captures rents from the dominance; alternatives have collapsed through
 *   non-use and path-dependent lock-in rather than active suppression. The
 *   constraint persists because the cost of coordinated exit exceeds the
 *   diffuse cost of remaining, not because any actor enforces it. This
 *   reading is distinguished from the beneficiary_maintained_reading (which
 *   sees active defense and extraction) and the hybrid_reading (which
 *   combines both mechanisms).
 *
 * KEY AGENTS:
 *   - dominant_incumbent: Agenda-setter (powerful/constrained) â occupies the legacy position and could theoretically open standards or facilitate transition, but faces a coordination problem where unilateral deviation is costly and returns are uncertain.
 *   - downstream_dependents: Primary payer (moderate/constrained) â locked into the legacy structure by compatibility requirements and switching costs; bear diffuse coordination costs without organized resistance.
 *   - consumer_base: Diffuse payer (organized/constrained) â experience reduced variety and innovation but lack individual incentive to coordinate alternatives.
 *   - prospective_entrants: Excluded voice (moderate/constrained) â barred not by active enforcement but by the rational impossibility of unilateral entry against an atrophied equilibrium.
 *   - competition_regulators: Analytical observer (institutional/analytical) â confront a structure that fits poorly with antitrust frameworks designed to detect active exclusion.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(market_naturalization__lapsed_alternative_reading, 0.15).
domain_priors:suppression_score(market_naturalization__lapsed_alternative_reading, 0.1).
domain_priors:theater_ratio(market_naturalization__lapsed_alternative_reading, 0.75).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(market_naturalization__lapsed_alternative_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(market_naturalization__lapsed_alternative_reading, suppression_requirement, 0.1).
narrative_ontology:constraint_metric(market_naturalization__lapsed_alternative_reading, theater_ratio, 0.75).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(market_naturalization__lapsed_alternative_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(market_naturalization__lapsed_alternative_reading, resistance, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(market_naturalization__lapsed_alternative_reading, piton).
narrative_ontology:human_readable(market_naturalization__lapsed_alternative_reading, "Market Dominance as Lapsed Closure (Lapsed Alternative Reading)").
narrative_ontology:topic_domain(market_naturalization__lapsed_alternative_reading, "political_economy/economic_history/institutional_analysis").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(market_naturalization__lapsed_alternative_reading, '70f7d957-bf3d-4cc2-8743-564a8ee49dc4').
narrative_ontology:cs_kernel_codification('70f7d957-bf3d-4cc2-8743-564a8ee49dc4', implicit).
narrative_ontology:cs_authority_grounding('70f7d957-bf3d-4cc2-8743-564a8ee49dc4', diffuse_epistemic).
narrative_ontology:cs_reading_relation('70f7d957-bf3d-4cc2-8743-564a8ee49dc4', market_naturalization__beneficiary_maintained_reading, coexists_with).
narrative_ontology:cs_reading_relation('70f7d957-bf3d-4cc2-8743-564a8ee49dc4', market_naturalization__hybrid_reading, coexists_with).
narrative_ontology:cs_axiom('70f7d957-bf3d-4cc2-8743-564a8ee49dc4', foundational, dominance_without_maintainers_is_natural).
narrative_ontology:cs_axiom_status(dominance_without_maintainers_is_natural, holdable).
narrative_ontology:cs_axiom_grounding('70f7d957-bf3d-4cc2-8743-564a8ee49dc4', dominance_without_maintainers_is_natural, empirically_contingent).
narrative_ontology:cs_axiom('70f7d957-bf3d-4cc2-8743-564a8ee49dc4', secondary, lapsed_closure_carries_no_intervention_duty).
narrative_ontology:cs_axiom_status(lapsed_closure_carries_no_intervention_duty, holdable).
narrative_ontology:cs_axiom_grounding('70f7d957-bf3d-4cc2-8743-564a8ee49dc4', lapsed_closure_carries_no_intervention_duty, conventional).
narrative_ontology:cs_reference_frame('70f7d957-bf3d-4cc2-8743-564a8ee49dc4', path_dependent_equilibrium).
narrative_ontology:cs_drift_state('70f7d957-bf3d-4cc2-8743-564a8ee49dc4', contemporary_antitrust_renewal, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('70f7d957-bf3d-4cc2-8743-564a8ee49dc4', '').
narrative_ontology:cs_kernel_id(market_naturalization__lapsed_alternative_reading, market_naturalization).

% --- Structural relationships ---
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(market_naturalization__lapsed_alternative_reading, downstream_dependents).
narrative_ontology:constraint_victim(market_naturalization__lapsed_alternative_reading, consumer_base).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Occupies the historically dominant market position. Maintains legacy interfaces and standards not because they generate abnormal rents, but because unilateral deviation would alienate existing customers and complementary producers. Could theoretically open standards or divest market share, but the returns to such a move are uncertain and the coordination costs of bringing the market along are high. Experiences the dominance as a legacy obligation rather than an active asset.
narrative_ontology:constraint_stakeholder(market_naturalization__lapsed_alternative_reading, dominant_incumbent, agenda_setter,
    powerful, biographical, constrained, global).

% Businesses, suppliers, and intermediaries whose operations are built around the dominant platform's technical specifications and contractual norms. Bear costs of legacy compatibility and reduced bargaining power, but these costs are diffuse and below the threshold for collective mobilization. Switching is technically possible but commercially irrational unless a critical mass of peers switches simultaneously.
narrative_ontology:constraint_stakeholder(market_naturalization__lapsed_alternative_reading, downstream_dependents, payer,
    moderate, biographical, constrained, national).

% End users who purchase from the dominant provider because it is the default compatible choice. Experience slightly higher prices and reduced product variety compared with a competitive counterfactual, but no single consumer is individually incentivized to absorb the search and switching costs required to seed an alternative. Their choices are individually rational but collectively suboptimal.
narrative_ontology:constraint_stakeholder(market_naturalization__lapsed_alternative_reading, consumer_base, payer,
    organized, biographical, constrained, national).

% Firms and developers that could offer technically superior or cheaper alternatives but cannot attract initial adoption because the installed base is locked into the legacy standard. Not actively excluded by litigation, exclusive dealing, or technical sabotage; excluded by the coordination problem that users will not try an alternative until others have tried it first.
narrative_ontology:constraint_stakeholder(market_naturalization__lapsed_alternative_reading, prospective_entrants, excluded,
    moderate, biographical, constrained, regional).

% Government agencies and courts tasked with maintaining competitive markets. Their prevailing frameworks require evidence of active exclusion, predatory pricing, or rent extraction to intervene. Confront a structure that persists without any of these smoking guns, leaving them without a clear theory of harm under existing antitrust categories.
narrative_ontology:constraint_stakeholder(market_naturalization__lapsed_alternative_reading, competition_regulators, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(market_naturalization__lapsed_alternative_reading, diffuse).
narrative_ontology:fixing_cost_class(market_naturalization__lapsed_alternative_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Historically provided a coordinating standard or platform that solved a collective-action problem in an immature market. Today it provides only residual coordination in the form of compatibility and predictability, with no live collective-action problem being actively solved.
% TRANSFER_FUNCTION: No active transfer occurs. Diffuse historical lock-in costs are borne by downstream firms and consumers who remain in the legacy equilibrium. No actor captures concentrated rents from the dominance itself; the only flows are the implicit costs of maintaining compatibility with an atrophied standard.
% ABSENT_VOICES: Prospective entrants and platform innovators are structurally absent: they would advocate for interoperability and coordinated migration but cannot form a coalition because the lapsed equilibrium makes unilateral entry irrational. Consumer advocates who would demand open standards are also absent because there is no suppression event to litigate.
% DISAPPEARANCE_RATIONALE: If the lapsed dominance disappeared overnight, market participants would need to coordinate on new standards immediately. Supply chains, consumer habits, and complementary investments would be forced into a distributed search for alternatives. The rearrangement would be chaotic but would ultimately liberate locked-in coordination and allow competitive dynamics to reassert.
% FOUNDING_PROBLEM: The original market lacked a coordinating standard or platform; a single solution emerged, solved the coordination failure, and became self-sustaining.
% FOUNDING_PROBLEM_CORROBORATION: Economic historians and institutional analysts outside the incumbent ecosystem attest that the founding coordination problem was solved decades ago. Contemporary antitrust economists and competition authorities corroborate that the arrangement now persists by inertia rather than ongoing function.
narrative_ontology:disappearance_verdict(market_naturalization__lapsed_alternative_reading, world_rearranges).
narrative_ontology:founding_problem_status(market_naturalization__lapsed_alternative_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(market_naturalization__lapsed_alternative_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(market_naturalization__lapsed_alternative_reading, 'none', 1).
narrative_ontology:epsilon_provenance(market_naturalization__lapsed_alternative_reading, 0.15, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(market_naturalization__lapsed_alternative_reading_tests).
:- end_tests(market_naturalization__lapsed_alternative_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.15 to reflect 'coordination costs only': the persistence of the dominant structure imposes diffuse costs (compatibility, legacy maintenance, reduced variety) but no concentrated extraction. Suppression is 0.10 because the reading explicitly denies active maintenance or enforcement; alternatives collapsed through atrophy, not coercion. Theater ratio is 0.75, high for a piton: the dominant structure persists largely as performance â actors continue to use it because it is the default, not because it optimizes outcomes. Accessibility collapse is 0.70 because alternatives have genuinely atrophied through non-use; restoring them would require coordinated collective action. Resistance is 0.15 because no party is hurt enough to mobilize against a diffuse, low-intensity cost. Temporal measurements trace the lifecycle decay from an active closure (t=0: epsilon=0.60, suppression=0.65) to the current lapsed state (t=40: epsilon=0.15, suppression=0.08), with theater_ratio rising as functional justification eroded.
 *
 * PERSPECTIVAL GAP:
 *   The incumbent seat experiences the constraint as a legacy coordination burden: they are locked into their own dominance by external expectations and compatibility obligations, unable to extract abnormal rents but also unable to unilaterally dissolve the structure. Downstream dependents experience it as a low-grade friction â individually rational to accept, collectively irrational to escape. Prospective entrants experience it as an invisible wall: no active gatekeeper to petition, just a coordination void. Regulators experience it as a category error â their tools assume an active suppressor, but the reading finds only inertia. The engine should compute seat divergence from the combination of low extractiveness, high theater, and differentiated exit options (constrained for all non-analytical seats).
 *
 * DIRECTIONALITY LOGIC:
 *   No beneficiaries are declared because this reading asserts no identifiable beneficiary class â a definitional feature of the lapsed closure. The engine will revert to canonical fallback directionality per power atom. In the absence of concentrated extraction, directionality is diffuse: the incumbent (powerful) sits slightly toward the beneficiary side by virtue of positional advantage and brand equity, while downstream dependents and consumers (moderate/organized) sit toward the target side as bearers of compatibility costs. The absence of victim declarations is deliberate: the reading treats these costs as coordination friction rather than asymmetric extraction, and declaring victims would falsely impute active suppression.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as piton prevents mislabeling the lapsed structure as either a rope (which would imply ongoing, benign coordination) or a snare (which would imply active, targeted extraction). The founding problem â providing a coordinating standard â is dead. The constraint persists by inertia. Labeling it a piton captures the atrophy: the original function is gone, the structure remains, and what sustains it is the absence of a coordination mechanism to escape, not the presence of a coordinating mechanism to stay. The high theater ratio (0.75) signals that most observed activity is performative maintenance of a legacy equilibrium rather than functional coordination.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    lapsed_vs_maintained_boundary,
    'Is the observed absence of active maintenance genuine, or has incumbent defense become so naturalized (embedded in standards, defaults, and expectations) that it appears lapsed to surface inspection?',
    'Deep ethnographic or process-tracing analysis of incumbent decision-making: if no resources are deployed to suppress alternatives and no rents are captured from dominance-specific mechanisms, the lapsed reading holds; if hidden enforcement or extraction is found, reclassify toward beneficiary_maintained.',
    'Would shift classification from piton to snare or tangled_rope if active defense is discovered; would confirm piton if genuine inertia is verified.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(lapsed_vs_maintained_boundary, empirical, 'Whether the lapsed state is genuine inertia or naturalized active defense.').

omega_variable(
    diffuse_cost_as_extraction,
    'Do the ''coordination costs only'' borne by downstream agents constitute extraction dressed as natural friction?',
    'Compare the incumbent''s profit rate and pricing power against a counterfactual competitive benchmark; if dominance yields abnormal returns, the cost is extractive despite appearing as inertia.',
    'Would raise extractiveness and potentially identify a hidden beneficiary class, invalidating the no-beneficiary claim.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(diffuse_cost_as_extraction, empirical, 'Whether diffuse coordination costs mask hidden extraction.').

omega_variable(
    path_dependence_efficiency,
    'Does the lapsed structure persist because it is efficient, or because of irreversible path dependence that locks in a suboptimal standard?',
    'Historical comparison with forked markets or regulatory-mandated transitions: if superior alternatives exist and would be adopted under coordinated switching, the persistence is pathological piton; if the dominant standard remains optimal, it may be a rope.',
    'Would distinguish between benign coordination legacy and inertial lock-in, affecting whether the constraint is read as piton or mountain-like natural equilibrium.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(path_dependence_efficiency, conceptual, 'Whether lapsed dominance is efficient equilibrium or pathological lock-in.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(market_naturalization__lapsed_alternative_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mark_tr_t0, market_naturalization__lapsed_alternative_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(mark_tr_t8, market_naturalization__lapsed_alternative_reading, theater_ratio, 8, 0.3).
narrative_ontology:measurement(mark_tr_t16, market_naturalization__lapsed_alternative_reading, theater_ratio, 16, 0.42).
narrative_ontology:measurement(mark_tr_t24, market_naturalization__lapsed_alternative_reading, theater_ratio, 24, 0.55).
narrative_ontology:measurement(mark_tr_t32, market_naturalization__lapsed_alternative_reading, theater_ratio, 32, 0.65).
narrative_ontology:measurement(mark_tr_t40, market_naturalization__lapsed_alternative_reading, theater_ratio, 40, 0.75).

% Extraction over time
narrative_ontology:measurement(mark_be_t0, market_naturalization__lapsed_alternative_reading, base_extractiveness, 0, 0.6).
narrative_ontology:measurement(mark_be_t8, market_naturalization__lapsed_alternative_reading, base_extractiveness, 8, 0.5).
narrative_ontology:measurement(mark_be_t16, market_naturalization__lapsed_alternative_reading, base_extractiveness, 16, 0.38).
narrative_ontology:measurement(mark_be_t24, market_naturalization__lapsed_alternative_reading, base_extractiveness, 24, 0.28).
narrative_ontology:measurement(mark_be_t32, market_naturalization__lapsed_alternative_reading, base_extractiveness, 32, 0.2).
narrative_ontology:measurement(mark_be_t40, market_naturalization__lapsed_alternative_reading, base_extractiveness, 40, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(mark_su_t0, market_naturalization__lapsed_alternative_reading, suppression_requirement, 0, 0.65).
narrative_ontology:measurement(mark_su_t8, market_naturalization__lapsed_alternative_reading, suppression_requirement, 8, 0.5).
narrative_ontology:measurement(mark_su_t16, market_naturalization__lapsed_alternative_reading, suppression_requirement, 16, 0.35).
narrative_ontology:measurement(mark_su_t24, market_naturalization__lapsed_alternative_reading, suppression_requirement, 24, 0.22).
narrative_ontology:measurement(mark_su_t32, market_naturalization__lapsed_alternative_reading, suppression_requirement, 32, 0.12).
narrative_ontology:measurement(mark_su_t40, market_naturalization__lapsed_alternative_reading, suppression_requirement, 40, 0.08).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(market_naturalization__lapsed_alternative_reading, market_naturalization__beneficiary_maintained_reading).
narrative_ontology:affects_constraint(market_naturalization__lapsed_alternative_reading, market_naturalization__hybrid_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the market_naturalization kernel, which decomposes into three structurally distinct claims about the maintenance of market dominance. The lapsed_alternative_reading claims epsilon near 0.15 (coordination costs only, no active maintenance), while the beneficiary_maintained_reading would claim substantially higher epsilon with active suppression. The hybrid_reading occupies the middle. They are linked as a constraint family because they share the same empirical domain but instantiate different structural relationships.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
