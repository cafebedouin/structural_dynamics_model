% ============================================================================
% CONSTRAINT STORY: market_naturalization__lapsed_alternative_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
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
 *   human_readable: Market Dominance as Lapsed Closure (No Active Maintenance Reading)
 *   domain: political_economy/economic_history/institutional_analysis
 *
 * SUMMARY:
 *   This constraint story instantiates the lapsed_alternative_reading of the
 *   market_naturalization kernel. The reading holds that market dominance
 *   structures — once actively maintained through entry barriers, regulatory
 *   capture, and strategic deterrence by incumbent capital holders — have
 *   lapsed into a self-sustaining inertial state. No identifiable class now
 *   actively defends or profits from the dominance; it persists because
 *   alternative coordinating mechanisms (decentralized markets, platform
 *   cooperatives, public utilities) atrophied through non-use during the
 *   active-maintenance phase. The constraint now operates as a piton: a
 *   degraded former snare/tangled_rope whose primary extractive function has
 *   atrophied, leaving only coordination-cost-level extractiveness (ε≈0.18)
 *   and performative maintenance (theater_ratio≈0.52). Suppression has
 *   decayed from active enforcement (0.45 at t=0) to residual structural
 *   friction (0.12 at t=30). The reading claims the constraint is a 'lapsed
 *   closure' — the closure event happened historically, and what remains is
 *   the sedimented structure requiring no active maintenance.
 *
 * KEY AGENTS:
 *   - competition_authorities: Potential agenda_setter (institutional/analytical) — could reactivate enforcement but treats dominance as natural market outcome
 *   - dispersed_market_participants: Payers (moderate/constrained) — bear diffuse efficiency losses from absent alternatives, no coordinated exit
 *   - potential_entrants: Payers/excluded (powerless/trapped) — face atrophied entry paths, not active barriers
 *   - market_incumbents: Beneficiary/payer dual (powerful/constrained) — incidentally advantaged by inertia, not active rent collection; bear costs of maintaining scale without competitive pressure
 *   - economic_analysts: Observers (analytical/analytical) — contest whether dominance is lapsed, actively maintained, or hybrid
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(market_naturalization__lapsed_alternative_reading, 0.18).
domain_priors:suppression_score(market_naturalization__lapsed_alternative_reading, 0.12).
domain_priors:theater_ratio(market_naturalization__lapsed_alternative_reading, 0.52).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(market_naturalization__lapsed_alternative_reading, extractiveness, 0.18).
narrative_ontology:constraint_metric(market_naturalization__lapsed_alternative_reading, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(market_naturalization__lapsed_alternative_reading, theater_ratio, 0.52).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(market_naturalization__lapsed_alternative_reading, accessibility_collapse, 0.71).
narrative_ontology:constraint_metric(market_naturalization__lapsed_alternative_reading, resistance, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(market_naturalization__lapsed_alternative_reading, piton).
narrative_ontology:human_readable(market_naturalization__lapsed_alternative_reading, "Market Dominance as Lapsed Closure (No Active Maintenance Reading)").
narrative_ontology:topic_domain(market_naturalization__lapsed_alternative_reading, "political_economy/economic_history/institutional_analysis").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(market_naturalization__lapsed_alternative_reading, '1e37923d-1017-4f23-984d-6c20fcec9f8d').
narrative_ontology:cs_kernel_codification('1e37923d-1017-4f23-984d-6c20fcec9f8d', distributed).
narrative_ontology:cs_authority_grounding('1e37923d-1017-4f23-984d-6c20fcec9f8d', distributed).
narrative_ontology:cs_reading_relation('1e37923d-1017-4f23-984d-6c20fcec9f8d', market_naturalization__beneficiary_maintained_reading, coexists_with).
narrative_ontology:cs_reading_relation('1e37923d-1017-4f23-984d-6c20fcec9f8d', market_naturalization__hybrid_reading, coexists_with).
narrative_ontology:cs_axiom('1e37923d-1017-4f23-984d-6c20fcec9f8d', foundational, active_maintenance_phase_ended).
narrative_ontology:cs_axiom_status(active_maintenance_phase_ended, holdable).
narrative_ontology:cs_axiom_grounding('1e37923d-1017-4f23-984d-6c20fcec9f8d', active_maintenance_phase_ended, empirically_contingent).
narrative_ontology:cs_axiom('1e37923d-1017-4f23-984d-6c20fcec9f8d', foundational, no_identifiable_beneficiary_class).
narrative_ontology:cs_axiom_status(no_identifiable_beneficiary_class, holdable).
narrative_ontology:cs_axiom_grounding('1e37923d-1017-4f23-984d-6c20fcec9f8d', no_identifiable_beneficiary_class, empirically_contingent).
narrative_ontology:cs_reference_frame('1e37923d-1017-4f23-984d-6c20fcec9f8d', active_closure_maintenance_phase).
narrative_ontology:cs_drift_state('1e37923d-1017-4f23-984d-6c20fcec9f8d', contemporary_post_neoliberal_consolidation, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('1e37923d-1017-4f23-984d-6c20fcec9f8d', '').
narrative_ontology:cs_kernel_id(market_naturalization__lapsed_alternative_reading, market_naturalization).

% --- Structural relationships ---
narrative_ontology:constraint_victim(market_naturalization__lapsed_alternative_reading, dispersed_market_participants).
narrative_ontology:constraint_victim(market_naturalization__lapsed_alternative_reading, potential_entrants).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(market_naturalization__lapsed_alternative_reading, market_incumbents).
narrative_ontology:constraint_victim(market_naturalization__lapsed_alternative_reading, market_incumbents).
narrative_ontology:constraint_vindicates(market_naturalization__lapsed_alternative_reading, spontaneous_order_efficiency).
narrative_ontology:constraint_vindicates(market_naturalization__lapsed_alternative_reading, institutional_path_dependence).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold statutory authority to investigate and restructure market dominance. In this reading's framing, they treat dominance as a natural market outcome and do not exercise their authority. Their inaction is not capture but analytical closure — the constraint appears to them as mountain. They could change it with a policy decision; the cost to fix is prohibitive relative to their mandate because it would require reconstructing atrophied alternatives.
narrative_ontology:constraint_stakeholder(market_naturalization__lapsed_alternative_reading, competition_authorities, agenda_setter,
    institutional, generational, analytical, global).

% Incumbent firms that dominate their markets. In this reading, they do not actively defend dominance — they simply operate within it. They incidentally benefit from atrophied alternatives (no competitive pressure, scale advantages), but they also bear costs: maintaining scale without competitive discipline creates inefficiency, and they cannot exit the dominance structure without losing their market position. They are not the agenda_setter because they do not set the rules; the rules lapsed.
narrative_ontology:constraint_stakeholder(market_naturalization__lapsed_alternative_reading, market_incumbents, beneficiary,
    powerful, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(market_naturalization__lapsed_alternative_reading, market_incumbents, payer).

% Consumers, workers, small firms, and suppliers who operate within the dominant market structure. They bear diffuse efficiency losses: higher prices, less innovation, fewer choices — but these are not extracted by an identifiable collector. The losses come from the absence of alternatives that once existed. Exit is constrained: they cannot individually reconstruct the missing alternatives, and collective action to do so faces the same coordination problem the dominance structure originally solved.
narrative_ontology:constraint_stakeholder(market_naturalization__lapsed_alternative_reading, dispersed_market_participants, payer,
    moderate, biographical, constrained, global).

% Firms or cooperatives that would enter the market if viable paths existed. The entry paths — distribution networks, standard-setting bodies, capital access channels — atrophied during the active-maintenance phase and were never rebuilt. They face not active barriers but missing infrastructure. Their situation is trapped: the constraint blocks them not by force but by absence of the means to act.
narrative_ontology:constraint_stakeholder(market_naturalization__lapsed_alternative_reading, potential_entrants, payer,
    powerless, immediate, trapped, global).

% Scholars and analysts who study market structure. They hold the three readings as live analytical positions: some see active maintenance (beneficiary_maintained), some see hybrid, some see lapsed closure (this reading). Their disagreement is not about values but about the current structural state of the constraint. They do not collect or pay; they diagnose.
narrative_ontology:constraint_stakeholder(market_naturalization__lapsed_alternative_reading, economic_analysts, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(market_naturalization__lapsed_alternative_reading, diffuse).
narrative_ontology:fixing_cost_class(market_naturalization__lapsed_alternative_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinating expectations and reducing transaction costs in complex economic activity by establishing a dominant standard or platform that all participants can rely on — the 'single trusted marketplace' function at economy-wide scale.
% TRANSFER_FUNCTION: Under this reading, the arrangement does not actively transfer surplus from payers to a beneficiary class. The diffuse costs borne by dispersed_participants and potential_entrants (higher search costs, lost innovation, foregone alternatives) are not collected by anyone — they are deadweight loss from atrophied coordination capacity. The market_incumbents' incidental advantages are not a transfer but a residual of scale without competitive pressure.
% ABSENT_VOICES: The voices of potential alternative coordinating mechanisms — platform cooperatives, public utility models, decentralized protocol governance, mutualist federations — are absent because the structures that would articulate them atrophied during the active-maintenance phase. They are not merely excluded from the conversation; the conversation's preconditions (viable alternatives with organized constituencies) were erased historically.
% DISAPPEARANCE_RATIONALE: If the dominance structure vanished overnight, the immediate result would be chaos — not because the structure was actively coordinating, but because the alternative coordinating mechanisms that would replace it have atrophied. The world would rearrange through a painful reconstruction period: new entry paths would need to be built, trust networks re-established, standards re-negotiated. The rearrangement would be costly and contested, not smooth.
% FOUNDING_PROBLEM: The problem of coordinating complex, large-scale economic activity without centralized planning — specifically, how to achieve the efficiency gains of standardization and network effects while avoiding the fragility of monopoly.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem is attested by Hayek (knowledge problem), Coase (transaction cost economics), and Williamson (governance structures) — none of whom are beneficiaries of market dominance. The contention: Hayekians argue the problem is live and markets solve it spontaneously; institutionalists (North, Ostrom) argue the problem requires active institutional design that has been neglected; critical political economists (Polanyi, Block) argue the problem was never solved by markets alone but by embedded social structures that have been dismantled.
narrative_ontology:disappearance_verdict(market_naturalization__lapsed_alternative_reading, world_rearranges).
narrative_ontology:founding_problem_status(market_naturalization__lapsed_alternative_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(market_naturalization__lapsed_alternative_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(market_naturalization__lapsed_alternative_reading, 'none', 1).
narrative_ontology:epsilon_provenance(market_naturalization__lapsed_alternative_reading, 0.18, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Extractiveness is low (0.18) because no active rent-extraction apparatus operates — the constraint's operation now consists of coordination costs (search, switching, network effects) that would exist under any dominant standard. Suppression is low (0.12) because no active enforcement machinery operates; residual friction comes from atrophied alternatives, not active barriers. Theater ratio is moderate-high (0.52) because institutional rhetoric (competition policy, innovation discourse) performs the maintenance that active enforcement once did. Accessibility collapse is high (0.71) because alternatives didn't just lose — they disappeared, making reconstruction costly. Resistance is near-zero (0.15) because no one experiences active coercion; the constraint feels like 'how markets work.' The claimed type is piton: a former extraction structure whose function atrophied, persisting through inertia and performative discourse.
 *
 * PERSPECTIVAL GAP:
 *   From the competition_authorities seat (analytical), the constraint appears as a mountain — market dominance is the natural outcome of efficiency. From dispersed_market_participants (moderate/constrained), it appears as a piton — they bear diffuse costs but see no one to blame, no lever to pull. From potential_entrants (powerless/trapped), it appears as a snare — entry paths are gone, but they cannot identify the suppressor. The engine computes this divergence from the structural data: same constraint, different effective extraction by seat.
 *
 * DIRECTIONALITY LOGIC:
 *   No beneficiary class is declared — this is the reading's core claim. Market incumbents are not listed as beneficiaries because the reading asserts they do not actively collect rents; any advantage they hold is incidental to scale, not extracted via the constraint. Dispersed_market_participants and potential_entrants are victims (payers) bearing diffuse costs of atrophied alternatives. Competition authorities are the agenda_setter — they could reactivate enforcement (change the constraint) but do not, treating the arrangement as natural. Directionality derives: incumbents d≈0.3 (incidental advantage, constrained exit), participants d≈0.6 (bear costs, constrained exit), entrants d≈0.8 (fully blocked by atrophied paths), authorities d≈0.1 (beneficiary of analytical closure).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — coordinating complex economic activity without centralized planning — remains live (status: contested). But the specific arrangement (market dominance as coordinating mechanism) has outlived its active-maintenance phase. The mandatrophy is resolved in the sense that the original mandate (active coordination via dominant standards) has lapsed; what remains is the sedimented structure. This prevents mislabeling the current inertial state as active coordination (rope) or active extraction (snare). The piton classification captures: the arrangement once solved a coordination problem actively (rope/snare phase), that function atrophied, and the constraint persists without a party benefiting enough to maintain it or hurt enough to fix it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    committer_kernel_reading_structure,
    'Is the market_naturalization kernel genuinely contested across these three readings, or does the lapsed_alternative_reading describe a historical phase that the other readings describe as ongoing?',
    'Historical-institutional analysis of whether active maintenance mechanisms (lobbying, regulatory capture, strategic entry deterrence) have ceased or merely become latent. Compare contemporary enforcement actions against the historical record of the kernel''s crystallization period.',
    'If active maintenance persists covertly, this reading''s low extractiveness claim is false and the constraint reclassifies toward tangled_rope or snare under the beneficiary_maintained_reading. If maintenance has genuinely lapsed, the piton classification holds and the kernel''s contestation is about historical sequencing, not current structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(committer_kernel_reading_structure, empirical, 'Whether the kernel''s three readings represent simultaneous analytical positions or sequential historical phases.').

omega_variable(
    beneficiary_class_latency,
    'Does ''no identifiable beneficiary class'' mean no beneficiaries exist, or that benefits have diffused below detection threshold while the constraint''s inertial structure still advantages incumbents?',
    'Counterfactual simulation: remove the dominance structure and measure surplus reallocation. If incumbents lose disproportionately, a latent beneficiary class exists despite no active rent-collection apparatus.',
    'Latent beneficiaries would convert this reading''s piton classification toward snare/tangled_rope under the hybrid_reading, and validate the beneficiary_maintained_reading''s core premise.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(beneficiary_class_latency, empirical, 'Whether diffuse/incidental advantages constitute a beneficiary class for classification purposes.').

omega_variable(
    alternatives_atrophy_mechanism,
    'Did alternatives atrophy through genuine non-use (coordination failure, path dependence) or through passive suppression (incumbents'' latent deterrence, regulatory friction maintained without active enforcement)?',
    'Compare entry rates and innovation patterns in adjacent markets where the dominance structure is absent vs. present. If entry is systematically lower only where dominance persists, passive suppression is operative.',
    'Passive suppression implies the constraint still extracts via deterrence, raising effective extractiveness and shifting classification toward tangled_rope. Genuine non-use supports the lapsed closure narrative.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternatives_atrophy_mechanism, empirical, 'Distinguishing path-dependent atrophy from latent deterrence as the cause of alternative collapse.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(market_naturalization__lapsed_alternative_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mnlr_tr_t0, market_naturalization__lapsed_alternative_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(mnlr_tr_t10, market_naturalization__lapsed_alternative_reading, theater_ratio, 10, 0.38).
narrative_ontology:measurement(mnlr_tr_t20, market_naturalization__lapsed_alternative_reading, theater_ratio, 20, 0.46).
narrative_ontology:measurement(mnlr_tr_t30, market_naturalization__lapsed_alternative_reading, theater_ratio, 30, 0.52).

% Extraction over time
narrative_ontology:measurement(mnlr_be_t0, market_naturalization__lapsed_alternative_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(mnlr_be_t10, market_naturalization__lapsed_alternative_reading, base_extractiveness, 10, 0.28).
narrative_ontology:measurement(mnlr_be_t20, market_naturalization__lapsed_alternative_reading, base_extractiveness, 20, 0.22).
narrative_ontology:measurement(mnlr_be_t30, market_naturalization__lapsed_alternative_reading, base_extractiveness, 30, 0.18).

% Suppression requirement over time
narrative_ontology:measurement(mnlr_su_t0, market_naturalization__lapsed_alternative_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(mnlr_su_t10, market_naturalization__lapsed_alternative_reading, suppression_requirement, 10, 0.32).
narrative_ontology:measurement(mnlr_su_t20, market_naturalization__lapsed_alternative_reading, suppression_requirement, 20, 0.18).
narrative_ontology:measurement(mnlr_su_t30, market_naturalization__lapsed_alternative_reading, suppression_requirement, 30, 0.12).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(market_naturalization__lapsed_alternative_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(market_naturalization__lapsed_alternative_reading, 0.15).
narrative_ontology:affects_constraint(market_naturalization__lapsed_alternative_reading, market_naturalization__beneficiary_maintained_reading).
narrative_ontology:affects_constraint(market_naturalization__lapsed_alternative_reading, market_naturalization__hybrid_reading).

% DUAL FORMULATION NOTE:
% This constraint and its two siblings form the market_naturalization constraint family. Each reading instantiates a different constraint with distinct ε, beneficiary/victim structure, and claimed_type. The lapsed_alternative_reading (this story) sees ε≈0.18, no beneficiaries, piton. The beneficiary_maintained_reading sees high ε, active beneficiaries (incumbent_capital_holders), snare/tangled_rope. The hybrid_reading sees intermediate ε, partial beneficiaries, tangled_rope. They are linked via affects_constraints because the historical active-maintenance phase (beneficiary_maintained) causally produced the atrophied alternatives that the lapsed reading describes.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(market_naturalization__lapsed_alternative_reading, powerful, 0.3).
constraint_indexing:directionality_override(market_naturalization__lapsed_alternative_reading, moderate, 0.6).
constraint_indexing:directionality_override(market_naturalization__lapsed_alternative_reading, powerless, 0.8).
constraint_indexing:directionality_override(market_naturalization__lapsed_alternative_reading, institutional, 0.1).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
