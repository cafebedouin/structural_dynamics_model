% ============================================================================
% CONSTRAINT STORY: market_naturalization__lapsed_alternative_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-11
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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:measurement_basis/2,
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
 *   constraint_id: market_naturalization__lapsed_alternative_reading
 *   human_readable: Naturalized Market Dominance as Lapsed Closure (No-Active-Maintenance Reading)
 *   domain: economic/political/historical
 *
 * SUMMARY:
 *   A staple market has been dominated by a single incumbent for roughly six
 *   decades. The dominant position arose out of a mid-century coordination
 *   failure — fragmented producers, incompatible specifications, unreliable
 *   counterparties — and crystallized around a common technical standard. On
 *   this reading of the arrangement, the closure of alternatives required
 *   upkeep only in its early decades; today nothing sustains it. No lobby
 *   defends the position, no contract bars the door, no agency patrols it.
 *   Rival channels emptied out through sheer disuse, would-be entrants
 *   allocate elsewhere because the surrounding complement ecosystem is gone,
 *   and the whole edifice persists the way a disused road grade persists:
 *   because it is there, and because nothing has a reason to remove it. Costs
 *   borne by consumers and dependent suppliers sit at the level of
 *   coordination service costs; no seat captures a closure-specific return.
 *   This story is ONE reading of the market_naturalization kernel (see
 *   commentary.kernel_context); it is decomposed into a three-file family
 *   because a single label — 'market dominance is natural' — conflates
 *   structurally distinct maintenance regimes with non-invariant epsilon. KEY
 *   AGENTS (by structural relationship): - dominant_incumbent_firm: Principal
 *   occupant (powerful/identity_locked) — sits atop the lapsed closure,
 *   collects scale-economy returns, defends nothing - household_consumers:
 *   Dual-positioned diffuse seat (powerless/constrained) — incidental
 *   collector of cheap reliable supply, bearer of the premium and lost
 *   variety - small_dependent_suppliers: Bearing seat (moderate/trapped) —
 *   margin-compressed specialists locked into the dominant specification -
 *   standards_body_secretariat: Administrator (institutional/constrained) —
 *   custodian of the standard's rules, could restructure, bears almost none
 *   of the costs - defunct_rival_producers: Historical bearing seat, now
 *   voiceless (powerless/trapped) — the atrophied alternatives' former
 *   carriers - would_be_entrants: Absent seat (moderate/mobile) — deterred
 *   not by barriers but by the missing ecosystem; their going elsewhere is
 *   the closure's continuation mechanism - economic_history_scholars:
 *   Analytical observer (analytical/analytical) — produced the archival
 *   record this reading rests on
 *
 * KEY AGENTS:
 *   - dominant_incumbent_firm: Principal occupant (powerful/identity_locked) — collects scale-economy returns, spends nothing on defense
 *   - household_consumers: Dual-positioned diffuse seat (powerless/constrained) — incidental beneficiary and premium-bearer
 *   - small_dependent_suppliers: Bearing seat (moderate/trapped) — specialized capacity locked to the dominant channel
 *   - standards_body_secretariat: Administrator (institutional/constrained) — could redraw access rules, has not in decades
 *   - defunct_rival_producers: Historical bearing seat, voiceless (powerless/trapped) — the atrophied alternatives
 *   - would_be_entrants: Absent seat (moderate/mobile) — deterred by missing complements, not by barriers
 *   - economic_history_scholars: Analytical observer (analytical/analytical) — archival auditors of the upkeep record
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(market_naturalization__lapsed_alternative_reading, 0.16).
domain_priors:suppression_score(market_naturalization__lapsed_alternative_reading, 0.12).
domain_priors:theater_ratio(market_naturalization__lapsed_alternative_reading, 0.06).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(market_naturalization__lapsed_alternative_reading, extractiveness, 0.16).
narrative_ontology:constraint_metric(market_naturalization__lapsed_alternative_reading, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(market_naturalization__lapsed_alternative_reading, theater_ratio, 0.06).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(market_naturalization__lapsed_alternative_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(market_naturalization__lapsed_alternative_reading, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(market_naturalization__lapsed_alternative_reading, piton).
narrative_ontology:human_readable(market_naturalization__lapsed_alternative_reading, "Naturalized Market Dominance as Lapsed Closure (No-Active-Maintenance Reading)").
narrative_ontology:topic_domain(market_naturalization__lapsed_alternative_reading, "economic/political/historical").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(market_naturalization__lapsed_alternative_reading, 'c885c6f2-699a-448b-bd9f-9bcb96dbcc44').
narrative_ontology:cs_kernel_codification('c885c6f2-699a-448b-bd9f-9bcb96dbcc44', distributed).
narrative_ontology:cs_authority_grounding('c885c6f2-699a-448b-bd9f-9bcb96dbcc44', expertise).
narrative_ontology:cs_interpretation_layer_present('c885c6f2-699a-448b-bd9f-9bcb96dbcc44').
narrative_ontology:cs_reading_relation('c885c6f2-699a-448b-bd9f-9bcb96dbcc44', market_naturalization__beneficiary_maintained_reading, forecloses).
narrative_ontology:cs_reading_relation('c885c6f2-699a-448b-bd9f-9bcb96dbcc44', market_naturalization__hybrid_reading, forecloses).
narrative_ontology:cs_axiom('c885c6f2-699a-448b-bd9f-9bcb96dbcc44', foundational, dominance_persists_without_defense).
narrative_ontology:cs_axiom_status(dominance_persists_without_defense, holdable).
narrative_ontology:cs_axiom_grounding('c885c6f2-699a-448b-bd9f-9bcb96dbcc44', dominance_persists_without_defense, empirically_contingent).
narrative_ontology:cs_axiom('c885c6f2-699a-448b-bd9f-9bcb96dbcc44', foundational, alternatives_atrophied_by_disuse).
narrative_ontology:cs_axiom_status(alternatives_atrophied_by_disuse, holdable).
narrative_ontology:cs_axiom_grounding('c885c6f2-699a-448b-bd9f-9bcb96dbcc44', alternatives_atrophied_by_disuse, empirically_contingent).
narrative_ontology:cs_axiom('c885c6f2-699a-448b-bd9f-9bcb96dbcc44', secondary, incumbent_returns_track_efficiency_not_closure).
narrative_ontology:cs_axiom_status(incumbent_returns_track_efficiency_not_closure, holdable).
narrative_ontology:cs_axiom_grounding('c885c6f2-699a-448b-bd9f-9bcb96dbcc44', incumbent_returns_track_efficiency_not_closure, empirically_contingent).
narrative_ontology:cs_reference_frame('c885c6f2-699a-448b-bd9f-9bcb96dbcc44', self_sustaining_market_default).
narrative_ontology:cs_drift_state('c885c6f2-699a-448b-bd9f-9bcb96dbcc44', contemporary_neobrandeisian_wave, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('c885c6f2-699a-448b-bd9f-9bcb96dbcc44', '').
narrative_ontology:cs_kernel_id(market_naturalization__lapsed_alternative_reading, market_naturalization).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(market_naturalization__lapsed_alternative_reading, dominant_incumbent_firm).
narrative_ontology:constraint_beneficiary(market_naturalization__lapsed_alternative_reading, household_consumers).
narrative_ontology:constraint_victim(market_naturalization__lapsed_alternative_reading, small_dependent_suppliers).
narrative_ontology:constraint_victim(market_naturalization__lapsed_alternative_reading, household_consumers).
narrative_ontology:constraint_victim(market_naturalization__lapsed_alternative_reading, defunct_rival_producers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Operates the large-scale production and distribution apparatus around which the staple market organized over six decades. Invests continuously in plant, logistics, and process efficiency; the archival record shows no sustained spending on lobbying, litigation, or contractual devices aimed at excluding rivals. Operating margins track measured scale economies. Walking away from the position would mean dismantling the firm's core lines and, with them, its sense of what it is.
narrative_ontology:constraint_stakeholder(market_naturalization__lapsed_alternative_reading, dominant_incumbent_firm, beneficiary,
    powerful, biographical, identity_locked, continental).

% Buy the staple cheaply and reliably through the dominant channel and plan household budgets around its stable specifications. Pay a modest premium over what a contested market would plausibly charge and choose from a narrower variety than decades ago. Substitution exists brand by brand, but the category default is settled and re-learning costs deter switching.
narrative_ontology:constraint_stakeholder(market_naturalization__lapsed_alternative_reading, household_consumers, beneficiary,
    powerless, biographical, constrained, continental).
narrative_ontology:stakeholder_secondary_role(market_naturalization__lapsed_alternative_reading, household_consumers, payer).

% Sell components and services into the dominant channel on posted terms, with margins held down to the level of the coordination services they consume. Plant and tooling are specified to the incumbent's standards, so redirecting output to any other channel means writing off specialized capacity.
narrative_ontology:constraint_stakeholder(market_naturalization__lapsed_alternative_reading, small_dependent_suppliers, payer,
    moderate, biographical, trapped, regional).

% Maintains the registry, versioning, and compatibility rules of the dominant technical standard. Processes revision requests as submitted and has initiated no restructuring of access rules in decades. Funded by flat member fees unrelated to market structure; its staff could in principle redraw the access architecture, but the analysis, balloting, and transition burden would dwarf the secretariat's budget and mandate.
narrative_ontology:constraint_stakeholder(market_naturalization__lapsed_alternative_reading, standards_body_secretariat, agenda_setter,
    institutional, generational, constrained, global).

% Once ran parallel production channels serving the same staple demand. Volumes migrated to the dominant standard across the interval's early decades and the firms exited; surviving assets sit idle, sold, or converted to other uses. None holds a seat in current trade bodies or standards ballots, and none is consulted about the arrangement that replaced them.
narrative_ontology:constraint_stakeholder(market_naturalization__lapsed_alternative_reading, defunct_rival_producers, payer,
    powerless, biographical, trapped, regional).
narrative_ontology:stakeholder_secondary_role(market_naturalization__lapsed_alternative_reading, defunct_rival_producers, excluded).

% Capital pools and management teams that periodically evaluate entering the staple market, find that rebuilding the vanished complement network — distributors, trained installers, spare-parts logistics — dominates projected returns, and allocate elsewhere instead. Nothing bars the door; the absence of the surrounding ecosystem does the deterring, and their going elsewhere is what keeps it absent.
narrative_ontology:constraint_stakeholder(market_naturalization__lapsed_alternative_reading, would_be_entrants, excluded,
    moderate, generational, mobile, global).

% Reconstruct the closure's origins in mid-century coordination failure and audit the subsequent record for signs of deliberate upkeep. Publish the archival studies on which the no-upkeep account rests, and dispute among themselves over isolated episodes that look like quiet discouragement rather than neglect.
narrative_ontology:constraint_stakeholder(market_naturalization__lapsed_alternative_reading, economic_history_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(market_naturalization__lapsed_alternative_reading, diffuse).
narrative_ontology:fixing_cost_class(market_naturalization__lapsed_alternative_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The market order coordinates exchange, price discovery, and capacity planning at continental scale without central direction; the dominant position supplies a settled technical standard and a stable set of expectations around which suppliers, distributors, and households organize their plans.
% TRANSFER_FUNCTION: Moves purchasing power from dispersed households and dependent suppliers to the incumbent as the price of transacting inside the settled order — at magnitudes that, on this reading, track the cost of the coordination services consumed rather than any closure-specific margin.
% ABSENT_VOICES: The defunct rival producers and the would-be entrants would contest the arrangement's harmlessness — the former as the parties the closure displaced, the latter as the parties it continues to deter — but both are absent: the former extinct as organizational voices, the latter scattered across other industries. Future generations who inherit the closure without having chosen it are absent in principle.
% DISAPPEARANCE_RATIONALE: If the dominant position dissolved overnight, supply contracts, distribution networks, installation trades, spare-parts logistics, and household price expectations would all reorganize around whatever successor structure emerged; the short run would be severely disruptive and the long run would settle into a differently shaped market. Arrangements across the economy depend on the settled order's continuing to exist.
% FOUNDING_PROBLEM: Mid-century coordination failure: fragmented producers, mutually incompatible specifications, unreliable counterparties, and no trusted settlement rail for the staple trade — a market unable to coordinate on a common standard.
% FOUNDING_PROBLEM_CORROBORATION: External corroboration exists: the economic-history literature and the antitrust-era legislative record attest both the founding coordination failure and its resolution, from seats outside the benefiting parties; business histories document the closure's crystallization. The incumbent firm publishes no founding-problem account of its own, and no beneficiary attests the problem's currency — which is itself signal that the founding justification has lapsed.
narrative_ontology:disappearance_verdict(market_naturalization__lapsed_alternative_reading, world_rearranges).
narrative_ontology:founding_problem_status(market_naturalization__lapsed_alternative_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(market_naturalization__lapsed_alternative_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(market_naturalization__lapsed_alternative_reading, 'none', 1).
narrative_ontology:epsilon_provenance(market_naturalization__lapsed_alternative_reading, 0.16, 'stealth/ox-alpha', 'none', direct).

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
 *   Extractiveness 0.16: the measurable costs borne inside the arrangement sit at the level of coordination service costs — transaction friction, spec-adherence overhead, the premium over a hypothetical contested-market price — with no identifiable seat collecting a closure-specific margin. Suppression 0.12: nothing applies force; what remains is switching friction and the discouraging absence of complements, which is inertia, not coercion. Theater 0.06: the reading's signature claim is the absence of maintenance, and the record shows upkeep-era rituals (anniversary leadership addresses, legacy reporting) quietly discontinued rather than performed. Accessibility_collapse 0.65: alternatives have already collapsed in fact — a participant surveying options finds essentially none viable — but not absolutely; funded complement rebuilds remain conceivable, which is why the figure sits below the natural-law band. Resistance 0.10: diffuse grumbling without a target, since there is no visible defender to resist. Claimed type piton follows the structural logic: a former coordination structure whose founding justification has expired, persisting by inertia, with no seat profiting enough to maintain it and no seat hurting enough to fix it. Metrics and claim were authored independently: if the engine computes divergence, that divergence is data. Temporal series run on the single shared grid {0,10,20,30,40,50,60}; no metric uses a private grid. The suppression_requirement series is deliberately omitted: there is no enforcement machinery whose build-up or erosion could be traced — the static scalar carries the whole enforcement picture. Trajectories are monotone-decaying (residual upkeep-era rents and rituals lapsing toward the coordination-cost floor), not cyclical; no intermittent-reinforcement dynamic is asserted.
 *
 * PERSPECTIVAL GAP:
 *   Identical structural facts classify differently by seat. From the incumbent's position the arrangement is an inherited operating environment: net gains, heavy load-bearing obligations, no adversary. From the dependent suppliers' position it is a margin ceiling they cannot route around, experienced as imposed. Consumers straddle: cheap reliable supply received, variety and bargaining power forfeited, with no awareness of either ledger. The secretariat experiences pure administration — revision requests in, ballots out — with no felt connection to the market structure its rules constitute. Excluded entrants experience nothing at all from inside, which is precisely the mechanism: their capital goes elsewhere and the closure deepens unopposed. The engine computes per-seat classifications from these structural differences; the claimed type does not adjudicate among them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations (dominant_incumbent_firm, household_consumers) pull those seats toward the subsidized pole; the victim declarations (small_dependent_suppliers, household_consumers' secondary bearing position, defunct_rival_producers) push bearing seats toward the full-target pole; trapped and identity_locked exits hold them there, while the entrants' mobile exit dampens their effective burden — they escape by investing elsewhere, which is itself how the alternatives stay atrophied. One override is declared: on beneficiary strength alone, the derivation would place the sole powerful-atom agent (the incumbent) near the full-beneficiary pole; under this reading it simultaneously bears the largest share of the arrangement's operating costs and books no closure-specific returns, so the override sets d=0.28 — a net gainer that is nonetheless materially loaded. No other seat's derivation is contested.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — mid-century coordination failure: no trusted common standard, unreliable counterparties, no settlement rail for the staple trade — is dead, solved so thoroughly that its solution became the unnoticed default. Yet the arrangement persists and the world would rearrange without it. That dead-problem-times-rearranging-world combination routes the story through the capture/zombie cross-check onto the piton/theater path, which is the classification claimed. The piton reading guards against mislabeling in both directions: against pure extraction (a capturer seat would have to exist; gain_flow='diffuse' records the affirmative, seat-by-seat check that none does — the incumbent's returns track scale economies, the secretariat's funding is flat member fees, consumers net out positively), and against pure coordination (a rope's founding justification is live and its participants are its beneficiaries by design; here the justification has lapsed and what remains is inertia wearing the shape of an institution). The residual risk to the whole reading is carried by the hidden_maintenance_channels omega: a forensic finding of systematic upkeep converts this story into its hybrid or beneficiary-maintained sibling.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    hidden_maintenance_channels,
    'Is the absence of observable upkeep genuine, or does maintenance occur through channels the public record misses — trade-association funding, patent thickets, procurement steering, standards-ballot influence?',
    'Forensic audit of incumbent trade-group expenditures, litigation dockets, lobbying registrations, and standards-body voting records across the interval.',
    'Systematic findings of upkeep collapse this reading into the hybrid or beneficiary-maintained sibling; extractiveness and suppression rise sharply and the piton classification fails.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(hidden_maintenance_channels, empirical, 'Whether zero-maintenance is a real structural fact or a recording artifact.').

omega_variable(
    atrophy_vs_quiet_strangulation,
    'Did rival channels fail through simple disuse, or through historical exclusive-dealing and pricing conduct that has since dropped out of living memory?',
    'Business-history reconstruction of specific rival failures: contract archives, shipping and distributorship records, contemporaneous trade-press coverage.',
    'A suppression component discovered in the closure''s origin retroactively validates the beneficiary-maintained reading''s mechanism and pushes this story toward tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(atrophy_vs_quiet_strangulation, empirical, 'Origin of the alternatives'' collapse: disuse versus suppressed exit.').

omega_variable(
    inertia_vs_natural_law_equivalence,
    'Is unmaintained inertial persistence classification-equivalent to natural law, or does counterfactual reopenability keep the arrangement in the constructed class?',
    'Test reopenability directly: examine whether any post-lapse entry succeeded where complement-network investment occurred; if rebuilds succeed when funded, degrees of freedom remain and the arrangement is not index-zero.',
    'Confirmation of reopenability holds the piton classification; demonstration that no funded rebuild can succeed would support mountain recertification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(inertia_vs_natural_law_equivalence, conceptual, 'Whether lapse-plus-inertia collapses into the natural-law category for classification purposes.').

omega_variable(
    diffuse_cost_ledger_completeness,
    'Is the payer-side ledger really coordination costs only, or do consumers and dependent suppliers bear unmeasured variety, quality, and resilience losses?',
    'Hedonic price studies against comparable contested categories; supply-disruption episodes as natural experiments measuring the cost of thin backup capacity.',
    'Material unmeasured losses raise effective costs borne by payer seats and could tip the aggregate classification upward despite the low headline figure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(diffuse_cost_ledger_completeness, empirical, 'Completeness of the coordination-costs-only accounting on the paying side.').

omega_variable(
    cs_authority_framing_underdetermination,
    'The declared commitment-system framing grounds authority in expertise (scholarly adjudication of the upkeep record); an equally coherent framing grounds authority in practice (the market''s frictionless daily operation is its own warrant, with no designated interpreter). Which framing governs?',
    'Inspect what signals the reading''s proponents actually cite: if archival audits and historiography carry the warrant, the expertise framing holds; if appeal is to the mere smoothness of ongoing commerce, the practice framing holds.',
    'Under the practice framing no designated interpreter exists, changing the computed commitment-system pattern and removing the interpretive-buffer reading of drift absorption.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cs_authority_framing_underdetermination, conceptual, 'Framing under-determination in the commitment-system classification of this reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(market_naturalization__lapsed_alternative_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mark_tr_t0, market_naturalization__lapsed_alternative_reading, theater_ratio, 0, 0.14).
narrative_ontology:measurement_basis(mark_tr_t0, observed).
narrative_ontology:measurement(mark_tr_t10, market_naturalization__lapsed_alternative_reading, theater_ratio, 10, 0.12).
narrative_ontology:measurement_basis(mark_tr_t10, observed).
narrative_ontology:measurement(mark_tr_t20, market_naturalization__lapsed_alternative_reading, theater_ratio, 20, 0.1).
narrative_ontology:measurement_basis(mark_tr_t20, observed).
narrative_ontology:measurement(mark_tr_t30, market_naturalization__lapsed_alternative_reading, theater_ratio, 30, 0.09).
narrative_ontology:measurement_basis(mark_tr_t30, observed).
narrative_ontology:measurement(mark_tr_t40, market_naturalization__lapsed_alternative_reading, theater_ratio, 40, 0.08).
narrative_ontology:measurement_basis(mark_tr_t40, observed).
narrative_ontology:measurement(mark_tr_t50, market_naturalization__lapsed_alternative_reading, theater_ratio, 50, 0.07).
narrative_ontology:measurement_basis(mark_tr_t50, observed).
narrative_ontology:measurement(mark_tr_t60, market_naturalization__lapsed_alternative_reading, theater_ratio, 60, 0.06).
narrative_ontology:measurement_basis(mark_tr_t60, observed).

% Extraction over time
narrative_ontology:measurement(mark_be_t0, market_naturalization__lapsed_alternative_reading, base_extractiveness, 0, 0.24).
narrative_ontology:measurement_basis(mark_be_t0, observed).
narrative_ontology:measurement(mark_be_t10, market_naturalization__lapsed_alternative_reading, base_extractiveness, 10, 0.21).
narrative_ontology:measurement_basis(mark_be_t10, observed).
narrative_ontology:measurement(mark_be_t20, market_naturalization__lapsed_alternative_reading, base_extractiveness, 20, 0.19).
narrative_ontology:measurement_basis(mark_be_t20, observed).
narrative_ontology:measurement(mark_be_t30, market_naturalization__lapsed_alternative_reading, base_extractiveness, 30, 0.18).
narrative_ontology:measurement_basis(mark_be_t30, observed).
narrative_ontology:measurement(mark_be_t40, market_naturalization__lapsed_alternative_reading, base_extractiveness, 40, 0.17).
narrative_ontology:measurement_basis(mark_be_t40, observed).
narrative_ontology:measurement(mark_be_t50, market_naturalization__lapsed_alternative_reading, base_extractiveness, 50, 0.16).
narrative_ontology:measurement_basis(mark_be_t50, observed).
narrative_ontology:measurement(mark_be_t60, market_naturalization__lapsed_alternative_reading, base_extractiveness, 60, 0.16).
narrative_ontology:measurement_basis(mark_be_t60, observed).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(market_naturalization__lapsed_alternative_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(market_naturalization__lapsed_alternative_reading, resource_allocation).
narrative_ontology:affects_constraint(market_naturalization__lapsed_alternative_reading, market_naturalization__beneficiary_maintained_reading).
narrative_ontology:affects_constraint(market_naturalization__lapsed_alternative_reading, market_naturalization__hybrid_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the market_naturalization kernel decomposes into three readings distinguished by the maintenance predicate. This file is the lapsed_alternative_reading (no upkeep, no capturer, epsilon 0.16); the beneficiary_maintained_reading file authors high epsilon with identified incumbent collectors; the hybrid_reading file authors a mixed structure. Each links to the others via affects_constraints; upstream/downstream evidential flow runs from the archival record outward — whichever reading the forensic maintenance audit supports inherits legitimacy conditions from the record and pressures the other two.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(market_naturalization__lapsed_alternative_reading, powerful, 0.28).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
