% ============================================================================
% CONSTRAINT STORY: market_naturalization__hybrid_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_market_naturalization__hybrid_reading, []).

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
    narrative_ontology:measurement_basis/2,
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
 *   constraint_id: market_naturalization__hybrid_reading
 *   human_readable: Market Dominance as Hybrid Lapse-and-Maintenance Structure
 *   domain: political economy/economic history/institutional analysis
 *
 * SUMMARY:
 *   In concentrated markets, dominance presents itself as a single thing —
 *   the leader's position — but it is held together by two different kinds of
 *   material. Part of the structure has lapsed into inertia: installed bases,
 *   switching costs, habituated procurement, and network effects that
 *   reproduce themselves year over year with nobody tending them. Another
 *   part is actively maintained: exclusive dealing renewed contract by
 *   contract, regulatory barriers lobbied for session by session, nascent
 *   rivals acquired as they emerge, standards bodies chaired and steered. The
 *   beneficiaries are the incumbent firms that book supra-competitive margins
 *   and the capital holders whose portfolios ride on margin durability. Those
 *   bearing the costs are the entrants who cannot reach the core, the
 *   suppliers locked to a single dominant channel, and the consumers who pay
 *   above-competitive prices for the genuine conveniences the structure also
 *   delivers. Extraction is moderate and uneven — heavier where maintenance
 *   is active and exit is blocked, lighter where the position rests on
 *   efficiencies customers would voluntarily pay for. KEY AGENTS (by
 *   structural relationship): - incumbent_market_leaders: Primary
 *   agenda-setter and collector (institutional/arbitrage) — administers the
 *   maintained elements, books the margins - incumbent_capital_holders:
 *   Passive beneficiary (powerful/arbitrage) — collects through ownership,
 *   administers nothing - would_be_entrants: Primary target
 *   (moderate/constrained) — bears the maintained barriers -
 *   dependent_small_suppliers: Primary target (powerless/trapped) — bears the
 *   lapsed-and-inherited dependencies - captive_consumers: Split seat
 *   (powerless/constrained) — pays the margin, receives the coordination -
 *   displaced_alternative_producers: Excluded voice (moderate/trapped) — the
 *   suppressed-alternatives residue - competition_authorities: Analytical
 *   observer (institutional/analytical) — episodic external check
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(market_naturalization__hybrid_reading, 0.58).
domain_priors:suppression_score(market_naturalization__hybrid_reading, 0.52).
domain_priors:theater_ratio(market_naturalization__hybrid_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(market_naturalization__hybrid_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(market_naturalization__hybrid_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(market_naturalization__hybrid_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(market_naturalization__hybrid_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(market_naturalization__hybrid_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(market_naturalization__hybrid_reading, tangled_rope).
narrative_ontology:human_readable(market_naturalization__hybrid_reading, "Market Dominance as Hybrid Lapse-and-Maintenance Structure").
narrative_ontology:topic_domain(market_naturalization__hybrid_reading, "political economy/economic history/institutional analysis").

domain_priors:requires_active_enforcement(market_naturalization__hybrid_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(market_naturalization__hybrid_reading, 'e5270196-1c36-477d-9679-d6b6da82fd9d').
narrative_ontology:cs_kernel_codification('e5270196-1c36-477d-9679-d6b6da82fd9d', distributed).
narrative_ontology:cs_authority_grounding('e5270196-1c36-477d-9679-d6b6da82fd9d', distributed).
narrative_ontology:cs_reading_relation('e5270196-1c36-477d-9679-d6b6da82fd9d', market_naturalization__lapsed_alternative_reading, forecloses).
narrative_ontology:cs_reading_relation('e5270196-1c36-477d-9679-d6b6da82fd9d', market_naturalization__beneficiary_maintained_reading, forecloses).
narrative_ontology:cs_axiom('e5270196-1c36-477d-9679-d6b6da82fd9d', foundational, dominance_persists_by_mixed_lapse_and_defense).
narrative_ontology:cs_axiom_status(dominance_persists_by_mixed_lapse_and_defense, holdable).
narrative_ontology:cs_axiom_grounding('e5270196-1c36-477d-9679-d6b6da82fd9d', dominance_persists_by_mixed_lapse_and_defense, empirically_contingent).
narrative_ontology:cs_axiom('e5270196-1c36-477d-9679-d6b6da82fd9d', secondary, maintenance_intensity_varies_by_sector).
narrative_ontology:cs_axiom_status(maintenance_intensity_varies_by_sector, holdable).
narrative_ontology:cs_axiom_grounding('e5270196-1c36-477d-9679-d6b6da82fd9d', maintenance_intensity_varies_by_sector, empirically_contingent).
narrative_ontology:cs_reference_frame('e5270196-1c36-477d-9679-d6b6da82fd9d', efficiency_founded_dominance).
narrative_ontology:cs_drift_state('e5270196-1c36-477d-9679-d6b6da82fd9d', contemporary_platform_concentration_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('e5270196-1c36-477d-9679-d6b6da82fd9d', '').
narrative_ontology:cs_kernel_id(market_naturalization__hybrid_reading, market_naturalization).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(market_naturalization__hybrid_reading, incumbent_market_leaders).
narrative_ontology:constraint_beneficiary(market_naturalization__hybrid_reading, incumbent_capital_holders).
narrative_ontology:constraint_victim(market_naturalization__hybrid_reading, would_be_entrants).
narrative_ontology:constraint_victim(market_naturalization__hybrid_reading, dependent_small_suppliers).
narrative_ontology:constraint_victim(market_naturalization__hybrid_reading, captive_consumers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(market_naturalization__hybrid_reading, captive_consumers).
narrative_ontology:constraint_vindicates(market_naturalization__hybrid_reading, consumer_welfare_standard).
narrative_ontology:constraint_vindicates(market_naturalization__hybrid_reading, meritocratic_market_allocation).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Operate the dominant position in concentrated sectors. Where the moat erodes they defend it deliberately: signing suppliers to exclusivity, lobbying for licensing rules that raise rivals' costs, acquiring nascent competitors as they emerge, chairing and steering standards committees. Where the position rests on accumulated habit — installed equipment, trained workforces, default procurement — they simply operate it. They book margins well above competitive levels and can redeploy capital across sectors and borders if a particular market turns hostile.
narrative_ontology:constraint_stakeholder(market_naturalization__hybrid_reading, incumbent_market_leaders, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(market_naturalization__hybrid_reading, incumbent_market_leaders, beneficiary).

% Hold equity in the dominant firms through direct stakes and index funds. Dividends and share appreciation track the durability of the margins; they administer nothing and decide little day to day, but their allocations reward whichever management defends the position best, and they can move capital to other sectors or jurisdictions at will.
narrative_ontology:constraint_stakeholder(market_naturalization__hybrid_reading, incumbent_capital_holders, beneficiary,
    powerful, biographical, arbitrage, global).

% Try to build businesses inside markets the incumbents occupy. Where defenses are actively tended they meet exclusive contracts they cannot match, distribution channels closed to them, and acquisition offers calibrated to be hard to refuse. Where defenses are merely inherited they face switching costs and network effects built up over decades. Entry into peripheral niches is feasible; entry into the core usually ends in acquisition or retreat.
narrative_ontology:constraint_stakeholder(market_naturalization__hybrid_reading, would_be_entrants, payer,
    moderate, biographical, constrained, national).

% Sell to, or buy through, one dominant firm's channel. Terms arrive as take-it-or-leave-it revisions; payment delays and margin squeezes are absorbed because the alternative is losing the primary revenue relationship. Years of tooling and process investment tuned to the dominant customer's specifications make leaving a write-off, not a transition.
narrative_ontology:constraint_stakeholder(market_naturalization__hybrid_reading, dependent_small_suppliers, payer,
    powerless, immediate, trapped, regional).

% Buy standardized products that interoperate and rarely fail — a real convenience the consolidated structure delivers. They also pay prices above competitive benchmarks and choose among fewer varieties. Switching means abandoning compatible accessories, learned interfaces, and service networks, so most stay and grumble.
narrative_ontology:constraint_stakeholder(market_naturalization__hybrid_reading, captive_consumers, payer,
    powerless, immediate, constrained, national).
narrative_ontology:stakeholder_secondary_role(market_naturalization__hybrid_reading, captive_consumers, beneficiary).

% Backed rival technical standards and rival channels that lost out — some through their own missteps, some through exclusive pre-installation deals and patent thickets they could not clear. They retain niche footholds and institutional memory of the road not taken; their voice in current market-design debates is marginal.
narrative_ontology:constraint_stakeholder(market_naturalization__hybrid_reading, displaced_alternative_producers, excluded,
    moderate, biographical, trapped, national).

% Investigate mergers, exclusive dealing, and predatory conduct; bring cases that occasionally unwind a maintained element. Enforcement capacity swings with administrations, and their case files are the best external record of which defenses are actively tended and which persist untended.
narrative_ontology:constraint_stakeholder(market_naturalization__hybrid_reading, competition_authorities, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(market_naturalization__hybrid_reading, incumbent_market_leaders).
narrative_ontology:fixing_cost_class(market_naturalization__hybrid_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Concentrated market structures solve recurring coordination problems once: a dominant standard makes components interoperable, a dominant channel gives suppliers predictable access terms, scale production lowers unit costs. Participants coordinate on the incumbent's terms instead of renegotiating market structure transaction by transaction.
% TRANSFER_FUNCTION: Moves supra-competitive margin from captive consumers and dependent suppliers to incumbent firms and, through distributions and appreciation, to their capital holders; moves standard-setting and market-access decisions from open competition to incumbent discretion.
% ABSENT_VOICES: Would-be entrants and displaced alternative producers are outside the conversation — the entry barriers are precisely what keeps them out. Consumers appear only as aggregated demand data, never as organized voice. Antitrust constituencies speak intermittently, between enforcement waves, and their capacity is itself a political variable the beneficiaries lobby over.
% DISAPPEARANCE_RATIONALE: If the dominance arrangements vanished overnight, standards would fragment and then re-coordinate competitively, supplier networks would diversify, margins would compress toward cost, and capital would redeploy — the industrial landscape would visibly reorganize over a period of years, with the heaviest rearrangement exactly where maintenance was most active.
% FOUNDING_PROBLEM: Late nineteenth-century markets were fragmented and violently cyclical: destructive price wars, incompatible technical standards, unreliable quality, chronic overcapacity. Consolidation promised stability, scale economies, and interoperability.
% FOUNDING_PROBLEM_CORROBORATION: Economic historians in the Chandler-lineage scale-economy studies attest the founding problem was real and was substantially solved in core industries — corroboration from outside the benefiting parties. Contemporary competition economists and antitrust case records attest that in many mature sectors the problem is now dead while the arrangements persist; sector-level audits by scholars with no incumbent affiliation support the shifted-function reading for those sectors.
narrative_ontology:disappearance_verdict(market_naturalization__hybrid_reading, world_rearranges).
narrative_ontology:founding_problem_status(market_naturalization__hybrid_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(market_naturalization__hybrid_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(market_naturalization__hybrid_reading, 'none', 1).
narrative_ontology:epsilon_provenance(market_naturalization__hybrid_reading, 0.58, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(market_naturalization__hybrid_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(market_naturalization__hybrid_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(market_naturalization__hybrid_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Claimed type is tangled_rope from this reading's own lights: the arrangement coordinates (interoperable standards, scale economies, reliable channels) and extracts asymmetrically through the same structure, and the maintained portion requires continuous enforcement effort. The metrics are authored independently as descriptions of operation. Extractiveness sits at 0.58 — clearly above coordination cost, short of a near-total capture take, because a substantial share of incumbent margin still purchases real scale and compatibility economies. Suppression 0.52: alternatives are not impossible, they are priced and lawyered out of reach — shut by active defense where maintenance operates, shut by accumulation where it has lapsed. Theater 0.30: a growing share of defense is rhetorical (efficiency narratives, consumer-welfare framing) layered over quieter contractual work. Accessibility collapse 0.45: alternatives remain visible and partly viable at the periphery, well below the mountain range. Resistance 0.55: entrants litigate, suppliers organize episodically, enforcement waves recur. The measurement series share one grid (t = 0..50, indexing roughly 1890–2015 at ~2.5-year steps): extractiveness and suppression dip at t≈20 as Progressive- and New Deal-era enforcement bit, then re-accumulate after the late-century relaxation of antitrust capacity. The re-accumulation, not the dip, is the trend; the oscillation tracks enforcement politics, and each relaxation lowers the cost of the next round of maintenance.
 *
 * PERSPECTIVAL GAP:
 *   Seats should classify differently. From the incumbent-leadership seat the structure reads as an achievement it administers: coordination it built, margins it earns, defense it calls competitiveness. From the entrant seat the same structure is a wall with a toll booth — the maintained elements are experienced as deliberate exclusion. From the supplier seat it is dependency without negotiation. From the consumer seat it splits: convenience received, surplus paid. From the enforcement seat it appears episodically, case by case, never whole. The engine computes these per-seat classifications from the structural data; divergence between the beneficiary-computed and target-computed types is the finding, not something the authored claim settles.
 *
 * DIRECTIONALITY LOGIC:
 *   The beneficiary/victim declarations drive the derivation. incumbent_market_leaders and incumbent_capital_holders sit near the beneficiary end of d, pushed further down by arbitrage-grade exit — capital that can leave a decaying moat is subsidized by the moats that persist. would_be_entrants carry high d (targets, constrained exit); dependent_small_suppliers carry the highest d (trapped — their specialization is unrecoverable outside the relationship). captive_consumers sit near symmetric: the dual payer/beneficiary declaration encodes receiving real coordination value while financing the margin. competition_authorities are analytical and feed no extraction arithmetic. Suppression stays unscaled — it is the raw structural fact that alternatives are closed by contract and lobbying rather than by nature; only extractiveness is scaled by directionality and scope in the engine's computation.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading is built to block both mandatrophy errors. Read as pure extraction, the arrangement invites dissolution remedies that destroy the surviving coordination function — interoperable standards and scale economies customers would repurchase voluntarily. Read as pure lapsed inheritance, the active maintenance disappears from view exactly where it is growing: theater ratio and suppression requirement both rise over the interval, and the post-relaxation re-accumulation of extractiveness is the signature of rent defense, not inertia. Hence founding_problem_status is contested rather than dead: in mature sectors the founding problem (fragmentation, price wars, incompatible standards) is solved and the arrangement persists past its mandate; in emerging sectors the problem recurs and the arrangement still performs. No mandatrophy resolution is declared because the mandate's death is sector-specific, not global.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    maintenance_quantifier_disaggregation,
    'Does the hybrid partition (some dominance elements lapsed, others actively maintained) survive sector-level disaggregation, or does one sibling reading dominate once domains are separated?',
    'Sector-by-sector audit correlating observable maintenance activity (lobbying expenditure, exclusive contracting, acquisitions of nascent rivals, standards-body control) with observed moat persistence after enforcement removal.',
    'If most sectors show pure lapse, this reading collapses toward the lapsed-alternative sibling and effective extraction falls; if most show active defense, it collapses toward the beneficiary-maintained sibling and suppression estimates rise.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(maintenance_quantifier_disaggregation, conceptual, 'Whether the hybrid partition of the maintenance quantifier is the correct frame or a transitional average of two purer sector regimes.').

omega_variable(
    lapse_vs_routinized_maintenance,
    'For elements classified as lapsed, is maintenance genuinely absent, or has it been routinized into invisible defaults (habitual procurement, pre-installed configurations, contractual auto-renewal) that no longer register as defense?',
    'Counterfactual removal tests: track decay rates of specific moat elements when their maintaining routine is disrupted (ownership change, regulatory interruption) versus matched elements with no maintaining routine.',
    'Genuine lapse supports reading those elements as inertial residue; routinized maintenance keeps them inside the actively enforced structure and raises effective suppression above the authored scalar.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(lapse_vs_routinized_maintenance, empirical, 'Whether apparent lapse is true absence of upkeep or maintenance made invisible by routinization.').

omega_variable(
    natural_equilibrium_vs_policy_artifact,
    'Is observed concentration the natural equilibrium of increasing returns and network effects, or an artifact of policy choices (intellectual property scope, incorporation and merger law, non-compete enforcement) that identifiable actors shaped?',
    'Cross-jurisdiction comparison of concentration under different policy regimes holding technology constant; natural experiments where specific legal supports were withdrawn.',
    'If artifact, beneficiaries exist by construction and effective extraction rises with the constructed share; if natural equilibrium, part of measured extraction is the irreducible price of the returns-to-scale coordination itself.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(natural_equilibrium_vs_policy_artifact, conceptual, 'The kernel''s underlying naturalization contest: how much of dominance is made versus grown.').

omega_variable(
    diffuse_victim_coalition_capacity,
    'Can the diffuse victim seats (consumers, small suppliers) convert latent numbers into coalition power sufficient to alter the maintenance economics?',
    'Historical incidence analysis of consumer cooperatives, supplier associations, and buyer cartels in concentrated sectors; organizational-cost modeling of coalition formation against incumbent counter-mobilization.',
    'Viable coalitions raise resistance and lower sustainable extraction; persistent failure of coalition formation indicates the authored suppression estimate understates the arrangement''s hold on its targets.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(diffuse_victim_coalition_capacity, empirical, 'Whether the powerless victim seats hold unrealized coalition potential.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(market_naturalization__hybrid_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mkt_nat_hybrid_tr_t0, market_naturalization__hybrid_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement_basis(mkt_nat_hybrid_tr_t0, observed).
narrative_ontology:measurement(mkt_nat_hybrid_tr_t10, market_naturalization__hybrid_reading, theater_ratio, 10, 0.17).
narrative_ontology:measurement_basis(mkt_nat_hybrid_tr_t10, observed).
narrative_ontology:measurement(mkt_nat_hybrid_tr_t20, market_naturalization__hybrid_reading, theater_ratio, 20, 0.21).
narrative_ontology:measurement_basis(mkt_nat_hybrid_tr_t20, observed).
narrative_ontology:measurement(mkt_nat_hybrid_tr_t30, market_naturalization__hybrid_reading, theater_ratio, 30, 0.24).
narrative_ontology:measurement_basis(mkt_nat_hybrid_tr_t30, observed).
narrative_ontology:measurement(mkt_nat_hybrid_tr_t40, market_naturalization__hybrid_reading, theater_ratio, 40, 0.27).
narrative_ontology:measurement_basis(mkt_nat_hybrid_tr_t40, observed).
narrative_ontology:measurement(mkt_nat_hybrid_tr_t50, market_naturalization__hybrid_reading, theater_ratio, 50, 0.3).
narrative_ontology:measurement_basis(mkt_nat_hybrid_tr_t50, observed).

% Extraction over time
narrative_ontology:measurement(mkt_nat_hybrid_be_t0, market_naturalization__hybrid_reading, base_extractiveness, 0, 0.34).
narrative_ontology:measurement_basis(mkt_nat_hybrid_be_t0, observed).
narrative_ontology:measurement(mkt_nat_hybrid_be_t10, market_naturalization__hybrid_reading, base_extractiveness, 10, 0.41).
narrative_ontology:measurement_basis(mkt_nat_hybrid_be_t10, observed).
narrative_ontology:measurement(mkt_nat_hybrid_be_t20, market_naturalization__hybrid_reading, base_extractiveness, 20, 0.37).
narrative_ontology:measurement_basis(mkt_nat_hybrid_be_t20, observed).
narrative_ontology:measurement(mkt_nat_hybrid_be_t30, market_naturalization__hybrid_reading, base_extractiveness, 30, 0.45).
narrative_ontology:measurement_basis(mkt_nat_hybrid_be_t30, observed).
narrative_ontology:measurement(mkt_nat_hybrid_be_t40, market_naturalization__hybrid_reading, base_extractiveness, 40, 0.53).
narrative_ontology:measurement_basis(mkt_nat_hybrid_be_t40, observed).
narrative_ontology:measurement(mkt_nat_hybrid_be_t50, market_naturalization__hybrid_reading, base_extractiveness, 50, 0.58).
narrative_ontology:measurement_basis(mkt_nat_hybrid_be_t50, observed).

% Suppression requirement over time
narrative_ontology:measurement(mkt_nat_hybrid_su_t0, market_naturalization__hybrid_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement_basis(mkt_nat_hybrid_su_t0, observed).
narrative_ontology:measurement(mkt_nat_hybrid_su_t10, market_naturalization__hybrid_reading, suppression_requirement, 10, 0.39).
narrative_ontology:measurement_basis(mkt_nat_hybrid_su_t10, observed).
narrative_ontology:measurement(mkt_nat_hybrid_su_t20, market_naturalization__hybrid_reading, suppression_requirement, 20, 0.29).
narrative_ontology:measurement_basis(mkt_nat_hybrid_su_t20, observed).
narrative_ontology:measurement(mkt_nat_hybrid_su_t30, market_naturalization__hybrid_reading, suppression_requirement, 30, 0.43).
narrative_ontology:measurement_basis(mkt_nat_hybrid_su_t30, observed).
narrative_ontology:measurement(mkt_nat_hybrid_su_t40, market_naturalization__hybrid_reading, suppression_requirement, 40, 0.49).
narrative_ontology:measurement_basis(mkt_nat_hybrid_su_t40, observed).
narrative_ontology:measurement(mkt_nat_hybrid_su_t50, market_naturalization__hybrid_reading, suppression_requirement, 50, 0.52).
narrative_ontology:measurement_basis(mkt_nat_hybrid_su_t50, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(market_naturalization__hybrid_reading, resource_allocation).
narrative_ontology:affects_constraint(market_naturalization__hybrid_reading, lapsed_alternative_reading).
narrative_ontology:affects_constraint(market_naturalization__hybrid_reading, beneficiary_maintained_reading).

% DUAL FORMULATION NOTE:
% Constraint family for the market_naturalization kernel. The colloquial label 'market dominance' covers three structurally distinct claims about what holds the position in place: all-lapsed (lapsed_alternative_reading), all-maintained (beneficiary_maintained_reading), and this hybrid mixture. Each is a separate story with its own epsilon assessed over the same standing arrangement — epsilon is reading-indexed, so the shared referent still yields different values per reading. They are linked because the hybrid reading cites lapsed elements as evidence against pure maintenance and maintained elements as evidence against pure lapse; citation runs in both directions across the family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
