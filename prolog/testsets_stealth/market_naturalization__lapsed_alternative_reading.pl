% ============================================================================
% CONSTRAINT STORY: market_naturalization__lapsed_alternative_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
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
    domain_priors:emerges_naturally/1,
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
 *   human_readable: Market Dominance as Lapsed Closure (Lapsed-Alternative Reading)
 *   domain: political economy/economic history/institutional analysis
 *
 * SUMMARY:
 *   This story instantiates the lapsed_alternative_reading of the
 *   market_naturalization kernel: the claim that market dominance over
 *   allocation is a closure whose original maintenance has lapsed, so that
 *   the arrangement now persists without any defending agent, with non-market
 *   alternatives atrophied through disuse rather than suppressed, and with
 *   extraction limited to coordination costs. The interval 0-60 glosses
 *   roughly 1965-2025, the period over which the explicit maintenance
 *   apparatus (capital controls, licensing regimes, bloc borders, overt
 *   industrial policy defending market allocation against planned
 *   alternatives) decayed while celebratory rhetoric expanded. Per the
 *   claim/metric independence rule, the claimed_type (mountain) states what
 *   this reading holds to be structurally true - persistence regardless of
 *   defense, no collecting party, no remaining degrees of freedom for
 *   alternatives - while the metrics state what the descriptive record shows;
 *   the engine computes per-seat classifications from the structural data and
 *   any divergence is the measurement the corpus exists to take. Sibling
 *   readings (beneficiary_maintained_reading, hybrid_reading) are separate
 *   constraint files linked through network.affects_constraints; their
 *   structural deltas differ precisely on maintenance-presence and
 *   beneficiary-identifiability, hence on epsilon.
 *
 * KEY AGENTS:
 *   - incumbent_firms: large-scale market participants (powerful/constrained) - bear coordination and competitive-discipline costs, receive coordination infrastructure; under this reading not a rent-collecting beneficiary class
 *   - household_consumers: diffuse participants (powerless/constrained) - pay and receive roughly symmetrically; hold no seat in allocation-governance conversations
 *   - small_enterprises: the disciplined tail (moderate/constrained) - sharpest exposure to competitive discipline, thinnest buffers
 *   - contract_enforcement_courts: residual administrators (institutional/trapped) - routine background enforcement of contracts and titles, not defense of any allocation regime
 *   - alternative_institution_builders: excluded voices (powerless/trapped) - would revive atrophied non-market arrangements; absent from the conversation
 *   - institutional_economic_historians: analytical observer (analytical/analytical) - sees the full lapse trajectory of alternatives and the decay of the maintenance apparatus
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(market_naturalization__lapsed_alternative_reading, 0.18).
domain_priors:suppression_score(market_naturalization__lapsed_alternative_reading, 0.25).
domain_priors:theater_ratio(market_naturalization__lapsed_alternative_reading, 0.32).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(market_naturalization__lapsed_alternative_reading, extractiveness, 0.18).
narrative_ontology:constraint_metric(market_naturalization__lapsed_alternative_reading, suppression_requirement, 0.25).
narrative_ontology:constraint_metric(market_naturalization__lapsed_alternative_reading, theater_ratio, 0.32).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(market_naturalization__lapsed_alternative_reading, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(market_naturalization__lapsed_alternative_reading, resistance, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(market_naturalization__lapsed_alternative_reading, mountain).
narrative_ontology:human_readable(market_naturalization__lapsed_alternative_reading, "Market Dominance as Lapsed Closure (Lapsed-Alternative Reading)").
narrative_ontology:topic_domain(market_naturalization__lapsed_alternative_reading, "political economy/economic history/institutional analysis").

domain_priors:emerges_naturally(market_naturalization__lapsed_alternative_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(market_naturalization__lapsed_alternative_reading, 'bd7728c3-6fb6-42a8-a5e0-0256e19be7a2').
narrative_ontology:cs_kernel_codification('bd7728c3-6fb6-42a8-a5e0-0256e19be7a2', distributed).
narrative_ontology:cs_authority_grounding('bd7728c3-6fb6-42a8-a5e0-0256e19be7a2', practice).
narrative_ontology:cs_interpretation_layer_present('bd7728c3-6fb6-42a8-a5e0-0256e19be7a2').
narrative_ontology:cs_reading_relation('bd7728c3-6fb6-42a8-a5e0-0256e19be7a2', market_naturalization__beneficiary_maintained_reading, influences).
narrative_ontology:cs_reading_relation('bd7728c3-6fb6-42a8-a5e0-0256e19be7a2', market_naturalization__hybrid_reading, coexists_with).
narrative_ontology:cs_axiom('bd7728c3-6fb6-42a8-a5e0-0256e19be7a2', foundational, market_dominance_persists_without_defense).
narrative_ontology:cs_axiom_status(market_dominance_persists_without_defense, holdable).
narrative_ontology:cs_axiom_grounding('bd7728c3-6fb6-42a8-a5e0-0256e19be7a2', market_dominance_persists_without_defense, empirically_contingent).
narrative_ontology:cs_axiom('bd7728c3-6fb6-42a8-a5e0-0256e19be7a2', foundational, alternatives_atrophied_without_suppression).
narrative_ontology:cs_axiom_status(alternatives_atrophied_without_suppression, holdable).
narrative_ontology:cs_axiom_grounding('bd7728c3-6fb6-42a8-a5e0-0256e19be7a2', alternatives_atrophied_without_suppression, empirically_contingent).
narrative_ontology:cs_reference_frame('bd7728c3-6fb6-42a8-a5e0-0256e19be7a2', self_sustaining_market_order).
narrative_ontology:cs_drift_state('bd7728c3-6fb6-42a8-a5e0-0256e19be7a2', post_financial_crisis_bailout_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('bd7728c3-6fb6-42a8-a5e0-0256e19be7a2', '').
narrative_ontology:cs_kernel_id(market_naturalization__lapsed_alternative_reading, market_naturalization).

% --- Structural relationships ---
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(market_naturalization__lapsed_alternative_reading, incumbent_firms).
narrative_ontology:constraint_beneficiary(market_naturalization__lapsed_alternative_reading, household_consumers).
narrative_ontology:constraint_victim(market_naturalization__lapsed_alternative_reading, incumbent_firms).
narrative_ontology:constraint_victim(market_naturalization__lapsed_alternative_reading, household_consumers).
narrative_ontology:constraint_victim(market_naturalization__lapsed_alternative_reading, small_enterprises).
narrative_ontology:constraint_vindicates(market_naturalization__lapsed_alternative_reading, spontaneous_order_doctrine).
narrative_ontology:constraint_vindicates(market_naturalization__lapsed_alternative_reading, price_signal_information_efficiency).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Operate inside price discipline at scale. Pay transaction, compliance, and competitive-discipline costs; receive standardized coordination infrastructure in return - contract enforcement, payment rails, logistics markets. Some lobby for favorable sectoral rules; whether that lobbying aggregates into defense of the overall allocation regime or merely into ordinary positioning within it is an open empirical question this story flags rather than answers. Exit takes the form of internalizing transactions through vertical integration or relocating operations, not of leaving market allocation altogether.
narrative_ontology:constraint_stakeholder(market_naturalization__lapsed_alternative_reading, incumbent_firms, payer,
    powerful, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(market_naturalization__lapsed_alternative_reading, incumbent_firms, beneficiary).

% Sell labor and buy necessities through markets. Bear search, price-comparison, and switching costs; receive allocation convenience and product variety in return. Participation in market exchange is compulsory for essentials, but no actor directs their participation, and they hold no seat in conversations about how allocation is organized.
narrative_ontology:constraint_stakeholder(market_naturalization__lapsed_alternative_reading, household_consumers, payer,
    powerless, immediate, constrained, national).
narrative_ontology:stakeholder_secondary_role(market_naturalization__lapsed_alternative_reading, household_consumers, beneficiary).

% Bear the sharpest edge of competitive discipline with the thinnest buffers. Cannot internalize transactions the way large firms can and cannot influence the price system they depend on. Closing or shifting trades is possible; existing outside market exchange is not.
narrative_ontology:constraint_stakeholder(market_naturalization__lapsed_alternative_reading, small_enterprises, payer,
    moderate, biographical, constrained, regional).

% Enforce contracts and property titles as routine background administration. Their dockets presuppose the commercial framework, but their posture is neutral application rather than campaigning for any particular allocation regime. They are constituted by the framework they administer; they neither champion nor resist it, and under this reading their activity is infrastructure, not defense.
narrative_ontology:constraint_stakeholder(market_naturalization__lapsed_alternative_reading, contract_enforcement_courts, agenda_setter,
    institutional, generational, trapped, national).

% Run cooperatives, commons arrangements, mutual-credit schemes, and time banks at the margins of the economy. The institutional templates they would reuse atrophied generations ago, so every experiment reconstructs know-how from scratch at prohibitive cost. They are absent from allocation-policy conversations, and their preferred arrangements have no standing forum.
narrative_ontology:constraint_stakeholder(market_naturalization__lapsed_alternative_reading, alternative_institution_builders, excluded,
    powerless, generational, trapped, local).

% Trace how non-market allocation institutions declined - which died of disuse, which were legislated or starved out, and what capability loss followed. From this seat the persistence of market allocation is a measurable historical process rather than a lived discipline or a lived restriction.
narrative_ontology:constraint_stakeholder(market_naturalization__lapsed_alternative_reading, institutional_economic_historians, observer,
    analytical, civilizational, analytical, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(market_naturalization__lapsed_alternative_reading, diffuse).
narrative_ontology:fixing_cost_class(market_naturalization__lapsed_alternative_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates decentralized exchange at scale: prices aggregate dispersed information about scarcity and preference, matching supply and demand across millions of parties without any central allocator.
% TRANSFER_FUNCTION: Moves the burden of coordination - search, price discovery, contracting, competitive discipline - diffusely across all participants rather than concentrating it on any class; goods and payment flow bilaterally between exchangers, with no directional transfer from an identifiable paying group to an identifiable receiving group.
% ABSENT_VOICES: Alternative-institution builders - cooperative organizers, commons practitioners, mutual-credit designers, degrowth theorists - would object that the closure foreclosed arrangements they value, and they are absent from allocation-policy tables, working at local margins. Household producers of non-market care labor are similarly voiceless: their contribution is rendered invisible by a framework that prices only marketed exchange.
% DISAPPEARANCE_RATIONALE: If market dominance vanished overnight, allocation would have to be reorganized from scratch - and precisely because the alternatives atrophied, the rearrangement would be slow and costly: planning capacity, commons governance, and reciprocal provision would all need rebuilding before provisioning continued at scale. Every named seat's situation depends on the arrangement's persistence.
% FOUNDING_PROBLEM: Industrial-scale exchange required allocation mechanisms faster and more legible than custom, guild regulation, or manorial provision; the market closure supplied price-coordinated allocation for mass production and distribution.
% FOUNDING_PROBLEM_CORROBORATION: The coordination problem is attested from outside any benefiting party: economic-history literature on the industrialization problem documents it independently of market advocacy, and the socialist planning experience constitutes backhanded corroboration - states that rejected market allocation built planning apparatuses to solve the same allocation-at-scale problem, attesting its reality while contesting the market's answer. Business historians and institutional economists across competing schools concur that the founding problem existed; they dispute only whether the closure was the best or fairest solution.
narrative_ontology:disappearance_verdict(market_naturalization__lapsed_alternative_reading, world_rearranges).
narrative_ontology:founding_problem_status(market_naturalization__lapsed_alternative_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(market_naturalization__lapsed_alternative_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(market_naturalization__lapsed_alternative_reading, 'none', 1).
narrative_ontology:epsilon_provenance(market_naturalization__lapsed_alternative_reading, 0.18, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(market_naturalization__lapsed_alternative_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(market_naturalization__lapsed_alternative_reading, ExtMetricName, E),
    domain_priors:suppression_score(market_naturalization__lapsed_alternative_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(market_naturalization__lapsed_alternative_reading),
    narrative_ontology:constraint_metric(market_naturalization__lapsed_alternative_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(market_naturalization__lapsed_alternative_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(market_naturalization__lapsed_alternative_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.18, just above the resource_allocation Boltzmann floor of 0.15, because this reading holds that the arrangement's costs are coordination costs - search, price discovery, contracting, competitive discipline - with no concentrated rent accruing to any class. Suppression is 0.25: no coercive defense of the arrangement remains, only residual background enforcement plus the passive barrier of atrophied alternatives (revival requires rebuilding capabilities that no longer exist, which is a real barrier but not coercion). Theater_ratio is 0.32 and rising across the interval: as the maintenance apparatus decayed, celebratory rhetoric (inevitability claims, inevitability-adjacent policy discourse) expanded to fill the space - performance substituted for enforcement rather than accompanying extraction. Accessibility_collapse is 0.78: alternatives are genuinely unavailable because the institutional know-how atrophied, though revival remains possible in principle, placing this below the ~0.85+ typical of physical-law mountains and above the partial-collapse typical of snares. Resistance is 0.15: there is no enforcer to resist; niche cooperative and commons movements build beside the arrangement rather than contesting it. The temporal series run on one shared grid (points 0,10,20,30,40,50,60) with all three metrics authored at every point. suppression_requirement is tracked deliberately: this story's subject IS enforcement-capacity change - the decay of the maintenance apparatus from 0.58 to 0.25 is the lapse itself, the falling trajectory the guidance reserves for narratives tracing enforcement decay. base_extractiveness runs flat near the floor (coordination technology improved slightly); theater_ratio rises monotonically as rhetoric replaced defense.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute differently. From the contract_enforcement_courts seat the arrangement is nearly invisible - neutral administration of contracts and titles, no lived discipline at all, the closest a social arrangement comes to mountain transparency. From the small_enterprises seat the same arrangement is a vivid daily discipline with no buffer and no influence over the price system it depends on - a seat from which the structure may compute as an ordinary demanding coordination mechanism or worse. From the alternative_institution_builders seat it is a foreclosed horizon: the arrangements they would build have no working templates left. From the institutional_economic_historians seat it is a measurable historical process. One arrangement, four experiences; the engine derives this divergence from the power and exit atoms plus the authored overrides, not from the claim.
 *
 * DIRECTIONALITY LOGIC:
 *   No beneficiary or victim classes are declared, because denying an identifiable beneficiary class is this reading's core structural commitment - the sibling readings assert such classes; this one denies them. Left to canonical power-atom fallbacks, the derivation would fabricate exactly the asymmetry the reading rejects (defaults tend to read powerful atoms as beneficiaries and powerless atoms as targets). Explicit overrides therefore pin every participating seat near symmetric: incumbent_firms at 0.45 (slightly subsidized by coordination at scale, but not rent-collectors), household_consumers at 0.50 (costs and benefits balance), small_enterprises at 0.55 (discipline bites hardest where buffers are thinnest), contract_enforcement_courts at 0.45 (administration without collection). alternative_institution_builders sit at 0.65, mildly target-side: the atrophied closure costs them their preferred lifeworld without extracting rents from them. Suppression is authored as a raw structural property and is not scaled; only extractiveness is scaled by directionality and scope in the engine's computation.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem - allocating industrial-scale exchange where customary, guild, and manorial mechanisms were too slow and illegible - is still live, and the arrangement still performs it, so there is no mandate outliving its function and no zombie flag expected from the status-by-verdict mismatch (live x world_rearranges). The classification guards against two opposite errors. Against the siblings' risk: reading active defense into every incumbent lobbying record would mislabel a possibly-lapsed coordination order as pure extraction. Against this reading's own risk: low measured extraction must not be read as benignity - the atrophy barrier is a real cost imposed on every would-be builder of alternatives even at the coordination-cost floor, and if the invisible_maintenance omega resolves against the reading, the genealogy flips from lapse to concealment and the classification follows the evidence rather than the claim.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    invisible_maintenance_ambiguity,
    'Is market dominance genuinely unmaintained, or is maintenance present but distributed and miscategorized as ordinary operation (lobbying, intellectual-property enforcement, bailouts, standards-process capture)?',
    'Comprehensive accounting of defensive expenditure: classify lobbying records, enforcement budgets, bailout history, and standards interventions as regime-maintenance versus sectoral rent-seeking, then test whether removing the classified subset would destabilize dominance.',
    'Substantial identified maintenance collapses this reading into the beneficiary-maintained or hybrid siblings (classification shifts toward tangled_rope or snare); confirmed absence lets the mountain claim certify.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(invisible_maintenance_ambiguity, empirical, 'Whether the closure''s maintenance is truly absent or merely invisible.').

omega_variable(
    atrophy_vs_suppression_origin,
    'Did non-market allocation alternatives die of endogenous disuse, or were they suppressed (enclosure statutes, legal disabilities on mutual aid, credit discrimination against cooperatives)?',
    'Historical mortality analysis of alternative institutions: distinguish cases that failed for internal coordination reasons from cases terminated by legal or financial suppression.',
    'Suppression-origin raises effective suppression and extraction and shifts classification toward snare or tangled_rope; atrophy-origin supports this reading''s low-suppression profile.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(atrophy_vs_suppression_origin, empirical, 'Origin of alternative-institution collapse: disuse versus suppression.').

omega_variable(
    constructed_origin_vs_natural_status,
    'Does a constraint that was historically constructed but now requires no maintenance qualify as emerging naturally, or does constructed origin permanently bar mountain certification?',
    'Conceptual settlement within the classification framework of whether self-sustaining-post-construction satisfies emerges_naturally, using gravity as the limiting benchmark case.',
    'If constructed origin bars mountain status, the claim downgrades to rope or piton even under full maintenance-absence; if self-sustainment suffices, the mountain claim stands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(constructed_origin_vs_natural_status, conceptual, 'Whether post-construction self-sustainment counts as naturality.').

omega_variable(
    coordination_cost_floor_measurement,
    'Is measured extraction genuinely at the coordination-cost floor for resource-allocation systems, or is rent concealed inside reported transaction and intermediation costs?',
    'Sector-level transaction-cost accounting compared against margin decomposition and benchmarks for comparable coordination services.',
    'Extraction persistently above the floor indicates concealed rent and pulls classification away from this reading toward the hybrid sibling.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_cost_floor_measurement, empirical, 'Whether low measured extraction reflects a true cost floor or hidden rent.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(market_naturalization__lapsed_alternative_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mark_tr_t0, market_naturalization__lapsed_alternative_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement_basis(mark_tr_t0, observed).
narrative_ontology:measurement(mark_tr_t10, market_naturalization__lapsed_alternative_reading, theater_ratio, 10, 0.15).
narrative_ontology:measurement_basis(mark_tr_t10, observed).
narrative_ontology:measurement(mark_tr_t20, market_naturalization__lapsed_alternative_reading, theater_ratio, 20, 0.19).
narrative_ontology:measurement_basis(mark_tr_t20, observed).
narrative_ontology:measurement(mark_tr_t30, market_naturalization__lapsed_alternative_reading, theater_ratio, 30, 0.24).
narrative_ontology:measurement_basis(mark_tr_t30, observed).
narrative_ontology:measurement(mark_tr_t40, market_naturalization__lapsed_alternative_reading, theater_ratio, 40, 0.27).
narrative_ontology:measurement_basis(mark_tr_t40, observed).
narrative_ontology:measurement(mark_tr_t50, market_naturalization__lapsed_alternative_reading, theater_ratio, 50, 0.3).
narrative_ontology:measurement_basis(mark_tr_t50, observed).
narrative_ontology:measurement(mark_tr_t60, market_naturalization__lapsed_alternative_reading, theater_ratio, 60, 0.32).
narrative_ontology:measurement_basis(mark_tr_t60, observed).

% Extraction over time
narrative_ontology:measurement(mark_be_t0, market_naturalization__lapsed_alternative_reading, base_extractiveness, 0, 0.21).
narrative_ontology:measurement_basis(mark_be_t0, observed).
narrative_ontology:measurement(mark_be_t10, market_naturalization__lapsed_alternative_reading, base_extractiveness, 10, 0.2).
narrative_ontology:measurement_basis(mark_be_t10, observed).
narrative_ontology:measurement(mark_be_t20, market_naturalization__lapsed_alternative_reading, base_extractiveness, 20, 0.19).
narrative_ontology:measurement_basis(mark_be_t20, observed).
narrative_ontology:measurement(mark_be_t30, market_naturalization__lapsed_alternative_reading, base_extractiveness, 30, 0.19).
narrative_ontology:measurement_basis(mark_be_t30, observed).
narrative_ontology:measurement(mark_be_t40, market_naturalization__lapsed_alternative_reading, base_extractiveness, 40, 0.18).
narrative_ontology:measurement_basis(mark_be_t40, observed).
narrative_ontology:measurement(mark_be_t50, market_naturalization__lapsed_alternative_reading, base_extractiveness, 50, 0.18).
narrative_ontology:measurement_basis(mark_be_t50, observed).
narrative_ontology:measurement(mark_be_t60, market_naturalization__lapsed_alternative_reading, base_extractiveness, 60, 0.18).
narrative_ontology:measurement_basis(mark_be_t60, observed).

% Suppression requirement over time
narrative_ontology:measurement(mark_su_t0, market_naturalization__lapsed_alternative_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement_basis(mark_su_t0, observed).
narrative_ontology:measurement(mark_su_t10, market_naturalization__lapsed_alternative_reading, suppression_requirement, 10, 0.52).
narrative_ontology:measurement_basis(mark_su_t10, observed).
narrative_ontology:measurement(mark_su_t20, market_naturalization__lapsed_alternative_reading, suppression_requirement, 20, 0.46).
narrative_ontology:measurement_basis(mark_su_t20, observed).
narrative_ontology:measurement(mark_su_t30, market_naturalization__lapsed_alternative_reading, suppression_requirement, 30, 0.4).
narrative_ontology:measurement_basis(mark_su_t30, observed).
narrative_ontology:measurement(mark_su_t40, market_naturalization__lapsed_alternative_reading, suppression_requirement, 40, 0.34).
narrative_ontology:measurement_basis(mark_su_t40, observed).
narrative_ontology:measurement(mark_su_t50, market_naturalization__lapsed_alternative_reading, suppression_requirement, 50, 0.29).
narrative_ontology:measurement_basis(mark_su_t50, observed).
narrative_ontology:measurement(mark_su_t60, market_naturalization__lapsed_alternative_reading, suppression_requirement, 60, 0.25).
narrative_ontology:measurement_basis(mark_su_t60, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(market_naturalization__lapsed_alternative_reading, resource_allocation).
narrative_ontology:affects_constraint(market_naturalization__lapsed_alternative_reading, market_naturalization__beneficiary_maintained_reading).
narrative_ontology:affects_constraint(market_naturalization__lapsed_alternative_reading, market_naturalization__hybrid_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'market dominance is natural' decomposes into three structurally distinct claims per the epsilon-invariance principle. This file (lapsed_alternative_reading) authors the lowest epsilon of the family: no identifiable beneficiary class, alternatives atrophied through non-use, extraction at the coordination-cost floor. The beneficiary_maintained_reading authors high epsilon with an identifiable incumbent beneficiary class and active enforcement; the hybrid_reading authors intermediate epsilon with mixed maintenance. The upstream/downstream structure runs from this reading outward: its policy operationalization (deregulation-era assumptions of self-sustaining markets) changed the evidentiary and institutional environment in which the sibling readings are argued. Each family member links the others via network.affects_constraints; no member is an orphan.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(market_naturalization__lapsed_alternative_reading, powerless, 0.55).
constraint_indexing:directionality_override(market_naturalization__lapsed_alternative_reading, moderate, 0.55).
constraint_indexing:directionality_override(market_naturalization__lapsed_alternative_reading, powerful, 0.45).
constraint_indexing:directionality_override(market_naturalization__lapsed_alternative_reading, institutional, 0.45).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
