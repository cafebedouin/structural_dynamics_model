% ============================================================================
% CONSTRAINT STORY: propagation_weight_mechanism
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-01-02
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_propagation_weight_mechanism, []).

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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: propagation_weight_mechanism
 *   human_readable: Propagation Weight Mechanism in Narrative Transmission
 *   domain: epistemology/information_theory/institutional_analysis
 *
 * SUMMARY:
 *   The propagation weight mechanism describes how the same features that
 *   make narratives transmissible — emotional resolution, clear role
 *   assignment, single-position coherence — constitute their explanatory
 *   ceiling by systematically shedding multi-position coherence. A story that
 *   propagates rapidly through a population does so because it is cognitively
 *   lightweight: it resolves emotional tension, assigns clear heroes and
 *   villains, and maintains internal consistency from a single observational
 *   position. But these same features make it structurally incapable of
 *   representing phenomena that require multi-position analysis to
 *   understand. The constraint operates as a filter: narratives that preserve
 *   cross-position coherence face higher cognitive load, slower transmission,
 *   and lower survival rates in competitive information environments. This
 *   creates an asymmetric extraction pattern where carriers of simple
 *   narratives capture influence and reach (beneficiaries), while
 *   truth-seekers in contested domains bear the cost of operating in an
 *   information environment systematically biased against accurate complex
 *   models (victims). The mechanism exhibits genuine coordination function —
 *   shared narratives enable collective action and discourse — but the
 *   coordination comes at the cost of epistemic accuracy in domains where
 *   single-position models are inadequate. The constraint is downstream of
 *   the positional coherence gradient (a mountain constraint establishing
 *   that multi-position coherence is harder to maintain than single-position
 *   coherence) but is itself contingent on current information architectures
 *   rather than being an immutable law.
 *
 * KEY AGENTS:
 *   - Truth Seekers in Contested Domains: Primary victim (powerless/trapped) — operate in information environment where accurate multi-position models are structurally disadvantaged in transmission; cannot exit epistemic commons
 *   - Multi-Position Analysts: Secondary victim (moderate/constrained) — face trade-off between analytical rigor and communicative reach; must simplify to propagate, losing coherence; can partially exit by choosing specialized audiences at career cost
 *   - Carriers of Simple Narratives: Primary beneficiary (institutional/arbitrage) — capture influence and reach through propagation advantage; experience mechanism as pure coordination
 *   - Institutional Gatekeepers: Mixed position (organized/mobile) — editors, platform algorithms, funding bodies that both coordinate information flow and extract rents through simplification pressure; benefit from manageable complexity but constrained by credibility degradation when falsehoods dominate
 *   - Epistemic Infrastructure Builders: Organized agents (organized/mobile) — building alternative transmission pathways (long-form platforms, federated networks, citation graphs) with sunset logic; see mechanism as temporary coordination failure
 *   - Epistemic Commons: Abstract victim (powerless/trapped) — collective good that cannot organize or exit; bears accumulated cost of propagable falsehoods contaminating shared knowledge base
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(propagation_weight_mechanism, 0.48).
domain_priors:suppression_score(propagation_weight_mechanism, 0.52).
domain_priors:theater_ratio(propagation_weight_mechanism, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(propagation_weight_mechanism, extractiveness, 0.48).
narrative_ontology:constraint_metric(propagation_weight_mechanism, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(propagation_weight_mechanism, theater_ratio, 0.38).

% --- Constraint claim ---
narrative_ontology:constraint_claim(propagation_weight_mechanism, tangled_rope).
narrative_ontology:human_readable(propagation_weight_mechanism, "Propagation Weight Mechanism in Narrative Transmission").
narrative_ontology:topic_domain(propagation_weight_mechanism, "epistemology/information_theory/institutional_analysis").

domain_priors:requires_active_enforcement(propagation_weight_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(propagation_weight_mechanism, carriers_of_simple_narratives).
narrative_ontology:constraint_beneficiary(propagation_weight_mechanism, institutional_gatekeepers).
narrative_ontology:constraint_beneficiary(propagation_weight_mechanism, consensus_maintainers).
narrative_ontology:constraint_victim(propagation_weight_mechanism, truth_seekers_in_contested_domains).
narrative_ontology:constraint_victim(propagation_weight_mechanism, multi_position_analysts).
narrative_ontology:constraint_victim(propagation_weight_mechanism, epistemic_commons).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: TRUTH SEEKER (SNARE) — Trapped in an information environment where propagable narratives systematically outcompete accurate ones. Cannot exit the epistemic commons; bears full cost of narrative simplification. Accurate multi-position analysis is structurally disadvantaged in transmission.
constraint_indexing:constraint_classification(propagation_weight_mechanism, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: MULTI-POSITION ANALYST (TANGLED ROPE) — Constrained by the trade-off between analytical rigor and communicative reach. Benefits from the coordination function (shared narrative frameworks enable discourse) but bears extraction cost (must simplify to propagate, losing cross-position coherence). Can partially exit by choosing specialized audiences, but at career cost.
constraint_indexing:constraint_classification(propagation_weight_mechanism, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: CARRIER OF SIMPLE NARRATIVES (ROPE) — Benefits from propagation advantage. Experiences the constraint as pure coordination: emotional resolution and role clarity make stories transmissible, enabling influence and reach. Net beneficiary — the mechanism rewards simplification, and this agent provides simplified narratives.
constraint_indexing:constraint_classification(propagation_weight_mechanism, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: INSTITUTIONAL GATEKEEPER (TANGLED ROPE) — Organized agents (editors, platform algorithms, funding bodies) both coordinate information flow and extract rents through simplification pressure. Benefits from manageable narrative complexity but also constrained by the mechanism's suppression of nuance — institutional credibility degrades when propagable falsehoods dominate.
constraint_indexing:constraint_classification(propagation_weight_mechanism, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: EPISTEMIC INFRASTRUCTURE BUILDER (SCAFFOLD) — Organized agents building alternative transmission pathways (long-form platforms, federated networks, citation graphs, adversarial collaboration protocols) see the propagation weight mechanism as a temporary coordination failure with a sunset. New media forms that reward depth over virality are emerging. Estimated sunset: 15-30 years as information architecture matures.
constraint_indexing:constraint_classification(propagation_weight_mechanism, scaffold,
    context(agent_power(organized),
            time_horizon(civilizational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — From a civilizational perspective, the propagation weight mechanism exhibits both genuine coordination (shared narratives enable collective action) and asymmetric extraction (propagable narratives systematically shed cross-position coherence, concentrating epistemic costs on those who need accurate multi-position models). The mechanism is not a natural law — it is a contingent property of current information architectures.
constraint_indexing:constraint_classification(propagation_weight_mechanism, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(propagation_weight_mechanism_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(propagation_weight_mechanism, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(propagation_weight_mechanism, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(propagation_weight_mechanism, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(propagation_weight_mechanism_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.48): Moderate-high. The propagation advantage of simple narratives creates measurable asymmetry: carriers of simplified stories capture influence, funding, and platform amplification, while those maintaining multi-position coherence face structural disadvantage in transmission. The extraction is not total — specialized audiences and institutional niches exist for complex analysis — but the baseline information environment systematically rewards simplification. The value reflects that much of the 'extraction' is a side effect of genuine coordination benefits (shared narratives enable discourse) rather than pure rent-seeking, but the asymmetry is real and concentrates epistemic costs on truth-seekers. Suppression (0.52): Moderate-high. Significant barriers to transmitting multi-position coherence include cognitive load limits, platform algorithm bias toward engagement over accuracy, funding concentration on communicable findings, and career incentives favoring reach over rigor. But suppression is not total — alternative architectures (long-form platforms, citation networks, adversarial collaboration protocols) are emerging, and specialized communities can sustain complex discourse. Theater ratio (0.38): Moderate. Some performative elements exist (claims of nuance that collapse under scrutiny, virtue signaling about epistemic humility) but the mechanism is not primarily theatrical — the propagation advantage is real and functional, not merely maintained through ritual. The theater has increased over the interval as platform dynamics have intensified simplification pressure.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates how the same structural phenomenon appears differently from different positions. Carriers of simple narratives see pure coordination (Rope) — propagable stories enable influence and collective action. Truth seekers see pure extraction (Snare) — the information environment systematically disadvantages accurate complex models. Multi-position analysts and institutional gatekeepers see mixed coordination and extraction (Tangled Rope) — the mechanism both enables and constrains their work. Epistemic infrastructure builders see a temporary problem with a sunset (Scaffold) — alternative architectures are emerging that reward depth over virality. The analytical observer recognizes the mechanism as contingent on current information architectures rather than immutable, but also sees genuine coordination function alongside asymmetric extraction. The perspectival gap is not a disagreement about facts but a difference in structural position — each perspective is a legitimate reading of the same underlying constraint from a different observational site.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is derived from structural position. Carriers of simple narratives are beneficiaries with arbitrage exit options — they can choose which narratives to amplify and face low cost for switching. Truth seekers in contested domains are victims with trapped exit — they cannot leave the epistemic commons and bear full cost of narrative simplification. Multi-position analysts are victims with constrained exit — they face high cost to maintain rigor (reduced reach, career penalty) but can partially exit by choosing specialized audiences. Institutional gatekeepers are mixed — they benefit from coordination function but also bear cost when propagable falsehoods degrade institutional credibility. The analytical observer sees both coordination and extraction, recognizing the mechanism as contingent rather than natural law.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint resolves mandatrophy by demonstrating that propagation weight is neither pure coordination nor pure extraction — it is a tangled rope from most perspectives. The coordination function is real: shared narratives enable collective action, discourse, and social coordination. But the coordination comes at epistemic cost: the same features that make narratives propagable (emotional resolution, role clarity, single-position coherence) systematically shed multi-position coherence, concentrating epistemic costs on those who need accurate complex models. The mechanism is not a natural law (it is contingent on current information architectures) but also not pure extraction (the propagation advantage serves genuine coordination functions). The tangled rope classification captures this duality: beneficiaries exist (carriers of simple narratives), victims exist (truth seekers in contested domains), coordination function exists (shared narratives enable discourse), and extraction exists (propagable narratives systematically disadvantage accurate complex models). The classification prevents both false naturalization (treating the mechanism as immutable when it is architectural) and false accusation (treating coordination benefits as pure extraction).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    propagation_fidelity_threshold,
    'What level of narrative simplification is inherent to human cognition vs contingent on current media architectures?',
    'Cross-cultural and historical analysis of narrative transmission fidelity; comparison of oral, written, and digital transmission modes; cognitive load studies of multi-position reasoning',
    'If simplification is primarily cognitive: mechanism is closer to mountain (immutable). If primarily architectural: mechanism is contingent and scaffold perspective is validated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(propagation_fidelity_threshold, empirical, 'Cognitive vs architectural sources of simplification pressure').

omega_variable(
    uncomfortable_truth_survival_rate,
    'Do uncomfortable truths propagate at systematically lower rates than comfortable falsehoods, or is the correlation confounded by other factors (complexity, novelty, source credibility)?',
    'Longitudinal tracking of claims with known truth values across propagation networks; control for complexity and source; measure survival differential for emotionally comfortable vs uncomfortable claims',
    'If systematic: extraction mechanism confirmed — propagation weight actively selects against truth in contested domains. If confounded: mechanism is coordination with side effects, not extraction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(uncomfortable_truth_survival_rate, empirical, 'Whether propagation weight systematically selects against uncomfortable truths').

omega_variable(
    alternative_architecture_effectiveness,
    'Do long-form platforms, federated networks, and citation-based architectures actually transmit multi-position coherence at higher fidelity, or do they merely shift the simplification pressure to different dimensions?',
    'Comparative fidelity analysis across platform types; measure cross-position coherence retention in transmitted narratives; identify where alternative architectures fail',
    'If effective: scaffold perspective confirmed — sunset is real. If ineffective: propagation weight is more fundamental than architectural, and scaffold is aspirational.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_architecture_effectiveness, empirical, 'Whether alternative information architectures preserve multi-position coherence').

omega_variable(
    extraction_vs_coordination_balance,
    'Is the propagation advantage of simple narratives primarily a coordination benefit (enabling collective action) or an extraction mechanism (concentrating epistemic costs on truth-seekers)?',
    'Measure correlation between narrative simplicity and collective action success vs correlation between simplicity and epistemic accuracy; identify domains where coordination and accuracy trade off vs domains where they align',
    'If primarily coordination: tangled_rope classification shifts toward rope from more perspectives. If primarily extraction: shifts toward snare from more perspectives.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(extraction_vs_coordination_balance, conceptual, 'Whether propagation weight is primarily coordination or extraction').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(propagation_weight_mechanism, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(propwt_tr_t0, propagation_weight_mechanism, theater_ratio, 0, 0.25).
narrative_ontology:measurement(propwt_tr_t10, propagation_weight_mechanism, theater_ratio, 10, 0.32).
narrative_ontology:measurement(propwt_tr_t20, propagation_weight_mechanism, theater_ratio, 20, 0.38).

% Extraction over time
narrative_ontology:measurement(propwt_be_t0, propagation_weight_mechanism, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(propwt_be_t10, propagation_weight_mechanism, base_extractiveness, 10, 0.42).
narrative_ontology:measurement(propwt_be_t20, propagation_weight_mechanism, base_extractiveness, 20, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(propagation_weight_mechanism, information_standard).

% DUAL FORMULATION NOTE:
% The propagation weight mechanism is downstream of the positional coherence gradient (mountain constraint establishing that multi-position coherence is harder to maintain than single-position coherence). The upstream constraint is a natural law; the propagation weight mechanism is a contingent architectural feature that exploits that natural law. The decomposition separates the immutable cognitive limit (positional coherence gradient) from the contingent transmission bias (propagation weight mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
