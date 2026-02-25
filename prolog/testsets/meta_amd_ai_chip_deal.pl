% ============================================================================
% CONSTRAINT STORY: meta_amd_ai_chip_deal
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-28
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_meta_amd_ai_chip_deal, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: meta_amd_ai_chip_deal
 *   human_readable: Meta-AMD Strategic AI Chip Supply Agreement
 *   domain: technological/economic
 *
 * SUMMARY:
 *   The frontier of artificial intelligence is critically dependent on access
 *   to vast amounts of specialized computing hardware, primarily high-end
 *   GPUs and accelerators. This supply is dominated by an oligopoly of
 *   chipmakers. This constraint story models the structural effects of
 *   large-scale, strategic sourcing deals, such as the one between Meta and
 *   AMD, where a hyperscaler secures a significant portion of future chip
 *   supply. While solving a critical coordination problem for the partners,
 *   these deals invariably concentrate market power and raise the barriers to
 *   entry for smaller companies, startups, and academic researchers, creating
 *   a system of immense extraction.
 *
 * KEY AGENTS:
 *   - Hyperscalers (Meta, Google, etc.): Primary beneficiary (institutional/arbitrage) - Secure compute supply to maintain their competitive moat.
 *   - Dominant Chipmakers (AMD, Nvidia): Primary beneficiary (institutional/arbitrage) - Gain predictable, massive revenue streams and de-risk capital expenditures.
 *   - AI Startups and Researchers: Primary victim (powerless/trapped) - Are priced out of the market for cutting-edge hardware, suppressing innovation.
 *   - Non-Partnered Enterprises: Secondary victim (organized/constrained) - Face higher prices and limited supply for their own AI initiatives.
 *   - Regulators: Institutional actor (institutional/constrained) - Attempt to balance innovation with concerns about market concentration.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(meta_amd_ai_chip_deal, 0.68).
domain_priors:suppression_score(meta_amd_ai_chip_deal, 0.8).
domain_priors:theater_ratio(meta_amd_ai_chip_deal, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(meta_amd_ai_chip_deal, extractiveness, 0.68).
narrative_ontology:constraint_metric(meta_amd_ai_chip_deal, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(meta_amd_ai_chip_deal, theater_ratio, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(meta_amd_ai_chip_deal, tangled_rope).
narrative_ontology:human_readable(meta_amd_ai_chip_deal, "Meta-AMD Strategic AI Chip Supply Agreement").
narrative_ontology:topic_domain(meta_amd_ai_chip_deal, "technological/economic").

domain_priors:requires_active_enforcement(meta_amd_ai_chip_deal).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(meta_amd_ai_chip_deal, hyperscalers).
narrative_ontology:constraint_beneficiary(meta_amd_ai_chip_deal, dominant_chipmakers).
narrative_ontology:constraint_victim(meta_amd_ai_chip_deal, ai_startups_and_researchers).
narrative_ontology:constraint_victim(meta_amd_ai_chip_deal, non_partnered_enterprises).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EXCLUDED INNOVATOR (SNARE) — A small AI startup or academic lab that cannot secure cutting-edge compute. They are priced out of the market, forced to use inferior hardware, or abandon research directions, effectively suppressing their ability to compete. The supply constraint is an insurmountable barrier. d≈0.95, f(d)≈1.42, σ=1.2 → χ≈1.16. This is well into Snare territory.
constraint_indexing:constraint_classification(meta_amd_ai_chip_deal, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: STRATEGIC PARTNER (ROPE) — From the perspective of Meta or AMD, this deal is a pure coordination mechanism to solve an immense supply chain and R&D challenge. It de-risks multi-billion dollar fab investments for AMD and secures a critical resource for Meta's future. The deal creates value for both. d≈0.05, f(d)≈-0.12, σ=1.2 → χ≈-0.10.
constraint_indexing:constraint_classification(meta_amd_ai_chip_deal, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: CONCERNED REGULATOR (TANGLED ROPE) — A national or international regulatory body sees both sides. They recognize the legitimate coordination required for such a complex market, but are also acutely aware of the market concentration and anticompetitive effects on smaller players. Their constrained exit options reflect their limited ability to alter the fundamental capital-intensive nature of the industry. d≈0.50 (derived from being an observer of both beneficiary/victim, but with constrained exit), f(d)≈0.65, σ=1.2 → χ≈0.53.
constraint_indexing:constraint_classification(meta_amd_ai_chip_deal, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 4: ANALYTICAL OBSERVER (TANGLED ROPE) — This view synthesizes the others, acknowledging the genuine coordination function while quantifying the high levels of extraction and suppression imposed on those outside the partnership. The structure simultaneously solves a problem for the powerful and creates a barrier for the powerless, which is the definition of a Tangled Rope.
constraint_indexing:constraint_classification(meta_amd_ai_chip_deal, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(meta_amd_ai_chip_deal_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(meta_amd_ai_chip_deal, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(meta_amd_ai_chip_deal, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(meta_amd_ai_chip_deal, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(meta_amd_ai_chip_deal_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (ε=0.68) is high, reflecting the massive premiums and competitive disadvantage imposed on those who cannot secure these deals. Suppression (0.80) is very high because there are few, if any, viable alternatives to the hardware produced by the chipmaking oligopoly for training state-of-the-art models. The theater ratio (0.30) is moderate; while the deals are functional, their public announcements are highly performative, designed to signal market power and technological leadership to investors, contributing to 'AI bubble' narratives.
 *
 * PERSPECTIVAL GAP:
 *   The gap is stark: Meta and AMD perceive a mutually beneficial coordination mechanism (Rope) that enables innovation. For an AI startup, the same deal functions as a market-closing Snare, locking them out of the key resource they need to survive. A regulator sees the difficult hybrid reality of a system that is both functional for its participants and harmful to the broader ecosystem (Tangled Rope). This gap between a Rope and a Snare is a classic signature of a Tangled Rope structure.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality 'd' is derived from structural position. Beneficiaries like Meta, with institutional power and arbitrage exit options (they could partner with Nvidia, or accelerate in-house designs), have a very low 'd', leading to a Rope classification. Victims like startups, who are trapped with no viable alternatives, have a very high 'd', resulting in a Snare. The system correctly computes these divergent realities from the same base properties by indexing to the agent's position.
 *
 * MANDATROPHY ANALYSIS:
 *   This case prevents the mandatrophy of mislabeling a highly extractive system as pure coordination. An analysis that only considered the partners (Meta and AMD) would incorrectly classify this as a Rope. By requiring the declaration of victims, the framework forces an accounting of the suppressive and extractive externalities of the deal. The final classification of Tangled Rope correctly identifies that a genuine coordination function is being leveraged to create and sustain an extractive power dynamic.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    in_house_viability,
    'Can in-house chip design by other major tech firms or open-source hardware efforts meaningfully break the AMD/Nvidia oligopoly?',
    'Tracking market share of non-oligopoly hardware in large-scale AI training deployments over a 5-year period. Success would be >15% market share captured by alternative designs.',
    'If viable alternatives emerge, suppression would decrease significantly, potentially shifting the constraint from a Tangled Rope towards a Rope as competition increases.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(in_house_viability, empirical, 'Whether in-house or open-source chips can break the supply oligopoly').

omega_variable(
    software_optimization_impact,
    'To what extent can advances in algorithmic efficiency and software optimization reduce the raw demand for cutting-edge hardware?',
    'Comparing performance gains on benchmark tasks from hardware upgrades versus software improvements (e.g., quantization, sparse models, efficient architectures).',
    'If software can provide a >5x efficiency gain, it would lower the barrier to entry, reducing the extractiveness and suppressive power of the hardware suppliers.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(software_optimization_impact, empirical, 'Whether software efficiency can outpace hardware demand').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(meta_amd_ai_chip_deal, 2020, 2030).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(meta_tr_t2020, meta_amd_ai_chip_deal, theater_ratio, 2020, 0.15).
narrative_ontology:measurement(meta_tr_t2025, meta_amd_ai_chip_deal, theater_ratio, 2025, 0.25).
narrative_ontology:measurement(meta_tr_t2030, meta_amd_ai_chip_deal, theater_ratio, 2030, 0.3).

% Extraction over time
narrative_ontology:measurement(meta_be_t2020, meta_amd_ai_chip_deal, base_extractiveness, 2020, 0.5).
narrative_ontology:measurement(meta_be_t2025, meta_amd_ai_chip_deal, base_extractiveness, 2025, 0.62).
narrative_ontology:measurement(meta_be_t2030, meta_amd_ai_chip_deal, base_extractiveness, 2030, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(meta_amd_ai_chip_deal, resource_allocation).
narrative_ontology:affects_constraint(meta_amd_ai_chip_deal, semiconductor_supply_chain).
narrative_ontology:affects_constraint(meta_amd_ai_chip_deal, large_language_model_market).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
