% ============================================================================
% CONSTRAINT STORY: bonbon_drop_sticker_craze
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-28
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_bonbon_drop_sticker_craze, []).

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
 *   constraint_id: bonbon_drop_sticker_craze
 *   human_readable: UHA Mikakuto's "Bonbon Drop" Collectible Sticker Promotion
 *   domain: economic/social
 *
 * SUMMARY:
 *   The UHA Mikakuto 'Bonbon Drop' sticker promotion is a classic example of
 *   a gacha (randomized collectible) mechanic applied to a low-cost consumer
 *   good. By bundling collectible, scarce stickers with candy, the system
 *   incentivizes repeat purchases far beyond the consumer's desire for the
 *   base product. This is amplified by social dynamics, including peer
 *   pressure among children and a nostalgia-driven resurgence of 'Heisei
 *   Joji' (90s/00s girl) culture among adults. The resulting manufactured
 *   scarcity creates a frantic consumer culture ('sticker patrols') and a
 *   lucrative secondary market for resellers.
 *
 * KEY AGENTS:
 *   - UHA Mikakuto & Q-LiA: Primary beneficiaries (institutional/arbitrage) - Drive candy sales and brand engagement through manufactured scarcity.
 *   - Collectors (Children & Adults): Primary victims (powerless/trapped to moderate/mobile) - Bear the financial cost of the gacha mechanic.
 *   - Parents of Collectors: Secondary victims (moderate/constrained) - Provide the funding for children's collections under social pressure.
 *   - Resellers: Secondary beneficiaries (organized/mobile) - Exploit the manufactured scarcity for profit on the secondary market.
 *   - Analytical Observer: Sees the full structure of coordination and extraction.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(bonbon_drop_sticker_craze, 0.55).
domain_priors:suppression_score(bonbon_drop_sticker_craze, 0.65).
domain_priors:theater_ratio(bonbon_drop_sticker_craze, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(bonbon_drop_sticker_craze, extractiveness, 0.55).
narrative_ontology:constraint_metric(bonbon_drop_sticker_craze, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(bonbon_drop_sticker_craze, theater_ratio, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(bonbon_drop_sticker_craze, tangled_rope).
narrative_ontology:human_readable(bonbon_drop_sticker_craze, "UHA Mikakuto's \"Bonbon Drop\" Collectible Sticker Promotion").
narrative_ontology:topic_domain(bonbon_drop_sticker_craze, "economic/social").

domain_priors:requires_active_enforcement(bonbon_drop_sticker_craze).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(bonbon_drop_sticker_craze, uha_mikakuto).
narrative_ontology:constraint_beneficiary(bonbon_drop_sticker_craze, sticker_manufacturer_qlia).
narrative_ontology:constraint_beneficiary(bonbon_drop_sticker_craze, resellers).
narrative_ontology:constraint_victim(bonbon_drop_sticker_craze, collectors).
narrative_ontology:constraint_victim(bonbon_drop_sticker_craze, parents_of_collectors).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% From the perspective of a child collector, the system is a snare. Participation is driven by peer dynamics and the desire for specific rare stickers. Exit (quitting) means social exclusion. The 'gacha' mechanic feels coercive and frustrating when desired stickers are not found, extracting allowance money for unwanted candy. d≈0.95, f(d)≈1.42, σ=0.8 → χ≈0.62. This just misses the snare threshold of 0.66, but the lived experience is one of being trapped.
constraint_indexing:constraint_classification(bonbon_drop_sticker_craze, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

% The manufacturer experiences the system as a pure coordination mechanism (Rope). It coordinates consumer demand, drives sales, and builds brand loyalty. The manufactured scarcity is a feature, not a bug, that increases engagement. As the primary beneficiary with full control, they experience no extraction. d≈0.05, f(d)≈-0.12, σ=1.0 → χ≈-0.07 (net subsidy).
constraint_indexing:constraint_classification(bonbon_drop_sticker_craze, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% Adult collectors, often driven by nostalgia, see both sides. They enjoy the community, trading, and the thrill of the hunt (coordination), but are also aware of the financial cost and exploitative gacha mechanic (extraction). They have mobile exit options (they can switch hobbies), making the constraint a Tangled Rope rather than a Snare. d≈0.85, f(d)≈1.15, σ=0.9 → χ≈0.57.
constraint_indexing:constraint_classification(bonbon_drop_sticker_craze, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(regional))).

% The analytical view confirms the Tangled Rope classification. The system possesses a genuine coordination function (creating a collector community) and a clear, asymmetric extraction mechanism (profiting from randomized, scarcity-driven repeat purchases). Both components are essential to its function. d≈0.72, f(d)≈1.15, σ=1.2 → χ≈0.76.
constraint_indexing:constraint_classification(bonbon_drop_sticker_craze, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% Resellers are organized beneficiaries. They see the system as a coordination mechanism for profit, coordinating scarce supply with high demand. They are not the primary institutional beneficiary but exploit the system's properties. Their exit is mobile (they can scalp the next popular item), and for them, the system is a pure Rope for generating income. d≈0.15, f(d)≈-0.01, σ=1.0 → χ≈-0.005.
constraint_indexing:constraint_classification(bonbon_drop_sticker_craze, rope,
    context(agent_power(organized),
            time_horizon(immediate),
            exit_options(mobile),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(bonbon_drop_sticker_craze_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(bonbon_drop_sticker_craze, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(bonbon_drop_sticker_craze, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(bonbon_drop_sticker_craze, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(bonbon_drop_sticker_craze_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (ε=0.55): Moderately high. The core extraction is the value paid for unwanted candy to obtain a chance at a desired sticker. While the candy has some value, the purchasing behavior is driven by the randomized reward, indicating a significant portion of the price is pure extraction. Suppression (0.65): High. There is no alternative way to obtain new stickers from the primary source; one must buy the candy. This forces participation in the gacha system or the inflated secondary market. Theater Ratio (0.30): Moderate. While social performance like trading and 'sticker patrols' is part of the craze, the core economic function of selling candy is direct and non-performative.
 *
 * PERSPECTIVAL GAP:
 *   The gap is stark. The manufacturer (UHA Mikakuto) sees a brilliant marketing campaign (Rope). The child collector, subject to peer pressure and allowance limits, experiences a coercive and frustrating chase (Snare). The adult collector, with more agency and funds, recognizes the exploitative nature but also derives genuine nostalgic joy and community, landing on a mixed classification (Tangled Rope). This highlights how the same system can be perceived as pure coordination, pure extraction, or a hybrid, depending on the agent's power and exit options.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries are the company and resellers, who profit from the system. Their structural position gives them a low directionality (d). Victims are the collectors and their parents, who bear the financial costs and psychological pressure. Their structural position gives them a high directionality (d). The engine's derivation from these declarations correctly models the flow of value, resulting in negative or low effective extraction (χ) for beneficiaries and high χ for victims.
 *
 * MANDATROPHY ANALYSIS:
 *   This case effectively resolves the mandatrophy. To label the craze as merely a 'fun hobby' (Rope) would ignore the structurally coercive and extractive gacha mechanic at its core. Conversely, to label it a pure 'scam' (Snare) would ignore the genuine coordination function it serves in creating a vibrant collector community and tapping into powerful nostalgic sentiment. The analytical classification of Tangled Rope correctly identifies that both functions are present and structurally essential to the phenomenon's success.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    nostalgia_vs_speculation,
    'Is the market primarily driven by genuine collector nostalgia (''Heisei Joji'' culture) or by secondary market speculation and hype?',
    'Analysis of secondary market sales volume and price velocity versus social media sentiment and community forum activity focused on collecting itself.',
    'If nostalgia-driven, the coordination aspect is stronger (Rope/Tangled Rope). If speculation-driven, the extraction is more severe (Tangled Rope/Snare).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(nostalgia_vs_speculation, empirical, 'Distinguishing between nostalgia-driven collecting and market speculation').

omega_variable(
    saturation_point,
    'At what point does collector burnout or market saturation cause the craze to collapse?',
    'Longitudinal tracking of sales data, secondary market prices for common and rare stickers, and social media engagement metrics.',
    'Identifies the natural lifecycle of such manufactured-scarcity systems, determining if it''s a short-term phenomenon (Scaffold-like) or a durable extractive model (Snare-like).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(saturation_point, empirical, 'The market saturation and burnout threshold for the sticker craze').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(bonbon_drop_sticker_craze, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bonb_tr_t0, bonbon_drop_sticker_craze, theater_ratio, 0, 0.15).
narrative_ontology:measurement(bonb_tr_t12, bonbon_drop_sticker_craze, theater_ratio, 12, 0.25).
narrative_ontology:measurement(bonb_tr_t24, bonbon_drop_sticker_craze, theater_ratio, 24, 0.3).

% Extraction over time
narrative_ontology:measurement(bonb_be_t0, bonbon_drop_sticker_craze, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(bonb_be_t12, bonbon_drop_sticker_craze, base_extractiveness, 12, 0.48).
narrative_ontology:measurement(bonb_be_t24, bonbon_drop_sticker_craze, base_extractiveness, 24, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(bonbon_drop_sticker_craze, resource_allocation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
