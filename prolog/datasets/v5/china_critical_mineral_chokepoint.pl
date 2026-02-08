% ============================================================================
% CONSTRAINT STORY: china_critical_mineral_chokepoint
% ============================================================================
% Generated: 2026-01-23
% Model: Gemini Pro (Revised)
% Source: Global Supply Chain Analysis / Geopolitical Strategy
% Status: [RESOLVED MANDATROPHY]
% ============================================================================

:- module(constraint_china_critical_mineral_chokepoint, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

% --- Namespace Hooks (Required for loading) ---
:- multifile 
    domain_priors:base_extractiveness/2,
    domain_priors:suppression_score/2,
    domain_priors:requires_active_enforcement/1,
    narrative_ontology:constraint_metric/3,
    constraint_indexing:constraint_classification/3.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * 
 * constraint_id: china_critical_mineral_chokepoint
 * human_readable: China Critical Mineral Chokepoint
 * domain: economic/technological/political
 * temporal_scope: 2010-Present
 * spatial_scope: Global
 * 
 * SUMMARY:
 * China's dominance over the processing and supply of critical minerals creates 
 * a global strategic chokepoint. 
 * While based on geology (Mountain), it functions as an instrument of 
 * diplomatic leverage (Rope/Snare).
 * 
 * KEY AGENTS:
 * - Downstream Manufacturers (Individual Powerless): Margins extracted by quotas.
 * - Chinese State Planners (Institutional): Uses dominance for coordination.
 * - Geological/Infrastructure Analyst (Analytical): Observes the immutable geological facts.
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (The "Reality" Layer)
   ========================================================================== */

narrative_ontology:interval(china_critical_mineral_chokepoint, 0, 10).
narrative_ontology:constraint_claim(china_critical_mineral_chokepoint, tangled_rope).

% Base extractiveness: 0.85. 
% High strategic extraction via value-chain dominance.
domain_priors:base_extractiveness(china_critical_mineral_chokepoint, 0.85).

% Suppression score: 0.70.
% The dominance of one supplier suppresses the viability of alternative sources.
domain_priors:suppression_score(china_critical_mineral_chokepoint, 0.70).

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(china_critical_mineral_chokepoint, extractiveness, 0.85).
narrative_ontology:constraint_metric(china_critical_mineral_chokepoint, suppression_requirement, 0.7).

% Enforcement: Requires active enforcement through export quotas and state-directed investment.
domain_priors:requires_active_enforcement(china_critical_mineral_chokepoint).

% BENEFICIARIES & VICTIMS
constraint_beneficiary(china_critical_mineral_chokepoint, state_industrial_planners).
constraint_victim(china_critical_mineral_chokepoint, downstream_tech_manufacturers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: DOWNSTREAM FIRM - Snare
   --------------------------------------------------------------------------
   WHO: powerless (Margins extracted by quotas and price manipulation)
   WHEN: immediate (Short-term production cycles)
   WHERE: trapped (Dependent on a single-source supply chain)
   
   WHY THIS CLASSIFICATION:
   For downstream firms, the chokepoint is a 'Snare'. Their production and
   profitability are strangled by their dependence on a single supplier who can
   dictate terms, creating extreme vulnerability.
   -------------------------------------------------------------------------- */
constraint_indexing:constraint_classification(
    china_critical_mineral_chokepoint, 
    snare, 
    context(
        agent_power(powerless),
        time_horizon(immediate),
        exit_options(trapped),
        spatial_scope(local)
    )
).

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: CHINESE STATE PLANNERS - Rope
   --------------------------------------------------------------------------
   WHO: institutional (Uses dominance for coordination and leverage)
   WHEN: historical (Long-term strategic industrial policy)
   WHERE: arbitrage (Leveraging a geological advantage into a global strategic tool)
   
   WHY THIS CLASSIFICATION:
   For Chinese state planners, the chokepoint is a 'Rope'. It is a powerful tool
   for economic coordination, geopolitical leverage, and ensuring the success
   of their own domestic industries.
   -------------------------------------------------------------------------- */
constraint_indexing:constraint_classification(
    china_critical_mineral_chokepoint, 
    rope, 
    context(
        agent_power(institutional),
        time_horizon(historical),
        exit_options(arbitrage),
        spatial_scope(national)
    )
).

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: GEOLOGICAL/INFRASTRUCTURE ANALYST - Mountain
   --------------------------------------------------------------------------
   WHO: analytical (Observes the immutable geological facts)
   WHEN: civilizational (Geological timescales)
   WHERE: analytical (Universal laws of resource distribution)
   
   WHY THIS CLASSIFICATION:
   To a geological analyst, the distribution of mineral deposits is a 'Mountain'.
   It is an immutable fact of the Earth's geology that certain regions are
   endowed with these resources, forming a fixed landscape upon which all
   economic and political strategies must be built.
   -------------------------------------------------------------------------- */
constraint_indexing:constraint_classification(
    china_critical_mineral_chokepoint, 
    mountain, 
    context(
        agent_power(analytical),
        time_horizon(civilizational),
        exit_options(analytical),
        spatial_scope(global)
    )
).

/* ==========================================================================
   4. TESTS (What We Learn About Constraints)
   ========================================================================== */

:- begin_tests(china_critical_mineral_chokepoint_tests).

test(multi_perspective_variance) :-
    constraint_indexing:constraint_classification(china_critical_mineral_chokepoint, Type1, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(china_critical_mineral_chokepoint, Type2, context(agent_power(institutional), _, _, _)),
    constraint_indexing:constraint_classification(china_critical_mineral_chokepoint, Type3, context(agent_power(analytical), _, _, _)),
    Type1 \= Type2,
    Type2 \= Type3,
    Type1 \= Type3.

:- end_tests(china_critical_mineral_chokepoint_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION (Commentary)
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * 
 * Model: Gemini Pro (Revised)
 * Date: 2026-01-23
 * 
 * KEY DECISIONS:
 * 
 * 1. MANDATROPHY STATUS: High extractiveness (0.85) and suppression (0.70)
 *    confirm this as a severe Mandatrophic constraint. It is 'RESOLVED' by
 *    recognizing that the geological 'Mountain' is leveraged as a strategic
 *    'Rope' by state actors, which in turn creates a 'Snare' for dependent firms.
 * 
 * 2. CLASSIFICATION RATIONALE:
 *    - Downstream Firm (Snare): Trapped by supply chain dominance.
 *    - Chinese State Planners (Rope): A tool for geopolitical leverage.
 *    - Geological Analyst (Mountain): Immutable fact of resource distribution.
 * 
 * 3. CORE INSIGHT: This constraint demonstrates how a natural 'Mountain'
 *    (geological deposits) can be transformed into a powerful 'Rope' of
 *    geopolitical strategy, which then functions as an economic 'Snare' for
 *    those dependent on the controlled resource.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */
/**
 * OMEGA IDENTIFICATION
 *
 * Mandatory Omega for high-extraction constraints.
 */
omega_variable(
    artificial_scarcity_threshold,
    "Is the chokepoint a result of physical depletion (Mountain) or managed market access (a strategic Rope/Snare)?",
    resolution_mechanism("Audit of export quota adjustments vs. global geological reserve reports and new mining projects development."),
    impact("If Mountain: Requires deep-sea/extra-planetary mining and long-term R&D. If Snare: Requires trade reform and diplomatic solutions."),
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. ALTERNATIVE ANALYSIS
   ========================================================================== */
/**
 * VIABLE ALTERNATIVES
 *
 * ALTERNATIVE 1: Supply Chain Diversification ("Friend-shoring")
 *    Viability: Developing alternative mining, processing, and refining capabilities in politically aligned countries. This is economically and technically viable in the long term.
 *    Suppression: Suppressed by Chinese economies of scale, lower labor costs, fewer environmental regulations, and existing infrastructure, which make it cost-prohibitive for other nations to compete in the short term.
 *
 * CONCLUSION:
 * The active suppression of alternative supply chains, despite their long-term
 * viability, reinforces the "Snare" for dependent nations and firms, highlighting
 * China's effective use of its 'Rope' to maintain the chokepoint.
 */

/* ==========================================================================
   8. INTEGRATION HOOKS
   ========================================================================== */

/**
 * TO USE THIS FILE:
 * 
 * 1. Load: ?- [constraints/china_critical_mineral_chokepoint].
 * 2. Multi-perspective: ?- multi_index_report(china_critical_mineral_chokepoint).
 * 3. Run tests: ?- run_tests(china_critical_mineral_chokepoint_tests).
 */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */