```prolog
% ============================================================================
% CONSTRAINT STORY: dead_sea_effect
% ============================================================================
% Generated: 2026-01-17
% Model: Gemini 2.0 Flash
% Source: Software Engineering Management / Bruce Webster / Organizational Theory
% ============================================================================

:- module(constraint_dead_sea_effect, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

% --- Namespace Hooks ---
:- multifile 
    domain_priors:base_extractiveness/2,
    domain_priors:suppression_score/2,
    domain_priors:requires_active_enforcement/1,
    constraint_indexing:constraint_classification/3.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: dead_sea_effect
 * human_readable: The Dead Sea Effect (Talent Evaporation)
 * domain: social/economic/technological
 * temporal_scope: Biographical to Generational
 * spatial_scope: Regional (Corporate/Institutional)
 * * SUMMARY:
 * The Dead Sea Effect occurs in large organizations when highly talented and 
 * mobile individuals "evaporate" (leave) because they have the best exit 
 * options, while the less talented/less mobile individuals stay behind, 
 * increasing the "salinity" (mediocrity) of the remaining talent pool.
 * * KEY AGENTS:
 * - The High-Performer: The "fresh water" who leaves as soon as the 
 * environment becomes toxic or stagnant.
 * - The Residue: The average-to-low performers who remain because they lack 
 * comparable exit options.
 * - The Management: The institutional force that fails to differentiate 
 * treatment, leading to the evaporation.
 * * NARRATIVE ARC:
 * This is an "Anti-Matching Market." Instead of sorting for quality, the 
 * system's constraints (rigid pay, bad management, bureaucracy) act as a 
 * filter that systematically removes the highest value participants, leaving 
 * a "Mountain" of institutional inertia.
 */

/* ==========================================================================
   2. BASE PROPERTIES (Reality Layer)
   ========================================================================== */

narrative_ontology:interval(dead_sea_effect, 0, 10).
narrative_ontology:constraint_claim(dead_sea_effect, organizational_decay).

% Base extractiveness: Moderate (0.5).
% The organization extracts the presence/stability of the mediocre at the cost 
% of the high-performer's potential, leading to a net loss of system value.
domain_priors:base_extractiveness(dead_sea_effect, 0.5).

% Suppression: Moderate (0.4).
% Talent "evaporation" is often invisible to management (who see low turnover 
% in the residue) until the system fails.
domain_priors:suppression_score(dead_sea_effect, 0.4).

% Enforcement: Emerges naturally from poor institutional design.
domain_priors:emerges_naturally(dead_sea_effect).

% Beneficiaries: Competitor Firms (who hire the "evaporated" talent).
constraint_beneficiary(dead_sea_effect, competitor_organizations).

% Victims: The Sinking Organization (loses innovation capacity).
constraint_victim(dead_sea_effect, the_stagnant_institution).

% Metrics
narrative_ontology:constraint_metric(dead_sea_effect, extractiveness, 0.5).
narrative_ontology:constraint_metric(dead_sea_effect, suppression_requirement, 0.4).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: THE HIGH-PERFORMER - Rope
   --------------------------------------------------------------------------
   WHO: individual_powerful (high human capital)
   WHEN: immediate
   WHERE: mobile (high exit options)
   SCOPE: global
   
   WHY THIS CLASSIFICATION:
   For the high-performer, the organization's decay is a "Rope." Because 
   they are mobile, the declining environment is simply the signal they 
   need to climb out to a better "Match." The constraint doesn't bind 
   them; it motivates their exit.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    dead_sea_effect,
    rope,
    context(
        agent_power(individual_powerful),
        time_horizon(immediate),
        exit_options(mobile),
        spatial_scope(global)
    )
) :-
    true.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: THE MEDIOCRE REMAINER - Noose
   --------------------------------------------------------------------------
   WHO: individual_powerless (low exit options)
   WHEN: biographical
   WHERE: trapped
   SCOPE: local
   
   WHY THIS CLASSIFICATION:
   For those who cannot leave, the increasing "salinity" is a "Noose." 
   As the high-performers leave, the workload and institutional decay 
   accelerate. Those remaining are trapped in a failing system with 
   no path to improvement, slowly suffocated by growing inefficiency.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    dead_sea_effect,
    noose,
    context(
        agent_power(individual_powerless),
        time_horizon(biographical),
        exit_options(trapped),
        spatial_scope(local)
    )
) :-
    domain_priors:base_extractiveness(dead_sea_effect, E),
    E >= 0.5,
    !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: THE OUTSIDE OBSERVER - Mountain
   --------------------------------------------------------------------------
   WHO: analytical
   WHEN: historical
   WHERE: analytical
   SCOPE: national
   
   WHY THIS CLASSIFICATION:
   To the historian or consultant, the Dead Sea Effect is a "Mountain." 
   It is an inevitable byproduct of entropy in large, unmanaged 
   hierarchies. It follows natural laws of incentive and mobility. 
   You cannot "wish" it away; you must re-engineer the geography.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    dead_sea_effect,
    mountain,
    context(
        agent_power(analytical),
        time_horizon(historical),
        exit_options(analytical),
        spatial_scope(national)
    )
) :-
    domain_priors:emerges_naturally(dead_sea_effect),
    !.

/* ==========================================================================
   4. TESTS
   ========================================================================== */

:- begin_tests(dead_sea_tests).

test(mobility_divergence) :-
    % Testing that mobile agents see a Rope (exit) while trapped agents see a Noose.
    constraint_indexing:constraint_classification(dead_sea_effect, rope, context(_, _, mobile, _)),
    constraint_indexing:constraint_classification(dead_sea_effect, noose, context(_, _, trapped, _)).

test(organizational_mountain) :-
    % Testing that the analytical view recognizes it as a structural Mountain.
    constraint_indexing:constraint_classification(dead_sea_effect, mountain, context(analytical, _, _, _)).

:- end_tests(dead_sea_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION (Commentary)
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * * Model: Gemini 2.0 Flash
 * * KEY DECISIONS:
 * The Dead Sea Effect is a "Reverse Matching Market." I classified it 
 * primarily around exit options. If you can leave, the constraint is 
 * helpful (it tells you to leave). If you can't, it's a death trap.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω)
   ========================================================================== */

omega_variable(
    remote_work_impact,
    "Does remote work increase the 'evaporation' rate by lowering exit costs?",
    resolution_mechanism("Comparative analysis of turnover rates in remote vs. in-office stagnant firms"),
    impact("If Yes: The Mountain of organizational decay becomes a 'Flash Flood' (rapid collapse)."),
    confidence_without_resolution(medium)
).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */

```

### The Dynamics of the Dead Sea Effect

The **Dead Sea Effect** is a classic organizational paradox. It describes how an institution can become less competent over time even if its total headcount remains stable.

#### How the "Evaporation" Happens:

1. **High Talent Mobility:** The most talented employees are like "fresh water"—they have the most options in the **Matching Market**. If the corporate environment becomes stagnant, bureaucratic, or toxic, they are the first to get offers elsewhere and leave.
2. **The Residue:** Employees with fewer options or lower performance are like "salt"—they tend to stay put because the "exit cost" for them is higher.
3. **Increasing Salinity:** Over time, the organization's culture is defined by those who stayed. This creates a feedback loop: as the culture becomes more bureaucratic (more "salty"), it drives away the remaining high-performers even faster.

#### The Role of AI-Evaluators and Matching Markets:

The rise of **AI-Evaluators** and **Skills-Based Hiring** significantly accelerates the Dead Sea Effect:

* **The Rope for Talent:** High-performers can now use AI tools to find and secure new matches almost instantly. Their "Exit Options" are at an all-time high.
* **The Noose for Organizations:** Institutions that rely on a "Noose" of institutional loyalty or high switching costs find that their best people are the only ones capable of slipping that noose.

#### Why Managers Miss It:

Management often looks at **Turnover Rates** as a health metric. In a Dead Sea scenario, turnover might actually be *low* because the "residue" is stable. The organization appears healthy on paper, but its "Mountain" of institutional knowledge is actually a hollow shell of mediocrity.

Would you like me to analyze how **"Golden Handcuffs"** (vesting stock options) act as a specific technological constraint to try and prevent this "evaporation"?
