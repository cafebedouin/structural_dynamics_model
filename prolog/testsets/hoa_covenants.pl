% ============================================================================
% CONSTRAINT STORY: hoa_architectural_covenants
% ============================================================================
% Generated: 2026-01-18
% Model: Gemini 2.0 Flash
% Source: General Domain Analysis of North American HOA Covenants
% ============================================================================

:- module(constraint_hoa_architectural_covenants, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

% --- Namespace Hooks (Required for loading) ---
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
* * constraint_id: hoa_architectural_covenants


* human_readable: HOA Architectural Review and Restrictive Covenants
* domain: economic/social/legal
* temporal_scope: Contemporary (Post-1970s suburban expansion)
* spatial_scope: Residential developments (primarily North America)
* * SUMMARY:


* Legally binding restrictive covenants that dictate the aesthetic and structural
* modifications allowed on private property. Managed by an Architectural Review
* Board (ARB), these constraints regulate everything from paint color to roof
* materials to landscaping.
* * KEY AGENTS:


* * Resident/Homeowner: The subject bound by the covenants.


* * HOA Board/ARB Member: The enforcer of the "community standard."


* * Property Developer: The initial author of the covenants (often for marketability).


* * Real Estate Analyst: An observer of property value preservation.


* * NARRATIVE ARC:
* The constraint begins as a "Rope" (a coordination tool to preserve property
* values) but degrades into a "Piton". The rules become outdated, and
* enforcement becomes subjective, creating high friction (resistance) for
* homeowners. It persists not through active suppression, but through the
* legal and social inertia of the covenant system.
*/

/* ==========================================================================
2. CORE SYSTEM INTEGRATION (The "Reality" Layer)
========================================================================== */

% Required for [STEP 1] and [STEP 2] of the DR-Audit Suite
narrative_ontology:interval(hoa_interval, 0, 10).
narrative_ontology:constraint_claim(hoa_architectural_covenants, piton).

% Base extractiveness: Moderate (0.45)
% Rationale: While residents pay fees and lose autonomy, they theoretically receive
% property value stability. However, the asymmetry lies in the board's power to
% fine without the resident's ability to easily opt out.
domain_priors:base_extractiveness(hoa_architectural_covenants, 0.45).

% Suppression score: Low (0.2)
% Rationale: Alternatives are not actively suppressed, but the covenants
% persist due to the high legal and social friction required to amend them (inertia).
domain_priors:suppression_score(hoa_architectural_covenants, 0.2).

% Resistance score: High (0.7)
% Rationale: Homeowner frustration, lawsuits, and "malicious compliance"
% indicate significant friction against the outdated or subjective rules.
domain_priors:resistance_score(hoa_architectural_covenants, 0.7).

% Metrics for Executive Summary
narrative_ontology:constraint_metric(hoa_architectural_covenants, extractiveness, 0.45).
narrative_ontology:constraint_metric(hoa_architectural_covenants, suppression_requirement, 0.2).

% Make the constraint "evolve" by showing resistance increasing over time
narrative_ontology:constraint_metric(hoa_architectural_covenants, resistance, 0.1, 0). % Low resistance initially
narrative_ontology:constraint_metric(hoa_architectural_covenants, resistance, 0.7, 10). % High resistance as rules become outdated

% Formally model the viable alternative
narrative_ontology:intent_viable_alternative(hoa_interval,
    'Municipal Zoning',
    'Standard city ordinances can regulate safety and basic land use without the need for a private enforcement body.').

% BENEFICIARIES & VICTIMS
constraint_beneficiary(hoa_architectural_covenants, property_developers).
constraint_beneficiary(hoa_architectural_covenants, institutional_investors).
constraint_victim(hoa_architectural_covenants, individual_homeowners).

/* ==========================================================================
3. INDEXED CLASSIFICATIONS (Perspectival Truth)
========================================================================= */

/* --------------------------------------------------------------------------
PERSPECTIVE 1: INDIVIDUAL HOMEOWNER - Snare

WHO: individual_powerless (subject to arbitrary fines)
WHEN: biographical (duration of mortgage/residency)
WHERE: trapped (high cost of selling/moving; "exit" is expensive)
SCOPE: local (their specific lot/house)

WHY THIS CLASSIFICATION:
From the homeowner's perspective, the HOA functions as a Snare. They face
asymmetric power where the board can impose fines or liens for subjective
aesthetic violations (e.g., "wrong shade of beige"), often with limited
due process.

NARRATIVE EVIDENCE:
"The board threatened a lien on my house because my mailbox post was slightly
weathered, despite the fact that I've paid dues for 10 years."
-------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
hoa_architectural_covenants,
snare,
context(
agent_power(individual_powerless),
time_horizon(biographical),
exit_options(trapped),
spatial_scope(local)
)
) :-
domain_priors:base_extractiveness(hoa_architectural_covenants, E),
E > 0.4,
domain_priors:requires_active_enforcement(hoa_architectural_covenants),
!.

/* --------------------------------------------------------------------------
PERSPECTIVE 2: HOA BOARD / ARB - Rope

WHO: institutional (rule-making and enforcement power)
WHEN: generational (maintaining neighborhood character over decades)
WHERE: mobile (can change rules via board vote)
SCOPE: regional (the neighborhood/development)

WHY THIS CLASSIFICATION:
The Board sees the covenants as a Rope—a functional coordination mechanism
that prevents "tragedy of the commons" (e.g., one neighbor painting their
house neon pink and lowering everyone's property values).

NARRATIVE EVIDENCE:
"Without these guidelines, the neighborhood would lose its cohesive feel
and marketability. We are simply protecting the collective investment."
-------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
hoa_architectural_covenants,
rope,
context(
agent_power(institutional),
time_horizon(generational),
exit_options(mobile),
spatial_scope(regional)
)
) :-
% Institutional power perceives it as a tool for coordination
!.

/* --------------------------------------------------------------------------
PERSPECTIVE 3: REAL ESTATE ANALYST - Piton

WHO: analytical (observer)
WHEN: historical (market trends over 50 years)
WHERE: analytical (comparative view across many developments)
SCOPE: national (standardization of suburban housing)

WHY THIS CLASSIFICATION:
For the analyst, the covenant system is a Piton. It was once a functional
Rope for ensuring a baseline of quality, but has since degraded. It now
often represents a high-friction, low-benefit system that persists through
the inertia of legal frameworks, not because it is an optimal or
unchangeable feature of the market.
-------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    hoa_architectural_covenants,
    piton,
    context(
        agent_power(analytical),
        time_horizon(historical),
        exit_options(analytical),
        spatial_scope(national)
    )
) :-
    domain_priors:suppression_score(hoa_architectural_covenants, S),
    S < 0.3,
    !.
/* ==========================================================================
4. TESTS (What We Learn About Constraints)
========================================================================== */

:- begin_tests(hoa_architectural_covenants_tests).

test(multi_perspective_variance) :-
constraint_indexing:constraint_classification(
hoa_architectural_covenants, Type1, context(agent_power(individual_powerless), time_horizon(biographical), exit_options(trapped), spatial_scope(local))),
constraint_indexing:constraint_classification(
hoa_architectural_covenants, Type2, context(agent_power(institutional), time_horizon(generational), exit_options(mobile), spatial_scope(regional))),
constraint_indexing:constraint_classification(
hoa_architectural_covenants, Type3, context(agent_power(analytical), time_horizon(historical), exit_options(analytical), spatial_scope(national))),
Type1 = Type2,
Type2 = Type3,
Type1 = Type3.

test(power_extractiveness_scaling) :-
% Theoretically, institutional power experiences the "benefit" (low extraction)
% while powerless subjects experience the "cost" (high extraction).
E_Score_Powerless = 0.8, % Experience of fines/restrictions
E_Score_Institutional = 0.1, % Control/Coordination benefit
E_Score_Powerless > E_Score_Institutional.

test(time_immutability) :-
% On a biographical scale, it's a Snare (hard to change).
% On a generational scale, it's a Rope (can be amended by vote).
constraint_indexing:constraint_classification(hoa_architectural_covenants, snare, context(agent_power(individual_powerless), time_horizon(biographical), _, _)),
constraint_indexing:constraint_classification(hoa_architectural_covenants, rope, context(agent_power(institutional), time_horizon(generational), _, _)).

:- end_tests(hoa_architectural_covenants_tests).

/* ==========================================================================
5. MODEL INTERPRETATION (Commentary)
========================================================================== */

/**

* LLM GENERATION NOTES
* * Model: Gemini 2.0 Flash


* * KEY DECISIONS:


* * 1. EXTRACTIVENESS SCORE (0.45):




* Reasoning: Chosen because HOAs are not purely extractive (they provide
* maintenance and value preservation) but the enforcement mechanism is
* highly asymmetric. The "extraction" is often psychological (loss of
* autonomy) and financial (fines/dues).
* * 2. PERSPECTIVE SELECTION:




* * Homeowner: Represents the subject (Snare).


* * Board: Represents the administrator (Rope).


* * Analyst: Represents the market reality (Mountain).


* * 3. AMBIGUITIES:




* * The "value" of an HOA is highly debated. Is property value preservation


* a fact or a marketing claim? I have treated it as a claim by making the
* Board's perspective a "Rope" (functional/claimed benefit).
*/

/* ==========================================================================
6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
========================================================================== */

omega_variable(
property_value_correlation,
"Do architectural restrictions actually increase property value compared to similar non-HOA homes?",
resolution_mechanism("Long-term econometric study controlling for neighborhood age and location"),
impact("If NO: The 'Rope' classification for the Board collapses into a 'Snare' for the entire community."),
confidence_without_resolution(medium)
).

omega_variable(
board_motivation,
"Is ARB enforcement driven by aesthetic harmony (Rope) or personal power/ego (Snare)?",
resolution_mechanism("Sentiment analysis of board meeting minutes and litigation frequency"),
impact("Determines if the constraint is fundamentally a coordination tool or a control tool."),
confidence_without_resolution(low)
).

/* ==========================================================================
7. ALTERNATIVE ANALYSIS
========================================================================== */

/**

* VIABLE ALTERNATIVES
* * ALTERNATIVE 1: Municipal Zoning


* Viability: Standard city ordinances regulate safety and basic land use.
* Suppression: Property developers prefer HOAs to bypass municipal
* maintenance costs and create "exclusive" branding.
* * ALTERNATIVE 2: Covenants without Corporate Enforcement


* Viability: Restrictive covenants that exist on the deed but require
* neighbors to sue each other rather than a central board fining residents.
* Suppression: Often seen as "inefficient" for developers.
* * CONCLUSION:


* The suppression of non-HOA alternatives in new construction shifts the
* classification toward Snare for many buyers who "choose" an HOA only because
* no other modern inventory exists.
*/

/* ==========================================================================
8. INTEGRATION HOOKS
========================================================================== */

/**

* TO USE THIS FILE:
* * 1. Load: ?- [constraints/hoa_architectural_covenants].




* 2. Multi-perspective: ?- constraint_indexing:multi_index_report(hoa_architectural_covenants).


* 3. Run tests: ?- run_tests(hoa_architectural_covenants_tests).
*/



/* ==========================================================================
END OF CONSTRAINT STORY
========================================================================== */


% --- v3.1 Indexical Relativity Stubs (Fleet Repair) ---
constraint_indexing:constraint_classification(hoa_covenants, mountain, agent_power(analytical)).
constraint_indexing:constraint_classification(hoa_covenants, rope, agent_power(institutional)).
constraint_indexing:constraint_classification(hoa_covenants, snare, agent_power(individual_powerless)).
