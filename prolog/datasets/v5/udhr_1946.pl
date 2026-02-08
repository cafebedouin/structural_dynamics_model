% ============================================================================
% CONSTRAINT STORY: udhr_1948
% ============================================================================
% Generated: 2026-01-19
% Model: Gemini 2.0 Flash
% Source: Universal Declaration of Human Rights (1948)
% ============================================================================

:- module(constraint_udhr_1948, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

% --- Namespace Hooks ---
:- multifile 
    domain_priors:base_extractiveness/2,
    domain_priors:suppression_score/2,
    domain_priors:requires_active_enforcement/1,
    narrative_ontology:constraint_metric/3,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:extractiveness_for_agent/3.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: udhr_1948
 * human_readable: Universal Declaration of Human Rights
 * domain: political/legal/social
 * temporal_scope: 1948 - Present (Civilizational aspiration)
 * spatial_scope: Global
 * * SUMMARY:
 * The UDHR is a foundational international document establishing "inalienable" 
 * rights for all humans. It serves as a "common standard of achievement" to 
 * prevent "barbarous acts" and provide a framework for freedom, justice, and peace.
 * * KEY AGENTS:
 * - The UN General Assembly: Institutional architect and rule-maker.
 * - The Individual Citizen: The intended beneficiary, often powerless relative to states.
 * - The Sovereign State: The primary enforcer and potential violator of the constraint.
 * * NARRATIVE ARC:
 * Born from the horrors of WWII, the UDHR moves from a proclamation of "natural law"
 * (Mountain) to a functional treaty framework (Rope), though it is often experienced
 * as a coercive limit (Snare) by states or a distant, unreachble ideal (Mountain)
 * by the oppressed.
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION
   ========================================================================== */

% Required for DR-Audit Suite
narrative_ontology:interval(un_era, 1948, 2026).
narrative_ontology:constraint_claim(udhr_1948, rope).

% Base extractiveness: 0.1
% Rationale: The UDHR is designed to be anti-extractive, returning agency to 
% individuals. However, minimal "extraction" exists in the form of states 
% yielding absolute sovereignty to the international order.
domain_priors:base_extractiveness(udhr_1948, 0.1).

% Suppression score: 0.2
% Rationale: It suppresses "tyranny and oppression" but encourages the visibility 
% of alternatives to authoritarianism. It presents itself as "natural."
domain_priors:suppression_score(udhr_1948, 0.2).

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(udhr_1948, extractiveness, 0.1).
narrative_ontology:constraint_metric(udhr_1948, suppression_requirement, 0.2).

% Enforcement: Requires active maintenance through treaties and international pressure.
domain_priors:requires_active_enforcement(udhr_1948).

% Beneficiaries & Victims
constraint_beneficiary(udhr_1948, human_family).
constraint_beneficiary(udhr_1948, individual_citizens).
constraint_victim(udhr_1948, totalitarians).
constraint_victim(udhr_1948, absolute_monarchs).

% Metrics
/* ==========================================================================
   3. INDEXED CLASSIFICATIONS
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: THE DISPOSSESSED REFUGEE - Mountain
   --------------------------------------------------------------------------
   WHO: powerless - No state protection, relying on "inherent" rights.
   WHEN: biographical - Their immediate survival depends on these "eternal" truths.
   WHERE: trapped - Often physically or legally unable to exit their situation.
   SCOPE: local/global - Focused on immediate safety via global principles.
   
   WHY THIS CLASSIFICATION:
   For the powerless, the UDHR is framed as "natural law" (born free and equal). 
   Because they cannot change the declaration and rely on its "inalienable" 
   status to survive, it functions as an unchangeable Mountain of reality.
   
   NARRATIVE EVIDENCE:
   "All human beings are born free and equal... endowed with reason and conscience." (Article 1)
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    udhr_1948,
    mountain,
    context(
        agent_power(powerless),
        time_horizon(biographical),
        exit_options(trapped),
        spatial_scope(global)
    )
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: THE UN GENERAL ASSEMBLY - Rope
   --------------------------------------------------------------------------
   WHO: institutional - The collective power of member states.
   WHEN: civilizational - Looking at the long-term progress of humanity.
   WHERE: mobile - States can negotiate, draft, and amend treaties.
   SCOPE: global - Worldwide jurisdiction of the declaration.
   
   WHY THIS CLASSIFICATION:
   To the UN, the UDHR is a "common standard of achievement" (Article 1). It is 
   a coordination mechanism (Rope) designed to promote "social progress" and 
   "friendly relations between nations." It is a tool for functional harmony.
   
   NARRATIVE EVIDENCE:
   "A common understanding... is of the greatest importance for the full realization of this pledge." (Preamble)
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    udhr_1948,
    rope,
    context(
        agent_power(institutional),
        time_horizon(civilizational),
        exit_options(mobile),
        spatial_scope(global)
    )
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: THE AUTHORITARIAN REGIME - Snare
   --------------------------------------------------------------------------
   WHO: institutional (local) - Power over a specific territory.
   WHEN: immediate - Focused on maintaining current control.
   WHERE: constrained - Subject to international sanctions/pressure if they fail.
   SCOPE: national - Their power is limited by these international standards.
   
   WHY THIS CLASSIFICATION:
   For a regime seeking absolute control, the UDHR is an asymmetric constraint 
   imposed by an external collective (UN). It restricts their "sovereign right" 
   to suppress dissent, acting as a Snare that tightens via international law.
   
   NARRATIVE EVIDENCE:
   "No one shall be subjected to arbitrary arrest... Everyone has the right to leave any country." (Articles 9, 13)
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    udhr_1948,
    snare,
    context(
        agent_power(institutional),
        time_horizon(immediate),
        exit_options(constrained),
        spatial_scope(national)
    )
) :-
    domain_priors:base_extractiveness(udhr_1948, E),
    E < 0.2. % Even low extraction is seen as coercive by those losing power.

/* ==========================================================================
   4. TESTS
   ========================================================================== */

:- begin_tests(udhr_1948_tests).

test(multi_perspective_variance) :-
    constraint_indexing:constraint_classification(udhr_1948, Type1, context(agent_power(powerless), time_horizon(biographical), exit_options(trapped), spatial_scope(global))),
    constraint_indexing:constraint_classification(udhr_1948, Type2, context(agent_power(institutional), time_horizon(civil_horizon), exit_options(mobile), spatial_scope(global))),
    Type1 = mountain,
    Type2 = rope.

test(regime_snare_perception) :-
    constraint_indexing:constraint_classification(udhr_1948, snare, context(agent_power(institutional), time_horizon(immediate), exit_options(constrained), spatial_scope(national))).

test(low_extractiveness_baseline) :-
    domain_priors:base_extractiveness(udhr_1948, E),
    E < 0.3.

:- end_tests(udhr_1948_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * * Model: Gemini 2.0 Flash
 * * KEY DECISIONS:
 * 1. BASE EXTRACTIVENESS (0.1): The UDHR is a "gift" of rights, but it extracts 
 * sovereignty from states.
 * 2. PERSPECTIVE SELECTION: Included the 'Dispossessed Refugee' (Powerless) 
 * to highlight the 'Mountain' aspect of natural law vs. the 'UN' (Institutional) 
 * 'Rope' of administrative coordination.
 * 3. CLASSIFICATION RATIONALE:
 * - Refugee -> Mountain: Because rights are declared "inalienable," they are
 * treated as fixed features of the universe.
 * - UN -> Rope: Because the document is a "standard of achievement" (a goal/tool).
 * - Authoritarian -> Snare: Because it limits the exercise of absolute power.
 * * AMBIGUITIES:
 * - The transition from a non-binding "Declaration" to binding "Covenants" (1966)
 * blurs the line between Rope and Snare. I kept the focus on the 1948 Declaration.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω)
   ========================================================================== */

omega_variable(
    enforcement_gap,
    "Is a right a 'Mountain' if there is no mechanism to enforce it?",
    resolution_mechanism("Observe correlation between UDHR citations and actual reduction in state violence."),
    impact("If no correlation: UDHR is a 'Scaffold' (hollow). If high: it is a true 'Mountain'."),
    confidence_without_resolution(medium)
).

omega_variable(
    universal_consensus,
    "Is the UDHR truly universal (Mountain) or a western liberal construct (Rope/Snare)?",
    resolution_mechanism("Cross-cultural longitudinal survey of value alignment across non-Western states."),
    impact("If Western only: Snare for non-Western states. If Universal: Mountain for humanity."),
    confidence_without_resolution(low)
).

/* ==========================================================================
   7. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES
 * * ALTERNATIVE 1: Westphalian Sovereignty (Absolute)
 * Viability: The status quo prior to 1948.
 * Suppression: Explicitly rejected by the UDHR's preamble ("barbarous acts").
 * Evidence: "It is essential... human rights should be protected by the rule of law."
 * * ALTERNATIVE 2: Regional/Religious Rights Codes
 * Viability: Cairo Declaration on Human Rights in Islam, etc.
 * Suppression: UDHR claims "universal" and "without distinction" status.
 * * CONCLUSION:
 * The presence of these alternatives, which the UDHR seeks to supersede or 
 * coordinate, confirms its role as a Rope/Snare depending on power level.
 */

/* ==========================================================================
   8. INTEGRATION HOOKS
   ========================================================================== */

% TO USE:
% ?- [udhr_1948].
% ?- run_tests(udhr_1948_tests).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */


% --- v3.1 Indexical Relativity Stubs (Fleet Repair) ---
constraint_indexing:constraint_classification(udhr_1946, mountain, agent_power(analytical)).
constraint_indexing:constraint_classification(udhr_1946, rope, agent_power(institutional)).
constraint_indexing:constraint_classification(udhr_1946, snare, agent_power(powerless)).
