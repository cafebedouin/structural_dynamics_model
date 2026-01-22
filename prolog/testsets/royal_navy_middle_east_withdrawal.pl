% [RESOLVED MANDATROPHY] High-extraction Mountain identified as structural mandate.
% ============================================================================
% CONSTRAINT STORY: royal_navy_middle_east_withdrawal
% ============================================================================
% Generated: 2026-01-20
% Model: Gemini 2.0 Flash
% Source: The Telegraph / Tom Cotterill (Jan 19, 2026)
% ============================================================================

:- module(constraint_rn_gulf_withdrawal, []).

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
 * * constraint_id: royal_navy_middle_east_withdrawal
 * human_readable: End of Permanent Royal Navy Presence in the Gulf
 * domain: political/security
 * temporal_scope: 1980 - 2026 (End of the permanent patrol era)
 * spatial_scope: Middle East / Persian Gulf
 * * SUMMARY:
 * The Royal Navy is withdrawing its last remaining ship (HMS Middleton) from the
 * Middle East, leaving Britain without a permanent fighting presence in the
 * [cite_start]Gulf for the first time since 1980[cite: 3]. This "retreat from the world stage"
 * is driven by a £28bn defense funding gap and a fleet that has shrunk from
 * [cite_start]65 ships in 2014 to 51 today[cite: 3].
 * * KEY AGENTS:
 * - UK Ministry of Defence (Institutional): Negotiating a "stretched" fleet
 * [cite_start]facing Russian activity in the North Atlantic[cite: 3].
 * - Defence Analysts (Analytical): Warning that the "Idea of global Britain is
 * [cite_start]all but over" due to dwindling ship numbers[cite: 3].
 * - Commercial Shipping/Regional Partners (Powerless): Dependent on the
 * "Armilla Patrol" and "Operation Kipion" for maritime security since
 * [cite_start]the 1980s[cite: 3].
 * * NARRATIVE ARC:
 * From a task force of 37 vessels in 2003 to just eight last year, the UK
 * presence has collapsed under the weight of "underinvestment in
 * [cite_start]strategically important areas"[cite: 1, 3]. The withdrawal signals a
 * transition from a "global" force to one that is purely regional and
 * [cite_start]Atlantic-focused[cite: 3].
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (The "Reality" Layer)
   ========================================================================= */

narrative_ontology:interval(rn_withdrawal_cycle, 2025, 2026).
narrative_ontology:constraint_claim(royal_navy_middle_east_withdrawal, noose).

% Base extractiveness score: High (0.80)
% Rationale: The UK "extracts" budget savings but at the cost of its global
% security architecture and influence. The merchant fleet loses protection
% while still being subject to the risks of the region.
domain_priors:base_extractiveness(royal_navy_middle_east_withdrawal, 0.80).

% Suppression score: Moderate (0.50)
% Rationale: Alternatives like drone warships (Atlantic Bastion) are "years
% away from being completed," effectively suppressing current maritime options[cite: 3].
domain_priors:suppression_score(royal_navy_middle_east_withdrawal, 0.50).

% Enforcement: Requires active enforcement
% Rationale: The decommissioning of major vessels (HMS Lancaster) and the
% non-replacement of HMS Middleton are active policy choices by the MoD[cite: 3].
domain_priors:requires_active_enforcement(royal_navy_middle_east_withdrawal).

% Beneficiaries and Victims
constraint_beneficiary(royal_navy_middle_east_withdrawal, [hm_treasury, adversarial_navies]).
constraint_victim(royal_navy_middle_east_withdrawal, [global_shipping, regional_allies_bahrain_uae]).

narrative_ontology:constraint_metric(royal_navy_middle_east_withdrawal, extractiveness, 0.80).
narrative_ontology:constraint_metric(royal_navy_middle_east_withdrawal, suppression_requirement, 0.50).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: REGIONAL MERCHANT CAPTAIN - Noose
   --------------------------------------------------------------------------
   WHO: individual_powerless (A price-taker for security in the Gulf)
   WHEN: immediate (2026 withdrawal window)
   WHERE: trapped (Operating in a region of "constant difficulty and trouble")
   SCOPE: national (Dependent on UK sovereign protection)

   WHY THIS CLASSIFICATION:
   For those relying on the Navy for protection against piracy or state
   aggression, this is a Noose. They are "trapped" by a decision they didn't
   [cite_start]make, losing their security "lifeline" while the risks remain[cite: 3].
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    royal_navy_middle_east_withdrawal,
    noose,
    context(
        agent_power(individual_powerless),
        time_horizon(immediate),
        exit_options(trapped),
        spatial_scope(national)
    )
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: FIRST SEA LORD (GEN SIR GWYN JENKINS) - Rope
   --------------------------------------------------------------------------
   WHO: institutional (In charge of resource allocation)
   WHEN: biographical (Managing a career-long fleet decline)
   WHERE: mobile (Shifting ships to face "Russian ships entering UK waters")
   SCOPE: global (Managing competing global requirements)

   WHY THIS CLASSIFICATION:
   For naval command, this is a Rope—a functional, if painful, coordination
   mechanism. They are "stretched" and must prioritize the North Atlantic
   [cite_start]over the Gulf to maintain basic "home" security[cite: 3].
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    royal_navy_middle_east_withdrawal,
    rope,
    context(
        agent_power(institutional),
        time_horizon(biographical),
        exit_options(mobile),
        spatial_scope(global)
    )
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: DEFENCE ANALYST (FRANCES TUSA) - Mountain
   --------------------------------------------------------------------------
   WHO: analytical (Observer of the long-term trend)
   WHEN: historical (Viewing the 2010-2026 decline)
   WHERE: analytical (Unconstrained by policy needs)
   SCOPE: global (The "Global Britain" framework)

   WHY THIS CLASSIFICATION:
   From an analytical view, this is a Mountain—the inevitable and unchangeable
   result of a decade of shrinking fleet numbers (from 13 frigates to 7).
   [cite_start]"It is simply not doable" to be global with current numbers[cite: 3].
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    royal_navy_middle_east_withdrawal,
    mountain,
    context(
        agent_power(analytical),
        time_horizon(historical),
        exit_options(analytical),
        spatial_scope(global)
    )
) :- !.

/* ==========================================================================
   4. TESTS (What We Learn About Constraints)
   ========================================================================== */

:- begin_tests(rn_withdrawal_tests).

test(multi_perspective_variance) :-
    constraint_indexing:constraint_classification(royal_navy_middle_east_withdrawal, noose, context(individual_powerless, immediate, trapped, _)),
    constraint_indexing:constraint_classification(royal_navy_middle_east_withdrawal, rope, context(institutional, biographical, mobile, _)),
    constraint_indexing:constraint_classification(royal_navy_middle_east_withdrawal, mountain, context(analytical, historical, _, _)).

test(resource_scarcity_lock) :-
    % If the agent is institutional and time is biographical, they see the choice (mobile).
    % If the agent is analytical and time is historical, they see the math (trapped/unchangeable).
    constraint_indexing:constraint_classification(royal_navy_middle_east_withdrawal, mountain, context(analytical, historical, analytical, global)).

:- end_tests(rn_withdrawal_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION (Commentary)
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * * Model: Gemini 2.0 Flash
 * * KEY DECISIONS:
 * * 1. PERSPECTIVAL GAP:
 * There is a sharp gap between the "Institutional" view (Rope), which frames
 * this as a strategic reprioritization for security, and the "Analytical"
 * view (Mountain), which sees it as the final, unavoidable collapse of a
 * [cite_start]"Global Britain" strategy that was never properly funded[cite: 3].
 * * 2. EXTRACTIVENESS SCORE (0.80):
 * The reporting emphasizes that ship numbers have dwindled from 37 in the
 * [cite_start]Gulf to zero[cite: 3]. This extraction of security from the region
 * happens while the UK maintains its "Naval Support Facility" as a hub,
 * [cite_start]essentially keeping the infrastructure but removing the "fighting presence"[cite: 3].
 * * 3. CONNECTION TO TFR83:
 * The "stretched" nature of the fleet mentioned by the First Sea Lord
 * mirrors the TFR83 warning that "economic security risks" and
 * [cite_start]"underinvestment" threaten U.S./Allied leadership in critical sectors[cite: 1, 3].
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    atlantic_bastion_viability,
    'Will \'drone warships\' successfully replace manned frigates in Gulf patrol roles?',
    resolution_mechanism('Monitor the deployment of drone-led Operation Kipion rotations in the late 2020s.'),
    impact('If successful, the Noose (lack of protection) becomes a Rope (coordinated drone security).'),
    confidence_without_resolution(low)
).

omega_variable(
    us_fifth_fleet_interception,
    'Will the U.S. Fifth Fleet reduce the UK\'s \'Deputy Commander\' status as Britain\'s presence reaches zero?',
    resolution_mechanism('Monitor the next deputy commander appointment in Bahrain.'),
    impact('High impact on UK diplomatic sway and \'Global\' status.'),
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES
 * * ALTERNATIVE 1: Maintaining a 'Presence' with Offshore Patrol Vessels
 * Viability: Admiral Lord Alan West suggests that even an Offshore Patrol Vessel
 * [cite_start]would be better than nothing[cite: 3].
 * Suppression: Rejected due to the fleet being "stretched" by the Russian
 * [cite_start]Northern Fleet[cite: 3].
 * * ALTERNATIVE 2: Drone Warships (Atlantic Bastion)
 * [cite_start]Viability: The official plan to replace manned ships with autonomous systems[cite: 3].
 * [cite_start]Suppression: Suppressed by the "years away" development timeline[cite: 3].
 * * CONCLUSION:
 * The existence of Alternative 1 (manned patrol) makes this a Noose for the
 * region; its rejection due to "biographical" resource constraints (Rope)
 * for the MoD highlights the Perspectival Gap.
 */

/* ==========================================================================
   8. INTEGRATION HOOKS
   ========================================================================== */

% ?- constraint_indexing:multi_index_report(royal_navy_middle_east_withdrawal).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
