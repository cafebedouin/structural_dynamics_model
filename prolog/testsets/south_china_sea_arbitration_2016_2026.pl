% ============================================================================
% CONSTRAINT STORY: south_china_sea_arbitration_2016_2026
% ============================================================================
% Generated: 2026-01-19
% Model: Gemini 2.0 Flash
% Source: PCA Case No. 2013-19; 2026 Regional Geopolitical Updates (Jan 19, 2026)
% ============================================================================

:- module(scs_arbitration_2026, []).

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
 * * constraint_id: south_china_sea_arbitration_2016_2026
 * human_readable: The 2016 South China Sea Arbitral Award
 * domain: legal/geopolitical/security
 * temporal_scope: 2016 - 2026 (Active contestation)
 * spatial_scope: South China Sea / West Philippine Sea
 * * SUMMARY:
 * On July 12, 2016, an Arbitral Tribunal ruled overwhelmingly in favor of the 
 * Philippines, invalidating China's "nine-dash line" and "historic rights" 
 * as incompatible with UNCLOS. By January 2026, this ruling serves as the 
 * primary legal constraint on maritime claims in the region, though its 
 * enforcement remains a matter of intense international pressure and local 
 * friction.
 * * KEY AGENTS:
 * - The Republic of the Philippines: The primary beneficiary and legal "owner" 
 * of the award.
 * - The People's Republic of China: The primary "subject" of the constraint, 
 * which it rejects as "null and void."
 * - International Community (US/EU/Japan): External enforcers who treat the 
 * ruling as a "binding" legal reality.
 * - Filipino Fisherfolk: Powerless individuals whose safety depends on the 
 * transition of this law from paper to physical reality.
 * * NARRATIVE ARC:
 * The ruling was intended as a "Rope" (a coordination tool for peaceful 
 * resolution). However, for China, it functions as a "Snare" (an asymmetric 
 * limit on perceived sovereign power). For the Philippines, it is becoming a 
 * "Mountain" (a fixed, final legal truth) that they refuse to re-negotiate.
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION
   ========================================================================== */

% Required for DR-Audit Suite
narrative_ontology:interval(scs_legal_era, 2016, 2026).
narrative_ontology:constraint_claim(south_china_sea_arbitration_2016_2026, mountain).

% Base extractiveness: 0.45
% Rationale: While legally just, it "extracts" vast maritime territory and 
% historic claims from China. Conversely, the lack of its enforcement extracts 
% resources and safety from Filipino fishers.
domain_priors:base_extractiveness(south_china_sea_arbitration_2016_2026, 0.45).

% Suppression: 0.35
% Rationale: It suppresses the "nine-dash line" but is itself suppressed by 
% Chinese ICAD (Illegal, Coercive, Aggressive, and Deceptive) activities.
domain_priors:suppression_score(south_china_sea_arbitration_2016_2026, 0.35).

% Enforcement: Requires intense active maintenance (diplomacy, patrols, sanctions).
domain_priors:requires_active_enforcement(south_china_sea_arbitration_2016_2026).

% Beneficiaries & Victims
constraint_beneficiary(south_china_sea_arbitration_2016_2026, philippines).
constraint_beneficiary(south_china_sea_arbitration_2016_2026, international_rules_based_order).
constraint_victim(south_china_sea_arbitration_2016_2026, chinese_maritime_expansion).

% Metrics
narrative_ontology:constraint_metric(south_china_sea_arbitration_2016_2026, extractiveness, 0.45).
narrative_ontology:constraint_metric(south_china_sea_arbitration_2016_2026, suppression_requirement, 0.35).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: FILIPINO FISHERMAN - Snare
   --------------------------------------------------------------------------
   WHO: individual_powerless - Directly exposed to water cannons and ramming.
   WHEN: immediate - Daily survival at Scarborough Shoal (Bajo de Masinloc).
   WHERE: trapped - Physically limited by the reach of the Chinese Coast Guard.
   SCOPE: local - A specific reef or fishing ground.
   
   WHY THIS CLASSIFICATION:
   While the law is on their side, the *gap* between the law and reality is a 
   Snare. They are promised rights by a document (UNCLOS/Award) that currently 
   cannot stop a water cannon from destroying their livelihood.
   -------------------------------------------------------------------------- */



constraint_indexing:constraint_classification(
    south_china_sea_arbitration_2016_2026,
    snare,
    context(
        agent_power(individual_powerless),
        time_horizon(immediate),
        exit_options(trapped),
        spatial_scope(local)
    )
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: PH DEPT. OF FOREIGN AFFAIRS (DFA) - Mountain
   --------------------------------------------------------------------------
   WHO: institutional - Rule-shapers and primary legal advocates.
   WHEN: civilizational - Viewing the award as a permanent contribution to law.
   WHERE: mobile - Engaging in global diplomacy to "slay the giant."
   SCOPE: global - Advocating for the award in the UN and ASEAN.
   
   WHY THIS CLASSIFICATION:
   The DFA treats the ruling as "final and binding." It is a non-negotiable, 
   permanent feature of the legal geography (Mountain) that serves as the 
   unshakable foundation for all Philippine foreign policy.
   
   NARRATIVE EVIDENCE:
   "The Awards... are both final and binding... a concrete example of how 
   international law should work." (Lazaro, Jan 19, 2026)
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    south_china_sea_arbitration_2016_2026,
    mountain,
    context(
        agent_power(institutional),
        time_horizon(civilizational),
        exit_options(mobile),
        spatial_scope(global)
    )
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: CHINESE EMBASSY / MOFA - Snare
   --------------------------------------------------------------------------
   WHO: institutional (opposing) - Powerful state agency.
   WHEN: immediate - Managing "provocations" at sea.
   WHERE: arbitrage - Acting as if the law does not exist while using other UNCLOS parts.
   SCOPE: national - Protecting "territorial sovereignty."
   
   WHY THIS CLASSIFICATION:
   For Beijing, the Award is an illegal, extractive "Snare" created without 
   their consent, designed to "infringe on sovereignty" and "mislead public 
   opinion." They view it as a weaponized legal artifact.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    south_china_sea_arbitration_2016_2026,
    snare,
    context(
        agent_power(institutional),
        time_horizon(immediate),
        exit_options(arbitrage),
        spatial_scope(national)
    )
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: THE INSTITUTIONAL AGENT (ASEAN/UN) - Rope
   --------------------------------------------------------------------------
   WHO: agent_power(institutional) - Regional blocs and international bodies.
   WHEN: generational - The 2016 ruling sets a precedent for decades.
   WHERE: arbitrage - Managing the friction between national sovereignty and UNCLOS.
   SCOPE: global - Affects the freedom of navigation for all global trade.
   
   WHY THIS CLASSIFICATION:
   The Arbitration is a Rope. While it lacks the physical enforcement of a 
   Mountain, it serves as a high-fidelity coordination tool. International 
   actors use the 2016 ruling to "pull" maritime behavior toward the 
   Rules-Based Order. It provides the legal "slack" necessary for diplomatic 
   de-escalation while simultaneously tightening the reputational cost of 
   non-compliance.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    south_china_sea_arbitration_2016_2026,
    rope,
    context(
        agent_power(institutional),
        time_horizon(generational),
        exit_options(arbitrage),
        spatial_scope(global)
    )
) :-
    domain_priors:requires_active_enforcement(south_china_sea_arbitration_2016_2026),
    !.

% Explicit priors reflecting the coordination energy of international law.
domain_priors:base_extractiveness(south_china_sea_arbitration_2016_2026, 0.3).
domain_priors:suppression_score(south_china_sea_arbitration_2016_2026, 0.4).

/* ==========================================================================
   4. TESTS
   ========================================================================== */

:- begin_tests(scs_arbitration_2026_tests).

test(multi_perspective_variance) :-
    constraint_indexing:constraint_classification(south_china_sea_arbitration_2016_2026, Type1, context(individual_powerless, immediate, trapped, local)),
    constraint_indexing:constraint_classification(south_china_sea_arbitration_2016_2026, Type2, context(institutional, civilizational, mobile, global)),
    Type1 = snare,
    Type2 = mountain.

test(enforcement_extractiveness) :-
    % Extraction for fishers remains high despite the legal victory because of lack of physical enforcement.
    domain_priors:base_extractiveness(south_china_sea_arbitration_2016_2026, E),
    E > 0.4.

:- end_tests(scs_arbitration_2026_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * * Model: Gemini 2.0 Flash
 * * KEY DECISIONS:
 * 1. THE 2026 "LIVE" STATUS: I've updated the context to include the Jan 19, 
 * 2026 statements from the Philippine DFA and Chinese Embassy.
 * 2. CLASSIFICATION SHIFT: I classified the DFA view as "Mountain." In 2016, 
 * it was a "Rope" (negotiable standard), but in 2026, Manila's position has 
 * hardened into a permanent, non-negotiable reality.
 * 3. EXTRACTIVENESS: The extraction is dual-sided. It extracts 
 * "historic pride/territory" from China and "physical safety" from fishers 
 * through the resulting tension.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω)
   ========================================================================== */

omega_variable(
    un_general_assembly_referral,
    "Will the Philippines successfully forward the 2016 Award to the ICJ for an advisory opinion in 2026?",
    resolution_mechanism("Monitor the vote count in the UNGA during the 2026 September session."),
    impact("If Yes: The award hardens into a global 'Mountain'. If No: It remains a regional 'Snare' for China."),
    confidence_without_resolution(low)
).

omega_variable(
    asean_chairmanship_2026,
    "Will Manila's 2026 ASEAN Chairmanship produce a 'Code of Conduct' that references the 2016 Award?",
    resolution_mechanism("Review the final communique of the 2026 ASEAN Summit in Kuala Lumpur/Manila."),
    impact("If Yes: UNCLOS is a functional 'Rope'. If No: ASEAN remains a 'Scaffold' (hollow)."),
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES
 * * ALTERNATIVE 1: Bilateral Negotiation (China's preference)
 * Viability: China claims this has worked with other neighbors.
 * Suppression: Rejected by the Philippines as an asymmetric venue where the 
 * weaker party is coerced.
 * * ALTERNATIVE 2: "Historic Rights" (Natural Law)
 * Viability: Practiced for centuries before UNCLOS.
 * Suppression: Explicitly invalidated by the 2016 Tribunal as "without lawful effect."
 */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
