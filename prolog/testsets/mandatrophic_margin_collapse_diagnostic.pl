% [RESOLVED MANDATROPHY] High-extraction Mountain identified as structural mandate.
% ============================================================================
% CONSTRAINT STORY: mandatrophy_systemic_collapse
% ============================================================================
% Generated: 2026-01-20
% Model: Gemini 2.0 Flash
% Source: Synthesis of Boeing, NASA MCO, and Geopolitical Mandatrophy Patterns
% ============================================================================

:- module(constraint_mandatrophy_diagnostic, []).

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
 * * constraint_id: mandatrophy_systemic_collapse
 * human_readable: Mandatrophy (Systemic Resilience Wasting)
 * domain: institutional/technological/economic
 * temporal_scope: Universal (Industrial/Late-Capitalist Era)
 * spatial_scope: Complex Systems (Aviation, Energy, Governance)
 * * SUMMARY:
 * Mandatrophy is the "invisible" extraction of a system's resilience (its "margin") 
 * to satisfy a high-priority administrative or political goal (its "mandate"). 
 * By treating safety buffers, technical expertise, and physical maintenance as 
 * "wasteful slack," the institution gains short-term performance while 
 * creating a state of "brittle stability" that ensures catastrophic failure 
 * when the first unanticipated stressor arrives.
 * * KEY AGENTS:
 * - The Mandator (KPI Architect): Institutional; views all non-active resources 
 * as "excess" to be optimized for the goal.
 * - The Operator (Maintenance Sensor): Individual powerless; the engineer or 
 * technician who perceives the tightening Snare as the "slack" disappears.
 * - The Secondary Victim: Individual powerless; the passenger, citizen, or 
 * consumer who experiences the sudden transition from "Rope" to "Mountain."
 * * NARRATIVE ARC:
 * The process is initially perceived as a Rope (optimizing a stagnant system). 
 * As the extraction continues, the system becomes a Snare, where every 
 * operational choice is "choked" by the lack of redundancy. Finally, the 
 * system hits a Mountain (a physical crash or grid collapse) where the 
 * mandate can no longer negotiate with material reality.
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (The "Reality" Layer)
   ========================================================================== */

% Structural Anchor for index extraction
narrative_ontology:interval(mandatrophy_lifecycle, 0, 10).
narrative_ontology:constraint_claim(mandatrophy_systemic_collapse, snare).

% Base extractiveness score (0.0-1.0)
% Rationale: 0.9. Extreme; Mandatrophy extracts the *future* survival capacity 
% of the system to pay for the *present* metric.
domain_priors:base_extractiveness(mandatrophy_systemic_collapse, 0.9).

% Suppression score (0.0-1.0)
% Rationale: 0.75. Alternatives like "Resilience Margin" and "Redundancy" 
% are suppressed as being "inefficient" or "obsolete" by the mandating logic.
domain_priors:suppression_score(mandatrophy_systemic_collapse, 0.75).

% Enforcement requirements
% Requires active enforcement (Audits, Lean-management KPIs, and the 
% removal of "dissenting" experts who prioritize the margin).
domain_priors:requires_active_enforcement(mandatrophy_systemic_collapse).

% Metrics for Executive Summary
narrative_ontology:constraint_metric(mandatrophy_systemic_collapse, extractiveness, 0.9).
narrative_ontology:constraint_metric(mandatrophy_systemic_collapse, suppression_requirement, 0.75).

% BENEFICIARIES & VICTIMS
constraint_beneficiary(mandatrophy_systemic_collapse, short_term_fiscal_metrics).
constraint_beneficiary(mandatrophy_systemic_collapse, administrative_centralization).
constraint_victim(mandatrophy_systemic_collapse, long_term_infrastructure_safety).
constraint_victim(mandatrophy_systemic_collapse, institutional_cognitive_diversity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: THE MANDATOR (HQ) - Rope
   --------------------------------------------------------------------------
   
   WHO: institutional - Power to set the "Lean" targets and divert maintenance funds.
   WHEN: biographical - Focused on the successful "turnaround" of the organization.
   WHERE: mobile - Not physically located at the "friction point" of the boilers/engines.
   SCOPE: national/global - Managing the aggregate performance.
   
   WHY THIS CLASSIFICATION:
   For HQ, the mandate is a Rope. It is the tool they use to "pull" a lazy or 
   bloated system toward modern efficiency. They view the "margin" as 
   parasitic drag that must be eliminated to succeed.
   -------------------------------------------------------------------------- */



constraint_indexing:constraint_classification(
    mandatrophy_systemic_collapse,
    rope,
    context(
        agent_power(institutional),
        time_horizon(biographical),
        exit_options(mobile),
        spatial_scope(national)
    )
) :-
    domain_priors:base_extractiveness(mandatrophy_systemic_collapse, E),
    E < 0.95,
    !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: THE OPERATOR / ENGINEER - Snare
   --------------------------------------------------------------------------
   
   WHO: individual_powerless - Bound by the mandate; must operate the 
         machine without the spare parts.
   WHEN: immediate - Tactical daily struggle to prevent "Yellow" alerts 
         from turning "Red."
   WHERE: trapped - Working within the failing facility or airframe.
   SCOPE: local - Immediate sensor range and physical environment.
   
   WHY THIS CLASSIFICATION:
   The operator sees the Snare. They feel the system "tightening"—each time 
   a redundancy is removed, their degrees of freedom decrease. The mandate 
   is a coercive force that chokes their ability to maintain safety.
   -------------------------------------------------------------------------- */



constraint_indexing:constraint_classification(
    mandatrophy_systemic_collapse,
    snare,
    context(
        agent_power(individual_powerless),
        time_horizon(immediate),
        exit_options(trapped),
        spatial_scope(local)
    )
) :-
    domain_priors:requires_active_enforcement(mandatrophy_systemic_collapse),
    !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: THE PASSENGER / CITIZEN - Mountain
   --------------------------------------------------------------------------
   
   WHO: individual_powerless - Subject to the "unforeseen" grid collapse or 
         technical malfunction.
   WHEN: historical - The sudden moment when the "Rope" breaks.
   WHERE: trapped - In the blackout, in the air, or in the energy crisis.
   SCOPE: global - The impact of systemic failure on society.
   
   WHY THIS CLASSIFICATION:
   For the victim, the collapse is a Mountain. It is a sudden, unyielding 
   event that appears as an Act of God or a "freak accident." They cannot 
   negotiate with a plane that has lost its engineering margin; they simply 
   hit the physical limit of the mandate's greed.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    mandatrophy_systemic_collapse,
    mountain,
    context(
        agent_power(individual_powerless),
        time_horizon(historical),
        exit_options(trapped),
        spatial_scope(global)
    )
) :-
    !.

/* ==========================================================================
   4. TESTS (What We Learn About Constraints)
   ========================================================================== */

:- begin_tests(mandatrophy_expansion_tests).

test(brittle_failure_signature) :-
    % Mandatrophy systems appear as Ropes to managers but have high 
    % extractiveness (Snare) for operators.
    constraint_indexing:constraint_classification(mandatrophy_systemic_collapse, rope, context(institutional, _, _, _)),
    domain_priors:base_extractiveness(mandatrophy_systemic_collapse, E),
    E > 0.8.

test(diagnostic_blindness) :-
    % The system suppresses the "Alternative" (Margin) to preserve the mandate.
    domain_priors:suppression_score(mandatrophy_systemic_collapse, S),
    S > 0.6.

:- end_tests(mandatrophy_expansion_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION (Commentary)
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * * Model: Gemini 2.0 Flash
 * * KEY DECISIONS:
 * 1. PERSPECTIVE MINIMUMS: Expanded to include the "Secondary Victim" 
 * (Mountain) to show how Mandatrophy creates sudden "Acts of God" out 
 * of slow administrative choices.
 * 2. SELECTION BIAS: I've noted that Mandatrophy is specifically the 
 * *selective* extraction of margins that are invisible to HQ but vital to 
 * the Operator.
 */

omega_variable(
    the_audit_paradox,
    "Do 'Safety Audits' in a mandatrophic system identify the loss of 
     margin (Rope) or merely validate the mandate (Snare)?",
    resolution_mechanism("Comparative analysis of Boeing self-certification 
    logs vs. physical failure post-mortems"),
    impact("If they validate mandate: The audit is a Snare component. 
            If they find margin-loss: It's an external Scaffold."),
    confidence_without_resolution(low)
).

/* ==========================================================================
   6. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES
 * * ALTERNATIVE 1: Redundancy-First Engineering (The "Apollo" Scaffold)
 * Viability: Historically proven to maintain safety in unknown environments.
 * Suppression: Actively rejected in "Faster, Better, Cheaper" or 
 * "Lean" eras as an inefficient use of capital.
 * * * ALTERNATIVE 2: Worker-Led Safety Veto
 * Viability: Allowing the Operator to stop the "Mandate" when the margin 
 * is breached (e.g., Alcoa's safety turn-around).
 * Suppression: Suppressed by "Schedule-Driven" environments where 
 * stoppage is treated as insubordination.
 */

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% 1. Load: ?- [mandatrophy_systemic_collapse].
% 2. Analyze: ?- multi_index_report(mandatrophy_systemic_collapse).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */


% --- v3.1 Indexical Relativity Stubs (Fleet Repair) ---
constraint_indexing:constraint_classification(mandatrophic_margin_collapse_diagnostic, mountain, agent_power(analytical)).
constraint_indexing:constraint_classification(mandatrophic_margin_collapse_diagnostic, rope, agent_power(institutional)).
constraint_indexing:constraint_classification(mandatrophic_margin_collapse_diagnostic, snare, agent_power(individual_powerless)).
