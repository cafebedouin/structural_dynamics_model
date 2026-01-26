% ============================================================================
% CONSTRAINT STORY: cia_fbi_legal_wall
% ============================================================================
% Generated: 2026-01-19
% Model: Gemini 2.0 Flash
% Source: The 9/11 Commission Report (2004)
% ============================================================================

:- module(constraint_cia_fbi_legal_wall, []).

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
 * * constraint_id: cia_fbi_legal_wall
 * human_readable: The CIA/FBI Intelligence-Criminal "Wall"
 * domain: political/legal
 * temporal_scope: 1995-2001 (Pre-USA PATRIOT Act)
 * spatial_scope: National (USA)
 * * SUMMARY:
 * The "Wall" was a set of legal and procedural barriers intended to separate 
 * foreign intelligence from domestic criminal investigations to protect civil 
 * liberties. In practice, it became a rigid organizational silo 
 * that prevented the sharing of critical terrorism data between the CIA and 
 * FBI field offices.
 * * KEY AGENTS:
 * - FBI Case Agent: Powerless subject bound by DOJ procedures.
 * - DOJ Office of Intelligence Policy and Review (OIPR): Institutional enforcers of the wall.
 * - Intelligence Community Leadership: Strategic actors attempting to navigate the silo.
 * * NARRATIVE ARC:
 * Originally established to prevent abuses (Rope), the Wall ossified into a 
 * Mountain for the agents on the ground, eventually functioning as a Snare 
 * that extracted national security by blinding the FBI to the presence of 
 * hijackers like Mihdhar and Hazmi inside the US.
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (The "Reality" Layer)
   ========================================================================== */

% Required for [STEP 1] and [STEP 2] of the DR-Audit Suite
narrative_ontology:interval(pre_911_silo_regime, 0, 10).
narrative_ontology:constraint_claim(cia_fbi_legal_wall, mountain).

% Base extractiveness score (0.0 = no extraction, 1.0 = full extraction)
% Rationale: High (0.7). While the intent was protection of rights, the 
% "extraction" was the systematic denial of safety information to the public 
% .
domain_priors:base_extractiveness(cia_fbi_legal_wall, 0.7).

% Suppression score (0.0 = no suppression, 1.0 = full suppression)
% Rationale: Severe (0.85). The Wall explicitly punished or prohibited the 
% visibility of information alternatives across the agency divide.
domain_priors:suppression_score(cia_fbi_legal_wall, 0.85).

% Enforcement requirements
% Requires active enforcement (DOJ OIPR and the FISA Court enforced the wall).
domain_priors:requires_active_enforcement(cia_fbi_legal_wall).

% Metrics required for Section 1 of the Executive Summary
narrative_ontology:constraint_metric(cia_fbi_legal_wall, extractiveness, 0.7).
narrative_ontology:constraint_metric(cia_fbi_legal_wall, suppression_requirement, 0.85).

% Beneficiaries and Victims
constraint_beneficiary(cia_fbi_legal_wall, institutional_risk_avoidance).
constraint_victim(cia_fbi_legal_wall, the_american_public).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: FBI FIELD AGENT - Mountain
   --------------------------------------------------------------------------
   
   WHO: individual_powerless - Bound by the "Wall" protocols.
   WHEN: immediate - Tactical focus on current investigations.
   WHERE: trapped - No authority to bypass DOJ "Special Agents in Charge" instructions.
   SCOPE: local - Focused on specific suspects in specific cities.
   
   WHY THIS CLASSIFICATION:
   To an agent, the Wall was an immovable feature of legal reality. Failing to 
   obey it meant career termination or legal jeopardy, regardless of its 
   logic.
   
   NARRATIVE EVIDENCE:
   "FBI headquarters... maintained a 'wall' between the intelligence and 
   criminal sides... This prevented analysts from seeing all relevant 
   information".
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    cia_fbi_legal_wall,
    mountain,
    context(
        agent_power(individual_powerless),
        time_horizon(immediate),
        exit_options(trapped),
        spatial_scope(local)
    )
) :-
    domain_priors:requires_active_enforcement(cia_fbi_legal_wall),
    !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: DOJ OIPR OFFICIAL - Rope
   --------------------------------------------------------------------------
   
   WHO: institutional - Power to interpret and enforce the wall.
   WHEN: biographical - Lifetime focus on legal precedent and civil liberties.
   WHERE: constrained - High political cost of violating FISA court norms.
   SCOPE: national - Overseeing the legal integrity of federal law enforcement.
   
   WHY THIS CLASSIFICATION:
   For the enforcers, the Wall was a vital Rope—a mechanism to ensure that 
   the power of the state was coordinated within Constitutional limits.
   
   NARRATIVE EVIDENCE:
   The DOJ officials were "concerned with preserving the purity of the 
   FISA-criminal distinction" to avoid legal challenges.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    cia_fbi_legal_wall,
    rope,
    context(
        agent_power(institutional),
        time_horizon(biographical),
        exit_options(constrained),
        spatial_scope(national)
    )
) :-
    domain_priors:suppression_score(cia_fbi_legal_wall, S),
    S > 0.5,
    !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: POST-9/11 FAMILIES/COMMISSIONERS - Snare
   --------------------------------------------------------------------------
   
   WHO: analytical - Looking back at the cost of the silo.
   WHEN: historical - Evaluating the barrier's role in the 2001 catastrophe.
   WHERE: analytical - Free from the pre-2001 legal constraints.
   SCOPE: global - Recognizing the trans-national consequences of a local silo.
   
   WHY THIS CLASSIFICATION:
   From a retrospective view, the Wall was a Snare. It tightened the range of 
   possible actions until the only outcome left was the success of the 
   terrorist plot.
   
   NARRATIVE EVIDENCE:
   The Commission described the wall as "the single greatest structural barrier" 
   that prevented the prevention of the attacks.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    cia_fbi_legal_wall,
    snare,
    context(
        agent_power(analytical),
        time_horizon(historical),
        exit_options(analytical),
        spatial_scope(global)
    )
) :-
    domain_priors:base_extractiveness(cia_fbi_legal_wall, E),
    E > 0.6,
    !.

/* ==========================================================================
   4. TESTS (What We Learn About Constraints)
   ========================================================================== */

:- begin_tests(cia_fbi_legal_wall_tests).

test(silo_extraction_variance) :-
    % Powerless agents (FBI field) experience the silo as a more extractive 
    % constraint on their effectiveness than institutional leaders.
    constraint_indexing:extractiveness_for_agent(cia_fbi_legal_wall, context(individual_powerless, immediate, trapped, local), Score1),
    constraint_indexing:extractiveness_for_agent(cia_fbi_legal_wall, context(institutional, biographical, constrained, national), Score2),
    Score1 > Score2.

test(wall_immutability_by_horizon) :-
    % For those trapped in the immediate timeframe, the Wall is a Mountain.
    constraint_indexing:effective_immutability(immediate, trapped, mountain).

:- end_tests(cia_fbi_legal_wall_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION (Commentary)
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * * Model: Gemini 2.0 Flash
 * * KEY DECISIONS:
 * 1. SUPPRESSION SCORE (0.85): High because the Wall didn't just 
 * separate agencies; it actively prevented analysts from even *knowing* * that information existed on the other side.
 * * 2. CLASSIFICATION SHIFT: I modeled the DOJ perspective as "Rope" to 
 * show the tragedy of the constraint: it was intended to be a 
 * "good" rule that turned "bad" through lack of feedback.
 */

omega_variable(
    civil_liberties_tradeoff,
    "Would a 'Wall-less' system pre-9/11 have resulted in domestic abuses that outweighed the security gain?",
    resolution_mechanism("Simulate domestic surveillance outcomes without 2001 legal constraints"),
    impact("If Yes: The Wall remains a tragic Rope. If No: The Wall was a pure Snare."),
    confidence_without_resolution(low)
).

/* ==========================================================================
   6. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES
 * * ALTERNATIVE 1: Integrated "Lead" Agencies
 * Viability: The UK's MI5 model (domestic intelligence separate from police but integrated) 
 * existed as a viable alternative but was culturally rejected in the US.
 * * CONCLUSION:
 * The existence of successful international alternatives suggests that the 
 * American "Wall" was not a natural "Mountain" of law, but a specific 
 * institutional choice (Snare/Rope).
 */

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% TO USE:
% 1. Load: ?- [constraint_cia_fbi_legal_wall].
% 2. Report: ?- constraint_indexing:multi_index_report(cia_fbi_legal_wall).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
