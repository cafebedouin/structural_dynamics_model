% ============================================================================
% CONSTRAINT STORY: gs1_gln_identification
% ============================================================================
% Generated: 2026-01-19
% Model: Gemini 2.0 Flash
% Source: GS1 General Specifications (Release 25.0) / GLN Management Rules
% ============================================================================

:- module(constraint_gs1_gln, []).

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
 * * constraint_id: gs1_gln_identification
 * human_readable: Global Location Number (GLN) Standard
 * domain: technological/economic
 * temporal_scope: 1974-Present
 * spatial_scope: Global (Logistics and Healthcare)
 * * SUMMARY:
 * The GLN is a 13-digit GS1 identification key used to identify physical 
 * locations (warehouses, hospital rooms) or legal entities (corporations). 
 * It constrains the digital "map" of global commerce by enforcing a 
 * single, globally unique identifier for every point of business interaction.
 * * KEY AGENTS:
 * - Warehouse Receiving Clerk: Individual powerless; scans GLNs as an 
 * immutable part of their environment.
 * - Supply Chain Director: Institutional; uses GLNs to coordinate complex 
 * inter-organizational shipping and billing.
 * - Regulatory Auditor: Analytical; evaluates the traceability and 
 * data integrity provided by the GLN hierarchy.
 * * NARRATIVE ARC:
 * What functions as a vital Rope for coordination in high-stakes industries 
 * like healthcare (ensuring meds go to the right room) appears as a 
 * Mountain to the workers who must manage the hardware. For small clinics 
 * or vendors, the mandatory adoption and licensing fees function as a Snare.
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (The "Reality" Layer)
   ========================================================================== */

% Required for [STEP 1] and [STEP 2] of the DR-Audit Suite
narrative_ontology:interval(gln_standard_lifecycle, 0, 10).
narrative_ontology:constraint_claim(gs1_gln_identification, rope).

% Base extractiveness score (0.0-1.0)
% Rationale: 0.3. Moderate; while fees exist, the primary "extraction" 
% is the administrative labor required to maintain accurate location registries.
domain_priors:base_extractiveness(gs1_gln_identification, 0.3).

% Suppression score (0.0-1.0)
% Rationale: 0.7. High; proprietary or internal location codes are 
% actively suppressed in B2B transactions to prevent "translation errors."
domain_priors:suppression_score(gs1_gln_identification, 0.7).

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(gs1_gln_identification, extractiveness, 0.3).
narrative_ontology:constraint_metric(gs1_gln_identification, suppression_requirement, 0.7).

% Enforcement requirements
% Requires active enforcement (Check digit validation and GS1 Registry lookup).
domain_priors:requires_active_enforcement(gs1_gln_identification).

% Metrics for Executive Summary
% BENEFICIARIES & VICTIMS
constraint_beneficiary(gs1_gln_identification, global_traceability_efficiency).
constraint_beneficiary(gs1_gln_identification, enterprise_resource_planning_vendors).
constraint_victim(gs1_gln_identification, small_scale_suppliers_overhead).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: RECEIVING CLERK - Mountain
   --------------------------------------------------------------------------
   
   WHO: powerless - Cannot change the GLN; must scan or manual enter.
   WHEN: immediate - Tactical execution of inventory receipt.
   WHERE: trapped - Bound by the software validation rules of the WMS.
   SCOPE: local - A specific loading dock or shelf.
   
   WHY THIS CLASSIFICATION:
   To the clerk, the GLN is a Mountain. It is an unchangeable, 13-digit fact 
   of the building. If the scanner doesn't recognize the GLN, the world stops. 
   There is zero room for negotiation with the barcode.
   -------------------------------------------------------------------------- */



constraint_indexing:constraint_classification(
    gs1_gln_identification,
    mountain,
    context(
        agent_power(powerless),
        time_horizon(immediate),
        exit_options(trapped),
        spatial_scope(local)
    )
) :-
    domain_priors:requires_active_enforcement(gs1_gln_identification),
    !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: LOGISTICS VP - Rope
   --------------------------------------------------------------------------
   
   WHO: institutional - Power to mandate GLN usage across a 1,000-node network.
   WHEN: biographical - Planning multi-year infrastructure upgrades.
   WHERE: mobile - Can adjust internal processes to accommodate the standard.
   SCOPE: global - Tracking cargo from manufacture to final destination.
   
   WHY THIS CLASSIFICATION:
   For the executive, the GLN is a Rope. It is a functional coordination 
   mechanism that weaves together thousands of disparate locations into 
   a single, actionable thread. It provides the "standardized tension" 
   required to hold a global supply chain together.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    gs1_gln_identification,
    rope,
    context(
        agent_power(institutional),
        time_horizon(biographical),
        exit_options(mobile),
        spatial_scope(global)
    )
) :-
    domain_priors:base_extractiveness(gs1_gln_identification, E),
    E < 0.5,
    !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: DATA PRIVACY ANALYST - Snare
   --------------------------------------------------------------------------
   
   WHO: analytical - Observer of the "surveillance" potential of fixed IDs.
   WHEN: historical - Evaluating the shift from physical to digital location.
   WHERE: analytical - Not bound by the commercial need for efficiency.
   SCOPE: global - Data sovereignty and competitive intelligence.
   
   WHY THIS CLASSIFICATION:
   The analyst identifies the Snare. Because GLNs are public and standardized, 
   they allow competitors to map a company's entire logistical footprint 
   with high precision. The "efficiency" of the Rope extracts strategic 
   secrecy, choking off a company's ability to hide its supply chain nodes.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    gs1_gln_identification,
    snare,
    context(
        agent_power(analytical),
        time_horizon(historical),
        exit_options(analytical),
        spatial_scope(global)
    )
) :-
    domain_priors:suppression_score(gs1_gln_identification, S),
    S > 0.6,
    !.

/* ==========================================================================
   4. TESTS (What We Learn About Constraints)
   ========================================================================== */

:- begin_tests(gs1_gln_tests).

test(multi_perspective_coordination) :-
    % VP (Rope) vs Clerk (Mountain)
    constraint_indexing:constraint_classification(gs1_gln_identification, T1, context(institutional, biographical, mobile, global)),
    constraint_indexing:constraint_classification(gs1_gln_identification, T2, context(powerless, immediate, trapped, local)),
    T1 \= T2.

test(enforcement_check) :-
    domain_priors:requires_active_enforcement(gs1_gln_identification).

:- end_tests(gs1_gln_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION (Commentary)
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * * Model: Gemini 2.0 Flash
 * * KEY DECISIONS:
 * 1. EXTRACTIVENESS (0.3): Rated lower than GTIN because locations are 
 * static, whereas products are high-turnover. The extraction is 
 * bureaucratic rather than purely financial.
 * 2. SUPPRESSION (0.7): High, because a "private" location number is 
 * essentially useless in the modern B2B ecosystem.
 */

omega_variable(
    registry_accuracy_decay,
    "Does the 'Mountain' status fail if the GS1 Registry data becomes 
     stale/inaccurate over time?",
    resolution_mechanism("Audit of GLN physical-to-digital mapping accuracy in healthcare sectors"),
    impact("If Inaccurate: The Mountain collapses into a Snare of errors. 
            If Accurate: It remains a reliable Rope."),
    confidence_without_resolution(medium)
).

/* ==========================================================================
   6. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES
 * * ALTERNATIVE 1: Proprietary Address Matching (fuzzy logic)
 * Viability: High for humans; low for automated systems due to 
 * "Industrial City, St 4" vs "4th Street Ind. Park" variations.
 * Suppression: Suppressed by the need for machine-readability and 
 * zero-ambiguity in automated warehouses.
 * * CONCLUSION:
 * The failure of fuzzy-logic alternatives in automated systems 
 * necessitates the GLN Rope, which eventually ossifies into a 
 * Mountain for the end-user.
 */

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% 1. Load: ?- [gs1_gln_identification].
% 2. Analyze: ?- multi_index_report(gs1_gln_identification).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */

% ============================================================================
% ENRICHMENT: Structural predicates for dynamic classification
% Generated: 2026-02-08
% Template: v5.2 namespace alignment
% Source: Derived from existing narrative and structural content in this file
% ============================================================================

% --- Multifile declarations for new predicates ---
:- multifile
    domain_priors:theater_ratio/2.

% --- Theater ratio (missing from base properties) ---
% Functional coordination mechanism — primarily substantive
domain_priors:theater_ratio(gs1_gln_identification, 0.13).
narrative_ontology:constraint_metric(gs1_gln_identification, theater_ratio, 0.13).
