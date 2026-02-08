% ============================================================================
% CONSTRAINT STORY: THAI_ARTICLE_112_MOUNTAIN
% ============================================================================
% Version: 3.4 (Deferential Realism Core)
% Logic: 3.3 (Indexed Tuple P,T,E,S)
% Generated: 2026-01-27
% ============================================================================

:- module(constraint_thai_article_112_mountain, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

% --- Namespace Hooks (Required for loading) ---
:- multifile 
    domain_priors:base_extractiveness/2,
    domain_priors:suppression_score/2,
    domain_priors:theater_ratio/2,
    domain_priors:requires_active_enforcement/1,
    narrative_ontology:has_sunset_clause/1,
    narrative_ontology:constraint_metric/3,
    narrative_ontology:constraint_claim/2,
    constraint_indexing:constraint_classification/3.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: thai_article_112_mountain
 * human_readable: Article 112 (Lèse-majesté Laws) as a Legal Mountain
 * domain: political/social/legal
 * * SUMMARY:
 * Article 112 of the Thai Criminal Code, which criminalizes insults against the 
 * monarchy, functions within the system as a "Mountain"—a fixed, unchangeable 
 * limit[cite: 80]. While logically a human-made law, the 2024 dissolution of the 
 * Move Forward Party [cite: 80, 81] and the subsequent exclusion of any party 
 * seeking its amendment from coalition talks in 2025/2026 [cite: 148] have 
 * effectively rendered it a physical-like boundary of the political possible.
 * * KEY AGENTS:
 * - People's Party: Subject (Organized) - Operates under the constraint while barred from voting for PMs who support amendment[cite: 148].
 * - Anutin Charnvirakul: Beneficiary (Institutional) - Rules out any coalition with those seeking amendment[cite: 148].
 * - The Constitutional Court: Auditor (Analytical) - Enforces the "Mountain" status by dissolving non-compliant parties[cite: 80].
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Mountain Requirement: Base extractiveness must be <= 0.05. 
% In this context, the "extraction" is zero-sum; the law is presented as a 
% "natural law" of the Thai state structure, making it a fixed coordinate.
domain_priors:base_extractiveness(thai_article_112_mountain, 0.04). 

% Suppression of alternatives is absolute; proposing them leads to party dissolution.
domain_priors:suppression_score(thai_article_112_mountain, 0.95).   

% Theater ratio is low because the enforcement is literal and terminal for political careers.
domain_priors:theater_ratio(thai_article_112_mountain, 0.10).       

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(thai_article_112_mountain, extractiveness, 0.04).
narrative_ontology:constraint_metric(thai_article_112_mountain, suppression_requirement, 0.95).
narrative_ontology:constraint_metric(thai_article_112_mountain, theater_ratio, 0.1).

% Constraint classification claim
narrative_ontology:constraint_claim(thai_article_112_mountain, mountain).

% Binary flags
domain_priors:requires_active_enforcement(thai_article_112_mountain).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE ANALYTICAL OBSERVER (MOUNTAIN)
% Viewed as an irreducible logical limit within the current Thai legal geometry.
constraint_indexing:constraint_classification(thai_article_112_mountain, mountain, 
    context(agent_power(analytical), 
            time_horizon(historical), 
            exit_options(analytical), 
            spatial_scope(national))).

% PERSPECTIVE 2: THE INSTITUTIONAL ACTOR (MOUNTAIN/ROPE)
% Viewed as a foundational, unchangeable pillar of state coordination.
constraint_indexing:constraint_classification(thai_article_112_mountain, mountain, 
    context(agent_power(institutional), 
            time_horizon(civilizational), 
            exit_options(mobile), 
            spatial_scope(national))).

% PERSPECTIVE 3: THE DISFRANCHISED PROGRESSIVE (SNARE)
% What the institution calls a "Mountain," the trapped agent experiences as 
% a predatory trap (Snare) that eliminates their political representation.
constraint_indexing:constraint_classification(thai_article_112_mountain, snare, 
    context(agent_power(powerless), 
            time_horizon(biographical), 
            exit_options(trapped), 
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(thai_article_112_mountain_tests).

test(mountain_validation) :-
    % Verify the analytical classification adheres to the Mountain extraction threshold.
    domain_priors:base_extractiveness(thai_article_112_mountain, E),

    E =< 0.05,
    constraint_indexing:constraint_classification(thai_article_112_mountain, mountain, context(agent_power(analytical), _, _, _)).

test(perspectival_gap_existence) :-
    % Verify the Subject sees a Snare while the Auditor sees a Mountain.
    constraint_indexing:constraint_classification(thai_article_112_mountain, snare, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(thai_article_112_mountain, mountain, context(agent_power(analytical), _, _, _)).

:- end_tests(thai_article_112_mountain_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * Article 112 is assigned the 'Mountain' type for the analytical index because 
 * in the 2026 political reality[cite: 4, 11], it functions as a non-negotiable 
 * physical limit of the environment. Unlike the Senate Veto (a Piton with high 
 * theater), Article 112 has a low theater ratio; the consequences for 
 * attempting to move the "mountain" are immediate and absolute.
 *
 * PERSPECTIVAL GAP:
 * The institutional power (Anutin/Conservatives) justifies the law as an 
 * irreducible pillar of the nation (Mountain)[cite: 148, 459]. The People's 
 * Party [cite: 85], having seen its predecessor dissolved[cite: 80], must treat 
 * it as a Mountain to survive, even if their base perceives it as a Snare.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    omega_legal_erosion,
    'Can a legal Mountain be converted back into a Snare or Rope through external international pressure?',
    'Observation of diplomatic and economic trade indices relative to Article 112 enforcement.',
    'Conversion to Snare (Change possible but coerced) vs Mountain Persistence (Unchangeable)',
    confidence_without_resolution(low)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(thai_article_112_mountain, 1908, 2026). 

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */

% ============================================================================
% ENRICHMENT: Structural predicates for remaining gaps
% Generated: 2026-02-08
% Template: v5.2 namespace alignment
% Source: Derived from narrative context in this file (thai_article_112_mountain)
% ============================================================================
constraint_beneficiary(thai_article_112_mountain, royalist_establishment).
constraint_victim(thai_article_112_mountain, progressive_political_movements).
