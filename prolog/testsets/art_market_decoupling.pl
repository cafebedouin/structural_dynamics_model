% ============================================================================
% CONSTRAINT STORY: art_market_decoupling
% ============================================================================
% Version: 3.4 (Deferential Realism Core)
% Logic: 3.3 (Indexed Tuple P,T,E,S)
% Generated: 2026-02-04
% ============================================================================

:- module(constraint_art_decoupling, []).

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
    narrative_ontology:interval/3,
    narrative_ontology:measurement/5,
    narrative_ontology:constraint_claim/2,
    narrative_ontology:constraint_beneficiary/2,
    narrative_ontology:constraint_victim/2,
    constraint_indexing:constraint_classification/3.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: art_market_decoupling
 * human_readable: The Obscene Decoupling
 * domain: economic/social
 * * SUMMARY:
 * David Bowie identifies a three-way decoupling between the artist, the 
 * elitist "art world" (networking/status), and the "obscene" art market 
 * (investment/stocks). Creativity becomes "lost in the middle" 
 * as the market treats art as a speculative asset.
 * * KEY AGENTS:
 * - The Artist: Subject (Powerless) - Creativity lost in the system.
 * - The Investor: Beneficiary (Institutional) - Buying art as stocks.
 * - The Critic/Bowie: Auditor (Analytical) - Observing the "disgusting" 
 * decoupling of value.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
% Extraction is "obscene" due to speculative "amounts of money".
domain_priors:base_extractiveness(art_market_decoupling, 0.84). 
% Suppression via "elitist club" gates: "you have to know the right people".
domain_priors:suppression_score(art_market_decoupling, 0.75).   
% High theater: "all about networking and status... being seen".
domain_priors:theater_ratio(art_market_decoupling, 0.92).       

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(art_market_decoupling, extractiveness, 0.84).
narrative_ontology:constraint_metric(art_market_decoupling, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(art_market_decoupling, theater_ratio, 0.92).

% Constraint classification claim
narrative_ontology:constraint_claim(art_market_decoupling, piton).

% Binary flags
domain_priors:requires_active_enforcement(art_market_decoupling).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE ARTIST (SNARE)
% High extraction ($E=0.84$) where creativity is lost to the market.
constraint_indexing:constraint_classification(art_market_decoupling, snare, 
    context(agent_power(powerless), 
            time_horizon(biographical), 
            exit_options(trapped), 
            spatial_scope(national))).

% PERSPECTIVE 2: THE INVESTOR (ROPE)
% Viewed as coordination infrastructure for capital ("buying stocks").
constraint_indexing:constraint_classification(art_market_decoupling, rope, 
    context(agent_power(institutional), 
            time_horizon(generational), 
            exit_options(mobile), 
            spatial_scope(global))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (PITON)
% Detects high theater ($TR=0.92$) maintaining a non-functional social club.
constraint_indexing:constraint_classification(art_market_decoupling, piton, 
    context(agent_power(analytical), 
            time_horizon(historical), 
            exit_options(analytical), 
            spatial_scope(global))) :-
    domain_priors:theater_ratio(art_market_decoupling, TR), TR > 0.70.

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(art_tests).

test(perspectival_gap) :-
    % Verify the Artist sees a Snare while the Investor sees a Rope.
    constraint_indexing:constraint_classification(art_market_decoupling, snare, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(art_market_decoupling, rope, context(agent_power(institutional), _, _, _)).

test(piton_status) :-
    % Verify Piton classification for the "disgusting" elitist theater.
    domain_priors:theater_ratio(art_market_decoupling, TR), TR > 0.70.

:- end_tests(art_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The "Perspectival Gap" here is a total decoupling of intent. The Investor 
 * coordinates wealth through "investment" (Rope), while the Artist 
 * is extracted from their own "creativity" (Snare). 
 * * [RESOLVED MANDATROPHY]:
 * The extreme extraction ($E=0.84$) is not a coordination tax; it is a 
 * speculative bubble where art serves as a theater for status ($TR=0.92$).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_creative_residue,
    'Does the art piece retain "good" quality independent of its market price?',
    'Blind aesthetic audit vs market value tracking over 50 years.',
    'If quality is low: Pure Piton. If quality is high: Tangled Rope.',
    confidence_without_resolution(low)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(art_market_decoupling, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio: Rising from "Community Openings" to "Global Networking Status."
narrative_ontology:measurement(art_tr_t0, art_market_decoupling, theater_ratio, 0, 0.45).
narrative_ontology:measurement(art_tr_t5, art_market_decoupling, theater_ratio, 5, 0.70).
narrative_ontology:measurement(art_tr_t10, art_market_decoupling, theater_ratio, 10, 0.92).

% Extraction: Transition from patronage to "obscene" stock-like investment.
narrative_ontology:measurement(art_ex_t0, art_market_decoupling, base_extractiveness, 0, 0.30).
narrative_ontology:measurement(art_ex_t5, art_market_decoupling, base_extractiveness, 5, 0.60).
narrative_ontology:measurement(art_ex_t10, art_market_decoupling, base_extractiveness, 10, 0.84).

/* ==========================================================================
   9. STRUCTURAL ENRICHMENT (BENEFICIARY / VICTIM)
   ========================================================================== */

% Piton enrichment: vestigial extraction identified from narrative context.
% The investor class treats art as speculative assets, while working artists
% lose creative autonomy to the "obscene" decoupling of value from creation.
narrative_ontology:constraint_beneficiary(art_market_decoupling, art_market_investors).
narrative_ontology:constraint_victim(art_market_decoupling, working_artists).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
