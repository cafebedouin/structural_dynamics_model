% ============================================================================
% CONSTRAINT STORY: collective_stupidity_2026
% ============================================================================
% Version: 3.4 (Deferential Realism Core)
% Logic: 3.3 (Indexed Tuple P,T,E,S)
% Generated: 2026-02-05
% ============================================================================

:- module(constraint_collective_stupidity_2026, []).

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
    narrative_ontology:constraint_beneficiary/2,
    narrative_ontology:constraint_victim/2,
    constraint_indexing:constraint_classification/3.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: collective_stupidity_2026
 * human_readable: The Cipolla-Galloway Stupidity Snare
 * domain: social/behavioral
 * * SUMMARY:
 * Stupidity is defined as causing damage to others without deriving personal gain. 
 * This creates a "Snare" because its lack of rational structure makes it 
 * impossible to organize a defense against. It is independent of credentials, 
 * affecting Ph.D.s and Presidents alike, leading to a "Sucking Black Hole" of 
 * societal utility.
 * * KEY AGENTS:
 * - Rational Agents (The Non-Stupid): Subject (Powerless)
 * - The Stupid (Self-Destructive Actors): Beneficiary (N/A - Net Negative)
 * - Narrative Engineers: Auditor (Analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Extraction is extreme (0.88). Stupidity extracts value from others 
% without creating it for anyone, representing a pure deadweight loss.
domain_priors:base_extractiveness(collective_stupidity_2026, 0.88). 

% Suppression is high (0.75) because the unpredictability of stupid 
% actions suppresses the ability of rational actors to organize a defense.
domain_priors:suppression_score(collective_stupidity_2026, 0.75).   

% Theater ratio is extreme (0.92). Stupid actions often wear the 
% mask of rational purpose but lack any functional outcome.
domain_priors:theater_ratio(collective_stupidity_2026, 0.92).       

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(collective_stupidity_2026, extractiveness, 0.88).
narrative_ontology:constraint_metric(collective_stupidity_2026, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(collective_stupidity_2026, theater_ratio, 0.92).

% Primary keys
% High-extraction stakeholders
narrative_ontology:constraint_beneficiary(collective_stupidity_2026, chaos_entropy).
narrative_ontology:constraint_victim(collective_stupidity_2026, rational_social_cohesion).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE RATIONAL AGENT (SNARE)
% For the non-stupid, stupidity is a Snare: a trap that lacks rational 
% structure and cannot be negotiated with or mitigated by reasoning.
constraint_indexing:constraint_classification(collective_stupidity_2026, snare, 
    context(agent_power(powerless), 
            time_horizon(biographical), 
            exit_options(trapped), 
            spatial_scope(global))).

% PERSPECTIVE 2: THE INSTITUTION (PITON)
% Organizations often treat stupidity as a Piton: an inertial, non-functional 
% behavior that persists despite causing constant damage.
constraint_indexing:constraint_classification(collective_stupidity_2026, piton, 
    context(agent_power(institutional), 
            time_horizon(generational), 
            exit_options(arbitrage), 
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (MOUNTAIN)
% From a civilizational view, the independent probability of stupidity 
% is a Mountain: an immutable, unchangeable physical fact of human systems.
constraint_indexing:constraint_classification(collective_stupidity_2026, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(collective_stupidity_2026_tests).

test(extraction_accumulation) :-
    domain_priors:base_extractiveness(collective_stupidity_2026, E),
    E > 0.46.

test(rational_defense_failure) :-
    domain_priors:theater_ratio(collective_stupidity_2026, TR),
    TR > 0.70.

:- end_tests(collective_stupidity_2026_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * Stupidity is modeled as a Snare because its core attribute is "net-negative" 
 * extraction. The high theater_ratio (0.92) captures the concept of "Hoodoo" 
 * or "Toxic People" who perform chaos without any underlying utility. 
 * The classification as a Mountain for analysts reflects the "Law of Stupidity": 
 * it is independent of all other human traits.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    omega_stupidity_duration,
    'Can the "duration and frequency" of stupidity be structurally reduced?',
    'Analysis of feedback loops in decentralized autonomous organizations.',
    'Success shifts the constraint to a Rope; Failure confirms it as a permanent Snare.',
    confidence_without_resolution(low)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(collective_stupidity_2026, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio remains high as stupidity is performative by nature.
narrative_ontology:measurement(st_tr_t0, collective_stupidity_2026, theater_ratio, 0, 0.92).
narrative_ontology:measurement(st_tr_t5, collective_stupidity_2026, theater_ratio, 5, 0.92).
narrative_ontology:measurement(st_tr_t10, collective_stupidity_2026, theater_ratio, 10, 0.92).

% Extraction rises as the "Sucking Black Hole" of stupidity accumulates damage.
narrative_ontology:measurement(st_ex_t0, collective_stupidity_2026, base_extractiveness, 0, 0.50).
narrative_ontology:measurement(st_ex_t5, collective_stupidity_2026, base_extractiveness, 5, 0.70).
narrative_ontology:measurement(st_ex_t10, collective_stupidity_2026, base_extractiveness, 10, 0.88).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
