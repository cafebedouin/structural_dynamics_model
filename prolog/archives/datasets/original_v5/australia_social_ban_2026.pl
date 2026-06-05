% ============================================================================
% CONSTRAINT STORY: australia_social_ban_2026
% ============================================================================
% Version: 3.4 (Deferential Realism Core)
% Logic: 3.3 (Indexed Tuple P,T,E,S)
% Generated: 2026-02-06
% ============================================================================

:- module(constraint_australia_social_ban_2026, []).

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
 * * constraint_id: australia_social_ban_2026
 * human_readable: Australian Under-16 Social Media Ban
 * domain: political/technological
 * * SUMMARY:
 * Effective December 2025, Australia's ban on social media for under-16s shifts 
 * the digital burden of proof onto platforms. While major platforms leverage 
 * existing behavioral data, privacy-focused ones like Substack extract 
 * identity documents (IDs) from users to avoid AU$50 million fines. This 
 * represents a structural pivot where digital interaction is prohibited 
 * unless expressly allowed.
 * * KEY AGENTS:
 * - Australian Minors (Under-16): Subject (Powerless)
 * - eSafety Commissioner / Government: Beneficiary (Institutional)
 * - Privacy Advocates / Substack Users: Auditor (Analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Extraction is high (0.72). The law extracts private identity data from adults 
% and digital access from minors.
domain_priors:base_extractiveness(australia_social_ban_2026, 0.72). 

% Suppression is high (0.88). The law "prohibits everything unless expressly 
% allowed," a departure from traditional English liberties.
domain_priors:suppression_score(australia_social_ban_2026, 0.88).   

% Theater ratio is moderate (0.55). Fines (AU$50M) exceed industrial 
% manslaughter penalties (AU$20M), suggesting a high theatrical emphasis 
% on "protection" over functional safety.
domain_priors:theater_ratio(australia_social_ban_2026, 0.55).       

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(australia_social_ban_2026, extractiveness, 0.72).
narrative_ontology:constraint_metric(australia_social_ban_2026, suppression_requirement, 0.88).
narrative_ontology:constraint_metric(australia_social_ban_2026, theater_ratio, 0.55).

% Primary keys for the classification engine
% Stakeholder declarations
narrative_ontology:constraint_beneficiary(australia_social_ban_2026, government_regulatory_bodies).
narrative_ontology:constraint_victim(australia_social_ban_2026, juvenile_digital_autonomy).

% Active enforcement is mandated by the AU$50 million penalty regime.
domain_priors:requires_active_enforcement(australia_social_ban_2026).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE MINOR (SNARE)
% For those under 16, the digital border is a Snare: an inescapable 
% prohibition with high suppression of social interaction.
constraint_indexing:constraint_classification(australia_social_ban_2026, snare, 
    context(agent_power(powerless), 
            time_horizon(biographical), 
            exit_options(trapped), 
            spatial_scope(national))).

% PERSPECTIVE 2: THE REGULATOR (ROPE)
% The eSafety Commissioner views the act as a Rope: essential coordination 
% to manage online safety and block under-16s from migration.
constraint_indexing:constraint_classification(australia_social_ban_2026, rope, 
    context(agent_power(institutional), 
            time_horizon(generational), 
            exit_options(mobile), 
            spatial_scope(national))).

% PERSPECTIVE 3: THE PRIVACY ADVOCATE (TANGLED ROPE)
% Analysts see a Tangled Rope: Coordination for "safety" mixed with 
% asymmetric extraction of ID documents from users.
constraint_indexing:constraint_classification(australia_social_ban_2026, tangled_rope,
    context(agent_power(analytical),
            time_horizon(historical),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(australia_social_ban_2026_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(australia_social_ban_2026, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(australia_social_ban_2026, TypeInstitutional, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeInstitutional.

test(extraction_threshold) :-
    domain_priors:base_extractiveness(australia_social_ban_2026, E),
    E >= 0.46.

:- end_tests(australia_social_ban_2026_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The extraction score (0.72) reflects the cost of ID verification 
 * forced upon privacy-focused platforms like Substack. 
 * The Theater Ratio (0.55) captures the massive disparity between 
 * social media fines and manslaughter penalties, indicating a 
 * performative political priority.
 *
 * * MANDATROPHY ANALYSIS:
 * [RESOLVED MANDATROPHY]
 * The system identifies a "Tangled Rope" for analysts because the 
 * stated goal of safety creates a genuine coordination layer, yet the 
 * "reasonable steps" requirement extracts significant privacy capital 
 * from the adult population.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    omega_id_leak_2026,
    'Will the government verification mechanism secure user data or leak it?',
    'Analysis of reported ID document leaks following the ban commencement.',
    'Secure data maintains the Rope; Leaks convert the adult experience to a Snare.',
    confidence_without_resolution(low)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(australia_social_ban_2026, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio rises as the list of "expressly excluded" platforms creates 
% a performative boundary for educational content.
narrative_ontology:measurement(au_tr_t0, australia_social_ban_2026, theater_ratio, 0, 0.35).
narrative_ontology:measurement(au_tr_t5, australia_social_ban_2026, theater_ratio, 5, 0.45).
narrative_ontology:measurement(au_tr_t10, australia_social_ban_2026, theater_ratio, 10, 0.55).

% Extraction spikes as platforms like Substack implement ID uploads.
narrative_ontology:measurement(au_ex_t0, australia_social_ban_2026, base_extractiveness, 0, 0.40).
narrative_ontology:measurement(au_ex_t5, australia_social_ban_2026, base_extractiveness, 5, 0.60).
narrative_ontology:measurement(au_ex_t10, australia_social_ban_2026, base_extractiveness, 10, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
