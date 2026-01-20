% ============================================================================
% CONSTRAINT STORY: medieval_church_hegemony
% ============================================================================
% Generated: 2026-01-17
% Model: Gemini 2.0 Flash
% Source: Historical Synthesis (Medieval Canon Law & Social Structures c. 1200 CE)
% ============================================================================

:- module(medieval_church_hegemony, []).

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
 * * constraint_id: medieval_church_hegemony
 * human_readable: The Medieval Ecclesiastical Hegemony
 * domain: religious/economic/political
 * temporal_scope: c. 1100-1400 CE
 * spatial_scope: Latin Christendom (Europe)
 * * SUMMARY:
 * A comprehensive social and spiritual constraint system where the Church 
 * controls the "means of salvation." It features mandatory tithing (10% tax), 
 * Latin-only liturgy (blocking direct textual access), and the threat of 
 * excommunication to enforce social and political compliance.
 * * KEY AGENTS:
 * - The Peasant (Laboratores): Powerless subjects providing the economic base via tithes.
 * - The Bishop/Clergy (Oratores): Institutional rule-makers and gatekeepers of grace.
 * - The Heretic/Dissenter: Subjects attempting "analytical" exit, facing high suppression.
 * * NARRATIVE ARC:
 * The Church functions as a cosmic "Mountain" for the faithful, providing 
 * meaning and social safety nets, but acts as a "Noose" for those seeking 
 * economic or intellectual independence from the papal hierarchy.
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (The "Reality" Layer)
   ========================================================================== */

narrative_ontology:interval(medieval_church_hegemony, 0, 10).
narrative_ontology:constraint_claim(medieval_church_hegemony, mountain).

% Base extractiveness: High (0.7). 
% Rationale: The 10% tithe was mandatory and often enforced by civil law, 
% while the Church owned roughly 1/3 of European land.
domain_priors:base_extractiveness(medieval_church_hegemony, 0.7).

% Suppression score: High (0.9). 
% Rationale: Scripture was restricted to Latin; vernacular translations 
% were often suppressed or labeled heretical to maintain the clergy's 
% role as sole intermediaries.
domain_priors:suppression_score(medieval_church_hegemony, 0.9).

% Enforcement: Requires active enforcement (Inquisition, Interdicts, Tithe collectors).
domain_priors:requires_active_enforcement(medieval_church_hegemony).

% Metrics for Section 1 of the Executive Summary
narrative_ontology:constraint_metric(medieval_church_hegemony, extractiveness, 0.7).
narrative_ontology:constraint_metric(medieval_church_hegemony, suppression_requirement, 0.9).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: THE SERF/PEASANT - Mountain
   --------------------------------------------------------------------------
   WHO: individual_powerless
   WHEN: biographical
   WHERE: trapped
   SCOPE: local
   
   WHY THIS CLASSIFICATION:
   For the illiterate peasant, the Church is the literal fabric of reality. 
   The liturgical year dictates time, and the priest holds the keys to 
   eternity. With no alternative worldviews available (high suppression), 
   the system appears as an unchangeable natural law.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    medieval_church_hegemony,
    mountain,
    context(
        agent_power(individual_powerless),
        time_horizon(biographical),
        exit_options(trapped),
        spatial_scope(local)
    )
) :-
    domain_priors:suppression_score(medieval_church_hegemony, S),
    S > 0.8,
    !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: THE MONARCH/NOBILITY - Rope
   --------------------------------------------------------------------------
   WHO: individual_powerful
   WHEN: historical
   WHERE: constrained
   SCOPE: national
   
   WHY THIS CLASSIFICATION:
   Kings and Lords use the Church as a functional coordination mechanism 
   (Rope) to legitimize their rule via "Divine Right." While they clash 
   with the Pope over investiture, they rely on the Church for literacy, 
   administration, and social stability.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    medieval_church_hegemony,
    rope,
    context(
        agent_power(individual_powerful),
        time_horizon(historical),
        exit_options(constrained),
        spatial_scope(national)
    )
) :-
    domain_priors:base_extractiveness(medieval_church_hegemony, E),
    E < 0.8,
    !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: THE SUPPRESSED DISSENTER - Noose
   --------------------------------------------------------------------------
   WHO: individual_powerless
   WHEN: immediate
   WHERE: trapped
   SCOPE: regional
   
   WHY THIS CLASSIFICATION:
   For groups like the Cathars or early Lollards, the Church is a Noose. 
   The refusal to pay tithes or the desire for vernacular scripture 
   results in asymmetric violence, asset forfeiture, and physical 
   annihilation.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    medieval_church_hegemony,
    noose,
    context(
        agent_power(individual_powerless),
        time_horizon(immediate),
        exit_options(trapped),
        spatial_scope(regional)
    )
) :-
    domain_priors:requires_active_enforcement(medieval_church_hegemony),
    domain_priors:base_extractiveness(medieval_church_hegemony, E),
    E > 0.6,
    !.

/* ==========================================================================
   4. TESTS (What We Learn About Constraints)
   ========================================================================== */

:- begin_tests(medieval_church_tests).

test(perspectival_shift) :-
    constraint_indexing:constraint_classification(medieval_church_hegemony, Mountain, context(individual_powerless, biographical, trapped, local)),
    constraint_indexing:constraint_classification(medieval_church_hegemony, Rope, context(individual_powerful, historical, constrained, national)),
    Mountain \= Rope.

test(tithe_extractiveness) :-
    % Test that powerless agents feel more extraction than institutional ones
    true.

:- end_tests(medieval_church_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION (Commentary)
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * * Model: Gemini 2.0 Flash
 * * KEY DECISIONS:
 * 1. SUPPRESSION (0.9): Chosen due to the explicit ban on laypeople owning 
 * vernacular bibles (e.g., Council of Toulouse, 1229).
 * 2. EXTRACTIVENESS (0.7): While the tithe was a 10% extraction, the 
 * Church also provided "social insurance" (hospitals, poor relief) which 
 * distinguishes it from a 1.0 "pure robbery" Noose.
 * 3. PERSPECTIVES: I used the "Monarch" to show how a constraint can be a 
 * "Rope" for one class while being a "Mountain" or "Noose" for another.
 */

/* ==========================================================================
   6. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES
 * * ALTERNATIVE 1: Vernacular Scripture
 * Viability: Successfully implemented later during the Reformation.
 * Suppression: Actively suppressed by Clerical monopoly on Latin.
 * * ALTERNATIVE 2: Voluntary Charity (vs. Mandatory Tithe)
 * Viability: Practiced by mendicant orders (Franciscans) but discouraged 
 * as a systemic replacement for the parochial tithe.
 */

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

/**
 * TO USE THIS FILE:
 * ?- [medieval_church_hegemony].
 * ?- run_tests(medieval_church_tests).
 */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */


% --- v3.1 Indexical Relativity Stubs (Fleet Repair) ---
constraint_indexing:constraint_classification(medieval_church_hegomony, mountain, agent_power(analytical)).
constraint_indexing:constraint_classification(medieval_church_hegomony, rope, agent_power(institutional)).
constraint_indexing:constraint_classification(medieval_church_hegomony, noose, agent_power(individual_powerless)).
