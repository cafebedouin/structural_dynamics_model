% ============================================================================
% CONSTRAINT STORY: juvenile_underclass_2026
% ============================================================================
% Version: 3.4 (Deferential Realism Core)
% Logic: 3.3 (Indexed Tuple P,T,E,S)
% Generated: 2026-02-05
% ============================================================================

:- module(constraint_juvenile_underclass_2026, []).

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
    narrative_ontology:constraint_claim/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:human_readable/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: juvenile_underclass_2026
 * human_readable: The Minor Underclass Structural Constraint
 * domain: social/political
 * * SUMMARY:
 * Children function as a permanent underclass, defined by a total lack of 
 * political agency and economic self-determination. While childhood is temporary 
 * for the individual, the *class* of children remains a permanent site of 
 * high extraction (labor preparation, institutional management) and total 
 * suppression of autonomy.
 * * KEY AGENTS:
 * - Children (Minors): Subject (Powerless)
 * - Educational/State Institutions: Beneficiary (Institutional)
 * - Human Rights Auditors: Auditor (Analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Extraction is extreme (0.75). The state and guardians extract time and 
% agency for "socialization" without the subject's consent or exit options.
domain_priors:base_extractiveness(juvenile_underclass_2026, 0.75). 

% Suppression is near-total (0.90). Minors are legally prohibited from 
% independent economic activity or political participation.
domain_priors:suppression_score(juvenile_underclass_2026, 0.90).   

% Theater ratio is high (0.80). The "protection" and "education" narratives 
% often mask the functional use of the underclass for labor-force grooming.
domain_priors:theater_ratio(juvenile_underclass_2026, 0.80).       

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(juvenile_underclass_2026, extractiveness, 0.75).
narrative_ontology:constraint_metric(juvenile_underclass_2026, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(juvenile_underclass_2026, theater_ratio, 0.8).

% Constraint classification claim
narrative_ontology:constraint_claim(juvenile_underclass_2026, piton).
narrative_ontology:human_readable(juvenile_underclass_2026, "The Minor Underclass Structural Constraint").

% Primary keys for the classification engine
% Stakeholder declarations
narrative_ontology:constraint_beneficiary(juvenile_underclass_2026, adult_institutional_hegemony).
narrative_ontology:constraint_victim(juvenile_underclass_2026, juvenile_autonomy).

% Active enforcement is the norm (Guardianship laws)
domain_priors:requires_active_enforcement(juvenile_underclass_2026).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE CHILD (MOUNTAIN)
% From the child's perspective, being a minor is an immutable Mountain: 
% an irreducible biological and legal limit with zero degrees of freedom.
constraint_indexing:constraint_classification(juvenile_underclass_2026, mountain, 
    context(agent_power(powerless), 
            time_horizon(biographical), 
            exit_options(trapped), 
            spatial_scope(national))).

% PERSPECTIVE 2: THE STATE (ROPE)
% Institutions view the management of children as a Rope: essential 
% coordination for future social stability and "civilized" development.
constraint_indexing:constraint_classification(juvenile_underclass_2026, rope, 
    context(agent_power(institutional), 
            time_horizon(generational), 
            exit_options(mobile), 
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (PITON)
% Analysts view the current guardianship model as a Piton: an inertial 
% maintenance of control that has lost functional alignment with modern rights.
constraint_indexing:constraint_classification(juvenile_underclass_2026, piton,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(juvenile_underclass_2026_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(juvenile_underclass_2026, mountain, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(juvenile_underclass_2026, rope, context(agent_power(institutional), _, _, _)).

test(piton_check) :-
    domain_priors:theater_ratio(juvenile_underclass_2026, TR),
    TR >= 0.70.

test(extraction_threshold) :-
    domain_priors:base_extractiveness(juvenile_underclass_2026, E),
    E > 0.46.

:- end_tests(juvenile_underclass_2026_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The extraction score (0.75) reflects the compulsory nature of schooling and 
 * the total absence of economic rights. The high theater_ratio (0.80) 
 * identifies the "Child Protection" industry as a performative mask for 
 * maintaining the class status. 
 *
 * * MANDATROPHY ANALYSIS:
 * [RESOLVED MANDATROPHY]
 * While the individual "exits" the class upon reaching adulthood, the 
 * structural Piton remains for the next cohort, indicating that the 
 * constraint is an inherent property of the social architecture.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    omega_juvenile_rights,
    'Can digital agency allow minors to bypass physical guardianship Snares?',
    'Analysis of decentralized identity adoption among under-18 populations.',
    'Success shifts the underclass to a Scaffold; Failure confirms the Piton.',
    confidence_without_resolution(low)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(juvenile_underclass_2026, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio rises as institutional "protection" metrics replace functional well-being.
narrative_ontology:measurement(ju_tr_t0, juvenile_underclass_2026, theater_ratio, 0, 0.40).
narrative_ontology:measurement(ju_tr_t5, juvenile_underclass_2026, theater_ratio, 5, 0.65).
narrative_ontology:measurement(ju_tr_t10, juvenile_underclass_2026, theater_ratio, 10, 0.80).

% Extraction remains high, reflecting the steady accumulation of institutional debt.
narrative_ontology:measurement(ju_ex_t0, juvenile_underclass_2026, base_extractiveness, 0, 0.50).
narrative_ontology:measurement(ju_ex_t5, juvenile_underclass_2026, base_extractiveness, 5, 0.62).
narrative_ontology:measurement(ju_ex_t10, juvenile_underclass_2026, base_extractiveness, 10, 0.75).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
