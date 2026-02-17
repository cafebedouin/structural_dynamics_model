% ============================================================================
% CONSTRAINT STORY: ulysses_oxen_1904
% ============================================================================
% Version: 3.4 (Deferential Realism Core)
% Logic: 3.3 (Indexed Tuple P,T,E,S)
% Generated: 2026-02-06
% ============================================================================

:- module(constraint_ulysses_oxen, []).

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
    narrative_ontology:interval/3,
    narrative_ontology:measurement/5,
    narrative_ontology:constraint_metric/3,
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
 * * constraint_id: ulysses_oxen_1904
 * human_readable: The Gestation Mountain (Holles Street)
 * domain: biological/linguistic/medical
 * * SUMMARY:
 * Chapter 14 models the "proliferent continuance" of life at the National 
 * Maternity Hospital. The nine-month term 
 * of gestation acts as an irreducible biological Mountain, while the 
 * evolution of the English language—parodied from Anglo-Saxon to modern 
 * slang—functions as a high-theater Piton of cultural history 
 *.
 * * KEY AGENTS:
 * - Mina Purefoy: Subject (Powerless) - Enduring the "original" evil of 
 * labor.
 * - The Medical Institution: Beneficiary (Institutional) - Coordinating 
 * the "tribute of its solicitude".
 * - The Wombfruit: Auditor (Analytical) - The "bright one, light one" 
 * moving toward birth.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
domain_priors:base_extractiveness(ulysses_oxen_1904, 0.55). % High physical and spiritual labor.
domain_priors:suppression_score(ulysses_oxen_1904, 0.95).   % Biological necessity of the term.
domain_priors:theater_ratio(ulysses_oxen_1904, 0.98).      % Peak linguistic mummery.

% Primary keys for classification engine
narrative_ontology:constraint_metric(ulysses_oxen_1904, extractiveness, 0.55).
narrative_ontology:constraint_metric(ulysses_oxen_1904, suppression_requirement, 0.95).
narrative_ontology:constraint_metric(ulysses_oxen_1904, theater_ratio, 0.98).

% Constraint classification claim
narrative_ontology:constraint_claim(ulysses_oxen_1904, piton).
narrative_ontology:human_readable(ulysses_oxen_1904, "The Gestation Mountain (Holles Street)").

% Structural Property Declarations
narrative_ontology:constraint_beneficiary(ulysses_oxen_1904, biological_continuance).
narrative_ontology:constraint_victim(ulysses_oxen_1904, mina_purefoy).
domain_priors:requires_active_enforcement(ulysses_oxen_1904). % The "Horhorn" quickening.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   ========================================================================== */

% PERSPECTIVE 1: THE LABORING MOTHER (SNARE)
% Effective Extraction: 0.55 * 1.5 (powerless) * 0.8 (local) = 0.66.
% Perceived as a "lutulent reality" of pain and "downwardtending" force 
% .
constraint_indexing:constraint_classification(ulysses_oxen_1904, snare, 
    context(agent_power(powerless), 
            time_horizon(biographical), 
            exit_options(trapped), 
            spatial_scope(local))).

% PERSPECTIVE 2: THE BIOLOGICAL LAW (MOUNTAIN)
% Effective Extraction: 0.55 * -0.2 (institutional) * 1.0 = -0.11.
% The "omnipollent nature’s incorrupted benefaction" functions as a 
% structural floor.
constraint_indexing:constraint_classification(ulysses_oxen_1904, mountain, 
    context(agent_power(institutional), 
            time_horizon(civilizational), 
            exit_options(mobile), 
            spatial_scope(universal))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (PITON)
% Extreme Theater Ratio (0.98) indicates a Piton of "exterior splendour" 
% masking "lutulent reality".
constraint_indexing:constraint_classification(ulysses_oxen_1904, piton,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ulysses_oxen_1904_tests).

test(perspectival_gap) :-
    % Verify shift from Snare (Mina) to Mountain (Biological Law).
    constraint_indexing:constraint_classification(ulysses_oxen_1904, snare, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(ulysses_oxen_1904, mountain, context(agent_power(institutional), _, _, _)).

test(piton_theater_check) :-
    % Piton classification requires theater_ratio >= 0.70.
    domain_priors:theater_ratio(ulysses_oxen_1904, TR),
    TR >= 0.70.

:- end_tests(ulysses_oxen_1904_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The "Oxen of the Sun" models the intersection of biological and 
 * linguistic evolution. While gestation is a Mountain—an irreducible 
 * "Fixed Point" of coordination for the species—the chapter's parodic 
 * style creates a peak Theater Ratio (0.98). This "mummery" of 
 * prose masks the "original evil" of physical pain, acting as a 
 * cultural Piton where the function of language is atrophied 
 * through imitation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    omega_linguistic_gestation,
    "Is the evolution of the English language a Rope for human sapience or a Snare of historical repetition?",
    "Review of the 'lutulent reality' vs the 'exterior splendour' of the parodies.",
    "Sapience confirms a Rope; 'downwardtending' repetition hardens the Snare.",
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ulysses_oxen_1904, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater Ratio (Peaking as the parodies move from medieval to 19th-century styles)
narrative_ontology:measurement(uo_tr_t0, ulysses_oxen_1904, theater_ratio, 0, 0.85).
narrative_ontology:measurement(uo_tr_t5, ulysses_oxen_1904, theater_ratio, 5, 0.99).
narrative_ontology:measurement(uo_tr_t10, ulysses_oxen_1904, theater_ratio, 10, 0.98).

% Extraction (Accumulating with the biological progress of labor and the drunken revelry)
narrative_ontology:measurement(uo_ex_t0, ulysses_oxen_1904, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(uo_ex_t5, ulysses_oxen_1904, base_extractiveness, 5, 0.52).
narrative_ontology:measurement(uo_ex_t10, ulysses_oxen_1904, base_extractiveness, 10, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
