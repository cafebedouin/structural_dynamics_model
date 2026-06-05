% ============================================================================
% CONSTRAINT STORY: antikythera_knowledge_loss
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-01
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_antikythera_knowledge_loss, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

% --- Constraint Identity Rule (DP-001: ε-Invariance) ---
% Each constraint story must have a single, stable base extractiveness (ε).
% If changing the observable used to evaluate this constraint would change ε,
% you are looking at two distinct constraints. Write separate .pl files for
% each, link them with affects_constraint/2, and document the relationship
% in both files' narrative context sections.
%
% The context tuple is CLOSED at arity 4: (P, T, E, S).
% Do not add measurement_basis, beneficiary/victim, or any other arguments.
% Linter Rule 23 enforces context/4.
%
% See: epsilon_invariance_principle.md

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: antikythera_knowledge_loss
 *   human_readable: Loss of Hellenistic Precision Gearing Knowledge
 *   domain: technological
 *
 * SUMMARY:
 *   The Antikythera Mechanism, a 2nd-century BC analog computer of
 *   astonishing complexity, demonstrates a level of mechanical and
 *   astronomical knowledge that was subsequently lost to the world for over
 *   1,500 years. The loss of this knowledge represents a significant setback
 *   in technological progress.
 *
 * KEY AGENTS:
 *   - Hellenistic Engineers: Primary victim (powerless/trapped) - their knowledge was lost.
 *   - Roman Empire: Beneficiary through the lack of need for high precision instruments (institutional/constrained) - simplified technology sufficient.
 *   - Medieval Craftsmen: Constrained, some benefit but mainly victims of lost ability (moderate/constrained)
 *   - Modern Scholars: Analytical observer attempting to understand the loss (analytical/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(antikythera_knowledge_loss, 0.6).
domain_priors:suppression_score(antikythera_knowledge_loss, 0.7).
domain_priors:theater_ratio(antikythera_knowledge_loss, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(antikythera_knowledge_loss, extractiveness, 0.6).
narrative_ontology:constraint_metric(antikythera_knowledge_loss, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(antikythera_knowledge_loss, theater_ratio, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(antikythera_knowledge_loss, tangled_rope).
narrative_ontology:human_readable(antikythera_knowledge_loss, "Loss of Hellenistic Precision Gearing Knowledge").
narrative_ontology:topic_domain(antikythera_knowledge_loss, "technological").

domain_priors:requires_active_enforcement(antikythera_knowledge_loss).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(antikythera_knowledge_loss, roman_military_expansion).
narrative_ontology:constraint_beneficiary(antikythera_knowledge_loss, lack_of_specialization).
narrative_ontology:constraint_victim(antikythera_knowledge_loss, hellenistic_engineers).
narrative_ontology:constraint_victim(antikythera_knowledge_loss, scientific_progress).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Perspective of the Hellenistic engineers whose knowledge was lost. They were unable to transmit or preserve their skills and knowledge due to societal factors. No exit; high extraction.
constraint_indexing:constraint_classification(antikythera_knowledge_loss, snare,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(global))).

% The Roman Empire, while benefiting from technological advancements, did not actively foster or preserve the specific knowledge required for such complex devices. Constrained to maintaining existing technologies, not innovating. Piton due to decay of specific knowledge domain.
constraint_indexing:constraint_classification(antikythera_knowledge_loss, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% Medieval craftsmen, lacking the specific knowledge, could not replicate the Antikythera mechanism but benefited from some related technologies. Constrained by available knowledge and resources. Tangled rope: some benefit, but extraction in terms of lost potential.
constraint_indexing:constraint_classification(antikythera_knowledge_loss, tangled_rope,
    context(agent_power(moderate),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% Analytical perspective observing the knowledge loss over centuries, understanding the complex interplay of factors that led to the suppression of such advanced technology. Sees tangled rope due to mixed coordination (related technologies preserved) and extraction (key knowledge lost).
constraint_indexing:constraint_classification(antikythera_knowledge_loss, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(antikythera_knowledge_loss_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(antikythera_knowledge_loss, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(antikythera_knowledge_loss, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(antikythera_knowledge_loss, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(antikythera_knowledge_loss, TR),
    TR >= 0.70.

:- end_tests(antikythera_knowledge_loss_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.6): High, reflecting the significant loss of knowledge. Suppression (0.7): High, few attempts to rediscover such knowledge, focus on different paths. Theater Ratio(0.2): Low, this wasn't actively hidden, just neglected.
 *
 * PERSPECTIVAL GAP:
 *   The Hellenistic engineers were the direct victims. Later groups were limited by what was preserved. The Roman focus shifted to practical engineering, suppressing more abstract understanding, resulting in the current information available.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries: Roman military expansion benefitted from practical technological advancements. Victims: Hellenistic engineers experienced a suppression of their knowledge, directly impacting their legacy and future contributions. The analytical observer views the loss as a historical phenomenon, assessing the structural factors that contributed to the knowledge's disappearance.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification prevents mislabeling coordination as pure extraction because the Roman empire benefited not from the destruction of knowledge but from its focus shifting, in a way losing touch with more complex scientific pursuits. It's a question of focus and resources more than direct suppression.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    specificity_of_loss,
    'Was the loss specific to the exact Antikythera mechanism design, or did it encompass broader precision engineering principles?',
    'Historical analysis of surviving texts and artifacts from the period.',
    'Narrow loss implies contingent factors; broad loss implies systemic suppression.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(specificity_of_loss, empirical, 'Scope of the knowledge loss: narrow vs. broad.').

omega_variable(
    role_of_roman_expansion,
    'To what extent did the Roman Empire''s focus on military and practical engineering contribute to the decline of theoretical and scientific pursuits?',
    'Comparative analysis of scientific advancements in regions under Roman vs. non-Roman influence.',
    'High contribution indicates active suppression; low contribution indicates passive neglect.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(role_of_roman_expansion, empirical, 'Impact of Roman expansion on knowledge loss.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(antikythera_knowledge_loss, 0, 1000).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(anti_tr_t0, antikythera_knowledge_loss, theater_ratio, 0, 0.1).
narrative_ontology:measurement(anti_tr_t500, antikythera_knowledge_loss, theater_ratio, 500, 0.2).
narrative_ontology:measurement(anti_tr_t1000, antikythera_knowledge_loss, theater_ratio, 1000, 0.3).

% Extraction over time
narrative_ontology:measurement(anti_be_t0, antikythera_knowledge_loss, base_extractiveness, 0, 0.2).
narrative_ontology:measurement(anti_be_t500, antikythera_knowledge_loss, base_extractiveness, 500, 0.4).
narrative_ontology:measurement(anti_be_t1000, antikythera_knowledge_loss, base_extractiveness, 1000, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
