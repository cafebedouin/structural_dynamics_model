% ============================================================================
% CONSTRAINT STORY: correct_latin_kernel__discontinuity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_correct_latin_kernel__discontinuity_reading, []).

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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: correct_latin_kernel__discontinuity_reading
 *   human_readable: Discontinuity Reading of Correct Latin Kernel
 *   domain: historical_linguistics/philology/intellectual_history
 *
 * SUMMARY:
 *   This constraint represents the 'discontinuity reading' of the 'correct
 *   Latin kernel,' which posits that Classical Latin and Medieval Latin are
 *   fundamentally distinct linguistic systems. This reading emerged during
 *   the Renaissance, driven by humanists who sought to 'restore' a perceived
 *   pure form of Latin by treating Medieval Latin as a corruption. The
 *   constraint describes the active enforcement of this distinction and the
 *   resulting extraction of prestige and authority by those who championed
 *   the 'correct' form.
 *
 * KEY AGENTS:
 *   - renaissance_humanists: Agenda-setter (institutional/mobile) — actively enforced the discontinuity.
 *   - classical_philologists: Beneficiary (organized/constrained) — benefited from the institutionalization of this reading.
 *   - medieval_scribes: Payer (powerless/trapped) — their linguistic practices were devalued.
 *   - vernacular_scholars: Payer (moderate/constrained) — their work was implicitly devalued.
 *   - modern_linguists: Observer (analytical/analytical) — analyze the construct without normative judgment.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(correct_latin_kernel__discontinuity_reading, 0.6).
domain_priors:suppression_score(correct_latin_kernel__discontinuity_reading, 0.7).
domain_priors:theater_ratio(correct_latin_kernel__discontinuity_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(correct_latin_kernel__discontinuity_reading, extractiveness, 0.6).
narrative_ontology:constraint_metric(correct_latin_kernel__discontinuity_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(correct_latin_kernel__discontinuity_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(correct_latin_kernel__discontinuity_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(correct_latin_kernel__discontinuity_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(correct_latin_kernel__discontinuity_reading, tangled_rope).
narrative_ontology:human_readable(correct_latin_kernel__discontinuity_reading, "Discontinuity Reading of Correct Latin Kernel").
narrative_ontology:topic_domain(correct_latin_kernel__discontinuity_reading, "historical_linguistics/philology/intellectual_history").

domain_priors:requires_active_enforcement(correct_latin_kernel__discontinuity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(correct_latin_kernel__discontinuity_reading, '2d4419ba-5b9c-4de8-abab-4d978b5d875d').
narrative_ontology:cs_kernel_codification('2d4419ba-5b9c-4de8-abab-4d978b5d875d', fixed_text).
narrative_ontology:cs_authority_grounding('2d4419ba-5b9c-4de8-abab-4d978b5d875d', lineage).
narrative_ontology:cs_interpretation_layer_present('2d4419ba-5b9c-4de8-abab-4d978b5d875d').
narrative_ontology:cs_reading_relation('2d4419ba-5b9c-4de8-abab-4d978b5d875d', correct_latin_kernel__continuity_reading, forecloses).
narrative_ontology:cs_reading_relation('2d4419ba-5b9c-4de8-abab-4d978b5d875d', correct_latin_kernel__hybrid_reading, coexists_with).
narrative_ontology:cs_axiom('2d4419ba-5b9c-4de8-abab-4d978b5d875d', foundational, classical_latin_is_pure).
narrative_ontology:cs_axiom_status(classical_latin_is_pure, holdable).
narrative_ontology:cs_axiom_grounding('2d4419ba-5b9c-4de8-abab-4d978b5d875d', classical_latin_is_pure, conventional).
narrative_ontology:cs_axiom('2d4419ba-5b9c-4de8-abab-4d978b5d875d', foundational, medieval_latin_is_corrupt).
narrative_ontology:cs_axiom_status(medieval_latin_is_corrupt, holdable).
narrative_ontology:cs_axiom_grounding('2d4419ba-5b9c-4de8-abab-4d978b5d875d', medieval_latin_is_corrupt, conventional).
narrative_ontology:cs_reference_frame('2d4419ba-5b9c-4de8-abab-4d978b5d875d', renaissance_purist_ideal).
narrative_ontology:cs_drift_state('2d4419ba-5b9c-4de8-abab-4d978b5d875d', contemporary_linguistic_scholarship, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('2d4419ba-5b9c-4de8-abab-4d978b5d875d', '').
narrative_ontology:cs_kernel_id(correct_latin_kernel__discontinuity_reading, correct_latin_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(correct_latin_kernel__discontinuity_reading, renaissance_humanists).
narrative_ontology:constraint_beneficiary(correct_latin_kernel__discontinuity_reading, classical_philologists).
narrative_ontology:constraint_victim(correct_latin_kernel__discontinuity_reading, medieval_scribes).
narrative_ontology:constraint_victim(correct_latin_kernel__discontinuity_reading, vernacular_scholars).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Actively promoted the idea of a 'pure' Classical Latin, distinct from the 'corrupt' Medieval forms. They established new pedagogical norms and textual criticism methods to enforce this distinction, positioning themselves as the arbiters of linguistic correctness.
narrative_ontology:constraint_stakeholder(correct_latin_kernel__discontinuity_reading, renaissance_humanists, agenda_setter,
    institutional, generational, mobile, regional).

% Benefit from the institutionalization of Classical Latin as a distinct and superior object of study. Their careers and academic prestige are built on the premise of recovering and maintaining this 'correct' form, often at the expense of Medieval Latin studies.
narrative_ontology:constraint_stakeholder(correct_latin_kernel__discontinuity_reading, classical_philologists, beneficiary,
    organized, generational, constrained, global).

% Their linguistic practices, which evolved naturally over centuries, were retroactively deemed 'corrupt' or 'barbaric' by later humanists. They bore the cost of this re-evaluation through the devaluation of their work and the imposition of new, artificial standards.
narrative_ontology:constraint_stakeholder(correct_latin_kernel__discontinuity_reading, medieval_scribes, payer,
    powerless, biographical, trapped, local).

% Often found their work on developing vernacular languages implicitly or explicitly devalued by the emphasis on a 'pure' Classical Latin. The discontinuity reading reinforced a hierarchy where their linguistic innovations were seen as deviations from a superior standard.
narrative_ontology:constraint_stakeholder(correct_latin_kernel__discontinuity_reading, vernacular_scholars, payer,
    moderate, biographical, constrained, national).

% Analyze the historical development of Latin without normative judgment, often challenging the 'discontinuity' narrative by emphasizing natural language change. They observe the historical enforcement of the 'correct' Latin standard as a social and intellectual construct.
narrative_ontology:constraint_stakeholder(correct_latin_kernel__discontinuity_reading, modern_linguists, observer,
    analytical, generational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Established a shared, 'correct' standard for Latin scholarship and pedagogy, allowing humanists to coordinate their efforts in textual criticism and education around a common, idealized linguistic target.
% TRANSFER_FUNCTION: Transferred linguistic authority and prestige from medieval traditions to Renaissance humanists and later classical philologists, along with the associated academic and cultural capital.
% ABSENT_VOICES: Medieval grammarians and educators, whose understanding of Latin was based on living tradition and natural evolution, were retrospectively silenced. They would have argued for the legitimacy of their contemporary usage as a continuous development.
% DISAPPEARANCE_RATIONALE: If the 'discontinuity' reading vanished, the entire edifice of classical philology as a distinct discipline focused on 'recovering' a lost language would collapse. Medieval Latin would be re-integrated into a continuous history of Latin, fundamentally altering academic structures and research priorities.
% FOUNDING_PROBLEM: The perceived 'decline' and 'corruption' of Latin during the Middle Ages, leading to a desire among Renaissance scholars to restore a perceived golden age of linguistic purity and clarity.
% FOUNDING_PROBLEM_CORROBORATION: Renaissance humanists and classical philologists attest the problem is live, emphasizing the need for rigorous standards. Modern historical linguists, from outside the benefiting parties, largely view the 'corruption' narrative as a normative judgment rather than an objective linguistic problem, arguing that language naturally evolves, and the 'problem' was a construct of the humanists' agenda.
narrative_ontology:disappearance_verdict(correct_latin_kernel__discontinuity_reading, world_rearranges).
narrative_ontology:founding_problem_status(correct_latin_kernel__discontinuity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(correct_latin_kernel__discontinuity_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(correct_latin_kernel__discontinuity_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(correct_latin_kernel__discontinuity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(correct_latin_kernel__discontinuity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(correct_latin_kernel__discontinuity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.6) reflects the transfer of cultural and academic capital from medieval traditions to classical humanism. Suppression (0.7) is high due to the active pedagogical and critical efforts to delegitimize Medieval Latin and enforce the 'correct' standard. Theater ratio (0.2) is relatively low, as the efforts to establish and enforce the distinction were genuinely functional in shaping academic discourse, even if based on a contested premise. Accessibility collapse (0.4) is moderate; while the 'correct' Latin became dominant, Medieval Latin texts and practices never fully disappeared. Resistance (0.3) was present from those who continued to use or study Medieval Latin, but it was largely overcome by the institutional power of the humanists.
 *
 * PERSPECTIVAL GAP:
 *   Renaissance humanists and classical philologists experienced this as a necessary coordination to restore linguistic purity. Medieval scribes and vernacular scholars, however, experienced it as an extractive imposition that devalued their work and traditions. Modern linguists view it as a historical construct with significant social and intellectual consequences.
 *
 * DIRECTIONALITY LOGIC:
 *   Renaissance humanists and classical philologists are beneficiaries, as the constraint elevates their academic and cultural standing. Medieval scribes and vernacular scholars are victims, as their linguistic practices and scholarship were devalued. Modern linguists are observers, analyzing the phenomenon without direct benefit or cost from its operation.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate (restoring 'pure' Latin) outlived its initial justification as linguistic understanding evolved. What began as a coordination effort to standardize Latin became a mechanism for academic and cultural gatekeeping. The 'discontinuity' reading, while initially functional for establishing a new scholarly paradigm, eventually became a source of extraction by devaluing alternative linguistic traditions. The engine's classification as Tangled Rope, rather than a pure Rope, captures this hybrid nature, where a coordination function (standardization) is intertwined with asymmetric extraction (devaluation of medieval forms).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    linguistic_purity_vs_evolution,
    'Is the distinction between Classical and Medieval Latin a matter of ''purity'' and ''corruption'' (as this reading claims), or a natural process of linguistic evolution?',
    'Further historical linguistic analysis, including sociolinguistic studies of Latin usage across different periods, to determine the extent of actual structural change versus normative judgment.',
    'If linguistic evolution is the dominant factor, the ''discontinuity'' reading''s foundational premise is weakened, potentially reclassifying it closer to a Snare or Piton, as its coordination function would be revealed as a cover for extraction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(linguistic_purity_vs_evolution, empirical, 'The underlying nature of the linguistic shift between Classical and Medieval Latin.').

omega_variable(
    reconstruction_as_recovery_vs_invention,
    'To what extent was the ''reconstruction'' of Classical Latin a recovery of a lost system, versus a creative re-invention or idealization by Renaissance humanists?',
    'Detailed philological comparison of humanist grammars and dictionaries with actual Classical texts, alongside analysis of the rhetorical strategies used to promote the ''new'' Latin.',
    'If largely an invention, the ''discontinuity'' reading''s claim to objective recovery is undermined, increasing its theater_ratio and extractiveness, as the ''coordination'' would be based on an artificial standard.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reconstruction_as_recovery_vs_invention, conceptual, 'The epistemic status of the ''reconstructed'' Classical Latin.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(correct_latin_kernel__discontinuity_reading, 1400, 1900).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(corr_tr_t1400, correct_latin_kernel__discontinuity_reading, theater_ratio, 1400, 0.1).
narrative_ontology:measurement(corr_tr_t1500, correct_latin_kernel__discontinuity_reading, theater_ratio, 1500, 0.15).
narrative_ontology:measurement(corr_tr_t1600, correct_latin_kernel__discontinuity_reading, theater_ratio, 1600, 0.18).
narrative_ontology:measurement(corr_tr_t1700, correct_latin_kernel__discontinuity_reading, theater_ratio, 1700, 0.19).
narrative_ontology:measurement(corr_tr_t1800, correct_latin_kernel__discontinuity_reading, theater_ratio, 1800, 0.2).
narrative_ontology:measurement(corr_tr_t1900, correct_latin_kernel__discontinuity_reading, theater_ratio, 1900, 0.2).

% Extraction over time
narrative_ontology:measurement(corr_be_t1400, correct_latin_kernel__discontinuity_reading, base_extractiveness, 1400, 0.4).
narrative_ontology:measurement(corr_be_t1500, correct_latin_kernel__discontinuity_reading, base_extractiveness, 1500, 0.5).
narrative_ontology:measurement(corr_be_t1600, correct_latin_kernel__discontinuity_reading, base_extractiveness, 1600, 0.55).
narrative_ontology:measurement(corr_be_t1700, correct_latin_kernel__discontinuity_reading, base_extractiveness, 1700, 0.58).
narrative_ontology:measurement(corr_be_t1800, correct_latin_kernel__discontinuity_reading, base_extractiveness, 1800, 0.59).
narrative_ontology:measurement(corr_be_t1900, correct_latin_kernel__discontinuity_reading, base_extractiveness, 1900, 0.6).

% Suppression requirement over time
narrative_ontology:measurement(corr_su_t1400, correct_latin_kernel__discontinuity_reading, suppression_requirement, 1400, 0.5).
narrative_ontology:measurement(corr_su_t1500, correct_latin_kernel__discontinuity_reading, suppression_requirement, 1500, 0.6).
narrative_ontology:measurement(corr_su_t1600, correct_latin_kernel__discontinuity_reading, suppression_requirement, 1600, 0.65).
narrative_ontology:measurement(corr_su_t1700, correct_latin_kernel__discontinuity_reading, suppression_requirement, 1700, 0.68).
narrative_ontology:measurement(corr_su_t1800, correct_latin_kernel__discontinuity_reading, suppression_requirement, 1800, 0.69).
narrative_ontology:measurement(corr_su_t1900, correct_latin_kernel__discontinuity_reading, suppression_requirement, 1900, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(correct_latin_kernel__discontinuity_reading, identity_coordination).
narrative_ontology:affects_constraint(correct_latin_kernel__discontinuity_reading, correct_latin_kernel__continuity_reading).
narrative_ontology:affects_constraint(correct_latin_kernel__discontinuity_reading, correct_latin_kernel__hybrid_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'correct_latin_kernel'. This 'discontinuity_reading' emphasizes the distinctness of Classical and Medieval Latin, contrasting with the 'continuity_reading' and 'hybrid_reading'.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
