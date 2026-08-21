% ============================================================================
% CONSTRAINT STORY: state_commitment_installation_mechanism__endogenous_climb_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_state_commitment_installation_mechanism__endogenous_climb_reading, []).

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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: state_commitment_installation_mechanism__endogenous_climb_reading
 *   human_readable: State Commitment Installation: Endogenous Climb Reading
 *   domain: historical_sociology/state_formation/cultural_authority
 *
 * SUMMARY:
 *   This constraint describes a mechanism by which new commitments (e.g.,
 *   scientific paradigms, legal principles, social norms) gain legitimacy
 *   within a state or society by demonstrating their superiority and climbing
 *   from institutional fringes to widespread adoption. It is a reading of the
 *   broader 'state_commitment_installation_mechanism' kernel, focusing on
 *   bottom-up legitimation rather than top-down imposition. The process is
 *   characterized by gradual adoption, visible grassroots advocacy, and
 *   resistance from established orders.
 *
 * KEY AGENTS:
 *   - fringe_actors: Primary beneficiaries (powerless/mobile) — initiate and demonstrate new commitments
 *   - advocates_of_new_commitment: Agenda setters (organized/constrained) — actively promote and build support
 *   - adopting_institutions: Secondary beneficiaries (institutional/constrained) — integrate new commitments into established structures
 *   - established_institutions: Primary payers (institutional/constrained) — resist displacement by new commitments
 *   - adherents_of_old_commitments: Secondary payers (moderate/identity_locked) — bear the cost of their commitments losing relevance
 *   - historical_sociologists: Analytical observers (analytical/analytical) — study the mechanism itself
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(state_commitment_installation_mechanism__endogenous_climb_reading, 0.4).
domain_priors:suppression_score(state_commitment_installation_mechanism__endogenous_climb_reading, 0.3).
domain_priors:theater_ratio(state_commitment_installation_mechanism__endogenous_climb_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(state_commitment_installation_mechanism__endogenous_climb_reading, extractiveness, 0.4).
narrative_ontology:constraint_metric(state_commitment_installation_mechanism__endogenous_climb_reading, suppression_requirement, 0.3).
narrative_ontology:constraint_metric(state_commitment_installation_mechanism__endogenous_climb_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(state_commitment_installation_mechanism__endogenous_climb_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(state_commitment_installation_mechanism__endogenous_climb_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(state_commitment_installation_mechanism__endogenous_climb_reading, rope).
narrative_ontology:human_readable(state_commitment_installation_mechanism__endogenous_climb_reading, "State Commitment Installation: Endogenous Climb Reading").
narrative_ontology:topic_domain(state_commitment_installation_mechanism__endogenous_climb_reading, "historical_sociology/state_formation/cultural_authority").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(state_commitment_installation_mechanism__endogenous_climb_reading, '918cd64f-c16e-497e-a772-320bb15d8022').
narrative_ontology:cs_kernel_codification('918cd64f-c16e-497e-a772-320bb15d8022', implicit).
narrative_ontology:cs_authority_grounding('918cd64f-c16e-497e-a772-320bb15d8022', practice).
narrative_ontology:cs_interpretation_layer_present('918cd64f-c16e-497e-a772-320bb15d8022').
narrative_ontology:cs_reading_relation('918cd64f-c16e-497e-a772-320bb15d8022', state_commitment_installation_mechanism__exogenous_imposition_reading, coexists_with).
narrative_ontology:cs_reading_relation('918cd64f-c16e-497e-a772-320bb15d8022', state_commitment_installation_mechanism__hybrid_cascade_reading, coexists_with).
narrative_ontology:cs_axiom('918cd64f-c16e-497e-a772-320bb15d8022', foundational, legitimacy_from_demonstrated_utility).
narrative_ontology:cs_axiom_status(legitimacy_from_demonstrated_utility, holdable).
narrative_ontology:cs_axiom_grounding('918cd64f-c16e-497e-a772-320bb15d8022', legitimacy_from_demonstrated_utility, empirically_contingent).
narrative_ontology:cs_axiom('918cd64f-c16e-497e-a772-320bb15d8022', foundational, bottom_up_social_change).
narrative_ontology:cs_axiom_status(bottom_up_social_change, holdable).
narrative_ontology:cs_axiom_grounding('918cd64f-c16e-497e-a772-320bb15d8022', bottom_up_social_change, empirically_contingent).
narrative_ontology:cs_reference_frame('918cd64f-c16e-497e-a772-320bb15d8022', evolutionary_legitimacy_paradigm).
narrative_ontology:cs_drift_state('918cd64f-c16e-497e-a772-320bb15d8022', contemporary_historical_sociology, gap(stable, minor, true)).
narrative_ontology:cs_created_at('918cd64f-c16e-497e-a772-320bb15d8022', '').
narrative_ontology:cs_kernel_id(state_commitment_installation_mechanism__endogenous_climb_reading, state_commitment_installation_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(state_commitment_installation_mechanism__endogenous_climb_reading, fringe_actors).
narrative_ontology:constraint_beneficiary(state_commitment_installation_mechanism__endogenous_climb_reading, advocates_of_new_commitment).
narrative_ontology:constraint_beneficiary(state_commitment_installation_mechanism__endogenous_climb_reading, adopting_institutions).
narrative_ontology:constraint_victim(state_commitment_installation_mechanism__endogenous_climb_reading, established_institutions).
narrative_ontology:constraint_victim(state_commitment_installation_mechanism__endogenous_climb_reading, adherents_of_old_commitments).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These are the initial adopters and innovators of new commitments, often operating at the margins of established power structures. They benefit from the new commitment's demonstrated superiority, which allows them to gain influence and legitimacy.
narrative_ontology:constraint_stakeholder(state_commitment_installation_mechanism__endogenous_climb_reading, fringe_actors, beneficiary,
    powerless, biographical, mobile, local).

% Groups or individuals actively promoting the new commitment, demonstrating its efficacy and building coalitions for its adoption. They invest significant effort in the 'climb' and gain authority as the commitment becomes more widespread.
narrative_ontology:constraint_stakeholder(state_commitment_installation_mechanism__endogenous_climb_reading, advocates_of_new_commitment, agenda_setter,
    organized, biographical, constrained, regional).

% Established organizations or state apparatuses that eventually integrate the new commitment, often after its superiority has been widely demonstrated. They benefit from increased efficiency, stability, or legitimacy that the new commitment provides.
narrative_ontology:constraint_stakeholder(state_commitment_installation_mechanism__endogenous_climb_reading, adopting_institutions, beneficiary,
    institutional, generational, constrained, national).

% Existing power structures or organizations whose authority or practices are challenged and eventually displaced by the rising new commitment. They bear the cost of losing relevance, resources, and legitimacy, often resisting the change.
narrative_ontology:constraint_stakeholder(state_commitment_installation_mechanism__endogenous_climb_reading, established_institutions, payer,
    institutional, generational, constrained, national).

% Individuals or groups deeply invested in the older, declining commitments, often due to identity or tradition. They experience the erosion of their worldview and social standing as the new commitment gains dominance.
narrative_ontology:constraint_stakeholder(state_commitment_installation_mechanism__endogenous_climb_reading, adherents_of_old_commitments, payer,
    moderate, biographical, identity_locked, local).

% Scholars who analyze the long-term historical processes of state formation and cultural change, observing how new commitments gain legitimacy and displace old ones through endogenous mechanisms.
narrative_ontology:constraint_stakeholder(state_commitment_installation_mechanism__endogenous_climb_reading, historical_sociologists, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a mechanism for social systems to adapt and integrate new, more effective commitments by allowing them to prove their worth and gain widespread acceptance, thereby ensuring societal evolution and stability.
% TRANSFER_FUNCTION: Transfers legitimacy, resources, and social capital from older, less effective commitments to newer, more effective ones, driven by demonstrated utility and bottom-up adoption.
% ABSENT_VOICES: Those whose identities are inextricably linked to the old commitments and who lack the power or means to articulate a compelling alternative to the rising new commitment. Their resistance is often framed as traditionalism or irrationality by the advocates of the new.
% DISAPPEARANCE_RATIONALE: If this endogenous legitimation mechanism vanished, societies would either stagnate with outdated commitments or new commitments would only emerge through top-down imposition or violent revolution, leading to less stable, less legitimate, and less adaptable social orders.
% FOUNDING_PROBLEM: How do societies legitimately evolve their core commitments without constant top-down imposition or violent revolution, ensuring new commitments are genuinely superior and widely accepted?
% FOUNDING_PROBLEM_CORROBORATION: Historical analysis across various state formations and cultural shifts by independent scholars (e.g., Max Weber, Charles Tilly, historical institutionalists) corroborates the existence and importance of such endogenous legitimation processes, distinct from purely coercive or purely rational-choice accounts. This perspective is a live area of research in historical sociology.
narrative_ontology:disappearance_verdict(state_commitment_installation_mechanism__endogenous_climb_reading, world_rearranges).
narrative_ontology:founding_problem_status(state_commitment_installation_mechanism__endogenous_climb_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(state_commitment_installation_mechanism__endogenous_climb_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(state_commitment_installation_mechanism__endogenous_climb_reading, 'none', 1).
narrative_ontology:epsilon_provenance(state_commitment_installation_mechanism__endogenous_climb_reading, 0.4, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(state_commitment_installation_mechanism__endogenous_climb_reading_tests).
:- end_tests(state_commitment_installation_mechanism__endogenous_climb_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The claimed type is 'rope' because the mechanism, by its own lights, facilitates a genuine coordination problem: how to evolve societal commitments towards superior forms. Extractiveness (0.4) is moderate, reflecting the friction and costs of displacing old commitments, but the net benefit for adopters is positive. Suppression (0.3) is low because the new commitment gains legitimacy through demonstration, not primarily through coercion, though established orders may resist. Theater ratio (0.15) is low, indicating a genuine, functional process. Accessibility collapse (0.65) is moderate, as alternatives (old commitments) gradually lose viability due to the new commitment's demonstrated superiority. Resistance (0.55) is moderate, reflecting the 'resistance at apex' from established orders.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of fringe actors and advocates, this mechanism is a beneficial 'rope' that allows for progress and adaptation. For established institutions and adherents of old commitments, it can feel like a 'snare' or 'tangled_rope' as their power and identity are eroded, even if the new commitment is objectively superior. The engine will compute these divergent classifications based on the declared structural positions.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification prevents mislabeling a genuine, albeit often contentious, process of societal evolution as pure extraction or top-down imposition. By identifying it as a 'rope', it highlights the coordination function of allowing superior commitments to emerge, while acknowledging the costs borne by those whose commitments are displaced. This distinguishes it from scenarios where new commitments are simply forced upon a population without demonstrated superiority.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    objectivity_of_superiority,
    'Is the ''demonstrated superiority'' of new commitments genuinely objective and universally beneficial, or is it a socially constructed narrative that primarily benefits the advocates of the new commitment?',
    'Longitudinal historical analysis comparing the outcomes of ''endogenously climbed'' commitments with counterfactuals or ''exogenously imposed'' commitments, assessed against a broad range of societal welfare metrics.',
    'If superiority is largely subjective or self-serving, the extractiveness of the ''climb'' mechanism would be higher, potentially reclassifying it towards a ''tangled_rope'' or ''snare'' from the perspective of the displaced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(objectivity_of_superiority, conceptual, 'Ambiguity regarding the objective vs. subjective nature of ''demonstrated superiority''.').

omega_variable(
    fringe_vs_established_threshold,
    'What specific criteria define ''fringe'' versus ''established'' actors and institutions in different historical contexts, and how does this boundary shift over time?',
    'Detailed empirical case studies across diverse historical periods and societies, employing quantitative and qualitative methods to map institutional networks, resource flows, and discursive power.',
    'A clearer understanding of these thresholds would refine the power and exit options of stakeholders, potentially altering their computed directionality and per-seat classifications.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fringe_vs_established_threshold, empirical, 'Defining the dynamic boundary between fringe and established actors.').

omega_variable(
    suppression_by_new_commitment,
    'To what extent does the ''climb'' of a new commitment involve active suppression of alternatives by the *new* commitment itself, rather than merely outcompeting them through demonstrated superiority?',
    'Micro-historical analysis of specific instances of commitment installation, focusing on the actions taken by advocates of the new commitment against existing alternatives (e.g., legal prohibitions, resource denial, discursive delegitimization).',
    'If active suppression by the new commitment is significant, the ''suppression'' metric would be higher, pushing the classification towards a ''tangled_rope'' or ''snare'' for those experiencing the suppression.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_by_new_commitment, empirical, 'Distinguishing between competitive displacement and active suppression by the new commitment.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(state_commitment_installation_mechanism__endogenous_climb_reading, 1800, 2000).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stat_tr_t1800, state_commitment_installation_mechanism__endogenous_climb_reading, theater_ratio, 1800, 0.18).
narrative_ontology:measurement(stat_tr_t1840, state_commitment_installation_mechanism__endogenous_climb_reading, theater_ratio, 1840, 0.17).
narrative_ontology:measurement(stat_tr_t1880, state_commitment_installation_mechanism__endogenous_climb_reading, theater_ratio, 1880, 0.16).
narrative_ontology:measurement(stat_tr_t1920, state_commitment_installation_mechanism__endogenous_climb_reading, theater_ratio, 1920, 0.15).
narrative_ontology:measurement(stat_tr_t1960, state_commitment_installation_mechanism__endogenous_climb_reading, theater_ratio, 1960, 0.15).
narrative_ontology:measurement(stat_tr_t2000, state_commitment_installation_mechanism__endogenous_climb_reading, theater_ratio, 2000, 0.15).

% Extraction over time
narrative_ontology:measurement(stat_be_t1800, state_commitment_installation_mechanism__endogenous_climb_reading, base_extractiveness, 1800, 0.3).
narrative_ontology:measurement(stat_be_t1840, state_commitment_installation_mechanism__endogenous_climb_reading, base_extractiveness, 1840, 0.33).
narrative_ontology:measurement(stat_be_t1880, state_commitment_installation_mechanism__endogenous_climb_reading, base_extractiveness, 1880, 0.36).
narrative_ontology:measurement(stat_be_t1920, state_commitment_installation_mechanism__endogenous_climb_reading, base_extractiveness, 1920, 0.38).
narrative_ontology:measurement(stat_be_t1960, state_commitment_installation_mechanism__endogenous_climb_reading, base_extractiveness, 1960, 0.39).
narrative_ontology:measurement(stat_be_t2000, state_commitment_installation_mechanism__endogenous_climb_reading, base_extractiveness, 2000, 0.4).

% Suppression requirement over time
narrative_ontology:measurement(stat_su_t1800, state_commitment_installation_mechanism__endogenous_climb_reading, suppression_requirement, 1800, 0.2).
narrative_ontology:measurement(stat_su_t1840, state_commitment_installation_mechanism__endogenous_climb_reading, suppression_requirement, 1840, 0.23).
narrative_ontology:measurement(stat_su_t1880, state_commitment_installation_mechanism__endogenous_climb_reading, suppression_requirement, 1880, 0.26).
narrative_ontology:measurement(stat_su_t1920, state_commitment_installation_mechanism__endogenous_climb_reading, suppression_requirement, 1920, 0.28).
narrative_ontology:measurement(stat_su_t1960, state_commitment_installation_mechanism__endogenous_climb_reading, suppression_requirement, 1960, 0.29).
narrative_ontology:measurement(stat_su_t2000, state_commitment_installation_mechanism__endogenous_climb_reading, suppression_requirement, 2000, 0.3).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(state_commitment_installation_mechanism__endogenous_climb_reading, identity_coordination).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
