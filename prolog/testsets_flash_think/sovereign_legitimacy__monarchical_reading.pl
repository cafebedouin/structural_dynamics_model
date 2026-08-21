% ============================================================================
% CONSTRAINT STORY: sovereign_legitimacy__monarchical_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sovereign_legitimacy__monarchical_reading, []).

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
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   constraint_id: sovereign_legitimacy__monarchical_reading
 *   human_readable: Monarchical Legitimacy (Inherited Right Reading)
 *   domain: political_philosophy/constitutional_theory/legitimacy_studies
 *
 * SUMMARY:
 *   This constraint represents the 'monarchical reading' of sovereign
 *   legitimacy, where authority is understood to flow downward from a
 *   sovereign through inherited right, grounded in divine sanction,
 *   tradition, and bloodline continuity. It describes a system where a
 *   hereditary ruling class and aristocratic hierarchy are primary
 *   beneficiaries, while subjects are largely excluded from political
 *   participation and bear the costs of maintaining the system. The
 *   constraint's persistence relies on high suppression of alternative
 *   legitimacy claims and active enforcement of the established order.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sovereign_legitimacy__monarchical_reading, 0.7).
domain_priors:suppression_score(sovereign_legitimacy__monarchical_reading, 0.9).
domain_priors:theater_ratio(sovereign_legitimacy__monarchical_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sovereign_legitimacy__monarchical_reading, extractiveness, 0.7).
narrative_ontology:constraint_metric(sovereign_legitimacy__monarchical_reading, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(sovereign_legitimacy__monarchical_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(sovereign_legitimacy__monarchical_reading, accessibility_collapse, 0.85).
narrative_ontology:constraint_metric(sovereign_legitimacy__monarchical_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sovereign_legitimacy__monarchical_reading, snare).
narrative_ontology:human_readable(sovereign_legitimacy__monarchical_reading, "Monarchical Legitimacy (Inherited Right Reading)").
narrative_ontology:topic_domain(sovereign_legitimacy__monarchical_reading, "political_philosophy/constitutional_theory/legitimacy_studies").

domain_priors:requires_active_enforcement(sovereign_legitimacy__monarchical_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sovereign_legitimacy__monarchical_reading, '731d636b-a4d5-486c-86cb-f15369a133e8').
narrative_ontology:cs_kernel_codification('731d636b-a4d5-486c-86cb-f15369a133e8', formalized).
narrative_ontology:cs_authority_grounding('731d636b-a4d5-486c-86cb-f15369a133e8', lineage).
narrative_ontology:cs_interpretation_layer_present('731d636b-a4d5-486c-86cb-f15369a133e8').
narrative_ontology:cs_reading_relation('731d636b-a4d5-486c-86cb-f15369a133e8', sovereign_legitimacy__republican_reading, forecloses).
narrative_ontology:cs_reading_relation('731d636b-a4d5-486c-86cb-f15369a133e8', sovereign_legitimacy__constitutional_hybrid_reading, influences).
narrative_ontology:cs_axiom('731d636b-a4d5-486c-86cb-f15369a133e8', foundational, divine_right_of_kings).
narrative_ontology:cs_axiom_status(divine_right_of_kings, holdable).
narrative_ontology:cs_axiom_grounding('731d636b-a4d5-486c-86cb-f15369a133e8', divine_right_of_kings, theological).
narrative_ontology:cs_axiom('731d636b-a4d5-486c-86cb-f15369a133e8', foundational, bloodline_continuity_as_legitimacy).
narrative_ontology:cs_axiom_status(bloodline_continuity_as_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('731d636b-a4d5-486c-86cb-f15369a133e8', bloodline_continuity_as_legitimacy, conventional).
narrative_ontology:cs_reference_frame('731d636b-a4d5-486c-86cb-f15369a133e8', absolute_monarchical_order).
narrative_ontology:cs_drift_state('731d636b-a4d5-486c-86cb-f15369a133e8', enlightenment_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('731d636b-a4d5-486c-86cb-f15369a133e8', '').
narrative_ontology:cs_kernel_id(sovereign_legitimacy__monarchical_reading, sovereign_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sovereign_legitimacy__monarchical_reading, hereditary_ruling_class).
narrative_ontology:constraint_beneficiary(sovereign_legitimacy__monarchical_reading, aristocratic_hierarchy).
narrative_ontology:constraint_victim(sovereign_legitimacy__monarchical_reading, subjects_excluded_from_participation).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds ultimate authority by birthright, claiming divine sanction and historical continuity. Benefits directly from the system's stability and the extraction of resources and loyalty from subjects. Exit means renouncing their identity and power.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__monarchical_reading, hereditary_ruling_class, agenda_setter,
    institutional, generational, identity_locked, national).

% Receives privileges, land, and positions of influence by virtue of their loyalty to the sovereign and their own inherited status. They enforce the sovereign's will locally and benefit from the social order it maintains. Exit would mean loss of status and wealth.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__monarchical_reading, aristocratic_hierarchy, beneficiary,
    powerful, generational, constrained, national).

% Are governed without direct consent, providing taxes, labor, and military service. Their participation in governance is denied, and their welfare is dependent on the sovereign's benevolence. Exit options are limited to rebellion, emigration, or passive resistance, all with high costs.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__monarchical_reading, subjects_excluded_from_participation, payer,
    powerless, biographical, trapped, national).

% Challenge the legitimacy of inherited authority, advocating for popular sovereignty and representative government. They are often suppressed, exiled, or imprisoned for their views, operating outside the recognized political structure.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__monarchical_reading, republican_advocates, excluded,
    organized, generational, constrained, national).

% Analyze the historical development and theoretical underpinnings of monarchical legitimacy, comparing it with alternative forms of governance. They are not directly subject to the constraint but provide critical analysis of its operation and effects.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__monarchical_reading, constitutional_scholars, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(sovereign_legitimacy__monarchical_reading, hereditary_ruling_class).
narrative_ontology:fixing_cost_class(sovereign_legitimacy__monarchical_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a clear, stable line of succession and a centralized authority structure, preventing civil war over leadership and unifying disparate territories under a single crown.
% TRANSFER_FUNCTION: Transfers ultimate political authority, wealth, and social status from the general populace to the hereditary ruling class and aristocracy, in exchange for perceived order, stability, and national identity.
% ABSENT_VOICES: Advocates for popular sovereignty, social contract theorists, and those who believe in elected representation are systematically excluded from the formal mechanisms of power and legitimacy validation. Their voices are suppressed or dismissed as seditious.
% DISAPPEARANCE_RATIONALE: If inherited monarchical legitimacy vanished overnight, it would lead to immediate power vacuums, succession crises, and widespread political instability. The entire social and political order, built on this foundational principle, would collapse and require fundamental reorganization.
% FOUNDING_PROBLEM: To prevent constant internecine warfare and power struggles by establishing an unambiguous, divinely sanctioned, and traditionally accepted method for determining leadership and maintaining social order.
% FOUNDING_PROBLEM_CORROBORATION: Proponents of monarchical rule (e.g., royal historians, traditionalist clergy) attest that the problem of order and stable succession remains live. Critics (e.g., political philosophers, republican movements) argue that the problem has been superseded by modern governance models or that the 'solution' itself became a source of extraction and oppression, making the founding problem effectively dead or solvable by other means.
narrative_ontology:disappearance_verdict(sovereign_legitimacy__monarchical_reading, world_rearranges).
narrative_ontology:founding_problem_status(sovereign_legitimacy__monarchical_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(sovereign_legitimacy__monarchical_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(sovereign_legitimacy__monarchical_reading, 'none', 1).
narrative_ontology:epsilon_provenance(sovereign_legitimacy__monarchical_reading, 0.7, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sovereign_legitimacy__monarchical_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(sovereign_legitimacy__monarchical_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(sovereign_legitimacy__monarchical_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The `extractiveness` starts high and slightly decreases over the interval as Enlightenment ideas challenge absolute rule, but remains substantial due to the inherent nature of inherited privilege. `Suppression_requirement` is very high and increases, reflecting the growing need for active coercion to maintain the system against rising republican and constitutionalist movements. `Theater_ratio` increases significantly, as the actual power of monarchs diminishes in many contexts, but the rituals and symbols of divine right and tradition are maintained for performative legitimacy.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the hereditary ruling class, this constraint is a natural and divinely ordained order, ensuring stability and prosperity. From the perspective of the subjects, it is an extractive system that denies their agency and imposes costs without consent. The engine's per-seat classification will highlight this divergence, showing the constraint as a Snare for subjects and a Beneficiary-aligned structure for the ruling class.
 *
 * DIRECTIONALITY LOGIC:
 *   The hereditary ruling class and aristocratic hierarchy are clear beneficiaries, receiving power, wealth, and status. Subjects are the primary victims, bearing the costs of governance without participation. Republican advocates are excluded, actively suppressed for challenging the foundational premise. Constitutional scholars act as analytical observers, assessing the system's operation.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    divine_sanction_empirical_status,
    'Is the divine sanction grounding monarchical legitimacy an empirically verifiable claim, or a theological/conventional one?',
    'Philosophical analysis of the nature of divine right claims, and historical examination of their social function versus their asserted truth-value.',
    'If purely theological/conventional, the constraint''s legitimacy rests on faith or social agreement, making it vulnerable to shifts in belief or convention. If it were empirically testable (which it is not), its falsification would collapse the entire legitimacy claim.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(divine_sanction_empirical_status, conceptual, 'The epistemic grounding of divine sanction in monarchical legitimacy.').

omega_variable(
    succession_stability_vs_extraction,
    'Does the stability provided by clear succession rules genuinely outweigh the extraction imposed on subjects, or is the stability merely a cover for rent-seeking?',
    'Comparative historical analysis of states with and without clear succession rules, examining rates of civil conflict versus levels of economic extraction and political participation.',
    'If stability is demonstrably achievable with less extraction or more participation, the monarchical reading''s justification for high extraction is weakened. If the stability is uniquely tied to this structure, it strengthens the coordination aspect, though not necessarily the extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(succession_stability_vs_extraction, empirical, 'The trade-off between monarchical stability and extraction.').

omega_variable(
    alternative_legitimacy_suppression_mechanism,
    'Is the suppression of republican alternatives primarily structural (e.g., lack of institutions for popular participation) or internalized (e.g., subjects'' belief in the divine right of kings)?',
    'Analysis of historical shifts in public opinion and the effectiveness of propaganda versus direct coercive force in maintaining monarchical rule. Post-revolutionary societal changes in belief systems.',
    'If internalized suppression is dominant, the constraint''s effective suppression is higher and more resilient to external challenges. If structural, removing the external barriers would more quickly lead to alternative legitimacy claims emerging.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_legitimacy_suppression_mechanism, empirical, 'Structural vs. internalized suppression mechanism for alternative legitimacy claims.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sovereign_legitimacy__monarchical_reading, 1600, 1900).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sove_tr_t1600, sovereign_legitimacy__monarchical_reading, theater_ratio, 1600, 0.1).
narrative_ontology:measurement(sove_tr_t1650, sovereign_legitimacy__monarchical_reading, theater_ratio, 1650, 0.15).
narrative_ontology:measurement(sove_tr_t1700, sovereign_legitimacy__monarchical_reading, theater_ratio, 1700, 0.2).
narrative_ontology:measurement(sove_tr_t1750, sovereign_legitimacy__monarchical_reading, theater_ratio, 1750, 0.28).
narrative_ontology:measurement(sove_tr_t1800, sovereign_legitimacy__monarchical_reading, theater_ratio, 1800, 0.35).
narrative_ontology:measurement(sove_tr_t1900, sovereign_legitimacy__monarchical_reading, theater_ratio, 1900, 0.4).

% Extraction over time
narrative_ontology:measurement(sove_be_t1600, sovereign_legitimacy__monarchical_reading, base_extractiveness, 1600, 0.85).
narrative_ontology:measurement(sove_be_t1650, sovereign_legitimacy__monarchical_reading, base_extractiveness, 1650, 0.82).
narrative_ontology:measurement(sove_be_t1700, sovereign_legitimacy__monarchical_reading, base_extractiveness, 1700, 0.78).
narrative_ontology:measurement(sove_be_t1750, sovereign_legitimacy__monarchical_reading, base_extractiveness, 1750, 0.75).
narrative_ontology:measurement(sove_be_t1800, sovereign_legitimacy__monarchical_reading, base_extractiveness, 1800, 0.72).
narrative_ontology:measurement(sove_be_t1900, sovereign_legitimacy__monarchical_reading, base_extractiveness, 1900, 0.7).

% Suppression requirement over time
narrative_ontology:measurement(sove_su_t1600, sovereign_legitimacy__monarchical_reading, suppression_requirement, 1600, 0.8).
narrative_ontology:measurement(sove_su_t1650, sovereign_legitimacy__monarchical_reading, suppression_requirement, 1650, 0.83).
narrative_ontology:measurement(sove_su_t1700, sovereign_legitimacy__monarchical_reading, suppression_requirement, 1700, 0.85).
narrative_ontology:measurement(sove_su_t1750, sovereign_legitimacy__monarchical_reading, suppression_requirement, 1750, 0.87).
narrative_ontology:measurement(sove_su_t1800, sovereign_legitimacy__monarchical_reading, suppression_requirement, 1800, 0.88).
narrative_ontology:measurement(sove_su_t1900, sovereign_legitimacy__monarchical_reading, suppression_requirement, 1900, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sovereign_legitimacy__monarchical_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(sovereign_legitimacy__monarchical_reading, feudal_land_tenure).
narrative_ontology:affects_constraint(sovereign_legitimacy__monarchical_reading, state_church_doctrine).
narrative_ontology:affects_constraint(sovereign_legitimacy__monarchical_reading, national_identity_formation).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'sovereign_legitimacy' kernel. It describes the monarchical interpretation, which emphasizes inherited right and divine sanction, in contrast to republican or constitutional hybrid readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
