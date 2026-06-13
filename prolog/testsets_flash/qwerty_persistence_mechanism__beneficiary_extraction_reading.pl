% ============================================================================
% CONSTRAINT STORY: qwerty_persistence_mechanism__beneficiary_extraction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_qwerty_persistence_mechanism__beneficiary_extraction_reading, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
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
 *   constraint_id: qwerty_persistence_mechanism__beneficiary_extraction_reading
 *   human_readable: QWERTY Persistence (Beneficiary Extraction Reading)
 *   domain: economic_history/technology_studies/path_dependence_theory
 *
 * SUMMARY:
 *   This constraint story represents the 'beneficiary extraction' reading of
 *   QWERTY's persistence. It argues that QWERTY's continued dominance,
 *   despite the emergence of technically superior alternatives, was not a
 *   natural outcome or a mere coordination failure, but rather the result of
 *   active maintenance and suppression by incumbent manufacturers (Remington,
 *   Union Typewriter) and associated industries (typing schools) to protect
 *   their sunk investments and market positions. This reading emphasizes the
 *   identifiable beneficiaries and victims of the QWERTY standard.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(qwerty_persistence_mechanism__beneficiary_extraction_reading, 0.85).
domain_priors:suppression_score(qwerty_persistence_mechanism__beneficiary_extraction_reading, 0.9).
domain_priors:theater_ratio(qwerty_persistence_mechanism__beneficiary_extraction_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(qwerty_persistence_mechanism__beneficiary_extraction_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(qwerty_persistence_mechanism__beneficiary_extraction_reading, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(qwerty_persistence_mechanism__beneficiary_extraction_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(qwerty_persistence_mechanism__beneficiary_extraction_reading, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(qwerty_persistence_mechanism__beneficiary_extraction_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(qwerty_persistence_mechanism__beneficiary_extraction_reading, snare).
narrative_ontology:human_readable(qwerty_persistence_mechanism__beneficiary_extraction_reading, "QWERTY Persistence (Beneficiary Extraction Reading)").
narrative_ontology:topic_domain(qwerty_persistence_mechanism__beneficiary_extraction_reading, "economic_history/technology_studies/path_dependence_theory").

domain_priors:requires_active_enforcement(qwerty_persistence_mechanism__beneficiary_extraction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(qwerty_persistence_mechanism__beneficiary_extraction_reading, 'da0070ee-a77e-413e-9002-af9cb2b8ff85').
narrative_ontology:cs_kernel_codification('da0070ee-a77e-413e-9002-af9cb2b8ff85', implicit).
narrative_ontology:cs_authority_grounding('da0070ee-a77e-413e-9002-af9cb2b8ff85', extraction).
narrative_ontology:cs_reading_relation('da0070ee-a77e-413e-9002-af9cb2b8ff85', qwerty_persistence_mechanism__lock_in_reading, coexists_with).
narrative_ontology:cs_reading_relation('da0070ee-a77e-413e-9002-af9cb2b8ff85', qwerty_persistence_mechanism__naturalization_reading, forecloses).
narrative_ontology:cs_axiom('da0070ee-a77e-413e-9002-af9cb2b8ff85', foundational, market_outcomes_reflect_power_dynamics).
narrative_ontology:cs_axiom_status(market_outcomes_reflect_power_dynamics, holdable).
narrative_ontology:cs_axiom_grounding('da0070ee-a77e-413e-9002-af9cb2b8ff85', market_outcomes_reflect_power_dynamics, empirically_contingent).
narrative_ontology:cs_axiom('da0070ee-a77e-413e-9002-af9cb2b8ff85', foundational, incumbents_actively_suppress_alternatives).
narrative_ontology:cs_axiom_status(incumbents_actively_suppress_alternatives, holdable).
narrative_ontology:cs_axiom_grounding('da0070ee-a77e-413e-9002-af9cb2b8ff85', incumbents_actively_suppress_alternatives, empirically_contingent).
narrative_ontology:cs_reference_frame('da0070ee-a77e-413e-9002-af9cb2b8ff85', competitive_market_efficiency).
narrative_ontology:cs_drift_state('da0070ee-a77e-413e-9002-af9cb2b8ff85', post_dvorak_era, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('da0070ee-a77e-413e-9002-af9cb2b8ff85', '').
narrative_ontology:cs_kernel_id(qwerty_persistence_mechanism__beneficiary_extraction_reading, qwerty_persistence_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(qwerty_persistence_mechanism__beneficiary_extraction_reading, remington_typewriter_company).
narrative_ontology:constraint_beneficiary(qwerty_persistence_mechanism__beneficiary_extraction_reading, union_typewriter_company).
narrative_ontology:constraint_beneficiary(qwerty_persistence_mechanism__beneficiary_extraction_reading, incumbent_typing_schools).
narrative_ontology:constraint_victim(qwerty_persistence_mechanism__beneficiary_extraction_reading, alternative_keyboard_inventors).
narrative_ontology:constraint_victim(qwerty_persistence_mechanism__beneficiary_extraction_reading, typing_students).
narrative_ontology:constraint_victim(qwerty_persistence_mechanism__beneficiary_extraction_reading, general_public).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The original manufacturer of QWERTY typewriters, heavily invested in the layout and its associated training. Actively promoted QWERTY and resisted adoption of alternatives to protect its market dominance and sunk costs in manufacturing and training infrastructure.
narrative_ontology:constraint_stakeholder(qwerty_persistence_mechanism__beneficiary_extraction_reading, remington_typewriter_company, agenda_setter,
    institutional, generational, arbitrage, national).

% A consortium of typewriter manufacturers that consolidated market power and collectively enforced QWERTY as the industry standard, leveraging their combined influence to suppress competing layouts and maintain high switching costs for users and other manufacturers.
narrative_ontology:constraint_stakeholder(qwerty_persistence_mechanism__beneficiary_extraction_reading, union_typewriter_company, agenda_setter,
    institutional, generational, arbitrage, national).

% Institutions that built their curricula and business models around teaching QWERTY. They benefited from the layout's dominance, as it ensured a steady demand for their specific training, and had little incentive to adopt or promote alternative layouts.
narrative_ontology:constraint_stakeholder(qwerty_persistence_mechanism__beneficiary_extraction_reading, incumbent_typing_schools, beneficiary,
    organized, biographical, constrained, local).

% Developed technically superior keyboard layouts (e.g., Dvorak) but faced insurmountable barriers to market entry due to the entrenched QWERTY standard, active suppression by incumbents, and high retraining costs for users. Their innovations were effectively stifled.
narrative_ontology:constraint_stakeholder(qwerty_persistence_mechanism__beneficiary_extraction_reading, alternative_keyboard_inventors, payer,
    powerless, biographical, trapped, national).

% Learned QWERTY as the only viable option for employment and general use. They bore the cost of learning a potentially inefficient layout and faced high switching costs if they desired to use an alternative, effectively locked into the dominant standard by career path dependence.
narrative_ontology:constraint_stakeholder(qwerty_persistence_mechanism__beneficiary_extraction_reading, typing_students, payer,
    moderate, immediate, identity_locked, local).

% Used QWERTY keyboards on typewriters and later computers, implicitly bearing the costs of a suboptimal design in terms of typing speed and ergonomic strain, without being aware of or having access to superior alternatives due to market suppression.
narrative_ontology:constraint_stakeholder(qwerty_persistence_mechanism__beneficiary_extraction_reading, general_public, payer,
    powerless, generational, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Established a universal standard for typewriter keyboards, allowing typists to move between machines and facilitating mass production and training on a single layout.
% TRANSFER_FUNCTION: Transferred economic benefits (market share, profits, training revenue) from the general public and alternative innovators to incumbent manufacturers and typing schools, by maintaining an artificially entrenched standard.
% ABSENT_VOICES: The voices of alternative keyboard inventors and ergonomic researchers were actively suppressed or marginalized by the dominant industry players. They would have advocated for technically superior and more efficient layouts.
% DISAPPEARANCE_RATIONALE: If QWERTY vanished overnight, the market for keyboard layouts would immediately open to competition based on efficiency and ergonomics. Manufacturers would rapidly innovate, typing schools would adapt, and users would likely adopt more efficient layouts, leading to a significant reorganization of the human-computer interface landscape.
% FOUNDING_PROBLEM: The initial problem was to create a functional and reliable keyboard layout for early mechanical typewriters, balancing mechanical constraints with typing speed and preventing key jams.
% FOUNDING_PROBLEM_CORROBORATION: While the initial mechanical constraints are long gone, incumbent manufacturers and some historical accounts still claim QWERTY's adequacy. However, extensive ergonomic research and the existence of demonstrably superior alternative layouts (e.g., Dvorak) corroborate that the original problem is dead, and the persistence is due to other factors, as attested by independent researchers and alternative inventors.
narrative_ontology:disappearance_verdict(qwerty_persistence_mechanism__beneficiary_extraction_reading, world_rearranges).
narrative_ontology:founding_problem_status(qwerty_persistence_mechanism__beneficiary_extraction_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(qwerty_persistence_mechanism__beneficiary_extraction_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(qwerty_persistence_mechanism__beneficiary_extraction_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(qwerty_persistence_mechanism__beneficiary_extraction_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(qwerty_persistence_mechanism__beneficiary_extraction_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(qwerty_persistence_mechanism__beneficiary_extraction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.85) because the QWERTY standard imposed significant costs on users (suboptimal typing efficiency, ergonomic strain) and stifled innovation from alternative keyboard inventors, while generating substantial profits for incumbents. Suppression is very high (0.9) due to active efforts by manufacturers to block competing layouts through market power, advertising, and control over training. Theater ratio is low (0.1) because the maintenance of QWERTY was a genuine, albeit extractive, market strategy, not primarily performative. Accessibility collapse is high (0.75) because alternatives were effectively made inaccessible, and resistance is moderate (0.4) as some inventors and researchers did actively challenge QWERTY, though largely unsuccessfully.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the incumbent manufacturers, QWERTY's persistence was a rational business strategy, a 'rope' of market coordination. From the perspective of alternative inventors and the public, it was a 'snare' of enforced extraction. This reading explicitly adopts the latter, emphasizing the active suppression and rent-seeking behavior.
 *
 * DIRECTIONALITY LOGIC:
 *   Incumbent manufacturers (Remington, Union Typewriter) and typing schools are clear beneficiaries, actively shaping and profiting from the QWERTY standard. Alternative keyboard inventors, typing students, and the general public are victims, bearing the costs of an entrenched, suboptimal system. The directionality for beneficiaries is low (subsidized by the constraint), and for victims, it is high (targeted by the constraint).
 *
 * MANDATROPHY ANALYSIS:
 *   This classification as a Snare directly addresses the mandatrophy question by asserting that QWERTY's original mandate (efficient mechanical typing) became obsolete, but the constraint persisted not through inertia, but through active, extractive maintenance. It prevents mislabeling it as a 'rope' (pure coordination) or 'piton' (inertial decay) by highlighting the concentrated benefits and active suppression.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    active_suppression_evidence,
    'To what extent was the suppression of alternative keyboard layouts a result of active, coordinated efforts by incumbents versus passive market inertia or consumer preference?',
    'Historical archival research into corporate strategies, lobbying efforts, and advertising campaigns of typewriter manufacturers, as well as analysis of patent suppression and market entry barriers for alternative designs.',
    'Strong evidence of active suppression reinforces the ''snare'' classification and the beneficiary extraction reading. Weak evidence would lend more credence to the ''lock-in'' or ''naturalization'' readings, potentially shifting classification towards ''tangled_rope'' or ''rope''.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(active_suppression_evidence, empirical, 'Distinguishing active suppression from passive market dynamics.').

omega_variable(
    extraction_vs_coordination_cost,
    'What proportion of the costs borne by users and alternative inventors can be attributed to genuine coordination benefits (e.g., universal compatibility) versus pure rent extraction by incumbents?',
    'Economic modeling comparing the efficiency gains of a universal standard against the documented inefficiencies of QWERTY and the suppressed potential of alternatives, quantifying the ''excess'' cost.',
    'A higher proportion of pure extraction strengthens the ''snare'' classification. If a significant portion of costs is deemed necessary for coordination, it might lean towards a ''tangled_rope'' classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_vs_coordination_cost, empirical, 'Quantifying the balance between coordination benefits and extractive costs.').

omega_variable(
    reading_framing_impact,
    'How does framing QWERTY''s persistence as ''beneficiary extraction'' (this reading) versus ''lock-in'' or ''naturalization'' (sibling readings) alter the perceived agency of actors and the potential for intervention?',
    'Comparative policy analysis: examine historical and contemporary interventions (e.g., Dvorak promotion, antitrust actions) and their success under different underlying assumptions about QWERTY''s nature.',
    'If this reading is adopted, it implies that active intervention (e.g., antitrust, regulatory mandates for open standards) is necessary to dismantle the snare. The ''lock-in'' reading might suggest less direct intervention, focusing on education or gradual transitions, while ''naturalization'' implies no intervention is needed.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_framing_impact, conceptual, 'Impact of different readings on policy and agency.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(qwerty_persistence_mechanism__beneficiary_extraction_reading, 1874, 1980).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qwer_tr_t1874, qwerty_persistence_mechanism__beneficiary_extraction_reading, theater_ratio, 1874, 0.05).
narrative_ontology:measurement_basis(qwer_tr_t1874, observed).
narrative_ontology:measurement(qwer_tr_t1890, qwerty_persistence_mechanism__beneficiary_extraction_reading, theater_ratio, 1890, 0.08).
narrative_ontology:measurement_basis(qwer_tr_t1890, observed).
narrative_ontology:measurement(qwer_tr_t1910, qwerty_persistence_mechanism__beneficiary_extraction_reading, theater_ratio, 1910, 0.1).
narrative_ontology:measurement_basis(qwer_tr_t1910, observed).
narrative_ontology:measurement(qwer_tr_t1930, qwerty_persistence_mechanism__beneficiary_extraction_reading, theater_ratio, 1930, 0.12).
narrative_ontology:measurement_basis(qwer_tr_t1930, observed).
narrative_ontology:measurement(qwer_tr_t1950, qwerty_persistence_mechanism__beneficiary_extraction_reading, theater_ratio, 1950, 0.1).
narrative_ontology:measurement_basis(qwer_tr_t1950, observed).
narrative_ontology:measurement(qwer_tr_t1980, qwerty_persistence_mechanism__beneficiary_extraction_reading, theater_ratio, 1980, 0.08).
narrative_ontology:measurement_basis(qwer_tr_t1980, observed).

% Extraction over time
narrative_ontology:measurement(qwer_be_t1874, qwerty_persistence_mechanism__beneficiary_extraction_reading, base_extractiveness, 1874, 0.4).
narrative_ontology:measurement_basis(qwer_be_t1874, observed).
narrative_ontology:measurement(qwer_be_t1890, qwerty_persistence_mechanism__beneficiary_extraction_reading, base_extractiveness, 1890, 0.6).
narrative_ontology:measurement_basis(qwer_be_t1890, observed).
narrative_ontology:measurement(qwer_be_t1910, qwerty_persistence_mechanism__beneficiary_extraction_reading, base_extractiveness, 1910, 0.75).
narrative_ontology:measurement_basis(qwer_be_t1910, observed).
narrative_ontology:measurement(qwer_be_t1930, qwerty_persistence_mechanism__beneficiary_extraction_reading, base_extractiveness, 1930, 0.85).
narrative_ontology:measurement_basis(qwer_be_t1930, observed).
narrative_ontology:measurement(qwer_be_t1950, qwerty_persistence_mechanism__beneficiary_extraction_reading, base_extractiveness, 1950, 0.88).
narrative_ontology:measurement_basis(qwer_be_t1950, observed).
narrative_ontology:measurement(qwer_be_t1980, qwerty_persistence_mechanism__beneficiary_extraction_reading, base_extractiveness, 1980, 0.85).
narrative_ontology:measurement_basis(qwer_be_t1980, observed).

% Suppression requirement over time
narrative_ontology:measurement(qwer_su_t1874, qwerty_persistence_mechanism__beneficiary_extraction_reading, suppression_requirement, 1874, 0.3).
narrative_ontology:measurement_basis(qwer_su_t1874, observed).
narrative_ontology:measurement(qwer_su_t1890, qwerty_persistence_mechanism__beneficiary_extraction_reading, suppression_requirement, 1890, 0.5).
narrative_ontology:measurement_basis(qwer_su_t1890, observed).
narrative_ontology:measurement(qwer_su_t1910, qwerty_persistence_mechanism__beneficiary_extraction_reading, suppression_requirement, 1910, 0.7).
narrative_ontology:measurement_basis(qwer_su_t1910, observed).
narrative_ontology:measurement(qwer_su_t1930, qwerty_persistence_mechanism__beneficiary_extraction_reading, suppression_requirement, 1930, 0.85).
narrative_ontology:measurement_basis(qwer_su_t1930, observed).
narrative_ontology:measurement(qwer_su_t1950, qwerty_persistence_mechanism__beneficiary_extraction_reading, suppression_requirement, 1950, 0.9).
narrative_ontology:measurement_basis(qwer_su_t1950, observed).
narrative_ontology:measurement(qwer_su_t1980, qwerty_persistence_mechanism__beneficiary_extraction_reading, suppression_requirement, 1980, 0.88).
narrative_ontology:measurement_basis(qwer_su_t1980, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(qwerty_persistence_mechanism__beneficiary_extraction_reading, information_standard).
narrative_ontology:affects_constraint(qwerty_persistence_mechanism__beneficiary_extraction_reading, qwerty_persistence_mechanism__lock_in_reading).
narrative_ontology:affects_constraint(qwerty_persistence_mechanism__beneficiary_extraction_reading, qwerty_persistence_mechanism__naturalization_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'qwerty_persistence_mechanism' kernel. This 'beneficiary extraction' reading emphasizes active incumbent maintenance and suppression, contrasting with the 'lock-in' (path dependence) and 'naturalization' (inherent adequacy) readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
