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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: qwerty_persistence_mechanism__beneficiary_extraction_reading
 *   human_readable: QWERTY Persistence: Beneficiary Extraction Reading
 *   domain: economic_history/technology_studies/path_dependence_theory
 *
 * SUMMARY:
 *   This constraint story presents the 'beneficiary extraction' reading of
 *   QWERTY's persistence. It argues that QWERTY's continued dominance,
 *   despite the emergence of technically superior alternatives, was not a
 *   natural outcome of market competition or inherent merit, but rather the
 *   result of active maintenance by incumbent manufacturers and typing
 *   schools to protect their investments and market position. This involved
 *   suppressing alternatives and leveraging existing infrastructure to create
 *   artificial switching costs, leading to substantial extraction from users
 *   and stifled innovation.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(qwerty_persistence_mechanism__beneficiary_extraction_reading, 0.82).
domain_priors:suppression_score(qwerty_persistence_mechanism__beneficiary_extraction_reading, 0.9).
domain_priors:theater_ratio(qwerty_persistence_mechanism__beneficiary_extraction_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(qwerty_persistence_mechanism__beneficiary_extraction_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(qwerty_persistence_mechanism__beneficiary_extraction_reading, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(qwerty_persistence_mechanism__beneficiary_extraction_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(qwerty_persistence_mechanism__beneficiary_extraction_reading, accessibility_collapse, 0.85).
narrative_ontology:constraint_metric(qwerty_persistence_mechanism__beneficiary_extraction_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(qwerty_persistence_mechanism__beneficiary_extraction_reading, snare).
narrative_ontology:human_readable(qwerty_persistence_mechanism__beneficiary_extraction_reading, "QWERTY Persistence: Beneficiary Extraction Reading").
narrative_ontology:topic_domain(qwerty_persistence_mechanism__beneficiary_extraction_reading, "economic_history/technology_studies/path_dependence_theory").

domain_priors:requires_active_enforcement(qwerty_persistence_mechanism__beneficiary_extraction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(qwerty_persistence_mechanism__beneficiary_extraction_reading, '7c7afc88-96a3-49a1-9b5e-f4a2d88b7a8a').
narrative_ontology:cs_kernel_codification('7c7afc88-96a3-49a1-9b5e-f4a2d88b7a8a', formalized).
narrative_ontology:cs_authority_grounding('7c7afc88-96a3-49a1-9b5e-f4a2d88b7a8a', extraction).
narrative_ontology:cs_reading_relation('7c7afc88-96a3-49a1-9b5e-f4a2d88b7a8a', qwerty_persistence_mechanism__lock_in_reading, coexists_with).
narrative_ontology:cs_reading_relation('7c7afc88-96a3-49a1-9b5e-f4a2d88b7a8a', qwerty_persistence_mechanism__naturalization_reading, forecloses).
narrative_ontology:cs_axiom('7c7afc88-96a3-49a1-9b5e-f4a2d88b7a8a', foundational, qwerty_maintained_for_profit).
narrative_ontology:cs_axiom_status(qwerty_maintained_for_profit, holdable).
narrative_ontology:cs_axiom_grounding('7c7afc88-96a3-49a1-9b5e-f4a2d88b7a8a', qwerty_maintained_for_profit, empirically_contingent).
narrative_ontology:cs_axiom('7c7afc88-96a3-49a1-9b5e-f4a2d88b7a8a', foundational, alternatives_actively_suppressed).
narrative_ontology:cs_axiom_status(alternatives_actively_suppressed, holdable).
narrative_ontology:cs_axiom_grounding('7c7afc88-96a3-49a1-9b5e-f4a2d88b7a8a', alternatives_actively_suppressed, empirically_contingent).
narrative_ontology:cs_reference_frame('7c7afc88-96a3-49a1-9b5e-f4a2d88b7a8a', qwerty_as_market_standard).
narrative_ontology:cs_drift_state('7c7afc88-96a3-49a1-9b5e-f4a2d88b7a8a', post_dvorak_challenge_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('7c7afc88-96a3-49a1-9b5e-f4a2d88b7a8a', '').
narrative_ontology:cs_kernel_id(qwerty_persistence_mechanism__beneficiary_extraction_reading, qwerty_persistence_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(qwerty_persistence_mechanism__beneficiary_extraction_reading, remington_union_typewriter).
narrative_ontology:constraint_beneficiary(qwerty_persistence_mechanism__beneficiary_extraction_reading, incumbent_typing_schools).
narrative_ontology:constraint_victim(qwerty_persistence_mechanism__beneficiary_extraction_reading, alternative_keyboard_manufacturers).
narrative_ontology:constraint_victim(qwerty_persistence_mechanism__beneficiary_extraction_reading, new_typing_schools).
narrative_ontology:constraint_victim(qwerty_persistence_mechanism__beneficiary_extraction_reading, typists).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% As the primary patent holder and early market leader, Remington (and later Union Typewriter) actively promoted QWERTY, invested heavily in its ecosystem, and resisted the adoption of alternative, potentially superior, keyboard layouts to protect their market position and training investments.
narrative_ontology:constraint_stakeholder(qwerty_persistence_mechanism__beneficiary_extraction_reading, remington_union_typewriter, agenda_setter,
    institutional, generational, arbitrage, global).

% Benefited significantly from QWERTY's dominance, as their established training curricula, materials, and instructor expertise were standardized around the layout. They had little incentive to switch to alternatives and often actively discouraged them.
narrative_ontology:constraint_stakeholder(qwerty_persistence_mechanism__beneficiary_extraction_reading, incumbent_typing_schools, beneficiary,
    organized, biographical, constrained, national).

% Companies that developed and attempted to market technically superior keyboard layouts (e.g., Dvorak) faced immense barriers to entry, active resistance from incumbents, and a market locked into QWERTY, leading to suppressed innovation and lost market opportunities.
narrative_ontology:constraint_stakeholder(qwerty_persistence_mechanism__beneficiary_extraction_reading, alternative_keyboard_manufacturers, payer,
    powerful, biographical, trapped, global).

% Schools attempting to teach alternative, more efficient layouts struggled to attract students due to the ubiquity of QWERTY keyboards and the lack of demand for non-QWERTY skills in the job market, effectively limiting their growth and reach.
narrative_ontology:constraint_stakeholder(qwerty_persistence_mechanism__beneficiary_extraction_reading, new_typing_schools, payer,
    moderate, biographical, constrained, local).

% Learned QWERTY as the default and often only available keyboard layout, bearing the long-term cost of a suboptimal design in terms of efficiency and ergonomic strain. Switching costs were high due to muscle memory and the lack of alternative hardware.
narrative_ontology:constraint_stakeholder(qwerty_persistence_mechanism__beneficiary_extraction_reading, typists, payer,
    powerless, biographical, identity_locked, universal).

% Analyze the historical, economic, and sociological factors contributing to QWERTY's persistence, often highlighting the role of incumbent interests and active suppression of alternatives.
narrative_ontology:constraint_stakeholder(qwerty_persistence_mechanism__beneficiary_extraction_reading, economic_historians, observer,
    analytical, generational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Initially provided a standardized keyboard layout for mass production and widespread adoption of typewriters, facilitating training and interoperability across devices.
% TRANSFER_FUNCTION: Transfers market dominance, profits, and the benefits of established training infrastructure to QWERTY manufacturers and incumbent typing schools, at the expense of alternative innovators and typists who bear efficiency costs.
% ABSENT_VOICES: Alternative keyboard designers (e.g., August Dvorak), early ergonomic researchers, and consumers who would have benefited from more efficient and ergonomic layouts were systematically marginalized or ignored in the market's evolution.
% DISAPPEARANCE_RATIONALE: If the active mechanisms maintaining QWERTY's dominance (incumbent protection, suppression of alternatives) vanished, the market would likely rapidly adopt more efficient and ergonomic layouts, leading to a significant reorganization of keyboard manufacturing, typing education, and user habits.
% FOUNDING_PROBLEM: The initial problem was to create a robust, mass-producible, and standardized typewriter layout that prevented key jamming and facilitated rapid typing for the era's mechanical limitations.
% FOUNDING_PROBLEM_CORROBORATION: Independent ergonomic studies and historical analyses by economic historians (e.g., Paul David's work on path dependence) corroborate that the initial technical problem was solved, but QWERTY's persistence became driven by incumbent interests and active suppression, not ongoing functional superiority. This contrasts with the claims of QWERTY's original beneficiaries.
narrative_ontology:disappearance_verdict(qwerty_persistence_mechanism__beneficiary_extraction_reading, world_rearranges).
narrative_ontology:founding_problem_status(qwerty_persistence_mechanism__beneficiary_extraction_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(qwerty_persistence_mechanism__beneficiary_extraction_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(qwerty_persistence_mechanism__beneficiary_extraction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(qwerty_persistence_mechanism__beneficiary_extraction_reading, 0.82, 'gemini-2.5-flash', 'none', direct).

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
 *   The high extractiveness (0.82) reflects the ongoing cost to typists of using a suboptimal layout and the lost opportunities for alternative manufacturers. Suppression (0.90) is severe due to the active efforts by incumbents to block alternatives through marketing, training standardization, and control over distribution channels. Theater ratio is low (0.10) because the maintenance activities were genuinely functional in preserving the incumbents' market share, even if the underlying coordination function had atrophied. Accessibility collapse is high (0.85) because alternatives, though technically viable, were made practically inaccessible to most users.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the QWERTY incumbents, their actions were legitimate business practices protecting valuable investments and providing a stable standard. From the perspective of alternative innovators and economic historians, these same actions constituted active suppression and rent-seeking, creating a market snare. The engine's classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Remington/Union Typewriter and incumbent typing schools are clear beneficiaries, actively shaping the market to their advantage. Alternative manufacturers and new typing schools are direct targets, facing barriers to entry and suppressed innovation. Typists are also targets, bearing the costs of an inefficient standard due to high switching costs and identity lock-in (muscle memory, job market expectations).
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    active_suppression_vs_passive_lock_in,
    'To what extent was QWERTY''s persistence due to active suppression by incumbents versus passive network effects and user inertia (lock-in)?',
    'Detailed historical analysis of corporate archives, marketing strategies, and lobbying efforts by QWERTY incumbents, contrasted with the independent growth trajectories of alternative technologies in unconstrained markets.',
    'If active suppression is dominant, the constraint is more clearly a Snare. If passive lock-in is the primary driver, it leans towards a Tangled Rope or even a Piton (if the active maintenance costs become too high for beneficiaries).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(active_suppression_vs_passive_lock_in, empirical, 'Distinguishing active incumbent intervention from emergent path dependence.').

omega_variable(
    technical_superiority_of_alternatives,
    'What was the true, empirically verifiable technical superiority (e.g., speed, ergonomics) of alternative keyboard layouts (e.g., Dvorak) over QWERTY, independent of market adoption challenges?',
    'Controlled, blinded scientific studies comparing typing efficiency and ergonomic impact across different layouts, controlling for training effects and user bias.',
    'If alternatives were demonstrably superior, it strengthens the argument that QWERTY''s persistence was extractive. If superiority was marginal or contested, it weakens the extraction claim and lends more credence to naturalization or lock-in readings.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(technical_superiority_of_alternatives, empirical, 'Empirical validation of alternative keyboard layouts'' performance claims.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(qwerty_persistence_mechanism__beneficiary_extraction_reading, 1870, 1980).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qwer_tr_t1870, qwerty_persistence_mechanism__beneficiary_extraction_reading, theater_ratio, 1870, 0.15).
narrative_ontology:measurement(qwer_tr_t1890, qwerty_persistence_mechanism__beneficiary_extraction_reading, theater_ratio, 1890, 0.12).
narrative_ontology:measurement(qwer_tr_t1910, qwerty_persistence_mechanism__beneficiary_extraction_reading, theater_ratio, 1910, 0.1).
narrative_ontology:measurement(qwer_tr_t1930, qwerty_persistence_mechanism__beneficiary_extraction_reading, theater_ratio, 1930, 0.08).
narrative_ontology:measurement(qwer_tr_t1950, qwerty_persistence_mechanism__beneficiary_extraction_reading, theater_ratio, 1950, 0.09).
narrative_ontology:measurement(qwer_tr_t1980, qwerty_persistence_mechanism__beneficiary_extraction_reading, theater_ratio, 1980, 0.1).

% Extraction over time
narrative_ontology:measurement(qwer_be_t1870, qwerty_persistence_mechanism__beneficiary_extraction_reading, base_extractiveness, 1870, 0.6).
narrative_ontology:measurement(qwer_be_t1890, qwerty_persistence_mechanism__beneficiary_extraction_reading, base_extractiveness, 1890, 0.7).
narrative_ontology:measurement(qwer_be_t1910, qwerty_persistence_mechanism__beneficiary_extraction_reading, base_extractiveness, 1910, 0.78).
narrative_ontology:measurement(qwer_be_t1930, qwerty_persistence_mechanism__beneficiary_extraction_reading, base_extractiveness, 1930, 0.85).
narrative_ontology:measurement(qwer_be_t1950, qwerty_persistence_mechanism__beneficiary_extraction_reading, base_extractiveness, 1950, 0.88).
narrative_ontology:measurement(qwer_be_t1980, qwerty_persistence_mechanism__beneficiary_extraction_reading, base_extractiveness, 1980, 0.82).

% Suppression requirement over time
narrative_ontology:measurement(qwer_su_t1870, qwerty_persistence_mechanism__beneficiary_extraction_reading, suppression_requirement, 1870, 0.7).
narrative_ontology:measurement(qwer_su_t1890, qwerty_persistence_mechanism__beneficiary_extraction_reading, suppression_requirement, 1890, 0.78).
narrative_ontology:measurement(qwer_su_t1910, qwerty_persistence_mechanism__beneficiary_extraction_reading, suppression_requirement, 1910, 0.85).
narrative_ontology:measurement(qwer_su_t1930, qwerty_persistence_mechanism__beneficiary_extraction_reading, suppression_requirement, 1930, 0.9).
narrative_ontology:measurement(qwer_su_t1950, qwerty_persistence_mechanism__beneficiary_extraction_reading, suppression_requirement, 1950, 0.92).
narrative_ontology:measurement(qwer_su_t1980, qwerty_persistence_mechanism__beneficiary_extraction_reading, suppression_requirement, 1980, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(qwerty_persistence_mechanism__beneficiary_extraction_reading, information_standard).
narrative_ontology:affects_constraint(qwerty_persistence_mechanism__beneficiary_extraction_reading, qwerty_persistence_mechanism__lock_in_reading).
narrative_ontology:affects_constraint(qwerty_persistence_mechanism__beneficiary_extraction_reading, qwerty_persistence_mechanism__naturalization_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'QWERTY persistence mechanism' kernel. This reading emphasizes active beneficiary extraction, while sibling readings focus on path-dependent lock-in or naturalized adequacy.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
