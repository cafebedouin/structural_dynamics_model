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
 *   constraint_id: qwerty_persistence_mechanism__beneficiary_extraction_reading
 *   human_readable: QWERTY Persistence via Beneficiary Extraction
 *   domain: economic_history/technology_studies/path_dependence
 *
 * SUMMARY:
 *   This constraint story analyzes QWERTY's persistence from the perspective
 *   that it was actively maintained by incumbent manufacturers and typing
 *   schools to protect their investments and market position, rather than
 *   being a natural outcome of market forces or inherent superiority. This
 *   reading emphasizes the role of identifiable beneficiaries and the
 *   suppression of alternatives. It is one reading of the
 *   'qwerty_persistence_mechanism' kernel.
 *
 * KEY AGENTS:
 *   - remington_union_typewriter: Primary beneficiary/agenda_setter (institutional/arbitrage) — actively maintained QWERTY's dominance.
 *   - incumbent_typing_schools: Secondary beneficiary (organized/constrained) — profited from QWERTY training.
 *   - alternative_keyboard_manufacturers: Primary victim (powerless/trapped) — suppressed from market access.
 *   - new_typing_schools: Secondary victim (powerless/constrained) — forced to teach QWERTY.
 *   - typists: Diffuse victim (moderate/identity_locked) — bore efficiency losses and high switching costs.
 *   - economic_historians: Analytical observer (analytical/analytical) — analyze the mechanisms of persistence.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(qwerty_persistence_mechanism__beneficiary_extraction_reading, 0.85).
domain_priors:suppression_score(qwerty_persistence_mechanism__beneficiary_extraction_reading, 0.75).
domain_priors:theater_ratio(qwerty_persistence_mechanism__beneficiary_extraction_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(qwerty_persistence_mechanism__beneficiary_extraction_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(qwerty_persistence_mechanism__beneficiary_extraction_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(qwerty_persistence_mechanism__beneficiary_extraction_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(qwerty_persistence_mechanism__beneficiary_extraction_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(qwerty_persistence_mechanism__beneficiary_extraction_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(qwerty_persistence_mechanism__beneficiary_extraction_reading, snare).
narrative_ontology:human_readable(qwerty_persistence_mechanism__beneficiary_extraction_reading, "QWERTY Persistence via Beneficiary Extraction").
narrative_ontology:topic_domain(qwerty_persistence_mechanism__beneficiary_extraction_reading, "economic_history/technology_studies/path_dependence").

domain_priors:requires_active_enforcement(qwerty_persistence_mechanism__beneficiary_extraction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(qwerty_persistence_mechanism__beneficiary_extraction_reading, 'c4c62c3e-6032-4f02-b14d-52a99d203399').
narrative_ontology:cs_kernel_codification('c4c62c3e-6032-4f02-b14d-52a99d203399', implicit).
narrative_ontology:cs_authority_grounding('c4c62c3e-6032-4f02-b14d-52a99d203399', extraction).
narrative_ontology:cs_interpretation_layer_present('c4c62c3e-6032-4f02-b14d-52a99d203399').
narrative_ontology:cs_reading_relation('c4c62c3e-6032-4f02-b14d-52a99d203399', qwerty_persistence_mechanism__lock_in_reading, coexists_with).
narrative_ontology:cs_reading_relation('c4c62c3e-6032-4f02-b14d-52a99d203399', qwerty_persistence_mechanism__naturalization_reading, forecloses).
narrative_ontology:cs_axiom('c4c62c3e-6032-4f02-b14d-52a99d203399', foundational, incumbent_profit_maximization_drives_standard_maintenance).
narrative_ontology:cs_axiom_status(incumbent_profit_maximization_drives_standard_maintenance, holdable).
narrative_ontology:cs_axiom_grounding('c4c62c3e-6032-4f02-b14d-52a99d203399', incumbent_profit_maximization_drives_standard_maintenance, empirically_contingent).
narrative_ontology:cs_axiom('c4c62c3e-6032-4f02-b14d-52a99d203399', foundational, technical_superiority_is_not_sufficient_for_market_adoption).
narrative_ontology:cs_axiom_status(technical_superiority_is_not_sufficient_for_market_adoption, holdable).
narrative_ontology:cs_axiom_grounding('c4c62c3e-6032-4f02-b14d-52a99d203399', technical_superiority_is_not_sufficient_for_market_adoption, empirically_contingent).
narrative_ontology:cs_reference_frame('c4c62c3e-6032-4f02-b14d-52a99d203399', competitive_market_meritocracy).
narrative_ontology:cs_drift_state('c4c62c3e-6032-4f02-b14d-52a99d203399', post_qwerty_entrenchment_era, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('c4c62c3e-6032-4f02-b14d-52a99d203399', '2024-07-30T12:00:00Z').
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

% The original manufacturers who established QWERTY as the standard. They actively promoted QWERTY, suppressed alternatives through marketing and distribution control, and benefited from the sunk costs of training and manufacturing infrastructure.
narrative_ontology:constraint_stakeholder(qwerty_persistence_mechanism__beneficiary_extraction_reading, remington_union_typewriter, agenda_setter,
    institutional, generational, arbitrage, national).

% Profited from teaching QWERTY, which became the de facto standard. Their curriculum and teacher training were invested in QWERTY, making them resistant to switching to alternative layouts.
narrative_ontology:constraint_stakeholder(qwerty_persistence_mechanism__beneficiary_extraction_reading, incumbent_typing_schools, beneficiary,
    organized, biographical, constrained, local).

% Developed technically superior keyboard layouts (e.g., Dvorak) but faced insurmountable market entry barriers due to QWERTY's entrenched position and active suppression by incumbents. They bore the costs of R&D without market access.
narrative_ontology:constraint_stakeholder(qwerty_persistence_mechanism__beneficiary_extraction_reading, alternative_keyboard_manufacturers, payer,
    powerless, biographical, trapped, national).

% Could not gain traction teaching alternative layouts due to lack of demand from employers and the installed base of QWERTY users. Forced to teach QWERTY to remain viable, despite potential efficiency gains from alternatives.
narrative_ontology:constraint_stakeholder(qwerty_persistence_mechanism__beneficiary_extraction_reading, new_typing_schools, payer,
    powerless, biographical, constrained, local).

% Invested time and effort in learning QWERTY. Faced high switching costs (re-training, lack of alternative keyboards) if they wanted to use a more efficient layout. Their 'identity' as a skilled typist was tied to QWERTY proficiency.
narrative_ontology:constraint_stakeholder(qwerty_persistence_mechanism__beneficiary_extraction_reading, typists, payer,
    moderate, biographical, identity_locked, local).

% Analyze the historical forces behind QWERTY's dominance, including the role of active suppression and beneficiary interests. Their analysis informs the understanding of path dependence and market failures.
narrative_ontology:constraint_stakeholder(qwerty_persistence_mechanism__beneficiary_extraction_reading, economic_historians, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Initially, QWERTY provided a common standard for typewriter operation, allowing typists to move between machines and facilitating mass production.
% TRANSFER_FUNCTION: Transferred market dominance and profits to incumbent manufacturers and typing schools by creating artificial switching costs and suppressing superior alternatives, extracting efficiency losses from typists and market share from innovators.
% ABSENT_VOICES: Alternative keyboard inventors and manufacturers, who were actively marginalized and whose superior designs were prevented from reaching the market. They would argue for open competition based on technical merit.
% DISAPPEARANCE_RATIONALE: If QWERTY's entrenched position and the mechanisms enforcing it vanished, there would be a rapid shift to more efficient keyboard layouts, driven by user demand and the re-emergence of suppressed alternatives. The entire keyboard manufacturing and typing education industries would reorganize.
% FOUNDING_PROBLEM: The initial problem was to create a functional and widely adopted keyboard layout for typewriters, balancing mechanical constraints with typing speed.
% FOUNDING_PROBLEM_CORROBORATION: Economic historians and technology studies scholars widely corroborate that the initial mechanical constraints were overcome decades ago, and that QWERTY's continued dominance is due to network effects and active incumbent protection, not inherent superiority or ongoing technical necessity. The problem it solved is long gone, but the solution persists due to other forces.
narrative_ontology:disappearance_verdict(qwerty_persistence_mechanism__beneficiary_extraction_reading, world_rearranges).
narrative_ontology:founding_problem_status(qwerty_persistence_mechanism__beneficiary_extraction_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(qwerty_persistence_mechanism__beneficiary_extraction_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(qwerty_persistence_mechanism__beneficiary_extraction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(qwerty_persistence_mechanism__beneficiary_extraction_reading, 0.85, 'gemini-2.5-flash', 'none', direct).

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
 *   Extractiveness is high because QWERTY's persistence imposed significant efficiency costs on typists and foreclosed superior alternatives, generating 'rents' for incumbents. Suppression is high due to active efforts by manufacturers to control distribution, marketing, and training, effectively blocking market entry for alternatives. Theater ratio is low because the maintenance of QWERTY was a genuine, albeit extractive, market strategy, not merely performative. Accessibility collapse is moderate because alternatives technically existed but were made practically inaccessible. Resistance is moderate, primarily from frustrated innovators and some academic critiques, but not widespread enough to challenge the entrenched system.
 *
 * PERSPECTIVAL GAP:
 *   The 'remington_union_typewriter' and 'incumbent_typing_schools' seats would experience this as a successful, legitimate market strategy, while 'alternative_keyboard_manufacturers' and 'typists' would experience it as an extractive snare. The engine's per-seat classification will reflect this divergence based on their declared roles and exit options.
 *
 * DIRECTIONALITY LOGIC:
 *   Remington/Union Typewriter and incumbent typing schools are beneficiaries (low d) as they directly profited from QWERTY's dominance. Alternative keyboard manufacturers, new typing schools, and typists are victims (high d) as they bore the costs of suppressed innovation and forced adherence to an inferior standard. Typists are 'identity_locked' due to their investment in QWERTY proficiency, making exit costly.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification prevents mislabeling QWERTY's persistence as a simple coordination problem (rope) or a natural outcome (mountain). By identifying active beneficiaries and victims, it highlights the extractive and suppressive mechanisms that sustained QWERTY long after its initial coordination function was fulfilled, indicating a clear case of mandatrophy where the original mandate (efficient typing standard) was superseded by rent-seeking.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    active_suppression_evidence,
    'To what extent was QWERTY''s dominance maintained by active suppression of alternatives versus passive network effects?',
    'Further historical research into marketing budgets, distribution agreements, and patent litigation specifically targeting alternative keyboard layouts.',
    'Strong evidence of active suppression would reinforce the ''snare'' classification and the beneficiary extraction reading. Weak evidence would lend more weight to the ''lock_in_reading'' (coordination failure).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(active_suppression_evidence, empirical, 'Distinguishing active incumbent protection from passive market dynamics.').

omega_variable(
    counterfactual_efficiency_gains,
    'What were the quantifiable efficiency gains lost by the widespread adoption of QWERTY over superior alternatives like Dvorak?',
    'Controlled ergonomic studies comparing typing speeds and error rates across layouts, and economic modeling of productivity losses at scale.',
    'Higher quantifiable losses would increase the measured ''extractiveness'' and strengthen the victim claims, further supporting the ''snare'' classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(counterfactual_efficiency_gains, empirical, 'Quantifying the economic cost of QWERTY''s persistence.').

omega_variable(
    reading_framing_choice,
    'Is the ''beneficiary_extraction_reading'' the most appropriate framing for QWERTY''s persistence, or do the ''lock_in_reading'' or ''naturalization_reading'' offer a more complete account?',
    'Comparative analysis of historical evidence against the core premises of each reading, assessing which best explains the observed market dynamics and agent behaviors.',
    'Adopting the ''lock_in_reading'' would shift the classification towards ''tangled_rope'' (coordination failure with diffuse costs), while the ''naturalization_reading'' would push towards ''rope'' or ''mountain'' (genuine coordination or natural fit), fundamentally altering the understanding of QWERTY''s nature.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_framing_choice, conceptual, 'Ambiguity in the primary mechanism driving QWERTY''s persistence.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(qwerty_persistence_mechanism__beneficiary_extraction_reading, 1874, 1980).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qwer_tr_t1874, qwerty_persistence_mechanism__beneficiary_extraction_reading, theater_ratio, 1874, 0.05).
narrative_ontology:measurement(qwer_tr_t1890, qwerty_persistence_mechanism__beneficiary_extraction_reading, theater_ratio, 1890, 0.1).
narrative_ontology:measurement(qwer_tr_t1910, qwerty_persistence_mechanism__beneficiary_extraction_reading, theater_ratio, 1910, 0.15).
narrative_ontology:measurement(qwer_tr_t1930, qwerty_persistence_mechanism__beneficiary_extraction_reading, theater_ratio, 1930, 0.2).
narrative_ontology:measurement(qwer_tr_t1950, qwerty_persistence_mechanism__beneficiary_extraction_reading, theater_ratio, 1950, 0.2).
narrative_ontology:measurement(qwer_tr_t1980, qwerty_persistence_mechanism__beneficiary_extraction_reading, theater_ratio, 1980, 0.2).

% Extraction over time
narrative_ontology:measurement(qwer_be_t1874, qwerty_persistence_mechanism__beneficiary_extraction_reading, base_extractiveness, 1874, 0.3).
narrative_ontology:measurement(qwer_be_t1890, qwerty_persistence_mechanism__beneficiary_extraction_reading, base_extractiveness, 1890, 0.5).
narrative_ontology:measurement(qwer_be_t1910, qwerty_persistence_mechanism__beneficiary_extraction_reading, base_extractiveness, 1910, 0.7).
narrative_ontology:measurement(qwer_be_t1930, qwerty_persistence_mechanism__beneficiary_extraction_reading, base_extractiveness, 1930, 0.8).
narrative_ontology:measurement(qwer_be_t1950, qwerty_persistence_mechanism__beneficiary_extraction_reading, base_extractiveness, 1950, 0.85).
narrative_ontology:measurement(qwer_be_t1980, qwerty_persistence_mechanism__beneficiary_extraction_reading, base_extractiveness, 1980, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(qwer_su_t1874, qwerty_persistence_mechanism__beneficiary_extraction_reading, suppression_requirement, 1874, 0.2).
narrative_ontology:measurement(qwer_su_t1890, qwerty_persistence_mechanism__beneficiary_extraction_reading, suppression_requirement, 1890, 0.45).
narrative_ontology:measurement(qwer_su_t1910, qwerty_persistence_mechanism__beneficiary_extraction_reading, suppression_requirement, 1910, 0.6).
narrative_ontology:measurement(qwer_su_t1930, qwerty_persistence_mechanism__beneficiary_extraction_reading, suppression_requirement, 1930, 0.7).
narrative_ontology:measurement(qwer_su_t1950, qwerty_persistence_mechanism__beneficiary_extraction_reading, suppression_requirement, 1950, 0.75).
narrative_ontology:measurement(qwer_su_t1980, qwerty_persistence_mechanism__beneficiary_extraction_reading, suppression_requirement, 1980, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(qwerty_persistence_mechanism__beneficiary_extraction_reading, information_standard).
narrative_ontology:affects_constraint(qwerty_persistence_mechanism__beneficiary_extraction_reading, qwerty_persistence_mechanism__lock_in_reading).
narrative_ontology:affects_constraint(qwerty_persistence_mechanism__beneficiary_extraction_reading, qwerty_persistence_mechanism__naturalization_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'qwerty_persistence_mechanism' kernel. This 'beneficiary_extraction_reading' emphasizes active incumbent maintenance and suppression, contrasting with the 'lock_in_reading' (path-dependent coordination failure) and the 'naturalization_reading' (inherent adequacy).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
