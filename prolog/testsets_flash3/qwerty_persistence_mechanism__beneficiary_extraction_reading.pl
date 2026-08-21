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
 *   domain: economic_history/technology_studies/path_dependence_theory
 *
 * SUMMARY:
 *   This constraint story analyzes the persistence of the QWERTY keyboard
 *   layout as a mechanism of beneficiary extraction. It argues that QWERTY's
 *   continued dominance, particularly after the initial mechanical rationale
 *   diminished, was actively maintained by incumbent manufacturers and typing
 *   schools to protect their investments and market position, rather than
 *   being a purely 'natural' outcome of market competition or an unavoidable
 *   'lock-in' from network effects. This reading emphasizes the agency of
 *   beneficiaries in suppressing alternatives and extracting value from the
 *   established standard.
 *
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
narrative_ontology:topic_domain(qwerty_persistence_mechanism__beneficiary_extraction_reading, "economic_history/technology_studies/path_dependence_theory").

domain_priors:requires_active_enforcement(qwerty_persistence_mechanism__beneficiary_extraction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(qwerty_persistence_mechanism__beneficiary_extraction_reading, '5027f0be-9b63-404c-a8e4-0091c3614e45').
narrative_ontology:cs_kernel_codification('5027f0be-9b63-404c-a8e4-0091c3614e45', implicit).
narrative_ontology:cs_authority_grounding('5027f0be-9b63-404c-a8e4-0091c3614e45', extraction).
narrative_ontology:cs_interpretation_layer_present('5027f0be-9b63-404c-a8e4-0091c3614e45').
narrative_ontology:cs_reading_relation('5027f0be-9b63-404c-a8e4-0091c3614e45', qwerty_persistence_mechanism__lock_in_reading, coexists_with).
narrative_ontology:cs_reading_relation('5027f0be-9b63-404c-a8e4-0091c3614e45', qwerty_persistence_mechanism__naturalization_reading, forecloses).
narrative_ontology:cs_axiom('5027f0be-9b63-404c-a8e4-0091c3614e45', foundational, active_suppression_of_alternatives).
narrative_ontology:cs_axiom_status(active_suppression_of_alternatives, holdable).
narrative_ontology:cs_axiom_grounding('5027f0be-9b63-404c-a8e4-0091c3614e45', active_suppression_of_alternatives, empirically_contingent).
narrative_ontology:cs_axiom('5027f0be-9b63-404c-a8e4-0091c3614e45', foundational, incumbent_market_power_leverage).
narrative_ontology:cs_axiom_status(incumbent_market_power_leverage, holdable).
narrative_ontology:cs_axiom_grounding('5027f0be-9b63-404c-a8e4-0091c3614e45', incumbent_market_power_leverage, empirically_contingent).
narrative_ontology:cs_reference_frame('5027f0be-9b63-404c-a8e4-0091c3614e45', uncontested_market_dominance).
narrative_ontology:cs_drift_state('5027f0be-9b63-404c-a8e4-0091c3614e45', post_dvorak_challenge_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('5027f0be-9b63-404c-a8e4-0091c3614e45', '2024-07-30T12:00:00Z').
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

% As the original patent holder and dominant manufacturer, Remington (and later Union Typewriter) actively promoted QWERTY, invested heavily in its ecosystem, and resisted adoption of alternatives to protect their market position and sunk costs in manufacturing and training.
narrative_ontology:constraint_stakeholder(qwerty_persistence_mechanism__beneficiary_extraction_reading, remington_union_typewriter, agenda_setter,
    institutional, generational, arbitrage, national).

% Benefited from the stability of QWERTY as the standard, as their training curricula and teacher expertise were tied to it. Adoption of new layouts would have required costly retraining and curriculum redesign, which they actively resisted.
narrative_ontology:constraint_stakeholder(qwerty_persistence_mechanism__beneficiary_extraction_reading, incumbent_typing_schools, beneficiary,
    organized, biographical, constrained, local).

% Attempted to introduce more efficient keyboard layouts (e.g., Dvorak) but faced insurmountable barriers due to QWERTY's entrenched market share, lack of typist training, and active resistance from incumbents. They bore the costs of R&D and marketing without gaining market access.
narrative_ontology:constraint_stakeholder(qwerty_persistence_mechanism__beneficiary_extraction_reading, alternative_keyboard_manufacturers, payer,
    powerless, immediate, trapped, national).

% Those attempting to teach alternative layouts struggled to attract students due to the lack of available keyboards and the perceived necessity of QWERTY for employment. They bore the cost of advocating for change without sufficient market demand.
narrative_ontology:constraint_stakeholder(qwerty_persistence_mechanism__beneficiary_extraction_reading, new_typing_schools, payer,
    moderate, biographical, constrained, local).

% Learned QWERTY due to its ubiquity and the need for employment, despite potential ergonomic disadvantages or slower typing speeds compared to alternatives. Their skill investment locked them into the layout, making switching costly and impractical.
narrative_ontology:constraint_stakeholder(qwerty_persistence_mechanism__beneficiary_extraction_reading, typists, payer,
    powerless, biographical, identity_locked, local).

% Analyze the historical forces behind QWERTY's dominance, examining evidence of strategic behavior by manufacturers and the role of network effects versus active suppression. Their analysis informs the understanding of path dependence.
narrative_ontology:constraint_stakeholder(qwerty_persistence_mechanism__beneficiary_extraction_reading, economic_historians, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Initially, QWERTY provided a standard layout for mechanical typewriters, facilitating training and interoperability across early machines. This function atrophied as alternatives emerged.
% TRANSFER_FUNCTION: Transferred market dominance and sustained profits to incumbent manufacturers and typing schools by suppressing competition from alternative, potentially superior, keyboard layouts. It also transferred the cost of suboptimal ergonomics and training to typists.
% ABSENT_VOICES: Designers and manufacturers of alternative, more efficient keyboard layouts (e.g., Dvorak) were effectively silenced by market exclusion and the active suppression of their innovations. Typists who experienced ergonomic issues or desired higher speeds also lacked a collective voice to demand change.
% DISAPPEARANCE_RATIONALE: If QWERTY vanished overnight, the entire keyboard manufacturing, training, and user ecosystem would be forced to adopt a new standard, likely leading to a more efficient and ergonomically sound layout. This would involve massive retraining and retooling, but ultimately a more optimal outcome for users and new manufacturers.
% FOUNDING_PROBLEM: The initial problem was to create a functional and robust keyboard layout for early mechanical typewriters that prevented key jamming and allowed for reasonable typing speeds.
% FOUNDING_PROBLEM_CORROBORATION: While QWERTY solved early mechanical problems, modern ergonomic and efficiency studies (from independent researchers and alternative keyboard advocates) demonstrate that the founding problem of optimal typing efficiency is no longer served by QWERTY, and indeed, it creates new problems. The incumbents' claims that QWERTY is 'good enough' are self-serving and not corroborated by external analysis.
narrative_ontology:disappearance_verdict(qwerty_persistence_mechanism__beneficiary_extraction_reading, world_rearranges).
narrative_ontology:founding_problem_status(qwerty_persistence_mechanism__beneficiary_extraction_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(qwerty_persistence_mechanism__beneficiary_extraction_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
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
 *   Extractiveness is high because the continued use of QWERTY, despite the existence of more efficient alternatives, imposed costs on typists (suboptimal performance, ergonomic issues) and suppressed innovation from alternative manufacturers. Suppression is also high, reflecting the active efforts by incumbents to discredit or block competing layouts through marketing, control over training, and lobbying. The theater ratio is low because the 'coordination' function of QWERTY became increasingly a cover for rent-seeking, but there was still a genuine, albeit suboptimal, standard being maintained.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the beneficiaries (incumbent manufacturers and typing schools), QWERTY's persistence is a natural outcome of its adequacy and their legitimate market success. From the perspective of victims (alternative manufacturers, typists), it is a clear case of market manipulation and extraction. This story aligns with the latter, emphasizing the active role of beneficiaries.
 *
 * DIRECTIONALITY LOGIC:
 *   Remington/Union Typewriter and incumbent typing schools are clear beneficiaries, actively shaping the market to their advantage. Alternative keyboard manufacturers and new typing schools are victims, unable to compete effectively. Typists are also victims, locked into a suboptimal standard due to training investments and the lack of viable alternatives. Economic historians act as observers, analyzing the structural dynamics.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's original mandate (preventing key jamming on mechanical typewriters) largely became obsolete with electric typewriters and computers. Its persistence is due to the active defense of accumulated investments and market power by beneficiaries, not a live coordination problem. This prevents mislabeling it as a 'rope' (pure coordination) or a 'piton' (inertial decay without active benefit capture).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    active_suppression_vs_network_effects,
    'To what extent was QWERTY''s persistence due to active suppression by beneficiaries versus passive network effects (e.g., typists learning QWERTY because others did)?',
    'Detailed historical analysis of marketing campaigns, lobbying efforts, and patent enforcement by incumbent manufacturers, compared against the independent growth of typist communities and training curricula.',
    'If active suppression was dominant, the constraint is more clearly a snare. If passive network effects were the primary driver, it leans more towards a tangled rope or even a degraded rope, where extraction is less about active malice and more about coordination failure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(active_suppression_vs_network_effects, empirical, 'Distinguishing active suppression from passive network effects in QWERTY''s persistence.').

omega_variable(
    suboptimal_vs_adequate,
    'Was QWERTY genuinely ''adequate'' for its time, or was it suboptimal even in its early stages, with its persistence always being a function of market power?',
    'Retrospective ergonomic and efficiency studies comparing QWERTY to contemporary (but suppressed) alternatives under historical conditions, accounting for mechanical limitations.',
    'If QWERTY was always suboptimal, the extraction began earlier and was more severe. If it was initially adequate but became suboptimal, the constraint''s classification might shift over time from a more benign form to a snare.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suboptimal_vs_adequate, empirical, 'Assessing QWERTY''s intrinsic adequacy versus its market-driven persistence.').

omega_variable(
    kernel_framing_ambiguity,
    'Is the ''qwerty_persistence_mechanism'' best framed as a beneficiary extraction mechanism, a path-dependent lock-in, or a natural outcome of competition?',
    'Further historical and economic research, particularly focusing on counterfactual scenarios and the agency of various actors. The choice of framing depends on the weight given to active suppression versus emergent properties.',
    'The classification of the constraint (snare, tangled_rope, or rope) depends heavily on this framing. This reading (beneficiary_extraction_reading) leads to a snare classification, while other readings would lead to different types.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_framing_ambiguity, conceptual, 'Ambiguity in framing the primary mechanism of QWERTY''s persistence.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(qwerty_persistence_mechanism__beneficiary_extraction_reading, 1874, 1950).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qwer_tr_t1874, qwerty_persistence_mechanism__beneficiary_extraction_reading, theater_ratio, 1874, 0.05).
narrative_ontology:measurement(qwer_tr_t1890, qwerty_persistence_mechanism__beneficiary_extraction_reading, theater_ratio, 1890, 0.1).
narrative_ontology:measurement(qwer_tr_t1910, qwerty_persistence_mechanism__beneficiary_extraction_reading, theater_ratio, 1910, 0.15).
narrative_ontology:measurement(qwer_tr_t1930, qwerty_persistence_mechanism__beneficiary_extraction_reading, theater_ratio, 1930, 0.18).
narrative_ontology:measurement(qwer_tr_t1950, qwerty_persistence_mechanism__beneficiary_extraction_reading, theater_ratio, 1950, 0.2).

% Extraction over time
narrative_ontology:measurement(qwer_be_t1874, qwerty_persistence_mechanism__beneficiary_extraction_reading, base_extractiveness, 1874, 0.3).
narrative_ontology:measurement(qwer_be_t1890, qwerty_persistence_mechanism__beneficiary_extraction_reading, base_extractiveness, 1890, 0.55).
narrative_ontology:measurement(qwer_be_t1910, qwerty_persistence_mechanism__beneficiary_extraction_reading, base_extractiveness, 1910, 0.7).
narrative_ontology:measurement(qwer_be_t1930, qwerty_persistence_mechanism__beneficiary_extraction_reading, base_extractiveness, 1930, 0.8).
narrative_ontology:measurement(qwer_be_t1950, qwerty_persistence_mechanism__beneficiary_extraction_reading, base_extractiveness, 1950, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(qwer_su_t1874, qwerty_persistence_mechanism__beneficiary_extraction_reading, suppression_requirement, 1874, 0.2).
narrative_ontology:measurement(qwer_su_t1890, qwerty_persistence_mechanism__beneficiary_extraction_reading, suppression_requirement, 1890, 0.45).
narrative_ontology:measurement(qwer_su_t1910, qwerty_persistence_mechanism__beneficiary_extraction_reading, suppression_requirement, 1910, 0.6).
narrative_ontology:measurement(qwer_su_t1930, qwerty_persistence_mechanism__beneficiary_extraction_reading, suppression_requirement, 1930, 0.7).
narrative_ontology:measurement(qwer_su_t1950, qwerty_persistence_mechanism__beneficiary_extraction_reading, suppression_requirement, 1950, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(qwerty_persistence_mechanism__beneficiary_extraction_reading, information_standard).
narrative_ontology:affects_constraint(qwerty_persistence_mechanism__beneficiary_extraction_reading, qwerty_persistence_mechanism__lock_in_reading).
narrative_ontology:affects_constraint(qwerty_persistence_mechanism__beneficiary_extraction_reading, qwerty_persistence_mechanism__naturalization_reading).

% DUAL FORMULATION NOTE:
% This story is one reading of the 'qwerty_persistence_mechanism' kernel. It focuses on active beneficiary extraction, contrasting with the 'lock_in_reading' (path dependence) and 'naturalization_reading' (inherent adequacy). All three are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
