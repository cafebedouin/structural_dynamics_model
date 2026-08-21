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
 *   human_readable: QWERTY Persistence via Beneficiary Extraction
 *   domain: economic_history/technology_studies/path_dependence
 *
 * SUMMARY:
 *   This constraint is the 'beneficiary_extraction_reading' of the
 *   'qwerty_persistence_mechanism' kernel. This reading posits that QWERTY's
 *   persistence is primarily due to active maintenance by identifiable
 *   beneficiaries (manufacturers, typing schools) who strategically protected
 *   their training investments and market position, rather than QWERTY's
 *   inherent superiority or passive lock-in. This involved active suppression
 *   of alternatives and extraction via artificial switching costs. Sibling
 *   readings include 'naturalization_reading' (QWERTY was genuinely adequate
 *   or became so) and 'lock_in_reading' (QWERTY persists through
 *   path-dependent coordination failure despite technical inferiority).
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
narrative_ontology:human_readable(qwerty_persistence_mechanism__beneficiary_extraction_reading, "QWERTY Persistence via Beneficiary Extraction").
narrative_ontology:topic_domain(qwerty_persistence_mechanism__beneficiary_extraction_reading, "economic_history/technology_studies/path_dependence").

domain_priors:requires_active_enforcement(qwerty_persistence_mechanism__beneficiary_extraction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(qwerty_persistence_mechanism__beneficiary_extraction_reading, 'e249b209-cd9d-4cc3-998c-42d53e8829ce').
narrative_ontology:cs_kernel_codification('e249b209-cd9d-4cc3-998c-42d53e8829ce', formalized).
narrative_ontology:cs_authority_grounding('e249b209-cd9d-4cc3-998c-42d53e8829ce', extraction).
narrative_ontology:cs_reading_relation('e249b209-cd9d-4cc3-998c-42d53e8829ce', qwerty_persistence_mechanism__naturalization_reading, forecloses).
narrative_ontology:cs_reading_relation('e249b209-cd9d-4cc3-998c-42d53e8829ce', qwerty_persistence_mechanism__lock_in_reading, coexists_with).
narrative_ontology:cs_axiom('e249b209-cd9d-4cc3-998c-42d53e8829ce', foundational, qwerty_maintained_for_incumbent_profit).
narrative_ontology:cs_axiom_status(qwerty_maintained_for_incumbent_profit, holdable).
narrative_ontology:cs_axiom_grounding('e249b209-cd9d-4cc3-998c-42d53e8829ce', qwerty_maintained_for_incumbent_profit, empirically_contingent).
narrative_ontology:cs_axiom('e249b209-cd9d-4cc3-998c-42d53e8829ce', foundational, alternative_layouts_actively_suppressed).
narrative_ontology:cs_axiom_status(alternative_layouts_actively_suppressed, holdable).
narrative_ontology:cs_axiom_grounding('e249b209-cd9d-4cc3-998c-42d53e8829ce', alternative_layouts_actively_suppressed, empirically_contingent).
narrative_ontology:cs_reference_frame('e249b209-cd9d-4cc3-998c-42d53e8829ce', uncontested_market_dominance).
narrative_ontology:cs_drift_state('e249b209-cd9d-4cc3-998c-42d53e8829ce', post_digital_era_ergonomic_awareness, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('e249b209-cd9d-4cc3-998c-42d53e8829ce', '').
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

% As the original patent holder and dominant manufacturer, Remington (and later Union Typewriter) actively promoted QWERTY, invested heavily in its ecosystem, and strategically suppressed alternatives to protect their market position and training investments. They directly benefited from QWERTY's entrenched status.
narrative_ontology:constraint_stakeholder(qwerty_persistence_mechanism__beneficiary_extraction_reading, remington_union_typewriter, agenda_setter,
    institutional, generational, arbitrage, global).

% Benefited significantly from QWERTY's dominance, as their established training materials, methods, and instructor expertise were standardized. They had little incentive to adopt or promote alternative layouts, which would have required costly retooling and retraining.
narrative_ontology:constraint_stakeholder(qwerty_persistence_mechanism__beneficiary_extraction_reading, incumbent_typing_schools, beneficiary,
    organized, biographical, constrained, national).

% Developed technically superior keyboard layouts (e.g., Dvorak) but faced insurmountable barriers to market entry and adoption due to QWERTY's entrenched position, active resistance from incumbents, and high switching costs for users. Their innovations were effectively suppressed.
narrative_ontology:constraint_stakeholder(qwerty_persistence_mechanism__beneficiary_extraction_reading, alternative_keyboard_manufacturers, payer,
    powerful, biographical, trapped, global).

% Struggled to establish themselves by promoting alternative, more efficient layouts. They faced a lack of market demand for non-QWERTY training, limited availability of alternative keyboards, and the overwhelming inertia of the QWERTY standard.
narrative_ontology:constraint_stakeholder(qwerty_persistence_mechanism__beneficiary_extraction_reading, new_typing_schools, payer,
    moderate, biographical, constrained, local).

% Were (and largely still are) forced to learn and use a suboptimal keyboard layout, incurring cognitive and physical costs (e.g., slower typing speeds, increased strain) with virtually no real choice due to the ubiquity of QWERTY devices and training.
narrative_ontology:constraint_stakeholder(qwerty_persistence_mechanism__beneficiary_extraction_reading, typists, payer,
    powerless, biographical, constrained, universal).

% Analyze the historical forces behind QWERTY's persistence, including market dynamics, strategic actions by incumbents, and the role of path dependence. They provide an analytical perspective on the constraint's origins and mechanisms.
narrative_ontology:constraint_stakeholder(qwerty_persistence_mechanism__beneficiary_extraction_reading, economic_historians, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provided a standardized keyboard layout that facilitated mass production of typewriters, simplified training, and enabled interoperability across different machines and users.
% TRANSFER_FUNCTION: Transfers market dominance and sustained profits from potentially superior alternative layouts to QWERTY manufacturers and associated training industries. It also transfers cognitive and physical costs (suboptimal efficiency, ergonomic strain) to typists.
% ABSENT_VOICES: Developers of alternative layouts (e.g., Dvorak), early ergonomic researchers, and typists advocating for more efficient designs were marginalized or ignored by the dominant industry players and the market forces they shaped. Their voices were actively suppressed or drowned out by the entrenched QWERTY ecosystem.
% DISAPPEARANCE_RATIONALE: If QWERTY vanished overnight, the entire keyboard manufacturing, training, and user base would need to re-standardize. This would lead to a period of significant disruption, followed by rapid innovation and adoption of more ergonomic and efficient keyboard designs, fundamentally reorganizing the human-computer interface landscape.
% FOUNDING_PROBLEM: The initial problem was to create a robust, fast, and jam-resistant mechanical typewriter layout for mass production, preventing common key lever clashes in early designs.
% FOUNDING_PROBLEM_CORROBORATION: Economic historians and technology studies scholars (outside the benefiting parties) corroborate that the original mechanical problem of key jamming is long solved with digital keyboards. They argue that QWERTY's persistence beyond this initial problem is due to other factors, including strategic incumbent behavior and artificial switching costs, rather than its inherent superiority or the continued relevance of its founding problem.
narrative_ontology:disappearance_verdict(qwerty_persistence_mechanism__beneficiary_extraction_reading, world_rearranges).
narrative_ontology:founding_problem_status(qwerty_persistence_mechanism__beneficiary_extraction_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(qwerty_persistence_mechanism__beneficiary_extraction_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
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
 *   The high extractiveness (0.82) reflects the ongoing cost to typists and the foregone benefits of superior alternatives, which were actively prevented from gaining market share. Suppression (0.90) is very high, indicating the extensive efforts by incumbents to maintain QWERTY's dominance through marketing, training standardization, and resistance to alternative designs. The low theater ratio (0.10) suggests that the 'coordination' function of QWERTY became largely a cover for rent extraction; the maintenance activities were functional for preserving market position, not for genuinely improving user experience or solving new coordination problems. Accessibility collapse is high because alternatives were effectively shut out of the market.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of QWERTY incumbents, the persistence was a natural outcome of market competition and their legitimate investments. From the perspective of alternative developers and typists, it was a clear case of market manipulation and enforced suboptimality. The engine's classification as a Snare reflects the latter, emphasizing the active, coercive elements of its persistence.
 *
 * DIRECTIONALITY LOGIC:
 *   Remington/Union Typewriter and incumbent typing schools are clear beneficiaries and agenda-setters, actively shaping the market to their advantage. Alternative keyboard manufacturers, new typing schools, and typists are the primary victims, bearing the costs of suppressed innovation and suboptimal design. The directionality for beneficiaries is low (subsidized), while for victims it is high (targeted for extraction).
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    active_suppression_vs_passive_lock_in,
    'To what extent was QWERTY''s persistence due to active, strategic suppression by incumbents (beneficiary_extraction_reading) versus passive, path-dependent coordination failure (lock_in_reading)?',
    'Detailed historical analysis of corporate archives, patent litigation records, and marketing strategies of incumbent manufacturers, compared against the diffusion patterns of alternative technologies.',
    'If active suppression is dominant, the constraint is more clearly a Snare. If passive lock-in is the primary driver, it leans more towards a Tangled Rope or even a Rope (if the coordination benefits outweigh the costs without active coercion).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(active_suppression_vs_passive_lock_in, empirical, 'Distinguishing between active market manipulation and passive market inertia in QWERTY''s persistence.').

omega_variable(
    qwerty_adequacy_vs_extraction,
    'Was QWERTY''s persistence due to its genuine adequacy or sufficient ''good enough'' quality (naturalization_reading), or was its continued dominance primarily a mechanism for beneficiary extraction despite superior alternatives (beneficiary_extraction_reading)?',
    'Comparative ergonomic and efficiency studies of QWERTY versus its historical alternatives (e.g., Dvorak), controlling for learning effects and user familiarity, combined with economic analysis of switching costs.',
    'If QWERTY is found to be genuinely adequate, the extractiveness metric might be lower, potentially shifting the classification towards a Rope. If it''s demonstrably suboptimal, the Snare classification is reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(qwerty_adequacy_vs_extraction, empirical, 'Assessing QWERTY''s intrinsic quality versus its persistence as an extractive mechanism.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(qwerty_persistence_mechanism__beneficiary_extraction_reading, 1873, 2000).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qwer_tr_t1873, qwerty_persistence_mechanism__beneficiary_extraction_reading, theater_ratio, 1873, 0.15).
narrative_ontology:measurement(qwer_tr_t1890, qwerty_persistence_mechanism__beneficiary_extraction_reading, theater_ratio, 1890, 0.12).
narrative_ontology:measurement(qwer_tr_t1920, qwerty_persistence_mechanism__beneficiary_extraction_reading, theater_ratio, 1920, 0.1).
narrative_ontology:measurement(qwer_tr_t1950, qwerty_persistence_mechanism__beneficiary_extraction_reading, theater_ratio, 1950, 0.08).
narrative_ontology:measurement(qwer_tr_t1975, qwerty_persistence_mechanism__beneficiary_extraction_reading, theater_ratio, 1975, 0.09).
narrative_ontology:measurement(qwer_tr_t2000, qwerty_persistence_mechanism__beneficiary_extraction_reading, theater_ratio, 2000, 0.1).

% Extraction over time
narrative_ontology:measurement(qwer_be_t1873, qwerty_persistence_mechanism__beneficiary_extraction_reading, base_extractiveness, 1873, 0.6).
narrative_ontology:measurement(qwer_be_t1890, qwerty_persistence_mechanism__beneficiary_extraction_reading, base_extractiveness, 1890, 0.7).
narrative_ontology:measurement(qwer_be_t1920, qwerty_persistence_mechanism__beneficiary_extraction_reading, base_extractiveness, 1920, 0.78).
narrative_ontology:measurement(qwer_be_t1950, qwerty_persistence_mechanism__beneficiary_extraction_reading, base_extractiveness, 1950, 0.85).
narrative_ontology:measurement(qwer_be_t1975, qwerty_persistence_mechanism__beneficiary_extraction_reading, base_extractiveness, 1975, 0.88).
narrative_ontology:measurement(qwer_be_t2000, qwerty_persistence_mechanism__beneficiary_extraction_reading, base_extractiveness, 2000, 0.82).

% Suppression requirement over time
narrative_ontology:measurement(qwer_su_t1873, qwerty_persistence_mechanism__beneficiary_extraction_reading, suppression_requirement, 1873, 0.7).
narrative_ontology:measurement(qwer_su_t1890, qwerty_persistence_mechanism__beneficiary_extraction_reading, suppression_requirement, 1890, 0.78).
narrative_ontology:measurement(qwer_su_t1920, qwerty_persistence_mechanism__beneficiary_extraction_reading, suppression_requirement, 1920, 0.85).
narrative_ontology:measurement(qwer_su_t1950, qwerty_persistence_mechanism__beneficiary_extraction_reading, suppression_requirement, 1950, 0.92).
narrative_ontology:measurement(qwer_su_t1975, qwerty_persistence_mechanism__beneficiary_extraction_reading, suppression_requirement, 1975, 0.95).
narrative_ontology:measurement(qwer_su_t2000, qwerty_persistence_mechanism__beneficiary_extraction_reading, suppression_requirement, 2000, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(qwerty_persistence_mechanism__beneficiary_extraction_reading, information_standard).

% DUAL FORMULATION NOTE:
% This story is one reading of the 'qwerty_persistence_mechanism' kernel, focusing on active beneficiary extraction. It is linked to sibling readings 'naturalization_reading' and 'lock_in_reading' which offer alternative explanations for QWERTY's persistence.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
