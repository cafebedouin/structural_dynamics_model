% ============================================================================
% CONSTRAINT STORY: marriage_commitment_reversal__practice_doctrine_gap
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_marriage_commitment_reversal__practice_doctrine_gap, []).

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
 *   constraint_id: marriage_commitment_reversal__practice_doctrine_gap
 *   human_readable: Marriage Commitment Reversal: Practice-Doctrine Gap
 *   domain: religious_institutional_history/commitment_systems/political_theology
 *
 * SUMMARY:
 *   This constraint describes the structural ambiguity within a religious
 *   institution where a core marriage-commitment principle (Section 132) was
 *   preserved in doctrine while its practice was suspended in public
 *   compliance due to external pressure. This reading highlights the gap
 *   between the stated principle and the actual behavior, which allowed the
 *   institution to survive but created significant internal costs for its
 *   members. The period 1890-1904 is critical as it marks the initial public
 *   suspension and the subsequent period of navigating this ambiguity.
 *
 * KEY AGENTS:
 *   - institutional_survival_of_church: Primary beneficiary (institutional/arbitrage)
 *   - general_membership: Primary victim (moderate/identity_locked)
 *   - fundamentalist_factions: Secondary victim (organized/constrained)
 *   - federal_government: Agenda setter (institutional/analytical)
 *   - church_leadership: Agenda setter (institutional/constrained)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_commitment_reversal__practice_doctrine_gap, 0.85).
domain_priors:suppression_score(marriage_commitment_reversal__practice_doctrine_gap, 0.7).
domain_priors:theater_ratio(marriage_commitment_reversal__practice_doctrine_gap, 0.6).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_commitment_reversal__practice_doctrine_gap, extractiveness, 0.85).
narrative_ontology:constraint_metric(marriage_commitment_reversal__practice_doctrine_gap, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(marriage_commitment_reversal__practice_doctrine_gap, theater_ratio, 0.6).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(marriage_commitment_reversal__practice_doctrine_gap, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(marriage_commitment_reversal__practice_doctrine_gap, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_commitment_reversal__practice_doctrine_gap, tangled_rope).
narrative_ontology:human_readable(marriage_commitment_reversal__practice_doctrine_gap, "Marriage Commitment Reversal: Practice-Doctrine Gap").
narrative_ontology:topic_domain(marriage_commitment_reversal__practice_doctrine_gap, "religious_institutional_history/commitment_systems/political_theology").

domain_priors:requires_active_enforcement(marriage_commitment_reversal__practice_doctrine_gap).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_commitment_reversal__practice_doctrine_gap, 'a2434f7a-3a69-4be8-bb36-5dc7826335de').
narrative_ontology:cs_kernel_codification('a2434f7a-3a69-4be8-bb36-5dc7826335de', fixed_text).
narrative_ontology:cs_authority_grounding('a2434f7a-3a69-4be8-bb36-5dc7826335de', lineage).
narrative_ontology:cs_interpretation_layer_present('a2434f7a-3a69-4be8-bb36-5dc7826335de').
narrative_ontology:cs_reading_relation('a2434f7a-3a69-4be8-bb36-5dc7826335de', marriage_commitment_reversal__exogenous_override_reading, coexists_with).
narrative_ontology:cs_reading_relation('a2434f7a-3a69-4be8-bb36-5dc7826335de', marriage_commitment_reversal__endogenous_reinterpretation_reading, coexists_with).
narrative_ontology:cs_axiom('a2434f7a-3a69-4be8-bb36-5dc7826335de', foundational, doctrinal_integrity_in_face_of_persecution).
narrative_ontology:cs_axiom_status(doctrinal_integrity_in_face_of_persecution, holdable).
narrative_ontology:cs_axiom_grounding('a2434f7a-3a69-4be8-bb36-5dc7826335de', doctrinal_integrity_in_face_of_persecution, deontological).
narrative_ontology:cs_axiom('a2434f7a-3a69-4be8-bb36-5dc7826335de', foundational, institutional_survival_as_supreme_good).
narrative_ontology:cs_axiom_status(institutional_survival_as_supreme_good, holdable).
narrative_ontology:cs_axiom_grounding('a2434f7a-3a69-4be8-bb36-5dc7826335de', institutional_survival_as_supreme_good, instrumental).
narrative_ontology:cs_reference_frame('a2434f7a-3a69-4be8-bb36-5dc7826335de', doctrinal_principle_uncompromised).
narrative_ontology:cs_drift_state('a2434f7a-3a69-4be8-bb36-5dc7826335de', post_manifesto_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('a2434f7a-3a69-4be8-bb36-5dc7826335de', '').
narrative_ontology:cs_kernel_id(marriage_commitment_reversal__practice_doctrine_gap, marriage_commitment_reversal).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_commitment_reversal__practice_doctrine_gap, institutional_survival_of_church).
narrative_ontology:constraint_victim(marriage_commitment_reversal__practice_doctrine_gap, general_membership).
narrative_ontology:constraint_victim(marriage_commitment_reversal__practice_doctrine_gap, fundamentalist_factions).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The church institution benefits from the ambiguity, allowing it to navigate external legal pressures while maintaining internal doctrinal claims. This dual-track legitimation enabled continued, albeit clandestine, practices in some jurisdictions, ensuring institutional continuity and growth.
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__practice_doctrine_gap, institutional_survival_of_church, beneficiary,
    institutional, generational, arbitrage, global).

% Members experienced bewilderment and a sense of betrayal due to the lack of clarity regarding the church's core marriage principles. Their commitment to the institution made exit difficult, leading to internal conflict and confusion.
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__practice_doctrine_gap, general_membership, payer,
    moderate, biographical, identity_locked, local).

% These factions resisted the perceived doctrinal shift, leading to schism and excommunication. They bore the cost of maintaining the original interpretation, often at great personal and communal expense, due to their deep commitment to the original principle.
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__practice_doctrine_gap, fundamentalist_factions, payer,
    organized, generational, constrained, regional).

% The external force that imposed legal and political pressure, leading to the suspension of practice. While not directly benefiting from the ambiguity, its actions created the conditions for the gap to emerge and persist.
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__practice_doctrine_gap, federal_government, agenda_setter,
    institutional, generational, analytical, national).

% The leadership navigated the tension between doctrine and practice, publicly suspending the practice while preserving the doctrinal principle. This allowed them to maintain institutional integrity in the face of external threats, albeit at the cost of internal clarity.
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__practice_doctrine_gap, church_leadership, agenda_setter,
    institutional, generational, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The constraint coordinated the church's response to external legal and political pressure, allowing it to maintain its institutional structure and doctrinal claims in the face of existential threats.
% TRANSFER_FUNCTION: Transferred clarity and consistency from the general membership and fundamentalist factions to the institutional survival of the church, by maintaining an ambiguous stance on a core doctrine.
% ABSENT_VOICES: Those who left the church due to the doctrinal ambiguity or those who were excommunicated for adhering to the original practice are absent. They would argue for clear, consistent doctrine and practice, even if it meant greater institutional sacrifice.
% DISAPPEARANCE_RATIONALE: If the practice-doctrine gap vanished, the church would be forced to either fully re-embrace the original practice (leading to renewed conflict with external authorities) or formally abandon the doctrine (leading to internal schism). The institutional structure and its relationship with its members would fundamentally change.
% FOUNDING_PROBLEM: The church faced existential threats from the federal government due to its marriage practices, risking disincorporation, confiscation of assets, and imprisonment of leaders.
% FOUNDING_PROBLEM_CORROBORATION: Historical records, government documents, and independent scholarly analyses corroborate the severe federal pressure. While the immediate threat of disincorporation has passed, the church leadership maintains that the need for institutional flexibility in a hostile legal environment remains, though this is contested by fundamentalist factions.
narrative_ontology:disappearance_verdict(marriage_commitment_reversal__practice_doctrine_gap, world_rearranges).
narrative_ontology:founding_problem_status(marriage_commitment_reversal__practice_doctrine_gap, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(marriage_commitment_reversal__practice_doctrine_gap, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(marriage_commitment_reversal__practice_doctrine_gap, 'none', 1).
narrative_ontology:epsilon_provenance(marriage_commitment_reversal__practice_doctrine_gap, 0.85, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(marriage_commitment_reversal__practice_doctrine_gap_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(marriage_commitment_reversal__practice_doctrine_gap, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(marriage_commitment_reversal__practice_doctrine_gap_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high because the ambiguity imposed significant costs on the membership, who experienced a loss of clarity and consistency in core doctrine. Suppression is high due to the institutional power to enforce public compliance and manage internal dissent. Theater ratio is significant because the public suspension of practice was a performance for external authorities, while the underlying doctrine remained, and some practices continued clandestinely. The rising extractiveness and theater ratio over the interval reflect the increasing strain of maintaining this gap.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the institutional leadership, this was a necessary strategic maneuver for survival. From the perspective of the general membership and fundamentalist factions, it was a betrayal of core principles and a source of profound confusion or schism. The engine's classification will reflect this divergence, with the institutional seat likely computing a more 'rope-like' or 'scaffold-like' type, while the membership seats compute a 'snare-like' or 'tangled_rope' type.
 *
 * DIRECTIONALITY LOGIC:
 *   The institutional survival of the church is the primary beneficiary, as the ambiguity allowed it to avoid dissolution. The general membership and fundamentalist factions are victims, bearing the costs of doctrinal confusion, identity strain, or schism. The federal government acts as an external agenda-setter, creating the pressure that necessitated the gap. Church leadership, while an agenda-setter, also faces constraints in navigating this tension.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate was to ensure institutional survival in the face of federal persecution. While the immediate threat of disincorporation passed, the ambiguity persisted, suggesting a form of mandatrophy where the 'solution' (the gap) became a self-sustaining mechanism for institutional flexibility, even as it extracted costs from members. The classification as a Tangled Rope captures both the coordination function (institutional survival) and the asymmetric extraction (from members).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    extent_of_clandestine_practice,
    'To what extent did the ''suspension'' of practice involve continued, albeit clandestine, marriage commitments, and how widely was this known within the church?',
    'Archival research into private records, diaries, and local congregational histories; demographic analysis of marriage patterns in claimed-legal jurisdictions during the period.',
    'If clandestine practice was widespread and known, the ''theater_ratio'' would be higher, and the ''suppression'' metric would be more accurately attributed to internal enforcement of public compliance rather than genuine cessation. This would strengthen the ''tangled_rope'' classification by highlighting the deliberate nature of the ambiguity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extent_of_clandestine_practice, empirical, 'Ambiguity regarding the true extent of continued marriage practices during the period of public suspension.').

omega_variable(
    doctrinal_integrity_vs_institutional_survival,
    'Was the preservation of Section 132 in doctrine a genuine commitment to its principle, or a strategic maneuver to maintain institutional legitimacy while adapting to external pressure?',
    'Analysis of internal theological debates and pronouncements from the period, comparing them with public statements and actions. Examination of subsequent doctrinal developments once external pressure subsided.',
    'If primarily a strategic maneuver, the ''extractiveness'' from members (in terms of clarity and consistency) is higher, as the institution prioritized its own survival over doctrinal integrity. This would push the classification closer to a ''snare'' for the membership seats. If a genuine commitment, the ''tangled_rope'' classification holds, with the extraction being a byproduct of a difficult coordination problem.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(doctrinal_integrity_vs_institutional_survival, conceptual, 'The underlying motivation for preserving the doctrine while suspending practice.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression primarily structural (external federal threats) or internalized (cognitive patterns of obedience and identity fusion within the church)?',
    'Post-exit suppression trajectory of excommunicated fundamentalists: if suppression persists after the extractive mechanism is removed (e.g., continued social ostracism, psychological distress), reclassify as partially internalized.',
    'If internalized, the constraint''s effective suppression on the general membership is higher than the structural measure suggests — members carry the suppression with them after exit, making exit more costly. This would amplify the ''snare'' aspects for those seats.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for the general membership.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_commitment_reversal__practice_doctrine_gap, 1890, 1904).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(marr_tr_t1890, marriage_commitment_reversal__practice_doctrine_gap, theater_ratio, 1890, 0.4).
narrative_ontology:measurement(marr_tr_t1894, marriage_commitment_reversal__practice_doctrine_gap, theater_ratio, 1894, 0.48).
narrative_ontology:measurement(marr_tr_t1898, marriage_commitment_reversal__practice_doctrine_gap, theater_ratio, 1898, 0.55).
narrative_ontology:measurement(marr_tr_t1901, marriage_commitment_reversal__practice_doctrine_gap, theater_ratio, 1901, 0.58).
narrative_ontology:measurement(marr_tr_t1904, marriage_commitment_reversal__practice_doctrine_gap, theater_ratio, 1904, 0.6).

% Extraction over time
narrative_ontology:measurement(marr_be_t1890, marriage_commitment_reversal__practice_doctrine_gap, base_extractiveness, 1890, 0.7).
narrative_ontology:measurement(marr_be_t1894, marriage_commitment_reversal__practice_doctrine_gap, base_extractiveness, 1894, 0.75).
narrative_ontology:measurement(marr_be_t1898, marriage_commitment_reversal__practice_doctrine_gap, base_extractiveness, 1898, 0.8).
narrative_ontology:measurement(marr_be_t1901, marriage_commitment_reversal__practice_doctrine_gap, base_extractiveness, 1901, 0.83).
narrative_ontology:measurement(marr_be_t1904, marriage_commitment_reversal__practice_doctrine_gap, base_extractiveness, 1904, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(marr_su_t1890, marriage_commitment_reversal__practice_doctrine_gap, suppression_requirement, 1890, 0.6).
narrative_ontology:measurement(marr_su_t1894, marriage_commitment_reversal__practice_doctrine_gap, suppression_requirement, 1894, 0.63).
narrative_ontology:measurement(marr_su_t1898, marriage_commitment_reversal__practice_doctrine_gap, suppression_requirement, 1898, 0.66).
narrative_ontology:measurement(marr_su_t1901, marriage_commitment_reversal__practice_doctrine_gap, suppression_requirement, 1901, 0.68).
narrative_ontology:measurement(marr_su_t1904, marriage_commitment_reversal__practice_doctrine_gap, suppression_requirement, 1904, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_commitment_reversal__practice_doctrine_gap, identity_coordination).
narrative_ontology:affects_constraint(marriage_commitment_reversal__practice_doctrine_gap, marriage_commitment_reversal__exogenous_override_reading).
narrative_ontology:affects_constraint(marriage_commitment_reversal__practice_doctrine_gap, marriage_commitment_reversal__endogenous_reinterpretation_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'marriage_commitment_reversal' kernel, focusing on the practice-doctrine gap. It is linked to sibling readings that emphasize exogenous override and endogenous reinterpretation, as these different framings of the same historical event have distinct structural implications.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
