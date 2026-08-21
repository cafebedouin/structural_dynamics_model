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
 *   constraint_id: marriage_commitment_reversal__practice_doctrine_gap
 *   human_readable: Marriage Commitment Reversal: Practice-Doctrine Gap
 *   domain: religious_institutional_history/commitment_systems/political_theology
 *
 * SUMMARY:
 *   This constraint describes the structural ambiguity within a religious
 *   institution where a core doctrinal principle (Section 132, mandating
 *   plural marriage) was preserved in official doctrine, while its practice
 *   was publicly suspended to comply with federal law. This created a gap
 *   between stated belief and actual behavior, allowing the institution to
 *   navigate external threats while maintaining internal claims of divine
 *   continuity. This reading focuses on the *structural ambiguity* itself as
 *   the constraint, rather than the specific reasons for the reversal.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_commitment_reversal__practice_doctrine_gap, 0.85).
domain_priors:suppression_score(marriage_commitment_reversal__practice_doctrine_gap, 0.78).
domain_priors:theater_ratio(marriage_commitment_reversal__practice_doctrine_gap, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_commitment_reversal__practice_doctrine_gap, extractiveness, 0.85).
narrative_ontology:constraint_metric(marriage_commitment_reversal__practice_doctrine_gap, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(marriage_commitment_reversal__practice_doctrine_gap, theater_ratio, 0.65).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(marriage_commitment_reversal__practice_doctrine_gap, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(marriage_commitment_reversal__practice_doctrine_gap, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_commitment_reversal__practice_doctrine_gap, tangled_rope).
narrative_ontology:human_readable(marriage_commitment_reversal__practice_doctrine_gap, "Marriage Commitment Reversal: Practice-Doctrine Gap").
narrative_ontology:topic_domain(marriage_commitment_reversal__practice_doctrine_gap, "religious_institutional_history/commitment_systems/political_theology").

domain_priors:requires_active_enforcement(marriage_commitment_reversal__practice_doctrine_gap).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_commitment_reversal__practice_doctrine_gap, '29f8e21e-9e20-4644-b726-f540f7e956f4').
narrative_ontology:cs_kernel_codification('29f8e21e-9e20-4644-b726-f540f7e956f4', fixed_text).
narrative_ontology:cs_authority_grounding('29f8e21e-9e20-4644-b726-f540f7e956f4', lineage).
narrative_ontology:cs_interpretation_layer_present('29f8e21e-9e20-4644-b726-f540f7e956f4').
narrative_ontology:cs_reading_relation('29f8e21e-9e20-4644-b726-f540f7e956f4', marriage_commitment_reversal__exogenous_override_reading, coexists_with).
narrative_ontology:cs_reading_relation('29f8e21e-9e20-4644-b726-f540f7e956f4', marriage_commitment_reversal__endogenous_reinterpretation_reading, coexists_with).
narrative_ontology:cs_axiom('29f8e21e-9e20-4644-b726-f540f7e956f4', foundational, divine_mandate_of_plural_marriage_is_eternal).
narrative_ontology:cs_axiom_status(divine_mandate_of_plural_marriage_is_eternal, holdable).
narrative_ontology:cs_axiom_grounding('29f8e21e-9e20-4644-b726-f540f7e956f4', divine_mandate_of_plural_marriage_is_eternal, theological).
narrative_ontology:cs_axiom('29f8e21e-9e20-4644-b726-f540f7e956f4', foundational, institutional_survival_requires_public_compliance).
narrative_ontology:cs_axiom_status(institutional_survival_requires_public_compliance, holdable).
narrative_ontology:cs_axiom_grounding('29f8e21e-9e20-4644-b726-f540f7e956f4', institutional_survival_requires_public_compliance, instrumental).
narrative_ontology:cs_reference_frame('29f8e21e-9e20-4644-b726-f540f7e956f4', unambiguous_divine_mandate_of_plural_marriage).
narrative_ontology:cs_drift_state('29f8e21e-9e20-4644-b726-f540f7e956f4', post_manifesto_era, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('29f8e21e-9e20-4644-b726-f540f7e956f4', '').
narrative_ontology:cs_kernel_id(marriage_commitment_reversal__practice_doctrine_gap, marriage_commitment_reversal).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_commitment_reversal__practice_doctrine_gap, institutional_leadership).
narrative_ontology:constraint_victim(marriage_commitment_reversal__practice_doctrine_gap, general_membership).
narrative_ontology:constraint_victim(marriage_commitment_reversal__practice_doctrine_gap, fundamentalist_factions).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintained the institutional structure and its claim to divine authority by publicly suspending a core practice (plural marriage) while preserving its doctrinal principle (Section 132). Benefited from the flexibility and ambiguity that allowed the institution to survive legal threats and expand, while managing internal dissent.
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__practice_doctrine_gap, institutional_leadership, agenda_setter,
    institutional, generational, arbitrage, global).

% Experienced bewilderment and a sense of betrayal as a core, divinely mandated practice was suspended while its doctrinal basis remained. Faced pressure to comply with the new (ambiguous) practice or risk social and spiritual ostracization. Paid with clarity and doctrinal consistency.
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__practice_doctrine_gap, general_membership, payer,
    moderate, biographical, constrained, national).

% Rejected the suspension of practice as a compromise of divine principle, leading to schism and the formation of splinter groups. Their identity was deeply tied to the original, unambiguous doctrine and practice, making compliance with the ambiguity impossible without abandoning their core beliefs.
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__practice_doctrine_gap, fundamentalist_factions, payer,
    organized, generational, identity_locked, national).

% Exerted significant legal and political pressure on the institution to abandon plural marriage, threatening its property and legal status. Its actions created the external conditions that necessitated the institutional response, but it did not directly control the internal doctrinal management.
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__practice_doctrine_gap, federal_government, observer,
    institutional, generational, analytical, national).

% Analyze the historical record, institutional statements, and internal documents to understand the motivations and consequences of the practice-doctrine gap. Their work often highlights the structural ambiguities and their impact on various stakeholders.
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__practice_doctrine_gap, historical_scholars, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(marriage_commitment_reversal__practice_doctrine_gap, institutional_leadership).
narrative_ontology:fixing_cost_class(marriage_commitment_reversal__practice_doctrine_gap, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To maintain the institutional structure and its claim to divine authority in the face of existential legal and social threats, by creating a public posture of compliance while preserving core doctrine.
% TRANSFER_FUNCTION: Transfers doctrinal clarity and consistency from the general membership and fundamentalist factions to the institutional leadership, in exchange for institutional survival and continued operation.
% ABSENT_VOICES: Those who left the institution due to the perceived doctrinal compromise or who were excommunicated for continuing the practice of plural marriage. Their voices would highlight the deep spiritual and social costs of the ambiguity.
% DISAPPEARANCE_RATIONALE: If the managed ambiguity vanished overnight, the institution's historical narrative of continuity and divine guidance would collapse. This would lead to a profound crisis of legitimacy, potentially fracturing the institution and forcing a definitive resolution on its core doctrines and practices.
% FOUNDING_PROBLEM: The existential threat posed by federal anti-polygamy laws (e.g., Edmunds-Tucker Act) to the institution's property, legal status, and leadership, while simultaneously upholding the divinely mandated principle of plural marriage.
% FOUNDING_PROBLEM_CORROBORATION: Historical federal court records, legislative debates, and independent religious historians corroborate the severe federal pressure and the institution's strategic response. While the specific federal threat passed, the doctrinal ambiguity it engendered persisted beyond the immediate crisis.
narrative_ontology:disappearance_verdict(marriage_commitment_reversal__practice_doctrine_gap, world_rearranges).
narrative_ontology:founding_problem_status(marriage_commitment_reversal__practice_doctrine_gap, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(marriage_commitment_reversal__practice_doctrine_gap, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
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
 *   Extractiveness is high (0.85) because the ambiguity demanded significant sacrifices from the general membership (clarity, consistency, sometimes personal relationships) for the sake of institutional survival. Suppression (0.78) was high due to the institution's control over information and the social/spiritual costs of dissent. Theater ratio (0.65) is substantial because the public suspension of practice, while doctrine remained, involved a significant performative element to satisfy external observers without fully abandoning internal commitments. The metrics reflect the period immediately following the 1890 Manifesto, where the gap became pronounced and managed.
 *
 * PERSPECTIVAL GAP:
 *   From the institutional leadership's perspective, this was a necessary, divinely guided adaptation for survival. From the perspective of the general membership, it was a confusing and often painful compromise. Fundamentalist factions viewed it as a betrayal of divine mandate. The engine's per-seat classification will highlight these divergences.
 *
 * DIRECTIONALITY LOGIC:
 *   The institutional leadership is the primary beneficiary (d=0.0-0.15), gaining flexibility and ensuring institutional survival. The general membership and fundamentalist factions are the primary targets (d=0.8-1.0), bearing the costs of bewilderment, internal conflict, and schism. The federal government acts as an external force, not directly benefiting from the internal ambiguity but creating the conditions for its emergence.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is this constraint best understood as a structural ambiguity (practice-doctrine gap), an exogenous override, or an endogenous reinterpretation?',
    'Analysis of internal institutional communications, theological justifications, and external political pressures to determine the primary driver and framing of the reversal.',
    'If primarily an exogenous override, the constraint''s extractiveness might be lower (as the institution is a victim of external force); if an endogenous reinterpretation, the theater ratio might be lower (as the doctrinal shift is genuine). This reading emphasizes the ambiguity itself as the constraint.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Distinguishing between different readings of the marriage commitment reversal.').

omega_variable(
    doctrinal_sincerity_ambiguity,
    'To what extent was the preservation of Section 132 in doctrine a genuine theological commitment versus a strategic maneuver to maintain internal legitimacy?',
    'Examination of subsequent doctrinal developments, private teachings, and the long-term institutional stance on plural marriage after the immediate federal threat subsided.',
    'If primarily strategic, the theater ratio and extractiveness would be higher, reflecting a more cynical manipulation of belief. If genuinely theological, the constraint might be closer to a Rope for the leadership, albeit a Tangled Rope for the membership.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(doctrinal_sincerity_ambiguity, empirical, 'Assessing the sincerity of doctrinal preservation amidst practice suspension.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression primarily structural (institutional control over information, social pressure) or internalized (members'' deep-seated belief in institutional authority)?',
    'Post-exit suppression trajectory of former members: if suppression persists as self-censorship or continued loyalty after leaving the institution, it indicates internalized suppression.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests, as members carry the suppression with them after exit, making exit less effective.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism in a religious context.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_commitment_reversal__practice_doctrine_gap, 1890, 1910).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(marr_tr_t1890, marriage_commitment_reversal__practice_doctrine_gap, theater_ratio, 1890, 0.5).
narrative_ontology:measurement(marr_tr_t1895, marriage_commitment_reversal__practice_doctrine_gap, theater_ratio, 1895, 0.58).
narrative_ontology:measurement(marr_tr_t1900, marriage_commitment_reversal__practice_doctrine_gap, theater_ratio, 1900, 0.65).
narrative_ontology:measurement(marr_tr_t1905, marriage_commitment_reversal__practice_doctrine_gap, theater_ratio, 1905, 0.63).
narrative_ontology:measurement(marr_tr_t1910, marriage_commitment_reversal__practice_doctrine_gap, theater_ratio, 1910, 0.65).

% Extraction over time
narrative_ontology:measurement(marr_be_t1890, marriage_commitment_reversal__practice_doctrine_gap, base_extractiveness, 1890, 0.75).
narrative_ontology:measurement(marr_be_t1895, marriage_commitment_reversal__practice_doctrine_gap, base_extractiveness, 1895, 0.8).
narrative_ontology:measurement(marr_be_t1900, marriage_commitment_reversal__practice_doctrine_gap, base_extractiveness, 1900, 0.85).
narrative_ontology:measurement(marr_be_t1905, marriage_commitment_reversal__practice_doctrine_gap, base_extractiveness, 1905, 0.83).
narrative_ontology:measurement(marr_be_t1910, marriage_commitment_reversal__practice_doctrine_gap, base_extractiveness, 1910, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(marr_su_t1890, marriage_commitment_reversal__practice_doctrine_gap, suppression_requirement, 1890, 0.7).
narrative_ontology:measurement(marr_su_t1895, marriage_commitment_reversal__practice_doctrine_gap, suppression_requirement, 1895, 0.75).
narrative_ontology:measurement(marr_su_t1900, marriage_commitment_reversal__practice_doctrine_gap, suppression_requirement, 1900, 0.78).
narrative_ontology:measurement(marr_su_t1905, marriage_commitment_reversal__practice_doctrine_gap, suppression_requirement, 1905, 0.77).
narrative_ontology:measurement(marr_su_t1910, marriage_commitment_reversal__practice_doctrine_gap, suppression_requirement, 1910, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_commitment_reversal__practice_doctrine_gap, identity_coordination).
narrative_ontology:affects_constraint(marriage_commitment_reversal__practice_doctrine_gap, marriage_commitment_reversal__exogenous_override_reading).
narrative_ontology:affects_constraint(marriage_commitment_reversal__practice_doctrine_gap, marriage_commitment_reversal__endogenous_reinterpretation_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'marriage_commitment_reversal' kernel, each representing a different interpretation of the historical events surrounding the suspension of plural marriage. This reading focuses on the structural ambiguity between doctrine and practice.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
