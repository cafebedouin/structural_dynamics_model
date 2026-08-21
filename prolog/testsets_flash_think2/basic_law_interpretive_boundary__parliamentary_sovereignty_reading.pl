% ============================================================================
% CONSTRAINT STORY: basic_law_interpretive_boundary__parliamentary_sovereignty_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_basic_law_interpretive_boundary__parliamentary_sovereignty_reading, []).

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
 *   constraint_id: basic_law_interpretive_boundary__parliamentary_sovereignty_reading
 *   human_readable: Knesset's Ultimate Authority over Basic Laws (Parliamentary Sovereignty Reading)
 *   domain: constitutional_law/comparative_constitutionalism
 *
 * SUMMARY:
 *   This constraint story instantiates the 'parliamentary sovereignty'
 *   reading of the 'basic_law_interpretive_boundary' kernel in Israeli
 *   constitutional law. In this reading, the Knesset, as the directly elected
 *   sovereign body, holds ultimate and unconstrained authority to interpret
 *   and amend Basic Laws via simple majority, including the power to override
 *   judicial review. This perspective views the judiciary's role as advisory,
 *   with no external veto on legislative will, leading to near-zero effective
 *   extraction for majoritarian policy (except where international treaty
 *   obligations might apply). The constraint is classified as a Tangled Rope
 *   because it coordinates the legislative process for the majority while
 *   extracting interpretive power from the judiciary and limiting avenues for
 *   civil society challenges.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, 0.65).
domain_priors:suppression_score(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, 0.78).
domain_priors:theater_ratio(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, tangled_rope).
narrative_ontology:human_readable(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, "Knesset's Ultimate Authority over Basic Laws (Parliamentary Sovereignty Reading)").
narrative_ontology:topic_domain(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, "constitutional_law/comparative_constitutionalism").

domain_priors:requires_active_enforcement(basic_law_interpretive_boundary__parliamentary_sovereignty_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, '3bd7e3bb-0638-4f95-9a8b-b21c9d717019').
narrative_ontology:cs_kernel_codification('3bd7e3bb-0638-4f95-9a8b-b21c9d717019', formalized).
narrative_ontology:cs_authority_grounding('3bd7e3bb-0638-4f95-9a8b-b21c9d717019', lineage).
narrative_ontology:cs_interpretation_layer_present('3bd7e3bb-0638-4f95-9a8b-b21c9d717019').
narrative_ontology:cs_reading_relation('3bd7e3bb-0638-4f95-9a8b-b21c9d717019', basic_law_interpretive_boundary__judicial_supremacy_reading, forecloses).
narrative_ontology:cs_reading_relation('3bd7e3bb-0638-4f95-9a8b-b21c9d717019', basic_law_interpretive_boundary__balanced_contestation_reading, forecloses).
narrative_ontology:cs_axiom('3bd7e3bb-0638-4f95-9a8b-b21c9d717019', foundational, legislative_supremacy_is_foundational).
narrative_ontology:cs_axiom_status(legislative_supremacy_is_foundational, holdable).
narrative_ontology:cs_axiom_grounding('3bd7e3bb-0638-4f95-9a8b-b21c9d717019', legislative_supremacy_is_foundational, conventional).
narrative_ontology:cs_axiom('3bd7e3bb-0638-4f95-9a8b-b21c9d717019', secondary, judicial_review_is_subordinate).
narrative_ontology:cs_axiom_status(judicial_review_is_subordinate, holdable).
narrative_ontology:cs_axiom_grounding('3bd7e3bb-0638-4f95-9a8b-b21c9d717019', judicial_review_is_subordinate, conventional).
narrative_ontology:cs_reference_frame('3bd7e3bb-0638-4f95-9a8b-b21c9d717019', unfettered_legislative_will).
narrative_ontology:cs_drift_state('3bd7e3bb-0638-4f95-9a8b-b21c9d717019', contemporary_judicial_activism_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('3bd7e3bb-0638-4f95-9a8b-b21c9d717019', '').
narrative_ontology:cs_kernel_id(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, basic_law_interpretive_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, knesset_majority).
narrative_ontology:constraint_beneficiary(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, governing_coalition).
narrative_ontology:constraint_victim(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, supreme_court).
narrative_ontology:constraint_victim(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, civil_society_organizations).
narrative_ontology:constraint_victim(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, minority_factions).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, electorate).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds the legislative power and, in this reading, the ultimate authority to interpret and amend Basic Laws. Benefits from unconstrained policy implementation and the absence of judicial vetoes.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, knesset_majority, agenda_setter,
    institutional, generational, arbitrage, national).

% The political bloc that forms the government, directly benefiting from the Knesset's unconstrained power to enact its agenda without judicial interference.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, governing_coalition, beneficiary,
    institutional, biographical, arbitrage, national).

% Its power of judicial review over Basic Laws is rendered advisory or subordinate by this reading. Bears the cost of diminished institutional authority and inability to invalidate legislation deemed unconstitutional.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, supreme_court, payer,
    institutional, generational, constrained, national).

% Advocate for human rights and constitutional protections. Bear the cost of reduced avenues for challenging legislative actions that they perceive as infringing on rights or violating constitutional principles.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, civil_society_organizations, payer,
    organized, biographical, constrained, national).

% Political parties not in the governing coalition. Bear the cost of lacking an effective judicial check on the majority's legislative power, making it harder to protect their interests or block unfavorable legislation.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, minority_factions, payer,
    organized, biographical, constrained, national).

% Benefits from a clear, democratically accountable legislative process and the direct implementation of policies reflecting the will of the majority. May also bear costs if policies are enacted without sufficient checks and balances.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, electorate, beneficiary,
    moderate, biographical, mobile, national).

% Observe and comment on the state of judicial independence and constitutional protections in Israel, but have no direct enforcement power over the Knesset's interpretation of its own Basic Laws.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, international_legal_bodies, observer,
    institutional, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, knesset_majority).
narrative_ontology:fixing_cost_class(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To coordinate legislative will and ensure the direct implementation of policy by the elected majority, preventing judicial obstruction or invalidation of laws based on Basic Law interpretation.
% TRANSFER_FUNCTION: Transfers ultimate interpretive power over Basic Laws from a potentially shared or judicially-mediated domain to the Knesset, and transfers policy outcomes directly from legislative will to citizens without judicial veto.
% ABSENT_VOICES: The judiciary (in terms of ultimate authority over Basic Laws), civil society organizations, and minority factions would object to the concentration of interpretive power in the Knesset, arguing for robust judicial review and constitutional protections.
% DISAPPEARANCE_RATIONALE: If the Knesset's ultimate authority to interpret and amend Basic Laws vanished overnight, the balance of power would shift dramatically, likely empowering the Supreme Court or creating a constitutional vacuum. This would lead to significant political and legal reorganization, as the judiciary would assert a stronger role in constitutional interpretation and review.
% FOUNDING_PROBLEM: To establish a clear, democratic source of ultimate legal authority in the absence of a formal, entrenched constitution, ensuring legislative supremacy and the ability of the elected majority to govern effectively.
% FOUNDING_PROBLEM_CORROBORATION: Legal scholars supporting parliamentary sovereignty, historical legislative practice, and the Knesset's own assertions attest that the problem of ensuring legislative supremacy and democratic accountability remains live. Opponents (judiciary, civil society, minority factions) contest this, arguing that the founding problem has evolved to require stronger checks and balances.
narrative_ontology:disappearance_verdict(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, world_rearranges).
narrative_ontology:founding_problem_status(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, 'none', 1).
narrative_ontology:epsilon_provenance(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(basic_law_interpretive_boundary__parliamentary_sovereignty_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(basic_law_interpretive_boundary__parliamentary_sovereignty_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness is moderate-high (0.65) because while the Knesset majority experiences low extraction, the constraint extracts significant power from the Supreme Court and limits the ability of civil society and minority factions to challenge legislation. Suppression is high (0.78) as this reading actively suppresses judicial review and alternative interpretations of Basic Laws. Theater ratio is low (0.15) because the Knesset's actions are direct assertions of power, not performative maintenance of an atrophied function. Accessibility collapse is high (0.80) for alternatives to the Knesset's ultimate authority, while resistance is also high (0.70) due to ongoing political and legal contestation from those whose power is extracted.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the Knesset majority, this constraint is a legitimate expression of democratic will, ensuring effective governance (a Rope-like function). From the perspective of the Supreme Court and civil society, it is an extractive mechanism that undermines checks and balances (a Snare-like function). The engine's computation of per-seat classification will reflect this divergence based on the declared structural relationships and exit options.
 *
 * DIRECTIONALITY LOGIC:
 *   The Knesset majority and governing coalition are clear beneficiaries (low directionality), as they gain unconstrained legislative power. The Supreme Court, civil society organizations, and minority factions are targets (high directionality), as their ability to act as checks on legislative power is extracted or suppressed. The electorate is a diffuse beneficiary, gaining from clear democratic accountability, but also potentially bearing costs from unchecked power.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reading_identity_parliamentary_sovereignty,
    'Is this constraint a genuine expression of parliamentary sovereignty, or a strategic assertion of power by the Knesset majority?',
    'Historical analysis of constitutional intent, comparative legal scholarship on parliamentary sovereignty, and analysis of the political context surrounding its assertion.',
    'If a genuine expression, the constraint''s legitimacy is higher, and its classification as a Tangled Rope is more stable. If a strategic assertion, its extractive nature is amplified, potentially pushing it closer to a Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_identity_parliamentary_sovereignty, conceptual, 'Ambiguity regarding the nature of the parliamentary sovereignty claim.').

omega_variable(
    structural_delta_vs_judicial_supremacy_reading,
    'What would be the structural changes if the ''judicial_supremacy_reading'' of the Basic Laws were adopted instead of this ''parliamentary_sovereignty_reading''?',
    'Analysis of legal precedents, proposed constitutional reforms, and the actual impact of judicial rulings in systems with strong judicial review.',
    'The primary impact would be a reversal of beneficiary/victim roles, with the Supreme Court gaining interpretive authority and the Knesset''s legislative power becoming constrained by judicial review. This would fundamentally alter the constraint''s extractiveness and suppression profile.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(structural_delta_vs_judicial_supremacy_reading, conceptual, 'Structural differences between parliamentary sovereignty and judicial supremacy readings.').

omega_variable(
    structural_delta_vs_balanced_contestation_reading,
    'What would be the structural changes if the ''balanced_contestation_reading'' of the Basic Laws were adopted instead of this ''parliamentary_sovereignty_reading''?',
    'Analysis of constitutional frameworks that explicitly define shared or bounded authority between legislative and judicial branches, and the practical outcomes of such arrangements.',
    'The impact would be a re-balancing of power, where both the Knesset and the Supreme Court would operate under mutually recognized limits. This would likely reduce the extractiveness from the judiciary and the suppression of civil society challenges, moving the constraint closer to a Rope or a less extractive Tangled Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(structural_delta_vs_balanced_contestation_reading, conceptual, 'Structural differences between parliamentary sovereignty and balanced contestation readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, 1948, 2023).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(basi_tr_t1948, basic_law_interpretive_boundary__parliamentary_sovereignty_reading, theater_ratio, 1948, 0.1).
narrative_ontology:measurement(basi_tr_t1970, basic_law_interpretive_boundary__parliamentary_sovereignty_reading, theater_ratio, 1970, 0.12).
narrative_ontology:measurement(basi_tr_t1992, basic_law_interpretive_boundary__parliamentary_sovereignty_reading, theater_ratio, 1992, 0.13).
narrative_ontology:measurement(basi_tr_t2005, basic_law_interpretive_boundary__parliamentary_sovereignty_reading, theater_ratio, 2005, 0.14).
narrative_ontology:measurement(basi_tr_t2015, basic_law_interpretive_boundary__parliamentary_sovereignty_reading, theater_ratio, 2015, 0.14).
narrative_ontology:measurement(basi_tr_t2023, basic_law_interpretive_boundary__parliamentary_sovereignty_reading, theater_ratio, 2023, 0.15).

% Extraction over time
narrative_ontology:measurement(basi_be_t1948, basic_law_interpretive_boundary__parliamentary_sovereignty_reading, base_extractiveness, 1948, 0.55).
narrative_ontology:measurement(basi_be_t1970, basic_law_interpretive_boundary__parliamentary_sovereignty_reading, base_extractiveness, 1970, 0.58).
narrative_ontology:measurement(basi_be_t1992, basic_law_interpretive_boundary__parliamentary_sovereignty_reading, base_extractiveness, 1992, 0.6).
narrative_ontology:measurement(basi_be_t2005, basic_law_interpretive_boundary__parliamentary_sovereignty_reading, base_extractiveness, 2005, 0.62).
narrative_ontology:measurement(basi_be_t2015, basic_law_interpretive_boundary__parliamentary_sovereignty_reading, base_extractiveness, 2015, 0.63).
narrative_ontology:measurement(basi_be_t2023, basic_law_interpretive_boundary__parliamentary_sovereignty_reading, base_extractiveness, 2023, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(basi_su_t1948, basic_law_interpretive_boundary__parliamentary_sovereignty_reading, suppression_requirement, 1948, 0.65).
narrative_ontology:measurement(basi_su_t1970, basic_law_interpretive_boundary__parliamentary_sovereignty_reading, suppression_requirement, 1970, 0.68).
narrative_ontology:measurement(basi_su_t1992, basic_law_interpretive_boundary__parliamentary_sovereignty_reading, suppression_requirement, 1992, 0.72).
narrative_ontology:measurement(basi_su_t2005, basic_law_interpretive_boundary__parliamentary_sovereignty_reading, suppression_requirement, 2005, 0.75).
narrative_ontology:measurement(basi_su_t2015, basic_law_interpretive_boundary__parliamentary_sovereignty_reading, suppression_requirement, 2015, 0.77).
narrative_ontology:measurement(basi_su_t2023, basic_law_interpretive_boundary__parliamentary_sovereignty_reading, suppression_requirement, 2023, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, basic_law_interpretive_boundary__judicial_supremacy_reading).
narrative_ontology:affects_constraint(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, basic_law_interpretive_boundary__balanced_contestation_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three distinct readings of the 'basic_law_interpretive_boundary' kernel. Each reading represents a different structural claim about the locus of ultimate authority over Basic Laws, leading to different ε values and classifications. They are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
