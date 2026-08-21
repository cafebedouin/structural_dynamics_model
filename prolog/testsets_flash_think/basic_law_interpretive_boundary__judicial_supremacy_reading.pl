% ============================================================================
% CONSTRAINT STORY: basic_law_interpretive_boundary__judicial_supremacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_basic_law_interpretive_boundary__judicial_supremacy_reading, []).

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
 *   constraint_id: basic_law_interpretive_boundary__judicial_supremacy_reading
 *   human_readable: Judicial Supremacy Reading of Basic Laws Interpretive Boundary
 *   domain: constitutional_law/comparative_constitutionalism
 *
 * SUMMARY:
 *   This constraint describes the 'judicial supremacy' reading of the Basic
 *   Laws' interpretive boundary in Israel, where the Supreme Court asserts
 *   its authority to interpret and enforce Basic Laws as a higher-order legal
 *   framework, including the power to invalidate contradictory legislation.
 *   This reading emerged and strengthened following the 'Constitutional
 *   Revolution' of the 1990s, leading to ongoing political and legal
 *   contestation over the balance of power between the judiciary and the
 *   legislature.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(basic_law_interpretive_boundary__judicial_supremacy_reading, 0.8).
domain_priors:suppression_score(basic_law_interpretive_boundary__judicial_supremacy_reading, 0.9).
domain_priors:theater_ratio(basic_law_interpretive_boundary__judicial_supremacy_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(basic_law_interpretive_boundary__judicial_supremacy_reading, extractiveness, 0.8).
narrative_ontology:constraint_metric(basic_law_interpretive_boundary__judicial_supremacy_reading, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(basic_law_interpretive_boundary__judicial_supremacy_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(basic_law_interpretive_boundary__judicial_supremacy_reading, accessibility_collapse, 0.85).
narrative_ontology:constraint_metric(basic_law_interpretive_boundary__judicial_supremacy_reading, resistance, 0.9).

% --- Constraint claim ---
narrative_ontology:constraint_claim(basic_law_interpretive_boundary__judicial_supremacy_reading, tangled_rope).
narrative_ontology:human_readable(basic_law_interpretive_boundary__judicial_supremacy_reading, "Judicial Supremacy Reading of Basic Laws Interpretive Boundary").
narrative_ontology:topic_domain(basic_law_interpretive_boundary__judicial_supremacy_reading, "constitutional_law/comparative_constitutionalism").

domain_priors:requires_active_enforcement(basic_law_interpretive_boundary__judicial_supremacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(basic_law_interpretive_boundary__judicial_supremacy_reading, '5561d92f-07ae-4fd7-a6bc-02e50ffdcf86').
narrative_ontology:cs_kernel_codification('5561d92f-07ae-4fd7-a6bc-02e50ffdcf86', formalized).
narrative_ontology:cs_authority_grounding('5561d92f-07ae-4fd7-a6bc-02e50ffdcf86', lineage).
narrative_ontology:cs_interpretation_layer_present('5561d92f-07ae-4fd7-a6bc-02e50ffdcf86').
narrative_ontology:cs_reading_relation('5561d92f-07ae-4fd7-a6bc-02e50ffdcf86', basic_law_interpretive_boundary__parliamentary_sovereignty_reading, forecloses).
narrative_ontology:cs_reading_relation('5561d92f-07ae-4fd7-a6bc-02e50ffdcf86', basic_law_interpretive_boundary__balanced_contestation_reading, coexists_with).
narrative_ontology:cs_axiom('5561d92f-07ae-4fd7-a6bc-02e50ffdcf86', foundational, basic_laws_are_supreme_law).
narrative_ontology:cs_axiom_status(basic_laws_are_supreme_law, holdable).
narrative_ontology:cs_axiom_grounding('5561d92f-07ae-4fd7-a6bc-02e50ffdcf86', basic_laws_are_supreme_law, conventional).
narrative_ontology:cs_axiom('5561d92f-07ae-4fd7-a6bc-02e50ffdcf86', foundational, judicial_review_is_constitutional_mandate).
narrative_ontology:cs_axiom_status(judicial_review_is_constitutional_mandate, holdable).
narrative_ontology:cs_axiom_grounding('5561d92f-07ae-4fd7-a6bc-02e50ffdcf86', judicial_review_is_constitutional_mandate, conventional).
narrative_ontology:cs_reference_frame('5561d92f-07ae-4fd7-a6bc-02e50ffdcf86', constitutional_supremacy_framework).
narrative_ontology:cs_drift_state('5561d92f-07ae-4fd7-a6bc-02e50ffdcf86', contemporary_political_contest, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('5561d92f-07ae-4fd7-a6bc-02e50ffdcf86', '').
narrative_ontology:cs_kernel_id(basic_law_interpretive_boundary__judicial_supremacy_reading, basic_law_interpretive_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(basic_law_interpretive_boundary__judicial_supremacy_reading, supreme_court).
narrative_ontology:constraint_beneficiary(basic_law_interpretive_boundary__judicial_supremacy_reading, rights_claimants).
narrative_ontology:constraint_beneficiary(basic_law_interpretive_boundary__judicial_supremacy_reading, legal_profession).
narrative_ontology:constraint_victim(basic_law_interpretive_boundary__judicial_supremacy_reading, knesset).
narrative_ontology:constraint_victim(basic_law_interpretive_boundary__judicial_supremacy_reading, government).
narrative_ontology:constraint_victim(basic_law_interpretive_boundary__judicial_supremacy_reading, electorate).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets the Basic Laws as a higher-order legal framework, invalidating contradictory legislation passed by the Knesset. Its rulings are binding, establishing its role as the ultimate arbiter of constitutional legality.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__judicial_supremacy_reading, supreme_court, agenda_setter,
    institutional, generational, arbitrage, national).

% The legislative body whose laws are subject to judicial review and potential invalidation by the Supreme Court. This constrains its legislative power and the direct implementation of its electoral mandate.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__judicial_supremacy_reading, knesset, payer,
    institutional, biographical, constrained, national).

% The executive branch, whose legislative agenda and policy implementation can be blocked or altered by Supreme Court rulings that invalidate laws or government actions deemed contrary to Basic Laws.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__judicial_supremacy_reading, government, payer,
    institutional, biographical, constrained, national).

% Individuals and groups who can petition the Supreme Court to protect their rights and liberties against legislative or executive actions, effectively gaining a veto point through litigation.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__judicial_supremacy_reading, rights_claimants, beneficiary,
    organized, biographical, mobile, national).

% The citizens whose legislative choices, expressed through their elected representatives in the Knesset, can be overturned by the Supreme Court, leading to a perceived democratic deficit.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__judicial_supremacy_reading, electorate, payer,
    organized, biographical, constrained, national).

% Benefits from the complexity and interpretive role of the Supreme Court, as the need for constitutional expertise and litigation services increases with robust judicial review.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__judicial_supremacy_reading, legal_profession, beneficiary,
    organized, biographical, mobile, national).

% Advocate for the Knesset's ultimate authority as the elected sovereign, arguing against judicial invalidation of legislation. Their perspective is structurally marginalized by this reading's assertion of judicial supremacy.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__judicial_supremacy_reading, parliamentary_sovereignty_advocates, excluded,
    organized, generational, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(basic_law_interpretive_boundary__judicial_supremacy_reading, supreme_court).
narrative_ontology:fixing_cost_class(basic_law_interpretive_boundary__judicial_supremacy_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a stable, rights-protective legal framework by ensuring legislative acts conform to higher constitutional principles embodied in the Basic Laws, preventing legislative overreach and ensuring legal consistency.
% TRANSFER_FUNCTION: Transfers ultimate interpretive authority over Basic Laws from the legislature to the judiciary, and transfers the power to nullify legislation from the political process to the courts. This shifts power from elected representatives to unelected judges.
% ABSENT_VOICES: Advocates for pure parliamentary sovereignty, who would argue that the elected body should have the final say on all legislation, including the interpretation and amendment of Basic Laws. Their arguments are often dismissed as undermining the rule of law.
% DISAPPEARANCE_RATIONALE: If the Supreme Court's binding interpretive and invalidation power vanished overnight, the legal system would lose its higher-order framework. This would likely lead to legislative instability, potential erosion of rights, and a rapid shift towards pure parliamentary sovereignty, fundamentally reorganizing the balance of power.
% FOUNDING_PROBLEM: To establish a robust constitutional framework and protect individual rights in the absence of a formal, entrenched constitution, preventing legislative overreach and ensuring a stable legal order.
% FOUNDING_PROBLEM_CORROBORATION: The Supreme Court and many legal scholars attest to the ongoing need for judicial review to protect rights and maintain constitutional order. However, political parties and some legal academics, particularly those advocating for parliamentary sovereignty, contest this, arguing the founding problem is either solved or that the court has overstepped its original mandate, leading to a democratic deficit.
narrative_ontology:disappearance_verdict(basic_law_interpretive_boundary__judicial_supremacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(basic_law_interpretive_boundary__judicial_supremacy_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(basic_law_interpretive_boundary__judicial_supremacy_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(basic_law_interpretive_boundary__judicial_supremacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(basic_law_interpretive_boundary__judicial_supremacy_reading, 0.8, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(basic_law_interpretive_boundary__judicial_supremacy_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(basic_law_interpretive_boundary__judicial_supremacy_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(basic_law_interpretive_boundary__judicial_supremacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The high extractiveness (0.80) reflects the significant transfer of legislative authority from the Knesset to the Supreme Court, as the court's power to nullify laws directly impacts the legislative process. Suppression (0.90) is high because judicial invalidation is binding and actively enforced, effectively closing off legislative alternatives. Theater ratio is low (0.10) because the court's actions are genuinely impactful and not merely performative. Resistance (0.90) is very high, reflecting the intense political debate and legislative attempts to curb judicial power, particularly in recent years.
 *
 * PERSPECTIVAL GAP:
 *   The Supreme Court and rights claimants perceive this arrangement as a necessary 'rope' or 'mountain' for upholding the rule of law and protecting fundamental rights. In contrast, the Knesset, Government, and Electorate often experience it as a 'snare' or 'tangled rope' that extracts their democratic legislative authority. The classification as a Tangled Rope acknowledges both the coordination function and the asymmetric extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   The Supreme Court is the primary agenda-setter and beneficiary, gaining significant institutional power and authority. Rights claimants and the legal profession also benefit from the enhanced judicial review. The Knesset, Government, and Electorate are the payers, as their legislative and democratic will is constrained and potentially overridden by judicial decisions. Parliamentary sovereignty advocates are excluded, as their core premise is rejected by this reading.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as a Tangled Rope prevents mislabeling this constraint. It acknowledges the genuine coordination function of providing a stable constitutional framework and protecting rights (a 'rope' aspect), while simultaneously recognizing the asymmetric extraction of legislative authority from the Knesset and the electorate (a 'snare' aspect). This prevents mislabeling it as a pure Rope (which would ignore the significant costs borne by the legislature) or a pure Snare (which would ignore the legitimate function of upholding higher law and protecting rights). This is not a case of mandatrophy, but rather a live, contested power dynamic.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    basic_laws_constitutional_status_ambiguity,
    'Are the Basic Laws truly a higher-order constitutional framework, or are they merely entrenched statutes that can be amended by a simple majority?',
    'A formal constitutional entrenchment process, or a clear, widely accepted legal consensus on their hierarchical status that transcends political contestation.',
    'If they are merely entrenched statutes, the Supreme Court''s power of invalidation is less legitimate, increasing the constraint''s effective extractiveness and pushing it closer to a Snare. If they are fully constitutional, the judicial supremacy reading is strengthened.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(basic_laws_constitutional_status_ambiguity, conceptual, 'Ambiguity regarding the constitutional status of Basic Laws.').

omega_variable(
    scope_of_judicial_review_ambiguity,
    'Is the Supreme Court''s power of judicial review limited to procedural aspects of legislation, or does it extend to substantive review of the content of laws?',
    'Clear legislative definition of the scope of judicial review, or a consistent, long-standing judicial practice that is accepted by all branches of government.',
    'If review is limited to procedure, the extractiveness from the legislature is lower. If it extends to substantive review, the extractiveness is higher, as the court can substitute its judgment for that of the legislature on policy matters.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scope_of_judicial_review_ambiguity, empirical, 'Ambiguity regarding the scope of judicial review (procedural vs. substantive).').

omega_variable(
    legitimacy_of_judicial_activism,
    'Does the Supreme Court''s assertive interpretation and invalidation of legislation constitute legitimate judicial activism in defense of rights, or an illegitimate overreach into the democratic mandate of the Knesset?',
    'Public opinion shifts, sustained political consensus, or a constitutional reform that redefines the court''s role and powers with broad societal acceptance.',
    'If perceived as illegitimate overreach, the constraint''s resistance increases, and its legitimacy as a coordination mechanism erodes, pushing it closer to a Snare. If seen as legitimate, its Rope-like coordination function is reinforced.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(legitimacy_of_judicial_activism, preference, 'Contestation over the legitimacy of judicial activism.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(basic_law_interpretive_boundary__judicial_supremacy_reading, 1992, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(basi_tr_t1992, basic_law_interpretive_boundary__judicial_supremacy_reading, theater_ratio, 1992, 0.15).
narrative_ontology:measurement(basi_tr_t1998, basic_law_interpretive_boundary__judicial_supremacy_reading, theater_ratio, 1998, 0.13).
narrative_ontology:measurement(basi_tr_t2004, basic_law_interpretive_boundary__judicial_supremacy_reading, theater_ratio, 2004, 0.12).
narrative_ontology:measurement(basi_tr_t2010, basic_law_interpretive_boundary__judicial_supremacy_reading, theater_ratio, 2010, 0.11).
narrative_ontology:measurement(basi_tr_t2016, basic_law_interpretive_boundary__judicial_supremacy_reading, theater_ratio, 2016, 0.1).
narrative_ontology:measurement(basi_tr_t2024, basic_law_interpretive_boundary__judicial_supremacy_reading, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(basi_be_t1992, basic_law_interpretive_boundary__judicial_supremacy_reading, base_extractiveness, 1992, 0.6).
narrative_ontology:measurement(basi_be_t1998, basic_law_interpretive_boundary__judicial_supremacy_reading, base_extractiveness, 1998, 0.68).
narrative_ontology:measurement(basi_be_t2004, basic_law_interpretive_boundary__judicial_supremacy_reading, base_extractiveness, 2004, 0.73).
narrative_ontology:measurement(basi_be_t2010, basic_law_interpretive_boundary__judicial_supremacy_reading, base_extractiveness, 2010, 0.76).
narrative_ontology:measurement(basi_be_t2016, basic_law_interpretive_boundary__judicial_supremacy_reading, base_extractiveness, 2016, 0.78).
narrative_ontology:measurement(basi_be_t2024, basic_law_interpretive_boundary__judicial_supremacy_reading, base_extractiveness, 2024, 0.8).

% Suppression requirement over time
narrative_ontology:measurement(basi_su_t1992, basic_law_interpretive_boundary__judicial_supremacy_reading, suppression_requirement, 1992, 0.7).
narrative_ontology:measurement(basi_su_t1998, basic_law_interpretive_boundary__judicial_supremacy_reading, suppression_requirement, 1998, 0.78).
narrative_ontology:measurement(basi_su_t2004, basic_law_interpretive_boundary__judicial_supremacy_reading, suppression_requirement, 2004, 0.83).
narrative_ontology:measurement(basi_su_t2010, basic_law_interpretive_boundary__judicial_supremacy_reading, suppression_requirement, 2010, 0.86).
narrative_ontology:measurement(basi_su_t2016, basic_law_interpretive_boundary__judicial_supremacy_reading, suppression_requirement, 2016, 0.88).
narrative_ontology:measurement(basi_su_t2024, basic_law_interpretive_boundary__judicial_supremacy_reading, suppression_requirement, 2024, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(basic_law_interpretive_boundary__judicial_supremacy_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(basic_law_interpretive_boundary__judicial_supremacy_reading, basic_law_interpretive_boundary__parliamentary_sovereignty_reading).
narrative_ontology:affects_constraint(basic_law_interpretive_boundary__judicial_supremacy_reading, basic_law_interpretive_boundary__balanced_contestation_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'basic_law_interpretive_boundary' kernel. Its high extractiveness and suppression reflect the structural consequences of judicial supremacy, which differs significantly from sibling readings that emphasize parliamentary sovereignty or a more balanced institutional contestation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
