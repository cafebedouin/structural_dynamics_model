% ============================================================================
% CONSTRAINT STORY: waitangi_sovereignty_allocation__partnership_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_waitangi_sovereignty_allocation__partnership_reading, []).

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
    narrative_ontology:constraint_vindicates/2,
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
 *   constraint_id: waitangi_sovereignty_allocation__partnership_reading
 *   human_readable: Treaty of Waitangi: Partnership Reading
 *   domain: constitutional_law/indigenous_rights/post_colonial_governance
 *
 * SUMMARY:
 *   This constraint represents the 'partnership reading' of the Treaty of
 *   Waitangi, which posits an ongoing relationship between the Crown and
 *   Māori requiring good faith consultation and active protection of Māori
 *   interests. This reading acknowledges textual ambiguity but emphasizes the
 *   spirit of cooperation and mutual obligation. It is one reading of the
 *   'waitangi_sovereignty_allocation' kernel, distinct from the
 *   'crown_sovereignty_reading' and 'rangatiratanga_reading'. The expected
 *   structural delta for this reading is that consultation requirements
 *   moderate Crown power, Treaty settlements provide redress, and the
 *   principles doctrine constrains but does not override parliamentary
 *   sovereignty.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(waitangi_sovereignty_allocation__partnership_reading, 0.65).
domain_priors:suppression_score(waitangi_sovereignty_allocation__partnership_reading, 0.55).
domain_priors:theater_ratio(waitangi_sovereignty_allocation__partnership_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(waitangi_sovereignty_allocation__partnership_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(waitangi_sovereignty_allocation__partnership_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(waitangi_sovereignty_allocation__partnership_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(waitangi_sovereignty_allocation__partnership_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(waitangi_sovereignty_allocation__partnership_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(waitangi_sovereignty_allocation__partnership_reading, tangled_rope).
narrative_ontology:human_readable(waitangi_sovereignty_allocation__partnership_reading, "Treaty of Waitangi: Partnership Reading").
narrative_ontology:topic_domain(waitangi_sovereignty_allocation__partnership_reading, "constitutional_law/indigenous_rights/post_colonial_governance").

domain_priors:requires_active_enforcement(waitangi_sovereignty_allocation__partnership_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(waitangi_sovereignty_allocation__partnership_reading, 'ffc2fe99-fbdd-4cb6-ad85-4c470520b224').
narrative_ontology:cs_kernel_codification('ffc2fe99-fbdd-4cb6-ad85-4c470520b224', fixed_text).
narrative_ontology:cs_authority_grounding('ffc2fe99-fbdd-4cb6-ad85-4c470520b224', lineage).
narrative_ontology:cs_interpretation_layer_present('ffc2fe99-fbdd-4cb6-ad85-4c470520b224').
narrative_ontology:cs_reading_relation('ffc2fe99-fbdd-4cb6-ad85-4c470520b224', waitangi_sovereignty_allocation__crown_sovereignty_reading, coexists_with).
narrative_ontology:cs_reading_relation('ffc2fe99-fbdd-4cb6-ad85-4c470520b224', waitangi_sovereignty_allocation__rangatiratanga_reading, coexists_with).
narrative_ontology:cs_axiom('ffc2fe99-fbdd-4cb6-ad85-4c470520b224', foundational, treaty_as_living_document).
narrative_ontology:cs_axiom_status(treaty_as_living_document, holdable).
narrative_ontology:cs_axiom_grounding('ffc2fe99-fbdd-4cb6-ad85-4c470520b224', treaty_as_living_document, conventional).
narrative_ontology:cs_axiom('ffc2fe99-fbdd-4cb6-ad85-4c470520b224', foundational, crown_has_kawanatanga_maori_retain_rangatiratanga).
narrative_ontology:cs_axiom_status(crown_has_kawanatanga_maori_retain_rangatiratanga, holdable).
narrative_ontology:cs_axiom_grounding('ffc2fe99-fbdd-4cb6-ad85-4c470520b224', crown_has_kawanatanga_maori_retain_rangatiratanga, deontological).
narrative_ontology:cs_reference_frame('ffc2fe99-fbdd-4cb6-ad85-4c470520b224', bicultural_constitutionalism).
narrative_ontology:cs_drift_state('ffc2fe99-fbdd-4cb6-ad85-4c470520b224', contemporary, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('ffc2fe99-fbdd-4cb6-ad85-4c470520b224', '').
narrative_ontology:cs_kernel_id(waitangi_sovereignty_allocation__partnership_reading, waitangi_sovereignty_allocation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(waitangi_sovereignty_allocation__partnership_reading, new_zealand_crown).
narrative_ontology:constraint_beneficiary(waitangi_sovereignty_allocation__partnership_reading, pakeha_settlers).
narrative_ontology:constraint_victim(waitangi_sovereignty_allocation__partnership_reading, maori_iwi_hapu).
narrative_ontology:constraint_vindicates(waitangi_sovereignty_allocation__partnership_reading, treaty_principles_doctrine).
narrative_ontology:constraint_vindicates(waitangi_sovereignty_allocation__partnership_reading, good_faith_governance).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The sovereign authority of New Zealand, which interprets and enforces the Treaty of Waitangi through its legal and administrative systems. Benefits from the legitimacy and stability provided by the partnership framework, even while making concessions and providing redress.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__partnership_reading, new_zealand_crown, agenda_setter,
    institutional, civilizational, arbitrage, national).

% Māori tribes and sub-tribes who ceded kāwanatanga (governorship) but retained tino rangatiratanga (full authority) over their affairs. They bear the costs of limited sovereignty and ongoing negotiation, but benefit from consultation, redress, and recognition of their rights within the partnership framework.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__partnership_reading, maori_iwi_hapu, payer,
    organized, generational, constrained, national).

% The non-Māori population of New Zealand. They benefit from the stable governance and constitutional framework established by the Treaty, which allows for orderly society and economic activity. They indirectly bear costs through taxes funding Treaty settlements.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__partnership_reading, pakeha_settlers, beneficiary,
    moderate, biographical, mobile, national).

% A permanent commission of inquiry that makes recommendations on claims brought by Māori relating to actions or omissions of the Crown that breach the promises made in the Treaty of Waitangi. It provides an institutional mechanism for addressing grievances within the partnership framework.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__partnership_reading, waitangi_tribunal, observer,
    institutional, biographical, analytical, national).

% Interprets the Treaty of Waitangi and its principles, shaping the legal understanding of the partnership. Its rulings can compel the Crown to act in accordance with Treaty principles, thereby influencing the constraint's operation.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__partnership_reading, new_zealand_judiciary, agenda_setter,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(waitangi_sovereignty_allocation__partnership_reading, new_zealand_crown).
narrative_ontology:fixing_cost_class(waitangi_sovereignty_allocation__partnership_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a framework for ongoing governance and resource management in a bicultural nation, aiming to reconcile Crown sovereignty with Māori rights and interests through principles of partnership, good faith, and active protection.
% TRANSFER_FUNCTION: Transfers a degree of sovereign authority and resource control from Māori to the Crown, in exchange for recognition of Māori rights, consultation, and redress through Treaty settlements and ongoing partnership mechanisms.
% ABSENT_VOICES: A pure 'rangatiratanga_reading' would argue for full Māori self-determination and challenge the Crown's ultimate sovereignty, asserting that Māori never ceded sovereignty. This perspective is present in advocacy but not fully integrated into the dominant legal framework.
% DISAPPEARANCE_RATIONALE: If the partnership reading of the Treaty vanished overnight, the entire constitutional and governance framework of New Zealand would collapse. The legitimacy of Crown authority would be fundamentally challenged, leading to profound political and social instability, and a re-negotiation of fundamental power structures between Māori and the Crown.
% FOUNDING_PROBLEM: To establish a basis for British settlement and governance in New Zealand while recognizing and protecting Māori authority and land rights, preventing conflict, and ensuring orderly development in a bicultural context.
% FOUNDING_PROBLEM_CORROBORATION: Historians, legal scholars, and international indigenous rights bodies corroborate the original intent to establish a relationship between two sovereign entities, though the interpretation of that relationship and the extent to which the founding problem is 'solved' remains contested. Māori advocacy groups consistently highlight ongoing breaches and unresolved issues.
narrative_ontology:disappearance_verdict(waitangi_sovereignty_allocation__partnership_reading, world_rearranges).
narrative_ontology:founding_problem_status(waitangi_sovereignty_allocation__partnership_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(waitangi_sovereignty_allocation__partnership_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(waitangi_sovereignty_allocation__partnership_reading, 'none', 1).
narrative_ontology:epsilon_provenance(waitangi_sovereignty_allocation__partnership_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(waitangi_sovereignty_allocation__partnership_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(waitangi_sovereignty_allocation__partnership_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(waitangi_sovereignty_allocation__partnership_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint is classified as a Tangled Rope because it genuinely coordinates a bicultural governance framework (benefiting the Crown and Pākehā settlers through stability and legitimacy) while simultaneously enabling asymmetric extraction from Māori (who bear the costs of limited sovereignty and ongoing negotiation within a Crown-dominant system). Active enforcement by the Crown's legal and administrative systems is required to maintain this balance. Extractiveness is moderate-high (0.65) as the Crown retains ultimate authority and benefits from the status quo, even with concessions. Suppression is moderate (0.55) as Māori resistance and advocacy are ongoing, but the Crown's power is significant. Theater ratio is moderate (0.40) as consultation can sometimes be performative, but also leads to genuine outcomes.
 *
 * PERSPECTIVAL GAP:
 *   From the Crown's perspective, this reading represents a fair and evolving constitutional arrangement. From many Māori perspectives, it is a compromise that still falls short of true self-determination, with the 'partnership' often feeling like a junior partner role. The engine's classification as Tangled Rope captures this inherent asymmetry, where coordination coexists with extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   The New Zealand Crown is the primary beneficiary and agenda-setter, deriving legitimacy and stable governance from the partnership framework. Pākehā settlers are also beneficiaries, enjoying the stability. Māori iwi and hapū are the primary payers, bearing the costs of limited sovereignty and the ongoing struggle for full self-determination, even as they gain recognition and redress. The Waitangi Tribunal and New Zealand Judiciary act as institutional observers and interpreters, shaping the constraint's application.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sovereignty_allocation_ambiguity,
    'Is the Treaty of Waitangi primarily about cession of sovereignty (Crown reading) or shared authority (partnership/rangatiratanga readings)?',
    'Further constitutional reform, international legal arbitration, or a national referendum on the Treaty''s foundational meaning.',
    'If resolved towards full cession, the partnership reading''s claims of shared authority would be undermined, increasing extraction. If resolved towards full rangatiratanga, the Crown''s claims to ultimate sovereignty would be challenged, decreasing extraction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sovereignty_allocation_ambiguity, conceptual, 'Fundamental ambiguity regarding the allocation of sovereignty by the Treaty.').

omega_variable(
    consultation_efficacy_vs_performance,
    'Is Crown consultation with Māori genuine good faith engagement aimed at shared decision-making, or primarily performative to satisfy legal requirements?',
    'Independent audits of consultation processes, analysis of Māori influence on policy outcomes, and longitudinal studies of Māori satisfaction with engagement.',
    'If consultation is primarily performative, the ''partnership'' aspect of the constraint is weaker, increasing effective extraction. If genuine, it strengthens the coordination function and mitigates extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(consultation_efficacy_vs_performance, empirical, 'The extent to which consultation is substantive versus theatrical.').

omega_variable(
    parliamentary_supremacy_constraint,
    'To what extent do Treaty principles genuinely constrain parliamentary sovereignty, or are they merely interpretive guides for legislation?',
    'Further judicial rulings explicitly limiting parliamentary power based on Treaty principles, or constitutional entrenchment of Treaty principles.',
    'If Treaty principles are found to be a strong constraint, Crown power is moderated, reducing extraction. If they are merely interpretive, Crown power remains largely unchecked, increasing extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(parliamentary_supremacy_constraint, conceptual, 'The actual legal force of Treaty principles against parliamentary sovereignty.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(waitangi_sovereignty_allocation__partnership_reading, 1975, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(wait_tr_t1975, waitangi_sovereignty_allocation__partnership_reading, theater_ratio, 1975, 0.2).
narrative_ontology:measurement(wait_tr_t1985, waitangi_sovereignty_allocation__partnership_reading, theater_ratio, 1985, 0.28).
narrative_ontology:measurement(wait_tr_t1995, waitangi_sovereignty_allocation__partnership_reading, theater_ratio, 1995, 0.34).
narrative_ontology:measurement(wait_tr_t2005, waitangi_sovereignty_allocation__partnership_reading, theater_ratio, 2005, 0.38).
narrative_ontology:measurement(wait_tr_t2015, waitangi_sovereignty_allocation__partnership_reading, theater_ratio, 2015, 0.4).
narrative_ontology:measurement(wait_tr_t2025, waitangi_sovereignty_allocation__partnership_reading, theater_ratio, 2025, 0.4).

% Extraction over time
narrative_ontology:measurement(wait_be_t1975, waitangi_sovereignty_allocation__partnership_reading, base_extractiveness, 1975, 0.55).
narrative_ontology:measurement(wait_be_t1985, waitangi_sovereignty_allocation__partnership_reading, base_extractiveness, 1985, 0.6).
narrative_ontology:measurement(wait_be_t1995, waitangi_sovereignty_allocation__partnership_reading, base_extractiveness, 1995, 0.62).
narrative_ontology:measurement(wait_be_t2005, waitangi_sovereignty_allocation__partnership_reading, base_extractiveness, 2005, 0.64).
narrative_ontology:measurement(wait_be_t2015, waitangi_sovereignty_allocation__partnership_reading, base_extractiveness, 2015, 0.65).
narrative_ontology:measurement(wait_be_t2025, waitangi_sovereignty_allocation__partnership_reading, base_extractiveness, 2025, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(wait_su_t1975, waitangi_sovereignty_allocation__partnership_reading, suppression_requirement, 1975, 0.45).
narrative_ontology:measurement(wait_su_t1985, waitangi_sovereignty_allocation__partnership_reading, suppression_requirement, 1985, 0.5).
narrative_ontology:measurement(wait_su_t1995, waitangi_sovereignty_allocation__partnership_reading, suppression_requirement, 1995, 0.52).
narrative_ontology:measurement(wait_su_t2005, waitangi_sovereignty_allocation__partnership_reading, suppression_requirement, 2005, 0.54).
narrative_ontology:measurement(wait_su_t2015, waitangi_sovereignty_allocation__partnership_reading, suppression_requirement, 2015, 0.55).
narrative_ontology:measurement(wait_su_t2025, waitangi_sovereignty_allocation__partnership_reading, suppression_requirement, 2025, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(waitangi_sovereignty_allocation__partnership_reading, identity_coordination).
narrative_ontology:affects_constraint(waitangi_sovereignty_allocation__partnership_reading, new_zealand_land_rights_regime).
narrative_ontology:affects_constraint(waitangi_sovereignty_allocation__partnership_reading, maori_resource_management).
narrative_ontology:affects_constraint(waitangi_sovereignty_allocation__partnership_reading, crown_sovereignty_reading).
narrative_ontology:affects_constraint(waitangi_sovereignty_allocation__partnership_reading, rangatiratanga_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'waitangi_sovereignty_allocation' kernel, each representing a distinct structural claim about the Treaty of Waitangi. They are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
