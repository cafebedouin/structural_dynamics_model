% ============================================================================
% CONSTRAINT STORY: treaty_authority_cession__rangatiratanga_retention_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_treaty_authority_cession__rangatiratanga_retention_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: treaty_authority_cession__rangatiratanga_retention_reading
 *   human_readable: Treaty of Waitangi: Rangatiratanga Retention Reading
 *   domain: constitutional_law/indigenous_rights/colonial_history
 *
 * SUMMARY:
 *   This constraint represents the 'rangatiratanga retention' reading of the
 *   Treaty of Waitangi, which asserts that the Māori text of the Treaty,
 *   particularly the retention of 'tino rangatiratanga' (full
 *   chieftainship/sovereignty) by hapū and iwi, is paramount. Under this
 *   reading, 'kāwanatanga' (governance) ceded to the Crown is limited to the
 *   right to govern its own subjects, not to exercise full sovereignty over
 *   Māori. The Treaty thus establishes a partnership requiring ongoing
 *   consent and negotiation for legitimate Crown action affecting Māori
 *   interests. This reading has gained significant traction through the
 *   Waitangi Tribunal and subsequent legal developments, shifting the
 *   constraint from a more extractive historical operation towards a genuine
 *   (though still contested) coordination mechanism.
 *
 * KEY AGENTS:
 *   - hapu_iwi: Primary beneficiary (organized/constrained) – retains authority, requires consent
 *   - crown_as_partner: Agenda setter (institutional/constrained) – exercises governance, requires consent
 *   - new_zealand_judiciary: Observer (institutional/analytical) – interprets the Treaty, influences application
 *   - settler_population: Payer (organized/constrained) – may bear costs of land returns or co-governance
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(treaty_authority_cession__rangatiratanga_retention_reading, 0.35).
domain_priors:suppression_score(treaty_authority_cession__rangatiratanga_retention_reading, 0.2).
domain_priors:theater_ratio(treaty_authority_cession__rangatiratanga_retention_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(treaty_authority_cession__rangatiratanga_retention_reading, extractiveness, 0.35).
narrative_ontology:constraint_metric(treaty_authority_cession__rangatiratanga_retention_reading, suppression_requirement, 0.2).
narrative_ontology:constraint_metric(treaty_authority_cession__rangatiratanga_retention_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(treaty_authority_cession__rangatiratanga_retention_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(treaty_authority_cession__rangatiratanga_retention_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(treaty_authority_cession__rangatiratanga_retention_reading, rope).
narrative_ontology:human_readable(treaty_authority_cession__rangatiratanga_retention_reading, "Treaty of Waitangi: Rangatiratanga Retention Reading").
narrative_ontology:topic_domain(treaty_authority_cession__rangatiratanga_retention_reading, "constitutional_law/indigenous_rights/colonial_history").

domain_priors:requires_active_enforcement(treaty_authority_cession__rangatiratanga_retention_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(treaty_authority_cession__rangatiratanga_retention_reading, '111cb3be-1e75-4695-ba08-1c6a01c5df35').
narrative_ontology:cs_kernel_codification('111cb3be-1e75-4695-ba08-1c6a01c5df35', fixed_text).
narrative_ontology:cs_authority_grounding('111cb3be-1e75-4695-ba08-1c6a01c5df35', lineage).
narrative_ontology:cs_interpretation_layer_present('111cb3be-1e75-4695-ba08-1c6a01c5df35').
narrative_ontology:cs_reading_relation('111cb3be-1e75-4695-ba08-1c6a01c5df35', treaty_authority_cession__crown_cession_reading, coexists_with).
narrative_ontology:cs_reading_relation('111cb3be-1e75-4695-ba08-1c6a01c5df35', treaty_authority_cession__retrospective_snare_exposure, influences).
narrative_ontology:cs_axiom('111cb3be-1e75-4695-ba08-1c6a01c5df35', foundational, maori_text_paramount).
narrative_ontology:cs_axiom_status(maori_text_paramount, holdable).
narrative_ontology:cs_axiom_grounding('111cb3be-1e75-4695-ba08-1c6a01c5df35', maori_text_paramount, conventional).
narrative_ontology:cs_axiom('111cb3be-1e75-4695-ba08-1c6a01c5df35', foundational, tino_rangatiratanga_retained).
narrative_ontology:cs_axiom_status(tino_rangatiratanga_retained, holdable).
narrative_ontology:cs_axiom_grounding('111cb3be-1e75-4695-ba08-1c6a01c5df35', tino_rangatiratanga_retained, deontological).
narrative_ontology:cs_reference_frame('111cb3be-1e75-4695-ba08-1c6a01c5df35', original_maori_intent).
narrative_ontology:cs_drift_state('111cb3be-1e75-4695-ba08-1c6a01c5df35', contemporary_legal_discourse, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('111cb3be-1e75-4695-ba08-1c6a01c5df35', '').
narrative_ontology:cs_kernel_id(treaty_authority_cession__rangatiratanga_retention_reading, treaty_authority_cession).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(treaty_authority_cession__rangatiratanga_retention_reading, hapu_iwi).
narrative_ontology:constraint_beneficiary(treaty_authority_cession__rangatiratanga_retention_reading, crown_as_partner).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(treaty_authority_cession__rangatiratanga_retention_reading, settler_population).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% As the collective Māori tribal groups, they are affirmed in their 'tino rangatiratanga' (full chieftainship) over their lands, resources, and people. They benefit from the requirement for Crown consultation and consent, and from the recognition of their self-determination. Their exit options are constrained by their inherent connection to their ancestral lands and identity, but they can resist or litigate.
narrative_ontology:constraint_stakeholder(treaty_authority_cession__rangatiratanga_retention_reading, hapu_iwi, beneficiary,
    organized, generational, constrained, national).

% The New Zealand government, operating under the principle of partnership. It exercises 'kāwanatanga' (governance) but is bound to seek consent and engage in good faith with hapū/iwi on matters affecting Māori. It benefits from the legitimacy derived from upholding the Treaty, but its actions are constrained by the partnership requirements. Its exit options are constrained by international and domestic legal obligations.
narrative_ontology:constraint_stakeholder(treaty_authority_cession__rangatiratanga_retention_reading, crown_as_partner, agenda_setter,
    institutional, generational, constrained, national).

% Interprets the Treaty and its implications for law and policy, particularly through the Waitangi Tribunal and higher courts. Its role is to adjudicate disputes and clarify the legal force of the Treaty, including the rangatiratanga retention reading. It does not directly benefit or pay, but its interpretations shape the constraint's operation.
narrative_ontology:constraint_stakeholder(treaty_authority_cession__rangatiratanga_retention_reading, new_zealand_judiciary, observer,
    institutional, civilizational, analytical, national).

% The non-Māori population of New Zealand. While not directly paying a 'fee', they may experience the 'costs' of this reading through land returns, co-governance arrangements, or shifts in resource allocation. They also benefit from a more just and equitable society, but some may resist changes to the status quo. Their exit options are generally mobile within the national context.
narrative_ontology:constraint_stakeholder(treaty_authority_cession__rangatiratanga_retention_reading, settler_population, payer,
    organized, biographical, mobile, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(treaty_authority_cession__rangatiratanga_retention_reading, diffuse).
narrative_ontology:fixing_cost_class(treaty_authority_cession__rangatiratanga_retention_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To establish a framework for shared governance and partnership between Māori (hapū/iwi) and the Crown, ensuring that Māori retain authority over their affairs while the Crown exercises legitimate governance.
% TRANSFER_FUNCTION: This reading aims to transfer authority and control over Māori lands, resources, and cultural practices back to hapū/iwi, while transferring the Crown's unilateral power to a shared, consensual model of governance.
% ABSENT_VOICES: Those who advocate for a strict 'crown_cession_reading' (English text paramount, full sovereignty ceded) are often marginalized in discussions centered on this reading. They would argue that the Treaty granted full sovereignty to the Crown and that partnership claims undermine national unity and legal certainty.
% DISAPPEARANCE_RATIONALE: If this reading of the Treaty vanished, the legal and political landscape of New Zealand would fundamentally shift. Māori claims to self-determination and resource rights would be severely undermined, leading to widespread social unrest, legal challenges, and a breakdown of the current framework for Crown-Māori relations. The entire constitutional order would need to be re-evaluated.
% FOUNDING_PROBLEM: The founding problem was to establish a basis for British settlement and governance in New Zealand while protecting Māori authority and land rights, preventing uncontrolled land acquisition and inter-tribal conflict, and ensuring a peaceful coexistence between Māori and settlers.
% FOUNDING_PROBLEM_CORROBORATION: Hapū/iwi and the Waitangi Tribunal consistently attest that the core problem of ensuring Māori self-determination and protecting their rights within a bicultural nation remains live. While the Crown acknowledges the Treaty's importance, its full commitment to the partnership model is often contested by Māori, who point to ongoing disparities and unresolved claims. Independent historians and legal scholars corroborate the ongoing nature of these challenges.
narrative_ontology:disappearance_verdict(treaty_authority_cession__rangatiratanga_retention_reading, world_rearranges).
narrative_ontology:founding_problem_status(treaty_authority_cession__rangatiratanga_retention_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(treaty_authority_cession__rangatiratanga_retention_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(treaty_authority_cession__rangatiratanga_retention_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(treaty_authority_cession__rangatiratanga_retention_reading_tests).
:- end_tests(treaty_authority_cession__rangatiratanga_retention_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.35) is moderate, reflecting the ongoing struggle for full implementation of this reading, but significantly lower than historical periods where the Crown asserted full sovereignty. Suppression (0.20) is relatively low, as Māori resistance and advocacy have been effective in challenging unilateral Crown action. Theater ratio (0.10) is low, as the partnership claims, while not perfectly realized, are increasingly backed by substantive legal and political processes. The metrics reflect the period from 1980 to 2020, during which the Waitangi Tribunal's influence grew and this reading gained legal and political weight.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of hapū/iwi, this reading represents a hard-won recognition of their inherent rights and a pathway to genuine partnership, making it a Rope. From the perspective of some settler populations, it may be perceived as an imposition or a form of reverse discrimination, potentially leading to a perception of extraction. The Crown's perspective is complex, balancing its historical claims with evolving legal obligations.
 *
 * DIRECTIONALITY LOGIC:
 *   Hapū/iwi are beneficiaries (d near 0.0) as this reading affirms their authority and requires their consent. The Crown, when acting as a genuine partner, also benefits from the legitimacy and stability derived from a consensual relationship (d near 0.2-0.3). The broader settler population may experience some costs (e.g., land returns, co-governance arrangements) but also benefits from a more just and stable society (d near 0.5-0.6).
 *
 * MANDATROPHY ANALYSIS:
 *   This reading actively combats mandatrophy by reasserting the original intent of the Māori text and challenging the historical drift towards unilateral Crown sovereignty. It seeks to revive the Treaty's function as a living constitutional document for partnership, rather than allowing it to become a Piton of colonial inertia or a Snare of ongoing extraction under a false premise of cession. The ongoing resistance and legal challenges prevent the partnership aspect from atrophying into mere performance.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identification,
    'Is this constraint a valid reading of the Treaty of Waitangi kernel, or is it an aspirational interpretation not fully grounded in historical practice?',
    'Further historical and linguistic analysis of 19th-century Māori legal concepts and their application, as well as judicial precedent from the Waitangi Tribunal and higher courts.',
    'If confirmed as a valid reading, it strengthens the legal basis for Māori self-determination and partnership claims. If deemed aspirational, its legal force is diminished, potentially reclassifying it as a Piton or even a Snare if its partnership claims are used to mask ongoing extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_identification, empirical, 'Ambiguity regarding the historical and legal grounding of the rangatiratanga retention reading.').

omega_variable(
    crown_compliance_ambiguity,
    'To what extent does the Crown genuinely adhere to the partnership and consent requirements implied by this reading, versus merely acknowledging them rhetorically?',
    'Empirical observation of Crown-hapū/iwi negotiations, legislative processes, and resource management decisions, specifically tracking instances of genuine co-governance and veto power exercised by Māori partners.',
    'If Crown compliance is largely rhetorical, the constraint''s effective extractiveness for hapū/iwi is higher, and its classification shifts towards a Tangled Rope or Snare, as the coordination function becomes cover for continued Crown dominance. If compliance is genuine, the Rope classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(crown_compliance_ambiguity, empirical, 'The gap between the Crown''s stated commitment to partnership and its actual practice.').

omega_variable(
    translation_asymmetry_impact,
    'How does the inherent translation asymmetry between the Māori and English texts of the Treaty affect the practical implementation of this reading, particularly regarding land and resource management?',
    'Detailed case studies of specific land claims and resource allocation decisions, analyzing how the differing textual interpretations have been reconciled or exacerbated in practice, and the resulting distribution of benefits and costs.',
    'If the translation asymmetry consistently leads to outcomes where Māori interests are undermined despite the ''rangatiratanga retention'' principle, the constraint operates as a Snare, extracting resources under the guise of a partnership agreement. This would align it more closely with the ''retrospective_snare_exposure'' sibling reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(translation_asymmetry_impact, conceptual, 'The impact of textual divergence on the practical operation of the Treaty.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(treaty_authority_cession__rangatiratanga_retention_reading, 1980, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(trea_tr_t0, treaty_authority_cession__rangatiratanga_retention_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(trea_tr_t20, treaty_authority_cession__rangatiratanga_retention_reading, theater_ratio, 20, 0.12).
narrative_ontology:measurement(trea_tr_t40, treaty_authority_cession__rangatiratanga_retention_reading, theater_ratio, 40, 0.1).

% Extraction over time
narrative_ontology:measurement(trea_be_t0, treaty_authority_cession__rangatiratanga_retention_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(trea_be_t20, treaty_authority_cession__rangatiratanga_retention_reading, base_extractiveness, 20, 0.38).
narrative_ontology:measurement(trea_be_t40, treaty_authority_cession__rangatiratanga_retention_reading, base_extractiveness, 40, 0.35).

% Suppression requirement over time
narrative_ontology:measurement(trea_su_t0, treaty_authority_cession__rangatiratanga_retention_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(trea_su_t20, treaty_authority_cession__rangatiratanga_retention_reading, suppression_requirement, 20, 0.25).
narrative_ontology:measurement(trea_su_t40, treaty_authority_cession__rangatiratanga_retention_reading, suppression_requirement, 40, 0.2).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(treaty_authority_cession__rangatiratanga_retention_reading, identity_coordination).
narrative_ontology:affects_constraint(treaty_authority_cession__rangatiratanga_retention_reading, treaty_authority_cession__crown_cession_reading).
narrative_ontology:affects_constraint(treaty_authority_cession__rangatiratanga_retention_reading, treaty_authority_cession__retrospective_snare_exposure).
narrative_ontology:affects_constraint(treaty_authority_cession__rangatiratanga_retention_reading, maori_land_rights_framework).
narrative_ontology:affects_constraint(treaty_authority_cession__rangatiratanga_retention_reading, resource_management_act_application).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'treaty_authority_cession' kernel, focusing on the Māori text and the retention of rangatiratanga. It directly influences and is influenced by the other readings, particularly in legal and political discourse.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
