% ============================================================================
% CONSTRAINT STORY: waitangi_sovereignty_allocation__crown_sovereignty_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_waitangi_sovereignty_allocation__crown_sovereignty_reading, []).

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
 *   constraint_id: waitangi_sovereignty_allocation__crown_sovereignty_reading
 *   human_readable: Treaty of Waitangi: Crown Sovereignty Reading
 *   domain: constitutional_law/indigenous_rights/post_colonial_governance
 *
 * SUMMARY:
 *   This constraint story instantiates the 'Crown Sovereignty Reading' of the
 *   Treaty of Waitangi, which interprets English Article I as ceding complete
 *   sovereignty to the British Crown, thereby establishing Westminster
 *   parliamentary supremacy over all of New Zealand. This reading implies the
 *   Crown exercises plenary legislative power without requiring Māori
 *   consent, leading to unilateral resource allocation and the subordination
 *   of Māori interests to parliamentary will. It is a highly contested
 *   interpretation, particularly by Māori, who assert that the Māori text of
 *   the Treaty (Article II) retained tino rangatiratanga (full authority) for
 *   Māori.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(waitangi_sovereignty_allocation__crown_sovereignty_reading, 0.85).
domain_priors:suppression_score(waitangi_sovereignty_allocation__crown_sovereignty_reading, 0.9).
domain_priors:theater_ratio(waitangi_sovereignty_allocation__crown_sovereignty_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(waitangi_sovereignty_allocation__crown_sovereignty_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(waitangi_sovereignty_allocation__crown_sovereignty_reading, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(waitangi_sovereignty_allocation__crown_sovereignty_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(waitangi_sovereignty_allocation__crown_sovereignty_reading, accessibility_collapse, 0.88).
narrative_ontology:constraint_metric(waitangi_sovereignty_allocation__crown_sovereignty_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(waitangi_sovereignty_allocation__crown_sovereignty_reading, snare).
narrative_ontology:human_readable(waitangi_sovereignty_allocation__crown_sovereignty_reading, "Treaty of Waitangi: Crown Sovereignty Reading").
narrative_ontology:topic_domain(waitangi_sovereignty_allocation__crown_sovereignty_reading, "constitutional_law/indigenous_rights/post_colonial_governance").

domain_priors:requires_active_enforcement(waitangi_sovereignty_allocation__crown_sovereignty_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(waitangi_sovereignty_allocation__crown_sovereignty_reading, '357b3e28-e7d0-4bed-b0c3-cac1e6057bf1').
narrative_ontology:cs_kernel_codification('357b3e28-e7d0-4bed-b0c3-cac1e6057bf1', fixed_text).
narrative_ontology:cs_authority_grounding('357b3e28-e7d0-4bed-b0c3-cac1e6057bf1', lineage).
narrative_ontology:cs_interpretation_layer_present('357b3e28-e7d0-4bed-b0c3-cac1e6057bf1').
narrative_ontology:cs_reading_relation('357b3e28-e7d0-4bed-b0c3-cac1e6057bf1', waitangi_sovereignty_allocation__partnership_reading, forecloses).
narrative_ontology:cs_reading_relation('357b3e28-e7d0-4bed-b0c3-cac1e6057bf1', waitangi_sovereignty_allocation__rangatiratanga_reading, forecloses).
narrative_ontology:cs_axiom('357b3e28-e7d0-4bed-b0c3-cac1e6057bf1', foundational, crown_plenary_sovereignty).
narrative_ontology:cs_axiom_status(crown_plenary_sovereignty, holdable).
narrative_ontology:cs_axiom_grounding('357b3e28-e7d0-4bed-b0c3-cac1e6057bf1', crown_plenary_sovereignty, conventional).
narrative_ontology:cs_axiom('357b3e28-e7d0-4bed-b0c3-cac1e6057bf1', secondary, maori_subordination_to_parliament).
narrative_ontology:cs_axiom_status(maori_subordination_to_parliament, holdable).
narrative_ontology:cs_axiom_grounding('357b3e28-e7d0-4bed-b0c3-cac1e6057bf1', maori_subordination_to_parliament, conventional).
narrative_ontology:cs_reference_frame('357b3e28-e7d0-4bed-b0c3-cac1e6057bf1', westminster_parliamentary_supremacy_framework).
narrative_ontology:cs_drift_state('357b3e28-e7d0-4bed-b0c3-cac1e6057bf1', contemporary_treaty_settlement_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('357b3e28-e7d0-4bed-b0c3-cac1e6057bf1', '').
narrative_ontology:cs_kernel_id(waitangi_sovereignty_allocation__crown_sovereignty_reading, waitangi_sovereignty_allocation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(waitangi_sovereignty_allocation__crown_sovereignty_reading, new_zealand_crown).
narrative_ontology:constraint_beneficiary(waitangi_sovereignty_allocation__crown_sovereignty_reading, non_maori_settlers).
narrative_ontology:constraint_victim(waitangi_sovereignty_allocation__crown_sovereignty_reading, maori_iwi_hapu).
narrative_ontology:constraint_victim(waitangi_sovereignty_allocation__crown_sovereignty_reading, maori_citizens).
narrative_ontology:constraint_vindicates(waitangi_sovereignty_allocation__crown_sovereignty_reading, westminster_parliamentary_supremacy_doctrine).
narrative_ontology:constraint_vindicates(waitangi_sovereignty_allocation__crown_sovereignty_reading, crown_plenary_sovereignty_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% As the sovereign power, the Crown (Parliament and Executive) exercises plenary legislative authority over all of New Zealand, including Māori lands and resources, without requiring Māori consent. This reading justifies unilateral resource allocation and policy decisions.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__crown_sovereignty_reading, new_zealand_crown, agenda_setter,
    institutional, generational, arbitrage, national).

% Subject to the plenary legislative power of the Crown, Māori tribes and sub-tribes lose effective control over their lands, resources, and self-determination. Their traditional authority (tino rangatiratanga) is subordinated to parliamentary will, leading to cultural and economic disadvantage. Exit means abandoning their ancestral lands and identity.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__crown_sovereignty_reading, maori_iwi_hapu, payer,
    powerless, generational, identity_locked, national).

% Benefit from the legal certainty and resource allocation decisions made by the Crown under this reading. Access to land and resources for economic development is facilitated, and their interests are prioritized within the unitary state structure.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__crown_sovereignty_reading, non_maori_settlers, beneficiary,
    powerful, generational, mobile, national).

% Historically, the judiciary largely upheld the Crown sovereignty reading, interpreting the Treaty in a way that affirmed parliamentary supremacy. While contemporary courts acknowledge Treaty principles, this reading still influences their interpretive framework, particularly regarding the limits of judicial review over parliamentary action.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__crown_sovereignty_reading, new_zealand_judiciary, agenda_setter,
    institutional, generational, constrained, national).

% As individuals, Māori citizens are subject to the same laws as all New Zealanders, but this reading denies their collective rights to self-determination and resource control, leading to systemic inequities. Their identity is deeply tied to their indigeneity and ancestral lands, making 'exit' from the system a profound loss.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__crown_sovereignty_reading, maori_citizens, payer,
    moderate, biographical, identity_locked, national).

% Monitor New Zealand's compliance with international indigenous rights standards. They critically assess the impact of Crown sovereignty on Māori self-determination and advocate for greater recognition of Māori rights, but have no direct enforcement power within New Zealand's domestic legal system.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__crown_sovereignty_reading, international_human_rights_bodies, observer,
    institutional, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a single, unified legal and governance system for all inhabitants of New Zealand under the authority of the British Crown, ensuring order, stability, and the efficient administration of the territory.
% TRANSFER_FUNCTION: Transfers complete legislative and resource control from Māori to the New Zealand Crown, enabling unilateral governance, resource allocation, and the establishment of a settler-dominated state.
% ABSENT_VOICES: Māori voices asserting retained tino rangatiratanga (full authority) or a partnership model were historically, and often continue to be, marginalized or dismissed within the dominant legal and political framework that prioritizes Crown sovereignty. Their perspectives are often treated as challenges to the constitutional order rather than legitimate claims within it.
% DISAPPEARANCE_RATIONALE: If the principle of Westminster parliamentary supremacy and plenary Crown sovereignty over all of Aotearoa vanished overnight, the entire constitutional and legal framework of New Zealand would collapse. This would necessitate a fundamental re-negotiation of power, land ownership, and resource allocation, likely leading to a radical restructuring of the state along Treaty lines, potentially involving Māori self-governance.
% FOUNDING_PROBLEM: To establish British sovereignty over New Zealand, secure land for British settlement, and bring Māori under British law to prevent inter-tribal warfare and protect Māori from unscrupulous settlers (as claimed by the Crown at the time).
% FOUNDING_PROBLEM_CORROBORATION: The Crown historically asserted these problems were live and justified its actions. However, Māori and many contemporary historians dispute the extent to which these were genuine problems for Māori or merely pretexts for colonization. Independent historical analysis from outside the benefiting parties supports the view that the 'founding problem' was largely a justification for asserting control, rather than a neutral assessment of Māori needs.
narrative_ontology:disappearance_verdict(waitangi_sovereignty_allocation__crown_sovereignty_reading, world_rearranges).
narrative_ontology:founding_problem_status(waitangi_sovereignty_allocation__crown_sovereignty_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(waitangi_sovereignty_allocation__crown_sovereignty_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(waitangi_sovereignty_allocation__crown_sovereignty_reading, 'none', 1).
narrative_ontology:epsilon_provenance(waitangi_sovereignty_allocation__crown_sovereignty_reading, 0.85, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(waitangi_sovereignty_allocation__crown_sovereignty_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(waitangi_sovereignty_allocation__crown_sovereignty_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(waitangi_sovereignty_allocation__crown_sovereignty_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The high extractiveness (0.85) reflects the significant transfer of land, resources, and decision-making power from Māori to the Crown and non-Māori settlers under this interpretation. Suppression (0.90) is high because the persistence of this reading relies on actively overriding or dismissing Māori claims to self-determination and enforcing Crown authority through legal and political means. The accessibility collapse (0.88) is severe, as this reading structurally denies alternatives to Crown rule. Resistance (0.75) is substantial, reflecting ongoing Māori activism, legal challenges, and political movements. The theater ratio (0.40) indicates that while some Crown actions may appear to engage with Māori concerns (e.g., limited consultation), the underlying power structure remains largely unchanged, with performative gestures masking continued unilateral control.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the Crown and many non-Māori, this reading represents the legitimate and necessary foundation of the New Zealand state, ensuring unified governance and stability. From the Māori perspective, it is a foundational act of dispossession and ongoing colonization, denying their inherent rights and authority. The engine's computation of per-seat classifications will highlight this divergence, showing the constraint as a Snare for Māori and a Beneficiary-aligned structure for the Crown.
 *
 * DIRECTIONALITY LOGIC:
 *   The New Zealand Crown is the primary beneficiary and agenda-setter, collecting the gains of plenary sovereignty. Non-Māori settlers also benefit from the resource allocation and legal certainty this reading provides. Māori iwi/hapū and Māori citizens are the primary targets and payers, bearing the costs of lost self-determination, land, and resources. The New Zealand Judiciary, while an agenda-setter in interpretation, is also constrained by the constitutional framework this reading reinforces. International human rights bodies act as observers, scrutinizing the impacts without direct enforcement power.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading, by asserting plenary Crown sovereignty, effectively frames any Māori assertion of tino rangatiratanga as a challenge to the legitimate constitutional order, rather than a valid claim within a shared framework. The high extractiveness and suppression metrics, coupled with the 'snare' classification, prevent mislabeling this unilateral power as mere 'coordination' for the common good, exposing the underlying extractive mechanism.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    treaty_text_interpretation_ambiguity,
    'Does the English Article I of the Treaty of Waitangi genuinely cede complete sovereignty, or is this an interpretation that maximizes Crown power over a more ambiguous text?',
    'Comparative linguistic analysis of 19th-century legal and diplomatic texts, and historical analysis of the intentions of the British drafters and Māori signatories, particularly regarding the nuances of ''kāwanatanga'' (governorship) versus ''sovereignty''.',
    'If the text is found to be genuinely ambiguous or to not cede complete sovereignty, the ''crown_sovereignty_reading'' loses its textual grounding, weakening its legitimacy and supporting alternative readings like ''partnership'' or ''rangatiratanga''. This would reduce the constraint''s effective extractiveness and suppression.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(treaty_text_interpretation_ambiguity, empirical, 'Ambiguity in the Treaty''s English text regarding sovereignty cession.').

omega_variable(
    legitimacy_of_unilateral_power,
    'Is the Crown''s exercise of plenary legislative power without Māori consent a legitimate constitutional arrangement, or is it an ongoing violation of indigenous rights?',
    'International legal precedent on indigenous self-determination (e.g., UNDRIP), evolving domestic jurisprudence on Treaty principles, and a shift in public and political consensus regarding the moral basis of the New Zealand state.',
    'If deemed illegitimate, the constraint''s moral and political authority would collapse, increasing resistance and potentially leading to constitutional reform that reallocates power. This would fundamentally alter the constraint''s type from Snare towards a more equitable coordination mechanism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legitimacy_of_unilateral_power, conceptual, 'Contestation over the moral and legal legitimacy of unilateral Crown power.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(waitangi_sovereignty_allocation__crown_sovereignty_reading, 1840, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(wait_tr_t1840, waitangi_sovereignty_allocation__crown_sovereignty_reading, theater_ratio, 1840, 0.1).
narrative_ontology:measurement(wait_tr_t1890, waitangi_sovereignty_allocation__crown_sovereignty_reading, theater_ratio, 1890, 0.2).
narrative_ontology:measurement(wait_tr_t1940, waitangi_sovereignty_allocation__crown_sovereignty_reading, theater_ratio, 1940, 0.3).
narrative_ontology:measurement(wait_tr_t1990, waitangi_sovereignty_allocation__crown_sovereignty_reading, theater_ratio, 1990, 0.45).
narrative_ontology:measurement(wait_tr_t2024, waitangi_sovereignty_allocation__crown_sovereignty_reading, theater_ratio, 2024, 0.4).

% Extraction over time
narrative_ontology:measurement(wait_be_t1840, waitangi_sovereignty_allocation__crown_sovereignty_reading, base_extractiveness, 1840, 0.75).
narrative_ontology:measurement(wait_be_t1890, waitangi_sovereignty_allocation__crown_sovereignty_reading, base_extractiveness, 1890, 0.85).
narrative_ontology:measurement(wait_be_t1940, waitangi_sovereignty_allocation__crown_sovereignty_reading, base_extractiveness, 1940, 0.9).
narrative_ontology:measurement(wait_be_t1990, waitangi_sovereignty_allocation__crown_sovereignty_reading, base_extractiveness, 1990, 0.88).
narrative_ontology:measurement(wait_be_t2024, waitangi_sovereignty_allocation__crown_sovereignty_reading, base_extractiveness, 2024, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(wait_su_t1840, waitangi_sovereignty_allocation__crown_sovereignty_reading, suppression_requirement, 1840, 0.7).
narrative_ontology:measurement(wait_su_t1890, waitangi_sovereignty_allocation__crown_sovereignty_reading, suppression_requirement, 1890, 0.85).
narrative_ontology:measurement(wait_su_t1940, waitangi_sovereignty_allocation__crown_sovereignty_reading, suppression_requirement, 1940, 0.92).
narrative_ontology:measurement(wait_su_t1990, waitangi_sovereignty_allocation__crown_sovereignty_reading, suppression_requirement, 1990, 0.88).
narrative_ontology:measurement(wait_su_t2024, waitangi_sovereignty_allocation__crown_sovereignty_reading, suppression_requirement, 2024, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(waitangi_sovereignty_allocation__crown_sovereignty_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(waitangi_sovereignty_allocation__crown_sovereignty_reading, new_zealand_resource_management_act).
narrative_ontology:affects_constraint(waitangi_sovereignty_allocation__crown_sovereignty_reading, maori_land_court_jurisdiction).
narrative_ontology:affects_constraint(waitangi_sovereignty_allocation__crown_sovereignty_reading, waitangi_sovereignty_allocation__partnership_reading).
narrative_ontology:affects_constraint(waitangi_sovereignty_allocation__crown_sovereignty_reading, waitangi_sovereignty_allocation__rangatiratanga_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three distinct readings of the 'waitangi_sovereignty_allocation' kernel. Its high extractiveness and suppression contrast sharply with the 'partnership_reading' and 'rangatiratanga_reading', which posit shared or retained Māori authority. All three are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
