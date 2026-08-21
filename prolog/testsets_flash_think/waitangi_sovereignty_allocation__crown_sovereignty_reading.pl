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
 *   constraint_id: waitangi_sovereignty_allocation__crown_sovereignty_reading
 *   human_readable: Crown Sovereignty over Aotearoa (English Article I Reading)
 *   domain: constitutional_law/indigenous_rights/post_colonial_governance
 *
 * SUMMARY:
 *   This constraint story instantiates the 'Crown Sovereignty' reading of the
 *   Treaty of Waitangi, which asserts that the English Article I ceded
 *   complete sovereignty to the British Crown in 1840, thereby establishing
 *   Westminster parliamentary supremacy over Aotearoa (New Zealand). This
 *   interpretation posits that the Crown exercises plenary legislative power
 *   without requiring Māori consent, enables unilateral resource allocation,
 *   and subordinates Māori interests to the will of Parliament. The claimed
 *   type ('rope') reflects the self-serving narrative of establishing a
 *   unified, stable governance framework, while the metrics reflect the
 *   highly extractive and suppressive reality for Māori.
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
narrative_ontology:constraint_metric(waitangi_sovereignty_allocation__crown_sovereignty_reading, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(waitangi_sovereignty_allocation__crown_sovereignty_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(waitangi_sovereignty_allocation__crown_sovereignty_reading, rope).
narrative_ontology:human_readable(waitangi_sovereignty_allocation__crown_sovereignty_reading, "Crown Sovereignty over Aotearoa (English Article I Reading)").
narrative_ontology:topic_domain(waitangi_sovereignty_allocation__crown_sovereignty_reading, "constitutional_law/indigenous_rights/post_colonial_governance").

domain_priors:requires_active_enforcement(waitangi_sovereignty_allocation__crown_sovereignty_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(waitangi_sovereignty_allocation__crown_sovereignty_reading, '3bbe3b56-f0cc-43b2-b25e-5a3912199f08').
narrative_ontology:cs_kernel_codification('3bbe3b56-f0cc-43b2-b25e-5a3912199f08', fixed_text).
narrative_ontology:cs_authority_grounding('3bbe3b56-f0cc-43b2-b25e-5a3912199f08', lineage).
narrative_ontology:cs_interpretation_layer_present('3bbe3b56-f0cc-43b2-b25e-5a3912199f08').
narrative_ontology:cs_reading_relation('3bbe3b56-f0cc-43b2-b25e-5a3912199f08', waitangi_sovereignty_allocation__partnership_reading, forecloses).
narrative_ontology:cs_reading_relation('3bbe3b56-f0cc-43b2-b25e-5a3912199f08', waitangi_sovereignty_allocation__rangatiratanga_reading, forecloses).
narrative_ontology:cs_axiom('3bbe3b56-f0cc-43b2-b25e-5a3912199f08', foundational, parliamentary_supremacy_absolute).
narrative_ontology:cs_axiom_status(parliamentary_supremacy_absolute, holdable).
narrative_ontology:cs_axiom_grounding('3bbe3b56-f0cc-43b2-b25e-5a3912199f08', parliamentary_supremacy_absolute, conventional).
narrative_ontology:cs_axiom('3bbe3b56-f0cc-43b2-b25e-5a3912199f08', foundational, crown_inherits_plenary_sovereignty).
narrative_ontology:cs_axiom_status(crown_inherits_plenary_sovereignty, holdable).
narrative_ontology:cs_axiom_grounding('3bbe3b56-f0cc-43b2-b25e-5a3912199f08', crown_inherits_plenary_sovereignty, conventional).
narrative_ontology:cs_reference_frame('3bbe3b56-f0cc-43b2-b25e-5a3912199f08', unqualified_parliamentary_supremacy).
narrative_ontology:cs_drift_state('3bbe3b56-f0cc-43b2-b25e-5a3912199f08', contemporary_treaty_claims_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('3bbe3b56-f0cc-43b2-b25e-5a3912199f08', '').
narrative_ontology:cs_kernel_id(waitangi_sovereignty_allocation__crown_sovereignty_reading, waitangi_sovereignty_allocation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(waitangi_sovereignty_allocation__crown_sovereignty_reading, new_zealand_crown).
narrative_ontology:constraint_beneficiary(waitangi_sovereignty_allocation__crown_sovereignty_reading, pakeha_settler_government).
narrative_ontology:constraint_beneficiary(waitangi_sovereignty_allocation__crown_sovereignty_reading, non_maori_landowners).
narrative_ontology:constraint_victim(waitangi_sovereignty_allocation__crown_sovereignty_reading, maori_iwi_hapu).
narrative_ontology:constraint_victim(waitangi_sovereignty_allocation__crown_sovereignty_reading, maori_citizens).
narrative_ontology:constraint_vindicates(waitangi_sovereignty_allocation__crown_sovereignty_reading, parliamentary_supremacy_doctrine).
narrative_ontology:constraint_vindicates(waitangi_sovereignty_allocation__crown_sovereignty_reading, crown_prerogative_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The ultimate legal authority, claiming plenary legislative power over Aotearoa based on the English text of Article I of the Treaty of Waitangi. Benefits from the ability to govern unilaterally and allocate resources without requiring Māori consent.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__crown_sovereignty_reading, new_zealand_crown, agenda_setter,
    institutional, generational, arbitrage, national).

% The elected government of New Zealand, which operates under the constitutional framework established by the Crown's interpretation. Benefits from the perceived legitimacy and stability of a singular, supreme parliamentary authority, enabling policy-making and resource management across the entire nation.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__crown_sovereignty_reading, pakeha_settler_government, beneficiary,
    institutional, generational, mobile, national).

% Māori tribes and sub-tribes, whose traditional authority (tino rangatiratanga) over their lands, resources, and people is subordinated by the Crown's claim of sovereignty. Bear the costs of land alienation, resource exploitation, and the imposition of foreign law, while being structurally constrained from exercising self-determination.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__crown_sovereignty_reading, maori_iwi_hapu, payer,
    powerless, generational, identity_locked, local).

% Individual Māori who live under a legal system that often fails to recognize or protect their Treaty rights and cultural practices, leading to socio-economic disparities and ongoing struggles for justice. Their identity is deeply tied to their indigeneity, making 'exit' from the system a form of cultural abandonment.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__crown_sovereignty_reading, maori_citizens, payer,
    powerless, biographical, identity_locked, national).

% Individuals and corporations who hold land titles derived from Crown grants, often originally acquired from Māori under the Crown's asserted sovereignty. Benefit from the security of these titles and the legal framework that underpins them, which would be destabilized by alternative interpretations of the Treaty.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__crown_sovereignty_reading, non_maori_landowners, beneficiary,
    powerful, generational, mobile, national).

% Organizations that monitor and report on human rights compliance, including indigenous rights. They analyze the Crown's actions against international standards and often highlight discrepancies between the Crown's interpretation and the principles of self-determination and partnership.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__crown_sovereignty_reading, international_human_rights_bodies, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(waitangi_sovereignty_allocation__crown_sovereignty_reading, new_zealand_crown).
narrative_ontology:fixing_cost_class(waitangi_sovereignty_allocation__crown_sovereignty_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a single, unified system of governance and law for all inhabitants of New Zealand under the authority of the Westminster Parliament, providing a clear legal framework for administration and resource management.
% TRANSFER_FUNCTION: Transfers ultimate legislative and resource control from Māori to the Crown, enabling unilateral resource allocation, land acquisition, and the imposition of a singular legal system across Aotearoa.
% ABSENT_VOICES: Māori voices asserting tino rangatiratanga (full authority) or a co-equal partnership are systematically marginalized or dismissed within the Crown's legal and political framework, which prioritizes parliamentary supremacy. Their perspectives are often relegated to 'special interest' rather than foundational constitutional claims.
% DISAPPEARANCE_RATIONALE: If this interpretation of Crown sovereignty vanished overnight, the entire legal and political framework of New Zealand, including land ownership, resource management, and legislative authority, would be fundamentally challenged. The nation's constitutional order would require a complete re-evaluation and likely a renegotiation of power between the Crown and Māori.
% FOUNDING_PROBLEM: To establish a clear, singular authority for the governance of New Zealand, particularly to manage British settlement, prevent inter-tribal warfare, and secure British interests, as perceived by the Crown in 1840.
% FOUNDING_PROBLEM_CORROBORATION: The Crown and its legal scholars attest that the problem of establishing stable, unified governance and managing a diverse population remains live. Māori legal scholars, activists, and international legal observers dispute this framing, arguing the original 'problem' was British expansionism and the 'solution' was colonial imposition, not genuine coordination. They contend the founding problem, as framed by the Crown, is largely 'dead' or was a pretext for extraction.
narrative_ontology:disappearance_verdict(waitangi_sovereignty_allocation__crown_sovereignty_reading, world_rearranges).
narrative_ontology:founding_problem_status(waitangi_sovereignty_allocation__crown_sovereignty_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(waitangi_sovereignty_allocation__crown_sovereignty_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
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
 *   Extractiveness is high (0.85) because this reading enables the Crown to unilaterally control land, resources, and legislative power, leading to significant historical and ongoing losses for Māori. Suppression is very high (0.90) as the Crown actively enforces its interpretation through legal and political means, often overriding Māori claims and suppressing resistance. The accessibility collapse for Māori is substantial (0.80) as their traditional governance structures are legally unrecognized. Resistance is high (0.75) due to continuous Māori activism and legal challenges. Theater ratio is moderate (0.40), reflecting increasing performative consultation with Māori over time that often does not genuinely alter the fundamental power imbalance or the Crown's ultimate authority.
 *
 * PERSPECTIVAL GAP:
 *   From the Crown's perspective, this interpretation provides a necessary and legitimate framework for stable governance and national unity (a 'rope'). From the Māori perspective, it is a fundamentally unjust and extractive imposition that denies their inherent sovereignty (a 'snare'). The engine's computation of per-seat classifications will highlight this divergence, showing the Crown as a beneficiary and Māori as victims, despite the 'rope' claim.
 *
 * DIRECTIONALITY LOGIC:
 *   The New Zealand Crown and the Pākehā settler government are the primary beneficiaries, gaining and exercising ultimate authority and control over resources. Non-Māori landowners also benefit from the security of land titles derived from this interpretation. Māori iwi/hapū and Māori citizens are the primary targets/payers, bearing the costs of lost sovereignty, land, and cultural autonomy. International human rights bodies act as analytical observers, assessing the situation against global standards.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    treaty_text_interpretation_ambiguity,
    'To what extent does the English Article I genuinely cede complete sovereignty, given the Māori text of Article II (tino rangatiratanga) and historical context?',
    'Comparative linguistic analysis of 1840s English and Māori legal concepts, historical records of negotiations, and international legal precedents regarding indigenous treaties.',
    'If the Māori text is found to retain substantial Māori authority, this reading''s claim of plenary Crown sovereignty would be fundamentally undermined, shifting its classification towards a Snare or Tangled Rope based on a false premise.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(treaty_text_interpretation_ambiguity, empirical, 'Ambiguity in the Treaty''s original texts regarding sovereignty transfer.').

omega_variable(
    founding_problem_legitimacy,
    'Was the Crown''s ''founding problem'' (establishing singular authority, preventing warfare) a genuine coordination need or a colonial justification for land acquisition and control?',
    'Historical analysis of pre-1840 Māori governance structures, inter-tribal relations, and British colonial policy motivations, drawing on non-Crown-aligned historical accounts.',
    'If the founding problem is found to be primarily a colonial justification, the ''rope'' claim of this reading would be further exposed as a cover story, increasing its effective extractiveness and supporting a Snare classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founding_problem_legitimacy, conceptual, 'Legitimacy of the Crown''s stated reasons for asserting sovereignty.').

omega_variable(
    impact_of_alternative_readings,
    'What would be the structural impact on New Zealand''s governance and resource allocation if the ''partnership_reading'' or ''rangatiratanga_reading'' were adopted as the primary constitutional interpretation?',
    'Legal and political modeling of alternative constitutional frameworks, including co-governance models, Māori self-determination over resources, and a Treaty-based constitution.',
    'Adoption of alternative readings would fundamentally alter the distribution of power and resources, likely reducing Crown extractiveness and suppression, and shifting the constraint towards a more equitable (Rope or Scaffold) or decentralized (Mountain-like for Māori self-governance) structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(impact_of_alternative_readings, preference, 'Consequences of adopting alternative Treaty interpretations.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(waitangi_sovereignty_allocation__crown_sovereignty_reading, 1840, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(wait_tr_t1840, waitangi_sovereignty_allocation__crown_sovereignty_reading, theater_ratio, 1840, 0.1).
narrative_ontology:measurement(wait_tr_t1880, waitangi_sovereignty_allocation__crown_sovereignty_reading, theater_ratio, 1880, 0.15).
narrative_ontology:measurement(wait_tr_t1920, waitangi_sovereignty_allocation__crown_sovereignty_reading, theater_ratio, 1920, 0.2).
narrative_ontology:measurement(wait_tr_t1960, waitangi_sovereignty_allocation__crown_sovereignty_reading, theater_ratio, 1960, 0.3).
narrative_ontology:measurement(wait_tr_t2000, waitangi_sovereignty_allocation__crown_sovereignty_reading, theater_ratio, 2000, 0.45).
narrative_ontology:measurement(wait_tr_t2024, waitangi_sovereignty_allocation__crown_sovereignty_reading, theater_ratio, 2024, 0.4).

% Extraction over time
narrative_ontology:measurement(wait_be_t1840, waitangi_sovereignty_allocation__crown_sovereignty_reading, base_extractiveness, 1840, 0.6).
narrative_ontology:measurement(wait_be_t1880, waitangi_sovereignty_allocation__crown_sovereignty_reading, base_extractiveness, 1880, 0.75).
narrative_ontology:measurement(wait_be_t1920, waitangi_sovereignty_allocation__crown_sovereignty_reading, base_extractiveness, 1920, 0.85).
narrative_ontology:measurement(wait_be_t1960, waitangi_sovereignty_allocation__crown_sovereignty_reading, base_extractiveness, 1960, 0.88).
narrative_ontology:measurement(wait_be_t2000, waitangi_sovereignty_allocation__crown_sovereignty_reading, base_extractiveness, 2000, 0.87).
narrative_ontology:measurement(wait_be_t2024, waitangi_sovereignty_allocation__crown_sovereignty_reading, base_extractiveness, 2024, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(wait_su_t1840, waitangi_sovereignty_allocation__crown_sovereignty_reading, suppression_requirement, 1840, 0.65).
narrative_ontology:measurement(wait_su_t1880, waitangi_sovereignty_allocation__crown_sovereignty_reading, suppression_requirement, 1880, 0.8).
narrative_ontology:measurement(wait_su_t1920, waitangi_sovereignty_allocation__crown_sovereignty_reading, suppression_requirement, 1920, 0.9).
narrative_ontology:measurement(wait_su_t1960, waitangi_sovereignty_allocation__crown_sovereignty_reading, suppression_requirement, 1960, 0.92).
narrative_ontology:measurement(wait_su_t2000, waitangi_sovereignty_allocation__crown_sovereignty_reading, suppression_requirement, 2000, 0.91).
narrative_ontology:measurement(wait_su_t2024, waitangi_sovereignty_allocation__crown_sovereignty_reading, suppression_requirement, 2024, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(waitangi_sovereignty_allocation__crown_sovereignty_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(waitangi_sovereignty_allocation__crown_sovereignty_reading, new_zealand_land_acquisition_laws).
narrative_ontology:affects_constraint(waitangi_sovereignty_allocation__crown_sovereignty_reading, resource_management_act_1991).
narrative_ontology:affects_constraint(waitangi_sovereignty_allocation__crown_sovereignty_reading, maori_language_act_1987).
narrative_ontology:affects_constraint(waitangi_sovereignty_allocation__crown_sovereignty_reading, waitangi_tribunal_jurisdiction).

% DUAL FORMULATION NOTE:
% This constraint is one of three distinct readings of the 'waitangi_sovereignty_allocation' kernel. Its 'complete Crown sovereignty' interpretation directly influences and is contested by the 'partnership_reading' and 'rangatiratanga_reading', which assert different distributions of authority and obligations under the Treaty.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
