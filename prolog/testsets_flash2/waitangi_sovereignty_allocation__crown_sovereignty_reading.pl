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
 *   human_readable: Waitangi Sovereignty Allocation (Crown Sovereignty Reading)
 *   domain: constitutional_law/indigenous_rights/post_colonial_governance
 *
 * SUMMARY:
 *   This constraint represents the 'Crown Sovereignty' reading of the Treaty
 *   of Waitangi, where the English Article I is interpreted as ceding
 *   complete sovereignty to the British Crown, establishing Westminster
 *   parliamentary supremacy in New Zealand. This reading asserts the Crown's
 *   plenary legislative power without requiring Māori consent, leading to
 *   unilateral resource allocation and the subordination of Māori interests
 *   to parliamentary will. This is one reading of a contested kernel
 *   (waitangi_sovereignty_allocation), with sibling readings
 *   ('partnership_reading' and 'rangatiratanga_reading') offering alternative
 *   interpretations of the Treaty's intent and effect.
 *
 * KEY AGENTS:
 *   - new_zealand_crown: Agenda setter (institutional/arbitrage) — claims plenary power
 *   - pakeha_settlers: Beneficiary (organized/mobile) — benefits from Crown's power
 *   - maori_iwi_hapu: Payer (organized/identity_locked) — bears costs, struggles for self-determination
 *   - maori_citizens: Payer (powerless/constrained) — bears costs, limited influence
 *   - international_human_rights_bodies: Observer (institutional/analytical) — monitors compliance
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(waitangi_sovereignty_allocation__crown_sovereignty_reading, 0.85).
domain_priors:suppression_score(waitangi_sovereignty_allocation__crown_sovereignty_reading, 0.75).
domain_priors:theater_ratio(waitangi_sovereignty_allocation__crown_sovereignty_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(waitangi_sovereignty_allocation__crown_sovereignty_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(waitangi_sovereignty_allocation__crown_sovereignty_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(waitangi_sovereignty_allocation__crown_sovereignty_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(waitangi_sovereignty_allocation__crown_sovereignty_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(waitangi_sovereignty_allocation__crown_sovereignty_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(waitangi_sovereignty_allocation__crown_sovereignty_reading, snare).
narrative_ontology:human_readable(waitangi_sovereignty_allocation__crown_sovereignty_reading, "Waitangi Sovereignty Allocation (Crown Sovereignty Reading)").
narrative_ontology:topic_domain(waitangi_sovereignty_allocation__crown_sovereignty_reading, "constitutional_law/indigenous_rights/post_colonial_governance").

domain_priors:requires_active_enforcement(waitangi_sovereignty_allocation__crown_sovereignty_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(waitangi_sovereignty_allocation__crown_sovereignty_reading, '00b2e667-484d-435e-a8a3-5fe026113622').
narrative_ontology:cs_kernel_codification('00b2e667-484d-435e-a8a3-5fe026113622', fixed_text).
narrative_ontology:cs_authority_grounding('00b2e667-484d-435e-a8a3-5fe026113622', lineage).
narrative_ontology:cs_interpretation_layer_present('00b2e667-484d-435e-a8a3-5fe026113622').
narrative_ontology:cs_reading_relation('00b2e667-484d-435e-a8a3-5fe026113622', waitangi_sovereignty_allocation__partnership_reading, coexists_with).
narrative_ontology:cs_reading_relation('00b2e667-484d-435e-a8a3-5fe026113622', waitangi_sovereignty_allocation__rangatiratanga_reading, coexists_with).
narrative_ontology:cs_axiom('00b2e667-484d-435e-a8a3-5fe026113622', foundational, parliamentary_supremacy_plenary_power).
narrative_ontology:cs_axiom_status(parliamentary_supremacy_plenary_power, holdable).
narrative_ontology:cs_axiom_grounding('00b2e667-484d-435e-a8a3-5fe026113622', parliamentary_supremacy_plenary_power, conventional).
narrative_ontology:cs_axiom('00b2e667-484d-435e-a8a3-5fe026113622', foundational, english_text_governs_treaty_interpretation).
narrative_ontology:cs_axiom_status(english_text_governs_treaty_interpretation, holdable).
narrative_ontology:cs_axiom_grounding('00b2e667-484d-435e-a8a3-5fe026113622', english_text_governs_treaty_interpretation, conventional).
narrative_ontology:cs_reference_frame('00b2e667-484d-435e-a8a3-5fe026113622', westminster_parliamentary_supremacy_1840).
narrative_ontology:cs_drift_state('00b2e667-484d-435e-a8a3-5fe026113622', contemporary_post_colonial_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('00b2e667-484d-435e-a8a3-5fe026113622', '').
narrative_ontology:cs_kernel_id(waitangi_sovereignty_allocation__crown_sovereignty_reading, waitangi_sovereignty_allocation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(waitangi_sovereignty_allocation__crown_sovereignty_reading, new_zealand_crown).
narrative_ontology:constraint_beneficiary(waitangi_sovereignty_allocation__crown_sovereignty_reading, pakeha_settlers).
narrative_ontology:constraint_victim(waitangi_sovereignty_allocation__crown_sovereignty_reading, maori_iwi_hapu).
narrative_ontology:constraint_victim(waitangi_sovereignty_allocation__crown_sovereignty_reading, maori_citizens).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% As the successor to the British Crown, it claims and exercises plenary legislative power over all of New Zealand, including Māori lands and resources, based on the English text of the Treaty. It benefits from unilateral control over resource allocation and policy-making, with no requirement for Māori consent.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__crown_sovereignty_reading, new_zealand_crown, agenda_setter,
    institutional, generational, arbitrage, national).

% Benefited historically and continue to benefit from the Crown's assertion of sovereignty, which enabled land acquisition, resource exploitation, and the establishment of a Westminster-style parliamentary system. Their interests are prioritized by the Crown's legislative agenda.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__crown_sovereignty_reading, pakeha_settlers, beneficiary,
    organized, generational, mobile, national).

% Bear the costs of the Crown's asserted sovereignty, experiencing loss of land, resources, and self-determination. Their traditional authority (tino rangatiratanga) is subordinated to parliamentary supremacy, leading to ongoing grievances and a struggle for recognition. Their identity is deeply tied to their ancestral lands and cultural practices, making 'exit' from the system a form of cultural annihilation.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__crown_sovereignty_reading, maori_iwi_hapu, payer,
    organized, generational, identity_locked, regional).

% Experience the effects of Crown sovereignty through socio-economic disparities, cultural erosion, and limited political influence. They are subject to laws and policies enacted without their full consent, often impacting their well-being and cultural practices. Exit options are limited by their status as citizens within the Crown's jurisdiction.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__crown_sovereignty_reading, maori_citizens, payer,
    powerless, biographical, constrained, national).

% Monitor New Zealand's compliance with international indigenous rights standards, often critiquing the Crown's interpretation of the Treaty and its impact on Māori self-determination. Their observations exert moral and reputational pressure but lack direct enforcement power.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__crown_sovereignty_reading, international_human_rights_bodies, observer,
    institutional, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Established a single, unified legal and administrative system for the entire territory of New Zealand, facilitating governance and resource management under a single authority.
% TRANSFER_FUNCTION: Transfers plenary legislative power and control over all land and natural resources from Māori (as understood by their traditional authority) to the New Zealand Crown, enabling the Crown to allocate these resources and govern without requiring Māori consent.
% ABSENT_VOICES: The full expression of Māori tino rangatiratanga (absolute sovereignty) as understood by Māori signatories is absent from the Crown's interpretation, as are the voices of those who would advocate for a co-sovereign or self-determining Māori nation within New Zealand.
% DISAPPEARANCE_RATIONALE: If the Crown's claim to complete sovereignty based on the English Article I vanished, the entire constitutional and legal framework of New Zealand would collapse. Land ownership, resource management, and legislative authority would become immediately contested, leading to a fundamental re-negotiation of power between Māori and the Crown, and a complete re-ordering of the state.
% FOUNDING_PROBLEM: The British Crown sought to establish a legitimate basis for colonial settlement and governance in New Zealand, to protect Māori from unchecked European expansion, and to secure its strategic interests in the region.
% FOUNDING_PROBLEM_CORROBORATION: The Crown maintains that its sovereignty was essential for establishing law and order and protecting all citizens. Māori scholars and leaders, supported by historical analysis and the Waitangi Tribunal findings (an independent body), argue that the founding problem was solved in a way that fundamentally undermined Māori authority and led to ongoing injustice, making the 'solution' itself the problem.
narrative_ontology:disappearance_verdict(waitangi_sovereignty_allocation__crown_sovereignty_reading, world_rearranges).
narrative_ontology:founding_problem_status(waitangi_sovereignty_allocation__crown_sovereignty_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(waitangi_sovereignty_allocation__crown_sovereignty_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
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
 *   The extractiveness is high (0.85) because this reading enables the Crown to unilaterally control vast resources and legislative power, which Māori would otherwise retain under their own interpretation of the Treaty. Suppression (0.75) is significant, as the Crown actively enforces its claim through legal and political means, often overriding Māori challenges. The theater ratio (0.20) is relatively low, as the Crown's actions are largely direct assertions of power rather than purely performative, though some rhetoric about 'one nation' or 'equal citizens' can be seen as theatrical cover for the underlying power dynamic. Resistance (0.70) is high, reflecting ongoing Māori activism, legal challenges, and political organizing against this interpretation.
 *
 * PERSPECTIVAL GAP:
 *   From the Crown's perspective, this reading provides a stable, legitimate basis for governance and national unity. From Māori perspectives, it is a fundamentally unjust and extractive imposition that denies their inherent rights and authority. The engine's classification will highlight this divergence, showing a Snare from Māori seats and potentially a Rope or Tangled Rope from the Crown's seat, depending on the full metric profile.
 *
 * DIRECTIONALITY LOGIC:
 *   The New Zealand Crown is the primary beneficiary, gaining full legislative and resource control (low d). Pakeha settlers are also beneficiaries, as their historical and contemporary interests are served by this interpretation. Māori iwi/hapū and Māori citizens are the primary targets, experiencing the direct costs of lost sovereignty and resources (high d), with identity_locked exit for iwi/hapū due to their deep cultural ties to land and self-determination.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading prevents mislabeling extraction as coordination by explicitly identifying the beneficiaries (Crown, Pakeha settlers) and victims (Māori) of the unilateral power assertion. The high extractiveness and suppression, coupled with active resistance, clearly indicate a Snare, rather than a legitimate coordination mechanism, from the perspective of those bearing the costs. The 'founding problem' is contested, indicating that the original justification for this allocation of sovereignty is no longer universally accepted, further supporting a Snare classification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    treaty_text_ambiguity,
    'Is the difference in sovereignty allocation between the English and Māori texts of the Treaty of Waitangi a genuine ambiguity, or a deliberate misrepresentation by the Crown?',
    'Further historical and linguistic analysis of 19th-century Māori and English legal concepts of sovereignty, and examination of the Crown''s negotiating intentions at the time of signing.',
    'If deliberate misrepresentation, the extractiveness and suppression of the Crown Sovereignty reading are amplified, as its foundation rests on a deceptive act. If genuine ambiguity, the conceptual basis for the Snare is slightly softened, though the practical effects remain extractive.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(treaty_text_ambiguity, empirical, 'Ambiguity vs. misrepresentation in Treaty texts.').

omega_variable(
    legitimacy_of_unilateral_power,
    'Can a claim to complete sovereignty, derived from a contested colonial treaty, be considered legitimate in a post-colonial context without the ongoing consent of the indigenous population?',
    'International legal precedent on indigenous rights and self-determination, and the evolving constitutional conventions within New Zealand regarding the Treaty''s status.',
    'If deemed illegitimate, the Crown Sovereignty reading''s foundational authority is undermined, shifting its classification closer to a pure Snare or even a Piton if its persistence becomes purely inertial. If deemed legitimate, its claimed type as a foundational constitutional principle is strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legitimacy_of_unilateral_power, conceptual, 'Legitimacy of Crown''s unilateral sovereignty claim.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression of Māori self-determination structural (legal barriers, resource control) or internalized (cognitive patterns, historical trauma)?',
    'Post-exit suppression trajectory: if Māori self-determination efforts continue to face internal barriers after structural impediments are removed, reclassify as partially internalized.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — Māori communities carry the suppression with them after structural barriers are removed, making the Snare more insidious.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism in Māori self-determination.').


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
narrative_ontology:measurement(wait_tr_t1960, waitangi_sovereignty_allocation__crown_sovereignty_reading, theater_ratio, 1960, 0.25).
narrative_ontology:measurement(wait_tr_t2000, waitangi_sovereignty_allocation__crown_sovereignty_reading, theater_ratio, 2000, 0.22).
narrative_ontology:measurement(wait_tr_t2024, waitangi_sovereignty_allocation__crown_sovereignty_reading, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(wait_be_t1840, waitangi_sovereignty_allocation__crown_sovereignty_reading, base_extractiveness, 1840, 0.6).
narrative_ontology:measurement(wait_be_t1880, waitangi_sovereignty_allocation__crown_sovereignty_reading, base_extractiveness, 1880, 0.75).
narrative_ontology:measurement(wait_be_t1920, waitangi_sovereignty_allocation__crown_sovereignty_reading, base_extractiveness, 1920, 0.8).
narrative_ontology:measurement(wait_be_t1960, waitangi_sovereignty_allocation__crown_sovereignty_reading, base_extractiveness, 1960, 0.85).
narrative_ontology:measurement(wait_be_t2000, waitangi_sovereignty_allocation__crown_sovereignty_reading, base_extractiveness, 2000, 0.88).
narrative_ontology:measurement(wait_be_t2024, waitangi_sovereignty_allocation__crown_sovereignty_reading, base_extractiveness, 2024, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(wait_su_t1840, waitangi_sovereignty_allocation__crown_sovereignty_reading, suppression_requirement, 1840, 0.5).
narrative_ontology:measurement(wait_su_t1880, waitangi_sovereignty_allocation__crown_sovereignty_reading, suppression_requirement, 1880, 0.7).
narrative_ontology:measurement(wait_su_t1920, waitangi_sovereignty_allocation__crown_sovereignty_reading, suppression_requirement, 1920, 0.75).
narrative_ontology:measurement(wait_su_t1960, waitangi_sovereignty_allocation__crown_sovereignty_reading, suppression_requirement, 1960, 0.8).
narrative_ontology:measurement(wait_su_t2000, waitangi_sovereignty_allocation__crown_sovereignty_reading, suppression_requirement, 2000, 0.78).
narrative_ontology:measurement(wait_su_t2024, waitangi_sovereignty_allocation__crown_sovereignty_reading, suppression_requirement, 2024, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
