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
 *   human_readable: Treaty of Waitangi: Crown-Māori Partnership Reading
 *   domain: constitutional_law/indigenous_rights/post_colonial_governance
 *
 * SUMMARY:
 *   This constraint represents the 'partnership reading' of the Treaty of
 *   Waitangi, which posits an ongoing relationship between the Crown and
 *   Māori requiring good faith consultation and active protection of Māori
 *   interests. This reading emerged significantly after the Treaty's signing,
 *   particularly gaining legal and political traction from the 1970s onwards
 *   with the establishment of the Waitangi Tribunal. It acknowledges textual
 *   ambiguities in the Treaty but interprets it as a foundational document
 *   for a bicultural nation, imposing duties on the Crown. This is one of
 *   three primary readings of the 'waitangi_sovereignty_allocation' kernel.
 *
 * KEY AGENTS:
 *   - crown_government: Agenda setter (institutional/constrained) — implements Treaty obligations
 *   - maori_iwi_hapu: Payer (organized/identity_locked) — bears historical costs, engages in partnership
 *   - new_zealand_judiciary: Agenda setter (institutional/analytical) — interprets Treaty principles
 *   - maori_sovereignty_advocates: Excluded (organized/identity_locked) — critiques partnership as insufficient
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(waitangi_sovereignty_allocation__partnership_reading, 0.55).
domain_priors:suppression_score(waitangi_sovereignty_allocation__partnership_reading, 0.5).
domain_priors:theater_ratio(waitangi_sovereignty_allocation__partnership_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(waitangi_sovereignty_allocation__partnership_reading, extractiveness, 0.55).
narrative_ontology:constraint_metric(waitangi_sovereignty_allocation__partnership_reading, suppression_requirement, 0.5).
narrative_ontology:constraint_metric(waitangi_sovereignty_allocation__partnership_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(waitangi_sovereignty_allocation__partnership_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(waitangi_sovereignty_allocation__partnership_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(waitangi_sovereignty_allocation__partnership_reading, tangled_rope).
narrative_ontology:human_readable(waitangi_sovereignty_allocation__partnership_reading, "Treaty of Waitangi: Crown-Māori Partnership Reading").
narrative_ontology:topic_domain(waitangi_sovereignty_allocation__partnership_reading, "constitutional_law/indigenous_rights/post_colonial_governance").

domain_priors:requires_active_enforcement(waitangi_sovereignty_allocation__partnership_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(waitangi_sovereignty_allocation__partnership_reading, 'a87d6cf2-0107-45e4-83fd-49532720b651').
narrative_ontology:cs_kernel_codification('a87d6cf2-0107-45e4-83fd-49532720b651', fixed_text).
narrative_ontology:cs_authority_grounding('a87d6cf2-0107-45e4-83fd-49532720b651', lineage).
narrative_ontology:cs_interpretation_layer_present('a87d6cf2-0107-45e4-83fd-49532720b651').
narrative_ontology:cs_reading_relation('a87d6cf2-0107-45e4-83fd-49532720b651', waitangi_sovereignty_allocation__crown_sovereignty_reading, coexists_with).
narrative_ontology:cs_reading_relation('a87d6cf2-0107-45e4-83fd-49532720b651', waitangi_sovereignty_allocation__rangatiratanga_reading, coexists_with).
narrative_ontology:cs_axiom('a87d6cf2-0107-45e4-83fd-49532720b651', foundational, treaty_as_living_document).
narrative_ontology:cs_axiom_status(treaty_as_living_document, holdable).
narrative_ontology:cs_axiom_grounding('a87d6cf2-0107-45e4-83fd-49532720b651', treaty_as_living_document, conventional).
narrative_ontology:cs_axiom('a87d6cf2-0107-45e4-83fd-49532720b651', foundational, crown_has_fiduciary_duty_to_maori).
narrative_ontology:cs_axiom_status(crown_has_fiduciary_duty_to_maori, holdable).
narrative_ontology:cs_axiom_grounding('a87d6cf2-0107-45e4-83fd-49532720b651', crown_has_fiduciary_duty_to_maori, deontological).
narrative_ontology:cs_reference_frame('a87d6cf2-0107-45e4-83fd-49532720b651', bicultural_governance_framework).
narrative_ontology:cs_drift_state('a87d6cf2-0107-45e4-83fd-49532720b651', contemporary_era, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('a87d6cf2-0107-45e4-83fd-49532720b651', '').
narrative_ontology:cs_kernel_id(waitangi_sovereignty_allocation__partnership_reading, waitangi_sovereignty_allocation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(waitangi_sovereignty_allocation__partnership_reading, crown_government).
narrative_ontology:constraint_beneficiary(waitangi_sovereignty_allocation__partnership_reading, new_zealand_state).
narrative_ontology:constraint_victim(waitangi_sovereignty_allocation__partnership_reading, maori_iwi_hapu).
narrative_ontology:constraint_victim(waitangi_sovereignty_allocation__partnership_reading, maori_citizens).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(waitangi_sovereignty_allocation__partnership_reading, new_zealand_public).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The executive branch of the New Zealand government, responsible for implementing Treaty principles, consulting with Māori, and negotiating settlements. Benefits from the stability and legitimacy provided by the partnership framework, but is constrained by its obligations.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__partnership_reading, crown_government, agenda_setter,
    institutional, generational, constrained, national).

% Māori tribes and sub-tribes, who bear the historical and ongoing costs of sovereignty transfer, but also engage in the partnership framework to assert their rights, seek redress, and protect their interests. Their identity is deeply tied to their ancestral lands and the Treaty.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__partnership_reading, maori_iwi_hapu, payer,
    organized, generational, identity_locked, national).

% Individual Māori citizens who navigate the New Zealand state, benefiting from some aspects of the partnership (e.g., cultural recognition, specific programs) but still experiencing systemic disadvantages and the ongoing effects of historical injustices.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__partnership_reading, maori_citizens, payer,
    moderate, biographical, constrained, national).

% Interprets the Treaty of Waitangi and its principles, shaping the legal framework of the partnership. Its rulings can compel Crown action and influence policy, acting as a crucial arbiter of the partnership's terms.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__partnership_reading, new_zealand_judiciary, agenda_setter,
    institutional, generational, analytical, national).

% The general population of New Zealand, which benefits from the social cohesion and international reputation derived from addressing historical injustices and fostering a bicultural national identity. Bears diffuse costs of redress but generally supports the partnership in principle.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__partnership_reading, new_zealand_public, beneficiary,
    moderate, biographical, mobile, national).

% A permanent commission of inquiry that makes recommendations on claims relating to the Treaty of Waitangi. It investigates historical grievances and contemporary issues, providing a crucial mechanism for accountability and informing the partnership's evolution.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__partnership_reading, treaty_of_waitangi_tribunal, observer,
    institutional, generational, analytical, national).

% Advocates for full Māori self-determination (tino rangatiratanga) who view the 'partnership' framework as an insufficient compromise that entrenches Crown sovereignty rather than genuinely sharing power. They operate outside the formal partnership structures, often critiquing its limitations.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__partnership_reading, maori_sovereignty_advocates, excluded,
    organized, generational, identity_locked, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(waitangi_sovereignty_allocation__partnership_reading, crown_government).
narrative_ontology:fixing_cost_class(waitangi_sovereignty_allocation__partnership_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a framework for ongoing reconciliation, co-governance, and the protection of Māori interests within the New Zealand state, aiming to resolve historical grievances and build a bicultural nation.
% TRANSFER_FUNCTION: Transfers some sovereign authority and resources from Māori to the Crown, but also imposes reciprocal obligations on the Crown to actively protect Māori interests, consult in good faith, and provide redress for Treaty breaches. It also transfers legitimacy to the Crown's governance.
% ABSENT_VOICES: Māori sovereignty advocates (from the rangatiratanga reading) are structurally excluded from the core 'partnership' framing, as their position challenges the fundamental premise of shared sovereignty that the partnership reading assumes. They would argue for a more radical re-allocation of power.
% DISAPPEARANCE_RATIONALE: If the partnership reading and its associated legal and political structures vanished overnight, the entire constitutional and governance framework of New Zealand would collapse. Māori claims would become unmediated, leading to profound political instability, social unrest, and a crisis of national identity. The state's legitimacy would be severely undermined.
% FOUNDING_PROBLEM: To establish a basis for British settlement and governance in New Zealand while securing Māori rights and authority over their lands, resources, and culture, following the signing of the Treaty of Waitangi in 1840.
% FOUNDING_PROBLEM_CORROBORATION: The New Zealand judiciary, the Waitangi Tribunal, and various international human rights bodies consistently affirm the ongoing relevance and unresolved nature of the Treaty's founding problems, particularly regarding historical grievances and the full realization of Māori rights. This corroboration comes from outside the immediate benefiting parties (Crown government).
narrative_ontology:disappearance_verdict(waitangi_sovereignty_allocation__partnership_reading, world_rearranges).
narrative_ontology:founding_problem_status(waitangi_sovereignty_allocation__partnership_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(waitangi_sovereignty_allocation__partnership_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(waitangi_sovereignty_allocation__partnership_reading, 'none', 1).
narrative_ontology:epsilon_provenance(waitangi_sovereignty_allocation__partnership_reading, 0.55, 'gemini-2.5-flash', 'none', direct).

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
 *   The constraint is classified as a Tangled Rope because it genuinely attempts to coordinate a bicultural relationship and address historical grievances (beneficiaries: Crown, NZ State, NZ Public), but it also involves asymmetric extraction (victims: Māori iwi/hapū, Māori citizens) due to the Crown's retained ultimate sovereignty and the ongoing power imbalance. Active enforcement is required to compel Crown compliance with consultation and protection duties, and to manage the ongoing process of redress. Extractiveness has decreased over time as the partnership framework has matured and settlements have occurred, but it remains substantial due to the inherent power differential. Suppression has similarly decreased, reflecting a shift from overt suppression of Māori rights to a more nuanced management of dissent within the partnership framework. Theater ratio has increased, indicating that while consultation is frequent, it doesn't always lead to substantive changes in Māori outcomes, sometimes serving more as a performative gesture.
 *
 * PERSPECTIVAL GAP:
 *   The Crown government and the New Zealand public tend to view the partnership reading as a successful and legitimate framework for reconciliation, emphasizing the coordination function and the benefits of stability. From the perspective of Māori iwi/hapū and Māori citizens, while the partnership offers avenues for redress and recognition, it often operates with an inherent power imbalance, leading to continued extraction and the need for constant vigilance and resistance to ensure Crown obligations are met. Māori sovereignty advocates view the entire framework as a compromise of fundamental rights.
 *
 * DIRECTIONALITY LOGIC:
 *   The Crown government and the New Zealand state are beneficiaries, as the partnership framework provides legitimacy, stability, and a mechanism for managing historical grievances, albeit with obligations. Māori iwi/hapū and Māori citizens are payers, as they continue to bear the costs of historical injustices and must actively engage to secure their rights within a system where the Crown retains ultimate authority. The New Zealand judiciary acts as an agenda-setter, interpreting and enforcing the terms of the partnership. Māori sovereignty advocates are excluded, as their fundamental challenge to Crown sovereignty falls outside the 'partnership' framing.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as a Tangled Rope prevents mislabeling this complex arrangement as either a pure Rope (ignoring the ongoing extraction and power imbalance) or a pure Snare (ignoring the genuine coordination efforts and the Crown's evolving obligations). The 'live' status of the founding problem, coupled with the 'world_rearranges' disappearance verdict, indicates that the constraint's mandate is still highly relevant, even if its implementation is contested and its benefits are unevenly distributed. The increasing theater ratio, however, suggests a risk of drift towards a Piton if consultation becomes purely performative without substantive outcomes.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    partnership_power_balance,
    'Is the ''partnership'' a genuine sharing of power, or does it primarily serve to legitimize Crown sovereignty while moderating its exercise?',
    'Empirical analysis of decision-making processes: track the proportion of joint decisions where Māori partners have veto power or where Māori interests prevail over Crown preferences, versus instances of mere consultation.',
    'If power sharing is minimal, the constraint''s effective extractiveness for Māori is higher, pushing it closer to a Snare. If genuine power sharing is demonstrated, it reinforces the Rope aspect of the Tangled Rope classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(partnership_power_balance, empirical, 'The actual distribution of power within the Crown-Māori partnership.').

omega_variable(
    consultation_effectiveness,
    'To what extent does Crown consultation with Māori genuinely influence policy outcomes, rather than being a procedural formality?',
    'Case studies tracking specific policy initiatives from initial consultation through final implementation, assessing the degree to which Māori input was incorporated and led to tangible changes.',
    'If consultation is largely performative, the theater_ratio is higher than currently estimated, and the effective suppression of Māori voices is greater, pushing the constraint towards a Piton or Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(consultation_effectiveness, empirical, 'The substantive impact of Crown consultation processes.').

omega_variable(
    textual_ambiguity_resolution,
    'Is the textual ambiguity of the Treaty of Waitangi an inherent feature requiring ongoing interpretation, or a deliberate historical obfuscation that perpetuates Crown dominance?',
    'Historical linguistic analysis of the Treaty''s drafting and contemporary legal-philosophical debate on the nature of foundational documents in post-colonial contexts.',
    'If ambiguity is seen as deliberate obfuscation, the foundational legitimacy of the partnership reading is weakened, potentially shifting its classification towards a Snare by highlighting its extractive origins.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(textual_ambiguity_resolution, conceptual, 'The nature and intent of the Treaty''s textual ambiguities.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(waitangi_sovereignty_allocation__partnership_reading, 1975, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(wait_tr_t1975, waitangi_sovereignty_allocation__partnership_reading, theater_ratio, 1975, 0.2).
narrative_ontology:measurement(wait_tr_t1985, waitangi_sovereignty_allocation__partnership_reading, theater_ratio, 1985, 0.25).
narrative_ontology:measurement(wait_tr_t1995, waitangi_sovereignty_allocation__partnership_reading, theater_ratio, 1995, 0.3).
narrative_ontology:measurement(wait_tr_t2005, waitangi_sovereignty_allocation__partnership_reading, theater_ratio, 2005, 0.35).
narrative_ontology:measurement(wait_tr_t2015, waitangi_sovereignty_allocation__partnership_reading, theater_ratio, 2015, 0.38).
narrative_ontology:measurement(wait_tr_t2025, waitangi_sovereignty_allocation__partnership_reading, theater_ratio, 2025, 0.4).

% Extraction over time
narrative_ontology:measurement(wait_be_t1975, waitangi_sovereignty_allocation__partnership_reading, base_extractiveness, 1975, 0.7).
narrative_ontology:measurement(wait_be_t1985, waitangi_sovereignty_allocation__partnership_reading, base_extractiveness, 1985, 0.65).
narrative_ontology:measurement(wait_be_t1995, waitangi_sovereignty_allocation__partnership_reading, base_extractiveness, 1995, 0.6).
narrative_ontology:measurement(wait_be_t2005, waitangi_sovereignty_allocation__partnership_reading, base_extractiveness, 2005, 0.58).
narrative_ontology:measurement(wait_be_t2015, waitangi_sovereignty_allocation__partnership_reading, base_extractiveness, 2015, 0.56).
narrative_ontology:measurement(wait_be_t2025, waitangi_sovereignty_allocation__partnership_reading, base_extractiveness, 2025, 0.55).

% Suppression requirement over time
narrative_ontology:measurement(wait_su_t1975, waitangi_sovereignty_allocation__partnership_reading, suppression_requirement, 1975, 0.7).
narrative_ontology:measurement(wait_su_t1985, waitangi_sovereignty_allocation__partnership_reading, suppression_requirement, 1985, 0.62).
narrative_ontology:measurement(wait_su_t1995, waitangi_sovereignty_allocation__partnership_reading, suppression_requirement, 1995, 0.55).
narrative_ontology:measurement(wait_su_t2005, waitangi_sovereignty_allocation__partnership_reading, suppression_requirement, 2005, 0.52).
narrative_ontology:measurement(wait_su_t2015, waitangi_sovereignty_allocation__partnership_reading, suppression_requirement, 2015, 0.51).
narrative_ontology:measurement(wait_su_t2025, waitangi_sovereignty_allocation__partnership_reading, suppression_requirement, 2025, 0.5).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(waitangi_sovereignty_allocation__partnership_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(waitangi_sovereignty_allocation__partnership_reading, new_zealand_constitutional_framework).
narrative_ontology:affects_constraint(waitangi_sovereignty_allocation__partnership_reading, maori_land_rights).
narrative_ontology:affects_constraint(waitangi_sovereignty_allocation__partnership_reading, maori_cultural_heritage_protection).
narrative_ontology:affects_constraint(waitangi_sovereignty_allocation__partnership_reading, waitangi_sovereignty_allocation__crown_sovereignty_reading).
narrative_ontology:affects_constraint(waitangi_sovereignty_allocation__partnership_reading, waitangi_sovereignty_allocation__rangatiratanga_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three primary readings of the 'waitangi_sovereignty_allocation' kernel. It focuses on the 'partnership' interpretation, distinct from the 'crown_sovereignty_reading' (full cession) and the 'rangatiratanga_reading' (Māori retention of full authority). All three are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
