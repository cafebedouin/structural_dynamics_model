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
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   interests. This reading emerged from judicial interpretation and
 *   political evolution, moderating the initial Crown sovereignty claim but
 *   falling short of full Māori self-determination. It is a tangled rope
 *   because it genuinely coordinates bicultural governance while still
 *   entrenching an asymmetric power dynamic where Māori bear significant
 *   costs in upholding the partnership.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(waitangi_sovereignty_allocation__partnership_reading, 0.45).
domain_priors:suppression_score(waitangi_sovereignty_allocation__partnership_reading, 0.3).
domain_priors:theater_ratio(waitangi_sovereignty_allocation__partnership_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(waitangi_sovereignty_allocation__partnership_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(waitangi_sovereignty_allocation__partnership_reading, suppression_requirement, 0.3).
narrative_ontology:constraint_metric(waitangi_sovereignty_allocation__partnership_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(waitangi_sovereignty_allocation__partnership_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(waitangi_sovereignty_allocation__partnership_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(waitangi_sovereignty_allocation__partnership_reading, tangled_rope).
narrative_ontology:human_readable(waitangi_sovereignty_allocation__partnership_reading, "Treaty of Waitangi: Partnership Reading").
narrative_ontology:topic_domain(waitangi_sovereignty_allocation__partnership_reading, "constitutional_law/indigenous_rights/post_colonial_governance").

domain_priors:requires_active_enforcement(waitangi_sovereignty_allocation__partnership_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(waitangi_sovereignty_allocation__partnership_reading, 'f72f6714-ea04-4777-ada9-01ab446f8db7').
narrative_ontology:cs_kernel_codification('f72f6714-ea04-4777-ada9-01ab446f8db7', fixed_text).
narrative_ontology:cs_authority_grounding('f72f6714-ea04-4777-ada9-01ab446f8db7', lineage).
narrative_ontology:cs_interpretation_layer_present('f72f6714-ea04-4777-ada9-01ab446f8db7').
narrative_ontology:cs_reading_relation('f72f6714-ea04-4777-ada9-01ab446f8db7', waitangi_sovereignty_allocation__crown_sovereignty_reading, coexists_with).
narrative_ontology:cs_reading_relation('f72f6714-ea04-4777-ada9-01ab446f8db7', waitangi_sovereignty_allocation__rangatiratanga_reading, coexists_with).
narrative_ontology:cs_axiom('f72f6714-ea04-4777-ada9-01ab446f8db7', foundational, treaty_as_living_document).
narrative_ontology:cs_axiom_status(treaty_as_living_document, holdable).
narrative_ontology:cs_axiom_grounding('f72f6714-ea04-4777-ada9-01ab446f8db7', treaty_as_living_document, conventional).
narrative_ontology:cs_axiom('f72f6714-ea04-4777-ada9-01ab446f8db7', foundational, active_protection_of_maori_interests).
narrative_ontology:cs_axiom_status(active_protection_of_maori_interests, holdable).
narrative_ontology:cs_axiom_grounding('f72f6714-ea04-4777-ada9-01ab446f8db7', active_protection_of_maori_interests, deontological).
narrative_ontology:cs_reference_frame('f72f6714-ea04-4777-ada9-01ab446f8db7', judicial_recognition_of_treaty_principles).
narrative_ontology:cs_drift_state('f72f6714-ea04-4777-ada9-01ab446f8db7', contemporary_political_discourse, gap(revival_pressure, minor, true)).
narrative_ontology:cs_created_at('f72f6714-ea04-4777-ada9-01ab446f8db7', '').
narrative_ontology:cs_kernel_id(waitangi_sovereignty_allocation__partnership_reading, waitangi_sovereignty_allocation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(waitangi_sovereignty_allocation__partnership_reading, maori_iwi_hapu).
narrative_ontology:constraint_beneficiary(waitangi_sovereignty_allocation__partnership_reading, crown_government).
narrative_ontology:constraint_victim(waitangi_sovereignty_allocation__partnership_reading, maori_iwi_hapu).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(waitangi_sovereignty_allocation__partnership_reading, settler_population).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefits from the recognition of partnership and consultation rights, leading to Treaty settlements and protection of cultural interests. However, also bears the cost of ongoing negotiation, litigation, and the inherent power imbalance in the partnership, often experiencing delays and diluted outcomes.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__partnership_reading, maori_iwi_hapu, beneficiary,
    organized, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(waitangi_sovereignty_allocation__partnership_reading, maori_iwi_hapu, payer).

% Administers the Treaty principles, engaging in consultation and settlement processes. Benefits from maintaining social license and international reputation, and from the stability provided by addressing historical grievances. Constrained by legal and political obligations to uphold the partnership, but retains ultimate legislative authority.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__partnership_reading, crown_government, agenda_setter,
    institutional, generational, constrained, national).

% Interprets the Treaty principles and enforces the Crown's obligations under the partnership reading. Its rulings shape the scope and nature of consultation and protection, acting as a check on Crown power but also operating within the existing constitutional framework.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__partnership_reading, new_zealand_judiciary, agenda_setter,
    institutional, civilizational, analytical, national).

% Contributes to the funding of Treaty settlements through taxation and may experience some policy adjustments due to consultation requirements. Generally benefits from national stability and a reconciled society, but some segments may resist perceived costs or challenges to existing property rights.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__partnership_reading, settler_population, payer,
    organized, biographical, mobile, national).

% Monitors New Zealand's adherence to indigenous rights and self-determination, providing external pressure and validation for the partnership reading. Their reports can influence domestic policy and international perception.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__partnership_reading, international_human_rights_bodies, observer,
    institutional, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a framework for ongoing governance and resource management in a bicultural nation, aiming to reconcile historical grievances and ensure the active protection of Māori interests alongside Crown sovereignty, preventing open conflict.
% TRANSFER_FUNCTION: Transfers land, resources, and financial redress from the Crown to Māori iwi and hapū as part of Treaty settlements, and transfers decision-making influence through consultation processes. It also transfers legitimacy to the Crown's governance by acknowledging Māori rights.
% ABSENT_VOICES: Those who advocate for full Māori self-determination (tino rangatiratanga) as a co-equal sovereignty, rather than a partnership under Crown supremacy, are often marginalized in the current framework. They would argue the partnership reading still entrenches an unequal power dynamic.
% DISAPPEARANCE_RATIONALE: If the partnership reading and its enforcement vanished, the legal and political landscape of New Zealand would be fundamentally destabilized. Treaty settlements would halt, consultation obligations would cease, and the basis for Māori claims would erode, likely leading to significant social unrest and international condemnation.
% FOUNDING_PROBLEM: The original Treaty of Waitangi aimed to establish British sovereignty while protecting Māori rights and property, but textual ambiguities and subsequent colonial practices led to widespread land confiscation, cultural suppression, and a profound breach of trust.
% FOUNDING_PROBLEM_CORROBORATION: Māori iwi and hapū, the Waitangi Tribunal, and international human rights bodies consistently attest that the founding problem of unresolved grievances and ongoing power imbalances remains live, despite significant progress in Treaty settlements. The Crown acknowledges the ongoing nature of reconciliation.
narrative_ontology:disappearance_verdict(waitangi_sovereignty_allocation__partnership_reading, world_rearranges).
narrative_ontology:founding_problem_status(waitangi_sovereignty_allocation__partnership_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(waitangi_sovereignty_allocation__partnership_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(waitangi_sovereignty_allocation__partnership_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(waitangi_sovereignty_allocation__partnership_reading_tests).
:- end_tests(waitangi_sovereignty_allocation__partnership_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness has decreased over time (from 0.65 to 0.45) due to Treaty settlements and increased recognition of Māori rights, but remains substantial because the Crown retains ultimate legislative authority and Māori interests are often accommodated rather than co-determined. Suppression (0.30) is relatively low, reflecting a democratic context with legal avenues for redress, but still present in the structural power imbalance. Theater ratio (0.20) is low, indicating genuine efforts at partnership, though some performative aspects exist in the ongoing negotiation processes.
 *
 * PERSPECTIVAL GAP:
 *   From the Crown's perspective, the partnership reading is a legitimate and evolving framework for bicultural governance, balancing competing interests. From many Māori perspectives, while an improvement over outright denial of rights, it remains a compromise that falls short of the full authority (tino rangatiratanga) promised in the Māori text of the Treaty, thus still entailing significant costs and extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   The Crown Government and Judiciary act as agenda-setters, benefiting from the legitimacy and stability the partnership provides, while also bearing the costs of negotiation and settlement. Māori iwi and hapū are both beneficiaries (receiving redress, having rights protected) and payers (bearing the costs of ongoing struggle, diluted outcomes). The settler population is a payer through taxation for settlements, but also benefits from national stability. International bodies are observers, providing external validation.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate (reconciling historical grievances and establishing bicultural governance) is still live, preventing mandatrophy. The ongoing nature of Treaty settlements and the active role of the Waitangi Tribunal demonstrate that the function has not atrophied, though its effectiveness and equity are continually contested.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    power_asymmetry_in_partnership,
    'Does the ''partnership'' reading genuinely establish an equitable power dynamic, or does it merely moderate Crown supremacy while retaining fundamental asymmetry?',
    'Analysis of decision-making outcomes over time: if Māori interests are consistently overridden or diluted despite consultation, it suggests persistent asymmetry. If co-governance models emerge with genuine shared authority, it suggests equity.',
    'If fundamentally asymmetric, the constraint''s effective extraction for Māori is higher than measured, pushing it closer to a Snare. If genuinely equitable, it moves closer to a Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(power_asymmetry_in_partnership, conceptual, 'Ambiguity of power balance within the partnership framework.').

omega_variable(
    textual_ambiguity_resolution,
    'To what extent does the partnership reading genuinely resolve the textual ambiguities between the English and Māori versions of the Treaty, or does it merely paper over them?',
    'Comparative legal analysis of how specific clauses are interpreted and applied in practice, and whether these interpretations align with both texts or prioritize one over the other.',
    'If it merely papers over ambiguities, the constraint''s stability is lower, and its reliance on active enforcement is higher. If it genuinely synthesizes a coherent meaning, its legitimacy is stronger.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(textual_ambiguity_resolution, conceptual, 'How the partnership reading addresses the Treaty''s textual ambiguities.').

omega_variable(
    parliamentary_sovereignty_limit,
    'Does the ''principles of the Treaty'' doctrine, central to the partnership reading, genuinely constrain parliamentary sovereignty, or is it ultimately subservient to it?',
    'Judicial review outcomes where parliamentary legislation is challenged on Treaty grounds: if courts consistently strike down or modify legislation, it indicates a strong constraint. If Parliament can easily override Treaty principles, the constraint is weak.',
    'If parliamentary sovereignty is genuinely constrained, the Crown''s power is lower, and the partnership is more robust. If it is subservient, the Crown''s power is higher, and the partnership is more precarious.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(parliamentary_sovereignty_limit, empirical, 'The actual limits placed on parliamentary sovereignty by the partnership reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(waitangi_sovereignty_allocation__partnership_reading, 1975, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(wait_tr_t1975, waitangi_sovereignty_allocation__partnership_reading, theater_ratio, 1975, 0.4).
narrative_ontology:measurement(wait_tr_t1990, waitangi_sovereignty_allocation__partnership_reading, theater_ratio, 1990, 0.3).
narrative_ontology:measurement(wait_tr_t2005, waitangi_sovereignty_allocation__partnership_reading, theater_ratio, 2005, 0.25).
narrative_ontology:measurement(wait_tr_t2024, waitangi_sovereignty_allocation__partnership_reading, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(wait_be_t1975, waitangi_sovereignty_allocation__partnership_reading, base_extractiveness, 1975, 0.65).
narrative_ontology:measurement(wait_be_t1990, waitangi_sovereignty_allocation__partnership_reading, base_extractiveness, 1990, 0.55).
narrative_ontology:measurement(wait_be_t2005, waitangi_sovereignty_allocation__partnership_reading, base_extractiveness, 2005, 0.48).
narrative_ontology:measurement(wait_be_t2024, waitangi_sovereignty_allocation__partnership_reading, base_extractiveness, 2024, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(wait_su_t1975, waitangi_sovereignty_allocation__partnership_reading, suppression_requirement, 1975, 0.5).
narrative_ontology:measurement(wait_su_t1990, waitangi_sovereignty_allocation__partnership_reading, suppression_requirement, 1990, 0.4).
narrative_ontology:measurement(wait_su_t2005, waitangi_sovereignty_allocation__partnership_reading, suppression_requirement, 2005, 0.35).
narrative_ontology:measurement(wait_su_t2024, waitangi_sovereignty_allocation__partnership_reading, suppression_requirement, 2024, 0.3).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(waitangi_sovereignty_allocation__partnership_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(waitangi_sovereignty_allocation__partnership_reading, waitangi_sovereignty_allocation__crown_sovereignty_reading).
narrative_ontology:affects_constraint(waitangi_sovereignty_allocation__partnership_reading, waitangi_sovereignty_allocation__rangatiratanga_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'waitangi_sovereignty_allocation' kernel. This 'partnership_reading' emphasizes ongoing bicultural governance and consultation, distinct from the 'crown_sovereignty_reading' (full cession) and 'rangatiratanga_reading' (full Māori authority).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
