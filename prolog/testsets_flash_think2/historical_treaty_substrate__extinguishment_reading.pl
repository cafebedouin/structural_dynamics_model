% ============================================================================
% CONSTRAINT STORY: historical_treaty_substrate__extinguishment_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_historical_treaty_substrate__extinguishment_reading, []).

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
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: historical_treaty_substrate__extinguishment_reading
 *   human_readable: Historical Treaty Substrate: Extinguishment Reading
 *   domain: legal/political/indigenous_law
 *
 * SUMMARY:
 *   This constraint represents the 'extinguishment reading' of historical
 *   treaties between Indigenous nations and settler states. It posits that
 *   Indigenous parties ceded full territorial sovereignty in exchange for
 *   defined reserves and payments, thereby extinguishing their inherent
 *   rights and establishing the settler state as the sole legitimate
 *   authority over the ceded lands. This reading is highly contested by
 *   Indigenous nations and international law, who view treaties as ongoing,
 *   nation-to-nation agreements or pacts for shared stewardship. The metrics
 *   reflect the high extraction and suppression inherent in this
 *   interpretation, which relies on state power to enforce its claims.
 *
 * KEY AGENTS:
 *   - Settler State: Primary agenda-setter and beneficiary (institutional/arbitrage) — claims full territorial control.
 *   - Settler Population: Primary beneficiary (powerful/mobile) — benefits from land access and resource development.
 *   - Indigenous Nations: Primary target/payer (powerless/trapped) — lost sovereignty, confined to reserves.
 *   - Indigenous Peoples: Primary target/payer (powerless/identity_locked) — bear social/economic costs of lost land.
 *   - Legal Scholars (Extinguishment): Observer/advocate (analytical/analytical) — reinforce this reading.
 *   - International Human Rights Bodies: Excluded (institutional/analytical) — challenge extinguishment but lack direct power.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(historical_treaty_substrate__extinguishment_reading, 0.88).
domain_priors:suppression_score(historical_treaty_substrate__extinguishment_reading, 0.92).
domain_priors:theater_ratio(historical_treaty_substrate__extinguishment_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(historical_treaty_substrate__extinguishment_reading, extractiveness, 0.88).
narrative_ontology:constraint_metric(historical_treaty_substrate__extinguishment_reading, suppression_requirement, 0.92).
narrative_ontology:constraint_metric(historical_treaty_substrate__extinguishment_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(historical_treaty_substrate__extinguishment_reading, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(historical_treaty_substrate__extinguishment_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(historical_treaty_substrate__extinguishment_reading, tangled_rope).
narrative_ontology:human_readable(historical_treaty_substrate__extinguishment_reading, "Historical Treaty Substrate: Extinguishment Reading").
narrative_ontology:topic_domain(historical_treaty_substrate__extinguishment_reading, "legal/political/indigenous_law").

domain_priors:requires_active_enforcement(historical_treaty_substrate__extinguishment_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(historical_treaty_substrate__extinguishment_reading, '591c21b8-4d5b-4614-be54-7e0b0f9c3a4f').
narrative_ontology:cs_kernel_codification('591c21b8-4d5b-4614-be54-7e0b0f9c3a4f', fixed_text).
narrative_ontology:cs_authority_grounding('591c21b8-4d5b-4614-be54-7e0b0f9c3a4f', lineage).
narrative_ontology:cs_interpretation_layer_present('591c21b8-4d5b-4614-be54-7e0b0f9c3a4f').
narrative_ontology:cs_reading_relation('591c21b8-4d5b-4614-be54-7e0b0f9c3a4f', historical_treaty_substrate__nation_to_nation_reading, forecloses).
narrative_ontology:cs_reading_relation('591c21b8-4d5b-4614-be54-7e0b0f9c3a4f', historical_treaty_substrate__stewardship_reading, forecloses).
narrative_ontology:cs_axiom('591c21b8-4d5b-4614-be54-7e0b0f9c3a4f', foundational, indigenous_sovereignty_extinguishable).
narrative_ontology:cs_axiom_status(indigenous_sovereignty_extinguishable, holdable).
narrative_ontology:cs_axiom_grounding('591c21b8-4d5b-4614-be54-7e0b0f9c3a4f', indigenous_sovereignty_extinguishable, conventional).
narrative_ontology:cs_axiom('591c21b8-4d5b-4614-be54-7e0b0f9c3a4f', foundational, treaties_as_property_deeds).
narrative_ontology:cs_axiom_status(treaties_as_property_deeds, holdable).
narrative_ontology:cs_axiom_grounding('591c21b8-4d5b-4614-be54-7e0b0f9c3a4f', treaties_as_property_deeds, conventional).
narrative_ontology:cs_reference_frame('591c21b8-4d5b-4614-be54-7e0b0f9c3a4f', colonial_land_transfer_paradigm).
narrative_ontology:cs_drift_state('591c21b8-4d5b-4614-be54-7e0b0f9c3a4f', contemporary_indigenous_rights_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('591c21b8-4d5b-4614-be54-7e0b0f9c3a4f', '').
narrative_ontology:cs_kernel_id(historical_treaty_substrate__extinguishment_reading, historical_treaty_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(historical_treaty_substrate__extinguishment_reading, settler_state).
narrative_ontology:constraint_beneficiary(historical_treaty_substrate__extinguishment_reading, settler_population).
narrative_ontology:constraint_victim(historical_treaty_substrate__extinguishment_reading, indigenous_nations).
narrative_ontology:constraint_victim(historical_treaty_substrate__extinguishment_reading, indigenous_peoples).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(historical_treaty_substrate__extinguishment_reading, indigenous_peoples).
narrative_ontology:constraint_vindicates(historical_treaty_substrate__extinguishment_reading, terra_nullius_doctrine).
narrative_ontology:constraint_vindicates(historical_treaty_substrate__extinguishment_reading, crown_sovereignty_doctrine).
narrative_ontology:constraint_vindicates(historical_treaty_substrate__extinguishment_reading, doctrine_of_discovery).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The sovereign entity that claims ultimate jurisdiction over all ceded territories. It interprets treaties as completed land transactions, granting it full control and extinguishing Indigenous title, while offering limited, defined rights in return. It benefits from vast land and resource access.
narrative_ontology:constraint_stakeholder(historical_treaty_substrate__extinguishment_reading, settler_state, agenda_setter,
    institutional, generational, arbitrage, national).

% Citizens and corporations within the settler state who benefit from the legal clarity of land title, enabling settlement, resource development, and economic activity on lands claimed through these treaties. They generally accept the state's interpretation of extinguishment.
narrative_ontology:constraint_stakeholder(historical_treaty_substrate__extinguishment_reading, settler_population, beneficiary,
    powerful, biographical, mobile, national).

% The original inhabitants and sovereign entities who entered into treaties. Under the extinguishment reading, they are deemed to have ceded their inherent sovereignty and territorial rights, being confined to reserves and limited treaty rights. They bear the primary cost of lost land, resources, and self-determination.
narrative_ontology:constraint_stakeholder(historical_treaty_substrate__extinguishment_reading, indigenous_nations, payer,
    powerless, civilizational, trapped, local).

% Individuals and communities within Indigenous nations who live with the direct consequences of the extinguishment reading. While they may receive some treaty benefits (annuities, reserve lands), these are seen as inadequate compensation for the loss of ancestral lands and self-governance. Their identity is deeply tied to their traditional territories.
narrative_ontology:constraint_stakeholder(historical_treaty_substrate__extinguishment_reading, indigenous_peoples, payer,
    powerless, generational, identity_locked, local).
narrative_ontology:stakeholder_secondary_role(historical_treaty_substrate__extinguishment_reading, indigenous_peoples, beneficiary).

% Legal academics and practitioners who interpret historical treaties through the lens of property law, emphasizing the transfer of title and the extinguishment of Indigenous sovereignty. Their work reinforces the legal framework of the settler state.
narrative_ontology:constraint_stakeholder(historical_treaty_substrate__extinguishment_reading, legal_scholars_extinguishment, observer,
    analytical, biographical, analytical, universal).

% Organizations and legal frameworks (e.g., UNDRIP) that advocate for Indigenous self-determination and inherent rights, often challenging the concept of extinguishment. While they offer a critical perspective, they lack direct enforcement power over the settler state's domestic legal interpretations.
narrative_ontology:constraint_stakeholder(historical_treaty_substrate__extinguishment_reading, international_human_rights_bodies, excluded,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(historical_treaty_substrate__extinguishment_reading, settler_state).
narrative_ontology:fixing_cost_class(historical_treaty_substrate__extinguishment_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To provide legal certainty and facilitate the orderly transfer of vast tracts of land from Indigenous control to settler jurisdiction, enabling colonial expansion, resource extraction, and the establishment of a new state's property regime.
% TRANSFER_FUNCTION: Transfers ultimate territorial sovereignty and resource rights from Indigenous nations to the settler state, in exchange for limited, defined reserve lands, annuities, and specific rights (e.g., hunting/fishing) for Indigenous parties.
% ABSENT_VOICES: Indigenous legal traditions, concepts of shared stewardship, and inherent sovereignty were largely excluded from the framing of treaties as property transactions. International human rights law, which would challenge the premise of extinguishment, was also absent or ignored in the historical context and continues to be marginalized by this reading.
% DISAPPEARANCE_RATIONALE: If the extinguishment reading vanished overnight, the legal foundation for much of the settler state's territorial claims would be undermined. This would trigger widespread land claims, necessitate renegotiation of resource agreements, and fundamentally reorder constitutional and property law, leading to a massive re-evaluation of Indigenous-settler relations.
% FOUNDING_PROBLEM: To resolve the 'problem' of Indigenous title to land, which impeded settler expansion and resource development, by legally extinguishing Indigenous sovereignty and transferring land to the Crown for settlement and economic exploitation.
% FOUNDING_PROBLEM_CORROBORATION: Settler governments, historical legal documents, and some legal scholars attest to this as the founding problem, framing it as a necessary step for nation-building. Indigenous nations, supported by legal anthropologists and international law, dispute this, arguing the problem was one of colonial land appropriation and denial of inherent rights, not genuine legal clarification.
narrative_ontology:disappearance_verdict(historical_treaty_substrate__extinguishment_reading, world_rearranges).
narrative_ontology:founding_problem_status(historical_treaty_substrate__extinguishment_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(historical_treaty_substrate__extinguishment_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(historical_treaty_substrate__extinguishment_reading, 'none', 1).
narrative_ontology:epsilon_provenance(historical_treaty_substrate__extinguishment_reading, 0.88, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(historical_treaty_substrate__extinguishment_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(historical_treaty_substrate__extinguishment_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(historical_treaty_substrate__extinguishment_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is very high (0.88) because this reading asserts the transfer of vast, inherent Indigenous sovereignty for limited, often inadequate, compensation. Suppression is also very high (0.92) as the settler state actively enforces this interpretation through legal and political means, often against significant Indigenous resistance, and suppresses alternative readings. Accessibility collapse is high (0.90) because this reading legally collapses alternatives to settler sovereignty. Resistance is high (0.75) due to ongoing Indigenous legal challenges and activism. Theater ratio is moderate (0.45) and rising, reflecting the increasing performative aspects of 'honoring' treaties while denying their original intent or ongoing significance, as the legal fiction becomes more apparent under scrutiny.
 *
 * PERSPECTIVAL GAP:
 *   The settler state experiences this constraint as a legitimate, foundational act of nation-building, providing legal certainty and enabling economic prosperity. Indigenous nations experience it as an ongoing act of dispossession and cultural erasure, a fundamental injustice that denies their inherent sovereignty and self-determination. The engine will compute these divergent classifications from the structural data, highlighting the deep conflict embedded in this reading.
 *
 * DIRECTIONALITY LOGIC:
 *   The settler state and population are clear beneficiaries, gaining vast land and resources. Indigenous nations and peoples are the primary targets, losing sovereignty and land. Legal scholars supporting this view are analytical observers. International human rights bodies are excluded, as their perspective directly challenges the core premise of extinguishment and is not integrated into the settler state's legal framework.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is this constraint accurately representing the ''extinguishment_reading'' of the ''historical_treaty_substrate'' kernel?',
    'Comparison with historical legal judgments, government policy documents, and academic interpretations that explicitly advocate for or rely upon the extinguishment principle.',
    'If not, the classification of this specific reading would be inaccurate, potentially misrepresenting the structural dynamics of the broader treaty kernel contest.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Confirms this constraint as the specified kernel reading.').

omega_variable(
    sovereignty_definition_ambiguity,
    'Is ''sovereignty'' in the context of historical treaties a transferable property that can be ceded, or an inherent, inalienable right of Indigenous nations?',
    'Legal and philosophical analysis of Indigenous legal traditions, international law (e.g., UNDRIP), and comparative constitutional theory regarding the nature of Indigenous self-determination.',
    'If sovereignty is inalienable, the extinguishment reading''s core premise is fundamentally flawed, reclassifying it as a Snare built on a legal fiction. If it is transferable, the reading''s internal coherence is strengthened, though its ethical implications remain contested.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sovereignty_definition_ambiguity, conceptual, 'Ambiguity in the nature of sovereignty in treaty contexts.').

omega_variable(
    consent_validity_ambiguity,
    'Was Indigenous consent to the ''extinguishment'' of sovereignty truly free, informed, and uncoerced, given power imbalances and cultural differences in understanding?',
    'Historical and anthropological research into the specific circumstances of treaty negotiations, including linguistic analysis, cultural context, and evidence of duress or misrepresentation.',
    'If consent was invalid, the legal legitimacy of the extinguishment reading collapses, supporting its reclassification as a Snare or even a Mountain of coercion rather than a legitimate (if extractive) Tangled Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(consent_validity_ambiguity, empirical, 'Validity of Indigenous consent to sovereignty extinguishment.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(historical_treaty_substrate__extinguishment_reading, 1700, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hist_tr_t1700, historical_treaty_substrate__extinguishment_reading, theater_ratio, 1700, 0.1).
narrative_ontology:measurement(hist_tr_t1780, historical_treaty_substrate__extinguishment_reading, theater_ratio, 1780, 0.15).
narrative_ontology:measurement(hist_tr_t1860, historical_treaty_substrate__extinguishment_reading, theater_ratio, 1860, 0.25).
narrative_ontology:measurement(hist_tr_t1940, historical_treaty_substrate__extinguishment_reading, theater_ratio, 1940, 0.35).
narrative_ontology:measurement(hist_tr_t2020, historical_treaty_substrate__extinguishment_reading, theater_ratio, 2020, 0.45).

% Extraction over time
narrative_ontology:measurement(hist_be_t1700, historical_treaty_substrate__extinguishment_reading, base_extractiveness, 1700, 0.75).
narrative_ontology:measurement(hist_be_t1780, historical_treaty_substrate__extinguishment_reading, base_extractiveness, 1780, 0.8).
narrative_ontology:measurement(hist_be_t1860, historical_treaty_substrate__extinguishment_reading, base_extractiveness, 1860, 0.85).
narrative_ontology:measurement(hist_be_t1940, historical_treaty_substrate__extinguishment_reading, base_extractiveness, 1940, 0.9).
narrative_ontology:measurement(hist_be_t2020, historical_treaty_substrate__extinguishment_reading, base_extractiveness, 2020, 0.88).

% Suppression requirement over time
narrative_ontology:measurement(hist_su_t1700, historical_treaty_substrate__extinguishment_reading, suppression_requirement, 1700, 0.7).
narrative_ontology:measurement(hist_su_t1780, historical_treaty_substrate__extinguishment_reading, suppression_requirement, 1780, 0.78).
narrative_ontology:measurement(hist_su_t1860, historical_treaty_substrate__extinguishment_reading, suppression_requirement, 1860, 0.85).
narrative_ontology:measurement(hist_su_t1940, historical_treaty_substrate__extinguishment_reading, suppression_requirement, 1940, 0.92).
narrative_ontology:measurement(hist_su_t2020, historical_treaty_substrate__extinguishment_reading, suppression_requirement, 2020, 0.92).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(historical_treaty_substrate__extinguishment_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(historical_treaty_substrate__extinguishment_reading, historical_treaty_substrate__nation_to_nation_reading).
narrative_ontology:affects_constraint(historical_treaty_substrate__extinguishment_reading, historical_treaty_substrate__stewardship_reading).
narrative_ontology:affects_constraint(historical_treaty_substrate__extinguishment_reading, settler_property_law).
narrative_ontology:affects_constraint(historical_treaty_substrate__extinguishment_reading, resource_extraction_permitting).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'historical_treaty_substrate' kernel. It represents the interpretation that treaties extinguished Indigenous sovereignty, contrasting with the 'nation_to_nation_reading' and 'stewardship_reading' which assert ongoing Indigenous rights.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
