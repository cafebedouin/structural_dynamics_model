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
 *   constraint_id: historical_treaty_substrate__extinguishment_reading
 *   human_readable: Historical Treaty Substrate: Extinguishment Reading
 *   domain: legal_anthropology/indigenous_law/comparative_constitutional_theory
 *
 * SUMMARY:
 *   This constraint story instantiates the 'extinguishment reading' of
 *   historical treaties between Indigenous nations and settler states. This
 *   reading asserts that Indigenous parties ceded full territorial
 *   sovereignty in exchange for defined reserves and payments, thereby
 *   extinguishing their original title and jurisdiction. It is a foundational
 *   legal premise for many settler states, enabling their claims to land and
 *   resources, but is fiercely contested by Indigenous nations who view
 *   treaties as ongoing, relational pacts.
 *
 * KEY AGENTS:
 *   - settler_state: Primary beneficiary and agenda-setter (institutional/arbitrage)
 *   - settler_population: Secondary beneficiary (powerful/mobile)
 *   - indigenous_nations: Primary target/victim (powerless/identity_locked)
 *   - indigenous_peoples: Primary target/victim (powerless/trapped)
 *   - legal_scholars_extinguishment: Observer/proponent (analytical/analytical)
 *   - international_human_rights_bodies: Observer/challenger (institutional/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(historical_treaty_substrate__extinguishment_reading, 0.9).
domain_priors:suppression_score(historical_treaty_substrate__extinguishment_reading, 0.85).
domain_priors:theater_ratio(historical_treaty_substrate__extinguishment_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(historical_treaty_substrate__extinguishment_reading, extractiveness, 0.9).
narrative_ontology:constraint_metric(historical_treaty_substrate__extinguishment_reading, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(historical_treaty_substrate__extinguishment_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(historical_treaty_substrate__extinguishment_reading, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(historical_treaty_substrate__extinguishment_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(historical_treaty_substrate__extinguishment_reading, snare).
narrative_ontology:human_readable(historical_treaty_substrate__extinguishment_reading, "Historical Treaty Substrate: Extinguishment Reading").
narrative_ontology:topic_domain(historical_treaty_substrate__extinguishment_reading, "legal_anthropology/indigenous_law/comparative_constitutional_theory").

domain_priors:requires_active_enforcement(historical_treaty_substrate__extinguishment_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(historical_treaty_substrate__extinguishment_reading, 'b4312033-7ead-4d79-82f6-483c226e95c7').
narrative_ontology:cs_kernel_codification('b4312033-7ead-4d79-82f6-483c226e95c7', fixed_text).
narrative_ontology:cs_authority_grounding('b4312033-7ead-4d79-82f6-483c226e95c7', lineage).
narrative_ontology:cs_interpretation_layer_present('b4312033-7ead-4d79-82f6-483c226e95c7').
narrative_ontology:cs_reading_relation('b4312033-7ead-4d79-82f6-483c226e95c7', historical_treaty_substrate__stewardship_reading, forecloses).
narrative_ontology:cs_reading_relation('b4312033-7ead-4d79-82f6-483c226e95c7', historical_treaty_substrate__nation_to_nation_reading, forecloses).
narrative_ontology:cs_axiom('b4312033-7ead-4d79-82f6-483c226e95c7', foundational, territorial_sovereignty_is_divisible_and_transferable).
narrative_ontology:cs_axiom_status(territorial_sovereignty_is_divisible_and_transferable, holdable).
narrative_ontology:cs_axiom_grounding('b4312033-7ead-4d79-82f6-483c226e95c7', territorial_sovereignty_is_divisible_and_transferable, conventional).
narrative_ontology:cs_axiom('b4312033-7ead-4d79-82f6-483c226e95c7', foundational, indigenous_nations_had_capacity_to_cede_sovereignty).
narrative_ontology:cs_axiom_status(indigenous_nations_had_capacity_to_cede_sovereignty, holdable).
narrative_ontology:cs_axiom_grounding('b4312033-7ead-4d79-82f6-483c226e95c7', indigenous_nations_had_capacity_to_cede_sovereignty, conventional).
narrative_ontology:cs_reference_frame('b4312033-7ead-4d79-82f6-483c226e95c7', crown_sovereignty_by_discovery).
narrative_ontology:cs_drift_state('b4312033-7ead-4d79-82f6-483c226e95c7', contemporary_international_law_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('b4312033-7ead-4d79-82f6-483c226e95c7', '').
narrative_ontology:cs_kernel_id(historical_treaty_substrate__extinguishment_reading, historical_treaty_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(historical_treaty_substrate__extinguishment_reading, settler_state).
narrative_ontology:constraint_beneficiary(historical_treaty_substrate__extinguishment_reading, settler_population).
narrative_ontology:constraint_victim(historical_treaty_substrate__extinguishment_reading, indigenous_nations).
narrative_ontology:constraint_victim(historical_treaty_substrate__extinguishment_reading, indigenous_peoples).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The sovereign authority that claims full territorial jurisdiction over lands acquired through historical treaties, interpreting these agreements as completed property transactions that extinguished Indigenous title. Benefits from clear legal title and resource access.
narrative_ontology:constraint_stakeholder(historical_treaty_substrate__extinguishment_reading, settler_state, agenda_setter,
    institutional, generational, arbitrage, national).

% Citizens and corporations within the settler state who benefit from the legal certainty of land ownership, resource development, and infrastructure built on former Indigenous territories, without acknowledging ongoing Indigenous sovereignty.
narrative_ontology:constraint_stakeholder(historical_treaty_substrate__extinguishment_reading, settler_population, beneficiary,
    powerful, biographical, mobile, national).

% The original inhabitants and sovereign entities who entered into treaties, but whose interpretation of these agreements as ongoing relationships or shared stewardship is legally overridden by the extinguishment reading. They bear the loss of territorial jurisdiction and self-determination.
narrative_ontology:constraint_stakeholder(historical_treaty_substrate__extinguishment_reading, indigenous_nations, payer,
    powerless, civilizational, identity_locked, local).

% Individual members of Indigenous nations who experience the direct consequences of the extinguishment reading, including dispossession, limited access to traditional lands and resources, and subjection to settler law without full consent. Their identity is deeply tied to their ancestral lands.
narrative_ontology:constraint_stakeholder(historical_treaty_substrate__extinguishment_reading, indigenous_peoples, payer,
    powerless, generational, trapped, local).

% Legal academics and practitioners who uphold and articulate the extinguishment reading, often based on historical legal doctrines and precedents, contributing to its intellectual and judicial persistence.
narrative_ontology:constraint_stakeholder(historical_treaty_substrate__extinguishment_reading, legal_scholars_extinguishment, observer,
    analytical, biographical, analytical, universal).

% Organizations and tribunals that review state practices against international human rights and Indigenous rights standards, often challenging the legal validity and moral legitimacy of extinguishment doctrines.
narrative_ontology:constraint_stakeholder(historical_treaty_substrate__extinguishment_reading, international_human_rights_bodies, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(historical_treaty_substrate__extinguishment_reading, settler_state).
narrative_ontology:fixing_cost_class(historical_treaty_substrate__extinguishment_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To establish a clear, singular legal framework for land ownership and governance within the claimed territory of the settler state, resolving potential conflicts over jurisdiction and resource access by asserting settler sovereignty.
% TRANSFER_FUNCTION: Transfers full territorial sovereignty and resource control from Indigenous nations to the settler state, in exchange for limited reserve lands, annuities, and specific rights for Indigenous parties.
% ABSENT_VOICES: The full, unceded sovereignty and legal traditions of Indigenous nations are structurally excluded from the settler legal framework that enforces the extinguishment reading. Their voices would assert ongoing jurisdiction and a relational understanding of treaties.
% DISAPPEARANCE_RATIONALE: If the extinguishment reading vanished overnight, the legal basis for much of the settler state's territorial claim and resource management would be undermined. Land titles would become contested, resource projects would halt, and the entire constitutional and legal landscape would require fundamental renegotiation with Indigenous nations, leading to a profound reorganization of governance and property relations.
% FOUNDING_PROBLEM: To secure vast territories for European settlement, resource extraction, and the expansion of colonial power, while minimizing conflict with Indigenous populations and establishing a clear legal basis for settler jurisdiction.
% FOUNDING_PROBLEM_CORROBORATION: The settler state's ongoing legal arguments and policy decisions consistently reflect the need to maintain clear jurisdiction and resource access, indicating the 'problem' of securing and legitimizing control over territory is still live from its perspective. Indigenous legal challenges and international human rights reports, while contesting the legitimacy of the solution, corroborate the settler state's historical and ongoing intent to secure this control.
narrative_ontology:disappearance_verdict(historical_treaty_substrate__extinguishment_reading, world_rearranges).
narrative_ontology:founding_problem_status(historical_treaty_substrate__extinguishment_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(historical_treaty_substrate__extinguishment_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(historical_treaty_substrate__extinguishment_reading, 'none', 1).
narrative_ontology:epsilon_provenance(historical_treaty_substrate__extinguishment_reading, 0.9, 'gemini-2.5-flash', 'none', direct).

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
 *   The extractiveness (0.90) is very high because this reading claims the complete transfer of a fundamental asset (sovereignty over vast territories) for limited compensation, with ongoing benefits accruing to the settler state. Suppression (0.85) is high due to the continuous legal and political enforcement required to maintain this interpretation against Indigenous challenges, often backed by state power. Accessibility collapse (0.90) is near total from the perspective of this reading, as it legally denies alternatives to settler jurisdiction. Resistance (0.75) is high, reflecting the sustained and growing Indigenous legal and political movements challenging this interpretation. Theater ratio (0.20) is low because the state's actions are largely functional in maintaining its claim, with less performative 'honoring' of the original spirit of the treaties.
 *
 * PERSPECTIVAL GAP:
 *   The settler state and population experience this constraint as a legitimate, settled legal framework that provides stability and prosperity. Indigenous nations and peoples, however, experience it as an ongoing act of dispossession and a fundamental denial of their inherent sovereignty, maintained through coercive legal and political structures. The engine's per-seat classification will highlight this divergence, with beneficiaries seeing a 'rope' or 'mountain' and victims experiencing a 'snare'.
 *
 * DIRECTIONALITY LOGIC:
 *   The settler state and population are clear beneficiaries, gaining land, resources, and jurisdictional clarity. Indigenous nations and peoples are the primary targets, losing sovereignty and control over their ancestral territories. Legal scholars supporting extinguishment act as analytical observers who reinforce the constraint's legitimacy. International human rights bodies act as analytical observers who challenge it.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is not subject to mandatrophy in the traditional sense, as its 'mandate' (securing settler jurisdiction) is still actively pursued by its beneficiaries. However, the contestation around its legitimacy and the rising resistance suggest that its functional persistence relies increasingly on suppression rather than genuine coordination, aligning with a snare classification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    legal_vs_moral_legitimacy,
    'Does the legal validity of the extinguishment reading (based on settler law) align with its moral and ethical legitimacy (based on Indigenous perspectives and international human rights)?',
    'International legal rulings, truth and reconciliation commissions, or shifts in domestic constitutional interpretation that explicitly address the moral dimensions of historical treaty interpretation.',
    'If a significant gap is acknowledged, the constraint''s effective legitimacy would collapse, increasing its perceived extractiveness and suppression from an ethical standpoint, even if legally maintained. This would amplify calls for renegotiation or reparations.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(legal_vs_moral_legitimacy, conceptual, 'The divergence between legal and moral claims of treaty extinguishment.').

omega_variable(
    consent_authenticity,
    'To what extent was Indigenous consent to the extinguishment of sovereignty truly free, informed, and uncoerced, given the power imbalances and cultural misunderstandings at the time of treaty-making?',
    'Historical and anthropological research into treaty negotiations, Indigenous oral histories, and legal analysis of the concept of ''consent'' in colonial contexts.',
    'Evidence of coerced or uninformed consent would fundamentally undermine the foundational axioms of the extinguishment reading, shifting its classification further towards a snare by exposing the coercive origins of the ''transaction''.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(consent_authenticity, empirical, 'The authenticity of Indigenous consent to sovereignty extinguishment.').

omega_variable(
    treaty_as_property_vs_relationship,
    'Is the ''property transaction'' framing of treaties an accurate reflection of Indigenous legal traditions and understandings of land, or is it a colonial imposition that fundamentally misrepresents the nature of the agreements?',
    'Comparative legal anthropology, Indigenous legal scholarship, and recognition of Indigenous legal orders within settler legal systems.',
    'If the property transaction framing is shown to be a misrepresentation, the entire conceptual basis of the extinguishment reading would be challenged, potentially leading to its reclassification as a conceptual snare or even a piton if its original ''function'' was never genuinely shared.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(treaty_as_property_vs_relationship, conceptual, 'The fundamental conceptual difference between property transfer and relational pacts in treaty interpretation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(historical_treaty_substrate__extinguishment_reading, 1763, 2023).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hist_tr_t1763, historical_treaty_substrate__extinguishment_reading, theater_ratio, 1763, 0.1).
narrative_ontology:measurement(hist_tr_t1850, historical_treaty_substrate__extinguishment_reading, theater_ratio, 1850, 0.15).
narrative_ontology:measurement(hist_tr_t1920, historical_treaty_substrate__extinguishment_reading, theater_ratio, 1920, 0.25).
narrative_ontology:measurement(hist_tr_t1970, historical_treaty_substrate__extinguishment_reading, theater_ratio, 1970, 0.22).
narrative_ontology:measurement(hist_tr_t2000, historical_treaty_substrate__extinguishment_reading, theater_ratio, 2000, 0.21).
narrative_ontology:measurement(hist_tr_t2023, historical_treaty_substrate__extinguishment_reading, theater_ratio, 2023, 0.2).

% Extraction over time
narrative_ontology:measurement(hist_be_t1763, historical_treaty_substrate__extinguishment_reading, base_extractiveness, 1763, 0.7).
narrative_ontology:measurement(hist_be_t1850, historical_treaty_substrate__extinguishment_reading, base_extractiveness, 1850, 0.8).
narrative_ontology:measurement(hist_be_t1920, historical_treaty_substrate__extinguishment_reading, base_extractiveness, 1920, 0.95).
narrative_ontology:measurement(hist_be_t1970, historical_treaty_substrate__extinguishment_reading, base_extractiveness, 1970, 0.92).
narrative_ontology:measurement(hist_be_t2000, historical_treaty_substrate__extinguishment_reading, base_extractiveness, 2000, 0.91).
narrative_ontology:measurement(hist_be_t2023, historical_treaty_substrate__extinguishment_reading, base_extractiveness, 2023, 0.9).

% Suppression requirement over time
narrative_ontology:measurement(hist_su_t1763, historical_treaty_substrate__extinguishment_reading, suppression_requirement, 1763, 0.6).
narrative_ontology:measurement(hist_su_t1850, historical_treaty_substrate__extinguishment_reading, suppression_requirement, 1850, 0.75).
narrative_ontology:measurement(hist_su_t1920, historical_treaty_substrate__extinguishment_reading, suppression_requirement, 1920, 0.9).
narrative_ontology:measurement(hist_su_t1970, historical_treaty_substrate__extinguishment_reading, suppression_requirement, 1970, 0.88).
narrative_ontology:measurement(hist_su_t2000, historical_treaty_substrate__extinguishment_reading, suppression_requirement, 2000, 0.86).
narrative_ontology:measurement(hist_su_t2023, historical_treaty_substrate__extinguishment_reading, suppression_requirement, 2023, 0.85).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=1763, tn=2023
narrative_ontology:measurement(hist_grid_01, historical_treaty_substrate__extinguishment_reading, accessibility_collapse(class), 1763, 0.6).
narrative_ontology:measurement(hist_grid_02, historical_treaty_substrate__extinguishment_reading, accessibility_collapse(class), 2023, 0.8).
narrative_ontology:measurement(hist_grid_03, historical_treaty_substrate__extinguishment_reading, accessibility_collapse(individual), 1763, 0.55).
narrative_ontology:measurement(hist_grid_04, historical_treaty_substrate__extinguishment_reading, accessibility_collapse(individual), 2023, 0.75).
narrative_ontology:measurement(hist_grid_05, historical_treaty_substrate__extinguishment_reading, accessibility_collapse(organizational), 1763, 0.65).
narrative_ontology:measurement(hist_grid_06, historical_treaty_substrate__extinguishment_reading, accessibility_collapse(organizational), 2023, 0.85).
narrative_ontology:measurement(hist_grid_07, historical_treaty_substrate__extinguishment_reading, accessibility_collapse(structural), 1763, 0.7).
narrative_ontology:measurement(hist_grid_08, historical_treaty_substrate__extinguishment_reading, accessibility_collapse(structural), 2023, 0.9).
narrative_ontology:measurement(hist_grid_09, historical_treaty_substrate__extinguishment_reading, resistance(class), 1763, 0.4).
narrative_ontology:measurement(hist_grid_10, historical_treaty_substrate__extinguishment_reading, resistance(class), 2023, 0.65).
narrative_ontology:measurement(hist_grid_11, historical_treaty_substrate__extinguishment_reading, resistance(individual), 1763, 0.45).
narrative_ontology:measurement(hist_grid_12, historical_treaty_substrate__extinguishment_reading, resistance(individual), 2023, 0.6).
narrative_ontology:measurement(hist_grid_13, historical_treaty_substrate__extinguishment_reading, resistance(organizational), 1763, 0.35).
narrative_ontology:measurement(hist_grid_14, historical_treaty_substrate__extinguishment_reading, resistance(organizational), 2023, 0.7).
narrative_ontology:measurement(hist_grid_15, historical_treaty_substrate__extinguishment_reading, resistance(structural), 1763, 0.3).
narrative_ontology:measurement(hist_grid_16, historical_treaty_substrate__extinguishment_reading, resistance(structural), 2023, 0.75).
narrative_ontology:measurement(hist_grid_17, historical_treaty_substrate__extinguishment_reading, stakes_inflation(class), 1763, 0.65).
narrative_ontology:measurement(hist_grid_18, historical_treaty_substrate__extinguishment_reading, stakes_inflation(class), 2023, 0.8).
narrative_ontology:measurement(hist_grid_19, historical_treaty_substrate__extinguishment_reading, stakes_inflation(individual), 1763, 0.6).
narrative_ontology:measurement(hist_grid_20, historical_treaty_substrate__extinguishment_reading, stakes_inflation(individual), 2023, 0.75).
narrative_ontology:measurement(hist_grid_21, historical_treaty_substrate__extinguishment_reading, stakes_inflation(organizational), 1763, 0.7).
narrative_ontology:measurement(hist_grid_22, historical_treaty_substrate__extinguishment_reading, stakes_inflation(organizational), 2023, 0.85).
narrative_ontology:measurement(hist_grid_23, historical_treaty_substrate__extinguishment_reading, stakes_inflation(structural), 1763, 0.75).
narrative_ontology:measurement(hist_grid_24, historical_treaty_substrate__extinguishment_reading, stakes_inflation(structural), 2023, 0.9).
narrative_ontology:measurement(hist_grid_25, historical_treaty_substrate__extinguishment_reading, suppression(class), 1763, 0.5).
narrative_ontology:measurement(hist_grid_26, historical_treaty_substrate__extinguishment_reading, suppression(class), 2023, 0.75).
narrative_ontology:measurement(hist_grid_27, historical_treaty_substrate__extinguishment_reading, suppression(individual), 1763, 0.45).
narrative_ontology:measurement(hist_grid_28, historical_treaty_substrate__extinguishment_reading, suppression(individual), 2023, 0.7).
narrative_ontology:measurement(hist_grid_29, historical_treaty_substrate__extinguishment_reading, suppression(organizational), 1763, 0.55).
narrative_ontology:measurement(hist_grid_30, historical_treaty_substrate__extinguishment_reading, suppression(organizational), 2023, 0.8).
narrative_ontology:measurement(hist_grid_31, historical_treaty_substrate__extinguishment_reading, suppression(structural), 1763, 0.6).
narrative_ontology:measurement(hist_grid_32, historical_treaty_substrate__extinguishment_reading, suppression(structural), 2023, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(historical_treaty_substrate__extinguishment_reading, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
