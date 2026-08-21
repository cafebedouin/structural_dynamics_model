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
 *   constraint_id: historical_treaty_substrate__extinguishment_reading
 *   human_readable: Historical Treaty Substrate (Extinguishment Reading)
 *   domain: legal_anthropology/indigenous_law/comparative_constitutional_theory
 *
 * SUMMARY:
 *   This constraint represents the 'extinguishment reading' of historical
 *   treaties between Indigenous nations and settler states. In this reading,
 *   treaties are interpreted as completed property transactions where
 *   Indigenous parties ceded territorial sovereignty in exchange for defined
 *   reserves and payments. This interpretation serves as the legal substrate
 *   for settler state expansion and resource exploitation, while
 *   simultaneously diminishing Indigenous self-determination. The constraint
 *   is claimed as a 'snare' due to its high extraction and suppression,
 *   despite being framed by its beneficiaries as a legitimate legal
 *   framework.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(historical_treaty_substrate__extinguishment_reading, 0.85).
domain_priors:suppression_score(historical_treaty_substrate__extinguishment_reading, 0.92).
domain_priors:theater_ratio(historical_treaty_substrate__extinguishment_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(historical_treaty_substrate__extinguishment_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(historical_treaty_substrate__extinguishment_reading, suppression_requirement, 0.92).
narrative_ontology:constraint_metric(historical_treaty_substrate__extinguishment_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(historical_treaty_substrate__extinguishment_reading, accessibility_collapse, 0.88).
narrative_ontology:constraint_metric(historical_treaty_substrate__extinguishment_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(historical_treaty_substrate__extinguishment_reading, snare).
narrative_ontology:human_readable(historical_treaty_substrate__extinguishment_reading, "Historical Treaty Substrate (Extinguishment Reading)").
narrative_ontology:topic_domain(historical_treaty_substrate__extinguishment_reading, "legal_anthropology/indigenous_law/comparative_constitutional_theory").

domain_priors:requires_active_enforcement(historical_treaty_substrate__extinguishment_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(historical_treaty_substrate__extinguishment_reading, '8eb03ab6-cf1a-458c-83fe-b513b90d4d77').
narrative_ontology:cs_kernel_codification('8eb03ab6-cf1a-458c-83fe-b513b90d4d77', fixed_text).
narrative_ontology:cs_authority_grounding('8eb03ab6-cf1a-458c-83fe-b513b90d4d77', lineage).
narrative_ontology:cs_interpretation_layer_present('8eb03ab6-cf1a-458c-83fe-b513b90d4d77').
narrative_ontology:cs_reading_relation('8eb03ab6-cf1a-458c-83fe-b513b90d4d77', historical_treaty_substrate__stewardship_reading, forecloses).
narrative_ontology:cs_reading_relation('8eb03ab6-cf1a-458c-83fe-b513b90d4d77', historical_treaty_substrate__nation_to_nation_reading, forecloses).
narrative_ontology:cs_axiom('8eb03ab6-cf1a-458c-83fe-b513b90d4d77', foundational, territorial_sovereignty_is_divisible_and_transferable).
narrative_ontology:cs_axiom_status(territorial_sovereignty_is_divisible_and_transferable, holdable).
narrative_ontology:cs_axiom_grounding('8eb03ab6-cf1a-458c-83fe-b513b90d4d77', territorial_sovereignty_is_divisible_and_transferable, conventional).
narrative_ontology:cs_axiom('8eb03ab6-cf1a-458c-83fe-b513b90d4d77', foundational, indigenous_title_is_a_burden_on_crown_title).
narrative_ontology:cs_axiom_status(indigenous_title_is_a_burden_on_crown_title, holdable).
narrative_ontology:cs_axiom_grounding('8eb03ab6-cf1a-458c-83fe-b513b90d4d77', indigenous_title_is_a_burden_on_crown_title, conventional).
narrative_ontology:cs_reference_frame('8eb03ab6-cf1a-458c-83fe-b513b90d4d77', pacta_sunt_servanda_as_property_transfer).
narrative_ontology:cs_drift_state('8eb03ab6-cf1a-458c-83fe-b513b90d4d77', contemporary_reconciliation_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('8eb03ab6-cf1a-458c-83fe-b513b90d4d77', '').
narrative_ontology:cs_kernel_id(historical_treaty_substrate__extinguishment_reading, historical_treaty_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(historical_treaty_substrate__extinguishment_reading, settler_state_governments).
narrative_ontology:constraint_beneficiary(historical_treaty_substrate__extinguishment_reading, settler_populations).
narrative_ontology:constraint_victim(historical_treaty_substrate__extinguishment_reading, indigenous_nations).
narrative_ontology:constraint_victim(historical_treaty_substrate__extinguishment_reading, indigenous_peoples).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets historical treaties as instruments of land cession and extinguishment of Indigenous sovereignty, enabling unfettered resource development and settlement. Benefits from clear, unencumbered title to vast territories. Actively defends this interpretation in courts and policy.
narrative_ontology:constraint_stakeholder(historical_treaty_substrate__extinguishment_reading, settler_state_governments, agenda_setter,
    institutional, generational, arbitrage, national).

% Benefit from the perceived legitimacy of land ownership and resource access derived from the extinguishment reading of treaties. Their economic and social structures are built upon this understanding of territorial rights.
narrative_ontology:constraint_stakeholder(historical_treaty_substrate__extinguishment_reading, settler_populations, beneficiary,
    organized, biographical, mobile, national).

% Are treated as having ceded inherent sovereignty over their traditional territories, retaining only limited, enumerated rights to reserves and annuities. Bear the cost of lost self-determination, cultural disruption, and economic marginalization. Their identity is deeply tied to their ancestral lands, making 'exit' from the treaty relationship a profound existential challenge.
narrative_ontology:constraint_stakeholder(historical_treaty_substrate__extinguishment_reading, indigenous_nations, payer,
    powerless, civilizational, identity_locked, local).

% Experience the direct consequences of the extinguishment reading, including limited access to traditional lands and resources, and subjection to settler state laws. Their ability to assert inherent rights is suppressed by the legal and political structures built on this interpretation.
narrative_ontology:constraint_stakeholder(historical_treaty_substrate__extinguishment_reading, indigenous_peoples, payer,
    powerless, generational, trapped, local).

% Monitor and critique the extinguishment reading from the perspective of Indigenous rights to self-determination and ancestral lands, often finding it in violation of international norms. Their observations provide external pressure but lack direct enforcement power.
narrative_ontology:constraint_stakeholder(historical_treaty_substrate__extinguishment_reading, international_human_rights_bodies, observer,
    institutional, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: From the settler state's perspective, it 'coordinates' the orderly transfer of vast territories from Indigenous control to settler jurisdiction, providing a legal basis for settlement and resource development, and defining the limited rights of Indigenous peoples within the new order.
% TRANSFER_FUNCTION: Transfers inherent Indigenous territorial sovereignty and jurisdiction to the settler state, in exchange for defined reserve lands, annuities, and specific, limited rights for Indigenous peoples.
% ABSENT_VOICES: The inherent, pre-existing sovereignty of Indigenous nations is rendered absent in the legal and political discourse that frames treaties as property transactions. Their understanding of treaties as ongoing, nation-to-nation agreements for shared stewardship is systematically excluded from the dominant legal interpretation.
% DISAPPEARANCE_RATIONALE: If the extinguishment reading of treaties vanished overnight, the legal and political landscape of settler states would be fundamentally destabilized. Land titles, resource development projects, and the very legitimacy of settler governance over vast territories would be called into question, leading to a profound reorganization of power and property relations.
% FOUNDING_PROBLEM: The settler state faced the problem of legitimizing its expansion and control over Indigenous territories, which were already occupied and governed by Indigenous nations, to facilitate settlement and resource extraction.
% FOUNDING_PROBLEM_CORROBORATION: Settler state governments and settler populations attest that the problem of clear title and unified jurisdiction remains live, as it underpins their entire legal and economic system. Indigenous nations and international human rights bodies attest that the 'problem' was a colonial construct, and its 'solution' continues to be a source of ongoing injustice, corroborating the persistence of the extractive arrangement.
narrative_ontology:disappearance_verdict(historical_treaty_substrate__extinguishment_reading, world_rearranges).
narrative_ontology:founding_problem_status(historical_treaty_substrate__extinguishment_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(historical_treaty_substrate__extinguishment_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(historical_treaty_substrate__extinguishment_reading, 'none', 1).
narrative_ontology:epsilon_provenance(historical_treaty_substrate__extinguishment_reading, 0.85, 'gemini-2.5-flash', 'none', direct).

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
 *   Extractiveness is high (0.85) because the reading enables the settler state to claim vast territories and resources without ongoing Indigenous consent, while providing minimal, often inadequate, compensation. Suppression is very high (0.92) as this reading is actively enforced through legal systems, policy, and sometimes physical force, to prevent Indigenous assertions of inherent sovereignty or alternative interpretations. Theater ratio is low (0.15) because the 'coordination' function (orderly transfer of land) is largely a cover for the underlying extraction; the enforcement is very real. Accessibility collapse is high (0.88) as this reading systematically denies alternatives to Indigenous peoples, leaving them few avenues for redress within the settler legal framework. Resistance is high (0.75) reflecting ongoing Indigenous legal challenges, protests, and political organizing against this interpretation.
 *
 * PERSPECTIVAL GAP:
 *   The settler state views this reading as a legitimate, foundational legal principle that enabled the formation of the modern state. Indigenous nations and peoples experience it as an ongoing act of dispossession and cultural suppression. The engine's classification as a 'snare' reflects the latter, highlighting the structural asymmetry and coercion inherent in this interpretation, despite the settler state's 'rope' or 'mountain' framing.
 *
 * DIRECTIONALITY LOGIC:
 *   Settler state governments and populations are the primary beneficiaries, gaining clear title to land and resources (low d). Indigenous nations and peoples are the primary targets, losing sovereignty and control over their ancestral territories (high d, identity_locked/trapped exit options). International human rights bodies act as observers, critiquing the extractive nature of this reading from an analytical distance.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate, from the settler perspective, was to 'civilize' and 'develop' the land, which required extinguishing Indigenous title. This mandate is still 'live' for settler states as it underpins their legal and economic systems. However, from an Indigenous perspective, the original 'problem' was the settler state's desire for land, and the 'solution' (extinguishment) is the ongoing problem. The classification as a snare prevents mislabeling this as a legitimate coordination mechanism by exposing the high extraction and suppression, and the contested nature of its founding problem and beneficiaries.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    treaty_intent_ambiguity,
    'Did Indigenous parties intend to cede inherent sovereignty, or did they understand treaties as agreements for shared use and coexistence?',
    'Historical and linguistic analysis of Indigenous oral traditions, legal systems, and diplomatic practices at the time of treaty-making, independent of settler colonial records.',
    'If Indigenous intent was not to cede sovereignty, the ''extinguishment reading'' is a misrepresentation, strengthening its classification as a snare and undermining the legitimacy of settler land claims. If intent was ambiguous or coerced, it highlights the power imbalance.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(treaty_intent_ambiguity, empirical, 'Ambiguity regarding Indigenous intent during treaty negotiations.').

omega_variable(
    legal_pluralism_recognition,
    'Should settler legal systems recognize Indigenous legal orders as co-existing and equally valid sources of law regarding treaty interpretation?',
    'Judicial decisions or legislative reforms that explicitly incorporate Indigenous legal principles and interpretations into treaty adjudication, moving beyond a purely common law or civil law framework.',
    'Recognition of legal pluralism would fundamentally challenge the ''extinguishment reading'' by introducing alternative, often non-cessionary, interpretations, potentially reclassifying the constraint towards a more contested or even a ''tangled_rope'' if genuine coordination with Indigenous legal orders were attempted.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legal_pluralism_recognition, conceptual, 'Whether Indigenous legal orders should be recognized in treaty interpretation.').

omega_variable(
    mandatrophy_of_civilizing_mission,
    'Has the ''civilizing mission'' justification for extinguishing Indigenous sovereignty become a dead mandate, yet the extractive structures it enabled persist?',
    'Analysis of contemporary settler state policy and public discourse: if the ''civilizing mission'' is explicitly repudiated, but the legal and economic structures of extinguishment remain intact, it indicates mandatrophy.',
    'If the mandate is dead, the persistence of the extinguishment reading is purely inertial and extractive, reinforcing its snare classification and highlighting the need for structural reform rather than mere policy adjustments.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(mandatrophy_of_civilizing_mission, empirical, 'Whether the original colonial justification for extinguishment is defunct but its effects persist.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(historical_treaty_substrate__extinguishment_reading, 0, 150).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hist_tr_t0, historical_treaty_substrate__extinguishment_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(hist_tr_t30, historical_treaty_substrate__extinguishment_reading, theater_ratio, 30, 0.2).
narrative_ontology:measurement(hist_tr_t60, historical_treaty_substrate__extinguishment_reading, theater_ratio, 60, 0.18).
narrative_ontology:measurement(hist_tr_t90, historical_treaty_substrate__extinguishment_reading, theater_ratio, 90, 0.15).
narrative_ontology:measurement(hist_tr_t120, historical_treaty_substrate__extinguishment_reading, theater_ratio, 120, 0.14).
narrative_ontology:measurement(hist_tr_t150, historical_treaty_substrate__extinguishment_reading, theater_ratio, 150, 0.15).

% Extraction over time
narrative_ontology:measurement(hist_be_t0, historical_treaty_substrate__extinguishment_reading, base_extractiveness, 0, 0.7).
narrative_ontology:measurement(hist_be_t30, historical_treaty_substrate__extinguishment_reading, base_extractiveness, 30, 0.78).
narrative_ontology:measurement(hist_be_t60, historical_treaty_substrate__extinguishment_reading, base_extractiveness, 60, 0.82).
narrative_ontology:measurement(hist_be_t90, historical_treaty_substrate__extinguishment_reading, base_extractiveness, 90, 0.85).
narrative_ontology:measurement(hist_be_t120, historical_treaty_substrate__extinguishment_reading, base_extractiveness, 120, 0.86).
narrative_ontology:measurement(hist_be_t150, historical_treaty_substrate__extinguishment_reading, base_extractiveness, 150, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(hist_su_t0, historical_treaty_substrate__extinguishment_reading, suppression_requirement, 0, 0.8).
narrative_ontology:measurement(hist_su_t30, historical_treaty_substrate__extinguishment_reading, suppression_requirement, 30, 0.85).
narrative_ontology:measurement(hist_su_t60, historical_treaty_substrate__extinguishment_reading, suppression_requirement, 60, 0.9).
narrative_ontology:measurement(hist_su_t90, historical_treaty_substrate__extinguishment_reading, suppression_requirement, 90, 0.92).
narrative_ontology:measurement(hist_su_t120, historical_treaty_substrate__extinguishment_reading, suppression_requirement, 120, 0.91).
narrative_ontology:measurement(hist_su_t150, historical_treaty_substrate__extinguishment_reading, suppression_requirement, 150, 0.92).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(historical_treaty_substrate__extinguishment_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(historical_treaty_substrate__extinguishment_reading, historical_treaty_substrate__stewardship_reading).
narrative_ontology:affects_constraint(historical_treaty_substrate__extinguishment_reading, historical_treaty_substrate__nation_to_nation_reading).
narrative_ontology:affects_constraint(historical_treaty_substrate__extinguishment_reading, resource_extraction_permitting).
narrative_ontology:affects_constraint(historical_treaty_substrate__extinguishment_reading, indigenous_land_claims_litigation).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'historical_treaty_substrate' kernel. This 'extinguishment_reading' is the dominant settler-colonial interpretation, which directly influences and is contested by the 'stewardship_reading' and 'nation_to_nation_reading'.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
