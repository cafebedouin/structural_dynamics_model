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
 *   constraint_id: treaty_authority_cession__rangatiratanga_retention_reading
 *   human_readable: Treaty of Waitangi: Rangatiratanga Retention Reading (Māori Text Controls)
 *   domain: constitutional_law/indigenous_rights/colonial_history
 *
 * SUMMARY:
 *   This constraint story instantiates the 'Rangatiratanga Retention' reading
 *   of the Treaty of Waitangi, where the Māori text is paramount (via contra
 *   proferentem). It posits the Treaty as establishing a partnership where
 *   Māori retain full chieftainship (tino rangatiratanga) and the Crown gains
 *   limited governance (kāwanatanga) over settlers, requiring ongoing Māori
 *   consent for actions affecting them. This reading frames the constraint as
 *   a Rope of partnership, but acknowledges historical and ongoing challenges
 *   to its full realization. The structural delta expected for this reading
 *   is a Rope of partnership requiring negotiated authority exercise, where
 *   Crown acts are legitimate only with hapū consent, and a retrospective
 *   snare is visible in land alienation under translation asymmetry.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(treaty_authority_cession__rangatiratanga_retention_reading, 0.25).
domain_priors:suppression_score(treaty_authority_cession__rangatiratanga_retention_reading, 0.4).
domain_priors:theater_ratio(treaty_authority_cession__rangatiratanga_retention_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(treaty_authority_cession__rangatiratanga_retention_reading, extractiveness, 0.25).
narrative_ontology:constraint_metric(treaty_authority_cession__rangatiratanga_retention_reading, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(treaty_authority_cession__rangatiratanga_retention_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(treaty_authority_cession__rangatiratanga_retention_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(treaty_authority_cession__rangatiratanga_retention_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(treaty_authority_cession__rangatiratanga_retention_reading, rope).
narrative_ontology:human_readable(treaty_authority_cession__rangatiratanga_retention_reading, "Treaty of Waitangi: Rangatiratanga Retention Reading (Māori Text Controls)").
narrative_ontology:topic_domain(treaty_authority_cession__rangatiratanga_retention_reading, "constitutional_law/indigenous_rights/colonial_history").

domain_priors:requires_active_enforcement(treaty_authority_cession__rangatiratanga_retention_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(treaty_authority_cession__rangatiratanga_retention_reading, 'cb0ab471-34bb-4746-92ac-334a0c265fcf').
narrative_ontology:cs_kernel_codification('cb0ab471-34bb-4746-92ac-334a0c265fcf', fixed_text).
narrative_ontology:cs_authority_grounding('cb0ab471-34bb-4746-92ac-334a0c265fcf', lineage).
narrative_ontology:cs_interpretation_layer_present('cb0ab471-34bb-4746-92ac-334a0c265fcf').
narrative_ontology:cs_reading_relation('cb0ab471-34bb-4746-92ac-334a0c265fcf', treaty_authority_cession__crown_cession_reading, coexists_with).
narrative_ontology:cs_reading_relation('cb0ab471-34bb-4746-92ac-334a0c265fcf', treaty_authority_cession__biculturalism_reading, coexists_with).
narrative_ontology:cs_reading_relation('cb0ab471-34bb-4746-92ac-334a0c265fcf', treaty_authority_cession__retrospective_snare_exposure, influences).
narrative_ontology:cs_axiom('cb0ab471-34bb-4746-92ac-334a0c265fcf', foundational, maori_text_paramount).
narrative_ontology:cs_axiom_status(maori_text_paramount, holdable).
narrative_ontology:cs_axiom_grounding('cb0ab471-34bb-4746-92ac-334a0c265fcf', maori_text_paramount, conventional).
narrative_ontology:cs_axiom('cb0ab471-34bb-4746-92ac-334a0c265fcf', foundational, tino_rangatiratanga_retained).
narrative_ontology:cs_axiom_status(tino_rangatiratanga_retained, holdable).
narrative_ontology:cs_axiom_grounding('cb0ab471-34bb-4746-92ac-334a0c265fcf', tino_rangatiratanga_retained, deontological).
narrative_ontology:cs_reference_frame('cb0ab471-34bb-4746-92ac-334a0c265fcf', original_maori_intent).
narrative_ontology:cs_drift_state('cb0ab471-34bb-4746-92ac-334a0c265fcf', contemporary, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('cb0ab471-34bb-4746-92ac-334a0c265fcf', '2024-07-30T12:00:00Z').
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

% Retain tino rangatiratanga (full chieftainship) over their lands, resources, and culture, granting the Crown only kāwanatanga (governance) for settlers. Their consent is required for legitimate Crown action affecting them. Exit is identity-locked as their existence is tied to their ancestral lands and self-determination.
narrative_ontology:constraint_stakeholder(treaty_authority_cession__rangatiratanga_retention_reading, hapu_iwi, beneficiary,
    organized, generational, identity_locked, national).

% Exercises legitimate governance (kāwanatanga) over settlers and for the general welfare, but only in partnership with hapū/iwi, requiring their ongoing consent for actions impacting Māori. The Crown benefits from a legitimate basis for its presence but is constrained by partnership obligations. Exit from this partnership model would undermine its own legitimacy.
narrative_ontology:constraint_stakeholder(treaty_authority_cession__rangatiratanga_retention_reading, crown_as_partner, agenda_setter,
    institutional, generational, constrained, national).

% Benefits from stable governance and the rule of law provided by the Crown, but also bears the costs of Crown obligations to Māori, including potential land returns or resource sharing. Their consent to Crown authority is implicitly conditional on the Crown's perceived legitimacy.
narrative_ontology:constraint_stakeholder(treaty_authority_cession__rangatiratanga_retention_reading, settler_population, payer,
    moderate, biographical, mobile, national).

% Interprets the Treaty of Waitangi, often favoring the Māori text via contra proferentem, and recommends actions to uphold Treaty principles. Their role is to provide independent analysis and uphold the rule of law, influencing both Crown and hapū/iwi.
narrative_ontology:constraint_stakeholder(treaty_authority_cession__rangatiratanga_retention_reading, judiciary_waitangi_tribunal, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a framework for two distinct peoples (Māori and settlers) to coexist and for the Crown to exercise governance while respecting Māori self-determination, preventing conflict over land and authority.
% TRANSFER_FUNCTION: Transfers limited governance authority (kāwanatanga) to the Crown, while retaining full chieftainship (tino rangatiratanga) for hapū/iwi. It also implies a transfer of responsibility for mutual well-being and resource management.
% ABSENT_VOICES: Future generations of Māori and settlers, whose interests are represented by current parties but who would experience the long-term consequences of this partnership model. Also, those who advocate for a unitary state without special Māori rights, whose views are often marginalized in Treaty discourse.
% DISAPPEARANCE_RATIONALE: If this reading of the Treaty vanished, the entire constitutional and social fabric of Aotearoa New Zealand would unravel. The Crown's legitimacy would be fundamentally challenged, Māori claims to sovereignty would intensify, and the basis for land ownership and resource management would become highly contested, leading to widespread social and political instability.
% FOUNDING_PROBLEM: To establish a basis for British settlement and governance in Aotearoa New Zealand while protecting Māori authority and land, preventing inter-tribal warfare, and ensuring peace and order between Māori and settlers.
% FOUNDING_PROBLEM_CORROBORATION: Māori leaders and scholars consistently attest that the core issues of rangatiratanga and partnership remain live, as the Crown has historically failed to uphold its Treaty obligations under this reading. Independent legal scholars and historians also corroborate the ongoing relevance of these foundational questions.
narrative_ontology:disappearance_verdict(treaty_authority_cession__rangatiratanga_retention_reading, world_rearranges).
narrative_ontology:founding_problem_status(treaty_authority_cession__rangatiratanga_retention_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(treaty_authority_cession__rangatiratanga_retention_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(treaty_authority_cession__rangatiratanga_retention_reading, 'none', 1).
narrative_ontology:epsilon_provenance(treaty_authority_cession__rangatiratanga_retention_reading, 0.25, 'gemini-2.5-flash', 'none', direct).

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
 *   Extractiveness (0.25) is low because this reading asserts a partnership where Māori retain significant authority, implying mutual benefit and limited extraction. Suppression (0.4) is moderate, reflecting the historical and ongoing need for Māori to assert their rights against Crown actions that deviate from this partnership model, but also the legal recognition of the Māori text. Theater ratio (0.1) is low, as this reading is actively advocated and litigated, not merely performed. Resistance (0.7) is high, reflecting continuous Māori advocacy and legal challenges to ensure the Crown adheres to this interpretation.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of hapū/iwi, this reading is the true and just interpretation, establishing a framework for self-determination. From the Crown's perspective, acknowledging this reading implies significant limitations on its sovereignty and requires ongoing negotiation. The engine's classification will reflect the 'Rope' nature of this partnership, but the omegas highlight the contestation with other readings that would yield different classifications.
 *
 * DIRECTIONALITY LOGIC:
 *   Hapū/iwi are beneficiaries, retaining rangatiratanga and having their consent required (low d). The Crown, as a partner, also benefits from legitimate governance but is constrained by partnership obligations (d near symmetric). The settler population is a payer, implicitly accepting the constraints of partnership. No explicit victims are declared for this reading, as it asserts a legitimate partnership, though historical actions under other readings have created victims.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identification,
    'Is this constraint a genuine ''Rope'' of partnership, or is its partnership function undermined by the historical dominance of the ''Crown Cession'' reading?',
    'Analysis of judicial and legislative adherence to contra proferentem and the extent of Crown consultation and consent-seeking with hapū/iwi. If the Crown consistently acts unilaterally, the ''Rope'' classification is weakened.',
    'If the partnership is consistently overridden, the constraint''s effective extractiveness rises, and its classification shifts towards a ''Tangled Rope'' or ''Snare'' from the Māori perspective.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'This constraint is one reading of the Treaty of Waitangi kernel (treaty_authority_cession). This reading is ''rangatiratanga_retention_reading''.').

omega_variable(
    translation_asymmetry_impact,
    'To what extent does the historical translation asymmetry between the Māori and English texts of the Treaty of Waitangi constitute an ongoing mechanism of extraction?',
    'Detailed historical and linguistic analysis of the drafting process, and the long-term consequences of Crown actions justified by the English text, particularly regarding land alienation.',
    'If the asymmetry is a persistent mechanism of extraction, the ''retrospective_snare_exposure'' reading gains empirical weight, and the ''Rangatiratanga Retention'' reading''s effectiveness as a ''Rope'' is diminished by the underlying structural flaw.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(translation_asymmetry_impact, empirical, 'The impact of the textual divergence on the constraint''s operation.').

omega_variable(
    consent_mechanism_clarity,
    'What constitutes ''ongoing consent'' from hapū/iwi for Crown actions, and is there a clear, consistently applied mechanism for obtaining it?',
    'Examination of specific case studies of Crown-Māori interactions, focusing on whether Māori input is genuinely sought and respected, or merely consultative. Legal precedent and policy guidelines for consultation.',
    'If consent mechanisms are weak or performative, the Crown''s exercise of kāwanatanga becomes more unilateral, increasing effective extractiveness and shifting the constraint towards a ''Tangled Rope'' from the Māori perspective.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(consent_mechanism_clarity, empirical, 'Clarity and efficacy of the consent mechanism in the partnership.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(treaty_authority_cession__rangatiratanga_retention_reading, 1840, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(trea_tr_t1840, treaty_authority_cession__rangatiratanga_retention_reading, theater_ratio, 1840, 0.05).
narrative_ontology:measurement(trea_tr_t1900, treaty_authority_cession__rangatiratanga_retention_reading, theater_ratio, 1900, 0.1).
narrative_ontology:measurement(trea_tr_t1950, treaty_authority_cession__rangatiratanga_retention_reading, theater_ratio, 1950, 0.15).
narrative_ontology:measurement(trea_tr_t2000, treaty_authority_cession__rangatiratanga_retention_reading, theater_ratio, 2000, 0.08).
narrative_ontology:measurement(trea_tr_t2024, treaty_authority_cession__rangatiratanga_retention_reading, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(trea_be_t1840, treaty_authority_cession__rangatiratanga_retention_reading, base_extractiveness, 1840, 0.1).
narrative_ontology:measurement(trea_be_t1900, treaty_authority_cession__rangatiratanga_retention_reading, base_extractiveness, 1900, 0.2).
narrative_ontology:measurement(trea_be_t1950, treaty_authority_cession__rangatiratanga_retention_reading, base_extractiveness, 1950, 0.25).
narrative_ontology:measurement(trea_be_t2000, treaty_authority_cession__rangatiratanga_retention_reading, base_extractiveness, 2000, 0.23).
narrative_ontology:measurement(trea_be_t2024, treaty_authority_cession__rangatiratanga_retention_reading, base_extractiveness, 2024, 0.25).

% Suppression requirement over time
narrative_ontology:measurement(trea_su_t1840, treaty_authority_cession__rangatiratanga_retention_reading, suppression_requirement, 1840, 0.3).
narrative_ontology:measurement(trea_su_t1900, treaty_authority_cession__rangatiratanga_retention_reading, suppression_requirement, 1900, 0.4).
narrative_ontology:measurement(trea_su_t1950, treaty_authority_cession__rangatiratanga_retention_reading, suppression_requirement, 1950, 0.45).
narrative_ontology:measurement(trea_su_t2000, treaty_authority_cession__rangatiratanga_retention_reading, suppression_requirement, 2000, 0.38).
narrative_ontology:measurement(trea_su_t2024, treaty_authority_cession__rangatiratanga_retention_reading, suppression_requirement, 2024, 0.4).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(treaty_authority_cession__rangatiratanga_retention_reading, identity_coordination).
narrative_ontology:affects_constraint(treaty_authority_cession__rangatiratanga_retention_reading, treaty_authority_cession__crown_cession_reading).
narrative_ontology:affects_constraint(treaty_authority_cession__rangatiratanga_retention_reading, treaty_authority_cession__biculturalism_reading).
narrative_ontology:affects_constraint(treaty_authority_cession__rangatiratanga_retention_reading, treaty_authority_cession__retrospective_snare_exposure).

% DUAL FORMULATION NOTE:
% This constraint is one of several readings of the Treaty of Waitangi's authority cession kernel. Each reading represents a distinct structural claim about the Treaty's function and consequences.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
