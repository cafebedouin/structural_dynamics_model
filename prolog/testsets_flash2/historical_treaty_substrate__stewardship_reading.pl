% ============================================================================
% CONSTRAINT STORY: historical_treaty_substrate__stewardship_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_historical_treaty_substrate__stewardship_reading, []).

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
 *   constraint_id: historical_treaty_substrate__stewardship_reading
 *   human_readable: Historical Treaty Substrate: Stewardship Reading
 *   domain: legal_anthropology/indigenous_law/comparative_constitutional_theory
 *
 * SUMMARY:
 *   This constraint represents the 'stewardship reading' of historical
 *   treaties, where treaties are understood as relational pacts for shared
 *   territorial stewardship, not as instruments for the cession of Indigenous
 *   sovereignty. It emphasizes mutual obligations for coexistence and joint
 *   management of resources. This reading is distinct from 'extinguishment'
 *   (treaties as property transactions) and 'nation-to-nation' (treaties as
 *   international agreements between equals). The metrics reflect a
 *   low-extraction, coordination-focused interpretation, where any extraction
 *   is primarily the cost of genuine co-management.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(historical_treaty_substrate__stewardship_reading, 0.3).
domain_priors:suppression_score(historical_treaty_substrate__stewardship_reading, 0.2).
domain_priors:theater_ratio(historical_treaty_substrate__stewardship_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(historical_treaty_substrate__stewardship_reading, extractiveness, 0.3).
narrative_ontology:constraint_metric(historical_treaty_substrate__stewardship_reading, suppression_requirement, 0.2).
narrative_ontology:constraint_metric(historical_treaty_substrate__stewardship_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(historical_treaty_substrate__stewardship_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(historical_treaty_substrate__stewardship_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(historical_treaty_substrate__stewardship_reading, rope).
narrative_ontology:human_readable(historical_treaty_substrate__stewardship_reading, "Historical Treaty Substrate: Stewardship Reading").
narrative_ontology:topic_domain(historical_treaty_substrate__stewardship_reading, "legal_anthropology/indigenous_law/comparative_constitutional_theory").

domain_priors:requires_active_enforcement(historical_treaty_substrate__stewardship_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(historical_treaty_substrate__stewardship_reading, 'ed38dba0-0c60-4b4d-b8e1-e9a1cc677c5a').
narrative_ontology:cs_kernel_codification('ed38dba0-0c60-4b4d-b8e1-e9a1cc677c5a', distributed).
narrative_ontology:cs_authority_grounding('ed38dba0-0c60-4b4d-b8e1-e9a1cc677c5a', practice).
narrative_ontology:cs_interpretation_layer_present('ed38dba0-0c60-4b4d-b8e1-e9a1cc677c5a').
narrative_ontology:cs_reading_relation('ed38dba0-0c60-4b4d-b8e1-e9a1cc677c5a', historical_treaty_substrate__extinguishment_reading, coexists_with).
narrative_ontology:cs_reading_relation('ed38dba0-0c60-4b4d-b8e1-e9a1cc677c5a', historical_treaty_substrate__nation_to_nation_reading, coexists_with).
narrative_ontology:cs_axiom('ed38dba0-0c60-4b4d-b8e1-e9a1cc677c5a', foundational, inherent_indigenous_sovereignty_uncoded).
narrative_ontology:cs_axiom_status(inherent_indigenous_sovereignty_uncoded, holdable).
narrative_ontology:cs_axiom_grounding('ed38dba0-0c60-4b4d-b8e1-e9a1cc677c5a', inherent_indigenous_sovereignty_uncoded, deontological).
narrative_ontology:cs_axiom('ed38dba0-0c60-4b4d-b8e1-e9a1cc677c5a', foundational, mutual_stewardship_obligation).
narrative_ontology:cs_axiom_status(mutual_stewardship_obligation, holdable).
narrative_ontology:cs_axiom_grounding('ed38dba0-0c60-4b4d-b8e1-e9a1cc677c5a', mutual_stewardship_obligation, conventional).
narrative_ontology:cs_reference_frame('ed38dba0-0c60-4b4d-b8e1-e9a1cc677c5a', original_relational_pact).
narrative_ontology:cs_drift_state('ed38dba0-0c60-4b4d-b8e1-e9a1cc677c5a', contemporary_legal_discourse, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('ed38dba0-0c60-4b4d-b8e1-e9a1cc677c5a', '').
narrative_ontology:cs_kernel_id(historical_treaty_substrate__stewardship_reading, historical_treaty_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(historical_treaty_substrate__stewardship_reading, indigenous_nations).
narrative_ontology:constraint_beneficiary(historical_treaty_substrate__stewardship_reading, settler_state_citizens).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(historical_treaty_substrate__stewardship_reading, settler_state_governments).
narrative_ontology:constraint_victim(historical_treaty_substrate__stewardship_reading, resource_extraction_industries).
narrative_ontology:constraint_vindicates(historical_treaty_substrate__stewardship_reading, inherent_indigenous_sovereignty).
narrative_ontology:constraint_vindicates(historical_treaty_substrate__stewardship_reading, ecological_interdependence).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Retain inherent sovereignty and jurisdiction over traditional territories, participating in shared governance and resource management. Their identity is deeply tied to the land and treaty relationships, making 'exit' from the relationship unthinkable, but they benefit from the mutual obligations of stewardship.
narrative_ontology:constraint_stakeholder(historical_treaty_substrate__stewardship_reading, indigenous_nations, beneficiary,
    organized, generational, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(historical_treaty_substrate__stewardship_reading, indigenous_nations, agenda_setter).

% Are obligated to seek consent, engage in shared governance, and ensure sustainable resource management in partnership with Indigenous nations. They bear the costs of consultation and co-management, but benefit from legitimate access to resources and stable relationships. Exit is constrained by legal and moral obligations.
narrative_ontology:constraint_stakeholder(historical_treaty_substrate__stewardship_reading, settler_state_governments, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(historical_treaty_substrate__stewardship_reading, settler_state_governments, payer).

% Benefit from stable, legitimate governance and sustainable resource use, as well as the moral clarity of upholding treaty obligations. They may bear indirect costs through taxation for co-management, but their direct relationship to the constraint is one of benefit from a well-ordered society.
narrative_ontology:constraint_stakeholder(historical_treaty_substrate__stewardship_reading, settler_state_citizens, beneficiary,
    moderate, biographical, mobile, national).

% Must operate under co-management agreements, obtain consent from Indigenous nations, and adhere to higher environmental and social standards. This increases their operating costs and reduces unilateral control, but provides greater long-term stability and social license. Exit means losing access to key resources.
narrative_ontology:constraint_stakeholder(historical_treaty_substrate__stewardship_reading, resource_extraction_industries, payer,
    powerful, immediate, constrained, regional).

% Analyze and articulate the principles of Indigenous law and treaty interpretation, advocating for the stewardship reading. They do not directly benefit or pay, but their work influences the legal and public discourse around the constraint.
narrative_ontology:constraint_stakeholder(historical_treaty_substrate__stewardship_reading, legal_scholars_indigenous_law, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a framework for shared territorial stewardship, ensuring mutual obligations for coexistence and sustainable resource management between Indigenous nations and settler states, preventing unilateral exploitation and conflict.
% TRANSFER_FUNCTION: Transfers decision-making authority and resource benefits from unilateral settler state control to a co-governed model, ensuring Indigenous nations' jurisdiction is respected and resources are managed for long-term ecological and community well-being.
% ABSENT_VOICES: Those who benefit from the extinguishment reading (e.g., certain historical land speculators or resource companies operating without Indigenous consent) are structurally excluded from this interpretation; they would argue against shared governance and for unfettered access to resources.
% DISAPPEARANCE_RATIONALE: If this reading of treaties vanished, the legal and political landscape would fundamentally shift. Indigenous nations would lose a key legal basis for asserting their rights and jurisdiction, leading to increased conflict over land and resources. The settler state would lose a framework for legitimate governance and reconciliation, leading to instability and moral discredit. Resource management would revert to unilateral, often unsustainable, practices.
% FOUNDING_PROBLEM: The historical problem was how to enable coexistence and shared use of lands and resources without extinguishing Indigenous sovereignty, ensuring mutual respect and long-term sustainability.
% FOUNDING_PROBLEM_CORROBORATION: Indigenous elders, legal scholars of Indigenous law, and international human rights bodies consistently corroborate that the problem of achieving genuine coexistence and stewardship, free from extinguishment, remains live and unresolved. This is attested through oral histories, legal challenges, and international declarations like UNDRIP.
narrative_ontology:disappearance_verdict(historical_treaty_substrate__stewardship_reading, world_rearranges).
narrative_ontology:founding_problem_status(historical_treaty_substrate__stewardship_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(historical_treaty_substrate__stewardship_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(historical_treaty_substrate__stewardship_reading, 'none', 1).
narrative_ontology:epsilon_provenance(historical_treaty_substrate__stewardship_reading, 0.3, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(historical_treaty_substrate__stewardship_reading_tests).
:- end_tests(historical_treaty_substrate__stewardship_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.3) because this reading emphasizes mutual benefit and shared responsibility, minimizing unilateral extraction. Suppression is low (0.2) as the constraint relies on consent and collaboration rather than coercion. Theater ratio is low (0.1) because the focus is on genuine co-management and relationship-building, not performative gestures. Resistance is high (0.7) because this reading is actively championed by Indigenous nations and their allies against dominant, more extractive interpretations. Accessibility collapse is moderate (0.4) as alternative, more extractive readings are still widely available and often legally privileged, but this reading offers a viable, principled alternative.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of Indigenous nations, this reading is a vindication of inherent rights and a path to genuine reconciliation. From the perspective of settler state governments, it represents a significant shift in power and responsibility, moving from unilateral control to shared authority. The engine's classification should reflect this shift towards a more equitable, coordination-focused arrangement compared to other readings.
 *
 * DIRECTIONALITY LOGIC:
 *   Indigenous nations are primary beneficiaries, retaining jurisdiction and participating in governance. Settler state governments are agenda-setters with obligations, benefiting from legitimacy and stability. Settler citizens benefit from a just society. Resource industries are payers, facing increased costs and shared decision-making. This reading aims for a more balanced distribution of costs and benefits, moving away from the high extraction of other interpretations.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading actively resists mandatrophy by re-centering the original, relational intent of treaties, preventing their degradation into mere instruments of land cession or administrative inconvenience. It ensures the mandate of mutual stewardship remains live and relevant, rather than atrophying into a performative exercise.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reading_legitimacy_status,
    'To what extent is the ''stewardship reading'' legally and politically recognized and implemented by settler state institutions, versus remaining an aspirational or contested interpretation?',
    'Analysis of judicial decisions, legislative enactments, and policy implementation over time, specifically looking for explicit adoption of co-management and consent principles.',
    'If widely recognized and implemented, the constraint operates as a genuine Rope or even a Mountain (in terms of its naturalness to Indigenous legal orders). If it remains largely aspirational, its effective extractiveness and suppression by the dominant legal order are higher, pushing it towards a Tangled Rope or Snare from the Indigenous perspective.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_legitimacy_status, empirical, 'The gap between the aspirational ''stewardship reading'' and its de facto institutionalization.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression (0.2) structural (e.g., legal precedents favoring extinguishment) or internalized (e.g., historical trauma impacting Indigenous nations'' capacity to assert rights)?',
    'Post-legal-reform trajectory: if suppression persists after legal barriers are removed, reclassify as partially internalized. This reading aims to reduce structural suppression, but internalized effects may linger.',
    'If internalized suppression is significant, the constraint''s effective suppression is higher than the structural measure suggests, as Indigenous nations carry historical burdens even when legal frameworks improve.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism in the context of historical power imbalances.').

omega_variable(
    kernel_framing_underdetermination,
    'Is the ''historical_treaty_substrate'' kernel best framed as a legal text, a relational pact, or a historical event?',
    'Analysis of the dominant interpretive traditions in legal anthropology and Indigenous legal theory. If a different framing (e.g., ''legal text'') were adopted, it would likely shift the classification towards a more formal, potentially extractive, interpretation.',
    'Framing as a ''legal text'' might emphasize formal interpretation and potentially support extinguishment readings, increasing extractiveness. Framing as a ''relational pact'' supports the stewardship reading, emphasizing ongoing obligations and co-management.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_framing_underdetermination, conceptual, 'The choice of kernel framing (text vs. relationship vs. event) influences the constraint''s structural properties and classification.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(historical_treaty_substrate__stewardship_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hist_tr_t0, historical_treaty_substrate__stewardship_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(hist_tr_t25, historical_treaty_substrate__stewardship_reading, theater_ratio, 25, 0.08).
narrative_ontology:measurement(hist_tr_t50, historical_treaty_substrate__stewardship_reading, theater_ratio, 50, 0.05).
narrative_ontology:measurement(hist_tr_t75, historical_treaty_substrate__stewardship_reading, theater_ratio, 75, 0.07).
narrative_ontology:measurement(hist_tr_t100, historical_treaty_substrate__stewardship_reading, theater_ratio, 100, 0.1).

% Extraction over time
narrative_ontology:measurement(hist_be_t0, historical_treaty_substrate__stewardship_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(hist_be_t25, historical_treaty_substrate__stewardship_reading, base_extractiveness, 25, 0.28).
narrative_ontology:measurement(hist_be_t50, historical_treaty_substrate__stewardship_reading, base_extractiveness, 50, 0.25).
narrative_ontology:measurement(hist_be_t75, historical_treaty_substrate__stewardship_reading, base_extractiveness, 75, 0.27).
narrative_ontology:measurement(hist_be_t100, historical_treaty_substrate__stewardship_reading, base_extractiveness, 100, 0.3).

% Suppression requirement over time
narrative_ontology:measurement(hist_su_t0, historical_treaty_substrate__stewardship_reading, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(hist_su_t25, historical_treaty_substrate__stewardship_reading, suppression_requirement, 25, 0.18).
narrative_ontology:measurement(hist_su_t50, historical_treaty_substrate__stewardship_reading, suppression_requirement, 50, 0.15).
narrative_ontology:measurement(hist_su_t75, historical_treaty_substrate__stewardship_reading, suppression_requirement, 75, 0.17).
narrative_ontology:measurement(hist_su_t100, historical_treaty_substrate__stewardship_reading, suppression_requirement, 100, 0.2).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(historical_treaty_substrate__stewardship_reading, identity_coordination).
narrative_ontology:affects_constraint(historical_treaty_substrate__stewardship_reading, historical_treaty_substrate__extinguishment_reading).
narrative_ontology:affects_constraint(historical_treaty_substrate__stewardship_reading, historical_treaty_substrate__nation_to_nation_reading).
narrative_ontology:affects_constraint(historical_treaty_substrate__stewardship_reading, resource_management_regulations).
narrative_ontology:affects_constraint(historical_treaty_substrate__stewardship_reading, indigenous_land_claims_processes).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'historical_treaty_substrate' kernel. This 'stewardship_reading' emphasizes mutual obligations and shared governance, contrasting with the 'extinguishment_reading' (cession of sovereignty) and the 'nation_to_nation_reading' (international agreement).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
