% ============================================================================
% CONSTRAINT STORY: historical_treaty_substrate__nation_to_nation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_historical_treaty_substrate__nation_to_nation_reading, []).

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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: historical_treaty_substrate__nation_to_nation_reading
 *   human_readable: Nation-to-Nation Treaty Interpretation
 *   domain: legal_anthropology/indigenous_law/comparative_constitutional_theory
 *
 * SUMMARY:
 *   This constraint represents the 'nation-to-nation' reading of historical
 *   treaties, asserting them as international agreements between sovereign
 *   equals, requiring ongoing consent and subject to modern treaty law
 *   principles. This reading challenges the unilateral assertion of settler
 *   state sovereignty and resource control, advocating for a consensual
 *   framework. The claimed type is 'rope' as it describes an ideal
 *   coordination mechanism, but the high suppression and resistance metrics
 *   reflect the ongoing struggle for its full recognition and implementation
 *   against entrenched settler state interests.
 *
 * KEY AGENTS:
 *   - Indigenous_nations: Primary beneficiary (organized/constrained) — gain recognition and consent rights
 *   - settler_state_governments: Primary payer/agenda_setter (institutional/constrained) — bear costs of compliance, constrained in unilateral action
 *   - settler_state_resource_industries: Payer (powerful/constrained) — face increased costs for consent and consultation
 *   - international_law_advocates: Beneficiary/observer (organized/mobile) — support and reinforce this reading
 *   - extinguishment_reading_proponents: Excluded (organized/identity_locked) — actively resist this reading
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(historical_treaty_substrate__nation_to_nation_reading, 0.25).
domain_priors:suppression_score(historical_treaty_substrate__nation_to_nation_reading, 0.75).
domain_priors:theater_ratio(historical_treaty_substrate__nation_to_nation_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(historical_treaty_substrate__nation_to_nation_reading, extractiveness, 0.25).
narrative_ontology:constraint_metric(historical_treaty_substrate__nation_to_nation_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(historical_treaty_substrate__nation_to_nation_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(historical_treaty_substrate__nation_to_nation_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(historical_treaty_substrate__nation_to_nation_reading, resistance, 0.8).

% --- Constraint claim ---
narrative_ontology:constraint_claim(historical_treaty_substrate__nation_to_nation_reading, rope).
narrative_ontology:human_readable(historical_treaty_substrate__nation_to_nation_reading, "Nation-to-Nation Treaty Interpretation").
narrative_ontology:topic_domain(historical_treaty_substrate__nation_to_nation_reading, "legal_anthropology/indigenous_law/comparative_constitutional_theory").

domain_priors:requires_active_enforcement(historical_treaty_substrate__nation_to_nation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(historical_treaty_substrate__nation_to_nation_reading, 'cb4af6dd-4ad4-430a-92e2-6f9fceb2fbb5').
narrative_ontology:cs_kernel_codification('cb4af6dd-4ad4-430a-92e2-6f9fceb2fbb5', fixed_text).
narrative_ontology:cs_authority_grounding('cb4af6dd-4ad4-430a-92e2-6f9fceb2fbb5', lineage).
narrative_ontology:cs_interpretation_layer_present('cb4af6dd-4ad4-430a-92e2-6f9fceb2fbb5').
narrative_ontology:cs_reading_relation('cb4af6dd-4ad4-430a-92e2-6f9fceb2fbb5', historical_treaty_substrate__extinguishment_reading, forecloses).
narrative_ontology:cs_reading_relation('cb4af6dd-4ad4-430a-92e2-6f9fceb2fbb5', historical_treaty_substrate__stewardship_reading, coexists_with).
narrative_ontology:cs_axiom('cb4af6dd-4ad4-430a-92e2-6f9fceb2fbb5', foundational, indigenous_nations_retain_inherent_sovereignty).
narrative_ontology:cs_axiom_status(indigenous_nations_retain_inherent_sovereignty, holdable).
narrative_ontology:cs_axiom_grounding('cb4af6dd-4ad4-430a-92e2-6f9fceb2fbb5', indigenous_nations_retain_inherent_sovereignty, deontological).
narrative_ontology:cs_axiom('cb4af6dd-4ad4-430a-92e2-6f9fceb2fbb5', foundational, treaties_are_international_agreements).
narrative_ontology:cs_axiom_status(treaties_are_international_agreements, holdable).
narrative_ontology:cs_axiom_grounding('cb4af6dd-4ad4-430a-92e2-6f9fceb2fbb5', treaties_are_international_agreements, conventional).
narrative_ontology:cs_reference_frame('cb4af6dd-4ad4-430a-92e2-6f9fceb2fbb5', pre_colonial_sovereignty_and_international_law).
narrative_ontology:cs_drift_state('cb4af6dd-4ad4-430a-92e2-6f9fceb2fbb5', contemporary_settler_state_jurisprudence, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('cb4af6dd-4ad4-430a-92e2-6f9fceb2fbb5', '').
narrative_ontology:cs_kernel_id(historical_treaty_substrate__nation_to_nation_reading, historical_treaty_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(historical_treaty_substrate__nation_to_nation_reading, indigenous_nations).
narrative_ontology:constraint_beneficiary(historical_treaty_substrate__nation_to_nation_reading, international_law_advocates).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(historical_treaty_substrate__nation_to_nation_reading, settler_state_governments).
narrative_ontology:constraint_victim(historical_treaty_substrate__nation_to_nation_reading, settler_state_resource_industries).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from the recognition of their inherent sovereignty and the requirement for their ongoing consent on matters affecting their territories. They actively advocate for this reading in domestic and international forums, seeking to enforce treaty obligations.
narrative_ontology:constraint_stakeholder(historical_treaty_substrate__nation_to_nation_reading, indigenous_nations, beneficiary,
    organized, generational, constrained, national).

% Bear the costs of compliance, negotiation, and potential litigation arising from this interpretation. They are constrained in their unilateral exercise of sovereignty and resource management, requiring engagement and consent from Indigenous nations. They also administer the legal system that may or may not uphold this reading.
narrative_ontology:constraint_stakeholder(historical_treaty_substrate__nation_to_nation_reading, settler_state_governments, payer,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(historical_treaty_substrate__nation_to_nation_reading, settler_state_governments, agenda_setter).

% Face increased costs and delays due to the requirement for Indigenous consent and consultation on resource projects. Their prior assumption of unhindered access to resources on treaty lands is challenged, making them 'payers' of the constraint's operational costs.
narrative_ontology:constraint_stakeholder(historical_treaty_substrate__nation_to_nation_reading, settler_state_resource_industries, payer,
    powerful, biographical, constrained, national).

% Benefit from the application of international human rights and treaty law principles to Indigenous-settler relations. They provide legal and political support to Indigenous nations, reinforcing the legitimacy of this reading on a global stage.
narrative_ontology:constraint_stakeholder(historical_treaty_substrate__nation_to_nation_reading, international_law_advocates, beneficiary,
    organized, biographical, mobile, global).
narrative_ontology:stakeholder_secondary_role(historical_treaty_substrate__nation_to_nation_reading, international_law_advocates, observer).

% Are indirectly affected by the legal and political shifts, potentially through changes in resource development, land use, and national identity. Their understanding of national history and sovereignty is challenged by this reading.
narrative_ontology:constraint_stakeholder(historical_treaty_substrate__nation_to_nation_reading, settler_state_citizens, observer,
    moderate, biographical, mobile, national).

% Are structurally excluded from the framework of this reading, as their core premise of Indigenous sovereignty cession is rejected. They would argue this reading undermines the foundational claims of settler state sovereignty and economic development, and actively resist its adoption.
narrative_ontology:constraint_stakeholder(historical_treaty_substrate__nation_to_nation_reading, extinguishment_reading_proponents, excluded,
    organized, biographical, identity_locked, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a framework for respectful, consensual relations and shared governance over lands and resources between Indigenous nations and settler states, preventing unilateral action and conflict by requiring ongoing consent and adherence to international law.
% TRANSFER_FUNCTION: Transfers decision-making power and resource control from unilateral settler state authority to a shared, consensual framework with Indigenous nations. It also transfers legitimacy to Indigenous governance structures and legal orders.
% ABSENT_VOICES: Proponents of the extinguishment reading are structurally excluded from this framework, as their core premise (cession of sovereignty) is rejected. They would argue this reading undermines settler state sovereignty and economic development, and actively resist its adoption.
% DISAPPEARANCE_RATIONALE: If this reading vanished overnight, the legal and political landscape regarding Indigenous rights and land claims would revert to a more colonial interpretation. This would lead to increased conflict, unilateral resource extraction by settler states, and a significant loss of legal and political leverage for Indigenous nations, fundamentally reorganizing the relationship.
% FOUNDING_PROBLEM: The historical and ongoing conflict arising from the unilateral assertion of sovereignty by settler states over Indigenous territories, leading to dispossession, resource exploitation, and denial of Indigenous self-determination, despite existing treaties.
% FOUNDING_PROBLEM_CORROBORATION: Indigenous legal scholars, international human rights bodies, and some progressive legal precedents corroborate that the problem of colonial power imbalances and unconsented resource extraction remains live. This corroboration comes from outside the immediate benefiting parties, lending independent weight to the claim.
narrative_ontology:disappearance_verdict(historical_treaty_substrate__nation_to_nation_reading, world_rearranges).
narrative_ontology:founding_problem_status(historical_treaty_substrate__nation_to_nation_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(historical_treaty_substrate__nation_to_nation_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(historical_treaty_substrate__nation_to_nation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(historical_treaty_substrate__nation_to_nation_reading, 0.25, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(historical_treaty_substrate__nation_to_nation_reading_tests).
:- end_tests(historical_treaty_substrate__nation_to_nation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The base extractiveness is low (0.25) because this reading, by its very nature, aims to prevent extraction without consent and establish equitable relations. However, the suppression (0.75) is high because settler state legal and political systems actively resist and marginalize this interpretation, preferring readings that uphold their unilateral authority. Resistance (0.80) is also high, reflecting the vigorous advocacy by Indigenous nations and their allies, met by strong opposition from settler state interests. The theater ratio (0.45) has increased over time, indicating that while settler states may pay lip service to 'nation-to-nation' relations, their actual practices often fall short, maintaining a performative aspect to engagement without full implementation of consent principles. The accessibility collapse is low (0.30) because this reading actively opens alternatives and legal avenues for Indigenous nations, though these paths are often difficult to pursue due to suppression.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of Indigenous nations and international law advocates, this reading is a vital framework for justice and coordination. From the perspective of settler state governments and resource industries, it is a constraint that imposes costs and limits their perceived sovereignty. The engine's computation will highlight this divergence, showing a 'rope' for beneficiaries and a 'tangled_rope' or 'snare' for those whose unilateral power is challenged, despite the constraint's ideal 'rope' claim.
 *
 * DIRECTIONALITY LOGIC:
 *   Indigenous nations are clear beneficiaries, gaining recognition of sovereignty and consent rights. International law advocates also benefit from the application of their principles. Settler state governments and resource industries are payers, as this reading constrains their prior unilateral power and imposes costs of negotiation and compliance. The 'excluded' proponents of the extinguishment reading are those whose worldview is directly challenged and whose interests are undermined by this interpretation.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification prevents mislabeling the constraint. While the settler state might frame its engagement as 'coordination,' the high suppression and resistance, coupled with the identified payers, reveal the ongoing contestation. The 'rope' claim reflects the ideal, while the metrics capture the reality of its contested implementation, preventing it from being dismissed as pure extraction or a fully functional coordination mechanism.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    rhetoric_vs_practice_gap,
    'To what extent is the ''nation-to-nation'' rhetoric adopted by settler states matched by actual changes in policy and practice regarding Indigenous consent and resource governance?',
    'Empirical analysis of legislative changes, court decisions, resource project approvals, and Indigenous-settler negotiation outcomes over time, comparing stated policy with implemented practice.',
    'If the gap is wide, the constraint''s effective extractiveness (from Indigenous nations) and theater_ratio are higher than currently measured, indicating a performative rather than substantive shift. This would push the computed type for Indigenous nations towards ''tangled_rope'' or ''snare''.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(rhetoric_vs_practice_gap, empirical, 'Assesses the sincerity and effectiveness of settler state adoption of the nation-to-nation principle.').

omega_variable(
    sovereignty_reconciliation_ambiguity,
    'How do settler state legal systems reconcile the inherent and ongoing sovereignty of Indigenous nations (as per this reading) with their own constitutional claims of plenary sovereignty?',
    'Conceptual analysis of judicial reasoning, legislative intent, and constitutional theory within settler states. Resolution would involve identifying specific legal doctrines or interpretive frameworks used for reconciliation or denial.',
    'If reconciliation is deemed impossible or consistently favors settler state claims, the suppression metric for this reading is structurally higher, and its long-term viability as a ''rope'' within domestic law is challenged, pushing it towards a ''scaffold'' or ''piton'' if its function atrophies.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sovereignty_reconciliation_ambiguity, conceptual, 'Examines the fundamental legal tension between competing claims of sovereignty.').

omega_variable(
    scope_of_ongoing_consent,
    'What is the practical and legal scope of ''ongoing consent'' required by this reading? Does it imply a veto, a right to consultation, or something else?',
    'Case law analysis, comparative legal studies of Free, Prior, and Informed Consent (FPIC) implementation, and ethnographic studies of Indigenous governance practices. Resolution would clarify the specific mechanisms and thresholds for consent.',
    'A narrow interpretation of consent (e.g., mere consultation) would increase the effective extractiveness from Indigenous nations, as their agency is limited. A broad interpretation (e.g., veto power) would reinforce the ''rope'' classification by ensuring genuine coordination and preventing unilateral action.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scope_of_ongoing_consent, empirical, 'Clarifies the practical implications of the consent requirement.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(historical_treaty_substrate__nation_to_nation_reading, 1970, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hist_tr_t1970, historical_treaty_substrate__nation_to_nation_reading, theater_ratio, 1970, 0.1).
narrative_ontology:measurement(hist_tr_t1980, historical_treaty_substrate__nation_to_nation_reading, theater_ratio, 1980, 0.2).
narrative_ontology:measurement(hist_tr_t1990, historical_treaty_substrate__nation_to_nation_reading, theater_ratio, 1990, 0.3).
narrative_ontology:measurement(hist_tr_t2000, historical_treaty_substrate__nation_to_nation_reading, theater_ratio, 2000, 0.38).
narrative_ontology:measurement(hist_tr_t2010, historical_treaty_substrate__nation_to_nation_reading, theater_ratio, 2010, 0.42).
narrative_ontology:measurement(hist_tr_t2020, historical_treaty_substrate__nation_to_nation_reading, theater_ratio, 2020, 0.45).

% Extraction over time
narrative_ontology:measurement(hist_be_t1970, historical_treaty_substrate__nation_to_nation_reading, base_extractiveness, 1970, 0.35).
narrative_ontology:measurement(hist_be_t1980, historical_treaty_substrate__nation_to_nation_reading, base_extractiveness, 1980, 0.3).
narrative_ontology:measurement(hist_be_t1990, historical_treaty_substrate__nation_to_nation_reading, base_extractiveness, 1990, 0.28).
narrative_ontology:measurement(hist_be_t2000, historical_treaty_substrate__nation_to_nation_reading, base_extractiveness, 2000, 0.27).
narrative_ontology:measurement(hist_be_t2010, historical_treaty_substrate__nation_to_nation_reading, base_extractiveness, 2010, 0.26).
narrative_ontology:measurement(hist_be_t2020, historical_treaty_substrate__nation_to_nation_reading, base_extractiveness, 2020, 0.25).

% Suppression requirement over time
narrative_ontology:measurement(hist_su_t1970, historical_treaty_substrate__nation_to_nation_reading, suppression_requirement, 1970, 0.85).
narrative_ontology:measurement(hist_su_t1980, historical_treaty_substrate__nation_to_nation_reading, suppression_requirement, 1980, 0.8).
narrative_ontology:measurement(hist_su_t1990, historical_treaty_substrate__nation_to_nation_reading, suppression_requirement, 1990, 0.78).
narrative_ontology:measurement(hist_su_t2000, historical_treaty_substrate__nation_to_nation_reading, suppression_requirement, 2000, 0.76).
narrative_ontology:measurement(hist_su_t2010, historical_treaty_substrate__nation_to_nation_reading, suppression_requirement, 2010, 0.75).
narrative_ontology:measurement(hist_su_t2020, historical_treaty_substrate__nation_to_nation_reading, suppression_requirement, 2020, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(historical_treaty_substrate__nation_to_nation_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(historical_treaty_substrate__nation_to_nation_reading, historical_treaty_substrate__extinguishment_reading).
narrative_ontology:affects_constraint(historical_treaty_substrate__nation_to_nation_reading, historical_treaty_substrate__stewardship_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'historical_treaty_substrate' kernel. This 'nation-to-nation' reading emphasizes Indigenous sovereignty and international law, contrasting with the 'extinguishment' reading (cession of sovereignty) and the 'stewardship' reading (relational pacts for shared governance).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
