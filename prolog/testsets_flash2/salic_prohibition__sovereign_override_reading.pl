% ============================================================================
% CONSTRAINT STORY: salic_prohibition__sovereign_override_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_salic_prohibition__sovereign_override_reading, []).

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
 *   constraint_id: salic_prohibition__sovereign_override_reading
 *   human_readable: Salic Law as Revocable Positive Law (Sovereign Override Reading)
 *   domain: constitutional_law/dynastic_succession/political_history
 *
 * SUMMARY:
 *   This constraint story models the 'sovereign override' reading of Salic
 *   Law, where the law is understood as revocable positive law subject to the
 *   legislative authority of the sovereign (e.g., through a Pragmatic
 *   Sanction). This reading asserts that female succession is permissible via
 *   sovereign act, and challengers to such an act are rebels against
 *   legitimate authority. The period of 1713-1748 covers the initial
 *   promulgation of the Pragmatic Sanction by Charles VI and the War of the
 *   Austrian Succession, which tested its enforceability.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(salic_prohibition__sovereign_override_reading, 0.4).
domain_priors:suppression_score(salic_prohibition__sovereign_override_reading, 0.6).
domain_priors:theater_ratio(salic_prohibition__sovereign_override_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(salic_prohibition__sovereign_override_reading, extractiveness, 0.4).
narrative_ontology:constraint_metric(salic_prohibition__sovereign_override_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(salic_prohibition__sovereign_override_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(salic_prohibition__sovereign_override_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(salic_prohibition__sovereign_override_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(salic_prohibition__sovereign_override_reading, tangled_rope).
narrative_ontology:human_readable(salic_prohibition__sovereign_override_reading, "Salic Law as Revocable Positive Law (Sovereign Override Reading)").
narrative_ontology:topic_domain(salic_prohibition__sovereign_override_reading, "constitutional_law/dynastic_succession/political_history").

domain_priors:requires_active_enforcement(salic_prohibition__sovereign_override_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(salic_prohibition__sovereign_override_reading, 'b803ba59-4874-4439-85ef-16460472a70e').
narrative_ontology:cs_kernel_codification('b803ba59-4874-4439-85ef-16460472a70e', formalized).
narrative_ontology:cs_authority_grounding('b803ba59-4874-4439-85ef-16460472a70e', lineage).
narrative_ontology:cs_interpretation_layer_present('b803ba59-4874-4439-85ef-16460472a70e').
narrative_ontology:cs_reading_relation('b803ba59-4874-4439-85ef-16460472a70e', salic_prohibition__immutable_mandate_reading, coexists_with).
narrative_ontology:cs_reading_relation('b803ba59-4874-4439-85ef-16460472a70e', salic_prohibition__cognatic_reversion_reading, coexists_with).
narrative_ontology:cs_axiom('b803ba59-4874-4439-85ef-16460472a70e', foundational, sovereign_legislative_supremacy).
narrative_ontology:cs_axiom_status(sovereign_legislative_supremacy, holdable).
narrative_ontology:cs_axiom_grounding('b803ba59-4874-4439-85ef-16460472a70e', sovereign_legislative_supremacy, conventional).
narrative_ontology:cs_axiom('b803ba59-4874-4439-85ef-16460472a70e', foundational, dynastic_continuity_over_tradition).
narrative_ontology:cs_axiom_status(dynastic_continuity_over_tradition, holdable).
narrative_ontology:cs_axiom_grounding('b803ba59-4874-4439-85ef-16460472a70e', dynastic_continuity_over_tradition, instrumental).
narrative_ontology:cs_reference_frame('b803ba59-4874-4439-85ef-16460472a70e', pragmatic_sanction_as_valid_law).
narrative_ontology:cs_drift_state('b803ba59-4874-4439-85ef-16460472a70e', war_of_austrian_succession_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('b803ba59-4874-4439-85ef-16460472a70e', '').
narrative_ontology:cs_kernel_id(salic_prohibition__sovereign_override_reading, salic_prohibition).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(salic_prohibition__sovereign_override_reading, ruling_dynasty).
narrative_ontology:constraint_beneficiary(salic_prohibition__sovereign_override_reading, sovereign_legislature).
narrative_ontology:constraint_victim(salic_prohibition__sovereign_override_reading, dispossessed_male_claimants).
narrative_ontology:constraint_victim(salic_prohibition__sovereign_override_reading, rival_dynasties).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(salic_prohibition__sovereign_override_reading, loyal_subjects).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The reigning royal family, whose succession is secured by the sovereign's power to override Salic Law. They benefit from dynastic continuity and the stability of a clear succession plan, even if it means altering traditional rules. They actively enforce the Pragmatic Sanction.
narrative_ontology:constraint_stakeholder(salic_prohibition__sovereign_override_reading, ruling_dynasty, agenda_setter,
    institutional, generational, constrained, national).

% The legislative body or monarchical authority that asserts the right to modify or override Salic Law through acts like the Pragmatic Sanction. They benefit from demonstrating their supreme legislative authority and ensuring dynastic stability.
narrative_ontology:constraint_stakeholder(salic_prohibition__sovereign_override_reading, sovereign_legislature, agenda_setter,
    institutional, generational, mobile, national).

% Male relatives who would have inherited under strict Salic Law but are dispossessed by a sovereign act allowing female succession. They bear the cost of lost claims and often resort to rebellion or diplomatic pressure, viewing the override as illegitimate.
narrative_ontology:constraint_stakeholder(salic_prohibition__sovereign_override_reading, dispossessed_male_claimants, payer,
    powerful, biographical, trapped, national).

% Foreign royal houses or noble families who might have claims to the throne under different interpretations of succession law, or who seek to exploit dynastic instability. They bear the cost of a consolidated, legally robust succession that prevents their intervention.
narrative_ontology:constraint_stakeholder(salic_prohibition__sovereign_override_reading, rival_dynasties, payer,
    institutional, generational, constrained, regional).

% The general populace who benefit from dynastic stability and the avoidance of succession crises, even if it means accepting a female monarch. They are coordinated by the sovereign's authority and the desire for peace.
narrative_ontology:constraint_stakeholder(salic_prohibition__sovereign_override_reading, loyal_subjects, beneficiary,
    moderate, biographical, constrained, national).

% Academics who study the historical evolution of succession laws, the legal basis of sovereign authority, and the impact of acts like the Pragmatic Sanction. They analyze the constraint's operation without directly participating in its enforcement or bearing its costs.
narrative_ontology:constraint_stakeholder(salic_prohibition__sovereign_override_reading, historical_scholars, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Ensures a clear and stable line of succession for the monarchy, preventing civil war or foreign intervention that might arise from ambiguous or contested claims under strict Salic Law, by allowing the sovereign to adapt the rules.
% TRANSFER_FUNCTION: Transfers the right of succession from male-only claimants to a designated female heir (and her descendants), thereby transferring dynastic legitimacy and political power, as well as the associated revenues and authority, to the new line.
% ABSENT_VOICES: The historical voices of those who believed in the divine or immutable nature of Salic Law, and who would have challenged any sovereign override as illegitimate, are absent from the contemporary legal discourse, though their arguments persist in historical and philosophical debates.
% DISAPPEARANCE_RATIONALE: If the sovereign's right to override Salic Law vanished, the succession would immediately become contested, leading to multiple claimants, potential civil war, and severe political instability as the dynastic order would collapse into a state of nature regarding succession.
% FOUNDING_PROBLEM: The problem of dynastic instability and the potential for succession crises when a ruling house faced a lack of suitable male heirs, threatening the continuity of the state and inviting foreign intervention.
% FOUNDING_PROBLEM_CORROBORATION: Historical records and contemporary constitutional analyses corroborate that dynastic continuity and stability remain live concerns for monarchical states, and that the ability to adapt succession laws is a mechanism to address this, attested by constitutional scholars and political historians outside the immediate beneficiaries.
narrative_ontology:disappearance_verdict(salic_prohibition__sovereign_override_reading, world_rearranges).
narrative_ontology:founding_problem_status(salic_prohibition__sovereign_override_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(salic_prohibition__sovereign_override_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(salic_prohibition__sovereign_override_reading, 'none', 1).
narrative_ontology:epsilon_provenance(salic_prohibition__sovereign_override_reading, 0.4, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(salic_prohibition__sovereign_override_reading_tests).
:- end_tests(salic_prohibition__sovereign_override_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.4) as it dispossesses male claimants but secures broader dynastic stability. Suppression is high (0.6) because the sovereign override often requires military and diplomatic enforcement against internal and external challengers. Theater ratio is low (0.2) as the act of overriding Salic Law is a direct exercise of power with real consequences, not mere performance. The fluctuations in extractiveness, suppression, and theater reflect the period of contestation and war following the Pragmatic Sanction, where the costs of enforcement and the benefits of stability were actively being realized.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the ruling dynasty and sovereign legislature, this is a necessary act of coordination to ensure stability. From the perspective of dispossessed male claimants and rival dynasties, it is an act of pure extraction and usurpation, requiring active suppression to maintain.
 *
 * DIRECTIONALITY LOGIC:
 *   The ruling dynasty and sovereign legislature are clear beneficiaries (low d) as they gain stability and assert authority. Dispossessed male claimants and rival dynasties are targets (high d) as they lose claims and face military opposition. Loyal subjects are beneficiaries of stability, bearing diffuse costs of war but gaining peace.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    salic_prohibition_kernel_reading,
    'Is Salic Law an immutable natural/divine mandate, a revocable positive law, or an anachronism never truly binding?',
    'Analysis of historical legal texts, constitutional documents, and political outcomes across different European monarchies. The ''sovereign override'' reading is empirically supported by acts like the Pragmatic Sanction and their subsequent enforcement.',
    'If Salic Law were immutable (immutable_mandate_reading), the sovereign override would be illegitimate, reclassifying the constraint as a Snare for the ruling dynasty. If it were an anachronism (cognatic_reversion_reading), the override would be unnecessary, and the constraint would be a Rope or even a Mountain of natural succession.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(salic_prohibition_kernel_reading, conceptual, 'This constraint is one reading of the ''salic_prohibition'' kernel, specifically the ''sovereign_override_reading''. Sibling readings include ''immutable_mandate_reading'' and ''cognatic_reversion_reading''.').

omega_variable(
    legitimacy_of_sovereign_override,
    'To what extent was the sovereign''s authority to override Salic Law genuinely accepted by other European powers and internal factions, beyond mere military enforcement?',
    'Diplomatic correspondence, treaty negotiations, and internal political debates of the period. The degree of ''soft power'' acceptance versus ''hard power'' enforcement.',
    'Higher genuine acceptance would reduce the ''suppression'' metric and shift the classification closer to a Rope. Lower acceptance would increase ''suppression'' and ''theater_ratio'', pushing it towards a Snare or Piton, indicating the constraint''s persistence relied more on coercion than consensus.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legitimacy_of_sovereign_override, empirical, 'The true extent of legitimacy for the sovereign override, beyond military might.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(salic_prohibition__sovereign_override_reading, 1713, 1748).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sali_tr_t1713, salic_prohibition__sovereign_override_reading, theater_ratio, 1713, 0.1).
narrative_ontology:measurement(sali_tr_t1720, salic_prohibition__sovereign_override_reading, theater_ratio, 1720, 0.15).
narrative_ontology:measurement(sali_tr_t1730, salic_prohibition__sovereign_override_reading, theater_ratio, 1730, 0.2).
narrative_ontology:measurement(sali_tr_t1740, salic_prohibition__sovereign_override_reading, theater_ratio, 1740, 0.25).
narrative_ontology:measurement(sali_tr_t1748, salic_prohibition__sovereign_override_reading, theater_ratio, 1748, 0.2).

% Extraction over time
narrative_ontology:measurement(sali_be_t1713, salic_prohibition__sovereign_override_reading, base_extractiveness, 1713, 0.3).
narrative_ontology:measurement(sali_be_t1720, salic_prohibition__sovereign_override_reading, base_extractiveness, 1720, 0.35).
narrative_ontology:measurement(sali_be_t1730, salic_prohibition__sovereign_override_reading, base_extractiveness, 1730, 0.4).
narrative_ontology:measurement(sali_be_t1740, salic_prohibition__sovereign_override_reading, base_extractiveness, 1740, 0.45).
narrative_ontology:measurement(sali_be_t1748, salic_prohibition__sovereign_override_reading, base_extractiveness, 1748, 0.4).

% Suppression requirement over time
narrative_ontology:measurement(sali_su_t1713, salic_prohibition__sovereign_override_reading, suppression_requirement, 1713, 0.5).
narrative_ontology:measurement(sali_su_t1720, salic_prohibition__sovereign_override_reading, suppression_requirement, 1720, 0.55).
narrative_ontology:measurement(sali_su_t1730, salic_prohibition__sovereign_override_reading, suppression_requirement, 1730, 0.6).
narrative_ontology:measurement(sali_su_t1740, salic_prohibition__sovereign_override_reading, suppression_requirement, 1740, 0.65).
narrative_ontology:measurement(sali_su_t1748, salic_prohibition__sovereign_override_reading, suppression_requirement, 1748, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(salic_prohibition__sovereign_override_reading, enforcement_mechanism).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'salic_prohibition' kernel, each representing a distinct structural claim about the nature and enforceability of Salic Law. This reading (sovereign_override_reading) asserts the sovereign's legislative power to alter succession rules.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
