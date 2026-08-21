% ============================================================================
% CONSTRAINT STORY: commerce_clause_text__expansive_federal_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_commerce_clause_text__expansive_federal_reading, []).

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
 *   constraint_id: commerce_clause_text__expansive_federal_reading
 *   human_readable: Expansive Federal Commerce Clause Interpretation
 *   domain: constitutional_law/federalism/commerce_regulation
 *
 * SUMMARY:
 *   This constraint story instantiates the 'expansive_federal_reading' of the
 *   Commerce Clause, which interprets 'interstate commerce' to include all
 *   economic activity with a substantial aggregate effect on national
 *   markets. This reading, largely solidified during the New Deal era, has
 *   enabled vast federal regulation of the economy, centralizing power and
 *   subordinating state regulatory autonomy. It is presented as a Tangled
 *   Rope because it genuinely coordinates national markets but does so
 *   through significant extraction of state power, maintained by active
 *   federal enforcement.
 *
 * KEY AGENTS:
 *   - federal_administrative_state: Primary agenda_setter (institutional/arbitrage) — benefits from expanded jurisdiction.
 *   - state_regulatory_autonomy: Primary payer (institutional/constrained) — bears the cost of federal preemption.
 *   - supreme_court: Agenda_setter (institutional/analytical) — defines and enforces the interpretation.
 *   - originalist_legal_scholars: Excluded (analytical/analytical) — critical voices outside the dominant framework.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(commerce_clause_text__expansive_federal_reading, 0.68).
domain_priors:suppression_score(commerce_clause_text__expansive_federal_reading, 0.75).
domain_priors:theater_ratio(commerce_clause_text__expansive_federal_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(commerce_clause_text__expansive_federal_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(commerce_clause_text__expansive_federal_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(commerce_clause_text__expansive_federal_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(commerce_clause_text__expansive_federal_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(commerce_clause_text__expansive_federal_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(commerce_clause_text__expansive_federal_reading, tangled_rope).
narrative_ontology:human_readable(commerce_clause_text__expansive_federal_reading, "Expansive Federal Commerce Clause Interpretation").
narrative_ontology:topic_domain(commerce_clause_text__expansive_federal_reading, "constitutional_law/federalism/commerce_regulation").

domain_priors:requires_active_enforcement(commerce_clause_text__expansive_federal_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(commerce_clause_text__expansive_federal_reading, 'ee1ac926-3856-4f5b-a4b2-341370bbf6db').
narrative_ontology:cs_kernel_codification('ee1ac926-3856-4f5b-a4b2-341370bbf6db', fixed_text).
narrative_ontology:cs_authority_grounding('ee1ac926-3856-4f5b-a4b2-341370bbf6db', lineage).
narrative_ontology:cs_interpretation_layer_present('ee1ac926-3856-4f5b-a4b2-341370bbf6db').
narrative_ontology:cs_reading_relation('ee1ac926-3856-4f5b-a4b2-341370bbf6db', commerce_clause_text__originalist_narrow_reading, forecloses).
narrative_ontology:cs_reading_relation('ee1ac926-3856-4f5b-a4b2-341370bbf6db', commerce_clause_text__substantial_effects_limited_reading, coexists_with).
narrative_ontology:cs_axiom('ee1ac926-3856-4f5b-a4b2-341370bbf6db', foundational, economic_interdependence_justifies_national_regulation).
narrative_ontology:cs_axiom_status(economic_interdependence_justifies_national_regulation, holdable).
narrative_ontology:cs_axiom_grounding('ee1ac926-3856-4f5b-a4b2-341370bbf6db', economic_interdependence_justifies_national_regulation, empirically_contingent).
narrative_ontology:cs_axiom('ee1ac926-3856-4f5b-a4b2-341370bbf6db', foundational, federal_power_to_address_national_problems).
narrative_ontology:cs_axiom_status(federal_power_to_address_national_problems, holdable).
narrative_ontology:cs_axiom_grounding('ee1ac926-3856-4f5b-a4b2-341370bbf6db', federal_power_to_address_national_problems, conventional).
narrative_ontology:cs_reference_frame('ee1ac926-3856-4f5b-a4b2-341370bbf6db', new_deal_era_federal_power).
narrative_ontology:cs_drift_state('ee1ac926-3856-4f5b-a4b2-341370bbf6db', contemporary_era, gap(stable, minor, true)).
narrative_ontology:cs_created_at('ee1ac926-3856-4f5b-a4b2-341370bbf6db', '').
narrative_ontology:cs_kernel_id(commerce_clause_text__expansive_federal_reading, commerce_clause_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(commerce_clause_text__expansive_federal_reading, federal_administrative_state).
narrative_ontology:constraint_beneficiary(commerce_clause_text__expansive_federal_reading, national_policy_coherence_advocates).
narrative_ontology:constraint_beneficiary(commerce_clause_text__expansive_federal_reading, large_national_corporations).
narrative_ontology:constraint_victim(commerce_clause_text__expansive_federal_reading, state_regulatory_autonomy).
narrative_ontology:constraint_victim(commerce_clause_text__expansive_federal_reading, local_variation_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefits from expanded jurisdiction over economic activity, allowing for uniform national regulation in areas like environmental protection, labor standards, and healthcare. Actively defends and expands this interpretation through litigation and policy-making.
narrative_ontology:constraint_stakeholder(commerce_clause_text__expansive_federal_reading, federal_administrative_state, agenda_setter,
    institutional, generational, arbitrage, national).

% Advocates for federal solutions to national problems, believing that a broad Commerce Clause is essential for effective governance and preventing a 'race to the bottom' among states. They benefit from the ability to enact and enforce uniform standards.
narrative_ontology:constraint_stakeholder(commerce_clause_text__expansive_federal_reading, national_policy_coherence_advocates, beneficiary,
    organized, generational, mobile, national).

% Prefer a single set of federal regulations over a patchwork of 50 state laws, reducing compliance costs and facilitating interstate operations. They lobby for federal preemption and benefit from the predictability of national standards.
narrative_ontology:constraint_stakeholder(commerce_clause_text__expansive_federal_reading, large_national_corporations, beneficiary,
    powerful, biographical, arbitrage, global).

% Represents the collective interest of states in retaining their traditional police powers and regulatory authority over intrastate economic activity. This interpretation often preempts state laws, limiting their ability to respond to local needs or experiment with different policies.
narrative_ontology:constraint_stakeholder(commerce_clause_text__expansive_federal_reading, state_regulatory_autonomy, payer,
    institutional, generational, constrained, national).

% Advocates for local control and diversity in economic regulation, arguing that a one-size-fits-all federal approach can be unresponsive to unique local conditions and preferences. They bear the cost of federal preemption and reduced local self-governance.
narrative_ontology:constraint_stakeholder(commerce_clause_text__expansive_federal_reading, local_variation_advocates, payer,
    organized, biographical, constrained, local).

% Argue that the expansive interpretation departs from the original meaning of the Commerce Clause, which they contend was limited to direct trade crossing state lines. While influential in academic and judicial discourse, their views are largely excluded from the practical application of the expansive reading.
narrative_ontology:constraint_stakeholder(commerce_clause_text__expansive_federal_reading, originalist_legal_scholars, excluded,
    analytical, generational, analytical, national).

% The ultimate arbiter of the Commerce Clause's scope, whose precedents define the boundaries of federal power. While it has occasionally reined in the expansive reading, its overall jurisprudence has largely affirmed the 'substantial effects' doctrine.
narrative_ontology:constraint_stakeholder(commerce_clause_text__expansive_federal_reading, supreme_court, agenda_setter,
    institutional, civilizational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(commerce_clause_text__expansive_federal_reading, federal_administrative_state).
narrative_ontology:fixing_cost_class(commerce_clause_text__expansive_federal_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To coordinate national economic activity, prevent states from erecting trade barriers, and enable the federal government to address economic problems that transcend state lines, ensuring a coherent national market.
% TRANSFER_FUNCTION: Transfers significant regulatory authority and policy-making power from individual states to the federal government, centralizing control over a vast array of economic activities.
% ABSENT_VOICES: Originalist legal scholars, states' rights advocates, and local communities who prefer localized governance and believe the federal government has overstepped its constitutional bounds. Their arguments for a narrower interpretation are often marginalized in federal policy debates.
% DISAPPEARANCE_RATIONALE: If this interpretation vanished overnight, federal agencies would lose jurisdiction over vast areas of economic activity, leading to a regulatory vacuum or a chaotic patchwork of state laws. National markets would fragment, and many federal programs (e.g., environmental, labor, civil rights) would be severely curtailed or invalidated, fundamentally reorganizing the balance of power in the US.
% FOUNDING_PROBLEM: The economic balkanization and interstate trade wars under the Articles of Confederation, where states imposed tariffs and trade barriers on each other, hindering national economic development and unity.
% FOUNDING_PROBLEM_CORROBORATION: Federal agencies, national business groups, and many constitutional scholars attest to the ongoing need for national economic regulation to prevent balkanization and ensure the functioning of national markets. While the specific nature of the 'problem' is contested, the need for national coordination is widely accepted outside of states' rights advocacy groups.
narrative_ontology:disappearance_verdict(commerce_clause_text__expansive_federal_reading, world_rearranges).
narrative_ontology:founding_problem_status(commerce_clause_text__expansive_federal_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(commerce_clause_text__expansive_federal_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(commerce_clause_text__expansive_federal_reading, 'none', 1).
narrative_ontology:epsilon_provenance(commerce_clause_text__expansive_federal_reading, 0.68, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(commerce_clause_text__expansive_federal_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(commerce_clause_text__expansive_federal_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(commerce_clause_text__expansive_federal_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.68) because the federal government gains substantial regulatory power and resources at the expense of states. Suppression is high (0.75) as states' ability to regulate their own economies is significantly curtailed, and federal courts actively enforce this preemption. Theater ratio is low (0.10) because the interpretation is a functional legal doctrine, not primarily performative. The temporal measurements show a rise in extractiveness and suppression from the New Deal era as the interpretation expanded, stabilizing in recent decades despite some judicial pushback.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of federal agencies and national corporations, this interpretation is a necessary Rope, providing efficient coordination for a complex national economy. From the perspective of states and local advocates, it functions as a Snare or Tangled Rope, extracting their sovereign power and imposing uniform rules that may not fit local conditions. The engine's computation of per-seat types will reflect this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   The federal administrative state and national policy advocates are clear beneficiaries, gaining power and coherence. Large national corporations also benefit from reduced compliance complexity. State regulatory autonomy and local variation advocates are victims, losing power and flexibility. Originalist legal scholars are excluded, as their alternative interpretation is not structurally accommodated within the dominant framework.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate (preventing economic balkanization) is still live, but its expansive application has arguably outgrown the original problem, leading to extraction of state power. The classification as Tangled Rope acknowledges both the coordination function and the asymmetric extraction, preventing it from being mislabeled as a pure Rope (ignoring state costs) or a pure Snare (ignoring national coordination benefits).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    aggregate_effects_scope_ambiguity,
    'What constitutes a ''substantial aggregate effect'' on interstate commerce, and how far can this doctrine extend to non-economic activity?',
    'Further Supreme Court jurisprudence clarifying the limits of the ''substantial effects'' test, particularly concerning non-economic activity or activities traditionally regulated by states.',
    'A narrower judicial interpretation would reduce federal extractiveness and suppression, potentially reclassifying the constraint closer to a Rope or even a Scaffold if a clear sunset or limitation were imposed. A broader interpretation would increase extraction and suppression.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(aggregate_effects_scope_ambiguity, conceptual, 'Ambiguity in the scope of ''substantial aggregate effects'' doctrine.').

omega_variable(
    federalism_balance_preference,
    'What is the optimal balance between national uniformity and state autonomy in economic regulation, and does this interpretation achieve it?',
    'This is a preference-based question, resolvable through political processes, constitutional amendments, or a sustained shift in judicial philosophy reflecting societal values regarding federalism.',
    'If a preference for greater state autonomy prevails, the current level of federal extraction would be deemed excessive, leading to pressure for reinterpretation or legislative changes. If national uniformity is prioritized, the current interpretation would be seen as appropriate.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(federalism_balance_preference, preference, 'Preference for federal vs. state power balance.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(commerce_clause_text__expansive_federal_reading, 1937, 2023).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comm_tr_t1937, commerce_clause_text__expansive_federal_reading, theater_ratio, 1937, 0.05).
narrative_ontology:measurement(comm_tr_t1950, commerce_clause_text__expansive_federal_reading, theater_ratio, 1950, 0.08).
narrative_ontology:measurement(comm_tr_t1970, commerce_clause_text__expansive_federal_reading, theater_ratio, 1970, 0.1).
narrative_ontology:measurement(comm_tr_t1990, commerce_clause_text__expansive_federal_reading, theater_ratio, 1990, 0.12).
narrative_ontology:measurement(comm_tr_t2010, commerce_clause_text__expansive_federal_reading, theater_ratio, 2010, 0.1).
narrative_ontology:measurement(comm_tr_t2023, commerce_clause_text__expansive_federal_reading, theater_ratio, 2023, 0.1).

% Extraction over time
narrative_ontology:measurement(comm_be_t1937, commerce_clause_text__expansive_federal_reading, base_extractiveness, 1937, 0.4).
narrative_ontology:measurement(comm_be_t1950, commerce_clause_text__expansive_federal_reading, base_extractiveness, 1950, 0.55).
narrative_ontology:measurement(comm_be_t1970, commerce_clause_text__expansive_federal_reading, base_extractiveness, 1970, 0.65).
narrative_ontology:measurement(comm_be_t1990, commerce_clause_text__expansive_federal_reading, base_extractiveness, 1990, 0.7).
narrative_ontology:measurement(comm_be_t2010, commerce_clause_text__expansive_federal_reading, base_extractiveness, 2010, 0.68).
narrative_ontology:measurement(comm_be_t2023, commerce_clause_text__expansive_federal_reading, base_extractiveness, 2023, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(comm_su_t1937, commerce_clause_text__expansive_federal_reading, suppression_requirement, 1937, 0.5).
narrative_ontology:measurement(comm_su_t1950, commerce_clause_text__expansive_federal_reading, suppression_requirement, 1950, 0.65).
narrative_ontology:measurement(comm_su_t1970, commerce_clause_text__expansive_federal_reading, suppression_requirement, 1970, 0.75).
narrative_ontology:measurement(comm_su_t1990, commerce_clause_text__expansive_federal_reading, suppression_requirement, 1990, 0.8).
narrative_ontology:measurement(comm_su_t2010, commerce_clause_text__expansive_federal_reading, suppression_requirement, 2010, 0.75).
narrative_ontology:measurement(comm_su_t2023, commerce_clause_text__expansive_federal_reading, suppression_requirement, 2023, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(commerce_clause_text__expansive_federal_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(commerce_clause_text__expansive_federal_reading, federal_environmental_regulation).
narrative_ontology:affects_constraint(commerce_clause_text__expansive_federal_reading, national_labor_standards).
narrative_ontology:affects_constraint(commerce_clause_text__expansive_federal_reading, federal_healthcare_mandates).
narrative_ontology:affects_constraint(commerce_clause_text__expansive_federal_reading, commerce_clause_text__originalist_narrow_reading).
narrative_ontology:affects_constraint(commerce_clause_text__expansive_federal_reading, commerce_clause_text__substantial_effects_limited_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three distinct readings of the 'commerce_clause_text' kernel, each with different structural properties and classifications. They are linked to show their interdependency and contestation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
