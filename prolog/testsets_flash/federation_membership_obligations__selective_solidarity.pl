% ============================================================================
% CONSTRAINT STORY: federation_membership_obligations__selective_solidarity
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_federation_membership_obligations__selective_solidarity, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: federation_membership_obligations__selective_solidarity
 *   human_readable: Federation Membership Obligations: Selective Solidarity Reading
 *   domain: political_economy/federalism/migration_policy/welfare_state_theory
 *
 * SUMMARY:
 *   This constraint describes a 'selective solidarity' reading of free
 *   movement rights within a federal or quasi-federal system (e.g., the EU).
 *   It posits that access to welfare benefits for mobile citizens is
 *   contingent on their economic contribution history and current activity
 *   status, rather than solely on citizenship. This creates a tiered system
 *   where economically active migrants have fuller rights than those deemed
 *   economically inactive, shifting the burden of welfare provision based on
 *   a contributory principle rather than a universal citizenship principle.
 *   This reading is a response to perceived fiscal pressures on national
 *   welfare states from free movement.
 *
 * KEY AGENTS:
 *   - net_contributor_member_states: Primary beneficiary (institutional/arbitrage) — benefit from reduced welfare burden.
 *   - economically_active_migrants: Secondary beneficiary (moderate/mobile) — retain most rights, but face potential future restrictions.
 *   - economically_inactive_migrants: Primary target (powerless/trapped) — face restricted welfare access, high extraction.
 *   - high_welfare_member_states: Primary victim (institutional/constrained) — bear the fiscal burden of universal access under other readings, but benefit from this reading's restrictions.
 *   - federal_courts: Agenda setter (institutional/analytical) — interpret and enforce the scope of free movement and welfare access.
 *   - pro-integration_advocacy_groups: Excluded (organized/constrained) — advocate for universal rights based on citizenship.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(federation_membership_obligations__selective_solidarity, 0.65).
domain_priors:suppression_score(federation_membership_obligations__selective_solidarity, 0.7).
domain_priors:theater_ratio(federation_membership_obligations__selective_solidarity, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(federation_membership_obligations__selective_solidarity, extractiveness, 0.65).
narrative_ontology:constraint_metric(federation_membership_obligations__selective_solidarity, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(federation_membership_obligations__selective_solidarity, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(federation_membership_obligations__selective_solidarity, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(federation_membership_obligations__selective_solidarity, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(federation_membership_obligations__selective_solidarity, tangled_rope).
narrative_ontology:human_readable(federation_membership_obligations__selective_solidarity, "Federation Membership Obligations: Selective Solidarity Reading").
narrative_ontology:topic_domain(federation_membership_obligations__selective_solidarity, "political_economy/federalism/migration_policy/welfare_state_theory").

domain_priors:requires_active_enforcement(federation_membership_obligations__selective_solidarity).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(federation_membership_obligations__selective_solidarity, 'e74676f5-0e55-458a-82a0-6ab6d9e744bc').
narrative_ontology:cs_kernel_codification('e74676f5-0e55-458a-82a0-6ab6d9e744bc', formalized).
narrative_ontology:cs_authority_grounding('e74676f5-0e55-458a-82a0-6ab6d9e744bc', lineage).
narrative_ontology:cs_interpretation_layer_present('e74676f5-0e55-458a-82a0-6ab6d9e744bc').
narrative_ontology:cs_reading_relation('e74676f5-0e55-458a-82a0-6ab6d9e744bc', federation_membership_obligations__integration_primary, influences).
narrative_ontology:cs_reading_relation('e74676f5-0e55-458a-82a0-6ab6d9e744bc', federation_membership_obligations__member_sovereignty_primary, coexists_with).
narrative_ontology:cs_axiom('e74676f5-0e55-458a-82a0-6ab6d9e744bc', foundational, welfare_access_contingent_on_contribution).
narrative_ontology:cs_axiom_status(welfare_access_contingent_on_contribution, holdable).
narrative_ontology:cs_axiom_grounding('e74676f5-0e55-458a-82a0-6ab6d9e744bc', welfare_access_contingent_on_contribution, conventional).
narrative_ontology:cs_axiom('e74676f5-0e55-458a-82a0-6ab6d9e744bc', secondary, fiscal_sustainability_trumps_universal_access).
narrative_ontology:cs_axiom_status(fiscal_sustainability_trumps_universal_access, holdable).
narrative_ontology:cs_axiom_grounding('e74676f5-0e55-458a-82a0-6ab6d9e744bc', fiscal_sustainability_trumps_universal_access, instrumental).
narrative_ontology:cs_reference_frame('e74676f5-0e55-458a-82a0-6ab6d9e744bc', contributory_welfare_state_model).
narrative_ontology:cs_drift_state('e74676f5-0e55-458a-82a0-6ab6d9e744bc', contemporary_fiscal_pressures, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('e74676f5-0e55-458a-82a0-6ab6d9e744bc', '').
narrative_ontology:cs_kernel_id(federation_membership_obligations__selective_solidarity, federation_membership_obligations).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(federation_membership_obligations__selective_solidarity, net_contributor_member_states).
narrative_ontology:constraint_beneficiary(federation_membership_obligations__selective_solidarity, economically_active_migrants).
narrative_ontology:constraint_victim(federation_membership_obligations__selective_solidarity, economically_inactive_migrants).
narrative_ontology:constraint_victim(federation_membership_obligations__selective_solidarity, high_welfare_member_states).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(federation_membership_obligations__selective_solidarity, high_welfare_member_states).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These states contribute more to the federal budget than they receive and often have robust welfare systems. They advocate for the contributory principle to limit perceived 'welfare tourism' and ensure the fiscal sustainability of their national systems, benefiting from reduced welfare expenditure for mobile citizens.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__selective_solidarity, net_contributor_member_states, beneficiary,
    institutional, generational, constrained, national).

% Mobile citizens who are employed or self-sufficient. They generally retain their free movement and welfare rights under this reading, as they contribute to the host state's economy. They benefit from the stability of the system, even if it restricts others.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__selective_solidarity, economically_active_migrants, beneficiary,
    moderate, biographical, mobile, regional).

% Mobile citizens who are unemployed, seeking work, or otherwise not deemed economically active according to host state criteria. They face significant restrictions on welfare access, including housing benefits and social assistance, leading to precarity and potential forced return. They bear the direct costs of this selective solidarity.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__selective_solidarity, economically_inactive_migrants, payer,
    powerless, immediate, trapped, regional).

% Member states with generous welfare provisions. Under a universal citizenship principle, they would bear higher costs from mobile citizens accessing benefits. This reading reduces that fiscal pressure, making them beneficiaries of the restrictions, even if they also have a strong commitment to federal integration.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__selective_solidarity, high_welfare_member_states, beneficiary,
    institutional, generational, constrained, national).

% The judicial bodies responsible for interpreting federal law, including free movement and citizenship rights. Their rulings shape the application of the contributory principle, often balancing national fiscal concerns with federal integration goals. They administer the constraint through legal precedent.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__selective_solidarity, federal_courts, agenda_setter,
    institutional, generational, analytical, continental).

% Non-governmental organizations and civil society groups that advocate for broader, more universal free movement rights and welfare access based on federal citizenship. They are often excluded from the direct policy-making process that shapes this reading, but they challenge it through litigation and public campaigns.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__selective_solidarity, pro_integration_advocacy_groups, excluded,
    organized, generational, constrained, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(federation_membership_obligations__selective_solidarity, net_contributor_member_states).
narrative_ontology:fixing_cost_class(federation_membership_obligations__selective_solidarity, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To coordinate the fiscal sustainability of national welfare states within a federal system that guarantees free movement, by linking welfare access to economic contribution.
% TRANSFER_FUNCTION: Transfers the fiscal burden of welfare provision for economically inactive mobile citizens from host member states to either the migrants themselves (through denial of benefits) or their states of origin (through repatriation pressures).
% ABSENT_VOICES: Pro-integration advocacy groups and economically inactive migrants themselves are often marginalized in the policy debates that solidify this reading. They would argue for universal rights based on federal citizenship, irrespective of economic status, but their perspectives are systematically underrepresented in the decision-making bodies.
% DISAPPEARANCE_RATIONALE: If this reading disappeared overnight, member states would face immediate and significant fiscal pressures from universal welfare access for all mobile citizens. This would likely trigger a rapid re-negotiation of federal treaties, potentially leading to either a more integrated welfare system or a reassertion of national sovereignty over borders and benefits, fundamentally altering the federal landscape.
% FOUNDING_PROBLEM: The perceived fiscal strain on national welfare states due to free movement of citizens, particularly those who are economically inactive, leading to concerns about 'welfare tourism' and the sustainability of social security systems.
% FOUNDING_PROBLEM_CORROBORATION: The problem is attested as live by net contributor member states and some national governments, citing ongoing budgetary pressures and public opinion. However, pro-integration advocacy groups and some economic analyses from outside the benefiting parties contest the magnitude of the problem, arguing it is often exaggerated for political reasons, making the status 'contested' rather than universally 'live'.
narrative_ontology:disappearance_verdict(federation_membership_obligations__selective_solidarity, world_rearranges).
narrative_ontology:founding_problem_status(federation_membership_obligations__selective_solidarity, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(federation_membership_obligations__selective_solidarity, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(federation_membership_obligations__selective_solidarity, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(federation_membership_obligations__selective_solidarity_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(federation_membership_obligations__selective_solidarity, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(federation_membership_obligations__selective_solidarity_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint is classified as a Tangled Rope because it genuinely coordinates the fiscal sustainability of national welfare systems (a collective action problem for member states) while simultaneously extracting from economically inactive migrants. Extractiveness (0.65) is substantial for those denied benefits, and suppression (0.7) is high due to legal and administrative barriers. The theater ratio (0.2) is relatively low, as the enforcement of contributory principles is a genuine, rather than performative, function of this reading. The rising extractiveness and suppression over time reflect the increasing political salience of welfare tourism concerns and the hardening of legal frameworks to address them.
 *
 * PERSPECTIVAL GAP:
 *   Net contributor member states and economically active migrants experience this as a legitimate coordination mechanism, ensuring fairness and sustainability. Economically inactive migrants and pro-integration advocacy groups experience it as an extractive snare, undermining fundamental rights and creating a two-tiered citizenship. Federal courts, as agenda setters, navigate these competing claims, often reinforcing the selective solidarity reading through their interpretations.
 *
 * DIRECTIONALITY LOGIC:
 *   Net contributor member states are beneficiaries (d=0.1) as their fiscal burden is reduced. Economically active migrants are also beneficiaries (d=0.3) as their rights are largely preserved, and they benefit from the perceived stability of the system. Economically inactive migrants are clear targets (d=0.9) as they are denied access to benefits. High welfare member states are victims (d=0.7) under the 'integration primary' reading, but this 'selective solidarity' reading reduces their victimhood, making them beneficiaries of the restriction. Federal courts are agenda setters (d=0.5) as they administer the rules.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification prevents mislabeling the constraint as a pure Rope (which would ignore the extraction from inactive migrants) or a pure Snare (which would ignore the genuine coordination function for member states' fiscal sustainability). The 'selective solidarity' reading attempts to resolve the tension between free movement and welfare state integrity, but in doing so, it creates a new set of victims. The constraint's mandate is to ensure fiscal sustainability, which is still live, but the method of achieving it is contested due to its extractive impact on a specific group.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    selective_solidarity_vs_integration,
    'Is the ''selective solidarity'' reading a necessary adaptation to fiscal realities, or a subversion of the foundational ''integration primary'' principle of free movement?',
    'Empirical analysis of long-term economic impacts of unrestricted vs. restricted welfare access for mobile citizens, coupled with legal precedent on the scope of EU citizenship rights.',
    'If a necessary adaptation, it might be reclassified as a more legitimate form of coordination (e.g., a Rope with higher inherent costs). If a subversion, it would reinforce its classification as an extractive Snare or Tangled Rope, highlighting the political nature of the ''solidarity'' claim.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(selective_solidarity_vs_integration, conceptual, 'Ambiguity between fiscal necessity and foundational principle erosion.').

omega_variable(
    economic_activity_definition_ambiguity,
    'How is ''economic activity'' precisely defined and applied across member states, and does this definition create arbitrary exclusions or administrative burdens for mobile citizens?',
    'Comparative legal analysis of national implementations and case law, tracking the proportion of mobile citizens denied welfare access due to definitional ambiguities.',
    'If definitions are inconsistent or overly restrictive, the suppression and extractiveness metrics would be higher, pushing the classification closer to a Snare due to arbitrary exclusion.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(economic_activity_definition_ambiguity, empirical, 'Ambiguity in defining ''economic activity'' for welfare access.').

omega_variable(
    kernel_reading_identification,
    'This constraint is a ''selective solidarity'' reading of the ''federation_membership_obligations'' kernel. What would change if the ''integration_primary'' or ''member_sovereignty_primary'' readings were adopted?',
    'Analysis of legal and policy changes under alternative readings: ''integration_primary'' would expand welfare access for all mobile citizens, increasing costs for high-welfare states; ''member_sovereignty_primary'' would allow member states to restrict free movement more broadly, potentially reducing migration flows.',
    'Adopting ''integration_primary'' would shift the constraint towards a Rope, with broader beneficiaries and lower extraction from mobile citizens. Adopting ''member_sovereignty_primary'' would likely result in a Snare for mobile citizens, with higher suppression and extraction, but potentially a Rope for member states.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'Impact of alternative readings of the federation membership obligations kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(federation_membership_obligations__selective_solidarity, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fede_tr_t0, federation_membership_obligations__selective_solidarity, theater_ratio, 0, 0.25).
narrative_ontology:measurement(fede_tr_t5, federation_membership_obligations__selective_solidarity, theater_ratio, 5, 0.22).
narrative_ontology:measurement(fede_tr_t10, federation_membership_obligations__selective_solidarity, theater_ratio, 10, 0.21).
narrative_ontology:measurement(fede_tr_t15, federation_membership_obligations__selective_solidarity, theater_ratio, 15, 0.2).

% Extraction over time
narrative_ontology:measurement(fede_be_t0, federation_membership_obligations__selective_solidarity, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(fede_be_t5, federation_membership_obligations__selective_solidarity, base_extractiveness, 5, 0.55).
narrative_ontology:measurement(fede_be_t10, federation_membership_obligations__selective_solidarity, base_extractiveness, 10, 0.6).
narrative_ontology:measurement(fede_be_t15, federation_membership_obligations__selective_solidarity, base_extractiveness, 15, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(fede_su_t0, federation_membership_obligations__selective_solidarity, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(fede_su_t5, federation_membership_obligations__selective_solidarity, suppression_requirement, 5, 0.6).
narrative_ontology:measurement(fede_su_t10, federation_membership_obligations__selective_solidarity, suppression_requirement, 10, 0.65).
narrative_ontology:measurement(fede_su_t15, federation_membership_obligations__selective_solidarity, suppression_requirement, 15, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(federation_membership_obligations__selective_solidarity, resource_allocation).
narrative_ontology:affects_constraint(federation_membership_obligations__selective_solidarity, federation_membership_obligations__integration_primary).
narrative_ontology:affects_constraint(federation_membership_obligations__selective_solidarity, federation_membership_obligations__member_sovereignty_primary).

% DUAL FORMULATION NOTE:
% This constraint is one reading ('selective_solidarity') of the 'federation_membership_obligations' kernel. It is linked to sibling readings ('integration_primary' and 'member_sovereignty_primary') which represent alternative interpretations of free movement and welfare access within a federal system.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
