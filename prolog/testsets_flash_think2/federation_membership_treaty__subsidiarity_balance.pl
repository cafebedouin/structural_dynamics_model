% ============================================================================
% CONSTRAINT STORY: federation_membership_treaty__subsidiarity_balance
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_federation_membership_treaty__subsidiarity_balance, []).

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
 *   constraint_id: federation_membership_treaty__subsidiarity_balance
 *   human_readable: Federation Membership Treaty: Subsidiarity Balance Reading
 *   domain: political_economy/federalism/migration_policy
 *
 * SUMMARY:
 *   This constraint represents the 'subsidiarity balance' reading of a
 *   federation's membership treaty, which posits that free movement rights,
 *   while fundamental, are not absolute and must operate within bounds of
 *   proportionality, allowing for legitimate national interests to constrain
 *   (but not eliminate) mobility. This reading seeks a middle ground between
 *   full integration and full national sovereignty, resulting in a graduated
 *   constraint structure where beneficiary and victim sets can vary by
 *   specific policy domain. It requires active enforcement to maintain this
 *   delicate balance against pressures from both integrationist and
 *   sovereigntist factions.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(federation_membership_treaty__subsidiarity_balance, 0.55).
domain_priors:suppression_score(federation_membership_treaty__subsidiarity_balance, 0.6).
domain_priors:theater_ratio(federation_membership_treaty__subsidiarity_balance, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(federation_membership_treaty__subsidiarity_balance, extractiveness, 0.55).
narrative_ontology:constraint_metric(federation_membership_treaty__subsidiarity_balance, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(federation_membership_treaty__subsidiarity_balance, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(federation_membership_treaty__subsidiarity_balance, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(federation_membership_treaty__subsidiarity_balance, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(federation_membership_treaty__subsidiarity_balance, tangled_rope).
narrative_ontology:human_readable(federation_membership_treaty__subsidiarity_balance, "Federation Membership Treaty: Subsidiarity Balance Reading").
narrative_ontology:topic_domain(federation_membership_treaty__subsidiarity_balance, "political_economy/federalism/migration_policy").

domain_priors:requires_active_enforcement(federation_membership_treaty__subsidiarity_balance).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(federation_membership_treaty__subsidiarity_balance, '7ef70bd2-256c-482b-84f3-9a652b168a50').
narrative_ontology:cs_kernel_codification('7ef70bd2-256c-482b-84f3-9a652b168a50', formalized).
narrative_ontology:cs_authority_grounding('7ef70bd2-256c-482b-84f3-9a652b168a50', lineage).
narrative_ontology:cs_interpretation_layer_present('7ef70bd2-256c-482b-84f3-9a652b168a50').
narrative_ontology:cs_reading_relation('7ef70bd2-256c-482b-84f3-9a652b168a50', federation_membership_treaty__integration_primary, coexists_with).
narrative_ontology:cs_reading_relation('7ef70bd2-256c-482b-84f3-9a652b168a50', federation_membership_treaty__sovereignty_primary, coexists_with).
narrative_ontology:cs_axiom('7ef70bd2-256c-482b-84f3-9a652b168a50', foundational, proportionality_principle).
narrative_ontology:cs_axiom_status(proportionality_principle, holdable).
narrative_ontology:cs_axiom_grounding('7ef70bd2-256c-482b-84f3-9a652b168a50', proportionality_principle, conventional).
narrative_ontology:cs_axiom('7ef70bd2-256c-482b-84f3-9a652b168a50', foundational, legitimate_national_interest_safeguard).
narrative_ontology:cs_axiom_status(legitimate_national_interest_safeguard, holdable).
narrative_ontology:cs_axiom_grounding('7ef70bd2-256c-482b-84f3-9a652b168a50', legitimate_national_interest_safeguard, conventional).
narrative_ontology:cs_reference_frame('7ef70bd2-256c-482b-84f3-9a652b168a50', balanced_federal_compact).
narrative_ontology:cs_drift_state('7ef70bd2-256c-482b-84f3-9a652b168a50', contemporary_political_economic_pressures, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('7ef70bd2-256c-482b-84f3-9a652b168a50', '').
narrative_ontology:cs_kernel_id(federation_membership_treaty__subsidiarity_balance, federation_membership_treaty).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(federation_membership_treaty__subsidiarity_balance, federation_institutions).
narrative_ontology:constraint_beneficiary(federation_membership_treaty__subsidiarity_balance, member_states_with_specific_interests).
narrative_ontology:constraint_beneficiary(federation_membership_treaty__subsidiarity_balance, mobile_citizens_with_justified_mobility).
narrative_ontology:constraint_victim(federation_membership_treaty__subsidiarity_balance, member_states_seeking_blanket_restrictions).
narrative_ontology:constraint_victim(federation_membership_treaty__subsidiarity_balance, unjustified_mobility_seekers).
narrative_ontology:constraint_victim(federation_membership_treaty__subsidiarity_balance, integrationist_advocates).
narrative_ontology:constraint_victim(federation_membership_treaty__subsidiarity_balance, sovereigntist_advocates).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(federation_membership_treaty__subsidiarity_balance, member_states_with_specific_interests).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the treaty, interprets proportionality, and adjudicates disputes, seeking to maintain a functional balance between member state autonomy and free movement principles. Benefits from the stability and legitimacy of the federation, which this balance aims to secure.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__subsidiarity_balance, federation_institutions, agenda_setter,
    institutional, generational, constrained, global).

% Benefit from the flexibility to implement policies addressing specific national concerns (e.g., labor market impacts, welfare system strain) while upholding the general principle of free movement. They also bear the cost of upholding free movement where it conflicts with purely national preferences.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__subsidiarity_balance, member_states_with_specific_interests, beneficiary,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(federation_membership_treaty__subsidiarity_balance, member_states_with_specific_interests, payer).

% Benefit from the right to move and reside across the federation, provided their mobility claims meet the proportionality and legitimate interest tests. Their rights are protected but not absolute, subject to the ongoing balance.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__subsidiarity_balance, mobile_citizens_with_justified_mobility, beneficiary,
    moderate, biographical, constrained, global).

% Bear the cost of being unable to impose blanket restrictions on free movement, even when facing domestic pressure. They are constrained by the treaty's balance, which limits their sovereign policy space in favor of federation-wide principles.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__subsidiarity_balance, member_states_seeking_blanket_restrictions, payer,
    organized, biographical, constrained, national).

% Individuals whose mobility claims are deemed not to meet the proportionality or legitimate interest criteria, facing restrictions or denial of rights. They bear the direct cost of the constraint's enforcement, with limited recourse.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__subsidiarity_balance, unjustified_mobility_seekers, payer,
    powerless, immediate, trapped, local).

% Advocate for a stronger, more expansive interpretation of free movement, viewing national restrictions as illegitimate barriers to a unified federation. Their preferred outcome of unrestricted mobility is suppressed by this reading's emphasis on balance and national interests.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__subsidiarity_balance, integrationist_advocates, excluded,
    organized, generational, analytical, global).

% Advocate for greater national control over borders and migration, viewing free movement as an infringement on national sovereignty. Their preferred outcome of blanket national control is suppressed by this reading's commitment to mobility rights.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__subsidiarity_balance, sovereigntist_advocates, excluded,
    organized, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the exercise of free movement rights with the legitimate national interests of member states, preventing both unrestricted mobility that could destabilize national systems and blanket restrictions that would undermine the federation's core principles.
% TRANSFER_FUNCTION: Transfers a degree of sovereign control from member states to the federation's legal framework in exchange for the benefits of free movement, while also transferring some individual mobility claims into a framework of conditional rights.
% ABSENT_VOICES: Advocates for pure integration (e.g., a fully unified federal citizenship with no national derogations) and pure sovereignty (e.g., unfettered national control over borders) are structurally excluded from the core balancing act, as this reading seeks a middle ground that suppresses both extremes.
% DISAPPEARANCE_RATIONALE: If this constraint vanished, the delicate balance would collapse. Member states would either impose widespread national restrictions, or free movement would become entirely unrestricted, leading to significant social and economic disruption and likely the unraveling of the federation's internal market and political cohesion.
% FOUNDING_PROBLEM: The founding problem was to reconcile the economic and social benefits of free movement within a federal structure with the enduring political and social demands for national sovereignty and the protection of distinct national interests.
% FOUNDING_PROBLEM_CORROBORATION: Legal scholars, federalist theorists, and various member state governments attest to the ongoing challenge of balancing these principles, citing continuous legal and political debates, and the persistent tension between federal integration and national autonomy.
narrative_ontology:disappearance_verdict(federation_membership_treaty__subsidiarity_balance, world_rearranges).
narrative_ontology:founding_problem_status(federation_membership_treaty__subsidiarity_balance, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(federation_membership_treaty__subsidiarity_balance, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(federation_membership_treaty__subsidiarity_balance, 'none', 1).
narrative_ontology:epsilon_provenance(federation_membership_treaty__subsidiarity_balance, 0.55, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(federation_membership_treaty__subsidiarity_balance_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(federation_membership_treaty__subsidiarity_balance, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(federation_membership_treaty__subsidiarity_balance_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.55) is moderate because the constraint extracts from both extremes: it limits unrestricted mobility (extracting from those who would benefit from it) and limits blanket national restrictions (extracting from states that would impose them). Suppression (0.60) is also moderate, reflecting the active legal and political effort required to prevent either extreme from dominating. Resistance (0.70) is high due to continuous pressure from both integrationist and sovereigntist camps. The theater ratio (0.20) is low, indicating that the constraint is largely functional in its balancing act, though some performative aspects exist in political rhetoric.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of federation institutions, this constraint is a necessary and legitimate 'tangled rope' that coordinates complex interests while imposing costs on all parties to maintain stability. For member states seeking blanket restrictions or individuals seeking unrestricted mobility, it is a more extractive 'snare' that limits their preferred actions. The engine's per-seat classification will capture these divergences.
 *
 * DIRECTIONALITY LOGIC:
 *   Federation institutions and member states with specific, balanced interests are beneficiaries, as they gain from the stability and flexibility this reading provides. Mobile citizens with justified claims also benefit. Member states seeking blanket restrictions and individuals whose mobility is deemed 'unjustified' are victims, as their preferred actions are curtailed. Integrationist and sovereigntist advocates are also victims in a sense, as their maximalist positions are suppressed by this balancing act.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    proportionality_interpretation_ambiguity,
    'What constitutes ''proportionality'' and ''legitimate national interests'' in specific policy domains, and how consistently are these interpreted across different federation institutions and member states?',
    'Detailed case law analysis, comparative legal studies across member states, and empirical assessment of the impact of specific national measures on free movement.',
    'If interpretations are highly inconsistent or biased, the constraint''s effective extractiveness and suppression could be higher for certain groups or states, potentially reclassifying it closer to a Snare for those seats. If consistently applied, it reinforces the Tangled Rope classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(proportionality_interpretation_ambiguity, conceptual, 'Ambiguity in the core balancing principles of the subsidiarity reading.').

omega_variable(
    dynamic_tension_resolution,
    'How does the balance between free movement and national interests dynamically shift in response to economic crises, security threats, or political populism, and does the treaty mechanism adequately adapt or become rigid?',
    'Longitudinal study of policy responses to major crises, analysis of judicial rulings during periods of stress, and assessment of treaty amendment or derogation processes.',
    'If the mechanism becomes rigid and fails to adapt, leading to disproportionate burdens on certain parties, the constraint could drift towards a Snare or Piton. If it adapts effectively, it reinforces its function as a dynamic Tangled Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dynamic_tension_resolution, empirical, 'The adaptability and resilience of the balancing mechanism under external pressures.').

omega_variable(
    kernel_reading_identification,
    'Is this constraint truly a distinct ''subsidiarity_balance'' reading, or is it merely a rhetorical cover for either ''integration_primary'' or ''sovereignty_primary'' in practice?',
    'Analysis of judicial outcomes and legislative compromises: if outcomes consistently reflect a genuine middle ground rather than favoring one extreme, the reading is distinct. If outcomes consistently lean one way, it suggests a rhetorical cover.',
    'If found to be a cover, the constraint would be reclassified as a variant of the dominant sibling reading, with its metrics adjusted to reflect the true underlying extractive or coordinative function.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'Verifying the distinct structural identity of the subsidiarity balance reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(federation_membership_treaty__subsidiarity_balance, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fede_tr_t0, federation_membership_treaty__subsidiarity_balance, theater_ratio, 0, 0.18).
narrative_ontology:measurement(fede_tr_t6, federation_membership_treaty__subsidiarity_balance, theater_ratio, 6, 0.19).
narrative_ontology:measurement(fede_tr_t12, federation_membership_treaty__subsidiarity_balance, theater_ratio, 12, 0.2).
narrative_ontology:measurement(fede_tr_t18, federation_membership_treaty__subsidiarity_balance, theater_ratio, 18, 0.21).
narrative_ontology:measurement(fede_tr_t24, federation_membership_treaty__subsidiarity_balance, theater_ratio, 24, 0.2).
narrative_ontology:measurement(fede_tr_t30, federation_membership_treaty__subsidiarity_balance, theater_ratio, 30, 0.2).

% Extraction over time
narrative_ontology:measurement(fede_be_t0, federation_membership_treaty__subsidiarity_balance, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(fede_be_t6, federation_membership_treaty__subsidiarity_balance, base_extractiveness, 6, 0.52).
narrative_ontology:measurement(fede_be_t12, federation_membership_treaty__subsidiarity_balance, base_extractiveness, 12, 0.54).
narrative_ontology:measurement(fede_be_t18, federation_membership_treaty__subsidiarity_balance, base_extractiveness, 18, 0.55).
narrative_ontology:measurement(fede_be_t24, federation_membership_treaty__subsidiarity_balance, base_extractiveness, 24, 0.56).
narrative_ontology:measurement(fede_be_t30, federation_membership_treaty__subsidiarity_balance, base_extractiveness, 30, 0.55).

% Suppression requirement over time
narrative_ontology:measurement(fede_su_t0, federation_membership_treaty__subsidiarity_balance, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(fede_su_t6, federation_membership_treaty__subsidiarity_balance, suppression_requirement, 6, 0.57).
narrative_ontology:measurement(fede_su_t12, federation_membership_treaty__subsidiarity_balance, suppression_requirement, 12, 0.59).
narrative_ontology:measurement(fede_su_t18, federation_membership_treaty__subsidiarity_balance, suppression_requirement, 18, 0.6).
narrative_ontology:measurement(fede_su_t24, federation_membership_treaty__subsidiarity_balance, suppression_requirement, 24, 0.61).
narrative_ontology:measurement(fede_su_t30, federation_membership_treaty__subsidiarity_balance, suppression_requirement, 30, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(federation_membership_treaty__subsidiarity_balance, enforcement_mechanism).
narrative_ontology:affects_constraint(federation_membership_treaty__subsidiarity_balance, federation_membership_treaty__integration_primary).
narrative_ontology:affects_constraint(federation_membership_treaty__subsidiarity_balance, federation_membership_treaty__sovereignty_primary).

% DUAL FORMULATION NOTE:
% This constraint is one of three distinct readings of the 'federation_membership_treaty' kernel, each with its own structural properties and classification. They are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
