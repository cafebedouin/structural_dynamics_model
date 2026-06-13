% ============================================================================
% CONSTRAINT STORY: jewish_territorial_claim__revisionist_zionism_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jewish_territorial_claim__revisionist_zionism_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: jewish_territorial_claim__revisionist_zionism_reading
 *   human_readable: Revisionist Zionist Maximalist Territorial Claim (Both Banks of Jordan)
 *   domain: political_history/settler_colonialism/nationalism_studies
 *
 * SUMMARY:
 *   This constraint represents the Revisionist Zionist reading of the Jewish
 *   territorial claim, advocating for immediate Jewish sovereignty over a
 *   maximalist territory (both banks of the Jordan River) and relying on an
 *   'Iron Wall' of military force to compel Arab acceptance rather than
 *   seeking consent. It is a Snare due to its high extractiveness and
 *   suppression, with identifiable victims and beneficiaries. This reading
 *   explicitly rejects the premises of other Zionist factions that might
 *   prioritize cultural development, socialist settlement, or political
 *   negotiation over immediate, maximalist territorial control enforced by
 *   military might.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jewish_territorial_claim__revisionist_zionism_reading, 0.95).
domain_priors:suppression_score(jewish_territorial_claim__revisionist_zionism_reading, 0.98).
domain_priors:theater_ratio(jewish_territorial_claim__revisionist_zionism_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jewish_territorial_claim__revisionist_zionism_reading, extractiveness, 0.95).
narrative_ontology:constraint_metric(jewish_territorial_claim__revisionist_zionism_reading, suppression_requirement, 0.98).
narrative_ontology:constraint_metric(jewish_territorial_claim__revisionist_zionism_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jewish_territorial_claim__revisionist_zionism_reading, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(jewish_territorial_claim__revisionist_zionism_reading, resistance, 0.85).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jewish_territorial_claim__revisionist_zionism_reading, snare).
narrative_ontology:human_readable(jewish_territorial_claim__revisionist_zionism_reading, "Revisionist Zionist Maximalist Territorial Claim (Both Banks of Jordan)").
narrative_ontology:topic_domain(jewish_territorial_claim__revisionist_zionism_reading, "political_history/settler_colonialism/nationalism_studies").

domain_priors:requires_active_enforcement(jewish_territorial_claim__revisionist_zionism_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jewish_territorial_claim__revisionist_zionism_reading, 'e1a2475d-a78f-4bcc-8d6b-1d440ce5a542').
narrative_ontology:cs_kernel_codification('e1a2475d-a78f-4bcc-8d6b-1d440ce5a542', formalized).
narrative_ontology:cs_authority_grounding('e1a2475d-a78f-4bcc-8d6b-1d440ce5a542', extraction).
narrative_ontology:cs_interpretation_layer_present('e1a2475d-a78f-4bcc-8d6b-1d440ce5a542').
narrative_ontology:cs_reading_relation('e1a2475d-a78f-4bcc-8d6b-1d440ce5a542', jewish_territorial_claim__political_zionism_reading, forecloses).
narrative_ontology:cs_reading_relation('e1a2475d-a78f-4bcc-8d6b-1d440ce5a542', jewish_territorial_claim__labor_zionism_reading, forecloses).
narrative_ontology:cs_reading_relation('e1a2475d-a78f-4bcc-8d6b-1d440ce5a542', jewish_territorial_claim__cultural_zionism_reading, forecloses).
narrative_ontology:cs_axiom('e1a2475d-a78f-4bcc-8d6b-1d440ce5a542', foundational, sovereignty_over_greater_israel_non_negotiable).
narrative_ontology:cs_axiom_status(sovereignty_over_greater_israel_non_negotiable, holdable).
narrative_ontology:cs_axiom_grounding('e1a2475d-a78f-4bcc-8d6b-1d440ce5a542', sovereignty_over_greater_israel_non_negotiable, deontological).
narrative_ontology:cs_axiom('e1a2475d-a78f-4bcc-8d6b-1d440ce5a542', foundational, arab_consent_irrelevant_to_jewish_sovereignty).
narrative_ontology:cs_axiom_status(arab_consent_irrelevant_to_jewish_sovereignty, holdable).
narrative_ontology:cs_axiom_grounding('e1a2475d-a78f-4bcc-8d6b-1d440ce5a542', arab_consent_irrelevant_to_jewish_sovereignty, conventional).
narrative_ontology:cs_reference_frame('e1a2475d-a78f-4bcc-8d6b-1d440ce5a542', maximalist_territorial_sovereignty_by_force).
narrative_ontology:cs_drift_state('e1a2475d-a78f-4bcc-8d6b-1d440ce5a542', contemporary_international_law_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('e1a2475d-a78f-4bcc-8d6b-1d440ce5a542', '').
narrative_ontology:cs_kernel_id(jewish_territorial_claim__revisionist_zionism_reading, jewish_territorial_claim).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jewish_territorial_claim__revisionist_zionism_reading, revisionist_zionist_movement).
narrative_ontology:constraint_beneficiary(jewish_territorial_claim__revisionist_zionism_reading, settler_population).
narrative_ontology:constraint_victim(jewish_territorial_claim__revisionist_zionism_reading, palestinian_arab_population).
narrative_ontology:constraint_victim(jewish_territorial_claim__revisionist_zionism_reading, neighboring_arab_states).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Advocates for and actively pursues the establishment of Jewish sovereignty over all territory from the Mediterranean Sea to the Jordan River, and beyond. Views military force as the primary means to achieve and maintain this claim, rejecting any notion of Arab consent as a prerequisite. Benefits from the expansion of territory and the consolidation of power.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__revisionist_zionism_reading, revisionist_zionist_movement, agenda_setter,
    institutional, generational, identity_locked, regional).

% Directly benefits from the territorial expansion and the establishment of new settlements, often receiving state support and protection. Their presence on the land is a direct manifestation of the maximalist claim, and they are protected by the military force that enforces it.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__revisionist_zionism_reading, settler_population, beneficiary,
    organized, biographical, constrained, local).

% Bears the direct costs of the territorial claim, including displacement, loss of land and property, restrictions on movement, and subjection to military rule. Their national aspirations and self-determination are directly suppressed by the constraint's enforcement.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__revisionist_zionism_reading, palestinian_arab_population, payer,
    powerless, generational, trapped, local).

% Experience geopolitical instability, refugee crises, and military confrontations as a result of the maximalist territorial claim. Their ability to influence the situation is constrained by the military superiority of the enforcing power and international political dynamics.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__revisionist_zionism_reading, neighboring_arab_states, payer,
    powerful, generational, constrained, regional).

% Observes and often condemns the maximalist territorial claims and the methods of their enforcement, but is largely ineffective in altering the constraint due to geopolitical interests and the determined resistance of the agenda-setter. Provides humanitarian aid to victims but cannot compel a resolution.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__revisionist_zionism_reading, international_community, observer,
    institutional, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the actions of the Revisionist Zionist movement and its supporters towards a unified goal of territorial expansion and the establishment of Jewish sovereignty over a maximalist 'Greater Israel'.
% TRANSFER_FUNCTION: Transfers land, resources, and political control from the Palestinian Arab population and neighboring Arab states to the Revisionist Zionist movement and its settler population, enforced by military power.
% ABSENT_VOICES: The voices of indigenous Palestinian Arabs are systematically excluded from the decision-making processes that define and enforce this claim. Their historical narratives, rights to self-determination, and proposals for alternative political arrangements are actively suppressed or dismissed as illegitimate.
% DISAPPEARANCE_RATIONALE: If this maximalist territorial claim and its enforcement vanished overnight, the entire geopolitical landscape of the Middle East would fundamentally rearrange. Palestinian Arabs would assert their rights to self-determination and return, borders would be contested, and the power dynamics in the region would shift dramatically, leading to a complete re-evaluation of sovereignty and land ownership.
% FOUNDING_PROBLEM: The perceived existential threat to the Jewish people, the historical connection to the land of Israel, and the desire for a secure, sovereign Jewish state that could not be threatened by external forces or internal dissent.
% FOUNDING_PROBLEM_CORROBORATION: The Revisionist Zionist movement and its political descendants consistently assert that the existential threat remains live, justifying the maximalist claim and its enforcement. This is corroborated by historical narratives of persecution and ongoing regional conflicts, though the necessity of the maximalist claim itself is contested by other Zionist factions and the international community.
narrative_ontology:disappearance_verdict(jewish_territorial_claim__revisionist_zionism_reading, world_rearranges).
narrative_ontology:founding_problem_status(jewish_territorial_claim__revisionist_zionism_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jewish_territorial_claim__revisionist_zionism_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(jewish_territorial_claim__revisionist_zionism_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jewish_territorial_claim__revisionist_zionism_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(jewish_territorial_claim__revisionist_zionism_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(jewish_territorial_claim__revisionist_zionism_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is extremely high (0.95) as it involves the direct appropriation of land and resources from an existing population. Suppression is also extremely high (0.98) due to the explicit reliance on military force and the rejection of indigenous consent, actively suppressing any alternatives or resistance. The theater ratio is very low (0.05) because the constraint is overtly about power and territorial control, with minimal performative justification beyond the core ideological claim. Accessibility collapse is high (0.9) as the military enforcement makes alternatives virtually impossible for the victims. Resistance is high (0.85) reflecting the ongoing conflict and opposition from the Palestinian population and Arab states.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the Revisionist Zionist movement, this is a necessary and just assertion of historical rights and security. From the perspective of the Palestinian Arab population, it is an act of colonial dispossession and ongoing oppression. The engine's classification as a Snare reflects the structural reality of extraction and suppression, regardless of the agenda-setter's internal justification.
 *
 * DIRECTIONALITY LOGIC:
 *   The Revisionist Zionist Movement and settler population are clear beneficiaries (d near 0.0) as they gain land, resources, and political power. The Palestinian Arab population and neighboring Arab states are clear victims (d near 1.0) as they suffer displacement, loss of sovereignty, and military subjugation. The international community acts as an observer, with limited ability to alter the constraint's operation.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    legitimacy_of_force_vs_consent,
    'Is the ''Iron Wall'' doctrine, which rejects Arab consent and relies on military force, a legitimate and sustainable basis for state-building and territorial claims?',
    'Long-term historical outcomes: if sustained peace and security are achieved without consent, it suggests a different structural dynamic than if ongoing conflict and instability persist.',
    'If unsustainable, the constraint''s long-term viability is compromised, potentially leading to a reclassification towards a Piton or even collapse. If sustainable, it would challenge assumptions about the necessity of consent for political stability.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legitimacy_of_force_vs_consent, empirical, 'The long-term viability of a state built on military compulsion without consent.').

omega_variable(
    historical_rights_vs_present_occupation,
    'To what extent do historical claims to land supersede the rights and presence of an existing indigenous population?',
    'International legal precedent and evolving norms of self-determination and indigenous rights.',
    'If present occupation and indigenous rights are prioritized, the constraint''s legitimacy is undermined, potentially leading to external pressure for dismantling or modification. If historical claims are prioritized, the constraint''s justification is strengthened.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(historical_rights_vs_present_occupation, conceptual, 'The conceptual hierarchy of historical claims versus present-day rights.').

omega_variable(
    maximalist_territory_vs_security,
    'Is the maximalist territorial claim (both banks of the Jordan) genuinely necessary for the security of the Jewish state, or is it an expansionist goal justified by security rhetoric?',
    'Independent military and geopolitical analysis comparing security needs with territorial demands, and evaluating alternative security arrangements.',
    'If not genuinely necessary for security, the ''Iron Wall'' justification becomes a cover story for extraction, reinforcing the Snare classification. If proven necessary, it would shift the understanding of the constraint''s underlying function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(maximalist_territory_vs_security, empirical, 'The relationship between territorial maximalism and genuine security needs.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jewish_territorial_claim__revisionist_zionism_reading, 1923, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jewi_tr_t1923, jewish_territorial_claim__revisionist_zionism_reading, theater_ratio, 1923, 0.1).
narrative_ontology:measurement(jewi_tr_t1948, jewish_territorial_claim__revisionist_zionism_reading, theater_ratio, 1948, 0.08).
narrative_ontology:measurement(jewi_tr_t1967, jewish_territorial_claim__revisionist_zionism_reading, theater_ratio, 1967, 0.05).
narrative_ontology:measurement(jewi_tr_t1993, jewish_territorial_claim__revisionist_zionism_reading, theater_ratio, 1993, 0.07).
narrative_ontology:measurement(jewi_tr_t2000, jewish_territorial_claim__revisionist_zionism_reading, theater_ratio, 2000, 0.06).
narrative_ontology:measurement(jewi_tr_t2024, jewish_territorial_claim__revisionist_zionism_reading, theater_ratio, 2024, 0.05).

% Extraction over time
narrative_ontology:measurement(jewi_be_t1923, jewish_territorial_claim__revisionist_zionism_reading, base_extractiveness, 1923, 0.7).
narrative_ontology:measurement(jewi_be_t1948, jewish_territorial_claim__revisionist_zionism_reading, base_extractiveness, 1948, 0.85).
narrative_ontology:measurement(jewi_be_t1967, jewish_territorial_claim__revisionist_zionism_reading, base_extractiveness, 1967, 0.9).
narrative_ontology:measurement(jewi_be_t1993, jewish_territorial_claim__revisionist_zionism_reading, base_extractiveness, 1993, 0.88).
narrative_ontology:measurement(jewi_be_t2000, jewish_territorial_claim__revisionist_zionism_reading, base_extractiveness, 2000, 0.92).
narrative_ontology:measurement(jewi_be_t2024, jewish_territorial_claim__revisionist_zionism_reading, base_extractiveness, 2024, 0.95).

% Suppression requirement over time
narrative_ontology:measurement(jewi_su_t1923, jewish_territorial_claim__revisionist_zionism_reading, suppression_requirement, 1923, 0.75).
narrative_ontology:measurement(jewi_su_t1948, jewish_territorial_claim__revisionist_zionism_reading, suppression_requirement, 1948, 0.9).
narrative_ontology:measurement(jewi_su_t1967, jewish_territorial_claim__revisionist_zionism_reading, suppression_requirement, 1967, 0.95).
narrative_ontology:measurement(jewi_su_t1993, jewish_territorial_claim__revisionist_zionism_reading, suppression_requirement, 1993, 0.92).
narrative_ontology:measurement(jewi_su_t2000, jewish_territorial_claim__revisionist_zionism_reading, suppression_requirement, 2000, 0.96).
narrative_ontology:measurement(jewi_su_t2024, jewish_territorial_claim__revisionist_zionism_reading, suppression_requirement, 2024, 0.98).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jewish_territorial_claim__revisionist_zionism_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(jewish_territorial_claim__revisionist_zionism_reading, jewish_territorial_claim__political_zionism_reading).
narrative_ontology:affects_constraint(jewish_territorial_claim__revisionist_zionism_reading, jewish_territorial_claim__labor_zionism_reading).
narrative_ontology:affects_constraint(jewish_territorial_claim__revisionist_zionism_reading, jewish_territorial_claim__cultural_zionism_reading).
narrative_ontology:affects_constraint(jewish_territorial_claim__revisionist_zionism_reading, palestinian_right_of_return).
narrative_ontology:affects_constraint(jewish_territorial_claim__revisionist_zionism_reading, two_state_solution_framework).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'jewish_territorial_claim' kernel. It is linked to other Zionist readings and to related constraints like the 'palestinian_right_of_return' and the 'two_state_solution_framework' as its operation directly impacts their viability.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
