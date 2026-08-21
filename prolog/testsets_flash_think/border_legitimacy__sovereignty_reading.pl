% ============================================================================
% CONSTRAINT STORY: border_legitimacy__sovereignty_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_border_legitimacy__sovereignty_reading, []).

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
 *   constraint_id: border_legitimacy__sovereignty_reading
 *   human_readable: State Border Authority (Sovereignty Reading)
 *   domain: political_philosophy/international_law/migration_studies
 *
 * SUMMARY:
 *   This constraint story instantiates the 'sovereignty_reading' of the
 *   'border_legitimacy' kernel. It describes the state's authority to control
 *   its borders as deriving from territorial sovereignty, granting it a
 *   legitimate right to exclude non-citizens. This reading emphasizes the
 *   state's role in defining its political community and protecting its
 *   interests, often in tension with alternative readings that prioritize
 *   freedom of movement or humanitarian obligations. The claimed type
 *   (Tangled Rope) reflects the dual function of coordinating internal state
 *   affairs and extracting from those excluded.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(border_legitimacy__sovereignty_reading, 0.78).
domain_priors:suppression_score(border_legitimacy__sovereignty_reading, 0.85).
domain_priors:theater_ratio(border_legitimacy__sovereignty_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(border_legitimacy__sovereignty_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(border_legitimacy__sovereignty_reading, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(border_legitimacy__sovereignty_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(border_legitimacy__sovereignty_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(border_legitimacy__sovereignty_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(border_legitimacy__sovereignty_reading, tangled_rope).
narrative_ontology:human_readable(border_legitimacy__sovereignty_reading, "State Border Authority (Sovereignty Reading)").
narrative_ontology:topic_domain(border_legitimacy__sovereignty_reading, "political_philosophy/international_law/migration_studies").

domain_priors:requires_active_enforcement(border_legitimacy__sovereignty_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(border_legitimacy__sovereignty_reading, 'b90a741c-c2eb-465b-943b-fa8cf355d0ec').
narrative_ontology:cs_kernel_codification('b90a741c-c2eb-465b-943b-fa8cf355d0ec', formalized).
narrative_ontology:cs_authority_grounding('b90a741c-c2eb-465b-943b-fa8cf355d0ec', lineage).
narrative_ontology:cs_interpretation_layer_present('b90a741c-c2eb-465b-943b-fa8cf355d0ec').
narrative_ontology:cs_reading_relation('b90a741c-c2eb-465b-943b-fa8cf355d0ec', border_legitimacy__freedom_of_movement_reading, forecloses).
narrative_ontology:cs_reading_relation('b90a741c-c2eb-465b-943b-fa8cf355d0ec', border_legitimacy__humanitarian_obligation_reading, coexists_with).
narrative_ontology:cs_axiom('b90a741c-c2eb-465b-943b-fa8cf355d0ec', foundational, state_territorial_integrity).
narrative_ontology:cs_axiom_status(state_territorial_integrity, holdable).
narrative_ontology:cs_axiom_grounding('b90a741c-c2eb-465b-943b-fa8cf355d0ec', state_territorial_integrity, conventional).
narrative_ontology:cs_axiom('b90a741c-c2eb-465b-943b-fa8cf355d0ec', foundational, right_to_exclude_non_citizens).
narrative_ontology:cs_axiom_status(right_to_exclude_non_citizens, holdable).
narrative_ontology:cs_axiom_grounding('b90a741c-c2eb-465b-943b-fa8cf355d0ec', right_to_exclude_non_citizens, conventional).
narrative_ontology:cs_reference_frame('b90a741c-c2eb-465b-943b-fa8cf355d0ec', westphalian_state_system).
narrative_ontology:cs_drift_state('b90a741c-c2eb-465b-943b-fa8cf355d0ec', contemporary_human_rights_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('b90a741c-c2eb-465b-943b-fa8cf355d0ec', '').
narrative_ontology:cs_kernel_id(border_legitimacy__sovereignty_reading, border_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(border_legitimacy__sovereignty_reading, sovereign_state).
narrative_ontology:constraint_beneficiary(border_legitimacy__sovereignty_reading, citizens).
narrative_ontology:constraint_victim(border_legitimacy__sovereignty_reading, excluded_migrants).
narrative_ontology:constraint_victim(border_legitimacy__sovereignty_reading, asylum_seekers_denied).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Asserts and enforces its right to control its borders and determine who enters and resides within its territory, based on the principle of territorial sovereignty. Benefits from maintaining internal order, national identity, and resource control.
narrative_ontology:constraint_stakeholder(border_legitimacy__sovereignty_reading, sovereign_state, agenda_setter,
    institutional, generational, arbitrage, national).

% Benefit from the perceived security, cultural cohesion, and economic stability that border controls are claimed to provide. Their access to state resources and political participation is protected by the exclusion of non-citizens.
narrative_ontology:constraint_stakeholder(border_legitimacy__sovereignty_reading, citizens, beneficiary,
    organized, biographical, mobile, national).

% Bear the direct costs of exclusion, including loss of opportunity, separation from family, and often dangerous journeys. Their freedom of movement is denied, and they are subject to the enforcement mechanisms of the border authority.
narrative_ontology:constraint_stakeholder(border_legitimacy__sovereignty_reading, excluded_migrants, payer,
    powerless, immediate, trapped, global).

% Are denied entry or protection despite claims of persecution or danger, based on the state's sovereign right to determine who qualifies for asylum and where. They face continued threats or return to unsafe conditions.
narrative_ontology:constraint_stakeholder(border_legitimacy__sovereignty_reading, asylum_seekers_denied, payer,
    powerless, immediate, trapped, global).

% Challenge the absolute nature of state sovereignty in the face of human rights obligations, advocating for more open borders or greater protections for migrants. They document abuses and lobby for policy changes.
narrative_ontology:constraint_stakeholder(border_legitimacy__sovereignty_reading, human_rights_advocates, observer,
    organized, generational, analytical, global).

% Interpret and apply international conventions related to borders, migration, and human rights. They provide legal frameworks and judgments that can influence, but not directly override, state sovereign claims.
narrative_ontology:constraint_stakeholder(border_legitimacy__sovereignty_reading, international_law_bodies, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(border_legitimacy__sovereignty_reading, sovereign_state).
narrative_ontology:fixing_cost_class(border_legitimacy__sovereignty_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the definition of a national political community, manages internal resource allocation, and provides security for citizens by controlling who enters and resides within the state's territory.
% TRANSFER_FUNCTION: Transfers control over territory, resources, and national identity to the sovereign state and its citizens, while extracting freedom of movement, economic opportunity, and safety from excluded non-citizens.
% ABSENT_VOICES: Excluded migrants and those advocating for universal freedom of movement are largely absent from the formal decision-making processes that define and enforce border policies. Their perspectives are often mediated through advocacy groups or international bodies.
% DISAPPEARANCE_RATIONALE: If state border authority vanished overnight, the global political system, national identities, economic structures, and patterns of human settlement would undergo a fundamental and rapid reorganization. The concept of the nation-state as currently understood would cease to exist.
% FOUNDING_PROBLEM: The problem of defining and protecting a distinct political community, managing its resources, and ensuring its security against external threats and uncontrolled entry.
% FOUNDING_PROBLEM_CORROBORATION: The continued existence of states, international relations theory, and public opinion in many countries consistently corroborate the ongoing relevance of this founding problem. While contested by some, the need for states to manage their borders is widely accepted as a live issue by a broad range of actors outside the immediate beneficiaries.
narrative_ontology:disappearance_verdict(border_legitimacy__sovereignty_reading, world_rearranges).
narrative_ontology:founding_problem_status(border_legitimacy__sovereignty_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(border_legitimacy__sovereignty_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(border_legitimacy__sovereignty_reading, 'none', 1).
narrative_ontology:epsilon_provenance(border_legitimacy__sovereignty_reading, 0.78, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(border_legitimacy__sovereignty_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(border_legitimacy__sovereignty_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(border_legitimacy__sovereignty_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.78) because the constraint imposes significant costs on excluded migrants, denying them fundamental freedoms and opportunities. Suppression is very high (0.85) due to the active and often coercive enforcement mechanisms (border patrols, detention, deportations) required to maintain exclusion. Theater ratio is low (0.1) as the enforcement is largely functional, directly achieving the goal of exclusion, rather than performative. Accessibility collapse is high (0.7) for those without legal pathways, as alternatives to state-controlled entry are severely limited. Resistance is moderate (0.6) from migrants and advocacy groups, but often met with intensified enforcement.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the sovereign state and its citizens, this constraint is a legitimate and necessary coordination mechanism for national security and identity. From the perspective of excluded migrants, it is a highly extractive and suppressive barrier that denies fundamental rights. The engine's per-seat classification will reflect this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   The sovereign state and its citizens are the primary beneficiaries, experiencing the constraint as a legitimate mechanism for self-determination and protection (low directionality). Excluded migrants and denied asylum seekers are the clear targets, bearing the full costs of the constraint (high directionality). Human rights advocates and international law bodies act as analytical observers, assessing the constraint's operation against alternative normative frameworks.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sovereignty_vs_human_rights_primacy,
    'Does state territorial sovereignty constitute an absolute right to exclude, or is it fundamentally limited by international human rights law and the principle of universal human dignity?',
    'Ongoing evolution of international legal jurisprudence, state practice, and the development of new international norms or treaties that explicitly define the boundaries of sovereign exclusion.',
    'If human rights are deemed to limit sovereignty more strongly, the legitimacy of exclusion would decrease, potentially leading to lower effective extraction for migrants and a reclassification towards a Snare or even a Piton if enforcement becomes purely theatrical. If sovereignty is reaffirmed as primary, the current classification holds.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sovereignty_vs_human_rights_primacy, conceptual, 'The fundamental tension between state sovereignty and universal human rights in border control.').

omega_variable(
    effectiveness_of_exclusion_for_national_interest,
    'To what extent does the exclusion of non-citizens genuinely serve the stated national interests (e.g., economic stability, security, cultural cohesion), versus creating unintended negative consequences (e.g., labor shortages, black markets, humanitarian crises, reputational damage)?',
    'Comprehensive, independent empirical studies on the long-term economic, social, and security impacts of various border regimes, including cost-benefit analyses that account for both direct and indirect effects.',
    'If exclusion is shown to be largely ineffective or counterproductive for national interests, the ''coordination function'' justification would weaken, increasing the perceived extractiveness and potentially shifting the classification towards a Snare. If it is demonstrably effective, the Tangled Rope classification is reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(effectiveness_of_exclusion_for_national_interest, empirical, 'Empirical efficacy of border exclusion in achieving national goals.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(border_legitimacy__sovereignty_reading, 1945, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bord_tr_t1945, border_legitimacy__sovereignty_reading, theater_ratio, 1945, 0.1).
narrative_ontology:measurement(bord_tr_t1965, border_legitimacy__sovereignty_reading, theater_ratio, 1965, 0.1).
narrative_ontology:measurement(bord_tr_t1985, border_legitimacy__sovereignty_reading, theater_ratio, 1985, 0.1).
narrative_ontology:measurement(bord_tr_t2005, border_legitimacy__sovereignty_reading, theater_ratio, 2005, 0.1).
narrative_ontology:measurement(bord_tr_t2025, border_legitimacy__sovereignty_reading, theater_ratio, 2025, 0.1).

% Extraction over time
narrative_ontology:measurement(bord_be_t1945, border_legitimacy__sovereignty_reading, base_extractiveness, 1945, 0.7).
narrative_ontology:measurement(bord_be_t1965, border_legitimacy__sovereignty_reading, base_extractiveness, 1965, 0.72).
narrative_ontology:measurement(bord_be_t1985, border_legitimacy__sovereignty_reading, base_extractiveness, 1985, 0.74).
narrative_ontology:measurement(bord_be_t2005, border_legitimacy__sovereignty_reading, base_extractiveness, 2005, 0.76).
narrative_ontology:measurement(bord_be_t2025, border_legitimacy__sovereignty_reading, base_extractiveness, 2025, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(bord_su_t1945, border_legitimacy__sovereignty_reading, suppression_requirement, 1945, 0.75).
narrative_ontology:measurement(bord_su_t1965, border_legitimacy__sovereignty_reading, suppression_requirement, 1965, 0.78).
narrative_ontology:measurement(bord_su_t1985, border_legitimacy__sovereignty_reading, suppression_requirement, 1985, 0.8).
narrative_ontology:measurement(bord_su_t2005, border_legitimacy__sovereignty_reading, suppression_requirement, 2005, 0.83).
narrative_ontology:measurement(bord_su_t2025, border_legitimacy__sovereignty_reading, suppression_requirement, 2025, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(border_legitimacy__sovereignty_reading, identity_coordination).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
