% ============================================================================
% CONSTRAINT STORY: border_normative_status__sovereignty_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_border_normative_status__sovereignty_primary, []).

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
 *   constraint_id: border_normative_status__sovereignty_primary
 *   human_readable: Border Normative Status: Sovereignty Primary Reading
 *   domain: political_philosophy/international_law/migration_studies
 *
 * SUMMARY:
 *   This constraint represents the 'sovereignty primary' reading of the
 *   normative status of borders, asserting that states have foundational
 *   authority to exclude non-members as an instrument of collective
 *   self-determination. This reading frames border enforcement as a
 *   legitimate state function, with the displacement of non-members treated
 *   as an externality or non-issue. It is one reading of the broader
 *   'border_normative_status' kernel, which also includes 'freedom_primary'
 *   and 'qualified_sovereignty' readings.
 *
 * KEY AGENTS:
 *   - Nation_states: Agenda-setter (institutional/constrained) — asserts and enforces border control.
 *   - Citizen_populations: Beneficiary (organized/mobile) — benefits from perceived security and stability.
 *   - Non_member_migrants: Payer (powerless/trapped) — bears the direct costs of exclusion.
 *   - Asylum_seekers: Payer (powerless/trapped) — faces barriers despite international conventions.
 *   - International_human_rights_advocates: Observer (organized/analytical) — challenges state actions.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(border_normative_status__sovereignty_primary, 0.65).
domain_priors:suppression_score(border_normative_status__sovereignty_primary, 0.78).
domain_priors:theater_ratio(border_normative_status__sovereignty_primary, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(border_normative_status__sovereignty_primary, extractiveness, 0.65).
narrative_ontology:constraint_metric(border_normative_status__sovereignty_primary, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(border_normative_status__sovereignty_primary, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(border_normative_status__sovereignty_primary, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(border_normative_status__sovereignty_primary, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(border_normative_status__sovereignty_primary, tangled_rope).
narrative_ontology:human_readable(border_normative_status__sovereignty_primary, "Border Normative Status: Sovereignty Primary Reading").
narrative_ontology:topic_domain(border_normative_status__sovereignty_primary, "political_philosophy/international_law/migration_studies").

domain_priors:requires_active_enforcement(border_normative_status__sovereignty_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(border_normative_status__sovereignty_primary, '9383818d-3cd1-42d0-b07d-32fa34db2114').
narrative_ontology:cs_kernel_codification('9383818d-3cd1-42d0-b07d-32fa34db2114', formalized).
narrative_ontology:cs_authority_grounding('9383818d-3cd1-42d0-b07d-32fa34db2114', lineage).
narrative_ontology:cs_interpretation_layer_present('9383818d-3cd1-42d0-b07d-32fa34db2114').
narrative_ontology:cs_reading_relation('9383818d-3cd1-42d0-b07d-32fa34db2114', border_normative_status__freedom_primary, coexists_with).
narrative_ontology:cs_reading_relation('9383818d-3cd1-42d0-b07d-32fa34db2114', border_normative_status__qualified_sovereignty, coexists_with).
narrative_ontology:cs_axiom('9383818d-3cd1-42d0-b07d-32fa34db2114', foundational, state_sovereignty_is_foundational).
narrative_ontology:cs_axiom_status(state_sovereignty_is_foundational, holdable).
narrative_ontology:cs_axiom_grounding('9383818d-3cd1-42d0-b07d-32fa34db2114', state_sovereignty_is_foundational, deontological).
narrative_ontology:cs_axiom('9383818d-3cd1-42d0-b07d-32fa34db2114', foundational, collective_self_determination_requires_exclusion).
narrative_ontology:cs_axiom_status(collective_self_determination_requires_exclusion, holdable).
narrative_ontology:cs_axiom_grounding('9383818d-3cd1-42d0-b07d-32fa34db2114', collective_self_determination_requires_exclusion, conventional).
narrative_ontology:cs_reference_frame('9383818d-3cd1-42d0-b07d-32fa34db2114', westphalian_state_system).
narrative_ontology:cs_drift_state('9383818d-3cd1-42d0-b07d-32fa34db2114', contemporary_human_rights_era, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('9383818d-3cd1-42d0-b07d-32fa34db2114', '2024-07-30T12:00:00Z').
narrative_ontology:cs_kernel_id(border_normative_status__sovereignty_primary, border_normative_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(border_normative_status__sovereignty_primary, nation_states).
narrative_ontology:constraint_beneficiary(border_normative_status__sovereignty_primary, citizen_populations).
narrative_ontology:constraint_victim(border_normative_status__sovereignty_primary, non_member_migrants).
narrative_ontology:constraint_victim(border_normative_status__sovereignty_primary, asylum_seekers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Assert and enforce the right to control their borders as a fundamental aspect of sovereignty and collective self-determination. They benefit from the ability to regulate entry, manage demographics, and protect national interests. Exit options are constrained by international norms and the practicalities of open borders.
narrative_ontology:constraint_stakeholder(border_normative_status__sovereignty_primary, nation_states, agenda_setter,
    institutional, generational, constrained, global).

% Benefit from the perceived security, cultural cohesion, and economic stability that border controls are claimed to provide. They delegate authority to the state for border management and often support policies of exclusion. Their direct costs are diffuse (e.g., higher prices for certain goods/services due to labor restrictions).
narrative_ontology:constraint_stakeholder(border_normative_status__sovereignty_primary, citizen_populations, beneficiary,
    organized, biographical, mobile, national).

% Bear the direct costs of exclusion, including denied entry, forced return, separation from families, and often dangerous journeys. Their options are limited to attempting unauthorized entry, seeking asylum (often with low success rates), or remaining in precarious situations in transit countries. They are the primary targets of border enforcement.
narrative_ontology:constraint_stakeholder(border_normative_status__sovereignty_primary, non_member_migrants, payer,
    powerless, immediate, trapped, global).

% Face significant barriers to entry and often prolonged detention or precarious legal status, despite international conventions. Their claims are adjudicated by states that prioritize sovereign control, leading to high personal costs and limited access to protection. Their identity as asylum seekers often makes them identity_locked to the process.
narrative_ontology:constraint_stakeholder(border_normative_status__sovereignty_primary, asylum_seekers, payer,
    powerless, immediate, trapped, global).

% Monitor state border practices against international human rights law and advocate for the rights of migrants and asylum seekers. They provide critical analysis and challenge state actions, but lack direct enforcement power over sovereign states.
narrative_ontology:constraint_stakeholder(border_normative_status__sovereignty_primary, international_human_rights_advocates, observer,
    organized, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the collective self-determination of a national community by defining its membership and controlling access to its territory, allowing for the management of resources, social services, and cultural identity.
% TRANSFER_FUNCTION: Transfers the right to determine membership and control territory to the nation-state, from a global pool of potential residents. It transfers the costs of exclusion (e.g., displacement, denied opportunity) from citizen populations to non-member migrants.
% ABSENT_VOICES: Non-member migrants and stateless persons are largely excluded from the political processes that define and enforce borders. If present, they would argue for greater freedom of movement and a re-evaluation of the moral basis of territorial exclusion.
% DISAPPEARANCE_RATIONALE: If the normative status of borders as legitimate instruments of exclusion vanished overnight, nation-states would lose a foundational claim to control their territory and membership. Global migration patterns would shift dramatically, national identities would be challenged, and the international system of states would undergo a profound reorganization.
% FOUNDING_PROBLEM: The need for distinct political communities to define their membership, manage their resources, and protect their collective interests against external pressures, leading to the formation of sovereign states with territorial control.
% FOUNDING_PROBLEM_CORROBORATION: Nation-states and their citizen populations widely attest that the founding problem of collective self-determination and territorial integrity remains live, citing ongoing geopolitical tensions, economic competition, and cultural preservation concerns. This is corroborated by the persistence of state-centric international law, though challenged by human rights frameworks.
narrative_ontology:disappearance_verdict(border_normative_status__sovereignty_primary, world_rearranges).
narrative_ontology:founding_problem_status(border_normative_status__sovereignty_primary, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(border_normative_status__sovereignty_primary, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(border_normative_status__sovereignty_primary, 'none', 1).
narrative_ontology:epsilon_provenance(border_normative_status__sovereignty_primary, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(border_normative_status__sovereignty_primary_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(border_normative_status__sovereignty_primary, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(border_normative_status__sovereignty_primary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint is classified as a Tangled Rope because it performs a genuine coordination function (collective self-determination for states/citizens) but simultaneously involves significant asymmetric extraction from non-member migrants and asylum seekers. Extractiveness is high (0.65) due to the severe costs imposed on those excluded. Suppression is also high (0.78) as it relies on active enforcement and the suppression of alternative pathways for entry. Theater ratio is low (0.15) as the enforcement is largely functional in achieving its stated goal of exclusion, rather than performative. The increasing extractiveness and suppression over time reflect the hardening of border regimes in response to global migration pressures.
 *
 * PERSPECTIVAL GAP:
 *   Nation-states and citizen populations experience this constraint as a legitimate and necessary mechanism for self-governance and protection, aligning with a 'rope' or even 'mountain' perspective. Non-member migrants and asylum seekers, however, experience it as a 'snare' due to the high costs, lack of alternatives, and active suppression. The engine's computation of per-seat classifications will reflect this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Nation-states are full beneficiaries (d=0.0) as they control the mechanism and derive authority from it. Citizen populations are also beneficiaries (d=0.15) due to perceived benefits and delegated authority. Non-member migrants and asylum seekers are full targets (d=1.0) as they bear the full costs of exclusion with minimal agency or exit options. International human rights advocates are analytical observers (d=0.5).
 *
 * MANDATROPHY ANALYSIS:
 *   This classification prevents mislabeling the constraint as a pure 'mountain' (natural law) or 'rope' (pure coordination), which would ignore the substantial extraction and suppression inherent in its operation. By identifying it as a Tangled Rope, the framework highlights the dual nature of the constraint: a coordination function for some, but an extractive mechanism for others. The 'live' status of the founding problem, combined with rising extractiveness, suggests an ongoing tension rather than mandatrophy, but the contestation over the founding problem's status (Q5) indicates a potential for future mandatrophy if the coordination function is perceived to atrophy while extraction persists.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sovereignty_vs_human_rights_priority,
    'Is the foundational authority of states to exclude non-members (sovereignty primary) genuinely compatible with universal human rights obligations, or does it inherently conflict?',
    'Legal and philosophical consensus on the hierarchy of international norms, or a landmark international court ruling that explicitly subordinates one principle to the other in border contexts.',
    'If incompatible, the ''sovereignty primary'' reading would be reclassified as a Snare, as its coordination function would be revealed as cover for rights violations. If compatible, its Tangled Rope status would be reinforced, acknowledging a legitimate but costly coordination.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sovereignty_vs_human_rights_priority, conceptual, 'Ambiguity regarding the ultimate normative priority between state sovereignty and universal human rights in border control.').

omega_variable(
    border_naturalness_vs_construct,
    'Is the concept of a sovereign border with exclusionary power a natural and inevitable feature of human political organization, or a historically contingent social construct?',
    'Anthropological and historical research demonstrating alternative forms of political organization without exclusionary territoriality, or a shift in global political philosophy towards post-sovereign models.',
    'If a natural feature, the constraint would lean towards a Mountain, reducing its perceived extractiveness. If a construct, its Tangled Rope status would be reinforced, emphasizing its active maintenance and potential for change.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(border_naturalness_vs_construct, empirical, 'Whether the exclusionary border is a natural or constructed phenomenon.').

omega_variable(
    reading_sovereignty_primary_vs_freedom_primary,
    'This constraint is the ''sovereignty_primary'' reading of the ''border_normative_status'' kernel. How would the classification change if the ''freedom_primary'' reading were adopted?',
    'Adoption of the ''freedom_primary'' reading would shift the foundational axiom to ''freedom_of_movement_is_fundamental'', leading to a re-evaluation of border enforcement as inherently extractive and suppressive unless extraordinarily justified.',
    'The ''freedom_primary'' reading would likely classify border enforcement as a Snare, with states as agenda-setters of an extractive mechanism and migrants as victims, and a significantly higher extractiveness score for the constraint itself.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_sovereignty_primary_vs_freedom_primary, conceptual, 'Impact of adopting the ''freedom_primary'' reading on constraint classification.').

omega_variable(
    reading_sovereignty_primary_vs_qualified_sovereignty,
    'This constraint is the ''sovereignty_primary'' reading of the ''border_normative_status'' kernel. How would the classification change if the ''qualified_sovereignty'' reading were adopted?',
    'Adoption of the ''qualified_sovereignty'' reading would introduce proportionality and human rights compliance as intrinsic limits on state border authority, leading to a re-evaluation of the legitimacy of current enforcement practices.',
    'The ''qualified_sovereignty'' reading would likely classify the constraint as a Tangled Rope, but with a lower extractiveness and suppression score, as state authority would be balanced by explicit obligations, potentially leading to more constrained exit options for states and fewer for migrants.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_sovereignty_primary_vs_qualified_sovereignty, conceptual, 'Impact of adopting the ''qualified_sovereignty'' reading on constraint classification.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(border_normative_status__sovereignty_primary, 1945, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bord_tr_t1945, border_normative_status__sovereignty_primary, theater_ratio, 1945, 0.1).
narrative_ontology:measurement(bord_tr_t1965, border_normative_status__sovereignty_primary, theater_ratio, 1965, 0.12).
narrative_ontology:measurement(bord_tr_t1985, border_normative_status__sovereignty_primary, theater_ratio, 1985, 0.13).
narrative_ontology:measurement(bord_tr_t2005, border_normative_status__sovereignty_primary, theater_ratio, 2005, 0.14).
narrative_ontology:measurement(bord_tr_t2024, border_normative_status__sovereignty_primary, theater_ratio, 2024, 0.15).

% Extraction over time
narrative_ontology:measurement(bord_be_t1945, border_normative_status__sovereignty_primary, base_extractiveness, 1945, 0.5).
narrative_ontology:measurement(bord_be_t1965, border_normative_status__sovereignty_primary, base_extractiveness, 1965, 0.55).
narrative_ontology:measurement(bord_be_t1985, border_normative_status__sovereignty_primary, base_extractiveness, 1985, 0.6).
narrative_ontology:measurement(bord_be_t2005, border_normative_status__sovereignty_primary, base_extractiveness, 2005, 0.63).
narrative_ontology:measurement(bord_be_t2024, border_normative_status__sovereignty_primary, base_extractiveness, 2024, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(bord_su_t1945, border_normative_status__sovereignty_primary, suppression_requirement, 1945, 0.6).
narrative_ontology:measurement(bord_su_t1965, border_normative_status__sovereignty_primary, suppression_requirement, 1965, 0.65).
narrative_ontology:measurement(bord_su_t1985, border_normative_status__sovereignty_primary, suppression_requirement, 1985, 0.7).
narrative_ontology:measurement(bord_su_t2005, border_normative_status__sovereignty_primary, suppression_requirement, 2005, 0.75).
narrative_ontology:measurement(bord_su_t2024, border_normative_status__sovereignty_primary, suppression_requirement, 2024, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(border_normative_status__sovereignty_primary, enforcement_mechanism).
narrative_ontology:affects_constraint(border_normative_status__sovereignty_primary, international_refugee_law_application).
narrative_ontology:affects_constraint(border_normative_status__sovereignty_primary, global_labor_mobility_regimes).
narrative_ontology:affects_constraint(border_normative_status__sovereignty_primary, national_identity_formation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
