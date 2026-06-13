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
 *   constraint_id: border_normative_status__sovereignty_primary
 *   human_readable: Sovereignty-Primary Border Normative Status
 *   domain: political_philosophy/international_law/migration_studies
 *
 * SUMMARY:
 *   This constraint represents the 'sovereignty-primary' reading of the
 *   normative status of borders, asserting that states have foundational
 *   authority to exclude non-members as an instrument of collective
 *   self-determination. It is a Snare from the perspective of excluded
 *   non-members, as it imposes high costs and severe suppression without
 *   their consent, while benefiting citizen members and the state apparatus.
 *   This reading is one of several competing interpretations of the
 *   'border_normative_status' kernel.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(border_normative_status__sovereignty_primary, 0.85).
domain_priors:suppression_score(border_normative_status__sovereignty_primary, 0.9).
domain_priors:theater_ratio(border_normative_status__sovereignty_primary, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(border_normative_status__sovereignty_primary, extractiveness, 0.85).
narrative_ontology:constraint_metric(border_normative_status__sovereignty_primary, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(border_normative_status__sovereignty_primary, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(border_normative_status__sovereignty_primary, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(border_normative_status__sovereignty_primary, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(border_normative_status__sovereignty_primary, snare).
narrative_ontology:human_readable(border_normative_status__sovereignty_primary, "Sovereignty-Primary Border Normative Status").
narrative_ontology:topic_domain(border_normative_status__sovereignty_primary, "political_philosophy/international_law/migration_studies").

domain_priors:requires_active_enforcement(border_normative_status__sovereignty_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(border_normative_status__sovereignty_primary, '93abb5b8-64c9-4e31-b819-6423bdc3da82').
narrative_ontology:cs_kernel_codification('93abb5b8-64c9-4e31-b819-6423bdc3da82', formalized).
narrative_ontology:cs_authority_grounding('93abb5b8-64c9-4e31-b819-6423bdc3da82', lineage).
narrative_ontology:cs_interpretation_layer_present('93abb5b8-64c9-4e31-b819-6423bdc3da82').
narrative_ontology:cs_reading_relation('93abb5b8-64c9-4e31-b819-6423bdc3da82', border_normative_status__freedom_primary, forecloses).
narrative_ontology:cs_reading_relation('93abb5b8-64c9-4e31-b819-6423bdc3da82', border_normative_status__qualified_sovereignty, coexists_with).
narrative_ontology:cs_axiom('93abb5b8-64c9-4e31-b819-6423bdc3da82', foundational, state_sovereignty_is_foundational).
narrative_ontology:cs_axiom_status(state_sovereignty_is_foundational, holdable).
narrative_ontology:cs_axiom_grounding('93abb5b8-64c9-4e31-b819-6423bdc3da82', state_sovereignty_is_foundational, deontological).
narrative_ontology:cs_axiom('93abb5b8-64c9-4e31-b819-6423bdc3da82', foundational, collective_self_determination_requires_exclusion).
narrative_ontology:cs_axiom_status(collective_self_determination_requires_exclusion, holdable).
narrative_ontology:cs_axiom_grounding('93abb5b8-64c9-4e31-b819-6423bdc3da82', collective_self_determination_requires_exclusion, conventional).
narrative_ontology:cs_reference_frame('93abb5b8-64c9-4e31-b819-6423bdc3da82', westphalian_sovereignty_model).
narrative_ontology:cs_drift_state('93abb5b8-64c9-4e31-b819-6423bdc3da82', contemporary_human_rights_era, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('93abb5b8-64c9-4e31-b819-6423bdc3da82', '').
narrative_ontology:cs_kernel_id(border_normative_status__sovereignty_primary, border_normative_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(border_normative_status__sovereignty_primary, citizen_members).
narrative_ontology:constraint_beneficiary(border_normative_status__sovereignty_primary, state_apparatus).
narrative_ontology:constraint_victim(border_normative_status__sovereignty_primary, excluded_non_members).
narrative_ontology:constraint_victim(border_normative_status__sovereignty_primary, asylum_seekers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The state, through its legislative and executive branches, defines citizenship, controls borders, and enforces exclusion policies. It claims this authority as foundational to its existence and the collective self-determination of its members. It benefits from maintaining control over its territory and population.
narrative_ontology:constraint_stakeholder(border_normative_status__sovereignty_primary, state_apparatus, agenda_setter,
    institutional, generational, arbitrage, national).

% Citizens benefit from the perceived security, cultural cohesion, and resource allocation within the state's borders, which they believe is protected by the exclusion of non-members. They exercise their collective self-determination through the state's actions.
narrative_ontology:constraint_stakeholder(border_normative_status__sovereignty_primary, citizen_members, beneficiary,
    organized, generational, mobile, national).

% Individuals who are denied entry or residence, facing severe restrictions on their freedom of movement, economic opportunity, and often personal safety. They bear the direct costs of exclusion, including separation from family, loss of livelihood, and risk of detention or deportation.
narrative_ontology:constraint_stakeholder(border_normative_status__sovereignty_primary, excluded_non_members, payer,
    powerless, biographical, trapped, global).

% Individuals fleeing persecution or conflict, seeking protection across borders. Their claims are often adjudicated under the state's sovereign authority, which may prioritize national interests over individual protection, leading to prolonged detention, denial of status, or forced return to unsafe conditions. Their identity as refugees is often tied to the very border they seek to cross.
narrative_ontology:constraint_stakeholder(border_normative_status__sovereignty_primary, asylum_seekers, payer,
    powerless, immediate, identity_locked, global).

% Monitor state compliance with international human rights law, including refugee conventions. They issue reports and recommendations but lack direct enforcement power over sovereign states, often finding their mandates in tension with claims of absolute state sovereignty.
narrative_ontology:constraint_stakeholder(border_normative_status__sovereignty_primary, international_human_rights_bodies, observer,
    institutional, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the collective self-determination of a national group by defining its membership and territorial scope, enabling shared governance and resource allocation within defined boundaries.
% TRANSFER_FUNCTION: Transfers the right to determine membership and control territory to the state, which then exercises this authority to exclude non-members, thereby transferring security and resource access to citizens at the cost of freedom and opportunity for non-members.
% ABSENT_VOICES: The voices of those excluded by borders, particularly those with compelling humanitarian claims, are largely absent from the decision-making processes that define and enforce these boundaries. They would argue for universal human rights and freedom of movement.
% DISAPPEARANCE_RATIONALE: If the normative status of borders as instruments of sovereign exclusion vanished, states would lose a foundational claim to territorial control. This would lead to massive population movements, a redefinition of citizenship, and a complete reorganization of international relations and global governance, as the very concept of a 'state' as currently understood would be undermined.
% FOUNDING_PROBLEM: The problem of defining a political community, securing its territory, and ensuring the collective self-determination and safety of its members against external threats and uncontrolled entry.
% FOUNDING_PROBLEM_CORROBORATION: The state apparatus and citizen members consistently attest that the founding problem of collective self-determination and security remains live, citing ongoing geopolitical instability and migration pressures. While international human rights bodies acknowledge the need for order, they contest the extent to which absolute exclusion is a necessary or legitimate solution, arguing for a more balanced approach.
narrative_ontology:disappearance_verdict(border_normative_status__sovereignty_primary, world_rearranges).
narrative_ontology:founding_problem_status(border_normative_status__sovereignty_primary, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(border_normative_status__sovereignty_primary, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(border_normative_status__sovereignty_primary, 'none', 1).

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
 *   The extractiveness (0.85) is high due to the severe costs imposed on excluded non-members, including loss of life, liberty, and opportunity. Suppression (0.9) is also very high, as states employ extensive legal, physical, and technological means to prevent entry and remove those deemed inadmissible. The theater ratio (0.1) is low, indicating that border enforcement is largely functional in achieving its stated goal of exclusion, with minimal performative aspects. Resistance (0.6) is substantial, coming from excluded individuals, human rights advocates, and some international bodies.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the state and its citizens, this constraint is a legitimate exercise of self-determination, perhaps even a Mountain or Rope. From the perspective of excluded non-members, it is a clear Snare, designed to extract their freedom and opportunity for the benefit of others. The engine's classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   The state apparatus and citizen members are clear beneficiaries (d near 0.0), as they gain control, security, and the ability to define their collective. Excluded non-members and asylum seekers are the primary targets (d near 1.0), bearing the full weight of the constraint's coercive power with minimal to no exit options. International human rights bodies are analytical observers, neither directly benefiting nor paying, but attempting to influence the constraint's operation.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sovereignty_vs_human_rights,
    'Is the state''s foundational authority to exclude non-members absolute, or is it inherently limited by universal human rights obligations?',
    'International legal precedent, evolving customary international law, and the outcome of major international tribunals or treaties that explicitly reconcile or prioritize these claims.',
    'If human rights obligations are deemed to limit sovereignty, the constraint''s legitimacy and scope of enforcement would be curtailed, potentially reclassifying it towards a Tangled Rope or Scaffold. If sovereignty is absolute, the Snare classification is reinforced.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sovereignty_vs_human_rights, conceptual, 'The fundamental tension between state sovereignty and universal human rights in border control.').

omega_variable(
    collective_self_determination_scope,
    'Does ''collective self-determination'' legitimately extend to absolute exclusion, or does it imply a responsibility to consider the global impact of such exclusion?',
    'Philosophical consensus on the scope of collective rights, or the emergence of new international norms regarding global distributive justice and shared responsibility for displaced populations.',
    'If self-determination is found to have external limits, the justification for high extractiveness and suppression would weaken, pushing the classification towards a Tangled Rope. If it is absolute, the current Snare classification is robust.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(collective_self_determination_scope, preference, 'The scope and limits of collective self-determination as a justification for border exclusion.').

omega_variable(
    displacement_as_externality,
    'Is the displacement and suffering of excluded non-members a legitimate externality of sovereign border control, or an intrinsic cost that must be accounted for by the excluding state?',
    'Legal and ethical frameworks that internalize the costs of displacement, e.g., through mandatory resettlement quotas or compensation mechanisms for states contributing to displacement.',
    'If treated as an intrinsic cost, the effective extractiveness of the constraint would be re-evaluated to include these costs, potentially strengthening the Snare classification or demanding new forms of accountability. If it remains an externality, the current classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(displacement_as_externality, empirical, 'Whether the costs borne by excluded non-members are an externality or an intrinsic part of the constraint''s operation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(border_normative_status__sovereignty_primary, 1945, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bord_tr_t1945, border_normative_status__sovereignty_primary, theater_ratio, 1945, 0.05).
narrative_ontology:measurement(bord_tr_t1970, border_normative_status__sovereignty_primary, theater_ratio, 1970, 0.07).
narrative_ontology:measurement(bord_tr_t1990, border_normative_status__sovereignty_primary, theater_ratio, 1990, 0.08).
narrative_ontology:measurement(bord_tr_t2010, border_normative_status__sovereignty_primary, theater_ratio, 2010, 0.09).
narrative_ontology:measurement(bord_tr_t2024, border_normative_status__sovereignty_primary, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(bord_be_t1945, border_normative_status__sovereignty_primary, base_extractiveness, 1945, 0.7).
narrative_ontology:measurement(bord_be_t1970, border_normative_status__sovereignty_primary, base_extractiveness, 1970, 0.75).
narrative_ontology:measurement(bord_be_t1990, border_normative_status__sovereignty_primary, base_extractiveness, 1990, 0.8).
narrative_ontology:measurement(bord_be_t2010, border_normative_status__sovereignty_primary, base_extractiveness, 2010, 0.83).
narrative_ontology:measurement(bord_be_t2024, border_normative_status__sovereignty_primary, base_extractiveness, 2024, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(bord_su_t1945, border_normative_status__sovereignty_primary, suppression_requirement, 1945, 0.75).
narrative_ontology:measurement(bord_su_t1970, border_normative_status__sovereignty_primary, suppression_requirement, 1970, 0.8).
narrative_ontology:measurement(bord_su_t1990, border_normative_status__sovereignty_primary, suppression_requirement, 1990, 0.85).
narrative_ontology:measurement(bord_su_t2010, border_normative_status__sovereignty_primary, suppression_requirement, 2010, 0.88).
narrative_ontology:measurement(bord_su_t2024, border_normative_status__sovereignty_primary, suppression_requirement, 2024, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(border_normative_status__sovereignty_primary, enforcement_mechanism).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'border_normative_status' kernel. The other readings are 'freedom_primary' and 'qualified_sovereignty', which offer alternative justifications and structural outcomes for border control.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
