% ============================================================================
% CONSTRAINT STORY: border_normative_status__freedom_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_border_normative_status__freedom_primary, []).

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
 *   constraint_id: border_normative_status__freedom_primary
 *   human_readable: Border Control Regime â Freedom-of-Movement Primary Reading
 *   domain: political/international_law/migration
 *
 * SUMMARY:
 *   This constraint story instantiates the freedom_primary reading of the
 *   border_normative_status kernel. The standing arrangement under contest is
 *   the international regime of territorial border controls and migration
 *   restrictions. From this reading, freedom of movement is a fundamental
 *   human right that borders impermissibly restrict; exclusion requires
 *   extraordinary justification. The current border enforcement regime is
 *   therefore read as a snare: a system of pure extraction whose coordination
 *   story (jurisdiction, public goods) is cover for the denial of a
 *   fundamental liberty. The constraint is actively enforced through walls,
 *   detention, deportation, and bureaucratic exclusion. The structural delta
 *   of this reading is that excluded migrants are not merely wrongly excluded
 *   within a legitimate framework but are victims of an illegitimate
 *   structure tout court, while workers trapped in domestic labor markets by
 *   emigration restrictions enter the victim set.
 *
 * KEY AGENTS:
 *   - Sovereign states (institutional/mobile): Primary agenda-setters and beneficiaries of territorial control.
 *   - Border enforcement agencies (organized/constrained): Administrative apparatus that executes exclusion.
 *   - Displaced domestic workers (powerless/trapped): Victims unable to emigrate due to border restrictions.
 *   - Asylum seekers (powerless/trapped): Victims of interception, detention, and refoulement.
 *   - Human rights observers (organized/analytical): Analytical position documenting violations from outside.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(border_normative_status__freedom_primary, 0.88).
domain_priors:suppression_score(border_normative_status__freedom_primary, 0.82).
domain_priors:theater_ratio(border_normative_status__freedom_primary, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(border_normative_status__freedom_primary, extractiveness, 0.88).
narrative_ontology:constraint_metric(border_normative_status__freedom_primary, suppression_requirement, 0.82).
narrative_ontology:constraint_metric(border_normative_status__freedom_primary, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(border_normative_status__freedom_primary, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(border_normative_status__freedom_primary, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(border_normative_status__freedom_primary, snare).
narrative_ontology:human_readable(border_normative_status__freedom_primary, "Border Control Regime â Freedom-of-Movement Primary Reading").
narrative_ontology:topic_domain(border_normative_status__freedom_primary, "political/international_law/migration").

domain_priors:requires_active_enforcement(border_normative_status__freedom_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(border_normative_status__freedom_primary, '9dd22e4e-15e4-438f-b6c8-6183a232c5f7').
narrative_ontology:cs_kernel_codification('9dd22e4e-15e4-438f-b6c8-6183a232c5f7', fixed_text).
narrative_ontology:cs_authority_grounding('9dd22e4e-15e4-438f-b6c8-6183a232c5f7', lineage).
narrative_ontology:cs_interpretation_layer_present('9dd22e4e-15e4-438f-b6c8-6183a232c5f7').
narrative_ontology:cs_reading_relation('9dd22e4e-15e4-438f-b6c8-6183a232c5f7', border_normative_status__sovereignty_primary, forecloses).
narrative_ontology:cs_reading_relation('9dd22e4e-15e4-438f-b6c8-6183a232c5f7', border_normative_status__qualified_sovereignty, coexists_with).
narrative_ontology:cs_axiom('9dd22e4e-15e4-438f-b6c8-6183a232c5f7', foundational, freedom_of_movement_prepolitical).
narrative_ontology:cs_axiom_status(freedom_of_movement_prepolitical, holdable).
narrative_ontology:cs_axiom_grounding('9dd22e4e-15e4-438f-b6c8-6183a232c5f7', freedom_of_movement_prepolitical, deontological).
narrative_ontology:cs_axiom('9dd22e4e-15e4-438f-b6c8-6183a232c5f7', foundational, exclusion_requires_extraordinary_justification).
narrative_ontology:cs_axiom_status(exclusion_requires_extraordinary_justification, holdable).
narrative_ontology:cs_axiom_grounding('9dd22e4e-15e4-438f-b6c8-6183a232c5f7', exclusion_requires_extraordinary_justification, deontological).
narrative_ontology:cs_reference_frame('9dd22e4e-15e4-438f-b6c8-6183a232c5f7', freedom_as_prepolitical_right).
narrative_ontology:cs_drift_state('9dd22e4e-15e4-438f-b6c8-6183a232c5f7', contemporary_state_practice, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('9dd22e4e-15e4-438f-b6c8-6183a232c5f7', '').
narrative_ontology:cs_kernel_id(border_normative_status__freedom_primary, border_normative_status).

% --- Structural relationships ---
narrative_ontology:constraint_victim(border_normative_status__freedom_primary, displaced_domestic_workers).
narrative_ontology:constraint_victim(border_normative_status__freedom_primary, asylum_seekers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(border_normative_status__freedom_primary, sovereign_states).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Construct and enforce territorial border controls, determining membership and exclusion. They capture sovereignty rents and jurisdictional control by restricting the entry and exit of non-citizens, while remaining formally free to liberalize or tighten policies.
narrative_ontology:constraint_stakeholder(border_normative_status__freedom_primary, sovereign_states, agenda_setter,
    institutional, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(border_normative_status__freedom_primary, sovereign_states, beneficiary).

% Physically administer border controls, operate detention facilities, and conduct deportations. Their budgets and mandates depend on the continuation of restrictive border regimes, though individual personnel may exit the profession.
narrative_ontology:constraint_stakeholder(border_normative_status__freedom_primary, border_enforcement_agencies, agenda_setter,
    organized, biographical, constrained, national).

% Workers trapped in low-wage domestic labor markets by emigration restrictions and border controls that prevent them from accessing higher-wage foreign labor markets, bearing the cost in foregone income and continued precarity.
narrative_ontology:constraint_stakeholder(border_normative_status__freedom_primary, displaced_domestic_workers, payer,
    powerless, biographical, trapped, national).

% Persons fleeing persecution or violence who are intercepted at borders, detained, or returned to unsafe territories. They bear the immediate cost of the enforcement regime through denial of refuge and physical danger.
narrative_ontology:constraint_stakeholder(border_normative_status__freedom_primary, asylum_seekers, payer,
    powerless, immediate, trapped, global).

% International organizations, legal scholars, and advocacy groups that document border violations and assert freedom of movement as a fundamental right. They do not collect from or pay into the constraint, but analyze and contest its legitimacy.
narrative_ontology:constraint_stakeholder(border_normative_status__freedom_primary, human_rights_observers, observer,
    organized, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Demarcates territorial jurisdiction and allocates membership for the provision of public goods and collective self-governance among states.
% TRANSFER_FUNCTION: Transfers freedom of movement and economic opportunity from mobility-restricted persons to sovereign states and their citizen populations through enforced territorial exclusion.
% ABSENT_VOICES: Stateless persons and unrecognized populations are structurally excluded from the political negotiations that establish border policies. Indigenous peoples whose territories are divided by borders are absent from the frameworks that legitimate those lines.
% DISAPPEARANCE_RATIONALE: If the border enforcement regime vanished overnight, labor markets would reorient toward open mobility, public goods systems would shift to residence-based models, and the current architecture of sovereignty and membership would undergo rapid transformation.
% FOUNDING_PROBLEM: The arrangement was built to solve the coordination problem of defining political community jurisdiction, managing collective self-determination, and allocating public goods in a world of territorial states.
% FOUNDING_PROBLEM_CORROBORATION: Critical migration scholars and human rights institutions attest from outside the beneficiary set that the original coordination problem has been superseded by alternative governance mechanisms and that the persistence of hard borders now serves primarily extractive and exclusionary functions unrelated to the founding need.
narrative_ontology:disappearance_verdict(border_normative_status__freedom_primary, world_rearranges).
narrative_ontology:founding_problem_status(border_normative_status__freedom_primary, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(border_normative_status__freedom_primary, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(border_normative_status__freedom_primary, 'none', 1).
narrative_ontology:epsilon_provenance(border_normative_status__freedom_primary, 0.88, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(border_normative_status__freedom_primary_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(border_normative_status__freedom_primary, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(border_normative_status__freedom_primary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.88 because the constraint denies a right classified by this reading as fundamental and prepolitical. Suppression is 0.82 because the regime's persistence depends on massive active enforcement (physical barriers, armed personnel, detention infrastructure) rather than voluntary compliance. Theater_ratio at 0.45 reflects the growing performative dimension of border policy (spectacle of walls, deterrence through visible cruelty, sovereignty rituals) relative to genuine coordination function. Accessibility_collapse at 0.75 captures the near-unthinkability of open-border alternatives within mainstream policy discourse. Resistance at 0.60 registers substantial but uneven opposition from migrant movements and human rights institutions. The temporal series show monotonic intensification from 1985-2025 as capital mobility increased while labor mobility remained rigidly controlled.
 *
 * PERSPECTIVAL GAP:
 *   The sovereign state seat experiences the constraint as a necessary support for political community and public goods provision; the engine will compute a low directionality and possibly a rope or tangled_rope classification from that seat. The asylum_seeker and displaced_domestic_worker seats experience the same structure as direct extraction of their liberty and opportunity; the engine will compute high directionality and snare classification. The divergence is structural, not perspectival error.
 *
 * DIRECTIONALITY LOGIC:
 *   Sovereign states and border enforcement agencies are structural beneficiaries (low d): the constraint subsidizes their control over territory and labor markets. Displaced domestic workers and asylum seekers are structural targets (high d): the constraint extracts freedom of movement and economic opportunity from them directly. Human rights observers occupy the analytical position with neutral d. The directionality derivation chain is driven by the victim declarations (payers) and the enforcement structure.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint prevents mislabeling by preserving the coordination-function narrative (jurisdiction, public goods) in the six_questions while authoring metrics that show the coordination story is cover for extraction. The founding_problem_status is declared dead because the original coordination need (territorial jurisdiction in a state system) has been superseded by human rights frameworks and alternative governance mechanisms. The high theater_ratio and rising suppression_requirement over the interval confirm that what persists is not the founding coordination but its theatrical maintenance.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    residual_coordination_justification,
    'Does any residual coordination function (public health, genuine security threat) justify a constrained form of territorial exclusion, or does freedom_primary render all border extraction illegitimate?',
    'Comparative case analysis of jurisdictions that have liberalized border regimes during non-crisis periods, measuring whether public goods provision and political stability collapsed or adapted.',
    'If genuine residual coordination exists, the constraint might reclassify as tangled_rope rather than snare; if none, the snare classification is reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(residual_coordination_justification, conceptual, 'Whether any territorial exclusion remains justified by coordination needs.').

omega_variable(
    victim_scope_displacement,
    'Are displaced domestic workers victims of border controls directly, or of domestic economic structures that would persist even with open borders?',
    'Econometric analysis of wage differentials and emigration elasticities in paired open/restricted labor markets, isolating the border effect from domestic institutional effects.',
    'If domestic structures are the primary cause, the victim set narrows and extractiveness may be revised downward; if borders are the binding constraint, the victim set and extractiveness remain high.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(victim_scope_displacement, empirical, 'Disentangling border causation from domestic economic causation for trapped workers.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(border_normative_status__freedom_primary, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bord_tr_t0, border_normative_status__freedom_primary, theater_ratio, 0, 0.2).
narrative_ontology:measurement(bord_tr_t8, border_normative_status__freedom_primary, theater_ratio, 8, 0.25).
narrative_ontology:measurement(bord_tr_t16, border_normative_status__freedom_primary, theater_ratio, 16, 0.3).
narrative_ontology:measurement(bord_tr_t24, border_normative_status__freedom_primary, theater_ratio, 24, 0.36).
narrative_ontology:measurement(bord_tr_t32, border_normative_status__freedom_primary, theater_ratio, 32, 0.41).
narrative_ontology:measurement(bord_tr_t40, border_normative_status__freedom_primary, theater_ratio, 40, 0.45).

% Extraction over time
narrative_ontology:measurement(bord_be_t0, border_normative_status__freedom_primary, base_extractiveness, 0, 0.62).
narrative_ontology:measurement(bord_be_t8, border_normative_status__freedom_primary, base_extractiveness, 8, 0.68).
narrative_ontology:measurement(bord_be_t16, border_normative_status__freedom_primary, base_extractiveness, 16, 0.74).
narrative_ontology:measurement(bord_be_t24, border_normative_status__freedom_primary, base_extractiveness, 24, 0.8).
narrative_ontology:measurement(bord_be_t32, border_normative_status__freedom_primary, base_extractiveness, 32, 0.85).
narrative_ontology:measurement(bord_be_t40, border_normative_status__freedom_primary, base_extractiveness, 40, 0.88).

% Suppression requirement over time
narrative_ontology:measurement(bord_su_t0, border_normative_status__freedom_primary, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(bord_su_t8, border_normative_status__freedom_primary, suppression_requirement, 8, 0.62).
narrative_ontology:measurement(bord_su_t16, border_normative_status__freedom_primary, suppression_requirement, 16, 0.68).
narrative_ontology:measurement(bord_su_t24, border_normative_status__freedom_primary, suppression_requirement, 24, 0.74).
narrative_ontology:measurement(bord_su_t32, border_normative_status__freedom_primary, suppression_requirement, 32, 0.79).
narrative_ontology:measurement(bord_su_t40, border_normative_status__freedom_primary, suppression_requirement, 40, 0.82).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(border_normative_status__freedom_primary, border_normative_status__sovereignty_primary).
narrative_ontology:affects_constraint(border_normative_status__freedom_primary, border_normative_status__qualified_sovereignty).

% DUAL FORMULATION NOTE:
% The colloquial label 'border sovereignty' conflates three structurally distinct claims. This story isolates the freedom_primary claim; sibling stories isolate sovereignty_primary and qualified_sovereignty. They form a constraint family linked by network edges.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
