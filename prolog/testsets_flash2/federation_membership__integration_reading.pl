% ============================================================================
% CONSTRAINT STORY: federation_membership__integration_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_federation_membership__integration_reading, []).

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
 *   constraint_id: federation_membership__integration_reading
 *   human_readable: Federation Membership (Integration Reading)
 *   domain: political_economy/federalism/migration_policy
 *
 * SUMMARY:
 *   This constraint story represents the 'integration reading' of federation
 *   membership, where supranational authority is legitimate, and free
 *   movement is a constitutional right. This reading emphasizes the
 *   irreversible nature of integration, with mobile citizens as primary
 *   beneficiaries and local labor markets bearing significant costs. The
 *   metrics reflect a growing extractiveness and suppression as the
 *   integration project deepened over time, requiring more active enforcement
 *   against national re-assertions of control.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(federation_membership__integration_reading, 0.68).
domain_priors:suppression_score(federation_membership__integration_reading, 0.75).
domain_priors:theater_ratio(federation_membership__integration_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(federation_membership__integration_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(federation_membership__integration_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(federation_membership__integration_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(federation_membership__integration_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(federation_membership__integration_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(federation_membership__integration_reading, tangled_rope).
narrative_ontology:human_readable(federation_membership__integration_reading, "Federation Membership (Integration Reading)").
narrative_ontology:topic_domain(federation_membership__integration_reading, "political_economy/federalism/migration_policy").

domain_priors:requires_active_enforcement(federation_membership__integration_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(federation_membership__integration_reading, 'f30ef732-0d58-4abb-94db-2c9c21b1430e').
narrative_ontology:cs_kernel_codification('f30ef732-0d58-4abb-94db-2c9c21b1430e', formalized).
narrative_ontology:cs_authority_grounding('f30ef732-0d58-4abb-94db-2c9c21b1430e', lineage).
narrative_ontology:cs_interpretation_layer_present('f30ef732-0d58-4abb-94db-2c9c21b1430e').
narrative_ontology:cs_reading_relation('f30ef732-0d58-4abb-94db-2c9c21b1430e', federation_membership__sovereignty_reading, coexists_with).
narrative_ontology:cs_axiom('f30ef732-0d58-4abb-94db-2c9c21b1430e', foundational, integration_is_irreversible).
narrative_ontology:cs_axiom_status(integration_is_irreversible, holdable).
narrative_ontology:cs_axiom_grounding('f30ef732-0d58-4abb-94db-2c9c21b1430e', integration_is_irreversible, conventional).
narrative_ontology:cs_axiom('f30ef732-0d58-4abb-94db-2c9c21b1430e', foundational, free_movement_is_constitutional_right).
narrative_ontology:cs_axiom_status(free_movement_is_constitutional_right, holdable).
narrative_ontology:cs_axiom_grounding('f30ef732-0d58-4abb-94db-2c9c21b1430e', free_movement_is_constitutional_right, deontological).
narrative_ontology:cs_reference_frame('f30ef732-0d58-4abb-94db-2c9c21b1430e', ever_closer_union).
narrative_ontology:cs_drift_state('f30ef732-0d58-4abb-94db-2c9c21b1430e', contemporary_nationalist_resurgence, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('f30ef732-0d58-4abb-94db-2c9c21b1430e', '').
narrative_ontology:cs_kernel_id(federation_membership__integration_reading, federation_membership).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(federation_membership__integration_reading, mobile_citizens).
narrative_ontology:constraint_beneficiary(federation_membership__integration_reading, supranational_institutions).
narrative_ontology:constraint_victim(federation_membership__integration_reading, local_labor_markets).
narrative_ontology:constraint_victim(federation_membership__integration_reading, national_border_authorities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(federation_membership__integration_reading, national_governments).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from the constitutional right to free movement across member states, enabling access to diverse labor markets and social services without national border restrictions. Their mobility is a core tenet of this reading.
narrative_ontology:constraint_stakeholder(federation_membership__integration_reading, mobile_citizens, beneficiary,
    moderate, biographical, mobile, continental).

% Derive their legitimacy and authority from the principle of irreversible integration, interpreting and enforcing free movement as a fundamental right. They actively work to prevent national re-assertion of border controls.
narrative_ontology:constraint_stakeholder(federation_membership__integration_reading, supranational_institutions, agenda_setter,
    institutional, generational, constrained, continental).

% Bear the costs of increased competition, wage depression, and strain on local public services due to uncontrolled influx of labor from other member states. They have limited mechanisms to control or mitigate these effects under the integration framework.
narrative_ontology:constraint_stakeholder(federation_membership__integration_reading, local_labor_markets, payer,
    powerless, immediate, trapped, local).

% Are constitutionally constrained from imposing border controls or migration restrictions on citizens from other member states, even when facing domestic pressure or security concerns. Their traditional function is superseded by supranational law.
narrative_ontology:constraint_stakeholder(federation_membership__integration_reading, national_border_authorities, payer,
    organized, biographical, constrained, national).

% Are bound by the foundational treaties of irreversible integration, limiting their ability to unilaterally control borders or migration policy. They participate in supranational decision-making but are also subject to its authority.
narrative_ontology:constraint_stakeholder(federation_membership__integration_reading, national_governments, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(federation_membership__integration_reading, national_governments, payer).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the economic and social integration of member states by ensuring a single market for labor and services, fostering a shared identity and preventing internal trade barriers.
% TRANSFER_FUNCTION: Transfers sovereign control over border and migration policy from national governments to supranational institutions, in exchange for economic integration and political stability. It also transfers labor and social costs to specific local markets.
% ABSENT_VOICES: Nationalist movements and local communities disproportionately affected by migration flows are often marginalized in the supranational discourse, arguing for a return to national control over borders and a re-evaluation of integration's costs.
% DISAPPEARANCE_RATIONALE: If the principle of irreversible integration and free movement vanished, national borders would immediately re-assert, leading to widespread disruption of labor markets, supply chains, and social structures across the former federation. Supranational institutions would lose their primary mandate.
% FOUNDING_PROBLEM: The founding problem was to prevent future inter-state conflicts, foster economic prosperity through a common market, and build a shared political identity after centuries of war.
% FOUNDING_PROBLEM_CORROBORATION: Supranational institutions and many mobile citizens attest that the problem of inter-state conflict and economic fragmentation remains live, requiring continued integration. Critics, including some national governments and local communities, argue that the original problem has been superseded by new challenges related to sovereignty and democratic accountability, making the status contested.
narrative_ontology:disappearance_verdict(federation_membership__integration_reading, world_rearranges).
narrative_ontology:founding_problem_status(federation_membership__integration_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(federation_membership__integration_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(federation_membership__integration_reading, 'none', 1).
narrative_ontology:epsilon_provenance(federation_membership__integration_reading, 0.68, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(federation_membership__integration_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(federation_membership__integration_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(federation_membership__integration_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.68) is high due to the uncompensated costs borne by local labor markets and the loss of national policy levers. Suppression (0.75) is substantial because national attempts to re-assert border controls are actively resisted and legally challenged by supranational institutions. The claimed type is 'tangled_rope' because it genuinely coordinates economic integration while simultaneously extracting sovereignty and imposing costs on specific national/local actors through the same structure, requiring active enforcement to maintain.
 *
 * PERSPECTIVAL GAP:
 *   Supranational institutions and mobile citizens perceive this as a beneficial 'rope' for coordination and rights, while national border authorities and local labor markets experience it as a 'snare' due to the imposed costs and loss of control. The engine's per-seat classification will reflect this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Mobile citizens are full beneficiaries (d=0.0) as the constraint directly subsidizes their mobility. Supranational institutions are also beneficiaries (d=0.1) as their power and legitimacy are enhanced. Local labor markets and national border authorities are targets (d=0.9 and d=0.8 respectively) as they bear the direct costs and loss of control. National governments are dual-positioned, acting as agenda-setters at the supranational level but also paying costs at the national level (d=0.5).
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as a Tangled Rope prevents mislabeling the constraint as a pure 'rope' (as proponents claim) by highlighting the asymmetric extraction and active enforcement required. It also avoids mislabeling it as a pure 'snare' by acknowledging the genuine coordination function of economic and political integration. The rising extractiveness and suppression over time suggest a drift towards greater extraction, even as the coordination function persists.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    integration_irreversibility_ambiguity,
    'Is federation membership truly irreversible, or can member states unilaterally re-assert national sovereignty and border controls without catastrophic consequences?',
    'Empirical observation of a member state attempting to exit or re-negotiate core integration principles, and the resulting economic/political fallout.',
    'If reversibility is demonstrated, the constraint''s suppression and extractiveness would be re-evaluated downwards, potentially reclassifying it as a ''rope'' or ''scaffold'' rather than a ''tangled_rope'', as exit options would be less constrained.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(integration_irreversibility_ambiguity, empirical, 'Uncertainty regarding the true irreversibility of federation membership.').

omega_variable(
    sovereignty_vs_integration_framing,
    'Is the core commitment of federation membership primarily about ''integration'' (pooling sovereignty for common good) or ''sovereignty'' (conditional delegation of powers)?',
    'Conceptual analysis of foundational treaties and legal precedents, combined with a survey of political discourse among member states'' populations and elites.',
    'If the ''sovereignty'' framing gains dominance, the ''integration_reading'' constraint would be seen as more extractive and suppressive, as it would be viewed as exceeding the original mandate. This would likely shift its classification towards a ''snare'' from the national perspective.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sovereignty_vs_integration_framing, conceptual, 'Ambiguity in the foundational framing of federation membership.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(federation_membership__integration_reading, 1950, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Extraction over time
narrative_ontology:measurement(fede_be_t1950, federation_membership__integration_reading, base_extractiveness, 1950, 0.3).
narrative_ontology:measurement(fede_be_t1970, federation_membership__integration_reading, base_extractiveness, 1970, 0.45).
narrative_ontology:measurement(fede_be_t1990, federation_membership__integration_reading, base_extractiveness, 1990, 0.58).
narrative_ontology:measurement(fede_be_t2010, federation_membership__integration_reading, base_extractiveness, 2010, 0.65).
narrative_ontology:measurement(fede_be_t2024, federation_membership__integration_reading, base_extractiveness, 2024, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(fede_su_t1950, federation_membership__integration_reading, suppression_requirement, 1950, 0.4).
narrative_ontology:measurement(fede_su_t1970, federation_membership__integration_reading, suppression_requirement, 1970, 0.55).
narrative_ontology:measurement(fede_su_t1990, federation_membership__integration_reading, suppression_requirement, 1990, 0.68).
narrative_ontology:measurement(fede_su_t2010, federation_membership__integration_reading, suppression_requirement, 2010, 0.72).
narrative_ontology:measurement(fede_su_t2024, federation_membership__integration_reading, suppression_requirement, 2024, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(federation_membership__integration_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(federation_membership__integration_reading, common_market_regulations).
narrative_ontology:affects_constraint(federation_membership__integration_reading, supranational_judicial_review).

% DUAL FORMULATION NOTE:
% This constraint is the 'integration_reading' of the 'federation_membership' kernel. It emphasizes irreversible integration, supranational authority, and free movement as a constitutional right. The sibling 'sovereignty_reading' (federation_membership__sovereignty_reading) emphasizes conditional treaty, national border legitimacy, and negotiable free movement.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
