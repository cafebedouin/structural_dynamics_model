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
 *   This constraint represents the 'integration reading' of federation
 *   membership, where supranational authority is legitimate, and free
 *   movement is an irreversible constitutional right. This reading emphasizes
 *   deeper political and economic integration, often at the expense of
 *   national sovereignty. The constraint is classified as a Tangled Rope due
 *   to its genuine coordination function (economic integration, peace)
 *   coupled with significant asymmetric extraction from national labor
 *   markets and border authorities, maintained by active enforcement from
 *   supranational institutions. The metrics reflect substantial extraction
 *   and suppression, with low theater, indicating a functional but highly
 *   contested arrangement.
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
narrative_ontology:cs_story_uid(federation_membership__integration_reading, '392eb956-94b3-497e-ac3a-de1262a5da44').
narrative_ontology:cs_kernel_codification('392eb956-94b3-497e-ac3a-de1262a5da44', formalized).
narrative_ontology:cs_authority_grounding('392eb956-94b3-497e-ac3a-de1262a5da44', lineage).
narrative_ontology:cs_interpretation_layer_present('392eb956-94b3-497e-ac3a-de1262a5da44').
narrative_ontology:cs_reading_relation('392eb956-94b3-497e-ac3a-de1262a5da44', federation_membership__sovereignty_reading, coexists_with).
narrative_ontology:cs_axiom('392eb956-94b3-497e-ac3a-de1262a5da44', foundational, supranational_law_supremacy).
narrative_ontology:cs_axiom_status(supranational_law_supremacy, holdable).
narrative_ontology:cs_axiom_grounding('392eb956-94b3-497e-ac3a-de1262a5da44', supranational_law_supremacy, deontological).
narrative_ontology:cs_axiom('392eb956-94b3-497e-ac3a-de1262a5da44', foundational, free_movement_constitutional_right).
narrative_ontology:cs_axiom_status(free_movement_constitutional_right, holdable).
narrative_ontology:cs_axiom_grounding('392eb956-94b3-497e-ac3a-de1262a5da44', free_movement_constitutional_right, deontological).
narrative_ontology:cs_reference_frame('392eb956-94b3-497e-ac3a-de1262a5da44', ever_closer_union_principle).
narrative_ontology:cs_drift_state('392eb956-94b3-497e-ac3a-de1262a5da44', contemporary_nationalist_resurgence, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('392eb956-94b3-497e-ac3a-de1262a5da44', '').
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

% Benefit from the constitutional right to free movement, allowing them to live and work across member states without significant barriers. This enhances their economic opportunities and personal freedoms, but they may face social integration challenges.
narrative_ontology:constraint_stakeholder(federation_membership__integration_reading, mobile_citizens, beneficiary,
    moderate, biographical, mobile, continental).

% Interpret and enforce the constitutional right to free movement, asserting their legitimacy over national laws. They gain authority and expand their mandate through deeper integration, but face resistance from national governments.
narrative_ontology:constraint_stakeholder(federation_membership__integration_reading, supranational_institutions, agenda_setter,
    institutional, generational, identity_locked, continental).

% Bear the costs of increased competition for jobs, wage depression in certain sectors, and strain on public services due to rapid influxes of workers. They have limited ability to control migration flows or mitigate impacts.
narrative_ontology:constraint_stakeholder(federation_membership__integration_reading, local_labor_markets, payer,
    powerless, immediate, constrained, local).

% Are constrained in their ability to control national borders and migration flows, as free movement is a constitutional right. They must enforce supranational directives, often against domestic political pressure, leading to a perceived loss of sovereignty.
narrative_ontology:constraint_stakeholder(federation_membership__integration_reading, national_border_authorities, payer,
    institutional, biographical, constrained, national).

% Are bound by the constitutional right to free movement, limiting their ability to enact independent migration policies. They must balance supranational obligations with national interests and public opinion, often leading to political tension.
narrative_ontology:constraint_stakeholder(federation_membership__integration_reading, national_governments, payer,
    powerful, generational, constrained, national).

% Argue for the primacy of national sovereignty and the right to control borders, viewing free movement as an erosion of national identity and self-determination. Their arguments are often marginalized by the integrationist legal framework.
narrative_ontology:constraint_stakeholder(federation_membership__integration_reading, sovereignty_advocates, excluded,
    organized, generational, identity_locked, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Facilitates economic integration and cultural exchange across member states by removing barriers to labor mobility and residency, fostering a unified internal market and shared identity.
% TRANSFER_FUNCTION: Transfers sovereign control over migration policy from national governments to supranational institutions, and economic opportunities from local labor markets to mobile citizens.
% ABSENT_VOICES: Local communities and national sovereignty advocates, who would argue for greater control over borders and local labor market protection, are structurally excluded from the supranational decision-making process that enshrines free movement as an irreversible right.
% DISAPPEARANCE_RATIONALE: If the integrationist reading of federation membership vanished, national governments would immediately reassert border controls, mobile citizens would lose their constitutional right to free movement, and the supranational institutions would lose a core pillar of their authority. The entire federal structure would revert to a looser confederation of states.
% FOUNDING_PROBLEM: The problem of fragmented national markets, limited economic opportunities, and historical conflicts between European nations, which integration aimed to overcome by fostering interdependence and shared prosperity.
% FOUNDING_PROBLEM_CORROBORATION: Supranational institutions and many mobile citizens attest that the founding problems of fragmentation and limited opportunity remain live, justifying continued integration. National governments and local labor markets, however, contest the efficacy and fairness of the current approach, arguing that new problems (e.g., social dumping, strain on public services) have emerged.
narrative_ontology:disappearance_verdict(federation_membership__integration_reading, world_rearranges).
narrative_ontology:founding_problem_status(federation_membership__integration_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(federation_membership__integration_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
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
 *   Extractiveness is high (0.68) because the benefits of free movement (for mobile citizens and supranational institutions) come at a significant cost to local labor markets and national control. Suppression (0.75) is also high, as national governments and border authorities are actively constrained by supranational law, and alternatives to free movement are suppressed. Theater ratio is low (0.15) because the supranational institutions genuinely enforce the right to free movement, and the coordination function is real, even if its costs are unevenly distributed. The slight dip in extractiveness at the end of the interval reflects increased resistance and political pressure from national actors, leading to some (minor) concessions or re-evaluations, but not a fundamental shift in the constraint's structure.
 *
 * PERSPECTIVAL GAP:
 *   Mobile citizens and supranational institutions experience this as a beneficial Rope, enabling opportunity and authority. Local labor markets and national border authorities experience it as a Snare, extracting control and economic stability. The engine's per-seat classification will reflect this divergence based on the declared roles and exit options.
 *
 * DIRECTIONALITY LOGIC:
 *   Mobile citizens are beneficiaries (d=0.0-0.2) due to enhanced opportunities. Supranational institutions are agenda-setters and beneficiaries (d=0.1-0.3) as they gain authority. Local labor markets and national border authorities are victims/payers (d=0.7-0.9) as they bear the costs of labor displacement and loss of control. National governments are also payers (d=0.6-0.8) due to constrained policy options. Sovereignty advocates are excluded (d=1.0) as their position is structurally marginalized.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate (fostering integration and peace) is still live, but its implementation (free movement as an irreversible right) has generated new problems and asymmetric costs. The classification as Tangled Rope prevents mislabeling it as a pure Rope (ignoring extraction) or a pure Snare (ignoring coordination). The ongoing contestation over its founding problem status (live vs. contested) highlights the tension between its original goals and its current extractive effects.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    irreversibility_vs_reversibility,
    'Is federation membership, and specifically the right to free movement, truly irreversible as a constitutional right, or is it a conditional treaty subject to renegotiation or withdrawal?',
    'A member state successfully withdrawing from the federation and re-establishing full border controls without legal or economic collapse, or a supranational court ruling explicitly affirming the conditional nature of membership.',
    'If reversible, the constraint''s suppression and extractiveness would be lower, as national actors would have a viable exit option, potentially reclassifying it towards a Rope or even a Scaffold. If truly irreversible, the current high suppression and extractiveness are justified by the structural reality.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(irreversibility_vs_reversibility, conceptual, 'Ambiguity over the fundamental nature of federation membership: irreversible integration vs. conditional treaty.').

omega_variable(
    labor_market_impact_quantification,
    'What is the precise economic impact of free movement on local labor markets in terms of wage depression, unemployment, and strain on public services, net of any benefits from increased labor supply?',
    'Comprehensive, independent econometric studies across multiple member states, disaggregated by sector and region, with transparent data and methodology.',
    'Clear evidence of significant net negative impact would strengthen the ''Snare'' aspect of the Tangled Rope classification for local labor markets and pressure for compensatory mechanisms or policy adjustments. Evidence of net positive or neutral impact would weaken the extraction claim.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(labor_market_impact_quantification, empirical, 'Quantification of the economic costs borne by local labor markets due to free movement.').

omega_variable(
    supranational_legitimacy_source,
    'Is the legitimacy of supranational authority primarily derived from the consent of member states (bottom-up) or from its own constitutional framework and direct citizen allegiance (top-down)?',
    'Analysis of public opinion data on allegiance, voting patterns in supranational elections, and the outcomes of constitutional crises where national and supranational law conflict.',
    'If legitimacy is primarily bottom-up, the ''sovereignty_reading'' gains strength, and the integrationist reading''s claim to irreversible authority is weakened. If top-down, the integrationist reading''s claims are reinforced, justifying higher suppression of national prerogatives.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(supranational_legitimacy_source, conceptual, 'The foundational source of legitimacy for supranational institutions.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(federation_membership__integration_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fede_tr_t0, federation_membership__integration_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(fede_tr_t10, federation_membership__integration_reading, theater_ratio, 10, 0.12).
narrative_ontology:measurement(fede_tr_t20, federation_membership__integration_reading, theater_ratio, 20, 0.15).
narrative_ontology:measurement(fede_tr_t30, federation_membership__integration_reading, theater_ratio, 30, 0.16).
narrative_ontology:measurement(fede_tr_t40, federation_membership__integration_reading, theater_ratio, 40, 0.15).
narrative_ontology:measurement(fede_tr_t50, federation_membership__integration_reading, theater_ratio, 50, 0.15).

% Extraction over time
narrative_ontology:measurement(fede_be_t0, federation_membership__integration_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(fede_be_t10, federation_membership__integration_reading, base_extractiveness, 10, 0.6).
narrative_ontology:measurement(fede_be_t20, federation_membership__integration_reading, base_extractiveness, 20, 0.65).
narrative_ontology:measurement(fede_be_t30, federation_membership__integration_reading, base_extractiveness, 30, 0.68).
narrative_ontology:measurement(fede_be_t40, federation_membership__integration_reading, base_extractiveness, 40, 0.69).
narrative_ontology:measurement(fede_be_t50, federation_membership__integration_reading, base_extractiveness, 50, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(fede_su_t0, federation_membership__integration_reading, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(fede_su_t10, federation_membership__integration_reading, suppression_requirement, 10, 0.65).
narrative_ontology:measurement(fede_su_t20, federation_membership__integration_reading, suppression_requirement, 20, 0.7).
narrative_ontology:measurement(fede_su_t30, federation_membership__integration_reading, suppression_requirement, 30, 0.73).
narrative_ontology:measurement(fede_su_t40, federation_membership__integration_reading, suppression_requirement, 40, 0.75).
narrative_ontology:measurement(fede_su_t50, federation_membership__integration_reading, suppression_requirement, 50, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(federation_membership__integration_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(federation_membership__integration_reading, national_border_control_policies).
narrative_ontology:affects_constraint(federation_membership__integration_reading, labor_market_regulation).

% DUAL FORMULATION NOTE:
% This constraint is the 'integration_reading' of the 'federation_membership' kernel. It is structurally distinct from the 'sovereignty_reading', which emphasizes national control and conditional membership. Both readings are linked as part of the same kernel contest.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
