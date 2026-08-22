% ============================================================================
% CONSTRAINT STORY: federation_membership__integration_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
 *   constraint_id: federation_membership__integration_reading
 *   human_readable: Federation Membership as Irreversible Integration (Integration Reading)
 *   domain: political_economy/federalism/migration
 *
 * SUMMARY:
 *   This constraint story instantiates the integration reading of the
 *   federation-membership kernel: the claim that federation membership
 *   constitutes an irreversible integration project in which supranational
 *   authority is legitimate and free movement of workers is a constitutional
 *   right rather than a negotiated policy concession. Under this reading, the
 *   constraint arranges labor markets across member states by prohibiting
 *   local border restrictions, generating genuine economic coordination while
 *   imposing concentrated adjustment costs on stationary low-wage workers in
 *   receiving regions. The sibling sovereignty reading treats the same treaty
 *   architecture as a conditional intergovernmental bargain. This story
 *   authors high extractiveness because the integration reading
 *   constitutionalizes mobility in ways that override local labor-market
 *   shields, producing structural displacement that mobile citizens arbitrate
 *   away.
 *
 * KEY AGENTS:
 *   - mobile_citizens: Primary beneficiary (moderate/mobile) â gain cross-border labor access and wage arbitrage
 *   - local_low_wage_workers: Primary target (powerless/constrained) â bear wage suppression and displacement from labor inflow
 *   - supranational_commission: Agenda-setter (institutional/constrained) â enforces free-movement acquis and infringement proceedings
 *   - federalism_scholars: Analytical observer â tracks the tension between mobility rights and territorial solidarity
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(federation_membership__integration_reading, 0.72).
domain_priors:suppression_score(federation_membership__integration_reading, 0.68).
domain_priors:theater_ratio(federation_membership__integration_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(federation_membership__integration_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(federation_membership__integration_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(federation_membership__integration_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(federation_membership__integration_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(federation_membership__integration_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(federation_membership__integration_reading, tangled_rope).
narrative_ontology:human_readable(federation_membership__integration_reading, "Federation Membership as Irreversible Integration (Integration Reading)").
narrative_ontology:topic_domain(federation_membership__integration_reading, "political_economy/federalism/migration").

domain_priors:requires_active_enforcement(federation_membership__integration_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(federation_membership__integration_reading, 'e348964b-a462-4fab-99b1-1bb6ea9c6c29').
narrative_ontology:cs_kernel_codification('e348964b-a462-4fab-99b1-1bb6ea9c6c29', formalized).
narrative_ontology:cs_authority_grounding('e348964b-a462-4fab-99b1-1bb6ea9c6c29', lineage).
narrative_ontology:cs_interpretation_layer_present('e348964b-a462-4fab-99b1-1bb6ea9c6c29').
narrative_ontology:cs_reading_relation('e348964b-a462-4fab-99b1-1bb6ea9c6c29', federation_membership__sovereignty_reading, influences).
narrative_ontology:cs_axiom('e348964b-a462-4fab-99b1-1bb6ea9c6c29', foundational, membership_irreversible).
narrative_ontology:cs_axiom_status(membership_irreversible, holdable).
narrative_ontology:cs_axiom_grounding('e348964b-a462-4fab-99b1-1bb6ea9c6c29', membership_irreversible, conventional).
narrative_ontology:cs_axiom('e348964b-a462-4fab-99b1-1bb6ea9c6c29', foundational, free_movement_constitutional_right).
narrative_ontology:cs_axiom_status(free_movement_constitutional_right, holdable).
narrative_ontology:cs_axiom_grounding('e348964b-a462-4fab-99b1-1bb6ea9c6c29', free_movement_constitutional_right, conventional).
narrative_ontology:cs_reference_frame('e348964b-a462-4fab-99b1-1bb6ea9c6c29', supranational_constitutional_order).
narrative_ontology:cs_drift_state('e348964b-a462-4fab-99b1-1bb6ea9c6c29', contemporary_political_cycle, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('e348964b-a462-4fab-99b1-1bb6ea9c6c29', '').
narrative_ontology:cs_kernel_id(federation_membership__integration_reading, federation_membership).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(federation_membership__integration_reading, mobile_citizens).
narrative_ontology:constraint_victim(federation_membership__integration_reading, local_low_wage_workers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Exercise free movement rights to seek employment across federation member states, gaining access to broader labor markets and higher wages than available in their origin regions. Their mobility is protected as a constitutional right of membership.
narrative_ontology:constraint_stakeholder(federation_membership__integration_reading, mobile_citizens, beneficiary,
    moderate, biographical, mobile, continental).

% Face increased labor competition from mobile citizens willing to accept lower wages or worse conditions, experiencing wage suppression and reduced bargaining power in local labor markets where they are anchored by family, housing, and language barriers.
narrative_ontology:constraint_stakeholder(federation_membership__integration_reading, local_low_wage_workers, payer,
    powerless, immediate, constrained, local).

% Administers and enforces the free-movement acquis, bringing infringement actions against member states that attempt to restrict labor mobility or impose border controls, deriving its authority from the irreversible integration reading of federation membership.
narrative_ontology:constraint_stakeholder(federation_membership__integration_reading, supranational_commission, agenda_setter,
    institutional, generational, constrained, continental).

% Analyze the tension between mobility rights and local labor-market protection, documenting distributional effects across different territorial levels of the federation.
narrative_ontology:constraint_stakeholder(federation_membership__integration_reading, federalism_scholars, observer,
    analytical, civilizational, analytical, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(federation_membership__integration_reading, mobile_citizens).
narrative_ontology:fixing_cost_class(federation_membership__integration_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates labor allocation across a multinational federation by removing border barriers to employment, matching workers to vacancies without state-level clearance, and creating a unified economic space.
% TRANSFER_FUNCTION: Transfers labor-market opportunity and wage premiums from stationary low-wage workers in high-mobility regions to mobile citizens who can arbitrage geographic wage differentials, while transferring regulatory authority over border control from national to supranational level.
% ABSENT_VOICES: Local community organizers and municipal governments in receiving regions with acute housing and service strain are largely excluded from the supranational constitutional design; they would argue for local derogations but are not treaty parties. Also, prospective emigrants from outside the federation are structurally absent from the free-movement right.
% DISAPPEARANCE_RATIONALE: If the irreversible integration and free-movement right vanished overnight, labor mobility would revert to bilateral visa and work-permit regimes, wage differentials between member states would likely persist or widen without arbitrage, and the supranational authority would lose a core competence â the federation's economic constitution would reorganize around national labor-market sovereignty.
% FOUNDING_PROBLEM: Post-war devastation and recurrent nationalist conflict in Europe created a need to bind national economies together so tightly that war became materially impossible, while addressing labor shortages and economic imbalances through cross-border mobility rather than protectionism.
% FOUNDING_PROBLEM_CORROBORATION: Federalist historians and early community architects attest the founding problem from the beneficiary side. Skeptical national constitutional courts and left-populist labor movements attest from outside the benefiting parties that the original security rationale is largely historical and the arrangement now serves mobile capital and labor at the expense of anchored workers; no neutral party holds uncontested corroboration.
narrative_ontology:disappearance_verdict(federation_membership__integration_reading, world_rearranges).
narrative_ontology:founding_problem_status(federation_membership__integration_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(federation_membership__integration_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(federation_membership__integration_reading, 'none', 1).
narrative_ontology:epsilon_provenance(federation_membership__integration_reading, 0.72, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness (0.72) is high because the constitutionalization of free movement removes local democratic control over labor inflows, transferring adjustment costs to relatively immobile low-wage workers who cannot arbitrage geography. Suppression (0.68) reflects the active enforcement machinery: infringement actions, fines, and legal supremacy that prevent member states from restoring border-based labor protection. Theater ratio (0.25) is low-moderate: most enforcement activity is functional (genuine legal oversight), though some rhetoric frames economic integration as existential peace project, which is partially performative. Accessibility collapse (0.60) is substantial because once free movement is constitutionalized, local alternatives (quotas, local hiring preferences) are legally invalid. Resistance (0.55) is significant and rising, visible in national court challenges and populist anti-mobility politics.
 *
 * PERSPECTIVAL GAP:
 *   The mobile citizen seat computes the constraint as coordination (access to jobs, unified market), while the local low-wage worker seat computes it as extraction (wage competition without democratic exit). The supranational institutional seat sees constitutional fidelity; the same structure reads as democratic deficit from the local payer seat. The engine derives this divergence from identical structural data via directionality: beneficiaries with mobile exit options receive low d, while trapped local payers receive high d.
 *
 * DIRECTIONALITY LOGIC:
 *   Mobile citizens are declared beneficiaries (d near the beneficiary end): they are subsidized by the constraint's removal of borders. Local low-wage workers are declared victims/payers (d near the target end): they bear the constraint's competitive pressure. The supranational authority sits between: it does not collect rents personally but wields enforcement power with constrained exit. No override is needed because the structural derivation captures the relationship.
 *
 * MANDATROPHY ANALYSIS:
 *   The integration reading prevents mandatrophy mislabeling by preserving the genuine coordination function: the post-war founding problem (binding economies to prevent conflict, solving cross-border labor mismatches) was real and the unified market produces allocative efficiency. However, the founding problem status is contested because the security rationale has attenuated while the legal machinery persists. If the arrangement were pure extraction (snare), we would expect no coordination function and cover-story suppression; here the coordination is measurable in GDP and labor-matching gains. If it were pure coordination (rope), we would expect symmetric distribution of gains; the victim set and wage-suppression evidence block that classification. Tangled rope captures the hybrid: coordination plus asymmetric cost-shielding.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    integration_reading_naturalness,
    'Is federation membership genuinely irreversible integration or a conditional treaty reversible by withdrawal?',
    'Historical evidence of attempted exits and their legal framing; analysis of whether withdrawal clauses exist in the kernel text and how they are interpreted by the authority structure.',
    'If reversible, the integration reading''s claim of irreversibility is a constructed narrative rather than structural law, potentially shifting classification toward snare or increasing measured extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(integration_reading_naturalness, conceptual, 'Whether irreversibility is a legal fact or a political narrative.').

omega_variable(
    labor_displacement_causation,
    'Does free movement cause net welfare loss for local low-wage workers, or are welfare losses driven by other factors that the constraint merely overlays?',
    'Econometric decomposition of wage effects controlling for sectoral and technological shocks.',
    'If displacement is not causally attributable to mobility, the victim structure weakens and epsilon should be lower; if causal, the extraction reading is reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(labor_displacement_causation, empirical, 'Causal attribution of local wage suppression to cross-border mobility.').

omega_variable(
    kernel_reading_contest,
    'Does the integration reading''s axiom of irreversible integration foreclose the sovereignty reading, or do they coexist within the federation''s legal order?',
    'Analysis of constitutional court judgments that reconcile supranational primacy with national constitutional identity.',
    'If the legal system structurally accommodates both readings, the integration reading''s foreclosure claim is overstated, affecting the cs_structure relation classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Whether integration and sovereignty readings are mutually exclusive or coexistent.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(federation_membership__integration_reading, 0, 70).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(federation_membership_integration_tr_t0, federation_membership__integration_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(federation_membership_integration_tr_t15, federation_membership__integration_reading, theater_ratio, 15, 0.12).
narrative_ontology:measurement(federation_membership_integration_tr_t30, federation_membership__integration_reading, theater_ratio, 30, 0.15).
narrative_ontology:measurement(federation_membership_integration_tr_t45, federation_membership__integration_reading, theater_ratio, 45, 0.2).
narrative_ontology:measurement(federation_membership_integration_tr_t60, federation_membership__integration_reading, theater_ratio, 60, 0.24).
narrative_ontology:measurement(federation_membership_integration_tr_t70, federation_membership__integration_reading, theater_ratio, 70, 0.25).

% Extraction over time
narrative_ontology:measurement(federation_membership_integration_be_t0, federation_membership__integration_reading, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(federation_membership_integration_be_t15, federation_membership__integration_reading, base_extractiveness, 15, 0.3).
narrative_ontology:measurement(federation_membership_integration_be_t30, federation_membership__integration_reading, base_extractiveness, 30, 0.4).
narrative_ontology:measurement(federation_membership_integration_be_t45, federation_membership__integration_reading, base_extractiveness, 45, 0.55).
narrative_ontology:measurement(federation_membership_integration_be_t60, federation_membership__integration_reading, base_extractiveness, 60, 0.68).
narrative_ontology:measurement(federation_membership_integration_be_t70, federation_membership__integration_reading, base_extractiveness, 70, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(federation_membership_integration_su_t0, federation_membership__integration_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(federation_membership_integration_su_t15, federation_membership__integration_reading, suppression_requirement, 15, 0.4).
narrative_ontology:measurement(federation_membership_integration_su_t30, federation_membership__integration_reading, suppression_requirement, 30, 0.5).
narrative_ontology:measurement(federation_membership_integration_su_t45, federation_membership__integration_reading, suppression_requirement, 45, 0.6).
narrative_ontology:measurement(federation_membership_integration_su_t60, federation_membership__integration_reading, suppression_requirement, 60, 0.65).
narrative_ontology:measurement(federation_membership_integration_su_t70, federation_membership__integration_reading, suppression_requirement, 70, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
