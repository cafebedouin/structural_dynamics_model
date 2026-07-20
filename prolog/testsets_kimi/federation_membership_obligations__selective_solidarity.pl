% ============================================================================
% CONSTRAINT STORY: federation_membership_obligations__selective_solidarity
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: federation_membership_obligations__selective_solidarity
 *   human_readable: Selective Solidarity: Contributory Free Movement Tiering
 *   domain: political_economy/federalism/migration_policy
 *
 * SUMMARY:
 *   Within the European Union and similar federal structures, free movement
 *   rights are not uniform but tiered according to economic contribution
 *   history and current activity status. Employed mobile workers retain full
 *   equal treatment, while jobseekers and economically inactive citizens face
 *   residence and welfare restrictions. This constraint is the
 *   selective_solidarity reading of the federation_membership_obligations
 *   kernel: it claims that solidarity and access must be earned through
 *   contribution, not granted by citizenship alone. It functions as a
 *   political compromise intended to preserve labor mobility by protecting
 *   host welfare systems, but structurally it bifurcates mobile citizens into
 *   full and partial members.
 *
 * KEY AGENTS:
 *   - Host member states (agenda_setter/beneficiary, institutional/constrained) â administer tiering and capture fiscal savings
 *   - Employed mobile workers (beneficiary, moderate/mobile) â retain full rights and mobility
 *   - Economically inactive mobile citizens (payer, powerless/constrained) â bear welfare exclusion and removal risk
 *   - Sending member states (excluded, institutional/constrained) â bear spillovers without decision control
 *   - Federation court (observer, institutional/analytical) â adjudicates and legitimizes the tiering
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(federation_membership_obligations__selective_solidarity, 0.62).
domain_priors:suppression_score(federation_membership_obligations__selective_solidarity, 0.58).
domain_priors:theater_ratio(federation_membership_obligations__selective_solidarity, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(federation_membership_obligations__selective_solidarity, extractiveness, 0.62).
narrative_ontology:constraint_metric(federation_membership_obligations__selective_solidarity, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(federation_membership_obligations__selective_solidarity, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(federation_membership_obligations__selective_solidarity, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(federation_membership_obligations__selective_solidarity, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(federation_membership_obligations__selective_solidarity, tangled_rope).
narrative_ontology:human_readable(federation_membership_obligations__selective_solidarity, "Selective Solidarity: Contributory Free Movement Tiering").
narrative_ontology:topic_domain(federation_membership_obligations__selective_solidarity, "political_economy/federalism/migration_policy").

domain_priors:requires_active_enforcement(federation_membership_obligations__selective_solidarity).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(federation_membership_obligations__selective_solidarity, '1206f845-07b4-48d5-8bc2-8c495b75ce6c').
narrative_ontology:cs_kernel_codification('1206f845-07b4-48d5-8bc2-8c495b75ce6c', formalized).
narrative_ontology:cs_authority_grounding('1206f845-07b4-48d5-8bc2-8c495b75ce6c', lineage).
narrative_ontology:cs_interpretation_layer_present('1206f845-07b4-48d5-8bc2-8c495b75ce6c').
narrative_ontology:cs_reading_relation('1206f845-07b4-48d5-8bc2-8c495b75ce6c', federation_membership_obligations__integration_primary, coexists_with).
narrative_ontology:cs_reading_relation('1206f845-07b4-48d5-8bc2-8c495b75ce6c', federation_membership_obligations__member_sovereignty_primary, coexists_with).
narrative_ontology:cs_axiom('1206f845-07b4-48d5-8bc2-8c495b75ce6c', foundational, solidarity_earned_through_contribution).
narrative_ontology:cs_axiom_status(solidarity_earned_through_contribution, holdable).
narrative_ontology:cs_axiom_grounding('1206f845-07b4-48d5-8bc2-8c495b75ce6c', solidarity_earned_through_contribution, conventional).
narrative_ontology:cs_axiom('1206f845-07b4-48d5-8bc2-8c495b75ce6c', foundational, economic_activity_defines_entitlement_tier).
narrative_ontology:cs_axiom_status(economic_activity_defines_entitlement_tier, holdable).
narrative_ontology:cs_axiom_grounding('1206f845-07b4-48d5-8bc2-8c495b75ce6c', economic_activity_defines_entitlement_tier, conventional).
narrative_ontology:cs_reference_frame('1206f845-07b4-48d5-8bc2-8c495b75ce6c', contributory_solidarity_framework).
narrative_ontology:cs_drift_state('1206f845-07b4-48d5-8bc2-8c495b75ce6c', post_crisis_enforcement_era, gap(revival_pressure, substantial, false)).
narrative_ontology:cs_created_at('1206f845-07b4-48d5-8bc2-8c495b75ce6c', '').
narrative_ontology:cs_kernel_id(federation_membership_obligations__selective_solidarity, federation_membership_obligations).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(federation_membership_obligations__selective_solidarity, host_member_states).
narrative_ontology:constraint_beneficiary(federation_membership_obligations__selective_solidarity, employed_mobile_workers).
narrative_ontology:constraint_victim(federation_membership_obligations__selective_solidarity, economically_inactive_mobile_citizens).
narrative_ontology:constraint_vindicates(federation_membership_obligations__selective_solidarity, contributory_welfare_principle).
narrative_ontology:constraint_vindicates(federation_membership_obligations__selective_solidarity, member_state_fiscal_sovereignty).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administer residence and social assistance programs for mobile citizens. Verify contribution history and current economic activity to restrict access to non-contributory benefits. Represent fiscal sovereignty interests in the Council of the EU and set enforcement practice through national welfare bureaucracies.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__selective_solidarity, host_member_states, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(federation_membership_obligations__selective_solidarity, host_member_states, beneficiary).

% Move across member states to take up employment. Retain full equal treatment in access to social advantages and residence rights based on worker status. Depend on the overall free movement framework for cross-border mobility but do not face the contribution-history barriers that restrict inactive citizens.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__selective_solidarity, employed_mobile_workers, beneficiary,
    moderate, biographical, mobile, continental).

% Have exercised free movement to reside in a host member state without current employment or sufficient past contributions. Face waiting periods, exclusions from non-contributory benefits, and potential removal for unreasonable burden on social assistance, despite holding formal citizenship of the federation.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__selective_solidarity, economically_inactive_mobile_citizens, payer,
    powerless, immediate, constrained, continental).

% Home countries of economically inactive mobile citizens. Bear fiscal and political spillovers when citizens are denied support in the host state and return or become stranded. Are present in EU forums but outvoted on host-state welfare closure decisions.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__selective_solidarity, sending_member_states, excluded,
    institutional, generational, constrained, national).

% Interprets the founding treaties and secondary legislation to adjudicate disputes over residence and welfare access. Has shifted jurisprudence toward accepting contributory and activity-based limitations, providing legal certainty for the tiering structure.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__selective_solidarity, federation_court, observer,
    institutional, generational, analytical, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(federation_membership_obligations__selective_solidarity, host_member_states).
narrative_ontology:fixing_cost_class(federation_membership_obligations__selective_solidarity, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Enables cross-border labor mobility by reducing host-state fiscal fears of welfare migration, preserving the single market's freedom of movement through a compromise that links entitlement to economic participation.
% TRANSFER_FUNCTION: Moves welfare cost risk from host member states to economically inactive mobile citizens and their home states, while preserving labor-mobility gains for employed workers.
% ABSENT_VOICES: Economically inactive mobile citizens are underrepresented in Council negotiations where host states set the policy frame; sending member states are present but outvoted on host-state welfare closure.
% DISAPPEARANCE_RATIONALE: Host states would face immediate fiscal and political pressure from unrestricted welfare claims, potentially triggering unilateral border controls or exits from free movement commitments; employed workers would lose predictable mobility rights as host states reassert closure.
% FOUNDING_PROBLEM: How to sustain politically viable cross-border labor mobility and market integration without forcing host welfare states to extend full non-contributory benefits to all mobile citizens, which could provoke anti-migration backlash and welfare state retrenchment.
% FOUNDING_PROBLEM_CORROBORATION: Host member states and fiscal conservatives attest the problem is live, citing budget sustainability and public opinion. Sending member states, mobile citizen advocates, and integrationist legal scholars attest the problem is overstated and the arrangement serves exclusionary closure. Independent migration economists and the European Commission provide mixed empirical corroboration outside the directly benefiting parties.
narrative_ontology:disappearance_verdict(federation_membership_obligations__selective_solidarity, world_rearranges).
narrative_ontology:founding_problem_status(federation_membership_obligations__selective_solidarity, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(federation_membership_obligations__selective_solidarity, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(federation_membership_obligations__selective_solidarity, 'none', 1).
narrative_ontology:epsilon_provenance(federation_membership_obligations__selective_solidarity, 0.62, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness is moderate-high (0.62) because a defined class of citizens loses welfare access they would hold under a citizenship-based principle. Suppression is moderate (0.58) because enforcement is bureaucratic-legal (residence checks, contribution verification) rather than coercive violence, but alternatives are structurally blocked. Theater ratio is moderate (0.42) because the public discourse of 'welfare tourism' and enforcement rituals often exceed measured fiscal abuse, creating performative gatekeeping. Accessibility collapse is moderate (0.60): citizenship-based alternatives are legally foreclosed but remain politically visible and contested. Resistance is moderate (0.50): affected citizens and sending states contest the rules in courts and councils, but host-state fiscal sovereignty claims dominate. The measurement series share one time grid to prevent misaligned substitution.
 *
 * PERSPECTIVAL GAP:
 *   From the host-state seat, the constraint is necessary coordination to prevent welfare-state destruction and anti-migration backlash. From the inactive-citizen seat, the same structure is extraction of citizenship-based entitlements. From the employed-worker seat, it is a beneficial equilibrium that preserves mobility. The engine computes this divergence from the structural data; the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Host member states sit at the beneficiary end: they set the rules and capture the fiscal transfer of avoided welfare expenditure. Employed mobile workers also sit toward the beneficiary end because they gain protected mobility from the compromise. Economically inactive mobile citizens are the clear targets: they bear the extraction of restricted rights despite formal citizenship, with constrained exit (return is possible but costly). Sending member states are excluded from the gain but bear externalities, sitting at a middling-high directionality as non-targeted excluded parties. The federation court is analytical.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope classification prevents mislabeling this as pure extraction (snare) because there is a genuine coordination function: cross-border labor mobility and single-market integration are real and valued. It prevents mislabeling it as pure coordination (rope) because the cost-bearing is asymmetrically loaded onto a identifiable victim class (economically inactive citizens) and requires active enforcement to hold. If the coordination function atrophied and only the fiscal gatekeeping remained, the constraint would drift toward snare or piton.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_location,
    'This constraint instantiates the selective_solidarity reading of the federation_membership_obligations kernel. Would adopting the integration_primary reading (citizenship-based welfare) or member_sovereignty_primary reading (national closure) produce a structurally different classification?',
    'Comparative analysis of the sibling constraint stories and cross-reading coupling measurement within the kernel family.',
    'If the selective_solidarity reading is unstable in practice, the constraint may be reclassified as a scaffold (transitional compromise) or snare (instrumentalized exclusion), depending on which sibling reading''s axioms dominate institutional behavior.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_location, conceptual, 'Committer-frame omega locating this constraint within the contested kernel and identifying structural delta from sibling readings.').

omega_variable(
    inactive_citizen_victim_status,
    'Are economically inactive mobile citizens genuine victims of extraction, or are they voluntarily opting into a system where they have not paid in?',
    'Empirical measurement of welfare access denial rates, poverty outcomes among mobile citizens, and comparison with static citizen access levels.',
    'If outcomes show severe deprivation, the extractiveness metric is validated; if outcomes show minimal impact because most are supported by family or return home, the extraction classification weakens toward rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(inactive_citizen_victim_status, empirical, 'Whether the restriction on inactive citizens produces material harm.').

omega_variable(
    enforcement_theater_vs_function,
    'Does the contribution-history verification serve a genuine coordination function (preventing benefit tourism and protecting fiscal trust) or primarily performative gatekeeping?',
    'Cross-national comparison of verification costs versus detected abuse rates; measurement of public trust in free movement before and after enforcement intensification.',
    'If abuse rates are negligible relative to enforcement cost, the theater_ratio rises and the coordination function is delegitimized; if abuse is material, the tangled_rope classification is reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_theater_vs_function, empirical, 'Whether enforcement tracks real coordination need or performative closure.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(federation_membership_obligations__selective_solidarity, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(federation_selective_solidarity_tr_t0, federation_membership_obligations__selective_solidarity, theater_ratio, 0, 0.2).
narrative_ontology:measurement(federation_selective_solidarity_tr_t5, federation_membership_obligations__selective_solidarity, theater_ratio, 5, 0.22).
narrative_ontology:measurement(federation_selective_solidarity_tr_t10, federation_membership_obligations__selective_solidarity, theater_ratio, 10, 0.28).
narrative_ontology:measurement(federation_selective_solidarity_tr_t15, federation_membership_obligations__selective_solidarity, theater_ratio, 15, 0.33).
narrative_ontology:measurement(federation_selective_solidarity_tr_t20, federation_membership_obligations__selective_solidarity, theater_ratio, 20, 0.38).
narrative_ontology:measurement(federation_selective_solidarity_tr_t25, federation_membership_obligations__selective_solidarity, theater_ratio, 25, 0.41).
narrative_ontology:measurement(federation_selective_solidarity_tr_t30, federation_membership_obligations__selective_solidarity, theater_ratio, 30, 0.42).

% Extraction over time
narrative_ontology:measurement(federation_selective_solidarity_be_t0, federation_membership_obligations__selective_solidarity, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(federation_selective_solidarity_be_t5, federation_membership_obligations__selective_solidarity, base_extractiveness, 5, 0.35).
narrative_ontology:measurement(federation_selective_solidarity_be_t10, federation_membership_obligations__selective_solidarity, base_extractiveness, 10, 0.4).
narrative_ontology:measurement(federation_selective_solidarity_be_t15, federation_membership_obligations__selective_solidarity, base_extractiveness, 15, 0.48).
narrative_ontology:measurement(federation_selective_solidarity_be_t20, federation_membership_obligations__selective_solidarity, base_extractiveness, 20, 0.55).
narrative_ontology:measurement(federation_selective_solidarity_be_t25, federation_membership_obligations__selective_solidarity, base_extractiveness, 25, 0.6).
narrative_ontology:measurement(federation_selective_solidarity_be_t30, federation_membership_obligations__selective_solidarity, base_extractiveness, 30, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(federation_selective_solidarity_su_t0, federation_membership_obligations__selective_solidarity, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(federation_selective_solidarity_su_t5, federation_membership_obligations__selective_solidarity, suppression_requirement, 5, 0.35).
narrative_ontology:measurement(federation_selective_solidarity_su_t10, federation_membership_obligations__selective_solidarity, suppression_requirement, 10, 0.42).
narrative_ontology:measurement(federation_selective_solidarity_su_t15, federation_membership_obligations__selective_solidarity, suppression_requirement, 15, 0.5).
narrative_ontology:measurement(federation_selective_solidarity_su_t20, federation_membership_obligations__selective_solidarity, suppression_requirement, 20, 0.58).
narrative_ontology:measurement(federation_selective_solidarity_su_t25, federation_membership_obligations__selective_solidarity, suppression_requirement, 25, 0.63).
narrative_ontology:measurement(federation_selective_solidarity_su_t30, federation_membership_obligations__selective_solidarity, suppression_requirement, 30, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(federation_membership_obligations__selective_solidarity, identity_coordination).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the federation_membership_obligations kernel, decomposed from the colloquial label 'EU free movement rights'. The integration_primary and member_sovereignty_primary readings are separate constraints with distinct epsilon values, beneficiary structures, and stakeholder configurations.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
