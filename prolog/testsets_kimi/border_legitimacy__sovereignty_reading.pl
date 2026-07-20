% ============================================================================
% CONSTRAINT STORY: border_legitimacy__sovereignty_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
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
    narrative_ontology:constraint_vindicates/2,
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
 *   human_readable: Territorial Sovereignty Right to Exclude (Sovereignty Reading)
 *   domain: political/migration/law
 *
 * SUMMARY:
 *   This constraint instantiates the sovereignty_reading of the
 *   border_legitimacy kernel. It treats the authority to control borders as
 *   derived from territorial sovereigntyâa foundational norm of the
 *   Westphalian state system. Under this reading, states possess a
 *   legitimate, enforceable right to exclude non-members, and the modern
 *   international order rests upon this jurisdictional allocation. The
 *   constraint coordinates territorial membership but asymmetrically extracts
 *   mobility and life chances from excluded migrants. The authored metrics
 *   describe high extraction, rising suppression, and increasing theater; the
 *   divergence between the sovereignty reading's claim of legitimate order
 *   and the operational reality of exclusion is the signal the engine is
 *   designed to measure.
 *
 * KEY AGENTS:
 *   - state_border_apparatus (agenda_setter, institutional/constrained) â administers enforcement and sets admission criteria
 *   - citizens_of_territorial_state (beneficiary, organized/mobile) â collect membership and economic rents
 *   - excluded_migrants (payer, powerless/trapped) â bear costs of exclusion
 *   - international_human_rights_bodies (observer, institutional/analytical) â monitor without binding authority
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(border_legitimacy__sovereignty_reading, 0.78).
domain_priors:suppression_score(border_legitimacy__sovereignty_reading, 0.86).
domain_priors:theater_ratio(border_legitimacy__sovereignty_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(border_legitimacy__sovereignty_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(border_legitimacy__sovereignty_reading, suppression_requirement, 0.86).
narrative_ontology:constraint_metric(border_legitimacy__sovereignty_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(border_legitimacy__sovereignty_reading, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(border_legitimacy__sovereignty_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(border_legitimacy__sovereignty_reading, tangled_rope).
narrative_ontology:human_readable(border_legitimacy__sovereignty_reading, "Territorial Sovereignty Right to Exclude (Sovereignty Reading)").
narrative_ontology:topic_domain(border_legitimacy__sovereignty_reading, "political/migration/law").

domain_priors:requires_active_enforcement(border_legitimacy__sovereignty_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(border_legitimacy__sovereignty_reading, '3c5a1803-f758-4c84-b725-81f293ecfe27').
narrative_ontology:cs_kernel_codification('3c5a1803-f758-4c84-b725-81f293ecfe27', formalized).
narrative_ontology:cs_authority_grounding('3c5a1803-f758-4c84-b725-81f293ecfe27', lineage).
narrative_ontology:cs_interpretation_layer_present('3c5a1803-f758-4c84-b725-81f293ecfe27').
narrative_ontology:cs_reading_relation('3c5a1803-f758-4c84-b725-81f293ecfe27', border_legitimacy__freedom_of_movement_reading, forecloses).
narrative_ontology:cs_reading_relation('3c5a1803-f758-4c84-b725-81f293ecfe27', border_legitimacy__humanitarian_obligation_reading, influences).
narrative_ontology:cs_axiom('3c5a1803-f758-4c84-b725-81f293ecfe27', foundational, territorial_sovereignty_entails_exclusion_right).
narrative_ontology:cs_axiom_status(territorial_sovereignty_entails_exclusion_right, holdable).
narrative_ontology:cs_axiom_grounding('3c5a1803-f758-4c84-b725-81f293ecfe27', territorial_sovereignty_entails_exclusion_right, conventional).
narrative_ontology:cs_axiom('3c5a1803-f758-4c84-b725-81f293ecfe27', foundational, statehood_requires_boundary_control).
narrative_ontology:cs_axiom_status(statehood_requires_boundary_control, holdable).
narrative_ontology:cs_axiom_grounding('3c5a1803-f758-4c84-b725-81f293ecfe27', statehood_requires_boundary_control, conventional).
narrative_ontology:cs_reference_frame('3c5a1803-f758-4c84-b725-81f293ecfe27', westphalian_territorial_sovereignty).
narrative_ontology:cs_drift_state('3c5a1803-f758-4c84-b725-81f293ecfe27', contemporary_migration_crisis_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('3c5a1803-f758-4c84-b725-81f293ecfe27', '').
narrative_ontology:cs_kernel_id(border_legitimacy__sovereignty_reading, border_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(border_legitimacy__sovereignty_reading, citizens_of_territorial_state).
narrative_ontology:constraint_beneficiary(border_legitimacy__sovereignty_reading, state_border_apparatus).
narrative_ontology:constraint_victim(border_legitimacy__sovereignty_reading, excluded_migrants).
narrative_ontology:constraint_vindicates(border_legitimacy__sovereignty_reading, westphalian_sovereignty_doctrine).
narrative_ontology:constraint_vindicates(border_legitimacy__sovereignty_reading, territorial_jurisdiction_monopoly).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers visa regimes, physical border control, detention, and deportation under domestic law and international treaty. Derives budget, personnel, and legal authority from the doctrine of territorial sovereignty. Sets admission criteria and allocates enforcement resources across land, sea, and air entry points.
narrative_ontology:constraint_stakeholder(border_legitimacy__sovereignty_reading, state_border_apparatus, agenda_setter,
    institutional, generational, constrained, national).

% Receive preferential access to the labor market, public goods, political participation, and legal protections tied to membership. Their collective consentâexpressed through elections, public opinion, or tacit acceptanceâlegitimates enforcement. Individual exit via emigration is legally possible but socially and economically costly.
narrative_ontology:constraint_stakeholder(border_legitimacy__sovereignty_reading, citizens_of_territorial_state, beneficiary,
    organized, generational, mobile, national).

% Seek entry for work, family reunification, or safety. Bear the direct costs of exclusion: denied mobility, wages, and status; risk of detention, physical violence, drowning, or deportation during irregular crossing attempts. Lack legal standing in the enforcing state's policy process and face tightening externalized enforcement in transit states.
narrative_ontology:constraint_stakeholder(border_legitimacy__sovereignty_reading, excluded_migrants, payer,
    powerless, immediate, trapped, global).

% Monitor state compliance with refugee law and human rights instruments, document pushbacks and detention conditions, and issue non-binding recommendations. They observe the tension between sovereignty claims and rights obligations but possess no enforcement authority to alter the constraint's operation directly.
narrative_ontology:constraint_stakeholder(border_legitimacy__sovereignty_reading, international_human_rights_bodies, observer,
    institutional, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(border_legitimacy__sovereignty_reading, diffuse).
narrative_ontology:fixing_cost_class(border_legitimacy__sovereignty_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Allocates exclusive territorial jurisdiction to recognized states, solving the coordination problem of overlapping violence, conflicting legal authority, and resource jurisdiction within defined boundaries.
% TRANSFER_FUNCTION: Transfers mobility rights and territorial access from non-citizens to the state and its members, enforced through physical barriers, legal prohibitions, and detention.
% ABSENT_VOICES: Excluded migrants are physically and legally absent from the fora that set admission criteria; prospective future citizens and diaspora communities lack standing. Human rights bodies are present in discourse but structurally excluded from binding agenda-setting.
% DISAPPEARANCE_RATIONALE: The modern international state system is built upon territorial sovereignty and the right to exclude. If the constraint disappeared overnight, jurisdictional boundaries would lose their primary enforcement mechanism, triggering massive reorganization of population flows, labor markets, public finance, and the state-citizen relationship.
% FOUNDING_PROBLEM: The Thirty Years' War and preceding European conflicts demonstrated the violence of overlapping, non-exclusive territorial and religious authority. The Westphalian system sought to monopolize legitimate violence within territorial boundaries and establish mutually recognized, non-overlapping jurisdiction.
% FOUNDING_PROBLEM_CORROBORATION: Historians of international relations corroborate the war origins of the sovereignty system. However, the claim that contemporary border enforcement at current intensity continues to serve that original coordination problemârather than economic protectionism and demographic controlâis contested by migration scholars and human rights advocates. No party outside the community of benefiting states unambiguously attests that the founding problem remains live at modern enforcement scale.
narrative_ontology:disappearance_verdict(border_legitimacy__sovereignty_reading, world_rearranges).
narrative_ontology:founding_problem_status(border_legitimacy__sovereignty_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(border_legitimacy__sovereignty_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(border_legitimacy__sovereignty_reading, 'none', 1).
narrative_ontology:epsilon_provenance(border_legitimacy__sovereignty_reading, 0.78, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness is high (0.78) because the constraint systematically denies mobility, labor-market access, and family unity to excluded migrants, transferring those opportunities to citizens and state authority. Suppression is higher still (0.86) because the constraint persists only through active, escalating enforcementâphysical barriers, detention, externalization to transit statesâand through suppressing alternatives such as open-movement regimes. Theater_ratio (0.48) reflects the increasing performative dimension of border enforcement (spectacle of walls, ritualized control) relative to the genuine jurisdictional coordination function. Accessibility_collapse (0.75) captures that, within the sovereignty framework, open borders are treated as literally unthinkable as policy. Resistance (0.60) is substantial and growing, driven by migrant movements and rights advocacy, which in turn fuels the enforcement ratchet.
 *
 * PERSPECTIVAL GAP:
 *   From the state_border_apparatus seat, the constraint is legitimate coordination of territorial order: the foundational problem of overlapping violence remains live, and enforcement is necessary to maintain jurisdiction. From the excluded_migrants seat, the same structure is direct extraction of life chances, with the coordination story serving as legitimating cover. Citizens experience the constraint as protective infrastructure and often do not perceive the extraction. The engine computes this divergence from the structural asymmetry in power, exit options, and beneficiary-victim roles.
 *
 * DIRECTIONALITY LOGIC:
 *   State_border_apparatus and citizens_of_territorial_state are declared beneficiaries with constrained or mobile exit, placing their directionality near the beneficiary end (low d). Excluded_migrants are declared victims with trapped exit, placing their directionality near the target end (high d). International_human_rights_bodies are analytical observers with no material stake. The engine will compute effective extraction as amplified for excluded_migrants and damped or inverted for the beneficiary seats.
 *
 * MANDATROPHY ANALYSIS:
 *   Classifying as tangled_rope prevents mislabeling the constraint as pure extraction (snare) by requiring acknowledgement of the genuine coordination functionâterritorial jurisdiction and the monopoly of legitimate violenceâwhile also preventing mislabeling it as pure coordination (rope) by requiring the declared victim set (excluded_migrants) and active enforcement. The sovereignty reading's legitimacy claim is the coordinating cover; the metrics reveal the extraction. If the coordination function were entirely absent (borders served no jurisdictional purpose), the classification would shift toward snare; if extraction were negligible (as with internal open borders within a federation), it would shift toward rope.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sovereignty_absolutism_vs_qualified,
    'Is territorial sovereignty in contemporary international law an absolute norm or one qualified by human rights obligations?',
    'Systematic review of international court judgments, treaty body interpretations, and constitutional court rulings on the scope of the right to exclude.',
    'If sovereignty is legally qualified, the effective extraction of this constraint is lower than authored and the classification may shift toward rope; if absolute, the reading holds and extraction remains high.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sovereignty_absolutism_vs_qualified, conceptual, 'Whether sovereignty is absolute or qualified in modern law.').

omega_variable(
    enforcement_coordination_or_extraction,
    'Does current border enforcement resource allocation primarily serve genuine territorial coordination or extractive exclusion?',
    'Comparative audit of enforcement budgets, border technologies, and outcomes against objective security threats and jurisdictional needs.',
    'If enforcement tracks security needs, the coordination function dominates; if it tracks exclusion intensity regardless of threat, the extraction layer dominates and theater_ratio rises.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_coordination_or_extraction, empirical, 'Coordination versus extraction in enforcement practice.').

omega_variable(
    sovereignty_reading_kernel_position,
    'What structural changes would occur if this constraint were authored from the freedom_of_movement_reading of the same kernel rather than the sovereignty_reading?',
    'Construct the sibling constraint story and compare beneficiary-victim sets and directionalities; if the structure inverts, the kernel is genuinely contested.',
    'Confirms that Îµ-invariance requires separate stories; validates that the divergence between readings is structural, not observational.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sovereignty_reading_kernel_position, conceptual, 'Structural delta between sovereignty and free movement readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(border_legitimacy__sovereignty_reading, 0, 75).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bl_sr_tr_t0, border_legitimacy__sovereignty_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(bl_sr_tr_t12, border_legitimacy__sovereignty_reading, theater_ratio, 12, 0.22).
narrative_ontology:measurement(bl_sr_tr_t25, border_legitimacy__sovereignty_reading, theater_ratio, 25, 0.28).
narrative_ontology:measurement(bl_sr_tr_t37, border_legitimacy__sovereignty_reading, theater_ratio, 37, 0.33).
narrative_ontology:measurement(bl_sr_tr_t50, border_legitimacy__sovereignty_reading, theater_ratio, 50, 0.38).
narrative_ontology:measurement(bl_sr_tr_t62, border_legitimacy__sovereignty_reading, theater_ratio, 62, 0.43).
narrative_ontology:measurement(bl_sr_tr_t75, border_legitimacy__sovereignty_reading, theater_ratio, 75, 0.48).

% Extraction over time
narrative_ontology:measurement(bl_sr_be_t0, border_legitimacy__sovereignty_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(bl_sr_be_t12, border_legitimacy__sovereignty_reading, base_extractiveness, 12, 0.48).
narrative_ontology:measurement(bl_sr_be_t25, border_legitimacy__sovereignty_reading, base_extractiveness, 25, 0.55).
narrative_ontology:measurement(bl_sr_be_t37, border_legitimacy__sovereignty_reading, base_extractiveness, 37, 0.62).
narrative_ontology:measurement(bl_sr_be_t50, border_legitimacy__sovereignty_reading, base_extractiveness, 50, 0.68).
narrative_ontology:measurement(bl_sr_be_t62, border_legitimacy__sovereignty_reading, base_extractiveness, 62, 0.73).
narrative_ontology:measurement(bl_sr_be_t75, border_legitimacy__sovereignty_reading, base_extractiveness, 75, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(bl_sr_su_t0, border_legitimacy__sovereignty_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(bl_sr_su_t12, border_legitimacy__sovereignty_reading, suppression_requirement, 12, 0.52).
narrative_ontology:measurement(bl_sr_su_t25, border_legitimacy__sovereignty_reading, suppression_requirement, 25, 0.58).
narrative_ontology:measurement(bl_sr_su_t37, border_legitimacy__sovereignty_reading, suppression_requirement, 37, 0.65).
narrative_ontology:measurement(bl_sr_su_t50, border_legitimacy__sovereignty_reading, suppression_requirement, 50, 0.74).
narrative_ontology:measurement(bl_sr_su_t62, border_legitimacy__sovereignty_reading, suppression_requirement, 62, 0.8).
narrative_ontology:measurement(bl_sr_su_t75, border_legitimacy__sovereignty_reading, suppression_requirement, 75, 0.86).


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
