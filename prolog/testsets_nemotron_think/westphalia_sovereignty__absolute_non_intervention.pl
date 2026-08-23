% ============================================================================
% CONSTRAINT STORY: westphalia_sovereignty__absolute_non_intervention
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_westphalia_sovereignty__absolute_non_intervention, []).

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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: westphalia_sovereignty__absolute_non_intervention
 *   human_readable: Westphalian Sovereignty: Absolute Non-Intervention
 *   domain: international_law/political_theory/state_systems
 *
 * SUMMARY:
 *   The absolute non-intervention reading of Westphalian sovereignty holds
 *   that external interference in domestic affairs is categorically
 *   illegitimate regardless of internal conduct — a mountain claim presenting
 *   territorial inviolability as a natural law of the state system. The
 *   constraint operates through UN Charter Articles 2(4) and 2(7), UNSC veto
 *   power, and diplomatic enforcement of sovereign immunity. Beneficiaries
 *   are state elites (especially authoritarian regimes) who claim territorial
 *   monopoly and gain immunity from external accountability. Victims are
 *   populations under authoritarian control and atrocity victims denied
 *   external protection. The reading presents itself as pure coordination
 *   (preventing interstate war) but carries substantial extraction
 *   (authoritarian immunity at population expense). The ε-invariance
 *   principle requires this reading to be a separate constraint story from
 *   conditional_responsibility and graded_sovereignty siblings, each with its
 *   own ε, stakeholders, and classification.
 *
 * KEY AGENTS:
 *   - state_elites_claiming_territorial_monopoly: Primary beneficiary (institutional/arbitrage) — collects immunity from accountability
 *   - authoritarian_regimes: Primary beneficiary (institutional/arbitrage) — gains absolute domestic control
 *   - populations_under_authoritarian_control: Primary victim (powerless/trapped) — bears extraction via denied protection
 *   - atrocity_victims_denied_protection: Primary victim (powerless/trapped) — bears extraction via denied intervention
 *   - democratic_states_upholding_norm: Secondary agenda_setter/beneficiary (institutional/constrained) — benefits from stability but constrained by norm
 *   - un_security_council_permanent_members: Primary agenda_setter (institutional/arbitrage) — enforces norm via veto power
 *   - r2p_advocates: Excluded (organized/constrained) — would object but structurally excluded by categorical logic
 *   - international_legal_scholars: Observer (analytical/analytical) — analyzes full structure
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(westphalia_sovereignty__absolute_non_intervention, 0.65).
domain_priors:suppression_score(westphalia_sovereignty__absolute_non_intervention, 0.8).
domain_priors:theater_ratio(westphalia_sovereignty__absolute_non_intervention, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(westphalia_sovereignty__absolute_non_intervention, extractiveness, 0.65).
narrative_ontology:constraint_metric(westphalia_sovereignty__absolute_non_intervention, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(westphalia_sovereignty__absolute_non_intervention, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(westphalia_sovereignty__absolute_non_intervention, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(westphalia_sovereignty__absolute_non_intervention, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(westphalia_sovereignty__absolute_non_intervention, mountain).
narrative_ontology:human_readable(westphalia_sovereignty__absolute_non_intervention, "Westphalian Sovereignty: Absolute Non-Intervention").
narrative_ontology:topic_domain(westphalia_sovereignty__absolute_non_intervention, "international_law/political_theory/state_systems").

domain_priors:requires_active_enforcement(westphalia_sovereignty__absolute_non_intervention).
domain_priors:emerges_naturally(westphalia_sovereignty__absolute_non_intervention).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(westphalia_sovereignty__absolute_non_intervention, '97231d4c-db6e-4cbe-a9d0-bc51f62e2d38').
narrative_ontology:cs_kernel_codification('97231d4c-db6e-4cbe-a9d0-bc51f62e2d38', formalized).
narrative_ontology:cs_authority_grounding('97231d4c-db6e-4cbe-a9d0-bc51f62e2d38', lineage).
narrative_ontology:cs_interpretation_layer_present('97231d4c-db6e-4cbe-a9d0-bc51f62e2d38').
narrative_ontology:cs_reading_relation('97231d4c-db6e-4cbe-a9d0-bc51f62e2d38', westphalia_sovereignty__conditional_responsibility, forecloses).
narrative_ontology:cs_reading_relation('97231d4c-db6e-4cbe-a9d0-bc51f62e2d38', westphalia_sovereignty__graded_sovereignty, forecloses).
narrative_ontology:cs_axiom('97231d4c-db6e-4cbe-a9d0-bc51f62e2d38', foundational, territorial_inviolability_categorical).
narrative_ontology:cs_axiom_status(territorial_inviolability_categorical, holdable).
narrative_ontology:cs_axiom_grounding('97231d4c-db6e-4cbe-a9d0-bc51f62e2d38', territorial_inviolability_categorical, conventional).
narrative_ontology:cs_axiom('97231d4c-db6e-4cbe-a9d0-bc51f62e2d38', foundational, non_intervention_per_se_legitimate).
narrative_ontology:cs_axiom_status(non_intervention_per_se_legitimate, holdable).
narrative_ontology:cs_axiom_grounding('97231d4c-db6e-4cbe-a9d0-bc51f62e2d38', non_intervention_per_se_legitimate, conventional).
narrative_ontology:cs_reference_frame('97231d4c-db6e-4cbe-a9d0-bc51f62e2d38', westphalian_sovereignty_framework).
narrative_ontology:cs_drift_state('97231d4c-db6e-4cbe-a9d0-bc51f62e2d38', contemporary_r2p_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('97231d4c-db6e-4cbe-a9d0-bc51f62e2d38', '').
narrative_ontology:cs_kernel_id(westphalia_sovereignty__absolute_non_intervention, westphalia_sovereignty).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(westphalia_sovereignty__absolute_non_intervention, state_elites_claiming_territorial_monopoly).
narrative_ontology:constraint_beneficiary(westphalia_sovereignty__absolute_non_intervention, authoritarian_regimes).
narrative_ontology:constraint_victim(westphalia_sovereignty__absolute_non_intervention, populations_under_authoritarian_control).
narrative_ontology:constraint_victim(westphalia_sovereignty__absolute_non_intervention, atrocity_victims_denied_protection).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(westphalia_sovereignty__absolute_non_intervention, democratic_states_upholding_norm).
narrative_ontology:constraint_vindicates(westphalia_sovereignty__absolute_non_intervention, westphalian_order_stability).
narrative_ontology:constraint_vindicates(westphalia_sovereignty__absolute_non_intervention, state_equality_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Claim and exercise territorial monopoly over defined populations and territory. Collect immunity from external accountability for internal conduct. Set the rules of recognition for the state system. Their exit is arbitrage-grade: they can move assets, change citizenship, or restructure the state while retaining the sovereignty asset. They benefit from the norm's categorical logic which treats any external scrutiny as illegitimate interference.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__absolute_non_intervention, state_elites_claiming_territorial_monopoly, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(westphalia_sovereignty__absolute_non_intervention, state_elites_claiming_territorial_monopoly, agenda_setter).

% Gain absolute domestic control shielded by the non-intervention norm. Use sovereignty as a license for internal repression without fear of external military or coercive response. Their exit options are arbitrage-grade at the regime level (they control the state apparatus) but the populations they govern are trapped. They actively defend the norm in UN forums and bilateral diplomacy.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__absolute_non_intervention, authoritarian_regimes, beneficiary,
    institutional, biographical, arbitrage, global).

% Bear the extraction of the norm: denied external protection, denied refugee access in many cases, subjected to repression that the norm renders 'internal affairs.' Their exit options are trapped — borders are closed by the same regimes the norm protects, international law offers no right of humanitarian entry, and asylum systems are increasingly restrictive. They have no voice in the UN system that enforces the norm.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__absolute_non_intervention, populations_under_authoritarian_control, payer,
    powerless, biographical, trapped, national).

% Face imminent mass killing, ethnic cleansing, or crimes against humanity while the absolute non-intervention norm blocks external military response. Their exit options are trapped at the moment of atrocity — no time for asylum, no responsibility-to-protect trigger under this reading. The norm's categorical logic makes their suffering a domestic matter regardless of scale. They are the most extreme expression of the constraint's victim set.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__absolute_non_intervention, atrocity_victims_denied_protection, payer,
    powerless, immediate, trapped, local).

% Uphold the norm because they benefit from the stable interstate order it provides (coordination function). They also bear costs: constrained foreign policy when atrocities occur, domestic political pressure to 'do something,' and reputational costs when they invoke sovereignty to avoid action. Their exit options are constrained — they could intervene unilaterally but face diplomatic isolation, legal liability, and norm erosion that threatens the order they value.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__absolute_non_intervention, democratic_states_upholding_norm, agenda_setter,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(westphalia_sovereignty__absolute_non_intervention, democratic_states_upholding_norm, beneficiary).

% Enforce the norm through veto power — any intervention resolution can be blocked by one P5 member. They are the active enforcement mechanism. They also benefit: the norm protects their own sovereignty and spheres of influence. Their exit is arbitrage-grade — they write the rules and can choose when to authorize exceptions (Libya 2011) or block them (Syria). They sit at the intersection of agenda-setting and benefit-collection.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__absolute_non_intervention, un_security_council_permanent_members, agenda_setter,
    institutional, generational, arbitrage, global).

% Advocate for the conditional_responsibility reading (R2P). They would object to absolute non-intervention but are structurally excluded by the norm's categorical logic — the norm defines their position as illegitimate interference. They operate through UNGA resolutions, NGO campaigns, and scholarly work, but cannot access the UNSC veto gate. Their exit is constrained: they work within the system to change it, but the system's core rule excludes their premise.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__absolute_non_intervention, r2p_advocates, excluded,
    organized, biographical, constrained, global).

% Analyze the full structure: the norm's history, its coordinate-extractive duality, the seat divergence, the kernel contest. They neither collect from nor pay into the constraint. Their analytical exit is complete — they can adopt any reading or none. They provide the external corroboration for the founding problem genealogy.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__absolute_non_intervention, international_legal_scholars, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(westphalia_sovereignty__absolute_non_intervention, state_elites_claiming_territorial_monopoly).
narrative_ontology:fixing_cost_class(westphalia_sovereignty__absolute_non_intervention, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a stable framework for interstate order by categorically prohibiting external interference in domestic jurisdiction, preventing endless intervention wars and establishing a baseline of mutual non-aggression among sovereigns.
% TRANSFER_FUNCTION: Transfers protection-from-intervention to state elites (especially authoritarian regimes) at the cost of denying external protection to populations suffering atrocities and repression. The transfer is immunity for elites, paid in vulnerability by populations.
% ABSENT_VOICES: Populations under authoritarian control, atrocity victims, would-be interveners motivated by humanitarian concern, and future generations who inherit a norm that immunizes internal repression — all structurally excluded by the norm's categorical logic which defines their concerns as 'interference.'
% DISAPPEARANCE_RATIONALE: If the absolute non-intervention norm vanished overnight, humanitarian intervention and R2P would become normalized practice; authoritarian regimes would lose categorical immunity; the UNSC veto would no longer block atrocity response as a matter of principle; the interstate order would shift from sovereignty-based to responsibility-based, with profound rearrangement of state behavior and international law.
% FOUNDING_PROBLEM: The Thirty Years' War and similar conflicts of religious/dynastic intervention devastated Europe (1618-1648); the Westphalian settlement built a system where sovereigns agreed not to interfere in each other's domestic affairs to end perpetual war among states.
% FOUNDING_PROBLEM_CORROBORATION: Historical scholarship on Westphalia (outside beneficiary set) confirms the interstate war problem was real and the settlement reduced it. The ICISS R2P commission report (2001), UN World Summit Outcome Document (2005), and subsequent UN Secretary-General reports (outside authoritarian beneficiary set) attest the problem has shifted: interstate war is rare but internal atrocity is prevalent, requiring a responsibility-based framework.
narrative_ontology:disappearance_verdict(westphalia_sovereignty__absolute_non_intervention, world_rearranges).
narrative_ontology:founding_problem_status(westphalia_sovereignty__absolute_non_intervention, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(westphalia_sovereignty__absolute_non_intervention, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(westphalia_sovereignty__absolute_non_intervention, 'none', 1).
narrative_ontology:epsilon_provenance(westphalia_sovereignty__absolute_non_intervention, 0.65, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(westphalia_sovereignty__absolute_non_intervention_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(westphalia_sovereignty__absolute_non_intervention, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(westphalia_sovereignty__absolute_non_intervention, ExtMetricName, E),
    domain_priors:suppression_score(westphalia_sovereignty__absolute_non_intervention, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(westphalia_sovereignty__absolute_non_intervention),
    narrative_ontology:constraint_metric(westphalia_sovereignty__absolute_non_intervention, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(westphalia_sovereignty__absolute_non_intervention, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(westphalia_sovereignty__absolute_non_intervention_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.65) is substantial because the norm transfers protection-from-intervention to state elites at the cost of populations denied external recourse — the transfer function is real and measurable in atrocity cases where intervention is blocked. Suppression (0.8) is high because the norm's persistence depends on active enforcement: UNSC vetoes block resolutions, diplomatic pressure isolates interveners, treaty law criminalizes unauthorized intervention. Theater ratio (0.4) is moderate: the interstate-order coordination function is genuine (the norm did reduce interstate war), but a growing share of enforcement activity defends authoritarian immunity rather than order. Accessibility collapse (0.7) is high because alternatives (humanitarian intervention, R2P) are largely collapsed by the categorical logic — the norm makes them conceptually illegitimate, not just difficult. Resistance (0.5) is moderate: R2P advocates and some states resist, but the norm's institutionalization in UN Charter and state practice makes resistance costly. The claimed_type is mountain (the reading's self-presentation) but metrics describe a constraint with beneficiaries, victims, active enforcement, and rising extraction — a false summit mountain candidate.
 *
 * PERSPECTIVAL GAP:
 *   From the state_elites/authoritarian_regimes seat (agenda_setter/beneficiary, institutional/arbitrage), the constraint appears as genuine coordination — they built the system, they maintain it, they benefit from its stability. From the populations/atrocity_victims seat (payer, powerless/trapped), the same structure operates as pure extraction — they bear the costs (denied protection) with zero exit options (trapped by borders the norm protects). From the democratic_states_upholding_norm seat (agenda_setter/beneficiary, institutional/constrained), the constraint is mixed: genuine coordination benefit (stability) but constrained by the norm's protection of actors they might otherwise pressure. The engine computes this seat divergence from the structural data; the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   State elites and authoritarian regimes are structural beneficiaries (collect immunity, control rules, arbitrage-grade exit via sovereignty itself — d near 0.0). Populations under authoritarian control and atrocity victims are structural targets (bear extraction, trapped exit, zero power — d near 1.0). Democratic states upholding the norm sit near symmetric (genuine coordination benefit, some cost in constrained foreign policy — d ~0.5). UNSC permanent members are agenda_setters with arbitrage exit (they write the rules) but also partial beneficiaries (veto power protects their own sovereignty) — derived d low, but override may be needed if they bear hidden costs. R2P advocates are excluded — their structural position is defined by exclusion from the conversation the constraint structures.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (interstate war prevention) is contested: historical scholarship confirms it was live in 1648 and substantially solved; R2P documents attest it has shifted to internal atrocity. The arrangement persists despite the shift — classic mandatrophy signature. The classification prevents mislabeling by exposing the beneficiary/victim asymmetry: if this were pure coordination (rope), victims would not exist; if pure extraction (snare), coordination function would be absent. The tangled reality (coordination for elites, extraction for populations) is exactly what the FSM detection captures — a mountain claim with declared beneficiaries triggers reclassification investigation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    mountain_vs_constructed_ambiguity,
    'Is the absolute non-intervention norm a genuine mountain (natural law of the state system) or a constructed constraint that benefits identifiable state elites?',
    'Historical analysis of whether the norm persists without active enforcement by beneficiaries; counterfactual modeling of state system without the norm; measurement of extraction concentration on beneficiary groups.',
    'If constructed, the constraint is a false summit mountain triggering FSM reclassification to tangled_rope; the categorical natural-law framing is exposed as cover for authoritarian immunity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mountain_vs_constructed_ambiguity, conceptual, 'Natural-law vs. constructed status of absolute non-intervention').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the high suppression of intervention structural (UNSC vetoes, treaty law, diplomatic pressure) or internalized (states self-censor intervention advocacy, NGOs adopt sovereignty language)?',
    'Post-intervention suppression trajectory: if suppression persists after UNSC veto threat is removed (e.g., unilateral intervention cases), reclassify as partially internalized.',
    'If internalized, effective suppression is higher than structural measure suggests — the norm is carried by advocates themselves, not just enforced by power.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression of intervention').

omega_variable(
    coordination_extraction_separability,
    'Is the interstate-order coordination function separable from the authoritarian-immunity extraction function, or are they structurally fused?',
    'Natural experiment from R2P era: if limited humanitarian intervention can occur without collapsing interstate order, functions are separable; if any breach triggers systemic instability, they are fused.',
    'If separable, the extraction component is a removable layer on a genuine coordination core; if fused, the mountain claim collapses entirely — the coordination story requires the extraction.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_extraction_separability, conceptual, 'Whether coordination and extraction are structurally separable in the Westphalian norm').

omega_variable(
    kernel_reading_committer_structure,
    'This constraint is the absolute_non_intervention reading of the westphalia_sovereignty kernel. How do the sibling readings (conditional_responsibility, graded_sovereignty) structurally differ, and where is the disagreement located?',
    'Map the structural delta: absolute reading forecloses intervention legitimacy; conditional reading makes it contingent on atrocity; graded reading makes it scalar. Disagreement is located on the axiom ''territorial_inviolability_categorical'' vs. ''protection_responsibility_conditional'' vs. ''sovereignty_scalar''.',
    'Clarifies that ε-invariance requires separate constraint stories for each reading — they have different victim sets, beneficiary sets, and extractiveness profiles.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_committer_structure, conceptual, 'Kernel-reading committer frame: absolute_non_intervention reading of westphalia_sovereignty kernel').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(westphalia_sovereignty__absolute_non_intervention, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(wsa_ni_tr_t0, westphalia_sovereignty__absolute_non_intervention, theater_ratio, 0, 0.15).
narrative_ontology:measurement(wsa_ni_tr_t20, westphalia_sovereignty__absolute_non_intervention, theater_ratio, 20, 0.2).
narrative_ontology:measurement(wsa_ni_tr_t40, westphalia_sovereignty__absolute_non_intervention, theater_ratio, 40, 0.25).
narrative_ontology:measurement(wsa_ni_tr_t60, westphalia_sovereignty__absolute_non_intervention, theater_ratio, 60, 0.3).
narrative_ontology:measurement(wsa_ni_tr_t80, westphalia_sovereignty__absolute_non_intervention, theater_ratio, 80, 0.35).
narrative_ontology:measurement(wsa_ni_tr_t100, westphalia_sovereignty__absolute_non_intervention, theater_ratio, 100, 0.4).

% Extraction over time
narrative_ontology:measurement(wsa_ni_be_t0, westphalia_sovereignty__absolute_non_intervention, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(wsa_ni_be_t20, westphalia_sovereignty__absolute_non_intervention, base_extractiveness, 20, 0.4).
narrative_ontology:measurement(wsa_ni_be_t40, westphalia_sovereignty__absolute_non_intervention, base_extractiveness, 40, 0.45).
narrative_ontology:measurement(wsa_ni_be_t60, westphalia_sovereignty__absolute_non_intervention, base_extractiveness, 60, 0.55).
narrative_ontology:measurement(wsa_ni_be_t80, westphalia_sovereignty__absolute_non_intervention, base_extractiveness, 80, 0.6).
narrative_ontology:measurement(wsa_ni_be_t100, westphalia_sovereignty__absolute_non_intervention, base_extractiveness, 100, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(wsa_ni_su_t0, westphalia_sovereignty__absolute_non_intervention, suppression_requirement, 0, 0.65).
narrative_ontology:measurement(wsa_ni_su_t20, westphalia_sovereignty__absolute_non_intervention, suppression_requirement, 20, 0.7).
narrative_ontology:measurement(wsa_ni_su_t40, westphalia_sovereignty__absolute_non_intervention, suppression_requirement, 40, 0.72).
narrative_ontology:measurement(wsa_ni_su_t60, westphalia_sovereignty__absolute_non_intervention, suppression_requirement, 60, 0.75).
narrative_ontology:measurement(wsa_ni_su_t80, westphalia_sovereignty__absolute_non_intervention, suppression_requirement, 80, 0.78).
narrative_ontology:measurement(wsa_ni_su_t100, westphalia_sovereignty__absolute_non_intervention, suppression_requirement, 100, 0.8).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(westphalia_sovereignty__absolute_non_intervention, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(westphalia_sovereignty__absolute_non_intervention, 0.12).
narrative_ontology:affects_constraint(westphalia_sovereignty__absolute_non_intervention, westphalia_sovereignty__conditional_responsibility).
narrative_ontology:affects_constraint(westphalia_sovereignty__absolute_non_intervention, westphalia_sovereignty__graded_sovereignty).

% DUAL FORMULATION NOTE:
% This constraint (absolute_non_intervention) and its siblings (conditional_responsibility, graded_sovereignty) form the westphalia_sovereignty constraint family. The absolute reading claims ε ≈ 0 (mountain); conditional reading claims ε moderate (tangled_rope — coordination of atrocity prevention with extraction from intervening states); graded reading claims ε variable by capacity tier. They share the kernel 'westphalian_sovereignty' but instantiate different constraints with different ε, stakeholders, and classifications. Network edges reflect upstream influence: absolute_non_intervention (original Westphalian settlement) structurally influences both downstream readings (which emerged as responses to its extraction profile).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(westphalia_sovereignty__absolute_non_intervention, institutional, 0.15).
constraint_indexing:directionality_override(westphalia_sovereignty__absolute_non_intervention, powerless, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
