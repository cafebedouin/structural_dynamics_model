% ============================================================================
% CONSTRAINT STORY: nuclear_impossibility_kernel__credibility_paradox_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_nuclear_impossibility_kernel__credibility_paradox_reading, []).

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
 *   constraint_id: nuclear_impossibility_kernel__credibility_paradox_reading
 *   human_readable: The Credibility Paradox: Deterrence Requires an Incredible Threat
 *   domain: strategic_studies/international_relations/nuclear_deterrence
 *
 * SUMMARY:
 *   This story instantiates the credibility_paradox_reading of the
 *   nuclear_impossibility_kernel: deterrence requires that a nuclear-armed
 *   state credibly threaten a response that, if actually carried out,
 *   guarantees its own destruction — a logical structure that makes the
 *   threat inherently doubted. Rather than treating this as either a settled
 *   physical impossibility (structural_contraction_reading) or a
 *   rational-choice cost calculation (rational_dropout_reading), this reading
 *   holds that the paradox is unstable and generative: because pure MAD
 *   deterrence is judged incredible by rational actors, nuclear states have
 *   spent eight decades building 'usable' nuclear options — flexible
 *   response, counterforce targeting, low-yield weapons, tailored deterrence,
 *   escalation ladders — precisely to restore the credibility that pure
 *   mutual destruction cannot supply. On this reading, 'unthinkability' is a
 *   rhetorical achievement maintained by doctrine communities, not a
 *   structural fact about the world; war remains reachable via graduated
 *   escalation, which is exactly what the modernization programs are built to
 *   make plausible.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(nuclear_impossibility_kernel__credibility_paradox_reading, 0.61).
domain_priors:suppression_score(nuclear_impossibility_kernel__credibility_paradox_reading, 0.72).
domain_priors:theater_ratio(nuclear_impossibility_kernel__credibility_paradox_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(nuclear_impossibility_kernel__credibility_paradox_reading, extractiveness, 0.61).
narrative_ontology:constraint_metric(nuclear_impossibility_kernel__credibility_paradox_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(nuclear_impossibility_kernel__credibility_paradox_reading, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(nuclear_impossibility_kernel__credibility_paradox_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(nuclear_impossibility_kernel__credibility_paradox_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(nuclear_impossibility_kernel__credibility_paradox_reading, tangled_rope).
narrative_ontology:human_readable(nuclear_impossibility_kernel__credibility_paradox_reading, "The Credibility Paradox: Deterrence Requires an Incredible Threat").
narrative_ontology:topic_domain(nuclear_impossibility_kernel__credibility_paradox_reading, "strategic_studies/international_relations/nuclear_deterrence").

domain_priors:requires_active_enforcement(nuclear_impossibility_kernel__credibility_paradox_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(nuclear_impossibility_kernel__credibility_paradox_reading, '2e751fe6-c3b0-4e2e-993d-55217d205de6').
narrative_ontology:cs_kernel_codification('2e751fe6-c3b0-4e2e-993d-55217d205de6', distributed).
narrative_ontology:cs_authority_grounding('2e751fe6-c3b0-4e2e-993d-55217d205de6', distributed).
narrative_ontology:cs_reading_relation('2e751fe6-c3b0-4e2e-993d-55217d205de6', nuclear_impossibility_kernel__structural_contraction_reading, coexists_with).
narrative_ontology:cs_reading_relation('2e751fe6-c3b0-4e2e-993d-55217d205de6', nuclear_impossibility_kernel__rational_dropout_reading, influences).
narrative_ontology:cs_axiom('2e751fe6-c3b0-4e2e-993d-55217d205de6', foundational, credibility_is_the_binding_constraint).
narrative_ontology:cs_axiom_status(credibility_is_the_binding_constraint, holdable).
narrative_ontology:cs_axiom_grounding('2e751fe6-c3b0-4e2e-993d-55217d205de6', credibility_is_the_binding_constraint, empirically_contingent).
narrative_ontology:cs_axiom('2e751fe6-c3b0-4e2e-993d-55217d205de6', foundational, unthinkability_is_performed_not_structural).
narrative_ontology:cs_axiom_status(unthinkability_is_performed_not_structural, holdable).
narrative_ontology:cs_axiom_grounding('2e751fe6-c3b0-4e2e-993d-55217d205de6', unthinkability_is_performed_not_structural, conventional).
narrative_ontology:cs_reference_frame('2e751fe6-c3b0-4e2e-993d-55217d205de6', assured_destruction_stability_doctrine).
narrative_ontology:cs_drift_state('2e751fe6-c3b0-4e2e-993d-55217d205de6', post_flexible_response_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('2e751fe6-c3b0-4e2e-993d-55217d205de6', '').
narrative_ontology:cs_kernel_id(nuclear_impossibility_kernel__credibility_paradox_reading, nuclear_impossibility_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(nuclear_impossibility_kernel__credibility_paradox_reading, nuclear_weapons_states_strategic_establishments).
narrative_ontology:constraint_beneficiary(nuclear_impossibility_kernel__credibility_paradox_reading, defense_contractors_and_modernization_industrial_base).
narrative_ontology:constraint_beneficiary(nuclear_impossibility_kernel__credibility_paradox_reading, extended_deterrence_alliance_leaderships).
narrative_ontology:constraint_victim(nuclear_impossibility_kernel__credibility_paradox_reading, allied_populations_under_extended_deterrence).
narrative_ontology:constraint_victim(nuclear_impossibility_kernel__credibility_paradox_reading, non_nuclear_states_facing_coercive_diplomacy).
narrative_ontology:constraint_victim(nuclear_impossibility_kernel__credibility_paradox_reading, domestic_taxpayers_funding_escalation_ladders).
narrative_ontology:constraint_victim(nuclear_impossibility_kernel__credibility_paradox_reading, future_generations_bearing_accident_risk).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(nuclear_impossibility_kernel__credibility_paradox_reading, extended_deterrence_alliance_leaderships).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Designs declaratory policy, force posture, and escalation doctrine around the credibility problem. Builds counterforce, limited-strike, and tailored-deterrence options specifically because pure mutual-suicide deterrence is judged too incredible to hold. Captures the strategic prestige, alliance leverage, and bureaucratic budget that flow from maintaining a 'usable' nuclear posture; can revise doctrine unilaterally with no external veto.
narrative_ontology:constraint_stakeholder(nuclear_impossibility_kernel__credibility_paradox_reading, nuclear_weapons_states_strategic_establishments, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(nuclear_impossibility_kernel__credibility_paradox_reading, nuclear_weapons_states_strategic_establishments, beneficiary).

% Supplies the low-yield warheads, precision delivery systems, and command-and-control upgrades that the credibility paradox generates demand for — each new 'usability' gap becomes a procurement line. Faces no meaningful risk from escalation and can shift between conventional and nuclear-adjacent contracts freely.
narrative_ontology:constraint_stakeholder(nuclear_impossibility_kernel__credibility_paradox_reading, defense_contractors_and_modernization_industrial_base, beneficiary,
    organized, biographical, mobile, national).

% Governments sheltering under another state's nuclear umbrella gain security guarantees without building their own arsenals, but must continuously reassure domestic audiences that the guarantor's incredible threat is actually credible — funding basing rights, joint exercises, and political capital to paper over the paradox. Cannot easily exit the alliance without triggering proliferation pressure at home.
narrative_ontology:constraint_stakeholder(nuclear_impossibility_kernel__credibility_paradox_reading, extended_deterrence_alliance_leaderships, beneficiary,
    institutional, generational, constrained, continental).
narrative_ontology:stakeholder_secondary_role(nuclear_impossibility_kernel__credibility_paradox_reading, extended_deterrence_alliance_leaderships, payer).

% Live within the target radius of an ally's nuclear guarantee and bear the accident, basing, and escalation risk generated by maintaining a credible-enough posture. Have essentially no voice in doctrine debates conducted by the guarantor state and cannot relocate outside the alliance's strategic geography without abandoning the security benefit entirely.
narrative_ontology:constraint_stakeholder(nuclear_impossibility_kernel__credibility_paradox_reading, allied_populations_under_extended_deterrence, payer,
    moderate, biographical, trapped, regional).

% Experience the paradox as pressure rather than abstraction: nuclear-armed states leverage the credibility gap to justify limited-war options and coercive signaling against them, precisely because 'unthinkable' full exchange is not the operative threat — usable escalation options are. Cannot acquire equivalent deterrent capability without triggering sanctions or preemption.
narrative_ontology:constraint_stakeholder(nuclear_impossibility_kernel__credibility_paradox_reading, non_nuclear_states_facing_coercive_diplomacy, payer,
    powerless, biographical, trapped, regional).

% Fund the continuous modernization of low-yield and precision-strike nuclear options that exist specifically to solve the incredibility problem, without a corresponding public debate about whether the underlying paradox is being managed or entrenched.
narrative_ontology:constraint_stakeholder(nuclear_impossibility_kernel__credibility_paradox_reading, domestic_taxpayers_funding_escalation_ladders, payer,
    powerless, generational, trapped, national).

% Inherit whatever residual probability of accidental or miscalculated nuclear use the credibility-restoring escalation ladders create; have no representation in present doctrine choices and cannot retroactively object to risk accumulated on their behalf.
narrative_ontology:constraint_stakeholder(nuclear_impossibility_kernel__credibility_paradox_reading, future_generations_bearing_accident_risk, payer,
    powerless, civilizational, trapped, global).

% Argue that resolving the credibility paradox by building 'usable' nuclear options makes war more likely rather than less, and that the paradox should instead be resolved by disarmament or minimal deterrence. Testify before legislatures and publish analysis but are structurally outside the closed doctrine-setting process of the nuclear establishments.
narrative_ontology:constraint_stakeholder(nuclear_impossibility_kernel__credibility_paradox_reading, arms_control_and_disarmament_advocates, excluded,
    organized, generational, constrained, global).

% Study the credibility paradox as a structural feature of deterrence theory, tracing how each generation of strategists has proposed technical or doctrinal fixes (flexible response, counterforce, tailored deterrence) to a problem some argue cannot be fixed, only managed or denied.
narrative_ontology:constraint_stakeholder(nuclear_impossibility_kernel__credibility_paradox_reading, strategic_studies_analysts, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared strategic framework that lets nuclear-armed states and their allies coordinate expectations about what will and will not trigger catastrophic retaliation, in principle stabilizing crisis behavior by making the costs of full-scale war common knowledge.
% TRANSFER_FUNCTION: Moves security assurance from nuclear guarantor states to allied populations in form, while moving real risk (accident, entrapment, escalation exposure) and real resources (modernization budgets, basing costs) from allied and domestic populations toward the strategic establishments and industrial base that manage the credibility problem.
% ABSENT_VOICES: Arms control advocates and disarmament researchers who argue the paradox should be resolved by de-escalating force postures, not by building more 'usable' options, are outside the closed doctrinal process; populations under extended deterrence and non-nuclear states subject to coercive signaling have essentially no seat in the states' internal doctrine debates.
% DISAPPEARANCE_RATIONALE: If the credibility paradox were somehow dissolved (e.g., through verified universal disarmament), the entire architecture of extended deterrence, alliance basing, and escalation-ladder procurement would need to reorganize — a major rearrangement for beneficiary institutions. But whether the underlying paradox is real (a structural feature of MAD) or largely rhetorical (a doctrine-community construct sustaining budgets and prestige) is exactly the contested question this reading exists to isolate; different parties would predict opposite consequences from its disappearance.
% FOUNDING_PROBLEM: Nuclear weapons made all-out war between major powers self-defeating for both sides, but a threat that guarantees the threatener's own destruction if carried out is not obviously credible — states needed some way to make deterrence believable despite this logical gap.
% FOUNDING_PROBLEM_CORROBORATION: Strategic studies analysts and several retired military planners outside current procurement chains attest that the credibility gap remains genuinely unresolved and that successive doctrinal fixes (flexible response, counterforce, tailored options) have not closed it, only shifted its location. Arms control researchers, an external and non-beneficiary community, corroborate that 'usability' programs are better explained by bureaucratic and industrial momentum than by a solved credibility problem. The strategic establishments themselves assert the problem is being actively and adequately managed — but that assertion comes from the benefiting parties and is not independently corroborated.
narrative_ontology:disappearance_verdict(nuclear_impossibility_kernel__credibility_paradox_reading, contested).
narrative_ontology:founding_problem_status(nuclear_impossibility_kernel__credibility_paradox_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(nuclear_impossibility_kernel__credibility_paradox_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(nuclear_impossibility_kernel__credibility_paradox_reading, 'none', 1).
narrative_ontology:epsilon_provenance(nuclear_impossibility_kernel__credibility_paradox_reading, 0.61, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(nuclear_impossibility_kernel__credibility_paradox_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(nuclear_impossibility_kernel__credibility_paradox_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(nuclear_impossibility_kernel__credibility_paradox_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.61) and suppression (0.72) are both substantial because this reading treats the paradox's 'resolution' industry — counterforce doctrine, low-yield modernization, alliance reassurance signaling — as an active, enforced, resource-consuming apparatus rather than a settled physical fact. Theater ratio (0.58) is high and rising because a large share of doctrinal and diplomatic activity (declaratory policy statements, extended-deterrence reassurance visits, flexible-response exercises) functions to perform credibility rather than to change the underlying physics, which this reading holds is unchanged. Accessibility_collapse is moderate (0.4), not high, because on this reading alternatives (disarmament, minimal deterrence, no-first-use) remain conceptually and politically available — they have been suppressed by doctrine-community consensus and alliance politics, not foreclosed by the physics. Resistance (0.62) reflects sustained arms-control and disarmament pushback across the entire interval.
 *
 * DIRECTIONALITY LOGIC:
 *   Strategic establishments and the defense-industrial base sit at the beneficiary end: the credibility problem itself is what justifies their continuous doctrinal relevance and procurement demand — a solved paradox would eliminate much of their rationale. Allied populations, non-nuclear states, and taxpayers sit at the target end: they fund and bear the risk of the escalation-restoring apparatus without controlling its design. Extended-deterrence alliance leaderships are dual-positioned (beneficiary of the security guarantee, payer of the political and resource cost of sustaining it) — the override is not needed here because the structural derivation captures this correctly from the beneficiary+payer secondary_role declaration.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (making deterrence credible despite MAD's logical structure) is authored as contested rather than resolved: strategic establishments assert active, adequate management; external analysts and arms-control researchers argue the problem persists unsolved and the apparatus has become partly self-perpetuating. Classifying this as tangled_rope rather than snare or mountain preserves the genuine coordination function (shared expectations that stabilize some crisis behavior) while registering that the same structure enforces asymmetric costs on populations who never consented to the doctrine debates that shape their risk exposure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    credibility_gap_real_or_rhetorical,
    'Is the credibility gap in nuclear deterrence a genuine, irreducible feature of MAD logic, or is it a rhetorical construct sustained by doctrine communities to justify continued relevance and procurement?',
    'Historical analysis of crisis behavior (Cuban Missile Crisis, Able Archer, India-Pakistan crises) for evidence of whether decision-makers actually treated the threat as incredible in practice, cross-referenced against declassified doctrine debates showing whether ''usability'' programs were driven by genuine strategic need or bureaucratic/industrial momentum.',
    'If the gap is genuinely irreducible, the apparatus built to manage it has real coordination value despite its costs (supporting tangled_rope). If it is substantially rhetorical, the constraint shifts toward snare — a manufactured problem sustaining an extractive apparatus.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(credibility_gap_real_or_rhetorical, conceptual, 'Whether the core credibility paradox is a real strategic problem or an institutionally convenient framing.').

omega_variable(
    kernel_reading_selection_ambiguity,
    'Which of the three sibling readings of the nuclear_impossibility_kernel (credibility_paradox, structural_contraction, rational_dropout) best describes the actual strategic history of the nuclear age, and could a single framework hold more than one simultaneously?',
    'Comparative analysis of declared doctrine across nuclear states over time: consistent pursuit of counterforce/limited-war capability supports this reading; consistent doctrinal acceptance of assured destruction as sufficient and stable supports structural_contraction; explicit rational-choice cost-benefit framing in strategic literature supports rational_dropout.',
    'Different readings imply different classifications and different policy prescriptions — if structural_contraction is correct, the modernization apparatus this reading treats as extractive theater is instead unnecessary or symbolic; if rational_dropout is correct, the relevant lever is cost-benefit calculation rather than doctrine-community credibility management.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_selection_ambiguity, conceptual, 'Which kernel reading the historical record actually supports, and whether the readings are mutually exclusive in practice.').

omega_variable(
    escalation_ladder_effectiveness,
    'Do graduated escalation options (low-yield weapons, counterforce targeting) actually restore credibility and stabilize deterrence, or do they lower the threshold for nuclear use and increase overall risk?',
    'Wargaming studies, historical near-miss analysis, and expert elicitation among strategists outside the programs'' own sponsoring institutions.',
    'If escalation ladders genuinely stabilize deterrence, the coordination function is stronger than the extraction reading suggests. If they increase use-risk, this reading''s extraction and suppression scores are conservative and the constraint is closer to snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(escalation_ladder_effectiveness, empirical, 'Whether ''usable'' nuclear options increase or decrease actual use-risk.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(nuclear_impossibility_kernel__credibility_paradox_reading, 1945, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nucl_tr_t1945, nuclear_impossibility_kernel__credibility_paradox_reading, theater_ratio, 1945, 0.2).
narrative_ontology:measurement(nucl_tr_t1962, nuclear_impossibility_kernel__credibility_paradox_reading, theater_ratio, 1962, 0.35).
narrative_ontology:measurement(nucl_tr_t1983, nuclear_impossibility_kernel__credibility_paradox_reading, theater_ratio, 1983, 0.5).
narrative_ontology:measurement(nucl_tr_t1991, nuclear_impossibility_kernel__credibility_paradox_reading, theater_ratio, 1991, 0.4).
narrative_ontology:measurement(nucl_tr_t2010, nuclear_impossibility_kernel__credibility_paradox_reading, theater_ratio, 2010, 0.48).
narrative_ontology:measurement(nucl_tr_t2025, nuclear_impossibility_kernel__credibility_paradox_reading, theater_ratio, 2025, 0.58).

% Extraction over time
narrative_ontology:measurement(nucl_be_t1945, nuclear_impossibility_kernel__credibility_paradox_reading, base_extractiveness, 1945, 0.32).
narrative_ontology:measurement(nucl_be_t1962, nuclear_impossibility_kernel__credibility_paradox_reading, base_extractiveness, 1962, 0.48).
narrative_ontology:measurement(nucl_be_t1983, nuclear_impossibility_kernel__credibility_paradox_reading, base_extractiveness, 1983, 0.57).
narrative_ontology:measurement(nucl_be_t1991, nuclear_impossibility_kernel__credibility_paradox_reading, base_extractiveness, 1991, 0.45).
narrative_ontology:measurement(nucl_be_t2010, nuclear_impossibility_kernel__credibility_paradox_reading, base_extractiveness, 2010, 0.55).
narrative_ontology:measurement(nucl_be_t2025, nuclear_impossibility_kernel__credibility_paradox_reading, base_extractiveness, 2025, 0.61).

% Suppression requirement over time
narrative_ontology:measurement(nucl_su_t1945, nuclear_impossibility_kernel__credibility_paradox_reading, suppression_requirement, 1945, 0.4).
narrative_ontology:measurement(nucl_su_t1962, nuclear_impossibility_kernel__credibility_paradox_reading, suppression_requirement, 1962, 0.6).
narrative_ontology:measurement(nucl_su_t1983, nuclear_impossibility_kernel__credibility_paradox_reading, suppression_requirement, 1983, 0.68).
narrative_ontology:measurement(nucl_su_t1991, nuclear_impossibility_kernel__credibility_paradox_reading, suppression_requirement, 1991, 0.55).
narrative_ontology:measurement(nucl_su_t2010, nuclear_impossibility_kernel__credibility_paradox_reading, suppression_requirement, 2010, 0.62).
narrative_ontology:measurement(nucl_su_t2025, nuclear_impossibility_kernel__credibility_paradox_reading, suppression_requirement, 2025, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(nuclear_impossibility_kernel__credibility_paradox_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(nuclear_impossibility_kernel__credibility_paradox_reading, structural_contraction_reading).
narrative_ontology:affects_constraint(nuclear_impossibility_kernel__credibility_paradox_reading, rational_dropout_reading).

% DUAL FORMULATION NOTE:
% This story is one of three sibling readings of nuclear_impossibility_kernel, decomposed per the ε-invariance principle: the natural-language concept 'the nuclear deterrence paradox' conflates a physical-impossibility claim (structural_contraction_reading), a rational-choice cost claim (rational_dropout_reading), and this credibility-instability claim. Each carries its own ε, beneficiary/victim structure, and claimed type; they are linked here rather than merged because measuring the underlying phenomenon by different observables (physical outcome vs. rational calculus vs. doctrinal credibility) yields materially different extraction profiles.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
