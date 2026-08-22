% ============================================================================
% CONSTRAINT STORY: westphalian_sovereignty__conditional_sovereignty
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_westphalian_sovereignty__conditional_sovereignty, []).

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
    narrative_ontology:measurement_basis/2,
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
 *   constraint_id: westphalian_sovereignty__conditional_sovereignty
 *   human_readable: Conditional Sovereignty: Responsibility and Intervention Rights
 *   domain: international_law/political_philosophy
 *
 * SUMMARY:
 *   Conditional sovereignty is one reading of the contested Westphalian
 *   sovereignty kernel. It holds that sovereignty is not unconditional:
 *   states that systematically violate human rights forfeit their immunity
 *   from external intervention. This reading competes with absolute
 *   sovereignty (states retain unconditional authority over internal affairs)
 *   and graduated sovereignty (sovereignty exists on a spectrum by capacity
 *   and legitimacy). The conditional-sovereignty reading emerged from the
 *   post-Cold War interventions and crystallized in the Responsibility to
 *   Protect doctrine. It is CLAIMED here as a snare because it creates an
 *   extraction mechanism: intervention-advocating powers and the
 *   international human rights regime benefit from the authority this reading
 *   confers on them; sovereign states and their populations bear the
 *   suppression cost of monitoring, potential intervention, and loss of
 *   classical immunity. The extractiveness is moderate (0.38) because the
 *   constraint does coordinate genuine international response to atrocity,
 *   but the threshold-setting and enforcement remain captured by powerful
 *   states. Suppression is high (0.71) because violating-state status is
 *   actively imposed by the regime and backed by threat of military action.
 *
 * KEY AGENTS:
 *   - Intervention-advocating powers (Western democracies, international institutions) — set thresholds, benefit from legitimacy
 *   - Sovereign states under threshold — lose classical immunity, bear monitoring and intervention threat
 *   - Powerless populations in violating states — identity-locked in their state's violation status, face intervention externalities
 *   - International human rights regime — gatekeepers of violation certification
 *   - Non-intervention defenders — excluded from threshold-setting
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(westphalian_sovereignty__conditional_sovereignty, 0.38).
domain_priors:suppression_score(westphalian_sovereignty__conditional_sovereignty, 0.71).
domain_priors:theater_ratio(westphalian_sovereignty__conditional_sovereignty, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(westphalian_sovereignty__conditional_sovereignty, extractiveness, 0.38).
narrative_ontology:constraint_metric(westphalian_sovereignty__conditional_sovereignty, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(westphalian_sovereignty__conditional_sovereignty, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(westphalian_sovereignty__conditional_sovereignty, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(westphalian_sovereignty__conditional_sovereignty, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(westphalian_sovereignty__conditional_sovereignty, snare).
narrative_ontology:human_readable(westphalian_sovereignty__conditional_sovereignty, "Conditional Sovereignty: Responsibility and Intervention Rights").
narrative_ontology:topic_domain(westphalian_sovereignty__conditional_sovereignty, "international_law/political_philosophy").

domain_priors:requires_active_enforcement(westphalian_sovereignty__conditional_sovereignty).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(westphalian_sovereignty__conditional_sovereignty, '94c0efcf-2b24-42cb-ab45-9408058bb22e').
narrative_ontology:cs_kernel_codification('94c0efcf-2b24-42cb-ab45-9408058bb22e', formalized).
narrative_ontology:cs_authority_grounding('94c0efcf-2b24-42cb-ab45-9408058bb22e', extraction).
narrative_ontology:cs_interpretation_layer_present('94c0efcf-2b24-42cb-ab45-9408058bb22e').
narrative_ontology:cs_reading_relation('94c0efcf-2b24-42cb-ab45-9408058bb22e', westphalian_sovereignty__absolute_sovereignty, forecloses).
narrative_ontology:cs_reading_relation('94c0efcf-2b24-42cb-ab45-9408058bb22e', westphalian_sovereignty__graduated_sovereignty, influences).
narrative_ontology:cs_axiom('94c0efcf-2b24-42cb-ab45-9408058bb22e', foundational, sovereignty_conditional_on_human_rights_compliance).
narrative_ontology:cs_axiom_status(sovereignty_conditional_on_human_rights_compliance, holdable).
narrative_ontology:cs_axiom_grounding('94c0efcf-2b24-42cb-ab45-9408058bb22e', sovereignty_conditional_on_human_rights_compliance, deontological).
narrative_ontology:cs_axiom('94c0efcf-2b24-42cb-ab45-9408058bb22e', foundational, external_intervention_legitimate_under_responsibility_threshold).
narrative_ontology:cs_axiom_status(external_intervention_legitimate_under_responsibility_threshold, holdable).
narrative_ontology:cs_axiom_grounding('94c0efcf-2b24-42cb-ab45-9408058bb22e', external_intervention_legitimate_under_responsibility_threshold, empirically_contingent).
narrative_ontology:cs_reference_frame('94c0efcf-2b24-42cb-ab45-9408058bb22e', classical_westphalian_sovereignty).
narrative_ontology:cs_drift_state('94c0efcf-2b24-42cb-ab45-9408058bb22e', contemporary_post_cold_war_intervention_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('94c0efcf-2b24-42cb-ab45-9408058bb22e', '2026-06-12T14:32:00Z').
narrative_ontology:cs_kernel_id(westphalian_sovereignty__conditional_sovereignty, westphalian_sovereignty).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(westphalian_sovereignty__conditional_sovereignty, intervention_advocating_powers).
narrative_ontology:constraint_beneficiary(westphalian_sovereignty__conditional_sovereignty, international_human_rights_regime).
narrative_ontology:constraint_victim(westphalian_sovereignty__conditional_sovereignty, sovereign_states_under_threshold).
narrative_ontology:constraint_victim(westphalian_sovereignty__conditional_sovereignty, populations_designated_as_violators).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(westphalian_sovereignty__conditional_sovereignty, populations_designated_as_violators).
narrative_ontology:constraint_beneficiary(westphalian_sovereignty__conditional_sovereignty, victims_of_alleged_violations).
narrative_ontology:constraint_vindicates(westphalian_sovereignty__conditional_sovereignty, universal_human_rights_doctrine).
narrative_ontology:constraint_vindicates(westphalian_sovereignty__conditional_sovereignty, responsibility_to_protect_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Western democracies and international bodies that define and enforce intervention thresholds. They set the standard for what constitutes systematic violation, determine which states are 'under threshold,' and retain discretion over intervention timing and scope. They benefit from the legitimacy this reading confers on their external actions and the geopolitical leverage conditional sovereignty provides.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__conditional_sovereignty, intervention_advocating_powers, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(westphalian_sovereignty__conditional_sovereignty, intervention_advocating_powers, beneficiary).

% States accused of systematic violations lose the classical Westphalian shield against external interference. They must respond to international investigations, accept monitoring mechanisms, endure public censure, and face the threat of intervention or sanctions. Their exit is constrained by the claim's universal applicability and their dependence on international recognition.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__conditional_sovereignty, sovereign_states_under_threshold, payer,
    powerful, generational, trapped, national).

% Civilian populations within states deemed violators face the prospect of external military intervention justified by this reading. They may escape specific violations through intervention but also face the trauma and destruction of war, displacement, and post-intervention chaos. Their identity is fused with their state's violation status.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__conditional_sovereignty, populations_designated_as_violators, payer,
    powerless, biographical, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(westphalian_sovereignty__conditional_sovereignty, populations_designated_as_violators, beneficiary).

% UN bodies, treaty bodies, and NGOs gain enforcement authority and legitimacy under this reading. Their investigations and determinations carry weight; their certification of violation status triggers legal and political consequences. They function as the gatekeepers of intervention legitimacy.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__conditional_sovereignty, international_human_rights_regime, beneficiary,
    institutional, generational, analytical, global).

% States that prioritize non-intervention doctrine and regional powers that resist external hierarchy are structurally excluded from setting the violation threshold. They would argue that conditional sovereignty is a mask for hegemonic intervention but lack the institutional power to set the definition of responsibility.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__conditional_sovereignty, non_intervention_defenders, excluded,
    institutional, generational, constrained, global).

% Groups suffering violations within violating states may receive external support, sanctuary, or military intervention in their favor. This reading provides them with an external recourse when domestic justice fails. However, their protection is contingent on international recognition of violation status and the willingness of external powers to act.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__conditional_sovereignty, victims_of_alleged_violations, beneficiary,
    powerless, biographical, trapped, national).

% International law scholars, philosophers, and analysts assess whether intervention is justified under this reading. They document violations, evaluate threshold-crossing claims, and analyze the legitimacy of external action. They hold no enforcement power but shape the interpretive frame.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__conditional_sovereignty, observer_scholars, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(westphalian_sovereignty__conditional_sovereignty, intervention_advocating_powers).
narrative_ontology:fixing_cost_class(westphalian_sovereignty__conditional_sovereignty, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Creates a framework for distinguishing legitimate state authority from illegitimate domination: states that respect human rights retain sovereignty; states that systematically violate lose their classical immunity from intervention. Solves the coordination problem of how international society responds to internal atrocities without abandoning the state system.
% TRANSFER_FUNCTION: Transfers authority over the definition of systematic violation from individual sovereigns to an international human rights regime; transfers legitimacy for external action from the classical balance-of-power to the conditional-responsibility framework; transfers protection and enforcement authority to intervening powers certified as acting in the name of human rights.
% ABSENT_VOICES: States that reject the universality of Western human rights norms are structurally excluded from determining what counts as a violation. Regional powers that oppose hierarchical intervention and post-colonial states that experienced intervention as colonialism cannot set the thresholds. Sovereignty-defending theorists have no institutional voice in the regime that operationalizes this reading.
% DISAPPEARANCE_RATIONALE: If conditional sovereignty evaporated, the classical Westphalian system (absolute immunity from intervention except via Security Council consent) would re-stabilize. Weak states would regain shielding; human rights advocacy would lose its intervention-triggering power; external actors would need explicit authorization rather than responsibility-to-protect certification. The entire post-1990s intervention architecture—humanitarian intervention, R2P doctrine, targeted sanctions, isolation—would lose its legitimizing foundation.
% FOUNDING_PROBLEM: The Cold War ended; the international community faced systematic atrocities in Bosnia, Rwanda, and later Syria—situations where sovereignty shielded perpetrators and non-intervention left victims unprotected. The founding problem: how to reconcile respect for state sovereignty with the moral imperative to prevent mass atrocity.
% FOUNDING_PROBLEM_CORROBORATION: Intervention advocates and human rights organizations attest the founding problem is live and this reading is the solution. Non-intervention defenders and post-colonial states attest the founding problem was defined in ways that privilege external powers and that the cure (conditional sovereignty) enables new forms of hegemonic intervention. Scholars and UN bodies outside the advocacy camp document the reading's use in geopolitically selective interventions (Libya, not Syria; Kosovo, not Tibet), undermining claims of universal application. Independent analyses show intervention thresholds are set by power, not principle.
narrative_ontology:disappearance_verdict(westphalian_sovereignty__conditional_sovereignty, world_rearranges).
narrative_ontology:founding_problem_status(westphalian_sovereignty__conditional_sovereignty, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(westphalian_sovereignty__conditional_sovereignty, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(westphalian_sovereignty__conditional_sovereignty, 'none', 1).
narrative_ontology:epsilon_provenance(westphalian_sovereignty__conditional_sovereignty, 0.38, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(westphalian_sovereignty__conditional_sovereignty_tests).
:- end_tests(westphalian_sovereignty__conditional_sovereignty_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises from 0.15 (1990, pre-R2P) to 0.41 (2011, post-Libya interventions) then plateaus at 0.38 (2018–2026), indicating that the constraint stabilized after initial expansion. The plateau reflects backlash from non-intervention advocates and rising awareness of selective application. Suppression requirement rises steeply from 0.35 to 0.74, tracking the maturation of monitoring mechanisms, sanctions regimes, and the credible threat of intervention. Theater ratio rises from 0.12 to 0.44, indicating that performative concern for human rights (public statements, UN resolutions, NGO campaigns) increasingly dominates actual intervention. The 2011 Libya intervention marked peak enforcement (theater + suppression both high), followed by Syrian conflict (2012+) where the reading's selective application became undeniable, causing theater to stabilize without rising further. The measurements are authored on a single shared time grid (every metric at every point) so temporal analysis has a coherent baseline.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter seat (intervention-advocating powers), the reading solves a real coordination problem: how to respond to atrocity while respecting sovereignty. From the payer seat (sovereign states under threat), the same reading operates as hegemonic domination disguised as universal principle. The engine computes these divergent classifications from the structural data: the agenda-setter holds institutional power and arbitrage exit (can intervene or not), driving low d; the payer holds institutional power but trapped exit (cannot escape the regime without losing legitimacy), driving high d. Same constraint, different directionalities, different computed types.
 *
 * DIRECTIONALITY LOGIC:
 *   Intervention-advocating powers: institutional power, arbitrage exit (can choose when/where to invoke the reading), beneficiary role → d near 0.2–0.3. Sovereign states under threshold: institutional power but trapped exit (subject to monitoring by the regime, cannot exit without renouncing sovereignty), payer role → d near 0.7–0.8. Powerless populations: powerless, identity-locked exit (fused with their state's status), caught between payer (threat of intervention) and beneficiary (potential protection) → d near 0.6. The override-free derivation chain should produce these d values from the authored beneficiary/victim declarations and exit-options data.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (how to respond to atrocity without violating sovereignty) was live in 1990–2005. By 2018–2026, the founding problem status is contested: advocates argue it remains live and conditional sovereignty is necessary; critics argue the problem was solved (international response exists, even imperfectly) and the arrangement now persists as rent-seeking by intervention-advocating powers. The theater-ratio plateau at 0.42 indicates the reading survives via performative maintenance: interventions are launched under R2P framing even when motivation is geopolitical; UN bodies invoke the threshold even when enforcement is selective. This is mandatrophy terrain: the constraint's original coordination function (respond to atrocity) is contested or fulfilled, but the extraction mechanism (authority to certify violation status, legitimacy to intervene, control over sovereignty conditionality) persists through theatrical restatement.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    violation_threshold_ambiguity,
    'What constitutes systematic violation sufficient to trigger intervention rights? Who determines this threshold?',
    'Empirical analysis of actual intervention decisions: do intervening powers apply consistent criteria across similar violation profiles, or does the threshold shift based on geopolitical interest?',
    'If thresholds are consistent and principle-driven, conditional sovereignty is a genuine gate. If thresholds are set by power and applied selectively, the constraint is pure snare—extraction masked as responsibility. This is the central empirical test.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(violation_threshold_ambiguity, empirical, 'Whether violation thresholds are principle-consistent or power-determined.').

omega_variable(
    intervention_selectivity,
    'Why were some systematic violations (Libya 2011) met with NATO intervention while others (Syria 2011+, Myanmar 2021+) were not?',
    'Document the stated reasons for intervening in some cases and not others; compare violation severity across cases; analyze whether geopolitical interest (oil, regional hegemony, great-power stakes) correlates with intervention decisions better than violation severity.',
    'High correlation between geopolitical interest and intervention (rather than violation severity) would indicate the reading is a cover story for power projection, not principle. This would classify the constraint as snare rather than tangled rope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(intervention_selectivity, empirical, 'Whether intervention is principle-driven or interest-driven.').

omega_variable(
    reading_vs_sibling_boundary,
    'Is conditional sovereignty logically foreclosed by absolute sovereignty, or do these readings coexist as genuinely live options?',
    'Assess whether major state actors (India, Russia, Brazil, African Union members) could coherently hold both the conditional-responsibility framing and absolute-sovereignty framing simultaneously, or whether they must choose one.',
    'If coexistence is possible (parties can hold conditional in some contexts, absolute in others), the readings coexist_with each other. If one framework truly rules out the other, the foreclosure relation obtains. Current evidence suggests coexistence: powerful states invoke absolute sovereignty to shield allies and conditional to critique rivals.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_vs_sibling_boundary, conceptual, 'Whether conditional and absolute sovereignty readings are mutually exclusive or simultaneously holdable.').

omega_variable(
    populations_identity_lock,
    'For populations in states designated as violators, is their identity-locked exit status structural or internalized?',
    'Post-intervention case analysis: do populations formerly locked into violator-state identity adopt new identities after intervention/regime change, or does the identity persist (as displaced persons, war trauma, etc.)?',
    'If identity-lock is structural (external designation only), suppression measurements underestimate the constraint''s impact. If internalized, the population carries the suppression with them after exit. Both readings are plausible; the true mechanism determines whether exit is actually more open than the structural data suggests.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(populations_identity_lock, empirical, 'Whether populations'' violation-status identity-lock is structural or internalized.').

omega_variable(
    responsibility_doctrine_contingency,
    'Is the Responsibility to Protect doctrine a permanent principle or a contingent reading that emerged from 1990s post-Cold War alignment?',
    'Historical analysis: track the doctrine''s adoption, implementation, and recent backlash (Russia, China, India rejecting selective application). Assess whether R2P is now entrenched or fragile.',
    'If contingent/fragile, conditional sovereignty may not persist as the reading of the Westphalian kernel; absolute or graduated readings could re-dominate. If entrenched, conditional sovereignty is the stable reading. Current evidence: R2P is under sustained challenge and may not survive great-power realignment.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(responsibility_doctrine_contingency, conceptual, 'Whether conditional sovereignty is a stable reading of the Westphalian kernel or a contingent post-Cold War coalition artifact.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(westphalian_sovereignty__conditional_sovereignty, 1990, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(west_tr_t1990, westphalian_sovereignty__conditional_sovereignty, theater_ratio, 1990, 0.12).
narrative_ontology:measurement_basis(west_tr_t1990, observed).
narrative_ontology:measurement(west_tr_t1999, westphalian_sovereignty__conditional_sovereignty, theater_ratio, 1999, 0.25).
narrative_ontology:measurement_basis(west_tr_t1999, observed).
narrative_ontology:measurement(west_tr_t2005, westphalian_sovereignty__conditional_sovereignty, theater_ratio, 2005, 0.35).
narrative_ontology:measurement_basis(west_tr_t2005, observed).
narrative_ontology:measurement(west_tr_t2011, westphalian_sovereignty__conditional_sovereignty, theater_ratio, 2011, 0.44).
narrative_ontology:measurement_basis(west_tr_t2011, observed).
narrative_ontology:measurement(west_tr_t2018, westphalian_sovereignty__conditional_sovereignty, theater_ratio, 2018, 0.42).
narrative_ontology:measurement_basis(west_tr_t2018, observed).
narrative_ontology:measurement(west_tr_t2026, westphalian_sovereignty__conditional_sovereignty, theater_ratio, 2026, 0.42).
narrative_ontology:measurement_basis(west_tr_t2026, observed).

% Extraction over time
narrative_ontology:measurement(west_be_t1990, westphalian_sovereignty__conditional_sovereignty, base_extractiveness, 1990, 0.15).
narrative_ontology:measurement_basis(west_be_t1990, observed).
narrative_ontology:measurement(west_be_t1999, westphalian_sovereignty__conditional_sovereignty, base_extractiveness, 1999, 0.28).
narrative_ontology:measurement_basis(west_be_t1999, observed).
narrative_ontology:measurement(west_be_t2005, westphalian_sovereignty__conditional_sovereignty, base_extractiveness, 2005, 0.35).
narrative_ontology:measurement_basis(west_be_t2005, observed).
narrative_ontology:measurement(west_be_t2011, westphalian_sovereignty__conditional_sovereignty, base_extractiveness, 2011, 0.41).
narrative_ontology:measurement_basis(west_be_t2011, observed).
narrative_ontology:measurement(west_be_t2018, westphalian_sovereignty__conditional_sovereignty, base_extractiveness, 2018, 0.38).
narrative_ontology:measurement_basis(west_be_t2018, observed).
narrative_ontology:measurement(west_be_t2026, westphalian_sovereignty__conditional_sovereignty, base_extractiveness, 2026, 0.38).
narrative_ontology:measurement_basis(west_be_t2026, observed).

% Suppression requirement over time
narrative_ontology:measurement(west_su_t1990, westphalian_sovereignty__conditional_sovereignty, suppression_requirement, 1990, 0.35).
narrative_ontology:measurement_basis(west_su_t1990, observed).
narrative_ontology:measurement(west_su_t1999, westphalian_sovereignty__conditional_sovereignty, suppression_requirement, 1999, 0.58).
narrative_ontology:measurement_basis(west_su_t1999, observed).
narrative_ontology:measurement(west_su_t2005, westphalian_sovereignty__conditional_sovereignty, suppression_requirement, 2005, 0.68).
narrative_ontology:measurement_basis(west_su_t2005, observed).
narrative_ontology:measurement(west_su_t2011, westphalian_sovereignty__conditional_sovereignty, suppression_requirement, 2011, 0.74).
narrative_ontology:measurement_basis(west_su_t2011, observed).
narrative_ontology:measurement(west_su_t2018, westphalian_sovereignty__conditional_sovereignty, suppression_requirement, 2018, 0.71).
narrative_ontology:measurement_basis(west_su_t2018, observed).
narrative_ontology:measurement(west_su_t2026, westphalian_sovereignty__conditional_sovereignty, suppression_requirement, 2026, 0.71).
narrative_ontology:measurement_basis(west_su_t2026, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(westphalian_sovereignty__conditional_sovereignty, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(westphalian_sovereignty__conditional_sovereignty, 0.12).
narrative_ontology:affects_constraint(westphalian_sovereignty__conditional_sovereignty, westphalian_sovereignty__absolute_sovereignty).
narrative_ontology:affects_constraint(westphalian_sovereignty__conditional_sovereignty, westphalian_sovereignty__graduated_sovereignty).
narrative_ontology:affects_constraint(westphalian_sovereignty__conditional_sovereignty, responsibility_to_protect_doctrine).
narrative_ontology:affects_constraint(westphalian_sovereignty__conditional_sovereignty, humanitarian_intervention_legitimacy).
narrative_ontology:affects_constraint(westphalian_sovereignty__conditional_sovereignty, international_sanctions_regime).

% DUAL FORMULATION NOTE:
% Conditional sovereignty is one of three structurally distinct readings of the Westphalian sovereignty kernel. The absolute-sovereignty reading (immunity from intervention) and graduated-sovereignty reading (spectrum-based) produce different beneficiary/victim structures and different ε values. All three readings share the same contested kernel but emit different constraints with different extracted. See westphalian_sovereignty__absolute_sovereignty and westphalian_sovereignty__graduated_sovereignty for the sibling readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(westphalian_sovereignty__conditional_sovereignty, institutional, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
