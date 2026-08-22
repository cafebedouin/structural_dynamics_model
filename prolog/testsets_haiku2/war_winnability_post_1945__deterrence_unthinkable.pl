% ============================================================================
% CONSTRAINT STORY: war_winnability_post_1945__deterrence_unthinkable
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_war_winnability_post_1945__deterrence_unthinkable, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: war_winnability_post_1945__deterrence_unthinkable
 *   human_readable: Nuclear Deterrence: War Winnability Exits the Strategic Space (Unthinkable Reading)
 *   domain: strategic_studies/international_relations
 *
 * SUMMARY:
 *   This constraint story instantiates the deterrence_unthinkable reading of
 *   the contested kernel war_winnability_post_1945. Under this reading,
 *   nuclear weapons made great-power total war categorically unwinnable: any
 *   escalation between peer nuclear powers terminates in mutual annihilation,
 *   making victory operationally incoherent. Strategic planning shifts
 *   entirely to war prevention, not war preparation. Military establishments
 *   suffer extraction of their traditional institutional mandate (planning
 *   for victory); civilian populations benefit through the removal of
 *   great-power total war from the reachable strategy space. The constraint
 *   is enforced through declaratory policy insisting that nuclear war is
 *   unthinkable, supported by strategic doctrine and mutual vulnerability.
 *   This reading competes with countervailing_thinkable (which asserts
 *   limited victory remains possible) and rhetorical_contraction (which
 *   asserts winnability became unsayable while remaining operationally
 *   planned). The authored metrics show high suppression (0.71 at interval
 *   end) because deterrence doctrine must actively suppress and delegitimize
 *   any planning for nuclear war victory; theater increases over time (0.22 →
 *   0.42) as the military establishment increasingly performs compliance with
 *   the unthinkable constraint while maintaining suppressed counterforce
 *   capabilities.
 *
 * KEY AGENTS:
 *   - Civilian populations (great powers): beneficiaries of the categorical removal of great-power total war — protected by mutual vulnerability and strategic incoherence of escalation.
 *   - Military establishments (great powers): institutional payers — lose their mandate to plan for victory; identity-locked to a profession whose primary objective becomes impossible.
 *   - Deterrence policymakers: agenda-setters — maintain the rule that war is unthinkable through declaratory policy, posture reviews, and strategic doctrine enforcement.
 *   - Nuclear-armed adversaries: observer seats — mutually locked in deterrence relationship; survival depends on shared belief that victory is impossible.
 *   - Proxy-war participants and non-nuclear states: displaced victims — absorb violence at sub-nuclear levels because direct great-power conflict is unthinkable.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(war_winnability_post_1945__deterrence_unthinkable, 0.68).
domain_priors:suppression_score(war_winnability_post_1945__deterrence_unthinkable, 0.71).
domain_priors:theater_ratio(war_winnability_post_1945__deterrence_unthinkable, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(war_winnability_post_1945__deterrence_unthinkable, extractiveness, 0.68).
narrative_ontology:constraint_metric(war_winnability_post_1945__deterrence_unthinkable, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(war_winnability_post_1945__deterrence_unthinkable, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(war_winnability_post_1945__deterrence_unthinkable, accessibility_collapse, 0.89).
narrative_ontology:constraint_metric(war_winnability_post_1945__deterrence_unthinkable, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(war_winnability_post_1945__deterrence_unthinkable, tangled_rope).
narrative_ontology:human_readable(war_winnability_post_1945__deterrence_unthinkable, "Nuclear Deterrence: War Winnability Exits the Strategic Space (Unthinkable Reading)").
narrative_ontology:topic_domain(war_winnability_post_1945__deterrence_unthinkable, "strategic_studies/international_relations").

domain_priors:requires_active_enforcement(war_winnability_post_1945__deterrence_unthinkable).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(war_winnability_post_1945__deterrence_unthinkable, 'b83f511c-1bfc-4597-a8cd-43aaab2c3962').
narrative_ontology:cs_kernel_codification('b83f511c-1bfc-4597-a8cd-43aaab2c3962', formalized).
narrative_ontology:cs_authority_grounding('b83f511c-1bfc-4597-a8cd-43aaab2c3962', extraction).
narrative_ontology:cs_interpretation_layer_present('b83f511c-1bfc-4597-a8cd-43aaab2c3962').
narrative_ontology:cs_reading_relation('b83f511c-1bfc-4597-a8cd-43aaab2c3962', war_winnability_post_1945__countervailing_thinkable, forecloses).
narrative_ontology:cs_reading_relation('b83f511c-1bfc-4597-a8cd-43aaab2c3962', war_winnability_post_1945__rhetorical_contraction, influences).
narrative_ontology:cs_axiom('b83f511c-1bfc-4597-a8cd-43aaab2c3962', foundational, nuclear_war_unwinnable_by_structure).
narrative_ontology:cs_axiom_status(nuclear_war_unwinnable_by_structure, holdable).
narrative_ontology:cs_axiom_grounding('b83f511c-1bfc-4597-a8cd-43aaab2c3962', nuclear_war_unwinnable_by_structure, empirically_contingent).
narrative_ontology:cs_axiom('b83f511c-1bfc-4597-a8cd-43aaab2c3962', foundational, great_power_war_prevention_mandatory).
narrative_ontology:cs_axiom_status(great_power_war_prevention_mandatory, holdable).
narrative_ontology:cs_axiom_grounding('b83f511c-1bfc-4597-a8cd-43aaab2c3962', great_power_war_prevention_mandatory, deontological).
narrative_ontology:cs_reference_frame('b83f511c-1bfc-4597-a8cd-43aaab2c3962', nuclear_war_unthinkable).
narrative_ontology:cs_drift_state('b83f511c-1bfc-4597-a8cd-43aaab2c3962', contemporary_proliferation_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('b83f511c-1bfc-4597-a8cd-43aaab2c3962', '').
narrative_ontology:cs_kernel_id(war_winnability_post_1945__deterrence_unthinkable, war_winnability_post_1945).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(war_winnability_post_1945__deterrence_unthinkable, civilian_populations_great_powers).
narrative_ontology:constraint_victim(war_winnability_post_1945__deterrence_unthinkable, military_establishments_great_powers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(war_winnability_post_1945__deterrence_unthinkable, proxy_war_participants).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from the categorical removal of great-power total war from the reachable space. Nuclear weapons made mutual annihilation the terminal state of any escalation, making large-scale warfare between nuclear states strategically incoherent. Civilians are protected by the logic that victory is impossible—mutual assured destruction is a shared interest in non-escalation. Their exit option is nonexistent: they cannot leave the nuclear-armed world; they are locked in by the strategic structure itself.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__deterrence_unthinkable, civilian_populations_great_powers, beneficiary,
    organized, generational, constrained, global).

% Bear the extraction of strategic coherence: their institutional mandate (defend against and plan for victory in major warfare) becomes logically incoherent under nuclear deterrence. Planning for victory against a peer nuclear power is operationally unthinkable—any action that terminates in nuclear exchange cannot be 'won' in any traditional military sense. Military establishments maintain massive force structures and strategic planning apparatus justified by deterrence logic, but deterrence logic denies them the objective (victory) that justifies a military establishment. They are identity-locked to warfare as a professional practice and institution; exit would mean ceasing to exist as institutions.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__deterrence_unthinkable, military_establishments_great_powers, payer,
    institutional, generational, identity_locked, global).

% Set and enforce the constraint through declaratory policy, nuclear posture statements, and strategic doctrine that insists victory is impossible and war is therefore unthinkable. They maintain the rule that 'there is no winning nuclear war' and that great-power conflict must be prevented, not won. They have some exit option—shifting to a countervailing doctrine or rhetorical repositioning—but doing so would destabilize the deterrent logic that their constituencies depend on.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__deterrence_unthinkable, deterrence_policymakers, agenda_setter,
    institutional, generational, mobile, global).

% Are locked in mutual vulnerability. Each side's survival depends on the other side believing that victory is impossible. The constraint is not imposed by one side on the other; it is the structural outcome of shared nuclear capability. No actor can unilaterally exit the deterrent relationship without risking war.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__deterrence_unthinkable, nuclear_armed_adversaries, observer,
    institutional, generational, analytical, global).

% Bear the extraction of violent conflict at the sub-nuclear level. Because direct great-power war is unthinkable, nuclear powers delegate warfare to proxy actors in regional conflicts, which persist and intensify. Proxy participants absorb the violence that nuclear deterrence displaced from the nuclear-armed states themselves.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__deterrence_unthinkable, proxy_war_participants, payer,
    moderate, biographical, trapped, regional).

% Are excluded from the benefit structure: they do not possess the weapons that make them non-vulnerable, but they depend on the nuclear peace between great powers for their own security. They have no say in the constraint's enforcement and cannot credibly threaten to break it. Some seek nuclear status to gain entry to the protected circle.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__deterrence_unthinkable, non_nuclear_states, excluded,
    moderate, generational, constrained, global).

% Would argue that the constraint should be made permanent through disarmament rather than maintained through the terror of mutual assured destruction. They are excluded from strategic planning and high-policy decisions; their voices are not in the room where deterrence doctrine is set. They face the paradox that the constraint protects them while depending on a logic (terror-based deterrence) they oppose.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__deterrence_unthinkable, disarmament_advocates, excluded,
    moderate, civilizational, trapped, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(war_winnability_post_1945__deterrence_unthinkable, diffuse).
narrative_ontology:fixing_cost_class(war_winnability_post_1945__deterrence_unthinkable, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the coordination problem of preventing great-power total war: both nuclear-armed sides have a mutual interest in avoiding escalation because escalation terminates in mutual annihilation. The constraint enforces mutual non-aggression through shared vulnerability rather than through treaty or trust. Each side's strategic interest is war prevention, not war preparation.
% TRANSFER_FUNCTION: Transfers strategic coherence away from military establishments (which lose their traditional objective of victory) and toward civilian populations (which gain protection from great-power total war). Also transfers the risk of warfare downward to proxy actors and non-nuclear states, who absorb violence that nuclear deterrence displaced from the protected sphere.
% ABSENT_VOICES: Military strategists and service branches advocating for counterforce doctrines and limited nuclear war remain partially in the room but are constrained by deterrence logic from fully asserting their position. Disarmament advocates, nuclear abolitionists, and non-nuclear states' strategic voices are structurally excluded from deterrence policy-setting. Proxy-war combatants have no representation in great-power strategic decisions.
% DISAPPEARANCE_RATIONALE: If the constraint—the belief that nuclear war is unwinnable and therefore unthinkable—disappeared overnight, the strategic space would reopen to great-power total war planning. Military establishments would recover their traditional mandate. Strategic doctrine would shift toward counterforce, damage limitation, and scenarios of 'victory.' The proxy war displacement would likely compress as great powers returned to direct competition. Civilian protection would evaporate.
% FOUNDING_PROBLEM: Great-power rivalry in the pre-nuclear era was terminated only by total military victory or exhaustion. The nuclear age created a scenario where traditional victory was impossible: any escalation between peer nuclear powers spirals to mutual annihilation, making the war unwinnable for all parties. The founding problem was: how do great powers coexist without fighting when warfare is the traditional arena of great-power competition?
% FOUNDING_PROBLEM_CORROBORATION: Strategic theorists (Schelling, Jervis, Waltz, Brodie) outside the military establishment attested to the problem and its nuclear resolution in the 1950s-1960s. Declassified strategic doctrine and posture reviews from competing nuclear powers confirm the constraint: both US and Soviet strategic planners worked within the assumption that nuclear war is unwinnable. Independent arms control analysts and historians of Cold War deterrence corroborate that the founding problem persisted throughout the nuclear era—the threat of great-power war remained until the structural unthinkability of nuclear escalation made direct conflict irrational.
narrative_ontology:disappearance_verdict(war_winnability_post_1945__deterrence_unthinkable, world_rearranges).
narrative_ontology:founding_problem_status(war_winnability_post_1945__deterrence_unthinkable, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(war_winnability_post_1945__deterrence_unthinkable, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(war_winnability_post_1945__deterrence_unthinkable, 'none', 1).
narrative_ontology:epsilon_provenance(war_winnability_post_1945__deterrence_unthinkable, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(war_winnability_post_1945__deterrence_unthinkable_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(war_winnability_post_1945__deterrence_unthinkable, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(war_winnability_post_1945__deterrence_unthinkable_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises from 0.45 to 0.68 over the 80-year interval because the constraint becomes progressively harder to maintain as technological proliferation and doctrinal evolution provide reasons to doubt its permanence. Each generation of strategists inherits the constraint as structurally binding while simultaneously exploring its margins (counterforce doctrine, limited nuclear scenarios, escalation ladders). Suppression rises correspondingly (0.38 → 0.71) because deterrence doctrine must actively suppress and delegitimize planning for nuclear victory, not merely discourage it. Theater ratio rises (0.22 → 0.42) because military establishments increasingly perform compliance with the constraint (declaratory no-first-use, strategic restraint, arms control participation) while maintaining suppressed operational planning for scenarios the constraint says are impossible. The measurements show that suppression and theater both stabilize after t=60, indicating the constraint has reached a stable enforcement state where the suppression apparatus is well-established and the performance of compliance is routinized. Extractiveness stabilizes because the structural cost to military establishments (loss of victory as a conceivable objective) reaches a plateau: further technological or doctrinal innovation cannot recover the lost mandate.
 *
 * PERSPECTIVAL GAP:
 *   From the civilian beneficiary seat, the constraint is pure protection: nuclear weapons made war unthinkable and therefore impossible, removing the greatest threat to survival. From the military establishment seat, the constraint is pure extraction: institutional coherence depends on planning for victory, and the constraint denies victory as a reachable objective. Policymakers split the difference: they maintain the constraint (deterrence unthinkable) while quietly funding and planning for the countervailing scenario (limited nuclear war). The engine should compute radically different types across seats—Mountain or Rope for civilians and policymakers; Snare or Tangled Rope for military establishments. This divergence is structural, not a measurement error.
 *
 * DIRECTIONALITY LOGIC:
 *   Civilian populations are beneficiaries (low directionality, ~0.15-0.25): they gain the greatest benefit (removal of existential threat) at nearly zero cost beyond continued vulnerability to proxy wars and strategic instability. They cannot exit; their directionality is driven entirely by beneficiary status. Military establishments are targets (high directionality, ~0.75-0.85): they bear the extraction (loss of institutional mandate), face extreme suppression of any alternative strategic vision, and are identity-locked to a profession that the constraint makes incoherent. Their exit would mean institutional suicide—abandoning their role as defenders. Deterrence policymakers are asymmetric (directionality ~0.45-0.55): they benefit from the constraint's stability (it prevents war) but bear costs in military establishment resistance and the need for constant enforcement. They have some exit option (shift to countervailing doctrine) but doing so would destabilize the deterrent. Proxy-war participants and non-nuclear states bear costs (high directionality, ~0.70-0.80) but have no formal status in the constraint system—they are secondary victims of the displacement mechanism.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint is NOT mandatrophy: it has a live founding problem (great-power rivalry remains), a live functional purpose (preventing nuclear war), and active enforcement (deterrence doctrine, strategic planning suppression, arms control regimes). The constraint is Tangled Rope, not Piton: there is genuine coordination (civilians benefit from war prevention; policymakers coordinate on mutual non-escalation) AND genuine extraction (military establishments lose their traditional objective). The theater_ratio rise (0.22 → 0.42) is NOT inertial performance of a dead function—it reflects the increasing sophistication and cost of suppressing alternative strategic visions (counterforce doctrine, limited war scenarios, first-strike stability concerns). The measurement series shows active enforcement hardening, not theatrical decay.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    ontological_vs_rhetorical_winnability,
    'Is war winnability actually impossible under nuclear deterrence (ontological impossibility), or has it merely become unsayable in official policy while remaining operationally planned (rhetorical suppression)?',
    'Declassified strategic planning documents, FOIA releases, and post-Cold War strategic reviews comparing public doctrine to operational plans. If operational plans consistently violate stated doctrine, winnability remained conceptually reachable despite rhetorical suppression.',
    'If ontological: the constraint is a structural fact of the nuclear age, shared by all parties, making great-power war truly unthinkable. If rhetorical: the constraint is maintained by suppression and performative compliance—it becomes a Snare or Piton, not coordination. The reading''s core claim hinges on this distinction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(ontological_vs_rhetorical_winnability, conceptual, 'Whether nuclear war winnability is categorically impossible or rhetorically suppressed.').

omega_variable(
    suppression_mechanism_structural_vs_internalized,
    'Is the measured suppression (0.71) primarily structural (external enforcement of doctrine, delegitimization of counterforce plans) or internalized (military strategists have absorbed the logic that victory is impossible)?',
    'Post-deterrence institutional behavior: if deterrence doctrine collapses and military establishments immediately resume planning for victory, suppression was primarily internalized cultural norm. If they require extensive re-authorization and doctrinal revision, suppression was primarily structural.',
    'If primarily structural: the constraint depends on active enforcement and could be reversed by policy change. If primarily internalized: the constraint has become a norm that persists even if formal enforcement relaxes. An internalized suppression would make the constraint more stable but also more extractive (the target cannot exit even if enforcement ends).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_internalized, empirical, 'Whether suppression of victory planning is externally enforced or internalized professional norm.').

omega_variable(
    civilian_beneficiary_permanence,
    'Are civilian populations genuine beneficiaries of the deterrence_unthinkable constraint, or are they incidental beneficiaries whose interests are secondary to great-power strategic stability?',
    'Analysis of deterrence policy formation: were civilian protection and welfare considered primary objectives, or were they post-hoc justifications for a strategic arrangement negotiated for other reasons (great-power stability, deterrent credibility)?',
    'If primary: civilian protection is genuinely coordinated and the constraint is true Tangled Rope. If secondary: civilians are incidental beneficiaries of an arrangement whose primary function is great-power coordination—the constraint is Rope or Piton with civilians as externalities, not true parties.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(civilian_beneficiary_permanence, conceptual, 'Whether civilian protection is a primary beneficiary or incidental side effect of great-power deterrence.').

omega_variable(
    military_mandate_recovery_path,
    'Is military-establishment exit from the deterrence_unthinkable constraint structurally possible, or is the identity-lock to the warfare profession absolute?',
    'Historical cases where military institutions successfully reframed their mission (e.g., transition from cavalry to armored warfare, or from conventional to cyber defense). If no precedent exists for military institutions surviving a mandate shift of this magnitude, exit is effectively impossible.',
    'If exit is possible: military establishments are Tangled Rope payers, bearing extraction but with theoretical alternatives. If exit is impossible: they are Snare victims, unable to leave the institutional role the constraint denies them.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(military_mandate_recovery_path, empirical, 'Whether military institutions can exit or reframe their mandate under the deterrence constraint.').

omega_variable(
    sibling_reading_mutual_exclusivity,
    'Can countervailing_thinkable and deterrence_unthinkable coexist in a single strategic framework, or do they logically foreclose each other?',
    'Examination of Cold War strategic doctrine: did the same military establishment officially hold both deterrence unthinkability (public doctrine) and countervailing viability (operational planning), or are these genuinely mutually exclusive positions held by different parties?',
    'If they coexist: the readings are sibling positions held simultaneously by different factions (coexists_with relation). If they foreclose: the constraint is itself a reading that rules out the alternative (forecloses relation), making this story''s claim stronger.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_mutual_exclusivity, conceptual, 'Whether the deterrence_unthinkable and countervailing_thinkable readings can be held simultaneously.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(war_winnability_post_1945__deterrence_unthinkable, 0, 80).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(war__tr_t0, war_winnability_post_1945__deterrence_unthinkable, theater_ratio, 0, 0.22).
narrative_ontology:measurement(war__tr_t10, war_winnability_post_1945__deterrence_unthinkable, theater_ratio, 10, 0.28).
narrative_ontology:measurement(war__tr_t20, war_winnability_post_1945__deterrence_unthinkable, theater_ratio, 20, 0.35).
narrative_ontology:measurement(war__tr_t40, war_winnability_post_1945__deterrence_unthinkable, theater_ratio, 40, 0.4).
narrative_ontology:measurement(war__tr_t60, war_winnability_post_1945__deterrence_unthinkable, theater_ratio, 60, 0.42).
narrative_ontology:measurement(war__tr_t80, war_winnability_post_1945__deterrence_unthinkable, theater_ratio, 80, 0.42).

% Extraction over time
narrative_ontology:measurement(war__be_t0, war_winnability_post_1945__deterrence_unthinkable, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(war__be_t10, war_winnability_post_1945__deterrence_unthinkable, base_extractiveness, 10, 0.52).
narrative_ontology:measurement(war__be_t20, war_winnability_post_1945__deterrence_unthinkable, base_extractiveness, 20, 0.58).
narrative_ontology:measurement(war__be_t40, war_winnability_post_1945__deterrence_unthinkable, base_extractiveness, 40, 0.64).
narrative_ontology:measurement(war__be_t60, war_winnability_post_1945__deterrence_unthinkable, base_extractiveness, 60, 0.68).
narrative_ontology:measurement(war__be_t80, war_winnability_post_1945__deterrence_unthinkable, base_extractiveness, 80, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(war__su_t0, war_winnability_post_1945__deterrence_unthinkable, suppression_requirement, 0, 0.38).
narrative_ontology:measurement(war__su_t10, war_winnability_post_1945__deterrence_unthinkable, suppression_requirement, 10, 0.52).
narrative_ontology:measurement(war__su_t20, war_winnability_post_1945__deterrence_unthinkable, suppression_requirement, 20, 0.61).
narrative_ontology:measurement(war__su_t40, war_winnability_post_1945__deterrence_unthinkable, suppression_requirement, 40, 0.68).
narrative_ontology:measurement(war__su_t60, war_winnability_post_1945__deterrence_unthinkable, suppression_requirement, 60, 0.71).
narrative_ontology:measurement(war__su_t80, war_winnability_post_1945__deterrence_unthinkable, suppression_requirement, 80, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(war_winnability_post_1945__deterrence_unthinkable, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(war_winnability_post_1945__deterrence_unthinkable, 0.12).
narrative_ontology:affects_constraint(war_winnability_post_1945__deterrence_unthinkable, war_winnability_post_1945__countervailing_thinkable).
narrative_ontology:affects_constraint(war_winnability_post_1945__deterrence_unthinkable, war_winnability_post_1945__rhetorical_contraction).
narrative_ontology:affects_constraint(war_winnability_post_1945__deterrence_unthinkable, proxy_war_displacement_mechanism).
narrative_ontology:affects_constraint(war_winnability_post_1945__deterrence_unthinkable, nuclear_proliferation_pressures).

% DUAL FORMULATION NOTE:
% This constraint is one reading (deterrence_unthinkable) of the contested kernel war_winnability_post_1945. Two sibling readings exist: countervailing_thinkable (limited nuclear victory remains possible) and rhetorical_contraction (winnability unsayable but operationally planned). These are SEPARATE constraint stories with different ε values, different beneficiary/victim structures, and different classifications. The kernel contest is unresolved; the readings are linked through network.affects_constraints and commentary.kernel_context rather than merged into one story.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(war_winnability_post_1945__deterrence_unthinkable, institutional, 0.8).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
