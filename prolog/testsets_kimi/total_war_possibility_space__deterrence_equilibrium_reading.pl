% ============================================================================
% CONSTRAINT STORY: total_war_possibility_space__deterrence_equilibrium_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_total_war_possibility_space__deterrence_equilibrium_reading, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: total_war_possibility_space__deterrence_equilibrium_reading
 *   human_readable: Deterrence Equilibrium Reading of Total War Possibility Space
 *   domain: international_relations/strategic_studies
 *
 * SUMMARY:
 *   This constraint story instantiates the deterrence_equilibrium_reading of
 *   the contested total_war_possibility_space kernel. It models the strategic
 *   condition in which total war between great powers remains reachable in
 *   military planning and doctrinal space, but is deterred by the existential
 *   costs of mutual vulnerability under nuclear arsenals. The constraint
 *   generates continuous investment in war-fighting and second-strike
 *   capabilities as deterrent signals, while enforcing a hierarchical nuclear
 *   order through alliance management and non-proliferation enforcement. As a
 *   kernel reading, it treats the deterrence equilibrium as one of three live
 *   framings of how total war is constrained, and authors structural data
 *   independently of the sibling nuclear_taboo and space_contraction
 *   readings.
 *
 * KEY AGENTS:
 *   - nuclear_weapon_states (agenda_setter/institutional): Administer deterrence posture, maintain arsenals, control escalation doctrine and alliance terms
 *   - nuclear_weapons_complex (beneficiary/powerful): Extracts resources from continuous modernization and maintenance of nuclear forces
 *   - extended_deterrence_recipients (beneficiary-payer/moderate): Receive security guarantees under alliance umbrellas, sacrifice strategic autonomy and independent options
 *   - non_aligned_non_nuclear_states (payer/moderate): Bear constraints of the non-proliferation regime and nuclear hierarchy without reciprocal security benefits
 *   - deterrence_theorists (observer/analytical): Maintain the intellectual framework justifying mutual vulnerability as stable equilibrium
 *   - anti_nuclear_advocates (excluded/powerless): Excluded from strategic planning discourse; would argue for abolition over equilibrium
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(total_war_possibility_space__deterrence_equilibrium_reading, 0.58).
domain_priors:suppression_score(total_war_possibility_space__deterrence_equilibrium_reading, 0.62).
domain_priors:theater_ratio(total_war_possibility_space__deterrence_equilibrium_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(total_war_possibility_space__deterrence_equilibrium_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(total_war_possibility_space__deterrence_equilibrium_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(total_war_possibility_space__deterrence_equilibrium_reading, theater_ratio, 0.35).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(total_war_possibility_space__deterrence_equilibrium_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(total_war_possibility_space__deterrence_equilibrium_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(total_war_possibility_space__deterrence_equilibrium_reading, tangled_rope).
narrative_ontology:human_readable(total_war_possibility_space__deterrence_equilibrium_reading, "Deterrence Equilibrium Reading of Total War Possibility Space").
narrative_ontology:topic_domain(total_war_possibility_space__deterrence_equilibrium_reading, "international_relations/strategic_studies").

domain_priors:requires_active_enforcement(total_war_possibility_space__deterrence_equilibrium_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(total_war_possibility_space__deterrence_equilibrium_reading, 'ef753d82-0e4c-49d4-90fa-61da55773bac').
narrative_ontology:cs_kernel_codification('ef753d82-0e4c-49d4-90fa-61da55773bac', distributed).
narrative_ontology:cs_authority_grounding('ef753d82-0e4c-49d4-90fa-61da55773bac', expertise).
narrative_ontology:cs_interpretation_layer_present('ef753d82-0e4c-49d4-90fa-61da55773bac').
narrative_ontology:cs_reading_relation('ef753d82-0e4c-49d4-90fa-61da55773bac', total_war_possibility_space__space_contraction_reading, forecloses).
narrative_ontology:cs_reading_relation('ef753d82-0e4c-49d4-90fa-61da55773bac', total_war_possibility_space__nuclear_taboo_reading, coexists_with).
narrative_ontology:cs_axiom('ef753d82-0e4c-49d4-90fa-61da55773bac', foundational, total_war_strategically_reachable_under_mutual_vulnerability).
narrative_ontology:cs_axiom_status(total_war_strategically_reachable_under_mutual_vulnerability, holdable).
narrative_ontology:cs_axiom_grounding('ef753d82-0e4c-49d4-90fa-61da55773bac', total_war_strategically_reachable_under_mutual_vulnerability, empirically_contingent).
narrative_ontology:cs_axiom('ef753d82-0e4c-49d4-90fa-61da55773bac', secondary, deterrence_requires_demonstrated_retaliatory_capability).
narrative_ontology:cs_axiom_status(deterrence_requires_demonstrated_retaliatory_capability, holdable).
narrative_ontology:cs_axiom_grounding('ef753d82-0e4c-49d4-90fa-61da55773bac', deterrence_requires_demonstrated_retaliatory_capability, instrumental).
narrative_ontology:cs_reference_frame('ef753d82-0e4c-49d4-90fa-61da55773bac', classical_deterrence_equilibrium).
narrative_ontology:cs_drift_state('ef753d82-0e4c-49d4-90fa-61da55773bac', post_cold_war_multipolarity, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('ef753d82-0e4c-49d4-90fa-61da55773bac', '').
narrative_ontology:cs_kernel_id(total_war_possibility_space__deterrence_equilibrium_reading, total_war_possibility_space).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(total_war_possibility_space__deterrence_equilibrium_reading, nuclear_weapon_states).
narrative_ontology:constraint_beneficiary(total_war_possibility_space__deterrence_equilibrium_reading, nuclear_weapons_complex).
narrative_ontology:constraint_beneficiary(total_war_possibility_space__deterrence_equilibrium_reading, extended_deterrence_recipients).
narrative_ontology:constraint_victim(total_war_possibility_space__deterrence_equilibrium_reading, non_aligned_non_nuclear_states).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(total_war_possibility_space__deterrence_equilibrium_reading, extended_deterrence_recipients).
narrative_ontology:constraint_vindicates(total_war_possibility_space__deterrence_equilibrium_reading, mutual_vulnerability_bargaining).
narrative_ontology:constraint_vindicates(total_war_possibility_space__deterrence_equilibrium_reading, extended_deterrence_credibility).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administer global deterrence posture through arsenals, command and control, alliance management, and escalation doctrine. They set the terms of mutual vulnerability and enforce non-proliferation through sanctions and regime pressure. Exit would require unilateral disarmament or alliance abandonment, both strategically catastrophic.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__deterrence_equilibrium_reading, nuclear_weapon_states, agenda_setter,
    institutional, generational, constrained, global).

% Collects sustained budgetary flows for warhead modernization, delivery system development, and infrastructure maintenance. The constraint justifies continuous investment independent of conventional threat cycles. Exit would mean pivoting to other defense sectors, but the specialized capital and labor are largely captive to nuclear missions.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__deterrence_equilibrium_reading, nuclear_weapons_complex, beneficiary,
    powerful, biographical, mobile, national).

% Receive security guarantees under nuclear umbrellas, reducing need for independent strategic autonomy. In exchange, they host bases, integrate into alliance command structures, and forgo certain independent military options. Exit would mean acquiring independent nuclear capability or accepting conventional vulnerability, both costly and politically fraught.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__deterrence_equilibrium_reading, extended_deterrence_recipients, beneficiary,
    moderate, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(total_war_possibility_space__deterrence_equilibrium_reading, extended_deterrence_recipients, payer).

% Bear the hierarchical costs of the nuclear order without reciprocal security benefits: constrained by the NPT from acquiring deterrent capability, subject to enforcement actions if they pursue independent programs, and exposed to existential risk generated by great-power arsenals. Their diplomatic efforts toward disarmament are systematically marginalized.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__deterrence_equilibrium_reading, non_aligned_non_nuclear_states, payer,
    moderate, generational, constrained, global).

% Maintain the intellectual architecture of mutual vulnerability, stability-instability paradoxes, and escalation ladder theorization. They produce the analytic vocabulary that makes the constraint legible to policymakers. Their exit consists of shifting to alternative security paradigms, but career and epistemic investments are bound to the deterrence framework.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__deterrence_equilibrium_reading, deterrence_theorists, observer,
    analytical, civilizational, analytical, global).

% Advance abolitionist and disarmament framings that challenge the necessity of perpetual deterrence. They are structurally excluded from strategic planning discourse, defense-funded research institutions, and treaty negotiations dominated by nuclear weapons states. Their voice registers in civil society but rarely in force posture decisions.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__deterrence_equilibrium_reading, anti_nuclear_advocates, excluded,
    powerless, generational, trapped, global).

narrative_ontology:fixing_cost_class(total_war_possibility_space__deterrence_equilibrium_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Prevents total war between nuclear-armed great powers by ensuring that the anticipated costs of full-scale nuclear exchange exceed any conceivable political benefit, thereby stabilizing relations under conditions of mutual vulnerability.
% TRANSFER_FUNCTION: Moves security guarantees and great-power status to nuclear weapon states; moves economic resources from general publics and non-nuclear states to nuclear weapons complexes; moves strategic autonomy from extended deterrence clients and non-aligned states to alliance hegemons.
% ABSENT_VOICES: Anti-nuclear advocates and Global South security scholars who contest the necessity of perpetual deterrence are structurally excluded from strategic planning discourse; populations downwind from test sites and potential conflict zones are not represented in escalation ladder theorization.
% DISAPPEARANCE_RATIONALE: Without the deterrence equilibrium, great powers would face radically reduced constraints on total war, extended deterrence alliances would require fundamental renegotiation or conventional rearmament, the non-proliferation regime would dissolve, and military-industrial flows would shift dramatically.
% FOUNDING_PROBLEM: The industrialization of warfare in the early 20th century eliminated natural limits on conflict destructiveness, culminating in WWII; nuclear weapons created the possibility of civilization-ending war in hours, requiring a stabilizing mechanism for great-power relations under existential threat.
% FOUNDING_PROBLEM_CORROBORATION: Post-revisionist historians and strategic theorists outside the active nuclear weapons complex corroborate the existential crisis of 1945; however, critical security scholars and representatives of non-nuclear states contest that perpetual deterrence was the only solution, arguing that abolition was and remains structurally possible, and note that corroboration of the necessity framing comes overwhelmingly from institutions funded by deterrence maintenance.
narrative_ontology:disappearance_verdict(total_war_possibility_space__deterrence_equilibrium_reading, world_rearranges).
narrative_ontology:founding_problem_status(total_war_possibility_space__deterrence_equilibrium_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(total_war_possibility_space__deterrence_equilibrium_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(total_war_possibility_space__deterrence_equilibrium_reading, 'none', 1).
narrative_ontology:epsilon_provenance(total_war_possibility_space__deterrence_equilibrium_reading, 0.58, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(total_war_possibility_space__deterrence_equilibrium_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(total_war_possibility_space__deterrence_equilibrium_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(total_war_possibility_space__deterrence_equilibrium_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58) reflects the substantial resources diverted to arsenal maintenance and the sovereignty costs imposed on non-nuclear states, while acknowledging the genuine coordination function of preventing total war. Suppression (0.62) captures the active enforcement of the non-proliferation regime and alliance discipline that keeps the hierarchical order in place. Theater_ratio (0.35) recognizes that a significant portion of nuclear postureâparticularly modernization programs and extended deterrence signalingâinvolves performative demonstration of resolve that exceeds strictly functional deterrence requirements. Accessibility_collapse (0.72) indicates that alternatives such as general and complete disarmament or defense dominance have been largely marginalized in great-power strategic discourse once the mutual vulnerability frame is accepted. Resistance (0.45) reflects sustained but structurally weak opposition from non-nuclear states and anti-nuclear movements. The temporal series show extraction and suppression rising through the Cold War, moderating briefly in the unipolar moment, and resurging with contemporary great-power competition and modernization programs.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat (nuclear_weapon_states) perceives the constraint as necessary coordination preventing civilizational catastrophe; the payer seat (non_aligned_non_nuclear_states) perceives an extractive hierarchy that reserves ultimate security to a few while externalizing nuclear risk to the many. Extended_deterrence_recipients occupy an intermediate seat where the coordination benefit is visible but the extraction through autonomy loss is also felt. The engine computes this divergence from identical structural data through the directionality derivation chain.
 *
 * DIRECTIONALITY LOGIC:
 *   Nuclear_weapon_states sit near the beneficiary end: they derive security, status, and bargaining leverage from the constraint they administer. The nuclear_weapons_complex sits firmly as beneficiary through budget capture. Extended_deterrence_recipients experience mixed directionalityâgaining security subsidy while paying in strategic autonomy and basing costsâplacing them near the middle. Non_aligned_non_nuclear_states are clear targets: they bear the constraint's hierarchical costs (non-proliferation limitations, existential risk, exclusion from security guarantees) without the compensating benefits, yielding high directionalities toward the target end.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problemâindustrialized total war made existentially catastrophic by nuclear weaponsâwas genuinely live in 1945. However, the persistence of massive arsenals and continuous modernization eight decades later suggests the arrangement now serves status-quo maintenance and budgetary capture as much as survival. The founding_problem_status is therefore contested: the problem is live in form but the specific solution (perpetual deterrence equilibrium) may have outlived its functional necessity relative to lower-cost alternatives. The Tangled Rope classification captures this by requiring both genuine coordination and asymmetric extraction, preventing mislabeling as pure extraction (which would ignore the war-prevention function) or pure coordination (which would ignore the hierarchical costs).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    deterrence_vs_taboo_mechanism,
    'Does the prevention of total war operate primarily through material cost-benefit calculation as this reading claims, or through constructed normative prohibition as the nuclear_taboo_reading claims?',
    'Comparative case analysis of non-nuclear conflicts and near-nuclear crises: if material vulnerability alone explains restraint, states without mutual vulnerability should show less restraint; if taboo operates independently, normative rhetoric should predict behavior after controlling for material balance.',
    'If taboo is primary, the extractiveness of the deterrence equilibrium (resource extraction for arsenals) is higher than functionally necessaryâcoordination could be achieved at lower cost. If deterrence is primary, the current resource extraction is the necessary price of survival.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(deterrence_vs_taboo_mechanism, empirical, 'Whether total war avoidance is driven by material deterrence or normative taboo').

omega_variable(
    multipolar_deterrence_stability,
    'Does the classical bilateral deterrence equilibrium remain stable under multipolarity, or has the possibility space for total war expanded beyond the deterrence frame?',
    'Strategic simulations and historical case studies of multipolar nuclear crises; analysis of emerging technology disruption including hypersonic delivery, cyber threats to command and control, and artificial intelligence in early warning.',
    'If unstable, the constraint is undergoing functional drift toward either scaffold (if transitional to a new equilibrium) or piton (if maintained theatrically despite lost function). If stable, it remains a tangled rope with continued asymmetric extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(multipolar_deterrence_stability, empirical, 'Multipolarity and emerging technology effects on deterrence stability').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(total_war_possibility_space__deterrence_equilibrium_reading, 0, 80).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(twps_der_tr_t0, total_war_possibility_space__deterrence_equilibrium_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(twps_der_tr_t20, total_war_possibility_space__deterrence_equilibrium_reading, theater_ratio, 20, 0.28).
narrative_ontology:measurement(twps_der_tr_t40, total_war_possibility_space__deterrence_equilibrium_reading, theater_ratio, 40, 0.38).
narrative_ontology:measurement(twps_der_tr_t60, total_war_possibility_space__deterrence_equilibrium_reading, theater_ratio, 60, 0.4).
narrative_ontology:measurement(twps_der_tr_t80, total_war_possibility_space__deterrence_equilibrium_reading, theater_ratio, 80, 0.35).

% Extraction over time
narrative_ontology:measurement(twps_der_be_t0, total_war_possibility_space__deterrence_equilibrium_reading, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(twps_der_be_t20, total_war_possibility_space__deterrence_equilibrium_reading, base_extractiveness, 20, 0.5).
narrative_ontology:measurement(twps_der_be_t40, total_war_possibility_space__deterrence_equilibrium_reading, base_extractiveness, 40, 0.62).
narrative_ontology:measurement(twps_der_be_t60, total_war_possibility_space__deterrence_equilibrium_reading, base_extractiveness, 60, 0.58).
narrative_ontology:measurement(twps_der_be_t80, total_war_possibility_space__deterrence_equilibrium_reading, base_extractiveness, 80, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(twps_der_su_t0, total_war_possibility_space__deterrence_equilibrium_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(twps_der_su_t20, total_war_possibility_space__deterrence_equilibrium_reading, suppression_requirement, 20, 0.48).
narrative_ontology:measurement(twps_der_su_t40, total_war_possibility_space__deterrence_equilibrium_reading, suppression_requirement, 40, 0.6).
narrative_ontology:measurement(twps_der_su_t60, total_war_possibility_space__deterrence_equilibrium_reading, suppression_requirement, 60, 0.58).
narrative_ontology:measurement(twps_der_su_t80, total_war_possibility_space__deterrence_equilibrium_reading, suppression_requirement, 80, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(total_war_possibility_space__deterrence_equilibrium_reading, total_war_possibility_space__nuclear_taboo_reading).
narrative_ontology:affects_constraint(total_war_possibility_space__deterrence_equilibrium_reading, total_war_possibility_space__space_contraction_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the total_war_possibility_space kernel, decomposed per the epsilon-invariance principle from the colloquial label 'total war constraint' into three structurally distinct claims: deterrence equilibrium (material cost-benefit), nuclear taboo (normative prohibition), and space contraction (cognitive unthinkability). Each reading carries a distinct epsilon, stakeholder structure, and classification.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
