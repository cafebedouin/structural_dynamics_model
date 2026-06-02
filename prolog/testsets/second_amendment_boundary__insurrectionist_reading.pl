% ============================================================================
% CONSTRAINT STORY: second_amendment_boundary__insurrectionist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_second_amendment_boundary__insurrectionist_reading, []).

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
    constraint_indexing:directionality_override/3,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: second_amendment_boundary__insurrectionist_reading
 *   human_readable: Second Amendment Insurrectionist Reading: Armed Resistance Capacity Against Tyranny
 *   domain: constitutional_law/political_theory/firearms_policy
 *
 * SUMMARY:
 *   The insurrectionist reading of the Second Amendment asserts that the
 *   right to bear arms is instrumental to the capacity of armed citizens to
 *   resist tyrannical government overreach, positioning firearm access as
 *   constitutional insurance against executive power consolidation. This
 *   reading sits within a contested kernel — the Second Amendment itself —
 *   that admits multiple structural interpretations with different extraction
 *   profiles and victim sets. The insurrectionist reading differs from the
 *   individual-right reading (which grounds protection in personal
 *   self-defense) and the militia-conditioned reading (which treats militia
 *   service as a limiting condition on the scope of protection). Each reading
 *   invokes the same constitutional text but instantiates different
 *   structural constraints: different beneficiaries, different victims,
 *   different suppression mechanisms, and different relationships to state
 *   authority and public safety. The insurrectionist reading specifically
 *   prioritizes armed deterrent capacity over state monopoly on legitimate
 *   violence, which generates a tangled_rope constraint: genuine coordination
 *   function (the deterrent capacity constrains executive overreach) coupled
 *   with asymmetric extraction (civilian casualty costs, public safety
 *   erosion, and enforcement burden are distributed broadly while deterrent
 *   benefits concentrate on armed citizens). The measurement trajectory shows
 *   rising extractiveness (0.35→0.58) and rising suppression requirement
 *   (0.58→0.72) over the 40-year interval, reflecting increasing enforcement
 *   burden as the reading's institutional embedding (DC v Heller 2008 and
 *   subsequent jurisprudence) creates mounting pressure for state and federal
 *   authorities to manage the public safety externalities of expanded
 *   protected firearm categories.
 *
 * KEY AGENTS:
 *   - Armed Citizens Claiming Deterrent Authority (institutional/arbitrage): Primary beneficiaries — extract legitimacy and legal protection for firearm access; bear minimal suppression risk under insurrectionist reading; have exit capacity through jurisdictional arbitrage
 *   - State Security Apparatus (institutional/trapped): Primary victim — faces erosion of violence monopoly, escalating enforcement burden, and constitutional constraints on regulation; trapped by constitutional framework; bears suppression and institutional legitimacy costs
 *   - Civilian Public Safety (powerless/trapped): Secondary victim — exposed to increased armed-conflict risk in hypothetical insurrectionist scenario; bears civilian casualty cost; no exit option; no coordination benefit from deterrent function
 *   - Armed Resistance Advocates (moderate/constrained): Beneficiary with constraints — benefits from legitimacy frame but faces legal and social barriers; constrained exit; mixed extraction and coordination experience
 *   - Firearms Industry and Commerce (institutional/arbitrage): Beneficiary — market expansion from protected military-grade categories; arbitrage capacity across jurisdictions; pure coordination perspective
 *   - Constitutional Jurisprudence Post-Heller (institutional/constrained): Actor maintaining performative fidelity to militia clause while operationalizing individual right — piton dynamics (degraded function maintained through institutional inertia)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(second_amendment_boundary__insurrectionist_reading, 0.58).
domain_priors:suppression_score(second_amendment_boundary__insurrectionist_reading, 0.72).
domain_priors:theater_ratio(second_amendment_boundary__insurrectionist_reading, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(second_amendment_boundary__insurrectionist_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(second_amendment_boundary__insurrectionist_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(second_amendment_boundary__insurrectionist_reading, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(second_amendment_boundary__insurrectionist_reading, tangled_rope).
narrative_ontology:human_readable(second_amendment_boundary__insurrectionist_reading, "Second Amendment Insurrectionist Reading: Armed Resistance Capacity Against Tyranny").
narrative_ontology:topic_domain(second_amendment_boundary__insurrectionist_reading, "constitutional_law/political_theory/firearms_policy").

domain_priors:requires_active_enforcement(second_amendment_boundary__insurrectionist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(second_amendment_boundary__insurrectionist_reading, 'ae5ac2bd-58e8-40da-8021-7142143153b5').
narrative_ontology:cs_kernel_codification('ae5ac2bd-58e8-40da-8021-7142143153b5', formalized).
narrative_ontology:cs_authority_grounding('ae5ac2bd-58e8-40da-8021-7142143153b5', lineage).
narrative_ontology:cs_interpretation_layer_present('ae5ac2bd-58e8-40da-8021-7142143153b5').
narrative_ontology:cs_reading_relation('ae5ac2bd-58e8-40da-8021-7142143153b5', individual_right_reading, coexists_with).
narrative_ontology:cs_reading_relation('ae5ac2bd-58e8-40da-8021-7142143153b5', militia_conditioned_reading, forecloses).
narrative_ontology:cs_axiom('ae5ac2bd-58e8-40da-8021-7142143153b5', foundational, individual_armed_capacity_deters_tyranny).
narrative_ontology:cs_axiom_status(individual_armed_capacity_deters_tyranny, holdable).
narrative_ontology:cs_axiom_grounding('ae5ac2bd-58e8-40da-8021-7142143153b5', individual_armed_capacity_deters_tyranny, empirically_contingent).
narrative_ontology:cs_axiom('ae5ac2bd-58e8-40da-8021-7142143153b5', foundational, state_monopoly_on_violence_enables_tyranny).
narrative_ontology:cs_axiom_status(state_monopoly_on_violence_enables_tyranny, holdable).
narrative_ontology:cs_axiom_grounding('ae5ac2bd-58e8-40da-8021-7142143153b5', state_monopoly_on_violence_enables_tyranny, empirically_contingent).
narrative_ontology:cs_reference_frame('ae5ac2bd-58e8-40da-8021-7142143153b5', armed_citizens_as_tyranny_constraint).
narrative_ontology:cs_drift_state('ae5ac2bd-58e8-40da-8021-7142143153b5', contemporary_post_heller_jurisprudence, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('ae5ac2bd-58e8-40da-8021-7142143153b5', '').
narrative_ontology:cs_kernel_id(second_amendment_boundary__insurrectionist_reading, second_amendment_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(second_amendment_boundary__insurrectionist_reading, armed_citizens_deterrent_claimants).
narrative_ontology:constraint_victim(second_amendment_boundary__insurrectionist_reading, state_security_apparatus).
narrative_ontology:constraint_victim(second_amendment_boundary__insurrectionist_reading, civilian_public_safety).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: UNARMED CIVILIANS (SNARE) — Trapped by the insurrectionist reading's structural logic: proliferation of military-capable firearms increases their exposure to armed conflict, reduces state monopoly on legitimate violence, and provides no exit option. They bear the extraction cost (civilian casualty risk in hypothetical armed confrontation) with no coordination benefit and no agency to exit. Maximum experienced extraction.
constraint_indexing:constraint_classification(second_amendment_boundary__insurrectionist_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: ARMED RESISTANCE ADVOCATES (TANGLED ROPE) — Benefit from the insurrectionist reading's legitimacy claim (deterrent against tyranny, insurance against government overreach) but face material constraints (legal barriers, social stigma, enforcement risk). The constraint coordinates a deterrent function (the presence of armed capacity theoretically constrains tyrannical impulses) while enabling asymmetric extraction: advocates extract legitimacy and political salience while bearing lower enforcement risk than the public.
constraint_indexing:constraint_classification(second_amendment_boundary__insurrectionist_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: FIREARMS INDUSTRY (ROPE) — Beneficiary with arbitrage capacity. The insurrectionist reading expands the protected domain to include military-grade arms, creating market expansion and legal defensibility. Experiences the constraint as pure coordination: the reading enables a business model by providing constitutional legitimacy. Effective extraction runs toward this agent; they exit at will via regulatory arbitrage across jurisdictions.
constraint_indexing:constraint_classification(second_amendment_boundary__insurrectionist_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: STATE SECURITY APPARATUS (SNARE) — Victim of the insurrectionist reading's structural logic. The reading directly contradicts the state monopoly on legitimate violence, which is the foundational premise of modern state authority. The state is trapped by the constitutional framing: it cannot exit the Second Amendment framework without constitutional amendment (high-cost, low-probability). It faces escalating enforcement burden and erosion of the violence monopoly. The constraint extracts institutional legitimacy from the state apparatus and transfers it to armed citizens.
constraint_indexing:constraint_classification(second_amendment_boundary__insurrectionist_reading, snare,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 5: CONSTITUTIONAL JURISPRUDENCE (PITON) — The insurrectionist reading invokes militia language but operationalizes as individual-right doctrine divorced from militia function (Supreme Court DC v Heller 2008 onwards). The jurisprudence maintains performative fidelity to the militia clause while implementing substantive individual-right protection. Theater ratio high: the insurrectionist reading cites militia as legitimacy source while enabling extraction divorced from militia coordination. The functional marriage between reading and doctrine has eroded; doctrine persists through institutional inertia.
constraint_indexing:constraint_classification(second_amendment_boundary__insurrectionist_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — From comparative/civilizational perspective, the insurrectionist reading coordinates a genuine function: constraining executive power through diffuse armed capacity. This coordination benefit is real and can be measured (institutional checks on tyranny). But the reading generates asymmetric extraction: the cost structure (civilian gun violence, public safety erosion, enforcement burden) is distributed broadly while the coordination benefit (deterrent capacity) accrues concentrated to armed citizens and firearms industry. The analytical observer sees a mixed coordination-extraction hybrid, not a pure natural law or pure predation.
constraint_indexing:constraint_classification(second_amendment_boundary__insurrectionist_reading, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(second_amendment_boundary__insurrectionist_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(second_amendment_boundary__insurrectionist_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(second_amendment_boundary__insurrectionist_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(second_amendment_boundary__insurrectionist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(second_amendment_boundary__insurrectionist_reading, TR),
    TR >= 0.70.

:- end_tests(second_amendment_boundary__insurrectionist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The insurrectionist reading generates extraction through several mechanisms: (1) asymmetric legitimacy transfer from state authority to armed citizens; (2) diffuse public-safety costs borne by unarmed population; (3) expansion of protected firearm categories that benefit firearms industry and armed advocates. The extractiveness is not maximal (snare territory ≥0.66) because the coordination function — deterrent against tyranny — is genuine (though contested in empirical efficacy). The measurement trajectory shows rising extractiveness as jurisprudential embedding creates institutional lock-in, increasing enforcement burden and suppression costs. Suppression (0.72): High. The insurrectionist reading requires substantial suppression mechanisms to coexist with public-safety constraints: licensing frameworks, background checks, mental-health screenings, ammunition regulation, storage requirements. The suppression is structural and enforced — not consensual or voluntary. The measurement trajectory shows rising suppression requirement as state authorities attempt to manage the public-safety externalities of expanded protected categories. Theater ratio (0.68): High. The insurrectionist reading maintains performative fidelity to the militia clause (militia is mentioned in the text) while operationalizing as individual-right doctrine divorced from militia coordination function. The reading cites historical militia context but the actual protected category (military-grade civilian firearms) exceeds historical militia provision. The theater reflects the gap between stated constraint (militia context) and actual constraint (individual deterrent claim).
 *
 * PERSPECTIVAL GAP:
 *   The insurrectionist reading exhibits a perspectival canyon between beneficiaries and victims. The firearms industry sees rope (coordination mechanism enabling market function). Armed resistance advocates see rope or tangled_rope (coordination benefit with manageable constraints). The state security apparatus sees snare (trapped by constitutional constraint, bearing erosion of monopoly on legitimate violence, with no exit). Unarmed civilians see snare (trapped by dispersed armed population without coordination benefit or exit). The analytical observer sees tangled_rope (genuine coordination function coupled with genuine asymmetric extraction and public-safety costs). No perspective sees mountain because the constraint is institutionally contingent — it depends on constitutional interpretation, which is a human artifact subject to change through amendment or jurisprudential reversal. The persisting disagreement across perspectives reflects genuine structural asymmetry: the beneficiaries and the constitutional system permit the reading; the victims and enforcement apparatus bear the costs. Mandatrophy is resolved through the tangled_rope classification: the constraint is not pure extraction (there is a coordination function) nor pure coordination (there is material asymmetry and extraction).
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality derives from the agent's structural position relative to the insurrectionist reading's extraction flow. Armed citizens and firearms industry are beneficiaries with arbitrage capacity (low d: 0.05–0.15), experiencing negative or minimal effective extraction. Armed resistance advocates are moderate beneficiaries with constraints (d≈0.35), experiencing moderate effective extraction because they benefit from legitimacy but face enforcement risk. Unarmed civilians are trapped victims (d≈0.95), bearing maximum effective extraction from civilian casualty risk and public-safety erosion with no coordination benefit. State security apparatus is institutionally trapped (d≈0.98), bearing institutional legitimacy erosion and escalating enforcement burden with no exit capacity. The analytical observer (d≈0.72) sees the symmetric balance point: genuine deterrent coordination function paired with genuine asymmetric extraction, justifying tangled_rope classification.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    deterrent_capacity_efficacy,
    'Does armed citizen capacity actually deter state tyranny, or is the deterrent claim a legitimacy narrative for extraction?',
    'Comparative institutional analysis: correlation between civilian armed capacity and prevention of tyrannical government action; historical cases where armed resistance succeeded vs failed; mechanisms of actual tyranny (surveillance, legal persecution, institutional capture) that are NOT constrained by distributed individual firearms',
    'If deterrent is effective: the coordination benefit is real, classification remains tangled_rope (mixed coordination and extraction). If deterrent is illusory: classification collapses toward snare (pure extraction with legitimacy theater).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(deterrent_capacity_efficacy, empirical, 'Whether armed citizen capacity provides actual deterrent against tyranny').

omega_variable(
    civilian_casualty_asymmetry,
    'What is the cost-benefit ratio of civilian firearm access in terms of public safety harm vs theoretical deterrent benefit?',
    'Epidemiological analysis comparing countries by armed civilian capacity and homicide/suicide rates; modeling of armed resistance scenarios and predicted civilian casualty rates; Bayesian updating on historical armed insurgency outcomes',
    'If harm exceeds theoretical benefit: victims set should expand to include broader public; extractiveness should increase toward snare threshold. If benefit exceeds harm: the tangled_rope classification is justified by measured coordination benefit.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(civilian_casualty_asymmetry, empirical, 'Cost-benefit of civilian firearm access: safety harm vs deterrent benefit').

omega_variable(
    insurrectionist_reading_vs_self_defense,
    'Is the insurrectionist reading''s core claim (armed capacity against tyranny) coherent with the self-defense reading (individual right to self-preservation), or does insurrectionist logic foreclose self-defense?',
    'Textual and logical analysis of Second Amendment readings; examination of whether a framework can simultaneously hold both insurrectionist deterrent and self-defense protection without contradiction or priority ranking',
    'If coherent: insurrectionist and self-defense readings coexist. If insurrectionist forecloses self-defense (e.g., requires arms militarily capable of state resistance, which implies civilian self-defense weapons are instrumentally inadequate): the readings are in foreclosure relationship.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(insurrectionist_reading_vs_self_defense, conceptual, 'Coherence between insurrectionist and self-defense readings of Second Amendment').

omega_variable(
    militia_clause_semantic_drift,
    'Does the militia clause in the Second Amendment function as a limiting condition (militia service required for protection) or as a historical context statement (well-ordered militia mentioned but does not limit the operative clause)?',
    'Original-intent analysis; comparative reading to other constitutional subordinate clauses; examination of how different judicial eras have parsed the clause structure; linguistic analysis of 18th-century militia discourse',
    'If limiting: insurrectionist reading must incorporate militia coordination function, reducing extractiveness and shifting classification. If context-only: the reading stands as pure individual-right doctrine, maintaining current extractiveness and tangled_rope classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(militia_clause_semantic_drift, conceptual, 'Whether militia clause limits Second Amendment protection or states context').

omega_variable(
    kernel_reading_contest_location,
    'Which sibling reading of the second_amendment_boundary kernel most directly contests the insurrectionist reading at the axiom level?',
    'Identification of foundational normative claims in each reading; examination of logical foreclosure vs coexistence structure; mapping of which reading''s success most directly undermines the others',
    'Clarifies the topology of the kernel contest: identifies whether readings are genuinely mutually exclusive (forecloses) or are different political coalitions staking simultaneous claims (coexists_with).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest_location, conceptual, 'Topology of Second Amendment kernel contest and reading relationships').

omega_variable(
    authority_grounding_legitimacy_source,
    'What legitimates the insurrectionist reading''s constitutional authority claim: fidelity to original intent, evolutionary constitutional interpretation, natural-rights philosophy, or practical necessity?',
    'Examination of which authority grounding (lineage=original intent, practice=evolved jurisprudence, expertise=constitutional law consensus, theological=natural rights) is actually doing the legitimacy work in judicial opinions and political argumentation',
    'Affects CS classification of authority_grounding and interpretation_layer_present. If legitimacy rests on evolving jurisprudence (practice), the constraint shows interpretation-layer mediation. If on original intent (lineage), it shows fixation. If on natural rights (theological/deontological axioms), it shows resistance to empirical challenge.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(authority_grounding_legitimacy_source, conceptual, 'Which authority grounding legitimates the insurrectionist reading').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(second_amendment_boundary__insurrectionist_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sa_insurg_tr_t0, second_amendment_boundary__insurrectionist_reading, theater_ratio, 0, 0.52).
narrative_ontology:measurement(sa_insurg_tr_t20, second_amendment_boundary__insurrectionist_reading, theater_ratio, 20, 0.62).
narrative_ontology:measurement(sa_insurg_tr_t40, second_amendment_boundary__insurrectionist_reading, theater_ratio, 40, 0.68).

% Extraction over time
narrative_ontology:measurement(sa_insurg_be_t0, second_amendment_boundary__insurrectionist_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(sa_insurg_be_t20, second_amendment_boundary__insurrectionist_reading, base_extractiveness, 20, 0.48).
narrative_ontology:measurement(sa_insurg_be_t40, second_amendment_boundary__insurrectionist_reading, base_extractiveness, 40, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(sa_insurg_su_t0, second_amendment_boundary__insurrectionist_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement(sa_insurg_su_t20, second_amendment_boundary__insurrectionist_reading, suppression_requirement, 20, 0.68).
narrative_ontology:measurement(sa_insurg_su_t40, second_amendment_boundary__insurrectionist_reading, suppression_requirement, 40, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(second_amendment_boundary__insurrectionist_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(second_amendment_boundary__insurrectionist_reading, state_monopoly_on_legitimate_violence).
narrative_ontology:affects_constraint(second_amendment_boundary__insurrectionist_reading, executive_power_constraint_mechanisms).
narrative_ontology:affects_constraint(second_amendment_boundary__insurrectionist_reading, firearms_market_expansion_through_legalization).

% DUAL FORMULATION NOTE:
% The insurrectionist reading is one constraint in the second_amendment_boundary family. The individual_right_reading and militia_conditioned_reading are structurally distinct constraints with different extractiveness profiles and beneficiary/victim sets. The insurrectionist reading is upstream of specific firearms categories (military-grade arms, semi-automatic weapons) and downstream of the general state monopoly on legitimate violence. These constraints are linked through network dependencies: the insurrectionist reading's success increases the political and legal pressure for expansion of protected firearm categories, which in turn affects market expansion constraints and state security apparatus legitimacy.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(second_amendment_boundary__insurrectionist_reading, institutional, 0.82).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
