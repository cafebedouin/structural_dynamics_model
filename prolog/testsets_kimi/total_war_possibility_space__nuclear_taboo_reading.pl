% ============================================================================
% CONSTRAINT STORY: total_war_possibility_space__nuclear_taboo_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_total_war_possibility_space__nuclear_taboo_reading, []).

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
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
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
 *   constraint_id: total_war_possibility_space__nuclear_taboo_reading
 *   human_readable: Nuclear Taboo Normative Prohibition of Total War
 *   domain: international_relations/strategic_studies
 *
 * SUMMARY:
 *   This constraint story instantiates the nuclear_taboo_reading of the
 *   total_war_possibility_space kernel. The claim is that total war became
 *   normatively prohibited not through material deterrence or strategic
 *   unthinkability alone, but through a constructed taboo that operates
 *   independent of material capability. The taboo generates genuine
 *   coordination (preventing nuclear use) while simultaneously extracting
 *   sovereignty from non-nuclear states and aspirants through the asymmetric
 *   NPT regime. The story treats the taboo as a tangled rope: a real
 *   normative barrier with active enforcement that also concentrates nuclear
 *   legitimacy in a permanent oligopoly.
 *
 * KEY AGENTS:
 *   - nuclear_weapon_states (agenda_setter/global) â administer the NPT and enforce non-proliferation while retaining exclusive nuclear legitimacy
 *   - aspirant_nuclear_states (payer/national) â bear sanctions and sovereignty costs for challenging the nuclear monopoly
 *   - non_nuclear_weapon_states (payer/global) â forwent nuclear option in exchange for disarmament promises and technology access that remain partially unfulfilled
 *   - international_security_institutions (agenda_setter/global) â administer safeguards and review conferences that maintain the regime
 *   - norm_entrepreneurs (beneficiary/global) â derive influence and career viability from the taboo's continued vitality
 *   - extended_deterrence_clients (payer/national) â rely on patron nuclear umbrellas and accept dependency as the price of non-proliferation
 *   - strategic_studies_observers (analytical/global) â analyze the taboo's strength and fragility from outside the regime's enforcement structure
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(total_war_possibility_space__nuclear_taboo_reading, 0.72).
domain_priors:suppression_score(total_war_possibility_space__nuclear_taboo_reading, 0.78).
domain_priors:theater_ratio(total_war_possibility_space__nuclear_taboo_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(total_war_possibility_space__nuclear_taboo_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(total_war_possibility_space__nuclear_taboo_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(total_war_possibility_space__nuclear_taboo_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(total_war_possibility_space__nuclear_taboo_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(total_war_possibility_space__nuclear_taboo_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(total_war_possibility_space__nuclear_taboo_reading, tangled_rope).
narrative_ontology:human_readable(total_war_possibility_space__nuclear_taboo_reading, "Nuclear Taboo Normative Prohibition of Total War").
narrative_ontology:topic_domain(total_war_possibility_space__nuclear_taboo_reading, "international_relations/strategic_studies").

domain_priors:requires_active_enforcement(total_war_possibility_space__nuclear_taboo_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(total_war_possibility_space__nuclear_taboo_reading, '86ef72c9-7026-42e1-b3c1-c7489d2cf216').
narrative_ontology:cs_kernel_codification('86ef72c9-7026-42e1-b3c1-c7489d2cf216', formalized).
narrative_ontology:cs_authority_grounding('86ef72c9-7026-42e1-b3c1-c7489d2cf216', lineage).
narrative_ontology:cs_interpretation_layer_present('86ef72c9-7026-42e1-b3c1-c7489d2cf216').
narrative_ontology:cs_reading_relation('86ef72c9-7026-42e1-b3c1-c7489d2cf216', total_war_possibility_space__deterrence_equilibrium_reading, coexists_with).
narrative_ontology:cs_reading_relation('86ef72c9-7026-42e1-b3c1-c7489d2cf216', total_war_possibility_space__space_contraction_reading, influences).
narrative_ontology:cs_axiom('86ef72c9-7026-42e1-b3c1-c7489d2cf216', foundational, nuclear_use_categorically_prohibited).
narrative_ontology:cs_axiom_status(nuclear_use_categorically_prohibited, holdable).
narrative_ontology:cs_axiom_grounding('86ef72c9-7026-42e1-b3c1-c7489d2cf216', nuclear_use_categorically_prohibited, deontological).
narrative_ontology:cs_axiom('86ef72c9-7026-42e1-b3c1-c7489d2cf216', foundational, normative_prohibition_independent_of_deterrence).
narrative_ontology:cs_axiom_status(normative_prohibition_independent_of_deterrence, holdable).
narrative_ontology:cs_axiom_grounding('86ef72c9-7026-42e1-b3c1-c7489d2cf216', normative_prohibition_independent_of_deterrence, empirically_contingent).
narrative_ontology:cs_reference_frame('86ef72c9-7026-42e1-b3c1-c7489d2cf216', nuclear_non_use_tradition).
narrative_ontology:cs_drift_state('86ef72c9-7026-42e1-b3c1-c7489d2cf216', contemporary_multipolar_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('86ef72c9-7026-42e1-b3c1-c7489d2cf216', '').
narrative_ontology:cs_kernel_id(total_war_possibility_space__nuclear_taboo_reading, total_war_possibility_space).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(total_war_possibility_space__nuclear_taboo_reading, nuclear_weapon_states).
narrative_ontology:constraint_beneficiary(total_war_possibility_space__nuclear_taboo_reading, international_security_institutions).
narrative_ontology:constraint_beneficiary(total_war_possibility_space__nuclear_taboo_reading, norm_entrepreneurs).
narrative_ontology:constraint_victim(total_war_possibility_space__nuclear_taboo_reading, aspirant_nuclear_states).
narrative_ontology:constraint_victim(total_war_possibility_space__nuclear_taboo_reading, non_nuclear_weapon_states).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(total_war_possibility_space__nuclear_taboo_reading, extended_deterrence_clients).
narrative_ontology:constraint_victim(total_war_possibility_space__nuclear_taboo_reading, extended_deterrence_clients).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Possess declared nuclear arsenals and sit as permanent NPT depositaries and Security Council powers. They set non-proliferation terms, enforce safeguards through sanctions and diplomacy, and retain the exclusive legal right to nuclear weapons under the NPT while justifying the asymmetry as responsible stewardship.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__nuclear_taboo_reading, nuclear_weapon_states, agenda_setter,
    institutional, generational, arbitrage, global).

% Administer the NPT review cycle, IAEA safeguards, and non-proliferation verification. Their mandate and budget depend on the taboo's persistence. They enforce compliance against aspirants but lack authority to compel disarmament by the recognized nuclear powers.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__nuclear_taboo_reading, international_security_institutions, agenda_setter,
    institutional, generational, constrained, global).

% Comprise the vast majority of states that ratified the NPT and renounced nuclear weapons in exchange for disarmament promises and technology-sharing. They participate in review conferences but remain structurally subordinate, unable to acquire independent nuclear deterrents without becoming pariahs.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__nuclear_taboo_reading, non_nuclear_weapon_states, payer,
    organized, generational, constrained, global).

% Seek nuclear capability for security or prestige but face sanctions, preventive threats, and international isolation. Their sovereignty is constrained by a regime they did not design, and their exit options are limited by economic and military encirclement.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__nuclear_taboo_reading, aspirant_nuclear_states, payer,
    moderate, biographical, trapped, national).

% Rely on patron nuclear umbrellas for security and accept dependence as the price of non-proliferation. They benefit from not maintaining independent arsenals but pay in strategic dependency and exposure to their patron's risk calculus.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__nuclear_taboo_reading, extended_deterrence_clients, payer,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(total_war_possibility_space__nuclear_taboo_reading, extended_deterrence_clients, beneficiary).

% Include advocacy networks, humanitarian campaigns, and epistemic communities whose influence, funding, and professional legitimacy are tied to the taboo's continued vitality. They mobilize shame and legal argument to punish norm violations and reinforce the prohibition.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__nuclear_taboo_reading, norm_entrepreneurs, beneficiary,
    organized, biographical, mobile, global).

% Analyze the taboo's strength, erosion, and structural function from outside the regime's enforcement apparatus. They document gaps between disarmament rhetoric and arsenals, and model what happens if the norm breaks.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__nuclear_taboo_reading, strategic_studies_observers, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(total_war_possibility_space__nuclear_taboo_reading, nuclear_weapon_states).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Prevents nuclear war and total-war escalation by constructing a shared normative prohibition that removes nuclear weapons from the usable policy toolkit, creating a predictable if asymmetric strategic environment.
% TRANSFER_FUNCTION: Moves the legitimate possession and threatened use of nuclear weapons from the general community of states to a self-selected oligopoly of declared nuclear powers, enforced through the NPT, IAEA safeguards, sanctions, and coercive non-proliferation measures.
% ABSENT_VOICES: Populations in nuclear-vulnerable regions and disarmed or precluded aspirant states who bear the risk of deterrence failure or preventive attack but are not seated at the strategic bargaining table.
% DISAPPEARANCE_RATIONALE: If the nuclear taboo vanished overnight, the non-proliferation regime would lose normative force, extended deterrence guarantees would be renegotiated or collapse, aspirant states would accelerate weaponization, and the international security order would reorganize around renewed nuclear competition and potential total-war scenarios.
% FOUNDING_PROBLEM: The prospect of civilization-scale destruction opened by atomic weapons in 1945, requiring a normative barrier to prevent unlimited total war and manage nuclear technology.
% FOUNDING_PROBLEM_CORROBORATION: Nuclear weapon states and international institutions attest the problem remains live. Aspirant states and some non-nuclear weapon states attest the problem has mutated into oligopoly maintenance; independent strategic studies scholarship and humanitarian advocacy from outside the beneficiary set corroborate the tension between disarmament promises and persistent asymmetry.
narrative_ontology:disappearance_verdict(total_war_possibility_space__nuclear_taboo_reading, world_rearranges).
narrative_ontology:founding_problem_status(total_war_possibility_space__nuclear_taboo_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(total_war_possibility_space__nuclear_taboo_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(total_war_possibility_space__nuclear_taboo_reading, 'none', 1).
narrative_ontology:epsilon_provenance(total_war_possibility_space__nuclear_taboo_reading, 0.72, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(total_war_possibility_space__nuclear_taboo_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(total_war_possibility_space__nuclear_taboo_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(total_war_possibility_space__nuclear_taboo_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.72) is high because the constraint permanently reserves nuclear status to five states under the NPT while demanding abstinence from all others, creating a two-tier sovereignty structure. Suppression (0.78) is high because the regime relies on active enforcement: IAEA inspections, sanctions on aspirants, and preventive military action against challengers. Theater ratio (0.45) reflects significant performative maintenance â repetitive NPT review conferences, rhetorical reaffirmations, and summitry that sometimes outpaces disarmament action. Accessibility collapse (0.70) is high because, once the taboo is understood, nuclear use and open proliferation become virtually unthinkable for most state actors, collapsing alternative strategic postures. Resistance (0.55) captures persistent challenge from aspirant states and occasional rhetorical erosion by nuclear powers themselves. The measurement series show extraction rising through the Cold War and NPT institutionalization, then plateauing with slight contemporary weakening as multipolar competition strains the norm.
 *
 * PERSPECTIVAL GAP:
 *   From the nuclear weapon states' seat, the taboo is legitimate coordination that prevents armageddon and justifies their special responsibilities. From the aspirant state seat, the same structure is discriminatory extraction that locks in permanent strategic subordination. From the non-nuclear state seat, the constraint is a mixed bargain: stability and extended deterrence versus forfeited strategic autonomy and unfulfilled disarmament pledges. The engine computes these divergent seat types from the structural data rather than adjudicating among them.
 *
 * DIRECTIONALITY LOGIC:
 *   Nuclear weapon states are beneficiaries of the oligopoly (low d); aspirant states are explicit targets of enforcement (high d); non-nuclear weapon states sit in the middle â structurally payers into the regime but with some coordination benefit dampening their effective extraction. International institutions administering the regime have moderate d because they are captured by member-state politics. Norm entrepreneurs benefit from the constraint's existence and have mobile exit, placing them near the beneficiary end.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope classification prevents the false dichotomy of reading the nuclear taboo as either pure coordination (rope) or pure oligopoly (snare). There is a genuine coordination function: the taboo has prevented nuclear war for eight decades. But the same structure that coordinates also extracts, because the non-proliferation norm is inseparable from the NPT's discriminatory codification. A scaffold reading would fail because there is no sunset clause â the nuclear powers have resisted Article VI disarmament commitments. A piton reading would fail because beneficiaries (nuclear states, institutions) are actively and substantially profiting from the constraint's maintenance, not merely performing inertia.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    taboo_materiality_gap,
    'Is the nuclear taboo causally effective independent of material deterrence, or does it ride on deterrence equilibrium?',
    'Historical case studies of nuclear near-use where normative considerations operated against pure strategic logic, and statistical analysis of crisis decision-making to isolate normative from material causal weight.',
    'If the taboo is purely epiphenomenal to deterrence, this reading collapses toward the deterrence_equilibrium_reading and the constraint''s coordination function is misattributed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(taboo_materiality_gap, empirical, 'Whether the taboo has independent causal force or is parasitic on deterrence').

omega_variable(
    norm_entrepreneur_dependency,
    'Does the taboo''s persistence depend on continued advocacy by norm entrepreneurs, or is it now self-sustaining in state practice?',
    'Track taboo strength in periods of low civil society attention versus high attention; measure state rhetoric decoupling and compliance with non-proliferation norms when advocacy pressure recedes.',
    'If dependent on entrepreneurs, the constraint is more fragile than institutionalist theory predicts and may degrade faster under multipolar competition.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(norm_entrepreneur_dependency, conceptual, 'Whether the taboo is sustained by active advocacy or embedded practice').

omega_variable(
    non_nuclear_asymmetry,
    'Are non-nuclear weapon states net beneficiaries of stability or net payers into a discriminatory regime?',
    'Comparative analysis of security outcomes for NNWS under NPT versus counterfactual proliferation scenarios, and assessment of Article VI compliance by nuclear powers.',
    'Resolves whether the broad non-nuclear tier is a victim seat or a coordinated beneficiary seat, shifting the effective extraction calculation for the majority of states.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(non_nuclear_asymmetry, empirical, 'Whether NNWS are victims of or beneficiaries from the nuclear taboo regime').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(total_war_possibility_space__nuclear_taboo_reading, 0, 80).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nuclear_taboo_tr_t0, total_war_possibility_space__nuclear_taboo_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(nuclear_taboo_tr_t10, total_war_possibility_space__nuclear_taboo_reading, theater_ratio, 10, 0.25).
narrative_ontology:measurement(nuclear_taboo_tr_t20, total_war_possibility_space__nuclear_taboo_reading, theater_ratio, 20, 0.35).
narrative_ontology:measurement(nuclear_taboo_tr_t30, total_war_possibility_space__nuclear_taboo_reading, theater_ratio, 30, 0.4).
narrative_ontology:measurement(nuclear_taboo_tr_t45, total_war_possibility_space__nuclear_taboo_reading, theater_ratio, 45, 0.44).
narrative_ontology:measurement(nuclear_taboo_tr_t60, total_war_possibility_space__nuclear_taboo_reading, theater_ratio, 60, 0.46).
narrative_ontology:measurement(nuclear_taboo_tr_t80, total_war_possibility_space__nuclear_taboo_reading, theater_ratio, 80, 0.45).

% Extraction over time
narrative_ontology:measurement(nuclear_taboo_be_t0, total_war_possibility_space__nuclear_taboo_reading, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(nuclear_taboo_be_t10, total_war_possibility_space__nuclear_taboo_reading, base_extractiveness, 10, 0.4).
narrative_ontology:measurement(nuclear_taboo_be_t20, total_war_possibility_space__nuclear_taboo_reading, base_extractiveness, 20, 0.55).
narrative_ontology:measurement(nuclear_taboo_be_t30, total_war_possibility_space__nuclear_taboo_reading, base_extractiveness, 30, 0.62).
narrative_ontology:measurement(nuclear_taboo_be_t45, total_war_possibility_space__nuclear_taboo_reading, base_extractiveness, 45, 0.68).
narrative_ontology:measurement(nuclear_taboo_be_t60, total_war_possibility_space__nuclear_taboo_reading, base_extractiveness, 60, 0.72).
narrative_ontology:measurement(nuclear_taboo_be_t80, total_war_possibility_space__nuclear_taboo_reading, base_extractiveness, 80, 0.7).

% Suppression requirement over time
narrative_ontology:measurement(nuclear_taboo_su_t0, total_war_possibility_space__nuclear_taboo_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(nuclear_taboo_su_t10, total_war_possibility_space__nuclear_taboo_reading, suppression_requirement, 10, 0.45).
narrative_ontology:measurement(nuclear_taboo_su_t20, total_war_possibility_space__nuclear_taboo_reading, suppression_requirement, 20, 0.6).
narrative_ontology:measurement(nuclear_taboo_su_t30, total_war_possibility_space__nuclear_taboo_reading, suppression_requirement, 30, 0.68).
narrative_ontology:measurement(nuclear_taboo_su_t45, total_war_possibility_space__nuclear_taboo_reading, suppression_requirement, 45, 0.75).
narrative_ontology:measurement(nuclear_taboo_su_t60, total_war_possibility_space__nuclear_taboo_reading, suppression_requirement, 60, 0.78).
narrative_ontology:measurement(nuclear_taboo_su_t80, total_war_possibility_space__nuclear_taboo_reading, suppression_requirement, 80, 0.76).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
