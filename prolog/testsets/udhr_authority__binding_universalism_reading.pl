% ============================================================================
% CONSTRAINT STORY: udhr_authority__binding_universalism_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-27
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_udhr_authority__binding_universalism_reading, []).

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
 *   constraint_id: udhr_authority__binding_universalism_reading
 *   human_readable: UDHR Binding Universalism: Individual Rights Authority Over State Sovereignty
 *   domain: international_law/human_rights_doctrine/political_philosophy
 *
 * SUMMARY:
 *   The binding universalism reading of UDHR authority claims that individual
 *   human rights are justiciable entitlements enforceable against states
 *   regardless of state consent. This reading subordinates state sovereignty
 *   to an international rights regime adjudicated by tribunals with authority
 *   to interpret and enforce individual claims. The constraint exhibits mixed
 *   coordination and extraction dynamics: human rights protections do
 *   coordinate genuine protection mechanisms and rule-of-law functions, but
 *   these coexist with asymmetric extraction of state autonomy and
 *   subordination of self-determination to international authority
 *   structures. The binding universalism reading instantiates one
 *   interpretation of the contested UDHR kernel; sibling readings
 *   (aspirational sovereignty, customary emergence) would produce different ε
 *   values and different beneficiary/victim structures. This story models
 *   binding universalism as a tangled rope: real coordination function paired
 *   with structural extraction of state authority, requiring active
 *   enforcement via tribunal machinery and normative pressure, with
 *   significant suppression of alternative legitimacy claims.
 *
 * KEY AGENTS:
 *   - Individual Rights Claimants: Primary beneficiary (powerless/trapped) — formal rights-holders; actual enforcement is slow and expensive; dependent on tribunal machinery
 *   - International Human Rights Tribunals: Primary beneficiary (institutional/arbitrage) — gain jurisdictional authority, legitimacy, institutional expansion, and resource flows from binding universalism framework
 *   - Human Rights NGOs and Networks: Secondary beneficiary (organized/constrained) — coordinate individual protection while extracting legitimacy and funding from binding universalism rhetoric
 *   - States Signatories: Primary victim (institutional/constrained) — experience subordination of autonomous authority to rights regime; also gain legitimacy and coordination benefits from rule-of-law function
 *   - Authoritarian/Non-Signatory States: Primary victim (institutional/constrained) — experience pure extraction via normative pressure and isolation without benefit of coordination; constrained exit via sanctions threat
 *   - UN Institutional Apparatus: Secondary actor (institutional/arbitrage) — maintains performative compliance machinery with limited actual enforcement capacity
 *   - Analytical Observer: Civilizational context (analytical/analytical) — risks naturalizing contested institutional claim as universal natural law
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(udhr_authority__binding_universalism_reading, 0.58).
domain_priors:suppression_score(udhr_authority__binding_universalism_reading, 0.72).
domain_priors:theater_ratio(udhr_authority__binding_universalism_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(udhr_authority__binding_universalism_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(udhr_authority__binding_universalism_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(udhr_authority__binding_universalism_reading, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(udhr_authority__binding_universalism_reading, tangled_rope).
narrative_ontology:human_readable(udhr_authority__binding_universalism_reading, "UDHR Binding Universalism: Individual Rights Authority Over State Sovereignty").
narrative_ontology:topic_domain(udhr_authority__binding_universalism_reading, "international_law/human_rights_doctrine/political_philosophy").

domain_priors:requires_active_enforcement(udhr_authority__binding_universalism_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(udhr_authority__binding_universalism_reading, '619e892c-d7b7-4473-b6e9-b23f80ea4c82').
narrative_ontology:cs_kernel_codification('619e892c-d7b7-4473-b6e9-b23f80ea4c82', formalized).
narrative_ontology:cs_authority_grounding('619e892c-d7b7-4473-b6e9-b23f80ea4c82', extraction).
narrative_ontology:cs_interpretation_layer_present('619e892c-d7b7-4473-b6e9-b23f80ea4c82').
narrative_ontology:cs_reading_relation('619e892c-d7b7-4473-b6e9-b23f80ea4c82', udhr_authority__aspirational_sovereignty_reading, coexists_with).
narrative_ontology:cs_reading_relation('619e892c-d7b7-4473-b6e9-b23f80ea4c82', udhr_authority__customary_emergence_reading, influences).
narrative_ontology:cs_axiom('619e892c-d7b7-4473-b6e9-b23f80ea4c82', foundational, individual_rights_pre_political_universality).
narrative_ontology:cs_axiom_status(individual_rights_pre_political_universality, holdable).
narrative_ontology:cs_axiom_grounding('619e892c-d7b7-4473-b6e9-b23f80ea4c82', individual_rights_pre_political_universality, deontological).
narrative_ontology:cs_axiom('619e892c-d7b7-4473-b6e9-b23f80ea4c82', foundational, tribunal_interpretive_supremacy).
narrative_ontology:cs_axiom_status(tribunal_interpretive_supremacy, holdable).
narrative_ontology:cs_axiom_grounding('619e892c-d7b7-4473-b6e9-b23f80ea4c82', tribunal_interpretive_supremacy, conventional).
narrative_ontology:cs_reference_frame('619e892c-d7b7-4473-b6e9-b23f80ea4c82', universal_individual_rights_regime).
narrative_ontology:cs_drift_state('619e892c-d7b7-4473-b6e9-b23f80ea4c82', contemporary_state_resistance, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('619e892c-d7b7-4473-b6e9-b23f80ea4c82', '').
narrative_ontology:cs_kernel_id(udhr_authority__binding_universalism_reading, udhr_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(udhr_authority__binding_universalism_reading, individual_rights_claimants).
narrative_ontology:constraint_beneficiary(udhr_authority__binding_universalism_reading, international_human_rights_tribunals).
narrative_ontology:constraint_victim(udhr_authority__binding_universalism_reading, state_autonomous_authority).
narrative_ontology:constraint_victim(udhr_authority__binding_universalism_reading, national_self_determination_capacity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PERSECUTED INDIVIDUAL (SNARE) — Trapped within state jurisdiction with no unilateral exit. The UDHR binding universalism reading grants formal rights but enforcement depends on international tribunal machinery that is expensive, slow, and politically vulnerable. The individual experiences pure extraction: state retains coercive power; international remedy is aspirational. No realistic exit short of physical flight or asylum.
constraint_indexing:constraint_classification(udhr_authority__binding_universalism_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: HUMAN RIGHTS NGOS (TANGLED ROPE) — Organized agents that genuinely coordinate individual protection mechanisms (legal representation, documentation, asylum networks) while extracting benefit from the binding universalism framework (funding, institutional legitimacy, career pathways). Constrained by state sovereignty claims and resource limits; also gain from the rhetorical authority that binding universalism provides. Mixed coordination and extraction.
constraint_indexing:constraint_classification(udhr_authority__binding_universalism_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: INTERNATIONAL TRIBUNALS (ROPE) — Primary beneficiary. The binding universalism reading grants these institutions jurisdictional authority, legitimacy, and resource flows. They experience the constraint as pure coordination: their function is to adjudicate individual rights claims against states. Arbitrage options abound (legitimacy, budget justification, institutional expansion). Extraction flows toward tribunals, not away.
constraint_indexing:constraint_classification(udhr_authority__binding_universalism_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: STATE SIGNATORIES (TANGLED ROPE) — Constrained by treaty obligations and reputational pressure but retain exit option of withdrawal or non-compliance (high cost but possible). Experience binding universalism as subordination of their autonomous authority to individual rights regime. Also benefit from coordination: binding universalism provides legitimacy for domestic rule of law, attracts investment, and reduces interstate war risk. Moderate extraction with genuine but costly exit.
constraint_indexing:constraint_classification(udhr_authority__binding_universalism_reading, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: AUTHORITARIAN STATES (SNARE) — Non-signatories or nominally compliant states experience binding universalism as pure extraction: the framework creates international legitimacy for sanctions, interference, and regime-change rhetoric without providing benefits to state autonomy. Constrained exit (withdrawal is possible but triggers isolation, sanctions). High suppression via normative pressure, institutional isolation, and threat of intervention.
constraint_indexing:constraint_classification(udhr_authority__binding_universalism_reading, snare,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: UN INSTITUTIONAL APPARATUS (PITON) — The UN Human Rights apparatus (commission, council, special rapporteurs) is substantially performative: produces reports, holds sessions, and issues statements with minimal enforcement capacity. Maintains institutional legitimacy through theatrical compliance with binding universalism norms while actual enforcement depends on state willingness and great-power politics. Theater ratio high despite binding universalism rhetoric.
constraint_indexing:constraint_classification(udhr_authority__binding_universalism_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW (MOUNTAIN) — From a universal/civilizational perspective, the binding universalism reading frames individual human rights as inherent and inalienable — existing prior to and independent of state consent. This perspective risks treating the UDHR authority structure as a discovery of natural law rather than a contested institutional claim. The engine's false summit detector will flag this as naturalization: the 'universality' of rights is the contested core, not a settled foundation.
constraint_indexing:constraint_classification(udhr_authority__binding_universalism_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(udhr_authority__binding_universalism_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(udhr_authority__binding_universalism_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(udhr_authority__binding_universalism_reading, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(udhr_authority__binding_universalism_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(udhr_authority__binding_universalism_reading, TR),
    TR >= 0.70.

:- end_tests(udhr_authority__binding_universalism_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The binding universalism reading extracts significant authority from states (subordination of autonomous decision-making, exposure to international judgment, reputational vulnerability). However, this is not maximal extraction because legitimate coordination functions exist: rights protections do enable rule of law, reduce arbitrary violence, and provide legitimacy gains. The trajectory shows extractiveness rising from 0.42 (1945, post-WWII consensus, minimal enforcement machinery) to peak of 0.61 (1995, tribunal expansion) then slight decline to 0.58 (2020, state pushback and enforcement gaps). This reflects the accumulation phase (tribunals building capacity) followed by resistance phase (states building legitimacy counters). Suppression (0.72): High. Significant suppression mechanisms include: normative pressure on signatory states to comply, threat of international isolation and sanctions, reputational damage from tribunal judgments, institutional asymmetry (individuals cannot withdraw; states face high withdrawal cost). Suppression is structural and not declining despite rhetoric of consent — states face coercive normative environment. Theater ratio (0.48): Moderate. Unlike the UN apparatus piton perspective (which sees theater at 0.70+), the binding universalism reading itself has moderate theater because tribunal machinery has real (if limited) enforcement capacity. Judgments do produce state compliance in some cases; remedies are genuinely pursued. The theater is not as high as performative review but higher than full enforcement would produce. The rising trajectory (0.35 → 0.48) reflects institutionalization of the rights regime — more formal procedures, more reports, more ritualized compliance mechanisms.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates how institutional authority structures are experienced radically differently by beneficiaries, victims, and observers. The tribunal sees coordination — their role is to adjudicate rights claims, a genuine coordination function. The individual sees entrapment — formal rights that are expensive and slow to enforce against a coercive state apparatus. The state sees subordination — an external authority structure claiming power to judge state legitimacy. Authoritarian states see pure extraction — the normative pressure to comply with a regime they reject, with no benefit of participation. The UN apparatus sees degraded performance — producing judgments and reports with limited enforcement capacity (piton perspective). The analytical observer risks seeing natural law — treating the universality claim as discovered rather than constructed. The binding universalism reading's contribution is to foreclose the view that this is merely aspirational rhetoric — it asserts actual binding authority, which produces the snare and subordination experiences for different agents.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality varies sharply across perspectives because the constraint's extraction flow is highly asymmetric. Individual rights claimants (powerless/trapped) face maximum extraction with no exit — derived d ≈ 0.95. International tribunals (institutional/arbitrage) are pure beneficiaries with arbitrage options — derived d ≈ 0.05. States signatories (institutional/constrained) face moderate extraction with costly exit — derived d ≈ 0.62. Authoritarian states (institutional/constrained) with no willingness to sign face higher extraction (normative pressure, isolation) despite non-participation — derived d ≈ 0.80. The sigmoid f(d) maps these to experienced extractiveness values that vary by structural position. A tribunal sees minimal chi because d is low; a persecuted individual sees maximal chi because d is high. States see moderate chi because they have some exit capacity (withdraw, non-comply) even though exit is costly. This produces the perspectival gap: the same constraint is experienced as beneficent coordination (tribunal view), entrapment (individual view), and constrained subordination (state view).
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    consent_threshold_ambiguity,
    'Does binding universalism require affirmative state consent, or does non-contradiction constitute consent? Where is the consent threshold?',
    'Doctrinal analysis: compare Vienna Convention text interpretation, state signature/ratification patterns, and tribunal interpretation of consent requirement. Historical analysis: track evolution of ''consent'' standard across decades of human rights jurisprudence.',
    'If high consent threshold: binding universalism is contractual coordination (Rope); states retain meaningful exit. If low threshold (non-contradiction=consent): binding universalism is unilateral extraction mechanism (Snare for states). Classification shifts across threshold.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(consent_threshold_ambiguity, conceptual, 'Ambiguity in what constitutes valid state consent to binding rights regime').

omega_variable(
    tribunal_coercive_capacity_gap,
    'How much of the binding universalism extraction is structural vs. performative? Can tribunals actually enforce judgments against powerful states?',
    'Empirical compliance audit: measure percentage of tribunal judgments actually enforced (full compliance, partial, none) disaggregated by state power level. Track sanction credibility over time.',
    'If high enforcement rate: binding universalism is structurally enforced (high χ, genuine snare/tangled_rope). If low: largely performative (piton). Classification and mandatrophy resolution depend on actual coercive capacity.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(tribunal_coercive_capacity_gap, empirical, 'Actual enforcement capacity of international human rights tribunals').

omega_variable(
    sovereignty_subordination_reading_contest,
    'Is binding universalism subordinating state sovereignty or merely constraining it? Is the constraint forced from outside or negotiated from within?',
    'This is the core omega flagging the kernel contest itself. Different readings produce different answers. The binding universalism reading (this one) assumes subordination; the aspirational sovereignty reading assumes constraint is negotiated; the customary emergence reading assumes constraints emerge from state practice itself.',
    'Resolving this omega means choosing a reading. The engine cannot resolve this — it is a structural choice about which reading''s axioms you hold.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sovereignty_subordination_reading_contest, conceptual, 'Whether binding universalism subordinates or negotiates state sovereignty').

omega_variable(
    natural_law_vs_institutional_claim,
    'Is the universality of human rights a discovery of natural law or a contested institutional claim grounded in post-WWII power politics?',
    'This omega documents the false summit risk: the binding universalism reading risks naturalizing what are actually contingent choices about authority grounding. Post-WWII context (colonial power decline, universal suffrage rhetoric, Cold War legitimacy competition) shaped UDHR. Alternative readings would emphasize institutional construction rather than natural law discovery.',
    'If natural law: binding universalism is mountain-like, unchallengeable, inevitable. If institutional claim: contingent, revisable, subject to power dynamics. This gap is where the oracle gap (Theorem 4) appears — single-position analysis cannot see the contestation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(natural_law_vs_institutional_claim, conceptual, 'Whether universal rights claims are natural law or contested institutional construction').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(udhr_authority__binding_universalism_reading, 0, 75).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(udhr_bu_theater_1945, udhr_authority__binding_universalism_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement(udhr_bu_theater_1970, udhr_authority__binding_universalism_reading, theater_ratio, 25, 0.42).
narrative_ontology:measurement(udhr_bu_theater_1995, udhr_authority__binding_universalism_reading, theater_ratio, 50, 0.51).
narrative_ontology:measurement(udhr_bu_theater_2020, udhr_authority__binding_universalism_reading, theater_ratio, 75, 0.48).

% Extraction over time
narrative_ontology:measurement(udhr_bu_extract_1945, udhr_authority__binding_universalism_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(udhr_bu_extract_1970, udhr_authority__binding_universalism_reading, base_extractiveness, 25, 0.55).
narrative_ontology:measurement(udhr_bu_extract_1995, udhr_authority__binding_universalism_reading, base_extractiveness, 50, 0.61).
narrative_ontology:measurement(udhr_bu_extract_2020, udhr_authority__binding_universalism_reading, base_extractiveness, 75, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(udhr_bu_supp_1945, udhr_authority__binding_universalism_reading, suppression_requirement, 0, 0.65).
narrative_ontology:measurement(udhr_bu_supp_1970, udhr_authority__binding_universalism_reading, suppression_requirement, 25, 0.7).
narrative_ontology:measurement(udhr_bu_supp_1995, udhr_authority__binding_universalism_reading, suppression_requirement, 50, 0.75).
narrative_ontology:measurement(udhr_bu_supp_2020, udhr_authority__binding_universalism_reading, suppression_requirement, 75, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(udhr_authority__binding_universalism_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(udhr_authority__binding_universalism_reading, 0.12).
narrative_ontology:affects_constraint(udhr_authority__binding_universalism_reading, udhr_authority__aspirational_sovereignty_reading).
narrative_ontology:affects_constraint(udhr_authority__binding_universalism_reading, udhr_authority__customary_emergence_reading).
narrative_ontology:affects_constraint(udhr_authority__binding_universalism_reading, international_tribunal_jurisdiction__coercive_authority).
narrative_ontology:affects_constraint(udhr_authority__binding_universalism_reading, state_sovereignty__subordination_to_rights_regime).

% DUAL FORMULATION NOTE:
% UDHR authority is a contested kernel with three structurally distinct readings. Each produces different ε values and different classification patterns. The binding universalism reading (this constraint) asserts direct tribunal authority subordinating state sovereignty — ε=0.58, Tangled Rope primary. The aspirational sovereignty reading asserts negotiated norm internalization — produces lower ε and different beneficiary structure (states as gradual beneficiaries). The customary emergence reading asserts state-practice-based authority — produces state-led emergence narrative with different victim set. All three are linked via network.affects_constraints because they model competing interpretations of the same institutional kernel and produce downstream effects on tribunal authority and state subordination constraints. This is a constraint family: the ε-invariance principle requires separate stories because the observable used to evaluate binding universalism (tribunal coercive capacity) versus aspirational norms (state internalization rate) produces different base extractiveness values.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(udhr_authority__binding_universalism_reading, institutional, 0.78).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
