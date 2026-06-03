% ============================================================================
% CONSTRAINT STORY: territorial_legitimacy_dual__two_state_coexistence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_territorial_legitimacy_dual__two_state_coexistence_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: territorial_legitimacy_dual__two_state_coexistence_reading
 *   human_readable: Two-State Coexistence via Mutual Recognition and 1967 Boundaries
 *   domain: political_theory/international_relations/territorial_sovereignty
 *
 * SUMMARY:
 *   The two-state coexistence reading instantiates one logical solution to
 *   the territorial legitimacy contest: mutual recognition of dual legitimacy
 *   with 1967 boundaries as the operative partition framework, bounded right
 *   of return, and security cooperation replacing zero-sum competition. This
 *   reading is ONE among three structurally distinct positions on the same
 *   kernel (territorial_legitimacy_dual). Unlike the Palestinian autochthony
 *   reading (which grounds legitimacy in continuous habitation and
 *   displacement trauma) or the Zionist refuge reading (which grounds
 *   legitimacy in historical persecution and UN partition acceptance), the
 *   two-state reading treats both legitimacy claims as valid but mutually
 *   constraining — accepting 1948 as the foundational partition moment for
 *   both peoples while using 1967 boundaries as the operational demarcation.
 *   The reading produces a tangled_rope constraint: it coordinates genuine
 *   partition acceptance while extracting compliance from those who reject
 *   bounded return (diaspora claimants), absolute security (Israeli
 *   communities), or partition itself (binational advocates). The
 *   theater_ratio trajectory (0.42 → 0.58) reflects increasing reliance on
 *   performative diplomatic rituals as implementation pressures accumulate —
 *   what begins as a negotiated framework increasingly depends on managed
 *   symbolism to mask non-compliance.
 *
 * KEY AGENTS:
 *   - Palestinian Diaspora Right-of-Return Claimants: Primary victims (powerless/trapped) — bear full cost of bounded return clause; have no exit and no structural advocate within the reading's framework
 *   - Israeli Security Communities: Secondary victims (moderate/constrained) — face ongoing vulnerability; security cooperation requires trust in compliance that historical patterns undermine
 *   - Israeli Settler Security Apparatus: Primary beneficiary (institutional/arbitrage) — gains territory, legitimacy, and international recognition; can exit to unilateral security doctrine while reading remains marginalized
 *   - Palestinian State Administrative Authority: Primary beneficiary (institutional/arbitrage) — gains sovereignty, state apparatus, and international legitimacy; can exit to unilateral state claims while reading remains marginalized
 *   - International Recognition Regime: Institutional orchestrator (powerful/mobile) — enforces boundaries and recognition; benefits from order but can selectively withdraw support
 *   - Binational Coexistence Advocates: Identity-locked victims (moderate/identity_locked) — their fundamental normative commitment (rejection of partition) is structurally foreclosed by the reading's foundational premise
 *   - Analytical Observer: Civilizational view (analytical/analytical) — observes the constraint's hybrid coordination-extraction structure and rising theater dependency
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(territorial_legitimacy_dual__two_state_coexistence_reading, 0.55).
domain_priors:suppression_score(territorial_legitimacy_dual__two_state_coexistence_reading, 0.62).
domain_priors:theater_ratio(territorial_legitimacy_dual__two_state_coexistence_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(territorial_legitimacy_dual__two_state_coexistence_reading, extractiveness, 0.55).
narrative_ontology:constraint_metric(territorial_legitimacy_dual__two_state_coexistence_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(territorial_legitimacy_dual__two_state_coexistence_reading, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(territorial_legitimacy_dual__two_state_coexistence_reading, tangled_rope).
narrative_ontology:human_readable(territorial_legitimacy_dual__two_state_coexistence_reading, "Two-State Coexistence via Mutual Recognition and 1967 Boundaries").
narrative_ontology:topic_domain(territorial_legitimacy_dual__two_state_coexistence_reading, "political_theory/international_relations/territorial_sovereignty").

domain_priors:requires_active_enforcement(territorial_legitimacy_dual__two_state_coexistence_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(territorial_legitimacy_dual__two_state_coexistence_reading, 'ee73c8f3-997d-4d51-b4d0-2b9ba47c7d15').
narrative_ontology:cs_kernel_codification('ee73c8f3-997d-4d51-b4d0-2b9ba47c7d15', formalized).
narrative_ontology:cs_authority_grounding('ee73c8f3-997d-4d51-b4d0-2b9ba47c7d15', distributed).
narrative_ontology:cs_reading_relation('ee73c8f3-997d-4d51-b4d0-2b9ba47c7d15', territorial_legitimacy_dual__palestinian_autochthony_reading, coexists_with).
narrative_ontology:cs_reading_relation('ee73c8f3-997d-4d51-b4d0-2b9ba47c7d15', territorial_legitimacy_dual__zionist_refuge_reading, coexists_with).
narrative_ontology:cs_axiom('ee73c8f3-997d-4d51-b4d0-2b9ba47c7d15', foundational, dual_legitimacy_mutual_recognition).
narrative_ontology:cs_axiom_status(dual_legitimacy_mutual_recognition, holdable).
narrative_ontology:cs_axiom_grounding('ee73c8f3-997d-4d51-b4d0-2b9ba47c7d15', dual_legitimacy_mutual_recognition, conventional).
narrative_ontology:cs_axiom('ee73c8f3-997d-4d51-b4d0-2b9ba47c7d15', foundational, bounded_return_partition_constraint).
narrative_ontology:cs_axiom_status(bounded_return_partition_constraint, holdable).
narrative_ontology:cs_axiom_grounding('ee73c8f3-997d-4d51-b4d0-2b9ba47c7d15', bounded_return_partition_constraint, instrumental).
narrative_ontology:cs_reference_frame('ee73c8f3-997d-4d51-b4d0-2b9ba47c7d15', partition_as_legitimacy_framework).
narrative_ontology:cs_drift_state('ee73c8f3-997d-4d51-b4d0-2b9ba47c7d15', contemporary_implementation_crisis, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('ee73c8f3-997d-4d51-b4d0-2b9ba47c7d15', '2026-02-26T14:32:00Z').
narrative_ontology:cs_kernel_id(territorial_legitimacy_dual__two_state_coexistence_reading, territorial_legitimacy_dual).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(territorial_legitimacy_dual__two_state_coexistence_reading, israeli_settler_security_apparatus).
narrative_ontology:constraint_beneficiary(territorial_legitimacy_dual__two_state_coexistence_reading, palestinian_state_administrative_authority).
narrative_ontology:constraint_victim(territorial_legitimacy_dual__two_state_coexistence_reading, palestinian_diaspora_right_of_return_claimants).
narrative_ontology:constraint_victim(territorial_legitimacy_dual__two_state_coexistence_reading, israeli_security_sacrifice_communities).
narrative_ontology:constraint_victim(territorial_legitimacy_dual__two_state_coexistence_reading, binational_coexistence_advocates).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PALESTINIAN DIASPORA CLAIMANT (SNARE) — Trapped by the reading's core compromise: legitimacy is mutual recognition WITH bounded return. The claimant has no exit option and no advocate within the framework. The 1967 boundary reading forecloses unlimited right of return — the reading's foundational premise (mutual recognition + partition) inherently denies this group's structural claim.
constraint_indexing:constraint_classification(territorial_legitimacy_dual__two_state_coexistence_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: ISRAELI SECURITY COMMUNITIES (SNARE) — Face constrained exit but high suppression. The reading frames their security need as legitimate but subordinates it to mutual recognition and bounded territorial concession. Significant extraction: security cooperation requires ongoing vulnerability to the other party's compliance.
constraint_indexing:constraint_classification(territorial_legitimacy_dual__two_state_coexistence_reading, snare,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: ISRAELI SETTLER SECURITY APPARATUS (ROPE) — Primary beneficiary with arbitrage options. Gains legitimacy, territorial control, and international recognition within the reading's framework. Can exit to unilateral security arrangements while the framework remains marginalized. Net coordination function: security cooperation framework that benefits institutional actors.
constraint_indexing:constraint_classification(territorial_legitimacy_dual__two_state_coexistence_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 4: PALESTINIAN STATE ADMINISTRATIVE AUTHORITY (ROPE) — Primary beneficiary with arbitrage options. Gains sovereignty, territorial legitimacy, and state apparatus authority within the reading's framework. Can exit to unilateral state claims while the reading remains marginalized. Net coordination function: state partition framework that benefits institutional actors.
constraint_indexing:constraint_classification(territorial_legitimacy_dual__two_state_coexistence_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 5: INTERNATIONAL RECOGNITION REGIME (TANGLED ROPE) — Powerful actors (UN, regional states, global powers) experience the reading as mixed coordination and extraction. The framework coordinates territorial partition acceptance while extracting compliance with externally-imposed boundaries. Powerful agents can exercise mobile exit (selective recognition, conditional support) but are also bound by the recognition logic they enforce. Moderate effective extraction due to power and mobility.
constraint_indexing:constraint_classification(territorial_legitimacy_dual__two_state_coexistence_reading, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: BINATIONAL COEXISTENCE ADVOCATES (SNARE) — Identity-locked victims of the reading's own foundational premise. These agents are constitutionally committed to rejecting partition itself, seeing coexistence without bounded territory as the legitimate framework. The reading forecloses their core identity commitment. Trapped not by material barriers but by the reading's structural negation of their entire normative framework. High suppression: the reading treats their position as foreclosed, not negotiable.
constraint_indexing:constraint_classification(territorial_legitimacy_dual__two_state_coexistence_reading, snare,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(identity_locked),
            spatial_scope(regional))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — The reading produces a genuine hybrid: it coordinates legitimate dual recognition (coordination function) while enforcing bounded return and territorial partition against competing claims (asymmetric extraction). From a civilizational analytical view, the reading contains both elements. Theater ratio is moderate (0.58) because the framework's enforcement mechanisms (international monitoring, border control, selective recognition) are partly genuine structural requirements and partly performative diplomatic ritual.
constraint_indexing:constraint_classification(territorial_legitimacy_dual__two_state_coexistence_reading, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(territorial_legitimacy_dual__two_state_coexistence_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(territorial_legitimacy_dual__two_state_coexistence_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(territorial_legitimacy_dual__two_state_coexistence_reading, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(territorial_legitimacy_dual__two_state_coexistence_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(territorial_legitimacy_dual__two_state_coexistence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.55, trajectory 0.35→0.55): The reading's extraction rises over time as implementation pressures mount. Initial extractiveness is low (0.35) when treated as a negotiation framework — the compromise offers something to each side. Over time, extractiveness rises as the framework reveals asymmetries: right of return cannot be fully honored, security cooperation produces ongoing risk for one party, and enforcement mechanisms favor institutional beneficiaries. The reading never reaches Snare levels because the institutional beneficiaries genuinely benefit and participate in maintenance. Suppression (0.62): Moderate-high. The reading requires active suppression of three alternatives: unlimited right of return (suppressed via bounded clause), absolute Israeli security (suppressed via security cooperation requirement), and binational coexistence (suppressed via partition foundationalism). Suppression rises from 0.48 to 0.62 as enforcement machinery builds to sustain the framework against defection. Theater ratio (0.58, trajectory 0.42→0.58): Moderate-high and rising. The reading's implementation increasingly depends on performative diplomacy — managed recognition rituals, symbolic boundary crossings, confidence-building theater — to mask non-compliance and mutual defection risks. As the structural incentives for defection strengthen, theatrical maintenance intensifies.
 *
 * PERSPECTIVAL GAP:
 *   The reading's core perspectival gap lies in the asymmetry between institutional beneficiaries' experience (Rope) and powerless agents' experience (Snare). Both groups participate in the same constraint, but their structural positions produce opposite classifications. This gap reflects the reading's true nature: it is not a cooperative solution but an enforced redistribution where institutional actors gain legitimacy while powerless claimants lose claims. The binational advocates' identity-lock perspective adds a second gap: their position is not merely constrained but foreclosed, making their experienced constraint different in kind from the others' constraints.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values are derived from beneficiary/victim declarations and exit_options. Palestinian diaspora claimants are identified as victims with trapped exit (no structural alternatives) → d ≈ 0.95 → high experienced extraction. Israeli settler security apparatus are identified as beneficiaries with arbitrage exit (can opt for unilateral security doctrine) → d ≈ 0.05 → negative or minimal effective extraction. Binational advocates are victims with identity_locked exit (structurally mobile but identity-fused with rejection of partition itself) → d ≈ 0.89 → high experienced extraction. The international regime is a powerful player with mobile exit (selective recognition) → d ≈ 0.55 → moderate extraction. The analytical observer uses canonical d for analytical power ≈ 0.72 → moderate-high apparent extraction from observer perspective.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    right_of_return_threshold_ambiguity,
    'What constitutes ''bounded right of return'' — is it a structural limit (limited compensation fund, phased resettlement numbers) or a foundational denial (return categorically impossible for diaspora majority)?',
    'Comparative analysis of historical partition precedent frameworks (India-Pakistan, Cyprus, Korea); assessment of whether proposed return mechanisms are structurally achievable or performative',
    'If threshold is structural/phased: extraction is real but negotiable (Tangled Rope from diaspora perspective). If return is foreclosed: extraction is pure negation, foreclosing legitimate historical claim (Snare, potentially with foreclosure of Palestinian autochthony reading).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(right_of_return_threshold_ambiguity, conceptual, 'Whether right of return is structurally limited or foundationally denied').

omega_variable(
    id_1967_boundaries_naturalness,
    'Are 1967 boundaries treated as a natural consequence of partition logic or as an arbitrary imperial-era demarcation? Is the reading dependent on accepting Green Line legitimacy as foundational?',
    'Discourse analysis of how the reading justifies 1967 selection (UN resolutions, demographic distribution, colonial cartography vs. organic nationalist boundaries); comparison with alternative territorial formulations',
    'If 1967 is treated as natural/inevitable: the reading appears mathematically determined. If 1967 is arbitrary choice: the reading''s authority is contested, opening space for alternative partitions. This affects whether the reading forecloses alternatives or merely coexists.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(id_1967_boundaries_naturalness, conceptual, 'Whether 1967 boundaries are natural or contingent to the reading').

omega_variable(
    mutual_recognition_symmetry,
    'Does the reading require symmetric mutual recognition (both parties recognize each other''s legitimacy) or asymmetric? Can one party recognize the other without reciprocation?',
    'Formalization of the recognition conditions; historical cases of one-sided recognition; analysis of whether the framework can sustain asymmetric acceptance',
    'If symmetric: the framework forecloses unilateral recognition claims (affects zionist_refuge_reading). If asymmetric: both readings can coexist, each recognizing the other selectively. High impact on reading_relations classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mutual_recognition_symmetry, conceptual, 'Whether mutual recognition must be symmetric').

omega_variable(
    security_cooperation_enforcement,
    'How is security cooperation to be enforced against defection — through credible mutual vulnerability, external guarantor power, or institutional lock-in? What prevents collapse into zero-sum competition?',
    'Game-theoretic analysis of incentive structures; historical precedent of comparable security arrangements; assessment of whether mechanisms prevent unilateral advantage-taking',
    'If enforcement is credible: security cooperation is a genuine coordination function (Rope elements). If enforcement is aspirational: security cooperation is performative, and extraction risk dominates (Snare for security-dependent agents). Affects theater_ratio measurement.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(security_cooperation_enforcement, empirical, 'Credibility of security cooperation enforcement mechanisms').

omega_variable(
    kernel_contest_foreclosure_structure,
    'Does the two-state coexistence reading logically foreclose either the Palestinian autochthony reading or the Zionist refuge reading, or do all three readings remain live positions held by different parties?',
    'Formal analysis of the axioms each reading rests on; identification of whether any reading''s foundational premise directly contradicts another''s; assessment of whether coexistence is logical or merely factual (different people holding incompatible views)',
    'If foreclosing: the reading_relations use ''forecloses''. If coexisting: use ''coexists_with''. If influential: use ''influences''. The structure of relations determines the kernel''s stability and whether the reading can claim to resolve the contest.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_contest_foreclosure_structure, conceptual, 'Logical structure of reading_relations within the territorial legitimacy kernel').

omega_variable(
    suppression_mechanism_structural_vs_performative,
    'Is suppression of alternative readings (binational coexistence, unlimited return, unified state) enforced by structural barriers (enforcement institutions, military capacity, resource control) or by performative legitimacy denial?',
    'Analysis of enforcement mechanisms for each suppressed position; assessment of whether alternatives are materially impossible or merely delegitimized; historical trajectory of suppression intensity',
    'If structural: suppression is 0.62 as measured, enforced by state apparatus and international regime. If performative: suppression may be lower than measured, as alternatives could mobilize if delegitimacy fell. Affects theater_ratio and mandatrophy analysis.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_performative, empirical, 'Whether suppression of alternatives is structural or performative').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(territorial_legitimacy_dual__two_state_coexistence_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(terr_legit_dual_two_state_tr_t0, territorial_legitimacy_dual__two_state_coexistence_reading, theater_ratio, 0, 0.42).
narrative_ontology:measurement(terr_legit_dual_two_state_tr_t10, territorial_legitimacy_dual__two_state_coexistence_reading, theater_ratio, 10, 0.52).
narrative_ontology:measurement(terr_legit_dual_two_state_tr_t20, territorial_legitimacy_dual__two_state_coexistence_reading, theater_ratio, 20, 0.58).

% Extraction over time
narrative_ontology:measurement(terr_legit_dual_two_state_be_t0, territorial_legitimacy_dual__two_state_coexistence_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(terr_legit_dual_two_state_be_t10, territorial_legitimacy_dual__two_state_coexistence_reading, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(terr_legit_dual_two_state_be_t20, territorial_legitimacy_dual__two_state_coexistence_reading, base_extractiveness, 20, 0.55).

% Suppression requirement over time
narrative_ontology:measurement(terr_legit_dual_two_state_su_t0, territorial_legitimacy_dual__two_state_coexistence_reading, suppression_requirement, 0, 0.48).
narrative_ontology:measurement(terr_legit_dual_two_state_su_t10, territorial_legitimacy_dual__two_state_coexistence_reading, suppression_requirement, 10, 0.58).
narrative_ontology:measurement(terr_legit_dual_two_state_su_t20, territorial_legitimacy_dual__two_state_coexistence_reading, suppression_requirement, 20, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(territorial_legitimacy_dual__two_state_coexistence_reading, identity_coordination).
narrative_ontology:affects_constraint(territorial_legitimacy_dual__two_state_coexistence_reading, territorial_legitimacy_dual__palestinian_autochthony_reading).
narrative_ontology:affects_constraint(territorial_legitimacy_dual__two_state_coexistence_reading, territorial_legitimacy_dual__zionist_refuge_reading).
narrative_ontology:affects_constraint(territorial_legitimacy_dual__two_state_coexistence_reading, security_cooperation_credibility__middle_east).

% DUAL FORMULATION NOTE:
% This constraint is one reading of a three-way kernel contest. The sibling readings (Palestinian autochthony and Zionist refuge) are separate constraint stories with different ε values, different beneficiary/victim structures, and different classification patterns. All three are linked via network.affects_constraints to show they are competing instantiations of the same contested kernel. The two-state reading has ε=0.55 (Tangled Rope); expect the autochthony reading to have higher ε (Snare from Israeli perspective) and the refuge reading to have lower ε (Rope from Zionist perspective). The asymmetry in ε values reflects the structural reality: each reading benefits certain parties and harms others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
