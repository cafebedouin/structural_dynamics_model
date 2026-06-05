% ============================================================================
% CONSTRAINT STORY: british_constitution__revolution_settlement
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_british_constitution__revolution_settlement, []).

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
 *   constraint_id: british_constitution__revolution_settlement
 *   human_readable: The British Constitution Fixed by the Revolution Settlement (1688-1701)
 *   domain: political/legal/constitutional
 *
 * SUMMARY:
 *   The British constitution was settled by the Revolution of 1688 and the
 *   settlement legislation of 1689-1701, which subordinated the Crown to
 *   Parliament and statute law. This reading instantiates one understanding
 *   of the British constitutional kernel — the claim that a specific moment
 *   (1688-1701) fixed the constitution's operative structure: the supremacy
 *   of Parliament, the Protestant succession, the elimination of prerogative
 *   taxation as arbitrary exaction, and the subordination of royal will to
 *   legal constraint. The constraint exhibits the classic tangled-rope
 *   structure: it simultaneously solves a genuine coordination problem (how
 *   to prevent arbitrary executive power and civil war over succession) and
 *   concentrates extraction benefits on identifiable agents (Parliament, the
 *   Protestant succession, merchant interests) while suppressing alternatives
 *   (Catholic restoration, divine-right claims, Stuart pretenders). The
 *   settlement's ambiguous status — partly statute, partly convention, partly
 *   ceremonial prerogative retained-but-subordinated — creates persistent
 *   strategic space for sibling readings to claim that the 'real'
 *   constitution lives elsewhere (in conventions, in charters, in
 *   judicialization, in parliamentary supremacy statutes). This story
 *   instantiates the thesis that 1688-1701 is THE foundational moment,
 *   distinct from and in potential logical conflict with readings that locate
 *   the constitution in medieval charters or in modern judicial review.
 *
 * KEY AGENTS:
 *   - Parliament: Primary institutional beneficiary (institutional/arbitrage) — captures subordinated Crown authority, taxation powers, succession control
 *   - Protestant Succession (Hanoverians): Beneficiary (institutional/arbitrage) — secured against Catholic claimant restoration; constitutional legitimacy replaces dynastic claim
 *   - Merchant Capital and Landed Gentry: Secondary beneficiaries (powerful/mobile) — benefit from prerogative taxation suppression and property security via law; constrained by parliamentary levies but have exit options (arbitrage)
 *   - Catholic Claimants and Stuart Restoration: Primary victims (powerless/trapped) — permanently excluded from political order; their competing legitimacy claim is suppressed without exit
 *   - Divine-Right Theory and Absolute Monarchy Doctrine: Victim (structural position) — the theoretical justification for prerogative power is replaced by parliamentary supremacy; suppression enforced through official doctrine and legislation
 *   - The Common Subject: Ambiguous position (moderate/constrained) — may benefit from prerogative taxation suppression but subject to parliamentary exaction in its place; constrained by law but not trapped as Catholic claimants are
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(british_constitution__revolution_settlement, 0.38).
domain_priors:suppression_score(british_constitution__revolution_settlement, 0.72).
domain_priors:theater_ratio(british_constitution__revolution_settlement, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(british_constitution__revolution_settlement, extractiveness, 0.38).
narrative_ontology:constraint_metric(british_constitution__revolution_settlement, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(british_constitution__revolution_settlement, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(british_constitution__revolution_settlement, tangled_rope).
narrative_ontology:human_readable(british_constitution__revolution_settlement, "The British Constitution Fixed by the Revolution Settlement (1688-1701)").
narrative_ontology:topic_domain(british_constitution__revolution_settlement, "political/legal/constitutional").

domain_priors:requires_active_enforcement(british_constitution__revolution_settlement).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(british_constitution__revolution_settlement, '5703019a-8a6d-4641-b5d6-83325681b449').
narrative_ontology:cs_kernel_codification('5703019a-8a6d-4641-b5d6-83325681b449', formalized).
narrative_ontology:cs_authority_grounding('5703019a-8a6d-4641-b5d6-83325681b449', lineage).
narrative_ontology:cs_interpretation_layer_present('5703019a-8a6d-4641-b5d6-83325681b449').
narrative_ontology:cs_reading_relation('5703019a-8a6d-4641-b5d6-83325681b449', british_constitution__constitutional_conventions, coexists_with).
narrative_ontology:cs_reading_relation('5703019a-8a6d-4641-b5d6-83325681b449', british_constitution__foundational_charters, influences).
narrative_ontology:cs_reading_relation('5703019a-8a6d-4641-b5d6-83325681b449', british_constitution__modern_judicialization, coexists_with).
narrative_ontology:cs_reading_relation('5703019a-8a6d-4641-b5d6-83325681b449', british_constitution__parliamentary_supremacy_statutes, forecloses).
narrative_ontology:cs_axiom('5703019a-8a6d-4641-b5d6-83325681b449', foundational, revolution_fixed_parliamentary_subordination).
narrative_ontology:cs_axiom_status(revolution_fixed_parliamentary_subordination, holdable).
narrative_ontology:cs_axiom_grounding('5703019a-8a6d-4641-b5d6-83325681b449', revolution_fixed_parliamentary_subordination, deontological).
narrative_ontology:cs_axiom('5703019a-8a6d-4641-b5d6-83325681b449', foundational, prerogative_taxation_suppression_permanent).
narrative_ontology:cs_axiom_status(prerogative_taxation_suppression_permanent, holdable).
narrative_ontology:cs_axiom_grounding('5703019a-8a6d-4641-b5d6-83325681b449', prerogative_taxation_suppression_permanent, empirically_contingent).
narrative_ontology:cs_reference_frame('5703019a-8a6d-4641-b5d6-83325681b449', parliamentary_subordination_of_crown).
narrative_ontology:cs_drift_state('5703019a-8a6d-4641-b5d6-83325681b449', contemporary_constitutional_practice, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('5703019a-8a6d-4641-b5d6-83325681b449', '2026-02-26T00:00:00Z').
narrative_ontology:cs_kernel_id(british_constitution__revolution_settlement, british_constitution).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(british_constitution__revolution_settlement, parliament).
narrative_ontology:constraint_beneficiary(british_constitution__revolution_settlement, protestant_succession).
narrative_ontology:constraint_beneficiary(british_constitution__revolution_settlement, merchant_capital).
narrative_ontology:constraint_victim(british_constitution__revolution_settlement, stuart_absolutism).
narrative_ontology:constraint_victim(british_constitution__revolution_settlement, catholic_claimants).
narrative_ontology:constraint_victim(british_constitution__revolution_settlement, divine_right_theory).
narrative_ontology:constraint_victim(british_constitution__revolution_settlement, prerogative_taxation).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CATHOLIC CLAIMANTS & DIVINE-RIGHT THEORY (SNARE) — Structurally excluded from the political order and unable to exit the constraint. The settlement suppresses the alternative legitimacy claim (divine right, Catholic succession) with high enforcement. No coalition capacity; the trapped position is structural.
constraint_indexing:constraint_classification(british_constitution__revolution_settlement, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: ESTABLISHED CHURCH & SECTS (TANGLED ROPE) — Coordinate religious governance under the settlement but remain subject to partial exclusion (non-conformists), partial extraction (church lands, tithes), and periodic enforcement actions. Mixed position: genuine coordination function (religious stability) alongside significant suppression and extraction.
constraint_indexing:constraint_classification(british_constitution__revolution_settlement, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: PARLIAMENT & PROTESTANT SUCCESSION (ROPE) — Primary beneficiary (institutional/arbitrage). Experiences the settlement as coordinate mechanism solving collective action problems (monarch cannot arbitrarily tax, dissolve parliament without consent, alter succession). Net benefit captured through subordinated Crown authority. Can exit by renegotiating terms (arbitrage).
constraint_indexing:constraint_classification(british_constitution__revolution_settlement, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: LANDED GENTRY & MERCHANT CAPITAL (TANGLED ROPE) — Powerful actors (powerful/mobile) who benefit from reduced prerogative taxation and property security but remain subject to parliamentary exaction and tariff regulation. High mobility (can relocate capital, diversify holdings) but constrained by property obligations and parliamentary levies. Mixed extraction and coordination.
constraint_indexing:constraint_classification(british_constitution__revolution_settlement, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: PREROGATIVE POWERS REMNANT (PITON) — Formal prerogative authority persists in Crown exercised-by-advice-of-ministers, but the functional subordination to parliamentary consent is now the operative mechanism. Theater ratio high (0.58+): ceremonial Crown acts, formal prerogative doctrines, but actual power flow runs through Parliament and statute. Vestigial authority maintained through institutional inertia and constitutional convention.
constraint_indexing:constraint_classification(british_constitution__revolution_settlement, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational vantage, the subordination of absolute monarchy to parliamentary consent appears as an immutable structural outcome: any complex polity must eventually limit executive power through some representative body, or face instability. This perspective risks naturalizing what is actually a contingent settlement negotiated under specific historical conditions. The constraint carries identifiable beneficiaries (Parliament, Protestant succession, merchant capital) and suppression of alternatives, signaling a false summit.
constraint_indexing:constraint_classification(british_constitution__revolution_settlement, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(british_constitution__revolution_settlement_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(british_constitution__revolution_settlement, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(british_constitution__revolution_settlement, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(british_constitution__revolution_settlement, TR),
    TR >= 0.70.

:- end_tests(british_constitution__revolution_settlement_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The settlement does suppress arbitrary prerogative taxation, which is a genuine reduction in executive extraction from subjects. However, parliamentary levies may have equaled or exceeded prerogative rates historically (omega variable: prerogative_taxation_suppression_durability). The settlement also creates asymmetric benefits for Parliament and merchant interests relative to common subjects. The value reflects that the constraint solves a real coordination problem (preventing civil war, stabilizing succession) alongside concentrating political power. Suppression (0.72): High. The settlement requires active enforcement against alternative legitimacy claims (Stuart restoration plots, divine-right advocacy, Catholic succession). Enforcement includes military (the standing army), legislative (penal laws against Catholics), and doctrinal (parliamentary supremacy statutes). The suppression does not rise to snare levels because the alternative claimants have no coalition capacity and the settlement is widely accepted by beneficiaries. Theater ratio (0.58): Moderate-high. The settlement includes genuine functional components (parliamentary supremacy, Bill of Rights, Act of Settlement enacting specific rules) alongside performative elements (the retained prerogative exercised-by-ministerial-advice, ceremonial Crown acts, constitutional conventions that are binding without legal enforcement). Theater has increased over the interval (0.42 → 0.58) as the functional subordination became normalized and the prerogative became vestigial.
 *
 * PERSPECTIVAL GAP:
 *   The revolution settlement reading produces a perspectival gap between beneficiaries and victims. Parliament and the Protestant succession experience the settlement as coordinate mechanism (Rope) — it solves succession and taxation coordination problems. Catholic claimants and divine-right advocates experience it as pure suppression (Snare) — they are trapped without exit or voice. The merchant and landed interests experience tangled-rope: genuine benefit from prerogative taxation suppression but ongoing parliamentary extraction and constraints. The analytical observer risks seeing the settlement as a natural law (Mountain) — the structural necessity of limiting executive power — but the constraint's beneficiaries and suppression of alternatives flag it as a false summit. The piton perspective on residual prerogative reveals that formal authority persists but real power flows through Parliament and convention; the theater has increased over time as the functional subordination became normalized. No reading sees the settlement as pure coordination with zero asymmetry.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is derived from the structural positions declared in beneficiaries and victims. Parliament and the Protestant succession are beneficiaries with high exit capacity (arbitrage) — they can renegotiate the settlement's terms through their own legislative power. They experience low or negative effective extraction (d ≈ 0.15–0.20) and see the constraint as Rope. Catholic claimants are victims with no exit capacity (trapped) — they are permanently excluded and cannot renegotiate. They experience maximum extraction (d ≈ 0.95) and see the constraint as Snare. Merchant capital benefits from prerogative suppression but pays parliamentary levies; they are powerful with mobile capital (powerful/mobile), giving them constrained-to-moderate exit options and moderate extraction experienced (d ≈ 0.55). Common subjects face similar mixed extraction and coordination but with lower exit capacity (moderate/constrained), increasing their experienced extraction. The analytical observer's directionality is derived from the global scope and civilizational time horizon, which abstracts from particular beneficiary positions and risks naturalizing the settlement's contingency.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint avoids mandatrophy by declaring genuine coordination function (parliamentary stability, succession certainty, reduced arbitrary exaction) alongside suppression and beneficiary extraction. The tangled-rope classification holds the coordination and extraction in tension rather than collapsing them. The perspectival gap between Parliament (sees rope) and Catholic claimants (see snare) is real and structural, not analytical confusion. The piton perspective on residual prerogative reveals that formal authority persists but real power has migrated to Parliament and convention — the theater has increased as the functional subordination normalized, suggesting the piton is a genuine structural observation (degraded formal authority maintained by institutional inertia) rather than a category mistake.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    revolution_settlement_vs_magna_carta_primacy,
    'Is the 1688-1701 settlement the true foundational moment of British constitutional subordination of Crown to law, or is it a reaffirmation of principles already embedded in Magna Carta (1215) and the medieval charter tradition?',
    'Historical comparison of enforcement mechanisms: did medieval charters produce durable constraints on prerogative, or did they require re-assertion via the Revolution? Analysis of continuity vs rupture in 1688 texts and contemporary claims.',
    'If Magna Carta already fixed the subordination: this reading (revolution_settlement) becomes a piton or rope rather than tangled_rope — it recapitulates rather than establishes. If 1688 is genuinely novel: the tangled_rope classification holds, and the settlement is the true moment of extraction of rights from absolutism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(revolution_settlement_vs_magna_carta_primacy, empirical, 'Whether 1688-1701 establishes or reaffirms parliamentary subordination of Crown').

omega_variable(
    prerogative_taxation_suppression_durability,
    'Was the suppression of prerogative taxation (extraction prevented from subjects) durable, or did later parliamentary exaction simply replace royal prerogative as the extraction mechanism?',
    'Tax burden analysis across pre-settlement, settlement, and post-settlement periods; comparison of extraction rates on subject populations (merchant, landed, commoners) under royal prerogative vs parliamentary levy; longitudinal tracking of revenue sources and distribution.',
    'If prerogative suppression was durable: the settlement genuinely reduced extraction on the commons and benefited the broader polity (rope or balanced tangled_rope). If parliamentary taxation matched or exceeded prerogative rates: the settlement redistributed extraction from Crown to Parliament, maintaining suppression and confirming snare/tangled_rope for common subjects.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(prerogative_taxation_suppression_durability, empirical, 'Whether prerogative taxation suppression was durable or replaced by parliamentary extraction').

omega_variable(
    divine_right_foreclosure_logic,
    'Does the revolution settlement logically foreclose divine-right theory and Stuart restoration (making them incompatible with the settlement framework), or does it merely suppress them as live competing claims that coexist beneath the formal settlement?',
    'Textual analysis of settlement instruments (Bill of Rights 1689, Act of Settlement 1701): do they rule out divine right as a logical possibility, or merely enact parliamentary subordination as the operative law without denying divine right''s theoretical coherence? Historical tracking of divine-right advocacy and Stuart restoration attempts post-1701.',
    'If foreclosed: the reading''s core axiom (parliamentary_subordination_of_crown) logically rules out the sibling foundational_charters reading that frames constitution as negotiated extraction of liberty document-by-document. If merely suppressed: both readings coexist, with the settlement influencing but not eliminating the charter narrative.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(divine_right_foreclosure_logic, conceptual, 'Whether the settlement logically forecloses divine-right theory or merely suppresses it').

omega_variable(
    statute_vs_convention_binding_mechanism,
    'Is the settlement''s binding mechanism primarily statutory (enforceable law via courts) or conventional (binding by mutual agreement and repeated practice without legal enforcement)?',
    'Comparative analysis of settlement provisions: which are codified in statute (Bill of Rights, Act of Settlement) vs which rest on usage and expectation (ministerial responsibility, cabinet government, dissolution protocol)? Historical cases where each type was tested or broken.',
    'If primarily statutory: this reading supports parliamentary_supremacy_statutes sibling (the real text is statute). If primarily conventional: this reading coexists with constitutional_conventions sibling — both claim the settlement but locate its binding force differently.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(statute_vs_convention_binding_mechanism, empirical, 'Whether settlement''s binding mechanism is statutory or conventional').

omega_variable(
    false_summit_natural_law_risk,
    'Does the naturalizing of the settlement (mountain classification: ''any complex polity must limit executive power'') obscure the contingent historical achievement and mask the ongoing extraction of alternatives (suppression of Catholic succession, divine-right theory)?',
    'Counter-factual analysis: what would have to be true for divine-right monarchy or Catholic succession to remain live alternatives post-1701? What institutions, enforcement mechanisms, and narrative work maintain the settlement as ''natural law'' rather than contingent suppression?',
    'If natural law: the mountain classification holds, and the settlement is immutable structural necessity. If contingent: the false-summit signature fires, revealing that identified beneficiaries and suppression of alternatives mark this as tangled_rope, not mountain.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(false_summit_natural_law_risk, conceptual, 'Whether the settlement is natural constitutional necessity or contingent suppression naturalized').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(british_constitution__revolution_settlement, 0, 14).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(britcons_rev_theater_1688, british_constitution__revolution_settlement, theater_ratio, 0, 0.42).
narrative_ontology:measurement(britcons_rev_theater_mid_18c, british_constitution__revolution_settlement, theater_ratio, 7, 0.55).
narrative_ontology:measurement(britcons_rev_theater_late_18c, british_constitution__revolution_settlement, theater_ratio, 14, 0.58).

% Extraction over time
narrative_ontology:measurement(britcons_rev_extract_1688, british_constitution__revolution_settlement, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(britcons_rev_extract_1701, british_constitution__revolution_settlement, base_extractiveness, 3, 0.42).
narrative_ontology:measurement(britcons_rev_extract_mid_18c, british_constitution__revolution_settlement, base_extractiveness, 7, 0.38).
narrative_ontology:measurement(britcons_rev_extract_late_18c, british_constitution__revolution_settlement, base_extractiveness, 14, 0.35).

% Suppression requirement over time
narrative_ontology:measurement(britcons_rev_suppression_1688, british_constitution__revolution_settlement, suppression_requirement, 0, 0.85).
narrative_ontology:measurement(britcons_rev_suppression_1701, british_constitution__revolution_settlement, suppression_requirement, 3, 0.78).
narrative_ontology:measurement(britcons_rev_suppression_mid_18c, british_constitution__revolution_settlement, suppression_requirement, 7, 0.72).
narrative_ontology:measurement(britcons_rev_suppression_late_18c, british_constitution__revolution_settlement, suppression_requirement, 14, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(british_constitution__revolution_settlement, enforcement_mechanism).
narrative_ontology:affects_constraint(british_constitution__revolution_settlement, british_constitution__constitutional_conventions).
narrative_ontology:affects_constraint(british_constitution__revolution_settlement, british_constitution__foundational_charters).
narrative_ontology:affects_constraint(british_constitution__revolution_settlement, british_constitution__modern_judicialization).
narrative_ontology:affects_constraint(british_constitution__revolution_settlement, british_constitution__parliamentary_supremacy_statutes).

% DUAL FORMULATION NOTE:
% The british_constitution kernel decomposes into five constraint stories, each instantiating a different reading of what grounds British constitutional authority. This reading (revolution_settlement) claims that 1688-1701 fixed the operative structure. Sibling readings locate the constitution in conventions (unenforceable usage), medieval charters (document-by-document extraction of liberty), modern judicial review (1998 onwards), or perpetual parliamentary supremacy (no fixed founding moment). Each reading has its own ε value and beneficiary/victim structure because they make different claims about the binding mechanism. The network links show how readings influence (and in some cases foreclose) one another.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(british_constitution__revolution_settlement, institutional, 0.18).
constraint_indexing:directionality_override(british_constitution__revolution_settlement, powerless, 0.92).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
