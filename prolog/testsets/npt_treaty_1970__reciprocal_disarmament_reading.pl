% ============================================================================
% CONSTRAINT STORY: npt_treaty_1970__reciprocal_disarmament_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_npt_treaty_1970__reciprocal_disarmament_reading, []).

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
 *   constraint_id: npt_treaty_1970__reciprocal_disarmament_reading
 *   human_readable: NPT Article VI Reciprocal Disarmament Obligation (1970)
 *   domain: international_law/nuclear_regime/reciprocal_bargain
 *
 * SUMMARY:
 *   The NPT Article VI reciprocal disarmament reading frames the Nuclear
 *   Nonproliferation Treaty as a binding reciprocal bargain in which
 *   Non-Nuclear-Weapon States (NNWS) commit to permanent non-weapons status
 *   in exchange for the Nuclear-Weapon States' (NWS) commitment to pursue
 *   nuclear disarmament 'in good faith.' This reading treats Article VI as a
 *   genuine obligation with legal force, not a performative declaration.
 *   Under this interpretation, the 55-year noncompliance by NWS with Article
 *   VI while NNWS honor their commitments constitutes structural injustice
 *   and extraction: the constraint coordinates proliferation prevention
 *   (benefiting NWS security and global stability) while the NWS extract
 *   permanent non-weapons status from NNWS without reciprocal disarmament.
 *   The reading is contested by two sibling readings: the oligopoly
 *   enforcement reading (treating NPT as NWS cartel coordination, Article VI
 *   as incidental) and the withdrawal sovereignty reading (treating NPT as a
 *   revocable contract, Article VI as non-binding on withdrawing states).
 *   This constraint is one of the three.
 *
 * KEY AGENTS:
 *   - Non-Nuclear-Weapon States (NNWS): Primary victim (powerless/trapped) — bear commitment to permanent non-weapons status; receive unfulfilled reciprocal disarmament obligation
 *   - Non-Aligned Movement (NAM) Coalition: Organized victim (organized/constrained) — leverages coalitional power to press disarmament claims; constrained by security interdependencies and technology access
 *   - Nuclear-Weapon States (NWS/P5): Primary beneficiary (institutional/arbitrage) — maintain strategic autonomy via Article VI reinterpretation as indefinite negotiation; coordinate NNWS restraint
 *   - IAEA & NPT Review Conference System: Institutional actor (institutional/constrained) — administers NNWS-side verification machinery; maintains theater through procedural compliance while Article VI verification remains absent
 *   - Treaty on Prohibition of Nuclear Weapons (TPNW) Coalition: Organized exit pathway (organized/constrained) — building alternate regime with sunset logic; represents NNWS attempt to force Article VI implementation or supersede NPT
 *   - Analytical Observer (International Law): Identity-locked (analytical/identity_locked) — perceives binding obligation textually but structurally trapped in legal positivism that cannot detect oligarchic power distribution
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(npt_treaty_1970__reciprocal_disarmament_reading, 0.58).
domain_priors:suppression_score(npt_treaty_1970__reciprocal_disarmament_reading, 0.62).
domain_priors:theater_ratio(npt_treaty_1970__reciprocal_disarmament_reading, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(npt_treaty_1970__reciprocal_disarmament_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(npt_treaty_1970__reciprocal_disarmament_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(npt_treaty_1970__reciprocal_disarmament_reading, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(npt_treaty_1970__reciprocal_disarmament_reading, tangled_rope).
narrative_ontology:human_readable(npt_treaty_1970__reciprocal_disarmament_reading, "NPT Article VI Reciprocal Disarmament Obligation (1970)").
narrative_ontology:topic_domain(npt_treaty_1970__reciprocal_disarmament_reading, "international_law/nuclear_regime/reciprocal_bargain").

domain_priors:requires_active_enforcement(npt_treaty_1970__reciprocal_disarmament_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(npt_treaty_1970__reciprocal_disarmament_reading, 'kernel-npt-reciprocal-disarmament-2026').
narrative_ontology:cs_kernel_codification('kernel-npt-reciprocal-disarmament-2026', formalized).
narrative_ontology:cs_authority_grounding('kernel-npt-reciprocal-disarmament-2026', extraction).
narrative_ontology:cs_interpretation_layer_present('kernel-npt-reciprocal-disarmament-2026').
narrative_ontology:cs_reading_relation('kernel-npt-reciprocal-disarmament-2026', npt_treaty_1970__oligopoly_enforcement_reading, forecloses).
narrative_ontology:cs_reading_relation('kernel-npt-reciprocal-disarmament-2026', npt_treaty_1970__withdrawal_sovereignty_reading, coexists_with).
narrative_ontology:cs_axiom('kernel-npt-reciprocal-disarmament-2026', foundational, article_vi_binding_disarmament_obligation).
narrative_ontology:cs_axiom_status(article_vi_binding_disarmament_obligation, holdable).
narrative_ontology:cs_axiom_grounding('kernel-npt-reciprocal-disarmament-2026', article_vi_binding_disarmament_obligation, deontological).
narrative_ontology:cs_axiom('kernel-npt-reciprocal-disarmament-2026', foundational, npt_reciprocal_bargain_structure).
narrative_ontology:cs_axiom_status(npt_reciprocal_bargain_structure, holdable).
narrative_ontology:cs_axiom_grounding('kernel-npt-reciprocal-disarmament-2026', npt_reciprocal_bargain_structure, deontological).
narrative_ontology:cs_reference_frame('kernel-npt-reciprocal-disarmament-2026', symmetrical_reciprocal_disarmament_regime).
narrative_ontology:cs_drift_state('kernel-npt-reciprocal-disarmament-2026', contemporary_2026, gap(axiom_overriding, severe, false)).
narrative_ontology:cs_created_at('kernel-npt-reciprocal-disarmament-2026', '').
narrative_ontology:cs_kernel_id(npt_treaty_1970__reciprocal_disarmament_reading, npt_treaty_1970).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(npt_treaty_1970__reciprocal_disarmament_reading, nnws_coalition).
narrative_ontology:constraint_beneficiary(npt_treaty_1970__reciprocal_disarmament_reading, non_aligned_movement).
narrative_ontology:constraint_victim(npt_treaty_1970__reciprocal_disarmament_reading, nws_strategic_autonomy).
narrative_ontology:constraint_victim(npt_treaty_1970__reciprocal_disarmament_reading, verification_infrastructure).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% NNWS PERSPECTIVE (SNARE): Structurally trapped by the bargain's core asymmetry. NNWS forgo nuclear weapons capacity and domestic fuel enrichment in exchange for a disarmament obligation (Article VI) that the NWS have not fulfilled for 55 years. No exit mechanism exists short of treaty withdrawal (costly and destabilizing). The constraint extracts commitment to permanent non-weapons status while the reciprocal obligation remains performative. Maximum experienced extraction from the perspective of a structurally dependent agent with no alternatives.
constraint_indexing:constraint_classification(npt_treaty_1970__reciprocal_disarmament_reading, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% NAM/NNWS COALITION PERSPECTIVE (TANGLED ROPE): Organized agents leveraging the coordination function (predictable proliferation barrier) while pressing normative claims on Article VI implementation. Constrained exit — walking away from NPT carries security risks and loses access to peaceful nuclear technology. But coalition cohesion provides real agency: the 2015 NPT Review Conference deadlock, recurring NNWS resolutions demanding disarmament timelines, and the 2017 Treaty on the Prohibition of Nuclear Weapons (TPNW) represent genuine constraint on NWS behavior, even though Article VI enforcement remains absent. Mixed extraction and coordination: the constraint secures NNWS cooperation while extracting their acceptance of an unfulfilled reciprocal.
constraint_indexing:constraint_classification(npt_treaty_1970__reciprocal_disarmament_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% NWS INSTITUTIONAL PERSPECTIVE (ROPE): The NWS benefit from Article VI's performative status. The constraint coordinates proliferation containment (preventing other states from developing weapons) while the NWS maintain strategic autonomy via indefinite arsenal modernization. Exit options are high — the NWS can reinterpret Article VI as 'good-faith negotiations' (open-ended) rather than binding disarmament commitment. The constraint appears to the NWS primarily as coordination: maintaining nonproliferation stability while preserving their own deterrent posture. Low experienced extraction because the NWS define the constraint's interpretation.
constraint_indexing:constraint_classification(npt_treaty_1970__reciprocal_disarmament_reading, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% STRATEGIC STABILITY PERSPECTIVE (MOUNTAIN): From a great-power realist framework, nuclear deterrence and proliferation containment are immutable features of international security. The NPT is read as codifying inevitable power distribution (NWS retain weapons; NNWS renounce them) rather than as a contested bargain. Article VI is reinterpreted as 'pursue disarmament in good faith' — inherently open-ended and compatible with indefinite modernization. This perspective risks naturalizing a contingent institutional arrangement (the 1970 NPT oligarchy) as a law of strategic necessity. The engine's false summit detector will flag this as a false mountain, revealing the reading's dependence on NWS interpretation privilege.
constraint_indexing:constraint_classification(npt_treaty_1970__reciprocal_disarmament_reading, mountain,
    context(agent_power(powerful),
            time_horizon(civilizational),
            exit_options(mobile),
            spatial_scope(universal))).

% IAEA/REVIEW CONFERENCE MACHINERY PERSPECTIVE (PITON): The NPT Review Conference structure and IAEA safeguards regime perform elaborate compliance verification for NNWS (enrichment monitoring, facility inspections, enhanced protocols) while Article VI disarmament remains unverified. The theater ratio is high: NPT Review Conferences convene every five years with deadlocked declarations (last successful final document: 2000), demonstrating that the verification machinery works only for one side of the bargain. The regime persists through institutional inertia — the IAEA and Review Conference structure are UNESCO-scale bureaucracies maintaining themselves through procedural compliance rather than functional verification. Theater ratio reflects the asymmetry: NNWS inspections are stringent and real; NWS 'transparency' is voluntary and largely theater.
constraint_indexing:constraint_classification(npt_treaty_1970__reciprocal_disarmament_reading, piton,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% NWS MODERNIZATION PERSPECTIVE (TANGLED ROPE): When examined as victims rather than beneficiaries, NWS face constraints on force structure transparency and deployment patterns. The NPT's implicit obligation to 'pursue disarmament' creates pressure for declaratory restraint (no explicit nuclear targeting doctrines, no publicly stated expansion plans) and verification costs (participating in review conferences, explaining modernization programs to skeptical NNWS delegations). The constraint coordinates NWS behavior through reputational pressure and alliance cohesion requirements, while the NWS extract their right to modernize arsenals. This reading flips the NWS from beneficiaries (institutional/arbitrage, experiencing rope) to constrained actors bearing mixed costs and benefits.
constraint_indexing:constraint_classification(npt_treaty_1970__reciprocal_disarmament_reading, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% TPNW/ALTERNATE REGIME PERSPECTIVE (SCAFFOLD): The 2017 Treaty on the Prohibition of Nuclear Weapons represents an organized attempt to build an exit pathway from the NPT's reciprocal stalemate. TPNW is a temporary coordination mechanism (sunset clause: designed to be superseded when NWS join or NPT Article VI is genuinely implemented). Low extractiveness because the TPNW coalition has agency and sees a clear exit path: either NWS capitulation on disarmament (absorbing TPNW frameworks) or a generational transition toward delegitimation of nuclear weapons. Theater ratio is lower than NPT machinery because TPNW lacks the institutional inertia — it is still mobilizing. The scaffold classification reflects that TPNW is a transitional structure with explicit political endpoint, not an indefinite coordination mechanism.
constraint_indexing:constraint_classification(npt_treaty_1970__reciprocal_disarmament_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% ANALYTICAL OBSERVER PERSPECTIVE (TANGLED ROPE with identity lock): This reading's analytical position is itself captured by the reciprocal framing. The analyst perceiving Article VI as a binding reciprocal obligation is locked into a legal-positivist identity that assumes treaty text equals binding commitment. The analyst can see the NWS non-compliance and the structural injustice, but cannot fully break from the frame that treats the NPT as a coherent agreement rather than a power-distribution document. The identity lock is professional: international law scholarship has institutionalized Article VI as a binding norm, creating career and credibility costs for abandoning the frame. Yet the analyst recognizes that NWS interpretation has treated Article VI as perpetually open-ended — creating a double-bind: the text says 'binding obligation,' but NWS practice says 'good-faith indefinite negotiation.' This perspective instantiates the oracle gap (Theorem 4): the analyst's native instruments (treaty interpretation, textual analysis) cannot detect the structure that cross-position analysis reveals (oligarchic power distribution masked by reciprocal language).
constraint_indexing:constraint_classification(npt_treaty_1970__reciprocal_disarmament_reading, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(identity_locked),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(npt_treaty_1970__reciprocal_disarmament_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(npt_treaty_1970__reciprocal_disarmament_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(npt_treaty_1970__reciprocal_disarmament_reading, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(npt_treaty_1970__reciprocal_disarmament_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(npt_treaty_1970__reciprocal_disarmament_reading, TR),
    TR >= 0.70.

:- end_tests(npt_treaty_1970__reciprocal_disarmament_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The constraint extracts from NNWS a binding commitment to non-weapons status while the reciprocal obligation (Article VI) remains unenforced and indefinite. The extractiveness is not maximal (0.75+) because NNWS have organized coalition options, TPNW exit pathway is available, and the constraint persists through NNWS continued membership (not coercion alone). The 30-year trajectory shows rising extractiveness: initial extractiveness (0.42) reflected relatively balanced NNWS expectations in 1970; by 2000, noncompliance became undeniable (extractiveness rises to 0.52); by 2026, the pattern is crystallized into a structural injustice narrative driving TPNW mobilization (extractiveness 0.58). Suppression (0.62): Moderate-high. Multiple barriers limit NNWS exit: security dependence on NPT's nonproliferation guarantee (if NNWS withdraw, other states may develop weapons), technology access restrictions, diplomatic isolation costs, and the coordination success of the regime itself (NNWS have internalized proliferation restraint). The 30-year trajectory shows rising suppression: as NWS modernize arsenals visibly, the costs of challenging Article VI noncompliance increase (career risk for diplomats, alliance fracture risks, security retaliation), driving suppression from 0.50 to 0.62. Theater ratio (0.68): High and rising. The NPT Review Conference machinery performs elaborate NNWS oversight (IAEA safeguards, enhanced protocols, facility inspections) while Article VI verification remains absent. Review Conference deadlocks (last successful final document 2000) demonstrate that the machinery maintains itself through procedural participation rather than substantive outcome. Theater ratio rises from 0.55 (early years with successful review documents) to 0.68 (contemporary deadlock era).
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap between NWS beneficiary and NNWS victim perspectives is maximal. NWS see Article VI as coordination (Rope) — a mechanism for maintaining proliferation stability while preserving strategic autonomy. NNWS see the same constraint as extraction (Snare) — a mechanism for securing permanent non-weapons status while the reciprocal obligation remains unfulfilled. The gap grows over time: in 1970, NNWS optimism about disarmament timelines made the constraint appear more balanced (perspective gap was moderate). By 2026, the gap is severe: NWS modernize arsenals openly, NNWS perceive broken faith, and the constraint appears to NNWS as pure entrapment. The organized NNWS coalition introduces a third perspective (Tangled Rope) representing the mixed experience of constrained agents with some leverage. The TPNW scaffold represents an emerging perspective showing NNWS agency and exit pathways. The analytical observer's Tangled Rope with identity lock captures the paradox: the observer sees the extraction structurally but is locked into a legal framework that assumes the treaty is binding, creating analytical tension.
 *
 * DIRECTIONALITY LOGIC:
 *   The reciprocal disarmament reading establishes directionality through explicit identification of NNWS as victims and NWS as beneficiaries. From NNWS perspective: they are structurally trapped (d ≈ 0.92), deriving high f(d) ≈ 1.35, producing high experienced extractiveness. From NWS perspective: they are beneficiaries with arbitrage exit (d ≈ 0.08), deriving low f(d) ≈ -0.08, producing negative experienced extractiveness (constraint appears as free coordination benefit). The NNWS coalition's organized status moderates their d slightly (d ≈ 0.65, f(d) ≈ 1.00) by introducing agency. The IAEA/Review Conference machinery operates under suppression mandate (institutional/constrained), deriving d ≈ 0.58, f(d) ≈ 0.70, experiencing moderate extraction. The TPNW coalition's organized status with clear exit pathway (organized/constrained with sunset) produces d ≈ 0.40, f(d) ≈ 0.40, moderate extraction reflective of transitional structure. The analytical observer with identity_locked status (d ≈ 0.72, f(d) ≈ 1.15) experiences high extracted tension between legal positivism and structural detection.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by showing that the classification depends critically on which reading of the kernel is operative. Under the reciprocal disarmament reading (this constraint), Article VI is a binding obligation and NNWS are victims — Tangled Rope is accurate, extractiveness is 0.58. Under the oligopoly enforcement reading (sibling), Article VI is performative and NNWS are stabilized peripheries — the constraint reclassifies to Rope, extractiveness drops to 0.30. Under the withdrawal sovereignty reading (sibling), Article VI is revocable and NNWS retain exit — the constraint reclassifies to Scaffold, extractiveness drops to 0.25. The three readings partition the possibility space: if the reciprocal reading is the operative kernel, then NNWS being treated as beneficiaries (as they are in the oligopoly reading) is false-summit error; if the oligopoly reading is operative, then NNWS claiming victimhood (as they do in the reciprocal reading) is a reinterpretation. Resolving mandatrophy requires resolving kernel contest, not measuring NPT behavior differently.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    article_vi_textual_ambiguity,
    'Does Article VI''s phrase ''pursue negotiations in good faith'' create a binding disarmament timetable or an open-ended obligation compatible with indefinite modernization?',
    'Linguistic analysis of ''good faith'' across treaty law; historical negotiation records (1968 NPT drafting); comparison with explicit sunset clauses in other arms control treaties (INF, START provisions); International Court of Justice advisory opinions on treaty interpretation methodology',
    'If timetable reading: Article VI is a binding Tangled Rope (NWS are constrained victims); NPT is structurally unjust. If indefinite reading: Article VI is performative (NWS benefit from Rope or Mountain framing); extractiveness drops to 0.30, constraint reclassifies as Rope or Piton.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(article_vi_textual_ambiguity, conceptual, 'Textual interpretation of Article VI commitment scope').

omega_variable(
    verification_gap_structural_vs_incidental,
    'Is the absence of Article VI verification mechanisms a structural feature of the oligarchic bargain, or an incidental implementation gap that could be fixed with technical innovation?',
    'Historical analysis: did the 1968 negotiators deliberately omit NWS verification mechanisms to preserve strategic autonomy? Did subsequent Review Conferences propose and reject verification architectures? Analysis of why IAEA safeguards work for NNWS enrichment but not for NWS disarmament stockpile management.',
    'If structural: verification gap is intentional oligarchic protection (extraction mechanism); suppression score justified at 0.62. If incidental: gap is technical problem with political solution; suppression drops, constraint reclassifies toward Rope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(verification_gap_structural_vs_incidental, empirical, 'Whether verification gap is structural design or incidental omission').

omega_variable(
    reciprocal_illusion_vs_actual_exchange,
    'Does the NPT constitute a genuine reciprocal bargain (NNWS restraint for NWS disarmament), or a power-distribution mechanism masquerading as reciprocity?',
    'Comparative analysis: examine alternative-universe outcomes if Article VI were binding. Model NWS incentive changes if disarmament timelines were enforceable. Survey NNWS leadership on whether they would accept the NPT as written if Article VI lacked any enforcement mechanism. Historical counterfactual: what would NNWS have demanded if they had equivalent military power in 1968?',
    'If genuine reciprocal bargain: constraint classification as Tangled Rope is accurate; extractiveness reflects imbalance. If power-distribution mechanism: NPT is closer to Snare (NNWS permanently constrained); extractiveness rises to 0.68+; suppression rises to 0.75+.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reciprocal_illusion_vs_actual_exchange, preference, 'Whether NPT is reciprocal bargain or masked power distribution').

omega_variable(
    nnws_coalition_power_threshold,
    'At what threshold of NNWS coalition cohesion (treaty withdrawal credibility, TPNW accession, domestic enrichment development) does the power asymmetry shift enough to force NWS renegotiation of Article VI?',
    'Game-theoretic modeling of coalition defection; historical analysis of nonproliferation crises (Iran, North Korea, Libya); survey of NNWS security calculations for disarmament-acceleration scenarios',
    'If threshold is low (<25% treaty withdrawal credibility): NNWS coalition already has latent leverage; classification shifts to Rope with NWS as beneficiaries. If threshold is high (>60%): NNWS remain trapped; constraint persists as Snare or Tangled Rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(nnws_coalition_power_threshold, empirical, 'Coalition power threshold for Article VI enforcement shift').

omega_variable(
    kernel_contest_in_1970,
    'In the 1970 NPT adoption, which reading was the operative kernel — reciprocal disarmament obligation or oligarchic nonproliferation coordination?',
    'Archival analysis of negotiation positions (USSR, US, NAM bloc statements); examination of which reading commanded majority support vs. which was imposed by negotiating superpowers; analysis of NNWS reservations and late signings as evidence of which reading was understood as binding',
    'If reciprocal reading was operative: this constraint''s reading is faithful to the intended kernel; false summit in Mountain perspective is justified. If oligarchic reading was operative: this constraint''s reading is a reinterpretation; it is itself subject to committer contest.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_contest_in_1970, empirical, 'Which reading was the operative kernel at NPT adoption').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(npt_treaty_1970__reciprocal_disarmament_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(npt_recip_tr_t0, npt_treaty_1970__reciprocal_disarmament_reading, theater_ratio, 0, 0.55).
narrative_ontology:measurement(npt_recip_tr_t15, npt_treaty_1970__reciprocal_disarmament_reading, theater_ratio, 15, 0.62).
narrative_ontology:measurement(npt_recip_tr_t30, npt_treaty_1970__reciprocal_disarmament_reading, theater_ratio, 30, 0.68).

% Extraction over time
narrative_ontology:measurement(npt_recip_be_t0, npt_treaty_1970__reciprocal_disarmament_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(npt_recip_be_t15, npt_treaty_1970__reciprocal_disarmament_reading, base_extractiveness, 15, 0.52).
narrative_ontology:measurement(npt_recip_be_t30, npt_treaty_1970__reciprocal_disarmament_reading, base_extractiveness, 30, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(npt_recip_su_t0, npt_treaty_1970__reciprocal_disarmament_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(npt_recip_su_t15, npt_treaty_1970__reciprocal_disarmament_reading, suppression_requirement, 15, 0.58).
narrative_ontology:measurement(npt_recip_su_t30, npt_treaty_1970__reciprocal_disarmament_reading, suppression_requirement, 30, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(npt_treaty_1970__reciprocal_disarmament_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(npt_treaty_1970__reciprocal_disarmament_reading, npt_treaty_1970__oligopoly_enforcement_reading).
narrative_ontology:affects_constraint(npt_treaty_1970__reciprocal_disarmament_reading, npt_treaty_1970__withdrawal_sovereignty_reading).
narrative_ontology:affects_constraint(npt_treaty_1970__reciprocal_disarmament_reading, iaea_comprehensive_safeguards_protocols).
narrative_ontology:affects_constraint(npt_treaty_1970__reciprocal_disarmament_reading, treaty_on_prohibition_of_nuclear_weapons).
narrative_ontology:affects_constraint(npt_treaty_1970__reciprocal_disarmament_reading, first_use_doctrine_opacity).

% DUAL FORMULATION NOTE:
% The NPT kernel admits three readings with materially different ε values. All three share the same underlying treaty text and institutional machinery but differ in how they interpret Article VI binding force and NNWS/NWS structural relationships. This constraint (reciprocal_disarmament_reading, ε=0.58) models the reading that treats Article VI as binding and NNWS as victims. Sibling reading oligopoly_enforcement_reading (ε≈0.30) models Article VI as performative. Sibling reading withdrawal_sovereignty_reading (ε≈0.25) models NPT as revocable. The three stories are linked by kernel identity, not by causal dependence. Which reading is operative affects classification; the three coexist in different actors' frameworks.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
