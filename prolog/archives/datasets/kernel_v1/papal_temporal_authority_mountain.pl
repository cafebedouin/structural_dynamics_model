% ============================================================================
% CONSTRAINT STORY: papal_temporal_authority_mountain
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_papal_temporal_authority_mountain, []).

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
    domain_priors:emerges_naturally/1,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: papal_temporal_authority_mountain
 *   human_readable: Papal Temporal Authority as Natural Law of Sovereignty
 *   domain: international_law/colonial_history/sovereignty_theory
 *
 * SUMMARY:
 *   The Treaty of Tordesillas (1494) and its precedent in papal bulls of the
 *   1490s (Inter Caetera, Eximiae Devotionis) represent one of history's
 *   clearest examples of a constraint that appears as natural law from some
 *   perspectives while revealing itself as a contingent institutional
 *   arrangement from others. The papal claim to temporal authority — the
 *   right to grant dominion over territories and peoples — became the sole
 *   legitimate framework for Iberian territorial division in the Americas.
 *   From the perspective of European sovereigns operating within the Catholic
 *   authority system, this appears as an immutable law: without papal
 *   sanction, no claim to territory could be made legible to other European
 *   powers. From the perspective of indigenous sovereigns not participating
 *   in the Catholic framework, the same constraint appears either as an
 *   incomprehensible external imposition (snare) or as a natural law imposed
 *   by a superior civilization (mountain, but only after internalization).
 *   The analytical challenge is to distinguish whether papal temporal
 *   authority is a genuine immutable principle (natural law, therefore
 *   mountain) or whether it is a contingent institutional arrangement that
 *   beneficiaries have naturalized to prevent renegotiation (false summit,
 *   therefore snare or tangled_rope after unmasking). The constraint's
 *   extractiveness is moderate (0.18) because while it does benefit the
 *   Iberian crowns and the papacy, much of what it accomplishes is genuine
 *   coordination: preventing war between Spain and Portugal over contested
 *   territories. The low theater ratio (0.35) reflects that papal authority
 *   functioned with relative transparency in the 15th-16th centuries — the
 *   legitimacy claim was explicit and openly debated among European elites,
 *   not hidden behind institutional theater.
 *
 * KEY AGENTS:
 *   - Taíno, Aztec, Inca, Congo sovereigns and polities: Primary victims (powerless/trapped) — territorial claims are rendered invisible; no access to papal authority system; suppression of sovereignty through framework exclusion
 *   - Spanish Crown (Castile): Primary beneficiary (institutional/arbitrage) — receives exclusive territorial grants; legitimacy certified by highest Catholic authority; can coordinate with Portugal without war
 *   - Portuguese Crown: Primary beneficiary (institutional/arbitrage) — receives exclusive territorial grants; legitimacy certified by highest Catholic authority; can coordinate with Spain without war
 *   - Roman Catholic Church (Papal Institution): Secondary beneficiary (institutional/arbitrage) — temporal authority is enhanced by successful mediation; claim to adjudicate political legitimacy is demonstrated and reinforced
 *   - Non-Catholic European powers (Ottoman Empire, various Muslim and Orthodox sovereigns): Tertiary actors (institutional/mobile) — excluded from the papal arbitration system; develop alternative frameworks (balance of power, colonial competition) that eventually supersede papal authority
 *   - Analytical Observer: Civilizational view (analytical/analytical) — can see both the coordination function (rope) and the contingent institutional basis (false summit); recognizes risk of naturalizing constructed legitimacy
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(papal_temporal_authority_mountain, 0.18).
domain_priors:suppression_score(papal_temporal_authority_mountain, 0.04).
domain_priors:theater_ratio(papal_temporal_authority_mountain, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(papal_temporal_authority_mountain, extractiveness, 0.18).
narrative_ontology:constraint_metric(papal_temporal_authority_mountain, suppression_requirement, 0.04).
narrative_ontology:constraint_metric(papal_temporal_authority_mountain, theater_ratio, 0.35).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(papal_temporal_authority_mountain, accessibility_collapse, 0.88).
narrative_ontology:constraint_metric(papal_temporal_authority_mountain, resistance, 0.12).

% --- Constraint claim ---
narrative_ontology:constraint_claim(papal_temporal_authority_mountain, mountain).
narrative_ontology:human_readable(papal_temporal_authority_mountain, "Papal Temporal Authority as Natural Law of Sovereignty").
narrative_ontology:topic_domain(papal_temporal_authority_mountain, "international_law/colonial_history/sovereignty_theory").

domain_priors:emerges_naturally(papal_temporal_authority_mountain).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(papal_temporal_authority_mountain, '4bc9ef47-1e92-4ef9-ae4b-b5b191471046').
narrative_ontology:cs_kernel_codification('4bc9ef47-1e92-4ef9-ae4b-b5b191471046', formalized).
narrative_ontology:cs_authority_grounding('4bc9ef47-1e92-4ef9-ae4b-b5b191471046', extraction).
narrative_ontology:cs_interpretation_layer_present('4bc9ef47-1e92-4ef9-ae4b-b5b191471046').
narrative_ontology:cs_created_at('4bc9ef47-1e92-4ef9-ae4b-b5b191471046', '').

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(papal_temporal_authority_mountain, iberian_crowns).
narrative_ontology:constraint_beneficiary(papal_temporal_authority_mountain, catholic_church_institutional_authority).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: INDIGENOUS SOVEREIGNS (MOUNTAIN) — From the position of a Taíno, Aztec, or Congo kingdom in 1494, papal authority to grant territories appears as an absolute law: the pope's grant is the only legitimating mechanism recognized by European powers; indigenous possession, governance, and prior occupation are structurally invisible to the legitimacy frame. No alternative framework for territorial legitimacy is available within the operative authority system. The constraint is immutable from this position — exit is impossible because the constraint defines which voices count in sovereignty discourse at all.
constraint_indexing:constraint_classification(papal_temporal_authority_mountain, mountain,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: IBERIAN CROWNS (MOUNTAIN) — Spain and Portugal experience papal authority as an immutable law grounding their own legitimacy. The pope's grant is the supreme authority for territorial claim; without papal sanction, their claims would be mere conquest or occupation. The constraint appears natural because it establishes the very framework within which their sovereignty becomes legible to other European powers. From this perspective, papal authority is unchangeable — it is the foundation, not a contingent arrangement.
constraint_indexing:constraint_classification(papal_temporal_authority_mountain, mountain,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: ROMAN CATHOLIC CHURCH (MOUNTAIN) — For the papal institution, temporal authority over territorial division is presented as a natural extension of spiritual authority: the pope, as vicar of Christ, possesses the power to grant dominion over lands and peoples. This is not a negotiable claim but a doctrinal foundation. The constraint appears immutable because it grounds the church's very right to participate in political authority at all.
constraint_indexing:constraint_classification(papal_temporal_authority_mountain, mountain,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: ANALYTICAL OBSERVER / INSTITUTIONAL CONVENTION (ROPE) — From a civilizational analytical position aware of historical contingency, papal temporal authority appears as a coordination mechanism, not a natural law. The constraint solved a genuine coordination problem in 1494: how do two Catholic monarchs divide overseas territory without going to war with each other? The pope provided a neutral arbiter and a legitimating framework. Viewed this way, the constraint is a high-order institutional convention — durable, not immutable. The analysis recognizes that papal authority was contingent on the specific geopolitical and religious context of late medieval Europe, that it depended on prior acceptance of papal authority by these monarchs, and that it was suspended or reinterpreted as European power structures changed. The constraint is changeable in principle, making it appear as coordination rather than natural law.
constraint_indexing:constraint_classification(papal_temporal_authority_mountain, rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 5: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From another analytical angle, one might argue that some form of supreme authority (papal, royal, or otherwise) must exist to adjudicate conflicting territorial claims — that the delegation of authority to *some* arbiter is a structural necessity of any international coordination system. From this view, the specific choice of the pope is contingent, but the principle of delegated arbitration is immutable. This perspective risks naturalizing the institutional arrangement as an immutable law of sovereignty itself. The false-summit detector will flag this perspective because beneficiaries are declared: the constraint naturalizes what is actually a contingent institutional arrangement that benefits the Iberian crowns and the papacy.
constraint_indexing:constraint_classification(papal_temporal_authority_mountain, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(papal_temporal_authority_mountain_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(papal_temporal_authority_mountain, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(papal_temporal_authority_mountain, TypeOther, context(agent_power(analytical), _, _, _)),
    TypePowerless \= TypeOther.

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(papal_temporal_authority_mountain, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(papal_temporal_authority_mountain, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(papal_temporal_authority_mountain, ExtMetricName, E),
    domain_priors:suppression_score(papal_temporal_authority_mountain, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(papal_temporal_authority_mountain),
    narrative_ontology:constraint_metric(papal_temporal_authority_mountain, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(papal_temporal_authority_mountain, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(papal_temporal_authority_mountain_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.18): Low-moderate. The constraint genuinely solves a coordination problem — preventing war between Spain and Portugal over new territories. This is a real coordination benefit. However, the benefit flows entirely to European powers while costs flow to indigenous sovereigns who are excluded from the system. The 0.18 value reflects that extractiveness is scaled by directionality: from the Iberian perspective, f(d) produces low effective extraction (beneficiaries with arbitrage exit); from the indigenous perspective, f(d) produces high effective extraction (victims with trapped exit), but the base extractiveness is moderate rather than high because the constraint primarily operates through framework exclusion rather than active coercion. Suppression (0.04): Very low. The suppression of indigenous claims is structural (they lack military power to contest) rather than imposed through the constraint itself. The constraint operates through framework definition — it makes indigenous claims unintelligible rather than suppressing them actively. Theater ratio (0.35): Low. Papal temporal authority functioned with high transparency in the 15th-16th centuries. The claim was explicit, openly debated among European elites, and not hidden behind institutional theater. The theater ratio rises over the interval as papal authority declines in practice (by 1700, papal grants mean little) but the fiction is maintained increasingly through performative gesture rather than substantive adjudication.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap is extreme and reveals the constraint's hidden structure. From the beneficiary Iberian perspective, the constraint appears as immutable natural law: the pope's authority to grant dominion is foundational, unchallengeable, and necessary for sovereignty claims. From the powerless indigenous perspective, the constraint also appears as immutable natural law, but for opposite reasons: European military superiority and religious authority combine to create an inescapable framework in which indigenous claims are never legible. From the analytical observer's institutional-convention perspective, the constraint appears as a high-order coordination mechanism (rope) — durable and functional but contingent on specific geopolitical circumstances. From the alternative analytical perspective that risks naturalizing institutional arrangements, the constraint appears as mountain again — but this time flagging the false-summit risk. The gap between 'natural law' and 'contingent institutional arrangement' is the diagnostic signal. That the beneficiaries experience it as natural law while the analytical observer can see it as contingent reveals that 'naturalness' may be a marker of successful power consolidation rather than actual immutability.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality (d) value is determined by the agent's structural relationship to the constraint. The Iberian crowns are beneficiaries with arbitrage exit — they can pursue territorial claims outside the papal framework if needed (they eventually do, as papal authority declines), so d is low (~0.10), producing negative effective extraction f(d) ≈ -0.12. They experience the constraint as beneficial coordination. Indigenous sovereigns are victims with no exit — they cannot contest papal authority or escape its framework without abandoning sovereignty claims entirely, so d is high (~0.95), producing high effective extraction f(d) ≈ 1.42. They experience maximum extraction. The analytical observer at the civilizational scale sees the constraint as institutional convention grounded in specific historical circumstances, so d is intermediate (~0.72), producing moderate extraction f(d) ≈ 1.15. The canonical derivation flows from power atom (institutional/arbitrage vs powerless/trapped) through the sigmoid to the experienced extractiveness. Suppression is unscaled (structural property of the constraint, not observer-relative).
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves its own mandatrophy through the false-summit signature. The beneficiaries (Iberian crowns, papacy) perceive the constraint as immutable natural law (mountain). The victims (indigenous sovereigns) also perceive it as immutable natural law, but through powerlessness rather than foundational authority. The analytical observer can see that the constraint naturalizes a contingent institutional arrangement: papal temporal authority was historically specific, depended on prior acceptance by Iberian crowns, and was gradually abandoned as alternative frameworks (balance of power, international law, colonial competition) emerged. The false-summit detector identifies that beneficiaries are declared on a mountain constraint, indicating that the 'natural law' appearance may be constructed. The mandatrophy resolves as: 'The constraint appears as mountain from perspectives that benefit from naturalizing it (beneficiaries) and from perspectives powerless to challenge it (victims), but analytical inspection reveals contingent institutional basis. This is not a true natural law but a successful power consolidation that has hidden its contingency behind appeals to immutable authority.' The constraint's true type, viewed from outside the beneficiary frame, is tangled_rope: it coordinates the division of territories between Iberian powers (genuine coordination function, low theater, beneficiary relationships) while extracting from indigenous sovereigns through framework exclusion (asymmetric extraction, high suppression for the powerless). The mountain classification persists from insider perspectives precisely because the institutional arrangement has succeeded in naturalizing itself.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_constructed_legitimacy,
    'Is papal temporal authority a natural law of sovereignty (immutable principle that any system must have), or a contingent institutional arrangement that beneficiaries have framed as natural law?',
    'Historical counterfactual analysis: alternative arbitration frameworks that emerged historically (Concert of Europe, balance-of-power diplomacy, international law without papal authority) and their success at coordinating territorial claims. If equivalent coordination occurred without papal authority, the ''naturalness'' claim is refuted.',
    'If natural law: constraint remains mountain across all contexts. If contingent: constraint is rope or tangled_rope; beneficiaries have constructed mountain appearance to prevent renegotiation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_legitimacy, conceptual, 'Whether papal temporal authority is immutable or naturalizes a contingent institutional arrangement').

omega_variable(
    accessibility_to_non_european_sovereigns,
    'Did non-European sovereigns (Aztec, Inca, Congo, Ottoman) have meaningful access to the papal arbitration system, or was accessibility structurally collapsed by language, geography, and religious authority barriers?',
    'Archival analysis: documented attempts by indigenous or non-Catholic polities to invoke or contest papal authority; documentation of communication pathways available to non-European sovereigns in the 1490s-1500s.',
    'If accessibility existed: constraint appears as coordination mechanism (rope). If accessibility was structurally collapsed: constraint appears as pure extraction (snare) from indigenous perspective; mountain only from European perspective within Catholic authority framework.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(accessibility_to_non_european_sovereigns, empirical, 'Whether non-European sovereigns could access papal arbitration').

omega_variable(
    doctrine_vs_political_instrumentalization,
    'Did the papacy advance papal temporal authority as a genuine theological doctrine grounded in biblical or apostolic authority, or as a post-hoc rationalization for political alliances with Iberian crowns?',
    'Theological historical analysis: dating of papal claims to temporal authority relative to the Reconquista and overseas expansion; comparison with earlier doctrinal statements on papal temporal power; examination of papal practice in non-Iberian disputes.',
    'If doctrine: constraint has internal consistency and may be immutable within the papal authority framework. If instrumentalization: constraint is snare disguised as mountain; beneficiaries manufactured the appearance of natural law for contingent political advantage.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(doctrine_vs_political_instrumentalization, empirical, 'Whether papal temporal authority is coherent doctrine or political instrumentalization').

omega_variable(
    suppression_mechanism_structural_vs_internalized,
    'Was the suppression of indigenous territorial claims structural (European military dominance and information asymmetry prevented indigenous sovereigns from contesting papal authority) or internalized (indigenous sovereigns internalized the Catholic framework and accepted papal legitimacy as authoritative)?',
    'Historical documentation: records of indigenous resistance to papal authority claims; evidence of indigenous acceptance or rejection of Catholic doctrine on papal power; analysis of indigenous sovereigns'' own legitimacy claims in dialogue with Europeans.',
    'If structural: constraint is snare (external barriers to contest). If internalized: constraint is rope from indigenous perspective only after internalization (identity_locked exit). If mixed: constraint is tangled_rope with internalized suppression as part of the extraction mechanism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_internalized, empirical, 'Whether suppression of indigenous claims was structural or internalized through doctrine').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(papal_temporal_authority_mountain, 0, 200).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(papal_tr_t0, papal_temporal_authority_mountain, theater_ratio, 0, 0.25).
narrative_ontology:measurement(papal_tr_t100, papal_temporal_authority_mountain, theater_ratio, 100, 0.35).
narrative_ontology:measurement(papal_tr_t200, papal_temporal_authority_mountain, theater_ratio, 200, 0.42).

% Extraction over time
narrative_ontology:measurement(papal_be_t0, papal_temporal_authority_mountain, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(papal_be_t100, papal_temporal_authority_mountain, base_extractiveness, 100, 0.18).
narrative_ontology:measurement(papal_be_t200, papal_temporal_authority_mountain, base_extractiveness, 200, 0.18).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(papal_temporal_authority_mountain, enforcement_mechanism).
narrative_ontology:affects_constraint(papal_temporal_authority_mountain, iberian_colonial_extraction_snare).
narrative_ontology:affects_constraint(papal_temporal_authority_mountain, indigenous_sovereignty_erasure_snare).

% DUAL FORMULATION NOTE:
% Papal temporal authority is the legitimating framework for Iberian colonial expansion. The constraint family includes: (1) papal_temporal_authority_mountain — the naturalizing frame itself; (2) iberian_colonial_extraction_snare — the extraction mechanism enabled by papal legitimacy; (3) indigenous_sovereignty_erasure_snare — the suppression of indigenous territorial claims. These three constraints are structurally linked: the mountain creates the conditions for both snares. The papal authority story models the legitimacy claim; the snare stories model the actual extraction that occurs within the frame the authority establishes.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
