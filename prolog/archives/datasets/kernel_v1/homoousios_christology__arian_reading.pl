% ============================================================================
% CONSTRAINT STORY: homoousios_christology__arian_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_homoousios_christology__arian_reading, []).

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
 *   constraint_id: homoousios_christology__arian_reading
 *   human_readable: Arian Christology: Subordinationist Authority and Imperial Coercion (4th-5th century)
 *   domain: historical_theology/ecclesiastical_politics/commitment_systems
 *
 * SUMMARY:
 *   The Arian reading of Christ's nature — that Christ is created and
 *   subordinate to the Father, not of identical substance (homoousios) —
 *   represents one reading of a contested theological kernel spanning from
 *   the Council of Nicaea (325 CE) through the Council of Constantinople (381
 *   CE). This reading instantiates a constraint that combines genuine
 *   theological coordination (unified ecclesiastical doctrine) with
 *   institutional extraction (enforced conformity via imperial coercion and
 *   episcopal discipline). The Arian reading was dominant in the Eastern
 *   Empire under Constantine's son Constantius II (337–361 CE) and faced
 *   systematic suppression from the pro-Nicene tradition after Theodosius I
 *   mandated pro-Nicene orthodoxy (380 CE). The constraint exhibits the full
 *   signature of a tangled rope: beneficiaries (non-Nicene bishops, imperial
 *   authority) coordinate ecclesiastical doctrine while simultaneously
 *   extracting legitimacy and institutional resources from dissenting
 *   communities. The primary victims (provincial communities forced to accept
 *   the formula, the Nicene theological tradition) experience the constraint
 *   as suppressive and non-coordinate. The constraint was ultimately reversed
 *   — not resolved, but inverted — when pro-Nicene became enforced orthodoxy,
 *   suggesting that the extractiveness was inherent to the enforcement
 *   mechanism rather than to the Arian formula itself.
 *
 * KEY AGENTS:
 *   - Arius and Early Arian Coalition (organized/arbitrage): Original proponent of subordinationist Christology; benefits from clarity and doctrinal unity; later eclipsed but tradition survives through eastern Germanic tribes and Ostrogothic settlement
 *   - Non-Nicene Episcopal Coalition (organized/arbitrage): Eusebius of Caesarea, later Arian-leaning bishops; benefits from imperial favor and ability to enforce doctrinal conformity; primary institutional beneficiaries
 *   - Eastern Imperial Authority (institutional/arbitrage): Constantine, Constantius II, later emperors; perceives subordinationism as aligned with monotheistic empire; benefits from ecclesiastical unity under imperial adjudication
 *   - Provincial Communities (powerless/trapped): Forced subscribers to Arian formula via imperial edict and episcopal enforcement; no exit options except apostasy or exile
 *   - Nicene Theological Tradition (organized/constrained): Athanasius, pro-Nicene bishops; constrained by exile, church confiscation, forced deposition; survives through clandestine networks and martyrdom narratives
 *   - Ecclesiastical Authority Structure (institutional/constrained): The bishopric itself as an institution; experiences mixed coordination (unified doctrine) and extraction (enforced conformity); undergoes reversal of dominance at 381 CE
 *   - Analytical Observer (analytical/analytical): Sees subordinationism as either logically necessary consequence of Platonic metaphysics (false summit risk) or contingent institutional arrangement (correct reading)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(homoousios_christology__arian_reading, 0.58).
domain_priors:suppression_score(homoousios_christology__arian_reading, 0.65).
domain_priors:theater_ratio(homoousios_christology__arian_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(homoousios_christology__arian_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(homoousios_christology__arian_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(homoousios_christology__arian_reading, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(homoousios_christology__arian_reading, tangled_rope).
narrative_ontology:human_readable(homoousios_christology__arian_reading, "Arian Christology: Subordinationist Authority and Imperial Coercion (4th-5th century)").
narrative_ontology:topic_domain(homoousios_christology__arian_reading, "historical_theology/ecclesiastical_politics/commitment_systems").

domain_priors:requires_active_enforcement(homoousios_christology__arian_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(homoousios_christology__arian_reading, '4a7f3c92-e1b4-4d2f-8b5c-6f2a9e8d1c47').
narrative_ontology:cs_kernel_codification('4a7f3c92-e1b4-4d2f-8b5c-6f2a9e8d1c47', formalized).
narrative_ontology:cs_authority_grounding('4a7f3c92-e1b4-4d2f-8b5c-6f2a9e8d1c47', extraction).
narrative_ontology:cs_interpretation_layer_present('4a7f3c92-e1b4-4d2f-8b5c-6f2a9e8d1c47').
narrative_ontology:cs_reading_relation('4a7f3c92-e1b4-4d2f-8b5c-6f2a9e8d1c47', homoousios_christology__pro_nicene_reading, coexists_with).
narrative_ontology:cs_reading_relation('4a7f3c92-e1b4-4d2f-8b5c-6f2a9e8d1c47', homoousios_christology__semi_arian_reading, influences).
narrative_ontology:cs_axiom('4a7f3c92-e1b4-4d2f-8b5c-6f2a9e8d1c47', foundational, christ_created_not_eternal).
narrative_ontology:cs_axiom_status(christ_created_not_eternal, holdable).
narrative_ontology:cs_axiom_grounding('4a7f3c92-e1b4-4d2f-8b5c-6f2a9e8d1c47', christ_created_not_eternal, empirically_contingent).
narrative_ontology:cs_axiom('4a7f3c92-e1b4-4d2f-8b5c-6f2a9e8d1c47', foundational, god_absolutely_transcendent_simplicity).
narrative_ontology:cs_axiom_status(god_absolutely_transcendent_simplicity, holdable).
narrative_ontology:cs_axiom_grounding('4a7f3c92-e1b4-4d2f-8b5c-6f2a9e8d1c47', god_absolutely_transcendent_simplicity, deontological).
narrative_ontology:cs_reference_frame('4a7f3c92-e1b4-4d2f-8b5c-6f2a9e8d1c47', subordinationist_ecclesiastical_order).
narrative_ontology:cs_drift_state('4a7f3c92-e1b4-4d2f-8b5c-6f2a9e8d1c47', post_constantinople_381_ce, gap(authority_erosion, severe, true)).
narrative_ontology:cs_created_at('4a7f3c92-e1b4-4d2f-8b5c-6f2a9e8d1c47', '2026-02-26T14:32:18Z').
narrative_ontology:cs_kernel_id(homoousios_christology__arian_reading, homoousios_christology).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(homoousios_christology__arian_reading, non_nicene_episcopal_coalition).
narrative_ontology:constraint_beneficiary(homoousios_christology__arian_reading, eastern_imperial_authority).
narrative_ontology:constraint_victim(homoousios_christology__arian_reading, nicene_theological_tradition).
narrative_ontology:constraint_victim(homoousios_christology__arian_reading, dissenting_provincial_communities).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PROVINCIAL COMMUNITY (SNARE) — Trapped within ecclesiastical jurisdiction; forced subscription to Arian formulas through imperial edict and episcopal coercion. Deposition of Nicene bishops, exile of resisters, confiscation of churches create total suppression. No exit options except apostasy or exile. Pure extraction: the community bears the cost of theological enforced conformity with no coordination benefit.
constraint_indexing:constraint_classification(homoousios_christology__arian_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: AMBIGUOUS PROVINCIAL BISHOP (TANGLED ROPE) — Constrained by imperial preference for Arian formulation but also depends on episcopal collegiality and theological legitimacy. Experiences mixed extraction and coordination: receives benefits of imperial favor and unified ecclesiastical discipline, but pays cost of enforcing unpopular doctrine and risks legitimacy collapse if Nicene sentiment shifts. Exit options are constrained (deposition, exile, loss of rank) but exist.
constraint_indexing:constraint_classification(homoousios_christology__arian_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 3: NON-NICENE EPISCOPAL COALITION (ROPE) — Organized bishops (Eusebius of Caesarea, Arius, their successors) perceive the constraint as pure coordination: achieving consensus on christological formula enables unified ecclesiastical governance and reduces ambiguity in doctrinal teaching. Benefits from authority concentration and the ability to enforce discipline through collective episcopal action. Arbitrage options: can move between imperial favor and theological flexibility to maximize position. Net beneficiary.
constraint_indexing:constraint_classification(homoousios_christology__arian_reading, rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 4: EASTERN IMPERIAL AUTHORITY (ROPE) — Sees the constraint as coordination of ecclesiastical unity under imperial oversight. The Arian reading aligns with imperial prerogative to adjudicate religious settlement (Constantine's Council of Nicaea, Constantius II's enforcement). Benefits from unified church hierarchy without the subordinationism of the Nicene formula challenging imperial structure. High arbitrage: can adjust theological requirements to maintain control.
constraint_indexing:constraint_classification(homoousios_christology__arian_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: NICENE THEOLOGICAL TRADITION (SNARE) — Constrained but organized (Athanasius, cohesive pro-Nicene network). Experiences enforced suppression: exile, church confiscation, forced exile of Nicene bishops (Athanasius exiled five times). The Arian constraint extracts legitimacy by appropriating imperial authority. The tradition survives through clandestine networks and appeal to martyrdom narratives, but faces sustained extraction via institutional exclusion. High suppression despite organized resistance — exit through imperial recourse is foreclosed.
constraint_indexing:constraint_classification(homoousios_christology__arian_reading, snare,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 6: ECCLESIASTICAL AUTHORITY STRUCTURE (TANGLED ROPE) — The constraint simultaneously coordinates episcopal discipline (unified creed reduces heresy disputes) and extracts from dissenting traditions (forced conformity via institutional power). The structure experiences mixed outcomes: efficiency of unified doctrine vs legitimacy costs of enforced conformity. Over time (generational scale), the constraint fails to stabilize — increasing resistance and eventual revision (Council of Constantinople 381) suggest the extraction mechanism eventually collapses under pressure.
constraint_indexing:constraint_classification(homoousios_christology__arian_reading, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational timescale, the subordinationism of the Arian reading appears as an inevitable logical position given the theological premises: if Christ is begotten (not eternal) and God is absolutely transcendent, then Christ cannot be of identical substance. This perspective sees Arianism as a necessary consequence of certain metaphysical commitments — a structural feature of monotheistic logic, not a contingent institutional arrangement. However, the structural data contradicts this: the constraint's enforceability depends on imperial coercion (suppression = 0.65), and the beneficiaries are identifiable political actors, not abstract logical necessity. The engine will flag this as a false summit.
constraint_indexing:constraint_classification(homoousios_christology__arian_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(homoousios_christology__arian_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(homoousios_christology__arian_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(homoousios_christology__arian_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(homoousios_christology__arian_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(homoousios_christology__arian_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The Arian reading's extractiveness derives from two sources: (1) theological uniformity imposed on dissenting communities via imperial and episcopal enforcement, and (2) the appropriation of ecclesiastical authority by non-Nicene bishops. Unlike pure coordination (Rope), where all parties benefit, the Arian constraint benefits identifiable actors (non-Nicene bishops, imperial authority) and harms others (Nicene tradition, powerless communities). The value reflects that the constraint is not maximally extractive (it does coordinate doctrine; there is a genuine coordination function alongside the extraction) but also not minimal (suppression is high, and victims are clearly identifiable). Suppression (0.65): High. Enforcement mechanisms include imperial edicts, deposition of Nicene bishops, exile of resisters (Athanasius exiled five times), church confiscation, and forced subscription to creeds. Dissent is costly and dangerous, particularly for organized resisters. Victims lack meaningful exit options. Theater ratio (0.48): Low-to-moderate. The Arian constraint is substantially substantive in theological content — the disagreement about Christ's nature is not performative but represents genuine metaphysical commitments. However, the performative element increases during periods of enforcement (forced oaths, insincere recantations) and decreases during periods of genuine theological debate (Council of Nicaea, later Cappadocian refinements). The value reflects a mid-point: the constraint has real intellectual stakes but requires increasing performative compliance as enforcement tightens.
 *
 * PERSPECTIVAL GAP:
 *   The Arian reading produces a full perspectival gap revealing the constraint's hybrid nature. The non-Nicene episcopal coalition sees pure coordination (Rope) — unified doctrine enables governance. The imperial authority sees coordination with imperial prerogative (Rope) — the ecclesiastical structure under imperial control. The provincial community sees pure extraction (Snare) — forced conformity with no coordination benefit. The Nicene tradition sees extraction masked as coordination (Snare) — forced exclusion from authority despite organized resistance. The ecclesiastical authority structure itself sees mixed coordination and extraction (Tangled Rope) — the doctrine unifies the institution but the enforcement destroys legitimacy. The analytical observer risks seeing logical necessity (Mountain: subordinationism is what monotheism requires) but the structural data reveals a false summit: the constraint depends entirely on imperial coercion (suppression = 0.65) and identifiable beneficiaries (non-Nicene bishops, imperial authority), not on logical inevitability.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality varies by agent's relationship to the extraction flow. Non-Nicene bishops benefit from ecclesiastical authority concentration and imperial favor (d ≈ 0.05–0.15, low d, negative chi). Imperial authority benefits from unified church under imperial control (d ≈ 0.10, low d). Provincial communities bear the full cost of enforced conformity with no coordination benefit (d ≈ 0.95, high d, high chi). The Nicene tradition is organized but constrained — organized enough to resist (not maximally powerless) but constrained enough that exit options are expensive (exile, loss of status, clandestine survival). The Nicene organized-resistance perspective produces d ≈ 0.65, moderate-to-high, reflecting that while organized, the agents lack arbitrage options and face sustained suppression. The ecclesiastical authority structure itself experiences d ≈ 0.50, symmetric — it benefits from doctrinal unity (low extraction) but pays legitimacy costs from forced conformity (extraction pressure). Over time, the accumulating legitimacy costs drive the constraint toward reversal (pro-Nicene becomes enforced orthodoxy by 381 CE).
 *
 * MANDATROPHY ANALYSIS:
 *   The Arian reading resolves the mandatrophy by showing that Arianism is a coherent reading of the homoousios kernel that produces a tangled rope rather than a mountain or pure snare. The constraint genuinely coordinates ecclesiastical doctrine (coordination function) while simultaneously extracting through imperial coercion (extraction mechanism). The key to resolution is that the extractiveness is high enough (0.58) to trigger suppression concerns but not so high that the coordination function disappears entirely. Unlike a pure snare (χ ≥ 0.66), the Arian constraint enables real ecclesiastical governance efficiency — bishops can enforce discipline, doctrinal disputes are theoretically settled, communication about Christ's nature is standardized. But unlike a pure rope (χ ≤ 0.35), the coordination function depends on coercion: without imperial enforcement and episcopal discipline, the doctrine would fragment (as indeed it does after 381 CE when Nicene becomes enforced instead). The tangled rope classification captures this: both genuine coordination and genuine extraction are operating, and neither can be removed without the constraint collapsing.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    subordinationism_logical_necessity,
    'Is Arian subordinationism a logically necessary consequence of Platonic metaphysics and Christian monotheism, or a contingent theological choice?',
    'Comparative analysis of Platonic metaphysics in Arius vs Nicene thinkers; demonstration of alternative metaphysical frameworks that accommodate homoousios within monotheism (e.g., essence vs energeia distinction in later Cappadocian theology)',
    'If logically necessary: mountain classification (natural law of theological logic) confirmed. If contingent choice: constraint is institutional arrangement, not natural law; false-summit detection applies.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(subordinationism_logical_necessity, conceptual, 'Whether subordinationism follows logically from Platonic metaphysics or is a contingent choice').

omega_variable(
    imperial_preference_directionality,
    'Did the Eastern Empire prefer Arianism for theological reasons, political-control reasons, or both?',
    'Historical analysis of Constantine''s theology vs his administrative preferences; examination of whether Constantius II enforced Arianism for consistency with his own beliefs or to weaken episcopal autonomy; comparison with later imperial theological preferences (Julian, Theodosius)',
    'If theological: constraint reflects genuine intellectual conviction distributing across power structures. If political-control: constraint is pure institutional extraction masked by theological language; changes classification trajectory.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(imperial_preference_directionality, empirical, 'Directionality of imperial theological preference: genuine belief vs political control').

omega_variable(
    arian_geographic_persistence,
    'Why did Arian Christianity persist in northern Europe and Ostrogothic Italy into the 6th century despite pro-Nicene imperial dominance?',
    'Examination of Arian Ostrogothic settlement, Vandal kingdom theological continuity, and Germanic tribal Christianity; analysis of whether persistence reflects genuine theological commitment or institutional decoupling from imperial enforcement',
    'If persistent via conviction: suggests the constraint has non-coercive mechanisms. If decoupled from enforcement: suggests the Arian reading depended entirely on imperial suppression and collapsed when enforcement weakened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(arian_geographic_persistence, empirical, 'Geographic persistence patterns of Arian theology after imperial suppression weakened').

omega_variable(
    kernel_reading_under_determination,
    'Is the Arian reading a genuine alternative reading of the homoousios kernel, or a rejection of the kernel''s applicability altogether?',
    'Textual analysis: does Arian discourse accept the homoousios term as the binding constraint and offer an alternative reading (e.g., ''homoousios means something other than pro-Nicenes think''), or does it reject homoousios as the binding kernel? If the latter, this is not a kernel reading but a competing kernel.',
    'If alternative reading: this constraint story is correctly framed as a reading of the homoousios kernel. If competing kernel: this story should be decomposed into two stories — one for homoousios (Nicene reading), one for a subordinationist kernel with its own codification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_under_determination, conceptual, 'Whether Arian subordinationism is a reading of homoousios or a rejection of the kernel itself').

omega_variable(
    theodosian_settlement_extractiveness_shift,
    'After Theodosius I mandated pro-Nicene orthodoxy (380 CE), did the extractiveness of the pro-Nicene constraint exceed that of the Arian constraint, suggesting the constraint reversed rather than dissolved?',
    'Comparative analysis of suppression, beneficiary, and victim dynamics before and after 380 CE; measurement of theological coercion mechanisms under both regimes',
    'If extractiveness reversed: this is not a constraint that was resolved but one that rotated — a different reading took the extraction mechanism. If extractiveness decreased: Nicene settlement genuinely stabilized authority. Changes omega resolution for reading_relations (influences vs forecloses).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(theodosian_settlement_extractiveness_shift, empirical, 'Whether pro-Nicene enforcement after 380 CE exceeded Arian enforcement before, suggesting constraint rotation').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(homoousios_christology__arian_reading, 325, 381).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(homo_arian_tr_t325, homoousios_christology__arian_reading, theater_ratio, 325, 0.35).
narrative_ontology:measurement(homo_arian_tr_t350, homoousios_christology__arian_reading, theater_ratio, 350, 0.48).

% Extraction over time
narrative_ontology:measurement(homo_arian_be_t325, homoousios_christology__arian_reading, base_extractiveness, 325, 0.15).
narrative_ontology:measurement(homo_arian_be_t340, homoousios_christology__arian_reading, base_extractiveness, 340, 0.42).
narrative_ontology:measurement(homo_arian_be_t360, homoousios_christology__arian_reading, base_extractiveness, 360, 0.58).
narrative_ontology:measurement(homo_arian_be_t378, homoousios_christology__arian_reading, base_extractiveness, 378, 0.48).
narrative_ontology:measurement(homo_arian_be_t381, homoousios_christology__arian_reading, base_extractiveness, 381, 0.25).

% Suppression requirement over time
narrative_ontology:measurement(homo_arian_su_t325, homoousios_christology__arian_reading, suppression_requirement, 325, 0.15).
narrative_ontology:measurement(homo_arian_su_t340, homoousios_christology__arian_reading, suppression_requirement, 340, 0.55).
narrative_ontology:measurement(homo_arian_su_t360, homoousios_christology__arian_reading, suppression_requirement, 360, 0.65).
narrative_ontology:measurement(homo_arian_su_t378, homoousios_christology__arian_reading, suppression_requirement, 378, 0.5).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(homoousios_christology__arian_reading, identity_coordination).
narrative_ontology:affects_constraint(homoousios_christology__arian_reading, homoousios_christology__pro_nicene_reading).
narrative_ontology:affects_constraint(homoousios_christology__arian_reading, homoousios_christology__semi_arian_reading).
narrative_ontology:affects_constraint(homoousios_christology__arian_reading, imperial_ecclesiastical_authority_4th_century).
narrative_ontology:affects_constraint(homoousios_christology__arian_reading, nicene_creed_enforcement).

% DUAL FORMULATION NOTE:
% The homoousios constraint family decomposes into three distinct constraint stories corresponding to the three readings (Arian, pro-Nicene, semi-Arian). Each reading instantiates a different ε value and different beneficiary/victim structure because the readings differ on what counts as correct doctrine and therefore what agents benefit from enforcing which formula. The Arian reading (this story, ε = 0.58) requires imperial enforcement and coerces provincial compliance. The pro-Nicene reading (sibling story, ε ≈ 0.45–0.55) eventually becomes enforced orthodoxy and reverses the victim/beneficiary relationship. The semi-Arian reading (sibling story, ε ≈ 0.35–0.45) attempts compromise but lacks stable enforcement mechanism. Network edges link these stories to show that the constraint reversal at 381 CE is not resolution but replacement of one tangled rope with another, and to show how later constraints (imperial ecclesiastical authority, nicene creed enforcement) depend on the outcome of this reading contest.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(homoousios_christology__arian_reading, organized, 0.65).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
