% ============================================================================
% CONSTRAINT STORY: homoousios_christology__semi_arian_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_homoousios_christology__semi_arian_reading, []).

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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: homoousios_christology__semi_arian_reading
 *   human_readable: Semi-Arian Christology: Homoiousios (Similar Substance) Compromise
 *   domain: ecclesiastical/theological/political
 *
 * SUMMARY:
 *   In the aftermath of the Council of Nicaea (325), the church fractured
 *   into competing factions over the nature of Christ. The Arian position
 *   (Christ as created, subordinate, not of God's substance) competed with
 *   the Pro-Nicene position (Christ as homoousios, consubstantial with God).
 *   The Semi-Arian formula emerged as a compromise: Christ is homoiousios (of
 *   SIMILAR substance with the Father, not identical substance). This reading
 *   claims to be a genuine coordination mechanism that allows bishops of
 *   different theological commitments to remain in communion. However, the
 *   authored metrics reveal rising extraction and theater over the interval
 *   as the compromise becomes increasingly performative — Semi-Arian bishops
 *   use the formula to maintain authority while the Pro-Nicene faction
 *   positions itself for eventual dominance (accomplished post-381). This is
 *   ONE READING of the contested kernel homoousios_christology; the other
 *   readings (pro_nicene_reading, arian_reading) are separate constraint
 *   stories in the same family, linked via network.affects_constraints.
 *
 * KEY AGENTS:
 *   - Semi-Arian bishops: agenda-setters who broker and defend the homoiousios compromise; their professional identity and episcopal authority are locked into this reading
 *   - Pro-Nicene bishops: organized but constrained; accept the compromise to avoid schism but see it as a retreat from proper doctrine
 *   - Arian bishops: similarly constrained; accept the compromise but experience it as a move toward Pro-Nicene positions that displaces their theology
 *   - Imperial authority: benefits from the schism-prevention function and enforces acceptance via imperial decree
 *   - Cappadocian Fathers and Pro-Nicene ascendant faction: benefit in the short term from stability while positioning for long-term triumph (realized at Constantinople 381)
 *   - Local congregations: excluded from the councils; trapped under whatever doctrine their bishop adopts
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(homoousios_christology__semi_arian_reading, 0.42).
domain_priors:suppression_score(homoousios_christology__semi_arian_reading, 0.38).
domain_priors:theater_ratio(homoousios_christology__semi_arian_reading, 0.29).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(homoousios_christology__semi_arian_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(homoousios_christology__semi_arian_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(homoousios_christology__semi_arian_reading, theater_ratio, 0.29).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(homoousios_christology__semi_arian_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(homoousios_christology__semi_arian_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(homoousios_christology__semi_arian_reading, rope).
narrative_ontology:human_readable(homoousios_christology__semi_arian_reading, "Semi-Arian Christology: Homoiousios (Similar Substance) Compromise").
narrative_ontology:topic_domain(homoousios_christology__semi_arian_reading, "ecclesiastical/theological/political").

domain_priors:requires_active_enforcement(homoousios_christology__semi_arian_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(homoousios_christology__semi_arian_reading, '72a8721a-aee3-4bbc-af4f-72415764950f').
narrative_ontology:cs_kernel_codification('72a8721a-aee3-4bbc-af4f-72415764950f', fixed_text).
narrative_ontology:cs_authority_grounding('72a8721a-aee3-4bbc-af4f-72415764950f', lineage).
narrative_ontology:cs_interpretation_layer_present('72a8721a-aee3-4bbc-af4f-72415764950f').
narrative_ontology:cs_reading_relation('72a8721a-aee3-4bbc-af4f-72415764950f', homoousios_christology__pro_nicene_reading, influences).
narrative_ontology:cs_reading_relation('72a8721a-aee3-4bbc-af4f-72415764950f', homoousios_christology__arian_reading, influences).
narrative_ontology:cs_axiom('72a8721a-aee3-4bbc-af4f-72415764950f', foundational, christ_similarity_not_identity).
narrative_ontology:cs_axiom_status(christ_similarity_not_identity, holdable).
narrative_ontology:cs_axiom_grounding('72a8721a-aee3-4bbc-af4f-72415764950f', christ_similarity_not_identity, deontological).
narrative_ontology:cs_axiom('72a8721a-aee3-4bbc-af4f-72415764950f', foundational, scriptural_subordination_compatibility).
narrative_ontology:cs_axiom_status(scriptural_subordination_compatibility, overridden).
narrative_ontology:cs_axiom_grounding('72a8721a-aee3-4bbc-af4f-72415764950f', scriptural_subordination_compatibility, empirically_contingent).
narrative_ontology:cs_reference_frame('72a8721a-aee3-4bbc-af4f-72415764950f', subordinationist_christology_with_divine_affirmation).
narrative_ontology:cs_drift_state('72a8721a-aee3-4bbc-af4f-72415764950f', post_cappadocian_reinterpretation_375_381, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('72a8721a-aee3-4bbc-af4f-72415764950f', '2026-06-12T14:32:18Z').
narrative_ontology:cs_kernel_id(homoousios_christology__semi_arian_reading, homoousios_christology).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(homoousios_christology__semi_arian_reading, semi_arian_bishops).
narrative_ontology:constraint_beneficiary(homoousios_christology__semi_arian_reading, imperial_authority).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(homoousios_christology__semi_arian_reading, pro_nicene_ascendant_faction).
narrative_ontology:constraint_victim(homoousios_christology__semi_arian_reading, pro_nicene_bishops).
narrative_ontology:constraint_victim(homoousios_christology__semi_arian_reading, arian_bishops).
narrative_ontology:constraint_victim(homoousios_christology__semi_arian_reading, pro_nicene_ascendant_faction).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Bishops who hold the homoiousios position as a genuine theological middle ground between Arianism and Pro-Nicene doctrine. They argue the similarity-of-substance formula preserves Christ's subordination to the Father (avoiding Arianism's complete separation) while affirming his divine nature (avoiding the charge of denying his divinity). They administer councils, write letters defending the position, and work to secure imperial backing for this formulation as the unified orthodox doctrine. Their professional identity and episcopal authority are bound to this reading.
narrative_ontology:constraint_stakeholder(homoousios_christology__semi_arian_reading, semi_arian_bishops, agenda_setter,
    organized, generational, identity_locked, continental).

% Bishops committed to homoousios (identical substance) who experience the semi-arian formula as a betrayal of Nicene orthodoxy. They are forced to accept the homoiousios compromise to maintain communion and avoid schism, even though they see it as a weakening of the proper doctrine. They bear the cost of theological concession and institutional accommodation without gaining the coordination benefit they seek (which would be unanimous acceptance of homoousios). Over the interval, they position themselves strategically to absorb Semi-Arianism into their own framework by 381.
narrative_ontology:constraint_stakeholder(homoousios_christology__semi_arian_reading, pro_nicene_bishops, payer,
    organized, generational, constrained, continental).

% Bishops holding the Arian doctrine (Christ as created, subordinate, not divine) experience the homoiousios compromise as a move away from their position toward the Pro-Nicene side. The formula's affirmation of Christ's genuine divine nature and similarity (not mere creatureliness) closes off their preferred doctrinal space. They are pressured toward acceptance by imperial authority and the need for church unity, bearing the cost of theological displacement without the benefit of doctrinal victory.
narrative_ontology:constraint_stakeholder(homoousios_christology__semi_arian_reading, arian_bishops, payer,
    organized, generational, constrained, continental).

% The Emperor and imperial administration benefit from the homoiousios compromise as a mechanism for avoiding schism and maintaining ecclesiastical unity, which serves imperial political order. The compromise formula is weak enough that both Arian and Pro-Nicene sides can claim partial victory (Semi-Arians see their formula, Pro-Nicenes see their trajectory toward homoousios), making it an attractive interim settlement. The imperial authority enforces acceptance via imperial letters and council decree to prevent the empire from fracturing along theological lines. Post-375, the Emperor shifts support toward Pro-Nicene orthodoxy, effectively dismantling the constraint.
narrative_ontology:constraint_stakeholder(homoousios_christology__semi_arian_reading, imperial_authority, beneficiary,
    institutional, generational, arbitrage, continental).

% Bishops and theologians (particularly the Cappadocian Fathers post-375) who see the homoiousios compromise as a temporary station on the trajectory toward full Pro-Nicene triumph. They benefit from its schism-prevention function in the short term while positioning themselves to absorb semi-Arianism into Pro-Nicene orthodoxy in the longer term (accomplished by the Council of Constantinople 381). They bear the cost of accepting imperfect language in the interim but collect the benefit of institutional stability and eventual doctrinal victory. Their intellectual work reframes homoiousios as a stepping stone to homoousios rather than a final settlement.
narrative_ontology:constraint_stakeholder(homoousios_christology__semi_arian_reading, pro_nicene_ascendant_faction, beneficiary,
    powerful, generational, mobile, continental).
narrative_ontology:stakeholder_secondary_role(homoousios_christology__semi_arian_reading, pro_nicene_ascendant_faction, payer).

% Lay Christians and local communities who follow the doctrine taught by their bishops but have no voice in the councils where the homoiousios compromise is negotiated. They must accept whatever formulation their bishop endorses, whether that bishop genuinely holds the semi-arian reading or is performatively adopting it for institutional reasons. Their theological preferences are not represented in the constraint's design. They are trapped: leaving the church is schism, but staying means accepting whatever doctrinal formula imperial-backed councils impose.
narrative_ontology:constraint_stakeholder(homoousios_christology__semi_arian_reading, local_congregations, excluded,
    powerless, biographical, trapped, local).

% Formal councils (especially those called by the Emperor, such as the Council of Constantinople 360) that assemble bishops and issue decrees establishing the homoiousios formula as binding doctrine. The councils function as the formal apparatus for enforcing doctrinal consensus and resolving theological disputes through imperial-backed authority. They observe competing doctrinal claims and adjudicate which will be declared orthodox. The councils are the venue where Semi-Arian bishops perform their authority and where Pro-Nicene bishops strategically accept compromise while planning long-term victory.
narrative_ontology:constraint_stakeholder(homoousios_christology__semi_arian_reading, ecumenical_councils, agenda_setter,
    institutional, generational, analytical, continental).
narrative_ontology:stakeholder_secondary_role(homoousios_christology__semi_arian_reading, ecumenical_councils, observer).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(homoousios_christology__semi_arian_reading, semi_arian_bishops).
narrative_ontology:fixing_cost_class(homoousios_christology__semi_arian_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the problem of ecclesiastical schism by providing a doctrinal formulation that both Arian and Pro-Nicene parties can accept (or at least tolerate) as a basis for communion and shared practice. The homoiousios formula coordinates on a middle-ground language that avoids full doctrinal victory for either side, enabling the church to remain unified under imperial authority rather than fracturing into competing orthodox factions.
% TRANSFER_FUNCTION: Moves institutional legitimacy and administrative authority from contested doctrinal positions toward the imperial-backed councils and Semi-Arian bishops who broker the compromise. Arian and Pro-Nicene bishops transfer some degree of theological independence in exchange for remaining in communion; local congregations transfer voice in doctrinal questions to their bishops and the councils.
% ABSENT_VOICES: Lay theologians and local communities have no representation in the councils where the homoiousios compromise is negotiated and imposed. They would object (if heard) to doctrinal decisions made without their input and to the compressed choice set (homoiousios or schism) imposed by imperial authority. Also structurally absent: the voices of future generations (post-381) for whom this compromise will be revealed as transitional and superseded by Pro-Nicene orthodoxy — their retrospective judgment that the formula failed to solve the permanent problem it claimed to address.
% DISAPPEARANCE_RATIONALE: If the homoiousios compromise and its enforcement machinery disappeared, the church would immediately split into competing communions: Pro-Nicene bishops would move toward pure homoousios doctrine, and Arian bishops would either accept Arianism openly or seek alternative compromise positions. The coordinating function that holds them in communion would be lost, fragmenting the ecclesiastical structure the empire depends on for unified religious authority. However, the historical record shows that after 381, when imperial authority switched to Pro-Nicene orthodoxy, the disappearance occurred: the church DID rearrange around homoousios (Pro-Nicene) orthodoxy, and homoiousios was absorbed into Pro-Nicene tradition as a transitional stepping stone.
% FOUNDING_PROBLEM: After the Council of Nicaea (325), the church faced a crisis: the Pro-Nicene formula (homoousios) was contested by bishops who saw it as philosophically problematic and not properly grounded in scripture; Arian positions remained strong in many regions. The empire could not tolerate ecclesiastical schism because religious fragmentation threatened political order. A compromise formula was needed that would allow bishops of different theological commitments to remain in communion while avoiding the extremes of either full Arianism or the philosophically precise homoousios.
% FOUNDING_PROBLEM_CORROBORATION: The Semi-Arian bishops attest the founding problem was acute and the homoiousios formula solved it for their generation. However, historical scholarship and conciliar records (post-hoc analysis from the Pro-Nicene ascending faction and later historians) attest that the problem was NOT substantially resolved by the compromise — the underlying theological issues remained live, and the compromise was eventually superseded by Pro-Nicene orthodoxy becoming dominant by 381. The founding problem (the need for compromise) was real; but the solution (homoiousios as permanent orthodoxy) failed to persist. When the Emperor switched imperial backing to Pro-Nicene orthodoxy at Constantinople 381, homoiousios collapsed, proving it had never been a stable settlement — only a temporary institutional holding pattern.
narrative_ontology:disappearance_verdict(homoousios_christology__semi_arian_reading, world_rearranges).
narrative_ontology:founding_problem_status(homoousios_christology__semi_arian_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(homoousios_christology__semi_arian_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(homoousios_christology__semi_arian_reading, 'none', 1).
narrative_ontology:epsilon_provenance(homoousios_christology__semi_arian_reading, 0.42, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(homoousios_christology__semi_arian_reading_tests).
:- end_tests(homoousios_christology__semi_arian_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extracted metrics model homoiousios as INITIALLY a genuine coordination mechanism (low extractiveness 0.18 at 325, immediately after Nicaea, when the formula genuinely offered a middle ground). However, extractiveness RISES STEADILY over the interval (to 0.42 by 360, 0.52 by 381), driven by: (1) Semi-Arian bishops increasingly using the formula to consolidate their authority rather than as a bridge, (2) rising tension as Pro-Nicene bishops recognize the compromise is temporary and begin maneuvering for advantage, (3) the theater ratio RISING (0.08 to 0.42) as bishops defend homoiousios less as a genuine doctrine and more as institutional performance. The constraint is CLAIMED as rope (coordination function: true, avoiding schism: true) but the metrics reveal extraction accumulation — a coordination mechanism being converted into a power-consolidation tool. This divergence is the measurement the corpus exists to take. Suppression requirement rises modestly (0.22 to 0.48) as imperial enforcement must increase to maintain acceptance as the compromise loses voluntary appeal. The one shared time grid ensures every metric is authored at every examined point (325, 340, 360, 375, 381); no misalignment.
 *
 * PERSPECTIVAL GAP:
 *   From the Semi-Arian bishops' perspective (agenda-setter seat), homoiousios is a genuine coordination achievement — they have navigated between Arianism and Pro-Nicene excess to find a stable middle. From the Pro-Nicene bishops' perspective (payer seat), the compromise is a temporary concession they accept for institutional stability but plan to supersede. From the Arian bishops' perspective (also payer seat), homoiousios is a move AWAY from their position, a displacement they accept under imperial pressure. From the imperial authority's perspective (beneficiary seat), the constraint works as long as it maintains unified ecclesiastical order. The engine will compute DIFFERENT types from these different structural positions: Semi-Arian leadership may compute as rope-beneficiary (they coordinate and benefit); Pro-Nicene and Arian payers will compute as constrained; the imperial authority computes as beneficiary-organizer. This structural divergence is exactly the perspectival asymmetry the framework is designed to surface.
 *
 * DIRECTIONALITY LOGIC:
 *   Semi-Arian bishops: d near 0.0 (beneficiary end) — they author the formula, benefit from its adoption as doctrine, and control the councils that enforce it. Their exit option is identity_locked (professional identity fused with the semi-arian position), but at the beneficiary end even identity-lock does not raise d because they collect from the constraint. Pro-Nicene bishops: d near 0.7 (target end) — they are pressured to accept the homoiousios formula, constrained exit (leaving means schism, which they oppose for institutional reasons), and organized power (they cannot unilaterally reject the compromise but will eventually absorb it into their victory). Arian bishops: d near 0.75 (target end) — similar pressure, constrained exit, organized power, but facing displacement toward Pro-Nicene terrain rather than Pro-Nicene bishops facing displacement toward Semi-Arian terrain. Imperial authority: d near 0.2 (beneficiary end) — they benefit from schism prevention, operate as arbitrage-capable (can switch support among factions), and hold institutional power to enforce. The directionality derivation chain produces these values from the declared beneficiary/victim structure and exit modulation; no override is needed.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding_problem_status is DEAD and the disappearance_verdict is WORLD_REARRANGES, producing the mandatrophy mismatch that flags a zombie constraint. The founding problem (the need for a compromise formula to prevent schism in the immediate post-Nicaea crisis) was acute and real but was NOT solved by homoiousios — instead, the formula became a temporary institutional holding pattern. By 381, the Pro-Nicene faction had consolidated enough power to simply declare homoousios orthodox at Constantinople, absorbing the Semi-Arian position into Pro-Nicene tradition and rendering homoiousios obsolete. The constraint persisted not because it solved the founding problem (the problem is gone: Pro-Nicene consensus exists), but because bishops and the imperial authority had invested institutional identity in maintaining it. This is the classic mandatrophy signature: a constraint whose founding purpose has been accomplished or abandoned, but which persists as organizational theater and identity performance. The theater_ratio rising from 0.08 to 0.42 is direct evidence of this mandatrophy trajectory — the formula is increasingly defended for institutional reasons, not doctrinal ones.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    semi_arian_theological_coherence,
    'Is homoiousios a genuinely coherent theological position, or is it inherently unstable and trapped between Arianism and Pro-Nicene doctrine?',
    'Theological analysis of the formula''s logical implications and historical reception. If the formula can be defended as a stable middle ground with internal consistency, it is coherent; if its logic necessarily drifts toward one side or collapses under scrutiny, it is unstable.',
    'If unstable, the rising extraction and theater metrics reflect the inevitable drift of the compromise toward collapse, not primarily an institutional capture. If coherent, the constraint''s failure to persist reflects political victory, not theoretical incoherence — the Pro-Nicene faction simply outmaneuvered the Semi-Arians.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(semi_arian_theological_coherence, conceptual, 'Whether homoiousios is logically defensible or inherently contradictory.').

omega_variable(
    semi_arian_vs_pro_nicene_trajectory,
    'Did Semi-Arian bishops genuinely believe homoiousios could be a permanent doctrinal settlement, or did they understand it as a tactical retreat anticipating eventual absorption into Pro-Nicene orthodoxy?',
    'Careful reading of Semi-Arian correspondence and theological writings from the 360s–380s: do their arguments defend homoiousios as final truth, or do they repeatedly hedge toward homoousios? Compare the rhetorical confidence of Semi-Arian bishops in 340 vs. 375.',
    'If Semi-Arians genuinely believed in the formula''s permanence, the constraint is a failed coordination attempt. If they anticipated absorption, the rising theater ratio reflects conscious performance and the constraint is already a captured/transitional form by 360.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(semi_arian_vs_pro_nicene_trajectory, empirical, 'Whether Semi-Arian commitment was genuine long-term doctrine or tactical interim position.').

omega_variable(
    reading_committer_framing_ambiguity,
    'Is this constraint the Semi-Arian reading''s own self-understanding, or is it a reconstruction imposed by a later (Pro-Nicene) analyst who sees homoiousios as a failed intermediate step?',
    'The authored claim, metrics, and omega answers reflect the reading AS UNDERSTOOD BY SEMI-ARIAN BISHOPS themselves (rule 1: generate the reading as it appears from within that frame, not as a Pro-Nicene analyst assessing its failure). If the reading is instead authored as an external judgment, the analysis is a guest composition, not the reading''s own episteme.',
    'If this is the Semi-Arian self-understanding, the low extractiveness at t0 and the honest trajectory are correct as described. If this is an external reconstruction, the entire ε profile needs revision to capture Semi-Arian confidence rather than historian''s hindsight about failure.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_committer_framing_ambiguity, conceptual, 'Whose framing is this constraint authored from: Semi-Arian self-understanding or Pro-Nicene retrospective analysis?').

omega_variable(
    imperial_enforcement_vs_voluntary_acceptance,
    'How much of the constraint''s persistence (especially post-360) is enforced by imperial decree, and how much reflects genuine episcopal buy-in to homoiousios as doctrine?',
    'Examine conciliar records and imperial legislation: when imperial support is withdrawn or the emperor switches sides (as happened post-381), how quickly does the episcopal consensus shift? If bishops immediately abandon homoiousios, the constraint was held primarily by imperial enforcement; if the formula retains adherents, some voluntary acceptance existed.',
    'High imperial enforcement would support a snare classification for payer bishops and explain the rising suppression metric. Voluntary acceptance would support the rope classification claimed. The measurement interval (325–381) catches the shift: by 381, imperial authority switches allegiance to Pro-Nicene orthodoxy, causing the constraint''s rapid collapse.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(imperial_enforcement_vs_voluntary_acceptance, empirical, 'Whether homoiousios persistence depends on imperial enforcement or genuine episcopal endorsement.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(homoousios_christology__semi_arian_reading, 325, 381).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(homo_tr_t325, homoousios_christology__semi_arian_reading, theater_ratio, 325, 0.08).
narrative_ontology:measurement(homo_tr_t340, homoousios_christology__semi_arian_reading, theater_ratio, 340, 0.14).
narrative_ontology:measurement(homo_tr_t360, homoousios_christology__semi_arian_reading, theater_ratio, 360, 0.29).
narrative_ontology:measurement(homo_tr_t375, homoousios_christology__semi_arian_reading, theater_ratio, 375, 0.35).
narrative_ontology:measurement(homo_tr_t381, homoousios_christology__semi_arian_reading, theater_ratio, 381, 0.42).

% Extraction over time
narrative_ontology:measurement(homo_be_t325, homoousios_christology__semi_arian_reading, base_extractiveness, 325, 0.18).
narrative_ontology:measurement(homo_be_t340, homoousios_christology__semi_arian_reading, base_extractiveness, 340, 0.28).
narrative_ontology:measurement(homo_be_t360, homoousios_christology__semi_arian_reading, base_extractiveness, 360, 0.42).
narrative_ontology:measurement(homo_be_t375, homoousios_christology__semi_arian_reading, base_extractiveness, 375, 0.48).
narrative_ontology:measurement(homo_be_t381, homoousios_christology__semi_arian_reading, base_extractiveness, 381, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(homo_su_t325, homoousios_christology__semi_arian_reading, suppression_requirement, 325, 0.22).
narrative_ontology:measurement(homo_su_t340, homoousios_christology__semi_arian_reading, suppression_requirement, 340, 0.31).
narrative_ontology:measurement(homo_su_t360, homoousios_christology__semi_arian_reading, suppression_requirement, 360, 0.38).
narrative_ontology:measurement(homo_su_t375, homoousios_christology__semi_arian_reading, suppression_requirement, 375, 0.42).
narrative_ontology:measurement(homo_su_t381, homoousios_christology__semi_arian_reading, suppression_requirement, 381, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(homoousios_christology__semi_arian_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(homoousios_christology__semi_arian_reading, 0.12).
narrative_ontology:affects_constraint(homoousios_christology__semi_arian_reading, homoousios_christology__pro_nicene_reading).
narrative_ontology:affects_constraint(homoousios_christology__semi_arian_reading, homoousios_christology__arian_reading).

% DUAL FORMULATION NOTE:
% This constraint is part of the homoousios_christology constraint family. The kernel is the standing ecclesiastical commitment about Christ's nature, adjudicated through conciliar decrees and imperial authority. Three readings instantiate three separate constraints: (1) arian_reading (Christ as created/subordinate), (2) pro_nicene_reading (Christ as homoousios/consubstantial), (3) this semi_arian_reading (Christ as homoiousios/similar substance). They share the same kernel (the commitment to resolve Christ's nature via church councils) but differ in ε, beneficiary structure, and historical trajectory. The semi_arian_reading influenced both siblings — it shaped the pro_nicene_reading's eventual victory by becoming the stepping stone toward full Pro-Nicene dominance (post-381), and it constrained the arian_reading's space by moving the ecclesiastical center of gravity toward Pro-Nicene terrain. The three stories are linked bidirectionally: semi_arian_reading affects both siblings via its influence on the dispute's terms; both siblings' eventual dominance/displacement affects this reading's classification and historical outcome.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
