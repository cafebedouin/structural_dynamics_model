% ============================================================================
% CONSTRAINT STORY: vatican_ii_authority__rupture_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_vatican_ii_authority__rupture_reading, []).

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
    narrative_ontology:cs_created_at/2,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: vatican_ii_authority__rupture_reading
 *   human_readable: Vatican II Authority Crisis (Rupture Reading): Doctrinal Break and Institutional Capture
 *   domain: theology/ecclesiology/religious_authority
 *
 * SUMMARY:
 *   Vatican II (1962-1965) represents one of the most consequential contested
 *   events in modern Catholicism. This constraint instantiates the RUPTURE
 *   READING: the Council represents a substantive break with prior teaching,
 *   documents contain doctrinal errors or irreconcilable contradictions with
 *   tradition, and the post-conciliar Church operates in crisis under
 *   captured authority. This is ONE reading of a contested kernel
 *   (vatican_ii_authority). The rupture reading claims that Vatican II's
 *   doctrinal shifts — on the nature of the Church, its relationship to
 *   non-Catholic bodies, the role of episcopal collegiality, liturgical
 *   reform, religious freedom, and the possibility of salvation outside the
 *   visible Church — constitute contradictions of prior authoritative
 *   teaching rather than legitimate developments. The beneficiary is the
 *   modernist theological faction, which used Council documents to advance
 *   hermeneutics of reform and doctrinal reinterpretation. The victims are
 *   traditional Catholic identity (which depends on continuity with
 *   pre-conciliar teaching) and doctrinal stability (which depends on
 *   unchanging authority). The constraint is maintained through suppression
 *   mechanisms: papal insistence that the Council is binding, institutional
 *   enforcement of post-conciliar reforms, and marginalization of traditional
 *   critiques through canonical penalties and intellectual delegitimation.
 *   This reading coexists with the continuity reading (Vatican II represents
 *   organic development, not rupture) and the composite overdetermination
 *   reading (Vatican II is an overdetermined composite of multiple doctrinal
 *   shifts with incompatible rationales, making unified 'rupture' or
 *   'continuity' classification impossible). From the rupture reading
 *   perspective, this measurement interval (1962-1975) captures the
 *   constraint's genesis and early manifestation: extractiveness and
 *   suppression both rise significantly as post-conciliar reforms are
 *   implemented and traditional objections are systematically excluded from
 *   institutional voice. Theater ratio rises as the institutional Church
 *   develops elaborate hermeneutical frameworks ('hermeneutics of
 *   continuity') to maintain plausibility that the Council represents no
 *   rupture — performative continuity language masks substantive reform
 *   implementation.
 *
 * KEY AGENTS:
 *   - Traditional Catholic Faithful: Primary victim (powerless/identity_locked) — bear the identity dissolution and doctrinal incoherence created by Council's break with prior teaching; cannot exit without abandoning Catholic identity; experience maximum extraction
 *   - SSPX and Sedevacantist Communities: Organized victims (organized/constrained) — maintain traditional identity and doctrinal coherence through schism/canonical exclusion; coordinating alternative ecclesiastical structure under heavy suppression
 *   - Modernist Theological Faction: Primary beneficiary (institutional/arbitrage) — gain hermeneutical authority to reinterpret doctrine; capture Council documents to advance reform agenda; experience constraint as coordination (solving problem of doctrinal adaptation to modernity)
 *   - Post-Conciliar Papal Leadership: Institutional beneficiary with constrained agency (powerful/mobile) — implement Council reforms while claiming continuity; manage the contradiction between rupture narrative and continuity claims; attempt to synthesize modernity with tradition through hermeneutical work
 *   - Post-Conciliar Institutional Church: Institutional persistence mechanism (institutional/constrained) — reforms are implemented through inertia and institutional investment; maintains theater of continuity despite underlying doctrinal instability
 *   - Analytical Observer: Sees false summit (analytical/analytical) — risks naturalizing rupture as inevitable institutional evolution rather than contestable doctrinal choice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vatican_ii_authority__rupture_reading, 0.68).
domain_priors:suppression_score(vatican_ii_authority__rupture_reading, 0.72).
domain_priors:theater_ratio(vatican_ii_authority__rupture_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vatican_ii_authority__rupture_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(vatican_ii_authority__rupture_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(vatican_ii_authority__rupture_reading, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vatican_ii_authority__rupture_reading, snare).
narrative_ontology:human_readable(vatican_ii_authority__rupture_reading, "Vatican II Authority Crisis (Rupture Reading): Doctrinal Break and Institutional Capture").
narrative_ontology:topic_domain(vatican_ii_authority__rupture_reading, "theology/ecclesiology/religious_authority").

domain_priors:requires_active_enforcement(vatican_ii_authority__rupture_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vatican_ii_authority__rupture_reading, '4c2a8f53-a9c3-41bc-955c-2c84da991929').
narrative_ontology:cs_kernel_codification('4c2a8f53-a9c3-41bc-955c-2c84da991929', formalized).
narrative_ontology:cs_authority_grounding('4c2a8f53-a9c3-41bc-955c-2c84da991929', extraction).
narrative_ontology:cs_interpretation_layer_present('4c2a8f53-a9c3-41bc-955c-2c84da991929').
narrative_ontology:cs_reading_relation('4c2a8f53-a9c3-41bc-955c-2c84da991929', vatican_ii_authority__continuity_reading, coexists_with).
narrative_ontology:cs_reading_relation('4c2a8f53-a9c3-41bc-955c-2c84da991929', vatican_ii_authority__composite_overdetermination_reading, influences).
narrative_ontology:cs_axiom('4c2a8f53-a9c3-41bc-955c-2c84da991929', foundational, council_documents_contain_contradictions).
narrative_ontology:cs_axiom_status(council_documents_contain_contradictions, holdable).
narrative_ontology:cs_axiom_grounding('4c2a8f53-a9c3-41bc-955c-2c84da991929', council_documents_contain_contradictions, empirically_contingent).
narrative_ontology:cs_axiom('4c2a8f53-a9c3-41bc-955c-2c84da991929', foundational, magisterial_continuity_binding).
narrative_ontology:cs_axiom_status(magisterial_continuity_binding, holdable).
narrative_ontology:cs_axiom_grounding('4c2a8f53-a9c3-41bc-955c-2c84da991929', magisterial_continuity_binding, deontological).
narrative_ontology:cs_created_at('4c2a8f53-a9c3-41bc-955c-2c84da991929', '2026-02-26T00:00:00Z').
narrative_ontology:cs_kernel_id(vatican_ii_authority__rupture_reading, vatican_ii_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vatican_ii_authority__rupture_reading, modernist_theological_faction).
narrative_ontology:constraint_victim(vatican_ii_authority__rupture_reading, traditional_catholic_identity).
narrative_ontology:constraint_victim(vatican_ii_authority__rupture_reading, doctrinal_stability).
narrative_ontology:constraint_victim(vatican_ii_authority__rupture_reading, conciliar_interpretive_authority).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: TRADITIONAL CATHOLIC FAITHFUL (SNARE) — Identity-locked by 2,000 years of doctrinal continuity and sacramental practice. Cannot exit without abandoning Catholic identity itself. Vatican II documents create doctrinal rupture that dissolves the normative ground of their faith practice. Maximum experienced extraction: the constraint forces choice between doctrinal coherence and institutional obedience. Suppression is total — the magisterium claims Council is binding; rejecting it means excommunication or schism.
constraint_indexing:constraint_classification(vatican_ii_authority__rupture_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(global))).

% PERSPECTIVE 2: SSPX AND SEDEVACANTIST COMMUNITIES (TANGLED ROPE) — Organized agents with some coordination function (preserving traditional Latin liturgy, maintaining doctrinal continuity claims) but operating under heavy suppression (canonical penalties, institutional exclusion). The constraint forces a genuine coordination problem (How do we maintain apostolic succession and doctrinal identity under rupture?) alongside extractive coercion (institutional pressure to accept the Council as binding). Exit is constrained but possible — sedevacantism and SSPX status are live, costly alternatives.
constraint_indexing:constraint_classification(vatican_ii_authority__rupture_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: MODERNIST THEOLOGICAL FACTION (ROPE) — Primary beneficiary. Vatican II documents are read as authorization for doctrinal reinterpretation ('living Church', ressourcement theology, hermeneutics of reform). This faction experiences the constraint as pure coordination: the Council solves the problem of adapting Catholic teaching to modernity without appearing to contradict the deposit of faith. Their exit option is arbitrage — they can reinterpret Council documents to mean almost anything, maintaining institutional plausibility while advancing modernist agenda.
constraint_indexing:constraint_classification(vatican_ii_authority__rupture_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: REFORMIST PAPAL FACTION (SCAFFOLD) — Post-conciliar popes (Paul VI through John Paul II) experienced the Council as a temporary coordination mechanism with an implicit sunset: implement Council reforms, stabilize the Church around Vatican II's teachings, and reach synthesis with modernity. The sunset was supposed to be reached by John Paul II's papacy. From this perspective, suppression (0.72) is moderate because the faction has agency — they can emphasize continuity hermeneutics or reform hermeneutics as needed. Theater ratio (0.58) reflects the performative work of maintaining both continuity and reform narratives simultaneously.
constraint_indexing:constraint_classification(vatican_ii_authority__rupture_reading, scaffold,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: POST-CONCILIAR INSTITUTIONAL CHURCH (PITON) — The institutional apparatus (episcopal conferences, Vatican II's legislative machinery, the reformed liturgy, updated catechesis) persists through inertia despite the underlying doctrinal crisis. Theater ratio (0.58 at institutional level, higher at specific ritual levels like liturgy) reflects that much post-conciliar activity is performative: implementing Council documents while avoiding their logical implications. The constraint is largely kept in place by the weight of institutional investments (seminaries reorganized, parishes restructured, religious education rewritten) rather than by active doctrinal conviction.
constraint_indexing:constraint_classification(vatican_ii_authority__rupture_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / FALSE NATURAL LAW VIEW (MOUNTAIN) — This perspective risks naturalizing the rupture itself as an inevitable law of institutional evolution: modern institutions must modernize or die; doctrinal change is inevitable; tradition cannot withstand secular pressure. This framing presents rupture as a law of nature rather than a contingent institutional choice. However, the structural data (beneficiary faction, victim identity, suppression mechanism) contradicts the mountain classification — the engine's false summit detector will flag this as naturalization of a contestable institutional arrangement.
constraint_indexing:constraint_classification(vatican_ii_authority__rupture_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(vatican_ii_authority__rupture_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(vatican_ii_authority__rupture_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(vatican_ii_authority__rupture_reading, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(vatican_ii_authority__rupture_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(vatican_ii_authority__rupture_reading, TR),
    TR >= 0.70.

:- end_tests(vatican_ii_authority__rupture_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. The rupture reading claims that Vatican II's doctrinal shifts extracted significant institutional authority from the traditional faithful and transferred it to the modernist faction. The measurement trajectory (0.35 → 0.68 across interval 1962-1975) reflects the constraint's intensification as post-conciliar reforms were implemented. Base extractiveness is high because the constraint forces beneficiaries (modernists) to gain doctrinal authority while victims (traditionalists) lose authority and identity coherence. Suppression (0.72): High. The constraint is maintained through multiple suppression mechanisms: (1) papal declarations of Council's binding authority; (2) institutional enforcement of post-conciliar reforms (liturgical changes, seminary restructuring); (3) canonical penalties for Council rejection (SSPX excommunication, sedevacantist delegitimation); (4) intellectual suppression (marginalization of traditional scholarship, exclusion from episcopal appointments). Suppression rises sharply (0.42 → 0.72) as enforcement infrastructure develops. Theater ratio (0.58): Moderate. The constraint has moderate performative content because post-conciliar leadership must maintain two contradictory narratives: that Vatican II represents no rupture (hermeneutics of continuity) while simultaneously implementing radical reforms. The theater consists of elaborate theological frameworks designed to prove continuity where structural observers see rupture. Theater ratio rises (0.35 → 0.58) as hermeneutical infrastructure develops to manage the contradiction. Claimed type: SNARE. The constraint operates as a snare from the victim perspective (traditional faithful are trapped by identity-lock and maximum suppression) and as a rope from the beneficiary perspective (modernists experience the Council as solving a legitimate coordination problem). The engine classification at the powerless/identity_locked/biographical context produces snare; the institutional/arbitrage/immediate context produces rope. The snare classification reflects the victim-centered measurement: the constraint extracts identity authority from those who cannot exit.
 *
 * PERSPECTIVAL GAP:
 *   The rupture reading exhibits maximum perspectival divergence. The traditional faithful (powerless/identity_locked) experience snare: doctrinal rupture that forces impossible choice between coherence and obedience, enforced by suppression. SSPX communities (organized/constrained) experience tangled rope: genuine coordination function (maintaining doctrinal tradition) alongside heavy extraction and exclusion. The modernist faction (institutional/arbitrage) experiences rope: the Council solves the legitimate problem of adapting tradition to modernity, with no experienced extraction (modernists see beneficiary status as earned authority from rigorous scholarship). The post-conciliar popes (powerful/mobile) experience scaffold: temporary coordination mechanism with implicit sunset to be reached through synthesis and doctrinal stabilization. The institutional Church (institutional/constrained) experiences piton: post-conciliar structure persists through inertia despite underlying incoherence. The analytical observer risks mountain: naturalizing rupture as inevitable institutional evolution. This perspectival range (snare → rope → scaffold → piton → mountain) is diagnostic of how the same structural facts are experienced radically differently by agents in different positions relative to the constraint.
 *
 * DIRECTIONALITY LOGIC:
 *   The rupture reading's directionality profile depends on whether the agent benefits from doctrinal reinterpretation (low d → low χ) or is harmed by doctrinal rupture (high d → high χ). The modernist faction receives low d (beneficiary + arbitrage exit) → negative effective extraction — they experience the constraint as enabling (they gain authority). The traditional faithful receive high d (victim + identity_locked exit) → maximum effective extraction — they experience the constraint as entrapping. The analytical observer (analytical/analytical) receives canonical d ≈ 0.73 → moderate effective extraction, reflecting the view that rupture creates legitimate complexity that any modernizing institution must navigate. The directionality computation reveals the asymmetry: the beneficiary's exit option is nearly free (arbitrage = reinterpretation authority), while the victim's exit option is identity-dissolving (identity_locked = losing Catholic self-understanding). This asymmetry is structurally encoded in the constraint's χ formula.
 *
 * MANDATROPHY ANALYSIS:
 *   The rupture reading faces the mandatrophy at extractiveness 0.68 > 0.70 threshold: it must resolve whether the observed institutional changes (liturgical reform, doctrinal reinterpretation, authority shifts) constitute legitimate coordination with extractive elements (tangled rope), or pure extraction under hermeneutical cover (snare), or something that fails to fall into either category (composite degradation). The rupture reading resolves mandatrophy by accepting the snare classification from the victim perspective: the constraint extracts authority from traditional identity while maintaining the appearance of continuity through performative hermeneutical work (theater_ratio 0.58). The beneficiary gets rope; the victim gets snare. The snare perspective is primary because it captures the structural asymmetry — the constraint's success depends on suppressing the victim's capability to reject it, while the beneficiary's participation is voluntary (arbitrage exit). Mandatrophy resolution: the constraint is a snare WITH beneficiaries who experience it as rope. This is diagnostically coherent — snares often look like rope to beneficiaries, who perceive only the coordination function while suppression operates on others. The resolution is NOT that the constraint is both snare and rope (incoherent), but that the same structural constraint produces radically different classifications depending on structural position, and the primary classification (snare) reflects the victim's perspective where suppression operates most forcefully.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    doctrinal_error_threshold,
    'What constitutes a ''doctrinal error'' grave enough to invalidate a Council or trigger its rejection — quantitatively small reinterpretations, explicit contradictions of prior definitions, or qualitative rupture in the authority structure itself?',
    'Comparative analysis of Council documents vs. pre-conciliar teaching on specific doctrines (papal authority, nature of the Church, relationship to non-Catholic bodies); examination of hermeneutical frameworks used by continuity vs. rupture readings to assess whether differences are errors or developments',
    'If threshold is quantitative reinterpretation: many continuity readings survive scrutiny, and rupture reading loses force. If threshold is qualitative authority rupture: rupture reading is strengthened — the question becomes whether Vatican II''s interpretive methodology broke with prior methodology, not whether specific doctrines changed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(doctrinal_error_threshold, conceptual, 'Definition of doctrinal error grave enough to constitute rupture').

omega_variable(
    hermeneutics_kernel_contest,
    'Is the hermeneutical framework used to interpret Vatican II documents (''hermeneutics of continuity'' vs. ''hermeneutics of reform'') itself part of the doctrinal content (and therefore subject to the same continuity/rupture question), or is it a meta-level interpretive tool (and therefore cannot be evaluated as continuity/rupture)?',
    'Investigation of whether Vatican II documents themselves claim a hermeneutical method; whether prior councils used different methods; whether the interpretive method is taught as doctrine or applied as scholarship; analysis of self-referentiality in the rupture claim itself',
    'If hermeneutics is content: the rupture reading applies to itself — ''hermeneutics of reform'' is an error that caused rupture, meaning the reading is self-refuting or recursively unstable. If hermeneutics is meta-tool: rupture claim remains stable but becomes less falsifiable (disputes move into hermeneutical disagreement rather than doctrinal fact).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(hermeneutics_kernel_contest, conceptual, 'Whether hermeneutical method is doctrinal content or meta-level interpretive tool').

omega_variable(
    authority_breakdown_temporality,
    'Did Vatican II cause the post-conciliar crisis, or did it merely reveal an already-present authority crisis in the modern Church? Did the Council rupture from tradition, or did tradition''s authority over modernity already collapse before Vatican II?',
    'Historical analysis of pre-conciliar Church institutional health, seminary populations, catechesis effectiveness, liturgical attendance, and authority compliance in late 1950s vs. early 1960s; comparison of crisis trajectories in conciliar vs. non-conciliar Christian bodies (Orthodox, Protestant) over the same period; causal attribution analysis separating Council from broader secularization dynamics',
    'If Council caused crisis: rupture reading is strengthened — Council decisions directly harmed institutional continuity. If crisis pre-existed: rupture reading explains symptoms but not causes — the crisis may reflect modernity''s assault on tradition, not Vatican II''s doctrinal errors. Extraction mechanism shifts from institutional betrayal to historical inevitability.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(authority_breakdown_temporality, empirical, 'Whether Vatican II caused or revealed the post-conciliar crisis').

omega_variable(
    beneficiary_structural_proof,
    'Are modernist theological gains (''living Church'' hermeneutics, pluralism in doctrine, openness to secular learning) genuine beneficiary extraction demonstrating Council captured by modernists, or are they legitimate doctrinal developments that would have occurred anyway through hermeneutical evolution?',
    'Comparative institutional analysis: trajectory of modernist influence in pre-conciliar theology vs. post-conciliar implementation; examination of whether modernist axioms were explicitly rejected or endorsed by Council documents; analysis of alternative development paths Vatican II foreclosed or preserved',
    'If modernist gains are capture evidence: beneficiary declaration is structurally justified; snare classification is supported. If gains are legitimate development: beneficiary faction may be pursuing valid theological trajectories that the Council authorized rather than imposed; extraction mechanism dissolves into disagreement about legitimate development.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_structural_proof, empirical, 'Whether modernist theological gains represent captured extraction or legitimate development').

omega_variable(
    identity_lock_reversibility,
    'Is the identity-lock experienced by traditional Catholics structurally irreversible (exit requires becoming a different person), or is it a psychological identification that could shift if doctrinal reinterpretation frameworks changed (making exit costlier but not identity-constituting)?',
    'Ethnographic/interview analysis of why traditional Catholics remain in or leave the Church; examination of narratives separating structural impossibility (''I cannot be Catholic and accept Vatican II'') from psychological cost (''I would be a different person''); analysis of Vatican III scenarios or other counterfactuals where Council might have ruled differently',
    'If identity-lock is true structural irreversibility: the constraint operates as snare even if structural barriers were removed. If it is high-cost psychological identification: the victim relationship could change if doctrinal authority shifted; snare classification may overstate the structural entrenchment.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_reversibility, empirical, 'Whether traditional Catholic identity-lock is structurally irreversible or psychologically contingent').

omega_variable(
    rupture_vs_composite_kernel_boundary,
    'Is Vatican II a single coherent rupture event (one doctrinal break), or is it an overdetermined composite of multiple distinct doctrinal shifts with incompatible rationales that cannot be unified into either ''rupture'' or ''continuity''?',
    'Structural decomposition of Vatican II Council documents into elementary doctrinal claims; analysis of whether each claim has the same relationship to prior teaching (all rupture, all continuity, or mixed); examination of whether a unified ''rupture'' reading requires smoothing over internal contradictions in the Council''s own assertions',
    'If single rupture: rupture reading remains stable and falsifiable. If composite overdetermination: the rupture reading is true of some Council claims but false of others; the reading must either fracture into sub-readings (separate constraint stories) or acknowledge that the ''rupture'' framing is retrospective narrative unification of contradictory claims. This directly triggers the composite_overdetermination_reading as an alternative constraint.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(rupture_vs_composite_kernel_boundary, empirical, 'Whether Vatican II is a unified rupture or composite of incompatible doctrinal shifts').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vatican_ii_authority__rupture_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vat2_rup_theater_1962, vatican_ii_authority__rupture_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement(vat2_rup_theater_1966, vatican_ii_authority__rupture_reading, theater_ratio, 4, 0.48).
narrative_ontology:measurement(vat2_rup_theater_1975, vatican_ii_authority__rupture_reading, theater_ratio, 10, 0.58).

% Extraction over time
narrative_ontology:measurement(vat2_rup_extract_1962, vatican_ii_authority__rupture_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(vat2_rup_extract_1966, vatican_ii_authority__rupture_reading, base_extractiveness, 4, 0.58).
narrative_ontology:measurement(vat2_rup_extract_1975, vatican_ii_authority__rupture_reading, base_extractiveness, 10, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(vat2_rup_suppress_1962, vatican_ii_authority__rupture_reading, suppression_requirement, 0, 0.42).
narrative_ontology:measurement(vat2_rup_suppress_1966, vatican_ii_authority__rupture_reading, suppression_requirement, 4, 0.68).
narrative_ontology:measurement(vat2_rup_suppress_1975, vatican_ii_authority__rupture_reading, suppression_requirement, 10, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vatican_ii_authority__rupture_reading, identity_coordination).
narrative_ontology:affects_constraint(vatican_ii_authority__rupture_reading, vatican_ii_authority__continuity_reading).
narrative_ontology:affects_constraint(vatican_ii_authority__rupture_reading, vatican_ii_authority__composite_overdetermination_reading).
narrative_ontology:affects_constraint(vatican_ii_authority__rupture_reading, post_conciliar_liturgical_reform).
narrative_ontology:affects_constraint(vatican_ii_authority__rupture_reading, episcopal_collegiality_authority_shift).

% DUAL FORMULATION NOTE:
% Vatican II authority kernel decomposes into three structurally distinct constraint stories with different ε values and different beneficiary/victim structures. The rupture reading (this story) claims Council documents contain doctrinal contradictions; ε = 0.68 (high extraction). The continuity reading claims Council represents organic development; ε ≈ 0.30 (coordination with moderate extraction). The composite_overdetermination reading claims Council is internally incoherent composite; ε ≈ 0.42 (tangled rope with high epistemic fragility). Each reading is a coherent constraint story with its own authority structure and victim/beneficiary asymmetries. They are not the same constraint viewed from different angles — they are different constraints (with different ε values and different extraction mechanisms) that share a kernel text. The readings network because rupture truth conditions affect continuity plausibility: if rupture reading is correct (contradictions exist), continuity reading loses empirical support; if continuity reading is correct (development is organic), rupture reading's core claim is falsified.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
