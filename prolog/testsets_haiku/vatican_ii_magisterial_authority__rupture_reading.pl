% ============================================================================
% CONSTRAINT STORY: vatican_ii_magisterial_authority__rupture_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_vatican_ii_magisterial_authority__rupture_reading, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_non_agent/2,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: vatican_ii_magisterial_authority__rupture_reading
 *   human_readable: Vatican II Magisterial Authority (Rupture Reading)
 *   domain: institutional/theological/hermeneutical
 *
 * SUMMARY:
 *   Vatican II (1962–1965) represents a flashpoint in Catholic institutional
 *   self-understanding. The RUPTURE READING holds that the Council's texts
 *   authorize fundamental theological reorientation incompatible with
 *   pre-conciliar teaching: religious freedom (DH) contradicts prior
 *   condemnations of indifferentism; the 'pilgrim Church' ecclesiology
 *   contradicts exclusive-truth claims; vernacular experimentation
 *   contradicts liturgical universalism. This reading is ONE OF THREE
 *   interpretations of the same contested kernel. The reformist institutional
 *   hierarchy enforces this reading through seminary curricula, magisterial
 *   language-setting, and active suppression of traditionalist and
 *   continuity-emphasis frameworks. The constraint operates as tangled_rope:
 *   genuine coordination (adapting institutional posture to modernity) fused
 *   with asymmetric extraction (privileging reformist interpretation,
 *   suppressing others). The rupture reading is not empirically false—it is a
 *   constructed interpretive choice with winners and losers.
 *
 * KEY AGENTS:
 *   - Reformist theological bloc: benefits from rupture reading, identity-fused with post-conciliar positions
 *   - Post-conciliar institutional hierarchy: agenda-setter, enforces rupture interpretation via curriculum/discipline
 *   - Traditionalist resistance: pays the cost, trapped exit, voices suppressed
 *   - Pre-conciliar systematic theology: treated as obsolete, no seat at interpretation table
 *   - Vatican II Studies historians: excluded, possess knowledge of how rupture reading was constructed
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vatican_ii_magisterial_authority__rupture_reading, 0.68).
domain_priors:suppression_score(vatican_ii_magisterial_authority__rupture_reading, 0.71).
domain_priors:theater_ratio(vatican_ii_magisterial_authority__rupture_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vatican_ii_magisterial_authority__rupture_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(vatican_ii_magisterial_authority__rupture_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(vatican_ii_magisterial_authority__rupture_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vatican_ii_magisterial_authority__rupture_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(vatican_ii_magisterial_authority__rupture_reading, resistance, 0.74).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vatican_ii_magisterial_authority__rupture_reading, tangled_rope).
narrative_ontology:human_readable(vatican_ii_magisterial_authority__rupture_reading, "Vatican II Magisterial Authority (Rupture Reading)").
narrative_ontology:topic_domain(vatican_ii_magisterial_authority__rupture_reading, "institutional/theological/hermeneutical").

domain_priors:requires_active_enforcement(vatican_ii_magisterial_authority__rupture_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vatican_ii_magisterial_authority__rupture_reading, '48ae9f9d-523f-4ba9-865e-3129014967ac').
narrative_ontology:cs_kernel_codification('48ae9f9d-523f-4ba9-865e-3129014967ac', fixed_text).
narrative_ontology:cs_authority_grounding('48ae9f9d-523f-4ba9-865e-3129014967ac', extraction).
narrative_ontology:cs_interpretation_layer_present('48ae9f9d-523f-4ba9-865e-3129014967ac').
narrative_ontology:cs_reading_relation('48ae9f9d-523f-4ba9-865e-3129014967ac', vatican_ii_magisterial_authority__continuity_reading, forecloses).
narrative_ontology:cs_reading_relation('48ae9f9d-523f-4ba9-865e-3129014967ac', vatican_ii_magisterial_authority__composite_overdetermination_reading, influences).
narrative_ontology:cs_axiom('48ae9f9d-523f-4ba9-865e-3129014967ac', foundational, vatican_ii_authorizes_doctrinal_discontinuity).
narrative_ontology:cs_axiom_status(vatican_ii_authorizes_doctrinal_discontinuity, holdable).
narrative_ontology:cs_axiom_grounding('48ae9f9d-523f-4ba9-865e-3129014967ac', vatican_ii_authorizes_doctrinal_discontinuity, deontological).
narrative_ontology:cs_axiom('48ae9f9d-523f-4ba9-865e-3129014967ac', foundational, error_lacks_institutional_right_to_institutional_expression).
narrative_ontology:cs_axiom_status(error_lacks_institutional_right_to_institutional_expression, overridden).
narrative_ontology:cs_axiom_grounding('48ae9f9d-523f-4ba9-865e-3129014967ac', error_lacks_institutional_right_to_institutional_expression, deontological).
narrative_ontology:cs_reference_frame('48ae9f9d-523f-4ba9-865e-3129014967ac', pre_conciliar_doctrinal_settlement).
narrative_ontology:cs_drift_state('48ae9f9d-523f-4ba9-865e-3129014967ac', contemporary_2024, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('48ae9f9d-523f-4ba9-865e-3129014967ac', '').
narrative_ontology:cs_kernel_id(vatican_ii_magisterial_authority__rupture_reading, vatican_ii_magisterial_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vatican_ii_magisterial_authority__rupture_reading, reformist_theological_bloc).
narrative_ontology:constraint_beneficiary(vatican_ii_magisterial_authority__rupture_reading, post_conciliar_institutional_hierarchy).
narrative_ontology:constraint_victim(vatican_ii_magisterial_authority__rupture_reading, traditionalist_resistance).
narrative_ontology:constraint_victim(vatican_ii_magisterial_authority__rupture_reading, pre_conciliar_systematic_theology).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(vatican_ii_magisterial_authority__rupture_reading, ecumenical_protestant_observers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Progressive theologians and bishops who read Vatican II as authorizing a fundamental reorientation toward religious freedom, ecumenism, vernacular liturgy, and modern world engagement. They benefit from the rupture reading because it legitimates their implementation agenda and insulates their interpretations from charges of infidelity. Their professional identity and intellectual project are fused with the conciliar texts as rupture. Exit would require abandoning their entire theological framework and institutional position within the post-conciliar Church.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__rupture_reading, reformist_theological_bloc, beneficiary,
    institutional, generational, identity_locked, global).

% The Vatican and national episcopal conferences that implemented Vatican II's decrees and set the interpretive standards for what counts as faithful development. They enforce the rupture reading by: (1) controlling seminary curricula to teach Vatican II as a watershed, (2) adjudicating disputes over implementation by appealing to 'the spirit of Vatican II,' (3) limiting traditionalist liturgical access and suppressing pre-conciliar theological frameworks in official teaching. They can choose to soften or reverse this enforcement (hence arbitrage exit), but doing so would delegitimate decades of magisterial teaching and institutional decisions.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__rupture_reading, post_conciliar_institutional_hierarchy, agenda_setter,
    institutional, generational, arbitrage, global).

% Catholics who hold that Vatican II contradicts prior magisterial teaching and represents a break with authentic tradition. They argue that error (religious indifferentism, liturgical rupture, episcopal authority subordination) has been canonized. Their exit options are schism (like the SSPX), marginalization within the official Church, or persistent costly dissent. The institutional machinery actively suppresses their theological frameworks and liturgical preferences, making their voice systemically excluded from authoritative interpretation.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__rupture_reading, traditionalist_resistance, payer,
    moderate, generational, trapped, global).

% The body of pre-Vatican II magisterial doctrine (on error's lack of rights, religious establishment, papal prerogatives, liturgical universalism). Under the rupture reading, these positions are declared superseded rather than reinterpreted. The constraint operates by treating them as obsolete frameworks that need not be integrated or honored in new magisterial acts.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__rupture_reading, pre_conciliar_systematic_theology, payer,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(vatican_ii_magisterial_authority__rupture_reading, pre_conciliar_systematic_theology).

% Protestant and Orthodox communities that engage the post-conciliar Church. They benefit from the rupture reading because it commits the Catholic Church to religious freedom, dialogue, and de-coupled ecclesiology—positions compatible with their own frameworks. The rupture reading makes the Church a partner in ecumenical conversation rather than a competitor claiming to be the only true Church. Their benefit depends on the rupture reading remaining institutionally enforced.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__rupture_reading, ecumenical_protestant_observers, beneficiary,
    powerful, biographical, mobile, global).

% The office responsible for doctrinal oversight (formerly Holy Office, now Dicastery for the Doctrine of Faith). In principle, they adjudicate compatibility with prior teaching, but they are structurally captured by the hierarchy's commitment to the rupture reading. They use 'hermeneutics of continuity' language to minimize discontinuity claims while enforcing the post-conciliar settlement through disciplinary action against theologians who emphasize rupture in revisionist directions.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__rupture_reading, vatican_doctrinal_congregation, observer,
    institutional, generational, analytical, global).

% Historians and theologians trained in Vatican II's actual proceedings (Vatican II Studies scholars, conciliar-process historians) who document how the rupture was manufactured through text manipulation, coalition-building, and selective interpretation. Their research is professionally marginalized within ecclesial institutions and faces systematic suppression from official teaching bodies. They would attest that the rupture reading is a constructed interpretive choice, not an inherent property of the texts.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__rupture_reading, academic_catholic_historians, excluded,
    moderate, biographical, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(vatican_ii_magisterial_authority__rupture_reading, post_conciliar_institutional_hierarchy).
narrative_ontology:fixing_cost_class(vatican_ii_magisterial_authority__rupture_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a mechanism for the Catholic institutional Church to adapt to modern conditions (ecumenical engagement, religious freedom, vernacular accessibility) while maintaining magisterial authority. The coordinated problem: how can an institution claiming divine guidance modernize without admitting prior teaching was false? Answer: declare a hermeneutical shift that redefines 'development' to include reversal-by-reframing.
% TRANSFER_FUNCTION: Moves interpretive authority from pre-conciliar systematic theology to post-conciliar pastoral sensibility; moves liturgical practice from Latin universalism to vernacular experimentation; moves ecclesiology from exclusive truth-claim to dialogical openness. The transfer flow runs FROM traditionalist frameworks and pre-conciliar institutional forms TO reformist theology and modern institutional adaptation. It is enforced through control of seminary education, magisterial language, and liturgical enforcement.
% ABSENT_VOICES: Vatican II Studies historians and process scholars (historians of how the texts were composed, who negotiated which language, what was suppressed or compromised) are structurally excluded from authoritative interpretation. Traditionalist resistance speaks but is marginalized as reactionary. Pre-conciliar systematic theology has no institutional seat—it is treated as a closed archive rather than a living partner in dialogue.
% DISAPPEARANCE_RATIONALE: If the rupture reading enforcement disappeared and the hermeneutic reverted to continuity-interpretation or composite-overdetermination reading, the post-conciliar settlement would collapse: seminary curricula would have to be rewritten, liturgical practice would be renegotiated, episcopal authority would be reframed, and the ecumenical apparatus would lose its interpretive anchor. The institutional hierarchy would lose the narrative that legitimates 50+ years of innovation as faithful development. The Catholic Church's relationship to modernity would be radically destabilized.
% FOUNDING_PROBLEM: Vatican II convened to modernize the Church's engagement with the contemporary world; how to adapt liturgy, theology, and institutional posture to 20th-century conditions while maintaining magisterial continuity? The founding problem as stated by the reformist bloc: overcome pre-conciliar institutional sclerosis. The founding problem as stated by traditionalists: maintain doctrinal coherence against rationalist pressures and institutional dissolution.
% FOUNDING_PROBLEM_CORROBORATION: The reformist institutional hierarchy attests the founding problem is live and Vatican II solved it. Progressive theologians attest the same. Academic historians of Vatican II attest that the founding problem was real but that the Council's texts did NOT unambiguously authorize the rupture interpretation—they document deliberate interpretive choices that framed ambiguous conciliar language as rupture. Traditionalist resistance attests that the founding problem was institutional dissolution disguised as modernization, and that Vatican II enabled it. The corroboration from outside the benefiting parties runs heavily toward 'the rupture reading is a constructed interpretation,' not an inherent property of the texts.
narrative_ontology:disappearance_verdict(vatican_ii_magisterial_authority__rupture_reading, world_rearranges).
narrative_ontology:founding_problem_status(vatican_ii_magisterial_authority__rupture_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vatican_ii_magisterial_authority__rupture_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(vatican_ii_magisterial_authority__rupture_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(vatican_ii_magisterial_authority__rupture_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(vatican_ii_magisterial_authority__rupture_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(vatican_ii_magisterial_authority__rupture_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness runs high and rising (0.18→0.68 over the interval) because the constraint's operation is increasingly explicit: seminary formation treats Vatican II as a watershed, magisterial language assumes the rupture, and traditionalist frameworks are systematically delegitimated. Theater ratio rises sharply (0.05→0.42) in the 1970s-1985 period as implementation shifted from doctrinal justification to performative commitment—magisterial teaching increasingly appeals to 'the spirit of Vatican II' without rigorously grounding innovations in conciliar texts themselves, a Goodhart-style metric substitution where 'conciliar legitimacy' becomes cover for institutional preference. Suppression requirement mirrors extractiveness: 1962 is low (Council still assembling), rises steeply 1970 (post-conciliar implementation), stabilizes by 2005 (traditionalist resistance has been marginalized, suppression no longer needs to intensify). Accessibility collapse is moderate (0.62): alternatives (continuity reading, composite reading, pre-conciliar frameworks) are not completely foreclosed—they persist in traditionalist communities, academic debate, and papal language—but institutional legitimacy has collapsed for them. Resistance is high (0.74): traditionalist communities mount real resistance, SSPX schism attests to it, and academic historians document the interpretive choices underlying the rupture reading.
 *
 * PERSPECTIVAL GAP:
 *   From the reformist hierarchy's seat, Vatican II is a triumphant adaptation: the Church modernized while maintaining magisterial authority. From the traditionalist seat, the same apparatus operates as enforced dissolution disguised as development. From the Vatican II Studies historian's seat (excluded), the constraint is an interpretive settlement that privileges one reading of ambiguous texts and suppresses others through institutional power, not textual clarity. The engine computes these divergences from the structural data: agenda-setter with arbitrage exit computing differently from trapped victims; beneficiary frames computing differently from payer frames. The authored claim (tangled_rope) reflects the structural ambiguity: there is genuine coordination (modernization) and genuine extraction (suppression of alternatives).
 *
 * DIRECTIONALITY LOGIC:
 *   The reformist institutional hierarchy sits at d near 0.0 (full beneficiary): they control interpretation, their theological frameworks are validated as 'development,' their preferred outcomes (ecumenism, religious freedom, pastoral flexibility) are institutionalized. Pre-conciliar systematic theology and traditionalist resistance sit at d near 1.0 (full target): their frameworks are actively delegitimated, their positions are suppressed, their exit is trapped or schismatic. The constraint's asymmetry is structural: one seat (reformist hierarchy) sets the terms and collects institutional legitimacy; the other seats (traditionalists, pre-conciliar theology) bear the cost of reinterpretation and institutional marginalization. Ecumenical Protestant observers benefit from the rupture reading (d near beneficiary) without controlling it—their benefit is incidental to the hierarchy's agenda, but their support reinforces it.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (modernize the Church while maintaining magisterial coherence) remains live, but its relationship to the rupture reading has degraded. Early post-conciliar texts (1965–1975) grounded innovations in conciliar authority with detailed exegesis. By 2005, magisterial teaching increasingly invokes 'the spirit of Vatican II' as self-justifying, a terminal substitution of narrative for textual grounding. The theater ratio rise (0.05→0.42) reflects this drift: the constraint's function shifted from genuine coordination work (textually grounding modern positions) to performance work (maintaining institutional legitimacy without rigorous hermeneutical defense). The 'mandatrophy' signature fires because: (1) the founding problem (magisterial coherence) is contested as resolved or dissolved; (2) the constraint persists despite this contestation because the benefiting hierarchy maintains enforcement; (3) alternative readings (continuity, composite) are suppressed not by logical refutation but by institutional power. The constraint shows partial mandatrophy: it retains a coordination function (ecumenical engagement genuinely depends on the rupture reading), but the proportion of enforcement activity devoted to managing alternatives (theater_ratio, suppression_requirement) has risen faster than the coordination function has deepened.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    textual_ambiguity_vs_institutional_choice,
    'Do Vatican II''s conciliar documents contain the rupture reading in their textual meaning, or is the rupture reading a constructed interpretation imposed by post-conciliar institutional choice?',
    'Systematic comparative exegesis of conciliar texts against pre-conciliar magisterium (Vatican I, Pius XII, Leo XIII) by historians external to both reformist and traditionalist camps; analysis of conciliar-process documents (interventions, schema revisions, voting patterns) to reconstruct what delegates intended. The vagaries of conciliar language (phrases like ''signs of the times,'' ''development,'' ''dialogue'') permit multiple readings; historical documentation can establish whether the rupture reading was intended or constructed post-hoc.',
    'If the rupture reading is textually grounded, the constraint operates as genuine institutional enforcement of a clear magisterial commitment. If constructed post-hoc, the constraint is partly Snare: the texts are used as cover for institutional remaking that was not authorized or visible in the conciliar debates themselves. Mandatrophy diagnosis would shift from ''partial'' to ''severe''—the founding problem (maintain magisterial coherence) would be dead, and the constraint would persist purely as enforcement of institutional preference.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(textual_ambiguity_vs_institutional_choice, empirical, 'Whether the rupture reading reflects the texts'' semantic content or institutional post-conciliar power.').

omega_variable(
    religious_freedom_contradiction,
    'Does Vatican II''s Declaration on Religious Freedom (Dignitatis Humanae) logically contradict pre-conciliar teaching on error and truth, or is it a legitimate development compatible with prior doctrine?',
    'Theological analysis of the formal claims: pre-conciliar teaching held that error has no rights; DH asserts a right to religious freedom without regard to truth-status. These appear contradictory. Resolution requires either: (a) demonstrating that pre-conciliar teaching did NOT actually hold what it appears to say (exegetical rescue); (b) showing that DH can be reinterpreted to avoid contradiction (hermeneutical flexibility); (c) accepting the contradiction as genuine rupture that is nonetheless legitimate (rupture advocates'' position). Each resolution carries different implications for the constraint''s classification.',
    'If the contradiction is genuine and acknowledged, the rupture reading is validated and the constraint operates as institutional enforcement of an explicit doctrinal shift. If the contradiction can be dissolved hermeneutically, the continuity reading gains plausibility and the constraint becomes less extractive (less suppression needed to maintain the narrative). If pre-conciliar teaching is reexegeted to have said something different, the rupture reading loses its clearest empirical anchor and becomes a constructed narrative.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(religious_freedom_contradiction, conceptual, 'Whether DH represents genuine doctrinal contradiction or legitimate development of prior teaching.').

omega_variable(
    suppression_internalization,
    'Is the measured suppression (0.71) structural (external institutional machinery excluding traditionalist frameworks) or internalized (traditionalist Catholics have absorbed the judgment that their positions are retrograde)?',
    'Post-exit surveys of traditionalist Catholics who have left the institutional Church or moved to traditionalist communities: do they maintain their intellectual frameworks with confidence, or do they carry internalized shame about their positions? Comparative analysis of traditionalist self-description in public forums vs. private (academic, confessional) settings. If private frames differ sharply from public, suppression is partly internalized.',
    'If suppression is primarily structural, exiting traditionalists should recover confidence in their frameworks; if internalized, suppression persists after exit. Internalized suppression suggests the constraint operates with higher effective intensity than the structural machinery alone would indicate—the cost to exit includes not just institutional marginalization but cognitive rehabilitation.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_internalization, empirical, 'Whether measured suppression is structural institutional machinery or internalized cognitive capture.').

omega_variable(
    hermeneutics_of_continuity_strategic_function,
    'Does the Vatican''s ''hermeneutics of continuity'' language (used by John Paul II, Benedict XVI) represent a genuine doctrinal commitment to continuity, or a strategic containment of the rupture narrative to prevent traditionalist escalation?',
    'Analysis of where continuity-hermeneutics language appears vs. where rupture-language appears in magisterial teaching; examination of how continuity language is applied to specific disputed doctrines (religious freedom, liturgical authority, papal prerogatives); comparison of rhetorical usage across different institutional contexts (formal magisterium vs. pastoral implementation). If continuity language is deployed selectively to prevent traditionalist resistance while rupture language drives actual implementation, it is strategic containment rather than substantive doctrine.',
    'If continuity-hermeneutics is strategic containment, the constraint is more Snare than Tangled Rope: the hierarchy''s public narrative acknowledges continuity to suppress organized resistance, while actual institutional behavior enforces rupture. If it is genuine doctrinal commitment, the constraint is more Tangled Rope: there is a real coordination function (bridging traditional and modern) even as suppression persists.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(hermeneutics_of_continuity_strategic_function, empirical, 'Whether continuity-hermeneutics is strategic or substantive.').

omega_variable(
    competing_kernel_readings_framework_incompatibility,
    'Can the three readings (rupture, continuity, composite) coexist within a single institutional framework, or does one reading necessarily foreclose the others?',
    'Structural analysis: the rupture reading asserts incompatibility between pre-conciliar and post-conciliar doctrine; the continuity reading asserts compatibility; the composite reading asserts overdetermination (both readings are encoded in the texts). These are logically distinct positions. Can an institution hold all three simultaneously (as pastoral adaptation, with different readings for different audiences), or does commitment to one necessarily exclude the others? Historical analysis of how the hierarchy has treated each reading—has it integrated them or suppressed alternatives?',
    'If the readings are mutually foreclosing, one must be chosen and the others suppressed—the constraint is necessarily enforcing a choice, not coordinating genuine ambiguity. If they can coexist, the hierarchy''s suppression of non-rupture readings is optional, not forced by logical structure. The framework incompatibility question maps directly to whether the constraint is Snare (forced choice) vs. Tangled Rope (coordinating genuine ambiguity).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(competing_kernel_readings_framework_incompatibility, conceptual, 'Whether the three kernel readings are logically compatible or mutually foreclosing.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vatican_ii_magisterial_authority__rupture_reading, 1962, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vati_tr_t1962, vatican_ii_magisterial_authority__rupture_reading, theater_ratio, 1962, 0.05).
narrative_ontology:measurement_basis(vati_tr_t1962, observed).
narrative_ontology:measurement(vati_tr_t1970, vatican_ii_magisterial_authority__rupture_reading, theater_ratio, 1970, 0.18).
narrative_ontology:measurement_basis(vati_tr_t1970, observed).
narrative_ontology:measurement(vati_tr_t1985, vatican_ii_magisterial_authority__rupture_reading, theater_ratio, 1985, 0.32).
narrative_ontology:measurement_basis(vati_tr_t1985, observed).
narrative_ontology:measurement(vati_tr_t2005, vatican_ii_magisterial_authority__rupture_reading, theater_ratio, 2005, 0.4).
narrative_ontology:measurement_basis(vati_tr_t2005, observed).
narrative_ontology:measurement(vati_tr_t2015, vatican_ii_magisterial_authority__rupture_reading, theater_ratio, 2015, 0.42).
narrative_ontology:measurement_basis(vati_tr_t2015, observed).
narrative_ontology:measurement(vati_tr_t2024, vatican_ii_magisterial_authority__rupture_reading, theater_ratio, 2024, 0.42).
narrative_ontology:measurement_basis(vati_tr_t2024, observed).

% Extraction over time
narrative_ontology:measurement(vati_be_t1962, vatican_ii_magisterial_authority__rupture_reading, base_extractiveness, 1962, 0.18).
narrative_ontology:measurement_basis(vati_be_t1962, observed).
narrative_ontology:measurement(vati_be_t1970, vatican_ii_magisterial_authority__rupture_reading, base_extractiveness, 1970, 0.42).
narrative_ontology:measurement_basis(vati_be_t1970, observed).
narrative_ontology:measurement(vati_be_t1985, vatican_ii_magisterial_authority__rupture_reading, base_extractiveness, 1985, 0.61).
narrative_ontology:measurement_basis(vati_be_t1985, observed).
narrative_ontology:measurement(vati_be_t2005, vatican_ii_magisterial_authority__rupture_reading, base_extractiveness, 2005, 0.65).
narrative_ontology:measurement_basis(vati_be_t2005, observed).
narrative_ontology:measurement(vati_be_t2015, vatican_ii_magisterial_authority__rupture_reading, base_extractiveness, 2015, 0.68).
narrative_ontology:measurement_basis(vati_be_t2015, observed).
narrative_ontology:measurement(vati_be_t2024, vatican_ii_magisterial_authority__rupture_reading, base_extractiveness, 2024, 0.68).
narrative_ontology:measurement_basis(vati_be_t2024, observed).

% Suppression requirement over time
narrative_ontology:measurement(vati_su_t1962, vatican_ii_magisterial_authority__rupture_reading, suppression_requirement, 1962, 0.15).
narrative_ontology:measurement_basis(vati_su_t1962, observed).
narrative_ontology:measurement(vati_su_t1970, vatican_ii_magisterial_authority__rupture_reading, suppression_requirement, 1970, 0.48).
narrative_ontology:measurement_basis(vati_su_t1970, observed).
narrative_ontology:measurement(vati_su_t1985, vatican_ii_magisterial_authority__rupture_reading, suppression_requirement, 1985, 0.62).
narrative_ontology:measurement_basis(vati_su_t1985, observed).
narrative_ontology:measurement(vati_su_t2005, vatican_ii_magisterial_authority__rupture_reading, suppression_requirement, 2005, 0.69).
narrative_ontology:measurement_basis(vati_su_t2005, observed).
narrative_ontology:measurement(vati_su_t2015, vatican_ii_magisterial_authority__rupture_reading, suppression_requirement, 2015, 0.71).
narrative_ontology:measurement_basis(vati_su_t2015, observed).
narrative_ontology:measurement(vati_su_t2024, vatican_ii_magisterial_authority__rupture_reading, suppression_requirement, 2024, 0.71).
narrative_ontology:measurement_basis(vati_su_t2024, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vatican_ii_magisterial_authority__rupture_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(vatican_ii_magisterial_authority__rupture_reading, 0.12).
narrative_ontology:affects_constraint(vatican_ii_magisterial_authority__rupture_reading, vatican_ii_magisterial_authority__continuity_reading).
narrative_ontology:affects_constraint(vatican_ii_magisterial_authority__rupture_reading, vatican_ii_magisterial_authority__composite_overdetermination_reading).

% DUAL FORMULATION NOTE:
% Vatican II magisterial authority is a contested kernel with three structurally distinct readings. This story (rupture_reading) encodes the interpretation that the Council represents fundamental doctrinal discontinuity. Sibling readings (continuity_reading, composite_overdetermination_reading) decompose the same kernel differently, with different ε values, different beneficiary/victim structures, and different types. Each reading is a separate constraint story. The three are linked via network.affects_constraints and form a constraint family in which the rupture reading (this story) is the dominant institutional enforcement, while continuity and composite readings persist as subaltern theological positions. The rupture reading's enforcement creates structural conditions (suppression of alternatives, reformist theological privilege) that influence the viability of the other readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(vatican_ii_magisterial_authority__rupture_reading, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
