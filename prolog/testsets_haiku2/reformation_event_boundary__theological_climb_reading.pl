% ============================================================================
% CONSTRAINT STORY: reformation_event_boundary__theological_climb_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_reformation_event_boundary__theological_climb_reading, []).

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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: reformation_event_boundary__theological_climb_reading
 *   human_readable: Reformation as Theological Doctrinal Breakthrough and Institutional Separation
 *   domain: religious/historical/epistemological
 *
 * SUMMARY:
 *   This constraint instantiates ONE reading of the contested kernel
 *   'reformation_event_boundary': the Reformation as primarily a theological
 *   innovation event. Under this reading, the Reformation enters history as a
 *   genuine doctrinal breakthrough — Luther's recovery and articulation of
 *   justification by faith alone — which required institutional separation
 *   from the Catholic Church to sustain. The theological reading casts the
 *   Catholic Church and the medieval theological consensus as the victims of
 *   this correction, and reformed believers and reformed institutions as
 *   beneficiaries. Extractiveness is moderate (0.42): the new doctrine
 *   extracts from the medieval consensus by displacing it, and the Catholic
 *   institutional authority bears this displacement; but the extraction is
 *   framed as correction, not predation. Suppression is lower (0.31) because
 *   the theological claim can be defended on intellectual grounds; it does
 *   not rely primarily on silencing alternatives but on persuading that the
 *   alternatives are in error. Theater rises from near-zero to 0.22 over the
 *   century as institutional Reformed churches calcify and performative piety
 *   replaces lived theology in some contexts. The reading is CLAIMED as
 *   tangled_rope because it combines genuine theological coordination
 *   (resolving the salvation problem for believers) with asymmetric
 *   extraction (the medieval consensus loses its legitimacy). The measurement
 *   series shows suppression stabilizing at the Peace of Augsburg (1555),
 *   after which the theological boundary is institutionally fixed.
 *
 * KEY AGENTS:
 *   - reformed_believers: powerless individuals experiencing theological liberation and institutional fragmentation
 *   - reformed_clergy and reformed_institutions: organized and powerful agents driving theological articulation and institutional separation
 *   - catholic_church_institutional_authority: institutional victim of the theological correction
 *   - medieval_theological_consensus: doctrinal structure displaced by the new reading
 *   - secular_rulers: excluded from the core theological contest but eventually protectors of the reform
 *   - theological_observers: analytical seat assessing the evidentiary basis of the theological-climb claim
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(reformation_event_boundary__theological_climb_reading, 0.42).
domain_priors:suppression_score(reformation_event_boundary__theological_climb_reading, 0.31).
domain_priors:theater_ratio(reformation_event_boundary__theological_climb_reading, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(reformation_event_boundary__theological_climb_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(reformation_event_boundary__theological_climb_reading, suppression_requirement, 0.31).
narrative_ontology:constraint_metric(reformation_event_boundary__theological_climb_reading, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(reformation_event_boundary__theological_climb_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(reformation_event_boundary__theological_climb_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(reformation_event_boundary__theological_climb_reading, tangled_rope).
narrative_ontology:human_readable(reformation_event_boundary__theological_climb_reading, "Reformation as Theological Doctrinal Breakthrough and Institutional Separation").
narrative_ontology:topic_domain(reformation_event_boundary__theological_climb_reading, "religious/historical/epistemological").

domain_priors:requires_active_enforcement(reformation_event_boundary__theological_climb_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(reformation_event_boundary__theological_climb_reading, '135d02a3-9849-447e-bad8-ee7002d0a864').
narrative_ontology:cs_kernel_codification('135d02a3-9849-447e-bad8-ee7002d0a864', fixed_text).
narrative_ontology:cs_authority_grounding('135d02a3-9849-447e-bad8-ee7002d0a864', lineage).
narrative_ontology:cs_interpretation_layer_present('135d02a3-9849-447e-bad8-ee7002d0a864').
narrative_ontology:cs_reading_relation('135d02a3-9849-447e-bad8-ee7002d0a864', reformation_event_boundary__political_swap_reading, influences).
narrative_ontology:cs_reading_relation('135d02a3-9849-447e-bad8-ee7002d0a864', reformation_event_boundary__composite_overdetermination_reading, coexists_with).
narrative_ontology:cs_axiom('135d02a3-9849-447e-bad8-ee7002d0a864', foundational, justification_by_faith_doctrinal_recovery).
narrative_ontology:cs_axiom_status(justification_by_faith_doctrinal_recovery, holdable).
narrative_ontology:cs_axiom_grounding('135d02a3-9849-447e-bad8-ee7002d0a864', justification_by_faith_doctrinal_recovery, empirically_contingent).
narrative_ontology:cs_axiom('135d02a3-9849-447e-bad8-ee7002d0a864', foundational, institutional_separation_theological_necessity).
narrative_ontology:cs_axiom_status(institutional_separation_theological_necessity, holdable).
narrative_ontology:cs_axiom_grounding('135d02a3-9849-447e-bad8-ee7002d0a864', institutional_separation_theological_necessity, deontological).
narrative_ontology:cs_reference_frame('135d02a3-9849-447e-bad8-ee7002d0a864', unified_christendom_scholastic_mediation).
narrative_ontology:cs_drift_state('135d02a3-9849-447e-bad8-ee7002d0a864', peace_of_augsburg_1555, gap(codification_collapse, substantial, true)).
narrative_ontology:cs_created_at('135d02a3-9849-447e-bad8-ee7002d0a864', '').
narrative_ontology:cs_kernel_id(reformation_event_boundary__theological_climb_reading, reformation_event_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(reformation_event_boundary__theological_climb_reading, reformed_believers).
narrative_ontology:constraint_beneficiary(reformation_event_boundary__theological_climb_reading, reformed_clergy).
narrative_ontology:constraint_beneficiary(reformation_event_boundary__theological_climb_reading, reformed_institutions).
narrative_ontology:constraint_victim(reformation_event_boundary__theological_climb_reading, catholic_church_institutional_authority).
narrative_ontology:constraint_victim(reformation_event_boundary__theological_climb_reading, medieval_theological_consensus).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Ordinary Christians whose understanding of salvation is reframed by justification-by-faith doctrine. Under the medieval consensus, salvation required ecclesiastical mediation and works-righteousness; under the reform reading, salvation is direct and faith-centered. They gain doctrinal clarity and personal agency in faith, though at the cost of institutional conflict and religious fragmentation.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__theological_climb_reading, reformed_believers, beneficiary,
    powerless, civilizational, identity_locked, continental).

% Protestant clergy and reformed theologians (Luther, Calvin, and their networks) articulate and defend the new doctrine. They gain intellectual authority and pastoral independence from Rome; they also incur personal risk (excommunication, political exile, execution). They drive the institutional separation by enforcing the new doctrine and refusing reconciliation with the medieval consensus.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__theological_climb_reading, reformed_clergy, beneficiary,
    organized, generational, constrained, continental).
narrative_ontology:stakeholder_secondary_role(reformation_event_boundary__theological_climb_reading, reformed_clergy, agenda_setter).

% Reformed churches and the institutional structures that emerge from them (Lutheran territorial churches, Calvinist consistories, Reformed congregations). They gain organizational independence, doctrinal control over their own worship and teaching, and control over clerical training and discipline. They are built on the theological breakthrough and exist to instantiate it.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__theological_climb_reading, reformed_institutions, beneficiary,
    powerful, civilizational, arbitrage, continental).
narrative_ontology:stakeholder_secondary_role(reformation_event_boundary__theological_climb_reading, reformed_institutions, agenda_setter).

% The Roman Catholic institutional apparatus (papal authority, curial hierarchy, episcopal structure, university theology faculties grounded in Scholasticism). From the theological-climb reading's perspective, this actor bears the cost of being theologically corrected: its doctrinal authority is challenged, its hierarchical structure is declared unnecessary for salvation, and its institutional monopoly on religious authority is broken. They respond with excommunication and institutional enforcement to suppress the reform doctrine.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__theological_climb_reading, catholic_church_institutional_authority, payer,
    institutional, civilizational, trapped, continental).

% The non-agent entity representing the inherited Scholastic theological framework: emphasis on works-righteousness, sacramental mediation, ecclesiastical hierarchy as the path to salvation. This is not an actor but a doctrinal structure. It 'bears the cost' by being displaced from the field of legitimate theological authority.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__theological_climb_reading, medieval_theological_consensus, payer,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(reformation_event_boundary__theological_climb_reading, medieval_theological_consensus).

% German princes and secular authorities who eventually protect and adopt the reform movement. In the theological-climb reading, they are excluded from the core theological contest; they become relevant later for political protection and institutional establishment, but they are not the driving agents of the theological breakthrough itself.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__theological_climb_reading, secular_rulers, excluded,
    powerful, biographical, mobile, national).

% Modern historians and theologians evaluating the claim that the Reformation was primarily a theological innovation event. They assess the evidentiary basis for the breakthrough claim and examine whether the theology or the politics was the causal driver.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__theological_climb_reading, theological_observers, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(reformation_event_boundary__theological_climb_reading, reformed_institutions).
narrative_ontology:fixing_cost_class(reformation_event_boundary__theological_climb_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Resolves the medieval theological problem of how believers achieve salvation and relate to God. The reform reading offers a coordinated answer: direct faith justifies, not ecclesiastical mediation or accumulated works. This coordinates believers' understanding of their own spiritual condition and their relationship to authority — no longer must each believer navigate a hierarchical ecclesiastical path; the doctrine itself provides the coordinate.
% TRANSFER_FUNCTION: The arrangement transfers doctrinal authority from the Catholic institutional hierarchy to reformed theological texts and communities interpreting Scripture directly. It also transfers pastoral authority from celibate priests to reformed clergy; spiritual agency from institutional mediation to individual conscience. The cost is institutional fragmentation, loss of unified Christendom, and centuries of religious conflict.
% ABSENT_VOICES: Catholic theologians and church hierarchy are present but their role is cast as the defender of error from this reading's perspective; they are not absent, but defeated. Absent are: the perspectives of ordinary medieval Catholics who might argue their lived piety and institutional loyalty were theologically sound; the Muslims, Jews, and other non-Christian actors whose historical presence is displaced by Christian internal dispute; and the long-term victims of religious warfare triggered by denominational separation.
% DISAPPEARANCE_RATIONALE: If the theological-climb event (the doctrinal breakthrough and institutional separation it necessitated) had never occurred, Christendom would have remained institutionally unified under papal authority. The medieval theological consensus would still govern Christian understanding of salvation. Reformed churches would not exist; millions of Christians would remain within a unified (though troubled) institutional structure. The religious fragmentation of Europe would not have occurred in this form.
% FOUNDING_PROBLEM: Medieval Christianity operated under a theological framework (works-righteousness, sacramental necessity, ecclesiastical hierarchy) that the reform reading claims obscured the biblical doctrine of justification by faith alone. The founding problem is: how to recover the true doctrine of salvation that was obscured by medieval corruption and institutional accretion?
% FOUNDING_PROBLEM_CORROBORATION: Reformed theologians and modern Protestant historians attest that the founding problem (theological distortion) was live and required recovery. Catholic theologians and modern Catholic historians dispute that medieval theology was distorted; they argue Scholasticism integrated faith and reason soundly and that the 'recovery' was actually an innovation and rupture. The theological disagreement cannot be resolved by external observers; what CAN be examined is whether the reform reading's evidentiary claims (that Luther's sources say what Luther claimed they say) hold under textual analysis. Modern reformation historians (including non-confessional scholars) debate whether the theological breakthroughs were genuinely *novel* or careful *recoveries* of earlier patristic sources.
narrative_ontology:disappearance_verdict(reformation_event_boundary__theological_climb_reading, world_rearranges).
narrative_ontology:founding_problem_status(reformation_event_boundary__theological_climb_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(reformation_event_boundary__theological_climb_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(reformation_event_boundary__theological_climb_reading, 'none', 1).
narrative_ontology:epsilon_provenance(reformation_event_boundary__theological_climb_reading, 0.42, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(reformation_event_boundary__theological_climb_reading_tests).
:- end_tests(reformation_event_boundary__theological_climb_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness builds from zero at 1500 to 0.42 by 1545 (the Council of Trent, where Catholic response crystallizes) and plateaus. The trajectory reflects the theological breakthrough's initial propagation, crystallization into institutional forms, and then stabilization under the Peace of Augsburg (1555). Theater rises modestly (0.22 by 1600) because while the theological content remains substantive, institutionalized Reformed churches gradually develop rituals and enforcement practices that become increasingly performative — the theater rise models the tension between genuine theology and institutional maintenance. Suppression requirement climbs from 0 to 0.31 as the Catholic Church moves from initial dismissal to organized Counter-Reformation enforcement (the Inquisition, the Index Librorum Prohibitorum). Suppression stabilizes at 0.31 by 1555 because the institutional separation is accomplished and enforced; further suppression is maintenance, not creation. The measurements are authored on one shared time grid (every metric at every time point) so temporal alignment is audited as valid.
 *
 * PERSPECTIVAL GAP:
 *   The reformed-clergy and reformed-institutions seats should compute this constraint very differently from the catholic_church_institutional_authority seat. From the reform perspective, this is genuine theological coordination (benign, corrective). From the Catholic perspective, it is extraction of doctrinal authority and institutional jurisdiction through schism. The engine computes the seat-wise divergence from structural data: reformed seats have lower directionality (beneficiaries with constrained exit = lower d); Catholic institutional authority has high directionality (victim of correction = higher d). The claimed_type (tangled_rope) reflects this asymmetry: one side coordinates around a new doctrine, the other side pays through loss of authority. This divergence is structural, not a failure of the framework to accommodate perspective.
 *
 * DIRECTIONALITY LOGIC:
 *   Reformed believers and clergy gain doctrinal clarity and pastoral agency; they are beneficiaries despite identity-locked exit (they cannot un-know the doctrine or return to the medieval consensus). Reformed institutions are powerful beneficiaries with arbitrage exit (they can appeal to secular rulers for protection). Catholic institutional authority loses doctrinal monopoly and institutional jurisdiction; it is a victim of correction. Extractiveness is amplified for this victim seat by the institutional scope (continental); extractiveness is damped for beneficiary seats by their dependent positioning (believers are powerless, clergy are identity-locked despite organizational power). Suppression requirement is high (0.31) because maintaining the separation requires the Catholic Church's active enforcement (excommunication, Inquisition, legal suppression of reformed texts); the reform beneficiaries contribute to suppression maintenance by refusing reconciliation and continuing to propagate the new doctrine.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint avoids the trap of mislabeling theological correction as pure extraction by including genuine coordination function: the reform resolves a real theological question (what justifies salvation?) and coordinates believers and clergy around a new answer. It is not a snare because the coordination is not a cover story; it is what the constraint is about. It is tangled_rope because the coordination is asymmetric: beneficiaries gain doctrinal clarity and institutional autonomy; victims (medieval consensus, Catholic authority) lose legitimacy. The active enforcement (Catholic suppression, reformed counter-enforcement) keeps it from collapsing into a stable rope. The claim/metric independence is maintained: we claim tangled_rope on structural grounds (asymmetric coordination) while authoring metrics (extractiveness, suppression) that describe actual operation without tuning toward the claim.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    novelty_vs_recovery_ambiguity,
    'Was justification-by-faith doctrine a genuine recovery of patristic and biblical sources, or was it a theological innovation projected backward onto earlier texts?',
    'Textual analysis of patristic sources, medieval scholastic readings of those same sources, and Reformation-era textual claims; examination of whether Reformation exegesis accurately represents its source texts or retroactively imposes new meaning.',
    'If recovery: the theological-climb reading is vindicated as correction of a misreading. If innovation: the Reformation enters as a more creative theological breach, potentially more like a snare (extraction of authority through new framing) than tangled-rope coordination. Classification could shift from tangled_rope toward snare if innovation is established.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(novelty_vs_recovery_ambiguity, empirical, 'Whether the theological breakthrough was genuinely new or a reinterpretation of existing doctrine.').

omega_variable(
    theology_vs_politics_causal_priority,
    'Did theological innovation drive political reorganization (theology as causal primary), or did political incentives drive theological framing (politics as causal primary)?',
    'Chronological analysis of theological writing vs. political action; examination of which actors initiated which changes and whether theological texts preceded or followed political moves; counterfactual analysis of whether the theology could have spread without political protection.',
    'If theology-primary: this reading''s classification stands as tangled_rope (theological coordination with asymmetric extraction). If politics-primary: the political-swap reading better describes the constraint, which would classify as snare (extraction of church assets and authority through theological post-hoc-ization). The readings coexist; this omega distinguishes which causal story is more empirically supported.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(theology_vs_politics_causal_priority, empirical, 'The causal priority between theological innovation and political reorganization.').

omega_variable(
    medieval_consensus_distortion_claim,
    'Did medieval Catholic theology actually obscure or distort biblical justification doctrine, or did it represent a legitimate theological development that the Reform reading mischaracterizes as corruption?',
    'Comparative theology: does medieval Scholasticism (Aquinas, Lombard, Bonaventure) actually teach works-righteousness without grace, or does it integrate grace and works in a coherent framework? Can modern Catholic theology defend its medieval inheritance, or does it acknowledge distortion and reform?',
    'If medieval theology is defensible: the extraction imposed by the reform reading is more severe (it displaces a legitimate doctrine, not a corrupted one). Classification could shift toward snare (extraction masked as correction). If medieval theology is genuinely distorted: this reading''s classification as tangled_rope (correction + asymmetric extraction) is validated.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(medieval_consensus_distortion_claim, conceptual, 'Whether medieval Catholic theology represented theological distortion or legitimate development.').

omega_variable(
    reformation_as_plural_event_underdetermination,
    'Is the Reformation a single event admitting multiple readings (as the kernel frame assumes), or is it so overdetermined and irreducibly composite that any single-reading framing is fundamentally misleading?',
    'Historiographic analysis: can the theological, political, institutional, and denominational processes be meaningfully separated and sequenced, or are they so interwoven that forcing a periodization and primary causal driver introduces systematic distortion? Can historians identify a coherent kernel that all readings contest, or do different readings describe genuinely different historical events that happen to overlap chronologically?',
    'If single contested kernel: this reading and its siblings are valid framings of the same phenomenon. If irreducible plurality: the composite-overdetermination reading better describes the constraint, which would require a new classification category (or reclassification as scaffold that decomposes into multiple simultaneous constraints). The reading-relations structure assumes a shared kernel; this omega documents the risk that the assumption fails.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reformation_as_plural_event_underdetermination, conceptual, 'Whether the Reformation is a single overdetermined event or a composite plurality of events that resist unified reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(reformation_event_boundary__theological_climb_reading, 1500, 1600).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(refo_tr_t1500, reformation_event_boundary__theological_climb_reading, theater_ratio, 1500, 0.0).
narrative_ontology:measurement(refo_tr_t1517, reformation_event_boundary__theological_climb_reading, theater_ratio, 1517, 0.08).
narrative_ontology:measurement(refo_tr_t1530, reformation_event_boundary__theological_climb_reading, theater_ratio, 1530, 0.12).
narrative_ontology:measurement(refo_tr_t1545, reformation_event_boundary__theological_climb_reading, theater_ratio, 1545, 0.16).
narrative_ontology:measurement(refo_tr_t1555, reformation_event_boundary__theological_climb_reading, theater_ratio, 1555, 0.18).
narrative_ontology:measurement(refo_tr_t1600, reformation_event_boundary__theological_climb_reading, theater_ratio, 1600, 0.22).

% Extraction over time
narrative_ontology:measurement(refo_be_t1500, reformation_event_boundary__theological_climb_reading, base_extractiveness, 1500, 0.0).
narrative_ontology:measurement(refo_be_t1517, reformation_event_boundary__theological_climb_reading, base_extractiveness, 1517, 0.15).
narrative_ontology:measurement(refo_be_t1530, reformation_event_boundary__theological_climb_reading, base_extractiveness, 1530, 0.35).
narrative_ontology:measurement(refo_be_t1545, reformation_event_boundary__theological_climb_reading, base_extractiveness, 1545, 0.42).
narrative_ontology:measurement(refo_be_t1555, reformation_event_boundary__theological_climb_reading, base_extractiveness, 1555, 0.42).
narrative_ontology:measurement(refo_be_t1600, reformation_event_boundary__theological_climb_reading, base_extractiveness, 1600, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(refo_su_t1500, reformation_event_boundary__theological_climb_reading, suppression_requirement, 1500, 0.0).
narrative_ontology:measurement(refo_su_t1517, reformation_event_boundary__theological_climb_reading, suppression_requirement, 1517, 0.15).
narrative_ontology:measurement(refo_su_t1530, reformation_event_boundary__theological_climb_reading, suppression_requirement, 1530, 0.28).
narrative_ontology:measurement(refo_su_t1545, reformation_event_boundary__theological_climb_reading, suppression_requirement, 1545, 0.31).
narrative_ontology:measurement(refo_su_t1555, reformation_event_boundary__theological_climb_reading, suppression_requirement, 1555, 0.31).
narrative_ontology:measurement(refo_su_t1600, reformation_event_boundary__theological_climb_reading, suppression_requirement, 1600, 0.28).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(reformation_event_boundary__theological_climb_reading, identity_coordination).
narrative_ontology:affects_constraint(reformation_event_boundary__theological_climb_reading, reformation_event_boundary__political_swap_reading).
narrative_ontology:affects_constraint(reformation_event_boundary__theological_climb_reading, reformation_event_boundary__composite_overdetermination_reading).

% DUAL FORMULATION NOTE:
% The Reformation is modeled as a contested kernel with three distinct readings: theological-climb (this story), political-swap (secular exploitation of theological disputes), and composite-overdetermination (irreducible simultaneity of theology, politics, institutions, and denominationalism). Each reading instantiates a different constraint with different ε, beneficiary/victim structures, and types. They are linked not as causally competing mechanisms within a single constraint, but as alternative framings of a common historical phenomenon. The theological-climb reading prioritizes theological texts and doctrinal innovation; the political-swap reading prioritizes state power and institutional seizure; the composite reading refuses periodization and causal priority. Each is valid as a reading; they do not converge to a single classification.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
