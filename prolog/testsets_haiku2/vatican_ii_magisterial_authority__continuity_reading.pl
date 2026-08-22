% ============================================================================
% CONSTRAINT STORY: vatican_ii_magisterial_authority__continuity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_vatican_ii_magisterial_authority__continuity_reading, []).

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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: vatican_ii_magisterial_authority__continuity_reading
 *   human_readable: Vatican II as Organic Development (Continuity Reading)
 *   domain: ecclesiology/hermeneutics
 *
 * SUMMARY:
 *   This constraint story instantiates the continuity reading of Vatican II:
 *   the claim that the Council represents organic development within unbroken
 *   tradition, with no rupture in prior magisterium. This is ONE reading of a
 *   contested kernel (vatican_ii_magisterial_authority). The story models the
 *   constraint as a Tangled Rope because it genuinely coordinates magisterial
 *   self-understanding across a historical discontinuity point (coordination
 *   function: unified interpretation prevents institutional fragmentation)
 *   AND asymmetrically extracts from traditionalists and pre-conciliar
 *   doctrine literalists by suppressing their lived experience of rupture and
 *   constraining their institutional voice. The constraint's strength has
 *   increased over the 64-year interval (1962–2026): initial extractiveness
 *   0.15 at the Council's opening rose to 0.68 by 2026, while theater_ratio
 *   (performative vs. functional activity) also rose from 0.25 to 0.58,
 *   suggesting that as institutional resistance to the continuity frame has
 *   grown, more enforcement energy goes into narrative maintenance rather
 *   than genuine doctrine.
 *
 * KEY AGENTS:
 *   - Vatican institutional authority (agenda-setter, institutional power): sets the hermeneutical frame; constraints implementation and interpretation worldwide
 *   - Continuity hermeneutics tradition (beneficiary, organized): theologians and bishops who defend continuity and benefit from its legitimacy
 *   - Latin rite traditionalists (payer, identity-locked): experience continuity reading as suppressive; marginalized for 50+ years
 *   - Pre-conciliar doctrine literalists (payer, trapped): caught between doctrinal loyalty and institutional authority; offered only technical reconciliations
 *   - Rupture-reading proponents (excluded, powerful): academic and progressive voices systematically sidelined from magisterial authority
 *   - Episcopal conferences (payer/beneficiary, constrained): implement Vatican II but must defend changes as organic rather than corrective
 *   - Academic historical community (observer, mobile): documents the empirical reality of conciliar rupture but remains external to magisterial interpretation
 *   - Society of St. Pius X (excluded, moderate): chosen organizational exit rather than accept suppressive continuity frame
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vatican_ii_magisterial_authority__continuity_reading, 0.68).
domain_priors:suppression_score(vatican_ii_magisterial_authority__continuity_reading, 0.72).
domain_priors:theater_ratio(vatican_ii_magisterial_authority__continuity_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vatican_ii_magisterial_authority__continuity_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(vatican_ii_magisterial_authority__continuity_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(vatican_ii_magisterial_authority__continuity_reading, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vatican_ii_magisterial_authority__continuity_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(vatican_ii_magisterial_authority__continuity_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vatican_ii_magisterial_authority__continuity_reading, tangled_rope).
narrative_ontology:human_readable(vatican_ii_magisterial_authority__continuity_reading, "Vatican II as Organic Development (Continuity Reading)").
narrative_ontology:topic_domain(vatican_ii_magisterial_authority__continuity_reading, "ecclesiology/hermeneutics").

domain_priors:requires_active_enforcement(vatican_ii_magisterial_authority__continuity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vatican_ii_magisterial_authority__continuity_reading, 'dab9f584-1eda-4c46-aa3f-d760a46cb9f6').
narrative_ontology:cs_kernel_codification('dab9f584-1eda-4c46-aa3f-d760a46cb9f6', fixed_text).
narrative_ontology:cs_authority_grounding('dab9f584-1eda-4c46-aa3f-d760a46cb9f6', extraction).
narrative_ontology:cs_interpretation_layer_present('dab9f584-1eda-4c46-aa3f-d760a46cb9f6').
narrative_ontology:cs_reading_relation('dab9f584-1eda-4c46-aa3f-d760a46cb9f6', vatican_ii_magisterial_authority__rupture_reading, forecloses).
narrative_ontology:cs_reading_relation('dab9f584-1eda-4c46-aa3f-d760a46cb9f6', vatican_ii_magisterial_authority__composite_overdetermination_reading, influences).
narrative_ontology:cs_axiom('dab9f584-1eda-4c46-aa3f-d760a46cb9f6', foundational, magisterial_continuity_across_discontinuity).
narrative_ontology:cs_axiom_status(magisterial_continuity_across_discontinuity, holdable).
narrative_ontology:cs_axiom_grounding('dab9f584-1eda-4c46-aa3f-d760a46cb9f6', magisterial_continuity_across_discontinuity, deontological).
narrative_ontology:cs_axiom('dab9f584-1eda-4c46-aa3f-d760a46cb9f6', secondary, doctrine_development_reconciles_apparent_contradiction).
narrative_ontology:cs_axiom_status(doctrine_development_reconciles_apparent_contradiction, holdable).
narrative_ontology:cs_axiom_grounding('dab9f584-1eda-4c46-aa3f-d760a46cb9f6', doctrine_development_reconciles_apparent_contradiction, empirically_contingent).
narrative_ontology:cs_reference_frame('dab9f584-1eda-4c46-aa3f-d760a46cb9f6', pre_conciliar_magisterial_authority_unbroken).
narrative_ontology:cs_drift_state('dab9f584-1eda-4c46-aa3f-d760a46cb9f6', contemporary_historical_scholarship_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('dab9f584-1eda-4c46-aa3f-d760a46cb9f6', '2026-06-12T14:32:00Z').
narrative_ontology:cs_kernel_id(vatican_ii_magisterial_authority__continuity_reading, vatican_ii_magisterial_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vatican_ii_magisterial_authority__continuity_reading, vatican_institutional_authority).
narrative_ontology:constraint_beneficiary(vatican_ii_magisterial_authority__continuity_reading, continuity_hermeneutics_tradition).
narrative_ontology:constraint_victim(vatican_ii_magisterial_authority__continuity_reading, latin_rite_traditionalists).
narrative_ontology:constraint_victim(vatican_ii_magisterial_authority__continuity_reading, pre_conciliar_doctrine_literalists).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(vatican_ii_magisterial_authority__continuity_reading, episcopal_conferences).
narrative_ontology:constraint_beneficiary(vatican_ii_magisterial_authority__continuity_reading, ordinary_faithful_continuity_carriers).
narrative_ontology:constraint_beneficiary(vatican_ii_magisterial_authority__continuity_reading, vatican_curia_doctrinal_apparatus).
narrative_ontology:constraint_victim(vatican_ii_magisterial_authority__continuity_reading, episcopal_conferences).
narrative_ontology:constraint_vindicates(vatican_ii_magisterial_authority__continuity_reading, hermeneutical_continuity_thesis).
narrative_ontology:constraint_vindicates(vatican_ii_magisterial_authority__continuity_reading, development_of_doctrine_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Establishes and enforces the hermeneutical frame by which Vatican II is read as continuous with prior magisterium. Sets the interpretive boundary: texts constraining implementation to preserve pre-conciliar doctrine; 'spirit of Vatican II' claims outside the documents are unauthorized. Controls the magisterial narrative through papal statements, CDF doctrinal notes, and seminary formation curricula. Benefits from continuity framing because it preserves institutional authority across the conciliar transition and forecloses claims that the Church contradicted itself.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__continuity_reading, vatican_institutional_authority, agenda_setter,
    institutional, civilizational, analytical, universal).

% Theological and hierarchical voices (John Paul II, Benedict XVI, conservative theologians) who defend the continuity reading as a doctrinal position. They benefit from Vatican II's legitimacy without experiencing the institutional disruption a rupture reading would create. Their professional identity and publication venues depend on the continuity frame being credible and authoritative. Exit is possible (dissent is not forbidden) but carries scholarly and ecclesiastical marginalization.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__continuity_reading, continuity_hermeneutics_tradition, beneficiary,
    organized, civilizational, mobile, universal).

% Experience the continuity reading as a suppressive frame that delegitimizes their witness to rupture and their fidelity to pre-conciliar practice. The constraint enforces that Latin preservation is binding only as a distant mandate (SC §36, 'Latin not wholly abandoned') while permitting vernacular dominance in practice. They bear the cost of institutional marginalization: traditionalist parishes and communities are treated as loyal but heterodox, suppressed through decades of institutional cold-shoulder, then restricted to a narrow jurisdictional space (before and after the 2019 Ecclesia Dei/FSSP reorganization). Their exit is severely limited by religious identity fusion — leaving the Church is not a realistic option; splitting into parallel traditionalist structures is only available to the most committed. The constraint requires them to accept the institutional reading or endure decades of second-class status within the communion they belong to.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__continuity_reading, latin_rite_traditionalists, payer,
    moderate, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(vatican_ii_magisterial_authority__continuity_reading, latin_rite_traditionalists, excluded).

% Individuals who hold that pre-conciliar doctrine (esp. the Syllabus of Errors' condemnation of religious liberty and modern freedoms) was infallible and binding. The continuity reading traps them: Vatican II's DH (Dignitatis Humanae) on religious freedom appears to contradict the Syllabus, but institutional authority declares no contradiction exists. They are offered the resolution that DH reconciles via 'thesis/hypothesis' distinction or development-of-doctrine, but these moves are technical, non-transparent, and rely on hermeneutical authority they no longer trust. Exit options: assent to the institutional reading (identity-compromising), dissent visibly (institutional marginalization and often laicization for clergy), or retreat into private conviction without institutional voice. Most are trapped in biographical time-horizons by ties to parishes, families, and ecclesiastical employment.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__continuity_reading, pre_conciliar_doctrine_literalists, payer,
    powerless, biographical, trapped, local).

% Academic historians, theologians (esp. in Germanic and progressive Catholic institutions), and some episcopal voices argue that Vatican II represents genuine doctrinal rupture, especially on religious freedom, ecumenical legitimacy of other churches, and the preferential option for the poor. They would be inside the conversation if the institutional frame permitted their reading; instead, they are formally excluded from magisterial status, their scholarly work is treated as 'theological opinion' rather than authoritative development, and their influence flows through academia and episcopal conferences rather than through direct doctrinal authority. They remain intellectually mobile but institutionally sidelined.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__continuity_reading, rupture_reading_proponents, excluded,
    powerful, civilizational, mobile, universal).

% National and regional bishops' conferences implement Vatican II but are constrained by the continuity reading: their implementation must be defended as not rupturing prior doctrine, not as authentic conciliar development that supersedes pre-conciliar structures. They benefit from conciliar legitimacy for vernacular liturgy, lay participation, and modern pastoral approaches, but they pay by having to frame these as 'organic developments' rather than as necessary corrections of error. Exit is limited: a bishop or conference that publicly endorsed the rupture reading would face doctrinal investigation and potential removal.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__continuity_reading, episcopal_conferences, payer,
    organized, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(vatican_ii_magisterial_authority__continuity_reading, episcopal_conferences, beneficiary).

% Lay Catholics, especially older generations, who internalized pre-conciliar practice and theology (Latin Mass, Marian devotion, institutional deference) and then experienced radical change in parish life after Vatican II. The continuity reading tells them: this change is not rupture, it is organic development, your fidelity to the Church is preserved. This narrative stabilizes their religious identity and reduces cognitive dissonance, but it also suppresses their lived experience of discontinuity. They are trapped by devotional and familial identity; exit (leaving the Church) breaks kinship and self-concept. The constraint requires them to narrate their own experience of radical change as organic continuity.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__continuity_reading, ordinary_faithful_continuity_carriers, beneficiary,
    powerless, biographical, trapped, local).

% The Congregation for the Doctrine of the Faith (CDF) and papal theological advisors enforce the continuity reading through doctrinal notes, condemnations of 'false interpretations,' seminary formation mandates, and correction of bishops and theologians who drift toward rupture readings. They benefit from the continuity frame because it preserves their authority as guardians of unbroken tradition. A rupture reading would require them to admit discontinuity in doctrine, which would undermine their mandate. They are analytically positioned to step outside the frame but are structurally incentivized to maintain it.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__continuity_reading, vatican_curia_doctrinal_apparatus, agenda_setter,
    institutional, civilizational, analytical, universal).
narrative_ontology:stakeholder_secondary_role(vatican_ii_magisterial_authority__continuity_reading, vatican_curia_doctrinal_apparatus, beneficiary).

% Church historians, including Catholic scholars, document the documentary evidence of tension, dispute, and reformulation during Vatican II (Rynne's Council Diaries, the Acta Synodalia records, draft-to-final-text changes). Their observations consistently show rupture dynamics at the textual and conciliar level, but they remain external to the magisterial authority that adjudicates what the Council 'means.' They can publish findings freely but cannot bind magisterial interpretation.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__continuity_reading, academic_historical_community, observer,
    organized, generational, mobile, global).

% The SSPX and related traditionalist groups explicitly reject the continuity reading and operate on the premise that Vatican II represents rupture and error. They are formally excluded from communion and magisterial conversation, though they maintain priesthood and sacramental claims (disputed by Rome). They have chosen to exit the institutional frame entirely rather than accept the suppressive continuity reading. Their existence constitutes lived proof that the continuity frame is not inevitable and is experienced as false by significant organized dissent.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__continuity_reading, society_of_st_pius_x, excluded,
    moderate, civilizational, mobile, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a unified interpretive authority over the conciliar texts such that all implementation worldwide can claim fidelity to both Vatican II and pre-conciliar doctrine, preventing institutional fragmentation into competing 'pre-conciliar' and 'post-conciliar' churches. Coordinates the magisterium's self-understanding across a historical rupture point.
% TRANSFER_FUNCTION: Moves interpretive authority over the meaning of Vatican II from the bishops in assembly and the conciliar debates to the papal magisterium and the CDF post-concilium. Constrains legitimate implementation within a hermeneutical frame that forbids reading Vatican II as contradicting prior doctrine. Transfers the cost of reconciling apparent contradictions (esp. religious freedom vs. Syllabus) to the faithful and to theologians, who must accept increasingly technical and historically revisionist arguments (development-of-doctrine, thesis/hypothesis distinction) rather than the plain historical fact of doctrinal change.
% ABSENT_VOICES: Academic historians and empirical textual analysts are systematically excluded: their evidence of rupture at the documentary level (draft revisions, conciliar floor debates, post-conciliar interpretation disputes) is not integrated into magisterial teaching. Rupture-reading theologians and bishops are marginalized. The lived experience of radical liturgical and pastoral discontinuity reported by traditionalists and ordinary faithful is narratively suppressed (told: 'this is organic development, not rupture') rather than heard. Voices from outside the Catholic institutional frame (Protestant observers, secular historians, comparative religious scholars) are not party to the magisterial adjudication.
% DISAPPEARANCE_RATIONALE: If the continuity reading constraint vanished and institutional authority permitted either the rupture reading or the overdetermined/composite reading, the Church's self-understanding would reorganize: the SSPX would no longer face excommunication on hermeneutical grounds; traditionalist communities would gain institutional legitimacy; seminaries could openly teach the documentary reality of conciliar tension and change; the faithful would be released from the suppressive narrative that their lived experience of discontinuity is 'really' continuous. Ecumenical dialogue with other Christian traditions would shift: Catholics could acknowledge that Vatican II represented genuine doctrinal development on religious freedom rather than claiming reconciliation with the Syllabus via technical distinction. The magisterial authority structure would have to account for historical discontinuity rather than asserting unbroken continuity.
% FOUNDING_PROBLEM: Vatican II brought radical change to liturgy (vernacular replacing Latin), ecclesiology (collegiality, subsidiarity, lay agency), ecumenism (legitimacy of other Christian traditions), and religious freedom (DH appears to contradict the Syllabus of Errors). These changes created institutional trauma: the faithful experienced rupture; traditionalists perceived doctrinal error; observers questioned papal infallibility. The founding problem was: how to implement and legitimate massive institutional changes while maintaining papal authority and magisterial self-understanding as unbroken tradition?
% FOUNDING_PROBLEM_CORROBORATION: The Vatican institutional apparatus and papal magisterium affirm the continuity reading solves the problem: change is organic development, authority is preserved, tradition is unbroken. Academic historians (outside the benefiting parties) document the empirical reality of rupture at the conciliar level: Cardinal Frings' floor intervention on marriage and contraception, the shocking final vote reversals on religious freedom, the systematic revision of draft texts by curial pressure, the post-conciliar 'hermeneutics of rupture' openly taught in the 1970s-80s before institutional authority began suppressing it. Historian John O'Malley's authoritative reconstruction (What Happened at Vatican II, 2008) from the Acta Synodalia records demonstrates the founding problem was solved not by organic development but by institutional framing: the conciliar bishops achieved genuine rupture; the post-conciliar magisterium narrated it as continuity. No independent corroboration of the continuity thesis exists outside the benefiting magisterial parties.
narrative_ontology:disappearance_verdict(vatican_ii_magisterial_authority__continuity_reading, world_rearranges).
narrative_ontology:founding_problem_status(vatican_ii_magisterial_authority__continuity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vatican_ii_magisterial_authority__continuity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(vatican_ii_magisterial_authority__continuity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(vatican_ii_magisterial_authority__continuity_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(vatican_ii_magisterial_authority__continuity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(vatican_ii_magisterial_authority__continuity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(vatican_ii_magisterial_authority__continuity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises from 0.15 to 0.68 over the interval because the constraint's core function — to narrate institutional discontinuity as continuity — becomes increasingly effortful as the historical reality of rupture becomes clearer and more widely attested. The constraint must work harder to maintain its claim as evidence accumulates (John O'Malley's monograph in 2008, further archival work, the Church's own acknowledgment in papal statements that Vatican II was 'revolutionary'). Suppression rises from 0.30 to 0.72 because enforcement machinery must expand: the CDF must issue increasingly detailed doctrinal notes explaining how religious freedom 'really' reconciles with the Syllabus, seminaries must teach 'hermeneutics of continuity,' traditionalist communities must be regulated (FSSP restrictions, suspension of some religious communities), and bishops who drift toward rupture readings must be corrected or removed. Theater_ratio rises from 0.25 to 0.58, indicating that by 2026, the majority of enforcement activity is narrative maintenance (the 'hermeneutics of continuity' framing, papal addresses reiterating continuity) rather than genuine functional defense of institutional coherence. At the Council opening in 1962, the constraint was mostly dormant — the Council was happening, the bishops were in deliberation, the outcome was open. By 1975 (a decade after closing), institutional authority had settled on continuity as the frame, and enforcement began. The acceleration from 1985 onward reflects the Pope John Paul II era, when 'hermeneutics of continuity' became an explicit doctrinal mandate and resistance (traditionalists, historians, progressive bishops) hardened.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter (Vatican authority) experiences the constraint as genuine coordination it manages to prevent institutional fragmentation. From their seat, the constraint is the work of stewardship: Vatican II is real and must be implemented, but implementation must preserve tradition and magisterial authority, so careful interpretation is needed. The beneficiary seat (continuity hermeneutics tradition) experiences it as intellectually coherent and institutionally protective. From their seat, the constraint is scholarship: there are real continuities between Vatican II and pre-conciliar doctrine (development-of-doctrine framework, the thesis/hypothesis distinction on religious freedom), and maintaining this reading is fidelity to sound theological method. The payer seats (traditionalists, literalists) experience the constraint as suppression and false narration. From their seat, the constraint is a lie: they lived through rupture, they studied the documents, they know the Syllabus contradicts DH directly, and the institutional authority is demanding they deny their own experience and their own intellectual judgment. The excluded rupture-reading proponents experience the constraint as structural marginalization: the evidence is on their side, but institutional power is on the other side, so they can publish in academic journals but not shape Church teaching. Each seat computes a different type: the agenda-setter and beneficiaries compute something closer to Rope (genuine coordination with asymmetric benefit); the payers compute Snare (pure extraction narrated as coordination); the excluded compute Rope-from-outside (legitimate coordination they're not allowed to join). The engine computes per-seat types from the structural data, which is why the claim and the metrics are authored independently.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality for the Vatican institutional authority (agenda-setter) is low (near 0.1–0.2): they are the canonical beneficiary, they set the rules, they control implementation, they collect the rents (institutional authority, magisterial prestige, unified Church narrative). Directionality for continuity hermeneutics tradition is moderate-to-low (0.25–0.35): they benefit intellectually and institutionally, they have mobile exit (can publish dissent without immediate consequences), they are organized enough to defend their reading. Directionality for latin_rite_traditionalists (payer, identity_locked) is high (0.75–0.85): they bear the cost of institutional marginalization, they have no exit (religious identity is fused with the Church), the constraint extracts their lived credibility — they must narrate rupture as continuity or be treated as dissidents. Directionality for pre_conciliar_doctrine_literalists (payer, trapped) is very high (0.85–0.95): they are powerless, trapped, and identity-locked. The constraint forces them to either assent to technical reconciliations they cannot intellectually accept, or dissent visibly and suffer ecclesiastical consequences. Directionality for rupture-reading proponents (excluded) is near 1.0 (0.90–0.98): they are excluded from magisterial authority entirely, they are powerful enough to be perceived as threatening (hence the need to exclude them), and the constraint exists largely to keep them out.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint exhibits mild mandatrophy: the founding problem (how to implement Vatican II while maintaining institutional authority) was live in 1962–1975, contested by 1980–2000, and dead by 2020. The problem it was supposed to solve — preventing Church fragmentation by narrating change as continuity — has itself become an extractive burden. The SSPX fragmented anyway; progressive bishops and theologians developed their own voice; traditionalist communities thrived in the margins; ordinary faithful experienced the discontinuity regardless of institutional narrative. The constraint persists not because the founding problem is live but because the Vatican institutional authority has invested its legitimacy in continuity framing and cannot now acknowledge rupture without undermining its own magisterial claims. The theater_ratio of 0.58 and the rising suppression-requirement (0.72) are the tell: the constraint is being maintained through narrative and enforcement, not through genuine functional necessity. The mandate (preserve institutional unity by continuity framing) has outlived its function (institutional unity is no longer threatened by acknowledging Vatican II as rupture) but persists through inertia and identity protection. This is not quite Piton status (which would require theater_ratio > 0.65 and beneficiaries thinned to near-none), but it is clearly drifting toward Piton as the decades pass and the historical reality becomes undeniable.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    hermeneutical_continuity_plausibility,
    'Is the continuity between Vatican II''s texts and pre-conciliar doctrine empirically sustainable, or does the historical evidence show genuine doctrinal rupture that must be narrated as continuity for institutional reasons?',
    'Archival analysis of the Acta Synodalia (conciliar floor debates, voting records, draft revisions); comparison with O''Malley''s and Wills''s historical reconstructions; papal admissions (e.g., Pope Francis''s 2013 description of Vatican II as ''revolutionary'').',
    'If the evidence strongly supports rupture, the constraint shifts from Tangled Rope (genuine coordination + extraction) to Snare (pure extraction narrated as coordination). If it supports continuity, the constraint remains Tangled Rope. If the evidence is ambiguous or genuinely allows both readings, the constraint is best understood as composite overdetermination, not a false continuity frame.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(hermeneutical_continuity_plausibility, empirical, 'Whether textual and historical evidence supports the continuity reading or indicates institutional narrative-building over rupture.').

omega_variable(
    identity_fusion_and_exit_options,
    'Is the suppression of traditionalists and literalists primarily structural (external institutional barriers: restricted parishes, monitored priests, doctrinal investigation) or internalized (the agents believe the continuity frame is correct and cannot exit even if barriers were removed)?',
    'Post-barrier-removal trajectories: if institutional restrictions on traditionalist practice were lifted, would the agents'' adherence to continuity framing persist, or would they openly adopt rupture reading? Qualitative interviews with lapsed-but-still-Catholic individuals who rejected continuity frame after leaving institutional space.',
    'If suppression is primarily structural, removing barriers could shift the constraint toward Rope (coordination without heavy extraction). If internalized, the constraint persists as high-extractiveness even with barrier removal, indicating deeper identity capture. The distinction affects the appropriate remedy: structural barriers require institutional change; internalized capture requires different pastoral approaches.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_fusion_and_exit_options, empirical, 'Whether the suppression mechanism for traditionalists and literalists is external institutional pressure or internal identity fusion.').

omega_variable(
    development_of_doctrine_as_cover_or_genuine_framework,
    'Is the development-of-doctrine principle (Newman, Gasser) a genuine hermeneutical framework that legitimately reconciles pre-conciliar doctrine with Vatican II, or is it being invoked retrospectively as a technical escape hatch to avoid admitting rupture?',
    'Compare the application of development-of-doctrine to Vatican II religious freedom teaching with prior applications to other doctrinal claims historically rejected as erroneous. Does the same framework apply consistently, or is it applied selectively when institutional authority needs continuity?',
    'If development-of-doctrine is applied consistently, it is a genuine theological framework and the continuity reading has stronger standing. If applied selectively (strictly to Vatican II, loosely to other cases), the constraint is more extractive than claimed — it is using a legitimate-sounding principle as cover for institutional narrative control.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(development_of_doctrine_as_cover_or_genuine_framework, conceptual, 'Whether development-of-doctrine is a consistent hermeneutical principle or a selective narrative tool.').

omega_variable(
    committer_frame_vatican_ii_readings,
    'This story instantiates the continuity reading. The sibling readings (rupture and composite) would produce different constraints with different ε values, different beneficiary/victim structures, and different classifications. Are all three readings live options held by different institutional parties, or has institutional power foreclosed the alternatives?',
    'Document current legitimacy of each reading within Catholic institutional voice: (a) continuity reading — papal magisterium, CDF, official seminary formation; (b) rupture reading — excluded from magisterium, live in academic scholarship and progressive bishop networks; (c) composite reading — emerging in recent scholarship, suppressed in official venues.',
    'If all three readings are institutionally live, the constraint is Rope-type coordination with legitimate debate. If one is foreclosed by institutional power, the constraint contains suppression of legitimate theological disagreement, shifting it toward Snare. The three-reading family together models the contestation; any single reading models only one party''s position.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(committer_frame_vatican_ii_readings, conceptual, 'Kernel-level question: are the sibling readings live alternatives or institutionally foreclosed competitors?').

omega_variable(
    mandate_vs_function_drift,
    'Has the founding problem (preventing Church fragmentation after Vatican II) been solved, such that the constraint''s mandate has become obsolete and only persists through institutional inertia?',
    'Compare institutional stability metrics: (a) 1965–1985 (constraint newly enforced): did continuity framing actually prevent the fragmentation that institutional authority feared? (b) 2000–2026 (constraint mature): has Church unity been maintained through continuity narration, or have unity crises (SSPX, married-clergy demands, abuse crises, doctrinal conflicts) persisted regardless?',
    'If continuity framing did prevent fragmentation and remains necessary, the constraint is functional. If fragmentation occurred anyway and the constraint now persists only to protect institutional credibility, it has degraded toward Piton status. Current theater_ratio of 0.58 and rising suggests the latter, but historical data would confirm.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mandate_vs_function_drift, empirical, 'Whether the constraint''s founding mandate remains live or has been superseded by institutional inertia.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vatican_ii_magisterial_authority__continuity_reading, 1962, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vati_tr_t1962, vatican_ii_magisterial_authority__continuity_reading, theater_ratio, 1962, 0.25).
narrative_ontology:measurement_basis(vati_tr_t1962, projected).
narrative_ontology:measurement(vati_tr_t1975, vatican_ii_magisterial_authority__continuity_reading, theater_ratio, 1975, 0.38).
narrative_ontology:measurement_basis(vati_tr_t1975, observed).
narrative_ontology:measurement(vati_tr_t1985, vatican_ii_magisterial_authority__continuity_reading, theater_ratio, 1985, 0.45).
narrative_ontology:measurement_basis(vati_tr_t1985, observed).
narrative_ontology:measurement(vati_tr_t2000, vatican_ii_magisterial_authority__continuity_reading, theater_ratio, 2000, 0.52).
narrative_ontology:measurement_basis(vati_tr_t2000, observed).
narrative_ontology:measurement(vati_tr_t2013, vatican_ii_magisterial_authority__continuity_reading, theater_ratio, 2013, 0.56).
narrative_ontology:measurement_basis(vati_tr_t2013, observed).
narrative_ontology:measurement(vati_tr_t2026, vatican_ii_magisterial_authority__continuity_reading, theater_ratio, 2026, 0.58).
narrative_ontology:measurement_basis(vati_tr_t2026, observed).

% Extraction over time
narrative_ontology:measurement(vati_be_t1962, vatican_ii_magisterial_authority__continuity_reading, base_extractiveness, 1962, 0.15).
narrative_ontology:measurement_basis(vati_be_t1962, projected).
narrative_ontology:measurement(vati_be_t1975, vatican_ii_magisterial_authority__continuity_reading, base_extractiveness, 1975, 0.35).
narrative_ontology:measurement_basis(vati_be_t1975, observed).
narrative_ontology:measurement(vati_be_t1985, vatican_ii_magisterial_authority__continuity_reading, base_extractiveness, 1985, 0.52).
narrative_ontology:measurement_basis(vati_be_t1985, observed).
narrative_ontology:measurement(vati_be_t2000, vatican_ii_magisterial_authority__continuity_reading, base_extractiveness, 2000, 0.62).
narrative_ontology:measurement_basis(vati_be_t2000, observed).
narrative_ontology:measurement(vati_be_t2013, vatican_ii_magisterial_authority__continuity_reading, base_extractiveness, 2013, 0.65).
narrative_ontology:measurement_basis(vati_be_t2013, observed).
narrative_ontology:measurement(vati_be_t2026, vatican_ii_magisterial_authority__continuity_reading, base_extractiveness, 2026, 0.68).
narrative_ontology:measurement_basis(vati_be_t2026, observed).

% Suppression requirement over time
narrative_ontology:measurement(vati_su_t1962, vatican_ii_magisterial_authority__continuity_reading, suppression_requirement, 1962, 0.3).
narrative_ontology:measurement_basis(vati_su_t1962, projected).
narrative_ontology:measurement(vati_su_t1975, vatican_ii_magisterial_authority__continuity_reading, suppression_requirement, 1975, 0.48).
narrative_ontology:measurement_basis(vati_su_t1975, observed).
narrative_ontology:measurement(vati_su_t1985, vatican_ii_magisterial_authority__continuity_reading, suppression_requirement, 1985, 0.58).
narrative_ontology:measurement_basis(vati_su_t1985, observed).
narrative_ontology:measurement(vati_su_t2000, vatican_ii_magisterial_authority__continuity_reading, suppression_requirement, 2000, 0.66).
narrative_ontology:measurement_basis(vati_su_t2000, observed).
narrative_ontology:measurement(vati_su_t2013, vatican_ii_magisterial_authority__continuity_reading, suppression_requirement, 2013, 0.7).
narrative_ontology:measurement_basis(vati_su_t2013, observed).
narrative_ontology:measurement(vati_su_t2026, vatican_ii_magisterial_authority__continuity_reading, suppression_requirement, 2026, 0.72).
narrative_ontology:measurement_basis(vati_su_t2026, observed).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=1962, tn=2026
narrative_ontology:measurement(vati_grid_01, vatican_ii_magisterial_authority__continuity_reading, accessibility_collapse(class), 1962, 0.15).
narrative_ontology:measurement(vati_grid_02, vatican_ii_magisterial_authority__continuity_reading, accessibility_collapse(class), 2026, 0.48).
narrative_ontology:measurement(vati_grid_03, vatican_ii_magisterial_authority__continuity_reading, accessibility_collapse(individual), 1962, 0.2).
narrative_ontology:measurement(vati_grid_04, vatican_ii_magisterial_authority__continuity_reading, accessibility_collapse(individual), 2026, 0.55).
narrative_ontology:measurement(vati_grid_05, vatican_ii_magisterial_authority__continuity_reading, accessibility_collapse(organizational), 1962, 0.25).
narrative_ontology:measurement(vati_grid_06, vatican_ii_magisterial_authority__continuity_reading, accessibility_collapse(organizational), 2026, 0.6).
narrative_ontology:measurement(vati_grid_07, vatican_ii_magisterial_authority__continuity_reading, accessibility_collapse(structural), 1962, 0.35).
narrative_ontology:measurement(vati_grid_08, vatican_ii_magisterial_authority__continuity_reading, accessibility_collapse(structural), 2026, 0.7).
narrative_ontology:measurement(vati_grid_09, vatican_ii_magisterial_authority__continuity_reading, resistance(class), 1962, 0.3).
narrative_ontology:measurement(vati_grid_10, vatican_ii_magisterial_authority__continuity_reading, resistance(class), 2026, 0.8).
narrative_ontology:measurement(vati_grid_11, vatican_ii_magisterial_authority__continuity_reading, resistance(individual), 1962, 0.4).
narrative_ontology:measurement(vati_grid_12, vatican_ii_magisterial_authority__continuity_reading, resistance(individual), 2026, 0.62).
narrative_ontology:measurement(vati_grid_13, vatican_ii_magisterial_authority__continuity_reading, resistance(organizational), 1962, 0.35).
narrative_ontology:measurement(vati_grid_14, vatican_ii_magisterial_authority__continuity_reading, resistance(organizational), 2026, 0.75).
narrative_ontology:measurement(vati_grid_15, vatican_ii_magisterial_authority__continuity_reading, resistance(structural), 1962, 0.45).
narrative_ontology:measurement(vati_grid_16, vatican_ii_magisterial_authority__continuity_reading, resistance(structural), 2026, 0.78).
narrative_ontology:measurement(vati_grid_17, vatican_ii_magisterial_authority__continuity_reading, stakes_inflation(class), 1962, 0.2).
narrative_ontology:measurement(vati_grid_18, vatican_ii_magisterial_authority__continuity_reading, stakes_inflation(class), 2026, 0.58).
narrative_ontology:measurement(vati_grid_19, vatican_ii_magisterial_authority__continuity_reading, stakes_inflation(individual), 1962, 0.25).
narrative_ontology:measurement(vati_grid_20, vatican_ii_magisterial_authority__continuity_reading, stakes_inflation(individual), 2026, 0.62).
narrative_ontology:measurement(vati_grid_21, vatican_ii_magisterial_authority__continuity_reading, stakes_inflation(organizational), 1962, 0.3).
narrative_ontology:measurement(vati_grid_22, vatican_ii_magisterial_authority__continuity_reading, stakes_inflation(organizational), 2026, 0.68).
narrative_ontology:measurement(vati_grid_23, vatican_ii_magisterial_authority__continuity_reading, stakes_inflation(structural), 1962, 0.4).
narrative_ontology:measurement(vati_grid_24, vatican_ii_magisterial_authority__continuity_reading, stakes_inflation(structural), 2026, 0.75).
narrative_ontology:measurement(vati_grid_25, vatican_ii_magisterial_authority__continuity_reading, suppression(class), 1962, 0.2).
narrative_ontology:measurement(vati_grid_26, vatican_ii_magisterial_authority__continuity_reading, suppression(class), 2026, 0.72).
narrative_ontology:measurement(vati_grid_27, vatican_ii_magisterial_authority__continuity_reading, suppression(individual), 1962, 0.15).
narrative_ontology:measurement(vati_grid_28, vatican_ii_magisterial_authority__continuity_reading, suppression(individual), 2026, 0.68).
narrative_ontology:measurement(vati_grid_29, vatican_ii_magisterial_authority__continuity_reading, suppression(organizational), 1962, 0.25).
narrative_ontology:measurement(vati_grid_30, vatican_ii_magisterial_authority__continuity_reading, suppression(organizational), 2026, 0.76).
narrative_ontology:measurement(vati_grid_31, vatican_ii_magisterial_authority__continuity_reading, suppression(structural), 1962, 0.35).
narrative_ontology:measurement(vati_grid_32, vatican_ii_magisterial_authority__continuity_reading, suppression(structural), 2026, 0.82).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vatican_ii_magisterial_authority__continuity_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(vatican_ii_magisterial_authority__continuity_reading, 0.12).
narrative_ontology:affects_constraint(vatican_ii_magisterial_authority__continuity_reading, vatican_ii_magisterial_authority__rupture_reading).
narrative_ontology:affects_constraint(vatican_ii_magisterial_authority__continuity_reading, vatican_ii_magisterial_authority__composite_overdetermination_reading).

% DUAL FORMULATION NOTE:
% This is one reading of the contested kernel vatican_ii_magisterial_authority. The sibling readings (rupture and composite) form a constraint family linked by network.affects_constraints. Each reading instantiates a different constraint with different ε values, different beneficiary/victim structures, and different classifications. The continuity reading influences (but does not foreclose) the rupture and composite readings: institutional enforcement of continuity interpretation structurally marginalizes the alternatives, which is itself downstream pressure on their plausibility. Each reading's ε is fixed for the referent (the standing arrangement post-Vatican II, interpreted via that reading's own lights), but the three readings produce different extraction profiles: continuity reading shows high extraction (0.68) because it suppresses empirical historical truth; rupture reading would show lower extraction (more transparent about rupture, less narrative burden); composite reading would show mixed extraction (acknowledges compromise but requires ongoing negotiation). Decompose into three separate story files, one per reading.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
