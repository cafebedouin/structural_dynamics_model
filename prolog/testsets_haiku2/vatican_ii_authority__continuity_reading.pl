% ============================================================================
% CONSTRAINT STORY: vatican_ii_authority__continuity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_vatican_ii_authority__continuity_reading, []).

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
 *   constraint_id: vatican_ii_authority__continuity_reading
 *   human_readable: Vatican II Continuity Authority (Organic Development Reading)
 *   domain: theology/ecclesiology/religious_authority
 *
 * SUMMARY:
 *   Vatican II (1962–1965) produced 16 documents claiming continuity with
 *   2,000 years of tradition while authorizing radical institutional reforms
 *   (liturgical modernization, collegiality, religious freedom doctrine,
 *   ecumenical openness). This constraint story instantiates the continuity
 *   reading: Vatican II is authentic organic development; the deposit of
 *   faith is unchanging; reforms are legitimate when interpreted through the
 *   hermeneutic of development and adjudicated by the living magisterium.
 *   This reading benefits progressive reformers who want both Vatican II's
 *   reforms AND fidelity claims, and it concentrates hermeneutical authority
 *   in the papacy. The structural cost is paid by traditionalist Catholics,
 *   whose objections to the reforms are declared inauthentic dissent from
 *   magisterial development. The kernel (Vatican II as authoritative text) is
 *   read through THREE distinct lenses — this story instantiates ONE of them
 *   (continuity). Sibling readings frame Vatican II as rupture (doctrinal
 *   break) or as composite overdetermination (ambiguous, incompatible
 *   rationales). The three readings coexist in live institutional dispute; no
 *   single reading has decisively foreclosed the others, though the hierarchy
 *   enforces the continuity reading as official doctrine.
 *
 * KEY AGENTS:
 *   - Progressive reformers (theological scholars, pastoral bishops) — structural beneficiaries of continuity framing; claim Vatican II legitimizes their modernization agenda
 *   - Post-conciliar papal magisterium — agenda-setter; monopolizes the interpretation of what counts as authentic development
 *   - Traditionalist faction objecting to rupture — structural payers; objections delegitimized by continuity framework; identity-locked (cannot exit the church without abandoning their faith commitment)
 *   - Schismatic communities (SSPX, sede vacante) — excluded from the authoritative conversation; declared in schism for rejecting continuity reading
 *   - Academic historians — observers; document historical record of the council's composition and text evolution, producing evidence relevant to adjudicating readings
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vatican_ii_authority__continuity_reading, 0.42).
domain_priors:suppression_score(vatican_ii_authority__continuity_reading, 0.58).
domain_priors:theater_ratio(vatican_ii_authority__continuity_reading, 0.31).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vatican_ii_authority__continuity_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(vatican_ii_authority__continuity_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(vatican_ii_authority__continuity_reading, theater_ratio, 0.31).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vatican_ii_authority__continuity_reading, accessibility_collapse, 0.67).
narrative_ontology:constraint_metric(vatican_ii_authority__continuity_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vatican_ii_authority__continuity_reading, rope).
narrative_ontology:human_readable(vatican_ii_authority__continuity_reading, "Vatican II Continuity Authority (Organic Development Reading)").
narrative_ontology:topic_domain(vatican_ii_authority__continuity_reading, "theology/ecclesiology/religious_authority").

domain_priors:requires_active_enforcement(vatican_ii_authority__continuity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vatican_ii_authority__continuity_reading, '32784c28-7fb8-4a5e-a22b-16d53e922422').
narrative_ontology:cs_kernel_codification('32784c28-7fb8-4a5e-a22b-16d53e922422', fixed_text).
narrative_ontology:cs_authority_grounding('32784c28-7fb8-4a5e-a22b-16d53e922422', lineage).
narrative_ontology:cs_interpretation_layer_present('32784c28-7fb8-4a5e-a22b-16d53e922422').
narrative_ontology:cs_reading_relation('32784c28-7fb8-4a5e-a22b-16d53e922422', vatican_ii_authority__rupture_reading, coexists_with).
narrative_ontology:cs_reading_relation('32784c28-7fb8-4a5e-a22b-16d53e922422', vatican_ii_authority__composite_overdetermination_reading, coexists_with).
narrative_ontology:cs_axiom('32784c28-7fb8-4a5e-a22b-16d53e922422', foundational, doctrine_unchanging_expression_develops).
narrative_ontology:cs_axiom_status(doctrine_unchanging_expression_develops, holdable).
narrative_ontology:cs_axiom_grounding('32784c28-7fb8-4a5e-a22b-16d53e922422', doctrine_unchanging_expression_develops, deontological).
narrative_ontology:cs_axiom('32784c28-7fb8-4a5e-a22b-16d53e922422', foundational, magisterium_sole_authentic_interpreter).
narrative_ontology:cs_axiom_status(magisterium_sole_authentic_interpreter, holdable).
narrative_ontology:cs_axiom_grounding('32784c28-7fb8-4a5e-a22b-16d53e922422', magisterium_sole_authentic_interpreter, conventional).
narrative_ontology:cs_reference_frame('32784c28-7fb8-4a5e-a22b-16d53e922422', magisterial_continuity_framework).
narrative_ontology:cs_drift_state('32784c28-7fb8-4a5e-a22b-16d53e922422', contemporary_traditionalist_resistance, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('32784c28-7fb8-4a5e-a22b-16d53e922422', '').
narrative_ontology:cs_kernel_id(vatican_ii_authority__continuity_reading, vatican_ii_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vatican_ii_authority__continuity_reading, progressive_reformers_claiming_continuity).
narrative_ontology:constraint_beneficiary(vatican_ii_authority__continuity_reading, post_conciliar_hierarchy).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(vatican_ii_authority__continuity_reading, lay_faithful_parish_level).
narrative_ontology:constraint_victim(vatican_ii_authority__continuity_reading, traditional_faction_objecting_rupture).
narrative_ontology:constraint_victim(vatican_ii_authority__continuity_reading, lay_faithful_parish_level).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Theological scholars, bishops, and pastoral leaders who endorse Vatican II's 16 documents as authentic development and use the continuity framework to justify post-conciliar reforms (liturgical modernization, increased lay participation, ecumenical openness, religious freedom doctrine). They benefit from the continuity reading because it grants them hermeneutical authority to interpret texts according to aggiornamento principles while maintaining fidelity claims. Their exit from the reading would require abandoning either the documents' validity or their reform agenda — a constrained choice between rejecting Vatican II or rejecting the modernization they pressed for.
narrative_ontology:constraint_stakeholder(vatican_ii_authority__continuity_reading, progressive_reformers_claiming_continuity, beneficiary,
    organized, generational, constrained, global).

% The institutional Catholic leadership (papal magisterium, Roman Curia, episcopal conferences) that promulgates Vatican II documents and adjudicates their interpretation. The continuity reading grants this hierarchy monopoly authority over the council's hermeneutics — only the living magisterium can authentically develop doctrine. They enforce the reading by declaring dissenting interpretations (rupture, over-determined ambiguity) as inauthentic or disloyal, while rewarding conformity with ecclesiastical advancement. Their exit options are extensive: they can revise the reading, convene a new council, or issue clarifying statements, but they do not leave the institutional church.
narrative_ontology:constraint_stakeholder(vatican_ii_authority__continuity_reading, post_conciliar_hierarchy, agenda_setter,
    institutional, generational, arbitrage, global).

% Conservative theologians, bishops, and lay communities (FSSP, SSPX sympathizers, sede vacante critics, and traditionalist parishes) who contest the continuity reading on grounds that Vatican II teachings contradict prior dogma (on religious freedom, collegiality, nature of priesthood, ecumenism). They bear the cost of the continuity framework because acceptance of it delegitimizes their objections — the hierarchy uses the continuity claim to declare their resistance to reforms as obstinate rejection of the magisterium. Their exit is severely identity-locked: leaving the Catholic Church to preserve traditional teaching means abandoning their ecclesial identity entirely. They remain inside, paying the cost of institutional marginalization and hermeneutical disempowerment.
narrative_ontology:constraint_stakeholder(vatican_ii_authority__continuity_reading, traditional_faction_objecting_rupture, payer,
    organized, generational, identity_locked, global).

% Communities that have formally rejected the post-Vatican II hierarchy (SSPX, sede vacante groups, various schismatic bodies) and thus are expelled from the conversation the continuity reading governs. They would argue that only a pre-conciliar hermeneutics can preserve doctrinal integrity, but their exclusion from the magisterium means their voice is administratively silenced — they are declared in schism or heresy by the very authority structure whose reading they contest.
narrative_ontology:constraint_stakeholder(vatican_ii_authority__continuity_reading, schismatic_and_sede_vacante_communities, excluded,
    moderate, generational, trapped, global).

% Secular and non-aligned historians of the council (Alberigo, Faggioli, Melloni, academic theologians outside institutional authority) who examine the historical record of the council's composition, floor debates, interventions, text evolution, and reception. They take no stake in the continuity-vs-rupture judgment; they document how the texts were actually generated and interpreted by different constituencies at different times, producing evidence relevant to adjudicating the reading but not themselves advocates for any reading.
narrative_ontology:constraint_stakeholder(vatican_ii_authority__continuity_reading, academic_historical_scholarship, observer,
    analytical, generational, analytical, global).

% The ordinary parish-level Catholic who experiences post-conciliar reforms (Mass in the vernacular, guitars instead of organs, married deacons, interfaith dialogue) as the lived reality of their faith. They benefit from reforms that make the liturgy comprehensible and participation accessible, but they also bear the disorientation of rapid change and the alienation some experience from abandoned traditional forms. The continuity reading is presented to them as official doctrine — they are not positioned as judges of Vatican II's meaning but as recipients of the hierarchy's authoritative interpretation.
narrative_ontology:constraint_stakeholder(vatican_ii_authority__continuity_reading, lay_faithful_parish_level, beneficiary,
    powerless, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(vatican_ii_authority__continuity_reading, lay_faithful_parish_level, payer).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(vatican_ii_authority__continuity_reading, post_conciliar_hierarchy).
narrative_ontology:fixing_cost_class(vatican_ii_authority__continuity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Vatican II and the continuity reading solve the institutional problem of how a 2,000-year tradition can remain authoritative while its concrete expression changes radically (liturgy, clergy discipline, ecumenical posture, state relations, biblical scholarship). The continuity framework coordinates the claim that the deposit of faith is unchanging with the reality that its pastoral expression must adapt, by subordinating reforms to a hermeneutic of development-not-rupture. This allows the post-conciliar church to claim fidelity while modernizing, enabling institutional legitimacy across both traditional and progressive constituencies.
% TRANSFER_FUNCTION: Moves hermeneutical authority from the whole people of God and the sensus fidelium toward the papal-curial magisterium as the sole arbiter of what counts as legitimate development. Moves interpretive control from the living, distributed theological tradition toward centralized curial doctrine offices. Moves credibility from historical scholarship toward ecclesiastical pronouncement — the continuity reading frames the council's 'authentic' meaning as knowable only through the living magisterium, not through historical investigation of what the bishops actually meant.
% ABSENT_VOICES: Historians who would testify that the council documents contain internal tensions, that some reforms represent genuine doctrinal shifts rather than development, that the council's meaning was genuinely contested among its own participants and cannot be read as univocally continuous. The traditionalist factions who argue for rupture are excluded not in fact but in authority — their objections are administratively invalidated as 'misreading' by the magisterium. Lay theological voices are absent from the council's own governance; women are entirely absent from the voting assembly and the authoritative interpretation afterward.
% DISAPPEARANCE_RATIONALE: If the continuity reading vanished and were replaced by either the rupture reading (Vatican II broke with tradition) or the composite reading (Vatican II is ambiguous/overdetermined), the institutional legitimacy of post-conciliar reforms would dissolve, episcopal authority would fragment into competing hermeneutics, and the Catholic Church would face an acute crisis of self-understanding. Massive institutional structures, liturgical practices, seminary curricula, and episcopal self-conception depend on the continuity frame being hegemonic. Its disappearance would require either a new authoritative decision (another council) or institutional schism.
% FOUNDING_PROBLEM: Vatican II was convened to update the church for the modern world while preserving doctrinal continuity. The council produced radical reforms in practice (liturgical language, collegiality, religious freedom teaching, ecumenical openness) that seemed discontinuous with prior teaching. The founding problem: how can these be legitimately done? The continuity reading answers: through authentic development, which is organic and homogeneous with tradition.
% FOUNDING_PROBLEM_CORROBORATION: Pope John XXIII, Vatican II's convener, explicitly invoked 'aggiornamento' (updating) as the council's goal, not doctrinal disruption — this is cited by continuity advocates. However, historians (Alberigo, Faggioli) document that the council's voting blocs, text negotiations, and ultimate documents represent genuine theological ruptures on collegiality, religious freedom, and relationship to modernity that cannot be credibly read as merely homogeneous development from pre-conciliar doctrine. The Catechism of the Catholic Church (1992) and Vatican documents (e.g., Ecclesia Dei, Ecclesia Dei Adflicta) continuously reiterate the continuity reading, but this is self-assertion by the beneficiary rather than independent corroboration. Academic church historians outside ecclesiastical authority structures largely attest that Vatican II involved substantial breaks, not merely development.
narrative_ontology:disappearance_verdict(vatican_ii_authority__continuity_reading, world_rearranges).
narrative_ontology:founding_problem_status(vatican_ii_authority__continuity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vatican_ii_authority__continuity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(vatican_ii_authority__continuity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(vatican_ii_authority__continuity_reading, 0.42, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(vatican_ii_authority__continuity_reading_tests).
:- end_tests(vatican_ii_authority__continuity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.42 at interval end) because the continuity reading concentrates hermeneutical authority in the magisterium without requiring massive coercion — the coordination function (how to modernize while claiming fidelity) is genuinely valuable to many constituencies. However, it is extractive because traditionalists pay a real cost (institutional marginalization, hermeneutical disempowerment) without compensation, and because the magisterium's monopoly on interpretation means dissent is categorized as disloyalty. Suppression is substantial (0.58) because maintaining the continuity reading requires continuously defeating rival interpretations of the same documents — traditionalist challenges are not refuted argumentatively but administratively excluded (schism declarations, denials of teaching posts). Theater ratio rises from 0.12 to 0.31 over the 60-year interval: initially the continuity framework was genuinely doing hermeneutical work (resolving real tensions), but as time passes and the historical reality of doctrinal shifts becomes undeniable, more enforcement energy goes to theatrical reassertion (commemorative documents, 'authentic interpretation' pronouncements) that defend the reading symbolically rather than argumentatively. The temporal pattern shows a constraint moving from genuine coordination (the early post-conciliar period) toward rent-seeking and theater (contemporary traditionalist pushback and official defensiveness). Accessibility collapse (0.67) is moderate-high because once one accepts the magisterium's monopoly on authentic development, alternatives largely collapse — but the collapse is not complete; rival readings remain live scholarly positions and attract institutional dissent (SSPX, sede vacante movements). Resistance (0.72) is substantial because the constraint meets real, organized pushback from traditionalists, from academic historians who document the council's ruptures, and from the renewal movements that Vatican II sparked which eventually challenged the hierarchy's own interpretations.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter (post-conciliar hierarchy) and the beneficiaries (progressive reformers) perceive the constraint as genuine rope: it solves a real coordination problem (how to modernize while staying faithful) and distributes the gains widely (reformers get their agenda, ordinary faithful get comprehensible liturgy, the institutional church maintains legitimacy across constituencies). The traditionalist payer, however, perceives this same structure as snare: the continuity framework is presented as logical necessity (this is what authentic development is), but it functions as a mechanism to delegitimize their objections by definition — if you object, you are rejecting the magisterium; if you object hard enough, you are in schism. The hierarchy controls what counts as coherent interpretation of the documents themselves, which is the extraction mechanism. The historical scholarship observer sees neither rope nor snare but rather a constraint imposed by institutional power: the documents are historically overdetermined (their meaning was genuinely contested among the bishops who wrote them), but the continuity reading selects one coherent interpretation and enforces it as official, suppressing the historical evidence that other readings were equally live among the council's participants.
 *
 * DIRECTIONALITY LOGIC:
 *   Progressive reformers (role: beneficiary) have low directionality (near 0.0) because they collect from the constraint — it legitimizes their agenda with fidelity claims they could not otherwise sustain. The post-conciliar hierarchy (role: agenda-setter) has arbitrage-grade exit (they can revise the reading, convene a new council, or clarify through new documents), so they sit at moderate directionality (0.3–0.4) — they benefit from the monopoly on interpretation but must continuously defend it. Traditionalist objectors (role: payer, but identity-locked) have high directionality (near 0.85) because they bear the cost (institutional marginalization, hermeneutical disempowerment, declared schism if they push too hard) and cannot exit without abandoning their identity as Catholic faithful committed to pre-conciliar doctrine. The identity-locking is crucial: they remain inside the church (trapped, not mobile) paying the cost of the constraint, because leaving means ceasing to be Catholic. This is the structural signature of a high-directionality target in an identity-coordination context. Lay faithful (role: beneficiary + payer) have near-symmetric directionality (0.45–0.55): they benefit from accessible reforms but also bear the alienation and disorientation of rapid change, and they have no hermeneutical voice in adjudicating the reading — it is imposed on them by institutional authority.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint is in early mandatrophy. The founding problem (how to modernize while claiming continuity) was live at Vatican II and for the first two decades after. By year 40–60 of the interval (2005–2025), the founding problem has substantially changed: the reforms are implemented, the modernization is accomplished, and the question is no longer 'how do we justify this change?' but 'do we want to keep it, slow it, or reverse it?' The continuity reading's function has shifted from solving a coordination problem to defending an accomplished fact against traditionalist challenge. The theater ratio's rise from 0.12 to 0.31 is the symptom: the constraint is increasingly maintained by symbolic reassertion (official declarations defending Vatican II's meaning) rather than by genuine hermeneutical work. A constraint that began as rope (solving the real problem of modernization-with-fidelity) is drifting toward piton (maintained by institutional inertia and theatrical defense even as the original coordination problem has been solved and the reform agenda accomplished). The magisterium could now honestly reframe the council as 'a decisive break with certain pre-conciliar practices, justified on pastoral grounds' — which would eliminate the mandatrophy by acknowledging what happened. Instead, the continuity frame persists, increasingly as performance.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    historical_rupture_vs_doctrinal_continuity,
    'Can Vatican II''s reforms (religious freedom doctrine, collegial governance, liturgical modernization, ecumenical openness) coherently be read as organic development from pre-conciliar doctrine, or do they represent genuine doctrinal shifts that break with prior teaching in substance, not merely expression?',
    'Detailed historical analysis of pre-conciliar magisterial teaching on each reform domain (religious freedom, collegiality, etc.), comparing propositions side-by-side; examination of the council''s voting record, floor debates, and text amendments to determine whether the final formulations represent continuity with or departure from prior doctrine; analysis of subsequent magisterial interpretations to assess whether later popes have had to revise Vatican II teachings (indicating that Vatican II itself contained errors, not the ''authentic development'' account allows).',
    'If historical analysis confirms genuine doctrinal shifts, the continuity reading collapses and the constraint becomes mandatrophy-resolved or reclassifies as snare (the continuity claim is a cover story). If analysis confirms homogeneous development, the continuity reading is vindicated and the constraint persists as rope. The rupture and composite readings shift accordingly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(historical_rupture_vs_doctrinal_continuity, empirical, 'Whether Vatican II''s doctrinal content represents development or rupture with prior magisterial teaching.').

omega_variable(
    magisterial_monopoly_vs_sensus_fidelium,
    'Does the continuity reading''s concentration of hermeneutical authority in the papal magisterium accurately reflect Catholic doctrine on how the church discerns development, or does it suppress the sensus fidelium (the sense of the faithful) and the role of distributed theological scholarship in authentic interpretation?',
    'Examination of Vatican II''s own Lumen Gentium (LG 12) on the sensus fidelium; analysis of whether the magisterium has solicited, incorporated, or dismissed input from theologians, historians, and lay believers in interpreting the council''s meaning; comparison with how development has actually been discerned historically (e.g., in the formulation of Marian doctrines, where the sensus fidelium played a recognized role); observation of whether traditionalist objections have been addressed argumentatively or administratively suppressed.',
    'If the magisterium''s monopoly is over-reaching relative to Catholic teaching on discernment, the constraint is revealed as extractive concentration of power (shifts toward snare classification). If the monopoly is justified by doctrine and practice, the rope classification holds. The traditional payer''s actual status (are they genuinely excluded from authentic interpretation, or do they have standing to be heard?) is the factual ground for this omega''s resolution.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(magisterial_monopoly_vs_sensus_fidelium, conceptual, 'Whether Vatican II''s own teaching on sensus fidelium is honored or violated by the continuity reading''s hermeneutical structure.').

omega_variable(
    coordination_function_obsolescence,
    'The founding problem (how to modernize while claiming fidelity to tradition) was live at Vatican II, but is it still live 60 years later? Has the constraint''s original coordination function been accomplished, making its persistence attributable to institutional inertia rather than real coordination need?',
    'Examination of the magisterium''s own statements and actions post-1985 (John Paul II''s papacy onward): are new documents defending the continuity reading because it solves a live coordination problem, or because it defends an accomplished institutional transition? Historical observation of whether the reforms (vernacular liturgy, collegiality, religious freedom) are now universally accepted within the church or continue to face organized resistance; if universally accepted, the coordination function is satisfied and persistence indicates mandatrophy; if resistance persists, the coordination function remains live.',
    'If the coordination function is obsolete, the constraint is mandatrophy-resolved and should be reclassified as piton (maintained by theater and inertia). The theater_ratio''s rise from 0.12 to 0.31 supports this hypothesis. Resolution would require the magisterium to acknowledge the historical reality of Vatican II''s shifts rather than continue performative continuity claims.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(coordination_function_obsolescence, empirical, 'Whether Vatican II''s founding coordination problem remains live or has been solved, making the constraint''s persistence attributable to mandate obsolescence.').

omega_variable(
    reading_foreclosure_vs_coexistence,
    'Does the continuity reading logically foreclose the rupture and composite readings within a single coherent theological framework, or do the three readings genuinely coexist as live positions held by different parties with no logical resolution possible?',
    'Formal logical analysis of the three readings'' core premises: (continuity) ''deposit of faith is unchanging; Vatican II is organic development''; (rupture) ''Vatican II contains doctrinal breaks with prior teaching''; (composite) ''Vatican II''s tensions are irreducible and cannot be resolved into either continuity or rupture.'' Do any two of these premises directly contradict each other such that no framework could hold both? Or can different parties maintain their respective readings without logical contradiction, differing only on how to weigh evidence and tradition?',
    'If the continuity reading logically forecloses the rupture reading, the cs_structure.reading_relations should be revised from coexists_with to forecloses, and the constraint''s classification as rope becomes more secure (it represents a resolved coordination problem). If the readings genuinely coexist without logical foreclosure, the constraint is tangled_rope or snare (the appearance of resolution is institutional enforcement, not logical necessity). Historical observation of institutional treatment (traditionalists declared in schism vs. treated as holding a dissenting but legitimate position within the church) is evidence for whether the authority structure is enforcing foreclosure or merely suppressing coexistence.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_foreclosure_vs_coexistence, conceptual, 'Whether the continuity reading logically forecloses rival readings or permits coexistence.').

omega_variable(
    identity_locking_mechanism_in_traditionalism,
    'Is the traditionalist faction''s exit-blocking (identity_locked status) a structural feature of Catholic faith commitment (one cannot be Catholic without accepting magisterial authority) or an artifact of the continuity reading (the reading enforces a false equivalence between accepting Vatican II and accepting the magisterium, preventing traditionalists from being both faithful and dissenting)?',
    'Examination of pre-Vatican II Catholic theology on the sensus fidelium and legitimate dissent from magisterial teaching; analysis of whether the continuity reading has reframed the magisterium''s authority such that dissent from Vatican II is now treated as equivalent to schism; observation of whether traditionalists who accept papal authority but contest Vatican II''s continuity are afforded any institutional space, or are forced to choose between full conformity and schism; empirical observation of actual traditionalist communities to determine whether their exit is genuinely identity-locked or constrained by institutional punishment (a different mechanism).',
    'If identity-locking is structural to Catholicism (not an artifact of the continuity reading), the traditionalist payers'' high directionality is unavoidable, and the constraint remains within the bounds of legitimate authority. If the continuity reading artificially creates identity-locking by refusing to allow dissent-within-conformity, the constraint is revealed as extractive suppression — the reading manufactures the impossibility of exit by equating disagreement with schism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_locking_mechanism_in_traditionalism, empirical, 'Whether traditionalist Catholics'' identity-locked status is intrinsic to faith or imposed by the continuity reading''s enforcement structure.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vatican_ii_authority__continuity_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vii_cont_tr_t0, vatican_ii_authority__continuity_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement_basis(vii_cont_tr_t0, observed).
narrative_ontology:measurement(vii_cont_tr_t10, vatican_ii_authority__continuity_reading, theater_ratio, 10, 0.18).
narrative_ontology:measurement_basis(vii_cont_tr_t10, observed).
narrative_ontology:measurement(vii_cont_tr_t20, vatican_ii_authority__continuity_reading, theater_ratio, 20, 0.24).
narrative_ontology:measurement_basis(vii_cont_tr_t20, observed).
narrative_ontology:measurement(vii_cont_tr_t30, vatican_ii_authority__continuity_reading, theater_ratio, 30, 0.29).
narrative_ontology:measurement_basis(vii_cont_tr_t30, observed).
narrative_ontology:measurement(vii_cont_tr_t40, vatican_ii_authority__continuity_reading, theater_ratio, 40, 0.32).
narrative_ontology:measurement_basis(vii_cont_tr_t40, observed).
narrative_ontology:measurement(vii_cont_tr_t50, vatican_ii_authority__continuity_reading, theater_ratio, 50, 0.31).
narrative_ontology:measurement_basis(vii_cont_tr_t50, observed).
narrative_ontology:measurement(vii_cont_tr_t60, vatican_ii_authority__continuity_reading, theater_ratio, 60, 0.31).
narrative_ontology:measurement_basis(vii_cont_tr_t60, observed).

% Extraction over time
narrative_ontology:measurement(vii_cont_be_t0, vatican_ii_authority__continuity_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement_basis(vii_cont_be_t0, observed).
narrative_ontology:measurement(vii_cont_be_t10, vatican_ii_authority__continuity_reading, base_extractiveness, 10, 0.35).
narrative_ontology:measurement_basis(vii_cont_be_t10, observed).
narrative_ontology:measurement(vii_cont_be_t20, vatican_ii_authority__continuity_reading, base_extractiveness, 20, 0.38).
narrative_ontology:measurement_basis(vii_cont_be_t20, observed).
narrative_ontology:measurement(vii_cont_be_t30, vatican_ii_authority__continuity_reading, base_extractiveness, 30, 0.41).
narrative_ontology:measurement_basis(vii_cont_be_t30, observed).
narrative_ontology:measurement(vii_cont_be_t40, vatican_ii_authority__continuity_reading, base_extractiveness, 40, 0.43).
narrative_ontology:measurement_basis(vii_cont_be_t40, observed).
narrative_ontology:measurement(vii_cont_be_t50, vatican_ii_authority__continuity_reading, base_extractiveness, 50, 0.42).
narrative_ontology:measurement_basis(vii_cont_be_t50, observed).
narrative_ontology:measurement(vii_cont_be_t60, vatican_ii_authority__continuity_reading, base_extractiveness, 60, 0.42).
narrative_ontology:measurement_basis(vii_cont_be_t60, observed).

% Suppression requirement over time
narrative_ontology:measurement(vii_cont_su_t0, vatican_ii_authority__continuity_reading, suppression_requirement, 0, 0.42).
narrative_ontology:measurement_basis(vii_cont_su_t0, observed).
narrative_ontology:measurement(vii_cont_su_t10, vatican_ii_authority__continuity_reading, suppression_requirement, 10, 0.5).
narrative_ontology:measurement_basis(vii_cont_su_t10, observed).
narrative_ontology:measurement(vii_cont_su_t20, vatican_ii_authority__continuity_reading, suppression_requirement, 20, 0.55).
narrative_ontology:measurement_basis(vii_cont_su_t20, observed).
narrative_ontology:measurement(vii_cont_su_t30, vatican_ii_authority__continuity_reading, suppression_requirement, 30, 0.59).
narrative_ontology:measurement_basis(vii_cont_su_t30, observed).
narrative_ontology:measurement(vii_cont_su_t40, vatican_ii_authority__continuity_reading, suppression_requirement, 40, 0.61).
narrative_ontology:measurement_basis(vii_cont_su_t40, observed).
narrative_ontology:measurement(vii_cont_su_t50, vatican_ii_authority__continuity_reading, suppression_requirement, 50, 0.59).
narrative_ontology:measurement_basis(vii_cont_su_t50, observed).
narrative_ontology:measurement(vii_cont_su_t60, vatican_ii_authority__continuity_reading, suppression_requirement, 60, 0.58).
narrative_ontology:measurement_basis(vii_cont_su_t60, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vatican_ii_authority__continuity_reading, global_infrastructure).
narrative_ontology:boltzmann_floor_override(vatican_ii_authority__continuity_reading, 0.25).
narrative_ontology:affects_constraint(vatican_ii_authority__continuity_reading, vatican_ii_authority__rupture_reading).
narrative_ontology:affects_constraint(vatican_ii_authority__continuity_reading, vatican_ii_authority__composite_overdetermination_reading).
narrative_ontology:affects_constraint(vatican_ii_authority__continuity_reading, catholic_magisterial_authority).
narrative_ontology:affects_constraint(vatican_ii_authority__continuity_reading, sensus_fidelium_authority).

% DUAL FORMULATION NOTE:
% Vatican II authority decomposes into three structurally distinct constraint stories: continuity_reading (this story), rupture_reading, and composite_overdetermination_reading. Each reading instantiates a different constraint with different ε values, beneficiary/victim structures, and classifications, all grounded in the same kernel (Vatican II documents) but interpreted through different hermeneutical commitments. The stories are linked as siblings in a constraint family; they affect one another because acceptance of one reading changes the legitimacy conditions and resource availability for the others. The continuity reading is the hegemonic institutional reading (enforced by the post-conciliar papacy); the rupture and composite readings are minority positions held by traditionalist factions and academic historians. No single reading has logically foreclosed the others despite institutional efforts to suppress competing readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(vatican_ii_authority__continuity_reading, organized, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
