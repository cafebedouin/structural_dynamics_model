% ============================================================================
% CONSTRAINT STORY: vatican_ii_doctrinal_authority__rupture_progressive_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_vatican_ii_doctrinal_authority__rupture_progressive_reading, []).

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
 *   constraint_id: vatican_ii_doctrinal_authority__rupture_progressive_reading
 *   human_readable: Vatican II Progressive Hermeneutics: Rupture Doctrine and Spirit-of-the-Council Authority
 *   domain: ecclesiology/institutional_history/hermeneutics
 *
 * SUMMARY:
 *   Vatican II (1962–1965) was a global Council of bishops that produced 16
 *   documents on Church doctrine and practice. The Council's texts contain
 *   genuine ambiguities and apparent tensions between innovation (religious
 *   freedom doctrine, ecumenical openness, liturgical reform) and continuity
 *   language (the Council is part of an unbroken apostolic tradition). This
 *   constraint story instantiates the progressive reading: Vatican II
 *   represents a necessary rupture with pre-conciliar rigidity; the textual
 *   ambiguities are read as intentional openings for further doctrinal
 *   development; the 'spirit of the Council' grants authority to ongoing
 *   reform beyond what the texts explicitly state. This reading has dominated
 *   Catholic institutions (episcopal conferences, seminaries, theological
 *   bodies) since the 1970s. However, it is contested by a traditionalist
 *   reading (which sees Vatican II as containing errors and ambiguities that
 *   enabled heterodox implementation) and by a continuity reading (which
 *   reads the Council as organic development within unchanging doctrine). The
 *   claim and metrics are intentionally divergent: this reading is CLAIMED as
 *   tangled_rope (genuine coordination function—resolving ambiguity about
 *   doctrinal development—plus asymmetric extraction of authority from
 *   traditionalist clergy) while the authored metrics describe moderate
 *   extractiveness and moderate suppression. The divergence reflects the
 *   structural ambiguity: is the progressive reading a genuine coordination
 *   solution to a real theological problem, or is it a power grab dressed in
 *   theological language? The engine's per-seat classification will expose
 *   whether beneficiary seats compute the constraint as rope while victim
 *   seats compute it as snare.
 *
 * KEY AGENTS:
 *   - progressive_reform_movements: beneficiary/agenda-setter (organized, generational, mobile) — interpret Vatican II as authorizing ongoing doctrinal development
 *   - traditionalist_clergy: payer (moderate power, biographical, identity-locked) — experience the progressive reading as delegitimization of pre-conciliar doctrine and their pastoral identity
 *   - ecumenical_dialogue_institutions: beneficiary (institutional, generational, constrained) — gain legitimacy for interfaith engagement from the progressive reading
 *   - pre_conciliar_institutional_beneficiaries: payer (institutional, generational, constrained) — lose interpretive monopoly as doctrinal authority distributes
 *   - vatican_doctrinal_commission: agenda-setter (institutional, generational, analytical) — must authoritatively adjudicate between readings while managing ambiguity
 *   - global_episcopacy: beneficiary/payer (organized, generational, constrained) — empowered by the reading but also burdened with managing contradictions
 *   - traditionalist_communities: excluded (organized, generational, trapped) — structurally barred from institutional influence on interpretation
 *   - conciliar_theologians: observer (analytical, biographical, analytical) — produce historical and textual evidence about what the Council actually said and intended
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vatican_ii_doctrinal_authority__rupture_progressive_reading, 0.62).
domain_priors:suppression_score(vatican_ii_doctrinal_authority__rupture_progressive_reading, 0.41).
domain_priors:theater_ratio(vatican_ii_doctrinal_authority__rupture_progressive_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__rupture_progressive_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__rupture_progressive_reading, suppression_requirement, 0.41).
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__rupture_progressive_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__rupture_progressive_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__rupture_progressive_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vatican_ii_doctrinal_authority__rupture_progressive_reading, tangled_rope).
narrative_ontology:human_readable(vatican_ii_doctrinal_authority__rupture_progressive_reading, "Vatican II Progressive Hermeneutics: Rupture Doctrine and Spirit-of-the-Council Authority").
narrative_ontology:topic_domain(vatican_ii_doctrinal_authority__rupture_progressive_reading, "ecclesiology/institutional_history/hermeneutics").

domain_priors:requires_active_enforcement(vatican_ii_doctrinal_authority__rupture_progressive_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vatican_ii_doctrinal_authority__rupture_progressive_reading, 'ab2b20db-602e-4d87-af96-772e3d35fedc').
narrative_ontology:cs_kernel_codification('ab2b20db-602e-4d87-af96-772e3d35fedc', formalized).
narrative_ontology:cs_authority_grounding('ab2b20db-602e-4d87-af96-772e3d35fedc', lineage).
narrative_ontology:cs_interpretation_layer_present('ab2b20db-602e-4d87-af96-772e3d35fedc').
narrative_ontology:cs_reading_relation('ab2b20db-602e-4d87-af96-772e3d35fedc', vatican_ii_doctrinal_authority__continuity_reading, forecloses).
narrative_ontology:cs_reading_relation('ab2b20db-602e-4d87-af96-772e3d35fedc', vatican_ii_doctrinal_authority__rupture_traditionalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('ab2b20db-602e-4d87-af96-772e3d35fedc', vatican_ii_doctrinal_authority__composite_overdetermination_reading, influences).
narrative_ontology:cs_axiom('ab2b20db-602e-4d87-af96-772e3d35fedc', foundational, doctrinal_rupture_necessary).
narrative_ontology:cs_axiom_status(doctrinal_rupture_necessary, holdable).
narrative_ontology:cs_axiom_grounding('ab2b20db-602e-4d87-af96-772e3d35fedc', doctrinal_rupture_necessary, deontological).
narrative_ontology:cs_axiom('ab2b20db-602e-4d87-af96-772e3d35fedc', foundational, ongoing_development_authorized_by_conciliar_intent).
narrative_ontology:cs_axiom_status(ongoing_development_authorized_by_conciliar_intent, holdable).
narrative_ontology:cs_axiom_grounding('ab2b20db-602e-4d87-af96-772e3d35fedc', ongoing_development_authorized_by_conciliar_intent, instrumental).
narrative_ontology:cs_axiom('ab2b20db-602e-4d87-af96-772e3d35fedc', secondary, faithful_reception_constitutes_doctrinal_meaning).
narrative_ontology:cs_axiom_status(faithful_reception_constitutes_doctrinal_meaning, holdable).
narrative_ontology:cs_axiom_grounding('ab2b20db-602e-4d87-af96-772e3d35fedc', faithful_reception_constitutes_doctrinal_meaning, conventional).
narrative_ontology:cs_reference_frame('ab2b20db-602e-4d87-af96-772e3d35fedc', pre_conciliar_doctrinal_rigidity_requiring_break).
narrative_ontology:cs_drift_state('ab2b20db-602e-4d87-af96-772e3d35fedc', contemporary_institutional_entrenchment, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('ab2b20db-602e-4d87-af96-772e3d35fedc', '').
narrative_ontology:cs_kernel_id(vatican_ii_doctrinal_authority__rupture_progressive_reading, vatican_ii_doctrinal_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vatican_ii_doctrinal_authority__rupture_progressive_reading, progressive_reform_movements).
narrative_ontology:constraint_beneficiary(vatican_ii_doctrinal_authority__rupture_progressive_reading, ecumenical_dialogue_institutions).
narrative_ontology:constraint_victim(vatican_ii_doctrinal_authority__rupture_progressive_reading, traditionalist_clergy).
narrative_ontology:constraint_victim(vatican_ii_doctrinal_authority__rupture_progressive_reading, pre_conciliar_institutional_beneficiaries).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(vatican_ii_doctrinal_authority__rupture_progressive_reading, global_episcopacy).
narrative_ontology:constraint_victim(vatican_ii_doctrinal_authority__rupture_progressive_reading, global_episcopacy).
narrative_ontology:constraint_vindicates(vatican_ii_doctrinal_authority__rupture_progressive_reading, doctrinal_development_as_living_tradition).
narrative_ontology:constraint_vindicates(vatican_ii_doctrinal_authority__rupture_progressive_reading, interpretation_authority_vested_in_faithful_reception).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Theologians, bishops, and pastoral leaders who interpret Vatican II as authorizing substantive doctrinal change and ongoing institutional reform. They frame the Council as a necessary rupture with pre-conciliar rigidity, read ambiguous texts as intentional openings for development, and treat post-conciliar implementation (liturgical reform, religious freedom doctrine, ecumenical engagement) as authentic realization of conciliar intent. They benefit from the doctrinal authority the 'spirit of the Council' grants them to pursue reforms without requiring new formal conciliar votes. Their power sits in theological production, episcopal conferences, and pastoral networks; they can exit into schism or institutional separation if the progressive reading is formally repudiated, but they maintain significant structural influence through seminaries, diocesan governance, and international theological bodies.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__rupture_progressive_reading, progressive_reform_movements, beneficiary,
    organized, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(vatican_ii_doctrinal_authority__rupture_progressive_reading, progressive_reform_movements, agenda_setter).

% Priests, bishops, and religious communities who view the pre-conciliar liturgy, doctrinal formulations, and institutional structures as authoritative expressions of unchanging tradition. They bear the cost of being displaced from institutional authority and from the interpretation of what the Council means. Their exit options are severely constrained by religious identity fusion (their self-concept as priests, their vocational identity, their relationship to the institutional Church) and by the difficulty of maintaining parallel traditional structures outside the institutional Church. They experience the progressive reading as a delegitimization of their entire pastoral formation and understanding of Church teaching.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__rupture_progressive_reading, traditionalist_clergy, payer,
    moderate, biographical, identity_locked, global).

% International bodies engaged in dialogue with Protestant, Orthodox, and Anglican churches. The progressive reading's interpretation of Vatican II as authorizing doctrinal openness and reframing of Church-world relations (Gaudium et Spes, Unitatis Redintegratio) provides legitimacy for these dialogues as authentic expressions of conciliar intent. They benefit from the authority structure the reading grants; they are constrained in their ability to exit if the reading is withdrawn, as dialogue partnerships and institutional commitments have been built on the assumption that Vatican II genuinely opens the tradition to engagement with other Christian communities.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__rupture_progressive_reading, ecumenical_dialogue_institutions, beneficiary,
    institutional, generational, constrained, global).

% The papal doctrinal apparatus, curial offices, and hierarchical structures whose authority rested on pre-conciliar frameworks. The progressive reading, by treating pre-conciliar doctrine (the Syllabus of Errors, anti-modernist decrees, pre-conciliar ecclesiology) as superseded or authentically developed beyond recognition, removes the ground on which their interpretive monopoly stood. They bear the cost of reduced control over doctrinal interpretation and must continuously defend against the claim that their 'rigidity' is what the Council broke with. Their exit options are constrained by institutional identity and the need to maintain the appearance of conciliar fidelity.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__rupture_progressive_reading, pre_conciliar_institutional_beneficiaries, payer,
    institutional, generational, constrained, global).

% The Vatican's official teaching body (Congregation for the Doctrine of the Faith and related offices) must authoritatively interpret Vatican II texts, adjudicate between competing readings, and determine which implementations count as authentic realization of conciliar intent versus distortion. This gatekeeping role sits atop a structural ambiguity: the Council documents themselves contain textual ambiguities and apparent tensions between innovation (Dignitatis Humanae on religious freedom) and continuity claims. The Commission operates under pressure from both progressive and traditionalist seats and must maintain both doctrinal authority and credibility as a neutral arbiter.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__rupture_progressive_reading, vatican_doctrinal_commission, agenda_setter,
    institutional, generational, analytical, global).

% The collective of bishops worldwide, who are empowered by the progressive reading to interpret and implement Vatican II at the diocesan and regional level. They benefit from expanded pastoral authority and from the legitimacy granted by treating their reception of the Council as constitutive of its meaning. They also bear the cost of managing internal contradiction: official documents sometimes contain language supporting continuity readings, creating pastoral and doctrinal tensions when bishops attempt to implement the progressive reading's full implications (especially regarding birth control, clerical celibacy, and liturgical experimentation).
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__rupture_progressive_reading, global_episcopacy, beneficiary,
    organized, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(vatican_ii_doctrinal_authority__rupture_progressive_reading, global_episcopacy, payer).

% Traditional Latin Mass communities, Lefebvrist groups, and other traditionalist organizations that explicitly reject the progressive reading and the authority structure it grants. They are structurally excluded from formal influence on official doctrine; their dissent is acknowledged but not accommodated in mainstream institutional authority. They would argue for a continuity or rupture-traditionalist reading that vindicates pre-conciliar doctrine, but the institutional gates close against them.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__rupture_progressive_reading, traditionalist_communities, excluded,
    organized, generational, trapped, global).

% Academic and ecclesiastical historians who study Vatican II's actual textual production, the debates on the Council floor, and the historical record of what drafters intended. They take a perch outside the normative claims of any reading and produce evidence (or dispute claims) about what the texts say and what the Council's historical actors understood themselves to be doing. Their analysis can support or undermine any reading's claim to textual fidelity, but they do not carry institutional authority to enforce an interpretation.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__rupture_progressive_reading, conciliar_theologians, observer,
    analytical, biographical, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(vatican_ii_doctrinal_authority__rupture_progressive_reading, progressive_reform_movements).
narrative_ontology:fixing_cost_class(vatican_ii_doctrinal_authority__rupture_progressive_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Resolves ambiguity about the Church's doctrinal relationship to modern secular societies and to other Christian traditions. Vatican II left textual gaps—most famously, how to reconcile religious freedom (Dignitatis Humanae) with pre-conciliar anti-liberalism. The progressive reading coordinates the faithful's theological expectation that the Church can authentically teach differently on these matters by treating Vatican II's textual ambiguities as intentional openings, not flaws. This coordination function permits ongoing doctrinal development without formal schism.
% TRANSFER_FUNCTION: Transfers interpretive authority from the pre-conciliar magisterium's monopoly on doctrine to a distributed authority structure: progressive bishops, theologians, and pastoral leaders gain standing to develop doctrine 'in the spirit of the Council,' beyond what the texts explicitly state. Traditionalist clergy and institutions structured around pre-conciliar doctrine lose institutional authority. The faithful's theological expectations shift: they are invited to read the Church as a living tradition in dialogue with modernity rather than defending a fixed deposit.
% ABSENT_VOICES: Traditionalist clergy and academically rigorous historical-critical scholars who would dispute that Vatican II texts actually authorize the breadth of reform claimed in the progressive reading. They are structurally excluded from the institutional interpretation process—their voice is acknowledged but not integrated into the authority structure that determines what the Council 'really meant.' Pre-conciliar theological minorities (who were outside the Council floor) have no presence. Non-Catholic Christian communities have advisory standing but not interpretive authority, despite being directly affected by the ecumenical theology the progressive reading instantiates.
% DISAPPEARANCE_RATIONALE: If the progressive reading vanished—if the Vatican officially declared that Vatican II represents continuity, not rupture, and that the 'spirit of the Council' does not authorize ongoing doctrinal development—the global Catholic Church would undergo profound institutional reorganization. Progressive bishops and theologians would face a legitimacy crisis and likely schism (formal or de facto). Traditionalist communities would gain institutional validation. Ecumenical dialogues would be reframed or abandoned. Dozens of post-conciliar developments (liturgical reform, religious freedom doctrine, the legitimacy of episcopal conferences as teaching bodies) would be placed under doctrinal suspicion. The Church's institutional coherence depends heavily on the acceptance of some reading of Vatican II as authoritative; which reading is accepted shapes the entire pastoral and doctrinal structure.
% FOUNDING_PROBLEM: Pre-conciliar Catholicism had become institutionally rigid: doctrine was transmitted through centralized pronouncements, the liturgy was understood as unchangeable, relations with modernity were combative rather than dialogical, and relations with other Christian traditions were one-way proclamation rather than mutual recognition. This rigidity was perceived as a strategic liability in a world of rapid social change and increasing religious pluralism. Vatican II was convened to address this rigidity—to enable the Church to maintain doctrinal substance while engaging authentically with modern societies and other Christian traditions.
% FOUNDING_PROBLEM_CORROBORATION: Pope John XXIII's opening address to the Council ('opening the windows of the Church') and the historical record of pre-conciliar debates (particularly the conflict between the Curia and progressive bishops over liturgical reform and ecumenism) confirm that some participants experienced pre-conciliar structures as rigid. However, traditionalist historians and some contemporary Church historians dispute whether the rigidity was dysfunctional or whether the Council's solutions actually addressed the stated problem rather than replacing one set of principles with another. Conservative bishops present at the Council testify that they understood themselves as preserving doctrine while modernizing its expression—a reading that supports continuity, not rupture. The founding problem is live—the tension between stability and responsiveness—but contested in how it was solved.
narrative_ontology:disappearance_verdict(vatican_ii_doctrinal_authority__rupture_progressive_reading, world_rearranges).
narrative_ontology:founding_problem_status(vatican_ii_doctrinal_authority__rupture_progressive_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vatican_ii_doctrinal_authority__rupture_progressive_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(vatican_ii_doctrinal_authority__rupture_progressive_reading, 'none', 1).
narrative_ontology:epsilon_provenance(vatican_ii_doctrinal_authority__rupture_progressive_reading, 0.62, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(vatican_ii_doctrinal_authority__rupture_progressive_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(vatican_ii_doctrinal_authority__rupture_progressive_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(vatican_ii_doctrinal_authority__rupture_progressive_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62 at interval end) is moderate-high because the progressive reading concentrates interpretive authority in a distributed but still gate-kept set of actors (progressive bishops, theologians, episcopal conferences) who can develop doctrine without new conciliar votes or explicit papal authorization. The founding constraint-beneficiaries (pre-conciliar institutional structures) lose authority and control. However, extractiveness is not as high as a snare because the coordination function is genuinely real: Vatican II did leave ambiguities about religious freedom, liturgical reform, and ecumenical theology that require interpretation. The reading provides a framework for the faithful to understand that the Church can teach differently on these matters without losing its identity. Suppression (0.41, moderate) reflects that traditionalist clergy are not physically coerced into silence; they can speak and do maintain parallel structures (the Lefebvrist seminary, traditional Latin Mass communities). However, they are institutionally suppressed: their interpretation of Vatican II is officially rejected, their preferred liturgy is restricted, and their doctrinal framework is treated as residual rather than authoritative. The measurement series runs over 60 years (roughly 1962 to 2022). Extractiveness rises from 0.38 (immediately post-Council, when the outcome was ambiguous) to 0.62 (contemporary, as the progressive reading consolidated institutional power through episcopal conferences and theology faculties) and then plateaus (suggesting the reading has reached stable institutional equilibrium, not ongoing expansion). Theater ratio rises from 0.22 to 0.38 because, over time, the progressive reading increasingly performs doctrinal continuity while actually enacting discontinuity—Vatican II is invoked as license for changes that go far beyond what the texts state. Suppression requirement rises from 0.28 to 0.41 because maintaining the progressive reading's authority required progressively blocking traditionalist alternative interpretations from institutional platforms (exclusion of traditionalist theologians from episcopal conferences, removal of traditional Latin Mass priests from parishes, suppression of traditionalist seminaries).
 *
 * PERSPECTIVAL GAP:
 *   The progressive beneficiary seats and the traditionalist payer seats should compute different types. From the beneficiary position, the constraint is genuine coordination: Vatican II raised hard questions about doctrinal development and the Church's relationship to modernity, and the progressive reading provides a coherent framework for answering them. Doctrinal authority is not extracted but distributed to those best positioned to interpret the Council's intent and apply it pastorally. From the traditionalist payer position, the constraint is asymmetric extraction: the progressive reading uses Vatican II's ambiguities as cover to overturn pre-conciliar doctrine and to exclude traditionalist clergy from authority. The same texts are being read—the constraint is the same—but the directionality is inverted. The engine will compute these differently from the structural data: progressive beneficiaries have high institutional power, generational time horizon, and mobile exit (they can deploy their interpretation widely and leave if it is repudiated), yielding low directionality (d near beneficiary end, low χ). Traditionalist payers have moderate individual power, biographical time horizon, and identity-locked exit (their priestly identity and pastoral vocation are fused to pre-conciliar theology), yielding high directionality (d near target end, high χ). The Vatican Doctrinal Commission sits as agenda-setter with high institutional power and analytical standing but also structural constraint: it must maintain institutional coherence across incompatible readings.
 *
 * DIRECTIONALITY LOGIC:
 *   Progressive reform movements occupy the beneficiary seat with institutional power and analytical standing. They have designed and promoted the interpretation that vindicates their theological vision. They benefit from the distributed authority the reading grants while maintaining enough institutional gatekeeping (through episcopal conferences, pontifical universities, curial advisory roles) to exclude competing readings from mainstream platforms. Their exit options are strong: they can leave for academic positions, interfaith organizations, or schismatic communities. Traditionalist clergy occupy the payer seat with moderate individual power and identity-locked exit. They bear the cost of institutional marginalization, loss of authority over doctrine and liturgy, and the pain of seeing their entire formation and self-concept treated as residual. Their exit options are severely constrained by religious identity fusion: leaving the priesthood means abandoning a vocational identity that is core to their self-concept. A few traditionalist bishops and priests have left for schismatic organizations (Lefebvrist and related groups), but the exit is partial and costly. The Vatican Doctrinal Commission is the structural gate-keeper: it adjudicates official doctrine and can formally endorse or repudiate any reading. Its directionalit is near symmetric (d ≈ 0.5) because it carries both beneficiary and payer functions—it legitimizes progressive reforms (beneficiary) but also must maintain traditionalist participation to avoid institutional schism (payer cost). However, the empirical record shows the Commission has moved over 60 years toward endorsing the progressive reading, making its practical directionality trend toward the beneficiary end. This asymmetry is not authored directly but emerges from the composition of the Commission's advisory bodies and from successive papal appointments.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (pre-conciliar rigidity as a strategic liability) is LIVE and CONTESTED. The constraint prevents mandatrophy by continuously performing the solution: doctrinal development is still framed as flowing from Vatican II's intent, and debates over Church teaching still invoke the Council's authority. However, there is a secondary mandatrophy risk: if the progressive reading's central claim—that Vatican II authorizes ongoing reform—becomes institutionally unmoored, the constraint could flip from tangled_rope to piton. This could happen if (1) a future Pope formally repudiates the 'spirit of the Council' framing and returns to a strict textual reading (as Pope John Paul II partially did with respect to liturgical reform), or (2) the progressive reforms produce outcomes (doctrinal confusion, pastoral fragmentation, statistical decline in religious practice) that trigger institutional nostalgia for pre-conciliar clarity. In the present moment, the progressive reading maintains mandatrophy by continuing to frame the opening of the Church as ongoing work—Vatican II did not finish; it opened a trajectory. The constraint prevents the revelation that Vatican II might have simply replaced one set of institutional problems with another (pre-conciliar rigidity → post-conciliar ambiguity and fragmentation).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    textual_ambiguity_intentionality,
    'Are Vatican II''s textual ambiguities (e.g., on religious freedom vs. pre-conciliar anti-liberalism, or on liturgical reform authorization) products of genuine theological openness, historical accident/compromise between conservative and progressive blocs, or deliberate obscurity allowing all parties to claim victory?',
    'Historical analysis of conciliar floor debates, examination of textual redaction history, and interviews with surviving Council participants and drafters (papal theologians, periti, bishops). Archival study of the Council''s working papers and rejected alternatives.',
    'If ambiguity is intentional, the progressive reading''s claim that the texts authorize ongoing development gains structural support. If ambiguity is accidental or a compromise between incompatible positions, the progressive reading''s authority is weakened—it is reading permission into accident, not discovering authorial intent. If ambiguity is deliberate obscurity, all readings are equally valid hermeneutically, and the question becomes pure institutional power: whose interpretation wins.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(textual_ambiguity_intentionality, empirical, 'Whether Vatican II''s textual ambiguities reflect intentional theological openness or institutional compromise.').

omega_variable(
    spirit_vs_letter_boundary,
    'Where is the boundary between authentic realization of Vatican II''s ''spirit'' and distortion? How much post-conciliar change (liturgical reform, clerical celibacy debate, contraception teachings revisited) remains within the Council''s intended trajectory versus departing into unauthorized heterodoxy?',
    'The question is conceptually unresolvable within the framework of the progressive reading itself, because the reading grants authority to interpreters to define the boundary. However, empirical signals include: (1) whether the faithful experience post-conciliar changes as continuous with their pre-conciliar expectations (if massive cognitive dissonance, possibly over-extension); (2) whether the outcomes of reforms match the stated goals (if Mass attendance collapsed while spiritual renewal was promised, the reform''s legitimacy weakens); (3) whether subsequent councils or popes reaffirm or restrict the progressive reading''s scope.',
    'If the boundary can be clarified (through empirical outcome measurement and subsequent formal magisterium pronouncements), the progressive reading''s authority can be either vindicated or constrained. If the boundary remains permanently contested, the constraint perpetuates ambiguity at its core: reform is always potentially authorized but never fully settled. This perpetual openness could be a coordination feature (enabling response to changing pastoral needs) or an extraction feature (enabling power grabs dressed in theological language).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(spirit_vs_letter_boundary, conceptual, 'The hermeneutical instability of ''spirit of the Council'' as a basis for doctrinal authority.').

omega_variable(
    identity_lock_durability,
    'How much of traditionalist clergy''s resistance to the progressive reading stems from genuine theological conviction (they believe pre-conciliar doctrine is correct) versus identity fusion with pre-conciliar structures (their priestly self-concept is constituted through pre-conciliar liturgy and doctrine, so the reading feels like erasure of their identity)?',
    'Longitudinal study of traditionalist priests: (1) do those who remain in the institutional Church gradually accept the progressive reading as they age and adapt to post-conciliar practice (suggesting identity lock is loose), or do they maintain resistance across decades (suggesting deep conviction or tight lock)? (2) Do defectors to schismatic traditional communities cite doctrinal reasons or identity-preservation reasons? (3) Do interviews and confessional testimonies reveal guilt/shame about deviation from progressive norms (internalization of suppression) or explicit rejection of progressive legitimacy (resistance to suppression)? (4) Post-exit trajectories: do traditionalists who leave the priesthood report psychological relief or continued identity distress?',
    'If identity lock is the primary mechanism, the suppression measured (0.41) understates the actual constraint on traditionalist clergy. Their experience includes not just external suppression but internalized suppression: they carry the progressive reading''s rejection of their formation with them even after exit. The constraint''s type for traditionalist seats would shift toward snare. If genuine conviction is primary, the suppression is structural but not internalized; resistance remains high and the constraint remains tangled_rope or snare depending on how much extraction occurs.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_durability, empirical, 'Whether traditionalist clergy resistance is grounded in theological conviction or identity fusion with pre-conciliar structures.').

omega_variable(
    authority_structure_foreclosure,
    'Does the progressive reading''s establishment of distributed interpretive authority in episcopal conferences and progressive theologians foreclose the possibility that a future formal Magisterium pronouncement could reverse the reading, or is reversal always formally possible?',
    'Legal/institutional analysis: Can a future Pope formally repudiate the progressive reading and re-establish textual literalism? Yes, institutionally. However, does the cultural and theological entrenchment of the progressive reading make reversal practically impossible—even if formally possible? If reversals has been attempted (Pope John Paul II''s attempts to restrict liturgical experimentation, his defense of pre-conciliar doctrine against progressive reinterpretation), do the reversals stick or do they leak out of institutional enforcement?',
    'If reversal is possible and can stick, the progressive reading is contingent authority, and the constraint remains tangled_rope (extraction can be reversed). If reversal is formally possible but practically unenforceable (the reading has become so institutionalized that even papal pronouncements cannot reverse it), the constraint has shifted toward snare: the beneficiaries have extracted authority in a way that cannot be taken back through normal institutional processes. This would be a reading-level form of institutional calcification—the progressive reading has become the ''new normal'' from which deviation is unthinkable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(authority_structure_foreclosure, empirical, 'Whether the progressive reading''s institutional entrenchment creates practical foreclosure on reversal by the formal Magisterium.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vatican_ii_doctrinal_authority__rupture_progressive_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vati_tr_t0, vatican_ii_doctrinal_authority__rupture_progressive_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement_basis(vati_tr_t0, observed).
narrative_ontology:measurement(vati_tr_t10, vatican_ii_doctrinal_authority__rupture_progressive_reading, theater_ratio, 10, 0.28).
narrative_ontology:measurement_basis(vati_tr_t10, observed).
narrative_ontology:measurement(vati_tr_t20, vatican_ii_doctrinal_authority__rupture_progressive_reading, theater_ratio, 20, 0.33).
narrative_ontology:measurement_basis(vati_tr_t20, observed).
narrative_ontology:measurement(vati_tr_t30, vatican_ii_doctrinal_authority__rupture_progressive_reading, theater_ratio, 30, 0.37).
narrative_ontology:measurement_basis(vati_tr_t30, observed).
narrative_ontology:measurement(vati_tr_t40, vatican_ii_doctrinal_authority__rupture_progressive_reading, theater_ratio, 40, 0.38).
narrative_ontology:measurement_basis(vati_tr_t40, projected).
narrative_ontology:measurement(vati_tr_t50, vatican_ii_doctrinal_authority__rupture_progressive_reading, theater_ratio, 50, 0.38).
narrative_ontology:measurement_basis(vati_tr_t50, projected).
narrative_ontology:measurement(vati_tr_t60, vatican_ii_doctrinal_authority__rupture_progressive_reading, theater_ratio, 60, 0.38).
narrative_ontology:measurement_basis(vati_tr_t60, projected).

% Extraction over time
narrative_ontology:measurement(vati_be_t0, vatican_ii_doctrinal_authority__rupture_progressive_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement_basis(vati_be_t0, observed).
narrative_ontology:measurement(vati_be_t10, vatican_ii_doctrinal_authority__rupture_progressive_reading, base_extractiveness, 10, 0.48).
narrative_ontology:measurement_basis(vati_be_t10, observed).
narrative_ontology:measurement(vati_be_t20, vatican_ii_doctrinal_authority__rupture_progressive_reading, base_extractiveness, 20, 0.56).
narrative_ontology:measurement_basis(vati_be_t20, observed).
narrative_ontology:measurement(vati_be_t30, vatican_ii_doctrinal_authority__rupture_progressive_reading, base_extractiveness, 30, 0.6).
narrative_ontology:measurement_basis(vati_be_t30, observed).
narrative_ontology:measurement(vati_be_t40, vatican_ii_doctrinal_authority__rupture_progressive_reading, base_extractiveness, 40, 0.62).
narrative_ontology:measurement_basis(vati_be_t40, projected).
narrative_ontology:measurement(vati_be_t50, vatican_ii_doctrinal_authority__rupture_progressive_reading, base_extractiveness, 50, 0.62).
narrative_ontology:measurement_basis(vati_be_t50, projected).
narrative_ontology:measurement(vati_be_t60, vatican_ii_doctrinal_authority__rupture_progressive_reading, base_extractiveness, 60, 0.62).
narrative_ontology:measurement_basis(vati_be_t60, projected).

% Suppression requirement over time
narrative_ontology:measurement(vati_su_t0, vatican_ii_doctrinal_authority__rupture_progressive_reading, suppression_requirement, 0, 0.28).
narrative_ontology:measurement_basis(vati_su_t0, observed).
narrative_ontology:measurement(vati_su_t10, vatican_ii_doctrinal_authority__rupture_progressive_reading, suppression_requirement, 10, 0.33).
narrative_ontology:measurement_basis(vati_su_t10, observed).
narrative_ontology:measurement(vati_su_t20, vatican_ii_doctrinal_authority__rupture_progressive_reading, suppression_requirement, 20, 0.37).
narrative_ontology:measurement_basis(vati_su_t20, observed).
narrative_ontology:measurement(vati_su_t30, vatican_ii_doctrinal_authority__rupture_progressive_reading, suppression_requirement, 30, 0.4).
narrative_ontology:measurement_basis(vati_su_t30, observed).
narrative_ontology:measurement(vati_su_t40, vatican_ii_doctrinal_authority__rupture_progressive_reading, suppression_requirement, 40, 0.41).
narrative_ontology:measurement_basis(vati_su_t40, projected).
narrative_ontology:measurement(vati_su_t50, vatican_ii_doctrinal_authority__rupture_progressive_reading, suppression_requirement, 50, 0.41).
narrative_ontology:measurement_basis(vati_su_t50, projected).
narrative_ontology:measurement(vati_su_t60, vatican_ii_doctrinal_authority__rupture_progressive_reading, suppression_requirement, 60, 0.41).
narrative_ontology:measurement_basis(vati_su_t60, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vatican_ii_doctrinal_authority__rupture_progressive_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(vatican_ii_doctrinal_authority__rupture_progressive_reading, 0.12).
narrative_ontology:affects_constraint(vatican_ii_doctrinal_authority__rupture_progressive_reading, vatican_ii_doctrinal_authority__continuity_reading).
narrative_ontology:affects_constraint(vatican_ii_doctrinal_authority__rupture_progressive_reading, vatican_ii_doctrinal_authority__rupture_traditionalist_reading).
narrative_ontology:affects_constraint(vatican_ii_doctrinal_authority__rupture_progressive_reading, vatican_ii_doctrinal_authority__composite_overdetermination_reading).

% DUAL FORMULATION NOTE:
% The vatican_ii_doctrinal_authority kernel instantiates four structurally distinct constraints, one per reading. Each reading produces different beneficiary/victim structures, different authority distributions, and different ε values. The rupture_progressive_reading (this story) treats Vatican II as enabling doctrinal change and grants authority to ongoing development. Sibling readings decompose as follows: (1) continuity_reading: Vatican II as organic development, centralized authority retained, ε near 0.25 (low extraction because no authority shift); (2) rupture_traditionalist_reading: Vatican II as containing errors, traditionalist authority vindicated, ε near 0.70 (higher extraction because it invalidates post-conciliar changes); (3) composite_overdetermination_reading: Vatican II as multiple distinct changes (liturgical, ecumenical, ecclesiological, political) packaged as unified, ε near 0.55 (moderate extraction because it shows the coherence is performative). Each reading is a different constraint with different stakeholder roles and different directionalities. Network edges link them to show that the readings are in competition for institutional authority: if one reading consolidates power, it constrains the others' ability to influence doctrine.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(vatican_ii_doctrinal_authority__rupture_progressive_reading, moderate, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
