% ============================================================================
% CONSTRAINT STORY: vatican_ii_doctrinal_authority__composite_overdetermination_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_vatican_ii_doctrinal_authority__composite_overdetermination_reading, []).

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
 *   constraint_id: vatican_ii_doctrinal_authority__composite_overdetermination_reading
 *   human_readable: Vatican II Doctrinal Authority: Composite Overdetermination Reading
 *   domain: ecclesiastical/institutional/hermeneutical
 *
 * SUMMARY:
 *   Vatican II (1962–1965) was presented as a unified aggiornamento
 *   (updating) of Catholic doctrine and practice. This reading rejects that
 *   framing. It argues Vatican II is not one constraint but the simultaneous
 *   instantiation of multiple structurally distinct constraints, each with
 *   its own beneficiaries, victims, extractiveness profile, and degree of
 *   rupture/continuity: (1) Liturgical authority transfer (Latin to
 *   vernacular, priest-facing change, simplified rubrics) extracted from
 *   preconciliar religious communities, benefited liturgical reformers and
 *   lay participation rhetoric. Extractiveness: high in lived experience,
 *   moderate in text. (2) Ecumenical opening (Unitatis Redintegratio, Nostra
 *   Aetate) transferred legitimacy to interfaith dialogue at cost to
 *   exclusivist Catholic identity claims. Extractiveness: moderate. (3)
 *   Religious freedom doctrine (Dignitatis Humanae) benefited
 *   democratic-context Catholics, extracted from older natural-law theology.
 *   Extractiveness: high in doctrine, low in immediate practice impact. (4)
 *   Ecclesiological collegiality (episcopal conference authority, distributed
 *   doctrinal voice) transferred power from Roman Curia to national
 *   hierarchies. Extractiveness: moderate, with enduring contestation. The
 *   'single reform' framing masks that each component exhibits different
 *   rates of change, different enforcement machinery, different victim
 *   cohorts. The continuity/rupture debate presupposes single-axis
 *   measurement; this reading claims the axis itself is the false
 *   move—different components are genuinely different, and the unified
 *   packaging is itself an extractive move (it obscures differentiation and
 *   permits repackaging of radical change as organic development).
 *
 * KEY AGENTS:
 *   - Conciliar progressives: Vatican II's drafting coalition, mostly European bishops and curial reformers (Cardinal Bea, Archbishop Montini, theologian Rahner); shaped documents toward maximal openness and innovation.
 *   - Traditionalist communities: SSPX, sede vacantists, preconciliar-rite Catholics who read Vatican II as doctrinal rupture and institutional illegitimacy; excluded from authorized interpretation.
 *   - Preconciliar religious orders: Benedictines, Dominicans, Franciscans whose charism and practice were rooted in preconciliar liturgy and theology; identity-locked into the old framework.
 *   - Reformist episcopal conferences: National hierarchies (France, Germany, Netherlands, later US) that leveraged Vatican II's collegial openness to implement reforms far beyond textual limits.
 *   - Roman Curia: Vatican bureaucracy executing Vatican II but losing interpretive monopoly; maintaining enforcement capacity through silencing and intervention.
 *   - Postconciliar theologians: Rahner, Schillebeeckx school; gained interpretive authority through Vatican II's pluralism but remained subject to Curia enforcement.
 *   - Vatican II Council texts: The 16 documents, ambiguous by design (products of compromise), function as the constraint's legitimacy anchor.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vatican_ii_doctrinal_authority__composite_overdetermination_reading, 0.58).
domain_priors:suppression_score(vatican_ii_doctrinal_authority__composite_overdetermination_reading, 0.42).
domain_priors:theater_ratio(vatican_ii_doctrinal_authority__composite_overdetermination_reading, 0.51).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__composite_overdetermination_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__composite_overdetermination_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__composite_overdetermination_reading, theater_ratio, 0.51).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__composite_overdetermination_reading, accessibility_collapse, 0.67).
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__composite_overdetermination_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vatican_ii_doctrinal_authority__composite_overdetermination_reading, tangled_rope).
narrative_ontology:human_readable(vatican_ii_doctrinal_authority__composite_overdetermination_reading, "Vatican II Doctrinal Authority: Composite Overdetermination Reading").
narrative_ontology:topic_domain(vatican_ii_doctrinal_authority__composite_overdetermination_reading, "ecclesiastical/institutional/hermeneutical").

domain_priors:requires_active_enforcement(vatican_ii_doctrinal_authority__composite_overdetermination_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vatican_ii_doctrinal_authority__composite_overdetermination_reading, '65e866d2-42f5-4f38-bcb5-52730ce9b782').
narrative_ontology:cs_kernel_codification('65e866d2-42f5-4f38-bcb5-52730ce9b782', fixed_text).
narrative_ontology:cs_authority_grounding('65e866d2-42f5-4f38-bcb5-52730ce9b782', extraction).
narrative_ontology:cs_interpretation_layer_present('65e866d2-42f5-4f38-bcb5-52730ce9b782').
narrative_ontology:cs_reading_relation('65e866d2-42f5-4f38-bcb5-52730ce9b782', vatican_ii_doctrinal_authority__continuity_reading, influences).
narrative_ontology:cs_reading_relation('65e866d2-42f5-4f38-bcb5-52730ce9b782', vatican_ii_doctrinal_authority__rupture_progressive_reading, coexists_with).
narrative_ontology:cs_reading_relation('65e866d2-42f5-4f38-bcb5-52730ce9b782', vatican_ii_doctrinal_authority__rupture_traditionalist_reading, coexists_with).
narrative_ontology:cs_axiom('65e866d2-42f5-4f38-bcb5-52730ce9b782', foundational, ecclesiastical_authority_is_pluridimensional_not_unidimensional).
narrative_ontology:cs_axiom_status(ecclesiastical_authority_is_pluridimensional_not_unidimensional, holdable).
narrative_ontology:cs_axiom_grounding('65e866d2-42f5-4f38-bcb5-52730ce9b782', ecclesiastical_authority_is_pluridimensional_not_unidimensional, empirically_contingent).
narrative_ontology:cs_axiom('65e866d2-42f5-4f38-bcb5-52730ce9b782', foundational, component_constraints_have_independent_extractiveness).
narrative_ontology:cs_axiom_status(component_constraints_have_independent_extractiveness, holdable).
narrative_ontology:cs_axiom_grounding('65e866d2-42f5-4f38-bcb5-52730ce9b782', component_constraints_have_independent_extractiveness, empirically_contingent).
narrative_ontology:cs_reference_frame('65e866d2-42f5-4f38-bcb5-52730ce9b782', unified_unambiguous_tradition).
narrative_ontology:cs_drift_state('65e866d2-42f5-4f38-bcb5-52730ce9b782', contemporary_postconciliar_pluralism, gap(codification_collapse, substantial, false)).
narrative_ontology:cs_created_at('65e866d2-42f5-4f38-bcb5-52730ce9b782', '').
narrative_ontology:cs_kernel_id(vatican_ii_doctrinal_authority__composite_overdetermination_reading, vatican_ii_doctrinal_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vatican_ii_doctrinal_authority__composite_overdetermination_reading, conciliar_progressives).
narrative_ontology:constraint_beneficiary(vatican_ii_doctrinal_authority__composite_overdetermination_reading, ecumenical_modernizers).
narrative_ontology:constraint_beneficiary(vatican_ii_doctrinal_authority__composite_overdetermination_reading, reformist_episcopal_conferences).
narrative_ontology:constraint_victim(vatican_ii_doctrinal_authority__composite_overdetermination_reading, traditionalist_communities).
narrative_ontology:constraint_victim(vatican_ii_doctrinal_authority__composite_overdetermination_reading, preconciliar_religious_orders).
narrative_ontology:constraint_victim(vatican_ii_doctrinal_authority__composite_overdetermination_reading, indigenous_liturgical_practices).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(vatican_ii_doctrinal_authority__composite_overdetermination_reading, postconciliar_theologians).
narrative_ontology:constraint_victim(vatican_ii_doctrinal_authority__composite_overdetermination_reading, roman_curia).
narrative_ontology:constraint_vindicates(vatican_ii_doctrinal_authority__composite_overdetermination_reading, episcopal_collegiality_doctrine).
narrative_ontology:constraint_vindicates(vatican_ii_doctrinal_authority__composite_overdetermination_reading, religious_freedom_principle).
narrative_ontology:constraint_vindicates(vatican_ii_doctrinal_authority__composite_overdetermination_reading, dialogue_capacity_claim).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Bishops, theologians, and curial officials who shaped the Council's texts toward ecclesiological innovation and openness to world dialogue. They frame Vatican II as necessary modernization and claim authority to interpret its 'spirit' beyond the letter. They benefit from the resulting authority redistribution (episcopal collegiality), expanded legitimacy for interfaith engagement, and continued institutional prestige. Their exit would require explicit schism or retirement from office; instead, they maintain interpretive authority over the Council's meaning.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__composite_overdetermination_reading, conciliar_progressives, agenda_setter,
    institutional, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(vatican_ii_doctrinal_authority__composite_overdetermination_reading, conciliar_progressives, beneficiary).

% Catholic traditionalists (SSPX-aligned, sede vacantist, preconciliar-rite communities) who read Vatican II as rupture from immutable doctrine. They bear the cost of systematic institutional marginalization: their sacramental practice is deemed illicit, their episcopal authority unrecognized, their schools and seminaries under persistent pressure. Formal schism is their only clean exit, but schism means abandoning the claim to represent the true Church. Their constrained position is the enforcement object: they can practice quietly or face exclusion.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__composite_overdetermination_reading, traditionalist_communities, payer,
    moderate, generational, constrained, global).

% National bishops' conferences in Europe, North America, and parts of Latin America that leveraged Vatican II's collegial openness to implement local adaptations (vernacular liturgy, married deacons, lay ministry) far beyond textual warrant. They benefit from increased autonomy from Roman oversight and legitimacy for responsive pastoral innovation. Their exit option is implicit: continued deference to Rome maintains their institutional position; significant resistance invites apostolic visits and intervention.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__composite_overdetermination_reading, reformist_episcopal_conferences, beneficiary,
    institutional, generational, mobile, national).

% Monastic and mendicant communities (Benedictines, Dominicans, Franciscans) whose charisms and practices were rooted in preconciliar ecclesiology and liturgy. Vatican II's liturgical reform (vernacular Mass, priest facing congregation, simplified rubrics) and push toward 'relevance' and 'dialogue with modernity' fundamentally altered their operational context. Their identity is fused with the preconciliar framework; exit would require spiritual dissolution or reformation of the order itself. They continue but with reduced institutional weight and growing internal tensions over fidelity to founding charism.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__composite_overdetermination_reading, preconciliar_religious_orders, payer,
    moderate, biographical, identity_locked, global).

% Pre-Christian and syncretic liturgical practices in mission territories that were tolerated or absorbed into preconciliar Catholic ritual. Vatican II's universalizing impulse, coupled with progressive episcopal conferences, eliminated many local-context liturgical adaptations in favor of Western-form vernacular Mass and standardized sacramental theology. Indigenous religious agents had no institutional voice in the Council and no exit except subordination or invisible practice. They bear the cost of cultural homogenization despite Vatican II's rhetoric of 'inculturatio.'
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__composite_overdetermination_reading, indigenous_liturgical_practices, payer,
    powerless, biographical, trapped, regional).
narrative_ontology:stakeholder_non_agent(vatican_ii_doctrinal_authority__composite_overdetermination_reading, indigenous_liturgical_practices).

% The Vatican bureaucracy (Secretariat of State, doctrinal agencies, liturgical officials) that implements and interprets conciliar documents. They function as the final adjudicator of what counts as legitimate Vatican II interpretation, yet the Council's collegial language and texts undercut their monopoly on doctrinal authority. They maintain enforcement capacity (can silence theologians, restrict sacramental practice, intervene in episcopal conferences) but must now justify interventions as defending the Council's authentic meaning rather than papal prerogative. They pay a cost in contested authority; they benefit from institutional continuity and residual enforcement power.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__composite_overdetermination_reading, roman_curia, agenda_setter,
    institutional, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(vatican_ii_doctrinal_authority__composite_overdetermination_reading, roman_curia, payer).

% Academic and pastoral theologians (Rahner, Schillebeeckx, later de Lubac school) who gained interpretive authority through Vatican II's opening. They frame themselves as Vatican II's legitimate voice, licensing theological innovation under the rubric of 'spirit of the Council.' Their exit option is implicit: continued theoretical obeisance to the Council maintains institutional credibility; sharp doctrinal deviation invites silencing (as with Küng, Schillebeeckx). They benefit from the Council's theological pluralism but remain subject to enforcement from the Roman Curia.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__composite_overdetermination_reading, postconciliar_theologians, beneficiary,
    powerful, biographical, mobile, global).

% The 16 conciliar documents themselves, treated as the fixed kernel of Vatican II's authority. All readings claim to honor the texts while advancing competing interpretations. The texts are ambiguous by design (products of compromise) and internally stratified (some documents more progressive, others more conservative). The texts function as the constraint's legitimacy anchor but their own underdetermination is the source of the constraint's extractive capacity.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__composite_overdetermination_reading, vatican_ii_council_texts, observer,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(vatican_ii_doctrinal_authority__composite_overdetermination_reading, vatican_ii_council_texts).

% Non-institutional Catholics in diaspora communities (Eastern European, African, Asian Catholic populations without organized institutional presence at the Council or after) who experienced Vatican II's changes as externally imposed shifts in ritual and doctrinal presentation without voice in the process. They could not shape the Council's agenda or interpretation; their cultural and linguistic needs were secondary to the universal standardization impulse. They remain subject to progressive episcopal conference decisions but lack organizational capacity to contest them.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__composite_overdetermination_reading, continental_catholicism, excluded,
    organized, generational, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(vatican_ii_doctrinal_authority__composite_overdetermination_reading, conciliar_progressives).
narrative_ontology:fixing_cost_class(vatican_ii_doctrinal_authority__composite_overdetermination_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Convenes global episcopacy to restate and reframe Catholic doctrine in response to 20th-century secularization, ecumenical opportunity, and institutional legitimacy crisis. Creates mechanisms (episcopal conferences, theological pluralism, engagement with modernity) for distributed doctrinal articulation beyond Roman monopoly. Addresses real coordination problem: how does a claims-to-be-universal institution maintain coherence and relevance when its preconciliar framing is increasingly uncompelling to educated adherents?
% TRANSFER_FUNCTION: Transfers interpretive authority from Roman Curia monolith to episcopal conferences and theological academy, creating space for doctrinal innovation under the banner of 'authentic Council interpretation.' Simultaneously transfers legitimacy from explicit doctrinal continuity claims to rhetorical continuity—permitting structural change in practice (liturgy, ecumenism, religious freedom doctrine) while maintaining verbal fidelity to tradition. The transfer is asymmetric: progressives gain interpretive license; traditionalists lose institutional protection.
% ABSENT_VOICES: Traditionalist bishops (Cardinal Lefebvre, Bishop de Castro Mayer) were present but marginalized; their interpretive stance—Vatican II as rupture requiring rejection—is locked out of legitimate conciliar interpretation. Indigenous Catholic communities and mission-territory bishops had minimal voice despite the Council's rhetoric about inculturation. The 'spirit of the Council' progressives invoke as interpretive authority were not textually mandated, silencing voices that claimed to read the text literally. Lay Catholics and women religious appear in documents but did not author them.
% DISAPPEARANCE_RATIONALE: Progressive reading: If Vatican II enforcement vanished, the Church would collapse backward into preconciliar rigidity and lose its capacity for moral dialogue with the modern world. Traditionalist reading: If Vatican II authority were rejected, the Church's doctrinal coherence and sacramental validity would be restored (traditionalists argue Vatican II created ambiguities that delegitimized postconciliar practice). The world would not directly 'rearrange'—Vatican II's influence on secular society is limited—but the Church's internal organizational principle would fundamentally shift. The disagreement is structural: what counts as 'rearrangement' depends on whether one reads Vatican II as constitutive of the Church's modern identity or as a contingent institutional choice.
% FOUNDING_PROBLEM: Pre-conciliar Catholic institutional authority was experienced as rigid, defensive, incomprehensible to modernity, unable to engage with Protestant/Orthodox ecumenism, and incapable of responding to living faith questions from educated lay Catholics. The Council convened to modernize the Church's pastoral presentation and doctrine without abandoning its truth claims—to speak the faith in contemporary language without changing the faith itself.
% FOUNDING_PROBLEM_CORROBORATION: Pre-conciliar bishops and theologians corroborate the founding problem: accounts from the 1950s pastoral crisis, seminary intellectual ferment, and ecumenical opportunity are widely documented. Traditionalists dispute whether the problem was real or a crisis of faith; they corroborate instead a preexisting problem—modernity's false promises and the Church's loss of nerve. Postconciliar historians (John O'Malley, Joseph Komonchak, drawing on conciliar archives outside beneficiary circles) document both the genuine institutional tension and the extent to which progressive interpretation outran textual warrant. Independent academic consensus: the founding problem was real and serious; the solution's effects are disputed.
narrative_ontology:disappearance_verdict(vatican_ii_doctrinal_authority__composite_overdetermination_reading, contested).
narrative_ontology:founding_problem_status(vatican_ii_doctrinal_authority__composite_overdetermination_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vatican_ii_doctrinal_authority__composite_overdetermination_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(vatican_ii_doctrinal_authority__composite_overdetermination_reading, 'none', 1).
narrative_ontology:epsilon_provenance(vatican_ii_doctrinal_authority__composite_overdetermination_reading, 0.58, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(vatican_ii_doctrinal_authority__composite_overdetermination_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(vatican_ii_doctrinal_authority__composite_overdetermination_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(vatican_ii_doctrinal_authority__composite_overdetermination_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate-high (0.58 at terminal point) because the constraint's operation redistributes institutional authority, doctrinal interpretive privilege, and legitimacy from traditionalist/Curia seats to progressive/collegial seats. The redistribution is justified by appeal to Council authority, but the Council itself is the arena where the redistribution happened—a mild circularity that this reading exposes. Suppression is moderate (0.42) because the constraint does not require violent coercion; it operates through administrative marginalization (silencing theologians, withholding faculties from SSPX priests, pressure on religious orders). Theater is high-moderate (0.51) because much postconciliar activity is performative—invoking the 'spirit of the Council' as license for changes the text does not clearly mandate; the theater serves to cloak extractive authority redistribution in the language of authentic interpretation. The measurement trajectory shows extraction rising steeply from 1962 (0.22) through 1995 (0.58), flattening and slightly declining by 2025 (0.58) as traditionalist institutional capacity is exhausted and the constraint reaches a quasi-stable configuration. Theater peaks at 2013 (0.54) during maximum hermeneutical contestation (Benedict XVI's traditionalist outreach, Francis's progressive pastoral reframing), then slightly declines as institutional positions calcify. Suppression rises sharply through 1978 (0.41) as progressive hegemony solidifies, then stabilizes around 0.42–0.44 as enforced silence becomes normalized.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats (traditionalist communities, preconciliar orders, indigenous practices) perceive extraction and marginalization; the beneficiary seats (progressives, reformist conferences, Curia with retained enforcement) perceive coordination and legitimate development. This is not a disagreement about facts but about the constraint's legitimacy and function. The constraint's persistence depends on the beneficiary reading being institutionally endorsed and the payer reading being marginalized—an asymmetry that is itself extractive.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality overrides: None required for initial analysis; the structural derivation from beneficiary/victim + exit is adequate. However, the Roman Curia's position is genuinely ambiguous—they are both beneficiary (retain enforcement capacity, set bounds on interpretation) and partial victim (lose monopoly on doctrinal authority, must justify interventions through Council language they did not author). The commentary captures this; the engine's directionality computation should flag the ambiguity.
 *
 * MANDATROPHY ANALYSIS:
 *   Vatican II was founded to solve a genuine coordination problem: the preconciliar Church's institutional rigidity, defensive posture, incomprehensibility to modernity, and inability to engage ecumenically or address living pastoral questions. That founding problem is contested but widely corroborated outside beneficiary circles (John O'Malley's institutional history, Vatican Archives research, conciliar diaries of moderate bishops). The problem was real. However, by 1995–2013, the founding problem's status became contested: traditionalists argue the Church lost its doctrinal nerve; progressives argue the problem remains live. The constraint's enforcement shifted: early enforcement (1968–1978) suppressed traditionalist resistance through administrative marginalization. By 2013, enforcement had become mostly theatrical—progressives invoke 'spirit of the Council' for changes without textual warrant, traditionalists invoke textual limit against progressive excess, the Curia maintains silent authority while allowing both to perform. This is mandatrophy: the founding problem (institutional crisis of credibility) is partially solved (the Church survived and expanded in some regions) but the solution's costs (marginalization of traditionalists, incoherence of doctrine) are now higher than the problem would have been (pre-conciliar Catholicism persists in SSPX and traditionalist communities, intact and growing in some cohorts). The constraint persists not because the founding problem demands it but because beneficiaries have institutional power to maintain enforcement and because traditionalists have no institutional path to replace it. This reading declares: Vatican II is a tangled_rope (genuine coordination function + asymmetric extraction) that has begun to exhibit piton characteristics (performance of unified reform masking internal decomposition, diminishing real function relative to enforced theater). The engine should detect this via the mandatrophy mismatch: founding_problem_status = contested + disappearance_verdict = contested + theater_ratio = 0.51 + base_extractiveness = 0.58 + measured suppression = 0.42. The mismatch flags that the constraint's extractive persistence does not track to a live founding problem.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    composite_vs_unified_framing,
    'Is Vatican II structurally composite (multiple independent constraints packaged as unified reform) or genuinely unified (a single coordinated shift across all dimensions)?',
    'Detailed comparative historiography of each component''s development, authorship, enforcement, and contestation. Trace liturgical reform documents separately from ecumenical ones, ecclesiological ones separately from religious freedom ones. If each component shows distinct authorship, distinct enforcement machinery, distinct victim/beneficiary cohorts, and distinct degrees of continuity/rupture, the composite hypothesis is supported.',
    'If composite: the continuity/rupture debate is a category error; different components are genuinely different. Vatican II cannot be fully ''understood'' via any single hermeneutic. If unified: a single reading (continuity or rupture) might capture the Council''s true logic.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(composite_vs_unified_framing, conceptual, 'Whether Vatican II is one constraint or multiple constraints falsely packaged as unified.').

omega_variable(
    extractiveness_by_component,
    'What is the base extractiveness (ε) of each Vatican II component measured independently: (1) liturgical authority transfer, (2) ecumenical opening, (3) religious freedom doctrine, (4) episcopal collegiality?',
    'Author separate constraint stories for each component. Measure each component''s extractiveness by its own beneficiary/victim structure, enforcement machinery, and degree of enforced change. Compare ε values across components.',
    'If component ε values diverge significantly (e.g., liturgical ε ≈ 0.75, ecumenical ε ≈ 0.42, religious freedom ε ≈ 0.68, collegiality ε ≈ 0.55), then Vatican II is genuinely composite and this reading''s claim is supported. If all components cluster around 0.58, the unified claim is supported.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(extractiveness_by_component, empirical, 'Decompose Vatican II into component constraints and measure each independently.').

omega_variable(
    mandatrophy_trajectory_ambiguity,
    'Is Vatican II currently in mandatrophy (founding problem solved/dead, but constraint persists due to institutional inertia and beneficiary power) or in active function (founding problem still live, enforcement justified)?',
    'Assess the founding problem''s current status: Is the preconciliar institutional crisis still live? Can traditionalist arguments credibly claim the old framework is restored and functional? What do postconciliar statistics show (institutional participation, doctrinal coherence, ecumenical fruit)? If the founding problem is substantially solved, Vatican II is mandatrophic. If still live, it remains active.',
    'Mandatrophy: the constraint should be reclassified toward piton or downgraded from active tangled_rope. Active: the constraint remains fully extractive but justified by coordination function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mandatrophy_trajectory_ambiguity, empirical, 'Whether Vatican II''s founding problem is live or dead; whether the constraint is mandatrophic.').

omega_variable(
    continuity_reading_coexistence,
    'Can the continuity_reading and the composite_overdetermination_reading coexist in a single institutional framework, or do they logically foreclose each other?',
    'Test coherence: Can one hold (a) Vatican II is organic development of unchanging tradition AND (b) different components have independent extractiveness and distinct continuity/rupture profiles? The continuity reading assumes a unitary tradition that is immutable; composite reading allows components to be differently extractive and differently continuous. If both can be true simultaneously under different interpretive regimes, they coexist. If they are logically contradictory (one party must be wrong), they foreclose.',
    'If coexist: both readings remain live options; the Council accommodates both. If foreclose: one reading must be rejected as incoherent (the continuity reading requires unified authority; composite reading requires pluridimensional authority).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(continuity_reading_coexistence, conceptual, 'Whether continuity and composite readings are logically compatible or foreclosing.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vatican_ii_doctrinal_authority__composite_overdetermination_reading, 1962, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vati_tr_t1962, vatican_ii_doctrinal_authority__composite_overdetermination_reading, theater_ratio, 1962, 0.12).
narrative_ontology:measurement_basis(vati_tr_t1962, observed).
narrative_ontology:measurement(vati_tr_t1968, vatican_ii_doctrinal_authority__composite_overdetermination_reading, theater_ratio, 1968, 0.28).
narrative_ontology:measurement_basis(vati_tr_t1968, observed).
narrative_ontology:measurement(vati_tr_t1978, vatican_ii_doctrinal_authority__composite_overdetermination_reading, theater_ratio, 1978, 0.42).
narrative_ontology:measurement_basis(vati_tr_t1978, observed).
narrative_ontology:measurement(vati_tr_t1995, vatican_ii_doctrinal_authority__composite_overdetermination_reading, theater_ratio, 1995, 0.48).
narrative_ontology:measurement_basis(vati_tr_t1995, observed).
narrative_ontology:measurement(vati_tr_t2013, vatican_ii_doctrinal_authority__composite_overdetermination_reading, theater_ratio, 2013, 0.54).
narrative_ontology:measurement_basis(vati_tr_t2013, observed).
narrative_ontology:measurement(vati_tr_t2025, vatican_ii_doctrinal_authority__composite_overdetermination_reading, theater_ratio, 2025, 0.51).
narrative_ontology:measurement_basis(vati_tr_t2025, observed).

% Extraction over time
narrative_ontology:measurement(vati_be_t1962, vatican_ii_doctrinal_authority__composite_overdetermination_reading, base_extractiveness, 1962, 0.22).
narrative_ontology:measurement_basis(vati_be_t1962, observed).
narrative_ontology:measurement(vati_be_t1968, vatican_ii_doctrinal_authority__composite_overdetermination_reading, base_extractiveness, 1968, 0.38).
narrative_ontology:measurement_basis(vati_be_t1968, observed).
narrative_ontology:measurement(vati_be_t1978, vatican_ii_doctrinal_authority__composite_overdetermination_reading, base_extractiveness, 1978, 0.52).
narrative_ontology:measurement_basis(vati_be_t1978, observed).
narrative_ontology:measurement(vati_be_t1995, vatican_ii_doctrinal_authority__composite_overdetermination_reading, base_extractiveness, 1995, 0.58).
narrative_ontology:measurement_basis(vati_be_t1995, observed).
narrative_ontology:measurement(vati_be_t2013, vatican_ii_doctrinal_authority__composite_overdetermination_reading, base_extractiveness, 2013, 0.61).
narrative_ontology:measurement_basis(vati_be_t2013, observed).
narrative_ontology:measurement(vati_be_t2025, vatican_ii_doctrinal_authority__composite_overdetermination_reading, base_extractiveness, 2025, 0.58).
narrative_ontology:measurement_basis(vati_be_t2025, observed).

% Suppression requirement over time
narrative_ontology:measurement(vati_su_t1962, vatican_ii_doctrinal_authority__composite_overdetermination_reading, suppression_requirement, 1962, 0.15).
narrative_ontology:measurement_basis(vati_su_t1962, observed).
narrative_ontology:measurement(vati_su_t1968, vatican_ii_doctrinal_authority__composite_overdetermination_reading, suppression_requirement, 1968, 0.32).
narrative_ontology:measurement_basis(vati_su_t1968, observed).
narrative_ontology:measurement(vati_su_t1978, vatican_ii_doctrinal_authority__composite_overdetermination_reading, suppression_requirement, 1978, 0.41).
narrative_ontology:measurement_basis(vati_su_t1978, observed).
narrative_ontology:measurement(vati_su_t1995, vatican_ii_doctrinal_authority__composite_overdetermination_reading, suppression_requirement, 1995, 0.44).
narrative_ontology:measurement_basis(vati_su_t1995, observed).
narrative_ontology:measurement(vati_su_t2013, vatican_ii_doctrinal_authority__composite_overdetermination_reading, suppression_requirement, 2013, 0.43).
narrative_ontology:measurement_basis(vati_su_t2013, observed).
narrative_ontology:measurement(vati_su_t2025, vatican_ii_doctrinal_authority__composite_overdetermination_reading, suppression_requirement, 2025, 0.42).
narrative_ontology:measurement_basis(vati_su_t2025, observed).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=1962, tn=2025
narrative_ontology:measurement(vati_grid_01, vatican_ii_doctrinal_authority__composite_overdetermination_reading, accessibility_collapse(class), 1962, 0.55).
narrative_ontology:measurement(vati_grid_02, vatican_ii_doctrinal_authority__composite_overdetermination_reading, accessibility_collapse(class), 2025, 0.68).
narrative_ontology:measurement(vati_grid_03, vatican_ii_doctrinal_authority__composite_overdetermination_reading, accessibility_collapse(individual), 1962, 0.35).
narrative_ontology:measurement(vati_grid_04, vatican_ii_doctrinal_authority__composite_overdetermination_reading, accessibility_collapse(individual), 2025, 0.78).
narrative_ontology:measurement(vati_grid_05, vatican_ii_doctrinal_authority__composite_overdetermination_reading, accessibility_collapse(organizational), 1962, 0.45).
narrative_ontology:measurement(vati_grid_06, vatican_ii_doctrinal_authority__composite_overdetermination_reading, accessibility_collapse(organizational), 2025, 0.71).
narrative_ontology:measurement(vati_grid_07, vatican_ii_doctrinal_authority__composite_overdetermination_reading, accessibility_collapse(structural), 1962, 0.62).
narrative_ontology:measurement(vati_grid_08, vatican_ii_doctrinal_authority__composite_overdetermination_reading, accessibility_collapse(structural), 2025, 0.63).
narrative_ontology:measurement(vati_grid_09, vatican_ii_doctrinal_authority__composite_overdetermination_reading, resistance(class), 1962, 0.42).
narrative_ontology:measurement(vati_grid_10, vatican_ii_doctrinal_authority__composite_overdetermination_reading, resistance(class), 2025, 0.68).
narrative_ontology:measurement(vati_grid_11, vatican_ii_doctrinal_authority__composite_overdetermination_reading, resistance(individual), 1962, 0.22).
narrative_ontology:measurement(vati_grid_12, vatican_ii_doctrinal_authority__composite_overdetermination_reading, resistance(individual), 2025, 0.75).
narrative_ontology:measurement(vati_grid_13, vatican_ii_doctrinal_authority__composite_overdetermination_reading, resistance(organizational), 1962, 0.35).
narrative_ontology:measurement(vati_grid_14, vatican_ii_doctrinal_authority__composite_overdetermination_reading, resistance(organizational), 2025, 0.72).
narrative_ontology:measurement(vati_grid_15, vatican_ii_doctrinal_authority__composite_overdetermination_reading, resistance(structural), 1962, 0.48).
narrative_ontology:measurement(vati_grid_16, vatican_ii_doctrinal_authority__composite_overdetermination_reading, resistance(structural), 2025, 0.71).
narrative_ontology:measurement(vati_grid_17, vatican_ii_doctrinal_authority__composite_overdetermination_reading, stakes_inflation(class), 1962, 0.42).
narrative_ontology:measurement(vati_grid_18, vatican_ii_doctrinal_authority__composite_overdetermination_reading, stakes_inflation(class), 2025, 0.62).
narrative_ontology:measurement(vati_grid_19, vatican_ii_doctrinal_authority__composite_overdetermination_reading, stakes_inflation(individual), 1962, 0.25).
narrative_ontology:measurement(vati_grid_20, vatican_ii_doctrinal_authority__composite_overdetermination_reading, stakes_inflation(individual), 2025, 0.71).
narrative_ontology:measurement(vati_grid_21, vatican_ii_doctrinal_authority__composite_overdetermination_reading, stakes_inflation(organizational), 1962, 0.38).
narrative_ontology:measurement(vati_grid_22, vatican_ii_doctrinal_authority__composite_overdetermination_reading, stakes_inflation(organizational), 2025, 0.65).
narrative_ontology:measurement(vati_grid_23, vatican_ii_doctrinal_authority__composite_overdetermination_reading, stakes_inflation(structural), 1962, 0.48).
narrative_ontology:measurement(vati_grid_24, vatican_ii_doctrinal_authority__composite_overdetermination_reading, stakes_inflation(structural), 2025, 0.58).
narrative_ontology:measurement(vati_grid_25, vatican_ii_doctrinal_authority__composite_overdetermination_reading, suppression(class), 1962, 0.22).
narrative_ontology:measurement(vati_grid_26, vatican_ii_doctrinal_authority__composite_overdetermination_reading, suppression(class), 2025, 0.48).
narrative_ontology:measurement(vati_grid_27, vatican_ii_doctrinal_authority__composite_overdetermination_reading, suppression(individual), 1962, 0.08).
narrative_ontology:measurement(vati_grid_28, vatican_ii_doctrinal_authority__composite_overdetermination_reading, suppression(individual), 2025, 0.38).
narrative_ontology:measurement(vati_grid_29, vatican_ii_doctrinal_authority__composite_overdetermination_reading, suppression(organizational), 1962, 0.18).
narrative_ontology:measurement(vati_grid_30, vatican_ii_doctrinal_authority__composite_overdetermination_reading, suppression(organizational), 2025, 0.42).
narrative_ontology:measurement(vati_grid_31, vatican_ii_doctrinal_authority__composite_overdetermination_reading, suppression(structural), 1962, 0.15).
narrative_ontology:measurement(vati_grid_32, vatican_ii_doctrinal_authority__composite_overdetermination_reading, suppression(structural), 2025, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vatican_ii_doctrinal_authority__composite_overdetermination_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(vatican_ii_doctrinal_authority__composite_overdetermination_reading, 0.15).
narrative_ontology:affects_constraint(vatican_ii_doctrinal_authority__composite_overdetermination_reading, vatican_ii_doctrinal_authority__continuity_reading).
narrative_ontology:affects_constraint(vatican_ii_doctrinal_authority__composite_overdetermination_reading, vatican_ii_doctrinal_authority__rupture_progressive_reading).
narrative_ontology:affects_constraint(vatican_ii_doctrinal_authority__composite_overdetermination_reading, vatican_ii_doctrinal_authority__rupture_traditionalist_reading).
narrative_ontology:affects_constraint(vatican_ii_doctrinal_authority__composite_overdetermination_reading, catholic_liturgical_authority__vernacular_shift).
narrative_ontology:affects_constraint(vatican_ii_doctrinal_authority__composite_overdetermination_reading, ecumenical_dialogue_legitimacy__nostra_aetate).
narrative_ontology:affects_constraint(vatican_ii_doctrinal_authority__composite_overdetermination_reading, religious_freedom_doctrine__dignitatis_humanae).
narrative_ontology:affects_constraint(vatican_ii_doctrinal_authority__composite_overdetermination_reading, episcopal_collegiality_authority__lumen_gentium).

% DUAL FORMULATION NOTE:
% Vatican II doctrinal authority is a contested kernel with four structural readings. The composite_overdetermination_reading claims Vatican II is not one constraint but the simultaneous instantiation of multiple distinct constraints, each with independent extractiveness. The sibling readings (continuity, rupture_progressive, rupture_traditionalist) all treat Vatican II as a unitary phenomenon to be classified as a whole. This reading decomposes it and influences the siblings by complicating their unitary claims. The four constraint stories form a kernel family linked by network.affects_constraints. The sibling readings occupy separate JSON files; together they model the contested interpretation of the vatican_ii_doctrinal_authority kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(vatican_ii_doctrinal_authority__composite_overdetermination_reading, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
