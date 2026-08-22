% ============================================================================
% CONSTRAINT STORY: biblical_authority__conciliar_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_biblical_authority__conciliar_reading, []).

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
    narrative_ontology:coordination_type/2,
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
 *   constraint_id: biblical_authority__conciliar_reading
 *   human_readable: Conciliar Reception of Scriptural Authority (Ecumenical Councils and Patristic Consensus)
 *   domain: theology/religious_studies/history_of_christianity
 *
 * SUMMARY:
 *   In the autocephalous Orthodox churches, scripture's authoritative meaning
 *   is settled through ecumenical councils and the consensus of the fathers,
 *   with tradition understood as a living continuity of liturgy, doctrine,
 *   and practice rather than as decrees issued by a supreme teacher. This
 *   story models that standing arrangement itself — the conciliar-patristic
 *   settlement as it operates — assessed by this reading's own lights; it is
 *   one reading of the contested kernel biblical_authority, with the sibling
 *   readings instantiated as separate constraint files linked in the network
 *   section. KEY AGENTS (by structural relationship): -
 *   episcopal_collegiality: agenda-setter and principal collector
 *   (institutional/identity_locked) — convenes councils and synods, defines
 *   dogma, gathers deference and interpretive authority -
 *   ordained_parish_clergy: beneficiary (organized/identity_locked) —
 *   mediates sacraments and teaching - monastic_doctrinal_establishment:
 *   beneficiary (organized/identity_locked) — curates the patristic corpus,
 *   informal doctrinal veto - orthodox_laity: dual beneficiary/payer
 *   (powerless/constrained) — receives stability and sacraments, bears
 *   exclusion from interpretation and slow-adaptation costs -
 *   dissenting_theologians: payer (moderate/constrained) — bear silencing and
 *   accusation risk - local_church_adaptation_movements: payer
 *   (organized/trapped) — seek liturgical and pastoral adaptation, face
 *   schism-level costs - historical_critical_scholars: excluded
 *   (moderate/mobile) - church_history_analysts: observer
 *   (analytical/analytical)
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(biblical_authority__conciliar_reading, 0.48).
domain_priors:suppression_score(biblical_authority__conciliar_reading, 0.34).
domain_priors:theater_ratio(biblical_authority__conciliar_reading, 0.43).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(biblical_authority__conciliar_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(biblical_authority__conciliar_reading, suppression_requirement, 0.34).
narrative_ontology:constraint_metric(biblical_authority__conciliar_reading, theater_ratio, 0.43).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(biblical_authority__conciliar_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(biblical_authority__conciliar_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(biblical_authority__conciliar_reading, tangled_rope).
narrative_ontology:human_readable(biblical_authority__conciliar_reading, "Conciliar Reception of Scriptural Authority (Ecumenical Councils and Patristic Consensus)").
narrative_ontology:topic_domain(biblical_authority__conciliar_reading, "theology/religious_studies/history_of_christianity").

domain_priors:requires_active_enforcement(biblical_authority__conciliar_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(biblical_authority__conciliar_reading, '3e561074-11d5-43a5-9f73-3e1d41e29e2c').
narrative_ontology:cs_kernel_codification('3e561074-11d5-43a5-9f73-3e1d41e29e2c', fixed_text).
narrative_ontology:cs_authority_grounding('3e561074-11d5-43a5-9f73-3e1d41e29e2c', lineage).
narrative_ontology:cs_interpretation_layer_present('3e561074-11d5-43a5-9f73-3e1d41e29e2c').
narrative_ontology:cs_reading_relation('3e561074-11d5-43a5-9f73-3e1d41e29e2c', biblical_authority__sola_scriptura_reading, forecloses).
narrative_ontology:cs_reading_relation('3e561074-11d5-43a5-9f73-3e1d41e29e2c', biblical_authority__tradition_scripture_reading, influences).
narrative_ontology:cs_axiom('3e561074-11d5-43a5-9f73-3e1d41e29e2c', foundational, scripture_requires_conciliar_reception).
narrative_ontology:cs_axiom_status(scripture_requires_conciliar_reception, holdable).
narrative_ontology:cs_axiom_grounding('3e561074-11d5-43a5-9f73-3e1d41e29e2c', scripture_requires_conciliar_reception, conventional).
narrative_ontology:cs_axiom('3e561074-11d5-43a5-9f73-3e1d41e29e2c', foundational, tradition_is_living_continuity_not_decree).
narrative_ontology:cs_axiom_status(tradition_is_living_continuity_not_decree, holdable).
narrative_ontology:cs_axiom_grounding('3e561074-11d5-43a5-9f73-3e1d41e29e2c', tradition_is_living_continuity_not_decree, theological).
narrative_ontology:cs_axiom('3e561074-11d5-43a5-9f73-3e1d41e29e2c', secondary, sacraments_are_encountered_mysteries).
narrative_ontology:cs_axiom_status(sacraments_are_encountered_mysteries, holdable).
narrative_ontology:cs_axiom_grounding('3e561074-11d5-43a5-9f73-3e1d41e29e2c', sacraments_are_encountered_mysteries, theological).
narrative_ontology:cs_reference_frame('3e561074-11d5-43a5-9f73-3e1d41e29e2c', undivided_church_conciliar_consensus).
narrative_ontology:cs_drift_state('3e561074-11d5-43a5-9f73-3e1d41e29e2c', contemporary_post_crete, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('3e561074-11d5-43a5-9f73-3e1d41e29e2c', '').
narrative_ontology:cs_kernel_id(biblical_authority__conciliar_reading, biblical_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(biblical_authority__conciliar_reading, episcopal_collegiality).
narrative_ontology:constraint_beneficiary(biblical_authority__conciliar_reading, ordained_parish_clergy).
narrative_ontology:constraint_beneficiary(biblical_authority__conciliar_reading, monastic_doctrinal_establishment).
narrative_ontology:constraint_beneficiary(biblical_authority__conciliar_reading, orthodox_laity).
narrative_ontology:constraint_victim(biblical_authority__conciliar_reading, dissenting_theologians).
narrative_ontology:constraint_victim(biblical_authority__conciliar_reading, local_church_adaptation_movements).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(biblical_authority__conciliar_reading, orthodox_laity).
narrative_ontology:constraint_vindicates(biblical_authority__conciliar_reading, patristic_consensus_as_hermeneutic_norm).
narrative_ontology:constraint_vindicates(biblical_authority__conciliar_reading, seven_ecumenical_councils_doctrinal_canon).
narrative_ontology:constraint_vindicates(biblical_authority__conciliar_reading, apostolic_tradition_living_transmission).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Bishops meeting in synod, and at rare historical moments in ecumenical council, set the terms on which scripture is authoritatively read: they define dogma, anathematize error, ordain and discipline clergy, and mark the line between apostolic teaching and novelty. Deference, interpretive authority, and material support flow to the episcopal office. A bishop who leaves the church loses the office itself, since the see and the man are bound together in consecration.
narrative_ontology:constraint_stakeholder(biblical_authority__conciliar_reading, episcopal_collegiality, agenda_setter,
    institutional, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(biblical_authority__conciliar_reading, episcopal_collegiality, beneficiary).

% Priests and deacons celebrate the sacraments, preach, and hand on the faith as received from the bishops and the fathers. Their livelihood and standing depend on remaining in good standing with their bishop. Ordination is understood as permanent, so leaving the altar leaves nothing comparable to return to.
narrative_ontology:constraint_stakeholder(biblical_authority__conciliar_reading, ordained_parish_clergy, beneficiary,
    organized, biographical, identity_locked, regional).

% Monasteries — Mount Athos above all — preserve and curate the patristic corpus, advise bishops informally, and mobilize opinion against innovations such as calendar change or ecumenical rapprochement. They hold no formal office in synod yet exercise a widely recognized informal veto on doctrinal change; their vows bind them to the life they defend.
narrative_ontology:constraint_stakeholder(biblical_authority__conciliar_reading, monastic_doctrinal_establishment, beneficiary,
    organized, generational, identity_locked, continental).

% The faithful receive the sacraments, keep the fasts and feasts, and inherit a doctrinal settlement they did not negotiate. They hold no seat in synod or council; their voice reaches doctrine indirectly, through liturgical practice and popular devotion. In traditionally Orthodox societies the church is fused with family and nation, so departure costs community, identity, and often livelihood.
narrative_ontology:constraint_stakeholder(biblical_authority__conciliar_reading, orthodox_laity, beneficiary,
    powerless, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(biblical_authority__conciliar_reading, orthodox_laity, payer).

% Academic and monastic theologians whose work presses past the received consensus — on anthropology, ecclesiology, or the limits of the patristic corpus — find publication channels narrowing, invitations withdrawn, and at the limit formal accusation. Some continue inside the bounds, some fall silent, and a few leave for other communions, losing their audience along with their standing.
narrative_ontology:constraint_stakeholder(biblical_authority__conciliar_reading, dissenting_theologians, payer,
    moderate, biographical, constrained, national).

% Parishes and jurisdictions pressing for calendar revision, vernacular liturgy, or pastoral accommodation meet the charge of innovation. The Old Calendarist splits show the price of pressing the point: those who acted lost communion with the churches they sought to reform and became sects in the eyes of the mother churches, forfeiting the very standing from which reform could be argued.
narrative_ontology:constraint_stakeholder(biblical_authority__conciliar_reading, local_church_adaptation_movements, payer,
    organized, generational, trapped, regional).

% Historians and textual critics who read the councils and the fathers as products of their imperial context rather than as timeless norms stand outside the frame entirely: their questions — redaction, political theology, manuscript history — are not admitted to the synodal agenda. They publish in university channels the church does not control and would contest the normativity of the consensus if given a seat.
narrative_ontology:constraint_stakeholder(biblical_authority__conciliar_reading, historical_critical_scholars, excluded,
    moderate, biographical, mobile, global).

% Comparative historians of doctrine watch how the conciliar claim is maintained across twelve centuries without a functioning ecumenical council: which synods are counted, which fathers are cited, which receptions are remembered and which forgotten. They collect no authority and bear no anathema; they describe.
narrative_ontology:constraint_stakeholder(biblical_authority__conciliar_reading, church_history_analysts, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(biblical_authority__conciliar_reading, episcopal_collegiality).
narrative_ontology:fixing_cost_class(biblical_authority__conciliar_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains doctrinal unity and a shared reading of scripture across many autocephalous churches without a single supreme interpreter: ecumenical councils aggregate dispersed episcopal judgment into binding consensus, patristic consensus supplies a common interpretive standard, and liturgical and calendrical continuity coordinate practice across jurisdictions.
% TRANSFER_FUNCTION: Moves interpretive authority and doctrinal decision rights from individuals, local communities, and scholars to the episcopal college acting in council; moves deference, obedience, and material support upward to the clergy; moves doctrinal stability, sacramental access, and insulation from doctrinal novelty downward to the laity.
% ABSENT_VOICES: Historical-critical scholars, whose method is ruled outside the frame; lay interpreters, who have no conciliar seat; theologians already anathematized or marginalized, whose objections survive only as condemned positions; the non-Chalcedonian Oriental churches, excluded by the very councils this reading treats as normative; and women, barred from episcopal and nearly all synodal seats.
% DISAPPEARANCE_RATIONALE: If the conciliar-patristic settlement vanished overnight, the autocephalous churches would drift onto independent doctrinal trajectories; the Nicene core would survive as text, but boundary-maintenance would reorganize around national churches, monastic networks, or congregational reading, and the episcopal mediation of sacraments together with the anathema machinery would lose its warrant.
% FOUNDING_PROBLEM: The apostolic churches faced rival readings of scripture — Arian, Nestorian, Monophysite — each claiming biblical warrant; the arrangement was built to distinguish authentic apostolic teaching from novelty by gathering the church's dispersed judgment in council and testing doctrine against the consensus of the fathers.
% FOUNDING_PROBLEM_CORROBORATION: Church historians and canonists outside the benefiting hierarchies attest the founding mechanism's dormancy — academic scholarship records roughly twelve centuries without a universally received council, and the Crete 2016 preparatory process itself documented the absence. The benefiting parties attest continued liveness, citing new doctrinal errors requiring conciliar discernment. The disagreement between these testimonies is the finding; no neutral seat currently adjudicates it.
narrative_ontology:disappearance_verdict(biblical_authority__conciliar_reading, world_rearranges).
narrative_ontology:founding_problem_status(biblical_authority__conciliar_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(biblical_authority__conciliar_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(biblical_authority__conciliar_reading, 'none', 1).
narrative_ontology:epsilon_provenance(biblical_authority__conciliar_reading, 0.48, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(biblical_authority__conciliar_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(biblical_authority__conciliar_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(biblical_authority__conciliar_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Time grid: t counts centuries since the First Council of Nicaea (t=0 is 325 CE; t=17 is the present), one shared grid for all three tracked series. Extractiveness is moderate (0.48 at present) and traces a hump: it climbed through the Ottoman millet era (t=12, 0.50), when bishops exercised state-delegated civil power over the faithful, then eased with secularization and settled at the present level — episcopal mediation is real but fragmented across autocephalous churches, none of which commands a single interpreter's leverage. Suppression is the raw structural property and is deliberately unscaled by power or scope; it has fallen from imperial-era coercion (0.68 at t=0, when anathemas were backed by exile and confiscation) to informal social pressure (0.34 now). That enforcement-decay trajectory is the story's central dynamic, which is why the suppression_requirement series is authored despite the static-endpoint picture. Theater ratio rises from 0.10 to 0.43: councils genuinely decided doctrine while they met, but no universally received ecumenical council has met since 787, and appeals to conciliar tradition increasingly decorate synodal business that no ecumenical body ratifies — Crete 2016, attended by a minority of churches, is the visible marker. Accessibility collapse is moderate (0.50): other communions and private reading remain reachable, so understanding the arrangement does not close every alternative. Resistance is substantial (0.55): the Arian-era turmoil, iconoclasm, the Old Calendarist schisms, and the Crete abstentions are all active refusals. The claimed type is authored independently of these metrics: the structure holds a genuine coordination function — dogmatic stability across many jurisdictions without a pope — together with real asymmetric extraction (interpretive monopoly, adaptation costs borne by payers), which is the tangled-rope shape. Coordination type is identity_coordination: the dominant function is maintaining membership boundaries and doctrinal identity across dispersed churches; the type-default floor applies, no override.
 *
 * PERSPECTIVAL GAP:
 *   From the episcopal seat the arrangement is the church's own life: councils as the body discerning truth, tradition as breathing continuity — coordination experienced from inside. From the payer seats the same structure is a closed interpretive circuit: those who read differently are anathematized rather than answered, and adaptation requests return as charges of novelty. The excluded scholarly seat sees a curated canon performing warrant-work for conclusions reached elsewhere. Same-power divergence: autocephalous jurisdictions of equal canonical rank experience the arrangement differently by size and diaspora position — large patriarchates can defy coordination attempts (the four churches that abstained from Crete), while small diaspora jurisdictions cannot, so exit by jurisdictional transfer is real for some payers and nominal for others. Inter-institutionally, the monastic establishment holds no office yet can veto what the episcopal agenda-setters propose, so the agenda-setting seat is itself internally divided.
 *
 * DIRECTIONALITY LOGIC:
 *   The beneficiary declarations drive the derivation: the episcopal college, parish clergy, and the monastic establishment sit near the beneficiary end (low d), and the two payer groups near the target end (high d). One override is authored: the powerless atom is pinned at d=0.5 because the laity are declared beneficiaries yet carry diffuse costs — no interpretive seat, slow doctrinal adaptation, and identity-fused exit costs in traditionally Orthodox societies — so derivation from the beneficiary declaration alone would misplace them at the subsidy end. The override is keyed to the power atom per the schema; the laity are the story's only powerless seat. Scope amplification applies to the global-scoped seats (episcopal college, laity): verifying doctrinal fidelity across continents is harder, which the engine registers as modestly amplified effective extraction on the target side. Suppression is never scaled by power or scope anywhere in the computation.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — binding discernment of authentic apostolic teaching amid rival scriptural readings — produced a mechanism that worked while empire could convene and enforce. The mechanism has been dormant at the ecumenical level for roughly twelve centuries; what persists is synodal governance inside the autocephalous churches plus the rhetorical appeal to conciliar precedent. Authoring the structure as tangled_rope rather than snare preserves the genuine coordination (dogmatic stability across jurisdictions, sacramental continuity) that a pure-extraction reading would erase; authoring it as anything purer than tangled_rope would erase the episcopal interpretive monopoly and the adaptation costs the payer seats bear. The rising theater series and the contested founding-status interview mark the ecumenical-level mandate as having outlived its function even while the local synodal function remains live — the mandatrophy is partial and level-specific, which is why the founding_problem_status is contested rather than dead. Fixing is prohibitive for whoever could fix it: reconvening a genuinely ecumenical council requires consensus among rival autocephalous jurisdictions currently in canonical rupture, and the cost of failure is schism, which is why the arrangement persists in its present half-functioning form.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indexicality,
    'This story instantiates one reading (conciliar_reading) of the kernel biblical_authority; how would epsilon, the beneficiary/victim structure, and the computed classification shift under the sibling readings sola_scriptura_reading and tradition_scripture_reading?',
    'Compile and compare the sibling stories as separate files; classification is reading-indexed, so cross-reading comparison runs through the network edges between files, never by averaging within one.',
    'Under sola_scriptura_reading the clerical mediation largely vanishes (no interpretive office to extract through) while fragmentation risk rises; under tradition_scripture_reading extraction concentrates further in a single magisterial office while fragmentation drops. Neither result transfers to this file; this story''s numbers are valid only for the conciliar arrangement.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_indexicality, conceptual, 'Reading-indexed classification of the biblical-authority kernel.').

omega_variable(
    patristic_consensus_construction,
    'Is ''patristic consensus'' a discovered living agreement among the fathers, or a retrospectively curated canon (selected fathers, selected florilegia) assembled to warrant conclusions reached on other grounds?',
    'Historiographical study of florilegia construction and citation practice across the doctrinal controversies: whether the cited consensus existed prior to the dispute or was assembled during it.',
    'If curated, part of the measured coordination is post-hoc warrant and the theater_ratio is understated; the hermeneutic standard operates as an episcopally curated artifact, pulling the structure toward the snare end of the hybrid range.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(patristic_consensus_construction, empirical, 'Discovered consensus versus curated canon as the operative hermeneutic standard.').

omega_variable(
    conciliar_dormancy_drift,
    'Has the conciliar mechanism atrophied into rhetorical appeal — no universally received ecumenical council since Second Nicaea (787), and the 2016 Council of Crete attended by a minority of autocephalous churches — such that the arrangement persists mainly by inertia and performance at the ecumenical level?',
    'Observe whether a future pan-Orthodox council achieves near-universal convocation and reception; track the theater_ratio series against that event.',
    'If dormancy persists and theater_ratio crosses 0.5, the structure drifts toward piton at the ecumenical level even while synodal governance remains functional inside the autocephalous churches.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(conciliar_dormancy_drift, empirical, 'Ecumenical-level mechanism dormancy versus living conciliarity.').

omega_variable(
    fragmentation_exit_genuineness,
    'Does autocephalous fragmentation give members genuine exit — transfer between jurisdictions dilutes coercive pressure — or do the jurisdictions converge on the same demands so that exit relocates rather than escapes them?',
    'Compare enforcement and adaptation costs across jurisdictions for identical disputes (calendar, remarriage canons, jurisdictional boundaries) for similarly situated believers.',
    'Genuine exit lowers effective suppression below the structural measure for mobile members; convergent extraction raises it; the classification of the payer seats shifts accordingly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fragmentation_exit_genuineness, empirical, 'Whether fragmentation functions as exit or as relocation of the same demands.').

omega_variable(
    lay_interpretive_agency,
    'Does liturgical participation constitute real interpretive agency for the laity inside conciliar reception (a sensus fidelium analogue, as when icon veneration preceded and shaped Nicaea II''s definition), or is lay reception purely passive?',
    'Study cases where liturgical and devotional practice preceded and shaped doctrinal reception, versus cases where practice followed imposed definition.',
    'Real agency lowers the powerless seat''s directionality below 0.5; purely passive reception raises it above 0.5; the authored override sits at the midpoint pending resolution.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(lay_interpretive_agency, conceptual, 'Lay agency inside conciliar reception.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(biblical_authority__conciliar_reading, 0, 17).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bibl_tr_t0, biblical_authority__conciliar_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(bibl_tr_t3, biblical_authority__conciliar_reading, theater_ratio, 3, 0.12).
narrative_ontology:measurement(bibl_tr_t6, biblical_authority__conciliar_reading, theater_ratio, 6, 0.18).
narrative_ontology:measurement(bibl_tr_t9, biblical_authority__conciliar_reading, theater_ratio, 9, 0.28).
narrative_ontology:measurement(bibl_tr_t12, biblical_authority__conciliar_reading, theater_ratio, 12, 0.33).
narrative_ontology:measurement(bibl_tr_t14, biblical_authority__conciliar_reading, theater_ratio, 14, 0.35).
narrative_ontology:measurement(bibl_tr_t16, biblical_authority__conciliar_reading, theater_ratio, 16, 0.4).
narrative_ontology:measurement(bibl_tr_t17, biblical_authority__conciliar_reading, theater_ratio, 17, 0.43).

% Extraction over time
narrative_ontology:measurement(bibl_be_t0, biblical_authority__conciliar_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(bibl_be_t3, biblical_authority__conciliar_reading, base_extractiveness, 3, 0.4).
narrative_ontology:measurement(bibl_be_t6, biblical_authority__conciliar_reading, base_extractiveness, 6, 0.42).
narrative_ontology:measurement(bibl_be_t9, biblical_authority__conciliar_reading, base_extractiveness, 9, 0.44).
narrative_ontology:measurement(bibl_be_t12, biblical_authority__conciliar_reading, base_extractiveness, 12, 0.5).
narrative_ontology:measurement(bibl_be_t14, biblical_authority__conciliar_reading, base_extractiveness, 14, 0.46).
narrative_ontology:measurement(bibl_be_t16, biblical_authority__conciliar_reading, base_extractiveness, 16, 0.47).
narrative_ontology:measurement(bibl_be_t17, biblical_authority__conciliar_reading, base_extractiveness, 17, 0.48).

% Suppression requirement over time
narrative_ontology:measurement(bibl_su_t0, biblical_authority__conciliar_reading, suppression_requirement, 0, 0.68).
narrative_ontology:measurement(bibl_su_t3, biblical_authority__conciliar_reading, suppression_requirement, 3, 0.65).
narrative_ontology:measurement(bibl_su_t6, biblical_authority__conciliar_reading, suppression_requirement, 6, 0.58).
narrative_ontology:measurement(bibl_su_t9, biblical_authority__conciliar_reading, suppression_requirement, 9, 0.5).
narrative_ontology:measurement(bibl_su_t12, biblical_authority__conciliar_reading, suppression_requirement, 12, 0.52).
narrative_ontology:measurement(bibl_su_t14, biblical_authority__conciliar_reading, suppression_requirement, 14, 0.44).
narrative_ontology:measurement(bibl_su_t16, biblical_authority__conciliar_reading, suppression_requirement, 16, 0.36).
narrative_ontology:measurement(bibl_su_t17, biblical_authority__conciliar_reading, suppression_requirement, 17, 0.34).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(biblical_authority__conciliar_reading, identity_coordination).
narrative_ontology:affects_constraint(biblical_authority__conciliar_reading, biblical_authority__sola_scriptura_reading).
narrative_ontology:affects_constraint(biblical_authority__conciliar_reading, biblical_authority__tradition_scripture_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'biblical authority' decomposes into three structurally distinct constraints, one per reading of the kernel: conciliar reception (this file — moderate extraction through episcopal mediation, moderate fragmentation across autocephalous churches), sola scriptura (separate file — clerical interpretive mediation largely absent, different beneficiary/victim set), and magisterial guardianship (separate file — extraction concentrated in a single office, fragmentation suppressed). The epsilon values differ because the arrangements differ, not because one underlying thing is measured inconsistently; family membership is carried by these network edges, and cross-reading comparison is legitimate only between files.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(biblical_authority__conciliar_reading, powerless, 0.5).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
