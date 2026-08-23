% ============================================================================
% CONSTRAINT STORY: biblical_authority__conciliar_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
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
 *   constraint_id: biblical_authority__conciliar_reading
 *   human_readable: Conciliar-Patristic Mediation of Biblical Authority
 *   domain: theology/religious_history
 *
 * SUMMARY:
 *   This story instantiates the conciliar reading of the kernel
 *   biblical_authority: Scripture is authoritatively interpreted through
 *   ecumenical councils and the consensus of the Fathers, with tradition
 *   understood as a living continuity of reception rather than a series of
 *   juridical decrees issued by a single magisterial office. The standing
 *   arrangement under assessment — the referent of every authored value here
 *   — is that conciliar-patristic mediation as it has actually operated from
 *   Nicaea onward and operates today across the autocephalous churches:
 *   bishops convene (increasingly rarely), definitions accumulate as a fixed
 *   inherited corpus, monastic and academic custodians curate the patristic
 *   witness, and members inside the communion may read Scripture freely but
 *   may not render binding doctrine privately. The arrangement has a real
 *   coordination product (one creed across many polities, heresies settled
 *   communally) and a real asymmetry (interpretive office, publication
 *   channels, and disciplinary power concentrated in the episcopate). Per the
 *   epsilon-invariance principle, the sibling readings are separate
 *   constraints with separate stories and separate epsilon values; nothing
 *   here hedges or averages across them. KEY AGENTS (by structural
 *   relationship): - episcopal_collegiality: Agenda-setter and principal
 *   collector (institutional / constrained) — convenes councils, ratifies
 *   definitions, disciplines dissent - autocephalous_patriarchates:
 *   Beneficiary with administrative agenda-setting reach (institutional /
 *   constrained) — regional custody of interpretation - monastic_scholarship:
 *   Beneficiary (organized / identity_locked) — curators of the patristic
 *   corpus - laity_in_communion: Dual-positioned bearer and recipient
 *   (moderate / constrained) - dissenting_theologians: Principal bearer
 *   (moderate / constrained) — proposals condemned, careers gated -
 *   academic_biblical_critics: Excluded (institutional / arbitrage) — no seat
 *   in adjudication - historical_ecclesiologists: Analytical observer
 *   (analytical / analytical)
 *
 * KEY AGENTS:
 *   - - episcopal_collegiality: Agenda-setter and principal beneficiary (institutional/constrained) — the college of bishops that convenes councils, ratifies definitions, and disciplines dissent; constrained exit because resignation from the college forfeits office itself
 *   - - autocephalous_patriarchates: Beneficiary (institutional/constrained) — national and regional church administrations holding local interpretive custody; exit would break communion with the sister churches that constitute them
 *   - - monastic_scholarship: Beneficiary (organized/identity_locked) — monasteries and academies that preserve, edit, and teach the patristic corpus; identity fusion of vow, career, and custodial role
 *   - - laity_in_communion: Payer with beneficiary secondary role (moderate/constrained) — receive the mysteries and a stable inherited faith; surrender private doctrinal judgment; exit means losing the sacramental community
 *   - - dissenting_theologians: Payer (moderate/constrained) — professors, clergy, and educated laity whose novel proposals are refused publication and office; exit priced in vocation and belonging
 *   - - academic_biblical_critics: Excluded (institutional/arbitrage) — external scholars with no seat and no stake in the communion's adjudication
 *   - - historical_ecclesiologists: Observer (analytical/analytical) — reconstruct attendance, pressure, and reception timelines from archives
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(biblical_authority__conciliar_reading, 0.58).
domain_priors:suppression_score(biblical_authority__conciliar_reading, 0.32).
domain_priors:theater_ratio(biblical_authority__conciliar_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(biblical_authority__conciliar_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(biblical_authority__conciliar_reading, suppression_requirement, 0.32).
narrative_ontology:constraint_metric(biblical_authority__conciliar_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(biblical_authority__conciliar_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(biblical_authority__conciliar_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(biblical_authority__conciliar_reading, tangled_rope).
narrative_ontology:human_readable(biblical_authority__conciliar_reading, "Conciliar-Patristic Mediation of Biblical Authority").
narrative_ontology:topic_domain(biblical_authority__conciliar_reading, "theology/religious_history").

domain_priors:requires_active_enforcement(biblical_authority__conciliar_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(biblical_authority__conciliar_reading, '5f008ff9-56b4-4ce1-b94e-d29628b871da').
narrative_ontology:cs_kernel_codification('5f008ff9-56b4-4ce1-b94e-d29628b871da', fixed_text).
narrative_ontology:cs_authority_grounding('5f008ff9-56b4-4ce1-b94e-d29628b871da', lineage).
narrative_ontology:cs_interpretation_layer_present('5f008ff9-56b4-4ce1-b94e-d29628b871da').
narrative_ontology:cs_reading_relation('5f008ff9-56b4-4ce1-b94e-d29628b871da', biblical_authority__sola_scriptura_reading, forecloses).
narrative_ontology:cs_reading_relation('5f008ff9-56b4-4ce1-b94e-d29628b871da', biblical_authority__tradition_scripture_reading, coexists_with).
narrative_ontology:cs_axiom('5f008ff9-56b4-4ce1-b94e-d29628b871da', foundational, corporate_reception_binds_doctrine).
narrative_ontology:cs_axiom_status(corporate_reception_binds_doctrine, holdable).
narrative_ontology:cs_axiom_grounding('5f008ff9-56b4-4ce1-b94e-d29628b871da', corporate_reception_binds_doctrine, theological).
narrative_ontology:cs_axiom('5f008ff9-56b4-4ce1-b94e-d29628b871da', foundational, tradition_is_living_continuity_not_decree).
narrative_ontology:cs_axiom_status(tradition_is_living_continuity_not_decree, holdable).
narrative_ontology:cs_axiom_grounding('5f008ff9-56b4-4ce1-b94e-d29628b871da', tradition_is_living_continuity_not_decree, conventional).
narrative_ontology:cs_reference_frame('5f008ff9-56b4-4ce1-b94e-d29628b871da', undivided_seven_council_consensus).
narrative_ontology:cs_drift_state('5f008ff9-56b4-4ce1-b94e-d29628b871da', contemporary_autocephalous_fragmentation, gap(authority_erosion, substantial, true)).
narrative_ontology:cs_created_at('5f008ff9-56b4-4ce1-b94e-d29628b871da', '').
narrative_ontology:cs_kernel_id(biblical_authority__conciliar_reading, biblical_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(biblical_authority__conciliar_reading, episcopal_collegiality).
narrative_ontology:constraint_beneficiary(biblical_authority__conciliar_reading, autocephalous_patriarchates).
narrative_ontology:constraint_beneficiary(biblical_authority__conciliar_reading, monastic_scholarship).
narrative_ontology:constraint_victim(biblical_authority__conciliar_reading, dissenting_theologians).
narrative_ontology:constraint_victim(biblical_authority__conciliar_reading, grassroots_renewal_movements).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(biblical_authority__conciliar_reading, laity_in_communion).
narrative_ontology:constraint_victim(biblical_authority__conciliar_reading, laity_in_communion).
narrative_ontology:constraint_vindicates(biblical_authority__conciliar_reading, apostolic_succession_doctrine).
narrative_ontology:constraint_vindicates(biblical_authority__conciliar_reading, reception_ecclesiology).
narrative_ontology:constraint_vindicates(biblical_authority__conciliar_reading, seven_council_creeds_normative).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Bishops who entered office through ordination traced to the apostles meet, when disputes demand it, in councils that define what the Church's shared reading of Scripture is. Between councils they govern through synods and by commemorating the received definitions. A bishop who teaches against the received consensus faces deposition by his peers; the college as a whole cannot revise doctrine without assembling across jurisdictions, and a bishop resigning the college forfeits the office itself.
narrative_ontology:constraint_stakeholder(biblical_authority__conciliar_reading, episcopal_collegiality, agenda_setter,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(biblical_authority__conciliar_reading, episcopal_collegiality, beneficiary).

% National and regional church administrations — patriarchates and their synods — hold day-to-day custody of interpretation inside their territories: they license theology faculties, approve catechetical materials, and decide which local synodal statements circulate. Their standing depends on remaining in communion with sister churches, which constrains unilateral doctrinal moves; several have recently exercised that constraint by boycotting a pan-Orthodox council they judged illegitimate.
narrative_ontology:constraint_stakeholder(biblical_authority__conciliar_reading, autocephalous_patriarchates, beneficiary,
    institutional, generational, constrained, continental).
narrative_ontology:stakeholder_secondary_role(biblical_authority__conciliar_reading, autocephalous_patriarchates, agenda_setter).

% Monasteries and church academies copy, edit, and teach the patristic corpus; their scholars supply the quotations that conciliar and synodal documents cite. Their vocation is vowed for life and their scholarly standing rests on guarding the received authors, so repudiating the consensus would end both the vow and the career simultaneously.
narrative_ontology:constraint_stakeholder(biblical_authority__conciliar_reading, monastic_scholarship, beneficiary,
    organized, generational, identity_locked, continental).

% Ordinary members hear Scripture within a fixed lectionary and hymnography that already embeds the conciliar decisions. When doctrinal questions arise they ask clergy; forming an independent judgment that departs from the received teaching brings pastoral correction, and leaving costs the sacramental community their family life and calendar are organized around. In exchange they receive the mysteries and an inherited faith that requires no personal adjudication of every controversy.
narrative_ontology:constraint_stakeholder(biblical_authority__conciliar_reading, laity_in_communion, payer,
    moderate, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(biblical_authority__conciliar_reading, laity_in_communion, beneficiary).

% Professors, clergy, and educated laypeople who propose new formulations — on anthropology, calendar, primacy, or hermeneutics — submit work through journals and committees controlled by the hierarchy. Proposals departing from consensus are refused publication in church venues, teaching posts are withdrawn, and in hard cases anathema is discussed. Exit exists but is priced in vocation and community: some move to other communions or to secular academia, abandoning the career their formation built.
narrative_ontology:constraint_stakeholder(biblical_authority__conciliar_reading, dissenting_theologians, payer,
    moderate, biographical, constrained, global).

% Lay-led currents pressing for liturgical, calendrical, or pastoral renewal inside particular churches lack any channel to place their proposals on a synodal agenda; their petitions wait on episcopal initiative that the consensus requirement discourages. Where they have proceeded anyway, whole communities have ended up outside communion (calendar schisms), absorbing the separation cost individually and permanently.
narrative_ontology:constraint_stakeholder(biblical_authority__conciliar_reading, grassroots_renewal_movements, payer,
    powerless, biographical, trapped, regional).

% University-based textual and historical critics produce readings of the biblical text with no seat in the communion's adjudication; their methods are neither commissioned nor answered by councils, and nothing blocks their careers because they stand outside the communion's employment and sacramental order entirely.
narrative_ontology:constraint_stakeholder(biblical_authority__conciliar_reading, academic_biblical_critics, excluded,
    institutional, biographical, arbitrage, global).

% Historians of the councils reconstruct attendance lists, imperial pressure, voting patterns, and reception timelines from archival sources. They belong to no church office and predict no doctrine; all parties use and ignore their findings as convenient.
narrative_ontology:constraint_stakeholder(biblical_authority__conciliar_reading, historical_ecclesiologists, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(biblical_authority__conciliar_reading, episcopal_collegiality).
narrative_ontology:fixing_cost_class(biblical_authority__conciliar_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides one adjudication mechanism for doctrinal disputes across many politically independent churches: contested readings are settled once, in council, against a curated patristic witness, so that polities with no shared sovereign nevertheless confess one faith — and the resulting definitions then serve as a standing reference that individual churches need not re-litigate.
% TRANSFER_FUNCTION: Moves binding interpretive authority from individual readers, local congregations, and innovating theologians to episcopal bodies acting collegially; moves deference and disciplinary obedience upward from members to the hierarchy; and moves doctrinal stability, a fixed creed, and access to the mysteries back down.
% ABSENT_VOICES: No lay delegate, woman, monastic voice, or external scholar holds a seat at councils; attendance is episcopal by construction, historically alongside convoking emperors. The separated non-Chalcedonian churches have been absent from the communion's adjudication since the fifth century, and the churches that boycotted the 2016 council removed themselves from its floor. Academic biblical criticism stands wholly outside the room whose conclusions it could test.
% DISAPPEARANCE_RATIONALE: If the conciliar-patristic mediation vanished overnight, each autocephalous church would adjudicate doctrine locally or not at all; the shared creedal core would fragment along national and jurisdictional lines; episcopal authority would lose its collective basis and reconstitute around local synods or charismatic teachers; and the entire ecumenical-dialogue architecture that presumes a common conciliar inheritance would lose its object. The arrangement's absence would be immediately and widely felt.
% FOUNDING_PROBLEM: The early churches faced successive communion-threatening disputes over how Scripture speaks of Christ — Arianism, Nestorianism, monotheletism — with no mechanism to settle what the whole Church teaches, risking permanent fragmentation into rival communions each claiming the same text.
% FOUNDING_PROBLEM_CORROBORATION: Secular historians of doctrine document the dispute-driven convening of the early councils from imperial and archival sources, independent of any beneficiary testimony; and the rival readings themselves corroborate the problem's reality — communions that reject this reading's solution still recite its creeds and treat its first councils as watershed events. No outside party, however, attests that the mechanism remains adequate today; on current adequacy the corroborating voices divide along communion lines.
narrative_ontology:disappearance_verdict(biblical_authority__conciliar_reading, world_rearranges).
narrative_ontology:founding_problem_status(biblical_authority__conciliar_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(biblical_authority__conciliar_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(biblical_authority__conciliar_reading, 'none', 1).
narrative_ontology:epsilon_provenance(biblical_authority__conciliar_reading, 0.58, 'stealth/ox-alpha', 'none', direct).

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
 *   Interval units are centuries elapsed since the Council of Nicaea (t0 = 325 CE; t17 = 2025 CE), and all three tracked series share that one grid — every metric is authored at every examined point, so the engine samples no substituted end-states. Extractiveness (0.58 at end, matching the scalar) rises steeply with imperial establishment (Theodosius and Justinian made conciliar definition coercive law), peaks in the high medieval communion, then declines slowly as disestablishment, nationalism, and diaspora pluralism erode the college's practical leverage, with a slight modern uptick as the rhetoric of 'Holy Tradition' intensifies while the instrument that produced it atrophied. The suppression series traces the enforcement arc the story actually tracks: build-up under Christian empire (peak around the iconoclast crisis and Nicaea II, t4 = 0.70), maturity through the Byzantine period, then long decay after 1453 and near-symbolic operation today — an enforcement-capacity trajectory, not a flat backdrop, which is why suppression_requirement is authored rather than left to the static scalar. Theater (ending 0.30, matching the scalar) climbs gently: synodal activity is real but an increasing share of maintenance consists of invoking the councils rhetorically rather than convening them. Accessibility_collapse 0.45 reflects that alternatives do not vanish once the mediation principle is grasped: rival readings persist institutionally next door (the sibling stories) and private reading is tolerated inside bounds. Resistance 0.50 reflects recurring, sometimes successful pushback — schisms, Old Calendarist splits, the 2016 boycott — but no abolition. The claimed type, tangled_rope, is stated from structure independently of these numbers: the arrangement demonstrably solves a communion-wide problem (settle contested readings once, for everyone, without a monarch-bishop) AND demonstrably collects through the same structure (office, publication, discipline concentrated in the episcopate). Suppression is authored as a raw structural property; the engine scales extractiveness by directionality and scope, not suppression.
 *
 * PERSPECTIVAL GAP:
 *   From the episcopal agenda-setter seat the same structure is experienced as the Church's own God-given manner of self-governance: binding definitions arrived at together are fidelity, not imposition, and the college bears heavy costs (assembly, unanimity-seeking, exposure to imperial and state pressure historically). From the dissenting theologian seat the identical structure operates as a closed adjudication market: no venue, no appeal, career termination. The engine computes this per-seat divergence from the structural data; the authored claim does not adjudicate it. Same-level lateral dynamics matter among the patriarchates: Moscow and Constantinople hold comparable institutional power yet experienced the 2016 Council oppositely — Constantinople as the long-delayed restoration of conciliar method it chairs, Moscow as an illegitimate diminution of conciliarity that licensed its own jurisdictional exit. Equal power atoms, opposed directionalities, differentiated by primacy claims and exit options rather than rank.
 *
 * DIRECTIONALITY LOGIC:
 *   Declarations drive the derivation. The episcopal college sits nearest the beneficiary pole (it writes the interpretive rules it administers; d near 0.0-0.1); the patriarchates collect custody rents regionally (low d, slightly raised by their own exposure to conciliar discipline); monastic scholarship collects prestige and purpose but with identity_locked exit, placing it low-d yet structurally unable to arbitrage its position. The laity occupy the middle: genuine sacramental and stability goods offset the surrendered judgment, landing near symmetry. Dissenting theologians and renewal movements sit near the full-target pole (their proposals are the direct object of enforcement; constrained exit amplifies). Continental-to-global scope raises verification cost across the communion, which the engine folds into amplified effective extraction on the target seats. No directionality_overrides are authored: beneficiary/victim declarations plus exit atoms already reproduce the relationships, and no agent wears a disguise the derivation would miss.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — settling communion-threatening doctrinal disputes by a mechanism binding on all churches — remains live: disputes recur, and even communions that reject this reading still recite its creeds. But the instrument that constituted the arrangement, a council receivable by the whole body, has not fully operated since the eighth century by the strictest count, and the 2016 attempt fell short of plenary attendance. The arrangement is therefore not resolved mandatrophy (the function persists) but neither is it healthy rope: maintenance increasingly consists of commemorating past councils rather than exercising the method, which is where the theater ratio registers. The tangled_rope classification is what prevents both mislabels: reading the structure as pure extraction ('priestcraft') erases the real heresy-prevention and unity function that even rivals rely on; reading it as pure coordination ('unity through concord') erases the identifiable bearers — the silenced theologian, the subordinated lay judgment — whose costs fund the episcopal seat's position. The R5 mismatch consumer reads founding_problem_status=live against disappearance_verdict=world_rearranges: consistent, no zombie flag.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    committer_kernel_structure,
    'This constraint is one reading of the kernel biblical_authority: how would the sibling readings (biblical_authority__sola_scriptura_reading, biblical_authority__tradition_scripture_reading) restructure it, and where exactly is the disagreement located?',
    'Compile all three sibling stories and compare computed types, victim sets, and epsilon over the shared kernel referent. The disagreement locates in the seat authorized to render binding interpretation: the bare self-interpreting text (sola), the received conciliar-patristic consensus (this reading), or the magisterial office (tradition reading).',
    'Adopting the sola reading removes the episcopal beneficiary seat entirely and re-targets unguided private readers; adopting the tradition reading recenters collection on a single magisterial office and converts the autocephalous churches from beneficiaries into targets. This story''s epsilon is indexed to the conciliar-patristic arrangement only.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(committer_kernel_structure, conceptual, 'Committer structure: which kernel, which reading, what siblings would change.').

omega_variable(
    reception_constitutionality,
    'Is a council''s definition binding at promulgation, or only upon reception by the whole body — and if reception is constitutive, who actually exercises binding authority?',
    'Comparative reception histories: Chalcedon''s decades-long acceptance in parts of the non-Greek East, the initial rejection of Leo''s Tome, tracing when resistance stopped being canonically legitimate.',
    'If reception is constitutive, binding authority diffuses below the episcopal agenda-setting seat and effective extraction on lay and dissenting seats falls; if promulgation binds outright, collection concentrates in the college.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reception_constitutionality, conceptual, 'Whether binding force sits in the decree or in the body''s reception of it.').

omega_variable(
    patristic_consensus_realism,
    'Is ''the consensus of the Fathers'' a recoverable majority voice in the pre-conciliar corpus, or a florilegium assembled retrospectively to serve present hierarchies?',
    'Digital corpus analysis comparing the complete patristic corpus against the citation sets deployed in conciliar acts, synodal documents, and modern catechesis.',
    'If the consensus is substantially curated, extractiveness and theater rise (the invoked past is partly manufactured); if it is robust, the coordination claim strengthens and the arrangement looks more purely functional.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(patristic_consensus_realism, empirical, 'Whether the invoked patristic consensus is discovered or constructed.').

omega_variable(
    suppression_structural_vs_internalized,
    'Is lay deference to conciliar-patristic interpretation structural (no accessible alternative adjudication inside the communion) or internalized (formed piety treating private doctrinal judgment as presumption)?',
    'Post-exit trajectories of converts out of the communion: whether capacity for independent doctrinal judgment returns promptly after departure or persists as scruple.',
    'If internalized, effective suppression travels with former members beyond institutional reach and outlasts the measured decay of enforcement machinery.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'Structural versus internalized mechanism of lay deference.').

omega_variable(
    ecumenicity_threshold,
    'What attendance or reception threshold makes a council ''ecumenical'' and hence binding for all autocephalous churches — and did the 2016 Council of Crete meet it with four churches absent?',
    'Canonical-theological analysis against precedent (Nicaea II''s initially thin reception) together with the ongoing Moscow-Constantinople rupture over Crete''s validity and jurisdiction.',
    'If the threshold is unmeetable under current fragmentation, the arrangement''s active enforcement approaches zero and classification slides toward inertial maintenance; if met, enforcement persists and the payer seats remain actively bound.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ecumenicity_threshold, empirical, 'Whether the conciliar instrument can currently bind the whole communion.').

omega_variable(
    kernel_codification_framing,
    'Should the kernel be framed as the fixed biblical text whose transmitted interpretation carries authority (codification fixed_text), or as the distributed practice of consensual reading across autocephalous churches (codification distributed, no single adjudicator)?',
    'Test which framing predicts practice: if autocephalous churches routinely adjudicate independently and reconcile post hoc, the distributed framing fits; if they treat the received conciliar corpus as a standing standard above local synods, fixed_text fits.',
    'Under the distributed framing the interpretation-layer declaration fails, enforcement reads as weaker and looser, and the classification could shift toward plain coordination; under fixed_text the lineage-grounded interpreter structure holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_codification_framing, conceptual, 'Under-determined codification framing of the kernel beneath this reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(biblical_authority__conciliar_reading, 0, 17).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bibl_tr_t0, biblical_authority__conciliar_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement_basis(bibl_tr_t0, observed).
narrative_ontology:measurement(bibl_tr_t2, biblical_authority__conciliar_reading, theater_ratio, 2, 0.14).
narrative_ontology:measurement_basis(bibl_tr_t2, observed).
narrative_ontology:measurement(bibl_tr_t4, biblical_authority__conciliar_reading, theater_ratio, 4, 0.18).
narrative_ontology:measurement_basis(bibl_tr_t4, observed).
narrative_ontology:measurement(bibl_tr_t7, biblical_authority__conciliar_reading, theater_ratio, 7, 0.22).
narrative_ontology:measurement_basis(bibl_tr_t7, observed).
narrative_ontology:measurement(bibl_tr_t10, biblical_authority__conciliar_reading, theater_ratio, 10, 0.24).
narrative_ontology:measurement_basis(bibl_tr_t10, observed).
narrative_ontology:measurement(bibl_tr_t13, biblical_authority__conciliar_reading, theater_ratio, 13, 0.26).
narrative_ontology:measurement_basis(bibl_tr_t13, observed).
narrative_ontology:measurement(bibl_tr_t15, biblical_authority__conciliar_reading, theater_ratio, 15, 0.28).
narrative_ontology:measurement_basis(bibl_tr_t15, observed).
narrative_ontology:measurement(bibl_tr_t16, biblical_authority__conciliar_reading, theater_ratio, 16, 0.29).
narrative_ontology:measurement_basis(bibl_tr_t16, observed).
narrative_ontology:measurement(bibl_tr_t17, biblical_authority__conciliar_reading, theater_ratio, 17, 0.3).
narrative_ontology:measurement_basis(bibl_tr_t17, observed).

% Extraction over time
narrative_ontology:measurement(bibl_be_t0, biblical_authority__conciliar_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement_basis(bibl_be_t0, observed).
narrative_ontology:measurement(bibl_be_t2, biblical_authority__conciliar_reading, base_extractiveness, 2, 0.52).
narrative_ontology:measurement_basis(bibl_be_t2, observed).
narrative_ontology:measurement(bibl_be_t4, biblical_authority__conciliar_reading, base_extractiveness, 4, 0.6).
narrative_ontology:measurement_basis(bibl_be_t4, observed).
narrative_ontology:measurement(bibl_be_t7, biblical_authority__conciliar_reading, base_extractiveness, 7, 0.64).
narrative_ontology:measurement_basis(bibl_be_t7, observed).
narrative_ontology:measurement(bibl_be_t10, biblical_authority__conciliar_reading, base_extractiveness, 10, 0.62).
narrative_ontology:measurement_basis(bibl_be_t10, observed).
narrative_ontology:measurement(bibl_be_t13, biblical_authority__conciliar_reading, base_extractiveness, 13, 0.58).
narrative_ontology:measurement_basis(bibl_be_t13, observed).
narrative_ontology:measurement(bibl_be_t15, biblical_authority__conciliar_reading, base_extractiveness, 15, 0.54).
narrative_ontology:measurement_basis(bibl_be_t15, observed).
narrative_ontology:measurement(bibl_be_t16, biblical_authority__conciliar_reading, base_extractiveness, 16, 0.55).
narrative_ontology:measurement_basis(bibl_be_t16, observed).
narrative_ontology:measurement(bibl_be_t17, biblical_authority__conciliar_reading, base_extractiveness, 17, 0.58).
narrative_ontology:measurement_basis(bibl_be_t17, observed).

% Suppression requirement over time
narrative_ontology:measurement(bibl_su_t0, biblical_authority__conciliar_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement_basis(bibl_su_t0, observed).
narrative_ontology:measurement(bibl_su_t2, biblical_authority__conciliar_reading, suppression_requirement, 2, 0.55).
narrative_ontology:measurement_basis(bibl_su_t2, observed).
narrative_ontology:measurement(bibl_su_t4, biblical_authority__conciliar_reading, suppression_requirement, 4, 0.7).
narrative_ontology:measurement_basis(bibl_su_t4, observed).
narrative_ontology:measurement(bibl_su_t7, biblical_authority__conciliar_reading, suppression_requirement, 7, 0.65).
narrative_ontology:measurement_basis(bibl_su_t7, observed).
narrative_ontology:measurement(bibl_su_t10, biblical_authority__conciliar_reading, suppression_requirement, 10, 0.52).
narrative_ontology:measurement_basis(bibl_su_t10, observed).
narrative_ontology:measurement(bibl_su_t13, biblical_authority__conciliar_reading, suppression_requirement, 13, 0.42).
narrative_ontology:measurement_basis(bibl_su_t13, observed).
narrative_ontology:measurement(bibl_su_t15, biblical_authority__conciliar_reading, suppression_requirement, 15, 0.36).
narrative_ontology:measurement_basis(bibl_su_t15, observed).
narrative_ontology:measurement(bibl_su_t16, biblical_authority__conciliar_reading, suppression_requirement, 16, 0.34).
narrative_ontology:measurement_basis(bibl_su_t16, observed).
narrative_ontology:measurement(bibl_su_t17, biblical_authority__conciliar_reading, suppression_requirement, 17, 0.32).
narrative_ontology:measurement_basis(bibl_su_t17, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(biblical_authority__conciliar_reading, identity_coordination).
narrative_ontology:affects_constraint(biblical_authority__conciliar_reading, biblical_authority__sola_scriptura_reading).
narrative_ontology:affects_constraint(biblical_authority__conciliar_reading, biblical_authority__tradition_scripture_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition of the kernel biblical_authority. The colloquial label 'how Scripture functions as authority' covers three structurally distinct arrangements with distinct epsilon values and distinct victim sets: the conciliar-patristic mediation (this story), the text-alone sufficiency claim (sola sibling), and the magisterial-custody claim (tradition sibling). Upstream/downstream texture: the canon of Scripture that the sola reading presupposes was itself fixed by the conciliar process this story describes, giving this reading a historical upstream influence on both siblings even where its normative claims conflict with theirs; the tradition sibling shares this story's patristic material but reroutes adjudication from reception to decree. Each file links the others via network.affects_constraints; no story hedges epsilon across the family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
