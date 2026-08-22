% ============================================================================
% CONSTRAINT STORY: biblical_authority__conciliar_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
 *   constraint_id: biblical_authority__conciliar_reading
 *   human_readable: Scripture Read Through Ecumenical Councils and Patristic Consensus (Conciliar/Orthodox Reading)
 *   domain: theology/religious_studies/history_of_christianity
 *
 * SUMMARY:
 *   This constraint models the Eastern Orthodox and broadly conciliar reading
 *   of biblical authority: Scripture is not self-interpreting and does not
 *   require a single magisterial office to guard its meaning; rather, the
 *   seven ecumenical councils and the accumulated consensus of the Church
 *   Fathers constitute the authoritative interpretive tradition, understood
 *   as living continuity carried by the whole episcopal college in communion
 *   rather than decreed from a single see. Extraction here is moderate and
 *   structurally different from a papal-magisterium reading: authority is
 *   diffused across bishops and autocephalous churches (episcopal, not papal,
 *   extraction), and fragmentation is a real cost — autocephaly protects
 *   local hierarchical power at the price of no unified doctrinal
 *   clearinghouse when disputes arise between sees. The primary victims are
 *   not dissenters from a single central authority but those seeking rapid
 *   doctrinal adaptation to changed pastoral circumstances, and
 *   diaspora/minority traditions whose communities parted ways at specific
 *   councils (non-Chalcedonian churches) and have borne centuries of reduced
 *   recognition.
 *
 * KEY AGENTS:
 *   - episcopal_college: primary agenda-setter, diffused institutional authority, near-total exit via arbitrage (moves between council venues, retains office regardless of any one dispute's outcome)
 *   - patristic_scholars: beneficiary class whose vocational and institutional standing depends on patristic consensus being treated as living witness
 *   - autocephalous_church_hierarchies: dual beneficiary/agenda-setter, protect local power via decentralization
 *   - doctrinal_reformers: payer, trapped between advocating change and being read as schismatic
 *   - local_congregations_seeking_adaptation: powerless payer, bears the practical pastoral cost
 *   - diaspora_minority_traditions: powerless payer, historically excluded by specific council outcomes
 *   - religious_historians: analytical observer of how conciliar consensus was actually constructed
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(biblical_authority__conciliar_reading, 0.42).
domain_priors:suppression_score(biblical_authority__conciliar_reading, 0.38).
domain_priors:theater_ratio(biblical_authority__conciliar_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(biblical_authority__conciliar_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(biblical_authority__conciliar_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(biblical_authority__conciliar_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(biblical_authority__conciliar_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(biblical_authority__conciliar_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(biblical_authority__conciliar_reading, tangled_rope).
narrative_ontology:human_readable(biblical_authority__conciliar_reading, "Scripture Read Through Ecumenical Councils and Patristic Consensus (Conciliar/Orthodox Reading)").
narrative_ontology:topic_domain(biblical_authority__conciliar_reading, "theology/religious_studies/history_of_christianity").

domain_priors:requires_active_enforcement(biblical_authority__conciliar_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(biblical_authority__conciliar_reading, '9ba9d3d3-dbbb-43f4-a90b-8e9ab399e8ca').
narrative_ontology:cs_kernel_codification('9ba9d3d3-dbbb-43f4-a90b-8e9ab399e8ca', fixed_text).
narrative_ontology:cs_authority_grounding('9ba9d3d3-dbbb-43f4-a90b-8e9ab399e8ca', lineage).
narrative_ontology:cs_interpretation_layer_present('9ba9d3d3-dbbb-43f4-a90b-8e9ab399e8ca').
narrative_ontology:cs_reading_relation('9ba9d3d3-dbbb-43f4-a90b-8e9ab399e8ca', biblical_authority__sola_scriptura_reading, coexists_with).
narrative_ontology:cs_reading_relation('9ba9d3d3-dbbb-43f4-a90b-8e9ab399e8ca', biblical_authority__tradition_scripture_reading, influences).
narrative_ontology:cs_axiom('9ba9d3d3-dbbb-43f4-a90b-8e9ab399e8ca', foundational, authority_diffused_across_episcopal_collegium).
narrative_ontology:cs_axiom_status(authority_diffused_across_episcopal_collegium, holdable).
narrative_ontology:cs_axiom_grounding('9ba9d3d3-dbbb-43f4-a90b-8e9ab399e8ca', authority_diffused_across_episcopal_collegium, conventional).
narrative_ontology:cs_axiom('9ba9d3d3-dbbb-43f4-a90b-8e9ab399e8ca', foundational, ecumenical_reception_not_unilateral_decree).
narrative_ontology:cs_axiom_status(ecumenical_reception_not_unilateral_decree, holdable).
narrative_ontology:cs_axiom_grounding('9ba9d3d3-dbbb-43f4-a90b-8e9ab399e8ca', ecumenical_reception_not_unilateral_decree, conventional).
narrative_ontology:cs_reference_frame('9ba9d3d3-dbbb-43f4-a90b-8e9ab399e8ca', seven_ecumenical_councils_undivided_church).
narrative_ontology:cs_drift_state('9ba9d3d3-dbbb-43f4-a90b-8e9ab399e8ca', post_great_schism_autocephalous_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('9ba9d3d3-dbbb-43f4-a90b-8e9ab399e8ca', '').
narrative_ontology:cs_kernel_id(biblical_authority__conciliar_reading, biblical_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(biblical_authority__conciliar_reading, episcopal_college).
narrative_ontology:constraint_beneficiary(biblical_authority__conciliar_reading, patristic_scholars).
narrative_ontology:constraint_beneficiary(biblical_authority__conciliar_reading, autocephalous_church_hierarchies).
narrative_ontology:constraint_victim(biblical_authority__conciliar_reading, doctrinal_reformers).
narrative_ontology:constraint_victim(biblical_authority__conciliar_reading, local_congregations_seeking_adaptation).
narrative_ontology:constraint_victim(biblical_authority__conciliar_reading, diaspora_minority_traditions).
narrative_ontology:constraint_vindicates(biblical_authority__conciliar_reading, conciliar_infallibility_in_ecumenical_council).
narrative_ontology:constraint_vindicates(biblical_authority__conciliar_reading, patristic_consensus_as_authoritative_witness).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Bishops in council, understood as collectively continuing apostolic succession, adjudicate what counts as authentic reception of Scripture through the lens of the seven ecumenical councils and the patristic corpus. No single bishop or see holds final say; authority is diffused across the collegium and confirmed by reception across the churches. They administer liturgical and doctrinal boundaries and can excommunicate or bless a reading as orthodox.
narrative_ontology:constraint_stakeholder(biblical_authority__conciliar_reading, episcopal_college, agenda_setter,
    institutional, civilizational, arbitrage, continental).

% Theologians and monastic scholars whose vocation is explicating the Fathers gain standing, teaching authority, and institutional position precisely because patristic consensus is treated as living authoritative witness rather than closed text. Their expertise is the currency the system runs on.
narrative_ontology:constraint_stakeholder(biblical_authority__conciliar_reading, patristic_scholars, beneficiary,
    organized, generational, constrained, continental).

% National and regional churches (Greek, Russian, Antiochian, and others) retain administrative and canonical self-governance under the conciliar framework rather than submitting to a single central magisterium. This decentralization protects local hierarchical power and cultural-liturgical particularity, at the cost of a unified doctrinal clearinghouse.
narrative_ontology:constraint_stakeholder(biblical_authority__conciliar_reading, autocephalous_church_hierarchies, beneficiary,
    institutional, civilizational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(biblical_authority__conciliar_reading, autocephalous_church_hierarchies, agenda_setter).

% Clergy or laity who believe Scripture and pastoral need require doctrinal development beyond what patristic consensus sanctions (on matters such as remarriage, ordination, or moral theology) find the councils treated as closed and consensus treated as already settled. Because no single authority can be lobbied or reformed unilaterally, and because dissent from received consensus reads as rupture with the whole tradition, their paths to change are narrow: local pastoral accommodation, quiet noncompliance, or schism.
narrative_ontology:constraint_stakeholder(biblical_authority__conciliar_reading, doctrinal_reformers, payer,
    moderate, biographical, trapped, national).

% Ordinary parishioners whose pastoral circumstances (intermarriage, divorce, modern social conditions) sit awkwardly against patristic-era norms bear the cost of a system that treats those norms as living and binding rather than historically contingent. Exit means leaving the sacramental community that structures their religious and social life, which for most is not a real option.
narrative_ontology:constraint_stakeholder(biblical_authority__conciliar_reading, local_congregations_seeking_adaptation, payer,
    powerless, biographical, trapped, local).

% Communities descended from churches that rejected some ecumenical councils (e.g. Oriental Orthodox groups after Chalcedon) or that hold divergent patristic emphases are treated as outside full communion by the conciliar mainstream, bearing centuries of reduced ecclesial recognition despite shared apostolic lineage.
narrative_ontology:constraint_stakeholder(biblical_authority__conciliar_reading, diaspora_minority_traditions, payer,
    powerless, generational, constrained, regional).

% Study how conciliar decisions were actually reached — political pressure from emperors, regional rivalries, the slow and contested process by which 'consensus' was retroactively declared — and assess how much of the claimed continuity is genuine doctrinal stability versus constructed narrative.
narrative_ontology:constraint_stakeholder(biblical_authority__conciliar_reading, religious_historians, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a mechanism for resolving doctrinal disputes across a geographically dispersed, linguistically diverse church without a single central arbiter: bishops gathered in council, backed by patristic testimony, produce decisions that (after a period of reception) bind the whole communion, preventing doctrinal fragmentation while avoiding concentration of interpretive power in one office.
% TRANSFER_FUNCTION: Moves interpretive authority and the power to declare orthodoxy from individual believers or local congregations to the episcopal collegium and the accumulated patristic corpus; moves adaptive flexibility away from local pastoral needs and toward continuity with historically settled consensus.
% ABSENT_VOICES: Communities that split from the conciliar mainstream after specific councils (non-Chalcedonian churches), lay theological movements advocating doctrinal development, and diaspora minorities whose local customs diverge from received patristic norms are treated as outside the boundary of legitimate interpretation rather than invited into the ongoing conciliar conversation.
% DISAPPEARANCE_RATIONALE: Episcopal hierarchies and patristic scholarship argue the whole edifice of shared doctrine, liturgy, and ecclesial identity would fragment into unmoored individual interpretation without conciliar-patristic authority; doctrinal reformers and diaspora communities argue that much of what appears as necessary continuity is inherited political settlement from the 4th-8th centuries that could be revisited without dissolving Christian identity itself — the disagreement is exactly what the kernel contest is about.
% FOUNDING_PROBLEM: The early church faced doctrinal chaos (Arianism, Nestorianism, Monophysitism, and other Christological and Trinitarian disputes) threatening to split apostolic communities into mutually anathematizing factions; ecumenical councils convened to establish binding common ground.
% FOUNDING_PROBLEM_CORROBORATION: Historians of late antiquity (working outside any single church's institutional structure) attest the founding problem — Christological fragmentation under imperial pressure — was real and acute in the 4th-7th centuries. The same historians note the councils' authority was also entangled with imperial politics and regional ecclesial rivalry, complicating the churches' own narrative that patristic consensus emerged purely from theological discernment. Diaspora minority traditions dispute that the resulting consensus was ever universal, since their own communities rejected specific council outcomes at the time.
narrative_ontology:disappearance_verdict(biblical_authority__conciliar_reading, contested).
narrative_ontology:founding_problem_status(biblical_authority__conciliar_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(biblical_authority__conciliar_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(biblical_authority__conciliar_reading, 'none', 1).
narrative_ontology:epsilon_provenance(biblical_authority__conciliar_reading, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(biblical_authority__conciliar_reading_tests).
:- end_tests(biblical_authority__conciliar_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.42) is moderate — lower than a centralized magisterial reading because authority is genuinely diffused across bishops and self-governing churches rather than concentrated in one office, but non-trivial because episcopal collegiality still forecloses individual or congregational reinterpretation and channels interpretive power to a specific clerical class (bishops + patristic scholars) rather than to the whole body of believers. Suppression (0.38) reflects real but not maximal coercive force: councils historically relied on imperial backing and excommunication rather than a unified enforcement apparatus, and the suppression_requirement series actually falls over the interval (0.5 to 0.38) as imperial coercive backing receded after late antiquity and the constraint came to rest more on institutional inertia, communal identity, and reception than active coercion. Theater ratio (0.3) is moderate: much conciliar and patristic invocation is genuine doctrinal reasoning, but a meaningful share of 'consensus' framing performs retroactive unity over what was, historically, a more contested and politically entangled process (per religious_historians' seat). Accessibility collapse (0.45) is moderate-low: rival readings (sola scriptura, papal magisterium, and non-Chalcedonian christologies) visibly persist as live alternatives, so alternatives have not collapsed the way they would under a genuine mountain.
 *
 * PERSPECTIVAL GAP:
 *   From the episcopal college's own seat, conciliar reception is a coordination technology solving real Christological chaos without concentrating power in one office — a genuinely more distributed and less extractive answer than either sola scriptura's individualism or papal magisterium's centralization. From the seat of doctrinal reformers and local congregations, the same diffusion becomes a different kind of trap: there is no single office to petition or reform, no clear locus of accountability, and 'living tradition' functions in practice as an extremely high bar against any change, since it requires reconstructing something resembling patristic-era consensus rather than a single authoritative ruling.
 *
 * DIRECTIONALITY LOGIC:
 *   Episcopal college and autocephalous hierarchies sit near the beneficiary end: they administer the interpretive boundary and retain office and standing regardless of any specific doctrinal dispute's outcome (arbitrage-grade exit). Patristic scholars are a secondary beneficiary class whose vocational standing depends on the arrangement's persistence. Doctrinal reformers and local congregations sit near the target end: trapped exit options, real costs from an arrangement they did not design and cannot easily petition to change. Diaspora minority traditions carry a distinct kind of cost — not contemporary suppression but inherited exclusion from specific historical council outcomes, which the conciliar reading's own logic treats as settled rather than open to revisiting.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (Christological and Trinitarian fragmentation threatening ecclesial unity) was genuinely acute in the 4th-7th centuries and is corroborated by historians outside the church hierarchy. Whether that problem remains live in its original form, or whether the conciliar-patristic apparatus now persists mainly to police boundaries against doctrinal development unrelated to Christology proper (marriage, ordination, bioethics), is exactly the contested founding_problem_status this story declares. The classification as tangled_rope rather than pure snare or pure rope reflects that both readings are defensible: real coordination function against real historical fragmentation risk, genuine ongoing extraction in the form of narrowed interpretive latitude for non-clerical voices and structurally disadvantaged diaspora communities.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    conciliar_reading_vs_sibling_readings,
    'Is the conciliar/episcopal-collegial reading of biblical authority a structurally distinct constraint from the papal-magisterial reading and the sola scriptura reading, or are all three better modeled as points on a single authority-concentration spectrum?',
    'This is handled by decomposition rather than internal resolution: each reading is authored as its own constraint story (conciliar_reading here; sola_scriptura_reading and tradition_scripture_reading as siblings) linked via network.affects_constraints, per the ε-invariance principle for kernel readings.',
    'Keeps ε stable within this story (moderate extraction, episcopal-not-papal) rather than averaging across readings that have genuinely different beneficiary structures, victim sets, and enforcement mechanisms.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(conciliar_reading_vs_sibling_readings, conceptual, 'Kernel decomposition: this story is one reading among three of the biblical_authority kernel.').

omega_variable(
    consensus_construction_vs_organic_reception,
    'Was patristic ''consensus'' as invoked by later councils and theologians a genuine, organic convergence of testimony, or is it substantially a retrospective narrative constructed by the winning faction of each doctrinal dispute (with imperial political backing) and then projected backward as timeless agreement?',
    'Comparative historical analysis of council proceedings, contemporary dissenting writings, and the timeline by which ''consensus'' claims were formalized relative to the actual disputes — much of this evidence already exists in patristic and conciliar acta and has been analyzed by historians of late antiquity.',
    'If substantially constructed, the theater_ratio and extractiveness for this reading should be revised upward, since ''living continuity'' would function partly as legitimating cover for contested, politically-mediated decisions rather than as descriptively accurate historical continuity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(consensus_construction_vs_organic_reception, empirical, 'Whether patristic consensus is organic or retrospectively constructed.').

omega_variable(
    episcopal_diffusion_genuine_or_nominal,
    'Does the absence of a single magisterial office actually produce meaningfully less extraction than a centralized magisterium, or does the episcopal collegium function in practice as an equally closed clerical class that merely lacks a single named office at its apex?',
    'Compare rates and mechanisms of doctrinal appeal, dissent-handling, and lay/clergy participation in decision-making across conciliar-reading churches versus magisterial-reading churches over comparable historical periods.',
    'If diffusion is largely nominal, this reading''s moderate extractiveness score (0.42, lower than a magisterial reading) would need to be revised upward toward parity with the tradition_scripture_reading sibling.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(episcopal_diffusion_genuine_or_nominal, empirical, 'Whether episcopal collegiality meaningfully reduces extraction relative to a magisterial office, or is a distinction without a difference.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(biblical_authority__conciliar_reading, 0, 1700).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bibl_tr_t0, biblical_authority__conciliar_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(bibl_tr_t300, biblical_authority__conciliar_reading, theater_ratio, 300, 0.15).
narrative_ontology:measurement(bibl_tr_t700, biblical_authority__conciliar_reading, theater_ratio, 700, 0.22).
narrative_ontology:measurement(bibl_tr_t1000, biblical_authority__conciliar_reading, theater_ratio, 1000, 0.26).
narrative_ontology:measurement(bibl_tr_t1400, biblical_authority__conciliar_reading, theater_ratio, 1400, 0.28).
narrative_ontology:measurement(bibl_tr_t1700, biblical_authority__conciliar_reading, theater_ratio, 1700, 0.3).

% Extraction over time
narrative_ontology:measurement(bibl_be_t0, biblical_authority__conciliar_reading, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(bibl_be_t300, biblical_authority__conciliar_reading, base_extractiveness, 300, 0.32).
narrative_ontology:measurement(bibl_be_t700, biblical_authority__conciliar_reading, base_extractiveness, 700, 0.38).
narrative_ontology:measurement(bibl_be_t1000, biblical_authority__conciliar_reading, base_extractiveness, 1000, 0.4).
narrative_ontology:measurement(bibl_be_t1400, biblical_authority__conciliar_reading, base_extractiveness, 1400, 0.41).
narrative_ontology:measurement(bibl_be_t1700, biblical_authority__conciliar_reading, base_extractiveness, 1700, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(bibl_su_t0, biblical_authority__conciliar_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(bibl_su_t300, biblical_authority__conciliar_reading, suppression_requirement, 300, 0.55).
narrative_ontology:measurement(bibl_su_t700, biblical_authority__conciliar_reading, suppression_requirement, 700, 0.45).
narrative_ontology:measurement(bibl_su_t1000, biblical_authority__conciliar_reading, suppression_requirement, 1000, 0.4).
narrative_ontology:measurement(bibl_su_t1400, biblical_authority__conciliar_reading, suppression_requirement, 1400, 0.38).
narrative_ontology:measurement(bibl_su_t1700, biblical_authority__conciliar_reading, suppression_requirement, 1700, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(biblical_authority__conciliar_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(biblical_authority__conciliar_reading, 0.1).
narrative_ontology:affects_constraint(biblical_authority__conciliar_reading, sola_scriptura_reading).
narrative_ontology:affects_constraint(biblical_authority__conciliar_reading, tradition_scripture_reading).

% DUAL FORMULATION NOTE:
% This story, sola_scriptura_reading, and tradition_scripture_reading form a constraint family instantiating the three major readings of the biblical_authority kernel. Each reading carries its own ε: sola_scriptura_reading is expected to show lower institutional extraction but higher fragmentation/interpretive-chaos costs; tradition_scripture_reading (magisterial) is expected to show higher concentrated extraction (single office) but lower fragmentation; this conciliar_reading sits structurally between them — moderate extraction, moderate fragmentation, diffused across an episcopal collegium rather than concentrated in one see. All three should link to each other via affects_constraints, since a shift in one reading's institutional standing (e.g. ecumenical rapprochement, schism) exerts structural pressure on the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
