% ============================================================================
% CONSTRAINT STORY: quran_9_5_scope__progressive_synthesis
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_quran_9_5_scope__progressive_synthesis, []).

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
 *   constraint_id: quran_9_5_scope__progressive_synthesis
 *   human_readable: Verse 9:5 Scope Settlement — Progressive Synthesis Reading (Time-Bound Directive)
 *   domain: religious/hermeneutical/political_theological
 *
 * SUMMARY:
 *   Under the progressive_synthesis reading, Verse 9:5 issues no standing
 *   directive to anyone: it was a time-bound seventh-century political
 *   directive, expired with its addressees, and the Quranic ethical
 *   trajectory governs in its place. The operative structure this story
 *   classifies is therefore not the verse's directive but the settlement that
 *   fixes that scope — the certified reading, its administrative machinery,
 *   and its incidence on the parties of the interpretive field. The
 *   settlement solves a real collective problem (an uncertified scope
 *   question that regenerates security crises and loyalty contests at each
 *   invocation) while dispossessing the structures whose warrant rests on the
 *   verse's continuing force, and it is held in place by active enforcement
 *   rather than unanimous conviction. KEY AGENTS (by structural
 *   relationship): - textualist_authority_structures: Primary target
 *   (organized/identity_locked) — bears the settlement's stripping of
 *   doctrinal warrant - militant_enforcement_networks: Secondary target
 *   (organized/identity_locked) — bears proscription and armed enforcement
 *   directly - state_religious_administrations: Agenda-setter and principal
 *   recipient (institutional/constrained) — administers the settlement and
 *   collects the transferred authority - secular_pluralist_frameworks:
 *   Primary beneficiary (institutional/mobile) — receives the defused-warrant
 *   dividend - muslim_minority_communities: Beneficiary with cost exposure
 *   (organized/constrained) — gains civic legibility, absorbs each
 *   resurgence's shocks - non_muslim_minority_populations: Silent beneficiary
 *   (powerless/trapped) — equal citizenship rides on the settlement holding -
 *   reformist_quran_scholarship: Beneficiary-intellectual (moderate/mobile) —
 *   supplies the settlement's arguments and collects the careers -
 *   ordinary_believers: Distributed beneficiary (powerless/identity_locked) —
 *   receives a civically livable scripture - independent_dissident_preachers:
 *   Excluded voice (organized/identity_locked) — monitored and refuted, never
 *   seated - comparative_jurisprudence_observers: Analytical observer
 *   (analytical/analytical) — sees the whole structure from outside
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(quran_9_5_scope__progressive_synthesis, 0.72).
domain_priors:suppression_score(quran_9_5_scope__progressive_synthesis, 0.55).
domain_priors:theater_ratio(quran_9_5_scope__progressive_synthesis, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(quran_9_5_scope__progressive_synthesis, extractiveness, 0.72).
narrative_ontology:constraint_metric(quran_9_5_scope__progressive_synthesis, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(quran_9_5_scope__progressive_synthesis, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(quran_9_5_scope__progressive_synthesis, accessibility_collapse, 0.52).
narrative_ontology:constraint_metric(quran_9_5_scope__progressive_synthesis, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(quran_9_5_scope__progressive_synthesis, tangled_rope).
narrative_ontology:human_readable(quran_9_5_scope__progressive_synthesis, "Verse 9:5 Scope Settlement — Progressive Synthesis Reading (Time-Bound Directive)").
narrative_ontology:topic_domain(quran_9_5_scope__progressive_synthesis, "religious/hermeneutical/political_theological").

domain_priors:requires_active_enforcement(quran_9_5_scope__progressive_synthesis).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(quran_9_5_scope__progressive_synthesis, '251d4638-c5f4-44bc-9f87-1761c2a2b277').
narrative_ontology:cs_kernel_codification('251d4638-c5f4-44bc-9f87-1761c2a2b277', fixed_text).
narrative_ontology:cs_authority_grounding('251d4638-c5f4-44bc-9f87-1761c2a2b277', lineage).
narrative_ontology:cs_interpretation_layer_present('251d4638-c5f4-44bc-9f87-1761c2a2b277').
narrative_ontology:cs_reading_relation('251d4638-c5f4-44bc-9f87-1761c2a2b277', quran_9_5_scope__abrogating_universal, forecloses).
narrative_ontology:cs_reading_relation('251d4638-c5f4-44bc-9f87-1761c2a2b277', quran_9_5_scope__contextual_defensive, influences).
narrative_ontology:cs_axiom('251d4638-c5f4-44bc-9f87-1761c2a2b277', foundational, directive_force_indexed_to_occasion).
narrative_ontology:cs_axiom_status(directive_force_indexed_to_occasion, holdable).
narrative_ontology:cs_axiom_grounding('251d4638-c5f4-44bc-9f87-1761c2a2b277', directive_force_indexed_to_occasion, conventional).
narrative_ontology:cs_axiom('251d4638-c5f4-44bc-9f87-1761c2a2b277', foundational, ethical_trajectory_governs_particular_commands).
narrative_ontology:cs_axiom_status(ethical_trajectory_governs_particular_commands, holdable).
narrative_ontology:cs_axiom_grounding('251d4638-c5f4-44bc-9f87-1761c2a2b277', ethical_trajectory_governs_particular_commands, theological).
narrative_ontology:cs_reference_frame('251d4638-c5f4-44bc-9f87-1761c2a2b277', time_bound_directive_under_ethical_trajectory).
narrative_ontology:cs_drift_state('251d4638-c5f4-44bc-9f87-1761c2a2b277', contemporary_post_territorial_caliphate, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('251d4638-c5f4-44bc-9f87-1761c2a2b277', '').
narrative_ontology:cs_kernel_id(quran_9_5_scope__progressive_synthesis, quran_9_5_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(quran_9_5_scope__progressive_synthesis, secular_pluralist_frameworks).
narrative_ontology:constraint_beneficiary(quran_9_5_scope__progressive_synthesis, muslim_minority_communities).
narrative_ontology:constraint_beneficiary(quran_9_5_scope__progressive_synthesis, non_muslim_minority_populations).
narrative_ontology:constraint_beneficiary(quran_9_5_scope__progressive_synthesis, ordinary_believers).
narrative_ontology:constraint_beneficiary(quran_9_5_scope__progressive_synthesis, reformist_quran_scholarship).
narrative_ontology:constraint_victim(quran_9_5_scope__progressive_synthesis, textualist_authority_structures).
narrative_ontology:constraint_victim(quran_9_5_scope__progressive_synthesis, militant_enforcement_networks).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(quran_9_5_scope__progressive_synthesis, state_religious_administrations).
narrative_ontology:constraint_victim(quran_9_5_scope__progressive_synthesis, muslim_minority_communities).
narrative_ontology:constraint_vindicates(quran_9_5_scope__progressive_synthesis, occasionalist_revelation_hermeneutic).
narrative_ontology:constraint_vindicates(quran_9_5_scope__progressive_synthesis, ethical_trajectory_supersession_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Ministry-level religious establishments and state-appointed councils of senior scholars that certify clergy, approve curricula, and issue the rulings that fix the verse's scope as a closed historical episode. They administer the settlement day to day and absorb the institutional space — posts, endowments, certification monopolies — that rival interpretive structures previously held. Their exit is limited: their authority exists inside the state apparatus that sponsors the settlement.
narrative_ontology:constraint_stakeholder(quran_9_5_scope__progressive_synthesis, state_religious_administrations, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(quran_9_5_scope__progressive_synthesis, state_religious_administrations, beneficiary).

% Constitutional states, international legal orders, and interfaith institutions that receive the settlement's central dividend: a mainstream reading under which no standing scriptural command targets them. They do not administer the settlement; they condition cooperation, security partnership, and civic inclusion on its holding, and fund its defense through counter-extremism and diplomatic programming.
narrative_ontology:constraint_stakeholder(quran_9_5_scope__progressive_synthesis, secular_pluralist_frameworks, beneficiary,
    institutional, generational, mobile, global).

% Diaspora and minority Muslim populations whose civic legibility improves when the verse reads as closed history: employers, neighbors, and security services treat their scripture as compatible with citizenship. The same communities absorb the shocks of every literalist resurgence — surveillance, loyalty tests, harassment — since each revival reattaches the verse's threat to them regardless of their own reading. Leaving either their tradition or their host societies carries costs they generally cannot pay.
narrative_ontology:constraint_stakeholder(quran_9_5_scope__progressive_synthesis, muslim_minority_communities, beneficiary,
    organized, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(quran_9_5_scope__progressive_synthesis, muslim_minority_communities, payer).

% Religious minorities living in Muslim-majority societies, whose equal citizenship and physical security depend on the verse remaining de-operationalized. They have the largest stake in the settlement's holding, the least voice in its administration, and no realistic exit from the jurisdictions where the question is decided.
narrative_ontology:constraint_stakeholder(quran_9_5_scope__progressive_synthesis, non_muslim_minority_populations, beneficiary,
    powerless, generational, trapped, regional).

% Abrogationist academies, hadith-centered juristic networks, and transnational movements whose institutional warrant rests on the verse's continuing legal force. The settlement strips their core doctrinal asset: state certification excludes their readings, curricula omit their chains of argument, and their claim is reclassified as extremism rather than jurisprudence. Their identity is fused with the claim — abandoning it dissolves the structure, so they contest rather than exit.
narrative_ontology:constraint_stakeholder(quran_9_5_scope__progressive_synthesis, textualist_authority_structures, payer,
    organized, generational, identity_locked, global).

% Armed movements that operationalize the verse as recruitment warrant and governing program. The settlement criminalizes their central citation, and state enforcement — proscription, battlefield defeat, prison deradicalization — aims squarely at them. Each wave of enforcement replenishes their martyrdom narratives, and their operational tempo follows an insurgency cycle rather than the settlement's administrative calendar.
narrative_ontology:constraint_stakeholder(quran_9_5_scope__progressive_synthesis, militant_enforcement_networks, payer,
    organized, immediate, identity_locked, global).

% Academic and seminary scholars who produce the philological and historical case that the verse addressed specific seventh-century treaty-breakers and expired with them. Careers, chairs, and publication fields concentrate around the settlement's premises; the same scholarship supplies the ruling councils with citable arguments. Individual scholars can move between institutions and countries with relative ease.
narrative_ontology:constraint_stakeholder(quran_9_5_scope__progressive_synthesis, reformist_quran_scholarship, beneficiary,
    moderate, biographical, mobile, global).
narrative_ontology:stakeholder_secondary_role(quran_9_5_scope__progressive_synthesis, reformist_quran_scholarship, agenda_setter).

% Lay Muslims who receive a scripture narratable as morally progressive and civically compatible. They hold no administrative role and collect the settlement passively, though interpretive reversals across generations — grandparents taught one scope, grandchildren another — impose quiet costs of trust and continuity. Membership in the faith community is not something most can or would exit.
narrative_ontology:constraint_stakeholder(quran_9_5_scope__progressive_synthesis, ordinary_believers, beneficiary,
    powerless, biographical, identity_locked, global).

% Unaffiliated preachers and online instructors outside state certification who continue teaching the verse's continuing force. They are not seated in the councils that fix the official scope; they appear in the settlement's paperwork only as objects of monitoring and refutation. Their objection — that political convenience has replaced exegetical method — reaches their audiences directly and the official record hardly at all.
narrative_ontology:constraint_stakeholder(quran_9_5_scope__progressive_synthesis, independent_dissident_preachers, excluded,
    organized, immediate, identity_locked, global).

% Scholars of comparative law and religious studies who track the settlement across jurisdictions: which states certify which scopes, how minority communities fare under each, and how the interpretive contest migrates between courts, curricula, and conflicts. They hold no stake in any reading prevailing and can see the whole structure from outside.
narrative_ontology:constraint_stakeholder(quran_9_5_scope__progressive_synthesis, comparative_jurisprudence_observers, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(quran_9_5_scope__progressive_synthesis, state_religious_administrations).
narrative_ontology:fixing_cost_class(quran_9_5_scope__progressive_synthesis, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Settles the scope of a foundational scriptural text so that Muslim legal, educational, and civic life can cohere internally and with pluralist state order: one certified reading replaces a per-preacher ambiguity that otherwise regenerates security crises, loyalty contests, and communal suspicion each time the verse is invoked.
% TRANSFER_FUNCTION: Moves interpretive authority and institutional position from textualist authority structures to state religious administrations and reformist scholarship; moves a security assurance — the defusing of the verse's martial warrant — between Muslim communities and majority societies in both directions; moves enforcement attention onto dissident textualist preaching.
% ABSENT_VOICES: Independent dissident preachers and the rank-and-file of abrogationist currents are administered by the settlement but not seated in it: certification councils decide the official scope, and unaffiliated textualists enter the record only as monitoring objects. Non-Muslim minorities in Muslim-majority societies — the parties with the largest stake in the verse staying de-operationalized — likewise hold no seat in the councils that decide it. The verse's original addressees, the seventh-century treaty-breaking tribes, are absent by fourteen centuries.
% DISAPPEARANCE_RATIONALE: If the settlement dissolved overnight — if the verse's scope reverted to open contest with no certified answer — minority-community civic arrangements would destabilize first, as suspicion mechanisms re-arm; counter-extremism architectures built on the defused reading would lose their scriptural ally; textualist structures would regain contested ground they currently hold only extra-institutionally; and interfaith and security cooperation conditioned on the reading would renegotiate. The rearrangement would be uneven across jurisdictions, sharpest where state certification is weakest.
% FOUNDING_PROBLEM: The collision between a seventh-century wartime directive and the modern pluralist state: Muslim-majority societies entering the nation-state system, and Muslim minorities living as citizens under non-Muslim majorities, required a reading of Verse 9:5 that did not stand in permanent contradiction to treaties, citizenship, and religious liberty.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the settlement's beneficiaries by the historical record of the late-colonial and post-independence period — Ottoman and British-Indian juridical debates over the martial verses, the 1924 abolition of the caliphate's jurisprudential apparatus, and the mid-century nationalization of religious institutions — and, negatively, by the textualist structures themselves, whose continued organization against the settlement attests that they regard the underlying question as unsettled. Security-service assessments also attest the problem's liveness, though they sit inside the beneficiary set and are weighted accordingly.
narrative_ontology:disappearance_verdict(quran_9_5_scope__progressive_synthesis, world_rearranges).
narrative_ontology:founding_problem_status(quran_9_5_scope__progressive_synthesis, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(quran_9_5_scope__progressive_synthesis, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(quran_9_5_scope__progressive_synthesis, 'none', 1).
narrative_ontology:epsilon_provenance(quran_9_5_scope__progressive_synthesis, 0.72, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(quran_9_5_scope__progressive_synthesis_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(quran_9_5_scope__progressive_synthesis, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(quran_9_5_scope__progressive_synthesis_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Metric referents are stated per series because this is a kernel reading. Base extractiveness (0.72) is authored over the standing arrangement under contest — the residual regime in which the verse's binding force is claimed and intermittently operationalized — as this reading assesses it: a scriptural warrant converted into standing threat against non-Muslims, a mobilization asset for armed movements, a loyalty-test liability for minority communities, and a legitimacy rent for the structures that claim it. The theater_ratio and suppression_requirement series track the settlement itself: its performative share (certification ritual, refutation programming that documents activity more than it changes minds) climbs from 0.08 to 0.30 as administration matures, and its enforcement intensity against literalist resurgence rises from 0.06 to 0.55 as state religious administration consolidated mid-century and ratcheted sharply after 2001. Accessibility_collapse (0.52) is partial: the literalist alternative persists in unadministered religious markets and resurges cyclically rather than collapsing. Resistance (0.58) is organized and durable — academies, militant networks, and dissident preaching contest the settlement continuously. The base_extractiveness series is deliberately non-monotonic: state-driven decay of the binding-force regime runs 1880–1990 (0.86 to 0.61), then revivalist re-operationalization lifts it through the insurgent cycle (0.66 to 0.72 by 2026) — one full V-cycle across eight shared grid points, with the endogeneity question routed to the enforcement_reactance_cycle omega. The claimed type is authored independently of the metrics: from this reading's seat the settlement is a tangled rope — genuine coordination (a certified scope replacing a per-preacher ambiguity) with asymmetric incidence (textualist structures pay, pluralist frameworks gain) held up by active enforcement. Where the engine's computed type diverges from that claim — the elevated epsilon invites a snare-flavored computation — the divergence is the datum, not an error: the epsilon belongs to the contested regime, not to the settlement.
 *
 * PERSPECTIVAL GAP:
 *   The paying seat and the beneficiary seats should compute differently. From the textualist seat the settlement is dispossession dressed as moderation: political authority reclassifying orthodox jurisprudence as extremism and transferring its institutional space to compliant rivals — experienced as persecution of interpretation, with identity-locked exit leaving intensification as the only recourse. From the pluralist and minority seats the same structure is the price of civic peace finally paid: scripture made narratable without threat. From the administrative seat it is routine governance — certification, curriculum, refutation — with little phenomenology of taking at all. Coalition dynamics run against the payer side's interests in a specific way: textualist structures are organized, transnational, and ideologically fused, so their response to the settlement is escalation rather than exit, which is why measured resistance stays high while the costs borne by that seat concentrate rather than diffuse.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive low directionalities. secular_pluralist_frameworks (institutional, mobile) sit nearest the subsidy end — the settlement insures them against a scriptural warrant targeting them, at the cost of programming budgets. muslim_minority_communities are genuinely dual-positioned (beneficiary primary, payer secondary): net gainers whose gains shrink with each resurgence, placing them nearer symmetric than pure beneficiaries. non_muslim_minority_populations and ordinary_believers collect passively with no administrative role. Victim declarations drive high directionalities: textualist_authority_structures (identity_locked) sit nearest the full-target end — the settlement takes precisely the asset their existence is organized around — and militant_enforcement_networks bear its armed enforcement directly. state_religious_administrations derive intermediate-low d as agenda-setters who also collect; reformist_quran_scholarship similarly low as beneficiary-intellectuals with portable exit. No directionality overrides were needed: the beneficiary/victim structure plus exit differentiation produces the correct ordering without correction.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — a seventh-century wartime directive colliding with the modern pluralist state — is live, not dead: it regenerates wherever literalist resurgence meets pluralist order, so the settlement's mandate has not outlived its function and no mandatrophy resolution is declared. The classification guards against mislabeling in both directions. Reading the settlement as pure coordination would miss the real dispossession its enforcement performs on textualist seats; reading it as pure extraction would miss the genuine collective-action problem it solves — an uncertified scope question that regenerates loyalty crises and security shocks at each invocation. The theater_ratio series is the early-warning line: if the founding problem were ever genuinely retired through universal contextualist uptake while certification machinery kept expanding, the settlement would drift toward inertial performance. The series' slow climb from 0.08 to 0.30 is the quantity to watch, and the mismatch consumer should pair founding_problem_status=live against the theater trajectory rather than against the origin narrative.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_position,
    'This constraint is one reading of the quran_9_5_scope kernel — the progressive_synthesis reading. What would each sibling reading change structurally if adopted instead?',
    'Not resolvable by data: the choice of reading is the contest itself. Cross-reading comparison of the three compiled stories locates the disagreement — under abrogating_universal the verse''s targets are the victims and the verse is a standing command; under contextual_defensive the verse stays defensively operative with treaty priority; under this reading the verse exits operative space and the textualist claimants become the paying party.',
    'Reading choice determines the entire incidence structure: beneficiaries, victims, and the enforcement object all swap partitions across the three stories. Classification outputs are not comparable across readings without this locator.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_position, conceptual, 'Committer-frame locator: this story instantiates the progressive_synthesis reading of the quran_9_5_scope kernel; siblings are abrogating_universal and contextual_defensive.').

omega_variable(
    temporal_index_disagreement_location,
    'Where exactly do the readings disagree — on the verse''s addressee scope, on its temporal validity, or on the abrogation hierarchy between 9:5 and the peaceful verses?',
    'Philological reconstruction of each reading''s committed position on the three separable axes (addressee, duration, naskh direction); a reading can be specified as a vector of axis values and the disagreement mapped onto whichever axis carries the load.',
    'If the disagreement reduces to the abrogation axis alone, contextual_defensive and this reading are variants of one constraint and should merge; if the temporal-validity axis is load-bearing, three separate constraints are required, as authored here.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(temporal_index_disagreement_location, conceptual, 'Locates the structural element on which the sibling readings of the kernel actually diverge.').

omega_variable(
    trajectory_supersession_principled_or_accommodative,
    'Is the claim that the Quranic ethical trajectory supersedes the verse''s literal application a principled hermeneutic internal to juristic method, or a post-hoc accommodation to modern political necessity?',
    'Philological and methodological analysis: whether the Quran''s own later material (the protection grant of 9:6, the treaty-priority argument of 9:13, the combat limits of 2:190) and the classical juristic categories admit trajectory-based reversal of a specific directive, or whether the move requires importing an external standard.',
    'If principled, the settlement rests on arguable method and its enforcement burden stays moderate; if accommodative, textualist counter-argument strengthens considerably and enforcement must carry more of the settlement''s weight, pushing measured suppression upward over time.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(trajectory_supersession_principled_or_accommodative, empirical, 'Whether the settlement''s core hermeneutic move is methodologically self-standing.').

omega_variable(
    settlement_durability_without_state_enforcement,
    'Can the settlement hold in religious markets the state does not administer — unregulated mosques, online instruction, prison chaplaincy — or does it require continuous enforcement wherever it operates?',
    'Compare literalist re-operationalization rates in state-administered versus unadministered religious markets across jurisdictions with comparable populations.',
    'If enforcement-dependent, the settlement''s suppressive component is structural and permanent; if self-sustaining through community uptake, the coordination component dominates and the suppression series overstates its steady-state character.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(settlement_durability_without_state_enforcement, empirical, 'Whether the settlement''s holding depends on continuous administrative enforcement or survives in unadministered religious markets.').

omega_variable(
    victim_designation_reading_indexed,
    'Are textualist authority structures victims of the settlement, or losers of a claim this reading assigns no legitimate standing — and does the difference matter to classification?',
    'Not resolvable from inside this reading: it turns on whether the verse''s continuing binding force is granted any legitimate standing, which is precisely the question the kernel contests. Cross-reading comparison is the only lever available.',
    'If the textualist claim has legitimate standing, the settlement''s taking from it is real extraction and the hybrid coordination/extraction reading of this story firms up; if not, the victim entry records the disposition of an illegitimate claim and effective extraction drops toward the coordination floor.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(victim_designation_reading_indexed, preference, 'The victim designation is reading-indexed; its classification consequence depends on standing granted to the textualist claim.').

omega_variable(
    enforcement_reactance_cycle,
    'Does the post-2001 revival of the verse''s operational invocation react to the settlement''s own enforcement intensity — suppression feeding martyrdom narrative and literalist identity reinforcement — or is it driven by exogenous geopolitical shocks?',
    'Cross-jurisdictional comparison of enforcement style against re-invocation incidence, holding geopolitical exposure constant; jurisdictions enforcing through inclusion and certification versus proscription and policing diverge predictably under the reactance hypothesis.',
    'If reactance-driven, raising enforcement feeds the cycle it targets and the settlement''s enforcement is partially self-undermining; the measured extraction series would then be partly endogenous to enforcement policy rather than an independent input.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(enforcement_reactance_cycle, empirical, 'Whether the observed cyclical re-operationalization is endogenous to the settlement''s enforcement or exogenous.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(quran_9_5_scope__progressive_synthesis, 1880, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qura_tr_t1880, quran_9_5_scope__progressive_synthesis, theater_ratio, 1880, 0.08).
narrative_ontology:measurement_basis(qura_tr_t1880, observed).
narrative_ontology:measurement(qura_tr_t1910, quran_9_5_scope__progressive_synthesis, theater_ratio, 1910, 0.11).
narrative_ontology:measurement_basis(qura_tr_t1910, observed).
narrative_ontology:measurement(qura_tr_t1940, quran_9_5_scope__progressive_synthesis, theater_ratio, 1940, 0.15).
narrative_ontology:measurement_basis(qura_tr_t1940, observed).
narrative_ontology:measurement(qura_tr_t1965, quran_9_5_scope__progressive_synthesis, theater_ratio, 1965, 0.19).
narrative_ontology:measurement_basis(qura_tr_t1965, observed).
narrative_ontology:measurement(qura_tr_t1990, quran_9_5_scope__progressive_synthesis, theater_ratio, 1990, 0.23).
narrative_ontology:measurement_basis(qura_tr_t1990, observed).
narrative_ontology:measurement(qura_tr_t2005, quran_9_5_scope__progressive_synthesis, theater_ratio, 2005, 0.27).
narrative_ontology:measurement_basis(qura_tr_t2005, observed).
narrative_ontology:measurement(qura_tr_t2015, quran_9_5_scope__progressive_synthesis, theater_ratio, 2015, 0.29).
narrative_ontology:measurement_basis(qura_tr_t2015, observed).
narrative_ontology:measurement(qura_tr_t2026, quran_9_5_scope__progressive_synthesis, theater_ratio, 2026, 0.3).
narrative_ontology:measurement_basis(qura_tr_t2026, observed).

% Extraction over time
narrative_ontology:measurement(qura_be_t1880, quran_9_5_scope__progressive_synthesis, base_extractiveness, 1880, 0.86).
narrative_ontology:measurement_basis(qura_be_t1880, observed).
narrative_ontology:measurement(qura_be_t1910, quran_9_5_scope__progressive_synthesis, base_extractiveness, 1910, 0.82).
narrative_ontology:measurement_basis(qura_be_t1910, observed).
narrative_ontology:measurement(qura_be_t1940, quran_9_5_scope__progressive_synthesis, base_extractiveness, 1940, 0.75).
narrative_ontology:measurement_basis(qura_be_t1940, observed).
narrative_ontology:measurement(qura_be_t1965, quran_9_5_scope__progressive_synthesis, base_extractiveness, 1965, 0.67).
narrative_ontology:measurement_basis(qura_be_t1965, observed).
narrative_ontology:measurement(qura_be_t1990, quran_9_5_scope__progressive_synthesis, base_extractiveness, 1990, 0.61).
narrative_ontology:measurement_basis(qura_be_t1990, observed).
narrative_ontology:measurement(qura_be_t2005, quran_9_5_scope__progressive_synthesis, base_extractiveness, 2005, 0.66).
narrative_ontology:measurement_basis(qura_be_t2005, observed).
narrative_ontology:measurement(qura_be_t2015, quran_9_5_scope__progressive_synthesis, base_extractiveness, 2015, 0.7).
narrative_ontology:measurement_basis(qura_be_t2015, observed).
narrative_ontology:measurement(qura_be_t2026, quran_9_5_scope__progressive_synthesis, base_extractiveness, 2026, 0.72).
narrative_ontology:measurement_basis(qura_be_t2026, observed).

% Suppression requirement over time
narrative_ontology:measurement(qura_su_t1880, quran_9_5_scope__progressive_synthesis, suppression_requirement, 1880, 0.06).
narrative_ontology:measurement_basis(qura_su_t1880, observed).
narrative_ontology:measurement(qura_su_t1910, quran_9_5_scope__progressive_synthesis, suppression_requirement, 1910, 0.09).
narrative_ontology:measurement_basis(qura_su_t1910, observed).
narrative_ontology:measurement(qura_su_t1940, quran_9_5_scope__progressive_synthesis, suppression_requirement, 1940, 0.16).
narrative_ontology:measurement_basis(qura_su_t1940, observed).
narrative_ontology:measurement(qura_su_t1965, quran_9_5_scope__progressive_synthesis, suppression_requirement, 1965, 0.28).
narrative_ontology:measurement_basis(qura_su_t1965, observed).
narrative_ontology:measurement(qura_su_t1990, quran_9_5_scope__progressive_synthesis, suppression_requirement, 1990, 0.37).
narrative_ontology:measurement_basis(qura_su_t1990, observed).
narrative_ontology:measurement(qura_su_t2005, quran_9_5_scope__progressive_synthesis, suppression_requirement, 2005, 0.5).
narrative_ontology:measurement_basis(qura_su_t2005, observed).
narrative_ontology:measurement(qura_su_t2015, quran_9_5_scope__progressive_synthesis, suppression_requirement, 2015, 0.54).
narrative_ontology:measurement_basis(qura_su_t2015, observed).
narrative_ontology:measurement(qura_su_t2026, quran_9_5_scope__progressive_synthesis, suppression_requirement, 2026, 0.55).
narrative_ontology:measurement_basis(qura_su_t2026, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(quran_9_5_scope__progressive_synthesis, enforcement_mechanism).
narrative_ontology:affects_constraint(quran_9_5_scope__progressive_synthesis, quran_9_5_scope__abrogating_universal).
narrative_ontology:affects_constraint(quran_9_5_scope__progressive_synthesis, quran_9_5_scope__contextual_defensive).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'the scope of Verse 9:5' covers three structurally distinct claims with different epsilon referents and largely disjoint victim sets — abrogating_universal (upstream classical doctrine; the verse's targets are its victims), contextual_defensive (middle position; treaty priority, defensively operative scope), progressive_synthesis (this file; the verse exits operative space and the textualist claimants are the paying party). Upstream/downstream structure: abrogating_universal is the doctrine both other readings argue against and serves as the classical baseline they cite or rebut; progressive_synthesis exerts downstream pressure on contextual_defensive by absorbing its constituency — once supersession is admitted for 9:5, the narrower defensive-validity claim loses its anchor — without logically excluding it. All three files link one another via affects_constraints per the family rule.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
