% ============================================================================
% CONSTRAINT STORY: vatican_ii_doctrinal_authority__rupture_traditionalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_vatican_ii_doctrinal_authority__rupture_traditionalist_reading, []).

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
 *   constraint_id: vatican_ii_doctrinal_authority__rupture_traditionalist_reading
 *   human_readable: Post-Conciliar Doctrinal Settlement as Assessed by the Rupture Reading
 *   domain: ecclesiology/institutional_history/hermeneutics
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the contested kernel
 *   'vatican_ii_doctrinal_authority': the rupture-traditionalist reading,
 *   which holds that the conciliar corpus marks a break with the prior
 *   irreformable magisterium and that its ambiguities are compromise-induced
 *   defects rather than fecund openness. The epsilon referent is the standing
 *   arrangement under contest — the post-conciliar doctrinal settlement as it
 *   actually operates (textual authority, implementation machinery,
 *   permission architecture around the inherited liturgy) — assessed by this
 *   reading's own lights. Per OQ-26 the values are reading-indexed over that
 *   fixed referent: the continuity reading authors different epsilon over the
 *   same arrangement, and the arrangement this reading would install is never
 *   the referent. The claim/metric gap is deliberate: the settlement is
 *   CLAIMED here as tangled_rope (genuine coordination function plus
 *   asymmetric extraction plus active enforcement) while the metrics describe
 *   what this seat observes; the engine computes per-seat classifications
 *   from the structural data and the divergence is the datum.
 *
 * KEY AGENTS:
 *   - roman_curia_and_magisterium: agenda-setter (institutional/identity_locked) — administers the settlement, collects interpretive discretion and disciplinary leverage
 *   - progressive_clergy_and_theologians: primary beneficiary (powerful/arbitrage) — converts textual latitude into careers and programs
 *   - national_episcopal_conferences: secondary beneficiary with agenda-setting reach (institutional/constrained) — collects standing from collegial structures
 *   - ordinary_parish_laity: dual-positioned (moderate/constrained) — collects vernacular access, bears catechetical and disciplinary churn
 *   - traditional_liturgy_laity: primary target (powerless/identity_locked) — bears displacement of inherited worship, exit fused with identity loss
 *   - traditionalist_clergy_and_seminarians: target (moderate/constrained) — canonical standing gated on continued toleration
 *   - missionary_religious_orders: target (organized/constrained) — bears charism-thinning and vocational collapse attributed to the settlement
 *   - sedevacantist_communities: excluded voice (powerless/trapped) — strongest objection rendered unspeakable by secession
 *   - council_history_scholars: analytical observer (institutional/analytical) — supplies archival ground truth that binds no faction
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, 0.78).
domain_priors:suppression_score(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, 0.72).
domain_priors:theater_ratio(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, 0.36).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, theater_ratio, 0.36).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, tangled_rope).
narrative_ontology:human_readable(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, "Post-Conciliar Doctrinal Settlement as Assessed by the Rupture Reading").
narrative_ontology:topic_domain(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, "ecclesiology/institutional_history/hermeneutics").

domain_priors:requires_active_enforcement(vatican_ii_doctrinal_authority__rupture_traditionalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, '72b0f3e3-1898-4f18-a160-66d3aa4efb93').
narrative_ontology:cs_kernel_codification('72b0f3e3-1898-4f18-a160-66d3aa4efb93', fixed_text).
narrative_ontology:cs_authority_grounding('72b0f3e3-1898-4f18-a160-66d3aa4efb93', lineage).
narrative_ontology:cs_interpretation_layer_present('72b0f3e3-1898-4f18-a160-66d3aa4efb93').
narrative_ontology:cs_reading_relation('72b0f3e3-1898-4f18-a160-66d3aa4efb93', vatican_ii_doctrinal_authority__continuity_reading, forecloses).
narrative_ontology:cs_reading_relation('72b0f3e3-1898-4f18-a160-66d3aa4efb93', vatican_ii_doctrinal_authority__rupture_progressive_reading, forecloses).
narrative_ontology:cs_axiom('72b0f3e3-1898-4f18-a160-66d3aa4efb93', foundational, conciliar_novelties_contradict_prior_magisterium).
narrative_ontology:cs_axiom_status(conciliar_novelties_contradict_prior_magisterium, holdable).
narrative_ontology:cs_axiom_grounding('72b0f3e3-1898-4f18-a160-66d3aa4efb93', conciliar_novelties_contradict_prior_magisterium, empirically_contingent).
narrative_ontology:cs_axiom('72b0f3e3-1898-4f18-a160-66d3aa4efb93', foundational, doctrinal_ambiguity_is_defective).
narrative_ontology:cs_axiom_status(doctrinal_ambiguity_is_defective, holdable).
narrative_ontology:cs_axiom_grounding('72b0f3e3-1898-4f18-a160-66d3aa4efb93', doctrinal_ambiguity_is_defective, deontological).
narrative_ontology:cs_axiom('72b0f3e3-1898-4f18-a160-66d3aa4efb93', secondary, implementation_failures_trace_to_textual_flaws).
narrative_ontology:cs_axiom_status(implementation_failures_trace_to_textual_flaws, holdable).
narrative_ontology:cs_axiom_grounding('72b0f3e3-1898-4f18-a160-66d3aa4efb93', implementation_failures_trace_to_textual_flaws, instrumental).
narrative_ontology:cs_reference_frame('72b0f3e3-1898-4f18-a160-66d3aa4efb93', unbroken_preconciliar_tradition).
narrative_ontology:cs_drift_state('72b0f3e3-1898-4f18-a160-66d3aa4efb93', contemporary_postconciliar_era, gap(axiom_overriding, severe, false)).
narrative_ontology:cs_created_at('72b0f3e3-1898-4f18-a160-66d3aa4efb93', '').
narrative_ontology:cs_kernel_id(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, vatican_ii_doctrinal_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, progressive_clergy_and_theologians).
narrative_ontology:constraint_beneficiary(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, national_episcopal_conferences).
narrative_ontology:constraint_beneficiary(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, ordinary_parish_laity).
narrative_ontology:constraint_victim(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, traditional_liturgy_laity).
narrative_ontology:constraint_victim(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, traditionalist_clergy_and_seminarians).
narrative_ontology:constraint_victim(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, missionary_religious_orders).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, ordinary_parish_laity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Promulgates and administers the conciliar settlement: issues implementing documents, rules on liturgical translations and permissions, appoints bishops, and adjudicates what the council texts require. Holds discretionary authority over meaning and discipline that the settlement's interpretive openness concentrates at the center; leaving the office means abdication, which the office itself renders extraordinary.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, roman_curia_and_magisterium, agenda_setter,
    institutional, generational, identity_locked, global).

% Staff seminaries, liturgy commissions, chanceries, and editorial platforms; build careers and programs on the settlement's interpretive latitude. Move fluidly between academic posts, diocesan offices, and publications; their standing depends on the texts remaining open-ended enough to authorize ongoing programmatic work.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, progressive_clergy_and_theologians, beneficiary,
    powerful, biographical, arbitrage, continental).

% Acquired standing through the settlement's collegial structures: issue joint documents, control national liturgical translations, and mediate between Rome and local churches. Withdrawing from the conference system would forfeit their collective voice, so participation is effectively compulsory.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, national_episcopal_conferences, beneficiary,
    institutional, generational, constrained, continental).
narrative_ontology:stakeholder_secondary_role(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, national_episcopal_conferences, agenda_setter).

% Received vernacular liturgy, expanded lay roles, and scriptural access; also inherited decades of catechetical flux, shifting disciplines, and parish consolidations. Most remain in parishes; the alternative is disaffiliation, which family ties and sacramental life make costly.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, ordinary_parish_laity, beneficiary,
    moderate, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, ordinary_parish_laity, payer).

% Attached to the pre-conciliar Mass and devotional year; travel long distances, fund chapels, and petition for access that depends on permissions they do not control. Their attachment is fused with family inheritance and devotional identity they will not trade for convenience; losing access means losing the form of worship that constitutes their religious life.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, traditional_liturgy_laity, payer,
    powerless, biographical, identity_locked, global).

% Formed for the older rites; serve in approved institutes, tightly rationed diocesan slots, or non-approved fraternities. Canonical standing, faculties to celebrate, and career progression all hinge on continued toleration; crossing between approved and unapproved structures carries real penalties in either direction.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, traditionalist_clergy_and_seminarians, payer,
    moderate, biographical, constrained, global).

% Congregations built on distinctive habits, rules, and evangelizing identities that thinned sharply after the settlement; vocations collapsed across the post-conciliar decades. The main lever left is refounding on older charisms; the alternative path is absorption or dissolution into wider diocesan structures.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, missionary_religious_orders, payer,
    organized, generational, constrained, global).

% Concluded that the papal line itself failed and broke communion entirely; their strongest form of the objection is thereby barred from official discussion by the very act of making it. They hold no leverage inside the system and seek none.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, sedevacantist_communities, excluded,
    powerless, generational, trapped, global).

% Reconstruct drafting histories, roll-call votes, and minority interventions from the council archives; publish in academic venues outside ecclesial discipline. Their findings supply ammunition to every faction without binding any of them.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, council_history_scholars, observer,
    institutional, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, roman_curia_and_magisterium).
narrative_ontology:fixing_cost_class(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The settlement coordinated a global church's encounter with modernity: one reformed liturgy for the Latin rite instead of regional variation, a common posture toward other Christians and religions, collegial governance linking Rome and local churches, and a shared catechetical vocabulary. Whatever its defects, these were real coordination problems with real pre-existing costs.
% TRANSFER_FUNCTION: Moves interpretive authority and liturgical form. Interpretive discretion over doctrine flows toward the Roman center and toward the academies and commissions that staff implementation; access to the inherited liturgy flows away from all faithful and back only by permission; institutional standing flows from clergy identified with the inherited forms toward clergy identified with the settlement.
% ABSENT_VOICES: The minority council fathers who warned against precisely the ambiguities later exploited, the traditionalist institutes now restricted, and the lay faithful attached to the inherited liturgy would object loudest; they sit outside the interpretive process, heard only when petitioning for permissions. Sedevacantist communities are wholly outside, silenced by their own secession. Their absence makes the settlement's internal unanimity partly an artifact of who was permitted to stay in the room.
% DISAPPEARANCE_RATIONALE: If the settlement vanished overnight, the Latin Church's liturgy, governance, ecumenical posture, and clerical formation would all rearrange: episcopal conferences would lose their standing, translation regimes would collapse back to Rome, the academies built on interpretive latitude would lose their warrant, and the permission architecture around the inherited liturgy would dissolve. Nothing about the current configuration survives the removal.
% FOUNDING_PROBLEM: Built to solve how a globally governed, pre-modern-structured church engages modern pluralism, democratic states, separated Christians, and a laity newly literate and mobile, without schism — the pastoral crisis diagnosed in the 1950s, answered with aggiornamento and ressourcement.
% FOUNDING_PROBLEM_CORROBORATION: Secular historians of the council (archival reconstruction of the preparatory commissions and the pastoral motivations of John XXIII) corroborate that a real diagnosed crisis drove the convocation; non-Catholic ecumenical partners corroborate that the opening was real and consequential. Against this, the minority fathers' published interventions and the traditionalist institutes attest that the diagnosis itself was contested from inside the hall. No seat outside all factions adjudicates the dispute; corroboration is real but factional on both sides.
narrative_ontology:disappearance_verdict(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, 0.78, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(vatican_ii_doctrinal_authority__rupture_traditionalist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(vatican_ii_doctrinal_authority__rupture_traditionalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is high (0.78 at interval end) because, on this reading, the settlement displaces teaching the prior magisterium held irreformable and gates the inherited liturgy behind a permission regime — costs concentrated on identifiable seats while the coordinating benefits diffuse. Suppression (0.72) is a raw structural property, unscaled: persistence rests on canonical restriction, appointment leverage, and faculty control, not on participant preference. Theater (0.36) is moderate: anniversaries, fidelity rhetoric, and curated commemorations consume a growing share of official energy while implementation diverges in both directions from the texts. Accessibility_collapse (0.42) is low-to-moderate because alternatives demonstrably persist — approved institutes, Eastern Catholic rites untouched by the Latin reform, non-approved fraternities — but each carries rising canonical or geographic cost. Resistance (0.70) is high and cross-generational: petitions, institute growth, and quiet non-implementation of restrictions by bishops. The temporal series run on ONE shared grid (seven points, all three metrics at every point). The enforcement trajectory is cyclical rather than monotonic: ratchet (1970 imposition), partial relaxation (1988 indult, 2007 universal permission), renewed ratchet (2021 restrictions) — the cycle tracks enforcement-politics phases, not intermittent reinforcement as an extraction mechanism; the base_properties scalars are measured at interval end, the post-ratchet phase.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute radically different types from identical texts. From the agenda-setter seat the settlement is self-authored continuity — the magisterium wrote it, interprets it, and experiences no extraction. From the payer seats the same machinery operates as displacement enforced by permission. Adherents of the continuity reading — a different constraint, not a seat in this one — experience the same texts as benign development with negligible extraction. The engine derives this divergence from power, exit, and declared position; nothing in the authored claim adjudicates whose experience is correct.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive low directionality: the curia collects discretion (near-full beneficiary), progressive clergy arbitrage latitude (nearest the beneficiary pole — their exit is precisely monetizing the interpretive space), episcopal conferences collect standing. Ordinary laity sit near symmetric: vernacular access against catechetical churn. Victim declarations drive high directionality: traditional-liturgy laity, identity-locked, sit nearest the full-target end — trapped or identity-locked targets always amplify effective extraction; traditionalist clergy are slightly damped by their constrained-but-real exits (approved institutes, migration between jurisdictions); missionary orders bear diffuse generational costs at organized power. Global spatial scope amplifies effective extraction modestly for the targets because verification of uniform application across the whole church is hard, while the center controls the interpretive standard.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — engaging modernity without schism — is disputed rather than dead: this reading holds the diagnosis was partly wrong and the cure iatrogenic; the progressive reading holds the problem live and the cure incomplete. Because status is contested, no mandatrophy verdict follows mechanically; the mismatch consumer watches founding_problem_status x disappearance_verdict for zombie drift. The classification discipline cuts both ways: naming the settlement's real coordination achievements (one liturgy, a common ecumenical posture, functioning collegiality) blocks a pure-extraction mislabel that would erase what the council actually solved; naming the victims and the enforcement machinery blocks a pure-coordination mislabel that would erase who pays. If the founding problem were ever judged dead while the arrangement persisted by inertia alone, the type would drift toward the degraded-inertial category — the cost-asymmetry test (administrator could change it, cost to fix exceeds what it bears) is already visible in the prohibitive fixing_cost authored below.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_position,
    'This constraint is one reading (rupture_traditionalist_reading) of the kernel vatican_ii_doctrinal_authority; what would the sibling readings change structurally if adopted as the operative account?',
    'Cross-file comparison of the sibling stories'' epsilon, victim sets, and computed types over the shared referent; the corpus holds all four readings as separate constraints precisely so this comparison is mechanical.',
    'Under continuity_reading the victim set collapses (no displacement of irreformable teaching occurs) and epsilon drops toward coordination cost; under rupture_progressive_reading the victim set inverts (rigid pre-conciliar formations become the extracted-from party); under composite_overdetermination_reading this story''s single epsilon improperly averages four distinct shifts and should decompose.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_position, conceptual, 'Committer-frame placement: which reading of the kernel this story instantiates and what siblings would restructure.').

omega_variable(
    ambiguity_error_or_development,
    'Are the conciliar texts'' ambiguities compromise-induced defects (this reading''s claim) or deliberate doctrinal openings intended to be completed by subsequent magisterial interpretation?',
    'Drafting-history analysis: commission schemata, relatio summaries, recorded emendations, and the modi rejected or accepted at each stage — archival work of the kind the council-history scholars produce.',
    'If the ambiguities were engineered compromises inserted against minority warnings, this reading''s epsilon stands and its error-thesis strengthens; if they were intentional polyvalence with a planned interpretive completion, part of the measured extraction is the price of the chosen method and the type softens toward coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ambiguity_error_or_development, empirical, 'Whether textual ambiguity is defect or design — the pivotal textual judgment separating this reading from continuity_reading.').

omega_variable(
    text_vs_implementation_causation,
    'Does heterodox post-conciliar implementation trace causally to flaws in the conciliar texts themselves, or to selective misreading by implementers that faithful reading could have prevented?',
    'Compare the promulgated text of each contested passage with the spread of implementations attributed to it, controlling for which implementations the magisterium subsequently corrected versus tolerated.',
    'If the texts compel the heterodox readings, epsilon attaches to the texts and the arrangement as constituted; if the texts permit orthodox readings that implementers ignored, much of the measured extraction belongs to the interpreter layer, and the constraint decomposes into a text-story and an implementation-story.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(text_vs_implementation_causation, empirical, 'Whether the causal chain runs through the texts or through their interpreters.').

omega_variable(
    secularization_confound,
    'How much of the victim-set damage (vocational collapse in missionary orders, liturgical attrition, catechetical drift) is attributable to the conciliar settlement versus concurrent secularization that would have struck regardless?',
    'Natural experiments: Eastern Catholic churches (untouched by the Latin liturgical reform) and comparable non-Catholic bodies across the same decades provide comparison trajectories for vocation and attendance decline.',
    'If secularization explains most of the decline, the settlement''s attributable extraction falls substantially and several victim declarations weaken; if the Eastern comparison shows materially different trajectories, the settlement-specific component is confirmed and epsilon stands.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(secularization_confound, empirical, 'Attribution confound between the settlement and ambient secularization in the victim-set record.').

omega_variable(
    suppression_mechanism_split,
    'Is the suppression experienced by traditional-liturgy laity and traditionalist clergy primarily structural (canonical restriction, permission gating, appointment leverage) or partially internalized (self-censorship, fear of the schism label, fusion of identity with persecution)?',
    'Post-liberalization trajectory: after 2007''s universal permission, did traditionalist demand and institutional confidence expand freely (structural suppression was the binding constraint) or stall (internalized components persist)? The 2021 re-restriction provides a second probe.',
    'If internalized components are substantial, effective suppression exceeds the structural measure and persists even where permissions loosen; if expansion followed liberalization cleanly, the structural measure is accurate and removal of restrictions would largely dissolve the suppressive force.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_split, empirical, 'Structural versus internalized composition of the measured suppression on the target seats.').

omega_variable(
    hermeneutic_coherence,
    'Is the official hermeneutic of renewal-in-continuity a coherent third position that dissolves the rupture/continuity dichotomy, or an unstable synthesis that secretly presupposes one sibling''s premise?',
    'Test the hermeneutic against the hardest cases (religious liberty, the ecumenical ecclesiology): if a principled rule reconciles the texts with the prior magisterium in all of them, the dichotomy was false; if each reconciliation requires an ad hoc move, the hermeneutic reduces to one sibling or the other case by case.',
    'If the hermeneutic is coherent, this reading and continuity_reading are not contradictories after all and the forecloses relations soften toward coexistence; if it is unstable, the kernel genuinely admits only the rival rupture judgments and the family structure simplifies.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(hermeneutic_coherence, conceptual, 'Whether the official mediating hermeneutic is a real third reading or a mask for one sibling — the precise location of the inter-reading disagreement.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, 1962, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vii_rupture_trad_tr_t1962, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, theater_ratio, 1962, 0.1).
narrative_ontology:measurement(vii_rupture_trad_tr_t1970, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, theater_ratio, 1970, 0.24).
narrative_ontology:measurement(vii_rupture_trad_tr_t1980, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, theater_ratio, 1980, 0.31).
narrative_ontology:measurement(vii_rupture_trad_tr_t1988, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, theater_ratio, 1988, 0.29).
narrative_ontology:measurement(vii_rupture_trad_tr_t2007, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, theater_ratio, 2007, 0.27).
narrative_ontology:measurement(vii_rupture_trad_tr_t2021, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, theater_ratio, 2021, 0.33).
narrative_ontology:measurement(vii_rupture_trad_tr_t2025, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, theater_ratio, 2025, 0.36).

% Extraction over time
narrative_ontology:measurement(vii_rupture_trad_be_t1962, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, base_extractiveness, 1962, 0.34).
narrative_ontology:measurement(vii_rupture_trad_be_t1970, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, base_extractiveness, 1970, 0.61).
narrative_ontology:measurement(vii_rupture_trad_be_t1980, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, base_extractiveness, 1980, 0.67).
narrative_ontology:measurement(vii_rupture_trad_be_t1988, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, base_extractiveness, 1988, 0.64).
narrative_ontology:measurement(vii_rupture_trad_be_t2007, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, base_extractiveness, 2007, 0.58).
narrative_ontology:measurement(vii_rupture_trad_be_t2021, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, base_extractiveness, 2021, 0.74).
narrative_ontology:measurement(vii_rupture_trad_be_t2025, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, base_extractiveness, 2025, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(vii_rupture_trad_su_t1962, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, suppression_requirement, 1962, 0.18).
narrative_ontology:measurement(vii_rupture_trad_su_t1970, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, suppression_requirement, 1970, 0.55).
narrative_ontology:measurement(vii_rupture_trad_su_t1980, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, suppression_requirement, 1980, 0.6).
narrative_ontology:measurement(vii_rupture_trad_su_t1988, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, suppression_requirement, 1988, 0.51).
narrative_ontology:measurement(vii_rupture_trad_su_t2007, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, suppression_requirement, 2007, 0.44).
narrative_ontology:measurement(vii_rupture_trad_su_t2021, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, suppression_requirement, 2021, 0.7).
narrative_ontology:measurement(vii_rupture_trad_su_t2025, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, suppression_requirement, 2025, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, continuity_reading).
narrative_ontology:affects_constraint(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, rupture_progressive_reading).
narrative_ontology:affects_constraint(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, composite_overdetermination_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition per the epsilon-invariance principle: the colloquial label 'what Vatican II did' covers four structurally distinct readings of one kernel, each with its own epsilon, victim set, and classification — forcing one story to carry them all would make epsilon observer-relative, which the chi formula forbids. Family shape: continuity_reading is upstream (the official hermeneutic, cited against both rupture readings); rupture_traditionalist_reading (this file) and rupture_progressive_reading share the factual premise that rupture occurred and invert its valuation; composite_overdetermination_reading cuts across all three by denying the unity of the object. Every member links the others via affects_constraints; divergence in their authored epsilons over the shared referent is the corpus's measurement, not an inconsistency.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
