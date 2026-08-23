% ============================================================================
% CONSTRAINT STORY: vatican_ii_authority__composite_overdetermination_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_vatican_ii_authority__composite_overdetermination_reading, []).

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
 *   constraint_id: vatican_ii_authority__composite_overdetermination_reading
 *   human_readable: Post-Conciliar Univocal-Interpretation Regime (Composite Overdetermination Reading)
 *   domain: theology/ecclesiology/religious_authority
 *
 * SUMMARY:
 *   This story instantiates the composite_overdetermination_reading of the
 *   kernel vatican_ii_authority. On this reading the conciliar corpus is not
 *   one interpretable event but an overdetermined composite of distinct
 *   doctrinal shifts carrying incompatible theological rationales — products
 *   of factional compromise at drafting time — and the standing arrangement
 *   under contest is the univocal-interpretation regime erected over that
 *   corpus: the claim, administered and enforced by Roman doctrinal offices,
 *   that the Council admits one authentic reading. The eps referent is that
 *   standing regime, assessed by this reading's own lights: it performs real
 *   communion-coordination (the polyvalence holds opposed factions in one
 *   body) while collecting asymmetric interpretive rents (jurisdiction over
 *   meaning, careers, canonical standing) that the texts cannot ground.
 *   Claimed type and metrics are independent authored facts: I claim
 *   tangled_rope because both a genuine coordination function and asymmetric
 *   extraction are structurally present, while the metrics describe the
 *   regime's actual operation as substantially extractive and episodically
 *   coercive. The sibling readings (continuity_reading, rupture_reading) are
 *   separate constraints in the same family, linked via
 *   network.affects_constraints; they are not folded into this story.
 *
 * KEY AGENTS:
 *   - - roman_curial_authorities: Primary beneficiary and agenda setter (institutional/identity_locked) — issues and enforces the official readings; collects interpretive jurisdiction, appointment leverage, and orthodoxy-defining power
 *   - - traditionalist_rite_communities: Primary target (moderate/trapped) — bears irregularization cycles and liturgical restriction
 *   - - progressive_theology_networks: Secondary target (organized/identity_locked) — bears doctrinal discipline whenever their readings gain traction
 *   - - academic_theologians: Dual-positioned (organized/constrained) — harvests the complexity dividend, pays enforcement costs
 *   - - rank_and_file_faithful: Diffuse bearers (powerless/constrained) — assent and confusion downstream, belonging and sacramental life upstream
 *   - - local_diocesan_bishops: Implementation intermediaries (institutional/constrained) — spend credibility executing each reversal
 *   - - ecumenical_partner_churches: External stakeholders (organized/mobile) — hold agreements priced in a reading they cannot vote on
 *   - - council_history_scholars: Analytical observer (analytical/analytical) — sees the drafting-level structure of the overdetermination
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vatican_ii_authority__composite_overdetermination_reading, 0.68).
domain_priors:suppression_score(vatican_ii_authority__composite_overdetermination_reading, 0.58).
domain_priors:theater_ratio(vatican_ii_authority__composite_overdetermination_reading, 0.5).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vatican_ii_authority__composite_overdetermination_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(vatican_ii_authority__composite_overdetermination_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(vatican_ii_authority__composite_overdetermination_reading, theater_ratio, 0.5).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vatican_ii_authority__composite_overdetermination_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(vatican_ii_authority__composite_overdetermination_reading, resistance, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vatican_ii_authority__composite_overdetermination_reading, tangled_rope).
narrative_ontology:human_readable(vatican_ii_authority__composite_overdetermination_reading, "Post-Conciliar Univocal-Interpretation Regime (Composite Overdetermination Reading)").
narrative_ontology:topic_domain(vatican_ii_authority__composite_overdetermination_reading, "theology/ecclesiology/religious_authority").

domain_priors:requires_active_enforcement(vatican_ii_authority__composite_overdetermination_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vatican_ii_authority__composite_overdetermination_reading, 'ac4ea7b7-0b31-480b-8db0-f2d4e1f47312').
narrative_ontology:cs_kernel_codification('ac4ea7b7-0b31-480b-8db0-f2d4e1f47312', fixed_text).
narrative_ontology:cs_authority_grounding('ac4ea7b7-0b31-480b-8db0-f2d4e1f47312', lineage).
narrative_ontology:cs_interpretation_layer_present('ac4ea7b7-0b31-480b-8db0-f2d4e1f47312').
narrative_ontology:cs_reading_relation('ac4ea7b7-0b31-480b-8db0-f2d4e1f47312', vatican_ii_authority__continuity_reading, forecloses).
narrative_ontology:cs_reading_relation('ac4ea7b7-0b31-480b-8db0-f2d4e1f47312', vatican_ii_authority__rupture_reading, forecloses).
narrative_ontology:cs_axiom('ac4ea7b7-0b31-480b-8db0-f2d4e1f47312', foundational, conciliar_corpus_structurally_irresolvable).
narrative_ontology:cs_axiom_status(conciliar_corpus_structurally_irresolvable, holdable).
narrative_ontology:cs_axiom_grounding('ac4ea7b7-0b31-480b-8db0-f2d4e1f47312', conciliar_corpus_structurally_irresolvable, empirically_contingent).
narrative_ontology:cs_axiom('ac4ea7b7-0b31-480b-8db0-f2d4e1f47312', secondary, communion_requires_tolerated_polyvalence).
narrative_ontology:cs_axiom_status(communion_requires_tolerated_polyvalence, holdable).
narrative_ontology:cs_axiom_grounding('ac4ea7b7-0b31-480b-8db0-f2d4e1f47312', communion_requires_tolerated_polyvalence, instrumental).
narrative_ontology:cs_reference_frame('ac4ea7b7-0b31-480b-8db0-f2d4e1f47312', overdetermined_compromise_corpus).
narrative_ontology:cs_drift_state('ac4ea7b7-0b31-480b-8db0-f2d4e1f47312', contemporary_synodality_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('ac4ea7b7-0b31-480b-8db0-f2d4e1f47312', '2026-08-05T09:15:00Z').
narrative_ontology:cs_kernel_id(vatican_ii_authority__composite_overdetermination_reading, vatican_ii_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vatican_ii_authority__composite_overdetermination_reading, roman_curial_authorities).
narrative_ontology:constraint_beneficiary(vatican_ii_authority__composite_overdetermination_reading, academic_theologians).
narrative_ontology:constraint_beneficiary(vatican_ii_authority__composite_overdetermination_reading, ecumenical_partner_churches).
narrative_ontology:constraint_victim(vatican_ii_authority__composite_overdetermination_reading, traditionalist_rite_communities).
narrative_ontology:constraint_victim(vatican_ii_authority__composite_overdetermination_reading, progressive_theology_networks).
narrative_ontology:constraint_victim(vatican_ii_authority__composite_overdetermination_reading, rank_and_file_faithful).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(vatican_ii_authority__composite_overdetermination_reading, rank_and_file_faithful).
narrative_ontology:constraint_victim(vatican_ii_authority__composite_overdetermination_reading, academic_theologians).
narrative_ontology:constraint_victim(vatican_ii_authority__composite_overdetermination_reading, local_diocesan_bishops).
narrative_ontology:constraint_vindicates(vatican_ii_authority__composite_overdetermination_reading, magisterial_interpretive_monopoly).
narrative_ontology:constraint_vindicates(vatican_ii_authority__composite_overdetermination_reading, hermeneutic_of_reform_in_continuity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Doctrinal congregations and related offices issue the authoritative readings: notifications, replies to formal doubts, implementation decrees, and the granting or withdrawal of teaching mandates. Every clarification they publish binds pastors and professors. Clarification requests arrive constantly from both directions — traditionalists asking whether the older rites return, reformers asking whether newer disciplines stand — and each answer disappoints one side. They cannot relinquish the claim to a single authentic reading without dissolving the office's reason for existing; the office and the claim are the same thing.
narrative_ontology:constraint_stakeholder(vatican_ii_authority__composite_overdetermination_reading, roman_curial_authorities, agenda_setter,
    institutional, generational, identity_locked, global).

% University and seminary theologians mine the conciliar texts' unresolved tensions for dissertations, journals, and conference economies; the unresolved passages are their renewable research commons. The same texts bind them professionally: teaching requires a mandate from local ordinaries, and reading against the currently favored hermeneutic has ended careers through investigations, withdrawn faculties, and silenced publications. Moving to secular departments is possible but forfeits the ecclesial audience that gives the work its stakes.
narrative_ontology:constraint_stakeholder(vatican_ii_authority__composite_overdetermination_reading, academic_theologians, beneficiary,
    organized, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(vatican_ii_authority__composite_overdetermination_reading, academic_theologians, payer).

% Clerical institutes and lay communities formed around the pre-conciliar liturgy and doctrinal memory. They alternate between tolerated pastoral provision and universal restriction of the older rites' public celebration. Leaving the communion ends their sacramental life as they understand it; staying means accepting whatever reading the next decade enforces. Their seminaries, chapels, and multi-generational identities are built inside the arrangement they contest.
narrative_ontology:constraint_stakeholder(vatican_ii_authority__composite_overdetermination_reading, traditionalist_rite_communities, payer,
    moderate, generational, trapped, continental).

% Reform movements, pastoral innovators, and national synodal initiatives pressing the council's openings — collegiality, conscience, ecumenical hospitality — toward structural change. Discipline reaches them when their readings gain institutional traction: doctrinal assessments, blocked appointments, disciplined synodal texts. Exiting the communion would concede the field to the opposite faction; their project only exists inside the body they seek to change.
narrative_ontology:constraint_stakeholder(vatican_ii_authority__composite_overdetermination_reading, progressive_theology_networks, payer,
    organized, biographical, identity_locked, continental).

% The baptized at large receive whichever liturgy, catechesis, and discipline the current implementation carries, and are asked for interior assent to officially promulgated readings. They gain belonging, sacramental life, and a community spanning generations and borders; they pay in reversals that unsettle parishes mid-stream, in catechetical confusion between generations, and in having no seat where the meanings are decided.
narrative_ontology:constraint_stakeholder(vatican_ii_authority__composite_overdetermination_reading, rank_and_file_faithful, payer,
    powerless, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(vatican_ii_authority__composite_overdetermination_reading, rank_and_file_faithful, beneficiary).

% National conferences and individual ordinaries implement directives that contradict the previous decade's directives: rites restored and then restricted, synodal processes encouraged and then disciplined. Their standing with clergy and laity erodes with each reversal they execute. There is no exit — office, succession, and livelihood all sit inside the system they administer.
narrative_ontology:constraint_stakeholder(vatican_ii_authority__composite_overdetermination_reading, local_diocesan_bishops, payer,
    institutional, biographical, constrained, regional).

% Reformed, Anglican, and Orthodox partners built decades of dialogue on particular readings of the council's openness texts — religious liberty, the subsistence formula, shared baptism. They hold no vote in how the texts are read going forward, and each restrictive turn devalues agreements signed under the earlier reading. They remain fully free to redirect their ecumenical investment elsewhere.
narrative_ontology:constraint_stakeholder(vatican_ii_authority__composite_overdetermination_reading, ecumenical_partner_churches, beneficiary,
    organized, generational, mobile, global).

% Historical-critical researchers with access to drafting-commission records, peritus diaries, and voting tallies. They reconstruct which ambiguities were negotiated, which were strategic instruments for passage, and which were accidents of translation — findings that reach official adjudication only when they happen to corroborate it.
narrative_ontology:constraint_stakeholder(vatican_ii_authority__composite_overdetermination_reading, council_history_scholars, observer,
    analytical, biographical, analytical, regional).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(vatican_ii_authority__composite_overdetermination_reading, roman_curial_authorities).
narrative_ontology:fixing_cost_class(vatican_ii_authority__composite_overdetermination_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Holds roughly 1.4 billion baptized members of irreconcilable theological temperaments inside one communion after a council whose passage required mutually contradictory concessions; the polyvalent formulas allowed opposing factions to ratify the same documents while reading them differently.
% TRANSFER_FUNCTION: Moves interpretive authority and assent upward toward Roman doctrinal offices — theologians seek mandates, bishops await confirmations, communities seek canonical recognition — and returns legitimacy, careers, and canonical standing downward selectively, to whoever aligns with the currently favored hermeneutic.
% ABSENT_VOICES: The rank-and-file baptized live every implementation but hold no seat in doctrinal adjudication; separated ecumenical partners signed agreements priced in readings they cannot vote on; historians with archival access sit adjacent to the table and are heard chiefly when they corroborate. All three stand outside the synodal structures, which remain episcopal.
% DISAPPEARANCE_RATIONALE: If the interpretive regime vanished overnight, the communion would fragment along the fault lines it currently manages: liturgical pluralism would harden into parallel jurisdictions, national episcopates would drift into divergent doctrine along trajectories already visible, and the papacy's universal jurisdiction would lose its working instrument. The present shape of the communion depends on the regime.
% FOUNDING_PROBLEM: Once the fathers accepted mutually contradictory compromise formulas to secure passage — on collegiality, religious liberty, and the sources of revelation — the institution faced the question 'what have we decided?' It needed to convert negotiated polyvalence into teachable, enforceable doctrine quickly enough to stop divergent implementations from hardening into de facto schism.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: the five-volume documentary history of the council produced by the Bologna school from commission archives; the published diaries and correspondence of periti on opposing drafting factions; and the synodal acta themselves, in which identical questions were formally re-submitted in 1985, 2005, and 2014-2018 — re-litigation that would be inexplicable had the founding problem been solved. The continuing institutional existence of organized dissenting bodies on both flanks is itself third-party attestation. No corroboration originates solely within the beneficiary set.
narrative_ontology:disappearance_verdict(vatican_ii_authority__composite_overdetermination_reading, world_rearranges).
narrative_ontology:founding_problem_status(vatican_ii_authority__composite_overdetermination_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vatican_ii_authority__composite_overdetermination_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(vatican_ii_authority__composite_overdetermination_reading, 'none', 1).
narrative_ontology:epsilon_provenance(vatican_ii_authority__composite_overdetermination_reading, 0.68, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(vatican_ii_authority__composite_overdetermination_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(vatican_ii_authority__composite_overdetermination_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(vatican_ii_authority__composite_overdetermination_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.68: the demanded assent systematically exceeds what any single reading of the corpus can ground, and the costs of misalignment land on identifiable seats (revoked mandates, suppressed institutes, disciplined synods) while the returns concentrate in one. Suppression 0.58: canonical machinery is real but episodic — investigations, visitations, rite restrictions arrive in waves rather than continuously, and much day-to-day compliance runs on career incentives rather than force. Theater_ratio 0.50: by the interval's end roughly half of official interpretive activity commemorates, restates, or re-celebrates the texts rather than resolving anything — jubilee hermeneutics, anniversary symposia, synods that reproduce the ambiguity in new genres. Accessibility_collapse is low (0.35): unlike a natural law, understanding this regime does not close off alternatives — the overdetermination itself guarantees that rival readings remain publishable, practicable, and defeasibly respectable, which is why resistance stays high (0.65) with organized bodies on both flanks plus the scholarly estate. Suppression is authored as a raw structural property and enters the engine unscaled; only extractiveness is directionality- and scope-scaled. All three temporal series run on one shared grid (t=0..60 in decades since the council's close) so every metric is asserted at every examined point. The enforcement series is intentionally included because this story specifically traces enforcement-capacity change: it oscillates rather than trends — heavy in the first two post-conciliar decades (theologian investigations, liberation-theology proceedings), relaxing toward tradition in the 2007-2019 window, re-tightening from 2021. The oscillation tracks pontificate and faction balance, and may itself function as intermittent reinforcement: each swing mobilizes one faction's hope of final vindication, keeping both flanks invested in the regime instead of exiting. Base_properties scalars report the interval-end state, measured in a tightening phase of the cycle.
 *
 * PERSPECTIVAL GAP:
 *   Seats compute differently from the same structure. From the curial seat the regime is fidelity-keeping: without a single authentic reading there is no magisterium, only a library. From the traditionalist seat it is persecution by alternation — tolerated, then restricted, then tolerated. From the progressive seat it is selective authoritarianism that disciplines innovation faster than it disciplines nostalgia. From the scholar's seat it is an inexhaustible commons guarded by people who occasionally confiscate the shovels. Same-level lateral dynamics matter: two theologians of identical nominal standing meet opposite fates keyed entirely to alignment with the current hermeneutic — one collects the complexity dividend with a mandate in hand, the other meets an investigator; the differentiator is constraint-specific, not global power. Inter-institutionally, the CDF experiences the texts as administrable, bishops' conferences experience them as implementation whiplash, and the ecumenical dicastery experiences them as diplomatic capital depreciating with each restrictive turn. The engine computes these divergences from the structural data; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Roman curial authorities anchor the beneficiary pole: they collect the regime's rents directly (doctrinal jurisdiction, appointment leverage, the power to make and unmake canonical standing), and their identity-lock raises their stake in its persistence. Academic theologians are declared beneficiaries and genuinely are net collectors of the ambiguity's research value, though enforcement exposure blunts the margin — the size of that blunting is carried by the scholar_net_position omega rather than by an override, because the sign of their net position is an open empirical question, not a settled structural fact. Traditionalist communities, progressive networks, and the rank-and-file faithful anchor the target pole: assent, restriction, and confusion flow toward them. Local bishops sit near the target end as transmission losses; ecumenical partners sit near the beneficiary end with mobile exit discounting their exposure. One structural subtlety is left to the stakeholder surface and this commentary rather than a directionality override: the Magisterium is simultaneously the regime's administrator and, on this reading, one of its casualties — every univocal claim it publishes is contradicted by another strand of the same corpus, so enforcement spends the enforcer's credibility (witnessed by the 2014-2016 dubia episode and the fourteen-year reversal from Summorum Pontificum to Traditionis Custodes). This dual position is recorded as secondary_role payer on the roman_curial_authorities stakeholder; no override was authored because the primary flow — rents toward Rome, compliance outward — is what the derivation should register.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope claim does double preventive work. Against the pure-extraction mislabel: the polyvalent texts really do solve a collective-action problem — without them the communion fragments at the drafting stage or at the first enforcement crisis, as the Écône sequence showed — so a snare verdict would erase the coordination that makes the extraction affordable. Against the pure-coordination mislabel: the rents are real, concentrated, and decoupled from doctrinal service quality, so a rope verdict would launder them. Mandatrophy status: the founding problem (converting negotiated polyvalence into enforceable doctrine) is live, not dead — each generation re-submits the same questions — so no resolved-mandatrophy declaration is made. The forward risk this story flags is piton drift: theater_ratio climbs monotonically across the interval (0.15 to 0.50) while the founding problem stays open; if enforcement capacity decays while commemorative activity keeps growing, the regime could persist as performance over an atrophied function, with the administrator unable to fix what it no longer profits from fixing. The fixing-cost assessment is prohibitive on either branch: resolving the corpus into continuity requires repudiating the readings the progressive flank lives by (schism), and resolving it into rupture collapses the institution's own continuity premise, which is the source of its authority to resolve anything.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_position,
    'This constraint instantiates one reading (composite_overdetermination_reading) of the kernel vatican_ii_authority; how would the sibling readings (continuity_reading, rupture_reading) restructure this story if instantiated instead?',
    'Generate the sibling stories and compare their beneficiary/victim sets and authored epsilons over the shared referent: continuity_reading relocates the harmed seats to those injured by rupture narratives; rupture_reading relocates beneficiaries to restoration projects and victims to the deposit itself.',
    'Every classification datum in this story is reading-relative; cross-reading conclusions are valid only through the family''s network edges, never by averaging epsilon values across readings.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_position, conceptual, 'Committer-frame position: this story is one of three readings of a single contested kernel, not a standalone constraint.').

omega_variable(
    overdetermination_or_underdetermination,
    'Is the conciliar corpus genuinely overdetermined — incompatible theological rationales each structurally necessary to the text — or merely underdetermined, with insufficient textual cues awaiting a decisive authorized interpreter?',
    'Full archival triangulation: drafting-commission records, rejected amendments, relatio synthesis, and peritus diaries, checked against the reception record of every attempted authoritative determination (the 1985 Synod, the 2005 hermeneutic address, subsequent dubia and replies).',
    'If merely underdetermined, a future settlement remains textually possible and the continuity_reading regains footing; if overdetermined, every settlement attempt regenerates the conflict it meant to close, confirming this reading''s core premise.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(overdetermination_or_underdetermination, empirical, 'Whether the ambiguity is a property of the corpus or a deficit of interpretation.').

omega_variable(
    deliberate_ambiguity_drafting_strategy,
    'Were the load-bearing ambiguities engineered for passage by drafting factions — deliberate instruments for getting mutually opposed blocs to sign — or are they accidental accretions of committee composition and translation?',
    'Comparative redaction history of the decisive chapters (collegiality, religious liberty, revelation), tracking which disambiguating amendments were proposed, by whom, and why they failed.',
    'Deliberate engineering strengthens the structural-conflict thesis — post-conciliar conflict is designed-in, not accidental; accretion weakens it and shifts weight toward resolvability.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(deliberate_ambiguity_drafting_strategy, empirical, 'Intentionality of the overdetermination''s construction.').

omega_variable(
    enforcement_drive_extraction_or_conviction,
    'Is the oscillating enforcement driven primarily by protection of interpretive rents, or by sincerely held doctrinal conviction that error harms souls?',
    'Compare enforcement timing against threat-to-rent episodes versus doctrinal-novelty episodes, using deliberation records and contemporaneous internal correspondence where accessible.',
    'A rent-protection drive pushes the computed type toward pure extraction; sincere conviction supports treating part of the measured burden as the coordination''s genuine cost.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(enforcement_drive_extraction_or_conviction, empirical, 'Motive attribution behind the enforcement cycle.').

omega_variable(
    scholar_net_position,
    'Are academic theologians net beneficiaries of the ambiguity, or do enforcement losses — revoked mandates, ended careers, silenced publications — outweigh the research dividends?',
    'Career-outcome cohort study of post-conciliar ecclesiology and dogmatic theology faculty against comparable secular disciplines, controlling for institutional tier.',
    'A net-loss finding raises the scholars'' effective directionality toward the target end and increases the extraction weighting; a net-gain finding sustains their declared beneficiary position.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scholar_net_position, empirical, 'Sign of the academic seat''s net position under the regime.').

omega_variable(
    authority_framing_underdetermination,
    'Is the declared commitment-system framing — a fixed-text kernel adjudicated by a lineage-grounded authority with an interpretive buffer — the only defensible structure, or does the real authority sit elsewhere: in the living interpretive tradition layered above the documents, or in papal jurisdictional primacy operating independently of the kernel?',
    'Counterfactual reframing test: if the kernel were the magisterium''s ongoing interpretive practice (implicit codification), drift classification shifts away from practice-drift; if authority is jurisdictional practice rather than lineage, the interpretation-buffer semantics change and the fixed text becomes advisory rather than constitutive.',
    'Adopting the tradition-as-kernel framing yields a self-stabilizing picture in which the regime''s oscillations are normal operation rather than drift; adopting the jurisdictional-practice framing removes the buffer that currently absorbs drift without surfacing revision — either alternative changes the commitment-system classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(authority_framing_underdetermination, conceptual, 'Framing under-determination in the commitment-system structure.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vatican_ii_authority__composite_overdetermination_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vati_tr_t0, vatican_ii_authority__composite_overdetermination_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement_basis(vati_tr_t0, observed).
narrative_ontology:measurement(vati_tr_t10, vatican_ii_authority__composite_overdetermination_reading, theater_ratio, 10, 0.22).
narrative_ontology:measurement_basis(vati_tr_t10, observed).
narrative_ontology:measurement(vati_tr_t20, vatican_ii_authority__composite_overdetermination_reading, theater_ratio, 20, 0.3).
narrative_ontology:measurement_basis(vati_tr_t20, observed).
narrative_ontology:measurement(vati_tr_t30, vatican_ii_authority__composite_overdetermination_reading, theater_ratio, 30, 0.38).
narrative_ontology:measurement_basis(vati_tr_t30, observed).
narrative_ontology:measurement(vati_tr_t40, vatican_ii_authority__composite_overdetermination_reading, theater_ratio, 40, 0.42).
narrative_ontology:measurement_basis(vati_tr_t40, observed).
narrative_ontology:measurement(vati_tr_t50, vatican_ii_authority__composite_overdetermination_reading, theater_ratio, 50, 0.47).
narrative_ontology:measurement_basis(vati_tr_t50, observed).
narrative_ontology:measurement(vati_tr_t60, vatican_ii_authority__composite_overdetermination_reading, theater_ratio, 60, 0.5).
narrative_ontology:measurement_basis(vati_tr_t60, observed).

% Extraction over time
narrative_ontology:measurement(vati_be_t0, vatican_ii_authority__composite_overdetermination_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement_basis(vati_be_t0, observed).
narrative_ontology:measurement(vati_be_t10, vatican_ii_authority__composite_overdetermination_reading, base_extractiveness, 10, 0.48).
narrative_ontology:measurement_basis(vati_be_t10, observed).
narrative_ontology:measurement(vati_be_t20, vatican_ii_authority__composite_overdetermination_reading, base_extractiveness, 20, 0.55).
narrative_ontology:measurement_basis(vati_be_t20, observed).
narrative_ontology:measurement(vati_be_t30, vatican_ii_authority__composite_overdetermination_reading, base_extractiveness, 30, 0.6).
narrative_ontology:measurement_basis(vati_be_t30, observed).
narrative_ontology:measurement(vati_be_t40, vatican_ii_authority__composite_overdetermination_reading, base_extractiveness, 40, 0.62).
narrative_ontology:measurement_basis(vati_be_t40, observed).
narrative_ontology:measurement(vati_be_t50, vatican_ii_authority__composite_overdetermination_reading, base_extractiveness, 50, 0.66).
narrative_ontology:measurement_basis(vati_be_t50, observed).
narrative_ontology:measurement(vati_be_t60, vatican_ii_authority__composite_overdetermination_reading, base_extractiveness, 60, 0.68).
narrative_ontology:measurement_basis(vati_be_t60, observed).

% Suppression requirement over time
narrative_ontology:measurement(vati_su_t0, vatican_ii_authority__composite_overdetermination_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement_basis(vati_su_t0, observed).
narrative_ontology:measurement(vati_su_t10, vatican_ii_authority__composite_overdetermination_reading, suppression_requirement, 10, 0.45).
narrative_ontology:measurement_basis(vati_su_t10, observed).
narrative_ontology:measurement(vati_su_t20, vatican_ii_authority__composite_overdetermination_reading, suppression_requirement, 20, 0.6).
narrative_ontology:measurement_basis(vati_su_t20, observed).
narrative_ontology:measurement(vati_su_t30, vatican_ii_authority__composite_overdetermination_reading, suppression_requirement, 30, 0.52).
narrative_ontology:measurement_basis(vati_su_t30, observed).
narrative_ontology:measurement(vati_su_t40, vatican_ii_authority__composite_overdetermination_reading, suppression_requirement, 40, 0.4).
narrative_ontology:measurement_basis(vati_su_t40, observed).
narrative_ontology:measurement(vati_su_t50, vatican_ii_authority__composite_overdetermination_reading, suppression_requirement, 50, 0.55).
narrative_ontology:measurement_basis(vati_su_t50, observed).
narrative_ontology:measurement(vati_su_t60, vatican_ii_authority__composite_overdetermination_reading, suppression_requirement, 60, 0.58).
narrative_ontology:measurement_basis(vati_su_t60, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vatican_ii_authority__composite_overdetermination_reading, identity_coordination).
narrative_ontology:affects_constraint(vatican_ii_authority__composite_overdetermination_reading, continuity_reading).
narrative_ontology:affects_constraint(vatican_ii_authority__composite_overdetermination_reading, rupture_reading).

% DUAL FORMULATION NOTE:
% Constraint family: one kernel (vatican_ii_authority), three readings emitted as separate stories per the eps-invariance principle. All three assess the SAME standing arrangement — the post-conciliar univocal-interpretation regime — but each authors eps by its own lights over the shared referent: continuity_reading authors low extraction over a referent it reads as faithful organic development; rupture_reading authors high extraction over a referent it reads as usurpation of the tradition; this story authors intermediate-high extraction over a referent it reads as real communion-coordination carrying asymmetric interpretive rents. Family topology: continuity_reading is upstream (the regime cites continuity to authorize its own interpretive monopoly); this reading sits midstream contesting the citability itself; rupture_reading is downstream-parasitic, since its case presupposes the regime's univocity claim to attack. Cross-reading comparison is valid only at the family level through these edges — never by averaging eps values across readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
