% ============================================================================
% CONSTRAINT STORY: john_1_1_logos__orthodox_christological
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_john_1_1_logos__orthodox_christological, []).

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
 *   constraint_id: john_1_1_logos__orthodox_christological
 *   human_readable: Nicene Logos Confession Boundary (Orthodox Christological Reading)
 *   domain: theology/biblical_hermeneutics/christology/ecclesial_governance
 *
 * SUMMARY:
 *   The Gospel of John opens by identifying the Word (Logos) with God and
 *   climaxes its prologue with 'the Word became flesh' (1:14). The orthodox
 *   Christological reading fixes this as ontological fact: the Logos is
 *   preexistent, fully divine, consubstantial with the Father, identical with
 *   the second person of the Trinity, and the incarnation is God himself
 *   becoming flesh. Read as a lived arrangement, the reading operates as a
 *   communion boundary: admission to the sacramental economy, recognition of
 *   baptisms and ordinations, and eligibility for teaching office are
 *   conditioned on confessing it. Its enforcement history runs from Nicaea
 *   (325) through theocratic coercion under Theodosius, medieval
 *   consolidation, fragmentation after the Reformation, toleration-era decay,
 *   and twentieth-century confessional re-hardening. The claim/metric
 *   relationship is independent by design: the claimed type records the
 *   structural judgment that both a genuine coordination function and
 *   asymmetric transfer through the same boundary are present; the metrics
 *   record the descriptive operating profile, including the long enforcement
 *   wave. COMMITTER FRAME: this file instantiates ONE reading of kernel
 *   john_1_1_logos. The sibling readings (subordinationist,
 *   non_incarnational_monotheist) are separate constraint stories with their
 *   own epsilon, victims, and classifications; nothing about the contest
 *   belongs inside this one. Their epsilon differs because each reading's
 *   enforcement history and victim set differ: the subordinationist reading's
 *   own tenure as imperial policy gives its story a reversed enforcement arc,
 *   and the non_incarnational reading carries little boundary machinery at
 *   all. KEY AGENTS (by structural relationship): - episcopal_hierarchies:
 *   agenda-setting administrator and principal collector
 *   (institutional/identity_locked) — sets and polices the confession,
 *   receives deference and support - trinitarian_laity: net beneficiary
 *   (organized/constrained) — receives sacramental assurance and communal
 *   identity, sustains the center - imperial_patronage_states: historical
 *   beneficiary-enforcer (institutional/arbitrage) — exchanged coercion for
 *   legitimation; exited at disestablishment - subordinationist_christians:
 *   principal target (organized/trapped) — anathematized, disenfranchised,
 *   exiled across the theocratic phase -
 *   non_incarnational_monotheist_christians: secondary target
 *   (moderate/constrained) — deposed, expelled, excluded from pulpits and
 *   tables - constitutional_tolerant_states: neutralizing observer
 *   (institutional/analytical) — removed the coercive floor -
 *   academic_biblical_scholars: analytical observer
 *   (institutional/analytical) — supplies the grammatical and reception
 *   evidence all seats cite
 *
 * KEY AGENTS:
 *   - episcopal_hierarchies — primary administrator and principal collector (institutional/identity_locked): sets and polices the confession, receives deference and material support
 *   - trinitarian_laity — net beneficiary (organized/constrained): receives sacramental assurance and communal identity, sustains the center
 *   - imperial_patronage_states — historical beneficiary-enforcer (institutional/arbitrage): exchanged coercion for legitimation; exited at disestablishment
 *   - subordinationist_christians — principal target (organized/trapped): anathematized, disenfranchised, exiled across the theocratic phase
 *   - non_incarnational_monotheist_christians — secondary target (moderate/constrained): deposed, expelled, excluded from pulpits and membership tables
 *   - constitutional_tolerant_states — neutralizing observer (institutional/analytical): withdrew the coercive floor post-disestablishment
 *   - academic_biblical_scholars — analytical observer (institutional/analytical): produces the grammatical and reception-history evidence every seat cites
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(john_1_1_logos__orthodox_christological, 0.6).
domain_priors:suppression_score(john_1_1_logos__orthodox_christological, 0.45).
domain_priors:theater_ratio(john_1_1_logos__orthodox_christological, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(john_1_1_logos__orthodox_christological, extractiveness, 0.6).
narrative_ontology:constraint_metric(john_1_1_logos__orthodox_christological, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(john_1_1_logos__orthodox_christological, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(john_1_1_logos__orthodox_christological, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(john_1_1_logos__orthodox_christological, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(john_1_1_logos__orthodox_christological, tangled_rope).
narrative_ontology:human_readable(john_1_1_logos__orthodox_christological, "Nicene Logos Confession Boundary (Orthodox Christological Reading)").
narrative_ontology:topic_domain(john_1_1_logos__orthodox_christological, "theology/biblical_hermeneutics/christology/ecclesial_governance").

domain_priors:requires_active_enforcement(john_1_1_logos__orthodox_christological).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(john_1_1_logos__orthodox_christological, '87605adc-c695-4e39-b646-0138b7b30430').
narrative_ontology:cs_kernel_codification('87605adc-c695-4e39-b646-0138b7b30430', fixed_text).
narrative_ontology:cs_authority_grounding('87605adc-c695-4e39-b646-0138b7b30430', lineage).
narrative_ontology:cs_interpretation_layer_present('87605adc-c695-4e39-b646-0138b7b30430').
narrative_ontology:cs_reading_relation('87605adc-c695-4e39-b646-0138b7b30430', john_1_1_logos__subordinationist, forecloses).
narrative_ontology:cs_reading_relation('87605adc-c695-4e39-b646-0138b7b30430', john_1_1_logos__non_incarnational_monotheist, forecloses).
narrative_ontology:cs_axiom('87605adc-c695-4e39-b646-0138b7b30430', foundational, logos_is_consubstantial_preexistent_divine_person).
narrative_ontology:cs_axiom_status(logos_is_consubstantial_preexistent_divine_person, holdable).
narrative_ontology:cs_axiom_grounding('87605adc-c695-4e39-b646-0138b7b30430', logos_is_consubstantial_preexistent_divine_person, theological).
narrative_ontology:cs_axiom('87605adc-c695-4e39-b646-0138b7b30430', secondary, incarnation_grounds_sacramental_economy).
narrative_ontology:cs_axiom_status(incarnation_grounds_sacramental_economy, holdable).
narrative_ontology:cs_axiom_grounding('87605adc-c695-4e39-b646-0138b7b30430', incarnation_grounds_sacramental_economy, instrumental).
narrative_ontology:cs_reference_frame('87605adc-c695-4e39-b646-0138b7b30430', nicene_apostolic_rule_of_faith).
narrative_ontology:cs_drift_state('87605adc-c695-4e39-b646-0138b7b30430', contemporary_critical_scholarship_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('87605adc-c695-4e39-b646-0138b7b30430', '').
narrative_ontology:cs_kernel_id(john_1_1_logos__orthodox_christological, john_1_1_logos).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(john_1_1_logos__orthodox_christological, episcopal_hierarchies).
narrative_ontology:constraint_beneficiary(john_1_1_logos__orthodox_christological, trinitarian_laity).
narrative_ontology:constraint_beneficiary(john_1_1_logos__orthodox_christological, imperial_patronage_states).
narrative_ontology:constraint_victim(john_1_1_logos__orthodox_christological, subordinationist_christians).
narrative_ontology:constraint_victim(john_1_1_logos__orthodox_christological, non_incarnational_monotheist_christians).
narrative_ontology:constraint_vindicates(john_1_1_logos__orthodox_christological, homoousion_consubstantiality_doctrine).
narrative_ontology:constraint_vindicates(john_1_1_logos__orthodox_christological, hypostatic_union_incarnation_formula).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Bishops, synods, and teaching offices define the confession, examine candidates and clergy, admit or refuse communicants, and ordain or depose on christological grounds. Deference, obedience, and material support flow to the office from the faithful. The officeholder's self-understanding is constituted by guardianship of transmitted teaching, so resignation or conversion dissolves the role-self rather than merely changing employment.
narrative_ontology:constraint_stakeholder(john_1_1_logos__orthodox_christological, episcopal_hierarchies, agenda_setter,
    institutional, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(john_1_1_logos__orthodox_christological, episcopal_hierarchies, beneficiary).

% Believers who confess the creed receive recognized baptism, eucharistic access, liturgical coherence, and assurance about salvation framed by the incarnation. They sustain the clergy financially and through attendance. Leaving costs congregation, family ties, and inherited identity, so exit is possible but expensive.
narrative_ontology:constraint_stakeholder(john_1_1_logos__orthodox_christological, trinitarian_laity, beneficiary,
    organized, biographical, constrained, global).

% From Constantine through the early modern establishments, rulers granted legal standing, buildings, and coercive backing to the winning confession and withdrew them from condemned parties, receiving a legitimacy-and-unity instrument in return. Disestablishment and toleration regimes ended this exchange; this seat has exited the arrangement entirely.
narrative_ontology:constraint_stakeholder(john_1_1_logos__orthodox_christological, imperial_patronage_states, beneficiary,
    institutional, civilizational, arbitrage, continental).
narrative_ontology:stakeholder_secondary_role(john_1_1_logos__orthodox_christological, imperial_patronage_states, agenda_setter).

% Communities confessing the Logos as the first and highest creature — Alexandrian presbyters around Arius, Homoian bishops, Gothic kingdoms under Ulfilas' line — held churches, courts, and imperial favor in alternation, then lost all of it after Theodosius: offices removed, buildings transferred, assemblies banned, writings surviving mostly in opponents' quotations. Recantation restored standing; flight beyond the frontier preserved the confession at the price of exile. Modern heirs report parallel exclusion from ecumenical bodies and shared communion.
narrative_ontology:constraint_stakeholder(john_1_1_logos__orthodox_christological, subordinationist_christians, payer,
    organized, generational, trapped, continental).

% Christians reading the Logos as divine wisdom, plan, or creative speech rather than a distinct incarnate person — Paul of Samosata's circle, adoptionist and modalist monarchians, the Socinian Polish Brethren with their academy at Rakow, modern unitarian and liberal-reading congregations — have been deposed, expelled, or barred from pulpits and membership tables, usually escaping the harshest penalties aimed at the subordinationist wing. Exit typically means forming separate denominations or quiet assimilation.
narrative_ontology:constraint_stakeholder(john_1_1_logos__orthodox_christological, non_incarnational_monotheist_christians, payer,
    moderate, generational, constrained, regional).

% Post-disestablishment states decline to enforce any christology, guaranteeing assembly and free exercise. They host litigation over internal property disputes and occasionally rule on them, but their neutrality removed the arrangement's former coercive floor.
narrative_ontology:constraint_stakeholder(john_1_1_logos__orthodox_christological, constitutional_tolerant_states, observer,
    institutional, generational, analytical, national).

% Textual critics, Koine semanticists, and historians of doctrine analyze John 1:1-14 and its reception without holding communion stakes. Their publications supply the grammatical and reception-history evidence every seat cites, and they face career incentives on all sides of the dispute.
narrative_ontology:constraint_stakeholder(john_1_1_logos__orthodox_christological, academic_biblical_scholars, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(john_1_1_logos__orthodox_christological, episcopal_hierarchies).
narrative_ontology:fixing_cost_class(john_1_1_logos__orthodox_christological, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides one authoritative answer to 'who is the Logos relative to the Father,' enabling mutual recognition of baptisms and ordinations, shared liturgy that addresses Christ as divine without splitting monotheism, reliable multigenerational transmission of teaching, and a membership criterion legible across languages and cultures.
% TRANSFER_FUNCTION: Moves recognition goods — communion, sacramental validity, teaching office, salvation-assurance — toward those confessing the Nicene reading and strips them from dissenters; moves deference, obedience, and material support toward the episcopal center; historically moved legal standing, offices, and property away from condemned parties.
% ABSENT_VOICES: The people the boundary condemns were rarely in the room where it was drawn: Arius was summoned to Nicaea but given no deliberative voice and his writings were destroyed, surviving mainly in opponents' citations; the Polish Brethren were expelled from Poland in 1658 rather than heard; modern non-Trinitarian bodies sit outside ecumenical tables whose membership bases require Trinitarian confession. The excluded voices are the victims themselves, positioned outside the councils, synods, and dialogues that decide their status.
% DISAPPEARANCE_RATIONALE: If the confession boundary vanished overnight, every major communion would have to renegotiate recognition rules for baptisms, ordinations, and eucharistic sharing; non-Trinitarian believers would reintegrate or form open-communion federations; the sacramental gatekeeping built on incarnational grounding would lose its warrant; hymnody, catechesis, and seminary curricula would be rewritten. With roughly 2.4 billion adherents structured around this boundary, the rearrangement would be enormous.
% FOUNDING_PROBLEM: After the church's legalization (312-313), an empire-scale communion lacked a shared rule for who Jesus is relative to God: Arius' teaching made liturgical address to Christ incoherent under strict monotheism, and bishops needed a criterion for mutual recognition and baptismal validity across a newly unified body. The arrangement was built to fix a single christological confession as the condition of communion.
% FOUNDING_PROBLEM_CORROBORATION: Secular historians of late antiquity — outside the benefiting parties — attest the political contingency of the settlement's shape (imperial patronage, Alexandrian presbyter conflicts, episcopal rivalries), corroborating the reading that the founding crisis was a passing configuration. The hierarchy attests instead that the founding problem is the perennial question of God's identity and therefore permanently live. External corroboration thus supports the contested verdict: the crisis documentation is corroborated from outside, while the perennial-liveness claim rests chiefly on insider testimony.
narrative_ontology:disappearance_verdict(john_1_1_logos__orthodox_christological, world_rearranges).
narrative_ontology:founding_problem_status(john_1_1_logos__orthodox_christological, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(john_1_1_logos__orthodox_christological, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(john_1_1_logos__orthodox_christological, 'none', 1).
narrative_ontology:epsilon_provenance(john_1_1_logos__orthodox_christological, 0.6, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(john_1_1_logos__orthodox_christological_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(john_1_1_logos__orthodox_christological, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(john_1_1_logos__orthodox_christological_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Transfer burden is authored at 0.60 for the present day: the anathema machinery no longer commands state force in most jurisdictions, but communion refusal, pulpit exclusion, and ecumenical-table bar still strip recognition goods from non-Trinitarian believers, and the arrangement's own soteriology prices that exclusion as ultimate. Suppression is authored at 0.45 — materially below its theocratic peak — because enforcement today is ecclesial rather than legal. Theater at 0.30: creedal recitation and anathema-formula repetition are partly performative where they no longer bind anything, yet boundary administration still does real work (mutual recognition, ordination, catechesis), so the ratio sits mid-low. Accessibility collapse 0.55: within the communion frame, alternatives to confession collapse almost completely, but exit to other traditions or private dissent remains real, unlike the pre-toleration era. Resistance 0.55 reflects fifteen centuries of organized dissent that twice captured imperial power (Constantius, Valens), founded durable counter-communions, and survives today. The temporal series runs on ONE shared ten-point grid so every metric is authored at every examined time point; suppression_requirement is authored rather than left static because the story's subject genuinely tracks enforcement-capacity change: built up 325-450, normalized through the medieval period, fragmented after the Reformation, decayed through toleration, and partially re-hardened with twentieth-century confessional revivals. The long wave — theocratic peak, toleration trough, confessional rebound — spans centuries and is driven by establishment politics and revival cycles, not intermittent reinforcement; the rebound phase models extraction operating through voluntary-affiliation boundary-marking rather than state coercion.
 *
 * PERSPECTIVAL GAP:
 *   From the episcopal seat the boundary is fidelity: guarding transmitted teaching, protecting worship from incoherence, administering sacraments whose validity depends on the incarnation being real. From the target seats the same structure is a wall: recognition goods stripped, assemblies banned, livelihoods forfeited over a grammatical and metaphysical reading they reject. The laity seat splits the difference — belonging and assurance purchased with tithe, deference, and bounded inquiry. Same-level differentiation: subordinationist_christians and non_incarnational_monotheist_christians hold the same nominal victim position but compute differently — the former were organized enough to hold imperial power twice and drew maximal enforcement when it turned; the latter were weaker, regionally concentrated, and more often merely expelled. Coalition failure is structural here: the two target classes condemned each other at various points (Arian councils also deposed adoptionist teachers), so the burdened class never coalesced. Academic observers register contingency; tolerant states register a private-association matter. The engine computes these per-seat divergences from the structural data; the authored claim adjudicates none of it.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive the derivation toward the subsidized pole: episcopal_hierarchies (agenda-setting plus collection) sit nearest it; imperial_patronage_states collected legitimation and exited with arbitrage-grade finality, pinning them near zero; trinitarian_laity benefit but also pay tithes and deference, placing them low but not minimal. Victim declarations drive the target side: subordinationist_christians combine victim status with trapped exit — historically recantation or exile — putting them nearest the full-target pole; non_incarnational_monotheist_christians carry victim status with constrained exit, high but below the trapped seat. Scope is global for the communion-spanning actors, modestly amplifying effective burden for targets since verifying conformity across languages and cultures is hard. Suppression is authored as a raw structural property and is NOT scaled — only the transfer metric receives directionality and scope scaling in the engine's computation.
 *
 * MANDATROPHY ANALYSIS:
 *   Classifying this as pure extraction would erase the real coordination: translocal, multilingual, multigenerational communities demonstrably need a shared answer to 'who is Christ relative to God' for liturgy, mutual recognition, and transmission, and the creed supplies it at scale. Classifying it as pure coordination would erase the anathema: the coordination was purchased by condemning the alternatives, transferring recognition goods away from dissenters, and maintaining enforcement machinery the whole way down — hence the hybrid classification with both gates satisfied (coordination beneficiaries, declared victims, active enforcement). Obsolescence risk concentrates in the nominal-creedal zone: where enforcement decayed fastest (the toleration trough, theater peaking at 0.38), the boundary approached performance without consequence, a degraded-function shadow; the twentieth-century rebound shows the function reviving wherever communities treat the confession as load-bearing. The founding problem is scored contested and the disappearance verdict is world_rearranges, so the mismatch consumer finds no dead-problem-plus-dependence flag: the arrangement persists because the question it answers is still argued over, not because nobody remembers why it exists.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'This constraint is one reading of kernel john_1_1_logos; would the subordinationist or non_incarnational_monotheist readings yield a different constraint — different victims, different enforcement warrants — such that this story''s classification profile is reading-relative rather than text-relative?',
    'Comparative authorship of the two sibling stories plus reception-history evidence establishing which reading carried enforcement power at each phase of the interval.',
    'If a sibling reading is textually superior, the anathematized were condemned for the text''s own meaning and this arrangement''s transfer of recognition goods away from them acquires an injustice-by-construction character, shifting weight toward the snare side of the hybrid.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Reading-relativity of the boundary''s moral and classification profile within the john_1_1_logos kernel.').

omega_variable(
    anarthrous_theos_grammar,
    'Does the anarthrous theos in John 1:1c denote full deity (definite or qualitative force), divinity-in-nature short of personal identity claims, or a created divinity (indefinite force)?',
    'Exhaustive Koine semantic study of pre-verbal anarthrous predicate nominatives (the Colwell-Harner line and successors), parallel Johannine usage, and patristic citation practice before and after Nicaea.',
    'Qualitative or definite force supports this reading''s textual warrant; indefinite force transfers warrant to the subordinationist sibling and undermines the anathemas'' grammatical ground.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(anarthrous_theos_grammar, empirical, 'The grammatical crux of 1:1c separating the sibling readings.').

omega_variable(
    revealed_truth_vs_constructed_boundary,
    'Is the confessed content a disclosure of divine reality that makes the boundary a response to truth, or a constructed arrangement whose principal effect is to concentrate recognition and authority in its administrators?',
    'Not resolvable by data alone; turns on prior commitments about revelation. Behavioral proxy: whether boundary administration across cases tracks doctrinal necessity or institutional interest.',
    'On the disclosure side, the suppression reads as necessary boundary-keeping and excess transfer shrinks toward coordination cost; on the construction side, the arrangement trends toward pure extraction riding a coordination story.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(revealed_truth_vs_constructed_boundary, preference, 'Natural-law versus constructed-character ambiguity for the confession content itself.').

omega_variable(
    enforcement_necessity_plurality_test,
    'Can a large communion sustain shared liturgy, mutual recognition, and multigenerational transmission with christological pluralism, or does boundary enforcement carry load that pluralism collapses?',
    'Longitudinal comparison of pluralist-confession communions (united-church unions with minimal creedal tests) against confessionally strict ones on cohesion, defection rates, and mutual-recognition trade outcomes.',
    'If pluralist bodies sustain the coordination goods, the measured enforcement is discretionary and the classification slides toward rope; if they fragment, enforcement is load-bearing and the tangled_rope reading is confirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_necessity_plurality_test, empirical, 'Whether the enforcement machinery is functionally necessary or discretionary.').

omega_variable(
    soteriological_cost_incomparability,
    'Does the scalar transfer measure systematically understate harm to victims because exclusion from the sacramental economy is priced as ultimate loss inside the system''s own terms, making any finite scalar a category error for the trapped seats?',
    'No external numeraire exists; sensitivity analysis comparing classification outcomes under alternative cost models (finite external disvalue versus insider-priced ultimate disvalue).',
    'If insider pricing governs, effective burden saturates near the ceiling for trapped victims and the arrangement approaches pure extraction; if external pricing governs, the authored mid-range value stands.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(soteriological_cost_incomparability, conceptual, 'Measurement-model dependence of victim cost for soteriological exclusion.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(john_1_1_logos__orthodox_christological, 0, 1700).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(john_tr_t0, john_1_1_logos__orthodox_christological, theater_ratio, 0, 0.1).
narrative_ontology:measurement_basis(john_tr_t0, observed).
narrative_ontology:measurement(john_tr_t56, john_1_1_logos__orthodox_christological, theater_ratio, 56, 0.08).
narrative_ontology:measurement_basis(john_tr_t56, observed).
narrative_ontology:measurement(john_tr_t126, john_1_1_logos__orthodox_christological, theater_ratio, 126, 0.07).
narrative_ontology:measurement_basis(john_tr_t126, observed).
narrative_ontology:measurement(john_tr_t400, john_1_1_logos__orthodox_christological, theater_ratio, 400, 0.12).
narrative_ontology:measurement_basis(john_tr_t400, observed).
narrative_ontology:measurement(john_tr_t800, john_1_1_logos__orthodox_christological, theater_ratio, 800, 0.18).
narrative_ontology:measurement_basis(john_tr_t800, observed).
narrative_ontology:measurement(john_tr_t1054, john_1_1_logos__orthodox_christological, theater_ratio, 1054, 0.22).
narrative_ontology:measurement_basis(john_tr_t1054, observed).
narrative_ontology:measurement(john_tr_t1200, john_1_1_logos__orthodox_christological, theater_ratio, 1200, 0.26).
narrative_ontology:measurement_basis(john_tr_t1200, observed).
narrative_ontology:measurement(john_tr_t1400, john_1_1_logos__orthodox_christological, theater_ratio, 1400, 0.34).
narrative_ontology:measurement_basis(john_tr_t1400, observed).
narrative_ontology:measurement(john_tr_t1550, john_1_1_logos__orthodox_christological, theater_ratio, 1550, 0.38).
narrative_ontology:measurement_basis(john_tr_t1550, observed).
narrative_ontology:measurement(john_tr_t1700, john_1_1_logos__orthodox_christological, theater_ratio, 1700, 0.3).
narrative_ontology:measurement_basis(john_tr_t1700, observed).

% Extraction over time
narrative_ontology:measurement(john_be_t0, john_1_1_logos__orthodox_christological, base_extractiveness, 0, 0.6).
narrative_ontology:measurement_basis(john_be_t0, observed).
narrative_ontology:measurement(john_be_t56, john_1_1_logos__orthodox_christological, base_extractiveness, 56, 0.78).
narrative_ontology:measurement_basis(john_be_t56, observed).
narrative_ontology:measurement(john_be_t126, john_1_1_logos__orthodox_christological, base_extractiveness, 126, 0.84).
narrative_ontology:measurement_basis(john_be_t126, observed).
narrative_ontology:measurement(john_be_t400, john_1_1_logos__orthodox_christological, base_extractiveness, 400, 0.76).
narrative_ontology:measurement_basis(john_be_t400, observed).
narrative_ontology:measurement(john_be_t800, john_1_1_logos__orthodox_christological, base_extractiveness, 800, 0.79).
narrative_ontology:measurement_basis(john_be_t800, observed).
narrative_ontology:measurement(john_be_t1054, john_1_1_logos__orthodox_christological, base_extractiveness, 1054, 0.72).
narrative_ontology:measurement_basis(john_be_t1054, observed).
narrative_ontology:measurement(john_be_t1200, john_1_1_logos__orthodox_christological, base_extractiveness, 1200, 0.67).
narrative_ontology:measurement_basis(john_be_t1200, observed).
narrative_ontology:measurement(john_be_t1400, john_1_1_logos__orthodox_christological, base_extractiveness, 1400, 0.51).
narrative_ontology:measurement_basis(john_be_t1400, observed).
narrative_ontology:measurement(john_be_t1550, john_1_1_logos__orthodox_christological, base_extractiveness, 1550, 0.48).
narrative_ontology:measurement_basis(john_be_t1550, observed).
narrative_ontology:measurement(john_be_t1700, john_1_1_logos__orthodox_christological, base_extractiveness, 1700, 0.6).
narrative_ontology:measurement_basis(john_be_t1700, observed).

% Suppression requirement over time
narrative_ontology:measurement(john_su_t0, john_1_1_logos__orthodox_christological, suppression_requirement, 0, 0.55).
narrative_ontology:measurement_basis(john_su_t0, observed).
narrative_ontology:measurement(john_su_t56, john_1_1_logos__orthodox_christological, suppression_requirement, 56, 0.82).
narrative_ontology:measurement_basis(john_su_t56, observed).
narrative_ontology:measurement(john_su_t126, john_1_1_logos__orthodox_christological, suppression_requirement, 126, 0.86).
narrative_ontology:measurement_basis(john_su_t126, observed).
narrative_ontology:measurement(john_su_t400, john_1_1_logos__orthodox_christological, suppression_requirement, 400, 0.74).
narrative_ontology:measurement_basis(john_su_t400, observed).
narrative_ontology:measurement(john_su_t800, john_1_1_logos__orthodox_christological, suppression_requirement, 800, 0.77).
narrative_ontology:measurement_basis(john_su_t800, observed).
narrative_ontology:measurement(john_su_t1054, john_1_1_logos__orthodox_christological, suppression_requirement, 1054, 0.68).
narrative_ontology:measurement_basis(john_su_t1054, observed).
narrative_ontology:measurement(john_su_t1200, john_1_1_logos__orthodox_christological, suppression_requirement, 1200, 0.64).
narrative_ontology:measurement_basis(john_su_t1200, observed).
narrative_ontology:measurement(john_su_t1400, john_1_1_logos__orthodox_christological, suppression_requirement, 1400, 0.4).
narrative_ontology:measurement_basis(john_su_t1400, observed).
narrative_ontology:measurement(john_su_t1550, john_1_1_logos__orthodox_christological, suppression_requirement, 1550, 0.36).
narrative_ontology:measurement_basis(john_su_t1550, observed).
narrative_ontology:measurement(john_su_t1700, john_1_1_logos__orthodox_christological, suppression_requirement, 1700, 0.45).
narrative_ontology:measurement_basis(john_su_t1700, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(john_1_1_logos__orthodox_christological, identity_coordination).
narrative_ontology:affects_constraint(john_1_1_logos__orthodox_christological, john_1_1_logos__subordinationist).
narrative_ontology:affects_constraint(john_1_1_logos__orthodox_christological, john_1_1_logos__non_incarnational_monotheist).

% DUAL FORMULATION NOTE:
% 'Who is the Logos of John 1:1?' decomposes into three structurally distinct constraints — one per declared reading of the kernel — each with its own epsilon, victim set, and enforcement history; they form a constraint family linked through affects_constraints. This member (orthodox_christological) carries the largest enforcement apparatus and the widest victim set because it won the fourth-century settlement. The subordinationist member's story centers on its own alternating tenure as imperial policy under Constantius and Valens and subsequent dispossession; the non_incarnational member's story carries minimal boundary machinery, mostly modern and low-enforcement. The upstream/downstream relation among family members runs through reception history: whichever reading holds institutional power writes the anathemas the others endure, so influence edges reverse across the interval.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
