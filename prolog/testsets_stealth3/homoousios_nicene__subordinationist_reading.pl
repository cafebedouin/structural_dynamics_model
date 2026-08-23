% ============================================================================
% CONSTRAINT STORY: homoousios_nicene__subordinationist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_homoousios_nicene__subordinationist_reading, []).

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
 *   constraint_id: homoousios_nicene__subordinationist_reading
 *   human_readable: Subordinationist Reading of Homoousios (Nicene Kernel)
 *   domain: historical_theology/ecclesiastical_history/philosophy_of_religion
 *
 * SUMMARY:
 *   The Nicene symbol's term homoousios is a contested kernel: what the term
 *   FIXES about the relation of Father and Son has been read in at least
 *   three structurally distinct ways. This story instantiates the
 *   subordinationist reading alone, as Rule 1 requires: the claim that
 *   homoousios is compatible with functional or ontological subordination -
 *   the Son derives being from the Father, shares divinity, and lacks
 *   equality. Authored as an operative interpretive-doctrinal arrangement,
 *   the reading has a genuine coordination record (it fixed credal semantics
 *   for Homoian and Gothic Christianity for centuries and preserved an
 *   anti-modalist hierarchical monotheism grounded in the Johannine and
 *   Pauline subordination texts) and a genuine extraction record (wherever
 *   enforced it deposed, exiled, and replaced the pro-Nicene equality party,
 *   and it cannot hold any territory without active enforcement).
 *   Beneficiaries are subordinationist communities; the victim is the
 *   equality party. The claim and the metrics are independent authored facts:
 *   claimed_type tangled_rope states the structural judgment (coordination
 *   plus asymmetric extraction plus enforcement); the metrics describe
 *   observed operation across the full 325-2026 interval, ending at a
 *   live-but-minority discursive position. Sibling readings are separate
 *   constraints linked via network.affects_constraints, not averaged into
 *   this one.
 *
 * KEY AGENTS:
 *   - arian_subordinationist_communities: Primary beneficiary (organized/identity_locked) - holds the licensed reading, staffs sees and congregations when backed
 *   - semi_arian_homoiousian_communities: Secondary beneficiary (organized/constrained) - shelters under the reading's umbrella while drifting toward the similarity reading
 *   - pro_nicene_equality_party: Primary payer (organized/identity_locked) - bears deposition, exile, and replacement wherever the reading is enforced
 *   - roman_imperial_authority: Agenda setter (institutional/arbitrage) - flips the enforced creed by edict at low cost to itself
 *   - homoian_episcopal_establishment: Agenda setter and receipt seat (institutional/mobile) - administers the settlement and collects vacated sees
 *   - ordinary_baptized_laity: Dual-positioned (moderate/constrained) - receives catechesis under the governing creed, bears the costs of schism cycles
 *   - patristic_scholarship: Analytical observer (analytical/analytical) - reconstructs the controversy from outside all confessional parties
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(homoousios_nicene__subordinationist_reading, 0.48).
domain_priors:suppression_score(homoousios_nicene__subordinationist_reading, 0.45).
domain_priors:theater_ratio(homoousios_nicene__subordinationist_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(homoousios_nicene__subordinationist_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(homoousios_nicene__subordinationist_reading, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(homoousios_nicene__subordinationist_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(homoousios_nicene__subordinationist_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(homoousios_nicene__subordinationist_reading, resistance, 0.8).

% --- Constraint claim ---
narrative_ontology:constraint_claim(homoousios_nicene__subordinationist_reading, tangled_rope).
narrative_ontology:human_readable(homoousios_nicene__subordinationist_reading, "Subordinationist Reading of Homoousios (Nicene Kernel)").
narrative_ontology:topic_domain(homoousios_nicene__subordinationist_reading, "historical_theology/ecclesiastical_history/philosophy_of_religion").

domain_priors:requires_active_enforcement(homoousios_nicene__subordinationist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(homoousios_nicene__subordinationist_reading, '978707ee-7c59-4191-9a86-907626479b8a').
narrative_ontology:cs_kernel_codification('978707ee-7c59-4191-9a86-907626479b8a', fixed_text).
narrative_ontology:cs_authority_grounding('978707ee-7c59-4191-9a86-907626479b8a', lineage).
narrative_ontology:cs_interpretation_layer_present('978707ee-7c59-4191-9a86-907626479b8a').
narrative_ontology:cs_reading_relation('978707ee-7c59-4191-9a86-907626479b8a', homoousios_nicene__metaphysical_equality_reading, coexists_with).
narrative_ontology:cs_reading_relation('978707ee-7c59-4191-9a86-907626479b8a', homoousios_nicene__honorific_similarity_reading, coexists_with).
narrative_ontology:cs_axiom('978707ee-7c59-4191-9a86-907626479b8a', foundational, son_derives_being_from_father).
narrative_ontology:cs_axiom_status(son_derives_being_from_father, holdable).
narrative_ontology:cs_axiom_grounding('978707ee-7c59-4191-9a86-907626479b8a', son_derives_being_from_father, theological).
narrative_ontology:cs_axiom('978707ee-7c59-4191-9a86-907626479b8a', foundational, homoousios_does_not_entail_equality).
narrative_ontology:cs_axiom_status(homoousios_does_not_entail_equality, holdable).
narrative_ontology:cs_axiom_grounding('978707ee-7c59-4191-9a86-907626479b8a', homoousios_does_not_entail_equality, theological).
narrative_ontology:cs_axiom('978707ee-7c59-4191-9a86-907626479b8a', secondary, father_monarchy_preserved_in_worship).
narrative_ontology:cs_axiom_status(father_monarchy_preserved_in_worship, holdable).
narrative_ontology:cs_axiom_grounding('978707ee-7c59-4191-9a86-907626479b8a', father_monarchy_preserved_in_worship, theological).
narrative_ontology:cs_reference_frame('978707ee-7c59-4191-9a86-907626479b8a', scriptural_monarchy_reference).
narrative_ontology:cs_drift_state('978707ee-7c59-4191-9a86-907626479b8a', contemporary_ecumenical_era, gap(repudiation_pressure, severe, true)).
narrative_ontology:cs_created_at('978707ee-7c59-4191-9a86-907626479b8a', '').
narrative_ontology:cs_kernel_id(homoousios_nicene__subordinationist_reading, homoousios_nicene).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(homoousios_nicene__subordinationist_reading, arian_subordinationist_communities).
narrative_ontology:constraint_beneficiary(homoousios_nicene__subordinationist_reading, semi_arian_homoiousian_communities).
narrative_ontology:constraint_victim(homoousios_nicene__subordinationist_reading, pro_nicene_equality_party).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(homoousios_nicene__subordinationist_reading, homoian_episcopal_establishment).
narrative_ontology:constraint_beneficiary(homoousios_nicene__subordinationist_reading, ordinary_baptized_laity).
narrative_ontology:constraint_victim(homoousios_nicene__subordinationist_reading, ordinary_baptized_laity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Confess the Son as divine yet derivative from the Father, teaching from the Johannine and Pauline subordination texts. When their reading holds imperial or royal backing they staff bishoprics, convene councils, and set the public creed; when it loses backing they persist as dissenting congregations, Gothic national churches, and eventually scholarly positions. Leaving the reading would mean abandoning the scriptural synthesis their communal identity is built on.
narrative_ontology:constraint_stakeholder(homoousios_nicene__subordinationist_reading, arian_subordinationist_communities, beneficiary,
    organized, generational, identity_locked, continental).

% Teach the Son as like the Father in substance and cooperate with subordinationist settlements while stopping short of denying the Son's true divinity. They gain doctrinal room under the reading's umbrella but sit closest to the similarity reading and migrate toward it whenever a settlement hardens beyond their comfort.
narrative_ontology:constraint_stakeholder(homoousios_nicene__subordinationist_reading, semi_arian_homoiousian_communities, beneficiary,
    organized, generational, constrained, continental).

% Confess homoousios as securing full equality of Father and Son - co-eternal, no subordination in being. Under subordinationist-backed settlements they suffer deposition, exile, and replacement: Athanasius's five expulsions, the purges under Constantius II and Valens, Vandal-era persecution in Africa. They are also kept out of the councils that pronounce against them. Their exit is confessionally impossible: recanting Nicaea dissolves the identity they are made of.
narrative_ontology:constraint_stakeholder(homoousios_nicene__subordinationist_reading, pro_nicene_equality_party, payer,
    organized, civilizational, identity_locked, continental).
narrative_ontology:stakeholder_secondary_role(homoousios_nicene__subordinationist_reading, pro_nicene_equality_party, excluded).

% Sets and reverses the enforced creed by edict: Constantius II and Valens impose Homoian formulas; Theodosius imposes Nicaea. Gains social unity and episcopal compliance from whichever settlement currently holds and bears essentially no doctrinal cost in switching sides.
narrative_ontology:constraint_stakeholder(homoousios_nicene__subordinationist_reading, roman_imperial_authority, agenda_setter,
    institutional, biographical, arbitrage, continental).

% Bishops who administer the subordinationist settlement: convene the formula councils at Sirmium and Rimini-Seleucia, depose non-conforming colleagues, and receive the vacated sees and attached patronage. Career mobility is high - many subscribe successive formulas as imperial winds shift, to the point where Jerome wrote that the whole world groaned and marveled to find itself Arian.
narrative_ontology:constraint_stakeholder(homoousios_nicene__subordinationist_reading, homoian_episcopal_establishment, agenda_setter,
    institutional, biographical, mobile, continental).
narrative_ontology:stakeholder_secondary_role(homoousios_nicene__subordinationist_reading, homoian_episcopal_establishment, beneficiary).

% Receive catechesis, liturgy, and baptism under whichever creed governs their locality, and bear the costs of the schism cycles - broken communion, rebaptism controversies, communal violence - without any seat in the councils that decide the formula.
narrative_ontology:constraint_stakeholder(homoousios_nicene__subordinationist_reading, ordinary_baptized_laity, beneficiary,
    moderate, biographical, constrained, continental).
narrative_ontology:stakeholder_secondary_role(homoousios_nicene__subordinationist_reading, ordinary_baptized_laity, payer).

% Modern historians and theologians who reconstruct the controversy from outside every confessional party: they attest that the fourth-century crisis over the term's meaning was real and prolonged, and they track the reading's persistence into contemporary debates over eternal functional subordination.
narrative_ontology:constraint_stakeholder(homoousios_nicene__subordinationist_reading, patristic_scholarship, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(homoousios_nicene__subordinationist_reading, homoian_episcopal_establishment).
narrative_ontology:fixing_cost_class(homoousios_nicene__subordinationist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates Christian teaching, liturgy, and baptismal confession around a hierarchical monotheism: one God the Father as sole source, the Son as divine yet derived - solving the problem of confessing Christ's divinity without collapsing Father and Son into one person (anti-modalist) while honoring the plain sense of texts such as John 14:28 and 1 Corinthians 15:28, and giving the term homoousios a workable meaning for communities that read it permissively.
% TRANSFER_FUNCTION: Moves doctrinal legitimacy, bishoprics, and patronage from holders of the strict-equality reading to subordinationist clergy and communities; moves coercive penalties (deposition, exile, confiscation) onto Nicene confessors wherever the reading holds imperial or royal backing.
% ABSENT_VOICES: Deposed and exiled Nicene bishops were barred from the Homoian councils that pronounced against them; laypeople had no seat in any formula council; and later generations were bound by settlements negotiated without them. The equality party's objection is recorded mainly in exile literature and underground correspondence rather than in the proceedings themselves.
% DISAPPEARANCE_RATIONALE: If the subordinationist license vanished overnight at its fourth-century peak, the Homoian settlement loses its confessional warrant, Gothic Christianity's theological basis shifts, episcopal appointments and imperial church policy reorganize around the equality party, and the half-century of formula competition collapses into a single enforced answer - the communion map of the Mediterranean rearranges. At interval end the dependence is thinner but real: contemporary subordinationist movements and the academic debate they anchor are organized around this reading's persistence.
% FOUNDING_PROBLEM: After Nicaea defined the Son as homoousios with the Father, the tradition faced an unsolved problem: what the new term could coherently mean - how to confess the Son as truly divine without either collapsing him into the Father (modalism) or splitting God in two (tritheism), while the subordination texts of Scripture retained their plain force. The subordinationist reading was built as one answer: fix the term as a floor that permits hierarchy.
% FOUNDING_PROBLEM_CORROBORATION: Academic historians of doctrine outside both confessional parties corroborate that the founding problem was real and remained unresolved for roughly half a century - the sustained formula competition documented in the concorial record and analyzed in standard histories of the period (for example Hanson's and Ayres's accounts of the fourth-century controversies). Contemporary systematic theologians debating eternal functional subordination attest that a version of the problem is still live. No corroboration is drawn from the benefiting subordinationist parties alone.
narrative_ontology:disappearance_verdict(homoousios_nicene__subordinationist_reading, world_rearranges).
narrative_ontology:founding_problem_status(homoousios_nicene__subordinationist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(homoousios_nicene__subordinationist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(homoousios_nicene__subordinationist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(homoousios_nicene__subordinationist_reading, 0.48, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(homoousios_nicene__subordinationist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(homoousios_nicene__subordinationist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(homoousios_nicene__subordinationist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction ends at 0.48 because the reading at interval end is a live minority position whose adoption anywhere reopens exclusion of equality-only readings and whose persistence pressures the enforced orthodoxy, while its historical peaks (0.70 at the Homoian settlement of 359) are carried by the measurement series rather than flattened into the scalar. Suppression is authored as a raw structural property and is deliberately NOT scaled by power or scope - only extraction is scaled in the engine's computation; the 0.45 end-state reflects partial modern re-institutionalization (confessional subscription and credentialing enforcement in pockets) after near-total enforcement decay. Theater_ratio 0.30 captures the performative element of serial formula-shopping councils (bishops signing successive creeds as imperial winds shifted) against predominantly functional theological work. Accessibility_collapse 0.45 is honestly mid-range: the rival equality reading never collapsed - it survived underground through the entire enforcement peak and ultimately reversed the settlement - so alternatives remained discoverable at real cost. Resistance 0.80 is among the highest defensible values: a century of organized Nicene resistance, five exiles of Athanasius, and eventual imperial reversal. The three metric series share one seven-point grid (325, 359, 381, 600, 1500, 1900, 2026) so every metric is authored at every examined time point. The trajectory is an arc (rise, peak, collapse, dormancy, revival), not a cycle - no intermittent-reinforcement mechanism is claimed.
 *
 * PERSPECTIVAL GAP:
 *   The payer seat and the agenda/beneficiary seats should compute sharply different types from the same structural data. From the pro-Nicene seat the arrangement is a heresy-labeling machine backed by imperial force: deposition, exile, and replacement of colleagues, with confessionally impossible exit. From the subordinationist and Homoian episcopal seats the same arrangement is necessary defense of scriptural monotheism against both modalist collapse and conciliar innovation - coordination they built and paid for. The imperial seat experiences neither: it arbitrages between settlements for social unity. The engine computes this per-seat divergence from the power, exit, and role data; the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive low directionality for arian_subordinationist_communities and semi_arian_homoiousian_communities; the victim declaration drives high directionality for pro_nicene_equality_party, amplified toward the full-target end by identity_locked exit (recanting Nicaea dissolves the confessor identity). The imperial agenda_setter derives near-beneficiary directionality through arbitrage-grade exit - it bears almost no cost from any settlement and switches at will. The Homoian episcopal establishment derives low directionality through capture-plus-mobility: it administers the arrangement and collects vacated sees, and its careers survive formula changes. Ordinary laity sit near symmetric: genuine catechetical and liturgical benefit, diffuse schism-cycle costs. No directionality_overrides are authored: the derivation chain produces accurate values from the declared roles and exit options, and any override keyed to a power atom would misfire across the multiple distinct agents sharing that atom in this story.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem - fixing what homoousios could coherently mean amid modalist and tritheist pressures - is contested rather than dead: institutional enforcement of any answer to it collapsed centuries ago, yet the problem is live in current academic theology and in denominational disputes over eternal functional subordination. Classifying this as tangled_rope rather than snare prevents mislabeling a real coordination achievement (a working semantic standard that held together Gothic Christianity for centuries) as pure extraction; refusing scaffold or piton prevents mistaking the modern residue for a transitional arrangement with a sunset or for mere theatrical inertia - the modern position does real critical work, and its historical carrier was never mostly performance. Because founding_problem_status is contested rather than dead alongside disappearance_verdict world_rearranges, the mismatch consumer finds no dead-problem-plus-dependence zombie signature; the honest open question is routed to the omegas instead.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indexicality,
    'This constraint is one reading of the homoousios_nicene kernel; would instantiating a sibling reading (metaphysical_equality_reading, honorific_similarity_reading) invert the beneficiary/victim polarity and change epsilon?',
    'Author the sibling stories as separate epsilon-invariant constraints and compare computed classifications. The disagreement is located in whether the term''s force is definitional (secures equality) or permissive (sets a floor beneath which hierarchy remains licensable).',
    'Under the metaphysical_equality_reading the victim set inverts (subordinationists become the suppressed party) and extraction attaches to the enforced Nicene settlement; under the honorific_similarity_reading extraction thins toward honorific convention and the payer set largely dissolves.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_indexicality, conceptual, 'Committer-frame ambiguity: one kernel, three readings, three structurally distinct constraints.').

omega_variable(
    enforcement_coalition_vs_content,
    'Is the measured extraction a property of the reading''s doctrinal content or of whichever enforcement coalition wields it?',
    'Compare extraction across phases where the same content was carried by different coalitions: Homoian-imperial enforcement (359-381), Gothic royal enforcement (5th-6th centuries), and modern purely discursive carriage. If the value tracks the coalition rather than the content, the classification belongs to the enforcement arrangement, not the reading.',
    'If coalition-driven, the reading itself sits nearer pure coordination and the extractive signal attaches to imperial creed-enforcement machinery generally; the story should be decomposed along the content/coaltion seam.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_coalition_vs_content, conceptual, 'Whether epsilon indexes the reading or the regime enforcing it.').

omega_variable(
    functional_ontological_decomposition,
    'Does permission for functional subordination carry the same extraction profile as permission for ontological subordination, or is this one colloquial label stretched over two constraints?',
    'Apply the epsilon-invariance test: if measuring the reading by its functional-subordination permission yields a clearly lower value than measuring it by its ontological-subordination permission, decompose into two linked stories.',
    'Decomposition would leave a low-extraction functional-ordering constraint (rope-leaning) and a higher-extraction ontological-derivation constraint (tangled_rope or snare-leaning), joined by network edges; the bundled story''s single value would be retired as an averaging artifact.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(functional_ontological_decomposition, conceptual, 'Possible hidden two-constraint structure inside the subordination-permissive claim.').

omega_variable(
    modern_descendant_continuity,
    'Do contemporary eternal-functional-subordination movements instantiate this same constraint, or a structurally distinct descendant with different enforcement machinery and victim sets?',
    'Trace enforcement mechanisms and bearing parties in modern denominational controversies (confessional subscription tests, credentialing disputes over trinitarian doctrine) against the fourth-century profile of council-stacking, deposition, and exile.',
    'If discontinuous, the modern tail of the measurement series describes a different constraint and this story''s interval should close near the extinction of institutional subordinationism rather than extend to the present.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(modern_descendant_continuity, empirical, 'Continuity of the modern revival with the historical arrangement.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(homoousios_nicene__subordinationist_reading, 325, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(homo_tr_t325, homoousios_nicene__subordinationist_reading, theater_ratio, 325, 0.15).
narrative_ontology:measurement(homo_tr_t359, homoousios_nicene__subordinationist_reading, theater_ratio, 359, 0.3).
narrative_ontology:measurement(homo_tr_t381, homoousios_nicene__subordinationist_reading, theater_ratio, 381, 0.35).
narrative_ontology:measurement(homo_tr_t600, homoousios_nicene__subordinationist_reading, theater_ratio, 600, 0.28).
narrative_ontology:measurement(homo_tr_t1500, homoousios_nicene__subordinationist_reading, theater_ratio, 1500, 0.1).
narrative_ontology:measurement(homo_tr_t1900, homoousios_nicene__subordinationist_reading, theater_ratio, 1900, 0.12).
narrative_ontology:measurement(homo_tr_t2026, homoousios_nicene__subordinationist_reading, theater_ratio, 2026, 0.3).

% Extraction over time
narrative_ontology:measurement(homo_be_t325, homoousios_nicene__subordinationist_reading, base_extractiveness, 325, 0.22).
narrative_ontology:measurement(homo_be_t359, homoousios_nicene__subordinationist_reading, base_extractiveness, 359, 0.7).
narrative_ontology:measurement(homo_be_t381, homoousios_nicene__subordinationist_reading, base_extractiveness, 381, 0.52).
narrative_ontology:measurement(homo_be_t600, homoousios_nicene__subordinationist_reading, base_extractiveness, 600, 0.4).
narrative_ontology:measurement(homo_be_t1500, homoousios_nicene__subordinationist_reading, base_extractiveness, 1500, 0.08).
narrative_ontology:measurement(homo_be_t1900, homoousios_nicene__subordinationist_reading, base_extractiveness, 1900, 0.18).
narrative_ontology:measurement(homo_be_t2026, homoousios_nicene__subordinationist_reading, base_extractiveness, 2026, 0.48).

% Suppression requirement over time
narrative_ontology:measurement(homo_su_t325, homoousios_nicene__subordinationist_reading, suppression_requirement, 325, 0.25).
narrative_ontology:measurement(homo_su_t359, homoousios_nicene__subordinationist_reading, suppression_requirement, 359, 0.85).
narrative_ontology:measurement(homo_su_t381, homoousios_nicene__subordinationist_reading, suppression_requirement, 381, 0.6).
narrative_ontology:measurement(homo_su_t600, homoousios_nicene__subordinationist_reading, suppression_requirement, 600, 0.5).
narrative_ontology:measurement(homo_su_t1500, homoousios_nicene__subordinationist_reading, suppression_requirement, 1500, 0.06).
narrative_ontology:measurement(homo_su_t1900, homoousios_nicene__subordinationist_reading, suppression_requirement, 1900, 0.15).
narrative_ontology:measurement(homo_su_t2026, homoousios_nicene__subordinationist_reading, suppression_requirement, 2026, 0.45).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(homoousios_nicene__subordinationist_reading, information_standard).
narrative_ontology:affects_constraint(homoousios_nicene__subordinationist_reading, homoousios_nicene__metaphysical_equality_reading).
narrative_ontology:affects_constraint(homoousios_nicene__subordinationist_reading, homoousios_nicene__honorific_similarity_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'homoousios' conflates three structurally distinct claims about the term's force, decomposed per the epsilon-invariance principle into a three-story constraint family: this subordinationist reading (permissive floor; extraction from the equality party when enforced), the metaphysical_equality_reading (definitional equality; extraction attaches to its enforcement of alternatives), and the honorific_similarity_reading (conventional likeness; thinnest extraction). This reading influences both siblings as downstream pressure: its persistence forced the equality settlement to clarify and police its definitional claim, and it competes for the same scriptural-hierarchy constituency the similarity reading courts. The equality reading is upstream in enforcement resources; this reading is upstream in dissent pressure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
