% ============================================================================
% CONSTRAINT STORY: homoousios_christology__arian_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_homoousios_christology__arian_reading, []).

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
 *   constraint_id: homoousios_christology__arian_reading
 *   human_readable: The Subordinationist Settlement: Christ Created and Subordinate (Arian Reading of the Homoousios Kernel)
 *   domain: historical theology / ecclesiastical politics / commitment systems
 *
 * SUMMARY:
 *   Between the outbreak of the Alexandrian controversy (318) and the Council
 *   of Constantinople (381), the claim that the Son is created and
 *   subordinate to the Father — not of identical substance with him —
 *   organized a real ecclesiastical-political arrangement: a distributed
 *   network of non-Nicene bishops holding sees from the Danube to Egypt, a
 *   repertoire of councils and creeds, and, at intervals, the enforcement
 *   machinery of the imperial court. This story instantiates the
 *   arian_reading of the homoousios_christology kernel as a clean,
 *   epsilon-invariant constraint. The referent of every metric is the
 *   subordinationist settlement itself — the arrangement the story is about —
 *   assessed by the reading's own lights (values are reading-indexed; the
 *   referent is fixed). The reading holds its settlement's enforcement to be
 *   doctrinally necessary discipline rather than rent-collection, and authors
 *   epsilon accordingly — materially lower than a pro-Nicene seat would
 *   author for the same referent — but it does not deny the machinery: Nicene
 *   bishops were deposed, exiled, and replaced through the same councils that
 *   coordinated the network's confession, and congregations received their
 *   bishops at enforcement turnover rates. The claimed type and the metrics
 *   are authored independently: the reading claims tangled_rope because it
 *   can name both the coordination function the settlement served and the
 *   extraction it performed; the metrics are the reading's honest descriptive
 *   account of that dual operation across the settlement's full lifecycle.
 *
 * KEY AGENTS:
 *   - non_nicene_episcopal_network: agenda-setter seat (organized/constrained) — the distributed coalition of Eastern bishops that administers the settlement: convenes councils, consecrates compliant bishops, drafts creeds, and petitions the court against Nicene holdouts
 *   - imperial_unity_administration: enforcement seat with beneficiary position (institutional/arbitrage) — the court that enforces or abandons the settlement as unity politics dictate
 *   - pro_nicene_bishops: primary payer seat (powerful/constrained) — the deposed and exiled hierarchy: Athanasius, Marcellus, Eustathius, Paul, Liberius, Hosius
 *   - pro_nicene_congregations: payer seat (organized/trapped) — urban congregations that bear every episcopal turnover and cannot leave the church
 *   - homoiousian_moderates: swing beneficiary/payer seat (organized/mobile) — the middle party that staffs the settlement's councils and drifts toward Nicaea
 *   - gothic_danubian_communities: beneficiary seat (moderate/constrained) — missionized communities receiving literacy, scripture, and identity through the settlement's missionary arm
 *   - egyptian_monastic_communities: excluded seat (organized/trapped) — moral authorities with enormous lay standing and no conciliar voice
 *   - pagan_imperial_observers: analytical observer seat (institutional/analytical) — the Ammianus circle, auditing the strife from outside the commitment system
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(homoousios_christology__arian_reading, 0.58).
domain_priors:suppression_score(homoousios_christology__arian_reading, 0.62).
domain_priors:theater_ratio(homoousios_christology__arian_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(homoousios_christology__arian_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(homoousios_christology__arian_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(homoousios_christology__arian_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(homoousios_christology__arian_reading, accessibility_collapse, 0.38).
narrative_ontology:constraint_metric(homoousios_christology__arian_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(homoousios_christology__arian_reading, tangled_rope).
narrative_ontology:human_readable(homoousios_christology__arian_reading, "The Subordinationist Settlement: Christ Created and Subordinate (Arian Reading of the Homoousios Kernel)").
narrative_ontology:topic_domain(homoousios_christology__arian_reading, "historical theology / ecclesiastical politics / commitment systems").

domain_priors:requires_active_enforcement(homoousios_christology__arian_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(homoousios_christology__arian_reading, 'a76dc2ef-83a6-4550-9937-4e3054284699').
narrative_ontology:cs_kernel_codification('a76dc2ef-83a6-4550-9937-4e3054284699', fixed_text).
narrative_ontology:cs_authority_grounding('a76dc2ef-83a6-4550-9937-4e3054284699', distributed).
narrative_ontology:cs_reading_relation('a76dc2ef-83a6-4550-9937-4e3054284699', homoousios_christology__pro_nicene_reading, forecloses).
narrative_ontology:cs_reading_relation('a76dc2ef-83a6-4550-9937-4e3054284699', homoousios_christology__semi_arian_reading, coexists_with).
narrative_ontology:cs_axiom('a76dc2ef-83a6-4550-9937-4e3054284699', foundational, son_is_created_not_unbegotten).
narrative_ontology:cs_axiom_status(son_is_created_not_unbegotten, holdable).
narrative_ontology:cs_axiom_grounding('a76dc2ef-83a6-4550-9937-4e3054284699', son_is_created_not_unbegotten, theological).
narrative_ontology:cs_axiom('a76dc2ef-83a6-4550-9937-4e3054284699', foundational, father_alone_unbegotten_transcendent).
narrative_ontology:cs_axiom_status(father_alone_unbegotten_transcendent, holdable).
narrative_ontology:cs_axiom_grounding('a76dc2ef-83a6-4550-9937-4e3054284699', father_alone_unbegotten_transcendent, theological).
narrative_ontology:cs_axiom('a76dc2ef-83a6-4550-9937-4e3054284699', secondary, homoousios_term_unscriptural_and_precondemned).
narrative_ontology:cs_axiom_status(homoousios_term_unscriptural_and_precondemned, holdable).
narrative_ontology:cs_axiom_grounding('a76dc2ef-83a6-4550-9937-4e3054284699', homoousios_term_unscriptural_and_precondemned, conventional).
narrative_ontology:cs_reference_frame('a76dc2ef-83a6-4550-9937-4e3054284699', pre_nicene_subordinationist_settlement).
narrative_ontology:cs_drift_state('a76dc2ef-83a6-4550-9937-4e3054284699', post_constantinopolitan_settlement, gap(repudiation_pressure, severe, true)).
narrative_ontology:cs_created_at('a76dc2ef-83a6-4550-9937-4e3054284699', '').
narrative_ontology:cs_kernel_id(homoousios_christology__arian_reading, homoousios_christology).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(homoousios_christology__arian_reading, non_nicene_episcopal_network).
narrative_ontology:constraint_beneficiary(homoousios_christology__arian_reading, imperial_unity_administration).
narrative_ontology:constraint_beneficiary(homoousios_christology__arian_reading, gothic_danubian_communities).
narrative_ontology:constraint_victim(homoousios_christology__arian_reading, pro_nicene_bishops).
narrative_ontology:constraint_victim(homoousios_christology__arian_reading, pro_nicene_congregations).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(homoousios_christology__arian_reading, homoiousian_moderates).
narrative_ontology:constraint_victim(homoousios_christology__arian_reading, homoiousian_moderates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% A distributed coalition of Eastern bishops who share the reading that the Son is begotten by the Father's will and therefore not identical in substance with him. They convene their own councils, draft and circulate creeds, recognize one another's ordinations, consecrate compliant successors into vacated sees, and petition the court against bishops who hold the homoousios formula. Their standing rests on conciliar legitimacy and scriptural argument rather than any single center; a bishop who crossed to the Nicene formula would lose his network's recognition, and the moderate wing has already begun leaving through the homoiousian middle rather than converting outright.
narrative_ontology:constraint_stakeholder(homoousios_christology__arian_reading, non_nicene_episcopal_network, agenda_setter,
    organized, generational, constrained, continental).

% The imperial court — Constantine in his last decade, Constantius II, and Valens in different degrees — treats the subordinationist settlement as the formula broad enough to command episcopal subscription, and enforces it accordingly: deposing, exiling, and replacing bishops who refuse, and handing their churches to compliant successors. The court is not confessionally committed; Constantine moved from exiling Arius to pressuring his opponents within a decade, and Julian abandoned enforcement altogether. What the court wants is a formula that holds the churches quiet, and it switches formulas when politics move.
narrative_ontology:constraint_stakeholder(homoousios_christology__arian_reading, imperial_unity_administration, agenda_setter,
    institutional, generational, arbitrage, continental).
narrative_ontology:stakeholder_secondary_role(homoousios_christology__arian_reading, imperial_unity_administration, beneficiary).

% Bishops who hold the homoousios formula — Athanasius of Alexandria above all, with Marcellus of Ancyra, Eustathius of Antioch, Paul of Constantinople, Liberius of Rome, Hosius of Corduba. They are deposed by engineered synods, exiled to frontier cities, and replaced by compliant successors; Athanasius alone is exiled five times across the interval. Submission — signing a diluted formula — was available and several took it under pressure (Liberius and Hosius in 357), at the cost of their standing with their own congregations; refusal meant exile, and most refused.
narrative_ontology:constraint_stakeholder(homoousios_christology__arian_reading, pro_nicene_bishops, payer,
    powerful, biographical, constrained, continental).

% Urban congregations in Alexandria, Antioch, and Constantinople whose bishops are swapped with each turn of enforcement. They riot against intruded clergy, shelter exiled bishops, and follow their deposed pastors into exile; Alexandria in this period sees recurring street violence between congregations. Leaving the congregation would mean leaving the church as they understand it, so their practical exit is closed even when their preferred doctrine loses every council.
narrative_ontology:constraint_stakeholder(homoousios_christology__arian_reading, pro_nicene_congregations, payer,
    organized, biographical, trapped, regional).

% The middle party around Basil of Ancyra and the Council of Ancyra (358): the Son is of like substance with the Father — not identical, not other. The settlement's broad umbrella gives them sees, council seats, and drafting roles for compromise formulas; they are the swing bloc every court must court. They are purged when the hard-line homoians win (the Aetian and Eunomian circles) and fall under suspicion when Nicaea wins; their mobility is real, and by the 360s much of their number has drifted toward the Nicene formula.
narrative_ontology:constraint_stakeholder(homoousios_christology__arian_reading, homoiousian_moderates, beneficiary,
    organized, biographical, mobile, continental).
narrative_ontology:stakeholder_secondary_role(homoousios_christology__arian_reading, homoiousian_moderates, payer).

% Gothic communities evangelized from the 340s through the mission associated with Ulfilas, who devised a Gothic alphabet and translated the scriptures into their language. They receive literacy, scripture, and a Christian identity that stands outside the Roman Nicene church's jurisdiction. Their commitment binds them to their own clergy and, later, to their royal houses' confession; repudiating it would mean repudiating the translated scripture their identity is written in.
narrative_ontology:constraint_stakeholder(homoousios_christology__arian_reading, gothic_danubian_communities, beneficiary,
    moderate, generational, constrained, regional).

% The Desert Fathers — Antony's circle and the Pachomian federation — carry enormous moral authority among the laity. They intervene episodically (Antony's letter to the emperor), shelter exiled clergy, and object to imperial troops being sent against monasteries, but they hold no seat in any council that drafts or enforces a creed. They are outside the conversation that decides, and cannot be expelled from a room they were never admitted to.
narrative_ontology:constraint_stakeholder(homoousios_christology__arian_reading, egyptian_monastic_communities, excluded,
    organized, biographical, trapped, regional).

% Senior pagan administrators and officers — the circle of Ammianus Marcellinus — watch the episcopal strife from outside the commitment system entirely. They assess it as a power struggle carried on under doctrinal banners, hold no stake in either formula, and record what they see in histories and correspondence; they are the period's nearest equivalent to an outside audit.
narrative_ontology:constraint_stakeholder(homoousios_christology__arian_reading, pagan_imperial_observers, observer,
    institutional, biographical, analytical, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(homoousios_christology__arian_reading, non_nicene_episcopal_network).
narrative_ontology:fixing_cost_class(homoousios_christology__arian_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Holds a distributed Eastern episcopate — with no single adjudicating center — to a shared subordinationist confession: common scriptural proof-texts, mutual recognition of ordinations, conciliar legitimacy for depositions and consecrations, and (from the 340s) a missionary program giving Gothic communities scripture in their own language.
% TRANSFER_FUNCTION: Moves episcopal sees, imperial favor, and drafting authority at councils toward bishops who subscribe to subordinationist formulas, and moves deposition, exile, and replacement onto bishops who refuse; moves creeds and clergy down to congregations at enforcement turnover rates, and moves liturgy, literacy, and scripture out to the Gothic mission field.
% ABSENT_VOICES: The monastic movement and the lay congregations had no conciliar voice — they objected through riots, shelters, letters, and martyrs but never drafted or voted on a creed; the Gothic communities were objects of mission for a decade before they were parties to any settlement; and Alexandria's urban poor bore the street-level costs of every episcopal turnover with no representation in any of the councils that caused them.
% DISAPPEARANCE_RATIONALE: If the subordinationist settlement vanished overnight, the Eastern episcopal map would rearrange (vacated sees, realigned councils, no engineered depositions), the imperial court would need a different unity formula (Constantius's entire religious policy was built on this one's workability), the Gothic mission's trajectory would change (the Gothic alphabet and Bible were its products), and the coerced-subscription cycle that shaped a generation of episcopal careers would not have occurred.
% FOUNDING_PROBLEM: How to articulate the relation of the Son to the Father without either collapsing the distinction — the Sabellian risk, which seemed to make the Father suffer and dissolve real worship of the Son into a mode of the Father — or dividing the object of worship into two unbegotten principles (ditheism). The subordinationist answer: the Son is genuinely begotten, created by the Father's free act before the ages, preserving both the real distinction of persons and the Father's sole unbegotten transcendence.
% FOUNDING_PROBLEM_CORROBORATION: The problem's reality is corroborated from outside the beneficiary set three ways: the pro-Nicene party itself (a hostile witness — its own formula at Nicaea presupposes the problem by answering it differently); the pre-Nicene Alexandrian exegetical tradition (Origen, Dionysius of Alexandria), which predates the beneficiary set and wrestled with the same texts; and the pagan imperial observers, who attest the strife's intensity while dismissing its content. No source outside the beneficiary set attests that this settlement's specific answer was the only live solution — that claim rests on the reading's own lights.
narrative_ontology:disappearance_verdict(homoousios_christology__arian_reading, world_rearranges).
narrative_ontology:founding_problem_status(homoousios_christology__arian_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(homoousios_christology__arian_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(homoousios_christology__arian_reading, 'none', 1).
narrative_ontology:epsilon_provenance(homoousios_christology__arian_reading, 0.58, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(homoousios_christology__arian_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(homoousios_christology__arian_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(homoousios_christology__arian_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.58: the reading-indexed assessment of the settlement at its enforced operation — real extraction (engineered depositions, exile, coerced subscriptions, intruded bishops) assessed as discipline rather than rent-collection; a pro-Nicene seat assessing the identical referent would author materially higher epsilon, and that divergence between readings over a fixed referent is the corpus's measurement, not an error to reconcile. Suppression 0.62: the settlement's persistence required machinery — imperial access, engineered synods, exile, and the coerced signatures of Liberius and Hosius (357) — which the reading does not deny; it denies only that the machinery was illegitimate. Theater 0.38: councils and creeds were substantially functional (they coordinated a real confession across a distributed episcopate), but the formula-shopping of the 350s — successive Sirmium creeds and the railroaded twin councils of Ariminum and Seleucia (359-360) — was transparently performative, and the settlement's imperial-facing activity decays toward performance at the interval's end. Accessibility collapse 0.38: the Nicene alternative never collapsed — it stayed organized, published, and ultimately victorious, which is itself evidence against any natural-law reading of this settlement. Resistance 0.68: forty years of organized resistance — Athanasius's five exiles, the Western episcopate's initial refusal at Ariminum, Alexandrian congregational violence, the monastic movement's shelter and protest. The measurement series runs on one shared time grid (eight points, time_point = year AD) so every tracked metric is authored at every examined point; the series is non-monotonic because its driver is imperial succession — an external factor, not an oscillation the settlement engineered as an extraction mechanism — and base_properties describe the settlement at its enforced-operation plateau rather than its terminal state, because classifying a completed arrangement by its 381 corpse-values would misclassify it as benign.
 *
 * PERSPECTIVAL GAP:
 *   The payer and agenda-setter seats compute differently from one structure. From the network's seat the settlement is doctrinal fidelity: a confession its members hold to be scriptural, persecuted under Constantine, ascendant under Constantius, and destroyed by force under Theodosius. From the Nicene payer's seat the same councils are the machinery that deposed Athanasius five times and handed his see to an intruded bishop. The court's seat differs from both: it neither confesses nor is persecuted — it consumes unity, and it switches formulas when politics move (Constantine's reversal within a decade of Nicaea, Julian's abandonment of enforcement altogether). The moderates' swing seat experiences the arrangement as shelter becoming trap: the umbrella that gave them sees and drafting roles in the 340s purged them when the hardliners won and suspected them when Nicaea did. Inter-institutionally, the network and the court are both agenda-setters with different relationships to the settlement — the network is confessionally bound to it, the court instrumentally attached — and their alliance is the arrangement's load-bearing wall: when the court withdraws (361), the settlement's extractive capacity collapses within two measurement points even though its doctrine loses no adherents.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations (the network, the court, the Gothic communities) drive low d; victim declarations (Nicene bishops, Nicene congregations) drive high d. Exit options modulate within those poles: the court's arbitrage-grade exit — it can and does switch formulas — sits it at the beneficiary end despite its enforcement role; the congregations' trapped exit sits them at the target end; the Nicene bishops' constrained exit (submission was available, and Liberius and Hosius took it) damps their d below the congregations'. The homoiousian moderates are the one seat whose derived d would be wrong: their secondary payer position and eventual exit make them near-symmetric (d approximately 0.45), but the override mechanism is keyed by power atom, and the network seat shares their atom ('organized') with a genuinely low d (approximately 0.2) — authoring the override would corrupt the network seat, so no override is declared and the residual is carried by the moderates' secondary role, situation text, and the drift omega. Same-level differentiation: the network and the moderates hold the same power atom but different exit options (constrained vs mobile) and different structural relationships (administrator vs swing passenger), which is what differentiates their computed seats despite equal nominal standing.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification prevents two opposite mislabels. Reading the settlement as pure extraction with a doctrinal cover story misses the coordination function: a distributed episcopate with no single center held a coherent monotheistic confession for six decades, maintained mutual recognition of ordinations across the East, and — through its missionary arm — produced the Gothic alphabet, the first Germanic-language Bible translation, and the churches that carried Christianity to the Danube. Reading it as pure coordination misses the victims: the engineered depositions, the exiles, the intruded bishops, the coerced signatures. Tangled rope holds both halves, and the lifecycle data shows the halves separating at the end: after Julian dissolved the enforcement machinery the imperial-facing settlement decayed toward performance (theater ratio ends at 0.42 while extractiveness ends at 0.10), while the Gothic sphere retained the coordination function with no extraction at all — the separability natural experiment recorded in the omegas. The founding problem — articulating the Son's generation without ditheism on one side and Sabellian collapse on the other — stayed live for this reading throughout; the arrangement was killed by enforcement reversal, not by atrophy, so mandatrophy is not resolved, and the founding problem's status is authored as contested because the pro-Nicene sibling holds the problem solved by a different route.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'This constraint is one reading of the homoousios_christology kernel — the arian_reading. What would the sibling readings (pro_nicene_reading, semi_arian_reading) change structurally if instantiated instead, and where exactly is the disagreement located?',
    'The siblings are authored as separate constraint files; comparing their victim sets, epsilon values, and enforcement histories against this story locates the disagreement''s structural consequences. The disagreement itself sits in one element: whether the Son''s generation is an act of the Father''s will (ontological posteriority, this reading''s premise) or an eternal subsisting relation of shared substance (the pro-Nicene premise).',
    'Under the pro-Nicene sibling the victim and beneficiary sets invert — the non-Nicene network becomes the victim class and the Nicene hierarchy the beneficiary class — and epsilon is re-authored materially higher from that seat. The semi-Arian sibling shares this story''s negative claim (not homoousios) but changes the positive claim, altering the content of the coordination function.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Committer structure: which kernel, which reading, what siblings would change, where the disagreement is located.').

omega_variable(
    enforcement_direction_ambiguity,
    'The imperial enforcement machinery runs in both directions across the interval — against this settlement (325-337), for it (337-361, 364-378), abandoned (361-363), and against it again (380-381). Are the measured extractiveness and suppression properties of the settlement itself, or artifacts of which way the machinery points at each time point?',
    'Seat-level separation of the settlement''s own coercive capacity (depositions its councils engineered, subscriptions its envoys coerced) from the court''s independent enforcement; re-author the suppression series counting only capacity the network controlled directly.',
    'If most measured suppression is the court''s rather than the settlement''s, the settlement''s own suppression requirement is materially lower and its persistence profile shifts toward voluntary coordination; if the network''s councils did the engineering, the current values stand.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_direction_ambiguity, empirical, 'Whether suppression metrics track the settlement''s own coercive capacity or the court''s enforcement direction.').

omega_variable(
    gothic_separability_natural_experiment,
    'After 381 the Gothic churches kept the settlement''s coordination function — doctrine, liturgy, the Gothic scripture — while losing every instrument of enforcement. Is the coordination function separable from the extraction, and does it survive the separation?',
    'Track the Gothic churches from 381 to their absorption (Visigothic Spain to the Third Council of Toledo, 589): if confession, liturgy, and community identity persisted for two centuries with zero coercive machinery, the functions are separable and the coordination core is genuine.',
    'Separability supports the tangled_rope reading''s coordination half as real rather than cover; inseparability would mean the settlement''s coordination was itself an enforcement artifact and the classification shifts toward pure extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(gothic_separability_natural_experiment, empirical, 'Whether the settlement''s coordination function survives without its enforcement machinery (the Gothic natural experiment).').

omega_variable(
    homoiousian_drift_erosion,
    'The homoiousian middle supplied the settlement''s swing votes and drafting capacity; by the 360s much of it had drifted toward the Nicene formula. Was the settlement''s beneficiary base stable, or was the middle always exit-bound?',
    'Track council rosters and episcopal careers from Ancyra (358) through the homoian collapse (364-366): the direction and rate of middle-party drift measures the beneficiary base''s erosion.',
    'If the middle was always transit, the settlement''s persistence rested on the committed core plus passengers, its coordination function was thinner than roster size suggests, and the semi-Arian sibling reading is best understood as this arrangement''s exit corridor rather than a stable alternative.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(homoiousian_drift_erosion, empirical, 'Whether the moderate beneficiary base was stable or structurally exit-bound.').

omega_variable(
    core_identity_fusion,
    'Was the committed core''s persistence doctrinal conviction, or identity-constituted opposition — the reading of homoousios as materialist (making the divine composite and divisible) and as crypto-Sabellian (collapsing the Son into the Father) that made exit unthinkable for the Eusebian hardliners?',
    'Compare exit behavior across the network''s strata: hardliners (Aetius, Eunomius) never exit; moderates exit through the homoiousian corridor; opportunists exit with each court reversal. The gradient maps identity fusion against career interest.',
    'If the core is identity-locked rather than merely constrained, the network seat''s exit atom should be identity_locked, its effective directionality sits nearer the beneficiary end, and the settlement''s collapse under Theodosius is best read as identity persistence underground (the Gothic churches) rather than conversion.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(core_identity_fusion, conceptual, 'Whether the committed core''s persistence is identity fusion or ordinary career constraint.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(homoousios_christology__arian_reading, 318, 381).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(homo_tr_t318, homoousios_christology__arian_reading, theater_ratio, 318, 0.08).
narrative_ontology:measurement(homo_tr_t325, homoousios_christology__arian_reading, theater_ratio, 325, 0.12).
narrative_ontology:measurement(homo_tr_t335, homoousios_christology__arian_reading, theater_ratio, 335, 0.22).
narrative_ontology:measurement(homo_tr_t346, homoousios_christology__arian_reading, theater_ratio, 346, 0.25).
narrative_ontology:measurement(homo_tr_t356, homoousios_christology__arian_reading, theater_ratio, 356, 0.48).
narrative_ontology:measurement(homo_tr_t363, homoousios_christology__arian_reading, theater_ratio, 363, 0.3).
narrative_ontology:measurement(homo_tr_t372, homoousios_christology__arian_reading, theater_ratio, 372, 0.38).
narrative_ontology:measurement(homo_tr_t381, homoousios_christology__arian_reading, theater_ratio, 381, 0.42).

% Extraction over time
narrative_ontology:measurement(homo_be_t318, homoousios_christology__arian_reading, base_extractiveness, 318, 0.12).
narrative_ontology:measurement(homo_be_t325, homoousios_christology__arian_reading, base_extractiveness, 325, 0.06).
narrative_ontology:measurement(homo_be_t335, homoousios_christology__arian_reading, base_extractiveness, 335, 0.3).
narrative_ontology:measurement(homo_be_t346, homoousios_christology__arian_reading, base_extractiveness, 346, 0.22).
narrative_ontology:measurement(homo_be_t356, homoousios_christology__arian_reading, base_extractiveness, 356, 0.66).
narrative_ontology:measurement(homo_be_t363, homoousios_christology__arian_reading, base_extractiveness, 363, 0.35).
narrative_ontology:measurement(homo_be_t372, homoousios_christology__arian_reading, base_extractiveness, 372, 0.5).
narrative_ontology:measurement(homo_be_t381, homoousios_christology__arian_reading, base_extractiveness, 381, 0.1).

% Suppression requirement over time
narrative_ontology:measurement(homo_su_t318, homoousios_christology__arian_reading, suppression_requirement, 318, 0.1).
narrative_ontology:measurement(homo_su_t325, homoousios_christology__arian_reading, suppression_requirement, 325, 0.05).
narrative_ontology:measurement(homo_su_t335, homoousios_christology__arian_reading, suppression_requirement, 335, 0.3).
narrative_ontology:measurement(homo_su_t346, homoousios_christology__arian_reading, suppression_requirement, 346, 0.2).
narrative_ontology:measurement(homo_su_t356, homoousios_christology__arian_reading, suppression_requirement, 356, 0.7).
narrative_ontology:measurement(homo_su_t363, homoousios_christology__arian_reading, suppression_requirement, 363, 0.12).
narrative_ontology:measurement(homo_su_t372, homoousios_christology__arian_reading, suppression_requirement, 372, 0.45).
narrative_ontology:measurement(homo_su_t381, homoousios_christology__arian_reading, suppression_requirement, 381, 0.05).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(homoousios_christology__arian_reading, identity_coordination).
narrative_ontology:affects_constraint(homoousios_christology__arian_reading, homoousios_christology__pro_nicene_reading).
narrative_ontology:affects_constraint(homoousios_christology__arian_reading, homoousios_christology__semi_arian_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'the Arian controversy' covers three structurally distinct settlements — the arian, pro-Nicene, and semi-Arian readings of one kernel — each with its own epsilon, beneficiary/victim structure, and enforcement history. They are modeled as three linked stories per the epsilon-invariance principle: measuring 'the Christology settlement' under Arian ascendancy yields one arrangement; measuring it under Nicene ascendancy yields another — different constraints, not one constraint with a measurement parameter. This reading influences the pro-Nicene sibling by defining the problem Nicaea's formula answered, and influences the semi-Arian sibling as the coalition umbrella whose enforcement excesses created the moderates' exit corridor.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
