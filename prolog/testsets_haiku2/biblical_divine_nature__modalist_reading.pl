% ============================================================================
% CONSTRAINT STORY: biblical_divine_nature__modalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_biblical_divine_nature__modalist_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: biblical_divine_nature__modalist_reading
 *   human_readable: Modalist Divine Nature Reading: Sequential Modes of One Person
 *   domain: theology/religious_authority/doctrinal_history
 *
 * SUMMARY:
 *   Modalism—the reading that Father, Son, and Spirit are sequential modes or
 *   roles of one divine person, not three simultaneous persons—is one of
 *   three major hermeneutical responses to the early Christian problem of
 *   reconciling biblical monotheism with the threefold revelation of God in
 *   Scripture. The modalist reading enables Jesus-centered devotional
 *   communities to affirm his full divinity without trinitarian metaphysics,
 *   and it claims to preserve biblical literalism against philosophical
 *   apparatus. However, it has been formally rejected as heretical (Sabellian
 *   heresy) by ecumenical councils and institutional Christianity. The
 *   constraint operates as a tangled rope: it provides genuine coordination
 *   (solving the exegetical and devotional problem for those who adopt it)
 *   while simultaneously extracting authority from institutional orthodoxy
 *   and imposing suppression through doctrinal enforcement. The reading is
 *   institutionally delegitimized yet textually intelligible, generating
 *   persistent low-level pressure from Scripture-alone reformers and
 *   Jesus-centered communities whose natural reading aligns with modalism.
 *
 * KEY AGENTS:
 *   - modalist_interpreters: theologians and exegetes advancing modalist readings; hold interpretive authority
 *   - jesus_centered_devotional_communities: believers practicing Jesus piety without trinitarian apparatus; benefit from the reading's simplicity; identity-locked to it
 *   - trinitarian_orthodoxy_defenders: institutional churches enforcing trinitarian doctrine; bear the cost of active suppression
 *   - ecumenical_authority_bodies: councils and creedal traditions that fixed trinitarian boundaries; gatekeeping agents
 *   - scripture_alone_reformers: excluded parties whose natural reading generates constant modalist pressure
 *   - philosophical_theologians: analysts evaluating the logical coherence of competing frameworks; observers
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(biblical_divine_nature__modalist_reading, 0.62).
domain_priors:suppression_score(biblical_divine_nature__modalist_reading, 0.58).
domain_priors:theater_ratio(biblical_divine_nature__modalist_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(biblical_divine_nature__modalist_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(biblical_divine_nature__modalist_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(biblical_divine_nature__modalist_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(biblical_divine_nature__modalist_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(biblical_divine_nature__modalist_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(biblical_divine_nature__modalist_reading, tangled_rope).
narrative_ontology:human_readable(biblical_divine_nature__modalist_reading, "Modalist Divine Nature Reading: Sequential Modes of One Person").
narrative_ontology:topic_domain(biblical_divine_nature__modalist_reading, "theology/religious_authority/doctrinal_history").

domain_priors:requires_active_enforcement(biblical_divine_nature__modalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(biblical_divine_nature__modalist_reading, '2dc2954b-e2a6-4021-b7c9-fa17ed64cad8').
narrative_ontology:cs_kernel_codification('2dc2954b-e2a6-4021-b7c9-fa17ed64cad8', fixed_text).
narrative_ontology:cs_authority_grounding('2dc2954b-e2a6-4021-b7c9-fa17ed64cad8', extraction).
narrative_ontology:cs_interpretation_layer_present('2dc2954b-e2a6-4021-b7c9-fa17ed64cad8').
narrative_ontology:cs_reading_relation('2dc2954b-e2a6-4021-b7c9-fa17ed64cad8', biblical_divine_nature__trinitarian_reading, coexists_with).
narrative_ontology:cs_reading_relation('2dc2954b-e2a6-4021-b7c9-fa17ed64cad8', biblical_divine_nature__unitarian_reading, coexists_with).
narrative_ontology:cs_axiom('2dc2954b-e2a6-4021-b7c9-fa17ed64cad8', foundational, sequential_modes_monotheism).
narrative_ontology:cs_axiom_status(sequential_modes_monotheism, holdable).
narrative_ontology:cs_axiom_grounding('2dc2954b-e2a6-4021-b7c9-fa17ed64cad8', sequential_modes_monotheism, empirically_contingent).
narrative_ontology:cs_axiom('2dc2954b-e2a6-4021-b7c9-fa17ed64cad8', foundational, biblical_narrative_sufficiency).
narrative_ontology:cs_axiom_status(biblical_narrative_sufficiency, holdable).
narrative_ontology:cs_axiom_grounding('2dc2954b-e2a6-4021-b7c9-fa17ed64cad8', biblical_narrative_sufficiency, conventional).
narrative_ontology:cs_reference_frame('2dc2954b-e2a6-4021-b7c9-fa17ed64cad8', apostolic_scripture_literal_reading).
narrative_ontology:cs_drift_state('2dc2954b-e2a6-4021-b7c9-fa17ed64cad8', post_nicene_orthodoxy_establishment, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('2dc2954b-e2a6-4021-b7c9-fa17ed64cad8', '').
narrative_ontology:cs_kernel_id(biblical_divine_nature__modalist_reading, biblical_divine_nature).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(biblical_divine_nature__modalist_reading, jesus_centered_devotional_communities).
narrative_ontology:constraint_victim(biblical_divine_nature__modalist_reading, trinitarian_orthodoxy_defendants).
narrative_ontology:constraint_victim(biblical_divine_nature__modalist_reading, subordination_doctrine_communities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(biblical_divine_nature__modalist_reading, trinitarian_orthodoxy_defenders).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Theologians and exegetes reading Scripture as describing God in sequential functional modes: Father in Old Testament law-giving and judgment, Son in New Testament incarnation and redemption, Spirit in post-Ascension presence and transformation. They author commentaries, lead teaching communities, and defend their reading against both trinitarian charge of Sabellian heresy and unitarian charge of insufficient monotheism. They claim their reading preserves biblical literalism and strict monotheism without importing Platonic philosophical apparatus. Their authority is interpretive and communal, not institutional; they are excluded from mainstream seminaries and magisterial church teaching offices.
narrative_ontology:constraint_stakeholder(biblical_divine_nature__modalist_reading, modalist_interpreters, agenda_setter,
    organized, generational, constrained, regional).

% Communities practicing Jesus-centered piety, mysticism, and devotion without the cognitive overhead of trinitarian hypostasis metaphysics. For them, modalism solves the key problem: Jesus is genuinely God (not a created subordinate, not a temporary mode), yet God remains one (strict monotheism, no tritheism charge). Their religious identity is constituted around 'Jesus is God, purely and simply'—a claim that modalism preserves and that they cannot relinquish without reconstructing their entire prayer and devotional life. They do not formally study theology; they inherit or adopt modalism through devotional literature and community practice.
narrative_ontology:constraint_stakeholder(biblical_divine_nature__modalist_reading, jesus_centered_devotional_communities, beneficiary,
    moderate, biographical, identity_locked, regional).

% Bishops, theological authorities, and magisterial church institutions (Roman Catholic, Eastern Orthodox, Protestant reformed churches) that officially teach trinitarian doctrine as the settled truth of Scripture and tradition. They are the primary enforcers of the constraint: they conduct councils, synods, and ecclesiastical proceedings that exclude modalist clergy; they define orthodoxy creeds (Nicene, Chalcedonian, Athanasian) that require trinitarian assent for sacramental participation; they maintain publishing and teaching institutions that marginaliz modalist theology. Their institutional legitimacy depends on trinitarian doctrine being THE correct interpretation of Scripture; if modalism is readmitted as a live option, their authority to define orthodoxy is undermined. They pay through continuous enforcement labor and doctrinal policing.
narrative_ontology:constraint_stakeholder(biblical_divine_nature__modalist_reading, trinitarian_orthodoxy_defenders, payer,
    institutional, civilizational, constrained, global).

% Communities and theologians reading Scripture as teaching Son and Spirit subordination to the Father (either ontological subordination—they are not eternal, not fully divine—or functional subordination—they are eternally divine but subordinate in role). They defend their exegetical ground against both modalist challenge (modalism refuses subordination, claiming one identical person) and trinitarian challenge (the 'subordination in the economy' explanation attempts to reconcile subordination language with trinitarian full equality). They are small in contemporary institutional Christianity but historically significant (4th-5th century Arian communities, Socinian movements, some contemporary non-trinitarian Protestants). They pay by defending their reading against stronger institutional opponents.
narrative_ontology:constraint_stakeholder(biblical_divine_nature__modalist_reading, subordination_doctrine_communities, payer,
    organized, biographical, constrained, regional).

% The formal councils and creedal traditions that authoritatively fixed trinitarian doctrine as the boundary of Christian orthodoxy (Council of Nicaea 325 CE, Council of Constantinople 381 CE, Council of Chalcedon 451 CE, and subsequent synods). They maintain the boundary by excluding modalism (anathema against Sabellius and modalist doctrine) and by requiring assent to trinitarian language as a condition of communion. Their authority to define orthodoxy depends entirely on the trinitarian boundary holding firm. If modalism is readmitted as a live interpretive option, 'orthodoxy' becomes incoherent (three different readings are all 'orthodox'?) and the councils' authority dissolves. They are trapped: they cannot reverse the anathemas without surrendering their power to define doctrine.
narrative_ontology:constraint_stakeholder(biblical_divine_nature__modalist_reading, ecumenical_authority_bodies, agenda_setter,
    institutional, civilizational, trapped, global).

% Protestant and other reform movements claiming Scripture as the sole authority for doctrine, rejecting ecumenical councils and philosophical apparatus. If they read Scripture without the framework of councils and Platonic philosophy, they naturally gravitate toward modalism (God presents himself as Father, then as Son, then as Spirit in biblical narrative—no need for hypostasis metaphysics). Yet most reformers inherited trinitarian confessions (Reformed confessions, Lutheran confessions, Wesleyan confessions all affirm trinitarianism formally) and thus internalized the boundary. They are excluded from having their natural exegetical reading admitted into mainstream conversation by the institutional framework they inherited—their Scripture-alone principle should lead to modalism, but the confessional tradition suppresses it.
narrative_ontology:constraint_stakeholder(biblical_divine_nature__modalist_reading, scripture_alone_reformers, excluded,
    organized, biographical, constrained, regional).

% Academic theologians and philosophers (including trinitarian scholars, modalist scholars, and unitarian scholars) who analyze the logical coherence and exegetical warrant of competing frameworks. They produce scholarship that influences both how communities frame the problem and how institutional authorities respond to it. They may defend or critique modalism on philosophical grounds; they serve as expert witnesses in the theological dispute. Their role is to clarify the structure of the problem, not to declare a winner—that is institutional authority's role.
narrative_ontology:constraint_stakeholder(biblical_divine_nature__modalist_reading, philosophical_theologians, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(biblical_divine_nature__modalist_reading, ecumenical_authority_bodies).
narrative_ontology:fixing_cost_class(biblical_divine_nature__modalist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a framework for understanding God's nature that preserves strict numerical monotheism while affirming Jesus's full divinity and the Spirit's reality as distinct biblical figures. Solves the exegetical problem of reconciling monotheism (God is one) with the biblical narrative of God's self-revelation in three distinguishable modes (Father in law-giving, Son in incarnation/redemption, Spirit in post-resurrection presence). Enables devotional communities to practice Jesus-centered piety without requiring philosophical apparatus (hypostasis/ousia metaphysics) that many believers find inaccessible or non-biblical.
% TRANSFER_FUNCTION: Transfers theological and hermeneutical authority FROM ecumenical councils (which enforce trinitarian doctrine through creedal requirement and institutional gatekeeping) TO direct Scripture reading and community-based interpretation. Modalism claims that councils added non-biblical philosophical apparatus and that returning to Scripture alone recovers simpler, more direct truth. The arrangement moves legitimacy from institutional creedal authority to interpretive communities' claims to biblical fidelity—a transfer that weakens institutional control and threatens the councils' power to define orthodoxy.
% ABSENT_VOICES: Communities practicing Scripture-alone exegesis without formal philosophical training are partially excluded from the official doctrinal conversation—they lack the technical vocabulary (hypostasis, ousia, coinherence, perichoresis) required for mainstream theological discourse, yet their intuitive reading of Scripture naturally aligns with modalism. Subordinationist communities are also excluded: both trinitarian and modalist frameworks treat them as biblically defeated, though subordinationists argue Scripture explicitly teaches functional or ontological distinction. Reform-minded Protestants claiming Scripture-alone authority are institutionally excluded from ecumenical councils (by their separation from the ancient church) yet their exegetical work constantly regenerates modalist-adjacent readings.
% DISAPPEARANCE_RATIONALE: If modalism as a live interpretive option disappeared entirely (full institutional suppression, internalization of trinitarian assumptions across all Scripture-reading communities), doctrinal authority would stabilize around trinitarianism and the councils' power would be secure. The world would rearrange: Scripture-alone reformers would either accept trinitarian complexity or shift to unitarian readings (neither comes naturally to them). If modalism disappeared because its reading of Scripture was recognized as more accurate and trinitarian doctrine was abandoned, institutional Christianity would reorganize around simplicity and biblical literalism. The verdict is contested because institutional defenders claim modalism was already logically refuted (so its disappearance is natural epistemic progress), while modalists and Scripture-alone communities claim its suppression requires active institutional enforcement (so it rearranges authority structures if removed).
% FOUNDING_PROBLEM: Early Christian theology faced an irreducible tension: Scripture describes God as one (monotheism), yet describes the Father, the Son, and the Spirit as distinct biblical figures with different roles and relationships. Three major solutions emerged in the early church: (1) modalism—God plays sequential roles like an actor; Father over Israel's history, Son in incarnation, Spirit after Ascension. (2) Trinitarianism—three eternal, coequal persons sharing one divine essence; distinct in hypostasis (mode of subsistence), identical in ousia (being). (3) Subordinationism—one supreme God (the Father) with divine subordinate powers (Son and Spirit). Modalism solved the founding problem by reframing: God is not three persons or three entities, but one person manifesting himself in three successive modes of self-revelation through salvation history—preserving monotheism without tripartite ontology and without requiring philosophical apparatus.
% FOUNDING_PROBLEM_CORROBORATION: Institutional ecumenical authorities (councils, magisterial churches) attest the founding problem is settled: trinitarianism is the correct solution, and modalism was a failed early attempt that councils rightly rejected. Philosophical theologians (including defenders of trinitarianism) acknowledge the founding problem was real and modalism was a serious, coherent attempted solution; they argue it fails on logical grounds (the 'modes problem': if Father and Son modes are sequential, either they are temporary, undermining their divinity, or simultaneous, undermining the sequential claim; if God reveals himself sequentially in time, is the mode not also temporal?), on exegetical grounds (John's prologue presents Father and Son in mutual relation, not sequential), and on incarnational grounds (if Jesus is God in mode, what is the 'I' that prays to 'the Father' in Gethsemane?). Scripture-alone reformers and Jesus-centered devotional communities implicitly attest that modalism solves their problem better than alternatives, but most have not explicitly chosen it (they inherited trinitarian language and confessions). No major institutional authority currently defends modalism as true—it remains anathematized as Sabellian heresy across ecumenical, Catholic, Orthodox, and Reformed traditions. The founding problem is dead in the sense that no mainstream orthodoxy treats it as unsolved; modalism survives as a suppressed but not-extinct reading, inertially maintained in small communities rather than actively defended by institutions.
narrative_ontology:disappearance_verdict(biblical_divine_nature__modalist_reading, contested).
narrative_ontology:founding_problem_status(biblical_divine_nature__modalist_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(biblical_divine_nature__modalist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(biblical_divine_nature__modalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(biblical_divine_nature__modalist_reading, 0.62, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(biblical_divine_nature__modalist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(biblical_divine_nature__modalist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(biblical_divine_nature__modalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62 at interval end) measures how much the modalist reading extracts authority from ecumenical councils and redirects it toward direct Scripture reading and community interpretation. It is neither low (the reading directly challenges institutional creedal authority) nor extremely high (it remains a coherent, defended theological position with real explanatory power for its adopters). Suppression (0.58) reflects the institutional machinery required to keep modalism anathematized: councils, synods, doctrinal enforcement, exclusion of modalist clergy. Theater (0.41 at end) indicates a moderate ratio of performative to functional activity: councils and magisterial churches maintain trinitarian orthodoxy partly through genuine systematic reasoning (functional) and partly through rote recitation of Nicene language and exclusion ritual (theatrical). The temporal measurements show a ratcheting pattern: extractiveness and suppression both rise sharply from the pre-council era (0-262 CE, modalism is live and unresolved) through Nicaea and Constantinople (262-381 CE, councils fix the boundary and enforcement machinery builds), then stabilize (900-1800 CE, enforcement is routinized but still required; the reading is suppressed but not extinct). Theater rises slower and more gradually, indicating that enforcement eventually becomes less about active refutation and more about institutional boundary maintenance.
 *
 * PERSPECTIVAL GAP:
 *   The modalist interpreters and Jesus-centered communities perceive this reading as liberating: it recovers biblical simplicity from philosophical baggage and enables unmediated Jesus devotion. They see the constraint as coordination (solving a real exegetical problem without metaphysical complexity). The trinitarian orthodoxy defenders and ecumenical authority bodies perceive it as threatening: it dissolves the boundary between orthodoxy and heresy, undermines the councils' authority, and reintroduces (they believe) a logically defective solution to the trinitarian problem. They see enforcement as necessary protection of truth. Philosophical theologians perceive the constraint as intellectually serious but defeated: they acknowledge modalism solved a real problem coherently but argue it fails on exegetical (modalism cannot account for the relational language in John's prologue—Father and Son are presented in mutual relation, not sequential) and logical grounds (the modes problem: if God sequentially adopts different modes, either the modes are temporary (making them not eternal properties, undermining their divinity) or they are eternal (making them simultaneous, undermining the sequential claim)). Scripture-alone reformers perceive it as natural but forbidden: their reading of Scripture naturally aligns with modalism, yet they are institutionally excluded from the conversation that would legitimize it.
 *
 * DIRECTIONALITY LOGIC:
 *   Modalist interpreters hold `organized` power and `constrained` exit: they can author and teach modalist theology, but their institutional options are limited (excluded from mainstream seminaries, publishing houses, pulpits; they must constitute separate communities or work in underground networks). They are not victims—they actively chose this reading—but they are targets of suppression. d ≈ 0.65-0.75 for this group: they bear the costs of exclusion while defending the reading. Jesus-centered devotional communities hold `moderate` power and `identity_locked` exit: they benefit from modalism because it solves their devotional problem without cognitive overhead, and they cannot exit without losing the doctrinal justification for Jesus's divinity that constitutes their religious identity. d ≈ 0.20-0.35: they benefit from the reading (low directionality toward victimization). Trinitarian orthodoxy defenders hold `institutional` power and `constrained` exit: they must enforce trinitarian doctrine to maintain their authority; if they stop enforcing, the boundary dissolves and their institutional standing is compromised. They are the payers—they bear the cost of maintaining suppression infrastructure. d ≈ 0.65-0.75: the arrangement extracts from them in the form of required enforcement labor. Subordination doctrine communities hold `organized` power but face pressure from both modalism (higher Jesus claims) and trinitarianism (the 'subordination in the economy' counter-explanation). They are also paying: they must defend their exegetical ground constantly. d ≈ 0.55-0.70.
 *
 * MANDATROPHY ANALYSIS:
 *   The modalist reading's founding problem—how to reconcile monotheism with the threefold revelation—is now dead (the problem is considered solved by trinitarian orthodoxy; modalism is read as a failed attempt). However, the constraint (the enforcement apparatus maintaining trinitarian orthodoxy against modalist pressure) persists actively, not inertially. This is NOT mandatrophy in the classical sense (a function that atrophied, leaving only theatrical enforcement). Instead, modalism generates persistent pressure from two sources: (1) Scripture-alone reformation movements, whose natural exegetical reading tends modalist; (2) Jesus-centered devotional communities, whose identity-lock to modalism creates ongoing demand for the reading. The institutional response is NOT pure theater—it requires ongoing defensive theology, doctrinal policing, and exclusion of modalist interpreters. A true piton would have neither concentrated beneficiary nor concentrated payer; modalism has both (ecumenical authorities benefit from the boundary being intact; modalist communities pay the cost of exclusion). It sits at the edge of mandatrophy (the original founding problem is dead, yet enforcement persists and is not purely theatrical), which is why the `theater_ratio` is moderate (0.41 at end, not extreme). The constraint's persistence is best explained as: institutional path-dependence (councils made a decision; reversing it would undermine institutional authority) + real ongoing theological dispute (the reading generates enough explanatory power that it cannot be completely suppressed).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    modalism_vs_trinitarianism_logical_coherence,
    'Is modalism logically coherent given the relational language of Scripture (John''s prologue, Paul''s ''sending'' language)? Or does the trinitarian framework better explain biblical duality-in-unity language?',
    'Systematic exegetical analysis comparing how each framework accounts for relational texts (John 1:1-2: ''the Word was with God and the Word was God''; John 10:30: ''I and the Father are one'' [which unity—essence or will?]). Philosophical analysis of the ''modes problem'': if Father and Son modes are sequential, are they eternal (undermining sequentiality) or temporary (undermining divinity)? If simultaneous, does modalism collapse into trinitarianism?',
    'If relational texts require distinct simultaneous persons, modalism is exegetically defective and the constraint is suppression of a false reading (snare). If the relational language can be explained as functional/revelational without requiring ontological distinction, modalism gains coherence and the suppression appears more extractive (snare charges strengthen). The logical-coherence question is foundational: if modalism is genuinely logically incoherent, the constraint''s enforcement appears justified; if coherent, the enforcement appears unjustified.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(modalism_vs_trinitarianism_logical_coherence, conceptual, 'Whether modalism is logically coherent or defeated by relational biblical language and the modes problem.').

omega_variable(
    structural_vs_internalized_suppression,
    'To what degree is modalism''s suppression structural (anathemas, institutional barriers, publishing exclusion) versus internalized (inherited trinitarian assumptions, cultural consensus, epistemological confidence in orthodoxy)?',
    'Historical counterfactual: if councils had never anathematized modalism and seminaries had published modalist theology alongside trinitarian, would independent communities naturally adopt modalism, or has the internalized suppression (centuries of trinitarian socialization) made modalism intuitively incoherent even without institutional barriers?',
    'If suppression is primarily structural, removing the institutional barriers would allow modalism to revive. If substantially internalized, the constraint''s suppression would persist even if institutions stopped enforcing it—targets would carry the suppression with them. This affects the remedy: institutional reform alone might not suffice if internalized suppression is dominant.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(structural_vs_internalized_suppression, empirical, 'Degree to which modalism''s suppression is structural (institutional barriers) versus internalized (cognitive/cultural).').

omega_variable(
    kernel_reading_genuine_diversity,
    'Is modalism a genuinely different reading of the same biblical kernel, or is it a logical variant that, once defeated, is correctly kept out of mainstream theological conversation?',
    'Community acceptance: do Scripture-alone reformed communities, when presented with modalism as a live option, find it more satisfying than trinitarian alternatives for their exegetical and devotional needs? Or do they see it as clearly defeated and correctly suppressed?',
    'If modalism is a genuine alternative reading with explanatory power, the constraint''s classification tilts toward unjust suppression (snare). If it is a logical dead-end correctly excluded, the constraint tilts toward legitimate boundary maintenance (rope or mountain-adjacent). The answer depends on whether the reading''s beneficiaries (Jesus-centered communities) represent a real constituency with distinctive exegetical and devotional needs, or whether they are simply not educated enough to understand why trinitarianism is necessary.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_genuine_diversity, preference, 'Whether modalism is a genuinely valuable alternative reading or a correctly-suppressed false solution.').

omega_variable(
    modalism_council_politics,
    'Was modalism anathematized primarily on exegetical/theological grounds (the reading is genuinely false), or was the decision politically motivated (councils wanted to enforce uniformity and philosophical sophistication, making modalism a target for institutional power consolidation)?',
    'Historical scholarship on council motivations, theological writings of the period, and analysis of whether the councils'' stated reasons (logical incoherence, misreading of Scripture) are consistent with the philosophical apparatus they adopted (Platonic hypostasis doctrine) or reflect defensive rationalization of an institutional decision.',
    'If anathematization was primarily political, the constraint''s suppression is extractive (snare). If primarily theological (the reading is genuinely incoherent), suppression is legitimate (rope or even mountain-adjacent if the incoherence is a logical necessity). The council''s adoption of Platonic philosophical apparatus (hypostasis, ousia) to define orthodoxy suggests that power consolidation was part of the motivation—using philosophy to exclude simpler readings from legitimacy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(modalism_council_politics, empirical, 'Whether modalism''s anathematization was primarily on theological or political grounds.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(biblical_divine_nature__modalist_reading, 0, 1800).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bibl_tr_t0, biblical_divine_nature__modalist_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(bibl_tr_t262, biblical_divine_nature__modalist_reading, theater_ratio, 262, 0.22).
narrative_ontology:measurement(bibl_tr_t325, biblical_divine_nature__modalist_reading, theater_ratio, 325, 0.28).
narrative_ontology:measurement(bibl_tr_t381, biblical_divine_nature__modalist_reading, theater_ratio, 381, 0.32).
narrative_ontology:measurement(bibl_tr_t900, biblical_divine_nature__modalist_reading, theater_ratio, 900, 0.4).
narrative_ontology:measurement(bibl_tr_t1500, biblical_divine_nature__modalist_reading, theater_ratio, 1500, 0.41).
narrative_ontology:measurement(bibl_tr_t1800, biblical_divine_nature__modalist_reading, theater_ratio, 1800, 0.41).

% Extraction over time
narrative_ontology:measurement(bibl_be_t0, biblical_divine_nature__modalist_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(bibl_be_t262, biblical_divine_nature__modalist_reading, base_extractiveness, 262, 0.48).
narrative_ontology:measurement(bibl_be_t325, biblical_divine_nature__modalist_reading, base_extractiveness, 325, 0.55).
narrative_ontology:measurement(bibl_be_t381, biblical_divine_nature__modalist_reading, base_extractiveness, 381, 0.58).
narrative_ontology:measurement(bibl_be_t900, biblical_divine_nature__modalist_reading, base_extractiveness, 900, 0.6).
narrative_ontology:measurement(bibl_be_t1500, biblical_divine_nature__modalist_reading, base_extractiveness, 1500, 0.62).
narrative_ontology:measurement(bibl_be_t1800, biblical_divine_nature__modalist_reading, base_extractiveness, 1800, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(bibl_su_t0, biblical_divine_nature__modalist_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(bibl_su_t262, biblical_divine_nature__modalist_reading, suppression_requirement, 262, 0.42).
narrative_ontology:measurement(bibl_su_t325, biblical_divine_nature__modalist_reading, suppression_requirement, 325, 0.51).
narrative_ontology:measurement(bibl_su_t381, biblical_divine_nature__modalist_reading, suppression_requirement, 381, 0.54).
narrative_ontology:measurement(bibl_su_t900, biblical_divine_nature__modalist_reading, suppression_requirement, 900, 0.56).
narrative_ontology:measurement(bibl_su_t1500, biblical_divine_nature__modalist_reading, suppression_requirement, 1500, 0.58).
narrative_ontology:measurement(bibl_su_t1800, biblical_divine_nature__modalist_reading, suppression_requirement, 1800, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(biblical_divine_nature__modalist_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(biblical_divine_nature__modalist_reading, 0.12).
narrative_ontology:affects_constraint(biblical_divine_nature__modalist_reading, biblical_divine_nature__trinitarian_reading).
narrative_ontology:affects_constraint(biblical_divine_nature__modalist_reading, biblical_divine_nature__unitarian_reading).

% DUAL FORMULATION NOTE:
% The biblical_divine_nature kernel decomposes into three structurally distinct constraint stories: modalist_reading (this file), trinitarian_reading, and unitarian_reading. Each instantiates a different reading of the same kernel (Scripture's account of God's nature), each has a different ε (modalism: 0.62 extractiveness; trinitarianism claims 0.0 as natural truth; unitarianism: 0.55-0.65), different beneficiary/victim sets, and different institutional fates. The three stories are linked by network edges showing that each reading influences the others: modalism forecloses trinitarianism's claim to scriptural uniqueness; trinitarianism influences modalism by defining orthodoxy boundaries; unitarianism coexists with both but under different power relations in different communities. No single story captures the theological dispute; the family captures the contested kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(biblical_divine_nature__modalist_reading, organized, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
