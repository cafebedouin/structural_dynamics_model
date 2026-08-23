% ============================================================================
% CONSTRAINT STORY: biblical_divine_nature__modalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
    narrative_ontology:constraint_vindicates/2,
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
 *   constraint_id: biblical_divine_nature__modalist_reading
 *   human_readable: Modalist Reading: One Divine Person in Sequential Manifestations
 *   domain: theology/religious_authority/doctrinal_history
 *
 * SUMMARY:
 *   The modalist reading of the divine nature holds that Father, Son, and
 *   Spirit are successive modes or roles of one divine person — one God who
 *   manifests as Father in creation and law, as Son in redemption, and as
 *   Spirit in indwelling sanctification — rather than three simultaneous
 *   persons. Where this reading is held, it constitutes a complete doctrinal
 *   arrangement: worship is coordinated around the single name of Jesus
 *   (baptism in the name of Jesus Christ per Acts 2:38, Jesus-name prayer and
 *   hymnody), ministers are credentialed on assent to the mode-reading,
 *   converts from creedal churches are baptized again because their prior
 *   baptism is not held to be the apostolic form, and members who come to
 *   read the texts as three persons face pastoral discipline. The arrangement
 *   has run twice in history: among second-century circles around Noetus,
 *   Praxeas, and Sabellius, and since 1914 in the Oneness Pentecostal
 *   denominations, which claim to restore the apostolic plain sense. It
 *   delivers what its members cannot get elsewhere at the same price — the
 *   full deity of Jesus and strict monotheism together, without philosophical
 *   apparatus — and it holds its shape by keeping rival interpretive
 *   traditions out of the teaching office and the baptismal font. The claimed
 *   type and the metrics below are authored independently: the claim states
 *   what this story asserts is structurally true; the metrics describe the
 *   arrangement's observed operation; the engine computes each seat's
 *   classification from the structural data.
 *
 * KEY AGENTS:
 *   - modalist_teaching_authority: agenda-setter (institutional/constrained) — administers the doctrine, credentials ministers, enforces the single-name baptism; collects interpretive authority and institutional continuity
 *   - modalist_community_members: primary beneficiary with a payer shadow (organized/identity_locked) — receive coherent monotheistic piety, community, and assurance; pay conformity and narrowed interpretive freedom
 *   - internal_trinitarian_dissenters: primary target (powerless/identity_locked) — members who come to read the texts as three persons; face discipline, and exit means losing community and salvific certainty at once
 *   - rebaptized_converts: secondary target (moderate/constrained) — converts from creedal churches required to repudiate their prior baptism and undergo Jesus-name baptism
 *   - trinitarian_readers: excluded party (institutional/trapped) — the creedal mainstream, barred from the modalist conversation, its baptism declared invalid; historically the force that repudiated the reading
 *   - unitarian_readers: excluded party (organized/trapped) — reject the reading from the opposite flank as overloading the Son's deity
 *   - doctrinal_historians: analytical observer — sees the full structure across the ancient condemnations and the modern revival
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(biblical_divine_nature__modalist_reading, 0.48).
domain_priors:suppression_score(biblical_divine_nature__modalist_reading, 0.62).
domain_priors:theater_ratio(biblical_divine_nature__modalist_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(biblical_divine_nature__modalist_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(biblical_divine_nature__modalist_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(biblical_divine_nature__modalist_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(biblical_divine_nature__modalist_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(biblical_divine_nature__modalist_reading, resistance, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(biblical_divine_nature__modalist_reading, tangled_rope).
narrative_ontology:human_readable(biblical_divine_nature__modalist_reading, "Modalist Reading: One Divine Person in Sequential Manifestations").
narrative_ontology:topic_domain(biblical_divine_nature__modalist_reading, "theology/religious_authority/doctrinal_history").

domain_priors:requires_active_enforcement(biblical_divine_nature__modalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(biblical_divine_nature__modalist_reading, '3c0c53e9-dd9e-4ff0-a6e3-8d158fda8ed7').
narrative_ontology:cs_kernel_codification('3c0c53e9-dd9e-4ff0-a6e3-8d158fda8ed7', fixed_text).
narrative_ontology:cs_authority_grounding('3c0c53e9-dd9e-4ff0-a6e3-8d158fda8ed7', lineage).
narrative_ontology:cs_interpretation_layer_present('3c0c53e9-dd9e-4ff0-a6e3-8d158fda8ed7').
narrative_ontology:cs_reading_relation('3c0c53e9-dd9e-4ff0-a6e3-8d158fda8ed7', biblical_divine_nature__trinitarian_reading, forecloses).
narrative_ontology:cs_reading_relation('3c0c53e9-dd9e-4ff0-a6e3-8d158fda8ed7', biblical_divine_nature__unitarian_reading, forecloses).
narrative_ontology:cs_axiom('3c0c53e9-dd9e-4ff0-a6e3-8d158fda8ed7', foundational, one_person_sequential_manifestations).
narrative_ontology:cs_axiom_status(one_person_sequential_manifestations, holdable).
narrative_ontology:cs_axiom_grounding('3c0c53e9-dd9e-4ff0-a6e3-8d158fda8ed7', one_person_sequential_manifestations, theological).
narrative_ontology:cs_axiom('3c0c53e9-dd9e-4ff0-a6e3-8d158fda8ed7', secondary, jesus_name_baptism_as_apostolic_form).
narrative_ontology:cs_axiom_status(jesus_name_baptism_as_apostolic_form, holdable).
narrative_ontology:cs_axiom_grounding('3c0c53e9-dd9e-4ff0-a6e3-8d158fda8ed7', jesus_name_baptism_as_apostolic_form, theological).
narrative_ontology:cs_reference_frame('3c0c53e9-dd9e-4ff0-a6e3-8d158fda8ed7', apostolic_plain_sense_monotheism).
narrative_ontology:cs_drift_state('3c0c53e9-dd9e-4ff0-a6e3-8d158fda8ed7', post_nicene_settlement, gap(repudiation_pressure, severe, false)).
narrative_ontology:cs_created_at('3c0c53e9-dd9e-4ff0-a6e3-8d158fda8ed7', '').
narrative_ontology:cs_kernel_id(biblical_divine_nature__modalist_reading, biblical_divine_nature).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(biblical_divine_nature__modalist_reading, modalist_teaching_authority).
narrative_ontology:constraint_beneficiary(biblical_divine_nature__modalist_reading, modalist_community_members).
narrative_ontology:constraint_victim(biblical_divine_nature__modalist_reading, internal_trinitarian_dissenters).
narrative_ontology:constraint_victim(biblical_divine_nature__modalist_reading, rebaptized_converts).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(biblical_divine_nature__modalist_reading, modalist_community_members).
narrative_ontology:constraint_vindicates(biblical_divine_nature__modalist_reading, strict_numerical_monotheism).
narrative_ontology:constraint_vindicates(biblical_divine_nature__modalist_reading, plain_sense_hermeneutic).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Pastors, district superintendents, and denominational boards that teach the mode-reading, license ministers on doctrinal assent, and administer baptism into the single name of Jesus. They write the doctrinal statements, discipline congregations that drift toward rival readings, and defend the position in public debate against the creedal mainstream and unitarian critics at once. Their authority, livelihood, and institutional continuity are bound to the doctrine they administer; softening it would dissolve the office they hold, though individual leaders have occasionally crossed over at the cost of their position.
narrative_ontology:constraint_stakeholder(biblical_divine_nature__modalist_reading, modalist_teaching_authority, agenda_setter,
    institutional, generational, constrained, global).

% Attend congregations where worship, hymnody, and baptism all assume one divine person in successive manifestations. They receive a coherent theology that affirms the full deity of Jesus and strict monotheism together, a close-knit community, and assurance that their baptism is the apostolic form. They also carry the costs: assent to the doctrinal statement, narrowed interpretive freedom when they read the texts, and — if they come to see three persons in the texts — a choice between silence and leaving everything they belong to.
narrative_ontology:constraint_stakeholder(biblical_divine_nature__modalist_reading, modalist_community_members, beneficiary,
    organized, biographical, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(biblical_divine_nature__modalist_reading, modalist_community_members, payer).

% Members who, studying the texts, come to hold that Father and Son are distinct persons. They have no institutional standing, no organized caucus, and no tolerated teaching role; voicing the reading brings pastoral discipline, and the community's teaching ties the validity of their baptism and their assurance of salvation to the doctrine they now doubt. Exit means leaving family networks, community, and the assurance framework in a single move.
narrative_ontology:constraint_stakeholder(biblical_divine_nature__modalist_reading, internal_trinitarian_dissenters, payer,
    powerless, biographical, identity_locked, regional).

% People who join from creedal churches and are told their prior baptism is not the apostolic form; they undergo baptism again into the single name of Jesus and formally repudiate the earlier rite. The cost is borne at entry — renouncing a rite and a community they belonged to — after which they hold the same standing as other members. They accept the terms because the movement offers what they came for: the deity of Jesus and monotheism without philosophical argument.
narrative_ontology:constraint_stakeholder(biblical_divine_nature__modalist_reading, rebaptized_converts, payer,
    moderate, biographical, constrained, global).

% The creedal mainstream — councils, confessions, and the large majority of world Christianity — holding Father, Son, and Spirit to be distinct persons in one essence. They are barred from teaching in modalist denominations, their baptism is treated as invalid by those denominations, and their theologians condemned the mode-reading as the Sabellian heresy from Tertullian onward. They engage the arrangement only from outside, through polemic and apologetics; admission to the modalist conversation would require renouncing their core premise.
narrative_ontology:constraint_stakeholder(biblical_divine_nature__modalist_reading, trinitarian_readers, excluded,
    institutional, civilizational, trapped, global).

% Churches and theologians holding that the Father alone is God and the Son is subordinate or created. They are excluded from the modalist conversation like the creedal mainstream, and they reject the mode-reading from the opposite flank: identifying the Son with the Father himself overloads the Son's deity and erases the Father-Son distinction their subordinationist reading depends on.
narrative_ontology:constraint_stakeholder(biblical_divine_nature__modalist_reading, unitarian_readers, excluded,
    organized, generational, trapped, continental).

% Patristics scholars and historians of doctrine who study Noetus, Praxeas, Sabellius, and the modern Oneness revival. They hold no stake in the arrangement, publish the scholarship all parties argue from, and can see the whole structure — the second-century problem, the ancient condemnations, the twentieth-century revival — from outside every seat.
narrative_ontology:constraint_stakeholder(biblical_divine_nature__modalist_reading, doctrinal_historians, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(biblical_divine_nature__modalist_reading, modalist_teaching_authority).
narrative_ontology:fixing_cost_class(biblical_divine_nature__modalist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves, once and centrally, the problem of affirming the full deity of Christ and the divinity of the Spirit while holding strict numerical monotheism, without Greek essence/person metaphysics: one God manifests successively as Father (creator and lawgiver), Son (redeemer), and Spirit (indweller and sanctifier). It also coordinates a shared worship grammar — baptism into the single name of Jesus, Jesus-centered prayer and hymnody — and a plain-sense hermeneutic a community can teach without philosophical training.
% TRANSFER_FUNCTION: Moves doctrinal assent and ritual conformity from members and converts to the community and its teaching office: acceptance of the mode-reading, baptism or rebaptism into the single name of Jesus, and renunciation of rival interpretive traditions. Moves interpretive authority, ordination credentials, and institutional continuity to the teaching authority. Moves condemnation outward toward readers who hold rival traditions.
% ABSENT_VOICES: Trinitarian-tradition readers are barred from teaching in modalist denominations and their baptism is treated as invalid; they would argue the mode-reading collapses the biblical Father-Son distinctions, and they argue this only from outside. Unitarian-tradition readers are excluded from the same conversation and would argue the reading overstates the Son's deity. Inside the communities, members who come to dissent have no organized voice: discipline is individual and congregational, so dissent leaves rather than aggregates.
% DISAPPEARANCE_RATIONALE: If the arrangement vanished overnight, the Oneness denominations would have to adopt a rival divine-nature framework or dissolve: baptism into the single name would stop, ministerial credentialing would lose its doctrinal test, hymnody and worship grammar would shift, and the ecumenical landscape would lose one of its standing parties. The rival camps would not rearrange — they would merely lose an opponent.
% FOUNDING_PROBLEM: In the late second century, teachers applying divine titles to Jesus while holding absolute monotheism faced two live dangers: ditheistic drift toward two gods, and Gnostic emanation schemes multiplying divine beings. The modalist answer — one divine person in successive manifestations — was built to secure monotheism and the Son's full deity in a single move, with no apparatus beyond the text itself.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: patristics scholarship, written by scholars with no stake in the arrangement, treats the second-century monotheism/Christology tension as a real problem and the modalist response as a serious, internally coherent solution rather than a cover story. Both rival traditions attest the problem is still live by continuing to contest the modalist solution on the merits; neither claims the underlying problem has dissolved. No source outside the communities attests that the founding problem is dead.
narrative_ontology:disappearance_verdict(biblical_divine_nature__modalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(biblical_divine_nature__modalist_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(biblical_divine_nature__modalist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(biblical_divine_nature__modalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(biblical_divine_nature__modalist_reading, 0.48, 'stealth/ox-alpha', 'none', direct).

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
 *   The claimed type is tangled_rope: the arrangement genuinely coordinates (a real theological problem solved once, centrally, at lower philosophical overhead than either rival manages) AND extracts asymmetrically (dissenters and converts bear costs the beneficiaries do not), and it requires active enforcement (credentialing, congregational discipline, rebaptism administration) to hold against both flanks and internal drift. Extractiveness is moderate (0.48) rather than high because the goods delivered are real and the costs are interpretive and communal rather than material; suppression is higher (0.62) because persistence depends on keeping rival readings out of the teaching office and the baptismal font, not on participant preference alone; theater is low (0.15) because the doctrine functionally governs baptism, hymnody, and ordination — little of its maintenance is performative; accessibility_collapse (0.60) reflects that rival readings are pre-condemned inside the community yet remain externally available; resistance (0.65) reflects seventeen centuries of condemnation from the creedal flank, rejection from the unitarian flank, and steady churn of leavers. Identity-lock dynamics: members' exit is fused — the movement's identity IS the Jesus-name doctrine, and its teaching ties salvation itself to the single-name baptism, so leaving means risking damnation as the community defines it; if that fusion broke and members came to see the reading as one tradition among several, exit would normalize and both suppression and extractiveness would fall. Suppression is both structural (credentialing, discipline, rebaptism administration) and internalized (salvation-assurance fusion); the split is carried as an omega rather than resolved in the scalar. Dissenting members have no coalition structure — discipline is individual and congregational, so dissent exits rather than aggregates, which is what keeps the payer seat weak despite its numbers. The measurement series run on one shared grid (T0 = 1914, the 'New Issue' split when mass rebaptism began; T100 = approximately 2014), with every tracked metric authored at every point: enforcement machinery consolidated through mid-century as denominational structures and credentialing systems hardened, then plateaued; extractiveness crept up modestly with institutional maturation; theater stayed low throughout.
 *
 * PERSPECTIVAL GAP:
 *   The teaching-authority seat should compute a coordination-dominant classification: from where it stands, the arrangement is the plain sense of Scripture it stewards, and discipline is flock-protection. The internal dissenter and rebaptized-convert seats should compute extraction-dominant classifications: from where they stand, the same structure is a regime that disciplines their reading and invalidates their baptism. The excluded creedal and unitarian seats see a third structure again — a heresy-producing minority movement — from entirely outside the arrangement's jurisdiction. Same-level dynamics: members and dissenters hold nominally identical standing inside the congregation, but the constraint differentiates them by what they come to believe, so identical global position yields opposite experiences of the same rules. The engine computes this divergence from power, exit, and role; the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   The teaching authority is the structural beneficiary: it collects interpretive authority, credentials, and institutional continuity from the arrangement it administers (d near the beneficiary end, damped by the siege costs of defending against both flanks simultaneously). Members are beneficiaries with a payer shadow: genuine goods — coherence, community, assurance — flow to them, while conformity and interpretive narrowing flow from them; their identity-locked exit keeps their derived d nearer the target end than their benefit status alone would place it. Internal dissenters and rebaptized converts are the targets: they bear discipline, repudiation of prior baptism, and compounded exit costs, with the least exit of any seat (d near the full-target end). The excluded rival readers are condemned by the arrangement but not governed by it — they sit outside its jurisdiction, which is why their exclusion rather than their extraction is the structurally salient fact. The base_properties beneficiary and victim declarations map one-to-one onto these seats.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification prevents two mislabels. Reading the arrangement as pure extraction would erase the genuine coordination function — the monotheism-plus-Christ-deity problem is real, the reading solves it at lower philosophical overhead than either rival, and members are net collectors; the tangled-rope classification keeps that function on the books while naming who pays for it. Reading it as pure coordination would erase the enforcement asymmetry — rebaptism, credentialing, and discipline are real costs imposed on identifiable seats, and rival readings are actively kept out of the teaching office. Mandatrophy is not resolved: the founding problem (monotheism plus the Son's full deity, without apparatus) is live for the holding communities, corroborated from outside by both the scholarly literature and the continued polemic of both rival flanks, so the arrangement is not a vestige performing a dead function — the theater_ratio stays low and no agenda-setter maintains it inertially.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_instantiation,
    'This constraint is one reading of the kernel biblical_divine_nature: what structurally changes if a sibling reading is instantiated instead?',
    'Compare the sibling stories (biblical_divine_nature__trinitarian_reading, biblical_divine_nature__unitarian_reading): each names a different beneficiary/victim set, enforcement object, and epsilon over its own arrangement.',
    'The disagreement is located in two structural elements: the number of divine persons (one in successive modes vs three hypostases in one essence vs the Father alone) and whether the Son''s deity is the Father''s own person instantiated. A sibling reading flips the victim set entirely — the trinitarian reading''s arrangement condemned modalists as heretics rather than disciplining internal dissenters — and recomputes every seat''s classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_instantiation, conceptual, 'Committer structure: which kernel, which reading, what siblings would change structurally.').

omega_variable(
    epsilon_referent_scope,
    'Should this story''s epsilon assess the modalist arrangement by the modalist reading''s own lights (rival-constitution convention, adopted here), or assess the standing Trinitarian settlement as the modalist reading contests it (advocacy-stance convention)?',
    'A corpus-level convention decision, checked by comparing the three sibling stories: if all three share one referent (the settlement), their epsilons diverge by evaluative lens; if each instantiates its own arrangement, each epsilon is a self-assessment over its own standing arrangement.',
    'Under the advocacy-stance convention, epsilon rises well above 0.5 — the modalist sees the settlement as extracting conformity to philosophical apparatus, condemning plain-sense monotheists as heretics, and invalidating Jesus-name baptism — and the beneficiary set shifts to creedal teaching authorities, pushing the computed type toward the snare-flavored end. Under the adopted convention, epsilon stays moderate and the payer seats are internal to the arrangement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(epsilon_referent_scope, conceptual, 'Framing under-determination in the epsilon referent for rival-constitution kernel readings.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured coercive force structural (ministerial credentialing, congregational discipline, rebaptism administration) or internalized (salvation-assurance fusion, fusion of the movement''s identity with the doctrine)?',
    'Post-exit trajectory: track leavers from Oneness denominations; if the felt coercive force persists after the disciplinary machinery no longer applies to them, the internalized share is substantial.',
    'If substantially internalized, the constraint''s effective coercive force exceeds the structural measure — leavers carry the fusion with them, exit costs stay high even where external barriers fall, and the identity_locked exit classification hardens for the member and dissenter seats.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs internalized suppression mechanism in a doctrinal community.').

omega_variable(
    plain_sense_univocality,
    'Is the modalist reading actually the plain sense of the biblical texts, or one interpretive tradition among several that the texts sustain?',
    'Exegetical assessment of the divine-personhood texts'' univocality — whether the Father-Son distinction passages admit the mode-reading without strain — conducted by scholars outside all three reading communities.',
    'If the texts are multivocal, the arrangement is one enforced tradition among rivals rather than a discovery: its coordination claim weakens (it coordinates a hermeneutic choice, not the text''s own sense), and the cost of keeping rival readings out weighs more heavily in classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(plain_sense_univocality, conceptual, 'Whether the reading''s no-philosophical-apparatus claim rests on textual univocality.').

omega_variable(
    ancient_modern_continuity,
    'Is the modern Oneness instantiation continuous with ancient modalism (Noetus, Praxeas, Sabellius), or a distinct twentieth-century re-derivation?',
    'Historical-genealogical scholarship tracing transmission (or independent re-derivation) of the mode-reading between the patristic condemnations and the 1914 ''New Issue'' revival.',
    'If discontinuous, the ancient condemnations count as resistance to a predecessor arrangement rather than to this one, and the interval''s enforcement history stands on its own; if continuous, seventeen centuries of repudiation bear directly on this constraint''s resistance profile and drift state.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ancient_modern_continuity, empirical, 'Genealogical continuity between patristic modalism and modern Oneness Pentecostalism.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(biblical_divine_nature__modalist_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bibl_tr_t0, biblical_divine_nature__modalist_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(bibl_tr_t20, biblical_divine_nature__modalist_reading, theater_ratio, 20, 0.11).
narrative_ontology:measurement(bibl_tr_t40, biblical_divine_nature__modalist_reading, theater_ratio, 40, 0.12).
narrative_ontology:measurement(bibl_tr_t60, biblical_divine_nature__modalist_reading, theater_ratio, 60, 0.13).
narrative_ontology:measurement(bibl_tr_t80, biblical_divine_nature__modalist_reading, theater_ratio, 80, 0.14).
narrative_ontology:measurement(bibl_tr_t100, biblical_divine_nature__modalist_reading, theater_ratio, 100, 0.15).

% Extraction over time
narrative_ontology:measurement(bibl_be_t0, biblical_divine_nature__modalist_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(bibl_be_t20, biblical_divine_nature__modalist_reading, base_extractiveness, 20, 0.42).
narrative_ontology:measurement(bibl_be_t40, biblical_divine_nature__modalist_reading, base_extractiveness, 40, 0.44).
narrative_ontology:measurement(bibl_be_t60, biblical_divine_nature__modalist_reading, base_extractiveness, 60, 0.45).
narrative_ontology:measurement(bibl_be_t80, biblical_divine_nature__modalist_reading, base_extractiveness, 80, 0.46).
narrative_ontology:measurement(bibl_be_t100, biblical_divine_nature__modalist_reading, base_extractiveness, 100, 0.48).

% Suppression requirement over time
narrative_ontology:measurement(bibl_su_t0, biblical_divine_nature__modalist_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(bibl_su_t20, biblical_divine_nature__modalist_reading, suppression_requirement, 20, 0.53).
narrative_ontology:measurement(bibl_su_t40, biblical_divine_nature__modalist_reading, suppression_requirement, 40, 0.58).
narrative_ontology:measurement(bibl_su_t60, biblical_divine_nature__modalist_reading, suppression_requirement, 60, 0.6).
narrative_ontology:measurement(bibl_su_t80, biblical_divine_nature__modalist_reading, suppression_requirement, 80, 0.61).
narrative_ontology:measurement(bibl_su_t100, biblical_divine_nature__modalist_reading, suppression_requirement, 100, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(biblical_divine_nature__modalist_reading, identity_coordination).
narrative_ontology:affects_constraint(biblical_divine_nature__modalist_reading, biblical_divine_nature__trinitarian_reading).
narrative_ontology:affects_constraint(biblical_divine_nature__modalist_reading, biblical_divine_nature__unitarian_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'the biblical doctrine of God' covers three structurally distinct claims and decomposes into a three-story constraint family: the modalist reading (this file), the trinitarian reading, and the unitarian reading. Each story carries its own epsilon, its own beneficiary/victim structure, and its own enforcement object, linked by these network edges. The upstream story by institutional and empirical weight is the trinitarian reading — the settlement whose texts and condemnations the other two contest — with the modalist and unitarian readings as downstream contestations. Each story assesses its own arrangement by its own lights; cross-reading epsilon divergence is carried in omega variables, never averaged into a single story.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
