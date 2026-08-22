% ============================================================================
% CONSTRAINT STORY: reformation_composite__theological_fragmentation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_reformation_composite__theological_fragmentation_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: reformation_composite__theological_fragmentation_reading
 *   human_readable: Reformation Doctrinal Fragmentation (Theological Reading)
 *   domain: historical_epistemology/religious_history
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the contested Reformation kernel:
 *   the theological fragmentation reading. The constraint is the structural
 *   fragmentation of Christian soteriology and ecclesiology into
 *   irreconcilably different doctrinal positions, each institutionalized in a
 *   confessional community with its own authority structure. The reading
 *   asserts that this fragmentation is FUNDAMENTALLY THEOLOGICAL — driven by
 *   doctrinal incompatibility on salvific mechanism and church order — rather
 *   than primarily political (nation-states asserting sovereignty) or
 *   technological (printing press enabling mass dissemination). The
 *   extraction arises because denominational leadership benefits from
 *   fragmentary authority structures, lay Christians bear the cost of schism
 *   and identity-locking, and unified Christendom's magisterial claim is
 *   undermined. Sibling readings (political_realignment_reading,
 *   technological_mediation_reading) offer competing frames for the same
 *   historical Reformation; this story does not adjudicate between them but
 *   articulates what the theological reading instantiates.
 *
 * KEY AGENTS:
 *   - Reformed denominational leadership (organizers of confessional boundaries, beneficiary)
 *   - Roman Curia and papal magisterium (defenders of unified authority, payer)
 *   - Lay Christians navigating schism (identity-locked targets of doctrinal enforcement)
 *   - Confessional theological authorities (beneficiaries of doctrinal work)
 *   - Protestant reformers and theologians (beneficiary agenda-setters)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(reformation_composite__theological_fragmentation_reading, 0.68).
domain_priors:suppression_score(reformation_composite__theological_fragmentation_reading, 0.54).
domain_priors:theater_ratio(reformation_composite__theological_fragmentation_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(reformation_composite__theological_fragmentation_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(reformation_composite__theological_fragmentation_reading, suppression_requirement, 0.54).
narrative_ontology:constraint_metric(reformation_composite__theological_fragmentation_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(reformation_composite__theological_fragmentation_reading, accessibility_collapse, 0.71).
narrative_ontology:constraint_metric(reformation_composite__theological_fragmentation_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(reformation_composite__theological_fragmentation_reading, tangled_rope).
narrative_ontology:human_readable(reformation_composite__theological_fragmentation_reading, "Reformation Doctrinal Fragmentation (Theological Reading)").
narrative_ontology:topic_domain(reformation_composite__theological_fragmentation_reading, "historical_epistemology/religious_history").

domain_priors:requires_active_enforcement(reformation_composite__theological_fragmentation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(reformation_composite__theological_fragmentation_reading, 'e7ebc0d6-8be6-4a28-93f5-28bff942b503').
narrative_ontology:cs_kernel_codification('e7ebc0d6-8be6-4a28-93f5-28bff942b503', fixed_text).
narrative_ontology:cs_authority_grounding('e7ebc0d6-8be6-4a28-93f5-28bff942b503', extraction).
narrative_ontology:cs_interpretation_layer_present('e7ebc0d6-8be6-4a28-93f5-28bff942b503').
narrative_ontology:cs_reading_relation('e7ebc0d6-8be6-4a28-93f5-28bff942b503', reformation_composite__political_realignment_reading, influences).
narrative_ontology:cs_reading_relation('e7ebc0d6-8be6-4a28-93f5-28bff942b503', reformation_composite__technological_mediation_reading, influences).
narrative_ontology:cs_axiom('e7ebc0d6-8be6-4a28-93f5-28bff942b503', foundational, doctrinal_incompatibility_primary).
narrative_ontology:cs_axiom_status(doctrinal_incompatibility_primary, holdable).
narrative_ontology:cs_axiom_grounding('e7ebc0d6-8be6-4a28-93f5-28bff942b503', doctrinal_incompatibility_primary, deontological).
narrative_ontology:cs_axiom('e7ebc0d6-8be6-4a28-93f5-28bff942b503', foundational, denominational_pluralism_legitimate).
narrative_ontology:cs_axiom_status(denominational_pluralism_legitimate, holdable).
narrative_ontology:cs_axiom_grounding('e7ebc0d6-8be6-4a28-93f5-28bff942b503', denominational_pluralism_legitimate, deontological).
narrative_ontology:cs_reference_frame('e7ebc0d6-8be6-4a28-93f5-28bff942b503', unified_catholic_christendom).
narrative_ontology:cs_drift_state('e7ebc0d6-8be6-4a28-93f5-28bff942b503', confessional_settlement_stabilized, gap(codification_collapse, severe, true)).
narrative_ontology:cs_created_at('e7ebc0d6-8be6-4a28-93f5-28bff942b503', '').
narrative_ontology:cs_kernel_id(reformation_composite__theological_fragmentation_reading, reformation_composite).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(reformation_composite__theological_fragmentation_reading, reformed_denominational_leadership).
narrative_ontology:constraint_beneficiary(reformation_composite__theological_fragmentation_reading, confessional_theological_authorities).
narrative_ontology:constraint_victim(reformation_composite__theological_fragmentation_reading, latin_christendom_unified_claim).
narrative_ontology:constraint_victim(reformation_composite__theological_fragmentation_reading, lay_christians_navigating_schism).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(reformation_composite__theological_fragmentation_reading, lay_christians_navigating_schism).
narrative_ontology:constraint_beneficiary(reformation_composite__theological_fragmentation_reading, protestant_theologians_and_reformers).
narrative_ontology:constraint_victim(reformation_composite__theological_fragmentation_reading, roman_curia_and_papal_magisterium).
narrative_ontology:constraint_vindicates(reformation_composite__theological_fragmentation_reading, sola_scriptura_doctrine).
narrative_ontology:constraint_vindicates(reformation_composite__theological_fragmentation_reading, justification_by_faith_alone).
narrative_ontology:constraint_vindicates(reformation_composite__theological_fragmentation_reading, predestination_doctrine).
narrative_ontology:constraint_vindicates(reformation_composite__theological_fragmentation_reading, episcopal_vs_congregational_polity_debate).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Denominational pastors, theologians, and synodal bodies (Lutheran, Calvinist, Reformed, Radical Reformation groups) articulate and enforce doctrinal boundaries through confessional documents (Augsburg Confession, Heidelberg Catechism, Trent decrees). They consolidate authority by distinguishing their soteriological claims from rivals and anchoring membership in doctrinal fidelity. They benefit from the fragmentation because it entrenches their leadership role as arbiter of orthodoxy within their confessional boundary.
narrative_ontology:constraint_stakeholder(reformation_composite__theological_fragmentation_reading, reformed_denominational_leadership, agenda_setter,
    organized, generational, constrained, continental).

% The Roman Church loses its claim to unified doctrinal authority over Latin Christendom. It must actively defend its own confessional position (Council of Trent, 1545-1563) against Protestant soteriological claims, dedicating resources to doctrinal refinement rather than coordination of a unified faith. The constraint forces the Curia to articulate what it believes rather than assume unity.
narrative_ontology:constraint_stakeholder(reformation_composite__theological_fragmentation_reading, roman_curia_and_papal_magisterium, payer,
    institutional, civilizational, trapped, continental).

% Laypeople must choose or are assigned a denominational affiliation based on doctrinal boundaries they often do not fully understand. The fragmentation into confessionally distinct churches means their identity as Christian becomes indexed to soteriological doctrine (saved by faith vs. works, predestined vs. free will). They carry the cost of schism (religious violence, confusion, community rupture) and a limited benefit (access to reformed worship/theology in some cases, but at the price of doctrinal conformity enforcement).
narrative_ontology:constraint_stakeholder(reformation_composite__theological_fragmentation_reading, lay_christians_navigating_schism, payer,
    powerless, biographical, identity_locked, local).
narrative_ontology:stakeholder_secondary_role(reformation_composite__theological_fragmentation_reading, lay_christians_navigating_schism, beneficiary).

% Theologians who write confessions and defend doctrinal positions gain status and institutional position. The fragmentation creates a field of theological work: competing systematizations of soteriology, ecclesiology, and biblical hermeneutics become the basis for academic and ecclesiastical authority. Universities and seminaries consolidate around confessional identity, creating institutional beneficiaries from the doctrinal divide.
narrative_ontology:constraint_stakeholder(reformation_composite__theological_fragmentation_reading, confessional_theological_authorities, beneficiary,
    powerful, generational, mobile, continental).

% The institutional and theological claim to a single unified Christian faith under one authority is fragmented and delegitimized. This is not an agent but a proposition that loses institutional force. The constraint undermines it by making doctrinal heterodoxy permanent and legitimate within distinct confessional spaces.
narrative_ontology:constraint_stakeholder(reformation_composite__theological_fragmentation_reading, unified_christendom_claim, payer,
    analytical, civilizational, analytical, continental).
narrative_ontology:stakeholder_non_agent(reformation_composite__theological_fragmentation_reading, unified_christendom_claim).

% Luther, Calvin, Melanchthon, and successor theologians articulate soteriological and ecclesiological positions that become structural and rival to Rome. They benefit from the fragmentation by establishing themselves as authoritative voices, creating new institutional bases (universities, printing networks, ecclesiastical hierarchies) rooted in doctrinal distinctiveness. Their theological claims become enforceable by generating denominational communities organized around those claims.
narrative_ontology:constraint_stakeholder(reformation_composite__theological_fragmentation_reading, protestant_theologians_and_reformers, beneficiary,
    organized, generational, mobile, continental).
narrative_ontology:stakeholder_secondary_role(reformation_composite__theological_fragmentation_reading, protestant_theologians_and_reformers, agenda_setter).

% Anabaptists, spiritualists, and sectarian groups are excluded from the mainstream confessional settlements (Lutheran, Reformed, Catholic). Their theological claims about baptism, church discipline, and religious violence do not fit the magisterial Reformation's political settlement. They remain marginal despite sharing some soteriological commitments with Protestants.
narrative_ontology:constraint_stakeholder(reformation_composite__theological_fragmentation_reading, radical_reformation_groups, excluded,
    powerless, biographical, trapped, local).

% Scholars analyze the Reformation as theological, political, or technological event; this reading instantiates the theological framing. They see the doctrinal boundaries and confessional documents as primary observables and interpret the fragmentation as driven by soteriological and ecclesiological irreconcilability rather than external politics or technology.
narrative_ontology:constraint_stakeholder(reformation_composite__theological_fragmentation_reading, early_modern_historians, observer,
    analytical, generational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(reformation_composite__theological_fragmentation_reading, reformed_denominational_leadership).
narrative_ontology:fixing_cost_class(reformation_composite__theological_fragmentation_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a stable field of doctrinal differentiation: competing soteriological claims (justification mechanism, predestination, free will) and ecclesiological claims (polity, sacraments, authority structure) are articulated, bounded, and defended within distinct confessional communities. This solves the problem of legitimate theological pluralism by making the pluralism itself institutional and enforced rather than informal and contested.
% TRANSFER_FUNCTION: Transfers authority over Christian identity from a unified magisterium (Rome) to multiple confessional authorities. Lay adherents transfer deference to a denominational magisterium (Lutheran, Reformed, Catholic reformed). Theological work and institutional resources flow to those who articulate and defend doctrinal boundaries.
% ABSENT_VOICES: Radical Reformation groups, women theologians, indigenous Christian communities, and lay Christians who reject denominational categories are excluded from the mainstream confessional settlements. They would assert alternative soteriologies and church orders but are kept out by the institutional consolidation of magisterial confessions.
% DISAPPEARANCE_RATIONALE: If the doctrinal fragmentation vanished overnight — if all Christians reverted to accepting unified magisterial authority — the institutional and intellectual structures of Protestantism, Catholicism, and Orthodox Christianity would dissolve. Denominational hierarchies, confessional seminaries, distinct worship practices, and theological traditions rooted in soteriological difference would reorganize. The world does not rearrange to unified Christianity, but it rearranges significantly from the post-Reformation plural structure.
% FOUNDING_PROBLEM: Late medieval Christendom faced mounting theological contestation: questions about justification mechanism, papal authority, biblical hermeneutics, and sacramental efficacy generated competing answers that could not be adjudicated within a unified framework. The founding problem is: how can doctrinally incompatible claims about salvation and church order coexist within Christian faith?
% FOUNDING_PROBLEM_CORROBORATION: Protestant reformers and Catholic Counter-Reformation theologians both attest that the founding problem is live: soteriology and ecclesiology remain systematically contested, and each confessional community claims its answer is correct. They disagree on whether the contestation is a problem (requiring unified resolution) or a feature (allowing legitimate pluralism). Historians of theology and comparative religion attest from outside the benefiting parties that the doctrinal claims are irreducibly incompatible on their own logical grounds.
narrative_ontology:disappearance_verdict(reformation_composite__theological_fragmentation_reading, world_rearranges).
narrative_ontology:founding_problem_status(reformation_composite__theological_fragmentation_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(reformation_composite__theological_fragmentation_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(reformation_composite__theological_fragmentation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(reformation_composite__theological_fragmentation_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(reformation_composite__theological_fragmentation_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(reformation_composite__theological_fragmentation_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(reformation_composite__theological_fragmentation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness measures how much the fragmentation extracts from those who expected or depended on unified Christian authority. It starts low (1500: 0.12) when the theological contest is emerging but not yet institutionalized. It rises steeply by 1530 (0.38) as Lutheran and Reformed confessions solidify and Rome mobilizes Counter-Reformation. It plateaus by 1600 (0.68) as the confessional settlement becomes stable and enforced — extractive but no longer accelerating. Suppression measures the active enforcement of doctrinal boundaries: the index rises as denominational hierarchies enforce orthodoxy, excommunicate deviants, and suppress alternative soteriologies (Radical Reformation). Theater ratio measures performative work that does not advance the core doctrinal function: it rises as confessional polemics become ritualized, but remains modest (0.22) because the doctrinal work itself is substantive — the constraint's function is not purely theatrical. Accessibility collapse measures how completely alternatives close once the theological framing is accepted: once you embrace the theological reading, rival doctrinal positions appear logically irreconcilable, making alternatives inaccessible (0.71). Resistance measures active pushback: many laypeople and radical reformers resist denominational identity-locking, generating persistent resistance (0.58).
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of denominational leadership and theological authorities, the fragmentation is a legitimate response to irreconcilable doctrinal differences — a coordination mechanism that establishes stable confessional communities. From the perspective of the Roman Curia, the fragmentation is an extraction that undermines unified authority and forces defensive doctrinal work. From the perspective of lay Christians, the fragmentation imposes identity-lock (you must choose or be assigned a denomination) at the cost of schism and community rupture. These seats compute different types: beneficiary seats compute the constraint as a rope (coordination under shared theology); payer seats compute it as snare or tangled_rope (enforced fragmentation that extracts authority and imposes costs). The engine's per-seat classification captures this perspectival divergence from the structural data alone.
 *
 * DIRECTIONALITY LOGIC:
 *   The denominational leadership benefits from the fragmentation — they become authorities within their confessional space, and the fragmentation entrenches their role. Their directionality is low (d ~ 0.2), making them beneficiaries. The Roman Curia is a target — forced to defend its own doctrine and lose unified authority — with directionality high (d ~ 0.8). Lay Christians are trapped between benefit (access to reformed worship/theology) and cost (schism, forced denomination choice): their directionality is near symmetric (d ~ 0.5) reflecting the secondary_role split. The constraint's effective extraction is amplified for the Curia (institutional power, large scope) and modulated differently for lay Christians (powerless but identity-locked, local scope). Theological authorities sit near the beneficiary end because doctrinal fragmentation creates their institutional niche.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (how can doctrinally incompatible claims coexist?) remains live and contested. The constraint does not solve it — it institutionalizes pluralism as the answer. The confessional settlement is NOT mandatrophic: it actively maintains its function by enforcing denominational boundaries and articulating doctrinal distinctions. The theatrical component (confessional polemic, ritualized doctrinal defense) is present but modest, suggesting the constraint still performs a real coordination function — establishing stable spaces for distinct theologies — rather than persisting as pure theater.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    doctrinal_vs_contingent_causation,
    'Are the doctrinal differences (soteriology, ecclesiology) the primary DRIVER of the Reformation''s fragmentation, or are they post-hoc RATIONALIZATION of political and economic causes?',
    'Counterfactual analysis: if the same doctrinal disputes had arisen in a politically unified Christendom (no emerging nation-states), would they have generated institutionalized denominations? Conversely, if the same political and economic pressures existed without the theological disputes, would fragmentation have occurred?',
    'If doctrines are primary, the theological reading holds; effective extraction is real and driven by doctrinal boundaries. If doctrines are rationalization, the constraint decomposes — the primary constraint is political or economic, and theology is its legitimation cover. The classification would shift from tangled_rope to snare (pure extraction rationalized theologically).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(doctrinal_vs_contingent_causation, conceptual, 'Whether doctrinal differences drive fragmentation or rationalize it.').

omega_variable(
    incompatibility_claim_verification,
    'Are the competing soteriological and ecclesiological claims logically irreconcilable on their own grounds, or could a unified theology accommodate them as legitimate internal plurality?',
    'Systematic theological analysis of core doctrinal claims: can justification-by-faith-alone coexist with works-informed justification in one framework? Can congregational and episcopal polity coexist? Medieval Catholic theology before the Reformation did accommodate some plurality; the question is whether the Protestant claims are categorically incompatible or merely novel.',
    'If incompatible, the fragmentation reflects real doctrinal impossibility and the constraint is coordination around pluralism. If compatible, the fragmentation is partly enforced by leadership preference for boundary-maintenance, raising the extraction measure and shifting classification toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(incompatibility_claim_verification, empirical, 'Whether doctrinal claims are logically irreconcilable.').

omega_variable(
    lay_identity_lock_internalization,
    'Is the identity-locking of lay Christians (forced denomination choice) structural (external enforcement) or internalized (adherents identify with denominational theology as constitutive of self)?',
    'Post-Reformation historical analysis: when did laypeople internalize Protestant vs. Catholic identity as self-constitutive? Did internalization follow enforcement, or did voluntary theological commitment precede or exceed external coercion?',
    'If internalized, the suppression measure understates the actual lock-in; the constraint''s effective suppression is higher because the target carries the constraint''s logic after external enforcement relaxes. If structural only, suppression properly reflects external coercion and post-Reformation enforcement decay would reverse the lock-in.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(lay_identity_lock_internalization, empirical, 'Whether lay denomination identity-lock is structural or internalized.').

omega_variable(
    unified_christendom_naturalness,
    'Was pre-Reformation unified Christendom a natural or constructed state? If constructed, how does that change the interpretation of its fragmentation?',
    'Medieval history analysis: to what extent did pre-Reformation unity depend on enforcement mechanisms, exclusion of heterodox voices, or suppression of doctrinal contestation? Was unity a default or an achievement?',
    'If unified Christendom was itself constructed through suppression, the fragmentation is not extraction but release from an imposed constraint — reversing the victim/beneficiary structure for lay Christians. The classification might shift from tangled_rope to rope (liberation into legitimate plurality) from certain perspectives.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(unified_christendom_naturalness, conceptual, 'Whether pre-Reformation unity was natural or constructed.').

omega_variable(
    reading_vs_sibling_foreclosure,
    'Does the theological reading logically foreclose the political or technological readings, or do they coexist as alternative causal emphases on the same event?',
    'Logical analysis: if you hold that doctrinal irreconcilability is the primary driver, does that require denying that political pressures and technology also played causal roles? Or can all three be live simultaneously with different agents prioritizing different causal streams?',
    'If foreclosure obtains, the theological reading is the only defensible reading within a unified analytical framework — the political and technological readings are incoherent competitors. If coexistence obtains, the readings are complementary framings of multicausal change, and all three readings describe the same Reformation from different perspectives.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_vs_sibling_foreclosure, conceptual, 'Whether the theological reading forecloses or coexists with political and technological readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(reformation_composite__theological_fragmentation_reading, 1500, 1650).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(refo_tr_t1500, reformation_composite__theological_fragmentation_reading, theater_ratio, 1500, 0.05).
narrative_ontology:measurement_basis(refo_tr_t1500, observed).
narrative_ontology:measurement(refo_tr_t1530, reformation_composite__theological_fragmentation_reading, theater_ratio, 1530, 0.12).
narrative_ontology:measurement_basis(refo_tr_t1530, observed).
narrative_ontology:measurement(refo_tr_t1560, reformation_composite__theological_fragmentation_reading, theater_ratio, 1560, 0.18).
narrative_ontology:measurement_basis(refo_tr_t1560, observed).
narrative_ontology:measurement(refo_tr_t1600, reformation_composite__theological_fragmentation_reading, theater_ratio, 1600, 0.22).
narrative_ontology:measurement_basis(refo_tr_t1600, observed).
narrative_ontology:measurement(refo_tr_t1650, reformation_composite__theological_fragmentation_reading, theater_ratio, 1650, 0.22).
narrative_ontology:measurement_basis(refo_tr_t1650, observed).

% Extraction over time
narrative_ontology:measurement(refo_be_t1500, reformation_composite__theological_fragmentation_reading, base_extractiveness, 1500, 0.12).
narrative_ontology:measurement_basis(refo_be_t1500, observed).
narrative_ontology:measurement(refo_be_t1530, reformation_composite__theological_fragmentation_reading, base_extractiveness, 1530, 0.38).
narrative_ontology:measurement_basis(refo_be_t1530, observed).
narrative_ontology:measurement(refo_be_t1560, reformation_composite__theological_fragmentation_reading, base_extractiveness, 1560, 0.62).
narrative_ontology:measurement_basis(refo_be_t1560, observed).
narrative_ontology:measurement(refo_be_t1600, reformation_composite__theological_fragmentation_reading, base_extractiveness, 1600, 0.68).
narrative_ontology:measurement_basis(refo_be_t1600, observed).
narrative_ontology:measurement(refo_be_t1650, reformation_composite__theological_fragmentation_reading, base_extractiveness, 1650, 0.68).
narrative_ontology:measurement_basis(refo_be_t1650, observed).

% Suppression requirement over time
narrative_ontology:measurement(refo_su_t1500, reformation_composite__theological_fragmentation_reading, suppression_requirement, 1500, 0.15).
narrative_ontology:measurement_basis(refo_su_t1500, observed).
narrative_ontology:measurement(refo_su_t1530, reformation_composite__theological_fragmentation_reading, suppression_requirement, 1530, 0.38).
narrative_ontology:measurement_basis(refo_su_t1530, observed).
narrative_ontology:measurement(refo_su_t1560, reformation_composite__theological_fragmentation_reading, suppression_requirement, 1560, 0.52).
narrative_ontology:measurement_basis(refo_su_t1560, observed).
narrative_ontology:measurement(refo_su_t1600, reformation_composite__theological_fragmentation_reading, suppression_requirement, 1600, 0.54).
narrative_ontology:measurement_basis(refo_su_t1600, observed).
narrative_ontology:measurement(refo_su_t1650, reformation_composite__theological_fragmentation_reading, suppression_requirement, 1650, 0.54).
narrative_ontology:measurement_basis(refo_su_t1650, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(reformation_composite__theological_fragmentation_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(reformation_composite__theological_fragmentation_reading, 0.12).
narrative_ontology:affects_constraint(reformation_composite__theological_fragmentation_reading, reformation_composite__political_realignment_reading).
narrative_ontology:affects_constraint(reformation_composite__theological_fragmentation_reading, reformation_composite__technological_mediation_reading).

% DUAL FORMULATION NOTE:
% The Reformation kernel decomposes into three constraint stories, each a different reading of the same historical event. The theological_fragmentation_reading (this story) anchors the primary observable in doctrinal incompatibility and denominational boundary maintenance. The political_realignment_reading anchors it in state sovereignty claims and institutional reconfigurations. The technological_mediation_reading anchors it in printing press transformation of information distribution. Each reading instantiates a different ε, beneficiary/victim set, and type. They are linked via network.affects_constraints to flag their shared kernel identity and enable kernel-level analysis. The theological reading does not foreclose the others — they coexist as live alternative causal emphases — but it does influence both: doctrinal articulation shapes what political realignment claims can be sustained, and theological texts are what get distributed via technology. All three readings are required to fully describe the Reformation; this story instantiates only the theological frame.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(reformation_composite__theological_fragmentation_reading, powerless, 0.65).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
