% ============================================================================
% CONSTRAINT STORY: biblical_divine_nature__unitarian_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_biblical_divine_nature__unitarian_reading, []).

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
 *   constraint_id: biblical_divine_nature__unitarian_reading
 *   human_readable: Unitarian Doctrinal Arrangement — Father Alone Is God, Flat Ecclesiology Reading
 *   domain: theology/religious authority/doctrinal history
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the kernel biblical_divine_nature
 *   — the unitarian reading: God is numerically singular, the Father alone is
 *   God, the Son and Spirit subordinate or created. The constraint modeled is
 *   the standing doctrinal arrangement that reading produces wherever it
 *   governs: a flat, congregational community held together by the fixed
 *   text's plain sense, with no magisterium, no interpretive buffer, and no
 *   mediating rents. Its structural delta from the sibling readings is low
 *   institutional authority, flat ecclesiology, and a victim set consisting
 *   of the institutional hierarchy and credal orthodoxy whose mediating
 *   authority the arrangement refuses. The arrangement has historically been
 *   more suppressed than suppressing — anathematized as the
 *   Constantinopolitan settlement extended (381), its carriers executed
 *   (Servetus, 1553) and expelled (Polish Brethren, 1658) — so its
 *   suppression profile is dominated by coercion directed against it, not
 *   coercion it deploys. Constraint-family note: the colloquial label 'the
 *   biblical doctrine of God' decomposes into three structurally distinct
 *   arrangements (this one; the trinitarian reading's credal-enforcement
 *   arrangement; the modalist reading's mode-conformity arrangement), each
 *   with its own ε, beneficiaries, and victims, linked through
 *   network.affects_constraints. Per the ε-invariance principle this story
 *   authors ε only for the unitarian arrangement as the unitarian reading
 *   assesses it: low — assent to a simple proposition, congregational
 *   discipline, no rent collection.
 *
 * KEY AGENTS:
 *   - unitarian_congregation_members: primary beneficiary (moderate / constrained) — coordinates worship and identity around the plain-sense reading; pays assent and exposure to congregational discipline
 *   - congregation_elders_and_ministers: agenda-setter and secondary beneficiary (moderate / mobile) — administers teaching and boundary discipline; the arrangement's only enforcement machinery
 *   - episcopal_institutional_hierarchy: primary external payer (institutional / constrained) — loses mediating authority wherever the arrangement wins adherents; historically the arrangement's persecutor rather than its subject
 *   - credal_orthodoxy_establishment: secondary payer and excluded voice (institutional / constrained) — carries the credal settlement the arrangement reclassifies as human innovation
 *   - internal_dissenters_and_doubters: internal payer (powerless / constrained) — bears excommunication and fellowship withdrawal at the boundary
 *   - historians_of_doctrine: analytical observer (analytical / analytical) — attests the descriptive history without endorsing the normative claim
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(biblical_divine_nature__unitarian_reading, 0.25).
domain_priors:suppression_score(biblical_divine_nature__unitarian_reading, 0.22).
domain_priors:theater_ratio(biblical_divine_nature__unitarian_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(biblical_divine_nature__unitarian_reading, extractiveness, 0.25).
narrative_ontology:constraint_metric(biblical_divine_nature__unitarian_reading, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(biblical_divine_nature__unitarian_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(biblical_divine_nature__unitarian_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(biblical_divine_nature__unitarian_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(biblical_divine_nature__unitarian_reading, rope).
narrative_ontology:human_readable(biblical_divine_nature__unitarian_reading, "Unitarian Doctrinal Arrangement — Father Alone Is God, Flat Ecclesiology Reading").
narrative_ontology:topic_domain(biblical_divine_nature__unitarian_reading, "theology/religious authority/doctrinal history").

domain_priors:requires_active_enforcement(biblical_divine_nature__unitarian_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(biblical_divine_nature__unitarian_reading, '1df6c893-01dc-40f7-872d-b9a16f115e99').
narrative_ontology:cs_kernel_codification('1df6c893-01dc-40f7-872d-b9a16f115e99', fixed_text).
narrative_ontology:cs_authority_grounding('1df6c893-01dc-40f7-872d-b9a16f115e99', diffuse_epistemic).
narrative_ontology:cs_reading_relation('1df6c893-01dc-40f7-872d-b9a16f115e99', biblical_divine_nature__trinitarian_reading, forecloses).
narrative_ontology:cs_reading_relation('1df6c893-01dc-40f7-872d-b9a16f115e99', biblical_divine_nature__modalist_reading, forecloses).
narrative_ontology:cs_axiom('1df6c893-01dc-40f7-872d-b9a16f115e99', foundational, father_alone_is_numerically_one_god).
narrative_ontology:cs_axiom_status(father_alone_is_numerically_one_god, holdable).
narrative_ontology:cs_axiom_grounding('1df6c893-01dc-40f7-872d-b9a16f115e99', father_alone_is_numerically_one_god, theological).
narrative_ontology:cs_axiom('1df6c893-01dc-40f7-872d-b9a16f115e99', secondary, son_and_spirit_subordinate_or_created_not_coessential).
narrative_ontology:cs_axiom_status(son_and_spirit_subordinate_or_created_not_coessential, holdable).
narrative_ontology:cs_axiom_grounding('1df6c893-01dc-40f7-872d-b9a16f115e99', son_and_spirit_subordinate_or_created_not_coessential, theological).
narrative_ontology:cs_reference_frame('1df6c893-01dc-40f7-872d-b9a16f115e99', apostolic_plain_sense_monotheism).
narrative_ontology:cs_drift_state('1df6c893-01dc-40f7-872d-b9a16f115e99', post_nicene_credal_era, gap(repudiation_pressure, severe, true)).
narrative_ontology:cs_created_at('1df6c893-01dc-40f7-872d-b9a16f115e99', '').
narrative_ontology:cs_kernel_id(biblical_divine_nature__unitarian_reading, biblical_divine_nature).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(biblical_divine_nature__unitarian_reading, unitarian_congregation_members).
narrative_ontology:constraint_beneficiary(biblical_divine_nature__unitarian_reading, congregation_elders_and_ministers).
narrative_ontology:constraint_victim(biblical_divine_nature__unitarian_reading, episcopal_institutional_hierarchy).
narrative_ontology:constraint_victim(biblical_divine_nature__unitarian_reading, credal_orthodoxy_establishment).
narrative_ontology:constraint_victim(biblical_divine_nature__unitarian_reading, internal_dissenters_and_doubters).
narrative_ontology:constraint_vindicates(biblical_divine_nature__unitarian_reading, plain_sense_sufficiency_of_scripture).
narrative_ontology:constraint_vindicates(biblical_divine_nature__unitarian_reading, subordinationist_christology).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Affirm the arrangement as the plain sense of the fixed text: one God, the Father; the Son his created or subordinate agent. They receive the community's identity goods — doctrinal clarity, unmediated access to the text, congregational belonging — and pay in assent and exposure to congregational discipline. Exit is real (trinitarian and post-Christian churches abound) but carries social and identity cost, and for many the congregation is their primary social world.
narrative_ontology:constraint_stakeholder(biblical_divine_nature__unitarian_reading, unitarian_congregation_members, beneficiary,
    moderate, biographical, constrained, global).

% Administer teaching, catechesis, and boundary discipline at congregational level — synods and elders' meetings in the historical branches, congregational votes in the modern ones. They collect standing and livelihood from the arrangement but command no coercive apparatus beyond fellowship withdrawal; their labor is the arrangement's only enforcement machinery. They can revise or drop the doctrinal requirement by congregational decision, at the price of schism with the convictional core.
narrative_ontology:constraint_stakeholder(biblical_divine_nature__unitarian_reading, congregation_elders_and_ministers, agenda_setter,
    moderate, biographical, mobile, global).
narrative_ontology:stakeholder_secondary_role(biblical_divine_nature__unitarian_reading, congregation_elders_and_ministers, beneficiary).

% The episcopal and institutional structure of the credal churches. Wherever the unitarian arrangement wins adherents, this hierarchy loses mediating authority: the confessions it enforces are reclassified as human tradition, and its role as necessary interpreter of the divine nature is refused. It cannot exit the contest — the arrangement's claim is aimed at its authority — but it retains countervailing institutional power and has historically been the arrangement's persecutor rather than its subject.
narrative_ontology:constraint_stakeholder(biblical_divine_nature__unitarian_reading, episcopal_institutional_hierarchy, payer,
    institutional, generational, constrained, global).

% Seminaries, confessional institutions, and orthodox theological establishments that carry the Nicene settlement. The arrangement reclassifies their credal formulas as post-apostolic innovation, stripping them of the status of restating the Bible's plain teaching. They would contest the reading's exegesis and its history, but inside the arrangement's conversation their testimony is pre-classified as tradition-bound and so not heard on the text's meaning.
narrative_ontology:constraint_stakeholder(biblical_divine_nature__unitarian_reading, credal_orthodoxy_establishment, payer,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(biblical_divine_nature__unitarian_reading, credal_orthodoxy_establishment, excluded).

% Members who drift toward credal formulas, deism, or post-Christianity inside unitarian congregations. They bear the arrangement's discipline: catechetical correction and, where they persist, withdrawal of fellowship or excommunication — the Socinian synods practiced it, and modern biblical-unitarian bodies withdraw fellowship from trinitarian-leaning members. Their exit is outward, but the congregation is often their whole social world, so the discipline lands on the people with the fewest alternatives.
narrative_ontology:constraint_stakeholder(biblical_divine_nature__unitarian_reading, internal_dissenters_and_doubters, payer,
    powerless, biographical, constrained, global).

% Study the kernel's history from outside every confessional seat: they attest that subordinationist readings predominated in the ante-Nicene period, that homoousios was novel and contested terminology even at Nicaea, and that the credal settlement was a development — without endorsing the reading's normative claim that the development was corruption.
narrative_ontology:constraint_stakeholder(biblical_divine_nature__unitarian_reading, historians_of_doctrine, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(biblical_divine_nature__unitarian_reading, diffuse).
narrative_ontology:fixing_cost_class(biblical_divine_nature__unitarian_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a community's worship, teaching, and membership around a shared reading of the fixed text — one God, the Father; the Son and Spirit as subordinate or created — through flat congregational structures rather than credal mediation, so that interpretive authority rests with lay readers of the text.
% TRANSFER_FUNCTION: Moves doctrinal assent and disciplinary standing between members and the gathered congregation, and relocates interpretive authority from credal institutions and the episcopal hierarchy to individual believers reading the text; correspondingly it refuses the transfers of credal assent, institutional deference, and hierarchical obedience that the rival credal arrangement collects from believers.
% ABSENT_VOICES: Two sets: credal-orthodox interlocutors, whose testimony the arrangement pre-classifies as tradition-bound and therefore inadmissible on the text's meaning; and internal dissenters, who are present in the congregations but silenced at the point of discipline — excommunicated Socinians, withdrawn-fellowship members in modern bodies. Historically, the arrangement's own carriers were the absent voices at the councils that fixed the kernel's dominant reading.
% DISAPPEARANCE_RATIONALE: The congregations living inside the arrangement would need a new doctrinal settlement within a generation — most would drift trinitarian-ward or post-Christian, as the liberal branch already did once its boundary discipline lapsed; the kernel contest would lose its only flat-ecclesiology reading; and the credal arrangement would face less contest at exactly the point (the divine nature) where its interpretive monopoly is most valuable.
% FOUNDING_PROBLEM: The arrangement was built against the credal settlement: its founders held that the church's post-apostolic creeds had replaced the Bible's plain monotheism with philosophical metaphysics — coessential deity, procession language — enforced by an episcopal hierarchy, and that worship of the Son as God violated the singularity the first commandment protects. Its founding problem was to restore and protect the text's plain-sense monotheism without credal or hierarchical mediation.
% FOUNDING_PROBLEM_CORROBORATION: Historians of doctrine outside the arrangement's beneficiary set corroborate the descriptive core: subordinationist readings predominated ante-Nicene, homoousios was novel and contested even at Nicaea, and the settlement was a development. They do not corroborate the normative claim that the development was corruption or that plain sense settles the exegesis — the credal churches attest the opposite, that the creeds preserved what the text always taught. The problem's status is therefore disputed between the arrangement and the establishment it contests, with external scholarship attesting the development but not the corruption.
narrative_ontology:disappearance_verdict(biblical_divine_nature__unitarian_reading, world_rearranges).
narrative_ontology:founding_problem_status(biblical_divine_nature__unitarian_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(biblical_divine_nature__unitarian_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(biblical_divine_nature__unitarian_reading, 'none', 1).
narrative_ontology:epsilon_provenance(biblical_divine_nature__unitarian_reading, 0.25, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(biblical_divine_nature__unitarian_reading_tests).
:- end_tests(biblical_divine_nature__unitarian_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.25 at interval end): the arrangement collects no rents — no tithing hierarchy, no mediating fees — and its extraction is assent to a proposition plus exposure to congregational discipline. Suppression is low-moderate (0.22): the arrangement's only coercive machinery is fellowship withdrawal, exit to other churches is genuinely available, and the scalar deliberately excludes the persecution the arrangement has suffered, which belongs to the rival credal arrangement's account. Theater is moderate at interval end (0.45) because the arrangement bifurcated: in the sectarian branch (biblical unitarian bodies) the doctrine is fully load-bearing, while the liberal branch's 1961 consolidation kept the unitarian name after the doctrinal content had largely lapsed — Goodhart drift visible in the series (theater 0.08 at 1658 rising to 0.55 at 1961). Accessibility collapse is low (0.35): rival readings of the same text have demonstrably persisted for seventeen centuries. Resistance is high (0.70): anathema, execution, expulsion, orthodox polemic, and continuous internal dissent. The suppression_requirement series is authored because this story specifically tracks enforcement-capacity change: the arrangement's discipline machinery built up under persecution (peak 0.55 at 1658), decayed with liberalization (0.06 at 1961), and was modestly rebuilt in sectarian revivals (0.22 at 2025). Claim/metric independence: the rope claim is authored from the arrangement's structure (genuine identity coordination, minimal coercive overhead, participants net beneficiaries, alternatives unsuppressed by this arrangement); the metrics from its observed operation; neither was tuned to the other or to a predicted engine output.
 *
 * PERSPECTIVAL GAP:
 *   The payer and beneficiary seats compute differently. From the member and elder seats the arrangement is a rope: identity coordination bought with assent and modest discipline. From the episcopal and credal-establishment seats the same arrangement is a stripping mechanism — it devalues the asset (interpretive monopoly) their authority rests on, and declared victim status drives their computed extraction toward the target end. The reading's own lights deny those seats' losses are wrongful extraction: what the hierarchy loses, on this reading, is rent it should never have collected. The engine computes the effective extraction; the moral gloss is the reading's, and the divergence between the structural computation and the reading's self-assessment is the measurement. The internal dissenter's seat diverges from both: from inside the congregation, discipline is fidelity to the text; from the dissenter's seat, it is exclusion administered by the only community they have — and part of that discipline's grip is internalized (the doubter has been formed to read their own doubt as unfaithfulness), which the suppression scalar does not separate from the structural component.
 *
 * DIRECTIONALITY LOGIC:
 *   Members and elders sit near the beneficiary end: the arrangement subsidizes them with identity goods and interpretive authority, and their payments (assent, discipline exposure) are small and internal. The hierarchy and credal establishment sit near the target end: they bear the arrangement's displacement costs wherever it wins adherents, with constrained exit — the contest is aimed at them and they cannot leave it. Internal dissenters are the sharpest targets: powerless within the discipline process, constrained exit, their exclusion is the boundary mechanism's direct product. The arrangement's global scope amplifies effective extraction modestly for the target seats (verifying doctrinal conformity across dispersed congregations is hard), but no seat approaches full-target extraction because no seat pays transfers — the extraction is status and standing, not rent. No directionality overrides were needed: the beneficiary/victim declarations plus the exit atoms produce the correct structural relationship for every seat.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification guards both failure modes. Reading the arrangement as a snare — tempting because its declared victims are prominent and its enforcement is real — would miss that no seat collects what the payers lose: the hierarchy's losses accrue to no beneficiary's account, and the receipt-surface check finds every seat empty of captured gains. Reading it as a mountain — the reading's own 'plain sense of scripture' framing invites exactly this — would miss that the arrangement is constructed, contested, and historically variable: emerges_naturally is honestly false and the naturalness claim is routed to an omega instead. The rope claim keeps the genuine coordination function visible while the measurement series tracks the drift that would degrade the liberal branch toward a piton if it completed: theater above 0.5 at 1961 marks the point where the unitarian name was maintained after its function had lapsed. The founding-problem interview shows the arrangement is not mandatrophy-resolved: the problem it was built for (credal metaphysics governing the text's meaning) is contested rather than dead, and external scholarship corroborates the development without the corruption.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'This constraint is one reading of the kernel biblical_divine_nature (reading: unitarian_reading). Is the unitarian reading the plain or natural sense of the fixed text — a discovered constraint — or one constructed reading among several?',
    'Historical-philological analysis of the text''s ante-Nicene reception and of how plain-sense judgments track the reader''s prior commitments; note that the trinitarian and modalist readings claim the same plain-sense warrant for incompatible conclusions.',
    'If the plain-sense claim holds, the arrangement''s fidelity framing strengthens and the credal arrangement''s extraction (enforcing metaphysics against the text''s plain teaching) rises; if not, all three readings stand as constructed competitors and the arrangement''s self-presentation as mere reading fails.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Whether the unitarian reading is the text''s natural sense or a constructed reading among siblings.').

omega_variable(
    sibling_structural_delta,
    'What would instantiating the trinitarian or modalist reading in place of this one change structurally?',
    'Compare the sibling stories directly: the trinitarian reading inverts this arrangement''s seat map — the episcopal hierarchy becomes agenda-setter, credal enforcement machinery appears, and the victim set becomes subordinationist dissenters (Servetus, the Polish Brethren); the modalist reading keeps low metaphysical machinery but concentrates conformity pressure on mode-language and personal devotion.',
    'Per-seat classifications flip across the family: seats computing as beneficiaries here compute as payers under the trinitarian story, and the persecution this arrangement''s carriers suffered lands in the sibling trinitarian story''s suppression account, not this one''s.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_structural_delta, conceptual, 'Cross-reading structural delta within the divine-nature kernel family.').

omega_variable(
    suppression_asymmetry_ambiguity,
    'Does the arrangement''s measured suppression reflect its own coercive overhead or the coercion historically directed against it by the rival credal arrangement?',
    'Separate the records: internal discipline (excommunication, fellowship withdrawal, catechetical subscription) from external coercion (anathemas, expulsions, executions). The scalar and the suppression_requirement series track only the former; the latter belongs to the sibling trinitarian story''s account.',
    'If the two were conflated, the arrangement''s suppression would be overstated by an order of magnitude and its rope classification would be unstable; kept separate, the arrangement is among the least coercive in the corpus relative to the coercion it absorbed.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(suppression_asymmetry_ambiguity, empirical, 'Internal discipline versus externally imposed persecution in the suppression scalar.').

omega_variable(
    interpretive_layer_reemergence,
    'The arrangement''s authority is diffuse — text plus reason, no magisterium, no interpretive buffer — yet it repeatedly grew interpretive layers (the Racovian Catechism''s subscription function, denominational statements, modern statements of faith). Is the flat-ecclesiology frame stable, or does the arrangement drift toward the interpretive-layer structure it was built to refuse?',
    'Track whether confessional documents acquire binding status in revival movements: if subscription becomes a membership condition enforced by withdrawal, the diffuse-epistemic authority has grown an interpretation layer and the structural delta decays.',
    'If the layer hardens, authority grounding shifts toward practice or lineage, the no-interpretive-buffer brittleness resolves, and the enforcement profile rises toward the tangled-rope boundary; if it stays advisory, the flat frame holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(interpretive_layer_reemergence, empirical, 'Re-emergence of interpretive layers under diffuse epistemic authority.').

omega_variable(
    derenting_versus_extraction,
    'Are the institutional hierarchy and credal orthodoxy genuinely victims of this arrangement, or is the victim declaration the reading''s own polemical reframing of what it calls de-renting?',
    'Distinguish status losses borne by those seats (real, and what drives their computed extraction) from wrongful extraction (which the reading denies committing): the arrangement strips authority it deems illegitimately acquired; whether the acquisition was illegitimate is the kernel contest itself.',
    'If the hierarchy''s losses are de-renting, the arrangement is a rope whose targets are rent-holders; if the boundary discipline extracts from internal members asymmetrically enough, the arrangement sits at the tangled-rope boundary — the per-seat computation will show which seats carry the asymmetry.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(derenting_versus_extraction, conceptual, 'Whether the declared victims are extraction targets or displaced rent-holders.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(biblical_divine_nature__unitarian_reading, 325, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bdn_unitarian_tr_t325, biblical_divine_nature__unitarian_reading, theater_ratio, 325, 0.1).
narrative_ontology:measurement(bdn_unitarian_tr_t381, biblical_divine_nature__unitarian_reading, theater_ratio, 381, 0.1).
narrative_ontology:measurement(bdn_unitarian_tr_t1553, biblical_divine_nature__unitarian_reading, theater_ratio, 1553, 0.12).
narrative_ontology:measurement(bdn_unitarian_tr_t1605, biblical_divine_nature__unitarian_reading, theater_ratio, 1605, 0.1).
narrative_ontology:measurement(bdn_unitarian_tr_t1658, biblical_divine_nature__unitarian_reading, theater_ratio, 1658, 0.08).
narrative_ontology:measurement(bdn_unitarian_tr_t1785, biblical_divine_nature__unitarian_reading, theater_ratio, 1785, 0.18).
narrative_ontology:measurement(bdn_unitarian_tr_t1825, biblical_divine_nature__unitarian_reading, theater_ratio, 1825, 0.22).
narrative_ontology:measurement(bdn_unitarian_tr_t1961, biblical_divine_nature__unitarian_reading, theater_ratio, 1961, 0.55).
narrative_ontology:measurement(bdn_unitarian_tr_t2025, biblical_divine_nature__unitarian_reading, theater_ratio, 2025, 0.45).

% Extraction over time
narrative_ontology:measurement(bdn_unitarian_be_t325, biblical_divine_nature__unitarian_reading, base_extractiveness, 325, 0.2).
narrative_ontology:measurement(bdn_unitarian_be_t381, biblical_divine_nature__unitarian_reading, base_extractiveness, 381, 0.22).
narrative_ontology:measurement(bdn_unitarian_be_t1553, biblical_divine_nature__unitarian_reading, base_extractiveness, 1553, 0.3).
narrative_ontology:measurement(bdn_unitarian_be_t1605, biblical_divine_nature__unitarian_reading, base_extractiveness, 1605, 0.32).
narrative_ontology:measurement(bdn_unitarian_be_t1658, biblical_divine_nature__unitarian_reading, base_extractiveness, 1658, 0.35).
narrative_ontology:measurement(bdn_unitarian_be_t1785, biblical_divine_nature__unitarian_reading, base_extractiveness, 1785, 0.26).
narrative_ontology:measurement(bdn_unitarian_be_t1825, biblical_divine_nature__unitarian_reading, base_extractiveness, 1825, 0.22).
narrative_ontology:measurement(bdn_unitarian_be_t1961, biblical_divine_nature__unitarian_reading, base_extractiveness, 1961, 0.14).
narrative_ontology:measurement(bdn_unitarian_be_t2025, biblical_divine_nature__unitarian_reading, base_extractiveness, 2025, 0.25).

% Suppression requirement over time
narrative_ontology:measurement(bdn_unitarian_su_t325, biblical_divine_nature__unitarian_reading, suppression_requirement, 325, 0.15).
narrative_ontology:measurement(bdn_unitarian_su_t381, biblical_divine_nature__unitarian_reading, suppression_requirement, 381, 0.25).
narrative_ontology:measurement(bdn_unitarian_su_t1553, biblical_divine_nature__unitarian_reading, suppression_requirement, 1553, 0.4).
narrative_ontology:measurement(bdn_unitarian_su_t1605, biblical_divine_nature__unitarian_reading, suppression_requirement, 1605, 0.5).
narrative_ontology:measurement(bdn_unitarian_su_t1658, biblical_divine_nature__unitarian_reading, suppression_requirement, 1658, 0.55).
narrative_ontology:measurement(bdn_unitarian_su_t1785, biblical_divine_nature__unitarian_reading, suppression_requirement, 1785, 0.3).
narrative_ontology:measurement(bdn_unitarian_su_t1825, biblical_divine_nature__unitarian_reading, suppression_requirement, 1825, 0.22).
narrative_ontology:measurement(bdn_unitarian_su_t1961, biblical_divine_nature__unitarian_reading, suppression_requirement, 1961, 0.06).
narrative_ontology:measurement(bdn_unitarian_su_t2025, biblical_divine_nature__unitarian_reading, suppression_requirement, 2025, 0.22).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(biblical_divine_nature__unitarian_reading, identity_coordination).
narrative_ontology:affects_constraint(biblical_divine_nature__unitarian_reading, biblical_divine_nature__trinitarian_reading).
narrative_ontology:affects_constraint(biblical_divine_nature__unitarian_reading, biblical_divine_nature__modalist_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'the biblical doctrine of God' decomposes into three structurally distinct arrangements — this unitarian arrangement (flat ecclesiology, low institutional authority, victims = hierarchy and credal orthodoxy), the trinitarian arrangement (credal enforcement, episcopal authority, victims = subordinationist dissenters), and the modalist arrangement (mode-conformity coordination). The ε values differ by reading per OQ-26 over the shared kernel referent. The trinitarian story is upstream: it holds institutional power over the kernel's dominant reading, and its enforcement history defines the suppression environment this arrangement has operated in. This story is downstream of that enforcement history and exerts revival pressure back on the trinitarian monopoly; both siblings are linked here and in their own files.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
