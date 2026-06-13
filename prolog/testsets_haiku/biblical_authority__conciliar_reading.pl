% ============================================================================
% CONSTRAINT STORY: biblical_authority__conciliar_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_biblical_authority__conciliar_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: biblical_authority__conciliar_reading
 *   human_readable: Conciliar Scriptural Authority via Patristic Consensus
 *   domain: theological/ecclesiastical
 *
 * SUMMARY:
 *   The conciliar-patristic reading of biblical authority holds that
 *   scripture's meaning is determined through the interpretive tradition of
 *   the Church Fathers and through ecumenical councils that represent the
 *   whole church in discerning doctrine. Tradition is not a separate deposit
 *   (as in Roman magisterium) but a living, organic continuity with apostolic
 *   teaching mediated through patristic consensus. Councils enforce this
 *   constraint by condemning departures from patristic orthodoxy and by
 *   directing doctrinal development within patristic boundaries. The reading
 *   distributes authority among bishops (avoiding papal centralization) while
 *   anchoring interpretation in the Fathers (avoiding fragmentation into
 *   private readings). This story models the constraint as a Tangled Rope: it
 *   does coordinate (solving the interpretation problem through conciliar
 *   deliberation) but also extracts (requiring deference to patristic
 *   authority, slowing doctrinal adaptation, maintaining episcopal
 *   collegiality as a benefit for some at the cost of others' interpretive
 *   freedom).
 *
 * KEY AGENTS:
 *   - ecumenical_councils: Convene to interpret scripture through patristic consensus; agenda-setters with institutional power at universal scope.
 *   - episcopal_collegiality: Structural beneficiary; gains authority from conciliar distribution rather than papal centralization.
 *   - monastic_scriptural_scholars: Payer; constrained to work within patristic-conciliar frame, cannot pursue novel exegesis independent of council approval.
 *   - rapid_doctrinal_adaptors: Payer; face suppression of innovation; must persuade a council rather than act on perceived doctrinal necessity.
 *   - individual_scriptural_interpreters: Payer; powerless and trapped; private reading permitted but doctrinal conclusions must align with patristic consensus.
 *   - heresiarchs_and_dissenters: Excluded and condemned; their readings are named and anathematized rather than debated.
 *   - autocephalous_churches: Benefit from patristic-conciliar authority but pay fragmentation costs when different traditions recognize different councils as ecumenical.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(biblical_authority__conciliar_reading, 0.48).
domain_priors:suppression_score(biblical_authority__conciliar_reading, 0.38).
domain_priors:theater_ratio(biblical_authority__conciliar_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(biblical_authority__conciliar_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(biblical_authority__conciliar_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(biblical_authority__conciliar_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(biblical_authority__conciliar_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(biblical_authority__conciliar_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(biblical_authority__conciliar_reading, tangled_rope).
narrative_ontology:human_readable(biblical_authority__conciliar_reading, "Conciliar Scriptural Authority via Patristic Consensus").
narrative_ontology:topic_domain(biblical_authority__conciliar_reading, "theological/ecclesiastical").

domain_priors:requires_active_enforcement(biblical_authority__conciliar_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(biblical_authority__conciliar_reading, '5c6b1ba6-0853-4c53-937d-d8e4f1a5af2e').
narrative_ontology:cs_kernel_codification('5c6b1ba6-0853-4c53-937d-d8e4f1a5af2e', fixed_text).
narrative_ontology:cs_authority_grounding('5c6b1ba6-0853-4c53-937d-d8e4f1a5af2e', lineage).
narrative_ontology:cs_interpretation_layer_present('5c6b1ba6-0853-4c53-937d-d8e4f1a5af2e').
narrative_ontology:cs_reading_relation('5c6b1ba6-0853-4c53-937d-d8e4f1a5af2e', biblical_authority__sola_scriptura_reading, coexists_with).
narrative_ontology:cs_reading_relation('5c6b1ba6-0853-4c53-937d-d8e4f1a5af2e', biblical_authority__tradition_scripture_reading, coexists_with).
narrative_ontology:cs_axiom('5c6b1ba6-0853-4c53-937d-d8e4f1a5af2e', foundational, living_patristic_tradition_authoritative).
narrative_ontology:cs_axiom_status(living_patristic_tradition_authoritative, holdable).
narrative_ontology:cs_axiom_grounding('5c6b1ba6-0853-4c53-937d-d8e4f1a5af2e', living_patristic_tradition_authoritative, conventional).
narrative_ontology:cs_axiom('5c6b1ba6-0853-4c53-937d-d8e4f1a5af2e', foundational, conciliar_consensus_binding).
narrative_ontology:cs_axiom_status(conciliar_consensus_binding, holdable).
narrative_ontology:cs_axiom_grounding('5c6b1ba6-0853-4c53-937d-d8e4f1a5af2e', conciliar_consensus_binding, deontological).
narrative_ontology:cs_reference_frame('5c6b1ba6-0853-4c53-937d-d8e4f1a5af2e', patristic_conciliar_consensus).
narrative_ontology:cs_drift_state('5c6b1ba6-0853-4c53-937d-d8e4f1a5af2e', contemporary_schism_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('5c6b1ba6-0853-4c53-937d-d8e4f1a5af2e', '').
narrative_ontology:cs_kernel_id(biblical_authority__conciliar_reading, biblical_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(biblical_authority__conciliar_reading, episcopal_collegiality).
narrative_ontology:constraint_beneficiary(biblical_authority__conciliar_reading, patristic_interpretive_tradition).
narrative_ontology:constraint_victim(biblical_authority__conciliar_reading, rapid_doctrinal_adaptors).
narrative_ontology:constraint_victim(biblical_authority__conciliar_reading, individual_scriptural_interpreters).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(biblical_authority__conciliar_reading, autocephalous_churches).
narrative_ontology:constraint_victim(biblical_authority__conciliar_reading, bishops_local_sees).
narrative_ontology:constraint_victim(biblical_authority__conciliar_reading, monastic_scriptural_scholars).
narrative_ontology:constraint_victim(biblical_authority__conciliar_reading, autocephalous_churches).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Convene to interpret scripture through collective deliberation grounded in patristic precedent. Claim authority from their representativeness of the whole church and from the Holy Spirit's guidance of consensus. Set the boundary between licit and heretical doctrine; their canons define what tradition legitimately contains. Enforcement depends on bishops enforcing conciliar decisions at the local level.
narrative_ontology:constraint_stakeholder(biblical_authority__conciliar_reading, ecumenical_councils, agenda_setter,
    institutional, generational, constrained, universal).

% Gains structural authority from conciliar processes that distribute power among bishops rather than concentrating it in a single magisterium. Each bishop participates in interpretation and enforcement at his see; conciliar membership gives local authority a universal voice. The constraint validates episcopal autonomy within an overall conciliar structure.
narrative_ontology:constraint_stakeholder(biblical_authority__conciliar_reading, episcopal_collegiality, beneficiary,
    institutional, generational, constrained, universal).

% A body of doctrine and hermeneutic precedent (not an actor) that gains vindication through conciliar appeal to patristic consensus. Each council that grounds its decision in Father-appeals reinforces the tradition's authority. The constraint operationalizes continuity with patristic sources.
narrative_ontology:constraint_stakeholder(biblical_authority__conciliar_reading, patristic_interpretive_tradition, beneficiary,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(biblical_authority__conciliar_reading, patristic_interpretive_tradition).

% Enforce conciliar decisions within their dioceses and interpret scripture within the conciliar-patristic frame. Gain authority from conciliar participation but bear the cost of enforcing canons against local resistance and of deferring novel interpretations to the council process rather than acting independently. Their autonomy is bounded by conciliar consensus.
narrative_ontology:constraint_stakeholder(biblical_authority__conciliar_reading, bishops_local_sees, agenda_setter,
    powerful, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(biblical_authority__conciliar_reading, bishops_local_sees, payer).

% Study scripture and produce exegetical work within the patristic-conciliar frame. Their scholarly agency is constrained: departing from patristic consensus risks condemnation as heretical; advancing new interpretations requires conciliar approval rather than scholarly validation alone. Career advancement depends on demonstrating patristic fidelity, not interpretive novelty.
narrative_ontology:constraint_stakeholder(biblical_authority__conciliar_reading, monastic_scriptural_scholars, payer,
    moderate, biographical, identity_locked, regional).

% Theologians and bishops who perceive the need for swift doctrinal development or reinterpretation in response to novel challenges or changed circumstances. The conciliar-patristic constraint prevents them from adapting doctrine rapidly; they must either persuade a council (slow and uncertain) or face condemnation as heretical. Exit (joining a non-conciliar tradition) means losing ecclesiastical standing and community.
narrative_ontology:constraint_stakeholder(biblical_authority__conciliar_reading, rapid_doctrinal_adaptors, payer,
    moderate, biographical, trapped, regional).

% Laypersons and lower clergy who wish to interpret scripture for themselves or their community. The constraint's requirement of conciliar-patristic consensus narrows the space for non-credentialed interpretation; private scriptural reading is permitted but doctrinal conclusions that depart from patristic consensus face correction. The constraint normalizes deference to learned interpreters.
narrative_ontology:constraint_stakeholder(biblical_authority__conciliar_reading, individual_scriptural_interpreters, payer,
    powerless, biographical, trapped, local).

% Theologians and sect leaders whose scriptural readings depart substantially from patristic consensus. Are not part of the conciliar conversation; are condemned through canons and anathemata rather than addressed through deliberation. Their exclusion is structural to the constraint's operation: the conciliar-patristic frame works only if dissenting readings are named, corrected, and de-legitimated.
narrative_ontology:constraint_stakeholder(biblical_authority__conciliar_reading, heresiarchs_and_dissenters, excluded,
    moderate, biographical, trapped, regional).

% Gain authority from participation in ecumenical councils and from claiming patristic continuity within their own traditions. Each autocephalous church (Constantinople, Alexandria, Antioch, Jerusalem, later Moscow, etc.) enforces conciliar canons and interprets scripture within the common frame. But they also bear fragmentation costs: different councils may carry different authority in different traditions; disagreement over which councils are ecumenical (e.g., the Filioque disputes) breaks the constraint's unity.
narrative_ontology:constraint_stakeholder(biblical_authority__conciliar_reading, autocephalous_churches, beneficiary,
    institutional, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(biblical_authority__conciliar_reading, autocephalous_churches, payer).

% Examines the constraint's operation across Christian traditions and across time: what councils are recognized, how patristic consensus is invoked, where it breaks down, and how different churches navigate the conciliar-patristic frame.
narrative_ontology:constraint_stakeholder(biblical_authority__conciliar_reading, analytical_observer, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(biblical_authority__conciliar_reading, episcopal_collegiality).
narrative_ontology:fixing_cost_class(biblical_authority__conciliar_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the scriptural interpretation problem by distributing deliberation across a college of bishops (ecumenical councils) grounded in a shared interpretive tradition (the Fathers). This avoids both the fragmentation of purely private interpretation and the centralization of a single magisterial voice. The constraint provides a framework for detecting and correcting heresy (departures from patristic consensus) through collective discernment rather than individual judgment.
% TRANSFER_FUNCTION: Moves interpretive authority from individual scholars and bishops to conciliar bodies; moves scriptural meaning from the text's surface to its patristic reading; moves doctrinal adaptation authority from rapid innovators to councils requiring consensus and patristic grounding. The bishops and monastic scholars pay a cost in autonomy and interpretive speed; the councils and the patristic tradition collect authority.
% ABSENT_VOICES: Sola scriptura reformers would argue for the text's sufficiency and against patristic mediation; they are excluded by the constraint's operating premise. Rapid doctrinal adaptors and individual interpreters are partially excluded — they may speak but must defer to conciliar consensus. Heresiarchs are systematically excluded through the anathematization process.
% DISAPPEARANCE_RATIONALE: If the conciliar-patristic constraint vanished, scripture interpretation would fragment into competing schools without a mechanism for detecting and correcting error; bishops would either centralize authority (moving toward a magisterium like Rome's) or localize it (toward congregationalism); the patristic interpretive tradition would lose institutional force and become one resource among many rather than the normative frame.
% FOUNDING_PROBLEM: Early Christianity faced rapid doctrinal variation and heretical departures from apostolic teaching. Patristic writers developed consensus on core doctrines (the Trinity, the two natures of Christ, the sacraments). Ecumenical councils were convened to settle disputes by representing the whole church and appealing to patristic authority. The problem was: how to preserve apostolic teaching against innovation while avoiding rigidity and respecting the Holy Spirit's ongoing guidance of the church.
% FOUNDING_PROBLEM_CORROBORATION: Conciliar tradition attests the problem remains live — heresy and doctrinal drift persist, and councils are needed to correct them. Reformation critics attest the founding problem is partly solved (the major heresies are settled) and the conciliar constraint now persists as a mechanism for protecting episcopal power and patristic institutional investment rather than detecting genuine error. Historical scholarship notes that conciliar authority itself has become contested: different churches recognize different councils as ecumenical, and appeals to patristic consensus often mask contemporary disputes about authority. The Filioque controversy and subsequent schisms demonstrate that 'patristic consensus' is itself subject to reinterpretation.
narrative_ontology:disappearance_verdict(biblical_authority__conciliar_reading, world_rearranges).
narrative_ontology:founding_problem_status(biblical_authority__conciliar_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(biblical_authority__conciliar_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(biblical_authority__conciliar_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(biblical_authority__conciliar_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(biblical_authority__conciliar_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(biblical_authority__conciliar_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.48 at interval end) because the constraint serves a genuine coordination function (interpreting scripture collectively rather than individually) but also enforces a specific interpretive regime that benefits episcopal collegiality and the patristic tradition. The measurement series shows a slight rise from 0.35 to 0.50 across the interval, then stabilization — this models the constraint's function: it was essential in the early councils (settling major heresies) but over time accumulated secondary uses (defending episcopal turf, protecting patristic institutional investment) without major structural change. Suppression is lower (0.38 at end) because the constraint relies more on persuasion and consensus than on coercive force — heresy is condemned but enforcement is primarily social (losing status, facing anathema) rather than state power. Theater rises slightly from 0.15 to 0.22, suggesting that over time councils increasingly perform the confirmation of existing consensus rather than genuinely deliberate novel problems. Accessibility collapse is moderate (0.62): once one accepts the patristic-conciliar frame, alternatives (private scripture reading, rapid innovation, papal centralization) become closed off; but the frame itself remains contestable (witness the Reformation, the East-West Schism). Resistance is substantial (0.58) because the constraint faces constant pressure from reformers, adaptationists, and those seeking faster doctrinal change.
 *
 * PERSPECTIVAL GAP:
 *   The beneficiary seat (episcopal collegiality) should compute the constraint as rope or near-rope: genuine coordination with manageable costs. The payer seats (rapid adaptors, individual interpreters, monastic scholars) should compute it as snare or tangled_rope: extraction dressed as coordination, with suppression they did not consent to. The analytical observer seat should compute a type reflecting the gap: if the coordination function is strong, tangled_rope; if the coordination has atrophied and only extraction remains, piton or snare. This divergence maps to the core tension in patristic-conciliar theology: is it a living tradition of authentic development, or a frozen monument to past decisions?
 *
 * DIRECTIONALITY LOGIC:
 *   Episcopal collegiality benefits from the constraint structurally: councils distribute power among bishops, validate local sees as interpreters, and give bishops a voice in ecumenical decision-making that a papal system would reserve to Rome. Their directionality is beneficiary-side (~0.2). Rapid doctrinal adaptors and individual interpreters are targets: they cannot pursue doctrinal paths independently; they must defer to councils and patristic authority. Their directionality is target-side (~0.8). Monastic scholars sit between: they gain standing as interpreters and scholars (beneficiary element) but are constrained in research autonomy (payer element); directionality near ~0.55. The patristic tradition itself is a vindicated proposition, not an actor — it is the instrument through which the constraint claims legitimacy. Heresiarchs have maximal target directionality (~1.0) because the constraint's primary function is to exclude and condemn them.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding_problem (rapid doctrinal variation and heretical departure from apostolic teaching) is substantively solved for major heresies by the early councils (Nicaea on the Trinity, Chalcedon on Christology). Modern councils (post-Schism) often ratify existing consensus rather than settle novel disputes. The constraint persists partly because the coordination problem remains (church still needs a frame for detecting heresy) but partly because the institutional structure (conciliar authority, episcopal collegiality, patristic vindication) has become valued as an end in itself. The mandatrophy is partial: the constraint shows signs of zombie institutional inertia (theater_ratio is modest but non-zero, and rises slightly over the interval) alongside continued functional value. The fragmentation of conciliar authority itself (different churches recognizing different councils, disputes over the Filioque) suggests the constraint's coordination function is degraded — it prevents neither schism nor doctrinal divergence among parties claiming conciliar fidelity. This maps to the piton-side risk: the constraint may be becoming primarily a mechanism for episcopal self-reproduction rather than a live answer to the founding problem.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    patristic_consensus_construction,
    'Is patristic consensus a discovered fact about the Church Fathers'' actual agreement, or is it a constructed reading that selectively privileges some patristic voices over others?',
    'Systematic historical analysis of patristic sources: survey the full range of patristic opinion on contested doctrines (e.g., the filioque, the nature of images) and measure actual consensus vs. the consensus claimed by councils. Compare councils'' patristic citations to the full patristic corpus.',
    'If consensus is constructed (selective citation, retrospective harmonization), the conciliar claim to ground authority in discovered consensus becomes a false legitimacy — the extraction is less a cost of coordination than a cost of maintaining a fabricated authority frame. If consensus is largely genuine, the extraction reflects a real coordination value.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(patristic_consensus_construction, empirical, 'Whether patristic consensus is discovered or constructed through selective reading.').

omega_variable(
    living_tradition_vs_fixed_deposit,
    'Does the conciliar-patristic reading permit genuine living development of doctrine, or does the requirement of grounding in patristic precedent freeze doctrine and prevent necessary adaptation?',
    'Compare the rate of doctrinal innovation in conciliar vs. non-conciliar traditions (sola scriptura, magisterial) over the same historical periods. Track whether major doctrinal shifts (e.g., on slavery, women''s roles, religious freedom) moved faster or slower under conciliar authority and whether such shifts faced greater resistance.',
    'If the conciliar frame permits adaptation, the constraint is genuinely a coordination mechanism; if it suppresses necessary change, the extraction becomes a cost of institutional rigidity. This maps to the victim class of ''rapid_doctrinal_adaptors.''',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(living_tradition_vs_fixed_deposit, empirical, 'Whether conciliar authority permits or suppresses necessary doctrinal development.').

omega_variable(
    conciliar_unity_fragmentation,
    'Does the conciliar-patristic frame actually produce unity and prevent heresy, or does it fragment into competing claims about which councils are ecumenical and what patristic consensus means?',
    'Historical record of schisms and divergences among churches claiming conciliar authority: the East-West Schism, Oriental Orthodoxy''s rejection of Chalcedon, disputes over the Filioque, modern disagreements about which councils are binding. Measure whether conciliar appeal resolves disputes or merely produces competing conciliar claims.',
    'If conciliar authority fragments, the constraint''s extraction does not secure the coordination benefit promised; it becomes tangled extraction without sufficient coordination function. This would lower the claimed type toward snare.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(conciliar_unity_fragmentation, empirical, 'Whether conciliar authority actually prevents fragmentation or merely masks it.').

omega_variable(
    kernel_reading_contestation,
    'This constraint is one reading of the biblical_authority kernel. How should the conciliar-patristic reading relate structurally to the sola_scriptura reading and the tradition_scripture (magisterial) reading? Are these three readings coexisting positions, or does conciliar reading foreclose or influence the others?',
    'Theological and historical analysis: Can a party hold the conciliar reading while accepting elements of sola scriptura (e.g., scriptural sufficiency for core doctrine)? Can conciliar and magisterial readings coexist (as in modern Catholic-Orthodox dialogue)? Or do the readings logically foreclose one another?',
    'This maps directly to the cs_structure.reading_relations field and determines whether the conciliar reading is in genuine contestation or in stable coexistence with siblings. Affects how the kernel''s authority structure is modeled.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Structural relationship between conciliar, sola scriptura, and magisterial readings of biblical authority.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(biblical_authority__conciliar_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bibl_tr_t0, biblical_authority__conciliar_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement_basis(bibl_tr_t0, observed).
narrative_ontology:measurement(bibl_tr_t5, biblical_authority__conciliar_reading, theater_ratio, 5, 0.17).
narrative_ontology:measurement_basis(bibl_tr_t5, observed).
narrative_ontology:measurement(bibl_tr_t10, biblical_authority__conciliar_reading, theater_ratio, 10, 0.18).
narrative_ontology:measurement_basis(bibl_tr_t10, observed).
narrative_ontology:measurement(bibl_tr_t15, biblical_authority__conciliar_reading, theater_ratio, 15, 0.2).
narrative_ontology:measurement_basis(bibl_tr_t15, observed).
narrative_ontology:measurement(bibl_tr_t20, biblical_authority__conciliar_reading, theater_ratio, 20, 0.21).
narrative_ontology:measurement_basis(bibl_tr_t20, observed).
narrative_ontology:measurement(bibl_tr_t25, biblical_authority__conciliar_reading, theater_ratio, 25, 0.22).
narrative_ontology:measurement_basis(bibl_tr_t25, observed).
narrative_ontology:measurement(bibl_tr_t30, biblical_authority__conciliar_reading, theater_ratio, 30, 0.23).
narrative_ontology:measurement_basis(bibl_tr_t30, observed).
narrative_ontology:measurement(bibl_tr_t35, biblical_authority__conciliar_reading, theater_ratio, 35, 0.22).
narrative_ontology:measurement_basis(bibl_tr_t35, observed).
narrative_ontology:measurement(bibl_tr_t40, biblical_authority__conciliar_reading, theater_ratio, 40, 0.22).
narrative_ontology:measurement_basis(bibl_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(bibl_be_t0, biblical_authority__conciliar_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement_basis(bibl_be_t0, observed).
narrative_ontology:measurement(bibl_be_t5, biblical_authority__conciliar_reading, base_extractiveness, 5, 0.38).
narrative_ontology:measurement_basis(bibl_be_t5, observed).
narrative_ontology:measurement(bibl_be_t10, biblical_authority__conciliar_reading, base_extractiveness, 10, 0.42).
narrative_ontology:measurement_basis(bibl_be_t10, observed).
narrative_ontology:measurement(bibl_be_t15, biblical_authority__conciliar_reading, base_extractiveness, 15, 0.45).
narrative_ontology:measurement_basis(bibl_be_t15, observed).
narrative_ontology:measurement(bibl_be_t20, biblical_authority__conciliar_reading, base_extractiveness, 20, 0.48).
narrative_ontology:measurement_basis(bibl_be_t20, observed).
narrative_ontology:measurement(bibl_be_t25, biblical_authority__conciliar_reading, base_extractiveness, 25, 0.5).
narrative_ontology:measurement_basis(bibl_be_t25, observed).
narrative_ontology:measurement(bibl_be_t30, biblical_authority__conciliar_reading, base_extractiveness, 30, 0.51).
narrative_ontology:measurement_basis(bibl_be_t30, observed).
narrative_ontology:measurement(bibl_be_t35, biblical_authority__conciliar_reading, base_extractiveness, 35, 0.48).
narrative_ontology:measurement_basis(bibl_be_t35, observed).
narrative_ontology:measurement(bibl_be_t40, biblical_authority__conciliar_reading, base_extractiveness, 40, 0.48).
narrative_ontology:measurement_basis(bibl_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(bibl_su_t0, biblical_authority__conciliar_reading, suppression_requirement, 0, 0.28).
narrative_ontology:measurement_basis(bibl_su_t0, observed).
narrative_ontology:measurement(bibl_su_t5, biblical_authority__conciliar_reading, suppression_requirement, 5, 0.3).
narrative_ontology:measurement_basis(bibl_su_t5, observed).
narrative_ontology:measurement(bibl_su_t10, biblical_authority__conciliar_reading, suppression_requirement, 10, 0.33).
narrative_ontology:measurement_basis(bibl_su_t10, observed).
narrative_ontology:measurement(bibl_su_t15, biblical_authority__conciliar_reading, suppression_requirement, 15, 0.35).
narrative_ontology:measurement_basis(bibl_su_t15, observed).
narrative_ontology:measurement(bibl_su_t20, biblical_authority__conciliar_reading, suppression_requirement, 20, 0.37).
narrative_ontology:measurement_basis(bibl_su_t20, observed).
narrative_ontology:measurement(bibl_su_t25, biblical_authority__conciliar_reading, suppression_requirement, 25, 0.38).
narrative_ontology:measurement_basis(bibl_su_t25, observed).
narrative_ontology:measurement(bibl_su_t30, biblical_authority__conciliar_reading, suppression_requirement, 30, 0.39).
narrative_ontology:measurement_basis(bibl_su_t30, observed).
narrative_ontology:measurement(bibl_su_t35, biblical_authority__conciliar_reading, suppression_requirement, 35, 0.38).
narrative_ontology:measurement_basis(bibl_su_t35, observed).
narrative_ontology:measurement(bibl_su_t40, biblical_authority__conciliar_reading, suppression_requirement, 40, 0.38).
narrative_ontology:measurement_basis(bibl_su_t40, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(biblical_authority__conciliar_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(biblical_authority__conciliar_reading, 0.12).
narrative_ontology:affects_constraint(biblical_authority__conciliar_reading, biblical_authority__sola_scriptura_reading).
narrative_ontology:affects_constraint(biblical_authority__conciliar_reading, biblical_authority__tradition_scripture_reading).
narrative_ontology:affects_constraint(biblical_authority__conciliar_reading, schism_east_west_kernel).
narrative_ontology:affects_constraint(biblical_authority__conciliar_reading, filioque_dispute__conciliar_reading).

% DUAL FORMULATION NOTE:
% The biblical_authority kernel contains three structurally distinct constraints corresponding to three live readings: conciliar (this story), sola scriptura, and magisterial. Each reading has distinct beneficiaries, victims, and ε values. The conciliar reading specifically benefits episcopal collegiality and the patristic tradition at the cost of rapid adaptors and individual interpreters. The network links show how conciliar authority influences but does not foreclose the sibling readings, and how the Filioque dispute and East-West Schism are downstream consequences of applying the conciliar reading to novel doctrinal questions.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(biblical_authority__conciliar_reading, institutional, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
