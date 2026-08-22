% ============================================================================
% CONSTRAINT STORY: biblical_authority__conciliar_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
 *   constraint_id: biblical_authority__conciliar_reading
 *   human_readable: Conciliar Authority: Scripture Through Ecumenical Consensus
 *   domain: theological/historical/ecclesiastical
 *
 * SUMMARY:
 *   The conciliar reading of biblical authority treats Scripture as the
 *   foundation of Christian truth but insists Scripture requires
 *   interpretation through the living tradition of the ecumenical councils
 *   and Church Fathers. Tradition is not a separate deposit (as in Tridentine
 *   Catholicism) but the organic continuation of apostolic teaching, mediated
 *   through collective episcopal pronouncement and patristic consensus. This
 *   reading was dominant in the early church and remains authoritative in
 *   Eastern Orthodox, Oriental Orthodox, and some Anglican traditions. It
 *   presents itself as stability and unity; critics (sola scriptura
 *   Protestants, contemporary scholars, lay theologians) see it as episcopal
 *   gatekeeping. The constraint is CLAIMED as tangled_rope (genuine doctrinal
 *   coordination + legitimate episcopal authority) while the authored metrics
 *   describe substantial extraction (bishops control doctrine, lay innovation
 *   is suppressed, patristic consensus can ossify) — the engine measures this
 *   divergence.
 *
 * KEY AGENTS:
 *   - Ecumenical councils (Nicaea I, Constantinople, Chalcedon, etc.) — set and enforce the interpretive framework
 *   - Patristic tradition (Fathers' writings, treated as living guidance) — legitimizes episcopal interpretation
 *   - Episcopal hierarchy (bishops collectively and individually) — benefits from interpretive authority, enforces doctrine
 *   - Autocephalous churches (Oriental Orthodox, Coptic, Ethiopian) — benefit from distributed rather than centralized authority
 *   - Theological innovators (individual theologians, local bishops) — constrained by need for consensus, identity-locked
 *   - Lay believers — powerless to interpret Scripture independently, trapped within clerical gatekeeping
 *   - Sola scriptura advocates (excluded) — would overturn the constraint by denying conciliar authority
 *   - Contemporary scholarship (excluded) — would reinterpret Scripture based on new historical evidence
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(biblical_authority__conciliar_reading, 0.58).
domain_priors:suppression_score(biblical_authority__conciliar_reading, 0.42).
domain_priors:theater_ratio(biblical_authority__conciliar_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(biblical_authority__conciliar_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(biblical_authority__conciliar_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(biblical_authority__conciliar_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(biblical_authority__conciliar_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(biblical_authority__conciliar_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(biblical_authority__conciliar_reading, tangled_rope).
narrative_ontology:human_readable(biblical_authority__conciliar_reading, "Conciliar Authority: Scripture Through Ecumenical Consensus").
narrative_ontology:topic_domain(biblical_authority__conciliar_reading, "theological/historical/ecclesiastical").

domain_priors:requires_active_enforcement(biblical_authority__conciliar_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(biblical_authority__conciliar_reading, 'e2592263-70e9-4df5-b2e8-62ba36946bc1').
narrative_ontology:cs_kernel_codification('e2592263-70e9-4df5-b2e8-62ba36946bc1', fixed_text).
narrative_ontology:cs_authority_grounding('e2592263-70e9-4df5-b2e8-62ba36946bc1', lineage).
narrative_ontology:cs_interpretation_layer_present('e2592263-70e9-4df5-b2e8-62ba36946bc1').
narrative_ontology:cs_reading_relation('e2592263-70e9-4df5-b2e8-62ba36946bc1', biblical_authority__sola_scriptura_reading, coexists_with).
narrative_ontology:cs_reading_relation('e2592263-70e9-4df5-b2e8-62ba36946bc1', biblical_authority__tradition_scripture_reading, influences).
narrative_ontology:cs_axiom('e2592263-70e9-4df5-b2e8-62ba36946bc1', foundational, apostolic_collegiality_primacy).
narrative_ontology:cs_axiom_status(apostolic_collegiality_primacy, holdable).
narrative_ontology:cs_axiom_grounding('e2592263-70e9-4df5-b2e8-62ba36946bc1', apostolic_collegiality_primacy, deontological).
narrative_ontology:cs_axiom('e2592263-70e9-4df5-b2e8-62ba36946bc1', foundational, patristic_consensus_authority).
narrative_ontology:cs_axiom_status(patristic_consensus_authority, holdable).
narrative_ontology:cs_axiom_grounding('e2592263-70e9-4df5-b2e8-62ba36946bc1', patristic_consensus_authority, conventional).
narrative_ontology:cs_reference_frame('e2592263-70e9-4df5-b2e8-62ba36946bc1', apostolic_conciliar_consensus).
narrative_ontology:cs_drift_state('e2592263-70e9-4df5-b2e8-62ba36946bc1', contemporary_pluralist_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('e2592263-70e9-4df5-b2e8-62ba36946bc1', '2026-06-11T14:32:00Z').
narrative_ontology:cs_kernel_id(biblical_authority__conciliar_reading, biblical_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(biblical_authority__conciliar_reading, episcopal_collegiality).
narrative_ontology:constraint_beneficiary(biblical_authority__conciliar_reading, autocephalous_church_autonomy).
narrative_ontology:constraint_victim(biblical_authority__conciliar_reading, rapid_doctrinal_innovation).
narrative_ontology:constraint_victim(biblical_authority__conciliar_reading, individual_scriptural_interpretation).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(biblical_authority__conciliar_reading, patristic_tradition).
narrative_ontology:constraint_beneficiary(biblical_authority__conciliar_reading, episcopal_hierarchy).
narrative_ontology:constraint_beneficiary(biblical_authority__conciliar_reading, autocephalous_churches).
narrative_ontology:constraint_victim(biblical_authority__conciliar_reading, theological_innovation).
narrative_ontology:constraint_victim(biblical_authority__conciliar_reading, lay_biblical_interpretation).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Ecumenical councils (Nicaea I, Constantinople, Chalcedon, etc.) gathered bishops to pronounce on scriptural interpretation and defend orthodox doctrine against heresies. Each council claims authority to settle interpretation for the entire church. The councils' pronouncements bind subsequent doctrine; deviation requires new conciliar action or schism.
narrative_ontology:constraint_stakeholder(biblical_authority__conciliar_reading, ecumenical_councils, agenda_setter,
    institutional, civilizational, constrained, universal).

% The writings of the Church Fathers (Athanasius, Basil, Gregory, Chrysostom, Augustine) codify scriptural interpretation and are treated as living guides to reading Scripture itself. Patristic consensus is read as the voice of the ancient church; it carries authority without formal magisterial decree. Individual theologians and bishops appeal to patristic precedent to validate interpretations.
narrative_ontology:constraint_stakeholder(biblical_authority__conciliar_reading, patristic_tradition, agenda_setter,
    institutional, civilizational, constrained, universal).
narrative_ontology:stakeholder_secondary_role(biblical_authority__conciliar_reading, patristic_tradition, beneficiary).

% Bishops, as successors of the apostles in the conciliar reading, are the authorized interpreters of Scripture in their dioceses and through the ecumenical councils. The reading confers on them corporate interpretive authority that lay believers and lower clergy cannot override. Bishops coordinate doctrine and defend orthodoxy collectively; the conciliar system reinforces episcopal collegiality.
narrative_ontology:constraint_stakeholder(biblical_authority__conciliar_reading, episcopal_hierarchy, beneficiary,
    institutional, generational, constrained, national).

% Autocephalous churches (Oriental Orthodox, Coptic, Ethiopian churches) maintain independent hierarchies while accepting conciliar authority as binding for doctrine. The conciliar reading preserves their autonomy: Scripture is interpreted through the councils that their bishops participated in or accepted, not through a centralized magisterium. They benefit from a distributed, collectively-binding authority structure.
narrative_ontology:constraint_stakeholder(biblical_authority__conciliar_reading, autocephalous_churches, beneficiary,
    institutional, generational, constrained, national).

% Individual theologians and local church leaders who discover new readings of Scripture or doctrinal implications must wait for or petition ecumenical council consensus to have their innovations accepted as orthodox. Rapid doctrinal innovation is constrained by the requirement to achieve consensus among dispersed bishops; heterodox innovations face suppression. The constraint locks identity: a theologian advancing doctrine outside the conciliar process risks heresy charges and exclusion from the church's magisterium.
narrative_ontology:constraint_stakeholder(biblical_authority__conciliar_reading, theological_innovation, payer,
    moderate, biographical, identity_locked, regional).

% Lay believers who read Scripture directly are taught to interpret it through the lens of conciliar doctrine and patristic commentary, not independently. The constraint requires lay readers to accept episcopal-guided interpretation and submit questionable readings to hierarchical authority. Lay exegesis that diverges from conciliar consensus is discouraged or condemned; lay believers have no formal input to conciliar decision-making.
narrative_ontology:constraint_stakeholder(biblical_authority__conciliar_reading, lay_biblical_interpretation, payer,
    powerless, biographical, trapped, local).

% The Church Fathers themselves are dead but their voices are mediated through tradition, commentary, and selective citation. A Father's minority opinion or developmental teaching is sometimes excluded from the consensus narrative to preserve unanimity. Their exclusion is from living interpretation, not historical records—they cannot challenge how their words are read.
narrative_ontology:constraint_stakeholder(biblical_authority__conciliar_reading, individual_church_fathers, excluded,
    powerful, civilizational, analytical, universal).

% Reformation and post-Reformation theologians who argue Scripture alone suffices as interpretive authority are explicitly rejected by this reading's framework. Their exclusion is structural: the conciliar reading treats Scripture-alone as insufficient and dangerous (prone to proliferation of heresies). Sola scriptura advocates would argue for lay access to Scripture and rapid doctrinal evolution; their participation would overturn the constraint itself.
narrative_ontology:constraint_stakeholder(biblical_authority__conciliar_reading, sola_scriptura_advocates, excluded,
    organized, biographical, identity_locked, global).

% The Roman Catholic magisterium (papal and conciliar authority unified under papal primacy) operates a centralized pronouncement system that the conciliar reading rejects as non-apostolic. Catholic theology is excluded from the conciliar reading's legitimacy frame; the conciliar reading treats papal decrees without ecumenical council consent as overreach. The two readings remain in structural tension.
narrative_ontology:constraint_stakeholder(biblical_authority__conciliar_reading, magisterial_catholicism, excluded,
    institutional, generational, identity_locked, global).

% Modern historical-critical and literary approaches to Scripture (source criticism, redaction criticism, archaeological findings) are systematically bracketed or reinterpreted to fit patristic categories. Contemporary scholarship would argue for updated doctrine based on new evidence; the conciliar reading's reliance on patristic consensus structures slow adoption of new biblical-historical knowledge. Scholars' voices are excluded from formal ecumenical processes.
narrative_ontology:constraint_stakeholder(biblical_authority__conciliar_reading, contemporary_biblical_scholarship, excluded,
    organized, biographical, constrained, global).

% The principle that doctrine changes only through ecumenical consensus is vindicated by the conciliar reading. Doctrinal stability is treated as a good—the constraint exists partly to prevent the church from fragmenting into competing theologies. This is not an actor but a vindicated outcome.
narrative_ontology:constraint_stakeholder(biblical_authority__conciliar_reading, doctrinal_stability, beneficiary,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(biblical_authority__conciliar_reading, doctrinal_stability).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(biblical_authority__conciliar_reading, episcopal_hierarchy).
narrative_ontology:fixing_cost_class(biblical_authority__conciliar_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates Christian doctrinal interpretation across geographically dispersed, culturally distinct churches by establishing a shared framework: Scripture is read through the consensus of ecumenical councils and the Church Fathers. This solves the collective-action problem of preventing theological fragmentation while avoiding a centralized magisterial authority structure (distinct from Catholic papal primacy).
% TRANSFER_FUNCTION: Transfers interpretive authority from individual Scripture readers (lay believers, innovative theologians) to the collective episcopal hierarchy, mediated through conciliar pronouncements and patristic precedent. The constraint moves the power to bind doctrine upward to bishops and councils, away from rapid local adaptation or lay exegesis.
% ABSENT_VOICES: Sola scriptura Protestants and contemporary biblical scholars are structurally excluded—their readings would overturn the constraint by arguing Scripture requires no conciliar filter or that modern historical evidence should reshape doctrine. Lay believers, while members of the church, have no vote in councils and their independent interpretations are subordinated. Individual Church Fathers whose opinions diverged from consensus are marginalized in the tradition's narrative.
% DISAPPEARANCE_RATIONALE: If conciliar authority and patristic consensus were abandoned, Christian doctrine would rapidly fragment into competing local theologies. Some denominations would adopt sola scriptura; others would innovate doctrine based on contemporary scholarship or local needs. The shared interpretive framework would collapse within a generation. Episcopal authority would weaken unless replaced by papal centralism or congregational voting.
% FOUNDING_PROBLEM: In the 4th–5th centuries, Christianity was fragmented by competing heresies (Arianism, Nestorianism, Monophysitism, Pelagianism, etc.), each claiming scriptural warrant. Individual bishops and theologians produced contradictory doctrines, threatening the unity and coherence of Christian faith. The church needed a mechanism to settle disputes authoritatively while preserving apostolic tradition and avoiding a single tyrannical authority.
% FOUNDING_PROBLEM_CORROBORATION: Modern Eastern Orthodox and Oriental Orthodox theologians attest the founding problem is still live: rapid doctrinal innovation threatens unity. Western Protestant and contemporary academic historians attest the founding problem has been substantially transformed by modern historical knowledge and cultural pluralism, rendering conciliar consensus increasingly slow and difficult to achieve. Competition authorities (in the metaphorical sense of independent scholars outside the church hierarchy) note that the conciliar reading has evolved from solving heresy to constraining doctrinal development itself.
narrative_ontology:disappearance_verdict(biblical_authority__conciliar_reading, world_rearranges).
narrative_ontology:founding_problem_status(biblical_authority__conciliar_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(biblical_authority__conciliar_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(biblical_authority__conciliar_reading, 'none', 1).
narrative_ontology:epsilon_provenance(biblical_authority__conciliar_reading, 0.58, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness rises from 0.42 (founding era, genuine coordination against heresies) to 0.61 (High Middle Ages, episcopal power consolidated) then moderates to 0.58 (contemporary, conciliar authority weakened by schism and scholarship). Theater ratio increases monotonically (0.12 to 0.38) as the functional coordination problem (preventing heresy) was solved and the constraint persists increasingly through ritual (patristic citation, conciliar form without binding force). Suppression declines from 0.55 (forceful heresy-suppression in 4th–5th centuries) to 0.42 (modern era, suppression of innovation remains but coercion is weaker—exit now possible via Protestantism, Orthodox schism, or academic theology). The measurement grid shares all time points across all metrics (one shared grid, no misalignment). Metrics are authored on a long interval (325–2026) to capture the constraint's evolution from functional coordination mechanism to ossified tradition-maintenance.
 *
 * PERSPECTIVAL GAP:
 *   Episcopal beneficiaries experience the constraint as legitimate authority protecting apostolic truth; lay believers and innovators experience it as gatekeeping limiting their voice. Councils present themselves as transparent to the Holy Spirit's guidance; modern historians see councils as politically contingent (influenced by imperial power, personality disputes, regional factions). Patristic consensus is treated by the reading as ancient unanimity; scholars note the Fathers disagreed on many points and later tradition selectively omits or reinterprets minority opinions. The engine computes different d values for bishops (beneficiary, low d) and theologians (target, high d) from the structural data—the perspectival gap is encoded in the measured directionality.
 *
 * DIRECTIONALITY LOGIC:
 *   Bishops are beneficiaries with institutional power and constrained exit (exit means schism or heresy charges), so d leans toward beneficiary (≈0.2–0.3). Theological innovators are targets with moderate power and identity-locked exit (leaving the church means abandoning career and community identity), so d leans toward target (≈0.65–0.75). Lay believers are trapped powerless agents with no formal input, so d is near full target (≈0.85). Patristic tradition is not an agent (it is a vindicated proposition and set of texts), so no d is assigned—it is treated in the commentary as the mechanism that legitimizes episcopal authority. Autocephalous churches are institutional beneficiaries with some exit (they can break communion if pushed, as they did historically), so d is moderate-beneficiary (≈0.35–0.45). The overrides for bishops and councils could be debated—some readings treat episcopal collegiality as genuine coordination, which would lower d slightly; the authored d values reflect the assumption that extraction (control of doctrine, gatekeeping of innovation) dominates the coordination benefit.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (heresy fragmentation) was live in the 4th–5th centuries and the constraint genuinely solved it. By the 12th–15th centuries, the founding problem had shifted: heresies were fewer and the constraint's function evolved toward maintaining stability against rapid doctrinal innovation. By the modern era (18th–21st centuries), the founding problem is contested: Eastern Orthodox theologians attest unity is still at risk; Western scholars and Protestants attest the founding problem is obsolete because historical knowledge has made patristic consensus implausible as a binding source. The constraint persists despite the contested status of its founding problem, which suggests the extraction (episcopal authority, gatekeeping of doctrine) is now the dominant function—mandatrophy is building. The theater_ratio rising while suppression_requirement declines is a signal: the constraint is maintained increasingly by ritual (conciliar forms, patristic citation) rather than by active enforcement or living coordination. A mandatrophy reading would say the constraint has outlived its mandate; the conciliar reading would say unity still requires shared doctrine and councils remain the mechanism for achieving it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    patristic_consensus_construction,
    'Is patristic consensus a genuine historical unanimity among the Fathers, or a later constructed narrative that selectively omits dissenting minority opinions?',
    'Systematic historical analysis of patristic texts and their later interpretations; comparison of how councils cited Fathers vs. what Fathers actually wrote; study of which patristic opinions were excluded from the ''consensus'' narrative.',
    'If consensus is constructed, the constraint''s legitimacy rests on a fiction—it is a snare disguised as rope. If consensus is genuine, the constraint represents authentic apostolic tradition and remains tangled_rope. The ε value would remain stable but the type classification could shift depending on the reader''s historical judgment.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(patristic_consensus_construction, empirical, 'Whether patristic consensus is historical fact or retrospective narrative construction.').

omega_variable(
    conciliar_authority_vs_papal_primacy,
    'Can conciliar authority remain binding without papal oversight, or does the conciliar system inevitably drift toward either papal centralism or regional fragmentation?',
    'Historical observation of how councils function when no universal primate exists (Oriental Orthodox, some autocephalous churches); analysis of whether councils convene regularly and enforce decisions without papal coordination.',
    'If councils require papal authority to function, the conciliar reading collapses into the tradition_scripture reading (magisterial primacy). If councils can self-organize and bind without Rome, the conciliar reading is structurally stable. This determines whether the reading forecloses the Catholic magisterium or merely coexists with it.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(conciliar_authority_vs_papal_primacy, empirical, 'Whether conciliar authority is self-sustaining or depends on centralized primacy.').

omega_variable(
    suppression_mechanism_internalization,
    'Is the measured suppression (0.42) structural (external barriers to innovation and lay interpretation) or internalized (bishops and theologians believe conciliar authority is legitimate and self-police their own innovation)?',
    'Post-council drift analysis: if bishops and theologians who leave the conciliar tradition (join Protestantism, embrace scholarship) maintain the same doctrinal conservatism, suppression is partially internalized. If they rapidly innovate, suppression is structural.',
    'If internalized, the constraint''s effective suppression is higher than the scalar suggests—the target carries the constraint with them after exit. If structural, the constraint depends on institutional barriers and would weaken if institutions change. This affects predictions about how the constraint would behave under schism or reformation pressure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'Whether suppression is structural or internalized as belief in authority.').

omega_variable(
    ecumenical_council_rarity,
    'Does the extreme rarity of ecumenical councils in the modern era (last was Vatican II in the Catholic tradition, 1962–65; the Orthodox churches have not convened an ecumenical council since 787) signal that the constraint is inoperative, or does it signal that the conciliar tradition now operates through consensus without formal councils?',
    'Examine whether modern theological consensus (e.g., on bioethics, Church and world relations) is achieved through councils or through informal patristic and scholarly consensus. If councils are truly abandoned, the conciliar reading has degraded into a Piton—maintained by ritual but not active function.',
    'If the constraint is piton-ified (theater_ratio rising, councils ceremonial), it may be vulnerable to sudden collapse or reformation if lay pressure mounts. If councils remain active through informal consensus, the constraint is still tangled_rope. This affects mandatrophy and longevity assessments.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(ecumenical_council_rarity, empirical, 'Whether ecumenical councils remain functionally active or have become theatrical maintenance.').

omega_variable(
    reading_contest_natural_or_constructed,
    'Are the three readings of the biblical_authority kernel (conciliar, sola_scriptura, tradition_scripture) genuine structural alternatives grounded in different hermeneutics, or are they post-hoc categorizations imposed on a continuous historical dispute?',
    'Historical genealogy: when did each reading''s core premises first appear? Were they always in tension, or did one emerge as a reaction to another? Do historical actors describe themselves using these categories or are they modern typologies?',
    'If the readings are genuine structural alternatives, each story represents a defensible position and the corpus should model all three. If they are historiographical impositions, the constraint boundaries are drawn by the analyst, not by the kernel itself—this affects how the corpus handles kernel decomposition vs. invention.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_contest_natural_or_constructed, conceptual, 'Whether the three biblical-authority readings are historical or historiographical constructs.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(biblical_authority__conciliar_reading, 325, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bibl_tr_t325, biblical_authority__conciliar_reading, theater_ratio, 325, 0.12).
narrative_ontology:measurement(bibl_tr_t600, biblical_authority__conciliar_reading, theater_ratio, 600, 0.18).
narrative_ontology:measurement(bibl_tr_t1100, biblical_authority__conciliar_reading, theater_ratio, 1100, 0.28).
narrative_ontology:measurement(bibl_tr_t1500, biblical_authority__conciliar_reading, theater_ratio, 1500, 0.35).
narrative_ontology:measurement(bibl_tr_t1800, biblical_authority__conciliar_reading, theater_ratio, 1800, 0.38).
narrative_ontology:measurement(bibl_tr_t2026, biblical_authority__conciliar_reading, theater_ratio, 2026, 0.38).

% Extraction over time
narrative_ontology:measurement(bibl_be_t325, biblical_authority__conciliar_reading, base_extractiveness, 325, 0.42).
narrative_ontology:measurement(bibl_be_t600, biblical_authority__conciliar_reading, base_extractiveness, 600, 0.48).
narrative_ontology:measurement(bibl_be_t1100, biblical_authority__conciliar_reading, base_extractiveness, 1100, 0.52).
narrative_ontology:measurement(bibl_be_t1500, biblical_authority__conciliar_reading, base_extractiveness, 1500, 0.58).
narrative_ontology:measurement(bibl_be_t1800, biblical_authority__conciliar_reading, base_extractiveness, 1800, 0.61).
narrative_ontology:measurement(bibl_be_t2026, biblical_authority__conciliar_reading, base_extractiveness, 2026, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(bibl_su_t325, biblical_authority__conciliar_reading, suppression_requirement, 325, 0.55).
narrative_ontology:measurement(bibl_su_t600, biblical_authority__conciliar_reading, suppression_requirement, 600, 0.48).
narrative_ontology:measurement(bibl_su_t1100, biblical_authority__conciliar_reading, suppression_requirement, 1100, 0.44).
narrative_ontology:measurement(bibl_su_t1500, biblical_authority__conciliar_reading, suppression_requirement, 1500, 0.42).
narrative_ontology:measurement(bibl_su_t1800, biblical_authority__conciliar_reading, suppression_requirement, 1800, 0.42).
narrative_ontology:measurement(bibl_su_t2026, biblical_authority__conciliar_reading, suppression_requirement, 2026, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(biblical_authority__conciliar_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(biblical_authority__conciliar_reading, 0.12).
narrative_ontology:affects_constraint(biblical_authority__conciliar_reading, biblical_authority__sola_scriptura_reading).
narrative_ontology:affects_constraint(biblical_authority__conciliar_reading, biblical_authority__tradition_scripture_reading).

% DUAL FORMULATION NOTE:
% This story is one reading of the biblical_authority kernel. It describes the conciliar reading's structure: Scripture interpreted through ecumenical councils and patristic consensus. Sibling readings (sola_scriptura, tradition_scripture) are separate constraint stories with different ε values and stakeholder structures. The three readings form a constraint family linked by affects_constraints edges. They are not three perspectives on one constraint; they are three structurally distinct constraints sharing a contested kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(biblical_authority__conciliar_reading, institutional, 0.28).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
