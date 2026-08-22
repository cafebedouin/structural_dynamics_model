% ============================================================================
% CONSTRAINT STORY: biblical_authority__tradition_scripture_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_biblical_authority__tradition_scripture_reading, []).

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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: biblical_authority__tradition_scripture_reading
 *   human_readable: Magisterial Tradition-Scripture Interpretive Authority
 *   domain: theology/religious_studies
 *
 * SUMMARY:
 *   This constraint instantiates the Roman Catholic reading of biblical
 *   authority: Scripture requires tradition for authoritative interpretation,
 *   and the magisterium (papal and episcopal authority claiming apostolic
 *   succession) guards the deposit of faith and adjudicates doctrinal
 *   development. The reading asserts that Scripture is never
 *   self-interpreting; it always requires the living tradition and the
 *   magisterium's judgment to extract its true meaning. This reading is
 *   distinct from sola-scriptura (Protestant) readings and from conciliar
 *   readings (which ground tradition in councils and patristic consensus
 *   rather than magisterial decree). The constraint concentrates interpretive
 *   authority in a centralized hierarchy, preventing doctrinal fragmentation
 *   but extracting lay interpretive agency and constraining theological
 *   innovation. The constraint is CLAIMED as tangled_rope (genuine
 *   coordination function solved) while the metrics describe substantial
 *   extraction (0.78), high suppression (0.71), and rising theater ratio
 *   (0.42) — the engine will measure this gap. The claim is correct: the
 *   constraint does solve a coordination problem (preventing doctrinal chaos)
 *   AND extracts substantially from those who pay (lay people,
 *   non-magisterial theologians). Both truths coexist structurally.
 *
 * KEY AGENTS:
 *   - Magisterial hierarchy (agenda-setter, institutional power): Sets doctrine, enforces interpretation, claims apostolic succession and unbroken tradition as warrant
 *   - Lay interpretive agency (payer, powerless, identity-locked): Forbidden from authoritative reading; pays in transfer of interpretive labor; exit is apostasy
 *   - Non-magisterial theologians (payer, moderate, constrained): Can write but subject to review and condemnation; pay in self-censorship and constraint
 *   - Sacramental mediation structure (beneficiary, non-actor): The doctrine that only ordained clergy can confer sacramental grace; locked together with interpretive monopoly
 *   - Competitor readings (excluded, organized): Protestant and Orthodox readings; kept outside by claim that only magisterium has unbroken apostolic authority
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(biblical_authority__tradition_scripture_reading, 0.78).
domain_priors:suppression_score(biblical_authority__tradition_scripture_reading, 0.71).
domain_priors:theater_ratio(biblical_authority__tradition_scripture_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(biblical_authority__tradition_scripture_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(biblical_authority__tradition_scripture_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(biblical_authority__tradition_scripture_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(biblical_authority__tradition_scripture_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(biblical_authority__tradition_scripture_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(biblical_authority__tradition_scripture_reading, tangled_rope).
narrative_ontology:human_readable(biblical_authority__tradition_scripture_reading, "Magisterial Tradition-Scripture Interpretive Authority").
narrative_ontology:topic_domain(biblical_authority__tradition_scripture_reading, "theology/religious_studies").

domain_priors:requires_active_enforcement(biblical_authority__tradition_scripture_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(biblical_authority__tradition_scripture_reading, '40550e37-52c8-480a-9337-8a58972718df').
narrative_ontology:cs_kernel_codification('40550e37-52c8-480a-9337-8a58972718df', formalized).
narrative_ontology:cs_authority_grounding('40550e37-52c8-480a-9337-8a58972718df', lineage).
narrative_ontology:cs_interpretation_layer_present('40550e37-52c8-480a-9337-8a58972718df').
narrative_ontology:cs_reading_relation('40550e37-52c8-480a-9337-8a58972718df', biblical_authority__sola_scriptura_reading, forecloses).
narrative_ontology:cs_reading_relation('40550e37-52c8-480a-9337-8a58972718df', biblical_authority__conciliar_reading, coexists_with).
narrative_ontology:cs_axiom('40550e37-52c8-480a-9337-8a58972718df', foundational, apostolic_succession_confers_interpretive_authority).
narrative_ontology:cs_axiom_status(apostolic_succession_confers_interpretive_authority, holdable).
narrative_ontology:cs_axiom_grounding('40550e37-52c8-480a-9337-8a58972718df', apostolic_succession_confers_interpretive_authority, theological).
narrative_ontology:cs_axiom('40550e37-52c8-480a-9337-8a58972718df', foundational, magisterium_exclusive_doctrinal_arbiter).
narrative_ontology:cs_axiom_status(magisterium_exclusive_doctrinal_arbiter, holdable).
narrative_ontology:cs_axiom_grounding('40550e37-52c8-480a-9337-8a58972718df', magisterium_exclusive_doctrinal_arbiter, conventional).
narrative_ontology:cs_reference_frame('40550e37-52c8-480a-9337-8a58972718df', apostolic_magisterial_authority).
narrative_ontology:cs_drift_state('40550e37-52c8-480a-9337-8a58972718df', contemporary_vatican_ii_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('40550e37-52c8-480a-9337-8a58972718df', '').
narrative_ontology:cs_kernel_id(biblical_authority__tradition_scripture_reading, biblical_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(biblical_authority__tradition_scripture_reading, magisterial_hierarchy).
narrative_ontology:constraint_beneficiary(biblical_authority__tradition_scripture_reading, sacramental_mediation_structure).
narrative_ontology:constraint_victim(biblical_authority__tradition_scripture_reading, lay_interpretive_agency).
narrative_ontology:constraint_victim(biblical_authority__tradition_scripture_reading, non_magisterial_theologians).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(biblical_authority__tradition_scripture_reading, subordinate_clergy).
narrative_ontology:constraint_victim(biblical_authority__tradition_scripture_reading, subordinate_clergy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The papacy and episcopal magisterium claim exclusive interpretive authority over Scripture and Tradition. They set doctrine through councils, papal declarations, and magisterial pronouncements; subordinate clergy transmit and enforce this interpretation; laity are expected to receive it. The magisterium justifies this by claiming custodianship of the deposit of faith and unbroken succession from apostles — they assert they alone can authentically read Scripture in light of living tradition.
narrative_ontology:constraint_stakeholder(biblical_authority__tradition_scripture_reading, magisterial_hierarchy, agenda_setter,
    institutional, civilizational, arbitrage, global).

% Priests and bishops below the magisterial tier benefit from the interpretive monopoly: their authority to teach, preach, and administer sacraments derives from the magisterium's doctrinal verdicts, which they legitimate by claim to continuity with apostolic tradition. They pay in submission to higher authority and in the labor of maintaining doctrinal consistency across parishes. Leaving the clerical structure means losing clerical identity and authority.
narrative_ontology:constraint_stakeholder(biblical_authority__tradition_scripture_reading, subordinate_clergy, beneficiary,
    moderate, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(biblical_authority__tradition_scripture_reading, subordinate_clergy, payer).

% Lay Catholics are forbidden from claiming authoritative interpretation of Scripture or doctrine. They may read Scripture privately but must accept the magisterium's interpretive verdicts publicly. Their agency is confined to assenting to magisterial teaching, catechesis (learning handed down), and following pastoral instruction. Exit means spiritual apostasy (loss of salvation within Catholic cosmology) or physical departure from the Church, both identity-shattering for those socialized into Catholic identity. The constraint extracts their interpretive labor — their questions, insights, and lived experience — which the magisterium appropriates as potential doctrinal material, deciding what counts as valid development.
narrative_ontology:constraint_stakeholder(biblical_authority__tradition_scripture_reading, lay_interpretive_agency, payer,
    powerless, biographical, identity_locked, global).

% Academic theologians, religious scholars, and church intellectuals outside the magisterium can write and teach, but their work is subject to magisterial review and can be condemned (silenced, forbidden, their books placed on the Index). They pay in the form of self-censorship, delayed publication, public retractions when their theology conflicts with emerging magisterial positions. Exit means loss of Church employment, loss of theological credibility within Catholic institutional structures, and sometimes exile from Church structures entirely. Their interpretive labor produces material the magisterium examines, judges, and appropriates or suppresses.
narrative_ontology:constraint_stakeholder(biblical_authority__tradition_scripture_reading, non_magisterial_theologians, payer,
    moderate, biographical, constrained, global).

% The doctrine that sacraments (especially penance and eucharist) confer grace ex opere operato (by the work performed itself) only when administered by validly ordained clergy under magisterial doctrine. This makes the sacrament-dispensing monopoly inseparable from magisterial interpretive authority — only the magisterium can certify valid orders, valid matter and form, valid intention. The two monopolies (interpretive + sacramental) are locked together by this doctrinal reading.
narrative_ontology:constraint_stakeholder(biblical_authority__tradition_scripture_reading, sacramental_mediation_structure, beneficiary,
    institutional, civilizational, arbitrage, global).
narrative_ontology:stakeholder_non_agent(biblical_authority__tradition_scripture_reading, sacramental_mediation_structure).

% Popular devotions, parish study groups, and lay-led spiritual movements (e.g., worker-priest movement, base ecclesial communities) that attempt to generate theological insight from lived experience and scriptural reading outside magisterial direction are periodically suppressed or brought under control. They are not at the table when doctrine is set; their exclusion is enforced when they claim interpretive standing they are told they lack.
narrative_ontology:constraint_stakeholder(biblical_authority__tradition_scripture_reading, lay_devotional_communities, excluded,
    powerless, biographical, identity_locked, regional).

% Internal reform movements (modernists, integralists, Vatican II progressives, tridentine conservatives) contest the magisterium's interpretive verdicts but remain within the Church's jurisdictional bounds. They are excluded from agenda-setting: reform proposals and reinterpretations must be submitted to magisterial judgment, which decides what counts as legitimate development vs. heresy. Their exclusion from the decision table is structural — maintained by the prior claim that only the magisterium can authoritatively read Scripture-and-Tradition as a unified deposit.
narrative_ontology:constraint_stakeholder(biblical_authority__tradition_scripture_reading, reform_factions, excluded,
    organized, biographical, trapped, national).

% Protestant denominations and other Christian traditions that claim Scripture is self-interpreting, or that councils and patristic consensus (not papal magisterium) are the measure of tradition, or that the Spirit guides reading without a hierarchical interpretive monopoly — are declared heterodox and kept outside. The constraint's enforcement is directed partly inward (controlling Catholic interpretation) and partly outward (marking competitor readings as illegitimate). Their exclusion is justified by the magisterial reading: only a claim to apostolic succession + unbroken tradition confers authority; competitor claims have broken that chain.
narrative_ontology:constraint_stakeholder(biblical_authority__tradition_scripture_reading, competitor_readings, excluded,
    organized, generational, trapped, global).

% Historians of Christianity, religious studies scholars, and external analysts examining the constraint's structural properties: its costs to doctrinal innovation, its benefits to institutional stability, its suppression mechanisms, the extraction of lay interpretive labor. They take no position in the theological dispute but observe the constraint's operation.
narrative_ontology:constraint_stakeholder(biblical_authority__tradition_scripture_reading, analytical_observer, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(biblical_authority__tradition_scripture_reading, magisterial_hierarchy).
narrative_ontology:fixing_cost_class(biblical_authority__tradition_scripture_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Preserves doctrinal continuity and prevents fragmentation into mutually incompatible interpretations of Scripture and apostolic teaching. A centralized magisterium with exclusive authority to adjudicate new readings against the deposit of faith solves the coordination problem: what counts as legitimate doctrinal development vs. rupture? If every interpreter were sovereign, doctrinal coherence would fracture. The constraint offers a single, stable answer: the magisterium decides, anchored in claimed apostolic succession and unbroken tradition.
% TRANSFER_FUNCTION: Transfers interpretive authority from distributed lay and scholarly readers to a concentrated magisterial hierarchy. Lay people transfer their right to read Scripture authoritatively; theologians transfer their verdicts to magisterial review; the transfer is enforced through sacramental control (clergy mediate grace; only validly ordained clergy are authorized; only the magisterium certifies valid orders) and through doctrinal policing (heterodox readings are condemned, sometimes their authors are silenced). The extraction is sustained by the claim that this transfer is itself the deposit of faith — a matter of revealed doctrine, not institutional choice.
% ABSENT_VOICES: Lay-led theological movements, popular devotional communities attempting to generate Scripture-reading from lived experience, academic theologians whose work is placed under scrutiny or condemned, reform factions proposing doctrinal reinterpretation, and competing Christian readings claiming their own apostolic pedigree — all would object that the magisterium's interpretive monopoly is neither revealed nor necessary for coordination. They are kept out by the prior framework that declares only magisterial reading as legitimate. Their exclusion is enforced by treating their objections themselves as products of illegitimate interpretation — a circular gate.
% DISAPPEARANCE_RATIONALE: If this constraint — the magisterial monopoly on authoritative Scripture-reading and tradition-guarding — disappeared, Catholic doctrine would immediately fragment. Some parishes would adopt sola-scriptura reading; others would embrace Vatican II progressive reinterpretations the magisterium had suppressed; others would hold to traditional discipline without a centralized arbiter; lay theologians would publish freely without review; sacramental theology might shift to non-ex-opere-operato models. The Church would reorganize around competing readings, councils would lose binding force, and the institutional stability the magisterium claims to provide would be gone. The constraint is not a natural law — its disappearance would be catastrophic only for the magisterium's institutional power, not for Christian faith itself (other traditions thrive without it).
% FOUNDING_PROBLEM: In the early Church, apostles taught orally and wrote letters; after their death, multiple competing claims arose about what they had taught and what could be legitimately developed from their teaching. The constraint was built to solve fragmentation: by claiming that apostolic succession preserved an unbroken chain of authentic interpretation, and by centralizing interpretive judgment in bishops in communion with Rome, the Church could maintain doctrinal continuity across time and space. Without such a center, every local community might read Scripture differently, creating institutional chaos.
% FOUNDING_PROBLEM_CORROBORATION: The magisterium attests the founding problem is still live: doctrinal fragmentation remains a constant threat and the magisterium's interpretive monopoly is essential to prevent it. Protestant and Orthodox Christianity attest the problem is substantially overstated: they maintain doctrinal coherence without a papal magisterium, through other coordination mechanisms (councils, scriptural primacy, tradition as interpreted community consensus). Academic historians and religious scholars document that the magisterium's monopoly was not present in the early Church and was gradually constructed over centuries; they argue it solved a real coordination problem within medieval institutional Christianity but that other solutions existed and continue to exist. Internal Catholic reformers attest the problem was oversolve: the magisterium's monopoly prevents legitimate doctrinal development and silences the Spirit's voice in the lay community and in scholarly discernment.
narrative_ontology:disappearance_verdict(biblical_authority__tradition_scripture_reading, world_rearranges).
narrative_ontology:founding_problem_status(biblical_authority__tradition_scripture_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(biblical_authority__tradition_scripture_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(biblical_authority__tradition_scripture_reading, 'none', 1).
narrative_ontology:epsilon_provenance(biblical_authority__tradition_scripture_reading, 0.78, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(biblical_authority__tradition_scripture_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(biblical_authority__tradition_scripture_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(biblical_authority__tradition_scripture_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness is 0.78 at interval end: the constraint transfers interpretive authority (a high-value resource) from distributed readers to a concentrated hierarchy, and this transfer is sustained not by voluntary coordination but by sacramental control (clergy mediate grace; laity need the sacraments) and doctrinal policing (heterodox readings are condemned). Suppression is 0.71: the constraint's persistence depends on actively silencing lay interpretation, suppressing reform movements, and marking competitor readings as illegitimate — these are real suppressive actions, not side effects. Theater ratio rises from 0.25 to 0.42: early in the interval, the magisterium's doctrinal adjudications are more consequential for actual faith practice (real coordination function); by the interval's end, more enforcement activity is performative maintenance of the interpretive monopoly itself (Vatican II's official openness to modern thought coexists with intensified policing of dissenting theologians; pastoral emphasis on lay participation coexists with reaffirmed clerical monopoly on doctrine). The rising theater reflects that the coordination function is partly solved (doctrinal coherence is maintained) but increasingly defended through rhetoric and symbolic action rather than through structural necessity. One shared time grid: every metric is authored at every time point, so temporal drift is readable across all three measures simultaneously.
 *
 * PERSPECTIVAL GAP:
 *   The magisterium experiences the constraint as genuine coordination it must maintain against fragmentation forces: they see their interpretive authority as a service, a custodianship of the deposit, an apostolic responsibility. Lay people and non-magisterial theologians experience the same constraint as enforced extraction: they are told their insights and questions do not count as authoritative, their theological work is subject to surveillance and judgment, their access to sacraments is mediated through clerical intermediaries who enforce magisterial doctrine. From the hierarchy's seat, the constraint is rope (solving a real problem, benefiting all participants through doctrinal stability). From the payer seats, it is snare (the coordination story is cover for extracting interpretive authority and sacramental control). The engine computes per-seat classification from power, exit, and beneficiary/victim data; the structural divergence follows from the fact that the same arrangement provides genuine coordination value to the beneficiary (the magisterium) while imposing asymmetric costs and suppression on the payers (lay people, constrained theologians). This is exactly what tangled_rope describes: both the coordination function and the extraction are structurally real.
 *
 * DIRECTIONALITY LOGIC:
 *   Magisterial hierarchy: d near 0.0 (full beneficiary). They set the rules, collect the interpretive authority, control the sacramental distribution. Their exit options are arbitrage-level (they can shift institutional forms, reinterpret doctrines, adapt the structure — they are not trapped). Their power is institutional. They benefit from the constraint's operation without bearing its costs. Lay interpretive agency: d near 1.0 (full target). They are forbidden the thing the constraint protects (authoritative interpretation). Their exit is identity_locked (apostasy shatters their faith identity and social position). Their power is powerless. They pay in transfer of interpretive labor and in constraint of their theological voice. Non-magisterial theologians: d around 0.70 (substantially targeted). They can write but are subject to review and condemnation; their exit is constrained (leaving the Church means losing theological credibility within Catholic structures). They pay in self-censorship and career risk. Subordinate clergy: d around 0.35 (partially target, beneficiary in some dimensions). They benefit from the monopoly (their authority derives from it) but pay in submission to higher authority and in the labor of enforcing doctrinal consistency. Their exit is constrained (leaving the priesthood is a major identity rupture). Sacramental mediation (non-actor): does not have a d value; it is a beneficiary structure, not an agent. The directional structure is asymmetric: the beneficiary seat gains authority and control; the payer seats lose interpretive agency and face suppression of dissent.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem is real: early Christianity faced genuine doctrinal fragmentation. The constraint was built to solve it by centralizing interpretive authority. BUT the constraint's mandate has partly outlived its function. Modern Catholic bishops, Vatican II, and even recent popes acknowledge that the magisterium's interpretive monopoly is TOO NARROW — they call for greater lay participation, more room for theological pluralism, deeper listening to the lived experience of the faithful. Yet the constraint persists and is even hardened in some domains (papal reaffirmations of celibacy, contraception prohibition, reinterpretation of Vatican II). Why? Partly because the founding problem (doctrinal chaos) is CONTESTED — some within the hierarchy believe openness leads to fragmentation; partly because the constraint provides institutional benefits (clerical control, sacramental monopoly) that persist independent of the coordination function's necessity; partly because decentralizing interpretive authority would require dismantling centuries-old hierarchical structures. The theater ratio rising indicates that enforcement increasingly defends the monopoly-for-its-own-sake rather than defending a necessary coordination function. This is the mandatrophy signature: the constraint endures not because the founding problem is live but because the institutional structure benefits from its persistence. The correction would be to genuinely decentralize interpretive authority — to treat the magisterium as one authoritative voice among others (bishops, theologians, lay communities) rather than THE voice — but that would require the magisterium to surrender the power the constraint preserves for it. Hence mandatrophy is partly resolved (acknowledged by the magisterium itself, at least in pastoral rhetoric) and partly persistent (the structures of control remain).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    revelation_vs_construction,
    'Is the magisterium''s interpretive authority itself a matter of revealed doctrine (divinely ordained, apostolic succession as a grace-conferring structure), or a constructed institutional arrangement justified post-hoc by appeal to tradition?',
    'Genealogical analysis: when did the claim to magisterial interpretive monopoly first appear in Church teaching? Was it explicit in early councils or did it gradually accrete? If the former, it supports the revelation reading; if the latter, it supports the construction reading. Additionally: do other Christian traditions claim apostolic succession but reject magisterial monopoly? If yes, the monopoly is revealed to be constructed, not essential to succession.',
    'If constructed, the constraint becomes a snare pure — institutional extraction justified by false claims to revealed status. If revealed, the constraint remains tangled_rope — a genuine (if contested) part of the deposit of faith that the magisterium must guard. The classification would shift from snare or tangled_rope depending on this resolution.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(revelation_vs_construction, conceptual, 'Whether magisterial interpretive authority is revealed or constructed.').

omega_variable(
    suppression_mechanism_identity_locked,
    'Is the measured suppression (0.71) primarily structural (Catholic identity is socially constituted; apostasy entails loss of community, family standing, salvation narrative) or primarily enforced (the magisterium actively polices reading, condemns theology, silences dissent)?',
    'Post-exit trajectory analysis: do lay people or theologians who leave the Catholic institutional structure continue to experience the suppression (inability to engage with Catholic theological community, loss of authority within Catholic frameworks) or does the suppression lift once they exit the institutional bounds? If suppression persists (identity remains damaged, access to Catholic discourse remains barred), it is internalized. If it lifts, it is primarily structural.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure — the target carries the suppression with them, making exit less liberating and re-entry harder. If structural, the constraint''s suppression is bounded to the institutional context; exit is structurally available even if costly. The former suggests deeper extraction; the latter suggests the constraint operates more like a bounded coordination structure with exit costs rather than a psychological cage.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(suppression_mechanism_identity_locked, empirical, 'Whether suppression of lay interpretation is structural or internalized/psychological.').

omega_variable(
    alternative_coordination_mechanisms,
    'Is the magisterium''s interpretive monopoly the ONLY mechanism that could preserve doctrinal coherence, or are there other institutional forms (councils, theological commissions with lay participation, subsidiarity of interpretation to local communities, networked discernment) that could coordinate doctrine without a centered monopoly?',
    'Counterfactual analysis: what would happen if the magisterium became one authoritative voice among many (alongside bishops in council, theological schools, lay commissions) rather than THE voice? Would doctrinal chaos ensue, or would other coordination mechanisms emerge? Historical comparison: how do Orthodox churches, Anglican communion, and Protestant denominations maintain doctrinal coherence without magisterial monopoly?',
    'If alternatives exist and function adequately elsewhere, the extraction is contingent — the magisterium could maintain its coordination function while surrendering its monopoly. The constraint would be partially remediable: a weaker form (magisterium as consultative rather than binding) could preserve coordination while reducing extraction. If magisterial monopoly is necessary, the extraction is inseparable from the coordination — the constraint is truly tangled_rope, not snare-dressed-as-rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_coordination_mechanisms, empirical, 'Whether magisterial monopoly is necessary for doctrinal coherence or whether alternative mechanisms suffice.').

omega_variable(
    kernel_reading_contention,
    'Which reading of biblical_authority is correct: tradition_scripture (magisterium), sola_scriptura (Protestant), or conciliar (councils/patristics)? Or is this a matter of competing frameworks with no single true reading?',
    'This omega documents that the three readings are INCOMMENSURABLE in the strong sense: they cannot be arbitrated by a single external standard. Each grounds its authority differently (magisterial, scriptural-literal, conciliar-consensus). Resolving the contest requires choosing a framework — which is itself a theological/philosophical claim, not an empirical discovery. The omega flags this irreducible pluralism: within Christianity, the kernel ''biblical authority'' admits multiple readings, none of which can logically foreclose the others from the standpoint of someone who has not already adopted one of the readings.',
    'This reading (tradition_scripture) is live and defensible within Catholic Christianity; its sibling readings are equally live within their own traditions. The constraint''s classification AS this reading is correct; the contest is real; the only route to a single verdict would be either (a) one reading logically forecloses the others (low confidence) or (b) external authority judges one correct (but that judgment would itself be theory-laden). The corpus documents this as a live contest, not a false equivalence.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contention, conceptual, 'The incommensurability of the three readings of biblical authority and the impossibility of a neutral arbiter.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(biblical_authority__tradition_scripture_reading, 0, 35).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bibl_tr_t0, biblical_authority__tradition_scripture_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement_basis(bibl_tr_t0, observed).
narrative_ontology:measurement(bibl_tr_t5, biblical_authority__tradition_scripture_reading, theater_ratio, 5, 0.28).
narrative_ontology:measurement_basis(bibl_tr_t5, observed).
narrative_ontology:measurement(bibl_tr_t10, biblical_authority__tradition_scripture_reading, theater_ratio, 10, 0.32).
narrative_ontology:measurement_basis(bibl_tr_t10, observed).
narrative_ontology:measurement(bibl_tr_t15, biblical_authority__tradition_scripture_reading, theater_ratio, 15, 0.36).
narrative_ontology:measurement_basis(bibl_tr_t15, observed).
narrative_ontology:measurement(bibl_tr_t20, biblical_authority__tradition_scripture_reading, theater_ratio, 20, 0.39).
narrative_ontology:measurement_basis(bibl_tr_t20, observed).
narrative_ontology:measurement(bibl_tr_t25, biblical_authority__tradition_scripture_reading, theater_ratio, 25, 0.41).
narrative_ontology:measurement_basis(bibl_tr_t25, observed).
narrative_ontology:measurement(bibl_tr_t30, biblical_authority__tradition_scripture_reading, theater_ratio, 30, 0.42).
narrative_ontology:measurement_basis(bibl_tr_t30, observed).
narrative_ontology:measurement(bibl_tr_t35, biblical_authority__tradition_scripture_reading, theater_ratio, 35, 0.42).
narrative_ontology:measurement_basis(bibl_tr_t35, observed).

% Extraction over time
narrative_ontology:measurement(bibl_be_t0, biblical_authority__tradition_scripture_reading, base_extractiveness, 0, 0.58).
narrative_ontology:measurement_basis(bibl_be_t0, observed).
narrative_ontology:measurement(bibl_be_t5, biblical_authority__tradition_scripture_reading, base_extractiveness, 5, 0.62).
narrative_ontology:measurement_basis(bibl_be_t5, observed).
narrative_ontology:measurement(bibl_be_t10, biblical_authority__tradition_scripture_reading, base_extractiveness, 10, 0.68).
narrative_ontology:measurement_basis(bibl_be_t10, observed).
narrative_ontology:measurement(bibl_be_t15, biblical_authority__tradition_scripture_reading, base_extractiveness, 15, 0.72).
narrative_ontology:measurement_basis(bibl_be_t15, observed).
narrative_ontology:measurement(bibl_be_t20, biblical_authority__tradition_scripture_reading, base_extractiveness, 20, 0.75).
narrative_ontology:measurement_basis(bibl_be_t20, observed).
narrative_ontology:measurement(bibl_be_t25, biblical_authority__tradition_scripture_reading, base_extractiveness, 25, 0.77).
narrative_ontology:measurement_basis(bibl_be_t25, observed).
narrative_ontology:measurement(bibl_be_t30, biblical_authority__tradition_scripture_reading, base_extractiveness, 30, 0.78).
narrative_ontology:measurement_basis(bibl_be_t30, observed).
narrative_ontology:measurement(bibl_be_t35, biblical_authority__tradition_scripture_reading, base_extractiveness, 35, 0.78).
narrative_ontology:measurement_basis(bibl_be_t35, observed).

% Suppression requirement over time
narrative_ontology:measurement(bibl_su_t0, biblical_authority__tradition_scripture_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement_basis(bibl_su_t0, observed).
narrative_ontology:measurement(bibl_su_t5, biblical_authority__tradition_scripture_reading, suppression_requirement, 5, 0.58).
narrative_ontology:measurement_basis(bibl_su_t5, observed).
narrative_ontology:measurement(bibl_su_t10, biblical_authority__tradition_scripture_reading, suppression_requirement, 10, 0.62).
narrative_ontology:measurement_basis(bibl_su_t10, observed).
narrative_ontology:measurement(bibl_su_t15, biblical_authority__tradition_scripture_reading, suppression_requirement, 15, 0.66).
narrative_ontology:measurement_basis(bibl_su_t15, observed).
narrative_ontology:measurement(bibl_su_t20, biblical_authority__tradition_scripture_reading, suppression_requirement, 20, 0.69).
narrative_ontology:measurement_basis(bibl_su_t20, observed).
narrative_ontology:measurement(bibl_su_t25, biblical_authority__tradition_scripture_reading, suppression_requirement, 25, 0.7).
narrative_ontology:measurement_basis(bibl_su_t25, observed).
narrative_ontology:measurement(bibl_su_t30, biblical_authority__tradition_scripture_reading, suppression_requirement, 30, 0.71).
narrative_ontology:measurement_basis(bibl_su_t30, observed).
narrative_ontology:measurement(bibl_su_t35, biblical_authority__tradition_scripture_reading, suppression_requirement, 35, 0.71).
narrative_ontology:measurement_basis(bibl_su_t35, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(biblical_authority__tradition_scripture_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(biblical_authority__tradition_scripture_reading, 0.14).
narrative_ontology:affects_constraint(biblical_authority__tradition_scripture_reading, biblical_authority__sola_scriptura_reading).
narrative_ontology:affects_constraint(biblical_authority__tradition_scripture_reading, biblical_authority__conciliar_reading).
narrative_ontology:affects_constraint(biblical_authority__tradition_scripture_reading, sacramental_grace_ex_opere_operato).
narrative_ontology:affects_constraint(biblical_authority__tradition_scripture_reading, clergy_celibacy_enforcement).

% DUAL FORMULATION NOTE:
% The constraint 'biblical_authority' is contested across three readings. This story instantiates the magisterial/tradition-scripture reading. Sibling stories (sola_scriptura_reading, conciliar_reading) describe the same kernel with different ε values, different victim/beneficiary structures, and different suppression mechanisms. The three stories form a constraint family linked by network.affects_constraints. Each story claims its own type (this one tangled_rope); the engine computes per-reading classification from the structural data. The contest is real; the readings coexist as live positions in different Christian traditions.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(biblical_authority__tradition_scripture_reading, moderate, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
