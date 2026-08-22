% ============================================================================
% CONSTRAINT STORY: biblical_authority__sola_scriptura_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_biblical_authority__sola_scriptura_reading, []).

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
 *   constraint_id: biblical_authority__sola_scriptura_reading
 *   human_readable: Sola Scriptura: Scripture Alone as Sufficient Authority
 *   domain: theology/religious_authority
 *
 * SUMMARY:
 *   Sola scriptura is the reading of biblical authority instantiated by
 *   Protestant Reformation theologians (Luther, Calvin, Zwingli) that asserts
 *   Scripture alone is the sufficient and self-interpreting norm for
 *   Christian doctrine and practice. The constraint operates by
 *   decentralizing doctrinal authority from hierarchical institutions
 *   (papacy, magisterium) to congregations, pastors, and believers. Under
 *   this reading, no tradition, council, or institutional succession can bind
 *   conscience beyond what Scripture explicitly teaches. The beneficiaries
 *   are lay believers (who gain interpretive autonomy), congregational
 *   communities (who govern doctrine locally), and theological innovators
 *   (who can propose novel readings with only scriptural warrant required).
 *   The victims are institutional churches grounded in hierarchical authority
 *   (Catholic, Orthodox, High Anglican) and the seekers of universal
 *   doctrinal coherence (fragmentation becomes structural, not remediable).
 *   This reading coexists with the tradition-Scripture reading (Catholic,
 *   Orthodox) and the conciliar reading (patristic consensus), all claiming
 *   the same kernel (biblical authority) but instantiating different
 *   constraints with different extraction profiles, suppression mechanisms,
 *   and beneficiary/victim structures.
 *
 * KEY AGENTS:
 *   - lay_believers: Gain direct access to doctrine and interpretive autonomy; previously required clerical mediation
 *   - congregational_communities: Become the locus of theological authority and discipline; no hierarchical approval needed
 *   - theological_innovators: Can found new doctrines (premillennialism, Calvinism, etc.) based on scriptural claims
 *   - clerical_monopoly_holders: Lose binding authority over doctrine; trapped in institutional identity
 *   - doctrinal_coherence_seekers: Bear the cost of denominational fragmentation; identity-locked to resolving unity
 *   - tradition_adherent_institutions: Structurally disadvantaged; their authority depends on rejected institutional succession
 *   - reformation_theologians: Agenda-setters; establish the interpretive framing and polemically defend it
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(biblical_authority__sola_scriptura_reading, 0.38).
domain_priors:suppression_score(biblical_authority__sola_scriptura_reading, 0.22).
domain_priors:theater_ratio(biblical_authority__sola_scriptura_reading, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(biblical_authority__sola_scriptura_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(biblical_authority__sola_scriptura_reading, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(biblical_authority__sola_scriptura_reading, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(biblical_authority__sola_scriptura_reading, accessibility_collapse, 0.41).
narrative_ontology:constraint_metric(biblical_authority__sola_scriptura_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(biblical_authority__sola_scriptura_reading, rope).
narrative_ontology:human_readable(biblical_authority__sola_scriptura_reading, "Sola Scriptura: Scripture Alone as Sufficient Authority").
narrative_ontology:topic_domain(biblical_authority__sola_scriptura_reading, "theology/religious_authority").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(biblical_authority__sola_scriptura_reading, '4d1444b3-f4ba-4622-8f12-f000e5dc4649').
narrative_ontology:cs_kernel_codification('4d1444b3-f4ba-4622-8f12-f000e5dc4649', formalized).
narrative_ontology:cs_authority_grounding('4d1444b3-f4ba-4622-8f12-f000e5dc4649', lineage).
narrative_ontology:cs_interpretation_layer_present('4d1444b3-f4ba-4622-8f12-f000e5dc4649').
narrative_ontology:cs_reading_relation('4d1444b3-f4ba-4622-8f12-f000e5dc4649', biblical_authority__tradition_scripture_reading, coexists_with).
narrative_ontology:cs_reading_relation('4d1444b3-f4ba-4622-8f12-f000e5dc4649', biblical_authority__conciliar_reading, coexists_with).
narrative_ontology:cs_axiom('4d1444b3-f4ba-4622-8f12-f000e5dc4649', foundational, scripture_sufficient_for_doctrine).
narrative_ontology:cs_axiom_status(scripture_sufficient_for_doctrine, holdable).
narrative_ontology:cs_axiom_grounding('4d1444b3-f4ba-4622-8f12-f000e5dc4649', scripture_sufficient_for_doctrine, deontological).
narrative_ontology:cs_axiom('4d1444b3-f4ba-4622-8f12-f000e5dc4649', foundational, perspicuity_of_sacred_text).
narrative_ontology:cs_axiom_status(perspicuity_of_sacred_text, holdable).
narrative_ontology:cs_axiom_grounding('4d1444b3-f4ba-4622-8f12-f000e5dc4649', perspicuity_of_sacred_text, empirically_contingent).
narrative_ontology:cs_axiom('4d1444b3-f4ba-4622-8f12-f000e5dc4649', secondary, rejection_of_authoritative_tradition).
narrative_ontology:cs_axiom_status(rejection_of_authoritative_tradition, holdable).
narrative_ontology:cs_axiom_grounding('4d1444b3-f4ba-4622-8f12-f000e5dc4649', rejection_of_authoritative_tradition, deontological).
narrative_ontology:cs_reference_frame('4d1444b3-f4ba-4622-8f12-f000e5dc4649', scriptural_sufficiency_framework).
narrative_ontology:cs_drift_state('4d1444b3-f4ba-4622-8f12-f000e5dc4649', contemporary_denominational_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('4d1444b3-f4ba-4622-8f12-f000e5dc4649', '').
narrative_ontology:cs_kernel_id(biblical_authority__sola_scriptura_reading, biblical_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(biblical_authority__sola_scriptura_reading, lay_believers).
narrative_ontology:constraint_beneficiary(biblical_authority__sola_scriptura_reading, congregational_communities).
narrative_ontology:constraint_beneficiary(biblical_authority__sola_scriptura_reading, theological_innovators).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(biblical_authority__sola_scriptura_reading, clerical_monopoly_holders).
narrative_ontology:constraint_victim(biblical_authority__sola_scriptura_reading, doctrinal_coherence_seekers).
narrative_ontology:constraint_victim(biblical_authority__sola_scriptura_reading, tradition_adherent_institutions).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Under sola scriptura, each believer has direct access to authoritative doctrine through Scripture and the Holy Spirit's illumination. No intermediary clerical class monopolizes interpretation. They can form congregations, ordain leaders, and evaluate doctrine against the text directly. Exit is available: moving to different congregations or traditions if scriptural reasoning diverges.
narrative_ontology:constraint_stakeholder(biblical_authority__sola_scriptura_reading, lay_believers, beneficiary,
    moderate, biographical, mobile, global).

% Congregations serve as the locus of interpretation and authority under sola scriptura. Each assembly interprets Scripture for itself and ordains leadership without hierarchical approval. They maintain theological autonomy and doctrinal authority at the local level. Exit is constrained by doctrinal identity and network effects (denominational alignment and funding structures) but doctrine remains congregationally governed.
narrative_ontology:constraint_stakeholder(biblical_authority__sola_scriptura_reading, congregational_communities, beneficiary,
    organized, generational, constrained, global).

% Scholars and denominational leaders can propose new doctrinal readings grounded in Scripture without requiring ecumenical or magisterial approval. The constraint opens a competitive marketplace of interpretation: novel readings (premillennialism, pietism, fundamentalism, prosperity theology) can flourish if they claim scriptural warrant. Their exit options are high: they can found new denominations, publish independently, and build followings based on interpretive novelty.
narrative_ontology:constraint_stakeholder(biblical_authority__sola_scriptura_reading, theological_innovators, beneficiary,
    powerful, generational, arbitrage, global).

% Institutional churches (Catholic, Orthodox, High Anglican traditions) that ground clerical authority in magisterial or apostolic-succession interpretation lose their monopoly on doctrinal adjudication. They bear the cost of doctrinal fragmentation and member defection. They are trapped by their institutional commitments to hierarchical authority; reversing the constraint would require abandoning centuries of institutional claims.
narrative_ontology:constraint_stakeholder(biblical_authority__sola_scriptura_reading, clerical_monopoly_holders, payer,
    powerful, generational, trapped, global).

% Christians invested in universal doctrinal consensus across traditions bear the cost of fragmentation. Sola scriptura produces hundreds of denominations with incompatible readings of salvation, sacraments, church order, and eschatology — all claiming Scripture as warrant. Unity becomes aspirational rather than structural. They are identity-locked: their Christian identity depends on resolving doctrinal questions, yet the constraint produces permanent irresolution.
narrative_ontology:constraint_stakeholder(biblical_authority__sola_scriptura_reading, doctrinal_coherence_seekers, payer,
    moderate, civilizational, identity_locked, global).

% Eastern Orthodox, Roman Catholic, and Anglo-Catholic institutions that treat patristic tradition, ecumenical councils, and apostolic succession as binding alongside Scripture are structurally disadvantaged. Their authority claims require institutional continuity and hierarchical validation; sola scriptura undercuts both by asserting Scripture's sufficiency and transparency. Institutional exit is impossible (identity consists in the institutional structure); doctrinal exit means abandoning the tradition entirely.
narrative_ontology:constraint_stakeholder(biblical_authority__sola_scriptura_reading, tradition_adherent_institutions, payer,
    institutional, civilizational, trapped, global).

% Ecumenical councils (Nicaea, Chalcedon, Constantinople) and magisterial bodies that historically adjudicated doctrine against heresy are structurally excluded under sola scriptura. Their authority derives from tradition and institutional succession, not scriptural warrant alone. They would argue that some doctrinal questions (the nature of Christ, the Trinity) require consensus beyond individual scriptural interpretation; they are kept out by the constraint's sufficiency claim.
narrative_ontology:constraint_stakeholder(biblical_authority__sola_scriptura_reading, heresy_determiner_councils, excluded,
    institutional, civilizational, trapped, global).

% Luther, Calvin, and Reformed theologians set the interpretive agenda by framing Scripture as sufficient and perspicuous. They do the hermeneutical and polemical work to establish the reading against institutional tradition. They benefit from the disruption of clerical monopoly and the opening of the interpretive marketplace. Their exit options are high: they can establish new denominations, author new theological systems, and influence entire regions through princely patronage.
narrative_ontology:constraint_stakeholder(biblical_authority__sola_scriptura_reading, reformation_theologians, agenda_setter,
    powerful, generational, arbitrage, global).

% Historians and religious studies scholars examine sola scriptura's operation as a doctrine-producing constraint. They observe both the liberation (lay autonomy, doctrinal innovation) and the fragmentation (hundreds of competing interpretations, denominational conflict). They measure the constraint's extractiveness, suppression, and persistence without occupying any institutional seat.
narrative_ontology:constraint_stakeholder(biblical_authority__sola_scriptura_reading, analytical_observer, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(biblical_authority__sola_scriptura_reading, theological_innovators).
narrative_ontology:fixing_cost_class(biblical_authority__sola_scriptura_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the post-Reformation problem of doctrinal authority after rejecting papal and magisterial monopoly: if Scripture alone is authoritative and self-interpreting, then authority does not depend on institutional hierarchy, continuous succession, or ecumenical consensus. Each congregation and believer can adjudicate doctrine directly through scriptural reasoning and Spirit-led illumination. This decentralizes authority-setting away from institutional centers.
% TRANSFER_FUNCTION: Transfers interpretive authority (and thus doctrinal power) from hierarchical institutions (papacy, magisterium, episcopate) to congregations, pastors, and individual believers. The flow is away from concentrated clerical monopoly toward distributed lay authority. It also transfers the cost of doctrinal coherence: no longer an institutional function, coherence becomes a voluntary problem that believers must solve through denominational alignment or ecumenical dialogue.
% ABSENT_VOICES: Ecumenical councils, patristic consensus bodies, and institutional magisterium are structurally excluded — they would argue that Scripture requires interpretive tradition to resolve ambiguities, that doctrinal unanimity requires institutional adjudication, and that lay interpretation leads to heresy and fragmentation. Their absence from the constraint's operation ensures the interpretive marketplace remains open to novelty without institutional veto.
% DISAPPEARANCE_RATIONALE: If sola scriptura disappeared and hierarchical tradition-bound authority were reinstated, Protestantism would structurally cease; Catholic and Orthodox institutions would recover their magisterial and conciliar monopolies; denominations would either dissolve back into unified hierarchies or face explicit schism rather than claiming equal scriptural warrant. The entire architecture of evangelical and Reformed Christianity depends on this constraint.
% FOUNDING_PROBLEM: After 1517, the medieval Catholic Church claimed unique interpretive authority grounded in apostolic succession and magisterial decree. The constraint was constructed to answer: by what authority does an institution (the Church) bind conscience on matters Scripture does not explicitly define? The Protestant answer: Scripture is sufficient; no institutional intermediary can claim binding authority beyond what Scripture teaches.
% FOUNDING_PROBLEM_CORROBORATION: Reformation theologians attest the problem remains live: institutional churches still claim doctrinal monopoly and bind conscience through tradition. Catholic and Orthodox authorities attest the founding problem is malframed: the issue is not monopoly but how to resolve Scripture's ambiguities; Scripture alone cannot answer every doctrinal question uniformly. Historians of the Reformation document both the institutional abuses sola scriptura was constructed against AND the doctrinal fragmentation it produced. The status remains contested among the parties.
narrative_ontology:disappearance_verdict(biblical_authority__sola_scriptura_reading, world_rearranges).
narrative_ontology:founding_problem_status(biblical_authority__sola_scriptura_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(biblical_authority__sola_scriptura_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(biblical_authority__sola_scriptura_reading, 'none', 1).
narrative_ontology:epsilon_provenance(biblical_authority__sola_scriptura_reading, 0.38, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(biblical_authority__sola_scriptura_reading_tests).
:- end_tests(biblical_authority__sola_scriptura_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness at 0.38 (moderate) reflects the opening of the interpretive marketplace: lay believers and congregations gain authority previously monopolized, which is a real benefit. However, the constraint redistributes rather than eliminates authority — pastors and denominational leaders claim interpretive leadership, and novel theological movements use sola scriptura to claim authority without institutional veto. The extractiveness is not concentrated like a snare (no single seat collects all gains) but distributed across multiple seats competing for interpretive legitimacy. Theater rises from 0.08 to 0.18 over 500 years: early Reformation theology had high functional content (polemic against papal claims, hermeneutical innovation), but Protestant institutions mature into their own interpretive monopolies (Reformed orthodoxy, Calvinist scholasticism) where the 'Scripture alone' rhetoric becomes partly theater — pastors and confessions claim scriptural warrant while de facto binding conscience to confessional standards. Suppression declines from 0.35 to 0.22: the constraint's early operation required substantial institutional resistance (Catholic Counter-Reformation, secular persecution of Protestants), but as Protestant churches institutionalize, suppression mechanisms become internalized (denominational loyalty, interpretive deference) rather than external. Accessibility collapse at 0.41 reflects moderate blockage of alternatives: once a believer accepts sola scriptura, alternatives (tradition-based authority, ecumenical consensus) seem categorically wrong, but the constraint itself does not physically prevent access to contrary views — it operates through argument, not exile. Resistance at 0.72 is high because both institutional Catholicism/Orthodoxy and doctrinal seekers actively resist; the constraint persists because it solves a real post-medieval coordination problem (authority without institutional monopoly), not because it suppresses alternatives thoroughly.
 *
 * PERSPECTIVAL GAP:
 *   From the lay believer seat, sola scriptura is genuine coordinate benefit: authority becomes accessible, interpretive power is distributed, institutional oppression lifts. From the clerical monopoly seat, it is pure extraction: institutional authority erodes, member loyalty is redirected, theological coherence becomes impossible to maintain — what looks like liberation to one seat looks like institutional dissolution to another. From the doctrinal coherence seeker's seat, it is neither pure benefit nor pure extraction but a tragic trade-off: individual freedom against universal truth. The engine computes these gaps from the power/exit/beneficiary data: low-power beneficiary (lay believers, mobile exit) produces low effective extraction; high-power target with trapped exit (clerical monopoly, institutional identity) produces high effective extraction. The same constraint operates as rope from some seats (genuine coordination benefit) and tangled rope or worse from others (concentrated extraction with coordination cover). The claim is 'rope' (the Reformation's own framing: Scripture as sufficient authority solves the coordination problem of authority without monopoly), but the metrics and per-seat analysis reveal a more complex picture.
 *
 * DIRECTIONALITY LOGIC:
 *   Lay believers and congregational communities are structural beneficiaries (d low, near 0.2): they gain authority, reduce exit costs to institutional correction, and can shape doctrine locally. Their directionality is upward (toward beneficiary) because the constraint subsidizes their autonomy. Theological innovators are also beneficiaries (d ≈ 0.15): they face no doctrinal veto, only scriptural-warrant claims. Clerical monopoly holders are targets (d ≈ 0.85): they lose authority, member defection, and institutional distinctiveness; exit is trapped (institutional identity IS hierarchical succession) so they cannot flee the cost. Doctrinal coherence seekers are targets (d ≈ 0.75): they bear the cost of fragmentation but cannot exit (identity-locked to Christianity itself, which fragments). Tradition-adherent institutions are targets (d ≈ 0.8): their authority is structurally undercut. Heresy-determiner councils are excluded (not targets, not beneficiaries, structurally out of the conversation). Reformation theologians are partial beneficiaries (d ≈ 0.25) but also partial agenda-setters (d shifts toward neutral as they institutionalize) — early on they benefit from disruption; later they defend the constraint against novelty.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (institutional Church's claim to monopoly authority over doctrine) was live in 1517 and remains live in 2026 — Catholic and Orthodox churches still claim magisterial or apostolic-succession authority; Protestant churches use confessions to bind conscience. The constraint was built to solve that problem (Scripture, not institution, is the ultimate court) but persists long after the problem's circumstances changed. In the 16th century, the problem was acute: one institution (Rome) claimed binding authority. By the 20th–21st centuries, institutional fragmentation is the problem: hundreds of denominations claim scriptural warrant, producing doctrinal chaos that sola scriptura cannot resolve. The constraint no longer solves the problem it was built for (distributed authority via Scripture alone) because Scripture's transparency is lower than promised and because denominations have institutionalized their own orthodoxies. Theater rising from 0.08 to 0.18 reflects this: early Reformation theology has high functional content (genuine polemic against monopoly), but mature Protestantism uses 'Scripture alone' rhetoric to defend denominational confessions and pastor authority — the theater is the appeal to Scripture to justify what is actually institutional tradition in a new form. Theater_ratio remains below 0.5, so the constraint is not classified as piton (not mostly performance); but the upward trend is a mandatrophy signal: the founding problem is receding and the constraint is beginning to serve as cover for new institutional monopolies rather than solving the original problem.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    perspicuity_vs_ambiguity,
    'Is Scripture sufficiently clear that believers can interpret it uniformly, or does Scripture''s perspicuity apply only to matters of salvation while leaving other doctrines fundamentally ambiguous?',
    'Historical analysis of how sola scriptura advocates themselves handled disagreement (e.g., Luther vs. Zwingli on the Eucharist); examination of whether denominational fragmentation arose from interpretive error or from Scripture''s genuine underdetermination on some questions.',
    'If Scripture is uniformly clear, the high doctrinal fragmentation (hundreds of denominations) is evidence of interpretive failure or bad faith, suggesting lower extractiveness and higher suppression of alternative readings. If Scripture is underdetermined on many points, the fragmentation is an inevitable feature, not a bug — the constraint becomes less extractive (it does what it claims to do) and suppression is lower (diversity is structural, not enforced).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(perspicuity_vs_ambiguity, empirical, 'Whether doctrinal fragmentation results from Scripture''s actual ambiguity or from believers'' failure to interpret correctly.').

omega_variable(
    lay_competence_and_guidance,
    'Does sola scriptura require that individual believers be competent exegetes, or does it allow for lay deference to gifted interpreters (pastors, scholars) while maintaining the principle that Scripture is the ultimate court of appeal?',
    'Analysis of how sola scriptura works in practice within congregations: do lay believers actually read and interpret Scripture directly, or do they defer to pastoral authority while retaining the theoretical right to correct the pastor against Scripture?',
    'If lay competence is required, the constraint creates a high accessibility bar (literacy, training, time) and may suppress those without resources to engage in detailed exegesis. If deferential guidance is permitted, suppression is lower but the distinction between sola scriptura and ''Scripture as ultimate court with pastoral mediation'' becomes thin — the constraint''s claimed benefit (lay autonomy) may be theatrical.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(lay_competence_and_guidance, conceptual, 'Whether sola scriptura requires active lay exegesis or permits deference to interpreters.').

omega_variable(
    institutional_fragmentation_cost,
    'What fraction of the measured suppression and theater_ratio is attributable to the constraint''s operation, versus to the pre-existing institutional conflict between Protestant and Catholic authorities?',
    'Counterfactual: if institutional conflict had been resolved without sola scriptura (e.g., via ecumenical compromise on authority structures), would doctrinal fragmentation persist at current levels? Historical analysis of whether fragmentation is driven by the sufficiency claim itself or by institutional competition using the claim as a banner.',
    'If fragmentation is mostly institutional competition using sola scriptura as ammunition, the constraint''s actual extractiveness is lower; if fragmentation is inherent to the principle itself, extractiveness is higher. This affects whether the constraint should be classified as rope (genuine coordination benefit) or tangled_rope (coordination cover for institutional conflict).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(institutional_fragmentation_cost, empirical, 'Whether doctrinal fragmentation is structural to sola scriptura or contingent on institutional politics.').

omega_variable(
    reading_alternatives_in_kernel,
    'What is the relationship between sola scriptura (this reading) and the sibling readings (conciliar, tradition-Scripture) within a single framework? Do they foreclose each other, coexist in different parties, or influence each other structurally?',
    'Theological analysis and historical practice: can a party hold sola scriptura AND regard patristic consensus as binding (partial coexistence), or do they logically exclude? Can the readings coevolve (e.g., sola scriptura allowing ''core doctrines only'' to be read through tradition while retaining ultimate Scripture authority)?',
    'Affects the reading_relations in cs_structure: determines whether the sibling readings coexist_with, foreclose, or influence this reading. This shapes how the engine models the constraint family''s coherence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_alternatives_in_kernel, conceptual, 'Structural relationship between sola scriptura and sibling readings of biblical authority.').

omega_variable(
    suppression_mechanism_structural_vs_internalized,
    'The measured suppression (0.22) reflects institutional resistance to sola scriptura and doctrinal policing by both Protestant and Catholic authorities. Is this suppression structural (external barriers to interpretation, institutional discipline, literacy barriers) or internalized (believers accepting interpretive limitations as divinely ordained)?',
    'Historical case studies of dissent and reinterpretation: when believers challenge dominant interpretations, are they met with external sanctions (excommunication, censorship) or do they self-censor based on learned deference? Post-dissent trajectories: does suppression persist after the believer leaves the authority structure, or does it lift?',
    'If mostly structural, the constraint''s effective suppression can be reduced by removing institutional barriers. If internalized, believers carry suppression with them even when alternatives are available — the constraint''s true suppression is higher than the structural measure suggests.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_internalized, empirical, 'Whether suppression of alternative interpretations is structural or internalized.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(biblical_authority__sola_scriptura_reading, 0, 500).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bibl_tr_t0, biblical_authority__sola_scriptura_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement(bibl_tr_t100, biblical_authority__sola_scriptura_reading, theater_ratio, 100, 0.11).
narrative_ontology:measurement(bibl_tr_t200, biblical_authority__sola_scriptura_reading, theater_ratio, 200, 0.14).
narrative_ontology:measurement(bibl_tr_t300, biblical_authority__sola_scriptura_reading, theater_ratio, 300, 0.17).
narrative_ontology:measurement(bibl_tr_t400, biblical_authority__sola_scriptura_reading, theater_ratio, 400, 0.18).
narrative_ontology:measurement(bibl_tr_t500, biblical_authority__sola_scriptura_reading, theater_ratio, 500, 0.18).

% Extraction over time
narrative_ontology:measurement(bibl_be_t0, biblical_authority__sola_scriptura_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(bibl_be_t100, biblical_authority__sola_scriptura_reading, base_extractiveness, 100, 0.32).
narrative_ontology:measurement(bibl_be_t200, biblical_authority__sola_scriptura_reading, base_extractiveness, 200, 0.36).
narrative_ontology:measurement(bibl_be_t300, biblical_authority__sola_scriptura_reading, base_extractiveness, 300, 0.38).
narrative_ontology:measurement(bibl_be_t400, biblical_authority__sola_scriptura_reading, base_extractiveness, 400, 0.37).
narrative_ontology:measurement(bibl_be_t500, biblical_authority__sola_scriptura_reading, base_extractiveness, 500, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(bibl_su_t0, biblical_authority__sola_scriptura_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(bibl_su_t100, biblical_authority__sola_scriptura_reading, suppression_requirement, 100, 0.31).
narrative_ontology:measurement(bibl_su_t200, biblical_authority__sola_scriptura_reading, suppression_requirement, 200, 0.26).
narrative_ontology:measurement(bibl_su_t300, biblical_authority__sola_scriptura_reading, suppression_requirement, 300, 0.23).
narrative_ontology:measurement(bibl_su_t400, biblical_authority__sola_scriptura_reading, suppression_requirement, 400, 0.22).
narrative_ontology:measurement(bibl_su_t500, biblical_authority__sola_scriptura_reading, suppression_requirement, 500, 0.22).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(biblical_authority__sola_scriptura_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(biblical_authority__sola_scriptura_reading, 0.12).
narrative_ontology:affects_constraint(biblical_authority__sola_scriptura_reading, biblical_authority__tradition_scripture_reading).
narrative_ontology:affects_constraint(biblical_authority__sola_scriptura_reading, biblical_authority__conciliar_reading).
narrative_ontology:affects_constraint(biblical_authority__sola_scriptura_reading, denominational_fragmentation_constraint).
narrative_ontology:affects_constraint(biblical_authority__sola_scriptura_reading, reformation_institutional_authority).

% DUAL FORMULATION NOTE:
% Sola scriptura is one reading of the biblical_authority kernel. It instantiates a constraint distinct from tradition_scripture_reading and conciliar_reading, all three of which contest the same kernel but structure authority, extraction, and fragmentation differently. Sola scriptura decentralizes authority (beneficiary: lay autonomy) at the cost of coherence (victim: doctrinal unity). The ε difference between these readings is substantial: sola scriptura ~0.38 (moderate extraction as redistribution), tradition_scripture ~0.52 (higher extraction from monopoly), conciliar ~0.41 (moderate, balanced between readings). Each instantiates a different constraint family member.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(biblical_authority__sola_scriptura_reading, powerful, 0.28).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
