% ============================================================================
% CONSTRAINT STORY: john_1_1_logos__subordinationist
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-13
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_john_1_1_logos__subordinationist, []).

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
 *   constraint_id: john_1_1_logos__subordinationist
 *   human_readable: Subordinationist Reading of John 1:1 Logos
 *   domain: theology/biblical_hermeneutics/christology
 *
 * SUMMARY:
 *   This constraint instantiates the subordinationist reading of John 1:1—the
 *   interpretation that Logos (the Word) is a created being or divine agent
 *   who, while first and highest of creatures, is not co-eternal or
 *   consubstantial with the Father. The reading competes with the orthodox
 *   Nicene reading (Logos as fully divine, co-eternal, consubstantial) and
 *   the non-incarnational reading (Logos as poetic language for divine
 *   wisdom). The constraint's operation is the enforcement of the orthodox
 *   reading against subordinationist exegesis through ecclesiastical
 *   authority, doctrinal pronouncement, and exclusion from authorized
 *   interpretation. The measurement series tracks extractiveness rising
 *   sharply from the pre-Nicene period (low, before enforcement machinery was
 *   built) through the Nicene councils (rapid rise at t=300) to the
 *   consolidated medieval period (plateau at t=700+, where suppression
 *   infrastructure is fully institutionalized). Theater ratio rises from
 *   near-zero (the founding function—doctrinal settlement—was real) to
 *   moderate (0.48: much enforcement activity now defends ecclesiastical
 *   prestige rather than preventing actual doctrinal confusion), reflecting
 *   the constraint's transition from functional coordination to defended
 *   monopoly. The claim/metric independence is deliberate: subordinationism
 *   is CLAIMED as rope by its exponents (genuine coordination function:
 *   settling doctrine, preventing schism) while the metrics describe
 *   substantially extractive, actively enforced operation—a tangled rope
 *   overlaying coordination with institutional extraction.
 *
 * KEY AGENTS:
 *   - Subordinationist exegetical schools: maintain that John 1:1 admits a subordinationist reading; benefit from interpretive ambiguity preservation; organized but not institutionally supreme
 *   - High-church orthodox traditions (ECO, RCC, Chalcedonian Protestantism): their doctrinal authority and sacramental claims depend on the Logos-as-fully-divine reading; paying the constraint via exclusion of alternatives
 *   - Ecumenical council authority (Nicaea, Constantinople, Chalcedon, and successors): agenda-setter; enforces orthodox reading through doctrinal pronouncement, canonical exclusion, prestige management
 *   - Non-trinitarian communities (JWs, some LDS, Unitarians): benefit from subordinationist legitimacy; have arbitrage-grade exit (build separate communities); lower institutional power than orthodox traditions
 *   - Arianism's heirs: residual communities with subordinationist identity; identity-locked to the reading; low institutional power
 *   - Historical-critical scholarship: structurally excluded from conciliar decision-making; their re-examination of John 1:1 on linguistic grounds is treated as either heretical or incompetent
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(john_1_1_logos__subordinationist, 0.62).
domain_priors:suppression_score(john_1_1_logos__subordinationist, 0.71).
domain_priors:theater_ratio(john_1_1_logos__subordinationist, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(john_1_1_logos__subordinationist, extractiveness, 0.62).
narrative_ontology:constraint_metric(john_1_1_logos__subordinationist, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(john_1_1_logos__subordinationist, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(john_1_1_logos__subordinationist, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(john_1_1_logos__subordinationist, resistance, 0.69).

% --- Constraint claim ---
narrative_ontology:constraint_claim(john_1_1_logos__subordinationist, tangled_rope).
narrative_ontology:human_readable(john_1_1_logos__subordinationist, "Subordinationist Reading of John 1:1 Logos").
narrative_ontology:topic_domain(john_1_1_logos__subordinationist, "theology/biblical_hermeneutics/christology").

domain_priors:requires_active_enforcement(john_1_1_logos__subordinationist).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(john_1_1_logos__subordinationist, 'd1767011-da51-4104-ac96-a7deb109f552').
narrative_ontology:cs_kernel_codification('d1767011-da51-4104-ac96-a7deb109f552', fixed_text).
narrative_ontology:cs_authority_grounding('d1767011-da51-4104-ac96-a7deb109f552', extraction).
narrative_ontology:cs_interpretation_layer_present('d1767011-da51-4104-ac96-a7deb109f552').
narrative_ontology:cs_reading_relation('d1767011-da51-4104-ac96-a7deb109f552', john_1_1_logos__orthodox_christological, coexists_with).
narrative_ontology:cs_reading_relation('d1767011-da51-4104-ac96-a7deb109f552', john_1_1_logos__non_incarnational_monotheist, coexists_with).
narrative_ontology:cs_axiom('d1767011-da51-4104-ac96-a7deb109f552', foundational, logos_created_not_eternal).
narrative_ontology:cs_axiom_status(logos_created_not_eternal, holdable).
narrative_ontology:cs_axiom_grounding('d1767011-da51-4104-ac96-a7deb109f552', logos_created_not_eternal, empirically_contingent).
narrative_ontology:cs_axiom('d1767011-da51-4104-ac96-a7deb109f552', foundational, logos_dependent_on_father).
narrative_ontology:cs_axiom_status(logos_dependent_on_father, holdable).
narrative_ontology:cs_axiom_grounding('d1767011-da51-4104-ac96-a7deb109f552', logos_dependent_on_father, deontological).
narrative_ontology:cs_reference_frame('d1767011-da51-4104-ac96-a7deb109f552', subordinate_logos_ontology).
narrative_ontology:cs_drift_state('d1767011-da51-4104-ac96-a7deb109f552', contemporary_historical_critical_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('d1767011-da51-4104-ac96-a7deb109f552', '').
narrative_ontology:cs_kernel_id(john_1_1_logos__subordinationist, john_1_1_logos).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(john_1_1_logos__subordinationist, subordinationist_exegetical_schools).
narrative_ontology:constraint_beneficiary(john_1_1_logos__subordinationist, non_trinitarian_communities).
narrative_ontology:constraint_beneficiary(john_1_1_logos__subordinationist, arianism_heirs).
narrative_ontology:constraint_victim(john_1_1_logos__subordinationist, high_church_orthodox_traditions).
narrative_ontology:constraint_victim(john_1_1_logos__subordinationist, ecumenical_council_authority).
narrative_ontology:constraint_victim(john_1_1_logos__subordinationist, sacramental_exclusivity_practitioners).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(john_1_1_logos__subordinationist, arianism_heirs).
narrative_ontology:constraint_victim(john_1_1_logos__subordinationist, institutional_clergy).
narrative_ontology:constraint_victim(john_1_1_logos__subordinationist, theological_seminaries).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Exegetical communities (some Protestant evangelical scholars, some non-trinitarian seminaries, historical-critical researchers) maintain that John 1:1 grammatically permits and textually supports a subordinationist reading. They benefit from the persistence of the reading as a live exegetical option, which keeps their scholarly work from being dismissed as heresy rather than merely contestable. They have some mobility: they can publish in secular academic venues, found independent schools, or move between denominational traditions that tolerate diverse views.
narrative_ontology:constraint_stakeholder(john_1_1_logos__subordinationist, subordinationist_exegetical_schools, beneficiary,
    organized, generational, mobile, global).

% The Eastern Orthodox, Roman Catholic, and high-church Protestant denominations whose doctrinal authority and sacramental identity rest on the Nicene reading of Christ as fully divine. They bear the cost of defending this reading against exegetical challenge, maintaining doctrinal discipline, and excluding subordinationist voices from pulpits and seminaries. Exiting this constraint would require doctrinal revision at the highest levels—synodal/conciliar reconsideration of creedal statements—which would destabilize centuries of accumulated authority and upset the spiritual identity of billions of believers.
narrative_ontology:constraint_stakeholder(john_1_1_logos__subordinationist, high_church_orthodox_traditions, payer,
    institutional, civilizational, constrained, global).

% The ecumenical councils (Nicaea 325, Constantinople 381, Ephesus 431, Chalcedon 451, and their modern successors in patriarchal, papal, and synodal structures) that declare the orthodox reading canonical and subordinationism heretical. They set the rules of interpretation, control access to authoritative pronouncement, and enforce doctrinal boundaries through excommunication and exclusion. Their power is not technological but institutional: the legitimacy of a council depends on recognition by other councils and by organized Christian bodies. They are constrained by the need to maintain consensus among constituent churches and to appear continuous with tradition.
narrative_ontology:constraint_stakeholder(john_1_1_logos__subordinationist, ecumenical_council_authority, agenda_setter,
    institutional, civilizational, constrained, global).

% Jehovah's Witnesses, some Latter-day Saint communities, Unitarian Universalists, and smaller non-trinitarian churches whose christology depends on a subordinate Logos. For them, the subordinationist reading is hermeneutical legitimacy—it anchors their claim to be biblical Christians despite rejection of Nicene doctrine. Their exit options are high: they have built institutional parallel structures (kingdoms halls, temples, meeting houses) outside the ecumenical council system and can sustain themselves independently.
narrative_ontology:constraint_stakeholder(john_1_1_logos__subordinationist, non_trinitarian_communities, beneficiary,
    moderate, biographical, arbitrage, regional).

% Residual theological communities in Eastern Christianity, folk Christianity in regions where Orthodox council authority is weak, and contemporary groups that identify with Arian or semi-Arian heritage. Their christological identity is constitutively bound to rejection of Nicene formula; accepting full divinity of the Logos would be self-dissolution. They are institutionally powerless—they have no councils, no seminaries, no ecumenical standing—but their persistent identity-locked attachment to subordinationism keeps the reading alive in practice and folk belief, creating interpretive space that councils must actively suppress.
narrative_ontology:constraint_stakeholder(john_1_1_logos__subordinationist, arianism_heirs, beneficiary,
    powerless, biographical, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(john_1_1_logos__subordinationist, arianism_heirs, payer).

% Academic biblical scholars, linguists, and historians trained in historical-critical methods who would examine John 1:1 on purely exegetical grounds—what the Greek grammar, the Johannine vocabulary, the LXX parallels, and the literary context suggest about the Logos's ontological status. They are excluded from the decision-making structure because their conclusions are expected to respect dogmatic constraints: if they arrive at subordinationist readings, they are treated as heretical; if they suggest the text is ambiguous, they are treated as technically incompetent (unable to reach clear conclusions). Their exclusion is what the enforcement machinery exists to maintain.
narrative_ontology:constraint_stakeholder(john_1_1_logos__subordinationist, historical_critical_scholars, excluded,
    powerful, generational, trapped, global).

% Bishops, priests, pastors, and ordained teachers in high-church traditions who must preach and teach the Nicene reading as true. They bear the cost of defending orthodoxy against intellectual challenge, managing parishioners who raise subordinationist questions, and maintaining doctrinal discipline in their communities. They have some institutional power (they can shape their congregation's teaching) but are constrained by hierarchical authority above them (they cannot unilaterally teach subordinationism without losing ordination).
narrative_ontology:constraint_stakeholder(john_1_1_logos__subordinationist, institutional_clergy, payer,
    powerful, biographical, constrained, global).

% Institutions that train clergy and theologians, setting the curriculum and the bounds of acceptable inquiry. They are agenda-setters insofar as they determine what exegetical positions will be taught as orthodox and which will be marginalized. They are payers insofar as maintaining orthodoxy in the curriculum constrains their ability to follow historical-critical scholarship wherever it leads, and they must navigate the tension between academic freedom and doctrinal loyalty to sponsoring churches.
narrative_ontology:constraint_stakeholder(john_1_1_logos__subordinationist, theological_seminaries, agenda_setter,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(john_1_1_logos__subordinationist, theological_seminaries, payer).

% Historians, philosophers of religion, and comparative religionists who study the subordinationist/orthodox contest without commitment to either position. They observe the constraint's enforcement machinery, document its historical contingency, and analyze the theological and political stakes without stake in maintaining or dissolving the constraint.
narrative_ontology:constraint_stakeholder(john_1_1_logos__subordinationist, analytical_observers, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(john_1_1_logos__subordinationist, ecumenical_council_authority).
narrative_ontology:fixing_cost_class(john_1_1_logos__subordinationist, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Settles a fundamental question about Christian identity and doctrine—the ontological status of Christ—thereby preventing intra-Christian fragmentation into incompatible theological traditions. Early Christianity had no settled doctrine on christology; different communities held different views (some subordinationist, some adopting Platonized Logos doctrine, some identifying Christ with divine Sophia). Councils unified doctrine, established a single canonical reading, and made Christianity legible as a single religion rather than a collection of sects.
% TRANSFER_FUNCTION: Transfers hermeneutical authority from local readers, exegetical schools, and philosophical inquiry to centralized ecclesiastical councils and their successors. It moves the burden of proof—those who dissent from the Nicene reading must justify their position against council authority, not argue on exegetical grounds alone. It also transfers cultural prestige from philosophical speculation (Platonic Logos theology) to approved Christian doctrine, narrowing the range of theologically respectable positions.
% ABSENT_VOICES: Non-trinitarian traditions (especially after Nicaea, when they were anathematized and largely suppressed), Syriac and Coptic Christian communities that retained subordinationist or semi-subordinationist views, and historical-critical scholars whose exegetical conclusions might diverge from dogmatic expectations. These voices are structurally excluded because conciliar authority is defined as having the power to bind doctrine for all Christians, which presupposes that certain seats do not have a vote.
% DISAPPEARANCE_RATIONALE: If the constraint disappeared—if subordinationist exegesis were suddenly admitted as legitimate and the enforcement machinery ceased—Christian institutional identity would immediately fractionate. High-church sacramental claims would lose their scriptural foundation. Ecumenical council authority would erode: if Nicene doctrine can be re-opened, why not other conciliar decisions? Non-trinitarian Christianity would claim biblical authority and expand. Theological seminaries would reopen John 1:1 to genuine academic debate, which would fragment Christian teaching into competing schools. The organizational coherence that councils created would dissolve within a generation.
% FOUNDING_PROBLEM: By the 4th century, Christian communities held divergent and incompatible views on Christ's relation to God, which created doctrinal instability and institutional schism. Some bishops taught that Christ was created (Arius), others that he was eternally divine (Athanasius), others that the Logos was functional rather than hypostatic (Eusebius and his allies). These differences were not abstract—they had liturgical consequences (should Christ be venerated? worshipped? prayed to?), jurisdictional consequences (which bishop's authority is canonical?), and political consequences (which theology aligns with the emperor's interests?). Councils were called to settle the question and establish a unified Christian orthodoxy.
% FOUNDING_PROBLEM_CORROBORATION: The councils themselves testify to the founding problem through their legislative energy and their intensity of conflict. The letters of Athanasius and the council canons document real theological disagreement and institutional pressure. However, modern scholarship (Hanson's _The Search for the Christian Doctrine of God_, Behr's _The Nicene Faith_) contests whether the founding problem was as severe as conciliar rhetoric suggests. The pre-Nicene Christian world shows theological pluralism but also surprising unity on practical matters (liturgy, ethics, communal identity); schism may have been more institutional (political competition among bishops) than doctrinal. Furthermore, the councils may have CREATED the binary subordinationist/orthodox contest where before there was a spectrum of views. No voice from outside the benefiting parties (councils, high-church authority) affirms the founding problem as posed; historical-critical scholars document ambiguity and contingency.
narrative_ontology:disappearance_verdict(john_1_1_logos__subordinationist, world_rearranges).
narrative_ontology:founding_problem_status(john_1_1_logos__subordinationist, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(john_1_1_logos__subordinationist, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(john_1_1_logos__subordinationist, 'none', 1).
narrative_ontology:epsilon_provenance(john_1_1_logos__subordinationist, 0.62, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(john_1_1_logos__subordinationist_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(john_1_1_logos__subordinationist, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(john_1_1_logos__subordinationist_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62 at interval end) is high because the constraint extracts hermeneutical authority from local readers and scholars, concentrates it in councils, and decouples it from textual evidence—the commission (doctrinal uniformity) is detached from the marginal cost (the actual interpretive labor needed to determine what John 1:1 says). Suppression (0.71) is substantial because the constraint persists by actively excluding alternative readings, not by reader preference—subordinationist exegesis is silenced, not voluntarily abandoned. Theater ratio (0.48) is elevated but not extreme: the founding function (doctrinal settlement, preventing schism) was real through the medieval period, but from the Reformation onward, much of the enforcement activity defends ecclesiastical prestige and institutional continuity rather than preventing actual theological collapse. The measurement series shows a sharp rise at t=300 (Nicaea), where the enforcement machinery is built and extractiveness jumps from pre-conciliar ambiguity (0.15) to institutional exclusion (0.42). From t=300 to t=700, extraction and suppression rise further as the machinery consolidates. From t=700 to t=1700, the metrics plateau, indicating that the constraint reaches a stable extractive state where enforcement infrastructure is mature and routinized. The theater ratio rises throughout, indicating that performative maintenance increases as the founding function becomes less urgent—by t=1700, defending the reading is partly defending institutional authority, not just settling doctrine.
 *
 * PERSPECTIVAL GAP:
 *   From the council-authority seat, this is rope: they genuinely solved a real doctrinal crisis (early Christianity was fragmenting on christological grounds) and the constraint prevents schism. From the high-church orthodoxy seat, it is rope-to-snare mixed: they benefit from doctrinal unity but bear the cost of defending a particular reading against exegetical challenge; their exit is theoretically available (doctrinal revision) but practically impossible (centuries of authority depend on the Nicene reading). From the subordinationist seat, it is snare: they are excluded from legitimate speech, their exegetical work is delegitimized, their reading is suppressed. From the historian-analyst seat, the constraint is tangled rope: it performs genuine coordination (doctrinal settlement) but does so by extraction (silencing alternatives) rather than by demonstration (winning exegetical argument on the evidence). The engine computes this multi-seat divergence from the structural data; no reconciliation of claim to metrics is needed or appropriate.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary directionality: Subordinationist exegetes benefit from the preservation of interpretive ambiguity and the reading's persistence as a live option (even in suppression); d ≈ 0.2 (moderate beneficiary). Non-trinitarian communities benefit from hermeneutical legitimacy for their christology; they have arbitrage-grade exit (build separate communities), so d ≈ 0.15 (beneficiary with mobile options). Arian heirs benefit from identity validation but are powerless and identity-locked; d ≈ 0.25 (beneficiary but structurally trapped). Payer directionality: High-church orthodoxy pays by defending a particular reading against exegetical challenge, bearing the institutional cost of exclusion machinery, and risking doctrinal erosion if suppression weakens; they are institutionally powerful but exit-constrained (doctrinal revision at council level is prohibitively costly); d ≈ 0.78 (strong target). Ecumenical council authority enforces the constraint and collects the legitimacy benefit; they are the agenda-setter; d ≈ 0.15 (beneficiary/administrator, low target directionality). Historical-critical scholarship is excluded and would challenge the reading if admitted; d ≈ 0.85 (target: would bear reputational cost from council authority if they speak). Overrides: none needed—the derivation from beneficiary/victim + power + exit produces accurate directionality for each seat.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate—to settle Christian doctrine on christology and prevent schism—was live and urgent in the 4th-7th centuries (founding problem status = live at t=0-700). By t=1200, the doctrine is settled: there is no serious threat of widespread doctrinal schism from within mainstream Christianity; subordinationist readings are contained and marginal. Yet the constraint persists: councils still enforce the reading, seminaries still exclude subordinationist exegesis, and high-church orthodoxy still treats the reading as heretical. This is mandatrophy: the constraint's founding function has atrophied, but the constraint remains, enforced by institutional inertia. The theater_ratio trajectory captures this: as the founding function weakens, performative maintenance (theological theater, canonical gestures, prestige defense) rises. The measurement at t=1700 shows a constraint that is still substantially extractive (0.62) and suppressive (0.71) but whose extraction is now defending institutional hierarchy rather than preventing theological disaster. This is the signature of tangled rope slipping toward piton territory: the coordination function is gone or greatly diminished, but the extraction persists because the institutions benefit from it and the cost to fix (doctrinal revision at council level) exceeds what any one seat bears. Classification: tangled rope, not piton, because the constraint still enforces active suppression (excluding alternative readings) and institutional actors still defend it energetically. But the trajectory and the mandatrophy signal suggest that if suppression were to slacken, the constraint would degrade rapidly to pure inertia.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    textual_ambiguity_john_1_1,
    'Is John 1:1 textually ambiguous about the Logos''s ontological status, or does the grammar and context clearly support one reading over subordinationism?',
    'Linguistic re-examination by historical-critical exegetes (studying Johannine koine, Platonic parallels, LXX usage of logos) comparing the three readings against the Greek text and textual variants, independent of doctrinal commitment.',
    'If the text is genuinely ambiguous, the constraint''s suppression of subordinationism is pure extraction (silencing a live exegetical option). If the grammar clearly rules out subordinationism, the suppression is defending demonstrable textual truth, reducing the constraint''s extractive character. If the grammar supports subordinationism, the constraint is enforcing a false reading and is a snare, not tangled rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(textual_ambiguity_john_1_1, empirical, 'Whether John 1:1 permits subordinationist exegesis on linguistic grounds.').

omega_variable(
    doctrinal_necessity_of_nicene_settlement,
    'Was the Nicene settlement a response to a genuine threat of doctrinal schism that required a unified pronouncement, or a consolidation of power by a faction (Athanasius + Constantine) over legitimate competitors (Arius, Eusebius)?',
    'Historiographical analysis of pre-Nicene Christian communities'' actual theological diversity (desert fathers, Eastern monks, Syriac Christianity, Egyptian Coptic Christianity), institutional stakes of council participants, and post-Nicene schism patterns. Did schism emerge from doctrinal pluralism, or from council coercion?',
    'If genuine schism threat, the constraint is rope: it solved a real coordination problem. If power consolidation, the constraint is tangled rope or snare: it extracted institutional authority by suppressing alternatives. If schism emerged FROM council coercion (Nicene schism splitting Eastern Christianity), the constraint created the problem it claimed to solve—a piton wrapped in a false founding myth.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(doctrinal_necessity_of_nicene_settlement, empirical, 'Whether Nicene councils responded to genuine schism threat or created institutional monopoly.').

omega_variable(
    identity_lock_high_church_authority,
    'How much of high-church orthodoxy''s institutional identity is genuinely constituted by adherence to Nicene Christology, versus how much is cultural/political inheritance that could be revised?',
    'Post-exit suppression trajectory: if high-church communities that revisit subordinationist readings find their other institutional functions (sacrament, community, moral teaching) intact and strengthened, identity lock was performative. If doctrinal revision triggers identity dissolution, institutional collapse, or loss of authority, the lock is structural.',
    'If identity lock is structural and deep, high-church orthodoxy genuinely cannot exit (trapped payer, high d). If performative, they could exit at moderate institutional cost (moderately constrained payer, lower d). This modulates the classification: truly trapped victims support snare classification; moderately constrained support tangled rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_high_church_authority, empirical, 'Whether high-church Christian identity is constitutively bound to Nicene Christology or culturally attached.').

omega_variable(
    subordinationist_reading_as_distinct_kernel_reading,
    'Is subordinationism a coherent, internally consistent reading of John 1:1, or is it a family of incoherent positions unified only by rejection of Nicene orthodoxy?',
    'Careful exegetical work showing how a subordinationist reading of 1:1-14 maintains internal consistency (the Logos as created, elevated, but dependent on the Father) and accounts for the full theological trajectory of John''s gospel, including the incarnation claim in 1:14.',
    'If subordinationism is coherent, it is a live exegetical option that the constraint suppresses through institutional power (tangled rope). If incoherent, the constraint is defending textual sense against sophistry (reducing extraction component, approaching rope or mountain). If the tradition shows subordinationism fragmented into incompatible positions (Arian vs. semi-Arian vs. Eunomian), then early suppression may have been textually justified.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(subordinationist_reading_as_distinct_kernel_reading, conceptual, 'Whether subordinationist christology is a coherent reading of John or an unstable composite of incompatible claims.').

omega_variable(
    kernel_vs_reading_drift_state,
    'Has the exegetical debate over John 1:1 shifted from a contest between three coherent readings (orthodox, subordinationist, non-incarnational) to a constraint where the non_incarnational reading is treated as either irrelevant or implicitly absorbed into orthodoxy, leaving only a binary (orthodox vs. subordinationist)?',
    'Survey of historical-critical commentaries and theological treatises on John 1:1 across centuries, documenting how many exegetical positions are treated as ''live options'' at each historical point, and whether the ternary structure of the kernel is stable or has collapsed toward binary.',
    'If the kernel has drifted from ternary to binary, the constraint''s operation has shifted from managing three-way pluralism to enforcing a binary orthodoxy—which is a different (and more extractive) structure. The constraint''s classification might change not because of measurement change but because the constraint itself has transformed (the reference frame has shifted). This is a case where the reading''s reference frame (what the kernel looks like FROM this reading''s vantage) is itself drifting.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_vs_reading_drift_state, conceptual, 'Whether the Logos kernel''s three-way reading structure is stable or has collapsed toward binary orthodoxy vs. subordinationism.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(john_1_1_logos__subordinationist, 0, 1700).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(john_tr_t0, john_1_1_logos__subordinationist, theater_ratio, 0, 0.05).
narrative_ontology:measurement_basis(john_tr_t0, projected).
narrative_ontology:measurement(john_tr_t300, john_1_1_logos__subordinationist, theater_ratio, 300, 0.25).
narrative_ontology:measurement_basis(john_tr_t300, observed).
narrative_ontology:measurement(john_tr_t700, john_1_1_logos__subordinationist, theater_ratio, 700, 0.4).
narrative_ontology:measurement_basis(john_tr_t700, observed).
narrative_ontology:measurement(john_tr_t1200, john_1_1_logos__subordinationist, theater_ratio, 1200, 0.48).
narrative_ontology:measurement_basis(john_tr_t1200, observed).
narrative_ontology:measurement(john_tr_t1600, john_1_1_logos__subordinationist, theater_ratio, 1600, 0.49).
narrative_ontology:measurement_basis(john_tr_t1600, observed).
narrative_ontology:measurement(john_tr_t1700, john_1_1_logos__subordinationist, theater_ratio, 1700, 0.48).
narrative_ontology:measurement_basis(john_tr_t1700, observed).

% Extraction over time
narrative_ontology:measurement(john_be_t0, john_1_1_logos__subordinationist, base_extractiveness, 0, 0.15).
narrative_ontology:measurement_basis(john_be_t0, projected).
narrative_ontology:measurement(john_be_t300, john_1_1_logos__subordinationist, base_extractiveness, 300, 0.42).
narrative_ontology:measurement_basis(john_be_t300, observed).
narrative_ontology:measurement(john_be_t700, john_1_1_logos__subordinationist, base_extractiveness, 700, 0.58).
narrative_ontology:measurement_basis(john_be_t700, observed).
narrative_ontology:measurement(john_be_t1200, john_1_1_logos__subordinationist, base_extractiveness, 1200, 0.64).
narrative_ontology:measurement_basis(john_be_t1200, observed).
narrative_ontology:measurement(john_be_t1600, john_1_1_logos__subordinationist, base_extractiveness, 1600, 0.62).
narrative_ontology:measurement_basis(john_be_t1600, observed).
narrative_ontology:measurement(john_be_t1700, john_1_1_logos__subordinationist, base_extractiveness, 1700, 0.62).
narrative_ontology:measurement_basis(john_be_t1700, observed).

% Suppression requirement over time
narrative_ontology:measurement(john_su_t0, john_1_1_logos__subordinationist, suppression_requirement, 0, 0.1).
narrative_ontology:measurement_basis(john_su_t0, projected).
narrative_ontology:measurement(john_su_t300, john_1_1_logos__subordinationist, suppression_requirement, 300, 0.55).
narrative_ontology:measurement_basis(john_su_t300, observed).
narrative_ontology:measurement(john_su_t700, john_1_1_logos__subordinationist, suppression_requirement, 700, 0.68).
narrative_ontology:measurement_basis(john_su_t700, observed).
narrative_ontology:measurement(john_su_t1200, john_1_1_logos__subordinationist, suppression_requirement, 1200, 0.71).
narrative_ontology:measurement_basis(john_su_t1200, observed).
narrative_ontology:measurement(john_su_t1600, john_1_1_logos__subordinationist, suppression_requirement, 1600, 0.72).
narrative_ontology:measurement_basis(john_su_t1600, observed).
narrative_ontology:measurement(john_su_t1700, john_1_1_logos__subordinationist, suppression_requirement, 1700, 0.71).
narrative_ontology:measurement_basis(john_su_t1700, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(john_1_1_logos__subordinationist, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(john_1_1_logos__subordinationist, 0.18).
narrative_ontology:affects_constraint(john_1_1_logos__subordinationist, john_1_1_logos__orthodox_christological).
narrative_ontology:affects_constraint(john_1_1_logos__subordinationist, john_1_1_logos__non_incarnational_monotheist).
narrative_ontology:affects_constraint(john_1_1_logos__subordinationist, nicene_council_authority).
narrative_ontology:affects_constraint(john_1_1_logos__subordinationist, trinitarianism_as_systematic_doctrine).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the John 1:1 Logos kernel. The three readings—subordinationist, orthodox_christological, and non_incarnational_monotheist—share the same textual referent (John 1:1-14) but interpret it differently, producing three structurally distinct constraints with different ε values, different beneficiary/victim structures, and different classifications. Each reading's ε is fixed relative to the standing arrangement it describes (the subordinationist reading, from the vantage of its tradition, describes a constraint whose referent is the enforcement of orthodoxy against subordinationist exegesis—this is what subordinationism sees as the 'standing arrangement under contest'). The three readings are linked by network.affects_constraints: subordinationism directly influences the orthodox reading (by creating interpretive pressure) and vice versa. The non_incarnational reading sits outside both camps but is affected by enforcement machinery designed for the binary subordinationist/orthodox contest (its marginalization is collateral to the binary debate).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(john_1_1_logos__subordinationist, organized, 0.32).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
