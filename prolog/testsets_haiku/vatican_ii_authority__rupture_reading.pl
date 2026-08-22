% ============================================================================
% CONSTRAINT STORY: vatican_ii_authority__rupture_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_vatican_ii_authority__rupture_reading, []).

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
 *   constraint_id: vatican_ii_authority__rupture_reading
 *   human_readable: Vatican II Authority Structure (Rupture Reading)
 *   domain: theological/ecclesiastical
 *
 * SUMMARY:
 *   The rupture reading of Vatican II authority asserts that the Second
 *   Vatican Council (1962-1965) represented a substantive break with
 *   pre-conciliar Catholic doctrine and practice, rather than organic
 *   development in continuity with tradition. Under this reading, Vatican II
 *   documents contain doctrinal errors, irreconcilable contradictions with
 *   prior magisterial teaching, and theological presuppositions
 *   (ressourcement, development, modern historical consciousness) that were
 *   smuggled past the Council's oversight. The constraint operates as an
 *   authority-claim enforced by the post-conciliar hierarchy: Vatican II's
 *   binding status is asserted despite internal defects and despite
 *   contradicting prior teaching. Beneficiaries are the progressive
 *   theological faction and the post-conciliar institutional hierarchy, who
 *   gain authority and interpretive monopoly through the Council's
 *   re-legitimating narrative. Victims include traditional Catholic identity
 *   (whose pre-1962 formation is relativized), practitioners of the
 *   Tridentine Mass (suppressed and discouraged), and the pre-conciliar
 *   magisterium (demoted from living authority to historical artifact). The
 *   constraint sits at the intersection of institutional coercion (the
 *   hierarchy enforces Vatican II's authority and punishes dissent) and
 *   internalized psychological suppression (traditionalists accept the
 *   Council's authority even while experiencing its teaching as erroneous,
 *   fusing their Catholic identity with obedience to what they believe to be
 *   defective doctrine). This produces the tangled-rope signature: genuine
 *   coordination function (needed to modernize the Church's institutional
 *   standing in a plural world) yoked to asymmetric extraction (progressive
 *   theology gains authority at tradition's expense).
 *
 * KEY AGENTS:
 *   - post_conciliar_episcopal_hierarchy: institutional agenda-setter, enforces Vatican II authority, cannot admit defect without destabilizing legitimacy, trapped in the constraint — identity-locked to the Council's success
 *   - progressive_theological_faction: beneficiary, gains teaching authority and institutional recognition through the Council's modernization, mobile exit to secular academia possible
 *   - traditional_catholic_identity: victim, bears suppression as doctrinal demotion and liturgical exclusion, identity-locked — exit means spiritual rupture the teaching forbids
 *   - tridentine_mass_practitioners: victim/payer, loses ritual access and legitimacy, constrained exit (can form independent communities at cost of schism charges)
 *   - SSPX traditionalist_resistance: excluded voice, explicitly rejects Vatican II authority, argues for doctrinal error — structurally expelled from magisterial conversation
 *   - theological_continuity_advocates: observer/competitor, argues Vatican II is organic development not rupture, directly contradicts this reading's core claim from within the Catholic tradition
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vatican_ii_authority__rupture_reading, 0.68).
domain_priors:suppression_score(vatican_ii_authority__rupture_reading, 0.72).
domain_priors:theater_ratio(vatican_ii_authority__rupture_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vatican_ii_authority__rupture_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(vatican_ii_authority__rupture_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(vatican_ii_authority__rupture_reading, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vatican_ii_authority__rupture_reading, accessibility_collapse, 0.79).
narrative_ontology:constraint_metric(vatican_ii_authority__rupture_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vatican_ii_authority__rupture_reading, tangled_rope).
narrative_ontology:human_readable(vatican_ii_authority__rupture_reading, "Vatican II Authority Structure (Rupture Reading)").
narrative_ontology:topic_domain(vatican_ii_authority__rupture_reading, "theological/ecclesiastical").

domain_priors:requires_active_enforcement(vatican_ii_authority__rupture_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vatican_ii_authority__rupture_reading, '705c034a-52c8-4a4b-be6b-3f63222233be').
narrative_ontology:cs_kernel_codification('705c034a-52c8-4a4b-be6b-3f63222233be', fixed_text).
narrative_ontology:cs_authority_grounding('705c034a-52c8-4a4b-be6b-3f63222233be', lineage).
narrative_ontology:cs_interpretation_layer_present('705c034a-52c8-4a4b-be6b-3f63222233be').
narrative_ontology:cs_reading_relation('705c034a-52c8-4a4b-be6b-3f63222233be', vatican_ii_authority__vatican_ii_authority_continuity_reading, coexists_with).
narrative_ontology:cs_reading_relation('705c034a-52c8-4a4b-be6b-3f63222233be', vatican_ii_authority__vatican_ii_authority_composite_overdetermination_reading, coexists_with).
narrative_ontology:cs_axiom('705c034a-52c8-4a4b-be6b-3f63222233be', foundational, vatican_ii_doctrinal_rupture).
narrative_ontology:cs_axiom_status(vatican_ii_doctrinal_rupture, holdable).
narrative_ontology:cs_axiom_grounding('705c034a-52c8-4a4b-be6b-3f63222233be', vatican_ii_doctrinal_rupture, empirically_contingent).
narrative_ontology:cs_axiom('705c034a-52c8-4a4b-be6b-3f63222233be', foundational, pre_conciliar_magisterium_non_superseded).
narrative_ontology:cs_axiom_status(pre_conciliar_magisterium_non_superseded, holdable).
narrative_ontology:cs_axiom_grounding('705c034a-52c8-4a4b-be6b-3f63222233be', pre_conciliar_magisterium_non_superseded, deontological).
narrative_ontology:cs_reference_frame('705c034a-52c8-4a4b-be6b-3f63222233be', pre_conciliar_doctrinal_authority).
narrative_ontology:cs_drift_state('705c034a-52c8-4a4b-be6b-3f63222233be', post_conciliar_institutional_consolidation, gap(codification_collapse, substantial, false)).
narrative_ontology:cs_created_at('705c034a-52c8-4a4b-be6b-3f63222233be', '').
narrative_ontology:cs_kernel_id(vatican_ii_authority__rupture_reading, vatican_ii_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vatican_ii_authority__rupture_reading, progressive_theological_faction).
narrative_ontology:constraint_beneficiary(vatican_ii_authority__rupture_reading, post_conciliar_episcopal_hierarchy).
narrative_ontology:constraint_victim(vatican_ii_authority__rupture_reading, traditional_catholic_identity).
narrative_ontology:constraint_victim(vatican_ii_authority__rupture_reading, tridentine_mass_practitioners).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(vatican_ii_authority__rupture_reading, ecumenical_protestant_partners).
narrative_ontology:constraint_vindicates(vatican_ii_authority__rupture_reading, modernist_theological_synthesis).
narrative_ontology:constraint_vindicates(vatican_ii_authority__rupture_reading, episcopal_collegiality_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the authority structure claimed by Vatican II documents. Claims the Council was ecumenical and binding, that its reforms express organic development of doctrine. Enforces the new Mass, promotes episcopal collegiality, and suppresses competing liturgical practices. Cannot admit defect without destabilizing the entire institutional legitimacy chain — institutional identity is fused with the Council's authority.
narrative_ontology:constraint_stakeholder(vatican_ii_authority__rupture_reading, post_conciliar_episcopal_hierarchy, agenda_setter,
    institutional, generational, trapped, global).

% Gains institutional recognition and teaching authority through Vatican II's endorsement of doctrinal development, ressourcement theology, and reformed liturgy. Benefits from the framing of modernism as organic development rather than rupture or error. Can exit to secular academia or independent theological publishing if the Church's structural support erodes.
narrative_ontology:constraint_stakeholder(vatican_ii_authority__rupture_reading, progressive_theological_faction, beneficiary,
    organized, generational, mobile, global).

% Bears the cost of doctrinal repositioning as error or abandonment. The Tridentine liturgy is suppressed, pre-conciliar devotional practices discouraged, and the authority of pre-1962 magisterium is relativized. Exit means leaving the One True Church — a psychological, relational, and spiritual rupture the teaching precisely forecloses as illicit. Suppression is internalized as obedience to legitimate authority.
narrative_ontology:constraint_stakeholder(vatican_ii_authority__rupture_reading, traditional_catholic_identity, payer,
    powerless, generational, identity_locked, global).

% Lose ritual access and institutional legitimacy. The Mass they were formed in is declared superseded; competing for altar time or starting independent chapels risks schism charges. Constrained rather than identity-locked because they can technically organize outside the hierarchy, but doing so costs in ecclesial standing and sacramental validity.
narrative_ontology:constraint_stakeholder(vatican_ii_authority__rupture_reading, tridentine_mass_practitioners, payer,
    moderate, biographical, constrained, regional).

% The prior teaching authority and doctrinal corpus. Under the rupture reading, Vatican II's documents declare prior teaching defective, incomplete, or erroneous — a symbolic demotion of the pre-1962 magisterium from living authority to historical artifact. Not a real agent, but listed to note what structural position it occupies in the constraint's logic.
narrative_ontology:constraint_stakeholder(vatican_ii_authority__rupture_reading, pre_conciliar_magisterium, payer,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(vatican_ii_authority__rupture_reading, pre_conciliar_magisterium).

% Explicitly rejects Vatican II's authority and maintains pre-conciliar doctrinal and liturgical forms. Held in schism or irregular standing by the post-conciliar hierarchy. Would argue that the Council's documents contain doctrinal errors and that pre-1962 teaching is not superseded; their voice is structurally expelled from official magisterial conversation.
narrative_ontology:constraint_stakeholder(vatican_ii_authority__rupture_reading, sspx_traditionalist_resistance, excluded,
    moderate, generational, trapped, regional).

% Gain recognition and dialogue status through Vatican II's opening to separated brethren and softening of pre-conciliar polemics against Protestantism. Benefit from the implicit judgment that prior Catholic teaching on Protestant heresy was erroneous or overstated. Excluded from deciding whether the Council was legitimate because their participation is predicated on accepting its authority.
narrative_ontology:constraint_stakeholder(vatican_ii_authority__rupture_reading, ecumenical_protestant_partners, beneficiary,
    organized, biographical, mobile, global).
narrative_ontology:stakeholder_secondary_role(vatican_ii_authority__rupture_reading, ecumenical_protestant_partners, excluded).

% Argue (from within Catholic tradition) that Vatican II represents organic development, not rupture. They attempt to show continuity between pre- and post-conciliar teaching on contested points (revelation, religious liberty, ecclesiology). Their reading directly contradicts this constraint's core claim, generating the kernel contest.
narrative_ontology:constraint_stakeholder(vatican_ii_authority__rupture_reading, theological_continuity_advocates, observer,
    organized, generational, mobile, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(vatican_ii_authority__rupture_reading, post_conciliar_episcopal_hierarchy).
narrative_ontology:fixing_cost_class(vatican_ii_authority__rupture_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Authorizes post-1962 Church practice and doctrine: a global institutional restructuring (episcopal collegiality, liturgical reform, ecumenical openness, revised biblical and theological frameworks) requires a legitimacy narrative that breaks continuity with past teaching while claiming to preserve essence. The constraint coordinates institutional actors around acceptance of the new regime.
% TRANSFER_FUNCTION: Transfers doctrinal authority from pre-1962 magisterial corpus to post-conciliar interpretation and from monarchical papal governance to collegial episcopal structures. Moves spiritual legitimacy from Tridentine sacramental form to reformed Mass. Transfers interpretive monopoly from pre-conciliar scholastic theology to modern ressourcement and development frameworks. Beneficiaries (progressive faction, post-conciliar hierarchy) gain institutional authority; victims (traditional identity, pre-conciliar magisterium) lose standing.
% ABSENT_VOICES: SSPX traditionalists and pre-conciliar doctrinal witnesses are structurally excluded from the Council's authority-conferring machinery. They would argue the Council's documents are defective, that its hermeneutic breaks with prior teaching, and that the post-conciliar Church is in doctrinal crisis. Their absence from the conversation is enforced by the same authority structure the rupture reading puts on trial — circularity that defines the constraint.
% DISAPPEARANCE_RATIONALE: If Vatican II's authority claim vanished (if the Council were declared invalid or its documents binding were repudiated), the post-conciliar Church's institutional legitimacy collapses. The hierarchy would lose its chief justification for liturgical reform, doctrinal repositioning, and structural centralization. Tridentine Catholicism would revive as a live option. Ecumenical partnerships would revert to pre-1962 polemical distance. The entire global Catholic institutional apparatus would reorganize around a different authority core.
% FOUNDING_PROBLEM: The Catholic Church faced modernist challenges in the late 19th and early 20th centuries: biblical criticism, evolutionary biology, democratic governance, and religious pluralism created pressure to revise doctrine and practice. Vatican II was called to address this pressure by modernizing the Church's self-understanding, theology, and discipline. Under the rupture reading, the solution chosen was capitulation to modernism rather than intelligent development — the Council is read as the defeat of pre-conciliar resistance to modern error.
% FOUNDING_PROBLEM_CORROBORATION: Progressive theologians and post-conciliar bishops attest the founding problem (modernist pressure, need for institutional updating) was live and urgent. SSPX resistance and conservative ecclesiology attest the founding problem WAS real but that Vatican II's solution was capitulation disguised as development — the problem was solved wrongly, not wisely. Independent theological historians and conservative scholars outside the hierarchy (Lefebvre, de Mattei, others) corroborate the contested status: the pressure was real, but the Council's response is read by this faction as doctrinal error, not legitimate development.
narrative_ontology:disappearance_verdict(vatican_ii_authority__rupture_reading, world_rearranges).
narrative_ontology:founding_problem_status(vatican_ii_authority__rupture_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vatican_ii_authority__rupture_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(vatican_ii_authority__rupture_reading, 'none', 1).
narrative_ontology:epsilon_provenance(vatican_ii_authority__rupture_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(vatican_ii_authority__rupture_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(vatican_ii_authority__rupture_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(vatican_ii_authority__rupture_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness measures how much the constraint transfers authority and doctrinal standing from pre-1962 teaching to post-conciliar reinterpretation. At interval t=0 (immediately post-Council), extractiveness is moderate (0.35) because the Council's documents are still fresh and the hierarchy has not yet consolidated enforcement. By t=60 (roughly 55 years post-Council, into the 2020s), extractiveness plateaus at 0.68: the progressive faction has institutionalized control of seminaries, publishing, and magisterial interpretation; the pre-conciliar teaching is treated as superseded; and the hierarchy has normalized the new Mass and doctrinal frameworks globally. The measurement series shows steady extraction accumulation (Goodhart drift signature) rather than sharp transition. Theater_ratio rises from 0.22 to 0.58: early post-conciliar life required real doctrinal argument and institutional reorganization (low theater); by mid-interval, the hierarchy begins citing Vatican II's authority without re-litigating its legitimacy (theater rises as the enforcement becomes performative affirmation of settled doctrine); plateau at 0.58 suggests an equilibrium where nearly 60% of enforcement activity is theater — defending the Council's authority by assertion and institutional boundary-maintenance rather than by ongoing doctrinal argument. Suppression_requirement rises from 0.48 to 0.72 because the constraint must work harder to maintain traditional Catholic consent as the pre-conciliar cadre ages out and younger generations are socialized exclusively into post-conciliar forms. The elderly traditionalist hears 'Vatican II' as illegitimate insertion; the 40-year-old priest-in-training hears it as settled truth. Maintaining the constraint across this generational handoff requires escalating enforcement (institutional exclusion of traditionalist candidates, public suppression of traditional liturgy, institutional shaming of pre-conciliar references). The shared time grid ensures every metric was authored at every measured moment; no metric is imputed or filled backward.
 *
 * PERSPECTIVAL GAP:
 *   The post-conciliar hierarchy and the traditional Catholic seat compute drastically different types from identical structural data. From the hierarchy's position (agenda-setter, powerful, mobile elite), the constraint coordinates a real modernization problem: the pre-conciliar Church faced institutional crisis (emptying seminaries, youth defection, credibility loss to modern thought). Vatican II re-legitimated Catholicism in a plural world — that is genuine coordination, supporting a rope or even scaffold classification. From the traditional Catholic seat (powerless, identity-locked), the same structure operates as enforced doctrinal error: the Council overturned or ambiguated prior teaching the seat treats as infallible; exit means severing spiritual identity; suppression is internalized as obligation to assent to what is perceived as heresy. The hierarchy sees binding authority and institutional necessity; the traditionalist sees authority assault and doctrinal betrayal. Both seats see the same enforcement machinery (hierarchical enforcement of Vatican II acceptance, suppression of contrary views, institutional punishment of traditionalist dissent), but directionality diverges: the hierarchy experiences the enforcement as defending legitimate coordination; the traditionalist experiences it as coercion masquerading as magisterial authority. The engine computes per-seat type from power + exit + beneficiary/victim data; the perspectival gap emerges in the structural divergence itself, not as disagreement about classification but as different lived relationships to the same constraint.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality for the post-conciliar hierarchy: powerful, mobile (can shift doctrine, interpret magisterium), institutional, beneficiary (gains authority, consolidates control). Derived d near 0.2 (beneficiary end). Directionality for the progressive theological faction: organized, mobile (can leave for secular academia), beneficiary (gains teaching authority). Derived d near 0.15 (mild beneficiary). Directionality for traditional Catholic identity: powerless, identity-locked (fused with Church membership and sacramental access), victim (suppressed and relativized). Derived d near 0.9 (target end). Directionality for SSPX resistance: moderate power, trapped (schism status forecloses reconciliation unless they accept Vatican II), excluded, payer. Derived d near 0.85. The constraint's effective extraction is therefore amplified for the powerless traditionalist (d near 1.0) and damped for the institutional hierarchy (d near 0.2), creating asymmetric impact: the hierarchy bears minimal χ while traditionalists bear maximal χ from the same ε. This asymmetry is the signature of tangled-rope with concentrated beneficiaries and diffuse victims.
 *
 * MANDATROPHY ANALYSIS:
 *   Vatican II instantiates the mandatrophy pattern: it was founded to solve a real coordination problem (Church's institutional legitimacy in modernity; integration of evolutionary biology, biblical criticism, democratic governance). The founding problem status is contested: progressives say the problem is live and Vatican II solved it; traditionalists say the problem was real but Vatican II capitulated rather than solved it wisely. The disappearance verdict is world_rearranges: if Vatican II's authority were voided, the post-conciliar institutional apparatus would destabilize immediately — the hierarchy's legitimacy depends on the Council's binding force. This (contested founding_problem + world_rearranges) is the mandatrophy trigger: a constraint whose removal would cause reorganization, whose founding problem is disputed, whose beneficiaries (progressive faction, post-conciliar hierarchy) are the ones attesting the problem is still live. The classification as tangled-rope prevents misidentifying this as pure rope (coordination without extraction) or pure snare (extraction without coordination): Vatican II genuinely coordinated the Church's institutional response to modernity, but it did so by transferring authority from pre-conciliar teaching to post-conciliar interpretation, making traditionalists the asymmetric victims of the coordination solution. Mandatrophy is partly resolved by the constraint's longevity: 60 years post-Council, the founding problem (modernity pressure) is still live in some form (institutional secularization persists, Pope Francis continues post-conciliar modernization), so the constraint has not yet crossed into pure zombie status. But the theater_ratio trajectory (rising to 0.58) and the suppression_requirement plateau suggest the constraint is moving toward piton: enforcement becomes less about defending the Council's doctrinal legitimacy and more about maintaining institutional boundary (performing the Council's authority without re-litigating it). The analysis does not resolve mandatrophy but documents the asymmetry: the beneficiaries (progressive theology, post-conciliar hierarchy) attest the founding problem is live; the victims (traditional identity) attest the founding problem was real but the solution was wrong. This contest over the founding problem's current status is unresolvable within the constraint itself — it is precisely the kernel dispute the three readings instantiate.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    rupture_vs_development_empirical,
    'Do Vatican II documents contain propositions that directly contradict pre-1962 magisterial teaching, or are apparent contradictions resolvable through hermeneutical continuity?',
    'Comparative doctrinal analysis: select concrete propositions (e.g., on religious liberty, revelation, the nature of the Church) and compare pre-conciliar and conciliar formulations. Assess whether the apparent contradiction is real (different assertions) or hermeneutical (different emphases resolving to compatible truths).',
    'If contradictions are substantive and non-hermeneutical, the rupture reading is sustained; if resolvable, the continuity reading gains traction and the constraint is reclassified downward in extractiveness.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(rupture_vs_development_empirical, empirical, 'Whether Vatican II documents contain irreducible doctrinal contradictions with prior teaching or whether apparent contradictions can be hermeneutically integrated.').

omega_variable(
    authority_circularity,
    'Is the post-conciliar hierarchy''s claim to authority grounded in Vatican II itself, creating a circularity where the constraint bootstraps its own legitimacy?',
    'Examine the chain of authority: does the post-conciliar hierarchy''s right to enforce Vatican II depend on Vatican II''s authority, or does it have independent institutional standing that precedes the Council? Is there a pre-conciliar magisterial basis that could validate Vatican II, or does Vatican II validate itself?',
    'If the authority is circular (Vatican II legitimates the hierarchy that legitimates Vatican II), suppression is likely internalizing subjects into the constraint rather than coordinating them around a truth-claim. This strengthens the tangled-rope classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(authority_circularity, conceptual, 'Whether the constraint''s legitimacy is self-grounding (circular) or anchored in independent magisterial authority.').

omega_variable(
    suppression_mechanism_internalization,
    'Is the suppression of traditional Catholic identity and Tridentine practice structural (external prohibition) or internalized (the constraint carries itself in the conscience of traditional believers)?',
    'Post-exit suppression trajectory: if a traditionalist Catholic formally exits the hierarchical Church (joining SSPX or an independent community), does the suppression persist in the form of guilt, perceived spiritual invalidity, or loss of community identity? If suppression persists after the external mechanism is removed, it is substantially internalized.',
    'Internalized suppression is more effective and harder to remedy — it indicates the constraint has fused with the victim''s identity rather than remaining external coercion. This deepens the extraction severity and suggests the constraint operates as quasi-snare for the identity-locked seat.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'Whether suppression of traditional practice is structural or internalized in Catholic identity.').

omega_variable(
    kernel_reading_scope,
    'Is this constraint one reading of a single contested kernel (vatican_ii_authority), or does it describe a different structure entirely from its sibling readings?',
    'Compare the kernel_id and reading_id declarations across all three readings (rupture, continuity, composite_overdetermination). If all three share the same kernel but disagree on reading, the constraint is properly scoped as one reading. If the structural assumptions differ (e.g., one reading posits Vatican II as a unifiable event while another denies the unity entirely), the readings may be describing different constraints.',
    'Proper kernel framing ensures the engine correctly routes the reading-contest logic; improper framing produces spurious constraint families. This omega documents whether the rupture reading shares a genuine structural kernel with its siblings or occupies a different analytical space.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_scope, conceptual, 'Whether this reading properly instantiates one kernel with its declared siblings or occupies a separate constraint structure.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vatican_ii_authority__rupture_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vati_tr_t0, vatican_ii_authority__rupture_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement_basis(vati_tr_t0, observed).
narrative_ontology:measurement(vati_tr_t10, vatican_ii_authority__rupture_reading, theater_ratio, 10, 0.35).
narrative_ontology:measurement_basis(vati_tr_t10, observed).
narrative_ontology:measurement(vati_tr_t20, vatican_ii_authority__rupture_reading, theater_ratio, 20, 0.48).
narrative_ontology:measurement_basis(vati_tr_t20, observed).
narrative_ontology:measurement(vati_tr_t30, vatican_ii_authority__rupture_reading, theater_ratio, 30, 0.54).
narrative_ontology:measurement_basis(vati_tr_t30, observed).
narrative_ontology:measurement(vati_tr_t40, vatican_ii_authority__rupture_reading, theater_ratio, 40, 0.57).
narrative_ontology:measurement_basis(vati_tr_t40, observed).
narrative_ontology:measurement(vati_tr_t50, vatican_ii_authority__rupture_reading, theater_ratio, 50, 0.58).
narrative_ontology:measurement_basis(vati_tr_t50, observed).
narrative_ontology:measurement(vati_tr_t60, vatican_ii_authority__rupture_reading, theater_ratio, 60, 0.58).
narrative_ontology:measurement_basis(vati_tr_t60, observed).

% Extraction over time
narrative_ontology:measurement(vati_be_t0, vatican_ii_authority__rupture_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement_basis(vati_be_t0, observed).
narrative_ontology:measurement(vati_be_t10, vatican_ii_authority__rupture_reading, base_extractiveness, 10, 0.48).
narrative_ontology:measurement_basis(vati_be_t10, observed).
narrative_ontology:measurement(vati_be_t20, vatican_ii_authority__rupture_reading, base_extractiveness, 20, 0.58).
narrative_ontology:measurement_basis(vati_be_t20, observed).
narrative_ontology:measurement(vati_be_t30, vatican_ii_authority__rupture_reading, base_extractiveness, 30, 0.64).
narrative_ontology:measurement_basis(vati_be_t30, observed).
narrative_ontology:measurement(vati_be_t40, vatican_ii_authority__rupture_reading, base_extractiveness, 40, 0.67).
narrative_ontology:measurement_basis(vati_be_t40, observed).
narrative_ontology:measurement(vati_be_t50, vatican_ii_authority__rupture_reading, base_extractiveness, 50, 0.68).
narrative_ontology:measurement_basis(vati_be_t50, observed).
narrative_ontology:measurement(vati_be_t60, vatican_ii_authority__rupture_reading, base_extractiveness, 60, 0.68).
narrative_ontology:measurement_basis(vati_be_t60, observed).

% Suppression requirement over time
narrative_ontology:measurement(vati_su_t0, vatican_ii_authority__rupture_reading, suppression_requirement, 0, 0.48).
narrative_ontology:measurement_basis(vati_su_t0, observed).
narrative_ontology:measurement(vati_su_t10, vatican_ii_authority__rupture_reading, suppression_requirement, 10, 0.58).
narrative_ontology:measurement_basis(vati_su_t10, observed).
narrative_ontology:measurement(vati_su_t20, vatican_ii_authority__rupture_reading, suppression_requirement, 20, 0.66).
narrative_ontology:measurement_basis(vati_su_t20, observed).
narrative_ontology:measurement(vati_su_t30, vatican_ii_authority__rupture_reading, suppression_requirement, 30, 0.7).
narrative_ontology:measurement_basis(vati_su_t30, observed).
narrative_ontology:measurement(vati_su_t40, vatican_ii_authority__rupture_reading, suppression_requirement, 40, 0.72).
narrative_ontology:measurement_basis(vati_su_t40, observed).
narrative_ontology:measurement(vati_su_t50, vatican_ii_authority__rupture_reading, suppression_requirement, 50, 0.72).
narrative_ontology:measurement_basis(vati_su_t50, observed).
narrative_ontology:measurement(vati_su_t60, vatican_ii_authority__rupture_reading, suppression_requirement, 60, 0.72).
narrative_ontology:measurement_basis(vati_su_t60, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vatican_ii_authority__rupture_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(vatican_ii_authority__rupture_reading, 0.12).
narrative_ontology:affects_constraint(vatican_ii_authority__rupture_reading, vatican_ii_authority_continuity_reading).
narrative_ontology:affects_constraint(vatican_ii_authority__rupture_reading, vatican_ii_authority_composite_overdetermination_reading).

% DUAL FORMULATION NOTE:
% Three readings of the single contested kernel vatican_ii_authority. The rupture reading asserts Vatican II as doctrinal break with errors/contradictions. The continuity reading asserts organic development and hermeneutical integration. The composite_overdetermination reading asserts radical incoherence resolvable into neither continuity nor rupture. All three share the same kernel (Vatican II's authority-claim) but interpret it according to different axioms and reference frames. The rupture reading influences both siblings by establishing the hypothesis that Vatican II documents merit scrutiny for doctrinal contradiction; the continuity and overdetermination readings respond to that challenge with different resolutions.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(vatican_ii_authority__rupture_reading, institutional, 0.22).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
