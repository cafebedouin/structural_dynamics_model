% ============================================================================
% CONSTRAINT STORY: vatican_ii_magisterial_authority__continuity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_vatican_ii_magisterial_authority__continuity_reading, []).

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
 *   constraint_id: vatican_ii_magisterial_authority__continuity_reading
 *   human_readable: Vatican II Magisterial Authority — Continuity Reading (Enforced Hermeneutic of Organic Development)
 *   domain: ecclesiology/institutional history/hermeneutics
 *
 * SUMMARY:
 *   Within the kernel of Vatican II's magisterial authority, this story
 *   instantiates the continuity reading: the Council as organic development
 *   within an unbroken tradition. The standing arrangement under contest is
 *   the discipline that enforces that reading — conciliar implementation
 *   constrained to preserve pre-conciliar doctrine, 'spirit of the Council'
 *   appeals ruled unauthorized, the Latin-preservation mandate of
 *   Sacrosanctum Concilium §36 treated as binding law rather than
 *   transitional permission, and Dignitatis Humanae receivable only as
 *   reconciled with the anti-modern corpus (thesis/hypothesis distinction or
 *   development of doctrine) rather than as its supersession.
 *   Constraint-family decomposition per the epsilon-invariance principle: the
 *   colloquial label 'how to read Vatican II' covers three structurally
 *   distinct arrangements. This reading authors a modest epsilon (~0.37 at
 *   interval end) because by its own lights the discipline protects a real
 *   inheritance and its burdens are legitimate correction of error; the
 *   rupture reading, holding the same referent, authors high epsilon (the
 *   enforced harmony experienced as institutional cover for a break the texts
 *   encode); the composite-overdetermination reading authors intermediate
 *   epsilon with a different victim set (every partisan seat paying for
 *   rivals' certainty). The three are linked as sibling readings and through
 *   network.affects_constraints rather than merged — forcing one story to
 *   span the readings would make epsilon observer-dependent, which is
 *   precisely the failure the decomposition rule forbids.
 *
 * KEY AGENTS:
 *   - roman_magisterium: agenda-setting seat (institutional/arbitrage) — promulgates the authorized reading, disciplines departures, and collects the arrangement's principal gains (vindicated unbroken authority)
 *   - traditionalist_clergy: primary beneficiary seat (organized/identity_locked) — pre-conciliar inheritance validated; owes assent in return
 *   - pre_conciliar_religious_orders: secondary beneficiary seat (organized/constrained) — founding charisms shielded from forced revision
 *   - conservative_doctrinal_laity: beneficiary seat with indirect costs (moderate/constrained)
 *   - progressive_theologians: primary payer seat (moderate/constrained) — unauthorized readings meet review gates and censure
 *   - vernacular_liturgists: payer seat (organized/constrained) — expansions rolled back to the SC §36 boundary
 *   - diocesan_pastoral_implementers: payer seat (moderate/constrained) — programs trimmed to continuity-compatible form
 *   - historical_critical_scholars: excluded seat (moderate/mobile) — discontinuity evidence stays outside the interpretive room
 *   - sedevacantist_traditionalists: excluded seat (powerless/identity_locked) — rupture charge aimed at the arrangement from outside it
 *   - ecumenical_dialogue_commissions: observer seat (institutional/analytical) — track whether the authorized reading permits convergence
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vatican_ii_magisterial_authority__continuity_reading, 0.37).
domain_priors:suppression_score(vatican_ii_magisterial_authority__continuity_reading, 0.6).
domain_priors:theater_ratio(vatican_ii_magisterial_authority__continuity_reading, 0.31).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vatican_ii_magisterial_authority__continuity_reading, extractiveness, 0.37).
narrative_ontology:constraint_metric(vatican_ii_magisterial_authority__continuity_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(vatican_ii_magisterial_authority__continuity_reading, theater_ratio, 0.31).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vatican_ii_magisterial_authority__continuity_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(vatican_ii_magisterial_authority__continuity_reading, resistance, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vatican_ii_magisterial_authority__continuity_reading, tangled_rope).
narrative_ontology:human_readable(vatican_ii_magisterial_authority__continuity_reading, "Vatican II Magisterial Authority — Continuity Reading (Enforced Hermeneutic of Organic Development)").
narrative_ontology:topic_domain(vatican_ii_magisterial_authority__continuity_reading, "ecclesiology/institutional history/hermeneutics").

domain_priors:requires_active_enforcement(vatican_ii_magisterial_authority__continuity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vatican_ii_magisterial_authority__continuity_reading, '5211f28a-579a-429c-87ba-259d4b517e59').
narrative_ontology:cs_kernel_codification('5211f28a-579a-429c-87ba-259d4b517e59', fixed_text).
narrative_ontology:cs_authority_grounding('5211f28a-579a-429c-87ba-259d4b517e59', lineage).
narrative_ontology:cs_interpretation_layer_present('5211f28a-579a-429c-87ba-259d4b517e59').
narrative_ontology:cs_reading_relation('5211f28a-579a-429c-87ba-259d4b517e59', vatican_ii_magisterial_authority__rupture_reading, forecloses).
narrative_ontology:cs_reading_relation('5211f28a-579a-429c-87ba-259d4b517e59', vatican_ii_magisterial_authority__composite_overdetermination_reading, forecloses).
narrative_ontology:cs_axiom('5211f28a-579a-429c-87ba-259d4b517e59', foundational, organic_continuity_of_magisterium).
narrative_ontology:cs_axiom_status(organic_continuity_of_magisterium, holdable).
narrative_ontology:cs_axiom_grounding('5211f28a-579a-429c-87ba-259d4b517e59', organic_continuity_of_magisterium, deontological).
narrative_ontology:cs_axiom('5211f28a-579a-429c-87ba-259d4b517e59', foundational, letter_only_binding_interpretive_rule).
narrative_ontology:cs_axiom_status(letter_only_binding_interpretive_rule, holdable).
narrative_ontology:cs_axiom_grounding('5211f28a-579a-429c-87ba-259d4b517e59', letter_only_binding_interpretive_rule, conventional).
narrative_ontology:cs_axiom('5211f28a-579a-429c-87ba-259d4b517e59', secondary, dh_syllabus_reconcilable_via_development).
narrative_ontology:cs_axiom_status(dh_syllabus_reconcilable_via_development, holdable).
narrative_ontology:cs_axiom_grounding('5211f28a-579a-429c-87ba-259d4b517e59', dh_syllabus_reconcilable_via_development, empirically_contingent).
narrative_ontology:cs_reference_frame('5211f28a-579a-429c-87ba-259d4b517e59', unbroken_tradition_transmission).
narrative_ontology:cs_drift_state('5211f28a-579a-429c-87ba-259d4b517e59', contemporary_implementation_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('5211f28a-579a-429c-87ba-259d4b517e59', '').
narrative_ontology:cs_kernel_id(vatican_ii_magisterial_authority__continuity_reading, vatican_ii_magisterial_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vatican_ii_magisterial_authority__continuity_reading, roman_magisterium).
narrative_ontology:constraint_beneficiary(vatican_ii_magisterial_authority__continuity_reading, traditionalist_clergy).
narrative_ontology:constraint_beneficiary(vatican_ii_magisterial_authority__continuity_reading, pre_conciliar_religious_orders).
narrative_ontology:constraint_beneficiary(vatican_ii_magisterial_authority__continuity_reading, conservative_doctrinal_laity).
narrative_ontology:constraint_victim(vatican_ii_magisterial_authority__continuity_reading, progressive_theologians).
narrative_ontology:constraint_victim(vatican_ii_magisterial_authority__continuity_reading, vernacular_liturgists).
narrative_ontology:constraint_victim(vatican_ii_magisterial_authority__continuity_reading, diocesan_pastoral_implementers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(vatican_ii_magisterial_authority__continuity_reading, conservative_doctrinal_laity).
narrative_ontology:constraint_vindicates(vatican_ii_magisterial_authority__continuity_reading, doctrine_of_indefectibility).
narrative_ontology:constraint_vindicates(vatican_ii_magisterial_authority__continuity_reading, theory_of_organic_doctrinal_development).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Promulgates and polices the authorized reading of the Council through doctrinal notifications, liturgical rulings, and the assent requirements attached to conciliar texts; disciplines implementation it judges unauthorized. Its unbroken-authority claim is the asset the continuity frame vindicates, so it collects the arrangement's principal returns: doctrinal capital and concentrated interpretive discretion. It funds and staffs the enforcement machinery and absorbs the reputational strain of visible dissent, and it alone can redefine or relax the rules it administers.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__continuity_reading, roman_magisterium, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(vatican_ii_magisterial_authority__continuity_reading, roman_magisterium, beneficiary).

% Priests and communities ministering in pre-conciliar forms. The continuity reading keeps their inheritance canonically respectable and the older missal defensible, and the binding Latin mandate secures the language their worship lives in. In return they owe assent to the Council as continuously read, including texts they privately doubt; refusing assent costs them the validation that anchors their ministry. Leaving the priesthood would mean abandoning an identity formed through ordination and those forms, so exit is not practically available however dissatisfied they become.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__continuity_reading, traditionalist_clergy, beneficiary,
    organized, generational, identity_locked, global).

% Orders founded before the Council whose constitutions and charisms predate it. The reading affirms their continuity and shields founding charisms from forced revision, which protects their canonical standing and internal life. They accept the conciliar texts as legitimately their own under existing canonical dependence; their option space runs from compliance to quiet negotiation, not departure, since their juridical existence is defined inside the structure.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__continuity_reading, pre_conciliar_religious_orders, beneficiary,
    organized, generational, constrained, global).

% Laypeople attached to doctrinal continuity. They receive coherence — one answer to what the Church teaches — and liturgical predictability, and many organize to defend the reading. They also absorb indirect costs: familiar postconciliar practices they have assimilated get corrected as unauthorized, and their parishes become recurring sites of liturgy conflict. Departure would mean leaving the community their family and devotional life are built inside, so most stay and absorb.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__continuity_reading, conservative_doctrinal_laity, beneficiary,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(vatican_ii_magisterial_authority__continuity_reading, conservative_doctrinal_laity, payer).

% Academic theologians developing readings the authorized frame rules out — appeals to the Council's spirit over its letter, or Dignitatis Humanae read as superseding the anti-modern corpus. Their work passes through censorial review; adverse verdicts mean delayed or refused publication, loss of teaching chairs, and public correction. Their subject matter is the Church itself, so exit means leaving the field that constitutes their vocation; some move to non-confessional faculties, at the price of audience and topic.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__continuity_reading, progressive_theologians, payer,
    moderate, biographical, constrained, continental).

% Liturgy offices and consultants in bishops' conferences expanding vernacular and inculturated worship beyond the authorized boundary. Corrections citing the Latin-preservation mandate unwind their translations and adaptations, sometimes years into implementation. Their institutional home is the structure reviewing them, so they hold organizational voice without organizational escape; their work survives only inside the permitted envelope.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__continuity_reading, vernacular_liturgists, payer,
    organized, biographical, constrained, regional).

% Pastors, catechetical directors, and program officers building pastoral practice on a premise of conciliar discontinuity — revised moral formation, widened ecumenical sharing, locally adapted governance. Review trims their programs back to continuity-compatible shape, and compliance functions as a career condition. They lack the standing to contest the frame and the mobility to leave the parishes their work serves.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__continuity_reading, diocesan_pastoral_implementers, payer,
    moderate, biographical, constrained, regional).

% Historians tracing the conciliar draft history, the fate of the minority interventions, and the points where the texts depart from prior formulations. They publish freely in academic venues but enter official interpretation only as filtered background material. They would object that a frame which fixes the conclusion before weighing the documents predetermines the inquiry; they stand outside the interpretive process their evidence bears on.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__continuity_reading, historical_critical_scholars, excluded,
    moderate, generational, mobile, continental).

% Traditionalists who hold the postconciliar see vacant and treat the continuity reading as a laundering of a real rupture. They are outside the conversation their charge targets: penalized, unrecognized, and organizationally marginal. Their identity is fused with the judgment that the break happened, so neither the reading nor its enforcement offers them anything to accept; their objection registers only as the extreme pole the frame defines itself against.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__continuity_reading, sedevacantist_traditionalists, excluded,
    powerless, generational, identity_locked, global).

% Joint commissions with Orthodox and Protestant partners tracking whether the authorized reading leaves room for convergence on authority, liturgy, and religious liberty. They take testimony from the seated parties, observe enforcement patterns, and report on effects without bearing any themselves. Their assessments feed partner-church decisions about dialogue rather than the frame's own administration.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__continuity_reading, ecumenical_dialogue_commissions, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(vatican_ii_magisterial_authority__continuity_reading, roman_magisterium).
narrative_ontology:fixing_cost_class(vatican_ii_magisterial_authority__continuity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains a single authorized interpretation of the Council across a global communion: one answer to what the texts mean and license, so that parishes, seminaries, publishers, and bishops' conferences coordinate on the same doctrinal baseline, and the pre-conciliar corpus remains usable alongside the conciliar one. Whatever else the arrangement does, it does solve the problem of concurrent rival readings inside one institution.
% TRANSFER_FUNCTION: Moves interpretive discretion and disciplinary leverage toward the Roman center — notifications, review gates, assent demands — and moves compliance costs outward: publication delay and career risk from theologians, rollback of vernacular expansion from liturgy offices, trimmed programs from pastoral implementers. It moves doctrinal capital (the credibility of an unbroken teaching chain) to the institution as a whole, and validation specifically to pre-conciliar constituencies.
% ABSENT_VOICES: Historical-critical scholars holding documentary evidence of discontinuity would object that the frame predetermines their findings; rupture-leaning and sedevacantist traditionalists would object that the reading launders a real break; laypeople formed in the postconciliar 'spirit' would object that their received practice is being retroactively ruled unauthorized. None sits inside the interpretive process — their objections arrive only as objects of adjudication (corrected, censured, or ignored), which is what the excluded seats record.
% DISAPPEARANCE_RATIONALE: If the single-reading discipline vanished overnight — the magisterium ceasing to enforce one authorized interpretation and letting continuity, rupture, and composite readings stand equal — the communion would begin sorting along the conciliar fault line: seminaries, orders, and dioceses aligning with rival readings, mutual recognition of ministries and sacraments strained, and the authority of the pre-conciliar corpus becoming a partisan question. Whether that rearrangement would be healthier is disputed between the seats; that it would be a rearrangement is not.
% FOUNDING_PROBLEM: Bind the Council's reforms to the inherited magisterium tightly enough that the authority which promulgated both remains intact — blocking the Council from functioning as a warrant for discontinuous innovation ('the spirit of the Council') while also blocking wholesale rejection of the Council by those who read it as a break.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the beneficiary set: the documented conciliar-era minority resistance (the organized interventions of the minority fathers against the schemas) attests that the binding problem was real and contested at the source; secular historians of the Council trace how the letter-versus-spirit split opened immediately upon promulgation; and the censured theologians, as adversarial witnesses, attest that the enforcement side of the problem remains active. No element of the attestation relies solely on the offices that administer the arrangement.
narrative_ontology:disappearance_verdict(vatican_ii_magisterial_authority__continuity_reading, world_rearranges).
narrative_ontology:founding_problem_status(vatican_ii_magisterial_authority__continuity_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vatican_ii_magisterial_authority__continuity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(vatican_ii_magisterial_authority__continuity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(vatican_ii_magisterial_authority__continuity_reading, 0.37, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(vatican_ii_magisterial_authority__continuity_reading_tests).
:- end_tests(vatican_ii_magisterial_authority__continuity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Claim and metrics are authored independently. The claimed type is what the structure shows on inspection: the arrangement performs a real coordination function — holding one authorized reading of the Council across a global communion prevents schism along the conciliar fault line and keeps a two-millennium teaching corpus usable — while the same machinery imposes asymmetric costs: careers, programs, and liturgical projects are trimmed to fit the frame, and rival readings of the same texts are kept out of office. Genuine coordination plus enforced asymmetry requiring active policing is why the claim is tangled_rope. The metrics are authored from this reading's own seat. Extractiveness is deliberately modest (0.37 at interval end) because a continuity-committed author rates the burdens it imposes as regrettable-but-legitimate discipline rather than extraction — this reading-indexed epsilon over the fixed referent is itself the corpus datum, and the gap between it and the payer seats' computed burden is the perspectival measurement this family exists to take. Suppression (0.60) is a raw structural fact the reading does not dispute — assent is mandatory, review gates publication, deviation ends careers — and is reported unscaled by power or scope. Theater (0.31) reflects anniversary-and-affirmation activity layered on functional enforcement. Accessibility collapse (0.45): rival readings survive in academia and traditionalist circles but collapse inside seminaries, chanceries, and publishing gates. Resistance (0.65): five decades of progressive dissent, traditionalist refusal, and recurring liturgy conflict. The suppression_requirement series traces enforcement capacity rather than sentiment: built up against the progressive pole through the censures era (peak ~0.63 at T=20), eased as normalization set in (0.48 at T=50), then re-ratcheted (0.60 at T=60) as enforcement redirected toward the traditionalist pole — a redirection of the machinery, not a reduction. The arc is rise-decay-ratchet rather than oscillatory, so no intermittent-reinforcement cycle is asserted. One gaming alert recorded for the Boltzmann layer: this constraint declares identity_coordination, and 'unbroken identity / this is who we are' is exactly the cover-story register that type invites — the coupling test should watch for extraction concentrated on powerless agents at large scope hiding behind the identity offset.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently from identical texts. From the agenda-setter seat the arrangement is stewardship it performs and can recalibrate at will (arbitrage exit): infrastructure it built. From the beneficiary seats the same structure is validation their identity depends on — the traditionalist clergyman's priestly self is constituted through the pre-conciliar forms the reading vindicates, so exit is unthinkable without dissolving the self, which is what makes the validation flows sticky and the assent owed in return cheap to collect. From the payer seats the structure operates as a ceiling: the theologian experiences the frame as the reason a manuscript cannot appear, the liturgist as the citation that unwinds a translation, the implementer as the reviewer's red pen — and exit forfeits vocation rather than relocating it (constrained, not mobile). Two same-level academic seats diverge on exposure alone: the theologian works inside the review perimeter (constrained), the historian outside it (mobile) — equal credentials, unequal constraint-specific burden, which is why nominal power parity does not yield equal experience. Excluded seats never enter the computation they would overturn: historians supply the discontinuity evidence and rupture-leaning traditionalists the break charge, but both stand outside the room. The payer seats share a burden but are professionally scattered — a theologian-liturgist-implementer coalition capable of contesting the frame exists sociologically, yet the same review gates that price individual dissent keep the coalition from organizing. The engine computes these divergences from the structural data; this story does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive d toward the subsidized end: traditionalist clergy and pre-conciliar orders collect validation; conservative laity collect coherence while carrying diffuse indirect costs (dual-positioned, so their d sits slightly above the pure-beneficiary floor). Victim declarations drive d toward the target end: theologians, liturgists, and implementers bear the transfer of discretion with constrained exits, which pushes their effective burden toward the full-target end relative to equally placed mobile agents. The magisterium is genuinely dual-positioned — it runs the machinery and collects its product — so its derived d sits near the beneficiary end with mild upward pressure from the enforcement costs it funds; the structural derivation captures this without an override, and none is authored. Inter-institutionally, bishops'-conference liturgy offices hold organizational power yet constrained exit: their institutional home is the very structure reviewing them, so organizational rank buys voice but not escape. Scope amplification is modest for a global structure: enforcement runs through dense local review channels (imprimatur, office review, assent demands), keeping verification comparatively tractable despite worldwide reach.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — binding the Council to the inherited magisterium so that the authority issuing both remains intact — is still live: every recent liturgy dispute, censure, and synodal fight re-runs it, so no obsolescence is declared and the dead-problem-plus-world-rearranges mismatch flag does not fire. The classification guards both mislabels. Read only from the payer seats, the arrangement looks like pure extraction — censorship with a unity alibi; the coordination half (schism prevention, corpus coherence, a usable pre-conciliar inheritance) is what stops that misread. Read only from the agenda-setter and beneficiary seats, it looks like costless stewardship; the enforced asymmetry and the careers spent on one side of it stop that misread. Holding both halves in one structure is what makes the seat divergence measurable rather than merely arguable, and the reading-indexed epsilon keeps this file comparable to its siblings: same referent, different lights, different number.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'This story instantiates the continuity reading of the kernel vatican_ii_magisterial_authority; the rupture and composite-overdetermination readings instantiate structurally different constraints over the same texts. Which reading governs the arrangement being measured?',
    'Not resolvable by data inside any single framework: resolution is a framework-level commitment (acceptance of an authorized interpreter able to decide conciliar/pre-conciliar compatibility). Corpus-level resolution proceeds by comparing the three sibling stories'' authored epsilon, victim sets, and per-seat classifications.',
    'If the rupture reading governed, beneficiary and victim sets invert (the magisterium''s continuity claim becomes the object the arrangement protects rather than the product it yields) and the classification shifts toward pure imposition. If the composite reading governed, enforcement of any single authorized reading becomes indefensible by construction and the coordination half of this story evaporates, leaving only the disciplinary machinery.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Committer structure routed here: one reading of a contested kernel; siblings are separate constraint files, not positions inside this one.').

omega_variable(
    continuity_naturalness_ambiguity,
    'Is ''organic development without rupture'' a discovered property of the tradition that the discipline merely tracks, or a constructed hermeneutic whose enforcement protects identifiable institutional interests?',
    'Independent historical analysis of the conciliar draft history and the fate of the minority interventions, conducted without presupposing the frame''s conclusion, plus counterfactual analysis of what the magisterium would forfeit if the continuity claim failed.',
    'If constructed, the arrangement wears natural-law dress over institutional interest: the reading''s modest authored epsilon reflects the frame''s self-assessment rather than the structure''s operation, and downstream seats should compute materially higher effective burden than the reading-indexed value authored here.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(continuity_naturalness_ambiguity, conceptual, 'Natural-law versus constructed ambiguity in the continuity claim (false-summit question).').

omega_variable(
    dh_syllabus_reconcilability,
    'Can Dignitatis Humanae be honestly reconciled with the anti-modern corpus''s condemnation of religious liberty via the thesis/hypothesis distinction or development criteria, or does the reconciliation require special pleading the surviving draft history does not support?',
    'Magisterially tolerated publication of the full DH draft history (including rejected schemas) under rules permitting a discontinuity finding, benchmarked against uncontested instances of accepted doctrinal development.',
    'If unreconcilable, the frame''s hardest reconciliation fails, the letter-over-spirit rule loses its showcase case, and the enforced-harmony component reads as cover — raising the measured burden well above the reading-indexed value authored in this file.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dh_syllabus_reconcilability, conceptual, 'Whether the reading''s flagship reconciliation (DH versus the Syllabus-era corpus) succeeds on the reading''s own stated criteria.').

omega_variable(
    sc36_latin_mandate_scope,
    'Does Sacrosanctum Concilium §36 bind as permanent universal law preserving Latin primacy, or as a transitional discipline whose continued enforcement is a reading-choice imposed on the text rather than demanded by it?',
    'Canonical-textual analysis of §36''s genre and its parallels within the Constitution, compared against how the same authority treats comparably worded provisions elsewhere in the conciliar corpus.',
    'If transitional, the reading''s binding-mandate claim weakens the letter-over-spirit axiom from inside: the frame would be enforcing its own preference while citing the letter as warrant, shifting burden estimates sharply toward the liturgist seats.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sc36_latin_mandate_scope, empirical, 'Legal-textual status of the SC §36 Latin preservation mandate under this reading.').

omega_variable(
    enforcement_symmetry_between_poles,
    'Is disciplinary attention distributed symmetrically across the two deviation poles (unauthorized progressive implementation and rupture-rejection), or does it concentrate on one pole at a time?',
    'Longitudinal coding of doctrinal congregation actions, liturgical corrections, seminary visitations, and canonical penalties by targeted pole across the interval.',
    'Concentrated enforcement means the modeled burden lands unevenly across payer seats over time: the post-2021 redirection toward the traditionalist pole raises effective burden there above what a symmetric picture implies, and the payer set may require temporal weighting rather than a single static profile.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(enforcement_symmetry_between_poles, empirical, 'Whether the arrangement polices both deviation poles evenly or serially.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vatican_ii_magisterial_authority__continuity_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vatican_ii_continuity_tr_t0, vatican_ii_magisterial_authority__continuity_reading, theater_ratio, 0, 0.14).
narrative_ontology:measurement_basis(vatican_ii_continuity_tr_t0, observed).
narrative_ontology:measurement(vatican_ii_continuity_tr_t10, vatican_ii_magisterial_authority__continuity_reading, theater_ratio, 10, 0.19).
narrative_ontology:measurement_basis(vatican_ii_continuity_tr_t10, observed).
narrative_ontology:measurement(vatican_ii_continuity_tr_t20, vatican_ii_magisterial_authority__continuity_reading, theater_ratio, 20, 0.23).
narrative_ontology:measurement_basis(vatican_ii_continuity_tr_t20, observed).
narrative_ontology:measurement(vatican_ii_continuity_tr_t30, vatican_ii_magisterial_authority__continuity_reading, theater_ratio, 30, 0.26).
narrative_ontology:measurement_basis(vatican_ii_continuity_tr_t30, observed).
narrative_ontology:measurement(vatican_ii_continuity_tr_t40, vatican_ii_magisterial_authority__continuity_reading, theater_ratio, 40, 0.25).
narrative_ontology:measurement_basis(vatican_ii_continuity_tr_t40, observed).
narrative_ontology:measurement(vatican_ii_continuity_tr_t50, vatican_ii_magisterial_authority__continuity_reading, theater_ratio, 50, 0.3).
narrative_ontology:measurement_basis(vatican_ii_continuity_tr_t50, observed).
narrative_ontology:measurement(vatican_ii_continuity_tr_t60, vatican_ii_magisterial_authority__continuity_reading, theater_ratio, 60, 0.31).
narrative_ontology:measurement_basis(vatican_ii_continuity_tr_t60, observed).

% Extraction over time
narrative_ontology:measurement(vatican_ii_continuity_be_t0, vatican_ii_magisterial_authority__continuity_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement_basis(vatican_ii_continuity_be_t0, observed).
narrative_ontology:measurement(vatican_ii_continuity_be_t10, vatican_ii_magisterial_authority__continuity_reading, base_extractiveness, 10, 0.34).
narrative_ontology:measurement_basis(vatican_ii_continuity_be_t10, observed).
narrative_ontology:measurement(vatican_ii_continuity_be_t20, vatican_ii_magisterial_authority__continuity_reading, base_extractiveness, 20, 0.38).
narrative_ontology:measurement_basis(vatican_ii_continuity_be_t20, observed).
narrative_ontology:measurement(vatican_ii_continuity_be_t30, vatican_ii_magisterial_authority__continuity_reading, base_extractiveness, 30, 0.36).
narrative_ontology:measurement_basis(vatican_ii_continuity_be_t30, observed).
narrative_ontology:measurement(vatican_ii_continuity_be_t40, vatican_ii_magisterial_authority__continuity_reading, base_extractiveness, 40, 0.33).
narrative_ontology:measurement_basis(vatican_ii_continuity_be_t40, observed).
narrative_ontology:measurement(vatican_ii_continuity_be_t50, vatican_ii_magisterial_authority__continuity_reading, base_extractiveness, 50, 0.35).
narrative_ontology:measurement_basis(vatican_ii_continuity_be_t50, observed).
narrative_ontology:measurement(vatican_ii_continuity_be_t60, vatican_ii_magisterial_authority__continuity_reading, base_extractiveness, 60, 0.37).
narrative_ontology:measurement_basis(vatican_ii_continuity_be_t60, observed).

% Suppression requirement over time
narrative_ontology:measurement(vatican_ii_continuity_su_t0, vatican_ii_magisterial_authority__continuity_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement_basis(vatican_ii_continuity_su_t0, observed).
narrative_ontology:measurement(vatican_ii_continuity_su_t10, vatican_ii_magisterial_authority__continuity_reading, suppression_requirement, 10, 0.54).
narrative_ontology:measurement_basis(vatican_ii_continuity_su_t10, observed).
narrative_ontology:measurement(vatican_ii_continuity_su_t20, vatican_ii_magisterial_authority__continuity_reading, suppression_requirement, 20, 0.63).
narrative_ontology:measurement_basis(vatican_ii_continuity_su_t20, observed).
narrative_ontology:measurement(vatican_ii_continuity_su_t30, vatican_ii_magisterial_authority__continuity_reading, suppression_requirement, 30, 0.58).
narrative_ontology:measurement_basis(vatican_ii_continuity_su_t30, observed).
narrative_ontology:measurement(vatican_ii_continuity_su_t40, vatican_ii_magisterial_authority__continuity_reading, suppression_requirement, 40, 0.52).
narrative_ontology:measurement_basis(vatican_ii_continuity_su_t40, observed).
narrative_ontology:measurement(vatican_ii_continuity_su_t50, vatican_ii_magisterial_authority__continuity_reading, suppression_requirement, 50, 0.48).
narrative_ontology:measurement_basis(vatican_ii_continuity_su_t50, observed).
narrative_ontology:measurement(vatican_ii_continuity_su_t60, vatican_ii_magisterial_authority__continuity_reading, suppression_requirement, 60, 0.6).
narrative_ontology:measurement_basis(vatican_ii_continuity_su_t60, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vatican_ii_magisterial_authority__continuity_reading, identity_coordination).
narrative_ontology:affects_constraint(vatican_ii_magisterial_authority__continuity_reading, papal_infallibility_authority_chain).
narrative_ontology:affects_constraint(vatican_ii_magisterial_authority__continuity_reading, dignitatis_humanae_reception).
narrative_ontology:affects_constraint(vatican_ii_magisterial_authority__continuity_reading, tridentine_latin_mass_access).

% DUAL FORMULATION NOTE:
% Constraint family (epsilon-invariant decomposition): the kernel vatican_ii_magisterial_authority splits into three stories — this continuity reading (authored epsilon ~0.37, reading-indexed), the rupture reading (expected high epsilon: same enforced arrangement, experienced as institutional cover), and the composite-overdetermination reading (intermediate epsilon, victim set spread across all partisan seats). The continuity reading is presently the upstream frame: it structurally conditions the operating environment of the other two readings through the review and disciplinary machinery it runs, hence edges to the reception stories it regulates (Dignitatis Humanae reception, Latin-mass access) and to the infallibility authority chain it protects. Sibling links proper ride cs_structure.reading_relations; this note records the family split and the expected epsilon deltas across members.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
