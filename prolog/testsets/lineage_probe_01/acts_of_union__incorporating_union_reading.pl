% ============================================================================
% CONSTRAINT STORY: acts_of_union__incorporating_union_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-27
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_acts_of_union__incorporating_union_reading, []).

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
 *   constraint_id: acts_of_union__incorporating_union_reading
 *   human_readable: Acts of Union 1707: Incorporating Union Reading (Unitary Absorption with Protected Exceptions)
 *   domain: legal/constitutional/doctrinal
 *
 * SUMMARY:
 *   The Union of 1707 brought Scotland and England under one Crown and one
 *   Parliament. The legal question is not whether the Union happened, but
 *   what kind of political structure it created. The incorporating_union
 *   reading claims that the Union operated through legal incorporation: one
 *   Parliament absorbed two, Westminster became the unitary sovereign
 *   framework, and Scottish institutions (the Kirk, the Court of Session,
 *   Scots law tradition) were protected by the founding articles but without
 *   structural guarantee of enforceability within a sovereign parliament that
 *   claims it cannot be bound by its predecessors. This reading is neither
 *   the radical entrenchment claim of the fundamental_terms version nor the
 *   purely positivist claim of the ordinary_statute version. It is the
 *   constitutional middle ground: real institutional survival for Scots law
 *   and the Kirk, but structural subordination to Westminster sovereignty.
 *   The constraint exhibits genuine tangled rope structure: Westminster
 *   experiences the Union as coordination of a unitary state with regional
 *   legal exceptions; Scottish legal institutions experience mixed
 *   coordination (they function, they have protected status) and extraction
 *   (they are absorbed into a larger chamber, their legislative voice is
 *   diluted, their entrenchment protection is performative rather than
 *   enforceable). The extractiveness of 0.52 reflects this hybrid: not as
 *   severe as a pure snare (Scotland's law and church actually exist and
 *   operate), but significant (Scottish parliamentary voice is structurally
 *   merged into Westminster's 650-seat chamber, yielding approximately 8%
 *   Scottish representation).
 *
 * KEY AGENTS:
 *   - Westminster Parliament: Primary beneficiary (institutional/arbitrage) — consolidates sovereignty, unitary legislative authority without fundamental structural change to English common law or parliamentary procedure
 *   - Scottish Parliamentary Voice: Primary victim (powerless/trapped) — representation absorbed into unitary chamber; no exit from subordination without dissolving Union
 *   - Scots Law and Kirk: Secondary victim and beneficiary (organized/constrained) — retain institutional function and formal protection, but protection is performative rather than enforceable; benefit from imperial markets, suffer from legislative subordination
 *   - Article XX (Scots Law Protection): Formal-doctrinal actor (institutional/arbitrage) — stated as protecting Scots law 'in all time coming' but enforced only as principle of statutory interpretation, not as constraint on Westminster amendment
 *   - Analytical Observer: Civilizational view (analytical/analytical) — sees the incorporating union as genuine constitutional hybrid with embedded tension between unitary supremacy doctrine and entrenchment language
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(acts_of_union__incorporating_union_reading, 0.52).
domain_priors:suppression_score(acts_of_union__incorporating_union_reading, 0.68).
domain_priors:theater_ratio(acts_of_union__incorporating_union_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(acts_of_union__incorporating_union_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(acts_of_union__incorporating_union_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(acts_of_union__incorporating_union_reading, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(acts_of_union__incorporating_union_reading, tangled_rope).
narrative_ontology:human_readable(acts_of_union__incorporating_union_reading, "Acts of Union 1707: Incorporating Union Reading (Unitary Absorption with Protected Exceptions)").
narrative_ontology:topic_domain(acts_of_union__incorporating_union_reading, "legal/constitutional/doctrinal").

domain_priors:requires_active_enforcement(acts_of_union__incorporating_union_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(acts_of_union__incorporating_union_reading, '9c09d891-89d4-447c-9680-bd216c4ab5c4').
narrative_ontology:cs_kernel_codification('9c09d891-89d4-447c-9680-bd216c4ab5c4', formalized).
narrative_ontology:cs_authority_grounding('9c09d891-89d4-447c-9680-bd216c4ab5c4', extraction).
narrative_ontology:cs_interpretation_layer_present('9c09d891-89d4-447c-9680-bd216c4ab5c4').
narrative_ontology:cs_reading_relation('9c09d891-89d4-447c-9680-bd216c4ab5c4', acts_of_union__fundamental_terms_reading, coexists_with).
narrative_ontology:cs_reading_relation('9c09d891-89d4-447c-9680-bd216c4ab5c4', acts_of_union__ordinary_statute_reading, coexists_with).
narrative_ontology:cs_axiom('9c09d891-89d4-447c-9680-bd216c4ab5c4', foundational, unitary_parliament_supreme).
narrative_ontology:cs_axiom_status(unitary_parliament_supreme, holdable).
narrative_ontology:cs_axiom_grounding('9c09d891-89d4-447c-9680-bd216c4ab5c4', unitary_parliament_supreme, conventional).
narrative_ontology:cs_axiom('9c09d891-89d4-447c-9680-bd216c4ab5c4', foundational, protective_exceptions_performative).
narrative_ontology:cs_axiom_status(protective_exceptions_performative, holdable).
narrative_ontology:cs_axiom_grounding('9c09d891-89d4-447c-9680-bd216c4ab5c4', protective_exceptions_performative, instrumental).
narrative_ontology:cs_reference_frame('9c09d891-89d4-447c-9680-bd216c4ab5c4', unitary_incorporation_with_exceptions).
narrative_ontology:cs_drift_state('9c09d891-89d4-447c-9680-bd216c4ab5c4', contemporary_westminster_sovereignty, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('9c09d891-89d4-447c-9680-bd216c4ab5c4', '').
narrative_ontology:cs_kernel_id(acts_of_union__incorporating_union_reading, acts_of_union).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(acts_of_union__incorporating_union_reading, westminster_parliament).
narrative_ontology:constraint_beneficiary(acts_of_union__incorporating_union_reading, english_institutional_continuity).
narrative_ontology:constraint_victim(acts_of_union__incorporating_union_reading, scottish_parliamentary_voice).
narrative_ontology:constraint_victim(acts_of_union__incorporating_union_reading, scots_law_entrenchment_claim).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SCOTTISH PARLIAMENTARY SOVEREIGNTY (SNARE) — The Scottish Parliament's voice is structurally absorbed into Westminster's unitary chamber. One parliament absorbed two; Scottish representatives hold 45-59 seats in a 650-seat chamber. No exit from this subordination without dissolving the Union itself. Maximum experienced extraction — legislative capacity consolidated upward, no meaningful veto or constitutional protection.
constraint_indexing:constraint_classification(acts_of_union__incorporating_union_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: SCOTS LAW ENTRENCHMENT DOCTRINE (SNARE) — The reading treats Act of Union Article XX ('no alteration of the laws which concern private right, except for evident utility of the people of Scotland') as legally binding in perpetuity, but Westminster's de facto power treats it as a political courtesy subject to revision. The doctrine has no structural protection and no exit mechanism. Entrenchment claim is suppressed by unitary sovereign doctrine.
constraint_indexing:constraint_classification(acts_of_union__incorporating_union_reading, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 3: SCOTTISH LEGAL AND ECCLESIASTICAL INSTITUTIONS (TANGLED ROPE) — Scottish courts, the Kirk, and the legal profession have genuine coordination role: they administer Scots law, ordain ministers, and interpret local doctrine within the Union framework. But they operate as protected exceptions within a unitary structure, not as coordinate sovereigns. They benefit from Union access to imperial markets and English wealth transfers, but at the cost of ultimate legislative subordination. Mixed coordination-extraction: real institutional function + asymmetric power.
constraint_indexing:constraint_classification(acts_of_union__incorporating_union_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: WESTMINSTER PARLIAMENT AND ENGLISH INSTITUTIONAL CONTINUITY (ROPE) — Westminster experiences the Union as coordination: English law, common law tradition, and parliamentary procedure become the unitary framework. Scottish legal pluralism is incorporated as an exception, not a rival. Westminster gains legislative supremacy without fundamental structural change to English institutions. Net coordination beneficiary — extraction flows toward this agent through consolidated sovereignty.
constraint_indexing:constraint_classification(acts_of_union__incorporating_union_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: ARTICLE XX AS LIVING TEXT (PITON) — Formally the Union declares Scots law protected 'in all time coming,' but in practice Westminster has repeatedly amended Scots law through standard statutes (e.g., Scots criminal law, family law revisions). The Article's enforcement is largely theatrical — courts cite it as a principle of interpretation rather than enforceable constraint. The performative gesture (parliament treating it as inviolable) persists despite de facto non-enforcement. Theater ratio high because the commitment is declared but regularly overridden.
constraint_indexing:constraint_classification(acts_of_union__incorporating_union_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — From the analytical position, the 1707 Union instantiates a genuine constitutional hybrid: a unitary parliament with entrenched exceptions. Scotland's institutions (law, church) have legal protection in the founding text, but Westminster's sovereignty doctrine (Parliament cannot bind its successors) suppresses that protection's enforceability. The constraint is stable tangled rope — real coordination (Scotland's legal system works), real extraction (Scottish voice absorbed), active enforcement (courts maintaining the fiction of Article XX protection while Westminster amends law), genuine asymmetry (English law is the unitary standard; Scots law is the protected exception).
constraint_indexing:constraint_classification(acts_of_union__incorporating_union_reading, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(acts_of_union__incorporating_union_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(acts_of_union__incorporating_union_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(acts_of_union__incorporating_union_reading, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(acts_of_union__incorporating_union_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(acts_of_union__incorporating_union_reading, TR),
    TR >= 0.70.

:- end_tests(acts_of_union__incorporating_union_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The constraint extracts Scottish parliamentary voice (absorption into larger chamber, 8% representation) and suppresses Scots law entrenchment claim (no structural remedy for Westminster overriding Article XX). But extractiveness is not as high as a pure snare because Scots law and the Kirk genuinely function, Scotland has institutional continuity, and some reserved matters (education, law, church) remain under Scottish control. The measurement trajectory (0.38 → 0.45 → 0.52) shows increasing extractiveness over 150 years as Westminster gradually amended Scots law (criminal law, family law, commercial law) through standard statutes, revealing that Article XX protection is not enforced — theater rises as Westminster treats the protection as courtesy rather than constraint. Suppression (0.68): High. Significant barriers to Scottish legal voice include: (1) numerical subordination (45-59 Scottish MPs in 650-seat chamber), (2) doctrine of Westminster sovereignty (Parliament cannot be bound by its predecessors), (3) majority rule (English and Welsh MPs can override Scottish preference), (4) centralized government (Westminster controls macroeconomic policy affecting Scotland), (5) media and cultural dominance of English law tradition. Suppression is not absolute (Scotland retains judicial system, legal profession, distinct law education), but structural barriers to changing the Union are severe. Theater ratio (0.58): Moderate-high and rising. The Union's founding emphasizes constitutional protection ('in all time coming,' Kirk security in Article XXV), but enforcement is largely performative. Courts cite Article XX as a principle of construction, but Westminster routinely amends Scots law without triggering remedies. The constraint's theater reflects gap between formal entrenchment language and actual enforcement through unitary sovereignty doctrine.
 *
 * PERSPECTIVAL GAP:
 *   The incorporating_union reading produces stark perspectival gaps across agents. Westminster Parliament perceives rope — coordination of a unitary state with regional legal exceptions, no fundamental breach with English institutional continuity. Scottish Parliament (the historical institution) perceives snare — absorption without exit, legislative voice diluted below functional threshold. Scottish legal institutions perceive tangled rope — they coordinate (they function, they have formal protection), but they extract (Westminster can and does amend Scots law unilaterally; they have no veto). Article XX perceives piton — the formal text is strong ('in all time coming'), but enforcement is theatrical (courts cite it as interpretive principle, Westminster overrides it through normal legislation). The analytical observer perceives the constraint as stable tangled rope — it is precisely what it claims to be: a unitary state with protected but subordinated Scottish exceptions. The gap reveals that the constraint's type depends on structural position: beneficiaries and organized actors see coordination; powerless absorbed agents see extraction; institutional survivors see mixed function-and-subordination.
 *
 * DIRECTIONALITY LOGIC:
 *   The incorporating_union reading specifies beneficiaries and victims as structural facts, not value judgments. Westminster Parliament benefits from the consolidation of sovereignty (d ≈ 0.05–0.15, beneficiary + arbitrage, experiences minimal effective extraction). Scottish parliamentary voice is the primary victim (d ≈ 0.85–0.95, powerless + trapped, experiences maximum extraction). Scottish legal institutions are secondary victims (d ≈ 0.55–0.65, organized + constrained, experience moderate extraction despite institutional survival). Article XX protection is suppressed (d ≈ 0.80, victim of doctrine of Parliament's unbounded sovereignty). The analytical observer (d ≈ 0.72, analytical) perceives the full structure: real institutional hybridity with embedded tension between unitary supremacy and entrenchment language. Directionality derivation does not require override because the structural relationships are unambiguous: one Parliament absorbed two; exceptions were protected but within unitary framework; Westminster is the beneficiary of consolidated sovereignty; Scottish voice is the victim of absorption.
 *
 * MANDATROPHY ANALYSIS:
 *   The incorporating_union reading resolves its own mandatrophy through tangled rope classification: it is neither coordination (rope) nor pure extraction (snare). It is a genuine hybrid. Westminster experiences coordination (unitary framework coordinates English and Scottish law under one sovereignty). Scotland experiences extraction (voice absorbed, representation subordinated). Scots law experiences mixed function-and-suppression (it operates, it is protected in principle, but the protection is performative). The constraint is active enforcement of this asymmetry: courts maintain the fiction that Article XX is binding while Westminster amends Scots law; Westminster treats Scots law protection as principle of statutory construction rather than enforceable constraint. The mandatrophy arises from the core tension in the constraint itself: can entrenchment provisions bind a sovereign parliament that claims succession without constraint? The incorporation reading answers: formally yes (Article XX states protection), structurally no (Westminster sovereignty doctrine overrides entrenchment claims). This is tangled rope, not rope-or-snare. Both coordination and extraction are real.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    entrenchment_enforceability_vs_sovereignty,
    'Can the foundational Union articles entrench legal principles against a sovereign parliament that claims it cannot be bound by its predecessors, or is entrenchment itself a subordination to the unitary sovereign that created the Union?',
    'Constitutional court judgment on the enforceability of Article XX constraints; historical analysis of Westminster''s actual pattern of Scots law amendments; comparison with other constitutional orders (federal systems, entrenchment doctrines in written constitutions); examination of whether Scottish legal principle or Westminster statute law prevails in interpretive disputes',
    'If entrenchment is enforceable: the constraint reclassifies toward rope (genuine coordinate protection). If entrenchment is unenforceable: the constraint remains snare from Scottish perspective (suppression without remedy). The analytical position moves toward acknowledging false entrenchment (piton degradation of the protection doctrine).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(entrenchment_enforceability_vs_sovereignty, conceptual, 'Can Union entrenchment provisions bind a sovereign parliament that claims succession without constraint?').

omega_variable(
    unitary_vs_federal_founding_intent,
    'Did the 1707 negotiators intend a unitary absorption of two parliaments into one (incorporating union), a federal structure with coordinate sovereigns, or an ambiguous hybrid that both parties understood differently?',
    'Historical manuscript analysis of negotiation records (Scottish and English archives); textual comparison of rejected federal proposals vs accepted language; examination of Scottish and English contemporary commentary (parliamentary debates, pamphlets, legal opinions) showing what each side believed they agreed to; analysis of whether Article XX language (''in all time coming'') reflects federal expectation of entrenchment or unitary expectation of legislative courtesy',
    'If intent was federal: fundamental_terms_reading and incorporating_union_reading both claim the same core intent (coordinate sovereignty with entrenchment), making them compete rather than coexist. If intent was unitary: incorporating_union_reading is correct, and fundamental_terms_reading is anachronistic reinterpretation. If intent was genuinely split (Scotland believed federal, England believed unitary): the constraint exhibits structurally embedded deception at its founding — the Union is built on misalignment.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(unitary_vs_federal_founding_intent, empirical, 'Did the 1707 parties intend unitary absorption or federal coordinate sovereignty?').

omega_variable(
    scots_law_as_protected_or_exceptional,
    'Is Scots law protected by the Union as an equal legal system within a multi-jurisdictional realm, or is it an exception tolerated within a unitary common-law framework whose standard is English law?',
    'Analysis of how Westminster courts treat Scots law statutes (as co-equal or subordinate); examination of conflict-of-laws doctrine in cases involving both jurisdictions; historical data on amendment rates for Scots law vs English law (if Scots law is protected, amendment rate should be near zero unless Scotland consents; if Scots law is exceptional, amendment rate should reflect Westminster''s unilateral authority); institutional analysis of whether Scottish legal voice has structural veto on legal changes',
    'If Scots law is protected equal system: the constraint approaches rope from Scottish perspective (genuine coordination of distinct legal frameworks). If Scots law is exceptional subordinate: the constraint remains snare (suppression of Scots law as coordinate authority). The extractiveness value is stable, but the beneficiary/victim framing shifts from ''Westminster absorbs two parliaments'' to ''Scots law is a tolerated exception in the English-common-law default.''',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(scots_law_as_protected_or_exceptional, empirical, 'Is Scots law protected as coordinate system or tolerated as exception?').

omega_variable(
    reading_identity_kernel_version,
    'This constraint is one reading of the acts_of_union kernel. Does the incorporating_union reading remain coherent, or does it foreclose itself through internal tension between ''protected exceptions'' and ''absorbed into unitary parliament''?',
    'Examination of whether the core claim (unitary parliament + protected exceptions for Scots law/Kirk) is internally consistent: can an institution be simultaneously absorbed into a unitary structure AND retain legally protected coordinate status? If the reading claims both, it may be unstable (collapsing toward either fundamental_terms_reading if it emphasizes entrenchment, or ordinary_statute_reading if it emphasizes unitary supremacy). If the reading can maintain the tension, it represents genuine constitutional hybridity (tangled rope).',
    'If incorporating_union reading forecloses itself: it is a transient reading that resolves toward one of its siblings. If it maintains coherence: it represents the genuine constitutional fact of 1707 (unitary structure with subordinated but surviving Scottish institutions). This is a meta-omega documenting the reading''s own stability under scrutiny.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_identity_kernel_version, conceptual, 'Is the incorporating_union reading internally coherent or does it collapse toward a sibling reading?').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(acts_of_union__incorporating_union_reading, 0, 150).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(union_inc_theater_1707, acts_of_union__incorporating_union_reading, theater_ratio, 0, 0.42).
narrative_ontology:measurement(union_inc_theater_1757, acts_of_union__incorporating_union_reading, theater_ratio, 50, 0.55).
narrative_ontology:measurement(union_inc_theater_1857, acts_of_union__incorporating_union_reading, theater_ratio, 150, 0.68).

% Extraction over time
narrative_ontology:measurement(union_inc_extract_1707, acts_of_union__incorporating_union_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(union_inc_extract_1757, acts_of_union__incorporating_union_reading, base_extractiveness, 50, 0.45).
narrative_ontology:measurement(union_inc_extract_1857, acts_of_union__incorporating_union_reading, base_extractiveness, 150, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(union_inc_supp_1707, acts_of_union__incorporating_union_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(union_inc_supp_1757, acts_of_union__incorporating_union_reading, suppression_requirement, 50, 0.62).
narrative_ontology:measurement(union_inc_supp_1857, acts_of_union__incorporating_union_reading, suppression_requirement, 150, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(acts_of_union__incorporating_union_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(acts_of_union__incorporating_union_reading, 0.18).
narrative_ontology:affects_constraint(acts_of_union__incorporating_union_reading, acts_of_union__fundamental_terms_reading).
narrative_ontology:affects_constraint(acts_of_union__incorporating_union_reading, acts_of_union__ordinary_statute_reading).
narrative_ontology:affects_constraint(acts_of_union__incorporating_union_reading, scottish_devolution_1997).
narrative_ontology:affects_constraint(acts_of_union__incorporating_union_reading, westminster_parliamentary_supremacy).

% DUAL FORMULATION NOTE:
% The acts_of_union kernel has three sibling readings as separate constraint stories: fundamental_terms_reading (MacCormick entrenchment), incorporating_union_reading (this story — unitary absorption with protected exceptions), and ordinary_statute_reading (orthodox positivism — statutes all the way down). The three readings coexist in live constitutional dispute. The incorporating_union reading structures Scottish devolution (1997) as a downstream constraint that acknowledges the unitary framework while carving out some executive-legislative autonomy within it. The reading also structures Westminster_parliamentary_supremacy as a competing constraint that claims unbounded sovereign authority inconsistent with entrenchment. All three readings ultimately affect the same underlying institutional reality (what constraints on Westminster's authority are legitimate); they differ on whether entrenchment is enforceable.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
