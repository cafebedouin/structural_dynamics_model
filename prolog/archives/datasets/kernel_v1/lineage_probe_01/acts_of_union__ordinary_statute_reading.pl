% ============================================================================
% CONSTRAINT STORY: acts_of_union__ordinary_statute_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_acts_of_union__ordinary_statute_reading, []).

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
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: acts_of_union__ordinary_statute_reading
 *   human_readable: Acts of Union as Ordinary Statutes (Orthodox Diceyan Reading)
 *   domain: constitutional_law/legal_doctrine
 *
 * SUMMARY:
 *   The ordinary-statute reading is the dominant doctrine of UK
 *   constitutional law: the Acts of Union (1707) that created the United
 *   Kingdom are statutes enacted by Parliament like any other, and therefore
 *   amendable by the Parliament they created. Under this reading,
 *   entrenchment is legally impossible — Dicey's axiom holds that a sovereign
 *   Parliament cannot bind itself. This doctrine suppresses alternative
 *   readings that claim the Union contained fundamental terms (the Kirk 'in
 *   all time coming,' Scots law as a protected exception) that even
 *   Westminster cannot revise. The ordinary-statute reading stabilizes
 *   Westminster supremacy but extracts from Scottish institutional autonomy
 *   by rendering its Union guarantees revocable at pleasure. The doctrine
 *   functions as an institutional accomplishment: it coordinates
 *   parliamentary self-understanding, justifies legislative flexibility, and
 *   establishes hierarchy (Westminster above all local authority). But it
 *   does so by suppressing a structurally available alternative — the
 *   fundamental-terms reading — and denying entrenchment as a legal category.
 *   Extractiveness has risen over 300 years as Westminster has actually
 *   exercised the power the doctrine claims (the Scotland Act 1998 and
 *   subsequent amendments show Westminster willingness to revise devolution),
 *   making the doctrine's claim to describe mere legality increasingly
 *   performative.
 *
 * KEY AGENTS:
 *   - Diceyan Orthodoxy: Primary beneficiary (institutional/arbitrage) — benefits from sovereignty doctrine that justifies Westminster supremacy and legislative unfetteredness
 *   - Scottish Entrenchment Claims: Primary victim (powerless/trapped) — the fundamental-terms doctrine is suppressed; Union protections are rendered revocable
 *   - Scottish Civil Society: Secondary victim (moderate/constrained) — Kirk, Law Society, universities operate under ongoing threat that their Union-guaranteed protections are at Westminster's pleasure
 *   - Westminster Parliament: Primary beneficiary (institutional/arbitrage) — benefits from doctrine that permits amendment of Union terms without Scottish consent
 *   - Scottish Devolved Government: Mixed (organized/constrained) — benefits from devolution coordination but faces asymmetry where Westminster can unilaterally amend devolved arrangements
 *   - UK Courts: Institutional actor with constrained exit — formally defer to Diceyan sovereignty but apply interpretive principles (purposive construction) that sometimes diverge from pure statutory logic
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(acts_of_union__ordinary_statute_reading, 0.58).
domain_priors:suppression_score(acts_of_union__ordinary_statute_reading, 0.72).
domain_priors:theater_ratio(acts_of_union__ordinary_statute_reading, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(acts_of_union__ordinary_statute_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(acts_of_union__ordinary_statute_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(acts_of_union__ordinary_statute_reading, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(acts_of_union__ordinary_statute_reading, snare).
narrative_ontology:human_readable(acts_of_union__ordinary_statute_reading, "Acts of Union as Ordinary Statutes (Orthodox Diceyan Reading)").
narrative_ontology:topic_domain(acts_of_union__ordinary_statute_reading, "constitutional_law/legal_doctrine").

domain_priors:requires_active_enforcement(acts_of_union__ordinary_statute_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(acts_of_union__ordinary_statute_reading, 'd9311a3a-36f8-4caf-9523-e00949dd6098').
narrative_ontology:cs_kernel_codification('d9311a3a-36f8-4caf-9523-e00949dd6098', fixed_text).
narrative_ontology:cs_authority_grounding('d9311a3a-36f8-4caf-9523-e00949dd6098', lineage).
narrative_ontology:cs_interpretation_layer_present('d9311a3a-36f8-4caf-9523-e00949dd6098').
narrative_ontology:cs_reading_relation('d9311a3a-36f8-4caf-9523-e00949dd6098', acts_of_union__fundamental_terms_reading, coexists_with).
narrative_ontology:cs_reading_relation('d9311a3a-36f8-4caf-9523-e00949dd6098', acts_of_union__incorporating_union_reading, coexists_with).
narrative_ontology:cs_axiom('d9311a3a-36f8-4caf-9523-e00949dd6098', foundational, parliament_cannot_bind_itself).
narrative_ontology:cs_axiom_status(parliament_cannot_bind_itself, holdable).
narrative_ontology:cs_axiom_grounding('d9311a3a-36f8-4caf-9523-e00949dd6098', parliament_cannot_bind_itself, deontological).
narrative_ontology:cs_axiom('d9311a3a-36f8-4caf-9523-e00949dd6098', foundational, union_acts_are_ordinary_statutes).
narrative_ontology:cs_axiom_status(union_acts_are_ordinary_statutes, holdable).
narrative_ontology:cs_axiom_grounding('d9311a3a-36f8-4caf-9523-e00949dd6098', union_acts_are_ordinary_statutes, conventional).
narrative_ontology:cs_reference_frame('d9311a3a-36f8-4caf-9523-e00949dd6098', westminster_sovereign_supremacy).
narrative_ontology:cs_drift_state('d9311a3a-36f8-4caf-9523-e00949dd6098', contemporary_devolution_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('d9311a3a-36f8-4caf-9523-e00949dd6098', '').
narrative_ontology:cs_kernel_id(acts_of_union__ordinary_statute_reading, acts_of_union).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(acts_of_union__ordinary_statute_reading, westminster_parliament).
narrative_ontology:constraint_beneficiary(acts_of_union__ordinary_statute_reading, diceyan_orthodoxy).
narrative_ontology:constraint_beneficiary(acts_of_union__ordinary_statute_reading, english_legal_tradition).
narrative_ontology:constraint_victim(acts_of_union__ordinary_statute_reading, scottish_entrenchment_claims).
narrative_ontology:constraint_victim(acts_of_union__ordinary_statute_reading, fundamental_terms_doctrine).
narrative_ontology:constraint_victim(acts_of_union__ordinary_statute_reading, scottish_institutional_autonomy).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SCOTTISH ENTRENCHMENT DOCTRINE (SNARE) — Trapped by Westminster's sovereign supremacy doctrine. The Kirk and Scots law are framed as statutes like any other, amendable at Westminster pleasure. Entrenchment claims (the core of fundamental-terms reading) cannot escape the ordinary-statute frame; they are suppressed, not acknowledged as structurally distinct. No legal exit from Westminster sovereignty.
constraint_indexing:constraint_classification(acts_of_union__ordinary_statute_reading, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: SCOTTISH CIVIL SOCIETY (SNARE) — Constrained by the doctrine's force in Westminster legislative practice and the courts' deference to sovereignty. The Kirk, Law Society, Universities operate under ongoing threat that their Union-guaranteed protections are revocable at Westminster's sovereign pleasure. Material barriers to exit are structural (relocation to independent state, regime change); legal barrier is doctrinal supremacy.
constraint_indexing:constraint_classification(acts_of_union__ordinary_statute_reading, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: WESTMINSTER PARLIAMENT (ROPE) — Experiences the ordinary-statute reading as pure coordination: Parliament created itself by the Union Acts and can amend anything it created. The doctrine coordinates parliamentary supremacy and legislative flexibility. Benefits from sovereignty unfettered by past commitments. Sees Scottish entrenchment as legally incoherent (you cannot bind a sovereign). Net beneficiary with arbitrage (can exit Union entirely or amend unilaterally).
constraint_indexing:constraint_classification(acts_of_union__ordinary_statute_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: DICEYAN ORTHODOXY (ROPE) — Pure coordination mechanism for Westminster constitutionalism. Parliamentary sovereignty (unfettered, omnipotent, indissoluble) is the foundational principle of UK law. The ordinary-statute reading coordinates three doctrinal commitments: (1) Parliament cannot bind itself, (2) the Union Acts are not special, (3) entrenchment is legally impossible. This doctrine benefits from the unity of Parliament and the clarity of the sovereignty principle. Sees itself as describing law-as-it-is, not as extracting from Scotland.
constraint_indexing:constraint_classification(acts_of_union__ordinary_statute_reading, rope,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(universal))).

% PERSPECTIVE 5: SCOTTISH DEVOLVED GOVERNMENT (TANGLED ROPE) — Organized actor (Scottish Parliament, Scottish Executive) with real but constrained exit options. The ordinary-statute reading creates asymmetry: Westminster can unilaterally amend devolution arrangements (as 2022 Scotland Act amendments show), but Scotland cannot unilaterally amend Union protections. Yet devolution itself demonstrates a working coordination mechanism — power sharing, institutional separation. The snare and rope coexist: genuine coordination on devolved matters, genuine extraction on fundamental Union terms.
constraint_indexing:constraint_classification(acts_of_union__ordinary_statute_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: DICEY'S THEORETICAL FRAMEWORK (PITON) — The ordinary-statute reading is substantially performative at this civilizational scale. Dicey's theory of parliamentary sovereignty (no Parliament can bind a future Parliament; entrenchment is legally impossible) is treated as natural law. Yet the theory persists largely through institutional inertia — law schools teach it, courts defer to it — despite structural disconfirmation (EU law, devolution arrangements, judicial review). Theater_ratio is high because the doctrine claims descriptive inevitability (what law IS) while operating as prescriptive doctrine (what Parliament should do). As a theoretical artifact, it has low functional verification but high performative weight.
constraint_indexing:constraint_classification(acts_of_union__ordinary_statute_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(universal))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / LOGICAL NECESSITY VIEW (MOUNTAIN) — From a formal-logical perspective, the ordinary-statute reading claims to describe a logical necessity: a sovereign Parliament cannot bind itself — it is constitutive of sovereignty that it can always revisit its own foundations. This perspective sees the doctrine as capturing something immutable about the structure of legislative power itself. However, this mountain is a false summit: the doctrine naturalizes what are actually contingent historical and institutional choices about how UK constitutionalism operates.
constraint_indexing:constraint_classification(acts_of_union__ordinary_statute_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(acts_of_union__ordinary_statute_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(acts_of_union__ordinary_statute_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(acts_of_union__ordinary_statute_reading, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(acts_of_union__ordinary_statute_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(acts_of_union__ordinary_statute_reading, TR),
    TR >= 0.70.

:- end_tests(acts_of_union__ordinary_statute_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The ordinary-statute reading enables Westminster to treat Union guarantees (particularly the Kirk and Scots law) as revocable, benefiting Westminster's legislative flexibility at the cost of Scottish institutional certainty. The extraction is not total because Scotland retains some institutional autonomy through devolution (a genuine coordination mechanism), but extractiveness has risen over 300 years as Westminster actually exercises amendment power (Scotland Act amendments, Sewel Convention erosion). The value reflects that the doctrine operationalizes an asymmetry: Westminster can revise the bargain unilaterally, Scotland cannot. Suppression (0.72): High. The ordinary-statute reading suppresses the fundamental-terms doctrine as a legal category — it denies entrenchment as conceptually possible in UK law. Courts do not engage fundamental-terms arguments on their merits; they dismiss entrenchment as 'not law.' This is doctrinal suppression (structural, not just practical). Media and public debate also reflects suppression — few Scots lawyers teach entrenchment as a live legal option despite its textual basis ('in all time coming'). Theater ratio (0.65): Moderate-high. The doctrine claims descriptive necessity (describing what law IS) but operates as prescriptive doctrine (governing what Parliament SHOULD do). Diceyan theory of sovereignty is taught in law schools as discovered truth, not as contested doctrine. Yet the theory is persistently contradicted by actual practice: EU law, devolution, human rights law all represent constraints on parliamentary supremacy that the pure doctrine cannot accommodate. The doctrine survives these disconfirmations through interpretive flexibility and institutional inertia rather than through empirical vindication.
 *
 * PERSPECTIVAL GAP:
 *   The ordinary-statute reading classifies as rope from Westminster and Diceyan orthodoxy's perspective (pure coordination, no extraction experienced) but as snare from Scottish entrenchment claims and civil society perspectives (trapped, no exit from revocability). The piton perspective sees the doctrine as degraded — maintained by inertia despite disconfirmation. The Scottish devolved government perspective captures the mixed reality: genuine coordination on devolved matters (tangled rope), but asymmetric extraction on Union fundamentals. The analytical observer's mountain is a false summit: the doctrine is defended as logical necessity but is actually a historical accomplishment that could be otherwise. The perspectival gap reveals that the constraint's function (coordinating parliamentary supremacy) and its extraction (rendering Scottish protections revocable) are inseparable from this reading.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's experienced extractiveness is computed from its structural position relative to the constraint. Westminster (institutional/arbitrage) has low d — it benefits from the doctrine, so extraction runs toward this agent. Scottish entrenchment claims (powerless/trapped) have high d — they are the target of suppression. Scottish civil society (moderate/constrained) has moderate-high d — they are partly protected by devolution (coordination function) but face the underlying extraction (Union protections revocable at pleasure). The piton perspective (Dicey's theory viewed at civilizational scope) has medium d because the doctrine is maintained despite disconfirmation — it is performative rather than functionally verified. The mountain perspective (logical necessity view) would require high accessibility_collapse (no alternative exists) and low resistance (cannot be overcome), but actual constitutional practice shows both alternatives exist (fundamental-terms reading is coherent; resistance to Diceyan logic is substantial in Scottish jurisprudence). The false summit is that the mountain perspective naturalizes what are historically contingent choices about how UK constitutionalism operates.
 *
 * MANDATROPHY ANALYSIS:
 *   KERNEL READING: This constraint resolves the mandatrophy by showing that the ordinary-statute reading is one of three structurally distinct interpretations of the Union kernel. The mandatrophy is not 'what does UK constitutional law actually say?' but 'which reading governs the Union?'. The ordinary-statute reading is the dominant doctrine, but it is not logically necessary — the fundamental-terms and incorporating-union readings are coherent alternatives grounded in the same text. The extractiveness (0.58) reflects that this reading suppresses alternatives and operationalizes asymmetry. If the fundamental-terms reading were operative, extractiveness would be near zero (entrenchment is irrevocable, no extraction possible). If the incorporating-union reading were operative, extractiveness would be higher for Scottish institutions (absorbed into unitary state, no special protection possible). The mandatrophy is resolved by recognizing that classification depends on which reading of the kernel has interpretive authority — and that authority is contested.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    entrenchment_logical_possibility,
    'Is entrenchment of the Union terms logically impossible (as Dicean sovereignty claims) or merely politically difficult and doctrinally suppressed?',
    'Comparative constitutional analysis: examine entrenchment mechanisms in other mature democracies (Germany, Canada, Australia) and their legal stability. Ask whether Diceyan logic applies equally to all nations or is specific to Westminster history. Analyze whether ''Parliament cannot bind itself'' is a logical truth or a doctrine about institutional continuity.',
    'If entrenchment is logically possible: ordinary-statute reading suppresses an available alternative and becomes snare. If entrenchment is logically impossible: the reading describes natural law and is mountain. If it is contingent on political will and doctrine: the reading is piton (performative maintenance of a degraded doctrine).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(entrenchment_logical_possibility, conceptual, 'Whether entrenchment is logically impossible or doctrinally suppressed').

omega_variable(
    diceyan_doctrine_vs_judicial_practice,
    'Do UK courts actually treat the Union Acts as ordinary statutes, or do they apply interpretive principles that acknowledge entrenchment-like constraints (e.g., purposive interpretation, protection of the Kirk, respect for Scots law)?',
    'Systematic analysis of case law since 1707: MacCormick v Lord Advocate (1953), Claim of Right for Scotland (1989), Miller/Cherry (2019), Scottish independence referendum reference (2023). Map judicial reasoning against the doctrine''s claim that Union protections are revocable at pleasure. Identify where courts diverge from pure Diceyan logic.',
    'If courts treat Union Acts as special: practice contradicts the ordinary-statute reading and it becomes piton (doctrine unsupported by judicial behavior). If courts treat Union Acts as ordinary: the reading is validated and snare classification is confirmed. If courts treat them as mixed: neither reading captures actual doctrine.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(diceyan_doctrine_vs_judicial_practice, empirical, 'Whether judicial practice diverges from Diceyan statutory treatment').

omega_variable(
    sovereignty_doctrine_stability,
    'Is Diceyan sovereignty doctrine stable under constitutional strain (devolution, human rights law, EU membership, Scottish independence movement) or is it eroding as Westminster accommodates exceptions?',
    'Historical tracking of doctrine''s authority: analysis of parliamentary rhetoric, judicial reasoning, and legal scholarship over 50+ years. Measurement of doctrine''s descriptive accuracy (does it predict outcomes?) vs prescriptive force (do institutions still defer to it?). Identify moments where Westminster violated the doctrine (e.g., restricting its own power via devolution) without renouncing the doctrine itself.',
    'If eroding: the ordinary-statute reading is piton (maintained by inertia, not function). If stable: it remains snare or rope (depending on beneficiary perspective). If transformed: a new doctrine (incorporating-union or fundamental-terms) has become operative and the ordinary-statute reading is foreclosed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sovereignty_doctrine_stability, empirical, 'Long-term stability of Diceyan sovereignty doctrine under constitutional stress').

omega_variable(
    kernel_contest_reading_choice,
    'Is this (ordinary_statute_reading) the most cogent interpretation of the Union Acts, or is it one reading among structurally equivalent alternatives (fundamental_terms_reading, incorporating_union_reading)?',
    'Text-based analysis of the 1707 Union Acts themselves; historical context of parliamentary intention; analysis of whether the text supports entrenchment language (''in all time coming'' re Kirk and law), incorporation doctrine (absorption vs federation), or pure statutory amendability. Doctrinal coherence: which reading best explains the courts'' actual treatment of Union protections?',
    'If ordinary-statute reading is uniquely supported by the text: alternative readings are foreclosed and this reading is foundational. If the text supports multiple readings: all three readings coexist (the kernel truly is contested). If the text supports entrenchment more clearly: this reading suppresses a more coherent interpretation and becomes a false summit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_contest_reading_choice, conceptual, 'Whether ordinary-statute reading is uniquely justified or one among alternatives').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(acts_of_union__ordinary_statute_reading, 0, 200).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(union_ord_tr_t0, acts_of_union__ordinary_statute_reading, theater_ratio, 0, 0.42).
narrative_ontology:measurement(union_ord_tr_t100, acts_of_union__ordinary_statute_reading, theater_ratio, 100, 0.58).
narrative_ontology:measurement(union_ord_tr_t200, acts_of_union__ordinary_statute_reading, theater_ratio, 200, 0.65).

% Extraction over time
narrative_ontology:measurement(union_ord_be_t0, acts_of_union__ordinary_statute_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(union_ord_be_t100, acts_of_union__ordinary_statute_reading, base_extractiveness, 100, 0.48).
narrative_ontology:measurement(union_ord_be_t200, acts_of_union__ordinary_statute_reading, base_extractiveness, 200, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(acts_of_union__ordinary_statute_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(acts_of_union__ordinary_statute_reading, acts_of_union__fundamental_terms_reading).
narrative_ontology:affects_constraint(acts_of_union__ordinary_statute_reading, acts_of_union__incorporating_union_reading).
narrative_ontology:affects_constraint(acts_of_union__ordinary_statute_reading, scottish_devolution_asymmetry).
narrative_ontology:affects_constraint(acts_of_union__ordinary_statute_reading, sewel_convention_erosion).

% DUAL FORMULATION NOTE:
% This story describes one reading of the acts_of_union kernel. The fundamental_terms_reading and incorporating_union_reading are sibling constraints describing alternative interpretations of the same constitutional foundation. Each reading has different extractiveness, suppression, and beneficiary/victim profiles. They are not the same constraint viewed from different angles — they are structurally distinct claims about what the Union Acts mean. The network links them as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(acts_of_union__ordinary_statute_reading, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
