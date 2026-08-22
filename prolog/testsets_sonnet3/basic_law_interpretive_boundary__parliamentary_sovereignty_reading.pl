% ============================================================================
% CONSTRAINT STORY: basic_law_interpretive_boundary__parliamentary_sovereignty_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_basic_law_interpretive_boundary__parliamentary_sovereignty_reading, []).

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
 *   constraint_id: basic_law_interpretive_boundary__parliamentary_sovereignty_reading
 *   human_readable: Knesset Parliamentary Sovereignty Reading of Basic Law Authority
 *   domain: constitutional_law/comparative_constitutionalism
 *
 * SUMMARY:
 *   This constraint authors the parliamentary sovereignty reading of the
 *   contested kernel governing the Basic Law interpretive boundary in
 *   Israel's constitutional order. Under this reading, the Knesset — as the
 *   elected sovereign body — retains ultimate authority to interpret, amend,
 *   and where necessary override judicial constraints on Basic Laws by
 *   ordinary majority vote. Judicial review under this reading functions in
 *   an advisory capacity: the Supreme Court's constitutional pronouncements
 *   carry persuasive but not binding force against a determined legislative
 *   majority. This reading gained practical salience with the 2023 Basic Law:
 *   The Judiciary (Amendment) curtailing reasonableness review of
 *   governmental decisions, passed amid the broader 2023 judicial reform
 *   controversy. The sovereignty reading is NOT the only defensible reading
 *   of the kernel — judicial_supremacy_reading and
 *   balanced_contestation_reading are separately authored sibling constraints
 *   instantiating structurally distinct claims about the same underlying
 *   textual and institutional ambiguity. This story does not describe or
 *   average over those siblings; it authors only the sovereignty reading's
 *   own epsilon, stakeholders, and classification.
 *
 * KEY AGENTS:
 *   - knesset_majority_coalition: Primary beneficiary (institutional/arbitrage) — sets and administers the sovereignty reading through ordinary legislation
 *   - sitting_executive_government: Secondary beneficiary (institutional/arbitrage) — governs free of binding judicial constraint on reasonableness or proportionality
 *   - supreme_court_of_israel: Structurally demoted actor (institutional/constrained) — retains interpretive voice but no binding override power under this reading
 *   - minority_political_blocs: Primary target (organized/constrained) — lose the judicial backstop against majoritarian legislative action
 *   - individual_rights_claimants: Primary target (powerless/trapped) — lose an independent forum capable of binding invalidation of rights-infringing statutes
 *   - international_treaty_bodies: Secondary actor (institutional/analytical) — the one domain where this reading concedes non-majoritarian constraint
 *   - constitutional_law_scholars: Analytical observer — documents the sovereignty/supremacy/balanced contest across readings
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, 0.28).
domain_priors:suppression_score(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, 0.42).
domain_priors:theater_ratio(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, rope).
narrative_ontology:human_readable(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, "Knesset Parliamentary Sovereignty Reading of Basic Law Authority").
narrative_ontology:topic_domain(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, "constitutional_law/comparative_constitutionalism").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, 'f027ae6a-d6a4-40b0-ae71-c49afda232ab').
narrative_ontology:cs_kernel_codification('f027ae6a-d6a4-40b0-ae71-c49afda232ab', distributed).
narrative_ontology:cs_authority_grounding('f027ae6a-d6a4-40b0-ae71-c49afda232ab', practice).
narrative_ontology:cs_interpretation_layer_present('f027ae6a-d6a4-40b0-ae71-c49afda232ab').
narrative_ontology:cs_reading_relation('f027ae6a-d6a4-40b0-ae71-c49afda232ab', basic_law_interpretive_boundary__judicial_supremacy_reading, forecloses).
narrative_ontology:cs_reading_relation('f027ae6a-d6a4-40b0-ae71-c49afda232ab', basic_law_interpretive_boundary__balanced_contestation_reading, influences).
narrative_ontology:cs_axiom('f027ae6a-d6a4-40b0-ae71-c49afda232ab', foundational, electoral_mandate_is_final_interpretive_authority).
narrative_ontology:cs_axiom_status(electoral_mandate_is_final_interpretive_authority, holdable).
narrative_ontology:cs_axiom_grounding('f027ae6a-d6a4-40b0-ae71-c49afda232ab', electoral_mandate_is_final_interpretive_authority, conventional).
narrative_ontology:cs_axiom('f027ae6a-d6a4-40b0-ae71-c49afda232ab', foundational, judicial_review_is_advisory_not_binding).
narrative_ontology:cs_axiom_status(judicial_review_is_advisory_not_binding, holdable).
narrative_ontology:cs_axiom_grounding('f027ae6a-d6a4-40b0-ae71-c49afda232ab', judicial_review_is_advisory_not_binding, conventional).
narrative_ontology:cs_axiom('f027ae6a-d6a4-40b0-ae71-c49afda232ab', secondary, international_treaty_obligation_binds_regardless_of_majority).
narrative_ontology:cs_axiom_status(international_treaty_obligation_binds_regardless_of_majority, holdable).
narrative_ontology:cs_axiom_grounding('f027ae6a-d6a4-40b0-ae71-c49afda232ab', international_treaty_obligation_binds_regardless_of_majority, instrumental).
narrative_ontology:cs_reference_frame('f027ae6a-d6a4-40b0-ae71-c49afda232ab', unwritten_constitutional_order_1958_basic_laws).
narrative_ontology:cs_drift_state('f027ae6a-d6a4-40b0-ae71-c49afda232ab', post_2023_judicial_reform_era, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('f027ae6a-d6a4-40b0-ae71-c49afda232ab', '').
narrative_ontology:cs_kernel_id(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, basic_law_interpretive_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, knesset_majority_coalition).
narrative_ontology:constraint_beneficiary(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, sitting_executive_government).
narrative_ontology:constraint_victim(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, minority_political_blocs).
narrative_ontology:constraint_victim(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, individual_rights_claimants).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds the votes to pass, amend, or reinterpret Basic Laws by ordinary majority, and under this reading treats judicial pronouncements on the constitutionality of its legislation as persuasive but non-binding. Can rewrite the very rules that would otherwise constrain it, including rules governing judicial review itself, as demonstrated by the 2023 reasonableness-review amendment.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, knesset_majority_coalition, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, knesset_majority_coalition, beneficiary).

% Governs without a binding proportionality or reasonableness check on many administrative and appointment decisions, since the reading treats judicial review of governmental action as advisory rather than authoritative once the legislature has acted to narrow that review's scope.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, sitting_executive_government, beneficiary,
    institutional, biographical, arbitrage, national).

% Continues to hear constitutional challenges and issue rulings on the compatibility of legislation with Basic Laws, but under this reading its invalidations carry persuasive rather than binding force against a determined Knesset majority. It retains institutional standing and can still shape public and legal discourse, but has no enforcement mechanism to compel Knesset compliance if the majority chooses to override or legislate around a ruling.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, supreme_court_of_israel, agenda_setter,
    institutional, generational, constrained, national).

% Retain electoral organizing capacity and parliamentary voice but, under this reading, lose the judiciary as a binding backstop against majoritarian legislative action that disadvantages their constituencies. Their remedy is confined to electoral politics and coalition-building rather than judicial appeal to a binding external check.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, minority_political_blocs, payer,
    organized, biographical, constrained, national).

% Individuals or small groups whose rights are affected by specific legislation have, under this reading, no forum capable of binding invalidation of a rights-infringing statute if the same majority that passed it also controls the scope of judicial review. Emigration is the only meaningful exit, and even that does not remedy an already-suffered infringement.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, individual_rights_claimants, payer,
    powerless, biographical, trapped, national).

% Bodies overseeing Israel's international treaty commitments are the one domain this reading concedes as a genuine external constraint on the Knesset — but they have no seat inside Israeli domestic constitutional practice and can only exert pressure through diplomatic, trade, or reputational channels rather than binding domestic legal force.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, international_treaty_bodies, excluded,
    institutional, generational, analytical, global).

% Document and analyze the ongoing contest among the three readings of the Basic Law interpretive boundary, comparing Israel's unwritten-constitution trajectory to other majoritarian and supremacy-model systems, without a direct stake in which reading prevails domestically.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, constitutional_law_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, knesset_majority_coalition).
narrative_ontology:fixing_cost_class(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a single, clear final decision-maker (the elected Knesset majority) for resolving disputes over the meaning and scope of Basic Laws, avoiding the paralysis that could result from an unelected judiciary and an elected legislature both claiming final interpretive authority over an unwritten constitutional order.
% TRANSFER_FUNCTION: Moves the practical capacity to constrain majoritarian legislative and executive action away from an independent judicial forum and concentrates it in the same electoral majority whose action might otherwise be constrained — a transfer of checking-power from courts and, indirectly, from minorities and individual rights-claimants, to the sitting parliamentary majority.
% ABSENT_VOICES: Individual rights-claimants whose specific grievances would previously have found a forum in binding judicial review are not organized as a coalition and have no seat in the legislative process that redefines the scope of that review; their objection — that a majority should not be the sole judge of the limits on its own power — is voiced by opposition politicians and legal scholars but is not itself a party to the Knesset's ordinary lawmaking process.
% DISAPPEARANCE_RATIONALE: If the sovereignty reading were abandoned overnight in favor of binding judicial supremacy, the Knesset would lose the ability to legislate around adverse constitutional rulings, the 2023-era amendments narrowing reasonableness review would likely be reversed or invalidated, and the practical balance of power between the legislature, executive, and judiciary in Israel would shift substantially toward the courts — a real institutional rearrangement, not a cosmetic one.
% FOUNDING_PROBLEM: Israel adopted Basic Laws incrementally from 1958 onward without ever ratifying a single formal constitution, leaving unresolved which institution — the elected Knesset or the appointed judiciary — holds final authority when a Basic Law's meaning is contested or when ordinary legislation appears to conflict with one.
% FOUNDING_PROBLEM_CORROBORATION: Comparative constitutional law scholars outside the Knesset coalition and outside the Supreme Court itself (writing in international law journals and comparative constitutionalism literature) attest that the institutional-authority question remains genuinely unresolved in Israeli law, distinguishing it from settled constitutional democracies with a ratified founding document and explicit judicial review clause; this corroboration comes from a source with no direct stake in which domestic reading prevails.
narrative_ontology:disappearance_verdict(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, world_rearranges).
narrative_ontology:founding_problem_status(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, 'none', 1).
narrative_ontology:epsilon_provenance(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, 0.28, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(basic_law_interpretive_boundary__parliamentary_sovereignty_reading_tests).
:- end_tests(basic_law_interpretive_boundary__parliamentary_sovereignty_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored low-moderate (0.28 at interval end) because, BY THIS READING'S OWN LIGHTS, the sovereignty arrangement is not extraction at all for ordinary majoritarian policy — it is the correct operation of democratic accountability. The nonzero value reflects the standing arrangement's actual operation as seen even sympathetically: a majority that can rewrite the rules governing its own review does extract some value from minorities and rights-claimants who lose an independent check, even under the reading's own framing that this is legitimate. Suppression is moderate (0.42) and rising modestly across the measured interval, tracking the 2023-era amendments that actively narrowed the scope of reasonableness review — this is a real, not merely rhetorical, curtailment of an existing check. Theater ratio stays low (0.20) because the arrangement is not performative; the Knesset's assertion of interpretive authority is functionally real, not a hollow ritual over a defunct process. Resistance is authored moderately high (0.55) because the reading is fiercely contested in practice — mass protests, cross-institutional pushback from the judiciary, and international commentary all constitute genuine resistance, which is precisely why this cannot be scored as mountain-like despite the reading's own claim to naturalness. Accessibility collapse is moderate (0.35), reflecting that alternative institutional arrangements (the sibling readings) remain live, contested, and actively argued in the same polity — alternatives have not collapsed the way they would under a genuine natural-law constraint.
 *
 * PERSPECTIVAL GAP:
 *   The engine should compute a real seat divergence here: from the knesset_majority_coalition and sitting_executive_government seats (institutional power, arbitrage exit — they can rewrite the very rule being contested), the arrangement should compute near mountain-adjacent or rope territory — a legitimate expression of democratic sovereignty they built and can always reconfigure. From the individual_rights_claimants seat (powerless, trapped — no alternative forum, no exit from the jurisdiction), the same structural arrangement should compute as substantially more extractive, because the loss of a binding external check removes their only structural counterweight to majoritarian legislative action. This divergence is exactly the kind of seat-relative computation the framework is built to surface, and it should not be reconciled away — the sovereignty reading is honestly authored as low-epsilon rope from its own vantage while the metrics simultaneously encode the real cost borne by minority and rights-claimant seats.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (knesset_majority_coalition, sitting_executive_government) hold institutional power with arbitrage-grade exit from constraint — they are the body that writes and can rewrite the rule, so directionality should sit near the full-beneficiary end. Victims (minority_political_blocs, individual_rights_claimants) are declared because the sovereignty reading's structural effect, even on its own terms, is to remove a binding external check that previously constrained majoritarian action against them; minority_political_blocs retain organized power and constrained exit (they can still contest electorally and mobilize), while individual_rights_claimants are powerless and trapped (no electoral remedy is available to someone whose rights are infringed by a law the majority that infringed them also controls). The supreme_court_of_israel is not declared a victim in the beneficiary/victim sense — it does not bear a cost, it loses a function — so it is captured instead as an agenda_setter with a demoted role, reflecting the sovereignty reading's own framing that judicial pronouncements remain persuasive but non-binding.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem this reading answers — establishing which institution has final interpretive word over an unwritten, incrementally assembled constitutional order — remains genuinely live: Israel still lacks a single ratified constitution, and the Basic Laws are still passed and amended by ordinary majority under the same procedural rules used for regular legislation. This reading cannot be classified as mandatrophic (a mandate that has outlived its function) because the underlying institutional ambiguity it resolves has not been resolved by any other mechanism — no constitutional convention, no supermajority entrenchment requirement, no binding referendum process has settled the question independently. The sovereignty reading is a live contest position, not a residual fossil.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_disagreement_locus,
    'Is this constraint one reading of a contested kernel (the Basic Law interpretive boundary), or the settled structure of Israeli constitutional order?',
    'Compare against the sibling readings (judicial_supremacy_reading, balanced_contestation_reading), each authored as separate constraints with their own epsilon and stakeholder structure; observe whether the Supreme Court''s own jurisprudence (post-2023 reasonableness ruling era) treats the sovereignty premise as live, dead, or contested.',
    'If the sovereignty reading is the operative one, judicial review functions as advisory-only and epsilon for majoritarian legislation stays near-zero except where international treaty obligations bind. If a sibling reading prevails structurally, this constraint''s own metrics would need re-authoring as a different constraint entirely, not a reinterpretation of this one.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_disagreement_locus, conceptual, 'Where the three sibling readings of the Basic Law interpretive boundary actually disagree, and which reading currently governs practice.').

omega_variable(
    unwritten_constitution_ambiguity,
    'Because Israel has no single formal constitution — only a stack of Basic Laws passed by ordinary Knesset majority and layered with a court-invented ''constitutional revolution'' doctrine (1995 onward) — is the sovereignty reading recovering an original design feature, or is it a majority coalition retroactively asserting a naturalized reading of an ambiguous kernel to insulate its own legislative program from review?',
    'Trace the drafting history of the Basic Laws (1958-1992) for explicit textual commitment to either judicial supremacy or parliamentary sovereignty; examine whether the 2023 Basic Law amendments limiting reasonableness review were passed with the specific intent of foreclosing judicial constraint on the same governing coalition''s other legislation.',
    'If the sovereignty reading is a genuine long-standing design feature, near-zero epsilon is warranted for ordinary legislation. If it is a coalition-specific naturalization deployed to immunize contemporaneous legislative acts from review, epsilon for the enabling constitutional theory itself (distinct from any specific law) is substantially higher — this is a false-summit-adjacent concern even though beneficiaries are declared and FSM is not separately triggered for a rope claim.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(unwritten_constitution_ambiguity, conceptual, 'Whether parliamentary sovereignty is original constitutional design or coalition-timed reading of an ambiguous, unwritten kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(basi_tr_t0, basic_law_interpretive_boundary__parliamentary_sovereignty_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(basi_tr_t5, basic_law_interpretive_boundary__parliamentary_sovereignty_reading, theater_ratio, 5, 0.14).
narrative_ontology:measurement(basi_tr_t10, basic_law_interpretive_boundary__parliamentary_sovereignty_reading, theater_ratio, 10, 0.17).
narrative_ontology:measurement(basi_tr_t15, basic_law_interpretive_boundary__parliamentary_sovereignty_reading, theater_ratio, 15, 0.19).
narrative_ontology:measurement(basi_tr_t20, basic_law_interpretive_boundary__parliamentary_sovereignty_reading, theater_ratio, 20, 0.2).

% Extraction over time
narrative_ontology:measurement(basi_be_t0, basic_law_interpretive_boundary__parliamentary_sovereignty_reading, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(basi_be_t5, basic_law_interpretive_boundary__parliamentary_sovereignty_reading, base_extractiveness, 5, 0.2).
narrative_ontology:measurement(basi_be_t10, basic_law_interpretive_boundary__parliamentary_sovereignty_reading, base_extractiveness, 10, 0.24).
narrative_ontology:measurement(basi_be_t15, basic_law_interpretive_boundary__parliamentary_sovereignty_reading, base_extractiveness, 15, 0.28).
narrative_ontology:measurement(basi_be_t20, basic_law_interpretive_boundary__parliamentary_sovereignty_reading, base_extractiveness, 20, 0.28).

% Suppression requirement over time
narrative_ontology:measurement(basi_su_t0, basic_law_interpretive_boundary__parliamentary_sovereignty_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(basi_su_t5, basic_law_interpretive_boundary__parliamentary_sovereignty_reading, suppression_requirement, 5, 0.33).
narrative_ontology:measurement(basi_su_t10, basic_law_interpretive_boundary__parliamentary_sovereignty_reading, suppression_requirement, 10, 0.37).
narrative_ontology:measurement(basi_su_t15, basic_law_interpretive_boundary__parliamentary_sovereignty_reading, suppression_requirement, 15, 0.4).
narrative_ontology:measurement(basi_su_t20, basic_law_interpretive_boundary__parliamentary_sovereignty_reading, suppression_requirement, 20, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, judicial_supremacy_reading).
narrative_ontology:affects_constraint(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, balanced_contestation_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three siblings decomposing the natural-language concept 'the Basic Law interpretive boundary' per the epsilon-invariance principle. judicial_supremacy_reading authors a substantially higher epsilon (binding judicial invalidation treated as an actively suppressed check under a sovereignty-maximizing majority) and a different beneficiary/victim structure (Supreme Court and rights-claimants as beneficiaries of a binding check; majority coalition as target of judicial constraint). balanced_contestation_reading authors an intermediate epsilon reflecting genuine bounded authority on both sides. All three share the same kernel_id (basic_law_interpretive_boundary) but instantiate structurally distinct constraints with distinct epsilon values, distinct stakeholder situations, and distinct classifications — they are linked here via affects_constraints rather than merged into one story with a measurement parameter.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
