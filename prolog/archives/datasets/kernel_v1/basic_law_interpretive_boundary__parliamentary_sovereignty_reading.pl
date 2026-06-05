% ============================================================================
% CONSTRAINT STORY: basic_law_interpretive_boundary__parliamentary_sovereignty_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-27
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_basic_law_parliamentary_sovereignty, []).

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
 *   constraint_id: basic_law_interpretive_boundary__parliamentary_sovereignty_reading
 *   human_readable: Knesset Parliamentary Sovereignty over Basic Laws (Interpretive Boundary Reading)
 *   domain: constitutional_law/judicial_review/parliamentary_authority
 *
 * SUMMARY:
 *   This constraint story instantiates ONE reading of a contested
 *   constitutional kernel: the basic law interpretive boundary that separates
 *   judicial authority from legislative authority in the Israeli
 *   constitutional order. This reading — parliamentary sovereignty — claims
 *   that the Knesset, as the sole repository of electoral legitimacy, holds
 *   ultimate and unconstrained authority to interpret, amend, and override
 *   the Basic Laws via simple majority. The reading does not recognize any
 *   external veto mechanism: neither judicial review nor supermajority
 *   requirements nor international treaty obligations constrain the Knesset's
 *   will. This reading is internally coherent but contested by at least two
 *   sibling readings: the judicial-supremacy reading (which claims the
 *   Supreme Court must enforce the Basic Laws against legislative
 *   contradiction) and the balanced-contestation reading (which claims both
 *   institutions hold legitimate but bounded authority). The
 *   parliamentary-sovereignty reading represents a pure-coordination
 *   mechanism for constitutional amendment: it solves the problem of how
 *   elected representatives enact constitutional change without requiring
 *   consensus or external veto. Extractiveness is near-zero because the
 *   coordination function is genuine and the rule imposes no hidden overhead.
 *   However, the reading's legitimacy depends critically on whether the
 *   elected Knesset majority actually represents voter preferences on
 *   constitutional matters — a factual question that omega variables must
 *   resolve.
 *
 * KEY AGENTS:
 *   - Knesset Majority: Primary beneficiary (institutional/arbitrage) — controls constitutional amendment process; experiences the constraint as pure coordination enabling their will
 *   - Parliamentary Minority: Secondary beneficiary with extraction risk (organized/constrained) — participates in legislative process but subject to majoritarian override of constitutional protections
 *   - Subordinate Constituencies: Victims (powerless/trapped) — lack electoral power to influence constitutional interpretation; face unilateral reinterpretation of their Basic Law protections
 *   - Constitutional Reform Movements: Organized agents (organized/constrained) — advocate for supermajority or judicial constraints; see this reading as transient, destined for reform
 *   - Supreme Court: Institutional actor (institutional/mobile) — formally subordinate but sustains real interpretive power through doctrinal innovation and public legitimacy
 *   - Analytical Observer: Cross-jurisdictional perspective (analytical/analytical) — evaluates parliamentary sovereignty as a coordination solution that may be unstable under political polarization
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, 0.08).
domain_priors:suppression_score(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, 0.12).
domain_priors:theater_ratio(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, extractiveness, 0.08).
narrative_ontology:constraint_metric(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, rope).
narrative_ontology:human_readable(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, "Knesset Parliamentary Sovereignty over Basic Laws (Interpretive Boundary Reading)").
narrative_ontology:topic_domain(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, "constitutional_law/judicial_review/parliamentary_authority").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, '53e86e2d-be92-440a-9dfb-ac4f5383266e').
narrative_ontology:cs_kernel_codification('53e86e2d-be92-440a-9dfb-ac4f5383266e', formalized).
narrative_ontology:cs_authority_grounding('53e86e2d-be92-440a-9dfb-ac4f5383266e', extraction).
narrative_ontology:cs_interpretation_layer_present('53e86e2d-be92-440a-9dfb-ac4f5383266e').
narrative_ontology:cs_reading_relation('53e86e2d-be92-440a-9dfb-ac4f5383266e', basic_law_interpretive_boundary__judicial_supremacy_reading, forecloses).
narrative_ontology:cs_reading_relation('53e86e2d-be92-440a-9dfb-ac4f5383266e', basic_law_interpretive_boundary__balanced_contestation_reading, coexists_with).
narrative_ontology:cs_axiom('53e86e2d-be92-440a-9dfb-ac4f5383266e', foundational, electoral_sovereignty_ultimate).
narrative_ontology:cs_axiom_status(electoral_sovereignty_ultimate, holdable).
narrative_ontology:cs_axiom_grounding('53e86e2d-be92-440a-9dfb-ac4f5383266e', electoral_sovereignty_ultimate, conventional).
narrative_ontology:cs_axiom('53e86e2d-be92-440a-9dfb-ac4f5383266e', foundational, judicial_review_not_binding).
narrative_ontology:cs_axiom_status(judicial_review_not_binding, holdable).
narrative_ontology:cs_axiom_grounding('53e86e2d-be92-440a-9dfb-ac4f5383266e', judicial_review_not_binding, deontological).
narrative_ontology:cs_reference_frame('53e86e2d-be92-440a-9dfb-ac4f5383266e', legislative_sovereignty_constitution).
narrative_ontology:cs_drift_state('53e86e2d-be92-440a-9dfb-ac4f5383266e', contemporary_polarized_democracy, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('53e86e2d-be92-440a-9dfb-ac4f5383266e', '').
narrative_ontology:cs_kernel_id(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, basic_law_interpretive_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, elected_knesset_majority).
narrative_ontology:constraint_beneficiary(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, majoritarian_coalition).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: KNESSET MAJORITY (ROPE) — The constraint appears as pure coordination: the Basic Laws establish the framework that the elected legislature interprets and amends. No extraction experienced; the legislature solves the collective-action problem of constitutional amendment and interpretation through its sovereign will. Maximum benefit, minimal coercion. The coordination function is clear: enabling the people's representatives to translate electoral mandates into constitutional action without external veto. Arbitrage exit option reflects the majority's capacity to amend Basic Laws unilaterally, making the constraint purely voluntary from their perspective.
constraint_indexing:constraint_classification(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 2: PARLIAMENTARY MINORITY (TANGLED ROPE) — The constraint creates genuine coordination benefits for minority parties (opposition participates in the legislative process, amendments require parliamentary procedures) but also enables majoritarian extraction: simple-majority override of judicial review means the minority's constitutional protections remain contingent on majority goodwill. The minority can organize and contest within parliament but cannot unilaterally prevent constitutional revision that harms their interests. Mixed experience: some coordination function (shared legislative procedures), but asymmetric extraction risk (majority can rewrite constitutional rules unilaterally).
constraint_indexing:constraint_classification(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: SUBORDINATE CONSTITUENCIES — TRAPPED (SNARE) — Populations lacking electoral power to influence Knesset composition experience this constraint as pure extraction: the sovereign Knesset can reinterpret or override Basic Law protections (freedom of worship, due process, property rights, minority protections) via simple majority without requiring supermajority consensus or judicial review. Their constitutional guarantees are entirely contingent on the goodwill of an elected majority they cannot control. Exit is impossible — they remain subject to the jurisdiction. This perspective sees maximum suppression: no judicial veto mechanism, no requirement for supermajority consensus, no protected exit option. Extraction is the threat of unilateral constitutional reinterpretation.
constraint_indexing:constraint_classification(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 4: CONSTITUTIONAL REFORM MOVEMENTS (SCAFFOLD) — Organized actors (civil rights groups, minority-protection organizations, constitutional scholars) see this constraint as temporary: the simple-majority rule is a transient institutional choice, not a permanent settlement. The sunset is political: as democratic publics demand stronger judicial review protections (European Court of Human Rights norms, constitutional courts in comparative democracies, public opinion toward entrenched rights), political pressure builds for supermajority amendment rules, semi-constitutional courts, or formal constitutional entrenchment. This perspective is currently aspirational (the reform hasn't succeeded) but structurally prescient — it identifies the pathway by which majoritarian sovereignty becomes constrained. Theater is moderate because the reform movement must engage in legislative persuasion, public education, and norm-setting rather than legal contestation.
constraint_indexing:constraint_classification(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: SUPREME COURT (PITON) — The court's formal subordination to Knesset sovereignty is substantially performative at the civilizational timescale. While the parliamentary-sovereignty reading denies the court interpretive authority over Basic Laws, the court has historically exercised interpretive power through constructed doctrines (proportionality review, fundamental rights implicit in the basic-law framework, review of procedural regularity). The court's institutional persistence through political challenge, its professional reputation, and the public expectation that judges rather than politicians adjudicate rights claims create a gap between the formal rule (Knesset sovereign) and the practice (court constrains majority will through interpretation). The piton classification reflects that the court's real authority is sustained largely through theater — the legitimacy narrative that judges are neutral, technical interpreters — rather than through constitutional enforcement power. Exit option is mobile because the court can continually reposition its jurisprudence to minimize direct confrontation with the Knesset.
constraint_indexing:constraint_classification(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (ROPE) — From a cross-jurisdictional perspective, the parliamentary-sovereignty rule solves a genuine coordination problem: how do elected representatives enact constitutional amendments without requiring supermajority consensus or external veto? This reading provides a clean solution: the Knesset, as the sole repository of electoral legitimacy, interprets and amends the constitutional framework unilaterally. The rule is internally coherent, requires minimal enforcement (simple majority voting procedure), and produces clarity about who has ultimate authority. Extractiveness at this perspective is near-zero because the coordination function is genuine and the rule itself imposes no hidden costs. The analytical observer sees this as a pure-coordination mechanism — a constitutional grammar that enables action without extractive overhead. The classification as rope (rather than mountain) reflects that this reading is a contingent institutional choice, not an immutable law of nature.
constraint_indexing:constraint_classification(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(basic_law_interpretive_boundary__parliamentary_sovereignty_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, TR),
    TR >= 0.70.

:- end_tests(basic_law_interpretive_boundary__parliamentary_sovereignty_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.08): Near-zero. The constraint solves a genuine coordination problem — how elected representatives amend constitutional text without requiring supermajority consensus or external veto. At the moment of enactment (1950 Israeli Declaration of Independence) and through its institutional operation, the rule produces a clear grammar for constitutional action with minimal overhead. Extractiveness is not zero because the rule does create asymmetry: those with electoral power to form a Knesset majority can unilaterally reinterpret the constitutional framework, while those without electoral power cannot. This is a thin extraction — the majority captures the power to redefine the constitutional rules — but it is genuine. However, from the perspective of the parliamentary-sovereignty reading itself, this extraction is not experienced as such: the majority sees coordination (their will enacted), not extraction. The near-zero value reflects the reading's own logic, not a claim about objective fairness. Suppression (0.12): Low. The constraint imposes minimal coercion: the legislative procedure for amendment is transparent and participatory (all Knesset members can vote). Alternatives are not suppressed in the sense of being forbidden — opposition parties can propose amendments, organize public campaigns, or appeal to electoral constituencies. However, suppression is not zero because those lacking electoral power have no procedural mechanism to constrain a determined majority. Suppression rises from 0.02 (1950, when the Israeli polity was less fragmented) to 0.12 (2026, as coalition dynamics and polarization reduce the power of procedural voice). Theater ratio (0.35): Moderate-low. The legislative amendment process is largely functional — Knesset votes are transparent, recorded, and produce binding constitutional change. But theater rises over time as the gap between formal parliamentary sovereignty and actual court authority widens. The court exercises interpretive power it formally does not possess (perspective 5: piton analysis), creating a performative gap. The rise from 0.25 to 0.35 reflects increasing theatrical maintenance: the parliament must rhetorically assert sovereignty while the court continually asserts interpretive authority, creating a ritual of contestation that is partly performative.
 *
 * PERSPECTIVAL GAP:
 *   The parliamentary-sovereignty reading produces stark perspectival gaps across the indexed contexts. The Knesset majority (institutional/arbitrage) sees pure rope — coordination enabling their will with no experienced extraction. The subordinate constituencies (powerless/trapped) see pure snare — constitutional guarantees held hostage to majoritarian goodwill. The analytical observer (analytical/analytical) sees rope — a coordination solution that may be unstable under polarization. The court (institutional/mobile) sees piton — it maintains real interpretive power through doctrinal theater despite formal subordination. These gaps are not artifacts of measurement uncertainty; they reflect genuine structural asymmetries in how different positions experience the constraint. The reading's coherence depends on which perspective is adopted. From the majority's view, it is purely coordinative. From the trapped view, it is purely extractive. The perspectival gap is the kernel contest itself — the different perspectives represent the competing readings (parliamentary-sovereignty vs. judicial-supremacy vs. balanced-contestation) and their divergent empirical claims about where authority actually lies.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) in this reading is determined by the agent's electoral power relative to constitutional change. Agents with majority control have d near 0.0 (full beneficiary): the Knesset majority. Agents without electoral power but subject to constitutional reinterpretation have d near 1.0 (full target): subordinate constituencies. Agents with procedural power but no veto (opposition, minority parties) have intermediate d (~0.50): they experience mixed benefits (legislative participation) and costs (majoritarian override risk). The court occupies an unusual position: formally subordinate (d should be high), but practically powerful through doctrinal authority (d effectively lower through mobile exit option). The directionality derivation from beneficiary/victim declarations produces accurate results: beneficiaries (elected majority) get low d → low extracted chi; victims (powerless constituencies) get high d → high experienced chi. The reading's ε (0.08) remains stable across these directionality variations because ε is a structural property of the constraint itself (how much power the rule concentrates), not a function of who experiences it.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading avoids mandatrophy by maintaining consistency between its claimed type (rope) and its structural logic. The rule is genuinely coordinative from the beneficiary perspective: it enables constitutional action by elected representatives without requiring impossible consensus. It is not snare-with-theater-disguised-as-rope; it is rope with the understanding that rope is experienced asymmetrically from different positions. The reading's weakness (not mandatrophy, but legitimacy vulnerability) lies in the empirical assumptions: (1) does the elected majority actually represent voter preferences? (2) does simple-majority rule remain stable under political polarization? If either assumption fails, extractiveness must be revised upward, and the reading may shift toward tangled_rope (coordination function genuine but extraction asymmetry severe) or snare (extraction risk dominant). The omegas document these vulnerabilities. The mandatrophy is resolved by epistemic honesty: this reading claims coordination, not perfect fairness. It sees the constraint as rope, and the rope is real — it enables action. The shadow side (that this rope extracts from powerless constituencies) is not mandatrophy; it is the asymmetry inherent in any democratic rule that ties authority to electoral power.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    electoral_representation_fidelity,
    'Does the Knesset''s elected majority actually represent the will of the general voting public, or has it been systematically biased by gerrymandering, strategic defection, or coalition arithmetic that decouples representation from electoral preferences?',
    'Analysis of seat distribution vs. electoral percentages over multiple election cycles; structural comparison of coalition-building power vs. vote share; cross-national empirical study of whether Israeli coalition majorities consistently reflect public opinion on constitutional questions specifically',
    'If elected majority reliably represents voter will: parliamentary sovereignty reading is legitimacy-grounded; extractiveness near-zero is accurate. If majority systematically diverges from voter preferences: extractiveness must be revised upward (majority extracts from non-represented voters); classification may shift toward tangled_rope or snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(electoral_representation_fidelity, empirical, 'Whether elected Knesset majority faithfully represents voter will on constitutional questions').

omega_variable(
    simple_majority_stability,
    'Does a simple-majority rule for Basic Law amendment reliably function as a stable constitutional grammar, or does it predictably devolve into constitutional crises when majority interests conflict sharply with entrenched minority protections?',
    'Historical analysis of attempted/successful Basic Law amendments and constitutional crises in Israel; comparative study of simple-majority rules in other constitutional systems; forward modeling of coalition dynamics under extreme political polarization (minority rights threatened by majority policy)',
    'If stable: parliamentary sovereignty reading is viable; extractiveness and suppression remain low. If unstable: the rule''s functional failure creates secondary governance costs; extractiveness effectively rises (the rule fails to deliver promised coordination); classification may shift toward piton (performative rule masking actual power dynamics).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(simple_majority_stability, empirical, 'Stability of simple-majority rule for Basic Law amendment under political polarization').

omega_variable(
    reading_kernel_foreclosure,
    'Does the parliamentary-sovereignty reading logically foreclose the judicial-supremacy reading within a single constitutional framework, or can both coexist as competing legitimate institutions with different spheres?',
    'Formal logical analysis of axioms: if parliamentary sovereignty includes the right to override judicial review, and judicial supremacy requires that courts can invalidate legislative acts, can both claims be held by the same legal system? Doctrine analysis: how Israeli case law and constitutional theory have negotiated this tension historically.',
    'If foreclosed: the reading relations should declare forecloses = judicial_supremacy_reading; the two readings are mutually exclusive frameworks. If coexisting: declare coexists_with for both judicial_supremacy and balanced_contestation; all three readings remain live options.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_kernel_foreclosure, conceptual, 'Whether parliamentary sovereignty logically rules out judicial supremacy in a single framework').

omega_variable(
    international_obligation_override,
    'Can the Knesset override international treaty obligations (European Court of Human Rights rulings, UN Covenant on Civil and Political Rights) via simple-majority Basic Law amendment, or do such obligations constrain parliamentary sovereignty?',
    'Textual analysis of Israel''s treaty commitments and their legal status; case law from Israeli Supreme Court on treaty override; comparative constitutional law on the relationship between domestic sovereignty and international law',
    'If Knesset can override: parliamentary sovereignty is unconstrained; extractiveness near-zero is correct. If international obligations bind the Knesset: there is an external veto on simple-majority action; extractiveness must be revised upward; the constraint becomes tangled_rope (parliament partially constrained by external commitments, not purely sovereign).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(international_obligation_override, empirical, 'Whether international treaty obligations constrain Knesset simple-majority sovereignty').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, 1950, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(blps_theater_1950, basic_law_interpretive_boundary__parliamentary_sovereignty_reading, theater_ratio, 1950, 0.25).
narrative_ontology:measurement(blps_theater_1992, basic_law_interpretive_boundary__parliamentary_sovereignty_reading, theater_ratio, 1992, 0.32).
narrative_ontology:measurement(blps_theater_2026, basic_law_interpretive_boundary__parliamentary_sovereignty_reading, theater_ratio, 2026, 0.35).

% Extraction over time
narrative_ontology:measurement(blps_extractiveness_1950, basic_law_interpretive_boundary__parliamentary_sovereignty_reading, base_extractiveness, 1950, 0.02).
narrative_ontology:measurement(blps_extractiveness_1992, basic_law_interpretive_boundary__parliamentary_sovereignty_reading, base_extractiveness, 1992, 0.05).
narrative_ontology:measurement(blps_extractiveness_2026, basic_law_interpretive_boundary__parliamentary_sovereignty_reading, base_extractiveness, 2026, 0.08).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, basic_law_interpretive_boundary__judicial_supremacy_reading).
narrative_ontology:affects_constraint(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, basic_law_interpretive_boundary__balanced_contestation_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of a contested constitutional kernel. The sibling readings (judicial_supremacy_reading and balanced_contestation_reading) represent structurally distinct claims about where ultimate authority lies in the Israeli constitutional order. All three stories share the same constraint_id prefix (basic_law_interpretive_boundary) but instantiate different readings with different ε values and beneficiary/victim structures. The parliamentary_sovereignty_reading claims near-zero extractiveness because it sees pure coordination (elected majority enacting constitutional change). The balanced_contestation_reading (ε=0.38, tangled_rope) claims mixed coordination and asymmetric extraction because it recognizes both institutions' legitimacy but acknowledges the majority's override power. The judicial_supremacy_reading (ε TBD) claims extractive suppression because it sees judicial authority constrained by parliamentary veto. These are not measurement artifacts or contextual variations — they are genuinely different structural claims about where constitutional authority resides. The network edge indicates structural influence: parliamentary-sovereignty forecloses judicial-supremacy; both coexist-with balanced-contestation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
