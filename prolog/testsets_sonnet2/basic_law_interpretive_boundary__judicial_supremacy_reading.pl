% ============================================================================
% CONSTRAINT STORY: basic_law_interpretive_boundary__judicial_supremacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_basic_law_interpretive_boundary__judicial_supremacy_reading, []).

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
 *   constraint_id: basic_law_interpretive_boundary__judicial_supremacy_reading
 *   human_readable: Judicial Supremacy Reading of Basic Law Interpretive Authority
 *   domain: constitutional_law/comparative_constitutionalism
 *
 * SUMMARY:
 *   This story instantiates the judicial supremacy reading of the contested
 *   Basic Law interpretive boundary kernel: the Supreme Court's post-1995
 *   constitutional review doctrine treats the Basic Laws as a higher-order
 *   framework whose violation by ordinary legislation is judicially
 *   cognizable and whose invalidation rulings bind the Knesset absent a Basic
 *   Law amendment. From this reading's own vantage, the standing arrangement
 *   is real judicial constraint on majoritarian legislation — the referent
 *   for extractiveness is this arrangement as it actually operates today, not
 *   the parliamentary-sovereignty or balanced-contestation alternatives. Two
 *   sibling constraints instantiate the other readings of the same kernel:
 *   parliamentary_sovereignty_reading (Knesset retains ultimate interpretive
 *   authority) and balanced_contestation_reading (bounded, mutually
 *   respecting authority). ε differs meaningfully across the three: this
 *   reading, authored on its own terms, treats the reasonableness-doctrine
 *   expansion as a substantial, rising transfer of policy authority from the
 *   elected legislature to the bench, and the metrics reflect that reading's
 *   own assessment of the arrangement it describes.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(basic_law_interpretive_boundary__judicial_supremacy_reading, 0.58).
domain_priors:suppression_score(basic_law_interpretive_boundary__judicial_supremacy_reading, 0.52).
domain_priors:theater_ratio(basic_law_interpretive_boundary__judicial_supremacy_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(basic_law_interpretive_boundary__judicial_supremacy_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(basic_law_interpretive_boundary__judicial_supremacy_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(basic_law_interpretive_boundary__judicial_supremacy_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(basic_law_interpretive_boundary__judicial_supremacy_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(basic_law_interpretive_boundary__judicial_supremacy_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(basic_law_interpretive_boundary__judicial_supremacy_reading, tangled_rope).
narrative_ontology:human_readable(basic_law_interpretive_boundary__judicial_supremacy_reading, "Judicial Supremacy Reading of Basic Law Interpretive Authority").
narrative_ontology:topic_domain(basic_law_interpretive_boundary__judicial_supremacy_reading, "constitutional_law/comparative_constitutionalism").

domain_priors:requires_active_enforcement(basic_law_interpretive_boundary__judicial_supremacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(basic_law_interpretive_boundary__judicial_supremacy_reading, '171ef0ec-d10f-43e9-aa37-def61ca1ff46').
narrative_ontology:cs_kernel_codification('171ef0ec-d10f-43e9-aa37-def61ca1ff46', distributed).
narrative_ontology:cs_authority_grounding('171ef0ec-d10f-43e9-aa37-def61ca1ff46', extraction).
narrative_ontology:cs_interpretation_layer_present('171ef0ec-d10f-43e9-aa37-def61ca1ff46').
narrative_ontology:cs_reading_relation('171ef0ec-d10f-43e9-aa37-def61ca1ff46', basic_law_interpretive_boundary__parliamentary_sovereignty_reading, forecloses).
narrative_ontology:cs_reading_relation('171ef0ec-d10f-43e9-aa37-def61ca1ff46', basic_law_interpretive_boundary__balanced_contestation_reading, influences).
narrative_ontology:cs_axiom('171ef0ec-d10f-43e9-aa37-def61ca1ff46', foundational, judicial_invalidation_binds_knesset_absent_amendment).
narrative_ontology:cs_axiom_status(judicial_invalidation_binds_knesset_absent_amendment, holdable).
narrative_ontology:cs_axiom_grounding('171ef0ec-d10f-43e9-aa37-def61ca1ff46', judicial_invalidation_binds_knesset_absent_amendment, conventional).
narrative_ontology:cs_axiom('171ef0ec-d10f-43e9-aa37-def61ca1ff46', secondary, basic_laws_form_entrenched_higher_tier).
narrative_ontology:cs_axiom_status(basic_laws_form_entrenched_higher_tier, holdable).
narrative_ontology:cs_axiom_grounding('171ef0ec-d10f-43e9-aa37-def61ca1ff46', basic_laws_form_entrenched_higher_tier, conventional).
narrative_ontology:cs_reference_frame('171ef0ec-d10f-43e9-aa37-def61ca1ff46', bank_mizrahi_constitutional_revolution_settlement).
narrative_ontology:cs_drift_state('171ef0ec-d10f-43e9-aa37-def61ca1ff46', post_2023_judicial_reform_contestation, gap(repudiation_pressure, severe, true)).
narrative_ontology:cs_created_at('171ef0ec-d10f-43e9-aa37-def61ca1ff46', '').
narrative_ontology:cs_kernel_id(basic_law_interpretive_boundary__judicial_supremacy_reading, basic_law_interpretive_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(basic_law_interpretive_boundary__judicial_supremacy_reading, rights_claimant_litigants).
narrative_ontology:constraint_beneficiary(basic_law_interpretive_boundary__judicial_supremacy_reading, supreme_court_justices).
narrative_ontology:constraint_beneficiary(basic_law_interpretive_boundary__judicial_supremacy_reading, minority_groups_seeking_judicial_protection).
narrative_ontology:constraint_beneficiary(basic_law_interpretive_boundary__judicial_supremacy_reading, constitutional_law_bar).
narrative_ontology:constraint_victim(basic_law_interpretive_boundary__judicial_supremacy_reading, knesset_majority_coalition).
narrative_ontology:constraint_victim(basic_law_interpretive_boundary__judicial_supremacy_reading, legislators_pursuing_majoritarian_reform).
narrative_ontology:constraint_victim(basic_law_interpretive_boundary__judicial_supremacy_reading, executive_branch_ministers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interpret the Basic Laws as a higher constitutional tier and exercise judicial review to strike down Knesset legislation found incompatible with them, most centrally under the reasonableness and proportionality doctrines developed since the 1990s constitutional revolution. Their invalidation rulings bind the Knesset unless overridden by amending the Basic Law itself, which is procedurally difficult. They administer the interpretive boundary and their institutional authority expands with each exercise of review.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__judicial_supremacy_reading, supreme_court_justices, agenda_setter,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(basic_law_interpretive_boundary__judicial_supremacy_reading, supreme_court_justices, beneficiary).

% Passes legislation reflecting an electoral mandate, only to see it invalidated or narrowed by judicial interpretation of Basic Law principles it did not explicitly enact. Formal recourse exists — amend the Basic Law, pass an override clause, or reconstitute the judicial appointments process — but each path requires supermajorities, prolonged political capital, or reform of the very system reviewing the reform, making exit costly and slow rather than foreclosed.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__judicial_supremacy_reading, knesset_majority_coalition, payer,
    powerful, biographical, constrained, national).

% Individuals and civil society organizations who bring petitions asking the Court to strike down legislation on Basic Law grounds — challenging conscription exemptions, discriminatory administrative practices, or infringements on due process. They gain an avenue to override a hostile legislative majority by litigating rather than mobilizing votes, and their access to this avenue is itself a form of political power that bypasses electoral arithmetic.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__judicial_supremacy_reading, rights_claimant_litigants, beneficiary,
    moderate, biographical, mobile, national).

% Populations without reliable Knesset majorities to protect their interests — certain ethnic, religious, or political minorities — rely on the Court as a counter-majoritarian backstop against legislation that would otherwise pass on a simple majority vote. For them the constraint is close to pure benefit: they have no comparable legislative leverage and depend structurally on judicial review remaining binding.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__judicial_supremacy_reading, minority_groups_seeking_judicial_protection, beneficiary,
    powerless, generational, trapped, national).

% Administrative decisions and cabinet-level policy are subject to the same reasonableness review the Court applies to primary legislation, constraining executive discretion on appointments, security policy, and resource allocation. Ministers can appeal to the Knesset for legislative cover but that legislation is itself subject to the same interpretive boundary, so the constraint recurs at every level of executive action.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__judicial_supremacy_reading, executive_branch_ministers, payer,
    powerful, biographical, constrained, national).

% Lawyers, legal scholars, and litigation NGOs whose professional and institutional relevance depends on an active, powerful judiciary willing to invalidate legislation. They advise petitioners, publish doctrine expanding the interpretive boundary, and their career and institutional incentives align closely with judicial supremacy remaining the operative reading of the kernel.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__judicial_supremacy_reading, constitutional_law_bar, beneficiary,
    organized, generational, arbitrage, national).

% Voters who elected the Knesset majority whose legislation was struck down have no direct mechanism to contest the interpretation itself — they can only try to elect a future majority large enough to pursue a Basic Law amendment or judicial reform, a much higher bar than the simple majority that passed the original law. Their electoral will is filtered through a body they did not select and cannot recall.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__judicial_supremacy_reading, excluded_majoritarian_public, excluded,
    powerless, biographical, trapped, national).

% Study the Israeli case as a data point in debates over judicial review without constitutional entrenchment, comparing it to Canada's notwithstanding clause and the UK's parliamentary sovereignty tradition, without directly bearing the costs or collecting the benefits of any particular reading.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__judicial_supremacy_reading, comparative_constitutional_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(basic_law_interpretive_boundary__judicial_supremacy_reading, diffuse).
narrative_ontology:fixing_cost_class(basic_law_interpretive_boundary__judicial_supremacy_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a mechanism to prevent simple legislative majorities from eroding baseline rights and procedural norms that a written, entrenched constitution would otherwise protect — solving the problem of majoritarian overreach in a system where Basic Laws were enacted incrementally and without the supermajority thresholds typical of constitutional amendment elsewhere.
% TRANSFER_FUNCTION: Moves effective policy-making authority over contested legislation from the elected Knesset majority to the unelected Supreme Court bench; transfers litigation leverage to organized rights-claimants and the constitutional bar, and transfers protective insurance to minority groups who lack majoritarian power, at the cost of majoritarian legislative finality for the coalition that won the election.
% ABSENT_VOICES: The Knesset majority itself, in its future legislative sessions, has no seat in the room when the Court interprets a Basic Law's scope — its only voice is retrospective, through argument at the bar or subsequent override legislation. The broader electorate that produced the majority is even further removed, represented only through the elected officials whose acts are being reviewed.
% DISAPPEARANCE_RATIONALE: If judicial invalidation ceased to bind the Knesset overnight, legislation currently blocked or narrowed by reasonableness review would take effect immediately, minority-protective doctrines built up since the 1990s constitutional revolution would lose their enforcement mechanism, and the constitutional bar's litigation strategy would lose its primary lever — political contestation would shift almost entirely back to the legislative and electoral arena.
% FOUNDING_PROBLEM: Israel lacks a single entrenched written constitution; the Basic Laws were enacted piecemeal without supermajority protection, leaving open how (or whether) legislation could ever be checked against them. The 1992 Basic Laws on human dignity and occupational freedom, followed by the 1995 Bank Mizrahi ruling, were built to solve the problem of a rights framework with no enforcement mechanism against ordinary legislation.
% FOUNDING_PROBLEM_CORROBORATION: Sitting and former justices and much of the academic legal community attest the founding problem (unenforceable rights provisions) remains live and requires judicial review to resolve. Knesset members from successive majority coalitions, coalition legal advisors, and a substantial body of comparative constitutional scholarship writing from outside both the Court and the government attest that the Court's own doctrine (particularly the reasonableness standard) expanded well past what the 1992 Basic Laws textually authorized, making the current scope of review a judicially self-authorized extension rather than a corroborated continuation of the original founding problem.
narrative_ontology:disappearance_verdict(basic_law_interpretive_boundary__judicial_supremacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(basic_law_interpretive_boundary__judicial_supremacy_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(basic_law_interpretive_boundary__judicial_supremacy_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(basic_law_interpretive_boundary__judicial_supremacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(basic_law_interpretive_boundary__judicial_supremacy_reading, 0.58, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(basic_law_interpretive_boundary__judicial_supremacy_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(basic_law_interpretive_boundary__judicial_supremacy_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(basic_law_interpretive_boundary__judicial_supremacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises from 0.28 to 0.58 over the interval, tracking the documented expansion of judicial review doctrine from the 1995 Bank Mizrahi ruling's initial narrow test through the broader reasonableness standard applied to legislative and executive acts. Suppression (0.52) reflects that once a law is struck down the Knesset's only recourse is Basic Law amendment or override legislation, both requiring political capital well beyond the ordinary majority that passed the original act — a genuine but not absolute barrier, hence moderate rather than near-total. Theater ratio is low (0.22) because the review function is substantively exercised, not performative; the Court issues binding, consequential rulings rather than symbolic gestures. Resistance is high (0.72) because Knesset majorities, ministers, and portions of the electorate actively contest the doctrine's legitimacy and have repeatedly attempted judicial reform legislation in response.
 *
 * DIRECTIONALITY LOGIC:
 *   Supreme Court justices and the constitutional bar sit at the beneficiary end: their institutional authority and professional relevance expand with the doctrine. Rights-claimant litigants and structurally powerless minority groups are also beneficiaries — for the latter especially, this reading is close to pure subsidy, since they lack comparable majoritarian leverage and depend on judicial review as their only structural protection. The Knesset majority coalition and executive ministers sit at the target end: they bear the transfer, their exit options are constrained rather than trapped (Basic Law amendment and judicial-appointment reform exist but are costly), and effective extraction against them is amplified accordingly. The excluded majoritarian public experiences the constraint at one further remove — trapped, with no direct standing to contest the interpretation itself.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding_problem status is authored as contested rather than dead precisely to avoid two symmetric mislabeling errors: treating the entire apparatus as pure extraction (ignoring that unenforceable rights provisions were a genuine problem in 1992) or treating its current scope as pure coordination (ignoring that the reasonableness standard's expansion outran what the founding Basic Laws textually authorized). The tangled_rope classification captures this directly — coordination function (protecting minorities and rights against majoritarian erosion) and asymmetric extraction (transfer of policy finality from an elected majority to an unelected bench) are both genuinely present and both require the active enforcement of binding invalidation to persist.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indeterminacy,
    'Is the judicial supremacy reading — binding, non-overridable invalidation absent Basic Law amendment — the correct structural description of the current Israeli constitutional arrangement, or is the parliamentary sovereignty reading (Knesset retains final say via simple majority override) or the balanced contestation reading more accurate?',
    'No single text resolves this: Israel''s Basic Laws do not contain an entrenchment clause specifying supermajority requirements for amendment, no explicit override mechanism was legislated until proposed 2023 reforms, and the Supreme Court''s own claim to binding review authority rests on judicial self-interpretation (Bank Mizrahi) rather than explicit constitutional text. Resolution would require either a definitive Basic Law amendment settling the override question, or long-run observation of whether the Knesset successfully overrides a judicial invalidation without further judicial invalidation of the override itself.',
    'If the parliamentary sovereignty reading is structurally correct, this constraint''s high suppression and extractiveness values are overstated — the Knesset''s constrained exit options would in fact be closer to mobile. If the judicial supremacy reading holds, the classification and rising extraction trend stand as authored.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_indeterminacy, conceptual, 'Which of three competing readings of the Basic Law interpretive boundary is the structurally correct description of the current system.').

omega_variable(
    reasonableness_doctrine_scope_ambiguity,
    'Did the reasonableness standard''s application to primary legislation (rather than only administrative acts) originate from a genuine extension of Basic Law rights protection, or from judicial self-authorization beyond what the 1992 Basic Laws textually granted?',
    'Comparative doctrinal history tracing the reasonableness standard''s application from administrative law (Kach party cases) through to primary legislation review, cross-checked against the explicit text of the 1992 Human Dignity and Liberty and Freedom of Occupation Basic Laws.',
    'If judicial self-authorization, the coordination function is weaker than claimed and the extraction component dominates more heavily than the tangled_rope classification''s balance suggests, pushing the structural reading closer to snare. If genuine textual extension, tangled_rope''s coordination/extraction balance holds as authored.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reasonableness_doctrine_scope_ambiguity, empirical, 'Whether the doctrine''s scope expansion is textually grounded or judicially self-authorized.').

omega_variable(
    false_summit_natural_constitutional_order,
    'Is the framing of the Basic Laws as an inherently higher-order legal tier a structural/natural feature of any constitutional system, or a constructed interpretive choice that benefits the judiciary and litigation-capable actors specifically?',
    'Comparative analysis: jurisdictions with codified, entrenched constitutions (supermajority amendment thresholds) versus Israel''s incremental, non-entrenched Basic Laws — if Israel''s arrangement functions identically to entrenched systems despite lacking entrenchment procedure, that supports the natural-hierarchy framing; if it depends entirely on judicial doctrine with no textual entrenchment, that supports the constructed framing.',
    'Bears on whether future stories treating ''constitutional supremacy'' as a mountain-like natural feature of this system should instead carry FSM-style beneficiary declarations, since this reading is not itself claimed as a mountain but the surrounding discourse sometimes treats the hierarchy as self-evidently natural.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(false_summit_natural_constitutional_order, conceptual, 'Whether treating Basic Laws as an inherent higher legal tier naturalizes what is actually a contested, judicially-constructed hierarchy.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(basic_law_interpretive_boundary__judicial_supremacy_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(basi_tr_t0, basic_law_interpretive_boundary__judicial_supremacy_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement(basi_tr_t5, basic_law_interpretive_boundary__judicial_supremacy_reading, theater_ratio, 5, 0.1).
narrative_ontology:measurement(basi_tr_t10, basic_law_interpretive_boundary__judicial_supremacy_reading, theater_ratio, 10, 0.13).
narrative_ontology:measurement(basi_tr_t15, basic_law_interpretive_boundary__judicial_supremacy_reading, theater_ratio, 15, 0.16).
narrative_ontology:measurement(basi_tr_t20, basic_law_interpretive_boundary__judicial_supremacy_reading, theater_ratio, 20, 0.18).
narrative_ontology:measurement(basi_tr_t25, basic_law_interpretive_boundary__judicial_supremacy_reading, theater_ratio, 25, 0.2).
narrative_ontology:measurement(basi_tr_t30, basic_law_interpretive_boundary__judicial_supremacy_reading, theater_ratio, 30, 0.22).

% Extraction over time
narrative_ontology:measurement(basi_be_t0, basic_law_interpretive_boundary__judicial_supremacy_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(basi_be_t5, basic_law_interpretive_boundary__judicial_supremacy_reading, base_extractiveness, 5, 0.35).
narrative_ontology:measurement(basi_be_t10, basic_law_interpretive_boundary__judicial_supremacy_reading, base_extractiveness, 10, 0.42).
narrative_ontology:measurement(basi_be_t15, basic_law_interpretive_boundary__judicial_supremacy_reading, base_extractiveness, 15, 0.48).
narrative_ontology:measurement(basi_be_t20, basic_law_interpretive_boundary__judicial_supremacy_reading, base_extractiveness, 20, 0.53).
narrative_ontology:measurement(basi_be_t25, basic_law_interpretive_boundary__judicial_supremacy_reading, base_extractiveness, 25, 0.56).
narrative_ontology:measurement(basi_be_t30, basic_law_interpretive_boundary__judicial_supremacy_reading, base_extractiveness, 30, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(basi_su_t0, basic_law_interpretive_boundary__judicial_supremacy_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(basi_su_t5, basic_law_interpretive_boundary__judicial_supremacy_reading, suppression_requirement, 5, 0.35).
narrative_ontology:measurement(basi_su_t10, basic_law_interpretive_boundary__judicial_supremacy_reading, suppression_requirement, 10, 0.4).
narrative_ontology:measurement(basi_su_t15, basic_law_interpretive_boundary__judicial_supremacy_reading, suppression_requirement, 15, 0.44).
narrative_ontology:measurement(basi_su_t20, basic_law_interpretive_boundary__judicial_supremacy_reading, suppression_requirement, 20, 0.47).
narrative_ontology:measurement(basi_su_t25, basic_law_interpretive_boundary__judicial_supremacy_reading, suppression_requirement, 25, 0.5).
narrative_ontology:measurement(basi_su_t30, basic_law_interpretive_boundary__judicial_supremacy_reading, suppression_requirement, 30, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(basic_law_interpretive_boundary__judicial_supremacy_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(basic_law_interpretive_boundary__judicial_supremacy_reading, basic_law_interpretive_boundary__parliamentary_sovereignty_reading).
narrative_ontology:affects_constraint(basic_law_interpretive_boundary__judicial_supremacy_reading, basic_law_interpretive_boundary__balanced_contestation_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the basic_law_interpretive_boundary kernel, decomposed per the ε-invariance principle rather than represented as one constraint with an observer-relative extraction value. judicial_supremacy_reading (this story) authors substantial and rising ε reflecting binding, hard-to-override judicial invalidation. parliamentary_sovereignty_reading would author low ε for the same underlying arrangement, since from that reading the Knesset retains effective final authority and judicial rulings are advisory-in-substance regardless of their formal binding language. balanced_contestation_reading would author moderate ε reflecting genuine mutual constraint without full capture by either institution. Each story carries its own claimed_type, stakeholders, and metrics; they are linked here rather than merged.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
