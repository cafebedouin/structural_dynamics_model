% ============================================================================
% CONSTRAINT STORY: kesavananda_basic_structure__judicial_usurpation_critique
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_kesavananda_basic_structure__judicial_usurpation_critique, []).

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
 *   constraint_id: kesavananda_basic_structure__judicial_usurpation_critique
 *   human_readable: Kesavananda Basic Structure Doctrine: Judicial Usurpation Reading
 *   domain: constitutional_law/judicial_review
 *
 * SUMMARY:
 *   In Kesavananda Bharati v. State of Kerala (1973), the Supreme Court of
 *   India established the doctrine of 'basic structure' — the claim that
 *   certain foundational elements of the Constitution cannot be amended, even
 *   by the supermajority procedure prescribed in Article 368. This doctrine
 *   has no textual basis in the Constitution. Article 368 states that
 *   Parliament, with a two-thirds majority in both houses plus ratification
 *   by half the states, may amend 'any provision' of the Constitution — it
 *   contains no exception for 'basic structure.' Yet the Court invented the
 *   doctrine and vested itself as sole arbiter of what elements are basic.
 *   This reading instantiates the usurpation critique: the basic structure
 *   doctrine is a judicial power grab that transfers constituent authority
 *   (the power to amend the Constitution) from the people, via their elected
 *   representatives and state legislatures, to an unelected bench curating an
 *   unwritten list. The doctrine is a snare: it traps democratic
 *   constitutional change in perpetuity.
 *
 * KEY AGENTS:
 *   - The Supreme Court of India: Primary beneficiary (institutional/arbitrage) — vests final authority over constitutional form in the bench; extracts constituent power
 *   - Parliamentary Amendment Coalition: Primary victim (powerless/trapped) — two-thirds majorities plus half the states cannot override judicial veto; constituent power is relocated
 *   - Democratic Amendment Process: Structural victim (analytical perspective) — the mechanism for democratic constitutional change is suppressed by judicial diktat
 *   - Indian Democracy (Functional): Secondary agent (moderate/constrained) — benefits from the doctrine's prevention of regime capture (Emergency-era safeguard) but bears the cost of judicial oligarchy
 *   - Sibling Readings: The democratic_safeguard_reading (usurpation was justified by its success preventing totalitarian amendment) and implied_limits_reading (basic structure is logical implication, not invention) coexist in Indian law
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(kesavananda_basic_structure__judicial_usurpation_critique, 0.68).
domain_priors:suppression_score(kesavananda_basic_structure__judicial_usurpation_critique, 0.72).
domain_priors:theater_ratio(kesavananda_basic_structure__judicial_usurpation_critique, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(kesavananda_basic_structure__judicial_usurpation_critique, extractiveness, 0.68).
narrative_ontology:constraint_metric(kesavananda_basic_structure__judicial_usurpation_critique, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(kesavananda_basic_structure__judicial_usurpation_critique, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(kesavananda_basic_structure__judicial_usurpation_critique, snare).
narrative_ontology:human_readable(kesavananda_basic_structure__judicial_usurpation_critique, "Kesavananda Basic Structure Doctrine: Judicial Usurpation Reading").
narrative_ontology:topic_domain(kesavananda_basic_structure__judicial_usurpation_critique, "constitutional_law/judicial_review").

domain_priors:requires_active_enforcement(kesavananda_basic_structure__judicial_usurpation_critique).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(kesavananda_basic_structure__judicial_usurpation_critique, '0976606a-6a92-475a-871f-935d2d15caee').
narrative_ontology:cs_kernel_codification('0976606a-6a92-475a-871f-935d2d15caee', formalized).
narrative_ontology:cs_authority_grounding('0976606a-6a92-475a-871f-935d2d15caee', extraction).
narrative_ontology:cs_interpretation_layer_present('0976606a-6a92-475a-871f-935d2d15caee').
narrative_ontology:cs_reading_relation('0976606a-6a92-475a-871f-935d2d15caee', kesavananda_basic_structure__democratic_safeguard_reading, coexists_with).
narrative_ontology:cs_reading_relation('0976606a-6a92-475a-871f-935d2d15caee', kesavananda_basic_structure__implied_limits_reading, influences).
narrative_ontology:cs_axiom('0976606a-6a92-475a-871f-935d2d15caee', foundational, constituent_power_supremacy).
narrative_ontology:cs_axiom_status(constituent_power_supremacy, holdable).
narrative_ontology:cs_axiom_grounding('0976606a-6a92-475a-871f-935d2d15caee', constituent_power_supremacy, deontological).
narrative_ontology:cs_axiom('0976606a-6a92-475a-871f-935d2d15caee', foundational, unelected_bench_veto_is_usurpation).
narrative_ontology:cs_axiom_status(unelected_bench_veto_is_usurpation, holdable).
narrative_ontology:cs_axiom_grounding('0976606a-6a92-475a-871f-935d2d15caee', unelected_bench_veto_is_usurpation, deontological).
narrative_ontology:cs_reference_frame('0976606a-6a92-475a-871f-935d2d15caee', constituent_democratic_amendment).
narrative_ontology:cs_drift_state('0976606a-6a92-475a-871f-935d2d15caee', contemporary_post_2024, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('0976606a-6a92-475a-871f-935d2d15caee', '2026-02-26T00:00:00Z').
narrative_ontology:cs_kernel_id(kesavananda_basic_structure__judicial_usurpation_critique, kesavananda_basic_structure).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(kesavananda_basic_structure__judicial_usurpation_critique, supreme_court_of_india).
narrative_ontology:constraint_beneficiary(kesavananda_basic_structure__judicial_usurpation_critique, unelected_judicial_bench).
narrative_ontology:constraint_victim(kesavananda_basic_structure__judicial_usurpation_critique, constituent_democratic_amendment_process).
narrative_ontology:constraint_victim(kesavananda_basic_structure__judicial_usurpation_critique, parliamentary_sovereignty).
narrative_ontology:constraint_victim(kesavananda_basic_structure__judicial_usurpation_critique, state_legislatures).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: AMENDMENT COALITION (SNARE) — Two-thirds supermajority of Parliament plus half the states cannot amend the Constitution's basic structure, no matter how democratic the mandate. The amendment power is trapped: structural barriers (unwritten judicial list) prevent legitimate constitutional change. Zero degrees of freedom.
constraint_indexing:constraint_classification(kesavananda_basic_structure__judicial_usurpation_critique, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: INDIAN DEMOCRACY / FUNCTIONAL CONTINUITY (TANGLED ROPE) — The basic structure doctrine provides genuine coordination function: it prevented the Emergency-era conversion of the Constitution into a one-party totalitarian document. The constraint genuinely solved a coordination problem (preserving democratic form against regime capture). But it also extracts: the judiciary's veto power is exercised without democratic accountability, and the list of basic-structure elements is curated by judges, not by the people. High suppression (democratic amendment blocked) coexists with genuine coordination benefit (democracy preserved against executive takeover).
constraint_indexing:constraint_classification(kesavananda_basic_structure__judicial_usurpation_critique, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: SUPREME COURT OF INDIA (ROPE) — The Court experiences the basic structure doctrine as pure coordination: it enables the Court to enforce constitutional identity and prevent regime collapse. The Court is the beneficiary — the doctrine vests final authority in the bench. No suppression is felt by the Court; indeed, the Court sees itself as the guardian of constitutional form. Net extraction flows toward the Court, not away.
constraint_indexing:constraint_classification(kesavananda_basic_structure__judicial_usurpation_critique, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: CONSTITUTIONAL THEORY / INSTITUTIONALIST VIEW (PITON) — From an analytically distanced view, the basic structure doctrine is substantially performative. The doctrine's core function (protecting democracy) is actually performed by: (a) institutional norms of judicial restraint, (b) parliamentary vigilance, (c) international pressure, and (d) public mobilization against authoritarian amendment. The judicial pronouncement of 'basic structure' adds theater to these deeper mechanisms. The doctrine persists through institutional inertia despite its core justification (preventing the Emergency) being temporally distant and institutionally superseded by democratic norms. Theater ratio is moderate-high because the doctrine's enforcement mechanism is vague (unwritten list) and the consequences are rarely tested.
constraint_indexing:constraint_classification(kesavananda_basic_structure__judicial_usurpation_critique, piton,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 5: ANALYTICAL OBSERVER / USURPATION CRITIQUE (SNARE) — From the perspective of constituent sovereignty, the basic structure doctrine is pure judicial extraction: the doctrine claims to protect democracy by vesting final constitutional authority in an unelected bench. This is a direct usurpation of constituent power — the power to amend the Constitution is transferred from the people (via supermajority democratic process) to the judiciary (via unwritten judicial list). The theater ratio is lower here than in the piton perspective because the mechanism of extraction is clear: the judiciary directly blocks amendments. The suppression is maximal: there is no exit for the amendment coalition. The extraction is complete: the judiciary extracts final authority over constitutional form.
constraint_indexing:constraint_classification(kesavananda_basic_structure__judicial_usurpation_critique, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(kesavananda_basic_structure__judicial_usurpation_critique_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(kesavananda_basic_structure__judicial_usurpation_critique, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(kesavananda_basic_structure__judicial_usurpation_critique, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(kesavananda_basic_structure__judicial_usurpation_critique, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(kesavananda_basic_structure__judicial_usurpation_critique, TR),
    TR >= 0.70.

:- end_tests(kesavananda_basic_structure__judicial_usurpation_critique_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. The doctrine directly extracts constituent power from the democratic process and vests it in the judiciary. The extraction is real and large-scale — any amendment touching a basic-structure element is blocked regardless of supermajority support. The value reflects that this is not subtle or hidden extraction; it is explicit usurpation. The measurement trajectory (0.42 → 0.48 → 0.65 → 0.68) shows accumulation: as the Court has applied the doctrine more expansively (adding elements to the basic-structure list post hoc), the extractiveness has risen. Suppression (0.72): High. The amendment process is suppressed by the unwritten judicial list. A supermajority plus half the states is rendered powerless. The measurement trajectory (0.55 → 0.72) shows intensification: as the Court has expanded the basic-structure doctrine and asserted it more confidently in cases like S. R. Bommai (1994) and subsequent jurisprudence, the suppression of alternative amendment pathways has increased. Theater ratio (0.58): Moderate-high. The basic structure doctrine is partially performative — it claims to protect democracy by preventing totalitarian constitutional replacement, but this protection is actually provided by institutional norms, parliamentary vigilance, and international pressure. The doctrine adds a layer of judicial pronouncement on top of these mechanisms. However, the theater is not as high as pure performance: the doctrine has real bite (it has blocked amendments), so it is not entirely fake ritual. The trajectory (0.48 → 0.58) shows increasing theater as the doctrine has become more entrenched in jurisprudence and less frequently tested.
 *
 * PERSPECTIVAL GAP:
 *   The core perspectival gap is between the Court (rope/coordination perspective) and the amendment coalition (snare/extraction perspective). The Court sees the basic structure doctrine as a tool for preserving constitutional identity and preventing regime collapse — the doctrine solved the coordination problem posed by the Emergency (preventing totalitarian amendment). The amendment coalition sees the doctrine as a permanent veto over democratic constitutional change — the doctrine is suppression. The piton perspective (institutionalist analysis) observes that the doctrine's function is increasingly performative: the core prevention mechanism (regime-capture prevention) operates through institutional norms and international pressure, not through the judicial pronouncement itself. The analytical usurpation perspective (this reading) emphasizes the raw power transfer: constituent authority has been relocated from the people to the Court. All perspectives are consistent with the base properties but diverge on whether the coordination benefit (preventing totalitarian amendment) outweighs the extraction cost (blocking democratic amendment).
 *
 * DIRECTIONALITY LOGIC:
 *   The Supreme Court's directionality (d ≈ 0.05, power_atom=institutional, exit_options=arbitrage) produces low effective extraction experienced by the Court — the doctrine is a coordination mechanism from the Court's perspective. The amendment coalition's directionality (d ≈ 0.95, power_atom=powerless, exit_options=trapped) produces maximum effective extraction experienced by the trapped parties. The gap between these directionalities is the perspectival gulf: the same constraint is coordination for the beneficiary and snare for the victim. The analytical observer's directionality (d ≈ 0.73, power_atom=analytical, exit_options=analytical) produces the usurpation reading — from civilizational scope, the doctrine is extraction of constituent power.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading resolves the mandatrophy by anchoring on the extraction mechanism: the basic structure doctrine transfers constituent power from the democratic process to the judiciary. The doctrine is snare because (1) it suppresses legitimate democratic amendment (suppression ≥ 0.60), (2) it extracts constituent authority (extractiveness ≥ 0.46), and (3) the amendment coalition cannot exit (trapped). However, the sibling readings produce different classifications: the democratic_safeguard_reading emphasizes the doctrine's coordination function (preventing regime capture) and would classify as tangled_rope; the implied_limits_reading emphasizes the logical/tautological nature of the constraint (amendment power cannot include power to cease-amend) and would classify as mountain. The three readings do not resolve into a single type because they rest on different premises about what the doctrine is fundamentally about: judicial usurpation (snare), democratic protection (tangled_rope), or logical necessity (mountain). The mandatrophy is irresolvable without choosing a reading.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    basic_structure_as_implicit_vs_judicial_invention,
    'Is the basic structure doctrine an implication necessarily contained in the amendment power (''amend this Constitution'' logically cannot include power to replace it), or is it a judicial construction imposed on a text that is silent on amendment limits?',
    'Linguistic and logical analysis of Article 368 (the amendment clause) and comparison to sister constitutions with explicit amendment limits vs. silent amendments. Tracing of Kesavananda judgment''s reasoning to determine whether the Court derived the doctrine from textual implication or asserted it as judicial innovation.',
    'If implicit: the reading shifts from usurpation to valid judicial enforcement of constitutional identity. If invention: the reading confirms usurpation — the Court created the doctrine post hoc to prevent the Emergency, then applied it retroactively as if it were always there. This is the central dispute between the implied_limits_reading and this reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(basic_structure_as_implicit_vs_judicial_invention, conceptual, 'Whether basic structure is textual implication or judicial invention').

omega_variable(
    curated_list_legitimacy_source,
    'What makes the judiciary''s curation of the basic-structure list legitimate? Is it: (a) democratic consensus that the Court speaks for the people''s constitutional identity, (b) institutional expertise in constitutional law, (c) the Court''s track record in preventing regime capture, or (d) nothing — pure power exercised without legitimacy?',
    'Survey of public and elite opinion on the Court''s legitimacy to define basic structure. Analysis of cases where the Court invoked or did not invoke basic structure — what pattern of power exercise emerges? Comparison to other jurisdictions that police constitutional identity through referenda, super-supermajorities, or constitutional courts with different accountability structures.',
    'If (a) consensus: basic structure is democratic safeguard (democratic_safeguard_reading dominates). If (b) expertise: valid judicial role (implied_limits_reading dominates). If (c) track record: contingent legitimacy (snare with genuine past coordination). If (d) pure power: usurpation confirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(curated_list_legitimacy_source, preference, 'Source of legitimacy for judicial curation of basic structure').

omega_variable(
    amendment_suppression_mechanism,
    'Is the suppression of amendment purely judicial (the Court blocks amendments), or is suppression sustained by the political difficulty of achieving supermajority + half-state consensus, which is separate from the basic structure doctrine?',
    'Counterfactual: if basic structure were eliminated, what fraction of blocked amendments would pass? Survey of proposed amendments that died without reaching the Court''s judgment (political barriers) vs. amendments blocked explicitly by basic structure doctrine. Examine the Australian constitutional amendment record (supermajority + referendum) for baseline difficulty without judicial basic-structure doctrine.',
    'If judicial: basic structure is the suppression mechanism (extraction by the Court). If political: the suppression is inherent to the supermajority threshold, and the basic structure doctrine amplifies pre-existing barriers rather than creating them uniquely.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(amendment_suppression_mechanism, empirical, 'Whether suppression is judicial veto or structural difficulty of supermajority amendment').

omega_variable(
    reading_contest_kernel_identity,
    'This constraint is one reading of the Kesavananda basic-structure kernel. Which of the three readings best fits the true structure of Indian constitutional law — judicial usurpation, implied limits, or democratic safeguard?',
    'This is not empirically resolvable. The three readings are incommensurable: they rest on different axioms about constituent sovereignty, judicial legitimacy, and constitutional identity. The readings coexist in Indian jurisprudence and political discourse. The resolution mechanism is argumentative and contestive — legal briefs, academic literature, and future case outcomes will refine the contest.',
    'Determines which omega-class analysis applies: (a) if usurpation confirmed, the constraint''s classification as snare is definitive; (b) if implied limits confirmed, the constraint shifts toward mountain or rope (judicial enforcement of tautological limits); (c) if safeguard confirmed, the constraint shifts toward tangled_rope (genuine coordination with democratic benefit). The three readings produce different terminal classifications.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_contest_kernel_identity, conceptual, 'Which reading of the Kesavananda kernel best describes constitutional reality').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(kesavananda_basic_structure__judicial_usurpation_critique, 1976, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(kbs_juc_theater_1976, kesavananda_basic_structure__judicial_usurpation_critique, theater_ratio, 0, 0.48).
narrative_ontology:measurement(kbs_juc_theater_2024, kesavananda_basic_structure__judicial_usurpation_critique, theater_ratio, 3, 0.58).

% Extraction over time
narrative_ontology:measurement(kbs_juc_extractiveness_1976, kesavananda_basic_structure__judicial_usurpation_critique, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(kbs_juc_extractiveness_1980, kesavananda_basic_structure__judicial_usurpation_critique, base_extractiveness, 1, 0.48).
narrative_ontology:measurement(kbs_juc_extractiveness_2000, kesavananda_basic_structure__judicial_usurpation_critique, base_extractiveness, 2, 0.65).
narrative_ontology:measurement(kbs_juc_extractiveness_2024, kesavananda_basic_structure__judicial_usurpation_critique, base_extractiveness, 3, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(kbs_juc_suppression_1976, kesavananda_basic_structure__judicial_usurpation_critique, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(kbs_juc_suppression_2024, kesavananda_basic_structure__judicial_usurpation_critique, suppression_requirement, 3, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(kesavananda_basic_structure__judicial_usurpation_critique, enforcement_mechanism).
narrative_ontology:affects_constraint(kesavananda_basic_structure__judicial_usurpation_critique, kesavananda_basic_structure__democratic_safeguard_reading).
narrative_ontology:affects_constraint(kesavananda_basic_structure__judicial_usurpation_critique, kesavananda_basic_structure__implied_limits_reading).
narrative_ontology:affects_constraint(kesavananda_basic_structure__judicial_usurpation_critique, indian_emergency_executive_overreach).
narrative_ontology:affects_constraint(kesavananda_basic_structure__judicial_usurpation_critique, constituent_assembly_authority_vs_amendment).

% DUAL FORMULATION NOTE:
% The Kesavananda basic-structure kernel manifests as THREE separate constraint stories with different epsilon values and classification types. This story (judicial_usurpation_critique, epsilon≈0.68, snare) represents one reading. The democratic_safeguard_reading has a lower epsilon (the doctrine prevented genuinely dangerous amendment, ε≈0.45, tangled_rope). The implied_limits_reading treats basic structure as logical necessity (ε≈0.10, mountain). These are not the same constraint viewed from different angles — they are genuinely different constraints with different structural epsilon values and different beneficiary/victim sets. They are linked by kernel identity: all three interpret the same foundational Court decision but extract different structural consequences from it.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
