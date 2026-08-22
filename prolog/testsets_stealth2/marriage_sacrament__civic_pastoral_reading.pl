% ============================================================================
% CONSTRAINT STORY: marriage_sacrament__civic_pastoral_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_marriage_sacrament__civic_pastoral_reading, []).

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
    narrative_ontology:coordination_type/2,
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
 *   constraint_id: marriage_sacrament__civic_pastoral_reading
 *   human_readable: Pastoral Discernment Regime for Marriage (Civic-Pastoral Reading of the Sacrament)
 *   domain: religious doctrine / canon law / political sociology
 *
 * SUMMARY:
 *   The civic-pastoral reading governs marriage as a pastoral relationship
 *   subject to human failure: indissolubility is affirmed as an ideal, and
 *   individual cases are resolved through compassionate discernment —
 *   pastoral accompaniment, internal-forum process, and a streamlined
 *   annulment track — rather than through fixed hierarchical adjudication.
 *   The arrangement crystallized across the post-conciliar period (interval
 *   0–60 maps to 1965–2025): the personalist reframing of the mid-1960s, the
 *   2015 streamlining of nullity processes, and the 2016 discernment
 *   framework, with implementation deepening unevenly since. This file
 *   instantiates ONE reading of the marriage_sacrament kernel; the sibling
 *   hierarchical-indissolubility reading (bond constitutive, adjudication
 *   hierarchical, paying class = divorced-and-remarried Catholics excluded
 *   from communion) is a separate constraint linked via
 *   network.affects_constraints, with its own epsilon. Epsilon's referent is
 *   the standing pastoral-discernment arrangement itself, assessed by this
 *   reading's own lights — not the hierarchical arrangement this reading
 *   displaces, and not the fully realized discernment practice it endorses.
 *   The claim and the metrics are independent authored facts: claimed_type
 *   states the structure I believe true (tangled_rope — genuine coordination
 *   holding ideal and mercy in one communion, with asymmetric burden falling
 *   on traditional laity); the metrics state what I believe descriptively
 *   true of the regime's operation. The engine computes per-seat
 *   classifications from the structural data; where computed types diverge
 *   from the claim, that divergence is the measurement.
 *
 * KEY AGENTS:
 *   - roman_magisterium: agenda-setter (institutional/arbitrage) — sets the doctrinal frame, disciplines resistance, collects retention while absorbing authority-erosion costs
 *   - diocesan_pastors: primary beneficiary (organized/constrained) — collect the discretionary adjudicative authority the regime creates; conduct the discernment the regime centers
 *   - divorced_remarried_catholics: beneficiary (organized/constrained) — gain a case-by-case path back to sacramental communion
 *   - traditional_catholics: primary target (organized/identity_locked) — bear doctrinal relativization; their identity depends on doctrinal stability
 *   - abandoned_first_spouses: secondary target (powerless/trapped) — first bonds set aside without standing in the process that re-determines them
 *   - adult_children_of_first_marriages: excluded voice (moderate/constrained) — their family history is adjudicated without a hearing
 *   - canon_law_tribunal_officials: secondary beneficiary (organized/constrained) — administer the streamlined formal track parallel to discernment
 *   - religious_sociologists: analytical observer — document the gap between norm and practice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_sacrament__civic_pastoral_reading, 0.48).
domain_priors:suppression_score(marriage_sacrament__civic_pastoral_reading, 0.32).
domain_priors:theater_ratio(marriage_sacrament__civic_pastoral_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_sacrament__civic_pastoral_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(marriage_sacrament__civic_pastoral_reading, suppression_requirement, 0.32).
narrative_ontology:constraint_metric(marriage_sacrament__civic_pastoral_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(marriage_sacrament__civic_pastoral_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(marriage_sacrament__civic_pastoral_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_sacrament__civic_pastoral_reading, tangled_rope).
narrative_ontology:human_readable(marriage_sacrament__civic_pastoral_reading, "Pastoral Discernment Regime for Marriage (Civic-Pastoral Reading of the Sacrament)").
narrative_ontology:topic_domain(marriage_sacrament__civic_pastoral_reading, "religious doctrine / canon law / political sociology").

domain_priors:requires_active_enforcement(marriage_sacrament__civic_pastoral_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_sacrament__civic_pastoral_reading, '17010782-2cf9-47f4-bb10-5d0f3ae7ca4d').
narrative_ontology:cs_kernel_codification('17010782-2cf9-47f4-bb10-5d0f3ae7ca4d', fixed_text).
narrative_ontology:cs_authority_grounding('17010782-2cf9-47f4-bb10-5d0f3ae7ca4d', lineage).
narrative_ontology:cs_interpretation_layer_present('17010782-2cf9-47f4-bb10-5d0f3ae7ca4d').
narrative_ontology:cs_reading_relation('17010782-2cf9-47f4-bb10-5d0f3ae7ca4d', marriage_sacrament__hierarchical_indissolubility_reading, forecloses).
narrative_ontology:cs_axiom('17010782-2cf9-47f4-bb10-5d0f3ae7ca4d', foundational, indissolubility_aspirational_not_constitutive).
narrative_ontology:cs_axiom_status(indissolubility_aspirational_not_constitutive, holdable).
narrative_ontology:cs_axiom_grounding('17010782-2cf9-47f4-bb10-5d0f3ae7ca4d', indissolubility_aspirational_not_constitutive, theological).
narrative_ontology:cs_axiom('17010782-2cf9-47f4-bb10-5d0f3ae7ca4d', foundational, case_discernment_over_tribunal_rule).
narrative_ontology:cs_axiom_status(case_discernment_over_tribunal_rule, holdable).
narrative_ontology:cs_axiom_grounding('17010782-2cf9-47f4-bb10-5d0f3ae7ca4d', case_discernment_over_tribunal_rule, instrumental).
narrative_ontology:cs_reference_frame('17010782-2cf9-47f4-bb10-5d0f3ae7ca4d', indissolubility_as_pastoral_ideal).
narrative_ontology:cs_drift_state('17010782-2cf9-47f4-bb10-5d0f3ae7ca4d', post_amoris_implementation_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('17010782-2cf9-47f4-bb10-5d0f3ae7ca4d', '').
narrative_ontology:cs_kernel_id(marriage_sacrament__civic_pastoral_reading, marriage_sacrament).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_sacrament__civic_pastoral_reading, divorced_remarried_catholics).
narrative_ontology:constraint_beneficiary(marriage_sacrament__civic_pastoral_reading, diocesan_pastors).
narrative_ontology:constraint_beneficiary(marriage_sacrament__civic_pastoral_reading, roman_magisterium).
narrative_ontology:constraint_victim(marriage_sacrament__civic_pastoral_reading, traditional_catholics).
narrative_ontology:constraint_victim(marriage_sacrament__civic_pastoral_reading, abandoned_first_spouses).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(marriage_sacrament__civic_pastoral_reading, canon_law_tribunal_officials).
narrative_ontology:constraint_vindicates(marriage_sacrament__civic_pastoral_reading, pastoral_gradualism).
narrative_ontology:constraint_vindicates(marriage_sacrament__civic_pastoral_reading, internal_forum_discernment).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets the doctrinal frame through conciliar teaching and post-conciliar documents, issues implementation guidance for the discernment regime, and disciplines resistance within the institution, including restrictions on traditionalist liturgical communities. Collects continued membership from both progressive and traditional wings and the standing of merciful interpreter; absorbs the cost of authority eroded by uneven enforcement across dioceses. Its exit is reframing: it authored the regime and can reissue, clarify, or reframe it.
narrative_ontology:constraint_stakeholder(marriage_sacrament__civic_pastoral_reading, roman_magisterium, agenda_setter,
    institutional, generational, arbitrage, global).

% Conduct the case-by-case discernment the regime centers: accompanying couples in irregular situations, weighing admission to communion, applying diocesan norms that vary widely. Collects the discretionary adjudicative authority that previously sat with tribunals and fixed rules. Bears the burden of inconsistent norms across dioceses and of parishioners who expect either stricter or looser application than their pastor gives. Exit would mean leaving pastoral office or the institution.
narrative_ontology:constraint_stakeholder(marriage_sacrament__civic_pastoral_reading, diocesan_pastors, beneficiary,
    organized, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(marriage_sacrament__civic_pastoral_reading, diocesan_pastors, agenda_setter).

% Live in second unions after civil or religious divorce and seek sacramental communion. Under this regime they gain a case-by-case path back to communion — accompaniment, internal-forum discernment, or streamlined annulment of the first bond — where the prior discipline offered none. The path's availability varies by diocese and pastor. Their alternative is permanent exclusion from communion or leaving the church they belong to.
narrative_ontology:constraint_stakeholder(marriage_sacrament__civic_pastoral_reading, divorced_remarried_catholics, beneficiary,
    organized, biographical, constrained, global).

% Hold the settled doctrine of marital indissolubility as constitutive of their religious identity and liturgical life. Experience the discernment regime as relativization: teaching they understood as fixed made discretionary and applied unevenly across dioceses. Their opposition is organized — formal theological dubia, traditionalist communities, liturgical-attachment movements — and is met with management rather than adjudication. Leaving the mainstream institution would cost them the very continuity they understand themselves to be preserving; some exit to traditionalist jurisdictions at high personal and communal cost.
narrative_ontology:constraint_stakeholder(marriage_sacrament__civic_pastoral_reading, traditional_catholics, payer,
    organized, generational, identity_locked, global).

% Are the spouses of the first marriages that discernment and streamlined annulment set aside. They hold no formal standing in internal-forum processes conducted about their own bonds: the status of their marriage is re-determined without their participation, testimony, or consent. Many learn of the reclassification only after the fact.
narrative_ontology:constraint_stakeholder(marriage_sacrament__civic_pastoral_reading, abandoned_first_spouses, payer,
    powerless, biographical, trapped, national).

% Are the adult children of unions the regime reclassifies. Their family history is the object the process adjudicates, yet no process consults them; they learn that their parents' marriage has been declared null or set aside as a finished fact. Their objection — that their origin is being re-written without a hearing — has no institutional address.
narrative_ontology:constraint_stakeholder(marriage_sacrament__civic_pastoral_reading, adult_children_of_first_marriages, excluded,
    moderate, biographical, constrained, national).

% Staff the marriage tribunals that administer the formal nullity track running parallel to pastoral discernment. The 2015 streamlining multiplied their caseload and preserved their function inside the regime; their work is the documentary counterpart to the pastors' internal-forum discretion. Exit would mean leaving canonical office.
narrative_ontology:constraint_stakeholder(marriage_sacrament__civic_pastoral_reading, canon_law_tribunal_officials, beneficiary,
    organized, biographical, constrained, national).

% Study marriage, divorce, annulment, and retention patterns across Catholic populations. They document the gap between normative teaching and practice, the variance in implementation across dioceses and countries, and the demographic effects of each discipline. They neither collect from the regime nor bear its costs.
narrative_ontology:constraint_stakeholder(marriage_sacrament__civic_pastoral_reading, religious_sociologists, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(marriage_sacrament__civic_pastoral_reading, diocesan_pastors).
narrative_ontology:fixing_cost_class(marriage_sacrament__civic_pastoral_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Holds a strict communal ideal (marital indissolubility) and a mass reality of marital failure in one communion: the regime provides a case-by-case path — pastoral accompaniment, internal-forum discernment, streamlined annulment — that keeps divorced-and-remarried members in sacramental communion without formally renouncing the ideal. It solves the collective problem the hierarchical reading leaves unsolved: mass exclusion of millions of members versus formal doctrinal abandonment.
% TRANSFER_FUNCTION: Moves adjudicative discretion from canonical tribunals and fixed rules to pastors' case-by-case judgment; moves sacramental access to divorced-and-remarried Catholics; moves normative certainty away from the laity as a class (the diffuse cost traditional Catholics bear); moves interpretive legitimacy to the magisterium as the merciful reader of the tradition.
% ABSENT_VOICES: Adult children of set-aside first marriages have no seat in any discernment process; abandoned first spouses lack formal standing in internal-forum procedures conducted about their own bonds; Orthodox churches, whose parallel economy practice is implicitly cited by the regime's defenders, are engaged in no consultation; traditionalist theologians were heard (the 2016 dubia) but their formal corrections received no adjudication.
% DISAPPEARANCE_RATIONALE: Diocesan norms, tribunal workflows, communion discipline, parish accompaniment programs, and the organized resistance structures of traditionalist communities all presuppose the regime. If it vanished overnight, every divorced-and-remarried Catholic would face an immediate either/or — exclusion from communion or the hierarchical reading's formal process — and the institution would have to adopt one sibling reading explicitly rather than holding the discernment middle.
% FOUNDING_PROBLEM: By the late twentieth century, millions of divorced-and-remarried Catholics were living in formal exclusion from sacramental communion: the institution faced mass pastoral abandonment of its members if discipline held, or de facto doctrinal abandonment if communion were admitted. The regime was built to hold both — keep the indissolubility ideal and find a case-by-case path for the excluded.
% FOUNDING_PROBLEM_CORROBORATION: The problem's reality is corroborated from outside the benefiting parties: secular religious sociologists document the scale of exclusion through attendance and divorce-pattern studies; Orthodox canonists attest the parallel problem in their own communion; traditionalist theologians and the dubia cardinals — opponents of the remedy — attest the pastoral problem was real while disputing the solution. No party disputes that the founding problem existed; the parties dispute whether the regime solves it or dissolves the doctrine it was built to preserve.
narrative_ontology:disappearance_verdict(marriage_sacrament__civic_pastoral_reading, world_rearranges).
narrative_ontology:founding_problem_status(marriage_sacrament__civic_pastoral_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(marriage_sacrament__civic_pastoral_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(marriage_sacrament__civic_pastoral_reading, 'none', 1).
narrative_ontology:epsilon_provenance(marriage_sacrament__civic_pastoral_reading, 0.48, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(marriage_sacrament__civic_pastoral_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(marriage_sacrament__civic_pastoral_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(marriage_sacrament__civic_pastoral_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is moderate (0.48): the regime's transfer is real but diffuse — normative certainty moves away from the laity as a class while discretion accrues to pastors; this is not the concentrated rent of a pure extraction structure but the asymmetric cost of a hybrid. Suppression is low-moderate (0.32) and is authored as a raw structural property (the engine scales only extractiveness): the regime coerces no one into the pastoral reading; it manages dissent inside the institution — declining to adjudicate formal theological corrections, disciplining traditionalist liturgical communities — rather than blocking exit, which remains available at identity cost. Theater is moderate and rising (0.15 to 0.42): as implementation outpaces the case-by-case requirement, the indissolubility ideal is increasingly affirmed in language while suspended in practice, and the regime's own discernment requirement becomes the theatrical element. Accessibility collapse is moderate (0.45): alternatives exist (traditionalist jurisdictions, Eastern churches, plain exit) but each costs the mainstream continuity that traditional identity is constituted by. Resistance is substantial (0.55): organized theological opposition and liturgical-attachment movements, met with management rather than adjudication. All three series run on one shared time grid (t=0–60, i.e., 1965–2025) with every metric authored at every point. Receipt: the gains demonstrably accrue to diocesan_pastors, who collect the discretionary authority the regime's relativization creates. Fixing cost: the seat that could restore the hierarchical discipline (the magisterium) could do so only at prohibitive cost — forfeiting the progressive wing, reversing a decade of implementation, and risking schism — relative to the coherence benefit.
 *
 * PERSPECTIVAL GAP:
 *   The payer and beneficiary seats compute different types from the same structure. From diocesan pastors' seat the regime is the machinery of their ministry: discernment is what they do, and the discretion is real authority. From traditional Catholics' seat the same regime is relativization — settled teaching made discretionary and applied unevenly. From divorced-and-remarried Catholics' seat it is return: the difference between permanent exclusion and communion. From the magisterium's seat it is simultaneously gain (retention, the standing of merciful interpreter) and loss (authority eroded by its own inconsistent enforcement). The engine computes these divergences from power, exit, and directionality data; the authored claim does not adjudicate among them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries: divorced_remarried_catholics (d near the beneficiary end — the regime subsidizes their communion access), diocesan_pastors (collect the discretion), canon_law_tribunal_officials (retained and expanded function). Targets: traditional_catholics (pay normative clarity; identity_locked exit places them near the full-target end — they cannot arbitrage an identity home), abandoned_first_spouses (pay their bond's status; trapped — no standing in the process that re-determines it). One override: the institutional atom is set to d=0.30. The magisterium is the regime's role-beneficiary, but the structural account has it paying a material self-cost — institutional authority eroded by its own inconsistent enforcement — so the derivation from beneficiary declarations alone (~0.10) would overstate its net position; 0.30 places it between beneficiary and symmetric, matching the retention-gain versus legitimacy-loss structure. gain_flow names diocesan_pastors because the discretion vacated by relativized norms demonstrably lands there, case by case.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope classification prevents two opposite mislabels. Reading the regime as pure extraction (doctrinal integrity taken from traditional laity) misses its genuine coordination function: holding a strict ideal and mass marital failure in one communion is a real collective problem the hierarchical reading leaves unsolved, and the regime solves it without formally abandoning the ideal. Reading it as pure coordination (compassionate accommodation) misses the asymmetric burden: the clarity traditional Catholics pay was not a cost they consented to, and the discretion pastors collect is not returned to those who paid. Mandatrophy is not resolved: the founding problem — divorced-and-remarried Catholics outside communion — is live, and the regime manages it rather than having outlived it, so no sunset or piton dynamics apply. The drift to watch is theater_ratio, whose steady rise tracks the ideal's progressive theatricalization: the classic signature that precedes mandate atrophy if implementation continues outrunning the discernment requirement the regime names as its own justification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_structural_delta,
    'This constraint is one reading (civic_pastoral_reading) of the marriage_sacrament kernel; the disagreement with the sibling reading (hierarchical_indissolubility_reading) is located in two structural elements: the modal status of the bond (aspirational ideal vs constitutive reality) and the seat of adjudication (pastoral discernment vs hierarchical tribunal). Does instantiating the sibling relocate the victim set to divorced-and-remarried Catholics and raise epsilon from moderate to high, confirming the two readings as distinct constraints?',
    'Author the sibling reading as its own constraint story and compare computed per-seat classifications; the family decomposition is confirmed if the victim set and effective-extraction profile move as the declared structural delta predicts.',
    'If confirmed, the two readings are genuinely distinct constraints and cross-reading epsilon comparison is a category error; if not, the kernel is better modeled as one constraint with contested enforcement rather than two readings.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_structural_delta, conceptual, 'Kernel-reading delta: whether the sibling reading constitutes a separate constraint with its own victim set and epsilon.').

omega_variable(
    secular_accommodation_ambiguity,
    'Is the pastoral regime an internal development of the tradition''s own logic (mercy unfolding from within) or a constructed accommodation to secular divorce norms, purchasing institutional legitimacy at the cost of doctrinal integrity?',
    'Compare regime adoption and implementation depth across jurisdictions with different secular divorce regimes; if implementation depth tracks secular-legal pressure rather than internal theological argument, the accommodation reading is supported.',
    'If accommodation, part of the burden traditional laity bear is the price of an exchange (doctrinal integrity for secular legitimacy) and the regime''s coordination claim weakens toward pure extraction; if internal development, the burden is the cost of a genuine interpretive transition and the coordination function stands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(secular_accommodation_ambiguity, empirical, 'Whether the regime is endogenous doctrinal development or exogenous accommodation to secular norms.').

omega_variable(
    discernment_reality_ambiguity,
    'Is the discernment process a real adjudicative practice (structured accompaniment, documented internal-forum records) or theatrical cover for de facto permission, varying by diocese?',
    'Diocesan-level audit of implementation: existence of structured accompaniment paths, documented discernment records, variance in admission outcomes across pastors within comparable cases.',
    'If largely theatrical, the indissolubility language is maintained performatively while the operative rule is permission, raising theater_ratio further and dating a type transition toward piton dynamics; if real, the regime is a functioning hybrid coordination structure.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(discernment_reality_ambiguity, empirical, 'Whether the discernment machinery is functional or performative.').

omega_variable(
    relativization_intrinsic_cost_ambiguity,
    'Is the cost traditional Catholics bear (doctrinal relativization and loss of normative clarity) a burden imposed by this regime specifically, or the intrinsic price any living tradition''s development exacts from the partisans of the prior formulation?',
    'Counterfactual comparison: price the relativization cost the sibling reading imposes on progressive laity when it governs; if the costs are symmetric across readings (each side pays when the other governs), the cost is contest-structural rather than regime-specific.',
    'If intrinsic, the regime''s effective extraction drops and its classification moves toward rope; if regime-specific, the tangled_rope classification with traditional Catholics as the paying class stands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(relativization_intrinsic_cost_ambiguity, conceptual, 'Whether traditional-lay relativization is regime-specific extraction or the structural cost of the kernel contest itself.').

omega_variable(
    first_spouse_standing_ambiguity,
    'Is the abandoned first spouse''s lack of standing in discernment processes an incidental feature of internal-forum practice, or constitutive of the regime''s transfer function (discretion flowing to pastors requires the process to be non-adversarial and therefore to exclude the first spouse)?',
    'Compare jurisdictions where first spouses retain formal standing in tribunal process with internal-forum regimes; observe whether pastoral discretion and case throughput depend structurally on the first spouse''s absence.',
    'If constitutive, the burden on first spouses is structural and their declaration as a paying class is load-bearing for classification; if incidental, their harm is an implementation defect correctable without regime change.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(first_spouse_standing_ambiguity, empirical, 'Whether first-spouse exclusion is structural to the regime or incidental to its implementation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_sacrament__civic_pastoral_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(marr_tr_t0, marriage_sacrament__civic_pastoral_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(marr_tr_t10, marriage_sacrament__civic_pastoral_reading, theater_ratio, 10, 0.18).
narrative_ontology:measurement(marr_tr_t20, marriage_sacrament__civic_pastoral_reading, theater_ratio, 20, 0.22).
narrative_ontology:measurement(marr_tr_t30, marriage_sacrament__civic_pastoral_reading, theater_ratio, 30, 0.25).
narrative_ontology:measurement(marr_tr_t40, marriage_sacrament__civic_pastoral_reading, theater_ratio, 40, 0.3).
narrative_ontology:measurement(marr_tr_t50, marriage_sacrament__civic_pastoral_reading, theater_ratio, 50, 0.38).
narrative_ontology:measurement(marr_tr_t60, marriage_sacrament__civic_pastoral_reading, theater_ratio, 60, 0.42).

% Extraction over time
narrative_ontology:measurement(marr_be_t0, marriage_sacrament__civic_pastoral_reading, base_extractiveness, 0, 0.2).
narrative_ontology:measurement(marr_be_t10, marriage_sacrament__civic_pastoral_reading, base_extractiveness, 10, 0.26).
narrative_ontology:measurement(marr_be_t20, marriage_sacrament__civic_pastoral_reading, base_extractiveness, 20, 0.31).
narrative_ontology:measurement(marr_be_t30, marriage_sacrament__civic_pastoral_reading, base_extractiveness, 30, 0.35).
narrative_ontology:measurement(marr_be_t40, marriage_sacrament__civic_pastoral_reading, base_extractiveness, 40, 0.4).
narrative_ontology:measurement(marr_be_t50, marriage_sacrament__civic_pastoral_reading, base_extractiveness, 50, 0.46).
narrative_ontology:measurement(marr_be_t60, marriage_sacrament__civic_pastoral_reading, base_extractiveness, 60, 0.48).

% Suppression requirement over time
narrative_ontology:measurement(marr_su_t0, marriage_sacrament__civic_pastoral_reading, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(marr_su_t10, marriage_sacrament__civic_pastoral_reading, suppression_requirement, 10, 0.22).
narrative_ontology:measurement(marr_su_t20, marriage_sacrament__civic_pastoral_reading, suppression_requirement, 20, 0.25).
narrative_ontology:measurement(marr_su_t30, marriage_sacrament__civic_pastoral_reading, suppression_requirement, 30, 0.27).
narrative_ontology:measurement(marr_su_t40, marriage_sacrament__civic_pastoral_reading, suppression_requirement, 40, 0.29).
narrative_ontology:measurement(marr_su_t50, marriage_sacrament__civic_pastoral_reading, suppression_requirement, 50, 0.31).
narrative_ontology:measurement(marr_su_t60, marriage_sacrament__civic_pastoral_reading, suppression_requirement, 60, 0.32).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_sacrament__civic_pastoral_reading, identity_coordination).
narrative_ontology:affects_constraint(marriage_sacrament__civic_pastoral_reading, marriage_sacrament__hierarchical_indissolubility_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'Catholic marriage doctrine' decomposes into two structurally distinct constraints per the epsilon-invariance principle: this file (civic_pastoral_reading — indissolubility as ideal with case-by-case discernment; moderate extraction; paying classes = traditional Catholics bearing relativization and first spouses lacking standing) and marriage_sacrament__hierarchical_indissolubility_reading (bond constitutive, hierarchical adjudication; high extraction for divorced-and-remarried Catholics excluded from communion). The hierarchical reading was the standing arrangement for centuries and is upstream: the civic-pastoral reading emerged as its modification and now creates downstream pressure on it — its adoption erodes the hierarchical regime's enforcement conditions and legitimacy — while the hierarchical reading's persistence supplies the organized resistance this regime manages. Measuring 'the marriage constraint' across both readings yields observer-dependent epsilon; each file holds one stable epsilon, and the family link records the dependency.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(marriage_sacrament__civic_pastoral_reading, institutional, 0.3).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
