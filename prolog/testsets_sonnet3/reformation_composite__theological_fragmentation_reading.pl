% ============================================================================
% CONSTRAINT STORY: reformation_composite__theological_fragmentation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_reformation_composite__theological_fragmentation_reading, []).

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
 *   constraint_id: reformation_composite__theological_fragmentation_reading
 *   human_readable: Confessional Doctrine as Denominational Boundary Constraint (Theological Reading)
 *   domain: religious/historical/institutional
 *
 * SUMMARY:
 *   This story is the theological-fragmentation reading of the Reformation
 *   kernel: it treats competing soteriological claims (justification by faith
 *   alone vs. faith and works) and ecclesiological claims (papal-episcopal
 *   succession vs. congregational or conciliar authority) as the primary
 *   causal engine, with denominational institutions as the structural
 *   residue. Confessional documents (Augsburg, Trent, Westminster, Dort) are
 *   read as constraint artifacts that fix doctrinal boundaries and license
 *   disciplinary enforcement — excommunication, exile, execution — against
 *   those who fall outside them. This reading does NOT claim the political or
 *   print-technology dynamics were absent; it claims that AS A THEOLOGICAL
 *   MATTER the doctrinal incompatibilities were real, load-bearing, and
 *   sufficient on their own terms to generate structurally incompatible
 *   denominations, and it holds ε fixed to that claim rather than averaging
 *   across readings.
 *
 * KEY AGENTS:
 *   - confessional_clergy_hierarchies: primary agenda-setters — draft and enforce the doctrinal boundary (institutional/arbitrage)
 *   - denominational_leadership: primary beneficiary — institutional survival depends on the boundary holding (organized/constrained)
 *   - cross_confessional_laity and dissenting_minority_sects: primary targets — bear exile, execution, exclusion from sacraments (powerless/trapped)
 *   - ecumenical_reformers: excluded voice — sought doctrinal reconciliation, marginalized by both confessionalizing sides
 *   - comparative_historians: analytical observer
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(reformation_composite__theological_fragmentation_reading, 0.58).
domain_priors:suppression_score(reformation_composite__theological_fragmentation_reading, 0.71).
domain_priors:theater_ratio(reformation_composite__theological_fragmentation_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(reformation_composite__theological_fragmentation_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(reformation_composite__theological_fragmentation_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(reformation_composite__theological_fragmentation_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(reformation_composite__theological_fragmentation_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(reformation_composite__theological_fragmentation_reading, resistance, 0.74).

% --- Constraint claim ---
narrative_ontology:constraint_claim(reformation_composite__theological_fragmentation_reading, tangled_rope).
narrative_ontology:human_readable(reformation_composite__theological_fragmentation_reading, "Confessional Doctrine as Denominational Boundary Constraint (Theological Reading)").
narrative_ontology:topic_domain(reformation_composite__theological_fragmentation_reading, "religious/historical/institutional").

domain_priors:requires_active_enforcement(reformation_composite__theological_fragmentation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(reformation_composite__theological_fragmentation_reading, '32866ddf-da08-47b7-85d8-16296647cdab').
narrative_ontology:cs_kernel_codification('32866ddf-da08-47b7-85d8-16296647cdab', formalized).
narrative_ontology:cs_authority_grounding('32866ddf-da08-47b7-85d8-16296647cdab', lineage).
narrative_ontology:cs_interpretation_layer_present('32866ddf-da08-47b7-85d8-16296647cdab').
narrative_ontology:cs_reading_relation('32866ddf-da08-47b7-85d8-16296647cdab', reformation_composite__political_realignment_reading, coexists_with).
narrative_ontology:cs_reading_relation('32866ddf-da08-47b7-85d8-16296647cdab', reformation_composite__technological_mediation_reading, coexists_with).
narrative_ontology:cs_axiom('32866ddf-da08-47b7-85d8-16296647cdab', foundational, soteriological_claims_are_truth_apt_and_mutually_exclusive).
narrative_ontology:cs_axiom_status(soteriological_claims_are_truth_apt_and_mutually_exclusive, holdable).
narrative_ontology:cs_axiom_grounding('32866ddf-da08-47b7-85d8-16296647cdab', soteriological_claims_are_truth_apt_and_mutually_exclusive, deontological).
narrative_ontology:cs_axiom('32866ddf-da08-47b7-85d8-16296647cdab', foundational, confessional_subscription_is_the_legitimate_mechanism_for_adjudicating_doctrinal_truth).
narrative_ontology:cs_axiom_status(confessional_subscription_is_the_legitimate_mechanism_for_adjudicating_doctrinal_truth, holdable).
narrative_ontology:cs_axiom_grounding('32866ddf-da08-47b7-85d8-16296647cdab', confessional_subscription_is_the_legitimate_mechanism_for_adjudicating_doctrinal_truth, conventional).
narrative_ontology:cs_reference_frame('32866ddf-da08-47b7-85d8-16296647cdab', single_apostolic_communion_doctrinal_unity).
narrative_ontology:cs_drift_state('32866ddf-da08-47b7-85d8-16296647cdab', peace_of_westphalia_1648, gap(codification_collapse, severe, true)).
narrative_ontology:cs_created_at('32866ddf-da08-47b7-85d8-16296647cdab', '').
narrative_ontology:cs_kernel_id(reformation_composite__theological_fragmentation_reading, reformation_composite).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(reformation_composite__theological_fragmentation_reading, confessional_clergy_hierarchies).
narrative_ontology:constraint_beneficiary(reformation_composite__theological_fragmentation_reading, denominational_leadership).
narrative_ontology:constraint_beneficiary(reformation_composite__theological_fragmentation_reading, theological_faculties).
narrative_ontology:constraint_victim(reformation_composite__theological_fragmentation_reading, cross_confessional_laity).
narrative_ontology:constraint_victim(reformation_composite__theological_fragmentation_reading, dissenting_minority_sects).
narrative_ontology:constraint_victim(reformation_composite__theological_fragmentation_reading, religiously_mixed_households).
narrative_ontology:constraint_vindicates(reformation_composite__theological_fragmentation_reading, sola_fide_doctrine).
narrative_ontology:constraint_vindicates(reformation_composite__theological_fragmentation_reading, sola_scriptura_doctrine).
narrative_ontology:constraint_vindicates(reformation_composite__theological_fragmentation_reading, apostolic_succession_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Draft and defend confessional documents (Augsburg Confession, Canons of Trent, Westminster Confession) that fix which soteriological claims are salvific and which are damnable error. Administer excommunication, ordination, and sacramental gatekeeping on the basis of subscription to these documents. Their institutional survival depends on the doctrinal boundary remaining sharp and enforced.
narrative_ontology:constraint_stakeholder(reformation_composite__theological_fragmentation_reading, confessional_clergy_hierarchies, agenda_setter,
    institutional, civilizational, arbitrage, continental).

% Build congregations, schools, and political alliances on the premise that their soteriological reading is the correct one and rival readings are structurally incompatible with salvation. Fragmentation into distinct denominations gives them a defensible market of adherents and a rationale for continued separate existence rather than reconciliation.
narrative_ontology:constraint_stakeholder(reformation_composite__theological_fragmentation_reading, denominational_leadership, beneficiary,
    organized, generational, constrained, national).

% Train clergy in the fine distinctions between justification-by-faith-alone and faith-plus-works, between real presence and memorial symbolism, between episcopal and congregational polity. Their disciplinary expertise and institutional funding depend on these distinctions being treated as irreconcilable rather than as differences of emphasis within a shared tradition.
narrative_ontology:constraint_stakeholder(reformation_composite__theological_fragmentation_reading, theological_faculties, beneficiary,
    organized, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(reformation_composite__theological_fragmentation_reading, theological_faculties, agenda_setter).

% Live in territories where the ruler's or majority's confession determines legal religious practice, access to sacraments, marriage validity, and burial rites. Cannot simply hold a personal synthesis of doctrines without being classed as heretical by at least one hierarchy; must choose a side or hide belief, often at cost to family, property, or safety.
narrative_ontology:constraint_stakeholder(reformation_composite__theological_fragmentation_reading, cross_confessional_laity, payer,
    powerless, biographical, trapped, local).

% Anabaptists, Socinians, and other groups whose soteriology or ecclesiology falls outside the range the major confessions treat as tolerable variation. Face execution, exile, or forced conversion under both Protestant and Catholic authorities precisely because the doctrinal boundary-drawing treats their positions as beyond the pale rather than as one more variant.
narrative_ontology:constraint_stakeholder(reformation_composite__theological_fragmentation_reading, dissenting_minority_sects, payer,
    powerless, biographical, trapped, regional).

% Spouses or kin who hold differing confessional commitments must navigate incompatible baptismal, marital, and funerary requirements. The doctrinal incompatibility that clergy insist is theologically necessary translates directly into practical family rupture, inheritance disputes, and custody conflicts over children's confession.
narrative_ontology:constraint_stakeholder(reformation_composite__theological_fragmentation_reading, religiously_mixed_households, payer,
    powerless, biographical, constrained, local).

% Figures like Melanchthon, Bucer, and later Calixtus who sought doctrinal compromise (the Colloquy of Marburg, Leipzig Interim, syncretist proposals) were marginalized by the confessionalizing hierarchies on both sides, whose institutional survival depended on treating the disputed articles as non-negotiable rather than reconcilable.
narrative_ontology:constraint_stakeholder(reformation_composite__theological_fragmentation_reading, ecumenical_reformers, excluded,
    moderate, generational, constrained, continental).

% Examine confessional documents, consistory records, and disciplinary case files to assess whether the doctrinal differences were theologically load-bearing or served as post-hoc justification for institutional and political separations already underway for other reasons.
narrative_ontology:constraint_stakeholder(reformation_composite__theological_fragmentation_reading, comparative_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(reformation_composite__theological_fragmentation_reading, denominational_leadership).
narrative_ontology:fixing_cost_class(reformation_composite__theological_fragmentation_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Confessional documents genuinely coordinate belief and practice within a denomination: they let clergy, laity, and institutions share a stable doctrinal vocabulary, standardize catechesis, and adjudicate disputes without re-litigating first principles in every congregation.
% TRANSFER_FUNCTION: The arrangement moves religious authority, sacramental legitimacy, and communal belonging away from anyone whose belief falls outside a given confession's boundary, and concentrates interpretive authority and institutional loyalty within the hierarchy that drew that boundary. In mixed and disputed territories it also moves security and property away from dissenting minorities toward the locally dominant confession.
% ABSENT_VOICES: Ecumenical reformers who argued the soteriological gap was bridgeable were structurally excluded from confession-drafting once each side's institutional identity came to depend on the gap remaining unbridged; lay believers holding syncretic or undecided views had no seat in doctrinal councils at all.
% DISAPPEARANCE_RATIONALE: If the doctrinal boundary-drawing function disappeared overnight — if soteriological and ecclesiological disputes were treated as reconcilable variants within one communion — the entire apparatus of separate denominational hierarchies, competing seminaries, confessional subscription requirements, and cross-confessional legal disabilities would lose its rationale; church property, clerical employment, and confessional schooling would have to reorganize around a unified or federated structure.
% FOUNDING_PROBLEM: Genuine, structurally distinct soteriological claims emerged (justification by faith alone vs. faith and works; consubstantiation/memorialism vs. transubstantiation; conciliar/congregational vs. papal-episcopal authority) that could not be simultaneously true as stated, and communities needed some way to determine which sacramental and pastoral practices were valid.
% FOUNDING_PROBLEM_CORROBORATION: Historical theologians within each tradition attest the doctrinal disputes are live and substantive (not resolved, not cosmetic). Comparative historians and some contemporary ecumenical bodies (e.g., the Lutheran-Catholic Joint Declaration on Justification, 1999) attest that at least some of the founding disputes have since been judged reconcilable in substance, suggesting the boundary persisted institutionally beyond what the theological gap alone required — corroboration exists on both sides of the contest, which is itself the signal that this founding-problem status is genuinely unsettled rather than settled and obscured.
narrative_ontology:disappearance_verdict(reformation_composite__theological_fragmentation_reading, world_rearranges).
narrative_ontology:founding_problem_status(reformation_composite__theological_fragmentation_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(reformation_composite__theological_fragmentation_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(reformation_composite__theological_fragmentation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(reformation_composite__theological_fragmentation_reading, 0.58, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(reformation_composite__theological_fragmentation_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(reformation_composite__theological_fragmentation_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(reformation_composite__theological_fragmentation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises from 0.22 at Luther's 95 Theses (largely a call for reform within one communion, low institutional lock-in yet) to 0.58 by Westphalia (1648), as confessionalization hardens doctrinal subscription into a precondition for legal existence, marriage validity, and property rights. Suppression tracks the same arc but peaks earlier and higher (0.80 at 1618, the eve of the Thirty Years' War) because active doctrinal policing — consistory courts, inquisitorial tribunals, forced conversions — intensifies before and during confrontation, then eases slightly post-Westphalia as territorial cuius regio settlement reduces (without eliminating) cross-border enforcement pressure. Theater ratio rises steadily (0.10 to 0.42) as confessional subscription increasingly functions as a loyalty marker and political-alliance signal distinct from its original doctrinal-adjudication function — a genuine Goodhart drift within this reading's own terms.
 *
 * DIRECTIONALITY LOGIC:
 *   Confessional clergy hierarchies and theological faculties sit at the beneficiary end: they administer the boundary and their institutional identity is constituted by its persistence (d low). Denominational leadership benefits similarly through congregational and political capture. Cross-confessional laity, dissenting minority sects, and mixed households sit at the target end: trapped or constrained exit, bearing exile, legal disability, or family rupture as the direct cost of the doctrinal boundary being enforced rather than negotiated (d high). Ecumenical reformers are excluded rather than coordinated — their proposed synthesis was foreclosed by the same institutional logic that made confessional hardening beneficial to the hierarchies on both sides.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding theological disputes (real soteriological and ecclesiological disagreement) were genuinely live circa 1517-1560. By the time of Westphalia, much of the enforcement apparatus persists not because the doctrinal dispute remains as sharply contested among ordinary believers as among specialists, but because denominational institutions have built identity, property, and political alliance on the boundary. The tangled_rope classification captures this: a real coordination function (shared doctrinal vocabulary, disciplinary predictability within a communion) persists alongside genuine, asymmetric extraction (exile and violence borne by dissenters and cross-confessional laity) requiring active enforcement (consistory courts, state-backed religious tests) to hold.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    theological_vs_political_primacy,
    'Were the soteriological and ecclesiological disputes the actual generative cause of denominational fragmentation, or were they the vocabulary in which pre-existing political and economic separations were expressed?',
    'Comparative case analysis of territories where political incentives for religious differentiation were weak or absent (e.g., regions with strong existing political unity) to see whether theological dispute alone produced comparable institutional fragmentation without a political driver.',
    'If theological content is shown to be largely epiphenomenal to political sovereignty assertions, this reading''s claimed primacy weakens substantially and the political_realignment_reading becomes the better fit for the same historical material; ε for THIS reading would not change (it is authored from this reading''s own lights) but confidence in the reading''s descriptive adequacy would.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(theological_vs_political_primacy, conceptual, 'Whether doctrine is generative cause or expressive vocabulary for political fragmentation.').

omega_variable(
    doctrinal_incommensurability_reality,
    'Were the core soteriological claims (sola fide vs. faith-plus-works; real presence variants) genuinely logically incompatible, or were they reconcilable positions that institutional actors chose to treat as incompatible for boundary-maintenance purposes?',
    'Examine subsequent ecumenical reconciliation documents (e.g., the 1999 Lutheran-Catholic Joint Declaration on Justification) that found substantial doctrinal agreement was possible after centuries of treating the positions as irreconcilable; assess whether 16th-century framings overstated the incompatibility relative to what later, less institutionally-invested theology found.',
    'If the disputes were genuinely reconcilable and treated as incompatible for institutional reasons, the coordination function claimed for confessional documents is substantially cover for extraction, pushing this reading toward snare; if genuinely incommensurable at the time, the tangled_rope reading (real coordination function plus real extraction) holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(doctrinal_incommensurability_reality, empirical, 'Whether the doctrinal gap was real incommensurability or constructed boundary-maintenance.').

omega_variable(
    beneficiary_capture_timing,
    'At what point did denominational leadership''s interest in maintained fragmentation begin to exceed the theological community''s interest in doctrinal accuracy — i.e., when did the founding theological problem stop being the operative driver and institutional self-preservation take over?',
    'Track disciplinary case records and confessional revision history for evidence of doctrinal positions hardening or softening in response to institutional/political pressure versus genuine theological argument, particularly around major territorial settlements (Peace of Augsburg 1555, Westphalia 1648).',
    'An early capture date would support a stronger mandatrophy reading (the founding problem was largely resolved or reframed well before 1648, and the persisting apparatus is substantially inertial/extractive); a late or absent capture date would support treating the extraction as continuously theologically-grounded rather than institutionally opportunistic.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(beneficiary_capture_timing, empirical, 'Timing of institutional capture relative to genuine theological dispute.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(reformation_composite__theological_fragmentation_reading, 1517, 1648).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(refo_tr_t1517, reformation_composite__theological_fragmentation_reading, theater_ratio, 1517, 0.1).
narrative_ontology:measurement(refo_tr_t1540, reformation_composite__theological_fragmentation_reading, theater_ratio, 1540, 0.2).
narrative_ontology:measurement(refo_tr_t1560, reformation_composite__theological_fragmentation_reading, theater_ratio, 1560, 0.3).
narrative_ontology:measurement(refo_tr_t1580, reformation_composite__theological_fragmentation_reading, theater_ratio, 1580, 0.36).
narrative_ontology:measurement(refo_tr_t1618, reformation_composite__theological_fragmentation_reading, theater_ratio, 1618, 0.4).
narrative_ontology:measurement(refo_tr_t1648, reformation_composite__theological_fragmentation_reading, theater_ratio, 1648, 0.42).

% Extraction over time
narrative_ontology:measurement(refo_be_t1517, reformation_composite__theological_fragmentation_reading, base_extractiveness, 1517, 0.22).
narrative_ontology:measurement(refo_be_t1540, reformation_composite__theological_fragmentation_reading, base_extractiveness, 1540, 0.38).
narrative_ontology:measurement(refo_be_t1560, reformation_composite__theological_fragmentation_reading, base_extractiveness, 1560, 0.48).
narrative_ontology:measurement(refo_be_t1580, reformation_composite__theological_fragmentation_reading, base_extractiveness, 1580, 0.53).
narrative_ontology:measurement(refo_be_t1618, reformation_composite__theological_fragmentation_reading, base_extractiveness, 1618, 0.61).
narrative_ontology:measurement(refo_be_t1648, reformation_composite__theological_fragmentation_reading, base_extractiveness, 1648, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(refo_su_t1517, reformation_composite__theological_fragmentation_reading, suppression_requirement, 1517, 0.35).
narrative_ontology:measurement(refo_su_t1540, reformation_composite__theological_fragmentation_reading, suppression_requirement, 1540, 0.55).
narrative_ontology:measurement(refo_su_t1560, reformation_composite__theological_fragmentation_reading, suppression_requirement, 1560, 0.68).
narrative_ontology:measurement(refo_su_t1580, reformation_composite__theological_fragmentation_reading, suppression_requirement, 1580, 0.72).
narrative_ontology:measurement(refo_su_t1618, reformation_composite__theological_fragmentation_reading, suppression_requirement, 1618, 0.8).
narrative_ontology:measurement(refo_su_t1648, reformation_composite__theological_fragmentation_reading, suppression_requirement, 1648, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(reformation_composite__theological_fragmentation_reading, reformation_composite__political_realignment_reading).
narrative_ontology:affects_constraint(reformation_composite__theological_fragmentation_reading, reformation_composite__technological_mediation_reading).

% DUAL FORMULATION NOTE:
% This story is one of three sibling readings of the reformation_composite kernel, each authored as a separate ε-invariant constraint per the ε-invariance principle. The theological_fragmentation_reading treats doctrinal incompatibility as the primary generative cause (this file); political_realignment_reading treats nation-state sovereignty assertion as primary; technological_mediation_reading treats print-mediated mass movement dynamics as primary. Each carries its own ε, beneficiary/victim structure, and classification assessed by its own lights over the same underlying historical arrangement. They are linked here rather than merged because merging would either force an artificial average ε across genuinely distinct causal claims or silently privilege one reading's framing as the sole truth.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
