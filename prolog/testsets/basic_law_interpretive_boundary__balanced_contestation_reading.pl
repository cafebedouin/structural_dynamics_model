% ============================================================================
% CONSTRAINT STORY: basic_law_interpretive_boundary__balanced_contestation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_basic_law_interpretive_boundary__balanced_contestation_reading, []).

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
 *   constraint_id: basic_law_interpretive_boundary__balanced_contestation_reading
 *   human_readable: Institutional Interpretive Boundary: Balanced Contestation Reading (Israeli Constitutional Framework)
 *   domain: constitutional_law/judicial_review/comparative_constitutionalism
 *
 * SUMMARY:
 *   Israel's Basic Laws create a constitutional framework without a single
 *   written Constitution. The state operates under a foundational ambiguity:
 *   are Basic Laws supreme law that courts must enforce, or can the Knesset
 *   amend them like ordinary legislation? The balanced-contestation reading
 *   asserts both—a difficult institutional equilibrium where courts interpret
 *   constitutional boundaries authoritatively, but legislatures retain
 *   ultimate amendment power at higher procedural cost. Neither institution
 *   is fully dominant; both claim legitimacy; the constraint persists through
 *   mutual forbearance and institutional dialogue rather than hierarchy. This
 *   is one reading of a contested kernel. The competing readings—judicial
 *   supremacy (courts enforce Basic Laws as supreme) and parliamentary
 *   sovereignty (Knesset retains unilateral amendment authority)—structure a
 *   triadic constitutional negotiation between the three branches.
 *
 * KEY AGENTS:
 *   - Supreme Court: interprets Basic Laws within its declared jurisdictional domain; invalidates ordinary legislation violating constitutional boundaries; maintains institutional prestige but faces political pressure from majoritarian coalitions
 *   - Knesset: retains ultimate amendment authority (can amend Basic Laws at higher procedural cost); constrained by judicial review but sovereign over policy space within constitutional boundaries
 *   - Executive: navigates interplay between court and legislature; absorbs costs from both directions; no independent constitutional authority in this reading
 *   - Individual challengers with minoritarian claims: depend entirely on court for remedy; have no other institutional avenue if court declines jurisdiction
 *   - Parliamentary minorities: gain leverage through court's interpretive authority; benefit when courts constrain majority legislation but are not direct beneficiaries
 *   - International human rights bodies: monitor through treaty obligations; influence both institutions through soft-law pressure; incorporated into the constraint as limits on legislative sovereignty
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(basic_law_interpretive_boundary__balanced_contestation_reading, 0.48).
domain_priors:suppression_score(basic_law_interpretive_boundary__balanced_contestation_reading, 0.31).
domain_priors:theater_ratio(basic_law_interpretive_boundary__balanced_contestation_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(basic_law_interpretive_boundary__balanced_contestation_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(basic_law_interpretive_boundary__balanced_contestation_reading, suppression_requirement, 0.31).
narrative_ontology:constraint_metric(basic_law_interpretive_boundary__balanced_contestation_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(basic_law_interpretive_boundary__balanced_contestation_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(basic_law_interpretive_boundary__balanced_contestation_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(basic_law_interpretive_boundary__balanced_contestation_reading, tangled_rope).
narrative_ontology:human_readable(basic_law_interpretive_boundary__balanced_contestation_reading, "Institutional Interpretive Boundary: Balanced Contestation Reading (Israeli Constitutional Framework)").
narrative_ontology:topic_domain(basic_law_interpretive_boundary__balanced_contestation_reading, "constitutional_law/judicial_review/comparative_constitutionalism").

domain_priors:requires_active_enforcement(basic_law_interpretive_boundary__balanced_contestation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(basic_law_interpretive_boundary__balanced_contestation_reading, '1f1b73ec-761f-4c6d-93fa-6039254903ab').
narrative_ontology:cs_kernel_codification('1f1b73ec-761f-4c6d-93fa-6039254903ab', distributed).
narrative_ontology:cs_authority_grounding('1f1b73ec-761f-4c6d-93fa-6039254903ab', extraction).
narrative_ontology:cs_interpretation_layer_present('1f1b73ec-761f-4c6d-93fa-6039254903ab').
narrative_ontology:cs_reading_relation('1f1b73ec-761f-4c6d-93fa-6039254903ab', basic_law_interpretive_boundary__judicial_supremacy_reading, coexists_with).
narrative_ontology:cs_reading_relation('1f1b73ec-761f-4c6d-93fa-6039254903ab', basic_law_interpretive_boundary__parliamentary_sovereignty_reading, coexists_with).
narrative_ontology:cs_axiom('1f1b73ec-761f-4c6d-93fa-6039254903ab', foundational, both_institutions_hold_legitimate_bounded_authority).
narrative_ontology:cs_axiom_status(both_institutions_hold_legitimate_bounded_authority, holdable).
narrative_ontology:cs_axiom_grounding('1f1b73ec-761f-4c6d-93fa-6039254903ab', both_institutions_hold_legitimate_bounded_authority, conventional).
narrative_ontology:cs_axiom('1f1b73ec-761f-4c6d-93fa-6039254903ab', foundational, constitutional_dialogue_requires_mutual_forbearance).
narrative_ontology:cs_axiom_status(constitutional_dialogue_requires_mutual_forbearance, holdable).
narrative_ontology:cs_axiom_grounding('1f1b73ec-761f-4c6d-93fa-6039254903ab', constitutional_dialogue_requires_mutual_forbearance, instrumental).
narrative_ontology:cs_axiom('1f1b73ec-761f-4c6d-93fa-6039254903ab', secondary, neither_institution_can_unilaterally_resolve_interpretation).
narrative_ontology:cs_axiom_status(neither_institution_can_unilaterally_resolve_interpretation, overridden).
narrative_ontology:cs_axiom_grounding('1f1b73ec-761f-4c6d-93fa-6039254903ab', neither_institution_can_unilaterally_resolve_interpretation, empirically_contingent).
narrative_ontology:cs_reference_frame('1f1b73ec-761f-4c6d-93fa-6039254903ab', dual_institutional_legitimacy).
narrative_ontology:cs_drift_state('1f1b73ec-761f-4c6d-93fa-6039254903ab', contemporary_judicial_overhaul_era, gap(authority_erosion, substantial, true)).
narrative_ontology:cs_created_at('1f1b73ec-761f-4c6d-93fa-6039254903ab', '').
narrative_ontology:cs_kernel_id(basic_law_interpretive_boundary__balanced_contestation_reading, basic_law_interpretive_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(basic_law_interpretive_boundary__balanced_contestation_reading, judicial_institutional_authority).
narrative_ontology:constraint_beneficiary(basic_law_interpretive_boundary__balanced_contestation_reading, legislative_institutional_authority).
narrative_ontology:constraint_beneficiary(basic_law_interpretive_boundary__balanced_contestation_reading, rule_of_law_doctrine).
narrative_ontology:constraint_victim(basic_law_interpretive_boundary__balanced_contestation_reading, individual_challengers_with_minoritarian_claims).
narrative_ontology:constraint_victim(basic_law_interpretive_boundary__balanced_contestation_reading, parliamentary_minorities).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(basic_law_interpretive_boundary__balanced_contestation_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(basic_law_interpretive_boundary__balanced_contestation_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(basic_law_interpretive_boundary__balanced_contestation_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(basic_law_interpretive_boundary__balanced_contestation_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(basic_law_interpretive_boundary__balanced_contestation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.48 at 2024) and rising from 1992 (0.35) because the constraint increasingly concentrates interpretive authority in courts and gate-keeps access to remedy through litigation. Early in the interval (1992), the institutional boundary was loosely defined; by 2024, both institutions have explicitly articulated their competing claims and the court has intervened more frequently in contested domains (security, religious affairs, electoral boundaries). The rise reflects not a shift toward pure extraction but accumulation of disputed cases where the court's interpretive authority prevents legislative majorities from implementing their policy agendas. Suppression is low (0.31) because neither institution suppresses the other's legitimacy claims—both remain accepted as legitimate; instead, the constraint operates through institutional friction and negotiated boundary revision. Theater ratio rises from 0.25 to 0.42 because an increasing share of institutional activity is procedural and performative: briefing international bodies, issuing public justifications for institutional positions, producing scholarly commentary—the real function (constitutional interpretation) has become inseparable from the theatrical defense of institutional legitimacy. The measurement grid is aligned: every metric is authored at the six shared time points (1992, 2000, 2008, 2016, 2020, 2024) spanning the interval, and each basis is marked 'observed' because the constraint has been historically documented and measured through case law, legislative votes, and institutional records.
 *
 * PERSPECTIVAL GAP:
 *   From the Court's seat, the arrangement is constitutional dialogue in service of rule of law: the Court maintains boundaries and the legislature amends them when it chooses, preserving the constraint as a cooperative institutional relationship. From legislative majority seats (right-wing coalitions in recent years), the arrangement is judicial overreach: courts invalidating elected-majority policies on constitutional grounds amounts to unelected officials overturning electoral mandates. From the executive's seat, the constraint is institutional friction: the executive must satisfy both the Court's interpretive requirements and the Knesset's policy mandates, which increasingly diverge. From minoritarian-claim seats, the constraint is mixed: courts offer remedy but access is litigious and uncertain; a sympathetic court is the only institutional recourse, but courts are not obligated to accept jurisdiction. These divergent perspectives should compute to different types in the engine: a Court-seat Tangled Rope (coordinating constitutional boundaries while extracting interpretive authority), a legislative-majority Snare (constrained from implementing elected mandates), an executive Tangled Rope (both coordinated and extracted from), a minoritarian-claim Snare (trapped with uncertain access to remedy). The authored claim is Tangled Rope because it asserts both coordination and asymmetric extraction are structurally present.
 *
 * DIRECTIONALITY LOGIC:
 *   The Court benefits from the constraint by retaining interpretive authority and institutional prestige (d near beneficiary end, ~0.25). The Knesset is ambiguously positioned: as an institution it retains ultimate amendment power (near symmetric, ~0.50) but legislative majorities face constraints from Court invalidation (higher d toward target, ~0.65). The executive is purely targeted by the constraint's friction (d near full target, ~0.75). Individual challengers with minoritarian claims are maximally targeted because they depend entirely on the Court's discretionary jurisdiction (d near full target, ~0.90). Parliamentary minorities benefit from Court protection of their interests, creating the most beneficiary-like directionality in the system (d near beneficiary, ~0.20). The reading as a whole treats this as asymmetric extraction (Court and minorities benefit at the expense of legislative majorities and the executive), sustained by the legitimacy of both Court and Knesset, which allows it to avoid full-snare classification. Directionality overrides are not needed because the structural data (beneficiary/victim declarations, exit options, power atoms) derives the correct d values.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem is live—the ambiguity between Basic-Law supremacy and parliamentary amendment authority persists and drives institutional contestation. The constraint itself was founded in the 1992 Constitutional Revolution (cases establishing judicial review of legislation against Basic Laws) to prevent legislative majorities from abolishing constitutional protections. This function remains live. However, there is a secondary drift: the constraint increasingly operates as a tool for judicial veto of any legislation a Court majority deems unconstitutional, not merely a safeguard against abolition. The theater ratio rising from 0.25 to 0.42 captures this: the Court increasingly justifies its decisions in terms of constitutional dialogue and democratic legitimacy, yet the decisions themselves do not follow from negotiated boundaries—they are assertions by the Court. This is not mandatrophy (the founding function has not died) but boundary expansion (the Court has expanded what counts as constitutional violation). The balanced-contestation reading assumes this boundary expansion can be negotiated and corrected by legislative amendment, which is the reading's most vulnerable assumption. The 2023–2024 Judicial Overhaul suggests the Knesset no longer believes that assumption.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    institutional_legitimacy_contestation,
    'What grounds the simultaneous legitimacy of both Court and Knesset in interpreting constitutional boundaries, given that both cannot be fully supreme?',
    'Examination of how each institution justifies its authority (Court: constitutional text and rule of law; Knesset: electoral legitimacy and sovereignty) and where these justifications would conflict in concrete cases; comparison to comparative constitutional jurisprudence on institutional dialogue (Canada''s notwithstanding clause, UK parliamentary sovereignty + HRA interaction).',
    'If one institution''s legitimacy claim is resolved as foreclosed (e.g., rule of law definitively requires courts to constrain legislatures, or electoral sovereignty definitively requires legislatures to constrain courts), the constraint would shift from Tangled Rope toward Snare or false-Mountain depending on which is foreclosed. If both remain genuinely live, the Tangled Rope classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_legitimacy_contestation, conceptual, 'Whether the balanced reading''s dual-legitimacy claim is coherent or masks a hidden hierarchy.').

omega_variable(
    basic_law_supremacy_ambiguity,
    'Are Basic Laws a supreme law category (requiring special amendment procedure and court enforcement), or are they ordinary legislation that happens to cover constitutional topics?',
    'Formal constitutional clarification (e.g., a written Constitution with explicit hierarchy), or sustained practice that resolves the ambiguity through institutional consensus (e.g., a stable supermajority requirement for Basic-Law amendment that the Knesset accepts and the Court enforces without challenge).',
    'If Basic Laws are determined to be supreme law, the constraint shifts toward judicial-supremacy reading; if ordinary legislation, toward parliamentary-sovereignty reading. The balanced reading depends on the ambiguity being genuinely unresolved. Resolution of the ambiguity would reclassify the constraint as the resolved reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(basic_law_supremacy_ambiguity, empirical, 'The foundational ambiguity the balanced reading leaves open: what is the constitutional status of Basic Laws?').

omega_variable(
    international_obligation_incorporation,
    'How much do international human rights obligations genuinely constrain either the Court''s interpretation or the legislature''s amendment authority, versus serving as rhetorical reference?',
    'Empirical measurement: do Court decisions shift when international monitoring pressure increases? Do legislatures decline amendments they would otherwise enact because of international objection? Do these pressures actually constrain or are they absorbed as cheap talk?',
    'If international pressure has substantive constraining force on both institutions, it becomes a third structural party to the constraint (not just an observer). This would reframe the constraint as triadic (Court-Knesset-International) rather than dyadic. If rhetorical only, the constraint remains dyadic and international references are theater.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(international_obligation_incorporation, empirical, 'Whether international human rights obligations are structurally binding on the constitutional arrangement or performatively referenced.').

omega_variable(
    judicial_overhaul_foreclosure,
    'Does the 2023–2024 Judicial Overhaul attempt foreclose the balanced-contestation reading by demonstrating that parliamentary majorities reject the reading''s core premise (mutual institutional legitimacy)?',
    'Outcome of the Overhaul attempt: if it succeeds in materially weakening the Court''s review power, the reading is empirically foreclosed (the legislature has rejected dual legitimacy). If it fails and a new equilibrium emerges, the reading persists with modified metrics. If ongoing contestation without resolution continues, the reading remains live but increasingly theatrical.',
    'If foreclosed by the Overhaul, the constraint becomes parliamentary-sovereignty or a degraded Piton (Court authority maintained through theater only, not genuine institutional acceptance). The current (2024) measurement assumes the reading remains live; future measurement will show whether the Overhaul shifts the constraint toward a different reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(judicial_overhaul_foreclosure, empirical, 'Whether the 2023–2024 Judicial Overhaul constitutes empirical foreclosure of the balanced-contestation reading by parliamentary rejection of its core legitimacy claim.').

omega_variable(
    minoritarian_access_guarantee,
    'Does the reading genuinely provide substantive access to remedy for individual minoritarian claims, or do the courts'' discretionary jurisdictional gatekeeping and strategic restraint make the theoretical access empty?',
    'Comparative measurement: success rate of constitutional petitions by minoritarian groups (religious minorities, LGBTQ+ rights, Palestinian-Israeli equality claims); impact analysis of successful petitions on actual policy implementation; comparison to legislative remedies available to the same groups.',
    'If access is substantively empty (courts reject most minoritarian claims, or accept them but legislatures override), the constraint does not coordinate protection of minorities; instead, it provides the theater of remedy without substance. This would shift the constraint toward Piton (performed legitimacy, atrophied function) for minoritarian claimants. If access is substantive, the coordination function holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(minoritarian_access_guarantee, empirical, 'Whether the reading''s promise of judicial remedy for minoritarian claims operates as genuine access or as procedural theater.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(basic_law_interpretive_boundary__balanced_contestation_reading, 1992, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(basi_tr_t1992, basic_law_interpretive_boundary__balanced_contestation_reading, theater_ratio, 1992, 0.25).
narrative_ontology:measurement(basi_tr_t2000, basic_law_interpretive_boundary__balanced_contestation_reading, theater_ratio, 2000, 0.28).
narrative_ontology:measurement(basi_tr_t2008, basic_law_interpretive_boundary__balanced_contestation_reading, theater_ratio, 2008, 0.35).
narrative_ontology:measurement(basi_tr_t2016, basic_law_interpretive_boundary__balanced_contestation_reading, theater_ratio, 2016, 0.4).
narrative_ontology:measurement(basi_tr_t2020, basic_law_interpretive_boundary__balanced_contestation_reading, theater_ratio, 2020, 0.41).
narrative_ontology:measurement(basi_tr_t2024, basic_law_interpretive_boundary__balanced_contestation_reading, theater_ratio, 2024, 0.42).

% Extraction over time
narrative_ontology:measurement(basi_be_t1992, basic_law_interpretive_boundary__balanced_contestation_reading, base_extractiveness, 1992, 0.35).
narrative_ontology:measurement(basi_be_t2000, basic_law_interpretive_boundary__balanced_contestation_reading, base_extractiveness, 2000, 0.42).
narrative_ontology:measurement(basi_be_t2008, basic_law_interpretive_boundary__balanced_contestation_reading, base_extractiveness, 2008, 0.48).
narrative_ontology:measurement(basi_be_t2016, basic_law_interpretive_boundary__balanced_contestation_reading, base_extractiveness, 2016, 0.51).
narrative_ontology:measurement(basi_be_t2020, basic_law_interpretive_boundary__balanced_contestation_reading, base_extractiveness, 2020, 0.47).
narrative_ontology:measurement(basi_be_t2024, basic_law_interpretive_boundary__balanced_contestation_reading, base_extractiveness, 2024, 0.48).

% Suppression requirement over time
narrative_ontology:measurement(basi_su_t1992, basic_law_interpretive_boundary__balanced_contestation_reading, suppression_requirement, 1992, 0.18).
narrative_ontology:measurement(basi_su_t2000, basic_law_interpretive_boundary__balanced_contestation_reading, suppression_requirement, 2000, 0.22).
narrative_ontology:measurement(basi_su_t2008, basic_law_interpretive_boundary__balanced_contestation_reading, suppression_requirement, 2008, 0.26).
narrative_ontology:measurement(basi_su_t2016, basic_law_interpretive_boundary__balanced_contestation_reading, suppression_requirement, 2016, 0.29).
narrative_ontology:measurement(basi_su_t2020, basic_law_interpretive_boundary__balanced_contestation_reading, suppression_requirement, 2020, 0.3).
narrative_ontology:measurement(basi_su_t2024, basic_law_interpretive_boundary__balanced_contestation_reading, suppression_requirement, 2024, 0.31).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(basic_law_interpretive_boundary__balanced_contestation_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(basic_law_interpretive_boundary__balanced_contestation_reading, 0.12).
narrative_ontology:affects_constraint(basic_law_interpretive_boundary__balanced_contestation_reading, basic_law_interpretive_boundary__judicial_supremacy_reading).
narrative_ontology:affects_constraint(basic_law_interpretive_boundary__balanced_contestation_reading, basic_law_interpretive_boundary__parliamentary_sovereignty_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the contested kernel basic_law_interpretive_boundary, which describes the constitutional allocation of authority over fundamental law in Israel. Three structurally distinct constraints arise from three competing readings: (1) balanced_contestation_reading (this story) asserts both institutions hold legitimate bounded authority in dialogical relationship; (2) judicial_supremacy_reading asserts courts enforce Basic Laws as supreme law, binding on the Knesset; (3) parliamentary_sovereignty_reading asserts the Knesset retains unilateral amendment authority over all law, including Basic Laws. The readings do not represent observer-relative perspectives on a single constraint; they represent different institutional commitments that would instantiate different ε values, different beneficiary/victim structures, and potentially different types if measured separately. This story measures only the balanced reading. The network link shows that this reading influences (and is influenced by) its sibling readings—different Israeli coalitions and international observers espouse different readings, and the contest between readings shapes the constraint's actual operation. All three stories share the same kernel_id but carry different reading_ids and would be classified as a constraint family in the corpus.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(basic_law_interpretive_boundary__balanced_contestation_reading, institutional, 0.65).
constraint_indexing:directionality_override(basic_law_interpretive_boundary__balanced_contestation_reading, powerless, 0.9).
constraint_indexing:directionality_override(basic_law_interpretive_boundary__balanced_contestation_reading, moderate, 0.2).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
