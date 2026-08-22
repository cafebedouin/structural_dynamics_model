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
 *   constraint_id: basic_law_interpretive_boundary__judicial_supremacy_reading
 *   human_readable: Basic Law Interpretive Boundary — Judicial Supremacy Reading
 *   domain: constitutional/political
 *
 * SUMMARY:
 *   This story instantiates the judicial supremacy reading of the Basic Law
 *   interpretive boundary kernel: the Supreme Court's 1995 Bank Mizrahi
 *   doctrine treats the Basic Laws as a higher-order constitutional tier and
 *   treats judicial invalidation of contradictory Knesset legislation as
 *   binding, without a recognized legislative override. Under this reading
 *   the Court is a constraint-enforcer standing above ordinary legislative
 *   majorities, and litigation becomes an effective veto channel for
 *   rights-claimants who cannot secure legislative majorities of their own.
 *   This is a deliberately partial account: the parliamentary sovereignty
 *   reading and the balanced contestation reading are separate constraints
 *   (siblings in this kernel), each with a different ε and a different
 *   stakeholder map, because they encode different — and mutually exclusive
 *   on the override question — accounts of where final interpretive authority
 *   sits.
 *
 * KEY AGENTS:
 *   - supreme_court_justices: institutional agenda-setter and beneficiary of expanded interpretive authority
 *   - knesset_legislative_majority: primary payer, whose enactments are subject to nullification
 *   - rights_claimant_litigants: beneficiary who gains an effective judicial veto channel
 *   - governing_coalition_voters: diffuse payer whose electoral mandate can be partially unwound
 *   - coalition_reform_advocates: excluded voice, formally shut out of this reading's settled-boundary framing
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(basic_law_interpretive_boundary__judicial_supremacy_reading, 0.61).
domain_priors:suppression_score(basic_law_interpretive_boundary__judicial_supremacy_reading, 0.52).
domain_priors:theater_ratio(basic_law_interpretive_boundary__judicial_supremacy_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(basic_law_interpretive_boundary__judicial_supremacy_reading, extractiveness, 0.61).
narrative_ontology:constraint_metric(basic_law_interpretive_boundary__judicial_supremacy_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(basic_law_interpretive_boundary__judicial_supremacy_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(basic_law_interpretive_boundary__judicial_supremacy_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(basic_law_interpretive_boundary__judicial_supremacy_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(basic_law_interpretive_boundary__judicial_supremacy_reading, tangled_rope).
narrative_ontology:human_readable(basic_law_interpretive_boundary__judicial_supremacy_reading, "Basic Law Interpretive Boundary — Judicial Supremacy Reading").
narrative_ontology:topic_domain(basic_law_interpretive_boundary__judicial_supremacy_reading, "constitutional/political").

domain_priors:requires_active_enforcement(basic_law_interpretive_boundary__judicial_supremacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(basic_law_interpretive_boundary__judicial_supremacy_reading, '079e2009-9722-4804-8066-b19dc4ca43e0').
narrative_ontology:cs_kernel_codification('079e2009-9722-4804-8066-b19dc4ca43e0', distributed).
narrative_ontology:cs_authority_grounding('079e2009-9722-4804-8066-b19dc4ca43e0', extraction).
narrative_ontology:cs_interpretation_layer_present('079e2009-9722-4804-8066-b19dc4ca43e0').
narrative_ontology:cs_reading_relation('079e2009-9722-4804-8066-b19dc4ca43e0', basic_law_interpretive_boundary__parliamentary_sovereignty_reading, forecloses).
narrative_ontology:cs_reading_relation('079e2009-9722-4804-8066-b19dc4ca43e0', basic_law_interpretive_boundary__balanced_contestation_reading, coexists_with).
narrative_ontology:cs_axiom('079e2009-9722-4804-8066-b19dc4ca43e0', foundational, basic_laws_constitute_supreme_binding_higher_law).
narrative_ontology:cs_axiom_status(basic_laws_constitute_supreme_binding_higher_law, holdable).
narrative_ontology:cs_axiom_grounding('079e2009-9722-4804-8066-b19dc4ca43e0', basic_laws_constitute_supreme_binding_higher_law, conventional).
narrative_ontology:cs_axiom('079e2009-9722-4804-8066-b19dc4ca43e0', foundational, judicial_invalidation_final_absent_constituent_override).
narrative_ontology:cs_axiom_status(judicial_invalidation_final_absent_constituent_override, holdable).
narrative_ontology:cs_axiom_grounding('079e2009-9722-4804-8066-b19dc4ca43e0', judicial_invalidation_final_absent_constituent_override, conventional).
narrative_ontology:cs_reference_frame('079e2009-9722-4804-8066-b19dc4ca43e0', bank_mizrahi_constitutional_revolution_doctrine).
narrative_ontology:cs_drift_state('079e2009-9722-4804-8066-b19dc4ca43e0', post_2023_judicial_reform_crisis, gap(repudiation_pressure, severe, true)).
narrative_ontology:cs_created_at('079e2009-9722-4804-8066-b19dc4ca43e0', '').
narrative_ontology:cs_kernel_id(basic_law_interpretive_boundary__judicial_supremacy_reading, basic_law_interpretive_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(basic_law_interpretive_boundary__judicial_supremacy_reading, rights_claimant_litigants).
narrative_ontology:constraint_beneficiary(basic_law_interpretive_boundary__judicial_supremacy_reading, minority_communities_relying_on_judicial_protection).
narrative_ontology:constraint_beneficiary(basic_law_interpretive_boundary__judicial_supremacy_reading, supreme_court_justices).
narrative_ontology:constraint_victim(basic_law_interpretive_boundary__judicial_supremacy_reading, knesset_legislative_majority).
narrative_ontology:constraint_victim(basic_law_interpretive_boundary__judicial_supremacy_reading, governing_coalition_voters).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interpret the Basic Laws as a higher constitutional tier and can strike down Knesset legislation found inconsistent with them. This reading treats that invalidation as binding — the Knesset cannot simply re-legislate around it by ordinary majority. The Court thereby administers the boundary of what the elected legislature may do, and its own institutional authority expands with each exercise of the power.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__judicial_supremacy_reading, supreme_court_justices, agenda_setter,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(basic_law_interpretive_boundary__judicial_supremacy_reading, supreme_court_justices, beneficiary).

% Passes legislation reflecting electoral mandates, but under this reading any statute a rights-claimant successfully challenges as inconsistent with a Basic Law can be nullified regardless of the size or freshness of the parliamentary majority. Absent a constitutional override mechanism recognized as legitimate, the majority's only paths are re-legislating within the boundaries the Court sets, amending Basic Laws (a heavier procedural lift), or accepting the loss.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__judicial_supremacy_reading, knesset_legislative_majority, payer,
    organized, biographical, constrained, national).

% Individuals and groups — often minorities without durable legislative majorities — bring petitions asking the Court to void statutes on Basic Law grounds. Under this reading they gain an effective veto path over ordinary legislation that a simple parliamentary majority cannot easily reverse, since it runs through the judicial rather than the electoral channel.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__judicial_supremacy_reading, rights_claimant_litigants, beneficiary,
    moderate, biographical, mobile, national).

% Voted for a legislative program that this reading allows the Court to partially unwind through judicial invalidation. They have no direct channel to contest a ruling except waiting for future elections, pursuing Basic Law amendment (which the same interpretive framework governs), or supporting political efforts to curb judicial review — all slow, uncertain, and themselves subject to the same interpretive boundary.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__judicial_supremacy_reading, governing_coalition_voters, payer,
    powerless, biographical, trapped, national).

% Advises the government on the legality of proposed legislation against the Court's Basic Law jurisprudence and can decline to defend statutes it judges unconstitutional under this reading. This gives the legal-professional apparatus significant pre-emptive influence over which laws even reach the floor in a form likely to survive review.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__judicial_supremacy_reading, attorney_general_and_state_prosecution, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(basic_law_interpretive_boundary__judicial_supremacy_reading, attorney_general_and_state_prosecution, observer).

% Argue that an unelected court should not have final, binding say over an elected legislature's enactments under a framework the Knesset itself never entrenched by supermajority. Under the judicial supremacy reading their position has no formal foothold — the reading treats the interpretive boundary as settled rather than as a live contest to be resolved by ordinary politics.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__judicial_supremacy_reading, coalition_reform_advocates, excluded,
    organized, biographical, trapped, national).

% Study the Israeli case as a data point in debates over strong-form versus weak-form judicial review. They document how the interpretive boundary was itself established mostly through judicial doctrine (post-1992 jurisprudence) rather than explicit constituent-power drafting, and compare its stability to other systems with entrenched constitutions.
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
% COORDINATION_FUNCTION: Provides a stable, judicially-administered floor of individual and minority rights that ordinary shifting legislative majorities cannot easily erode, and gives investors, minorities, and international partners a predictable baseline of legal protection independent of election outcomes.
% TRANSFER_FUNCTION: Moves effective policymaking authority over rights-adjacent legislation from the elected Knesset majority to the Supreme Court and the litigants who can invoke it; shifts practical veto power from ballot-box majorities to successful petitioners and the justices who rule on their claims.
% ABSENT_VOICES: Coalition reform advocates and legislators who believe the Knesset's electoral mandate should be final have no recognized channel within this reading — the reading treats the boundary as settled doctrine rather than a live question requiring their input; their objections surface only as external political pressure to change the rules entirely (override legislation, judicial appointments reform), not as a voice inside the interpretive framework itself.
% DISAPPEARANCE_RATIONALE: If binding judicial invalidation vanished overnight, Knesset majorities could enact and retain legislation the Court currently blocks or narrows; rights-claimant litigation would lose its practical teeth as a check on ordinary lawmaking; minority protections would depend entirely on future electoral coalitions rather than a judicially-enforced floor. The entire architecture of pre-legislative legal caution inside government ministries would also unwind.
% FOUNDING_PROBLEM: Israel lacks a single entrenched written constitution; the Basic Laws were passed as ordinary legislation with an understanding (contested from the start) that they would eventually form a constitutional framework. The founding problem this reading solves is filling that gap: establishing SOME binding check on legislative majorities in the absence of a formally ratified constitution, primarily to protect rights and structural norms that a simple majority might otherwise erode.
% FOUNDING_PROBLEM_CORROBORATION: Rights-claimant litigants and civil liberties organizations attest the problem remains live — citing legislative proposals they view as threatening minority protections. Coalition reform advocates and a substantial bloc of legal scholars outside the beneficiary set attest the problem is either overstated or that the judicial-supremacy solution itself has become the entrenched arrangement it was meant to prevent, since the binding-invalidation doctrine was established by the Court's own 1995 Bank Mizrahi ruling rather than by explicit constituent authorization — corroboration for the 'dead or captured' reading comes from comparative constitutional scholars documenting that few other democracies vest this degree of unreviewable interpretive finality in a court that assigned itself the power.
narrative_ontology:disappearance_verdict(basic_law_interpretive_boundary__judicial_supremacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(basic_law_interpretive_boundary__judicial_supremacy_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(basic_law_interpretive_boundary__judicial_supremacy_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(basic_law_interpretive_boundary__judicial_supremacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(basic_law_interpretive_boundary__judicial_supremacy_reading, 0.61, 'claude-sonnet-5', 'none', direct).

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
 *   Extraction is authored moderate-high (0.61 at 2023) because binding judicial invalidation genuinely transfers policymaking finality away from elected majorities toward the Court and successful litigants — a real transfer, not merely rhetorical. Suppression (0.52) reflects that legislative majorities have no recognized formal override; their only paths are constitutionally heavier (Basic Law amendment) or purely political (appointments reform, override legislation itself contested). Theater ratio stays comparatively low (0.22) because the enforcement function — actual case adjudication and invalidation — is real and substantively exercised, not merely performed. Accessibility collapse is moderate (0.48): alternative institutional arrangements (explicit override mechanisms, a ratified constitution) remain politically live, unlike a true mountain. Resistance is high (0.72), tracking the sustained political mobilization against judicial supremacy this reading has generated. All three tracked metrics run on one shared time grid from 1992 (Bank Mizrahi) to 2023.
 *
 * PERSPECTIVAL GAP:
 *   From the Court's seat, this reading describes principled constitutional guardianship: enforcing rights protections a fragmented, majoritarian legislature might erode. From the Knesset majority's seat, the identical structure operates as an unelected body's binding veto over democratically mandated policy, exercised via a doctrine the Court itself announced without explicit constituent authorization. The engine computes these as structurally different seat experiences from the same authored data — this story does not adjudicate which seat's story is correct.
 *
 * DIRECTIONALITY LOGIC:
 *   Supreme Court justices sit near the beneficiary end: they administer the boundary and their institutional authority is what expands under this reading. Rights-claimant litigants are beneficiaries by structural position — they gain a veto path unavailable through ordinary electoral competition. The Knesset legislative majority and governing coalition voters sit toward the target end: their enacted preferences are what gets nullified, and their electoral mandate does not translate into an override capacity within this reading. Coalition reform advocates are excluded rather than positioned as targets or beneficiaries — the reading simply does not recognize their preferred framing (Knesset-final authority) as live.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — the absence of any binding check on legislative majorities given no ratified constitution — was real in 1992. Whether it remains live in 2023 is genuinely contested: rights organizations point to ongoing legislative threats to minority protections as evidence the problem persists; reform advocates and outside scholars argue the 'solution' (self-assigned binding judicial finality) has itself become the entrenched arrangement, exceeding what any constituent process authorized. This tangled_rope classification is chosen specifically to avoid two mislabeling errors: calling the arrangement pure coordination (ignoring the real transfer of authority away from elected majorities) or calling it pure extraction (ignoring the genuine minority-protection function the doctrine performs, which the parliamentary sovereignty reading's own proponents concede has some value even as they contest its legitimacy).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    self_authorized_vs_constituent_authorized_boundary,
    'Is the binding force of judicial invalidation under the Basic Laws a genuine higher-law constraint that any constitutional order would need, or is it a boundary the Court effectively authorized for itself via Bank Mizrahi without an explicit constituent-power ratification (e.g., referendum, supermajority entrenchment)?',
    'Comparative analysis of how other written and unwritten constitutional systems established binding judicial review (explicit constitutional text vs. judicial doctrine), combined with a genealogical account of Knesset intent when passing the original Basic Laws as ordinary legislation.',
    'If self-authorized without constituent ratification, the ''higher-order framework'' claim is weaker than the parliamentary sovereignty reading suggests it should be treated, and the judicial supremacy reading''s extraction from legislative majorities looks less like enforcing a settled higher law and more like an institutional power grab dressed in constitutional language.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(self_authorized_vs_constituent_authorized_boundary, conceptual, 'Whether the binding-invalidation doctrine has constituent-level legitimacy or is judicially self-conferred.').

omega_variable(
    kernel_reading_indeterminacy,
    'Which of the three readings of the basic_law_interpretive_boundary kernel (judicial_supremacy, parliamentary_sovereignty, balanced_contestation) is the operative one at any given moment, given that Israeli constitutional practice has not definitively settled the question and different governments have pursued legislation (e.g., override clauses, reasonableness-standard curbs) attempting to shift the operative reading?',
    'Track which reading''s institutional predictions actually obtain in subsequent legislative-judicial confrontations: does the Knesset successfully override a Court ruling by simple majority (favoring parliamentary_sovereignty), does the Court''s invalidation hold without recourse (favoring judicial_supremacy), or does a negotiated modus vivendi emerge (favoring balanced_contestation)?',
    'The three sibling readings are not merely interpretive preferences — only one (or a shifting mixture) actually describes operative practice at any time; this omega is the location where the kernel contest is genuinely open rather than resolved, and where the committer structure (which reading this story instantiates) is itself contestable political terrain, not settled fact.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_indeterminacy, conceptual, 'Which sibling reading of the kernel actually describes operative institutional practice, and whether that changes over time.').

omega_variable(
    beneficiary_composition_stability,
    'Are rights_claimant_litigants and minority_communities a stable beneficiary class across all subject matters, or does the beneficiary/victim assignment flip depending on which rights are at stake (e.g., religious-liberty claims vs. security-detention claims may produce different winners under the same doctrine)?',
    'Case-level coding of Supreme Court Basic Law rulings by subject matter and by which political/demographic coalition benefited from each invalidation, to test whether beneficiary composition is stable or issue-contingent.',
    'If beneficiary composition varies substantially by issue area, the single beneficiaries/victims declaration in this story is a simplification averaging over a heterogeneous docket, and issue-specific sub-stories may be warranted under the ε-invariance principle.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_composition_stability, empirical, 'Whether the beneficiary class is stable across subject-matter domains or varies by issue.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(basic_law_interpretive_boundary__judicial_supremacy_reading, 1992, 2023).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(basi_tr_t1992, basic_law_interpretive_boundary__judicial_supremacy_reading, theater_ratio, 1992, 0.1).
narrative_ontology:measurement(basi_tr_t1997, basic_law_interpretive_boundary__judicial_supremacy_reading, theater_ratio, 1997, 0.12).
narrative_ontology:measurement(basi_tr_t2003, basic_law_interpretive_boundary__judicial_supremacy_reading, theater_ratio, 2003, 0.14).
narrative_ontology:measurement(basi_tr_t2009, basic_law_interpretive_boundary__judicial_supremacy_reading, theater_ratio, 2009, 0.16).
narrative_ontology:measurement(basi_tr_t2015, basic_law_interpretive_boundary__judicial_supremacy_reading, theater_ratio, 2015, 0.18).
narrative_ontology:measurement(basi_tr_t2019, basic_law_interpretive_boundary__judicial_supremacy_reading, theater_ratio, 2019, 0.2).
narrative_ontology:measurement(basi_tr_t2023, basic_law_interpretive_boundary__judicial_supremacy_reading, theater_ratio, 2023, 0.22).

% Extraction over time
narrative_ontology:measurement(basi_be_t1992, basic_law_interpretive_boundary__judicial_supremacy_reading, base_extractiveness, 1992, 0.28).
narrative_ontology:measurement(basi_be_t1997, basic_law_interpretive_boundary__judicial_supremacy_reading, base_extractiveness, 1997, 0.35).
narrative_ontology:measurement(basi_be_t2003, basic_law_interpretive_boundary__judicial_supremacy_reading, base_extractiveness, 2003, 0.42).
narrative_ontology:measurement(basi_be_t2009, basic_law_interpretive_boundary__judicial_supremacy_reading, base_extractiveness, 2009, 0.48).
narrative_ontology:measurement(basi_be_t2015, basic_law_interpretive_boundary__judicial_supremacy_reading, base_extractiveness, 2015, 0.53).
narrative_ontology:measurement(basi_be_t2019, basic_law_interpretive_boundary__judicial_supremacy_reading, base_extractiveness, 2019, 0.57).
narrative_ontology:measurement(basi_be_t2023, basic_law_interpretive_boundary__judicial_supremacy_reading, base_extractiveness, 2023, 0.61).

% Suppression requirement over time
narrative_ontology:measurement(basi_su_t1992, basic_law_interpretive_boundary__judicial_supremacy_reading, suppression_requirement, 1992, 0.2).
narrative_ontology:measurement(basi_su_t1997, basic_law_interpretive_boundary__judicial_supremacy_reading, suppression_requirement, 1997, 0.28).
narrative_ontology:measurement(basi_su_t2003, basic_law_interpretive_boundary__judicial_supremacy_reading, suppression_requirement, 2003, 0.34).
narrative_ontology:measurement(basi_su_t2009, basic_law_interpretive_boundary__judicial_supremacy_reading, suppression_requirement, 2009, 0.39).
narrative_ontology:measurement(basi_su_t2015, basic_law_interpretive_boundary__judicial_supremacy_reading, suppression_requirement, 2015, 0.44).
narrative_ontology:measurement(basi_su_t2019, basic_law_interpretive_boundary__judicial_supremacy_reading, suppression_requirement, 2019, 0.48).
narrative_ontology:measurement(basi_su_t2023, basic_law_interpretive_boundary__judicial_supremacy_reading, suppression_requirement, 2023, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(basic_law_interpretive_boundary__judicial_supremacy_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(basic_law_interpretive_boundary__judicial_supremacy_reading, basic_law_interpretive_boundary__parliamentary_sovereignty_reading).
narrative_ontology:affects_constraint(basic_law_interpretive_boundary__judicial_supremacy_reading, basic_law_interpretive_boundary__balanced_contestation_reading).

% DUAL FORMULATION NOTE:
% This story is one of three siblings decomposing the natural-language concept 'the Basic Law interpretive boundary' into structurally distinct constraints per the ε-invariance principle. Each sibling reading has its own ε, beneficiary/victim structure, and claimed_type: judicial_supremacy_reading (this story, tangled_rope, ε=0.61) treats binding judicial invalidation as settled; parliamentary_sovereignty_reading treats Knesset simple-majority override as the operative rule; balanced_contestation_reading treats both institutions as bounded and non-final. They are linked here rather than merged because the underlying institutional dispute is genuinely live and produces different real-world predictions, not merely different evaluative framings of one fixed arrangement.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(basic_law_interpretive_boundary__judicial_supremacy_reading, organized, 0.68).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
