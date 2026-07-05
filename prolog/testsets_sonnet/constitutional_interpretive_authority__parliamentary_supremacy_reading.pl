% ============================================================================
% CONSTRAINT STORY: constitutional_interpretive_authority__parliamentary_supremacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_constitutional_interpretive_authority__parliamentary_supremacy_reading, []).

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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: constitutional_interpretive_authority__parliamentary_supremacy_reading
 *   human_readable: Parliamentary Supremacy Reading of Final Interpretive Authority
 *   domain: constitutional_law/political_theory/jurisprudence
 *
 * SUMMARY:
 *   This constraint instantiates the parliamentary-supremacy reading of the
 *   contested constitutional-interpretive-authority kernel: the elected
 *   legislature holds final interpretive authority over the constitution's
 *   meaning, and no court may void or disapply a duly enacted statute on
 *   constitutional grounds. This is a specific, structurally distinct claim
 *   from the judicial-supremacy reading (courts hold final authority, can
 *   nullify statutes) and the coordinate-construction reading (no branch
 *   holds finality; meaning is negotiated across branches over time). Each
 *   reading is authored as its own constraint story with its own epsilon;
 *   this file covers only the parliamentary-supremacy claim.
 *
 * KEY AGENTS:
 *   - sitting_parliamentary_majority: agenda_setter/beneficiary (institutional/arbitrage) — holds and exercises final interpretive discretion
 *   - executive_cabinet_drawing_confidence_from_majority: beneficiary (institutional/arbitrage) — gains policy certainty from absence of judicial veto
 *   - judiciary: excluded (institutional/constrained) — retains persuasive but not binding interpretive voice
 *   - electoral_minorities_with_no_legislative_recourse: payer (powerless/trapped) — bears cost of majority decisions with no judicial remedy
 *   - rights_claimants_seeking_judicial_remedy_against_statute: payer (moderate/constrained) — litigation cannot produce durable relief against statute
 *   - constitutional_law_scholars: observer (analytical/analytical) — comparative analysis across kernel readings
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(constitutional_interpretive_authority__parliamentary_supremacy_reading, 0.42).
domain_priors:suppression_score(constitutional_interpretive_authority__parliamentary_supremacy_reading, 0.55).
domain_priors:theater_ratio(constitutional_interpretive_authority__parliamentary_supremacy_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(constitutional_interpretive_authority__parliamentary_supremacy_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(constitutional_interpretive_authority__parliamentary_supremacy_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(constitutional_interpretive_authority__parliamentary_supremacy_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(constitutional_interpretive_authority__parliamentary_supremacy_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(constitutional_interpretive_authority__parliamentary_supremacy_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(constitutional_interpretive_authority__parliamentary_supremacy_reading, tangled_rope).
narrative_ontology:human_readable(constitutional_interpretive_authority__parliamentary_supremacy_reading, "Parliamentary Supremacy Reading of Final Interpretive Authority").
narrative_ontology:topic_domain(constitutional_interpretive_authority__parliamentary_supremacy_reading, "constitutional_law/political_theory/jurisprudence").

domain_priors:requires_active_enforcement(constitutional_interpretive_authority__parliamentary_supremacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(constitutional_interpretive_authority__parliamentary_supremacy_reading, 'fd2860b8-dbb6-4e59-941f-4f60a782a77f').
narrative_ontology:cs_kernel_codification('fd2860b8-dbb6-4e59-941f-4f60a782a77f', distributed).
narrative_ontology:cs_authority_grounding('fd2860b8-dbb6-4e59-941f-4f60a782a77f', practice).
narrative_ontology:cs_interpretation_layer_present('fd2860b8-dbb6-4e59-941f-4f60a782a77f').
narrative_ontology:cs_reading_relation('fd2860b8-dbb6-4e59-941f-4f60a782a77f', constitutional_interpretive_authority__judicial_supremacy_reading, forecloses).
narrative_ontology:cs_reading_relation('fd2860b8-dbb6-4e59-941f-4f60a782a77f', constitutional_interpretive_authority__coordinate_construction_reading, coexists_with).
narrative_ontology:cs_axiom('fd2860b8-dbb6-4e59-941f-4f60a782a77f', foundational, electoral_accountability_is_sole_legitimate_ground_for_finality).
narrative_ontology:cs_axiom_status(electoral_accountability_is_sole_legitimate_ground_for_finality, holdable).
narrative_ontology:cs_axiom_grounding('fd2860b8-dbb6-4e59-941f-4f60a782a77f', electoral_accountability_is_sole_legitimate_ground_for_finality, conventional).
narrative_ontology:cs_axiom('fd2860b8-dbb6-4e59-941f-4f60a782a77f', secondary, judicial_review_of_statute_is_illegitimate_countermajoritarian_override).
narrative_ontology:cs_axiom_status(judicial_review_of_statute_is_illegitimate_countermajoritarian_override, holdable).
narrative_ontology:cs_axiom_grounding('fd2860b8-dbb6-4e59-941f-4f60a782a77f', judicial_review_of_statute_is_illegitimate_countermajoritarian_override, instrumental).
narrative_ontology:cs_reference_frame('fd2860b8-dbb6-4e59-941f-4f60a782a77f', parliamentary_sovereignty_founding_settlement).
narrative_ontology:cs_drift_state('fd2860b8-dbb6-4e59-941f-4f60a782a77f', contemporary_rights_era, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('fd2860b8-dbb6-4e59-941f-4f60a782a77f', '').
narrative_ontology:cs_kernel_id(constitutional_interpretive_authority__parliamentary_supremacy_reading, constitutional_interpretive_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(constitutional_interpretive_authority__parliamentary_supremacy_reading, sitting_parliamentary_majority).
narrative_ontology:constraint_beneficiary(constitutional_interpretive_authority__parliamentary_supremacy_reading, executive_cabinet_drawing_confidence_from_majority).
narrative_ontology:constraint_victim(constitutional_interpretive_authority__parliamentary_supremacy_reading, electoral_minorities_with_no_legislative_recourse).
narrative_ontology:constraint_victim(constitutional_interpretive_authority__parliamentary_supremacy_reading, rights_claimants_seeking_judicial_remedy_against_statute).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds the votes to pass, amend, or repeal any statute, including statutes that touch fundamental rights, without a court able to strike the result down. Justifies this as democratic legitimacy — only the electorally accountable branch should have the last word on what the constitution permits. Faces no institutional check beyond the next election and its own internal party discipline.
narrative_ontology:constraint_stakeholder(constitutional_interpretive_authority__parliamentary_supremacy_reading, sitting_parliamentary_majority, agenda_setter,
    institutional, biographical, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(constitutional_interpretive_authority__parliamentary_supremacy_reading, sitting_parliamentary_majority, beneficiary).

% Drafts and drives most legislation the majority enacts and benefits directly from the absence of judicial veto over its legislative program. Gains policy certainty and speed that judicial-supremacy systems deny equivalent executives.
narrative_ontology:constraint_stakeholder(constitutional_interpretive_authority__parliamentary_supremacy_reading, executive_cabinet_drawing_confidence_from_majority, beneficiary,
    institutional, biographical, arbitrage, national).

% May interpret statutes, issue declarations of incompatibility, or develop common-law rights protections, but cannot void or disapply an Act of Parliament on constitutional grounds. Retains interpretive voice but not interpretive finality; its constitutional role is persuasive rather than binding on the legislature.
narrative_ontology:constraint_stakeholder(constitutional_interpretive_authority__parliamentary_supremacy_reading, judiciary, excluded,
    institutional, generational, constrained, national).

% Groups whose interests the sitting majority has no electoral incentive to protect bear the practical cost when legislation disadvantages them: no court can override the statute on their behalf, and their remedy is confined to future elections they may structurally be unable to win.
narrative_ontology:constraint_stakeholder(constitutional_interpretive_authority__parliamentary_supremacy_reading, electoral_minorities_with_no_legislative_recourse, payer,
    powerless, biographical, trapped, national).

% Individuals or groups whose rights are burdened by a specific statute can litigate, but the furthest a court can go is a non-binding declaration or an interpretation straining the statute's words; the legislature can restore the burden by simply re-enacting or clarifying its intent. Legal victory does not translate into durable relief.
narrative_ontology:constraint_stakeholder(constitutional_interpretive_authority__parliamentary_supremacy_reading, rights_claimants_seeking_judicial_remedy_against_statute, payer,
    moderate, biographical, constrained, national).

% Study and compare the parliamentary-supremacy arrangement against judicial-supremacy and coordinate-construction systems, documenting when the absence of judicial veto has protected democratic responsiveness and when it has left minorities without remedy.
narrative_ontology:constraint_stakeholder(constitutional_interpretive_authority__parliamentary_supremacy_reading, constitutional_law_scholars, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Concentrates final constitutional interpretation in the elected branch so that contested questions of public value are resolved by a body accountable to voters rather than by unelected judges, avoiding the coordination failure of two branches each claiming final say over the same text.
% TRANSFER_FUNCTION: Moves the power to settle constitutional meaning from courts to the sitting legislative majority; correspondingly moves the cost of erroneous or rights-burdening legislation from a judicially-correctable event onto whoever the statute burdens, until the electorate changes the majority.
% ABSENT_VOICES: Electoral minorities and rights claimants whose interests do not track the sitting majority's electoral coalition would argue for a judicially enforceable backstop; they are present in litigation but structurally unable to obtain binding relief, and are not represented in the legislative process that produced the statute they contest.
% DISAPPEARANCE_RATIONALE: If parliamentary supremacy were displaced overnight by judicial supremacy, courts would gain the power to void Acts of Parliament, legislative drafting would begin anticipating judicial review, rights litigation would become a live check on statute rather than a persuasive one, and the executive would lose the policy certainty it currently enjoys — a substantial rearrangement of where constitutional authority actually sits.
% FOUNDING_PROBLEM: Historically built to end an era in which unelected judicial or monarchical authority could override the will of an elected assembly, on the premise that legitimacy for binding collective decisions should run through electoral accountability rather than through appointed office.
% FOUNDING_PROBLEM_CORROBORATION: The sitting majority and executive attest the founding problem remains live — unelected judicial override of democratic decisions is treated as an ongoing risk requiring the doctrine's continuation. Constitutional scholars outside government, along with rights claimants and minority advocacy groups, attest that in mature democracies the original problem (arbitrary non-elected veto) has been substantially supplanted by a different one — legislative majorities using unreviewable authority to burden minorities who cannot win elections — and that the doctrine now serves majority discretion rather than the anti-arbitrariness rationale it was built on.
narrative_ontology:disappearance_verdict(constitutional_interpretive_authority__parliamentary_supremacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(constitutional_interpretive_authority__parliamentary_supremacy_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(constitutional_interpretive_authority__parliamentary_supremacy_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(constitutional_interpretive_authority__parliamentary_supremacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(constitutional_interpretive_authority__parliamentary_supremacy_reading, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(constitutional_interpretive_authority__parliamentary_supremacy_reading_tests).
:- end_tests(constitutional_interpretive_authority__parliamentary_supremacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is moderate (0.42 at interval end) rather than extreme: the coordination function (resolving constitutional disputes through an accountable, electorally-correctable body) is genuine and substantial, which is why this reading is not authored as a snare. But asymmetric cost falls on electoral minorities and rights claimants who structurally cannot win the majoritarian process that is their only avenue of redress — hence tangled_rope rather than rope. Suppression (0.55) reflects that the arrangement is actively maintained: it requires the judiciary's forbearance from claiming a nullification power, sustained by constitutional convention, statute, or entrenched doctrine, and enforced against litigants who seek a stronger remedy than a declaration. Theater ratio is low (0.2) — this is a substantive, functioning allocation of authority, not primarily performative, though it rises slightly over the measured interval as declaration-of-incompatibility mechanisms accumulate without binding force, producing an increasing appearance of judicial engagement that does not change outcomes.
 *
 * PERSPECTIVAL GAP:
 *   From the sitting majority's seat, this arrangement is legitimate democratic self-governance — coordination solving the problem of unaccountable interpretive override. From the seat of a rights claimant who has won a declaration of incompatibility that the legislature is free to ignore, the identical structure is extraction: a favorable legal finding with no binding force. The engine should compute these as different seat-level classifications from the same structural facts, not reconcile them to a single verdict.
 *
 * DIRECTIONALITY LOGIC:
 *   The sitting parliamentary majority and the executive it commands are the structural beneficiaries: they receive interpretive discretion unconstrained by judicial override, d near the beneficiary end. Electoral minorities without realistic prospects of becoming the majority, and rights claimants whose only remedy is persuasive rather than binding, sit near the target end — trapped or constrained exit, no institutional lever to compel a different outcome. The judiciary itself is neither straightforwardly a beneficiary nor a victim under this reading; it is excluded from the finality contest altogether, which is precisely the structural delta this reading requires relative to the judicial-supremacy sibling.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — preventing unelected judicial or monarchical veto of democratic decisions — is contested rather than resolved: proponents treat it as permanently live (any judicial supremacy risks a return to unaccountable override), while critics observe that the doctrine, once justified against arbitrary non-elected authority, now also functions to insulate majority decisions from correction on behalf of minorities the majority has no electoral incentive to protect. This is exactly the kind of status where declaring outright mandatrophy-resolved or mandatrophy-live would overclaim; the founding_problem_status is authored as contested and left for the mismatch consumer to flag against the computed classification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_selection_parliamentary,
    'Is parliamentary supremacy the structurally correct reading of final interpretive authority for a given constitutional order, or is it one contestable reading among coordinate_construction_reading and judicial_supremacy_reading, each instantiating a different constraint?',
    'No single empirical resolution exists; the choice tracks a jurisdiction''s actual constitutional text, entrenched convention, and judicial practice (e.g., presence or absence of a constitutional court with nullification power, presence of an entrenched bill of rights, doctrine of parliamentary sovereignty vs. constitutional supremacy clauses).',
    'If a jurisdiction''s actual practice tracks coordinate_construction_reading rather than pure parliamentary supremacy, this story''s stakeholder structure and beneficiary/victim declarations would not apply to that jurisdiction and a different story should be generated instead.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_selection_parliamentary, conceptual, 'Documents that this story instantiates one reading of a contested kernel; the reading itself is not empirically adjudicable, only jurisdiction-matchable.').

omega_variable(
    sibling_reading_structural_delta,
    'What would change structurally if the judicial_supremacy_reading or coordinate_construction_reading applied instead of this one?',
    'Compare beneficiary/victim sets across the three sibling stories directly: under judicial_supremacy_reading the judiciary enters the beneficiary/agenda_setter set and the legislature becomes a payer seat when its statutes are struck; under coordinate_construction_reading no single branch holds the agenda_setter role and the extraction/coordination balance is redistributed across an ongoing dialogue rather than concentrated at one branch.',
    'The disagreement between readings is located specifically in who holds the agenda_setter role for constitutional meaning and whether coercion is legitimated via electoral mandate (this reading) or rights-grounding (judicial_supremacy_reading) or neither being final (coordinate_construction_reading).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_structural_delta, conceptual, 'Locates the exact structural element the three kernel readings disagree on.').

omega_variable(
    minority_protection_empirical_ambiguity,
    'Does the absence of judicial nullification power measurably correlate with worse outcomes for electoral minorities across jurisdictions that have adopted this reading, relative to jurisdictions with judicial supremacy?',
    'Comparative empirical study of minority-rights outcomes across parliamentary-supremacy jurisdictions (e.g., UK pre-Human Rights Act, New Zealand) versus judicial-supremacy jurisdictions (e.g., US, Germany), controlling for other institutional variables.',
    'If outcomes are not measurably worse, the extractiveness authored here may overstate the actual cost borne by minority payer seats; if outcomes are measurably worse, it may understate it.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(minority_protection_empirical_ambiguity, empirical, 'Whether the authored extraction level tracks real comparative outcomes for minority stakeholders.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(constitutional_interpretive_authority__parliamentary_supremacy_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cons_tr_t0, constitutional_interpretive_authority__parliamentary_supremacy_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement(cons_tr_t10, constitutional_interpretive_authority__parliamentary_supremacy_reading, theater_ratio, 10, 0.1).
narrative_ontology:measurement(cons_tr_t20, constitutional_interpretive_authority__parliamentary_supremacy_reading, theater_ratio, 20, 0.12).
narrative_ontology:measurement(cons_tr_t30, constitutional_interpretive_authority__parliamentary_supremacy_reading, theater_ratio, 30, 0.14).
narrative_ontology:measurement(cons_tr_t40, constitutional_interpretive_authority__parliamentary_supremacy_reading, theater_ratio, 40, 0.17).
narrative_ontology:measurement(cons_tr_t50, constitutional_interpretive_authority__parliamentary_supremacy_reading, theater_ratio, 50, 0.18).
narrative_ontology:measurement(cons_tr_t60, constitutional_interpretive_authority__parliamentary_supremacy_reading, theater_ratio, 60, 0.2).

% Extraction over time
narrative_ontology:measurement(cons_be_t0, constitutional_interpretive_authority__parliamentary_supremacy_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(cons_be_t10, constitutional_interpretive_authority__parliamentary_supremacy_reading, base_extractiveness, 10, 0.31).
narrative_ontology:measurement(cons_be_t20, constitutional_interpretive_authority__parliamentary_supremacy_reading, base_extractiveness, 20, 0.34).
narrative_ontology:measurement(cons_be_t30, constitutional_interpretive_authority__parliamentary_supremacy_reading, base_extractiveness, 30, 0.37).
narrative_ontology:measurement(cons_be_t40, constitutional_interpretive_authority__parliamentary_supremacy_reading, base_extractiveness, 40, 0.39).
narrative_ontology:measurement(cons_be_t50, constitutional_interpretive_authority__parliamentary_supremacy_reading, base_extractiveness, 50, 0.41).
narrative_ontology:measurement(cons_be_t60, constitutional_interpretive_authority__parliamentary_supremacy_reading, base_extractiveness, 60, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(cons_su_t0, constitutional_interpretive_authority__parliamentary_supremacy_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(cons_su_t10, constitutional_interpretive_authority__parliamentary_supremacy_reading, suppression_requirement, 10, 0.44).
narrative_ontology:measurement(cons_su_t20, constitutional_interpretive_authority__parliamentary_supremacy_reading, suppression_requirement, 20, 0.47).
narrative_ontology:measurement(cons_su_t30, constitutional_interpretive_authority__parliamentary_supremacy_reading, suppression_requirement, 30, 0.5).
narrative_ontology:measurement(cons_su_t40, constitutional_interpretive_authority__parliamentary_supremacy_reading, suppression_requirement, 40, 0.52).
narrative_ontology:measurement(cons_su_t50, constitutional_interpretive_authority__parliamentary_supremacy_reading, suppression_requirement, 50, 0.54).
narrative_ontology:measurement(cons_su_t60, constitutional_interpretive_authority__parliamentary_supremacy_reading, suppression_requirement, 60, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(constitutional_interpretive_authority__parliamentary_supremacy_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(constitutional_interpretive_authority__parliamentary_supremacy_reading, 0.1).
narrative_ontology:affects_constraint(constitutional_interpretive_authority__parliamentary_supremacy_reading, judicial_supremacy_reading).
narrative_ontology:affects_constraint(constitutional_interpretive_authority__parliamentary_supremacy_reading, coordinate_construction_reading).

% DUAL FORMULATION NOTE:
% This story is one of three sibling constraints decomposing the natural-language concept 'final constitutional interpretive authority.' Each sibling has a distinct epsilon, distinct beneficiary/victim structure, and distinct claimed_type: parliamentary_supremacy_reading (this file, tangled_rope, legislature as beneficiary), judicial_supremacy_reading (courts as beneficiary/agenda_setter, legislature as payer when struck down), coordinate_construction_reading (no concentrated beneficiary, extraction diffused across an ongoing inter-branch bargaining process — likely a rope or scaffold depending on how institutionalized the dialogue mechanism is). Link all three via affects_constraints; do not average their epsilon values into a single 'constitutional interpretive authority' epsilon.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(constitutional_interpretive_authority__parliamentary_supremacy_reading, moderate, 0.62).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
