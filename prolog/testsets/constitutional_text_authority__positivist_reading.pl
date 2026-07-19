% ============================================================================
% CONSTRAINT STORY: constitutional_text_authority__positivist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_constitutional_text_authority__positivist_reading, []).

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
 *   constraint_id: constitutional_text_authority__positivist_reading
 *   human_readable: Constitutional Validity as Enactment Pedigree (Positivist Reading)
 *   domain: legal/constitutional/interpretive_jurisprudence
 *
 * SUMMARY:
 *   This story is one reading of the constitutional_text_authority kernel:
 *   the positivist reading, under which constitutional validity derives from
 *   formal enactment procedures and institutional sources, and the
 *   law/morality distinction is maintained — moral content is irrelevant to
 *   whether an enactment is valid law. The constraint operates as a validity
 *   register policed by courts and doctrinal establishment: arguments are
 *   admitted or excluded by type. It coordinates legality across deep
 *   pluralism (genuine rope component) while insulating enactments of
 *   procedurally proper majorities from an entire class of challenge and
 *   routing moral contest through majoritarian channels (asymmetric
 *   extraction component). This file is deliberately decomposed from its
 *   sibling readings per the epsilon-invariance principle: the originalist
 *   reading adds a historical-meaning mooring the positivist reading lacks,
 *   and the living constitutionalist reading re-admits moral content as
 *   authority-constitutive — each instantiates a different constraint with a
 *   different victim set, and conflating them under the label 'constitutional
 *   text authority' was the natural-language ambiguity, not a single
 *   constraint. Claim/metric independence is maintained: the claimed type
 *   reflects the structural judgment that coordination and extraction
 *   genuinely co-occur here; the metrics describe the doctrine's actual
 *   operation, including the drift toward formalist theater as practice
 *   departs from strict separability.
 *
 * KEY AGENTS:
 *   - legislative_majorities: Primary beneficiary (institutional/mobile) — their enactments gain validity by pedigree alone, insulated from moral-invalidity challenge; the extraction accrues here
 *   - formalist_judiciary: Agenda-setter (institutional/identity_locked) — administers the validity test, polices the law/morality boundary, collects decision-economy and legitimation cover
 *   - positivist_legal_academy: Secondary beneficiary and doctrine producer (organized/identity_locked) — careers and doctrinal authority built on the separability thesis
 *   - unjust_law_subjects: Primary payer (powerless/trapped) — bear formally valid but morally objectionable law with remedy routed to a forum where they lack numbers
 *   - moral_invalidity_claimants: Secondary payer (moderate/constrained) — their core argument type is excluded from the validity register; must translate or abandon
 *   - natural_law_advocates: Excluded seat (moderate/constrained) — structurally barred from adjudicative validity discourse while free in the academy
 *   - comparative_constitutional_scholars: Analytical observer (analytical/analytical) — sees the gap between professed and practiced validity tests across systems
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(constitutional_text_authority__positivist_reading, 0.45).
domain_priors:suppression_score(constitutional_text_authority__positivist_reading, 0.5).
domain_priors:theater_ratio(constitutional_text_authority__positivist_reading, 0.34).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(constitutional_text_authority__positivist_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(constitutional_text_authority__positivist_reading, suppression_requirement, 0.5).
narrative_ontology:constraint_metric(constitutional_text_authority__positivist_reading, theater_ratio, 0.34).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(constitutional_text_authority__positivist_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(constitutional_text_authority__positivist_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(constitutional_text_authority__positivist_reading, tangled_rope).
narrative_ontology:human_readable(constitutional_text_authority__positivist_reading, "Constitutional Validity as Enactment Pedigree (Positivist Reading)").
narrative_ontology:topic_domain(constitutional_text_authority__positivist_reading, "legal/constitutional/interpretive_jurisprudence").

domain_priors:requires_active_enforcement(constitutional_text_authority__positivist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(constitutional_text_authority__positivist_reading, '0877704a-4487-4a87-aba0-b042c8c06a97').
narrative_ontology:cs_kernel_codification('0877704a-4487-4a87-aba0-b042c8c06a97', formalized).
narrative_ontology:cs_authority_grounding('0877704a-4487-4a87-aba0-b042c8c06a97', lineage).
narrative_ontology:cs_interpretation_layer_present('0877704a-4487-4a87-aba0-b042c8c06a97').
narrative_ontology:cs_reading_relation('0877704a-4487-4a87-aba0-b042c8c06a97', constitutional_text_authority__originalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('0877704a-4487-4a87-aba0-b042c8c06a97', constitutional_text_authority__living_constitutionalist_reading, forecloses).
narrative_ontology:cs_axiom('0877704a-4487-4a87-aba0-b042c8c06a97', foundational, validity_grounded_in_enactment_pedigree).
narrative_ontology:cs_axiom_status(validity_grounded_in_enactment_pedigree, holdable).
narrative_ontology:cs_axiom_grounding('0877704a-4487-4a87-aba0-b042c8c06a97', validity_grounded_in_enactment_pedigree, conventional).
narrative_ontology:cs_axiom('0877704a-4487-4a87-aba0-b042c8c06a97', secondary, moral_content_irrelevant_to_legal_validity).
narrative_ontology:cs_axiom_status(moral_content_irrelevant_to_legal_validity, holdable).
narrative_ontology:cs_axiom_grounding('0877704a-4487-4a87-aba0-b042c8c06a97', moral_content_irrelevant_to_legal_validity, conventional).
narrative_ontology:cs_reference_frame('0877704a-4487-4a87-aba0-b042c8c06a97', enacted_pedigree_validity_framework).
narrative_ontology:cs_drift_state('0877704a-4487-4a87-aba0-b042c8c06a97', contemporary_adjudicative_practice, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('0877704a-4487-4a87-aba0-b042c8c06a97', '').
narrative_ontology:cs_kernel_id(constitutional_text_authority__positivist_reading, constitutional_text_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(constitutional_text_authority__positivist_reading, legislative_majorities).
narrative_ontology:constraint_beneficiary(constitutional_text_authority__positivist_reading, formalist_judiciary).
narrative_ontology:constraint_beneficiary(constitutional_text_authority__positivist_reading, positivist_legal_academy).
narrative_ontology:constraint_victim(constitutional_text_authority__positivist_reading, unjust_law_subjects).
narrative_ontology:constraint_victim(constitutional_text_authority__positivist_reading, moral_invalidity_claimants).
narrative_ontology:constraint_vindicates(constitutional_text_authority__positivist_reading, separability_thesis).
narrative_ontology:constraint_vindicates(constitutional_text_authority__positivist_reading, rule_of_recognition_doctrine).
narrative_ontology:constraint_vindicates(constitutional_text_authority__positivist_reading, sources_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Enactments acquire binding constitutional and statutory validity by passing formal procedure alone — sponsorship, readings, votes, promulgation. No moral-content test can strip validity from what they enact; challenges to morally objectionable but procedurally proper law are routed back to them as requests for legislative repeal, a forum where they hold the numbers. They operate the procedure that constitutes the validity test itself.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__positivist_reading, legislative_majorities, beneficiary,
    institutional, generational, mobile, national).

% Administer the source-based validity test and police the boundary between legal and moral argument: they decide which arguments enter the validity register. The role supplies decision-economy (pedigree is checkable; moral truth is not) and legitimation cover ('the court applies the law; it does not make it'). Professional identity is constituted through this self-description — a judge who adjudicates moral content directly has, on this reading, stopped doing the judicial job, which makes abandoning the frame a professional identity break rather than a doctrinal preference.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__positivist_reading, formalist_judiciary, agenda_setter,
    institutional, generational, identity_locked, national).

% Produce, refine, and teach the separability doctrine and the rule of recognition; credential the officials who administer the validity test; referee appointments and prestige within jurisprudence. Careers, chairs, and canonical syllabi are built on the law/morality distinction as the organizing insight of the discipline. They collect doctrinal authority without themselves wielding coercive power.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__positivist_reading, positivist_legal_academy, beneficiary,
    organized, generational, identity_locked, continental).
narrative_ontology:stakeholder_secondary_role(constitutional_text_authority__positivist_reading, positivist_legal_academy, agenda_setter).

% Live under formally valid but morally objectionable enactments. Their moral objection to the law is ruled legally irrelevant to its validity; the prescribed remedy is legislative reform, a forum in which they are by hypothesis a minority without the numbers. Exit is emigration, disobedience at criminal cost, or waiting out a political cycle they do not control.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__positivist_reading, unjust_law_subjects, payer,
    powerless, biographical, trapped, national).

% Litigants and advocates whose core claim is that grave injustice defeats legal validity. The validity register excludes that argument type, so they must redescribe moral claims as textual, structural, or procedural defects — a translation that discards cases where the text is clear and the injustice is the point. They can keep litigating, but only in a vocabulary that has already conceded the frame.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__positivist_reading, moral_invalidity_claimants, payer,
    moderate, biographical, constrained, national).

% Scholars and practitioners holding that unjust enactments can fail to be law in the fullest sense. They remain free to publish and teach, but their argument is structurally barred from the adjudicative validity register — it is treated as a moral critique of law, not a legal argument about it. Their objection would be that the exclusion itself, not any particular enactment, is the injury.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__positivist_reading, natural_law_advocates, excluded,
    moderate, generational, constrained, continental).

% Compare validity doctrines across jurisdictions — systems that adopted Radbruch-style extreme-injustice exceptions, systems with explicit moral-review clauses, systems maintaining strict pedigree tests. They collect no rents from any arrangement and can describe each system's actual operative criterion, including gaps between professed and practiced validity tests.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__positivist_reading, comparative_constitutional_scholars, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(constitutional_text_authority__positivist_reading, legislative_majorities).
narrative_ontology:fixing_cost_class(constitutional_text_authority__positivist_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a content-neutral criterion for identifying valid law: officials, courts, and citizens can converge on what the law is by checking enactment pedigree, without first resolving contested moral questions. In conditions of deep moral and religious pluralism this coordinates legal recognition across parties who agree on nothing else.
% TRANSFER_FUNCTION: Moves legitimation cost and argumentative burden. Enactments of procedurally proper majorities are insulated from moral-invalidity challenge; the burden of contesting unjust-but-valid law is transferred from adjudication (where the moral argument would be cognizable) to legislative reform (slow, majoritarian). Officials and enacting majorities receive decision-economy and legitimation; subjects of unjust valid law and moral-invalidity claimants carry the displaced moral cost.
% ABSENT_VOICES: Natural law advocates and moral-invalidity claimants are physically present in the academy and the courts but structurally absent from the validity conversation — their core argument (grave injustice defeats validity) is ruled out of register. Retrospective claimants against historically unjust valid regimes are absent entirely; their cases surface only as theoretical embarrassments (the Radbruch problem) rather than live legal claims.
% DISAPPEARANCE_RATIONALE: If the source-based validity test vanished, every validity determination would require another criterion — contemporary moral principle, ratification-fixed meaning, or convergent practice — and adjudication would reorganize around whichever replaced it; in that sense the world rearranges. But incorporation clauses and soft-positivist practice already admit moral content at the margins of many systems, so practitioners dispute whether removal would transform outcomes or merely redescribe what courts already do. The dispute itself is the honest verdict.
% FOUNDING_PROBLEM: How to identify valid law and coordinate official practice in a polity that no longer shares a moral theology: after the collapse of a common natural-law framework, legal systems needed a validity criterion not hostage to contested moral truth, and legislatures needed to make binding law for pluralist societies without each enactment refighting the moral wars.
% FOUNDING_PROBLEM_CORROBORATION: Legal historians outside the positivist academy attest the post-Reformation pluralism-and-identification problem as the actual historical driver. Comparative constitutional scholars attest that pedigree-based validity tests continue to coordinate official practice in morally diverse states. Natural law critics of the Finnis type concede the identification problem is real while disputing the pedigree-only answer; post-1945 Radbruch courts attest the problem was live but held the answer insufficient at the extremes — corroborating the founding problem while contesting this arrangement's solution.
narrative_ontology:disappearance_verdict(constitutional_text_authority__positivist_reading, contested).
narrative_ontology:founding_problem_status(constitutional_text_authority__positivist_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(constitutional_text_authority__positivist_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-18',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k3', 'max_tokens=32000,temperature=default,reasoning=max').
narrative_ontology:story_seed(constitutional_text_authority__positivist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(constitutional_text_authority__positivist_reading, 0.45, 'kimi-k3', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(constitutional_text_authority__positivist_reading_tests).
:- end_tests(constitutional_text_authority__positivist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.45) rather than high because the coordination function is real and load-bearing — pedigree-based validity genuinely solves the pluralist identification problem, and the extraction is the exclusion of one argument class rather than open-ended rent. The series falls slightly across the interval (0.52 to 0.45) as post-1945 Radbruch-style exceptions and incorporationist softening admit moral content at the margins, shrinking the victim set. Suppression is moderate (0.50) and falling (0.62 to 0.50): the law/morality boundary is still actively policed — argument-type exclusion is enforcement, not preference — but doctrinal enforcement has decayed as moral reasoning normalizes in constitutional adjudication; this is an enforcement-decay narrative, which is why suppression_requirement is tracked. Theater rises (0.18 to 0.34): the formalist self-description ('we apply the enacted law, we do not make moral judgments') increasingly diverges from adjudicative practice that deploys proportionality, dignity, and moral reading while retaining the pedigree rhetoric. Accessibility_collapse is partial (0.42) — alternative validity criteria (moral readings, historical-meaning readings) persist and operate in rival courts and doctrines. Resistance is substantial (0.58): natural law revival, Dworkinian moral reading, and living constitutionalism contest the doctrine continuously, and the post-war era produced an actual institutional defeat at the Radbruch margin. All three series share one time grid (0, 6, 12, 18, 24, 30); final values match base_properties.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter seat (formalist_judiciary) and the beneficiary seats (legislative_majorities, positivist_legal_academy) the arrangement computes as coordination they built, administer, or profit from — validity is checkable, legitimation is supplied, and no one in these seats bears the excluded-argument cost. From the payer seats (unjust_law_subjects, moral_invalidity_claimants) the same structure operates as enforced extraction: an entire class of their arguments is ruled out of register and their remedy is routed through a forum structurally stacked against them. The engine computes this divergence from the declared beneficiary/victim structure and exit options; the authored claim does not adjudicate between the seats.
 *
 * DIRECTIONALITY LOGIC:
 *   Legislative majorities sit nearest the full-beneficiary end: the validity test subsidizes everything they enact by stripping a challenge class, and they are mobile within the procedure they control. The judiciary and academy are beneficiaries with identity-locked exit — they collect decision-economy, legitimation, and doctrinal authority, and their lock-in is professional identity (the judicial role IS pedigree application on this reading), not coercion. Unjust-law subjects are trapped full targets: valid law binds them, exit is emigration or punishment, and their numbers disadvantage in the routed remedy is the structural injury. Moral-invalidity claimants are constrained targets: resourced enough to litigate, but only in a translated vocabulary that concedes the frame. Natural law advocates are excluded rather than targeted — their exclusion from the register is itself the enforcement object. Suppression is authored as a raw structural property, unscaled; the engine scales extractiveness by directionality and scope.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — identifying valid law and coordinating official practice under deep moral pluralism — is live, corroborated from outside the beneficiary set by legal historians, comparative scholars, and even the doctrine's natural law critics. So this is not mandatrophy: no zombie arrangement, and the (live x contested) pairing raises no capture/zombie flag. The classification's guard function runs both directions: calling the positivist reading a pure snare ignores the genuine coordination achievement that even its critics concede; calling it a pure rope ignores the asymmetric exclusion of moral-invalidity argument and the identity-locked enforcement establishment that maintains the boundary. The tangled claim records both, and the falling extraction/suppression series records that the entanglement has loosened as moral content re-entered at the margins.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    positivist_reading_committer_frame,
    'This constraint instantiates only the positivist reading of the constitutional_text_authority kernel. Would the sibling readings instantiate structurally different constraints — and is the disagreement located exactly at the validity condition (pedigree-only vs pedigree-plus-historical-meaning vs contemporary-moral-principle)?',
    'Independent classification of the sibling constraint files (constitutional_text_authority__originalist_reading, constitutional_text_authority__living_constitutionalist_reading); comparison of their computed per-seat types, epsilons, and victim sets against this file; empirical observation of which validity criterion operative courts actually apply when the readings conflict.',
    'If the living constitutionalist reading describes operative practice, this file''s victim set (moral_invalidity_claimants, unjust_law_subjects) shrinks and its extraction belongs to doctrinal discourse rather than operative law; if the originalist reading is operative, the victim set changes composition (parties injured by ratification-fixed meaning rather than by moral-content exclusion). The epsilon authored here is stable only within this reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(positivist_reading_committer_frame, conceptual, 'Committer structure: one reading of a contested kernel; disagreement located at the validity condition.').

omega_variable(
    coordination_extraction_separability,
    'Is the exclusion of moral-invalidity arguments an inseparable cost of coordinating legality under deep moral pluralism, or is it an asymmetric extraction removable without collapsing the coordination function — e.g., by a Radbruch-style extreme-injustice exception?',
    'Comparative evidence from jurisdictions that adopted the Radbruch formula or explicit moral-review clauses after 1945: if source-based validity continued to coordinate official practice while extreme-injustice invalidation operated at the margin, the components are separable and the measured extraction is genuinely removable.',
    'If separable, part of the measured extractiveness is asymmetric extraction and the tangled structure is confirmed with a demonstrated cheap fix (consistent with fixing_cost: cheap); if inseparable, the extraction is coordination cost and the effective epsilon should fall toward the rope floor.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_extraction_separability, empirical, 'Whether the law/morality exclusion is coordination cost or removable extraction.').

omega_variable(
    authority_grounding_framing_alternative,
    'Is this constraint''s authority grounding best framed as lineage (validity runs through the chain of valid enactments back to the founding enactment, as declared) or as practice (a Hartian rule of recognition whose content is fixed by convergent official practice)? Both are coherent positivist framings producing different cs_structure declarations.',
    'Analysis of how officials resolve pedigree doubts: appeal to the enactment chain and its authorizing rules supports lineage; appeal to what officials collectively accept supports practice. Cross-check cs_pattern outputs under both declarations for classification difference.',
    'Under the practice framing, drift migrates into official acceptance itself and the interpretation layer sits inside official custom rather than doctrine; the cs_pattern classification and the drift_state reading could change even though epsilon, stakeholders, and the claimed type do not.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(authority_grounding_framing_alternative, conceptual, 'Lineage vs practice grounding under-determination within the positivist frame (Kelsen vs Hart framing choice).').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(constitutional_text_authority__positivist_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cons_tr_t0, constitutional_text_authority__positivist_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(cons_tr_t6, constitutional_text_authority__positivist_reading, theater_ratio, 6, 0.21).
narrative_ontology:measurement(cons_tr_t12, constitutional_text_authority__positivist_reading, theater_ratio, 12, 0.24).
narrative_ontology:measurement(cons_tr_t18, constitutional_text_authority__positivist_reading, theater_ratio, 18, 0.28).
narrative_ontology:measurement(cons_tr_t24, constitutional_text_authority__positivist_reading, theater_ratio, 24, 0.31).
narrative_ontology:measurement(cons_tr_t30, constitutional_text_authority__positivist_reading, theater_ratio, 30, 0.34).

% Extraction over time
narrative_ontology:measurement(cons_be_t0, constitutional_text_authority__positivist_reading, base_extractiveness, 0, 0.52).
narrative_ontology:measurement(cons_be_t6, constitutional_text_authority__positivist_reading, base_extractiveness, 6, 0.51).
narrative_ontology:measurement(cons_be_t12, constitutional_text_authority__positivist_reading, base_extractiveness, 12, 0.49).
narrative_ontology:measurement(cons_be_t18, constitutional_text_authority__positivist_reading, base_extractiveness, 18, 0.48).
narrative_ontology:measurement(cons_be_t24, constitutional_text_authority__positivist_reading, base_extractiveness, 24, 0.46).
narrative_ontology:measurement(cons_be_t30, constitutional_text_authority__positivist_reading, base_extractiveness, 30, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(cons_su_t0, constitutional_text_authority__positivist_reading, suppression_requirement, 0, 0.62).
narrative_ontology:measurement(cons_su_t6, constitutional_text_authority__positivist_reading, suppression_requirement, 6, 0.6).
narrative_ontology:measurement(cons_su_t12, constitutional_text_authority__positivist_reading, suppression_requirement, 12, 0.57).
narrative_ontology:measurement(cons_su_t18, constitutional_text_authority__positivist_reading, suppression_requirement, 18, 0.55).
narrative_ontology:measurement(cons_su_t24, constitutional_text_authority__positivist_reading, suppression_requirement, 24, 0.52).
narrative_ontology:measurement(cons_su_t30, constitutional_text_authority__positivist_reading, suppression_requirement, 30, 0.5).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(constitutional_text_authority__positivist_reading, identity_coordination).
narrative_ontology:affects_constraint(constitutional_text_authority__positivist_reading, constitutional_text_authority__originalist_reading).
narrative_ontology:affects_constraint(constitutional_text_authority__positivist_reading, constitutional_text_authority__living_constitutionalist_reading).

% DUAL FORMULATION NOTE:
% Constraint family decomposition of the constitutional_text_authority kernel per the epsilon-invariance principle. The natural-language label 'constitutional text authority' conflates three structurally distinct validity claims: this file (positivist_reading: validity from enactment pedigree, moral content excluded, moderate extraction), the originalist_reading (adds ratification-fixed meaning as a further constraint on interpreters, different victim composition), and the living_constitutionalist_reading (re-admits moral content as authority-constitutive, different epsilon and a different payer set). The readings differ at the validity condition itself, so they are three constraints, not one constraint under three observables. This reading converges with originalism on text-fidelity (coexists_with) and logically forecloses the living reading's core premise within a single adjudicative framework (forecloses).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
