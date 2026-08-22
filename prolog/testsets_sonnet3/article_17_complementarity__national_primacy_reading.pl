% ============================================================================
% CONSTRAINT STORY: article_17_complementarity__national_primacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_article_17_complementarity__national_primacy_reading, []).

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
 *   constraint_id: article_17_complementarity__national_primacy_reading
 *   human_readable: Article 17 Complementarity — National Primacy Reading
 *   domain: international_law/criminal_justice/state_sovereignty
 *
 * SUMMARY:
 *   This constraint instantiates the national-primacy reading of Article 17
 *   complementarity within the Rome Statute system: national courts are
 *   presumptively adequate forums for prosecuting international crimes, and
 *   the ICC bears an affirmative burden to demonstrate that domestic
 *   proceedings are a sham or that the state is unwilling or unable genuinely
 *   to carry them out before the Court may assert jurisdiction. This reading
 *   treats complementarity primarily as a sovereignty-protection mechanism —
 *   a guarantee that ratifying the Rome Statute would not subordinate
 *   national legal systems to an untested supranational court. The sibling
 *   reading, international_oversight_reading, treats the same textual
 *   provision as an accountability-trigger mechanism with a low threshold for
 *   'unwilling or unable,' interpreted broadly to capture elite immunity and
 *   victor's justice. The two readings share the same kernel text (Article
 *   17's admissibility criteria) but diverge sharply on where the evidentiary
 *   burden sits and how much domestic institutional weakness the ICC must
 *   show before displacing national jurisdiction — producing very different
 *   victim sets and very different ε values, which is why they are authored
 *   as separate constraint stories rather than one story with an interpretive
 *   parameter.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(article_17_complementarity__national_primacy_reading, 0.42).
domain_priors:suppression_score(article_17_complementarity__national_primacy_reading, 0.38).
domain_priors:theater_ratio(article_17_complementarity__national_primacy_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(article_17_complementarity__national_primacy_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(article_17_complementarity__national_primacy_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(article_17_complementarity__national_primacy_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(article_17_complementarity__national_primacy_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(article_17_complementarity__national_primacy_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(article_17_complementarity__national_primacy_reading, tangled_rope).
narrative_ontology:human_readable(article_17_complementarity__national_primacy_reading, "Article 17 Complementarity — National Primacy Reading").
narrative_ontology:topic_domain(article_17_complementarity__national_primacy_reading, "international_law/criminal_justice/state_sovereignty").

domain_priors:requires_active_enforcement(article_17_complementarity__national_primacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(article_17_complementarity__national_primacy_reading, '76ccb070-c3a6-4167-96da-d411330f8b40').
narrative_ontology:cs_kernel_codification('76ccb070-c3a6-4167-96da-d411330f8b40', fixed_text).
narrative_ontology:cs_authority_grounding('76ccb070-c3a6-4167-96da-d411330f8b40', lineage).
narrative_ontology:cs_interpretation_layer_present('76ccb070-c3a6-4167-96da-d411330f8b40').
narrative_ontology:cs_reading_relation('76ccb070-c3a6-4167-96da-d411330f8b40', article_17_complementarity__international_oversight_reading, coexists_with).
narrative_ontology:cs_axiom('76ccb070-c3a6-4167-96da-d411330f8b40', foundational, national_proceedings_presumptively_genuine).
narrative_ontology:cs_axiom_status(national_proceedings_presumptively_genuine, holdable).
narrative_ontology:cs_axiom_grounding('76ccb070-c3a6-4167-96da-d411330f8b40', national_proceedings_presumptively_genuine, conventional).
narrative_ontology:cs_axiom('76ccb070-c3a6-4167-96da-d411330f8b40', foundational, burden_of_inadmissibility_rests_on_icc).
narrative_ontology:cs_axiom_status(burden_of_inadmissibility_rests_on_icc, holdable).
narrative_ontology:cs_axiom_grounding('76ccb070-c3a6-4167-96da-d411330f8b40', burden_of_inadmissibility_rests_on_icc, conventional).
narrative_ontology:cs_reference_frame('76ccb070-c3a6-4167-96da-d411330f8b40', rome_statute_ratification_compromise).
narrative_ontology:cs_drift_state('76ccb070-c3a6-4167-96da-d411330f8b40', post_kenya_libya_admissibility_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('76ccb070-c3a6-4167-96da-d411330f8b40', '').
narrative_ontology:cs_kernel_id(article_17_complementarity__national_primacy_reading, article_17_complementarity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(article_17_complementarity__national_primacy_reading, national_judiciaries).
narrative_ontology:constraint_beneficiary(article_17_complementarity__national_primacy_reading, sovereignty_maximizing_states).
narrative_ontology:constraint_beneficiary(article_17_complementarity__national_primacy_reading, state_security_apparatuses).
narrative_ontology:constraint_victim(article_17_complementarity__national_primacy_reading, victims_in_states_with_sham_proceedings).
narrative_ontology:constraint_victim(article_17_complementarity__national_primacy_reading, victims_in_states_with_weak_but_genuine_proceedings).
narrative_ontology:constraint_victim(article_17_complementarity__national_primacy_reading, atrocity_survivors_awaiting_prosecution).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Retain first-priority jurisdiction over international crimes committed within or by their nationals. Under this reading they are presumed adequate unless the ICC can affirmatively prove the proceedings are a sham designed to shield the accused. They control the evidentiary record, the pace of proceedings, and the characterization of their own institutional capacity — all facts the ICC must overcome to displace them.
narrative_ontology:constraint_stakeholder(article_17_complementarity__national_primacy_reading, national_judiciaries, beneficiary,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(article_17_complementarity__national_primacy_reading, national_judiciaries, agenda_setter).

% States that value the principle that no external body may substitute its judgment for a national legal system's absent proof of collapse. They shape the treaty's interpretive practice through diplomatic pressure, non-cooperation threats, and appointments to internal ICC bodies, ensuring the admissibility threshold stays high and the burden stays on the Court.
narrative_ontology:constraint_stakeholder(article_17_complementarity__national_primacy_reading, sovereignty_maximizing_states, beneficiary,
    institutional, civilizational, arbitrage, global).

% Military and intelligence structures implicated in alleged atrocities benefit when domestic proceedings — however slow, under-resourced, or narrowly scoped — count as 'genuine' enough to bar ICC jurisdiction. They supply the evidence and witnesses that make a domestic case appear active, controlling its tempo without technically obstructing it.
narrative_ontology:constraint_stakeholder(article_17_complementarity__national_primacy_reading, state_security_apparatuses, beneficiary,
    institutional, generational, arbitrage, national).

% Live in states running proceedings designed to protect perpetrators rather than convict them. Because the ICC bears the burden of proving the sham, and sham proceedings are built to look procedurally adequate, these victims often wait years for the evidentiary threshold to be met, if it ever is. They have no standing to bring the admissibility challenge themselves.
narrative_ontology:constraint_stakeholder(article_17_complementarity__national_primacy_reading, victims_in_states_with_sham_proceedings, payer,
    powerless, biographical, trapped, local).

% Live in states whose judiciaries are genuinely trying but under-resourced, slow, or lacking capacity for complex atrocity crimes prosecution. Under the national-primacy reading, 'genuine effort' — not outcome or capacity — is presumptively sufficient to keep the case out of the ICC's reach, so these victims fall structurally outside the Court's jurisdiction even though justice may never actually arrive domestically.
narrative_ontology:constraint_stakeholder(article_17_complementarity__national_primacy_reading, victims_in_states_with_weak_but_genuine_proceedings, payer,
    powerless, biographical, trapped, local).

% Individuals and communities who testified, filed complaints, or otherwise engaged with domestic mechanisms that later stalled. The high inadmissibility threshold means their cases can sit in domestic limbo indefinitely without triggering ICC review, because indefinite delay short of proven bad faith does not meet the 'unwilling or unable' bar under this reading.
narrative_ontology:constraint_stakeholder(article_17_complementarity__national_primacy_reading, atrocity_survivors_awaiting_prosecution, payer,
    powerless, biographical, trapped, local).

% Must build an evidentiary case that a national proceeding is a sham or the state is unwilling/unable, before it may even open an investigation over state objection. Lacks independent investigative access inside the state's territory absent cooperation, so gathering the very evidence needed to meet its burden is often blocked by the state whose adequacy is in question.
narrative_ontology:constraint_stakeholder(article_17_complementarity__national_primacy_reading, icc_office_of_the_prosecutor, agenda_setter,
    organized, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(article_17_complementarity__national_primacy_reading, icc_office_of_the_prosecutor, excluded).

% Document sham proceedings and victim testimony and press for ICC intervention, but have no formal role in the admissibility determination itself. Their reports are treated as supplementary evidence at most, never as a trigger on their own for shifting the burden.
narrative_ontology:constraint_stakeholder(article_17_complementarity__national_primacy_reading, human_rights_ngos, excluded,
    organized, biographical, constrained, global).

% Can refer situations to the ICC or defer investigations under Article 16, layering a second political filter on top of the complementarity analysis. Its permanent members, several of which are strong sovereignty-maximizing states, shape which situations even reach the admissibility question.
narrative_ontology:constraint_stakeholder(article_17_complementarity__national_primacy_reading, un_security_council, observer,
    institutional, generational, analytical, global).
narrative_ontology:stakeholder_secondary_role(article_17_complementarity__national_primacy_reading, un_security_council, agenda_setter).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(article_17_complementarity__national_primacy_reading, diffuse).
narrative_ontology:fixing_cost_class(article_17_complementarity__national_primacy_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Prevents the ICC from becoming a court of first instance that displaces functioning domestic legal systems, preserving each state's primary right and responsibility to prosecute its own nationals and avoiding duplicative, costly, and legitimacy-draining parallel prosecutions.
% TRANSFER_FUNCTION: Moves the burden of proof from the state (to show its proceedings are adequate) to the ICC (to show they are not), and correspondingly moves practical access to international accountability away from victims in weak-but-genuine or slow domestic systems toward states that can maintain a facially adequate process.
% ABSENT_VOICES: Individual victims and survivor communities have no standing to trigger or contest an admissibility determination directly; human rights NGOs can submit information but cannot compel the Prosecutor's burden to shift. Their voices enter only as evidence, never as parties to the Article 17 analysis.
% DISAPPEARANCE_RATIONALE: If the national-primacy reading were displaced by a lower admissibility threshold, dozens of pending situations where domestic proceedings are slow-moving but not proven shams would become eligible for ICC review; sovereignty-maximizing states would face materially increased exposure to Court jurisdiction, and national judiciaries would lose their present near-automatic priority.
% FOUNDING_PROBLEM: The Rome Statute's drafters needed to reconcile universal jurisdiction over atrocity crimes with the reality that no state would ratify a treaty subordinating its judiciary to an untested international court; complementarity was the compromise that secured broad ratification by guaranteeing national courts first priority.
% FOUNDING_PROBLEM_CORROBORATION: States parties and their diplomatic representatives attest the founding problem (protecting sovereignty to secure ratification) remains live and the high threshold is still necessary. Independent bodies outside the beneficiary set — including ICC Pre-Trial Chamber jurisprudence critical commentary, UN human rights special rapporteurs, and victim-representative submissions in cases such as the Kenya and Libya situations — attest that the threshold as applied has shifted from protecting genuine domestic capacity to shielding non-prosecution, i.e. the founding problem has been substantially resolved for many states but the high-burden arrangement persists as a shield rather than a bridge.
narrative_ontology:disappearance_verdict(article_17_complementarity__national_primacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(article_17_complementarity__national_primacy_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(article_17_complementarity__national_primacy_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(article_17_complementarity__national_primacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(article_17_complementarity__national_primacy_reading, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(article_17_complementarity__national_primacy_reading_tests).
:- end_tests(article_17_complementarity__national_primacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.42 at interval end) because the reading's core function — preventing wasteful parallel prosecutions and honoring functioning domestic institutions — is a genuine coordination good; the extraction is concentrated in the residual category of victims whose domestic proceedings are technically ongoing but functionally inert, a category that has grown as states have learned to maintain facially adequate proceedings specifically to bar ICC jurisdiction (reflected in the rising extractiveness and suppression_requirement series). Theater ratio rises modestly (0.15 to 0.30) as more domestic proceedings are initiated primarily to satisfy the admissibility threshold rather than to prosecute. Suppression is substantially lower than extraction because this reading operates mostly through evidentiary burden allocation rather than direct coercive enforcement — the 'suppression' here is the structural difficulty of proving a negative (that a state is NOT genuinely proceeding) rather than active coercion of victims.
 *
 * PERSPECTIVAL GAP:
 *   From the seat of a sovereignty-maximizing state or a national judiciary, this constraint reads as principled subsidiarity — a rope preserving legitimate institutional hierarchy. From the seat of a victim in a state running a facially adequate but substantively inert proceeding, the same structure reads as an extraction mechanism: a legal threshold engineered to be met by appearance rather than outcome, converting a coordination principle into a shield against accountability. The engine computes these divergent seat classifications from the same structural data; this story does not resolve which seat is correct, only that the divergence is structurally produced by the burden-allocation choice this reading makes.
 *
 * DIRECTIONALITY LOGIC:
 *   National judiciaries and sovereignty-maximizing states are structural beneficiaries: the reading gives them the presumption of adequacy and places the burden of displacement on the ICC, which they can resist through non-cooperation. State security apparatuses benefit indirectly by controlling the pace and appearance of domestic proceedings without needing to obstruct them outright. Victims in all three named groups are targets: their access to any accountability forum depends entirely on whether the ICC can clear an evidentiary bar it often cannot clear without state cooperation — the very cooperation the accused state controls. The ICC Office of the Prosecutor sits in an unusual structural position: nominally an agenda-setter under the Statute, but functionally constrained because its investigative access depends on the cooperation of the state whose adequacy is being assessed.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (justifying ratification by guaranteeing sovereignty) was genuinely live in 1998 and substantially resolved for many states by the 2010s as domestic capacity-building matured; but the high-burden architecture that solved that founding problem has persisted unchanged even as its operative effect shifted from protecting genuine capacity-building states toward shielding states running proceedings calibrated purely to defeat admissibility. The founding_problem_status of 'contested' reflects that some states' domestic problem remains genuinely live while for others it is functionally dead, and the single admissibility standard does not distinguish between them — this is exactly the kind of divergence the disappearance-verdict and founding-problem-status fields exist to surface rather than resolve by fiat.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sham_proof_evidentiary_asymmetry,
    'Is the high evidentiary burden the ICC bears to prove a national proceeding is a ''sham'' a legitimate safeguard against premature international intervention, or a structurally self-defeating requirement given that the ICC lacks independent investigative access inside the state whose adequacy is being assessed?',
    'Comparative analysis of ICC admissibility rulings (e.g., Kenya, Libya, Uganda situations) tracking how often the Prosecutor''s burden was met versus how often cases stalled for lack of evidence the state itself controlled and withheld.',
    'If the burden is structurally unmeetable without state cooperation, the national-primacy reading functions less as principled subsidiarity and more as an extraction mechanism dressed as a sovereignty safeguard, which would push the computed classification toward snare or tangled_rope with a heavier extraction weighting.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sham_proof_evidentiary_asymmetry, empirical, 'Whether the ICC''s evidentiary burden under this reading is practically satisfiable given state control over evidence.').

omega_variable(
    kernel_reading_disagreement_locus,
    'Is the disagreement between the national_primacy_reading and the international_oversight_reading located in the interpretation of ''unwilling or unable'' (a textual/interpretive question) or in a prior normative commitment about where sovereignty should yield to accountability (a values question that interpretation merely encodes)?',
    'Doctrinal analysis of ICC Appeals Chamber jurisprudence and travaux préparatoires to determine whether the text itself underdetermines the threshold, versus survey of state and NGO positions to determine whether the disagreement predates and drives the interpretive choice.',
    'If the disagreement is prior and normative, no textual clarification of Article 17 can resolve it and the kernel will continue to emit two structurally stable, mutually irreconcilable constraints; if it is genuinely textual/interpretive, doctrinal convergence over time is possible and the two readings could eventually merge into one.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_disagreement_locus, conceptual, 'Whether the kernel''s two readings reflect an interpretive gap or a prior normative fork.').

omega_variable(
    genuine_capacity_vs_appearance_calibration,
    'Is the rising extractiveness trend (0.22 to 0.42) driven by states genuinely improving domestic capacity in ways that legitimately raise the bar for ICC intervention, or by states learning to calibrate proceedings specifically to survive admissibility review without delivering substantive justice?',
    'Longitudinal case-outcome tracking: compare conviction rates, sentence severity, and case closure patterns in states whose proceedings successfully defeated ICC admissibility challenges against baseline domestic prosecution rates for comparable ordinary crimes.',
    'If calibration-to-survive is the dominant driver, the rising extractiveness reflects genuine institutional learning to exploit the burden allocation, strengthening the case that this reading has drifted from coordination toward extraction over the measured interval.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(genuine_capacity_vs_appearance_calibration, empirical, 'Whether rising extraction reflects genuine capacity growth or strategic proceeding-calibration to defeat admissibility.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(article_17_complementarity__national_primacy_reading, 1998, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(arti_tr_t1998, article_17_complementarity__national_primacy_reading, theater_ratio, 1998, 0.15).
narrative_ontology:measurement(arti_tr_t2002, article_17_complementarity__national_primacy_reading, theater_ratio, 2002, 0.18).
narrative_ontology:measurement(arti_tr_t2008, article_17_complementarity__national_primacy_reading, theater_ratio, 2008, 0.22).
narrative_ontology:measurement(arti_tr_t2013, article_17_complementarity__national_primacy_reading, theater_ratio, 2013, 0.26).
narrative_ontology:measurement(arti_tr_t2018, article_17_complementarity__national_primacy_reading, theater_ratio, 2018, 0.29).
narrative_ontology:measurement(arti_tr_t2024, article_17_complementarity__national_primacy_reading, theater_ratio, 2024, 0.3).

% Extraction over time
narrative_ontology:measurement(arti_be_t1998, article_17_complementarity__national_primacy_reading, base_extractiveness, 1998, 0.22).
narrative_ontology:measurement(arti_be_t2002, article_17_complementarity__national_primacy_reading, base_extractiveness, 2002, 0.28).
narrative_ontology:measurement(arti_be_t2008, article_17_complementarity__national_primacy_reading, base_extractiveness, 2008, 0.33).
narrative_ontology:measurement(arti_be_t2013, article_17_complementarity__national_primacy_reading, base_extractiveness, 2013, 0.37).
narrative_ontology:measurement(arti_be_t2018, article_17_complementarity__national_primacy_reading, base_extractiveness, 2018, 0.4).
narrative_ontology:measurement(arti_be_t2024, article_17_complementarity__national_primacy_reading, base_extractiveness, 2024, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(arti_su_t1998, article_17_complementarity__national_primacy_reading, suppression_requirement, 1998, 0.25).
narrative_ontology:measurement(arti_su_t2002, article_17_complementarity__national_primacy_reading, suppression_requirement, 2002, 0.28).
narrative_ontology:measurement(arti_su_t2008, article_17_complementarity__national_primacy_reading, suppression_requirement, 2008, 0.31).
narrative_ontology:measurement(arti_su_t2013, article_17_complementarity__national_primacy_reading, suppression_requirement, 2013, 0.34).
narrative_ontology:measurement(arti_su_t2018, article_17_complementarity__national_primacy_reading, suppression_requirement, 2018, 0.36).
narrative_ontology:measurement(arti_su_t2024, article_17_complementarity__national_primacy_reading, suppression_requirement, 2024, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(article_17_complementarity__national_primacy_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(article_17_complementarity__national_primacy_reading, 0.12).
narrative_ontology:affects_constraint(article_17_complementarity__national_primacy_reading, article_17_complementarity__international_oversight_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of two readings of the article_17_complementarity kernel. The sibling, article_17_complementarity__international_oversight_reading, authors a lower admissibility threshold, a broader victim set (extending to weak-but-genuine proceedings), and correspondingly higher extractiveness attributed to state non-cooperation and elite shielding. The two stories share the kernel text but diverge on burden allocation, beneficiary set, and victim set, producing distinct and stable ε values consistent with the ε-invariance principle — they are linked here rather than merged.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
