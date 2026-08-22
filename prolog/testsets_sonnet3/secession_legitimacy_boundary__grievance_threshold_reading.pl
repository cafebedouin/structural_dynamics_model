% ============================================================================
% CONSTRAINT STORY: secession_legitimacy_boundary__grievance_threshold_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_secession_legitimacy_boundary__grievance_threshold_reading, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
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
 *   constraint_id: secession_legitimacy_boundary__grievance_threshold_reading
 *   human_readable: Grievance-Threshold Reading of the Secession Legitimacy Boundary
 *   domain: political/federalism/resource_politics
 *
 * SUMMARY:
 *   This story instantiates the grievance-threshold reading of the contested
 *   secession-legitimacy kernel: the claim that secession becomes legitimate
 *   once federal actions cross an objective threshold of structural
 *   injustice, independent of what the constitutional text permits. Unlike
 *   the constitutional-impossibility reading (which forecloses unilateral
 *   exit entirely) or the popular-sovereignty reading (which makes a
 *   referendum self-legitimating), this reading conditions legitimacy on a
 *   substantive injustice test with a claimed burden of proof. The ε authored
 *   here is for the standing arrangement AS THIS READING SEES IT: a threshold
 *   doctrine currently being asserted and contested, whose 'objective burden
 *   of proof' is in practice set and adjudicated largely by the secessionist
 *   coalition and its aligned resource-sector funders, producing real
 *   extraction (redirected resource rents, externalized fiscal costs,
 *   subordinated treaty and minority claims) even though the doctrine
 *   presents itself as a neutral justice test.
 *
 * KEY AGENTS:
 *   - secessionist_political_movements: agenda_setter, defines and elevates the threshold claim
 *   - provincial_resource_sector_elites: beneficiary, captures resource rents if threshold is judged crossed
 *   - provincial_minority_populations: payer, lose federal protections without consenting to the narrative
 *   - federal_transfer_recipient_regions: payer, absorb fiscal shortfall from a decision they had no part in
 *   - border_indigenous_nations: payer/excluded, treaty priority subordinated to the threshold question
 *   - federal_government: excluded, its textual counter-argument is relativized rather than dispositive
 *   - constitutional_courts: observer, asked to adjudicate a test with no settled evidentiary standard
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(secession_legitimacy_boundary__grievance_threshold_reading, 0.62).
domain_priors:suppression_score(secession_legitimacy_boundary__grievance_threshold_reading, 0.58).
domain_priors:theater_ratio(secession_legitimacy_boundary__grievance_threshold_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(secession_legitimacy_boundary__grievance_threshold_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(secession_legitimacy_boundary__grievance_threshold_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(secession_legitimacy_boundary__grievance_threshold_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(secession_legitimacy_boundary__grievance_threshold_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(secession_legitimacy_boundary__grievance_threshold_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(secession_legitimacy_boundary__grievance_threshold_reading, tangled_rope).
narrative_ontology:human_readable(secession_legitimacy_boundary__grievance_threshold_reading, "Grievance-Threshold Reading of the Secession Legitimacy Boundary").
narrative_ontology:topic_domain(secession_legitimacy_boundary__grievance_threshold_reading, "political/federalism/resource_politics").

domain_priors:requires_active_enforcement(secession_legitimacy_boundary__grievance_threshold_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(secession_legitimacy_boundary__grievance_threshold_reading, 'e35efea2-4a3f-4f96-98e2-71c6ddff21b1').
narrative_ontology:cs_kernel_codification('e35efea2-4a3f-4f96-98e2-71c6ddff21b1', distributed).
narrative_ontology:cs_authority_grounding('e35efea2-4a3f-4f96-98e2-71c6ddff21b1', distributed).
narrative_ontology:cs_reading_relation('e35efea2-4a3f-4f96-98e2-71c6ddff21b1', secession_legitimacy_boundary__constitutional_impossibility_reading, forecloses).
narrative_ontology:cs_reading_relation('e35efea2-4a3f-4f96-98e2-71c6ddff21b1', secession_legitimacy_boundary__popular_sovereignty_reading, coexists_with).
narrative_ontology:cs_reading_relation('e35efea2-4a3f-4f96-98e2-71c6ddff21b1', secession_legitimacy_boundary__treaty_primacy_reading, influences).
narrative_ontology:cs_axiom('e35efea2-4a3f-4f96-98e2-71c6ddff21b1', foundational, structural_injustice_overrides_textual_finality).
narrative_ontology:cs_axiom_status(structural_injustice_overrides_textual_finality, holdable).
narrative_ontology:cs_axiom_grounding('e35efea2-4a3f-4f96-98e2-71c6ddff21b1', structural_injustice_overrides_textual_finality, deontological).
narrative_ontology:cs_axiom('e35efea2-4a3f-4f96-98e2-71c6ddff21b1', foundational, injustice_threshold_is_objectively_ascertainable).
narrative_ontology:cs_axiom_status(injustice_threshold_is_objectively_ascertainable, holdable).
narrative_ontology:cs_axiom_grounding('e35efea2-4a3f-4f96-98e2-71c6ddff21b1', injustice_threshold_is_objectively_ascertainable, empirically_contingent).
narrative_ontology:cs_reference_frame('e35efea2-4a3f-4f96-98e2-71c6ddff21b1', constitutional_text_as_defeasible_by_severe_injustice).
narrative_ontology:cs_drift_state('e35efea2-4a3f-4f96-98e2-71c6ddff21b1', contemporary_resource_federalism_disputes, gap(revival_pressure, substantial, false)).
narrative_ontology:cs_created_at('e35efea2-4a3f-4f96-98e2-71c6ddff21b1', '').
narrative_ontology:cs_kernel_id(secession_legitimacy_boundary__grievance_threshold_reading, secession_legitimacy_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(secession_legitimacy_boundary__grievance_threshold_reading, provincial_resource_sector_elites).
narrative_ontology:constraint_beneficiary(secession_legitimacy_boundary__grievance_threshold_reading, secessionist_political_movements).
narrative_ontology:constraint_victim(secession_legitimacy_boundary__grievance_threshold_reading, provincial_minority_populations).
narrative_ontology:constraint_victim(secession_legitimacy_boundary__grievance_threshold_reading, federal_transfer_recipient_regions).
narrative_ontology:constraint_victim(secession_legitimacy_boundary__grievance_threshold_reading, border_indigenous_nations).
narrative_ontology:constraint_vindicates(secession_legitimacy_boundary__grievance_threshold_reading, structural_injustice_supersedes_textual_constitutionalism).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Assembles the case that federal resource taxation, transfer formulas, or regulatory vetoes constitute structural injustice sufficient to license exit outside the constitutional text. Controls which grievances are elevated to threshold-crossing status and which evidence counts. Gains political capital and potential control of resource revenue if the threshold argument succeeds.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__grievance_threshold_reading, secessionist_political_movements, agenda_setter,
    organized, generational, mobile, regional).

% Stand to retain a larger share of resource rents currently redistributed through federal transfer mechanisms if the grievance threshold is judged crossed. Fund and amplify the structural-injustice narrative; can relocate capital or shift investment regardless of the secession outcome, giving them low personal exposure to the risk they are promoting.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__grievance_threshold_reading, provincial_resource_sector_elites, beneficiary,
    powerful, biographical, arbitrage, regional).

% Includes linguistic, political, or ideological minorities within the seceding province who did not consent to the grievance narrative and who would lose federal constitutional protections, courts, and transfer-funded services if secession proceeds. Cannot easily relocate; their objections are treated as internal noise within the province's own referendum-style framing rather than as a competing rights claim.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__grievance_threshold_reading, provincial_minority_populations, payer,
    powerless, biographical, trapped, regional).

% Poorer regions elsewhere in the federation rely on transfers drawn partly from the resource-rich province's contributions. If the grievance threshold is judged crossed and the province exits or renegotiates the resource-transfer formula, these regions absorb a funding shortfall they had no voice in creating. Their exit from the federation is not on the table; they simply bear the fiscal consequence of someone else's threshold claim.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__grievance_threshold_reading, federal_transfer_recipient_regions, payer,
    moderate, generational, constrained, national).

% Hold treaty relationships with the federal Crown/government predating the province's own founding, on lands that would be redrawn by any secession. The grievance-threshold framing treats their treaty status as a subordinate detail to be resolved after the injustice threshold question, rather than as a prior and independent legitimacy condition. Cannot exit the territory in question and are not consulted in constructing the threshold test.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__grievance_threshold_reading, border_indigenous_nations, payer,
    powerless, civilizational, trapped, regional).
narrative_ontology:stakeholder_secondary_role(secession_legitimacy_boundary__grievance_threshold_reading, border_indigenous_nations, excluded).

% Denies that any threshold of policy grievance can license unilateral exit outside the amendment process, but under this reading its constitutional counter-argument is treated as merely one input into whether the threshold has been crossed, not as dispositive. Retains formal authority but has its legitimacy claim relativized by the reading's own test.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__grievance_threshold_reading, federal_government, excluded,
    institutional, generational, constrained, national).

% Would be asked to adjudicate whether the alleged structural injustice meets the objective burden of proof this reading requires. Their prior jurisprudence (constitutional text, negotiated-exit doctrine) sits in tension with a legitimacy test the reading places above that text when injustice is severe enough.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__grievance_threshold_reading, constitutional_courts, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a principled off-ramp for populations who can demonstrate they are structurally and persistently disadvantaged by federal policy, preventing purely textual constitutionalism from trapping a province in a genuinely extractive relationship it cannot exit through ordinary politics.
% TRANSFER_FUNCTION: If the threshold is judged crossed, moves fiscal authority over resource rents and regulatory control from the federal government to the province, and moves the burden of proof for legitimacy from constitutional text to a contested injustice assessment controlled largely by the secessionist coalition itself.
% ABSENT_VOICES: Provincial minorities who dissent from the grievance narrative, other federation regions who depend on current transfers, and border Indigenous nations whose treaty rights are logically prior to the province's own claim to the territory are all structurally outside the room where the threshold question is decided.
% DISAPPEARANCE_RATIONALE: Secessionist movements and resource elites would say the world rearranges catastrophically without this legitimacy pathway, since it is their only route to exit outside constitutional amendment. Federal institutions, minority populations, and treaty nations would say the world is largely unchanged or improved, since the amendment process and treaty primacy already provide the legitimate channels this reading claims are insufficient.
% FOUNDING_PROBLEM: Constitutional amendment and secession-by-negotiation processes can be structurally captured by the very federal majority a province is trying to exit, making textual unilateral-secession bans function as a lock rather than a neutral procedural safeguard when federal policy is genuinely extractive.
% FOUNDING_PROBLEM_CORROBORATION: Secessionist movements and allied constitutional scholars attest the problem is live, citing historical cases of amendment-process capture. Federal government representatives, most sitting constitutional courts, and treaty nations attest the problem is either unproven in the specific case or subordinate to unresolved treaty and minority-rights questions — no assessment from outside the secessionist coalition and its funders has independently validated that the threshold has in fact been crossed in any live case.
narrative_ontology:disappearance_verdict(secession_legitimacy_boundary__grievance_threshold_reading, contested).
narrative_ontology:founding_problem_status(secession_legitimacy_boundary__grievance_threshold_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(secession_legitimacy_boundary__grievance_threshold_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(secession_legitimacy_boundary__grievance_threshold_reading, 'none', 1).
narrative_ontology:epsilon_provenance(secession_legitimacy_boundary__grievance_threshold_reading, 0.62, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(secession_legitimacy_boundary__grievance_threshold_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(secession_legitimacy_boundary__grievance_threshold_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(secession_legitimacy_boundary__grievance_threshold_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62) reflects that under this reading's own operation, 'structural injustice' is a contestable, movement-controlled determination rather than a court-verified fact in any existing case — the coordination function (a real off-ramp from genuine federal capture) is entangled with an extraction function (resource elites and secessionist elites capturing rents and authority by asserting the threshold is crossed). Suppression (0.58) is moderate: the reading does not forcibly prevent dissent, but it structurally treats minority and treaty objections as subordinate procedural noise within the threshold test, which is itself a suppressive framing device. Theater ratio (0.40) is notable because much of the 'objective burden of proof' apparatus (commissioned studies, expert panels selected by the secessionist coalition) performs rigor without a binding external adjudicator, and this ratio is authored to rise over the interval as the doctrine matures rhetorically while remaining institutionally untested.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda_setter and beneficiary seats, this looks like a genuine coordination breakthrough: a principled way to escape an otherwise textually locked, extractive federal relationship. From the payer seats — minorities, transfer-recipient regions, treaty nations — the same structure looks like an extraction mechanism dressed as a justice test, where the burden of proof is set by the party who benefits from meeting it. The engine computing divergent seat classifications from this same structural data is exactly the intended measurement; the claimed_type (tangled_rope) already encodes that both the coordination function and the extraction are real and coexist.
 *
 * DIRECTIONALITY LOGIC:
 *   Secessionist movements and resource elites sit near the beneficiary end: they set the threshold test's content, control its evidentiary apparatus, and have mobile or arbitrage-grade exit from the downside risk. Provincial minorities and border Indigenous nations sit near the full-target end: trapped exit options, no voice in constructing the test, and concrete loss of protections or land-status certainty if the threshold is judged crossed. Federal transfer-recipient regions are a diffuse victim class: moderate power, constrained exit, bearing a fiscal externality with no seat at the table deciding the threshold question.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — that federal amendment processes can be captured by the majority a province seeks to exit — may well be a live and real problem in some historical cases (founding_problem_status: contested, not dismissed). Classifying this reading as tangled_rope rather than snare or rope avoids two errors: treating the entire doctrine as pure cynical extraction (which would ignore that federal overreach can genuinely occur and that some coordination function is real), and treating it as pure legitimate coordination (which would ignore that the threshold's content and evidentiary standard are set by the very parties who profit from the threshold being judged crossed, with no independent corroboration in any existing case).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    objective_burden_of_proof_ambiguity,
    'Can ''structural injustice'' ever be assessed by a genuinely neutral, non-partisan adjudicator, or does every existing institutional candidate for that role (provincial courts, federal courts, ad hoc commissions) carry structural bias toward one side of the secession question?',
    'Comparative study of prior threshold-style adjudications (e.g., Supreme Court reference opinions on secession) to determine whether any adjudicator has produced a ruling the losing side accepted as fair on process grounds even while disagreeing on outcome.',
    'If no neutral adjudicator is structurally available, the threshold test is unfalsifiable in practice and the doctrine functions closer to snare (extraction dressed as justice test); if a workable neutral adjudicator exists, the doctrine is closer to a genuine rope-like coordination safeguard.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(objective_burden_of_proof_ambiguity, conceptual, 'Whether an objective adjudicator for the injustice threshold can exist in practice.').

omega_variable(
    treaty_priority_subordination,
    'Does the grievance-threshold test''s ordering — assess federal-provincial injustice first, treaty status second — itself constitute a structural injustice against treaty nations, independent of how the federal-provincial question is resolved?',
    'Legal and historical analysis of whether treaty rights are properly logically and temporally prior to the provincial-federal relationship the threshold test evaluates.',
    'If treaty priority is logically prior, this reading''s ordering is itself part of the extraction it claims to identify, strengthening the tangled_rope (or even snare) classification specifically with respect to border_indigenous_nations.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(treaty_priority_subordination, conceptual, 'Whether subordinating treaty status to the injustice threshold is itself an injustice.').

omega_variable(
    threshold_reading_kernel_disagreement_locus,
    'Where exactly does this reading''s premise conflict with the sibling readings, and is any conflict severe enough to be a forecloses relation rather than coexists_with or influences?',
    'This is committer-frame content, not resolvable by data internal to this story: it documents that this reading treats constitutional text as defeasible by injustice severity, which is a live disagreement with constitutional_impossibility_reading (text as non-defeasible) but does not itself resolve whether treaty_primacy_reading''s prior-consent claim is compatible with a crossed threshold.',
    'Clarifies for downstream network analysis that this reading and constitutional_impossibility_reading are in direct tension over whether text can ever be overridden, while this reading and popular_sovereignty_reading could in principle both be satisfied in the same case (a referendum result that also demonstrates injustice) — informing the reading_relations declared in cs_structure.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(threshold_reading_kernel_disagreement_locus, conceptual, 'Where this reading''s disagreement with siblings is located, for cs_structure.reading_relations.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(secession_legitimacy_boundary__grievance_threshold_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sece_tr_t0, secession_legitimacy_boundary__grievance_threshold_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement(sece_tr_t4, secession_legitimacy_boundary__grievance_threshold_reading, theater_ratio, 4, 0.26).
narrative_ontology:measurement(sece_tr_t8, secession_legitimacy_boundary__grievance_threshold_reading, theater_ratio, 8, 0.3).
narrative_ontology:measurement(sece_tr_t12, secession_legitimacy_boundary__grievance_threshold_reading, theater_ratio, 12, 0.33).
narrative_ontology:measurement(sece_tr_t16, secession_legitimacy_boundary__grievance_threshold_reading, theater_ratio, 16, 0.36).
narrative_ontology:measurement(sece_tr_t20, secession_legitimacy_boundary__grievance_threshold_reading, theater_ratio, 20, 0.38).
narrative_ontology:measurement(sece_tr_t24, secession_legitimacy_boundary__grievance_threshold_reading, theater_ratio, 24, 0.4).

% Extraction over time
narrative_ontology:measurement(sece_be_t0, secession_legitimacy_boundary__grievance_threshold_reading, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(sece_be_t4, secession_legitimacy_boundary__grievance_threshold_reading, base_extractiveness, 4, 0.46).
narrative_ontology:measurement(sece_be_t8, secession_legitimacy_boundary__grievance_threshold_reading, base_extractiveness, 8, 0.51).
narrative_ontology:measurement(sece_be_t12, secession_legitimacy_boundary__grievance_threshold_reading, base_extractiveness, 12, 0.55).
narrative_ontology:measurement(sece_be_t16, secession_legitimacy_boundary__grievance_threshold_reading, base_extractiveness, 16, 0.58).
narrative_ontology:measurement(sece_be_t20, secession_legitimacy_boundary__grievance_threshold_reading, base_extractiveness, 20, 0.6).
narrative_ontology:measurement(sece_be_t24, secession_legitimacy_boundary__grievance_threshold_reading, base_extractiveness, 24, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(sece_su_t0, secession_legitimacy_boundary__grievance_threshold_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(sece_su_t4, secession_legitimacy_boundary__grievance_threshold_reading, suppression_requirement, 4, 0.4).
narrative_ontology:measurement(sece_su_t8, secession_legitimacy_boundary__grievance_threshold_reading, suppression_requirement, 8, 0.46).
narrative_ontology:measurement(sece_su_t12, secession_legitimacy_boundary__grievance_threshold_reading, suppression_requirement, 12, 0.5).
narrative_ontology:measurement(sece_su_t16, secession_legitimacy_boundary__grievance_threshold_reading, suppression_requirement, 16, 0.53).
narrative_ontology:measurement(sece_su_t20, secession_legitimacy_boundary__grievance_threshold_reading, suppression_requirement, 20, 0.56).
narrative_ontology:measurement(sece_su_t24, secession_legitimacy_boundary__grievance_threshold_reading, suppression_requirement, 24, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(secession_legitimacy_boundary__grievance_threshold_reading, constitutional_impossibility_reading).
narrative_ontology:affects_constraint(secession_legitimacy_boundary__grievance_threshold_reading, popular_sovereignty_reading).
narrative_ontology:affects_constraint(secession_legitimacy_boundary__grievance_threshold_reading, treaty_primacy_reading).

% DUAL FORMULATION NOTE:
% This story is one of four sibling readings of the secession_legitimacy_boundary kernel. Each reading authors its own ε, beneficiary/victim structure, and classification independently — this reading claims tangled_rope with ε=0.62 for the grievance-threshold arrangement specifically; the constitutional_impossibility_reading, popular_sovereignty_reading, and treaty_primacy_reading are separate files with their own metrics. Do not average or reconcile ε values across the family; each reading's ε is indexed to that reading's own account of the standing arrangement.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
