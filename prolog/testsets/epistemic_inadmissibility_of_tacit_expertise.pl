% ============================================================================
% CONSTRAINT STORY: epistemic_inadmissibility_of_tacit_expertise
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_epistemic_inadmissibility_of_tacit_expertise, []).

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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: epistemic_inadmissibility_of_tacit_expertise
 *   human_readable: Categorical Exclusion of Unlicensed Tacit Knowledge from Institutional Evidentiary Standing
 *   domain: institutional/epistemic
 *
 * SUMMARY:
 *   In many technical and forensic domains — trades, traditional medicine,
 *   search-and-rescue, craft production, informal engineering repair — the
 *   person whose direct tacit knowledge actually produced a correct outcome
 *   is categorically barred from having that knowledge entered as evidence in
 *   the institutional process that later adjudicates the matter, purely
 *   because they lack a credential. The rule is procedural: it excludes a
 *   class of witness before any evaluation of what they know, on the stated
 *   ground that unlicensed testimony is inherently unreliable hearsay. The
 *   coordination function is real — bright-line credentialing rules let
 *   adjudicators process cases without re-deriving epistemic warrant each
 *   time — but the same structure asymmetrically transfers evidentiary
 *   standing, income, and legitimacy to the credentialing authority and its
 *   licensed class, at the direct expense of unlicensed practitioners whose
 *   knowledge is demonstrably causal in specific cases.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(epistemic_inadmissibility_of_tacit_expertise, 0.68).
domain_priors:suppression_score(epistemic_inadmissibility_of_tacit_expertise, 0.79).
domain_priors:theater_ratio(epistemic_inadmissibility_of_tacit_expertise, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(epistemic_inadmissibility_of_tacit_expertise, extractiveness, 0.68).
narrative_ontology:constraint_metric(epistemic_inadmissibility_of_tacit_expertise, suppression_requirement, 0.79).
narrative_ontology:constraint_metric(epistemic_inadmissibility_of_tacit_expertise, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(epistemic_inadmissibility_of_tacit_expertise, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(epistemic_inadmissibility_of_tacit_expertise, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(epistemic_inadmissibility_of_tacit_expertise, tangled_rope).
narrative_ontology:human_readable(epistemic_inadmissibility_of_tacit_expertise, "Categorical Exclusion of Unlicensed Tacit Knowledge from Institutional Evidentiary Standing").
narrative_ontology:topic_domain(epistemic_inadmissibility_of_tacit_expertise, "institutional/epistemic").

domain_priors:requires_active_enforcement(epistemic_inadmissibility_of_tacit_expertise).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(epistemic_inadmissibility_of_tacit_expertise, institutional_continuity_and_credentialing_authority).
narrative_ontology:constraint_victim(epistemic_inadmissibility_of_tacit_expertise, unlicensed_practitioners_whose_knowledge_produced_the_outcome).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(epistemic_inadmissibility_of_tacit_expertise, credentialed_expert_witnesses).
narrative_ontology:constraint_beneficiary(epistemic_inadmissibility_of_tacit_expertise, professional_licensing_boards).
narrative_ontology:constraint_victim(epistemic_inadmissibility_of_tacit_expertise, downstream_parties_relying_on_the_ruling).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Writes and administers the evidentiary admissibility rules that determine which testimony an inquiry, tribunal, or licensing board may weigh. Categorically excludes testimony from unlicensed sources as a class, independent of accuracy, framing this as necessary to prevent unverifiable or unfalsifiable claims from contaminating the record. Its own credentialing pipeline is the sole recognized channel of admissible expertise, so every case resolved via credentialed testimony reinforces demand for its licensing function and its authority to certify who may speak with evidentiary weight.
narrative_ontology:constraint_stakeholder(epistemic_inadmissibility_of_tacit_expertise, institutional_continuity_and_credentialing_authority, agenda_setter,
    institutional, civilizational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(epistemic_inadmissibility_of_tacit_expertise, institutional_continuity_and_credentialing_authority, beneficiary).

% Possess tacit, somatic, or apprenticeship-transmitted knowledge that in fact produced the correct diagnosis, repair, rescue, or outcome under investigation. Their account of what they did and why is barred from the record as a categorical matter — the rule does not ask whether their testimony is true, only whether they hold the credential. They cannot buy, study, or litigate their way into admissibility within the timeframe that matters, because the exclusion attaches to their class, not their claim.
narrative_ontology:constraint_stakeholder(epistemic_inadmissibility_of_tacit_expertise, unlicensed_practitioners_whose_knowledge_produced_the_outcome, payer,
    powerless, biographical, trapped, local).

% Hold the license that makes their testimony admissible by default, whether or not they possess the tacit knowledge that actually explains the outcome. They are frequently called to reconstruct, post hoc, an account of causation that the unlicensed practitioner already knew directly. Their market position and courtroom/tribunal income depend on remaining the sole admissible channel.
narrative_ontology:constraint_stakeholder(epistemic_inadmissibility_of_tacit_expertise, credentialed_expert_witnesses, beneficiary,
    organized, generational, mobile, national).

% Applies the admissibility rule as written, excluding tacit-knowledge testimony at the gate before weighing it. Benefits from a bright-line rule that reduces adjudicative burden and appellate exposure, but as a result sometimes reaches an outcome contradicted by evidence it never allowed itself to hear.
narrative_ontology:constraint_stakeholder(epistemic_inadmissibility_of_tacit_expertise, adjudicating_body, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(epistemic_inadmissibility_of_tacit_expertise, adjudicating_body, observer).

% Patients, clients, or affected parties whose case outcome depends on a ruling built on an artificially narrowed evidentiary record. They bear the cost when the excluded tacit knowledge would have changed the finding, but they have no visibility into what was excluded and no standing to challenge the admissibility rule itself.
narrative_ontology:constraint_stakeholder(epistemic_inadmissibility_of_tacit_expertise, downstream_parties_relying_on_the_ruling, payer,
    moderate, biographical, constrained, regional).

% Would have a stake in any reform that recognized non-credentialed causal knowledge as admissible, since it would erode the exclusivity of licensure as the sole gateway to evidentiary standing. Not formally part of the adjudication but structurally protected by its outcomes; not invited to testify against the rule that benefits them.
narrative_ontology:constraint_stakeholder(epistemic_inadmissibility_of_tacit_expertise, professional_licensing_boards, excluded,
    organized, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(epistemic_inadmissibility_of_tacit_expertise, professional_licensing_boards, beneficiary).

% Study the historical construction of admissibility doctrine and can trace which exclusions tracked genuine reliability concerns versus consolidation of professional gatekeeping power. Have no power to alter the rule but can document the pattern across cases and jurisdictions.
narrative_ontology:constraint_stakeholder(epistemic_inadmissibility_of_tacit_expertise, legal_and_epistemic_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(epistemic_inadmissibility_of_tacit_expertise, institutional_continuity_and_credentialing_authority).
narrative_ontology:fixing_cost_class(epistemic_inadmissibility_of_tacit_expertise, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Bright-line admissibility rules solve a genuine problem: adjudicators cannot individually re-litigate the reliability of every witness's method in every case, so a categorical proxy (credentialing) lets tribunals process cases at scale without re-deriving epistemic warrant from scratch each time.
% TRANSFER_FUNCTION: Moves evidentiary standing and the income/prestige that follows it away from unlicensed practitioners whose knowledge produced the outcome and toward credentialed experts and the credentialing authority, regardless of which party's account is actually true in a given case.
% ABSENT_VOICES: The unlicensed practitioners whose knowledge is being excluded are, by the rule's own operation, never heard on the question of whether their exclusion was warranted in the specific case — the rule bars them at the gate, before any adjudicator can hear the substance of the objection.
% DISAPPEARANCE_RATIONALE: If the categorical exclusion vanished and admissibility turned on content-based reliability testing instead of credential status, tribunals would need new mechanisms for weighing unlicensed testimony case-by-case, credentialed experts would lose default evidentiary priority, and outcomes in cases where tacit knowledge diverges from credentialed reconstruction would shift.
% FOUNDING_PROBLEM: Courts and tribunals needed a workable filter against unreliable, unfalsifiable, or self-serving testimony in technical matters, and needed some proxy for competence that did not require re-litigating epistemology in every case.
% FOUNDING_PROBLEM_CORROBORATION: The credentialing authority and adjudicating bodies attest the rule remains necessary to filter unreliable testimony. Independent legal historians and reliability researchers outside the credentialing system attest that the category (licensure) has drifted from a proxy for reliability into an independently enforced gatekeeping function, citing documented cases where excluded tacit-knowledge testimony was later corroborated as accurate.
narrative_ontology:disappearance_verdict(epistemic_inadmissibility_of_tacit_expertise, world_rearranges).
narrative_ontology:founding_problem_status(epistemic_inadmissibility_of_tacit_expertise, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(epistemic_inadmissibility_of_tacit_expertise, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-09',
    'uke_narrative', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'temperature=0.2,max_tokens=8192').
narrative_ontology:story_seed(epistemic_inadmissibility_of_tacit_expertise, 'none', 1).
narrative_ontology:epsilon_provenance(epistemic_inadmissibility_of_tacit_expertise, 0.68, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(epistemic_inadmissibility_of_tacit_expertise_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(epistemic_inadmissibility_of_tacit_expertise, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(epistemic_inadmissibility_of_tacit_expertise_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (0.68) reflects that the exclusion transfers evidentiary weight to credentialed actors independent of the truth-value of what is excluded — this is a defining feature of the constraint, not incidental to it. Suppression (0.79) is high and rising because enforcing the categorical bar requires active gatekeeping at every admissibility hearing; it is a raw structural fact about the rule's operation, not scaled by scope or power. Theater ratio (0.42) is moderate: the rule performs rigor (excluding 'unverifiable' testimony) while its actual verification function has partly atrophied into credential-checking rather than content assessment. Accessibility collapse (0.62) is substantial but not complete — appellate and legislative routes to reform the rule exist, unlike a true natural-law mountain. Resistance (0.58) reflects active pushback from affected trades, patient advocacy groups, and reform-minded legal scholars.
 *
 * PERSPECTIVAL GAP:
 *   From the credentialing authority's seat, the rule looks like pure coordination: a necessary filter against unreliable testimony that lets adjudication scale. From the unlicensed practitioner's seat, the identical rule operates as extraction — their true, outcome-producing knowledge is barred not because it is wrong but because of who holds it. The engine should compute divergent seat classifications from this same structural data; the claimed_type (tangled_rope) is intended to capture that both the coordination function and the extraction are genuinely present simultaneously, not that one seat's account is more correct than the other's.
 *
 * DIRECTIONALITY LOGIC:
 *   The credentialing authority and credentialed experts sit near the beneficiary end of directionality: the rule was built by and administers to their advantage, and their exit options (arbitrage, mobile) reflect institutional control over the rule itself. Unlicensed practitioners sit at the full-target end: trapped exit options (no timely path to credentialing that would matter for the case at hand), and the exclusion attaches to their class rather than their individual claim, which is precisely what makes it categorical rather than evaluative. Downstream parties relying on rulings are moderate targets — they bear diffuse cost when the excluded knowledge would have changed the outcome, but have no standing to contest the rule itself.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — filtering genuinely unreliable testimony — remains partly live (there is real variance in the reliability of untested claims), which is why this is not classified as a pure snare. But the mechanism has drifted: it now excludes categorically rather than evaluating content, which means demonstrably correct tacit knowledge is barred alongside genuinely unreliable claims. Classifying this as tangled_rope rather than snare preserves the genuine coordination function (adjudicative tractability) while still naming the asymmetric extraction (credentialing authority and credentialed experts capture standing that unlicensed practitioners are foreclosed from, regardless of accuracy) — a pure snare framing would erase the real coordination problem the rule was built to solve, and a pure rope framing would erase the documented cases where the exclusion produced wrong outcomes.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reliability_proxy_vs_gatekeeping_capture,
    'Does the credential-based admissibility rule still function primarily as a reliability proxy, or has it drifted into an independently self-perpetuating gatekeeping mechanism whose primary function is now protecting the credentialing authority''s market position?',
    'Comparative case analysis: track outcomes in jurisdictions or tribunals that have piloted content-based reliability testing for unlicensed testimony against outcomes under the categorical rule, controlling for case type and stakes.',
    'If content-based testing produces comparable or better accuracy without the credentialing filter, the categorical rule''s coordination justification collapses and the constraint moves toward snare; if content-based testing performs worse, the coordination function is vindicated and the constraint sits more firmly as a genuine (if costly) tangled rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reliability_proxy_vs_gatekeeping_capture, empirical, 'Whether the exclusion still tracks genuine reliability risk or has become self-perpetuating gatekeeping.').

omega_variable(
    correctable_vs_structural_exclusion,
    'Is the categorical exclusion correctable within the existing institutional framework (e.g., via a case-by-case reliability hearing carve-out) or does correcting it require dismantling the credentialing authority''s evidentiary monopoly entirely?',
    'Track legislative or rule-making proposals that attempt narrow carve-outs for demonstrated tacit expertise and observe whether the credentialing authority absorbs, blocks, or co-opts them.',
    'If carve-outs are absorbed successfully, mandatrophy is resolvable within the current structure; if consistently blocked or co-opted into further credentialing requirements, the constraint''s persistence depends more heavily on active institutional self-interest than on any residual coordination need.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(correctable_vs_structural_exclusion, conceptual, 'Whether reform is structurally available within the current framework or requires abolishing the credentialing monopoly.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(epistemic_inadmissibility_of_tacit_expertise, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(epis_tr_t0, epistemic_inadmissibility_of_tacit_expertise, theater_ratio, 0, 0.2).
narrative_ontology:measurement(epis_tr_t8, epistemic_inadmissibility_of_tacit_expertise, theater_ratio, 8, 0.26).
narrative_ontology:measurement(epis_tr_t16, epistemic_inadmissibility_of_tacit_expertise, theater_ratio, 16, 0.31).
narrative_ontology:measurement(epis_tr_t24, epistemic_inadmissibility_of_tacit_expertise, theater_ratio, 24, 0.35).
narrative_ontology:measurement(epis_tr_t32, epistemic_inadmissibility_of_tacit_expertise, theater_ratio, 32, 0.39).
narrative_ontology:measurement(epis_tr_t40, epistemic_inadmissibility_of_tacit_expertise, theater_ratio, 40, 0.42).

% Extraction over time
narrative_ontology:measurement(epis_be_t0, epistemic_inadmissibility_of_tacit_expertise, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(epis_be_t8, epistemic_inadmissibility_of_tacit_expertise, base_extractiveness, 8, 0.49).
narrative_ontology:measurement(epis_be_t16, epistemic_inadmissibility_of_tacit_expertise, base_extractiveness, 16, 0.55).
narrative_ontology:measurement(epis_be_t24, epistemic_inadmissibility_of_tacit_expertise, base_extractiveness, 24, 0.6).
narrative_ontology:measurement(epis_be_t32, epistemic_inadmissibility_of_tacit_expertise, base_extractiveness, 32, 0.65).
narrative_ontology:measurement(epis_be_t40, epistemic_inadmissibility_of_tacit_expertise, base_extractiveness, 40, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(epis_su_t0, epistemic_inadmissibility_of_tacit_expertise, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(epis_su_t8, epistemic_inadmissibility_of_tacit_expertise, suppression_requirement, 8, 0.62).
narrative_ontology:measurement(epis_su_t16, epistemic_inadmissibility_of_tacit_expertise, suppression_requirement, 16, 0.68).
narrative_ontology:measurement(epis_su_t24, epistemic_inadmissibility_of_tacit_expertise, suppression_requirement, 24, 0.72).
narrative_ontology:measurement(epis_su_t32, epistemic_inadmissibility_of_tacit_expertise, suppression_requirement, 32, 0.76).
narrative_ontology:measurement(epis_su_t40, epistemic_inadmissibility_of_tacit_expertise, suppression_requirement, 40, 0.79).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(epistemic_inadmissibility_of_tacit_expertise, enforcement_mechanism).

% DUAL FORMULATION NOTE:
% This story addresses the categorical admissibility rule itself (the procedural bar), not the separate question of whether licensing regimes generally track competence (a different, likely less extractive constraint) or whether specific credentialing bodies have captured rule-making processes (a related but distinct institutional-capture story). Decomposed per the ε-invariance principle: measuring 'does licensure track competence' yields a different, generally lower extraction value than measuring 'is truthful tacit-knowledge testimony categorically barred regardless of accuracy,' which is what this story evaluates.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
