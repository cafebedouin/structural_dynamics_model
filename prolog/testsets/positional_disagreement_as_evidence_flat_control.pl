% ============================================================================
% CONSTRAINT STORY: positional_disagreement_as_evidence_flat_control
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_positional_disagreement_as_evidence_flat_control, []).

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
    narrative_ontology:flat_control_of/2,
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: positional_disagreement_as_evidence_flat_control
 *   human_readable: Positional Disagreement as Legitimate Epistemic Evidence
 *   domain: epistemology/philosophy_of_technology/institutional_analysis
 *
 * SUMMARY:
 *   This story treats the epistemic-normative commitment itself — 'a
 *   positioned observer's disagreement is legitimate evidence, not bias to be
 *   averaged away' — as a single constraint operating across academic,
 *   institutional, and lay reporting contexts. It began as a corrective
 *   against a real asymmetry (unmarked 'neutral' reports outranking marked
 *   'positioned' reports of the same arrangement) and has, over the measured
 *   interval, accreted a secondary function: blocking adjudication between
 *   positioned reports even where convergent inquiry could in principle
 *   settle the underlying question. The claim is authored as tangled_rope
 *   because both functions are present and intertwined through the same
 *   mechanism — the same methodological norm that protects marginalized
 *   testimony also insulates any positioned claim from correction, including
 *   ones amenable to synthesis. This is authored FLAT: no reading
 *   decomposition, no cs_structure axioms/reading_relations. The contestation
 *   instead surfaces as perspectival disagreement across stakeholder seats
 *   and as omegas.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(positional_disagreement_as_evidence_flat_control, 0.38).
domain_priors:suppression_score(positional_disagreement_as_evidence_flat_control, 0.42).
domain_priors:theater_ratio(positional_disagreement_as_evidence_flat_control, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(positional_disagreement_as_evidence_flat_control, extractiveness, 0.38).
narrative_ontology:constraint_metric(positional_disagreement_as_evidence_flat_control, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(positional_disagreement_as_evidence_flat_control, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(positional_disagreement_as_evidence_flat_control, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(positional_disagreement_as_evidence_flat_control, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(positional_disagreement_as_evidence_flat_control, tangled_rope).
narrative_ontology:human_readable(positional_disagreement_as_evidence_flat_control, "Positional Disagreement as Legitimate Epistemic Evidence").
narrative_ontology:topic_domain(positional_disagreement_as_evidence_flat_control, "epistemology/philosophy_of_technology/institutional_analysis").

domain_priors:requires_active_enforcement(positional_disagreement_as_evidence_flat_control).

% --- Construction-pair linkage (forced-flat control of a kernel) ---
narrative_ontology:flat_control_of(positional_disagreement_as_evidence_flat_control, positional_disagreement_as_evidence).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(positional_disagreement_as_evidence_flat_control, standpoint_researchers).
narrative_ontology:constraint_beneficiary(positional_disagreement_as_evidence_flat_control, marginalized_position_holders).
narrative_ontology:constraint_beneficiary(positional_disagreement_as_evidence_flat_control, qualitative_methodology_field).
narrative_ontology:constraint_victim(positional_disagreement_as_evidence_flat_control, cross_position_synthesis_seekers).
narrative_ontology:constraint_victim(positional_disagreement_as_evidence_flat_control, institutional_decision_makers).
narrative_ontology:constraint_victim(positional_disagreement_as_evidence_flat_control, disagreeing_junior_reporters).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(positional_disagreement_as_evidence_flat_control, disagreeing_junior_reporters).
narrative_ontology:constraint_vindicates(positional_disagreement_as_evidence_flat_control, standpoint_epistemology_thesis).
narrative_ontology:constraint_vindicates(positional_disagreement_as_evidence_flat_control, situated_knowledge_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Build and police the norm that a positioned report is itself data. Their disciplinary standing, publication pipelines, and methodological authority depend on the claim holding: if disagreement were instead treated as noise to be averaged toward a neutral estimate, much of their apparatus (standpoint theory, situated-knowledge methodology, positionality statements) would lose its evidentiary warrant. They administer the norm through peer review, hiring, and citation practices that reward treating positioned reports as irreducible.
narrative_ontology:constraint_stakeholder(positional_disagreement_as_evidence_flat_control, standpoint_researchers, agenda_setter,
    institutional, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(positional_disagreement_as_evidence_flat_control, standpoint_researchers, beneficiary).

% Occupy structural positions (subordinated by race, class, disability, colonial history) whose reports were historically dismissed as bias or resentment. The commitment gives their testimony standing it previously lacked — it is the mechanism by which their account of an arrangement counts as evidence rather than complaint. They cannot easily exit their position to test the claim from elsewhere; the commitment is their only lever against dismissal.
narrative_ontology:constraint_stakeholder(positional_disagreement_as_evidence_flat_control, marginalized_position_holders, beneficiary,
    powerless, biographical, trapped, national).

% Policymakers, mediators, and practitioners who need to act on a single arrangement (a workplace policy, a clinical protocol, a contested historical event) and cannot indefinitely hold two irreconcilable positioned reports as equally final. The commitment blocks them from resolving disagreement by adjudication or averaging — they must instead hold both reports open, which delays decisions and shifts the cost of unresolved disagreement onto whoever needs the arrangement settled.
narrative_ontology:constraint_stakeholder(positional_disagreement_as_evidence_flat_control, cross_position_synthesis_seekers, payer,
    moderate, immediate, constrained, national).

% Managers, judges, and administrators who must rule on contested arrangements. Under the commitment, dismissing one side's account as mistaken (rather than differently positioned but equally evidential) is itself treated as an epistemic violation, not a legitimate finding. This raises the cost and risk of any decision that resolves rather than preserves the disagreement, even where an underlying fact could in principle be established.
narrative_ontology:constraint_stakeholder(positional_disagreement_as_evidence_flat_control, institutional_decision_makers, payer,
    powerful, biographical, constrained, national).

% Junior researchers, employees, or community members who report an arrangement differently than a more senior or more institutionally legible reporter. The commitment protects them from having their account waved away as mere bias, but it also means their disagreement can be permanently unresolved rather than adjudicated — they carry the ongoing cost of not being believed OR disbelieved, just perpetually 'positioned.'
narrative_ontology:constraint_stakeholder(positional_disagreement_as_evidence_flat_control, disagreeing_junior_reporters, payer,
    powerless, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(positional_disagreement_as_evidence_flat_control, disagreeing_junior_reporters, beneficiary).

% Analysts and mediators who believe that at least some positional disagreements are resolvable by better information, shared instruments, or convergent inquiry, and that treating all such disagreement as irreducible evidence forecloses inquiry that would otherwise settle the matter. Their view is rarely represented inside the standpoint-methodology literature itself; they publish in adjacent fields or are recast as failing to understand positionality.
narrative_ontology:constraint_stakeholder(positional_disagreement_as_evidence_flat_control, neutral_synthesis_advocates, excluded,
    moderate, biographical, constrained, national).

% Analyze the commitment's structure without a stake in its administration: is it a genuine epistemic advance (each position samples a different facet of a complex arrangement, so disagreement is informative) or a normative overreach (using an epistemic-sounding claim to insulate certain reports from correction)? They can trace which cases the commitment illuminates and which it is used to shield.
narrative_ontology:constraint_stakeholder(positional_disagreement_as_evidence_flat_control, philosophical_epistemologists, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(positional_disagreement_as_evidence_flat_control, diffuse).
narrative_ontology:fixing_cost_class(positional_disagreement_as_evidence_flat_control, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves a genuine epistemic problem: a single 'view from nowhere' assessment of a complex social arrangement often misses facets that only a particular structural position reveals (e.g., how a policy feels to those it is applied to versus those who administer it). Treating positioned disagreement as evidence rather than noise lets multiple genuinely different vantage points contribute information that averaging or majority-adjudication would erase.
% TRANSFER_FUNCTION: Moves epistemic authority and the burden of resolution: institutional decision-makers and synthesis-seekers who need a settled account absorb the cost of leaving disagreements open, while standpoint researchers and marginalized reporters gain protection from having their accounts overridden or averaged away. It also transfers disciplinary prestige and methodological gatekeeping power to researchers who administer the standpoint framework.
% ABSENT_VOICES: Neutral-synthesis advocates who hold that some (not all) positional disagreements are resolvable by shared instruments or further inquiry are structurally underrepresented in the venues that adjudicate the commitment's scope — journals and review panels built around standpoint methodology have limited incentive to platform a view that would narrow the commitment's application.
% DISAPPEARANCE_RATIONALE: If the commitment vanished, decision-makers would regain a tool for closing disputes by adjudication or synthesis, which would help synthesis-seekers and institutional actors but strip marginalized reporters and standpoint researchers of a hard-won protection against having their accounts dismissed as bias. Whether the world 'rearranges' or 'stays the same' depends entirely on which seat you ask — which is itself the commitment's own subject matter, making the disappearance question reflexively contested rather than settled by the story's other facts.
% FOUNDING_PROBLEM: Historically, reports from subordinated or minority structural positions (workers describing working conditions, colonized peoples describing colonial administration, women describing workplace dynamics) were routinely discounted as subjective, biased, or emotionally compromised relative to a supposedly neutral or expert account, which was itself unmarked as positioned. The commitment was built to stop that asymmetric discounting.
% FOUNDING_PROBLEM_CORROBORATION: Historians of science and social epistemologists outside standpoint theory (e.g., work on testimonial injustice) corroborate that the asymmetric-discounting problem was real and in many domains persists — supporting founding_problem_status as at least partly live. But philosophical epistemologists and neutral-synthesis advocates, also outside the beneficiary set, corroborate a second claim: the commitment has in some venues expanded past the asymmetric-discounting case into blocking ANY adjudication between positioned reports, including cases where convergent inquiry could resolve the disagreement — supporting a contested rather than simply live verdict.
narrative_ontology:disappearance_verdict(positional_disagreement_as_evidence_flat_control, contested).
narrative_ontology:founding_problem_status(positional_disagreement_as_evidence_flat_control, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(positional_disagreement_as_evidence_flat_control, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-14',
    'unspecified', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'unspecified').
narrative_ontology:story_seed(positional_disagreement_as_evidence_flat_control, 'none', 1).
narrative_ontology:epsilon_provenance(positional_disagreement_as_evidence_flat_control, 0.38, 'claude-sonnet-5', 'cheap_confession_2026_20260814_151329', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(positional_disagreement_as_evidence_flat_control_tests).
:- end_tests(positional_disagreement_as_evidence_flat_control_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate and rising (0.18 to 0.38) because the coordination function (protecting genuinely irreducible positional testimony) is real and substantial, but a growing share of the constraint's operation extends the protection to disagreements that are not in fact irreducible — this is the extractive layer, and it grows over the interval as the norm's institutional footprint (peer review standards, positionality-statement requirements, DEI-adjacent adjudication protocols) hardens. Suppression is moderate (0.42): the norm does not physically prevent adjudication, but it makes attempting adjudication professionally costly (charges of erasure, epistemic violence, or failing to understand positionality) — a reputational suppression mechanism rather than a coercive one. Theater ratio is moderate-low (0.28) reflecting that most invocations of the norm are substantively engaged with the case at hand, though a rising minority are performative citations that foreclose inquiry without engaging the specific disagreement's resolvability.
 *
 * PERSPECTIVAL GAP:
 *   From the standpoint-researcher and marginalized-position-holder seats, the constraint reads as coordination: it correctly refuses a false neutrality that was itself a covert positional bias, and its expansion into harder cases is simply consistent application of a sound principle. From the institutional-decision-maker and synthesis-seeker seats, the same norm reads as extraction: it removes a legitimate tool (comparative adjudication) from their hands and imposes indefinite deferral as the only 'respectful' posture, regardless of whether the specific disagreement is actually irreducible. The engine should register this as genuine seat divergence, not error in either seat's computation — the structural data (beneficiary/victim declarations, exit options) is what generates the divergence, not a hidden preference for one reading.
 *
 * DIRECTIONALITY LOGIC:
 *   Standpoint researchers and marginalized position holders sit near the beneficiary end: the commitment either grants them disciplinary authority (researchers) or protects their testimony from dismissal (position holders) — though the latter's exit options are 'trapped' rather than 'mobile,' since their benefit is inseparable from an identity position they cannot simply leave. Cross-position synthesis seekers and institutional decision-makers sit toward the target end: they bear the transaction cost of a norm that blocks the tool (adjudication, averaging, synthesis) they need to close a live decision. Disagreeing junior reporters are genuinely dual-positioned — protected from dismissal, but also denied resolution, which is itself a cost distinct from being disbelieved.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (asymmetric discounting of positioned testimony) is genuinely live in many domains — this prevents flatly declaring the whole constraint an extraction-only mandatrophy case. But the corroboration split (philosophical epistemologists and neutral-synthesis advocates identifying scope creep into resolvable disagreements) shows the mandate has partially outlived its original function in a specific subset of cases, without the norm itself distinguishing that subset. The tangled_rope classification captures exactly this: a live coordination core with an accreted extractive shell operating through the identical mechanism, which is why disentangling them (which cases are genuinely irreducible vs. which are resolvable-but-protected) is the analytically load-bearing question rather than a settled fact.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    irreducibility_boundary,
    'Is there a principled boundary between positional disagreements that are genuinely irreducible (each position samples a different real facet of the arrangement) and those that are resolvable by further shared inquiry but are being protected from resolution by the same norm?',
    'Case-level analysis: track disputes where the commitment was invoked, then check whether subsequent shared-instrument or convergent inquiry ever did resolve superficially ''positional'' disagreements without contradiction from either original reporter. A nonzero resolution rate in cases the norm treated as irreducible would establish that the boundary is being drawn too broadly.',
    'If a large share of protected disagreements are in fact resolvable, the constraint''s extractive share is substantially larger than the coordination-only reading suggests, pushing the classification further toward tangled_rope or even snare for the affected subset. If irreducibility genuinely tracks most invocations, the coordination function dominates and the constraint reads closer to a rope with modest extractive drift.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(irreducibility_boundary, empirical, 'Whether positional disagreements protected by the norm are actually irreducible or merely unresolved.').

omega_variable(
    asymmetric_discounting_persistence,
    'How much of the original asymmetric-discounting problem (unmarked neutral reports outranking marked positional reports) still exists in the domains where the commitment is now most heavily invoked?',
    'Comparative institutional audit across time and domain: measure whether ''neutral'' framing still receives systematically higher credibility than explicitly positioned framing in decision venues, controlling for the content of the report.',
    'If the asymmetry has substantially closed in a given domain, the founding problem there is dead and continued invocation of the commitment in that domain is closer to inertial or capture-driven than corrective — supporting a piton or extraction-dominant reading for that subset. If the asymmetry persists, the coordination function remains fully load-bearing there.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(asymmetric_discounting_persistence, empirical, 'Whether the founding asymmetric-discounting problem remains live in the domains where the norm is now enforced most strongly.').

omega_variable(
    epistemic_vs_normative_framing,
    'Is ''positional disagreement is legitimate evidence'' best understood as a strictly epistemic claim (about what counts as data) or as a normative claim wearing epistemic language (about who is owed deference and why)?',
    'Conceptual analysis of whether the claim''s force survives when stripped of its normative payload — would the commitment be defended purely on grounds of improving arrangement-modeling accuracy, independent of any concern about historical discounting or respect owed to marginalized reporters?',
    'If the claim is irreducibly normative, its extractive potential is structural (it is doing political work under epistemic cover), which would push toward a more extraction-weighted reading regardless of measured metrics. If it is strictly epistemic, the extractive drift measured here is better explained as incidental institutional capture rather than built into the claim''s nature.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(epistemic_vs_normative_framing, conceptual, 'Whether the commitment''s justificatory force is epistemic, normative, or an unstable fusion of both.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(positional_disagreement_as_evidence_flat_control, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(posi_tr_t0, positional_disagreement_as_evidence_flat_control, theater_ratio, 0, 0.1).
narrative_ontology:measurement(posi_tr_t8, positional_disagreement_as_evidence_flat_control, theater_ratio, 8, 0.14).
narrative_ontology:measurement(posi_tr_t16, positional_disagreement_as_evidence_flat_control, theater_ratio, 16, 0.18).
narrative_ontology:measurement(posi_tr_t24, positional_disagreement_as_evidence_flat_control, theater_ratio, 24, 0.22).
narrative_ontology:measurement(posi_tr_t32, positional_disagreement_as_evidence_flat_control, theater_ratio, 32, 0.25).
narrative_ontology:measurement(posi_tr_t40, positional_disagreement_as_evidence_flat_control, theater_ratio, 40, 0.28).

% Extraction over time
narrative_ontology:measurement(posi_be_t0, positional_disagreement_as_evidence_flat_control, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(posi_be_t8, positional_disagreement_as_evidence_flat_control, base_extractiveness, 8, 0.24).
narrative_ontology:measurement(posi_be_t16, positional_disagreement_as_evidence_flat_control, base_extractiveness, 16, 0.29).
narrative_ontology:measurement(posi_be_t24, positional_disagreement_as_evidence_flat_control, base_extractiveness, 24, 0.33).
narrative_ontology:measurement(posi_be_t32, positional_disagreement_as_evidence_flat_control, base_extractiveness, 32, 0.36).
narrative_ontology:measurement(posi_be_t40, positional_disagreement_as_evidence_flat_control, base_extractiveness, 40, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(posi_su_t0, positional_disagreement_as_evidence_flat_control, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(posi_su_t8, positional_disagreement_as_evidence_flat_control, suppression_requirement, 8, 0.27).
narrative_ontology:measurement(posi_su_t16, positional_disagreement_as_evidence_flat_control, suppression_requirement, 16, 0.32).
narrative_ontology:measurement(posi_su_t24, positional_disagreement_as_evidence_flat_control, suppression_requirement, 24, 0.36).
narrative_ontology:measurement(posi_su_t32, positional_disagreement_as_evidence_flat_control, suppression_requirement, 32, 0.39).
narrative_ontology:measurement(posi_su_t40, positional_disagreement_as_evidence_flat_control, suppression_requirement, 40, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(positional_disagreement_as_evidence_flat_control, identity_coordination).
narrative_ontology:boltzmann_floor_override(positional_disagreement_as_evidence_flat_control, 0.1).

% DUAL FORMULATION NOTE:
% Authored as a flat (non-decomposed) construction of a substrate that, in other generation runs, is split into multiple kernel readings (e.g., a strict testimonial-injustice-correction reading versus a scope-creep/anti-adjudication reading). This file deliberately holds both functions inside one constraint per the flat-construction control instructions, rather than authoring cs_structure.reading_relations/axioms.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
