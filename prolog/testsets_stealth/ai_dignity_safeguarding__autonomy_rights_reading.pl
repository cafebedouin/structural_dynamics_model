% ============================================================================
% CONSTRAINT STORY: ai_dignity_safeguarding__autonomy_rights_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ai_dignity_safeguarding__autonomy_rights_reading, []).

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
 *   constraint_id: ai_dignity_safeguarding__autonomy_rights_reading
 *   human_readable: Autonomy-Rights Reading of AI Dignity Safeguarding: Democratic Regulation with Rights-Gated Enhancement
 *   domain: technological governance/theological ethics/philosophical anthropology
 *
 * SUMMARY:
 *   Under the autonomy-rights reading, human dignity is grounded in autonomy,
 *   rationality, and rights, and safeguarding it means building a democratic
 *   regulatory apparatus around artificial intelligence: transparency
 *   mandates, labor and privacy protections, algorithmic accountability, and
 *   a consent-and-rights gate on human enhancement. The standing arrangement
 *   this story describes is that apparatus as it has actually accumulated — a
 *   real coordination core (common disclosure standards, redress channels,
 *   liability clarity) wrapped around a growing extraction margin (compliance
 *   costs that entrench incumbents, an audit industry whose revenue scales
 *   with regulatory scope, and legitimacy collected from subjects the
 *   apparatus protects unevenly). The claim/metric split is deliberate: the
 *   arrangement is CLAIMED as tangled_rope because both a genuine
 *   coordination function and asymmetric extraction run through the same
 *   structure, while the metrics are authored independently as descriptive
 *   best estimates. The engine computes per-seat classifications from the
 *   structural data; divergence between seats is the finding, not an error.
 *   KEY AGENTS (by structural relationship): - democratic_regulators: agenda
 *   setter (institutional/constrained) — drafts and enforces the rules,
 *   dependent on regulated parties for technical information -
 *   autonomous_rights_bearers: primary beneficiary (organized/mobile) — hold
 *   the rights the apparatus enforces, receive protection unevenly -
 *   incumbent_ai_platforms: payer with secondary beneficiary position
 *   (powerful/arbitrage) — bear compliance costs, convert them into moats -
 *   small_ai_developers: payer (moderate/constrained) — bear the same
 *   obligations without the compliance capacity -
 *   algorithmic_decision_subjects: payer (powerless/trapped) — subject to
 *   opaque automated decisions, formally protected, practically waiting -
 *   displaced_workers: payer (organized/constrained) — bear automation
 *   transition costs under consultation and adjustment rules -
 *   accountability_audit_industry: beneficiary (organized/mobile) — sells the
 *   conformity assessment the rules mandate - enhancement_frontier_seekers:
 *   excluded (moderate/mobile) — would widen the enhancement gate -
 *   religious_dignity_traditionalists: excluded (organized/constrained) —
 *   would tighten it on different grounds - digital_rights_watchdogs:
 *   observer (organized/analytical) — litigate and publish the gap between
 *   promise and delivery
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_dignity_safeguarding__autonomy_rights_reading, 0.48).
domain_priors:suppression_score(ai_dignity_safeguarding__autonomy_rights_reading, 0.42).
domain_priors:theater_ratio(ai_dignity_safeguarding__autonomy_rights_reading, 0.32).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_dignity_safeguarding__autonomy_rights_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(ai_dignity_safeguarding__autonomy_rights_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(ai_dignity_safeguarding__autonomy_rights_reading, theater_ratio, 0.32).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ai_dignity_safeguarding__autonomy_rights_reading, accessibility_collapse, 0.38).
narrative_ontology:constraint_metric(ai_dignity_safeguarding__autonomy_rights_reading, resistance, 0.46).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_dignity_safeguarding__autonomy_rights_reading, tangled_rope).
narrative_ontology:human_readable(ai_dignity_safeguarding__autonomy_rights_reading, "Autonomy-Rights Reading of AI Dignity Safeguarding: Democratic Regulation with Rights-Gated Enhancement").
narrative_ontology:topic_domain(ai_dignity_safeguarding__autonomy_rights_reading, "technological governance/theological ethics/philosophical anthropology").

domain_priors:requires_active_enforcement(ai_dignity_safeguarding__autonomy_rights_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_dignity_safeguarding__autonomy_rights_reading, '243e690e-e2ad-49e7-ab2e-33231738d7af').
narrative_ontology:cs_kernel_codification('243e690e-e2ad-49e7-ab2e-33231738d7af', formalized).
narrative_ontology:cs_authority_grounding('243e690e-e2ad-49e7-ab2e-33231738d7af', practice).
narrative_ontology:cs_interpretation_layer_present('243e690e-e2ad-49e7-ab2e-33231738d7af').
narrative_ontology:cs_reading_relation('243e690e-e2ad-49e7-ab2e-33231738d7af', ai_dignity_safeguarding__ai_dignity_imago_dei_reading, coexists_with).
narrative_ontology:cs_reading_relation('243e690e-e2ad-49e7-ab2e-33231738d7af', ai_dignity_safeguarding__ai_dignity_posthuman_continuity_reading, influences).
narrative_ontology:cs_axiom('243e690e-e2ad-49e7-ab2e-33231738d7af', foundational, dignity_grounded_in_autonomy_and_rights).
narrative_ontology:cs_axiom_status(dignity_grounded_in_autonomy_and_rights, holdable).
narrative_ontology:cs_axiom_grounding('243e690e-e2ad-49e7-ab2e-33231738d7af', dignity_grounded_in_autonomy_and_rights, deontological).
narrative_ontology:cs_axiom('243e690e-e2ad-49e7-ab2e-33231738d7af', foundational, enhancement_permitted_within_rights_limits).
narrative_ontology:cs_axiom_status(enhancement_permitted_within_rights_limits, holdable).
narrative_ontology:cs_axiom_grounding('243e690e-e2ad-49e7-ab2e-33231738d7af', enhancement_permitted_within_rights_limits, instrumental).
narrative_ontology:cs_axiom('243e690e-e2ad-49e7-ab2e-33231738d7af', secondary, automated_decisions_require_accountability).
narrative_ontology:cs_axiom_status(automated_decisions_require_accountability, holdable).
narrative_ontology:cs_axiom_grounding('243e690e-e2ad-49e7-ab2e-33231738d7af', automated_decisions_require_accountability, conventional).
narrative_ontology:cs_reference_frame('243e690e-e2ad-49e7-ab2e-33231738d7af', autonomous_rights_bearing_person).
narrative_ontology:cs_drift_state('243e690e-e2ad-49e7-ab2e-33231738d7af', contemporary_algorithmic_scale_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('243e690e-e2ad-49e7-ab2e-33231738d7af', '').
narrative_ontology:cs_kernel_id(ai_dignity_safeguarding__autonomy_rights_reading, ai_dignity_safeguarding).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_dignity_safeguarding__autonomy_rights_reading, autonomous_rights_bearers).
narrative_ontology:constraint_beneficiary(ai_dignity_safeguarding__autonomy_rights_reading, accountability_audit_industry).
narrative_ontology:constraint_beneficiary(ai_dignity_safeguarding__autonomy_rights_reading, incumbent_ai_platforms).
narrative_ontology:constraint_victim(ai_dignity_safeguarding__autonomy_rights_reading, algorithmic_decision_subjects).
narrative_ontology:constraint_victim(ai_dignity_safeguarding__autonomy_rights_reading, displaced_workers).
narrative_ontology:constraint_victim(ai_dignity_safeguarding__autonomy_rights_reading, small_ai_developers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(ai_dignity_safeguarding__autonomy_rights_reading, incumbent_ai_platforms).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Draft and enforce the transparency, accountability, labor, and privacy rules governing AI systems; run consultations, accredit auditors, and pursue enforcement actions. They depend on the regulated firms for the technical information needed to regulate them, and their authority rests on democratic mandate and periodic renewal rather than on any ability to exit the task.
narrative_ontology:constraint_stakeholder(ai_dignity_safeguarding__autonomy_rights_reading, democratic_regulators, agenda_setter,
    institutional, generational, constrained, national).

% Hold the rights the apparatus enforces: data protection, explanation of automated decisions affecting them, labor recourse, and channels of appeal. Protection reaches them unevenly depending on jurisdiction, enforcement budget, and their own resources; they can vote, petition, associate, and in the last resort relocate to jurisdictions whose rules they prefer.
narrative_ontology:constraint_stakeholder(ai_dignity_safeguarding__autonomy_rights_reading, autonomous_rights_bearers, beneficiary,
    organized, biographical, mobile, global).

% Bear the heaviest absolute compliance costs — documentation, conformity assessment, disclosure, works-council obligations — while holding the compliance capacity that converts those costs into barriers for smaller rivals. They gain liability clarity, certification signals that reassure customers, and privileged access to consultation processes through which they shape the rules they operate under. Relocation, restructuring, and multi-jurisdiction optimization are open to them.
narrative_ontology:constraint_stakeholder(ai_dignity_safeguarding__autonomy_rights_reading, incumbent_ai_platforms, payer,
    powerful, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(ai_dignity_safeguarding__autonomy_rights_reading, incumbent_ai_platforms, beneficiary).

% Face the same documentation, audit, and disclosure obligations as incumbents without dedicated compliance staff, so the fixed costs weigh far heavier against revenue. Their realistic options are niche markets, acquisition by larger firms, or relocation to lighter jurisdictions — each of which forecloses part of the business they built.
narrative_ontology:constraint_stakeholder(ai_dignity_safeguarding__autonomy_rights_reading, small_ai_developers, payer,
    moderate, biographical, constrained, regional).

% Are scored, ranked, filtered, and moderated by automated systems gating credit, employment, welfare, housing, and speech. They cannot opt out of the systems that decide their access, and the formal rights to explanation and appeal require time, literacy, and legal resources many do not have. Where deployment outruns enforcement, they carry the harm while the arrangement's legitimacy rests partly on the promise of protecting them.
narrative_ontology:constraint_stakeholder(ai_dignity_safeguarding__autonomy_rights_reading, algorithmic_decision_subjects, payer,
    powerless, immediate, trapped, national).

% Bear the transition costs of automation-driven restructuring — lost roles, wage pressure, retraining demands late in working lives. The apparatus entitles them to consultation and adjustment assistance, but their experience varies sharply with union density, local labor law, and whether the consulting duty arrives before or after the termination decision.
narrative_ontology:constraint_stakeholder(ai_dignity_safeguarding__autonomy_rights_reading, displaced_workers, payer,
    organized, biographical, constrained, regional).

% Sells the conformity assessments, bias audits, documentation services, and certifications the rules mandate. Its revenue scales with the breadth of regulatory scope and the pace of new mandates rather than with the resolution of the harms the rules target, and its personnel move fluidly between auditing firms, consultancies, and the agencies that oversee them.
narrative_ontology:constraint_stakeholder(ai_dignity_safeguarding__autonomy_rights_reading, accountability_audit_industry, beneficiary,
    organized, biographical, mobile, continental).

% Seek cognitive, biological, or augmentative technologies that the arrangement admits only through the consent-and-rights gate. Where a desired enhancement fails the gate they travel to permissive jurisdictions or gray markets, accepting safety risks the gate was designed to screen. They would argue for wider permissibility in the rulemaking conversations they rarely enter.
narrative_ontology:constraint_stakeholder(ai_dignity_safeguarding__autonomy_rights_reading, enhancement_frontier_seekers, excluded,
    moderate, biographical, mobile, global).

% Communities whose teaching locates human worth prior to any capability and treats the person as inviolable rather than optimizable. They would press for tighter limits on enhancement and on commodified self-modification, on grounds the current rulemaking framework does not admit as operative reasons. They appear episodically in ethics consultations but stand outside the technical core where the gate's boundaries are actually drawn.
narrative_ontology:constraint_stakeholder(ai_dignity_safeguarding__autonomy_rights_reading, religious_dignity_traditionalists, excluded,
    organized, generational, constrained, global).

% Litigate test cases, audit deployed systems, and publish comparisons between the apparatus's protective rhetoric and its delivered outcomes. Their findings supply the evidentiary base that legislatures, journalists, and affected individuals use to contest whether the arrangement's enforcement matches its promises.
narrative_ontology:constraint_stakeholder(ai_dignity_safeguarding__autonomy_rights_reading, digital_rights_watchdogs, observer,
    organized, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ai_dignity_safeguarding__autonomy_rights_reading, accountability_audit_industry).
narrative_ontology:fixing_cost_class(ai_dignity_safeguarding__autonomy_rights_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves collective-action problems that no single deployer can solve alone: common disclosure and documentation standards make AI systems legible to those affected; redress channels and liability rules make deployment decisions contestable; coordinated labor-transition and privacy floors prevent jurisdictional races to the bottom; the accreditation system supplies a shared trust signal.
% TRANSFER_FUNCTION: Moves compliance costs, disclosure obligations, and audit fees from the operating margins of AI developers and deployers into the enforcement and certification layer; moves explanation rights, appeal channels, and adjustment assistance toward affected individuals and workers; moves decision-audit visibility from private firms to public scrutiny.
% ABSENT_VOICES: Enhancement-frontier seekers would widen the consent-and-rights gate and are largely outside the rulemaking room; religious dignity traditionalists would tighten it on grounds the framework does not count as operative reasons; and the algorithmically governed themselves are formally consulted but practically voiceless — participation requires time, expertise, and standing that the most exposed lack, so the consensus behind the rules partly reflects who was able to attend.
% DISAPPEARANCE_RATIONALE: If the arrangement vanished overnight, deployment would reorganize around liability norms, market pressure, and whatever patchwork of sectoral rules survived: redress channels would close, the audit and certification industry would lose its mandated demand, opaque scoring would expand until scandal rebuilt some replacement, and enhancement markets would fragment between permissive and restrictive jurisdictions almost immediately.
% FOUNDING_PROBLEM: Unchecked AI deployment produced opaque consequential decisions, labor displacement without recourse, privacy erosion at scale, and human-enhancement pressures advancing faster than any deliberative process could evaluate them; the arrangement was built to make AI governable within a rights-respecting democratic frame while keeping cautious space open for beneficial enhancement.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: digital-rights litigation dockets and watchdog audits document continuing opacity harms; labor statistics and union testimony document displacement without adequate adjustment; academic audit literature documents discriminatory automated decisions surviving existing rules. Industry associations independently attest the compliance burden is real, confirming the cost side from the paying seat. No seat attests the founding problems are solved.
narrative_ontology:disappearance_verdict(ai_dignity_safeguarding__autonomy_rights_reading, world_rearranges).
narrative_ontology:founding_problem_status(ai_dignity_safeguarding__autonomy_rights_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ai_dignity_safeguarding__autonomy_rights_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(ai_dignity_safeguarding__autonomy_rights_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ai_dignity_safeguarding__autonomy_rights_reading, 0.48, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ai_dignity_safeguarding__autonomy_rights_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(ai_dignity_safeguarding__autonomy_rights_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(ai_dignity_safeguarding__autonomy_rights_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.48 sits in the low-to-moderate band the structural delta predicts: the coordination core is real (disclosure standards, redress channels, and liability rules solve genuine collective-action problems no single deployer can solve alone), but three extraction margins run through the same structure — compliance costs that scale regressively against small developers, an audit/certification layer whose revenue grows with regulatory scope rather than with resolved harms, and legitimacy harvested from decision-subjects whose protection arrives slower than the systems harming them. Suppression 0.42 is authored as a raw structural property, unscaled: the apparatus compels disclosure and audit, but alternatives remain partly open — jurisdictional relocation, open-source development paths, gray-market enhancement — so the option space narrows without closing. Theater_ratio 0.32 reflects documented ethics-washing (checkbox impact assessments, advisory boards without authority) alongside functioning core mechanisms. Accessibility_collapse 0.38 is low because rival dignity framings and lighter jurisdictions remain visible and usable — this arrangement does not collapse the alternative space the way a natural limit would. Resistance 0.46 registers sustained industry lobbying, jurisdictional competition, and enhancement advocacy. The temporal series run on ONE shared grid (points 0,4,8,12,16,20,24) with all three metrics authored at every point; suppression_requirement is tracked because the story specifically traces enforcement-capacity build-up, which rose monotonically as the apparatus matured from light-touch principles to hard conformity-assessment law.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute divergent types from identical structural data. From the regulator's seat the arrangement is legitimate institution-building under democratic mandate. From the incumbent platform's seat it is a manageable cost that converts into competitive advantage — closer to a subsidy it administers than a burden it suffers. From the small developer's seat the same rules are an existential fixed cost. From the decision-subject's seat the arrangement is a promise of protection that keeps arriving after the harm. From the audit industry's seat it is a revenue line. The engine computes these divergences from power, exit, and directional position; nothing in the authored claim adjudicates among them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive low directionality for autonomous_rights_bearers (protected, mobile, organized) and accountability_audit_industry (paid by the enforcement demand the rules create). Victim declarations drive high directionality for algorithmic_decision_subjects (trapped, powerless — nearest the full-target end), displaced_workers, and small_ai_developers. One override is declared: for the powerful power atom (occupied by incumbent_ai_platforms), the automatic derivation from the dual payer/beneficiary role would land near symmetric (d≈0.5), but the moat economics — compliance capacity that small rivals cannot replicate, liability clarity, certification as trust signal — make incumbents net beneficiaries of the arrangement they nominally pay into. The override sets d=0.30 to record that structural fact. Scope amplification applies modestly: the arrangement operates at national-to-global scope, making verification harder and tilting effective extraction upward for trapped targets.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — opaque automated decisions, displacement without recourse, privacy erosion, enhancement pressure outrunning deliberation — remains live, corroborated from outside the benefiting parties by watchdog litigation, labor-market statistics, and audit literature. That liveness blocks piton classification: the arrangement has not outlived its function. The genuine coordination function blocks snare classification: the disclosure and redress infrastructure solves real problems even where enforcement lags. The asymmetric extraction margins block rope classification: someone specific pays for the audit layer, and someone specific waits unprotected while the system trades on their inclusion. No sunset clause exists, so scaffold does not apply. The classification discipline here prevents two symmetrical mislabels: reading the compliance burden as pure extraction erases the solved collective-action problems; reading the arrangement as pure coordination erases who finances the enforcement layer and who bears its shortfalls.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'This constraint instantiates one reading (autonomy_rights) of the contested kernel ai_dignity_safeguarding — how would the imago_dei_reading or the posthuman_continuity_reading restructure the victim set, the enhancement gate, and the enforcement object?',
    'Comparative analysis of the sibling stories'' structural deltas; legislative episodes in which a rival reading captures the rulemaking agenda (enhancement prohibition waves versus deregulatory openings) reveal which reading holds authority in a given jurisdiction.',
    'If another reading captures the rulemaking seat, this arrangement''s beneficiaries and victims shift or invert — enhancement seekers become protected rather than excluded, or the consent gate dissolves entirely; epsilon and classification are recomputed for the successor arrangement, not averaged across readings.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Committer structure: one reading of a triply-read dignity kernel; sibling readings are separate constraints.').

omega_variable(
    residual_harm_attribution,
    'Are algorithmic_decision_subjects victims of the regulatory arrangement itself, or of the gap between its protective promise and its enforcement reach?',
    'Compare measured harm rates in fully-enforced jurisdictions against under-enforced ones; if harms track enforcement capacity rather than the arrangement''s existence, the harm is an enforcement deficit rather than extraction through the structure.',
    'If attribution runs to the enforcement gap, effective extraction falls toward rope territory; if the arrangement''s design choices (standing limits, burden-of-proof placement on individuals) themselves obstruct redress, the extraction component stands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(residual_harm_attribution, empirical, 'Whether residual algorithmic harms count against the arrangement or against its incompleteness.').

omega_variable(
    compliance_moat_magnitude,
    'Does the compliance burden function primarily as incumbent entrenchment or as a neutral fixed cost of trustworthy deployment?',
    'Measure startup formation rates and market concentration before and after major compliance milestones; decompose compliance spending into safety-relevant versus documentation-only components.',
    'A strong moat signal raises effective extraction and pushes the tangled_rope reading toward the snare boundary; a weak moat supports the coordination-first reading of the same structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(compliance_moat_magnitude, empirical, 'Magnitude of the regulatory-moat component inside total compliance costs.').

omega_variable(
    enforcement_ratchet_direction,
    'Does the rising suppression_requirement trajectory represent maturing legitimate enforcement, or a bureaucratic ratchet in which the enforcement layer protects its own growth?',
    'Track whether marginal enforcement spending correlates with reduced measured harms or merely with expanded audit scope; examine sunset reviews and mandate renewals for evidence of self-perpetuation independent of outcomes.',
    'A ratchet pattern would raise expected theater_ratio in mature jurisdictions and support piton-drift hypotheses for the enforcement layer; harm-correlated growth supports genuinely functional enforcement.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(enforcement_ratchet_direction, empirical, 'Direction and quality of the enforcement-capacity build-out over the interval.').

omega_variable(
    enhancement_gate_boundary,
    'Where exactly does consent-based, rights-preserving enhancement end and dignity-violating enhancement begin — is the gate''s boundary determinate enough to administer without discretionary drift?',
    'Accumulated adjudicated cases and regulatory guidance forming a jurisprudence of the gate; comparative outcomes across jurisdictions that draw the line differently.',
    'An indeterminate boundary makes the gate administrable only through discretionary enforcement, raising suppression and shifting the structure toward enforced-extraction readings; a determinate boundary stabilizes the coordination core.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(enhancement_gate_boundary, conceptual, 'Determinacy of the consent-and-rights enhancement gate.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_dignity_safeguarding__autonomy_rights_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(aidig_ar_tr_t0, ai_dignity_safeguarding__autonomy_rights_reading, theater_ratio, 0, 0.14).
narrative_ontology:measurement(aidig_ar_tr_t4, ai_dignity_safeguarding__autonomy_rights_reading, theater_ratio, 4, 0.17).
narrative_ontology:measurement(aidig_ar_tr_t8, ai_dignity_safeguarding__autonomy_rights_reading, theater_ratio, 8, 0.2).
narrative_ontology:measurement(aidig_ar_tr_t12, ai_dignity_safeguarding__autonomy_rights_reading, theater_ratio, 12, 0.24).
narrative_ontology:measurement(aidig_ar_tr_t16, ai_dignity_safeguarding__autonomy_rights_reading, theater_ratio, 16, 0.27).
narrative_ontology:measurement(aidig_ar_tr_t20, ai_dignity_safeguarding__autonomy_rights_reading, theater_ratio, 20, 0.3).
narrative_ontology:measurement(aidig_ar_tr_t24, ai_dignity_safeguarding__autonomy_rights_reading, theater_ratio, 24, 0.32).

% Extraction over time
narrative_ontology:measurement(aidig_ar_be_t0, ai_dignity_safeguarding__autonomy_rights_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(aidig_ar_be_t4, ai_dignity_safeguarding__autonomy_rights_reading, base_extractiveness, 4, 0.34).
narrative_ontology:measurement(aidig_ar_be_t8, ai_dignity_safeguarding__autonomy_rights_reading, base_extractiveness, 8, 0.38).
narrative_ontology:measurement(aidig_ar_be_t12, ai_dignity_safeguarding__autonomy_rights_reading, base_extractiveness, 12, 0.42).
narrative_ontology:measurement(aidig_ar_be_t16, ai_dignity_safeguarding__autonomy_rights_reading, base_extractiveness, 16, 0.45).
narrative_ontology:measurement(aidig_ar_be_t20, ai_dignity_safeguarding__autonomy_rights_reading, base_extractiveness, 20, 0.47).
narrative_ontology:measurement(aidig_ar_be_t24, ai_dignity_safeguarding__autonomy_rights_reading, base_extractiveness, 24, 0.48).

% Suppression requirement over time
narrative_ontology:measurement(aidig_ar_su_t0, ai_dignity_safeguarding__autonomy_rights_reading, suppression_requirement, 0, 0.18).
narrative_ontology:measurement(aidig_ar_su_t4, ai_dignity_safeguarding__autonomy_rights_reading, suppression_requirement, 4, 0.24).
narrative_ontology:measurement(aidig_ar_su_t8, ai_dignity_safeguarding__autonomy_rights_reading, suppression_requirement, 8, 0.29).
narrative_ontology:measurement(aidig_ar_su_t12, ai_dignity_safeguarding__autonomy_rights_reading, suppression_requirement, 12, 0.33).
narrative_ontology:measurement(aidig_ar_su_t16, ai_dignity_safeguarding__autonomy_rights_reading, suppression_requirement, 16, 0.37).
narrative_ontology:measurement(aidig_ar_su_t20, ai_dignity_safeguarding__autonomy_rights_reading, suppression_requirement, 20, 0.4).
narrative_ontology:measurement(aidig_ar_su_t24, ai_dignity_safeguarding__autonomy_rights_reading, suppression_requirement, 24, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_dignity_safeguarding__autonomy_rights_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(ai_dignity_safeguarding__autonomy_rights_reading, ai_dignity_imago_dei_reading).
narrative_ontology:affects_constraint(ai_dignity_safeguarding__autonomy_rights_reading, ai_dignity_posthuman_continuity_reading).

% DUAL FORMULATION NOTE:
% Constraint family: 'AI dignity safeguarding' is a contested kernel with three readings emitted as separate stories. This file is the autonomy_rights_reading (regulated-tool category, rights-gated enhancement, victims include the opaquely-governed and the displaced). The imago_dei_reading shares the referent domain but authors a different victim set (any treatment of the person as optimizable) and rejects the enhancement permissions this reading grants. The posthuman_continuity_reading authors a near-empty victim set for consensual enhancement and treats this reading's gate itself as the extractive element. Epsilon differs across the family because each reading assesses a DIFFERENT standing arrangement, not one arrangement under different observables; the stories are linked by network edges rather than merged.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(ai_dignity_safeguarding__autonomy_rights_reading, powerful, 0.3).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
