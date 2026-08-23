% ============================================================================
% CONSTRAINT STORY: humane_treatment_standard__proportionality_balancing
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_humane_treatment_standard__proportionality_balancing, []).

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
 *   constraint_id: humane_treatment_standard__proportionality_balancing
 *   human_readable: Common Article 3 Proportionality Balancing Standard (Judicial Gatekeeping Reading)
 *   domain: international_humanitarian_law/state_security/human_rights
 *
 * SUMMARY:
 *   A judicially administered balancing regime governs the treatment of
 *   persons detained in non-international armed conflict and security
 *   operations. Reviewing courts and tribunals decide, case by case, whether
 *   a proposed interrogation or detention practice weighs acceptably against
 *   the security need it serves: under this arrangement no technique is
 *   categorically barred, and none is available on executive assertion alone.
 *   Interrogation programs operate under documentation requirements, review
 *   thresholds, and the standing possibility that a tribunal will disallow a
 *   method after the fact. Detainees hold no seat in the weighing; their
 *   bodily and psychological integrity is the interest on the scale. The
 *   constraint is CLAIMED here as tangled_rope while the metrics are authored
 *   independently from its observed operation — any divergence the engine
 *   computes is the datum, not an error. Per the epsilon-referent rule for
 *   kernel-reading stories, extractiveness is scored for the standing
 *   balancing arrangement as this reading itself appraises it, never for the
 *   stricter counterfactual arrangements alternative readings would install.
 *   KEY AGENTS (by structural relationship): - detained_persons: Primary
 *   target (powerless/trapped) — bears the residual harm whenever the balance
 *   resolves toward security - national_security_agencies: Primary
 *   beneficiary (powerful/constrained) — receives court-calibrated
 *   operational latitude and legal cover - judicial_review_bodies:
 *   Agenda-setter and institutional beneficiary
 *   (institutional/identity_locked) — administers the weighing; jurisdiction
 *   accrues to them - state_executives: Secondary beneficiary and partial
 *   payer (institutional/constrained) — gains treaty-compliance credentials,
 *   pays in constrained technique menus - field_interrogators: Operational
 *   payers (moderate/constrained) — carry documentation burdens, career risk,
 *   and moral injury - humanitarian_monitoring_bodies: Analytical observer
 *   (organized/analytical) — sees the full structure across jurisdictions,
 *   holds no deciding seat - absolute_prohibition_advocates: Excluded voice
 *   (organized/constrained) — their claim enters the procedure only as one
 *   weighted interest, never as a trump
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(humane_treatment_standard__proportionality_balancing, 0.58).
domain_priors:suppression_score(humane_treatment_standard__proportionality_balancing, 0.62).
domain_priors:theater_ratio(humane_treatment_standard__proportionality_balancing, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(humane_treatment_standard__proportionality_balancing, extractiveness, 0.58).
narrative_ontology:constraint_metric(humane_treatment_standard__proportionality_balancing, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(humane_treatment_standard__proportionality_balancing, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(humane_treatment_standard__proportionality_balancing, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(humane_treatment_standard__proportionality_balancing, resistance, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(humane_treatment_standard__proportionality_balancing, tangled_rope).
narrative_ontology:human_readable(humane_treatment_standard__proportionality_balancing, "Common Article 3 Proportionality Balancing Standard (Judicial Gatekeeping Reading)").
narrative_ontology:topic_domain(humane_treatment_standard__proportionality_balancing, "international_humanitarian_law/state_security/human_rights").

domain_priors:requires_active_enforcement(humane_treatment_standard__proportionality_balancing).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(humane_treatment_standard__proportionality_balancing, '47642004-3165-41b5-bf57-402e26bc4188').
narrative_ontology:cs_kernel_codification('47642004-3165-41b5-bf57-402e26bc4188', fixed_text).
narrative_ontology:cs_authority_grounding('47642004-3165-41b5-bf57-402e26bc4188', lineage).
narrative_ontology:cs_interpretation_layer_present('47642004-3165-41b5-bf57-402e26bc4188').
narrative_ontology:cs_reading_relation('47642004-3165-41b5-bf57-402e26bc4188', humane_treatment_standard__absolute_prohibition, coexists_with).
narrative_ontology:cs_reading_relation('47642004-3165-41b5-bf57-402e26bc4188', humane_treatment_standard__contextual_necessity, coexists_with).
narrative_ontology:cs_axiom('47642004-3165-41b5-bf57-402e26bc4188', foundational, dignity_security_commensurable).
narrative_ontology:cs_axiom_status(dignity_security_commensurable, holdable).
narrative_ontology:cs_axiom_grounding('47642004-3165-41b5-bf57-402e26bc4188', dignity_security_commensurable, instrumental).
narrative_ontology:cs_axiom('47642004-3165-41b5-bf57-402e26bc4188', foundational, case_by_case_adjudication_supremacy).
narrative_ontology:cs_axiom_status(case_by_case_adjudication_supremacy, holdable).
narrative_ontology:cs_axiom_grounding('47642004-3165-41b5-bf57-402e26bc4188', case_by_case_adjudication_supremacy, conventional).
narrative_ontology:cs_axiom('47642004-3165-41b5-bf57-402e26bc4188', secondary, procedural_safeguards_sufficiency).
narrative_ontology:cs_axiom_status(procedural_safeguards_sufficiency, holdable).
narrative_ontology:cs_axiom_grounding('47642004-3165-41b5-bf57-402e26bc4188', procedural_safeguards_sufficiency, instrumental).
narrative_ontology:cs_reference_frame('47642004-3165-41b5-bf57-402e26bc4188', judicial_balancing_equilibrium).
narrative_ontology:cs_drift_state('47642004-3165-41b5-bf57-402e26bc4188', contemporary, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('47642004-3165-41b5-bf57-402e26bc4188', '').
narrative_ontology:cs_kernel_id(humane_treatment_standard__proportionality_balancing, humane_treatment_standard).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(humane_treatment_standard__proportionality_balancing, national_security_agencies).
narrative_ontology:constraint_beneficiary(humane_treatment_standard__proportionality_balancing, judicial_review_bodies).
narrative_ontology:constraint_beneficiary(humane_treatment_standard__proportionality_balancing, state_executives).
narrative_ontology:constraint_victim(humane_treatment_standard__proportionality_balancing, detained_persons).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(humane_treatment_standard__proportionality_balancing, national_security_agencies).
narrative_ontology:constraint_victim(humane_treatment_standard__proportionality_balancing, state_executives).
narrative_ontology:constraint_victim(humane_treatment_standard__proportionality_balancing, field_interrogators).
narrative_ontology:constraint_vindicates(humane_treatment_standard__proportionality_balancing, proportionality_principle).
narrative_ontology:constraint_vindicates(humane_treatment_standard__proportionality_balancing, case_by_case_adjudication).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Persons interned during non-international armed conflicts and security operations. Their bodily and psychological integrity is the interest placed on the scale in each determination. They cannot leave custody, choose the tribunal, or decline the weighing; they reach the procedure only through counsel they may not have. When the balance resolves toward security they absorb the authorized harm in full; when it resolves toward dignity they receive a protection they had no hand in shaping.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__proportionality_balancing, detained_persons, payer,
    powerless, biographical, trapped, global).

% Run detention and interrogation programs. They receive court-calibrated permission to employ specific techniques, converting legal risk into operational latitude carrying a tribunal's imprimatur. They pay in disallowed methods, documentation obligations, and oversight exposure. They cannot exit the legal order, though they can shift operations toward venues beyond easy judicial reach, at reputational and alliance cost.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__proportionality_balancing, national_security_agencies, beneficiary,
    powerful, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(humane_treatment_standard__proportionality_balancing, national_security_agencies, payer).

% Courts and tribunals that decide treatment permissibility case by case, setting the terms under which security agencies may operate. Each determination adds to their jurisdiction and deepens their role. The gatekeeping function has become constitutive of their institutional self-understanding as guardians of both dignity and security; declining to weigh would mean ceding the field to executive self-certification, which their identity cannot accommodate.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__proportionality_balancing, judicial_review_bodies, agenda_setter,
    institutional, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(humane_treatment_standard__proportionality_balancing, judicial_review_bodies, beneficiary).

% Governments party to the treaty framework. They gain court-legitimated detention policy, treaty-compliance credentials, and deniability when individual determinations are attributed to the judiciary. They pay in constrained technique menus, litigation exposure, and periodic political cost when review outcomes or program details become public. Exiting would mean denouncing the framework, at severe diplomatic cost.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__proportionality_balancing, state_executives, beneficiary,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(humane_treatment_standard__proportionality_balancing, state_executives, payer).

% Personnel who execute approved techniques under documentation and review requirements. They bear career and legal risk when a method they used is later disallowed, and moral injury when one they requested is allowed. They cannot leave the assignment structure without professional cost, and the permissibility of their actions is decided by others, sometimes long after the fact.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__proportionality_balancing, field_interrogators, payer,
    moderate, biographical, constrained, regional).

% ICRC-style institutions that visit detention sites, compile confidential findings, and press for stronger protections across jurisdictions. They see the full structure — programs, sites, outcomes — but hold no deciding seat; their leverage is confidentiality-backed persuasion and, failing that, publicity.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__proportionality_balancing, humanitarian_monitoring_bodies, observer,
    organized, generational, analytical, global).

% UN special procedures, human rights organizations, and parts of the legal academy who hold that weighing dignity against security at all already concedes the essential point. Inside the balancing procedure their claim can enter only as one weighted interest among others, never as an unconditional limit. They remain in the wider discourse through reporting, treaty-body review, and scholarship, but not in the seat where outcomes are decided.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__proportionality_balancing, absolute_prohibition_advocates, excluded,
    organized, generational, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(humane_treatment_standard__proportionality_balancing, national_security_agencies).
narrative_ontology:fixing_cost_class(humane_treatment_standard__proportionality_balancing, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared decision procedure that lets states conduct security detention while maintaining a humanitarian floor: courts weigh detainee dignity against security need case by case, converting an unmanageable collision between humanitarian minima and operational necessity into administrable adjudication with documentation, review thresholds, and after-the-fact disallowance.
% TRANSFER_FUNCTION: Moves decision-authority over detainee treatment from executive assertion to judicial determination; moves bodily and psychological risk onto detainees, who absorb the residual whenever the security weight prevails; and delivers legitimation — court-signed permission — to the security agencies operating the approved techniques.
% ABSENT_VOICES: Absolute-prohibition jurists, torture survivors, and the detainees themselves are structurally absent from the deciding seat: survivors rarely testify in the forums that set technique policy, and the unconditional-limit claim has no admissible form inside a weighing procedure — it can only arrive as one more interest to be balanced. They are present in the surrounding discourse (treaty bodies, special procedures, scholarship) but not where outcomes are determined.
% DISAPPEARANCE_RATIONALE: If the balancing standard vanished overnight, every pending treatment determination would lose its deciding procedure: courts would lose the gatekeeping jurisdiction that currently constitutes their role, agencies would fall back on self-certification or face newly rigid floors, and detention practice would reorganize around whichever pole the surviving institutions favored. The entire adjudicative apparatus — review thresholds, documentation regimes, approved-technique lists — exists only relative to the weighing procedure.
% FOUNDING_PROBLEM: After 1949, states faced the problem of binding themselves to humane-treatment minima in internal conflicts where reciprocity is absent and security imperatives are acute — without adopting a standard so rigid that security institutions would simply disregard it, nor so loose that it licensed whatever the executive chose to do. The balancing reading was built to solve that rigidity-versus-usability dilemma by interposing a judicial weigher.
% FOUNDING_PROBLEM_CORROBORATION: Military legal advisers and security officials attest the necessity side of the founding problem from inside the benefiting set. Outside it, the ICRC's detention-visiting findings, UN Special Rapporteur reporting, and comparative legal scholarship attest that the necessity framing is systematically inflated and that jurisdictions facing comparable threat levels operate under fixed absolute floors without operational collapse — corroborating that the problem the arrangement solves is partly constructed by the arrangement's own beneficiaries. No attesting source exists that is fully outside the dispute; both clusters corroborate their own side, which is itself the signal recorded in the status.
narrative_ontology:disappearance_verdict(humane_treatment_standard__proportionality_balancing, world_rearranges).
narrative_ontology:founding_problem_status(humane_treatment_standard__proportionality_balancing, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(humane_treatment_standard__proportionality_balancing, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(humane_treatment_standard__proportionality_balancing, 'none', 1).
narrative_ontology:epsilon_provenance(humane_treatment_standard__proportionality_balancing, 0.58, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(humane_treatment_standard__proportionality_balancing_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(humane_treatment_standard__proportionality_balancing, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(humane_treatment_standard__proportionality_balancing_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.58: the balancing regime genuinely constrains interrogators relative to unchecked executive discretion — methods are disallowed, documentation is compelled, and the possibility of review disciplines practice — but it also systematically leaves detainees absorbing the residual in every case where the security weight prevails, and the procedure itself confers legality on harms a fixed floor would bar. Suppression 0.62 is a raw structural property, unscaled by power or scope: it rests on custody itself, site secrecy, and classification of interrogation guidance, not on any scaled multiplier. Theater_ratio 0.40: the review function is real (techniques have been banned, detainees occasionally protected), but a substantial and growing share of activity is justificatory — written opinions and compliance archives that ratify outcomes reached elsewhere, peaking around 2014 with the documentation surge that followed public scrutiny of detention programs. Accessibility_collapse 0.60: for actors inside the adjudicative framework, the two poles — fixed absolute floors and unconstrained executive discretion — present as equally unavailable extremes, so the understood alternative set collapses toward the balancing middle; the poles remain live outside the framework, which caps collapse below mountain-grade. Resistance 0.52: detainees and their counsel litigate, monitoring bodies publish, and parts of the legal academy attack the weighing methodology — real but structurally contained resistance, since the framework converts objection into one more input to be weighed. Temporal shape: all three series run on one shared grid (1949, 1969, 1987, 1999, 2006, 2014, 2025). Enforcement machinery builds monotonically from bare treaty text to dense judicial oversight (suppression_requirement 0.20 to 0.62), extraction climbs with each formalization of the balancing method (0.30 to a 2014 peak of 0.60), and theater pulses with crisis eras — the post-2001 litigation surge and the 2014 accountability-documentation wave — before partial consolidation to 0.40 at interval end, where the base_properties scalars are measured. This is a crisis-pulse shape, not a true oscillation: the spikes track external scrutiny events, not an internal reinforcement cycle.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute divergently. From the bench, the arrangement is due process itself: a procedure that protects detainees from the worst and agencies from lawlessness, administered by neutral weighers. From the cell, the same procedure is a machine that periodically authorizes one's own degradation after weighing one's interests without one's presence — protection arrives as someone else's calculation. From the agency, it is calibrated permission: a way to convert legal risk into operational latitude with a court's signature attached. The judicial seat is additionally identity_locked in a specific sense: the gatekeeping function has become constitutive of these bodies' institutional self-understanding as guardians of both dignity and security, so recusal is not merely costly but unthinkable — a court that declined to weigh would, in its own eyes, abandon detainees to executive self-certification. If that identity frame broke (if courts came to see the weighing as legitimation rather than protection), the arrangement would not survive in its current form: either the fixed-floor pole or the executive-discretion pole would absorb the vacated territory. Detainees, by contrast, cannot coalition: custody isolates them by design, which is why their powerlessness does not aggregate into organized resistance despite their numbers.
 *
 * DIRECTIONALITY LOGIC:
 *   The beneficiary/victim declarations drive the derivation and no overrides are needed. detained_persons (victim, powerless, trapped, global scope) derive near the full-target end: they bear the transferred dignity latitude and possess no exit whatsoever — custody forecloses departure, and the procedure offers no seat. national_security_agencies (beneficiary with secondary payer position, powerful, constrained) derive near the beneficiary end but damped: they receive the operative good (approved techniques, defensible programs) while paying in prohibited methods and oversight exposure. judicial_review_bodies (agenda_setter with secondary beneficiary position) derive near the beneficiary end: jurisdiction and institutional purpose accrue to them with each determination, even as they experience themselves as neutral administrators. state_executives sit mildly beneficiary-side: credentials and deniability in, constrained menus and political exposure out. field_interrogators occupy the middle: they pay procedural burdens and career risk yet receive the legal cover that makes their work survivable, so neither pole fits. Because the constraint operates at global scope with routine recourse to classified sites, verification difficulty amplifies effective extraction for the target seat — the engine owns that arithmetic; the authored scope atoms supply its input. Suppression contributes no scaling anywhere: it is a raw structural property, and the commentary treats it as such.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope claim prevents mislabeling in both directions. Reading the arrangement as pure coordination (the bench's self-description: a procedure protecting everyone) ignores the asymmetric residue — detainees bear what the balance trades away, and no beneficiary seat shares that exposure. Reading it as pure extraction (the abolitionist description: litigation-grade cover for coercion) ignores the genuine constraint the bench imposes relative to the discretion it replaced — methods are disallowed, and the disallowances bind. Both halves are structurally present, which is the tangled_rope signature: coordination function plus asymmetric extraction through the same structure, held together by active judicial enforcement. On mandatrophy: the founding problem (binding wartime detention to humane minima without paralyzing security operations) remains contested-live rather than dead, so no resolved-mandatrophy flag is authored; but the function is migrating — the measurement series shows the justificatory share of activity growing faster than the protective share, which is the early drift signature worth watching. If the ratchet omega resolves affirmatively and theater continues climbing past 0.5, the arrangement's coordination half is atrophying and the classification should be revisited downward.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_commitment,
    'This constraint instantiates the proportionality_balancing reading of the humane_treatment_standard kernel; what changes structurally if a sibling reading governs the same referent arrangement instead?',
    'Comparative classification of the sibling stories (humane_treatment_standard__absolute_prohibition, humane_treatment_standard__contextual_necessity): same standing arrangement as referent, different epsilon, different victim sets, different deciding seat.',
    'Under the absolute sibling the balancing seat disappears entirely (fixed floors, courts as enforcers rather than weighers, no per-case discretion); under the contextual sibling the deciding seat migrates to the executive and courts audit baselines only. This story''s court-centric structure, and its particular extraction profile, exist only under this reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_commitment, conceptual, 'Committer-frame record: this file is one reading of the humane_treatment_standard kernel, not the kernel itself.').

omega_variable(
    dignity_claim_modality_disagreement,
    'Where the kernel contest is located: are detainee-dignity claims categorical trumps (absolute reading), overridable defaults (contextual-necessity reading), or weighted interests commensurable with security needs (this reading)?',
    'Conceptual, not empirical: resolution requires committing to a theory of the norm''s modality; no dataset settles whether dignity interests are commensurable with security interests.',
    'Adopting the trump view dissolves this constraint into the absolute sibling (the balancing seat and its extraction vanish); adopting the default view dissolves it into the contextual sibling (courts lose the deciding seat and the arrangement''s center of gravity moves to the executive).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(dignity_claim_modality_disagreement, conceptual, 'The structural locus of the kernel contest: the modality of dignity claims under Common Article 3.').

omega_variable(
    balancing_ratchet_drift,
    'Does the security weight in successive judicial determinations ratchet upward, with each precedent normalizing what the previous one conceded, or does the dignity-security equilibrium hold?',
    'Longitudinal coding of published determinations across the interval: technique approval rates, dissent rates, floor-revision events, and the gap between initial review standards and steady-state approvals.',
    'A confirmed ratchet drives effective extraction upward over time and converges this reading''s practice toward the contextual-necessity sibling in operation; classification drifts from tangled_rope toward snare as the coordination half atrophies.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(balancing_ratchet_drift, empirical, 'Whether case-by-case balancing drifts monotonically toward the security pole.').

omega_variable(
    gatekeeper_independence,
    'Are the reviewing courts independent gatekeepers, or has the gatekeeping function fused with legitimation of predetermined detention policy?',
    'Compare pre-review expectations with outcomes: reversal rates on agency requests, incidence of ex parte and classified-an nex procedures, and correlation between security-branch requests and approvals.',
    'If legitimation dominates, the authored theater_ratio understates performance and the arrangement operates closer to cover than constraint; effective extraction exceeds the authored value and the payer seat''s computed classification hardens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(gatekeeper_independence, empirical, 'Court independence versus legitimation function in the balancing seat.').

omega_variable(
    residual_harm_incidence,
    'How often does the balance resolve in favor of security, and with what measured harm to detainees — that is, what is the realized rather than nominal rate at which the arrangement transfers dignity latitude?',
    'Detention-site monitoring data cross-referenced with published determinations; survivor testimony correlated against approved-technique lists.',
    'High incidence with serious harm pushes realized extraction above the authored 0.58 and strengthens the extraction half of the tangled_rope structure; low incidence supports emphasizing the coordination half and softening the payer-seat classification.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(residual_harm_incidence, empirical, 'Realized frequency and severity of security-favoring balancing outcomes.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(humane_treatment_standard__proportionality_balancing, 1949, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(huma_tr_t1949, humane_treatment_standard__proportionality_balancing, theater_ratio, 1949, 0.1).
narrative_ontology:measurement(huma_tr_t1969, humane_treatment_standard__proportionality_balancing, theater_ratio, 1969, 0.15).
narrative_ontology:measurement(huma_tr_t1987, humane_treatment_standard__proportionality_balancing, theater_ratio, 1987, 0.25).
narrative_ontology:measurement(huma_tr_t1999, humane_treatment_standard__proportionality_balancing, theater_ratio, 1999, 0.35).
narrative_ontology:measurement(huma_tr_t2006, humane_treatment_standard__proportionality_balancing, theater_ratio, 2006, 0.42).
narrative_ontology:measurement(huma_tr_t2014, humane_treatment_standard__proportionality_balancing, theater_ratio, 2014, 0.48).
narrative_ontology:measurement(huma_tr_t2025, humane_treatment_standard__proportionality_balancing, theater_ratio, 2025, 0.4).

% Extraction over time
narrative_ontology:measurement(huma_be_t1949, humane_treatment_standard__proportionality_balancing, base_extractiveness, 1949, 0.3).
narrative_ontology:measurement(huma_be_t1969, humane_treatment_standard__proportionality_balancing, base_extractiveness, 1969, 0.35).
narrative_ontology:measurement(huma_be_t1987, humane_treatment_standard__proportionality_balancing, base_extractiveness, 1987, 0.45).
narrative_ontology:measurement(huma_be_t1999, humane_treatment_standard__proportionality_balancing, base_extractiveness, 1999, 0.52).
narrative_ontology:measurement(huma_be_t2006, humane_treatment_standard__proportionality_balancing, base_extractiveness, 2006, 0.58).
narrative_ontology:measurement(huma_be_t2014, humane_treatment_standard__proportionality_balancing, base_extractiveness, 2014, 0.6).
narrative_ontology:measurement(huma_be_t2025, humane_treatment_standard__proportionality_balancing, base_extractiveness, 2025, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(huma_su_t1949, humane_treatment_standard__proportionality_balancing, suppression_requirement, 1949, 0.2).
narrative_ontology:measurement(huma_su_t1969, humane_treatment_standard__proportionality_balancing, suppression_requirement, 1969, 0.28).
narrative_ontology:measurement(huma_su_t1987, humane_treatment_standard__proportionality_balancing, suppression_requirement, 1987, 0.38).
narrative_ontology:measurement(huma_su_t1999, humane_treatment_standard__proportionality_balancing, suppression_requirement, 1999, 0.5).
narrative_ontology:measurement(huma_su_t2006, humane_treatment_standard__proportionality_balancing, suppression_requirement, 2006, 0.62).
narrative_ontology:measurement(huma_su_t2014, humane_treatment_standard__proportionality_balancing, suppression_requirement, 2014, 0.66).
narrative_ontology:measurement(huma_su_t2025, humane_treatment_standard__proportionality_balancing, suppression_requirement, 2025, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(humane_treatment_standard__proportionality_balancing, enforcement_mechanism).
narrative_ontology:affects_constraint(humane_treatment_standard__proportionality_balancing, humane_treatment_standard__absolute_prohibition).
narrative_ontology:affects_constraint(humane_treatment_standard__proportionality_balancing, humane_treatment_standard__contextual_necessity).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'what Common Article 3 requires for detainee treatment' decomposes into three structurally distinct constraints — one per reading of the fixed 1949 text. Each sibling carries its own epsilon, victim set, deciding seat, and enforcement structure; this file instantiates the proportionality_balancing member. The siblings are linked bidirectionally through affects_constraints so contamination propagation and cross-reading comparison operate at the family level. Epsilon differs across members because the referent arrangement is appraised by each reading's own lights: the absolute reading scores the same standing arrangement against a fixed-floor standard (higher extraction), the contextual reading against a baseline-plus-override standard (lower), and this reading against its own balancing standard (authored here at 0.58).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
