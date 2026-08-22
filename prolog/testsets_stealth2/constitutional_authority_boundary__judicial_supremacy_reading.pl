% ============================================================================
% CONSTRAINT STORY: constitutional_authority_boundary__judicial_supremacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_constitutional_authority_boundary__judicial_supremacy_reading, []).

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
 *   constraint_id: constitutional_authority_boundary__judicial_supremacy_reading
 *   human_readable: Judicial Supremacy Reading of the Constitutional Authority Boundary
 *   domain: constitutional/political/institutional
 *
 * SUMMARY:
 *   The constitutional text is read to establish courts as the final,
 *   unchallengeable arbiters of all constitutional questions, with authority
 *   to invalidate legislative and executive acts and no political remedy
 *   against a ruling. Under this arrangement the bench defines the reach of
 *   its own review, strikes enacted policy after the fact, and accumulates
 *   interpretive authority that no other branch can revoke by ordinary means;
 *   legislatures comply, amend at supermajority cost, or wait generations for
 *   the bench's composition to turn. KEY AGENTS (by structural relationship):
 *   constitutional_judiciary — agenda-setter and primary beneficiary
 *   (institutional/identity_locked), administers the arrangement and collects
 *   interpretive authority; national_legislature — primary target
 *   (powerful/trapped), bears the veto over enacted policy with no override
 *   remedy; executive_branch — secondary target (powerful/constrained),
 *   exposed to invalidation but holding appointment and enforcement levers;
 *   electoral_majorities — diffuse target (moderate/trapped), enacted
 *   programs fall after the fact; state_governments — dual-positioned
 *   (organized/constrained), pay review costs yet invoke the same review
 *   against national overreach; constitutional_law_professoriate and
 *   policy_litigation_groups — secondary beneficiaries
 *   (organized/identity_locked, organized/mobile), the demand side of
 *   centralized interpretation; comparative_constitutional_observers —
 *   analytical observer, sees the full structure. The ε referent throughout
 *   is the standing judicial-supremacy arrangement itself as this reading
 *   assesses it — never any alternative arrangement the reading might
 *   endorse.
 *
 * KEY AGENTS:
 *   - constitutional_judiciary: agenda-setter and primary beneficiary (institutional/identity_locked) — administers the arrangement, defines its own jurisdiction, collects interpretive authority
 *   - national_legislature: primary target (powerful/trapped) — enacts policy subject to unappealable invalidation; no override channel exists
 *   - executive_branch: secondary target (powerful/constrained) — executive acts subject to invalidation; partial recapture via nominations and enforcement discretion
 *   - electoral_majorities: diffuse target (moderate/trapped) — election winners whose enacted programs fall to later rulings
 *   - state_governments: dual-positioned actor (organized/constrained) — bears review costs and draws on the same review against national preemption
 *   - constitutional_law_professoriate: secondary beneficiary (organized/identity_locked) — careers priced off the doctrinal output of centralized interpretation
 *   - policy_litigation_groups: secondary beneficiary (organized/mobile) — converts minority positions into binding outcomes through the adjudicative channel
 *   - comparative_constitutional_observers: analytical observer — documents jurisdiction expansion and the founding-to-present gap
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(constitutional_authority_boundary__judicial_supremacy_reading, 0.66).
domain_priors:suppression_score(constitutional_authority_boundary__judicial_supremacy_reading, 0.62).
domain_priors:theater_ratio(constitutional_authority_boundary__judicial_supremacy_reading, 0.26).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(constitutional_authority_boundary__judicial_supremacy_reading, extractiveness, 0.66).
narrative_ontology:constraint_metric(constitutional_authority_boundary__judicial_supremacy_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(constitutional_authority_boundary__judicial_supremacy_reading, theater_ratio, 0.26).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(constitutional_authority_boundary__judicial_supremacy_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(constitutional_authority_boundary__judicial_supremacy_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(constitutional_authority_boundary__judicial_supremacy_reading, tangled_rope).
narrative_ontology:human_readable(constitutional_authority_boundary__judicial_supremacy_reading, "Judicial Supremacy Reading of the Constitutional Authority Boundary").
narrative_ontology:topic_domain(constitutional_authority_boundary__judicial_supremacy_reading, "constitutional/political/institutional").

domain_priors:requires_active_enforcement(constitutional_authority_boundary__judicial_supremacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(constitutional_authority_boundary__judicial_supremacy_reading, '08d13b15-4469-4532-9da7-26c723800a34').
narrative_ontology:cs_kernel_codification('08d13b15-4469-4532-9da7-26c723800a34', fixed_text).
narrative_ontology:cs_authority_grounding('08d13b15-4469-4532-9da7-26c723800a34', lineage).
narrative_ontology:cs_interpretation_layer_present('08d13b15-4469-4532-9da7-26c723800a34').
narrative_ontology:cs_reading_relation('08d13b15-4469-4532-9da7-26c723800a34', constitutional_authority_boundary__coordinate_construction_reading, forecloses).
narrative_ontology:cs_reading_relation('08d13b15-4469-4532-9da7-26c723800a34', constitutional_authority_boundary__parliamentary_primacy_reading, forecloses).
narrative_ontology:cs_axiom('08d13b15-4469-4532-9da7-26c723800a34', foundational, courts_hold_exclusive_final_constitutional_authority).
narrative_ontology:cs_axiom_status(courts_hold_exclusive_final_constitutional_authority, holdable).
narrative_ontology:cs_axiom_grounding('08d13b15-4469-4532-9da7-26c723800a34', courts_hold_exclusive_final_constitutional_authority, conventional).
narrative_ontology:cs_axiom('08d13b15-4469-4532-9da7-26c723800a34', foundational, invalidated_acts_require_no_political_remedy).
narrative_ontology:cs_axiom_status(invalidated_acts_require_no_political_remedy, holdable).
narrative_ontology:cs_axiom_grounding('08d13b15-4469-4532-9da7-26c723800a34', invalidated_acts_require_no_political_remedy, instrumental).
narrative_ontology:cs_reference_frame('08d13b15-4469-4532-9da7-26c723800a34', founding_text_final_arbiter_designation).
narrative_ontology:cs_drift_state('08d13b15-4469-4532-9da7-26c723800a34', contemporary_doctrinal_expansion_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('08d13b15-4469-4532-9da7-26c723800a34', '').
narrative_ontology:cs_kernel_id(constitutional_authority_boundary__judicial_supremacy_reading, constitutional_authority_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(constitutional_authority_boundary__judicial_supremacy_reading, constitutional_judiciary).
narrative_ontology:constraint_beneficiary(constitutional_authority_boundary__judicial_supremacy_reading, constitutional_law_professoriate).
narrative_ontology:constraint_beneficiary(constitutional_authority_boundary__judicial_supremacy_reading, policy_litigation_groups).
narrative_ontology:constraint_victim(constitutional_authority_boundary__judicial_supremacy_reading, national_legislature).
narrative_ontology:constraint_victim(constitutional_authority_boundary__judicial_supremacy_reading, executive_branch).
narrative_ontology:constraint_victim(constitutional_authority_boundary__judicial_supremacy_reading, electoral_majorities).
narrative_ontology:constraint_victim(constitutional_authority_boundary__judicial_supremacy_reading, state_governments).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(constitutional_authority_boundary__judicial_supremacy_reading, state_governments).
narrative_ontology:constraint_vindicates(constitutional_authority_boundary__judicial_supremacy_reading, judicial_review_doctrine).
narrative_ontology:constraint_vindicates(constitutional_authority_boundary__judicial_supremacy_reading, constitutional_finality_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Decides which constitutional questions reach adjudication, defines the reach of its own review through doctrine, and issues rulings that bind the other branches. Collects interpretive authority, prestige, and doctrinal capital; its members hold life terms insulated from the electoral cycle. Leaving the arbiter role would mean dismantling the institution's self-understanding and its members' professional identities — the role is not a job the institution holds but what the institution is.
narrative_ontology:constraint_stakeholder(constitutional_authority_boundary__judicial_supremacy_reading, constitutional_judiciary, agenda_setter,
    institutional, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(constitutional_authority_boundary__judicial_supremacy_reading, constitutional_judiciary, beneficiary).

% Drafts and enacts statutes that take effect subject to later invalidation by the bench. Once a ruling strikes a law, ordinary legislation cannot restore it; the available responses are constitutional amendment requiring supermajorities across many units, waiting decades for appointments to shift the bench's composition, or drafting around the holding. Its members serve short electoral terms while the rulings outlast them.
narrative_ontology:constraint_stakeholder(constitutional_authority_boundary__judicial_supremacy_reading, national_legislature, payer,
    powerful, biographical, trapped, national).

% Issues orders and administers agencies whose actions can be struck down by ruling. Holds partial counters: it nominates judges, executes or slows judgments, and controls the government's litigation posture — levers that soften but do not remove its exposure. A hostile bench can nullify its program and a friendly one can shield it, which makes the appointment channel a prize contested every political cycle.
narrative_ontology:constraint_stakeholder(constitutional_authority_boundary__judicial_supremacy_reading, executive_branch, payer,
    powerful, biographical, constrained, national).

% Win elections, enact platforms, and watch portions of the enacted program fall to rulings issued years later by officials they never voted on and cannot remove. Their recourse runs through generational channels — amendment, long-horizon appointment politics — that rarely align with the biographical window in which any particular majority exists.
narrative_ontology:constraint_stakeholder(constitutional_authority_boundary__judicial_supremacy_reading, electoral_majorities, payer,
    moderate, biographical, trapped, national).

% Litigate constantly against national statutes and administrative rules that preempt their powers, and are themselves subject to review when their own acts conflict with national law. They lose some rounds and win others, and routinely petition the very authority that constrains them when national overreach threatens — bearing the arrangement's costs and drawing on it in the same decade.
narrative_ontology:constraint_stakeholder(constitutional_authority_boundary__judicial_supremacy_reading, state_governments, payer,
    organized, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(constitutional_authority_boundary__judicial_supremacy_reading, state_governments, beneficiary).

% Teaches, writes, and advises within a field whose subject matter is the bench's doctrinal output. Journals, curricula, clerkship pipelines, and consulting markets all price the product of centralized interpretation. Their professional standing depends on adjudication remaining central to constitutional meaning; a shift of final authority elsewhere would strand a large share of the profession's accumulated capital.
narrative_ontology:constraint_stakeholder(constitutional_authority_boundary__judicial_supremacy_reading, constitutional_law_professoriate, beneficiary,
    organized, generational, identity_locked, national).

% Pursue policy goals through constitutional litigation when the legislative route is blocked, converting minority positions into nationwide outcomes a ballot-box defeat could never produce. For them the adjudicative channel is not a cost but an asset — continued access to it is what they organize to protect, and they can shift forums or strategies with relative ease.
narrative_ontology:constraint_stakeholder(constitutional_authority_boundary__judicial_supremacy_reading, policy_litigation_groups, beneficiary,
    organized, biographical, mobile, national).

% Study the arrangement from outside its enforcement: comparing finality mechanisms across polities, tracking the expansion of adjudicative jurisdiction, and documenting the distance between the founding-era designation and contemporary practice. They collect nothing from the arrangement and bear none of its costs.
narrative_ontology:constraint_stakeholder(constitutional_authority_boundary__judicial_supremacy_reading, comparative_constitutional_observers, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(constitutional_authority_boundary__judicial_supremacy_reading, constitutional_judiciary).
narrative_ontology:fixing_cost_class(constitutional_authority_boundary__judicial_supremacy_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single terminal resolution for constitutional disputes among branches and between levels of government: once the court speaks, the question is closed, so governance does not stall on permanently contested norms and lower-level actors have a determinate hierarchy of law to plan against.
% TRANSFER_FUNCTION: Moves interpretive authority and effective veto power over enacted policy from legislatures, executives, and electoral majorities to the judiciary; concretely, transfers the fate of statutes and administrative acts from the bodies that enacted them to a bench of unelected adjudicators, along with the prestige, jurisdiction, and doctrinal capital that accumulate to whoever holds final say.
% ABSENT_VOICES: Elected legislators whose statutes were invalidated, and the electoral coalitions behind them, have no formal seat in the adjudicative process that ends their policy; their objections surface only through appointment battles, amendment campaigns, or open non-compliance, none of which is a recognized channel inside the arrangement. Future majorities bound by durable rulings are absent by temporal position — they cannot yet object to constraints imposed before they could organize.
% DISAPPEARANCE_RATIONALE: Every invalidated statute's status would reopen; inter-branch and federal-state constitutional disputes would lose their terminal resolution mechanism and reorganize around whichever branch or coalition could muster enforcement; the precedent stock that disciplines lower courts and anchors planning would decay; and appointment politics and amendment campaigns would shift from marginal to central channels of constitutional change.
% FOUNDING_PROBLEM: A polity emerging from confederation faced recurring inter-branch standoffs and federal-state conflicts over the reach of supreme law, with no agreed umpire: disputes over constitutional meaning threatened either paralysis or force, and conflicting interpretations across jurisdictions left the hierarchy of norms indeterminate.
% FOUNDING_PROBLEM_CORROBORATION: Ratification-era records from outside the future beneficiary set corroborate both halves of the genealogy: state convention debates and contemporary pamphlets — including opposition writers who warned specifically of judicial aggrandizement — attest the real coordination problem and the fear that the proposed cure concentrated interpretive power. Modern political-science and comparative-constitution scholarship, likewise outside the beneficiary set, attests that the original deadlock problem persists in attenuated form while the arrangement's scope has expanded well past its founding footprint, supporting 'contested' over 'live' or 'dead'.
narrative_ontology:disappearance_verdict(constitutional_authority_boundary__judicial_supremacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(constitutional_authority_boundary__judicial_supremacy_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(constitutional_authority_boundary__judicial_supremacy_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(constitutional_authority_boundary__judicial_supremacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(constitutional_authority_boundary__judicial_supremacy_reading, 0.66, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(constitutional_authority_boundary__judicial_supremacy_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(constitutional_authority_boundary__judicial_supremacy_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(constitutional_authority_boundary__judicial_supremacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.66 at interval end) because the veto is decoupled from accountability: the bench strikes enacted policy, defines the boundary of its own power, and faces no ordinary-law remedy, so authority and rents concentrate in the arbiter seat. Suppression (0.62) is predominantly structural — the absence of any override channel and the supermajority amendment barrier close the legislature's alternatives — with a smaller internalized component in the legal-cultural treatment of judicial finality as simply what constitutionalism is. Theater ratio (0.26) reflects a real adjudicative function that dominates output, alongside a growing share of opinion-writing devoted to defending the review power itself rather than resolving the case before the court. Accessibility collapse (0.58) is substantial but incomplete: once the arrangement is understood, the legislature's alternatives compress to comply-amend-wait, yet the amendment channel keeps full collapse from occurring. Resistance (0.55) is persistent and real — recurring court-curbing and jurisdiction-stripping proposals, defiance episodes, term-limit and accountability movements — because the arrangement must be actively maintained against the branches it binds. The three measurement series share one time grid (points 0-30 at step 5) so every metric is authored at every examined point; end-state values equal the scalar base_properties. The suppression_requirement series is authored deliberately: the story tracks enforcement-capacity change, from an early regime that had to assert itself against defiance episodes to a mature regime whose compliance infrastructure runs automatically — a rising trajectory modeling enforcement hardening, not merely shifting extraction. The rising base_extractiveness series models rent layered onto coordination as jurisdiction expands; it will trip the extraction-accumulation abductive trigger, which is appropriate — the hypothesis deserves investigation, not suppression.
 *
 * PERSPECTIVAL GAP:
 *   From the bench's seat the arrangement is the constitution working as designed: finality, neutrality, the rule of law — the coordination function is vivid and the costs are invisible from inside the arbiter role, so the computed classification from that seat sits near the coordination pole. From the legislature's seat the identical structure operates as an unaccountable veto with no remedy: the same ruling that closes a dispute for the court forecloses a policy for the elected branch, so the computed classification from that seat sits near the extraction pole. Electoral majorities experience the arrangement as temporal dispossession — policy enacted inside a biographical window is undone on a generational clock. The engine computes this per-seat divergence from the structural data; the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   The beneficiary/victim declarations map cleanly onto the structural relationships, and no directionality overrides are needed: the derivation chain differentiates the two powerful payers by exit options (the trapped legislature derives nearer the full-target end than the constrained executive, whose nomination and enforcement levers partially recapture interpretive influence), and the state governments' mixed position is carried by their secondary beneficiary role, pulling their derived directionality toward the middle rather than the target pole. The judiciary derives nearest the beneficiary end — beneficiary declaration, agenda-setting power, identity-locked exit. The professoriate and litigation groups derive low as declared beneficiaries, the latter moderated by its mobile exit. Electoral majorities derive near the full-target end: victim declaration, trapped exit, and no recognized channel of redress.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — inter-branch and federal-state deadlock with no agreed umpire — is contested rather than dead, so this is not a classic mandate-outlived-function case; the arrangement still performs its coordinating work. What has drifted is scope: the arrangement's operation now extends far past its founding footprint, and the drift is unacknowledged by the authority structure, which presents expansion as faithful application. Classifying this as tangled_rope rather than rope prevents laundering the no-remedy veto as mere coordination cost; classifying it as snare would erase the genuine finality function that even the bound branches rely on for planning and that the comparative record shows alternatives struggle to replace. The receipt surface sharpens the picture: gains accrue to a named seat (the bench), and fixing is prohibitive for any actor that could attempt it, which rules out the piton reading despite the growing theatrical component — the administrator here profits from maintenance, so inertia is not what holds the arrangement up.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    textual_authority_location,
    'Does the constitutional text itself designate courts as final arbiters of constitutional questions, or does it leave interpretive authority distributed among the branches?',
    'Close textual analysis of the constitutional provisions together with drafting-convention and ratification records establishing what the adopting polity understood itself to enact.',
    'If the text distributes authority, this reading''s arrangement is a post-ratification construction whose concentration of interpretive power reflects institutional self-aggrandizement rather than design, and the judiciary''s beneficiary position becomes the central structural fact; if the text designates finality, part of the measured concentration is the price of the designed coordination itself.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(textual_authority_location, conceptual, 'Where the ratified text locates final interpretive authority — the load-bearing ambiguity beneath the reading.').

omega_variable(
    kernel_membership_and_sibling_delta,
    'This constraint is the judicial_supremacy_reading of kernel constitutional_authority_boundary; what would a sibling reading change structurally — one distributing final interpretive authority among co-equal branches, or one locating it in the elected legislature?',
    'Authoring the sibling stories as separate constraints and comparing computed per-seat classifications, epsilon profiles, and beneficiary/victim structures across the constraint family.',
    'Under a distributed-authority sibling, the judiciary exits the beneficiary set, the legislature''s constrained-policy-space position dissolves into a shared interpretive burden, and the no-remedy transfer channel disappears; under a legislative-finality sibling, the bench becomes advisory and the entire veto-based transfer reverses direction. The disagreement between readings is located entirely in the final-authority-location element.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_membership_and_sibling_delta, conceptual, 'Committer structure: kernel membership, reading identity, and the structural delta each sibling would produce.').

omega_variable(
    override_mechanism_intrinsicness,
    'Is the absence of a legislative override remedy intrinsic to the arrangement''s design, or an omission that ordinary institutional development could supply without breaking the coordination function?',
    'Comparative constitutional record: jurisdictions that adopted override or notwithstanding mechanisms while retaining judicial review, tracked for finality performance and inter-branch conflict rates.',
    'If an override is institutionally compatible with finality, the no-remedy feature is separable and the arrangement trends toward the extraction pole; if incompatible, part of the measured concentration is the unavoidable price of the coordination the arrangement performs.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(override_mechanism_intrinsicness, empirical, 'Whether the no-remedy feature is design or omission.').

omega_variable(
    finality_necessity_for_coordination,
    'Is a single final arbiter structurally necessary to produce constitutional finality, or can finality emerge from plural settlement mechanisms such as political-branch accommodation and iterative amendment?',
    'Institutional analysis of dispute-resolution performance in polities without judicial supremacy: frequency and duration of unresolved inter-branch constitutional conflict, and the stability of planning expectations for lower-level actors.',
    'If a single arbiter is necessary, the coordination function is genuine and constrains classification away from the pure-extraction pole; if dispensable, the coordination story weakens toward cover and the extraction reading strengthens.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(finality_necessity_for_coordination, conceptual, 'Whether the coordination function requires the monopolized form this reading instantiates.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(constitutional_authority_boundary__judicial_supremacy_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cons_tr_t0, constitutional_authority_boundary__judicial_supremacy_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(cons_tr_t5, constitutional_authority_boundary__judicial_supremacy_reading, theater_ratio, 5, 0.15).
narrative_ontology:measurement(cons_tr_t10, constitutional_authority_boundary__judicial_supremacy_reading, theater_ratio, 10, 0.18).
narrative_ontology:measurement(cons_tr_t15, constitutional_authority_boundary__judicial_supremacy_reading, theater_ratio, 15, 0.2).
narrative_ontology:measurement(cons_tr_t20, constitutional_authority_boundary__judicial_supremacy_reading, theater_ratio, 20, 0.22).
narrative_ontology:measurement(cons_tr_t25, constitutional_authority_boundary__judicial_supremacy_reading, theater_ratio, 25, 0.24).
narrative_ontology:measurement(cons_tr_t30, constitutional_authority_boundary__judicial_supremacy_reading, theater_ratio, 30, 0.26).

% Extraction over time
narrative_ontology:measurement(cons_be_t0, constitutional_authority_boundary__judicial_supremacy_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(cons_be_t5, constitutional_authority_boundary__judicial_supremacy_reading, base_extractiveness, 5, 0.5).
narrative_ontology:measurement(cons_be_t10, constitutional_authority_boundary__judicial_supremacy_reading, base_extractiveness, 10, 0.55).
narrative_ontology:measurement(cons_be_t15, constitutional_authority_boundary__judicial_supremacy_reading, base_extractiveness, 15, 0.59).
narrative_ontology:measurement(cons_be_t20, constitutional_authority_boundary__judicial_supremacy_reading, base_extractiveness, 20, 0.63).
narrative_ontology:measurement(cons_be_t25, constitutional_authority_boundary__judicial_supremacy_reading, base_extractiveness, 25, 0.65).
narrative_ontology:measurement(cons_be_t30, constitutional_authority_boundary__judicial_supremacy_reading, base_extractiveness, 30, 0.66).

% Suppression requirement over time
narrative_ontology:measurement(cons_su_t0, constitutional_authority_boundary__judicial_supremacy_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(cons_su_t5, constitutional_authority_boundary__judicial_supremacy_reading, suppression_requirement, 5, 0.46).
narrative_ontology:measurement(cons_su_t10, constitutional_authority_boundary__judicial_supremacy_reading, suppression_requirement, 10, 0.52).
narrative_ontology:measurement(cons_su_t15, constitutional_authority_boundary__judicial_supremacy_reading, suppression_requirement, 15, 0.56).
narrative_ontology:measurement(cons_su_t20, constitutional_authority_boundary__judicial_supremacy_reading, suppression_requirement, 20, 0.59).
narrative_ontology:measurement(cons_su_t25, constitutional_authority_boundary__judicial_supremacy_reading, suppression_requirement, 25, 0.61).
narrative_ontology:measurement(cons_su_t30, constitutional_authority_boundary__judicial_supremacy_reading, suppression_requirement, 30, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(constitutional_authority_boundary__judicial_supremacy_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(constitutional_authority_boundary__judicial_supremacy_reading, constitutional_authority_boundary__coordinate_construction_reading).
narrative_ontology:affects_constraint(constitutional_authority_boundary__judicial_supremacy_reading, constitutional_authority_boundary__parliamentary_primacy_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'who decides what the constitution means' decomposes into three structurally distinct constraints — the judicial supremacy reading (this file), the coordinate construction reading, and the parliamentary primacy reading — each with its own epsilon, beneficiary/victim structure, and classification, linked per the family rule. The readings are not one constraint viewed from angles: their epsilon values differ because the location of final interpretive authority changes who pays and who collects. This reading sits downstream in influence: accumulated precedent and entrenched doctrine raise the operating cost of the coordinate reading (every year of practice is evidence the distributed frame cannot recover), while the veto's visible counter-majoritarian outputs raise the political salience of legislative-finality proposals. Each family member links to the others; orphaning any one would sever the contamination-propagation analysis that tracks how doctrine accumulation in this reading degrades the viability of its siblings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
