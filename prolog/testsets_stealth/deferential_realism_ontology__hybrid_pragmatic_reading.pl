% ============================================================================
% CONSTRAINT STORY: deferential_realism_ontology__hybrid_pragmatic_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_deferential_realism_ontology__hybrid_pragmatic_reading, []).

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
 *   constraint_id: deferential_realism_ontology__hybrid_pragmatic_reading
 *   human_readable: Hybrid Pragmatic Reading of the Constraint Typology
 *   domain: epistemological/normative/institutional
 *
 * SUMMARY:
 *   The constraint under authorship is the standing arrangement of the
 *   constraint typology as practiced under the hybrid pragmatic reading: a
 *   two-register classification regime whose observational register handles
 *   physically and coordination-grounded core cases (stable across
 *   interpretive communities, cheap to apply, little contested) and whose
 *   constructed register handles peripheral cases where the verdict turns on
 *   judgments about which beneficiaries are legitimate. The register boundary
 *   is not self-maintaining — it is held by active adjudication machinery
 *   (review standards, editorial norms, corpus validation rules) that
 *   intensified as the peripheral caseload grew. The claim/metric gap is
 *   deliberate: the arrangement is CLAIMED as tangled_rope because it pairs a
 *   genuine, heavily used coordination function with asymmetric, enforced
 *   cost-bearing at the periphery; the metrics are authored as descriptively
 *   true of that operation, and the engine computes per-seat classifications
 *   from the structural data without reconciling them to the claim. KEY
 *   AGENTS (by structural relationship): - typology_framework_maintainers:
 *   Agenda-setting seat (institutional/identity_locked) — administers
 *   standards, adjudicates the register boundary, accrues adjudication
 *   authority - constraint_typology_practitioners: Beneficiary seat
 *   (organized/constrained) — collects comparability and audience from the
 *   shared vocabulary - institutional_design_reformers: Beneficiary seat with
 *   exposure (organized/mobile) — converts favorable peripheral verdicts into
 *   reform momentum - policy_makers: Dual-positioned seat
 *   (institutional/mobile) — consumes classifications, owns the risk of
 *   reversed verdicts - classified_arrangement_operators: Primary target seat
 *   (powerful/constrained) — bears the costs of verdicts framed by others'
 *   legitimacy criteria - heterodox_analysts: Secondary target seat
 *   (moderate/constrained) — pays career costs for divergent peripheral
 *   verdicts - rival_reading_advocates: Excluded seat (organized/trapped) —
 *   holds the two rival accounts, outside the adjudication loop their
 *   objections address - epistemic_norm_observers: Analytical seat — assesses
 *   whether verdicts track their subject matter
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(deferential_realism_ontology__hybrid_pragmatic_reading, 0.52).
domain_priors:suppression_score(deferential_realism_ontology__hybrid_pragmatic_reading, 0.5).
domain_priors:theater_ratio(deferential_realism_ontology__hybrid_pragmatic_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(deferential_realism_ontology__hybrid_pragmatic_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(deferential_realism_ontology__hybrid_pragmatic_reading, suppression_requirement, 0.5).
narrative_ontology:constraint_metric(deferential_realism_ontology__hybrid_pragmatic_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(deferential_realism_ontology__hybrid_pragmatic_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(deferential_realism_ontology__hybrid_pragmatic_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(deferential_realism_ontology__hybrid_pragmatic_reading, tangled_rope).
narrative_ontology:human_readable(deferential_realism_ontology__hybrid_pragmatic_reading, "Hybrid Pragmatic Reading of the Constraint Typology").
narrative_ontology:topic_domain(deferential_realism_ontology__hybrid_pragmatic_reading, "epistemological/normative/institutional").

domain_priors:requires_active_enforcement(deferential_realism_ontology__hybrid_pragmatic_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(deferential_realism_ontology__hybrid_pragmatic_reading, '0a0ac60a-cadd-455f-9ad6-70664af34c13').
narrative_ontology:cs_kernel_codification('0a0ac60a-cadd-455f-9ad6-70664af34c13', formalized).
narrative_ontology:cs_authority_grounding('0a0ac60a-cadd-455f-9ad6-70664af34c13', expertise).
narrative_ontology:cs_interpretation_layer_present('0a0ac60a-cadd-455f-9ad6-70664af34c13').
narrative_ontology:cs_reading_relation('0a0ac60a-cadd-455f-9ad6-70664af34c13', deferential_realism_ontology__immutable_diagnostic_reading, coexists_with).
narrative_ontology:cs_reading_relation('0a0ac60a-cadd-455f-9ad6-70664af34c13', deferential_realism_ontology__rhetorical_scaffold_reading, coexists_with).
narrative_ontology:cs_axiom('0a0ac60a-cadd-455f-9ad6-70664af34c13', foundational, classification_confidence_tracks_grounding_type).
narrative_ontology:cs_axiom_status(classification_confidence_tracks_grounding_type, holdable).
narrative_ontology:cs_axiom_grounding('0a0ac60a-cadd-455f-9ad6-70664af34c13', classification_confidence_tracks_grounding_type, instrumental).
narrative_ontology:cs_axiom('0a0ac60a-cadd-455f-9ad6-70664af34c13', foundational, peripheral_verdicts_require_legitimacy_deliberation).
narrative_ontology:cs_axiom_status(peripheral_verdicts_require_legitimacy_deliberation, holdable).
narrative_ontology:cs_axiom_grounding('0a0ac60a-cadd-455f-9ad6-70664af34c13', peripheral_verdicts_require_legitimacy_deliberation, conventional).
narrative_ontology:cs_reference_frame('0a0ac60a-cadd-455f-9ad6-70664af34c13', fixed_core_constructed_periphery_settlement).
narrative_ontology:cs_drift_state('0a0ac60a-cadd-455f-9ad6-70664af34c13', contemporary_meta_analysis_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('0a0ac60a-cadd-455f-9ad6-70664af34c13', '').
narrative_ontology:cs_kernel_id(deferential_realism_ontology__hybrid_pragmatic_reading, deferential_realism_ontology).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(deferential_realism_ontology__hybrid_pragmatic_reading, typology_framework_maintainers).
narrative_ontology:constraint_beneficiary(deferential_realism_ontology__hybrid_pragmatic_reading, constraint_typology_practitioners).
narrative_ontology:constraint_beneficiary(deferential_realism_ontology__hybrid_pragmatic_reading, institutional_design_reformers).
narrative_ontology:constraint_beneficiary(deferential_realism_ontology__hybrid_pragmatic_reading, policy_makers).
narrative_ontology:constraint_victim(deferential_realism_ontology__hybrid_pragmatic_reading, classified_arrangement_operators).
narrative_ontology:constraint_victim(deferential_realism_ontology__hybrid_pragmatic_reading, heterodox_analysts).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(deferential_realism_ontology__hybrid_pragmatic_reading, policy_makers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Set and administer the classification standards: they decide what counts as a legitimate beneficiary declaration, adjudicate borderline cases between the observational and constructed registers, and maintain the corpus machinery that validates authored stories. Each contested peripheral case they settle adds to their adjudication authority. Their careers, institutional positions, and scholarly identities are fused with the framework's continuation; leaving would mean abandoning the body of work that constitutes their standing.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__hybrid_pragmatic_reading, typology_framework_maintainers, agenda_setter,
    institutional, generational, identity_locked, global).

% Apply the two-register scheme in research and policy analysis: they classify arrangements, publish verdicts, and trade on a shared vocabulary that makes their work legible across domains. The scheme gives them comparability and an audience. Switching to a rival vocabulary would cost retraining, severed citation networks, and loss of the publication venues organized around the current scheme.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__hybrid_pragmatic_reading, constraint_typology_practitioners, beneficiary,
    organized, biographical, constrained, global).

% Use peripheral verdicts to press for policy change: when a mechanism's beneficiaries are judged illegitimate by their lights, the constructed register lets them convert that judgment into a formal classification with reform momentum behind it. They are exposed in turn — when a rival interpretive community flips a peripheral verdict, their prior declarations become liabilities. Unlike the practitioners, they can migrate to other critical vocabularies (rights talk, cost-benefit analysis) at moderate cost.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__hybrid_pragmatic_reading, institutional_design_reformers, beneficiary,
    organized, biographical, mobile, national).

% Consume ready-made classifications to justify regulatory and budgetary decisions without commissioning bespoke analysis. They also absorb the downside: acting on a contested peripheral verdict that a rival community later overturns leaves them owning a reversed decision. They can commission any analysis they like, so their dependence is discretionary rather than structural.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__hybrid_pragmatic_reading, policy_makers, beneficiary,
    institutional, immediate, mobile, national).
narrative_ontology:stakeholder_secondary_role(deferential_realism_ontology__hybrid_pragmatic_reading, policy_makers, payer).

% Run the mechanisms that sit in the contested periphery — payment systems, labor arrangements, platform rules, institutional hierarchies — and bear the consequences of verdicts produced by other people's normative frames: reform pressure, reputational damage, litigation exposure, and mandated restructuring. They cannot exit being classified; their recourse is contesting the verdict inside a process whose legitimacy criteria they did not author.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__hybrid_pragmatic_reading, classified_arrangement_operators, payer,
    powerful, biographical, constrained, global).

% Produce peripheral verdicts that diverge from their interpretive community's prevailing normative frame. Divergence carries career cost: reviewers discount the work, citation networks thin, and grant panels route funding elsewhere. Adjacent fields exist but moving means rebuilding professional standing from partial credit.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__hybrid_pragmatic_reading, heterodox_analysts, payer,
    moderate, biographical, constrained, global).

% Champion the two rival accounts of the typology — the purely observational instrument and the openly declaratory critical vocabulary. They stand outside the hybrid community's internal adjudication loops: their objections are recorded but do not shape the standards being objected to. Leaving the debate entirely would abandon their scholarly projects, so they remain inside a conversation structured by the account they reject.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__hybrid_pragmatic_reading, rival_reading_advocates, excluded,
    organized, generational, trapped, global).

% Philosophers of science and methodologists who assess whether the scheme's verdicts track their subject matter, whether the core/periphery boundary is principled, and whether the constructed register is disciplined or decorative. They collect no benefits and bear no burdens from particular verdicts; their output is assessment of the scheme itself.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__hybrid_pragmatic_reading, epistemic_norm_observers, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(deferential_realism_ontology__hybrid_pragmatic_reading, typology_framework_maintainers).
narrative_ontology:fixing_cost_class(deferential_realism_ontology__hybrid_pragmatic_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared, comparable classification vocabulary for institutional arrangements across domains, so that researchers, reformers, and officials can communicate about how arrangements work without renegotiating terms case by case. The physically and coordination-grounded core anchors discourse in cases all interpretive communities read the same way; the contested periphery hosts explicit, rule-governed negotiation where normative frames differ.
% TRANSFER_FUNCTION: Moves epistemic authority — and the policy attention and reputational standing that follow it — from the operators of classified arrangements to the interpretive communities producing the verdicts. Each contested peripheral verdict transfers standing from the mechanism's operators to whichever community's legitimacy frame prevailed, with the maintainer seat accumulating adjudication authority across settlements.
% ABSENT_VOICES: Rival-reading advocates are structurally outside the hybrid community's standard-setting: the observational-instrument partisans would demand that every verdict be observationally correctable, and the declaratory-vocabulary partisans would deny that any discipline governs peripheral construction. Classified arrangement operators participate only as objects of classification — they are never co-authors of the legitimacy criteria that classify them.
% DISAPPEARANCE_RATIONALE: If the hybrid scheme vanished overnight, constraint discourse would fragment into non-communicating camps — purely observational taxonomies unable to handle normatively loaded cases, and openly rhetorical critique unable to sustain cross-domain comparison. Ongoing peripheral contests would lose their venue, in-flight classifications would lose their warrant, and the accumulated corpus of comparable verdicts would become orphaned.
% FOUNDING_PROBLEM: Early constraint analysis had no shared typology: physical limits, coordination devices, and mechanisms that hide extraction behind coordination cover stories were discussed in incompatible vocabularies, so cross-domain learning was impossible and arrangements that coordinated one group while burdening another passed unexamined.
% FOUNDING_PROBLEM_CORROBORATION: Corroboration from outside the benefiting parties: the philosophy-of-science literature on the normative loading of classificatory schemes documents the problem the two-register design addresses; policy-evaluation studies repeatedly catch arrangements that present burden-shifting as service provision; and heterodox analysts — who bear costs under the current settlement — attest the founding problem is live while disputing the hybrid community's handling of it. The maintainer seat's own attestation of liveness is in-set and is not counted as corroboration.
narrative_ontology:disappearance_verdict(deferential_realism_ontology__hybrid_pragmatic_reading, world_rearranges).
narrative_ontology:founding_problem_status(deferential_realism_ontology__hybrid_pragmatic_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(deferential_realism_ontology__hybrid_pragmatic_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(deferential_realism_ontology__hybrid_pragmatic_reading, 'none', 1).
narrative_ontology:epsilon_provenance(deferential_realism_ontology__hybrid_pragmatic_reading, 0.52, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(deferential_realism_ontology__hybrid_pragmatic_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(deferential_realism_ontology__hybrid_pragmatic_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(deferential_realism_ontology__hybrid_pragmatic_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness 0.52 is a blend: the observational register imposes near-zero burden on anyone (classifying a physical limit costs its subjects nothing), while the constructed register transfers real standing — operators of peripherally classified arrangements absorb reform pressure and reputational cost from verdicts whose warrant is partly other communities' values. Suppression 0.50 is authored as a raw structural property, unscaled: it measures the boundary-policing machinery itself (standards committees, review gatekeeping, validation rules that reject stories breaking register discipline), which is substantial but leaves both rival readings operable elsewhere. Theater_ratio 0.30 reflects a specific performance: peripheral verdicts are routinely reported in the language of measurement even though this reading itself declares the register constructed — the vocabulary of observation is partly costume. Accessibility_collapse 0.30 is low by design: understanding the hybrid scheme does not close off alternatives, because the whole architecture concedes that rival schemes remain rational for other communities. Resistance 0.60 is the highest-authored metric because the periphery's open contestation IS the resistance record — every flipped verdict, disputed admissibility ruling, and rival-reading manifesto is the arrangement being actively pushed against. The temporal series run on one shared seven-point grid (T=0..24, years since the framework's consolidation) with all three metrics authored at every point. The suppression_requirement series is included because the story specifically tracks enforcement-capacity change: adjudication machinery was built out as peripheral caseload grew, hardening from light-touch norms (0.35) to institutionalized gatekeeping (0.50) before plateauing.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute differently, and the structural data explains why. From the maintainer seat the arrangement is a mature instrument whose register discipline protects the periphery from rhetorical capture — low personal burden, high authority accrual, identity fused with the framework's continuation. From the operator seat the same machinery is governance by others' values: verdicts arrive with observational pretensions attached to constructed warrants, and exit from being classified does not exist. From the heterodox analyst seat the arrangement prices dissent: the register discipline that looks like rigor from the center looks like gatekeeping from a diverging verdict. Rival-reading partisans see either residual positivism (insufficient nerve about construction) or insufficient discipline (naive surrender to declaration). The engine derives these divergences from power, exit, and role data; the authored claim does not adjudicate among them.
 *
 * DIRECTIONALITY LOGIC:
 *   Maintainers, practitioners, reformers, and policy makers sit toward the beneficiary pole: the vocabulary subsidizes their work, their advocacy, and their decisions, with policy makers pulled back toward symmetric by their secondary exposure to reversed verdicts. Classified arrangement operators sit nearest the full-target pole — powerful globally but constrained, since no exit exists from being an object of classification, and trapped targets amplify effective burden. Heterodox analysts are targets at lower power: their constraint is career-structural rather than jurisdictional. Rival-reading advocates are excluded rather than coordinated — the register boundary exists substantially to keep their accounts from governing verdicts, so their exclusion is the enforcement object itself. Observers hold the analytical seat and take no flow. No directionality overrides are authored: the beneficiary/victim declarations plus exit options already place every seat correctly, and the one dual-positioned agent (policy_makers) is captured by secondary_role rather than by an override that would misfire across the shared power atom.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — telling coordination apart from burden-shifting, and respecting limits that no one chose — is live: every new domain regenerates it, which is why the scheme cannot be retired. The mandatrophy risk concentrates at the periphery. If peripheral adjudication degenerated into ceremony — verdicts predetermined by community allegiance, with the constructed register as alibi — theater_ratio would climb past 0.5 and the arrangement would drift toward performance maintained by inertia. The current series shows theater rising slowly (0.18 to 0.30 over the interval) and plateauing alongside suppression: watched, not yet degenerate. The hybrid reading's own discipline is its anti-mandatrophy device: by declaring the constructed register constructed, it keeps peripheral verdicts honest about their warrant and blocks the failure mode of the declaratory rival, in which the vocabulary detaches from referents entirely and classification becomes pure persuasion. Classification under this reading therefore prevents two opposite mislabelings: it stops the observational register from being stretched over normative cases (where it would launder constructed verdicts as discoveries), and it stops the constructed register from collapsing into fiat (where it would dissolve the scheme's comparative function).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_underdetermination,
    'Does the hybrid settlement (fixed observational core, constructed deliberated periphery) describe the typology''s actual structure, or is it one of three live readings whose adoption changes the arrangement''s victim set, epsilon, and enforcement requirements?',
    'Track whether peripheral disputes converge observationally over time (drift toward the immutable-diagnostic account) or track community allegiance irrespective of evidence (drift toward the declaratory account); corpus-level meta-analysis of verdict stability across interpretive communities with disjoint stake profiles.',
    'If the immutable reading prevails, peripheral epsilon becomes measurable, the constructed register''s enforcement loses warrant, and the operator seat''s grievance converts into a correctable-error complaint. If the declaratory reading prevails, the entire scheme becomes persuasion infrastructure and the practitioner beneficiary set reconstitutes as a rhetorical coalition.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_underdetermination, conceptual, 'Which reading of the typology kernel the standing arrangement actually instantiates.').

omega_variable(
    legitimacy_judgment_circularity,
    'Can peripheral classification escape circularity — the communities empowered to judge which beneficiaries are legitimate are themselves beneficiaries of the classification regime that empowers them?',
    'Compare peripheral verdicts across interpretive communities with disjoint stake profiles; measure convergence rates on identical cases against stake-alignment predictions.',
    'High circularity raises the effective burden on classified operators (verdicts track classifier interests) and pushes the arrangement toward pure extraction at the periphery; low circularity supports the hybrid claim that construction is disciplined rather than self-serving.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legitimacy_judgment_circularity, empirical, 'Whether the constructed register''s legitimacy judgments are disciplined or self-interested.').

omega_variable(
    core_stability_genuineness,
    'Are core classifications stable across interpretive communities because they are grounded in physical and coordination facts, or because the communities share training lineages that manufacture agreement?',
    'Test core-case agreement in communities with disjoint training histories; adversarial collaborations on borderline core cases where grounding type is itself disputed.',
    'If core stability is a training artifact, the register boundary is softer than claimed, the suppression spent defending it is unjustified, and peripheral-style contestation legitimately extends inward.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(core_stability_genuineness, empirical, 'Whether the observational register''s stability is fact-grounded or conventionally reproduced.').

omega_variable(
    epsilon_measurement_hybrid_validity,
    'Is the hybrid measurement protocol (observational for core types, constructed for peripheral types) a principled epistemic division or a device that shields peripheral verdicts from observational challenge they could survive?',
    'Attempt controlled observational measurement of peripheral burden under agreed proxies; compare inter-community disagreement rates on peripheral cases against core-case baselines.',
    'If peripheral burden proves observably tractable, the immutable reading gains ground, the constructed register''s enforcement loses justification, and the operator seat''s standing improves; if not, the hybrid division is vindicated as load-bearing.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(epsilon_measurement_hybrid_validity, conceptual, 'Whether the two-register measurement protocol marks a real epistemic boundary.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression of rival classifications structural (gatekeeping machinery, review standards, validation rules) or internalized (practitioners having absorbed register discipline as professional virtue)?',
    'Post-exit trajectory analysis: analysts who leave the hybrid community — do they immediately adopt rival readings, or do they retain the two-register discipline after the enforcing machinery no longer reaches them?',
    'If internalized, suppression persists beyond the enforcement infrastructure and rival readings stay suppressed even where gatekeeping lapses, raising the arrangement''s effective suppression above the structural measure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural versus internalized mechanism of register-boundary enforcement.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(deferential_realism_ontology__hybrid_pragmatic_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(defe_tr_t0, deferential_realism_ontology__hybrid_pragmatic_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(defe_tr_t4, deferential_realism_ontology__hybrid_pragmatic_reading, theater_ratio, 4, 0.21).
narrative_ontology:measurement(defe_tr_t8, deferential_realism_ontology__hybrid_pragmatic_reading, theater_ratio, 8, 0.24).
narrative_ontology:measurement(defe_tr_t12, deferential_realism_ontology__hybrid_pragmatic_reading, theater_ratio, 12, 0.26).
narrative_ontology:measurement(defe_tr_t16, deferential_realism_ontology__hybrid_pragmatic_reading, theater_ratio, 16, 0.28).
narrative_ontology:measurement(defe_tr_t20, deferential_realism_ontology__hybrid_pragmatic_reading, theater_ratio, 20, 0.29).
narrative_ontology:measurement(defe_tr_t24, deferential_realism_ontology__hybrid_pragmatic_reading, theater_ratio, 24, 0.3).

% Extraction over time
narrative_ontology:measurement(defe_be_t0, deferential_realism_ontology__hybrid_pragmatic_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(defe_be_t4, deferential_realism_ontology__hybrid_pragmatic_reading, base_extractiveness, 4, 0.42).
narrative_ontology:measurement(defe_be_t8, deferential_realism_ontology__hybrid_pragmatic_reading, base_extractiveness, 8, 0.46).
narrative_ontology:measurement(defe_be_t12, deferential_realism_ontology__hybrid_pragmatic_reading, base_extractiveness, 12, 0.49).
narrative_ontology:measurement(defe_be_t16, deferential_realism_ontology__hybrid_pragmatic_reading, base_extractiveness, 16, 0.51).
narrative_ontology:measurement(defe_be_t20, deferential_realism_ontology__hybrid_pragmatic_reading, base_extractiveness, 20, 0.52).
narrative_ontology:measurement(defe_be_t24, deferential_realism_ontology__hybrid_pragmatic_reading, base_extractiveness, 24, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(defe_su_t0, deferential_realism_ontology__hybrid_pragmatic_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(defe_su_t4, deferential_realism_ontology__hybrid_pragmatic_reading, suppression_requirement, 4, 0.4).
narrative_ontology:measurement(defe_su_t8, deferential_realism_ontology__hybrid_pragmatic_reading, suppression_requirement, 8, 0.44).
narrative_ontology:measurement(defe_su_t12, deferential_realism_ontology__hybrid_pragmatic_reading, suppression_requirement, 12, 0.47).
narrative_ontology:measurement(defe_su_t16, deferential_realism_ontology__hybrid_pragmatic_reading, suppression_requirement, 16, 0.49).
narrative_ontology:measurement(defe_su_t20, deferential_realism_ontology__hybrid_pragmatic_reading, suppression_requirement, 20, 0.5).
narrative_ontology:measurement(defe_su_t24, deferential_realism_ontology__hybrid_pragmatic_reading, suppression_requirement, 24, 0.5).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(deferential_realism_ontology__hybrid_pragmatic_reading, information_standard).
narrative_ontology:affects_constraint(deferential_realism_ontology__hybrid_pragmatic_reading, deferential_realism_ontology__immutable_diagnostic_reading).
narrative_ontology:affects_constraint(deferential_realism_ontology__hybrid_pragmatic_reading, deferential_realism_ontology__rhetorical_scaffold_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition per the epsilon-invariance principle: the colloquial label 'the constraint typology' covers three structurally distinct epistemic arrangements that this kernel decomposition separates. The hybrid pragmatic reading (this file) authors a two-register regime with blended epsilon (0.52), operator and heterodox-analyst victim sets, and active register-boundary enforcement. The immutable_diagnostic_reading authors a uniform observational instrument with negligible extraction at the core and a different failure mode (unacknowledged normative loading). The rhetorical_scaffold_reading authors a declaratory critical vocabulary whose extraction profile runs through persuasive capture rather than adjudication. The upstream hybrid settlement influences both siblings' operating environments — its core successes lend credibility to observational methods while its constructed periphery legitimizes normative verdict-talk — and all three files link one another through affects_constraints so contamination propagation can trace register-boundary failures across the family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
