% ============================================================================
% CONSTRAINT STORY: jurisprudential_method_kernel__hanafi_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jurisprudential_method_kernel__hanafi_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: jurisprudential_method_kernel__hanafi_reading
 *   human_readable: Hanafi Reading: Law via Qiyas and Istihsan as Legitimate Extension of Divine Intent
 *   domain: Islamic Jurisprudence / Legal Philosophy / Institutional History
 *
 * SUMMARY:
 *   This story instantiates the Hanafi reading of the jurisprudential method
 *   kernel: law derives from Qur'an and Hadith but is extended to novel cases
 *   through structured analogical reasoning (qiyas) and juristic preference
 *   (istihsan), on the premise that reason is a legitimate tool for realizing
 *   divine intent beyond the literal text. This is one reading among several
 *   contested readings of the same kernel (the proper method of Islamic legal
 *   derivation) — the Hanbali, Maliki, and Shafi'i readings are separate
 *   constraints with their own ε values, beneficiary structures, and
 *   stakeholder sets, linked here only through network edges, never merged
 *   into this one. The Hanafi reading's structural signature is high
 *   extraction concentrated on novel/unprecedented cases, where the outcome
 *   depends heavily on the presiding jurist's discretionary reasoning rather
 *   than on a fixed, independently checkable text.
 *
 * KEY AGENTS:
 *   - hanafi_trained_jurists: Primary beneficiary and agenda-setter (institutional/arbitrage) — their specialized method is the resource the whole apparatus depends on
 *   - abbasid_court_administrators: Institutional beneficiary — flexible law suited to governing a vast, diverse empire
 *   - urban_merchant_litigants: Beneficiary — genuine coordination gain from a legal system able to answer novel commercial questions
 *   - textualist_hadith_scholars: Primary target — their claim to exclusive interpretive authenticity is structurally displaced
 *   - litigants_in_novel_cases_without_rationalist_advocates: Powerless target — bear the cost of discretionary variance with no recourse to a settled text
 *   - rival_madhhab_jurists: Excluded — competing methodologies structurally marginalized where Hanafi courts hold administrative dominance
 *   - contemporary_legal_historians: Analytical observer
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jurisprudential_method_kernel__hanafi_reading, 0.58).
domain_priors:suppression_score(jurisprudential_method_kernel__hanafi_reading, 0.42).
domain_priors:theater_ratio(jurisprudential_method_kernel__hanafi_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jurisprudential_method_kernel__hanafi_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(jurisprudential_method_kernel__hanafi_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(jurisprudential_method_kernel__hanafi_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jurisprudential_method_kernel__hanafi_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(jurisprudential_method_kernel__hanafi_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jurisprudential_method_kernel__hanafi_reading, tangled_rope).
narrative_ontology:human_readable(jurisprudential_method_kernel__hanafi_reading, "Hanafi Reading: Law via Qiyas and Istihsan as Legitimate Extension of Divine Intent").
narrative_ontology:topic_domain(jurisprudential_method_kernel__hanafi_reading, "Islamic Jurisprudence / Legal Philosophy / Institutional History").

domain_priors:requires_active_enforcement(jurisprudential_method_kernel__hanafi_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jurisprudential_method_kernel__hanafi_reading, 'ae751b46-b938-43be-ae13-0ef46eb716f0').
narrative_ontology:cs_kernel_codification('ae751b46-b938-43be-ae13-0ef46eb716f0', distributed).
narrative_ontology:cs_authority_grounding('ae751b46-b938-43be-ae13-0ef46eb716f0', lineage).
narrative_ontology:cs_interpretation_layer_present('ae751b46-b938-43be-ae13-0ef46eb716f0').
narrative_ontology:cs_reading_relation('ae751b46-b938-43be-ae13-0ef46eb716f0', jurisprudential_method_kernel__maliki_reading, coexists_with).
narrative_ontology:cs_reading_relation('ae751b46-b938-43be-ae13-0ef46eb716f0', jurisprudential_method_kernel__shafii_reading, influences).
narrative_ontology:cs_reading_relation('ae751b46-b938-43be-ae13-0ef46eb716f0', jurisprudential_method_kernel__hanbali_reading, forecloses).
narrative_ontology:cs_axiom('ae751b46-b938-43be-ae13-0ef46eb716f0', foundational, reason_legitimately_extends_divine_intent).
narrative_ontology:cs_axiom_status(reason_legitimately_extends_divine_intent, holdable).
narrative_ontology:cs_axiom_grounding('ae751b46-b938-43be-ae13-0ef46eb716f0', reason_legitimately_extends_divine_intent, conventional).
narrative_ontology:cs_axiom('ae751b46-b938-43be-ae13-0ef46eb716f0', secondary, juristic_preference_may_override_strict_analogy).
narrative_ontology:cs_axiom_status(juristic_preference_may_override_strict_analogy, holdable).
narrative_ontology:cs_axiom_grounding('ae751b46-b938-43be-ae13-0ef46eb716f0', juristic_preference_may_override_strict_analogy, instrumental).
narrative_ontology:cs_reference_frame('ae751b46-b938-43be-ae13-0ef46eb716f0', kufan_rationalist_transmission).
narrative_ontology:cs_drift_state('ae751b46-b938-43be-ae13-0ef46eb716f0', post_ottoman_codification_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('ae751b46-b938-43be-ae13-0ef46eb716f0', '').
narrative_ontology:cs_kernel_id(jurisprudential_method_kernel__hanafi_reading, jurisprudential_method_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jurisprudential_method_kernel__hanafi_reading, hanafi_trained_jurists).
narrative_ontology:constraint_beneficiary(jurisprudential_method_kernel__hanafi_reading, abbasid_court_administrators).
narrative_ontology:constraint_beneficiary(jurisprudential_method_kernel__hanafi_reading, urban_merchant_litigants).
narrative_ontology:constraint_victim(jurisprudential_method_kernel__hanafi_reading, textualist_hadith_scholars).
narrative_ontology:constraint_victim(jurisprudential_method_kernel__hanafi_reading, litigants_in_novel_cases_without_rationalist_advocates).
narrative_ontology:constraint_vindicates(jurisprudential_method_kernel__hanafi_reading, reason_as_legitimate_extension_of_divine_intent).
narrative_ontology:constraint_vindicates(jurisprudential_method_kernel__hanafi_reading, juristic_preference_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Trained extensively in qiyas and istihsan, these jurists administer courts across the Abbasid and later Ottoman domains. Their specialized reasoning skill is the scarce resource the whole system runs on: novel commercial, administrative, and criminal cases route through their analogical judgment rather than through a closed textual corpus. They set the interpretive agenda for what counts as valid extension of revealed sources and train the next generation into the same method, reproducing their own indispensability.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__hanafi_reading, hanafi_trained_jurists, agenda_setter,
    institutional, civilizational, arbitrage, continental).
narrative_ontology:stakeholder_secondary_role(jurisprudential_method_kernel__hanafi_reading, hanafi_trained_jurists, beneficiary).

% Rely on the Hanafi method's flexibility to adjudicate an expanding, ethnically and commercially diverse empire whose problems the founding-era texts never anticipated. Analogical reasoning gives the state a legal apparatus that can absorb new fiscal, land-tenure, and criminal questions without waiting for scriptural unanimity, which is what makes centralized administration over vast territory tractable at all.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__hanafi_reading, abbasid_court_administrators, beneficiary,
    institutional, generational, constrained, continental).

% Bring novel contractual and commercial disputes — partnerships, credit instruments, cross-regional trade arrangements — that have no direct textual precedent. The qiyas/istihsan apparatus lets a qualified jurist reason from analogous cases to a workable ruling, giving merchants a forum that can actually resolve their disputes instead of declaring them unanswerable.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__hanafi_reading, urban_merchant_litigants, beneficiary,
    moderate, biographical, constrained, regional).

% Hold that authentic law must trace directly to transmitted text and consensus, and that discretionary reasoning smuggles human judgment into what should be divine command. Their claim to interpretive authority is structurally displaced wherever a Hanafi court prefers a jurist's analogical ruling over a weaker or absent textual chain. They can preach and write against the method but cannot exit the shared legal-theological field without abandoning the claim to speak on law at all.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__hanafi_reading, textualist_hadith_scholars, payer,
    organized, civilizational, constrained, continental).

% Ordinary parties whose cases fall into interpretive gaps bear the cost when the outcome depends on which jurist's istihsan is applied and how skillfully their side's advocate can construct the analogy. They cannot appeal to a fixed text that would settle the matter independent of the presiding jurist's discretionary judgment, and they typically lack the resources to retain the most persuasive rationalist advocate.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__hanafi_reading, litigants_in_novel_cases_without_rationalist_advocates, payer,
    powerless, immediate, trapped, local).

% Shafi'i, Maliki, and Hanbali jurists hold competing accounts of what counts as a legitimate source and how much room reasoning may occupy. Where Hanafi courts hold administrative dominance, their alternative methodologies are not formally silenced but are structurally marginalized from the venues where rulings actually issue and precedent accumulates.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__hanafi_reading, rival_madhhab_jurists, excluded,
    organized, civilizational, constrained, continental).

% Study how each school's methodological commitments track its institutional fortunes — which empires patronized which school, how administrative convenience shaped doctrinal preference — without being party to the theological dispute itself.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__hanafi_reading, contemporary_legal_historians, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(jurisprudential_method_kernel__hanafi_reading, hanafi_trained_jurists).
narrative_ontology:fixing_cost_class(jurisprudential_method_kernel__hanafi_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a working method for extending a finite revealed corpus to an open-ended and growing set of novel legal questions across a vast, diverse, and changing empire, so that courts can issue rulings instead of declaring cases unanswerable.
% TRANSFER_FUNCTION: Moves interpretive authority — and the material and status rewards that follow from being the party whose ruling issues — from parties who hold a text-literal or consensus-only claim toward jurists whose specialized training in analogical reasoning and juristic preference makes them the indispensable adjudicators of novel cases.
% ABSENT_VOICES: Textualist hadith scholars object that istihsan substitutes human preference for divine command, but their objection operates from outside the Hanafi courts' own operating premises and cannot compel those courts to abandon analogical method. Litigants without access to skilled rationalist advocacy have no voice at all in how their own cases get analogized.
% DISAPPEARANCE_RATIONALE: If the Hanafi method's legitimation of qiyas and istihsan were withdrawn, courts operating under it would lose their primary mechanism for resolving cases the founding texts never addressed; administrators would need to fall back on ad hoc decree, consensus-seeking that novel or contested cases often cannot achieve, or a wholesale shift toward a rival school's method — the entire apparatus of Hanafi-administered law across its historical domains depends on this method being accepted as legitimate.
% FOUNDING_PROBLEM: The revealed corpus (Qur'an and transmitted Hadith) is finite and was fixed within a specific early Arabian social and commercial context, while the Muslim polity rapidly expanded into new territories, economies, and social arrangements the texts never directly addressed — someone had to decide how, and how far, revealed intent extends to unprecedented cases.
% FOUNDING_PROBLEM_CORROBORATION: Hanafi jurists themselves attest the problem remains permanently live, since novel cases never stop arising. Textualist critics from the Hanbali tradition attest that the problem is being used to license reasoning well beyond what genealogical or textual necessity requires — from outside the Hanafi school's own beneficiary set, they corroborate that a real founding problem exists but dispute that the scope of qiyas/istihsan required to address it is proportionate to the problem itself, rather than expansive by institutional convenience.
narrative_ontology:disappearance_verdict(jurisprudential_method_kernel__hanafi_reading, world_rearranges).
narrative_ontology:founding_problem_status(jurisprudential_method_kernel__hanafi_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jurisprudential_method_kernel__hanafi_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(jurisprudential_method_kernel__hanafi_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jurisprudential_method_kernel__hanafi_reading, 0.58, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jurisprudential_method_kernel__hanafi_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(jurisprudential_method_kernel__hanafi_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(jurisprudential_method_kernel__hanafi_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (0.58) reflects that the coordination benefit (courts can actually resolve novel cases) is real but comes bundled with a substantial transfer: interpretive authority, and the material/status rewards of being the party whose reasoning prevails, concentrate in hanafi-trained jurists at the expense of textualist claimants and under-resourced litigants. Suppression is moderate (0.42) rather than high because the constraint does not physically bar rival schools from existing or writing — it works through institutional dominance (which courts get administrative backing) rather than through coercive prohibition of alternative doctrine. Theater ratio is low-moderate (0.22): the analogical apparatus does real interpretive work, but a growing share of scholarly production over time serves to legitimate rulings after the fact rather than to generate them, which is why theater_ratio rises modestly across the measured interval. accessibility_collapse (0.4) is moderate: a litigant or scholar can still, in principle, appeal to a rival school's method, but doing so inside a Hanafi-administered court system offers little practical purchase. resistance (0.55) is substantial, tracking the genuinely organized and durable textualist counter-tradition.
 *
 * PERSPECTIVAL GAP:
 *   From the seat of hanafi_trained_jurists and abbasid_court_administrators, this method is straightforwardly coordinative: it is what makes a functioning legal order possible at imperial scale. From the seat of textualist_hadith_scholars and litigants without rationalist advocacy, the same structure operates as an extraction of interpretive authority that displaces a rival, text-grounded claim to legitimacy and imposes discretionary variance on parties who cannot influence how their case gets analogized. The engine computes these as different classifications from the same structural data; neither seat is in error about its own experience.
 *
 * DIRECTIONALITY LOGIC:
 *   hanafi_trained_jurists and abbasid_court_administrators are declared beneficiaries because the constraint's operation directly produces their authority, income, and administrative capacity — d sits near the beneficiary end. textualist_hadith_scholars are declared victims because their core epistemic claim (only transmitted text and consensus are authoritative) is structurally overridden every time a court prefers analogical reasoning — d sits near the target end despite their organizational strength, because their exit option (constrained: they remain inside the same theological field) does not let them escape the structural displacement. litigants_in_novel_cases_without_rationalist_advocates sit at the far target end: powerless, trapped, and bearing the full cost of discretionary variance with no textual anchor to appeal to.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — a finite revealed corpus meeting an open-ended set of novel cases — remains genuinely live wherever new commercial, technological, or social arrangements arise, which is permanent rather than time-bound. This prevents mislabeling the Hanafi method as pure extraction: the coordination function (resolving otherwise-unanswerable cases) does not expire. But the founding_problem_status is marked contested rather than clearly live, because critics corroborate that the scope of discretionary reasoning actually exercised has grown well past what strict textual necessity would require — the method's institutional entrenchment (training pipelines, court patronage) gives jurists incentive to expand qiyas/istihsan's domain beyond the narrow gap-filling role the founding problem alone would justify. This is the seat divergence the classification is built to hold open rather than resolve by fiat.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    hanafi_reading_vs_siblings_scope,
    'Is the Hanafi reading''s extensive use of qiyas and istihsan a proportionate response to genuine textual gaps, or does it constitute the innovation (bid''ah) that the Hanbali reading charges it with — and would a Shafi''i-style four-tier hierarchy resolve the same novel cases with less discretionary variance?',
    'This is not resolvable within this story: each sibling reading (hanbali_reading, maliki_reading, shafii_reading) is authored as its own constraint with its own ε and stakeholder structure. The disagreement is located in what counts as a legitimate source and how much room human reasoning may occupy relative to fixed text, transmitted consensus, or living communal practice.',
    'If the Hanbali critique is structurally correct, the coordination story here is closer to cover for jurist-class rent extraction than genuine gap-filling; if the Hanafi self-account is correct, the extraction measured here is closer to the necessary cost of administering law at imperial scale. This story assumes the Hanafi reading''s own framing for its ε (a reading-indexed referent, per the ε-invariance principle) rather than adjudicating between readings.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(hanafi_reading_vs_siblings_scope, conceptual, 'Committer-frame ambiguity: this constraint is one reading among four contested readings of the jurisprudential method kernel; disagreement is located in source legitimacy and the permissible scope of reasoned extension.').

omega_variable(
    istihsan_discretion_bound,
    'Is juristic preference (istihsan) bounded by identifiable, checkable criteria internal to the Hanafi method, or is it, as critics charge, effectively unbounded discretion dressed in technical vocabulary?',
    'Comparative analysis of Hanafi legal manuals across centuries: if istihsan rulings converge on stable, citable precedent over time, discretion is functionally bounded; if rulings diverge unpredictably by jurist and era, the discretion is closer to unbounded.',
    'A bounded-discretion finding would lower the effective extraction attributable to individual jurist variance; an unbounded finding would support the victim group''s claim that outcomes depend more on advocate skill and jurist preference than on principled derivation from revealed sources.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(istihsan_discretion_bound, empirical, 'Whether juristic preference operates under real internal constraint or amounts to unconstrained discretion.').

omega_variable(
    beneficiary_class_natural_or_constructed,
    'Did the class of hanafi-trained jurists emerge as a natural consequence of solving a real coordination problem, or was the training pipeline and administrative patronage constructed specifically to entrench that class''s interpretive monopoly?',
    'Historical analysis of Abbasid court patronage patterns: did administrative support for Hanafi courts precede or follow the maturation of the qiyas/istihsan method, and did rival schools face comparable patronage opportunities?',
    'If patronage was contingent on political convenience rather than the method''s superior problem-solving capacity, extraction is better understood as a captured administrative relationship rather than a naturally emergent coordination solution.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(beneficiary_class_natural_or_constructed, empirical, 'Whether the beneficiary class''s dominance reflects genuine functional superiority or contingent political patronage.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jurisprudential_method_kernel__hanafi_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(juri_tr_t0, jurisprudential_method_kernel__hanafi_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(juri_tr_t20, jurisprudential_method_kernel__hanafi_reading, theater_ratio, 20, 0.13).
narrative_ontology:measurement(juri_tr_t40, jurisprudential_method_kernel__hanafi_reading, theater_ratio, 40, 0.16).
narrative_ontology:measurement(juri_tr_t60, jurisprudential_method_kernel__hanafi_reading, theater_ratio, 60, 0.18).
narrative_ontology:measurement(juri_tr_t80, jurisprudential_method_kernel__hanafi_reading, theater_ratio, 80, 0.2).
narrative_ontology:measurement(juri_tr_t100, jurisprudential_method_kernel__hanafi_reading, theater_ratio, 100, 0.22).

% Extraction over time
narrative_ontology:measurement(juri_be_t0, jurisprudential_method_kernel__hanafi_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(juri_be_t20, jurisprudential_method_kernel__hanafi_reading, base_extractiveness, 20, 0.42).
narrative_ontology:measurement(juri_be_t40, jurisprudential_method_kernel__hanafi_reading, base_extractiveness, 40, 0.5).
narrative_ontology:measurement(juri_be_t60, jurisprudential_method_kernel__hanafi_reading, base_extractiveness, 60, 0.54).
narrative_ontology:measurement(juri_be_t80, jurisprudential_method_kernel__hanafi_reading, base_extractiveness, 80, 0.56).
narrative_ontology:measurement(juri_be_t100, jurisprudential_method_kernel__hanafi_reading, base_extractiveness, 100, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(juri_su_t0, jurisprudential_method_kernel__hanafi_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(juri_su_t20, jurisprudential_method_kernel__hanafi_reading, suppression_requirement, 20, 0.33).
narrative_ontology:measurement(juri_su_t40, jurisprudential_method_kernel__hanafi_reading, suppression_requirement, 40, 0.36).
narrative_ontology:measurement(juri_su_t60, jurisprudential_method_kernel__hanafi_reading, suppression_requirement, 60, 0.39).
narrative_ontology:measurement(juri_su_t80, jurisprudential_method_kernel__hanafi_reading, suppression_requirement, 80, 0.41).
narrative_ontology:measurement(juri_su_t100, jurisprudential_method_kernel__hanafi_reading, suppression_requirement, 100, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jurisprudential_method_kernel__hanafi_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(jurisprudential_method_kernel__hanafi_reading, 0.12).
narrative_ontology:affects_constraint(jurisprudential_method_kernel__hanafi_reading, jurisprudential_method_kernel__hanbali_reading).
narrative_ontology:affects_constraint(jurisprudential_method_kernel__hanafi_reading, jurisprudential_method_kernel__maliki_reading).
narrative_ontology:affects_constraint(jurisprudential_method_kernel__hanafi_reading, jurisprudential_method_kernel__shafii_reading).

% DUAL FORMULATION NOTE:
% This story is one of four sibling constraints decomposing the natural-language concept 'the proper method of Islamic legal derivation' (the jurisprudential_method_kernel). Each reading (hanafi, hanbali, maliki, shafii) authors its own ε, beneficiary/victim structure, and classification, per the ε-invariance principle: the underlying disagreement is not a measurement-parameter question but a genuine structural difference in what each school takes to be a legitimate source of law and how much room reasoning may occupy. The Hanafi reading shows the highest expected ε on novel cases specifically because its method licenses the widest scope for discretionary extension beyond directly transmitted text; the Hanbali reading is expected to show markedly lower ε on the same axis because it rejects discretionary reasoning as illegitimate innovation. Readers should not average across these four files to get 'the' extraction of Islamic jurisprudential method — there is no single such quantity.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(jurisprudential_method_kernel__hanafi_reading, organized, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
