% ============================================================================
% CONSTRAINT STORY: ai_human_relationship__instrumental_subsidiarity
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-14
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ai_human_relationship__instrumental_subsidiarity, []).

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
 *   constraint_id: ai_human_relationship__instrumental_subsidiarity
 *   human_readable: AI Governance as Instrumental Subsidiarity (Regulate-the-Neutral-Tool Reading)
 *   domain: technology_ethics/political_theology/regulatory_policy
 *
 * SUMMARY:
 *   This story authors ONE reading — instrumental_subsidiarity — of the
 *   contested kernel ai_human_relationship: AI as a morally neutral tool
 *   whose ethical weight is carried entirely by use-case and regulatory
 *   frame, with subsidiarity operationalized as a procedural safeguard
 *   (disclosure, audit, appeal) rather than a substantive claim about the
 *   ends technology must serve. The reading has genuine coordination value —
 *   it gives fast-moving deployment a common legal vocabulary that avoids
 *   both paralysis and a Wild West of unaccountable harm. But the same
 *   framework that solves that coordination problem also relocates liability
 *   onto individual decision-subjects and workers who lack resources to use
 *   the procedural remedies it offers, while creating a compliance-industry
 *   rent stream whose survival depends on the neutrality premise never being
 *   formally abandoned. This is a tangled rope: real coordination function,
 *   real asymmetric extraction, and active enforcement (courts, regulators,
 *   and audit regimes) holding the structure together.
 *
 * KEY AGENTS:
 *   - ai_deploying_corporations: institutional beneficiary/agenda_setter — shapes the standards that classify its own product as neutral
 *   - state_regulatory_bodies: institutional agenda_setter/beneficiary — gains mandate from governing but depends on the regulated industry for capacity
 *   - compliance_industry_intermediaries: organized beneficiary — monetizes the procedural apparatus itself
 *   - algorithmically_managed_workers / automated_decision_subjects / marginalized_communities_without_appeal_access: powerless payers — bear the harms the procedural remedies are formally supposed to address
 *   - catholic_social_teaching_advocates: excluded — reject the neutrality premise but are confined to non-binding preambles
 *   - independent_ai_ethics_researchers: analytical observer
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_human_relationship__instrumental_subsidiarity, 0.42).
domain_priors:suppression_score(ai_human_relationship__instrumental_subsidiarity, 0.38).
domain_priors:theater_ratio(ai_human_relationship__instrumental_subsidiarity, 0.47).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_human_relationship__instrumental_subsidiarity, extractiveness, 0.42).
narrative_ontology:constraint_metric(ai_human_relationship__instrumental_subsidiarity, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(ai_human_relationship__instrumental_subsidiarity, theater_ratio, 0.47).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ai_human_relationship__instrumental_subsidiarity, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(ai_human_relationship__instrumental_subsidiarity, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_human_relationship__instrumental_subsidiarity, tangled_rope).
narrative_ontology:human_readable(ai_human_relationship__instrumental_subsidiarity, "AI Governance as Instrumental Subsidiarity (Regulate-the-Neutral-Tool Reading)").
narrative_ontology:topic_domain(ai_human_relationship__instrumental_subsidiarity, "technology_ethics/political_theology/regulatory_policy").

domain_priors:requires_active_enforcement(ai_human_relationship__instrumental_subsidiarity).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_human_relationship__instrumental_subsidiarity, '24eb8c39-1a18-4115-a912-fab5b7dd0741').
narrative_ontology:cs_kernel_codification('24eb8c39-1a18-4115-a912-fab5b7dd0741', distributed).
narrative_ontology:cs_authority_grounding('24eb8c39-1a18-4115-a912-fab5b7dd0741', distributed).
narrative_ontology:cs_reading_relation('24eb8c39-1a18-4115-a912-fab5b7dd0741', ai_human_relationship__technocratic_optimization, influences).
narrative_ontology:cs_reading_relation('24eb8c39-1a18-4115-a912-fab5b7dd0741', ai_human_relationship__incarnational_humanism, coexists_with).
narrative_ontology:cs_axiom('24eb8c39-1a18-4115-a912-fab5b7dd0741', foundational, technology_morally_neutral_use_determines_ethics).
narrative_ontology:cs_axiom_status(technology_morally_neutral_use_determines_ethics, holdable).
narrative_ontology:cs_axiom_grounding('24eb8c39-1a18-4115-a912-fab5b7dd0741', technology_morally_neutral_use_determines_ethics, conventional).
narrative_ontology:cs_axiom('24eb8c39-1a18-4115-a912-fab5b7dd0741', secondary, subsidiarity_satisfied_by_procedural_safeguard).
narrative_ontology:cs_axiom_status(subsidiarity_satisfied_by_procedural_safeguard, holdable).
narrative_ontology:cs_axiom_grounding('24eb8c39-1a18-4115-a912-fab5b7dd0741', subsidiarity_satisfied_by_procedural_safeguard, instrumental).
narrative_ontology:cs_reference_frame('24eb8c39-1a18-4115-a912-fab5b7dd0741', liberal_legal_technology_neutrality_doctrine).
narrative_ontology:cs_drift_state('24eb8c39-1a18-4115-a912-fab5b7dd0741', contemporary_algorithmic_governance_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('24eb8c39-1a18-4115-a912-fab5b7dd0741', '').
narrative_ontology:cs_kernel_id(ai_human_relationship__instrumental_subsidiarity, ai_human_relationship).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_human_relationship__instrumental_subsidiarity, ai_deploying_corporations).
narrative_ontology:constraint_beneficiary(ai_human_relationship__instrumental_subsidiarity, compliance_industry_intermediaries).
narrative_ontology:constraint_beneficiary(ai_human_relationship__instrumental_subsidiarity, state_regulatory_bodies).
narrative_ontology:constraint_victim(ai_human_relationship__instrumental_subsidiarity, algorithmically_managed_workers).
narrative_ontology:constraint_victim(ai_human_relationship__instrumental_subsidiarity, automated_decision_subjects).
narrative_ontology:constraint_victim(ai_human_relationship__instrumental_subsidiarity, marginalized_communities_without_appeal_access).
narrative_ontology:constraint_vindicates(ai_human_relationship__instrumental_subsidiarity, technological_neutrality_thesis).
narrative_ontology:constraint_vindicates(ai_human_relationship__instrumental_subsidiarity, procedural_subsidiarity_sufficiency).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Deploy AI systems under a legal-compliance framework they helped draft through standards bodies and lobbying. Because the tool is classified as morally neutral, liability attaches to 'use-cases' and downstream operators rather than to design choices upstream. They fund the transparency and audit apparatus that certifies their own systems, and can relocate deployment jurisdictions when a regulatory regime tightens.
narrative_ontology:constraint_stakeholder(ai_human_relationship__instrumental_subsidiarity, ai_deploying_corporations, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(ai_human_relationship__instrumental_subsidiarity, ai_deploying_corporations, agenda_setter).

% Auditors, certification bodies, and ethics-board consultants who monetize the subsidiarity framework by selling compliance documentation, model cards, and impact assessments. Their business model requires the neutrality premise to remain intact — if technology were held non-neutral, their procedural product would be insufficient by design.
narrative_ontology:constraint_stakeholder(ai_human_relationship__instrumental_subsidiarity, compliance_industry_intermediaries, beneficiary,
    organized, biographical, mobile, global).

% Write and enforce the rules that operationalize subsidiarity — risk tiers, disclosure duties, appeal mechanisms. Gain legitimacy and institutional mandate from being seen to govern a genuinely new domain, but are structurally dependent on the regulated firms for technical expertise, funding of assessment infrastructure, and political cover; enforcement capacity trails deployment speed.
narrative_ontology:constraint_stakeholder(ai_human_relationship__instrumental_subsidiarity, state_regulatory_bodies, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(ai_human_relationship__instrumental_subsidiarity, state_regulatory_bodies, beneficiary).

% Have shifts, wages, and terminations determined by systems whose neutrality classification means the worker must locate and contest a specific misapplication rather than challenge the system's legitimacy. Formal appeal channels exist on paper but require resources, time, and technical literacy most workers lack; leaving the platform means leaving the income.
narrative_ontology:constraint_stakeholder(ai_human_relationship__instrumental_subsidiarity, algorithmically_managed_workers, payer,
    powerless, immediate, trapped, national).

% Loan applicants, benefits claimants, and defendants scored or triaged by AI systems. The subsidiarity frame directs their grievance toward procedural remedy (request an explanation, file an appeal) rather than toward the deployment decision itself, and the burden of proving harm falls on them.
narrative_ontology:constraint_stakeholder(ai_human_relationship__instrumental_subsidiarity, automated_decision_subjects, payer,
    powerless, immediate, trapped, national).

% Communities with the least access to legal aid, language support, or digital literacy bear disproportionate harm from automated systems (policing, welfare, housing) while having the least capacity to use the procedural safeguards the framework offers as its remedy.
narrative_ontology:constraint_stakeholder(ai_human_relationship__instrumental_subsidiarity, marginalized_communities_without_appeal_access, payer,
    powerless, biographical, trapped, regional).

% Argue that treating technology as neutral obscures the way design choices already encode ends and values, and that subsidiarity properly understood requires substantive ordering toward the common good, not merely procedural checks. Their theological argument is acknowledged in preambles to regulatory documents but rarely shapes binding provisions.
narrative_ontology:constraint_stakeholder(ai_human_relationship__instrumental_subsidiarity, catholic_social_teaching_advocates, excluded,
    moderate, civilizational, constrained, global).

% Study the gap between the procedural safeguards on paper and harm outcomes in deployment; produce evidence used by both regulators seeking to tighten rules and by advocates arguing the framework is structurally insufficient.
narrative_ontology:constraint_stakeholder(ai_human_relationship__instrumental_subsidiarity, independent_ai_ethics_researchers, observer,
    moderate, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ai_human_relationship__instrumental_subsidiarity, ai_deploying_corporations).
narrative_ontology:fixing_cost_class(ai_human_relationship__instrumental_subsidiarity, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a common legal-procedural vocabulary — risk tiers, disclosure duties, audit trails, appeal rights — that lets firms, regulators, and courts coordinate around a shared set of expectations for a fast-moving technology, avoiding a chaotic patchwork of ad hoc liability rulings.
% TRANSFER_FUNCTION: Moves the burden of proof and the cost of contesting harm from the deploying institution to the individual decision-subject or worker, while moving compliance revenue from deploying firms to the certification and audit industry that services the neutrality framework.
% ABSENT_VOICES: Communities most harmed by automated decisions rarely appear in the standards-setting bodies that write the technical definitions of 'risk' and 'transparency'; theological and substantive-ethics traditions that reject the neutrality premise are cited in preambles but excluded from binding text.
% DISAPPEARANCE_RATIONALE: If the instrumental-subsidiarity framework vanished, deploying firms would lose their liability shield tied to use-case classification, the compliance-audit industry would lose its product, and courts would have to adjudicate AI harms directly against design and deployment decisions rather than against procedural compliance — a substantial reallocation of liability and cost.
% FOUNDING_PROBLEM: Rapid AI deployment outpaced existing liability and regulatory categories; some workable legal vocabulary was needed quickly to prevent either regulatory paralysis or unaccountable harm, and 'treat the tool as neutral, regulate the use' offered a tractable, familiar legal analogy (borrowed from earlier technology-neutral doctrines).
% FOUNDING_PROBLEM_CORROBORATION: Regulators and deploying firms attest the framework remains necessary to keep pace with deployment speed. Independent AI ethics researchers and Catholic social teaching advocates — outside the beneficiary set — attest the founding problem has been substantially met by the existence of workable substantive-harm doctrines elsewhere in law (e.g. product liability, disparate impact) and that the neutrality framing now functions primarily to preserve firm discretion rather than to solve a genuine categorization gap.
narrative_ontology:disappearance_verdict(ai_human_relationship__instrumental_subsidiarity, world_rearranges).
narrative_ontology:founding_problem_status(ai_human_relationship__instrumental_subsidiarity, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ai_human_relationship__instrumental_subsidiarity, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(ai_human_relationship__instrumental_subsidiarity, 'none', 1).
narrative_ontology:epsilon_provenance(ai_human_relationship__instrumental_subsidiarity, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ai_human_relationship__instrumental_subsidiarity_tests).
:- end_tests(ai_human_relationship__instrumental_subsidiarity_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (0.42) and suppression (0.38) are moderate rather than severe because the coordination function is real — the legal vocabulary genuinely reduces chaos relative to no framework at all — but both rise across the interval as the compliance industry matures and audit/appeal machinery hardens into something closer to enforcement infrastructure than a stopgap. Theater ratio (0.47, rising from 0.22) reflects the growing gap between the volume of disclosure/audit activity and its actual capacity to alter deployment decisions or deliver remedy to harmed individuals — the procedural apparatus increasingly performs accountability more than it delivers it. Accessibility collapse is moderate (0.35): alternative framings (substantive/theological, or strict-liability) remain articulable and are not suppressed outright, they are simply excluded from binding text. Resistance is comparatively high (0.55) because CST advocates, some regulators, and affected communities actively contest the neutrality premise rather than accepting it as settled.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter/beneficiary seats, this reading looks like rope: a sensible, minimal, procedurally fair way to govern a genuinely novel technology without overreach. From the payer seats, the same structure computes as tangled — genuine coordination value exists, but it rides on a liability-shifting mechanism that requires active enforcement (courts enforcing procedural-not-substantive standards) to keep the burden on the decision-subject rather than the designer. The engine's per-seat computation should surface this divergence directly from the declared power/exit data rather than from any narrative framing.
 *
 * DIRECTIONALITY LOGIC:
 *   Deploying corporations and the compliance industry sit near the beneficiary end: they set or shape the rules, capture the certification revenue, and retain exit/relocation options if regulatory regimes tighten (arbitrage/mobile exit). State regulators are a genuine dual seat — they gain institutional mandate (beneficiary-flavored) but are also structurally dependent on the very firms they regulate for technical capacity, which pulls their effective directionality toward the middle rather than the pure-target or pure-beneficiary pole. Workers and decision-subjects are trapped targets: no meaningful exit from algorithmic management or automated triage, and the procedural remedy the framework offers requires resources they structurally lack, so effective extraction on them is amplified beyond the base rate.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — a fast-moving technology outrunning existing legal categories — was genuinely live at the framework's origin. Whether it remains live is contested: substantive-harm doctrines already exist elsewhere in law (product liability, disparate-impact analysis) that could in principle absorb AI harms without the neutrality fiction, suggesting the original coordination problem may now be substantially solved by adjacent legal machinery, while the instrumental-subsidiarity framework persists partly because it now has an entrenched beneficiary (the compliance industry) with an interest in its continuation. Classifying this as tangled_rope rather than snare preserves the genuine coordination function (a shared standards vocabulary that did reduce chaos) while refusing to let that function launder the asymmetric burden-shifting the same structure accomplishes — mislabeling it pure extraction would erase the real coordination gain it once provided; mislabeling it pure coordination would erase the diffuse victims who pay for it now.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    neutrality_premise_ambiguity,
    'Is the claim that AI systems are morally neutral tools a defensible structural fact about technology, or is it itself a constructed framing that benefits parties who profit from deferring ethical weight to ''use-case'' rather than design?',
    'Comparative analysis of design-stage value embedding (training data selection, objective function specification, deployment defaults) against the claim that ethical valence enters only at use-case; if design choices demonstrably pre-determine downstream harms independent of use-case, the neutrality premise is empirically weaker than the framework assumes.',
    'If neutrality is constructed rather than structural, the tangled_rope classification understates extraction — the framework would function closer to a snare wearing coordination language, since the ''coordination'' (shared vocabulary) would itself be organized around protecting a false premise.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(neutrality_premise_ambiguity, conceptual, 'Whether technological neutrality is a genuine structural fact or a beneficiary-serving construction.').

omega_variable(
    kernel_reading_disagreement_locus,
    'Where exactly does this reading (instrumental_subsidiarity) diverge from its siblings (technocratic_optimization, incarnational_humanism) — is the disagreement about facts (does technology have inherent orientation?) or about values (should law encode substantive ends or only procedural safeguards)?',
    'This is the committer-structure question routed here per Rule 2: it is not resolvable within this single reading''s constraint story. Documenting it as an omega rather than inventing a schema field for it, per the authoring rules.',
    'If the disagreement is factual (about whether design encodes ends), evidence could in principle move the kernel toward one reading. If it is normative (about the sufficiency of procedural vs. substantive governance), no empirical resolution exists and the kernel remains a genuine multi-reading contest indefinitely.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_disagreement_locus, conceptual, 'Locates the structural disagreement between sibling kernel readings without adjudicating it inside this story.').

omega_variable(
    subsidiarity_procedural_vs_substantive,
    'Does the Catholic social teaching principle of subsidiarity, properly understood, permit a purely procedural operationalization (disclosure/audit/appeal), or does it require the substantive ordering-toward-common-good that the incarnational_humanism reading insists on?',
    'Close textual and magisterial analysis of subsidiarity''s use in CST documents (Quadragesimo Anno through Fratelli Tutti) applied specifically to AI governance texts, cross-checked against how subsidiarity has been operationalized in other domains (healthcare, education) where CST has spoken more concretely.',
    'If subsidiarity requires substantive ordering, this reading''s self-description as a faithful application of CST subsidiarity is itself contestable — it may be borrowing the term''s legitimacy while dropping its substantive content, which would strengthen the false-summit-adjacent reading of this constraint''s beneficiary structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(subsidiarity_procedural_vs_substantive, conceptual, 'Whether the procedural subsidiarity claimed here is faithful to or a thinning of the CST doctrine it invokes.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_human_relationship__instrumental_subsidiarity, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ai_h_tr_t0, ai_human_relationship__instrumental_subsidiarity, theater_ratio, 0, 0.22).
narrative_ontology:measurement(ai_h_tr_t4, ai_human_relationship__instrumental_subsidiarity, theater_ratio, 4, 0.27).
narrative_ontology:measurement(ai_h_tr_t8, ai_human_relationship__instrumental_subsidiarity, theater_ratio, 8, 0.32).
narrative_ontology:measurement(ai_h_tr_t12, ai_human_relationship__instrumental_subsidiarity, theater_ratio, 12, 0.37).
narrative_ontology:measurement(ai_h_tr_t16, ai_human_relationship__instrumental_subsidiarity, theater_ratio, 16, 0.41).
narrative_ontology:measurement(ai_h_tr_t20, ai_human_relationship__instrumental_subsidiarity, theater_ratio, 20, 0.44).
narrative_ontology:measurement(ai_h_tr_t24, ai_human_relationship__instrumental_subsidiarity, theater_ratio, 24, 0.47).

% Extraction over time
narrative_ontology:measurement(ai_h_be_t0, ai_human_relationship__instrumental_subsidiarity, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(ai_h_be_t4, ai_human_relationship__instrumental_subsidiarity, base_extractiveness, 4, 0.31).
narrative_ontology:measurement(ai_h_be_t8, ai_human_relationship__instrumental_subsidiarity, base_extractiveness, 8, 0.34).
narrative_ontology:measurement(ai_h_be_t12, ai_human_relationship__instrumental_subsidiarity, base_extractiveness, 12, 0.37).
narrative_ontology:measurement(ai_h_be_t16, ai_human_relationship__instrumental_subsidiarity, base_extractiveness, 16, 0.39).
narrative_ontology:measurement(ai_h_be_t20, ai_human_relationship__instrumental_subsidiarity, base_extractiveness, 20, 0.41).
narrative_ontology:measurement(ai_h_be_t24, ai_human_relationship__instrumental_subsidiarity, base_extractiveness, 24, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(ai_h_su_t0, ai_human_relationship__instrumental_subsidiarity, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(ai_h_su_t4, ai_human_relationship__instrumental_subsidiarity, suppression_requirement, 4, 0.24).
narrative_ontology:measurement(ai_h_su_t8, ai_human_relationship__instrumental_subsidiarity, suppression_requirement, 8, 0.28).
narrative_ontology:measurement(ai_h_su_t12, ai_human_relationship__instrumental_subsidiarity, suppression_requirement, 12, 0.31).
narrative_ontology:measurement(ai_h_su_t16, ai_human_relationship__instrumental_subsidiarity, suppression_requirement, 16, 0.34).
narrative_ontology:measurement(ai_h_su_t20, ai_human_relationship__instrumental_subsidiarity, suppression_requirement, 20, 0.36).
narrative_ontology:measurement(ai_h_su_t24, ai_human_relationship__instrumental_subsidiarity, suppression_requirement, 24, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_human_relationship__instrumental_subsidiarity, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(ai_human_relationship__instrumental_subsidiarity, 0.1).
narrative_ontology:affects_constraint(ai_human_relationship__instrumental_subsidiarity, ai_human_relationship__technocratic_optimization).
narrative_ontology:affects_constraint(ai_human_relationship__instrumental_subsidiarity, ai_human_relationship__incarnational_humanism).

% DUAL FORMULATION NOTE:
% Three sibling constraint stories instantiate the ai_human_relationship kernel's three declared readings: instrumental_subsidiarity (this story — neutral tool, procedural safeguard), technocratic_optimization (AI as efficiency instrument, human value measured by productivity), and incarnational_humanism (AI ordered to integral human development, common good, and irreducible human dignity). Each carries its own ε, beneficiary/victim structure, and computed type per the ε-invariance principle; they are linked here rather than merged because the natural-language label 'how should we think about AI and humanity' covers three structurally distinct normative claims with different victim sets and different persistence logics.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
