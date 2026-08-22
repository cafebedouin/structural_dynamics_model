% ============================================================================
% CONSTRAINT STORY: geneva_conventions_1949__humanitarian_ceiling_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_geneva_conventions_1949__humanitarian_ceiling_reading, []).

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
 *   constraint_id: geneva_conventions_1949__humanitarian_ceiling_reading
 *   human_readable: Geneva Conventions 1949 — Humanitarian Ceiling Reading
 *   domain: political/legal/humanitarian
 *
 * SUMMARY:
 *   The 1949 Geneva Conventions establish international humanitarian law
 *   binding on state militaries. This constraint models ONE reading of the
 *   contested kernel: the humanitarian ceiling reading, which interprets the
 *   Conventions as establishing absolute humanitarian minimums that apply
 *   unconditionally, regardless of adversary compliance, irregular warfare
 *   tactics, or security claims. Under this reading, state militaries must
 *   provide medical treatment, shelter, and basic protections to all
 *   detainees and civilians; torture and summary execution are prohibited
 *   absolutely; irregular combatants retain baseline humanitarian protection
 *   even without formal POW recognition. The ceiling reading suppresses the
 *   security-maximization reading (which claims necessity overrides
 *   protections in asymmetric conflict) and pressures the
 *   conditional-reciprocity reading (which treats protections as mutual
 *   obligations subject to proportional degradation when adversaries violate
 *   them). The architectural constraint is the interpretation itself: the
 *   text is fixed but meaning is contested. This JSON models the ceiling
 *   reading as a structurally distinct constraint with its own extraction,
 *   suppression, beneficiaries, and victims — distinct from what the
 *   conditional-reciprocity or security-maximization readings would author.
 *
 * KEY AGENTS:
 *   - state_militaries: Payers; bear unconditional humanitarian obligations
 *   - protected_persons_category: Beneficiaries; gain absolute protections unconditionally
 *   - humanitarian_advocacy_movement: Beneficiaries; gain legitimacy and mandate from the ceiling reading
 *   - international_legal_institutions: Agenda-setters; interpret and enforce the ceiling, gain jurisdiction and authority
 *   - security_apparatus: Payers; face constraints on interrogation and operational methods
 *   - adversarial_non_state_actors: Excluded; nominally bound but enforcement is asymmetric
 *   - civilian_populations: Beneficiaries; gain absolute protection from targeting and indiscriminate harm
 *   - political_security_maximizers: Excluded; the ceiling reading forecloses their security rationales structurally
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(geneva_conventions_1949__humanitarian_ceiling_reading, 0.62).
domain_priors:suppression_score(geneva_conventions_1949__humanitarian_ceiling_reading, 0.71).
domain_priors:theater_ratio(geneva_conventions_1949__humanitarian_ceiling_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(geneva_conventions_1949__humanitarian_ceiling_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(geneva_conventions_1949__humanitarian_ceiling_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(geneva_conventions_1949__humanitarian_ceiling_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(geneva_conventions_1949__humanitarian_ceiling_reading, accessibility_collapse, 0.79).
narrative_ontology:constraint_metric(geneva_conventions_1949__humanitarian_ceiling_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(geneva_conventions_1949__humanitarian_ceiling_reading, tangled_rope).
narrative_ontology:human_readable(geneva_conventions_1949__humanitarian_ceiling_reading, "Geneva Conventions 1949 — Humanitarian Ceiling Reading").
narrative_ontology:topic_domain(geneva_conventions_1949__humanitarian_ceiling_reading, "political/legal/humanitarian").

domain_priors:requires_active_enforcement(geneva_conventions_1949__humanitarian_ceiling_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(geneva_conventions_1949__humanitarian_ceiling_reading, 'ca83584b-0466-4ca1-9843-51050ba6553a').
narrative_ontology:cs_kernel_codification('ca83584b-0466-4ca1-9843-51050ba6553a', fixed_text).
narrative_ontology:cs_authority_grounding('ca83584b-0466-4ca1-9843-51050ba6553a', lineage).
narrative_ontology:cs_interpretation_layer_present('ca83584b-0466-4ca1-9843-51050ba6553a').
narrative_ontology:cs_reading_relation('ca83584b-0466-4ca1-9843-51050ba6553a', geneva_conventions_1949__conditional_reciprocity_reading, coexists_with).
narrative_ontology:cs_reading_relation('ca83584b-0466-4ca1-9843-51050ba6553a', geneva_conventions_1949__security_maximization_reading, coexists_with).
narrative_ontology:cs_axiom('ca83584b-0466-4ca1-9843-51050ba6553a', foundational, unconditional_human_dignity).
narrative_ontology:cs_axiom_status(unconditional_human_dignity, holdable).
narrative_ontology:cs_axiom_grounding('ca83584b-0466-4ca1-9843-51050ba6553a', unconditional_human_dignity, deontological).
narrative_ontology:cs_axiom('ca83584b-0466-4ca1-9843-51050ba6553a', foundational, absolute_prohibition_doctrine).
narrative_ontology:cs_axiom_status(absolute_prohibition_doctrine, holdable).
narrative_ontology:cs_axiom_grounding('ca83584b-0466-4ca1-9843-51050ba6553a', absolute_prohibition_doctrine, deontological).
narrative_ontology:cs_reference_frame('ca83584b-0466-4ca1-9843-51050ba6553a', post_wwii_atrocity_prevention_consensus).
narrative_ontology:cs_drift_state('ca83584b-0466-4ca1-9843-51050ba6553a', contemporary_irregular_warfare_environment, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('ca83584b-0466-4ca1-9843-51050ba6553a', '2026-06-11T14:32:00Z').
narrative_ontology:cs_kernel_id(geneva_conventions_1949__humanitarian_ceiling_reading, geneva_conventions_1949).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(geneva_conventions_1949__humanitarian_ceiling_reading, protected_persons_category).
narrative_ontology:constraint_beneficiary(geneva_conventions_1949__humanitarian_ceiling_reading, humanitarian_advocacy_movement).
narrative_ontology:constraint_beneficiary(geneva_conventions_1949__humanitarian_ceiling_reading, international_legal_institutions).
narrative_ontology:constraint_victim(geneva_conventions_1949__humanitarian_ceiling_reading, state_militaries).
narrative_ontology:constraint_victim(geneva_conventions_1949__humanitarian_ceiling_reading, security_apparatus).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(geneva_conventions_1949__humanitarian_ceiling_reading, civilian_populations).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Bear the asymmetric burden of humanitarian obligations: must provide medical treatment, shelter, and basic protections to combatants and detainees regardless of whether adversaries reciprocate or comply. Face enforcement through international courts, domestic litigation, and reputational consequences for violations. Must maintain these standards even when adversaries use irregular tactics, deny POW status to captured soldiers, or operate outside the treaty framework entirely. The burden falls entirely on state actors; non-state armed groups operate under lesser documented accountability.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__humanitarian_ceiling_reading, state_militaries, payer,
    institutional, generational, constrained, global).

% Civilians, wounded combatants, and prisoners of war receive absolute protections: shelter, medical care, prohibition on torture, summary execution, and abuse. These protections are unconditional — they do not depend on adversary compliance, the legal status of the armed group holding them, or military necessity claims. Protection extends even to combatants who do not qualify for POW status (irregular fighters, combatants from non-state armed groups) under the expanded interpretation this reading endorses.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__humanitarian_ceiling_reading, protected_persons_category, beneficiary,
    powerless, immediate, trapped, global).

% International humanitarian NGOs, the International Committee of the Red Cross, and civil society organizations gain standing and mandate authority from the humanitarian ceiling reading. The interpretation vindicates their advocacy, funds research and monitoring, and provides legal grounds for institutional campaigns. Their legitimacy and political capital depend on the permanence of this reading.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__humanitarian_ceiling_reading, humanitarian_advocacy_movement, beneficiary,
    organized, generational, mobile, global).

% International Court of Justice, International Criminal Court, UN Human Rights mechanisms, and treaty monitoring bodies interpret and enforce the humanitarian ceiling reading. They gain authority and jurisdiction from the expansive interpretation; their caseloads, mandates, and institutional relevance expand when states are held to absolute humanitarian standards rather than reciprocal or necessity-based exceptions. They adjudicate violations and can impose accountability.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__humanitarian_ceiling_reading, international_legal_institutions, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(geneva_conventions_1949__humanitarian_ceiling_reading, international_legal_institutions, beneficiary).

% Military intelligence, special operations, and interrogation personnel face constraints on their operational methods: enhanced interrogation, stress positions, and other coercive techniques are prohibited unconditionally, even when adversaries deny treatment to state prisoners. They argue these constraints reduce operational effectiveness in asymmetric conflict and that reciprocity should permit calibrated relaxation. The humanitarian ceiling reading suppresses this argument structurally.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__humanitarian_ceiling_reading, security_apparatus, payer,
    powerful, biographical, constrained, national).

% Irregular armed groups, insurgencies, and non-state combatants are nominally bound by the same humanitarian protections but face asymmetric documentation, enforcement, and accountability mechanisms. They are excluded from the Geneva framework's formal dispute resolution and rarely subject to international prosecution; the enforcement asymmetry means the ceiling operates primarily as a constraint on state behavior. This reading does not reduce the applicable standard for non-state actors but does not increase enforcement against them proportionately.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__humanitarian_ceiling_reading, adversarial_non_state_actors, excluded,
    moderate, biographical, trapped, regional).

% Gain absolute prohibitions on targeting, indiscriminate bombardment, collective punishment, and forced displacement. These protections are unconditional — they do not depend on civilian compliance or whether non-state adversaries observe the same rules. Civilians cannot legally be harmed for the military advantage gained.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__humanitarian_ceiling_reading, civilian_populations, beneficiary,
    powerless, immediate, trapped, regional).

% Policymakers and military strategists who prioritize rapid victory and force protection over humanitarian constraints are excluded from the decision frame. This reading suppresses security-maximization arguments by treating humanitarian protections as non-negotiable legal obligations rather than policy trade-offs. The exclusion is structural: the ceiling reading forecloses the legitimacy of security rationales as exceptions to humanitarian law.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__humanitarian_ceiling_reading, political_security_maximizers, excluded,
    institutional, biographical, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(geneva_conventions_1949__humanitarian_ceiling_reading, international_legal_institutions).
narrative_ontology:fixing_cost_class(geneva_conventions_1949__humanitarian_ceiling_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a universal legal floor for human treatment in armed conflict: common standards for all state militaries eliminate the race-to-the-bottom that reciprocal arrangements would create, reduce combatant incentives to violate the rules, and provide neutral ground for international accountability and post-conflict reconciliation.
% TRANSFER_FUNCTION: Transfers the burden of unconditional compliance from mutual reciprocal obligation to unilateral state responsibility: states bear the cost of providing protections even when adversaries do not reciprocate. International legal institutions and humanitarian organizations transfer legitimacy from individual states to a supra-national normative framework. Asylum, protection, and investigative capacity transfer from ad-hoc arrangements to formalized institutions.
% ABSENT_VOICES: Non-state armed groups and insurgencies are systematically excluded from formal rulemaking despite being bound by the same standards — they have no seat in treaty negotiations, monitoring mechanisms are weaker for their violations, and enforcement is asymmetric. Military security establishments that argue necessity should permit derogation are excluded by the reading's structural suppression of security rationales.
% DISAPPEARANCE_RATIONALE: If the humanitarian ceiling reading disappeared and the conditional-reciprocity reading took its place entirely, state militaries would retain enforcement discretion to degrade protections when adversaries operate outside the framework (immediate restructuring of interrogation policy, detainee treatment, and civilian targeting thresholds). But the law itself would not disappear — states would reorganize around the conditional-reciprocity framework, which is itself an established reading of the same text. The question is therefore not disappearance but institutional rearrangement.
% FOUNDING_PROBLEM: World War II industrial genocide, mass civilian casualties, torture of prisoners, and absence of accountability created a post-war consensus that absolute prohibitions on certain acts — regardless of military context — are necessary to prevent civilizational collapse. The founding problem is: how do you prevent states from repeating atrocities?
% FOUNDING_PROBLEM_CORROBORATION: The humanitarian reading is attested to by ICRC, International Criminal Court prosecutors, academic human rights law, and civil society organizations outside the state security establishment. States and military planners attest the founding problem has shifted: modern irregular warfare and non-state actors are presented as the new threat, requiring flexibility from the absolute ceiling. Independent historians of WWII and genocide prevention scholars support the humanitarian reading's connection to the original atrocity prevention mandate.
narrative_ontology:disappearance_verdict(geneva_conventions_1949__humanitarian_ceiling_reading, contested).
narrative_ontology:founding_problem_status(geneva_conventions_1949__humanitarian_ceiling_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(geneva_conventions_1949__humanitarian_ceiling_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(geneva_conventions_1949__humanitarian_ceiling_reading, 'none', 1).
narrative_ontology:epsilon_provenance(geneva_conventions_1949__humanitarian_ceiling_reading, 0.62, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(geneva_conventions_1949__humanitarian_ceiling_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(geneva_conventions_1949__humanitarian_ceiling_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(geneva_conventions_1949__humanitarian_ceiling_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness at 0.62 reflects the burden placed on state militaries to maintain protections unconditionally: they must provide care, shelter, and dignity regardless of whether adversaries reciprocate or operate outside the framework. The measurement series shows extractiveness rising from 0.35 (1949, immediate post-war consensus, high state agreement) to 0.62 by 2011 (post-9/11 security environment and irregular warfare challenge the ceiling increasingly). It plateaus at 0.62 by 2020–2026 because the ceiling reading has consolidated institutional support despite ongoing security-sector resistance; the ceiling is now legally codified and monitored through permanent institutions. Suppression at 0.71 reflects the structural prohibition of security rationales as legitimate exceptions: the humanitarian ceiling reading treats necessity claims as non-cognizable in the legal frame — this is active suppression of an alternative framing, not merely a passive constraint. Theater ratio at 0.28 (moderate) indicates that enforcement and monitoring are genuine institutional activities (ICRC visits, ICC investigations, treaty bodies), but a significant fraction is performative: states ceremonially reaffirm the Conventions while security establishments seek operative workarounds (classification disputes, administrative detention, enhanced interrogation justified under different legal theories). The measurement series shows theater rising from 0.12 to 0.28 over the interval, consistent with increasing gap between the ceiling reading's normative claim and the security-sector's operational resistance. All six time points are on a shared grid: every metric is authored at every interval point to enable temporal analysis without misalignment.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seats (international legal institutions) and beneficiary seats (protected persons, humanitarian organizations) experience this constraint as genuine coordination solving a post-WWII atrocity-prevention problem, with legitimate burden-sharing (state militaries accept constraints, international institutions provide monitoring, humanitarian movement provides advocacy). The payer seats (state militaries, security apparatus) experience it as imposed extraction: they bear costs (training restrictions, interrogation limits, detainee care requirements) while international institutions gain authority and humanitarian organizations gain funding and mandate, with asymmetric enforcement against state actors and weak accountability for non-state adversaries. These divergent percepts are computed by the engine from the structural data: the agenda-setter and beneficiary roles combined with institutional power and organized/powerless stakes create the perception of coordination; the payer role combined with constrained exit and institutional power constraints creates the extraction percept. The reading itself (humanitarian ceiling) suppresses the security-maximization argument structurally, which is why security-apparatus seats classify this as suppression; the conditional-reciprocity reading would permit security arguments as legitimate, creating a different classification from those same seats.
 *
 * DIRECTIONALITY LOGIC:
 *   State militaries and security apparatus: d near 1.0 (full targets). They are declared victims in base_properties, constrained in exit (cannot easily withdraw from international law), and bear asymmetric burden. The ceiling reading provides no escape hatch for necessity or reciprocal degradation. Protected persons and humanitarian organizations: d near 0.0 (beneficiaries). They gain absolute protections and institutional legitimacy without operational costs. International legal institutions: d near 0.25–0.35 (partial beneficiary with light costs). They gain jurisdiction and authority from the ceiling's enforcement but bear modest operational costs (investigations, litigation, institutional overhead). Civilian populations: d near 0.0 (pure beneficiaries). They gain absolute protection without operative participation. Excluded actors (adversarial non-state armed groups, security maximizers): they do not populate directionality — their exclusion is the structural fact. The asymmetry is intentional: the humanitarian ceiling reading creates structured burden on state militaries to produce a universal floor; the burden would be symmetric if the reading were conditional-reciprocity, which would permit state operational flexibility proportional to adversary violations.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (post-WWII atrocity prevention) and founding_problem_status (contested) + disappearance_verdict (contested) generate a mandatrophy signal. The humanitarian advocates argue the problem is live: atrocities still occur and the ceiling provides the legal framework preventing worse outcomes. Security establishments argue the problem has shifted: irregular warfare and non-state actors are now the threat, and the ceiling's constraints prevent effective counter-insurgency and force protection. The mismatch is: founding_problem = atrocity prevention; founding_problem_status = contested (advocates say live, security says obsolete); disappearance_verdict = contested (humanitarian says world would rearrange toward atrocities, security says world would adapt operational doctrine and security outcomes improve). This mismatch flags potential mandatrophy (the founding problem's function has eroded or shifted), but the reading itself rejects that flag: it reasserts the ceiling precisely because it contests the security-maximization reading's claim that the problem is obsolete. The tension is built into the kernel: whether the humanitarian ceiling reading is an up-to-date constraint or a degraded one depends on whether post-WWII atrocity prevention remains the organizing principle of international humanitarian law.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reading_vs_text_ambiguity,
    'Does the 1949 Geneva Conventions text itself mandate the humanitarian ceiling reading, or is the ceiling an expansive interpretation layered onto an ambiguous text?',
    'Textual analysis by neutral legal scholars comparing the original treaty language, travaux préparatoires, and the Additional Protocols of 1977 — which clarified certain provisions but left others contested. The 1977 Protocols'' codification of irregular combatant protections is read as validating the ceiling by humanitarian advocates and as expanding the ceiling beyond original intent by state security advocates.',
    'If the ceiling is textually mandated, challenges to it are violations of the treaty law itself. If the ceiling is an interpretive layer, the conditional-reciprocity reading becomes structurally coequal and treaty revision becomes legitimate. The relationship between the three readings depends on this resolution.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_vs_text_ambiguity, conceptual, 'Whether the humanitarian ceiling is mandated by the treaty text or is an expansive interpretation.').

omega_variable(
    asymmetric_enforcement_mechanism,
    'Does the asymmetric enforcement against state actors vs. non-state actors stabilize the humanitarian ceiling or undermine it by creating a two-tiered system?',
    'Empirical analysis of ICC prosecutions, domestic war crimes trials, and civil suits: count prosecutions against state actors, non-state actors, and individuals in asymmetric ratios. Compare enforcement burden and accountability outcomes across institutional levels. Examine whether the asymmetry is strategic (states have courts that can be pressured) or structural (non-state actors lack recognizable command hierarchies for accountability).',
    'If enforcement asymmetry is primarily structural and strategic (targeting state institutional capacity), the ceiling''s stability depends on maintaining state accountability mechanisms and international oversight. If enforcement asymmetry reflects indifference or inability to hold non-state actors accountable, the ceiling becomes a selective burden on state militaries alone and loses the universal coordination function. The constraint could reclassify toward snare (extraction by institutional bias) rather than tangled_rope (genuine coordination with asymmetric burden-sharing).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(asymmetric_enforcement_mechanism, empirical, 'Whether asymmetric enforcement undermines the coordination function or is intrinsic to it.').

omega_variable(
    necessity_vs_ceiling_suppression,
    'What is the true suppression mechanism the humanitarian ceiling reading operates: does it suppress security rationales through legal prohibition and institutional oversight, or does suppression persist mainly through discourse and professional ethics with limited enforcement teeth?',
    'Track documented cases where state security establishments have attempted to claim necessity derogations (enhanced interrogation programs post-9/11, targeted assassination, detention without trial, etc.), then measure the legal and institutional consequences: prosecutions, sanctions, policy reversals, or accountability gaps. A high suppression metric paired with demonstrated-but-unpunished violations suggests theater rather than structural suppression.',
    'High structural suppression with enforcement would confirm the ceiling''s power as genuine constraint. If suppression is primarily performative (violations occur, accountability is slow or absent, policy-level resistance persists), the constraint reclassifies toward piton (maintained theatrically by institutional inertia) and the effective ceiling for actual operations is lower than the authored metric suggests. The divergence between the reading''s normative claim and operational reality would be captured.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(necessity_vs_ceiling_suppression, empirical, 'Whether suppression is structural enforcement or theatrical performance.').

omega_variable(
    kernel_reading_identity,
    'Is the humanitarian ceiling reading one of three structurally coequal interpretations of an ambiguous kernel, or is it the authoritative reading that the competing readings are rebellions against?',
    'Historical and institutional analysis: examine which reading dominated international legal education, institutional practice, and norm-setting in each era (post-1949, post-1977, post-2001, contemporary). Track shifts in which reading has had controlling institutional support. The authority question determines whether the three readings are siblings or whether one is canonical with the others as persistent deviations.',
    'If the humanitarian ceiling is canonical and the others are deviations, this constraint is the principal constraint and the others are alternative framings. If the three are truly coequal and context-dependent, then the classification depends on which reading is operationalized in each theater. The framing affects whether the constraint is stable or contested across institutional levels.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Whether the humanitarian ceiling is the canonical reading or one of three coequal alternatives.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(geneva_conventions_1949__humanitarian_ceiling_reading, 1949, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gene_tr_t1949, geneva_conventions_1949__humanitarian_ceiling_reading, theater_ratio, 1949, 0.12).
narrative_ontology:measurement(gene_tr_t1975, geneva_conventions_1949__humanitarian_ceiling_reading, theater_ratio, 1975, 0.18).
narrative_ontology:measurement(gene_tr_t1995, geneva_conventions_1949__humanitarian_ceiling_reading, theater_ratio, 1995, 0.22).
narrative_ontology:measurement(gene_tr_t2011, geneva_conventions_1949__humanitarian_ceiling_reading, theater_ratio, 2011, 0.26).
narrative_ontology:measurement(gene_tr_t2020, geneva_conventions_1949__humanitarian_ceiling_reading, theater_ratio, 2020, 0.28).
narrative_ontology:measurement(gene_tr_t2026, geneva_conventions_1949__humanitarian_ceiling_reading, theater_ratio, 2026, 0.28).

% Extraction over time
narrative_ontology:measurement(gene_be_t1949, geneva_conventions_1949__humanitarian_ceiling_reading, base_extractiveness, 1949, 0.35).
narrative_ontology:measurement(gene_be_t1975, geneva_conventions_1949__humanitarian_ceiling_reading, base_extractiveness, 1975, 0.48).
narrative_ontology:measurement(gene_be_t1995, geneva_conventions_1949__humanitarian_ceiling_reading, base_extractiveness, 1995, 0.58).
narrative_ontology:measurement(gene_be_t2011, geneva_conventions_1949__humanitarian_ceiling_reading, base_extractiveness, 2011, 0.61).
narrative_ontology:measurement(gene_be_t2020, geneva_conventions_1949__humanitarian_ceiling_reading, base_extractiveness, 2020, 0.62).
narrative_ontology:measurement(gene_be_t2026, geneva_conventions_1949__humanitarian_ceiling_reading, base_extractiveness, 2026, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(gene_su_t1949, geneva_conventions_1949__humanitarian_ceiling_reading, suppression_requirement, 1949, 0.45).
narrative_ontology:measurement(gene_su_t1975, geneva_conventions_1949__humanitarian_ceiling_reading, suppression_requirement, 1975, 0.54).
narrative_ontology:measurement(gene_su_t1995, geneva_conventions_1949__humanitarian_ceiling_reading, suppression_requirement, 1995, 0.62).
narrative_ontology:measurement(gene_su_t2011, geneva_conventions_1949__humanitarian_ceiling_reading, suppression_requirement, 2011, 0.68).
narrative_ontology:measurement(gene_su_t2020, geneva_conventions_1949__humanitarian_ceiling_reading, suppression_requirement, 2020, 0.71).
narrative_ontology:measurement(gene_su_t2026, geneva_conventions_1949__humanitarian_ceiling_reading, suppression_requirement, 2026, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(geneva_conventions_1949__humanitarian_ceiling_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(geneva_conventions_1949__humanitarian_ceiling_reading, 0.12).
narrative_ontology:affects_constraint(geneva_conventions_1949__humanitarian_ceiling_reading, geneva_conventions_1949__conditional_reciprocity_reading).
narrative_ontology:affects_constraint(geneva_conventions_1949__humanitarian_ceiling_reading, geneva_conventions_1949__security_maximization_reading).
narrative_ontology:affects_constraint(geneva_conventions_1949__humanitarian_ceiling_reading, international_criminal_court__mandate_and_jurisdiction).
narrative_ontology:affects_constraint(geneva_conventions_1949__humanitarian_ceiling_reading, torture_absolute_prohibition__jus_cogens_reading).

% DUAL FORMULATION NOTE:
% The 1949 Geneva Conventions kernel admits three structurally distinct readings instantiated as separate constraints. This file models the humanitarian ceiling reading. The conditional-reciprocity and security-maximization readings are separate constraints with their own ε values, stakeholders, and classifications. All three readings compete over the same legal text; network linkages capture the family relationship and enable contamination analysis (if the ceiling reading's authority erodes, conditional-reciprocity becomes the operative framework; if security-maximization gains institutional adoption, enforcement mechanisms shift). The three constraints are not versions of each other — they are alternative institutional framings of the same kernel with different extraction profiles, suppression mechanisms, and beneficiary structures.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(geneva_conventions_1949__humanitarian_ceiling_reading, institutional, 0.32).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
