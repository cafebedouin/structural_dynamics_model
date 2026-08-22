% ============================================================================
% CONSTRAINT STORY: fourteenth_amendment_equal_protection__anti_caste_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_fourteenth_amendment_equal_protection__anti_caste_reading, []).

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
    narrative_ontology:measurement_basis/2,
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
 *   constraint_id: fourteenth_amendment_equal_protection__anti_caste_reading
 *   human_readable: Equal Protection Anti-Caste Mandate (Anti-Subordination Reading)
 *   domain: constitutional law/political philosophy/civil rights
 *
 * SUMMARY:
 *   This story authors the anti-caste reading of the Fourteenth Amendment's
 *   Equal Protection Clause: the command that the state actively dismantle
 *   racial, gender, and status hierarchy through corrective action. The ε
 *   referent is the standing arrangement under contest — the American
 *   racial/gender/status hierarchy as reproduced through formally-neutral
 *   arrangements (wealth transmission, school and neighborhood sorting,
 *   institutional default positions) — assessed by this reading's own lights,
 *   which count structural reproduction as the continuing injury. On that
 *   referent the arrangement remains deeply extractive (ε 0.80) even after de
 *   jure caste was dismantled, which is precisely the reading's warrant: the
 *   injury persists, so the corrective mandate persists. The constraint's own
 *   operation has decayed across the interval — its enforcement apparatus was
 *   built to a peak in the 1960s and has been dismantled piecewise by the
 *   courts (Croson, Adarand, Shelby County, SFFA) — while the remedial
 *   apparatus that remains has drifted toward performance. The claimed type
 *   (scaffold: a transitional support with a declared terminus) and the
 *   metrics (decayed enforcement, rising theater) are authored independently;
 *   any divergence the engine computes between them is the measurement, not
 *   an error to reconcile. KEY AGENTS (by structural relationship): see
 *   key_agents.
 *
 * KEY AGENTS:
 *   - black_americans — paradigm beneficiary (organized / identity_locked): the corrective flow lands here; the injury's burden also attaches here; neither can be exited
 *   - hierarchy_advantage_holders — diffuse payer (powerful / constrained): bears dismantlement of unearned advantage; unorganized as a class; resistance routes through the colorblindness movement
 *   - remedially_classified_individuals — individuated payer (organized / constrained): the SFFA constituency; bears legible, litigable classification costs
 *   - subordinated_group_dissenters — excluded voice (moderate / identity_locked): internal dissent the reading's warrant recharacterizes as false consciousness
 *   - federal_courts — agenda_setter (institutional / analytical): built the enforcement, now dismantles it; the seat flipped within the interval
 *   - congressional_and_agency_enforcers — agenda_setter (institutional / constrained): built and administered the machinery; now defends remnants
 *   - civil_rights_advocacy_organizations — agenda_setter with beneficiary position (organized / identity_locked): enforces the program, collects fee-shifted resources, and cannot survive mission-completion
 *   - constitutional_scholars — analytical observer (analytical / analytical): maps the lineage and constitutes the reading's post-judicial intellectual life
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fourteenth_amendment_equal_protection__anti_caste_reading, 0.8).
domain_priors:suppression_score(fourteenth_amendment_equal_protection__anti_caste_reading, 0.15).
domain_priors:theater_ratio(fourteenth_amendment_equal_protection__anti_caste_reading, 0.66).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fourteenth_amendment_equal_protection__anti_caste_reading, extractiveness, 0.8).
narrative_ontology:constraint_metric(fourteenth_amendment_equal_protection__anti_caste_reading, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(fourteenth_amendment_equal_protection__anti_caste_reading, theater_ratio, 0.66).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(fourteenth_amendment_equal_protection__anti_caste_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(fourteenth_amendment_equal_protection__anti_caste_reading, resistance, 0.85).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fourteenth_amendment_equal_protection__anti_caste_reading, scaffold).
narrative_ontology:human_readable(fourteenth_amendment_equal_protection__anti_caste_reading, "Equal Protection Anti-Caste Mandate (Anti-Subordination Reading)").
narrative_ontology:topic_domain(fourteenth_amendment_equal_protection__anti_caste_reading, "constitutional law/political philosophy/civil rights").

domain_priors:requires_active_enforcement(fourteenth_amendment_equal_protection__anti_caste_reading).
narrative_ontology:has_sunset_clause(fourteenth_amendment_equal_protection__anti_caste_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(fourteenth_amendment_equal_protection__anti_caste_reading, 'f2888f84-33fd-4da1-8eff-a546bf5d90bb').
narrative_ontology:cs_kernel_codification('f2888f84-33fd-4da1-8eff-a546bf5d90bb', fixed_text).
narrative_ontology:cs_authority_grounding('f2888f84-33fd-4da1-8eff-a546bf5d90bb', lineage).
narrative_ontology:cs_interpretation_layer_present('f2888f84-33fd-4da1-8eff-a546bf5d90bb').
narrative_ontology:cs_reading_relation('f2888f84-33fd-4da1-8eff-a546bf5d90bb', fourteenth_amendment_equal_protection__formal_equality_reading, forecloses).
narrative_ontology:cs_axiom('f2888f84-33fd-4da1-8eff-a546bf5d90bb', foundational, anti_subordination_is_the_command).
narrative_ontology:cs_axiom_status(anti_subordination_is_the_command, holdable).
narrative_ontology:cs_axiom_grounding('f2888f84-33fd-4da1-8eff-a546bf5d90bb', anti_subordination_is_the_command, deontological).
narrative_ontology:cs_axiom('f2888f84-33fd-4da1-8eff-a546bf5d90bb', foundational, state_neutrality_can_entrench_caste).
narrative_ontology:cs_axiom_status(state_neutrality_can_entrench_caste, holdable).
narrative_ontology:cs_axiom_grounding('f2888f84-33fd-4da1-8eff-a546bf5d90bb', state_neutrality_can_entrench_caste, empirically_contingent).
narrative_ontology:cs_axiom('f2888f84-33fd-4da1-8eff-a546bf5d90bb', secondary, remedial_classifications_are_not_invidious).
narrative_ontology:cs_axiom_status(remedial_classifications_are_not_invidious, holdable).
narrative_ontology:cs_axiom_grounding('f2888f84-33fd-4da1-8eff-a546bf5d90bb', remedial_classifications_are_not_invidious, instrumental).
narrative_ontology:cs_reference_frame('f2888f84-33fd-4da1-8eff-a546bf5d90bb', reconstruction_anti_caste_mandate).
narrative_ontology:cs_drift_state('f2888f84-33fd-4da1-8eff-a546bf5d90bb', contemporary_post_sffa_era, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('f2888f84-33fd-4da1-8eff-a546bf5d90bb', '').
narrative_ontology:cs_kernel_id(fourteenth_amendment_equal_protection__anti_caste_reading, fourteenth_amendment_equal_protection).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fourteenth_amendment_equal_protection__anti_caste_reading, black_americans).
narrative_ontology:constraint_beneficiary(fourteenth_amendment_equal_protection__anti_caste_reading, women).
narrative_ontology:constraint_beneficiary(fourteenth_amendment_equal_protection__anti_caste_reading, status_subordinated_minorities).
narrative_ontology:constraint_victim(fourteenth_amendment_equal_protection__anti_caste_reading, hierarchy_advantage_holders).
narrative_ontology:constraint_victim(fourteenth_amendment_equal_protection__anti_caste_reading, remedially_classified_individuals).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(fourteenth_amendment_equal_protection__anti_caste_reading, civil_rights_advocacy_organizations).
narrative_ontology:constraint_vindicates(fourteenth_amendment_equal_protection__anti_caste_reading, anti_subordination_principle).
narrative_ontology:constraint_vindicates(fourteenth_amendment_equal_protection__anti_caste_reading, section_five_enforcement_power).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Descendants of enslaved and Jim Crow-subordinated citizens; the reading's paradigm protected class. They receive the corrective flow — desegregation orders, voting-rights remedies, admissions and contracting consideration — while continuing to bear the standing hierarchy's burdens: the median racial wealth gap, school and neighborhood isolation, disparate policing. They cannot exit the racial category; both the remedy's warrant and the injury's burden attach to an identity that cannot be shed.
narrative_ontology:constraint_stakeholder(fourteenth_amendment_equal_protection__anti_caste_reading, black_americans, beneficiary,
    organized, generational, identity_locked, national).

% Receive the anti-caste logic's extension to gender: heightened scrutiny, Title IX enforcement, and sex-conscious corrective programs trace to the same warrant. The seat is internally heterogeneous — women positioned at racial or class intersections carry compounded subordination that single-axis remedies reach unevenly. Gender, like race, cannot be exited.
narrative_ontology:constraint_stakeholder(fourteenth_amendment_equal_protection__anti_caste_reading, women, beneficiary,
    organized, generational, identity_locked, national).

% Groups subordinated by ethnicity, religion, disability, or sexuality that the anti-caste logic reaches case by case. Some dimensions of their subordination are partially exitable (concealment, passing, class mobility) and some are not; the corrective flow reaches them less systematically than the racial and gender cores.
narrative_ontology:constraint_stakeholder(fourteenth_amendment_equal_protection__anti_caste_reading, status_subordinated_minorities, beneficiary,
    moderate, biographical, constrained, national).

% The diffuse population whose unearned advantages — network access, wealth transmission, institutional default positioning, neighborhood and school quality — the corrective program works to dismantle. Their costs are real but mostly invisible to them as costs: a displaced positional advantage rarely announces itself. They are not organized as a self-conscious class; their resistance operates through the colorblindness movement and through ordinary politics.
narrative_ontology:constraint_stakeholder(fourteenth_amendment_equal_protection__anti_caste_reading, hierarchy_advantage_holders, payer,
    powerful, generational, constrained, national).

% Individuals outside the protected classes who bear race-conscious classification directly: Asian-American applicants weighed in holistic admissions, employees passed over under consent-decree preferences, contractors excluded by set-asides. Their costs are individuated and legible — a specific denial with a specific causal claim — which is what made them organizable into the litigation vehicle that won Students for Fair Admissions.
narrative_ontology:constraint_stakeholder(fourteenth_amendment_equal_protection__anti_caste_reading, remedially_classified_individuals, payer,
    organized, biographical, constrained, national).

% Members of the protected classes who reject the corrective project itself — Justice Thomas is the paradigm. The reading structurally excludes their objection: within the anti-caste framework, their dissent is read as adaptation to the hierarchy rather than as a rival claim on the Clause's meaning. They hold voice in public discourse but no standing inside the warrant.
narrative_ontology:constraint_stakeholder(fourteenth_amendment_equal_protection__anti_caste_reading, subordinated_group_dissenters, excluded,
    moderate, biographical, identity_locked, national).

% Adjudicate when the corrective warrant attaches, design the remedies, and police their limits. The seat's relationship to the program inverted inside the interval: the Warren Court built and enforced it; the current Court has dismantled its principal instruments (Croson, Adarand, Shelby County, Students for Fair Admissions). The same institution experiences the program as its own creation and then as its own error.
narrative_ontology:constraint_stakeholder(fourteenth_amendment_equal_protection__anti_caste_reading, federal_courts, agenda_setter,
    institutional, generational, analytical, national).

% Congress under Section Five and the civil rights agencies (DOJ, EEOC, Education OCR) built and administered the enforcement machinery: preclearance, desegregation orders, disparate-impact regulations. Their capacity has been curtailed by the same doctrinal sequence and their current agenda is largely defensive — preserving remnants against further contraction.
narrative_ontology:constraint_stakeholder(fourteenth_amendment_equal_protection__anti_caste_reading, congressional_and_agency_enforcers, agenda_setter,
    institutional, generational, constrained, national).

% Litigate and organize to enforce the corrective program (LDF, MALDEF, ACLU and successors). Their funding, standing, and institutional mission are fused with the program's continuation; mission-completion would dissolve them. They also collect a documented share of the flow directly through fee-shifting under 42 U.S.C. § 1988.
narrative_ontology:constraint_stakeholder(fourteenth_amendment_equal_protection__anti_caste_reading, civil_rights_advocacy_organizations, agenda_setter,
    organized, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(fourteenth_amendment_equal_protection__anti_caste_reading, civil_rights_advocacy_organizations, beneficiary).

% The analytical seat: they map the reading's lineage (Karst, Fiss, Siegel), track its doctrinal fortunes, and constitute much of the intellectual life the reading retains after judicial repudiation. They collect nothing and pay nothing; they describe.
narrative_ontology:constraint_stakeholder(fourteenth_amendment_equal_protection__anti_caste_reading, constitutional_scholars, observer,
    analytical, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(fourteenth_amendment_equal_protection__anti_caste_reading, black_americans).
narrative_ontology:fixing_cost_class(fourteenth_amendment_equal_protection__anti_caste_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves a collective-action problem no private actor can: structural hierarchy — segregated wealth, schools, networks, institutional default positions — reproduces itself through formally-private transactions and formally-neutral institutions, and dismantling it requires coordinated, sustained state corrective action (litigation, legislation, administrative enforcement, remedial design) that no individual, market, or locality can supply.
% TRANSFER_FUNCTION: Moves institutional access, life chances, political power, and remedial resources toward racially, gender-, and status-subordinated groups, and moves away from holders of hierarchy-derived advantage — through desegregation orders, voting-rights remedies, set-asides and preferences, consent decrees, and fee-shifted enforcement resources.
% ABSENT_VOICES: Three seats are structurally absent or silenced. First, the diffuse dominant-group majority bears the program's costs without organized voice at the remedial design table — remedies are designed by courts, agencies, and advocacy organizations, not by those whose advantages are dismantled. Second, subordinated-group dissenters (the Justice Thomas seat) hold public voice but are excluded from the reading's own warrant, which recharacterizes their objection as false consciousness. Third, the formerly enslaved and their immediate descendants, whose injury the founding warrant answered, are no longer present to attest whether the remedy answers their claim — the warrant now rests on inherited attestation.
% DISAPPEARANCE_RATIONALE: If the anti-caste reading vanished overnight, the remaining school desegregation decrees would terminate, Section 2 vote-dilution litigation would lose its remedial anchor, disparate-impact theory would lose its constitutional foothold, and the formally-neutral reproduction of the standing hierarchy would proceed without constitutional check — the arrangement the reading targets would rearrange around its absence.
% FOUNDING_PROBLEM: The Fourteenth Amendment was ratified to answer the Black Codes: the former Confederate states re-subordinating freedpeople through law immediately after abolition — state-sponsored caste as the founding injury.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: the Federal Reserve Survey of Consumer Finances wealth-gap series (persistent roughly 8-10:1 median gap), UCLA Civil Rights Project school-segregation data (Black student isolation now exceeds 1970 intensity in many metros), and HUD paired-audit housing discrimination studies. Decisively, the colorblindness movement's own briefs and the SFFA majority concede persistent disparity while disputing the constitutional warrant for remedial classification — adversaries attest the injury while contesting the remedy.
narrative_ontology:disappearance_verdict(fourteenth_amendment_equal_protection__anti_caste_reading, world_rearranges).
narrative_ontology:founding_problem_status(fourteenth_amendment_equal_protection__anti_caste_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(fourteenth_amendment_equal_protection__anti_caste_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(fourteenth_amendment_equal_protection__anti_caste_reading, 'none', 1).
narrative_ontology:epsilon_provenance(fourteenth_amendment_equal_protection__anti_caste_reading, 0.8, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(fourteenth_amendment_equal_protection__anti_caste_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(fourteenth_amendment_equal_protection__anti_caste_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(fourteenth_amendment_equal_protection__anti_caste_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.80) is authored on the reading's referent — the standing hierarchy — not on the remedial programs: the reading's defining claim is that formally-neutral reproduction of caste is the injury, and the disparity series (wealth, segregation, policing) supports a high assessment that dipped only modestly after de jure dismantlement and ticked upward after 2008. Suppression (0.15) is the constraint's own coercive force, which has decayed with its enforcement apparatus; suppression is a raw structural property, unscaled by power or scope — the standing hierarchy's suppressiveness lives in the beneficiaries' exit positions (identity_locked), not in this scalar. Theater (0.66) is the honest Goodhart signal: the apparatus increasingly performs anti-caste commitment (diversity statements, offices, commemorations) while the functional core (decrees, Section 2 litigation) shrinks — a symptom of repudiation, not yet the constraint's whole operation. Accessibility collapse (0.35): the colorblind alternative remains fully live and institutionally dominant; this reading forecloses it logically, not practically. Resistance (0.85): organized, funded, and victorious — SFFA is resistance winning. The series run on one shared grid (T = year − 1954): extractiveness of the target stays high while the constraint's enforcement decays monotonically and theater rises monotonically — the signature of a transitional program whose transition stalled and whose apparatus professionalized. No cyclical dynamics: the drift is monotone.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the beneficiary seats compute differently: from hierarchy_advantage_holders and remedially_classified_individuals the program, when operative, takes positional advantage and imposes classifications — it is experienced as extraction. From black_americans and the advocacy organizations it is subsidy and warrant. The agenda-setter seat flipped within the interval: federal_courts moved from enforcer to repudiator, so the same institution experiences the program as its own creation and then as its own error. The engine computes this divergence from the structural data; the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   The beneficiary declarations put black_americans, women, and status_subordinated_minorities near the full-beneficiary end (d near 0): the corrective flow subsidizes them and they cannot exit the identity the warrant attaches to. The victim declarations put hierarchy_advantage_holders near the full-target end — their costs are identity-attached, diffuse, and their exit is constrained — and remedially_classified_individuals similarly high, with the amplification that their costs are individuated and litigable, which is what organized them into an effective litigation seat. The advocacy organizations derive low d from their beneficiary position; their identity-lock shapes persistence, not directionality, so no override is needed. The courts' d is seat-relative and flipped with their doctrine; role plus exit derives the correct value. No directionality_overrides are authored: the derivation chain from declared roles and exit positions produces the correct d for every seat.
 *
 * MANDATROPHY ANALYSIS:
 *   The scaffold claim prevents mislabeling in both directions. Against a snare reading: the payer seats' extraction experience is real, but the program's gains land on the subordinated rather than on a self-dealing capturer, and the reading's own tradition declares a terminus — Grutter's 25-year expectation is a literal declared sunset, and the remedial rationale's warrant is injury-contingent throughout. A snare has no declared terminus. Against a rope reading: this is not steady-state coordination — its justification is the transition, and it carries real coercive overhead and asymmetric costs. The mandatrophy risk is genuine and rising: the theater series (0.10 to 0.66) is the classic drift signature of a transitional program whose transition stalled while its apparatus professionalized, and the enforcement apparatus's identity-lock (advocacy organizations whose survival requires the mission, compliance bureaucracies built on it) would resist conceding mission-completion if the injury closed. Currently the R5 mismatch does not fire: founding_problem_status=live is corroborated by adversaries' concessions, so the apparatus's persistence tracks a real injury — but the theater series marks exactly where mandatrophy would incubate if the injury closed while the apparatus persisted.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indexicality,
    'This constraint is one reading of kernel fourteenth_amendment_equal_protection — the anti_caste_reading. What would the sibling reading (formal_equality_reading) change structurally, and where exactly is the disagreement located?',
    'The sibling is authored as its own constraint story with its own ε, its own beneficiary structure (it protects the unclassified rather than the subordinated), and its own type; cross-reading comparison is valid only across the two files. The disagreement''s location is the axiom state_neutrality_can_entrench_caste, recorded in cs_structure for both readings.',
    'If the sibling''s ε were authored inside this file, ε-invariance would break: the standing arrangement''s extractiveness would oscillate with the observable chosen (hierarchy-measure versus classification-count), and the classification would become observer-relative — exactly the failure the decomposition rule exists to prevent.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_indexicality, conceptual, 'Committer structure: one kernel, two readings, one ε per reading; the sibling is a different constraint, not a measurement parameter of this one.').

omega_variable(
    remedial_warrant_terminus,
    'Is the corrective warrant genuinely transitional (a support structure with a real terminus) or an effectively permanent standing mandate? The reading declares a sunset — Grutter''s 25-year expectation, the injury-contingent remedial rationale — but no mechanism enforces the terminus, and structural disparity may be self-reproducing indefinitely.',
    'Longitudinal disparity data: if the measured hierarchy converges, the terminus arrives and the transitional warrant dissolves with the injury; if disparity is self-reproducing without bound, the warrant is permanent and the constraint re-reads as a standing coordination regime rather than a transitional one.',
    'The declared sunset''s honesty is the pivot between a transitional reading and a steady-state reading; resolving it changes the type the structural data support and whether the program''s persistence past its declaration counts as drift or as fidelity.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(remedial_warrant_terminus, empirical, 'Whether the anti-caste mandate''s declared terminus is real or aspirational.').

omega_variable(
    corrective_efficacy,
    'Does state corrective action actually dismantle structural hierarchy, or does it redistribute position within it — or entrench classification itself as a political resource? The reading''s instrumental premise (corrective action works at structural scale) is empirically contingent and largely untested.',
    'Natural experiments: court-ordered desegregation cohorts, preclearance jurisdictions before and after Shelby County, lottery-based admissions designs — measuring downstream wealth, segregation, and political-power trajectories against matched controls.',
    'If corrective action is ineffective, the warrant collapses to pure deontological commitment and the payers bear costs without the transition the warrant promises — the constraint''s justification fails on its own instrumental terms even if the deontological core survives.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(corrective_efficacy, empirical, 'Whether the remedial instrument achieves the dismantlement that legitimates it.').

omega_variable(
    remedial_capture_within_beneficiary_set,
    'Who within the beneficiary groups actually captures the corrective flow — the most subordinated members, or the best-positioned (class-stratified capture, the ''rich Black kid'' problem)?',
    'Beneficiary-composition audits of preference programs — admissions, contracting, court-ordered remedies — stratified by class within group, tracking which decile of the beneficiary population receives the flow.',
    'If gains concentrate in the best-positioned members, the most subordinated bear the hierarchy without receiving the remedy: the receipt surface degrades toward capture by an elite fraction of the beneficiary set, and the constraint''s effective operation inverts for its paradigm claimants.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(remedial_capture_within_beneficiary_set, empirical, 'Class-stratified capture of remedial gains within the beneficiary groups.').

omega_variable(
    enforcement_decay_vs_mission_completion,
    'Is the enforcement decay (suppression_requirement 0.90 to 0.15) decay under repudiation, or mission completion — has the hierarchy actually closed and the apparatus is winding down?',
    'The disparity series cross-checked against the theater series: mission completion predicts theater falling as the apparatus dissolves; repudiation predicts theater rising as function shrinks while performance persists (the observed pattern).',
    'If the decay reflects repudiation rather than completion, the standing arrangement''s high extractiveness persists unaddressed and the reading''s custodians face a revival project rather than obsolescence; if completion, the remaining apparatus is residual and the theater ratio marks genuine wind-down.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(enforcement_decay_vs_mission_completion, empirical, 'Whether the enforcement collapse is suppression of the program or completion of it.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fourteenth_amendment_equal_protection__anti_caste_reading, 0, 70).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(four_tr_t0, fourteenth_amendment_equal_protection__anti_caste_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement_basis(four_tr_t0, observed).
narrative_ontology:measurement(four_tr_t10, fourteenth_amendment_equal_protection__anti_caste_reading, theater_ratio, 10, 0.15).
narrative_ontology:measurement_basis(four_tr_t10, observed).
narrative_ontology:measurement(four_tr_t24, fourteenth_amendment_equal_protection__anti_caste_reading, theater_ratio, 24, 0.3).
narrative_ontology:measurement_basis(four_tr_t24, observed).
narrative_ontology:measurement(four_tr_t35, fourteenth_amendment_equal_protection__anti_caste_reading, theater_ratio, 35, 0.4).
narrative_ontology:measurement_basis(four_tr_t35, observed).
narrative_ontology:measurement(four_tr_t41, fourteenth_amendment_equal_protection__anti_caste_reading, theater_ratio, 41, 0.45).
narrative_ontology:measurement_basis(four_tr_t41, observed).
narrative_ontology:measurement(four_tr_t59, fourteenth_amendment_equal_protection__anti_caste_reading, theater_ratio, 59, 0.55).
narrative_ontology:measurement_basis(four_tr_t59, observed).
narrative_ontology:measurement(four_tr_t69, fourteenth_amendment_equal_protection__anti_caste_reading, theater_ratio, 69, 0.65).
narrative_ontology:measurement_basis(four_tr_t69, observed).
narrative_ontology:measurement(four_tr_t70, fourteenth_amendment_equal_protection__anti_caste_reading, theater_ratio, 70, 0.66).
narrative_ontology:measurement_basis(four_tr_t70, observed).

% Extraction over time
narrative_ontology:measurement(four_be_t0, fourteenth_amendment_equal_protection__anti_caste_reading, base_extractiveness, 0, 0.95).
narrative_ontology:measurement_basis(four_be_t0, observed).
narrative_ontology:measurement(four_be_t10, fourteenth_amendment_equal_protection__anti_caste_reading, base_extractiveness, 10, 0.88).
narrative_ontology:measurement_basis(four_be_t10, observed).
narrative_ontology:measurement(four_be_t24, fourteenth_amendment_equal_protection__anti_caste_reading, base_extractiveness, 24, 0.8).
narrative_ontology:measurement_basis(four_be_t24, observed).
narrative_ontology:measurement(four_be_t35, fourteenth_amendment_equal_protection__anti_caste_reading, base_extractiveness, 35, 0.78).
narrative_ontology:measurement_basis(four_be_t35, observed).
narrative_ontology:measurement(four_be_t41, fourteenth_amendment_equal_protection__anti_caste_reading, base_extractiveness, 41, 0.77).
narrative_ontology:measurement_basis(four_be_t41, observed).
narrative_ontology:measurement(four_be_t59, fourteenth_amendment_equal_protection__anti_caste_reading, base_extractiveness, 59, 0.75).
narrative_ontology:measurement_basis(four_be_t59, observed).
narrative_ontology:measurement(four_be_t69, fourteenth_amendment_equal_protection__anti_caste_reading, base_extractiveness, 69, 0.79).
narrative_ontology:measurement_basis(four_be_t69, observed).
narrative_ontology:measurement(four_be_t70, fourteenth_amendment_equal_protection__anti_caste_reading, base_extractiveness, 70, 0.8).
narrative_ontology:measurement_basis(four_be_t70, observed).

% Suppression requirement over time
narrative_ontology:measurement(four_su_t0, fourteenth_amendment_equal_protection__anti_caste_reading, suppression_requirement, 0, 0.9).
narrative_ontology:measurement_basis(four_su_t0, observed).
narrative_ontology:measurement(four_su_t10, fourteenth_amendment_equal_protection__anti_caste_reading, suppression_requirement, 10, 0.85).
narrative_ontology:measurement_basis(four_su_t10, observed).
narrative_ontology:measurement(four_su_t24, fourteenth_amendment_equal_protection__anti_caste_reading, suppression_requirement, 24, 0.7).
narrative_ontology:measurement_basis(four_su_t24, observed).
narrative_ontology:measurement(four_su_t35, fourteenth_amendment_equal_protection__anti_caste_reading, suppression_requirement, 35, 0.5).
narrative_ontology:measurement_basis(four_su_t35, observed).
narrative_ontology:measurement(four_su_t41, fourteenth_amendment_equal_protection__anti_caste_reading, suppression_requirement, 41, 0.42).
narrative_ontology:measurement_basis(four_su_t41, observed).
narrative_ontology:measurement(four_su_t59, fourteenth_amendment_equal_protection__anti_caste_reading, suppression_requirement, 59, 0.28).
narrative_ontology:measurement_basis(four_su_t59, observed).
narrative_ontology:measurement(four_su_t69, fourteenth_amendment_equal_protection__anti_caste_reading, suppression_requirement, 69, 0.16).
narrative_ontology:measurement_basis(four_su_t69, observed).
narrative_ontology:measurement(four_su_t70, fourteenth_amendment_equal_protection__anti_caste_reading, suppression_requirement, 70, 0.15).
narrative_ontology:measurement_basis(four_su_t70, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fourteenth_amendment_equal_protection__anti_caste_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(fourteenth_amendment_equal_protection__anti_caste_reading, formal_equality_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'Equal Protection' conflates two structurally distinct claims about what the Clause commands. This story authors the anti-caste reading (command: anti-subordination; ε referent: the standing structural hierarchy, assessed at 0.80); the sibling formal_equality_reading authors the colorblind command (ε referent: racial classification itself, with the standing arrangement treated as largely unobjectionable). The ε difference follows from the axiom split recorded in cs_structure: this reading holds state_neutrality_can_entrench_caste foundational; the sibling denies it. The readings foreclose one another within any single legal framework; both files link via affects_constraints per the ε-invariance decomposition rule.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
