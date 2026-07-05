% ============================================================================
% CONSTRAINT STORY: fourteenth_amendment_equal_protection__anti_caste_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   human_readable: Equal Protection as Anti-Caste Mandate (Affirmative State Corrective Action)
 *   domain: constitutional_law/civil_rights/political_philosophy
 *
 * SUMMARY:
 *   This story instantiates the anti-caste reading of the Equal Protection
 *   Clause: the claim that Equal Protection is not satisfied by mere formal
 *   neutrality but requires the state to actively dismantle racial, gender,
 *   and status hierarchy, including through race- and status-conscious
 *   corrective action. This reading emerged from Reconstruction-era
 *   anti-subordination theory, gained doctrinal traction through Brown-era
 *   desegregation remedies and disparate-impact jurisprudence, and remains a
 *   live, contested position within constitutional law and political
 *   philosophy — held by scholars, civil rights litigators, and some
 *   doctrinal lines of case law, and opposed by formal-equality proponents
 *   who read the same clause as prohibiting state racial classification
 *   generally. This is one of two structurally distinct constraints sharing
 *   the label 'Equal Protection' — its sibling, the formal_equality_reading,
 *   has a different beneficiary structure (no subordinated-group beneficiary
 *   class), a different victim structure, and a lower extraction profile
 *   because it does not authorize affirmative remedial transfer. The two are
 *   linked as siblings in a shared kernel, not two measurements of one
 *   constraint.
 *
 * KEY AGENTS:
 *   - racial_minority_communities: primary beneficiary (organized/constrained) — the reading's intended remedial object
 *   - civil_rights_enforcement_agencies: agenda_setter (institutional/arbitrage) — administers and litigates the doctrine
 *   - nonminority_applicants_in_targeted_domains: primary payer (moderate/constrained) — bears concentrated individual cost of remedial allocation
 *   - state_actors_facing_compliance_liability: institutional payer (institutional/trapped) — bears compliance and litigation burden
 *   - formal_equality_advocates: excluded voice (organized/mobile) — competing doctrinal claim structurally subordinated within this reading
 *   - constitutional_scholars: analytical observer (analytical/analytical) — traces doctrinal lineage and contestation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fourteenth_amendment_equal_protection__anti_caste_reading, 0.58).
domain_priors:suppression_score(fourteenth_amendment_equal_protection__anti_caste_reading, 0.42).
domain_priors:theater_ratio(fourteenth_amendment_equal_protection__anti_caste_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fourteenth_amendment_equal_protection__anti_caste_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(fourteenth_amendment_equal_protection__anti_caste_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(fourteenth_amendment_equal_protection__anti_caste_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(fourteenth_amendment_equal_protection__anti_caste_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(fourteenth_amendment_equal_protection__anti_caste_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fourteenth_amendment_equal_protection__anti_caste_reading, tangled_rope).
narrative_ontology:human_readable(fourteenth_amendment_equal_protection__anti_caste_reading, "Equal Protection as Anti-Caste Mandate (Affirmative State Corrective Action)").
narrative_ontology:topic_domain(fourteenth_amendment_equal_protection__anti_caste_reading, "constitutional_law/civil_rights/political_philosophy").

domain_priors:requires_active_enforcement(fourteenth_amendment_equal_protection__anti_caste_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(fourteenth_amendment_equal_protection__anti_caste_reading, 'bf20e4ec-3992-4930-9f39-d1aad2c48b7f').
narrative_ontology:cs_kernel_codification('bf20e4ec-3992-4930-9f39-d1aad2c48b7f', fixed_text).
narrative_ontology:cs_authority_grounding('bf20e4ec-3992-4930-9f39-d1aad2c48b7f', lineage).
narrative_ontology:cs_interpretation_layer_present('bf20e4ec-3992-4930-9f39-d1aad2c48b7f').
narrative_ontology:cs_reading_relation('bf20e4ec-3992-4930-9f39-d1aad2c48b7f', fourteenth_amendment_equal_protection__formal_equality_reading, coexists_with).
narrative_ontology:cs_axiom('bf20e4ec-3992-4930-9f39-d1aad2c48b7f', foundational, equal_protection_requires_affirmative_correction).
narrative_ontology:cs_axiom_status(equal_protection_requires_affirmative_correction, holdable).
narrative_ontology:cs_axiom_grounding('bf20e4ec-3992-4930-9f39-d1aad2c48b7f', equal_protection_requires_affirmative_correction, deontological).
narrative_ontology:cs_axiom('bf20e4ec-3992-4930-9f39-d1aad2c48b7f', secondary, facial_neutrality_insufficient_absent_disparate_impact_remedy).
narrative_ontology:cs_axiom_status(facial_neutrality_insufficient_absent_disparate_impact_remedy, holdable).
narrative_ontology:cs_axiom_grounding('bf20e4ec-3992-4930-9f39-d1aad2c48b7f', facial_neutrality_insufficient_absent_disparate_impact_remedy, instrumental).
narrative_ontology:cs_reference_frame('bf20e4ec-3992-4930-9f39-d1aad2c48b7f', reconstruction_anti_subordination_purpose).
narrative_ontology:cs_drift_state('bf20e4ec-3992-4930-9f39-d1aad2c48b7f', post_1990s_strict_scrutiny_narrowing, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('bf20e4ec-3992-4930-9f39-d1aad2c48b7f', '').
narrative_ontology:cs_kernel_id(fourteenth_amendment_equal_protection__anti_caste_reading, fourteenth_amendment_equal_protection).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fourteenth_amendment_equal_protection__anti_caste_reading, racial_minority_communities).
narrative_ontology:constraint_beneficiary(fourteenth_amendment_equal_protection__anti_caste_reading, women_seeking_remedial_programs).
narrative_ontology:constraint_beneficiary(fourteenth_amendment_equal_protection__anti_caste_reading, historically_excluded_status_groups).
narrative_ontology:constraint_beneficiary(fourteenth_amendment_equal_protection__anti_caste_reading, civil_rights_enforcement_agencies).
narrative_ontology:constraint_victim(fourteenth_amendment_equal_protection__anti_caste_reading, nonminority_applicants_in_targeted_domains).
narrative_ontology:constraint_victim(fourteenth_amendment_equal_protection__anti_caste_reading, state_actors_facing_compliance_liability).
narrative_ontology:constraint_victim(fourteenth_amendment_equal_protection__anti_caste_reading, municipalities_bearing_remediation_costs).
narrative_ontology:constraint_vindicates(fourteenth_amendment_equal_protection__anti_caste_reading, anti_subordination_principle).
narrative_ontology:constraint_vindicates(fourteenth_amendment_equal_protection__anti_caste_reading, substantive_equality_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Historically excluded from housing, employment, education, and political power by explicit and structural mechanisms. Under this reading, they are the intended object of affirmative state correction: set-asides, districting remedies, disparate-impact liability, and compensatory admissions policies. Their exit from the constraint's protective ambit means returning to a formal-equality regime that, in their view, freezes existing hierarchy in place.
narrative_ontology:constraint_stakeholder(fourteenth_amendment_equal_protection__anti_caste_reading, racial_minority_communities, beneficiary,
    organized, generational, constrained, national).

% Benefit from sex-conscious remedial measures (targeted hiring, funding parity enforcement, disparate-impact claims) justified by this reading's premise that formal neutrality perpetuates status hierarchy. Exit means losing legal grounds to challenge facially neutral practices with disparate downstream effects.
narrative_ontology:constraint_stakeholder(fourteenth_amendment_equal_protection__anti_caste_reading, women_seeking_remedial_programs, beneficiary,
    organized, generational, constrained, national).

% Includes groups organized around disability, language-minority status, or other status markers historically treated as caste-adjacent. Gain standing to argue that facially neutral rules producing hierarchical outcomes violate Equal Protection, not merely rules that explicitly classify by status.
narrative_ontology:constraint_stakeholder(fourteenth_amendment_equal_protection__anti_caste_reading, historically_excluded_status_groups, beneficiary,
    moderate, generational, constrained, national).

% Federal and state civil rights offices, courts applying disparate-impact and remedial doctrine, and legislatures authorizing corrective programs. They administer and enforce this reading by promulgating rules, litigating claims, and mandating remedial compliance. Their institutional mandate and budget expand under this reading; they could narrow enforcement but bear no comparable cost from maintaining it.
narrative_ontology:constraint_stakeholder(fourteenth_amendment_equal_protection__anti_caste_reading, civil_rights_enforcement_agencies, agenda_setter,
    institutional, generational, arbitrage, national).

% Individuals who lose admissions slots, contracts, or hiring preferences to remedial allocations justified by this reading's anti-subordination logic. They bear a concentrated individual cost for a diffuse structural remedy; their only recourse is litigation challenging the remedial program itself, which this reading's doctrine is built to withstand.
narrative_ontology:constraint_stakeholder(fourteenth_amendment_equal_protection__anti_caste_reading, nonminority_applicants_in_targeted_domains, payer,
    moderate, biographical, constrained, national).

% Municipal employers, school districts, and licensing bodies must design, defend, and continually justify remedial classifications against constitutional challenge, disparate-impact audits, and shifting doctrinal standards. Exit is not available while the anti-caste reading governs; noncompliance risks litigation and loss of federal funding.
narrative_ontology:constraint_stakeholder(fourteenth_amendment_equal_protection__anti_caste_reading, state_actors_facing_compliance_liability, payer,
    institutional, biographical, trapped, regional).

% Bear the direct fiscal and administrative cost of remedial districting, compliance monitoring, and consent-decree obligations imposed to correct historical hierarchy. Costs are borne locally even where the underlying hierarchy was produced or tolerated at higher jurisdictional levels.
narrative_ontology:constraint_stakeholder(fourteenth_amendment_equal_protection__anti_caste_reading, municipalities_bearing_remediation_costs, payer,
    moderate, generational, trapped, regional).

% Hold that any state action classifying by race or status — remedial or not — reproduces the harm Equal Protection was meant to end. They are not absent from public debate but are structurally excluded from this reading's own doctrinal framework, which treats their objection as a symptom of the hierarchy needing correction rather than a competing constitutional claim.
narrative_ontology:constraint_stakeholder(fourteenth_amendment_equal_protection__anti_caste_reading, formal_equality_advocates, excluded,
    organized, generational, mobile, national).

% Analyze how anti-subordination doctrine has moved through Reconstruction-era statutes, Brown-era desegregation remedies, and disparate-impact jurisprudence, tracking where the reading has advanced, retrenched, or been formally narrowed by later courts.
narrative_ontology:constraint_stakeholder(fourteenth_amendment_equal_protection__anti_caste_reading, constitutional_scholars, observer,
    analytical, civilizational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(fourteenth_amendment_equal_protection__anti_caste_reading, diffuse).
narrative_ontology:fixing_cost_class(fourteenth_amendment_equal_protection__anti_caste_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates state institutions around a shared obligation to identify and dismantle group-based hierarchy, rather than merely refrain from creating new classifications — solving the problem that facially neutral rules can perpetuate historical subordination without any single actor being formally at fault.
% TRANSFER_FUNCTION: Moves institutional resources, admissions and employment slots, contract allocations, and legal standing from groups and individuals occupying advantaged positions under existing arrangements to groups and individuals identified as historically subordinated, administered through courts, agencies, and legislatures.
% ABSENT_VOICES: Formal-equality advocates and individuals disadvantaged by specific remedial allocations are present in litigation but structurally excluded from shaping the doctrine's own premises — the anti-caste reading treats their objection as evidence of unresolved hierarchy rather than as a competing constitutional principle entitled to equal doctrinal footing.
% DISAPPEARANCE_RATIONALE: If this reading disappeared overnight in favor of a purely formal-equality regime, affirmative remedial programs, disparate-impact liability, and race- or sex-conscious corrective measures would lose their constitutional grounding; enforcement agencies would lose litigation tools, remedial admissions and contracting programs would face immediate legal exposure, and subordinated groups would lose a primary avenue for challenging facially neutral practices with unequal effects.
% FOUNDING_PROBLEM: Formal legal equality after Reconstruction and again after the civil rights era proved insufficient to dismantle entrenched racial, gender, and status hierarchy — facially neutral rules (literacy tests, seniority systems, zoning, at-large elections) reproduced subordination without any explicit classification triggering scrutiny.
% FOUNDING_PROBLEM_CORROBORATION: Civil rights enforcement agencies and beneficiary groups attest the founding problem remains live, citing persistent disparate outcomes in wealth, incarceration, and political representation. Independent empirical work by economists and sociologists outside the enforcement apparatus (labor market audit studies, residential segregation research) corroborates persistent structural disparity, though it does not independently corroborate that state-administered corrective classification is the necessary or best remedy — that normative step is contested by formal-equality scholars and by some empirical researchers who attribute persistence to non-state mechanisms.
narrative_ontology:disappearance_verdict(fourteenth_amendment_equal_protection__anti_caste_reading, world_rearranges).
narrative_ontology:founding_problem_status(fourteenth_amendment_equal_protection__anti_caste_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(fourteenth_amendment_equal_protection__anti_caste_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(fourteenth_amendment_equal_protection__anti_caste_reading, 'none', 1).
narrative_ontology:epsilon_provenance(fourteenth_amendment_equal_protection__anti_caste_reading, 0.58, 'claude-sonnet-5', 'none', direct).

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
 *   Extraction is authored high-moderate (0.58) because affirmative remedial programs genuinely transfer concrete goods — admissions slots, contracts, districting advantage — from one identifiable set of parties to another, and this transfer is the reading's explicit point, not incidental to it. Suppression is authored moderate (0.42) and declining over the measured interval: the reading depends on active judicial and administrative enforcement to survive formal-equality challenges, but enforcement intensity has softened since the 1990s as courts narrowed strict-scrutiny tolerance for race-conscious remedies. Theater ratio is moderate (0.28): some remedial programs persist as compliance-signaling gestures (diversity statements, disparate-impact audits with no enforcement teeth) even where genuine remedial transfer has been curtailed by later doctrine — this is tracked in the theater_ratio rise from 0.10 to 0.30 as courts constrained substantive remedies while institutions retained procedural performance of the anti-caste commitment. Accessibility collapse is moderate-low (0.35): formal-equality alternatives remain live and contested, not foreclosed — this is precisely why the story is a tangled_rope rather than a mountain or snare. Resistance is high (0.72): formal-equality advocates, disadvantaged nonminority applicants, and a substantial doctrinal counter-tradition actively contest this reading in courts and legislatures; it has never achieved uncontested settlement.
 *
 * PERSPECTIVAL GAP:
 *   From the enforcement-agency seat, this reading is the coordination solution to a genuine, well-documented structural problem — hierarchy reproduced through facially neutral means. From the nonminority-applicant-payer seat, the identical structure operates as an enforced transfer imposed without individualized fault-finding. Both experiences are structurally real; the engine's per-seat computation should register the agenda_setter side nearer coordination and the payer side nearer extraction, which is the seat divergence this reading's tangled_rope classification is meant to capture rather than average away.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary groups (racial minority communities, women, other status groups) sit near the beneficiary end of directionality because the reading's remedial apparatus is structurally organized around transferring goods and standing to them, even though many individual members of these groups do not directly receive any specific remedial allocation. Payer groups (nonminority applicants, compliance-liable state actors, cost-bearing municipalities) sit near the target end because they bear concrete, identifiable costs — lost admissions slots, litigation exposure, compliance budgets — through the same structure that delivers the remedial benefit. The enforcement-agency seat is institutional and holds arbitrage-grade exit (it can narrow or expand enforcement discretion) which is why it is coded agenda_setter rather than beneficiary despite gaining institutional mandate from the reading's persistence.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — facially neutral rules reproducing hierarchy without triggering formal scrutiny — remains empirically live by outside corroboration (labor-market and housing-segregation research independent of civil rights agencies), which prevents this from being classified as pure zombie mandatrophy. But the specific remedial mechanisms authorized under this reading have drifted from substantive-transfer remedies (desegregation orders, hard set-asides) toward procedural/theatrical compliance (disparate-impact audits, diversity reporting) as courts narrowed permissible remedies — the founding_problem_status is authored 'contested' rather than 'dead' because the underlying hierarchy persists by outside measurement even as the doctrinal tools available to address it have been substantially curtailed. This is a tangled_rope, not a snare, precisely because a real coordination function (correcting documented structural hierarchy) coexists with real, concentrated extraction from specific payers — collapsing either half of that structure into the other would mislabel either the remedy as pure theft or the cost-bearing individuals' objections as illegitimate.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    anti_caste_vs_formal_equality_kernel_contest,
    'Is the Equal Protection Clause''s core commitment the anti-caste principle (affirmative dismantling of hierarchy) or the formal-equality principle (prohibition on state classification)? Both readings claim the same constitutional text and Reconstruction-era history as their warrant.',
    'No empirical resolution mechanism exists — this is a live doctrinal and political contest resolved provisionally by Supreme Court composition and precedent, subject to reversal with changes in judicial personnel. Historical evidence about Reconstruction-era Congressional intent is itself contested between the two readings'' own historians.',
    'If the anti-caste reading prevails doctrinally, affirmative remedial programs retain constitutional legitimacy and this constraint''s high-ε remedial transfers continue to be authorized. If the formal-equality reading prevails, this constraint''s core mechanism (group-conscious state correction) becomes constitutionally suspect or foreclosed, and the sibling constraint''s lower-ε, non-remedial structure becomes the governing constraint instead.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(anti_caste_vs_formal_equality_kernel_contest, conceptual, 'The kernel-level contest between anti-caste and formal-equality readings of Equal Protection, which this story instantiates one side of.').

omega_variable(
    structural_inequality_causal_attribution,
    'To what extent is persistent group-based disparity in outcomes (wealth, incarceration, representation) attributable to ongoing state and private discriminatory mechanisms this reading targets, versus to non-state, non-discriminatory causal factors this reading''s remedies would not address?',
    'Longitudinal audit studies, natural experiments from jurisdictions with and without specific remedial programs, and decomposition analysis distinguishing discrimination-attributable from other-attributable disparity.',
    'If disparity is substantially discrimination-attributable, the anti-caste reading''s coordination function is well-grounded in ongoing (not merely historical) harm. If substantially non-discrimination-attributable, the reading''s remedial transfers extract from current payers to address a problem its mechanisms cannot actually solve, which would push the classification toward tangled_rope-with-weaker-coordination-leg or even snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(structural_inequality_causal_attribution, empirical, 'Whether the causal mechanisms producing measured disparity are the ones this reading''s remedies target.').

omega_variable(
    remedial_program_beneficiary_targeting_precision,
    'Do actual remedial programs authorized under this reading reach the historically subordinated individuals the doctrine is theorized to benefit, or do benefits diffuse to advantaged members of nominally subordinated groups (class-skew within group-based remedies)?',
    'Distributional analysis of who actually receives admissions, contracting, and employment benefits under existing remedial programs, disaggregated by within-group socioeconomic status.',
    'If benefits skew toward already-advantaged group members, the reading''s beneficiary declaration is less precise than authored here, and the true directionality picture would show a narrower, more class-inflected beneficiary set than ''racial_minority_communities'' broadly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(remedial_program_beneficiary_targeting_precision, empirical, 'Whether the declared beneficiary groups accurately capture who receives the remedial transfer in practice.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fourteenth_amendment_equal_protection__anti_caste_reading, 1954, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(four_tr_t1954, fourteenth_amendment_equal_protection__anti_caste_reading, theater_ratio, 1954, 0.1).
narrative_ontology:measurement(four_tr_t1968, fourteenth_amendment_equal_protection__anti_caste_reading, theater_ratio, 1968, 0.15).
narrative_ontology:measurement(four_tr_t1978, fourteenth_amendment_equal_protection__anti_caste_reading, theater_ratio, 1978, 0.2).
narrative_ontology:measurement(four_tr_t1995, fourteenth_amendment_equal_protection__anti_caste_reading, theater_ratio, 1995, 0.28).
narrative_ontology:measurement(four_tr_t2010, fourteenth_amendment_equal_protection__anti_caste_reading, theater_ratio, 2010, 0.3).
narrative_ontology:measurement(four_tr_t2024, fourteenth_amendment_equal_protection__anti_caste_reading, theater_ratio, 2024, 0.28).

% Extraction over time
narrative_ontology:measurement(four_be_t1954, fourteenth_amendment_equal_protection__anti_caste_reading, base_extractiveness, 1954, 0.35).
narrative_ontology:measurement(four_be_t1968, fourteenth_amendment_equal_protection__anti_caste_reading, base_extractiveness, 1968, 0.45).
narrative_ontology:measurement(four_be_t1978, fourteenth_amendment_equal_protection__anti_caste_reading, base_extractiveness, 1978, 0.55).
narrative_ontology:measurement(four_be_t1995, fourteenth_amendment_equal_protection__anti_caste_reading, base_extractiveness, 1995, 0.62).
narrative_ontology:measurement(four_be_t2010, fourteenth_amendment_equal_protection__anti_caste_reading, base_extractiveness, 2010, 0.58).
narrative_ontology:measurement(four_be_t2024, fourteenth_amendment_equal_protection__anti_caste_reading, base_extractiveness, 2024, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(four_su_t1954, fourteenth_amendment_equal_protection__anti_caste_reading, suppression_requirement, 1954, 0.6).
narrative_ontology:measurement(four_su_t1968, fourteenth_amendment_equal_protection__anti_caste_reading, suppression_requirement, 1968, 0.5).
narrative_ontology:measurement(four_su_t1978, fourteenth_amendment_equal_protection__anti_caste_reading, suppression_requirement, 1978, 0.45).
narrative_ontology:measurement(four_su_t1995, fourteenth_amendment_equal_protection__anti_caste_reading, suppression_requirement, 1995, 0.42).
narrative_ontology:measurement(four_su_t2010, fourteenth_amendment_equal_protection__anti_caste_reading, suppression_requirement, 2010, 0.4).
narrative_ontology:measurement(four_su_t2024, fourteenth_amendment_equal_protection__anti_caste_reading, suppression_requirement, 2024, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fourteenth_amendment_equal_protection__anti_caste_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(fourteenth_amendment_equal_protection__anti_caste_reading, fourteenth_amendment_equal_protection__formal_equality_reading).

% DUAL FORMULATION NOTE:
% This story and fourteenth_amendment_equal_protection__formal_equality_reading are sibling readings of the same kernel (fourteenth_amendment_equal_protection), not two measurements of one constraint. This reading authorizes affirmative state correction and carries a substantially higher ε because it legitimates concrete remedial transfer; the formal-equality sibling prohibits state classification generally (remedial or invidious alike) absent compelling justification and carries a lower ε because it does not authorize transfer, only occasionally permits narrowly-tailored exceptions. Each reading has its own beneficiary/victim structure, its own stakeholders, and its own classification; they are linked here per the ε-invariance decomposition principle rather than combined into one story with an observable parameter.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
