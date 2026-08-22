% ============================================================================
% CONSTRAINT STORY: fourteenth_amendment_equal_protection__anti_caste_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
 *   constraint_id: fourteenth_amendment_equal_protection__anti_caste_reading
 *   human_readable: Equal Protection Anti-Caste Reading: Active Remedial State Action
 *   domain: constitutional_law/civil_rights
 *
 * SUMMARY:
 *   The Fourteenth Amendment's Equal Protection Clause is a contested kernel.
 *   The anti-caste reading interprets it as mandating active state
 *   remediation of racial, gender, and status hierarchies: subordinated
 *   groups are beneficiaries of affirmative action and institutional reform;
 *   historically privileged actors and institutions are payers bearing the
 *   costs of remedial redistribution. This reading claims that passive
 *   non-discrimination is insufficient—the state must actively dismantle
 *   inherited stratification. The rival formal equality reading claims that
 *   Equal Protection prohibits state racial/gender consciousness itself,
 *   treating remedial action as illegitimate discrimination. This story
 *   instantiates the anti-caste reading as a clean constraint with ε=0.62,
 *   treating it as a tangled rope: genuine coordination (organizing remedial
 *   state action around a constitutional mandate) coupled with asymmetric
 *   extraction (privileged groups bear institutional and wealth
 *   redistribution costs). The formal equality reading is a different
 *   constraint (not this one); the two readings coexist as live positions
 *   held by different constitutional coalitions.
 *
 * KEY AGENTS:
 *   - historically_subordinated_racial_groups (beneficiary, organized/generational; constrained exit)
 *   - historically_subordinated_gender_groups (beneficiary, organized/generational; constrained exit)
 *   - low_status_castes (beneficiary, powerless/generational; identity-locked exit)
 *   - established_majority_position_holders (payer, powerful/biographical; constrained exit)
 *   - historically_privileged_institutional_beneficiaries (payer, institutional/generational; constrained exit)
 *   - supreme_court_institutional_anchor (agenda_setter, institutional/generational; analytical exit)
 *   - state_legislative_bodies (agenda_setter + payer, institutional/generational; constrained exit)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fourteenth_amendment_equal_protection__anti_caste_reading, 0.62).
domain_priors:suppression_score(fourteenth_amendment_equal_protection__anti_caste_reading, 0.58).
domain_priors:theater_ratio(fourteenth_amendment_equal_protection__anti_caste_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fourteenth_amendment_equal_protection__anti_caste_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(fourteenth_amendment_equal_protection__anti_caste_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(fourteenth_amendment_equal_protection__anti_caste_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(fourteenth_amendment_equal_protection__anti_caste_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(fourteenth_amendment_equal_protection__anti_caste_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fourteenth_amendment_equal_protection__anti_caste_reading, tangled_rope).
narrative_ontology:human_readable(fourteenth_amendment_equal_protection__anti_caste_reading, "Equal Protection Anti-Caste Reading: Active Remedial State Action").
narrative_ontology:topic_domain(fourteenth_amendment_equal_protection__anti_caste_reading, "constitutional_law/civil_rights").

domain_priors:requires_active_enforcement(fourteenth_amendment_equal_protection__anti_caste_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(fourteenth_amendment_equal_protection__anti_caste_reading, '9873ea37-fb0e-4310-9488-ec74e67dd203').
narrative_ontology:cs_kernel_codification('9873ea37-fb0e-4310-9488-ec74e67dd203', fixed_text).
narrative_ontology:cs_authority_grounding('9873ea37-fb0e-4310-9488-ec74e67dd203', lineage).
narrative_ontology:cs_interpretation_layer_present('9873ea37-fb0e-4310-9488-ec74e67dd203').
narrative_ontology:cs_reading_relation('9873ea37-fb0e-4310-9488-ec74e67dd203', fourteenth_amendment_equal_protection__formal_equality_reading, coexists_with).
narrative_ontology:cs_axiom('9873ea37-fb0e-4310-9488-ec74e67dd203', foundational, hierarchy_dismantling_constitutional_mandate).
narrative_ontology:cs_axiom_status(hierarchy_dismantling_constitutional_mandate, holdable).
narrative_ontology:cs_axiom_grounding('9873ea37-fb0e-4310-9488-ec74e67dd203', hierarchy_dismantling_constitutional_mandate, deontological).
narrative_ontology:cs_axiom('9873ea37-fb0e-4310-9488-ec74e67dd203', secondary, passive_nondiscrimination_insufficient).
narrative_ontology:cs_axiom_status(passive_nondiscrimination_insufficient, holdable).
narrative_ontology:cs_axiom_grounding('9873ea37-fb0e-4310-9488-ec74e67dd203', passive_nondiscrimination_insufficient, empirically_contingent).
narrative_ontology:cs_reference_frame('9873ea37-fb0e-4310-9488-ec74e67dd203', radical_reconstruction_equality_mandate).
narrative_ontology:cs_drift_state('9873ea37-fb0e-4310-9488-ec74e67dd203', contemporary_conservative_jurisprudence_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('9873ea37-fb0e-4310-9488-ec74e67dd203', '').
narrative_ontology:cs_kernel_id(fourteenth_amendment_equal_protection__anti_caste_reading, fourteenth_amendment_equal_protection).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fourteenth_amendment_equal_protection__anti_caste_reading, historically_subordinated_racial_groups).
narrative_ontology:constraint_beneficiary(fourteenth_amendment_equal_protection__anti_caste_reading, historically_subordinated_gender_groups).
narrative_ontology:constraint_beneficiary(fourteenth_amendment_equal_protection__anti_caste_reading, low_status_castes).
narrative_ontology:constraint_victim(fourteenth_amendment_equal_protection__anti_caste_reading, established_majority_position_holders).
narrative_ontology:constraint_victim(fourteenth_amendment_equal_protection__anti_caste_reading, historically_privileged_institutional_beneficiaries).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(fourteenth_amendment_equal_protection__anti_caste_reading, state_legislative_bodies).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Under this reading, stand to receive state-directed remedial action: affirmative education programs, hiring preferences, wealth-redistribution measures, and deconcentration of institutional power. Their position is that the Equal Protection mandate requires active state dismantling of the hierarchies that created and sustain their subordination. Exit from this reading's protections would mean abandoning the claim to remedial action.
narrative_ontology:constraint_stakeholder(fourteenth_amendment_equal_protection__anti_caste_reading, historically_subordinated_racial_groups, beneficiary,
    organized, generational, constrained, national).

% Under this reading, become beneficiaries of state corrective action aimed at dismantling gender hierarchy: family law reform, employment equity measures, educational access programs, and targeted institutional change. The constraint legitimates their claim that Equal Protection is not mere non-discrimination but affirmative restructuring of gendered power.
narrative_ontology:constraint_stakeholder(fourteenth_amendment_equal_protection__anti_caste_reading, historically_subordinated_gender_groups, beneficiary,
    organized, generational, constrained, national).

% Under this reading become eligible for remedial action: educational reservations, occupational preferences, wealth redistribution, and institutional deconcentration. Their status as subordinated is itself treated as the injury Equal Protection must remedy, not merely discrimination in particular transactions.
narrative_ontology:constraint_stakeholder(fourteenth_amendment_equal_protection__anti_caste_reading, low_status_castes, beneficiary,
    powerless, generational, identity_locked, national).

% Under this reading, bear costs through affirmative action programs that reduce their institutional access (education, employment, contracting), through wealth redistribution, and through institutional reorganization that displaces their centrality. They experience this as extraction—loss of position and resources—and argue they bear remedial burdens for historical injustices they did not personally commit.
narrative_ontology:constraint_stakeholder(fourteenth_amendment_equal_protection__anti_caste_reading, established_majority_position_holders, payer,
    powerful, biographical, constrained, national).

% Institutions built on and perpetuating racial/gender/caste hierarchy (universities with historical exclusion, employers with segregated promotion ladders, wealth accumulated through discriminatory systems) must undergo remedial restructuring: curriculum change, hiring reform, asset redistribution, deconcentration of institutional power. They experience this as dismantling of the inherited structural advantage the anti-caste reading treats as illegitimate.
narrative_ontology:constraint_stakeholder(fourteenth_amendment_equal_protection__anti_caste_reading, historically_privileged_institutional_beneficiaries, payer,
    institutional, generational, constrained, national).

% The formal equality reading's parties—those who would argue that remedial race/gender consciousness is itself unconstitutional discrimination—are structurally excluded from this reading's frame. They are not absent from the broader constitutional debate, but under the anti-caste reading their position is treated as foreclosed by the foundational axiom that hierarchy-dismantling is the constitutional mandate.
narrative_ontology:constraint_stakeholder(fourteenth_amendment_equal_protection__anti_caste_reading, formal_equality_reading_parties, excluded,
    institutional, generational, trapped, national).

% The Supreme Court, as the authoritative interpreter of the Fourteenth Amendment, sets whether this reading or the formal equality reading governs constitutional enforcement. The Court's doctrine has shifted multiple times, currently favoring formal equality over affirmative remediation. Under the anti-caste reading, the Court's conservative restriction of remedial authority is itself illegitimate—a failure to enforce the true constitutional mandate.
narrative_ontology:constraint_stakeholder(fourteenth_amendment_equal_protection__anti_caste_reading, supreme_court_institutional_anchor, agenda_setter,
    institutional, generational, analytical, national).

% State legislatures face contradictory pressures: the anti-caste reading legitimates remedial statutes; formal equality and conservative jurisprudence restrict them. Many legislatures are both agenda-setters (choosing which reading to follow) and payers (bearing political cost of remedial programs from majority constituencies opposing them).
narrative_ontology:constraint_stakeholder(fourteenth_amendment_equal_protection__anti_caste_reading, state_legislative_bodies, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(fourteenth_amendment_equal_protection__anti_caste_reading, state_legislative_bodies, payer).

% Comparative constitutional law and political philosophy: the anti-caste reading aligns with transformative constitutionalism and Indian constitutional doctrine (Articles 15-16 affirmative action). The formal equality reading reflects liberal-individualist doctrine dominant in Anglo-American law. The reading contest is partially empirical (whether hierarchies self-correct or require active dismantling) and partially normative (whether the state's obligation is negative non-discrimination or affirmative hierarchy-remediation).
narrative_ontology:constraint_stakeholder(fourteenth_amendment_equal_protection__anti_caste_reading, analytical_observer, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(fourteenth_amendment_equal_protection__anti_caste_reading, established_majority_position_holders).
narrative_ontology:fixing_cost_class(fourteenth_amendment_equal_protection__anti_caste_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Organizes state institutions (courts, legislatures, administrative agencies) and remedial capacity around a single constitutional mandate: that Equal Protection requires active dismantling of racial, gender, and status hierarchies through state corrective action. This coordinates subordinated groups' remedial claims with state authority's obligation to undertake remediation.
% TRANSFER_FUNCTION: Moves institutional position (education access, employment opportunity, contracting preference), accumulated wealth (through reparative redistribution and asset-building programs), and concentrated institutional power from historically privileged groups and institutions toward historically subordinated groups through affirmative action, remedial programs, and deconcentration initiatives.
% ABSENT_VOICES: Persons who hold the formal equality reading and believe remedial race/gender consciousness itself violates Equal Protection are structurally excluded from the anti-caste reading's framework. They are not absent from the broader constitutional conversation—the formal equality reading commands a majority on the contemporary Supreme Court—but within the anti-caste reading they are treated as committed to a position that forecloses hierarchy-remediation. Conservative institutional actors benefiting from existing hierarchies lack representation in the anti-caste reading's authority structure.
% DISAPPEARANCE_RATIONALE: Proponents (anti-caste advocates): if the remedial mandate disappeared, racial/gender/caste hierarchies would crystallize and deepen—subordinated groups would lose legal claims to remedial action, institutional stratification would persist, and accumulated wealth would be transmitted uninterrupted to privileged heirs. The world would rearrange into more durable hierarchy. Opponents (formal equality advocates): if active remedial mandates disappeared, individual merit-based selection and market competition would gradually correct stratification; removing remedial programs would reduce state overhead and legal categorization, allowing hierarchies to attenuate naturally over generational time. The contest is whether hierarchy requires active state remedy or passive non-discrimination suffices.
% FOUNDING_PROBLEM: Racial slavery, Jim Crow segregation, gender-based legal disabilities, occupational castes, and inherited institutional segregation created cumulative subordination and durable structured inequality. Wealth, occupational access, educational attainment, political power, and institutional control remain stratified along racial, gender, and status lines. The anti-caste reading holds this founding problem requires active constitutional remedy—passive non-discrimination cannot dismantle what centuries of systematic exclusion created.
% FOUNDING_PROBLEM_CORROBORATION: Historical scholarship (comprehensive documentation of slavery, Jim Crow, gender-based legal disability, occupational segregation from outside benefiting parties) attests the founding problem is real. Empirical social science (wealth gaps, occupational stratification, educational segregation, health disparities, political underrepresentation) from independent researchers confirms persistent structured inequality. The contest is not whether historical subordination occurred or produces durable effects—that is established—but whether it constitutes a founding problem demanding active remedial state action (anti-caste reading) or whether it is a historical fact that passive non-discrimination going forward addresses (formal equality reading). Justice Ruth Bader Ginsburg's dissent in Shelby County and legal scholars outside the benefiting parties support the anti-caste reading's foundational diagnosis.
narrative_ontology:disappearance_verdict(fourteenth_amendment_equal_protection__anti_caste_reading, contested).
narrative_ontology:founding_problem_status(fourteenth_amendment_equal_protection__anti_caste_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(fourteenth_amendment_equal_protection__anti_caste_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(fourteenth_amendment_equal_protection__anti_caste_reading, 'none', 1).
narrative_ontology:epsilon_provenance(fourteenth_amendment_equal_protection__anti_caste_reading, 0.62, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness measures at 0.62 because the constraint legitimates substantial redistribution of institutional position and accumulated wealth from historically privileged groups toward subordinated groups. This is not extraction in the predatory sense—the anti-caste reading frames it as constitutionally mandated correction of prior wrongful hierarchy—but from the payer seats, the constraint operates as structural loss of position and resources. The measurement trajectory shows extraction rising from 0.35 (t=0, early post-Civil Rights Act era) to a peak of 0.64 (t=35, high affirmative action period post-Gratz/Grutter), then stabilizing around 0.62 as Supreme Court jurisprudence shifted toward formal equality constraints (Fisher v. University of Texas, Shelby County). Theater ratio rises from 0.25 to 0.43 as affirmative action programs become more proceduralized and legalistic (extensive compliance documentation, diversity statements) while substantive remediation capacity narrows—increasing performance relative to outcomes. Suppression requirement follows a similar trajectory: the constraint requires active institutional suppression of market/meritocratic sorting to maintain remedial programs, peaking as resistance from privileged groups and conservative jurisprudence mounted (justices, business interests, voters opposed to remedial programs). One shared time grid anchors all metrics to the same observation points.
 *
 * PERSPECTIVAL GAP:
 *   The anti-caste reading should compute substantially differently across seats. From the agenda-setter institutional seat (Supreme Court, state legislatures), the constraint appears as a genuine coordination mandate requiring active institutional restructuring to comply with constitutional obligation. From the beneficiary seats (subordinated groups), it appears as a hard-won remedial guarantee they must defend against ongoing erosion (which drives high resistance at 0.72). From the payer seats (privileged groups, privileged institutions), it appears as enforced extraction disguised as constitutional remedy—institutions lose position, individuals lose opportunity, accumulated wealth is redistributed, all at legal gunpoint. The engine computes this divergence from power atoms and directionality; it is not pre-adjudicated by the claimed type.
 *
 * DIRECTIONALITY LOGIC:
 *   Subordinated groups have low directionality (d toward 0.0, beneficiary end) because the constraint subsidizes their access claims—they receive state-directed institutional entry and wealth redistribution without directly administering or defending the constraint. Historically privileged groups have high directionality (d toward 1.0, target end) because they bear costs: reduced institutional access, wealth redistribution, institutional reorganization—all imposed by the state to enforce the remedial mandate. Institutional actors like universities sit near 0.5 (symmetric): they must reorganize their structures (cost) but also gain legitimacy and federal funding tied to diversity metrics (benefit). The Supreme Court is the agenda-setter (d near 0.0 in its own frame—it defines the constitutional meaning) but also partially targeted (cannot escape having to adjudicate the contest and increasingly constrained by political pressure to rule against remedial programs). State legislatures are dual-positioned: agenda-setters when they choose which reading to follow, payers when they bear political cost from majority voters opposing remedial programs.
 *
 * MANDATROPHY ANALYSIS:
 *   The anti-caste reading's founding problem is historical subordination through slavery, Jim Crow, gender-based legal disabilities, and caste-like occupational hierarchy. The constraint is an attempt to remedy that founding problem through active state dismantling. The measurement series shows a rise in extractiveness and theater ratio concurrent with growing Supreme Court skepticism of remedial programs (Shelby County 2013 onwards). This creates a mandatrophy hypothesis: the founding problem (historical subordination) remains live, but the constraint's capacity to address it is decaying—the Court is narrowing remedial authority, theater (procedural compliance) is rising relative to actual redistribution, and effective remediation is stabilizing or declining even as the extractive burden on privileged groups persists. The founding_problem_status = contested because beneficiary groups attest the problem is live and worsening (wealth gaps, occupational segregation, educational stratification persist), while the formal equality reading increasingly denies that remedial action is the constitutional solution. Mandatrophy resolution turns on whether the constraint's foundational purpose (active remediation of hierarchy) is still operant or has become decorative performance—the measurement trajectory toward plateau with rising theater suggests early signs of inertial persistence (piton precursor).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    hierarchy_causation_empirical,
    'Do racial, gender, and status hierarchies persist primarily due to historical subordination whose effects compound over time (requiring active remediation), or due to ongoing individual and institutional choices that market mechanisms will self-correct?',
    'Long-term natural experiments: jurisdictions that enacted strong remedial programs vs. weak remedial programs, holding other factors constant. Intergenerational wealth and occupational mobility studies. Comparative evidence (India''s Article 15 reservations outcomes vs. non-remedial jurisdictions).',
    'If hierarchies self-correct through market and meritocratic mechanisms, the constraint''s extractiveness drops and formal equality dominates. If hierarchies compound and require active remediation, extractiveness remains high and the anti-caste reading''s constitutional mandate holds. This is the empirical hinge on which reading victory turns.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(hierarchy_causation_empirical, empirical, 'Whether hierarchy persistence is self-correcting or requires active state remediation.').

omega_variable(
    remedial_vs_distributional_mandate,
    'Is the constitutional mandate for remedial action limited to correcting past specific harms (narrow remediation: only for identified discrimination victims) or does it extend to correcting structural inequality produced by historical subordination regardless of individual culpability (broad remediation: group-based structural correction)?',
    'Constitutional hermeneutics and case law doctrine: how narrowly or broadly courts define the remedial mandate. Political struggle over affirmative action scope. Comparative constitutional interpretation (India''s Article 16 reservation doctrine treats structural inequality as remediable; Anglo-American law increasingly narrows the scope).',
    'Narrow remediation would dramatically lower extractiveness (ε drops to 0.25-0.35) because only specific past-harm-victims qualify for remedial action, not all structurally subordinated groups. Broad remediation keeps ε elevated (0.55+) because remedial programs extend to whole groups whose historical subordination created structural inequality. The constraint''s classification hinges partly on this distinction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(remedial_vs_distributional_mandate, conceptual, 'Scope of the constitutional remedial mandate: specific past harms or structural inequality produced by historical subordination.').

omega_variable(
    suppression_mechanism_structural_vs_internalized,
    'Is the suppression of remedial action primarily structural (legal barriers, judicial doctrine, political opposition, resource constraints) or does it include internalized suppression where beneficiary groups internalize the formal equality reading and accept non-remedial status?',
    'Activist and organizing ecology: how much resistance comes from external legal/political barriers vs. how much from internalized acceptance of formal equality framing. Post-remedial-removal trajectory: if suppression persists after legal barriers fall, internalization is significant. Social movement narrative analysis.',
    'If structural, removing legal barriers and installing supportive judges could restore remedial enforcement. If internalized, beneficiary groups may not press remedial claims even if legal barriers fall, lowering the constraint''s operative extraction. Understanding the mechanism informs whether the suppression value (0.58) understates or fairly captures the dynamics.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_internalized, empirical, 'Whether suppression of remedial action is structural or internalized (or both, and in what proportion).').

omega_variable(
    formal_equality_foreclosure,
    'Does the anti-caste reading''s commitment to active hierarchy-dismantling logically foreclose the formal equality reading, or do the readings coexist as alternative constitutional interpretations neither of which logically eliminates the other?',
    'Hermeneutical analysis: can a court or constitutional framework hold both readings simultaneously without internal contradiction? Or does committing to one necessarily deny the other''s core premise?',
    'If foreclosure is genuine (one premise directly contradicts the other), the reading_relations entry should be forecloses, not coexists_with, and the constraint''s classification shifts toward zero degrees of freedom for interpretation. If coexistence is possible (readings reflect different normative priorities rather than logical contradiction), the readings remain genuinely live alternatives and classification remains contestable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(formal_equality_foreclosure, conceptual, 'Whether anti-caste and formal equality readings logically foreclose each other or coexist as alternative live interpretations.').

omega_variable(
    piton_risk_trajectory,
    'As Supreme Court jurisprudence increasingly constrains remedial action (Shelby County, Fisher v. University of Texas), is the anti-caste reading persisting primarily through institutional inertia and theatrical compliance (remedial bureaucracy, diversity statements, procedural overhead) while actual substantive redistribution decays—a path toward piton classification?',
    'Measure the gap between procedural remedial activity (compliance filings, diversity programs, affirmative action processing) and actual outcomes (wealth redistribution realized, occupational mobility achieved, institutional power deconcentration). Compare theater_ratio trend (rising from 0.25 to 0.41, plateaued) against outcome effectiveness metrics.',
    'If substantive remediation is decaying faster than procedural theater, the constraint is becoming a degraded piton: performs remedial function for legitimacy purposes while actual hierarchy-dismantling capacity atrophies. This would shift classification toward piton and suggest mandatrophy (founding problem live, remedial capacity dead).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(piton_risk_trajectory, empirical, 'Whether the anti-caste reading is transitioning toward piton status as judicial constraint narrows remedial capacity.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fourteenth_amendment_equal_protection__anti_caste_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(four_tr_t0, fourteenth_amendment_equal_protection__anti_caste_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(four_tr_t7, fourteenth_amendment_equal_protection__anti_caste_reading, theater_ratio, 7, 0.3).
narrative_ontology:measurement(four_tr_t14, fourteenth_amendment_equal_protection__anti_caste_reading, theater_ratio, 14, 0.35).
narrative_ontology:measurement(four_tr_t21, fourteenth_amendment_equal_protection__anti_caste_reading, theater_ratio, 21, 0.4).
narrative_ontology:measurement(four_tr_t28, fourteenth_amendment_equal_protection__anti_caste_reading, theater_ratio, 28, 0.42).
narrative_ontology:measurement(four_tr_t35, fourteenth_amendment_equal_protection__anti_caste_reading, theater_ratio, 35, 0.43).
narrative_ontology:measurement(four_tr_t42, fourteenth_amendment_equal_protection__anti_caste_reading, theater_ratio, 42, 0.41).
narrative_ontology:measurement(four_tr_t50, fourteenth_amendment_equal_protection__anti_caste_reading, theater_ratio, 50, 0.41).

% Extraction over time
narrative_ontology:measurement(four_be_t0, fourteenth_amendment_equal_protection__anti_caste_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(four_be_t7, fourteenth_amendment_equal_protection__anti_caste_reading, base_extractiveness, 7, 0.42).
narrative_ontology:measurement(four_be_t14, fourteenth_amendment_equal_protection__anti_caste_reading, base_extractiveness, 14, 0.48).
narrative_ontology:measurement(four_be_t21, fourteenth_amendment_equal_protection__anti_caste_reading, base_extractiveness, 21, 0.55).
narrative_ontology:measurement(four_be_t28, fourteenth_amendment_equal_protection__anti_caste_reading, base_extractiveness, 28, 0.6).
narrative_ontology:measurement(four_be_t35, fourteenth_amendment_equal_protection__anti_caste_reading, base_extractiveness, 35, 0.64).
narrative_ontology:measurement(four_be_t42, fourteenth_amendment_equal_protection__anti_caste_reading, base_extractiveness, 42, 0.62).
narrative_ontology:measurement(four_be_t50, fourteenth_amendment_equal_protection__anti_caste_reading, base_extractiveness, 50, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(four_su_t0, fourteenth_amendment_equal_protection__anti_caste_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(four_su_t7, fourteenth_amendment_equal_protection__anti_caste_reading, suppression_requirement, 7, 0.5).
narrative_ontology:measurement(four_su_t14, fourteenth_amendment_equal_protection__anti_caste_reading, suppression_requirement, 14, 0.54).
narrative_ontology:measurement(four_su_t21, fourteenth_amendment_equal_protection__anti_caste_reading, suppression_requirement, 21, 0.58).
narrative_ontology:measurement(four_su_t28, fourteenth_amendment_equal_protection__anti_caste_reading, suppression_requirement, 28, 0.6).
narrative_ontology:measurement(four_su_t35, fourteenth_amendment_equal_protection__anti_caste_reading, suppression_requirement, 35, 0.61).
narrative_ontology:measurement(four_su_t42, fourteenth_amendment_equal_protection__anti_caste_reading, suppression_requirement, 42, 0.58).
narrative_ontology:measurement(four_su_t50, fourteenth_amendment_equal_protection__anti_caste_reading, suppression_requirement, 50, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fourteenth_amendment_equal_protection__anti_caste_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(fourteenth_amendment_equal_protection__anti_caste_reading, 0.18).
narrative_ontology:affects_constraint(fourteenth_amendment_equal_protection__anti_caste_reading, fourteenth_amendment_equal_protection__formal_equality_reading).

% DUAL FORMULATION NOTE:
% The Fourteenth Amendment Equal Protection Clause is a contested kernel instantiated by two structurally distinct constraints: the anti-caste reading (this file) interprets the clause as mandating active state remediation of racial, gender, and status hierarchies; the formal equality reading interprets it as prohibiting state racial/gender consciousness. These are not the same constraint viewed from different angles—they entail opposite institutional mandates and beneficiary/victim structures. The anti-caste reading claims high ε (0.62) for substantial remedial redistribution; the formal equality reading would claim low ε for protecting individual non-discrimination. Their ε values differ by design, not by measurement perspective. They are linked via network.affects_constraints because the two readings influence each other's legitimacy conditions: Court shifts toward formal equality narrow the anti-caste reading's operational space; anti-caste arguments pressure the formal equality framing to justify why hierarchy-remediation is unconstitutional. The readings coexist as live constitutional positions held by different coalitions and remain a genuine site of structural contest over Equal Protection's meaning.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(fourteenth_amendment_equal_protection__anti_caste_reading, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
