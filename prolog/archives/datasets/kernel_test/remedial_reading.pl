% ============================================================================
% CONSTRAINT STORY: remedial_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_remedial_reading, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: remedial_reading
 *   human_readable: Equal Protection via Remedial Race-Conscious Classification
 *   domain: constitutional_law/civil_rights
 *
 * SUMMARY:
 *   The remedial reading of equal protection holds that historical
 *   group-based racial subordination (slavery, Jim Crow, discriminatory
 *   redlining, educational exclusion) creates constitutional justification
 *   for race-conscious remedial measures in education, employment, and
 *   contracting. Under this reading, equal protection requires not colorblind
 *   treatment but targeted remediation of subordinated groups' accumulated
 *   disadvantage. This instantiates a Tangled Rope constraint: genuine
 *   coordination function (addressing real historical subordination through
 *   institutional remediation) combined with asymmetric extraction
 *   (majority-group individuals face competitive disadvantage, institutional
 *   enforcement costs are real). The remedial reading is ONE reading of a
 *   contested constitutional kernel — 'equal protection of the laws' — that
 *   also admits colorblind readings (race-consciousness itself violates equal
 *   protection) and antisubordination readings (equality measured by outcome
 *   parity, not necessarily through race-conscious classification). This
 *   story generates only the remedial reading as a clean ε-invariant
 *   constraint; sibling readings are separate constraint stories with
 *   different ε values and beneficiary/victim structures. Theater ratio
 *   (0.48) reflects moderate performativity: remedial enforcement has evolved
 *   into procedural compliance and diversity accounting alongside substantive
 *   institutional change. Base extractiveness (0.58) reflects a hybrid:
 *   genuine coordination addressing historical extraction combined with
 *   measured extraction from majority applicants. Suppression (0.62)
 *   indicates high barriers to exit from the remedial regime for both
 *   beneficiaries (cannot unidentify from group) and victims (competitive
 *   disadvantage is built into institutional policy).
 *
 * KEY AGENTS:
 *   - Historically Subordinated Racial Groups: Primary beneficiaries (powerless/trapped, beneficiary status) — structured as group-level beneficiaries of remedial allocation; experience both coordination (remedying exclusion) and partial extraction (identity-lock in race-conscious category)
 *   - Individual Majority Group Applicants: Primary victims (moderate/constrained, victim status) — face concrete competitive disadvantage in zero-sum allocation (college admissions, hiring, contracting); experience high suppression from rule opacity and inability to contest group-level remedy through individual merit claim
 *   - Educational/Employment Institutions: Mandatory enforcement agents (institutional/constrained, enforcement status) — required to implement race-conscious remedies; bear administrative burden, legal vulnerability, and reputational risk; constrained exit from remedial obligations
 *   - Civil Rights Coalition: Organized beneficiary advocates (organized/mobile, coordination status) — define remedial goals, generate political pressure for enforcement, control remedial framing; maintain exit option through political coalition formation
 *   - Judicial/Regulatory Enforcement System: Institutional sustainer (institutional/constrained, status degradation) — maintains remedial regime through precedent, regulation, and litigation; theater-ratio increase indicates procedural compliance displacing substantive transformation
 *   - Racial Justice Movement (Reparations Frame): Organized beneficiary advocates (organized/mobile, scaffold frame) — perceive remedial race-consciousness as temporary coordination mechanism with explicit sunset as reparations infrastructure matures; maintain mobile exit through alternative remedial strategy
 *   - Colorblind Doctrine Advocates: Organized victims (organized/arbitrage, alternative frame) — argue race-consciousness violates equal protection; maintain organized exit option through alternative constitutional reading (see sibling constraint: colorblind_reading)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(remedial_reading, 0.58).
domain_priors:suppression_score(remedial_reading, 0.62).
domain_priors:theater_ratio(remedial_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(remedial_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(remedial_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(remedial_reading, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(remedial_reading, tangled_rope).
narrative_ontology:human_readable(remedial_reading, "Equal Protection via Remedial Race-Conscious Classification").
narrative_ontology:topic_domain(remedial_reading, "constitutional_law/civil_rights").

domain_priors:requires_active_enforcement(remedial_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(remedial_reading, '1a02ef93-c649-478e-857e-5a20e588b23c').
narrative_ontology:cs_created_at('1a02ef93-c649-478e-857e-5a20e588b23c', '').
narrative_ontology:cs_kernel_codification('1a02ef93-c649-478e-857e-5a20e588b23c', fixed_text).
narrative_ontology:cs_authority_grounding('1a02ef93-c649-478e-857e-5a20e588b23c', lineage).
narrative_ontology:cs_interpretation_layer_present('1a02ef93-c649-478e-857e-5a20e588b23c').

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(remedial_reading, historically_subordinated_racial_groups).
narrative_ontology:constraint_victim(remedial_reading, individual_majority_applicants).
narrative_ontology:constraint_victim(remedial_reading, majority_group_opportunity_access).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: HISTORICALLY SUBORDINATED GROUP MEMBER (SNARE-to-ROPE transition) — Structurally trapped within legacy subordination (no exit from group membership or intergenerational disadvantage). Race-conscious remedies offer coordination function (allocation of scarce educational/employment opportunities toward those bearing historical extraction) but simultaneously create new extraction: the remedy is contingent on continued group identification and continued need-demonstration. The agent cannot fully exit the remedial regime because doing so forfeits the benefit. However, this is experienced as ROPE rather than SNARE from this perspective — the coordination function is genuine (addressing real historical subordination) and the agent perceives agency through collective remedy. Classification: ROPE from the beneficiary's own vantage.
constraint_indexing:constraint_classification(remedial_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: INDIVIDUAL MAJORITY GROUP APPLICANT (SNARE) — Faces competitive disadvantage in admissions/hiring due to race-conscious preferences. No structural barriers to exit (can relocate, pursue alternative institutions/employers), but meaningful career costs. Suppression is high: the applicant experiences the remedy as opaque rule-making where their individual merit is subordinated to group-level racial accounting. The applicant perceives minimal coordination function — the remedy redistributes their opportunity without their consent or participation in defining the remedial goal. Classification: SNARE from the victim's perspective — moderate power with constrained exit, high suppression, experienced as extraction without coordination benefit.
constraint_indexing:constraint_classification(remedial_reading, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: EDUCATIONAL/EMPLOYMENT INSTITUTION (TANGLED ROPE) — Mandatory enforcer of race-conscious remedies (via affirmative action mandates, diversity requirements, or settlement agreements). Genuine coordination function: remedies address institutional complicity in historical discrimination and correct for legacy-driven homogeneity that excludes qualified candidates from underrepresented groups. Genuine asymmetric extraction: institutions bear enforcement costs (administrative burden, legal vulnerability, reputational risk if quotas are perceived as lowering standards), constrained by legal requirements they did not consent to. Institutional exit is constrained — cannot simply abandon remedial obligations without facing legal liability. Classification: TANGLED ROPE — both coordination (addressing historical institutional exclusion) and enforcement-driven extraction (compliance costs, reduced discretion, legal exposure).
constraint_indexing:constraint_classification(remedial_reading, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: CIVIL RIGHTS COALITION (ROPE) — Organized advocates for remedial race-consciousness (civil rights organizations, racial justice movements, community groups). Pure coordination: define and enforce the goal of remedying historical subordination through institutional change. Experience the constraint as organizing tool rather than extraction — they control the framing, generate collective political power, and maintain mobile exit (can pressure institutions, legislate, or shift strategy). Classification: ROPE — beneficiary agents with agency and low experienced extraction.
constraint_indexing:constraint_classification(remedial_reading, rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: JUDICIAL/REGULATORY ENFORCEMENT SYSTEM (PITON) — Maintains remedial race-consciousness regime through ongoing litigation, regulatory guidance, and precedent-setting. Theater ratio (0.48) reflects that enforcement has become substantially procedural: institutions perform compliance (tracking diversity metrics, documenting remedial intent) while the underlying goal (remedying subordination) recedes into administrative accounting. The enforcement mechanism has partial atrophy: original purpose (correcting institutional discrimination) persists but is increasingly legalistic and theatricalized (diversity officer roles, compliance budgets) rather than substantively transformative. Classification: PITON — degraded coordination function sustained by institutional momentum and legal inertia.
constraint_indexing:constraint_classification(remedial_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: RACIAL JUSTICE REPARATIONS FRAMEWORK (SCAFFOLD with sunset logic) — Organized agents anticipate that temporary race-conscious remedies will transition to direct reparations, wealth transfer, and institutional transformation addressing root causes of subordination (slavery, Jim Crow, redlining) rather than fixing discrimination symptoms through admissions policy. This perspective sees remedial race-consciousness as a temporary coordination mechanism with an explicit sunset: as reparations infrastructure matures and subordination's structural causes are addressed, race-conscious admissions become redundant. Classification: SCAFFOLD — beneficiary agents with perceived exit path and declining extraction as root-cause remedies supplant group-based classification. Theater low because reparations framing shifts from accounting (diversity metrics) to transformation (wealth distribution, institutional redesign).
constraint_indexing:constraint_classification(remedial_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (FALSE SUMMIT CANDIDATE) — From civilizational scope, some perspectives treat race-based group subordination as an immutable structural feature of political systems (Pareto-optimal hierarchies, evolutionary group competition, etc.). Under this framing, remedial race-consciousness addresses an unchangeable social reality and thus appears as natural law accommodation rather than contingent policy. However, the structural data contradicts this: identifiable beneficiaries (historically subordinated groups) exist, asymmetric extraction (majority group competitive disadvantage) is measurable, and active enforcement (institutional mandates, legal pressure) sustains the mechanism. The engine will identify this as a false summit — the naturalization of what is actually a constructed remedial regime grounded in historical contingency, not immutable law.
constraint_indexing:constraint_classification(remedial_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(remedial_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(remedial_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(remedial_reading, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(remedial_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(remedial_reading, TR),
    TR >= 0.70.

:- end_tests(remedial_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The remedial reading involves genuine coordination (addressing documented historical subordination through institutional remedy) but also measurable extraction. Base extractiveness starts low (0.35) when remedies target severe, recent discrimination but increases over time (0.58 at T=20) as subordination moderates but remedies persist, creating extractive overshoot. The increase reflects that as historical subordination attenuates, continued race-conscious classification extracts more from majority applicants without proportionate benefit to most-subordinated. Suppression (0.62): High. Multiple barriers exist to exit: subordinated groups cannot unclaim identity-based disadvantage, majority applicants cannot opt out of competitive classification, institutions cannot legally abandon remedial obligations. The suppression is structural (legal requirement) not merely coercive, making it genuinely high. Theater ratio (0.48): Moderate. Institutional remedial enforcement has evolved into procedural compliance (diversity officer positions, demographic tracking, strategic plans) alongside substantive change (curriculum redesign, recruitment expansion). The rise over time (0.25 → 0.48) reflects creeping performativity: institutions demonstrate compliance through metrics while subordinating structures persist. The remedial regime is neither pure coordination nor pure extraction — it genuinely addresses historical exclusion while simultaneously becoming decoupled from the underlying goal through administrative routinization.
 *
 * PERSPECTIVAL GAP:
 *   Dramatic perspectival divergence across the observation site. The beneficiary (historically subordinated group member) experiences the constraint as ROPE — genuine coordination addressing real subordination, with benefits that outweigh costs. The majority victim experiences SNARE — competitive disadvantage with high suppression and no perceived coordination benefit. The institution experiences TANGLED ROPE — forced coordination alongside extraction (compliance burden). The organized civil rights movement experiences ROPE — mobilizing tool with strong agency. The enforcement system experiences PITON — degraded into procedural ritual. The reparations-framed movement experiences SCAFFOLD — temporary measure with sunset path. The colorblind analytical observer risks false summit — naturalizing race-consciousness as immutable law rather than contingent policy choice. The gap reflects genuine structural differences in how agents relate to the remedial regime, not mere disagreement — the remedial mechanism legitimately benefits subordinated groups while extracting from majority applicants, and agents experience this asymmetry differently based on their structural position.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) is derived from beneficiary/victim declarations and power/exit combinations. Historically subordinated groups (beneficiary, powerless/trapped) have d ≈ 0.05 (full beneficiary with no exit, high f(d) negative value reducing experienced extractiveness). Individual majority applicants (victim, moderate/constrained) have d ≈ 0.75 (moderate power with constrained exit, high f(d) value amplifying experienced extraction). Institutions (neither pure beneficiary nor victim; enforcement agents) derive d from mandated role as remedial enforcer: constrained exit + balanced harm/benefit yields d ≈ 0.50 (moderate extraction from forced coordination). Civil rights coalition (beneficiary, organized/mobile) have d ≈ 0.20 (beneficiary with organizational exit option, enabling arbitrage flexibility). No directionality overrides are needed — the derivation chain produces correct d values from structural data. The engine will compute chi from these d values, f(d), and scope modifiers. National scope (σ=1.0) means chi = ε × f(d) × 1.0, producing chi values that rank: beneficiary < coalition < institution < victim, mirroring the perspectival gap.
 *
 * MANDATROPHY ANALYSIS:
 *   The remedial reading resolves the mandatrophy by clearly declaring beneficiaries (historically subordinated groups) and victims (individual majority applicants experiencing competitive disadvantage). This prevents confusion between pure coordination (Rope) and mixed coordination-extraction (Tangled Rope). The constraint is NOT a rope (pure coordination with no victims) because majority applicants bear real costs. The constraint is NOT a snare (pure extraction with no coordination function) because the remedy genuinely addresses historical subordination. The constraint IS a Tangled Rope (both coordination and asymmetric extraction with required enforcement). The mandatrophy's resolution depends on acknowledging that the remedial goal (addressing group-based subordination) is legitimately coordinated WHILE acknowledging that the remedial mechanism (race-conscious classification) produces extraction from majority applicants. Both claims are true; both must be incorporated into the classification. Tangled Rope captures this duality. The temporal increase in theater_ratio (0.25 → 0.48) indicates that institutional remediation is degrading from substantive change toward procedural compliance, a piton signal that should trigger review of whether the remedial regime remains functional or has become inert institutional theater.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'Is this remedial reading of equal protection the authoritative interpretation, or is a colorblind reading (equal treatment regardless of race) or an antisubordination reading (equality measured by eliminating group subordination, not necessarily through race-consciousness) more aligned with constitutional commitment?',
    'Constitutional jurisprudence across different regimes; cross-national comparison of equal protection frameworks; historical analysis of legislative intent in civil rights statutes',
    'If remedial reading is authoritative: race-conscious measures are constitutionally mandated and high suppression of majority-group applicants is justified. If colorblind reading is authoritative: race-consciousness is unconstitutional and the constraint inverts (race-consciousness becomes snare for subordinated groups). If antisubordination reading is authoritative: remedies must be calibrated to outcome disparities, not racial classification per se, shifting enforcement mechanism and ε.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Which reading of equal protection commitment is authoritative: remedial, colorblind, or antisubordination?').

omega_variable(
    remedial_targeting_accuracy,
    'Do race-based classifications accurately target those who have suffered historical subordination, or do they create both false positives (majority-group individuals from disadvantaged socioeconomic backgrounds) and false negatives (minority-group individuals from advantaged backgrounds)?',
    'Intragenerational vs intergenerational benefits analysis; socioeconomic status distribution within racial groups; longitudinal tracking of remedial beneficiaries'' historical disadvantage correlation',
    'If targeting is accurate: remedial reading''s ε remains ~0.58 (legitimate coordination with measured extraction). If targeting is poor: ε should increase toward pure snare (extraction misdirected and excessive suppression of undeserving majority applicants; inadequate benefit for most-disadvantaged). If targeting is mixed: omegas on implementation variance across institutions.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(remedial_targeting_accuracy, empirical, 'Accuracy of race-based targeting in reaching historically subordinated individuals').

omega_variable(
    temporal_subordination_attenuation,
    'Do race-conscious remedies measurably reduce group-based outcome disparities over time, or do they stabilize/persist without converging to equality?',
    'Longitudinal tracking of educational attainment, income, wealth, health, incarceration disparities across racial groups; before/after analysis of remedial policy implementation; cross-national comparison of remedial regimes with different intensities',
    'If remedies converge disparity to near-zero: scaffold sunset logic is validated; temporal measurement should show extractiveness declining as subordination attenuates. If disparities persist despite remedies: extractiveness should increase (scaffolding not working as intended) or reclassify to Tangled Rope with higher ε if remedies prevent further subordination accumulation without eliminating existing disparity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(temporal_subordination_attenuation, empirical, 'Whether race-conscious remedies produce measurable convergence in group-based outcome disparities').

omega_variable(
    institutional_compliance_performativity,
    'To what extent do race-conscious remedial requirements become decoupled from the underlying goal of remedying subordination, instead evolving into performative compliance (diversity budgets, demographic accounting, theatrical commitment) that sustains appearance of remediation without substantive change?',
    'Comparison of institutional diversity statements vs actual power distribution; analysis of diversity officer role scope and influence; case studies of institutions that nominally comply with remedial requirements while maintaining subordinating structures',
    'If performativity is high (>0.50 theater_ratio): piton classification is correct. If performativity is low (<0.30): theater_ratio should be revised downward and Tangled Rope classification becomes more pronounced. Theater trajectory (rising or stable?) indicates whether remedial regime is functionally maturing (decline) or degrading (rise).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(institutional_compliance_performativity, empirical, 'Extent of performative compliance decoupling from substantive remediation goal').

omega_variable(
    majority_applicant_distributional_impact,
    'Are majority-group applicants who face race-conscious remedial disadvantage drawn from disadvantaged socioeconomic backgrounds (making them secondary victims of both class and remedial targeting), or primarily from privileged backgrounds (making them primary targets of remedial extraction)?',
    'Socioeconomic distribution analysis of majority-group applicants; biographical case documentation of rejected candidates; comparative advantage analysis within majority group',
    'If majority victims are primarily disadvantaged: suppression value should increase (unjust extraction from secondary victims) and victim group identification becomes more complex. If majority victims are primarily privileged: suppression remains justified as targeting relatively advantaged agents; tangled rope ε remains ~0.58 with clear beneficiary-victim asymmetry.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(majority_applicant_distributional_impact, empirical, 'Socioeconomic status of majority-group applicants experiencing remedial disadvantage').

omega_variable(
    identity_lock_in_beneficiary_identity,
    'Do beneficiaries of race-conscious remedies become identity-locked within the remedial regime (dependent on race-conscious classification for opportunities, unable to imagine themselves outside the remedial category), or do they experience the remedy as temporary scaffolding enabling exit into institutions/opportunities they could not access without it?',
    'Longitudinal interview data on beneficiaries'' perception of remedial regime durability; comparison of beneficiary career trajectories post-remedial gain; analysis of whether remedial beneficiaries advocate for perpetuation or sunset of remedial mechanisms',
    'If identity-locked: remedial reading becomes more extractive from beneficiary perspective (compensation for group membership perpetuates group identity as basis for access). If scaffold perception dominates: sunset logic is validated and beneficiaries see structural path to independence from race-consciousness.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_in_beneficiary_identity, empirical, 'Whether beneficiaries experience remedial regime as temporary scaffold or identity-constituting lock').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(remedial_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(reme_tr_t0, remedial_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(reme_tr_t10, remedial_reading, theater_ratio, 10, 0.38).
narrative_ontology:measurement(reme_tr_t20, remedial_reading, theater_ratio, 20, 0.48).

% Extraction over time
narrative_ontology:measurement(reme_be_t0, remedial_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(reme_be_t10, remedial_reading, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(reme_be_t20, remedial_reading, base_extractiveness, 20, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(remedial_reading, identity_coordination).
narrative_ontology:affects_constraint(remedial_reading, colorblind_reading).
narrative_ontology:affects_constraint(remedial_reading, antisubordination_reading).

% DUAL FORMULATION NOTE:
% The equal_protection_commitment kernel generates three structurally distinct constraints: (1) remedial_reading (ε=0.58, Tangled Rope) — race-consciousness required by remedial goal; (2) colorblind_reading (ε varies, Mountain or Snare candidate) — race-consciousness prohibited by equal protection principle; (3) antisubordination_reading (ε=varies, depends on empirical remedy design) — outcome parity required, classification method contingent. Each reading has different beneficiary/victim identification, different enforcement structures, different temporal dynamics. The remedial_reading specifically targets subordination-based benefit allocation; colorblind_reading treats all race-based sorting as prohibited; antisubordination_reading measures subordination by outcomes, not classification. Network links indicate family relationship and mutual contestation. Sibling readings should be consulted for full equal_protection architecture.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
