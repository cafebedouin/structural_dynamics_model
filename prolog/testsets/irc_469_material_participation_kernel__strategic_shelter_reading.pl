% ============================================================================
% CONSTRAINT STORY: irc_469_material_participation_kernel__strategic_shelter_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_irc_469_material_participation_kernel__strategic_shelter_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: irc_469_material_participation_kernel__strategic_shelter_reading
 *   human_readable: IRC 469 Material Participation as Strategic Tax Shelter (Permissive Reading)
 *   domain: tax_law/real_estate_investment/passive_loss_regulation
 *
 * SUMMARY:
 *   Under the Strategic Shelter Reading of IRC 469 material participation,
 *   the regulatory threshold for qualifying as a passive activity (and thus
 *   subject to passive loss limitations) is interpreted permissively,
 *   allowing high-income investors to structure real estate participation
 *   through aggressive hour-counting and grouping elections to retain
 *   deductibility of losses. The 1986 Tax Reform Act intended passive loss
 *   limitations to prevent wealthy taxpayers from sheltering ordinary income
 *   (wages, business profits) with real estate losses. This reading
 *   systematizes techniques to circumvent that intent while remaining within
 *   a plausible legal interpretation. The constraint exhibits a clear
 *   tangled_rope structure: genuine coordination function (the material
 *   participation threshold clarifies activity classification), asymmetric
 *   extraction (high-income strategists benefit; passive investors lose
 *   deductions; policy intent is undermined), and active enforcement (IRS
 *   audit, contemporaneous documentation requirements, grouping election
 *   elections require formal filing). The theater ratio reflects that
 *   enforcement is substantially performative — IRS audit rates are too low
 *   to credibly verify hour-counting claims, and the compliance
 *   infrastructure cannot reconstruct strategic grouping elections post-hoc.
 *   The extractiveness trajectory shows increasing optimization over the
 *   10-year interval as tax sheltering strategies become more sophisticated
 *   and widely adopted.
 *
 * KEY AGENTS:
 *   - High-Income Real Estate Investors: Primary beneficiary (institutional/arbitrage) — capture passive loss deductions despite minimal material participation; arbitrage between aggressive structuring and passive positioning
 *   - Tax Shelter Advisors: Primary beneficiary (institutional/arbitrage) — profit from hour-counting optimization, grouping election design, and documentation strategies
 *   - Passive Loss Limitation Policy Intent: Primary victim (powerless/trapped) — the 1986 legislative intent cannot organize or exit; bears full extraction as the permissive reading systematizes evasion
 *   - Passive Real Estate Investors: Secondary victim (moderate/constrained) — legitimate passive investors are excluded from deductions while aggressive strategists benefit; face resource barriers to strategic structuring
 *   - IRS Compliance Infrastructure: Institutional actor (institutional/arbitrage) — maintains performative enforcement; perceives enforcement mechanism as degraded but cannot exit without legislative authorization
 *   - Tax Reform Coalition: Organized agents (organized/mobile) — advocates for stricter definitions and real-time audit capability; perceive political window for regulatory tightening
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(irc_469_material_participation_kernel__strategic_shelter_reading, 0.58).
domain_priors:suppression_score(irc_469_material_participation_kernel__strategic_shelter_reading, 0.48).
domain_priors:theater_ratio(irc_469_material_participation_kernel__strategic_shelter_reading, 0.62).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(irc_469_material_participation_kernel__strategic_shelter_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(irc_469_material_participation_kernel__strategic_shelter_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(irc_469_material_participation_kernel__strategic_shelter_reading, theater_ratio, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(irc_469_material_participation_kernel__strategic_shelter_reading, tangled_rope).
narrative_ontology:human_readable(irc_469_material_participation_kernel__strategic_shelter_reading, "IRC 469 Material Participation as Strategic Tax Shelter (Permissive Reading)").
narrative_ontology:topic_domain(irc_469_material_participation_kernel__strategic_shelter_reading, "tax_law/real_estate_investment/passive_loss_regulation").

domain_priors:requires_active_enforcement(irc_469_material_participation_kernel__strategic_shelter_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(irc_469_material_participation_kernel__strategic_shelter_reading, '53a12fa6-a819-4033-a23b-4896726dac2c').
narrative_ontology:cs_kernel_codification('53a12fa6-a819-4033-a23b-4896726dac2c', formalized).
narrative_ontology:cs_authority_grounding('53a12fa6-a819-4033-a23b-4896726dac2c', extraction).
narrative_ontology:cs_interpretation_layer_present('53a12fa6-a819-4033-a23b-4896726dac2c').
narrative_ontology:cs_reading_relation('53a12fa6-a819-4033-a23b-4896726dac2c', irc_469_material_participation_kernel__strict_gatekeeper_reading, forecloses).
narrative_ontology:cs_axiom('53a12fa6-a819-4033-a23b-4896726dac2c', foundational, material_participation_as_deduction_optimization_boundary).
narrative_ontology:cs_axiom_status(material_participation_as_deduction_optimization_boundary, holdable).
narrative_ontology:cs_axiom_grounding('53a12fa6-a819-4033-a23b-4896726dac2c', material_participation_as_deduction_optimization_boundary, conventional).
narrative_ontology:cs_axiom('53a12fa6-a819-4033-a23b-4896726dac2c', foundational, aggressive_hour_counting_and_grouping_within_statutory_bounds_is_lawful).
narrative_ontology:cs_axiom_status(aggressive_hour_counting_and_grouping_within_statutory_bounds_is_lawful, holdable).
narrative_ontology:cs_axiom_grounding('53a12fa6-a819-4033-a23b-4896726dac2c', aggressive_hour_counting_and_grouping_within_statutory_bounds_is_lawful, empirically_contingent).
narrative_ontology:cs_reference_frame('53a12fa6-a819-4033-a23b-4896726dac2c', passive_loss_shelter_optimization_framework).
narrative_ontology:cs_drift_state('53a12fa6-a819-4033-a23b-4896726dac2c', contemporary_enforcement_atrophy_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('53a12fa6-a819-4033-a23b-4896726dac2c', '').
narrative_ontology:cs_kernel_id(irc_469_material_participation_kernel__strategic_shelter_reading, irc_469_material_participation_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(irc_469_material_participation_kernel__strategic_shelter_reading, high_income_real_estate_investors).
narrative_ontology:constraint_beneficiary(irc_469_material_participation_kernel__strategic_shelter_reading, tax_shelter_advisors).
narrative_ontology:constraint_victim(irc_469_material_participation_kernel__strategic_shelter_reading, passive_loss_limitation_policy_intent).
narrative_ontology:constraint_victim(irc_469_material_participation_kernel__strategic_shelter_reading, lower_income_passive_investors).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: POLICY INTENT (SNARE) — The 1986 Tax Reform Act's passive loss limitation was enacted to prevent high-income earners from sheltering ordinary income through real estate losses. This policy intent cannot exit; it bears full extraction from the permissive reading of material participation. The constraint systematizes the evasion of legislative purpose.
constraint_indexing:constraint_classification(irc_469_material_participation_kernel__strategic_shelter_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: PASSIVE REAL ESTATE INVESTOR (TANGLED ROPE) — Moderately constrained by the material participation threshold but also benefits from genuine coordination: the constraint clarifies what is and is not deductible, reducing ambiguity. However, bears asymmetric extraction: if their real economic participation is marginal, they are shut out of deductions while aggressive strategists who game the hour-counting rules benefit. Mixed extraction with coordination function.
constraint_indexing:constraint_classification(irc_469_material_participation_kernel__strategic_shelter_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: HIGH-INCOME INVESTOR (ROPE) — Benefits from the permissive threshold interpretation. Experiences the constraint as pure coordination: material participation rules clarify what is deductible, and the permissive reading enables lawful tax shelter. Net beneficiary with arbitrage options — can switch between aggressively structured and passive positions.
constraint_indexing:constraint_classification(irc_469_material_participation_kernel__strategic_shelter_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: TAX REFORM COALITION (SCAFFOLD) — Organized agents (tax reform advocates, policy researchers) see the permissive reading as a temporary institutional failure with a political sunset. Stricter passive loss limitations, tracking reforms (e.g., real-time audit trails), and definition tightening in future tax codes represent exit pathways. Low effective extraction because the coalition perceives agency and a policy window for change.
constraint_indexing:constraint_classification(irc_469_material_participation_kernel__strategic_shelter_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: IRS COMPLIANCE INFRASTRUCTURE (PITON) — The IRS theoretically enforces material participation standards through documentation and audit, but the enforcement mechanism is largely performative. Audit rates for individual returns are <1%; the IRS lacks resources to reconstruct hour-counting or grouping elections. The compliance theater persists (Schedule E documentation, contemporaneous records requirements) but enforcement capacity has atrophied. Piton classification reflects degraded function maintained through inertia.
constraint_indexing:constraint_classification(irc_469_material_participation_kernel__strategic_shelter_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, the constraint appears as an immutable technical boundary: distinguishing material from passive participation requires definition, and any definition creates boundary cases. Some gaming of thresholds is inevitable in any system. However, the structural data contradicts the mountain classification — the engine will detect this as a false summit, revealing that a technically 'inevitable' boundary is actually a contingent institutional choice with clear beneficiaries.
constraint_indexing:constraint_classification(irc_469_material_participation_kernel__strategic_shelter_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(irc_469_material_participation_kernel__strategic_shelter_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(irc_469_material_participation_kernel__strategic_shelter_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(irc_469_material_participation_kernel__strategic_shelter_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(irc_469_material_participation_kernel__strategic_shelter_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(irc_469_material_participation_kernel__strategic_shelter_reading, TR),
    TR >= 0.70.

:- end_tests(irc_469_material_participation_kernel__strategic_shelter_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderately high. The permissive reading enables material tax benefits (potentially 20–40% of reported losses) to high-income investors while excluding passive investors with lower documentation sophistication. The extraction is not total (compliant investors still face the threshold) but systematic and asymmetric. The rising trajectory (0.42 → 0.58) reflects increasing sophistication in hour-counting and grouping strategies as tax advisors optimize the permissive reading. Suppression (0.48): Moderate. Suppression mechanisms include IRS audit capacity (structurally insufficient — <1% audit rate for individuals), contemporaneous documentation requirements (imposing but not insurmountable for sophisticated actors), and time zone complexity (genuine skill barriers for unsophisticated actors). The declining trajectory (0.62 → 0.48) reflects increasing advisory capacity to manage documentation burden — suppression effectiveness erodes as strategies become standardized. Theater ratio (0.62): Moderate-high. Compliance theater includes contemporaneous time records, grouping election filings, and audit support documentation. This theater is substantial because IRS cannot verify the underlying facts (whether the taxpayer actually spent 100+ hours on each activity) given enforcement resource constraints. The rising trajectory reflects increasing theater elaboration as strategic documentation becomes more sophisticated — more detailed records are created for audit defense, not necessarily to reflect actual participation.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap is rooted in the kernel ambiguity: whether material participation is a definitional upper bound (permissive reading) or a gating threshold (strict reading). From the beneficiary's perspective, the rule is clear and coordinate — material participation delineates deductibility; optimizing within it is lawful tax planning. From the policy intent's perspective, the rule is systematically subverted — the permissive reading inverts the 1986 intent. From the passive investor's perspective, the rule creates asymmetry — compliance is rewarded with non-deductibility while strategic structuring is rewarded with deductibility. From the analytical observer's perspective, the ambiguity is not incidental but constitutional: IRC 469 contains the seeds of both readings, and the choice between them is a political judgment, not a technical determination.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is determined by each agent's structural relationship to the permissive interpretation. High-income investors and tax advisors are explicit beneficiaries with arbitrage options (can choose aggressive or passive structuring) — derived d ≈ 0.15, producing negative or low f(d), low χ from their perspective. The policy intent is an abstract victim with zero exit capacity — derived d ≈ 0.95, producing maximum f(d), maximum χ. Passive investors are victims with constrained exit (can comply but lose deductions) — derived d ≈ 0.75, producing high f(d), moderate-high χ. The IRS infrastructure is nominally institutional but captured by resource constraints and legislative authorization limits (constrained exit despite nominally powerful position) — derived d ≈ 0.55, producing moderate f(d), moderate χ. No directionality overrides are required; the derivation chain from structural data produces stable perspectival d values.
 *
 * MANDATROPHY ANALYSIS:
 *   KERNEL READING CONSTRAINT: Mandatrophy is resolved by recognizing that this is ONE reading of a contested kernel, not a universal classification. The strategic shelter reading instantiates a permissive interpretation (material participation as optimization frontier) that coexists with the strict gatekeeper reading (material participation as protective barrier). No single classification is 'correct' — the presheaf over the kernel contains both readings simultaneously held by different regulatory/interpretive communities. The mandatrophy is not 'which type?' but 'which reading are you assuming about what IRC 469 meant to accomplish?' The false summit risk is acute: the analytical observer can naturalize the permissive reading as 'inevitable technical boundary' when it is actually a contestable institutional choice. The kernel analysis (reading_relations, axioms, drift_state) documents this contestation within the JSON structure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    hours_counting_verification_gap,
    'Can contemporaneous time documentation for 100+ hours of unobserved real estate participation be reliably verified post-hoc?',
    'IRS audit data: correlation between claimed participation hours and documentary evidence found in audits; analysis of disallowance rates for material participation claims',
    'If verifiable: permissive reading is structurally sustainable — genuine participation is distinguishable from gaming. If unverifiable: suppression gate (can the agent credibly claim participation without evidence?) collapses, and the constraint is pure strategic extraction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(hours_counting_verification_gap, empirical, 'Whether hour-counting claims can be verified').

omega_variable(
    grouping_election_aggregation_intent,
    'Do grouping elections (treating multiple real estate ventures as a single activity for material participation aggregation) satisfy the spirit of passive loss limitation or systematically circumvent it?',
    'Legislative history analysis of 1986 Tax Reform Act''s passive loss limitation intent; empirical study of grouping election patterns and tax shelter outcomes; comparative analysis of similar jurisdictions with stricter grouping restrictions',
    'If grouping respects intent: the permissive reading is a legitimate policy interpretation. If grouping systematically enables evasion: the constraint is structurally designed extraction, and the tight tangled_rope classification should shift toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(grouping_election_aggregation_intent, conceptual, 'Whether grouping elections serve policy intent').

omega_variable(
    kernel_ambiguity_material_participation_definition,
    'Does IRC 469 establish material participation as a definitional ceiling (maximum deductibility without active participation) or a minimum threshold (must clear to qualify for any deduction)?',
    'Statutory language analysis (IRC 469(h)); legislative history; IRS regulation interpretation; case law jurisprudence across circuits',
    'If ceiling: permissive reading is justified — material participation is the upper bound. If minimum: strict reading is justified — material participation is the gating threshold. This is the core kernel ambiguity between readings.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_ambiguity_material_participation_definition, conceptual, 'Core ambiguity in material participation definition').

omega_variable(
    reading_foreclosure_structure,
    'Do the axioms of the permissive reading (passive loss rules as deduction optimization tools, not protective barriers) logically foreclose the strict reading (passive loss rules as mandatory gating requirements)?',
    'Logical analysis of axiom compatibility; examination of whether a single regulatory framework could hold both readings simultaneously without contradiction',
    'If forecloses: the two readings cannot coexist in a single legal interpretation — one must yield. If coexists: both readings are defensible within different institutional commitments or interpretive communities.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_foreclosure_structure, conceptual, 'Whether readings are logically incompatible').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(irc_469_material_participation_kernel__strategic_shelter_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(irc469_strat_tr_t0, irc_469_material_participation_kernel__strategic_shelter_reading, theater_ratio, 0, 0.48).
narrative_ontology:measurement(irc469_strat_tr_t5, irc_469_material_participation_kernel__strategic_shelter_reading, theater_ratio, 5, 0.55).
narrative_ontology:measurement(irc469_strat_tr_t10, irc_469_material_participation_kernel__strategic_shelter_reading, theater_ratio, 10, 0.62).

% Extraction over time
narrative_ontology:measurement(irc469_strat_be_t0, irc_469_material_participation_kernel__strategic_shelter_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(irc469_strat_be_t5, irc_469_material_participation_kernel__strategic_shelter_reading, base_extractiveness, 5, 0.5).
narrative_ontology:measurement(irc469_strat_be_t10, irc_469_material_participation_kernel__strategic_shelter_reading, base_extractiveness, 10, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(irc469_strat_su_t0, irc_469_material_participation_kernel__strategic_shelter_reading, suppression_requirement, 0, 0.62).
narrative_ontology:measurement(irc469_strat_su_t5, irc_469_material_participation_kernel__strategic_shelter_reading, suppression_requirement, 5, 0.55).
narrative_ontology:measurement(irc469_strat_su_t10, irc_469_material_participation_kernel__strategic_shelter_reading, suppression_requirement, 10, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(irc_469_material_participation_kernel__strategic_shelter_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(irc_469_material_participation_kernel__strategic_shelter_reading, irc_469_material_participation_kernel__strict_gatekeeper_reading).
narrative_ontology:affects_constraint(irc_469_material_participation_kernel__strategic_shelter_reading, passive_loss_limitation_audit_capacity_bottleneck).
narrative_ontology:affects_constraint(irc_469_material_participation_kernel__strategic_shelter_reading, real_estate_syndication_loss_acceleration).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the IRC 469 kernel; the strict_gatekeeper_reading is the sibling. The two readings share identical statutory text but differ fundamentally in their interpretation of material participation's role (optimization frontier vs protective barrier). Each reading has its own ε, its own beneficiary/victim structure, and its own perspectives. The network links them as reading siblings within the same kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
