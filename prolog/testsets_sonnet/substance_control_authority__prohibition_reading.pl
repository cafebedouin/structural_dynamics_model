% ============================================================================
% CONSTRAINT STORY: substance_control_authority__prohibition_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_substance_control_authority__prohibition_reading, []).

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
 *   constraint_id: substance_control_authority__prohibition_reading
 *   human_readable: Criminalization of Drug Use/Possession (Prohibition Reading)
 *   domain: public_health_policy/criminal_justice/political_economy
 *
 * SUMMARY:
 *   This story instantiates the prohibition reading of the
 *   substance_control_authority kernel: state authority to criminalize drug
 *   use and possession, justified as protecting third parties from
 *   drug-related crime and social disorder. It is one of three sibling
 *   readings of the same underlying kernel — the state's authority over
 *   psychoactive substance use — the others being a harm_reduction_reading
 *   (accept use, minimize health harms) and a legalization_reading (regulate
 *   as legal commerce). These are not measurement variants of one constraint;
 *   they are structurally distinct constraints with different victim sets,
 *   different enforcement mechanisms, and different epsilon values, generated
 *   as separate stories per the epsilon-invariance principle. This story's
 *   epsilon is fixed to what the prohibition reading actually does:
 *   incarceration as the primary mechanism, criminalization of the user (not
 *   only the trafficker or the third-party harm), and documented racial
 *   disparity in application.
 *
 * KEY AGENTS:
 *   - state_prohibition_authority: institutional agenda_setter — legislates and enforces possession criminalization
 *   - drug_users: powerless, trapped payer — bears criminal liability for use/possession itself
 *   - low_income_minority_communities: powerless, trapped payer — bears concentrated, disparate enforcement
 *   - carceral_industry_contractors: organized beneficiary — revenue scales with incarceration volume
 *   - law_enforcement_agencies: institutional beneficiary/agenda_setter — funding and leverage tied to arrest volume
 *   - prosecutorial_apparatus: institutional beneficiary — caseload and plea leverage from possession charges
 *   - residential_property_owners: moderate beneficiary — local deterrence effect on property/safety
 *   - public_health_practitioners: excluded — harm-reduction evidence largely outside statutory design
 *   - third_party_crime_victims: moderate beneficiary — the nominal deterrence beneficiaries
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(substance_control_authority__prohibition_reading, 0.71).
domain_priors:suppression_score(substance_control_authority__prohibition_reading, 0.88).
domain_priors:theater_ratio(substance_control_authority__prohibition_reading, 0.52).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(substance_control_authority__prohibition_reading, extractiveness, 0.71).
narrative_ontology:constraint_metric(substance_control_authority__prohibition_reading, suppression_requirement, 0.88).
narrative_ontology:constraint_metric(substance_control_authority__prohibition_reading, theater_ratio, 0.52).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(substance_control_authority__prohibition_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(substance_control_authority__prohibition_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(substance_control_authority__prohibition_reading, tangled_rope).
narrative_ontology:human_readable(substance_control_authority__prohibition_reading, "Criminalization of Drug Use/Possession (Prohibition Reading)").
narrative_ontology:topic_domain(substance_control_authority__prohibition_reading, "public_health_policy/criminal_justice/political_economy").

domain_priors:requires_active_enforcement(substance_control_authority__prohibition_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(substance_control_authority__prohibition_reading, 'f3b6bddd-b9dd-4f41-83d2-ef1ec1b0a59f').
narrative_ontology:cs_kernel_codification('f3b6bddd-b9dd-4f41-83d2-ef1ec1b0a59f', formalized).
narrative_ontology:cs_authority_grounding('f3b6bddd-b9dd-4f41-83d2-ef1ec1b0a59f', extraction).
narrative_ontology:cs_interpretation_layer_present('f3b6bddd-b9dd-4f41-83d2-ef1ec1b0a59f').
narrative_ontology:cs_reading_relation('f3b6bddd-b9dd-4f41-83d2-ef1ec1b0a59f', substance_control_authority__harm_reduction_reading, coexists_with).
narrative_ontology:cs_reading_relation('f3b6bddd-b9dd-4f41-83d2-ef1ec1b0a59f', substance_control_authority__legalization_reading, forecloses).
narrative_ontology:cs_axiom('f3b6bddd-b9dd-4f41-83d2-ef1ec1b0a59f', foundational, possession_itself_warrants_criminal_liability).
narrative_ontology:cs_axiom_status(possession_itself_warrants_criminal_liability, holdable).
narrative_ontology:cs_axiom_grounding('f3b6bddd-b9dd-4f41-83d2-ef1ec1b0a59f', possession_itself_warrants_criminal_liability, instrumental).
narrative_ontology:cs_axiom('f3b6bddd-b9dd-4f41-83d2-ef1ec1b0a59f', foundational, deterrence_through_incarceration_reduces_third_party_harm).
narrative_ontology:cs_axiom_status(deterrence_through_incarceration_reduces_third_party_harm, holdable).
narrative_ontology:cs_axiom_grounding('f3b6bddd-b9dd-4f41-83d2-ef1ec1b0a59f', deterrence_through_incarceration_reduces_third_party_harm, empirically_contingent).
narrative_ontology:cs_reference_frame('f3b6bddd-b9dd-4f41-83d2-ef1ec1b0a59f', deterrence_based_criminal_prohibition).
narrative_ontology:cs_drift_state('f3b6bddd-b9dd-4f41-83d2-ef1ec1b0a59f', post_mass_incarceration_evidence_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('f3b6bddd-b9dd-4f41-83d2-ef1ec1b0a59f', '').
narrative_ontology:cs_kernel_id(substance_control_authority__prohibition_reading, substance_control_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(substance_control_authority__prohibition_reading, carceral_industry_contractors).
narrative_ontology:constraint_beneficiary(substance_control_authority__prohibition_reading, law_enforcement_agencies).
narrative_ontology:constraint_beneficiary(substance_control_authority__prohibition_reading, residential_property_owners).
narrative_ontology:constraint_beneficiary(substance_control_authority__prohibition_reading, prosecutorial_apparatus).
narrative_ontology:constraint_victim(substance_control_authority__prohibition_reading, drug_users).
narrative_ontology:constraint_victim(substance_control_authority__prohibition_reading, low_income_minority_communities).
narrative_ontology:constraint_victim(substance_control_authority__prohibition_reading, nonviolent_possession_defendants).
narrative_ontology:constraint_victim(substance_control_authority__prohibition_reading, families_of_incarcerated_users).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(substance_control_authority__prohibition_reading, third_party_crime_victims).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Legislates and enforces criminal penalties for drug possession/use, framing the arrangement as protection of third parties from drug-related crime and disorder. Controls charging thresholds, sentencing guidelines, and enforcement priorities. Justifies continued criminalization by pointing to deterrence theory even where recidivism and use rates suggest limited deterrent effect.
narrative_ontology:constraint_stakeholder(substance_control_authority__prohibition_reading, state_prohibition_authority, agenda_setter,
    institutional, generational, analytical, national).

% Bear criminal liability for possession and use itself, independent of any harm caused to a third party. Face arrest, prosecution, incarceration, and collateral consequences (loss of housing, employment, custody, voting rights) that persist long after any sentence is served. Exit requires either abstaining entirely (foreclosed by addiction dynamics for a large subset) or successfully concealing use, which is itself a form of trappedness.
narrative_ontology:constraint_stakeholder(substance_control_authority__prohibition_reading, drug_users, payer,
    powerless, biographical, trapped, local).

% Experience concentrated enforcement — stop-and-search, buy-bust operations, and possession sweeps disproportionately target these neighborhoods relative to measured usage rates. Mass removal of working-age residents to incarceration destabilizes family and economic structures across generations. Geographic and economic constraints make relocation away from heavily policed areas largely unavailable.
narrative_ontology:constraint_stakeholder(substance_control_authority__prohibition_reading, low_income_minority_communities, payer,
    powerless, generational, trapped, regional).

% Operate private prisons, supply correctional services, and provide court-mandated treatment/monitoring programs whose revenue scales directly with possession arrest and incarceration volume. Lobby to preserve criminal penalties and resist diversion or decriminalization reforms that would shrink the population they bill against.
narrative_ontology:constraint_stakeholder(substance_control_authority__prohibition_reading, carceral_industry_contractors, beneficiary,
    organized, biographical, arbitrage, national).

% Receive federal and state grant funding, asset forfeiture proceeds, and staffing justifications tied to drug enforcement activity and arrest statistics. Possession statutes provide a low-evidentiary-bar charge that supports stop rates, informant recruitment, and plea leverage in unrelated investigations.
narrative_ontology:constraint_stakeholder(substance_control_authority__prohibition_reading, law_enforcement_agencies, beneficiary,
    institutional, biographical, arbitrage, regional).
narrative_ontology:stakeholder_secondary_role(substance_control_authority__prohibition_reading, law_enforcement_agencies, agenda_setter).

% Uses possession charges as leverage to extract plea deals and cooperation in larger cases, and as a routine caseload that demonstrates institutional activity to funders and electorates. Career advancement is partly measured by conviction volume, which possession statutes reliably supply.
narrative_ontology:constraint_stakeholder(substance_control_authority__prohibition_reading, prosecutorial_apparatus, beneficiary,
    institutional, biographical, arbitrage, regional).

% Benefit from neighborhood-level deterrence effects when visible drug activity is suppressed near their property, supporting property values and perceived safety. Can relocate if enforcement in their area weakens, unlike the populations who remain subject to it.
narrative_ontology:constraint_stakeholder(substance_control_authority__prohibition_reading, residential_property_owners, beneficiary,
    moderate, biographical, mobile, local).

% Argue from epidemiological and treatment-outcome evidence that criminalization deters neither use nor overdose and instead drives users away from testing, treatment, and emergency services for fear of arrest. Their harm-reduction framework is largely excluded from statutory design, which remains anchored in criminal-legal rather than public-health premises.
narrative_ontology:constraint_stakeholder(substance_control_authority__prohibition_reading, public_health_practitioners, excluded,
    organized, generational, constrained, national).

% Are the intended beneficiaries of the deterrence rationale — those who would otherwise suffer property crime, violence, or disorder attributable to drug markets. The degree to which possession criminalization (as opposed to trafficking or violent-crime enforcement specifically) actually reduces their victimization is contested and not separately measured by the statutes as applied.
narrative_ontology:constraint_stakeholder(substance_control_authority__prohibition_reading, third_party_crime_victims, beneficiary,
    moderate, immediate, constrained, local).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(substance_control_authority__prohibition_reading, diffuse).
narrative_ontology:fixing_cost_class(substance_control_authority__prohibition_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Deters drug-related crime and social disorder that third parties would otherwise bear, by attaching criminal liability to possession and use themselves rather than only to harms caused to others.
% TRANSFER_FUNCTION: Moves liberty, economic stability, and family continuity from drug users and the communities they live in toward carceral contractors, law enforcement budgets, and prosecutorial caseload metrics, nominally in exchange for reduced disorder experienced by third parties.
% ABSENT_VOICES: Public health practitioners and harm-reduction advocates, who would argue the deterrence rationale is empirically unsupported and that criminalization itself generates the disorder (untreated addiction, unregulated markets, violence over territory) it claims to prevent, are largely outside the statutory design process, which remains dominated by criminal-legal institutional actors.
% DISAPPEARANCE_RATIONALE: If possession/use criminalization vanished overnight, arrest volumes tied to drug charges would collapse, carceral population and associated contractor revenue would fall sharply, law enforcement funding streams tied to drug enforcement metrics would need restructuring, and millions of people currently living under threat of prosecution or with existing records would face a materially different set of life options. Whether third-party crime and disorder would rise, fall, or be unaffected is itself contested and central to the omega below.
% FOUNDING_PROBLEM: Perceived rising drug-related crime, violence, and community disorder in the mid-to-late 20th century, framed as requiring criminal deterrence of use and possession themselves as a means of suppressing the associated market and its externalities.
% FOUNDING_PROBLEM_CORROBORATION: State prohibition authorities and law enforcement agencies attest the founding problem remains live, citing ongoing drug-market violence and disorder. Independent criminological research, public health bodies (e.g., epidemiological overdose and incarceration-outcome studies), and legislative commissions in multiple jurisdictions that have studied decriminalization outcomes report that criminalization of use/possession specifically shows weak or no deterrent effect on the disorder it targets, suggesting the mechanism has substantially decoupled from the problem it was built to solve even where third-party harm from trafficking-adjacent violence persists.
narrative_ontology:disappearance_verdict(substance_control_authority__prohibition_reading, world_rearranges).
narrative_ontology:founding_problem_status(substance_control_authority__prohibition_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(substance_control_authority__prohibition_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(substance_control_authority__prohibition_reading, 'none', 1).
narrative_ontology:epsilon_provenance(substance_control_authority__prohibition_reading, 0.71, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(substance_control_authority__prohibition_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(substance_control_authority__prohibition_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(substance_control_authority__prohibition_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored high (0.71 at interval end) because the mechanism attaches criminal liability directly to the user's own possession/use rather than to demonstrated harm to a third party, and the resulting incarceration and collateral consequences fall overwhelmingly on users and their communities rather than on the trafficking or violence the deterrence rationale nominally targets. Suppression is authored very high (0.88) because the arrangement depends on active, continuous enforcement infrastructure (policing, prosecution, incarceration) and offers users essentially no lawful alternative once possession/use itself is the offense — there is no exit through moderated behavior short of complete abstention, which is not achievable for a large fraction of the affected population given addiction dynamics. Theater ratio is authored at a substantial 0.52 by interval end because a large and growing share of enforcement activity (arrest-volume metrics, asset forfeiture totals, caseload statistics) functions as institutional self-justification rather than measurable third-party protection, particularly as deterrence research has increasingly failed to show the promised effect on disorder. Accessibility collapse (0.62) and resistance (0.70) reflect that the criminalization framework has not fully foreclosed alternatives — harm-reduction and legalization reforms exist and are gaining ground in some jurisdictions — but resistance to the prohibition framework remains substantial and organized (public health bodies, decriminalization advocates, affected communities).
 *
 * DIRECTIONALITY LOGIC:
 *   Drug users and low-income minority communities are the structural targets: they bear criminal liability and disparate enforcement with essentially no exit (trapped), which the derivation chain correctly pushes toward the high-d/full-target end. Carceral contractors, law enforcement, and prosecutorial institutions are structural beneficiaries whose funding, caseload, and revenue scale with enforcement volume — their arbitrage-grade exit options (they can redirect resources or lobby for continuation) place them near the beneficiary end. Third-party crime victims and residential property owners are declared beneficiaries of the nominal deterrence function, but their exit options are only constrained/moderate rather than institutional, reflecting that they benefit from an externality of enforcement rather than controlling the mechanism itself.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope classification separates the genuine coordination claim (third-party protection from drug-related disorder) from the asymmetric extraction that has grown around it (carceral revenue, enforcement-metric careerism, disparate racialized application). This prevents two mislabelings: treating the whole arrangement as pure coordination (ignoring the victim set and enforcement dependency) and treating it as pure extraction with no genuine coordination claim at all (ignoring that some third-party protection interest is real and contested, not fabricated). The rising theater_ratio and extractiveness series document a coordination function increasingly displaced by institutional self-perpetuation — the classic tangled-rope drift pattern.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    deterrence_effect_of_possession_criminalization,
    'Does criminalizing use/possession itself (as distinct from trafficking or violent drug-market activity) actually reduce the third-party crime and disorder it is justified by, or does it merely relocate and sometimes amplify disorder by driving markets underground and destabilizing communities through mass incarceration?',
    'Comparative jurisdictional analysis of crime and disorder metrics before/after decriminalization or depenalization reforms (e.g., Portugal 2001, various U.S. state reforms), controlling for confounds; longitudinal recidivism and community-stability data.',
    'If deterrence effect is weak or absent, the coordination-function claim underlying this reading''s tangled_rope classification weakens substantially, pushing the computed type toward snare; if deterrence effect is robust and separable from incarceration harms, the coordination claim is stronger and the tangled_rope framing (rather than snare) is better supported.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(deterrence_effect_of_possession_criminalization, empirical, 'Whether possession/use criminalization causally reduces third-party drug-related crime and disorder.').

omega_variable(
    prohibition_reading_kernel_contest,
    'Is the prohibition reading the legitimate operative reading of state substance-control authority, or has it been substantially displaced by evidence supporting the harm_reduction_reading and legalization_reading as better solutions to the same underlying third-party-protection problem?',
    'This is the committer-level contest among the three sibling readings of the substance_control_authority kernel. Resolution would require either a jurisdiction fully replacing prohibition with one of the sibling frameworks and observing outcomes, or a normative/political settlement about which reading the state should adopt — the readings are not adjudicated by data alone since they also differ in underlying values (liberty vs. paternalism vs. public order).',
    'If the harm_reduction_reading or legalization_reading is adopted as the operative kernel reading in a given jurisdiction, this prohibition_reading constraint ceases to exist there (structurally different constraint, different victim set, different ε) rather than being ''reformed'' — the readings are mutually exclusive as operative legal regimes even though they can coexist across jurisdictions.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(prohibition_reading_kernel_contest, preference, 'Which sibling reading of the substance_control_authority kernel is the legitimate operative one.').

omega_variable(
    racial_disparity_natural_vs_constructed,
    'Is the documented racial disparity in enforcement application an incidental byproduct of neutral statutory design interacting with pre-existing socioeconomic geography, or a constructed/foreseeable feature of how possession statutes are selectively enforced?',
    'Comparison of usage-rate surveys (self-reported, roughly uniform across racial groups) against arrest and prosecution rate data by race and neighborhood; analysis of resource allocation decisions by law enforcement leadership.',
    'If disparity is substantially constructed rather than incidental, it strengthens the victim-set claim for low_income_minority_communities and supports higher extractiveness/suppression attribution to enforcement practice rather than statutory text alone.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(racial_disparity_natural_vs_constructed, empirical, 'Whether enforcement disparity is incidental or a constructed feature of how the statute is applied.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(substance_control_authority__prohibition_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(subs_tr_t0, substance_control_authority__prohibition_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(subs_tr_t8, substance_control_authority__prohibition_reading, theater_ratio, 8, 0.32).
narrative_ontology:measurement(subs_tr_t16, substance_control_authority__prohibition_reading, theater_ratio, 16, 0.38).
narrative_ontology:measurement(subs_tr_t24, substance_control_authority__prohibition_reading, theater_ratio, 24, 0.44).
narrative_ontology:measurement(subs_tr_t32, substance_control_authority__prohibition_reading, theater_ratio, 32, 0.49).
narrative_ontology:measurement(subs_tr_t40, substance_control_authority__prohibition_reading, theater_ratio, 40, 0.52).

% Extraction over time
narrative_ontology:measurement(subs_be_t0, substance_control_authority__prohibition_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(subs_be_t8, substance_control_authority__prohibition_reading, base_extractiveness, 8, 0.5).
narrative_ontology:measurement(subs_be_t16, substance_control_authority__prohibition_reading, base_extractiveness, 16, 0.58).
narrative_ontology:measurement(subs_be_t24, substance_control_authority__prohibition_reading, base_extractiveness, 24, 0.64).
narrative_ontology:measurement(subs_be_t32, substance_control_authority__prohibition_reading, base_extractiveness, 32, 0.68).
narrative_ontology:measurement(subs_be_t40, substance_control_authority__prohibition_reading, base_extractiveness, 40, 0.71).

% Suppression requirement over time
narrative_ontology:measurement(subs_su_t0, substance_control_authority__prohibition_reading, suppression_requirement, 0, 0.68).
narrative_ontology:measurement(subs_su_t8, substance_control_authority__prohibition_reading, suppression_requirement, 8, 0.74).
narrative_ontology:measurement(subs_su_t16, substance_control_authority__prohibition_reading, suppression_requirement, 16, 0.79).
narrative_ontology:measurement(subs_su_t24, substance_control_authority__prohibition_reading, suppression_requirement, 24, 0.83).
narrative_ontology:measurement(subs_su_t32, substance_control_authority__prohibition_reading, suppression_requirement, 32, 0.86).
narrative_ontology:measurement(subs_su_t40, substance_control_authority__prohibition_reading, suppression_requirement, 40, 0.88).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(substance_control_authority__prohibition_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(substance_control_authority__prohibition_reading, substance_control_authority__harm_reduction_reading).
narrative_ontology:affects_constraint(substance_control_authority__prohibition_reading, substance_control_authority__legalization_reading).

% DUAL FORMULATION NOTE:
% This story is one of three constraints instantiating the substance_control_authority kernel. prohibition_reading, harm_reduction_reading, and legalization_reading are structurally distinct constraints with different epsilon values, victim sets, and enforcement mechanisms — not three measurements of one constraint. prohibition_reading uniquely places drug users themselves in the victim set via criminalization and carries the highest suppression value among the three due to its dependence on incarceration as primary mechanism. Network edges here represent the same-kernel sibling relationship, not causal downstream influence in the usual sense — adoption of one reading in a jurisdiction structurally forecloses concurrent operation of the others there.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(substance_control_authority__prohibition_reading, organized, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
