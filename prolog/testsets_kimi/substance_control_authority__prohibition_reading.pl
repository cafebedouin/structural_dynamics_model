% ============================================================================
% CONSTRAINT STORY: substance_control_authority__prohibition_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
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
 *   constraint_id: substance_control_authority__prohibition_reading
 *   human_readable: State Drug Prohibition Authority (Prohibition Reading)
 *   domain: public_health_policy/criminal_justice/political_economy
 *
 * SUMMARY:
 *   This constraint instantiates the prohibition_reading of the
 *   substance_control_authority kernel. It models the exercise of state power
 *   to criminalize drug use and possession, justified as protecting
 *   third-party communities from drug-related crime and social disorder.
 *   Under this reading, drug users are structurally positioned as victims of
 *   incarceration and enforcement, while the carceral state apparatus
 *   captures budgetary flows, institutional mandate, and political capital.
 *   Racially targeted enforcement concentrates extraction on specific
 *   communities. The sibling harm_reduction_reading and legalization_reading
 *   relocate users from victim to beneficiary or agent status and replace
 *   criminalization with health or market mechanisms; those are separate
 *   constraints under epsilon-invariance.
 *
 * KEY AGENTS:
 *   - carceral_state_apparatus (institutional/constrained): agenda-setter and concentrated beneficiary â administers prohibition and captures budgetary and political rents
 *   - drug_users (powerless/trapped): primary target â bear incarceration, fines, and criminal records
 *   - racially_targeted_communities (powerless/trapped): secondary target â experience disparate enforcement and generational collateral damage
 *   - third_party_residents (moderate/constrained): claimed beneficiaries â receive contested safety benefits and pay tax costs
 *   - public_health_advocates (organized/constrained): excluded voice â would replace criminalization with health interventions but are kept out of the policy framework
 *   - civil_liberties_observers (organized/analytical): analytical seat â document disparities and efficacy failures without collecting from the constraint
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(substance_control_authority__prohibition_reading, 0.82).
domain_priors:suppression_score(substance_control_authority__prohibition_reading, 0.88).
domain_priors:theater_ratio(substance_control_authority__prohibition_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(substance_control_authority__prohibition_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(substance_control_authority__prohibition_reading, suppression_requirement, 0.88).
narrative_ontology:constraint_metric(substance_control_authority__prohibition_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(substance_control_authority__prohibition_reading, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(substance_control_authority__prohibition_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(substance_control_authority__prohibition_reading, tangled_rope).
narrative_ontology:human_readable(substance_control_authority__prohibition_reading, "State Drug Prohibition Authority (Prohibition Reading)").
narrative_ontology:topic_domain(substance_control_authority__prohibition_reading, "public_health_policy/criminal_justice/political_economy").

domain_priors:requires_active_enforcement(substance_control_authority__prohibition_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(substance_control_authority__prohibition_reading, '64a2c566-fa32-42b2-b1b2-42d6745c670b').
narrative_ontology:cs_kernel_codification('64a2c566-fa32-42b2-b1b2-42d6745c670b', formalized).
narrative_ontology:cs_authority_grounding('64a2c566-fa32-42b2-b1b2-42d6745c670b', lineage).
narrative_ontology:cs_interpretation_layer_present('64a2c566-fa32-42b2-b1b2-42d6745c670b').
narrative_ontology:cs_reading_relation('64a2c566-fa32-42b2-b1b2-42d6745c670b', substance_control_authority__harm_reduction_reading, coexists_with).
narrative_ontology:cs_reading_relation('64a2c566-fa32-42b2-b1b2-42d6745c670b', substance_control_authority__legalization_reading, coexists_with).
narrative_ontology:cs_axiom('64a2c566-fa32-42b2-b1b2-42d6745c670b', foundational, criminalization_as_protective_authority).
narrative_ontology:cs_axiom_status(criminalization_as_protective_authority, holdable).
narrative_ontology:cs_axiom_grounding('64a2c566-fa32-42b2-b1b2-42d6745c670b', criminalization_as_protective_authority, conventional).
narrative_ontology:cs_axiom('64a2c566-fa32-42b2-b1b2-42d6745c670b', foundational, deterrence_reduces_disorder).
narrative_ontology:cs_axiom_status(deterrence_reduces_disorder, holdable).
narrative_ontology:cs_axiom_grounding('64a2c566-fa32-42b2-b1b2-42d6745c670b', deterrence_reduces_disorder, empirically_contingent).
narrative_ontology:cs_reference_frame('64a2c566-fa32-42b2-b1b2-42d6745c670b', protective_police_power).
narrative_ontology:cs_drift_state('64a2c566-fa32-42b2-b1b2-42d6745c670b', mass_incarceration_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('64a2c566-fa32-42b2-b1b2-42d6745c670b', '').
narrative_ontology:cs_kernel_id(substance_control_authority__prohibition_reading, substance_control_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(substance_control_authority__prohibition_reading, carceral_state_apparatus).
narrative_ontology:constraint_beneficiary(substance_control_authority__prohibition_reading, third_party_residents).
narrative_ontology:constraint_victim(substance_control_authority__prohibition_reading, drug_users).
narrative_ontology:constraint_victim(substance_control_authority__prohibition_reading, racially_targeted_communities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers drug prohibition through policing, prosecution, and incarceration. Receives budget allocations, asset forfeiture, and institutional mandate from the constraint. Politically and fiscally dependent on the continuation of the drug war for a portion of its power and funding.
narrative_ontology:constraint_stakeholder(substance_control_authority__prohibition_reading, carceral_state_apparatus, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(substance_control_authority__prohibition_reading, carceral_state_apparatus, beneficiary).

% Subject to arrest, incarceration, fines, and criminal records for use or possession. Exit is structurally blocked by the criminalization of their behavior and the stigma that persists after release.
narrative_ontology:constraint_stakeholder(substance_control_authority__prohibition_reading, drug_users, payer,
    powerless, biographical, trapped, national).

% Experience disproportionate enforcement, sentencing, and collateral consequences despite similar drug-use rates to other populations. Geographic and economic concentration of policing makes exit from targeting nearly impossible.
narrative_ontology:constraint_stakeholder(substance_control_authority__prohibition_reading, racially_targeted_communities, payer,
    powerless, generational, trapped, national).

% Claimed beneficiaries of reduced drug-related crime and public disorder. Their actual exposure to harm is mediated by whether prohibition reduces or inflates black-market violence; they bear the tax cost of enforcement but are told they receive safety.
narrative_ontology:constraint_stakeholder(substance_control_authority__prohibition_reading, third_party_residents, beneficiary,
    moderate, biographical, constrained, national).

% Advocate for treating drug use as a health issue rather than a criminal one. Structurally excluded from the prohibition policy framework; their interventions such as needle exchange and safe supply are often criminalized or defunded under this reading.
narrative_ontology:constraint_stakeholder(substance_control_authority__prohibition_reading, public_health_advocates, excluded,
    organized, generational, constrained, national).

% Document racial disparities, constitutional violations, and efficacy failures of prohibition. Do not collect from or pay into the constraint; provide external analytical framing.
narrative_ontology:constraint_stakeholder(substance_control_authority__prohibition_reading, civil_liberties_observers, observer,
    organized, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(substance_control_authority__prohibition_reading, carceral_state_apparatus).
narrative_ontology:fixing_cost_class(substance_control_authority__prohibition_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Preventing drug-related crime and social disorder by deterring drug consumption through the threat and application of criminal sanctions, and removing drug users from public space via incarceration.
% TRANSFER_FUNCTION: Moves liberty, bodily autonomy, and life chances from drug users to the carceral state apparatus; moves tax revenue from the general public to enforcement and corrections budgets; moves a contested claim of safety to third-party residents.
% ABSENT_VOICES: Drug users are present in the system only as defendants, not as policy participants. Public health advocates and harm reduction practitioners are structurally excluded from the prohibition framework. Communities of color are overrepresented in enforcement but underrepresented in policy design.
% DISAPPEARANCE_RATIONALE: If the criminalization framework disappeared overnight, law enforcement and prison budgets would lose a primary mandate, illicit markets would shift toward regulated or decriminalized models, millions of current inmates would require release, and the political economy of the drug war would unravel.
% FOUNDING_PROBLEM: Perceived epidemic of drug use and associated crime and disorder in the mid-to-late 20th century, compounded by moral panic and racialized fear.
% FOUNDING_PROBLEM_CORROBORATION: Civil liberties organizations and public health researchers from outside the enforcement apparatus contest the severity and framing of the founding problem, citing racialized origins and evidence that prohibition itself generates crime. Some community organizations attest to genuine disorder but attribute it to the black market created by prohibition rather than to drug use per se.
narrative_ontology:disappearance_verdict(substance_control_authority__prohibition_reading, world_rearranges).
narrative_ontology:founding_problem_status(substance_control_authority__prohibition_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(substance_control_authority__prohibition_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(substance_control_authority__prohibition_reading, 'none', 1).
narrative_ontology:epsilon_provenance(substance_control_authority__prohibition_reading, 0.82, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness (0.82) is high because the constraint systematically transfers liberty and life chances from users to the state via incarceration, while generating fiscal and political rents for enforcement institutions. Suppression (0.88) reflects the active carceral machinery required to sustain prohibition. Theater_ratio (0.45) captures the mix of genuine coercion and political performance ('tough on crime' rhetoric decoupled from public safety outcomes). Accessibility_collapse (0.75) is high because criminal records and parole systems block exit, and policy alternatives such as harm reduction and legalization are structurally suppressed. Resistance (0.60) reflects sustained reform movements and state-level legalization experiments that challenge the federal prohibition framework. The metric series trace the ratchet of the War on Drugs: rapid escalation in the 1980s, peak severity in the 1990s-2000s, and modest moderation without structural reform in the 2010s-2020s.
 *
 * PERSPECTIVAL GAP:
 *   The carceral state apparatus experiences the constraint as a legitimate coordination mechanism generating order and institutional survival; drug users and targeted communities experience it as violent extraction. Third-party residents experience a contested mix of claimed safety benefits and realized tax costs. The engine will compute different per-seat classifications from this structural asymmetry.
 *
 * DIRECTIONALITY LOGIC:
 *   The carceral_state_apparatus is a concentrated beneficiary and agenda-setter (low d), subsidized by the constraint through budgets and mandate. Drug users and racially targeted communities are the primary targets (high d), with trapped exit options that amplify effective extraction. Third_party_residents are nominally beneficiaries but their benefit is diffuse and contested (mid-low d). Public_health_advocates are structurally excluded, receiving no directional position within the constraint's operation.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problemâdrug-related crime and disorderâis contested in both its original severity and its persistence. If the problem is dead or better solved by non-carceral means, the constraint persists as a zombie institution (mandatrophy). The prohibition reading resists mandatrophy resolution because the carceral apparatus captures concentrated gains while the costs are diffused across users and taxpayers, producing a political economy that sustains the constraint beyond its coordination justification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'This constraint is the prohibition_reading of the substance_control_authority kernel. Sibling readings (harm_reduction_reading, legalization_reading) relocate users from victim to beneficiary or agent status and replace criminalization with health or market mechanisms. Do these represent structurally distinct constraints under epsilon-invariance, or merely policy preferences within the same authority structure?',
    'Compare stakeholder directionality structures across readings: if the relocation of users from victim to beneficiary changes the sign of effective extraction for that seat, the readings are distinct constraints.',
    'If distinct, the kernel decomposes into separate constraints with divergent epsilon values and types; if preferences only, the same constraint is toggled by political coalitions.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Whether sibling readings are distinct constraints or preferences').

omega_variable(
    deterrence_efficacy,
    'Does criminalizing drug use actually protect third parties from drug-related crime and disorder through deterrence, or does prohibition inflate black-market violence and disorder beyond any deterrent effect?',
    'Comparative analysis of drug-related crime rates in jurisdictions before and after decriminalization or legalization; natural experiments from state-level cannabis reform.',
    'If deterrence is ineffective, the coordination story collapses and the constraint computes toward snare; if effective, tangled_rope classification is reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(deterrence_efficacy, empirical, 'Whether prohibition deterrence produces claimed protective benefits').

omega_variable(
    racial_disparity_intentionality,
    'Are the documented racial disparities in drug law enforcement an incidental byproduct of policing patterns, or a structural mechanism intentionally concentrating extraction on specific communities?',
    'Historical analysis of legislative intent (e.g., 1986 Anti-Drug Abuse Act sentencing disparities) and spatial enforcement data.',
    'If structural and intentional, extraction is amplified and identity-locked; if incidental, the asymmetry is less systematically extractive.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(racial_disparity_intentionality, empirical, 'Intentionality of racial disparity in enforcement').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression primarily structural (incarceration, policing) or internalized (stigma, self-censorship, isolation from reality-testing communities)?',
    'Post-legalization suppression trajectory: if drug users continue to avoid services or hide use after decriminalization, suppression is partially internalized.',
    'If internalized, effective suppression exceeds the structural measure; the constraint''s extractive reach persists even after formal removal.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, conceptual, 'Structural versus internalized suppression mechanism').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(substance_control_authority__prohibition_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sca_prohibition_tr_t0, substance_control_authority__prohibition_reading, theater_ratio, 0, 0.3).
narrative_ontology:measurement(sca_prohibition_tr_t5, substance_control_authority__prohibition_reading, theater_ratio, 5, 0.4).
narrative_ontology:measurement(sca_prohibition_tr_t10, substance_control_authority__prohibition_reading, theater_ratio, 10, 0.5).
narrative_ontology:measurement(sca_prohibition_tr_t15, substance_control_authority__prohibition_reading, theater_ratio, 15, 0.55).
narrative_ontology:measurement(sca_prohibition_tr_t20, substance_control_authority__prohibition_reading, theater_ratio, 20, 0.5).
narrative_ontology:measurement(sca_prohibition_tr_t25, substance_control_authority__prohibition_reading, theater_ratio, 25, 0.47).
narrative_ontology:measurement(sca_prohibition_tr_t30, substance_control_authority__prohibition_reading, theater_ratio, 30, 0.45).

% Extraction over time
narrative_ontology:measurement(sca_prohibition_be_t0, substance_control_authority__prohibition_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(sca_prohibition_be_t5, substance_control_authority__prohibition_reading, base_extractiveness, 5, 0.63).
narrative_ontology:measurement(sca_prohibition_be_t10, substance_control_authority__prohibition_reading, base_extractiveness, 10, 0.72).
narrative_ontology:measurement(sca_prohibition_be_t15, substance_control_authority__prohibition_reading, base_extractiveness, 15, 0.8).
narrative_ontology:measurement(sca_prohibition_be_t20, substance_control_authority__prohibition_reading, base_extractiveness, 20, 0.82).
narrative_ontology:measurement(sca_prohibition_be_t25, substance_control_authority__prohibition_reading, base_extractiveness, 25, 0.81).
narrative_ontology:measurement(sca_prohibition_be_t30, substance_control_authority__prohibition_reading, base_extractiveness, 30, 0.82).

% Suppression requirement over time
narrative_ontology:measurement(sca_prohibition_su_t0, substance_control_authority__prohibition_reading, suppression_requirement, 0, 0.65).
narrative_ontology:measurement(sca_prohibition_su_t5, substance_control_authority__prohibition_reading, suppression_requirement, 5, 0.75).
narrative_ontology:measurement(sca_prohibition_su_t10, substance_control_authority__prohibition_reading, suppression_requirement, 10, 0.85).
narrative_ontology:measurement(sca_prohibition_su_t15, substance_control_authority__prohibition_reading, suppression_requirement, 15, 0.9).
narrative_ontology:measurement(sca_prohibition_su_t20, substance_control_authority__prohibition_reading, suppression_requirement, 20, 0.88).
narrative_ontology:measurement(sca_prohibition_su_t25, substance_control_authority__prohibition_reading, suppression_requirement, 25, 0.87).
narrative_ontology:measurement(sca_prohibition_su_t30, substance_control_authority__prohibition_reading, suppression_requirement, 30, 0.88).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(substance_control_authority__prohibition_reading, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
