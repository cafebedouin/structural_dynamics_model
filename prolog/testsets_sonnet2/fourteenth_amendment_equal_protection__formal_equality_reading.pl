% ============================================================================
% CONSTRAINT STORY: fourteenth_amendment_equal_protection__formal_equality_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_fourteenth_amendment_equal_protection__formal_equality_reading, []).

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
 *   constraint_id: fourteenth_amendment_equal_protection__formal_equality_reading
 *   human_readable: Equal Protection — Formal Equality (Anticlassification) Reading
 *   domain: constitutional_law/civil_rights
 *
 * SUMMARY:
 *   This story instantiates the formal equality (anticlassification) reading
 *   of the Fourteenth Amendment's Equal Protection Clause: the state may not
 *   classify persons explicitly by race or status without surviving strict
 *   scrutiny, and this bar applies symmetrically to invidious and remedial
 *   classifications alike. Beginning from Brown's condemnation of
 *   state-imposed segregation, the doctrine hardened across Bakke, Croson,
 *   Adarand, Parents Involved, and Students for Fair Admissions into a rule
 *   that increasingly treats race-conscious remedial programs — set-asides,
 *   voluntary integration plans, affirmative action — as constitutionally
 *   suspect on the same terms as Jim Crow-era classification. The structural
 *   delta from the sibling anti-caste reading is exactly the point of this
 *   story: under formal equality, state corrective action enters the victim
 *   set (its administrators and intended beneficiaries bear the doctrine's
 *   costs) rather than being the thing the Amendment exists to enable, and
 *   pre-existing structural inequality is treated as constitutional
 *   background noise rather than a live wrong the state is obligated to
 *   remedy through race-conscious means. This is not the anti-caste reading
 *   measured differently — it is a structurally distinct claim with its own
 *   ε, its own beneficiaries, and its own victims, linked to the anti-caste
 *   reading only through the shared kernel.
 *
 * KEY AGENTS:
 *   - white_plaintiffs_in_reverse_discrimination_suits: Beneficiary (moderate/mobile) — collects standing to challenge race-conscious remedies
 *   - beneficiaries_of_affirmative_action_programs: Primary target (powerless/trapped) — bears the doctrine's remedial closure
 *   - federal_judiciary: Agenda-setter (institutional/analytical) — articulates and enforces the anticlassification test
 *   - communities_with_unremedied_structural_inequality: Excluded (powerless/trapped) — structural claim never enters the doctrinal frame
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fourteenth_amendment_equal_protection__formal_equality_reading, 0.42).
domain_priors:suppression_score(fourteenth_amendment_equal_protection__formal_equality_reading, 0.55).
domain_priors:theater_ratio(fourteenth_amendment_equal_protection__formal_equality_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fourteenth_amendment_equal_protection__formal_equality_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(fourteenth_amendment_equal_protection__formal_equality_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(fourteenth_amendment_equal_protection__formal_equality_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(fourteenth_amendment_equal_protection__formal_equality_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(fourteenth_amendment_equal_protection__formal_equality_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fourteenth_amendment_equal_protection__formal_equality_reading, tangled_rope).
narrative_ontology:human_readable(fourteenth_amendment_equal_protection__formal_equality_reading, "Equal Protection — Formal Equality (Anticlassification) Reading").
narrative_ontology:topic_domain(fourteenth_amendment_equal_protection__formal_equality_reading, "constitutional_law/civil_rights").

domain_priors:requires_active_enforcement(fourteenth_amendment_equal_protection__formal_equality_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(fourteenth_amendment_equal_protection__formal_equality_reading, 'c93cb3b8-d24b-46e6-8bee-586f2b8ab031').
narrative_ontology:cs_kernel_codification('c93cb3b8-d24b-46e6-8bee-586f2b8ab031', fixed_text).
narrative_ontology:cs_authority_grounding('c93cb3b8-d24b-46e6-8bee-586f2b8ab031', lineage).
narrative_ontology:cs_interpretation_layer_present('c93cb3b8-d24b-46e6-8bee-586f2b8ab031').
narrative_ontology:cs_reading_relation('c93cb3b8-d24b-46e6-8bee-586f2b8ab031', fourteenth_amendment_equal_protection__anti_caste_reading, coexists_with).
narrative_ontology:cs_axiom('c93cb3b8-d24b-46e6-8bee-586f2b8ab031', foundational, anticlassification_is_the_constitutional_command).
narrative_ontology:cs_axiom_status(anticlassification_is_the_constitutional_command, holdable).
narrative_ontology:cs_axiom_grounding('c93cb3b8-d24b-46e6-8bee-586f2b8ab031', anticlassification_is_the_constitutional_command, deontological).
narrative_ontology:cs_axiom('c93cb3b8-d24b-46e6-8bee-586f2b8ab031', foundational, individual_right_against_racial_classification_outweighs_group_remedial_purpose).
narrative_ontology:cs_axiom_status(individual_right_against_racial_classification_outweighs_group_remedial_purpose, holdable).
narrative_ontology:cs_axiom_grounding('c93cb3b8-d24b-46e6-8bee-586f2b8ab031', individual_right_against_racial_classification_outweighs_group_remedial_purpose, deontological).
narrative_ontology:cs_reference_frame('c93cb3b8-d24b-46e6-8bee-586f2b8ab031', brown_anticlassification_baseline).
narrative_ontology:cs_drift_state('c93cb3b8-d24b-46e6-8bee-586f2b8ab031', post_sffa_2023, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('c93cb3b8-d24b-46e6-8bee-586f2b8ab031', '').
narrative_ontology:cs_kernel_id(fourteenth_amendment_equal_protection__formal_equality_reading, fourteenth_amendment_equal_protection).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fourteenth_amendment_equal_protection__formal_equality_reading, white_plaintiffs_in_reverse_discrimination_suits).
narrative_ontology:constraint_beneficiary(fourteenth_amendment_equal_protection__formal_equality_reading, institutions_seeking_colorblind_compliance_cover).
narrative_ontology:constraint_beneficiary(fourteenth_amendment_equal_protection__formal_equality_reading, opponents_of_race_conscious_remedial_programs).
narrative_ontology:constraint_victim(fourteenth_amendment_equal_protection__formal_equality_reading, beneficiaries_of_affirmative_action_programs).
narrative_ontology:constraint_victim(fourteenth_amendment_equal_protection__formal_equality_reading, minority_contractors_under_set_aside_programs).
narrative_ontology:constraint_victim(fourteenth_amendment_equal_protection__formal_equality_reading, school_districts_pursuing_voluntary_integration).
narrative_ontology:constraint_victim(fourteenth_amendment_equal_protection__formal_equality_reading, state_corrective_action_administrators).
narrative_ontology:constraint_vindicates(fourteenth_amendment_equal_protection__formal_equality_reading, colorblind_constitution_doctrine).
narrative_ontology:constraint_vindicates(fourteenth_amendment_equal_protection__formal_equality_reading, individual_rights_over_group_remedy_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Bring suits challenging admissions or contracting programs that use explicit racial classification, arguing that any racial classification burdening them individually triggers strict scrutiny regardless of remedial purpose. Courts applying this reading treat their individual claim as constitutionally weightier than the group-level remedial rationale on the other side.
narrative_ontology:constraint_stakeholder(fourteenth_amendment_equal_protection__formal_equality_reading, white_plaintiffs_in_reverse_discrimination_suits, beneficiary,
    moderate, biographical, mobile, national).

% Employers, universities, and agencies that would prefer not to run race-conscious remedial programs use the formal equality reading as legal cover to dismantle or avoid such programs, framing compliance with anticlassification doctrine as itself the constitutional mandate rather than a constraint on a separate mandate to remedy.
narrative_ontology:constraint_stakeholder(fourteenth_amendment_equal_protection__formal_equality_reading, institutions_seeking_colorblind_compliance_cover, beneficiary,
    institutional, generational, arbitrage, national).

% Historically excluded groups who would benefit from race-conscious admissions, hiring, or contracting preferences find those programs invalidated or chilled because the state cannot classify by race to remedy the effects of prior classification by race, absent a narrowly tailored compelling interest showing that is deliberately hard to clear. They bear the cost of a rule they had no hand in framing to their disadvantage.
narrative_ontology:constraint_stakeholder(fourteenth_amendment_equal_protection__formal_equality_reading, beneficiaries_of_affirmative_action_programs, payer,
    powerless, biographical, trapped, national).

% Rely on government contracting set-asides designed to offset documented historical exclusion from public contracting; those set-asides are struck or narrowed under strict scrutiny because they classify by race, even where the underlying exclusion is well documented. Alternative race-neutral proxies capture far fewer of the intended beneficiaries.
narrative_ontology:constraint_stakeholder(fourteenth_amendment_equal_protection__formal_equality_reading, minority_contractors_under_set_aside_programs, payer,
    powerless, biographical, constrained, regional).

% Adopt voluntary school assignment plans using race as a factor to prevent resegregation; these plans are struck under the same anticlassification logic applied to invidious discrimination, collapsing the distinction between segregative and integrative uses of racial classification. Districts must redesign around race-neutral proxies that are demonstrably less effective at the stated integration goal.
narrative_ontology:constraint_stakeholder(fourteenth_amendment_equal_protection__formal_equality_reading, school_districts_pursuing_voluntary_integration, payer,
    moderate, generational, constrained, regional).

% Legislatures and agencies that wish to design explicit racial remedies for documented structural harms must instead satisfy strict scrutiny's narrow tailoring and compelling interest requirements, which the doctrine sets deliberately high; most such administrators are also the entities whose enforcement discretion is being constrained by the reading, making them both rule-administrators and rule-payers.
narrative_ontology:constraint_stakeholder(fourteenth_amendment_equal_protection__formal_equality_reading, state_corrective_action_administrators, payer,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(fourteenth_amendment_equal_protection__formal_equality_reading, state_corrective_action_administrators, agenda_setter).

% Courts, particularly the Supreme Court, articulate and enforce the anticlassification principle, deciding case by case which racial classifications survive strict scrutiny. Their doctrinal choices determine the reading's reach and are themselves contested along ideological lines within the judiciary.
narrative_ontology:constraint_stakeholder(fourteenth_amendment_equal_protection__formal_equality_reading, federal_judiciary, agenda_setter,
    institutional, generational, analytical, national).

% Communities whose disadvantage traces to historical state-sponsored racial classification (redlining, exclusion from GI Bill benefits, segregated schooling) have no direct voice in anticlassification doctrine's case-by-case development; the doctrine treats the structural inequality they inherited as pre-constitutional background rather than a live constitutional wrong requiring a state remedy, so their interest in continued corrective action is not itself a party to the anticlassification litigation.
narrative_ontology:constraint_stakeholder(fourteenth_amendment_equal_protection__formal_equality_reading, communities_with_unremedied_structural_inequality, excluded,
    powerless, civilizational, trapped, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(fourteenth_amendment_equal_protection__formal_equality_reading, diffuse).
narrative_ontology:fixing_cost_class(fourteenth_amendment_equal_protection__formal_equality_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single, administrable rule — government may not classify persons by race or status without surviving strict scrutiny — that lets courts adjudicate a huge range of disparate claims (admissions, contracting, redistricting, employment) under one doctrinal test rather than case-by-case balancing of group history and social context.
% TRANSFER_FUNCTION: Moves the burden of proof and the practical benefit of the doctrine from groups seeking race-conscious remedy to individuals and institutions objecting to being classified by race; narrows the toolkit available to state actors attempting to correct documented historical racial harms, effectively transferring continued disadvantage onto communities the remedial programs targeted.
% ABSENT_VOICES: Communities whose disadvantage originates in historical state racial classification are not parties to the individual-plaintiff litigation that develops and hardens anticlassification doctrine; their structural claim is treated as outside the frame the doctrine adjudicates, so they never get to argue that the 'classification' at issue is corrective rather than invidious.
% DISAPPEARANCE_RATIONALE: If the formal equality reading disappeared and the anti-caste reading fully displaced it, race-conscious admissions, contracting set-asides, and voluntary integration plans would face far lower scrutiny; large administrative and litigation machinery currently built around 'narrow tailoring' and 'compelling interest' proof would be substantially reorganized, and the population of programs currently struck down or chilled would expand significantly.
% FOUNDING_PROBLEM: The formal equality reading was built to prevent the state from ever again using racial classification as an instrument of subordination, drawing directly on the post-Reconstruction and Jim Crow history in which 'benign' and 'separate but equal' classifications were used to entrench racial hierarchy; the anticlassification rule aims to foreclose that entire toolkit permanently, including when the state's professed purpose is remedial.
% FOUNDING_PROBLEM_CORROBORATION: Originalist and formalist legal scholars and several sitting justices attest the founding problem (state weaponization of racial classification) remains live and that any exception invites its recurrence. Civil rights historians, sociologists of stratification, and dissenting justices outside the doctrine's beneficiary set attest that the founding problem targeted by Reconstruction-era framers was structural racial subordination itself, not classification as such, and that the formal equality reading has been redirected against the very remedial programs the Fourteenth Amendment's framers authorized in the Freedmen's Bureau legislation passed contemporaneously with the Amendment.
narrative_ontology:disappearance_verdict(fourteenth_amendment_equal_protection__formal_equality_reading, world_rearranges).
narrative_ontology:founding_problem_status(fourteenth_amendment_equal_protection__formal_equality_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(fourteenth_amendment_equal_protection__formal_equality_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(fourteenth_amendment_equal_protection__formal_equality_reading, 'none', 1).
narrative_ontology:epsilon_provenance(fourteenth_amendment_equal_protection__formal_equality_reading, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(fourteenth_amendment_equal_protection__formal_equality_reading_tests).
:- end_tests(fourteenth_amendment_equal_protection__formal_equality_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.42) and rising over the measured interval because the doctrine's practical bite against remedial programs has sharpened case by case — from Bakke's narrow allowance of race as one factor, through Croson and Adarand's strict scrutiny for set-asides, to Parents Involved and SFFA's near-categorical rejection of race-conscious admissions and assignment. Suppression is higher (0.55) because the doctrine's persistence depends on active judicial enforcement striking down state remedial programs that would otherwise proliferate; this is not passive coordination, it is continuously litigated and continuously re-imposed. Theater ratio is moderate-low (0.28): the doctrine performs a genuine coordinating function (a single administrable rule across many domains) but an increasing share of its operation is symbolic commitment to 'colorblindness' that outpaces any showing that classification-blindness actually reduces racial subordination on the ground.
 *
 * DIRECTIONALITY LOGIC:
 *   White plaintiffs and institutions preferring colorblind compliance sit near the beneficiary end of directionality: the rule hands them standing and doctrinal cover with low structural cost to themselves. Beneficiaries of affirmative action, minority contractors, and school districts pursuing voluntary integration sit near the target end: the same rule closes off tools they need and were, under a different reading, entitled to use. State corrective action administrators occupy a dual position — they administer enforcement of a rule that simultaneously constrains their own remedial discretion, which is why they carry both agenda_setter and payer roles. The federal judiciary is the analytical agenda-setter whose doctrinal choices determine how far the reading reaches, but judges are themselves institutionally exit-options: 'analytical,' reflecting their structurally different relationship to the constraint's operation.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification prevents mislabeling this reading as pure extraction: it does perform real coordination work by giving courts one administrable test across a huge range of claims, and it responds to a genuine historical wrong (state-weaponized racial classification under Jim Crow). But it also requires active enforcement against a beneficiary class it did not originally target (state actors attempting integration or remedy), producing a tangled_rope rather than a rope: coordination and extraction riding the same doctrinal machinery, distinguishable only by which classification and which purpose a court is willing to credit.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    compelling_interest_threshold_stability,
    'Is the strict-scrutiny ''compelling interest'' bar for race-conscious remedies a stable, principled threshold, or does its content shift opportunistically depending on whether the classification burdens or benefits historically subordinated groups?',
    'Comparative doctrinal analysis of case outcomes where the state''s asserted interest is remedying documented past discrimination versus other asserted state interests (national security, electoral districting) at comparable evidentiary strength; track whether remedial interests are held to a higher evidentiary bar than non-remedial compelling interests.',
    'If the bar is applied asymmetrically higher against remedial classifications, that supports the anti-caste reading''s claim that formal equality doctrine is substantively anti-remedial rather than neutrally anticlassification; if applied symmetrically, that supports the formal equality reading''s claim to principled neutrality.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(compelling_interest_threshold_stability, empirical, 'Whether strict scrutiny''s compelling-interest bar is neutral across remedial and non-remedial state purposes.').

omega_variable(
    kernel_reading_which_is_the_true_amendment,
    'Does the Fourteenth Amendment''s original public meaning and legislative history (contemporaneous Freedmen''s Bureau and Civil Rights Act legislation) support the formal equality (anticlassification) reading or the anti-caste (antisubordination) reading as the Amendment''s core commitment?',
    'This is the kernel-level disagreement itself and is not resolvable within this single reading''s story; the two readings (formal_equality_reading here, anti_caste_reading as sibling) are authored as separate constraints precisely because they give incompatible answers. Resolution, if any, would come from historical and doctrinal scholarship establishing which reading better fits the Amendment''s drafting history and immediate post-ratification implementation.',
    'If the anti-caste reading is the historically correct one, this story''s claimed_type and victim/beneficiary structure describe a doctrinal drift away from the Amendment''s founding purpose rather than a faithful application of it; if the formal equality reading is correct, the anti-caste reading is the drift.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_which_is_the_true_amendment, conceptual, 'Kernel-level disagreement about which reading captures the Fourteenth Amendment''s original commitment; not resolved within this story by design.').

omega_variable(
    structural_inequality_as_background_or_wrong,
    'Is documented structural racial inequality that traces to prior state action properly treated as constitutional background (a social fact courts do not remedy directly) or as a continuing constitutional wrong the state is obligated to correct through race-conscious means?',
    'Track whether courts applying the formal equality reading acknowledge documented causal chains from historical state action (redlining, exclusionary GI Bill administration, de jure segregation) to present disparities, and whether that acknowledgment ever changes the scrutiny applied to a remedial classification.',
    'If courts consistently decline to weight documented causal history in the scrutiny analysis, that corroborates this reading''s authored victim set (state corrective action administrators and intended beneficiaries) as a stable structural feature rather than a contingent litigation outcome.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(structural_inequality_as_background_or_wrong, conceptual, 'Whether structural inequality counts as a live constitutional wrong or inert background under this reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fourteenth_amendment_equal_protection__formal_equality_reading, 1954, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(four_tr_t1954, fourteenth_amendment_equal_protection__formal_equality_reading, theater_ratio, 1954, 0.1).
narrative_ontology:measurement(four_tr_t1978, fourteenth_amendment_equal_protection__formal_equality_reading, theater_ratio, 1978, 0.15).
narrative_ontology:measurement(four_tr_t1995, fourteenth_amendment_equal_protection__formal_equality_reading, theater_ratio, 1995, 0.2).
narrative_ontology:measurement(four_tr_t2007, fourteenth_amendment_equal_protection__formal_equality_reading, theater_ratio, 2007, 0.24).
narrative_ontology:measurement(four_tr_t2016, fourteenth_amendment_equal_protection__formal_equality_reading, theater_ratio, 2016, 0.26).
narrative_ontology:measurement(four_tr_t2023, fourteenth_amendment_equal_protection__formal_equality_reading, theater_ratio, 2023, 0.28).

% Extraction over time
narrative_ontology:measurement(four_be_t1954, fourteenth_amendment_equal_protection__formal_equality_reading, base_extractiveness, 1954, 0.12).
narrative_ontology:measurement(four_be_t1978, fourteenth_amendment_equal_protection__formal_equality_reading, base_extractiveness, 1978, 0.2).
narrative_ontology:measurement(four_be_t1995, fourteenth_amendment_equal_protection__formal_equality_reading, base_extractiveness, 1995, 0.3).
narrative_ontology:measurement(four_be_t2007, fourteenth_amendment_equal_protection__formal_equality_reading, base_extractiveness, 2007, 0.36).
narrative_ontology:measurement(four_be_t2016, fourteenth_amendment_equal_protection__formal_equality_reading, base_extractiveness, 2016, 0.38).
narrative_ontology:measurement(four_be_t2023, fourteenth_amendment_equal_protection__formal_equality_reading, base_extractiveness, 2023, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(four_su_t1954, fourteenth_amendment_equal_protection__formal_equality_reading, suppression_requirement, 1954, 0.2).
narrative_ontology:measurement(four_su_t1978, fourteenth_amendment_equal_protection__formal_equality_reading, suppression_requirement, 1978, 0.32).
narrative_ontology:measurement(four_su_t1995, fourteenth_amendment_equal_protection__formal_equality_reading, suppression_requirement, 1995, 0.42).
narrative_ontology:measurement(four_su_t2007, fourteenth_amendment_equal_protection__formal_equality_reading, suppression_requirement, 2007, 0.48).
narrative_ontology:measurement(four_su_t2016, fourteenth_amendment_equal_protection__formal_equality_reading, suppression_requirement, 2016, 0.5).
narrative_ontology:measurement(four_su_t2023, fourteenth_amendment_equal_protection__formal_equality_reading, suppression_requirement, 2023, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fourteenth_amendment_equal_protection__formal_equality_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(fourteenth_amendment_equal_protection__formal_equality_reading, anti_caste_reading).

% DUAL FORMULATION NOTE:
% This story and anti_caste_reading are two structurally distinct constraints emitted from the same contested kernel (fourteenth_amendment_equal_protection). They are not the same constraint viewed under different observables: they disagree about who the beneficiary and victim classes are, about whether structural inequality is background or a live wrong, and about the compelling-interest threshold's application to remedial state action. Per the ε-invariance principle, each reading carries its own ε, its own claimed_type, and its own stakeholder set; the two files are linked here rather than merged.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(fourteenth_amendment_equal_protection__formal_equality_reading, institutional, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
