% ============================================================================
% CONSTRAINT STORY: equal_protection_commitment__colorblind_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_equal_protection_commitment__colorblind_reading, []).

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
 *   constraint_id: equal_protection_commitment__colorblind_reading
 *   human_readable: State Racial Classification in Selective Admissions (Colorblind Reading)
 *   domain: Constitutional Law / Political Philosophy / Social Policy
 *
 * SUMMARY:
 *   From 1978 to 2023, selective American universities evaluated applicants
 *   with race as a factor, transferring admission probability toward
 *   historically underrepresented groups and sustaining the practice through
 *   judicial deference, institutional consensus, and the long exclusion of
 *   the colorblind challenge from doctrinal resolution. This story
 *   instantiates the colorblind_reading of the equal_protection_commitment
 *   kernel. Per the epsilon-referent rule for kernel readings, the referent
 *   is the standing arrangement under contest, race-conscious state
 *   classification in selective admissions, assessed by this reading's own
 *   lights: the classification itself is the harm, the disfavored classified
 *   applicants are the injured parties, and the administering institutions
 *   are the responsible operators. The colorblind prohibition is this
 *   reading's endorsed standard, not the referent, and it is deliberately not
 *   folded into the classification. KEY AGENTS (by structural relationship):
 *   asian_american_applicants, primary target (organized/constrained), bears
 *   the largest measured classification burden and is internally divided;
 *   white_applicants, secondary target (moderate/constrained);
 *   historically_underrepresented_admitted_applicants, primary beneficiary
 *   (moderate/constrained), receives the transferred admission probability;
 *   preference_administering_universities, agenda-setter and institutional
 *   beneficiary (institutional/arbitrage), administers the machinery and
 *   collects discretion and prestige; diversity_advocacy_establishment,
 *   organized beneficiary (organized/identity_locked);
 *   colorblind_constitutionalists, excluded challengers
 *   (organized/constrained) kept outside the settlement until 2023;
 *   supreme_court_of_the_united_states, doctrinal agenda-setter
 *   (institutional/constrained) that sustained and then terminated the
 *   arrangement; comparative_constitutional_scholars, analytical observer
 *   (analytical/analytical).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(equal_protection_commitment__colorblind_reading, 0.44).
domain_priors:suppression_score(equal_protection_commitment__colorblind_reading, 0.28).
domain_priors:theater_ratio(equal_protection_commitment__colorblind_reading, 0.46).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(equal_protection_commitment__colorblind_reading, extractiveness, 0.44).
narrative_ontology:constraint_metric(equal_protection_commitment__colorblind_reading, suppression_requirement, 0.28).
narrative_ontology:constraint_metric(equal_protection_commitment__colorblind_reading, theater_ratio, 0.46).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(equal_protection_commitment__colorblind_reading, accessibility_collapse, 0.52).
narrative_ontology:constraint_metric(equal_protection_commitment__colorblind_reading, resistance, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(equal_protection_commitment__colorblind_reading, snare).
narrative_ontology:human_readable(equal_protection_commitment__colorblind_reading, "State Racial Classification in Selective Admissions (Colorblind Reading)").
narrative_ontology:topic_domain(equal_protection_commitment__colorblind_reading, "Constitutional Law / Political Philosophy / Social Policy").

domain_priors:requires_active_enforcement(equal_protection_commitment__colorblind_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(equal_protection_commitment__colorblind_reading, 'e6f86d3c-c485-4327-8b10-a1e852f388ea').
narrative_ontology:cs_kernel_codification('e6f86d3c-c485-4327-8b10-a1e852f388ea', fixed_text).
narrative_ontology:cs_authority_grounding('e6f86d3c-c485-4327-8b10-a1e852f388ea', lineage).
narrative_ontology:cs_interpretation_layer_present('e6f86d3c-c485-4327-8b10-a1e852f388ea').
narrative_ontology:cs_reading_relation('e6f86d3c-c485-4327-8b10-a1e852f388ea', equal_protection_commitment__remedial_reading, forecloses).
narrative_ontology:cs_reading_relation('e6f86d3c-c485-4327-8b10-a1e852f388ea', equal_protection_commitment__diversity_reading, forecloses).
narrative_ontology:cs_axiom('e6f86d3c-c485-4327-8b10-a1e852f388ea', foundational, state_racial_classification_per_se_unconstitutional).
narrative_ontology:cs_axiom_status(state_racial_classification_per_se_unconstitutional, holdable).
narrative_ontology:cs_axiom_grounding('e6f86d3c-c485-4327-8b10-a1e852f388ea', state_racial_classification_per_se_unconstitutional, deontological).
narrative_ontology:cs_axiom('e6f86d3c-c485-4327-8b10-a1e852f388ea', secondary, formal_equal_treatment_lexically_prior_to_outcome_equity).
narrative_ontology:cs_axiom_status(formal_equal_treatment_lexically_prior_to_outcome_equity, holdable).
narrative_ontology:cs_axiom_grounding('e6f86d3c-c485-4327-8b10-a1e852f388ea', formal_equal_treatment_lexically_prior_to_outcome_equity, deontological).
narrative_ontology:cs_reference_frame('e6f86d3c-c485-4327-8b10-a1e852f388ea', harlan_colorblind_citizenship).
narrative_ontology:cs_drift_state('e6f86d3c-c485-4327-8b10-a1e852f388ea', post_sffa_contemporary, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('e6f86d3c-c485-4327-8b10-a1e852f388ea', '').
narrative_ontology:cs_kernel_id(equal_protection_commitment__colorblind_reading, equal_protection_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(equal_protection_commitment__colorblind_reading, historically_underrepresented_admitted_applicants).
narrative_ontology:constraint_beneficiary(equal_protection_commitment__colorblind_reading, preference_administering_universities).
narrative_ontology:constraint_beneficiary(equal_protection_commitment__colorblind_reading, diversity_advocacy_establishment).
narrative_ontology:constraint_victim(equal_protection_commitment__colorblind_reading, asian_american_applicants).
narrative_ontology:constraint_victim(equal_protection_commitment__colorblind_reading, white_applicants).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Applied to selective universities that, during the period, evaluated candidates with race as a factor; admissions-data research indicated they faced the highest effective bar of any racial group at several elite institutions. They could not decline to be racially categorized, and seeking admission outside the selective channel meant forfeiting the tier where credentials carry the largest lifetime returns. The category is internally divided: national organizations representing segments of it filed briefs defending race-conscious admissions even as other segments supplied the plaintiffs and evidence for the challenge.
narrative_ontology:constraint_stakeholder(equal_protection_commitment__colorblind_reading, asian_american_applicants, payer,
    organized, biographical, constrained, national).

% Faced race as a negative factor at selective institutions during the period, with smaller measured effects than for Asian-American applicants. Attitudes within the group skew heavily against racial preferences, giving the challenge durable electoral and donor support; individual applicants nonetheless held the same bounded choice set as everyone else in the channel: absorb the classification or pursue admission elsewhere.
narrative_ontology:constraint_stakeholder(equal_protection_commitment__colorblind_reading, white_applicants, payer,
    moderate, biographical, constrained, national).

% Received admission offers at selective institutions under processes in which race contributed positively to the evaluation. The advantage concentrated at the most selective tier, where access translates into outsized economic and social returns. Many recipients and their communities describe the consideration of race as recognition of obstacles overcome; others describe having their admission associated with their race as a burden they never chose.
narrative_ontology:constraint_stakeholder(equal_protection_commitment__colorblind_reading, historically_underrepresented_admitted_applicants, beneficiary,
    moderate, biographical, constrained, national).

% Designed and operated the evaluation systems that assigned weight to race, defended them through successive lawsuits, and adjusted mechanics whenever courts pushed back, shifting weight toward essays, personal ratings, and adversity factors after 2023. They gained discretionary control over class composition, alignment with peer institutions, and freedom from federal-compliance pressure during the deference era; they bore heavy litigation costs and, after 2023, the forced restructuring of their admissions machinery.
narrative_ontology:constraint_stakeholder(equal_protection_commitment__colorblind_reading, preference_administering_universities, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(equal_protection_commitment__colorblind_reading, preference_administering_universities, beneficiary).

% Civil-rights organizations, higher-education associations, and portions of the legal academy whose programs, funding streams, and professional standing are bound to race-conscious remediation as the operative theory of equality. Defending the mechanism is constitutive of their institutional identity; abandoning it would require re-founding their account of justice, which is why they litigated, lobbied, and mobilized at every juncture from Bakke through SFFA.
narrative_ontology:constraint_stakeholder(equal_protection_commitment__colorblind_reading, diversity_advocacy_establishment, beneficiary,
    organized, generational, identity_locked, national).

% Litigators, scholars, and advocacy organizations holding that the Constitution forbids state racial classification outright. For roughly four decades they stood outside the governing doctrinal settlement: courts repeatedly declined to resolve their challenges, and professional incentives penalized the position. They supplied the litigation strategy, the plaintiffs, and the statistical evidence that finally reached the Supreme Court in 2023.
narrative_ontology:constraint_stakeholder(equal_protection_commitment__colorblind_reading, colorblind_constitutionalists, excluded,
    organized, generational, constrained, national).

% Held the decisive gate throughout: it created the diversity pathway in 1978 while refusing to adopt the colorblind rule, maintained the pathway through 2003 and 2016, and in 2023 withdrew deference and barred race-conscious admissions outright. Its doctrinal choices, not any statute, defined what the arrangement permitted at each stage, and it remained bound by its own precedents and the amendment text while doing so.
narrative_ontology:constraint_stakeholder(equal_protection_commitment__colorblind_reading, supreme_court_of_the_united_states, agenda_setter,
    institutional, civilizational, constrained, national).

% Track how different democracies handle state racial classification, including India's scheduled-caste reservations, Brazil's quota systems, and France's prohibition on ethnic statistics, and can situate the American dispute against systems that never adopted a colorblind baseline.
narrative_ontology:constraint_stakeholder(equal_protection_commitment__colorblind_reading, comparative_constitutional_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(equal_protection_commitment__colorblind_reading, historically_underrepresented_admitted_applicants).
narrative_ontology:fixing_cost_class(equal_protection_commitment__colorblind_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The arrangement coordinated selective admissions into a managed distribution of seats across racial groups, and coordinated elite-institution legitimacy with civil-rights-era expectations of remedial representation; holistic review and the diversity rationale gave administrators a shared framework for pursuing demographic goals under judicial cover.
% TRANSFER_FUNCTION: Moved admission probability at selective institutions from high-statistical-profile applicants, disproportionately Asian-American and secondarily white, toward applicants from historically underrepresented racial groups; and moved doctrinal discretion to the administering institutions under judicial deference, at the price of accumulating litigation exposure.
% ABSENT_VOICES: Colorblind constitutionalists were structurally outside the settlement for four decades: courts declined their repeated petitions, so the unanimity of the deference era was produced partly by leaving the dissenting reading unresolved rather than refuted. Applicants on every side were conscripted into racial categories without consent, and the internal division among Asian-American communities was flattened by both camps, each claiming to speak for the group.
% DISAPPEARANCE_RATIONALE: If the arrangement vanished overnight, selective admissions would redistribute toward the statistical and contextual profiles the mechanism had been offsetting, top-tier demographics would shift, and the advocacy, litigation, and compliance infrastructure built around the arrangement would lose its object. Conversely, if the colorblind prohibition were abandoned overnight, the arrangement would revive openly. Either way the world rearranges, which is what keeps this a live constitutional contest rather than settled background fact.
% FOUNDING_PROBLEM: Articulated in Harlan's 1896 dissent against Plessy: the Constitution 'neither knows nor tolerates classes among citizens,' a rule against state-legislated racial caste. The arrangement this story assesses consolidated after 1978 to pursue the inverse inheritance: opening institutions that formal caste and its aftermath had kept closed, and securing the educational benefits of student-body diversity.
% FOUNDING_PROBLEM_CORROBORATION: Historians of Reconstruction and the civil-rights legislative record corroborate the founding problem's original form from outside any benefiting party. On its present status the record splits along the very readings in contest: subordination-effects scholarship and the remedial tradition attest continuities of the problem, while demographic and legal histories attesting the death of formal state caste support the colorblind claim that the problem in its founding form is closed. No adjudicator outside the contest exists; the disagreement over the founding problem is the kernel dispute itself.
narrative_ontology:disappearance_verdict(equal_protection_commitment__colorblind_reading, world_rearranges).
narrative_ontology:founding_problem_status(equal_protection_commitment__colorblind_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(equal_protection_commitment__colorblind_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(equal_protection_commitment__colorblind_reading, 'none', 1).
narrative_ontology:epsilon_provenance(equal_protection_commitment__colorblind_reading, 0.44, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(equal_protection_commitment__colorblind_reading_tests).
:- end_tests(equal_protection_commitment__colorblind_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.44, mid-band of the manifest's expected 0.35-0.50, because the reading holds classification itself to be the harm: every classified applicant bears it, yet the per-event magnitude is bounded, a distorted admission decision rather than total dispossession, so the value sits at moderate-high rather than extreme. Suppression is authored at 0.28 as an end-state snapshot: the enforcement apparatus that sustained the arrangement (certiorari denial, deference, professional-cost enforcement) collapsed when the Court withdrew deference in 2023, and the series records the ratchet up to 0.68 and the cliff down. Theater rises monotonically from 0.20 to 0.46 as the diversity rationale increasingly served as litigation armor diverging from operational mechanics, a gap the 2018-2019 trial discovery made public; this is Goodhart drift, the proxy goal displacing the stated function. Accessibility collapse is moderate (0.52) because alternatives outside the classified channel persisted but the elite tier closed. Resistance is high (0.64): the challenge was organized, funded, and ultimately victorious. The three series share one eight-point grid so no metric is sampled against another metric's end-state. Claim and metrics are independent: the snare claim is this reading's structural verdict (the coordination story cannot redeem a mechanism the reading holds per se illegitimate, victims are identifiable, and persistence required active enforcement plus suppression of the colorblind alternative), while the end-state metric profile, low suppression with elevated theater and diffuse residual extraction, is admittedly piton-adjacent; the proxy-era omega below is what decides whether the arrangement decays toward inertia or stabilizes as covert extraction, and the divergence between claim and computed type is left to the engine as the measurement the corpus exists to take.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently by construction. From the administering universities' position the arrangement was mission execution under lawful deference, and they experienced its termination as confiscation of a tool they had been told was constitutional; from the classified applicants' position the same machinery was a categorization imposed without consent. The polarity inverted in 2023: yesterday's agenda-setters became the governed party of the colorblind rule. Same-level dynamics differentiate actors the atoms alone would lump together: asian_american_applicants and white_applicants share the payer position but differ in effect size and coalition structure; diversity_advocacy_establishment and colorblind_constitutionalists hold equal organized power with opposed identity commitments, separated by exit type, the former identity_locked (organizations that have become their function, exit requiring re-founding their theory of justice) and the latter merely constrained (external litigation as the only available voice). The engine computes these divergences from the structural data; nothing in the authored claim adjudicates them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive low directionality: admitted recipients collect the transferred admission probability, universities collect discretion, prestige, and deference-era autonomy, and the advocacy establishment collects standing and programmatic continuity. Victim declarations drive high directionality: classified applicants bear the categorization and the effective bar, with no procedural exit from being raced. The Court sits near symmetric on fallback, having collected doctrinal authority from managing the controversy while bearing legitimacy risk in both directions. Directionality overrides are deliberately omitted: overrides key on the power atom, not the agent, so any override here would distort same-atom actors (universities and the Court both institutional; white applicants and admitted recipients both moderate). Two refinements the derivation cannot express are recorded here instead: the Asian-American seat's effective target position is softened by the community's internal split, and the universities' net position remains beneficiary-side despite late-interval litigation costs because four decades of collected discretion and prestige dominate the ledger.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem, state-legislated racial caste, was real, corroborated from outside the benefiting parties, and was substantially resolved in its original direction; the arrangement assessed here was founded afterward, on a contested reading of what remains to be solved. Founding_problem_status is contested rather than dead, paired with disappearance_verdict world_rearranges, so the mismatch consumer finds no dead-mandate zombie flag, correctly: the mandate is disputed, not lapsed. The classification prevents mislabeling in both directions. Crediting the diversity rationale as pure coordination would hide the classified applicants' burden, which is this reading's finding; reading the arrangement as extraction with no function would erase the genuine redistribution its beneficiaries received, which is the siblings' finding. The corpus carries both assessments over one shared referent, and the divergence among them is the datum, not an error to be reconciled.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_committer_structure,
    'This constraint is the colorblind_reading instantiation of the equal_protection_commitment kernel; what structural differences would the sibling readings (remedial_reading, diversity_reading) introduce over the same referent?',
    'Compare the three sibling stories'' beneficiary/victim sets, epsilon values, and computed types over the shared referent of state racial classification in selective admissions, 1978-2023.',
    'The remedial reading would place historically subordinated groups in the beneficiary set and recast the same programs as anti-caste coordination with low epsilon; the diversity reading would credit the educational-coordination function and land near a hybrid coordination type. This story''s snare verdict and 0.44 epsilon are indexical to the colorblind seat, not topic-level facts.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_committer_structure, conceptual, 'Reading-indexed classification over a shared kernel referent; committer structure routed here rather than into standard fields.').

omega_variable(
    classification_per_se_harm_status,
    'Is racial classification by the state a cognizable harm in itself, independent of how outcomes distribute?',
    'Weigh the dignity and expressive-harm literature against welfare-outcome studies; note that the SFFA record tested outcome-based defenses and left the per-se premise itself normative rather than empirically settled.',
    'If the per-se premise weakens, epsilon falls toward the diversity reading''s assessment and the snare verdict loses its foundation; if it strengthens, epsilon rises and proxy-era mechanisms inherit the full verdict.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(classification_per_se_harm_status, conceptual, 'Whether the reading''s foundational harm premise holds independently of outcome distributions.').

omega_variable(
    proxy_era_extraction_persistence,
    'Do post-2023 essay, personal-rating, and adversity-factor mechanisms constitute racial classification by proxy that sustains the arrangement, or genuine individualized review leaving only residual extraction?',
    'Admissions-data audits correlating proxy inputs with race at constant academic metrics, and litigation discovery of the kind that exposed the pre-2023 mechanics.',
    'Determines whether the 0.44 endpoint is a decay floor heading toward inertial persistence or a stable covert level; decides the snare-versus-piton drift question for the next interval.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(proxy_era_extraction_persistence, empirical, 'Whether classification survived 2023 by migrating to facially neutral proxies.').

omega_variable(
    asian_american_coalition_structure,
    'Does aggregating Asian-American applicants as a single victim seat erase the community''s internal split, and would disaggregation change the resistance and coalition picture?',
    'Disaggregated survey and litigation-participation data by ethnicity, immigration generation, and class within the category.',
    'A split seat weakens simple victim-coalition predictions and explains why organized Asian-American entities appeared on both sides in the SFFA briefs; it conditions any coalition-power analysis for the payer seats.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(asian_american_coalition_structure, empirical, 'Internal heterogeneity of the primary target seat and its coalition consequences.').

omega_variable(
    founding_problem_inversion_ambiguity,
    'Is the colorblind rule''s modern application against remedial classification fidelity to its founding purpose, that no state caste of any kind be legislated, or an inversion of it, a rule forged against white-supremacist caste now shielding inherited disadvantage?',
    'Reconstruction-era exegesis weighed against post-civil-rights doctrinal history, including how the founding generation''s own remedial statutes and Harlan''s broader record bear on the rule''s intended scope.',
    'Sets founding_problem_status between live and contested and determines whether the arrangement''s genealogy supports or undermines this reading''s verdict.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founding_problem_inversion_ambiguity, conceptual, 'Whether the rule''s changed target constitutes fidelity or inversion of its founding problem.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(equal_protection_commitment__colorblind_reading, 1978, 2023).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(epcb_tr_t1978, equal_protection_commitment__colorblind_reading, theater_ratio, 1978, 0.2).
narrative_ontology:measurement(epcb_tr_t1985, equal_protection_commitment__colorblind_reading, theater_ratio, 1985, 0.24).
narrative_ontology:measurement(epcb_tr_t1992, equal_protection_commitment__colorblind_reading, theater_ratio, 1992, 0.28).
narrative_ontology:measurement(epcb_tr_t1999, equal_protection_commitment__colorblind_reading, theater_ratio, 1999, 0.33).
narrative_ontology:measurement(epcb_tr_t2006, equal_protection_commitment__colorblind_reading, theater_ratio, 2006, 0.38).
narrative_ontology:measurement(epcb_tr_t2013, equal_protection_commitment__colorblind_reading, theater_ratio, 2013, 0.42).
narrative_ontology:measurement(epcb_tr_t2019, equal_protection_commitment__colorblind_reading, theater_ratio, 2019, 0.44).
narrative_ontology:measurement(epcb_tr_t2023, equal_protection_commitment__colorblind_reading, theater_ratio, 2023, 0.46).

% Extraction over time
narrative_ontology:measurement(epcb_be_t1978, equal_protection_commitment__colorblind_reading, base_extractiveness, 1978, 0.3).
narrative_ontology:measurement(epcb_be_t1985, equal_protection_commitment__colorblind_reading, base_extractiveness, 1985, 0.33).
narrative_ontology:measurement(epcb_be_t1992, equal_protection_commitment__colorblind_reading, base_extractiveness, 1992, 0.37).
narrative_ontology:measurement(epcb_be_t1999, equal_protection_commitment__colorblind_reading, base_extractiveness, 1999, 0.4).
narrative_ontology:measurement(epcb_be_t2006, equal_protection_commitment__colorblind_reading, base_extractiveness, 2006, 0.46).
narrative_ontology:measurement(epcb_be_t2013, equal_protection_commitment__colorblind_reading, base_extractiveness, 2013, 0.49).
narrative_ontology:measurement(epcb_be_t2019, equal_protection_commitment__colorblind_reading, base_extractiveness, 2019, 0.5).
narrative_ontology:measurement(epcb_be_t2023, equal_protection_commitment__colorblind_reading, base_extractiveness, 2023, 0.44).

% Suppression requirement over time
narrative_ontology:measurement(epcb_su_t1978, equal_protection_commitment__colorblind_reading, suppression_requirement, 1978, 0.45).
narrative_ontology:measurement(epcb_su_t1985, equal_protection_commitment__colorblind_reading, suppression_requirement, 1985, 0.48).
narrative_ontology:measurement(epcb_su_t1992, equal_protection_commitment__colorblind_reading, suppression_requirement, 1992, 0.52).
narrative_ontology:measurement(epcb_su_t1999, equal_protection_commitment__colorblind_reading, suppression_requirement, 1999, 0.56).
narrative_ontology:measurement(epcb_su_t2006, equal_protection_commitment__colorblind_reading, suppression_requirement, 2006, 0.61).
narrative_ontology:measurement(epcb_su_t2013, equal_protection_commitment__colorblind_reading, suppression_requirement, 2013, 0.65).
narrative_ontology:measurement(epcb_su_t2019, equal_protection_commitment__colorblind_reading, suppression_requirement, 2019, 0.68).
narrative_ontology:measurement(epcb_su_t2023, equal_protection_commitment__colorblind_reading, suppression_requirement, 2023, 0.28).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(equal_protection_commitment__colorblind_reading, resource_allocation).
narrative_ontology:affects_constraint(equal_protection_commitment__colorblind_reading, equal_protection_commitment__remedial_reading).
narrative_ontology:affects_constraint(equal_protection_commitment__colorblind_reading, equal_protection_commitment__diversity_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'equal protection' decomposes into three reading-stories over one kernel and one shared referent, state racial classification in selective admissions, 1978-2023. Per the OQ-26 rule the referent is shared while epsilon is reading-indexed: this story authors 0.44 with classified applicants as victims and a snare verdict; the remedial reading authors low epsilon over the same referent, treating the programs as anti-caste coordination; the diversity reading authors intermediate epsilon with a credited coordination function. Family members link via affects_constraints. Upstream/downstream here is doctrinal rather than causal: each reading cites the same clause text and the same case line (Bakke, Grutter, Fisher, SFFA) as warrant, so the edges record kinship and contamination exposure, not one reading generating another.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
