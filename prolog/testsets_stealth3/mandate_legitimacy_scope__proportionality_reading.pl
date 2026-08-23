% ============================================================================
% CONSTRAINT STORY: mandate_legitimacy_scope__proportionality_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_mandate_legitimacy_scope__proportionality_reading, []).

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
 *   constraint_id: mandate_legitimacy_scope__proportionality_reading
 *   human_readable: Proportionality Reading of Vaccination Mandate Legitimacy
 *   domain: public_health_ethics/constitutional_law/medical_autonomy
 *
 * SUMMARY:
 *   This story instantiates the proportionality reading of the
 *   mandate_legitimacy_scope kernel. The standing arrangement under contest
 *   is compulsory-vaccination practice as it actually operates — school-entry
 *   laws, healthcare employment requirements, military and federal workforce
 *   mandates — assessed by this reading's own lights. On those lights the
 *   arrangement splits cleanly: mandates backed by severe disease,
 *   high-efficacy safe vaccines, and no workable less restrictive alternative
 *   (measles being the paradigm) are legitimate coordination; mandates riding
 *   on weak parameters (annual influenza with modest efficacy, boosters with
 *   waning benefit, rules blind to documented prior infection) are coercion
 *   without adequate justification. The reading therefore authors a moderate
 *   epsilon over the whole arrangement and a victim set concentrated on the
 *   weak-parameter periphery. Sibling readings (public_health_primary,
 *   bodily_autonomy_primary) are separate constraints with their own epsilon
 *   and victim structures, linked through network.affects_constraints;
 *   nothing about them is averaged into this file. Claim/metric independence
 *   holds: tangled_rope is claimed from the structure (a genuine coordination
 *   function joined to asymmetric extraction, actively enforced), while the
 *   metrics are authored as descriptive of observed operation. KEY AGENTS (by
 *   structural relationship): - constitutional_review_courts: Agenda setter
 *   (institutional/analytical) — administers the proportionality standard and
 *   sets its stringency - public_health_authorities: Primary beneficiary with
 *   agenda-setting duties (institutional/constrained) — supplies the
 *   parameters, collects legitimated mandate authority -
 *   immunocompromised_individuals: Protected beneficiary (organized/trapped)
 *   — relies on coverage produced by endorsed mandates -
 *   pre_vaccination_age_infants: Protected beneficiary (powerless/trapped) —
 *   shielded only by surrounding coverage -
 *   disproportionately_mandated_workers: Primary target
 *   (organized/constrained) — bears weak-parameter coercion -
 *   vaccine_adverse_event_bearers: Secondary target (powerless/trapped) —
 *   bear residual injury priced as acceptable aggregate risk -
 *   conscientious_objector_parents: Excluded voice
 *   (organized/identity_locked) — categorical objection with no seat in the
 *   parameter frame - bioethics_scholars: Analytical observer
 *   (analytical/analytical) — documents rubber-stamp review and articulates
 *   the objections courts compress Assumption stated: interval time points
 *   map approximately to calendar years t0=1990 through t30=2020, chosen
 *   because the modern mandate-expansion and retrenchment record falls in
 *   that window.
 *
 * KEY AGENTS:
 *   - constitutional_review_courts: agenda setter — applies, refines, or declines the proportionality standard; sets its stringency for everyone else
 *   - public_health_authorities: primary beneficiary with agenda-setting duties — designs mandates, supplies severity/safety/coverage data, collects legitimated coercive authority
 *   - immunocompromised_individuals: protected beneficiary — depends on community immunity from mandates that survive the test
 *   - pre_vaccination_age_infants: protected beneficiary — shielded only by surrounding coverage during the highest-risk window
 *   - disproportionately_mandated_workers: primary target — bears mandates whose parameter case is weak; union voice without exit
 *   - vaccine_adverse_event_bearers: secondary target — injury absorbed as residual risk within an accepted aggregate trade-off
 *   - conscientious_objector_parents: excluded voice — categorical objection the parameter frame has no seat for
 *   - bioethics_scholars: analytical observer — audits review quality from outside the compliance structure
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(mandate_legitimacy_scope__proportionality_reading, 0.5).
domain_priors:suppression_score(mandate_legitimacy_scope__proportionality_reading, 0.52).
domain_priors:theater_ratio(mandate_legitimacy_scope__proportionality_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(mandate_legitimacy_scope__proportionality_reading, extractiveness, 0.5).
narrative_ontology:constraint_metric(mandate_legitimacy_scope__proportionality_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(mandate_legitimacy_scope__proportionality_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(mandate_legitimacy_scope__proportionality_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(mandate_legitimacy_scope__proportionality_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(mandate_legitimacy_scope__proportionality_reading, tangled_rope).
narrative_ontology:human_readable(mandate_legitimacy_scope__proportionality_reading, "Proportionality Reading of Vaccination Mandate Legitimacy").
narrative_ontology:topic_domain(mandate_legitimacy_scope__proportionality_reading, "public_health_ethics/constitutional_law/medical_autonomy").

domain_priors:requires_active_enforcement(mandate_legitimacy_scope__proportionality_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(mandate_legitimacy_scope__proportionality_reading, '8487e8cb-dd3f-4dc4-b4c1-cc8291e06628').
narrative_ontology:cs_kernel_codification('8487e8cb-dd3f-4dc4-b4c1-cc8291e06628', distributed).
narrative_ontology:cs_authority_grounding('8487e8cb-dd3f-4dc4-b4c1-cc8291e06628', lineage).
narrative_ontology:cs_interpretation_layer_present('8487e8cb-dd3f-4dc4-b4c1-cc8291e06628').
narrative_ontology:cs_reading_relation('8487e8cb-dd3f-4dc4-b4c1-cc8291e06628', mandate_legitimacy_scope__public_health_primary, influences).
narrative_ontology:cs_reading_relation('8487e8cb-dd3f-4dc4-b4c1-cc8291e06628', mandate_legitimacy_scope__bodily_autonomy_primary, coexists_with).
narrative_ontology:cs_axiom('8487e8cb-dd3f-4dc4-b4c1-cc8291e06628', foundational, coercion_legitimacy_is_parameter_indexed).
narrative_ontology:cs_axiom_status(coercion_legitimacy_is_parameter_indexed, holdable).
narrative_ontology:cs_axiom_grounding('8487e8cb-dd3f-4dc4-b4c1-cc8291e06628', coercion_legitimacy_is_parameter_indexed, instrumental).
narrative_ontology:cs_axiom('8487e8cb-dd3f-4dc4-b4c1-cc8291e06628', secondary, least_restrictive_means_before_compulsion).
narrative_ontology:cs_axiom_status(least_restrictive_means_before_compulsion, holdable).
narrative_ontology:cs_axiom_grounding('8487e8cb-dd3f-4dc4-b4c1-cc8291e06628', least_restrictive_means_before_compulsion, instrumental).
narrative_ontology:cs_reference_frame('8487e8cb-dd3f-4dc4-b4c1-cc8291e06628', parameter_conditioned_coercion_authority).
narrative_ontology:cs_drift_state('8487e8cb-dd3f-4dc4-b4c1-cc8291e06628', post_covid_mandate_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('8487e8cb-dd3f-4dc4-b4c1-cc8291e06628', '').
narrative_ontology:cs_kernel_id(mandate_legitimacy_scope__proportionality_reading, mandate_legitimacy_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(mandate_legitimacy_scope__proportionality_reading, immunocompromised_individuals).
narrative_ontology:constraint_beneficiary(mandate_legitimacy_scope__proportionality_reading, pre_vaccination_age_infants).
narrative_ontology:constraint_beneficiary(mandate_legitimacy_scope__proportionality_reading, public_health_authorities).
narrative_ontology:constraint_victim(mandate_legitimacy_scope__proportionality_reading, disproportionately_mandated_workers).
narrative_ontology:constraint_victim(mandate_legitimacy_scope__proportionality_reading, vaccine_adverse_event_bearers).
narrative_ontology:constraint_vindicates(mandate_legitimacy_scope__proportionality_reading, proportionality_review_doctrine).
narrative_ontology:constraint_vindicates(mandate_legitimacy_scope__proportionality_reading, least_restrictive_means_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Apply, refine, or decline the proportionality standard when mandate disputes reach them: weighing disease-severity evidence, vaccine risk-benefit profiles, and the availability of less restrictive means, their rulings set how demanding the test is for every other actor. They neither vaccinate nor refuse; their stake is doctrinal — the coherence of the framework they administer and the precedent line they extend or cut back.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__proportionality_reading, constitutional_review_courts, agenda_setter,
    institutional, generational, analytical, national).

% Design and administer vaccination mandates and supply the severity, safety, and coverage data the proportionality assessment consumes. When mandates pass the test they gain a legitimated coercive instrument and higher coverage; when review is loose they gain it cheaply. Exit is constrained: having built programs, staffing models, and political capital around mandate authority, abandoning the tool carries programmatic and career cost.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__proportionality_reading, public_health_authorities, beneficiary,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(mandate_legitimacy_scope__proportionality_reading, public_health_authorities, agenda_setter).

% Rely on community immunity produced by mandates that survive the proportionality test, because they cannot be safely vaccinated or respond poorly to vaccines. They bear exposure risk wherever coverage falls and cannot exit the risk environment individually; patient-advocacy networks give them collective voice, but their protection depends entirely on other people's compliance.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__proportionality_reading, immunocompromised_individuals, beneficiary,
    organized, biographical, trapped, national).

% Too young for measles vaccination during the highest-risk window, they are protected only by the coverage surrounding them. They cannot act, speak, or relocate on their own behalf; their interest enters the assessment through caregivers, pediatricians, and epidemiological argument.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__proportionality_reading, pre_vaccination_age_infants, beneficiary,
    powerless, immediate, trapped, local).

% Healthcare, military, and other workers subject to mandates where the parameter case is weak — annual influenza shots of modest efficacy, boosters with waning benefit, requirements that ignore documented prior infection. Compliance costs them time, side-effect risk, and sometimes employment; refusal costs them their position. Unions give them voice but not exit: licensure, seniority, and pensions are tied to the very institutions doing the mandating.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__proportionality_reading, disproportionately_mandated_workers, payer,
    organized, biographical, constrained, national).

% People who suffer recognized adverse events after mandated doses. The proportionality framework prices their injury as residual risk within an acceptable aggregate trade-off; compensation runs through narrow administrative channels with damage caps and long timelines. They cannot return to their pre-injury position, and their individual claim competes publicly against the population-level success story that justified the mandate.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__proportionality_reading, vaccine_adverse_event_bearers, payer,
    powerless, biographical, trapped, national).

% Parents whose objection is categorical — that compelled injection is wrong regardless of disease parameters. The proportionality conversation has no seat for them: it presupposes coercion can be justified and argues only about when. They organize for exemptions, litigate on religious-liberty grounds, and absorb social sanction; leaving their position would mean abandoning commitments that constitute their community identity.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__proportionality_reading, conscientious_objector_parents, excluded,
    organized, generational, identity_locked, national).

% Analyze the framework from outside: publishing on proportionality's stringency, documenting where review is rubber-stamp, comparing jurisdictions, and articulating the autonomy-side objections the courts compress into footnotes. They carry no compliance obligations; their influence runs through amicus briefs, advisory committees, and the training of future reviewers.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__proportionality_reading, bioethics_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(mandate_legitimacy_scope__proportionality_reading, public_health_authorities).
narrative_ontology:fixing_cost_class(mandate_legitimacy_scope__proportionality_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Converts an open-ended contest over state medical coercion into a parameterized decision procedure: disease severity, vaccine risk-benefit profile, and the exhaustion of less restrictive means are assessed before compulsion is deployed, giving legislatures, agencies, courts, and objectors a shared evidentiary vocabulary for a fight that would otherwise be pure power.
% TRANSFER_FUNCTION: Moves coercive burden — injection obligation, exclusion from school or employment on refusal — onto individuals when the parameter case supports it, and moves protection from severe disease toward those who cannot be vaccinated; in the reverse direction it moves decisional limits onto the state, forbidding compulsion where the parameters fail.
% ABSENT_VOICES: Categorical autonomy objectors sit outside the frame: the test presupposes compulsion can be justified and argues only about when, so the prior question — whether the state may compel at all — is never heard inside it. Adults who cannot vaccinate for medical reasons are represented by proxies rather than seated. Workers bearing weak-parameter mandates had no seat when the parameters were set; they arrive only as litigants after the fact.
% DISAPPEARANCE_RATIONALE: Without the proportionality test, every mandate dispute collapses into a binary between categorical public-health authority and categorical bodily sovereignty: courts lose the mediating doctrine, agencies lose the evidentiary discipline the test imposes, and weak-parameter mandates either proliferate unchecked or all mandates fall together. School-entry systems, healthcare employment rules, and exemption litigation would all reorganize around whichever absolute prevailed.
% FOUNDING_PROBLEM: Smallpox-era police power confronted bodily liberty: Cambridge, Massachusetts compelled vaccination in 1902, Henning Jacobson refused, and the resulting litigation forced the question of when collective disease defense legitimately overrides individual refusal. The proportionality answer was built to distinguish legitimate collective defense from arbitrary coercion by indexing legitimacy to measurable parameters rather than deciding the matter categorically.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: a century of judicial opinions applying and refining the standard — including contemporary rulings vacating weak-parameter mandates — constitutional-law scholarship across ideological camps treating the parameter question as unsettled and recurring, and bioethics literature independently deriving least-restrictive-means requirements. Public health agencies also attest liveness, but self-interestedly; the external judicial and scholarly attestations stand on their own.
narrative_ontology:disappearance_verdict(mandate_legitimacy_scope__proportionality_reading, world_rearranges).
narrative_ontology:founding_problem_status(mandate_legitimacy_scope__proportionality_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(mandate_legitimacy_scope__proportionality_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(mandate_legitimacy_scope__proportionality_reading, 'none', 1).
narrative_ontology:epsilon_provenance(mandate_legitimacy_scope__proportionality_reading, 0.5, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(mandate_legitimacy_scope__proportionality_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(mandate_legitimacy_scope__proportionality_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(mandate_legitimacy_scope__proportionality_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is moderate (0.50 at interval end) because the standing arrangement blends an endorsed core — measles-style mandates the reading judges legitimate, where coercion buys genuine protection for the powerless — with a condemned periphery of weak-parameter mandates the reading judges extractive. Suppression (0.52) is predominantly structural: school exclusion, employment termination, licensure consequences, with a smaller internalized component (social sanction pressing compliance); it is authored raw and unscaled, per the engine's design — only extractiveness is scaled by directionality and scope. Theater (0.40) reflects a review process that performs genuine parameter analysis in outbreak-driven, high-salience cases while rubber-stamping agency assertions in routine ones. Accessibility collapse (0.45): alternatives — exemption processes, testing regimes, remote arrangements, private schooling — persist but at real cost, so options narrow without vanishing. Resistance (0.60): sustained litigation, exemption movements, and legislative counter-moves meet the arrangement continuously. All three tracked series share one six-point grid; the mid-interval acceleration corresponds to expansion of mandates into weak-parameter contexts (healthcare-worker influenza rules, then pandemic-era boosters), and the terminal dip to post-pandemic retrenchment (federal mandate vacatur, military rescission, university rollbacks). Base properties report the end-state.
 *
 * PERSPECTIVAL GAP:
 *   From the payer seats the arrangement computes as enforced extraction: workers face termination over mandates whose parameter case they can document as weak, and injured bearers find their harm priced as someone else's acceptable aggregate. From the protected beneficiary seats the same arrangement computes as justified coordination that keeps classmates and nurses immune around people who cannot be vaccinated. From the agenda-setter seat it is doctrine to be administered coherently. The engine derives these divergent per-seat classifications from the declared directionalities and exits; this commentary does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Immunocompromised individuals and infants are declared beneficiaries with trapped exit: the constraint subsidizes them (shielding they cannot purchase individually), placing their d near the beneficiary end — notably, trapped exit does not push them toward target because the protective flow runs toward them. Disproportionately mandated workers are declared victims with constrained exit (licensure and seniority tied to the mandating institutions), pushing their effective extraction toward the full-target end. Adverse-event bearers are victims with trapped exit — injury already realized, recourse capped and slow. Public health authorities are beneficiaries but also run the machinery; their d sits low but not minimal, since mandate backfire costs them politically. Courts are analytical. Spatial scope is national: verification of parameter claims is institutionally feasible but politically contestable, supporting moderate amplification rather than extreme.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem is live, so no mandatrophy is declared: each new pathogen and vaccine generation reopens the parameter assessment, and the R5 mismatch consumer finds status=live paired with verdict=world_rearranges — no zombie flag. The tangled_rope classification is what prevents mislabeling in both directions: reading the whole arrangement as a snare erases the endorsed core that genuinely protects the powerless (infants, the immunocompromised), while reading it as pure rope erases the condemned periphery where coercion demonstrably outruns its justification. The hybrid type holds both facts in one structure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'This constraint is one reading of the mandate_legitimacy_scope kernel — would instantiating a sibling reading change the structural classification?',
    'Author the sibling files: public_health_primary would collapse the victim set toward empty (compulsion presumptively legitimate when collective protection is at stake, epsilon low) and bodily_autonomy_primary would expand it to every coerced person regardless of parameters (epsilon high); the disagreement is located in categorical versus parameter-indexed legitimacy.',
    'If public_health_primary controlled, this reading''s victims dissolve into justified costs; if bodily_autonomy_primary controlled, the endorsed core becomes extraction too — the moderate blended epsilon and conditional victim set are artifacts of this reading, not of the arrangement alone.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Committer structure: one reading of a three-reading kernel; siblings would restructure victim set and epsilon.').

omega_variable(
    severity_threshold_indeterminacy,
    'Who fixes the numerical thresholds (what disease severity counts as serious, what vaccine efficacy counts as sufficient) and with what acknowledged error bars?',
    'Comparative review of agency parameter-setting practice and judicial scrutiny of threshold choices; epidemiological sensitivity analysis showing how mandate verdicts flip within plausible confidence intervals.',
    'Loose thresholds shrink the condemned periphery and lower measured epsilon; strict, error-bar-honest thresholds enlarge it — the moderate epsilon is hostage to threshold discipline.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(severity_threshold_indeterminacy, empirical, 'Threshold-setting discretion drives where the legitimate/illegitimate line falls.').

omega_variable(
    natural_immunity_accounting,
    'Does documented prior infection count toward the vaccine-efficacy parameter, or must mandates ignore recovery status?',
    'Immunological evidence on durability of infection-derived versus vaccine-derived protection, and review of mandate texts that disregard documented recovery.',
    'Counting natural immunity converts a large worker cohort from mandated to exempt, cutting the victim set and lowering epsilon for the employment-mandate segment; ignoring it inflates apparent necessity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_immunity_accounting, empirical, 'Whether recovered individuals are wrongly swept into the mandated class.').

omega_variable(
    pathogen_conditional_decomposition_pressure,
    'The expected structural delta says the victim set and epsilon vary by pathogen (measles mandate legitimate, influenza mandate not) — can one story honestly carry a single blended epsilon?',
    'If per-pathogen assessment shows non-overlapping verdicts, decompose into per-pathogen constraint stories (a low-epsilon measles-mandate story and a high-epsilon influenza-mandate story) linked by network.affects_constraints, per the epsilon-invariance principle.',
    'The current blended epsilon (0.50) is the honest single-file compromise; decomposition would replace it with two stable epsilon values and sharpen both victim sets.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(pathogen_conditional_decomposition_pressure, conceptual, 'Decomposition pressure from pathogen-conditioned verdicts within one reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(mandate_legitimacy_scope__proportionality_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mand_tr_t0, mandate_legitimacy_scope__proportionality_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement(mand_tr_t6, mandate_legitimacy_scope__proportionality_reading, theater_ratio, 6, 0.26).
narrative_ontology:measurement(mand_tr_t12, mandate_legitimacy_scope__proportionality_reading, theater_ratio, 12, 0.3).
narrative_ontology:measurement(mand_tr_t18, mandate_legitimacy_scope__proportionality_reading, theater_ratio, 18, 0.44).
narrative_ontology:measurement(mand_tr_t24, mandate_legitimacy_scope__proportionality_reading, theater_ratio, 24, 0.52).
narrative_ontology:measurement(mand_tr_t30, mandate_legitimacy_scope__proportionality_reading, theater_ratio, 30, 0.4).

% Extraction over time
narrative_ontology:measurement(mand_be_t0, mandate_legitimacy_scope__proportionality_reading, base_extractiveness, 0, 0.36).
narrative_ontology:measurement(mand_be_t6, mandate_legitimacy_scope__proportionality_reading, base_extractiveness, 6, 0.4).
narrative_ontology:measurement(mand_be_t12, mandate_legitimacy_scope__proportionality_reading, base_extractiveness, 12, 0.44).
narrative_ontology:measurement(mand_be_t18, mandate_legitimacy_scope__proportionality_reading, base_extractiveness, 18, 0.58).
narrative_ontology:measurement(mand_be_t24, mandate_legitimacy_scope__proportionality_reading, base_extractiveness, 24, 0.64).
narrative_ontology:measurement(mand_be_t30, mandate_legitimacy_scope__proportionality_reading, base_extractiveness, 30, 0.5).

% Suppression requirement over time
narrative_ontology:measurement(mand_su_t0, mandate_legitimacy_scope__proportionality_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(mand_su_t6, mandate_legitimacy_scope__proportionality_reading, suppression_requirement, 6, 0.44).
narrative_ontology:measurement(mand_su_t12, mandate_legitimacy_scope__proportionality_reading, suppression_requirement, 12, 0.48).
narrative_ontology:measurement(mand_su_t18, mandate_legitimacy_scope__proportionality_reading, suppression_requirement, 18, 0.62).
narrative_ontology:measurement(mand_su_t24, mandate_legitimacy_scope__proportionality_reading, suppression_requirement, 24, 0.7).
narrative_ontology:measurement(mand_su_t30, mandate_legitimacy_scope__proportionality_reading, suppression_requirement, 30, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(mandate_legitimacy_scope__proportionality_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(mandate_legitimacy_scope__proportionality_reading, mandate_legitimacy_scope__public_health_primary).
narrative_ontology:affects_constraint(mandate_legitimacy_scope__proportionality_reading, mandate_legitimacy_scope__bodily_autonomy_primary).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition per the epsilon-invariance principle: the colloquial label 'vaccine mandate legitimacy' covers three structurally distinct constraints — categorical public-health authorization (public_health_primary), parameter-indexed authorization (this file), and categorical autonomy veto (bodily_autonomy_primary). Each carries its own epsilon, victim set, and classification; none averages over the others. Structural flow: this reading sits between the siblings — it imposes parameter-proof requirements on public_health_primary's legitimacy conditions (influences) while coexisting with bodily_autonomy_primary as rival live positions held by different factions. A fourth decomposition axis (per-pathogen stories) is documented in omega pathogen_conditional_decomposition_pressure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
