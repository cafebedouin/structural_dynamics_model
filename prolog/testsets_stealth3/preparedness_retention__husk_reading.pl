% ============================================================================
% CONSTRAINT STORY: preparedness_retention__husk_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_preparedness_retention__husk_reading, []).

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
 *   constraint_id: preparedness_retention__husk_reading
 *   human_readable: Disaster-Exercise Regime as Memorial Performance (Husk Reading)
 *   domain: governance/institutional_memory/disaster_preparedness
 *
 * SUMMARY:
 *   A statutory exercise-and-inspection cycle obliges every safety region,
 *   water board, and municipality to stage announced, scripted disaster
 *   exercises and submit them to rubric-based inspection. This story authors
 *   that standing arrangement as the husk_reading of the
 *   preparedness_retention kernel sees it: the cycle's visible product is
 *   memorial performance - anniversaries commemorated, scenarios rehearsed to
 *   evaluator checkpoints, completion statistics filed - while the tacit,
 *   physically embedded competence a real dike-breach night requires is left
 *   to atrophy. Resource allocation inside the cycle favors what inspects
 *   well over what retains skill; the yield accrues to the administrative
 *   tier as legitimacy, budget justification, and careers, and the bill is
 *   presented to frontline responders' hours and to the population behind the
 *   dikes when a design-scale flood finally arrives. The claimed type and the
 *   metrics are authored independently: the snare claim states this reading's
 *   structural verdict; the metric values state what the arrangement's
 *   operation looks like from the same seat. The engine computes per-seat
 *   classifications from the structural data; sibling readings of the same
 *   kernel are separate constraint files, not positions argued inside this
 *   one. KEY AGENTS (by structural relationship): -
 *   national_infrastructure_ministry: Agenda setter (institutional/arbitrage)
 *   - writes the exercise obligations, collects the legitimacy and
 *   budget-justification yield - regional_safety_directors: Primary
 *   beneficiary (organized/identity_locked) - stages the ceremonies, converts
 *   completion into career and budget - inspection_and_audit_bodies:
 *   Beneficiary and secondary agenda-setter (institutional/arbitrage) - owns
 *   the rubrics that define compliance - elected_executives: Beneficiary
 *   (powerful/immediate/arbitrage) - harvests the visible credit -
 *   frontline_responders: Primary target (organized/constrained) - supplies
 *   the hours; carries the skill decay - flood_plain_residents: Ultimate
 *   target (powerless/trapped) - funds the cycle, inherits its deficit at
 *   design-flood scale - volunteer_dike_watch: Target
 *   (powerless/trapped/local) - aging custodians displaced by documentation -
 *   unscripted_training_advocates: Excluded voice (moderate/constrained) -
 *   holds the alternative the rubrics foreclose -
 *   disaster_research_community: Analytical observer (moderate/analytical) -
 *   measures the gap the framework does not
 *
 * KEY AGENTS:
 *   - national_infrastructure_ministry: agenda setter (institutional/arbitrage) - sets statutory exercise obligations, receives the cycle's yield as demonstrable discharge of its preparedness duty
 *   - regional_safety_directors: primary beneficiary (organized/identity_locked) - professional standing constituted through the exercise calendar they administer
 *   - inspection_and_audit_bodies: beneficiary with secondary agenda-setting role (institutional/arbitrage) - mandate and publication output scale with inspectable activity
 *   - elected_executives: beneficiary (powerful/immediate/arbitrage) - converts exercise imagery into electoral credit on short horizons
 *   - frontline_responders: primary target (organized/constrained) - hours consumed by scripted rehearsal; improvisational fluency left to atrophy
 *   - flood_plain_residents: ultimate target (powerless/trapped) - bear the deferred capacity cost at design-flood scale; hold no seat in scenario design
 *   - volunteer_dike_watch: target (powerless/trapped/local) - field knowledge chain thinning as documentation displaces rounds
 *   - unscripted_training_advocates: excluded voice (moderate/constrained) - proposes no-notice, failure-tolerant formats that score poorly on compliance instruments
 *   - disaster_research_community: analytical observer (moderate/analytical) - publishes the realism and decay evidence
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(preparedness_retention__husk_reading, 0.66).
domain_priors:suppression_score(preparedness_retention__husk_reading, 0.58).
domain_priors:theater_ratio(preparedness_retention__husk_reading, 0.72).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(preparedness_retention__husk_reading, extractiveness, 0.66).
narrative_ontology:constraint_metric(preparedness_retention__husk_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(preparedness_retention__husk_reading, theater_ratio, 0.72).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(preparedness_retention__husk_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(preparedness_retention__husk_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(preparedness_retention__husk_reading, snare).
narrative_ontology:human_readable(preparedness_retention__husk_reading, "Disaster-Exercise Regime as Memorial Performance (Husk Reading)").
narrative_ontology:topic_domain(preparedness_retention__husk_reading, "governance/institutional_memory/disaster_preparedness").

domain_priors:requires_active_enforcement(preparedness_retention__husk_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(preparedness_retention__husk_reading, 'eb661674-ed6c-4364-bcb1-86f95aaba5f0').
narrative_ontology:cs_kernel_codification('eb661674-ed6c-4364-bcb1-86f95aaba5f0', formalized).
narrative_ontology:cs_authority_grounding('eb661674-ed6c-4364-bcb1-86f95aaba5f0', extraction).
narrative_ontology:cs_interpretation_layer_present('eb661674-ed6c-4364-bcb1-86f95aaba5f0').
narrative_ontology:cs_reading_relation('eb661674-ed6c-4364-bcb1-86f95aaba5f0', preparedness_retention__competence_reading, forecloses).
narrative_ontology:cs_reading_relation('eb661674-ed6c-4364-bcb1-86f95aaba5f0', preparedness_retention__hybrid_reading, influences).
narrative_ontology:cs_axiom('eb661674-ed6c-4364-bcb1-86f95aaba5f0', foundational, ceremonial_compliance_displaces_live_skill).
narrative_ontology:cs_axiom_status(ceremonial_compliance_displaces_live_skill, holdable).
narrative_ontology:cs_axiom_grounding('eb661674-ed6c-4364-bcb1-86f95aaba5f0', ceremonial_compliance_displaces_live_skill, empirically_contingent).
narrative_ontology:cs_axiom('eb661674-ed6c-4364-bcb1-86f95aaba5f0', foundational, retention_requires_unscripted_realistic_exercise).
narrative_ontology:cs_axiom_status(retention_requires_unscripted_realistic_exercise, holdable).
narrative_ontology:cs_axiom_grounding('eb661674-ed6c-4364-bcb1-86f95aaba5f0', retention_requires_unscripted_realistic_exercise, empirically_contingent).
narrative_ontology:cs_axiom('eb661674-ed6c-4364-bcb1-86f95aaba5f0', secondary, inspection_visibility_is_operative_good).
narrative_ontology:cs_axiom_status(inspection_visibility_is_operative_good, holdable).
narrative_ontology:cs_axiom_grounding('eb661674-ed6c-4364-bcb1-86f95aaba5f0', inspection_visibility_is_operative_good, conventional).
narrative_ontology:cs_reference_frame('eb661674-ed6c-4364-bcb1-86f95aaba5f0', exercise_as_retention_mandate).
narrative_ontology:cs_drift_state('eb661674-ed6c-4364-bcb1-86f95aaba5f0', contemporary_inspection_regime, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('eb661674-ed6c-4364-bcb1-86f95aaba5f0', '').
narrative_ontology:cs_kernel_id(preparedness_retention__husk_reading, preparedness_retention).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(preparedness_retention__husk_reading, national_infrastructure_ministry).
narrative_ontology:constraint_beneficiary(preparedness_retention__husk_reading, regional_safety_directors).
narrative_ontology:constraint_beneficiary(preparedness_retention__husk_reading, inspection_and_audit_bodies).
narrative_ontology:constraint_beneficiary(preparedness_retention__husk_reading, elected_executives).
narrative_ontology:constraint_victim(preparedness_retention__husk_reading, frontline_responders).
narrative_ontology:constraint_victim(preparedness_retention__husk_reading, flood_plain_residents).
narrative_ontology:constraint_victim(preparedness_retention__husk_reading, volunteer_dike_watch).
narrative_ontology:constraint_vindicates(preparedness_retention__husk_reading, compliance_visibility_equivalence).
narrative_ontology:constraint_vindicates(preparedness_retention__husk_reading, commemoration_as_preparedness).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Carries the statutory duty to keep flood and disaster response capacity demonstrably maintained across the country. It writes the exercise-frequency obligations, approves the inspection framework, and answers the legislature with completion statistics. The cycle returns to it as budget justification, international benchmarking standing, and the ability to declare the preparedness file in order; its position is effectively unconstrained because it defines what counts as compliance.
narrative_ontology:constraint_stakeholder(preparedness_retention__husk_reading, national_infrastructure_ministry, agenda_setter,
    institutional, generational, arbitrage, national).

% Plan and stage the exercises their region owes the framework: scenario scripts, evaluator checklists, after-action reports. Their advancement and their region's budget settlements track completed, well-documented exercises; a director who spent the same money on unannounced field weeks would generate findings instead of credit. Their professional standing is built inside this calendar - peers, conferences, and promotion paths all run through it.
narrative_ontology:constraint_stakeholder(preparedness_retention__husk_reading, regional_safety_directors, beneficiary,
    organized, biographical, identity_locked, regional).

% Operate the scoring rubrics that decide whether an exercise counts. Their mandate size, staffing, and publication output scale with the volume of inspectable activity; every new obligation creates findings to publish. They also shape the next cycle by writing the rubrics the following exercises are staged against.
narrative_ontology:constraint_stakeholder(preparedness_retention__husk_reading, inspection_and_audit_bodies, beneficiary,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(preparedness_retention__husk_reading, inspection_and_audit_bodies, agenda_setter).

% Mayors, water-board chairs, and provincial deputies open the large visible exercises, stand beside responders in front of cameras, and cite completion rates in budget debates. The credit is immediate and personal; the competence question arrives, if ever, on someone else's term of office.
narrative_ontology:constraint_stakeholder(preparedness_retention__husk_reading, elected_executives, beneficiary,
    powerful, immediate, arbitrage, regional).

% Firefighters, crisis-team members, and dike crews give the evenings and days the calendar demands. Most exercises they attend are announced, scripted around evaluator checkpoints, and repeated on a known rhythm; the improvised, physically demanding work that built older cohorts' fluency gets the leftover time. They comply because refusal is a disciplinary matter and because the exercises are what their employers count.
narrative_ontology:constraint_stakeholder(preparedness_retention__husk_reading, frontline_responders, payer,
    organized, biographical, constrained, regional).

% Live behind the dikes the framework exists to protect. They fund the cycle through taxes and water-board charges, see its public face as press-friendly exercises and anniversary commemorations, and have no seat in scenario design. What reaches them from the arrangement is reassurance; what reaches them in a real surge is whatever response capacity actually survived the intervening years. Moving away from the risk is costly enough that most stay.
narrative_ontology:constraint_stakeholder(preparedness_retention__husk_reading, flood_plain_residents, payer,
    powerless, generational, trapped, regional).

% Aging local volunteers who once walked the dikes on a fixed rhythm and knew every culvert and weak spot by name. Their rounds increasingly compete with the documentation the framework requires of them, and recruitment no longer replaces attrition; the knowledge chain that ran from old hands to new ones is thinning. Their standing now comes from certificates and ceremonial thanks rather than from the field authority experience used to confer.
narrative_ontology:constraint_stakeholder(preparedness_retention__husk_reading, volunteer_dike_watch, payer,
    powerless, generational, trapped, local).

% Veteran responders, trainers, and a minority of officials who argue for no-notice, failure-tolerant field exercises and competency-based assessment. They circulate proposals and case studies, but they hold no seat in the rubric committees, and their formats score poorly on the compliance instruments that determine budgets.
narrative_ontology:constraint_stakeholder(preparedness_retention__husk_reading, unscripted_training_advocates, excluded,
    moderate, biographical, constrained, national).

% Researchers in disaster sociology and crisis-management studies who compare exercise investment against response outcomes across regions and decades. They publish the decay curves and realism critiques, advise sporadically, and hold no enforcement or budget power.
narrative_ontology:constraint_stakeholder(preparedness_retention__husk_reading, disaster_research_community, observer,
    moderate, biographical, analytical, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(preparedness_retention__husk_reading, national_infrastructure_ministry).
narrative_ontology:fixing_cost_class(preparedness_retention__husk_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains a common multi-agency exercise calendar, a shared incident-command vocabulary, and inter-organizational contact structures across regions and disciplines, and gives oversight bodies a uniform instrument for comparing preparedness across jurisdictions.
% TRANSFER_FUNCTION: Moves responder hours, agency budget, and organizational attention from unscripted field training toward documented, inspectable exercise products; moves public reassurance and electoral credit toward the administrative and executive tiers that stage and certify the exercises.
% ABSENT_VOICES: Frontline responders' realism objections reach decision tables only as anonymized survey aggregates; residents behind the dikes and the aging volunteer corps have no seat in scenario design; unscripted-training advocates sit outside the rubric committees whose instruments define what counts as preparation. The unanimity of the framework's self-assessments is produced in rooms these seats never entered.
% DISAPPEARANCE_RATIONALE: Exercise calendars, compliance reporting lines, audit publication cycles, and the budget justifications built on them would collapse immediately; the thin inter-agency contact networks the cycle maintains would decay within a few years; the administrative tier would lose its primary preparedness credential and elected executives their preparedness imagery. Response capacity itself would not improve overnight - this reading's claim is that little of it flows from the ceremony - but every arrangement organized around the ceremony would rearrange.
% FOUNDING_PROBLEM: After catastrophic storm-surge floods and their commission inquiries, the state needed assurance that dispersed dike crews, municipal services, and newly formed crisis organs could act together under flood conditions across generations of staff turnover; scheduled joint exercise was instituted as the retention instrument.
% FOUNDING_PROBLEM_CORROBORATION: The historical flood-inquiry record and the delta-commission lineage attest the founding problem from outside today's benefiting parties; contemporary courts of audit and disaster-sociology research - none of them exercise-budget recipients - corroborate that turnover-driven skill decay remains real while documenting that the regime's measured outputs track compliance visibility rather than response performance. No benefiting party's attestation is relied on.
narrative_ontology:disappearance_verdict(preparedness_retention__husk_reading, world_rearranges).
narrative_ontology:founding_problem_status(preparedness_retention__husk_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(preparedness_retention__husk_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(preparedness_retention__husk_reading, 'none', 1).
narrative_ontology:epsilon_provenance(preparedness_retention__husk_reading, 0.66, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(preparedness_retention__husk_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(preparedness_retention__husk_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(preparedness_retention__husk_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.66 for the standing arrangement as this reading assesses it: the cycle consumes responder hours, agency budget, and organizational attention at a scale comparable to real training, and returns compliance artifacts rather than retrievable skill. Suppression at 0.58 is a raw structural value, unscaled by the engine: roughly seventy percent of it is structural (statutory obligation, rubric scoring, budget dependency, disciplinary exposure) and thirty percent internalized (commemorative honor - the sense that skipping exercises dishonors the flood dead - and professional identity fused with the ritual calendar). Theater_ratio at 0.72 is the husk signature: the share of exercise activity that is staging for evaluators rather than skill-building - announced dates, scripted injects, pre-briefed players, camera positions. Accessibility_collapse at 0.48 reflects that alternatives (no-notice exercises, competency-based assessment) remain imaginable and are practiced nowhere at scale: understood but foreclosed by rubric economics, the partial-collapse profile of an enforced arrangement rather than a natural law. Resistance at 0.45 registers veteran dissent, research critique, and occasional audit findings on realism, blunted because the organized payer seat's grievances are settled bilaterally - a coalition of responders, residents, and volunteers is structurally possible but has never formed. The measurement series share one grid (T=0..25, mapped to roughly 2000-2025): extractiveness, theater, and the suppression requirement all harden monotonically as the compliance economy matures - supranational flood directives, safety-region formation, and successive audit mandates each added inspectable surface, which is why the enforcement-capacity series is tracked at all. The annual exercise season oscillates, but the tracked quantities trend, so no cyclical battery is authored.
 *
 * PERSPECTIVAL GAP:
 *   The beneficiary seats and the payer seats compute differently from the same structure. From the ministry and inspectorate seats the cycle is a functioning governance instrument they operate and are judged by; from the regional-director seat it is professional duty fused with career - an identity-locked position in which calling the ceremony hollow is self-annihilating, since peers, promotion paths, and conference life all run through the calendar; from the responder seat it is staged time competing with the unscripted work that built older cohorts' fluency; from the resident seat it is invisible reassurance whose cost is deferred to the flood night. If the directors' professional frame broke - if a credible audit showed near-zero skill transfer - their exit options would loosen, their directionality would rise toward the payer end, and the regime would lose the administrative engine that stages it. The engine derives this divergence from the structural data; the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations place the ministry, directors, inspectors, and executives near the beneficiary end: the cycle subsidizes them with legitimacy, mandate, and credit, and their exits (metric control, office-hopping) are the best in the field. Responder, resident, and volunteer declarations place those seats near the target end: they supply the hours, the taxes, and the deferred capacity cost, with constrained or trapped exits. One override is authored for the powerless seats (flood_plain_residents, volunteer_dike_watch) at d=0.85: the derivation could read the reassurance residents receive, and the stipends and recognition volunteers receive, as partial benefit pulling d below full-target; this reading counts those byproducts as part of the extraction itself - comfortable populations under-adapt privately, and ceremonial recognition substitutes for the field authority the volunteer corps once held - so the override pins them near the target end. Suppression is authored as a raw structural property and is not scaled; extractiveness is what the engine scales by directionality and spatial scope.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem - keeping dispersed response capacity alive across generations of staff turnover - is still real, so the arrangement cannot be dismissed as a solved problem's zombie; the genealogy status is authored contested, not dead, which keeps the obsolescence check from firing falsely. Against the opposite error: the cycle presents as pure coordination (a common calendar, a shared vocabulary), and the husk reading's accounting is what blocks that mislabel - the coordination residue is priced honestly as thin (contact lists and vocabulary survive even ceremonial exercises) while the extraction side (hours, budget, deferred capacity) is concentrated and growing. Naming the agenda setters (ministry, inspectorate) and the payers (responders, residents, volunteers) keeps the classification anchored in who could change the arrangement and who bears its costs; the receipt surface records that the gains land on the ministry seat, and that fixing is prohibitive for whoever could fix it, because replacement would require rewriting statute, dismantling rubric careers, and accepting blame exposure during any transition-period event.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_selection,
    'Which reading of the preparedness_retention kernel is structurally accurate of the exercise-and-inspection regime - memorial performance (this reading), competence preservation, or stratified retention?',
    'Live-event and no-notice exercise outcome audits correlated with prior exercise exposure: if measured skill transfer tracks exercise volume, the competence sibling gains; if transfer is confined to specialized enclaves, the hybrid sibling gains; if negligible everywhere, this reading stands.',
    'A competence_reading verdict would collapse this story''s epsilon toward coordination cost and move the type toward rope or tangled_rope; a hybrid_reading verdict would decompose this story into layered constraints; confirmation leaves the snare classification intact.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_selection, empirical, 'Reading selection for the preparedness_retention kernel; this file instantiates the husk_reading only.').

omega_variable(
    ceremony_to_competence_ratio,
    'What fraction of mandated exercise activity produces retrievable tacit response skill rather than inspectable compliance artifacts?',
    'Paired announced-versus-no-notice exercise designs with blinded performance scoring; longitudinal skill-decay curves for participating cohorts.',
    'Ratios approaching 1.0 confirm the husk structure and sustain the high theater_ratio; mid-range ratios force re-authoring toward the hybrid structure and lower epsilon.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ceremony_to_competence_ratio, empirical, 'The husk reading''s core quantity: ceremony share of exercise activity.').

omega_variable(
    specialized_enclave_competence,
    'Does live technical competence survive in specialized water-defense and crisis organs (the hybrid sibling''s carve-out), such that the broader ceremony draws on a real reservoir - or is the visible apparatus uniformly ceremonial as this reading holds?',
    'Independent skills audit separating specialized engineering and dike organs from general crisis-management organs, scored against identical realistic-task batteries.',
    'If enclaves hold live competence, this reading must scope down to the general societal layer, narrowing the victim set and lowering aggregate epsilon; if they do not, the husk reading extends undivided and the hybrid sibling loses its factual basis.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(specialized_enclave_competence, empirical, 'Location of the disagreement with the hybrid sibling: whether a competence enclave exists behind the ceremony.').

omega_variable(
    counterfactual_capacity_baseline,
    'What would response capacity be in the absence of the mandated exercise regime - did the regime displace a richer informal apprenticeship it now claims credit for, or did it create coordination capacity from nothing?',
    'Historical comparison of pre-mandate informal training and ad-hoc exercise practice against current outputs, controlling for technology and staffing change.',
    'A displaced-apprenticeship finding raises effective extraction above the authored epsilon; a from-nothing finding lowers it toward the enforcement_mechanism coordination floor.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(counterfactual_capacity_baseline, empirical, 'Baseline ambiguity underlying the extraction estimate.').

omega_variable(
    suppression_structural_vs_internalized,
    'Is the regime''s suppression of deviation primarily structural (statute, inspection scoring, budget dependency) or internalized (commemorative honor, professional identity fused with the ritual calendar)?',
    'Post-reform trajectory: if regions granted rubric freedom still stage announced, scripted exercises, the internalized share dominates; if they shift to no-notice formats, suppression was structural.',
    'Internalized dominance means deregulation alone will not restore live exercise - the arrangement would persist as inertial performance after the enforcement machinery is removed; structural dominance means statutory amendment suffices.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'Mechanism split behind the authored suppression value.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(preparedness_retention__husk_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(prep_tr_t0, preparedness_retention__husk_reading, theater_ratio, 0, 0.55).
narrative_ontology:measurement_basis(prep_tr_t0, observed).
narrative_ontology:measurement(prep_tr_t5, preparedness_retention__husk_reading, theater_ratio, 5, 0.6).
narrative_ontology:measurement_basis(prep_tr_t5, observed).
narrative_ontology:measurement(prep_tr_t10, preparedness_retention__husk_reading, theater_ratio, 10, 0.64).
narrative_ontology:measurement_basis(prep_tr_t10, observed).
narrative_ontology:measurement(prep_tr_t15, preparedness_retention__husk_reading, theater_ratio, 15, 0.67).
narrative_ontology:measurement_basis(prep_tr_t15, observed).
narrative_ontology:measurement(prep_tr_t20, preparedness_retention__husk_reading, theater_ratio, 20, 0.7).
narrative_ontology:measurement_basis(prep_tr_t20, observed).
narrative_ontology:measurement(prep_tr_t25, preparedness_retention__husk_reading, theater_ratio, 25, 0.72).
narrative_ontology:measurement_basis(prep_tr_t25, observed).

% Extraction over time
narrative_ontology:measurement(prep_be_t0, preparedness_retention__husk_reading, base_extractiveness, 0, 0.44).
narrative_ontology:measurement_basis(prep_be_t0, observed).
narrative_ontology:measurement(prep_be_t5, preparedness_retention__husk_reading, base_extractiveness, 5, 0.5).
narrative_ontology:measurement_basis(prep_be_t5, observed).
narrative_ontology:measurement(prep_be_t10, preparedness_retention__husk_reading, base_extractiveness, 10, 0.55).
narrative_ontology:measurement_basis(prep_be_t10, observed).
narrative_ontology:measurement(prep_be_t15, preparedness_retention__husk_reading, base_extractiveness, 15, 0.59).
narrative_ontology:measurement_basis(prep_be_t15, observed).
narrative_ontology:measurement(prep_be_t20, preparedness_retention__husk_reading, base_extractiveness, 20, 0.63).
narrative_ontology:measurement_basis(prep_be_t20, observed).
narrative_ontology:measurement(prep_be_t25, preparedness_retention__husk_reading, base_extractiveness, 25, 0.66).
narrative_ontology:measurement_basis(prep_be_t25, observed).

% Suppression requirement over time
narrative_ontology:measurement(prep_su_t0, preparedness_retention__husk_reading, suppression_requirement, 0, 0.38).
narrative_ontology:measurement_basis(prep_su_t0, observed).
narrative_ontology:measurement(prep_su_t5, preparedness_retention__husk_reading, suppression_requirement, 5, 0.44).
narrative_ontology:measurement_basis(prep_su_t5, observed).
narrative_ontology:measurement(prep_su_t10, preparedness_retention__husk_reading, suppression_requirement, 10, 0.49).
narrative_ontology:measurement_basis(prep_su_t10, observed).
narrative_ontology:measurement(prep_su_t15, preparedness_retention__husk_reading, suppression_requirement, 15, 0.53).
narrative_ontology:measurement_basis(prep_su_t15, observed).
narrative_ontology:measurement(prep_su_t20, preparedness_retention__husk_reading, suppression_requirement, 20, 0.56).
narrative_ontology:measurement_basis(prep_su_t20, observed).
narrative_ontology:measurement(prep_su_t25, preparedness_retention__husk_reading, suppression_requirement, 25, 0.58).
narrative_ontology:measurement_basis(prep_su_t25, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(preparedness_retention__husk_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(preparedness_retention__husk_reading, preparedness_retention__competence_reading).
narrative_ontology:affects_constraint(preparedness_retention__husk_reading, preparedness_retention__hybrid_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'preparedness retention' decomposes into three structurally distinct claims about the same standing exercise-and-inspection arrangement, authored as separate constraint files per the epsilon-invariance principle. This file authors the husk_reading (memorial performance; high ceremony-to-competence ratio; beneficiary is institutional legitimacy, victim is response capacity). The competence_reading authors the regime's self-description and functions as the upstream legitimacy source this reading parasitizes; the hybrid_reading authors the negotiated settlement (competence enclaves plus societal ceremony) that the husk critique pressures downstream. Measuring the arrangement one way yields low epsilon and another way high epsilon precisely because the label conflates the three readings; the family link replaces any attempt to average them inside one story.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(preparedness_retention__husk_reading, powerless, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
