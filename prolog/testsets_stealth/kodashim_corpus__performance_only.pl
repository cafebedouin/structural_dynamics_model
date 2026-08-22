% ============================================================================
% CONSTRAINT STORY: kodashim_corpus__performance_only
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_kodashim_corpus__performance_only, []).

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
 *   constraint_id: kodashim_corpus__performance_only
 *   human_readable: Kodashim Corpus as Dormant Blueprint (Performance-Only Reading)
 *   domain: religious_studies/rabbinic_judaism/commitment_system
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the kodashim_corpus kernel:
 *   performance_only, under which the sacrificial-law corpus is a dormant
 *   blueprint — fully authoritative, wholly inoperative — awaiting messianic
 *   restoration of physical sacrifice. The standing arrangement under contest
 *   (the ε referent) is the institutional complex that teaches the corpus as
 *   preparation, builds restoration infrastructure, and collects devotion,
 *   tuition, and donations against a payoff no current agent can deliver. Two
 *   sibling readings instantiate different constraints from the same kernel
 *   and are NOT part of this story: study_as_exercise (study itself performs
 *   the mitzvah — the kernel is occupied now, and study delivers what it
 *   claims, so ε drops sharply) and substitution_archive (prayer and study
 *   superseded sacrifice — the corpus is memorial, nothing is promised, so ε
 *   drops). The colloquial label 'studying Kodashim' therefore decomposes
 *   into three structurally distinct arrangements with different ε values,
 *   linked via network.affects_constraints. KEY AGENTS (by structural
 *   relationship): messianic_preparation_institutions — agenda-setter
 *   (institutional/identity_locked), administers the reading and collects the
 *   gains; restoration_devotees — primary target (moderate/identity_locked),
 *   bears misallocated devotion; kodashim_teachers — secondary beneficiary
 *   (moderate/constrained), livelihood and standing;
 *   yeshiva_students_in_kodashim_tracks — secondary target
 *   (powerless/trapped); diaspora_laity_funding_restoration — financial
 *   target (moderate/constrained); sibling_reading_communities — excluded
 *   (organized/mobile); comparative_religion_historians — analytical
 *   observer.
 *
 * KEY AGENTS:
 *   - messianic_preparation_institutions: Agenda-setter (institutional/identity_locked) — administers the anticipatory apparatus, collects tuition, donations, and standing
 *   - restoration_devotees: Primary target (moderate/identity_locked) — bears misallocated devotion against a permanently deferred fulfillment
 *   - kodashim_teachers: Secondary beneficiary (moderate/constrained) — collects salary and scholarly standing; expertise is corpus-specific
 *   - yeshiva_students_in_kodashim_tracks: Secondary target (powerless/trapped) — assigned the curriculum before dissent can mature
 *   - diaspora_laity_funding_restoration: Financial target (moderate/constrained) — remote giving sustained by communal ties
 *   - sibling_reading_communities: Excluded (organized/mobile) — hold the rival readings, kept at the boundary
 *   - comparative_religion_historians: Analytical observer (analytical/analytical) — sees the full structure, holds no seat inside it
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(kodashim_corpus__performance_only, 0.76).
domain_priors:suppression_score(kodashim_corpus__performance_only, 0.58).
domain_priors:theater_ratio(kodashim_corpus__performance_only, 0.52).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(kodashim_corpus__performance_only, extractiveness, 0.76).
narrative_ontology:constraint_metric(kodashim_corpus__performance_only, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(kodashim_corpus__performance_only, theater_ratio, 0.52).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(kodashim_corpus__performance_only, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(kodashim_corpus__performance_only, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(kodashim_corpus__performance_only, snare).
narrative_ontology:human_readable(kodashim_corpus__performance_only, "Kodashim Corpus as Dormant Blueprint (Performance-Only Reading)").
narrative_ontology:topic_domain(kodashim_corpus__performance_only, "religious_studies/rabbinic_judaism/commitment_system").

domain_priors:requires_active_enforcement(kodashim_corpus__performance_only).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(kodashim_corpus__performance_only, 'd61393ac-e045-438f-80a1-e109adfd052b').
narrative_ontology:cs_kernel_codification('d61393ac-e045-438f-80a1-e109adfd052b', fixed_text).
narrative_ontology:cs_authority_grounding('d61393ac-e045-438f-80a1-e109adfd052b', lineage).
narrative_ontology:cs_interpretation_layer_present('d61393ac-e045-438f-80a1-e109adfd052b').
narrative_ontology:cs_reading_relation('d61393ac-e045-438f-80a1-e109adfd052b', kodashim_corpus__study_as_exercise, forecloses).
narrative_ontology:cs_reading_relation('d61393ac-e045-438f-80a1-e109adfd052b', kodashim_corpus__substitution_archive, forecloses).
narrative_ontology:cs_axiom('d61393ac-e045-438f-80a1-e109adfd052b', foundational, physical_avodah_required_for_fulfillment).
narrative_ontology:cs_axiom_status(physical_avodah_required_for_fulfillment, holdable).
narrative_ontology:cs_axiom_grounding('d61393ac-e045-438f-80a1-e109adfd052b', physical_avodah_required_for_fulfillment, deontological).
narrative_ontology:cs_axiom('d61393ac-e045-438f-80a1-e109adfd052b', foundational, sacrificial_service_resumes_messianically).
narrative_ontology:cs_axiom_status(sacrificial_service_resumes_messianically, holdable).
narrative_ontology:cs_axiom_grounding('d61393ac-e045-438f-80a1-e109adfd052b', sacrificial_service_resumes_messianically, theological).
narrative_ontology:cs_reference_frame('d61393ac-e045-438f-80a1-e109adfd052b', dormant_blueprint_awaiting_restoration).
narrative_ontology:cs_drift_state('d61393ac-e045-438f-80a1-e109adfd052b', contemporary, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('d61393ac-e045-438f-80a1-e109adfd052b', '').
narrative_ontology:cs_kernel_id(kodashim_corpus__performance_only, kodashim_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(kodashim_corpus__performance_only, messianic_preparation_institutions).
narrative_ontology:constraint_beneficiary(kodashim_corpus__performance_only, kodashim_teachers).
narrative_ontology:constraint_victim(kodashim_corpus__performance_only, restoration_devotees).
narrative_ontology:constraint_victim(kodashim_corpus__performance_only, yeshiva_students_in_kodashim_tracks).
narrative_ontology:constraint_victim(kodashim_corpus__performance_only, diaspora_laity_funding_restoration).
narrative_ontology:constraint_vindicates(kodashim_corpus__performance_only, messianic_restoration_doctrine).
narrative_ontology:constraint_vindicates(kodashim_corpus__performance_only, prophetic_sacrifice_resumption).
narrative_ontology:constraint_vindicates(kodashim_corpus__performance_only, study_as_preparation_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Operate yeshiva tracks devoted to the sacrificial order, publish restoration blueprints and vessel specifications, train aspirant priests, and run reconstruction projects. Income arrives as tuition, earmarked donations, and publication revenue; standing arrives as recognition as the faithful custodians of readiness. Curricula, staffing, and fundraising all presuppose that the corpus will become operative, and the institution's own continuation is bound to that presupposition. Leaving the mission would mean dissolving the organization's reason for existing.
narrative_ontology:constraint_stakeholder(kodashim_corpus__performance_only, messianic_preparation_institutions, agenda_setter,
    institutional, generational, identity_locked, global).

% Structure daily study and household giving around readiness for a service they cannot perform. They memorize procedure sequences, volunteer at reconstruction sites, and treat each completed tractate as one step closer to qualified participation. The fulfillment they seek is permanently scheduled for a day no one can name; meanwhile their calendars, budgets, and social worlds are organized by the anticipation. Stepping back would mean remaking their devotional identity, friendships, and self-understanding.
narrative_ontology:constraint_stakeholder(kodashim_corpus__performance_only, restoration_devotees, payer,
    moderate, biographical, identity_locked, global).

% Make their livelihood teaching the sacrificial order inside the preparation institutions. They hold rare specialized expertise, earn salaries and scholarly reputation from the enrollment the anticipatory framing sustains, and would face a thin market for that expertise outside these institutions. Some privately hold more present-tense framings of the material but teach within the frame their employer maintains.
narrative_ontology:constraint_stakeholder(kodashim_corpus__performance_only, kodashim_teachers, beneficiary,
    moderate, biographical, constrained, global).

% Are enrolled in curricula that assign the sacrificial order as obligatory preparation. They are young, financially dependent on the institutions, and formed socially inside them; opting out of the track means leaving the school, and often the community, at an age when both constitute their entire world.
narrative_ontology:constraint_stakeholder(kodashim_corpus__performance_only, yeshiva_students_in_kodashim_tracks, payer,
    powerless, immediate, trapped, regional).

% Donate from distant communities to vessel reconstruction, priestly training, and publication projects they will never personally operate. Their giving is sustained by newsletters, speaking tours, and communal fundraising events; stopping is socially uncomplicated but cuts against friendships, family expectations, and the story they tell about their own faithfulness.
narrative_ontology:constraint_stakeholder(kodashim_corpus__performance_only, diaspora_laity_funding_restoration, payer,
    moderate, biographical, constrained, global).

% Hold and teach the rival readings of the same corpus — that study itself performs the service, or that prayer and study superseded it. Inside this reading's institutions their framings are treated as misunderstanding to be corrected rather than positions to be answered; they publish, debate, and recruit at the boundary but have no seat in the internal deliberations where curricula and fundraising are set.
narrative_ontology:constraint_stakeholder(kodashim_corpus__performance_only, sibling_reading_communities, excluded,
    organized, generational, mobile, global).

% Document the corpus's transmission history, the institutionalization of restoration-preparation, and the defection patterns between readings. They collect archives, interview leavers, and publish analyses; they neither fund nor staff the arrangement, and their assessments carry no formal weight inside its deliberations.
narrative_ontology:constraint_stakeholder(kodashim_corpus__performance_only, comparative_religion_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(kodashim_corpus__performance_only, messianic_preparation_institutions).
narrative_ontology:fixing_cost_class(kodashim_corpus__performance_only, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates dispersed communities around a shared anticipatory discipline: a standardized curriculum for the sacrificial order, preservation of procedural knowledge, and a collective readiness culture maintained across generations in the absence of a functioning temple.
% TRANSFER_FUNCTION: Moves devotion, study-years, tuition, and earmarked donations from individual adherents and distant sympathizers to the preparation institutions, against a promised return — mitzvah fulfillment in a restored service — that no current agent can deliver or schedule.
% ABSENT_VOICES: Doubting devotees have no sanctioned channel: within the institutions, hesitation about the restoration premise reads as weak faith rather than a position, so it surfaces only as quiet attrition. Holders of the sibling readings stand outside the wall — they argue publicly at the boundary but are not admitted to the conversations where curricula, staffing, and fundraising are decided.
% DISAPPEARANCE_RATIONALE: The texts themselves would survive under any reading — preservation is not unique to this arrangement. What would vanish is the anticipatory apparatus: the tuition stream, the earmarked donation pipelines, the priest-training tracks, the reconstruction projects. Devotees would redistribute their study time to sibling-reading frameworks or to other orders of study, and institutional staff and donors would follow the redirected demand. The parties' arrangements demonstrably depend on the constraint's persistence.
% FOUNDING_PROBLEM: After the destruction of the Second Temple, preserve complete procedural knowledge of the sacrificial service so that worship could resume upon restoration.
% FOUNDING_PROBLEM_CORROBORATION: The institutions attest the problem is live: restoration may come at any time, so readiness must be continuous. Outside the benefiting parties, academic historians of rabbinic literature document that the preservation goal was substantially achieved many centuries ago; the sibling-reading academies attest the same from inside the tradition; and observed defection patterns show adherents resolving the deferral tension by reframing rather than by detecting imminence. No source outside the beneficiary set attests that continued preparation tracks a verifiable near-term resumption.
narrative_ontology:disappearance_verdict(kodashim_corpus__performance_only, world_rearranges).
narrative_ontology:founding_problem_status(kodashim_corpus__performance_only, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(kodashim_corpus__performance_only, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(kodashim_corpus__performance_only, 'none', 1).
narrative_ontology:epsilon_provenance(kodashim_corpus__performance_only, 0.76, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(kodashim_corpus__performance_only_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(kodashim_corpus__performance_only, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(kodashim_corpus__performance_only_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored high (0.76 at interval end) because the arrangement takes present, concrete goods — study-years, tuition, earmarked donations, devotional identity — against a return (mitzvah fulfillment in a restored service) that no agent in the arrangement can produce, verify, or schedule; legitimacy is collected continuously while delivery is structurally deferred. Suppression (0.58) is authored as a raw structural property — it is NOT scaled by power or scope; only extractiveness is scaled by the engine's directionality and scope modifiers. Its mechanism is predominantly internalized (identity fusion with anticipated participation, fear of arriving unprepared at the moment that matters) layered over mild structural pressure (educational dependence, communal standing); the omega suppression_internalization_split carries the split. Theater (0.52) reflects the growth of rehearsal activity — vessel reconstruction, priestly reenactment training, preparatory procedure drills — that performs readiness without performing anything else, balanced against the genuinely functional textual-preservation core. Accessibility_collapse (0.35) is low: once the deferral structure is seen, the sibling readings and ordinary curricula remain fully available; nothing structural blocks them. Resistance (0.45) is moderate: the historical record shows adherents resolving the deferral tension chiefly by migrating to sibling readings rather than by confronting the institutions. The measurement series run on one shared seven-point grid so every tracked metric is authored at every examined time point; the secular trend is institutionalization (rising extraction, theater, and enforcement capacity as informal communal expectation matured into organized apparatus), with messianic-fervor cycles superimposed — the cycles modulate intensity, but the deferral structure, not the oscillation, is the extraction mechanism. Coalition potential among the powerless seat (students) is blunted structurally: dissent would have to mature inside the very institutions that form their identity and livelihood prospects. Boltzmann note: identity_coordination is declared because the arrangement does coordinate membership and boundary-maintenance (who counts as among-the-ready); the known gaming risk is acknowledged — 'this is who we are' is precisely the cover story this reading deploys, so coupling that concentrates extraction on low-power agents at wide scope should be read as nonsensical despite the complexity offset.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter seat the arrangement computes as faithful stewardship: a hard, expensive duty maintained across generations against indifference — rope-like from inside. From the payer seats the same structure operates as perpetual deferral: every present contribution buys a credit redeemable only at an event no one controls. The teacher seat splits: a genuine scholarly function (the corpus is real, difficult, and worth preserving) is entangled with a framing that converts that function into open-ended obligation. The engine computes these per-seat classifications from the structural data; the divergence between the institution's self-understanding and the devotees' experienced deferral is the measurement this story exists to take.
 *
 * DIRECTIONALITY LOGIC:
 *   The beneficiary/victim declarations drive the derivation. messianic_preparation_institutions sit nearest the beneficiary pole (they collect tuition, donations, and standing; d near 0.05–0.1), amplified rather than offset by their identity_locked exit — the institution has become its mission. kodashim_teachers derive mildly beneficiary (salary and standing, constrained exit; d roughly 0.2–0.3). restoration_devotees, yeshiva_students_in_kodashim_tracks, and diaspora_laity_funding_restoration derive near the target pole (they transfer devotion, time, and money; d roughly 0.8–0.9), with identity lock pushing the devotees furthest toward full-target. sibling_reading_communities are excluded rather than coordinated — keeping their framings at the boundary is part of what the enforcement machinery does — and comparative_religion_historians take the analytical seat. No directionality overrides were needed: the derivation from declared roles, power, and exit options reproduces the structural relationships.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — preserve complete sacrificial procedure through exile so worship could resume — was originally a genuine transitional charge, scaffold-shaped: carry the knowledge until restoration. The knowledge-preservation half is long since accomplished; what persists is the anticipation apparatus, whose continuation no longer tracks the founding problem but the institutions' own reproduction. The R5 mismatch (status=contested crossed with verdict=world_rearranges) routes the capture/zombie cross-check against the computed theater path. Classifying this reading as snare prevents the opposite error: reading the residual preservation function as the arrangement's present justification (which would license rope), when under this reading's own lights preservation is instrumental to a promised resumption that does the legitimating work. Resolution stays open via the omegas: if the community adopted a sibling reading, the arrangement would resolve into either a rope (study-as-exercise: real present-tense coordination) or a plain curated archive (substitution: no constraint beyond curation).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_committer_structure,
    'This constraint is one reading (performance_only) of the kodashim_corpus kernel; how would instantiating a sibling reading change the structural data?',
    'Adopting study_as_exercise relocates performance into the present (extractiveness collapses toward coordination cost, since study delivers what it claims); adopting substitution_archive converts the corpus to memorial (the promised-delivery structure disappears entirely). The disagreement is located in one structural element: the corpus''s ontological status — dormant blueprint versus occupied-through-study versus superseded memorial.',
    'Under either sibling reading this snare classification does not survive: the victim set (misallocated devotees) dissolves because study either delivers now or promises nothing. The snare verdict is conditional on this reading holding.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_committer_structure, conceptual, 'Committer structure: one of three readings of the kodashim kernel; sibling adoption dissolves the snare structure.').

omega_variable(
    resumption_unverifiability,
    'Can the promised resumption of physical sacrifice be verified or falsified within any horizon available to current agents?',
    'None available in principle inside the framework: the messianic event is exogenous to every agent in the arrangement. Resolution would require either the event itself or the community adopting a sibling reading that dissolves the promise.',
    'As long as the question stays open, legitimacy continues to accrue against an undeliverable return and the snare structure persists; a sibling-reading migration is the only internal resolution path.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(resumption_unverifiability, conceptual, 'The constraint''s legitimating promise is unverifiable within any agent horizon.').

omega_variable(
    institutional_sincerity_vs_capture,
    'Do the preparation institutions knowingly collect against an undeliverable return, or do they sincerely hold the restoration premise?',
    'Financial disclosure under independent review, leadership statements under adversarial conditions, and comparison of private curricular choices against public fundraising claims.',
    'If capture is established, the arrangement is pure rent collection and the snare verdict hardens; if sincere, part of the measured extraction is the cost of a genuinely held (if unverifiable) conviction, and the moral accounting shifts without changing the structural deferral.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(institutional_sincerity_vs_capture, empirical, 'Whether institutional gains are cynical rents or sincere mission costs.').

omega_variable(
    misallocated_devotion_counterfactual,
    'Is the devotees'' allocation genuinely misallocated, or does preparation carry option value proportional to the probability of resumption?',
    'No decisive resolution: the answer depends on the probability assigned to resumption, which the framework itself forbids estimating from evidence. Bayesian updates arrive only through lived events interpreted inside the framework.',
    'A high subjective probability makes the arrangement insurance-like (lower effective extraction against the devotee seat); a low probability makes it pure deferral loss. The victim designation is robust across the range, but severity is not.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(misallocated_devotion_counterfactual, conceptual, 'Victimhood depends on the unestimable probability of the promised restoration.').

omega_variable(
    suppression_internalization_split,
    'Is the measured suppression structural (educational dependence, communal standing, marriage and business networks) or internalized (identity fusion with anticipated participation, fear of arriving unprepared)?',
    'Post-exit suppression trajectory: track leavers who adopt sibling readings or leave the specialty; if deference-shaped hesitation and guilt persist after all structural ties are severed, the internalized share is substantial.',
    'If internalized, effective suppression is higher than the structural measure suggests — leavers carry the constraint with them — and exit-option atoms overstate the freedom of the moderate-power payer seats.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_internalization_split, empirical, 'Structural versus internalized suppression mechanism in the payer seats.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(kodashim_corpus__performance_only, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(koda_tr_t0, kodashim_corpus__performance_only, theater_ratio, 0, 0.15).
narrative_ontology:measurement(koda_tr_t10, kodashim_corpus__performance_only, theater_ratio, 10, 0.22).
narrative_ontology:measurement(koda_tr_t20, kodashim_corpus__performance_only, theater_ratio, 20, 0.3).
narrative_ontology:measurement(koda_tr_t30, kodashim_corpus__performance_only, theater_ratio, 30, 0.38).
narrative_ontology:measurement(koda_tr_t40, kodashim_corpus__performance_only, theater_ratio, 40, 0.45).
narrative_ontology:measurement(koda_tr_t50, kodashim_corpus__performance_only, theater_ratio, 50, 0.49).
narrative_ontology:measurement(koda_tr_t60, kodashim_corpus__performance_only, theater_ratio, 60, 0.52).

% Extraction over time
narrative_ontology:measurement(koda_be_t0, kodashim_corpus__performance_only, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(koda_be_t10, kodashim_corpus__performance_only, base_extractiveness, 10, 0.55).
narrative_ontology:measurement(koda_be_t20, kodashim_corpus__performance_only, base_extractiveness, 20, 0.62).
narrative_ontology:measurement(koda_be_t30, kodashim_corpus__performance_only, base_extractiveness, 30, 0.68).
narrative_ontology:measurement(koda_be_t40, kodashim_corpus__performance_only, base_extractiveness, 40, 0.72).
narrative_ontology:measurement(koda_be_t50, kodashim_corpus__performance_only, base_extractiveness, 50, 0.74).
narrative_ontology:measurement(koda_be_t60, kodashim_corpus__performance_only, base_extractiveness, 60, 0.76).

% Suppression requirement over time
narrative_ontology:measurement(koda_su_t0, kodashim_corpus__performance_only, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(koda_su_t10, kodashim_corpus__performance_only, suppression_requirement, 10, 0.32).
narrative_ontology:measurement(koda_su_t20, kodashim_corpus__performance_only, suppression_requirement, 20, 0.4).
narrative_ontology:measurement(koda_su_t30, kodashim_corpus__performance_only, suppression_requirement, 30, 0.47).
narrative_ontology:measurement(koda_su_t40, kodashim_corpus__performance_only, suppression_requirement, 40, 0.53).
narrative_ontology:measurement(koda_su_t50, kodashim_corpus__performance_only, suppression_requirement, 50, 0.56).
narrative_ontology:measurement(koda_su_t60, kodashim_corpus__performance_only, suppression_requirement, 60, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(kodashim_corpus__performance_only, identity_coordination).
narrative_ontology:affects_constraint(kodashim_corpus__performance_only, kodashim_corpus__study_as_exercise).
narrative_ontology:affects_constraint(kodashim_corpus__performance_only, kodashim_corpus__substitution_archive).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition per the ε-invariance principle: the colloquial label 'studying Kodashim' covers three structurally distinct arrangements instantiated from one kernel. performance_only (this file, ε≈0.76, snare: legitimacy drawn from an undeliverable future) sits downstream of the fixed-text kernel's lineage authority; study_as_exercise (ε low: study delivers what it claims) and substitution_archive (ε low: nothing promised, nothing withheld) dissolve the victim set entirely. Upstream/downstream structure: the performance_only reading cites the corpus's binding authority (the upstream claim) as warrant for present obligation; the sibling readings cite the same authority to deny that warrant. Each family member links the others via network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
