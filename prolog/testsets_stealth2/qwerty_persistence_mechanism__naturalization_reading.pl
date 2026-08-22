% ============================================================================
% CONSTRAINT STORY: qwerty_persistence_mechanism__naturalization_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_qwerty_persistence_mechanism__naturalization_reading, []).

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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:measurement_basis/2,
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
 *   constraint_id: qwerty_persistence_mechanism__naturalization_reading
 *   human_readable: QWERTY Persistence as Market-Tested Adequacy (Naturalization Reading)
 *   domain: economic_history/technology_studies
 *
 * SUMMARY:
 *   The QWERTY letter arrangement, developed in the 1870s for mechanical
 *   typewriters and spread by Remington mass production, remains the dominant
 *   input layout across roughly a century and a half of successive device
 *   generations. This story instantiates the NATURALIZATION READING of the
 *   contested persistence kernel: the arrangement persists because it is
 *   genuinely adequate for real typing work, rival layouts (Dvorak, Colemak)
 *   lapsed through fair competition after failing to demonstrate decisive
 *   advantages under honest measurement, and the costs of switching reflect
 *   genuine skill investment rather than imposed barriers. No seat
 *   administers, enforces, or collects from the arrangement; it maintains
 *   itself through use. Per Rule 1, the contest with the sibling readings is
 *   NOT described inside this constraint: the rival structures (trapped
 *   typists under the lock-in reading; capturing manufacturers under the
 *   extraction reading) live in separate files linked through the network
 *   section, and the disagreement's locations are carried as omega variables.
 *   The claim/metrics split is deliberate and independent: the arrangement is
 *   CLAIMED as pure coordination, and the authored metrics independently
 *   describe low-extraction, low-suppression, low-theater operation on a
 *   shared seven-point time grid. KEY AGENTS (by structural relationship): -
 *   touch_typists_collectively: net beneficiary (moderate/constrained) —
 *   holds the arrangement's value in portable motor skill -
 *   employers_of_trained_typists: beneficiary (institutional/mobile) — hires
 *   against a universal skill referent - keyboard_device_manufacturers:
 *   passive beneficiary (institutional/mobile) — builds to the practiced
 *   arrangement, holds no administrative lever - keyboard_standards_bodies:
 *   agenda_setter in form only (institutional/constrained) — ratifies and
 *   codifies practice after the fact - individual_layout_switchers:
 *   beneficiary who tested exit (moderate/constrained) — retrained, measured,
 *   reverted - dvorak_colemak_advocates: excluded (moderate/mobile) — hold
 *   the rival superiority claim outside the equilibrium -
 *   technology_historians_economists: observer (analytical/analytical) —
 *   adjudicate the empirical record
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(qwerty_persistence_mechanism__naturalization_reading, 0.12).
domain_priors:suppression_score(qwerty_persistence_mechanism__naturalization_reading, 0.08).
domain_priors:theater_ratio(qwerty_persistence_mechanism__naturalization_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(qwerty_persistence_mechanism__naturalization_reading, extractiveness, 0.12).
narrative_ontology:constraint_metric(qwerty_persistence_mechanism__naturalization_reading, suppression_requirement, 0.08).
narrative_ontology:constraint_metric(qwerty_persistence_mechanism__naturalization_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(qwerty_persistence_mechanism__naturalization_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(qwerty_persistence_mechanism__naturalization_reading, resistance, 0.18).

% --- Constraint claim ---
narrative_ontology:constraint_claim(qwerty_persistence_mechanism__naturalization_reading, rope).
narrative_ontology:human_readable(qwerty_persistence_mechanism__naturalization_reading, "QWERTY Persistence as Market-Tested Adequacy (Naturalization Reading)").
narrative_ontology:topic_domain(qwerty_persistence_mechanism__naturalization_reading, "economic_history/technology_studies").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(qwerty_persistence_mechanism__naturalization_reading, 'fc9e0850-8a15-402d-97e1-51d7c5f6d205').
narrative_ontology:cs_kernel_codification('fc9e0850-8a15-402d-97e1-51d7c5f6d205', distributed).
narrative_ontology:cs_authority_grounding('fc9e0850-8a15-402d-97e1-51d7c5f6d205', practice).
narrative_ontology:cs_interpretation_layer_present('fc9e0850-8a15-402d-97e1-51d7c5f6d205').
narrative_ontology:cs_reading_relation('fc9e0850-8a15-402d-97e1-51d7c5f6d205', qwerty_persistence_mechanism__lock_in_reading, forecloses).
narrative_ontology:cs_reading_relation('fc9e0850-8a15-402d-97e1-51d7c5f6d205', qwerty_persistence_mechanism__beneficiary_extraction_reading, forecloses).
narrative_ontology:cs_axiom('fc9e0850-8a15-402d-97e1-51d7c5f6d205', foundational, qwerty_adequacy_market_vindicated).
narrative_ontology:cs_axiom_status(qwerty_adequacy_market_vindicated, holdable).
narrative_ontology:cs_axiom_grounding('fc9e0850-8a15-402d-97e1-51d7c5f6d205', qwerty_adequacy_market_vindicated, empirically_contingent).
narrative_ontology:cs_axiom('fc9e0850-8a15-402d-97e1-51d7c5f6d205', foundational, switching_costs_constitute_skill_capital).
narrative_ontology:cs_axiom_status(switching_costs_constitute_skill_capital, holdable).
narrative_ontology:cs_axiom_grounding('fc9e0850-8a15-402d-97e1-51d7c5f6d205', switching_costs_constitute_skill_capital, empirically_contingent).
narrative_ontology:cs_reference_frame('fc9e0850-8a15-402d-97e1-51d7c5f6d205', market_tested_adequacy_baseline).
narrative_ontology:cs_drift_state('fc9e0850-8a15-402d-97e1-51d7c5f6d205', contemporary_input_method_transition, gap(stable, minor, true)).
narrative_ontology:cs_created_at('fc9e0850-8a15-402d-97e1-51d7c5f6d205', '2026-08-20T14:32:00Z').
narrative_ontology:cs_kernel_id(qwerty_persistence_mechanism__naturalization_reading, qwerty_persistence_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(qwerty_persistence_mechanism__naturalization_reading, touch_typists_collectively).
narrative_ontology:constraint_beneficiary(qwerty_persistence_mechanism__naturalization_reading, employers_of_trained_typists).
narrative_ontology:constraint_beneficiary(qwerty_persistence_mechanism__naturalization_reading, keyboard_device_manufacturers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(qwerty_persistence_mechanism__naturalization_reading, individual_layout_switchers).
narrative_ontology:constraint_vindicates(qwerty_persistence_mechanism__naturalization_reading, market_selection_adequacy_thesis).
narrative_ontology:constraint_vindicates(qwerty_persistence_mechanism__naturalization_reading, liebowitz_margolis_keys_correction).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hundreds of millions of people whose typing skill, acquired once, works on every machine, in every employer's office, and in every country using the Latin alphabet. They bear no fee and face no enforcement; leaving the shared layout would mean retraining at their own expense for a benefit the record has not demonstrated. Their aggregate habit is the only thing that maintains the arrangement.
narrative_ontology:constraint_stakeholder(qwerty_persistence_mechanism__naturalization_reading, touch_typists_collectively, beneficiary,
    moderate, biographical, constrained, global).

% Organizations that hire against a universal skill referent: any applicant trained anywhere is productive on day one, and internal retraining programs face no vendor lock. Some firms experimented with alternative layouts during efficiency campaigns and reverted; none reports interference in its choice.
narrative_ontology:constraint_stakeholder(qwerty_persistence_mechanism__naturalization_reading, employers_of_trained_typists, beneficiary,
    institutional, generational, mobile, global).

% Device makers who build to the arrangement customers already know, selling into a market where layout novelty is a support cost rather than a feature. They produce what demand coordinates on; alternative-layout boards appear on price lists and sell in vanishing volumes. They hold no lever over the arrangement itself and collect no fee from it.
narrative_ontology:constraint_stakeholder(qwerty_persistence_mechanism__naturalization_reading, keyboard_device_manufacturers, beneficiary,
    institutional, generational, mobile, global).

% National and international standards committees that publish formal layout specifications. Their documents ratify what practice has already settled; they possess no mechanism to move the installed base of habits, labels, and curricula, and their revisions track usage rather than lead it. Administering the codification is their entire role in the arrangement.
narrative_ontology:constraint_stakeholder(qwerty_persistence_mechanism__naturalization_reading, keyboard_standards_bodies, agenda_setter,
    institutional, generational, constrained, global).

% Office workers, writers, and enthusiasts who retrained on alternative layouts during productivity or ergonomics waves. Most reverted when measured gains fell short of the retraining disruption or failed to replicate in their own workflows; a minority stayed and report satisfaction. Their round trip is the market test this arrangement's continuance rests on: exit was available, attempted, and declined on the merits.
narrative_ontology:constraint_stakeholder(qwerty_persistence_mechanism__naturalization_reading, individual_layout_switchers, beneficiary,
    moderate, biographical, constrained, global).

% Communities and small vendors who maintain alternative layouts, publish comparative studies, and sell training materials. They hold no procurement seat, no standards leverage, and shrinking mainstream attention. Their claim — that a demonstrably better arrangement lost out — is the assertion this arrangement's continued dominance weighs against; they operate entirely outside the equilibrium they criticize.
narrative_ontology:constraint_stakeholder(qwerty_persistence_mechanism__naturalization_reading, dvorak_colemak_advocates, excluded,
    moderate, biographical, mobile, global).

% Researchers who reconstruct the design history from Remington-era archives, audit the conflict-of-interest structure of the mid-century rival-layout studies, and test persistence explanations against switching-cost data. They hold no stake in any layout and supply the external corroboration for the genealogy interview.
narrative_ontology:constraint_stakeholder(qwerty_persistence_mechanism__naturalization_reading, technology_historians_economists, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(qwerty_persistence_mechanism__naturalization_reading, diffuse).
narrative_ontology:fixing_cost_class(qwerty_persistence_mechanism__naturalization_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the recurring collective-action problem of typing-skill portability: one shared letter arrangement makes motor training, keycap labeling, curricula, proficiency exams, hiring tests, and repair knowledge interoperate across every machine, employer, and country using the Latin alphabet. Each new typist's training is usable everywhere; each device is usable by every trained hand.
% TRANSFER_FUNCTION: Moves essentially nothing. No fee, tribute, or work product flows to any seat by virtue of the arrangement. The only transfer inside it is the ordinary one within skill formation: a learner's practice hours convert into the learner's own portable human capital.
% ABSENT_VOICES: Alternative-layout advocates and some ergonomics researchers hold the claim that a superior arrangement was crowded out; they stand outside the equilibrium with no procurement seat and no standards leverage. Historians who dispute the jamming-origin story likewise sit outside the practical conversation, which proceeds as if the layout were a given of nature rather than a dated artifact.
% DISAPPEARANCE_RATIONALE: Overnight removal would strand the largest stock of trained motor skill in history: every labeled keycap, typing curriculum, proficiency exam, and hiring pipeline would lose its referent simultaneously, and hundreds of millions of typists would face immediate retraining or degraded input on every device they touch. The world would rearrange around whatever successor convention emerged, at enormous coordination cost — the definitive signature that arrangements depend on it.
% FOUNDING_PROBLEM: Managing type-bar collisions and operator pacing in 1870s mechanical typewriters: adjacent frequently-struck bars jammed, so the arrangement separated common letter pairs, and on some accounts accommodated telegraph transcribers' rhythm.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside any benefiting party: historians of technology, including the archival reconstruction of the Remington-era development published by the Yasuokas, attest that the mechanical jamming rationale expired with type-bar machines and that portions of the origin story are later mythologizing. No contemporary beneficiary seat invokes the founding problem at all — typists, employers, and manufacturers justify the arrangement, when they justify it at all, purely by its present-day adequacy. The mismatch between the dead founding problem and the world-rearranging dependence is carried openly for the capture/zombie cross-check, which this story expects to clear on the live-coordination and low-theater evidence.
narrative_ontology:disappearance_verdict(qwerty_persistence_mechanism__naturalization_reading, world_rearranges).
narrative_ontology:founding_problem_status(qwerty_persistence_mechanism__naturalization_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(qwerty_persistence_mechanism__naturalization_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(qwerty_persistence_mechanism__naturalization_reading, 'none', 1).
narrative_ontology:epsilon_provenance(qwerty_persistence_mechanism__naturalization_reading, 0.12, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(qwerty_persistence_mechanism__naturalization_reading_tests).
:- end_tests(qwerty_persistence_mechanism__naturalization_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is 0.12 because the only candidate cost under this reading is foregone output should a rival layout be genuinely superior — a premise the reading holds empirically unproven (the influential pro-rival studies of the 1930s-40s were conducted under the rival inventor's own supervision, a conflict of interest the correction literature documented). Retraining hours are recouped human capital, not rent. Suppression is 0.08 because no enforcement machinery exists: exit is legally and technically free (software remapping costs nothing, alternative boards are purchasable), and the residual friction is the ordinary price any shared standard charges, which this reading declines to count as coercion. Theater is 0.10 because no compliance rituals exist; the arrangement runs on habit and infrastructure, with a small bump around the Dvorak-advocacy era (T=75) when efficiency experts performed layout conversions for publicity, decaying afterward. Accessibility_collapse is 0.30: alternatives remain fully known and available — they collapsed commercially, not epistemically, and this reading attributes the commercial lapse to merit. Resistance is 0.18: episodic advocacy waves and brief corporate experiments, with no sustained opposition because no identifiable class is harmed. The temporal series are intentionally near-flat on one shared grid (points 0, 25, 50, 75, 100, 125, 150; every tracked metric authored at every point): this reading's content IS stability, and a rising-extraction trajectory would contradict its own premise. No suppression_requirement series is authored because the enforcement picture is static at effectively zero — there is no enforcement capacity whose build-up or erosion could be traced; the scalar 0.08 captures the whole picture. Suppression is authored as a raw structural property and is not scaled by context; extractiveness alone is scaled by the engine through directionality and scope, and from a 0.12 base the global-scope amplification remains negligible.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently from the same arrangement. From the touch_typists_collectively seat the standard is invisible infrastructure: skill acquired once works everywhere, and no cost is experienced as imposed. From the dvorak_colemak_advocates seat the same arrangement presents as a closed door — their alternative cannot get procurement traction — though under this reading the door is closed by demonstrated merit, not by enforcement, which is precisely what the lapse_fairness_question omega tests. From the keyboard_standards_bodies seat the arrangement is a codification duty: they publish standards that trail practice and hold no lever over the installed base. From the technology_historians_economists seat the striking fact is origin-function decoupling: the design rationale (type-bar collision management) is unrelated to the present justification (skill portability). Same-level dynamics: touch_typists_collectively and dvorak_colemak_advocates hold the same nominal power atom (moderate) but are differentiated by exit history — the switchers among typists actually attempted exit and returned on the merits, while advocates never entered the equilibrium at all. The engine computes per-seat classifications from this structural data; under the authored declarations every participant seat derives near-beneficiary directionality, and the sharpest divergence is between participants and the excluded advocate seat.
 *
 * DIRECTIONALITY LOGIC:
 *   Every declared beneficiary derives low directionality and therefore low or negative effective extraction: typists hold the arrangement's value as portable skill, employers hire against it at no premium, and manufacturers build to it at competitive margins. No victims are declared — deliberately, because the absence of a victim class is this reading's distinguishing structural signature against both siblings. The one seat a rival reading would recast (keyboard_device_manufacturers as active maintainer) is, under this reading, a plain beneficiary with no administrative lever: they follow demand rather than shape it, so the derivation chain from beneficiary declaration plus mobile exit plus institutional power already yields the correct near-beneficiary directionality and no override is needed. The excluded advocate seat sits outside the beneficiary/victim derivation entirely, which is structurally correct: exclusion-by-merit is not extraction from them. Global spatial scope modestly amplifies effective extraction in the engine's arithmetic, but from this base the amplification is immaterial.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding mandate — managing type-bar collisions and operator pacing in mechanical typewriters — is dead: it expired with the machines that posed it, and historians corroborate both the expiry and the partial mythologizing of the origin story. Yet the arrangement is not vestigial, because a successor function fully occupies the structure: skill portability across every device, employer, and country, reproduced continuously by every new typist who trains on the shared layout. This is why the story anticipates the R5 mismatch probe (founding_problem_status dead x disappearance_verdict world_rearranges) and expects it to clear on cross-check: theater is 0.10, no agenda-setter bears a fixable harm (the standards bodies hold no lever, and the benefit of fixing is approximately zero or negative under the adequacy premise), and the coordination function is live rather than performed. The classification guards against the sibling error in both directions: reading genuine skill-formation cost as extraction would fabricate a victim class and misclassify a benign standard as coercive, while complacent acceptance without the falsifiable omegas would miss the case where a replicated rival advantage converts foregone surplus into real, widely borne cost. The mandatrophy declaration therefore lives in the genealogy interview (status: dead, corroborated externally), not in any tuned metric.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_commitment,
    'This story instantiates the naturalization_reading of kernel qwerty_persistence_mechanism; which of the three rival readings matches the underlying arrangement''s actual structure?',
    'Comparative evaluation across the three sibling stories against a shared empirical battery: switching-cost accounting, Dvorak-advantage meta-analysis, and archival manufacturer-conduct records. Whichever reading''s declared victim and enforcement structure survives the battery is retained; the others are retired as misreadings.',
    'If a sibling wins, this story''s epsilon of 0.12 and empty victim set are wrong: the lock_in_reading would add trapped-typist victims and raise epsilon substantially; the beneficiary_extraction_reading would name keyboard_device_manufacturers as the capturer seat with concentrated gain_flow.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_commitment, conceptual, 'Committer structure: this is one of three rival readings of the QWERTY-persistence kernel; the contest is routed here, not folded into the classification.').

omega_variable(
    dvorak_advantage_empirics,
    'Does any alternative layout confer a real, decision-relevant advantage in speed, accuracy, or comfort over QWERTY for ordinary professional typing?',
    'Preregistered controlled trials with motivated, compensated subjects; longitudinal repetitive-strain and error-rate data; replications independent of layout-patent holders, weighting heavily the documented conflict of interest in the 1930s-40s studies that Dvorak himself ran or supervised.',
    'A large replicated advantage collapses this reading''s adequacy premise: foregone output becomes a real cost borne by every typist, epsilon rises sharply, and the story recomputes toward a hybrid coordination-plus-foregone-surplus structure. A null or small result secures the low-extraction reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(dvorak_advantage_empirics, empirical, 'The load-bearing empirical question for the adequacy claim: is the rival layout actually better?').

omega_variable(
    lapse_fairness_question,
    'Did alternative layouts lapse through fair competition, or did incumbent-side frictions such as procurement defaults, training-pipeline inertia, and bundled licensing tilt the field against them?',
    'Archival business history of typewriter and computer procurement decisions, manufacturer conduct records, and counterfactual adoption modeling under neutral-default conditions.',
    'Documented tilting transfers explanatory weight to the beneficiary_extraction_reading and would add suppressed-alternative victims to this story''s structural declarations.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(lapse_fairness_question, empirical, 'Whether the competitive process that eliminated alternatives was fair, as this reading asserts.').

omega_variable(
    switching_cost_classification,
    'Are observed switching costs genuine, portable skill investment, or artificially inflated coordination barriers that function as imposed costs?',
    'Longitudinal tracking of individual retrainers'' productivity and wage trajectories after moving to an alternative layout, benchmarked against learning curves for comparable tool transitions.',
    'If costs prove non-portable or inflated by incumbent design choices, the genuine-skill-investment premise fails, the cost structure reads as imposed rather than invested, and effective extraction rises for every seated agent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(switching_cost_classification, empirical, 'Nature of the switching costs this reading classifies as legitimate human-capital formation.').

omega_variable(
    adequacy_threshold_framing,
    'Is genuinely-adequate the correct classification bar, or should the arrangement be judged against the best achievable layout, under which even a fair-competition winner registers foregone surplus as a cost?',
    'Normative framing choice in the classification literature between satisficing (adequacy) and optimality standards for coordination goods; no empirical datum alone settles it.',
    'Under an optimality frame the story drifts toward a hybrid coordination-plus-extraction classification even with fair competition established; under the satisficing frame it stands as pure coordination.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(adequacy_threshold_framing, conceptual, 'Conceptual ambiguity in the adequacy standard that separates this reading from a foregone-surplus account.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(qwerty_persistence_mechanism__naturalization_reading, 0, 150).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qwer_tr_t0, qwerty_persistence_mechanism__naturalization_reading, theater_ratio, 0, 0.04).
narrative_ontology:measurement_basis(qwer_tr_t0, observed).
narrative_ontology:measurement(qwer_tr_t25, qwerty_persistence_mechanism__naturalization_reading, theater_ratio, 25, 0.05).
narrative_ontology:measurement_basis(qwer_tr_t25, observed).
narrative_ontology:measurement(qwer_tr_t50, qwerty_persistence_mechanism__naturalization_reading, theater_ratio, 50, 0.06).
narrative_ontology:measurement_basis(qwer_tr_t50, observed).
narrative_ontology:measurement(qwer_tr_t75, qwerty_persistence_mechanism__naturalization_reading, theater_ratio, 75, 0.08).
narrative_ontology:measurement_basis(qwer_tr_t75, observed).
narrative_ontology:measurement(qwer_tr_t100, qwerty_persistence_mechanism__naturalization_reading, theater_ratio, 100, 0.07).
narrative_ontology:measurement_basis(qwer_tr_t100, observed).
narrative_ontology:measurement(qwer_tr_t125, qwerty_persistence_mechanism__naturalization_reading, theater_ratio, 125, 0.09).
narrative_ontology:measurement_basis(qwer_tr_t125, observed).
narrative_ontology:measurement(qwer_tr_t150, qwerty_persistence_mechanism__naturalization_reading, theater_ratio, 150, 0.1).
narrative_ontology:measurement_basis(qwer_tr_t150, observed).

% Extraction over time
narrative_ontology:measurement(qwer_be_t0, qwerty_persistence_mechanism__naturalization_reading, base_extractiveness, 0, 0.06).
narrative_ontology:measurement_basis(qwer_be_t0, observed).
narrative_ontology:measurement(qwer_be_t25, qwerty_persistence_mechanism__naturalization_reading, base_extractiveness, 25, 0.07).
narrative_ontology:measurement_basis(qwer_be_t25, observed).
narrative_ontology:measurement(qwer_be_t50, qwerty_persistence_mechanism__naturalization_reading, base_extractiveness, 50, 0.08).
narrative_ontology:measurement_basis(qwer_be_t50, observed).
narrative_ontology:measurement(qwer_be_t75, qwerty_persistence_mechanism__naturalization_reading, base_extractiveness, 75, 0.09).
narrative_ontology:measurement_basis(qwer_be_t75, observed).
narrative_ontology:measurement(qwer_be_t100, qwerty_persistence_mechanism__naturalization_reading, base_extractiveness, 100, 0.1).
narrative_ontology:measurement_basis(qwer_be_t100, observed).
narrative_ontology:measurement(qwer_be_t125, qwerty_persistence_mechanism__naturalization_reading, base_extractiveness, 125, 0.11).
narrative_ontology:measurement_basis(qwer_be_t125, observed).
narrative_ontology:measurement(qwer_be_t150, qwerty_persistence_mechanism__naturalization_reading, base_extractiveness, 150, 0.12).
narrative_ontology:measurement_basis(qwer_be_t150, observed).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(qwerty_persistence_mechanism__naturalization_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(qwerty_persistence_mechanism__naturalization_reading, information_standard).
narrative_ontology:affects_constraint(qwerty_persistence_mechanism__naturalization_reading, lock_in_reading).
narrative_ontology:affects_constraint(qwerty_persistence_mechanism__naturalization_reading, beneficiary_extraction_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial question 'why does QWERTY persist?' decomposes, per the epsilon-invariance principle, into three structurally distinct constraint stories sharing the kernel qwerty_persistence_mechanism. This file (naturalization_reading) authors epsilon at 0.12 with no victims and no enforcement; lock_in_reading would author substantially higher epsilon with trapped-typist victims and coordination-failure persistence; beneficiary_extraction_reading would author high epsilon with manufacturers as capturer and active suppression of alternatives. The upstream member is the empirical adequacy record (the post-1990 correction literature auditing the original rival-layout studies), which feeds this reading's claims; the sibling readings cite the pre-correction record upstream of themselves. Every family member links the others through affects_constraints; orphaning any member would sever the contamination-propagation analysis across the kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
