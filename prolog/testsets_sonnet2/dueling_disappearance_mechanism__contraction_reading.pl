% ============================================================================
% CONSTRAINT STORY: dueling_disappearance_mechanism__contraction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_dueling_disappearance_mechanism__contraction_reading, []).

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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: dueling_disappearance_mechanism__contraction_reading
 *   human_readable: Dignity-Culture Substrate Displacing Honor-Culture Axioms (Contraction Reading)
 *   domain: historical sociology / cultural anthropology / legal history
 *
 * SUMMARY:
 *   This story instantiates the 'contraction reading' of the
 *   dueling-disappearance kernel: dueling did not lose to competing
 *   institutions or get suppressed by statute so much as the underlying moral
 *   vocabulary (honor-culture axioms about reputation, courage, and personal
 *   adjudication of insult) contracted and was displaced by an incompatible
 *   dignity-culture vocabulary (equal inherent worth, due process,
 *   self-restraint). On this reading the mechanism is axiomatic substrate
 *   shift, not competitive institutional displacement (a separate sibling
 *   constraint, institutional_displacement_reading) and not multi-causal
 *   overdetermination (overdetermined_composite_reading, also a separate
 *   sibling constraint). Because the mechanism is a shift in what is
 *   culturally thinkable rather than an enforced prohibition, this reading's
 *   claimed type is mountain: once the dignity-culture substrate takes hold,
 *   dueling becomes literally unthinkable rather than merely illegal or
 *   inconvenient, and no party need actively suppress it going forward.
 *
 * KEY AGENTS:
 *   - honor_culture_practitioners: primary victims (moderate/trapped) — bear the cost of framework illegibility, cannot exit because the ground itself moved
 *   - displaced_aristocratic_gentry: secondary victims (moderate/trapped) — lose the status-reproduction apparatus honor rituals provided
 *   - dignity_culture_adherents: primary beneficiaries (organized/mobile) — their vocabulary becomes ambient common sense without needing to win an argument
 *   - bourgeois_professional_class: secondary beneficiaries (organized/mobile) — their credential-based status logic is vindicated by the substrate shift
 *   - cultural_historians: analytical observer — study the axiom shift as the causal engine
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dueling_disappearance_mechanism__contraction_reading, 0.18).
domain_priors:suppression_score(dueling_disappearance_mechanism__contraction_reading, 0.28).
domain_priors:theater_ratio(dueling_disappearance_mechanism__contraction_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dueling_disappearance_mechanism__contraction_reading, extractiveness, 0.18).
narrative_ontology:constraint_metric(dueling_disappearance_mechanism__contraction_reading, suppression_requirement, 0.28).
narrative_ontology:constraint_metric(dueling_disappearance_mechanism__contraction_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dueling_disappearance_mechanism__contraction_reading, accessibility_collapse, 0.88).
narrative_ontology:constraint_metric(dueling_disappearance_mechanism__contraction_reading, resistance, 0.12).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dueling_disappearance_mechanism__contraction_reading, mountain).
narrative_ontology:human_readable(dueling_disappearance_mechanism__contraction_reading, "Dignity-Culture Substrate Displacing Honor-Culture Axioms (Contraction Reading)").
narrative_ontology:topic_domain(dueling_disappearance_mechanism__contraction_reading, "historical sociology / cultural anthropology / legal history").

domain_priors:emerges_naturally(dueling_disappearance_mechanism__contraction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dueling_disappearance_mechanism__contraction_reading, '1cb6553f-f0e2-4931-a04e-5dd06c91e94e').
narrative_ontology:cs_kernel_codification('1cb6553f-f0e2-4931-a04e-5dd06c91e94e', distributed).
narrative_ontology:cs_authority_grounding('1cb6553f-f0e2-4931-a04e-5dd06c91e94e', practice).
narrative_ontology:cs_interpretation_layer_present('1cb6553f-f0e2-4931-a04e-5dd06c91e94e').
narrative_ontology:cs_reading_relation('1cb6553f-f0e2-4931-a04e-5dd06c91e94e', dueling_disappearance_mechanism__institutional_displacement_reading, coexists_with).
narrative_ontology:cs_reading_relation('1cb6553f-f0e2-4931-a04e-5dd06c91e94e', dueling_disappearance_mechanism__overdetermined_composite_reading, influences).
narrative_ontology:cs_axiom('1cb6553f-f0e2-4931-a04e-5dd06c91e94e', foundational, moral_vocabulary_is_the_causal_engine).
narrative_ontology:cs_axiom_status(moral_vocabulary_is_the_causal_engine, holdable).
narrative_ontology:cs_axiom_grounding('1cb6553f-f0e2-4931-a04e-5dd06c91e94e', moral_vocabulary_is_the_causal_engine, conventional).
narrative_ontology:cs_axiom('1cb6553f-f0e2-4931-a04e-5dd06c91e94e', secondary, axiom_displacement_requires_no_active_enforcement).
narrative_ontology:cs_axiom_status(axiom_displacement_requires_no_active_enforcement, holdable).
narrative_ontology:cs_axiom_grounding('1cb6553f-f0e2-4931-a04e-5dd06c91e94e', axiom_displacement_requires_no_active_enforcement, empirically_contingent).
narrative_ontology:cs_reference_frame('1cb6553f-f0e2-4931-a04e-5dd06c91e94e', honor_culture_reciprocal_recognition_order).
narrative_ontology:cs_drift_state('1cb6553f-f0e2-4931-a04e-5dd06c91e94e', post_bellum_dignity_consolidation, gap(axiom_overriding, severe, false)).
narrative_ontology:cs_created_at('1cb6553f-f0e2-4931-a04e-5dd06c91e94e', '').
narrative_ontology:cs_kernel_id(dueling_disappearance_mechanism__contraction_reading, dueling_disappearance_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dueling_disappearance_mechanism__contraction_reading, dignity_culture_adherents).
narrative_ontology:constraint_beneficiary(dueling_disappearance_mechanism__contraction_reading, bourgeois_professional_class).
narrative_ontology:constraint_victim(dueling_disappearance_mechanism__contraction_reading, honor_culture_practitioners).
narrative_ontology:constraint_victim(dueling_disappearance_mechanism__contraction_reading, displaced_aristocratic_gentry).
narrative_ontology:constraint_vindicates(dueling_disappearance_mechanism__contraction_reading, universal_equal_dignity_doctrine).
narrative_ontology:constraint_vindicates(dueling_disappearance_mechanism__contraction_reading, moral_progress_narrative).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Gentlemen whose entire framework for adjudicating insult, reputation, and standing depended on the possibility of the duel as a live recourse. As dignity-culture axioms displaced the shared moral vocabulary that made honor claims intelligible, their grievances stopped registering as claims at all — not merely losing the fight over dueling, but losing the language in which a duel could even be proposed as an answer. Cannot 'exit' because the surrounding culture, not a specific rule, moved out from under them.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__contraction_reading, honor_culture_practitioners, payer,
    moderate, generational, trapped, national).

% The social stratum whose status order was legible through honor rituals lost the ritual apparatus that reproduced their distinction from the emerging middle class. Their standing depended on a system of meaning that became, from outside it, simply unavailable — not banned so much as unthinkable.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__contraction_reading, displaced_aristocratic_gentry, payer,
    moderate, generational, trapped, national).

% The rising social formation — professionals, clergy, reformers, an expanding middle class — whose moral vocabulary (equal inherent worth, due process, self-restraint as virtue) became the ambient common sense. They did not need to defeat dueling in argument; the vocabulary in which dueling made sense simply stopped being spoken.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__contraction_reading, dignity_culture_adherents, beneficiary,
    organized, civilizational, mobile, national).

% Lawyers, merchants, and civic officials whose authority depended on institutionalized, textual, credential-based standing rather than personal courage-under-fire. The dignity-culture substrate validated their basis of status while delegitimizing the honor-culture basis that had excluded or subordinated them.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__contraction_reading, bourgeois_professional_class, beneficiary,
    organized, generational, mobile, national).

% Study the shift in moral vocabulary itself as the causal engine — examining diaries, sermons, conduct manuals, and periodical rhetoric for evidence that the axiomatic ground moved rather than that dueling was merely out-competed or outlawed.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__contraction_reading, cultural_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(dueling_disappearance_mechanism__contraction_reading, diffuse).
narrative_ontology:fixing_cost_class(dueling_disappearance_mechanism__contraction_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Honor-culture dueling coordinated status disputes among social equals who lacked (or refused) recourse to state adjudication — it settled contested claims about courage and reputation within a shared, mutually legible code. This reading holds that coordination function did not lose a competition; the axioms that made it legible dissolved.
% TRANSFER_FUNCTION: Nothing is actively transferred in the contraction reading's mechanism — no coercive extraction machinery is exercised against honor-culture adherents. What is 'moved' is intelligibility itself: standing, deference, and moral legitimacy migrate from honor-code holders to dignity-code holders as the substrate underneath both groups shifts, leaving one framework unable to make claims the other can hear.
% ABSENT_VOICES: Honor-culture practitioners themselves, in their own terms, are structurally absent from the historical and legal record produced after the transition — their framework is preserved (if at all) as antiquarian curiosity or moral cautionary tale, not as a live claim requiring an answer. No dignity-culture forum exists in which an honor claim could be adjudicated as an honor claim.
% DISAPPEARANCE_RATIONALE: Under the contraction reading, the specific formal prohibitions and institutional decisions are treated as downstream signals rather than the causal engine; if the legal apparatus banning dueling vanished overnight, nothing would rearrange, because the underlying moral vocabulary that made dueling legible has already withdrawn. The world already reorganized around dignity axioms decades before the last formal remnants were repealed or fell into disuse; removing surface law changes nothing because the substrate constraint is what is doing the work.
% FOUNDING_PROBLEM: Honor-culture dueling was never the problem being solved by this constraint; the problem this reading identifies is retrospective — explaining why a once-thinkable practice became unthinkable without any single external actor abolishing it by force. The 'arrangement' here is the dignity-culture substrate itself, which was not built to solve the dueling problem but simply displaced the axioms that gave dueling its sense.
% FOUNDING_PROBLEM_CORROBORATION: Social historians outside any dignity-culture advocacy tradition (e.g., comparative anthropologists of honor societies, and scholars sympathetic to reconstructing honor-culture logic on its own terms, such as historians of the antebellum South and dueling codes) attest that the honor-culture framework was not defeated by argument or institution but became structurally illegible to observers already standing inside dignity-culture assumptions — corroboration exists, but it is thin: most surviving testimony to the loss comes from partisans of the older order writing valedictories, not from a neutral third party untouched by either framework.
narrative_ontology:disappearance_verdict(dueling_disappearance_mechanism__contraction_reading, world_unchanged).
narrative_ontology:founding_problem_status(dueling_disappearance_mechanism__contraction_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dueling_disappearance_mechanism__contraction_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(dueling_disappearance_mechanism__contraction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(dueling_disappearance_mechanism__contraction_reading, 0.18, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(dueling_disappearance_mechanism__contraction_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(dueling_disappearance_mechanism__contraction_reading, ExtMetricName, E),
    domain_priors:suppression_score(dueling_disappearance_mechanism__contraction_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(dueling_disappearance_mechanism__contraction_reading),
    narrative_ontology:constraint_metric(dueling_disappearance_mechanism__contraction_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(dueling_disappearance_mechanism__contraction_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(dueling_disappearance_mechanism__contraction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is authored low (0.18 at 1900) and rising only slightly, because the contraction reading holds that no active machinery of extraction is required — nothing is taken from honor-culture practitioners by force; their framework simply stops being spoken by anyone with cultural authority. Suppression is authored moderate (0.28) rather than negligible, because the reading does still acknowledge some social sanctioning (satire, moral condemnation, exclusion from polite society) operating alongside the axiomatic shift, but this is secondary to the substrate mechanism, not its engine. Accessibility collapse is authored very high (0.88): once dignity-culture axioms are ambient, there is no live alternative vocabulary in which an honor claim registers as a claim — the collapse is close to total and largely irreversible within the historical window. Resistance is authored low (0.12): honor-culture adherents mounted rhetorical and some violent resistance (the persistence of dueling in isolated pockets, especially the antebellum South, into the 1850s-60s) but this resistance is characterized, on this reading, as a lagging residue rather than an active contest capable of reversing the shift.
 *
 * PERSPECTIVAL GAP:
 *   From the honor-culture practitioner's seat, the loss is total and largely invisible in its own mechanism — the practitioner does not experience 'losing an argument' about dueling, but experiences their own claims failing to land as claims at all in an unfamiliar moral idiom. From the dignity-culture adherent's seat, there was no coercion or victory to notice; the norm simply always seemed self-evidently correct, because that adherent stands entirely inside the substrate that displaced the older one. The engine should compute these seats very differently: the payer seats experience something close to epistemic erasure, while the beneficiary seats experience nothing at all requiring justification — which is exactly the signature the contraction reading is claiming as its structural fact, distinguishing it from a rope story about coordination or a tangled_rope/snare story about active suppression.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (dignity_culture_adherents, bourgeois_professional_class) are declared with mobile exit and organized power because the substrate shift is precisely what makes their standing frictionless — they do not need to actively defend the new order, it simply is the order. Victims (honor_culture_practitioners, displaced_aristocratic_gentry) are declared trapped because the exit that would matter — returning to a cultural moment where honor-code claims were legible — is not available; there is no market alternative or geographic relocation that restores intelligibility once the substrate has moved. This differs sharply from a directionality profile driven by active coercion (which would look more like a snare), which is why the contraction reading claims mountain rather than tangled_rope or snare despite naming victims — the schema's FSM-adjacent tension (a 'mountain' with declared victims/beneficiaries) is itself the analytical point: is dignity culture a genuine irreversible substrate, or a constructed hegemony that happens to benefit identifiable groups? That tension is intentionally left open via omega, not resolved by tuning the metrics.
 *
 * MANDATROPHY ANALYSIS:
 *   The contraction reading resists mandatrophy mislabeling in a specific way: because the claimed type is mountain (not rope), there is no live 'mandate' whose obsolescence needs to be tracked in the ordinary sense — the founding_problem answer reflects this by naming the founding problem as effectively retrospective and already dead. This prevents the analysis from treating dignity culture as an institution that once solved a coordination problem and has now outlived it (a scaffold/piton story); instead it treats the shift as a substrate-level event with no active administrator to hold accountable for sunsetting it, which is structurally distinct from the institutional_displacement_reading's implicit claim that specific institutions (courts, banks, libel law) are administrable substitutes that could in principle be rolled back.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    mountain_or_constructed_hegemony,
    'Is the dignity-culture substrate a genuine, irreversible cultural-evolutionary fact (a mountain, in the sense of being beyond any single party''s power to reverse), or is it a constructed ideological hegemony that happens to benefit the rising bourgeois-professional class and can be named as such — making this a false summit (FSM candidate) rather than a true mountain?',
    'Comparative historical-sociological analysis of societies where dignity-culture norms did NOT displace honor-culture norms (e.g., contemporary honor-culture-persistent regions) to test whether the shift required specific institutional/economic preconditions (industrialization, professional credentialing, print literacy) that would make it a constructed rather than natural process; if the shift is contingent on identifiable enabling conditions that a coalition actively promoted, it looks more like tangled_rope with FSM dynamics than a genuine mountain.',
    'If resolved toward ''constructed hegemony,'' the claimed_type should shift toward tangled_rope or snare, and the beneficiary declarations here (dignity_culture_adherents, bourgeois_professional_class) would represent active extraction rather than incidental substrate benefit. If resolved toward ''genuine substrate shift,'' the mountain claim is vindicated and the beneficiary declarations are better read as passive windfall, not extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mountain_or_constructed_hegemony, conceptual, 'Whether dignity-culture displacement is a natural mountain or a false-summit hegemony benefiting a specific class.').

omega_variable(
    kernel_reading_location_of_disagreement,
    'Where exactly do the three kernel readings (contraction, institutional_displacement, overdetermined_composite) disagree — is it about which causal factor was NECESSARY, which was SUFFICIENT, or merely which factor to foreground for explanatory purposes, given that a historian could accept all three empirical claims simultaneously?',
    'Careful reconstruction of each reading''s claimed counterfactual: contraction_reading implies dueling would have disappeared even absent specific legal prohibitions or institutional substitutes (given enough time for the axiom shift alone), while institutional_displacement_reading implies the axiom shift alone would have been insufficient without courts/banking/libel-law substitutes. The overdetermined_composite_reading denies any single reading''s counterfactual claim is testable in isolation.',
    'If contraction_reading''s strong counterfactual claim (axiom shift alone sufficient) cannot be sustained, this reading''s mountain classification is undermined and it collapses toward being one contributing factor within the composite reading rather than a standalone irreversible constraint.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_location_of_disagreement, conceptual, 'Locating the precise structural disagreement between sibling kernel readings.').

omega_variable(
    honor_culture_illegibility_measurement,
    'Can ''illegibility'' of honor-culture claims be measured independently of the historical record produced by the dignity-culture victors, given that surviving sources are disproportionately dignity-culture-authored?',
    'Search for honor-culture self-testimony (private correspondence, unpublished memoirs, regional folk practice records) from communities where dueling persisted longest (e.g., the antebellum and immediate postbellum American South) to assess whether practitioners experienced the shift as illegibility or as active suppression they could name and resist.',
    'If practitioners could and did name the suppression mechanism clearly (rather than experiencing pure illegibility), the victim experience is closer to the institutional_displacement_reading''s suppression-by-substitute-institution story than to this reading''s substrate-contraction story, weakening the mountain claim''s evidentiary basis.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(honor_culture_illegibility_measurement, empirical, 'Whether honor-culture practitioners experienced illegibility or nameable suppression.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dueling_disappearance_mechanism__contraction_reading, 1750, 1900).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(duel_tr_t1750, dueling_disappearance_mechanism__contraction_reading, theater_ratio, 1750, 0.04).
narrative_ontology:measurement(duel_tr_t1780, dueling_disappearance_mechanism__contraction_reading, theater_ratio, 1780, 0.05).
narrative_ontology:measurement(duel_tr_t1810, dueling_disappearance_mechanism__contraction_reading, theater_ratio, 1810, 0.06).
narrative_ontology:measurement(duel_tr_t1840, dueling_disappearance_mechanism__contraction_reading, theater_ratio, 1840, 0.08).
narrative_ontology:measurement(duel_tr_t1870, dueling_disappearance_mechanism__contraction_reading, theater_ratio, 1870, 0.09).
narrative_ontology:measurement(duel_tr_t1900, dueling_disappearance_mechanism__contraction_reading, theater_ratio, 1900, 0.1).

% Extraction over time
narrative_ontology:measurement(duel_be_t1750, dueling_disappearance_mechanism__contraction_reading, base_extractiveness, 1750, 0.08).
narrative_ontology:measurement(duel_be_t1780, dueling_disappearance_mechanism__contraction_reading, base_extractiveness, 1780, 0.1).
narrative_ontology:measurement(duel_be_t1810, dueling_disappearance_mechanism__contraction_reading, base_extractiveness, 1810, 0.13).
narrative_ontology:measurement(duel_be_t1840, dueling_disappearance_mechanism__contraction_reading, base_extractiveness, 1840, 0.16).
narrative_ontology:measurement(duel_be_t1870, dueling_disappearance_mechanism__contraction_reading, base_extractiveness, 1870, 0.17).
narrative_ontology:measurement(duel_be_t1900, dueling_disappearance_mechanism__contraction_reading, base_extractiveness, 1900, 0.18).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(dueling_disappearance_mechanism__contraction_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dueling_disappearance_mechanism__contraction_reading, identity_coordination).
narrative_ontology:affects_constraint(dueling_disappearance_mechanism__contraction_reading, institutional_displacement_reading).
narrative_ontology:affects_constraint(dueling_disappearance_mechanism__contraction_reading, overdetermined_composite_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the dueling_disappearance_mechanism kernel. contraction_reading (this file) claims the mechanism is an axiomatic substrate shift and types as mountain. institutional_displacement_reading claims the mechanism is competitive institutional substitution (courts, banking, libel law out-competing dueling) and is expected to type closer to rope/tangled_rope, since specific administrable institutions can be named as agenda-setters. overdetermined_composite_reading declines to name a dominant mechanism, holding that legal prohibition, institutional modernization, cultural shift, and Civil War trauma were jointly sufficient and independently operative; its classification is expected to sit ambiguously across types depending on which factor's structural data dominates. All three share the same underlying historical phenomenon (dueling's disappearance) but author different epsilon values, different beneficiary/victim structures, and different claimed types because they identify different causal mechanisms as primary. Per the ε-invariance principle, these are three distinct constraints, not one constraint measured three ways.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
