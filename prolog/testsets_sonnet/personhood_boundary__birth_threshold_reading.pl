% ============================================================================
% CONSTRAINT STORY: personhood_boundary__birth_threshold_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_personhood_boundary__birth_threshold_reading, []).

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
 *   constraint_id: personhood_boundary__birth_threshold_reading
 *   human_readable: Birth Threshold Reading of the Personhood Boundary
 *   domain: moral_philosophy/historical_ethics
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the contested personhood_boundary
 *   kernel: the birth threshold reading, under which live birth alone confers
 *   full moral standing and legal personhood on all born humans, with no
 *   further test of fitness, capacity, or potential. Two sibling readings of
 *   the same kernel — fitness_contingent_reading and potential_based_reading
 *   — are separate constraint stories with their own ε values,
 *   beneficiary/victim structures, and classifications; they are not folded
 *   into this one. This reading is claimed as a mountain: in jurisdictions
 *   where it is settled doctrine, it presents as a near-bright-line,
 *   minimally contested rule that homicide law applies to born humans without
 *   exception, requiring little active defense once adopted. The metrics are
 *   authored low on extraction and moderate-low on suppression because the
 *   rule, once in place, does not extract rents from any party and imposes
 *   only the enforcement cost of applying homicide law uniformly; the modest
 *   suppression figure reflects the residual enforcement needed against the
 *   minority position that would carve out exceptions for severely disabled
 *   or non-standard infants.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(personhood_boundary__birth_threshold_reading, 0.08).
domain_priors:suppression_score(personhood_boundary__birth_threshold_reading, 0.22).
domain_priors:theater_ratio(personhood_boundary__birth_threshold_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(personhood_boundary__birth_threshold_reading, extractiveness, 0.08).
narrative_ontology:constraint_metric(personhood_boundary__birth_threshold_reading, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(personhood_boundary__birth_threshold_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(personhood_boundary__birth_threshold_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(personhood_boundary__birth_threshold_reading, resistance, 0.28).

% --- Constraint claim ---
narrative_ontology:constraint_claim(personhood_boundary__birth_threshold_reading, mountain).
narrative_ontology:human_readable(personhood_boundary__birth_threshold_reading, "Birth Threshold Reading of the Personhood Boundary").
narrative_ontology:topic_domain(personhood_boundary__birth_threshold_reading, "moral_philosophy/historical_ethics").

domain_priors:requires_active_enforcement(personhood_boundary__birth_threshold_reading).
domain_priors:emerges_naturally(personhood_boundary__birth_threshold_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(personhood_boundary__birth_threshold_reading, 'f0bf5c28-d8d0-4453-9a02-546f9191ed92').
narrative_ontology:cs_kernel_codification('f0bf5c28-d8d0-4453-9a02-546f9191ed92', distributed).
narrative_ontology:cs_authority_grounding('f0bf5c28-d8d0-4453-9a02-546f9191ed92', practice).
narrative_ontology:cs_interpretation_layer_present('f0bf5c28-d8d0-4453-9a02-546f9191ed92').
narrative_ontology:cs_reading_relation('f0bf5c28-d8d0-4453-9a02-546f9191ed92', personhood_boundary__fitness_contingent_reading, forecloses).
narrative_ontology:cs_reading_relation('f0bf5c28-d8d0-4453-9a02-546f9191ed92', personhood_boundary__potential_based_reading, coexists_with).
narrative_ontology:cs_axiom('f0bf5c28-d8d0-4453-9a02-546f9191ed92', foundational, birth_event_sufficient_for_standing).
narrative_ontology:cs_axiom_status(birth_event_sufficient_for_standing, holdable).
narrative_ontology:cs_axiom_grounding('f0bf5c28-d8d0-4453-9a02-546f9191ed92', birth_event_sufficient_for_standing, deontological).
narrative_ontology:cs_axiom('f0bf5c28-d8d0-4453-9a02-546f9191ed92', secondary, capacity_irrelevant_to_born_human_standing).
narrative_ontology:cs_axiom_status(capacity_irrelevant_to_born_human_standing, holdable).
narrative_ontology:cs_axiom_grounding('f0bf5c28-d8d0-4453-9a02-546f9191ed92', capacity_irrelevant_to_born_human_standing, deontological).
narrative_ontology:cs_reference_frame('f0bf5c28-d8d0-4453-9a02-546f9191ed92', universal_born_human_standing).
narrative_ontology:cs_drift_state('f0bf5c28-d8d0-4453-9a02-546f9191ed92', contemporary_neonatal_bioethics_era, gap(repudiation_pressure, minor, true)).
narrative_ontology:cs_created_at('f0bf5c28-d8d0-4453-9a02-546f9191ed92', '').
narrative_ontology:cs_kernel_id(personhood_boundary__birth_threshold_reading, personhood_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(personhood_boundary__birth_threshold_reading, born_infants).
narrative_ontology:constraint_beneficiary(personhood_boundary__birth_threshold_reading, disabled_born_persons).
narrative_ontology:constraint_beneficiary(personhood_boundary__birth_threshold_reading, state_homicide_law_regimes).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(personhood_boundary__birth_threshold_reading, parents_and_caregivers_seeking_selective_nontreatment).
narrative_ontology:constraint_vindicates(personhood_boundary__birth_threshold_reading, universal_born_moral_standing).
narrative_ontology:constraint_vindicates(personhood_boundary__birth_threshold_reading, homicide_applies_to_all_born_humans).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Cannot advocate for themselves in any way. Under this reading, the fact of live birth alone secures full moral standing and legal protection against killing, regardless of any capacity, fitness, or disability. They receive protection unconditionally and bear no cost from the arrangement.
narrative_ontology:constraint_stakeholder(personhood_boundary__birth_threshold_reading, born_infants, beneficiary,
    powerless, biographical, trapped, national).

% Under sibling readings (potential-based, fitness-contingent) their standing could be made conditional on demonstrated capacities they may never meet. Under the birth threshold reading, their moral standing is settled at birth and is not revisited based on prognosis, disability, or perceived quality of life.
narrative_ontology:constraint_stakeholder(personhood_boundary__birth_threshold_reading, disabled_born_persons, beneficiary,
    powerless, biographical, trapped, national).

% Administers and enforces the boundary by defining homicide law to apply uniformly to all born humans without a fitness or capacity test. Must investigate and prosecute killings of infants, including severely disabled infants, as it would any other homicide. Cannot exempt a class of born humans without abandoning the bright-line rule the reading depends on.
narrative_ontology:constraint_stakeholder(personhood_boundary__birth_threshold_reading, state_homicide_law_regimes, agenda_setter,
    institutional, generational, constrained, national).

% Facing severe neonatal diagnoses, some caregivers who would prefer discretion to withhold treatment or life based on prognosis are constrained by the reading's refusal to make standing conditional on prognosis. Their preference for a fitness- or potential-based exception is foreclosed by the bright line; they must operate within a legal system that treats all born infants identically regardless of prognosis.
narrative_ontology:constraint_stakeholder(personhood_boundary__birth_threshold_reading, parents_and_caregivers_seeking_selective_nontreatment, payer,
    moderate, biographical, constrained, national).

% Argue that demonstrated fitness, not the biological event of birth, should ground moral standing. Under the birth threshold reading's dominance in most legal systems, their position has no operative legal home for born humans — it survives only as academic or minority philosophical argument, not as enforceable doctrine.
narrative_ontology:constraint_stakeholder(personhood_boundary__birth_threshold_reading, advocates_of_fitness_contingent_reading, excluded,
    moderate, biographical, constrained, national).

% Study and debate the comparative merits of birth-threshold, fitness-contingent, and potential-based readings, tracing their historical instantiations (infanticide practices, disability rights movements, legal reform) without holding enforcement power themselves.
narrative_ontology:constraint_stakeholder(personhood_boundary__birth_threshold_reading, bioethics_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(personhood_boundary__birth_threshold_reading, diffuse).
narrative_ontology:fixing_cost_class(personhood_boundary__birth_threshold_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a single, administrable, non-discretionary line for who counts as a rights-bearing person, allowing legal and medical systems to apply uniform protection without requiring case-by-case capacity or prognosis assessments.
% TRANSFER_FUNCTION: Moves no resources between parties in the ordinary sense; what it moves is the burden of proof and the locus of discretion — placing the determination of moral standing outside the hands of any individual clinician, parent, or state actor and fixing it to a bright, observable biological event.
% ABSENT_VOICES: Advocates of the fitness-contingent and potential-based readings are present in academic bioethics but structurally excluded from legal doctrine in jurisdictions that have adopted the birth threshold as settled homicide law; their arguments surface mainly in edge cases (severe congenital anomaly, extreme prematurity) rather than as live legal doctrine.
% DISAPPEARANCE_RATIONALE: If the birth threshold reading disappeared as the operative legal standard, homicide law would need a new non-arbitrary line (fitness, potential, viability, or otherwise), reopening every case involving severely disabled or non-standard infants to individualized standing determinations — reintroducing exactly the discretionary, case-by-case adjudication the bright line exists to foreclose.
% FOUNDING_PROBLEM: Historically and cross-culturally, moral and legal status of newborns has been contested and revisable (infanticide practices tied to deformity, sex, or family circumstance in numerous societies); the birth threshold reading was built to foreclose discretionary, retroactive revocation of standing based on post-birth assessment of the infant's qualities.
% FOUNDING_PROBLEM_CORROBORATION: Disability rights organizations and historians of infanticide practice, external to any single legal or religious tradition benefiting from the rule, corroborate that discretionary post-birth standing determinations have historically tracked disability, sex, and economic convenience — supporting the claim that a non-discretionary bright line addresses a real and recurring problem. Advocates of the sibling readings dispute that birth is the philosophically correct place to draw the line, but do not dispute that the historical problem of discretionary infanticide was real.
narrative_ontology:disappearance_verdict(personhood_boundary__birth_threshold_reading, world_rearranges).
narrative_ontology:founding_problem_status(personhood_boundary__birth_threshold_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(personhood_boundary__birth_threshold_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(personhood_boundary__birth_threshold_reading, 'none', 1).
narrative_ontology:epsilon_provenance(personhood_boundary__birth_threshold_reading, 0.08, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(personhood_boundary__birth_threshold_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(personhood_boundary__birth_threshold_reading, ExtMetricName, E),
    domain_priors:suppression_score(personhood_boundary__birth_threshold_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(personhood_boundary__birth_threshold_reading),
    narrative_ontology:constraint_metric(personhood_boundary__birth_threshold_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(personhood_boundary__birth_threshold_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(personhood_boundary__birth_threshold_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is authored very low (0.08 at interval end) because the rule does not transfer resources or standing from any born human to another party — it universalizes protection rather than allocating it selectively. Suppression is modest (0.22) and rising slightly over the interval, reflecting the gradual hardening of homicide law's uniform application against pressure from advocates of fitness- or potential-based carve-outs in hard cases (severe congenital anomaly, extreme prematurity) rather than any extractive enforcement against a victim class. Accessibility collapse is authored fairly high (0.72): once a jurisdiction adopts the bright line, alternative case-by-case discretionary standing determinations become very difficult to reintroduce without dismantling the whole homicide-law framework for infants. Resistance is authored moderate-low (0.28): most resistance to this reading comes from a comparatively narrow set of bioethical and religious-conservative positions favoring alternative thresholds, not from an organized victim class.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries are all born humans, particularly born infants and disabled born persons, whose standing is not contingent on demonstrated fitness or potential — the derivation gives them low d (near full beneficiary) since the constraint subsidizes their protection unconditionally. No victims are declared for this reading: the birth threshold reading does not identify any class of born human excluded from standing, which is precisely its structural point relative to its siblings. Parents and caregivers seeking selective nontreatment based on prognosis are payers in a narrower, procedural sense — they bear the cost of a legal system that will not treat some newborns' deaths as excusable homicide based on disability or prognosis — but they are not victims of the rule in the beneficiary/victim sense; they are constrained rather than extracted from.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — discretionary, retroactive revocation of newborn standing tied to deformity, sex, or convenience — remains live in many historical and some contemporary contexts, which is why founding_problem_status is authored as contested rather than dead: some jurisdictions and communities still practice or debate selective nontreatment in ways the bright line was built to foreclose. This keeps the birth threshold reading from being mislabeled as pure inertial ritual (a piton) — it continues to do the coordination work of foreclosing a real, recurring discretionary harm, even where enforcement burden persists.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    birth_as_natural_vs_constructed_line,
    'Is the birth event a genuinely natural, non-arbitrary line for moral standing (a mountain), or is it a historically contingent, constructed convention that happens to benefit born infants and the legal regimes that administer homicide law by making adjudication tractable?',
    'Comparative historical and cross-cultural analysis of where societies have drawn the standing line (conception, viability, birth, demonstrated capacity, weaning) and whether birth has any privileged biological or metaphysical status beyond administrative convenience.',
    'If birth is shown to be primarily an administrative convenience rather than a principled moral threshold, the reading is better modeled as a constructed convention that benefits identifiable parties (born infants, homicide-law administrators) rather than a discovered natural fact — supporting reclassification pressure toward tangled_rope via the false-summit signature.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(birth_as_natural_vs_constructed_line, conceptual, 'Whether the birth threshold is a natural moral fact or a constructed administrative convention with beneficiaries.').

omega_variable(
    kernel_reading_disagreement_location,
    'Where exactly is the substantive disagreement between the birth_threshold, fitness_contingent, and potential_based readings located — is it a factual dispute about when relevant capacities arise, or an irreducible normative dispute about which property (biological event, demonstrated capacity, or potential) ought to ground standing?',
    'Philosophical analysis distinguishing empirical claims (when do capacities in fact develop) from normative claims (which developmental marker is morally load-bearing) within each reading''s foundational axioms.',
    'If the disagreement is purely normative and irreducible, the three readings are permanently coexisting within public discourse; if partly empirical, developments in neonatal and developmental science could shift which reading is empirically best supported without resolving the normative core.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_disagreement_location, conceptual, 'Whether the kernel''s sibling readings diverge on facts, values, or both.').

omega_variable(
    selective_nontreatment_prevalence,
    'How prevalent is de facto selective nontreatment of severely disabled newborns in jurisdictions that formally adopt the birth threshold reading, and does this practice constitute a quiet, unacknowledged accommodation of the potential-based or fitness-contingent readings within nominally birth-threshold legal systems?',
    'Empirical survey of neonatal intensive care withdrawal-of-treatment decisions and their legal treatment, compared against the formal doctrine.',
    'High prevalence of quiet accommodation would suggest the birth threshold reading''s accessibility_collapse and resistance figures are overstated at the level of actual practice, even where they hold at the level of formal law — indicating the reading is more contested in practice than its doctrinal dominance suggests.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(selective_nontreatment_prevalence, empirical, 'Whether formal adoption of the birth threshold reading is undermined by informal practice.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(personhood_boundary__birth_threshold_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pers_tr_t0, personhood_boundary__birth_threshold_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement(pers_tr_t12, personhood_boundary__birth_threshold_reading, theater_ratio, 12, 0.08).
narrative_ontology:measurement(pers_tr_t24, personhood_boundary__birth_threshold_reading, theater_ratio, 24, 0.09).
narrative_ontology:measurement(pers_tr_t36, personhood_boundary__birth_threshold_reading, theater_ratio, 36, 0.09).
narrative_ontology:measurement(pers_tr_t48, personhood_boundary__birth_threshold_reading, theater_ratio, 48, 0.1).
narrative_ontology:measurement(pers_tr_t60, personhood_boundary__birth_threshold_reading, theater_ratio, 60, 0.1).

% Extraction over time
narrative_ontology:measurement(pers_be_t0, personhood_boundary__birth_threshold_reading, base_extractiveness, 0, 0.06).
narrative_ontology:measurement(pers_be_t12, personhood_boundary__birth_threshold_reading, base_extractiveness, 12, 0.06).
narrative_ontology:measurement(pers_be_t24, personhood_boundary__birth_threshold_reading, base_extractiveness, 24, 0.07).
narrative_ontology:measurement(pers_be_t36, personhood_boundary__birth_threshold_reading, base_extractiveness, 36, 0.07).
narrative_ontology:measurement(pers_be_t48, personhood_boundary__birth_threshold_reading, base_extractiveness, 48, 0.08).
narrative_ontology:measurement(pers_be_t60, personhood_boundary__birth_threshold_reading, base_extractiveness, 60, 0.08).

% Suppression requirement over time
narrative_ontology:measurement(pers_su_t0, personhood_boundary__birth_threshold_reading, suppression_requirement, 0, 0.15).
narrative_ontology:measurement(pers_su_t12, personhood_boundary__birth_threshold_reading, suppression_requirement, 12, 0.17).
narrative_ontology:measurement(pers_su_t24, personhood_boundary__birth_threshold_reading, suppression_requirement, 24, 0.18).
narrative_ontology:measurement(pers_su_t36, personhood_boundary__birth_threshold_reading, suppression_requirement, 36, 0.19).
narrative_ontology:measurement(pers_su_t48, personhood_boundary__birth_threshold_reading, suppression_requirement, 48, 0.21).
narrative_ontology:measurement(pers_su_t60, personhood_boundary__birth_threshold_reading, suppression_requirement, 60, 0.22).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(personhood_boundary__birth_threshold_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(personhood_boundary__birth_threshold_reading, personhood_boundary__fitness_contingent_reading).
narrative_ontology:affects_constraint(personhood_boundary__birth_threshold_reading, personhood_boundary__potential_based_reading).

% DUAL FORMULATION NOTE:
% This story is one of three sibling readings of the personhood_boundary kernel, decomposed per the epsilon-invariance principle: birth_threshold_reading (this story, near-mountain, empty victim set, universal born-human beneficiary set), fitness_contingent_reading (victim set includes pre-fitness born humans, substantially different epsilon), and potential_based_reading (victim set may include severely disabled infants, different epsilon again). Each reading is authored as a separate constraint with its own base_properties, stakeholders, and classification; they are linked here via affects_constraints rather than merged into one multi-valued story.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
