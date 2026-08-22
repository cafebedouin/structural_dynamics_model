% ============================================================================
% CONSTRAINT STORY: personhood_boundary__birth_threshold_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   human_readable: Personhood Boundary: Birth Threshold Reading
 *   domain: moral_philosophy/commitment_systems
 *
 * SUMMARY:
 *   The birth_threshold_reading instantiates one normative reading of the
 *   personhood boundary kernel. It declares: personhood begins at the moment
 *   of birth (biological event demarcating the boundary); all born human
 *   organisms possess moral standing and inalienable rights; no state
 *   authority is legitimate in excluding any born human from the protected
 *   class. This reading vindicates the doctrine of universal human dignity
 *   and grounds rights claims on the universal birthright. The reading is
 *   held across much contemporary rights-based philosophy, constitutional law
 *   (post-1948 framing), and international humanitarian norms. Its sibling
 *   readings — fitness_contingent (standing contingent on demonstrated
 *   capability) and potential_based (standing on potential for rational
 *   agency) — coexist as live positions in academic and some legal contexts,
 *   but birth_threshold has achieved institutional dominance in post-WWII
 *   liberal frameworks. The story models THIS reading only, not the contest;
 *   it does not describe the sibling readings' claims or metrics.
 *
 * KEY AGENTS:
 *   - birth_threshold_advocates: Philosophers, legal scholars, humanitarian organizations, rights-based movements, most constitutional courts in liberal democracies — hold the reading as discovered truth
 *   - fitness_contingent_advocates: Some developmental ethics traditions, historical eugenic movements, some capability-contingent frameworks — hold that demonstrated fitness is necessary
 *   - potential_based_advocates: Some disability scholars, some religious traditions, some virtue-ethics frameworks — hold that potential is sufficient but actual capacity matters
 *   - all_born_humans: Structurally beneficiary of the reading's claim (universal standing); no exclusion permitted
 *   - pre_birth_entities: Structurally excluded from standing under this reading (fertilized embryos, fetuses, early-stage conceptuses)
 *   - severely_disabled_infants: Ambiguously situated: birth_threshold grants them standing automatically; fitness_contingent and potential_based readings would question it; the dispute is BETWEEN readings
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(personhood_boundary__birth_threshold_reading, 0.42).
domain_priors:suppression_score(personhood_boundary__birth_threshold_reading, 0.31).
domain_priors:theater_ratio(personhood_boundary__birth_threshold_reading, 0.08).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(personhood_boundary__birth_threshold_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(personhood_boundary__birth_threshold_reading, suppression_requirement, 0.31).
narrative_ontology:constraint_metric(personhood_boundary__birth_threshold_reading, theater_ratio, 0.08).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(personhood_boundary__birth_threshold_reading, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(personhood_boundary__birth_threshold_reading, resistance, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(personhood_boundary__birth_threshold_reading, mountain).
narrative_ontology:human_readable(personhood_boundary__birth_threshold_reading, "Personhood Boundary: Birth Threshold Reading").
narrative_ontology:topic_domain(personhood_boundary__birth_threshold_reading, "moral_philosophy/commitment_systems").

domain_priors:emerges_naturally(personhood_boundary__birth_threshold_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(personhood_boundary__birth_threshold_reading, '159108cd-ae3e-4060-9f57-6013526bfab1').
narrative_ontology:cs_kernel_codification('159108cd-ae3e-4060-9f57-6013526bfab1', fixed_text).
narrative_ontology:cs_authority_grounding('159108cd-ae3e-4060-9f57-6013526bfab1', lineage).
narrative_ontology:cs_interpretation_layer_present('159108cd-ae3e-4060-9f57-6013526bfab1').
narrative_ontology:cs_reading_relation('159108cd-ae3e-4060-9f57-6013526bfab1', personhood_boundary__fitness_contingent_reading, coexists_with).
narrative_ontology:cs_reading_relation('159108cd-ae3e-4060-9f57-6013526bfab1', personhood_boundary__potential_based_reading, coexists_with).
narrative_ontology:cs_axiom('159108cd-ae3e-4060-9f57-6013526bfab1', foundational, birth_marks_moral_personhood).
narrative_ontology:cs_axiom_status(birth_marks_moral_personhood, holdable).
narrative_ontology:cs_axiom_grounding('159108cd-ae3e-4060-9f57-6013526bfab1', birth_marks_moral_personhood, deontological).
narrative_ontology:cs_axiom('159108cd-ae3e-4060-9f57-6013526bfab1', foundational, universal_standing_no_exclusion).
narrative_ontology:cs_axiom_status(universal_standing_no_exclusion, holdable).
narrative_ontology:cs_axiom_grounding('159108cd-ae3e-4060-9f57-6013526bfab1', universal_standing_no_exclusion, deontological).
narrative_ontology:cs_reference_frame('159108cd-ae3e-4060-9f57-6013526bfab1', universal_human_dignity_doctrine).
narrative_ontology:cs_drift_state('159108cd-ae3e-4060-9f57-6013526bfab1', contemporary_bioethical_challenge_era, gap(axiom_overriding, substantial, true)).
narrative_ontology:cs_created_at('159108cd-ae3e-4060-9f57-6013526bfab1', '2026-06-11T14:32:00Z').
narrative_ontology:cs_kernel_id(personhood_boundary__birth_threshold_reading, personhood_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(personhood_boundary__birth_threshold_reading, all_born_humans).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(personhood_boundary__birth_threshold_reading, severely_disabled_infants).
narrative_ontology:constraint_vindicates(personhood_boundary__birth_threshold_reading, human_dignity_doctrine).
narrative_ontology:constraint_vindicates(personhood_boundary__birth_threshold_reading, universal_moral_standing_after_birth).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Receive universal moral standing and inalienable rights under this reading. No authority is legitimate in excluding any born human from the protected class. Protection extends to severely disabled infants, profoundly cognitively impaired persons, and all others simply by virtue of birth. The reading provides unconditional standing regardless of demonstrated capability, potential, or fitness.
narrative_ontology:constraint_stakeholder(personhood_boundary__birth_threshold_reading, all_born_humans, beneficiary,
    organized, generational, analytical, universal).

% Set and maintain the reading's institutional legitimacy through philosophical argument, constitutional interpretation, and international humanitarian law. Philosophers, constitutional courts, rights-based organizations, and most liberal democracies' legal frameworks hold and enforce this reading. They exclude or marginalize competing fitness_contingent and potential_based readings by controlling which interpretations receive authoritative hearing in legal and academic forums.
narrative_ontology:constraint_stakeholder(personhood_boundary__birth_threshold_reading, birth_threshold_advocates, agenda_setter,
    institutional, generational, analytical, universal).

% Argue that demonstrated capability or fitness is necessary for full moral standing; hold that birth alone is insufficient. Historically represented in eugenic movements, some contemporary developmental ethics, and capability-contingent frameworks. Their reading is institutionally marginal — mostly confined to academic debate; they would have voice in policy but are largely kept out of authoritative legal interpretation by the dominant birth_threshold institutional structure.
narrative_ontology:constraint_stakeholder(personhood_boundary__birth_threshold_reading, fitness_contingent_advocates, excluded,
    moderate, biographical, constrained, national).

% Argue that potential for rational agency (not yet realized) is the ground of standing; hold that birth_threshold's universal inclusion of severely disabled infants overreaches. Represented in some disability ethics scholarship, some religious traditions, some virtue-ethics frameworks. Their reading questions whether unconditional standing is justified when potential is absent or profoundly limited. They are institutionally marginal relative to birth_threshold, but more intellectually defended than fitness_contingent reading because they emphasize potential rather than actualized capability.
narrative_ontology:constraint_stakeholder(personhood_boundary__birth_threshold_reading, potential_based_advocates, excluded,
    moderate, biographical, constrained, national).

% Receive unconditional moral standing and protection under birth_threshold, regardless of cognitive capacity or fitness. Cannot advocate for themselves; their standing is granted and defended by birth_threshold_advocates on their behalf. Are the empirical boundary case where fitness_contingent and potential_based readings would question or reduce standing; birth_threshold provides categorical protection.
narrative_ontology:constraint_stakeholder(personhood_boundary__birth_threshold_reading, severely_disabled_infants, beneficiary,
    powerless, immediate, trapped, local).

% Codifies and enforces the birth_threshold reading through treaties, conventions, and norm-setting (Universal Declaration of Human Rights, Convention on the Rights of the Child, genocide conventions). Treats all born humans as moral persons with inalienable rights. Serves as an institutional anchor for the reading; deviation by any state regime faces normative pressure and potential sanctions.
narrative_ontology:constraint_stakeholder(personhood_boundary__birth_threshold_reading, international_humanitarian_regime, agenda_setter,
    institutional, generational, analytical, universal).
narrative_ontology:stakeholder_secondary_role(personhood_boundary__birth_threshold_reading, international_humanitarian_regime, observer).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(personhood_boundary__birth_threshold_reading, birth_threshold_advocates).
narrative_ontology:fixing_cost_class(personhood_boundary__birth_threshold_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a single, universal boundary for moral personhood that applies everywhere and excludes no born human: all born humans are persons with equal standing. Solves the coordination problem of determining who deserves protection in law, ethics, and policy without requiring ongoing evaluation of individual capability or fitness.
% TRANSFER_FUNCTION: Allocates moral standing from the set of all born humans to a collective protected status; redistributes institutional legitimacy from competing readings (fitness_contingent, potential_based) to birth_threshold by controlling which interpretations receive authoritative hearing. All born humans receive standing; competing frameworks lose institutional voice.
% ABSENT_VOICES: Pre-birth entities (fertilized embryos, fetuses, conceptuses) would object to their exclusion if they could speak, but the reading defines them as non-persons lacking moral standing — their exclusion is built into the framework itself, not into suppression of competing advocates. Fitness_contingent and potential_based advocates are present in academic discourse but excluded from authoritative legal and policy interpretation; their absence from binding decision-making is structural.
% DISAPPEARANCE_RATIONALE: If the birth_threshold reading disappeared and no replacement established itself, the world would rearrange substantially: no universal moral boundary would exist; different jurisdictions would adopt different thresholds (fitness, potential, religious markers); the legal protection of severely disabled infants would collapse or become contingent; the international humanitarian regime would fracture; competing frameworks would proliferate. The constraint's disappearance would destabilize global human-rights norms and leave moral standing ambiguous.
% FOUNDING_PROBLEM: Establish universal moral standing for all born humans without exception, so that no authority could legitimately exclude any born person from protection — eliminating the possibility of caste systems, slavery, eugenic exclusion, or capability-contingent denial of rights.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem remains live: contemporary bioethics, disability policy, and international humanitarian law still grapple with boundary questions (when life-sustaining treatment can be withheld from severely disabled infants, whether brain-death entities lose personhood, how to integrate severe cognitive disability into rights frameworks). Philosophers, legal scholars, and human-rights organizations outside the birth_threshold camp attest the founding problem persists — they argue birth_threshold solves only part of it (establishes universal standing) while creating new problems (unconditional standing for infants with no demonstrated capacity). The corroboration is mixed: the founding problem is alive, but the reading's solution is contested.
narrative_ontology:disappearance_verdict(personhood_boundary__birth_threshold_reading, world_rearranges).
narrative_ontology:founding_problem_status(personhood_boundary__birth_threshold_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(personhood_boundary__birth_threshold_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(personhood_boundary__birth_threshold_reading, 'none', 1).
narrative_ontology:epsilon_provenance(personhood_boundary__birth_threshold_reading, 0.42, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   The birth_threshold_reading is claimed as mountain (emerges_naturally: true) because it instantiates what adherents take to be discovered moral truth: personhood IS a natural fact about born humans, not a constructed arrangement. Extractiveness is measured at 0.42 — moderate because the reading does establish universal standing (low extraction in the direction of universality), but the reading is maintained partly by institutional and epistemic enforcement that excludes or marginalizes fitness_contingent and potential_based competing readings (moderate extraction in the direction of enforcement). Suppression is 0.31 because sibling readings are not legally barred in most jurisdictions but are institutionally marginalized (not forcibly suppressed, but have limited hearing in authoritative forums). Theater ratio is low (0.08) because the reading's primary function is truthful description of personhood, not performative maintenance; the small theater component reflects academic debates that sometimes become rituals of affirmation rather than genuine inquiry. Accessibility_collapse is high (0.78) because once the reading is accepted, the alternative boundary seems unthinkable — other thresholds (fitness, potential) collapse as accessible alternatives within the framework. Resistance is moderate (0.52): strong resistance from disability scholars and potential_based ethicists who argue birth_threshold's exclusion of pre-birth entities is arbitrary; weaker resistance from fitness_contingent advocates who mostly operate in academic margins.
 *
 * PERSPECTIVAL GAP:
 *   From the reading's internal standpoint: no extraction occurs (all born humans are protected equally). From an external standpoint: the reading extracts authority from alternative moral framings and institutional legitimacy from the enforcement machinery that marginalizes fitness_contingent and potential_based readings. From the standpoint of a severely_disabled_infant: the reading unconditionally grants standing (lowest possible d); from the standpoint of a potential_based advocate: the reading overreaches by granting standing to infants whose potential for rational agency is not yet evident.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries: all_born_humans (universal standing, no exclusion, d ≈ 0.0 for this group). Victims/Excluded: pre_birth_entities (no standing under this reading, but they are NON-AGENTS and do not enter the stakeholder surface or directionality calculation — they cannot voice interests). Agenda-setters: birth_threshold_advocates in philosophical, legal, and institutional seats (power: institutional/organized; set the terms of the debate; maintain the reading's institutional dominance). The contradiction is: if all born humans are beneficiaries with universal standing, who is extracted from? The answer is the reading's own internal logic: the extraction runs in the direction of those excluded from standing (pre-birth entities), but pre-birth entities are not agents in the moral sense the reading defines. This is not a defect of the model — it is the reading's own structural claim: moral standing begins at birth, so pre-birth entities cannot claim to be harmed by the reading because they have no moral standing to claim with. The extraction (0.42) is measured as institutional enforcement excluding potential_based and fitness_contingent readings from authoritative voice — a moderate asymmetric cost borne by competing framings, not by born humans.
 *
 * MANDATROPHY ANALYSIS:
 *   The reading does not exhibit mandatrophy: its founding problem (establishing universal moral standing for all born humans) is still live in discourse and institutional practice. The reading is not a degraded function maintained theatrically; it is an active commitment with institutional enforcement. However, the reading sits at risk of false-summit classification (Mountain with declared beneficiaries triggers FSM evaluation): all_born_humans are declared beneficiaries, yet the reading is claimed as naturally emergent. The omega variables (natural_law_vs_constructed_boundary, sibling_foreclosure_possibility) document the irreducible ambiguity: whether the birth threshold is discovered or constructed-yet-universal. If constructed-yet-universal (constructed boundary that benefits all born humans by establishing universal standing), the constraint is a false summit and should be reclassified as tangled_rope (coordinates universal standing + asymmetrically enforces exclusion of alternative readings). The mandatrophy boundary is not crossed because the reading's founding problem remains live; the false-summit boundary is crossed if the reading is maintained partly by institutional benefit to born-humans rather than purely by discovered truth.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_constructed_boundary,
    'Is the birth threshold a natural fact about personhood (discovered), or a constructed boundary that benefits all born humans by establishing universal standing?',
    'Trace the reading''s historical emergence: does it track contingent policy choices and institutional benefits to specific actors, or does it describe a discovered moral invariant? Compare with sibling readings'' framing of the same biological event.',
    'If constructed-and-beneficial, the constraint is a false summit: the reading''s own logic vindicates universal standing for all born humans, but the reading itself was selected/maintained partly because it benefits born humans over pre-birth entities. Classification would shift from mountain to tangled_rope (coordination + asymmetric benefit). If genuinely discovered, it remains mountain.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_boundary, conceptual, 'Whether the birth threshold is a natural law or a constructed-yet-universal commitment.').

omega_variable(
    sibling_foreclosure_possibility,
    'Do the fitness_contingent and potential_based readings genuinely coexist with birth_threshold, or does birth_threshold''s core premise foreclose them within a single framework?',
    'Test whether a single moral framework could simultaneously hold ''all born humans have standing'' (birth_threshold) AND ''demonstrated fitness is necessary for standing'' (fitness_contingent). Can both be true in one system, or is one a direct negation of the other?',
    'If foreclosure is real (one premise logically rules out the other), the relation is forecloses, not coexists_with. Foreclosure suggests the readings instantiate genuinely competing commitments, not just different policy emphases. If coexistence is possible (a framework could hold both, with different scopes), the relation is coexists_with.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_foreclosure_possibility, conceptual, 'Whether readings are logically compatible or mutually exclusive.').

omega_variable(
    suppression_mechanism_ambiguity,
    'The measured suppression (0.31) reflects what: structural legal barriers preventing alternative readings from being heard, or internalized acceptance of the birth boundary within the reading''s own tradition?',
    'Examine whether suppression decreases when legal barriers are removed (suggesting structural suppression) or persists when alternatives are articulated (suggesting internalized acceptance). In jurisdictions where potential_based or fitness_contingent readings are fully legally permitted but rarely adopted, does suppression still register?',
    'If structural, the suppression is exterior to the reading''s own logical force and reflects institutional enforcement. If internalized, the reading has convinced its adherents from within. High internalized suppression makes exit from the reading''s framework itself difficult, not just legal alternatives.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Whether suppression of sibling readings is structural or internalized.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(personhood_boundary__birth_threshold_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pers_tr_t0, personhood_boundary__birth_threshold_reading, theater_ratio, 0, 0.06).
narrative_ontology:measurement_basis(pers_tr_t0, observed).
narrative_ontology:measurement(pers_tr_t10, personhood_boundary__birth_threshold_reading, theater_ratio, 10, 0.07).
narrative_ontology:measurement_basis(pers_tr_t10, observed).
narrative_ontology:measurement(pers_tr_t20, personhood_boundary__birth_threshold_reading, theater_ratio, 20, 0.075).
narrative_ontology:measurement_basis(pers_tr_t20, observed).
narrative_ontology:measurement(pers_tr_t30, personhood_boundary__birth_threshold_reading, theater_ratio, 30, 0.08).
narrative_ontology:measurement_basis(pers_tr_t30, observed).

% Extraction over time
narrative_ontology:measurement(pers_be_t0, personhood_boundary__birth_threshold_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement_basis(pers_be_t0, observed).
narrative_ontology:measurement(pers_be_t10, personhood_boundary__birth_threshold_reading, base_extractiveness, 10, 0.4).
narrative_ontology:measurement_basis(pers_be_t10, observed).
narrative_ontology:measurement(pers_be_t20, personhood_boundary__birth_threshold_reading, base_extractiveness, 20, 0.41).
narrative_ontology:measurement_basis(pers_be_t20, observed).
narrative_ontology:measurement(pers_be_t30, personhood_boundary__birth_threshold_reading, base_extractiveness, 30, 0.42).
narrative_ontology:measurement_basis(pers_be_t30, observed).

% Suppression requirement over time
narrative_ontology:measurement(pers_su_t0, personhood_boundary__birth_threshold_reading, suppression_requirement, 0, 0.28).
narrative_ontology:measurement_basis(pers_su_t0, observed).
narrative_ontology:measurement(pers_su_t10, personhood_boundary__birth_threshold_reading, suppression_requirement, 10, 0.295).
narrative_ontology:measurement_basis(pers_su_t10, observed).
narrative_ontology:measurement(pers_su_t20, personhood_boundary__birth_threshold_reading, suppression_requirement, 20, 0.305).
narrative_ontology:measurement_basis(pers_su_t20, observed).
narrative_ontology:measurement(pers_su_t30, personhood_boundary__birth_threshold_reading, suppression_requirement, 30, 0.31).
narrative_ontology:measurement_basis(pers_su_t30, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(personhood_boundary__birth_threshold_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(personhood_boundary__birth_threshold_reading, 0.12).
narrative_ontology:affects_constraint(personhood_boundary__birth_threshold_reading, personhood_boundary__fitness_contingent_reading).
narrative_ontology:affects_constraint(personhood_boundary__birth_threshold_reading, personhood_boundary__potential_based_reading).

% DUAL FORMULATION NOTE:
% The personhood_boundary kernel decomposes into three structurally distinct constraints, one per reading. Each reading instantiates a different ε (extractiveness of enforcement), different beneficiary/victim structure, and different resistance profile. Decomposition rationale: the kernel is the contestation itself — the three readings are not different measurements of one constraint, but three different constraints sharing a common contested normative ground. Each reading's ε is defined over the standing arrangement under that reading's interpretation. Readings are linked via network.affects_constraints to enable contamination and coupling analysis: if birth_threshold's institutional authority erodes, potential_based and fitness_contingent readings may gain ground (affects). If a sibling reading's empirical grounding is undermined, birth_threshold's exclusion of that reading becomes harder to justify (affects). The three stories together form the personhood_boundary constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
