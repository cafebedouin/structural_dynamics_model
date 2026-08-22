% ============================================================================
% CONSTRAINT STORY: personhood_boundary__fitness_contingent_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_personhood_boundary__fitness_contingent_reading, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: personhood_boundary__fitness_contingent_reading
 *   human_readable: Personhood Contingent on Demonstrated Fitness
 *   domain: moral_philosophy/historical_ethics
 *
 * SUMMARY:
 *   This constraint instantiates one reading of the contested personhood
 *   kernel: the claim that moral standing is contingent on demonstrated
 *   fitness (cognitive competence, physical viability, or recognizable human
 *   form) rather than automatically accorded to all born humans. Under this
 *   reading, newborns—especially those with disabilities or developmental
 *   atypicality—enter a pre-personhood status during which they lack legal
 *   and moral protection. The constraint is enforced by state authority
 *   gatekeeping who qualifies as a person, backed by institutional mechanisms
 *   (medical evaluation, legal doctrine, institutional practice) that exclude
 *   infants failing the test. The arrangement extracts authority over
 *   life-and-death and moves vulnerable entities into the category of
 *   evaluable objects rather than rights-bearers. This is a reading-specific
 *   story: it describes the structural operation of fitness-contingent
 *   personhood as this reading understands and enacts it, assessed by that
 *   reading's own lights (the referent ε is the standing fitness-contingent
 *   arrangement, not the birth-threshold alternative).
 *
 * KEY AGENTS:
 *   - state_authority: institutional agenda-setter with power to define and administer fitness criteria
 *   - fitness_evaluators: institutional beneficiary with gatekeeping authority over personhood entry
 *   - pre_fitness_infants, severely_disabled_infants, cognitively_atypical_newborns: powerless victims trapped outside personhood until or unless deemed fit
 *   - parents_and_guardians: dual-positioned beneficiaries/payers, holding discretionary authority during pre-fitness window
 *   - birth_threshold_advocates, potential_based_theorists: excluded voices arguing for alternative personhood framings
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(personhood_boundary__fitness_contingent_reading, 0.89).
domain_priors:suppression_score(personhood_boundary__fitness_contingent_reading, 0.91).
domain_priors:theater_ratio(personhood_boundary__fitness_contingent_reading, 0.62).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(personhood_boundary__fitness_contingent_reading, extractiveness, 0.89).
narrative_ontology:constraint_metric(personhood_boundary__fitness_contingent_reading, suppression_requirement, 0.91).
narrative_ontology:constraint_metric(personhood_boundary__fitness_contingent_reading, theater_ratio, 0.62).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(personhood_boundary__fitness_contingent_reading, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(personhood_boundary__fitness_contingent_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(personhood_boundary__fitness_contingent_reading, snare).
narrative_ontology:human_readable(personhood_boundary__fitness_contingent_reading, "Personhood Contingent on Demonstrated Fitness").
narrative_ontology:topic_domain(personhood_boundary__fitness_contingent_reading, "moral_philosophy/historical_ethics").

domain_priors:requires_active_enforcement(personhood_boundary__fitness_contingent_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(personhood_boundary__fitness_contingent_reading, '8b681ad7-478d-4264-8a4c-a1b66f8bca7a').
narrative_ontology:cs_kernel_codification('8b681ad7-478d-4264-8a4c-a1b66f8bca7a', distributed).
narrative_ontology:cs_authority_grounding('8b681ad7-478d-4264-8a4c-a1b66f8bca7a', extraction).
narrative_ontology:cs_interpretation_layer_present('8b681ad7-478d-4264-8a4c-a1b66f8bca7a').
narrative_ontology:cs_reading_relation('8b681ad7-478d-4264-8a4c-a1b66f8bca7a', personhood_boundary__birth_threshold_reading, forecloses).
narrative_ontology:cs_reading_relation('8b681ad7-478d-4264-8a4c-a1b66f8bca7a', personhood_boundary__potential_based_reading, coexists_with).
narrative_ontology:cs_axiom('8b681ad7-478d-4264-8a4c-a1b66f8bca7a', foundational, personhood_contingent_on_demonstrated_capacity).
narrative_ontology:cs_axiom_status(personhood_contingent_on_demonstrated_capacity, holdable).
narrative_ontology:cs_axiom_grounding('8b681ad7-478d-4264-8a4c-a1b66f8bca7a', personhood_contingent_on_demonstrated_capacity, empirically_contingent).
narrative_ontology:cs_axiom('8b681ad7-478d-4264-8a4c-a1b66f8bca7a', foundational, state_authority_legitimate_personhood_gatekeeper).
narrative_ontology:cs_axiom_status(state_authority_legitimate_personhood_gatekeeper, holdable).
narrative_ontology:cs_axiom_grounding('8b681ad7-478d-4264-8a4c-a1b66f8bca7a', state_authority_legitimate_personhood_gatekeeper, conventional).
narrative_ontology:cs_reference_frame('8b681ad7-478d-4264-8a4c-a1b66f8bca7a', fitness_contingent_personhood_regime).
narrative_ontology:cs_drift_state('8b681ad7-478d-4264-8a4c-a1b66f8bca7a', contemporary_rights_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('8b681ad7-478d-4264-8a4c-a1b66f8bca7a', '').
narrative_ontology:cs_kernel_id(personhood_boundary__fitness_contingent_reading, personhood_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(personhood_boundary__fitness_contingent_reading, state_authority).
narrative_ontology:constraint_beneficiary(personhood_boundary__fitness_contingent_reading, fitness_evaluators).
narrative_ontology:constraint_victim(personhood_boundary__fitness_contingent_reading, pre_fitness_infants).
narrative_ontology:constraint_victim(personhood_boundary__fitness_contingent_reading, severely_disabled_infants).
narrative_ontology:constraint_victim(personhood_boundary__fitness_contingent_reading, cognitively_atypical_newborns).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(personhood_boundary__fitness_contingent_reading, parents_and_guardians).
narrative_ontology:constraint_victim(personhood_boundary__fitness_contingent_reading, parents_and_guardians).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Establishes and administers the fitness test that determines personhood entry. Controls what counts as fitness, who administers the evaluation, and what recourse exists for failure. Collects enforcement authority and institutional prestige from the monopoly on personhood certification. May exclude infants deemed unfit from legal protections, inheritance, and burial rites.
narrative_ontology:constraint_stakeholder(personhood_boundary__fitness_contingent_reading, state_authority, agenda_setter,
    institutional, generational, arbitrage, national).

% Physicians, midwives, or specialized judges who conduct the fitness assessment. Gain professional authority, income, and epistemic power from the gatekeeping role. Their judgment determines which infants enter the moral community; their standards define what counts as human.
narrative_ontology:constraint_stakeholder(personhood_boundary__fitness_contingent_reading, fitness_evaluators, beneficiary,
    institutional, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(personhood_boundary__fitness_contingent_reading, fitness_evaluators, agenda_setter).

% Entities not yet subjected to or not yet passed the fitness test. Under this reading, they lack moral standing until certified. They may be abandoned, infanticided, or denied basic care without violating the moral law because they are not yet recognized as persons. Their survival depends entirely on the discretionary choice of those who control the fitness gate.
narrative_ontology:constraint_stakeholder(personhood_boundary__fitness_contingent_reading, pre_fitness_infants, payer,
    powerless, immediate, trapped, local).

% Newborns with congenital conditions, neurological impairments, or developmental disabilities that fail to meet the fitness criteria. Permanently excluded from personhood under this reading. They bear the cost of the arrangement through non-recognition: they may be left to die, institutionalized indefinitely, or used for medical experimentation without legal recourse.
narrative_ontology:constraint_stakeholder(personhood_boundary__fitness_contingent_reading, severely_disabled_infants, payer,
    powerless, immediate, trapped, local).

% Infants whose cognitive development deviates from expected norms—born with conditions affecting perception, processing, or early responsiveness. The fitness test may be calibrated to exclude them. They cannot advocate for themselves and depend entirely on whether evaluators recognize their fitness; if not, they remain non-persons.
narrative_ontology:constraint_stakeholder(personhood_boundary__fitness_contingent_reading, cognitively_atypical_newborns, payer,
    powerless, immediate, trapped, local).

% Hold a dual position: they benefit from the fitness gate by having legal authority to determine an infant's fate during the pre-fitness window (abandonment, death, institutionalization are state-licensed), but they also bear risk—their own capacity to parent may become the subject of fitness evaluation, and their disabled or atypical children become perpetual legal non-persons.
narrative_ontology:constraint_stakeholder(personhood_boundary__fitness_contingent_reading, parents_and_guardians, beneficiary,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(personhood_boundary__fitness_contingent_reading, parents_and_guardians, payer).

% Moral philosophers, disability advocates, and legal theorists who hold that personhood begins at birth and that all born humans possess intrinsic moral standing. They are systematically excluded from the institutions that define and administer fitness tests under this reading. Their arguments for unconditional personhood are treated as naive or dangerous.
narrative_ontology:constraint_stakeholder(personhood_boundary__fitness_contingent_reading, birth_threshold_advocates, excluded,
    organized, generational, constrained, national).

% Moral philosophers who ground personhood in potential for rational agency. They would argue for personhood based on developmental trajectory rather than demonstrated current fitness. They are excluded from fitness-test design and operate in constant tension with the fitness-contingent framework.
narrative_ontology:constraint_stakeholder(personhood_boundary__fitness_contingent_reading, potential_based_theorists, excluded,
    moderate, generational, constrained, national).

% Records the structural operation of this reading: who enters the moral community via what mechanism, what costs attach to pre-fitness status, how the fitness gate is defended institutionally, and what resistance it encounters.
narrative_ontology:constraint_stakeholder(personhood_boundary__fitness_contingent_reading, analytical_observer, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(personhood_boundary__fitness_contingent_reading, state_authority).
narrative_ontology:fixing_cost_class(personhood_boundary__fitness_contingent_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Nominally coordinates a rational community: by admitting only those demonstrably fit for moral reciprocity, the arrangement purports to maintain the integrity of moral standing as meaningful rather than universal. The stated problem is that unconditional personhood would include entities incapable of participation in the moral compact.
% TRANSFER_FUNCTION: Transfers authority over life-and-death from law and reciprocal duty to state-administered fitness determination. Pre-fitness infants are moved from the status of potential persons with inherent rights to the status of evaluable objects. The arrangement moves life-or-death decision power from universal protection norms to discretionary state authority.
% ABSENT_VOICES: Severely disabled and cognitively atypical infants themselves cannot testify; they are the paradigm case of enforced silence. Birth-threshold advocates and potential-based theorists are structurally excluded from fitness-test design and institutional validation. Their counter-readings of personhood are treated as contestable philosophy rather than operative principle.
% DISAPPEARANCE_RATIONALE: If the fitness-contingent reading and its enforcement vanished, infants would immediately enter the moral community at birth; abandonment and infanticide of disabled or atypical newborns would become legal violations; institutional care standards would shift from discretionary warehousing to rights-based protection. The social and legal world would reorganize around unconditional personhood at birth.
% FOUNDING_PROBLEM: How to prevent the moral community from becoming incoherent by including entities wholly incapable of participation in reciprocal moral reasoning. Early modern and ancient theories held that personhood required demonstrated capacity for rational reflection, language, or recognizable human form.
% FOUNDING_PROBLEM_CORROBORATION: Historical authorities (Aristotle, Aquinas, early modern theorists) attest that the founding problem was live in their eras—they held fitness-contingent personhood as coherent moral doctrine. Contemporary disability scholars, bioethicists, and human-rights frameworks attest that the founding problem is resolved: modern developmental science shows infants develop capacities over time rather than possessing or lacking them at birth, making fitness-contingency incoherent. No contemporary external corroboration of the founding problem exists; only institutional authorities benefiting from the gate maintain the claim.
narrative_ontology:disappearance_verdict(personhood_boundary__fitness_contingent_reading, world_rearranges).
narrative_ontology:founding_problem_status(personhood_boundary__fitness_contingent_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(personhood_boundary__fitness_contingent_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(personhood_boundary__fitness_contingent_reading, 'none', 1).
narrative_ontology:epsilon_provenance(personhood_boundary__fitness_contingent_reading, 0.89, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(personhood_boundary__fitness_contingent_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(personhood_boundary__fitness_contingent_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(personhood_boundary__fitness_contingent_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is extremely high (0.89) because the arrangement transfers fundamental status—personhood itself—from automatic inclusion to contingent state certification; the cost to pre-fitness entities is exclusion from the moral community entirely, not a marginal extraction but categorical non-recognition. Suppression is equally extreme (0.91) because the pre-fitness entities are powerless and voiceless; resistance to the gate is mounted entirely by external advocates (birth-threshold and potential-based theorists) who are systematically excluded from institutional power. Theater is substantial (0.62) and rising because the fitness test is dressed as scientific/medical evaluation ('objective fitness criteria') even as it carries implicit normative judgments about what counts as acceptable humanity—the theatrical element grows as the evaluators accumulate and formalize the medical language. Accessibility collapse is high (0.78) because once an infant is classified as unfit, alternatives to their non-personhood effectively vanish; they cannot petition, cannot appeal to a higher moral authority, cannot exit. The measurement series traces extractiveness and suppression requirement holding steady at near-maximum while theater rises slightly as medical framing accumulates over the interval.
 *
 * PERSPECTIVAL GAP:
 *   From the state-authority seat, this constraint is the preservation of moral coherence: it prevents the moral community from becoming a incoherent category that includes entities incapable of moral reasoning. From the pre-fitness-infant seat, it is pure extraction and exclusion: they have no say in the criteria, no recourse if deemed unfit, and no path to appeal. From the birth-threshold advocate seat, it is a false summit—presented as natural fitness but actually constructed institutional authority. The engine computes these divergent classifications from the structural data: the beneficiary seats and target seats should compute to radically different constraint types even though they inhabit the same rule. This divergence is the analytical signal that the claimed type (snare) has substantive ground.
 *
 * DIRECTIONALITY LOGIC:
 *   State authority and fitness evaluators sit at the beneficiary end of directionality (d → 0.0): they collect institutional power, authority, and epistemic prestige from controlling the personhood gate. Pre-fitness and disabled infants sit at the full-target end (d → 1.0): they bear the cost of non-personhood (exclusion from moral protection, susceptibility to abandonment or death) with no exit option—they are trapped in their pre-fitness status indefinitely. Parents and guardians occupy a dual position: they benefit from discretionary authority over pre-fitness infants during the gate period, but they also pay through uncertainty about whether their own children will be deemed fit and through permanent alienation of disabled children into legal non-personhood. The excluded advocates (birth-threshold and potential-based theorists) operate outside the constraint's formal structure but experience it as suppressive: their alternative readings are marginalized, their arguments are excluded from policy, and they cannot change the evaluative criteria from within.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (ensuring moral coherence by excluding incapable entities) is categorically DEAD in contemporary developmental science and disability ethics. Modern developmental psychology shows that capacities for language, reasoning, and reciprocity develop gradually over infancy and childhood—there is no discrete fitness threshold at which a newborn suddenly becomes capable. Simultaneously, universal human-rights frameworks have moved to unconditional personhood at birth. The arrangement persists not because the founding problem remains live but because institutional authorities have invested in the gatekeeping role itself: physicians claim expertise in fitness evaluation, states claim authority over personhood definition, and the disability of exit for pre-fitness infants creates a constituency locked into dependence. The theater ratio rising while extractiveness plateaus suggests that the arrangement is increasingly performative—the scientific/medical language of 'fitness evaluation' substitutes for a founding moral problem that no longer coheres, and the performance maintains institutional power in the absence of genuine coordination justification. This is a mandatrophy candidate: the founding function is obsolete, the arrangement persists through institutional inertia and gatekeeping authority, and the main work is theatrical maintenance of the medical/scientific facade.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    fitness_criteria_boundary,
    'What specific criteria constitute ''demonstrated fitness''? Is the boundary set at viability, recognizable humanity, cognitive response, language, or some other marker?',
    'Historical analysis of fitness-test implementations (medical evaluations, legal doctrine, institutional practice) across cultures and eras. What did actual fitness gatekeepers measure and why?',
    'Different criteria boundaries would shift who enters and who is excluded from the pre-fitness-victim set. A narrow boundary (only severe disability) would reduce extractiveness; a broad boundary (any deviation from assumed-normal) would increase it.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(fitness_criteria_boundary, empirical, 'The operational definition of fitness used to determine personhood entry').

omega_variable(
    suppression_internalization,
    'Is the suppression of pre-fitness entities and excluded advocates structural (enforced by institutional exclusion and legal barriers) or internalized (incorporated into self-concept and moral intuition)?',
    'Post-exit trajectory analysis: if an infant is deemed unfit but then later recognized as a person (through advocacy or institutional change), do suppression effects persist? Do excluded advocates internalize the gatekeeping authority even when arguing against it?',
    'If suppression is structural, removing the fitness gate would rapidly restore protection and voice. If internalized, protection would recover more slowly and excluded advocates would need deprogramming. This affects the cost and trajectory of fixing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_internalization, empirical, 'Whether suppression persists through institutional barriers alone or also through internalized moral intuitions').

omega_variable(
    alternative_framing_natural_law,
    'Could fitness-contingency be reframed as natural law (a mountain) rather than an institutional snare? Is there an underlying fact about personhood that fitness tests merely discover?',
    'Comparison with actual natural laws (gravity, logical contradiction): can fitness-contingency persist absent institutional enforcement? Would pre-fitness entities spontaneously organize as non-persons if the gatekeeping state vanished? Would parents naturally refuse protection to unfit newborns without legal permission?',
    'If fitness-contingency is genuinely natural, the extracted authority is less extractive (it is enforcing a natural fact rather than constructing a false one). If it is constructed institutional authority, the snare classification holds. This is the false-summit test.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(alternative_framing_natural_law, conceptual, 'Whether fitness-contingent personhood is a natural fact or an institutional construction').

omega_variable(
    authority_motivation_ambiguity,
    'Do state authorities and fitness evaluators maintain the fitness gate primarily because they believe personhood genuinely requires fitness (coherence motivation) or because the gatekeeping role itself is valuable to them (power-collection motivation)?',
    'Examine how fitness evaluators revise criteria in response to evidence. If developmental science shows capacities develop gradually, do they update the boundary or defend it? Do they resist efforts to democratize personhood entry?',
    'If motivation is coherence, the arrangement might be reformed by better science (showing fitness is not a threshold but a trajectory). If motivation is power-collection, institutional change would be required to break the gate. The motivation affects repair costs.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(authority_motivation_ambiguity, empirical, 'Whether the fitness gate is maintained for its stated epistemic function or for the authority it grants').

omega_variable(
    reading_foreclosure_asymmetry,
    'The fitness-contingent reading forecloses the birth-threshold reading (they cannot coexist in a single moral framework). But birth-threshold advocates mount resistance; do they mount it as internal critique (trying to correct the framework from within) or external critique (rejecting the framework entirely)?',
    'Examine advocacy literature and institutional testimony from birth-threshold advocates. Do they argue ''personhood must start at birth'' (internal critique of fitness criteria) or ''the fitness-gate framework itself is incoherent'' (external critique)?',
    'Internal critique accepts the personhood-kernel framework and argues for its correction; external critique rejects the kernel itself. Different amendment strategies follow. This determines whether advocacy pressure leads to boundary-shift or framework-replacement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_foreclosure_asymmetry, conceptual, 'Whether excluded advocates critique from within the fitness-gate framework or reject it entirely').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(personhood_boundary__fitness_contingent_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pers_tr_t0, personhood_boundary__fitness_contingent_reading, theater_ratio, 0, 0.45).
narrative_ontology:measurement_basis(pers_tr_t0, observed).
narrative_ontology:measurement(pers_tr_t3, personhood_boundary__fitness_contingent_reading, theater_ratio, 3, 0.5).
narrative_ontology:measurement_basis(pers_tr_t3, observed).
narrative_ontology:measurement(pers_tr_t6, personhood_boundary__fitness_contingent_reading, theater_ratio, 6, 0.55).
narrative_ontology:measurement_basis(pers_tr_t6, observed).
narrative_ontology:measurement(pers_tr_t12, personhood_boundary__fitness_contingent_reading, theater_ratio, 12, 0.61).
narrative_ontology:measurement_basis(pers_tr_t12, observed).
narrative_ontology:measurement(pers_tr_t18, personhood_boundary__fitness_contingent_reading, theater_ratio, 18, 0.62).
narrative_ontology:measurement_basis(pers_tr_t18, observed).
narrative_ontology:measurement(pers_tr_t24, personhood_boundary__fitness_contingent_reading, theater_ratio, 24, 0.62).
narrative_ontology:measurement_basis(pers_tr_t24, observed).

% Extraction over time
narrative_ontology:measurement(pers_be_t0, personhood_boundary__fitness_contingent_reading, base_extractiveness, 0, 0.82).
narrative_ontology:measurement_basis(pers_be_t0, observed).
narrative_ontology:measurement(pers_be_t3, personhood_boundary__fitness_contingent_reading, base_extractiveness, 3, 0.85).
narrative_ontology:measurement_basis(pers_be_t3, observed).
narrative_ontology:measurement(pers_be_t6, personhood_boundary__fitness_contingent_reading, base_extractiveness, 6, 0.87).
narrative_ontology:measurement_basis(pers_be_t6, observed).
narrative_ontology:measurement(pers_be_t12, personhood_boundary__fitness_contingent_reading, base_extractiveness, 12, 0.89).
narrative_ontology:measurement_basis(pers_be_t12, observed).
narrative_ontology:measurement(pers_be_t18, personhood_boundary__fitness_contingent_reading, base_extractiveness, 18, 0.88).
narrative_ontology:measurement_basis(pers_be_t18, observed).
narrative_ontology:measurement(pers_be_t24, personhood_boundary__fitness_contingent_reading, base_extractiveness, 24, 0.89).
narrative_ontology:measurement_basis(pers_be_t24, observed).

% Suppression requirement over time
narrative_ontology:measurement(pers_su_t0, personhood_boundary__fitness_contingent_reading, suppression_requirement, 0, 0.88).
narrative_ontology:measurement_basis(pers_su_t0, observed).
narrative_ontology:measurement(pers_su_t3, personhood_boundary__fitness_contingent_reading, suppression_requirement, 3, 0.89).
narrative_ontology:measurement_basis(pers_su_t3, observed).
narrative_ontology:measurement(pers_su_t6, personhood_boundary__fitness_contingent_reading, suppression_requirement, 6, 0.9).
narrative_ontology:measurement_basis(pers_su_t6, observed).
narrative_ontology:measurement(pers_su_t12, personhood_boundary__fitness_contingent_reading, suppression_requirement, 12, 0.91).
narrative_ontology:measurement_basis(pers_su_t12, observed).
narrative_ontology:measurement(pers_su_t18, personhood_boundary__fitness_contingent_reading, suppression_requirement, 18, 0.91).
narrative_ontology:measurement_basis(pers_su_t18, observed).
narrative_ontology:measurement(pers_su_t24, personhood_boundary__fitness_contingent_reading, suppression_requirement, 24, 0.91).
narrative_ontology:measurement_basis(pers_su_t24, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(personhood_boundary__fitness_contingent_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(personhood_boundary__fitness_contingent_reading, 0.18).
narrative_ontology:affects_constraint(personhood_boundary__fitness_contingent_reading, personhood_boundary__birth_threshold_reading).
narrative_ontology:affects_constraint(personhood_boundary__fitness_contingent_reading, personhood_boundary__potential_based_reading).

% DUAL FORMULATION NOTE:
% This is one reading of the personhood_boundary kernel. The fitness-contingent reading grounds personhood entry in demonstrated capacity and state certification; it forecloses the birth_threshold reading (personhood at birth for all born humans) because the two readings cannot coexist in a single moral framework. It coexists with the potential_based reading (both exclude some infants, but on different criteria). All three readings describe the same kernel (personhood status and entry mechanism) but authorize different victims and different gatekeeping authority. The network links show how changing one reading's implementation (e.g., by recognizing all pre-fitness infants as persons) would structurally influence the viability of competing readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(personhood_boundary__fitness_contingent_reading, organized, 0.38).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
