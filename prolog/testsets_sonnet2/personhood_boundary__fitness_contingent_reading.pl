% ============================================================================
% CONSTRAINT STORY: personhood_boundary__fitness_contingent_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   human_readable: Fitness-Contingent Personhood Boundary (Post-Natal Fitness Test Reading)
 *   domain: moral_philosophy/historical_ethics/commitment_systems
 *
 * SUMMARY:
 *   This story instantiates the fitness-contingent reading of the contested
 *   personhood-boundary kernel: moral standing is not conferred at birth (the
 *   birth_threshold_reading) nor grounded in potential for rational agency
 *   alone (the potential_based_reading), but is contingent on an infant
 *   passing a demonstrated-fitness evaluation performed by an external
 *   authority after birth. Under this reading, pre-fitness entities — infants
 *   who have not yet been evaluated, and those found wanting — lack moral
 *   standing and can be legally and communally excluded from protection.
 *   Historically this reading grounds practices from ancient infant exposure
 *   (Spartan and Roman variants) through nineteenth- and twentieth-century
 *   eugenic selective non-treatment of disabled newborns to contemporary
 *   residual debates over selective non-resuscitation. The ε authored here is
 *   high because, from the reading's own internal logic, the arrangement
 *   extracts survival, protection, and standing from the excluded class and
 *   hands discretionary sorting power to institutions with independent
 *   interest in reduced resource burden — this is the reading's own
 *   operation, not a claim about what a rights-respecting alternative would
 *   produce.
 *
 * KEY AGENTS:
 *   - pre_fitness_infants: primary target (powerless/trapped) — bears total exclusion risk with no capacity to contest
 *   - disabled_neonates: disproportionately targeted subgroup (powerless/trapped)
 *   - state_selection_authorities: primary agenda-setter (institutional/arbitrage) — defines and enforces the fitness threshold
 *   - fitness_evaluating_physicians: administering beneficiary (organized/constrained) — professional identity bound to the boundary's operation
 *   - resource_conserving_institutions: diffuse beneficiary (institutional/arbitrage) — avoids care costs without administering the test
 *   - disability_rights_advocates: excluded objecting voice (moderate/constrained)
 *   - moral_philosophers_observing: analytical observer (analytical/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(personhood_boundary__fitness_contingent_reading, 0.86).
domain_priors:suppression_score(personhood_boundary__fitness_contingent_reading, 0.88).
domain_priors:theater_ratio(personhood_boundary__fitness_contingent_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(personhood_boundary__fitness_contingent_reading, extractiveness, 0.86).
narrative_ontology:constraint_metric(personhood_boundary__fitness_contingent_reading, suppression_requirement, 0.88).
narrative_ontology:constraint_metric(personhood_boundary__fitness_contingent_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(personhood_boundary__fitness_contingent_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(personhood_boundary__fitness_contingent_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(personhood_boundary__fitness_contingent_reading, snare).
narrative_ontology:human_readable(personhood_boundary__fitness_contingent_reading, "Fitness-Contingent Personhood Boundary (Post-Natal Fitness Test Reading)").
narrative_ontology:topic_domain(personhood_boundary__fitness_contingent_reading, "moral_philosophy/historical_ethics/commitment_systems").

domain_priors:requires_active_enforcement(personhood_boundary__fitness_contingent_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(personhood_boundary__fitness_contingent_reading, '820d4dac-e25e-44bf-9969-03e95dfa87e7').
narrative_ontology:cs_kernel_codification('820d4dac-e25e-44bf-9969-03e95dfa87e7', distributed).
narrative_ontology:cs_authority_grounding('820d4dac-e25e-44bf-9969-03e95dfa87e7', practice).
narrative_ontology:cs_interpretation_layer_present('820d4dac-e25e-44bf-9969-03e95dfa87e7').
narrative_ontology:cs_reading_relation('820d4dac-e25e-44bf-9969-03e95dfa87e7', personhood_boundary__birth_threshold_reading, forecloses).
narrative_ontology:cs_reading_relation('820d4dac-e25e-44bf-9969-03e95dfa87e7', personhood_boundary__potential_based_reading, influences).
narrative_ontology:cs_axiom('820d4dac-e25e-44bf-9969-03e95dfa87e7', foundational, standing_requires_demonstrated_present_fitness).
narrative_ontology:cs_axiom_status(standing_requires_demonstrated_present_fitness, holdable).
narrative_ontology:cs_axiom_grounding('820d4dac-e25e-44bf-9969-03e95dfa87e7', standing_requires_demonstrated_present_fitness, conventional).
narrative_ontology:cs_axiom('820d4dac-e25e-44bf-9969-03e95dfa87e7', secondary, birth_alone_insufficient_for_moral_community_admission).
narrative_ontology:cs_axiom_status(birth_alone_insufficient_for_moral_community_admission, holdable).
narrative_ontology:cs_axiom_grounding('820d4dac-e25e-44bf-9969-03e95dfa87e7', birth_alone_insufficient_for_moral_community_admission, conventional).
narrative_ontology:cs_reference_frame('820d4dac-e25e-44bf-9969-03e95dfa87e7', post_natal_evaluative_threshold_tradition).
narrative_ontology:cs_drift_state('820d4dac-e25e-44bf-9969-03e95dfa87e7', contemporary_bioethics_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('820d4dac-e25e-44bf-9969-03e95dfa87e7', '').
narrative_ontology:cs_kernel_id(personhood_boundary__fitness_contingent_reading, personhood_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(personhood_boundary__fitness_contingent_reading, state_selection_authorities).
narrative_ontology:constraint_beneficiary(personhood_boundary__fitness_contingent_reading, fitness_evaluating_physicians).
narrative_ontology:constraint_beneficiary(personhood_boundary__fitness_contingent_reading, resource_conserving_institutions).
narrative_ontology:constraint_victim(personhood_boundary__fitness_contingent_reading, pre_fitness_infants).
narrative_ontology:constraint_victim(personhood_boundary__fitness_contingent_reading, disabled_neonates).
narrative_ontology:constraint_victim(personhood_boundary__fitness_contingent_reading, families_of_excluded_infants).
narrative_ontology:constraint_vindicates(personhood_boundary__fitness_contingent_reading, fitness_as_ground_of_standing).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Newborns whose moral standing under this reading does not yet exist; they are subject to a fitness evaluation performed by others, cannot consent, cannot appeal, and cannot exit the evaluation process. Whether they are admitted to the moral community is decided entirely by external authorities before they can demonstrate anything on their own behalf.
narrative_ontology:constraint_stakeholder(personhood_boundary__fitness_contingent_reading, pre_fitness_infants, payer,
    powerless, immediate, trapped, local).

% Infants presenting with disability or impairment at the fitness evaluation are disproportionately found to fail the fitness threshold, placing them permanently outside the reading's moral community regardless of subsequent development or capacity.
narrative_ontology:constraint_stakeholder(personhood_boundary__fitness_contingent_reading, disabled_neonates, payer,
    powerless, immediate, trapped, local).

% Parents and kin of an infant found unfit bear the grief, social stigma, and practical consequences of the exclusion decision. They may object to the fitness determination but have limited standing to contest it once the evaluating authority has ruled, and dissent itself can mark them as deviant from the community's accepted moral order.
narrative_ontology:constraint_stakeholder(personhood_boundary__fitness_contingent_reading, families_of_excluded_infants, payer,
    moderate, biographical, constrained, local).

% Sets and administers the fitness criteria, appoints or licenses the evaluators, and enforces the consequence of failure (exclusion from legal and moral protections). Frames the arrangement as safeguarding the community's collective welfare and resource allocation, and controls both the definition of fitness and the record of who has passed it.
narrative_ontology:constraint_stakeholder(personhood_boundary__fitness_contingent_reading, state_selection_authorities, agenda_setter,
    institutional, generational, arbitrage, national).

% Professionals empowered to conduct and certify fitness determinations gain social authority, institutional standing, and often direct compensation from performing evaluations. Their professional identity and livelihood are partly constituted by administering the boundary, which gives them limited incentive to challenge it even where they harbor private doubts.
narrative_ontology:constraint_stakeholder(personhood_boundary__fitness_contingent_reading, fitness_evaluating_physicians, beneficiary,
    organized, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(personhood_boundary__fitness_contingent_reading, fitness_evaluating_physicians, agenda_setter).

% Hospitals, welfare systems, and state budgets that would otherwise bear the cost of caring for infants deemed unfit avoid that cost once exclusion is legally sanctioned. They benefit from the boundary without directly administering it.
narrative_ontology:constraint_stakeholder(personhood_boundary__fitness_contingent_reading, resource_conserving_institutions, beneficiary,
    institutional, generational, arbitrage, national).

% Would argue that fitness is an arbitrary and historically weaponized criterion for moral standing, citing its use to justify infanticide and eugenic exclusion. They are rarely given standing in the bodies that set or apply fitness criteria and must operate from outside the evaluating institutions.
narrative_ontology:constraint_stakeholder(personhood_boundary__fitness_contingent_reading, disability_rights_advocates, excluded,
    moderate, generational, constrained, national).

% Study the fitness-contingent reading as one of several competing personhood boundaries, tracing its historical instantiations (infant exposure practices, eugenic sterilization regimes, contemporary disability-selective abortion debates) without personal stake in any given evaluation.
narrative_ontology:constraint_stakeholder(personhood_boundary__fitness_contingent_reading, moral_philosophers_observing, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The reading purports to coordinate scarce communal resources (care, protection, inheritance, legal recognition) by restricting their allocation to entities that have demonstrated a threshold of viability or capacity, rather than extending them universally at birth.
% TRANSFER_FUNCTION: Moves the burden of survival, care, and resource allocation away from the community and onto the excluded infant and its family; simultaneously moves discretionary authority over who counts as a person from the individual (who cannot claim it for themself) to the evaluating institution.
% ABSENT_VOICES: The infants themselves have no voice by construction — the entire reading is defined by their inability to demonstrate anything on their own behalf prior to evaluation. Disability rights advocates and post-hoc adult testimony from formerly 'unfit'-classified survivors are structurally kept outside the bodies that set fitness criteria.
% DISAPPEARANCE_RATIONALE: If the fitness-contingent boundary were abolished, every infant would enter the moral community at birth (or at conception, under sibling readings) without the deferred waiting period; evaluating physicians would lose an administrative function, state authorities would lose an active exclusion power, and resource-allocation decisions currently justified by 'not yet a person' would require new, non-exclusionary justification. Historical practices instantiating this reading (infant exposure, eugenic culling of disabled newborns) would become unambiguously classified as killing persons rather than pre-personhood determinations.
% FOUNDING_PROBLEM: Ancient and early-modern communities facing high infant mortality, scarce resources, and no reliable early diagnosis of viability sought a principled-seeming way to decide which newborns to invest scarce care in — fitness testing offered an apparently objective sorting criterion.
% FOUNDING_PROBLEM_CORROBORATION: State authorities and some bioethicists defend the fitness criterion as addressing real resource-scarcity and severe-impairment triage dilemmas that persist today. Disability rights scholars, human rights bodies (documented in UN CRPD commentary), and historians of eugenics attest from outside the beneficiary set that the fitness criterion has functioned primarily to launder discriminatory exclusion and lacks a principled, non-arbitrary threshold — corroboration exists on both sides, which is itself part of why this reading remains a live and contested kernel instantiation rather than settled.
narrative_ontology:disappearance_verdict(personhood_boundary__fitness_contingent_reading, world_rearranges).
narrative_ontology:founding_problem_status(personhood_boundary__fitness_contingent_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(personhood_boundary__fitness_contingent_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(personhood_boundary__fitness_contingent_reading, 'none', 1).
narrative_ontology:epsilon_provenance(personhood_boundary__fitness_contingent_reading, 0.86, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness is authored high (0.86) because, under this reading's own terms, the fitness test transfers survival and protection away from a class that cannot participate in its own evaluation, toward institutions that gain administrative authority or avoided cost. Suppression is authored even higher (0.88) because enforcement is total: an infant found unfit has no procedural recourse, and dissenting families face social and sometimes legal sanction for contesting a fitness ruling. Theater ratio rises across the interval (0.2 to 0.4) reflecting the historical drift from openly declared exposure/culling practices toward more procedurally dressed 'best interest' and triage committees that preserve the same exclusionary function under increasingly bureaucratic and clinical language — the coordination story (resource stewardship, triage necessity) becomes more elaborate even as the underlying sorting function persists. Accessibility collapse (0.72) is high but not maximal: alternative readings (birth threshold, potential-based) remain visible and contested in parallel legal and philosophical traditions, so collapse is substantial but not complete. Resistance (0.58) is moderate-high, reflecting sustained disability-rights and human-rights pushback against this reading specifically.
 *
 * DIRECTIONALITY LOGIC:
 *   Pre-fitness infants and disabled neonates are declared victims with d approaching the full-target end: they are trapped by construction (they cannot exit an evaluation of their own personhood) and bear the entire cost of exclusion. State selection authorities and evaluating physicians sit near the beneficiary end: they hold institutional or organized power, control the criteria, and gain administrative authority or professional standing from operating the boundary. Resource-conserving institutions benefit diffusely without directly administering the test, which is why they are declared beneficiaries but not agenda_setters. Families occupy an intermediate position — real cost-bearers with some social power but constrained exit, since contesting a fitness ruling can itself be read as deviance from the community's accepted moral order.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (scarce resources, high infant mortality, no reliable early viability diagnosis) is largely dead in contemporary developed contexts with modern neonatal medicine, yet the fitness-contingent logic persists in residual and reactivated forms (selective non-treatment debates, disability-selective practices). The founding_problem_status is authored as contested rather than flatly dead because state authorities and some bioethicists continue to invoke genuine triage dilemmas in resource-constrained settings — but the corroboration record shows the dead-problem reading is well supported from outside the beneficiary set. This is precisely the mandatrophy pattern the classification exists to catch: an arrangement whose original coordination rationale has substantially expired but which persists because the administering and resource-conserving beneficiaries retain both the authority and the incentive to maintain it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_selection_ambiguity,
    'Is the fitness-contingent reading a defensible independent account of when moral standing begins, or is it structurally indistinguishable from the potential_based_reading applied selectively to convenient cases (i.e., a rhetorical variant rather than a distinct kernel commitment)?',
    'Compare the historical and doctrinal record of jurisdictions/traditions that explicitly articulate a fitness test as distinct from a potential-for-rational-agency criterion — if the criteria used in practice track potential (capacity trajectory) rather than demonstrated present fitness, the readings collapse into one.',
    'If the readings collapse, this story and the potential_based_reading sibling should be merged or one should be marked derivative of the other in network.affects_constraints rather than treated as fully independent kernel instantiations.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_selection_ambiguity, conceptual, 'Whether fitness-contingent and potential-based readings are structurally distinct or the same commitment under different labels.').

omega_variable(
    sibling_reading_disagreement_locus,
    'Where exactly does the fitness_contingent_reading structurally diverge from the birth_threshold_reading — is it the existence of ANY post-natal evaluation window, or specifically the content of the fitness criteria used within that window?',
    'Trace historical instantiations where a post-natal waiting period existed but the criteria applied were effectively universal (i.e., near-100% pass rate) versus instantiations with substantively exclusionary criteria — the former would be closer to a procedural variant of birth_threshold_reading, the latter a genuinely distinct boundary.',
    'Narrows or widens the class of historical/contemporary practices properly classified under this reading versus the birth_threshold_reading, affecting which real-world arrangements this story''s metrics should be understood to describe.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_disagreement_locus, conceptual, 'Locating the precise structural disagreement between this reading and the birth_threshold_reading sibling.').

omega_variable(
    natural_vs_constructed_fitness_criterion,
    'Is ''demonstrated fitness'' itself a natural, discoverable property of the infant, or is the fitness threshold a constructed social choice dressed in naturalistic language?',
    'Examine whether fitness criteria have varied historically and cross-culturally in ways that track resource scarcity and social convenience rather than any stable biological marker — convergent variation with social conditions would indicate construction rather than discovery.',
    'If fitness criteria are shown to track social convenience rather than a stable natural marker, the reading''s claim to ground personhood in an objective demonstrated property is undermined, strengthening the extractive characterization already authored here.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(natural_vs_constructed_fitness_criterion, empirical, 'Whether the fitness criterion is a natural marker or a socially constructed and historically variable threshold.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(personhood_boundary__fitness_contingent_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pers_tr_t0, personhood_boundary__fitness_contingent_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(pers_tr_t20, personhood_boundary__fitness_contingent_reading, theater_ratio, 20, 0.25).
narrative_ontology:measurement(pers_tr_t40, personhood_boundary__fitness_contingent_reading, theater_ratio, 40, 0.3).
narrative_ontology:measurement(pers_tr_t60, personhood_boundary__fitness_contingent_reading, theater_ratio, 60, 0.34).
narrative_ontology:measurement(pers_tr_t80, personhood_boundary__fitness_contingent_reading, theater_ratio, 80, 0.37).
narrative_ontology:measurement(pers_tr_t100, personhood_boundary__fitness_contingent_reading, theater_ratio, 100, 0.4).

% Extraction over time
narrative_ontology:measurement(pers_be_t0, personhood_boundary__fitness_contingent_reading, base_extractiveness, 0, 0.68).
narrative_ontology:measurement(pers_be_t20, personhood_boundary__fitness_contingent_reading, base_extractiveness, 20, 0.74).
narrative_ontology:measurement(pers_be_t40, personhood_boundary__fitness_contingent_reading, base_extractiveness, 40, 0.79).
narrative_ontology:measurement(pers_be_t60, personhood_boundary__fitness_contingent_reading, base_extractiveness, 60, 0.82).
narrative_ontology:measurement(pers_be_t80, personhood_boundary__fitness_contingent_reading, base_extractiveness, 80, 0.84).
narrative_ontology:measurement(pers_be_t100, personhood_boundary__fitness_contingent_reading, base_extractiveness, 100, 0.86).

% Suppression requirement over time
narrative_ontology:measurement(pers_su_t0, personhood_boundary__fitness_contingent_reading, suppression_requirement, 0, 0.7).
narrative_ontology:measurement(pers_su_t20, personhood_boundary__fitness_contingent_reading, suppression_requirement, 20, 0.75).
narrative_ontology:measurement(pers_su_t40, personhood_boundary__fitness_contingent_reading, suppression_requirement, 40, 0.79).
narrative_ontology:measurement(pers_su_t60, personhood_boundary__fitness_contingent_reading, suppression_requirement, 60, 0.83).
narrative_ontology:measurement(pers_su_t80, personhood_boundary__fitness_contingent_reading, suppression_requirement, 80, 0.86).
narrative_ontology:measurement(pers_su_t100, personhood_boundary__fitness_contingent_reading, suppression_requirement, 100, 0.88).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(personhood_boundary__fitness_contingent_reading, identity_coordination).
narrative_ontology:affects_constraint(personhood_boundary__fitness_contingent_reading, personhood_boundary__birth_threshold_reading).
narrative_ontology:affects_constraint(personhood_boundary__fitness_contingent_reading, personhood_boundary__potential_based_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the personhood_boundary kernel, each authored as an independent ε-invariant constraint per the ε-invariance principle. birth_threshold_reading treats all born humans as possessing standing with no post-natal test (low authored ε for the boundary mechanism itself). potential_based_reading grounds standing in potential for rational agency, producing a narrower and differently-shaped victim set (severely disabled infants specifically) than this reading's broader post-natal evaluation window. This reading (fitness_contingent_reading) authors the highest ε of the three because it affirmatively constructs an active exclusionary testing apparatus with ongoing institutional enforcement, rather than a default inclusion rule or a potential-based exception. The three stories should be read together to understand the full kernel contest; none is the 'correct' one and none averages with the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
